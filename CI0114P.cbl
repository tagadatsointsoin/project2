       IDENTIFICATION DIVISION.                                         CI0114
       PROGRAM-ID.  CI0114P.                                            CI0114
      *AUTHOR.         PAPERLESS FILING INFORMATION.                    CI0114
      *DATE-COMPILED.   09/08/14.                                       CI0114
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 1998                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE CATS SYSTEM AND ALL INFORMATION RELATING THERETO,    *  ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE CATS SYSTEM AND ALL            *  ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE CATS       *  ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 1998                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0114
       CONFIGURATION SECTION.                                           CI0114
       SOURCE-COMPUTER. IBM-370.                                        CI0114
       OBJECT-COMPUTER. IBM-370.                                        CI0114
       DATA DIVISION.                                                   CI0114
       WORKING-STORAGE SECTION.                                         CI0114
       01                 BR30.                                         CI0114
            10            BR30-C199.                                    CI0114
            11            BR30-CLID.                                    CI0114
            12            BR30-CLIDO  PICTURE  9(3).                    CI0114
            12            BR30-CLIDN.                                   CI0114
            13            BR30-CLIDNP PICTURE  X(12).                   CI0114
            13            BR30-CLIDND PICTURE  9(8).                    CI0114
            10            BR30-CDEL1  PICTURE  9(3).                    CI0114
            10            BR30-DCACG  PICTURE  9(8).                    CI0114
            10            BR30-NRTSQ1 PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            10            BR30-FILLER PICTURE  X(100).                  CI0114
            10            BR30-CLORN  PICTURE  X(45).                   CI0114
            10            BR30-CL18.                                    CI0114
            11            BR30-CL18K.                                   CI0114
            12            BR30-NRTSQ  PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            BR30-NTR    PICTURE  9(8).                    CI0114
            11            BR30-GECKD  PICTURE  9.                       CI0114
            11            BR30-GEEND  PICTURE  9(8).                    CI0114
            11            BR30-NPDIN  PICTURE  X(4).                    CI0114
            11            BR30-IRTNA  PICTURE  X.                       CI0114
            11            BR30-IRTNP  PICTURE  X.                       CI0114
            11            BR30-IRTNW  PICTURE  X.                       CI0114
            10            BR30-IBNKI  PICTURE  X.                       CI0114
            10            BR30-FILLER PICTURE  X(100).                  CI0114
       01                 CL01.                                         CI0114
            10            CL01-CL01K.                                   CI0114
            11            CL01-C199.                                    CI0114
            12            CL01-CLID.                                    CI0114
            13            CL01-CLIDO  PICTURE  9(3).                    CI0114
            13            CL01-CLIDN.                                   CI0114
            14            CL01-CLIDNP PICTURE  X(12).                   CI0114
            14            CL01-CLIDND PICTURE  9(8).                    CI0114
            10            CL01-GECKD  PICTURE  9.                       CI0114
            10            CL01-GEMDA  PICTURE  9(8).                    CI0114
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0114
                          BINARY.                                       CI0114
            10            CL01-GECUC  PICTURE  99.                      CI0114
            10            CL01-CLDOR  PICTURE  9(8).                    CI0114
            10            CL01-CLLNG  PICTURE  XX.                      CI0114
            10            CL01-GESLC  PICTURE  99.                      CI0114
            10            CL01-CLTYP  PICTURE  X.                       CI0114
            10            CL01-CLCLS  PICTURE  9(3).                    CI0114
            10            CL01-CLTWRC PICTURE  99.                      CI0114
            10            CL01-CLPVC  PICTURE  99.                      CI0114
            10            CL01-CLIND  PICTURE  9(3).                    CI0114
            10            CL01-CLTRC  PICTURE  99.                      CI0114
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0114
                          COMPUTATIONAL-3.                              CI0114
            10            CL01-AYSIDA PICTURE  9(3).                    CI0114
            10            CL01-AYSID  PICTURE  9(5).                    CI0114
            10            CL01-CLSTR  PICTURE  9(2).                    CI0114
            10            CL01-CLC11  PICTURE  X.                       CI0114
            10            CL01-CLTIN  PICTURE  9(12).                   CI0114
            10            CL01-CLTND  PICTURE  9(8).                    CI0114
            10            CL01-CLTINC PICTURE  9.                       CI0114
            10            CL01-CCDWA  PICTURE  9.                       CI0114
            10            CL01-CICES  PICTURE  X.                       CI0114
            10            CL01-CLTRA  PICTURE  9(2).                    CI0114
            10            CL01-DIRSY  PICTURE  9(4)                     CI0114
                          COMPUTATIONAL-3.                              CI0114
            10            CL01-CFEDS  PICTURE  X.                       CI0114
            10            CL01-FILLER PICTURE  X(06).                   CI0114
       01                 CL03.                                         CI0114
            10            CL03-GEDLA  PICTURE  9(8).                    CI0114
            10            CL03-DDREP  PICTURE  9(8).                    CI0114
            10            CL03-DPRFR  PICTURE  9(8).                    CI0114
            10            CL03-IACCI  PICTURE  X.                       CI0114
            10            CL03-CLDOB  PICTURE  9(8).                    CI0114
            10            CL03-CLDOD  PICTURE  9(8).                    CI0114
            10            CL03-CLDTH  PICTURE  X.                       CI0114
            10            CL03-CCINI  PICTURE  X.                       CI0114
            10            CL03-FILLER PICTURE  X(1).                    CI0114
            10            CL03-CLAIN  PICTURE  S9(11)                   CI0114
                          COMPUTATIONAL-3.                              CI0114
            10            CL03-CCAOD  PICTURE  999.                     CI0114
            10            CL03-CLMAR  PICTURE  X.                       CI0114
            10            CL03-C198.                                    CI0114
            11            CL03-CLNAM.                                   CI0114
            12            CL03-CLNAMH PICTURE  X(6).                    CI0114
            12            CL03-CLNAMF PICTURE  X(20).                   CI0114
            12            CL03-CLNAMM.                                  CI0114
            13            CL03-CLNAMI PICTURE  X.                       CI0114
            13            CL03-CLNAMR PICTURE  X(14).                   CI0114
            12            CL03-CLNAML PICTURE  X(25).                   CI0114
            12            CL03-CLNAMS PICTURE  X(4).                    CI0114
            10            CL03-FILLER PICTURE  X(10).                   CI0114
            10            CL03-MPRFS  PICTURE  X(4).                    CI0114
            10            CL03-CLOCC  PICTURE  9(3).                    CI0114
            10            CL03-CLRET  PICTURE  X.                       CI0114
            10            CL03-IOCOB  PICTURE  X.                       CI0114
            10            CL03-CLSEX  PICTURE  X.                       CI0114
            10            CL03-CLWIL  PICTURE  X.                       CI0114
            10            CL03-GECFC  PICTURE  99.                      CI0114
            10            CL03-GECFY  PICTURE  9(4).                    CI0114
            10            CL03-ICUSC  PICTURE  X.                       CI0114
            10            CL03-MCTYC  PICTURE  X(20).                   CI0114
            10            CL03-CLWIP  PICTURE  X.                       CI0114
            10            CL03-CLCTXF PICTURE  99.                      CI0114
            10            CL03-CLCUS  PICTURE  99.                      CI0114
            10            CL03-NPDLU  PICTURE  9(5).                    CI0114
            10            CL03-CLEMI  PICTURE  X.                       CI0114
            10            CL03-GEPHNH PICTURE  X(14).                   CI0114
            10            CL03-GEPHNB PICTURE  X(14).                   CI0114
            10            CL03-GEPHNX PICTURE  9(4).                    CI0114
            10            CL03-GEPHNA PICTURE  X(14).                   CI0114
            10            CL03-FILLER PICTURE  X(3).                    CI0114
            10            CL03-IAPRT  PICTURE  X.                       CI0114
            10            CL03-CEMSC  PICTURE  X.                       CI0114
            10            CL03-CSEPS  PICTURE  X.                       CI0114
            10            CL03-CRACE  PICTURE  X.                       CI0114
            10            CL03-CNIRA  PICTURE  X.                       CI0114
            10            CL03-FILLER PICTURE  X(11).                   CI0114
       01                 CL12.                                         CI0114
            10            CL12-GEDLA  PICTURE  9(8).                    CI0114
            10            CL12-CLBCD  PICTURE  9(3).                    CI0114
            10            CL12-CLFDW  PICTURE  X.                       CI0114
            10            CL12-CLOSD  PICTURE  9(8).                    CI0114
            10            CL12-CLOED  PICTURE  9(8).                    CI0114
            10            CL12-CLOEI  PICTURE  X.                       CI0114
            10            CL12-CLIBN  PICTURE  X(20).                   CI0114
            10            CL12-CLINT  PICTURE  9(3).                    CI0114
            10            CL12-CLONE  PICTURE  9(9).                    CI0114
            10            CL12-CLORC  PICTURE  99.                      CI0114
            10            CL12-CLORN  PICTURE  X(45).                   CI0114
            10            CL12-CLORP  PICTURE  X(25).                   CI0114
            10            CL12-GEPHNB PICTURE  X(14).                   CI0114
            10            CL12-GEPHNX PICTURE  9(4).                    CI0114
            10            CL12-GEPHNA PICTURE  X(14).                   CI0114
            10            CL12-GEFYE  PICTURE  9(4).                    CI0114
            10            CL12-AYCDE  PICTURE  9(3).                    CI0114
            10            CL12-AYID   PICTURE  9(5).                    CI0114
            10            CL12-CFOBO  PICTURE  99.                      CI0114
            10            CL12-CLINRG                                   CI0114
                          OCCURS       003     TIMES.                   CI0114
            11            CL12-CLIRT  PICTURE  99.                      CI0114
            11            CL12-CLINR  PICTURE  X(3).                    CI0114
            11            CL12-CLIRD  PICTURE  9(8).                    CI0114
            10            CL12-IOTXE  PICTURE  X.                       CI0114
            10            CL12-IO501  PICTURE  X.                       CI0114
            10            CL12-IOFOG  PICTURE  X.                       CI0114
            10            CL12-IOPRA  PICTURE  X.                       CI0114
            10            CL12-IOSCS  PICTURE  X.                       CI0114
            10            CL12-IACHA  PICTURE  X.                       CI0114
            10            CL12-IFORG  PICTURE  X.                       CI0114
            10            CL12-IFIND  PICTURE  X.                       CI0114
            10            CL12-CFCNT3 PICTURE  X(2).                    CI0114
            10            CL12-FILLER PICTURE  X(06).                   CI0114
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0003           PIC X(8) VALUE 'CI0003P '.
       01  CI0018           PIC X(8) VALUE 'CI0018P '.                  AM0018
       01  CI0025           PIC X(8) VALUE 'CI0025P '.                  AM0025
       01                 CX01.                                         CI0114
            10            CX01-CX01K.                                   CI0114
            11            CX01-C199.                                    CI0114
            12            CX01-CLID.                                    CI0114
            13            CX01-CLIDO  PICTURE  9(3).                    CI0114
            13            CX01-CLIDN.                                   CI0114
            14            CX01-CLIDNP PICTURE  X(12).                   CI0114
            14            CX01-CLIDND PICTURE  9(8).                    CI0114
            10            CX01-GEMDA  PICTURE  9(8).                    CI0114
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0114
                          BINARY.                                       CI0114
            10            CX01-FILLER PICTURE  X(5).                    CI0114
       01                 CX18.                                         CI0114
            10            CX18-CX18K.                                   CI0114
            11            CX18-NBASQ  PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            10            CX18-NPBN   PICTURE  X(20).                   CI0114
            10            CX18-CCBAT  PICTURE  99.                      CI0114
            10            CX18-DACHP  PICTURE  9(8).                    CI0114
            10            CX18-CSTPRE PICTURE  99.                      CI0114
            10            CX18-C199.                                    CI0114
            11            CX18-CLID.                                    CI0114
            12            CX18-CLIDO  PICTURE  9(3).                    CI0114
            12            CX18-CLIDN.                                   CI0114
            13            CX18-CLIDNP PICTURE  X(12).                   CI0114
            13            CX18-CLIDND PICTURE  9(8).                    CI0114
            10            CX18-MCSIG  PICTURE  X(30).                   CI0114
            10            CX18-CPBNU  PICTURE  X.                       CI0114
            10            CX18-CSPCR  PICTURE  99.                      CI0114
            10            CX18-DAPCR  PICTURE  9(8).                    CI0114
            10            CX18-FILLER PICTURE  XX.                      CI0114
       01                 CX21.                                         CI0114
            10            CX21-GELL   PICTURE  9(4)                     CI0114
                          BINARY.                                       CI0114
            10            CX21-CZ00.                                    CI0114
            11            CX21-CX21K.                                   CI0114
            12            CX21-CDEL1  PICTURE  9(3).                    CI0114
            12            CX21-NDELS  PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            10            CX21-CZ99.                                    CI0114
            11            CX21-FILLER PICTURE  X(165).                  CI0114
            10            CX21-CZ01                                     CI0114
                          REDEFINES            CX21-CZ99.               CI0114
            11            CX21-NBASQ  PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            CX21-GECSQ  PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            10            CX21-CZ02                                     CI0114
                          REDEFINES            CX21-CZ99.               CI0114
            11            CX21-CPAYE  PICTURE  9(2).                    CI0114
            11            CX21-C199.                                    CI0114
            12            CX21-CLID.                                    CI0114
            13            CX21-CLIDO  PICTURE  9(3).                    CI0114
            13            CX21-CLIDN.                                   CI0114
            14            CX21-CLIDNP PICTURE  X(12).                   CI0114
            14            CX21-CLIDND PICTURE  9(8).                    CI0114
            11            CX21-GECSQ1 PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            CX21-NBASQT PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            CX21-TDELI  PICTURE  X(30).                   CI0114
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0114
            10            XW05-XW06.                                    CI0114
            11            XW05-XDBPCB.                                  CI0114
            12            XW05-XDBDNM PICTURE  X(08)                    CI0114
                          VALUE                SPACE.                   CI0114
            12            XW05-XSEGLV PICTURE  X(02)                    CI0114
                          VALUE                SPACE.                   CI0114
            12            XW05-XRC    PICTURE  X(02)                    CI0114
                          VALUE                SPACE.                   CI0114
            12            XW05-XPROPT PICTURE  X(04)                    CI0114
                          VALUE                SPACE.                   CI0114
            12            XW05-FILLER PICTURE  S9(5)                    CI0114
                          VALUE                ZERO                     CI0114
                          BINARY.                                       CI0114
            12            XW05-XSEGNM PICTURE  X(08)                    CI0114
                          VALUE                SPACE.                   CI0114
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0114
                          VALUE                ZERO                     CI0114
                          BINARY.                                       CI0114
            12            XW05-XSEGNB PICTURE  9(05)                    CI0114
                          VALUE                ZERO                     CI0114
                          BINARY.                                       CI0114
            12            XW05-XCOKEY PICTURE  X(70)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            XW05-XW07.                                    CI0114
            11            XW05-XIOPCB.                                  CI0114
            12            XW05-XTERMI PICTURE  X(08)                    CI0114
                          VALUE                SPACE.                   CI0114
            12            XW05-FILLER PICTURE  XX                       CI0114
                          VALUE                SPACE.                   CI0114
            12            XW05-XRC1   PICTURE  X(02)                    CI0114
                          VALUE                SPACE.                   CI0114
            12            XW05-FILLER PICTURE  X(12)                    CI0114
                          VALUE                SPACE.                   CI0114
            12            XW05-XMODNM PICTURE  X(8)                     CI0114
                          VALUE                SPACE.                   CI0114
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0114
                          VALUE                ZERO.                    CI0114
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0114
                          VALUE                ZERO.                    CI0114
            10            XW05-XGU    PICTURE  X(4)                     CI0114
                          VALUE                'GU  '.                  CI0114
            10            XW05-XGHU   PICTURE  X(4)                     CI0114
                          VALUE                'GHU '.                  CI0114
            10            XW05-XGN    PICTURE  X(4)                     CI0114
                          VALUE                'GN  '.                  CI0114
            10            XW05-XGHN   PICTURE  X(4)                     CI0114
                          VALUE                'GHN '.                  CI0114
            10            XW05-XGNP   PICTURE  X(4)                     CI0114
                          VALUE                'GNP '.                  CI0114
            10            XW05-XGHNP  PICTURE  X(4)                     CI0114
                          VALUE                'GHNP'.                  CI0114
            10            XW05-XREPL  PICTURE  XXXX                     CI0114
                          VALUE                'REPL'.                  CI0114
            10            XW05-XISRT  PICTURE  X(4)                     CI0114
                          VALUE                'ISRT'.                  CI0114
            10            XW05-XDLET  PICTURE  X(4)                     CI0114
                          VALUE                'DLET'.                  CI0114
            10            XW05-XOPEN  PICTURE  X(4)                     CI0114
                          VALUE                'OPEN'.                  CI0114
            10            XW05-XCLSE  PICTURE  X(4)                     CI0114
                          VALUE                'CLSE'.                  CI0114
            10            XW05-XCHKP  PICTURE  X(4)                     CI0114
                          VALUE                'CHKP'.                  CI0114
            10            XW05-XXRST  PICTURE  X(4)                     CI0114
                          VALUE                'XRST'.                  CI0114
            10            XW05-XTERM  PICTURE  X(4)                     CI0114
                          VALUE                'TERM'.                  CI0114
            10            XW05-XNFPAC PICTURE  X(13)                    CI0114
                          VALUE                SPACE.                   CI0114
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0114
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0114

      *PASS AREA TO/FROM CI0018 (CLIENTS) - SOURCE ACCT
      *!WF DSP=FC DSL=DU SEL=14 FOR=I LEV=1 PLT=FC
       01                 FC00.                                         CI0114
          05              FC00-SUITE.                                   CI0114
            15       FILLER         PICTURE  X(00917).                  CI0114
       01                 FC14  REDEFINES      FC00.                    CI0114
            10            FC14-C299.                                    CI0114
            11            FC14-CTID.                                    CI0114
            12            FC14-CTIDA  PICTURE  9(3).                    CI0114
            12            FC14-CTIDN.                                   CI0114
            13            FC14-CTIDNP PICTURE  X(13).                   CI0114
            13            FC14-CTIDND PICTURE  9(11).                   CI0114
            10            FC14-DCACG  PICTURE  9(8).                    CI0114
            10            FC14-IPOCH  PICTURE  X.                       CI0114
            10            FC14-FILLER PICTURE  X(100).                  CI0114
            10            FC14-CLID01.                                  CI0114
            11            FC14-CLIDO1 PICTURE  X(3).                    CI0114
            11            FC14-NCLID1.                                  CI0114
            12            FC14-CLIDP1 PICTURE  X(12).                   CI0114
            12            FC14-CLIDNA PICTURE  9(8).                    CI0114
            10            FC14-CLCTR  PICTURE  9(3).                    CI0114
            10            FC14-DU21                                     CI0114
                          OCCURS       025     TIMES.                   CI0114
            11            FC14-C199.                                    CI0114
            12            FC14-CLID.                                    CI0114
            13            FC14-CLIDO  PICTURE  9(3).                    CI0114
            13            FC14-CLIDN.                                   CI0114
            14            FC14-CLIDNP PICTURE  X(12).                   CI0114
            14            FC14-CLIDND PICTURE  9(8).                    CI0114
            11            FC14-CLCTRC PICTURE  9(3).                    CI0114
            10            FC14-QITEM  PICTURE  9(3).                    CI0114
            10            FC14-XIMAX  PICTURE  S9(4)                    CI0114
                          BINARY.                                       CI0114
            10            FC14-CRROL  PICTURE  X.                       CI0114
            10            FC14-FILLER PICTURE  X(099).                  CI0114


      *PASS AREA TO/FROM CI0003 (OWNERSHIP LINES) - SOURCE ACCT
      *!WF DSP=FU DSL=DU SEL=04 FOR=I LEV=1 PLT=FU
       01                 FU00.                                         CI0114
          05              FU00-SUITE.                                   CI0114
            15       FILLER         PICTURE  X(00407).                  CI0114
       01                 FU04  REDEFINES      FU00.                    CI0114
            10            FU04-C299.                                    CI0114
            11            FU04-CTID.                                    CI0114
            12            FU04-CTIDA  PICTURE  9(3).                    CI0114
            12            FU04-CTIDN.                                   CI0114
            13            FU04-CTIDNP PICTURE  X(13).                   CI0114
            13            FU04-CTIDND PICTURE  9(11).                   CI0114
            10            FU04-IPOCH  PICTURE  X.                       CI0114
            10            FU04-FILLER PICTURE  X(099).                  CI0114
            10            FU04-CTTLN1 PICTURE  X(30).                   CI0114
            10            FU04-CTTLN2 PICTURE  X(30).                   CI0114
            10            FU04-CTTLN3 PICTURE  X(30).                   CI0114
            10            FU04-CTTBO1 PICTURE  X(45).                   CI0114
            10            FU04-CTTBO2 PICTURE  X(45).                   CI0114
            10            FU04-CTOWN  PICTURE  9(3).                    CI0114
            10            FU04-IUGMA  PICTURE  X.                       CI0114
            10            FU04-FILLER PICTURE  X(096).                  CI0114

      *GENERATE INDEX FOR MESSAGE TEXT (ONLY IF 'OCCURS' SPECIFIED)     ADU071
      *                   MS03                                          ADU071
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
       01  LENGTH-FIELDS.                                               AANA15
      * 9LSX FIELDS CONTAIN POINT OF LAST CHARACTER IN NAMES            AANA15
           05 7-NA10-9LSH    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9LSF    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9LSM    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9LSL    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9LSS    PIC S9(4) COMP  VALUE ZERO.                AANA15
      * 9TSPAC CONTAINS TOTAL NUMBER OF BLANKS NEEDED IN NAME.          AANA15
           05 7-NA10-9TSPAC  PIC S9(4) COMP  VALUE ZERO.                AANA15
      * 9STX FIELDS CONTAIN POINT OF FIRST NON-BLANK CHARACTER IN NAMES AANA15
           05 7-NA10-9STH    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9STF    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9STM    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9STL    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9STS    PIC S9(4) COMP  VALUE ZERO.                AANA15
       01  7-NA10-CLNAM.                                                AANA15
           05 7-NA10-CLNAMH.                                            AANA15
              10 7-NA10-9NMH      PIC X  OCCURS  6 TIMES.               AANA15
           05 7-NA10-CLNAMF.                                            AANA15
              10 7-NA10-9NMF      PIC X  OCCURS 20 TIMES.               AANA15
           05 7-NA10-CLNAMM.                                            AANA15
              10 7-NA10-9NMM      PIC X  OCCURS 15 TIMES.               AANA15
           05 7-NA10-CLNAML.                                            AANA15
              10 7-NA10-9NML      PIC X  OCCURS 25 TIMES.               AANA15
           05 7-NA10-CLNAMS.                                            AANA15
              10 7-NA10-9NMS      PIC X  OCCURS  4 TIMES.               AANA15
       01  7-NA10-COCLNM1.                                              AANA15
           05 7-NA10-COCLNM PIC X OCCURS 74.                            AANA15
      *FOLLOWING CODES CONTAIN FORMATTING RULES.                        AANA15
       01  7-NA10-9PRINT.                                               AANA15
           05 7-NA10-9PRTH     PIC X.                                   AANA15
           05 7-NA10-9PRTF     PIC X.                                   AANA15
           05 7-NA10-9PRTM     PIC X.                                   AANA15
           05 7-NA10-9PRTL     PIC X.                                   AANA15
           05 7-NA10-9PRTS     PIC X.                                   AANA15
       01  7-NA10-9WKNM.                                                AANA15
           05 7-NA10-9WKNM1 PIC X  OCCURS 30 TIMES.                     AANA15
                                                                        AM0003
      ******************************************************************AM0003
      **     PCB ADDRESS LIST FOR CI0003.  MODULE CI0003 WILL NEED     *AM0003
      **     PCB'S FOR:                                                *AM0003
      **                CONTRACT DATABASE(CT1P)                        *AM0003
      ******************************************************************AM0003
                                                                        AM0003
       01  CI0003A-PCB-ADDRESS-LIST.                                    AM0003
           05  CI0003A-PCB-CT1P-PTR1      POINTER.                      AM0003
                                                                        AM0003
      ******************************************************************AM0003
      **     PCB ADDRESS LIST FOR CI0003.  MODULE CI0003 WILL NEED     *AM0003
      **     PCB'S FOR:                                                *AM0003
      **                CONTRACT DATABASE(CT1P)                        *AM0003
      ******************************************************************AM0003
                                                                        AM0003
       01  CI0003B-PCB-ADDRESS-LIST.                                    AM0003
           05  CI0003B-PCB-CT1P-PTR1      POINTER.                      AM0003
                                                                        AM0025
      ******************************************************************AM0025
      **     PCB ADDRESS LIST FOR CI0025.  MODULE CI0025 WILL NEED     *AM0025
      **     PCB'S FOR:                                                *AM0025
      **                CLIENT DATABASE(CL1P)                          *AM0025
      ******************************************************************AM0025
                                                                        AM0025
       01  CI0025A-PCB-ADDRESS-LIST.                                    AM0025
           05  CI0025A-PCB-CL1P-PTR1      POINTER.                      AM0025
                                                                        AM0018
      ******************************************************************AM0018
      **     PCB ADDRESS LIST FOR CI0018.  MODULE CI0018 WILL NEED     *AM0018
      **     PCB'S FOR:                                                *AM0018
      **                CONTRACT DATABASE(CT1P)                        *AM0018
      ******************************************************************AM0018
                                                                        AM0018
       01  CI0018A-PCB-ADDRESS-LIST.                                    AM0018
           05  CI0018A-PCB-CT1P-PTR1      POINTER.                      AM0018
      ******************************************************************ADUTAB
      **              TABLE TA5A ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA5A-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=5A FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA5A.                                                CI0114
           04    G-TA5A-PARAM.                                          CI0114
             10  G-TA5A-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0114
                        VALUE      +772.                                CI0114
             10  G-TA5A-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0114
                        VALUE      +001.                                CI0114
             10  G-TA5A-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0114
                        VALUE      +017.                                CI0114
             10  G-TA5A-NUAPP  PICTURE 99                               CI0114
                        VALUE       0.                                  CI0114
             10  G-TA5A-NUTAB  PICTURE X(6)                             CI0114
                        VALUE 'TA005A'.                                 CI0114
             10  G-TA5A-TABFO  PICTURE XX                 VALUE SPACE.  CI0114
             10  G-TA5A-TABCR  PICTURE XX                 VALUE SPACE.  CI0114
             10  G-TA5A-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0114
             10  G-TA5A-NUSSC  PICTURE X  VALUE   ' '.                  CI0114
             10  G-TA5A-NUSSY  PICTURE X                  VALUE SPACE.  CI0114
             10  G-TA5A-TRANID PICTURE X(4)               VALUE SPACE.  CI0114
             10  G-TA5A-FILSYS.                                         CI0114
             15  G-TA5A-USERC  PICTURE X(6)               VALUE SPACE.  CI0114
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0114
           04             TA5A.                                         CI0114
            10            TA5A-GAPSC.                                   CI0114
            11            TA5A-CTIDA  PICTURE  9(3)                     CI0114
                          VALUE                ZERO.                    CI0114
            11            TA5A-PRCOD  PICTURE  9(5)                     CI0114
                          VALUE                ZERO.                    CI0114
            11            TA5A-PRSCD  PICTURE  X(9)                     CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-PRCLN  PICTURE  X(60)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-PRCMN  PICTURE  X(20)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-PRCSN  PICTURE  X(9)                     CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-MRCLN1 PICTURE  X(51)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-MRCLN2 PICTURE  X(51)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-MRCLN3 PICTURE  X(51)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-MRCLN4 PICTURE  X(51)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-MRCMN2 PICTURE  X(20)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-MRCMN3 PICTURE  X(20)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-PRCCS1 PICTURE  X(15)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-PRCCS2 PICTURE  X(15)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-PRCCS3 PICTURE  X(15)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-MPCLN  PICTURE  X(45)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-MPCL1  PICTURE  X(45)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-MSP1   PICTURE  X(60)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-MSP5   PICTURE  X(30)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-MSP03  PICTURE  X(3)                     CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-MPRDG  PICTURE  X(20)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-CPRDG  PICTURE  9(2)                     CI0114
                          VALUE                ZERO.                    CI0114
            10            TA5A-MPRDA1 PICTURE  X(50)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-CPRDA1 PICTURE  9(3)                     CI0114
                          VALUE                ZERO.                    CI0114
            10            TA5A-MSP06  PICTURE  X(20)                    CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-CPOIN  PICTURE  X                        CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-CPITY  PICTURE  9(3)                     CI0114
                          VALUE                ZERO.                    CI0114
            10            TA5A-CLITY  PICTURE  9(3)                     CI0114
                          VALUE                ZERO.                    CI0114
            10            TA5A-IVARP  PICTURE  X                        CI0114
                          VALUE                SPACE.                   CI0114
            10            TA5A-CASCL  PICTURE  9(3)                     CI0114
                          VALUE                ZERO.                    CI0114
            10            TA5A-ZDA88  PICTURE  X(88)                    CI0114
                          VALUE                SPACE.                   CI0114
      **                                                                ADUTAB
       01                 TO01.                                         CI0114
            10            TO01-CT01K.                                   CI0114
            11            TO01-C299.                                    CI0114
            12            TO01-CTID.                                    CI0114
            13            TO01-CTIDA  PICTURE  9(3).                    CI0114
            13            TO01-CTIDN.                                   CI0114
            14            TO01-CTIDNP PICTURE  X(13).                   CI0114
            14            TO01-CTIDND PICTURE  9(11).                   CI0114
            10            TO01-GECKD  PICTURE  9.                       CI0114
            10            TO01-GEMDA  PICTURE  9(8).                    CI0114
            10            TO01-NSEQ4B PICTURE  9(8)                     CI0114
                          BINARY.                                       CI0114
            10            TO01-GECUC  PICTURE  99.                      CI0114
            10            TO01-CTAUL  PICTURE  9(3).                    CI0114
            10            TO01-DIRAC  PICTURE  9(4).                    CI0114
            10            TO01-CTCCI  PICTURE  X.                       CI0114
            10            TO01-CTCUS  PICTURE  999.                     CI0114
            10            TO01-CTEFD  PICTURE  9(8).                    CI0114
            10            TO01-CTIAD  PICTURE  9(8).                    CI0114
            10            TO01-CLCUS  PICTURE  99.                      CI0114
            10            TO01-CAMMB  PICTURE  X(3).                    CI0114
            10            TO01-CKPMM  PICTURE  X.                       CI0114
            10            TO01-CTLAD  PICTURE  9(8).                    CI0114
            10            TO01-IPERS  PICTURE  X.                       CI0114
            10            TO01-AUNCB  PICTURE  S9(7)V99                 CI0114
                          COMPUTATIONAL-3.                              CI0114
            10            TO01-CTLAT  PICTURE  9(8).                    CI0114
            10            TO01-CTLATC PICTURE  9(6).                    CI0114
            10            TO01-IMEGA  PICTURE  X.                       CI0114
            10            TO01-DIRAB  PICTURE  9(8).                    CI0114
            10            TO01-COLRQ  PICTURE  X.                       CI0114
            10            TO01-ZDA04  PICTURE  X(4).                    CI0114
            10            TO01-CTLPD  PICTURE  9(8).                    CI0114
            10            TO01-CIRASP PICTURE  9.                       CI0114
            10            TO01-CIRATP PICTURE  99.                      CI0114
            10            TO01-DRTHC  PICTURE  9(8).                    CI0114
            10            TO01-CPPTC  PICTURE  X.                       CI0114
            10            TO01-ZDA06  PICTURE  X(6).                    CI0114
            10            TO01-CTACD  PICTURE  9(8).                    CI0114
            10            TO01-CTNLI  PICTURE  X.                       CI0114
            10            TO01-CTRHO  PICTURE  9(8).                    CI0114
            10            TO01-CTSGD  PICTURE  9(8).                    CI0114
            10            TO01-CPATP  PICTURE  X(1).                    CI0114
            10            TO01-IRSTA  PICTURE  X.                       CI0114
            10            TO01-CTSTA  PICTURE  99.                      CI0114
            10            TO01-CTSSC  PICTURE  99.                      CI0114
            10            TO01-PRLIN  PICTURE  9(3).                    CI0114
            10            TO01-PRCOD  PICTURE  9(5).                    CI0114
            10            TO01-PRSCD  PICTURE  X(9).                    CI0114
            10            TO01-CTLNI  PICTURE  X.                       CI0114
            10            TO01-AYSIDA PICTURE  9(3).                    CI0114
            10            TO01-AYSID  PICTURE  9(5).                    CI0114
            10            TO01-CTBMC  PICTURE  99.                      CI0114
            10            TO01-CINAR  PICTURE  99.                      CI0114
            10            TO01-CPHTR  PICTURE  X.                       CI0114
            10            TO01-CDSTR  PICTURE  XX.                      CI0114
            10            TO01-CQACT  PICTURE  999.                     CI0114
            10            TO01-CIRAS  PICTURE  999.                     CI0114
            10            TO01-CIRAT  PICTURE  999.                     CI0114
            10            TO01-CLRAY  PICTURE  9(5).                    CI0114
            10            TO01-CATTP  PICTURE  X.                       CI0114

      *PASS AREA TO/FROM CI0003 (OWNERSHIP LINES) - DEST ACCT
      *!WF DSP=TU DSL=DU SEL=04 FOR=I LEV=1 PLT=TU
       01                 TU00.                                         CI0114
          05              TU00-SUITE.                                   CI0114
            15       FILLER         PICTURE  X(00407).                  CI0114
       01                 TU04  REDEFINES      TU00.                    CI0114
            10            TU04-C299.                                    CI0114
            11            TU04-CTID.                                    CI0114
            12            TU04-CTIDA  PICTURE  9(3).                    CI0114
            12            TU04-CTIDN.                                   CI0114
            13            TU04-CTIDNP PICTURE  X(13).                   CI0114
            13            TU04-CTIDND PICTURE  9(11).                   CI0114
            10            TU04-IPOCH  PICTURE  X.                       CI0114
            10            TU04-FILLER PICTURE  X(099).                  CI0114
            10            TU04-CTTLN1 PICTURE  X(30).                   CI0114
            10            TU04-CTTLN2 PICTURE  X(30).                   CI0114
            10            TU04-CTTLN3 PICTURE  X(30).                   CI0114
            10            TU04-CTTBO1 PICTURE  X(45).                   CI0114
            10            TU04-CTTBO2 PICTURE  X(45).                   CI0114
            10            TU04-CTOWN  PICTURE  9(3).                    CI0114
            10            TU04-IUGMA  PICTURE  X.                       CI0114
            10            TU04-FILLER PICTURE  X(096).                  CI0114

      *>>>>>> Common CICS Fields for VSAM file processing               ADU125
      *              (As used by macro ADU104)                          ADU125
                                                                        ADU125
       01               W-CICS-FIELDS.                                  ADU125
           05           LTH               PIC 9(4) COMP  VALUE ZERO.    ADU125
                                                                        ADU125
      *>>>>>> VSAM File error handling fields used for CI0013           ADU125
                                                                        ADU125
       01               W-PASS-CI0013.                                  ADU125
           05           W-PASS-VSFUNC     PIC X(8).                     ADU125
           05           W-PASS-EIBDS      PIC X(8).                     ADU125
           05           W-PASS-EIBRCODE   PIC X(6).                     ADU125
           05           W-PASS-IK         PIC X(1).                     ADU125
      *>>>>> Working Storage Fields for VSAM Access of WM18             ADU104
                                                                        ADU104
       01               5-WM18.                                         ADU104
      *!WI pl=VS104                                                     ADU104
           05           5-WM18-NDDNA       VALUE 'EZTAUTO'              ADU104
                        PICTURE X(8).                                   CI0114
      *!WI pl=VS105                                                     ADU104
           05           5-WM18-NRCLN       VALUE +1300                  ADU104
                        PICTURE S9(4)                                   CI0114
                          BINARY.                                       CI0114
      *!WI pl=VS106                                                     ADU104
           05           5-WM18-CVSFC                                    ADU104
                        PICTURE X(4).                                   CI0114
           05             WM18-CF          PIC X VALUE '0'.             ADU104
                                                                        ADU104
      *ERROR MSG INFO RETURNED FROM CI0013
       01  VSAM-MSG.
           05  FILLER              PIC  X(13) VALUE  SPACES.
           05  VSAM-MSG-CODE       PIC  X(02) VALUE  SPACES.
           05  FILLER              PIC  X(02) VALUE  SPACES.
           05  VSAM-MSG-TEXT       PIC  X(63) VALUE  SPACES.

      ******************************************************************
      **     WORKING STORAGE MISC FIELDS                               *
      ******************************************************************

       01  W-WORK-MISC.

      *    INDICATES IF TA5A TABLE ENTRY FOUND
      *    '0' = FOUND
      *    '1' = NOT FOUND
           05  TA5A-IK          PIC X.

      *    PRIMING AREAS FOR CI0025 READ
      *!WI
           05  W-CDEL1
                        PICTURE 9(3).                                   CI0114
      *!WI
           05  W-CLID4
                        PICTURE X(23).                                  CI0114

      *    WORK AREAS USED IN NAME PACKER ROUTINE
      *!WI
           05  W-CLNAM
                        PICTURE X(70).                                  CI0114
           05  W-CLNAM-SHORT    PIC X(30).
           05  W-AANA15-RETURN  PIC X.

      *INDICES ARE SET UP FOR EACH PART OF THE NAME.  USED AS FOLLOWS:  AANA15
      *IXXXXR = USED AS VARIABLE INDEX                                  AANA15
      *IXXXXL = ACTUAL # OF CHARACTERS IN EACH FIELD                    AANA15
      *IXXXXM = MAXIMUM # OF CHARACTERS IN EACH FIELD                   AANA15
      *                 7-NAMH                                          AANA15
      *                 7-NAMF                                          AANA15
      *                 7-NAMM                                          AANA15
      *                 7-NAML                                          AANA15
      *                 7-NAMS                                          AANA15
      *                 7-TOTL                                          AANA15
      *                 7-NMWK                                          AANA15
       01   DEBUT-WSS.                                                  CI0114
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0114
            05   IK     PICTURE X.                                      CI0114
       01  CONSTANTES-PAC.                                              CI0114
           05  FILLER  PICTURE X(87)   VALUE                            CI0114
                     '6015 CAT09/08/14CI0114ADMIN   14:34:55CI0114P AMERCI0114
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0114
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0114
           05  NUGNA   PICTURE X(5).                                    CI0114
           05  APPLI   PICTURE X(3).                                    CI0114
           05  DATGN   PICTURE X(8).                                    CI0114
           05  PROGR   PICTURE X(6).                                    CI0114
           05  CODUTI  PICTURE X(8).                                    CI0114
           05  TIMGN   PICTURE X(8).                                    CI0114
           05  PROGE   PICTURE X(8).                                    CI0114
           05  COBASE  PICTURE X(4).                                    CI0114
           05  DATGNC  PICTURE X(10).                                   CI0114
           05  RELEAS  PICTURE X(7).                                    CI0114
           05  DATGE   PICTURE X(10).                                   CI0114
           05  DATSQ   PICTURE X(10).                                   CI0114
       01  DATCE.                                                       CI0114
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0114
         05  DATOR.                                                     CI0114
           10  DATOA  PICTURE XX.                                       CI0114
           10  DATOM  PICTURE XX.                                       CI0114
           10  DATOJ  PICTURE XX.                                       CI0114
       01   VARIABLES-CONDITIONNELLES.                                  CI0114
            05                  FT      PICTURE X VALUE '0'.            CI0114
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0114
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0114
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           INAMFL PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMFR PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMFM PICTURE S9(4) VALUE +0020.              AANA15
            05           INAMHL PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMHR PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMHM PICTURE S9(4) VALUE +0006.              AANA15
            05           INAMLL PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMLR PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMLM PICTURE S9(4) VALUE +0025.              AANA15
            05           INAMML PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMMR PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMMM PICTURE S9(4) VALUE +0015.              AANA15
            05           INAMSL PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMSR PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMSM PICTURE S9(4) VALUE +0004.              AANA15
            05           INMWKL PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INMWKR PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INMWKM PICTURE S9(4) VALUE +0025.              AANA15
            05           ITOTLL PICTURE S9(4) VALUE  ZERO.              AANA15
            05           ITOTLR PICTURE S9(4) VALUE  ZERO.              AANA15
            05           ITOTLM PICTURE S9(4) VALUE +0074.              AANA15
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0114
            05       5-BR00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0114
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0114
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0114
            05       5-TO00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0114
       01               S-CL01-SSA.                                     CI0114
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0114
                                      VALUE 'CL01    '.                 CI0114
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0114
            10          S-CL01-CCOD   PICTURE X(5)                      CI0114
                                      VALUE '-----'.                    CI0114
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0114
       01            S-CLU01-SSA.                                       CI0114
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0114
                                      VALUE 'CL01    '.                 CI0114
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0114
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0114
                                      VALUE '-----'.                    CI0114
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0114
                                      VALUE '(CL01K'.                   CI0114
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0114
            10       S-CLU01-CL01K.                                     CI0114
            11       S-CLU01-C199.                                      CI0114
            12       S-CLU01-CLID.                                      CI0114
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0114
            13       S-CLU01-CLIDN.                                     CI0114
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0114
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0114
            10  FILLER   PICTURE X    VALUE ')'.                        CI0114
       01               S-CL03-SSA.                                     CI0114
            10         S1-CL03-SEGNAM PICTURE X(8)                      CI0114
                                      VALUE 'CL03    '.                 CI0114
            10         S1-CL03-CCOM   PICTURE X VALUE '*'.              CI0114
            10          S-CL03-CCOD   PICTURE X(5)                      CI0114
                                      VALUE '-----'.                    CI0114
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0114
       01            S-CLA03-SSA.                                       CI0114
            10      S1-CLA03-SEGNAM PICTURE X(8)                        CI0114
                                      VALUE 'CL03    '.                 CI0114
            10      S1-CLA03-CCOM   PICTURE X VALUE '*'.                CI0114
            10       S-CLA03-CCOD   PICTURE X(5)                        CI0114
                                      VALUE '-----'.                    CI0114
            10      S1-CLA03-FLDNAM PICTURE X(9)                        CI0114
                                      VALUE '(CLDOD'.                   CI0114
            10       S-CLA03-OPER  PICTURE XX VALUE ' ='.               CI0114
            10       S-CLA03-CLDOD    PICTURE  9(8).                    CI0114
            10  FILLER   PICTURE X    VALUE ')'.                        CI0114
       01               S-CL12-SSA.                                     CI0114
            10         S1-CL12-SEGNAM PICTURE X(8)                      CI0114
                                      VALUE 'CL12    '.                 CI0114
            10         S1-CL12-CCOM   PICTURE X VALUE '*'.              CI0114
            10          S-CL12-CCOD   PICTURE X(5)                      CI0114
                                      VALUE '-----'.                    CI0114
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0114
       01               S-CX01-SSA.                                     CI0114
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0114
                                      VALUE 'CX01    '.                 CI0114
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0114
            10          S-CX01-CCOD   PICTURE X(5)                      CI0114
                                      VALUE '-----'.                    CI0114
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0114
       01            S-CXU01-SSA.                                       CI0114
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0114
                                      VALUE 'CX01    '.                 CI0114
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0114
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0114
                                      VALUE '-----'.                    CI0114
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0114
                                      VALUE '(CX01K'.                   CI0114
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0114
            10       S-CXU01-CX01K.                                     CI0114
            11       S-CXU01-C199.                                      CI0114
            12       S-CXU01-CLID.                                      CI0114
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0114
            13       S-CXU01-CLIDN.                                     CI0114
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0114
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0114
            10  FILLER   PICTURE X    VALUE ')'.                        CI0114
       01               S-CX18-SSA.                                     CI0114
            10         S1-CX18-SEGNAM PICTURE X(8)                      CI0114
                                      VALUE 'CX18    '.                 CI0114
            10         S1-CX18-CCOM   PICTURE X VALUE '*'.              CI0114
            10          S-CX18-CCOD   PICTURE X(5)                      CI0114
                                      VALUE '-----'.                    CI0114
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0114
       01            S-CXA18-SSA.                                       CI0114
            10      S1-CXA18-SEGNAM PICTURE X(8)                        CI0114
                                      VALUE 'CX18    '.                 CI0114
            10      S1-CXA18-CCOM   PICTURE X VALUE '*'.                CI0114
            10       S-CXA18-CCOD   PICTURE X(5)                        CI0114
                                      VALUE '-----'.                    CI0114
            10      S1-CXA18-FLDNAM PICTURE X(9)                        CI0114
                                      VALUE '(CSTPRE'.                  CI0114
            10       S-CXA18-OPER  PICTURE XX VALUE ' ='.               CI0114
            10       S-CXA18-CSTPRE   PICTURE  99.                      CI0114
            10  FILLER   PICTURE X    VALUE ')'.                        CI0114
       01            S-CXB18-SSA.                                       CI0114
            10      S1-CXB18-SEGNAM PICTURE X(8)                        CI0114
                                      VALUE 'CX18    '.                 CI0114
            10      S1-CXB18-CCOM   PICTURE X VALUE '*'.                CI0114
            10       S-CXB18-CCOD   PICTURE X(5)                        CI0114
                                      VALUE '-----'.                    CI0114
            10      S1-CXB18-FLDNAM PICTURE X(9)                        CI0114
                                      VALUE '(CSPCR'.                   CI0114
            10       S-CXB18-OPER  PICTURE XX VALUE ' ='.               CI0114
            10       S-CXB18-CSPCR    PICTURE  99.                      CI0114
            10  FILLER   PICTURE X    VALUE ')'.                        CI0114
       01            S-CXU18-SSA.                                       CI0114
            10      S1-CXU18-SEGNAM PICTURE X(8)                        CI0114
                                      VALUE 'CX18    '.                 CI0114
            10      S1-CXU18-CCOM   PICTURE X VALUE '*'.                CI0114
            10       S-CXU18-CCOD   PICTURE X(5)                        CI0114
                                      VALUE '-----'.                    CI0114
            10      S1-CXU18-FLDNAM PICTURE X(9)                        CI0114
                                      VALUE '(CX18K'.                   CI0114
            10       S-CXU18-OPER  PICTURE XX VALUE ' ='.               CI0114
            10       S-CXU18-CX18K.                                     CI0114
            11       S-CXU18-NBASQ    PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            10  FILLER   PICTURE X    VALUE ')'.                        CI0114
       01               S-CX21-SSA.                                     CI0114
            10         S1-CX21-SEGNAM PICTURE X(8)                      CI0114
                                      VALUE 'CX21    '.                 CI0114
            10         S1-CX21-CCOM   PICTURE X VALUE '*'.              CI0114
            10          S-CX21-CCOD   PICTURE X(5)                      CI0114
                                      VALUE '-----'.                    CI0114
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0114
       01            S-CXA21-SSA.                                       CI0114
            11      S1-CXA21-SEGNAM PICTURE X(8)                        CI0114
                                      VALUE 'CX21    '.                 CI0114
            11      S1-CXA21-CCOM   PICTURE X VALUE '*'.                CI0114
            11       S-CXA21-CCOD   PICTURE X(5)                        CI0114
                                      VALUE '-----'.                    CI0114
            11      S1-CXA21-FLDNAM PICTURE X(9)                        CI0114
                                      VALUE '(GECSQ1'.                  CI0114
            11       S-CXA21-OPER  PICTURE XX VALUE ' ='.               CI0114
            11       S-CXA21-GECSQ1   PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            11  FILLER   PICTURE X    VALUE ')'.                        CI0114
       01            S-CXU21-SSA.                                       CI0114
            11      S1-CXU21-SEGNAM PICTURE X(8)                        CI0114
                                      VALUE 'CX21    '.                 CI0114
            11      S1-CXU21-CCOM   PICTURE X VALUE '*'.                CI0114
            11       S-CXU21-CCOD   PICTURE X(5)                        CI0114
                                      VALUE '-----'.                    CI0114
            11      S1-CXU21-FLDNAM PICTURE X(9)                        CI0114
                                      VALUE '(CX21K'.                   CI0114
            11       S-CXU21-OPER  PICTURE XX VALUE ' ='.               CI0114
            11       S-CXU21-CX21K.                                     CI0114
            12       S-CXU21-CDEL1    PICTURE  9(3).                    CI0114
            12       S-CXU21-NDELS    PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            11  FILLER   PICTURE X    VALUE ')'.                        CI0114
       01               S-TO01-SSA.                                     CI0114
            10         S1-TO01-SEGNAM PICTURE X(8)                      CI0114
                                      VALUE 'CT01    '.                 CI0114
            10         S1-TO01-CCOM   PICTURE X VALUE '*'.              CI0114
            10          S-TO01-CCOD   PICTURE X(5)                      CI0114
                                      VALUE '-----'.                    CI0114
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0114
       01            S-TOU01-SSA.                                       CI0114
            10      S1-TOU01-SEGNAM PICTURE X(8)                        CI0114
                                      VALUE 'CT01    '.                 CI0114
            10      S1-TOU01-CCOM   PICTURE X VALUE '*'.                CI0114
            10       S-TOU01-CCOD   PICTURE X(5)                        CI0114
                                      VALUE '-----'.                    CI0114
            10      S1-TOU01-FLDNAM PICTURE X(9)                        CI0114
                                      VALUE '(CT01K'.                   CI0114
            10       S-TOU01-OPER  PICTURE XX VALUE ' ='.               CI0114
            10       S-TOU01-CT01K.                                     CI0114
            11       S-TOU01-C299.                                      CI0114
            12       S-TOU01-CTID.                                      CI0114
            13       S-TOU01-CTIDA    PICTURE  9(3).                    CI0114
            13       S-TOU01-CTIDN.                                     CI0114
            14       S-TOU01-CTIDNP   PICTURE  X(13).                   CI0114
            14       S-TOU01-CTIDND   PICTURE  9(11).                   CI0114
            10  FILLER   PICTURE X    VALUE ')'.                        CI0114
       01   ZONES-UTILISATEUR PICTURE X.                                CI0114
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
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0114
          05              PA00-SUITE.                                   CI0114
            15       FILLER         PICTURE  X(00106).                  CI0114
       01                 PA06  REDEFINES      PA00.                    CI0114
            10            PA06-XDBPCB.                                  CI0114
            11            PA06-XDBDNM PICTURE  X(08).                   CI0114
            11            PA06-XSEGLV PICTURE  X(02).                   CI0114
            11            PA06-XRC    PICTURE  X(02).                   CI0114
            11            PA06-XPROPT PICTURE  X(04).                   CI0114
            11            PA06-FILLER PICTURE  S9(5)                    CI0114
                          BINARY.                                       CI0114
            11            PA06-XSEGNM PICTURE  X(08).                   CI0114
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0114
                          BINARY.                                       CI0114
            11            PA06-XSEGNB PICTURE  9(05)                    CI0114
                          BINARY.                                       CI0114
            11            PA06-XCOKEY PICTURE  X(70).                   CI0114
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0114
          05              PB00-SUITE.                                   CI0114
            15       FILLER         PICTURE  X(00106).                  CI0114
       01                 PB06  REDEFINES      PB00.                    CI0114
            10            PB06-XDBPCB.                                  CI0114
            11            PB06-XDBDNM PICTURE  X(08).                   CI0114
            11            PB06-XSEGLV PICTURE  X(02).                   CI0114
            11            PB06-XRC    PICTURE  X(02).                   CI0114
            11            PB06-XPROPT PICTURE  X(04).                   CI0114
            11            PB06-FILLER PICTURE  S9(5)                    CI0114
                          BINARY.                                       CI0114
            11            PB06-XSEGNM PICTURE  X(08).                   CI0114
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0114
                          BINARY.                                       CI0114
            11            PB06-XSEGNB PICTURE  9(05)                    CI0114
                          BINARY.                                       CI0114
            11            PB06-XCOKEY PICTURE  X(70).                   CI0114
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0114
          05              PC00-SUITE.                                   CI0114
            15       FILLER         PICTURE  X(00106).                  CI0114
       01                 PC06  REDEFINES      PC00.                    CI0114
            10            PC06-XDBPCB.                                  CI0114
            11            PC06-XDBDNM PICTURE  X(08).                   CI0114
            11            PC06-XSEGLV PICTURE  X(02).                   CI0114
            11            PC06-XRC    PICTURE  X(02).                   CI0114
            11            PC06-XPROPT PICTURE  X(04).                   CI0114
            11            PC06-FILLER PICTURE  S9(5)                    CI0114
                          BINARY.                                       CI0114
            11            PC06-XSEGNM PICTURE  X(08).                   CI0114
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0114
                          BINARY.                                       CI0114
            11            PC06-XSEGNB PICTURE  9(05)                    CI0114
                          BINARY.                                       CI0114
            11            PC06-XCOKEY PICTURE  X(70).                   CI0114
      *
      ******************************************************************
      **  THIS SEGMENT CONTAINS THE LAYOUT FOR THE VSAM FILE - EZTAUTO *
      ******************************************************************
      *
      *!WF DSP=WM DSL=PJ SEL=18 FOR=I LEV=1 PLT=05
       01                 WM00.                                         CI0114
          05              WM00-SUITE.                                   CI0114
            15       FILLER         PICTURE  X(01300).                  CI0114
       01                 WM18  REDEFINES      WM00.                    CI0114
            10            WM18-GETRK.                                   CI0114
            11            WM18-CCONFA PICTURE  X(12).                   CI0114
            11            WM18-MAPPN  PICTURE  X(10).                   CI0114
            11            WM18-CLID   PICTURE  X(23).                   CI0114
            11            WM18-CTID   PICTURE  X(27).                   CI0114
            11            WM18-NSEQA  PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            10            WM18-GEDAT.                                   CI0114
            11            WM18-MREQN  PICTURE  X(45).                   CI0114
            11            WM18-TREQT  PICTURE  X(30).                   CI0114
            11            WM18-GEOPD2 PICTURE  X(8).                    CI0114
            11            WM18-CLIDP  PICTURE  X(23).                   CI0114
            11            WM18-DTIME  PICTURE  X(11).                   CI0114
            11            WM18-CENM   PICTURE  X(30).                   CI0114
            11            WM18-CLORN1 PICTURE  X(45).                   CI0114
            11            WM18-CLDOB  PICTURE  9(8).                    CI0114
            11            WM18-CLTIN  PICTURE  9(12).                   CI0114
            11            WM18-MREQ   PICTURE  X(10).                   CI0114
            11            WM18-DCACG  PICTURE  9(8).                    CI0114
            11            WM18-DEFFT  PICTURE  9(8).                    CI0114
            11            WM18-TTRTP  PICTURE  X(30).                   CI0114
            11            WM18-CACTA  PICTURE  X(1).                    CI0114
            11            WM18-CLORN  PICTURE  X(45).                   CI0114
            11            WM18-NTR    PICTURE  9(8).                    CI0114
            11            WM18-NPBN   PICTURE  X(20).                   CI0114
            11            WM18-CCBAT  PICTURE  99.                      CI0114
            11            WM18-ADBRQ  PICTURE  S9(11)V99                CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            WM18-PACT1  PICTURE  S999V999                 CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            WM18-QSHOWQ PICTURE  S9(9)V999                CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            WM18-PWHLD  PICTURE  S999V9(5)                CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            WM18-CTWHAT PICTURE  S9(7)V99                 CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            WM18-CPMTF  PICTURE  99.                      CI0114
            11            WM18-CPMTCB PICTURE  X(3).                    CI0114
            11            WM18-CIRMO  PICTURE  X(12).                   CI0114
            11            WM18-GESTD  PICTURE  9(8).                    CI0114
            11            WM18-GEEND  PICTURE  9(8).                    CI0114
            11            WM18-MCSIG  PICTURE  X(30).                   CI0114
            11            WM18-CPAYC  PICTURE  X(2).                    CI0114
            11            WM18-CPAYF  PICTURE  X(2).                    CI0114
            11            WM18-CTID01 PICTURE  X(27).                   CI0114
            11            WM18-CTTLN1 PICTURE  X(30).                   CI0114
            11            WM18-CTTLN2 PICTURE  X(30).                   CI0114
            11            WM18-CTTBO1 PICTURE  X(45).                   CI0114
            11            WM18-CTTBO2 PICTURE  X(45).                   CI0114
            11            WM18-CTTLNA PICTURE  X(30).                   CI0114
            11            WM18-CTTLNB PICTURE  X(30).                   CI0114
            11            WM18-CTTBO3 PICTURE  X(45).                   CI0114
            11            WM18-MTTBO4 PICTURE  X(45).                   CI0114
            11            WM18-CTIDA  PICTURE  9(3).                    CI0114
            11            WM18-CTIDA1 PICTURE  9(3).                    CI0114
            11            WM18-PRSCD  PICTURE  X(9).                    CI0114
            11            WM18-CPRSC2 PICTURE  X(9).                    CI0114
            11            WM18-PRCOD  PICTURE  9(5).                    CI0114
            11            WM18-PRCODX PICTURE  9(5).                    CI0114
            11            WM18-PRCMN  PICTURE  X(20).                   CI0114
            11            WM18-PRCMN1 PICTURE  X(20).                   CI0114
            11            WM18-GECSQ  PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            WM18-CLID4  PICTURE  X(23).                   CI0114
            11            WM18-CPORT  PICTURE  X.                       CI0114
            11            WM18-CLCUS  PICTURE  99.                      CI0114
            11            WM18-NIRACM PICTURE  9(2).                    CI0114
            11            WM18-CAUAC  PICTURE  S9(5)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            WM18-GENAL1 PICTURE  X(30).                   CI0114
            11            WM18-GENAL2 PICTURE  X(30).                   CI0114
            11            WM18-GESAD1 PICTURE  X(30).                   CI0114
            11            WM18-GESAD2 PICTURE  X(30).                   CI0114
            11            WM18-GESAD3 PICTURE  X(30).                   CI0114
            11            WM18-ICUST  PICTURE  X.                       CI0114
            11            WM18-AFEET  PICTURE  S9(5)V99                 CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            WM18-ITERF  PICTURE  X.                       CI0114
            11            WM18-ATERF  PICTURE  S9(5)V99                 CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            WM18-CCDSCW PICTURE  9(2).                    CI0114
            11            WM18-ICDSC  PICTURE  X.                       CI0114
            11            WM18-CARTY  PICTURE  99.                      CI0114
            11            WM18-NARRS  PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            WM18-CDEL1  PICTURE  9(3).                    CI0114
            11            WM18-NDELS  PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            WM18-CARST  PICTURE  99.                      CI0114
            11            WM18-CARTZ  PICTURE  99.                      CI0114
            11            WM18-NPISQ  PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            WM18-NAPDS  PICTURE  S9(3)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            WM18-TSRTF  PICTURE  X(60).                   CI0114
            11            WM18-CDSTR  PICTURE  XX.                      CI0114
            11            WM18-CQACT  PICTURE  999.                     CI0114
            11            WM18-FILLER PICTURE  X(108).                  CI0114
      *
      *
      ******************************************************************
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *
      **     TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES   *
      ******************************************************************
      *
      *!WF DSP=DE DSL=DU SEL=10 FOR=I DES=1 LEV=1 PLT=85
       01                 DE10.                                         CI0114
            10            DE10-DU11.                                    CI0114
            11            DE10-XFONC  PICTURE  X(4).                    CI0114
            11            DE10-MPSBN  PICTURE  X(8).                    CI0114
            11            DE10-XDBDNM PICTURE  X(08).                   CI0114
            11            DE10-XSEGNM PICTURE  X(08).                   CI0114
            11            DE10-XRC    PICTURE  X(02).                   CI0114
            11            DE10-MSEG   PICTURE  X(08).                   CI0114
            11            DE10-XCOKEY PICTURE  X(70).                   CI0114
            11            DE10-CUIBR  PICTURE  X(01).                   CI0114
            11            DE10-CUIBA  PICTURE  X(01).                   CI0114
            11            DE10-IPBIK  PICTURE  X(1).                    CI0114
            10            DE10-DU03.                                    CI0114
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            DE10-CMSSF  PICTURE  XX.                      CI0114
            11            DE10-DU09.                                    CI0114
            12            DE10-CMESA  PICTURE  S9(9)                    CI0114
                          BINARY.                                       CI0114
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0114
                          BINARY.                                       CI0114
            12            DE10-CMESB  PICTURE  S9(9)                    CI0114
                          BINARY.                                       CI0114
            12            DE10-CMSST  PICTURE  S9(9)                    CI0114
                          BINARY.                                       CI0114
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0114
                          BINARY.                                       CI0114
            12            DE10-QELLAA PICTURE  S9(9)                    CI0114
                          BINARY.                                       CI0114
            12            DE10-TMESS4 PICTURE  X(512).                  CI0114
      *
      *
      *
      *
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0114
          05              MS00-SUITE.                                   CI0114
            15       FILLER         PICTURE  X(00542).                  CI0114
       01                 MS03  REDEFINES      MS00.                    CI0114
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            10            MS03-CMSSF  PICTURE  XX.                      CI0114
            10            MS03-DU09.                                    CI0114
            11            MS03-CMESA  PICTURE  S9(9)                    CI0114
                          BINARY.                                       CI0114
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0114
                          BINARY.                                       CI0114
            11            MS03-CMESB  PICTURE  S9(9)                    CI0114
                          BINARY.                                       CI0114
            11            MS03-CMSST  PICTURE  S9(9)                    CI0114
                          BINARY.                                       CI0114
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0114
                          BINARY.                                       CI0114
            11            MS03-QELLAA PICTURE  S9(9)                    CI0114
                          BINARY.                                       CI0114
            11            MS03-TMESS4 PICTURE  X(512).                  CI0114
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0114
            10            MX11-QMSGS  PICTURE  9(03).                   CI0114
            10            MX11-PJ09                                     CI0114
                          OCCURS       025     TIMES.                   CI0114
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0114
                          COMPUTATIONAL-3.                              CI0114
            11            MX11-CMESB  PICTURE  S9(9)                    CI0114
                          BINARY.                                       CI0114
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                WM18
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0114
      *               *                                   *             CI0114
      *               *INITIALISATIONS                    *             CI0114
      *               *                                   *             CI0114
      *               *************************************.            CI0114
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
      *N02BD.    NOTE *BAD RECORD - GET OUT               *.
       F02BD.    IF    WM18-CCONFA = SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F02BD-FN.
      *SOME PROGRAMS CALL CI0114 WHEN
      *THEY SHOULD NOT. EXAMPLE: CIMC88
      *CAN THINK IT IS WORKING WITH A
      *PHONE TRAN, WHEN ACTUALLY IT'S A
      *WRITTEN TRAN. THE INPUT CHCR
      *(HOW CHG REC'D CODE) TO MC88
      *REFERS TO A PREVIOUS TRAN, & NOT
      *THE CURRENT TRAN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F02BD-FN. EXIT.
      *N02CB.    NOTE *INITIALIZE IF AMT IS PRESENT       *.
       F02CB.                                                           lv10
      *
                 IF    WM18-ADBRQ > ZEROS                               DOT
           MOVE        ZEROS TO WM18-PACT1
           WM18-QSHOWQ.
       F02CB-FN. EXIT.
      *N02CD.    NOTE *INITIALIZE IF % IS PRESENT         *.
       F02CD.    IF    WM18-MAPPN = 'UD'                                lv10
                 NEXT SENTENCE ELSE GO TO     F02CD-FN.
      *
                 IF    WM18-PACT1 > ZEROS                               DOT
           MOVE        ZEROS TO WM18-ADBRQ
           WM18-QSHOWQ.
       F02CD-FN. EXIT.
      *N02SC.    NOTE *SET ADDRESS FOR DATABASES          *.
       F02SC.                                                           lv10
      *.
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
       F02SC-FN. EXIT.
       F02-FN.   EXIT.
      *N04CB.    NOTE *VSAM Error conditions caught       *.            ADU125
       F04CB.                                                           lv10
      *     by CICS results in                                          ADU125
      *     execution of F97VA                                          ADU125
           EXEC CICS   HANDLE CONDITION                                 ADU125
                       ERROR (F97VA)                                    ADU125
                       DUPREC (F94CB)
                       NOTOPEN (F92ZZ)                       END-EXEC.
       F04CB-FN. EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0114
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0114
      *               *                                   *             CI0114
      *               *FIN DE TRAITEMENT                  *             CI0114
      *               *                                   *             CI0114
      *               *************************************.            CI0114
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0114
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *READ TABLES FOR PRODUCT MEDIUM     *
      *               *                                   *
      *               *************************************.
       F40.      IF    WM18-MAPPN = 'UD'                                lv05
                 OR    WM18-MAPPN = 'SD'
                 OR    (WM18-MAPPN = 'BA'
                 AND   WM18-CACTA NOT = 'M')
                 NEXT SENTENCE ELSE GO TO     F40-FN.
      *NAME &
      *GET TO A/C DETAILS FOR MD04 &
      *GET TAXPAYER CLIENT ID FOR UD
      *N40BB.    NOTE *READ TA5A FOR SOURCE ACCT          *.
       F40BB.    IF    WM18-MAPPN NOT = 'BA'                            lv10
                 NEXT SENTENCE ELSE GO TO     F40BB-FN.
           INITIALIZE  TA5A
           MOVE        WM18-CTIDA TO TA5A-CTIDA
           MOVE        WM18-PRCOD TO TA5A-PRCOD.
                 IF    WM18-CTIDA = 002                                 DOT
           MOVE        WM18-PRSCD TO TA5A-PRSCD
                 ELSE
           MOVE        SPACES TO TA5A-PRSCD.
           MOVE        '0' TO TA5A-IK                                   DOT
           PERFORM     F92TA THRU F92TA-FN.
                 IF    TA5A-IK = '1'                                    DOT
      *TA5A TABLE ENTRY NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012405 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N40BD.    NOTE *MOVE THE PRODUCT MEDIUM NAME       *.
       F40BD.                                                           lv15
           MOVE        TA5A-PRCMN TO WM18-PRCMN.
       F40BD-FN. EXIT.
       F40BB-FN. EXIT.
      *N40BG.    NOTE *GET 'TO' A/C PRODUCT DETAILS       *.
       F40BG.    IF    WM18-MAPPN = 'SD'                                lv10
                 AND   (WM18-CACTA = 'D' OR 'M')
                 AND   (WM18-CPAYC = 'A' OR 'C'
                 OR    WM18-CPAYC = 'L' OR 'F'
                 OR    WM18-CPAYC = 'I' OR 'RB')
                 NEXT SENTENCE ELSE GO TO     F40BG-FN.
      *WHEN MD04 CALLS CI0114
           MOVE        WM18-CTID01 TO S-TOU01-CTID
           PERFORM     F94CT THRU F94CT-FN.
                 IF    IK = '1'                                         DOT
      *IF CT01 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012078 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N40BI.    NOTE *CT01 FOR 'TO' ACCT FOUND           *.
       F40BI.                                                           lv15
           MOVE        TO01-CTIDA TO WM18-CTIDA1
           MOVE        TO01-PRCOD TO WM18-PRCODX
           MOVE        TO01-PRSCD TO WM18-CPRSC2.
       F40BI-FN. EXIT.
       F40BG-FN. EXIT.
      *N40CB.    NOTE *READ TA5A FOR DEST ACCT            *.
       F40CB.    IF    (WM18-CPAYC = 'A' OR 'C'                         lv10
                 OR    WM18-CPAYC = 'F' OR 'L'
                 OR    WM18-CPAYC = 'I' OR 'RB')
                 OR    WM18-MAPPN = 'BA'
                 NEXT SENTENCE ELSE GO TO     F40CB-FN.
           INITIALIZE  TA5A
           MOVE        WM18-CTIDA1 TO TA5A-CTIDA
           MOVE        WM18-PRCODX TO TA5A-PRCOD.
                 IF    WM18-CTIDA1 = 002                                DOT
           MOVE        WM18-CPRSC2 TO TA5A-PRSCD
                 ELSE
           MOVE        SPACES TO TA5A-PRSCD.
           MOVE        '0' TO TA5A-IK                                   DOT
           PERFORM     F92TA THRU F92TA-FN.
                 IF    TA5A-IK = '1'                                    DOT
      *TA5A TABLE ENTRY NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012405 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N40CD.    NOTE *MOVE THE PRODUCT MEDIUM NAME       *.
       F40CD.                                                           lv15
           MOVE        TA5A-PRCMN TO WM18-PRCMN1.
       F40CD-FN. EXIT.
       F40CB-FN. EXIT.
      *N40DB.    NOTE *GET TAXPAYER CLID FOR UD           *.
       F40DB.    IF    WM18-MAPPN = 'UD'                                lv10
                 NEXT SENTENCE ELSE GO TO     F40DB-FN.
      *CALL CI0018
           PERFORM     F92HA THRU F92HA-FN
      *MOVE TAXPAYER CLID INTO LINKAGE
           MOVE        FC14-CLID01 TO WM18-CLID.
       F40DB-FN. EXIT.
       F40-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *GET CLIENT AND BANK INFO           *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50BB.    NOTE *READ CL01 FOR CALLER CLIENT        *.
       F50BB.    IF    WM18-CLIDP > ZEROS                               lv10
                 AND   WM18-TREQT = 'Client'
                 NEXT SENTENCE ELSE GO TO     F50BB-FN.
      *'Client' TEXT NEEDS TO BE IN
      *SMALLER CASE
           MOVE        WM18-CLIDP TO S-CLU01-CLID
           PERFORM     F94CL THRU F94CL-FN.
      *N50BD.    NOTE *CALLER CLIENT CL01 NOT FOUND       *.
       F50BD.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50BD-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012012 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50BD-900. GO TO F50BG-FN.
       F50BD-FN. EXIT.
      *N50BG.    NOTE *CL01 FND; MOVE CALLER CLIENT TIN   *.
       F50BG.                                                           lv15
           MOVE        CL01-CLTIN TO WM18-CLTIN.
       F50BG-FN. EXIT.
      *N50BI.    NOTE *READ CL03 FOR DOB INFO             *.
       F50BI.    IF    CL01-CLTYP = 'P'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50BI-FN.
           PERFORM     F94C1 THRU F94C1-FN.
                 IF    IK = '1'                                         DOT
      *IF CL03 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012161 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50BI-FN. EXIT.
      *N50BK.    NOTE *MOVE CALLER CLIENT DOB             *.
       F50BK.                                                           lv15
      *
           MOVE        CL03-CLDOB TO WM18-CLDOB.
       F50BK-FN. EXIT.
       F50BB-FN. EXIT.
      *N50BM.    NOTE *READ CL01 FOR CLIENT INFO          *.
       F50BM.                                                           lv10
           INITIALIZE  CL01
           MOVE        WM18-CLID TO S-CLU01-CLID
           PERFORM     F94CL THRU F94CL-FN.
      *N50BP.    NOTE *CL01 NOT FOUND                     *.
       F50BP.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50BP-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012012 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50BP-FN. EXIT.
       F50BM-FN. EXIT.
      *N50CB.    NOTE *IF CLIENT IS A PERSON, READ CL03   *.
       F50CB.    IF    CL01-CLTYP = 'P'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F50CB-FN.
           INITIALIZE  CL03
           PERFORM     F94C1 THRU F94C1-FN.
      *N50CD.    NOTE *CL03 NOT FOUND                     *.
       F50CD.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50CD-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012161 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50CD-FN. EXIT.
      *N50CG.    NOTE *CL03 FOUND; FORMAT PERSON NAME     *.
       F50CG.                                                           lv15
           MOVE        CL03-CLNAM TO W-CLNAM
      *CALL NAME PACKER ROUTINE
           PERFORM     F95 THRU F95-FN.
                 IF    W-AANA15-RETURN NOT = 'E'                        DOT
           MOVE        W-CLNAM-SHORT TO WM18-CENM
                 ELSE
           MOVE        'UNKNOWN' TO WM18-CENM.
       F50CG-FN. EXIT.
       F50CB-FN. EXIT.
      *N50CI.    NOTE *IF CLIENT IS AN ORGANIZATION,      *.
       F50CI.    IF    CL01-CLTYP = 'O'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F50CI-FN.
      *READ CL12
           PERFORM     F94C2 THRU F94C2-FN.
      *N50CJ.    NOTE *CL12 NOT FOUND                     *.
       F50CJ.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50CJ-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013854 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50CJ-900. GO TO F50CL-FN.
       F50CJ-FN. EXIT.
      *N50CL.    NOTE *CL12 FOUND; MOVE ORG NAME          *.
       F50CL.                                                           lv15
           MOVE        CL12-CLORN TO WM18-CLORN1.
       F50CL-FN. EXIT.
       F50CI-FN. EXIT.
      *N50DA.    NOTE *GOING TO A BANK ON AN SD UPDATE    *.
       F50DA.    IF    WM18-MAPPN = 'SD'                                lv10
                 AND   WM18-CPAYC = 'B'
                 AND   (WM18-CACTA = 'A' OR
                       ((WM18-CACTA = 'C' OR 'R')
                       AND WM18-CAUAC = 00003))
                 NEXT SENTENCE ELSE GO TO     F50DA-FN.
      *OR CHANGE
           MOVE        WM18-CLID TO S-CXU01-CLID
           MOVE        WM18-CDEL1 TO S-CXU21-CDEL1
           MOVE        WM18-NDELS TO S-CXU21-NDELS
      *GU CX21
           PERFORM     F94DB THRU F94DB-FN.
      *N50DB.    NOTE *CX21 NOT FOUND                     *.
       F50DB.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50DB-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012028 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50DB-900. GO TO F50DC-FN.
       F50DB-FN. EXIT.
      *N50DC.    NOTE *CX21 FOUND; READ CX18              *.
       F50DC.                                                           lv15
                 IF    WM18-CDEL1 = 001 OR 003                          DOT
      *ON WIRE OR ACH USE NBASQ
           MOVE        CX21-NBASQ TO S-CXU18-NBASQ
                 ELSE
      *ELSE USE OTHER KEY
           MOVE        CX21-NBASQT TO S-CXU18-NBASQ.
           PERFORM     F94DD THRU F94DD-FN.                             DOT
      *N50DD.    NOTE *CX18 NOT FOUND                     *.
       F50DD.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F50DD-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012027 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50DD-900. GO TO F50DE-FN.
       F50DD-FN. EXIT.
      *N50DE.    NOTE *CX18 FOUND; MOVE BANK INFO         *.
       F50DE.                                                           lv20
           MOVE        CX18-NPBN TO WM18-NPBN
           MOVE        CX18-CCBAT TO WM18-CCBAT.
       F50DE-FN. EXIT.
       F50DC-FN. EXIT.
       F50DA-FN. EXIT.
      *N50DH.    NOTE *GET BANK NAME AND RTN              *.
       F50DH.    IF    WM18-CPAYC = 'B'                                 lv10
                 OR    WM18-CPAYF = 'W'
                 OR    (WM18-MAPPN = 'BA'
                 AND   WM18-CACTA NOT = 'M')
                 NEXT SENTENCE ELSE GO TO     F50DH-FN.
      *N50DI.    NOTE *SD TO BANK                         *.
       F50DI.    IF    WM18-CPAYC = 'B'                                 lv15
                 OR    WM18-CPAYF = 'W'
                 NEXT SENTENCE ELSE GO TO     F50DI-FN.
      *OR UD WIRE TO BANK
           MOVE        WM18-CDEL1 TO W-CDEL1.
                 IF    WM18-MAPPN = 'SD'                                DOT
                 AND   WM18-CPAYC = 'B'
                 AND   (WM18-CACTA = 'A' OR
                       ((WM18-CACTA = 'C' OR 'R')
                       AND WM18-CAUAC = 00003))
      *ON AN SD UPDATE OR CHANGE
           MOVE        CX18-CLID TO W-CLID4
                 ELSE
      *USE CLID4 FROM UD OR SD DRIVER
      *MODULE FOR A BANK
           MOVE        WM18-CLID4 TO W-CLID4.
      *CALL CI0025                                                      DOT
           PERFORM     F92FA THRU F92FA-FN.
       F50DI-FN. EXIT.
      *N50DK.    NOTE *BA; CACTA NOT = M                  *.
       F50DK.    IF    WM18-MAPPN = 'BA'                                lv15
                 AND   WM18-CACTA NOT = 'M'
                 NEXT SENTENCE ELSE GO TO     F50DK-FN.
           MOVE        WM18-CLID4 TO W-CLID4
      *CALL CI0025 FOR ACH
           MOVE        003 TO W-CDEL1
           PERFORM     F92FA THRU F92FA-FN.
                 IF    BR30-IBNKI = 'N'                                 DOT
      *IF ACH BANK WAS NOT FOUND,
      *CALL CI0025 AGAIN FOR PAPER
           MOVE        002 TO W-CDEL1
           PERFORM     F92FA THRU F92FA-FN.
       F50DK-FN. EXIT.
      *N50DL.    NOTE *MOVE ORG NAME & RTN FROM CI0025    *.
       F50DL.                                                           lv15
           MOVE        BR30-CLORN TO WM18-CLORN
           MOVE        BR30-NTR TO WM18-NTR.
       F50DL-FN. EXIT.
       F50DH-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *GET OWNERSHIP                      *
      *               *                                   *
      *               *************************************.
       F55.           EXIT.                                             lv05
      *N55CB.    NOTE *GET OWNERSHIP INFO FOR SOURCE      *.
       F55CB.    IF    WM18-MAPPN NOT = 'BA'                            lv10
                 NEXT SENTENCE ELSE GO TO     F55CB-FN.
      *BY CALLING CI0003
           PERFORM     F92BA THRU F92BA-FN.
      *N55CD.    NOTE *MOVE OWNERSHIP INFO                *.
       F55CD.                                                           lv15
      *
           MOVE        FU04-CTTLN1 TO WM18-CTTLN1
           MOVE        FU04-CTTLN2 TO WM18-CTTLN2
           MOVE        FU04-CTTBO1 TO WM18-CTTBO1
           MOVE        FU04-CTTBO2 TO WM18-CTTBO2.
       F55CD-FN. EXIT.
       F55CB-FN. EXIT.
      *N55CG.    NOTE *GET OWNERSHIP INFO FOR DESTN       *.
       F55CG.    IF    (WM18-CPAYC = 'A' OR 'C'                         lv10
                 OR    WM18-CPAYC = 'F' OR 'L'
                 OR    WM18-CPAYC = 'I' OR 'RB')
                 OR    (WM18-MAPPN = 'BA'
                 AND   WM18-CACTA NOT = 'M')
                 NEXT SENTENCE ELSE GO TO     F55CG-FN.
      *BY CALLING CI0003 AGAIN
      *-> ONLY ON A TRANSFER FOR SD &
      *-> NOT WHEN CIMC89 CALLS CI0114
           PERFORM     F92CA THRU F92CA-FN.
      *N55CI.    NOTE *MOVE OWNERSHIP INFO                *.
       F55CI.                                                           lv15
           MOVE        TU04-CTTLN1 TO WM18-CTTLNA
           MOVE        TU04-CTTLN2 TO WM18-CTTLNB
           MOVE        TU04-CTTBO1 TO WM18-CTTBO3
           MOVE        TU04-CTTBO2 TO WM18-MTTBO4.
       F55CI-FN. EXIT.
       F55CG-FN. EXIT.
      *N55GD.    NOTE *ON AN SD WRITE OUT SUITABLE        *.
       F55GD.    IF    WM18-MAPPN = 'SD'                                lv10
                 NEXT SENTENCE ELSE GO TO     F55GD-FN.
      *MAPPN TO BREAK ON
      *FOR SD FUNDS (CARTZ = 01) AND
      *MOST CERT PARTIAL (CARTZ = 02),
      *MAPPN WILL REMAIN AS 'SD'
                 IF    WM18-CARTZ = 05                                  DOT
      *FUND DIVIDEND ARRANGEMENT
           MOVE        'SDDIV' TO WM18-MAPPN.
                 IF    WM18-CARTZ = 06                                  DOT
      *CERT INTEREST ARRANGEMENT
           MOVE        'SDINT' TO WM18-MAPPN.
                 IF    WM18-CARTZ = 02                                  DOT
                 AND   WM18-CTIDA = 001
                 AND   (WM18-PRCOD = 181 OR 961)
      *MKT STRAT INTRA-ACCT TRANSFERS
           MOVE        'SDINTRA' TO WM18-MAPPN.
       F55GD-FN. EXIT.
       F55-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *WRITE TO THE VSAM FILE             *
      *               *                                   *
      *               *************************************.
       F65.           EXIT.                                             lv05
      *N65BB.    NOTE *WRITE SEGMENT WM18 TO THE VSAM     *.
       F65BB.                                                           lv10
      *FILE - EZTAUTO
           PERFORM     F92DB THRU F92DB-FN.
       F65BB-FN. EXIT.
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
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *PERFORMED FUNCTIONS                *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92BA.    NOTE *CALL CI0003 - ACCT OWNER/BENE      *.            AM0003
       F92BA.                                                           lv10
      *                                                                 AM0003
      *********************************                                 AM0003
      ** THIS MODULE WILL READ THE    *                                 AM0003
      ** CONTRACT DATABASE TO GET THE *                                 AM0003
      ** ACCOUNT OWNERSHIP AND        *                                 AM0003
      ** BENEFICIARY LINES FOR THE    *                                 AM0003
      ** REQUESTED ACCOUNT NUMBER.    *                                 AM0003
      *********************************                                 AM0003
      *                                                                 AM0003
           INITIALIZE      FU04                                         AM0003
           MOVE        WM18-CTID TO FU04-CTID                           AM0003
           MOVE        'Y' TO FU04-IPOCH                                AM0003
           SET CI0003A-PCB-CT1P-PTR1 TO                                 AM0003
                       PCB-CT1P-PTR1                                    AM0003
           INITIALIZE      DE10-DU03                                    AM0003
           CALL        CI0003 USING                                     AM0003
           DFHEIBLK                                                     AM0003
           DFHCOMMAREA                                                  AM0003
           DLIUIBII                                                     AM0003
           CI0003A-PCB-ADDRESS-LIST                                     AM0003
           FU04                                                         AM0003
           DE10                                                         AM0003
           MS03.                                                        AM0003
      *N92BB.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F92BB.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F92BB-FN.                 ADU071
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
       F92BB-900. GO TO F92BC-FN.
       F92BB-FN. EXIT.
      *N92BC.    NOTE *NO ERRORS                          *.            ADU071
       F92BC.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F92BC-FN. EXIT.
       F92BA-FN. EXIT.
      *N92CA.    NOTE *CALL CI0003 - ACCT OWNER/BENE      *.            AM0003
       F92CA.                                                           lv10
      *                                                                 AM0003
      *********************************                                 AM0003
      ** THIS MODULE WILL READ THE    *                                 AM0003
      ** CONTRACT DATABASE TO GET THE *                                 AM0003
      ** ACCOUNT OWNERSHIP AND        *                                 AM0003
      ** BENEFICIARY LINES FOR THE    *                                 AM0003
      ** REQUESTED ACCOUNT NUMBER.    *                                 AM0003
      *********************************                                 AM0003
      *                                                                 AM0003
           INITIALIZE      TU04                                         AM0003
           MOVE        WM18-CTID01 TO TU04-CTID                         AM0003
           MOVE        'Y' TO TU04-IPOCH                                AM0003
           SET CI0003B-PCB-CT1P-PTR1 TO                                 AM0003
                       PCB-CT1P-PTR1                                    AM0003
           INITIALIZE      DE10-DU03                                    AM0003
           CALL        CI0003 USING                                     AM0003
           DFHEIBLK                                                     AM0003
           DFHCOMMAREA                                                  AM0003
           DLIUIBII                                                     AM0003
           CI0003B-PCB-ADDRESS-LIST                                     AM0003
           TU04                                                         AM0003
           DE10                                                         AM0003
           MS03.                                                        AM0003
      *N92CB.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F92CB.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F92CB-FN.                 ADU071
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
       F92CB-900. GO TO F92CC-FN.
       F92CB-FN. EXIT.
      *N92CC.    NOTE *NO ERRORS                          *.            ADU071
       F92CC.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F92CC-FN. EXIT.
       F92CA-FN. EXIT.
      *N92DB.    NOTE *---> WRTE of VSAM file             *.            ADU117
       F92DB.                                                           lv10
           MOVE        'WRTE' TO 5-WM18-CVSFC                           ADU117
           W-PASS-VSFUNC                                                ADU117
           MOVE        '0' TO IK.                                       ADU117
                 IF    W-PASS-VSFUNC NOT = 'RDNX'                       DOT
      *NOTE: Key must be primed unless                                  ADU117
      *access is 'Read Next'                                            ADU117
           MOVE        WM18-GETRK TO WM18-GETRK.                        ADU117
      *Execute the VSAM access                                          DOT
           PERFORM     F94CB THRU F94CB-FN.                             ADU117
                 IF    IK = '0'                                         DOT
           MOVE        '1' TO WM18-CF                                   ADU117
                 ELSE                                                   ADU117
           MOVE        '0' TO WM18-CF.                                  ADU117
      *Endif                                                            DOT
       F92DB-FN. EXIT.
      *N92FA.    NOTE *CALL CI0025 - BANK'S NAME/RTN      *.            AM0025
       F92FA.                                                           lv10
      *                                                                 AM0025
      *********************************                                 AM0025
      ** THIS MODULE WILL READ THE    *                                 AM0025
      ** CLIENT DATABASE TO GET THE   *                                 AM0025
      ** BANK'S NAME AND RTN USING THE*                                 AM0025
      ** BANK CLIENT ID NUMBER PASSED.                                  AM0025
      *********************************                                 AM0025
      *                                                                 AM0025
           INITIALIZE      BR30                                         AM0025
           MOVE        W-CLID4 TO BR30-CLID                             AM0025
           MOVE        W-CDEL1 TO BR30-CDEL1                            AM0025
           MOVE        WM18-DCACG TO BR30-DCACG                         AM0025
           MOVE        ZEROS TO BR30-NRTSQ1                             AM0025
           SET CI0025A-PCB-CL1P-PTR1 TO                                 AM0025
                       PCB-CL1P-PTR1                                    AM0025
           INITIALIZE      DE10-DU03                                    AM0025
           CALL        CI0025 USING                                     AM0025
           DFHEIBLK                                                     AM0025
           DFHCOMMAREA                                                  AM0025
           DLIUIBII                                                     AM0025
           CI0025A-PCB-ADDRESS-LIST                                     AM0025
           BR30                                                         AM0025
           DE10                                                         AM0025
           MS03.                                                        AM0025
      *N92FB.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F92FB.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F92FB-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0025 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0025 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F92FB-900. GO TO F92FC-FN.
       F92FB-FN. EXIT.
      *N92FC.    NOTE *NO ERRORS                          *.            ADU071
       F92FC.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F92FC-FN. EXIT.
       F92FA-FN. EXIT.
      *N92HA.    NOTE *CALL CI0018 - ACCT CLIENTS         *.            AM0018
       F92HA.                                                           lv10
      *                                                                 AM0018
      *********************************                                 AM0018
      ** THIS MODULE WILL READ THE    *                                 AM0018
      ** CONTRACT DATABASE TO GET THE *                                 AM0018
      ** TAXPAYER CLIENT ID AND OWNER *                                 AM0018
      ** CLIENT ID'S ASSOCIATED WITH  *                                 AM0018
      ** THE ACCOUNT NUMBER.          *                                 AM0018
      *********************************                                 AM0018
      *                                                                 AM0018
           INITIALIZE      FC14                                         AM0018
           MOVE        WM18-CTID TO FC14-CTID                           AM0018
           MOVE        WM18-DCACG TO FC14-DCACG                         AM0018
           MOVE        25 TO FC14-XIMAX                                 AM0018
           MOVE        'Y' TO FC14-IPOCH                                AM0018
           SET CI0018A-PCB-CT1P-PTR1 TO                                 AM0018
                       PCB-CT1P-PTR1                                    AM0018
           INITIALIZE      DE10-DU03                                    AM0018
           CALL        CI0018 USING                                     AM0018
           DFHEIBLK                                                     AM0018
           DFHCOMMAREA                                                  AM0018
           DLIUIBII                                                     AM0018
           CI0018A-PCB-ADDRESS-LIST                                     AM0018
           FC14                                                         AM0018
           DE10                                                         AM0018
           MS03.                                                        AM0018
      *N92HB.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F92HB.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F92HB-FN.                 ADU071
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
       F92HB-900. GO TO F92HC-FN.
       F92HB-FN. EXIT.
      *N92HC.    NOTE *NO ERRORS                          *.            ADU071
       F92HC.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F92HC-FN. EXIT.
       F92HA-FN. EXIT.
      *N92TA.    NOTE *RANDOM TABLE READ FOR TA5A         *.            ADUTAB
       F92TA.                                                           lv10
           MOVE        'R1' TO G-TA5A-TABFO                             ADUTAB
           COMPUTE     G-TA5A-LTH = 60 + G-TA5A-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA5A-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA5A)                                ADUTAB
                       LENGTH (G-TA5A-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA5A-TABCR NOT = '00'                          DOT
           PERFORM     F92TD THRU F92TD-FN.                             ADUTAB
       F92TA-FN. EXIT.
      *N92TD.    NOTE *ERROR ON TABLE READ FOR TA5A       *.
       F92TD.                                                           lv10
           MOVE        '1' TO TA5A-IK.
       F92TD-FN. EXIT.
      *N92ZZ.    NOTE *VSAM FILE NOT OPEN - SEND ERROR    *.
       F92ZZ.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013766 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F92ZZ-FN. EXIT.
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
      *N93XV.    NOTE *---> VSAM Error checks             *.            ADU125
       F93XV.                                                           lv10
      *                                                                 ADU125
      *Set IK from CI0013                                               ADU125
           MOVE        W-PASS-IK TO IK.                                 ADU125
                 IF    MS03-CMESB = +999999999                          DOT
      *Severe error encountered                                         ADU125
      *   (1) Send message                                              ADU125
      *   (2) Snap dump                                                 ADU125
      *   (3) Terminate                                                 ADU125
           MOVE        +11 TO MS03-CMESB                                ADU125
      *THIS LINE ONLY VALID IF AN RPC
           PERFORM     F98ET THRU F98ET-FN.                             ADU125
                 IF    MS03-NMESS2 NOT = ZEROS                          DOT
      *---> Process encountered error                                   ADU125
      *     and terminate                                               ADU125
      *THIS LINE ONLY VALID IF AN RPC
      *OMIT PERFORM
      *MOVE SEVERITY CODE & TERMINATE
           MOVE        11 TO MS03-CMESB
           MOVE                     ALL '1' TO FT GO TO F20.
       F93XV-FN. EXIT.
       F93-FN.   EXIT.
      *N94.      NOTE *************************************.
      *               *                                   *
      *               *PERFORMED VSAM & DATABASE CALLS    *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94CB.    NOTE *RPC VSAM File Access Macro         *.            ADU104
       F94CB.                                                           lv10
      *    for accesing EZTAUTO                                         ADU104
           ADD         +1 TO WM18-NSEQA
      *Note: All VSAM errors are                                        ADU104
      *controlled by CICS Handle                                        ADU104
      *condition as per ADU125                                          ADU104
                 IF    5-WM18-CVSFC = 'READ'                            DOT
      *VSAM read access - Random                                        ADU104
           MOVE        5-WM18-NRCLN TO LTH                              ADU104
           EXEC CICS   READ                                             ADU104
                       DATASET (5-WM18-NDDNA)                           ADU104
                       LENGTH (5-WM18-NRCLN)                            ADU104
                       RIDFLD (WM18-GETRK)                              ADU104
                       INTO (WM18)                           END-EXEC.  ADU104
                 IF    5-WM18-CVSFC = 'STBR'                            DOT
      *VSAM Start Browse                                                ADU104
           EXEC CICS   STARTBR                                          ADU104
                       DATASET (5-WM18-NDDNA)                           ADU104
                       RIDFLD (WM18-GETRK)                   END-EXEC.  ADU104
                 IF    5-WM18-CVSFC = 'RDNX'                            DOT
      *VSAM Read Next Sequential Access                                 ADU104
           MOVE        5-WM18-NRCLN TO LTH                              ADU104
           EXEC CICS   READNEXT                                         ADU104
                       DATASET (5-WM18-NDDNA)                           ADU104
                       LENGTH (5-WM18-NRCLN)                            ADU104
                       RIDFLD (WM18-GETRK)                              ADU104
                       INTO (WM18)                           END-EXEC.  ADU104
                 IF    5-WM18-CVSFC = 'EDBR'                            DOT
      *VSAM End Browse                                                  ADU104
           EXEC CICS   ENDBR                                            ADU104
                       DATASET (5-WM18-NDDNA)                END-EXEC.  ADU104
                 IF    5-WM18-CVSFC = 'WRTE'                            DOT
      *VSAM Write                                                       ADU104
           MOVE        5-WM18-NRCLN TO LTH                              ADU104
           EXEC CICS   WRITE FROM (WM18)                                ADU104
                       FILE (5-WM18-NDDNA)                              ADU104
                       LENGTH (5-WM18-NRCLN)                            ADU104
                       RIDFLD (WM18-GETRK)                   END-EXEC.  ADU104
                 IF    5-WM18-CVSFC = 'DLTE'                            DOT
      *VSAM Delete                                                      ADU104
           EXEC CICS   DELETE                                           ADU104
                       DATASET (5-WM18-NDDNA)                           ADU104
                       RIDFLD (WM18-GETRK)                   END-EXEC.  ADU104
       F94CB-FN. EXIT.
      *N94CL.    NOTE *CALL GU ON CL01                    *.            ADU026
       F94CL.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CL01                                                    ADU026
           S-CLU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CL-FN. EXIT.
      *N94CT.    NOTE *CALL GU ON TO01                    *.            ADU026
       F94CT.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'TO01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 TO01                                                    ADU026
           S-TOU01-SSA                                                  ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CT-FN. EXIT.
      *N94C1.    NOTE *CALL GU ON CL03                    *.            ADU026
       F94C1.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CL03                                                    ADU026
           S-CLU01-SSA S-CL03-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C1-FN. EXIT.
      *N94C2.    NOTE *CALL GU ON CL12                    *.            ADU026
       F94C2.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CL12                                                    ADU026
           S-CLU01-SSA S-CL12-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C2-FN. EXIT.
      *N94DB.    NOTE *CALL GU ON CX21                    *.            ADU026
       F94DB.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX21' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PC06 CX21                                                    ADU026
           S-CXU01-SSA S-CXU21-SSA                                      ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DB-FN. EXIT.
      *N94DD.    NOTE *CALL GU ON CX18                    *.            ADU026
       F94DD.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PC06 CX18                                                    ADU026
           S-CXU01-SSA S-CXU18-SSA                                      ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DD-FN. EXIT.
       F94-FN.   EXIT.
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *FORMAT NAME FROM 70 TO 30 CHARS    *
      *               *                                   *
      *               *************************************.
       F95.           EXIT.                                             lv05
      *N95QA.    NOTE *INITIALIZATION OF WORK AREAS       *.            AANA15
       F95QA.                                                           lv10
           MOVE        ZERO TO INAMHL INAMFL INAMML                     AANA15
           INAMLL INAMSL ITOTLL                                         AANA15
           MOVE        ZERO TO INAMHR INAMFR INAMMR INAMLR              AANA15
           INAMSR ITOTLR                                                AANA15
           MOVE        ZERO TO 7-NA10-9STH 7-NA10-9STF                  AANA15
           7-NA10-9STM 7-NA10-9STL                                      AANA15
           7-NA10-9STS 7-NA10-9TSPAC                                    AANA15
           MOVE        ZERO TO 7-NA10-9LSH 7-NA10-9LSF                  AANA15
           7-NA10-9LSM 7-NA10-9LSL                                      AANA15
           7-NA10-9LSS                                                  AANA15
           MOVE        SPACES TO W-AANA15-RETURN                        AANA15
           MOVE        W-CLNAM TO 7-NA10-CLNAM.                         AANA15
      *N95QD.    NOTE *COUNT CHARACTERS IN HONORIFIC      *.            AANA15
       F95QD.    IF    7-NA10-CLNAMH NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F95QD-FN.                 AANA15
           MOVE        INAMHM TO INMWKR INMWKM                          AANA15
           MOVE        7-NA10-CLNAMH TO 7-NA10-9WKNM                    AANA15
           PERFORM     F95TC THRU F95TC-FN                              AANA15
           MOVE        INMWKR TO INAMHL.                                AANA15
                 IF    INAMHL > 0                                       DOT
           PERFORM     F95TH THRU F95TH-FN                              AANA15
           MOVE        INAMHL TO 7-NA10-9LSH                            AANA15
           COMPUTE     INAMHL = INAMHL - (INMWKR - 1)                   AANA15
           MOVE        INMWKR TO 7-NA10-9STH.                           AANA15
       F95QD-FN. EXIT.
      *N95QJ.    NOTE *COUNT CHARACTERS IN FIRST NAME     *.            AANA15
       F95QJ.    IF    7-NA10-CLNAMF NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F95QJ-FN.                 AANA15
           MOVE        INAMFM TO INMWKR INMWKM                          AANA15
           MOVE        7-NA10-CLNAMF TO 7-NA10-9WKNM                    AANA15
           PERFORM     F95TC THRU F95TC-FN                              AANA15
           MOVE        INMWKR TO INAMFL.                                AANA15
                 IF    INAMFL > 0                                       DOT
           PERFORM     F95TH THRU F95TH-FN                              AANA15
           MOVE        INAMFL TO 7-NA10-9LSF                            AANA15
           COMPUTE     INAMFL = INAMFL - (INMWKR - 1)                   AANA15
           MOVE        INMWKR TO 7-NA10-9STF.                           AANA15
       F95QJ-FN. EXIT.
      *N95QO.    NOTE *COUNT CHARACTERS IN MIDDLE NAME    *.            AANA15
       F95QO.    IF    7-NA10-CLNAMM NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F95QO-FN.                 AANA15
           MOVE        INAMMM TO INMWKR INMWKM                          AANA15
           MOVE        7-NA10-CLNAMM TO 7-NA10-9WKNM                    AANA15
           PERFORM     F95TC THRU F95TC-FN                              AANA15
           MOVE        INMWKR TO INAMML.                                AANA15
                 IF    INAMML > 0                                       DOT
           PERFORM     F95TH THRU F95TH-FN                              AANA15
           MOVE        INAMML TO 7-NA10-9LSM                            AANA15
           COMPUTE     INAMML = INAMML - (INMWKR - 1)                   AANA15
           MOVE        INMWKR TO 7-NA10-9STM.                           AANA15
       F95QO-FN. EXIT.
      *N95QT.    NOTE *COUNT CHARACTERS IN LAST NAME      *.            AANA15
       F95QT.    IF    7-NA10-CLNAML NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F95QT-FN.                 AANA15
           MOVE        INAMLM TO INMWKR INMWKM                          AANA15
           MOVE        7-NA10-CLNAML TO 7-NA10-9WKNM                    AANA15
           PERFORM     F95TC THRU F95TC-FN                              AANA15
           MOVE        INMWKR TO INAMLL.                                AANA15
                 IF    INAMLL > 0                                       DOT
           PERFORM     F95TH THRU F95TH-FN                              AANA15
           MOVE        INAMLL TO 7-NA10-9LSL                            AANA15
           COMPUTE     INAMLL = INAMLL - (INMWKR - 1)                   AANA15
           MOVE        INMWKR TO 7-NA10-9STL.                           AANA15
       F95QT-FN. EXIT.
      *N95RA.    NOTE *COUNT CHARACTERS IN SUFFIX         *.            AANA15
       F95RA.    IF    7-NA10-CLNAMS NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F95RA-FN.                 AANA15
           MOVE        INAMSM TO INMWKR INMWKM                          AANA15
           MOVE        7-NA10-CLNAMS TO 7-NA10-9WKNM                    AANA15
           PERFORM     F95TC THRU F95TC-FN                              AANA15
           MOVE        INMWKR TO INAMSL.                                AANA15
                 IF    INAMSL > 0                                       DOT
           PERFORM     F95TH THRU F95TH-FN                              AANA15
           MOVE        INAMSL TO 7-NA10-9LSS                            AANA15
           COMPUTE     INAMSL = INAMSL - (INMWKR - 1)                   AANA15
           MOVE        INMWKR TO 7-NA10-9STS.                           AANA15
       F95RA-FN. EXIT.
      *N95RG.    NOTE *COUNT NUMBER OF SPACES NEEDED      *.            AANA15
       F95RG.                                                           lv15
           MOVE        4 TO 7-NA10-9TSPAC.                              AANA15
                 IF    INAMHL = ZERO                                    DOT
           SUBTRACT    1 FROM 7-NA10-9TSPAC.                            AANA15
                 IF    INAMSL = ZERO                                    DOT
           SUBTRACT    1 FROM 7-NA10-9TSPAC.                            AANA15
                 IF    INAMML = ZERO                                    DOT
           SUBTRACT    1 FROM 7-NA10-9TSPAC.                            AANA15
                 IF    INAMFL = ZERO                                    DOT
                 OR    INAMLL = ZERO                                    AANA15
           SUBTRACT    1 FROM 7-NA10-9TSPAC.                            AANA15
       F95RG-FN. EXIT.
      *N95RK.    NOTE *CALCULATE TOTAL NUMBER OF CHRS     *.            AANA15
       F95RK.                                                           lv15
           COMPUTE     ITOTLL = INAMHL +                                AANA15
           INAMFL + INAMML +                                            AANA15
           INAMLL + INAMSL +                                            AANA15
           7-NA10-9TSPAC                                                AANA15
      *IF NO NAME - ERROR                                               AANA15
                 IF    ITOTLL = ZERO                                    DOT
           MOVE        'E' TO W-AANA15-RETURN                           AANA15
               GO TO     F95RK-FN.                                      AANA15
      *PRINT FULL NAME IF SPACE PERMITS                                 AANA15
                 IF    ITOTLL NOT > 30                                  DOT
           MOVE        'HFMLS' TO 7-NA10-9PRINT                         AANA15
           MOVE        1 TO W-AANA15-RETURN                             AANA15
               GO TO     F95RK-FN.                                      AANA15
      *CALC. LENGTH WITHOUT HONORIIFIC                                  AANA15
                 IF    INAMHL > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL -                                AANA15
           INAMHL - 1.                                                  AANA15
                 IF    ITOTLL NOT > 30                                  DOT
           MOVE        ' FMLS' TO 7-NA10-9PRINT                         AANA15
           MOVE        2 TO W-AANA15-RETURN                             AANA15
               GO TO     F95RK-FN.                                      AANA15
      *CALC. LENGTH WITH MIDDLE INITIAL                                 AANA15
                 IF    INAMML > 1                                       DOT
           COMPUTE     ITOTLL = ITOTLL -                                AANA15
           INAMML + 1.                                                  AANA15
                 IF    ITOTLL NOT > 30                                  DOT
           MOVE        ' FILS' TO 7-NA10-9PRINT                         AANA15
           MOVE        3 TO W-AANA15-RETURN                             AANA15
               GO TO     F95RK-FN.                                      AANA15
      *CALC. LENGTH W/O MIDDLE INITIAL                                  AANA15
                 IF    INAMML > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL - 2.                             AANA15
                 IF    ITOTLL NOT > 30                                  DOT
           MOVE        ' F LS' TO 7-NA10-9PRINT                         AANA15
           MOVE        4 TO W-AANA15-RETURN                             AANA15
               GO TO     F95RK-FN.                                      AANA15
      *CALC. LENGTH WITH FIRST INITIAL                                  AANA15
                 IF    INAMFL > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL -                                AANA15
           INAMFL + 1.                                                  AANA15
                 IF    ITOTLL NOT > 30                                  DOT
           MOVE        ' I LS' TO 7-NA10-9PRINT                         AANA15
           MOVE        5 TO W-AANA15-RETURN                             AANA15
               GO TO     F95RK-FN.                                      AANA15
      *CALC. LENGTH WITHOUT SUFFIX                                      AANA15
                 IF    INAMSL > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL - INAMSL - 1.                    AANA15
                 IF    ITOTLL NOT > 30                                  DOT
           MOVE        ' I L ' TO 7-NA10-9PRINT                         AANA15
           MOVE        6 TO W-AANA15-RETURN                             AANA15
               GO TO     F95RK-FN.                                      AANA15
      *PRINT ONLY LAST NAME.                                            AANA15
           MOVE        '   L ' TO 7-NA10-9PRINT                         DOT
           MOVE        7 TO W-AANA15-RETURN                             AANA15
               GO TO     F95RK-FN.                                      AANA15
       F95RK-FN. EXIT.
      *N95RO.    NOTE *FORMAT NAME                        *.            AANA15
       F95RO.                                                           lv15
           MOVE        0 TO INMWKR INMWKM                               AANA15
           MOVE        1 TO ITOTLR                                      AANA15
           MOVE        SPACES TO 7-NA10-COCLNM1.                        AANA15
       F95RO-FN. EXIT.
      *N95RS.    NOTE *MOVE HONORIFIC IF POSSIBLE         *.            AANA15
       F95RS.    IF    7-NA10-9PRTH = 'H'                               lv15
                 AND   INAMHL > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F95RS-FN.                 AANA15
           MOVE        7-NA10-CLNAMH TO 7-NA10-9WKNM                    AANA15
           MOVE        7-NA10-9LSH TO INMWKM                            AANA15
           MOVE        7-NA10-9STH TO INMWKR                            AANA15
           PERFORM     F95TP THRU F95TP-FN                              AANA15
           ADD         1 TO ITOTLR.                                     AANA15
       F95RS-FN. EXIT.
      *N95RX.    NOTE *MOVE FIRST NAME IF POSSIBLE        *.            AANA15
       F95RX.    IF    7-NA10-9PRTF = 'F'                               lv15
                 AND   INAMFL > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F95RX-FN.                 AANA15
           MOVE        7-NA10-CLNAMF TO 7-NA10-9WKNM                    AANA15
           MOVE        7-NA10-9LSF TO INMWKM                            AANA15
           MOVE        7-NA10-9STF TO INMWKR                            AANA15
           PERFORM     F95TP THRU F95TP-FN                              AANA15
           ADD         1 TO ITOTLR.                                     AANA15
       F95RX-FN. EXIT.
      *N95RZ.    NOTE *MOVE FIRST INITIAL ?               *.            AANA15
       F95RZ.    IF    7-NA10-9PRTF = 'I'                               lv15
                 AND   INAMFL > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F95RZ-FN.                 AANA15
           MOVE        7-NA10-9NMF (7-NA10-9STF) TO                     AANA15
           7-NA10-COCLNM (ITOTLR)                                       AANA15
           ADD         2 TO ITOTLR.                                     AANA15
       F95RZ-FN. EXIT.
      *N95SB.    NOTE *MOVE MIDDLE NAME ?                 *.            AANA15
       F95SB.    IF    7-NA10-9PRTM = 'M'                               lv15
                 AND   INAMML > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F95SB-FN.                 AANA15
           MOVE        7-NA10-CLNAMM TO 7-NA10-9WKNM                    AANA15
           MOVE        7-NA10-9LSM TO INMWKM                            AANA15
           MOVE        7-NA10-9STM TO INMWKR                            AANA15
           PERFORM     F95TP THRU F95TP-FN                              AANA15
           ADD         1 TO ITOTLR.                                     AANA15
       F95SB-FN. EXIT.
      *N95SE.    NOTE *MOVE MIDDLE INITIAL                *.            AANA15
       F95SE.    IF    7-NA10-9PRTM = 'I'                               lv15
                 AND   INAMML > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F95SE-FN.                 AANA15
           MOVE        7-NA10-9NMM (7-NA10-9STM) TO                     AANA15
           7-NA10-COCLNM (ITOTLR)                                       AANA15
           ADD         2 TO ITOTLR.                                     AANA15
       F95SE-FN. EXIT.
      *N95SH.    NOTE *MOVE LAST NAME                     *.            AANA15
       F95SH.    IF    7-NA10-9PRTL = 'L'                               lv15
                 AND   INAMLL > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F95SH-FN.                 AANA15
           MOVE        7-NA10-CLNAML TO 7-NA10-9WKNM                    AANA15
           MOVE        7-NA10-9LSL TO INMWKM                            AANA15
           MOVE        7-NA10-9STL TO INMWKR                            AANA15
           PERFORM     F95TP THRU F95TP-FN                              AANA15
           ADD         1 TO ITOTLR.                                     AANA15
       F95SH-FN. EXIT.
      *N95SL.    NOTE *MOVE SUFFIX ?                      *.            AANA15
       F95SL.    IF    7-NA10-9PRTS = 'S'                               lv15
                 AND   INAMSL > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F95SL-FN.                 AANA15
           MOVE        7-NA10-CLNAMS TO 7-NA10-9WKNM                    AANA15
           MOVE        7-NA10-9LSS TO INMWKM                            AANA15
           MOVE        7-NA10-9STS TO INMWKR                            AANA15
           PERFORM     F95TP THRU F95TP-FN.                             AANA15
       F95SL-FN. EXIT.
      *N95SZ.    NOTE *MOVE PACKED NAME TO PASSED PARM    *.            AANA15
       F95SZ.                                                           lv15
           MOVE        7-NA10-COCLNM1 TO                                AANA15
           W-CLNAM-SHORT.                                               AANA15
       F95SZ-FN. EXIT.
       F95QA-FN. EXIT.
      *N95TC.    NOTE *DECREMENT COUNTER                  *.            AANA15
       F95TC.    IF    INMWKR > 0                                       lv10
                 AND   7-NA10-9WKNM1 (INMWKR)                           AANA15
                       = SPACES                                         AANA15
                 NEXT SENTENCE ELSE GO TO     F95TC-FN.                 AANA15
           SUBTRACT    1 FROM INMWKR.                                   AANA15
       F95TC-900. GO TO F95TC.
       F95TC-FN. EXIT.
      *N95TH.    NOTE *RESET SUBSCRIPT                    *.            AANA15
       F95TH.                                                           lv10
           MOVE        INMWKR TO INMWKM                                 AANA15
           MOVE        1 TO INMWKR                                      AANA15
           PERFORM     F95TL THRU F95TL-FN.                             AANA15
       F95TH-FN. EXIT.
      *N95TL.    NOTE *INCREMENT COUNTER                  *.            AANA15
       F95TL.    IF    INMWKR NOT >                                     lv10
                       INMWKM                                           AANA15
                 AND   7-NA10-9WKNM1 (INMWKR)                           AANA15
                       = SPACES                                         AANA15
                 NEXT SENTENCE ELSE GO TO     F95TL-FN.                 AANA15
           ADD         1 TO INMWKR.                                     AANA15
       F95TL-900. GO TO F95TL.
       F95TL-FN. EXIT.
      *N95TP.    NOTE *SAVE CHARACTERS                    *.            AANA15
       F95TP.    IF    INMWKR NOT > INMWKM                              lv10
                 NEXT SENTENCE ELSE GO TO     F95TP-FN.                 AANA15
           MOVE        7-NA10-9WKNM1 (INMWKR) TO                        AANA15
           7-NA10-COCLNM (ITOTLR)                                       AANA15
           ADD         1 TO ITOTLR INMWKR.                              AANA15
       F95TP-900. GO TO F95TP.
       F95TP-FN. EXIT.
       F95-FN.   EXIT.
      *N97VA.    NOTE *---> VSAM Error process            *.            ADU125
       F97VA.                                                           lv10
      *     Executed from CICS Handle                                   ADU125
      *     as per F04CB                                                ADU125
           INITIALIZE  MS03                                             ADU125
           MOVE        EIBDS TO W-PASS-EIBDS                            ADU125
           MOVE        EIBRCODE TO W-PASS-EIBRCODE                      ADU125
           MOVE        '1' TO IK                                        ADU125
           MOVE        IK TO W-PASS-IK                                  ADU125
      *ADU117 completed W-PASS-VSFUNC                                   ADU125
           MOVE        'CI0013P ' TO W-PASS-XPROGR                      ADU125
           CALL        W-PASS-XPROGR                                    ADU125
           USING DFHEIBLK                                               ADU125
           DFHCOMMAREA                                                  ADU125
           W-PASS-CI0013                                                ADU125
           MS03                                                         ADU125
           PERFORM     F93XV THRU F93XV-FN.                             ADU125
       F97VA-FN. EXIT.
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
