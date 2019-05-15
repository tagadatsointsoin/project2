       IDENTIFICATION DIVISION.                                         CI0001
       PROGRAM-ID.  CI0001P.                                            CI0001
      *AUTHOR.         M\M - DESTINATION INFO MODULE.                   CI0001
      *DATE-COMPILED.   09/08/14.                                       CI0001
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
      ******************************************************************$2000
      ** YEAR 2000 COMPLIANT - YES                                      $2000
      ** (THIS IS NOT CERTIFICATION FOR YEAR 2000)                      $2000
      ******************************************************************$2000
       ENVIRONMENT DIVISION.                                            CI0001
       CONFIGURATION SECTION.                                           CI0001
       SOURCE-COMPUTER. IBM-370.                                        CI0001
       OBJECT-COMPUTER. IBM-370.                                        CI0001
       DATA DIVISION.                                                   CI0001
       WORKING-STORAGE SECTION.                                         CI0001
      *                                                                 AMDU07
      ******************************************************************AMDU07
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU07
      **     ACCOUNT'S ADDRESS.  IT ALSO CONTAINS THE CL24 SEGMENT.    *AMDU07
      ******************************************************************AMDU07
      *                                                                 AMDU07
      *!WF DSP=AA DSL=DU SEL=07 FOR=I LEV=1                             AMDU07
       01                 AA00.                                         CI0001
          05              AA00-SUITE.                                   CI0001
            15       FILLER         PICTURE  X(00437).                  CI0001
       01                 AA07  REDEFINES      AA00.                    CI0001
            10            AA07-C299.                                    CI0001
            11            AA07-CTID.                                    CI0001
            12            AA07-CTIDA  PICTURE  9(3).                    CI0001
            12            AA07-CTIDN.                                   CI0001
            13            AA07-CTIDNP PICTURE  X(13).                   CI0001
            13            AA07-CTIDND PICTURE  9(11).                   CI0001
            10            AA07-DCACG  PICTURE  9(8).                    CI0001
            10            AA07-FILLER PICTURE  X(100).                  CI0001
            10            AA07-CL24.                                    CI0001
            11            AA07-GELL   PICTURE  9(4)                     CI0001
                          BINARY.                                       CI0001
            11            AA07-CL24K.                                   CI0001
            12            AA07-GECSQ  PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            11            AA07-GECSD  PICTURE  9(8).                    CI0001
            11            AA07-GECED  PICTURE  9(8).                    CI0001
            11            AA07-CREQ2  PICTURE  X.                       CI0001
            11            AA07-FILLER PICTURE  X(4).                    CI0001
            11            AA07-GECTA  PICTURE  X.                       CI0001
            11            AA07-GELCD  PICTURE  9(8).                    CI0001
            11            AA07-GEADS  PICTURE  9.                       CI0001
            11            AA07-GECIT  PICTURE  X(25).                   CI0001
            11            AA07-GECTRY PICTURE  X(20).                   CI0001
            11            AA07-GECTY  PICTURE  9(3).                    CI0001
            11            AA07-GEPCD  PICTURE  X(12).                   CI0001
            11            AA07-GEST   PICTURE  X(8).                    CI0001
            11            AA07-IRESA  PICTURE  X.                       CI0001
            11            AA07-FILLER PICTURE  X(8).                    CI0001
            11            AA07-GESAD  PICTURE  X(30)                    CI0001
                          OCCURS       003     TIMES.                   CI0001
            10            AA07-FILLER PICTURE  X(100).                  CI0001
      *                                                                 AMDU07
      *                                                                 AMDU07
      *                                                                 AMDU07
      *                                                                 AMDU07
      *                                                                 AMDU04
      ******************************************************************AMDU04
      **     SEGMENT THAT CONTAINS THE ACCOUNT OWNERSHIP AND           *AMDU04
      **     BENEFICIARY FOR REQUESTED ACCOUNT ID NUMBER               *AMDU04
      ******************************************************************AMDU04
      *                                                                 AMDU04
      *!WF DSP=AB DSL=DU SEL=04 FOR=I LEV=1                             AMDU04
       01                 AB00.                                         CI0001
          05              AB00-SUITE.                                   CI0001
            15       FILLER         PICTURE  X(00407).                  CI0001
       01                 AB04  REDEFINES      AB00.                    CI0001
            10            AB04-C299.                                    CI0001
            11            AB04-CTID.                                    CI0001
            12            AB04-CTIDA  PICTURE  9(3).                    CI0001
            12            AB04-CTIDN.                                   CI0001
            13            AB04-CTIDNP PICTURE  X(13).                   CI0001
            13            AB04-CTIDND PICTURE  9(11).                   CI0001
            10            AB04-IPOCH  PICTURE  X.                       CI0001
            10            AB04-FILLER PICTURE  X(099).                  CI0001
            10            AB04-CTTLN1 PICTURE  X(30).                   CI0001
            10            AB04-CTTLN2 PICTURE  X(30).                   CI0001
            10            AB04-CTTLN3 PICTURE  X(30).                   CI0001
            10            AB04-CTTBO1 PICTURE  X(45).                   CI0001
            10            AB04-CTTBO2 PICTURE  X(45).                   CI0001
            10            AB04-CTOWN  PICTURE  9(3).                    CI0001
            10            AB04-IUGMA  PICTURE  X.                       CI0001
            10            AB04-FILLER PICTURE  X(096).                  CI0001
      *                                                                 AMDU04
      *                                                                 AMDU04
      *                                                                 AMDU04
      *                                                                 AMDU04
      *                                                                 AMDU30
      ******************************************************************AMDU30
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET A      *AMDU30
      **     BANK'S NAME AND RTN.                                      *AMDU30
      ******************************************************************AMDU30
      *                                                                 AMDU30
      *!WF DSP=BR DSL=DU SEL=30 FOR=I LEV=1                             AMDU30
       01                 BR00.                                         CI0001
          05              BR00-SUITE.                                   CI0001
            15       FILLER         PICTURE  X(00308).                  CI0001
       01                 BR30  REDEFINES      BR00.                    CI0001
            10            BR30-C199.                                    CI0001
            11            BR30-CLID.                                    CI0001
            12            BR30-CLIDO  PICTURE  9(3).                    CI0001
            12            BR30-CLIDN.                                   CI0001
            13            BR30-CLIDNP PICTURE  X(12).                   CI0001
            13            BR30-CLIDND PICTURE  9(8).                    CI0001
            10            BR30-CDEL1  PICTURE  9(3).                    CI0001
            10            BR30-DCACG  PICTURE  9(8).                    CI0001
            10            BR30-NRTSQ1 PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            BR30-FILLER PICTURE  X(100).                  CI0001
            10            BR30-CLORN  PICTURE  X(45).                   CI0001
            10            BR30-CL18.                                    CI0001
            11            BR30-CL18K.                                   CI0001
            12            BR30-NRTSQ  PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            11            BR30-NTR    PICTURE  9(8).                    CI0001
            11            BR30-GECKD  PICTURE  9.                       CI0001
            11            BR30-GEEND  PICTURE  9(8).                    CI0001
            11            BR30-NPDIN  PICTURE  X(4).                    CI0001
            11            BR30-IRTNA  PICTURE  X.                       CI0001
            11            BR30-IRTNP  PICTURE  X.                       CI0001
            11            BR30-IRTNW  PICTURE  X.                       CI0001
            10            BR30-IBNKI  PICTURE  X.                       CI0001
            10            BR30-FILLER PICTURE  X(100).                  CI0001
      *                                                                 AMDU30
      *                                                                 AMDU30
      *                                                                 AMDU30
      *                                                                 AMDU30
      *                                                                 AMDU05
      ******************************************************************AMDU05
      **     SEGMENT THAT CONTAINS THE REQUESTED CLIENT'S ADDRESS      *AMDU05
      ******************************************************************AMDU05
      *                                                                 AMDU05
      *!WF DSP=CA DSL=DU SEL=05 FOR=I LEV=1                             AMDU05
       01                 CA00.                                         CI0001
          05              CA00-SUITE.                                   CI0001
            15       FILLER         PICTURE  X(00435).                  CI0001
       01                 CA05  REDEFINES      CA00.                    CI0001
            10            CA05-C199.                                    CI0001
            11            CA05-CLID.                                    CI0001
            12            CA05-CLIDO  PICTURE  9(3).                    CI0001
            12            CA05-CLIDN.                                   CI0001
            13            CA05-CLIDNP PICTURE  X(12).                   CI0001
            13            CA05-CLIDND PICTURE  9(8).                    CI0001
            10            CA05-GECSQ1 PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            CA05-DCACG  PICTURE  9(8).                    CI0001
            10            CA05-FILLER PICTURE  X(100).                  CI0001
            10            CA05-CL24.                                    CI0001
            11            CA05-GELL   PICTURE  9(4)                     CI0001
                          BINARY.                                       CI0001
            11            CA05-CL24K.                                   CI0001
            12            CA05-GECSQ  PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            11            CA05-GECSD  PICTURE  9(8).                    CI0001
            11            CA05-GECED  PICTURE  9(8).                    CI0001
            11            CA05-CREQ2  PICTURE  X.                       CI0001
            11            CA05-FILLER PICTURE  X(4).                    CI0001
            11            CA05-GECTA  PICTURE  X.                       CI0001
            11            CA05-GELCD  PICTURE  9(8).                    CI0001
            11            CA05-GEADS  PICTURE  9.                       CI0001
            11            CA05-GECIT  PICTURE  X(25).                   CI0001
            11            CA05-GECTRY PICTURE  X(20).                   CI0001
            11            CA05-GECTY  PICTURE  9(3).                    CI0001
            11            CA05-GEPCD  PICTURE  X(12).                   CI0001
            11            CA05-GEST   PICTURE  X(8).                    CI0001
            11            CA05-IRESA  PICTURE  X.                       CI0001
            11            CA05-FILLER PICTURE  X(8).                    CI0001
            11            CA05-GESAD  PICTURE  X(30)                    CI0001
                          OCCURS       003     TIMES.                   CI0001
            10            CA05-FILLER PICTURE  X(100).                  CI0001
      *                                                                 AMDU05
      *                                                                 AMDU05
      *                                                                 AMDU05
      *                                                                 AMDU05
       01                 CL00.                                         CI0001
            02            CL01.                                         CI0001
            10            CL01-CL01K.                                   CI0001
            11            CL01-C199.                                    CI0001
            12            CL01-CLID.                                    CI0001
            13            CL01-CLIDO  PICTURE  9(3).                    CI0001
            13            CL01-CLIDN.                                   CI0001
            14            CL01-CLIDNP PICTURE  X(12).                   CI0001
            14            CL01-CLIDND PICTURE  9(8).                    CI0001
            10            CL01-GECKD  PICTURE  9.                       CI0001
            10            CL01-GEMDA  PICTURE  9(8).                    CI0001
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0001
                          BINARY.                                       CI0001
            10            CL01-GECUC  PICTURE  99.                      CI0001
            10            CL01-CLDOR  PICTURE  9(8).                    CI0001
            10            CL01-CLLNG  PICTURE  XX.                      CI0001
            10            CL01-GESLC  PICTURE  99.                      CI0001
            10            CL01-CLTYP  PICTURE  X.                       CI0001
            10            CL01-CLCLS  PICTURE  9(3).                    CI0001
            10            CL01-CLTWRC PICTURE  99.                      CI0001
            10            CL01-CLPVC  PICTURE  99.                      CI0001
            10            CL01-CLIND  PICTURE  9(3).                    CI0001
            10            CL01-CLTRC  PICTURE  99.                      CI0001
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            CL01-AYSIDA PICTURE  9(3).                    CI0001
            10            CL01-AYSID  PICTURE  9(5).                    CI0001
            10            CL01-CLSTR  PICTURE  9(2).                    CI0001
            10            CL01-CLC11  PICTURE  X.                       CI0001
            10            CL01-CLTIN  PICTURE  9(12).                   CI0001
            10            CL01-CLTND  PICTURE  9(8).                    CI0001
            10            CL01-CLTINC PICTURE  9.                       CI0001
            10            CL01-CCDWA  PICTURE  9.                       CI0001
            10            CL01-CICES  PICTURE  X.                       CI0001
            10            CL01-CLTRA  PICTURE  9(2).                    CI0001
            10            CL01-DIRSY  PICTURE  9(4)                     CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            CL01-CFEDS  PICTURE  X.                       CI0001
            10            CL01-FILLER PICTURE  X(06).                   CI0001
            02            CL03.                                         CI0001
            10            CL03-GEDLA  PICTURE  9(8).                    CI0001
            10            CL03-DDREP  PICTURE  9(8).                    CI0001
            10            CL03-DPRFR  PICTURE  9(8).                    CI0001
            10            CL03-IACCI  PICTURE  X.                       CI0001
            10            CL03-CLDOB  PICTURE  9(8).                    CI0001
            10            CL03-CLDOD  PICTURE  9(8).                    CI0001
            10            CL03-CLDTH  PICTURE  X.                       CI0001
            10            CL03-CCINI  PICTURE  X.                       CI0001
            10            CL03-FILLER PICTURE  X(1).                    CI0001
            10            CL03-CLAIN  PICTURE  S9(11)                   CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            CL03-CCAOD  PICTURE  999.                     CI0001
            10            CL03-CLMAR  PICTURE  X.                       CI0001
            10            CL03-C198.                                    CI0001
            11            CL03-CLNAM.                                   CI0001
            12            CL03-CLNAMH PICTURE  X(6).                    CI0001
            12            CL03-CLNAMF PICTURE  X(20).                   CI0001
            12            CL03-CLNAMM.                                  CI0001
            13            CL03-CLNAMI PICTURE  X.                       CI0001
            13            CL03-CLNAMR PICTURE  X(14).                   CI0001
            12            CL03-CLNAML PICTURE  X(25).                   CI0001
            12            CL03-CLNAMS PICTURE  X(4).                    CI0001
            10            CL03-FILLER PICTURE  X(10).                   CI0001
            10            CL03-MPRFS  PICTURE  X(4).                    CI0001
            10            CL03-CLOCC  PICTURE  9(3).                    CI0001
            10            CL03-CLRET  PICTURE  X.                       CI0001
            10            CL03-IOCOB  PICTURE  X.                       CI0001
            10            CL03-CLSEX  PICTURE  X.                       CI0001
            10            CL03-CLWIL  PICTURE  X.                       CI0001
            10            CL03-GECFC  PICTURE  99.                      CI0001
            10            CL03-GECFY  PICTURE  9(4).                    CI0001
            10            CL03-ICUSC  PICTURE  X.                       CI0001
            10            CL03-MCTYC  PICTURE  X(20).                   CI0001
            10            CL03-CLWIP  PICTURE  X.                       CI0001
            10            CL03-CLCTXF PICTURE  99.                      CI0001
            10            CL03-CLCUS  PICTURE  99.                      CI0001
            10            CL03-NPDLU  PICTURE  9(5).                    CI0001
            10            CL03-CLEMI  PICTURE  X.                       CI0001
            10            CL03-GEPHNH PICTURE  X(14).                   CI0001
            10            CL03-GEPHNB PICTURE  X(14).                   CI0001
            10            CL03-GEPHNX PICTURE  9(4).                    CI0001
            10            CL03-GEPHNA PICTURE  X(14).                   CI0001
            10            CL03-FILLER PICTURE  X(3).                    CI0001
            10            CL03-IAPRT  PICTURE  X.                       CI0001
            10            CL03-CEMSC  PICTURE  X.                       CI0001
            10            CL03-CSEPS  PICTURE  X.                       CI0001
            10            CL03-CRACE  PICTURE  X.                       CI0001
            10            CL03-CNIRA  PICTURE  X.                       CI0001
            10            CL03-FILLER PICTURE  X(11).                   CI0001
            02            CL12.                                         CI0001
            10            CL12-GEDLA  PICTURE  9(8).                    CI0001
            10            CL12-CLBCD  PICTURE  9(3).                    CI0001
            10            CL12-CLFDW  PICTURE  X.                       CI0001
            10            CL12-CLOSD  PICTURE  9(8).                    CI0001
            10            CL12-CLOED  PICTURE  9(8).                    CI0001
            10            CL12-CLOEI  PICTURE  X.                       CI0001
            10            CL12-CLIBN  PICTURE  X(20).                   CI0001
            10            CL12-CLINT  PICTURE  9(3).                    CI0001
            10            CL12-CLONE  PICTURE  9(9).                    CI0001
            10            CL12-CLORC  PICTURE  99.                      CI0001
            10            CL12-CLORN  PICTURE  X(45).                   CI0001
            10            CL12-CLORP  PICTURE  X(25).                   CI0001
            10            CL12-GEPHNB PICTURE  X(14).                   CI0001
            10            CL12-GEPHNX PICTURE  9(4).                    CI0001
            10            CL12-GEPHNA PICTURE  X(14).                   CI0001
            10            CL12-GEFYE  PICTURE  9(4).                    CI0001
            10            CL12-AYCDE  PICTURE  9(3).                    CI0001
            10            CL12-AYID   PICTURE  9(5).                    CI0001
            10            CL12-CFOBO  PICTURE  99.                      CI0001
            10            CL12-CLINRG                                   CI0001
                          OCCURS       003     TIMES.                   CI0001
            11            CL12-CLIRT  PICTURE  99.                      CI0001
            11            CL12-CLINR  PICTURE  X(3).                    CI0001
            11            CL12-CLIRD  PICTURE  9(8).                    CI0001
            10            CL12-IOTXE  PICTURE  X.                       CI0001
            10            CL12-IO501  PICTURE  X.                       CI0001
            10            CL12-IOFOG  PICTURE  X.                       CI0001
            10            CL12-IOPRA  PICTURE  X.                       CI0001
            10            CL12-IOSCS  PICTURE  X.                       CI0001
            10            CL12-IACHA  PICTURE  X.                       CI0001
            10            CL12-IFORG  PICTURE  X.                       CI0001
            10            CL12-IFIND  PICTURE  X.                       CI0001
            10            CL12-CFCNT3 PICTURE  X(2).                    CI0001
            10            CL12-FILLER PICTURE  X(06).                   CI0001
            02            CL18.                                         CI0001
            10            CL18-CL18K.                                   CI0001
            11            CL18-NRTSQ  PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            CL18-NTR    PICTURE  9(8).                    CI0001
            10            CL18-GECKD  PICTURE  9.                       CI0001
            10            CL18-GEEND  PICTURE  9(8).                    CI0001
            10            CL18-NPDIN  PICTURE  X(4).                    CI0001
            10            CL18-IRTNA  PICTURE  X.                       CI0001
            10            CL18-IRTNP  PICTURE  X.                       CI0001
            10            CL18-IRTNW  PICTURE  X.                       CI0001
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU002
       01  CI0003           PIC X(8) VALUE 'CI0003P '.
       01  CI0004           PIC X(8) VALUE 'CI0004P '.                  AM0004
       01  CI0006           PIC X(8) VALUE 'CI0006P '.
       01  CI0007           PIC X(8) VALUE 'CI0007P '.
       01  CI0025           PIC X(8) VALUE 'CI0025P '.
       01                 CT00.                                         CI0001
            02            CT01.                                         CI0001
            10            CT01-CT01K.                                   CI0001
            11            CT01-C299.                                    CI0001
            12            CT01-CTID.                                    CI0001
            13            CT01-CTIDA  PICTURE  9(3).                    CI0001
            13            CT01-CTIDN.                                   CI0001
            14            CT01-CTIDNP PICTURE  X(13).                   CI0001
            14            CT01-CTIDND PICTURE  9(11).                   CI0001
            10            CT01-GECKD  PICTURE  9.                       CI0001
            10            CT01-GEMDA  PICTURE  9(8).                    CI0001
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0001
                          BINARY.                                       CI0001
            10            CT01-GECUC  PICTURE  99.                      CI0001
            10            CT01-CTAUL  PICTURE  9(3).                    CI0001
            10            CT01-DIRAC  PICTURE  9(4).                    CI0001
            10            CT01-CTCCI  PICTURE  X.                       CI0001
            10            CT01-CTCUS  PICTURE  999.                     CI0001
            10            CT01-CTEFD  PICTURE  9(8).                    CI0001
            10            CT01-CTIAD  PICTURE  9(8).                    CI0001
            10            CT01-CLCUS  PICTURE  99.                      CI0001
            10            CT01-CAMMB  PICTURE  X(3).                    CI0001
            10            CT01-CKPMM  PICTURE  X.                       CI0001
            10            CT01-CTLAD  PICTURE  9(8).                    CI0001
            10            CT01-IPERS  PICTURE  X.                       CI0001
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            CT01-CTLAT  PICTURE  9(8).                    CI0001
            10            CT01-CTLATC PICTURE  9(6).                    CI0001
            10            CT01-IMEGA  PICTURE  X.                       CI0001
            10            CT01-DIRAB  PICTURE  9(8).                    CI0001
            10            CT01-COLRQ  PICTURE  X.                       CI0001
            10            CT01-ZDA04  PICTURE  X(4).                    CI0001
            10            CT01-CTLPD  PICTURE  9(8).                    CI0001
            10            CT01-CIRASP PICTURE  9.                       CI0001
            10            CT01-CIRATP PICTURE  99.                      CI0001
            10            CT01-DRTHC  PICTURE  9(8).                    CI0001
            10            CT01-CPPTC  PICTURE  X.                       CI0001
            10            CT01-ZDA06  PICTURE  X(6).                    CI0001
            10            CT01-CTACD  PICTURE  9(8).                    CI0001
            10            CT01-CTNLI  PICTURE  X.                       CI0001
            10            CT01-CTRHO  PICTURE  9(8).                    CI0001
            10            CT01-CTSGD  PICTURE  9(8).                    CI0001
            10            CT01-CPATP  PICTURE  X(1).                    CI0001
            10            CT01-IRSTA  PICTURE  X.                       CI0001
            10            CT01-CTSTA  PICTURE  99.                      CI0001
            10            CT01-CTSSC  PICTURE  99.                      CI0001
            10            CT01-PRLIN  PICTURE  9(3).                    CI0001
            10            CT01-PRCOD  PICTURE  9(5).                    CI0001
            10            CT01-PRSCD  PICTURE  X(9).                    CI0001
            10            CT01-CTLNI  PICTURE  X.                       CI0001
            10            CT01-AYSIDA PICTURE  9(3).                    CI0001
            10            CT01-AYSID  PICTURE  9(5).                    CI0001
            10            CT01-CTBMC  PICTURE  99.                      CI0001
            10            CT01-CINAR  PICTURE  99.                      CI0001
            10            CT01-CPHTR  PICTURE  X.                       CI0001
            10            CT01-CDSTR  PICTURE  XX.                      CI0001
            10            CT01-CQACT  PICTURE  999.                     CI0001
            10            CT01-CIRAS  PICTURE  999.                     CI0001
            10            CT01-CIRAT  PICTURE  999.                     CI0001
            10            CT01-CLRAY  PICTURE  9(5).                    CI0001
            10            CT01-CATTP  PICTURE  X.                       CI0001
       01                 CX00.                                         CI0001
            02            CX01.                                         CI0001
            10            CX01-CX01K.                                   CI0001
            11            CX01-C199.                                    CI0001
            12            CX01-CLID.                                    CI0001
            13            CX01-CLIDO  PICTURE  9(3).                    CI0001
            13            CX01-CLIDN.                                   CI0001
            14            CX01-CLIDNP PICTURE  X(12).                   CI0001
            14            CX01-CLIDND PICTURE  9(8).                    CI0001
            10            CX01-GEMDA  PICTURE  9(8).                    CI0001
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0001
                          BINARY.                                       CI0001
            10            CX01-FILLER PICTURE  X(5).                    CI0001
            02            CX18.                                         CI0001
            10            CX18-CX18K.                                   CI0001
            11            CX18-NBASQ  PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            CX18-NPBN   PICTURE  X(20).                   CI0001
            10            CX18-CCBAT  PICTURE  99.                      CI0001
            10            CX18-DACHP  PICTURE  9(8).                    CI0001
            10            CX18-CSTPRE PICTURE  99.                      CI0001
            10            CX18-C199.                                    CI0001
            11            CX18-CLID.                                    CI0001
            12            CX18-CLIDO  PICTURE  9(3).                    CI0001
            12            CX18-CLIDN.                                   CI0001
            13            CX18-CLIDNP PICTURE  X(12).                   CI0001
            13            CX18-CLIDND PICTURE  9(8).                    CI0001
            10            CX18-MCSIG  PICTURE  X(30).                   CI0001
            10            CX18-CPBNU  PICTURE  X.                       CI0001
            10            CX18-CSPCR  PICTURE  99.                      CI0001
            10            CX18-DAPCR  PICTURE  9(8).                    CI0001
            10            CX18-FILLER PICTURE  XX.                      CI0001
            02            CX21.                                         CI0001
            10            CX21-GELL   PICTURE  9(4)                     CI0001
                          BINARY.                                       CI0001
            10            CX21-CZ00.                                    CI0001
            11            CX21-CX21K.                                   CI0001
            12            CX21-CDEL1  PICTURE  9(3).                    CI0001
            12            CX21-NDELS  PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            CX21-CZ99.                                    CI0001
            11            CX21-FILLER PICTURE  X(165).                  CI0001
            10            CX21-CZ01                                     CI0001
                          REDEFINES            CX21-CZ99.               CI0001
            11            CX21-NBASQ  PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            11            CX21-GECSQ  PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            CX21-CZ02                                     CI0001
                          REDEFINES            CX21-CZ99.               CI0001
            11            CX21-CPAYE  PICTURE  9(2).                    CI0001
            11            CX21-C199.                                    CI0001
            12            CX21-CLID.                                    CI0001
            13            CX21-CLIDO  PICTURE  9(3).                    CI0001
            13            CX21-CLIDN.                                   CI0001
            14            CX21-CLIDNP PICTURE  X(12).                   CI0001
            14            CX21-CLIDND PICTURE  9(8).                    CI0001
            11            CX21-GECSQ1 PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            11            CX21-NBASQT PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            11            CX21-TDELI  PICTURE  X(30).                   CI0001
      *                                                                 AMDU02
      ******************************************************************AMDU02
      **     SEGMENT THAT CONTAINS THE DESTINATION DATA FOR FORMATTING *AMDU02
      **     INTO DESTINATION INFORMATION LINES.                       *AMDU02
      ******************************************************************AMDU02
      *                                                                 AMDU02
      *!WF DSP=DI DSL=DU SEL=02 FOR=I LEV=1                             AMDU02
       01                 DI00.                                         CI0001
          05              DI00-SUITE.                                   CI0001
            15       FILLER         PICTURE  X(00800).                  CI0001
       01                 DI02  REDEFINES      DI00.                    CI0001
            10            DI02-CPAY1  PICTURE  X(2).                    CI0001
            10            DI02-ISHAD  PICTURE  X.                       CI0001
            10            DI02-CTTLN1 PICTURE  X(30).                   CI0001
            10            DI02-CTTLN2 PICTURE  X(30).                   CI0001
            10            DI02-CTTLN3 PICTURE  X(30).                   CI0001
            10            DI02-CL24.                                    CI0001
            11            DI02-GELL   PICTURE  9(4)                     CI0001
                          BINARY.                                       CI0001
            11            DI02-CL24K.                                   CI0001
            12            DI02-GECSQ  PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            11            DI02-GECSD  PICTURE  9(8).                    CI0001
            11            DI02-GECED  PICTURE  9(8).                    CI0001
            11            DI02-CREQ2  PICTURE  X.                       CI0001
            11            DI02-FILLER PICTURE  X(4).                    CI0001
            11            DI02-GECTA  PICTURE  X.                       CI0001
            11            DI02-GELCD  PICTURE  9(8).                    CI0001
            11            DI02-GEADS  PICTURE  9.                       CI0001
            11            DI02-GECIT  PICTURE  X(25).                   CI0001
            11            DI02-GECTRY PICTURE  X(20).                   CI0001
            11            DI02-GECTY  PICTURE  9(3).                    CI0001
            11            DI02-GEPCD  PICTURE  X(12).                   CI0001
            11            DI02-GEST   PICTURE  X(8).                    CI0001
            11            DI02-IRESA  PICTURE  X.                       CI0001
            11            DI02-FILLER PICTURE  X(8).                    CI0001
            11            DI02-GESAD  PICTURE  X(30)                    CI0001
                          OCCURS       003     TIMES.                   CI0001
            10            DI02-CLTYP  PICTURE  X.                       CI0001
            10            DI02-CLORN  PICTURE  X(45).                   CI0001
            10            DI02-C198.                                    CI0001
            11            DI02-CLNAM.                                   CI0001
            12            DI02-CLNAMH PICTURE  X(6).                    CI0001
            12            DI02-CLNAMF PICTURE  X(20).                   CI0001
            12            DI02-CLNAMM.                                  CI0001
            13            DI02-CLNAMI PICTURE  X.                       CI0001
            13            DI02-CLNAMR PICTURE  X(14).                   CI0001
            12            DI02-CLNAML PICTURE  X(25).                   CI0001
            12            DI02-CLNAMS PICTURE  X(4).                    CI0001
            10            DI02-TDELI  PICTURE  X(30).                   CI0001
            10            DI02-NPBN   PICTURE  X(20).                   CI0001
            10            DI02-NTR    PICTURE  9(8).                    CI0001
            10            DI02-GECKD1 PICTURE  9.                       CI0001
            10            DI02-CCBAT  PICTURE  99.                      CI0001
            10            DI02-C299.                                    CI0001
            11            DI02-CTID.                                    CI0001
            12            DI02-CTIDA  PICTURE  9(3).                    CI0001
            12            DI02-CTIDN.                                   CI0001
            13            DI02-CTIDNP PICTURE  X(13).                   CI0001
            13            DI02-CTIDND PICTURE  9(11).                   CI0001
            10            DI02-GECKD  PICTURE  9.                       CI0001
            10            DI02-PRCMN  PICTURE  X(20).                   CI0001
            10            DI02-MCSIG  PICTURE  X(30).                   CI0001
            10            DI02-MAPPN  PICTURE  X(10).                   CI0001
            10            DI02-FILLER PICTURE  X(240).                  CI0001
      *                                                                 AMDU02
      *                                                                 AMDU02
      *                                                                 AMDU02
      *                                                                 AMDU02
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0001
            10            XW05-XW06.                                    CI0001
            11            XW05-XDBPCB.                                  CI0001
            12            XW05-XDBDNM PICTURE  X(08)                    CI0001
                          VALUE                SPACE.                   CI0001
            12            XW05-XSEGLV PICTURE  X(02)                    CI0001
                          VALUE                SPACE.                   CI0001
            12            XW05-XRC    PICTURE  X(02)                    CI0001
                          VALUE                SPACE.                   CI0001
            12            XW05-XPROPT PICTURE  X(04)                    CI0001
                          VALUE                SPACE.                   CI0001
            12            XW05-FILLER PICTURE  S9(5)                    CI0001
                          VALUE                ZERO                     CI0001
                          BINARY.                                       CI0001
            12            XW05-XSEGNM PICTURE  X(08)                    CI0001
                          VALUE                SPACE.                   CI0001
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0001
                          VALUE                ZERO                     CI0001
                          BINARY.                                       CI0001
            12            XW05-XSEGNB PICTURE  9(05)                    CI0001
                          VALUE                ZERO                     CI0001
                          BINARY.                                       CI0001
            12            XW05-XCOKEY PICTURE  X(70)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            XW05-XW07.                                    CI0001
            11            XW05-XIOPCB.                                  CI0001
            12            XW05-XTERMI PICTURE  X(08)                    CI0001
                          VALUE                SPACE.                   CI0001
            12            XW05-FILLER PICTURE  XX                       CI0001
                          VALUE                SPACE.                   CI0001
            12            XW05-XRC1   PICTURE  X(02)                    CI0001
                          VALUE                SPACE.                   CI0001
            12            XW05-FILLER PICTURE  X(12)                    CI0001
                          VALUE                SPACE.                   CI0001
            12            XW05-XMODNM PICTURE  X(8)                     CI0001
                          VALUE                SPACE.                   CI0001
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0001
                          VALUE                ZERO.                    CI0001
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0001
                          VALUE                ZERO.                    CI0001
            10            XW05-XGU    PICTURE  X(4)                     CI0001
                          VALUE                'GU  '.                  CI0001
            10            XW05-XGHU   PICTURE  X(4)                     CI0001
                          VALUE                'GHU '.                  CI0001
            10            XW05-XGN    PICTURE  X(4)                     CI0001
                          VALUE                'GN  '.                  CI0001
            10            XW05-XGHN   PICTURE  X(4)                     CI0001
                          VALUE                'GHN '.                  CI0001
            10            XW05-XGNP   PICTURE  X(4)                     CI0001
                          VALUE                'GNP '.                  CI0001
            10            XW05-XGHNP  PICTURE  X(4)                     CI0001
                          VALUE                'GHNP'.                  CI0001
            10            XW05-XREPL  PICTURE  XXXX                     CI0001
                          VALUE                'REPL'.                  CI0001
            10            XW05-XISRT  PICTURE  X(4)                     CI0001
                          VALUE                'ISRT'.                  CI0001
            10            XW05-XDLET  PICTURE  X(4)                     CI0001
                          VALUE                'DLET'.                  CI0001
            10            XW05-XOPEN  PICTURE  X(4)                     CI0001
                          VALUE                'OPEN'.                  CI0001
            10            XW05-XCLSE  PICTURE  X(4)                     CI0001
                          VALUE                'CLSE'.                  CI0001
            10            XW05-XCHKP  PICTURE  X(4)                     CI0001
                          VALUE                'CHKP'.                  CI0001
            10            XW05-XXRST  PICTURE  X(4)                     CI0001
                          VALUE                'XRST'.                  CI0001
            10            XW05-XTERM  PICTURE  X(4)                     CI0001
                          VALUE                'TERM'.                  CI0001
            10            XW05-XNFPAC PICTURE  X(13)                    CI0001
                          VALUE                SPACE.                   CI0001
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0001
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *                                                                 AMDU01
      ******************************************************************AMDU01
      **     SEGMENT THAT CONTAINS THE REFORMATTED DESTINATION         *AMDU01
      **     INFORMATION LINES(WHO, WHERE AND HOW).                    *AMDU01
      ******************************************************************AMDU01
      *                                                                 AMDU01
      *!WF DSP=DL DSL=DU SEL=01 FOR=I LEV=1                             AMDU01
       01                 DL00.                                         CI0001
          05              DL00-SUITE.                                   CI0001
            15       FILLER         PICTURE  X(01202).                  CI0001
       01                 DL01  REDEFINES      DL00.                    CI0001
            10            DL01-TWHOL1 PICTURE  X(100).                  CI0001
            10            DL01-TWHOL2 PICTURE  X(100).                  CI0001
            10            DL01-TWHOL3 PICTURE  X(100).                  CI0001
            10            DL01-TWHRL1 PICTURE  X(100).                  CI0001
            10            DL01-TWHRL2 PICTURE  X(100).                  CI0001
            10            DL01-TWHRL3 PICTURE  X(100).                  CI0001
            10            DL01-TWHRL4 PICTURE  X(100).                  CI0001
            10            DL01-TWHRL5 PICTURE  X(100).                  CI0001
            10            DL01-TWHRL6 PICTURE  X(100).                  CI0001
            10            DL01-TWHRL7 PICTURE  X(100).                  CI0001
            10            DL01-THOWL  PICTURE  X(100).                  CI0001
            10            DL01-CPAY1  PICTURE  X(2).                    CI0001
            10            DL01-FILLER PICTURE  X(100).                  CI0001
      *                                                                 AMDU01
      *                                                                 AMDU01
      *                                                                 AMDU01
      *                                                                 AMDU01
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0001
       01  W-PRCMN-FIELDS.                                              ADU066
           05  W-PRCMN                     PIC X(20).                   ADU066
           05  FILLER REDEFINES W-PRCMN.                                ADU066
               10  W-PRCMN-TABLE OCCURS 20 TIMES.                       ADU066
                   15  W-PRCMN-X           PIC X(01).                   ADU066
           05  W-PRCMN-INDEX               PIC S9(03).                  ADU066
       01  W-MSP03-FIELDS.                                              ADU066
           05  W-MSP03                     PIC X(03).                   ADU066
           05  FILLER REDEFINES W-MSP03.                                ADU066
               10  W-MSP03-TABLE OCCURS 03 TIMES.                       ADU066
                   15  W-MSP03-X           PIC X(01).                   ADU066
           05  W-MSP03-INDEX               PIC S9(03).                  ADU066
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
                                                                        AM0003
      ******************************************************************AM0003
      **     PCB ADDRESS LIST FOR CI0003.  MODULE CI0003 WILL NEED     *AM0003
      **     PCB'S FOR:                                                *AM0003
      **                CONTRACT DATABASE(CT1P)                        *AM0003
      ******************************************************************AM0003
                                                                        AM0003
       01  CI0003C-PCB-ADDRESS-LIST.                                    AM0003
           05  CI0003C-PCB-CT1P-PTR1      POINTER.                      AM0003
                                                                        AM0004
      ******************************************************************AM0004
      **     PCB ADDRESS LIST FOR CI0004.  MODULE CI0003 WILL NEED     *AM0004
      **     PCB'S FOR:                                                *AM0004
      **                CLIENT DATABASE(CL1P)                          *AM0004
      **                CONTRACT DATABASE(CT1P)                        *AM0004
      ******************************************************************AM0004
                                                                        AM0004
       01  CI0004D-PCB-ADDRESS-LIST.                                    AM0004
           05  CI0004D-PCB-CL1P-PTR1      POINTER.                      AM0004
           05  CI0004D-PCB-CT1P-PTR1      POINTER.                      AM0004
                                                                        AM0006
      ******************************************************************AM0006
      **     PCB ADDRESS LIST FOR CI0006.  MODULE CI0003 WILL NEED     *AM0006
      **     PCB'S FOR:                                                *AM0006
      **                CLIENT DATABASE(CL1P)                          *AM0006
      ******************************************************************AM0006
                                                                        AM0006
       01  CI0006E-PCB-ADDRESS-LIST.                                    AM0006
           05  CI0006E-PCB-CL1P-PTR1      POINTER.                      AM0006
                                                                        AM0006
      ******************************************************************AM0006
      **     PCB ADDRESS LIST FOR CI0006.  MODULE CI0003 WILL NEED     *AM0006
      **     PCB'S FOR:                                                *AM0006
      **                CLIENT DATABASE(CL1P)                          *AM0006
      ******************************************************************AM0006
                                                                        AM0006
       01  CI0006F-PCB-ADDRESS-LIST.                                    AM0006
           05  CI0006F-PCB-CL1P-PTR1      POINTER.                      AM0006
                                                                        AM0006
      ******************************************************************AM0006
      **     PCB ADDRESS LIST FOR CI0006.  MODULE CI0003 WILL NEED     *AM0006
      **     PCB'S FOR:                                                *AM0006
      **                CLIENT DATABASE(CL1P)                          *AM0006
      ******************************************************************AM0006
                                                                        AM0006
       01  CI0006G-PCB-ADDRESS-LIST.                                    AM0006
           05  CI0006G-PCB-CL1P-PTR1      POINTER.                      AM0006
                                                                        AM0006
      ******************************************************************AM0006
      **     PCB ADDRESS LIST FOR CI0006.  MODULE CI0003 WILL NEED     *AM0006
      **     PCB'S FOR:                                                *AM0006
      **                CLIENT DATABASE(CL1P)                          *AM0006
      ******************************************************************AM0006
                                                                        AM0006
       01  CI0006H-PCB-ADDRESS-LIST.                                    AM0006
           05  CI0006H-PCB-CL1P-PTR1      POINTER.                      AM0006
                                                                        AM0025
      ******************************************************************AM0025
      **     PCB ADDRESS LIST FOR CI0025.  MODULE CI0025 WILL NEED     *AM0025
      **     PCB'S FOR:                                                *AM0025
      **                CLIENT DATABASE(CL1P)                          *AM0025
      ******************************************************************AM0025
                                                                        AM0025
       01  CI0025I-PCB-ADDRESS-LIST.                                    AM0025
           05  CI0025I-PCB-CL1P-PTR1      POINTER.                      AM0025
                                                                        AM0025
      ******************************************************************AM0025
      **     PCB ADDRESS LIST FOR CI0025.  MODULE CI0025 WILL NEED     *AM0025
      **     PCB'S FOR:                                                *AM0025
      **                CLIENT DATABASE(CL1P)                          *AM0025
      ******************************************************************AM0025
                                                                        AM0025
       01  CI0025J-PCB-ADDRESS-LIST.                                    AM0025
           05  CI0025J-PCB-CL1P-PTR1      POINTER.                      AM0025
      ******************************************************************ADUTAB
      **              TABLE TA5A ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA5A-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=5A FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA5A.                                                CI0001
           04    G-TA5A-PARAM.                                          CI0001
             10  G-TA5A-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0001
                        VALUE      +772.                                CI0001
             10  G-TA5A-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0001
                        VALUE      +001.                                CI0001
             10  G-TA5A-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0001
                        VALUE      +017.                                CI0001
             10  G-TA5A-NUAPP  PICTURE 99                               CI0001
                        VALUE       0.                                  CI0001
             10  G-TA5A-NUTAB  PICTURE X(6)                             CI0001
                        VALUE 'TA005A'.                                 CI0001
             10  G-TA5A-TABFO  PICTURE XX                 VALUE SPACE.  CI0001
             10  G-TA5A-TABCR  PICTURE XX                 VALUE SPACE.  CI0001
             10  G-TA5A-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0001
             10  G-TA5A-NUSSC  PICTURE X  VALUE   ' '.                  CI0001
             10  G-TA5A-NUSSY  PICTURE X                  VALUE SPACE.  CI0001
             10  G-TA5A-TRANID PICTURE X(4)               VALUE SPACE.  CI0001
             10  G-TA5A-FILSYS.                                         CI0001
             15  G-TA5A-USERC  PICTURE X(6)               VALUE SPACE.  CI0001
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0001
           04             TA5A.                                         CI0001
            10            TA5A-GAPSC.                                   CI0001
            11            TA5A-CTIDA  PICTURE  9(3)                     CI0001
                          VALUE                ZERO.                    CI0001
            11            TA5A-PRCOD  PICTURE  9(5)                     CI0001
                          VALUE                ZERO.                    CI0001
            11            TA5A-PRSCD  PICTURE  X(9)                     CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-PRCLN  PICTURE  X(60)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-PRCMN  PICTURE  X(20)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-PRCSN  PICTURE  X(9)                     CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-MRCLN1 PICTURE  X(51)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-MRCLN2 PICTURE  X(51)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-MRCLN3 PICTURE  X(51)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-MRCLN4 PICTURE  X(51)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-MRCMN2 PICTURE  X(20)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-MRCMN3 PICTURE  X(20)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-PRCCS1 PICTURE  X(15)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-PRCCS2 PICTURE  X(15)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-PRCCS3 PICTURE  X(15)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-MPCLN  PICTURE  X(45)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-MPCL1  PICTURE  X(45)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-MSP1   PICTURE  X(60)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-MSP5   PICTURE  X(30)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-MSP03  PICTURE  X(3)                     CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-MPRDG  PICTURE  X(20)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-CPRDG  PICTURE  9(2)                     CI0001
                          VALUE                ZERO.                    CI0001
            10            TA5A-MPRDA1 PICTURE  X(50)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-CPRDA1 PICTURE  9(3)                     CI0001
                          VALUE                ZERO.                    CI0001
            10            TA5A-MSP06  PICTURE  X(20)                    CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-CPOIN  PICTURE  X                        CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-CPITY  PICTURE  9(3)                     CI0001
                          VALUE                ZERO.                    CI0001
            10            TA5A-CLITY  PICTURE  9(3)                     CI0001
                          VALUE                ZERO.                    CI0001
            10            TA5A-IVARP  PICTURE  X                        CI0001
                          VALUE                SPACE.                   CI0001
            10            TA5A-CASCL  PICTURE  9(3)                     CI0001
                          VALUE                ZERO.                    CI0001
            10            TA5A-ZDA88  PICTURE  X(88)                    CI0001
                          VALUE                SPACE.                   CI0001
      **                                                                ADUTAB
       01   DEBUT-WSS.                                                  CI0001
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0001
            05   IK     PICTURE X.                                      CI0001
       01  CONSTANTES-PAC.                                              CI0001
           05  FILLER  PICTURE X(87)   VALUE                            CI0001
                     '6015 CAT09/08/14CI0001ADMIN   14:33:50CI0001P AMERCI0001
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0001
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0001
           05  NUGNA   PICTURE X(5).                                    CI0001
           05  APPLI   PICTURE X(3).                                    CI0001
           05  DATGN   PICTURE X(8).                                    CI0001
           05  PROGR   PICTURE X(6).                                    CI0001
           05  CODUTI  PICTURE X(8).                                    CI0001
           05  TIMGN   PICTURE X(8).                                    CI0001
           05  PROGE   PICTURE X(8).                                    CI0001
           05  COBASE  PICTURE X(4).                                    CI0001
           05  DATGNC  PICTURE X(10).                                   CI0001
           05  RELEAS  PICTURE X(7).                                    CI0001
           05  DATGE   PICTURE X(10).                                   CI0001
           05  DATSQ   PICTURE X(10).                                   CI0001
       01  DATCE.                                                       CI0001
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0001
         05  DATOR.                                                     CI0001
           10  DATOA  PICTURE XX.                                       CI0001
           10  DATOM  PICTURE XX.                                       CI0001
           10  DATOJ  PICTURE XX.                                       CI0001
       01   VARIABLES-CONDITIONNELLES.                                  CI0001
            05                  FT      PICTURE X VALUE '0'.            CI0001
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0001
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0001
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0001
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0001
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0001
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0001
       01               S-CL01-SSA.                                     CI0001
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0001
                                      VALUE 'CL01    '.                 CI0001
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0001
            10          S-CL01-CCOD   PICTURE X(5)                      CI0001
                                      VALUE '-----'.                    CI0001
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0001
       01            S-CLU01-SSA.                                       CI0001
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0001
                                      VALUE 'CL01    '.                 CI0001
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0001
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0001
                                      VALUE '-----'.                    CI0001
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0001
                                      VALUE '(CL01K'.                   CI0001
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0001
            10       S-CLU01-CL01K.                                     CI0001
            11       S-CLU01-C199.                                      CI0001
            12       S-CLU01-CLID.                                      CI0001
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0001
            13       S-CLU01-CLIDN.                                     CI0001
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0001
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0001
            10  FILLER   PICTURE X    VALUE ')'.                        CI0001
       01               S-CL03-SSA.                                     CI0001
            10         S1-CL03-SEGNAM PICTURE X(8)                      CI0001
                                      VALUE 'CL03    '.                 CI0001
            10         S1-CL03-CCOM   PICTURE X VALUE '*'.              CI0001
            10          S-CL03-CCOD   PICTURE X(5)                      CI0001
                                      VALUE '-----'.                    CI0001
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0001
       01            S-CLA03-SSA.                                       CI0001
            10      S1-CLA03-SEGNAM PICTURE X(8)                        CI0001
                                      VALUE 'CL03    '.                 CI0001
            10      S1-CLA03-CCOM   PICTURE X VALUE '*'.                CI0001
            10       S-CLA03-CCOD   PICTURE X(5)                        CI0001
                                      VALUE '-----'.                    CI0001
            10      S1-CLA03-FLDNAM PICTURE X(9)                        CI0001
                                      VALUE '(CLDOD'.                   CI0001
            10       S-CLA03-OPER  PICTURE XX VALUE ' ='.               CI0001
            10       S-CLA03-CLDOD    PICTURE  9(8).                    CI0001
            10  FILLER   PICTURE X    VALUE ')'.                        CI0001
       01               S-CL12-SSA.                                     CI0001
            10         S1-CL12-SEGNAM PICTURE X(8)                      CI0001
                                      VALUE 'CL12    '.                 CI0001
            10         S1-CL12-CCOM   PICTURE X VALUE '*'.              CI0001
            10          S-CL12-CCOD   PICTURE X(5)                      CI0001
                                      VALUE '-----'.                    CI0001
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0001
       01               S-CL18-SSA.                                     CI0001
            10         S1-CL18-SEGNAM PICTURE X(8)                      CI0001
                                      VALUE 'CL18    '.                 CI0001
            10         S1-CL18-CCOM   PICTURE X VALUE '*'.              CI0001
            10          S-CL18-CCOD   PICTURE X(5)                      CI0001
                                      VALUE '-----'.                    CI0001
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0001
       01            S-CLU18-SSA.                                       CI0001
            10      S1-CLU18-SEGNAM PICTURE X(8)                        CI0001
                                      VALUE 'CL18    '.                 CI0001
            10      S1-CLU18-CCOM   PICTURE X VALUE '*'.                CI0001
            10       S-CLU18-CCOD   PICTURE X(5)                        CI0001
                                      VALUE '-----'.                    CI0001
            10      S1-CLU18-FLDNAM PICTURE X(9)                        CI0001
                                      VALUE '(CL18K'.                   CI0001
            10       S-CLU18-OPER  PICTURE XX VALUE ' ='.               CI0001
            10       S-CLU18-CL18K.                                     CI0001
            11       S-CLU18-NRTSQ    PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            10  FILLER   PICTURE X    VALUE ')'.                        CI0001
       01            S-CL118-SSA.                                       CI0001
            10      S1-CL118-SEGNAM PICTURE X(8)                        CI0001
                                      VALUE 'CL18    '.                 CI0001
            10      S1-CL118-CCOM   PICTURE X VALUE '*'.                CI0001
            10       S-CL118-CCOD   PICTURE X(5)                        CI0001
                                      VALUE '-----'.                    CI0001
            10      S1-CL118-FLDNAM PICTURE X(9)                        CI0001
                                      VALUE '(XNTR'.                    CI0001
            10       S-CL118-OPER  PICTURE XX VALUE ' ='.               CI0001
            10       S-CL118-NTR      PICTURE  9(8).                    CI0001
            10  FILLER   PICTURE X    VALUE ')'.                        CI0001
       01            S-CL218-SSA.                                       CI0001
            10      S1-CL218-SEGNAM PICTURE X(8)                        CI0001
                                      VALUE 'CL18    '.                 CI0001
            10      S1-CL218-CCOM   PICTURE X VALUE '*'.                CI0001
            10       S-CL218-CCOD   PICTURE X(5)                        CI0001
                                      VALUE '-----'.                    CI0001
            10      S1-CL218-FLDNAM PICTURE X(9)                        CI0001
                                      VALUE '(XGEEND'.                  CI0001
            10       S-CL218-OPER  PICTURE XX VALUE ' ='.               CI0001
            10       S-CL218-GEEND    PICTURE  9(8).                    CI0001
            10  FILLER   PICTURE X    VALUE ')'.                        CI0001
       01            S-CL318-SSA.                                       CI0001
            10      S1-CL318-SEGNAM PICTURE X(8)                        CI0001
                                      VALUE 'CL18    '.                 CI0001
            10      S1-CL318-CCOM   PICTURE X VALUE '*'.                CI0001
            10       S-CL318-CCOD   PICTURE X(5)                        CI0001
                                      VALUE '-----'.                    CI0001
            10      S1-CL318-FLDNAM PICTURE X(9)                        CI0001
                                      VALUE '(XIRTNA'.                  CI0001
            10       S-CL318-OPER  PICTURE XX VALUE ' ='.               CI0001
            10       S-CL318-IRTNA    PICTURE  X.                       CI0001
            10  FILLER   PICTURE X    VALUE ')'.                        CI0001
       01            S-CL418-SSA.                                       CI0001
            10      S1-CL418-SEGNAM PICTURE X(8)                        CI0001
                                      VALUE 'CL18    '.                 CI0001
            10      S1-CL418-CCOM   PICTURE X VALUE '*'.                CI0001
            10       S-CL418-CCOD   PICTURE X(5)                        CI0001
                                      VALUE '-----'.                    CI0001
            10      S1-CL418-FLDNAM PICTURE X(9)                        CI0001
                                      VALUE '(XIRTNP'.                  CI0001
            10       S-CL418-OPER  PICTURE XX VALUE ' ='.               CI0001
            10       S-CL418-IRTNP    PICTURE  X.                       CI0001
            10  FILLER   PICTURE X    VALUE ')'.                        CI0001
       01               S-CT01-SSA.                                     CI0001
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0001
                                      VALUE 'CT01    '.                 CI0001
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0001
            10          S-CT01-CCOD   PICTURE X(5)                      CI0001
                                      VALUE '-----'.                    CI0001
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0001
       01            S-CTU01-SSA.                                       CI0001
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0001
                                      VALUE 'CT01    '.                 CI0001
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0001
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0001
                                      VALUE '-----'.                    CI0001
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0001
                                      VALUE '(CT01K'.                   CI0001
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0001
            10       S-CTU01-CT01K.                                     CI0001
            11       S-CTU01-C299.                                      CI0001
            12       S-CTU01-CTID.                                      CI0001
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0001
            13       S-CTU01-CTIDN.                                     CI0001
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0001
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0001
            10  FILLER   PICTURE X    VALUE ')'.                        CI0001
       01               S-CX01-SSA.                                     CI0001
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0001
                                      VALUE 'CX01    '.                 CI0001
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0001
            10          S-CX01-CCOD   PICTURE X(5)                      CI0001
                                      VALUE '-----'.                    CI0001
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0001
       01            S-CXU01-SSA.                                       CI0001
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0001
                                      VALUE 'CX01    '.                 CI0001
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0001
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0001
                                      VALUE '-----'.                    CI0001
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0001
                                      VALUE '(CX01K'.                   CI0001
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0001
            10       S-CXU01-CX01K.                                     CI0001
            11       S-CXU01-C199.                                      CI0001
            12       S-CXU01-CLID.                                      CI0001
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0001
            13       S-CXU01-CLIDN.                                     CI0001
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0001
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0001
            10  FILLER   PICTURE X    VALUE ')'.                        CI0001
       01               S-CX18-SSA.                                     CI0001
            10         S1-CX18-SEGNAM PICTURE X(8)                      CI0001
                                      VALUE 'CX18    '.                 CI0001
            10         S1-CX18-CCOM   PICTURE X VALUE '*'.              CI0001
            10          S-CX18-CCOD   PICTURE X(5)                      CI0001
                                      VALUE '-----'.                    CI0001
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0001
       01            S-CXA18-SSA.                                       CI0001
            10      S1-CXA18-SEGNAM PICTURE X(8)                        CI0001
                                      VALUE 'CX18    '.                 CI0001
            10      S1-CXA18-CCOM   PICTURE X VALUE '*'.                CI0001
            10       S-CXA18-CCOD   PICTURE X(5)                        CI0001
                                      VALUE '-----'.                    CI0001
            10      S1-CXA18-FLDNAM PICTURE X(9)                        CI0001
                                      VALUE '(CSTPRE'.                  CI0001
            10       S-CXA18-OPER  PICTURE XX VALUE ' ='.               CI0001
            10       S-CXA18-CSTPRE   PICTURE  99.                      CI0001
            10  FILLER   PICTURE X    VALUE ')'.                        CI0001
       01            S-CXB18-SSA.                                       CI0001
            10      S1-CXB18-SEGNAM PICTURE X(8)                        CI0001
                                      VALUE 'CX18    '.                 CI0001
            10      S1-CXB18-CCOM   PICTURE X VALUE '*'.                CI0001
            10       S-CXB18-CCOD   PICTURE X(5)                        CI0001
                                      VALUE '-----'.                    CI0001
            10      S1-CXB18-FLDNAM PICTURE X(9)                        CI0001
                                      VALUE '(CSPCR'.                   CI0001
            10       S-CXB18-OPER  PICTURE XX VALUE ' ='.               CI0001
            10       S-CXB18-CSPCR    PICTURE  99.                      CI0001
            10  FILLER   PICTURE X    VALUE ')'.                        CI0001
       01            S-CXU18-SSA.                                       CI0001
            10      S1-CXU18-SEGNAM PICTURE X(8)                        CI0001
                                      VALUE 'CX18    '.                 CI0001
            10      S1-CXU18-CCOM   PICTURE X VALUE '*'.                CI0001
            10       S-CXU18-CCOD   PICTURE X(5)                        CI0001
                                      VALUE '-----'.                    CI0001
            10      S1-CXU18-FLDNAM PICTURE X(9)                        CI0001
                                      VALUE '(CX18K'.                   CI0001
            10       S-CXU18-OPER  PICTURE XX VALUE ' ='.               CI0001
            10       S-CXU18-CX18K.                                     CI0001
            11       S-CXU18-NBASQ    PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            10  FILLER   PICTURE X    VALUE ')'.                        CI0001
       01               S-CX21-SSA.                                     CI0001
            10         S1-CX21-SEGNAM PICTURE X(8)                      CI0001
                                      VALUE 'CX21    '.                 CI0001
            10         S1-CX21-CCOM   PICTURE X VALUE '*'.              CI0001
            10          S-CX21-CCOD   PICTURE X(5)                      CI0001
                                      VALUE '-----'.                    CI0001
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0001
       01            S-CXA21-SSA.                                       CI0001
            11      S1-CXA21-SEGNAM PICTURE X(8)                        CI0001
                                      VALUE 'CX21    '.                 CI0001
            11      S1-CXA21-CCOM   PICTURE X VALUE '*'.                CI0001
            11       S-CXA21-CCOD   PICTURE X(5)                        CI0001
                                      VALUE '-----'.                    CI0001
            11      S1-CXA21-FLDNAM PICTURE X(9)                        CI0001
                                      VALUE '(GECSQ1'.                  CI0001
            11       S-CXA21-OPER  PICTURE XX VALUE ' ='.               CI0001
            11       S-CXA21-GECSQ1   PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            11  FILLER   PICTURE X    VALUE ')'.                        CI0001
       01            S-CXU21-SSA.                                       CI0001
            11      S1-CXU21-SEGNAM PICTURE X(8)                        CI0001
                                      VALUE 'CX21    '.                 CI0001
            11      S1-CXU21-CCOM   PICTURE X VALUE '*'.                CI0001
            11       S-CXU21-CCOD   PICTURE X(5)                        CI0001
                                      VALUE '-----'.                    CI0001
            11      S1-CXU21-FLDNAM PICTURE X(9)                        CI0001
                                      VALUE '(CX21K'.                   CI0001
            11       S-CXU21-OPER  PICTURE XX VALUE ' ='.               CI0001
            11       S-CXU21-CX21K.                                     CI0001
            12       S-CXU21-CDEL1    PICTURE  9(3).                    CI0001
            12       S-CXU21-NDELS    PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            11  FILLER   PICTURE X    VALUE ')'.                        CI0001
       01   ZONES-UTILISATEUR PICTURE X.                                CI0001
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
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0001
          05              PA00-SUITE.                                   CI0001
            15       FILLER         PICTURE  X(00106).                  CI0001
       01                 PA06  REDEFINES      PA00.                    CI0001
            10            PA06-XDBPCB.                                  CI0001
            11            PA06-XDBDNM PICTURE  X(08).                   CI0001
            11            PA06-XSEGLV PICTURE  X(02).                   CI0001
            11            PA06-XRC    PICTURE  X(02).                   CI0001
            11            PA06-XPROPT PICTURE  X(04).                   CI0001
            11            PA06-FILLER PICTURE  S9(5)                    CI0001
                          BINARY.                                       CI0001
            11            PA06-XSEGNM PICTURE  X(08).                   CI0001
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0001
                          BINARY.                                       CI0001
            11            PA06-XSEGNB PICTURE  9(05)                    CI0001
                          BINARY.                                       CI0001
            11            PA06-XCOKEY PICTURE  X(70).                   CI0001
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0001
          05              PB00-SUITE.                                   CI0001
            15       FILLER         PICTURE  X(00106).                  CI0001
       01                 PB06  REDEFINES      PB00.                    CI0001
            10            PB06-XDBPCB.                                  CI0001
            11            PB06-XDBDNM PICTURE  X(08).                   CI0001
            11            PB06-XSEGLV PICTURE  X(02).                   CI0001
            11            PB06-XRC    PICTURE  X(02).                   CI0001
            11            PB06-XPROPT PICTURE  X(04).                   CI0001
            11            PB06-FILLER PICTURE  S9(5)                    CI0001
                          BINARY.                                       CI0001
            11            PB06-XSEGNM PICTURE  X(08).                   CI0001
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0001
                          BINARY.                                       CI0001
            11            PB06-XSEGNB PICTURE  9(05)                    CI0001
                          BINARY.                                       CI0001
            11            PB06-XCOKEY PICTURE  X(70).                   CI0001
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0001
          05              PC00-SUITE.                                   CI0001
            15       FILLER         PICTURE  X(00106).                  CI0001
       01                 PC06  REDEFINES      PC00.                    CI0001
            10            PC06-XDBPCB.                                  CI0001
            11            PC06-XDBDNM PICTURE  X(08).                   CI0001
            11            PC06-XSEGLV PICTURE  X(02).                   CI0001
            11            PC06-XRC    PICTURE  X(02).                   CI0001
            11            PC06-XPROPT PICTURE  X(04).                   CI0001
            11            PC06-FILLER PICTURE  S9(5)                    CI0001
                          BINARY.                                       CI0001
            11            PC06-XSEGNM PICTURE  X(08).                   CI0001
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0001
                          BINARY.                                       CI0001
            11            PC06-XSEGNB PICTURE  9(05)                    CI0001
                          BINARY.                                       CI0001
            11            PC06-XCOKEY PICTURE  X(70).                   CI0001
      *                                                                 AMDU08
      ******************************************************************AMDU08
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU08
      **     SCHEDULED PAYOUT'S DESTINATION INFORMATION.               *AMDU08
      ******************************************************************AMDU08
      *                                                                 AMDU08
      *!WF DSP=DS DSL=DU SEL=08 FOR=I LEV=1                             AMDU08
       01                 DS00.                                         CI0001
          05              DS00-SUITE.                                   CI0001
            15       FILLER         PICTURE  X(01491).                  CI0001
       01                 DS08  REDEFINES      DS00.                    CI0001
            10            DS08-C299.                                    CI0001
            11            DS08-CTID.                                    CI0001
            12            DS08-CTIDA  PICTURE  9(3).                    CI0001
            12            DS08-CTIDN.                                   CI0001
            13            DS08-CTIDNP PICTURE  X(13).                   CI0001
            13            DS08-CTIDND PICTURE  9(11).                   CI0001
            10            DS08-C199.                                    CI0001
            11            DS08-CLID.                                    CI0001
            12            DS08-CLIDO  PICTURE  9(3).                    CI0001
            12            DS08-CLIDN.                                   CI0001
            13            DS08-CLIDNP PICTURE  X(12).                   CI0001
            13            DS08-CLIDND PICTURE  9(8).                    CI0001
            10            DS08-DCACG  PICTURE  9(8).                    CI0001
            10            DS08-ISHAD  PICTURE  X.                       CI0001
            10            DS08-MAPPN  PICTURE  X(10).                   CI0001
            10            DS08-FILLER PICTURE  X(90).                   CI0001
            10            DS08-DU01.                                    CI0001
            11            DS08-TWHOL1 PICTURE  X(100).                  CI0001
            11            DS08-TWHOL2 PICTURE  X(100).                  CI0001
            11            DS08-TWHOL3 PICTURE  X(100).                  CI0001
            11            DS08-TWHRL1 PICTURE  X(100).                  CI0001
            11            DS08-TWHRL2 PICTURE  X(100).                  CI0001
            11            DS08-TWHRL3 PICTURE  X(100).                  CI0001
            11            DS08-TWHRL4 PICTURE  X(100).                  CI0001
            11            DS08-TWHRL5 PICTURE  X(100).                  CI0001
            11            DS08-TWHRL6 PICTURE  X(100).                  CI0001
            11            DS08-TWHRL7 PICTURE  X(100).                  CI0001
            11            DS08-THOWL  PICTURE  X(100).                  CI0001
            11            DS08-CPAY1  PICTURE  X(2).                    CI0001
            11            DS08-FILLER PICTURE  X(100).                  CI0001
            10            DS08-CX21K.                                   CI0001
            11            DS08-CDEL1  PICTURE  9(3).                    CI0001
            11            DS08-NDELS  PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            DS08-CX18K.                                   CI0001
            11            DS08-NBASQ  PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            DS08-CLID4  PICTURE  X(23).                   CI0001
            10            DS08-CTTBO1 PICTURE  X(45).                   CI0001
            10            DS08-CTTBO2 PICTURE  X(45).                   CI0001
            10            DS08-FILLER PICTURE  X(10).                   CI0001
      *                                                                 AMDU08
      *                                                                 AMDU08
      *                                                                 AMDU08
      *                                                                 AMDU08
      *
      ******************************************************************
      **     CX14 SEGMENT CONTAINS THE DESTINATION DATA FROM THE       *
      **     ARRANGEMENT DATABASE.                                     *
      ******************************************************************
      *
      *!WF DSP=CX DSL=CX SEL=14 FOR=I LEV=1 PLT=10
       01                 CX00.                                         CI0001
          05              CX00-SUITE.                                   CI0001
            15       FILLER         PICTURE  X(00057).                  CI0001
       01                 CX14  REDEFINES      CX00.                    CI0001
            10            CX14-GELL   PICTURE  9(4)                     CI0001
                          BINARY.                                       CI0001
            10            CX14-CX14K.                                   CI0001
            11            CX14-NPISQ  PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            CX14-ACOTD  PICTURE  S9(9)V99                 CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            CX14-PPOTD  PICTURE  S9(3)V99                 CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            CX14-QPSTD  PICTURE  S9(7)V999                CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            CX14-CPITC  PICTURE  99.                      CI0001
            10            CX14-FILLER PICTURE  X(04).                   CI0001
            10            CX14-CY97.                                    CI0001
            11            CX14-FILLER PICTURE  X(32).                   CI0001
            10            CX14-CY30                                     CI0001
                          REDEFINES            CX14-CY97.               CI0001
            11            CX14-IOWNC  PICTURE  X.                       CI0001
            11            CX14-CTYPE  PICTURE  X.                       CI0001
            11            CX14-C299.                                    CI0001
            12            CX14-CTID.                                    CI0001
            13            CX14-CTIDA  PICTURE  9(3).                    CI0001
            13            CX14-CTIDN.                                   CI0001
            14            CX14-CTIDNP PICTURE  X(13).                   CI0001
            14            CX14-CTIDND PICTURE  9(11).                   CI0001
            11            CX14-CPMTC  PICTURE  99.                      CI0001
            11            CX14-IACSD  PICTURE  X.                       CI0001
            10            CX14-CY31                                     CI0001
                          REDEFINES            CX14-CY97.               CI0001
            11            CX14-FILLER PICTURE  X(2).                    CI0001
            11            CX14-IDELI  PICTURE  X.                       CI0001
            11            CX14-CDEL1  PICTURE  9(3).                    CI0001
            11            CX14-NDELS  PICTURE  S9(3)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            CX14-CY32                                     CI0001
                          REDEFINES            CX14-CY97.               CI0001
            11            CX14-GCUSPZ PICTURE  X(12).                   CI0001
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
       01                 DE00.                                         CI0001
          05              DE00-SUITE.                                   CI0001
            15       FILLER         PICTURE  X(00653).                  CI0001
       01                 DE10  REDEFINES      DE00.                    CI0001
            10            DE10-DU11.                                    CI0001
            11            DE10-XFONC  PICTURE  X(4).                    CI0001
            11            DE10-MPSBN  PICTURE  X(8).                    CI0001
            11            DE10-XDBDNM PICTURE  X(08).                   CI0001
            11            DE10-XSEGNM PICTURE  X(08).                   CI0001
            11            DE10-XRC    PICTURE  X(02).                   CI0001
            11            DE10-MSEG   PICTURE  X(08).                   CI0001
            11            DE10-XCOKEY PICTURE  X(70).                   CI0001
            11            DE10-CUIBR  PICTURE  X(01).                   CI0001
            11            DE10-CUIBA  PICTURE  X(01).                   CI0001
            11            DE10-IPBIK  PICTURE  X(1).                    CI0001
            10            DE10-DU03.                                    CI0001
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            11            DE10-CMSSF  PICTURE  XX.                      CI0001
            11            DE10-DU09.                                    CI0001
            12            DE10-CMESA  PICTURE  S9(9)                    CI0001
                          BINARY.                                       CI0001
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0001
                          BINARY.                                       CI0001
            12            DE10-CMESB  PICTURE  S9(9)                    CI0001
                          BINARY.                                       CI0001
            12            DE10-CMSST  PICTURE  S9(9)                    CI0001
                          BINARY.                                       CI0001
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0001
                          BINARY.                                       CI0001
            12            DE10-QELLAA PICTURE  S9(9)                    CI0001
                          BINARY.                                       CI0001
            12            DE10-TMESS4 PICTURE  X(512).                  CI0001
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
       01                 MS00.                                         CI0001
          05              MS00-SUITE.                                   CI0001
            15       FILLER         PICTURE  X(00542).                  CI0001
       01                 MS03  REDEFINES      MS00.                    CI0001
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0001
                          COMPUTATIONAL-3.                              CI0001
            10            MS03-CMSSF  PICTURE  XX.                      CI0001
            10            MS03-DU09.                                    CI0001
            11            MS03-CMESA  PICTURE  S9(9)                    CI0001
                          BINARY.                                       CI0001
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0001
                          BINARY.                                       CI0001
            11            MS03-CMESB  PICTURE  S9(9)                    CI0001
                          BINARY.                                       CI0001
            11            MS03-CMSST  PICTURE  S9(9)                    CI0001
                          BINARY.                                       CI0001
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0001
                          BINARY.                                       CI0001
            11            MS03-QELLAA PICTURE  S9(9)                    CI0001
                          BINARY.                                       CI0001
            11            MS03-TMESS4 PICTURE  X(512).                  CI0001
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                DS08
                                CX14
                                DE10
                                MS03.                                   ADU002
      *N01.      NOTE *************************************.            CI0001
      *               *                                   *             CI0001
      *               *INITIALISATIONS                    *             CI0001
      *               *                                   *             CI0001
      *               *************************************.            CI0001
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
      *N02CA.    NOTE *INITIALIZE SEGMENTS                *.
       F02CA.                                                           lv10
           INITIALIZE      CL01
           INITIALIZE      CL03
           INITIALIZE      CL12
           INITIALIZE      CL18
           INITIALIZE      CT01
           INITIALIZE      CX18
           INITIALIZE      CX21
           INITIALIZE      MS03.
       F02CA-FN. EXIT.
      *N02DA.    NOTE *** VERIFY THAT IT IS CATS SD  **   *.
       F02DA.                                                           lv10
      *********************************
      ** IF THE CALLING PROGRAM IS    *
      ** NOT A CAST SD APPLICATION,   *
      ** MOVE SPACES TO THE           *
      ** APPLICATION NAME - MAPPN     *
      *********************************
                 IF    DS08-MAPPN NOT =                                 DOT
                       'SD        '
      **** CALLING PROGRAM NOT = SD ***
           MOVE        SPACES TO DS08-MAPPN
           MOVE        SPACES TO DI02-MAPPN
                 ELSE
      ****  99 LEVEL ELSE STATEMENT ***
           MOVE        DS08-MAPPN TO DI02-MAPPN.
      ****  99 LEVEL END  STATEMENT ***                                 DOT
      ** INITIALIZE BENEFICIARY LINES *
           MOVE        SPACES TO DS08-CTTBO1
           MOVE        SPACES TO DS08-CTTBO1.
       F02DA-FN. EXIT.
      *N02XA.    NOTE *SET ADDRESSES FOR DATABASES        *.
       F02XA.                                                           lv10
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0001
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0001
      *               *                                   *             CI0001
      *               *FIN DE TRAITEMENT                  *             CI0001
      *               *                                   *             CI0001
      *               *************************************.            CI0001
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0001
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
      *               *TRANSFER TO ANOTHER IDS ACCOUNT    *
      *               *                                   *
      *               *************************************.
       F40.      IF    CX14-CPITC = 02                                  lv05
                 NEXT SENTENCE ELSE GO TO     F40-FN.
      *
      *********************************
      ** TRANSFER TO ANOTHER IDS      *
      ** ACCOUNT.                     *
      **  - GET PRODUCT NAME          *
      **  - CALL CI0003 TO GET ACCOUNT*
      **    OWNERSHIP LINES           *
      **  - CALL CI0007 TO FORMAT THE *
      **    DESTINATION WHO, WHERE AND*
      **    HOW LINES                 *
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
           MOVE        CX14-CTID TO S-CTU01-CTID.
       F40BA-FN. EXIT.
      *N40CA.    NOTE *READ CT01 SEGMENT                  *.
       F40CA.                                                           lv10
           PERFORM     F94CT THRU F94CT-FN.
       F40CA-FN. EXIT.
      *N40DA.    NOTE *CHECK IF ACCT NUMBER NOT FOUND     *.
       F40DA.    IF    IK = '1'                                         lv10
                 OR    DE10-NMESS2 NOT = ZEROS
                 NEXT SENTENCE ELSE GO TO     F40DA-FN.
      *********************************
      ** IF ACCOUNT NUMBER NOT FOUND  *
      ** ON THE CONTRACT DATABASE     *
      ** GET APPROPRIATE MESSAGE INFO *
      ** BY CALLING CI0002 AND RETURN *
      ** TO CALLING MODULE.           *
      *********************************
      *
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012011 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98IC THRU F98IC-FN.                             ADU019
      *N40DZ.    NOTE *RETURN TO CALLING MODULE           *.
       F40DZ.                                                           lv15
           MOVE                     ALL '1' TO FT GO TO F20.
       F40DZ-FN. EXIT.
       F40DA-FN. EXIT.
      *N40EA.    NOTE *RANDOM TABLE READ FOR TA5A         *.            ADUTAB
       F40EA.                                                           lv10
           MOVE        CT01-CTIDA TO TA5A-CTIDA
           MOVE        CT01-PRCOD TO TA5A-PRCOD.
                 IF    CT01-CTIDA = 002                                 DOT
           MOVE        CT01-PRSCD TO TA5A-PRSCD
                 ELSE
           MOVE        SPACES TO TA5A-PRSCD.
           MOVE        'R1' TO G-TA5A-TABFO                             DOT
           COMPUTE     G-TA5A-LTH = 60 + G-TA5A-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA5A-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA5A)                                ADUTAB
                       LENGTH (G-TA5A-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA5A-TABCR NOT = '00'                          DOT
           MOVE        'UNKNOWN' TO TA5A-PRCMN.
                 IF    CT01-CTIDA = 002                                 DOT
           MOVE        TA5A-PRCMN TO W-PRCMN
           MOVE        TA5A-MSP03 TO W-MSP03
           PERFORM     F91DD THRU F91DD-FN
           MOVE        W-PRCMN TO TA5A-PRCMN.
       F40EA-FN. EXIT.
      *N40FA.    NOTE *CALL CI0003 - ACCT OWNER/BENE      *.            AM0003
       F40FA.                                                           lv10
      *                                                                 AM0003
      *********************************                                 AM0003
      ** THIS MODULE WILL READ THE    *                                 AM0003
      ** CONTRACT DATABASE TO GET THE *                                 AM0003
      ** ACCOUNT OWNERSHIP AND        *                                 AM0003
      ** BENEFICIARY LINES FOR THE    *                                 AM0003
      ** REQUESTED ACCOUNT NUMBER.    *                                 AM0003
      *********************************                                 AM0003
      *                                                                 AM0003
           INITIALIZE      AB04                                         AM0003
           MOVE        CX14-CTID TO AB04-CTID                           AM0003
           MOVE        'N' TO AB04-IPOCH                                AM0003
           SET CI0003A-PCB-CT1P-PTR1 TO                                 AM0003
                       PCB-CT1P-PTR1                                    AM0003
           INITIALIZE      DE10-DU03                                    AM0003
           CALL        CI0003 USING                                     AM0003
           DFHEIBLK                                                     AM0003
           DFHCOMMAREA                                                  AM0003
           DLIUIBII                                                     AM0003
           CI0003A-PCB-ADDRESS-LIST                                     AM0003
           AB04                                                         AM0003
           DE10                                                         AM0003
           MS03.                                                        AM0003
       F40FA-FN. EXIT.
      *N40GA.    NOTE *CHECK IF THERE WAS NO ERROR        *.
       F40GA.    IF    MS03-NMESS2 = ZEROS                              lv10
                 AND   DE10-NMESS2 = ZEROS
                 NEXT SENTENCE ELSE GO TO     F40GA-FN.
      *********************************
      ** FORMAT DESTINATION LINES ONLY*
      ** IF THERE ARE NO ERRORS.      *
      *********************************
           INITIALIZE  MS03.
                 IF    DS08-MAPPN = 'SD        '                        DOT
      **** IS CALLING PROGRAM = SD  ***
           MOVE        AB04-CTTBO1 TO DS08-CTTBO1
           MOVE        AB04-CTTBO2 TO DS08-CTTBO2.
      **** END 99 LEVEL IF STATEMENT **                                 DOT
      *N40PA.    NOTE *CALL CI0007 - FORMAT DEST LINES    *.            AM0007
       F40PA.                                                           lv15
      *                                                                 AM0007
      *********************************                                 AM0007
      ** THIS MODULE WILL FORMAT THE  *                                 AM0007
      ** DESTINATION LINES TO BE SENT *                                 AM0007
      ** BACK TO THE WORKSTATION FOR  *                                 AM0007
      ** DISPLAY.  ALL THE            *                                 AM0007
      ** APPROPRIATE VALUES NEED TO BE*                                 AM0007
      ** FILLED IN SEGMENT DI02.      *                                 AM0007
      ** THE HOW, WHO AND WHERE LINES *                                 AM0007
      ** WILL BE SENT BACK IN SEGMENT *                                 AM0007
      ** DL01                         *                                 AM0007
      *********************************                                 AM0007
      *                                                                 AM0007
           MOVE        'TR' TO DI02-CPAY1                               AM0007
           MOVE        DS08-MAPPN TO DI02-MAPPN                         AM0007
           MOVE        AB04-CTTLN1 TO DI02-CTTLN1                       AM0007
           MOVE        AB04-CTTLN2 TO DI02-CTTLN2                       AM0007
           MOVE        AB04-CTTLN3 TO DI02-CTTLN3                       AM0007
           MOVE        AA07-CL24 TO DI02-CL24                           AM0007
           MOVE        CL01-CLTYP TO DI02-CLTYP                         AM0007
           MOVE        CL12-CLORN TO DI02-CLORN                         AM0007
           MOVE        CL03-C198 TO DI02-C198                           AM0007
           MOVE        CX21-TDELI TO DI02-TDELI                         AM0007
           MOVE        CX18-NPBN TO DI02-NPBN                           AM0007
           MOVE        CX18-CCBAT TO DI02-CCBAT                         AM0007
           MOVE        CL18-NTR TO DI02-NTR                             AM0007
           MOVE        CL18-GECKD TO DI02-GECKD1                        AM0007
           MOVE        CT01-C299 TO DI02-C299                           AM0007
           MOVE        CT01-GECKD TO DI02-GECKD                         AM0007
           MOVE        TA5A-PRCMN TO DI02-PRCMN                         AM0007
           MOVE        CX18-MCSIG TO DI02-MCSIG                         AM0007
           MOVE        DS08-ISHAD TO DI02-ISHAD                         AM0007
           INITIALIZE      DL01                                         AM0007
           CALL        CI0007 USING                                     AM0007
           DFHEIBLK                                                     AM0007
           DFHCOMMAREA                                                  AM0007
           DI02                                                         AM0007
           DL01                                                         AM0007
           MS03.                                                        AM0007
           MOVE        DL01 TO DS08-DU01.                               AM0007
       F40PA-FN. EXIT.
       F40GA-FN. EXIT.
      *N40ZA.    NOTE *RETURN TO CALLING MODULE           *.
       F40ZA.                                                           lv10
           MOVE                     ALL '1' TO FT GO TO F20.
       F40ZA-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *CHECK TO OWNER                     *
      *               *                                   *
      *               *************************************.
       F45.      IF    CX14-CPITC = 01                                  lv05
                 AND   CX14-IDELI = 'Y'
                 NEXT SENTENCE ELSE GO TO     F45-FN.
      *********************************
      ** CHECK TO OWNER:              *
      **  - CALL CI0003 TO GET ACCOUNT*
      **    OWNERSHIP LINES           *
      **  - CALL CI0004 TO GET ACCOUNT*
      **    ADDRESS                   *
      **  - CALL CI0007 TO FORMAT THE *
      **    DESTINATION WHO, WHERE AND*
      **    HOW LINES                 *
      *********************************
      *
      *N45FA.    NOTE *CALL CI0003 - ACCT OWNER/BENE      *.            AM0003
       F45FA.                                                           lv10
      *                                                                 AM0003
      *********************************                                 AM0003
      ** THIS MODULE WILL READ THE    *                                 AM0003
      ** CONTRACT DATABASE TO GET THE *                                 AM0003
      ** ACCOUNT OWNERSHIP AND        *                                 AM0003
      ** BENEFICIARY LINES FOR THE    *                                 AM0003
      ** REQUESTED ACCOUNT NUMBER.    *                                 AM0003
      *********************************                                 AM0003
      *                                                                 AM0003
           INITIALIZE      AB04                                         AM0003
           MOVE        DS08-CTID TO AB04-CTID                           AM0003
           MOVE        'Y' TO AB04-IPOCH                                AM0003
           SET CI0003B-PCB-CT1P-PTR1 TO                                 AM0003
                       PCB-CT1P-PTR1                                    AM0003
           INITIALIZE      DE10-DU03                                    AM0003
           CALL        CI0003 USING                                     AM0003
           DFHEIBLK                                                     AM0003
           DFHCOMMAREA                                                  AM0003
           DLIUIBII                                                     AM0003
           CI0003B-PCB-ADDRESS-LIST                                     AM0003
           AB04                                                         AM0003
           DE10                                                         AM0003
           MS03.                                                        AM0003
       F45FA-FN. EXIT.
      *N45GA.    NOTE *CHECK IF THERE WAS NO ERROR        *.
       F45GA.    IF    MS03-NMESS2 = ZEROS                              lv10
                 AND   DE10-NMESS2 = ZEROS
                 NEXT SENTENCE ELSE GO TO     F45GA-FN.
      *********************************
      ** GET ACCOUNT ADDRESS ONLY IF  *
      ** THERE ARE NO ERRORS.         *
      *********************************
           INITIALIZE  MS03.
      *N45HA.    NOTE *CALL CI0004 - ACCOUNT ADDRESS      *.            AM0004
       F45HA.                                                           lv15
      *                                                                 AM0004
      *********************************                                 AM0004
      ** THIS MODULE WILL READ THE    *                                 AM0004
      ** CONTRACT DATABASE TO THE     *                                 AM0004
      ** OWNER CLIENT THAT THE ADDRESS*                                 AM0004
      ** ASSOCIATED WITH IT.  IT WILL *                                 AM0004
      ** THEN CALL MODULE CI0006 TO   *                                 AM0004
      ** GET THE CLIENT'S ADDRESS     *                                 AM0004
      ** SEGMENT(CL24).               *                                 AM0004
      *********************************                                 AM0004
      *                                                                 AM0004
           INITIALIZE      AA07                                         AM0004
           MOVE        DS08-CTID TO AA07-CTID                           AM0004
           MOVE        DS08-DCACG TO AA07-DCACG                         AM0004
           SET CI0004D-PCB-CT1P-PTR1 TO                                 AM0004
                       PCB-CT1P-PTR1                                    AM0004
           SET CI0004D-PCB-CL1P-PTR1 TO                                 AM0004
                       PCB-CL1P-PTR1                                    AM0004
           INITIALIZE DE10-DU03                                         AM0004
           CALL        CI0004 USING                                     AM0004
           DFHEIBLK                                                     AM0004
           DFHCOMMAREA                                                  AM0004
           DLIUIBII                                                     AM0004
           CI0004D-PCB-ADDRESS-LIST                                     AM0004
           AA07                                                         AM0004
           DE10                                                         AM0004
           MS03.                                                        AM0004
       F45HA-FN. EXIT.
      *N45IA.    NOTE *CHECK IF THERE WAS NO ERROR        *.
       F45IA.    IF    MS03-NMESS2 = ZEROS                              lv15
                 AND   DE10-NMESS2 = ZEROS
                 NEXT SENTENCE ELSE GO TO     F45IA-FN.
      *********************************
      ** FORMAT DESTINATION LINES ONLY*
      ** IF THERE ARE NO ERRORS.      *
      *********************************
           INITIALIZE  MS03.
      *N45PA.    NOTE *CALL CI0007 - FORMAT DEST LINES    *.            AM0007
       F45PA.                                                           lv20
      *                                                                 AM0007
      *********************************                                 AM0007
      ** THIS MODULE WILL FORMAT THE  *                                 AM0007
      ** DESTINATION LINES TO BE SENT *                                 AM0007
      ** BACK TO THE WORKSTATION FOR  *                                 AM0007
      ** DISPLAY.  ALL THE            *                                 AM0007
      ** APPROPRIATE VALUES NEED TO BE*                                 AM0007
      ** FILLED IN SEGMENT DI02.      *                                 AM0007
      ** THE HOW, WHO AND WHERE LINES *                                 AM0007
      ** WILL BE SENT BACK IN SEGMENT *                                 AM0007
      ** DL01                         *                                 AM0007
      *********************************                                 AM0007
      *                                                                 AM0007
           MOVE        'O ' TO DI02-CPAY1                               AM0007
           MOVE        DS08-MAPPN TO DI02-MAPPN                         AM0007
           MOVE        AB04-CTTLN1 TO DI02-CTTLN1                       AM0007
           MOVE        AB04-CTTLN2 TO DI02-CTTLN2                       AM0007
           MOVE        AB04-CTTLN3 TO DI02-CTTLN3                       AM0007
           MOVE        AA07-CL24 TO DI02-CL24                           AM0007
           MOVE        CL01-CLTYP TO DI02-CLTYP                         AM0007
           MOVE        CL12-CLORN TO DI02-CLORN                         AM0007
           MOVE        CL03-C198 TO DI02-C198                           AM0007
           MOVE        CX21-TDELI TO DI02-TDELI                         AM0007
           MOVE        CX18-NPBN TO DI02-NPBN                           AM0007
           MOVE        CX18-CCBAT TO DI02-CCBAT                         AM0007
           MOVE        CL18-NTR TO DI02-NTR                             AM0007
           MOVE        CL18-GECKD TO DI02-GECKD1                        AM0007
           MOVE        CT01-C299 TO DI02-C299                           AM0007
           MOVE        CT01-GECKD TO DI02-GECKD                         AM0007
           MOVE        TA5A-PRCMN TO DI02-PRCMN                         AM0007
           MOVE        CX18-MCSIG TO DI02-MCSIG                         AM0007
           MOVE        DS08-ISHAD TO DI02-ISHAD                         AM0007
           INITIALIZE      DL01                                         AM0007
           CALL        CI0007 USING                                     AM0007
           DFHEIBLK                                                     AM0007
           DFHCOMMAREA                                                  AM0007
           DI02                                                         AM0007
           DL01                                                         AM0007
           MS03.                                                        AM0007
           MOVE        DL01 TO DS08-DU01.                               AM0007
       F45PA-FN. EXIT.
       F45IA-FN. EXIT.
       F45GA-FN. EXIT.
      *N45ZA.    NOTE *RETURN TO CALLING MODULE           *.
       F45ZA.                                                           lv10
           MOVE                     ALL '1' TO FT GO TO F20.
       F45ZA-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *READ CX21 SEGMENT                  *
      *               *                                   *
      *               *************************************.
       F50.      IF    CX14-CPITC = 01                                  lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *
      *********************************
      ** THE DESTINATION IS EITHER A: *
      **  - CHECK TO OWNER AT         *
      **    ALTERNATE ADDRESS         *
      **  - SPECIAL PAYEE             *
      **  - CHECK TO BANK             *
      **  - DIRECT DEPOSIT            *
      ** SO THE CX21 SEGMENT NEEDS TO *
      ** BE READ TO DESTINATION INFO  *
      *********************************
      *
      *N50BA.    NOTE *SET UP SSAS FOR CX21 READ          *.
       F50BA.                                                           lv10
      *
      *********************************
      ** SET UP SSAS FOR CX01 AND CX21*
      ** USING CLIENT ID NUMBER PASSED*
      ** AND CX21 KEY FROM CX14       *
      ** SEGMENT                      *
      *********************************
      *
           MOVE        DS08-CLID TO S-CXU01-CLID
           MOVE        CX14-CDEL1 TO S-CXU21-CDEL1
           MOVE        CX14-NDELS TO S-CXU21-NDELS.
       F50BA-FN. EXIT.
      *N50CA.    NOTE *READ CX21 SEGMENT                  *.
       F50CA.                                                           lv10
           PERFORM     F94CS THRU F94CS-FN.
       F50CA-FN. EXIT.
      *N50DA.    NOTE *CHECK IF CX21 SEGMENT NOT FOUND    *.
       F50DA.    IF    IK = '1'                                         lv10
                 OR    DE10-NMESS2 NOT = ZEROS
                 NEXT SENTENCE ELSE GO TO     F50DA-FN.
      *********************************
      ** IF CX21 SEGMENT NOT FOUND ON *
      ** THE ARRANGEMENT DATABASE     *
      ** GET APPROPRIATE MESSAGE INFO *
      ** BY CALLING CI0002 AND RETURN *
      ** TO CALLING MODULE.           *
      *********************************
      *
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012029 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98IC THRU F98IC-FN.                             ADU019
      *N50DZ.    NOTE *RETURN TO CALLING MODULE           *.
       F50DZ.                                                           lv15
           MOVE                     ALL '1' TO FT GO TO F20.
       F50DZ-FN. EXIT.
       F50DA-FN. EXIT.
      *N50EA.    NOTE *CX21 SEGMENT FOUND                 *.
       F50EA.                                                           lv10
      *
      *********************************
      ** PASS BACK THE CX21 KEY DATA  *
      ** VIA DS08                     *
      *********************************
      *
           MOVE        CX21-CX21K TO DS08-CX21K
           MOVE        CX21-CLID TO DS08-CLID4.
       F50EA-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *CHECK TO OWNER AT ALTERNATE ADDR   *
      *               *                                   *
      *               *************************************.
       F55.      IF    CX14-CPITC = 01                                  lv05
                 AND   CX14-IDELI = 'N'
                 AND   CX21-CDEL1 = 002
                 AND   CX21-CPAYE = 02
                 NEXT SENTENCE ELSE GO TO     F55-FN.
      *********************************
      ** CHECK TO OWNER AT ALTERNATE  *
      ** ADDRESS                      *
      **  - CALL CI0003 TO GET ACCOUNT*
      **    OWNERSHIP LINES           *
      **  - CALL CI0006 TO GET CLIENT *
      **    ADDRESS                   *
      **  - CALL CI0007 TO FORMAT THE *
      **    DESTINATION WHO, WHERE AND*
      **    HOW LINES                 *
      *********************************
      *
      *N55FA.    NOTE *CALL CI0003 - ACCT OWNER/BENE      *.            AM0003
       F55FA.                                                           lv10
      *                                                                 AM0003
      *********************************                                 AM0003
      ** THIS MODULE WILL READ THE    *                                 AM0003
      ** CONTRACT DATABASE TO GET THE *                                 AM0003
      ** ACCOUNT OWNERSHIP AND        *                                 AM0003
      ** BENEFICIARY LINES FOR THE    *                                 AM0003
      ** REQUESTED ACCOUNT NUMBER.    *                                 AM0003
      *********************************                                 AM0003
      *                                                                 AM0003
           INITIALIZE      AB04                                         AM0003
           MOVE        DS08-CTID TO AB04-CTID                           AM0003
           MOVE        'Y' TO AB04-IPOCH                                AM0003
           SET CI0003C-PCB-CT1P-PTR1 TO                                 AM0003
                       PCB-CT1P-PTR1                                    AM0003
           INITIALIZE      DE10-DU03                                    AM0003
           CALL        CI0003 USING                                     AM0003
           DFHEIBLK                                                     AM0003
           DFHCOMMAREA                                                  AM0003
           DLIUIBII                                                     AM0003
           CI0003C-PCB-ADDRESS-LIST                                     AM0003
           AB04                                                         AM0003
           DE10                                                         AM0003
           MS03.                                                        AM0003
       F55FA-FN. EXIT.
      *N55GA.    NOTE *CHECK IF THERE WAS NO ERROR        *.
       F55GA.    IF    MS03-NMESS2 = ZEROS                              lv10
                 AND   DE10-NMESS2 = ZEROS
                 NEXT SENTENCE ELSE GO TO     F55GA-FN.
      *********************************
      ** GET CLIENT ADDRESS ONLY IF   *
      ** THERE ARE NO ERRORS.         *
      *********************************
           INITIALIZE  MS03.
      *N55HA.    NOTE *CALL CI0006 - CLIENT ADDRESS       *.            AM0006
       F55HA.                                                           lv15
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
           MOVE        CX21-CLID TO CA05-CLID                           AM0006
           MOVE        CX21-GECSQ1 TO CA05-GECSQ1                       AM0006
           MOVE        DS08-DCACG TO CA05-DCACG                         AM0006
           SET CI0006E-PCB-CL1P-PTR1 TO                                 AM0006
                       PCB-CL1P-PTR1                                    AM0006
           INITIALIZE DE10-DU03                                         AM0006
           CALL        CI0006 USING                                     AM0006
           DFHEIBLK                                                     AM0006
           DFHCOMMAREA                                                  AM0006
           DLIUIBII                                                     AM0006
           CI0006E-PCB-ADDRESS-LIST                                     AM0006
           CA05                                                         AM0006
           DE10                                                         AM0006
           MS03.                                                        AM0006
       F55HA-FN. EXIT.
      *N55IA.    NOTE *CHECK IF THERE WAS NO ERROR        *.
       F55IA.    IF    MS03-NMESS2 = ZEROS                              lv15
                 AND   DE10-NMESS2 = ZEROS
                 NEXT SENTENCE ELSE GO TO     F55IA-FN.
      *********************************
      ** FORMAT DESTINATION LINES ONLY*
      ** IF THERE ARE NO ERRORS.      *
      *********************************
           INITIALIZE  MS03.
      *N55PA.    NOTE *CALL CI0007 - FORMAT DEST LINES    *.            AM0007
       F55PA.                                                           lv20
      *                                                                 AM0007
      *********************************                                 AM0007
      ** THIS MODULE WILL FORMAT THE  *                                 AM0007
      ** DESTINATION LINES TO BE SENT *                                 AM0007
      ** BACK TO THE WORKSTATION FOR  *                                 AM0007
      ** DISPLAY.  ALL THE            *                                 AM0007
      ** APPROPRIATE VALUES NEED TO BE*                                 AM0007
      ** FILLED IN SEGMENT DI02.      *                                 AM0007
      ** THE HOW, WHO AND WHERE LINES *                                 AM0007
      ** WILL BE SENT BACK IN SEGMENT *                                 AM0007
      ** DL01                         *                                 AM0007
      *********************************                                 AM0007
      *                                                                 AM0007
           MOVE        'OA' TO DI02-CPAY1                               AM0007
           MOVE        DS08-MAPPN TO DI02-MAPPN                         AM0007
           MOVE        AB04-CTTLN1 TO DI02-CTTLN1                       AM0007
           MOVE        AB04-CTTLN2 TO DI02-CTTLN2                       AM0007
           MOVE        AB04-CTTLN3 TO DI02-CTTLN3                       AM0007
           MOVE        CA05-CL24 TO DI02-CL24                           AM0007
           MOVE        CL01-CLTYP TO DI02-CLTYP                         AM0007
           MOVE        CL12-CLORN TO DI02-CLORN                         AM0007
           MOVE        CL03-C198 TO DI02-C198                           AM0007
           MOVE        CX21-TDELI TO DI02-TDELI                         AM0007
           MOVE        CX18-NPBN TO DI02-NPBN                           AM0007
           MOVE        CX18-CCBAT TO DI02-CCBAT                         AM0007
           MOVE        CL18-NTR TO DI02-NTR                             AM0007
           MOVE        CL18-GECKD TO DI02-GECKD1                        AM0007
           MOVE        CT01-C299 TO DI02-C299                           AM0007
           MOVE        CT01-GECKD TO DI02-GECKD                         AM0007
           MOVE        TA5A-PRCMN TO DI02-PRCMN                         AM0007
           MOVE        CX18-MCSIG TO DI02-MCSIG                         AM0007
           MOVE        DS08-ISHAD TO DI02-ISHAD                         AM0007
           INITIALIZE      DL01                                         AM0007
           CALL        CI0007 USING                                     AM0007
           DFHEIBLK                                                     AM0007
           DFHCOMMAREA                                                  AM0007
           DI02                                                         AM0007
           DL01                                                         AM0007
           MS03.                                                        AM0007
           MOVE        DL01 TO DS08-DU01.                               AM0007
       F55PA-FN. EXIT.
       F55IA-FN. EXIT.
       F55GA-FN. EXIT.
      *N55ZA.    NOTE *RETURN TO CALLING MODULE           *.
       F55ZA.                                                           lv10
           MOVE                     ALL '1' TO FT GO TO F20.
       F55ZA-FN. EXIT.
       F55-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *CHECK TO SPECIAL PAYEE             *
      *               *                                   *
      *               *************************************.
       F60.      IF    CX14-CPITC = 01                                  lv05
                 AND   CX14-IDELI = 'N'
                 AND   CX21-CDEL1 = 002
                 AND   CX21-CPAYE = 03
                 NEXT SENTENCE ELSE GO TO     F60-FN.
      *********************************
      ** CHECK TO SPECIAL PAYEE       *
      **  - GET CLIENT'S NAME         *
      **  - CALL CI0006 TO GET CLIENT *
      **    ADDRESS                   *
      **  - CALL CI0007 TO FORMAT THE *
      **    DESTINATION WHO, WHERE AND*
      **    HOW LINES                 *
      *********************************
      *
      *N60BA.    NOTE *SET UP SSA FOR CL01 READ           *.
       F60BA.                                                           lv10
      *
      *********************************
      ** SET UP SSA FOR CL01 READ     *
      ** USING CLIENT ID NUMBER       *
      ** FROM CX21 SEGMENT            *
      *********************************
      *
           MOVE        CX21-CLID TO S-CLU01-CLID.
       F60BA-FN. EXIT.
      *N60CA.    NOTE *READ CL01 SEGMENT                  *.
       F60CA.                                                           lv10
           PERFORM     F94CL THRU F94CL-FN.
       F60CA-FN. EXIT.
      *N60DA.    NOTE *CHECK IF CLIENT NUMBER NOT FOUND   *.
       F60DA.    IF    IK = '1'                                         lv10
                 OR    DE10-NMESS2 NOT = ZEROS
                 NEXT SENTENCE ELSE GO TO     F60DA-FN.
      *********************************
      ** IF CLIENT NUMBER NOT FOUND   *
      ** ON THE CLIENT DATABASE       *
      ** GET APPROPRIATE MESSAGE INFO *
      ** BY CALLING CI0002 AND RETURN *
      ** TO CALLING MODULE.           *
      *********************************
      *
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012012 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98IC THRU F98IC-FN.                             ADU019
      *N60DZ.    NOTE *RETURN TO CALLING MODULE           *.
       F60DZ.                                                           lv15
           MOVE                     ALL '1' TO FT GO TO F20.
       F60DZ-FN. EXIT.
       F60DA-FN. EXIT.
      *N60EA.    NOTE *CHECK IF CLIENT IS A PERSON        *.
       F60EA.    IF    CL01-CLTYP = 'P'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F60EA-FN.
      *
      *********************************
      ** READ CL03 SEGMENT FOR PERSON *
      ** NAME                         *
      *********************************
      *
      *N60EF.    NOTE *READ CL03 SEGMENT                  *.
       F60EF.                                                           lv15
      *
      *********************************
      ** READ CL03 SEGMENT FOR PERSON *
      ** NAME                         *
      *********************************
      *
           PERFORM     F94CP THRU F94CP-FN.
       F60EF-FN. EXIT.
      *N60EM.    NOTE *CHECK IF CL03 SEGMENT NOT FOUND    *.
       F60EM.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F60EM-FN.
      *
      *********************************
      ** IF CL03 SEGMENT NOT FOUND,   *
      ** MOVE 'UNKNOWN' TO NAME       *
      *********************************
      *
           MOVE        'UNKNOWN' TO CL03-C198.
       F60EM-FN. EXIT.
       F60EA-900. GO TO F60FA-FN.
       F60EA-FN. EXIT.
      *N60FA.    NOTE *CLIENT IS AN ORGANIZATION          *.
       F60FA.                                                           lv10
      *
      *********************************
      ** READ CL12 SEGMENT FOR        *
      ** ORGANIZATION NAME            *
      *********************************
      *
      *N60FF.    NOTE *READ CL12 SEGMENT                  *.
       F60FF.                                                           lv15
      *
      *********************************
      ** READ CL12 SEGMENT FOR PERSON *
      ** NAME                         *
      *********************************
      *
           PERFORM     F94CO THRU F94CO-FN.
       F60FF-FN. EXIT.
      *N60FM.    NOTE *CHECK IF CL12 SEGMENT NOT FOUND    *.
       F60FM.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F60FM-FN.
      *
      *********************************
      ** IF CL12 SEGMENT NOT FOUND,   *
      ** MOVE 'UNKNOWN' TO NAME       *
      *********************************
      *
           MOVE        'UNKNOWN' TO CL12-CLORN.
       F60FM-FN. EXIT.
       F60FA-FN. EXIT.
      *N60HA.    NOTE *CALL CI0006 - CLIENT ADDRESS       *.            AM0006
       F60HA.                                                           lv10
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
           MOVE        CX21-CLID TO CA05-CLID                           AM0006
           MOVE        CX21-GECSQ1 TO CA05-GECSQ1                       AM0006
           MOVE        DS08-DCACG TO CA05-DCACG                         AM0006
           SET CI0006F-PCB-CL1P-PTR1 TO                                 AM0006
                       PCB-CL1P-PTR1                                    AM0006
           INITIALIZE DE10-DU03                                         AM0006
           CALL        CI0006 USING                                     AM0006
           DFHEIBLK                                                     AM0006
           DFHCOMMAREA                                                  AM0006
           DLIUIBII                                                     AM0006
           CI0006F-PCB-ADDRESS-LIST                                     AM0006
           CA05                                                         AM0006
           DE10                                                         AM0006
           MS03.                                                        AM0006
       F60HA-FN. EXIT.
      *N60IA.    NOTE *CHECK IF THERE WAS NO ERROR        *.
       F60IA.    IF    MS03-NMESS2 = ZEROS                              lv10
                 AND   DE10-NMESS2 = ZEROS
                 NEXT SENTENCE ELSE GO TO     F60IA-FN.
      *********************************
      ** FORMAT DESTINATION LINES ONLY*
      ** IF THERE ARE NO ERRORS.      *
      *********************************
           INITIALIZE  MS03.
      *N60PA.    NOTE *CALL CI0007 - FORMAT DEST LINES    *.            AM0007
       F60PA.                                                           lv15
      *                                                                 AM0007
      *********************************                                 AM0007
      ** THIS MODULE WILL FORMAT THE  *                                 AM0007
      ** DESTINATION LINES TO BE SENT *                                 AM0007
      ** BACK TO THE WORKSTATION FOR  *                                 AM0007
      ** DISPLAY.  ALL THE            *                                 AM0007
      ** APPROPRIATE VALUES NEED TO BE*                                 AM0007
      ** FILLED IN SEGMENT DI02.      *                                 AM0007
      ** THE HOW, WHO AND WHERE LINES *                                 AM0007
      ** WILL BE SENT BACK IN SEGMENT *                                 AM0007
      ** DL01                         *                                 AM0007
      *********************************                                 AM0007
      *                                                                 AM0007
           MOVE        'S ' TO DI02-CPAY1                               AM0007
           MOVE        DS08-MAPPN TO DI02-MAPPN                         AM0007
           MOVE        AB04-CTTLN1 TO DI02-CTTLN1                       AM0007
           MOVE        AB04-CTTLN2 TO DI02-CTTLN2                       AM0007
           MOVE        AB04-CTTLN3 TO DI02-CTTLN3                       AM0007
           MOVE        CA05-CL24 TO DI02-CL24                           AM0007
           MOVE        CL01-CLTYP TO DI02-CLTYP                         AM0007
           MOVE        CL12-CLORN TO DI02-CLORN                         AM0007
           MOVE        CL03-C198 TO DI02-C198                           AM0007
           MOVE        CX21-TDELI TO DI02-TDELI                         AM0007
           MOVE        CX18-NPBN TO DI02-NPBN                           AM0007
           MOVE        CX18-CCBAT TO DI02-CCBAT                         AM0007
           MOVE        CL18-NTR TO DI02-NTR                             AM0007
           MOVE        CL18-GECKD TO DI02-GECKD1                        AM0007
           MOVE        CT01-C299 TO DI02-C299                           AM0007
           MOVE        CT01-GECKD TO DI02-GECKD                         AM0007
           MOVE        TA5A-PRCMN TO DI02-PRCMN                         AM0007
           MOVE        CX18-MCSIG TO DI02-MCSIG                         AM0007
           MOVE        DS08-ISHAD TO DI02-ISHAD                         AM0007
           INITIALIZE      DL01                                         AM0007
           CALL        CI0007 USING                                     AM0007
           DFHEIBLK                                                     AM0007
           DFHCOMMAREA                                                  AM0007
           DI02                                                         AM0007
           DL01                                                         AM0007
           MS03.                                                        AM0007
           MOVE        DL01 TO DS08-DU01.                               AM0007
       F60PA-FN. EXIT.
       F60IA-FN. EXIT.
      *N60ZA.    NOTE *RETURN TO CALLING MODULE           *.
       F60ZA.                                                           lv10
           MOVE                     ALL '1' TO FT GO TO F20.
       F60ZA-FN. EXIT.
       F60-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *CHECK TO BANK                      *
      *               *                                   *
      *               *************************************.
       F65.      IF    CX14-CPITC = 01                                  lv05
                 AND   CX14-IDELI = 'N'
                 AND   CX21-CDEL1 = 002
                 AND   CX21-CPAYE = 01
                 NEXT SENTENCE ELSE GO TO     F65-FN.
      *********************************
      ** CHECK TO BANK                *
      **  - READ CX18 SEGMENT TO GET  *
      **    BANK CLIENT ID NUMBER     *
      **  - GET CLIENT'S NAME         *
      **  - CALL CI0006 TO GET CLIENT *
      **    ADDRESS                   *
      **  - CALL CI0007 TO FORMAT THE *
      **    DESTINATION WHO, WHERE AND*
      **    HOW LINES                 *
      *********************************
      *
      *N65BA.    NOTE *SET UP SSA FOR CX18 READ           *.
       F65BA.                                                           lv10
      *
      *********************************
      ** SET UP SSA FOR CX18 READ     *
      ** USING SEQUENCE NUMBER        *
      ** FROM CX21 SEGMENT  AND       *
      ** CLIENT ID NUMBER PASSED      *
      *********************************
      *
           MOVE        DS08-CLID TO S-CXU01-CLID
           MOVE        CX21-NBASQT TO S-CXU18-NBASQ.
       F65BA-FN. EXIT.
      *N65CA.    NOTE *READ CX18 SEGMENT                  *.
       F65CA.                                                           lv10
           PERFORM     F94CB THRU F94CB-FN.
       F65CA-FN. EXIT.
      *N65DA.    NOTE *CHECK IF CX18 SEGMENT NOT FOUND    *.
       F65DA.    IF    IK = '1'                                         lv10
                 OR    DE10-NMESS2 NOT = ZEROS
                 NEXT SENTENCE ELSE GO TO     F65DA-FN.
      *N65EA.    NOTE ** IF CALLING APPLICATION IS SD *   *.
       F65EA.    IF    DS08-MAPPN = 'SD        '                        lv15
                 NEXT SENTENCE ELSE GO TO     F65EA-FN.
           MOVE        'UNKNOWN BANK' TO BR30-CLORN
           MOVE        ZERO TO BR30-NTR
           MOVE        ZERO TO BR30-GECKD
           MOVE        'N' TO BR30-IRTNA
           MOVE        SPACES TO CA05-CL24
           MOVE        'O' TO CL01-CLTYP
           MOVE        SPACES TO CL03-C198
           MOVE        SPACES TO CX18-NPBN
           MOVE        ZEROES TO CX18-CCBAT
           MOVE        SPACES TO CX18-MCSIG
           PERFORM     F92BA THRU F92BA-FN
           MOVE        ZEROES TO DE10-NMESS2
           MOVE        ZEROES TO MS03-NMESS2.
       F65EA-900. GO TO F65EG-FN.
       F65EA-FN. EXIT.
      *N65EG.    NOTE *** CALLING APPLICATION NOT SD **   *.
       F65EG.                                                           lv15
      *
      *********************************
      ** IF CX18 SEGMENT NOT FOUND    *
      ** ON THE ARRANGEMENT DATABASE, *
      ** GET APPROPRIATE MESSAGE INFO *
      ** BY CALLING CI0002 AND RETURN *
      ** TO CALLING MODULE.           *
      *********************************
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012027 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98IC THRU F98IC-FN.                             ADU019
       F65EG-FN. EXIT.
      *N65EZ.    NOTE *RETURN TO CALLING MODULE           *.
       F65EZ.                                                           lv15
           MOVE                     ALL '1' TO FT GO TO F20.
       F65EZ-FN. EXIT.
       F65DA-FN. EXIT.
      *N65FA.    NOTE *CX18 SEGMENT FOUND                 *.
       F65FA.                                                           lv10
      *
      *********************************
      ** CX18 SEGMENT FOUND, PASS BACK*
      ** CX18 KEY DATA                *
      *********************************
      *
           MOVE        CX18-CX18K TO DS08-CX18K
           MOVE        CX18-CLID TO DS08-CLID4.
       F65FA-FN. EXIT.
      *N65GA.    NOTE *CALL CI0025 - BANK'S NAME/RTN      *.            AM0025
       F65GA.                                                           lv10
      *                                                                 AM0025
      *********************************                                 AM0025
      ** THIS MODULE WILL READ THE    *                                 AM0025
      ** CLIENT DATABASE TO GET THE   *                                 AM0025
      ** BANK'S NAME AND RTN USING THE*                                 AM0025
      ** BANK CLIENT ID NUMBER PASSED.                                  AM0025
      *********************************                                 AM0025
      *                                                                 AM0025
           INITIALIZE      BR30                                         AM0025
           MOVE        CX18-CLID TO BR30-CLID                           AM0025
           MOVE        002 TO BR30-CDEL1                                AM0025
           MOVE        DS08-DCACG TO BR30-DCACG                         AM0025
           MOVE        ZEROS TO BR30-NRTSQ1                             AM0025
           SET CI0025I-PCB-CL1P-PTR1 TO                                 AM0025
                       PCB-CL1P-PTR1                                    AM0025
           INITIALIZE      DE10-DU03                                    AM0025
           CALL        CI0025 USING                                     AM0025
           DFHEIBLK                                                     AM0025
           DFHCOMMAREA                                                  AM0025
           DLIUIBII                                                     AM0025
           CI0025I-PCB-ADDRESS-LIST                                     AM0025
           BR30                                                         AM0025
           DE10                                                         AM0025
           MS03.                                                        AM0025
       F65GA-FN. EXIT.
      *N65HA.    NOTE *CHECK IF THERE WAS NO ERROR        *.
       F65HA.    IF    MS03-NMESS2 = ZEROS                              lv10
                 AND   DE10-NMESS2 = ZEROS
                 NEXT SENTENCE ELSE GO TO     F65HA-FN.
      *********************************
      ** FORMAT DESTINATION LINES ONLY*
      ** IF THERE ARE NO ERRORS.      *
      *********************************
           INITIALIZE  MS03.
      *N65IA.    NOTE *CALL CI0006 - CLIENT ADDRESS       *.            AM0006
       F65IA.                                                           lv15
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
           MOVE        CX18-CLID TO CA05-CLID                           AM0006
           MOVE        CX21-GECSQ1 TO CA05-GECSQ1                       AM0006
           MOVE        DS08-DCACG TO CA05-DCACG                         AM0006
           SET CI0006G-PCB-CL1P-PTR1 TO                                 AM0006
                       PCB-CL1P-PTR1                                    AM0006
           INITIALIZE DE10-DU03                                         AM0006
           CALL        CI0006 USING                                     AM0006
           DFHEIBLK                                                     AM0006
           DFHCOMMAREA                                                  AM0006
           DLIUIBII                                                     AM0006
           CI0006G-PCB-ADDRESS-LIST                                     AM0006
           CA05                                                         AM0006
           DE10                                                         AM0006
           MS03.                                                        AM0006
       F65IA-FN. EXIT.
      *N65JA.    NOTE *CHECK IF THERE WAS NO ERROR        *.
       F65JA.    IF    MS03-NMESS2 = ZEROS                              lv15
                 AND   DE10-NMESS2 = ZEROS
                 NEXT SENTENCE ELSE GO TO     F65JA-FN.
      *********************************
      ** FORMAT DESTINATION LINES ONLY*
      ** IF THERE ARE NO ERRORS.      *
      *********************************
           INITIALIZE  MS03
           PERFORM     F92BA THRU F92BA-FN.
       F65JA-FN. EXIT.
       F65HA-900. GO TO F65LA-FN.
       F65HA-FN. EXIT.
      *N65LA.    NOTE **** CI0025 RETURN W/ERROR MSG **   *.
       F65LA.         EXIT.                                             lv10
      *N65MA.    NOTE ** IF CALLING APPLICATION = SD **   *.
       F65MA.    IF    DS08-MAPPN = 'SD        '                        lv15
                 NEXT SENTENCE ELSE GO TO     F65MA-FN.
      *N65NA.    NOTE *** IF PROGRAM ERROR IN CI0025 **   *.
       F65NA.    IF    MS03-NMESS2 NOT = ZEROS                          lv20
                 AND   MS03-CMESB < 11
                 NEXT SENTENCE ELSE GO TO     F65NA-FN.
      *********************************
      ** CI0025 RETURNED AN ERROR     *
      ** THAT IS NOT SEVERE           *
      *********************************
           INITIALIZE  MS03
           MOVE        'UNKNOWN BANK' TO BR30-CLORN
           MOVE        ZERO TO BR30-NTR
           MOVE        ZERO TO BR30-GECKD
           MOVE        'N' TO BR30-IRTNA
           MOVE        SPACES TO CA05-CL24
           MOVE        'O' TO CL01-CLTYP
           MOVE        SPACES TO CL03-C198
           PERFORM     F92BA THRU F92BA-FN.
       F65NA-FN. EXIT.
       F65MA-FN. EXIT.
       F65LA-FN. EXIT.
      *N65ZA.    NOTE *RETURN TO CALLING MODULE           *.
       F65ZA.                                                           lv10
           MOVE                     ALL '1' TO FT GO TO F20.
       F65ZA-FN. EXIT.
       F65-FN.   EXIT.
      *N70.      NOTE *************************************.
      *               *                                   *
      *               *DIRECT DEPOSIT                     *
      *               *                                   *
      *               *************************************.
       F70.      IF    CX14-CPITC = 01                                  lv05
                 AND   CX14-IDELI = 'N'
                 AND   CX21-CDEL1 = 003
                 NEXT SENTENCE ELSE GO TO     F70-FN.
      *********************************
      ** DIRECT DEPOSIT               *
      **  - READ CX18 SEGMENT TO GET  *
      **    BANK CLIENT ID NUMBER     *
      **  - GET CLIENT'S NAME         *
      **  - CALL CI0006 TO GET CLIENT *
      **    ADDRESS                   *
      **  - CALL CI0007 TO FORMAT THE *
      **    DESTINATION WHO, WHERE AND*
      **    HOW LINES                 *
      *********************************
      *
      *N70BA.    NOTE *SET UP SSA FOR CX18 READ           *.
       F70BA.                                                           lv10
      *
      *********************************
      ** SET UP SSA FOR CX18 READ     *
      ** USING SEQUENCE NUMBER        *
      ** FROM CX21 SEGMENT  AND       *
      ** CLIENT ID NUMBER PASSED      *
      *********************************
      *
           MOVE        DS08-CLID TO S-CXU01-CLID
           MOVE        CX21-NBASQ TO S-CXU18-NBASQ.
       F70BA-FN. EXIT.
      *N70CA.    NOTE *READ CX18 SEGMENT                  *.
       F70CA.                                                           lv10
           PERFORM     F94CB THRU F94CB-FN.
       F70CA-FN. EXIT.
      *N70DA.    NOTE *CHECK IF CX18 SEGMENT NOT FOUND    *.
       F70DA.    IF    IK = '1'                                         lv10
                 OR    DE10-NMESS2 NOT = ZEROS
                 NEXT SENTENCE ELSE GO TO     F70DA-FN.
      *N70EA.    NOTE ** IF CALLING APPLICATION IS SD *   *.
       F70EA.    IF    DS08-MAPPN = 'SD        '                        lv15
                 NEXT SENTENCE ELSE GO TO     F70EA-FN.
           MOVE        'UNKNOWN BANK' TO BR30-CLORN
           MOVE        ZERO TO BR30-NTR
           MOVE        ZERO TO BR30-GECKD
           MOVE        'N' TO BR30-IRTNA
           MOVE        SPACES TO CA05-CL24
           MOVE        'O' TO CL01-CLTYP
           MOVE        SPACES TO CL03-C198
           MOVE        SPACES TO CX18-NPBN
           MOVE        ZEROES TO CX18-CCBAT
           MOVE        SPACES TO CX18-MCSIG
           PERFORM     F92CA THRU F92CA-FN
           MOVE        ZEROES TO DE10-NMESS2
           MOVE        ZEROES TO MS03-NMESS2.
       F70EA-900. GO TO F70EG-FN.
       F70EA-FN. EXIT.
      *N70EG.    NOTE *** CALLING APPLICATION NOT SD **   *.
       F70EG.                                                           lv15
      *
      *********************************
      ** IF CX18 SEGMENT NOT FOUND    *
      ** ON THE ARRANGEMENT DATABASE, *
      ** GET APPROPRIATE MESSAGE INFO *
      ** BY CALLING CI0002 AND RETURN *
      ** TO CALLING MODULE.           *
      *********************************
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012027 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98IC THRU F98IC-FN.                             ADU019
       F70EG-FN. EXIT.
      *N70EZ.    NOTE *RETURN TO CALLING MODULE           *.
       F70EZ.                                                           lv15
           MOVE                     ALL '1' TO FT GO TO F20.
       F70EZ-FN. EXIT.
       F70DA-FN. EXIT.
      *N70FA.    NOTE *CX18 SEGMENT FOUND                 *.
       F70FA.                                                           lv10
      *
      *********************************
      ** CX18 SEGMENT FOUND, PASS BACK*
      ** CX18 KEY DATA                *
      *********************************
      *
           MOVE        CX18-CX18K TO DS08-CX18K
           MOVE        CX18-CLID TO DS08-CLID4.
       F70FA-FN. EXIT.
      *N70GA.    NOTE *CALL CI0025 - BANK'S NAME/RTN      *.            AM0025
       F70GA.                                                           lv10
      *                                                                 AM0025
      *********************************                                 AM0025
      ** THIS MODULE WILL READ THE    *                                 AM0025
      ** CLIENT DATABASE TO GET THE   *                                 AM0025
      ** BANK'S NAME AND RTN USING THE*                                 AM0025
      ** BANK CLIENT ID NUMBER PASSED.                                  AM0025
      *********************************                                 AM0025
      *                                                                 AM0025
           INITIALIZE      BR30                                         AM0025
           MOVE        CX18-CLID TO BR30-CLID                           AM0025
           MOVE        003 TO BR30-CDEL1                                AM0025
           MOVE        DS08-DCACG TO BR30-DCACG                         AM0025
           MOVE        ZEROS TO BR30-NRTSQ1                             AM0025
           SET CI0025J-PCB-CL1P-PTR1 TO                                 AM0025
                       PCB-CL1P-PTR1                                    AM0025
           INITIALIZE      DE10-DU03                                    AM0025
           CALL        CI0025 USING                                     AM0025
           DFHEIBLK                                                     AM0025
           DFHCOMMAREA                                                  AM0025
           DLIUIBII                                                     AM0025
           CI0025J-PCB-ADDRESS-LIST                                     AM0025
           BR30                                                         AM0025
           DE10                                                         AM0025
           MS03.                                                        AM0025
       F70GA-FN. EXIT.
      *N70HA.    NOTE *CHECK IF THERE WAS NO ERROR        *.
       F70HA.    IF    MS03-NMESS2 = ZEROS                              lv10
                 AND   DE10-NMESS2 = ZEROS
                 NEXT SENTENCE ELSE GO TO     F70HA-FN.
      *********************************
      ** FORMAT DESTINATION LINES ONLY*
      ** IF THERE ARE NO ERRORS.      *
      *********************************
           INITIALIZE  MS03.
      *N70IA.    NOTE *CALL CI0006 - CLIENT ADDRESS       *.            AM0006
       F70IA.                                                           lv15
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
           MOVE        CX18-CLID TO CA05-CLID                           AM0006
           MOVE        CX21-GECSQ TO CA05-GECSQ1                        AM0006
           MOVE        DS08-DCACG TO CA05-DCACG                         AM0006
           SET CI0006H-PCB-CL1P-PTR1 TO                                 AM0006
                       PCB-CL1P-PTR1                                    AM0006
           INITIALIZE DE10-DU03                                         AM0006
           CALL        CI0006 USING                                     AM0006
           DFHEIBLK                                                     AM0006
           DFHCOMMAREA                                                  AM0006
           DLIUIBII                                                     AM0006
           CI0006H-PCB-ADDRESS-LIST                                     AM0006
           CA05                                                         AM0006
           DE10                                                         AM0006
           MS03.                                                        AM0006
       F70IA-FN. EXIT.
      *N70JA.    NOTE *CHECK IF THERE WAS NO ERROR        *.
       F70JA.    IF    MS03-NMESS2 = ZEROS                              lv15
                 AND   DE10-NMESS2 = ZEROS
                 NEXT SENTENCE ELSE GO TO     F70JA-FN.
      *********************************
      ** FORMAT DESTINATION LINES ONLY*
      ** IF THERE ARE NO ERRORS.      *
      *********************************
           INITIALIZE  MS03
           PERFORM     F92CA THRU F92CA-FN.
       F70JA-FN. EXIT.
       F70HA-900. GO TO F70LA-FN.
       F70HA-FN. EXIT.
      *N70LA.    NOTE **** CI0025 RETURN W/ERROR MSG **   *.
       F70LA.         EXIT.                                             lv10
      *N70MA.    NOTE ** IF CALLING APPLICATION = SD **   *.
       F70MA.    IF    DS08-MAPPN = 'SD        '                        lv15
                 NEXT SENTENCE ELSE GO TO     F70MA-FN.
      *N70NA.    NOTE *** IF PROGRAM ERROR IN CI0025 **   *.
       F70NA.    IF    MS03-NMESS2 NOT = ZEROS                          lv20
                 AND   MS03-CMESB < 11
                 NEXT SENTENCE ELSE GO TO     F70NA-FN.
      *********************************
      ** CI0025 RETURNED AN ERROR     *
      ** THAT IS NOT SEVERE           *
      *********************************
           INITIALIZE  MS03
           MOVE        'UNKNOWN BANK' TO BR30-CLORN
           MOVE        ZERO TO BR30-NTR
           MOVE        ZERO TO BR30-GECKD
           MOVE        'N' TO BR30-IRTNA
           MOVE        SPACES TO CA05-CL24
           MOVE        'O' TO CL01-CLTYP
           MOVE        SPACES TO CL03-C198
           PERFORM     F92CA THRU F92CA-FN.
       F70NA-FN. EXIT.
       F70MA-FN. EXIT.
       F70LA-FN. EXIT.
      *N70ZA.    NOTE *RETURN TO CALLING MODULE           *.
       F70ZA.                                                           lv10
           MOVE                     ALL '1' TO FT GO TO F20.
       F70ZA-FN. EXIT.
       F70-FN.   EXIT.
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
      *N91DD.    NOTE *INSERT PROD SUB-CODE INTO PRCMN    *.            ADU066
       F91DD.                                                           lv10
      *                                                                 ADU066
      *INSERT THE ONE CHARACTER MSP03                                   ADU066
      *INTO THE PRODUCT NAME PRCMN.                                     ADU066
      *A SPACE, '-', AND MSP03 SHOULD                                   ADU066
      *BE INSERTED AFTER THE LAST CHAR-                                 ADU066
      *ACTER OF THE LAST NAME IN PRCMN.                                 ADU066
      *                                                                 ADU066
      *N91DF.    NOTE *INITIALIZATION                     *.            ADU066
       F91DF.                                                           lv15
           MOVE        21 TO W-PRCMN-INDEX                              ADU066
           MOVE        04 TO W-MSP03-INDEX.                             ADU066
       F91DF-FN. EXIT.
      *N91DH.    NOTE *FIND THE CHARACTER IN MSP03        *.            ADU066
       F91DH.                       GO TO     F91DH-B.                  lv15
       F91DH-A.
                 IF    W-MSP03-X (W-MSP03-INDEX)                        ADU066
                       NOT = SPACES                                     ADU066
                 OR    W-MSP03-INDEX < 1                                ADU066
                                    GO TO     F91DH-FN.                 ADU066
       F91DH-B.
           SUBTRACT    1 FROM W-MSP03-INDEX.                            ADU066
       F91DH-900. GO TO F91DH-A.
       F91DH-FN. EXIT.
      *N91DJ.    NOTE *IF A CHARACTER WAS FOUND           *.            ADU066
       F91DJ.    IF    W-MSP03-INDEX > ZERO                             lv15
                 NEXT SENTENCE ELSE GO TO     F91DJ-FN.                 ADU066
      *N91DL.    NOTE *FIND THE PLACE IN PRCMN            *.            ADU066
       F91DL.                                                           lv20
      *                                                                 ADU066
      *PUT THE MSP03 CHARACTER AT THE                                   ADU066
      *END OF PRCMN.  IT SHOULD BE                                      ADU066
      *INSERTED ALONG WITH A SPACE AND                                  ADU066
      *A DASH RIGHT AFTER THE LAST CHAR                                 ADU066
      *IN THE PRCMN NAME.                                               ADU066
      *                                                                 ADU066
      *N91DN.    NOTE *FIND THE PLACE IN PRCMN            *.            ADU066
       F91DN.                       GO TO     F91DN-B.                  lv25
       F91DN-A.
                 IF    W-PRCMN-X (W-PRCMN-INDEX)                        ADU066
                       NOT = SPACES                                     ADU066
                 OR    W-PRCMN-INDEX < 1                                ADU066
                                    GO TO     F91DN-FN.                 ADU066
       F91DN-B.
           SUBTRACT    1 FROM W-PRCMN-INDEX.                            ADU066
       F91DN-900. GO TO F91DN-A.
       F91DN-FN. EXIT.
      *N91DP.    NOTE *IF THERE ARE NOT ENOUGH SPACES     *.            ADU066
       F91DP.    IF    W-PRCMN-INDEX > 16                               lv25
                 NEXT SENTENCE ELSE GO TO     F91DP-FN.                 ADU066
           MOVE        16 TO W-PRCMN-INDEX.                             ADU066
       F91DP-FN. EXIT.
      *N91DR.    NOTE *INSERT THE STUFF                   *.            ADU066
       F91DR.                                                           lv25
           ADD         1 TO W-PRCMN-INDEX                               ADU066
           MOVE        SPACE TO W-PRCMN-X (W-PRCMN-INDEX)               ADU066
           ADD         1 TO W-PRCMN-INDEX                               ADU066
           MOVE        '-' TO W-PRCMN-X (W-PRCMN-INDEX)                 ADU066
           ADD         1 TO W-PRCMN-INDEX                               ADU066
           MOVE        SPACE TO W-PRCMN-X (W-PRCMN-INDEX)               ADU066
           ADD         1 TO W-PRCMN-INDEX                               ADU066
           MOVE        W-MSP03-X (W-MSP03-INDEX) TO                     ADU066
           W-PRCMN-X (W-PRCMN-INDEX).                                   ADU066
       F91DR-FN. EXIT.
       F91DL-FN. EXIT.
       F91DJ-FN. EXIT.
       F91DD-FN. EXIT.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               ******   CALLS TO CI0007    *****   *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92BA.    NOTE *CALL CI0007 - FORMAT DEST LINES    *.            AM0007
       F92BA.                                                           lv10
      *                                                                 AM0007
      *********************************                                 AM0007
      ** THIS MODULE WILL FORMAT THE  *                                 AM0007
      ** DESTINATION LINES TO BE SENT *                                 AM0007
      ** BACK TO THE WORKSTATION FOR  *                                 AM0007
      ** DISPLAY.  ALL THE            *                                 AM0007
      ** APPROPRIATE VALUES NEED TO BE*                                 AM0007
      ** FILLED IN SEGMENT DI02.      *                                 AM0007
      ** THE HOW, WHO AND WHERE LINES *                                 AM0007
      ** WILL BE SENT BACK IN SEGMENT *                                 AM0007
      ** DL01                         *                                 AM0007
      *********************************                                 AM0007
      *                                                                 AM0007
           MOVE        'B ' TO DI02-CPAY1                               AM0007
           MOVE        DS08-MAPPN TO DI02-MAPPN                         AM0007
           MOVE        AB04-CTTLN1 TO DI02-CTTLN1                       AM0007
           MOVE        AB04-CTTLN2 TO DI02-CTTLN2                       AM0007
           MOVE        AB04-CTTLN3 TO DI02-CTTLN3                       AM0007
           MOVE        CA05-CL24 TO DI02-CL24                           AM0007
           MOVE        CL01-CLTYP TO DI02-CLTYP                         AM0007
           MOVE        BR30-CLORN TO DI02-CLORN                         AM0007
           MOVE        CL03-C198 TO DI02-C198                           AM0007
           MOVE        CX21-TDELI TO DI02-TDELI                         AM0007
           MOVE        CX18-NPBN TO DI02-NPBN                           AM0007
           MOVE        CX18-CCBAT TO DI02-CCBAT                         AM0007
           MOVE        BR30-NTR TO DI02-NTR                             AM0007
           MOVE        BR30-GECKD TO DI02-GECKD1                        AM0007
           MOVE        CT01-C299 TO DI02-C299                           AM0007
           MOVE        CT01-GECKD TO DI02-GECKD                         AM0007
           MOVE        TA5A-PRCMN TO DI02-PRCMN                         AM0007
           MOVE        CX18-MCSIG TO DI02-MCSIG                         AM0007
           MOVE        DS08-ISHAD TO DI02-ISHAD                         AM0007
           INITIALIZE      DL01                                         AM0007
           CALL        CI0007 USING                                     AM0007
           DFHEIBLK                                                     AM0007
           DFHCOMMAREA                                                  AM0007
           DI02                                                         AM0007
           DL01                                                         AM0007
           MS03.                                                        AM0007
           MOVE        DL01 TO DS08-DU01.                               AM0007
       F92BA-FN. EXIT.
      *N92CA.    NOTE *CALL CI0007 - FORMAT DEST LINES    *.            AM0007
       F92CA.                                                           lv10
      *                                                                 AM0007
      *********************************                                 AM0007
      ** THIS MODULE WILL FORMAT THE  *                                 AM0007
      ** DESTINATION LINES TO BE SENT *                                 AM0007
      ** BACK TO THE WORKSTATION FOR  *                                 AM0007
      ** DISPLAY.  ALL THE            *                                 AM0007
      ** APPROPRIATE VALUES NEED TO BE*                                 AM0007
      ** FILLED IN SEGMENT DI02.      *                                 AM0007
      ** THE HOW, WHO AND WHERE LINES *                                 AM0007
      ** WILL BE SENT BACK IN SEGMENT *                                 AM0007
      ** DL01                         *                                 AM0007
      *********************************                                 AM0007
      *                                                                 AM0007
           MOVE        'D ' TO DI02-CPAY1                               AM0007
           MOVE        DS08-MAPPN TO DI02-MAPPN                         AM0007
           MOVE        AB04-CTTLN1 TO DI02-CTTLN1                       AM0007
           MOVE        AB04-CTTLN2 TO DI02-CTTLN2                       AM0007
           MOVE        AB04-CTTLN3 TO DI02-CTTLN3                       AM0007
           MOVE        CA05-CL24 TO DI02-CL24                           AM0007
           MOVE        CL01-CLTYP TO DI02-CLTYP                         AM0007
           MOVE        BR30-CLORN TO DI02-CLORN                         AM0007
           MOVE        CL03-C198 TO DI02-C198                           AM0007
           MOVE        CX21-TDELI TO DI02-TDELI                         AM0007
           MOVE        CX18-NPBN TO DI02-NPBN                           AM0007
           MOVE        CX18-CCBAT TO DI02-CCBAT                         AM0007
           MOVE        BR30-NTR TO DI02-NTR                             AM0007
           MOVE        BR30-GECKD TO DI02-GECKD1                        AM0007
           MOVE        CT01-C299 TO DI02-C299                           AM0007
           MOVE        CT01-GECKD TO DI02-GECKD                         AM0007
           MOVE        TA5A-PRCMN TO DI02-PRCMN                         AM0007
           MOVE        CX18-MCSIG TO DI02-MCSIG                         AM0007
           MOVE        DS08-ISHAD TO DI02-ISHAD                         AM0007
           INITIALIZE      DL01                                         AM0007
           CALL        CI0007 USING                                     AM0007
           DFHEIBLK                                                     AM0007
           DFHCOMMAREA                                                  AM0007
           DI02                                                         AM0007
           DL01                                                         AM0007
           MS03.                                                        AM0007
           MOVE        DL01 TO DS08-DU01.                               AM0007
       F92CA-FN. EXIT.
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
      *N94CB.    NOTE *CALL GU ON CX18                    *.            ADU026
       F94CB.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX18                                                    ADU026
           S-CXU01-SSA S-CXU18-SSA                                      ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CB-FN. EXIT.
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
      *N94CO.    NOTE *CALL GN ON CL12                    *.            ADU026
       F94CO.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PB06 CL12                                                    ADU026
           S-CLU01-SSA S-CL12-SSA                                       ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CO-FN. EXIT.
      *N94CP.    NOTE *CALL GN ON CL03                    *.            ADU026
       F94CP.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PB06 CL03                                                    ADU026
           S-CLU01-SSA S-CL03-SSA                                       ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CP-FN. EXIT.
      *N94CS.    NOTE *CALL GU ON CX21                    *.            ADU026
       F94CS.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX21' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX21                                                    ADU026
           S-CXU01-SSA S-CXU21-SSA                                      ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CS-FN. EXIT.
      *N94CT.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94CT.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PC06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
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
