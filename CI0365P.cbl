       IDENTIFICATION DIVISION.                                         CI0365
       PROGRAM-ID.  CI0365P.                                            CI0365
      *AUTHOR.         FA ONE TIME TRANS DRIVER.                        CI0365
      *DATE-COMPILED.   09/08/14.                                       CI0365
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2013                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE FA     SYSTEM AND ALL INFORMATION RELATING THERETO,    *ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE FA     SYSTEM AND ALL            *ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE FA           *ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 2013                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0365
       CONFIGURATION SECTION.                                           CI0365
       SOURCE-COMPUTER. IBM-370.                                        CI0365
       OBJECT-COMPUTER. IBM-370.                                        CI0365
       DATA DIVISION.                                                   CI0365
       WORKING-STORAGE SECTION.                                         CI0365
       COPY ACFUAREA.
      *                                                                 AM0108
      ******************************************************************AM0108
      ** WORKING STORAGE SEGMENT CI0108                                *AM0108
      ******************************************************************AM0108
      *                                                                 AM0108
      *!WF DSP=CF DSL=PJ SEL=47 FOR=I LEV=1                             AM0108
       01                 CF00.                                         CI0365
          05              CF00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00147).                  CI0365
       01                 CF47  REDEFINES      CF00.                    CI0365
            10            CF47-MAPPN  PICTURE  X(10).                   CI0365
            10            CF47-CTID   PICTURE  X(27)                    CI0365
                          OCCURS       002     TIMES.                   CI0365
            10            CF47-ICUST  PICTURE  X                        CI0365
                          OCCURS       002     TIMES.                   CI0365
            10            CF47-IACFPD PICTURE  X(1).                    CI0365
            10            CF47-AFEED  PICTURE  S9(5)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            CF47-IWAIV  PICTURE  X.                       CI0365
            10            CF47-AFEET  PICTURE  S9(5)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            CF47-ITERF  PICTURE  X.                       CI0365
            10            CF47-ATERF  PICTURE  S9(5)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            CF47-CCACT  PICTURE  99.                      CI0365
            10            CF47-CPLTYP PICTURE  X(14).                   CI0365
            10            CF47-CSPDT  PICTURE  X.                       CI0365
            10            CF47-CPAYF  PICTURE  X(2).                    CI0365
            10            CF47-FILLER PICTURE  X(47).                   CI0365
      *                                                                 AM0108
       01  7-CF00-STAGING.                                              AM0108
      *!WI pl=CF080                                                     AM0108
           05  7-CF00-MAPPN            VALUE SPACES                     AM0108
                        PICTURE X(10).                                  CI0365
      *!WI pl=CF090                                                     AM0108
           05  7-CF00-CTID    OCCURS 2                                  AM0108
                        PICTURE X(27).                                  CI0365
      *!WI pl=CF100                                                     AM0108
           05  7-CF00-ICUST   OCCURS 2                                  AM0108
                        PICTURE X.                                      CI0365
      *!WI pl=CF110                                                     AM0108
           05  7-CF00-CSPDT            VALUE SPACE                      AM0108
                        PICTURE X.                                      CI0365
      *!WI pl=CF120                                                     AM0108
           05  7-CF00-CPAYF            VALUE SPACES                     AM0108
                        PICTURE X(2).                                   CI0365
      ******************************************************************AM0108
      **     PCB ADDRESS LIST FOR CI0108.  MODULE CI0108 WILL NEED     *AM0108
      **     PCB'S FOR:                                                *AM0108
      **             CONTRACT DATABASE           (CT1P)                *AM0108
      **             CLIENT DATABASE             (CL1P)                *AM0108
      **             GROUP DATABASE              (GR1P)                *AM0108
      **             ACCOUNT ACTIVITY DATABASE   (ACAP)                *AM0108
      **                                                               *AM0108
      ******************************************************************AM0108
      *                                                                 AM0108
       01  CI0108-PCB-ADDR-LIST.                                        AM0108
           05  CI0108-PCB-CL1P-PTR1      POINTER.                       AM0108
           05  CI0108-PCB-CT1P-PTR1      POINTER.                       AM0108
           05  CI0108-PCB-GR1P-PTR1      POINTER.                       AM0108
           05  CI0108-PCB-ACAP-PTR1      POINTER.                       AM0108
       01                 CL01.                                         CI0365
            10            CL01-CL01K.                                   CI0365
            11            CL01-C199.                                    CI0365
            12            CL01-CLID.                                    CI0365
            13            CL01-CLIDO  PICTURE  9(3).                    CI0365
            13            CL01-CLIDN.                                   CI0365
            14            CL01-CLIDNP PICTURE  X(12).                   CI0365
            14            CL01-CLIDND PICTURE  9(8).                    CI0365
            10            CL01-GECKD  PICTURE  9.                       CI0365
            10            CL01-GEMDA  PICTURE  9(8).                    CI0365
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0365
                          BINARY.                                       CI0365
            10            CL01-GECUC  PICTURE  99.                      CI0365
            10            CL01-CLDOR  PICTURE  9(8).                    CI0365
            10            CL01-CLLNG  PICTURE  XX.                      CI0365
            10            CL01-GESLC  PICTURE  99.                      CI0365
            10            CL01-CLTYP  PICTURE  X.                       CI0365
            10            CL01-CLCLS  PICTURE  9(3).                    CI0365
            10            CL01-CLTWRC PICTURE  99.                      CI0365
            10            CL01-CLPVC  PICTURE  99.                      CI0365
            10            CL01-CLIND  PICTURE  9(3).                    CI0365
            10            CL01-CLTRC  PICTURE  99.                      CI0365
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            CL01-AYSIDA PICTURE  9(3).                    CI0365
            10            CL01-AYSID  PICTURE  9(5).                    CI0365
            10            CL01-CLSTR  PICTURE  9(2).                    CI0365
            10            CL01-CLC11  PICTURE  X.                       CI0365
            10            CL01-CLTIN  PICTURE  9(12).                   CI0365
            10            CL01-CLTND  PICTURE  9(8).                    CI0365
            10            CL01-CLTINC PICTURE  9.                       CI0365
            10            CL01-CCDWA  PICTURE  9.                       CI0365
            10            CL01-CICES  PICTURE  X.                       CI0365
            10            CL01-CLTRA  PICTURE  9(2).                    CI0365
            10            CL01-DIRSY  PICTURE  9(4)                     CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            CL01-CFEDS  PICTURE  X.                       CI0365
            10            CL01-FILLER PICTURE  X(06).                   CI0365
       01                 CL03.                                         CI0365
            10            CL03-GEDLA  PICTURE  9(8).                    CI0365
            10            CL03-DDREP  PICTURE  9(8).                    CI0365
            10            CL03-DPRFR  PICTURE  9(8).                    CI0365
            10            CL03-IACCI  PICTURE  X.                       CI0365
            10            CL03-CLDOB  PICTURE  9(8).                    CI0365
            10            CL03-CLDOD  PICTURE  9(8).                    CI0365
            10            CL03-CLDTH  PICTURE  X.                       CI0365
            10            CL03-CCINI  PICTURE  X.                       CI0365
            10            CL03-FILLER PICTURE  X(1).                    CI0365
            10            CL03-CLAIN  PICTURE  S9(11)                   CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            CL03-CCAOD  PICTURE  999.                     CI0365
            10            CL03-CLMAR  PICTURE  X.                       CI0365
            10            CL03-C198.                                    CI0365
            11            CL03-CLNAM.                                   CI0365
            12            CL03-CLNAMH PICTURE  X(6).                    CI0365
            12            CL03-CLNAMF PICTURE  X(20).                   CI0365
            12            CL03-CLNAMM.                                  CI0365
            13            CL03-CLNAMI PICTURE  X.                       CI0365
            13            CL03-CLNAMR PICTURE  X(14).                   CI0365
            12            CL03-CLNAML PICTURE  X(25).                   CI0365
            12            CL03-CLNAMS PICTURE  X(4).                    CI0365
            10            CL03-FILLER PICTURE  X(10).                   CI0365
            10            CL03-MPRFS  PICTURE  X(4).                    CI0365
            10            CL03-CLOCC  PICTURE  9(3).                    CI0365
            10            CL03-CLRET  PICTURE  X.                       CI0365
            10            CL03-IOCOB  PICTURE  X.                       CI0365
            10            CL03-CLSEX  PICTURE  X.                       CI0365
            10            CL03-CLWIL  PICTURE  X.                       CI0365
            10            CL03-GECFC  PICTURE  99.                      CI0365
            10            CL03-GECFY  PICTURE  9(4).                    CI0365
            10            CL03-ICUSC  PICTURE  X.                       CI0365
            10            CL03-MCTYC  PICTURE  X(20).                   CI0365
            10            CL03-CLWIP  PICTURE  X.                       CI0365
            10            CL03-CLCTXF PICTURE  99.                      CI0365
            10            CL03-CLCUS  PICTURE  99.                      CI0365
            10            CL03-NPDLU  PICTURE  9(5).                    CI0365
            10            CL03-CLEMI  PICTURE  X.                       CI0365
            10            CL03-GEPHNH PICTURE  X(14).                   CI0365
            10            CL03-GEPHNB PICTURE  X(14).                   CI0365
            10            CL03-GEPHNX PICTURE  9(4).                    CI0365
            10            CL03-GEPHNA PICTURE  X(14).                   CI0365
            10            CL03-FILLER PICTURE  X(3).                    CI0365
            10            CL03-IAPRT  PICTURE  X.                       CI0365
            10            CL03-CEMSC  PICTURE  X.                       CI0365
            10            CL03-CSEPS  PICTURE  X.                       CI0365
            10            CL03-CRACE  PICTURE  X.                       CI0365
            10            CL03-CNIRA  PICTURE  X.                       CI0365
            10            CL03-FILLER PICTURE  X(11).                   CI0365
       01                 CL12.                                         CI0365
            10            CL12-GEDLA  PICTURE  9(8).                    CI0365
            10            CL12-CLBCD  PICTURE  9(3).                    CI0365
            10            CL12-CLFDW  PICTURE  X.                       CI0365
            10            CL12-CLOSD  PICTURE  9(8).                    CI0365
            10            CL12-CLOED  PICTURE  9(8).                    CI0365
            10            CL12-CLOEI  PICTURE  X.                       CI0365
            10            CL12-CLIBN  PICTURE  X(20).                   CI0365
            10            CL12-CLINT  PICTURE  9(3).                    CI0365
            10            CL12-CLONE  PICTURE  9(9).                    CI0365
            10            CL12-CLORC  PICTURE  99.                      CI0365
            10            CL12-CLORN  PICTURE  X(45).                   CI0365
            10            CL12-CLORP  PICTURE  X(25).                   CI0365
            10            CL12-GEPHNB PICTURE  X(14).                   CI0365
            10            CL12-GEPHNX PICTURE  9(4).                    CI0365
            10            CL12-GEPHNA PICTURE  X(14).                   CI0365
            10            CL12-GEFYE  PICTURE  9(4).                    CI0365
            10            CL12-AYCDE  PICTURE  9(3).                    CI0365
            10            CL12-AYID   PICTURE  9(5).                    CI0365
            10            CL12-CFOBO  PICTURE  99.                      CI0365
            10            CL12-CLINRG                                   CI0365
                          OCCURS       003     TIMES.                   CI0365
            11            CL12-CLIRT  PICTURE  99.                      CI0365
            11            CL12-CLINR  PICTURE  X(3).                    CI0365
            11            CL12-CLIRD  PICTURE  9(8).                    CI0365
            10            CL12-IOTXE  PICTURE  X.                       CI0365
            10            CL12-IO501  PICTURE  X.                       CI0365
            10            CL12-IOFOG  PICTURE  X.                       CI0365
            10            CL12-IOPRA  PICTURE  X.                       CI0365
            10            CL12-IOSCS  PICTURE  X.                       CI0365
            10            CL12-IACHA  PICTURE  X.                       CI0365
            10            CL12-IFORG  PICTURE  X.                       CI0365
            10            CL12-IFIND  PICTURE  X.                       CI0365
            10            CL12-CFCNT3 PICTURE  X(2).                    CI0365
            10            CL12-FILLER PICTURE  X(06).                   CI0365
       01                 CL18.                                         CI0365
            10            CL18-CL18K.                                   CI0365
            11            CL18-NRTSQ  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            CL18-NTR    PICTURE  9(8).                    CI0365
            10            CL18-GECKD  PICTURE  9.                       CI0365
            10            CL18-GEEND  PICTURE  9(8).                    CI0365
            10            CL18-NPDIN  PICTURE  X(4).                    CI0365
            10            CL18-IRTNA  PICTURE  X.                       CI0365
            10            CL18-IRTNP  PICTURE  X.                       CI0365
            10            CL18-IRTNW  PICTURE  X.                       CI0365
       01                 CL2Y.                                         CI0365
            10            CL2Y-CL2YK.                                   CI0365
            11            CL2Y-NTR    PICTURE  9(8).                    CI0365
            11            CL2Y-C199.                                    CI0365
            12            CL2Y-CLID.                                    CI0365
            13            CL2Y-CLIDO  PICTURE  9(3).                    CI0365
            13            CL2Y-CLIDN.                                   CI0365
            14            CL2Y-CLIDNP PICTURE  X(12).                   CI0365
            14            CL2Y-CLIDND PICTURE  9(8).                    CI0365
            11            CL2Y-NRTSQ  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0003           PIC X(8) VALUE 'CI0003P '.
       01  CI0014           PIC X(8) VALUE 'CI0014P '.                  AM0014
       01  CI0016           PIC X(8) VALUE 'CI0016P '.                  AM0016
       01  CI0018           PIC X(8) VALUE 'CI0018P '.
       01  CI0019           PIC X(8) VALUE 'CI0019P '.
       01  CI0020           PIC X(8) VALUE 'CI0020P '.                  AM0020
       01  CI0074           PIC X(8) VALUE 'CI0074P '.                  AM0074
       01  CI0108           PIC X(8) VALUE 'CI0108P '.                  AM0108
       01  CI0273           PIC X(8) VALUE 'CI0273P '.                  AM0273
      *                                                                 ADU155
      ******************************************************************ADU155
      ** WORK AREA NEEDED FOR MACRO ADU155                             *ADU155
      **        DATE COMMON AREA FOR EXECUTING CICS ASKTIME/FORMATTIME *ADU155
      ******************************************************************ADU155
      *                                                                 ADU155
      *!WI pl=DD100                                                     ADU155
       01  DD01-XMSTS                                                   ADU155
                        PICTURE S9(15)                                  CI0365
                          COMPUTATIONAL-3.                              CI0365
       01  DD01-F2CCYY             PIC S9(08) COMP.                     ADU155
      *!WI pl=DD200                                                     ADU155
       01  DD01-XDAT69                                                  ADU155
                        PICTURE 9(6).                                   CI0365
       01  DD01-UDATE.                                                  ADU155
           05  DD01-YEAR           PIC  9(04).                          ADU155
           05  DD01-MMDD           PIC  9(04).                          ADU155
      *!WI pl=DD280                                                     ADU155
       01  DD01-XDATCU REDEFINES DD01-UDATE                             ADU155
                        PICTURE X(8).                                   CI0365
       01  DEL-ER   PIC 9(01).
      *!WF DSP=DD DSL=XW SEL=0301 FOR=I LEV=1 PLT=DD
       01                 DD00.                                         CI0365
          05              DD00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00137).                  CI0365
       01                 DD01  REDEFINES      DD00.                    CI0365
            10            DD01-XERRCT PICTURE  9(3).                    CI0365
            10            DD01-XERRNU.                                  CI0365
            11            DD01-XERRN1 PICTURE  X.                       CI0365
            11            DD01-XERRN2 PICTURE  99.                      CI0365
            10            DD01-XTITLE PICTURE  X(40).                   CI0365
            10            DD01-XDATE  PICTURE  X(8).                    CI0365
            10            DD01-XOC    PICTURE  X.                       CI0365
            10            DD01-XER    PICTURE  X.                       CI0365
            10            DD01-XTRAIM PICTURE  X(80).                   CI0365
            10            DD01-XIPRIN PICTURE  X.                       CI0365
       01                 DD03  REDEFINES      DD00.                    CI0365
            10            DD03-XDATG.                                   CI0365
            11            DD03-XDAT1.                                   CI0365
            12            DD03-XDAT19 PICTURE  99.                      CI0365
            11            DD03-XDAT2.                                   CI0365
            12            DD03-XDAT29 PICTURE  99.                      CI0365
            11            DD03-XDAT3.                                   CI0365
            12            DD03-XDAT39 PICTURE  99.                      CI0365
            11            DD03-XDAT4.                                   CI0365
            12            DD03-XDAT49 PICTURE  99.                      CI0365
            10            DD03-XLEAPY PICTURE  99.                      CI0365
            10            DD03-DTGCY  PICTURE  9(4).                    CI0365
            10            DD03-FILLER                                   CI0365
                          REDEFINES            DD03-DTGCY.              CI0365
            11            DD03-DTGCC  PICTURE  9(2).                    CI0365
            11            DD03-DTGYY  PICTURE  9(2).                    CI0365
            10       FILLER         PICTURE  X(00123).                  CI0365
      *                                                                 ADU155
      ******************************************************            AADA82
      ****      WORK AREAS FOR COMMON DATE UTILITY       ***            AADA82
      ******************************************************            AADA82
      **                                                                AADA82
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA82
      **                                                                AADA82
      **   SEGMENT DD30 - FUNCTION LAYOUT                               AADA82
      **                                                                AADA82
      *!WF DSP=DF DSL=DD SEL=30 FOR=I DES=2 LEV=1                       AADA82
       01                 DF30.                                         CI0365
            10            DF30-CDTFN  PICTURE  9(4)                     CI0365
                          VALUE                ZERO.                    CI0365
            10            DF30-CDTSF  PICTURE  9(4)                     CI0365
                          VALUE                ZERO.                    CI0365
            10            DF30-CDTSC  PICTURE  9(4)                     CI0365
                          VALUE                ZERO.                    CI0365
            10            DF30-FILLER PICTURE  X(40)                    CI0365
                          VALUE                SPACE.                   CI0365
       01                 DF34.                                         CI0365
            10            DF34-CAINS  PICTURE  X(03)                    CI0365
                          VALUE                SPACE.                   CI0365
            10            DF34-CDTUC  PICTURE  9                        CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-NDTUN  PICTURE  S9(05)                   CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-FILLER PICTURE  X(162)                   CI0365
                          VALUE                SPACE.                   CI0365
            10            DF34-DTGRG.                                   CI0365
            11            DF34-DTGCY.                                   CI0365
            12            DF34-DTGCC  PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            12            DF34-DTGYY  PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            11            DF34-DTGMM  PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            11            DF34-DTGDD  PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-DTJUL.                                   CI0365
            11            DF34-DTJCY.                                   CI0365
            12            DF34-DTJCC  PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            12            DF34-DTJYY  PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            11            DF34-DTJDD  PICTURE  9(3)                     CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTFM  PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTLM  PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTFF  PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTLF  PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTFW  PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTLW  PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CCDOWA PICTURE  9                        CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CCDRW  PICTURE  9                        CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-FILLER PICTURE  X(58)                    CI0365
                          VALUE                SPACE.                   CI0365
            10            DF34-DTGRGA.                                  CI0365
            11            DF34-DTGCYA.                                  CI0365
            12            DF34-DTGCCA PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            12            DF34-DTGYYA PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            11            DF34-DTGMMA PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            11            DF34-DTGDDA PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-DTJULA.                                  CI0365
            11            DF34-DTJCYA.                                  CI0365
            12            DF34-DTJCCA PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            12            DF34-DTJYYA PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            11            DF34-DTJDDA PICTURE  9(3)                     CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTFMA PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTLMA PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTFFA PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTLFA PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTFWA PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTLWA PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CCDOWB PICTURE  9                        CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CCDRWA PICTURE  9                        CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-FILLER PICTURE  X(58)                    CI0365
                          VALUE                SPACE.                   CI0365
            10            DF34-DTGRGB.                                  CI0365
            11            DF34-DTGCYB.                                  CI0365
            12            DF34-DTGCCB PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            12            DF34-DTGYYB PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            11            DF34-DTGMMB PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            11            DF34-DTGDDB PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-DTJULB.                                  CI0365
            11            DF34-DTJCYB.                                  CI0365
            12            DF34-DTJCCB PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            12            DF34-DTJYYB PICTURE  9(2)                     CI0365
                          VALUE                ZERO.                    CI0365
            11            DF34-DTJDDB PICTURE  9(3)                     CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTFMB PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTLMB PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTFFB PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTLFB PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTFWB PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CDTLWB PICTURE  9(01)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CCDOWC PICTURE  9                        CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-CCDRWB PICTURE  9                        CI0365
                          VALUE                ZERO.                    CI0365
            10            DF34-FILLER PICTURE  X(58)                    CI0365
                          VALUE                SPACE.                   CI0365
            10            DF34-FILLER PICTURE  X(40)                    CI0365
                          VALUE                SPACE.                   CI0365
      **                                                                AADA82
      **   SEGMENT DD34 - CONVERT DATE LAYOUT                           AADA82
      **                                                                AADA82
      *!WF DSP=DF DSL=DD SEL=34 FOR=I DES=2 LEV=1                       AADA82
      **                                                                AADA82
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0365
            10            XW05-XW06.                                    CI0365
            11            XW05-XDBPCB.                                  CI0365
            12            XW05-XDBDNM PICTURE  X(08)                    CI0365
                          VALUE                SPACE.                   CI0365
            12            XW05-XSEGLV PICTURE  X(02)                    CI0365
                          VALUE                SPACE.                   CI0365
            12            XW05-XRC    PICTURE  X(02)                    CI0365
                          VALUE                SPACE.                   CI0365
            12            XW05-XPROPT PICTURE  X(04)                    CI0365
                          VALUE                SPACE.                   CI0365
            12            XW05-FILLER PICTURE  S9(5)                    CI0365
                          VALUE                ZERO                     CI0365
                          BINARY.                                       CI0365
            12            XW05-XSEGNM PICTURE  X(08)                    CI0365
                          VALUE                SPACE.                   CI0365
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0365
                          VALUE                ZERO                     CI0365
                          BINARY.                                       CI0365
            12            XW05-XSEGNB PICTURE  9(05)                    CI0365
                          VALUE                ZERO                     CI0365
                          BINARY.                                       CI0365
            12            XW05-XCOKEY PICTURE  X(70)                    CI0365
                          VALUE                SPACE.                   CI0365
            10            XW05-XW07.                                    CI0365
            11            XW05-XIOPCB.                                  CI0365
            12            XW05-XTERMI PICTURE  X(08)                    CI0365
                          VALUE                SPACE.                   CI0365
            12            XW05-FILLER PICTURE  XX                       CI0365
                          VALUE                SPACE.                   CI0365
            12            XW05-XRC1   PICTURE  X(02)                    CI0365
                          VALUE                SPACE.                   CI0365
            12            XW05-FILLER PICTURE  X(12)                    CI0365
                          VALUE                SPACE.                   CI0365
            12            XW05-XMODNM PICTURE  X(8)                     CI0365
                          VALUE                SPACE.                   CI0365
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0365
                          VALUE                ZERO.                    CI0365
            10            XW05-XGU    PICTURE  X(4)                     CI0365
                          VALUE                'GU  '.                  CI0365
            10            XW05-XGHU   PICTURE  X(4)                     CI0365
                          VALUE                'GHU '.                  CI0365
            10            XW05-XGN    PICTURE  X(4)                     CI0365
                          VALUE                'GN  '.                  CI0365
            10            XW05-XGHN   PICTURE  X(4)                     CI0365
                          VALUE                'GHN '.                  CI0365
            10            XW05-XGNP   PICTURE  X(4)                     CI0365
                          VALUE                'GNP '.                  CI0365
            10            XW05-XGHNP  PICTURE  X(4)                     CI0365
                          VALUE                'GHNP'.                  CI0365
            10            XW05-XREPL  PICTURE  XXXX                     CI0365
                          VALUE                'REPL'.                  CI0365
            10            XW05-XISRT  PICTURE  X(4)                     CI0365
                          VALUE                'ISRT'.                  CI0365
            10            XW05-XDLET  PICTURE  X(4)                     CI0365
                          VALUE                'DLET'.                  CI0365
            10            XW05-XOPEN  PICTURE  X(4)                     CI0365
                          VALUE                'OPEN'.                  CI0365
            10            XW05-XCLSE  PICTURE  X(4)                     CI0365
                          VALUE                'CLSE'.                  CI0365
            10            XW05-XCHKP  PICTURE  X(4)                     CI0365
                          VALUE                'CHKP'.                  CI0365
            10            XW05-XXRST  PICTURE  X(4)                     CI0365
                          VALUE                'XRST'.                  CI0365
            10            XW05-XTERM  PICTURE  X(4)                     CI0365
                          VALUE                'TERM'.                  CI0365
            10            XW05-XNFPAC PICTURE  X(13)                    CI0365
                          VALUE                SPACE.                   CI0365
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0365
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0365
       01                 DO13.                                         CI0365
            10            DO13-C299.                                    CI0365
            11            DO13-CTID.                                    CI0365
            12            DO13-CTIDA  PICTURE  9(3).                    CI0365
            12            DO13-CTIDN.                                   CI0365
            13            DO13-CTIDNP PICTURE  X(13).                   CI0365
            13            DO13-CTIDND PICTURE  9(11).                   CI0365
            10            DO13-CTID01.                                  CI0365
            11            DO13-CTIDA1 PICTURE  9(3).                    CI0365
            11            DO13-CTIDN1.                                  CI0365
            12            DO13-CTIDP1 PICTURE  X(13).                   CI0365
            12            DO13-CTIDNA PICTURE  9(11).                   CI0365
            10            DO13-DCACG  PICTURE  9(8).                    CI0365
            10            DO13-IOWNC  PICTURE  X.                       CI0365
            10            DO13-MAPPN  PICTURE  X(10).                   CI0365
            10            DO13-FILLER PICTURE  X(90).                   CI0365

      *PASS AREA TO/FROM CI0006
      *!WF DSP=DU DSL=DU SEL=05 FOR=I LEV=1 PLT=DU
       01                 DU00.                                         CI0365
          05              DU00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00435).                  CI0365
       01                 DU05  REDEFINES      DU00.                    CI0365
            10            DU05-C199.                                    CI0365
            11            DU05-CLID.                                    CI0365
            12            DU05-CLIDO  PICTURE  9(3).                    CI0365
            12            DU05-CLIDN.                                   CI0365
            13            DU05-CLIDNP PICTURE  X(12).                   CI0365
            13            DU05-CLIDND PICTURE  9(8).                    CI0365
            10            DU05-GECSQ1 PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            DU05-DCACG  PICTURE  9(8).                    CI0365
            10            DU05-FILLER PICTURE  X(100).                  CI0365
            10            DU05-CL24.                                    CI0365
            11            DU05-GELL   PICTURE  9(4)                     CI0365
                          BINARY.                                       CI0365
            11            DU05-CL24K.                                   CI0365
            12            DU05-GECSQ  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            11            DU05-GECSD  PICTURE  9(8).                    CI0365
            11            DU05-GECED  PICTURE  9(8).                    CI0365
            11            DU05-CREQ2  PICTURE  X.                       CI0365
            11            DU05-FILLER PICTURE  X(4).                    CI0365
            11            DU05-GECTA  PICTURE  X.                       CI0365
            11            DU05-GELCD  PICTURE  9(8).                    CI0365
            11            DU05-GEADS  PICTURE  9.                       CI0365
            11            DU05-GECIT  PICTURE  X(25).                   CI0365
            11            DU05-GECTRY PICTURE  X(20).                   CI0365
            11            DU05-GECTY  PICTURE  9(3).                    CI0365
            11            DU05-GEPCD  PICTURE  X(12).                   CI0365
            11            DU05-GEST   PICTURE  X(8).                    CI0365
            11            DU05-IRESA  PICTURE  X.                       CI0365
            11            DU05-FILLER PICTURE  X(8).                    CI0365
            11            DU05-GESAD  PICTURE  X(30)                    CI0365
                          OCCURS       003     TIMES.                   CI0365
            10            DU05-FILLER PICTURE  X(100).                  CI0365
      *

      *PASS AREA TO/FROM CI0003 (OWNERSHIP LINES) - SOURCE ACCT
      *!WF DSP=FA DSL=DU SEL=04 FOR=I DES=1 LEV=1 PLT=FA
       01                 FA04.                                         CI0365
            10            FA04-C299.                                    CI0365
            11            FA04-CTID.                                    CI0365
            12            FA04-CTIDA  PICTURE  9(3).                    CI0365
            12            FA04-CTIDN.                                   CI0365
            13            FA04-CTIDNP PICTURE  X(13).                   CI0365
            13            FA04-CTIDND PICTURE  9(11).                   CI0365
            10            FA04-IPOCH  PICTURE  X.                       CI0365
            10            FA04-FILLER PICTURE  X(099).                  CI0365
            10            FA04-CTTLN1 PICTURE  X(30).                   CI0365
            10            FA04-CTTLN2 PICTURE  X(30).                   CI0365
            10            FA04-CTTLN3 PICTURE  X(30).                   CI0365
            10            FA04-CTTBO1 PICTURE  X(45).                   CI0365
            10            FA04-CTTBO2 PICTURE  X(45).                   CI0365
            10            FA04-CTOWN  PICTURE  9(3).                    CI0365
            10            FA04-IUGMA  PICTURE  X.                       CI0365
            10            FA04-FILLER PICTURE  X(096).                  CI0365

      *PASS AREA TO/FROM CI0018 (CLIENTS) - SOURCE ACCT
      *!WF DSP=FC DSL=DU SEL=14 FOR=I DES=1 LEV=1 PLT=FC
       01                 FC14.                                         CI0365
            10            FC14-C299.                                    CI0365
            11            FC14-CTID.                                    CI0365
            12            FC14-CTIDA  PICTURE  9(3).                    CI0365
            12            FC14-CTIDN.                                   CI0365
            13            FC14-CTIDNP PICTURE  X(13).                   CI0365
            13            FC14-CTIDND PICTURE  9(11).                   CI0365
            10            FC14-DCACG  PICTURE  9(8).                    CI0365
            10            FC14-IPOCH  PICTURE  X.                       CI0365
            10            FC14-FILLER PICTURE  X(100).                  CI0365
            10            FC14-CLID01.                                  CI0365
            11            FC14-CLIDO1 PICTURE  X(3).                    CI0365
            11            FC14-NCLID1.                                  CI0365
            12            FC14-CLIDP1 PICTURE  X(12).                   CI0365
            12            FC14-CLIDNA PICTURE  9(8).                    CI0365
            10            FC14-CLCTR  PICTURE  9(3).                    CI0365
            10            FC14-DU21                                     CI0365
                          OCCURS       025     TIMES.                   CI0365
            11            FC14-C199.                                    CI0365
            12            FC14-CLID.                                    CI0365
            13            FC14-CLIDO  PICTURE  9(3).                    CI0365
            13            FC14-CLIDN.                                   CI0365
            14            FC14-CLIDNP PICTURE  X(12).                   CI0365
            14            FC14-CLIDND PICTURE  9(8).                    CI0365
            11            FC14-CLCTRC PICTURE  9(3).                    CI0365
            10            FC14-QITEM  PICTURE  9(3).                    CI0365
            10            FC14-XIMAX  PICTURE  S9(4)                    CI0365
                          BINARY.                                       CI0365
            10            FC14-CRROL  PICTURE  X.                       CI0365
            10            FC14-FILLER PICTURE  X(099).                  CI0365

      *PASS AREA TO/FROM CI0019 (GROUPS) - SOURCE ACCT
      *!WF DSP=FG DSL=DU SEL=15 FOR=I DES=1 LEV=1 PLT=FG
       01                 FG15.                                         CI0365
            10            FG15-C299.                                    CI0365
            11            FG15-CTID.                                    CI0365
            12            FG15-CTIDA  PICTURE  9(3).                    CI0365
            12            FG15-CTIDN.                                   CI0365
            13            FG15-CTIDNP PICTURE  X(13).                   CI0365
            13            FG15-CTIDND PICTURE  9(11).                   CI0365
            10            FG15-DCACG  PICTURE  9(8).                    CI0365
            10            FG15-IPOCH  PICTURE  X.                       CI0365
            10            FG15-FILLER PICTURE  X(100).                  CI0365
            10            FG15-DU18                                     CI0365
                          OCCURS       010     TIMES.                   CI0365
            11            FG15-CT10.                                    CI0365
            12            FG15-CT10K.                                   CI0365
            13            FG15-GR98.                                    CI0365
            14            FG15-GRID.                                    CI0365
            15            FG15-GRIDC  PICTURE  9(3).                    CI0365
            15            FG15-GRIDN.                                   CI0365
            16            FG15-GRIDNP PICTURE  99.                      CI0365
            16            FG15-GRIDND PICTURE  9(8).                    CI0365
            12            FG15-GR97                                     CI0365
                          REDEFINES            FG15-CT10K.              CI0365
            13            FG15-GRIDCB PICTURE  9(3).                    CI0365
            13            FG15-FILLER PICTURE  X(10).                   CI0365
            12            FG15-GERSD  PICTURE  9(8).                    CI0365
            12            FG15-GERED  PICTURE  9(8).                    CI0365
            12            FG15-GRCSI  PICTURE  X.                       CI0365
            11            FG15-GR01.                                    CI0365
            12            FG15-GR01K.                                   CI0365
            13            FG15-GR98.                                    CI0365
            14            FG15-GRID.                                    CI0365
            15            FG15-GRIDC  PICTURE  9(3).                    CI0365
            15            FG15-GRIDN.                                   CI0365
            16            FG15-GRIDNP PICTURE  99.                      CI0365
            16            FG15-GRIDND PICTURE  9(8).                    CI0365
            12            FG15-GECKD  PICTURE  9.                       CI0365
            12            FG15-GEMDA  PICTURE  9(8).                    CI0365
            12            FG15-NSEQ4B PICTURE  9(8)                     CI0365
                          BINARY.                                       CI0365
            12            FG15-GRDOR  PICTURE  9(8).                    CI0365
            12            FG15-GRIAD  PICTURE  9(8).                    CI0365
            12            FG15-GECUC  PICTURE  99.                      CI0365
            12            FG15-GRLNG  PICTURE  99.                      CI0365
            12            FG15-GESLC  PICTURE  99.                      CI0365
            12            FG15-AYSIDA PICTURE  9(3).                    CI0365
            12            FG15-AYSID  PICTURE  9(5).                    CI0365
            12            FG15-GRCSD  PICTURE  9(8).                    CI0365
            12            FG15-GRCFD  PICTURE  9(8).                    CI0365
            12            FG15-GRNCL  PICTURE  S9(5)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            12            FG15-GRNCT  PICTURE  S9(5)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            12            FG15-GRSFC  PICTURE  99.                      CI0365
            12            FG15-GRCRN  PICTURE  9(3).                    CI0365
            12            FG15-GRCSS  PICTURE  X.                       CI0365
            12            FG15-MKSRC  PICTURE  99                       CI0365
                          OCCURS       010     TIMES.                   CI0365
            12            FG15-NEFPS  PICTURE  X(5).                    CI0365
            12            FG15-DEFPS  PICTURE  9(8).                    CI0365
            12            FG15-DLSRV  PICTURE  9(8).                    CI0365
            12            FG15-CTLNI  PICTURE  X.                       CI0365
            12            FG15-CGRLI  PICTURE  X.                       CI0365
            12            FG15-CAMGR  PICTURE  9(5)                     CI0365
                          COMPUTATIONAL-3.                              CI0365
            12            FG15-CAMGS  PICTURE  9(5)                     CI0365
                          COMPUTATIONAL-3.                              CI0365
            12            FG15-CAMGN  PICTURE  9(3)                     CI0365
                          COMPUTATIONAL-3.                              CI0365
            12            FG15-CGRMF  PICTURE  X.                       CI0365
            12            FG15-FILLER PICTURE  X(08).                   CI0365
            11            FG15-GR07.                                    CI0365
            12            FG15-GEDLA  PICTURE  9(8).                    CI0365
            12            FG15-GRAID  PICTURE  X(12).                   CI0365
            12            FG15-GRPAP  PICTURE  X(14).                   CI0365
            12            FG15-GEPHNX PICTURE  9(4).                    CI0365
            12            FG15-DPLEF  PICTURE  9(8).                    CI0365
            12            FG15-DPLAM  PICTURE  9(8).                    CI0365
            12            FG15-NCPFN  PICTURE  9(6).                    CI0365
            12            FG15-GEFYE  PICTURE  9(4).                    CI0365
            12            FG15-FILLER PICTURE  X(06).                   CI0365
            12            FG15-GRPAN  PICTURE  X(45).                   CI0365
            12            FG15-CGRPA  PICTURE  99.                      CI0365
            12            FG15-IPRTT7 PICTURE  X.                       CI0365
            12            FG15-GRPED  PICTURE  9(8).                    CI0365
            12            FG15-FILLER PICTURE  X(05).                   CI0365
            12            FG15-GRPLC  PICTURE  99.                      CI0365
            12            FG15-GRPLT  PICTURE  99.                      CI0365
            12            FG15-FILLER PICTURE  X(04).                   CI0365
            12            FG15-GEADI  PICTURE  X.                       CI0365
            12            FG15-GRCFA  PICTURE  S9(11)V99                CI0365
                          COMPUTATIONAL-3.                              CI0365
            12            FG15-GECFY  PICTURE  9(4).                    CI0365
            12            FG15-GECFC  PICTURE  99.                      CI0365
            12            FG15-MEMPL  PICTURE  X(20).                   CI0365
            12            FG15-CAUNIT PICTURE  X(4).                    CI0365
            12            FG15-FILLER PICTURE  X(21).                   CI0365
            12            FG15-GRPPP  PICTURE  999.                     CI0365
            12            FG15-CCORT  PICTURE  9(3).                    CI0365
            12            FG15-CIDRP  PICTURE  99.                      CI0365
            12            FG15-CCDWA  PICTURE  9.                       CI0365
            12            FG15-IERSA  PICTURE  X.                       CI0365
            12            FG15-DERSA  PICTURE  9(8).                    CI0365
            12            FG15-FILLER PICTURE  X(04).                   CI0365
            10            FG15-QITEM  PICTURE  9(3).                    CI0365
            10            FG15-XIMAX  PICTURE  S9(4)                    CI0365
                          BINARY.                                       CI0365
            10            FG15-FILLER PICTURE  X(100).                  CI0365
       01                 FR00.                                         CI0365
            02            FR01.                                         CI0365
            10            FR01-CT01K.                                   CI0365
            11            FR01-C299.                                    CI0365
            12            FR01-CTID.                                    CI0365
            13            FR01-CTIDA  PICTURE  9(3).                    CI0365
            13            FR01-CTIDN.                                   CI0365
            14            FR01-CTIDNP PICTURE  X(13).                   CI0365
            14            FR01-CTIDND PICTURE  9(11).                   CI0365
            10            FR01-GECKD  PICTURE  9.                       CI0365
            10            FR01-GEMDA  PICTURE  9(8).                    CI0365
            10            FR01-NSEQ4B PICTURE  9(8)                     CI0365
                          BINARY.                                       CI0365
            10            FR01-GECUC  PICTURE  99.                      CI0365
            10            FR01-CTAUL  PICTURE  9(3).                    CI0365
            10            FR01-DIRAC  PICTURE  9(4).                    CI0365
            10            FR01-CTCCI  PICTURE  X.                       CI0365
            10            FR01-CTCUS  PICTURE  999.                     CI0365
            10            FR01-CTEFD  PICTURE  9(8).                    CI0365
            10            FR01-CTIAD  PICTURE  9(8).                    CI0365
            10            FR01-CLCUS  PICTURE  99.                      CI0365
            10            FR01-CAMMB  PICTURE  X(3).                    CI0365
            10            FR01-CKPMM  PICTURE  X.                       CI0365
            10            FR01-CTLAD  PICTURE  9(8).                    CI0365
            10            FR01-IPERS  PICTURE  X.                       CI0365
            10            FR01-AUNCB  PICTURE  S9(7)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            FR01-CTLAT  PICTURE  9(8).                    CI0365
            10            FR01-CTLATC PICTURE  9(6).                    CI0365
            10            FR01-IMEGA  PICTURE  X.                       CI0365
            10            FR01-DIRAB  PICTURE  9(8).                    CI0365
            10            FR01-COLRQ  PICTURE  X.                       CI0365
            10            FR01-ZDA04  PICTURE  X(4).                    CI0365
            10            FR01-CTLPD  PICTURE  9(8).                    CI0365
            10            FR01-CIRASP PICTURE  9.                       CI0365
            10            FR01-CIRATP PICTURE  99.                      CI0365
            10            FR01-DRTHC  PICTURE  9(8).                    CI0365
            10            FR01-CPPTC  PICTURE  X.                       CI0365
            10            FR01-ZDA06  PICTURE  X(6).                    CI0365
            10            FR01-CTACD  PICTURE  9(8).                    CI0365
            10            FR01-CTNLI  PICTURE  X.                       CI0365
            10            FR01-CTRHO  PICTURE  9(8).                    CI0365
            10            FR01-CTSGD  PICTURE  9(8).                    CI0365
            10            FR01-CPATP  PICTURE  X(1).                    CI0365
            10            FR01-IRSTA  PICTURE  X.                       CI0365
            10            FR01-CTSTA  PICTURE  99.                      CI0365
            10            FR01-CTSSC  PICTURE  99.                      CI0365
            10            FR01-PRLIN  PICTURE  9(3).                    CI0365
            10            FR01-PRCOD  PICTURE  9(5).                    CI0365
            10            FR01-PRSCD  PICTURE  X(9).                    CI0365
            10            FR01-CTLNI  PICTURE  X.                       CI0365
            10            FR01-AYSIDA PICTURE  9(3).                    CI0365
            10            FR01-AYSID  PICTURE  9(5).                    CI0365
            10            FR01-CTBMC  PICTURE  99.                      CI0365
            10            FR01-CINAR  PICTURE  99.                      CI0365
            10            FR01-CPHTR  PICTURE  X.                       CI0365
            10            FR01-CDSTR  PICTURE  XX.                      CI0365
            10            FR01-CQACT  PICTURE  999.                     CI0365
            10            FR01-CIRAS  PICTURE  999.                     CI0365
            10            FR01-CIRAT  PICTURE  999.                     CI0365
            10            FR01-CLRAY  PICTURE  9(5).                    CI0365
            10            FR01-CATTP  PICTURE  X.                       CI0365
      *=================================================================AM0273
      **  WORKING STORAGE FOR CI0273.*                                  AM0273
      *=================================================================AM0273
      *!WF DSP=LM DSL=V1 SEL=70 FOR=I LEV=1                             AM0273
       01                 LM00.                                         CI0365
          05              LM00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(01155).                  CI0365
       01                 LM70  REDEFINES      LM00.                    CI0365
            10            LM70-C299.                                    CI0365
            11            LM70-CTID.                                    CI0365
            12            LM70-CTIDA  PICTURE  9(3).                    CI0365
            12            LM70-CTIDN.                                   CI0365
            13            LM70-CTIDNP PICTURE  X(13).                   CI0365
            13            LM70-CTIDND PICTURE  9(11).                   CI0365
            10            LM70-GECKD  PICTURE  9.                       CI0365
            10            LM70-ICUST  PICTURE  X.                       CI0365
            10            LM70-PRCOD  PICTURE  9(5).                    CI0365
            10            LM70-PRSCD  PICTURE  X(9).                    CI0365
            10            LM70-DCACG9 PICTURE  9(8).                    CI0365
            10            LM70-NAASQ  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-CAATY  PICTURE  9(3).                    CI0365
            10            LM70-CACTO  PICTURE  9(3).                    CI0365
            10            LM70-CASTC  PICTURE  99.                      CI0365
            10            LM70-ITRAN  PICTURE  X.                       CI0365
            10            LM70-GEAUN  PICTURE  9(5).                    CI0365
            10            LM70-GEOPD2 PICTURE  X(8).                    CI0365
            10            LM70-DEFFT  PICTURE  9(8).                    CI0365
            10            LM70-CTRTP  PICTURE  X(2).                    CI0365
            10            LM70-CTWHAT PICTURE  S9(7)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-PWHLD  PICTURE  S999V9(5)                CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-GETIM  PICTURE  S9(7)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-ADBRQA PICTURE  S9(9)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-IWTHH  PICTURE  X.                       CI0365
            10            LM70-CLCUS  PICTURE  99.                      CI0365
            10            LM70-CCACT  PICTURE  99.                      CI0365
            10            LM70-AFEET  PICTURE  S9(5)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-ITERF  PICTURE  X.                       CI0365
            10            LM70-ATERF  PICTURE  S9(5)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-CLDOB  PICTURE  9(8).                    CI0365
            10            LM70-CPLTYP PICTURE  X(14).                   CI0365
            10            LM70-IACFPD PICTURE  X(1).                    CI0365
            10            LM70-CDELI  PICTURE  9(3).                    CI0365
            10            LM70-CPAYC  PICTURE  X(2).                    CI0365
            10            LM70-ACOTD  PICTURE  S9(9)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-NPBN   PICTURE  X(20).                   CI0365
            10            LM70-CCBAT  PICTURE  99.                      CI0365
            10            LM70-CLID4.                                   CI0365
            11            LM70-CLIDA  PICTURE  9(3).                    CI0365
            11            LM70-CLIDNP PICTURE  X(12).                   CI0365
            11            LM70-CLIDNA PICTURE  9(8).                    CI0365
            10            LM70-GENAL1 PICTURE  X(30).                   CI0365
            10            LM70-GENAL2 PICTURE  X(30).                   CI0365
            10            LM70-GESAD1 PICTURE  X(30).                   CI0365
            10            LM70-GESAD2 PICTURE  X(30).                   CI0365
            10            LM70-GESAD3 PICTURE  X(30).                   CI0365
            10            LM70-NTR    PICTURE  9(8).                    CI0365
            10            LM70-GECKD1 PICTURE  9.                       CI0365
            10            LM70-IMQMG  PICTURE  X.                       CI0365
            10            LM70-NIPAD  PICTURE  X(15).                   CI0365
            10            LM70-CLNAM.                                   CI0365
            11            LM70-CLNAMH PICTURE  X(6).                    CI0365
            11            LM70-CLNAMF PICTURE  X(20).                   CI0365
            11            LM70-CLNAMI PICTURE  X.                       CI0365
            11            LM70-CLNAMR PICTURE  X(14).                   CI0365
            11            LM70-CLNAML PICTURE  X(25).                   CI0365
            11            LM70-CLNAMS PICTURE  X(4).                    CI0365
            10            LM70-CSLCT  PICTURE  X.                       CI0365
            10            LM70-C199.                                    CI0365
            11            LM70-CLID.                                    CI0365
            12            LM70-CLIDO  PICTURE  9(3).                    CI0365
            12            LM70-CLIDN.                                   CI0365
            13            LM70-CLIDNP PICTURE  X(12).                   CI0365
            13            LM70-CLIDND PICTURE  9(8).                    CI0365
            10            LM70-GECKD2 PICTURE  9.                       CI0365
            10            LM70-CPROCM PICTURE  X.                       CI0365
            10            LM70-NAASQL PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-NSEQ4B PICTURE  9(8)                     CI0365
                          BINARY.                                       CI0365
            10            LM70-CLTIN  PICTURE  9(12).                   CI0365
            10            LM70-IPULL  PICTURE  X.                       CI0365
            10            LM70-NBTCH  PICTURE  9(4).                    CI0365
            10            LM70-CVSYS  PICTURE  X(2).                    CI0365
            10            LM70-NPISQ  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-CPITC  PICTURE  99.                      CI0365
            10            LM70-PPOTD  PICTURE  S9(3)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-ITRNB  PICTURE  X.                       CI0365
            10            LM70-CTTLN1 PICTURE  X(30).                   CI0365
            10            LM70-CTTLN2 PICTURE  X(30).                   CI0365
            10            LM70-CTTLN3 PICTURE  X(30).                   CI0365
            10            LM70-CTTBO1 PICTURE  X(45).                   CI0365
            10            LM70-CTTBO2 PICTURE  X(45).                   CI0365
            10            LM70-PRCMN  PICTURE  X(20).                   CI0365
            10            LM70-TTRTP  PICTURE  X(30).                   CI0365
            10            LM70-DXTMSA PICTURE  X(26).                   CI0365
            10            LM70-DXTMS2 PICTURE  X(26).                   CI0365
            10            LM70-CUPIQ  PICTURE  X.                       CI0365
            10            LM70-IQACT  PICTURE  X.                       CI0365
            10            LM70-MAPPN  PICTURE  X(10).                   CI0365
            10            LM70-CTTYPG PICTURE  X(04).                   CI0365
            10            LM70-CLORN  PICTURE  X(45).                   CI0365
            10            LM70-APMTL  PICTURE  S9(9)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-MPMTFL PICTURE  X(24).                   CI0365
            10            LM70-ANETTQ PICTURE  S9(9)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-TTBAL  PICTURE  X(15).                   CI0365
            10            LM70-MPRN4  PICTURE  X(35).                   CI0365
            10            LM70-CCONF  PICTURE  X(25).                   CI0365
            10            LM70-DCACG  PICTURE  9(8).                    CI0365
            10            LM70-NMESA  PICTURE  9(6).                    CI0365
            10            LM70-MCSIG  PICTURE  X(30).                   CI0365
            10            LM70-IERRC  PICTURE  X.                       CI0365
            10            LM70-AVLMN  PICTURE  S9(7)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-AVLMX  PICTURE  S9(7)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-AVCSH  PICTURE  S9(11)V99                CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-ACVALM PICTURE  S9(11)V99                CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-INDRS  PICTURE  X.                       CI0365
            10            LM70-GRID   PICTURE  X(13).                   CI0365
            10            LM70-AACTV  PICTURE  S9(11)V99                CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-CTCUS  PICTURE  999.                     CI0365
            10            LM70-CCDSCW PICTURE  9(2).                    CI0365
            10            LM70-CHCR   PICTURE  99.                      CI0365
            10            LM70-IOWNG  PICTURE  X.                       CI0365
            10            LM70-GECSQ  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LM70-CQACT  PICTURE  999.                     CI0365
            10            LM70-CTOWN  PICTURE  9(3).                    CI0365
            10            LM70-NGEOR  PICTURE  9(08).                   CI0365
            10            LM70-FILLER PICTURE  X(85).                   CI0365
      *                                                                 AM0273
      ******************************************************************AM0273
      ** PCB ADDRESS LIST FOR CI0273.  MODULE CI0273 WILL NEED         *AM0273
      ** PCB'S FOR:                                                    *AM0273
      **             CONTRACT DATABASE   (CT1P)                        *AM0273
      **             CLIENT DATABASE     (CL1P)                        *AM0273
      **             ACTIVITY DATABASE   (ACAP)                        *AM0273
      **             CERT HISTORY DB     (CH1P)                        *AM0273
      **             CERT ACCRUAL DB     (CCRP)                        *AM0273
      **             INTEREST DATABASE   (CPRP)                        *AM0273
      **             PRODUCT TABLE DB    (CBTP)                        *AM0273
      **             CERT CONTRACT DB    (CA1P)                        *AM0273
      ******************************************************************AM0273
       01  CI0273-LK-PCB-ADDR-LIST.                                     AM0273
           05  CI0273-LK-PCB-CT1P-PTR1        POINTER.                  AM0273
           05  CI0273-LK-PCB-CL1P-PTR1        POINTER.                  AM0273
           05  CI0273-LK-PCB-ACAP-PTR1        POINTER.                  AM0273
           05  CI0273-LK-PCB-CH1P-PTR1        POINTER.                  AM0273
           05  CI0273-LK-PCB-CCRP-PTR1        POINTER.                  AM0273
           05  CI0273-LK-PCB-CPRP-PTR1        POINTER.                  AM0273
           05  CI0273-LK-PCB-CBTP-PTR1        POINTER.                  AM0273
           05  CI0273-LK-PCB-CA1P-PTR1        POINTER.                  AM0273
      *GENERATE INDEX FOR MESSAGE TEXT (ONLY IF 'OCCURS' SPECIFIED)     ADU071
      *                   MS03                                          ADU071
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
       01               7-XX01.                                         $AACTG
         05             7-XX01-DATMOD             PIC X(8)              $AACTG
                                                  VALUE 'MWS100EX'.     $AACTG
         05             7-XX01-IDTFLD.                                  $AACTG
           10           7-XX01-ICURR              PIC X(04)             $AACTG
                                                  VALUE LOW-VALUES.     $AACTG
           10           7-XX01-ICODES             PIC X(04)             $AACTG
                                                  VALUE LOW-VALUES.     $AACTG
           10           7-XX01-INOOPT             PIC S9(01) COMP-3     $AACTG
                                                  VALUE +0.             $AACTG
           10           7-XX01-IOPCON             PIC X(03)             $AACTG
                                                  VALUE 'IDS'.          $AACTG
         05             7-XX01-RDTFLD.                                  $AACTG
           10           7-XX01-RCDATE             PIC X(04)             $AACTG
                                                  VALUE LOW-VALUES.     $AACTG
           10           7-XX01-RCJUL              PIC S9(7) COMP-3      $AACTG
                                                  VALUE +0.             $AACTG
           10           7-XX01-TCCODE             PIC X(04)             $AACTG
                                                  VALUE LOW-VALUES.     $AACTG
           10           7-XX01-TCALPH             PIC X(12)             $AACTG
                                                  VALUE SPACES.         $AACTG
           10           7-XX01-RNDATE             PIC X(04)             $AACTG
                                                  VALUE LOW-VALUES.     $AACTG
           10           7-XX01-RNJUL              PIC S9(07) COMP-3     $AACTG
                                                  VALUE +0.             $AACTG
           10           7-XX01-RNCODE             PIC X(04)             $AACTG
                                                  VALUE LOW-VALUES.     $AACTG
           10           7-XX01-RNALPH             PIC X(12)             $AACTG
                                                  VALUE SPACES.         $AACTG
         05             7-XX01-PCKDAT             PIC S9(09) COMP-3     $AACTG
                                                  VALUE +0.             $AACTG
         05             7-XX01-PUDAT              PIC S9(09) COMP-3     $AACTG
                                                  VALUE +0.             $AACTG
         05             7-XX01-PUSDAT REDEFINES 7-XX01-PUDAT.           $AACTG
           10           7-XX01-UNSDAT             PIC X(04).            $AACTG
           10           FILLER                    PIC S9(01) COMP-3.    $AACTG
         05             7-XX01-CHKPDT             PIC S9(09) COMP-3     $AACTG
                                                  VALUE +0.             $AACTG
         05             7-XX01-USEDAT REDEFINES 7-XX01-CHKPDT.          $AACTG
           10           7-XX01-CHKDAT             PIC X(04).            $AACTG
           10           FILLER                    PIC X.                $AACTG
         05             7-XX01-COMDAT.                                  $AACTG
           10           7-XX01-NEXTDT             PIC S9(08)            $AACTG
                                                  VALUE +0.             $AACTG
           10           FILLER                    PIC X.                $AACTG
      *                                                                 AM0020
      ******************************************************************AM0020
      **     SEGMENT THAT CONTAINS THE CAMS ACCOUNTING DATES           *AM0020
      ******************************************************************AM0020
      *                                                                 AM0020
      *!WF DSP=NS DSL=NS SEL=20 FOR=I LEV=1                             AM0020
       01                 NS00.                                         CI0365
          05              NS00-00.                                      CI0365
            10            NS00-NS00K.                                   CI0365
            11            NS00-PRCSTK PICTURE  XX.                      CI0365
          05              NS00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00078).                  CI0365
       01                 NS20  REDEFINES      NS00.                    CI0365
            10       FILLER         PICTURE  X(00002).                  CI0365
            10            NS20-DCACG  PICTURE  9(8).                    CI0365
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            NS20-CCDAT  PICTURE  X(8).                    CI0365
            10            NS20-DCALP  PICTURE  X(12).                   CI0365
            10            NS20-DNACG  PICTURE  9(8).                    CI0365
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            NS20-CNDAT  PICTURE  X(8).                    CI0365
            10            NS20-DNALP  PICTURE  X(12).                   CI0365
            10            NS20-DCACD  PICTURE  X(10).                   CI0365
            10            NS20-FILLER PICTURE  X(4).                    CI0365
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
                                                                        AM0003
      ******************************************************************AM0003
      **     PCB ADDRESS LIST FOR CI0003.  MODULE CI0003 WILL NEED     *AM0003
      **     PCB'S FOR:                                                *AM0003
      **                CONTRACT DATABASE(CT1P)                        *AM0003
      ******************************************************************AM0003
                                                                        AM0003
       01  CI0003B-PCB-ADDRESS-LIST.                                    AM0003
           05  CI0003B-PCB-CT1P-PTR1      POINTER.                      AM0003
                                                                        AM0018
      ******************************************************************AM0018
      **     PCB ADDRESS LIST FOR CI0018.  MODULE CI0018 WILL NEED     *AM0018
      **     PCB'S FOR:                                                *AM0018
      **                CONTRACT DATABASE(CT1P)                        *AM0018
      ******************************************************************AM0018
                                                                        AM0018
       01  CI0018C-PCB-ADDRESS-LIST.                                    AM0018
           05  CI0018C-PCB-CT1P-PTR1      POINTER.                      AM0018
                                                                        AM0018
      ******************************************************************AM0018
      **     PCB ADDRESS LIST FOR CI0018.  MODULE CI0018 WILL NEED     *AM0018
      **     PCB'S FOR:                                                *AM0018
      **                CONTRACT DATABASE(CT1P)                        *AM0018
      ******************************************************************AM0018
                                                                        AM0018
       01  CI0018D-PCB-ADDRESS-LIST.                                    AM0018
           05  CI0018D-PCB-CT1P-PTR1      POINTER.                      AM0018
                                                                        AM0019
      ******************************************************************AM0019
      **     PCB ADDRESS LIST FOR CI0019.  MODULE CI0019 WILL NEED     *AM0019
      **     PCB'S FOR:                                                *AM0019
      **                CONTRACT DATABASE(CT1P)                        *AM0019
      **                GROUP DATABASE(GR1P)                           *AM0019
      ******************************************************************AM0019
                                                                        AM0019
       01  CI0019E-PCB-ADDRESS-LIST.                                    AM0019
           05  CI0019E-PCB-CT1P-PTR1      POINTER.                      AM0019
           05  CI0019E-PCB-GR1P-PTR1      POINTER.                      AM0019
                                                                        AM0019
      ******************************************************************AM0019
      **     PCB ADDRESS LIST FOR CI0019.  MODULE CI0019 WILL NEED     *AM0019
      **     PCB'S FOR:                                                *AM0019
      **                CONTRACT DATABASE(CT1P)                        *AM0019
      **                GROUP DATABASE(GR1P)                           *AM0019
      ******************************************************************AM0019
                                                                        AM0019
       01  CI0019F-PCB-ADDRESS-LIST.                                    AM0019
           05  CI0019F-PCB-CT1P-PTR1      POINTER.                      AM0019
           05  CI0019F-PCB-GR1P-PTR1      POINTER.                      AM0019

      ******************************************************************
      **     SWITCHES USED THOUGHOUT THIS PGM.                         *
      ******************************************************************

       01  WS-SWITCHES.
           05  WS-PROCESS-TRANS               PIC X  VALUE SPACE.
               88 EDIT-REQUEST                       VALUE 'E'.
               88 UPDATE-REQUEST                     VALUE 'U'.
           05  WS-VALID-AMOUNTS               PIC X  VALUE 'N'.
               88 VALID-AMOUNT                       VALUE 'Y'.
               88 NOT-VALID-AMOUNT                   VALUE 'N'.
           05  WS-ACTION-TRANS                PIC X  VALUE 'A'.
               88 ADD-REQUEST                        VALUE 'A'.
               88 DEL-REQUEST                        VALUE 'M'.
           05  WS-CORE-HOURS                   PIC X VALUE 'Y'.
               88 CORE-TRANS                         VALUE 'Y'.
               88 OFF-TRANS                          VALUE 'N'.

      *PASS AREA TO/FROM CI0003 (OWNERSHIP LINES) - DEST ACCT
      *!WF DSP=TA DSL=DU SEL=04 FOR=I DES=1 LEV=1 PLT=TA
       01                 TA04.                                         CI0365
            10            TA04-C299.                                    CI0365
            11            TA04-CTID.                                    CI0365
            12            TA04-CTIDA  PICTURE  9(3).                    CI0365
            12            TA04-CTIDN.                                   CI0365
            13            TA04-CTIDNP PICTURE  X(13).                   CI0365
            13            TA04-CTIDND PICTURE  9(11).                   CI0365
            10            TA04-IPOCH  PICTURE  X.                       CI0365
            10            TA04-FILLER PICTURE  X(099).                  CI0365
            10            TA04-CTTLN1 PICTURE  X(30).                   CI0365
            10            TA04-CTTLN2 PICTURE  X(30).                   CI0365
            10            TA04-CTTLN3 PICTURE  X(30).                   CI0365
            10            TA04-CTTBO1 PICTURE  X(45).                   CI0365
            10            TA04-CTTBO2 PICTURE  X(45).                   CI0365
            10            TA04-CTOWN  PICTURE  9(3).                    CI0365
            10            TA04-IUGMA  PICTURE  X.                       CI0365
            10            TA04-FILLER PICTURE  X(096).                  CI0365

      *PASS AREA TO/FROM CI0018 (CLIENTS) - DEST ACCT
      *!WF DSP=TC DSL=DU SEL=14 FOR=I DES=1 LEV=1 PLT=TC
       01                 TC14.                                         CI0365
            10            TC14-C299.                                    CI0365
            11            TC14-CTID.                                    CI0365
            12            TC14-CTIDA  PICTURE  9(3).                    CI0365
            12            TC14-CTIDN.                                   CI0365
            13            TC14-CTIDNP PICTURE  X(13).                   CI0365
            13            TC14-CTIDND PICTURE  9(11).                   CI0365
            10            TC14-DCACG  PICTURE  9(8).                    CI0365
            10            TC14-IPOCH  PICTURE  X.                       CI0365
            10            TC14-FILLER PICTURE  X(100).                  CI0365
            10            TC14-CLID01.                                  CI0365
            11            TC14-CLIDO1 PICTURE  X(3).                    CI0365
            11            TC14-NCLID1.                                  CI0365
            12            TC14-CLIDP1 PICTURE  X(12).                   CI0365
            12            TC14-CLIDNA PICTURE  9(8).                    CI0365
            10            TC14-CLCTR  PICTURE  9(3).                    CI0365
            10            TC14-DU21                                     CI0365
                          OCCURS       025     TIMES.                   CI0365
            11            TC14-C199.                                    CI0365
            12            TC14-CLID.                                    CI0365
            13            TC14-CLIDO  PICTURE  9(3).                    CI0365
            13            TC14-CLIDN.                                   CI0365
            14            TC14-CLIDNP PICTURE  X(12).                   CI0365
            14            TC14-CLIDND PICTURE  9(8).                    CI0365
            11            TC14-CLCTRC PICTURE  9(3).                    CI0365
            10            TC14-QITEM  PICTURE  9(3).                    CI0365
            10            TC14-XIMAX  PICTURE  S9(4)                    CI0365
                          BINARY.                                       CI0365
            10            TC14-CRROL  PICTURE  X.                       CI0365
            10            TC14-FILLER PICTURE  X(099).                  CI0365
       01                 TD16.                                         CI0365
            10            TD16-C299.                                    CI0365
            11            TD16-CTID.                                    CI0365
            12            TD16-CTIDA  PICTURE  9(3).                    CI0365
            12            TD16-CTIDN.                                   CI0365
            13            TD16-CTIDNP PICTURE  X(13).                   CI0365
            13            TD16-CTIDND PICTURE  9(11).                   CI0365
            10            TD16-CTID01.                                  CI0365
            11            TD16-CTIDA1 PICTURE  9(3).                    CI0365
            11            TD16-CTIDN1.                                  CI0365
            12            TD16-CTIDP1 PICTURE  X(13).                   CI0365
            12            TD16-CTIDNA PICTURE  9(11).                   CI0365
            10            TD16-DCACG  PICTURE  9(8).                    CI0365
            10            TD16-IOWNC  PICTURE  X.                       CI0365
            10            TD16-IPOCH  PICTURE  X.                       CI0365
            10            TD16-FILLER PICTURE  X(100).                  CI0365
            10            TD16-CTYPE  PICTURE  X.                       CI0365
            10            TD16-FILLER PICTURE  X(100).                  CI0365

      *PASS AREA TO/FROM CI0019 (GROUPS) - DEST ACCT
      *!WF DSP=TG DSL=DU SEL=15 FOR=I LEV=1 PLT=TG
       01                 TG00.                                         CI0365
          05              TG00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(04181).                  CI0365
       01                 TG15  REDEFINES      TG00.                    CI0365
            10            TG15-C299.                                    CI0365
            11            TG15-CTID.                                    CI0365
            12            TG15-CTIDA  PICTURE  9(3).                    CI0365
            12            TG15-CTIDN.                                   CI0365
            13            TG15-CTIDNP PICTURE  X(13).                   CI0365
            13            TG15-CTIDND PICTURE  9(11).                   CI0365
            10            TG15-DCACG  PICTURE  9(8).                    CI0365
            10            TG15-IPOCH  PICTURE  X.                       CI0365
            10            TG15-FILLER PICTURE  X(100).                  CI0365
            10            TG15-DU18                                     CI0365
                          OCCURS       010     TIMES.                   CI0365
            11            TG15-CT10.                                    CI0365
            12            TG15-CT10K.                                   CI0365
            13            TG15-GR98.                                    CI0365
            14            TG15-GRID.                                    CI0365
            15            TG15-GRIDC  PICTURE  9(3).                    CI0365
            15            TG15-GRIDN.                                   CI0365
            16            TG15-GRIDNP PICTURE  99.                      CI0365
            16            TG15-GRIDND PICTURE  9(8).                    CI0365
            12            TG15-GR97                                     CI0365
                          REDEFINES            TG15-CT10K.              CI0365
            13            TG15-GRIDCB PICTURE  9(3).                    CI0365
            13            TG15-FILLER PICTURE  X(10).                   CI0365
            12            TG15-GERSD  PICTURE  9(8).                    CI0365
            12            TG15-GERED  PICTURE  9(8).                    CI0365
            12            TG15-GRCSI  PICTURE  X.                       CI0365
            11            TG15-GR01.                                    CI0365
            12            TG15-GR01K.                                   CI0365
            13            TG15-GR98.                                    CI0365
            14            TG15-GRID.                                    CI0365
            15            TG15-GRIDC  PICTURE  9(3).                    CI0365
            15            TG15-GRIDN.                                   CI0365
            16            TG15-GRIDNP PICTURE  99.                      CI0365
            16            TG15-GRIDND PICTURE  9(8).                    CI0365
            12            TG15-GECKD  PICTURE  9.                       CI0365
            12            TG15-GEMDA  PICTURE  9(8).                    CI0365
            12            TG15-NSEQ4B PICTURE  9(8)                     CI0365
                          BINARY.                                       CI0365
            12            TG15-GRDOR  PICTURE  9(8).                    CI0365
            12            TG15-GRIAD  PICTURE  9(8).                    CI0365
            12            TG15-GECUC  PICTURE  99.                      CI0365
            12            TG15-GRLNG  PICTURE  99.                      CI0365
            12            TG15-GESLC  PICTURE  99.                      CI0365
            12            TG15-AYSIDA PICTURE  9(3).                    CI0365
            12            TG15-AYSID  PICTURE  9(5).                    CI0365
            12            TG15-GRCSD  PICTURE  9(8).                    CI0365
            12            TG15-GRCFD  PICTURE  9(8).                    CI0365
            12            TG15-GRNCL  PICTURE  S9(5)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            12            TG15-GRNCT  PICTURE  S9(5)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            12            TG15-GRSFC  PICTURE  99.                      CI0365
            12            TG15-GRCRN  PICTURE  9(3).                    CI0365
            12            TG15-GRCSS  PICTURE  X.                       CI0365
            12            TG15-MKSRC  PICTURE  99                       CI0365
                          OCCURS       010     TIMES.                   CI0365
            12            TG15-NEFPS  PICTURE  X(5).                    CI0365
            12            TG15-DEFPS  PICTURE  9(8).                    CI0365
            12            TG15-DLSRV  PICTURE  9(8).                    CI0365
            12            TG15-CTLNI  PICTURE  X.                       CI0365
            12            TG15-CGRLI  PICTURE  X.                       CI0365
            12            TG15-CAMGR  PICTURE  9(5)                     CI0365
                          COMPUTATIONAL-3.                              CI0365
            12            TG15-CAMGS  PICTURE  9(5)                     CI0365
                          COMPUTATIONAL-3.                              CI0365
            12            TG15-CAMGN  PICTURE  9(3)                     CI0365
                          COMPUTATIONAL-3.                              CI0365
            12            TG15-CGRMF  PICTURE  X.                       CI0365
            12            TG15-FILLER PICTURE  X(08).                   CI0365
            11            TG15-GR07.                                    CI0365
            12            TG15-GEDLA  PICTURE  9(8).                    CI0365
            12            TG15-GRAID  PICTURE  X(12).                   CI0365
            12            TG15-GRPAP  PICTURE  X(14).                   CI0365
            12            TG15-GEPHNX PICTURE  9(4).                    CI0365
            12            TG15-DPLEF  PICTURE  9(8).                    CI0365
            12            TG15-DPLAM  PICTURE  9(8).                    CI0365
            12            TG15-NCPFN  PICTURE  9(6).                    CI0365
            12            TG15-GEFYE  PICTURE  9(4).                    CI0365
            12            TG15-FILLER PICTURE  X(06).                   CI0365
            12            TG15-GRPAN  PICTURE  X(45).                   CI0365
            12            TG15-CGRPA  PICTURE  99.                      CI0365
            12            TG15-IPRTT7 PICTURE  X.                       CI0365
            12            TG15-GRPED  PICTURE  9(8).                    CI0365
            12            TG15-FILLER PICTURE  X(05).                   CI0365
            12            TG15-GRPLC  PICTURE  99.                      CI0365
            12            TG15-GRPLT  PICTURE  99.                      CI0365
            12            TG15-FILLER PICTURE  X(04).                   CI0365
            12            TG15-GEADI  PICTURE  X.                       CI0365
            12            TG15-GRCFA  PICTURE  S9(11)V99                CI0365
                          COMPUTATIONAL-3.                              CI0365
            12            TG15-GECFY  PICTURE  9(4).                    CI0365
            12            TG15-GECFC  PICTURE  99.                      CI0365
            12            TG15-MEMPL  PICTURE  X(20).                   CI0365
            12            TG15-CAUNIT PICTURE  X(4).                    CI0365
            12            TG15-FILLER PICTURE  X(21).                   CI0365
            12            TG15-GRPPP  PICTURE  999.                     CI0365
            12            TG15-CCORT  PICTURE  9(3).                    CI0365
            12            TG15-CIDRP  PICTURE  99.                      CI0365
            12            TG15-CCDWA  PICTURE  9.                       CI0365
            12            TG15-IERSA  PICTURE  X.                       CI0365
            12            TG15-DERSA  PICTURE  9(8).                    CI0365
            12            TG15-FILLER PICTURE  X(04).                   CI0365
            10            TG15-QITEM  PICTURE  9(3).                    CI0365
            10            TG15-XIMAX  PICTURE  S9(4)                    CI0365
                          BINARY.                                       CI0365
            10            TG15-FILLER PICTURE  X(100).                  CI0365
       01                 TO01.                                         CI0365
            10            TO01-CT01K.                                   CI0365
            11            TO01-C299.                                    CI0365
            12            TO01-CTID.                                    CI0365
            13            TO01-CTIDA  PICTURE  9(3).                    CI0365
            13            TO01-CTIDN.                                   CI0365
            14            TO01-CTIDNP PICTURE  X(13).                   CI0365
            14            TO01-CTIDND PICTURE  9(11).                   CI0365
            10            TO01-GECKD  PICTURE  9.                       CI0365
            10            TO01-GEMDA  PICTURE  9(8).                    CI0365
            10            TO01-NSEQ4B PICTURE  9(8)                     CI0365
                          BINARY.                                       CI0365
            10            TO01-GECUC  PICTURE  99.                      CI0365
            10            TO01-CTAUL  PICTURE  9(3).                    CI0365
            10            TO01-DIRAC  PICTURE  9(4).                    CI0365
            10            TO01-CTCCI  PICTURE  X.                       CI0365
            10            TO01-CTCUS  PICTURE  999.                     CI0365
            10            TO01-CTEFD  PICTURE  9(8).                    CI0365
            10            TO01-CTIAD  PICTURE  9(8).                    CI0365
            10            TO01-CLCUS  PICTURE  99.                      CI0365
            10            TO01-CAMMB  PICTURE  X(3).                    CI0365
            10            TO01-CKPMM  PICTURE  X.                       CI0365
            10            TO01-CTLAD  PICTURE  9(8).                    CI0365
            10            TO01-IPERS  PICTURE  X.                       CI0365
            10            TO01-AUNCB  PICTURE  S9(7)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            TO01-CTLAT  PICTURE  9(8).                    CI0365
            10            TO01-CTLATC PICTURE  9(6).                    CI0365
            10            TO01-IMEGA  PICTURE  X.                       CI0365
            10            TO01-DIRAB  PICTURE  9(8).                    CI0365
            10            TO01-COLRQ  PICTURE  X.                       CI0365
            10            TO01-ZDA04  PICTURE  X(4).                    CI0365
            10            TO01-CTLPD  PICTURE  9(8).                    CI0365
            10            TO01-CIRASP PICTURE  9.                       CI0365
            10            TO01-CIRATP PICTURE  99.                      CI0365
            10            TO01-DRTHC  PICTURE  9(8).                    CI0365
            10            TO01-CPPTC  PICTURE  X.                       CI0365
            10            TO01-ZDA06  PICTURE  X(6).                    CI0365
            10            TO01-CTACD  PICTURE  9(8).                    CI0365
            10            TO01-CTNLI  PICTURE  X.                       CI0365
            10            TO01-CTRHO  PICTURE  9(8).                    CI0365
            10            TO01-CTSGD  PICTURE  9(8).                    CI0365
            10            TO01-CPATP  PICTURE  X(1).                    CI0365
            10            TO01-IRSTA  PICTURE  X.                       CI0365
            10            TO01-CTSTA  PICTURE  99.                      CI0365
            10            TO01-CTSSC  PICTURE  99.                      CI0365
            10            TO01-PRLIN  PICTURE  9(3).                    CI0365
            10            TO01-PRCOD  PICTURE  9(5).                    CI0365
            10            TO01-PRSCD  PICTURE  X(9).                    CI0365
            10            TO01-CTLNI  PICTURE  X.                       CI0365
            10            TO01-AYSIDA PICTURE  9(3).                    CI0365
            10            TO01-AYSID  PICTURE  9(5).                    CI0365
            10            TO01-CTBMC  PICTURE  99.                      CI0365
            10            TO01-CINAR  PICTURE  99.                      CI0365
            10            TO01-CPHTR  PICTURE  X.                       CI0365
            10            TO01-CDSTR  PICTURE  XX.                      CI0365
            10            TO01-CQACT  PICTURE  999.                     CI0365
            10            TO01-CIRAS  PICTURE  999.                     CI0365
            10            TO01-CIRAT  PICTURE  999.                     CI0365
            10            TO01-CLRAY  PICTURE  9(5).                    CI0365
            10            TO01-CATTP  PICTURE  X.                       CI0365
      ******************************************************************AM0074
      **   THIS AREA IS USED BY THE TAX SETTLEMENT CODE MODULE, CI0074 *AM0074
      **   AND CONTAINS STORAGE FOR A STAGING AREA, THE SEGMENT THAT   *AM0074
      **   CONTAINS I/O PARAMETERS, AND A PCB ADDRESS LIST.            *AM0074
      ******************************************************************AM0074
                                                                        AM0074
       01  TS00-STAGING-AREA.                                           AM0074
      *!WI pl=TS110                                                     AM0074
           05  TS00-CTID                                                AM0074
                        PICTURE X(27)                                   CI0365
                               VALUE ZEROS.                             AM0074
      *!WI pl=TS120                                                     AM0074
           05  TS00-CLID                                                AM0074
                        PICTURE X(23)                                   CI0365
                               VALUE ZEROS.                             AM0074
      *!WI pl=TS130                                                     AM0074
           05  TS00-CLCUS                                               AM0074
                        PICTURE 99                                      CI0365
                               VALUE ZEROS.                             AM0074
      *!WI pl=TS140                                                     AM0074
           05  TS00-DCACG                                               AM0074
                        PICTURE 9(8)                                    CI0365
                               VALUE ZEROS.                             AM0074
      *!WI pl=TS150                                                     AM0074
           05  TS00-CAUNIT                                              AM0074
                        PICTURE X(4)                                    CI0365
                               VALUE SPACES.                            AM0074
      *!WI pl=TS160                                                     AM0074
           05  TS00-GEOPID                                              AM0074
                        PICTURE X(6)                                    CI0365
                               VALUE SPACES.                            AM0074
      *!WI pl=TS170                                                     AM0074
           05  TS00-MAPPN                                               AM0074
                        PICTURE X(10)                                   CI0365
                               VALUE SPACES.                            AM0074
      *!WI pl=TS180                                                     AM0074
           05  TS00-CUPIQ                                               AM0074
                        PICTURE X                                       CI0365
                               VALUE SPACES.                            AM0074
      *!WI pl=TS190                                                     AM0074
           05  TS00-CTID01                                              AM0074
                        PICTURE X(27)                                   CI0365
                               VALUE SPACES.                            AM0074
      *!WI pl=TS200                                                     AM0074
           05  TS00-CTYPE                                               AM0074
                        PICTURE X                                       CI0365
                               VALUE SPACES.                            AM0074
      *!WI pl=TS210                                                     AM0074
           05  TS00-CPAYF                                               AM0074
                        PICTURE X(2)                                    CI0365
                               VALUE SPACES.                            AM0074
                                                                        AM0074
                                                                        AM0074
                                                                        AM0074
      *    THIS SEGMENT CONTAINS THE I/O PARAMETERS FOR CI0074          AM0074
                                                                        AM0074
      *!WF DSP=TS DSL=PJ SEL=15 FOR=I DES=1 LEV=1                       AM0074
       01                 TS15.                                         CI0365
            10            TS15-CT99.                                    CI0365
            11            TS15-CT99K.                                   CI0365
            12            TS15-C299.                                    CI0365
            13            TS15-CTID.                                    CI0365
            14            TS15-CTIDA  PICTURE  9(3).                    CI0365
            14            TS15-CTIDN.                                   CI0365
            15            TS15-CTIDNP PICTURE  X(13).                   CI0365
            15            TS15-CTIDND PICTURE  9(11).                   CI0365
            10            TS15-CLID   PICTURE  X(23).                   CI0365
            10            TS15-CLCUS  PICTURE  99.                      CI0365
            10            TS15-DCACG  PICTURE  9(8).                    CI0365
            10            TS15-CAUNIT PICTURE  X(4).                    CI0365
            10            TS15-GEOPID PICTURE  X(6).                    CI0365
            10            TS15-CLCUSA PICTURE  XXX.                     CI0365
            10            TS15-MAPPN  PICTURE  X(10).                   CI0365
            10            TS15-CUPIQ  PICTURE  X.                       CI0365
            10            TS15-CTID01 PICTURE  X(27).                   CI0365
            10            TS15-CTYPE  PICTURE  X.                       CI0365
            10            TS15-CPAYF  PICTURE  X(2).                    CI0365
            10            TS15-FILLER PICTURE  X(84).                   CI0365
                                                                        AM0074
                                                                        AM0074
      *-----> PCB ADDRESS LIST FOR CALLING MODULE CI0074                AM0074
      *                                                                 AM0074
       01               CI0074-PCB-ADDRESS-LIST.                        AM0074
            05          CI0074-PCB-CT1P-PTR1        POINTER.            AM0074
            05          CI0074-PCB-CL1P-PTR1        POINTER.            AM0074
            05          CI0074-PCB-GR1P-PTR1        POINTER.            AM0074
            05          CI0074-PCB-TR1P-PTR1        POINTER.            AM0074
      *****************************************************************
      *                  GENERAL WORKING STORAGE FIELDS
      *****************************************************************
      *
       01  W217-GENERAL-FIELDS.
      *!WI
           05  W217-CDEL1       VALUE 003
                        PICTURE 9(3).                                   CI0365
      *!WI
           05  W217-CHCR        VALUE 03
                        PICTURE 99.                                     CI0365
      *!WI
           05  W217-IOWNG       VALUE 'Y'
                        PICTURE X.                                      CI0365
      *!WI
           05  W217-MCSIG
                        PICTURE X(30).                                  CI0365
      **************************************************************
      *LINKAGE SEGMENT FOR CI0105
      **************************************************************
      *!WF DSP=WM DSL=WM SEL=7172 FOR=I DES=1 LEV=1
      * PLT=WM
       01                 WM71.                                         CI0365
            10            WM71-MAPPN  PICTURE  X(10).                   CI0365
            10            WM71-CLID   PICTURE  X(23).                   CI0365
            10            WM71-C299.                                    CI0365
            11            WM71-CTID.                                    CI0365
            12            WM71-CTIDA  PICTURE  9(3).                    CI0365
            12            WM71-CTIDN.                                   CI0365
            13            WM71-CTIDNP PICTURE  X(13).                   CI0365
            13            WM71-CTIDND PICTURE  9(11).                   CI0365
            10            WM71-GECSQ  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            WM71-CHCR   PICTURE  99.                      CI0365
            10            WM71-IOWNG  PICTURE  X.                       CI0365
            10            WM71-NPBN   PICTURE  X(20).                   CI0365
            10            WM71-NTR    PICTURE  9(8).                    CI0365
            10            WM71-CCBAT  PICTURE  99.                      CI0365
            10            WM71-CLID4  PICTURE  X(23).                   CI0365
            10            WM71-MCSIG  PICTURE  X(30).                   CI0365
            10            WM71-CDEL1  PICTURE  9(3).                    CI0365
            10            WM71-DCACG  PICTURE  9(8).                    CI0365
            10            WM71-GEOPD2 PICTURE  X(8).                    CI0365
            10            WM71-CAUNIT PICTURE  X(4).                    CI0365
            10            WM71-FILLER PICTURE  X(50).                   CI0365
       01                 WM72.                                         CI0365
            10            WM72-ICX01  PICTURE  X.                       CI0365
            10            WM72-ICX03  PICTURE  X.                       CI0365
            10            WM72-ICX06  PICTURE  X.                       CI0365
            10            WM72-ICX09  PICTURE  X.                       CI0365
            10            WM72-ICX18  PICTURE  X.                       CI0365
            10            WM72-ICX21  PICTURE  X.                       CI0365
            10            WM72-FILLER PICTURE  X(50).                   CI0365
      *!WF DSP=LX DSL=CX SEL=01030609 FOR=I DES=1 LEV=1
      * PLT=WM
       01                 LX01.                                         CI0365
            10            LX01-CX01K.                                   CI0365
            11            LX01-C199.                                    CI0365
            12            LX01-CLID.                                    CI0365
            13            LX01-CLIDO  PICTURE  9(3).                    CI0365
            13            LX01-CLIDN.                                   CI0365
            14            LX01-CLIDNP PICTURE  X(12).                   CI0365
            14            LX01-CLIDND PICTURE  9(8).                    CI0365
            10            LX01-GEMDA  PICTURE  9(8).                    CI0365
            10            LX01-NSEQ4B PICTURE  9(8)                     CI0365
                          BINARY.                                       CI0365
            10            LX01-FILLER PICTURE  X(5).                    CI0365
       01                 LX03.                                         CI0365
            10            LX03-GELL   PICTURE  9(4)                     CI0365
                          BINARY.                                       CI0365
            10            LX03-CY00.                                    CI0365
            11            LX03-CX03K.                                   CI0365
            12            LX03-CARTY  PICTURE  99.                      CI0365
            12            LX03-NARRS  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            11            LX03-CARST  PICTURE  99.                      CI0365
            11            LX03-GECSQ  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            11            LX03-CPMTG  PICTURE  99.                      CI0365
            11            LX03-GRCRNG PICTURE  9(3).                    CI0365
            11            LX03-DEXDT  PICTURE  9(8).                    CI0365
            11            LX03-DASUP  PICTURE  9(8).                    CI0365
            11            LX03-CSTEC  PICTURE  X(3).                    CI0365
            11            LX03-FILLER PICTURE  X(17).                   CI0365
            11            LX03-CY50.                                    CI0365
            12            LX03-NARID  PICTURE  X(30).                   CI0365
            11            LX03-CY51                                     CI0365
                          REDEFINES            LX03-CY50.               CI0365
            12            LX03-NDIDN  PICTURE  9(12).                   CI0365
            12            LX03-FILLER PICTURE  X(18).                   CI0365
            11            LX03-CY52                                     CI0365
                          REDEFINES            LX03-CY50.               CI0365
            12            LX03-NAIDC  PICTURE  9(12).                   CI0365
            12            LX03-FILLER PICTURE  X(18).                   CI0365
            11            LX03-CY53                                     CI0365
                          REDEFINES            LX03-CY50.               CI0365
            12            LX03-NAMEXB PICTURE  9(15).                   CI0365
            12            LX03-FILLER PICTURE  X(15).                   CI0365
            10            LX03-CY99.                                    CI0365
            11            LX03-FILLER PICTURE  X(109).                  CI0365
            10            LX03-CY01                                     CI0365
                          REDEFINES            LX03-CY99.               CI0365
            11            LX03-NBASQ  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            11            LX03-ICPCI  PICTURE  X.                       CI0365
            11            LX03-CLUPD  PICTURE  9(3).                    CI0365
            11            LX03-DLAUP  PICTURE  9(8).                    CI0365
            11            LX03-CWRC   PICTURE  99.                      CI0365
            11            LX03-CHCR   PICTURE  99.                      CI0365
            11            LX03-GEOPD2 PICTURE  X(8).                    CI0365
            11            LX03-GEAUN  PICTURE  9(5).                    CI0365
            11            LX03-DPCHD  PICTURE  9(8).                    CI0365
            11            LX03-DLRCHK PICTURE  9(8).                    CI0365
            11            LX03-QTRCHK PICTURE  9(2).                    CI0365
            11            LX03-DNPMT  PICTURE  9(8).                    CI0365
            11            LX03-APMTLA PICTURE  S9(9)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LX03-CY02                                     CI0365
                          REDEFINES            LX03-CY99.               CI0365
            11            LX03-QSIRQ  PICTURE  99.                      CI0365
            11            LX03-QDRMN  PICTURE  9(2)                     CI0365
                          COMPUTATIONAL-3.                              CI0365
            11            LX03-DDPRE  PICTURE  9(8).                    CI0365
            11            LX03-DDSHP  PICTURE  9(8).                    CI0365
            11            LX03-NDRFTB PICTURE  9(5).                    CI0365
            11            LX03-QDIPBJ PICTURE  9(3).                    CI0365
            11            LX03-DDSHPA PICTURE  9(8).                    CI0365
            11            LX03-NDRFTF PICTURE  9(5).                    CI0365
            11            LX03-QDIPBK PICTURE  9(3).                    CI0365
            11            LX03-CREOR  PICTURE  X(1).                    CI0365
            11            LX03-CREOR1 PICTURE  X(1).                    CI0365
            11            LX03-DDASC  PICTURE  9(8).                    CI0365
            11            LX03-FILLER PICTURE  X(7).                    CI0365
            10            LX03-CY03                                     CI0365
                          REDEFINES            LX03-CY99.               CI0365
            11            LX03-DLAUP1 PICTURE  9(8).                    CI0365
            11            LX03-GEOPD3 PICTURE  X(8).                    CI0365
            11            LX03-DNPMT1 PICTURE  9(8).                    CI0365
            11            LX03-DOPDA  PICTURE  99.                      CI0365
            11            LX03-CPMTF  PICTURE  99.                      CI0365
            11            LX03-CIRMO  PICTURE  X(12).                   CI0365
            11            LX03-CPALL  PICTURE  X(1).                    CI0365
            11            LX03-CCOLM  PICTURE  9(2).                    CI0365
            11            LX03-CBLTP  PICTURE  X(1).                    CI0365
            11            LX03-CASUB  PICTURE  9(2).                    CI0365
            11            LX03-CBLFM  PICTURE  9(2).                    CI0365
            11            LX03-IBILS  PICTURE  X.                       CI0365
            11            LX03-IPAOS  PICTURE  X.                       CI0365
            11            LX03-CBLSQ  PICTURE  X(4).                    CI0365
            11            LX03-DLBPD  PICTURE  9(8).                    CI0365
            11            LX03-DNBPD  PICTURE  9(8).                    CI0365
            11            LX03-DODBD  PICTURE  9(8).                    CI0365
            11            LX03-CPSRE  PICTURE  99.                      CI0365
            11            LX03-ISPHN  PICTURE  X.                       CI0365
            11            LX03-TCARR  PICTURE  X(6).                    CI0365
            11            LX03-CBKPT  PICTURE  9(2).                    CI0365
            11            LX03-IECNT  PICTURE  X.                       CI0365
            11            LX03-ICONV  PICTURE  X(1).                    CI0365
            11            LX03-FILLER PICTURE  X(4).                    CI0365
            10            LX03-CY04                                     CI0365
                          REDEFINES            LX03-CY99.               CI0365
            11            LX03-CCARD  PICTURE  X(02).                   CI0365
            11            LX03-MCSIG4 PICTURE  X(20).                   CI0365
            11            LX03-IREMT  PICTURE  X(01).                   CI0365
            11            LX03-ISBILA PICTURE  X.                       CI0365
            11            LX03-DLBPDA PICTURE  9(8).                    CI0365
            11            LX03-DNBPDA.                                  CI0365
            12            LX03-DNCYM  PICTURE  9(6).                    CI0365
            12            LX03-CEDTD  PICTURE  9(2).                    CI0365
            11            LX03-AREMT  PICTURE  S9(7)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            11            LX03-DREMT  PICTURE  9(8).                    CI0365
            11            LX03-ADBRQ  PICTURE  S9(11)V99                CI0365
                          COMPUTATIONAL-3.                              CI0365
            11            LX03-CLUPD1 PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            11            LX03-DLAUP3 PICTURE  9(8).                    CI0365
            11            LX03-CWRC2  PICTURE  99.                      CI0365
            11            LX03-CHCR2  PICTURE  99.                      CI0365
            11            LX03-GEOPD9 PICTURE  X(8).                    CI0365
            11            LX03-GEAUN1 PICTURE  9(5).                    CI0365
            11            LX03-DPCHD1 PICTURE  9(8).                    CI0365
       01                 LX06.                                         CI0365
            10            LX06-CX06K.                                   CI0365
            11            LX06-C299.                                    CI0365
            12            LX06-CTID.                                    CI0365
            13            LX06-CTIDA  PICTURE  9(3).                    CI0365
            13            LX06-CTIDN.                                   CI0365
            14            LX06-CTIDNP PICTURE  X(13).                   CI0365
            14            LX06-CTIDND PICTURE  9(11).                   CI0365
            10            LX06-NPECK  PICTURE  9(02).                   CI0365
            10            LX06-FILLER PICTURE  X.                       CI0365
       01                 LX09.                                         CI0365
            10            LX09-CX09K.                                   CI0365
            11            LX09-NPAIS  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LX09-CDEL1  PICTURE  9(3).                    CI0365
            10            LX09-NDELS  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LX09-CDEST  PICTURE  99.                      CI0365
            10            LX09-DISUP  PICTURE  9(8).                    CI0365
            10            LX09-CLUPD  PICTURE  9(3).                    CI0365
            10            LX09-DLAUP  PICTURE  9(8).                    CI0365
            10            LX09-GEOPD2 PICTURE  X(8).                    CI0365
            10            LX09-DPCHD  PICTURE  9(8).                    CI0365
            10            LX09-FILLER PICTURE  X(06).                   CI0365
       01                 LX18.                                         CI0365
            10            LX18-CX18K.                                   CI0365
            11            LX18-NBASQ  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LX18-NPBN   PICTURE  X(20).                   CI0365
            10            LX18-CCBAT  PICTURE  99.                      CI0365
            10            LX18-DACHP  PICTURE  9(8).                    CI0365
            10            LX18-CSTPRE PICTURE  99.                      CI0365
            10            LX18-C199.                                    CI0365
            11            LX18-CLID.                                    CI0365
            12            LX18-CLIDO  PICTURE  9(3).                    CI0365
            12            LX18-CLIDN.                                   CI0365
            13            LX18-CLIDNP PICTURE  X(12).                   CI0365
            13            LX18-CLIDND PICTURE  9(8).                    CI0365
            10            LX18-MCSIG  PICTURE  X(30).                   CI0365
            10            LX18-CPBNU  PICTURE  X.                       CI0365
            10            LX18-CSPCR  PICTURE  99.                      CI0365
            10            LX18-DAPCR  PICTURE  9(8).                    CI0365
            10            LX18-FILLER PICTURE  XX.                      CI0365
       01                 LX21.                                         CI0365
            10            LX21-GELL   PICTURE  9(4)                     CI0365
                          BINARY.                                       CI0365
            10            LX21-CZ00.                                    CI0365
            11            LX21-CX21K.                                   CI0365
            12            LX21-CDEL1  PICTURE  9(3).                    CI0365
            12            LX21-NDELS  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LX21-CZ99.                                    CI0365
            11            LX21-FILLER PICTURE  X(165).                  CI0365
            10            LX21-CZ01                                     CI0365
                          REDEFINES            LX21-CZ99.               CI0365
            11            LX21-NBASQ  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            11            LX21-GECSQ  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LX21-CZ02                                     CI0365
                          REDEFINES            LX21-CZ99.               CI0365
            11            LX21-CPAYE  PICTURE  9(2).                    CI0365
            11            LX21-C199.                                    CI0365
            12            LX21-CLID.                                    CI0365
            13            LX21-CLIDO  PICTURE  9(3).                    CI0365
            13            LX21-CLIDN.                                   CI0365
            14            LX21-CLIDNP PICTURE  X(12).                   CI0365
            14            LX21-CLIDND PICTURE  9(8).                    CI0365
            11            LX21-GECSQ1 PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            11            LX21-NBASQT PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            11            LX21-TDELI  PICTURE  X(30).                   CI0365
      *!WF DSP=LX DSL=CX SEL=1821 FOR=I DES=1 LEV=1
      * PLT=WM

      *-----------------------------------------------------------------
      *      WORKING STORAGE FOR ON DEMAND FA TRANSACTIONS
      *-----------------------------------------------------------------
      *!WI
       01  WS-DXTMSA      VALUE SPACES
                        PICTURE X(26).                                  CI0365
      *!WI
       01  WS-DCACG9      VALUE ZEROES
                        PICTURE 9(8).                                   CI0365
      *!WI
       01  WS-GETIM6
                        PICTURE 9(06).                                  CI0365
      *!WI
       01  WS-DXTMSQ
                        PICTURE X(26).                                  CI0365
       01  FILLER REDEFINES WS-DXTMSQ.
           05  FILLER           PIC X(11).
      *!WI
           05  WS-TRECTF
                        PICTURE X(15).                                  CI0365
      *!WI
       01  WS-GETIM
                        PICTURE S9(7)                                   CI0365
                          COMPUTATIONAL-3.                              CI0365
       01  WS-DCACG.
              10 WS-DTGCY.
      *!WI
                 15 WS-DTGCC
                        PICTURE 9(2).                                   CI0365
      *!WI
                 15 WS-DTGYY
                        PICTURE 9(2).                                   CI0365
      *!WI
              10 WS-DTGMM
                        PICTURE 9(2).                                   CI0365
      *!WI
              10 WS-DTGDD
                        PICTURE 9(2).                                   CI0365
      *!WI
       01  WS-DNACG
                        PICTURE 9(8).                                   CI0365

      *!WI
       01  WS-DXTMS2
                        PICTURE X(26).                                  CI0365
       01  WS00-CLID     PIC X(23).
       01  WS00-GECSQ    PIC S9(3).
      *!WI
       01  WS00-PRCOD
                        PICTURE 9(5).                                   CI0365
      *!WI
       01  WS00-CIRAP
                        PICTURE XX.                                     CI0365
           88 VALID-CIRAP       VALUES 'CU' 'PR' 'IT' '  '.

      *!WI
       01  WS00-MPMTFL
                        PICTURE X(24).                                  CI0365
           88 VALID-MPMTFL      VALUES 'One-Time Now            '
                                       'One-Time Future         '.
           88 ONE-TIME-NOW      VALUES 'One-Time Now            '.
           88 ONE-TIME-FUTURE   VALUES 'One-Time Future         '.
       01  WS01.
      *!WI
           05  WS01-DEFFT
                        PICTURE 9(8).                                   CI0365
      *!WI
           05  WS01-DNPMT
                        PICTURE 9(8).                                   CI0365
      *!WI
           05  WS01-DCACG
                        PICTURE 9(8).                                   CI0365
      *!WI
           05  WS01-DNACG
                        PICTURE 9(8).                                   CI0365
      *!WI
           05  WS01-NDTUN
                        PICTURE S9(05).                                 CI0365
           05  WS01-DCACG1   PIC 9(8) VALUE ZEROES.
           05  WS01-DNPMT1   PIC 9(8) VALUE ZEROES.
           05  WS01-COUNT    PIC S9(4) COMP.
       01   DEBUT-WSS.                                                  CI0365
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0365
            05   IK     PICTURE X.                                      CI0365
       01  CONSTANTES-PAC.                                              CI0365
           05  FILLER  PICTURE X(87)   VALUE                            CI0365
                     '6015 CAT09/08/14CI0365ADMIN   14:35:22CI0365P AMERCI0365
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0365
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0365
           05  NUGNA   PICTURE X(5).                                    CI0365
           05  APPLI   PICTURE X(3).                                    CI0365
           05  DATGN   PICTURE X(8).                                    CI0365
           05  PROGR   PICTURE X(6).                                    CI0365
           05  CODUTI  PICTURE X(8).                                    CI0365
           05  TIMGN   PICTURE X(8).                                    CI0365
           05  PROGE   PICTURE X(8).                                    CI0365
           05  COBASE  PICTURE X(4).                                    CI0365
           05  DATGNC  PICTURE X(10).                                   CI0365
           05  RELEAS  PICTURE X(7).                                    CI0365
           05  DATGE   PICTURE X(10).                                   CI0365
           05  DATSQ   PICTURE X(10).                                   CI0365
       01  DATCE.                                                       CI0365
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0365
         05  DATOR.                                                     CI0365
           10  DATOA  PICTURE XX.                                       CI0365
           10  DATOM  PICTURE XX.                                       CI0365
           10  DATOJ  PICTURE XX.                                       CI0365
       01   VARIABLES-CONDITIONNELLES.                                  CI0365
            05                  FT      PICTURE X VALUE '0'.            CI0365
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0365
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0365
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0365
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0365
            05       5-DO00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0365
            05       5-FR00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0365
            05       5-TD00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0365
            05       5-TO00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0365
       01               S-CL01-SSA.                                     CI0365
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0365
                                      VALUE 'CL01    '.                 CI0365
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0365
            10          S-CL01-CCOD   PICTURE X(5)                      CI0365
                                      VALUE '-----'.                    CI0365
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0365
       01            S-CLU01-SSA.                                       CI0365
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0365
                                      VALUE 'CL01    '.                 CI0365
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0365
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0365
                                      VALUE '-----'.                    CI0365
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0365
                                      VALUE '(CL01K'.                   CI0365
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0365
            10       S-CLU01-CL01K.                                     CI0365
            11       S-CLU01-C199.                                      CI0365
            12       S-CLU01-CLID.                                      CI0365
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0365
            13       S-CLU01-CLIDN.                                     CI0365
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0365
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0365
            10  FILLER   PICTURE X    VALUE ')'.                        CI0365
       01               S-CL03-SSA.                                     CI0365
            10         S1-CL03-SEGNAM PICTURE X(8)                      CI0365
                                      VALUE 'CL03    '.                 CI0365
            10         S1-CL03-CCOM   PICTURE X VALUE '*'.              CI0365
            10          S-CL03-CCOD   PICTURE X(5)                      CI0365
                                      VALUE '-----'.                    CI0365
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0365
       01            S-CLA03-SSA.                                       CI0365
            10      S1-CLA03-SEGNAM PICTURE X(8)                        CI0365
                                      VALUE 'CL03    '.                 CI0365
            10      S1-CLA03-CCOM   PICTURE X VALUE '*'.                CI0365
            10       S-CLA03-CCOD   PICTURE X(5)                        CI0365
                                      VALUE '-----'.                    CI0365
            10      S1-CLA03-FLDNAM PICTURE X(9)                        CI0365
                                      VALUE '(CLDOD'.                   CI0365
            10       S-CLA03-OPER  PICTURE XX VALUE ' ='.               CI0365
            10       S-CLA03-CLDOD    PICTURE  9(8).                    CI0365
            10  FILLER   PICTURE X    VALUE ')'.                        CI0365
       01               S-CL12-SSA.                                     CI0365
            10         S1-CL12-SEGNAM PICTURE X(8)                      CI0365
                                      VALUE 'CL12    '.                 CI0365
            10         S1-CL12-CCOM   PICTURE X VALUE '*'.              CI0365
            10          S-CL12-CCOD   PICTURE X(5)                      CI0365
                                      VALUE '-----'.                    CI0365
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0365
       01               S-CL18-SSA.                                     CI0365
            10         S1-CL18-SEGNAM PICTURE X(8)                      CI0365
                                      VALUE 'CL18    '.                 CI0365
            10         S1-CL18-CCOM   PICTURE X VALUE '*'.              CI0365
            10          S-CL18-CCOD   PICTURE X(5)                      CI0365
                                      VALUE '-----'.                    CI0365
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0365
       01            S-CLU18-SSA.                                       CI0365
            10      S1-CLU18-SEGNAM PICTURE X(8)                        CI0365
                                      VALUE 'CL18    '.                 CI0365
            10      S1-CLU18-CCOM   PICTURE X VALUE '*'.                CI0365
            10       S-CLU18-CCOD   PICTURE X(5)                        CI0365
                                      VALUE '-----'.                    CI0365
            10      S1-CLU18-FLDNAM PICTURE X(9)                        CI0365
                                      VALUE '(CL18K'.                   CI0365
            10       S-CLU18-OPER  PICTURE XX VALUE ' ='.               CI0365
            10       S-CLU18-CL18K.                                     CI0365
            11       S-CLU18-NRTSQ    PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10  FILLER   PICTURE X    VALUE ')'.                        CI0365
       01            S-CL118-SSA.                                       CI0365
            10      S1-CL118-SEGNAM PICTURE X(8)                        CI0365
                                      VALUE 'CL18    '.                 CI0365
            10      S1-CL118-CCOM   PICTURE X VALUE '*'.                CI0365
            10       S-CL118-CCOD   PICTURE X(5)                        CI0365
                                      VALUE '-----'.                    CI0365
            10      S1-CL118-FLDNAM PICTURE X(9)                        CI0365
                                      VALUE '(XNTR'.                    CI0365
            10       S-CL118-OPER  PICTURE XX VALUE ' ='.               CI0365
            10       S-CL118-NTR      PICTURE  9(8).                    CI0365
            10  FILLER   PICTURE X    VALUE ')'.                        CI0365
       01            S-CL218-SSA.                                       CI0365
            10      S1-CL218-SEGNAM PICTURE X(8)                        CI0365
                                      VALUE 'CL18    '.                 CI0365
            10      S1-CL218-CCOM   PICTURE X VALUE '*'.                CI0365
            10       S-CL218-CCOD   PICTURE X(5)                        CI0365
                                      VALUE '-----'.                    CI0365
            10      S1-CL218-FLDNAM PICTURE X(9)                        CI0365
                                      VALUE '(XGEEND'.                  CI0365
            10       S-CL218-OPER  PICTURE XX VALUE ' ='.               CI0365
            10       S-CL218-GEEND    PICTURE  9(8).                    CI0365
            10  FILLER   PICTURE X    VALUE ')'.                        CI0365
       01            S-CL318-SSA.                                       CI0365
            10      S1-CL318-SEGNAM PICTURE X(8)                        CI0365
                                      VALUE 'CL18    '.                 CI0365
            10      S1-CL318-CCOM   PICTURE X VALUE '*'.                CI0365
            10       S-CL318-CCOD   PICTURE X(5)                        CI0365
                                      VALUE '-----'.                    CI0365
            10      S1-CL318-FLDNAM PICTURE X(9)                        CI0365
                                      VALUE '(XIRTNA'.                  CI0365
            10       S-CL318-OPER  PICTURE XX VALUE ' ='.               CI0365
            10       S-CL318-IRTNA    PICTURE  X.                       CI0365
            10  FILLER   PICTURE X    VALUE ')'.                        CI0365
       01            S-CL418-SSA.                                       CI0365
            10      S1-CL418-SEGNAM PICTURE X(8)                        CI0365
                                      VALUE 'CL18    '.                 CI0365
            10      S1-CL418-CCOM   PICTURE X VALUE '*'.                CI0365
            10       S-CL418-CCOD   PICTURE X(5)                        CI0365
                                      VALUE '-----'.                    CI0365
            10      S1-CL418-FLDNAM PICTURE X(9)                        CI0365
                                      VALUE '(XIRTNP'.                  CI0365
            10       S-CL418-OPER  PICTURE XX VALUE ' ='.               CI0365
            10       S-CL418-IRTNP    PICTURE  X.                       CI0365
            10  FILLER   PICTURE X    VALUE ')'.                        CI0365
       01               S-CL2Y-SSA.                                     CI0365
            10         S1-CL2Y-SEGNAM PICTURE X(8)                      CI0365
                                      VALUE 'CL2Y    '.                 CI0365
            10         S1-CL2Y-CCOM   PICTURE X VALUE '*'.              CI0365
            10          S-CL2Y-CCOD   PICTURE X(5)                      CI0365
                                      VALUE '-----'.                    CI0365
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0365
       01            S-CLU2Y-SSA.                                       CI0365
            10      S1-CLU2Y-SEGNAM PICTURE X(8)                        CI0365
                                      VALUE 'CL2Y    '.                 CI0365
            10      S1-CLU2Y-CCOM   PICTURE X VALUE '*'.                CI0365
            10       S-CLU2Y-CCOD   PICTURE X(5)                        CI0365
                                      VALUE '-----'.                    CI0365
            10      S1-CLU2Y-FLDNAM PICTURE X(9)                        CI0365
                                      VALUE '(CL2YK'.                   CI0365
            10       S-CLU2Y-OPER  PICTURE XX VALUE ' ='.               CI0365
            10       S-CLU2Y-CL2YK.                                     CI0365
            11       S-CLU2Y-NTR      PICTURE  9(8).                    CI0365
            11       S-CLU2Y-C199.                                      CI0365
            12       S-CLU2Y-CLID.                                      CI0365
            13       S-CLU2Y-CLIDO    PICTURE  9(3).                    CI0365
            13       S-CLU2Y-CLIDN.                                     CI0365
            14       S-CLU2Y-CLIDNP   PICTURE  X(12).                   CI0365
            14       S-CLU2Y-CLIDND   PICTURE  9(8).                    CI0365
            11       S-CLU2Y-NRTSQ    PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10  FILLER   PICTURE X    VALUE ')'.                        CI0365
       01               S-FR01-SSA.                                     CI0365
            10         S1-FR01-SEGNAM PICTURE X(8)                      CI0365
                                      VALUE 'CT01    '.                 CI0365
            10         S1-FR01-CCOM   PICTURE X VALUE '*'.              CI0365
            10          S-FR01-CCOD   PICTURE X(5)                      CI0365
                                      VALUE '-----'.                    CI0365
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0365
       01            S-FRU01-SSA.                                       CI0365
            10      S1-FRU01-SEGNAM PICTURE X(8)                        CI0365
                                      VALUE 'CT01    '.                 CI0365
            10      S1-FRU01-CCOM   PICTURE X VALUE '*'.                CI0365
            10       S-FRU01-CCOD   PICTURE X(5)                        CI0365
                                      VALUE '-----'.                    CI0365
            10      S1-FRU01-FLDNAM PICTURE X(9)                        CI0365
                                      VALUE '(CT01K'.                   CI0365
            10       S-FRU01-OPER  PICTURE XX VALUE ' ='.               CI0365
            10       S-FRU01-CT01K.                                     CI0365
            11       S-FRU01-C299.                                      CI0365
            12       S-FRU01-CTID.                                      CI0365
            13       S-FRU01-CTIDA    PICTURE  9(3).                    CI0365
            13       S-FRU01-CTIDN.                                     CI0365
            14       S-FRU01-CTIDNP   PICTURE  X(13).                   CI0365
            14       S-FRU01-CTIDND   PICTURE  9(11).                   CI0365
            10  FILLER   PICTURE X    VALUE ')'.                        CI0365
       01               S-TO01-SSA.                                     CI0365
            10         S1-TO01-SEGNAM PICTURE X(8)                      CI0365
                                      VALUE 'CT01    '.                 CI0365
            10         S1-TO01-CCOM   PICTURE X VALUE '*'.              CI0365
            10          S-TO01-CCOD   PICTURE X(5)                      CI0365
                                      VALUE '-----'.                    CI0365
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0365
       01            S-TOU01-SSA.                                       CI0365
            10      S1-TOU01-SEGNAM PICTURE X(8)                        CI0365
                                      VALUE 'CT01    '.                 CI0365
            10      S1-TOU01-CCOM   PICTURE X VALUE '*'.                CI0365
            10       S-TOU01-CCOD   PICTURE X(5)                        CI0365
                                      VALUE '-----'.                    CI0365
            10      S1-TOU01-FLDNAM PICTURE X(9)                        CI0365
                                      VALUE '(CT01K'.                   CI0365
            10       S-TOU01-OPER  PICTURE XX VALUE ' ='.               CI0365
            10       S-TOU01-CT01K.                                     CI0365
            11       S-TOU01-C299.                                      CI0365
            12       S-TOU01-CTID.                                      CI0365
            13       S-TOU01-CTIDA    PICTURE  9(3).                    CI0365
            13       S-TOU01-CTIDN.                                     CI0365
            14       S-TOU01-CTIDNP   PICTURE  X(13).                   CI0365
            14       S-TOU01-CTIDND   PICTURE  9(11).                   CI0365
            10  FILLER   PICTURE X    VALUE ')'.                        CI0365
       01   ZONES-UTILISATEUR PICTURE X.                                CI0365
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
      ** PCB POINTER FOR ACAP                                           ADU015
            05 PCB-ACAP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR DATP                                           ADU015
            05 PCB-DATP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR ARAY                                           ADU015
            05 PCB-ARAY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CLUY                                           ADU015
            05 PCB-CLUY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR GR1P                                           ADU015
            05 PCB-GR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CH1P                                           ADU015
            05 PCB-CH1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CCRP                                           ADU015
            05 PCB-CCRP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CPRP                                           ADU015
            05 PCB-CPRP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CBTP                                           ADU015
            05 PCB-CBTP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CA1P                                           ADU015
            05 PCB-CA1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR TR1P                                           ADU015
            05 PCB-TR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR ACAP                                             ADU015
      *!WF DSP=XA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XA00.                                         CI0365
          05              XA00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00106).                  CI0365
       01                 XA06  REDEFINES      XA00.                    CI0365
            10            XA06-XDBPCB.                                  CI0365
            11            XA06-XDBDNM PICTURE  X(08).                   CI0365
            11            XA06-XSEGLV PICTURE  X(02).                   CI0365
            11            XA06-XRC    PICTURE  X(02).                   CI0365
            11            XA06-XPROPT PICTURE  X(04).                   CI0365
            11            XA06-FILLER PICTURE  S9(5)                    CI0365
                          BINARY.                                       CI0365
            11            XA06-XSEGNM PICTURE  X(08).                   CI0365
            11            XA06-XKEYLN PICTURE  S9(05)                   CI0365
                          BINARY.                                       CI0365
            11            XA06-XSEGNB PICTURE  9(05)                    CI0365
                          BINARY.                                       CI0365
            11            XA06-XCOKEY PICTURE  X(70).                   CI0365
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=XB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XB00.                                         CI0365
          05              XB00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00106).                  CI0365
       01                 XB06  REDEFINES      XB00.                    CI0365
            10            XB06-XDBPCB.                                  CI0365
            11            XB06-XDBDNM PICTURE  X(08).                   CI0365
            11            XB06-XSEGLV PICTURE  X(02).                   CI0365
            11            XB06-XRC    PICTURE  X(02).                   CI0365
            11            XB06-XPROPT PICTURE  X(04).                   CI0365
            11            XB06-FILLER PICTURE  S9(5)                    CI0365
                          BINARY.                                       CI0365
            11            XB06-XSEGNM PICTURE  X(08).                   CI0365
            11            XB06-XKEYLN PICTURE  S9(05)                   CI0365
                          BINARY.                                       CI0365
            11            XB06-XSEGNB PICTURE  9(05)                    CI0365
                          BINARY.                                       CI0365
            11            XB06-XCOKEY PICTURE  X(70).                   CI0365
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=XC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XC00.                                         CI0365
          05              XC00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00106).                  CI0365
       01                 XC06  REDEFINES      XC00.                    CI0365
            10            XC06-XDBPCB.                                  CI0365
            11            XC06-XDBDNM PICTURE  X(08).                   CI0365
            11            XC06-XSEGLV PICTURE  X(02).                   CI0365
            11            XC06-XRC    PICTURE  X(02).                   CI0365
            11            XC06-XPROPT PICTURE  X(04).                   CI0365
            11            XC06-FILLER PICTURE  S9(5)                    CI0365
                          BINARY.                                       CI0365
            11            XC06-XSEGNM PICTURE  X(08).                   CI0365
            11            XC06-XKEYLN PICTURE  S9(05)                   CI0365
                          BINARY.                                       CI0365
            11            XC06-XSEGNB PICTURE  9(05)                    CI0365
                          BINARY.                                       CI0365
            11            XC06-XCOKEY PICTURE  X(70).                   CI0365
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=XD DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XD00.                                         CI0365
          05              XD00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00106).                  CI0365
       01                 XD06  REDEFINES      XD00.                    CI0365
            10            XD06-XDBPCB.                                  CI0365
            11            XD06-XDBDNM PICTURE  X(08).                   CI0365
            11            XD06-XSEGLV PICTURE  X(02).                   CI0365
            11            XD06-XRC    PICTURE  X(02).                   CI0365
            11            XD06-XPROPT PICTURE  X(04).                   CI0365
            11            XD06-FILLER PICTURE  S9(5)                    CI0365
                          BINARY.                                       CI0365
            11            XD06-XSEGNM PICTURE  X(08).                   CI0365
            11            XD06-XKEYLN PICTURE  S9(05)                   CI0365
                          BINARY.                                       CI0365
            11            XD06-XSEGNB PICTURE  9(05)                    CI0365
                          BINARY.                                       CI0365
            11            XD06-XCOKEY PICTURE  X(70).                   CI0365
      *** PCB MASK FOR DATP                                             ADU015
      *!WF DSP=XF DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XF00.                                         CI0365
          05              XF00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00106).                  CI0365
       01                 XF06  REDEFINES      XF00.                    CI0365
            10            XF06-XDBPCB.                                  CI0365
            11            XF06-XDBDNM PICTURE  X(08).                   CI0365
            11            XF06-XSEGLV PICTURE  X(02).                   CI0365
            11            XF06-XRC    PICTURE  X(02).                   CI0365
            11            XF06-XPROPT PICTURE  X(04).                   CI0365
            11            XF06-FILLER PICTURE  S9(5)                    CI0365
                          BINARY.                                       CI0365
            11            XF06-XSEGNM PICTURE  X(08).                   CI0365
            11            XF06-XKEYLN PICTURE  S9(05)                   CI0365
                          BINARY.                                       CI0365
            11            XF06-XSEGNB PICTURE  9(05)                    CI0365
                          BINARY.                                       CI0365
            11            XF06-XCOKEY PICTURE  X(70).                   CI0365
      *** PCB MASK FOR ARAY                                             ADU015
      *!WF DSP=XG DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XG00.                                         CI0365
          05              XG00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00106).                  CI0365
       01                 XG06  REDEFINES      XG00.                    CI0365
            10            XG06-XDBPCB.                                  CI0365
            11            XG06-XDBDNM PICTURE  X(08).                   CI0365
            11            XG06-XSEGLV PICTURE  X(02).                   CI0365
            11            XG06-XRC    PICTURE  X(02).                   CI0365
            11            XG06-XPROPT PICTURE  X(04).                   CI0365
            11            XG06-FILLER PICTURE  S9(5)                    CI0365
                          BINARY.                                       CI0365
            11            XG06-XSEGNM PICTURE  X(08).                   CI0365
            11            XG06-XKEYLN PICTURE  S9(05)                   CI0365
                          BINARY.                                       CI0365
            11            XG06-XSEGNB PICTURE  9(05)                    CI0365
                          BINARY.                                       CI0365
            11            XG06-XCOKEY PICTURE  X(70).                   CI0365
      *** PCB MASK FOR CLUY                                             ADU015
      *!WF DSP=XH DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XH00.                                         CI0365
          05              XH00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00106).                  CI0365
       01                 XH06  REDEFINES      XH00.                    CI0365
            10            XH06-XDBPCB.                                  CI0365
            11            XH06-XDBDNM PICTURE  X(08).                   CI0365
            11            XH06-XSEGLV PICTURE  X(02).                   CI0365
            11            XH06-XRC    PICTURE  X(02).                   CI0365
            11            XH06-XPROPT PICTURE  X(04).                   CI0365
            11            XH06-FILLER PICTURE  S9(5)                    CI0365
                          BINARY.                                       CI0365
            11            XH06-XSEGNM PICTURE  X(08).                   CI0365
            11            XH06-XKEYLN PICTURE  S9(05)                   CI0365
                          BINARY.                                       CI0365
            11            XH06-XSEGNB PICTURE  9(05)                    CI0365
                          BINARY.                                       CI0365
            11            XH06-XCOKEY PICTURE  X(70).                   CI0365
      *** PCB MASK FOR GR1P                                             ADU015
      *!WF DSP=XI DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XI00.                                         CI0365
          05              XI00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00106).                  CI0365
       01                 XI06  REDEFINES      XI00.                    CI0365
            10            XI06-XDBPCB.                                  CI0365
            11            XI06-XDBDNM PICTURE  X(08).                   CI0365
            11            XI06-XSEGLV PICTURE  X(02).                   CI0365
            11            XI06-XRC    PICTURE  X(02).                   CI0365
            11            XI06-XPROPT PICTURE  X(04).                   CI0365
            11            XI06-FILLER PICTURE  S9(5)                    CI0365
                          BINARY.                                       CI0365
            11            XI06-XSEGNM PICTURE  X(08).                   CI0365
            11            XI06-XKEYLN PICTURE  S9(05)                   CI0365
                          BINARY.                                       CI0365
            11            XI06-XSEGNB PICTURE  9(05)                    CI0365
                          BINARY.                                       CI0365
            11            XI06-XCOKEY PICTURE  X(70).                   CI0365
      *** PCB MASK FOR CH1P                                             ADU015
      *!WF DSP=XJ DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XJ00.                                         CI0365
          05              XJ00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00106).                  CI0365
       01                 XJ06  REDEFINES      XJ00.                    CI0365
            10            XJ06-XDBPCB.                                  CI0365
            11            XJ06-XDBDNM PICTURE  X(08).                   CI0365
            11            XJ06-XSEGLV PICTURE  X(02).                   CI0365
            11            XJ06-XRC    PICTURE  X(02).                   CI0365
            11            XJ06-XPROPT PICTURE  X(04).                   CI0365
            11            XJ06-FILLER PICTURE  S9(5)                    CI0365
                          BINARY.                                       CI0365
            11            XJ06-XSEGNM PICTURE  X(08).                   CI0365
            11            XJ06-XKEYLN PICTURE  S9(05)                   CI0365
                          BINARY.                                       CI0365
            11            XJ06-XSEGNB PICTURE  9(05)                    CI0365
                          BINARY.                                       CI0365
            11            XJ06-XCOKEY PICTURE  X(70).                   CI0365
      *** PCB MASK FOR CCRP                                             ADU015
      *!WF DSP=XK DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XK00.                                         CI0365
          05              XK00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00106).                  CI0365
       01                 XK06  REDEFINES      XK00.                    CI0365
            10            XK06-XDBPCB.                                  CI0365
            11            XK06-XDBDNM PICTURE  X(08).                   CI0365
            11            XK06-XSEGLV PICTURE  X(02).                   CI0365
            11            XK06-XRC    PICTURE  X(02).                   CI0365
            11            XK06-XPROPT PICTURE  X(04).                   CI0365
            11            XK06-FILLER PICTURE  S9(5)                    CI0365
                          BINARY.                                       CI0365
            11            XK06-XSEGNM PICTURE  X(08).                   CI0365
            11            XK06-XKEYLN PICTURE  S9(05)                   CI0365
                          BINARY.                                       CI0365
            11            XK06-XSEGNB PICTURE  9(05)                    CI0365
                          BINARY.                                       CI0365
            11            XK06-XCOKEY PICTURE  X(70).                   CI0365
      *** PCB MASK FOR CPRP                                             ADU015
      *!WF DSP=XL DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XL00.                                         CI0365
          05              XL00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00106).                  CI0365
       01                 XL06  REDEFINES      XL00.                    CI0365
            10            XL06-XDBPCB.                                  CI0365
            11            XL06-XDBDNM PICTURE  X(08).                   CI0365
            11            XL06-XSEGLV PICTURE  X(02).                   CI0365
            11            XL06-XRC    PICTURE  X(02).                   CI0365
            11            XL06-XPROPT PICTURE  X(04).                   CI0365
            11            XL06-FILLER PICTURE  S9(5)                    CI0365
                          BINARY.                                       CI0365
            11            XL06-XSEGNM PICTURE  X(08).                   CI0365
            11            XL06-XKEYLN PICTURE  S9(05)                   CI0365
                          BINARY.                                       CI0365
            11            XL06-XSEGNB PICTURE  9(05)                    CI0365
                          BINARY.                                       CI0365
            11            XL06-XCOKEY PICTURE  X(70).                   CI0365
      *** PCB MASK FOR CBTP                                             ADU015
      *!WF DSP=XM DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XM00.                                         CI0365
          05              XM00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00106).                  CI0365
       01                 XM06  REDEFINES      XM00.                    CI0365
            10            XM06-XDBPCB.                                  CI0365
            11            XM06-XDBDNM PICTURE  X(08).                   CI0365
            11            XM06-XSEGLV PICTURE  X(02).                   CI0365
            11            XM06-XRC    PICTURE  X(02).                   CI0365
            11            XM06-XPROPT PICTURE  X(04).                   CI0365
            11            XM06-FILLER PICTURE  S9(5)                    CI0365
                          BINARY.                                       CI0365
            11            XM06-XSEGNM PICTURE  X(08).                   CI0365
            11            XM06-XKEYLN PICTURE  S9(05)                   CI0365
                          BINARY.                                       CI0365
            11            XM06-XSEGNB PICTURE  9(05)                    CI0365
                          BINARY.                                       CI0365
            11            XM06-XCOKEY PICTURE  X(70).                   CI0365
      *** PCB MASK FOR CA1P                                             ADU015
      *!WF DSP=XN DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XN00.                                         CI0365
          05              XN00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00106).                  CI0365
       01                 XN06  REDEFINES      XN00.                    CI0365
            10            XN06-XDBPCB.                                  CI0365
            11            XN06-XDBDNM PICTURE  X(08).                   CI0365
            11            XN06-XSEGLV PICTURE  X(02).                   CI0365
            11            XN06-XRC    PICTURE  X(02).                   CI0365
            11            XN06-XPROPT PICTURE  X(04).                   CI0365
            11            XN06-FILLER PICTURE  S9(5)                    CI0365
                          BINARY.                                       CI0365
            11            XN06-XSEGNM PICTURE  X(08).                   CI0365
            11            XN06-XKEYLN PICTURE  S9(05)                   CI0365
                          BINARY.                                       CI0365
            11            XN06-XSEGNB PICTURE  9(05)                    CI0365
                          BINARY.                                       CI0365
            11            XN06-XCOKEY PICTURE  X(70).                   CI0365
      *** PCB MASK FOR TR1P                                             ADU015
      *!WF DSP=XO DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XO00.                                         CI0365
          05              XO00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00106).                  CI0365
       01                 XO06  REDEFINES      XO00.                    CI0365
            10            XO06-XDBPCB.                                  CI0365
            11            XO06-XDBDNM PICTURE  X(08).                   CI0365
            11            XO06-XSEGLV PICTURE  X(02).                   CI0365
            11            XO06-XRC    PICTURE  X(02).                   CI0365
            11            XO06-XPROPT PICTURE  X(04).                   CI0365
            11            XO06-FILLER PICTURE  S9(5)                    CI0365
                          BINARY.                                       CI0365
            11            XO06-XSEGNM PICTURE  X(08).                   CI0365
            11            XO06-XKEYLN PICTURE  S9(05)                   CI0365
                          BINARY.                                       CI0365
            11            XO06-XSEGNB PICTURE  9(05)                    CI0365
                          BINARY.                                       CI0365
            11            XO06-XCOKEY PICTURE  X(70).                   CI0365
      *
      ******************************************************************
      * * INPUT SEGMENT FOR CI0365                                     *
      ******************************************************************
      *!WF DSP=LK DSL=V1 SEL=70 FOR=I DES=1 LEV=1 PLT=50
       01                 LK70.                                         CI0365
            10            LK70-C299.                                    CI0365
            11            LK70-CTID.                                    CI0365
            12            LK70-CTIDA  PICTURE  9(3).                    CI0365
            12            LK70-CTIDN.                                   CI0365
            13            LK70-CTIDNP PICTURE  X(13).                   CI0365
            13            LK70-CTIDND PICTURE  9(11).                   CI0365
            10            LK70-GECKD  PICTURE  9.                       CI0365
            10            LK70-ICUST  PICTURE  X.                       CI0365
            10            LK70-PRCOD  PICTURE  9(5).                    CI0365
            10            LK70-PRSCD  PICTURE  X(9).                    CI0365
            10            LK70-DCACG9 PICTURE  9(8).                    CI0365
            10            LK70-NAASQ  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-CAATY  PICTURE  9(3).                    CI0365
            10            LK70-CACTO  PICTURE  9(3).                    CI0365
            10            LK70-CASTC  PICTURE  99.                      CI0365
            10            LK70-ITRAN  PICTURE  X.                       CI0365
            10            LK70-GEAUN  PICTURE  9(5).                    CI0365
            10            LK70-GEOPD2 PICTURE  X(8).                    CI0365
            10            LK70-DEFFT  PICTURE  9(8).                    CI0365
            10            LK70-CTRTP  PICTURE  X(2).                    CI0365
            10            LK70-CTWHAT PICTURE  S9(7)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-PWHLD  PICTURE  S999V9(5)                CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-GETIM  PICTURE  S9(7)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-ADBRQA PICTURE  S9(9)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-IWTHH  PICTURE  X.                       CI0365
            10            LK70-CLCUS  PICTURE  99.                      CI0365
            10            LK70-CCACT  PICTURE  99.                      CI0365
            10            LK70-AFEET  PICTURE  S9(5)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-ITERF  PICTURE  X.                       CI0365
            10            LK70-ATERF  PICTURE  S9(5)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-CLDOB  PICTURE  9(8).                    CI0365
            10            LK70-CPLTYP PICTURE  X(14).                   CI0365
            10            LK70-IACFPD PICTURE  X(1).                    CI0365
            10            LK70-CDELI  PICTURE  9(3).                    CI0365
            10            LK70-CPAYC  PICTURE  X(2).                    CI0365
            10            LK70-ACOTD  PICTURE  S9(9)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-NPBN   PICTURE  X(20).                   CI0365
            10            LK70-CCBAT  PICTURE  99.                      CI0365
            10            LK70-CLID4.                                   CI0365
            11            LK70-CLIDA  PICTURE  9(3).                    CI0365
            11            LK70-CLIDNP PICTURE  X(12).                   CI0365
            11            LK70-CLIDNA PICTURE  9(8).                    CI0365
            10            LK70-GENAL1 PICTURE  X(30).                   CI0365
            10            LK70-GENAL2 PICTURE  X(30).                   CI0365
            10            LK70-GESAD1 PICTURE  X(30).                   CI0365
            10            LK70-GESAD2 PICTURE  X(30).                   CI0365
            10            LK70-GESAD3 PICTURE  X(30).                   CI0365
            10            LK70-NTR    PICTURE  9(8).                    CI0365
            10            LK70-GECKD1 PICTURE  9.                       CI0365
            10            LK70-IMQMG  PICTURE  X.                       CI0365
            10            LK70-NIPAD  PICTURE  X(15).                   CI0365
            10            LK70-CLNAM.                                   CI0365
            11            LK70-CLNAMH PICTURE  X(6).                    CI0365
            11            LK70-CLNAMF PICTURE  X(20).                   CI0365
            11            LK70-CLNAMI PICTURE  X.                       CI0365
            11            LK70-CLNAMR PICTURE  X(14).                   CI0365
            11            LK70-CLNAML PICTURE  X(25).                   CI0365
            11            LK70-CLNAMS PICTURE  X(4).                    CI0365
            10            LK70-CSLCT  PICTURE  X.                       CI0365
            10            LK70-C199.                                    CI0365
            11            LK70-CLID.                                    CI0365
            12            LK70-CLIDO  PICTURE  9(3).                    CI0365
            12            LK70-CLIDN.                                   CI0365
            13            LK70-CLIDNP PICTURE  X(12).                   CI0365
            13            LK70-CLIDND PICTURE  9(8).                    CI0365
            10            LK70-GECKD2 PICTURE  9.                       CI0365
            10            LK70-CPROCM PICTURE  X.                       CI0365
            10            LK70-NAASQL PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-NSEQ4B PICTURE  9(8)                     CI0365
                          BINARY.                                       CI0365
            10            LK70-CLTIN  PICTURE  9(12).                   CI0365
            10            LK70-IPULL  PICTURE  X.                       CI0365
            10            LK70-NBTCH  PICTURE  9(4).                    CI0365
            10            LK70-CVSYS  PICTURE  X(2).                    CI0365
            10            LK70-NPISQ  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-CPITC  PICTURE  99.                      CI0365
            10            LK70-PPOTD  PICTURE  S9(3)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-ITRNB  PICTURE  X.                       CI0365
            10            LK70-CTTLN1 PICTURE  X(30).                   CI0365
            10            LK70-CTTLN2 PICTURE  X(30).                   CI0365
            10            LK70-CTTLN3 PICTURE  X(30).                   CI0365
            10            LK70-CTTBO1 PICTURE  X(45).                   CI0365
            10            LK70-CTTBO2 PICTURE  X(45).                   CI0365
            10            LK70-PRCMN  PICTURE  X(20).                   CI0365
            10            LK70-TTRTP  PICTURE  X(30).                   CI0365
            10            LK70-DXTMSA PICTURE  X(26).                   CI0365
            10            LK70-DXTMS2 PICTURE  X(26).                   CI0365
            10            LK70-CUPIQ  PICTURE  X.                       CI0365
            10            LK70-IQACT  PICTURE  X.                       CI0365
            10            LK70-MAPPN  PICTURE  X(10).                   CI0365
            10            LK70-CTTYPG PICTURE  X(04).                   CI0365
            10            LK70-CLORN  PICTURE  X(45).                   CI0365
            10            LK70-APMTL  PICTURE  S9(9)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-MPMTFL PICTURE  X(24).                   CI0365
            10            LK70-ANETTQ PICTURE  S9(9)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-TTBAL  PICTURE  X(15).                   CI0365
            10            LK70-MPRN4  PICTURE  X(35).                   CI0365
            10            LK70-CCONF  PICTURE  X(25).                   CI0365
            10            LK70-DCACG  PICTURE  9(8).                    CI0365
            10            LK70-NMESA  PICTURE  9(6).                    CI0365
            10            LK70-MCSIG  PICTURE  X(30).                   CI0365
            10            LK70-IERRC  PICTURE  X.                       CI0365
            10            LK70-AVLMN  PICTURE  S9(7)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-AVLMX  PICTURE  S9(7)V99                 CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-AVCSH  PICTURE  S9(11)V99                CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-ACVALM PICTURE  S9(11)V99                CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-INDRS  PICTURE  X.                       CI0365
            10            LK70-GRID   PICTURE  X(13).                   CI0365
            10            LK70-AACTV  PICTURE  S9(11)V99                CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-CTCUS  PICTURE  999.                     CI0365
            10            LK70-CCDSCW PICTURE  9(2).                    CI0365
            10            LK70-CHCR   PICTURE  99.                      CI0365
            10            LK70-IOWNG  PICTURE  X.                       CI0365
            10            LK70-GECSQ  PICTURE  S9(3)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            LK70-CQACT  PICTURE  999.                     CI0365
            10            LK70-CTOWN  PICTURE  9(3).                    CI0365
            10            LK70-NGEOR  PICTURE  9(08).                   CI0365
            10            LK70-FILLER PICTURE  X(85).                   CI0365
       01                 LK81.                                         CI0365
            10            LK81-C299.                                    CI0365
            11            LK81-CTID.                                    CI0365
            12            LK81-CTIDA  PICTURE  9(3).                    CI0365
            12            LK81-CTIDN.                                   CI0365
            13            LK81-CTIDNP PICTURE  X(13).                   CI0365
            13            LK81-CTIDND PICTURE  9(11).                   CI0365
            10            LK81-GECKD  PICTURE  9.                       CI0365
            10            LK81-CTTLN1 PICTURE  X(30).                   CI0365
            10            LK81-CTTLN2 PICTURE  X(30).                   CI0365
            10            LK81-CTTLN3 PICTURE  X(30).                   CI0365
            10            LK81-CTTBO1 PICTURE  X(45).                   CI0365
            10            LK81-CTTBO2 PICTURE  X(45).                   CI0365
            10            LK81-PRCMN  PICTURE  X(20).                   CI0365
            10            LK81-CIRAP  PICTURE  XX.                      CI0365
            10            LK81-IQACT  PICTURE  X.                       CI0365
            10            LK81-CTTYP2 PICTURE  X.                       CI0365
            10            LK81-CTYPE  PICTURE  X.                       CI0365
            10            LK81-IOWNC  PICTURE  X.                       CI0365
            10            LK81-CLCUS  PICTURE  99.                      CI0365
            10            LK81-CCACT  PICTURE  99.                      CI0365
            10            LK81-CPLTYP PICTURE  X(14).                   CI0365
            10            LK81-ITERF  PICTURE  X.                       CI0365
            10            LK81-IACFPD PICTURE  X(1).                    CI0365
            10            LK81-FILLER PICTURE  X(50).                   CI0365
      *
      *!WF DSP=LK DSL=V1 SEL=81 FOR=I DES=1 LEV=1 PLT=50
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0365
          05              DE00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00653).                  CI0365
       01                 DE10  REDEFINES      DE00.                    CI0365
            10            DE10-DU11.                                    CI0365
            11            DE10-XFONC  PICTURE  X(4).                    CI0365
            11            DE10-MPSBN  PICTURE  X(8).                    CI0365
            11            DE10-XDBDNM PICTURE  X(08).                   CI0365
            11            DE10-XSEGNM PICTURE  X(08).                   CI0365
            11            DE10-XRC    PICTURE  X(02).                   CI0365
            11            DE10-MSEG   PICTURE  X(08).                   CI0365
            11            DE10-XCOKEY PICTURE  X(70).                   CI0365
            11            DE10-CUIBR  PICTURE  X(01).                   CI0365
            11            DE10-CUIBA  PICTURE  X(01).                   CI0365
            11            DE10-IPBIK  PICTURE  X(1).                    CI0365
            10            DE10-DU03.                                    CI0365
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            11            DE10-CMSSF  PICTURE  XX.                      CI0365
            11            DE10-DU09.                                    CI0365
            12            DE10-CMESA  PICTURE  S9(9)                    CI0365
                          BINARY.                                       CI0365
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0365
                          BINARY.                                       CI0365
            12            DE10-CMESB  PICTURE  S9(9)                    CI0365
                          BINARY.                                       CI0365
            12            DE10-CMSST  PICTURE  S9(9)                    CI0365
                          BINARY.                                       CI0365
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0365
                          BINARY.                                       CI0365
            12            DE10-QELLAA PICTURE  S9(9)                    CI0365
                          BINARY.                                       CI0365
            12            DE10-TMESS4 PICTURE  X(512).                  CI0365
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
       01                 MS00.                                         CI0365
          05              MS00-SUITE.                                   CI0365
            15       FILLER         PICTURE  X(00542).                  CI0365
       01                 MS03  REDEFINES      MS00.                    CI0365
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            10            MS03-CMSSF  PICTURE  XX.                      CI0365
            10            MS03-DU09.                                    CI0365
            11            MS03-CMESA  PICTURE  S9(9)                    CI0365
                          BINARY.                                       CI0365
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0365
                          BINARY.                                       CI0365
            11            MS03-CMESB  PICTURE  S9(9)                    CI0365
                          BINARY.                                       CI0365
            11            MS03-CMSST  PICTURE  S9(9)                    CI0365
                          BINARY.                                       CI0365
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0365
                          BINARY.                                       CI0365
            11            MS03-QELLAA PICTURE  S9(9)                    CI0365
                          BINARY.                                       CI0365
            11            MS03-TMESS4 PICTURE  X(512).                  CI0365
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0365
            10            MX11-QMSGS  PICTURE  9(03).                   CI0365
            10            MX11-PJ09                                     CI0365
                          OCCURS       025     TIMES.                   CI0365
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0365
                          COMPUTATIONAL-3.                              CI0365
            11            MX11-CMESB  PICTURE  S9(9)                    CI0365
                          BINARY.                                       CI0365
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                LK70
                                LK81
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
       F0TSC.                                                           lv10
      *SET ADDRESS FOR ACAP                                             DOT
           SET ADDRESS OF XA06 TO                                       ADU015
                PCB-ACAP-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF XB06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF XC06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF XD06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR DATP                                             DOT
           SET ADDRESS OF XF06 TO                                       ADU015
                PCB-DATP-PTR1.                                          ADU015
      *SET ADDRESS FOR ARAY                                             DOT
           SET ADDRESS OF XG06 TO                                       ADU015
                PCB-ARAY-PTR1.                                          ADU015
      *SET ADDRESS FOR CLUY                                             DOT
           SET ADDRESS OF XH06 TO                                       ADU015
                PCB-CLUY-PTR1.                                          ADU015
      *SET ADDRESS FOR GR1P                                             DOT
           SET ADDRESS OF XI06 TO                                       ADU015
                PCB-GR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CH1P                                             DOT
           SET ADDRESS OF XJ06 TO                                       ADU015
                PCB-CH1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CCRP                                             DOT
           SET ADDRESS OF XK06 TO                                       ADU015
                PCB-CCRP-PTR1.                                          ADU015
      *SET ADDRESS FOR CPRP                                             DOT
           SET ADDRESS OF XL06 TO                                       ADU015
                PCB-CPRP-PTR1.                                          ADU015
      *SET ADDRESS FOR CBTP                                             DOT
           SET ADDRESS OF XM06 TO                                       ADU015
                PCB-CBTP-PTR1.                                          ADU015
      *SET ADDRESS FOR CA1P                                             DOT
           SET ADDRESS OF XN06 TO                                       ADU015
                PCB-CA1P-PTR1.                                          ADU015
      *SET ADDRESS FOR TR1P                                             DOT
           SET ADDRESS OF XO06 TO                                       ADU015
                PCB-TR1P-PTR1.                                          ADU015
       F0TSC-FN. EXIT.
      *N01.      NOTE *************************************.            CI0365
      *               *                                   *             CI0365
      *               *INITIALISATIONS                    *             CI0365
      *               *                                   *             CI0365
      *               *************************************.            CI0365
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
      *N02BB.    NOTE *INITIALIZE WORKING STORAGE         *.
       F02BB.                                                           lv10
      *DATABASE SEGMENTS
           INITIALIZE  FR01
           TO01.
       F02BB-FN. EXIT.
      *N02CA.    NOTE *NEEDS TO BE DONE ONLY ONCE         *.
       F02CA.                                                           lv10
      ******************************
           MOVE        EIBTIME TO WS-GETIM
           PERFORM     F92CD THRU F92CD-FN
           MOVE        DD01-UDATE TO WS-DCACG.
       F02CA-FN. EXIT.
      *N02DA.    NOTE *INITIALIZE LINKAGE SEGMENTS        *.
       F02DA.                                                           lv10
      ******************************
           INITIALIZE  DE10
           MS03.
       F02DA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0365
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0365
      *               *                                   *             CI0365
      *               *FIN DE TRAITEMENT                  *             CI0365
      *               *                                   *             CI0365
      *               *************************************.            CI0365
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0365
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N25DB.    NOTE *SAVE THE TRANSACTION DETAILS       *.
       F25DB.                                                           lv10
           MOVE        LK70-CUPIQ TO WS-PROCESS-TRANS
           MOVE        LK70-CPROCM TO WS-ACTION-TRANS
           MOVE        LK70-IMQMG TO WS-CORE-HOURS.
       F25DB-FN. EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *VERIFY ALL THE INPUT FIELDS FOR    *
      *               *                                   *
      *               *************************************.
       F30.                                                             lv05
      *PROCESSING
      *********************************
      *N30BB.    NOTE *VALIDATE SYSTEM RECOGNIZED         *.
       F30BB.    IF    LK70-MAPPN NOT = 'EFU'                           lv10
                 NEXT SENTENCE ELSE GO TO     F30BB-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012734 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30BB-FN. EXIT.
      *N30BE.    NOTE *VALIDATE NUMERIC CTID              *.
       F30BE.    IF    LK70-CTID NOT NUMERIC                            lv10
                 OR    LK70-CTID = ZEROES
                 NEXT SENTENCE ELSE GO TO     F30BE-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013140 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30BE-FN. EXIT.
      *N30BH.    NOTE *ACTION CODE CAN ONLY BE -          *.
       F30BH.    IF    LK70-CPROCM NOT = ('A'                           lv10
                       AND 'D')
                 NEXT SENTENCE ELSE GO TO     F30BH-FN.
      *ADD OR DELETE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012382 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30BH-FN. EXIT.
      *N30BK.    NOTE *UPDATE CODE ONLY BE VERIFY OR      *.
       F30BK.    IF    LK70-CUPIQ NOT = ('E'                            lv10
                       AND 'U')
                 NEXT SENTENCE ELSE GO TO     F30BK-FN.
      *CONFIRM
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012382 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30BK-FN. EXIT.
      *N30BM.    NOTE *VALIDATION FOR DELETE MODE         *.
       F30BM.    IF    DEL-REQUEST                                      lv10
                 NEXT SENTENCE ELSE GO TO     F30BM-FN.
      *N30BP.    NOTE *VALIDATE ACCT ACTIVITY SEQ # IS    *.
       F30BP.    IF    LK70-NAASQ NOT NUMERIC                           lv15
                 OR    LK70-NAASQ = ZERO
                 NEXT SENTENCE ELSE GO TO     F30BP-FN.
      *NUMERIC
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012449 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30BP-FN. EXIT.
      *N30BT.    NOTE *VALIDATE SEQUENCE NUMBER NUMERIC   *.
       F30BT.    IF    LK70-NPISQ NOT NUMERIC                           lv15
                 OR    LK70-NPISQ = ZERO
                 NEXT SENTENCE ELSE GO TO     F30BT-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012821 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30BT-FN. EXIT.
       F30BM-FN. EXIT.
      *N30CA.    NOTE *CHECK FOR PAYMENT FREQUENCY DESC   *.
       F30CA.                                                           lv10
      *NOT VALID-MPMTFL
           MOVE        LK70-MPMTFL TO WS00-MPMTFL.
                 IF    NOT VALID-MPMTFL                                 DOT
                 OR    LK70-MPMTFL = SPACE
      *INVALID VALUE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015606 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CA-FN. EXIT.
      *N30CB.    NOTE *CHECK FOR INTERNAL TRANSFER ONLY   *.
       F30CB.    IF    LK81-CTTYP2 = 'T'                                lv10
                 NEXT SENTENCE ELSE GO TO     F30CB-FN.
      *
      *N30CE.    NOTE *CHECK FOR IRA TYPE CONTRIBUTION    *.
       F30CE.                                                           lv15
           MOVE        LK81-CIRAP TO WS00-CIRAP.
                 IF    NOT VALID-CIRAP                                  DOT
      *INVALID VALUE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013184 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CE-FN. EXIT.
      *N30CJ.    NOTE *VALIDATE NUMERIC TO ACCT NUMBER    *.
       F30CJ.    IF    LK81-CTID NOT NUMERIC                            lv15
                 OR    LK81-CTID = ZEROES
                 NEXT SENTENCE ELSE GO TO     F30CJ-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013140 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CJ-FN. EXIT.
       F30CB-FN. EXIT.
      *N30CL.    NOTE *CHECK THE INPUTS FOR ACH-OUT       *.
       F30CL.    IF    LK81-CTTYP2 = 'A'                                lv10
                 NEXT SENTENCE ELSE GO TO     F30CL-FN.
      *N30CN.    NOTE *CHECK FOR VALID BANK NUMBER        *.
       F30CN.    IF    LK70-NPBN = SPACES                               lv15
                 NEXT SENTENCE ELSE GO TO     F30CN-FN.
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012605 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CN-FN. EXIT.
      *N30CQ.    NOTE *CHECK FOR VALID BANK ACCOUNT       *.
       F30CQ.    IF    LK70-TTBAL = SPACES                              lv15
                 NEXT SENTENCE ELSE GO TO     F30CQ-FN.
      *TYPE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012158 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CQ-FN. EXIT.
      *N30CT.    NOTE *VALIDATE NUMERIC BANK CLIENT ID    *.
       F30CT.    IF    LK70-CLID4 = ZEROES                              lv15
                 NEXT SENTENCE ELSE GO TO     F30CT-FN.
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012612 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CT-FN. EXIT.
      *N30DB.    NOTE *VALIDATE THE DISBURSEMENT AMOUNT   *.
       F30DB.    IF    LK70-ADBRQA < 100                                lv15
                 NEXT SENTENCE ELSE GO TO     F30DB-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014239 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DB-FN. EXIT.
       F30CL-FN. EXIT.
      *N30DF.    NOTE *QUALIFIED ACCOUNT CHECKS           *.
       F30DF.    IF    LK70-IQACT = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F30DF-FN.
      *N30DH.    NOTE *IF WITHHOLDING PERCENT < 0         *.
       F30DH.    IF    LK70-PWHLD < 0                                   lv15
                 NEXT SENTENCE ELSE GO TO     F30DH-FN.
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012175 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DH-FN. EXIT.
      *N30DK.    NOTE *IF WITHHOLDING PERCENT NOT         *.
       F30DK.    IF    LK70-PWHLD >= 1                                  lv15
                 AND   LK70-PWHLD <= 9
                 NEXT SENTENCE ELSE GO TO     F30DK-FN.
      *WITHIN LIMITS
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014600 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DK-FN. EXIT.
      *N30DN.    NOTE *IF WITHHOLDING PERCENT > 99        *.
       F30DN.    IF    LK70-PWHLD > 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F30DN-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014453 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DN-FN. EXIT.
       F30DF-FN. EXIT.
      *N30DT.    NOTE *VALIDATE PERSON CLIENT ID          *.
       F30DT.    IF    LK70-CLID NOT NUMERIC                            lv10
                 OR    LK70-CLID = ZEROES
                 NEXT SENTENCE ELSE GO TO     F30DT-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012612 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DT-FN. EXIT.
       F30-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *SET THE EFFECTIVE DATE BOTH FOR    *
      *               *                                   *
      *               *************************************.
       F35.                                                             lv05
      *ONE-TIME NOW AND ONE-TIME
      *FUTURE TRANSACTION.
      *N35BB.    NOTE *CALL CI0020 - CAMS ACCTG DATE      *.            AM0020
       F35BB.                                                           lv10
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
      *N35BC.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F35BC.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 0)                                  ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F35BC-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0020 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0020 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F35BC-900. GO TO F35BD-FN.
       F35BC-FN. EXIT.
      *N35BD.    NOTE *NO ERRORS                          *.            ADU071
       F35BD.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F35BD-FN. EXIT.
       F35BB-FN. EXIT.
      *N35BF.    NOTE *CHECK WHETHER THE TRANSACTION      *.
       F35BF.    IF    ONE-TIME-FUTURE                                  lv10
                 NEXT SENTENCE ELSE GO TO     F35BF-FN.
      *EFFECTIVE DATE PASSED IS A
      *BUSINESS DAY FOR ONE TIME FUTURE
      *TRANSACTION
           INITIALIZE  WS01
           MOVE        LK70-DEFFT TO WS01-DCACG1
           PERFORM     F92NB THRU F92NB-FN.
      *SET THE EFFECTIVE DATE                                           DOT
           MOVE        WS01-DCACG TO LK70-DEFFT.
       F35BF-FN. EXIT.
      *N35BG.    NOTE *SET EFFECTIVE DATE BASES ON THE    *.
       F35BG.    IF    ONE-TIME-NOW                                     lv10
                 NEXT SENTENCE ELSE GO TO     F35BG-FN.
      *CUT-OFF TIME FOR ONE TIME NOW
      *TRANSACTION
                 IF    LK70-CTIDA = 001                                 DOT
      *CERTS ACH-OUT: 10:00 PM CST
      *CERTS TO BROKERAGE: 10 PM CST
           MOVE        220000 TO WS-GETIM6.
                 IF    LK70-CTIDA = 133                                 DOT
      *BROKERAGE TO CERTS: 3:00 PM CST
           MOVE        150000 TO WS-GETIM6.
      *N35BJ.    NOTE *NEXT ACCTG DAY IF AFTER CUT-OFF    *.
       F35BJ.                                                           lv15
      *TIME OR IF A BUSINESS DAY
      *FOR SATURDAY AND SUNDAY THE DATE
      *WILL BE OF MONDAY
           MOVE        WS-GETIM TO LK70-GETIM
           MOVE        NS20-DCACG TO LK70-DEFFT.
                 IF    WS-GETIM > WS-GETIM6                             DOT
                 AND   NS20-DCACG = WS-DCACG
           MOVE        NS20-DNACG TO LK70-DEFFT.
       F35BJ-FN. EXIT.
       F35BG-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *ADDITIONAL POST VALIDATION         *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *********************************
      *N40BD.    NOTE *FOR ADDING A FA TRANSACTION        *.
       F40BD.    IF    ADD-REQUEST                                      lv10
                 NEXT SENTENCE ELSE GO TO     F40BD-FN.
      *N40CB.    NOTE *CLIENT DB CHECKS                   *.
       F40CB.                                                           lv15
           MOVE        LK70-CLID TO S-CLU01-CLID
           PERFORM     F94EA THRU F94EA-FN.
      *N40CE.    NOTE *CLIENT NOT FOUND                   *.
       F40CE.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F40CE-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012740 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40CE-FN. EXIT.
       F40CB-FN. EXIT.
      *N40DB.    NOTE *CERT OR BETA BROK ACCT DB CHECKS   *.
       F40DB.                                                           lv15
           MOVE        LK70-CTID TO S-FRU01-CTID
           PERFORM     F94DA THRU F94DA-FN.
      *N40DE.    NOTE *CT01 NOT FOUND FOR SOURCE ACCT     *.
       F40DE.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F40DE-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012234 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40DE-FN. EXIT.
       F40DB-FN. EXIT.
      *N40DF.    NOTE *CHECK BANK EXISTS ON CLIENT DB     *.
       F40DF.    IF    LK81-CTTYP2 = 'A'                                lv15
                 NEXT SENTENCE ELSE GO TO     F40DF-FN.
           MOVE        LOW-VALUES TO S-CLU2Y-CL2YK
           MOVE        LK70-CLID4 TO S-CLU2Y-CLID
           MOVE        LK70-NTR TO S-CLU2Y-NTR
           MOVE        'GE' TO S-CLU2Y-OPER
      *READ CL2Y SEGMENT
           PERFORM     F94FA THRU F94FA-FN.
      *N40DH.    NOTE *PROCESS IF BANK CLIENT IS FOUND    *.
       F40DH.                                                           lv20
      *
                 IF    IK = '0'                                         DOT
                 AND   CL2Y-NTR = LK70-NTR
                 AND   CL2Y-CLID = LK70-CLID4
      *IF BANK CLIENT FOUND
      *CONTINUE
           CONTINUE
                 ELSE
      *IF BANK CLIENT NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012612 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40DH-FN. EXIT.
       F40DF-FN. EXIT.
      *N40DI.    NOTE *CERT OR BETA BROK ACCT DB CHECKS   *.
       F40DI.    IF    LK81-CTTYP2 = 'T'                                lv15
                 NEXT SENTENCE ELSE GO TO     F40DI-FN.
           MOVE        LK81-CTID TO S-TOU01-CTID
           PERFORM     F94DB THRU F94DB-FN.
      *N40DJ.    NOTE *CT01 NOT FOUND FOR DESTINATION     *.
       F40DJ.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F40DJ-FN.
      *ACCOUNT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012234 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40DJ-FN. EXIT.
       F40DI-FN. EXIT.
       F40BD-FN. EXIT.
      *N40DM.    NOTE *CALCULATE THE WITHHOLDING AND      *.
       F40DM.    IF    LK70-IQACT = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F40DM-FN.
      *NET AMOUNT QUALIFIED ACCOUNTS
      *
      *CALCULATE NET AMOUNT
           COMPUTE     LK70-ANETTQ ROUNDED =
           (1 - (LK70-PWHLD / 100)) *
           LK70-ADBRQA
      *CALCULATE WITHHOLDING AMOUNT
           COMPUTE     LK70-CTWHAT = LK70-ADBRQA -
           LK70-ANETTQ.
       F40DM-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *********************************   *
      *               *                                   *
      *               *************************************.
       F45.      IF    EDIT-REQUEST                                     lv05
                 NEXT SENTENCE ELSE GO TO     F45-FN.
      *VALIDATION OF BUSINESS RULES FOR
      *VERIFICATION PAGE.
      *********************************
      *N45BD.    NOTE *VALIDATE THE BUSINESS RULES FOR    *.
       F45BD.    IF    LK81-CTTYP2 = 'A' OR 'T'                         lv10
                 NEXT SENTENCE ELSE GO TO     F45BD-FN.
      *CERTS ACH-OUT, 1XTIME CERTS TO
      *BETA BROKERAGE OR 1XTIME BETA
      *BROKERAGE TO CERTS TRANSACTIONS
      *N45DB.    NOTE *CALL CI0273 FOR BUSINESS RULES     *.
       F45DB.                                                           lv15
      *VALIDATION
           MOVE        LK70 TO LM70
           PERFORM     F91FB THRU F91FB-FN.
       F45DB-FN. EXIT.
      *N45DE.    NOTE *CHECK IF ANY BUSINESS RULES HAS    *.
       F45DE.    IF    LM70-IERRC = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F45DE-FN.
      *FAILED
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        LM70-NMESA TO MS03-NMESS2                        ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45DE-FN. EXIT.
       F45BD-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *PROCESS FOR INTERNAL TRANSFER      *
      *               *                                   *
      *               *************************************.
       F50.      IF    LK81-CTTYP2 = 'T'                                lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *N50BA.    NOTE *PERFORM CALLS FOR FROM/TO ACCTS    *.
       F50BA.                                                           lv10
      *CI0003 - OWNERSHIP LINES
      *CI0018 - CLIENTS
      *CI0019 - GROUPS
           PERFORM     F91AF THRU F91AF-FN.
       F50BA-FN. EXIT.
      *N50BC.    NOTE *CALL CI0014 TO DETERMINE IF THE    *.
       F50BC.                                                           lv10
      *TWO ACCOUNTS PASSED HAVE DIFF
      *OWNERSHIPS
           PERFORM     F91CB THRU F91CB-FN.
       F50BC-FN. EXIT.
      *N50BF.    NOTE *DETERMINE TRANSFER TYPE CODE       *.
       F50BF.                                                           lv10
      *********************************
      ** CALL MODULE CI0016 TO        *
      ** DETERMINE THE TRANSFER TYPE  *
      ** CODE BETWEEN THE 2 ACCOUNT   *
      ** NUMBERS.                     *
      *********************************
           INITIALIZE  MS03
           PERFORM     F91AB THRU F91AB-FN
           MOVE        TD16-CTYPE TO LK81-CTYPE
           MOVE        TD16-IOWNC TO LK81-IOWNC.
       F50BF-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *DETERMINE TAX SETTLEMENT CODE,     *
      *               *                                   *
      *               *************************************.
       F55.                                                             lv05
      *CSTD ACCOUNT TYPE CODE AND
      *CSTD PLAN TYPE CODE
      *N55BA.    NOTE *DETERMINE TAX SETTLEMENT CODE      *.
       F55BA.                                                           lv10
      *CALL CI0074
           INITIALIZE  TS00-STAGING-AREA
           MOVE        LK70-CTID TO TS00-CTID
           MOVE        LK70-CLID TO TS00-CLID
           MOVE        NS20-DCACG TO TS00-DCACG
           MOVE        LK70-MAPPN TO TS00-MAPPN
           MOVE        'I' TO TS00-CUPIQ
           MOVE        LK81-CTID TO TS00-CTID01
           MOVE        LK81-CTYPE TO TS00-CTYPE
           PERFORM     F91TS THRU F91TS-FN.
       F55BA-FN. EXIT.
      *N55BD.    NOTE *DETERMINE CSTD ACCOUNT TYPE CODE   *.
       F55BD.                                                           lv10
      *AND CSTD PLAN TYPE CODE BY
      *CALLING CI0108
           INITIALIZE  7-CF00-STAGING
           MOVE        LK70-MAPPN TO 7-CF00-MAPPN
           MOVE        LK70-CTID TO 7-CF00-CTID (1)
           MOVE        LK81-CTID TO 7-CF00-CTID (2)
           PERFORM     F91CF THRU F91CF-FN.
       F55BD-FN. EXIT.
      *N55BF.    NOTE *POPULATE THE VALUE FOR TAX         *.
       F55BF.                                                           lv10
      *SETTLEMENT CODE (CLCUS),
      *CSTD ACCOUNT TYPE CODE (CCACT),
      *CSTD PLAN TYPE CODE (CPLTYP),
      *CSTD TERMINATION FEE IND (ITERF)
      *ANNL CSTD FEE PAID IND (IACFPD)
           MOVE        TS15-CLCUS TO LK81-CLCUS
           MOVE        CF47-CCACT TO LK81-CCACT
           MOVE        CF47-CPLTYP TO LK81-CPLTYP
           MOVE        CF47-ITERF TO LK81-ITERF
           MOVE        CF47-IACFPD TO LK81-IACFPD.
       F55BF-FN. EXIT.
       F55-FN.   EXIT.
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
      *N91.      NOTE *************************************.
      *               *                                   *
      *               *CALLED MODULES                     *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91AB.    NOTE *DETERMINE TRANSFER TYPE CODE       *.
       F91AB.                                                           lv10
      *********************************
      ** CALL MODULE CI0016 TO        *
      ** DETERMINE THE TRANSFER TYPE  *
      ** CODE BETWEEN THE 2 ACCOUNT   *
      ** NUMBERS.                     *
      *********************************
           INITIALIZE  MS03.
      *N91AC.    NOTE *CALL CI0016 - TRANSFER TYPE        *.            AM0016
       F91AC.                                                           lv15
      *                                                                 AM0016
      *********************************                                 AM0016
      ** CI0016 WILL DETERMINE THE    *                                 AM0016
      ** TRANSFER TYPE BETWEEN THE 2  *                                 AM0016
      ** ACCOUNT NUMBERS PASSED.      *                                 AM0016
      ** TD16-CTID WILL CONTAIN THE   *                                 AM0016
      ** 'FROM' ACCOUNT NUMBER.       *                                 AM0016
      ** TD16-CTID01 WILL CONTAIN THE *                                 AM0016
      ** 'TO' ACCOUNT NUMBER.         *                                 AM0016
      ** MODULE CI0014 MUST BE CALLED *                                 AM0016
      ** BEFORE CI0016 TO SET THE     *                                 AM0016
      ** DIFFERENT OWNERSHIP INDICATOR*                                 AM0016
      ** (DO13-IOWNC).  THE TYPE OF   *                                 AM0016
      ** TRANSFER WILL BE RETURNED IN *                                 AM0016
      ** TD16-CTYPE.  THE VALID VALUES*                                 AM0016
      ** FOR TD16-CTYPE ARE:          *                                 AM0016
      **  - T: TRANSFER               *                                 AM0016
      **  - D: DOUBLE TRANSFER        *                                 AM0016
      **  - E: EXCHANGE               *                                 AM0016
      *********************************                                 AM0016
      *                                                                 AM0016
           MOVE        LK70-CTID TO TD16-CTID                           AM0016
           MOVE        LK81-CTID TO TD16-CTID01                         AM0016
           MOVE        NS20-DCACG TO TD16-DCACG                         AM0016
           MOVE        DO13-IOWNC TO TD16-IOWNC                         AM0016
           CALL        CI0016 USING                                     AM0016
           DFHEIBLK                                                     AM0016
           DFHCOMMAREA                                                  AM0016
           TD16                                                         AM0016
           FR01                                                         AM0016
           FA04                                                         AM0016
           FC14                                                         AM0016
           FG15                                                         AM0016
           TO01                                                         AM0016
           TA04                                                         AM0016
           TC14                                                         AM0016
           TG15                                                         AM0016
           MS03                                                         AM0016
           MX11.                                                        AM0016
      *N91AD.    NOTE *NON-DL1 ERROR                      *.            ADU070
       F91AD.    IF    MS03-NMESS2 > ZERO                               lv20
                 AND   MS03-CMESB > 10                                  ADU070
                 NEXT SENTENCE ELSE GO TO     F91AD-FN.                 ADU070
      *OF A CERTAIN SEVERITY                                            ADU070
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU070
           MOVE        CI0016 TO MS03-TMESS4 (IMS03R : 6)               ADU070
           ADD         +7 TO MS03-QELLAA                                ADU070
           MOVE                     ALL '1' TO FT GO TO F20.            ADU070
       F91AD-900. GO TO F91AE-FN.
       F91AD-FN. EXIT.
      *N91AE.    NOTE *NO ERRORS                          *.            ADU070
       F91AE.                                                           lv20
           INITIALIZE  MS03.                                            ADU070
       F91AE-FN. EXIT.
       F91AC-FN. EXIT.
       F91AB-FN. EXIT.
      *N91AF.    NOTE *CALL ROUTINES FOR FROM/TO ACCTS    *.
       F91AF.                                                           lv10
      *********************************
      ** MAKE CALLS TO THE FOLLOWING  *
      ** ROUTINES FOR FROM/TO ACCOUNT *
      **  CI0003 -  OWNER/BENE INFO   *
      **  CI0018 -  ACCOUNT CLIENTS   *
      **  CI0019 -  ACCOUNT GROUPS    *
      *********************************
      *
           INITIALIZE  MS03.
      *N91AH.    NOTE *SOURCE ACCOUNT                     *.
       F91AH.         EXIT.                                             lv15
      *N91AI.    NOTE *CALL CI0003 - ACCT OWNER/BENE      *.            AM0003
       F91AI.                                                           lv20
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
           MOVE        LK70-CTID TO FA04-CTID                           AM0003
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
      *N91AJ.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F91AJ.    IF    (MS03-NMESS2 > ZERO                              lv25
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F91AJ-FN.                 ADU071
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
       F91AJ-900. GO TO F91AK-FN.
       F91AJ-FN. EXIT.
      *N91AK.    NOTE *NO ERRORS                          *.            ADU071
       F91AK.                                                           lv25
           INITIALIZE  MS03.                                            ADU071
       F91AK-FN. EXIT.
       F91AI-FN. EXIT.
      *N91AO.    NOTE *CALL CI0018 - ACCT CLIENTS         *.            AM0018
       F91AO.                                                           lv20
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
           MOVE        LK70-CTID TO FC14-CTID                           AM0018
           MOVE        NS20-DCACG TO FC14-DCACG                         AM0018
           MOVE        25 TO FC14-XIMAX                                 AM0018
           MOVE        'Y' TO FC14-IPOCH                                AM0018
           SET CI0018C-PCB-CT1P-PTR1 TO                                 AM0018
                       PCB-CT1P-PTR1                                    AM0018
           INITIALIZE      DE10-DU03                                    AM0018
           CALL        CI0018 USING                                     AM0018
           DFHEIBLK                                                     AM0018
           DFHCOMMAREA                                                  AM0018
           DLIUIBII                                                     AM0018
           CI0018C-PCB-ADDRESS-LIST                                     AM0018
           FC14                                                         AM0018
           DE10                                                         AM0018
           MS03.                                                        AM0018
      *N91AP.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F91AP.    IF    (MS03-NMESS2 > ZERO                              lv25
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F91AP-FN.                 ADU071
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
       F91AP-900. GO TO F91AQ-FN.
       F91AP-FN. EXIT.
      *N91AQ.    NOTE *NO ERRORS                          *.            ADU071
       F91AQ.                                                           lv25
           INITIALIZE  MS03.                                            ADU071
       F91AQ-FN. EXIT.
       F91AO-FN. EXIT.
      *N91AU.    NOTE *CALL CI0019 - ACCOUNT GROUPS       *.            AM0019
       F91AU.                                                           lv20
      *                                                                 AM0019
      *********************************                                 AM0019
      ** THIS MODULE WILL READ THE    *                                 AM0019
      ** CONTRACT DATABASE TO GET ALL *                                 AM0019
      ** THE GROUPS FOR THE ACCOUNT   *                                 AM0019
      ** NUMBER.                      *                                 AM0019
      *********************************                                 AM0019
      *                                                                 AM0019
           INITIALIZE      FG15                                         AM0019
           MOVE        LK70-CTID TO FG15-CTID                           AM0019
           MOVE        NS20-DCACG TO FG15-DCACG                         AM0019
           MOVE        10 TO FG15-XIMAX                                 AM0019
           MOVE        'Y' TO FG15-IPOCH                                AM0019
           SET CI0019E-PCB-CT1P-PTR1 TO                                 AM0019
                       PCB-CT1P-PTR1                                    AM0019
           SET CI0019E-PCB-GR1P-PTR1 TO                                 AM0019
                       PCB-GR1P-PTR1                                    AM0019
           INITIALIZE      DE10-DU03                                    AM0019
           CALL        CI0019 USING                                     AM0019
           DFHEIBLK                                                     AM0019
           DFHCOMMAREA                                                  AM0019
           DLIUIBII                                                     AM0019
           CI0019E-PCB-ADDRESS-LIST                                     AM0019
           FG15                                                         AM0019
           DE10                                                         AM0019
           MS03.                                                        AM0019
      *N91AV.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F91AV.    IF    (MS03-NMESS2 > ZERO                              lv25
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F91AV-FN.                 ADU071
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
       F91AV-900. GO TO F91AW-FN.
       F91AV-FN. EXIT.
      *N91AW.    NOTE *NO ERRORS                          *.            ADU071
       F91AW.                                                           lv25
           INITIALIZE  MS03.                                            ADU071
       F91AW-FN. EXIT.
       F91AU-FN. EXIT.
       F91AH-FN. EXIT.
      *N91BB.    NOTE *DESTINATION ACCOUNT                *.
       F91BB.         EXIT.                                             lv15
      *N91BI.    NOTE *CALL CI0003 - ACCT OWNER/BENE      *.            AM0003
       F91BI.                                                           lv20
      *                                                                 AM0003
      *********************************                                 AM0003
      ** THIS MODULE WILL READ THE    *                                 AM0003
      ** CONTRACT DATABASE TO GET THE *                                 AM0003
      ** ACCOUNT OWNERSHIP AND        *                                 AM0003
      ** BENEFICIARY LINES FOR THE    *                                 AM0003
      ** REQUESTED ACCOUNT NUMBER.    *                                 AM0003
      *********************************                                 AM0003
      *                                                                 AM0003
           INITIALIZE      TA04                                         AM0003
           MOVE        LK81-CTID TO TA04-CTID                           AM0003
           MOVE        'Y' TO TA04-IPOCH                                AM0003
           SET CI0003B-PCB-CT1P-PTR1 TO                                 AM0003
                       PCB-CT1P-PTR1                                    AM0003
           INITIALIZE      DE10-DU03                                    AM0003
           CALL        CI0003 USING                                     AM0003
           DFHEIBLK                                                     AM0003
           DFHCOMMAREA                                                  AM0003
           DLIUIBII                                                     AM0003
           CI0003B-PCB-ADDRESS-LIST                                     AM0003
           TA04                                                         AM0003
           DE10                                                         AM0003
           MS03.                                                        AM0003
      *N91BJ.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F91BJ.    IF    (MS03-NMESS2 > ZERO                              lv25
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F91BJ-FN.                 ADU071
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
       F91BJ-900. GO TO F91BK-FN.
       F91BJ-FN. EXIT.
      *N91BK.    NOTE *NO ERRORS                          *.            ADU071
       F91BK.                                                           lv25
           INITIALIZE  MS03.                                            ADU071
       F91BK-FN. EXIT.
       F91BI-FN. EXIT.
      *N91BO.    NOTE *CALL CI0018 - ACCT CLIENTS         *.            AM0018
       F91BO.                                                           lv20
      *                                                                 AM0018
      *********************************                                 AM0018
      ** THIS MODULE WILL READ THE    *                                 AM0018
      ** CONTRACT DATABASE TO GET THE *                                 AM0018
      ** TAXPAYER CLIENT ID AND OWNER *                                 AM0018
      ** CLIENT ID'S ASSOCIATED WITH  *                                 AM0018
      ** THE ACCOUNT NUMBER.          *                                 AM0018
      *********************************                                 AM0018
      *                                                                 AM0018
           INITIALIZE      TC14                                         AM0018
           MOVE        LK81-CTID TO TC14-CTID                           AM0018
           MOVE        NS20-DCACG TO TC14-DCACG                         AM0018
           MOVE        25 TO TC14-XIMAX                                 AM0018
           MOVE        'Y' TO TC14-IPOCH                                AM0018
           SET CI0018D-PCB-CT1P-PTR1 TO                                 AM0018
                       PCB-CT1P-PTR1                                    AM0018
           INITIALIZE      DE10-DU03                                    AM0018
           CALL        CI0018 USING                                     AM0018
           DFHEIBLK                                                     AM0018
           DFHCOMMAREA                                                  AM0018
           DLIUIBII                                                     AM0018
           CI0018D-PCB-ADDRESS-LIST                                     AM0018
           TC14                                                         AM0018
           DE10                                                         AM0018
           MS03.                                                        AM0018
      *N91BP.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F91BP.    IF    (MS03-NMESS2 > ZERO                              lv25
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F91BP-FN.                 ADU071
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
       F91BP-900. GO TO F91BQ-FN.
       F91BP-FN. EXIT.
      *N91BQ.    NOTE *NO ERRORS                          *.            ADU071
       F91BQ.                                                           lv25
           INITIALIZE  MS03.                                            ADU071
       F91BQ-FN. EXIT.
       F91BO-FN. EXIT.
      *N91BU.    NOTE *CALL CI0019 - ACCOUNT GROUPS       *.            AM0019
       F91BU.                                                           lv20
      *                                                                 AM0019
      *********************************                                 AM0019
      ** THIS MODULE WILL READ THE    *                                 AM0019
      ** CONTRACT DATABASE TO GET ALL *                                 AM0019
      ** THE GROUPS FOR THE ACCOUNT   *                                 AM0019
      ** NUMBER.                      *                                 AM0019
      *********************************                                 AM0019
      *                                                                 AM0019
           INITIALIZE      TG15                                         AM0019
           MOVE        LK81-CTID TO TG15-CTID                           AM0019
           MOVE        NS20-DCACG TO TG15-DCACG                         AM0019
           MOVE        10 TO TG15-XIMAX                                 AM0019
           MOVE        'Y' TO TG15-IPOCH                                AM0019
           SET CI0019F-PCB-CT1P-PTR1 TO                                 AM0019
                       PCB-CT1P-PTR1                                    AM0019
           SET CI0019F-PCB-GR1P-PTR1 TO                                 AM0019
                       PCB-GR1P-PTR1                                    AM0019
           INITIALIZE      DE10-DU03                                    AM0019
           CALL        CI0019 USING                                     AM0019
           DFHEIBLK                                                     AM0019
           DFHCOMMAREA                                                  AM0019
           DLIUIBII                                                     AM0019
           CI0019F-PCB-ADDRESS-LIST                                     AM0019
           TG15                                                         AM0019
           DE10                                                         AM0019
           MS03.                                                        AM0019
      *N91BV.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F91BV.    IF    (MS03-NMESS2 > ZERO                              lv25
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F91BV-FN.                 ADU071
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
       F91BV-900. GO TO F91BW-FN.
       F91BV-FN. EXIT.
      *N91BW.    NOTE *NO ERRORS                          *.            ADU071
       F91BW.                                                           lv25
           INITIALIZE  MS03.                                            ADU071
       F91BW-FN. EXIT.
       F91BU-FN. EXIT.
       F91BB-FN. EXIT.
       F91AF-FN. EXIT.
      *N91CB.    NOTE *CALL CI0014 - DIFF OWNERSHIP       *.            AM0014
       F91CB.                                                           lv10
      *                                                                 AM0014
      *********************************                                 AM0014
      ** THIS MODULE WILL DETERMINE   *                                 AM0014
      ** IF THE 2 ACCOUNT NUMBERS     *                                 AM0014
      ** PASSED HAVE DIFFERENT        *                                 AM0014
      ** OWNERSHIPS.                  *                                 AM0014
      *********************************                                 AM0014
      *                                                                 AM0014
           INITIALIZE  DO13                                             AM0014
           MOVE        'UD' TO DO13-MAPPN
           MOVE        LK70-CTID TO DO13-CTID                           AM0014
           MOVE        LK81-CTID TO DO13-CTID01                         AM0014
           MOVE        NS20-DCACG TO DO13-DCACG                         AM0014
           CALL        CI0014 USING                                     AM0014
           DFHEIBLK                                                     AM0014
           DFHCOMMAREA                                                  AM0014
           DO13                                                         AM0014
           FR01                                                         AM0014
           FA04                                                         AM0014
           FC14                                                         AM0014
           FG15                                                         AM0014
           TO01                                                         AM0014
           TA04                                                         AM0014
           TC14                                                         AM0014
           TG15                                                         AM0014
           MS03                                                         AM0014
           MX11.                                                        AM0014
      *N91CC.    NOTE *NON-DL1 ERROR                      *.            ADU070
       F91CC.    IF    MS03-NMESS2 > ZERO                               lv15
                 AND   MS03-CMESB > 10                                  ADU070
                 NEXT SENTENCE ELSE GO TO     F91CC-FN.                 ADU070
      *OF A CERTAIN SEVERITY                                            ADU070
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU070
           MOVE        CI0014 TO MS03-TMESS4 (IMS03R : 6)               ADU070
           ADD         +7 TO MS03-QELLAA                                ADU070
           MOVE                     ALL '1' TO FT GO TO F20.            ADU070
       F91CC-900. GO TO F91CD-FN.
       F91CC-FN. EXIT.
      *N91CD.    NOTE *NO ERRORS                          *.            ADU070
       F91CD.                                                           lv15
           INITIALIZE  MS03.                                            ADU070
       F91CD-FN. EXIT.
       F91CB-FN. EXIT.
      *N91CF.    NOTE *REQUEST CUST/TERM FEES             *.            AM0108
       F91CF.                                                           lv10
      *********************************                                 AM0108
      ** THIS MODULE WILL REQUEST THE *                                 AM0108
      ** CI0108 MODULE TO RETURN ANY  *                                 AM0108
      ** APPROPRIATE CUST/TERM FEES   *                                 AM0108
      *********************************                                 AM0108
           INITIALIZE  CF47                                             AM0108
           MOVE        7-CF00-MAPPN TO CF47-MAPPN                       AM0108
           MOVE        7-CF00-CTID (1) TO CF47-CTID (1)                 AM0108
           MOVE        7-CF00-CTID (2) TO CF47-CTID (2)                 AM0108
           MOVE        7-CF00-CSPDT TO CF47-CSPDT                       AM0108
           MOVE        7-CF00-CPAYF TO CF47-CPAYF                       AM0108
           SET CI0108-PCB-CL1P-PTR1 TO                                  AM0108
                      PCB-CL1P-PTR1                                     AM0108
           SET CI0108-PCB-CT1P-PTR1 TO                                  AM0108
                      PCB-CT1P-PTR1                                     AM0108
           SET CI0108-PCB-GR1P-PTR1 TO                                  AM0108
                      PCB-GR1P-PTR1                                     AM0108
           SET CI0108-PCB-ACAP-PTR1 TO                                  AM0108
                      PCB-ACAP-PTR1                                     AM0108
           INITIALIZE  DE10-DU03                                        AM0108
           CALL        CI0108 USING                                     AM0108
           DFHEIBLK                                                     AM0108
           DFHCOMMAREA                                                  AM0108
           DLIUIBII                                                     AM0108
           CI0108-PCB-ADDR-LIST                                         AM0108
           NS20                                                         AM0108
           CF47                                                         AM0108
           DE10                                                         AM0108
           MS03                                                         AM0108
           MX11.                                                        AM0108
      *N91CG.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F91CG.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F91CG-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0108 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0108 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F91CG-900. GO TO F91CH-FN.
       F91CG-FN. EXIT.
      *N91CH.    NOTE *NO ERRORS                          *.            ADU071
       F91CH.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F91CH-FN. EXIT.
       F91CF-FN. EXIT.
      *N91FB.    NOTE *VALIDATE ONE-TIME TRANS AMT        *.            AM0273
       F91FB.                                                           lv10
      *********************************                                 AM0273
      ** CALL CI0273 PROGRAM          *                                 AM0273
      *********************************                                 AM0273
           SET CI0273-LK-PCB-CT1P-PTR1 TO                               AM0273
                         PCB-CT1P-PTR1                                  AM0273
           SET CI0273-LK-PCB-CL1P-PTR1 TO                               AM0273
                         PCB-CL1P-PTR1                                  AM0273
           SET CI0273-LK-PCB-ACAP-PTR1 TO                               AM0273
                         PCB-ACAP-PTR1                                  AM0273
           SET CI0273-LK-PCB-CH1P-PTR1 TO                               AM0273
                         PCB-CH1P-PTR1                                  AM0273
           SET CI0273-LK-PCB-CCRP-PTR1 TO                               AM0273
                         PCB-CCRP-PTR1                                  AM0273
           SET CI0273-LK-PCB-CPRP-PTR1 TO                               AM0273
                         PCB-CPRP-PTR1                                  AM0273
           SET CI0273-LK-PCB-CBTP-PTR1 TO                               AM0273
                         PCB-CBTP-PTR1                                  AM0273
           SET CI0273-LK-PCB-CA1P-PTR1 TO                               AM0273
                         PCB-CA1P-PTR1                                  AM0273
      *                                                                 AM0273
           CALL        CI0273 USING                                     AM0273
           DFHEIBLK                                                     AM0273
           DFHCOMMAREA                                                  AM0273
           DLIUIBII                                                     AM0273
           CI0273-LK-PCB-ADDR-LIST                                      AM0273
           LM70                                                         AM0273
           DE10                                                         AM0273
           MS03                                                         AM0273
           MX11.                                                        AM0273
      *N91FC.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F91FC.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F91FC-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0273 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0273 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F91FC-900. GO TO F91FD-FN.
       F91FC-FN. EXIT.
      *N91FD.    NOTE *NO ERRORS                          *.            ADU071
       F91FD.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F91FD-FN. EXIT.
       F91FB-FN. EXIT.
      *N91TS.    NOTE *CALL CI0074                        *.            AM0074
       F91TS.                                                           lv10
      *********************************                                 AM0074
      *DETERMINE CUSTODIAL (TAX)                                        AM0074
      *SETTLEMENT CODE (CLCUS)                                          AM0074
      *********************************                                 AM0074
           INITIALIZE  TS15                                             AM0074
           DE10-DU03                                                    AM0074
           MOVE        TS00-CTID TO TS15-CTID                           AM0074
           MOVE        TS00-CLID TO TS15-CLID                           AM0074
           MOVE        TS00-CLCUS TO TS15-CLCUS                         AM0074
           MOVE        TS00-DCACG TO TS15-DCACG                         AM0074
           MOVE        TS00-CAUNIT TO TS15-CAUNIT                       AM0074
           MOVE        TS00-GEOPID TO TS15-GEOPID                       AM0074
           MOVE        TS00-MAPPN TO TS15-MAPPN                         AM0074
           MOVE        TS00-CUPIQ TO TS15-CUPIQ                         AM0074
           MOVE        TS00-CTID01 TO TS15-CTID01                       AM0074
           MOVE        TS00-CTYPE TO TS15-CTYPE                         AM0074
           MOVE        TS00-CPAYF TO TS15-CPAYF                         AM0074
           SET CI0074-PCB-CT1P-PTR1 TO                                  AM0074
                      PCB-CT1P-PTR1                                     AM0074
           SET CI0074-PCB-CL1P-PTR1 TO                                  AM0074
                      PCB-CL1P-PTR1                                     AM0074
           SET CI0074-PCB-GR1P-PTR1 TO                                  AM0074
                      PCB-GR1P-PTR1                                     AM0074
           SET CI0074-PCB-TR1P-PTR1 TO                                  AM0074
                      PCB-TR1P-PTR1                                     AM0074
           CALL        CI0074 USING                                     AM0074
           DFHEIBLK                                                     AM0074
           DFHCOMMAREA                                                  AM0074
           DLIUIBII                                                     AM0074
           CI0074-PCB-ADDRESS-LIST                                      AM0074
           TS15                                                         AM0074
           FC14                                                         AM0074
           TC14                                                         AM0074
           DE10                                                         AM0074
           MS03                                                         AM0074
           MX11.                                                        AM0074
      *N91TT.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F91TT.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F91TT-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0074 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0074 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F91TT-900. GO TO F91TU-FN.
       F91TT-FN. EXIT.
      *N91TU.    NOTE *NO ERRORS                          *.            ADU071
       F91TU.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F91TU-FN. EXIT.
       F91TS-FN. EXIT.
       F91-FN.   EXIT.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *DATE CONVERSION                    *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92BR.    NOTE *CDU - DATE DIFF/CALCULATION        *.            AADA82
       F92BR.                                                           lv10
      ** * * * * * * * * * * * * * * *                                  AADA82
      *This code calls the common date                                  AADA82
      *utility MWS100EX to calculate                                    AADA82
      *the difference between 2 dates                                   AADA82
      *or calculate a new date (add/                                    AADA82
      *subtract days). It uses a                                        AADA82
      *dynamic call.                                                    AADA82
      ** * * * * * * * * * * * * * * *                                  AADA82
      *Before the call set the subfunc                                  AADA82
      *request code DF30-CDTSF:                                         AADA82
      *  8 = date difference                                            AADA82
      *  9 = date add/subtract days                                     AADA82
      ** * * * * * * * * * * * * * * *                                  AADA82
      *Check return code DF30-CDTSC                                     AADA82
      *after the call.                                                  AADA82
      *    0 = Error Free                                               AADA82
      *    3 = Invalid Date                                             AADA82
      *    5 = Invalid Day                                              AADA82
      *    6 = Invalid Month                                            AADA82
      ** * * * * * * * * * * * * * * *                                  AADA82
           MOVE        4 TO DF30-CDTFN                                  AADA82
           CALL        MWS100EX USING DF30                              AADA82
           DF34.                                                        AADA82
       F92BR-FN. EXIT.
      *N92CD.    NOTE *DATE CONVERSION                    *.
       F92CD.                                                           lv10
           EXEC CICS   ASKTIME ABSTIME (DD01-XMSTS)          END-EXEC.  ADU155
           EXEC CICS   FORMATTIME ABSTIME (DD01-XMSTS)                  ADU155
                       YYMMDD (DD01-XDAT69)                             ADU155
                       YEAR (DD01-F2CCYY)                    END-EXEC.  ADU155
           COMPUTE     DD01-YEAR = DD01-F2CCYY                          ADU155
      ** MOVE DD01-UDATE TO YOUR FIELD                                  ADU155
           MOVE        DD01-XDAT69 (3:4) TO DD01-MMDD.                  ADU155
       F92CD-FN. EXIT.
      *N92NA.    NOTE *VALIDATE OR GET NEXT ACCTG DATE    *.
       F92NA.                                                           lv10
           MOVE        WS01-DCACG TO 7-XX01-PCKDAT                      $AACTG
           COMPUTE     7-XX01-PUDAT =                                   $AACTG
           (7-XX01-PCKDAT * 10)                                         $AACTG
           MOVE        7-XX01-UNSDAT TO 7-XX01-ICURR                    $AACTG
           CALL        7-XX01-DATMOD USING                              $AACTG
           7-XX01-IDTFLD                                                $AACTG
           7-XX01-RDTFLD                                                $AACTG
           MOVE        7-XX01-RCDATE TO 7-XX01-CHKDAT.                  $AACTG
                 IF    7-XX01-CHKPDT = +177607040                       DOT
           MOVE        ZEROES TO WS01-DNACG                             $AACTG
                 ELSE                                                   $AACTG
           MOVE        7-XX01-RNDATE TO 7-XX01-CHKDAT                   $AACTG
           COMPUTE     7-XX01-NEXTDT =                                  $AACTG
           (7-XX01-CHKPDT / 10)                                         $AACTG
           MOVE        7-XX01-NEXTDT TO WS01-DNACG.                     $AACTG
       F92NA-FN. EXIT.
      *N92NB.    NOTE *CHECK WHETHER THE TRANSACTION      *.
       F92NB.                                                           lv10
      *EFFECTIVE DATE PASSED IS A
      *BUSINESS DAY
           INITIALIZE  DF34
           MOVE        ZEROES TO WS01-DCACG
           MOVE        ZEROES TO WS01-DNACG
           MOVE        WS01-DCACG1 TO WS01-DCACG
           MOVE        +0 TO DF34-NDTUN
           PERFORM     F92NA THRU F92NA-FN.
      *N92NE.    NOTE *CONVERT TO THE NEXT BUSINESS DAY   *.
       F92NE.    IF    WS01-DNACG = ZEROES                              lv15
                 NEXT SENTENCE ELSE GO TO     F92NE-FN.
      *IF THE DATE IS NOT AN ACCOUNTG
      *DATE
           ADD         +1 TO DF34-NDTUN
           MOVE        WS01-DCACG1 TO DF34-DTGRGA
           MOVE        0 TO DF34-CDTUC
           MOVE        9 TO DF30-CDTSF
           PERFORM     F92BR THRU F92BR-FN.
      *N92NI.    NOTE *CHECK WHETHER THE NEXT DATE IS A   *.
       F92NI.                                                           lv20
      *BUSINESS DAY
           MOVE        DF34-DTGRGB TO WS01-DCACG
           PERFORM     F92NA THRU F92NA-FN.
       F92NI-FN. EXIT.
       F92NE-900. GO TO F92NE.
       F92NE-FN. EXIT.
       F92NB-FN. EXIT.
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
      *N94.      NOTE *************************************.
      *               *                                   *
      *               *DATABASE ACCESS CALLS              *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94DA.    NOTE *CALL GU ON FR01                    *.            ADU026
       F94DA.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'FR01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XC06 FR01                                                    ADU026
           S-FRU01-SSA                                                  ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DA-FN. EXIT.
      *N94DB.    NOTE *CALL GU ON TO01                    *.            ADU026
       F94DB.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'TO01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XC06 TO01                                                    ADU026
           S-TOU01-SSA                                                  ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DB-FN. EXIT.
      *N94EA.    NOTE *CALL GU ON CL01                    *.            ADU026
       F94EA.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XB06 CL01                                                    ADU026
           S-CLU01-SSA                                                  ADU026
           MOVE        XB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94EA-FN. EXIT.
      *N94FA.    NOTE *CALL GU ON CL2Y                    *.            ADU026
       F94FA.                                                           lv10
           MOVE        'CLUY' TO DE10-XDBDNM                            ADU026
           MOVE        'CL2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XH06 CL2Y                                                    ADU026
           S-CLU2Y-SSA                                                  ADU026
           MOVE        XH06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94FA-FN. EXIT.
       F94-FN.   EXIT.
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
