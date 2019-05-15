       IDENTIFICATION DIVISION.                                         CI0082
       PROGRAM-ID.  CI0082P.                                            CI0082
      *AUTHOR.         M\M - VALIDATE SOURCE ACCOUNT.                   CI0082
      *DATE-COMPILED.   09/08/14.                                       CI0082
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
       ENVIRONMENT DIVISION.                                            CI0082
       CONFIGURATION SECTION.                                           CI0082
       SOURCE-COMPUTER. IBM-370.                                        CI0082
       OBJECT-COMPUTER. IBM-370.                                        CI0082
       DATA DIVISION.                                                   CI0082
       WORKING-STORAGE SECTION.                                         CI0082
      *
      ******************************************************************
      **     CI0018 - SEGMENT OF PARAMETERS     NEEDED TO GET THE      *
      **     REQUESTED TYPE OF CLIENTS FOR THE ACCOUNT NUMBER PASSED.  *
      ******************************************************************
      *
      *DP: AC  DL: DU SEL: 14______ PICT: I DESC: _ LEV: 1 ORG: _ SS: _
      *
      *
       01                 CL01.                                         CI0082
            10            CL01-CL01K.                                   CI0082
            11            CL01-C199.                                    CI0082
            12            CL01-CLID.                                    CI0082
            13            CL01-CLIDO  PICTURE  9(3).                    CI0082
            13            CL01-CLIDN.                                   CI0082
            14            CL01-CLIDNP PICTURE  X(12).                   CI0082
            14            CL01-CLIDND PICTURE  9(8).                    CI0082
            10            CL01-GECKD  PICTURE  9.                       CI0082
            10            CL01-GEMDA  PICTURE  9(8).                    CI0082
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0082
                          BINARY.                                       CI0082
            10            CL01-GECUC  PICTURE  99.                      CI0082
            10            CL01-CLDOR  PICTURE  9(8).                    CI0082
            10            CL01-CLLNG  PICTURE  XX.                      CI0082
            10            CL01-GESLC  PICTURE  99.                      CI0082
            10            CL01-CLTYP  PICTURE  X.                       CI0082
            10            CL01-CLCLS  PICTURE  9(3).                    CI0082
            10            CL01-CLTWRC PICTURE  99.                      CI0082
            10            CL01-CLPVC  PICTURE  99.                      CI0082
            10            CL01-CLIND  PICTURE  9(3).                    CI0082
            10            CL01-CLTRC  PICTURE  99.                      CI0082
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            10            CL01-AYSIDA PICTURE  9(3).                    CI0082
            10            CL01-AYSID  PICTURE  9(5).                    CI0082
            10            CL01-CLSTR  PICTURE  9(2).                    CI0082
            10            CL01-CLC11  PICTURE  X.                       CI0082
            10            CL01-CLTIN  PICTURE  9(12).                   CI0082
            10            CL01-CLTND  PICTURE  9(8).                    CI0082
            10            CL01-CLTINC PICTURE  9.                       CI0082
            10            CL01-CCDWA  PICTURE  9.                       CI0082
            10            CL01-CICES  PICTURE  X.                       CI0082
            10            CL01-CLTRA  PICTURE  9(2).                    CI0082
            10            CL01-DIRSY  PICTURE  9(4)                     CI0082
                          COMPUTATIONAL-3.                              CI0082
            10            CL01-CFEDS  PICTURE  X.                       CI0082
            10            CL01-FILLER PICTURE  X(06).                   CI0082
       01                 CL03.                                         CI0082
            10            CL03-GEDLA  PICTURE  9(8).                    CI0082
            10            CL03-DDREP  PICTURE  9(8).                    CI0082
            10            CL03-DPRFR  PICTURE  9(8).                    CI0082
            10            CL03-IACCI  PICTURE  X.                       CI0082
            10            CL03-CLDOB  PICTURE  9(8).                    CI0082
            10            CL03-CLDOD  PICTURE  9(8).                    CI0082
            10            CL03-CLDTH  PICTURE  X.                       CI0082
            10            CL03-CCINI  PICTURE  X.                       CI0082
            10            CL03-FILLER PICTURE  X(1).                    CI0082
            10            CL03-CLAIN  PICTURE  S9(11)                   CI0082
                          COMPUTATIONAL-3.                              CI0082
            10            CL03-CCAOD  PICTURE  999.                     CI0082
            10            CL03-CLMAR  PICTURE  X.                       CI0082
            10            CL03-C198.                                    CI0082
            11            CL03-CLNAM.                                   CI0082
            12            CL03-CLNAMH PICTURE  X(6).                    CI0082
            12            CL03-CLNAMF PICTURE  X(20).                   CI0082
            12            CL03-CLNAMM.                                  CI0082
            13            CL03-CLNAMI PICTURE  X.                       CI0082
            13            CL03-CLNAMR PICTURE  X(14).                   CI0082
            12            CL03-CLNAML PICTURE  X(25).                   CI0082
            12            CL03-CLNAMS PICTURE  X(4).                    CI0082
            10            CL03-FILLER PICTURE  X(10).                   CI0082
            10            CL03-MPRFS  PICTURE  X(4).                    CI0082
            10            CL03-CLOCC  PICTURE  9(3).                    CI0082
            10            CL03-CLRET  PICTURE  X.                       CI0082
            10            CL03-IOCOB  PICTURE  X.                       CI0082
            10            CL03-CLSEX  PICTURE  X.                       CI0082
            10            CL03-CLWIL  PICTURE  X.                       CI0082
            10            CL03-GECFC  PICTURE  99.                      CI0082
            10            CL03-GECFY  PICTURE  9(4).                    CI0082
            10            CL03-ICUSC  PICTURE  X.                       CI0082
            10            CL03-MCTYC  PICTURE  X(20).                   CI0082
            10            CL03-CLWIP  PICTURE  X.                       CI0082
            10            CL03-CLCTXF PICTURE  99.                      CI0082
            10            CL03-CLCUS  PICTURE  99.                      CI0082
            10            CL03-NPDLU  PICTURE  9(5).                    CI0082
            10            CL03-CLEMI  PICTURE  X.                       CI0082
            10            CL03-GEPHNH PICTURE  X(14).                   CI0082
            10            CL03-GEPHNB PICTURE  X(14).                   CI0082
            10            CL03-GEPHNX PICTURE  9(4).                    CI0082
            10            CL03-GEPHNA PICTURE  X(14).                   CI0082
            10            CL03-FILLER PICTURE  X(3).                    CI0082
            10            CL03-IAPRT  PICTURE  X.                       CI0082
            10            CL03-CEMSC  PICTURE  X.                       CI0082
            10            CL03-CSEPS  PICTURE  X.                       CI0082
            10            CL03-CRACE  PICTURE  X.                       CI0082
            10            CL03-CNIRA  PICTURE  X.                       CI0082
            10            CL03-FILLER PICTURE  X(11).                   CI0082
       01                 CL12.                                         CI0082
            10            CL12-GEDLA  PICTURE  9(8).                    CI0082
            10            CL12-CLBCD  PICTURE  9(3).                    CI0082
            10            CL12-CLFDW  PICTURE  X.                       CI0082
            10            CL12-CLOSD  PICTURE  9(8).                    CI0082
            10            CL12-CLOED  PICTURE  9(8).                    CI0082
            10            CL12-CLOEI  PICTURE  X.                       CI0082
            10            CL12-CLIBN  PICTURE  X(20).                   CI0082
            10            CL12-CLINT  PICTURE  9(3).                    CI0082
            10            CL12-CLONE  PICTURE  9(9).                    CI0082
            10            CL12-CLORC  PICTURE  99.                      CI0082
            10            CL12-CLORN  PICTURE  X(45).                   CI0082
            10            CL12-CLORP  PICTURE  X(25).                   CI0082
            10            CL12-GEPHNB PICTURE  X(14).                   CI0082
            10            CL12-GEPHNX PICTURE  9(4).                    CI0082
            10            CL12-GEPHNA PICTURE  X(14).                   CI0082
            10            CL12-GEFYE  PICTURE  9(4).                    CI0082
            10            CL12-AYCDE  PICTURE  9(3).                    CI0082
            10            CL12-AYID   PICTURE  9(5).                    CI0082
            10            CL12-CFOBO  PICTURE  99.                      CI0082
            10            CL12-CLINRG                                   CI0082
                          OCCURS       003     TIMES.                   CI0082
            11            CL12-CLIRT  PICTURE  99.                      CI0082
            11            CL12-CLINR  PICTURE  X(3).                    CI0082
            11            CL12-CLIRD  PICTURE  9(8).                    CI0082
            10            CL12-IOTXE  PICTURE  X.                       CI0082
            10            CL12-IO501  PICTURE  X.                       CI0082
            10            CL12-IOFOG  PICTURE  X.                       CI0082
            10            CL12-IOPRA  PICTURE  X.                       CI0082
            10            CL12-IOSCS  PICTURE  X.                       CI0082
            10            CL12-IACHA  PICTURE  X.                       CI0082
            10            CL12-IFORG  PICTURE  X.                       CI0082
            10            CL12-IFIND  PICTURE  X.                       CI0082
            10            CL12-CFCNT3 PICTURE  X(2).                    CI0082
            10            CL12-FILLER PICTURE  X(06).                   CI0082
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0003           PIC X(8) VALUE 'CI0003P '.                  AM0003
       01  CI0004           PIC X(8) VALUE 'CI0004P '.                  AM0004
       01  CI0018           PIC X(8) VALUE 'CI0018P '.                  AM0018
       01  CI0019           PIC X(8) VALUE 'CI0019P '.                  AM0019
       01  CI0020           PIC X(8) VALUE 'CI0020P '.                  AM0020
      ******************************************************************AM0081
      *        CI0081 I/O FIELDS                                       *AM0081
      ******************************************************************AM0081
                                                                        AM0081
      *!WF DSP=WZ DSL=WZ SEL=05 FOR=I LEV=1                             AM0081
       01                 WZ00.                                         CI0082
          05              WZ00-00.                                      CI0082
            10            WZ00-DCACG  PICTURE  9(8).                    CI0082
            10            WZ00-GEAUN  PICTURE  9(5).                    CI0082
            10            WZ00-GEOPD2 PICTURE  X(8).                    CI0082
            10            WZ00-CAATY  PICTURE  9(3).                    CI0082
            10            WZ00-NBTCH  PICTURE  9(4).                    CI0082
            10            WZ00-CTIDA  PICTURE  9(3).                    CI0082
            10            WZ00-CTIDNP PICTURE  X(13).                   CI0082
            10            WZ00-CTIDND PICTURE  9(11).                   CI0082
            10            WZ00-GECKD  PICTURE  9.                       CI0082
            10            WZ00-PRCOD  PICTURE  9(5).                    CI0082
            10            WZ00-PRSCD  PICTURE  X(9).                    CI0082
            10            WZ00-ICUST  PICTURE  X.                       CI0082
            10            WZ00-NAASQ  PICTURE  S9(3)                    CI0082
                          COMPUTATIONAL-3.                              CI0082
            10            WZ00-CRECTU PICTURE  99.                      CI0082
          05              WZ00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(00165).                  CI0082
       01                 WZ05  REDEFINES      WZ00.                    CI0082
            10       FILLER         PICTURE  X(00075).                  CI0082
            10            WZ05-MAPPN  PICTURE  X(10).                   CI0082
            10            WZ05-CTID   PICTURE  X(27).                   CI0082
            10            WZ05-FILLER PICTURE  X(50).                   CI0082
            10            WZ05-CLID   PICTURE  X(23).                   CI0082
            10            WZ05-GECSQ  PICTURE  S9(3)                    CI0082
                          COMPUTATIONAL-3.                              CI0082
            10            WZ05-ICLID  PICTURE  X.                       CI0082
            10            WZ05-NARRS  PICTURE  S9(3)                    CI0082
                          COMPUTATIONAL-3.                              CI0082
            10            WZ05-FILLER PICTURE  X(50).                   CI0082
                                                                        AM0081
       01 CI0081                    PIC X(8) VALUE 'CI0081P'.           AM0081
      ******************************************************************AM0081
      *        PCB ADDRESS LIST FOR CI0052.  PCB'S ARE NEEDED FOR      *AM0081
      *        CAMS CONTRACT (CT1P) FOR CT01, CT07, CT09;              *AM0081
      *        CAMS CLIENT (CL1P) FOR CL01, CL24;                      *AM0081
      *        CATS ARRANGEMENT (AR1P) FOR CX01,CX03; AND              *AM0081
      *        CATS ARRANGEMENT INDEX (ARAY) FOR CX2Y.                 *AM0081
      ******************************************************************AM0081
                                                                        AM0081
       01  CI0081D-PCB-ADDRESS-LIST.                                    AM0081
           05  CI0081D-PCB-CT1P-PTR1        POINTER.                    AM0081
           05  CI0081D-PCB-CL1P-PTR1        POINTER.                    AM0081
           05  CI0081D-PCB-AR1P-PTR1        POINTER.                    AM0081
           05  CI0081D-PCB-ARAY-PTR1        POINTER.                    AM0081
       01                 CT01.                                         CI0082
            10            CT01-CT01K.                                   CI0082
            11            CT01-C299.                                    CI0082
            12            CT01-CTID.                                    CI0082
            13            CT01-CTIDA  PICTURE  9(3).                    CI0082
            13            CT01-CTIDN.                                   CI0082
            14            CT01-CTIDNP PICTURE  X(13).                   CI0082
            14            CT01-CTIDND PICTURE  9(11).                   CI0082
            10            CT01-GECKD  PICTURE  9.                       CI0082
            10            CT01-GEMDA  PICTURE  9(8).                    CI0082
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0082
                          BINARY.                                       CI0082
            10            CT01-GECUC  PICTURE  99.                      CI0082
            10            CT01-CTAUL  PICTURE  9(3).                    CI0082
            10            CT01-DIRAC  PICTURE  9(4).                    CI0082
            10            CT01-CTCCI  PICTURE  X.                       CI0082
            10            CT01-CTCUS  PICTURE  999.                     CI0082
            10            CT01-CTEFD  PICTURE  9(8).                    CI0082
            10            CT01-CTIAD  PICTURE  9(8).                    CI0082
            10            CT01-CLCUS  PICTURE  99.                      CI0082
            10            CT01-CAMMB  PICTURE  X(3).                    CI0082
            10            CT01-CKPMM  PICTURE  X.                       CI0082
            10            CT01-CTLAD  PICTURE  9(8).                    CI0082
            10            CT01-IPERS  PICTURE  X.                       CI0082
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            10            CT01-CTLAT  PICTURE  9(8).                    CI0082
            10            CT01-CTLATC PICTURE  9(6).                    CI0082
            10            CT01-IMEGA  PICTURE  X.                       CI0082
            10            CT01-DIRAB  PICTURE  9(8).                    CI0082
            10            CT01-COLRQ  PICTURE  X.                       CI0082
            10            CT01-ZDA04  PICTURE  X(4).                    CI0082
            10            CT01-CTLPD  PICTURE  9(8).                    CI0082
            10            CT01-CIRASP PICTURE  9.                       CI0082
            10            CT01-CIRATP PICTURE  99.                      CI0082
            10            CT01-DRTHC  PICTURE  9(8).                    CI0082
            10            CT01-CPPTC  PICTURE  X.                       CI0082
            10            CT01-ZDA06  PICTURE  X(6).                    CI0082
            10            CT01-CTACD  PICTURE  9(8).                    CI0082
            10            CT01-CTNLI  PICTURE  X.                       CI0082
            10            CT01-CTRHO  PICTURE  9(8).                    CI0082
            10            CT01-CTSGD  PICTURE  9(8).                    CI0082
            10            CT01-CPATP  PICTURE  X(1).                    CI0082
            10            CT01-IRSTA  PICTURE  X.                       CI0082
            10            CT01-CTSTA  PICTURE  99.                      CI0082
            10            CT01-CTSSC  PICTURE  99.                      CI0082
            10            CT01-PRLIN  PICTURE  9(3).                    CI0082
            10            CT01-PRCOD  PICTURE  9(5).                    CI0082
            10            CT01-PRSCD  PICTURE  X(9).                    CI0082
            10            CT01-CTLNI  PICTURE  X.                       CI0082
            10            CT01-AYSIDA PICTURE  9(3).                    CI0082
            10            CT01-AYSID  PICTURE  9(5).                    CI0082
            10            CT01-CTBMC  PICTURE  99.                      CI0082
            10            CT01-CINAR  PICTURE  99.                      CI0082
            10            CT01-CPHTR  PICTURE  X.                       CI0082
            10            CT01-CDSTR  PICTURE  XX.                      CI0082
            10            CT01-CQACT  PICTURE  999.                     CI0082
            10            CT01-CIRAS  PICTURE  999.                     CI0082
            10            CT01-CIRAT  PICTURE  999.                     CI0082
            10            CT01-CLRAY  PICTURE  9(5).                    CI0082
            10            CT01-CATTP  PICTURE  X.                       CI0082
       01                 CT17.                                         CI0082
            10            CT17-GELL   PICTURE  9(4)                     CI0082
                          BINARY.                                       CI0082
            10            CT17-CT17K.                                   CI0082
            11            CT17-NSUBA  PICTURE  9(15).                   CI0082
            10            CT17-NCGRP                                    CI0082
                          REDEFINES            CT17-CT17K               CI0082
               PICTURE    99.                                           CI0082
            10            CT17-IBASE  PICTURE  X.                       CI0082
            10            CT17-C280.                                    CI0082
            11            CT17-FILLER PICTURE  X(178).                  CI0082
            10            CT17-C281                                     CI0082
                          REDEFINES            CT17-C280.               CI0082
            11            CT17-AMFYP  PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AMFYP1 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-PRATE  PICTURE  9(3)V99.                 CI0082
            11            CT17-IREPL  PICTURE  X.                       CI0082
            11            CT17-PREPL  PICTURE  9(3).                    CI0082
            11            CT17-NSCHI  PICTURE  999.                     CI0082
            11            CT17-AFSTB  PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ASNDB  PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APRDC  PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AAPCA1 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AAPCP  PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CEABL  PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CEABL1 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CEABL2 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ACOMT1 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APRCDD PICTURE  S9(11)                   CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ICALC  PICTURE  X.                       CI0082
            11            CT17-IFRNT  PICTURE  X.                       CI0082
            11            CT17-ABMFB  PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ABMFB1 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ABPMB  PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ABPMB1 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-IANNP  PICTURE  X.                       CI0082
            11            CT17-ICNVR  PICTURE  X.                       CI0082
            11            CT17-GESTD  PICTURE  9(8).                    CI0082
            11            CT17-GEEND  PICTURE  9(8).                    CI0082
            11            CT17-AECMN  PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APPRM  PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ARCUA  PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-NSCHIG PICTURE  999.                     CI0082
            11            CT17-AECMP  PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AECPB  PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CCFEE  PICTURE  X.                       CI0082
            10            CT17-C282                                     CI0082
                          REDEFINES            CT17-C280.               CI0082
            11            CT17-NSCHI7 PICTURE  999.                     CI0082
            11            CT17-ACOMT3 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APRCDE PICTURE  S9(11)                   CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ICNVR1 PICTURE  X.                       CI0082
            11            CT17-GESTD1 PICTURE  9(8).                    CI0082
            11            CT17-GEEND3 PICTURE  9(8).                    CI0082
            11            CT17-NSCHIH PICTURE  999.                     CI0082
            11            CT17-AECMPA PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AECPBA PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CCFEEA PICTURE  X.                       CI0082
            10            CT17-C283                                     CI0082
                          REDEFINES            CT17-C280.               CI0082
            11            CT17-ICNVR2 PICTURE  X.                       CI0082
            11            CT17-GESTD2 PICTURE  9(8).                    CI0082
            11            CT17-GEEND2 PICTURE  9(8).                    CI0082
            11            CT17-AEXAC  PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APFEEK PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AACBL  PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CEABL3 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CEABL4 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CEABL5 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ICALC1 PICTURE  X.                       CI0082
            11            CT17-ISBIL  PICTURE  X.                       CI0082
            10            CT17-C284                                     CI0082
                          REDEFINES            CT17-C280.               CI0082
            11            CT17-ICNVR3 PICTURE  X.                       CI0082
            11            CT17-GESTD3 PICTURE  9(8).                    CI0082
            11            CT17-GEEND4 PICTURE  9(8).                    CI0082
            10            CT17-C285                                     CI0082
                          REDEFINES            CT17-C280.               CI0082
            11            CT17-ICNVR5 PICTURE  X.                       CI0082
            11            CT17-GESTD4 PICTURE  9(8).                    CI0082
            11            CT17-GEEND6 PICTURE  9(8).                    CI0082
            11            CT17-IINSP  PICTURE  X.                       CI0082
            11            CT17-CDMOD  PICTURE  9(2).                    CI0082
            11            CT17-NSCHI9 PICTURE  999.                     CI0082
            11            CT17-PREPL5 PICTURE  S9(3)V9(3).              CI0082
            11            CT17-AGAPA  PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AFABA  PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CEABL9 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CEABLA PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CEABLB PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AFABA2 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AFABA3 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ACOMT4 PICTURE  S9(15)V99                CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APRCDF PICTURE  S9(11)                   CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APFEP1 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APFEP3 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APFEP4 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APFEP5 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APFEP6 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APFEP7 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APFEP8 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APFEPA PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AGAPA1 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AFABA1 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AFABA4 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CPRPM4 PICTURE  9(3).                    CI0082
            11            CT17-APPRM2 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-DSBST  PICTURE  9(8).                    CI0082
            11            CT17-NCTYR  PICTURE  999.                     CI0082
            11            CT17-ISPCV  PICTURE  X.                       CI0082
            11            CT17-ISETC  PICTURE  X.                       CI0082
            11            CT17-IANNC  PICTURE  X.                       CI0082
            11            CT17-NSCHIE PICTURE  999.                     CI0082
            11            CT17-NMGAT  PICTURE  9(2).                    CI0082
            10            CT17-C286                                     CI0082
                          REDEFINES            CT17-C280.               CI0082
            11            CT17-ABEAC1 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AEXAC2 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AACBL1 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CEABL6 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CEABL7 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CEABL8 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AUEPC  PICTURE  S9(11)                   CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CTLPM  PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ICNVR4 PICTURE  X.                       CI0082
            11            CT17-ICALC2 PICTURE  X.                       CI0082
            11            CT17-QSYPAA PICTURE  9(2).                    CI0082
            11            CT17-GESTD5 PICTURE  9(8).                    CI0082
            11            CT17-GEEND5 PICTURE  9(8).                    CI0082
            10            CT17-C287                                     CI0082
                          REDEFINES            CT17-C280.               CI0082
            11            CT17-CDMOD2 PICTURE  9(2).                    CI0082
            11            CT17-NSCHIA PICTURE  999.                     CI0082
            11            CT17-GESTD6 PICTURE  9(8).                    CI0082
            11            CT17-GEEND7 PICTURE  9(8).                    CI0082
            11            CT17-DCMST  PICTURE  9(8).                    CI0082
            11            CT17-PPDST  PICTURE  S9(4)V999                CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-PRATE1 PICTURE  S9(4)V999                CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CRATE  PICTURE  X.                       CI0082
            11            CT17-PREPL6 PICTURE  S9(4)V9(3)               CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AGAPA4 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AGAPA3 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APFEPD PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-APFEPC PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ASTTL2 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ASTTL3 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ASTTL4 PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CPRPM5 PICTURE  9(3).                    CI0082
            11            CT17-APPRM3 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ACOMT5 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-IANNC1 PICTURE  X.                       CI0082
            11            CT17-NCTYR1 PICTURE  999.                     CI0082
            11            CT17-AAPCP2 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-AAPCP1 PICTURE  S9(9)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CEABLE PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CEABLC PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-CEABLD PICTURE  S9(7)V99                 CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-IFRNT1 PICTURE  X.                       CI0082
            11            CT17-APRCD8 PICTURE  S9(11)                   CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            CT17-ICNVR6 PICTURE  X.                       CI0082
            11            CT17-IREPL5 PICTURE  X.                       CI0082
            11            CT17-CATRF  PICTURE  9(3).                    CI0082
            11            CT17-IRENT  PICTURE  X.                       CI0082
            11            CT17-IPCIP  PICTURE  X.                       CI0082
            11            CT17-NSCHIF PICTURE  999.                     CI0082
       01                 CT18.                                         CI0082
            10            CT18-CT18K.                                   CI0082
            11            CT18-GESQ2  PICTURE  99.                      CI0082
            11            CT18-CMACR  PICTURE  9(3).                    CI0082
            11            CT18-GERSD  PICTURE  9(8).                    CI0082
            11            CT18-C199.                                    CI0082
            12            CT18-CLID.                                    CI0082
            13            CT18-CLIDO  PICTURE  9(3).                    CI0082
            13            CT18-CLIDN.                                   CI0082
            14            CT18-CLIDNP PICTURE  X(12).                   CI0082
            14            CT18-CLIDND PICTURE  9(8).                    CI0082
            11            CT18-CPATP  PICTURE  X(1).                    CI0082
            10            CT18-GERED  PICTURE  9(8).                    CI0082
            10            CT18-PSMAP  PICTURE  S9(3)V9(4)               CI0082
                          COMPUTATIONAL-3.                              CI0082
            10            CT18-NSORG  PICTURE  9(3).                    CI0082
            10            CT18-CUNPD  PICTURE  X(2).                    CI0082
            10            CT18-NCPSP  PICTURE  S9(03)                   CI0082
                          COMPUTATIONAL-3.                              CI0082
            10            CT18-PCPSP  PICTURE  S9(3)V9(4)               CI0082
                          COMPUTATIONAL-3.                              CI0082
            10            CT18-CRSET  PICTURE  9(3).                    CI0082
            10            CT18-CRELT  PICTURE  9(3).                    CI0082
            10            CT18-FILLER PICTURE  X(10).                   CI0082
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0082
            10            XW05-XW06.                                    CI0082
            11            XW05-XDBPCB.                                  CI0082
            12            XW05-XDBDNM PICTURE  X(08)                    CI0082
                          VALUE                SPACE.                   CI0082
            12            XW05-XSEGLV PICTURE  X(02)                    CI0082
                          VALUE                SPACE.                   CI0082
            12            XW05-XRC    PICTURE  X(02)                    CI0082
                          VALUE                SPACE.                   CI0082
            12            XW05-XPROPT PICTURE  X(04)                    CI0082
                          VALUE                SPACE.                   CI0082
            12            XW05-FILLER PICTURE  S9(5)                    CI0082
                          VALUE                ZERO                     CI0082
                          BINARY.                                       CI0082
            12            XW05-XSEGNM PICTURE  X(08)                    CI0082
                          VALUE                SPACE.                   CI0082
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0082
                          VALUE                ZERO                     CI0082
                          BINARY.                                       CI0082
            12            XW05-XSEGNB PICTURE  9(05)                    CI0082
                          VALUE                ZERO                     CI0082
                          BINARY.                                       CI0082
            12            XW05-XCOKEY PICTURE  X(70)                    CI0082
                          VALUE                SPACE.                   CI0082
            10            XW05-XW07.                                    CI0082
            11            XW05-XIOPCB.                                  CI0082
            12            XW05-XTERMI PICTURE  X(08)                    CI0082
                          VALUE                SPACE.                   CI0082
            12            XW05-FILLER PICTURE  XX                       CI0082
                          VALUE                SPACE.                   CI0082
            12            XW05-XRC1   PICTURE  X(02)                    CI0082
                          VALUE                SPACE.                   CI0082
            12            XW05-FILLER PICTURE  X(12)                    CI0082
                          VALUE                SPACE.                   CI0082
            12            XW05-XMODNM PICTURE  X(8)                     CI0082
                          VALUE                SPACE.                   CI0082
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0082
                          VALUE                ZERO.                    CI0082
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0082
                          VALUE                ZERO.                    CI0082
            10            XW05-XGU    PICTURE  X(4)                     CI0082
                          VALUE                'GU  '.                  CI0082
            10            XW05-XGHU   PICTURE  X(4)                     CI0082
                          VALUE                'GHU '.                  CI0082
            10            XW05-XGN    PICTURE  X(4)                     CI0082
                          VALUE                'GN  '.                  CI0082
            10            XW05-XGHN   PICTURE  X(4)                     CI0082
                          VALUE                'GHN '.                  CI0082
            10            XW05-XGNP   PICTURE  X(4)                     CI0082
                          VALUE                'GNP '.                  CI0082
            10            XW05-XGHNP  PICTURE  X(4)                     CI0082
                          VALUE                'GHNP'.                  CI0082
            10            XW05-XREPL  PICTURE  XXXX                     CI0082
                          VALUE                'REPL'.                  CI0082
            10            XW05-XISRT  PICTURE  X(4)                     CI0082
                          VALUE                'ISRT'.                  CI0082
            10            XW05-XDLET  PICTURE  X(4)                     CI0082
                          VALUE                'DLET'.                  CI0082
            10            XW05-XOPEN  PICTURE  X(4)                     CI0082
                          VALUE                'OPEN'.                  CI0082
            10            XW05-XCLSE  PICTURE  X(4)                     CI0082
                          VALUE                'CLSE'.                  CI0082
            10            XW05-XCHKP  PICTURE  X(4)                     CI0082
                          VALUE                'CHKP'.                  CI0082
            10            XW05-XXRST  PICTURE  X(4)                     CI0082
                          VALUE                'XRST'.                  CI0082
            10            XW05-XTERM  PICTURE  X(4)                     CI0082
                          VALUE                'TERM'.                  CI0082
            10            XW05-XNFPAC PICTURE  X(13)                    CI0082
                          VALUE                SPACE.                   CI0082
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0082
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0082
      *
      ******************************************************************
      **     SEGMENT USED FOR CALLING CI0004                           *
      ******************************************************************
      *
      *!WF DSP=DU DSL=DU SEL=07 FOR=I LEV=1 PLT=DU
       01                 DU00.                                         CI0082
          05              DU00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(00437).                  CI0082
       01                 DU07  REDEFINES      DU00.                    CI0082
            10            DU07-C299.                                    CI0082
            11            DU07-CTID.                                    CI0082
            12            DU07-CTIDA  PICTURE  9(3).                    CI0082
            12            DU07-CTIDN.                                   CI0082
            13            DU07-CTIDNP PICTURE  X(13).                   CI0082
            13            DU07-CTIDND PICTURE  9(11).                   CI0082
            10            DU07-DCACG  PICTURE  9(8).                    CI0082
            10            DU07-FILLER PICTURE  X(100).                  CI0082
            10            DU07-CL24.                                    CI0082
            11            DU07-GELL   PICTURE  9(4)                     CI0082
                          BINARY.                                       CI0082
            11            DU07-CL24K.                                   CI0082
            12            DU07-GECSQ  PICTURE  S9(3)                    CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            DU07-GECSD  PICTURE  9(8).                    CI0082
            11            DU07-GECED  PICTURE  9(8).                    CI0082
            11            DU07-CREQ2  PICTURE  X.                       CI0082
            11            DU07-FILLER PICTURE  X(4).                    CI0082
            11            DU07-GECTA  PICTURE  X.                       CI0082
            11            DU07-GELCD  PICTURE  9(8).                    CI0082
            11            DU07-GEADS  PICTURE  9.                       CI0082
            11            DU07-GECIT  PICTURE  X(25).                   CI0082
            11            DU07-GECTRY PICTURE  X(20).                   CI0082
            11            DU07-GECTY  PICTURE  9(3).                    CI0082
            11            DU07-GEPCD  PICTURE  X(12).                   CI0082
            11            DU07-GEST   PICTURE  X(8).                    CI0082
            11            DU07-IRESA  PICTURE  X.                       CI0082
            11            DU07-FILLER PICTURE  X(8).                    CI0082
            11            DU07-GESAD  PICTURE  X(30)                    CI0082
                          OCCURS       003     TIMES.                   CI0082
            10            DU07-FILLER PICTURE  X(100).                  CI0082
      *
      *
      *
      *
      *
      ******************************************************************
      **     SEGMENT THAT CONTAINS THE ACCOUNT OWNERSHIP AND           *
      **     BENEFICIARY FOR REQUESTED ACCOUNT ID NUMBER               *
      ******************************************************************
      *
      *!WF DSP=FA DSL=DU SEL=04 FOR=I LEV=1 PLT=FA
       01                 FA00.                                         CI0082
          05              FA00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(00407).                  CI0082
       01                 FA04  REDEFINES      FA00.                    CI0082
            10            FA04-C299.                                    CI0082
            11            FA04-CTID.                                    CI0082
            12            FA04-CTIDA  PICTURE  9(3).                    CI0082
            12            FA04-CTIDN.                                   CI0082
            13            FA04-CTIDNP PICTURE  X(13).                   CI0082
            13            FA04-CTIDND PICTURE  9(11).                   CI0082
            10            FA04-IPOCH  PICTURE  X.                       CI0082
            10            FA04-FILLER PICTURE  X(099).                  CI0082
            10            FA04-CTTLN1 PICTURE  X(30).                   CI0082
            10            FA04-CTTLN2 PICTURE  X(30).                   CI0082
            10            FA04-CTTLN3 PICTURE  X(30).                   CI0082
            10            FA04-CTTBO1 PICTURE  X(45).                   CI0082
            10            FA04-CTTBO2 PICTURE  X(45).                   CI0082
            10            FA04-CTOWN  PICTURE  9(3).                    CI0082
            10            FA04-IUGMA  PICTURE  X.                       CI0082
            10            FA04-FILLER PICTURE  X(096).                  CI0082
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *
      **     GROUPS FOR THE ACCOUNT NUMBER PASSED.                     *
      ******************************************************************
      *
      *DP: FG  DL: DU SEL: 15______ PICT: I DESC: _ LEV: 1 ORG: _ SS: _
      *
      *
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
      *                                                                 AM0020
      ******************************************************************AM0020
      **     SEGMENT THAT CONTAINS THE CAMS ACCOUNTING DATES           *AM0020
      ******************************************************************AM0020
      *                                                                 AM0020
      *!WF DSP=NS DSL=NS SEL=20 FOR=I LEV=1                             AM0020
       01                 NS00.                                         CI0082
          05              NS00-00.                                      CI0082
            10            NS00-NS00K.                                   CI0082
            11            NS00-PRCSTK PICTURE  XX.                      CI0082
          05              NS00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(00078).                  CI0082
       01                 NS20  REDEFINES      NS00.                    CI0082
            10       FILLER         PICTURE  X(00002).                  CI0082
            10            NS20-DCACG  PICTURE  9(8).                    CI0082
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0082
                          COMPUTATIONAL-3.                              CI0082
            10            NS20-CCDAT  PICTURE  X(8).                    CI0082
            10            NS20-DCALP  PICTURE  X(12).                   CI0082
            10            NS20-DNACG  PICTURE  9(8).                    CI0082
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0082
                          COMPUTATIONAL-3.                              CI0082
            10            NS20-CNDAT  PICTURE  X(8).                    CI0082
            10            NS20-DNALP  PICTURE  X(12).                   CI0082
            10            NS20-DCACD  PICTURE  X(10).                   CI0082
            10            NS20-FILLER PICTURE  X(4).                    CI0082
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
                                                                        AM0019
      ******************************************************************AM0019
      **     PCB ADDRESS LIST FOR CI0019.  MODULE CI0019 WILL NEED     *AM0019
      **     PCB'S FOR:                                                *AM0019
      **                CONTRACT DATABASE(CT1P)                        *AM0019
      **                GROUP DATABASE(GR1P)                           *AM0019
      ******************************************************************AM0019
                                                                        AM0019
       01  CI0019C-PCB-ADDRESS-LIST.                                    AM0019
           05  CI0019C-PCB-CT1P-PTR1      POINTER.                      AM0019
           05  CI0019C-PCB-GR1P-PTR1      POINTER.                      AM0019
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
       01  7-CT18-SSA.
           05  FILLER           PIC X(08)     VALUE 'CT18    '.
           05  FILLER           PIC X(01)     VALUE '*'.
           05  FILLER           PIC X(05)     VALUE '-----'.
           05  FILLER           PIC X(01)     VALUE '('.
           05  FILLER           PIC X(08)     VALUE 'GESQ2   '.
           05  FILLER           PIC X(02)     VALUE ' ='.
           05  7-CT18-GESQ2     PIC 99.
           05  FILLER           PIC X(01)     VALUE '&'.
           05  FILLER           PIC X(08)     VALUE 'CMACR   '.
           05  FILLER           PIC X(02)     VALUE ' ='.
           05  7-CT18-CMACR     PIC 9(3).
           05  FILLER           PIC X(01)     VALUE '&'.
           05  FILLER           PIC X(08)     VALUE 'GERED   '.
           05  FILLER           PIC X(02)     VALUE ' ='.
           05  7-CT18-GERED     PIC 9(08).
           05  FILLER           PIC X(01)     VALUE ')'.
      *
      ******************************************************************
      **     WORKING STORAGE MISC FIELDS                               *
      ******************************************************************
      *
       01  7-WORK-FIELDS.
           05  7-WK-COUNT       PIC 9(3).
      *!WI
           05  7-WK-QITEM
                        PICTURE 9(3).                                   CI0082
      *!WI
           05  7-WK-CLID
                        PICTURE X(23).                                  CI0082
      *!WI
           05  7-WK1-CLID
                        PICTURE X(23).                                  CI0082
      *!WI
           05  7-WK2-CLID
                        PICTURE X(23).                                  CI0082
      *!WI
           05  7-PART-IAIND
                        PICTURE X.                                      CI0082
      *!WI
           05  7-HSHLD-IAIND
                        PICTURE X.                                      CI0082
      *
      ******************************************************************
      **     WORK VERSION OF CT10 TO BREAK DOWN THE OCCURS PORTION     *
      **     OF THE ARRAY OUT OF CI0019                                *
      ******************************************************************
      *
      *!WF DSP=WK DSL=CT SEL=10 FOR=I LEV=1 PLT=WK
       01                 WK00.                                         CI0082
          05              WK00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(00030).                  CI0082
       01                 WK10  REDEFINES      WK00.                    CI0082
            10            WK10-CT10K.                                   CI0082
            11            WK10-GR98.                                    CI0082
            12            WK10-GRID.                                    CI0082
            13            WK10-GRIDC  PICTURE  9(3).                    CI0082
            13            WK10-GRIDN.                                   CI0082
            14            WK10-GRIDNP PICTURE  99.                      CI0082
            14            WK10-GRIDND PICTURE  9(8).                    CI0082
            10            WK10-GR97                                     CI0082
                          REDEFINES            WK10-CT10K.              CI0082
            11            WK10-GRIDCB PICTURE  9(3).                    CI0082
            11            WK10-FILLER PICTURE  X(10).                   CI0082
            10            WK10-GERSD  PICTURE  9(8).                    CI0082
            10            WK10-GERED  PICTURE  9(8).                    CI0082
            10            WK10-GRCSI  PICTURE  X.                       CI0082
      *
      *
      *
      ******************************************************************
      **     WORK VERSION OF DU21 TO BREAK DOWN THE OCCURS PORTION     *
      **     OF THE ARRAY OUT OF CI0018                                *
      ******************************************************************
      *
      *!WF DSP=WS DSL=DU SEL=21 FOR=I LEV=1 PLT=WK
       01                 WS00.                                         CI0082
          05              WS00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(00026).                  CI0082
       01                 WS21  REDEFINES      WS00.                    CI0082
            10            WS21-C199.                                    CI0082
            11            WS21-CLID.                                    CI0082
            12            WS21-CLIDO  PICTURE  9(3).                    CI0082
            12            WS21-CLIDN.                                   CI0082
            13            WS21-CLIDNP PICTURE  X(12).                   CI0082
            13            WS21-CLIDND PICTURE  9(8).                    CI0082
            10            WS21-CLCTRC PICTURE  9(3).                    CI0082
      *
      *
       01  7-FIELDS.
           05  7-CLIENT-HAS-NAME-INFO          PIC X(01) VALUE 'N'.
       01   DEBUT-WSS.                                                  CI0082
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0082
            05   IK     PICTURE X.                                      CI0082
       01  CONSTANTES-PAC.                                              CI0082
           05  FILLER  PICTURE X(87)   VALUE                            CI0082
                     '6015 CAT09/08/14CI0082ADMIN   14:34:41CI0082P AMERCI0082
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0082
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0082
           05  NUGNA   PICTURE X(5).                                    CI0082
           05  APPLI   PICTURE X(3).                                    CI0082
           05  DATGN   PICTURE X(8).                                    CI0082
           05  PROGR   PICTURE X(6).                                    CI0082
           05  CODUTI  PICTURE X(8).                                    CI0082
           05  TIMGN   PICTURE X(8).                                    CI0082
           05  PROGE   PICTURE X(8).                                    CI0082
           05  COBASE  PICTURE X(4).                                    CI0082
           05  DATGNC  PICTURE X(10).                                   CI0082
           05  RELEAS  PICTURE X(7).                                    CI0082
           05  DATGE   PICTURE X(10).                                   CI0082
           05  DATSQ   PICTURE X(10).                                   CI0082
       01  DATCE.                                                       CI0082
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0082
         05  DATOR.                                                     CI0082
           10  DATOA  PICTURE XX.                                       CI0082
           10  DATOM  PICTURE XX.                                       CI0082
           10  DATOJ  PICTURE XX.                                       CI0082
       01   VARIABLES-CONDITIONNELLES.                                  CI0082
            05                  FT      PICTURE X VALUE '0'.            CI0082
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0082
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0082
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0082
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0082
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0082
       01               S-CL01-SSA.                                     CI0082
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0082
                                      VALUE 'CL01    '.                 CI0082
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0082
            10          S-CL01-CCOD   PICTURE X(5)                      CI0082
                                      VALUE '-----'.                    CI0082
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0082
       01            S-CLU01-SSA.                                       CI0082
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0082
                                      VALUE 'CL01    '.                 CI0082
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0082
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0082
                                      VALUE '-----'.                    CI0082
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0082
                                      VALUE '(CL01K'.                   CI0082
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0082
            10       S-CLU01-CL01K.                                     CI0082
            11       S-CLU01-C199.                                      CI0082
            12       S-CLU01-CLID.                                      CI0082
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0082
            13       S-CLU01-CLIDN.                                     CI0082
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0082
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0082
            10  FILLER   PICTURE X    VALUE ')'.                        CI0082
       01               S-CL03-SSA.                                     CI0082
            10         S1-CL03-SEGNAM PICTURE X(8)                      CI0082
                                      VALUE 'CL03    '.                 CI0082
            10         S1-CL03-CCOM   PICTURE X VALUE '*'.              CI0082
            10          S-CL03-CCOD   PICTURE X(5)                      CI0082
                                      VALUE '-----'.                    CI0082
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0082
       01            S-CLA03-SSA.                                       CI0082
            10      S1-CLA03-SEGNAM PICTURE X(8)                        CI0082
                                      VALUE 'CL03    '.                 CI0082
            10      S1-CLA03-CCOM   PICTURE X VALUE '*'.                CI0082
            10       S-CLA03-CCOD   PICTURE X(5)                        CI0082
                                      VALUE '-----'.                    CI0082
            10      S1-CLA03-FLDNAM PICTURE X(9)                        CI0082
                                      VALUE '(CLDOD'.                   CI0082
            10       S-CLA03-OPER  PICTURE XX VALUE ' ='.               CI0082
            10       S-CLA03-CLDOD    PICTURE  9(8).                    CI0082
            10  FILLER   PICTURE X    VALUE ')'.                        CI0082
       01               S-CL12-SSA.                                     CI0082
            10         S1-CL12-SEGNAM PICTURE X(8)                      CI0082
                                      VALUE 'CL12    '.                 CI0082
            10         S1-CL12-CCOM   PICTURE X VALUE '*'.              CI0082
            10          S-CL12-CCOD   PICTURE X(5)                      CI0082
                                      VALUE '-----'.                    CI0082
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0082
       01               S-CT01-SSA.                                     CI0082
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0082
                                      VALUE 'CT01    '.                 CI0082
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0082
            10          S-CT01-CCOD   PICTURE X(5)                      CI0082
                                      VALUE '-----'.                    CI0082
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0082
       01            S-CTU01-SSA.                                       CI0082
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0082
                                      VALUE 'CT01    '.                 CI0082
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0082
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0082
                                      VALUE '-----'.                    CI0082
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0082
                                      VALUE '(CT01K'.                   CI0082
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0082
            10       S-CTU01-CT01K.                                     CI0082
            11       S-CTU01-C299.                                      CI0082
            12       S-CTU01-CTID.                                      CI0082
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0082
            13       S-CTU01-CTIDN.                                     CI0082
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0082
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0082
            10  FILLER   PICTURE X    VALUE ')'.                        CI0082
       01               S-CT17-SSA.                                     CI0082
            10         S1-CT17-SEGNAM PICTURE X(8)                      CI0082
                                      VALUE 'CT17    '.                 CI0082
            10         S1-CT17-CCOM   PICTURE X VALUE '*'.              CI0082
            10          S-CT17-CCOD   PICTURE X(5)                      CI0082
                                      VALUE '-----'.                    CI0082
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0082
       01            S-CTA17-SSA.                                       CI0082
            10      S1-CTA17-SEGNAM PICTURE X(8)                        CI0082
                                      VALUE 'CT17    '.                 CI0082
            10      S1-CTA17-CCOM   PICTURE X VALUE '*'.                CI0082
            10       S-CTA17-CCOD   PICTURE X(5)                        CI0082
                                      VALUE '-----'.                    CI0082
            10      S1-CTA17-FLDNAM PICTURE X(9)                        CI0082
                                      VALUE '(IBASE'.                   CI0082
            10       S-CTA17-OPER  PICTURE XX VALUE ' ='.               CI0082
            10       S-CTA17-IBASE    PICTURE  X.                       CI0082
            10  FILLER   PICTURE X    VALUE ')'.                        CI0082
       01            S-CTU17-SSA.                                       CI0082
            10      S1-CTU17-SEGNAM PICTURE X(8)                        CI0082
                                      VALUE 'CT17    '.                 CI0082
            10      S1-CTU17-CCOM   PICTURE X VALUE '*'.                CI0082
            10       S-CTU17-CCOD   PICTURE X(5)                        CI0082
                                      VALUE '-----'.                    CI0082
            10      S1-CTU17-FLDNAM PICTURE X(9)                        CI0082
                                      VALUE '(CT17K'.                   CI0082
            10       S-CTU17-OPER  PICTURE XX VALUE ' ='.               CI0082
            10       S-CTU17-CT17K.                                     CI0082
            11       S-CTU17-NSUBA    PICTURE  9(15).                   CI0082
            10  FILLER   PICTURE X    VALUE ')'.                        CI0082
       01               S-CT18-SSA.                                     CI0082
            10         S1-CT18-SEGNAM PICTURE X(8)                      CI0082
                                      VALUE 'CT18    '.                 CI0082
            10         S1-CT18-CCOM   PICTURE X VALUE '*'.              CI0082
            10          S-CT18-CCOD   PICTURE X(5)                      CI0082
                                      VALUE '-----'.                    CI0082
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0082
       01            S-CTA18-SSA.                                       CI0082
            11      S1-CTA18-SEGNAM PICTURE X(8)                        CI0082
                                      VALUE 'CT18    '.                 CI0082
            11      S1-CTA18-CCOM   PICTURE X VALUE '*'.                CI0082
            11       S-CTA18-CCOD   PICTURE X(5)                        CI0082
                                      VALUE '-----'.                    CI0082
            11      S1-CTA18-FLDNAM PICTURE X(9)                        CI0082
                                      VALUE '(GESQ2'.                   CI0082
            11       S-CTA18-OPER  PICTURE XX VALUE ' ='.               CI0082
            11       S-CTA18-GESQ2    PICTURE  99.                      CI0082
            11  FILLER   PICTURE X    VALUE ')'.                        CI0082
       01            S-CTB18-SSA.                                       CI0082
            11      S1-CTB18-SEGNAM PICTURE X(8)                        CI0082
                                      VALUE 'CT18    '.                 CI0082
            11      S1-CTB18-CCOM   PICTURE X VALUE '*'.                CI0082
            11       S-CTB18-CCOD   PICTURE X(5)                        CI0082
                                      VALUE '-----'.                    CI0082
            11      S1-CTB18-FLDNAM PICTURE X(9)                        CI0082
                                      VALUE '(CMACR'.                   CI0082
            11       S-CTB18-OPER  PICTURE XX VALUE ' ='.               CI0082
            11       S-CTB18-CMACR    PICTURE  9(3).                    CI0082
            11  FILLER   PICTURE X    VALUE ')'.                        CI0082
       01            S-CTC18-SSA.                                       CI0082
            10      S1-CTC18-SEGNAM PICTURE X(8)                        CI0082
                                      VALUE 'CT18    '.                 CI0082
            10      S1-CTC18-CCOM   PICTURE X VALUE '*'.                CI0082
            10       S-CTC18-CCOD   PICTURE X(5)                        CI0082
                                      VALUE '-----'.                    CI0082
            10      S1-CTC18-FLDNAM PICTURE X(9)                        CI0082
                                      VALUE '(GERED'.                   CI0082
            10       S-CTC18-OPER  PICTURE XX VALUE ' ='.               CI0082
            10       S-CTC18-GERED    PICTURE  9(8).                    CI0082
            10  FILLER   PICTURE X    VALUE ')'.                        CI0082
       01            S-CTD18-SSA.                                       CI0082
            11      S1-CTD18-SEGNAM PICTURE X(8)                        CI0082
                                      VALUE 'CT18    '.                 CI0082
            11      S1-CTD18-CCOM   PICTURE X VALUE '*'.                CI0082
            11       S-CTD18-CCOD   PICTURE X(5)                        CI0082
                                      VALUE '-----'.                    CI0082
            11      S1-CTD18-FLDNAM PICTURE X(9)                        CI0082
                                      VALUE '(CPATP'.                   CI0082
            11       S-CTD18-OPER  PICTURE XX VALUE ' ='.               CI0082
            11       S-CTD18-CPATP    PICTURE  X(1).                    CI0082
            11  FILLER   PICTURE X    VALUE ')'.                        CI0082
       01            S-CTU18-SSA.                                       CI0082
            10      S1-CTU18-SEGNAM PICTURE X(8)                        CI0082
                                      VALUE 'CT18    '.                 CI0082
            10      S1-CTU18-CCOM   PICTURE X VALUE '*'.                CI0082
            10       S-CTU18-CCOD   PICTURE X(5)                        CI0082
                                      VALUE '-----'.                    CI0082
            10      S1-CTU18-FLDNAM PICTURE X(9)                        CI0082
                                      VALUE '(CT18K'.                   CI0082
            10       S-CTU18-OPER  PICTURE XX VALUE ' ='.               CI0082
            10       S-CTU18-CT18K.                                     CI0082
            11       S-CTU18-GESQ2    PICTURE  99.                      CI0082
            11       S-CTU18-CMACR    PICTURE  9(3).                    CI0082
            11       S-CTU18-GERSD    PICTURE  9(8).                    CI0082
            11       S-CTU18-C199.                                      CI0082
            12       S-CTU18-CLID.                                      CI0082
            13       S-CTU18-CLIDO    PICTURE  9(3).                    CI0082
            13       S-CTU18-CLIDN.                                     CI0082
            14       S-CTU18-CLIDNP   PICTURE  X(12).                   CI0082
            14       S-CTU18-CLIDND   PICTURE  9(8).                    CI0082
            11       S-CTU18-CPATP    PICTURE  X(1).                    CI0082
            10  FILLER   PICTURE X    VALUE ')'.                        CI0082
       01   ZONES-UTILISATEUR PICTURE X.                                CI0082
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
      ** PCB POINTER FOR ARAY                                           ADU015
            05 PCB-ARAY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0082
          05              PA00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(00106).                  CI0082
       01                 PA06  REDEFINES      PA00.                    CI0082
            10            PA06-XDBPCB.                                  CI0082
            11            PA06-XDBDNM PICTURE  X(08).                   CI0082
            11            PA06-XSEGLV PICTURE  X(02).                   CI0082
            11            PA06-XRC    PICTURE  X(02).                   CI0082
            11            PA06-XPROPT PICTURE  X(04).                   CI0082
            11            PA06-FILLER PICTURE  S9(5)                    CI0082
                          BINARY.                                       CI0082
            11            PA06-XSEGNM PICTURE  X(08).                   CI0082
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0082
                          BINARY.                                       CI0082
            11            PA06-XSEGNB PICTURE  9(05)                    CI0082
                          BINARY.                                       CI0082
            11            PA06-XCOKEY PICTURE  X(70).                   CI0082
      *** PCB MASK FOR GR1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0082
          05              PB00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(00106).                  CI0082
       01                 PB06  REDEFINES      PB00.                    CI0082
            10            PB06-XDBPCB.                                  CI0082
            11            PB06-XDBDNM PICTURE  X(08).                   CI0082
            11            PB06-XSEGLV PICTURE  X(02).                   CI0082
            11            PB06-XRC    PICTURE  X(02).                   CI0082
            11            PB06-XPROPT PICTURE  X(04).                   CI0082
            11            PB06-FILLER PICTURE  S9(5)                    CI0082
                          BINARY.                                       CI0082
            11            PB06-XSEGNM PICTURE  X(08).                   CI0082
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0082
                          BINARY.                                       CI0082
            11            PB06-XSEGNB PICTURE  9(05)                    CI0082
                          BINARY.                                       CI0082
            11            PB06-XCOKEY PICTURE  X(70).                   CI0082
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0082
          05              PC00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(00106).                  CI0082
       01                 PC06  REDEFINES      PC00.                    CI0082
            10            PC06-XDBPCB.                                  CI0082
            11            PC06-XDBDNM PICTURE  X(08).                   CI0082
            11            PC06-XSEGLV PICTURE  X(02).                   CI0082
            11            PC06-XRC    PICTURE  X(02).                   CI0082
            11            PC06-XPROPT PICTURE  X(04).                   CI0082
            11            PC06-FILLER PICTURE  S9(5)                    CI0082
                          BINARY.                                       CI0082
            11            PC06-XSEGNM PICTURE  X(08).                   CI0082
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0082
                          BINARY.                                       CI0082
            11            PC06-XSEGNB PICTURE  9(05)                    CI0082
                          BINARY.                                       CI0082
            11            PC06-XCOKEY PICTURE  X(70).                   CI0082
      *** PCB MASK FOR ARAY                                             ADU015
      *!WF DSP=PD DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PD00.                                         CI0082
          05              PD00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(00106).                  CI0082
       01                 PD06  REDEFINES      PD00.                    CI0082
            10            PD06-XDBPCB.                                  CI0082
            11            PD06-XDBDNM PICTURE  X(08).                   CI0082
            11            PD06-XSEGLV PICTURE  X(02).                   CI0082
            11            PD06-XRC    PICTURE  X(02).                   CI0082
            11            PD06-XPROPT PICTURE  X(04).                   CI0082
            11            PD06-FILLER PICTURE  S9(5)                    CI0082
                          BINARY.                                       CI0082
            11            PD06-XSEGNM PICTURE  X(08).                   CI0082
            11            PD06-XKEYLN PICTURE  S9(05)                   CI0082
                          BINARY.                                       CI0082
            11            PD06-XSEGNB PICTURE  9(05)                    CI0082
                          BINARY.                                       CI0082
            11            PD06-XCOKEY PICTURE  X(70).                   CI0082
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=PE DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PE00.                                         CI0082
          05              PE00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(00106).                  CI0082
       01                 PE06  REDEFINES      PE00.                    CI0082
            10            PE06-XDBPCB.                                  CI0082
            11            PE06-XDBDNM PICTURE  X(08).                   CI0082
            11            PE06-XSEGLV PICTURE  X(02).                   CI0082
            11            PE06-XRC    PICTURE  X(02).                   CI0082
            11            PE06-XPROPT PICTURE  X(04).                   CI0082
            11            PE06-FILLER PICTURE  S9(5)                    CI0082
                          BINARY.                                       CI0082
            11            PE06-XSEGNM PICTURE  X(08).                   CI0082
            11            PE06-XKEYLN PICTURE  S9(05)                   CI0082
                          BINARY.                                       CI0082
            11            PE06-XSEGNB PICTURE  9(05)                    CI0082
                          BINARY.                                       CI0082
            11            PE06-XCOKEY PICTURE  X(70).                   CI0082
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED FOR CI9001    *
      ******************************************************************
      *
      *!WF DSP=PJ DSL=PJ SEL=10 FOR=I LEV=1 PLT=10
       01                 PJ00.                                         CI0082
          05              PJ00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(00291).                  CI0082
       01                 PJ10  REDEFINES      PJ00.                    CI0082
            10            PJ10-MAPPN  PICTURE  X(10).                   CI0082
            10            PJ10-PROGR  PICTURE  X(06).                   CI0082
            10            PJ10-CUPIQ  PICTURE  X.                       CI0082
            10            PJ10-CHCR   PICTURE  99.                      CI0082
            10            PJ10-C299.                                    CI0082
            11            PJ10-CTID.                                    CI0082
            12            PJ10-CTIDA  PICTURE  9(3).                    CI0082
            12            PJ10-CTIDN.                                   CI0082
            13            PJ10-CTIDNP PICTURE  X(13).                   CI0082
            13            PJ10-CTIDND PICTURE  9(11).                   CI0082
            10            PJ10-CARTZ  PICTURE  99                       CI0082
                          OCCURS       006     TIMES.                   CI0082
            10            PJ10-TTRTP  PICTURE  X(30)                    CI0082
                          OCCURS       006     TIMES.                   CI0082
            10            PJ10-QITEM  PICTURE  9(3).                    CI0082
            10            PJ10-PRCOD  PICTURE  9(5).                    CI0082
            10            PJ10-FILLER PICTURE  X(45).                   CI0082
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED FOR CI0018    *
      ******************************************************************
      *
      *!WF DSP=AC DSL=DU SEL=14 FOR=I LEV=1 PLT=20
       01                 AC00.                                         CI0082
          05              AC00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(00917).                  CI0082
       01                 AC14  REDEFINES      AC00.                    CI0082
            10            AC14-C299.                                    CI0082
            11            AC14-CTID.                                    CI0082
            12            AC14-CTIDA  PICTURE  9(3).                    CI0082
            12            AC14-CTIDN.                                   CI0082
            13            AC14-CTIDNP PICTURE  X(13).                   CI0082
            13            AC14-CTIDND PICTURE  9(11).                   CI0082
            10            AC14-DCACG  PICTURE  9(8).                    CI0082
            10            AC14-IPOCH  PICTURE  X.                       CI0082
            10            AC14-FILLER PICTURE  X(100).                  CI0082
            10            AC14-CLID01.                                  CI0082
            11            AC14-CLIDO1 PICTURE  X(3).                    CI0082
            11            AC14-NCLID1.                                  CI0082
            12            AC14-CLIDP1 PICTURE  X(12).                   CI0082
            12            AC14-CLIDNA PICTURE  9(8).                    CI0082
            10            AC14-CLCTR  PICTURE  9(3).                    CI0082
            10            AC14-DU21                                     CI0082
                          OCCURS       025     TIMES.                   CI0082
            11            AC14-C199.                                    CI0082
            12            AC14-CLID.                                    CI0082
            13            AC14-CLIDO  PICTURE  9(3).                    CI0082
            13            AC14-CLIDN.                                   CI0082
            14            AC14-CLIDNP PICTURE  X(12).                   CI0082
            14            AC14-CLIDND PICTURE  9(8).                    CI0082
            11            AC14-CLCTRC PICTURE  9(3).                    CI0082
            10            AC14-QITEM  PICTURE  9(3).                    CI0082
            10            AC14-XIMAX  PICTURE  S9(4)                    CI0082
                          BINARY.                                       CI0082
            10            AC14-CRROL  PICTURE  X.                       CI0082
            10            AC14-FILLER PICTURE  X(099).                  CI0082
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED FOR CI0019    *
      ******************************************************************
      *
      *!WF DSP=FG DSL=DU SEL=15 FOR=I LEV=1 PLT=30
       01                 FG00.                                         CI0082
          05              FG00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(04181).                  CI0082
       01                 FG15  REDEFINES      FG00.                    CI0082
            10            FG15-C299.                                    CI0082
            11            FG15-CTID.                                    CI0082
            12            FG15-CTIDA  PICTURE  9(3).                    CI0082
            12            FG15-CTIDN.                                   CI0082
            13            FG15-CTIDNP PICTURE  X(13).                   CI0082
            13            FG15-CTIDND PICTURE  9(11).                   CI0082
            10            FG15-DCACG  PICTURE  9(8).                    CI0082
            10            FG15-IPOCH  PICTURE  X.                       CI0082
            10            FG15-FILLER PICTURE  X(100).                  CI0082
            10            FG15-DU18                                     CI0082
                          OCCURS       010     TIMES.                   CI0082
            11            FG15-CT10.                                    CI0082
            12            FG15-CT10K.                                   CI0082
            13            FG15-GR98.                                    CI0082
            14            FG15-GRID.                                    CI0082
            15            FG15-GRIDC  PICTURE  9(3).                    CI0082
            15            FG15-GRIDN.                                   CI0082
            16            FG15-GRIDNP PICTURE  99.                      CI0082
            16            FG15-GRIDND PICTURE  9(8).                    CI0082
            12            FG15-GR97                                     CI0082
                          REDEFINES            FG15-CT10K.              CI0082
            13            FG15-GRIDCB PICTURE  9(3).                    CI0082
            13            FG15-FILLER PICTURE  X(10).                   CI0082
            12            FG15-GERSD  PICTURE  9(8).                    CI0082
            12            FG15-GERED  PICTURE  9(8).                    CI0082
            12            FG15-GRCSI  PICTURE  X.                       CI0082
            11            FG15-GR01.                                    CI0082
            12            FG15-GR01K.                                   CI0082
            13            FG15-GR98.                                    CI0082
            14            FG15-GRID.                                    CI0082
            15            FG15-GRIDC  PICTURE  9(3).                    CI0082
            15            FG15-GRIDN.                                   CI0082
            16            FG15-GRIDNP PICTURE  99.                      CI0082
            16            FG15-GRIDND PICTURE  9(8).                    CI0082
            12            FG15-GECKD  PICTURE  9.                       CI0082
            12            FG15-GEMDA  PICTURE  9(8).                    CI0082
            12            FG15-NSEQ4B PICTURE  9(8)                     CI0082
                          BINARY.                                       CI0082
            12            FG15-GRDOR  PICTURE  9(8).                    CI0082
            12            FG15-GRIAD  PICTURE  9(8).                    CI0082
            12            FG15-GECUC  PICTURE  99.                      CI0082
            12            FG15-GRLNG  PICTURE  99.                      CI0082
            12            FG15-GESLC  PICTURE  99.                      CI0082
            12            FG15-AYSIDA PICTURE  9(3).                    CI0082
            12            FG15-AYSID  PICTURE  9(5).                    CI0082
            12            FG15-GRCSD  PICTURE  9(8).                    CI0082
            12            FG15-GRCFD  PICTURE  9(8).                    CI0082
            12            FG15-GRNCL  PICTURE  S9(5)                    CI0082
                          COMPUTATIONAL-3.                              CI0082
            12            FG15-GRNCT  PICTURE  S9(5)                    CI0082
                          COMPUTATIONAL-3.                              CI0082
            12            FG15-GRSFC  PICTURE  99.                      CI0082
            12            FG15-GRCRN  PICTURE  9(3).                    CI0082
            12            FG15-GRCSS  PICTURE  X.                       CI0082
            12            FG15-MKSRC  PICTURE  99                       CI0082
                          OCCURS       010     TIMES.                   CI0082
            12            FG15-NEFPS  PICTURE  X(5).                    CI0082
            12            FG15-DEFPS  PICTURE  9(8).                    CI0082
            12            FG15-DLSRV  PICTURE  9(8).                    CI0082
            12            FG15-CTLNI  PICTURE  X.                       CI0082
            12            FG15-CGRLI  PICTURE  X.                       CI0082
            12            FG15-CAMGR  PICTURE  9(5)                     CI0082
                          COMPUTATIONAL-3.                              CI0082
            12            FG15-CAMGS  PICTURE  9(5)                     CI0082
                          COMPUTATIONAL-3.                              CI0082
            12            FG15-CAMGN  PICTURE  9(3)                     CI0082
                          COMPUTATIONAL-3.                              CI0082
            12            FG15-CGRMF  PICTURE  X.                       CI0082
            12            FG15-FILLER PICTURE  X(08).                   CI0082
            11            FG15-GR07.                                    CI0082
            12            FG15-GEDLA  PICTURE  9(8).                    CI0082
            12            FG15-GRAID  PICTURE  X(12).                   CI0082
            12            FG15-GRPAP  PICTURE  X(14).                   CI0082
            12            FG15-GEPHNX PICTURE  9(4).                    CI0082
            12            FG15-DPLEF  PICTURE  9(8).                    CI0082
            12            FG15-DPLAM  PICTURE  9(8).                    CI0082
            12            FG15-NCPFN  PICTURE  9(6).                    CI0082
            12            FG15-GEFYE  PICTURE  9(4).                    CI0082
            12            FG15-FILLER PICTURE  X(06).                   CI0082
            12            FG15-GRPAN  PICTURE  X(45).                   CI0082
            12            FG15-CGRPA  PICTURE  99.                      CI0082
            12            FG15-IPRTT7 PICTURE  X.                       CI0082
            12            FG15-GRPED  PICTURE  9(8).                    CI0082
            12            FG15-FILLER PICTURE  X(05).                   CI0082
            12            FG15-GRPLC  PICTURE  99.                      CI0082
            12            FG15-GRPLT  PICTURE  99.                      CI0082
            12            FG15-FILLER PICTURE  X(04).                   CI0082
            12            FG15-GEADI  PICTURE  X.                       CI0082
            12            FG15-GRCFA  PICTURE  S9(11)V99                CI0082
                          COMPUTATIONAL-3.                              CI0082
            12            FG15-GECFY  PICTURE  9(4).                    CI0082
            12            FG15-GECFC  PICTURE  99.                      CI0082
            12            FG15-MEMPL  PICTURE  X(20).                   CI0082
            12            FG15-CAUNIT PICTURE  X(4).                    CI0082
            12            FG15-FILLER PICTURE  X(21).                   CI0082
            12            FG15-GRPPP  PICTURE  999.                     CI0082
            12            FG15-CCORT  PICTURE  9(3).                    CI0082
            12            FG15-CIDRP  PICTURE  99.                      CI0082
            12            FG15-CCDWA  PICTURE  9.                       CI0082
            12            FG15-IERSA  PICTURE  X.                       CI0082
            12            FG15-DERSA  PICTURE  9(8).                    CI0082
            12            FG15-FILLER PICTURE  X(04).                   CI0082
            10            FG15-QITEM  PICTURE  9(3).                    CI0082
            10            FG15-XIMAX  PICTURE  S9(4)                    CI0082
                          BINARY.                                       CI0082
            10            FG15-FILLER PICTURE  X(100).                  CI0082
      *
      ******************************************************************
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *
      ******************************************************************
      *
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0082
          05              DE00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(00653).                  CI0082
       01                 DE10  REDEFINES      DE00.                    CI0082
            10            DE10-DU11.                                    CI0082
            11            DE10-XFONC  PICTURE  X(4).                    CI0082
            11            DE10-MPSBN  PICTURE  X(8).                    CI0082
            11            DE10-XDBDNM PICTURE  X(08).                   CI0082
            11            DE10-XSEGNM PICTURE  X(08).                   CI0082
            11            DE10-XRC    PICTURE  X(02).                   CI0082
            11            DE10-MSEG   PICTURE  X(08).                   CI0082
            11            DE10-XCOKEY PICTURE  X(70).                   CI0082
            11            DE10-CUIBR  PICTURE  X(01).                   CI0082
            11            DE10-CUIBA  PICTURE  X(01).                   CI0082
            11            DE10-IPBIK  PICTURE  X(1).                    CI0082
            10            DE10-DU03.                                    CI0082
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            DE10-CMSSF  PICTURE  XX.                      CI0082
            11            DE10-DU09.                                    CI0082
            12            DE10-CMESA  PICTURE  S9(9)                    CI0082
                          BINARY.                                       CI0082
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0082
                          BINARY.                                       CI0082
            12            DE10-CMESB  PICTURE  S9(9)                    CI0082
                          BINARY.                                       CI0082
            12            DE10-CMSST  PICTURE  S9(9)                    CI0082
                          BINARY.                                       CI0082
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0082
                          BINARY.                                       CI0082
            12            DE10-QELLAA PICTURE  S9(9)                    CI0082
                          BINARY.                                       CI0082
            12            DE10-TMESS4 PICTURE  X(512).                  CI0082
      *
      *
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0082
          05              MS00-SUITE.                                   CI0082
            15       FILLER         PICTURE  X(00542).                  CI0082
       01                 MS03  REDEFINES      MS00.                    CI0082
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0082
                          COMPUTATIONAL-3.                              CI0082
            10            MS03-CMSSF  PICTURE  XX.                      CI0082
            10            MS03-DU09.                                    CI0082
            11            MS03-CMESA  PICTURE  S9(9)                    CI0082
                          BINARY.                                       CI0082
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0082
                          BINARY.                                       CI0082
            11            MS03-CMESB  PICTURE  S9(9)                    CI0082
                          BINARY.                                       CI0082
            11            MS03-CMSST  PICTURE  S9(9)                    CI0082
                          BINARY.                                       CI0082
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0082
                          BINARY.                                       CI0082
            11            MS03-QELLAA PICTURE  S9(9)                    CI0082
                          BINARY.                                       CI0082
            11            MS03-TMESS4 PICTURE  X(512).                  CI0082
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0082
            10            MX11-QMSGS  PICTURE  9(03).                   CI0082
            10            MX11-PJ09                                     CI0082
                          OCCURS       025     TIMES.                   CI0082
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0082
                          COMPUTATIONAL-3.                              CI0082
            11            MX11-CMESB  PICTURE  S9(9)                    CI0082
                          BINARY.                                       CI0082
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PJ10
                                AC14
                                FG15
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0082
      *               *                                   *             CI0082
      *               *INITIALISATIONS                    *             CI0082
      *               *                                   *             CI0082
      *               *************************************.            CI0082
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
      *N02DC.    NOTE *SET ADDRESS FOR DB ACCESSES        *.
       F02DC.                                                           lv10
      *
      *********************************
      ** SET ADDRESSES FOR DB POINTERS*
      **   CT1P - CONTRACT DB         *
      **   GR1P - GROUP    DB         *
      **   CL1P - CLIENT   DB         *
      *********************************
      *.
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR GR1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-GR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR ARAY                                             DOT
           SET ADDRESS OF PD06 TO                                       ADU015
                PCB-ARAY-PTR1.                                          ADU015
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF PE06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
       F02DC-FN. EXIT.
      *N02FA.    NOTE *SEGMENT RE-INITIALIZATION          *.
       F02FA.                                                           lv10
           INITIALIZE  MS03.
       F02FA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0082
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0082
      *               *                                   *             CI0082
      *               *FIN DE TRAITEMENT                  *             CI0082
      *               *                                   *             CI0082
      *               *************************************.            CI0082
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0082
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *EDIT INCOMING PARMS                *
      *               *                                   *
      *               *************************************.
       F30.                                                             lv05
      *
      *********************************
      ** THIS AREA WILL BE USED FOR   *
      ** INITIALIZATION OF WORK AREAS *
      *********************************
      *
      *N30FA.    NOTE *ENSURE HOW IS WRITTEN OR TELE      *.
       F30FA.    IF    PJ10-CHCR = 02                                   lv10
                 OR    PJ10-CHCR = 03
                 NEXT SENTENCE ELSE GO TO     F30FA-FN.
       F30FA-900. GO TO F30FM-FN.
       F30FA-FN. EXIT.
      *N30FM.    NOTE *ELSE... ERROR                      *.
       F30FM.                                                           lv10
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012053 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30FM-FN. EXIT.
      *N30HA.    NOTE *ENSURE CONTRACT IS NUMERIC         *.
       F30HA.    IF    PJ10-CTID NUMERIC                                lv10
                 NEXT SENTENCE ELSE GO TO     F30HA-FN.
       F30HA-900. GO TO F30HM-FN.
       F30HA-FN. EXIT.
      *N30HM.    NOTE *ELSE... ERROR                      *.
       F30HM.                                                           lv10
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012004 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30HM-FN. EXIT.
      *N30JA.    NOTE *EDIT FUNCTION REQUESTED INQ/UPDT   *.
       F30JA.    IF    PJ10-CUPIQ = 'I'                                 lv10
                 OR    PJ10-CUPIQ = 'U'
                 NEXT SENTENCE ELSE GO TO     F30JA-FN.
      *********************************
      ** THIS PGM PERFORM EDITS FOR   *
      ** MANY PROCESSES.  IT FUNCTIONS*
      ** IN BOTH INQ & UPDATE MODES   *
      ** BASED ON THE CALLING PGM.    *
      ** THERE ARE SOME ERRORS THAT   *
      ** ARE ALWAYS CRITCAL.  OTHERS  *
      ** FUNCTION BASED ON THE MODE   *
      ** OF PROCESSING.               *
      ** **************************** *
      *
      *  ---- CONTINUE ----
       F30JA-900. GO TO F30JT-FN.
       F30JA-FN. EXIT.
      *N30JT.    NOTE *ELSE... ERROR                      *.
       F30JT.                                                           lv10
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012596 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30JT-FN. EXIT.
      *N30LA.    NOTE *CALL CI0004 - ACCOUNT ADDRESS      *.            AM0004
       F30LA.                                                           lv10
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
           INITIALIZE      DU07                                         AM0004
           MOVE        PJ10-CTID TO DU07-CTID                           AM0004
           MOVE        NS20-DCACG TO DU07-DCACG                         AM0004
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
           DU07                                                         AM0004
           DE10                                                         AM0004
           MS03.                                                        AM0004
      *N30LB.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F30LB.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F30LB-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0004 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0004 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F30LB-900. GO TO F30LD-FN.
       F30LB-FN. EXIT.
      *N30LD.    NOTE *NO ERRORS                          *.            ADU071
       F30LD.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F30LD-FN. EXIT.
       F30LA-FN. EXIT.
      *N30LE.    NOTE *IF ADDRESS IS UNDELIVERABLE...     *.
       F30LE.    IF    DU07-GEADS = 2                                   lv10
                 NEXT SENTENCE ELSE GO TO     F30LE-FN.
      *N30LH.    NOTE * ...AND THIS IS AN INQUIRY         *.
       F30LH.    IF    PJ10-CUPIQ = 'I'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F30LH-FN.
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        012817 TO MS03-NMESS2                            ADU119
           PERFORM     F98MX THRU F98MX-FN.                             ADU119
       F30LH-900. GO TO F30LJ-FN.
       F30LH-FN. EXIT.
      *N30LJ.    NOTE *...ELSE IF THIS IS AN UPDATE       *.
       F30LJ.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012817 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30LJ-FN. EXIT.
       F30LE-FN. EXIT.
       F30-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *MAINLINE INITIALIZATIONS           *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *
      *********************************
      ** THIS AREA WILL BE USED FOR   *
      ** INITIALIZATION PRIOR TO      *
      ** PROCESSING VALID ACCOUNTS    *
      *********************************
      *
      *N40FA.    NOTE *CALL CI0020 - CAMS ACCTG DATE      *.            AM0020
       F40FA.                                                           lv10
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
      *N40FF.    NOTE *NON-DL1 ERROR                      *.            ADU070
       F40FF.    IF    MS03-NMESS2 > ZERO                               lv15
                 AND   MS03-CMESB > 10                                  ADU070
                 NEXT SENTENCE ELSE GO TO     F40FF-FN.                 ADU070
      *OF A CERTAIN SEVERITY                                            ADU070
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU070
           MOVE        CI0020 TO MS03-TMESS4 (IMS03R : 6)               ADU070
           ADD         +7 TO MS03-QELLAA                                ADU070
           MOVE                     ALL '1' TO FT GO TO F20.            ADU070
       F40FF-900. GO TO F40FH-FN.
       F40FF-FN. EXIT.
      *N40FH.    NOTE *NO ERRORS                          *.            ADU070
       F40FH.                                                           lv15
           INITIALIZE  MS03.                                            ADU070
       F40FH-FN. EXIT.
       F40FA-FN. EXIT.
      *N40HA.    NOTE *READ SOURCE CONTRACT               *.
       F40HA.                                                           lv10
      *
           MOVE        PJ10-CTID TO S-CTU01-CTID
      *
           PERFORM     F94CT THRU F94CT-FN.
       F40HA-FN. EXIT.
      *N40HH.    NOTE *IF CONTRACT WAS NOT FOUND          *.
       F40HH.    IF    IK = '1'                                         lv10
                 NEXT SENTENCE ELSE GO TO     F40HH-FN.
      *
           MOVE        012011 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HH-900. GO TO F40HJ-FN.
       F40HH-FN. EXIT.
      *N40HJ.    NOTE *ELSE, CONTRACT WAS FOUND           *.
       F40HJ.                                                           lv10
      *MOVE PRODUCT CODE TO PASS AREA
      *
           MOVE        CT01-PRCOD TO PJ10-PRCOD.
       F40HJ-FN. EXIT.
      *N40IM.    NOTE *CALL CI0003 - ACCT OWNER/BENE      *.            AM0003
       F40IM.                                                           lv10
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
           MOVE        PJ10-CTID TO FA04-CTID                           AM0003
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
      *N40IR.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F40IR.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F40IR-FN.                 ADU071
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
       F40IR-900. GO TO F40IU-FN.
       F40IR-FN. EXIT.
      *N40IU.    NOTE *NO ERRORS                          *.            ADU071
       F40IU.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F40IU-FN. EXIT.
      *N40NA.    NOTE *IF NO OWNERSHIP LINES FOUND        *.
       F40NA.    IF    FA04-CTTLN1 = SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F40NA-FN.
           MOVE        012794 TO MS03-NMESS2.
                 IF    PJ10-CUPIQ NOT = 'I'                             DOT
           PERFORM     F98ET THRU F98ET-FN.
           PERFORM     F98MX THRU F98MX-FN.                             DOT
       F40NA-FN. EXIT.
       F40IM-FN. EXIT.
       F40-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATE ACCOUNT CLIENTS           *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      **                              *
      **                              *
      *N50BA.    NOTE *CALL CI0018 - ACCT CLIENTS         *.            AM0018
       F50BA.                                                           lv10
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
           MOVE        PJ10-CTID TO AC14-CTID                           AM0018
           MOVE        NS20-DCACG TO AC14-DCACG                         AM0018
           MOVE        25 TO AC14-XIMAX                                 AM0018
      *
           MOVE        'A' TO AC14-CRROL
      *
           MOVE        'Y' TO AC14-IPOCH                                AM0018
           SET CI0018B-PCB-CT1P-PTR1 TO                                 AM0018
                       PCB-CT1P-PTR1                                    AM0018
           INITIALIZE      DE10-DU03                                    AM0018
           CALL        CI0018 USING                                     AM0018
           DFHEIBLK                                                     AM0018
           DFHCOMMAREA                                                  AM0018
           DLIUIBII                                                     AM0018
           CI0018B-PCB-ADDRESS-LIST                                     AM0018
           AC14                                                         AM0018
           DE10                                                         AM0018
           MS03.                                                        AM0018
      *N50BF.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F50BF.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F50BF-FN.                 ADU071
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
       F50BF-900. GO TO F50BH-FN.
       F50BF-FN. EXIT.
      *N50BH.    NOTE *NO ERRORS                          *.            ADU071
       F50BH.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F50BH-FN. EXIT.
       F50BA-FN. EXIT.
      *N50BM.    NOTE *PERFORM ACCOUNT CLIENT(S) EDITS    *.
       F50BM.                                                           lv10
      *
      *********************************
      ** IF NOT IN INQUIRY MODE       *
      **  ERR & RETURN TO CALLING PGM *
      ** ELSE                         *
      **  LOAD ERR TO ARRAY OF INFO   *
      **  ERRORS AND CONTINUE IN PGM  *
      *********************************
      *
      *N50CB.    NOTE *IF NO CLIENT ADDRESS FOUND         *.
       F50CB.    IF    AC14-QITEM = 0                                   lv15
                 NEXT SENTENCE ELSE GO TO     F50CB-FN.
           MOVE        012031 TO MS03-NMESS2.
                 IF    PJ10-CUPIQ NOT = 'I'                             DOT
           PERFORM     F98ET THRU F98ET-FN.
           PERFORM     F98MX THRU F98MX-FN.                             DOT
       F50CB-FN. EXIT.
      *N50CH.    NOTE *IF NO TAXPAYER CLIENT FOUND        *.
       F50CH.    IF    AC14-CLID01 NOT NUMERIC                          lv15
                 NEXT SENTENCE ELSE GO TO     F50CH-FN.
           MOVE        012792 TO MS03-NMESS2.
                 IF    PJ10-CUPIQ NOT = 'I'                             DOT
           PERFORM     F98ET THRU F98ET-FN.
           PERFORM     F98MX THRU F98MX-FN.                             DOT
       F50CH-900. GO TO F50EA-FN.
       F50CH-FN. EXIT.
      *N50EA.    NOTE *ELSE VERIFY CLIENT WITH TAXPAYER   *.
       F50EA.         EXIT.                                             lv15
      *N50ED.    NOTE *SET UP & READ CL01 FOR TAX PAYER   *.
       F50ED.                                                           lv20
      *
      *********************************
      ** SET UP SSA FOR CL01 SEGMENT  *
      *********************************
      *
           MOVE        AC14-CLID01 TO S-CLU01-CLID
      *
      *********************************
      ** READ THE CL01 SEGMENT        *
      *********************************
      *
           PERFORM     F94CL THRU F94CL-FN.
       F50ED-FN. EXIT.
      *N50EH.    NOTE *IF TAX PAYER NOT ON CLIENT DB      *.
       F50EH.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F50EH-FN.
      *
      *********************************
      ** SEND AN ERROR MESSAGE IF CL01*
      ** SEGMENT NOT FOUND.           *
      *********************************
      *
           MOVE        012788 TO MS03-NMESS2.
                 IF    PJ10-CUPIQ NOT = 'I'                             DOT
           PERFORM     F98ET THRU F98ET-FN.
           PERFORM     F98MX THRU F98MX-FN.                             DOT
       F50EH-FN. EXIT.
      *N50EM.    NOTE *IF NO TIN SPECIFIED FOR TAXPAYER   *.
       F50EM.    IF    CL01-CLTIN = 0                                   lv20
                 NEXT SENTENCE ELSE GO TO     F50EM-FN.
      *N50EO.    NOTE *IF CERTAIN PROG - INFO. ERROR      *.
       F50EO.    IF    PJ10-PROGR = 'CI9001'                            lv25
                 OR    PJ10-PROGR = 'CI9013'
                 NEXT SENTENCE ELSE GO TO     F50EO-FN.
      *
      *INFO. ERROR WHEN RETRIEVING TRAN
      *TYPES (CI9001) AND WHEN
      *RETRIEVING PRODUCTS THAT CAN BE
      *USED IN SETTING UP A NEW ACCT
      *(CI9013).
      *
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        012436 TO MS03-NMESS2                            ADU119
           PERFORM     F98MX THRU F98MX-FN.                             ADU119
       F50EO-FN. EXIT.
       F50EM-FN. EXIT.
       F50EA-FN. EXIT.
       F50BM-FN. EXIT.
       F50-FN.   EXIT.
      *N52.      NOTE *************************************.
      *               *                                   *
      *               *REVIEW RETURNED ARRAY              *
      *               *                                   *
      *               *************************************.
       F52.           EXIT.                                             lv05
      *N52BA.    NOTE *REVIEW ARRAY OF ACCOUNT CLIENTS    *.
       F52BA.                                                           lv10
           MOVE        ZERO TO 7-WK-COUNT
           MOVE        SPACES TO 7-WK-CLID
           7-WK1-CLID
           7-WK2-CLID
           MOVE        +1 TO 7-WK-QITEM
           MOVE        'N' TO 7-PART-IAIND
           INITIALIZE  WS21.
       F52BA-FN. EXIT.
      *N52DA.    NOTE *LOOP WHILE MORE ACCT CLIENTS       *.
       F52DA.    IF    7-WK-QITEM NOT >                                 lv10
                       AC14-QITEM
                 NEXT SENTENCE ELSE GO TO     F52DA-FN.
           MOVE        AC14-DU21 (7-WK-QITEM) TO WS21.
      *N52DG.    NOTE *IF 'PARTICIPANT' SET SWITCH        *.
       F52DG.    IF    WS21-CLCTRC = 006                                lv15
                 NEXT SENTENCE ELSE GO TO     F52DG-FN.
      *
           MOVE        'Y' TO 7-PART-IAIND.
       F52DG-FN. EXIT.
      *N52FA.    NOTE *IF CLIENT IS AN 'OWNER'            *.
       F52FA.    IF    WS21-CLCTRC = 001                                lv15
                 NEXT SENTENCE ELSE GO TO     F52FA-FN.
           ADD         +1 TO 7-WK-COUNT.
      *N52FD.    NOTE *IF NO OTHER OWNERS FOUND YET       *.
       F52FD.    IF    7-WK1-CLID = SPACES                              lv20
                 NEXT SENTENCE ELSE GO TO     F52FD-FN.
      **  STORE FOR 2ND OWNER CHECKING
      **
           MOVE        WS21-CLID TO 7-WK1-CLID.
       F52FD-900. GO TO F52FG-FN.
       F52FD-FN. EXIT.
      *N52FG.    NOTE *ELSE - STORE IN 2ND OWNER FIELD    *.
       F52FG.                                                           lv20
      **
           MOVE        WS21-CLID TO 7-WK2-CLID.
       F52FG-FN. EXIT.
       F52FA-FN. EXIT.
      *N52FM.    NOTE *INCREMENT ARRAY INDEX              *.
       F52FM.                                                           lv15
           ADD         +1 TO 7-WK-QITEM.
       F52FM-FN. EXIT.
       F52DA-900. GO TO F52DA.
       F52DA-FN. EXIT.
      *N52IA.    NOTE *IF NO OWNERS WERE FND IN ARRAY     *.
       F52IA.    IF    7-WK-COUNT = 0                                   lv10
                 NEXT SENTENCE ELSE GO TO     F52IA-FN.
           MOVE        012233 TO MS03-NMESS2.
                 IF    PJ10-CUPIQ NOT = 'I'                             DOT
           PERFORM     F98ET THRU F98ET-FN.
           PERFORM     F98MX THRU F98MX-FN.                             DOT
       F52IA-900. GO TO F52IM-FN.
       F52IA-FN. EXIT.
      *N52IM.    NOTE *ELSE  OWNERS WERE FND              *.
       F52IM.                                                           lv10
      *
      *N52KA.    NOTE *IF 2ND ONWER FLD NOT TAXPAYER      *.
       F52KA.    IF    7-WK1-CLID NOT = AC14-CLID01                     lv15
                 NEXT SENTENCE ELSE GO TO     F52KA-FN.
      *STORE ID AS A 2ND OWNER
      *
           MOVE        7-WK1-CLID TO 7-WK-CLID.
       F52KA-900. GO TO F52KD-FN.
       F52KA-FN. EXIT.
      *N52KD.    NOTE *ELSE                               *.
       F52KD.         EXIT.                                             lv15
      *N52KG.    NOTE *IF NO ID STORED IN 2ND WORK FLD    *.
       F52KG.    IF    7-WK2-CLID = SPACES                              lv20
                 NEXT SENTENCE ELSE GO TO     F52KG-FN.
      *STORE THE 1ST WORK CLID AS THE
      *SECOND OWNER.
           MOVE        7-WK1-CLID TO 7-WK-CLID.
       F52KG-900. GO TO F52KJ-FN.
       F52KG-FN. EXIT.
      *N52KJ.    NOTE *ELSE STORE 2ND WORK FLD --         *.
       F52KJ.                                                           lv20
      *STORE THE 2ND WORK CLID AS THE
      *SECOND OWNER.
           MOVE        7-WK2-CLID TO 7-WK-CLID.
       F52KJ-FN. EXIT.
       F52KD-FN. EXIT.
      *N52NA.    NOTE *IF 2ND OWNER ID FIELD IS INVALID   *.
       F52NA.    IF    7-WK-CLID NOT NUMERIC                            lv15
                 NEXT SENTENCE ELSE GO TO     F52NA-FN.
           MOVE        012793 TO MS03-NMESS2.
                 IF    PJ10-CUPIQ NOT = 'I'                             DOT
           PERFORM     F98ET THRU F98ET-FN.
           PERFORM     F98MX THRU F98MX-FN.                             DOT
       F52NA-FN. EXIT.
      *N52NC.    NOTE *VERIFY 2ND OWNER EXISTS            *.
       F52NC.         EXIT.                                             lv15
      *N52NF.    NOTE *SET UP & READ CL01 FOR 2ND OWNER   *.
       F52NF.                                                           lv20
      *
      *********************************
      ** SET UP SSA FOR CL01 SEGMENT  *
      *********************************
      *
           MOVE        7-WK-CLID TO S-CLU01-CLID
      *
      *********************************
      ** READ THE CL01 SEGMENT        *
      *********************************
      *
           PERFORM     F94CL THRU F94CL-FN.
       F52NF-FN. EXIT.
      *N52NJ.    NOTE *IF SECOND OWNER NOT FOUND          *.
       F52NJ.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F52NJ-FN.
      *
           MOVE        012784 TO MS03-NMESS2.
                 IF    PJ10-CUPIQ NOT = 'I'                             DOT
           PERFORM     F98ET THRU F98ET-FN.
           PERFORM     F98MX THRU F98MX-FN.                             DOT
       F52NJ-FN. EXIT.
       F52NC-FN. EXIT.
       F52IM-FN. EXIT.
       F52-FN.   EXIT.
      *N60BA.    NOTE *CALL CI0019 - ACCOUNT GROUPS       *.            AM0019
       F60BA.                                                           lv10
      *                                                                 AM0019
      *********************************                                 AM0019
      ** THIS MODULE WILL READ THE    *                                 AM0019
      ** CONTRACT DATABASE TO GET ALL *                                 AM0019
      ** THE GROUPS FOR THE ACCOUNT   *                                 AM0019
      ** NUMBER.                      *                                 AM0019
      *********************************                                 AM0019
      *                                                                 AM0019
           INITIALIZE      FG15                                         AM0019
           MOVE        PJ10-CTID TO FG15-CTID                           AM0019
           MOVE        NS20-DCACG TO FG15-DCACG                         AM0019
           MOVE        15 TO FG15-XIMAX                                 AM0019
           MOVE        'Y' TO FG15-IPOCH                                AM0019
           SET CI0019C-PCB-CT1P-PTR1 TO                                 AM0019
                       PCB-CT1P-PTR1                                    AM0019
           SET CI0019C-PCB-GR1P-PTR1 TO                                 AM0019
                       PCB-GR1P-PTR1                                    AM0019
           INITIALIZE      DE10-DU03                                    AM0019
           CALL        CI0019 USING                                     AM0019
           DFHEIBLK                                                     AM0019
           DFHCOMMAREA                                                  AM0019
           DLIUIBII                                                     AM0019
           CI0019C-PCB-ADDRESS-LIST                                     AM0019
           FG15                                                         AM0019
           DE10                                                         AM0019
           MS03.                                                        AM0019
      *N60BF.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F60BF.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F60BF-FN.                 ADU071
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
       F60BF-900. GO TO F60BH-FN.
       F60BF-FN. EXIT.
      *N60BH.    NOTE *NO ERRORS                          *.            ADU071
       F60BH.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F60BH-FN. EXIT.
       F60BA-FN. EXIT.
      *N60BM.    NOTE *PERFORM ACCOUNT GROUP EDITS        *.
       F60BM.                                                           lv10
      *
      *********************************
      ** IF NOT IN INQUIRY MODE       *
      **  ERR & RETURN TO CALLING PGM *
      ** ELSE                         *
      **  LOAD ERR TO ARRAY OF INFO   *
      **  ERRORS AND CONTINUE IN PGM  *
      *********************************
      *
      *N60CB.    NOTE *IF NO ACCOUNT GROUPS FOUND - ERR   *.
       F60CB.    IF    FG15-QITEM = 0                                   lv15
                 NEXT SENTENCE ELSE GO TO     F60CB-FN.
           MOVE        012246 TO MS03-NMESS2.
                 IF    PJ10-CUPIQ NOT = 'I'                             DOT
           PERFORM     F98ET THRU F98ET-FN.
           PERFORM     F98MX THRU F98MX-FN.                             DOT
       F60CB-FN. EXIT.
       F60BM-FN. EXIT.
      *N62.      NOTE *************************************.
      *               *                                   *
      *               *REVIEW CI0019 ARRAY RETURNED       *
      *               *                                   *
      *               *************************************.
       F62.           EXIT.                                             lv05
      *N62BA.    NOTE *REVIEW ARRAY OF ACCOUNT GROUPS     *.
       F62BA.                                                           lv10
           MOVE        +1 TO 7-WK-QITEM
           MOVE        'N' TO 7-HSHLD-IAIND
           INITIALIZE  WK10.
       F62BA-FN. EXIT.
      *N62DA.    NOTE *LOOP WHILE MORE GROUPS IN ARRAY    *.
       F62DA.    IF    7-WK-QITEM NOT >                                 lv10
                       FG15-QITEM
                 NEXT SENTENCE ELSE GO TO     F62DA-FN.
           MOVE        FG15-CT10 (7-WK-QITEM) TO WK10.
      *N62DG.    NOTE *IF 'HOUSEHOLD' GROUP FOUND         *.
       F62DG.    IF    WK10-GRIDC = 001                                 lv15
                 NEXT SENTENCE ELSE GO TO     F62DG-FN.
      *
           MOVE        'Y' TO 7-HSHLD-IAIND.
       F62DG-FN. EXIT.
      *N62FA.    NOTE *IF NO GROUP ID IS FOUND FOR        *.
       F62FA.    IF    (WK10-GRID = ZERO)                               lv15
                 OR    (WK10-GRIDC = 002
                 AND   FG15-GRPLT (7-WK-QITEM)
                       = ZERO)
                 NEXT SENTENCE ELSE GO TO     F62FA-FN.
      *THE ACCOUNT - NO GR01 FOUND
      *   OR
      *IN A PENSION GROUP AND NO GR07
      *(PENSION SEG) FND -  ISSUE ERR
      *
           MOVE        12227 TO MS03-NMESS2.
                 IF    PJ10-CUPIQ NOT = 'I'                             DOT
           PERFORM     F98ET THRU F98ET-FN.
           PERFORM     F98MX THRU F98MX-FN.                             DOT
       F62FA-FN. EXIT.
      *N62FM.    NOTE *INCREMENT ARRAY INDEX              *.
       F62FM.                                                           lv15
           ADD         +1 TO 7-WK-QITEM.
       F62FM-FN. EXIT.
       F62DA-900. GO TO F62DA.
       F62DA-FN. EXIT.
      *N62IA.    NOTE *IF HH GRP AND PARTICIPANT ROLE     *.
       F62IA.    IF    7-HSHLD-IAIND = 'Y'                              lv10
                 AND   7-PART-IAIND = 'Y'
                 NEXT SENTENCE ELSE GO TO     F62IA-FN.
           MOVE        012785 TO MS03-NMESS2.
                 IF    PJ10-CUPIQ NOT = 'I'                             DOT
           PERFORM     F98ET THRU F98ET-FN.
           PERFORM     F98MX THRU F98MX-FN.                             DOT
       F62IA-FN. EXIT.
       F62-FN.   EXIT.
      *N65HA.    NOTE *EDIT FOR NO SERVICE ADVISOR        *.
       F65HA.    IF    PJ10-PROGR = 'CI9013'                            lv10
                 NEXT SENTENCE ELSE GO TO     F65HA-FN.
      *
      *********************************
      ** THIS EDIT IS INFORMATIONAL TO*
      ** THE PROGRAM THAT RETRIEVES   *
      ** VALID PRODUCTS FOR A NEW ACCT*
      ** SET UP.                      *
      *********************************
      *
           MOVE        PJ10-CTID TO S-CTU01-CTID
      *
      *POSITION TO THE FIRST CT17
           MOVE        'F----' TO S-CT17-CCOD
      *
           MOVE        ZEROS TO 7-CT18-GESQ2
           MOVE        008 TO 7-CT18-CMACR
           MOVE        ZEROS TO 7-CT18-GERED
      *
           PERFORM     F96CT THRU F96CT-FN.
      *N65HH.    NOTE *IF AN ACCT PLANNER NOT FOUND       *.
       F65HH.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F65HH-FN.
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        012307 TO MS03-NMESS2                            ADU119
           PERFORM     F98MX THRU F98MX-FN.                             ADU119
       F65HH-FN. EXIT.
       F65HA-FN. EXIT.
      *N65KD.    NOTE *CHECK FOR NAME OF CLIENT           *.
       F65KD.                                                           lv10
      *RETURN ERROR 12238 IF THE CLIENT
      *HAS NO NAME INFORMATION.
      *
           MOVE        'Y' TO 7-CLIENT-HAS-NAME-INFO.
      *N65KH.    NOTE *CALL CI0081 - ARRANGEMENT CLIENT   *.            AM0081
       F65KH.                                                           lv15
      *                                                                 AM0081
      *********************************                                 AM0081
      ** THIS MODULE WILL READ THE    *                                 AM0081
      ** CAMS CONTRACT, CLIENT,       *                                 AM0081
      ** ARRANGEMENT, AND ARRANGEMENT *                                 AM0081
      ** INDEX.                       *                                 AM0081
      *********************************                                 AM0081
      *                                                                 AM0081
           INITIALIZE  WZ05                                             AM0081
           MOVE        PJ10-CTID TO                                     AM0081
           WZ05-CTID                                                    AM0081
           MOVE        PJ10-MAPPN TO                                    AM0081
           WZ05-MAPPN                                                   AM0081
           SET CI0081D-PCB-CT1P-PTR1 TO                                 AM0081
                         PCB-CT1P-PTR1                                  AM0081
           SET CI0081D-PCB-CL1P-PTR1 TO                                 AM0081
                         PCB-CL1P-PTR1                                  AM0081
           SET CI0081D-PCB-AR1P-PTR1 TO                                 AM0081
                         PCB-AR1P-PTR1                                  AM0081
           SET CI0081D-PCB-ARAY-PTR1 TO                                 AM0081
                         PCB-ARAY-PTR1                                  AM0081
           INITIALIZE  DE10-DU03                                        AM0081
           CALL        CI0081 USING                                     AM0081
           DFHEIBLK                                                     AM0081
           DFHCOMMAREA                                                  AM0081
           DLIUIBII                                                     AM0081
           CI0081D-PCB-ADDRESS-LIST                                     AM0081
           WZ05                                                         AM0081
           DE10                                                         AM0081
           MS03                                                         AM0081
           MX11.                                                        AM0081
       F65KH-FN. EXIT.
      *N65KM.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F65KM.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F65KM-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0081 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0081 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F65KM-900. GO TO F65KO-FN.
       F65KM-FN. EXIT.
      *N65KO.    NOTE *NO ERRORS                          *.            ADU071
       F65KO.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F65KO-FN. EXIT.
      *N65KQ.    NOTE *READ THE CL01                      *.
       F65KQ.                                                           lv15
      *
           MOVE        WZ05-CLID TO S-CLU01-CLID
           PERFORM     F94CL THRU F94CL-FN.
      *N65KV.    NOTE *IF THE CL01 READ WAS OKAY          *.
       F65KV.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F65KV-FN.
      *N65KZ.    NOTE *IF IT'S A PERSON CLIENT            *.
       F65KZ.    IF    CL01-CLTYP = 'P'                                 lv25
                 NEXT SENTENCE ELSE GO TO     F65KZ-FN.
      *
      *READ THE CL03
      *
           PERFORM     F94CM THRU F94CM-FN.
                 IF    IK = '1'                                         DOT
           MOVE        'N' TO 7-CLIENT-HAS-NAME-INFO.
       F65KZ-FN. EXIT.
      *N65MD.    NOTE *IF IT'S AN ORGANIZATION CLIENT     *.
       F65MD.    IF    CL01-CLTYP = 'O'                                 lv25
                 NEXT SENTENCE ELSE GO TO     F65MD-FN.
      *
      *READ THE CL12
      *
           PERFORM     F94CN THRU F94CN-FN.
                 IF    IK = '1'                                         DOT
           MOVE        'N' TO 7-CLIENT-HAS-NAME-INFO.
       F65MD-FN. EXIT.
       F65KV-900. GO TO F65MJ-FN.
       F65KV-FN. EXIT.
      *N65MJ.    NOTE *IF THE CL01 READ WAS NOT OKAY      *.
       F65MJ.                                                           lv20
           MOVE        'N' TO 7-CLIENT-HAS-NAME-INFO.
       F65MJ-FN. EXIT.
      *N65MO.    NOTE *IF THERE IS NO NAME INFO           *.
       F65MO.    IF    7-CLIENT-HAS-NAME-INFO = 'N'                     lv20
                 NEXT SENTENCE ELSE GO TO     F65MO-FN.
      *
      *SEND THE ERROR MESSAGE
      *
           MOVE        012238 TO MS03-NMESS2.
                 IF    PJ10-CUPIQ NOT = 'I'                             DOT
           PERFORM     F98ET THRU F98ET-FN.
           PERFORM     F98MX THRU F98MX-FN.                             DOT
       F65MO-FN. EXIT.
       F65KQ-FN. EXIT.
       F65KD-FN. EXIT.
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
           PC06 CL01                                                    ADU026
           S-CLU01-SSA                                                  ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CL-FN. EXIT.
      *N94CM.    NOTE *CALL GU ON CL03                    *.            ADU026
       F94CM.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PC06 CL03                                                    ADU026
           S-CLU01-SSA S-CL03-SSA                                       ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CM-FN. EXIT.
      *N94CN.    NOTE *CALL GU ON CL12                    *.            ADU026
       F94CN.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PC06 CL12                                                    ADU026
           S-CLU01-SSA S-CL12-SSA                                       ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CN-FN. EXIT.
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
      *N96CT.    NOTE *CALL GU ON CT18                    *.            ADU026
       F96CT.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CT18                                                    ADU026
           S-CTU01-SSA S-CT17-SSA                                       ADU026
           7-CT18-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F96CT-FN. EXIT.
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
           MOVE        IMX11L TO MX11-QMSGS                             ADU102
           MOVE        0 TO MS03-NMESS2.
       F98MX-FN. EXIT.
       F98-FN.   EXIT.
