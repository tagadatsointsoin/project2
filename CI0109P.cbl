       IDENTIFICATION DIVISION.                                         CI0109
       PROGRAM-ID.  CI0109P.                                            CI0109
      *AUTHOR.         MD76 - GENERAL EDITS.                            CI0109
      *DATE-COMPILED.   09/08/14.                                       CI0109
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
       ENVIRONMENT DIVISION.                                            CI0109
       CONFIGURATION SECTION.                                           CI0109
       SOURCE-COMPUTER. IBM-370.                                        CI0109
       OBJECT-COMPUTER. IBM-370.                                        CI0109
       DATA DIVISION.                                                   CI0109
       WORKING-STORAGE SECTION.                                         CI0109

      *PASS AREA TO/FROM CI0006
      *!WF DSP=CA DSL=DU SEL=05 FOR=I LEV=1 PLT=CA
       01                 CA00.                                         CI0109
          05              CA00-SUITE.                                   CI0109
            15       FILLER         PICTURE  X(00435).                  CI0109
       01                 CA05  REDEFINES      CA00.                    CI0109
            10            CA05-C199.                                    CI0109
            11            CA05-CLID.                                    CI0109
            12            CA05-CLIDO  PICTURE  9(3).                    CI0109
            12            CA05-CLIDN.                                   CI0109
            13            CA05-CLIDNP PICTURE  X(12).                   CI0109
            13            CA05-CLIDND PICTURE  9(8).                    CI0109
            10            CA05-GECSQ1 PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CA05-DCACG  PICTURE  9(8).                    CI0109
            10            CA05-FILLER PICTURE  X(100).                  CI0109
            10            CA05-CL24.                                    CI0109
            11            CA05-GELL   PICTURE  9(4)                     CI0109
                          BINARY.                                       CI0109
            11            CA05-CL24K.                                   CI0109
            12            CA05-GECSQ  PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CA05-GECSD  PICTURE  9(8).                    CI0109
            11            CA05-GECED  PICTURE  9(8).                    CI0109
            11            CA05-CREQ2  PICTURE  X.                       CI0109
            11            CA05-FILLER PICTURE  X(4).                    CI0109
            11            CA05-GECTA  PICTURE  X.                       CI0109
            11            CA05-GELCD  PICTURE  9(8).                    CI0109
            11            CA05-GEADS  PICTURE  9.                       CI0109
            11            CA05-GECIT  PICTURE  X(25).                   CI0109
            11            CA05-GECTRY PICTURE  X(20).                   CI0109
            11            CA05-GECTY  PICTURE  9(3).                    CI0109
            11            CA05-GEPCD  PICTURE  X(12).                   CI0109
            11            CA05-GEST   PICTURE  X(8).                    CI0109
            11            CA05-IRESA  PICTURE  X.                       CI0109
            11            CA05-FILLER PICTURE  X(8).                    CI0109
            11            CA05-GESAD  PICTURE  X(30)                    CI0109
                          OCCURS       003     TIMES.                   CI0109
            10            CA05-FILLER PICTURE  X(100).                  CI0109
      *
       01                 CL01.                                         CI0109
            10            CL01-CL01K.                                   CI0109
            11            CL01-C199.                                    CI0109
            12            CL01-CLID.                                    CI0109
            13            CL01-CLIDO  PICTURE  9(3).                    CI0109
            13            CL01-CLIDN.                                   CI0109
            14            CL01-CLIDNP PICTURE  X(12).                   CI0109
            14            CL01-CLIDND PICTURE  9(8).                    CI0109
            10            CL01-GECKD  PICTURE  9.                       CI0109
            10            CL01-GEMDA  PICTURE  9(8).                    CI0109
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0109
                          BINARY.                                       CI0109
            10            CL01-GECUC  PICTURE  99.                      CI0109
            10            CL01-CLDOR  PICTURE  9(8).                    CI0109
            10            CL01-CLLNG  PICTURE  XX.                      CI0109
            10            CL01-GESLC  PICTURE  99.                      CI0109
            10            CL01-CLTYP  PICTURE  X.                       CI0109
            10            CL01-CLCLS  PICTURE  9(3).                    CI0109
            10            CL01-CLTWRC PICTURE  99.                      CI0109
            10            CL01-CLPVC  PICTURE  99.                      CI0109
            10            CL01-CLIND  PICTURE  9(3).                    CI0109
            10            CL01-CLTRC  PICTURE  99.                      CI0109
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CL01-AYSIDA PICTURE  9(3).                    CI0109
            10            CL01-AYSID  PICTURE  9(5).                    CI0109
            10            CL01-CLSTR  PICTURE  9(2).                    CI0109
            10            CL01-CLC11  PICTURE  X.                       CI0109
            10            CL01-CLTIN  PICTURE  9(12).                   CI0109
            10            CL01-CLTND  PICTURE  9(8).                    CI0109
            10            CL01-CLTINC PICTURE  9.                       CI0109
            10            CL01-CCDWA  PICTURE  9.                       CI0109
            10            CL01-CICES  PICTURE  X.                       CI0109
            10            CL01-CLTRA  PICTURE  9(2).                    CI0109
            10            CL01-DIRSY  PICTURE  9(4)                     CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CL01-CFEDS  PICTURE  X.                       CI0109
            10            CL01-FILLER PICTURE  X(06).                   CI0109
       01                 CL24.                                         CI0109
            10            CL24-GELL   PICTURE  9(4)                     CI0109
                          BINARY.                                       CI0109
            10            CL24-CL24K.                                   CI0109
            11            CL24-GECSQ  PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CL24-GECSD  PICTURE  9(8).                    CI0109
            10            CL24-GECED  PICTURE  9(8).                    CI0109
            10            CL24-CREQ2  PICTURE  X.                       CI0109
            10            CL24-FILLER PICTURE  X(4).                    CI0109
            10            CL24-GECTA  PICTURE  X.                       CI0109
            10            CL24-GELCD  PICTURE  9(8).                    CI0109
            10            CL24-GEADS  PICTURE  9.                       CI0109
            10            CL24-GECIT  PICTURE  X(25).                   CI0109
            10            CL24-GECTRY PICTURE  X(20).                   CI0109
            10            CL24-GECTY  PICTURE  9(3).                    CI0109
            10            CL24-GEPCD  PICTURE  X(12).                   CI0109
            10            CL24-GEST   PICTURE  X(8).                    CI0109
            10            CL24-IRESA  PICTURE  X.                       CI0109
            10            CL24-FILLER PICTURE  X(8).                    CI0109
            10            CL24-GESAD  PICTURE  X(30)                    CI0109
                          OCCURS       003     TIMES.                   CI0109
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0006           PIC X(8) VALUE 'CI0006P '.                  AM0006
       01                 CT01.                                         CI0109
            10            CT01-CT01K.                                   CI0109
            11            CT01-C299.                                    CI0109
            12            CT01-CTID.                                    CI0109
            13            CT01-CTIDA  PICTURE  9(3).                    CI0109
            13            CT01-CTIDN.                                   CI0109
            14            CT01-CTIDNP PICTURE  X(13).                   CI0109
            14            CT01-CTIDND PICTURE  9(11).                   CI0109
            10            CT01-GECKD  PICTURE  9.                       CI0109
            10            CT01-GEMDA  PICTURE  9(8).                    CI0109
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0109
                          BINARY.                                       CI0109
            10            CT01-GECUC  PICTURE  99.                      CI0109
            10            CT01-CTAUL  PICTURE  9(3).                    CI0109
            10            CT01-DIRAC  PICTURE  9(4).                    CI0109
            10            CT01-CTCCI  PICTURE  X.                       CI0109
            10            CT01-CTCUS  PICTURE  999.                     CI0109
            10            CT01-CTEFD  PICTURE  9(8).                    CI0109
            10            CT01-CTIAD  PICTURE  9(8).                    CI0109
            10            CT01-CLCUS  PICTURE  99.                      CI0109
            10            CT01-CAMMB  PICTURE  X(3).                    CI0109
            10            CT01-CKPMM  PICTURE  X.                       CI0109
            10            CT01-CTLAD  PICTURE  9(8).                    CI0109
            10            CT01-IPERS  PICTURE  X.                       CI0109
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CT01-CTLAT  PICTURE  9(8).                    CI0109
            10            CT01-CTLATC PICTURE  9(6).                    CI0109
            10            CT01-IMEGA  PICTURE  X.                       CI0109
            10            CT01-DIRAB  PICTURE  9(8).                    CI0109
            10            CT01-COLRQ  PICTURE  X.                       CI0109
            10            CT01-ZDA04  PICTURE  X(4).                    CI0109
            10            CT01-CTLPD  PICTURE  9(8).                    CI0109
            10            CT01-CIRASP PICTURE  9.                       CI0109
            10            CT01-CIRATP PICTURE  99.                      CI0109
            10            CT01-DRTHC  PICTURE  9(8).                    CI0109
            10            CT01-CPPTC  PICTURE  X.                       CI0109
            10            CT01-ZDA06  PICTURE  X(6).                    CI0109
            10            CT01-CTACD  PICTURE  9(8).                    CI0109
            10            CT01-CTNLI  PICTURE  X.                       CI0109
            10            CT01-CTRHO  PICTURE  9(8).                    CI0109
            10            CT01-CTSGD  PICTURE  9(8).                    CI0109
            10            CT01-CPATP  PICTURE  X(1).                    CI0109
            10            CT01-IRSTA  PICTURE  X.                       CI0109
            10            CT01-CTSTA  PICTURE  99.                      CI0109
            10            CT01-CTSSC  PICTURE  99.                      CI0109
            10            CT01-PRLIN  PICTURE  9(3).                    CI0109
            10            CT01-PRCOD  PICTURE  9(5).                    CI0109
            10            CT01-PRSCD  PICTURE  X(9).                    CI0109
            10            CT01-CTLNI  PICTURE  X.                       CI0109
            10            CT01-AYSIDA PICTURE  9(3).                    CI0109
            10            CT01-AYSID  PICTURE  9(5).                    CI0109
            10            CT01-CTBMC  PICTURE  99.                      CI0109
            10            CT01-CINAR  PICTURE  99.                      CI0109
            10            CT01-CPHTR  PICTURE  X.                       CI0109
            10            CT01-CDSTR  PICTURE  XX.                      CI0109
            10            CT01-CQACT  PICTURE  999.                     CI0109
            10            CT01-CIRAS  PICTURE  999.                     CI0109
            10            CT01-CIRAT  PICTURE  999.                     CI0109
            10            CT01-CLRAY  PICTURE  9(5).                    CI0109
            10            CT01-CATTP  PICTURE  X.                       CI0109
       01                 CT07.                                         CI0109
            10            CT07-CT07K.                                   CI0109
            11            CT07-C199.                                    CI0109
            12            CT07-CLID.                                    CI0109
            13            CT07-CLIDO  PICTURE  9(3).                    CI0109
            13            CT07-CLIDN.                                   CI0109
            14            CT07-CLIDNP PICTURE  X(12).                   CI0109
            14            CT07-CLIDND PICTURE  9(8).                    CI0109
       01                 CT09.                                         CI0109
            10            CT09-A100.                                    CI0109
            11            CT09-GELL   PICTURE  9(4)                     CI0109
                          BINARY.                                       CI0109
            11            CT09-CT09K.                                   CI0109
            12            CT09-CLCTRC PICTURE  9(3).                    CI0109
            11            CT09-GERSD  PICTURE  9(8).                    CI0109
            11            CT09-GERED  PICTURE  9(8).                    CI0109
            10            CT09-A199.                                    CI0109
            11            CT09-FILLER PICTURE  X(20).                   CI0109
            10            CT09-A101                                     CI0109
                          REDEFINES            CT09-A199.               CI0109
            11            CT09-GECSQ  PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT09-CTAXR  PICTURE  X.                       CI0109
            11            CT09-GETAI  PICTURE  X.                       CI0109
            11            CT09-CTLACD PICTURE  9(8).                    CI0109
            11            CT09-GEPCS  PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CT09-A102                                     CI0109
                          REDEFINES            CT09-A199.               CI0109
            11            CT09-CLPID  PICTURE  9(9).                    CI0109
       01                 CT17.                                         CI0109
            10            CT17-GELL   PICTURE  9(4)                     CI0109
                          BINARY.                                       CI0109
            10            CT17-CT17K.                                   CI0109
            11            CT17-NSUBA  PICTURE  9(15).                   CI0109
            10            CT17-NCGRP                                    CI0109
                          REDEFINES            CT17-CT17K               CI0109
               PICTURE    99.                                           CI0109
            10            CT17-IBASE  PICTURE  X.                       CI0109
            10            CT17-C280.                                    CI0109
            11            CT17-FILLER PICTURE  X(178).                  CI0109
            10            CT17-C281                                     CI0109
                          REDEFINES            CT17-C280.               CI0109
            11            CT17-AMFYP  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AMFYP1 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-PRATE  PICTURE  9(3)V99.                 CI0109
            11            CT17-IREPL  PICTURE  X.                       CI0109
            11            CT17-PREPL  PICTURE  9(3).                    CI0109
            11            CT17-NSCHI  PICTURE  999.                     CI0109
            11            CT17-AFSTB  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ASNDB  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APRDC  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AAPCA1 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AAPCP  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CEABL  PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CEABL1 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CEABL2 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ACOMT1 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APRCDD PICTURE  S9(11)                   CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ICALC  PICTURE  X.                       CI0109
            11            CT17-IFRNT  PICTURE  X.                       CI0109
            11            CT17-ABMFB  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ABMFB1 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ABPMB  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ABPMB1 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-IANNP  PICTURE  X.                       CI0109
            11            CT17-ICNVR  PICTURE  X.                       CI0109
            11            CT17-GESTD  PICTURE  9(8).                    CI0109
            11            CT17-GEEND  PICTURE  9(8).                    CI0109
            11            CT17-AECMN  PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APPRM  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ARCUA  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-NSCHIG PICTURE  999.                     CI0109
            11            CT17-AECMP  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AECPB  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CCFEE  PICTURE  X.                       CI0109
            10            CT17-C282                                     CI0109
                          REDEFINES            CT17-C280.               CI0109
            11            CT17-NSCHI7 PICTURE  999.                     CI0109
            11            CT17-ACOMT3 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APRCDE PICTURE  S9(11)                   CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ICNVR1 PICTURE  X.                       CI0109
            11            CT17-GESTD1 PICTURE  9(8).                    CI0109
            11            CT17-GEEND3 PICTURE  9(8).                    CI0109
            11            CT17-NSCHIH PICTURE  999.                     CI0109
            11            CT17-AECMPA PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AECPBA PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CCFEEA PICTURE  X.                       CI0109
            10            CT17-C283                                     CI0109
                          REDEFINES            CT17-C280.               CI0109
            11            CT17-ICNVR2 PICTURE  X.                       CI0109
            11            CT17-GESTD2 PICTURE  9(8).                    CI0109
            11            CT17-GEEND2 PICTURE  9(8).                    CI0109
            11            CT17-AEXAC  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APFEEK PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AACBL  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CEABL3 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CEABL4 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CEABL5 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ICALC1 PICTURE  X.                       CI0109
            11            CT17-ISBIL  PICTURE  X.                       CI0109
            10            CT17-C284                                     CI0109
                          REDEFINES            CT17-C280.               CI0109
            11            CT17-ICNVR3 PICTURE  X.                       CI0109
            11            CT17-GESTD3 PICTURE  9(8).                    CI0109
            11            CT17-GEEND4 PICTURE  9(8).                    CI0109
            10            CT17-C285                                     CI0109
                          REDEFINES            CT17-C280.               CI0109
            11            CT17-ICNVR5 PICTURE  X.                       CI0109
            11            CT17-GESTD4 PICTURE  9(8).                    CI0109
            11            CT17-GEEND6 PICTURE  9(8).                    CI0109
            11            CT17-IINSP  PICTURE  X.                       CI0109
            11            CT17-CDMOD  PICTURE  9(2).                    CI0109
            11            CT17-NSCHI9 PICTURE  999.                     CI0109
            11            CT17-PREPL5 PICTURE  S9(3)V9(3).              CI0109
            11            CT17-AGAPA  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AFABA  PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CEABL9 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CEABLA PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CEABLB PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AFABA2 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AFABA3 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ACOMT4 PICTURE  S9(15)V99                CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APRCDF PICTURE  S9(11)                   CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APFEP1 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APFEP3 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APFEP4 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APFEP5 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APFEP6 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APFEP7 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APFEP8 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APFEPA PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AGAPA1 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AFABA1 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AFABA4 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CPRPM4 PICTURE  9(3).                    CI0109
            11            CT17-APPRM2 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-DSBST  PICTURE  9(8).                    CI0109
            11            CT17-NCTYR  PICTURE  999.                     CI0109
            11            CT17-ISPCV  PICTURE  X.                       CI0109
            11            CT17-ISETC  PICTURE  X.                       CI0109
            11            CT17-IANNC  PICTURE  X.                       CI0109
            11            CT17-NSCHIE PICTURE  999.                     CI0109
            11            CT17-NMGAT  PICTURE  9(2).                    CI0109
            10            CT17-C286                                     CI0109
                          REDEFINES            CT17-C280.               CI0109
            11            CT17-ABEAC1 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AEXAC2 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AACBL1 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CEABL6 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CEABL7 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CEABL8 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AUEPC  PICTURE  S9(11)                   CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CTLPM  PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ICNVR4 PICTURE  X.                       CI0109
            11            CT17-ICALC2 PICTURE  X.                       CI0109
            11            CT17-QSYPAA PICTURE  9(2).                    CI0109
            11            CT17-GESTD5 PICTURE  9(8).                    CI0109
            11            CT17-GEEND5 PICTURE  9(8).                    CI0109
            10            CT17-C287                                     CI0109
                          REDEFINES            CT17-C280.               CI0109
            11            CT17-CDMOD2 PICTURE  9(2).                    CI0109
            11            CT17-NSCHIA PICTURE  999.                     CI0109
            11            CT17-GESTD6 PICTURE  9(8).                    CI0109
            11            CT17-GEEND7 PICTURE  9(8).                    CI0109
            11            CT17-DCMST  PICTURE  9(8).                    CI0109
            11            CT17-PPDST  PICTURE  S9(4)V999                CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-PRATE1 PICTURE  S9(4)V999                CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CRATE  PICTURE  X.                       CI0109
            11            CT17-PREPL6 PICTURE  S9(4)V9(3)               CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AGAPA4 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AGAPA3 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APFEPD PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-APFEPC PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ASTTL2 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ASTTL3 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ASTTL4 PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CPRPM5 PICTURE  9(3).                    CI0109
            11            CT17-APPRM3 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ACOMT5 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-IANNC1 PICTURE  X.                       CI0109
            11            CT17-NCTYR1 PICTURE  999.                     CI0109
            11            CT17-AAPCP2 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-AAPCP1 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CEABLE PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CEABLC PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-CEABLD PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-IFRNT1 PICTURE  X.                       CI0109
            11            CT17-APRCD8 PICTURE  S9(11)                   CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            CT17-ICNVR6 PICTURE  X.                       CI0109
            11            CT17-IREPL5 PICTURE  X.                       CI0109
            11            CT17-CATRF  PICTURE  9(3).                    CI0109
            11            CT17-IRENT  PICTURE  X.                       CI0109
            11            CT17-IPCIP  PICTURE  X.                       CI0109
            11            CT17-NSCHIF PICTURE  999.                     CI0109
       01                 CT18.                                         CI0109
            10            CT18-CT18K.                                   CI0109
            11            CT18-GESQ2  PICTURE  99.                      CI0109
            11            CT18-CMACR  PICTURE  9(3).                    CI0109
            11            CT18-GERSD  PICTURE  9(8).                    CI0109
            11            CT18-C199.                                    CI0109
            12            CT18-CLID.                                    CI0109
            13            CT18-CLIDO  PICTURE  9(3).                    CI0109
            13            CT18-CLIDN.                                   CI0109
            14            CT18-CLIDNP PICTURE  X(12).                   CI0109
            14            CT18-CLIDND PICTURE  9(8).                    CI0109
            11            CT18-CPATP  PICTURE  X(1).                    CI0109
            10            CT18-GERED  PICTURE  9(8).                    CI0109
            10            CT18-PSMAP  PICTURE  S9(3)V9(4)               CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CT18-NSORG  PICTURE  9(3).                    CI0109
            10            CT18-CUNPD  PICTURE  X(2).                    CI0109
            10            CT18-NCPSP  PICTURE  S9(03)                   CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CT18-PCPSP  PICTURE  S9(3)V9(4)               CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CT18-CRSET  PICTURE  9(3).                    CI0109
            10            CT18-CRELT  PICTURE  9(3).                    CI0109
            10            CT18-FILLER PICTURE  X(10).                   CI0109
       01               7-DA01.                                         $9DAT2
         05             7-DA01-9DAT8              PIC 9(08).            $9DAT2
         05             7-DA01-IDAT8              PIC 9(08).            $9DAT2
       01  7-CTSR.                                                      $CTSR2
      * PARAMETERS.                                                     $CTSR2
      *  THE ACCOUNT ID NUMBER                                          $CTSR2
      *!WI pl=DA230                                                     $CTSR2
           05  7-CTSR-CTID                                              $CTSR2
                        PICTURE X(27).                                  CI0109
      *  THE DATE TO FIND PLANNER ON ACCOUNT. ZERO IS CURRENT.          $CTSR2
      *!WI pl=DA245                                                     $CTSR2
           05  7-CTSR-DEFFT                                             $CTSR2
                        PICTURE 9(8).                                   CI0109
      *  THE CURRENT SYSTEM DATE FROM WHATEVER SOURCE YOUR PROGRAM USES.$CTSR2
      *!WI pl=DA255                                                     $CTSR2
           05  7-CTSR-DCACG                                             $CTSR2
                        PICTURE 9(8).                                   CI0109
      *                                                                 $CTSR2
      * RETURN FIELDS.                                                  $CTSR2
      *  THE PLANNER ON THE ACCOUNT.                                    $CTSR2
           05  7-CTSR-CLID.                                             $CTSR2
      *!WI pl=DA292                                                     $CTSR2
               10  7-CTSR-CLIDO                                         $CTSR2
                        PICTURE 9(3).                                   CI0109
               10  7-CTSR-CLIDN.                                        $CTSR2
      *!WI pl=DA296                                                     $CTSR2
                   15  7-CTSR-CLIDNP                                    $CTSR2
                        PICTURE X(12).                                  CI0109
      *!WI pl=DA298                                                     $CTSR2
                   15  7-CTSR-CLIDND                                    $CTSR2
                        PICTURE 9(8).                                   CI0109
      *  RETURN CODE. 'N' INDICATES RETRIEVAL SUCCESSFUL                $CTSR2
      *               'Y' INDICATES NO ACTIVE SERVICE PLANNER ON ACCOUNT$CTSR2
      *!WI pl=DA330                                                     $CTSR2
           05  7-CTSR-IIDSW                                             $CTSR2
                        PICTURE X.                                      CI0109
      *                                                                 $CTSR2
      *  BOOLEAN SSA FOR THE QUALIFIED CALL OF CT17 TO FIND THE BASE    $CTSR2
      *      PLAN FOR THIS ACCOUNT                                      $CTSR2
      *                                                                 $CTSR2
           05  7-CTSR-SSA.                                              $CTSR2
               10  FILLER          PIC X(08)   VALUE 'CT17    '.        $CTSR2
               10  FILLER          PIC X(01)   VALUE '*'.               $CTSR2
               10  7-CTSR-CCOD     PIC X(05)   VALUE '-----'.           $CTSR2
               10  FILLER          PIC X(01)   VALUE '('.               $CTSR2
               10  FILLER          PIC X(08)   VALUE 'IBASE   '.        $CTSR2
               10  FILLER          PIC X(02)   VALUE ' ='.              $CTSR2
      *!WI pl=DA472                                                     $CTSR2
               10  7-CTSR-IBASE                                         $CTSR2
                        PICTURE X.                                      CI0109
               10  FILLER          PIC X(01)   VALUE ')'.               $CTSR2
      *                                                                 $CTSR2
      * THIS DATE CONTAINS THE EFFECTIVE DATE IN 999'S COMPLEMENT.      $CTSR2
       01  7-CTSR-9DEFFT           PIC 9(08).                           $CTSR2
      ******************************************************            AADA81
      ****      WORK AREAS FOR COMMON DATE UTILITY       ***            AADA81
      ******************************************************            AADA81
      **                                                                AADA81
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA81
      **                                                                AADA81
      **   SEGMENT DD30 - FUNCTION LAYOUT                               AADA81
      **                                                                AADA81
      *!WF DSP=DD DSL=DD SEL=30 FOR=I DES=2 LEV=1                       AADA81
       01                 DD30.                                         CI0109
            10            DD30-CDTFN  PICTURE  9(4)                     CI0109
                          VALUE                ZERO.                    CI0109
            10            DD30-CDTSF  PICTURE  9(4)                     CI0109
                          VALUE                ZERO.                    CI0109
            10            DD30-CDTSC  PICTURE  9(4)                     CI0109
                          VALUE                ZERO.                    CI0109
            10            DD30-FILLER PICTURE  X(40)                    CI0109
                          VALUE                SPACE.                   CI0109
       01                 DD33.                                         CI0109
            10            DD33-DTGRG.                                   CI0109
            11            DD33-DTGCY.                                   CI0109
            12            DD33-DTGCC  PICTURE  9(2)                     CI0109
                          VALUE                ZERO.                    CI0109
            12            DD33-DTGYY  PICTURE  9(2)                     CI0109
                          VALUE                ZERO.                    CI0109
            11            DD33-DTGMM  PICTURE  9(2)                     CI0109
                          VALUE                ZERO.                    CI0109
            11            DD33-DTGDD  PICTURE  9(2)                     CI0109
                          VALUE                ZERO.                    CI0109
            10            DD33-DTJULC.                                  CI0109
            11            DD33-DTJCY.                                   CI0109
            12            DD33-DTJCC  PICTURE  9(2)                     CI0109
                          VALUE                ZERO.                    CI0109
            12            DD33-DTJYY  PICTURE  9(2)                     CI0109
                          VALUE                ZERO.                    CI0109
            11            DD33-DTJDDC PICTURE  S9(3)                    CI0109
                          VALUE                ZERO.                    CI0109
            11            DD33-DTJDD                                    CI0109
                          REDEFINES            DD33-DTJDDC              CI0109
               PICTURE    9(3).                                         CI0109
            10            DD33-DTJUL                                    CI0109
                          REDEFINES            DD33-DTJULC              CI0109
               PICTURE    9(7).                                         CI0109
            10            DD33-DTDYR  PICTURE  9(3)                     CI0109
                          VALUE                ZERO.                    CI0109
            10            DD33-DTDMO  PICTURE  9(2)                     CI0109
                          VALUE                ZERO.                    CI0109
            10            DD33-FILLER PICTURE  X(18)                    CI0109
                          VALUE                SPACE.                   CI0109
      **                                                                AADA81
      **   SEGMENT DD33 - CONVERT DATE LAYOUT                           AADA81
      **                                                                AADA81
      *!WF DSP=DD DSL=DD SEL=33 FOR=I DES=2 LEV=1                       AADA81
      **                                                                AADA81
      *      WORK AREA FOR PMS AAOAG3                                   AAOAG3
       01  7-OAGE-PASSED-FIELDS.                                        AAOAG3
           05  7-OAGE-BIRTH-DATE      PIC 9(7).                         AAOAG3
           05  FILLER REDEFINES                                         AAOAG3
               7-OAGE-BIRTH-DATE.                                       AAOAG3
               10  7-OAGE-BD-CCYY     PIC 9(4).                         AAOAG3
               10  7-OAGE-BD-CCYY-RED REDEFINES                         AAOAG3
                   7-OAGE-BD-CCYY.                                      AAOAG3
                   15  7-OAGE-BD-CC   PIC 99.                           AAOAG3
                   15  7-OAGE-BD-YY   PIC 99.                           AAOAG3
               10  7-OAGE-BD-DDD      PIC 999.                          AAOAG3
           05  7-OAGE-CURRENT-DATE    PIC 9(7).                         AAOAG3
           05  FILLER REDEFINES                                         AAOAG3
               7-OAGE-CURRENT-DATE.                                     AAOAG3
               10  7-OAGE-CD-CCYY     PIC 9(4).                         AAOAG3
               10  7-OAGE-CD-CCYY-RED REDEFINES                         AAOAG3
                   7-OAGE-CD-CCYY.                                      AAOAG3
                   15  7-OAGE-CD-CC   PIC 99.                           AAOAG3
                   15  7-OAGE-CD-YY   PIC 99.                           AAOAG3
               10  7-OAGE-CD-DDD      PIC 999.                          AAOAG3
       01  7-OAGE-WORK-AREA.                                            AAOAG3
           05  7-OAGE-CLIENT-AGE      PIC 999V9.                        AAOAG3
           05  7-OAGE-AGE-DAYS        PIC 999.                          AAOAG3
           05  7-OAGE-AGE-YRS         PIC 999.                          AAOAG3
       01  7-OAGE-LITERALS.                                             AAOAG3
           05  7-OAGE-DAYS-IN-HALF-YR PIC 999  VALUE 182.               AAOAG3
           05  7-OAGE-DAYS-IN-A-YR    PIC 999  VALUE 365.               AAOAG3
           05  7-OAGE-HALF-PERCENT    PIC V9   VALUE .5.                AAOAG3
           05  7-OAGE-19TH-CENTURY    PIC 99   VALUE 19.                AAOAG3
           05  7-OAGE-20TH-CENTURY    PIC 99   VALUE 20.                AAOAG3
      *      END OF WORK AREA FOR PMS AAOAG3                            AAOAG3
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0109
            10            XW05-XW06.                                    CI0109
            11            XW05-XDBPCB.                                  CI0109
            12            XW05-XDBDNM PICTURE  X(08)                    CI0109
                          VALUE                SPACE.                   CI0109
            12            XW05-XSEGLV PICTURE  X(02)                    CI0109
                          VALUE                SPACE.                   CI0109
            12            XW05-XRC    PICTURE  X(02)                    CI0109
                          VALUE                SPACE.                   CI0109
            12            XW05-XPROPT PICTURE  X(04)                    CI0109
                          VALUE                SPACE.                   CI0109
            12            XW05-FILLER PICTURE  S9(5)                    CI0109
                          VALUE                ZERO                     CI0109
                          BINARY.                                       CI0109
            12            XW05-XSEGNM PICTURE  X(08)                    CI0109
                          VALUE                SPACE.                   CI0109
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0109
                          VALUE                ZERO                     CI0109
                          BINARY.                                       CI0109
            12            XW05-XSEGNB PICTURE  9(05)                    CI0109
                          VALUE                ZERO                     CI0109
                          BINARY.                                       CI0109
            12            XW05-XCOKEY PICTURE  X(70)                    CI0109
                          VALUE                SPACE.                   CI0109
            10            XW05-XW07.                                    CI0109
            11            XW05-XIOPCB.                                  CI0109
            12            XW05-XTERMI PICTURE  X(08)                    CI0109
                          VALUE                SPACE.                   CI0109
            12            XW05-FILLER PICTURE  XX                       CI0109
                          VALUE                SPACE.                   CI0109
            12            XW05-XRC1   PICTURE  X(02)                    CI0109
                          VALUE                SPACE.                   CI0109
            12            XW05-FILLER PICTURE  X(12)                    CI0109
                          VALUE                SPACE.                   CI0109
            12            XW05-XMODNM PICTURE  X(8)                     CI0109
                          VALUE                SPACE.                   CI0109
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0109
                          VALUE                ZERO.                    CI0109
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0109
                          VALUE                ZERO.                    CI0109
            10            XW05-XGU    PICTURE  X(4)                     CI0109
                          VALUE                'GU  '.                  CI0109
            10            XW05-XGHU   PICTURE  X(4)                     CI0109
                          VALUE                'GHU '.                  CI0109
            10            XW05-XGN    PICTURE  X(4)                     CI0109
                          VALUE                'GN  '.                  CI0109
            10            XW05-XGHN   PICTURE  X(4)                     CI0109
                          VALUE                'GHN '.                  CI0109
            10            XW05-XGNP   PICTURE  X(4)                     CI0109
                          VALUE                'GNP '.                  CI0109
            10            XW05-XGHNP  PICTURE  X(4)                     CI0109
                          VALUE                'GHNP'.                  CI0109
            10            XW05-XREPL  PICTURE  XXXX                     CI0109
                          VALUE                'REPL'.                  CI0109
            10            XW05-XISRT  PICTURE  X(4)                     CI0109
                          VALUE                'ISRT'.                  CI0109
            10            XW05-XDLET  PICTURE  X(4)                     CI0109
                          VALUE                'DLET'.                  CI0109
            10            XW05-XOPEN  PICTURE  X(4)                     CI0109
                          VALUE                'OPEN'.                  CI0109
            10            XW05-XCLSE  PICTURE  X(4)                     CI0109
                          VALUE                'CLSE'.                  CI0109
            10            XW05-XCHKP  PICTURE  X(4)                     CI0109
                          VALUE                'CHKP'.                  CI0109
            10            XW05-XXRST  PICTURE  X(4)                     CI0109
                          VALUE                'XRST'.                  CI0109
            10            XW05-XTERM  PICTURE  X(4)                     CI0109
                          VALUE                'TERM'.                  CI0109
            10            XW05-XNFPAC PICTURE  X(13)                    CI0109
                          VALUE                SPACE.                   CI0109
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0109
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0109
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
                                                                        AM0006
      ******************************************************************AM0006
      **     PCB ADDRESS LIST FOR CI0006.  MODULE CI0003 WILL NEED     *AM0006
      **     PCB'S FOR:                                                *AM0006
      **                CLIENT DATABASE(CL1P)                          *AM0006
      ******************************************************************AM0006
                                                                        AM0006
       01  CI0006A-PCB-ADDRESS-LIST.                                    AM0006
           05  CI0006A-PCB-CL1P-PTR1      POINTER.                      AM0006

      *SSA TO GET CT18 WITH 0 END DATE
       01               7-CTA18-1-SSA.                                  AAADBL
         05             FILLER          PIC X(08)   VALUE 'CT18'.       AAADBL
         05             FILLER          PIC X(01)   VALUE '*'.          AAADBL
         05             7-CTA18-1-CCOD PIC X(05)  VALUE '-----'.        AAADBL
         05             FILLER          PIC X(09) VALUE '(GERED   '.
         05             FILLER          PIC X(02) VALUE ' ='.
         05             FILLER          PIC 9(08) VALUE ZERO.
         05             FILLER          PIC X(01)   VALUE ')'.          AAADBL

      ***WORKING STRG FOR AATOTI*************
      *!WI
           05  WS00-AATOTI   VALUE ZEROES
                        PICTURE S9(11)V99                               CI0109
                          COMPUTATIONAL-3.                              CI0109

       01  WS10-WORK-AREAS.

      *    COUNT OF REQUESTED DISBURSEMENT METHODS
      *    ('AMOUNTS' FROM ADD/CHG SCREEN: 'DOLLAR AMT', 'PERCENTAGE
      *    AMT', 'SHARE AMT')
           05  WS10-1CTAMT      PIC 99       VALUE 0.

      *    NEW MONEY DOLLAR AMOUNT ON AN ACCOUNT
      *!WI
           05  WS10-ACNMO
                        PICTURE S9(9)V99                                CI0109
                          COMPUTATIONAL-3                               CI0109
                                             VALUE 0.
      *    NEW MONEY SHARES ON AN ACCOUNT
      *!WI
           05  WS10-QSHNM
                        PICTURE S9(10)V999                              CI0109
                                             VALUE 0.
      *    CURRENT NET ASSETS
      *!WI
           05  WS10-ACNTA
                        PICTURE S9(11)V99                               CI0109
                          COMPUTATIONAL-3                               CI0109
                                             VALUE 0.
      *    HOLDING AREA FOR CLID OF 1ST PLANNER ON ACCT
      *!WI
           05  WS10-CLID8
                        PICTURE X(23)                                   CI0109
                                             VALUE SPACES.

      *THE CF FIELDS SIGNIFY A SEGMENT FOUND IN MACRO AOAADR, $CTSR2
       01  W-SEGM-1CF.
           05  CT07-CF      PIC X(01)        VALUE '0'.
           05  CT09-CF      PIC X(01)        VALUE '0'.
           05  CL24-CF      PIC X(01)        VALUE '0'.
           05  CT17-CF      PIC X(01)        VALUE '0'.
           05  CT18-CF      PIC X(01)        VALUE '0'.
      *DETERMINE SOURCE IRA TYPE AND STATUS FOR RMD
       01 WS01-IRA-TYPE  PIC X(6) VALUE SPACES.
          88 INVALID-IRA VALUES
                         '001003'
                         '003003'
                         '004003'
                         '005001'
                         '005003'
                         '006001'
                         '006003'
                         '007000'.
       01 FILLER REDEFINES WS01-IRA-TYPE.
      *!WI
          05 WS01-CIRAT
                        PICTURE 999.                                    CI0109
      *!WI
          05 WS01-CIRAS
                        PICTURE 999.                                    CI0109

       01   DEBUT-WSS.                                                  CI0109
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0109
            05   IK     PICTURE X.                                      CI0109
       01  CONSTANTES-PAC.                                              CI0109
           05  FILLER  PICTURE X(87)   VALUE                            CI0109
                     '6015 CAT09/08/14CI0109ADMIN   14:34:51CI0109P AMERCI0109
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0109
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0109
           05  NUGNA   PICTURE X(5).                                    CI0109
           05  APPLI   PICTURE X(3).                                    CI0109
           05  DATGN   PICTURE X(8).                                    CI0109
           05  PROGR   PICTURE X(6).                                    CI0109
           05  CODUTI  PICTURE X(8).                                    CI0109
           05  TIMGN   PICTURE X(8).                                    CI0109
           05  PROGE   PICTURE X(8).                                    CI0109
           05  COBASE  PICTURE X(4).                                    CI0109
           05  DATGNC  PICTURE X(10).                                   CI0109
           05  RELEAS  PICTURE X(7).                                    CI0109
           05  DATGE   PICTURE X(10).                                   CI0109
           05  DATSQ   PICTURE X(10).                                   CI0109
       01  DATCE.                                                       CI0109
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0109
         05  DATOR.                                                     CI0109
           10  DATOA  PICTURE XX.                                       CI0109
           10  DATOM  PICTURE XX.                                       CI0109
           10  DATOJ  PICTURE XX.                                       CI0109
       01   VARIABLES-CONDITIONNELLES.                                  CI0109
            05                  FT      PICTURE X VALUE '0'.            CI0109
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0109
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0109
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0109
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0109
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0109
       01               S-CL01-SSA.                                     CI0109
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0109
                                      VALUE 'CL01    '.                 CI0109
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0109
            10          S-CL01-CCOD   PICTURE X(5)                      CI0109
                                      VALUE '-----'.                    CI0109
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0109
       01            S-CLU01-SSA.                                       CI0109
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CL01    '.                 CI0109
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0109
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(CL01K'.                   CI0109
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0109
            10       S-CLU01-CL01K.                                     CI0109
            11       S-CLU01-C199.                                      CI0109
            12       S-CLU01-CLID.                                      CI0109
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0109
            13       S-CLU01-CLIDN.                                     CI0109
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0109
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0109
            10  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01               S-CL24-SSA.                                     CI0109
            10         S1-CL24-SEGNAM PICTURE X(8)                      CI0109
                                      VALUE 'CL24    '.                 CI0109
            10         S1-CL24-CCOM   PICTURE X VALUE '*'.              CI0109
            10          S-CL24-CCOD   PICTURE X(5)                      CI0109
                                      VALUE '-----'.                    CI0109
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0109
       01            S-CLA24-SSA.                                       CI0109
            10      S1-CLA24-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CL24    '.                 CI0109
            10      S1-CLA24-CCOM   PICTURE X VALUE '*'.                CI0109
            10       S-CLA24-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            10      S1-CLA24-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(GECED'.                   CI0109
            10       S-CLA24-OPER  PICTURE XX VALUE ' ='.               CI0109
            10       S-CLA24-GECED    PICTURE  9(8).                    CI0109
            10  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01            S-CLB24-SSA.                                       CI0109
            10      S1-CLB24-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CL24    '.                 CI0109
            10      S1-CLB24-CCOM   PICTURE X VALUE '*'.                CI0109
            10       S-CLB24-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            10      S1-CLB24-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(IRESA'.                   CI0109
            10       S-CLB24-OPER  PICTURE XX VALUE ' ='.               CI0109
            10       S-CLB24-IRESA    PICTURE  X.                       CI0109
            10  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01            S-CLC24-SSA.                                       CI0109
            10      S1-CLC24-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CL24    '.                 CI0109
            10      S1-CLC24-CCOM   PICTURE X VALUE '*'.                CI0109
            10       S-CLC24-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            10      S1-CLC24-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(GEADS'.                   CI0109
            10       S-CLC24-OPER  PICTURE XX VALUE ' ='.               CI0109
            10       S-CLC24-GEADS    PICTURE  9.                       CI0109
            10  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01            S-CLD24-SSA.                                       CI0109
            10      S1-CLD24-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CL24    '.                 CI0109
            10      S1-CLD24-CCOM   PICTURE X VALUE '*'.                CI0109
            10       S-CLD24-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            10      S1-CLD24-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(GEST'.                    CI0109
            10       S-CLD24-OPER  PICTURE XX VALUE ' ='.               CI0109
            10       S-CLD24-GEST     PICTURE  X(8).                    CI0109
            10  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01            S-CLU24-SSA.                                       CI0109
            10      S1-CLU24-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CL24    '.                 CI0109
            10      S1-CLU24-CCOM   PICTURE X VALUE '*'.                CI0109
            10       S-CLU24-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            10      S1-CLU24-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(CL24K'.                   CI0109
            10       S-CLU24-OPER  PICTURE XX VALUE ' ='.               CI0109
            10       S-CLU24-CL24K.                                     CI0109
            11       S-CLU24-GECSQ    PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            10  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01               S-CT01-SSA.                                     CI0109
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0109
                                      VALUE 'CT01    '.                 CI0109
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0109
            10          S-CT01-CCOD   PICTURE X(5)                      CI0109
                                      VALUE '-----'.                    CI0109
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0109
       01            S-CTU01-SSA.                                       CI0109
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CT01    '.                 CI0109
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0109
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(CT01K'.                   CI0109
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0109
            10       S-CTU01-CT01K.                                     CI0109
            11       S-CTU01-C299.                                      CI0109
            12       S-CTU01-CTID.                                      CI0109
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0109
            13       S-CTU01-CTIDN.                                     CI0109
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0109
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0109
            10  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01               S-CT07-SSA.                                     CI0109
            10         S1-CT07-SEGNAM PICTURE X(8)                      CI0109
                                      VALUE 'CT07    '.                 CI0109
            10         S1-CT07-CCOM   PICTURE X VALUE '*'.              CI0109
            10          S-CT07-CCOD   PICTURE X(5)                      CI0109
                                      VALUE '-----'.                    CI0109
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0109
       01            S-CTU07-SSA.                                       CI0109
            10      S1-CTU07-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CT07    '.                 CI0109
            10      S1-CTU07-CCOM   PICTURE X VALUE '*'.                CI0109
            10       S-CTU07-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            10      S1-CTU07-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(CT07K'.                   CI0109
            10       S-CTU07-OPER  PICTURE XX VALUE ' ='.               CI0109
            10       S-CTU07-CT07K.                                     CI0109
            11       S-CTU07-C199.                                      CI0109
            12       S-CTU07-CLID.                                      CI0109
            13       S-CTU07-CLIDO    PICTURE  9(3).                    CI0109
            13       S-CTU07-CLIDN.                                     CI0109
            14       S-CTU07-CLIDNP   PICTURE  X(12).                   CI0109
            14       S-CTU07-CLIDND   PICTURE  9(8).                    CI0109
            10  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01               S-CT09-SSA.                                     CI0109
            10         S1-CT09-SEGNAM PICTURE X(8)                      CI0109
                                      VALUE 'CT09    '.                 CI0109
            10         S1-CT09-CCOM   PICTURE X VALUE '*'.              CI0109
            10          S-CT09-CCOD   PICTURE X(5)                      CI0109
                                      VALUE '-----'.                    CI0109
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0109
       01            S-CTA09-SSA.                                       CI0109
            11      S1-CTA09-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CT09    '.                 CI0109
            11      S1-CTA09-CCOM   PICTURE X VALUE '*'.                CI0109
            11       S-CTA09-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            11      S1-CTA09-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(GERED'.                   CI0109
            11       S-CTA09-OPER  PICTURE XX VALUE ' ='.               CI0109
            11       S-CTA09-GERED    PICTURE  9(8).                    CI0109
            11  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01            S-CTB09-SSA.                                       CI0109
            11      S1-CTB09-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CT09    '.                 CI0109
            11      S1-CTB09-CCOM   PICTURE X VALUE '*'.                CI0109
            11       S-CTB09-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            11      S1-CTB09-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(GECSQ'.                   CI0109
            11       S-CTB09-OPER  PICTURE XX VALUE ' ='.               CI0109
            11       S-CTB09-GECSQ    PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            11  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01            S-CTU09-SSA.                                       CI0109
            11      S1-CTU09-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CT09    '.                 CI0109
            11      S1-CTU09-CCOM   PICTURE X VALUE '*'.                CI0109
            11       S-CTU09-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            11      S1-CTU09-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(CT09K'.                   CI0109
            11       S-CTU09-OPER  PICTURE XX VALUE ' ='.               CI0109
            11       S-CTU09-CT09K.                                     CI0109
            12       S-CTU09-CLCTRC   PICTURE  9(3).                    CI0109
            11  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01               S-CT17-SSA.                                     CI0109
            10         S1-CT17-SEGNAM PICTURE X(8)                      CI0109
                                      VALUE 'CT17    '.                 CI0109
            10         S1-CT17-CCOM   PICTURE X VALUE '*'.              CI0109
            10          S-CT17-CCOD   PICTURE X(5)                      CI0109
                                      VALUE '-----'.                    CI0109
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0109
       01            S-CTA17-SSA.                                       CI0109
            10      S1-CTA17-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CT17    '.                 CI0109
            10      S1-CTA17-CCOM   PICTURE X VALUE '*'.                CI0109
            10       S-CTA17-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            10      S1-CTA17-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(IBASE'.                   CI0109
            10       S-CTA17-OPER  PICTURE XX VALUE ' ='.               CI0109
            10       S-CTA17-IBASE    PICTURE  X.                       CI0109
            10  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01            S-CTU17-SSA.                                       CI0109
            10      S1-CTU17-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CT17    '.                 CI0109
            10      S1-CTU17-CCOM   PICTURE X VALUE '*'.                CI0109
            10       S-CTU17-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            10      S1-CTU17-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(CT17K'.                   CI0109
            10       S-CTU17-OPER  PICTURE XX VALUE ' ='.               CI0109
            10       S-CTU17-CT17K.                                     CI0109
            11       S-CTU17-NSUBA    PICTURE  9(15).                   CI0109
            10  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01               S-CT18-SSA.                                     CI0109
            10         S1-CT18-SEGNAM PICTURE X(8)                      CI0109
                                      VALUE 'CT18    '.                 CI0109
            10         S1-CT18-CCOM   PICTURE X VALUE '*'.              CI0109
            10          S-CT18-CCOD   PICTURE X(5)                      CI0109
                                      VALUE '-----'.                    CI0109
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0109
       01            S-CTA18-SSA.                                       CI0109
            11      S1-CTA18-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CT18    '.                 CI0109
            11      S1-CTA18-CCOM   PICTURE X VALUE '*'.                CI0109
            11       S-CTA18-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            11      S1-CTA18-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(GESQ2'.                   CI0109
            11       S-CTA18-OPER  PICTURE XX VALUE ' ='.               CI0109
            11       S-CTA18-GESQ2    PICTURE  99.                      CI0109
            11  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01            S-CTB18-SSA.                                       CI0109
            11      S1-CTB18-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CT18    '.                 CI0109
            11      S1-CTB18-CCOM   PICTURE X VALUE '*'.                CI0109
            11       S-CTB18-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            11      S1-CTB18-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(CMACR'.                   CI0109
            11       S-CTB18-OPER  PICTURE XX VALUE ' ='.               CI0109
            11       S-CTB18-CMACR    PICTURE  9(3).                    CI0109
            11  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01            S-CTC18-SSA.                                       CI0109
            10      S1-CTC18-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CT18    '.                 CI0109
            10      S1-CTC18-CCOM   PICTURE X VALUE '*'.                CI0109
            10       S-CTC18-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            10      S1-CTC18-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(GERED'.                   CI0109
            10       S-CTC18-OPER  PICTURE XX VALUE ' ='.               CI0109
            10       S-CTC18-GERED    PICTURE  9(8).                    CI0109
            10  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01            S-CTD18-SSA.                                       CI0109
            11      S1-CTD18-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CT18    '.                 CI0109
            11      S1-CTD18-CCOM   PICTURE X VALUE '*'.                CI0109
            11       S-CTD18-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            11      S1-CTD18-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(CPATP'.                   CI0109
            11       S-CTD18-OPER  PICTURE XX VALUE ' ='.               CI0109
            11       S-CTD18-CPATP    PICTURE  X(1).                    CI0109
            11  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01            S-CTU18-SSA.                                       CI0109
            10      S1-CTU18-SEGNAM PICTURE X(8)                        CI0109
                                      VALUE 'CT18    '.                 CI0109
            10      S1-CTU18-CCOM   PICTURE X VALUE '*'.                CI0109
            10       S-CTU18-CCOD   PICTURE X(5)                        CI0109
                                      VALUE '-----'.                    CI0109
            10      S1-CTU18-FLDNAM PICTURE X(9)                        CI0109
                                      VALUE '(CT18K'.                   CI0109
            10       S-CTU18-OPER  PICTURE XX VALUE ' ='.               CI0109
            10       S-CTU18-CT18K.                                     CI0109
            11       S-CTU18-GESQ2    PICTURE  99.                      CI0109
            11       S-CTU18-CMACR    PICTURE  9(3).                    CI0109
            11       S-CTU18-GERSD    PICTURE  9(8).                    CI0109
            11       S-CTU18-C199.                                      CI0109
            12       S-CTU18-CLID.                                      CI0109
            13       S-CTU18-CLIDO    PICTURE  9(3).                    CI0109
            13       S-CTU18-CLIDN.                                     CI0109
            14       S-CTU18-CLIDNP   PICTURE  X(12).                   CI0109
            14       S-CTU18-CLIDND   PICTURE  9(8).                    CI0109
            11       S-CTU18-CPATP    PICTURE  X(1).                    CI0109
            10  FILLER   PICTURE X    VALUE ')'.                        CI0109
       01   ZONES-UTILISATEUR PICTURE X.                                CI0109
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
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=XE DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XE00.                                         CI0109
          05              XE00-SUITE.                                   CI0109
            15       FILLER         PICTURE  X(00106).                  CI0109
       01                 XE06  REDEFINES      XE00.                    CI0109
            10            XE06-XDBPCB.                                  CI0109
            11            XE06-XDBDNM PICTURE  X(08).                   CI0109
            11            XE06-XSEGLV PICTURE  X(02).                   CI0109
            11            XE06-XRC    PICTURE  X(02).                   CI0109
            11            XE06-XPROPT PICTURE  X(04).                   CI0109
            11            XE06-FILLER PICTURE  S9(5)                    CI0109
                          BINARY.                                       CI0109
            11            XE06-XSEGNM PICTURE  X(08).                   CI0109
            11            XE06-XKEYLN PICTURE  S9(05)                   CI0109
                          BINARY.                                       CI0109
            11            XE06-XSEGNB PICTURE  9(05)                    CI0109
                          BINARY.                                       CI0109
            11            XE06-XCOKEY PICTURE  X(70).                   CI0109
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=XF DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XF00.                                         CI0109
          05              XF00-SUITE.                                   CI0109
            15       FILLER         PICTURE  X(00106).                  CI0109
       01                 XF06  REDEFINES      XF00.                    CI0109
            10            XF06-XDBPCB.                                  CI0109
            11            XF06-XDBDNM PICTURE  X(08).                   CI0109
            11            XF06-XSEGLV PICTURE  X(02).                   CI0109
            11            XF06-XRC    PICTURE  X(02).                   CI0109
            11            XF06-XPROPT PICTURE  X(04).                   CI0109
            11            XF06-FILLER PICTURE  S9(5)                    CI0109
                          BINARY.                                       CI0109
            11            XF06-XSEGNM PICTURE  X(08).                   CI0109
            11            XF06-XKEYLN PICTURE  S9(05)                   CI0109
                          BINARY.                                       CI0109
            11            XF06-XSEGNB PICTURE  9(05)                    CI0109
                          BINARY.                                       CI0109
            11            XF06-XCOKEY PICTURE  X(70).                   CI0109

      *PASS AREA TO/FROM CI0109
      *!WF DSP=PE DSL=PJ SEL=58 FOR=I DES=1 LEV=1 PLT=10
       01                 PE58.                                         CI0109
            10            PE58-CPITC  PICTURE  99.                      CI0109
            10            PE58-CDELI  PICTURE  9(3).                    CI0109
            10            PE58-FILLER PICTURE  X(100).                  CI0109
            10            PE58-ITELR  PICTURE  X.                       CI0109
            10            PE58-AEDRQ  PICTURE  S9(11)V99                CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PE58-FILLER PICTURE  X(50).                   CI0109

      *PASS AREA FROM CI9026
      *!WF DSP=PJ DSL=PJ SEL=27 FOR=I DES=1 LEV=1 PLT=10
       01                 PJ27.                                         CI0109
            10            PJ27-IWTHH  PICTURE  X.                       CI0109
            10            PJ27-CTWTC  PICTURE  9(2).                    CI0109
            10            PJ27-AWITH  PICTURE  S9(11)V99                CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ27-DCACG  PICTURE  9(8).                    CI0109
            10            PJ27-DNACG  PICTURE  9(8).                    CI0109
            10            PJ27-AMNBR  PICTURE  S9(5)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ27-CFLOW  PICTURE  X.                       CI0109
            10            PJ27-QSHNM  PICTURE  S9(10)V999.              CI0109
            10            PJ27-FILLER PICTURE  X(86).                   CI0109
       01                 PJ46.                                         CI0109
            10            PJ46-MAPPN  PICTURE  X(10).                   CI0109
            10            PJ46-PROGR  PICTURE  X(06).                   CI0109
            10            PJ46-CACTA  PICTURE  X(1).                    CI0109
            10            PJ46-CHCR   PICTURE  99.                      CI0109
            10            PJ46-CTID   PICTURE  X(27).                   CI0109
            10            PJ46-CTID01 PICTURE  X(27).                   CI0109
            10            PJ46-DCACG9 PICTURE  9(8).                    CI0109
            10            PJ46-NAASQ  PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-NPISQ  PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-NEIBT  PICTURE  X(7).                    CI0109
            10            PJ46-CPAYF  PICTURE  X(2).                    CI0109
            10            PJ46-IPLIN  PICTURE  X.                       CI0109
            10            PJ46-CLID8  PICTURE  X(23).                   CI0109
            10            PJ46-ADBRQ  PICTURE  S9(11)V99                CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-PACT1  PICTURE  S999V999                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-QSHOWQ PICTURE  S9(9)V999                CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-CPORT  PICTURE  X.                       CI0109
            10            PJ46-CPMTCB PICTURE  X(3).                    CI0109
            10            PJ46-GENAL1 PICTURE  X(30).                   CI0109
            10            PJ46-GENAL2 PICTURE  X(30).                   CI0109
            10            PJ46-GESAD1 PICTURE  X(30).                   CI0109
            10            PJ46-GESAD2 PICTURE  X(30).                   CI0109
            10            PJ46-GESAD3 PICTURE  X(30).                   CI0109
            10            PJ46-CCDSCW PICTURE  9(2).                    CI0109
            10            PJ46-CIRAP  PICTURE  XX.                      CI0109
            10            PJ46-AFEET  PICTURE  S9(5)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-ATERF  PICTURE  S9(5)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-CTWHAT PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-PWHLD  PICTURE  S999V9(5)                CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-GETIM  PICTURE  S9(7)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-DEFFT  PICTURE  9(8).                    CI0109
            10            PJ46-CLID   PICTURE  X(23).                   CI0109
            10            PJ46-NPAIS  PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-IEXML  PICTURE  X.                       CI0109
            10            PJ46-AEXML  PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-TPAYB  PICTURE  X(30).                   CI0109
            10            PJ46-NAIRB  PICTURE  X(30).                   CI0109
            10            PJ46-CSPDT  PICTURE  X.                       CI0109
            10            PJ46-GECSQ  PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-IOWNG  PICTURE  X.                       CI0109
            10            PJ46-CLIDP  PICTURE  X(23).                   CI0109
            10            PJ46-MREQN  PICTURE  X(45).                   CI0109
            10            PJ46-TREQT  PICTURE  X(30).                   CI0109
            10            PJ46-CLID2  PICTURE  X(23).                   CI0109
            10            PJ46-CCONFA PICTURE  X(12).                   CI0109
            10            PJ46-DTIME  PICTURE  X(11).                   CI0109
            10            PJ46-IDRMD  PICTURE  X.                       CI0109
            10            PJ46-FILLER PICTURE  X(92).                   CI0109
            10            PJ46-CTID02 PICTURE  X(27).                   CI0109
            10            PJ46-DCAC92 PICTURE  9(8).                    CI0109
            10            PJ46-NAASQ2 PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-NPISQ2 PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-CTID03 PICTURE  X(27).                   CI0109
            10            PJ46-DCAC93 PICTURE  9(8).                    CI0109
            10            PJ46-NAASQ3 PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-NPISQ3 PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            PJ46-CLCUS  PICTURE  99.                      CI0109
            10            PJ46-FILLER PICTURE  X(76).                   CI0109


      *PASS AREA TO/FROM CI0108 (VIA CI9026)
      *!WF DSP=CI DSL=PJ SEL=47 FOR=I DES=1 LEV=1 PLT=20
       01                 CI47.                                         CI0109
            10            CI47-MAPPN  PICTURE  X(10).                   CI0109
            10            CI47-CTID   PICTURE  X(27)                    CI0109
                          OCCURS       002     TIMES.                   CI0109
            10            CI47-ICUST  PICTURE  X                        CI0109
                          OCCURS       002     TIMES.                   CI0109
            10            CI47-IACFPD PICTURE  X(1).                    CI0109
            10            CI47-AFEED  PICTURE  S9(5)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI47-IWAIV  PICTURE  X.                       CI0109
            10            CI47-AFEET  PICTURE  S9(5)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI47-ITERF  PICTURE  X.                       CI0109
            10            CI47-ATERF  PICTURE  S9(5)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI47-CCACT  PICTURE  99.                      CI0109
            10            CI47-CPLTYP PICTURE  X(14).                   CI0109
            10            CI47-CSPDT  PICTURE  X.                       CI0109
            10            CI47-CPAYF  PICTURE  X(2).                    CI0109
            10            CI47-FILLER PICTURE  X(47).                   CI0109
       01                 CI52.                                         CI0109
            10            CI52-AACTV  PICTURE  S9(11)V99                CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI52-ADDAC  PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI52-AGRPV  PICTURE  S9(11)V99                CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI52-QSHOW  PICTURE  S9(10)V999               CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI52-AFAVP  PICTURE  S9(4)V9(3)               CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI52-DASOF  PICTURE  9(8).                    CI0109
            10            CI52-QDHGF  PICTURE  9(2).                    CI0109
            10            CI52-QSHIS  PICTURE  S9(10)V999               CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI52-CPORT  PICTURE  X.                       CI0109
            10            CI52-AWDRTP PICTURE  S9(11)V99                CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI52-AWDRTC PICTURE  S9(11)V99                CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI52-APPAYA PICTURE  S9(11)V99                CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI52-APPAYN PICTURE  S9(11)V99                CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI52-IGOTYA PICTURE  X.                       CI0109
            10            CI52-QTYUD1 PICTURE  9(5).                    CI0109
            10            CI52-QTYUD2 PICTURE  9(5).                    CI0109
            10            CI52-AGOFD  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI52-AGOFD1 PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI52-ANGOF  PICTURE  S9(9)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI52-AATOTI PICTURE  S9(11)V99                CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI52-QSHOM  PICTURE  S9(10)V999               CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI52-FILLER PICTURE  X(43).                   CI0109
       01                 CI53.                                         CI0109
            10            CI53-FILLER PICTURE  X(40).                   CI0109
            10            CI53-DCACG  PICTURE  9(8).                    CI0109
            10            CI53-CHCR   PICTURE  99.                      CI0109
            10            CI53-MAPPN  PICTURE  X(10).                   CI0109
            10            CI53-CTID   PICTURE  X(27)                    CI0109
                          OCCURS       002     TIMES.                   CI0109
            10            CI53-CMESS  PICTURE  9.                       CI0109
            10            CI53-CIRAP  PICTURE  XX                       CI0109
                          OCCURS       010     TIMES.                   CI0109
       01                 CI57.                                         CI0109
            10            CI57-C299.                                    CI0109
            11            CI57-CTID.                                    CI0109
            12            CI57-CTIDA  PICTURE  9(3).                    CI0109
            12            CI57-CTIDN.                                   CI0109
            13            CI57-CTIDNP PICTURE  X(13).                   CI0109
            13            CI57-CTIDND PICTURE  9(11).                   CI0109
            10            CI57-PRCOD  PICTURE  9(5).                    CI0109
            10            CI57-PRSCD  PICTURE  X(9).                    CI0109
            10            CI57-CTCCI  PICTURE  X.                       CI0109
            10            CI57-CPAYF  PICTURE  X(2).                    CI0109
            10            CI57-CLTYP  PICTURE  X.                       CI0109
            10            CI57-CLDTH  PICTURE  X.                       CI0109
            10            CI57-CTID01 PICTURE  X(27).                   CI0109
            10            CI57-PRCOD1 PICTURE  9(5).                    CI0109
            10            CI57-CPRSC1 PICTURE  X(9).                    CI0109
            10            CI57-CTCCIA PICTURE  X.                       CI0109
            10            CI57-QCLAGE PICTURE  9(3)V9                   CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI57-IDRMD  PICTURE  X.                       CI0109
            10            CI57-FILLER PICTURE  X(95).                   CI0109
            10            CI57-ICDSC  PICTURE  X.                       CI0109
            10            CI57-ICDSU  PICTURE  X.                       CI0109
            10            CI57-CCDSCW PICTURE  9(2).                    CI0109
       01                 CI67.                                         CI0109
            10            CI67-CTID01 PICTURE  X(27).                   CI0109
            10            CI67-C299.                                    CI0109
            11            CI67-CTID.                                    CI0109
            12            CI67-CTIDA  PICTURE  9(3).                    CI0109
            12            CI67-CTIDN.                                   CI0109
            13            CI67-CTIDNP PICTURE  X(13).                   CI0109
            13            CI67-CTIDND PICTURE  9(11).                   CI0109
            10            CI67-IDELI  PICTURE  X.                       CI0109
            10            CI67-CDEL1  PICTURE  9(3).                    CI0109
            10            CI67-NDELS  PICTURE  S9(3)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-IUGMA  PICTURE  X.                       CI0109
            10            CI67-DCACG  PICTURE  9(8).                    CI0109
            10            CI67-AACTVD PICTURE  S9(11)V99.               CI0109
            10            CI67-AACTV  PICTURE  S9(11)V99                CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-AFAVP  PICTURE  S9(4)V9(3)               CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-INMRC  PICTURE  X(01).                   CI0109
            10            CI67-IARLNA PICTURE  X.                       CI0109
            10            CI67-CELBL  PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-AMINAL PICTURE  S9(7)V99.                CI0109
            10            CI67-AMAXAL PICTURE  S9(7)V99.                CI0109
            10            CI67-IARRGA PICTURE  X.                       CI0109
            10            CI67-AMIND  PICTURE  S9(7)V99.                CI0109
            10            CI67-AMAXAR PICTURE  S9(7)V99.                CI0109
            10            CI67-IARPSA PICTURE  X.                       CI0109
            10            CI67-CELBA  PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-AMAXA  PICTURE  S9(7)V99.                CI0109
            10            CI67-QSHOWQ PICTURE  S9(9)V999                CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-QSHOW  PICTURE  S9(10)V999               CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-PACT1  PICTURE  S999V999                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-PPOT1  PICTURE  S9(3)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-QMTH   PICTURE  9(3).                    CI0109
            10            CI67-QMTH1  PICTURE  9(3).                    CI0109
            10            CI67-ALDDUE PICTURE  9(08).                   CI0109
            10            CI67-ALPAGM PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-ALPAGQ PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-ALPAGS PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-ALPAGR PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-IFQQT  PICTURE  X.                       CI0109
            10            CI67-IFQSA  PICTURE  X.                       CI0109
            10            CI67-AINPU  PICTURE  S9(9)V99.                CI0109
            10            CI67-QFOSPD PICTURE  9(2).                    CI0109
            10            CI67-MAPPN  PICTURE  X(10).                   CI0109
            10            CI67-CHCR   PICTURE  99.                      CI0109
            10            CI67-IFDAB  PICTURE  X.                       CI0109
            10            CI67-IFDAG  PICTURE  X.                       CI0109
            10            CI67-IFDAP  PICTURE  X.                       CI0109
            10            CI67-IFDAS  PICTURE  X(01).                   CI0109
            10            CI67-ALPMOD PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-CEIT   PICTURE  9(3).                    CI0109
            10            CI67-QSHIS  PICTURE  S9(10)V999               CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-QSHES  PICTURE  S9(10)V999               CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            CI67-CIRAP  PICTURE  XX.                      CI0109
            10            CI67-CLDOB  PICTURE  9(8).                    CI0109
            10            CI67-IARCDA PICTURE  X.                       CI0109
            10            CI67-FILLER PICTURE  X(38).                   CI0109

      *PASS AREA FROM CI0103 (VIA CI9026)
      *!WF DSP=CI DSL=PJ SEL=52 FOR=I DES=1 LEV=1 PLT=20

      *PASS AREA TO/FROM CI0106 (VIA CI9026)
      *!WF DSP=CI DSL=PJ SEL=53 FOR=I DES=1 LEV=1 PLT=20

      *PASS AREA TO/FROM CI0107 (VIA CI9026)
      *!WF DSP=CI DSL=PJ SEL=57 FOR=I DES=1 LEV=1 PLT=20

      *PASS AREA TO/FROM CI0067 (VIA CI9026)
      *!WF DSP=CI DSL=DU SEL=67 FOR=I DES=1 LEV=1 PLT=20


      *SOURCE ('FROM') ACCOUNT CONTRACT DATABASE ROOT (VIA CI9026)
      *!WF DSP=FR DSL=CT SEL=01 FOR=I DES=1 LEV=1 PLT=30
       01                 FR01.                                         CI0109
            10            FR01-CT01K.                                   CI0109
            11            FR01-C299.                                    CI0109
            12            FR01-CTID.                                    CI0109
            13            FR01-CTIDA  PICTURE  9(3).                    CI0109
            13            FR01-CTIDN.                                   CI0109
            14            FR01-CTIDNP PICTURE  X(13).                   CI0109
            14            FR01-CTIDND PICTURE  9(11).                   CI0109
            10            FR01-GECKD  PICTURE  9.                       CI0109
            10            FR01-GEMDA  PICTURE  9(8).                    CI0109
            10            FR01-NSEQ4B PICTURE  9(8)                     CI0109
                          BINARY.                                       CI0109
            10            FR01-GECUC  PICTURE  99.                      CI0109
            10            FR01-CTAUL  PICTURE  9(3).                    CI0109
            10            FR01-DIRAC  PICTURE  9(4).                    CI0109
            10            FR01-CTCCI  PICTURE  X.                       CI0109
            10            FR01-CTCUS  PICTURE  999.                     CI0109
            10            FR01-CTEFD  PICTURE  9(8).                    CI0109
            10            FR01-CTIAD  PICTURE  9(8).                    CI0109
            10            FR01-CLCUS  PICTURE  99.                      CI0109
            10            FR01-CAMMB  PICTURE  X(3).                    CI0109
            10            FR01-CKPMM  PICTURE  X.                       CI0109
            10            FR01-CTLAD  PICTURE  9(8).                    CI0109
            10            FR01-IPERS  PICTURE  X.                       CI0109
            10            FR01-AUNCB  PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            FR01-CTLAT  PICTURE  9(8).                    CI0109
            10            FR01-CTLATC PICTURE  9(6).                    CI0109
            10            FR01-IMEGA  PICTURE  X.                       CI0109
            10            FR01-DIRAB  PICTURE  9(8).                    CI0109
            10            FR01-COLRQ  PICTURE  X.                       CI0109
            10            FR01-ZDA04  PICTURE  X(4).                    CI0109
            10            FR01-CTLPD  PICTURE  9(8).                    CI0109
            10            FR01-CIRASP PICTURE  9.                       CI0109
            10            FR01-CIRATP PICTURE  99.                      CI0109
            10            FR01-DRTHC  PICTURE  9(8).                    CI0109
            10            FR01-CPPTC  PICTURE  X.                       CI0109
            10            FR01-ZDA06  PICTURE  X(6).                    CI0109
            10            FR01-CTACD  PICTURE  9(8).                    CI0109
            10            FR01-CTNLI  PICTURE  X.                       CI0109
            10            FR01-CTRHO  PICTURE  9(8).                    CI0109
            10            FR01-CTSGD  PICTURE  9(8).                    CI0109
            10            FR01-CPATP  PICTURE  X(1).                    CI0109
            10            FR01-IRSTA  PICTURE  X.                       CI0109
            10            FR01-CTSTA  PICTURE  99.                      CI0109
            10            FR01-CTSSC  PICTURE  99.                      CI0109
            10            FR01-PRLIN  PICTURE  9(3).                    CI0109
            10            FR01-PRCOD  PICTURE  9(5).                    CI0109
            10            FR01-PRSCD  PICTURE  X(9).                    CI0109
            10            FR01-CTLNI  PICTURE  X.                       CI0109
            10            FR01-AYSIDA PICTURE  9(3).                    CI0109
            10            FR01-AYSID  PICTURE  9(5).                    CI0109
            10            FR01-CTBMC  PICTURE  99.                      CI0109
            10            FR01-CINAR  PICTURE  99.                      CI0109
            10            FR01-CPHTR  PICTURE  X.                       CI0109
            10            FR01-CDSTR  PICTURE  XX.                      CI0109
            10            FR01-CQACT  PICTURE  999.                     CI0109
            10            FR01-CIRAS  PICTURE  999.                     CI0109
            10            FR01-CIRAT  PICTURE  999.                     CI0109
            10            FR01-CLRAY  PICTURE  9(5).                    CI0109
            10            FR01-CATTP  PICTURE  X.                       CI0109

      *DESTINATION ('TO') ACCOUNT CONTRACT DATABASE ROOT (VIA CI9026)
      *!WF DSP=TO DSL=CT SEL=01 FOR=I DES=1 LEV=1 PLT=30
       01                 TO01.                                         CI0109
            10            TO01-CT01K.                                   CI0109
            11            TO01-C299.                                    CI0109
            12            TO01-CTID.                                    CI0109
            13            TO01-CTIDA  PICTURE  9(3).                    CI0109
            13            TO01-CTIDN.                                   CI0109
            14            TO01-CTIDNP PICTURE  X(13).                   CI0109
            14            TO01-CTIDND PICTURE  9(11).                   CI0109
            10            TO01-GECKD  PICTURE  9.                       CI0109
            10            TO01-GEMDA  PICTURE  9(8).                    CI0109
            10            TO01-NSEQ4B PICTURE  9(8)                     CI0109
                          BINARY.                                       CI0109
            10            TO01-GECUC  PICTURE  99.                      CI0109
            10            TO01-CTAUL  PICTURE  9(3).                    CI0109
            10            TO01-DIRAC  PICTURE  9(4).                    CI0109
            10            TO01-CTCCI  PICTURE  X.                       CI0109
            10            TO01-CTCUS  PICTURE  999.                     CI0109
            10            TO01-CTEFD  PICTURE  9(8).                    CI0109
            10            TO01-CTIAD  PICTURE  9(8).                    CI0109
            10            TO01-CLCUS  PICTURE  99.                      CI0109
            10            TO01-CAMMB  PICTURE  X(3).                    CI0109
            10            TO01-CKPMM  PICTURE  X.                       CI0109
            10            TO01-CTLAD  PICTURE  9(8).                    CI0109
            10            TO01-IPERS  PICTURE  X.                       CI0109
            10            TO01-AUNCB  PICTURE  S9(7)V99                 CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            TO01-CTLAT  PICTURE  9(8).                    CI0109
            10            TO01-CTLATC PICTURE  9(6).                    CI0109
            10            TO01-IMEGA  PICTURE  X.                       CI0109
            10            TO01-DIRAB  PICTURE  9(8).                    CI0109
            10            TO01-COLRQ  PICTURE  X.                       CI0109
            10            TO01-ZDA04  PICTURE  X(4).                    CI0109
            10            TO01-CTLPD  PICTURE  9(8).                    CI0109
            10            TO01-CIRASP PICTURE  9.                       CI0109
            10            TO01-CIRATP PICTURE  99.                      CI0109
            10            TO01-DRTHC  PICTURE  9(8).                    CI0109
            10            TO01-CPPTC  PICTURE  X.                       CI0109
            10            TO01-ZDA06  PICTURE  X(6).                    CI0109
            10            TO01-CTACD  PICTURE  9(8).                    CI0109
            10            TO01-CTNLI  PICTURE  X.                       CI0109
            10            TO01-CTRHO  PICTURE  9(8).                    CI0109
            10            TO01-CTSGD  PICTURE  9(8).                    CI0109
            10            TO01-CPATP  PICTURE  X(1).                    CI0109
            10            TO01-IRSTA  PICTURE  X.                       CI0109
            10            TO01-CTSTA  PICTURE  99.                      CI0109
            10            TO01-CTSSC  PICTURE  99.                      CI0109
            10            TO01-PRLIN  PICTURE  9(3).                    CI0109
            10            TO01-PRCOD  PICTURE  9(5).                    CI0109
            10            TO01-PRSCD  PICTURE  X(9).                    CI0109
            10            TO01-CTLNI  PICTURE  X.                       CI0109
            10            TO01-AYSIDA PICTURE  9(3).                    CI0109
            10            TO01-AYSID  PICTURE  9(5).                    CI0109
            10            TO01-CTBMC  PICTURE  99.                      CI0109
            10            TO01-CINAR  PICTURE  99.                      CI0109
            10            TO01-CPHTR  PICTURE  X.                       CI0109
            10            TO01-CDSTR  PICTURE  XX.                      CI0109
            10            TO01-CQACT  PICTURE  999.                     CI0109
            10            TO01-CIRAS  PICTURE  999.                     CI0109
            10            TO01-CIRAT  PICTURE  999.                     CI0109
            10            TO01-CLRAY  PICTURE  9(5).                    CI0109
            10            TO01-CATTP  PICTURE  X.                       CI0109

      *PASS AREA TO/FROM CI9027
      *!WF DSP=PJ DSL=PJ SEL=46 FOR=I DES=1 LEV=1 PLT=30


      ******************************************************************
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *
      ******************************************************************
      *
      *!WF DSP=DE DSL=DU SEL=10 FOR=I DES=1 LEV=1 PLT=85
       01                 DE10.                                         CI0109
            10            DE10-DU11.                                    CI0109
            11            DE10-XFONC  PICTURE  X(4).                    CI0109
            11            DE10-MPSBN  PICTURE  X(8).                    CI0109
            11            DE10-XDBDNM PICTURE  X(08).                   CI0109
            11            DE10-XSEGNM PICTURE  X(08).                   CI0109
            11            DE10-XRC    PICTURE  X(02).                   CI0109
            11            DE10-MSEG   PICTURE  X(08).                   CI0109
            11            DE10-XCOKEY PICTURE  X(70).                   CI0109
            11            DE10-CUIBR  PICTURE  X(01).                   CI0109
            11            DE10-CUIBA  PICTURE  X(01).                   CI0109
            11            DE10-IPBIK  PICTURE  X(1).                    CI0109
            10            DE10-DU03.                                    CI0109
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            DE10-CMSSF  PICTURE  XX.                      CI0109
            11            DE10-DU09.                                    CI0109
            12            DE10-CMESA  PICTURE  S9(9)                    CI0109
                          BINARY.                                       CI0109
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0109
                          BINARY.                                       CI0109
            12            DE10-CMESB  PICTURE  S9(9)                    CI0109
                          BINARY.                                       CI0109
            12            DE10-CMSST  PICTURE  S9(9)                    CI0109
                          BINARY.                                       CI0109
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0109
                          BINARY.                                       CI0109
            12            DE10-QELLAA PICTURE  S9(9)                    CI0109
                          BINARY.                                       CI0109
            12            DE10-TMESS4 PICTURE  X(512).                  CI0109

      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0109
          05              MS00-SUITE.                                   CI0109
            15       FILLER         PICTURE  X(00542).                  CI0109
       01                 MS03  REDEFINES      MS00.                    CI0109
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            10            MS03-CMSSF  PICTURE  XX.                      CI0109
            10            MS03-DU09.                                    CI0109
            11            MS03-CMESA  PICTURE  S9(9)                    CI0109
                          BINARY.                                       CI0109
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0109
                          BINARY.                                       CI0109
            11            MS03-CMESB  PICTURE  S9(9)                    CI0109
                          BINARY.                                       CI0109
            11            MS03-CMSST  PICTURE  S9(9)                    CI0109
                          BINARY.                                       CI0109
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0109
                          BINARY.                                       CI0109
            11            MS03-QELLAA PICTURE  S9(9)                    CI0109
                          BINARY.                                       CI0109
            11            MS03-TMESS4 PICTURE  X(512).                  CI0109
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0109
            10            MX11-QMSGS  PICTURE  9(03).                   CI0109
            10            MX11-PJ09                                     CI0109
                          OCCURS       025     TIMES.                   CI0109
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0109
                          COMPUTATIONAL-3.                              CI0109
            11            MX11-CMESB  PICTURE  S9(9)                    CI0109
                          BINARY.                                       CI0109
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PE58
                                PJ27
                                CI47
                                CI52
                                CI53
                                CI57
                                CI67
                                FR01
                                TO01
                                PJ46
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0109
      *               *                                   *             CI0109
      *               *INITIALISATIONS                    *             CI0109
      *               *                                   *             CI0109
      *               *************************************.            CI0109
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
      *N02IN.    NOTE *INITIALIZE WORK FIELDS             *.
       F02IN.                                                           lv10
           INITIALIZE  WS10-WORK-AREAS.
       F02IN-FN. EXIT.
      *N02SC.    NOTE *SET PCB ADDRESSES                  *.
       F02SC.                                                           lv10
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF XE06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF XF06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
       F02SC-FN. EXIT.
      *N02VD.    NOTE *NUMERIC CHECK ON INPUT DATA        *.
       F02VD.    IF    PJ46-PWHLD NUMERIC                               lv10
                 AND   PJ46-CTWHAT NUMERIC
                 AND   PJ46-CCDSCW NUMERIC
                 AND   CI57-CCDSCW NUMERIC
                 AND   PJ46-CHCR NUMERIC
                 AND   PJ46-GETIM NUMERIC
                 AND   PJ46-DEFFT NUMERIC
                 AND   PJ27-DCACG NUMERIC
                 AND   PJ46-ADBRQ NUMERIC
                 AND   PJ46-PACT1 NUMERIC
                 AND   PJ46-QSHOWQ NUMERIC
                 AND   CI52-AATOTI NUMERIC
                 AND   CI52-AGOFD1 NUMERIC
                 AND   CI67-AMIND NUMERIC
                 AND   PJ46-AFEET NUMERIC
                 AND   CI47-AFEET NUMERIC
                 AND   PJ46-ATERF NUMERIC
                 AND   CI47-ATERF NUMERIC
                 AND   FR01-PRCOD NUMERIC
                 AND   FR01-PRSCD NUMERIC
                 AND   CI67-CELBL NUMERIC
                 AND   PJ46-AEXML NUMERIC
                 AND   CI67-ALPMOD NUMERIC
                 AND   CI67-QSHES NUMERIC
                 AND   CI52-QSHIS NUMERIC
                 AND   CI52-QSHOW NUMERIC
                 AND   CI52-AFAVP NUMERIC
                 AND   CI52-AWDRTP NUMERIC
                 AND   CI52-APPAYA NUMERIC
                 NEXT SENTENCE ELSE GO TO     F02VD-FN.
       F02VD-900. GO TO F02VF-FN.
       F02VD-FN. EXIT.
      *N02VF.    NOTE *ERROR IF NON NUMERIC INPUT         *.
       F02VF.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012309 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F02VF-FN. EXIT.
       F02-FN.   EXIT.
      *N03.      NOTE *************************************.
      *               *                                   *
      *               *CALCULATE MISCELLANEOUS AMOUNTS    *
      *               *                                   *
      *               *************************************.
       F03.           EXIT.                                             lv05
      *N03DA.    NOTE *IF EXPRESS MAIL FEE AMT APPLIES    *.
       F03DA.    IF    PJ46-AEXML > 0                                   lv10
                 NEXT SENTENCE ELSE GO TO     F03DA-FN.
      *DEDUCT IT FROM THE AMT AVAILABLE
      *FOR REDEMPTION
           SUBTRACT    PJ46-AEXML FROM CI52-AATOTI.
       F03DA-FN. EXIT.
      *N03JA.    NOTE *IF A PARTIAL REDEMPTION            *.
       F03JA.    IF    PJ46-CPORT = 'P'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F03JA-FN.
      *CALC ESTIMATED DISB AMT REQSTD
      *N03KA.    NOTE *$ AMT REQUESTED                    *.
       F03KA.    IF    PJ46-ADBRQ > ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F03KA-FN.
           MOVE        PJ46-ADBRQ TO PE58-AEDRQ.
       F03KA-FN. EXIT.
      *N03KD.    NOTE *SHR AMT REQUESTED                  *.
       F03KD.    IF    PJ46-QSHOWQ > ZERO                               lv15
                 NEXT SENTENCE ELSE GO TO     F03KD-FN.
      *MULTIPLY NO OF SHARES BY SHARE
      *PRICE
           COMPUTE     PE58-AEDRQ =
           PJ46-QSHOWQ * CI52-AFAVP.
       F03KD-FN. EXIT.
      *N03KG.    NOTE *PERCENTAGE AMT REQUESTED           *.
       F03KG.    IF    PJ46-PACT1 > ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F03KG-FN.
      *MULTIPLY TOT SHARES OF
      *ACCT BY % AND THEN SHARE PRICE
           COMPUTE     PE58-AEDRQ =
           (CI52-QSHOW * PJ46-PACT1 / 100)
           * CI52-AFAVP.
       F03KG-FN. EXIT.
       F03JA-900. GO TO F03LA-FN.
       F03JA-FN. EXIT.
      *N03LA.    NOTE *ELSE FULL MOVE ACCOUNT VALUE       *.
       F03LA.                                                           lv10
           MOVE        CI52-AATOTI TO PE58-AEDRQ.
       F03LA-FN. EXIT.
      *N03MA.    NOTE *CALCULATE NEW MONEY SHRS, $ AMT    *.
       F03MA.                                                           lv10
           COMPUTE     WS10-QSHNM = CI52-QSHOW -
           CI52-QSHOM
           COMPUTE     WS10-ACNMO = WS10-QSHNM *
           CI52-AFAVP.
       F03MA-FN. EXIT.
      *N03MF.    NOTE *CALC NET ASSETS                    *.
       F03MF.         EXIT.                                             lv10
      *N03MG.    NOTE *ACCT HAS CERT SHRS                 *.
       F03MG.    IF    CI52-QSHIS > 0                                   lv15
                 OR    CI67-QSHES > 0
                 NEXT SENTENCE ELSE GO TO     F03MG-FN.
      *OR LOI ESCROWED SHRS
           COMPUTE     WS10-ACNTA =
           ((CI52-QSHOW - CI52-QSHIS -
           CI67-QSHES)
           * CI52-AFAVP)
           - CI52-AWDRTP
           + CI52-APPAYA.
       F03MG-900. GO TO F03MK-FN.
       F03MG-FN. EXIT.
      *N03MK.    NOTE *NO CERT SHARES OR ESCROWED SHRS    *.
       F03MK.                                                           lv15
           MOVE        CI52-AATOTI TO WS10-ACNTA.
       F03MK-FN. EXIT.
       F03MF-FN. EXIT.
       F03-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0109
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0109
      *               *                                   *             CI0109
      *               *FIN DE TRAITEMENT                  *             CI0109
      *               *                                   *             CI0109
      *               *************************************.            CI0109
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0109
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N25.      NOTE *************************************.
      *               *                                   *
      *               *GET CLIENT ADDR; GET PLANNER       *
      *               *                                   *
      *               *************************************.
       F25.           EXIT.                                             lv05
      *N25DM.    NOTE *CHECK TO OWNER                     *.
       F25DM.    IF    PJ46-CPAYF = 'O' OR 'A'                          lv10
                 NEXT SENTENCE ELSE GO TO     F25DM-FN.
      *GET OWNER ADDRESS INFORMATION
      *CL24 VIA CT07 & CT09
      *GU CT01
           MOVE        FR01-CTID TO S-CTU01-CTID
           PERFORM     F94DA THRU F94DA-FN.
                 IF    IK = ZERO                                        DOT
      *CT01 FOUND
      *FETCH ADDRESS USING MACRO AOAADR
           PERFORM     F95VA THRU F95VA-FN.
       F25DM-FN. EXIT.
      *N25DP.    NOTE *GET SPECIAL PAYEE ADDRESS          *.
       F25DP.    IF    PJ46-CPAYF = 'S'                                 lv10
                 AND   PJ46-CSPDT = 'A'
                 NEXT SENTENCE ELSE GO TO     F25DP-FN.
      *N25DS.    NOTE *CALL CI0006 - CLIENT ADDRESS       *.            AM0006
       F25DS.                                                           lv15
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
           MOVE        PJ46-CLID TO CA05-CLID                           AM0006
           MOVE        PJ46-GECSQ TO CA05-GECSQ1                        AM0006
           MOVE        PJ27-DCACG TO CA05-DCACG                         AM0006
           SET CI0006A-PCB-CL1P-PTR1 TO                                 AM0006
                       PCB-CL1P-PTR1                                    AM0006
           INITIALIZE DE10-DU03                                         AM0006
           CALL        CI0006 USING                                     AM0006
           DFHEIBLK                                                     AM0006
           DFHCOMMAREA                                                  AM0006
           DLIUIBII                                                     AM0006
           CI0006A-PCB-ADDRESS-LIST                                     AM0006
           CA05                                                         AM0006
           DE10                                                         AM0006
           MS03.                                                        AM0006
      *N25DT.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F25DT.    IF    (MS03-NMESS2 > ZERO                              lv20
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F25DT-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0006 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0006 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F25DT-900. GO TO F25DU-FN.
       F25DT-FN. EXIT.
      *N25DU.    NOTE *NO ERRORS                          *.            ADU071
       F25DU.                                                           lv20
           INITIALIZE  MS03                                             ADU071
           MOVE        CA05-CL24 TO CL24.
       F25DU-FN. EXIT.
       F25DS-FN. EXIT.
       F25DP-FN. EXIT.
      *N25FA.    NOTE *GU CT01 FOR SOURCE ACCT            *.
       F25FA.                                                           lv10
           MOVE        FR01-CTID TO S-CTU01-CTID
           PERFORM     F94DA THRU F94DA-FN.
      *N25FB.    NOTE *CT01 NOT FOUND                     *.
       F25FB.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F25FB-FN.
      *---> Send BAD CONTRACT Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012004 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F25FB-FN. EXIT.
       F25FA-FN. EXIT.
      *N25FD.    NOTE *VALIDATE PLANNER ID IF ASSISTED    *.
       F25FD.    IF    PJ46-IPLIN = 'Y'                                 lv10
                 AND   PJ46-CLID8 > ZERO
                 NEXT SENTENCE ELSE GO TO     F25FD-FN.
      *AND PLANNER ID PRESENT
      *INITIAL READ OF CT18 (GN)
           PERFORM     F94EE THRU F94EE-FN.
                 IF    IK = '0'                                         DOT
      *STORE FIRST PLANNER FOR ACCOUNT
      *THIS ID WILL BE STORED ON THE
      *DBASE IF THE UI PLANNER ID IS
      *DIFFERENT FROM THE ACCOUNTS
           MOVE        CT18-CLID TO WS10-CLID8.
      *N25FK.    NOTE *READ THROUGH CT18S FOR PLANNER     *.
       F25FK.                       GO TO     F25FK-B.                  lv15
       F25FK-A.
                 IF    IK = '1'
                 OR    CT18-CLID = PJ46-CLID8
                                    GO TO     F25FK-FN.
       F25FK-B.
           PERFORM     F94EE THRU F94EE-FN.
       F25FK-900. GO TO F25FK-A.
       F25FK-FN. EXIT.
      *N25FN.    NOTE *MOVE FIRST PLANNER OF ACCOUNT      *.
       F25FN.    IF    CT18-CLID NOT = PJ46-CLID8                       lv15
                 NEXT SENTENCE ELSE GO TO     F25FN-FN.
      *IF UI PLANNER ID DOES NOT BELONG
      *THIS ACCOUNT
           MOVE        WS10-CLID8 TO PJ46-CLID8.
       F25FN-FN. EXIT.
       F25FD-900. GO TO F25FQ-FN.
       F25FD-FN. EXIT.
      *N25FQ.    NOTE *NOT PLANNER ASSISTED AND/OR NO     *.
       F25FQ.                                                           lv10
      *PLANNER ID
      *NOTE: IPLIN = Y BUT CLID8 = 0
      *ON ADVISOR OF THE DAY CALLS
      *N25FU.    NOTE *GET 'ACCT SERVICE REP' USING       *.
       F25FU.                                                           lv15
      *MACRO $CTSR2
           MOVE        PJ27-DCACG TO 7-CTSR-DCACG
           MOVE        ZEROS TO 7-CTSR-DEFFT
           MOVE        FR01-CTID TO 7-CTSR-CTID
           PERFORM     F94UC THRU F94UC-FN.
      *N25FW.    NOTE *CT18 FOUND. MOVE PLANNER ID.       *.
       F25FW.    IF    CT18-CF = '1'                                    lv20
                 NEXT SENTENCE ELSE GO TO     F25FW-FN.
           MOVE        7-CTSR-CLID TO PJ46-CLID8.
       F25FW-FN. EXIT.
       F25FU-FN. EXIT.
       F25FQ-FN. EXIT.
       F25-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *BUSINESS EDITS SOURCE              *
      *               *                                   *
      *               *************************************.
       F30.           EXIT.                                             lv05
      *N30CC.    NOTE *WITHHOLDING PERCENT OR AMT NOT     *.
       F30CC.    IF    (PJ46-PWHLD > 0                                  lv10
                 OR    PJ46-CTWHAT > 0)
                 AND   PJ27-IWTHH = 'N'
                 NEXT SENTENCE ELSE GO TO     F30CC-FN.
      *ALLOWED WHEN WITHHOLDING
      *INDICATOR IS NO
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013471 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CC-FN. EXIT.
      *N30CJ.    NOTE *WITHHOLDING PERCENT                *.
       F30CJ.    IF    (PJ46-PWHLD > 100                                lv10
                 OR    PJ46-PWHLD < 1)
                 AND   PJ46-PWHLD NOT = 0
                 NEXT SENTENCE ELSE GO TO     F30CJ-FN.
      *---> Send BAD SOURCE Message                                     ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013398 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CJ-FN. EXIT.
      *N30FF.    NOTE *UI WAIVER CODE MUST EQUAL          *.
       F30FF.    IF    CI57-ICDSC = 'Y'                                 lv10
                 AND   CI57-ICDSU = 'N'
                 AND   PJ46-CCDSCW NOT =
                       CI57-CCDSCW
                 AND   CI57-CCDSCW NOT = 6
                 NEXT SENTENCE ELSE GO TO     F30FF-FN.
      *CODE FROM CI0107 IF CODE IS
      *APPLICABLE AND CANNOT BE UPDATED
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013460 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30FF-FN. EXIT.
      *N30GF.    NOTE *CANNOT ENTER A FULL DISBURSEMENT   *.
       F30GF.    IF    PJ46-CPORT = 'F'                                 lv10
                 AND   CI52-CPORT = 'F'
                 NEXT SENTENCE ELSE GO TO     F30GF-FN.
      *WHEN ONE ALREADY EXISTS
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013453 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30GF-FN. EXIT.
      *N30JF.    NOTE *CHECK WHAT WAS REQUESTED           *.
       F30JF.                                                           lv10
                 IF    PJ46-ADBRQ > 0                                   DOT
      *A $ AMT WAS REQUESTED
           ADD         1 TO WS10-1CTAMT
           MOVE        ZERO TO PJ46-PACT1
           PJ46-QSHOWQ.
                 IF    PJ46-PACT1 > 0                                   DOT
      *A % WAS REQUESTED
           ADD         1 TO WS10-1CTAMT
           MOVE        ZERO TO PJ46-ADBRQ
           PJ46-QSHOWQ.
                 IF    PJ46-QSHOWQ > 0                                  DOT
      *SHRS WERE REQUESTED
           ADD         1 TO WS10-1CTAMT
           MOVE        ZERO TO PJ46-ADBRQ
           PJ46-PACT1.
       F30JF-FN. EXIT.
      *N30JJ.    NOTE *AMOUNTS MUST BE 0 WHEN FULL        *.
       F30JJ.    IF    PJ46-CPORT = 'F'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F30JJ-FN.
           MOVE        ZERO TO PJ46-ADBRQ
           PJ46-PACT1
           PJ46-QSHOWQ.
      *N30JK.    NOTE *AMOUNTS MUST BE 0 WHEN FULL        *.
       F30JK.    IF    WS10-1CTAMT NOT = 0                              lv15
                 NEXT SENTENCE ELSE GO TO     F30JK-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013389 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30JK-FN. EXIT.
       F30JJ-FN. EXIT.
      *N30JM.    NOTE *SELECT AN AMT WHEN CPORT = P       *.
       F30JM.    IF    PJ46-CPORT = 'P'                                 lv10
                 AND   WS10-1CTAMT = 0
                 NEXT SENTENCE ELSE GO TO     F30JM-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013390 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30JM-FN. EXIT.
      *N30JP.    NOTE *SELECT ONE AMT ONLY                *.
       F30JP.    IF    PJ46-CPORT = 'P'                                 lv10
                 AND   WS10-1CTAMT > 1
                 NEXT SENTENCE ELSE GO TO     F30JP-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013391 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30JP-FN. EXIT.
       F30-FN.   EXIT.
      *N31.      NOTE *************************************.
      *               *                                   *
      *               *EDIT AMOUNTS SOURCE                *
      *               *                                   *
      *               *************************************.
       F31.           EXIT.                                             lv05
      *N31CF.    NOTE *AMOUNT EDITS FOR PARTIAL ONLY      *.
       F31CF.    IF    PJ46-CPORT = 'P'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F31CF-FN.
      *N31CG.    NOTE *DEDUCT ACCRUED DIVIDEND FROM       *.
       F31CG.                                                           lv15
      *TOTAL AVAILABLE ASSET.
           MOVE        CI52-AATOTI TO WS00-AATOTI.
                 IF    CI52-ADDAC > 0                                   DOT
           SUBTRACT    CI52-ADDAC FROM WS00-AATOTI.
      *                                                                 DOT
       F31CG-FN. EXIT.
      *N31EF.    NOTE *INSUFFICIENT ASSETS                *.
       F31EF.    IF    PE58-AEDRQ > WS00-AATOTI                         lv15
                 NEXT SENTENCE ELSE GO TO     F31EF-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013448 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F31EF-FN. EXIT.
      *N31GF.    NOTE *REQUESTED DISB AMT CANNOT BE >     *.
       F31GF.    IF    PE58-AEDRQ > CI52-AGOFD1                         lv15
                 AND   (PJ46-CPAYF = 'W' OR 'D')
                 NEXT SENTENCE ELSE GO TO     F31GF-FN.
      *GOOD FUNDS AMOUNT FOR WIRES AND
      *DIRECT DEPOSITS
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013451 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F31GF-FN. EXIT.
       F31CF-FN. EXIT.
      *N31GK.    NOTE *ACCOUNT VALUE CANNOT BE > GOOD     *.
       F31GK.    IF    PJ46-CPORT = 'F'                                 lv10
                 AND   CI52-AATOTI > CI52-AGOFD1
                 AND   (PJ46-CPAYF = 'W' OR 'D')
                 NEXT SENTENCE ELSE GO TO     F31GK-FN.
      *FUNDS FOR A FULL DISBURSEMENT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013491 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F31GK-FN. EXIT.
      *N31HF.    NOTE *WITHHOLDING AMOUNT CANNOT BE >     *.
       F31HF.    IF    PJ46-CTWHAT > CI52-AATOTI                        lv10
                 NEXT SENTENCE ELSE GO TO     F31HF-FN.
      *GROSS ACCOUNT VALUE
      *---> Send BAD SOURCE Message                                     ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013399 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F31HF-FN. EXIT.
      *N31JD.    NOTE *MINIMUM PAYMENT EDITS FOR PHONE    *.
       F31JD.    IF    PJ46-CHCR = 03                                   lv10
                 OR    PJ46-IDRMD = 'Y'
                 NEXT SENTENCE ELSE GO TO     F31JD-FN.
      *DETERMINE CLIENTS AGE
           INITIALIZE  DD30 DD33 7-OAGE-PASSED-FIELDS
           MOVE        PJ27-DCACG TO DD33-DTGRG
           MOVE        4 TO DD30-CDTSF
           PERFORM     F91AB THRU F91AB-FN.
                 IF    DD30-CDTSC = 0                                   DOT
           MOVE        DD33-DTJUL TO 7-OAGE-CURRENT-DATE.
           INITIALIZE  DD30 DD33                                        DOT
           MOVE        CI67-CLDOB TO DD33-DTGRG
           MOVE        4 TO DD30-CDTSF
           PERFORM     F91AB THRU F91AB-FN.
                 IF    DD30-CDTSC = 0                                   DOT
           MOVE        DD33-DTJUL TO 7-OAGE-BIRTH-DATE.
                 IF    7-OAGE-CURRENT-DATE > ZERO                       DOT
                 AND   7-OAGE-BIRTH-DATE > ZERO
           PERFORM     F91FA THRU F91FA-FN.
      *N31JF.    NOTE *MINIMUM PAYMENT AMOUNT FOR         *.
       F31JF.    IF    PJ46-CPORT = 'P'                                 lv15
                 AND   (PJ46-CPAYF = 'A' OR 'S'
                 OR    PJ46-CPAYF = 'D' OR 'O')
                 AND   PE58-AEDRQ < CI67-AMIND
                 AND   PJ46-CHCR = 03
                 NEXT SENTENCE ELSE GO TO     F31JF-FN.
      *CLIENT ADDRESS, SPECIAL PAYEE
      *ADDRESS OR BANK BY PHONE
                 IF    FR01-CTIDA = 002                                 DOT
                 AND   FR01-CQACT > ZERO
                 AND   7-OAGE-CLIENT-AGE < 70.5
      *IF QUALIFIED MUTUAL FUND AND
      *CLIENT AGE < 70.5
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014597 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN                              ADU119
                 ELSE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013401 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F31JF-FN. EXIT.
       F31JD-FN. EXIT.
      *N31LF.    NOTE *AMT REQUESTED > NET ASSETS         *.
       F31LF.    IF    PE58-AEDRQ > WS10-ACNTA                          lv10
                 NEXT SENTENCE ELSE GO TO     F31LF-FN.
                 IF    CI52-QSHIS > ZERO                                DOT
                 AND   CI67-QSHES > ZERO
      *HAS BOTH CERT SHRS
      *AND LOI ESCROWED SHRS
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013892 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
                 IF    CI52-QSHIS > ZERO                                DOT
      *HAS CERT SHRS
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013449 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
                 IF    CI67-QSHES > ZERO                                DOT
      *HAS LOI ESCROWED SHRS
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013889 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F31LF-FN. EXIT.
       F31-FN.   EXIT.
      *N32.      NOTE *************************************.
      *               *                                   *
      *               *EDIT CUSTODIAL & TERMINAION FEE    *
      *               *                                   *
      *               *************************************.
       F32.           EXIT.                                             lv05
      *N32CD.    NOTE *CAN ONLY HAVE CUST FEE ON FULL     *.
       F32CD.    IF    PJ46-CPORT NOT = 'F'                             lv10
                 AND   PJ46-AFEET > 0
                 NEXT SENTENCE ELSE GO TO     F32CD-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013489 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F32CD-FN. EXIT.
      *N32CF.    NOTE *ANNUAL CUSTODIAL FEE IS PAID       *.
       F32CF.    IF    CI47-IACFPD = 'Y'                                lv10
                 AND   PJ46-AFEET > 0
                 NEXT SENTENCE ELSE GO TO     F32CF-FN.
      *UI FEE AMT CANNOT BE > 0
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013476 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F32CF-FN. EXIT.
      *N32DA.    NOTE *FEE EDITS IF CUSTODIAL ACCT        *.
       F32DA.    IF    CI47-ICUST (1) = 'Y'                             lv10
                 NEXT SENTENCE ELSE GO TO     F32DA-FN.
      *N32DB.    NOTE *ANNUAL CUSTODIAL FEE NOT PAID      *.
       F32DB.    IF    CI47-IACFPD = 'N'                                lv15
                 NEXT SENTENCE ELSE GO TO     F32DB-FN.
      *N32DC.    NOTE *EDIT CUSTODIAL FEE ERROR IF        *.
       F32DC.    IF    CI47-IWAIV = 'Y'                                 lv20
                 AND   CI47-AFEET > 0
                 NEXT SENTENCE ELSE GO TO     F32DC-FN.
      *FEE WAIVED AND UI FEE AMT > 0
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013404 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F32DC-FN. EXIT.
      *N32DE.    NOTE *EDITS WHEN CUSTODIAL FEE IS        *.
       F32DE.    IF    CI47-IWAIV = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F32DE-FN.
      *NOT WAIVED
      *N32DG.    NOTE *CUSTODIAL FEE MUST BE = 0 WHEN     *.
       F32DG.    IF    CI47-AFEET = 0                                   lv25
                 AND   PJ46-AFEET NOT = 0
                 NEXT SENTENCE ELSE GO TO     F32DG-FN.
      *IT HAS BEEN PAID
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013406 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F32DG-FN. EXIT.
      *N32DH.    NOTE *IF UI CUST FEE > 0 IT IS NOT A     *.
       F32DH.    IF    PJ46-AFEET > 0                                   lv25
                 NEXT SENTENCE ELSE GO TO     F32DH-FN.
      *FEE OVERIDE
      *N32DI.    NOTE *CUST FEE INPUT MUST = FEE DUE IF   *.
       F32DI.    IF    CI47-AFEET NOT =                                 lv30
                       PJ46-AFEET
                 NEXT SENTENCE ELSE GO TO     F32DI-FN.
      *NO OVERIDE FROM UI (FEE = 0)
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013405 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F32DI-FN. EXIT.
       F32DH-FN. EXIT.
       F32DE-FN. EXIT.
       F32DB-FN. EXIT.
       F32DA-FN. EXIT.
      *N32EB.    NOTE *CAN ONLY HAVE TERM FEE ON FULL     *.
       F32EB.    IF    PJ46-CPORT NOT = 'F'                             lv10
                 AND   PJ46-ATERF > 0
                 NEXT SENTENCE ELSE GO TO     F32EB-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013490 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N32ED.    NOTE *EDIT TERMINATION FEE - ERROR IF    *.
       F32ED.    IF    CI47-ITERF = 'N'                                 lv15
                 AND   PJ46-ATERF > 0
                 NEXT SENTENCE ELSE GO TO     F32ED-FN.
      *FEE NOT APPLICABLE AND AMT > 0
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013407 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F32ED-FN. EXIT.
      *N32EG.    NOTE *EDITS WHEN TERMINATION FEE IS      *.
       F32EG.    IF    CI47-ITERF = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F32EG-FN.
      *APPLICABLE
      *N32EI.    NOTE *UI TERMINATION FEE MUST = CI0108   *.
       F32EI.    IF    PJ46-ATERF > 0                                   lv20
                 NEXT SENTENCE ELSE GO TO     F32EI-FN.
      *FEE IF UI FEE > 0 (NO OVERIDE)
      *N32EJ.    NOTE *UI TERMINATION FEE MUST = THE      *.
       F32EJ.    IF    PJ46-ATERF NOT =                                 lv25
                       CI47-ATERF
                 NEXT SENTENCE ELSE GO TO     F32EJ-FN.
      *FEE FROM CI0108
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013408 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F32EJ-FN. EXIT.
       F32EI-FN. EXIT.
      *N32EM.    NOTE *UI FEE MUST BE = 0 WHEN IT HAS     *.
       F32EM.    IF    PJ46-ATERF > 0                                   lv20
                 AND   CI47-ATERF = 0
                 NEXT SENTENCE ELSE GO TO     F32EM-FN.
      *BEEN PAID
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013409 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F32EM-FN. EXIT.
       F32EG-FN. EXIT.
       F32EB-FN. EXIT.
       F32-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *SOURCE TO DESTINATION EDITS        *
      *               *                                   *
      *               *************************************.
       F35.           EXIT.                                             lv05
      *N35CC.    NOTE *NEW MONEY / OLD MONEY EDITS FROM   *.
       F35CC.    IF    PJ46-CPAYF = 'TR'                                lv10
                 NEXT SENTENCE ELSE GO TO     F35CC-FN.
      *CLASS A TO CLASS B ACCOUNTS
      *N35CG.    NOTE *TFM/CSH A TO CLASS B OR C DISB     *.
       F35CG.    IF    (FR01-PRCOD = 16                                 lv15
                 OR    ((FR01-PRCOD = 13
                 OR    FR01-PRCOD = 167)
                 AND   FR01-PRSCD = '000000001'))
                 AND   (TO01-PRSCD = '000000002'
                 OR    TO01-PRSCD = '000000006')
                 NEXT SENTENCE ELSE GO TO     F35CG-FN.
      *OR RVS GOVT MONEY MKT FD A TO
      *CLASS B OR C DISB
      *N35CK.    NOTE *FULL DISB                          *.
       F35CK.    IF    PJ46-CPORT = 'F'                                 lv20
                 AND   (CI52-QSHOM * CI52-AFAVP) >
                       99.99
                 NEXT SENTENCE ELSE GO TO     F35CK-FN.
      *ERR IF > 99.99 OF OLD MONEY
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013456 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35CK-FN. EXIT.
      *N35CN.    NOTE *PARTIAL DISB                       *.
       F35CN.    IF    PJ46-CPORT = 'P'                                 lv20
                 AND   PE58-AEDRQ >
                       (WS10-ACNMO + 99.99)
                 NEXT SENTENCE ELSE GO TO     F35CN-FN.
      *ERR IF REDM > NEW MONEY + 99.99
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013457 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35CN-FN. EXIT.
       F35CG-FN. EXIT.
       F35CC-FN. EXIT.
      *N35CP.    NOTE *DETERMINE SOURC IRA TYPE&STATUS    *.
       F35CP.                                                           lv10
           MOVE        FR01-CIRAT TO WS01-CIRAT.
                 IF    FR01-CIRAT = 7                                   DOT
           MOVE        0 TO WS01-CIRAS
                 ELSE
           MOVE        FR01-CIRAS TO WS01-CIRAS.
       F35CP-FN. EXIT.
      *N35CR.    NOTE *VALIDATE RMD INDICATOR,            *.
       F35CR.    IF    PJ46-IDRMD = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35CR-FN.
                 IF    FR01-CQACT NOT = ZEROS                           DOT
                 AND   FR01-CTIDA = 002
                 AND   FR01-PRSCD = ('000000002'
                 OR    '000000006')
                 AND   ((PJ46-CPAYF = 'TR'
                 AND   TO01-CQACT = ZEROS)
                 OR    PJ46-CPAYF = ('A' OR 'O'
                 OR    'W' OR 'D' OR 'S' OR 'P'))
                 AND   7-OAGE-CLIENT-AGE >= 70.5
                 AND   NOT INVALID-IRA
      *FROM QUALIFIED CLASS B/C FUND
      *DESTINATION IS NON-QUALIFIED
      *AMPF ACCOUNT OR CHECK TO CLIENT
      *OR  BANK OR PAYEE, AND CLIEN AGE
      *OVER 70.5, THEN RMD CAN BE YES,
           MOVE        'Y' TO PJ46-IDRMD
                 ELSE
      *---> Send ERROR Message                                          ADU119
      *      and                                                        ADU119
           MOVE        015458 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35CR-FN. EXIT.
      *N35DF.    NOTE *WHEN PLANNER ASSISTED IS YES       *.
       F35DF.    IF    PJ46-IPLIN = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35DF-FN.
      *N35DH.    NOTE *HOW RECEIVED MUST BE PHONE         *.
       F35DH.    IF    PJ46-CHCR = 03                                   lv15
                 NEXT SENTENCE ELSE GO TO     F35DH-FN.
       F35DH-900. GO TO F35DK-FN.
       F35DH-FN. EXIT.
      *N35DK.    NOTE *ERROR PLANNER ASSISTED             *.
       F35DK.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013379 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35DK-FN. EXIT.
       F35DF-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *DESTINATION EDITS                  *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40EF.    NOTE *UI SHOULD NOT SEND A CIRAP VALUE   *.
       F40EF.    IF    PJ46-CIRAP NOT = SPACES                          lv10
                 AND   CI53-CIRAP (1) = SPACES
                 NEXT SENTENCE ELSE GO TO     F40EF-FN.
      *---> Send BAD SOURCE Message                                     ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013461 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40EF-FN. EXIT.
      *N40EJ.    NOTE *UI SHOULD SEND A CIRAP VALUE       *.
       F40EJ.    IF    PJ46-CIRAP = SPACES                              lv10
                 AND   CI53-CIRAP (1) NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F40EJ-FN.
      *---> Send BAD SOURCE Message                                     ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013462 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40EJ-FN. EXIT.
      *N40EP.    NOTE *VALIDATE CONTRIBUTION TYPE FROM    *.
       F40EP.    IF    PJ46-CIRAP =                                     lv10
                       CI53-CIRAP (1)
                 OR    CI53-CIRAP (2)
                 OR    CI53-CIRAP (3)
                 OR    CI53-CIRAP (4)
                 OR    CI53-CIRAP (5)
                 OR    CI53-CIRAP (6)
                 OR    CI53-CIRAP (7)
                 OR    CI53-CIRAP (8)
                 OR    CI53-CIRAP (9)
                 OR    CI53-CIRAP (10)
                 NEXT SENTENCE ELSE GO TO     F40EP-FN.
      *UI AGAINST VALUES FROM CI0106
      *CI0106 OCCURANCE 1 TO 10
      *UI OCCURANCE 11
       F40EP-900. GO TO F40EQ-FN.
       F40EP-FN. EXIT.
      *N40EQ.    NOTE *ERROR ON INVALID CIRAP FOR TRAN    *.
       F40EQ.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013452 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40EQ-FN. EXIT.
      *N40FD.    NOTE *TRANSFER TO AEFA ACCT              *.
       F40FD.    IF    PJ46-CPAYF = 'TR'                                lv10
                 NEXT SENTENCE ELSE GO TO     F40FD-FN.
      *N40FF.    NOTE *VALIDATE PAYMENT TYPE CODE         *.
       F40FF.    IF    PJ46-CPMTCB NOT = 'LON'                          lv15
                 AND   PJ46-CPMTCB NOT = 'REG'
                 AND   PJ46-CPMTCB NOT = 'CD '
                 NEXT SENTENCE ELSE GO TO     F40FF-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013477 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40FF-FN. EXIT.
      *N40FI.    NOTE *LOAN REPAYMENT                     *.
       F40FI.    IF    PJ46-CPMTCB = 'LON'                              lv15
                 NEXT SENTENCE ELSE GO TO     F40FI-FN.
      *N40FJ.    NOTE *IS REPAYMENT TO LOAN ALLOWED       *.
       F40FJ.    IF    CI67-IARLNA = 'N'                                lv20
                 NEXT SENTENCE ELSE GO TO     F40FJ-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013478 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40FJ-FN. EXIT.
      *N40FK.    NOTE *REQUESTED DISB AMT > LOAN BAL      *.
       F40FK.    IF    PE58-AEDRQ > CI67-CELBL                          lv20
                 NEXT SENTENCE ELSE GO TO     F40FK-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013479 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40FK-FN. EXIT.
      *N40FN.    NOTE *REQUESTED DISB AMT < MIN LOAN      *.
       F40FN.    IF    PE58-AEDRQ < CI67-AMINAL                         lv20
                 AND   (TO01-CTIDA = 004 OR 005)
                 AND   (TO01-PRCOD = 210 OR 221)
                 NEXT SENTENCE ELSE GO TO     F40FN-FN.
      *REPAYMENT AMT
      *(SPL, VUL PRODUCTS)
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013480 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40FN-FN. EXIT.
       F40FI-FN. EXIT.
      *N40FP.    NOTE *CHANGE 'REG' TO ADD OR NEW OR CD   *.
       F40FP.    IF    PJ46-CPMTCB = 'REG'                              lv15
                 NEXT SENTENCE ELSE GO TO     F40FP-FN.
      *N40FQ.    NOTE *BASED ON ADMIN                     *.
       F40FQ.         EXIT.                                             lv20
      *N40FR.    NOTE *FINANCIAL PLANS: 'ADD'             *.
       F40FR.    IF    TO01-CTIDA =                                     lv25
                       013
                 NEXT SENTENCE ELSE GO TO     F40FR-FN.
           MOVE        'ADD' TO PJ46-CPMTCB.
       F40FR-900. GO TO F40FQ-FN.
       F40FR-FN. EXIT.
      *N40FS.    NOTE *BROKERAGE: 'CD ' (CASH DEPOSIT)    *.
       F40FS.    IF    TO01-CTIDA =                                     lv25
                       021 OR 133
                 NEXT SENTENCE ELSE GO TO     F40FS-FN.
           MOVE        'CD ' TO PJ46-CPMTCB.
       F40FS-900. GO TO F40FQ-FN.
       F40FS-FN. EXIT.
      *N40FT.    NOTE *ALL OTHER ADMINS                   *.
       F40FT.                                                           lv25
                 IF    TO01-CTSTA = 02                                  DOT
           MOVE        'ADD' TO PJ46-CPMTCB
                 ELSE
           MOVE        'NEW' TO PJ46-CPMTCB.
       F40FT-FN. EXIT.
       F40FQ-FN. EXIT.
       F40FP-FN. EXIT.
       F40FD-900. GO TO F40FU-FN.
       F40FD-FN. EXIT.
      *N40FU.    NOTE *INITIALIZE CPMTCB WHEN NOT TR      *.
       F40FU.                                                           lv10
           MOVE        SPACES TO PJ46-CPMTCB.
       F40FU-FN. EXIT.
      *N40GD.    NOTE *ALL FIELDS SENT FOR EXPRESS MAIL   *.
       F40GD.    IF    PJ46-CPAYF = 'S' OR 'A'                          lv10
                 OR    PJ46-CPAYF = 'P' OR 'CT'
                 OR    PJ46-CPAYF = 'O' OR 'W'
                 NEXT SENTENCE ELSE GO TO     F40GD-FN.
      *N40GF.    NOTE *ALL FIELDS SENT FOR EXPRESS MAIL   *.
       F40GF.    IF    PJ46-IEXML = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F40GF-FN.
      *N40GJ.    NOTE *IS PAYBY FIELD FILLED              *.
       F40GJ.    IF    PJ46-TPAYB = SPACES                              lv20
                 NEXT SENTENCE ELSE GO TO     F40GJ-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013481 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40GJ-FN. EXIT.
      *N40GM.    NOTE *IS AIR BILL FIELD FILLED           *.
       F40GM.    IF    PJ46-NAIRB = SPACES                              lv20
                 AND   PJ46-CPAYF NOT = 'W'
                 NEXT SENTENCE ELSE GO TO     F40GM-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013482 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40GM-FN. EXIT.
      *N40GO.    NOTE *IS THE FEE AMOUNT > 0              *.
       F40GO.    IF    PJ46-AEXML NOT > 0                               lv20
                 NEXT SENTENCE ELSE GO TO     F40GO-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013483 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40GO-FN. EXIT.
       F40GF-FN. EXIT.
       F40GD-900. GO TO F40GT-FN.
       F40GD-FN. EXIT.
      *N40GT.    NOTE *INITIALISE EXPRESS MAIL FIELDS     *.
       F40GT.                                                           lv10
           MOVE        ZERO TO PJ46-AEXML
           MOVE        'N' TO PJ46-IEXML
           MOVE        SPACES TO PJ46-TPAYB
           PJ46-NAIRB.
       F40GT-FN. EXIT.
      *N40KB.    NOTE *VALIDATE NAME LINE                 *.
       F40KB.    IF    (PJ46-CPAYF = 'S' OR 'P'                         lv10
                 OR    PJ46-CPAYF = 'W' OR 'CT'
                 OR    PJ46-CPAYF = 'D')
                 NEXT SENTENCE ELSE GO TO     F40KB-FN.
      *N40KD.    NOTE *VALIDATE NAME LINE                 *.
       F40KD.    IF    PJ46-GENAL1 = SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F40KD-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013446 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40KD-FN. EXIT.
      *N40KJ.    NOTE *VALIDATE ADDRESS LINES             *.
       F40KJ.    IF    PJ46-GESAD1 = SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F40KJ-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013447 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40KJ-FN. EXIT.
       F40KB-900. GO TO F40KK-FN.
       F40KB-FN. EXIT.
      *N40KK.    NOTE *INITIALISE ADDRESS LINES           *.
       F40KK.                                                           lv10
           MOVE        SPACES TO PJ46-GESAD1
           PJ46-GESAD2
           PJ46-GESAD3
           PJ46-GENAL1
           PJ46-GENAL2.
       F40KK-FN. EXIT.
      *N40LF.    NOTE *EDIT IF PAY TO A LIFE PREMIUM      *.
       F40LF.    IF    CI67-ALPMOD > 0                                  lv10
                 NEXT SENTENCE ELSE GO TO     F40LF-FN.
      *N40LI.    NOTE *CANNOT DO A FULL DISBURSEMENT      *.
       F40LI.    IF    PJ46-CPORT = 'F'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F40LI-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013473 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40LI-FN. EXIT.
      *N40LK.    NOTE *ONLY AMOUNT FOR PAYMENT TO LIFE    *.
       F40LK.    IF    PJ46-ADBRQ NOT > 0                               lv15
                 NEXT SENTENCE ELSE GO TO     F40LK-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013474 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40LK-FN. EXIT.
       F40LF-FN. EXIT.
       F40-FN.   EXIT.
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
      *N91AB.    NOTE *CDU - DATE VALIDATE/CONVERT        *.            AADA81
       F91AB.                                                           lv10
      ** * * * * * * * * * * * * * * *                                  AADA81
      *This code calls the common date                                  AADA81
      *utility MWS100EX to validate a                                   AADA81
      *gregorian date or convert a date                                 AADA81
      *with a dynamic call.                                             AADA81
      ** * * * * * * * * * * * * * * *                                  AADA81
      *Before the call set the subfunc                                  AADA81
      *request code DD30-CDTSF:                                         AADA81
      *  1 = greg to julian conversion                                  AADA81
      *      (without greg validation)                                  AADA81
      *  2 = julian to greg conversion                                  AADA81
      *  3 = gregorian validation                                       AADA81
      *  4 = greg to julian conversion                                  AADA81
      *      (with greg validation)                                     AADA81
      ** * * * * * * * * * * * * * * *                                  AADA81
      *Check return code DD30-CDTSC                                     AADA81
      *after the call.                                                  AADA81
      *    0 = Error Free                                               AADA81
      *    3 = Invalid Date                                             AADA81
      *    5 = Invalid Day                                              AADA81
      *    6 = Invalid Month                                            AADA81
      ** * * * * * * * * * * * * * * *                                  AADA81
           MOVE        3 TO DD30-CDTFN                                  AADA81
           CALL        MWS100EX USING DD30                              AADA81
           DD33.                                                        AADA81
       F91AB-FN. EXIT.
      *N91FA.    NOTE *MACRO AAOAG3  -  CALC CLIENT AGE   *.            AAOAG3
       F91FA.                                                           lv10
      *INITIALIZE WORK AREAS                                            AAOAG3
           MOVE        ZEROES TO 7-OAGE-CLIENT-AGE                      AAOAG3
           7-OAGE-AGE-DAYS                                              AAOAG3
           7-OAGE-AGE-YRS.                                              AAOAG3
                 IF    7-OAGE-CURRENT-DATE                              DOT
                       NOT NUMERIC                                      AAOAG3
                 OR    7-OAGE-CURRENT-DATE = ZEROES                     AAOAG3
                 OR    7-OAGE-BIRTH-DATE                                AAOAG3
                       NOT NUMERIC                                      AAOAG3
                 OR    7-OAGE-BIRTH-DATE = ZEROES                       AAOAG3
                 OR    7-OAGE-BD-CC = ZEROES                            AAOAG3
                 OR    7-OAGE-CD-CC = ZEROES                            AAOAG3
      *RETURN CLIENT AGE OF ZERO IF                                     AAOAG3
      *EITHER PASSED DATES ARE INVALID                                  AAOAG3
               GO TO     F91FA-FN.                                      AAOAG3
                 IF    7-OAGE-BIRTH-DATE >                              DOT
                       7-OAGE-CURRENT-DATE                              AAOAG3
      *RETURN CLIENT AGE OF ZERO IF                                     AAOAG3
      *BIRTH DATE > CURRENT DATE                                        AAOAG3
               GO TO     F91FA-FN.                                      AAOAG3
                 IF    7-OAGE-BD-DDD >                                  DOT
                       7-OAGE-CD-DDD                                    AAOAG3
      *SUBTRACT 1 FROM CURRENT DATE                                     AAOAG3
      *YEAR AND ADD 365 DAYS IF DAYS IN                                 AAOAG3
      *BIRTH DATE > CURRENT DATE DAYS                                   AAOAG3
           SUBTRACT    1 FROM 7-OAGE-CD-CCYY                            AAOAG3
           ADD         7-OAGE-DAYS-IN-A-YR TO                           AAOAG3
           7-OAGE-CD-DDD.                                               AAOAG3
      *CALCULATE AGE DAYS                                               DOT
           COMPUTE     7-OAGE-AGE-DAYS =                                AAOAG3
           7-OAGE-CD-DDD - 7-OAGE-BD-DDD.                               AAOAG3
                 IF    7-OAGE-AGE-DAYS NOT <                            DOT
                       7-OAGE-DAYS-IN-HALF-YR                           AAOAG3
      *ADD 1/2 YEAR TO CLIENT AGE                                       AAOAG3
           MOVE        7-OAGE-HALF-PERCENT TO                           AAOAG3
           7-OAGE-CLIENT-AGE.                                           AAOAG3
      *CALCULATE AGE YEARS                                              DOT
           COMPUTE     7-OAGE-AGE-YRS =                                 AAOAG3
           7-OAGE-CD-CCYY - 7-OAGE-BD-CCYY                              AAOAG3
      *ADD AGE YEARS TO CLIENT AGE                                      AAOAG3
           ADD         7-OAGE-AGE-YRS TO                                AAOAG3
           7-OAGE-CLIENT-AGE.                                           AAOAG3
       F91FA-FN. EXIT.
      *N93.      NOTE *************************************.            ADU129
      *               *                                   *             ADU129
      *               *---> Common DL/1 Error Checks      *             ADU129
      *               *                                   *             ADU129
      *               *************************************.            ADU129
       F93.           EXIT.                                             lv05
      *N93DA.    NOTE *NINES COMPLEMENT DATE FORMAT       *.            $9DAT2
       F93DA.                                                           lv10
           MOVE        99999999 TO 7-DA01-9DAT8                         $9DAT2
           SUBTRACT    7-DA01-IDAT8 FROM 7-DA01-9DAT8.                  $9DAT2
       F93DA-FN. EXIT.
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
      *N94CD.    NOTE *CALL GU ON CL24                    *.            ADU026
       F94CD.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL24' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XE06 CL24                                                    ADU026
           S-CLU01-SSA S-CLU24-SSA                                      ADU026
           MOVE        XE06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CD-FN. EXIT.
      *N94C7.    NOTE *CALL GN ON CT17                    *.            ADU026
       F94C7.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT17' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XF06 CT17                                                    ADU026
           S-CTU01-SSA 7-CTSR-SSA                                       ADU026
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C7-FN. EXIT.
      *N94C8.    NOTE *CALL GNP ON CT18                   *.            ADU026
       F94C8.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGNP                        ADU026
           XF06 CT18                                                    ADU026
           S-CTU01-SSA 7-CTSR-SSA                                       ADU026
           S-CT18-SSA
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGNP TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C8-FN. EXIT.
      *N94DA.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94DA.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XF06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DA-FN. EXIT.
      *N94DB.    NOTE *CALL GN ON CT07                    *.            ADU026
       F94DB.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT07' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XF06 CT07                                                    ADU026
           S-CTU01-SSA S-CT07-SSA                                       ADU026
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DB-FN. EXIT.
      *N94DC.    NOTE *CALL GN ON CT09                    *.            ADU026
       F94DC.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT09' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XF06 CT09                                                    ADU026
           S-CTU01-SSA S-CTU07-SSA                                      ADU026
           S-CT09-SSA
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DC-FN. EXIT.
      *N94EE.    NOTE *CALL GN ON CT18                    *.            ADU026
       F94EE.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XF06 CT18                                                    ADU026
           S-CTU01-SSA S-CT17-SSA                                       ADU026
           7-CTA18-1-SSA
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94EE-FN. EXIT.
      *N94UC.    NOTE *INITIALIZATION                     *.            $CTSR2
       F94UC.                                                           lv10
      *********************************                                 $CTSR2
      *** $CTSR GET ACCT SERVICE REP **                                 $CTSR2
      *********************************                                 $CTSR2
           MOVE        'N' TO 7-CTSR-IIDSW                              $CTSR2
           MOVE        '0' TO CT17-CF                                   $CTSR2
           MOVE        '0' TO CT18-CF                                   $CTSR2
      *CODE YOUR ERROR WITHIN THIS 'IF'                                 $CTSR2
                 IF    7-CTSR-DEFFT NOT NUMERIC                         DOT
           MOVE        'Y' TO 7-CTSR-IIDSW                              $CTSR2
               GO TO     F94UC-FN.                                      $CTSR2
      *CODE YOUR ERROR WITHIN THIS 'IF'                                 $CTSR2
                 IF    7-CTSR-DCACG NOT NUMERIC                         DOT
           MOVE        'Y' TO 7-CTSR-IIDSW                              $CTSR2
               GO TO     F94UC-FN.                                      $CTSR2
      *IF THE EFFECTIVE DATE IS ZERO,                                   DOT
      *USE THE CURRENT DATE AS THE                                      $CTSR2
      *EFFECTIVE DATE, TO FIND CURRENT                                  $CTSR2
      *ACTIVE SERVICE REP.                                              $CTSR2
                 IF    7-CTSR-DEFFT = ZERO                              DOT
           MOVE        7-CTSR-DCACG TO 7-CTSR-DEFFT.                    $CTSR2
      *TRANSLATE EFFECTIVE DATE INTO                                    DOT
      *TO 999'S COMPLEMENT.                                             $CTSR2
      * -THIS 999'S COMPLEMENT DATE                                     $CTSR2
      *  IS USED TO COMPARE START                                       $CTSR2
      *  DATES TO, AS START DATES ARE                                   $CTSR2
      *  ALSO 999'S COMPLEMENT.                                         $CTSR2
           MOVE        7-CTSR-DEFFT TO 7-DA01-IDAT8                     $CTSR2
           PERFORM     F93DA THRU F93DA-FN                              $CTSR2
           MOVE        7-DA01-9DAT8 TO 7-CTSR-9DEFFT                    $CTSR2
      *READ SUB-ACCOUNT CT17                                            $CTSR2
      *QUALIFY BOTH CT01 AND CT17                                       $CTSR2
      *READ SUB-ACCT,                                                   $CTSR2
      *  SUB-ACCT = 0,                                                  $CTSR2
           MOVE        7-CTSR-CTID TO S-CTU01-CT01K                     $CTSR2
           MOVE        'Y' TO 7-CTSR-IBASE                              $CTSR2
           PERFORM     F94C7 THRU F94C7-FN.                             $CTSR2
                 IF    IK = '1'                                         DOT
           MOVE        'Y' TO 7-CTSR-IIDSW                              $CTSR2
               GO TO     F94UC-FN.                                      $CTSR2
      *N94UH.    NOTE *FIND ACTIVE SERV REP               *.            $CTSR2
       F94UH.                                                           lv15
      *NEED SEQUENCE NO = 00                                            $CTSR2
      *AND....                                                          $CTSR2
      *   ROLE = 8                                                      $CTSR2
      *AND....                                                          $CTSR2
      *  START DATE >= EFFECT DATE                                      $CTSR2
      *    (BOTH IN 999'S COMPL),                                       $CTSR2
      *  END DATE = 0 OR >= EFFECT DATE                                 $CTSR2
           MOVE        LOW-VALUES TO S-CTU18-CT18K                      $CTSR2
           MOVE        'GE' TO S-CTU18-OPER                             $CTSR2
           MOVE        00 TO S-CTU18-GESQ2                              $CTSR2
           MOVE        008 TO S-CTU18-CMACR.                            $CTSR2
       F94UH-FN. EXIT.
      *N94UK.    NOTE *READ CT18'S                        *.            $CTSR2
       F94UK.                       GO TO     F94UK-B.                  lv15
       F94UK-A.
                 IF    IK = '1'                                         $CTSR2
                 OR    CT18-GESQ2 > 00                                  $CTSR2
                                    GO TO     F94UK-FN.                 $CTSR2
       F94UK-B.
           PERFORM     F94C8 THRU F94C8-FN.                             $CTSR2
      *N94UN.    NOTE *GOOD RECORD                        *.            $CTSR2
       F94UN.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F94UN-FN.                 $CTSR2
      *N94UT.    NOTE *   FOR ALL ORGS                    *.            $CTSR2
       F94UT.    IF    CT18-GESQ2 = 00                                  lv25
                 AND   CT18-CMACR = 008                                 $CTSR2
                 AND   (CT18-GERSD >                                    $CTSR2
                       7-CTSR-9DEFFT                                    $CTSR2
                       OR CT18-GERSD =                                  $CTSR2
                       7-CTSR-9DEFFT)                                   $CTSR2
                 AND   (CT18-GERED = ZERO                               $CTSR2
                       OR (CT18-GERED >                                 $CTSR2
                       7-CTSR-DEFFT                                     $CTSR2
                       OR CT18-GERED =                                  $CTSR2
                       7-CTSR-DEFFT))                                   $CTSR2
                 NEXT SENTENCE ELSE GO TO     F94UT-FN.                 $CTSR2
           MOVE        CT18-CLID TO 7-CTSR-CLID                         $CTSR2
           MOVE        '1' TO CT18-CF                                   $CTSR2
               GO TO     F94UK-FN.                                      $CTSR2
       F94UT-FN. EXIT.
       F94UN-FN. EXIT.
       F94UK-900. GO TO F94UK-A.
       F94UK-FN. EXIT.
       F94UC-FN. EXIT.
      *N94UZ.    NOTE ***** $CTSR  END ****************   *.            $CTSR2
       F94UZ.         EXIT.                                             lv10
       F94UZ-FN. EXIT.
      *N95VA.    NOTE *GET ACCOUNTS CORRECT ADDRESS       *.            AOAADR
       F95VA.                                                           lv10
           MOVE        ZERO TO CT07-CF                                  AOAADR
           CT09-CF                                                      AOAADR
           CL24-CF.                                                     AOAADR
      *N95VB.    NOTE *FIND CURRENT ADDRESS POINTER       *.            AOAADR
       F95VB.                                                           lv15
           MOVE        CT01-CT01K TO S-CTU01-CT01K.                     AOAADR
      *N95VD.    NOTE *READ THE CONTRACT/CLIENT INTERS.   *.            AOAADR
       F95VD.                       GO TO     F95VD-B.                  lv20
       F95VD-A.
                 IF    CT07-CF = ZERO                                   AOAADR
                 OR    CT09-CF = '1'                                    AOAADR
                                    GO TO     F95VD-FN.                 AOAADR
       F95VD-B.
           MOVE        ZERO TO CT07-CF                                  AOAADR
           PERFORM     F94DB THRU F94DB-FN.
                 IF    IK = ZERO                                        DOT
      *RELATED CLIENT FOUND                                             AOAADR
           MOVE        '1' TO CT07-CF                                   AOAADR
           MOVE        CT07-CT07K TO S-CTU07-CT07K                      AOAADR
                 ELSE                                                   AOAADR
      *RELATED CLIENT NOT FOUND                                         AOAADR
               GO TO     F95VD-900.                                     AOAADR
      *N95VF.    NOTE *LOOP CLIENT ROLES FOR ADDRESS      *.            AOAADR
       F95VF.                       GO TO     F95VF-B.                  lv25
       F95VF-A.
                 IF    IK = '1'                                         AOAADR
                 OR    CT09-CF = '1'                                    AOAADR
                                    GO TO     F95VF-FN.                 AOAADR
       F95VF-B.
      *POINTER                                                          AOAADR
           PERFORM     F94DC THRU F94DC-FN.
      *N95VG.    NOTE *CURRENT ADDRESS POINTER FOUND      *.            AOAADR
       F95VG.    IF    IK = ZERO                                        lv30
                 AND   CT09-GELL > +21                                  AOAADR
                 AND   CT09-GERED = ZEROES                              AOAADR
                 AND   CT09-GECSQ > ZERO                                AOAADR
                 NEXT SENTENCE ELSE GO TO     F95VG-FN.                 AOAADR
      *NOTE: CTO9-CF IS ONLY SET IF                                     AOAADR
      *IF A 'VALID' ADDRESS POINTER                                     AOAADR
      *IS FOUND ON THE CLIENT ROLE.                                     AOAADR
                 IF    (CT09-CLCTRC = '001'                             DOT
                 OR    '007')                                           AOAADR
      *OWNER ROLE HAS VALID POINTER                                     AOAADR
           MOVE        '1' TO CT09-CF.                                  AOAADR
                 IF    (CT01-CTIDA = 004                                DOT
                 OR    005)                                             AOAADR
                 AND   (CT09-CLCTRC = '002'                             AOAADR
                 OR    '003')                                           AOAADR
      *INSURED/ANNUITANT, VALID POINTER                                 AOAADR
      *ONLY ON LIFE ADMINS. 004 AND 005                                 AOAADR
           MOVE        '1' TO CT09-CF.                                  AOAADR
       F95VG-FN. EXIT.
       F95VF-900. GO TO F95VF-A.
       F95VF-FN. EXIT.
       F95VD-900. GO TO F95VD-A.
       F95VD-FN. EXIT.
       F95VB-FN. EXIT.
      *N95VI.    NOTE *GET CLIENT ADDRESS FOR ACCOUNT     *.            AOAADR
       F95VI.                                                           lv15
                 IF    CT09-CF = ZERO                                   DOT
      * NO VALID ADDRESS POINTER FOUND                                  AOAADR
      *MARK ROW WITH ERROR
           PERFORM     F95VZ THRU F95VZ-FN
               GO TO     F95VA-FN.                                      AOAADR
      * VALID ADDRESS POINTER FOUND                                     AOAADR
           MOVE        CT07-CLID TO S-CLU01-CL01K                       AOAADR
           MOVE        CT09-GECSQ TO S-CLU24-CL24K                      AOAADR
      * INITIALIZE OCCURS AREA                                          AOAADR
           MOVE        SPACE TO CL24-GESAD (1)                          AOAADR
           CL24-GESAD (2)                                               AOAADR
           CL24-GESAD (3)                                               AOAADR
           PERFORM     F94CD THRU F94CD-FN.
                 IF    IK = ZERO                                        DOT
      *ADDRESS FOUND ON CLIENT                                          AOAADR
           MOVE        '1' TO CL24-CF                                   AOAADR
                 ELSE                                                   AOAADR
      *NO  ADDRESS FOUND ON CLIENT                                      AOAADR
           MOVE        'UNKNOWN CLIENT ADDRESS' TO
           CL24-GESAD (1).
       F95VI-FN. EXIT.
       F95VA-FN. EXIT.
      *N95VZ.    NOTE *CT09 ERROR MESSAGE                 *.
       F95VZ.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013403 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F95VZ-FN. EXIT.
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
