       IDENTIFICATION DIVISION.                                         CI0103
       PROGRAM-ID.  CI0103P.                                            CI0103
      *AUTHOR.         AVAILABLE ASSETS UD.                             CI0103
      *DATE-COMPILED.   09/08/14.                                       CI0103
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
       ENVIRONMENT DIVISION.                                            CI0103
       CONFIGURATION SECTION.                                           CI0103
       SOURCE-COMPUTER. IBM-370.                                        CI0103
       OBJECT-COMPUTER. IBM-370.                                        CI0103
       DATA DIVISION.                                                   CI0103
       WORKING-STORAGE SECTION.                                         CI0103
      ******************************************************************
      ** LINKAGE TO CALL CI0033
      ******************************************************************
      *!WF DSP=WZ DSL=G6 SEL=52 FOR=I DES=1 LEV=1 PLT=CI
       01                 WZ52.                                         CI0103
            10            WZ52-CTID   PICTURE  X(27).                   CI0103
            10            WZ52-DASOF  PICTURE  9(8).                    CI0103
            10            WZ52-AACTV  PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            WZ52-QSHOW  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            WZ52-AFAVP  PICTURE  S9(4)V9(3)               CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            WZ52-DDVAC  PICTURE  9(8).                    CI0103
            10            WZ52-DLAUP2 PICTURE  9(8).                    CI0103
            10            WZ52-ADDAC  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            WZ52-CMESS  PICTURE  9.                       CI0103
            10            WZ52-NMESS  PICTURE  9(4).                    CI0103
            10            WZ52-CTEFD  PICTURE  9(8).                    CI0103
            10            WZ52-DPRGL  PICTURE  9(8).                    CI0103
            10            WZ52-QSHOM  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            WZ52-ADOMO  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            WZ52-IDDIV  PICTURE  X.                       CI0103
            10            WZ52-IDPAS  PICTURE  X.                       CI0103
            10            WZ52-AGRPV  PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            WZ52-QSHNM  PICTURE  S9(10)V999.              CI0103
            10            WZ52-SK73.                                    CI0103
            11            WZ52-CLDTY  PICTURE  XX.                      CI0103
            11            WZ52-NIDX   PICTURE  S9(8)                    CI0103
                          BINARY.                                       CI0103
            11            WZ52-ACFIF  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            WZ52-QSFIO  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            WZ52-ACFIFF PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            WZ52-QSFIF  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            WZ52-QSFID  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            WZ52-SA25                                     CI0103
                          OCCURS       015     TIMES.                   CI0103
            12            WZ52-SA25K.                                   CI0103
            13            WZ52-GEYR   PICTURE  9(4).                    CI0103
            12            WZ52-ACFIFB PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            WZ52-QSFIB  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            WZ52-SA26                                     CI0103
                          OCCURS       025     TIMES.                   CI0103
            12            WZ52-GSA26K.                                  CI0103
            13            WZ52-DL42.                                    CI0103
            14            WZ52-GEYR1  PICTURE  9(4).                    CI0103
            14            WZ52-GEMTH1 PICTURE  99.                      CI0103
            14            WZ52-NDAY01 PICTURE  99.                      CI0103
            12            WZ52-GSA26.                                   CI0103
            13            WZ52-ACFIFN PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WZ52-QSFIBN PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            WZ52-IINDV  PICTURE  X.                       CI0103
            11            WZ52-IINEG  PICTURE  X.                       CI0103
            11            WZ52-GERTC  PICTURE  X.                       CI0103
            11            WZ52-XFONC  PICTURE  X(4).                    CI0103
            10            WZ52-QDHGF  PICTURE  9(2).                    CI0103
            10            WZ52-QSHIS  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            WZ52-QSHES  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            WZ52-NACID  PICTURE  S9(11)                   CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            WZ52-IBIFU  PICTURE  X.                       CI0103
            10            WZ52-QCSAC  PICTURE  S9(11)V9(4).             CI0103
            10            WZ52-ACOSA  PICTURE  S9(13)V99.               CI0103
            10            WZ52-ACACB  PICTURE  S9(13)V99.               CI0103
            10            WZ52-QNCOS  PICTURE  S9(11)V9(4).             CI0103
            10            WZ52-ANCSA  PICTURE  S9(13)V99.               CI0103
            10            WZ52-ANCBA  PICTURE  S9(13)V99.               CI0103
            10            WZ52-ICOVE  PICTURE  X.                       CI0103
            10            WZ52-ICOVN  PICTURE  X.                       CI0103
            10            WZ52-CRCDL  PICTURE  X.                       CI0103
            10            WZ52-FILLER PICTURE  X(100).                  CI0103
      **
      ******************************************************************
      ** LINKAGE TO CALL CI0100
      ******************************************************************
      *!WF DSP=WJ DSL=PJ SEL=40 FOR=I DES=1 LEV=1 PLT=CI
       01                 WJ40.                                         CI0103
            10            WJ40-MAPPN  PICTURE  X(10).                   CI0103
            10            WJ40-CFUNC  PICTURE  X(3).                    CI0103
            10            WJ40-CASTC  PICTURE  99                       CI0103
                          OCCURS       006     TIMES.                   CI0103
            10            WJ40-CAATY  PICTURE  9(3)                     CI0103
                          OCCURS       003     TIMES.                   CI0103
            10            WJ40-C299.                                    CI0103
            11            WJ40-CTID.                                    CI0103
            12            WJ40-CTIDA  PICTURE  9(3).                    CI0103
            12            WJ40-CTIDN.                                   CI0103
            13            WJ40-CTIDNP PICTURE  X(13).                   CI0103
            13            WJ40-CTIDND PICTURE  9(11).                   CI0103
            10            WJ40-DCACG9 PICTURE  9(8).                    CI0103
            10            WJ40-NAASQ  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            WJ40-NPISQ  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            WJ40-CIRAP  PICTURE  XX.                      CI0103
            10            WJ40-IPERT  PICTURE  X.                       CI0103
            10            WJ40-NEIBT  PICTURE  X(7).                    CI0103
            10            WJ40-GESQ2C PICTURE  S99                      CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            WJ40-MIPPS  PICTURE  X(4).                    CI0103
            10            WJ40-IENDP  PICTURE  X.                       CI0103
            10            WJ40-FILLER PICTURE  X(20).                   CI0103
       01                 WJ41.                                         CI0103
            10            WJ41-IENDP  PICTURE  X.                       CI0103
            10            WJ41-MIPPS  PICTURE  X(4).                    CI0103
            10            WJ41-GC01.                                    CI0103
            11            WJ41-GC01K.                                   CI0103
            12            WJ41-C299.                                    CI0103
            13            WJ41-CTID.                                    CI0103
            14            WJ41-CTIDA  PICTURE  9(3).                    CI0103
            14            WJ41-CTIDN.                                   CI0103
            15            WJ41-CTIDNP PICTURE  X(13).                   CI0103
            15            WJ41-CTIDND PICTURE  9(11).                   CI0103
            11            WJ41-DCAG9L PICTURE  9(8).                    CI0103
            11            WJ41-NAASQL PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            WJ41-ICUST  PICTURE  X.                       CI0103
            11            WJ41-NSEQ4B PICTURE  9(8)                     CI0103
                          BINARY.                                       CI0103
            11            WJ41-PRCOD  PICTURE  9(5).                    CI0103
            11            WJ41-PRSCD  PICTURE  X(9).                    CI0103
            11            WJ41-FILLER PICTURE  X(8).                    CI0103
            10            WJ41-IGC01  PICTURE  X(01).                   CI0103
            10            WJ41-QDECT9 PICTURE  99.                      CI0103
            10            WJ41-FILLER PICTURE  X(20).                   CI0103
            10            WJ41-GAKEY                                    CI0103
                          OCCURS       010     TIMES.                   CI0103
            11            WJ41-IGC03  PICTURE  X(01).                   CI0103
            11            WJ41-IGC04  PICTURE  X(01).                   CI0103
            11            WJ41-IGC06  PICTURE  X(01).                   CI0103
            11            WJ41-IGC12  PICTURE  X(01).                   CI0103
            11            WJ41-IGC21  PICTURE  X(01).                   CI0103
            11            WJ41-GC03.                                    CI0103
            12            WJ41-GELL   PICTURE  9(4)                     CI0103
                          BINARY.                                       CI0103
            12            WJ41-GD00.                                    CI0103
            13            WJ41-GC03K.                                   CI0103
            14            WJ41-DCACG9 PICTURE  9(8).                    CI0103
            14            WJ41-NAASQ  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CAATY  PICTURE  9(3).                    CI0103
            13            WJ41-CVSYS  PICTURE  X(2).                    CI0103
            13            WJ41-CACTO  PICTURE  9(3).                    CI0103
            13            WJ41-CATRN.                                   CI0103
            14            WJ41-CATRF  PICTURE  9(3).                    CI0103
            14            WJ41-CATRS  PICTURE  9(3).                    CI0103
            13            WJ41-CASTC  PICTURE  99.                      CI0103
            13            WJ41-IPULL  PICTURE  X.                       CI0103
            13            WJ41-GEAUN  PICTURE  9(5).                    CI0103
            13            WJ41-GEOPD2 PICTURE  X(8).                    CI0103
            13            WJ41-NBTCH  PICTURE  9(4).                    CI0103
            13            WJ41-DEFFT  PICTURE  9(8).                    CI0103
            13            WJ41-NSUNT  PICTURE  9(4).                    CI0103
            13            WJ41-ITRAN  PICTURE  X.                       CI0103
            13            WJ41-DLAUP1 PICTURE  9(8).                    CI0103
            13            WJ41-ADRET  PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-TTRMS  PICTURE  X(12).                   CI0103
            13            WJ41-IDELT  PICTURE  X.                       CI0103
            13            WJ41-GEOPDM PICTURE  X(8).                    CI0103
            13            WJ41-FILLER PICTURE  X(07).                   CI0103
            12            WJ41-GD09.                                    CI0103
            13            WJ41-FILLER PICTURE  X(70).                   CI0103
            12            WJ41-GD01                                     CI0103
                          REDEFINES            WJ41-GD09.               CI0103
            13            WJ41-ADBRQ  PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CTRTP  PICTURE  X(2).                    CI0103
            13            WJ41-CPORT  PICTURE  X.                       CI0103
            13            WJ41-CSCRNU PICTURE  X(4).                    CI0103
            13            WJ41-DLAUP  PICTURE  9(8).                    CI0103
            13            WJ41-CTWHAT PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-PWHLD  PICTURE  S999V9(5)                CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-IWTHH  PICTURE  X.                       CI0103
            13            WJ41-NDRFT  PICTURE  9(5).                    CI0103
            13            WJ41-IDPAP  PICTURE  X.                       CI0103
            13            WJ41-GETIM  PICTURE  S9(7)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-QNACT  PICTURE  9(3).                    CI0103
            13            WJ41-AEDRQ  PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-IPLIN  PICTURE  X.                       CI0103
            13            WJ41-CLIDNB PICTURE  9(8).                    CI0103
            13            WJ41-CSLCT  PICTURE  X.                       CI0103
            13            WJ41-ITELE  PICTURE  X.                       CI0103
            13            WJ41-FILLER PICTURE  X(06).                   CI0103
            12            WJ41-GD02                                     CI0103
                          REDEFINES            WJ41-GD09.               CI0103
            13            WJ41-CSYST  PICTURE  99.                      CI0103
            13            WJ41-FILLER PICTURE  X.                       CI0103
            13            WJ41-ACASH  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-DTRAC  PICTURE  9(8).                    CI0103
            13            WJ41-CTRSO  PICTURE  9(02).                   CI0103
            13            WJ41-NTRCE  PICTURE  9(06).                   CI0103
            13            WJ41-GECKD1 PICTURE  9.                       CI0103
            13            WJ41-CCOLL  PICTURE  X(3).                    CI0103
            13            WJ41-CLTDP  PICTURE  X(3).                    CI0103
            13            WJ41-PSLLD  PICTURE  S99V999                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-ISLOR  PICTURE  X.                       CI0103
            13            WJ41-ITPAC  PICTURE  X.                       CI0103
            13            WJ41-CPMTCA PICTURE  XXX.                     CI0103
            13            WJ41-CSERV  PICTURE  X(3).                    CI0103
            13            WJ41-ACOMO  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-IPLIN1 PICTURE  X.                       CI0103
            13            WJ41-INQEX  PICTURE  X.                       CI0103
            13            WJ41-CTKRAA PICTURE  X(12).                   CI0103
            13            WJ41-CCSMQ  PICTURE  X.                       CI0103
            13            WJ41-IVAEX1 PICTURE  X.                       CI0103
            13            WJ41-IHPMT  PICTURE  X(1).                    CI0103
            13            WJ41-GETIM3 PICTURE  S9(7)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            WJ41-GD03                                     CI0103
                          REDEFINES            WJ41-GD09.               CI0103
            13            WJ41-CATRNC PICTURE  9(6).                    CI0103
            13            WJ41-APRNT1 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-QSHOWT PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-ACINVT PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-ACOMO7 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-QSHOMW PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-ATAXT3 PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CTSTR  PICTURE  9(2).                    CI0103
            13            WJ41-ICIRA  PICTURE  X.                       CI0103
            13            WJ41-GETIM2 PICTURE  S9(7)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CPMTCX PICTURE  XX.                      CI0103
            13            WJ41-FILLER PICTURE  X(16).                   CI0103
            12            WJ41-GD99.                                    CI0103
            13            WJ41-FILLER PICTURE  X(248).                  CI0103
            12            WJ41-GD10                                     CI0103
                          REDEFINES            WJ41-GD99.               CI0103
            13            WJ41-MROTC  PICTURE  X(7).                    CI0103
            13            WJ41-CEDSC  PICTURE  9(1).                    CI0103
            13            WJ41-ILPOI  PICTURE  X(1).                    CI0103
            13            WJ41-AWRCH  PICTURE  S9(3)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CHCOC1 PICTURE  9(2).                    CI0103
            13            WJ41-CHCOC2 PICTURE  9(2).                    CI0103
            13            WJ41-CHCOC3 PICTURE  9(2).                    CI0103
            13            WJ41-CHCOC4 PICTURE  9(2).                    CI0103
            13            WJ41-CMCOC1 PICTURE  9(3).                    CI0103
            13            WJ41-CMCOC2 PICTURE  9(3).                    CI0103
            13            WJ41-CMCOC3 PICTURE  9(3).                    CI0103
            13            WJ41-GD11.                                    CI0103
            14            WJ41-FILLER PICTURE  X(219).                  CI0103
            13            WJ41-GD12                                     CI0103
                          REDEFINES            WJ41-GD11.               CI0103
            14            WJ41-CELLO  PICTURE  9(1).                    CI0103
            14            WJ41-CECLO  PICTURE  9(1).                    CI0103
            14            WJ41-AEXML  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-CEPI   PICTURE  X(1).                    CI0103
            14            WJ41-CEXTY  PICTURE  X.                       CI0103
            14            WJ41-CROPC  PICTURE  9(1).                    CI0103
            14            WJ41-CPUTY  PICTURE  9(1).                    CI0103
            14            WJ41-IMCII  PICTURE  X(1).                    CI0103
            14            WJ41-GEMISC                                   CI0103
                          OCCURS       010     TIMES.                   CI0103
            15            WJ41-AMGLA  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            15            WJ41-CMGLC  PICTURE  9(1).                    CI0103
            15            WJ41-NMGLN  PICTURE  9(4).                    CI0103
            14            WJ41-ACTRN  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-IWRBK  PICTURE  X.                       CI0103
            14            WJ41-IFEDX  PICTURE  X.                       CI0103
            14            WJ41-ICNTR  PICTURE  X.                       CI0103
            14            WJ41-IOCKH  PICTURE  X.                       CI0103
            14            WJ41-ICRCK  PICTURE  X.                       CI0103
            14            WJ41-NHMPN  PICTURE  S9(10)                   CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-ITELR1 PICTURE  X.                       CI0103
            13            WJ41-GD13                                     CI0103
                          REDEFINES            WJ41-GD11.               CI0103
            14            WJ41-DREDO  PICTURE  9(8).                    CI0103
            14            WJ41-CATRNR PICTURE  9(6).                    CI0103
            14            WJ41-CEVN   PICTURE  9(9).                    CI0103
            14            WJ41-ISUSP  PICTURE  X(1).                    CI0103
            13            WJ41-GD15                                     CI0103
                          REDEFINES            WJ41-GD11.               CI0103
            14            WJ41-CPUTZ  PICTURE  9(1).                    CI0103
            14            WJ41-CETLB  PICTURE  9(3).                    CI0103
            14            WJ41-QTRMC  PICTURE  9(3).                    CI0103
            14            WJ41-DEFFTE PICTURE  9(8).                    CI0103
            14            WJ41-DEFFTF PICTURE  9(8).                    CI0103
            14            WJ41-DEFFTG PICTURE  9(8).                    CI0103
            14            WJ41-XZ1A   PICTURE  X.                       CI0103
            14            WJ41-XZ1B   PICTURE  X.                       CI0103
            14            WJ41-XZ1C   PICTURE  X.                       CI0103
            14            WJ41-XZ1D   PICTURE  X.                       CI0103
            14            WJ41-XZ1E   PICTURE  X.                       CI0103
            14            WJ41-XZ1F   PICTURE  X.                       CI0103
            14            WJ41-XZ1G   PICTURE  X.                       CI0103
            14            WJ41-XZ1H   PICTURE  X.                       CI0103
            14            WJ41-XZ1I   PICTURE  X.                       CI0103
            14            WJ41-DEFFTH PICTURE  9(8).                    CI0103
            13            WJ41-GD19                                     CI0103
                          REDEFINES            WJ41-GD11.               CI0103
            14            WJ41-GD11.                                    CI0103
            15            WJ41-FILLER PICTURE  X(219).                  CI0103
            12            WJ41-GD20                                     CI0103
                          REDEFINES            WJ41-GD99.               CI0103
            13            WJ41-ADDACT PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-ISIGV  PICTURE  X.                       CI0103
            13            WJ41-IALLF  PICTURE  X.                       CI0103
            13            WJ41-QSHOWQ PICTURE  S9(9)V999                CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CCDSCW PICTURE  9(2).                    CI0103
            13            WJ41-IDWRL  PICTURE  X.                       CI0103
            13            WJ41-ITELR  PICTURE  X.                       CI0103
            13            WJ41-IABIN  PICTURE  X.                       CI0103
            13            WJ41-PACT1  PICTURE  S999V999                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-IBFAF  PICTURE  X.                       CI0103
            13            WJ41-IFRSA  PICTURE  X.                       CI0103
            13            WJ41-ICRCAN PICTURE  X.                       CI0103
            13            WJ41-ACACTV PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-AGFND  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-QCSHOW PICTURE  S9(9)V999                CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-QCSHIS PICTURE  S9(9)V999                CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-NDTRC  PICTURE  9(8).                    CI0103
            13            WJ41-CAERU  PICTURE  X(4).                    CI0103
            13            WJ41-IFDGO  PICTURE  X.                       CI0103
            13            WJ41-PSLLD2 PICTURE  S99V999                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-ISLOR2 PICTURE  X.                       CI0103
            13            WJ41-QSFIO  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-QSFID  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CGDIN  PICTURE  X.                       CI0103
            13            WJ41-DGDIN  PICTURE  9(8).                    CI0103
            12            WJ41-GD30                                     CI0103
                          REDEFINES            WJ41-GD99.               CI0103
            13            WJ41-ISKED  PICTURE  X.                       CI0103
            13            WJ41-CENXC  PICTURE  9(2).                    CI0103
            13            WJ41-GD31.                                    CI0103
            14            WJ41-FILLER PICTURE  X(245).                  CI0103
            13            WJ41-GD32                                     CI0103
                          REDEFINES            WJ41-GD31.               CI0103
            14            WJ41-IABIN1 PICTURE  X.                       CI0103
            14            WJ41-CLDOD  PICTURE  9(8).                    CI0103
            14            WJ41-NCLAM  PICTURE  9(5)                     CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-ISURR  PICTURE  X.                       CI0103
            14            WJ41-GEHCD  PICTURE  9(3).                    CI0103
            14            WJ41-CRATC  PICTURE  9(4).                    CI0103
            14            WJ41-AMAXD  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-ASCHGA PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-APYOM  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-IWTHH1 PICTURE  X.                       CI0103
            14            WJ41-CPAYCL PICTURE  X(2).                    CI0103
            14            WJ41-CTSAO  PICTURE  X.                       CI0103
            14            WJ41-NCONF  PICTURE  9(08).                   CI0103
            14            WJ41-CLID   PICTURE  X(23).                   CI0103
            14            WJ41-CARTY  PICTURE  99.                      CI0103
            14            WJ41-NARRS  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-CARTZ  PICTURE  99.                      CI0103
            14            WJ41-NAPDS  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-CPMTO  PICTURE  X.                       CI0103
            14            WJ41-DNPMT  PICTURE  9(8).                    CI0103
            14            WJ41-IPCTV  PICTURE  X.                       CI0103
            14            WJ41-IMECH  PICTURE  X(01).                   CI0103
            14            WJ41-IMVAO  PICTURE  X(1).                    CI0103
            14            WJ41-AMVA1  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-CACTS  PICTURE  X.                       CI0103
            14            WJ41-CTSPP  PICTURE  X(1).                    CI0103
            14            WJ41-CACT4  PICTURE  X(2).                    CI0103
            14            WJ41-IVAEX  PICTURE  X.                       CI0103
            14            WJ41-DFPMT  PICTURE  9(8).                    CI0103
            14            WJ41-IDEMD  PICTURE  X.                       CI0103
            14            WJ41-IOFST  PICTURE  X.                       CI0103
            14            WJ41-AMXLB  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-ACULB  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-DEIRNB PICTURE  9(8).                    CI0103
            14            WJ41-DEFFE  PICTURE  9(8).                    CI0103
            14            WJ41-DEFFR  PICTURE  9(8).                    CI0103
            14            WJ41-ISPUP  PICTURE  X.                       CI0103
            14            WJ41-CPNCG  PICTURE  X.                       CI0103
            14            WJ41-IEXPU  PICTURE  X.                       CI0103
            14            WJ41-IPPCF  PICTURE  X.                       CI0103
            14            WJ41-NAAPT  PICTURE  9(2).                    CI0103
            14            WJ41-PWHLDS PICTURE  S999V9(5)                CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-ISWHO  PICTURE  X(1).                    CI0103
            13            WJ41-GD33                                     CI0103
                          REDEFINES            WJ41-GD31.               CI0103
            14            WJ41-CPAYC  PICTURE  X(2).                    CI0103
            14            WJ41-ADBRQX PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-ADBRQV PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-APTXR  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-CTRTPE PICTURE  X(2).                    CI0103
            14            WJ41-NCLAMI PICTURE  S9(9)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-CLIDO8 PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-CLIDN  PICTURE  X(20).                   CI0103
            14            WJ41-DSET01 PICTURE  S9(8)                    CI0103
                          BINARY.                                       CI0103
            14            WJ41-CTSET1 PICTURE  S9(6)                    CI0103
                          BINARY.                                       CI0103
            14            WJ41-DSET02 PICTURE  S9(8)                    CI0103
                          BINARY.                                       CI0103
            14            WJ41-CTSET2 PICTURE  S9(6)                    CI0103
                          BINARY.                                       CI0103
            13            WJ41-GD34                                     CI0103
                          REDEFINES            WJ41-GD31.               CI0103
            14            WJ41-QNOFM  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-CLTRM  PICTURE  99.                      CI0103
            14            WJ41-AMXLN  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-ALADJ  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-ACHK   PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-APRMO  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-IMECH1 PICTURE  X(01).                   CI0103
            14            WJ41-CACT41 PICTURE  X(2).                    CI0103
            14            WJ41-ACDSCC PICTURE  S9(05)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-ACDSCD PICTURE  S9(05)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-GD39                                     CI0103
                          REDEFINES            WJ41-GD31.               CI0103
            14            WJ41-GD31.                                    CI0103
            15            WJ41-FILLER PICTURE  X(245).                  CI0103
            12            WJ41-GD40                                     CI0103
                          REDEFINES            WJ41-GD99.               CI0103
            13            WJ41-NTR    PICTURE  9(8).                    CI0103
            13            WJ41-NPBNC  PICTURE  X(24).                   CI0103
            13            WJ41-CRREV  PICTURE  X(3).                    CI0103
            13            WJ41-CSUSL  PICTURE  S9.                      CI0103
            13            WJ41-NMGLN1 PICTURE  9(4).                    CI0103
            13            WJ41-DCAC92 PICTURE  9(8).                    CI0103
            13            WJ41-NAASQ3 PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-GD49.                                    CI0103
            14            WJ41-FILLER PICTURE  X(198).                  CI0103
            13            WJ41-GD41                                     CI0103
                          REDEFINES            WJ41-GD49.               CI0103
            14            WJ41-CRREF  PICTURE  9(2).                    CI0103
            14            WJ41-CORIR  PICTURE  X(02).                   CI0103
            14            WJ41-CIPDB  PICTURE  X(03).                   CI0103
            14            WJ41-CPAYH  PICTURE  X(02).                   CI0103
            14            WJ41-NAMEX  PICTURE  9(15)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-DCHAE  PICTURE  9(4).                    CI0103
            14            WJ41-DRQST  PICTURE  S9(8)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-GD42                                     CI0103
                          REDEFINES            WJ41-GD49.               CI0103
            14            WJ41-CPMTCB PICTURE  X(3).                    CI0103
            12            WJ41-GD50                                     CI0103
                          REDEFINES            WJ41-GD99.               CI0103
            13            WJ41-ALOAD  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-PSLLD4 PICTURE  S99V999                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CSUSL1 PICTURE  S9.                      CI0103
            13            WJ41-CRREV1 PICTURE  X(3).                    CI0103
            13            WJ41-ADDAC  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-DL13.                                    CI0103
            14            WJ41-GEYR   PICTURE  9(4).                    CI0103
            14            WJ41-GEMTH  PICTURE  99.                      CI0103
            14            WJ41-NDAY   PICTURE  99.                      CI0103
            13            WJ41-NSEQ3P PICTURE  S9(5)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-XZ6A   PICTURE  X(6).                    CI0103
            13            WJ41-XZ7    PICTURE  X(7).                    CI0103
            13            WJ41-XZ6B   PICTURE  X(6).                    CI0103
            13            WJ41-XZ6    PICTURE  X(6).                    CI0103
            13            WJ41-XZ6C   PICTURE  X(6).                    CI0103
            13            WJ41-XZ20   PICTURE  X(20).                   CI0103
            13            WJ41-CATRN1 PICTURE  9(6).                    CI0103
            13            WJ41-ADDAC2 PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-ATAXT2 PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-ACOMOT PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-XZ5    PICTURE  X(5).                    CI0103
            13            WJ41-IREVD  PICTURE  X(1).                    CI0103
            13            WJ41-ISUSP1 PICTURE  X(1).                    CI0103
            13            WJ41-XZ6D   PICTURE  X(6).                    CI0103
            13            WJ41-XZ13   PICTURE  X(13).                   CI0103
            13            WJ41-CWHTP2 PICTURE  X(3).                    CI0103
            13            WJ41-CWHTP3 PICTURE  X(3).                    CI0103
            13            WJ41-DTREN  PICTURE  9(8).                    CI0103
            13            WJ41-NAASQ1 PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            WJ41-GD51                                     CI0103
                          REDEFINES            WJ41-GD99.               CI0103
            13            WJ41-ADOMOT PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-ACGLT  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-ACGST  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CTXMT  PICTURE  9(2).                    CI0103
            13            WJ41-ALOAD3 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-FILLER PICTURE  X(31).                   CI0103
            12            WJ41-GD52                                     CI0103
                          REDEFINES            WJ41-GD99.               CI0103
            13            WJ41-DEFFT5 PICTURE  9(8).                    CI0103
            13            WJ41-PSLLD5 PICTURE  S99V999                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CSUSL2 PICTURE  S9.                      CI0103
            13            WJ41-ALOAD2 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-DL22.                                    CI0103
            14            WJ41-NYEAR1 PICTURE  9(4).                    CI0103
            14            WJ41-GEMTHA PICTURE  99.                      CI0103
            14            WJ41-NDAY01 PICTURE  99.                      CI0103
            13            WJ41-NSEQ3R PICTURE  S9(5)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CWHTP  PICTURE  X(3).                    CI0103
            13            WJ41-CWHFR  PICTURE  X(3).                    CI0103
            13            WJ41-CATRN7 PICTURE  9(6).                    CI0103
            13            WJ41-ATAXT5 PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-QSHOT  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-ACINT3 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CWHTP1 PICTURE  X(3).                    CI0103
            13            WJ41-CWHFR1 PICTURE  X(3).                    CI0103
            13            WJ41-ACOMO5 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-QSHOMU PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-ACASH1 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-FILLER PICTURE  X(04).                   CI0103
            13            WJ41-CATRN8 PICTURE  9(6).                    CI0103
            13            WJ41-ALOAD1 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-PSLLD1 PICTURE  S99V999                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-QSHOT1 PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-ACINT4 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CSUSL4 PICTURE  S9.                      CI0103
            13            WJ41-ACOMO4 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            WJ41-GD60                                     CI0103
                          REDEFINES            WJ41-GD99.               CI0103
            13            WJ41-GEOPDD PICTURE  X(8)                     CI0103
                          OCCURS       005     TIMES.                   CI0103
            13            WJ41-DLAUP3 PICTURE  9(8)                     CI0103
                          OCCURS       005     TIMES.                   CI0103
            13            WJ41-GEOPDB PICTURE  X(8).                    CI0103
            13            WJ41-DLAUP4 PICTURE  9(8).                    CI0103
            13            WJ41-ITELR2 PICTURE  X.                       CI0103
            13            WJ41-IPMTA  PICTURE  X.                       CI0103
            13            WJ41-CCSMG  PICTURE  X.                       CI0103
            13            WJ41-CPLEC  PICTURE  XX.                      CI0103
            13            WJ41-CORTYA PICTURE  X(3).                    CI0103
            13            WJ41-CACTBC PICTURE  X(1).                    CI0103
            13            WJ41-CGSPIA PICTURE  X.                       CI0103
            13            WJ41-IPTRDA PICTURE  X(01).                   CI0103
            13            WJ41-GCUSPY PICTURE  X(12).                   CI0103
            13            WJ41-CPALLA PICTURE  X(1).                    CI0103
            13            WJ41-QSHO5A PICTURE  S9(9)V999                CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-IFRSAB PICTURE  X.                       CI0103
            13            WJ41-DELOI  PICTURE  9(8).                    CI0103
            13            WJ41-IAROAA PICTURE  X.                       CI0103
            13            WJ41-ACINVR PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-ILTINA PICTURE  X.                       CI0103
            13            WJ41-ALOIDA PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CFUNTA PICTURE  X(2).                    CI0103
            13            WJ41-CLGND  PICTURE  X.                       CI0103
            13            WJ41-CPH3U  PICTURE  X.                       CI0103
            13            WJ41-GESTD  PICTURE  9(8).                    CI0103
            13            WJ41-GEEND  PICTURE  9(8).                    CI0103
            13            WJ41-CPMTF  PICTURE  99.                      CI0103
            13            WJ41-CNAVR  PICTURE  X(1).                    CI0103
            12            WJ41-GD70                                     CI0103
                          REDEFINES            WJ41-GD99.               CI0103
            13            WJ41-CMEMO  PICTURE  X(2).                    CI0103
            13            WJ41-ALPLDT PICTURE  9(8).                    CI0103
            13            WJ41-CTLPD  PICTURE  9(8).                    CI0103
            13            WJ41-CPAYCM PICTURE  X(2).                    CI0103
            11            WJ41-GC06.                                    CI0103
            12            WJ41-GELL   PICTURE  9(4)                     CI0103
                          BINARY.                                       CI0103
            12            WJ41-GE00.                                    CI0103
            13            WJ41-GC06K.                                   CI0103
            14            WJ41-NPISQ  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-ACOTD  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-PPOTD  PICTURE  S9(3)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-QPSTD  PICTURE  S9(7)V999                CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CPITC  PICTURE  99.                      CI0103
            13            WJ41-ITRNB  PICTURE  X.                       CI0103
            13            WJ41-FILLER PICTURE  X(14).                   CI0103
            12            WJ41-GE98.                                    CI0103
            13            WJ41-FILLER PICTURE  X(240).                  CI0103
            12            WJ41-GE10                                     CI0103
                          REDEFINES            WJ41-GE98.               CI0103
            13            WJ41-CDELI  PICTURE  9(3).                    CI0103
            13            WJ41-CPAYC  PICTURE  X(2).                    CI0103
            13            WJ41-ICHKP  PICTURE  X.                       CI0103
            13            WJ41-CLTIN  PICTURE  9(12).                   CI0103
            13            WJ41-IFHAI  PICTURE  X.                       CI0103
            13            WJ41-CDQUA  PICTURE  X(2).                    CI0103
            13            WJ41-FILLER PICTURE  X(07).                   CI0103
            13            WJ41-GE99.                                    CI0103
            14            WJ41-FILLER PICTURE  X(212).                  CI0103
            13            WJ41-GE01                                     CI0103
                          REDEFINES            WJ41-GE99.               CI0103
            14            WJ41-NTR    PICTURE  9(8).                    CI0103
            14            WJ41-GECKD  PICTURE  9.                       CI0103
            14            WJ41-NPBN   PICTURE  X(20).                   CI0103
            14            WJ41-CCBAT  PICTURE  99.                      CI0103
            14            WJ41-CLID4  PICTURE  X(23).                   CI0103
            14            WJ41-GENAL1 PICTURE  X(30)                    CI0103
                          OCCURS       002     TIMES.                   CI0103
            14            WJ41-GESAD1 PICTURE  X(30)                    CI0103
                          OCCURS       003     TIMES.                   CI0103
            13            WJ41-GE02                                     CI0103
                          REDEFINES            WJ41-GE99.               CI0103
            14            WJ41-GENAL  PICTURE  X(30)                    CI0103
                          OCCURS       002     TIMES.                   CI0103
            14            WJ41-GESAD  PICTURE  X(30)                    CI0103
                          OCCURS       003     TIMES.                   CI0103
            13            WJ41-GE03                                     CI0103
                          REDEFINES            WJ41-GE99.               CI0103
            14            WJ41-NCHKN  PICTURE  9(11).                   CI0103
            13            WJ41-GE04                                     CI0103
                          REDEFINES            WJ41-GE99.               CI0103
            14            WJ41-CTIDAP PICTURE  9(3).                    CI0103
            14            WJ41-PRCOD  PICTURE  9(5).                    CI0103
            14            WJ41-TDELI  PICTURE  X(30).                   CI0103
            14            WJ41-CINCD  PICTURE  9(02).                   CI0103
            12            WJ41-GE20                                     CI0103
                          REDEFINES            WJ41-GE98.               CI0103
            13            WJ41-C299.                                    CI0103
            14            WJ41-CTID.                                    CI0103
            15            WJ41-CTIDA  PICTURE  9(3).                    CI0103
            15            WJ41-CTIDN.                                   CI0103
            16            WJ41-CTIDNP PICTURE  X(13).                   CI0103
            16            WJ41-CTIDND PICTURE  9(11).                   CI0103
            13            WJ41-DCACG9 PICTURE  9(8).                    CI0103
            13            WJ41-NAASQ  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            WJ41-CIRAP  PICTURE  XX.                      CI0103
            13            WJ41-CTYPE  PICTURE  X.                       CI0103
            13            WJ41-INACT  PICTURE  X.                       CI0103
            13            WJ41-FILLER PICTURE  X(01).                   CI0103
            13            WJ41-ITPAC  PICTURE  X.                       CI0103
            13            WJ41-ITAXI  PICTURE  X.                       CI0103
            13            WJ41-IOWNC  PICTURE  X.                       CI0103
            13            WJ41-CDVCD  PICTURE  X(2).                    CI0103
            13            WJ41-CTCUS  PICTURE  999.                     CI0103
            13            WJ41-CPMTCB PICTURE  X(3).                    CI0103
            13            WJ41-CASTC1 PICTURE  99.                      CI0103
            13            WJ41-PRCOD1 PICTURE  9(5).                    CI0103
            13            WJ41-CPRSC1 PICTURE  X(9).                    CI0103
            13            WJ41-CPRTB  PICTURE  X.                       CI0103
            13            WJ41-CBRKD  PICTURE  9(4).                    CI0103
            13            WJ41-FILLER PICTURE  X(12).                   CI0103
            12            WJ41-GE30                                     CI0103
                          REDEFINES            WJ41-GE98.               CI0103
            13            WJ41-CFIDC  PICTURE  X(5).                    CI0103
            13            WJ41-CPHSE  PICTURE  9(2).                    CI0103
            13            WJ41-FILLER PICTURE  X(05).                   CI0103
            13            WJ41-IABIN  PICTURE  X.                       CI0103
            13            WJ41-PDFND  PICTURE  S999V9(3)                CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            WJ41-GE40                                     CI0103
                          REDEFINES            WJ41-GE98.               CI0103
            13            WJ41-CACCT  PICTURE  X.                       CI0103
            13            WJ41-CPAYR  PICTURE  X(2).                    CI0103
            13            WJ41-CDELI1 PICTURE  9(3).                    CI0103
            13            WJ41-CATRN.                                   CI0103
            14            WJ41-CATRF  PICTURE  9(3).                    CI0103
            14            WJ41-CATRS  PICTURE  9(3).                    CI0103
            13            WJ41-DEFFT  PICTURE  9(8).                    CI0103
            13            WJ41-CTYPC  PICTURE  X.                       CI0103
            13            WJ41-CIRAPA PICTURE  XX.                      CI0103
            13            WJ41-FILLER PICTURE  X(09).                   CI0103
            13            WJ41-GE49.                                    CI0103
            14            WJ41-FILLER PICTURE  X(208).                  CI0103
            13            WJ41-GE41                                     CI0103
                          REDEFINES            WJ41-GE49.               CI0103
            14            WJ41-NCHKN1 PICTURE  9(6).                    CI0103
            13            WJ41-GE42                                     CI0103
                          REDEFINES            WJ41-GE49.               CI0103
            14            WJ41-CTID1.                                   CI0103
            15            WJ41-CTIDA1 PICTURE  9(3).                    CI0103
            15            WJ41-CTIDP1 PICTURE  X(13).                   CI0103
            15            WJ41-CTIDN1 PICTURE  9(11).                   CI0103
            13            WJ41-GE43                                     CI0103
                          REDEFINES            WJ41-GE49.               CI0103
            14            WJ41-GENAL2 PICTURE  X(30)                    CI0103
                          OCCURS       002     TIMES.                   CI0103
            14            WJ41-GESAD2 PICTURE  X(30)                    CI0103
                          OCCURS       003     TIMES.                   CI0103
            13            WJ41-GE44                                     CI0103
                          REDEFINES            WJ41-GE49.               CI0103
            14            WJ41-CTID01.                                  CI0103
            15            WJ41-CTIDA6 PICTURE  9(3).                    CI0103
            15            WJ41-NTIDP2 PICTURE  X(13).                   CI0103
            15            WJ41-CTIDN2 PICTURE  9(11).                   CI0103
            14            WJ41-GECKD2 PICTURE  9.                       CI0103
            14            WJ41-PACCT  PICTURE  S999V99                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-PLOAN  PICTURE  S999V99                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-PADPT  PICTURE  S999V99                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            14            WJ41-IPCTL  PICTURE  X.                       CI0103
            14            WJ41-IPCTP  PICTURE  X.                       CI0103
            14            WJ41-CEUNT  PICTURE  S9(5)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            WJ41-GE31                                     CI0103
                          REDEFINES            WJ41-GE98.               CI0103
            13            WJ41-GCUSPZ PICTURE  X(12).                   CI0103
            11            WJ41-GC12                                     CI0103
                          REDEFINES            WJ41-GC06.               CI0103
            12            WJ41-GC12K.                                   CI0103
            13            WJ41-CIRAP  PICTURE  XX.                      CI0103
            12            WJ41-AIRCT  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            WJ41-FILLER PICTURE  X.                       CI0103
            11            WJ41-GC04.                                    CI0103
            12            WJ41-CLCUS  PICTURE  99.                      CI0103
            12            WJ41-CCACT  PICTURE  99.                      CI0103
            12            WJ41-AFEET  PICTURE  S9(5)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            WJ41-ITERF  PICTURE  X.                       CI0103
            12            WJ41-ATERF  PICTURE  S9(5)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            WJ41-CLDOB  PICTURE  9(8).                    CI0103
            12            WJ41-CPLTYP PICTURE  X(14).                   CI0103
            12            WJ41-IACFPD PICTURE  X(1).                    CI0103
            12            WJ41-FILLER PICTURE  X(14).                   CI0103
            11            WJ41-GC21                                     CI0103
                          REDEFINES            WJ41-GC04.               CI0103
            12            WJ41-C299.                                    CI0103
            13            WJ41-CTID.                                    CI0103
            14            WJ41-CTIDA  PICTURE  9(3).                    CI0103
            14            WJ41-CTIDN.                                   CI0103
            15            WJ41-CTIDNP PICTURE  X(13).                   CI0103
            15            WJ41-CTIDND PICTURE  9(11).                   CI0103
            12            WJ41-DCACG9 PICTURE  9(8).                    CI0103
            12            WJ41-NAASQ  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            WJ41-FILLER PICTURE  X.                       CI0103
      *!WF DSP=WJ DSL=PJ SEL=41 FOR=I DES=1 LEV=1 PLT=CI
      **
      ******************************************************************
      ** SEGMENTS TO MOVE CI0100 RETURN FIELDS INTO
      ******************************************************************
      *!WF DSP=GC DSL=GC SEL=01 FOR=I DES=1 LEV=1 PLT=CI
       01                 GC01.                                         CI0103
            10            GC01-GC01K.                                   CI0103
            11            GC01-C299.                                    CI0103
            12            GC01-CTID.                                    CI0103
            13            GC01-CTIDA  PICTURE  9(3).                    CI0103
            13            GC01-CTIDN.                                   CI0103
            14            GC01-CTIDNP PICTURE  X(13).                   CI0103
            14            GC01-CTIDND PICTURE  9(11).                   CI0103
            10            GC01-DCAG9L PICTURE  9(8).                    CI0103
            10            GC01-NAASQL PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GC01-ICUST  PICTURE  X.                       CI0103
            10            GC01-NSEQ4B PICTURE  9(8)                     CI0103
                          BINARY.                                       CI0103
            10            GC01-PRCOD  PICTURE  9(5).                    CI0103
            10            GC01-PRSCD  PICTURE  X(9).                    CI0103
            10            GC01-FILLER PICTURE  X(8).                    CI0103
       01                 GC03.                                         CI0103
            10            GC03-GELL   PICTURE  9(4)                     CI0103
                          BINARY.                                       CI0103
            10            GC03-GD00.                                    CI0103
            11            GC03-GC03K.                                   CI0103
            12            GC03-DCACG9 PICTURE  9(8).                    CI0103
            12            GC03-NAASQ  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-CAATY  PICTURE  9(3).                    CI0103
            11            GC03-CVSYS  PICTURE  X(2).                    CI0103
            11            GC03-CACTO  PICTURE  9(3).                    CI0103
            11            GC03-CATRN.                                   CI0103
            12            GC03-CATRF  PICTURE  9(3).                    CI0103
            12            GC03-CATRS  PICTURE  9(3).                    CI0103
            11            GC03-CASTC  PICTURE  99.                      CI0103
            11            GC03-IPULL  PICTURE  X.                       CI0103
            11            GC03-GEAUN  PICTURE  9(5).                    CI0103
            11            GC03-GEOPD2 PICTURE  X(8).                    CI0103
            11            GC03-NBTCH  PICTURE  9(4).                    CI0103
            11            GC03-DEFFT  PICTURE  9(8).                    CI0103
            11            GC03-NSUNT  PICTURE  9(4).                    CI0103
            11            GC03-ITRAN  PICTURE  X.                       CI0103
            11            GC03-DLAUP1 PICTURE  9(8).                    CI0103
            11            GC03-ADRET  PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-TTRMS  PICTURE  X(12).                   CI0103
            11            GC03-IDELT  PICTURE  X.                       CI0103
            11            GC03-GEOPDM PICTURE  X(8).                    CI0103
            11            GC03-FILLER PICTURE  X(07).                   CI0103
            10            GC03-GD09.                                    CI0103
            11            GC03-FILLER PICTURE  X(70).                   CI0103
            10            GC03-GD01                                     CI0103
                          REDEFINES            GC03-GD09.               CI0103
            11            GC03-ADBRQ  PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-CTRTP  PICTURE  X(2).                    CI0103
            11            GC03-CPORT  PICTURE  X.                       CI0103
            11            GC03-CSCRNU PICTURE  X(4).                    CI0103
            11            GC03-DLAUP  PICTURE  9(8).                    CI0103
            11            GC03-CTWHAT PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-PWHLD  PICTURE  S999V9(5)                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-IWTHH  PICTURE  X.                       CI0103
            11            GC03-NDRFT  PICTURE  9(5).                    CI0103
            11            GC03-IDPAP  PICTURE  X.                       CI0103
            11            GC03-GETIM  PICTURE  S9(7)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-QNACT  PICTURE  9(3).                    CI0103
            11            GC03-AEDRQ  PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-IPLIN  PICTURE  X.                       CI0103
            11            GC03-CLIDNB PICTURE  9(8).                    CI0103
            11            GC03-CSLCT  PICTURE  X.                       CI0103
            11            GC03-ITELE  PICTURE  X.                       CI0103
            11            GC03-FILLER PICTURE  X(06).                   CI0103
            10            GC03-GD02                                     CI0103
                          REDEFINES            GC03-GD09.               CI0103
            11            GC03-CSYST  PICTURE  99.                      CI0103
            11            GC03-FILLER PICTURE  X.                       CI0103
            11            GC03-ACASH  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-DTRAC  PICTURE  9(8).                    CI0103
            11            GC03-CTRSO  PICTURE  9(02).                   CI0103
            11            GC03-NTRCE  PICTURE  9(06).                   CI0103
            11            GC03-GECKD1 PICTURE  9.                       CI0103
            11            GC03-CCOLL  PICTURE  X(3).                    CI0103
            11            GC03-CLTDP  PICTURE  X(3).                    CI0103
            11            GC03-PSLLD  PICTURE  S99V999                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-ISLOR  PICTURE  X.                       CI0103
            11            GC03-ITPAC  PICTURE  X.                       CI0103
            11            GC03-CPMTCA PICTURE  XXX.                     CI0103
            11            GC03-CSERV  PICTURE  X(3).                    CI0103
            11            GC03-ACOMO  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-IPLIN1 PICTURE  X.                       CI0103
            11            GC03-INQEX  PICTURE  X.                       CI0103
            11            GC03-CTKRAA PICTURE  X(12).                   CI0103
            11            GC03-CCSMQ  PICTURE  X.                       CI0103
            11            GC03-IVAEX1 PICTURE  X.                       CI0103
            11            GC03-IHPMT  PICTURE  X(1).                    CI0103
            11            GC03-GETIM3 PICTURE  S9(7)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GC03-GD03                                     CI0103
                          REDEFINES            GC03-GD09.               CI0103
            11            GC03-CATRNC PICTURE  9(6).                    CI0103
            11            GC03-APRNT1 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-QSHOWT PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-ACINVT PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-ACOMO7 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-QSHOMW PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-ATAXT3 PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-CTSTR  PICTURE  9(2).                    CI0103
            11            GC03-ICIRA  PICTURE  X.                       CI0103
            11            GC03-GETIM2 PICTURE  S9(7)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-CPMTCX PICTURE  XX.                      CI0103
            11            GC03-FILLER PICTURE  X(16).                   CI0103
            10            GC03-GD99.                                    CI0103
            11            GC03-FILLER PICTURE  X(248).                  CI0103
            10            GC03-GD10                                     CI0103
                          REDEFINES            GC03-GD99.               CI0103
            11            GC03-MROTC  PICTURE  X(7).                    CI0103
            11            GC03-CEDSC  PICTURE  9(1).                    CI0103
            11            GC03-ILPOI  PICTURE  X(1).                    CI0103
            11            GC03-AWRCH  PICTURE  S9(3)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-CHCOC1 PICTURE  9(2).                    CI0103
            11            GC03-CHCOC2 PICTURE  9(2).                    CI0103
            11            GC03-CHCOC3 PICTURE  9(2).                    CI0103
            11            GC03-CHCOC4 PICTURE  9(2).                    CI0103
            11            GC03-CMCOC1 PICTURE  9(3).                    CI0103
            11            GC03-CMCOC2 PICTURE  9(3).                    CI0103
            11            GC03-CMCOC3 PICTURE  9(3).                    CI0103
            11            GC03-GD11.                                    CI0103
            12            GC03-FILLER PICTURE  X(219).                  CI0103
            11            GC03-GD12                                     CI0103
                          REDEFINES            GC03-GD11.               CI0103
            12            GC03-CELLO  PICTURE  9(1).                    CI0103
            12            GC03-CECLO  PICTURE  9(1).                    CI0103
            12            GC03-AEXML  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-CEPI   PICTURE  X(1).                    CI0103
            12            GC03-CEXTY  PICTURE  X.                       CI0103
            12            GC03-CROPC  PICTURE  9(1).                    CI0103
            12            GC03-CPUTY  PICTURE  9(1).                    CI0103
            12            GC03-IMCII  PICTURE  X(1).                    CI0103
            12            GC03-GEMISC                                   CI0103
                          OCCURS       010     TIMES.                   CI0103
            13            GC03-AMGLA  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            GC03-CMGLC  PICTURE  9(1).                    CI0103
            13            GC03-NMGLN  PICTURE  9(4).                    CI0103
            12            GC03-ACTRN  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-IWRBK  PICTURE  X.                       CI0103
            12            GC03-IFEDX  PICTURE  X.                       CI0103
            12            GC03-ICNTR  PICTURE  X.                       CI0103
            12            GC03-IOCKH  PICTURE  X.                       CI0103
            12            GC03-ICRCK  PICTURE  X.                       CI0103
            12            GC03-NHMPN  PICTURE  S9(10)                   CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-ITELR1 PICTURE  X.                       CI0103
            11            GC03-GD13                                     CI0103
                          REDEFINES            GC03-GD11.               CI0103
            12            GC03-DREDO  PICTURE  9(8).                    CI0103
            12            GC03-CATRNR PICTURE  9(6).                    CI0103
            12            GC03-CEVN   PICTURE  9(9).                    CI0103
            12            GC03-ISUSP  PICTURE  X(1).                    CI0103
            11            GC03-GD15                                     CI0103
                          REDEFINES            GC03-GD11.               CI0103
            12            GC03-CPUTZ  PICTURE  9(1).                    CI0103
            12            GC03-CETLB  PICTURE  9(3).                    CI0103
            12            GC03-QTRMC  PICTURE  9(3).                    CI0103
            12            GC03-DEFFTE PICTURE  9(8).                    CI0103
            12            GC03-DEFFTF PICTURE  9(8).                    CI0103
            12            GC03-DEFFTG PICTURE  9(8).                    CI0103
            12            GC03-XZ1A   PICTURE  X.                       CI0103
            12            GC03-XZ1B   PICTURE  X.                       CI0103
            12            GC03-XZ1C   PICTURE  X.                       CI0103
            12            GC03-XZ1D   PICTURE  X.                       CI0103
            12            GC03-XZ1E   PICTURE  X.                       CI0103
            12            GC03-XZ1F   PICTURE  X.                       CI0103
            12            GC03-XZ1G   PICTURE  X.                       CI0103
            12            GC03-XZ1H   PICTURE  X.                       CI0103
            12            GC03-XZ1I   PICTURE  X.                       CI0103
            12            GC03-DEFFTH PICTURE  9(8).                    CI0103
            11            GC03-GD19                                     CI0103
                          REDEFINES            GC03-GD11.               CI0103
            12            GC03-GD11.                                    CI0103
            13            GC03-FILLER PICTURE  X(219).                  CI0103
            10            GC03-GD20                                     CI0103
                          REDEFINES            GC03-GD99.               CI0103
            11            GC03-ADDACT PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-ISIGV  PICTURE  X.                       CI0103
            11            GC03-IALLF  PICTURE  X.                       CI0103
            11            GC03-QSHOWQ PICTURE  S9(9)V999                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-CCDSCW PICTURE  9(2).                    CI0103
            11            GC03-IDWRL  PICTURE  X.                       CI0103
            11            GC03-ITELR  PICTURE  X.                       CI0103
            11            GC03-IABIN  PICTURE  X.                       CI0103
            11            GC03-PACT1  PICTURE  S999V999                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-IBFAF  PICTURE  X.                       CI0103
            11            GC03-IFRSA  PICTURE  X.                       CI0103
            11            GC03-ICRCAN PICTURE  X.                       CI0103
            11            GC03-ACACTV PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-AGFND  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-QCSHOW PICTURE  S9(9)V999                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-QCSHIS PICTURE  S9(9)V999                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-NDTRC  PICTURE  9(8).                    CI0103
            11            GC03-CAERU  PICTURE  X(4).                    CI0103
            11            GC03-IFDGO  PICTURE  X.                       CI0103
            11            GC03-PSLLD2 PICTURE  S99V999                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-ISLOR2 PICTURE  X.                       CI0103
            11            GC03-QSFIO  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-QSFID  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-CGDIN  PICTURE  X.                       CI0103
            11            GC03-DGDIN  PICTURE  9(8).                    CI0103
            10            GC03-GD30                                     CI0103
                          REDEFINES            GC03-GD99.               CI0103
            11            GC03-ISKED  PICTURE  X.                       CI0103
            11            GC03-CENXC  PICTURE  9(2).                    CI0103
            11            GC03-GD31.                                    CI0103
            12            GC03-FILLER PICTURE  X(245).                  CI0103
            11            GC03-GD32                                     CI0103
                          REDEFINES            GC03-GD31.               CI0103
            12            GC03-IABIN1 PICTURE  X.                       CI0103
            12            GC03-CLDOD  PICTURE  9(8).                    CI0103
            12            GC03-NCLAM  PICTURE  9(5)                     CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-ISURR  PICTURE  X.                       CI0103
            12            GC03-GEHCD  PICTURE  9(3).                    CI0103
            12            GC03-CRATC  PICTURE  9(4).                    CI0103
            12            GC03-AMAXD  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-ASCHGA PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-APYOM  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-IWTHH1 PICTURE  X.                       CI0103
            12            GC03-CPAYCL PICTURE  X(2).                    CI0103
            12            GC03-CTSAO  PICTURE  X.                       CI0103
            12            GC03-NCONF  PICTURE  9(08).                   CI0103
            12            GC03-CLID   PICTURE  X(23).                   CI0103
            12            GC03-CARTY  PICTURE  99.                      CI0103
            12            GC03-NARRS  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-CARTZ  PICTURE  99.                      CI0103
            12            GC03-NAPDS  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-CPMTO  PICTURE  X.                       CI0103
            12            GC03-DNPMT  PICTURE  9(8).                    CI0103
            12            GC03-IPCTV  PICTURE  X.                       CI0103
            12            GC03-IMECH  PICTURE  X(01).                   CI0103
            12            GC03-IMVAO  PICTURE  X(1).                    CI0103
            12            GC03-AMVA1  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-CACTS  PICTURE  X.                       CI0103
            12            GC03-CTSPP  PICTURE  X(1).                    CI0103
            12            GC03-CACT4  PICTURE  X(2).                    CI0103
            12            GC03-IVAEX  PICTURE  X.                       CI0103
            12            GC03-DFPMT  PICTURE  9(8).                    CI0103
            12            GC03-IDEMD  PICTURE  X.                       CI0103
            12            GC03-IOFST  PICTURE  X.                       CI0103
            12            GC03-AMXLB  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-ACULB  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-DEIRNB PICTURE  9(8).                    CI0103
            12            GC03-DEFFE  PICTURE  9(8).                    CI0103
            12            GC03-DEFFR  PICTURE  9(8).                    CI0103
            12            GC03-ISPUP  PICTURE  X.                       CI0103
            12            GC03-CPNCG  PICTURE  X.                       CI0103
            12            GC03-IEXPU  PICTURE  X.                       CI0103
            12            GC03-IPPCF  PICTURE  X.                       CI0103
            12            GC03-NAAPT  PICTURE  9(2).                    CI0103
            12            GC03-PWHLDS PICTURE  S999V9(5)                CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-ISWHO  PICTURE  X(1).                    CI0103
            11            GC03-GD33                                     CI0103
                          REDEFINES            GC03-GD31.               CI0103
            12            GC03-CPAYC  PICTURE  X(2).                    CI0103
            12            GC03-ADBRQX PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-ADBRQV PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-APTXR  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-CTRTPE PICTURE  X(2).                    CI0103
            12            GC03-NCLAMI PICTURE  S9(9)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-CLIDO8 PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-CLIDN  PICTURE  X(20).                   CI0103
            12            GC03-DSET01 PICTURE  S9(8)                    CI0103
                          BINARY.                                       CI0103
            12            GC03-CTSET1 PICTURE  S9(6)                    CI0103
                          BINARY.                                       CI0103
            12            GC03-DSET02 PICTURE  S9(8)                    CI0103
                          BINARY.                                       CI0103
            12            GC03-CTSET2 PICTURE  S9(6)                    CI0103
                          BINARY.                                       CI0103
            11            GC03-GD34                                     CI0103
                          REDEFINES            GC03-GD31.               CI0103
            12            GC03-QNOFM  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-CLTRM  PICTURE  99.                      CI0103
            12            GC03-AMXLN  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-ALADJ  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-ACHK   PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-APRMO  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-IMECH1 PICTURE  X(01).                   CI0103
            12            GC03-CACT41 PICTURE  X(2).                    CI0103
            12            GC03-ACDSCC PICTURE  S9(05)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-ACDSCD PICTURE  S9(05)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-GD39                                     CI0103
                          REDEFINES            GC03-GD31.               CI0103
            12            GC03-GD31.                                    CI0103
            13            GC03-FILLER PICTURE  X(245).                  CI0103
            10            GC03-GD40                                     CI0103
                          REDEFINES            GC03-GD99.               CI0103
            11            GC03-NTR    PICTURE  9(8).                    CI0103
            11            GC03-NPBNC  PICTURE  X(24).                   CI0103
            11            GC03-CRREV  PICTURE  X(3).                    CI0103
            11            GC03-CSUSL  PICTURE  S9.                      CI0103
            11            GC03-NMGLN1 PICTURE  9(4).                    CI0103
            11            GC03-DCAC92 PICTURE  9(8).                    CI0103
            11            GC03-NAASQ3 PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-GD49.                                    CI0103
            12            GC03-FILLER PICTURE  X(198).                  CI0103
            11            GC03-GD41                                     CI0103
                          REDEFINES            GC03-GD49.               CI0103
            12            GC03-CRREF  PICTURE  9(2).                    CI0103
            12            GC03-CORIR  PICTURE  X(02).                   CI0103
            12            GC03-CIPDB  PICTURE  X(03).                   CI0103
            12            GC03-CPAYH  PICTURE  X(02).                   CI0103
            12            GC03-NAMEX  PICTURE  9(15)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC03-DCHAE  PICTURE  9(4).                    CI0103
            12            GC03-DRQST  PICTURE  S9(8)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-GD42                                     CI0103
                          REDEFINES            GC03-GD49.               CI0103
            12            GC03-CPMTCB PICTURE  X(3).                    CI0103
            10            GC03-GD50                                     CI0103
                          REDEFINES            GC03-GD99.               CI0103
            11            GC03-ALOAD  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-PSLLD4 PICTURE  S99V999                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-CSUSL1 PICTURE  S9.                      CI0103
            11            GC03-CRREV1 PICTURE  X(3).                    CI0103
            11            GC03-ADDAC  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-DL13.                                    CI0103
            12            GC03-GEYR   PICTURE  9(4).                    CI0103
            12            GC03-GEMTH  PICTURE  99.                      CI0103
            12            GC03-NDAY   PICTURE  99.                      CI0103
            11            GC03-NSEQ3P PICTURE  S9(5)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-XZ6A   PICTURE  X(6).                    CI0103
            11            GC03-XZ7    PICTURE  X(7).                    CI0103
            11            GC03-XZ6B   PICTURE  X(6).                    CI0103
            11            GC03-XZ6    PICTURE  X(6).                    CI0103
            11            GC03-XZ6C   PICTURE  X(6).                    CI0103
            11            GC03-XZ20   PICTURE  X(20).                   CI0103
            11            GC03-CATRN1 PICTURE  9(6).                    CI0103
            11            GC03-ADDAC2 PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-ATAXT2 PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-ACOMOT PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-XZ5    PICTURE  X(5).                    CI0103
            11            GC03-IREVD  PICTURE  X(1).                    CI0103
            11            GC03-ISUSP1 PICTURE  X(1).                    CI0103
            11            GC03-XZ6D   PICTURE  X(6).                    CI0103
            11            GC03-XZ13   PICTURE  X(13).                   CI0103
            11            GC03-CWHTP2 PICTURE  X(3).                    CI0103
            11            GC03-CWHTP3 PICTURE  X(3).                    CI0103
            11            GC03-DTREN  PICTURE  9(8).                    CI0103
            11            GC03-NAASQ1 PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GC03-GD51                                     CI0103
                          REDEFINES            GC03-GD99.               CI0103
            11            GC03-ADOMOT PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-ACGLT  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-ACGST  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-CTXMT  PICTURE  9(2).                    CI0103
            11            GC03-ALOAD3 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-FILLER PICTURE  X(31).                   CI0103
            10            GC03-GD52                                     CI0103
                          REDEFINES            GC03-GD99.               CI0103
            11            GC03-DEFFT5 PICTURE  9(8).                    CI0103
            11            GC03-PSLLD5 PICTURE  S99V999                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-CSUSL2 PICTURE  S9.                      CI0103
            11            GC03-ALOAD2 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-DL22.                                    CI0103
            12            GC03-NYEAR1 PICTURE  9(4).                    CI0103
            12            GC03-GEMTHA PICTURE  99.                      CI0103
            12            GC03-NDAY01 PICTURE  99.                      CI0103
            11            GC03-NSEQ3R PICTURE  S9(5)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-CWHTP  PICTURE  X(3).                    CI0103
            11            GC03-CWHFR  PICTURE  X(3).                    CI0103
            11            GC03-CATRN7 PICTURE  9(6).                    CI0103
            11            GC03-ATAXT5 PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-QSHOT  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-ACINT3 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-CWHTP1 PICTURE  X(3).                    CI0103
            11            GC03-CWHFR1 PICTURE  X(3).                    CI0103
            11            GC03-ACOMO5 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-QSHOMU PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-ACASH1 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-FILLER PICTURE  X(04).                   CI0103
            11            GC03-CATRN8 PICTURE  9(6).                    CI0103
            11            GC03-ALOAD1 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-PSLLD1 PICTURE  S99V999                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-QSHOT1 PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-ACINT4 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-CSUSL4 PICTURE  S9.                      CI0103
            11            GC03-ACOMO4 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GC03-GD60                                     CI0103
                          REDEFINES            GC03-GD99.               CI0103
            11            GC03-GEOPDD PICTURE  X(8)                     CI0103
                          OCCURS       005     TIMES.                   CI0103
            11            GC03-DLAUP3 PICTURE  9(8)                     CI0103
                          OCCURS       005     TIMES.                   CI0103
            11            GC03-GEOPDB PICTURE  X(8).                    CI0103
            11            GC03-DLAUP4 PICTURE  9(8).                    CI0103
            11            GC03-ITELR2 PICTURE  X.                       CI0103
            11            GC03-IPMTA  PICTURE  X.                       CI0103
            11            GC03-CCSMG  PICTURE  X.                       CI0103
            11            GC03-CPLEC  PICTURE  XX.                      CI0103
            11            GC03-CORTYA PICTURE  X(3).                    CI0103
            11            GC03-CACTBC PICTURE  X(1).                    CI0103
            11            GC03-CGSPIA PICTURE  X.                       CI0103
            11            GC03-IPTRDA PICTURE  X(01).                   CI0103
            11            GC03-GCUSPY PICTURE  X(12).                   CI0103
            11            GC03-CPALLA PICTURE  X(1).                    CI0103
            11            GC03-QSHO5A PICTURE  S9(9)V999                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-IFRSAB PICTURE  X.                       CI0103
            11            GC03-DELOI  PICTURE  9(8).                    CI0103
            11            GC03-IAROAA PICTURE  X.                       CI0103
            11            GC03-ACINVR PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-ILTINA PICTURE  X.                       CI0103
            11            GC03-ALOIDA PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC03-CFUNTA PICTURE  X(2).                    CI0103
            11            GC03-CLGND  PICTURE  X.                       CI0103
            11            GC03-CPH3U  PICTURE  X.                       CI0103
            11            GC03-GESTD  PICTURE  9(8).                    CI0103
            11            GC03-GEEND  PICTURE  9(8).                    CI0103
            11            GC03-CPMTF  PICTURE  99.                      CI0103
            11            GC03-CNAVR  PICTURE  X(1).                    CI0103
            10            GC03-GD70                                     CI0103
                          REDEFINES            GC03-GD99.               CI0103
            11            GC03-CMEMO  PICTURE  X(2).                    CI0103
            11            GC03-ALPLDT PICTURE  9(8).                    CI0103
            11            GC03-CTLPD  PICTURE  9(8).                    CI0103
            11            GC03-CPAYCM PICTURE  X(2).                    CI0103
       01                 GC06.                                         CI0103
            10            GC06-GELL   PICTURE  9(4)                     CI0103
                          BINARY.                                       CI0103
            10            GC06-GE00.                                    CI0103
            11            GC06-GC06K.                                   CI0103
            12            GC06-NPISQ  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC06-ACOTD  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC06-PPOTD  PICTURE  S9(3)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC06-QPSTD  PICTURE  S9(7)V999                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC06-CPITC  PICTURE  99.                      CI0103
            11            GC06-ITRNB  PICTURE  X.                       CI0103
            11            GC06-FILLER PICTURE  X(14).                   CI0103
            10            GC06-GE98.                                    CI0103
            11            GC06-FILLER PICTURE  X(240).                  CI0103
            10            GC06-GE10                                     CI0103
                          REDEFINES            GC06-GE98.               CI0103
            11            GC06-CDELI  PICTURE  9(3).                    CI0103
            11            GC06-CPAYC  PICTURE  X(2).                    CI0103
            11            GC06-ICHKP  PICTURE  X.                       CI0103
            11            GC06-CLTIN  PICTURE  9(12).                   CI0103
            11            GC06-IFHAI  PICTURE  X.                       CI0103
            11            GC06-CDQUA  PICTURE  X(2).                    CI0103
            11            GC06-FILLER PICTURE  X(07).                   CI0103
            11            GC06-GE99.                                    CI0103
            12            GC06-FILLER PICTURE  X(212).                  CI0103
            11            GC06-GE01                                     CI0103
                          REDEFINES            GC06-GE99.               CI0103
            12            GC06-NTR    PICTURE  9(8).                    CI0103
            12            GC06-GECKD  PICTURE  9.                       CI0103
            12            GC06-NPBN   PICTURE  X(20).                   CI0103
            12            GC06-CCBAT  PICTURE  99.                      CI0103
            12            GC06-CLID4  PICTURE  X(23).                   CI0103
            12            GC06-GENAL1 PICTURE  X(30)                    CI0103
                          OCCURS       002     TIMES.                   CI0103
            12            GC06-GESAD1 PICTURE  X(30)                    CI0103
                          OCCURS       003     TIMES.                   CI0103
            11            GC06-GE02                                     CI0103
                          REDEFINES            GC06-GE99.               CI0103
            12            GC06-GENAL  PICTURE  X(30)                    CI0103
                          OCCURS       002     TIMES.                   CI0103
            12            GC06-GESAD  PICTURE  X(30)                    CI0103
                          OCCURS       003     TIMES.                   CI0103
            11            GC06-GE03                                     CI0103
                          REDEFINES            GC06-GE99.               CI0103
            12            GC06-NCHKN  PICTURE  9(11).                   CI0103
            11            GC06-GE04                                     CI0103
                          REDEFINES            GC06-GE99.               CI0103
            12            GC06-CTIDAP PICTURE  9(3).                    CI0103
            12            GC06-PRCOD  PICTURE  9(5).                    CI0103
            12            GC06-TDELI  PICTURE  X(30).                   CI0103
            12            GC06-CINCD  PICTURE  9(02).                   CI0103
            10            GC06-GE20                                     CI0103
                          REDEFINES            GC06-GE98.               CI0103
            11            GC06-C299.                                    CI0103
            12            GC06-CTID.                                    CI0103
            13            GC06-CTIDA  PICTURE  9(3).                    CI0103
            13            GC06-CTIDN.                                   CI0103
            14            GC06-CTIDNP PICTURE  X(13).                   CI0103
            14            GC06-CTIDND PICTURE  9(11).                   CI0103
            11            GC06-DCACG9 PICTURE  9(8).                    CI0103
            11            GC06-NAASQ  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GC06-CIRAP  PICTURE  XX.                      CI0103
            11            GC06-CTYPE  PICTURE  X.                       CI0103
            11            GC06-INACT  PICTURE  X.                       CI0103
            11            GC06-FILLER PICTURE  X(01).                   CI0103
            11            GC06-ITPAC  PICTURE  X.                       CI0103
            11            GC06-ITAXI  PICTURE  X.                       CI0103
            11            GC06-IOWNC  PICTURE  X.                       CI0103
            11            GC06-CDVCD  PICTURE  X(2).                    CI0103
            11            GC06-CTCUS  PICTURE  999.                     CI0103
            11            GC06-CPMTCB PICTURE  X(3).                    CI0103
            11            GC06-CASTC1 PICTURE  99.                      CI0103
            11            GC06-PRCOD1 PICTURE  9(5).                    CI0103
            11            GC06-CPRSC1 PICTURE  X(9).                    CI0103
            11            GC06-CPRTB  PICTURE  X.                       CI0103
            11            GC06-CBRKD  PICTURE  9(4).                    CI0103
            11            GC06-FILLER PICTURE  X(12).                   CI0103
            10            GC06-GE30                                     CI0103
                          REDEFINES            GC06-GE98.               CI0103
            11            GC06-CFIDC  PICTURE  X(5).                    CI0103
            11            GC06-CPHSE  PICTURE  9(2).                    CI0103
            11            GC06-FILLER PICTURE  X(05).                   CI0103
            11            GC06-IABIN  PICTURE  X.                       CI0103
            11            GC06-PDFND  PICTURE  S999V9(3)                CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GC06-GE40                                     CI0103
                          REDEFINES            GC06-GE98.               CI0103
            11            GC06-CACCT  PICTURE  X.                       CI0103
            11            GC06-CPAYR  PICTURE  X(2).                    CI0103
            11            GC06-CDELI1 PICTURE  9(3).                    CI0103
            11            GC06-CATRN.                                   CI0103
            12            GC06-CATRF  PICTURE  9(3).                    CI0103
            12            GC06-CATRS  PICTURE  9(3).                    CI0103
            11            GC06-DEFFT  PICTURE  9(8).                    CI0103
            11            GC06-CTYPC  PICTURE  X.                       CI0103
            11            GC06-CIRAPA PICTURE  XX.                      CI0103
            11            GC06-FILLER PICTURE  X(09).                   CI0103
            11            GC06-GE49.                                    CI0103
            12            GC06-FILLER PICTURE  X(208).                  CI0103
            11            GC06-GE41                                     CI0103
                          REDEFINES            GC06-GE49.               CI0103
            12            GC06-NCHKN1 PICTURE  9(6).                    CI0103
            11            GC06-GE42                                     CI0103
                          REDEFINES            GC06-GE49.               CI0103
            12            GC06-CTID1.                                   CI0103
            13            GC06-CTIDA1 PICTURE  9(3).                    CI0103
            13            GC06-CTIDP1 PICTURE  X(13).                   CI0103
            13            GC06-CTIDN1 PICTURE  9(11).                   CI0103
            11            GC06-GE43                                     CI0103
                          REDEFINES            GC06-GE49.               CI0103
            12            GC06-GENAL2 PICTURE  X(30)                    CI0103
                          OCCURS       002     TIMES.                   CI0103
            12            GC06-GESAD2 PICTURE  X(30)                    CI0103
                          OCCURS       003     TIMES.                   CI0103
            11            GC06-GE44                                     CI0103
                          REDEFINES            GC06-GE49.               CI0103
            12            GC06-CTID01.                                  CI0103
            13            GC06-CTIDA6 PICTURE  9(3).                    CI0103
            13            GC06-NTIDP2 PICTURE  X(13).                   CI0103
            13            GC06-CTIDN2 PICTURE  9(11).                   CI0103
            12            GC06-GECKD2 PICTURE  9.                       CI0103
            12            GC06-PACCT  PICTURE  S999V99                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC06-PLOAN  PICTURE  S999V99                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC06-PADPT  PICTURE  S999V99                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GC06-IPCTL  PICTURE  X.                       CI0103
            12            GC06-IPCTP  PICTURE  X.                       CI0103
            12            GC06-CEUNT  PICTURE  S9(5)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GC06-GE31                                     CI0103
                          REDEFINES            GC06-GE98.               CI0103
            11            GC06-GCUSPZ PICTURE  X(12).                   CI0103
       01                 GC21.                                         CI0103
            10            GC21-C299.                                    CI0103
            11            GC21-CTID.                                    CI0103
            12            GC21-CTIDA  PICTURE  9(3).                    CI0103
            12            GC21-CTIDN.                                   CI0103
            13            GC21-CTIDNP PICTURE  X(13).                   CI0103
            13            GC21-CTIDND PICTURE  9(11).                   CI0103
            10            GC21-DCACG9 PICTURE  9(8).                    CI0103
            10            GC21-NAASQ  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GC21-FILLER PICTURE  X.                       CI0103
      *!WF DSP=GC DSL=GC SEL=03 FOR=I DES=1 LEV=1 PLT=CI
      *!WF DSP=GC DSL=GC SEL=06 FOR=I DES=1 LEV=1 PLT=CI
      *!WF DSP=GC DSL=GC SEL=21 FOR=I DES=1 LEV=1 PLT=CI
      **
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0020           PIC X(8) VALUE 'CI0020P '.                  AM0020
       01  CI0033           PIC X(8) VALUE 'CI0033P '.                  AM0033
       01  CI0100           PIC X(8) VALUE 'CI0100P '.                  AM0100
       01                 CT00.                                         CI0103
          05              CT00-SUITE.                                   CI0103
            15       FILLER         PICTURE  X(00222).                  CI0103
       01                 CT01  REDEFINES      CT00.                    CI0103
            10            CT01-CT01K.                                   CI0103
            11            CT01-C299.                                    CI0103
            12            CT01-CTID.                                    CI0103
            13            CT01-CTIDA  PICTURE  9(3).                    CI0103
            13            CT01-CTIDN.                                   CI0103
            14            CT01-CTIDNP PICTURE  X(13).                   CI0103
            14            CT01-CTIDND PICTURE  9(11).                   CI0103
            10            CT01-GECKD  PICTURE  9.                       CI0103
            10            CT01-GEMDA  PICTURE  9(8).                    CI0103
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0103
                          BINARY.                                       CI0103
            10            CT01-GECUC  PICTURE  99.                      CI0103
            10            CT01-CTAUL  PICTURE  9(3).                    CI0103
            10            CT01-DIRAC  PICTURE  9(4).                    CI0103
            10            CT01-CTCCI  PICTURE  X.                       CI0103
            10            CT01-CTCUS  PICTURE  999.                     CI0103
            10            CT01-CTEFD  PICTURE  9(8).                    CI0103
            10            CT01-CTIAD  PICTURE  9(8).                    CI0103
            10            CT01-CLCUS  PICTURE  99.                      CI0103
            10            CT01-CAMMB  PICTURE  X(3).                    CI0103
            10            CT01-CKPMM  PICTURE  X.                       CI0103
            10            CT01-CTLAD  PICTURE  9(8).                    CI0103
            10            CT01-IPERS  PICTURE  X.                       CI0103
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            CT01-CTLAT  PICTURE  9(8).                    CI0103
            10            CT01-CTLATC PICTURE  9(6).                    CI0103
            10            CT01-IMEGA  PICTURE  X.                       CI0103
            10            CT01-DIRAB  PICTURE  9(8).                    CI0103
            10            CT01-COLRQ  PICTURE  X.                       CI0103
            10            CT01-ZDA04  PICTURE  X(4).                    CI0103
            10            CT01-CTLPD  PICTURE  9(8).                    CI0103
            10            CT01-CIRASP PICTURE  9.                       CI0103
            10            CT01-CIRATP PICTURE  99.                      CI0103
            10            CT01-DRTHC  PICTURE  9(8).                    CI0103
            10            CT01-CPPTC  PICTURE  X.                       CI0103
            10            CT01-ZDA06  PICTURE  X(6).                    CI0103
            10            CT01-CTACD  PICTURE  9(8).                    CI0103
            10            CT01-CTNLI  PICTURE  X.                       CI0103
            10            CT01-CTRHO  PICTURE  9(8).                    CI0103
            10            CT01-CTSGD  PICTURE  9(8).                    CI0103
            10            CT01-CPATP  PICTURE  X(1).                    CI0103
            10            CT01-IRSTA  PICTURE  X.                       CI0103
            10            CT01-CTSTA  PICTURE  99.                      CI0103
            10            CT01-CTSSC  PICTURE  99.                      CI0103
            10            CT01-PRLIN  PICTURE  9(3).                    CI0103
            10            CT01-PRCOD  PICTURE  9(5).                    CI0103
            10            CT01-PRSCD  PICTURE  X(9).                    CI0103
            10            CT01-CTLNI  PICTURE  X.                       CI0103
            10            CT01-AYSIDA PICTURE  9(3).                    CI0103
            10            CT01-AYSID  PICTURE  9(5).                    CI0103
            10            CT01-CTBMC  PICTURE  99.                      CI0103
            10            CT01-CINAR  PICTURE  99.                      CI0103
            10            CT01-CPHTR  PICTURE  X.                       CI0103
            10            CT01-CDSTR  PICTURE  XX.                      CI0103
            10            CT01-CQACT  PICTURE  999.                     CI0103
            10            CT01-CIRAS  PICTURE  999.                     CI0103
            10            CT01-CIRAT  PICTURE  999.                     CI0103
            10            CT01-CLRAY  PICTURE  9(5).                    CI0103
            10            CT01-CATTP  PICTURE  X.                       CI0103
      ******************************************************            AADA82
      ****      WORK AREAS FOR COMMON DATE UTILITY       ***            AADA82
      ******************************************************            AADA82
      **                                                                AADA82
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA82
      **                                                                AADA82
      **   SEGMENT DD30 - FUNCTION LAYOUT                               AADA82
      **                                                                AADA82
      *!WF DSP=DD DSL=DD SEL=30 FOR=I DES=2 LEV=1                       AADA82
       01                 DD30.                                         CI0103
            10            DD30-CDTFN  PICTURE  9(4)                     CI0103
                          VALUE                ZERO.                    CI0103
            10            DD30-CDTSF  PICTURE  9(4)                     CI0103
                          VALUE                ZERO.                    CI0103
            10            DD30-CDTSC  PICTURE  9(4)                     CI0103
                          VALUE                ZERO.                    CI0103
            10            DD30-FILLER PICTURE  X(40)                    CI0103
                          VALUE                SPACE.                   CI0103
       01                 DD34.                                         CI0103
            10            DD34-CAINS  PICTURE  X(03)                    CI0103
                          VALUE                SPACE.                   CI0103
            10            DD34-CDTUC  PICTURE  9                        CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-NDTUN  PICTURE  S9(05)                   CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-FILLER PICTURE  X(162)                   CI0103
                          VALUE                SPACE.                   CI0103
            10            DD34-DTGRG.                                   CI0103
            11            DD34-DTGCY.                                   CI0103
            12            DD34-DTGCC  PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            12            DD34-DTGYY  PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            11            DD34-DTGMM  PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            11            DD34-DTGDD  PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-DTJUL.                                   CI0103
            11            DD34-DTJCY.                                   CI0103
            12            DD34-DTJCC  PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            12            DD34-DTJYY  PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            11            DD34-DTJDD  PICTURE  9(3)                     CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTFM  PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTLM  PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTFF  PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTLF  PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTFW  PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTLW  PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CCDOWA PICTURE  9                        CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CCDRW  PICTURE  9                        CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-FILLER PICTURE  X(58)                    CI0103
                          VALUE                SPACE.                   CI0103
            10            DD34-DTGRGA.                                  CI0103
            11            DD34-DTGCYA.                                  CI0103
            12            DD34-DTGCCA PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            12            DD34-DTGYYA PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            11            DD34-DTGMMA PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            11            DD34-DTGDDA PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-DTJULA.                                  CI0103
            11            DD34-DTJCYA.                                  CI0103
            12            DD34-DTJCCA PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            12            DD34-DTJYYA PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            11            DD34-DTJDDA PICTURE  9(3)                     CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTFMA PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTLMA PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTFFA PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTLFA PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTFWA PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTLWA PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CCDOWB PICTURE  9                        CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CCDRWA PICTURE  9                        CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-FILLER PICTURE  X(58)                    CI0103
                          VALUE                SPACE.                   CI0103
            10            DD34-DTGRGB.                                  CI0103
            11            DD34-DTGCYB.                                  CI0103
            12            DD34-DTGCCB PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            12            DD34-DTGYYB PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            11            DD34-DTGMMB PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            11            DD34-DTGDDB PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-DTJULB.                                  CI0103
            11            DD34-DTJCYB.                                  CI0103
            12            DD34-DTJCCB PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            12            DD34-DTJYYB PICTURE  9(2)                     CI0103
                          VALUE                ZERO.                    CI0103
            11            DD34-DTJDDB PICTURE  9(3)                     CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTFMB PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTLMB PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTFFB PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTLFB PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTFWB PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CDTLWB PICTURE  9(01)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CCDOWC PICTURE  9                        CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-CCDRWB PICTURE  9                        CI0103
                          VALUE                ZERO.                    CI0103
            10            DD34-FILLER PICTURE  X(58)                    CI0103
                          VALUE                SPACE.                   CI0103
            10            DD34-FILLER PICTURE  X(40)                    CI0103
                          VALUE                SPACE.                   CI0103
      **                                                                AADA82
      **   SEGMENT DD34 - CONVERT DATE LAYOUT                           AADA82
      **                                                                AADA82
      *!WF DSP=DD DSL=DD SEL=34 FOR=I DES=2 LEV=1                       AADA82
      **                                                                AADA82
      ******************************************************************
      ** WORK AREAS FOR AADA58 - CALL DATE ARITHMETIC MODULE
      ******************************************************************
      **
      *!WF DSP=DG DSL=DD SEL=01 FOR=I DES=2 LEV=1 PLT=DG
       01                 DG01.                                         CI0103
            10            DG01-XDAT8.                                   CI0103
            11            DG01-XDATC  PICTURE  XX                       CI0103
                          VALUE                SPACE.                   CI0103
            11            DG01-XDATY  PICTURE  XX                       CI0103
                          VALUE                SPACE.                   CI0103
            11            DG01-XDATM  PICTURE  XX                       CI0103
                          VALUE                SPACE.                   CI0103
            11            DG01-XDATD  PICTURE  XX                       CI0103
                          VALUE                SPACE.                   CI0103
            10            DG01-XDAT8D                                   CI0103
                          REDEFINES            DG01-XDAT8               CI0103
               PICTURE    9(8).                                         CI0103
            10            DG01-XDAT81.                                  CI0103
            11            DG01-XDATM1 PICTURE  XX                       CI0103
                          VALUE                SPACE.                   CI0103
            11            DG01-XDATD1 PICTURE  XX                       CI0103
                          VALUE                SPACE.                   CI0103
            11            DG01-XDATC1 PICTURE  XX                       CI0103
                          VALUE                SPACE.                   CI0103
            11            DG01-XDATY1 PICTURE  XX                       CI0103
                          VALUE                SPACE.                   CI0103
            10            DG01-XDAT80                                   CI0103
                          REDEFINES            DG01-XDAT81              CI0103
               PICTURE    9(8).                                         CI0103
            10            DG01-XDAT62.                                  CI0103
            11            DG01-XDATM2 PICTURE  XX                       CI0103
                          VALUE                SPACE.                   CI0103
            11            DG01-XDATD2 PICTURE  XX                       CI0103
                          VALUE                SPACE.                   CI0103
            11            DG01-XDATY2 PICTURE  XX                       CI0103
                          VALUE                SPACE.                   CI0103
            10            DG01-XDAT69                                   CI0103
                          REDEFINES            DG01-XDAT62              CI0103
               PICTURE    9(6).                                         CI0103
            10            DG01-XDATCU.                                  CI0103
            11            DG01-XDATC9 PICTURE  99                       CI0103
                          VALUE                ZERO.                    CI0103
            11            DG01-XDAYMD.                                  CI0103
            12            DG01-XDATY9 PICTURE  99                       CI0103
                          VALUE                ZERO.                    CI0103
            12            DG01-XDAMD.                                   CI0103
            13            DG01-XDATM9 PICTURE  99                       CI0103
                          VALUE                ZERO.                    CI0103
            13            DG01-XDATD9 PICTURE  99                       CI0103
                          VALUE                ZERO.                    CI0103
            10            DG01-XDAT89 PICTURE  9(8)                     CI0103
                          VALUE                ZERO.                    CI0103
            10            DG01-XDAJC  PICTURE  9(7)                     CI0103
                          VALUE                ZERO.                    CI0103
            10            DG01-XDAJC1.                                  CI0103
            11            DG01-XDAJC9 PICTURE  99                       CI0103
                          VALUE                ZERO.                    CI0103
            11            DG01-XDAJY  PICTURE  99                       CI0103
                          VALUE                ZERO.                    CI0103
            11            DG01-XDAJN  PICTURE  999                      CI0103
                          VALUE                ZERO.                    CI0103
            10            DG01-XDAB   PICTURE  9(5)                     CI0103
                          VALUE                ZERO.                    CI0103
            10            DG01-DD05.                                    CI0103
            11            DG01-XDACT  PICTURE  S9(3)                    CI0103
                          VALUE                ZERO                     CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            DG01-XDACV  PICTURE  S9                       CI0103
                          VALUE                ZERO                     CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            DG01-XDAGP  PICTURE  S9(9)                    CI0103
                          VALUE                ZERO                     CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            DG01-XDAJP  PICTURE  S9(7)                    CI0103
                          VALUE                ZERO                     CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            DG01-XDACV1 PICTURE  S9                       CI0103
                          VALUE                ZERO                     CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            DG01-XDAGP1 PICTURE  S9(9)                    CI0103
                          VALUE                ZERO                     CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            DG01-XDAJP1 PICTURE  S9(7)                    CI0103
                          VALUE                ZERO                     CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            DG01-XW03.                                    CI0103
            11            DG01-XDATG.                                   CI0103
            12            DG01-XDAT1.                                   CI0103
            13            DG01-XDAT19 PICTURE  99                       CI0103
                          VALUE                ZERO.                    CI0103
            12            DG01-XDAT2.                                   CI0103
            13            DG01-XDAT29 PICTURE  99                       CI0103
                          VALUE                ZERO.                    CI0103
            12            DG01-XDAT3.                                   CI0103
            13            DG01-XDAT39 PICTURE  99                       CI0103
                          VALUE                ZERO.                    CI0103
            12            DG01-XDAT4.                                   CI0103
            13            DG01-XDAT49 PICTURE  99                       CI0103
                          VALUE                ZERO.                    CI0103
            11            DG01-XLEAPY PICTURE  99                       CI0103
                          VALUE                ZERO.                    CI0103
            11            DG01-DTGCY  PICTURE  9(4)                     CI0103
                          VALUE                ZERO.                    CI0103
            11            DG01-FILLER                                   CI0103
                          REDEFINES            DG01-DTGCY.              CI0103
            12            DG01-DTGCC  PICTURE  9(2).                    CI0103
            12            DG01-DTGYY  PICTURE  9(2).                    CI0103
      **
      ******************************************************************
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0103
            10            XW05-XW06.                                    CI0103
            11            XW05-XDBPCB.                                  CI0103
            12            XW05-XDBDNM PICTURE  X(08)                    CI0103
                          VALUE                SPACE.                   CI0103
            12            XW05-XSEGLV PICTURE  X(02)                    CI0103
                          VALUE                SPACE.                   CI0103
            12            XW05-XRC    PICTURE  X(02)                    CI0103
                          VALUE                SPACE.                   CI0103
            12            XW05-XPROPT PICTURE  X(04)                    CI0103
                          VALUE                SPACE.                   CI0103
            12            XW05-FILLER PICTURE  S9(5)                    CI0103
                          VALUE                ZERO                     CI0103
                          BINARY.                                       CI0103
            12            XW05-XSEGNM PICTURE  X(08)                    CI0103
                          VALUE                SPACE.                   CI0103
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0103
                          VALUE                ZERO                     CI0103
                          BINARY.                                       CI0103
            12            XW05-XSEGNB PICTURE  9(05)                    CI0103
                          VALUE                ZERO                     CI0103
                          BINARY.                                       CI0103
            12            XW05-XCOKEY PICTURE  X(70)                    CI0103
                          VALUE                SPACE.                   CI0103
            10            XW05-XW07.                                    CI0103
            11            XW05-XIOPCB.                                  CI0103
            12            XW05-XTERMI PICTURE  X(08)                    CI0103
                          VALUE                SPACE.                   CI0103
            12            XW05-FILLER PICTURE  XX                       CI0103
                          VALUE                SPACE.                   CI0103
            12            XW05-XRC1   PICTURE  X(02)                    CI0103
                          VALUE                SPACE.                   CI0103
            12            XW05-FILLER PICTURE  X(12)                    CI0103
                          VALUE                SPACE.                   CI0103
            12            XW05-XMODNM PICTURE  X(8)                     CI0103
                          VALUE                SPACE.                   CI0103
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0103
                          VALUE                ZERO.                    CI0103
            10            XW05-XGU    PICTURE  X(4)                     CI0103
                          VALUE                'GU  '.                  CI0103
            10            XW05-XGHU   PICTURE  X(4)                     CI0103
                          VALUE                'GHU '.                  CI0103
            10            XW05-XGN    PICTURE  X(4)                     CI0103
                          VALUE                'GN  '.                  CI0103
            10            XW05-XGHN   PICTURE  X(4)                     CI0103
                          VALUE                'GHN '.                  CI0103
            10            XW05-XGNP   PICTURE  X(4)                     CI0103
                          VALUE                'GNP '.                  CI0103
            10            XW05-XGHNP  PICTURE  X(4)                     CI0103
                          VALUE                'GHNP'.                  CI0103
            10            XW05-XREPL  PICTURE  XXXX                     CI0103
                          VALUE                'REPL'.                  CI0103
            10            XW05-XISRT  PICTURE  X(4)                     CI0103
                          VALUE                'ISRT'.                  CI0103
            10            XW05-XDLET  PICTURE  X(4)                     CI0103
                          VALUE                'DLET'.                  CI0103
            10            XW05-XOPEN  PICTURE  X(4)                     CI0103
                          VALUE                'OPEN'.                  CI0103
            10            XW05-XCLSE  PICTURE  X(4)                     CI0103
                          VALUE                'CLSE'.                  CI0103
            10            XW05-XCHKP  PICTURE  X(4)                     CI0103
                          VALUE                'CHKP'.                  CI0103
            10            XW05-XXRST  PICTURE  X(4)                     CI0103
                          VALUE                'XRST'.                  CI0103
            10            XW05-XTERM  PICTURE  X(4)                     CI0103
                          VALUE                'TERM'.                  CI0103
            10            XW05-XNFPAC PICTURE  X(13)                    CI0103
                          VALUE                SPACE.                   CI0103
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0103
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0103
      *                                                                 ADU155
      ******************************************************************ADU155
      ** WORK AREA NEEDED FOR MACRO ADU155                             *ADU155
      **        DATE COMMON AREA FOR EXECUTING CICS ASKTIME/FORMATTIME *ADU155
      ******************************************************************ADU155
      *                                                                 ADU155
      *!WI pl=DT100                                                     ADU155
       01  DT01-XMSTS                                                   ADU155
                        PICTURE S9(15)                                  CI0103
                          COMPUTATIONAL-3.                              CI0103
       01  DT01-F2CCYY             PIC S9(08) COMP.                     ADU155
      *!WI pl=DT200                                                     ADU155
       01  DT01-XDAT69                                                  ADU155
                        PICTURE 9(6).                                   CI0103
       01  DT01-UDATE.                                                  ADU155
           05  DT01-YEAR           PIC  9(04).                          ADU155
           05  DT01-MMDD           PIC  9(04).                          ADU155
      *!WI pl=DT280                                                     ADU155
       01  DT01-XDATCU REDEFINES DT01-UDATE                             ADU155
                        PICTURE X(8).                                   CI0103
      *                                                                 ADU155
      *SEGMENTS AND WORKING STORAGE VARIABLES FOR CALLING DBI5000N.     AWSDBI
      *!WF DSP=SQ DSL=SQ SEL=1L2L3L FOR=I DES=1 LEV=1                   AWSDBI
       01                 SQ1F.                                         CI0103
            10            SQ1F-CSYSI  PICTURE  X(4).                    CI0103
            10            SQ1F-CFSPN  PICTURE  X(3).                    CI0103
            10            SQ1F-NRERO  PICTURE  9(3).                    CI0103
            10            SQ1F-FILLER PICTURE  X(35).                   CI0103
            10            SQ1F-GTD75                                    CI0103
                          OCCURS 0 TO  150     TIMES                    CI0103
                          DEPENDING  ON        SQ1F-NRERO.              CI0103
            11            SQ1F-CTID.                                    CI0103
            12            SQ1F-CTIDA  PICTURE  9(3).                    CI0103
            12            SQ1F-CTIDNP PICTURE  X(13).                   CI0103
            12            SQ1F-CTIDND PICTURE  9(11).                   CI0103
            11            SQ1F-PRCOD  PICTURE  9(5).                    CI0103
            11            SQ1F-CPRSCN PICTURE  9(9).                    CI0103
            11            SQ1F-FILLER PICTURE  X(34).                   CI0103
       01                 SQ1L.                                         CI0103
            10            SQ1L-NPVERH PICTURE  9(3).                    CI0103
            10            SQ1L-NPVERC PICTURE  9(3).                    CI0103
            10            SQ1L-NPVERD PICTURE  9(3).                    CI0103
            10            SQ1L-NVIEW  PICTURE  X(4).                    CI0103
            10            SQ1L-NCUSR2 PICTURE  X(12).                   CI0103
            10            SQ1L-CPRT2  PICTURE  X.                       CI0103
            10            SQ1L-MRPIB1 PICTURE  X(08).                   CI0103
            10            SQ1L-CLOGY  PICTURE  X.                       CI0103
            10            SQ1L-QTOUT  PICTURE  9(5).                    CI0103
            10            SQ1L-GELLP1 PICTURE  S9(5)                    CI0103
                          BINARY.                                       CI0103
            10            SQ1L-GELLP2 PICTURE  S9(5)                    CI0103
                          BINARY.                                       CI0103
            10            SQ1L-CVSIZ  PICTURE  X(1).                    CI0103
            10            SQ1L-FILLER PICTURE  X(49).                   CI0103
       01                 SQ2L.                                         CI0103
            10            SQ2L-NPID4  PICTURE  X(4).                    CI0103
            10            SQ2L-NVIEW  PICTURE  X(4).                    CI0103
            10            SQ2L-CTRID  PICTURE  X(4).                    CI0103
            10            SQ2L-CPRGSX PICTURE  X(8).                    CI0103
            10            SQ2L-COMND  PICTURE  X(8).                    CI0103
            10            SQ2L-CERRE1 PICTURE  X(5).                    CI0103
            10            SQ2L-CSEVR1 PICTURE  X(2).                    CI0103
            10            SQ2L-GERD1.                                   CI0103
            11            SQ2L-FILLER PICTURE  X(3).                    CI0103
            11            SQ2L-CERRE2 PICTURE  X(5).                    CI0103
            10            SQ2L-TERMT  PICTURE  X(66).                   CI0103
            10            SQ2L-GERD2.                                   CI0103
            11            SQ2L-FILLER PICTURE  X(3).                    CI0103
            11            SQ2L-CERRE3 PICTURE  X(5).                    CI0103
            10            SQ2L-TER255 PICTURE  X(255).                  CI0103
            10            SQ2L-FILLER PICTURE  X(50).                   CI0103
       01                 SQ3L.                                         CI0103
            10            SQ3L-GELLP3 PICTURE  S9(5)                    CI0103
                          BINARY.                                       CI0103
            10            SQ3L-GELLP4 PICTURE  S9(5)                    CI0103
                          BINARY.                                       CI0103
            10            SQ3L-NAREA  PICTURE  9(03).                   CI0103
            10            SQ3L-GEOPDM PICTURE  X(8).                    CI0103
            10            SQ3L-CAPIR1 PICTURE  9(3).                    CI0103
            10            SQ3L-FILLER PICTURE  X(50).                   CI0103
       01                 SQ5F.                                         CI0103
            10            SQ5F-GRFIX.                                   CI0103
            11            SQ5F-CERRE1 PICTURE  X(5).                    CI0103
            11            SQ5F-NRURO  PICTURE  9(3).                    CI0103
            11            SQ5F-FILLER PICTURE  X(32).                   CI0103
            10            SQ5F-GRVAR                                    CI0103
                          OCCURS 0 TO  150     TIMES                    CI0103
                          DEPENDING  ON        SQ5F-NRURO.              CI0103
            11            SQ5F-CERRE2 PICTURE  X(5).                    CI0103
            11            SQ5F-CTID.                                    CI0103
            12            SQ5F-CTIDA  PICTURE  9(3).                    CI0103
            12            SQ5F-CTIDNP PICTURE  X(13).                   CI0103
            12            SQ5F-NITDN1 PICTURE  9(11).                   CI0103
            11            SQ5F-PRCOD  PICTURE  9(5).                    CI0103
            11            SQ5F-CPRSCN PICTURE  9(9).                    CI0103
            11            SQ5F-ANGOF1 PICTURE  S9(9)V99.                CI0103
            11            SQ5F-AGOFD2 PICTURE  S9(9)V99.                CI0103
            11            SQ5F-AFAV10 PICTURE  S9(4)V9(3).              CI0103
            11            SQ5F-FILLER PICTURE  X(47).                   CI0103
      *                                                                 AWSDBI
       01     WL00-REQUEST.                                             AWSDBI
              05   FILLER    PIC X OCCURS 1 TO 32554                    AWSDBI
                             DEPENDING ON SQ1L-GELLP1.                  AWSDBI
      *                                                                 AWSDBI
       01     WL00-RESPONSE.                                            AWSDBI
              05   FILLER    PIC X OCCURS 1 TO 32594                    AWSDBI
                             DEPENDING ON SQ1L-GELLP2.                  AWSDBI
      *                                                                 AWSDBI
      *WORKING STORAGE VARIABLE FOR NAME OF BROKER MODULE DBI5000N.     AWSDBI
       01     DBI5000N  PIC X(8) VALUE 'DBI5000N'.                      AWSDBI
      *                                                                 AWSDBI
                                                                        AM0100
      ******************************************************************AM0100
      **     FIELDS USED IN THE PARAMETER LIST OF CI0100.  THESE WILL  *AM0100
      **     BE VALUED AND PASSED IN THE CALLING PROGRAM.              *AM0100
      ******************************************************************AM0100
                                                                        AM0100
       01  7-GC00-AREA.                                                 AM0100
      *!WI pl=GC140                                                     AM0100
           05  7-GC00-MAPPN                                             AM0100
                        PICTURE X(10)                                   CI0103
                                   VALUE SPACES.                        AM0100
      *!WI pl=GC180                                                     AM0100
           05  7-GC00-CFUNC                                             AM0100
                        PICTURE X(3)                                    CI0103
                                   VALUE SPACES.                        AM0100
      *!WI pl=GC220                                                     AM0100
           05  7-GC00-CASTC OCCURS 6                                    AM0100
                        PICTURE 99.                                     CI0103
      *!WI pl=GC240                                                     AM0100
           05  7-GC00-CAATY OCCURS 3                                    AM0100
                        PICTURE 9(3).                                   CI0103
           05  7-GC00-C299.                                             AM0100
      *!WI pl=GC280                                                     AM0100
               10  7-GC00-CTID                                          AM0100
                        PICTURE X(27)                                   CI0103
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC320                                                     AM0100
           05  7-GC00-DCACG9                                            AM0100
                        PICTURE 9(8)                                    CI0103
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC360                                                     AM0100
           05  7-GC00-NAASQ                                             AM0100
                        PICTURE S9(3)                                   CI0103
                          COMPUTATIONAL-3                               CI0103
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC400                                                     AM0100
           05  7-GC00-NPISQ                                             AM0100
                        PICTURE S9(3)                                   CI0103
                          COMPUTATIONAL-3                               CI0103
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC440                                                     AM0100
           05  7-GC00-CIRAP                                             AM0100
                        PICTURE XX                                      CI0103
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC480                                                     AM0100
           05  7-GC00-IPERT                                             AM0100
                        PICTURE X                                       CI0103
                                   VALUE SPACES.                        AM0100
      *!WI pl=GC520                                                     AM0100
           05  7-GC00-NEIBT                                             AM0100
                        PICTURE X(7)                                    CI0103
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC560                                                     AM0100
           05  7-GC00-GESQ2C                                            AM0100
                        PICTURE S99                                     CI0103
                          COMPUTATIONAL-3                               CI0103
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC600                                                     AM0100
           05  7-GC00-MIPPS                                             AM0100
                        PICTURE X(4)                                    CI0103
                                   VALUE SPACES.                        AM0100
      *!WI pl=GC640                                                     AM0100
           05  7-GC00-IENDP                                             AM0100
                        PICTURE X                                       CI0103
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
       01                 GX01.                                         CI0103
            10            GX01-GC01K.                                   CI0103
            11            GX01-C299.                                    CI0103
            12            GX01-CTID.                                    CI0103
            13            GX01-CTIDA  PICTURE  9(3).                    CI0103
            13            GX01-CTIDN.                                   CI0103
            14            GX01-CTIDNP PICTURE  X(13).                   CI0103
            14            GX01-CTIDND PICTURE  9(11).                   CI0103
            10            GX01-DCAG9L PICTURE  9(8).                    CI0103
            10            GX01-NAASQL PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GX01-ICUST  PICTURE  X.                       CI0103
            10            GX01-NSEQ4B PICTURE  9(8)                     CI0103
                          BINARY.                                       CI0103
            10            GX01-PRCOD  PICTURE  9(5).                    CI0103
            10            GX01-PRSCD  PICTURE  X(9).                    CI0103
            10            GX01-FILLER PICTURE  X(8).                    CI0103
       01                 GX03.                                         CI0103
            10            GX03-GELL   PICTURE  9(4)                     CI0103
                          BINARY.                                       CI0103
            10            GX03-GD00.                                    CI0103
            11            GX03-GC03K.                                   CI0103
            12            GX03-DCACG9 PICTURE  9(8).                    CI0103
            12            GX03-NAASQ  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-CAATY  PICTURE  9(3).                    CI0103
            11            GX03-CVSYS  PICTURE  X(2).                    CI0103
            11            GX03-CACTO  PICTURE  9(3).                    CI0103
            11            GX03-CATRN.                                   CI0103
            12            GX03-CATRF  PICTURE  9(3).                    CI0103
            12            GX03-CATRS  PICTURE  9(3).                    CI0103
            11            GX03-CASTC  PICTURE  99.                      CI0103
            11            GX03-IPULL  PICTURE  X.                       CI0103
            11            GX03-GEAUN  PICTURE  9(5).                    CI0103
            11            GX03-GEOPD2 PICTURE  X(8).                    CI0103
            11            GX03-NBTCH  PICTURE  9(4).                    CI0103
            11            GX03-DEFFT  PICTURE  9(8).                    CI0103
            11            GX03-NSUNT  PICTURE  9(4).                    CI0103
            11            GX03-ITRAN  PICTURE  X.                       CI0103
            11            GX03-DLAUP1 PICTURE  9(8).                    CI0103
            11            GX03-ADRET  PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-TTRMS  PICTURE  X(12).                   CI0103
            11            GX03-IDELT  PICTURE  X.                       CI0103
            11            GX03-GEOPDM PICTURE  X(8).                    CI0103
            11            GX03-FILLER PICTURE  X(07).                   CI0103
            10            GX03-GD09.                                    CI0103
            11            GX03-FILLER PICTURE  X(70).                   CI0103
            10            GX03-GD01                                     CI0103
                          REDEFINES            GX03-GD09.               CI0103
            11            GX03-ADBRQ  PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-CTRTP  PICTURE  X(2).                    CI0103
            11            GX03-CPORT  PICTURE  X.                       CI0103
            11            GX03-CSCRNU PICTURE  X(4).                    CI0103
            11            GX03-DLAUP  PICTURE  9(8).                    CI0103
            11            GX03-CTWHAT PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-PWHLD  PICTURE  S999V9(5)                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-IWTHH  PICTURE  X.                       CI0103
            11            GX03-NDRFT  PICTURE  9(5).                    CI0103
            11            GX03-IDPAP  PICTURE  X.                       CI0103
            11            GX03-GETIM  PICTURE  S9(7)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-QNACT  PICTURE  9(3).                    CI0103
            11            GX03-AEDRQ  PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-IPLIN  PICTURE  X.                       CI0103
            11            GX03-CLIDNB PICTURE  9(8).                    CI0103
            11            GX03-CSLCT  PICTURE  X.                       CI0103
            11            GX03-ITELE  PICTURE  X.                       CI0103
            11            GX03-FILLER PICTURE  X(06).                   CI0103
            10            GX03-GD02                                     CI0103
                          REDEFINES            GX03-GD09.               CI0103
            11            GX03-CSYST  PICTURE  99.                      CI0103
            11            GX03-FILLER PICTURE  X.                       CI0103
            11            GX03-ACASH  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-DTRAC  PICTURE  9(8).                    CI0103
            11            GX03-CTRSO  PICTURE  9(02).                   CI0103
            11            GX03-NTRCE  PICTURE  9(06).                   CI0103
            11            GX03-GECKD1 PICTURE  9.                       CI0103
            11            GX03-CCOLL  PICTURE  X(3).                    CI0103
            11            GX03-CLTDP  PICTURE  X(3).                    CI0103
            11            GX03-PSLLD  PICTURE  S99V999                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-ISLOR  PICTURE  X.                       CI0103
            11            GX03-ITPAC  PICTURE  X.                       CI0103
            11            GX03-CPMTCA PICTURE  XXX.                     CI0103
            11            GX03-CSERV  PICTURE  X(3).                    CI0103
            11            GX03-ACOMO  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-IPLIN1 PICTURE  X.                       CI0103
            11            GX03-INQEX  PICTURE  X.                       CI0103
            11            GX03-CTKRAA PICTURE  X(12).                   CI0103
            11            GX03-CCSMQ  PICTURE  X.                       CI0103
            11            GX03-IVAEX1 PICTURE  X.                       CI0103
            11            GX03-IHPMT  PICTURE  X(1).                    CI0103
            11            GX03-GETIM3 PICTURE  S9(7)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GX03-GD03                                     CI0103
                          REDEFINES            GX03-GD09.               CI0103
            11            GX03-CATRNC PICTURE  9(6).                    CI0103
            11            GX03-APRNT1 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-QSHOWT PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-ACINVT PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-ACOMO7 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-QSHOMW PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-ATAXT3 PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-CTSTR  PICTURE  9(2).                    CI0103
            11            GX03-ICIRA  PICTURE  X.                       CI0103
            11            GX03-GETIM2 PICTURE  S9(7)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-CPMTCX PICTURE  XX.                      CI0103
            11            GX03-FILLER PICTURE  X(16).                   CI0103
            10            GX03-GD99.                                    CI0103
            11            GX03-FILLER PICTURE  X(248).                  CI0103
            10            GX03-GD10                                     CI0103
                          REDEFINES            GX03-GD99.               CI0103
            11            GX03-MROTC  PICTURE  X(7).                    CI0103
            11            GX03-CEDSC  PICTURE  9(1).                    CI0103
            11            GX03-ILPOI  PICTURE  X(1).                    CI0103
            11            GX03-AWRCH  PICTURE  S9(3)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-CHCOC1 PICTURE  9(2).                    CI0103
            11            GX03-CHCOC2 PICTURE  9(2).                    CI0103
            11            GX03-CHCOC3 PICTURE  9(2).                    CI0103
            11            GX03-CHCOC4 PICTURE  9(2).                    CI0103
            11            GX03-CMCOC1 PICTURE  9(3).                    CI0103
            11            GX03-CMCOC2 PICTURE  9(3).                    CI0103
            11            GX03-CMCOC3 PICTURE  9(3).                    CI0103
            11            GX03-GD11.                                    CI0103
            12            GX03-FILLER PICTURE  X(219).                  CI0103
            11            GX03-GD12                                     CI0103
                          REDEFINES            GX03-GD11.               CI0103
            12            GX03-CELLO  PICTURE  9(1).                    CI0103
            12            GX03-CECLO  PICTURE  9(1).                    CI0103
            12            GX03-AEXML  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-CEPI   PICTURE  X(1).                    CI0103
            12            GX03-CEXTY  PICTURE  X.                       CI0103
            12            GX03-CROPC  PICTURE  9(1).                    CI0103
            12            GX03-CPUTY  PICTURE  9(1).                    CI0103
            12            GX03-IMCII  PICTURE  X(1).                    CI0103
            12            GX03-GEMISC                                   CI0103
                          OCCURS       010     TIMES.                   CI0103
            13            GX03-AMGLA  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            13            GX03-CMGLC  PICTURE  9(1).                    CI0103
            13            GX03-NMGLN  PICTURE  9(4).                    CI0103
            12            GX03-ACTRN  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-IWRBK  PICTURE  X.                       CI0103
            12            GX03-IFEDX  PICTURE  X.                       CI0103
            12            GX03-ICNTR  PICTURE  X.                       CI0103
            12            GX03-IOCKH  PICTURE  X.                       CI0103
            12            GX03-ICRCK  PICTURE  X.                       CI0103
            12            GX03-NHMPN  PICTURE  S9(10)                   CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-ITELR1 PICTURE  X.                       CI0103
            11            GX03-GD13                                     CI0103
                          REDEFINES            GX03-GD11.               CI0103
            12            GX03-DREDO  PICTURE  9(8).                    CI0103
            12            GX03-CATRNR PICTURE  9(6).                    CI0103
            12            GX03-CEVN   PICTURE  9(9).                    CI0103
            12            GX03-ISUSP  PICTURE  X(1).                    CI0103
            11            GX03-GD15                                     CI0103
                          REDEFINES            GX03-GD11.               CI0103
            12            GX03-CPUTZ  PICTURE  9(1).                    CI0103
            12            GX03-CETLB  PICTURE  9(3).                    CI0103
            12            GX03-QTRMC  PICTURE  9(3).                    CI0103
            12            GX03-DEFFTE PICTURE  9(8).                    CI0103
            12            GX03-DEFFTF PICTURE  9(8).                    CI0103
            12            GX03-DEFFTG PICTURE  9(8).                    CI0103
            12            GX03-XZ1A   PICTURE  X.                       CI0103
            12            GX03-XZ1B   PICTURE  X.                       CI0103
            12            GX03-XZ1C   PICTURE  X.                       CI0103
            12            GX03-XZ1D   PICTURE  X.                       CI0103
            12            GX03-XZ1E   PICTURE  X.                       CI0103
            12            GX03-XZ1F   PICTURE  X.                       CI0103
            12            GX03-XZ1G   PICTURE  X.                       CI0103
            12            GX03-XZ1H   PICTURE  X.                       CI0103
            12            GX03-XZ1I   PICTURE  X.                       CI0103
            12            GX03-DEFFTH PICTURE  9(8).                    CI0103
            11            GX03-GD19                                     CI0103
                          REDEFINES            GX03-GD11.               CI0103
            12            GX03-GD11.                                    CI0103
            13            GX03-FILLER PICTURE  X(219).                  CI0103
            10            GX03-GD20                                     CI0103
                          REDEFINES            GX03-GD99.               CI0103
            11            GX03-ADDACT PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-ISIGV  PICTURE  X.                       CI0103
            11            GX03-IALLF  PICTURE  X.                       CI0103
            11            GX03-QSHOWQ PICTURE  S9(9)V999                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-CCDSCW PICTURE  9(2).                    CI0103
            11            GX03-IDWRL  PICTURE  X.                       CI0103
            11            GX03-ITELR  PICTURE  X.                       CI0103
            11            GX03-IABIN  PICTURE  X.                       CI0103
            11            GX03-PACT1  PICTURE  S999V999                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-IBFAF  PICTURE  X.                       CI0103
            11            GX03-IFRSA  PICTURE  X.                       CI0103
            11            GX03-ICRCAN PICTURE  X.                       CI0103
            11            GX03-ACACTV PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-AGFND  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-QCSHOW PICTURE  S9(9)V999                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-QCSHIS PICTURE  S9(9)V999                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-NDTRC  PICTURE  9(8).                    CI0103
            11            GX03-CAERU  PICTURE  X(4).                    CI0103
            11            GX03-IFDGO  PICTURE  X.                       CI0103
            11            GX03-PSLLD2 PICTURE  S99V999                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-ISLOR2 PICTURE  X.                       CI0103
            11            GX03-QSFIO  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-QSFID  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-CGDIN  PICTURE  X.                       CI0103
            11            GX03-DGDIN  PICTURE  9(8).                    CI0103
            10            GX03-GD30                                     CI0103
                          REDEFINES            GX03-GD99.               CI0103
            11            GX03-ISKED  PICTURE  X.                       CI0103
            11            GX03-CENXC  PICTURE  9(2).                    CI0103
            11            GX03-GD31.                                    CI0103
            12            GX03-FILLER PICTURE  X(245).                  CI0103
            11            GX03-GD32                                     CI0103
                          REDEFINES            GX03-GD31.               CI0103
            12            GX03-IABIN1 PICTURE  X.                       CI0103
            12            GX03-CLDOD  PICTURE  9(8).                    CI0103
            12            GX03-NCLAM  PICTURE  9(5)                     CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-ISURR  PICTURE  X.                       CI0103
            12            GX03-GEHCD  PICTURE  9(3).                    CI0103
            12            GX03-CRATC  PICTURE  9(4).                    CI0103
            12            GX03-AMAXD  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-ASCHGA PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-APYOM  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-IWTHH1 PICTURE  X.                       CI0103
            12            GX03-CPAYCL PICTURE  X(2).                    CI0103
            12            GX03-CTSAO  PICTURE  X.                       CI0103
            12            GX03-NCONF  PICTURE  9(08).                   CI0103
            12            GX03-CLID   PICTURE  X(23).                   CI0103
            12            GX03-CARTY  PICTURE  99.                      CI0103
            12            GX03-NARRS  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-CARTZ  PICTURE  99.                      CI0103
            12            GX03-NAPDS  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-CPMTO  PICTURE  X.                       CI0103
            12            GX03-DNPMT  PICTURE  9(8).                    CI0103
            12            GX03-IPCTV  PICTURE  X.                       CI0103
            12            GX03-IMECH  PICTURE  X(01).                   CI0103
            12            GX03-IMVAO  PICTURE  X(1).                    CI0103
            12            GX03-AMVA1  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-CACTS  PICTURE  X.                       CI0103
            12            GX03-CTSPP  PICTURE  X(1).                    CI0103
            12            GX03-CACT4  PICTURE  X(2).                    CI0103
            12            GX03-IVAEX  PICTURE  X.                       CI0103
            12            GX03-DFPMT  PICTURE  9(8).                    CI0103
            12            GX03-IDEMD  PICTURE  X.                       CI0103
            12            GX03-IOFST  PICTURE  X.                       CI0103
            12            GX03-AMXLB  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-ACULB  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-DEIRNB PICTURE  9(8).                    CI0103
            12            GX03-DEFFE  PICTURE  9(8).                    CI0103
            12            GX03-DEFFR  PICTURE  9(8).                    CI0103
            12            GX03-ISPUP  PICTURE  X.                       CI0103
            12            GX03-CPNCG  PICTURE  X.                       CI0103
            12            GX03-IEXPU  PICTURE  X.                       CI0103
            12            GX03-IPPCF  PICTURE  X.                       CI0103
            12            GX03-NAAPT  PICTURE  9(2).                    CI0103
            12            GX03-PWHLDS PICTURE  S999V9(5)                CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-ISWHO  PICTURE  X(1).                    CI0103
            11            GX03-GD33                                     CI0103
                          REDEFINES            GX03-GD31.               CI0103
            12            GX03-CPAYC  PICTURE  X(2).                    CI0103
            12            GX03-ADBRQX PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-ADBRQV PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-APTXR  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-CTRTPE PICTURE  X(2).                    CI0103
            12            GX03-NCLAMI PICTURE  S9(9)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-CLIDO8 PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-CLIDN  PICTURE  X(20).                   CI0103
            12            GX03-DSET01 PICTURE  S9(8)                    CI0103
                          BINARY.                                       CI0103
            12            GX03-CTSET1 PICTURE  S9(6)                    CI0103
                          BINARY.                                       CI0103
            12            GX03-DSET02 PICTURE  S9(8)                    CI0103
                          BINARY.                                       CI0103
            12            GX03-CTSET2 PICTURE  S9(6)                    CI0103
                          BINARY.                                       CI0103
            11            GX03-GD34                                     CI0103
                          REDEFINES            GX03-GD31.               CI0103
            12            GX03-QNOFM  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-CLTRM  PICTURE  99.                      CI0103
            12            GX03-AMXLN  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-ALADJ  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-ACHK   PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-APRMO  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-IMECH1 PICTURE  X(01).                   CI0103
            12            GX03-CACT41 PICTURE  X(2).                    CI0103
            12            GX03-ACDSCC PICTURE  S9(05)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-ACDSCD PICTURE  S9(05)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-GD39                                     CI0103
                          REDEFINES            GX03-GD31.               CI0103
            12            GX03-GD31.                                    CI0103
            13            GX03-FILLER PICTURE  X(245).                  CI0103
            10            GX03-GD40                                     CI0103
                          REDEFINES            GX03-GD99.               CI0103
            11            GX03-NTR    PICTURE  9(8).                    CI0103
            11            GX03-NPBNC  PICTURE  X(24).                   CI0103
            11            GX03-CRREV  PICTURE  X(3).                    CI0103
            11            GX03-CSUSL  PICTURE  S9.                      CI0103
            11            GX03-NMGLN1 PICTURE  9(4).                    CI0103
            11            GX03-DCAC92 PICTURE  9(8).                    CI0103
            11            GX03-NAASQ3 PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-GD49.                                    CI0103
            12            GX03-FILLER PICTURE  X(198).                  CI0103
            11            GX03-GD41                                     CI0103
                          REDEFINES            GX03-GD49.               CI0103
            12            GX03-CRREF  PICTURE  9(2).                    CI0103
            12            GX03-CORIR  PICTURE  X(02).                   CI0103
            12            GX03-CIPDB  PICTURE  X(03).                   CI0103
            12            GX03-CPAYH  PICTURE  X(02).                   CI0103
            12            GX03-NAMEX  PICTURE  9(15)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX03-DCHAE  PICTURE  9(4).                    CI0103
            12            GX03-DRQST  PICTURE  S9(8)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-GD42                                     CI0103
                          REDEFINES            GX03-GD49.               CI0103
            12            GX03-CPMTCB PICTURE  X(3).                    CI0103
            10            GX03-GD50                                     CI0103
                          REDEFINES            GX03-GD99.               CI0103
            11            GX03-ALOAD  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-PSLLD4 PICTURE  S99V999                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-CSUSL1 PICTURE  S9.                      CI0103
            11            GX03-CRREV1 PICTURE  X(3).                    CI0103
            11            GX03-ADDAC  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-DL13.                                    CI0103
            12            GX03-GEYR   PICTURE  9(4).                    CI0103
            12            GX03-GEMTH  PICTURE  99.                      CI0103
            12            GX03-NDAY   PICTURE  99.                      CI0103
            11            GX03-NSEQ3P PICTURE  S9(5)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-XZ6A   PICTURE  X(6).                    CI0103
            11            GX03-XZ7    PICTURE  X(7).                    CI0103
            11            GX03-XZ6B   PICTURE  X(6).                    CI0103
            11            GX03-XZ6    PICTURE  X(6).                    CI0103
            11            GX03-XZ6C   PICTURE  X(6).                    CI0103
            11            GX03-XZ20   PICTURE  X(20).                   CI0103
            11            GX03-CATRN1 PICTURE  9(6).                    CI0103
            11            GX03-ADDAC2 PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-ATAXT2 PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-ACOMOT PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-XZ5    PICTURE  X(5).                    CI0103
            11            GX03-IREVD  PICTURE  X(1).                    CI0103
            11            GX03-ISUSP1 PICTURE  X(1).                    CI0103
            11            GX03-XZ6D   PICTURE  X(6).                    CI0103
            11            GX03-XZ13   PICTURE  X(13).                   CI0103
            11            GX03-CWHTP2 PICTURE  X(3).                    CI0103
            11            GX03-CWHTP3 PICTURE  X(3).                    CI0103
            11            GX03-DTREN  PICTURE  9(8).                    CI0103
            11            GX03-NAASQ1 PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GX03-GD51                                     CI0103
                          REDEFINES            GX03-GD99.               CI0103
            11            GX03-ADOMOT PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-ACGLT  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-ACGST  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-CTXMT  PICTURE  9(2).                    CI0103
            11            GX03-ALOAD3 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-FILLER PICTURE  X(31).                   CI0103
            10            GX03-GD52                                     CI0103
                          REDEFINES            GX03-GD99.               CI0103
            11            GX03-DEFFT5 PICTURE  9(8).                    CI0103
            11            GX03-PSLLD5 PICTURE  S99V999                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-CSUSL2 PICTURE  S9.                      CI0103
            11            GX03-ALOAD2 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-DL22.                                    CI0103
            12            GX03-NYEAR1 PICTURE  9(4).                    CI0103
            12            GX03-GEMTHA PICTURE  99.                      CI0103
            12            GX03-NDAY01 PICTURE  99.                      CI0103
            11            GX03-NSEQ3R PICTURE  S9(5)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-CWHTP  PICTURE  X(3).                    CI0103
            11            GX03-CWHFR  PICTURE  X(3).                    CI0103
            11            GX03-CATRN7 PICTURE  9(6).                    CI0103
            11            GX03-ATAXT5 PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-QSHOT  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-ACINT3 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-CWHTP1 PICTURE  X(3).                    CI0103
            11            GX03-CWHFR1 PICTURE  X(3).                    CI0103
            11            GX03-ACOMO5 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-QSHOMU PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-ACASH1 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-FILLER PICTURE  X(04).                   CI0103
            11            GX03-CATRN8 PICTURE  9(6).                    CI0103
            11            GX03-ALOAD1 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-PSLLD1 PICTURE  S99V999                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-QSHOT1 PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-ACINT4 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-CSUSL4 PICTURE  S9.                      CI0103
            11            GX03-ACOMO4 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GX03-GD60                                     CI0103
                          REDEFINES            GX03-GD99.               CI0103
            11            GX03-GEOPDD PICTURE  X(8)                     CI0103
                          OCCURS       005     TIMES.                   CI0103
            11            GX03-DLAUP3 PICTURE  9(8)                     CI0103
                          OCCURS       005     TIMES.                   CI0103
            11            GX03-GEOPDB PICTURE  X(8).                    CI0103
            11            GX03-DLAUP4 PICTURE  9(8).                    CI0103
            11            GX03-ITELR2 PICTURE  X.                       CI0103
            11            GX03-IPMTA  PICTURE  X.                       CI0103
            11            GX03-CCSMG  PICTURE  X.                       CI0103
            11            GX03-CPLEC  PICTURE  XX.                      CI0103
            11            GX03-CORTYA PICTURE  X(3).                    CI0103
            11            GX03-CACTBC PICTURE  X(1).                    CI0103
            11            GX03-CGSPIA PICTURE  X.                       CI0103
            11            GX03-IPTRDA PICTURE  X(01).                   CI0103
            11            GX03-GCUSPY PICTURE  X(12).                   CI0103
            11            GX03-CPALLA PICTURE  X(1).                    CI0103
            11            GX03-QSHO5A PICTURE  S9(9)V999                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-IFRSAB PICTURE  X.                       CI0103
            11            GX03-DELOI  PICTURE  9(8).                    CI0103
            11            GX03-IAROAA PICTURE  X.                       CI0103
            11            GX03-ACINVR PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-ILTINA PICTURE  X.                       CI0103
            11            GX03-ALOIDA PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX03-CFUNTA PICTURE  X(2).                    CI0103
            11            GX03-CLGND  PICTURE  X.                       CI0103
            11            GX03-CPH3U  PICTURE  X.                       CI0103
            11            GX03-GESTD  PICTURE  9(8).                    CI0103
            11            GX03-GEEND  PICTURE  9(8).                    CI0103
            11            GX03-CPMTF  PICTURE  99.                      CI0103
            11            GX03-CNAVR  PICTURE  X(1).                    CI0103
            10            GX03-GD70                                     CI0103
                          REDEFINES            GX03-GD99.               CI0103
            11            GX03-CMEMO  PICTURE  X(2).                    CI0103
            11            GX03-ALPLDT PICTURE  9(8).                    CI0103
            11            GX03-CTLPD  PICTURE  9(8).                    CI0103
            11            GX03-CPAYCM PICTURE  X(2).                    CI0103
       01                 GX06.                                         CI0103
            10            GX06-GELL   PICTURE  9(4)                     CI0103
                          BINARY.                                       CI0103
            10            GX06-GE00.                                    CI0103
            11            GX06-GC06K.                                   CI0103
            12            GX06-NPISQ  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX06-ACOTD  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX06-PPOTD  PICTURE  S9(3)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX06-QPSTD  PICTURE  S9(7)V999                CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX06-CPITC  PICTURE  99.                      CI0103
            11            GX06-ITRNB  PICTURE  X.                       CI0103
            11            GX06-FILLER PICTURE  X(14).                   CI0103
            10            GX06-GE98.                                    CI0103
            11            GX06-FILLER PICTURE  X(240).                  CI0103
            10            GX06-GE10                                     CI0103
                          REDEFINES            GX06-GE98.               CI0103
            11            GX06-CDELI  PICTURE  9(3).                    CI0103
            11            GX06-CPAYC  PICTURE  X(2).                    CI0103
            11            GX06-ICHKP  PICTURE  X.                       CI0103
            11            GX06-CLTIN  PICTURE  9(12).                   CI0103
            11            GX06-IFHAI  PICTURE  X.                       CI0103
            11            GX06-CDQUA  PICTURE  X(2).                    CI0103
            11            GX06-FILLER PICTURE  X(07).                   CI0103
            11            GX06-GE99.                                    CI0103
            12            GX06-FILLER PICTURE  X(212).                  CI0103
            11            GX06-GE01                                     CI0103
                          REDEFINES            GX06-GE99.               CI0103
            12            GX06-NTR    PICTURE  9(8).                    CI0103
            12            GX06-GECKD  PICTURE  9.                       CI0103
            12            GX06-NPBN   PICTURE  X(20).                   CI0103
            12            GX06-CCBAT  PICTURE  99.                      CI0103
            12            GX06-CLID4  PICTURE  X(23).                   CI0103
            12            GX06-GENAL1 PICTURE  X(30)                    CI0103
                          OCCURS       002     TIMES.                   CI0103
            12            GX06-GESAD1 PICTURE  X(30)                    CI0103
                          OCCURS       003     TIMES.                   CI0103
            11            GX06-GE02                                     CI0103
                          REDEFINES            GX06-GE99.               CI0103
            12            GX06-GENAL  PICTURE  X(30)                    CI0103
                          OCCURS       002     TIMES.                   CI0103
            12            GX06-GESAD  PICTURE  X(30)                    CI0103
                          OCCURS       003     TIMES.                   CI0103
            11            GX06-GE03                                     CI0103
                          REDEFINES            GX06-GE99.               CI0103
            12            GX06-NCHKN  PICTURE  9(11).                   CI0103
            11            GX06-GE04                                     CI0103
                          REDEFINES            GX06-GE99.               CI0103
            12            GX06-CTIDAP PICTURE  9(3).                    CI0103
            12            GX06-PRCOD  PICTURE  9(5).                    CI0103
            12            GX06-TDELI  PICTURE  X(30).                   CI0103
            12            GX06-CINCD  PICTURE  9(02).                   CI0103
            10            GX06-GE20                                     CI0103
                          REDEFINES            GX06-GE98.               CI0103
            11            GX06-C299.                                    CI0103
            12            GX06-CTID.                                    CI0103
            13            GX06-CTIDA  PICTURE  9(3).                    CI0103
            13            GX06-CTIDN.                                   CI0103
            14            GX06-CTIDNP PICTURE  X(13).                   CI0103
            14            GX06-CTIDND PICTURE  9(11).                   CI0103
            11            GX06-DCACG9 PICTURE  9(8).                    CI0103
            11            GX06-NAASQ  PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            GX06-CIRAP  PICTURE  XX.                      CI0103
            11            GX06-CTYPE  PICTURE  X.                       CI0103
            11            GX06-INACT  PICTURE  X.                       CI0103
            11            GX06-FILLER PICTURE  X(01).                   CI0103
            11            GX06-ITPAC  PICTURE  X.                       CI0103
            11            GX06-ITAXI  PICTURE  X.                       CI0103
            11            GX06-IOWNC  PICTURE  X.                       CI0103
            11            GX06-CDVCD  PICTURE  X(2).                    CI0103
            11            GX06-CTCUS  PICTURE  999.                     CI0103
            11            GX06-CPMTCB PICTURE  X(3).                    CI0103
            11            GX06-CASTC1 PICTURE  99.                      CI0103
            11            GX06-PRCOD1 PICTURE  9(5).                    CI0103
            11            GX06-CPRSC1 PICTURE  X(9).                    CI0103
            11            GX06-CPRTB  PICTURE  X.                       CI0103
            11            GX06-CBRKD  PICTURE  9(4).                    CI0103
            11            GX06-FILLER PICTURE  X(12).                   CI0103
            10            GX06-GE30                                     CI0103
                          REDEFINES            GX06-GE98.               CI0103
            11            GX06-CFIDC  PICTURE  X(5).                    CI0103
            11            GX06-CPHSE  PICTURE  9(2).                    CI0103
            11            GX06-FILLER PICTURE  X(05).                   CI0103
            11            GX06-IABIN  PICTURE  X.                       CI0103
            11            GX06-PDFND  PICTURE  S999V9(3)                CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GX06-GE40                                     CI0103
                          REDEFINES            GX06-GE98.               CI0103
            11            GX06-CACCT  PICTURE  X.                       CI0103
            11            GX06-CPAYR  PICTURE  X(2).                    CI0103
            11            GX06-CDELI1 PICTURE  9(3).                    CI0103
            11            GX06-CATRN.                                   CI0103
            12            GX06-CATRF  PICTURE  9(3).                    CI0103
            12            GX06-CATRS  PICTURE  9(3).                    CI0103
            11            GX06-DEFFT  PICTURE  9(8).                    CI0103
            11            GX06-CTYPC  PICTURE  X.                       CI0103
            11            GX06-CIRAPA PICTURE  XX.                      CI0103
            11            GX06-FILLER PICTURE  X(09).                   CI0103
            11            GX06-GE49.                                    CI0103
            12            GX06-FILLER PICTURE  X(208).                  CI0103
            11            GX06-GE41                                     CI0103
                          REDEFINES            GX06-GE49.               CI0103
            12            GX06-NCHKN1 PICTURE  9(6).                    CI0103
            11            GX06-GE42                                     CI0103
                          REDEFINES            GX06-GE49.               CI0103
            12            GX06-CTID1.                                   CI0103
            13            GX06-CTIDA1 PICTURE  9(3).                    CI0103
            13            GX06-CTIDP1 PICTURE  X(13).                   CI0103
            13            GX06-CTIDN1 PICTURE  9(11).                   CI0103
            11            GX06-GE43                                     CI0103
                          REDEFINES            GX06-GE49.               CI0103
            12            GX06-GENAL2 PICTURE  X(30)                    CI0103
                          OCCURS       002     TIMES.                   CI0103
            12            GX06-GESAD2 PICTURE  X(30)                    CI0103
                          OCCURS       003     TIMES.                   CI0103
            11            GX06-GE44                                     CI0103
                          REDEFINES            GX06-GE49.               CI0103
            12            GX06-CTID01.                                  CI0103
            13            GX06-CTIDA6 PICTURE  9(3).                    CI0103
            13            GX06-NTIDP2 PICTURE  X(13).                   CI0103
            13            GX06-CTIDN2 PICTURE  9(11).                   CI0103
            12            GX06-GECKD2 PICTURE  9.                       CI0103
            12            GX06-PACCT  PICTURE  S999V99                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX06-PLOAN  PICTURE  S999V99                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX06-PADPT  PICTURE  S999V99                  CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GX06-IPCTL  PICTURE  X.                       CI0103
            12            GX06-IPCTP  PICTURE  X.                       CI0103
            12            GX06-CEUNT  PICTURE  S9(5)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GX06-GE31                                     CI0103
                          REDEFINES            GX06-GE98.               CI0103
            11            GX06-GCUSPZ PICTURE  X(12).                   CI0103
       01                 GQ00.                                         CI0103
            02            GQ01.                                         CI0103
            10            GQ01-GELL   PICTURE  9(4)                     CI0103
                          BINARY.                                       CI0103
            10            GQ01-GMISC.                                   CI0103
            11            GQ01-GS00.                                    CI0103
            12            GQ01-GT01.                                    CI0103
            13            GQ01-GQ01K.                                   CI0103
            14            GQ01-CANUMB PICTURE  X(27).                   CI0103
            14            GQ01-CAMCTR PICTURE  9(5).                    CI0103
            14            GQ01-GESQ2  PICTURE  99.                      CI0103
            12            GQ01-GT02                                     CI0103
                          REDEFINES            GQ01-GT01.               CI0103
            13            GQ01-C199.                                    CI0103
            14            GQ01-CLID.                                    CI0103
            15            GQ01-CLIDO  PICTURE  9(3).                    CI0103
            15            GQ01-CLIDN.                                   CI0103
            16            GQ01-CLIDNP PICTURE  X(12).                   CI0103
            16            GQ01-CLIDND PICTURE  9(8).                    CI0103
            12            GQ01-GT03                                     CI0103
                          REDEFINES            GQ01-GT01.               CI0103
            13            GQ01-C299.                                    CI0103
            14            GQ01-CTID.                                    CI0103
            15            GQ01-CTIDA  PICTURE  9(3).                    CI0103
            15            GQ01-CTIDN.                                   CI0103
            16            GQ01-CTIDNP PICTURE  X(13).                   CI0103
            16            GQ01-CTIDND PICTURE  9(11).                   CI0103
            12            GQ01-GT04                                     CI0103
                          REDEFINES            GQ01-GT01.               CI0103
            13            GQ01-NPBN   PICTURE  X(20).                   CI0103
            12            GQ01-GT05                                     CI0103
                          REDEFINES            GQ01-GT01.               CI0103
            13            GQ01-GR98.                                    CI0103
            14            GQ01-GRID.                                    CI0103
            15            GQ01-GRIDC  PICTURE  9(3).                    CI0103
            15            GQ01-GRIDN.                                   CI0103
            16            GQ01-GRIDNP PICTURE  99.                      CI0103
            16            GQ01-GRIDND PICTURE  9(8).                    CI0103
            12            GQ01-GT06                                     CI0103
                          REDEFINES            GQ01-GT01.               CI0103
            13            GQ01-NTR    PICTURE  9(8).                    CI0103
            12            GQ01-GT07                                     CI0103
                          REDEFINES            GQ01-GT01.               CI0103
            13            GQ01-NTRAC  PICTURE  9(14).                   CI0103
            12            GQ01-GT08                                     CI0103
                          REDEFINES            GQ01-GT01.               CI0103
            13            GQ01-NSRAB  PICTURE  9(7).                    CI0103
            13            GQ01-GECKD  PICTURE  9.                       CI0103
            13            GQ01-NBLCK  PICTURE  9(5).                    CI0103
            13            GQ01-CTRID  PICTURE  X(4).                    CI0103
            12            GQ01-GT19                                     CI0103
                          REDEFINES            GQ01-GT01.               CI0103
            13            GQ01-GEOPD2 PICTURE  X(8).                    CI0103
            12            GQ01-CACKD  PICTURE  9.                       CI0103
            12            GQ01-CENTT  PICTURE  X.                       CI0103
            12            GQ01-CADATE PICTURE  X(8).                    CI0103
            12            GQ01-GETIM  PICTURE  S9(7)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12            GQ01-GEOPID PICTURE  X(6).                    CI0103
            12            GQ01-CAUNIT PICTURE  X(4).                    CI0103
            12            GQ01-XTERMI PICTURE  X(08).                   CI0103
            12            GQ01-CAPPL  PICTURE  X(8).                    CI0103
            12            GQ01-CSYS   PICTURE  X(4).                    CI0103
            12            GQ01-NTRSU  PICTURE  999.                     CI0103
            12            GQ01-FILLER PICTURE  X(20).                   CI0103
            11            GQ01-XMISL  PICTURE  X(599).                  CI0103
      *
      ******************************************************************
      **     SEGMENT THAT CONTAINS THE MISC TRAN CERT ADJUSTMENTS      *
      ******************************************************************
      *
      *!WF DSP=GS DSL=GS SEL=42 FOR=I LEV=1 PLT=GS
       01                 GS00.                                         CI0103
          05              GS00-00.                                      CI0103
            10            GS00-GT01.                                    CI0103
            11            GS00-GQ01K.                                   CI0103
            12            GS00-CANUMB PICTURE  X(27).                   CI0103
            12            GS00-CAMCTR PICTURE  9(5).                    CI0103
            12            GS00-GESQ2  PICTURE  99.                      CI0103
            10            GS00-GT02                                     CI0103
                          REDEFINES            GS00-GT01.               CI0103
            11            GS00-C199.                                    CI0103
            12            GS00-CLID.                                    CI0103
            13            GS00-CLIDO  PICTURE  9(3).                    CI0103
            13            GS00-CLIDN.                                   CI0103
            14            GS00-CLIDNP PICTURE  X(12).                   CI0103
            14            GS00-CLIDND PICTURE  9(8).                    CI0103
            10            GS00-GT03                                     CI0103
                          REDEFINES            GS00-GT01.               CI0103
            11            GS00-C299.                                    CI0103
            12            GS00-CTID.                                    CI0103
            13            GS00-CTIDA  PICTURE  9(3).                    CI0103
            13            GS00-CTIDN.                                   CI0103
            14            GS00-CTIDNP PICTURE  X(13).                   CI0103
            14            GS00-CTIDND PICTURE  9(11).                   CI0103
            10            GS00-GT04                                     CI0103
                          REDEFINES            GS00-GT01.               CI0103
            11            GS00-NPBN   PICTURE  X(20).                   CI0103
            10            GS00-GT05                                     CI0103
                          REDEFINES            GS00-GT01.               CI0103
            11            GS00-GR98.                                    CI0103
            12            GS00-GRID.                                    CI0103
            13            GS00-GRIDC  PICTURE  9(3).                    CI0103
            13            GS00-GRIDN.                                   CI0103
            14            GS00-GRIDNP PICTURE  99.                      CI0103
            14            GS00-GRIDND PICTURE  9(8).                    CI0103
            10            GS00-GT06                                     CI0103
                          REDEFINES            GS00-GT01.               CI0103
            11            GS00-NTR    PICTURE  9(8).                    CI0103
            10            GS00-GT07                                     CI0103
                          REDEFINES            GS00-GT01.               CI0103
            11            GS00-NTRAC  PICTURE  9(14).                   CI0103
            10            GS00-GT08                                     CI0103
                          REDEFINES            GS00-GT01.               CI0103
            11            GS00-NSRAB  PICTURE  9(7).                    CI0103
            11            GS00-GECKD  PICTURE  9.                       CI0103
            11            GS00-NBLCK  PICTURE  9(5).                    CI0103
            11            GS00-CTRID  PICTURE  X(4).                    CI0103
            10            GS00-GT19                                     CI0103
                          REDEFINES            GS00-GT01.               CI0103
            11            GS00-GEOPD2 PICTURE  X(8).                    CI0103
            10            GS00-CACKD  PICTURE  9.                       CI0103
            10            GS00-CENTT  PICTURE  X.                       CI0103
            10            GS00-CADATE PICTURE  X(8).                    CI0103
            10            GS00-GETIM  PICTURE  S9(7)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GS00-GEOPID PICTURE  X(6).                    CI0103
            10            GS00-CAUNIT PICTURE  X(4).                    CI0103
            10            GS00-XTERMI PICTURE  X(08).                   CI0103
            10            GS00-CAPPL  PICTURE  X(8).                    CI0103
            10            GS00-CSYS   PICTURE  X(4).                    CI0103
            10            GS00-NTRSU  PICTURE  999.                     CI0103
            10            GS00-FILLER PICTURE  X(20).                   CI0103
          05              GS00-SUITE.                                   CI0103
            15       FILLER         PICTURE  X(00042).                  CI0103
       01                 GS42  REDEFINES      GS00.                    CI0103
            10       FILLER         PICTURE  X(00101).                  CI0103
            10            GS42-CTDIDA PICTURE  9(12).                   CI0103
            10            GS42-ATDVS  PICTURE  S9(8)V9(5)               CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GS42-ACASH  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            GS42-CAMTY  PICTURE  XX.                      CI0103
            10            GS42-CATRN  PICTURE  9(6).                    CI0103
            10            GS42-CTDDI  PICTURE  9(8).                    CI0103
            10            GS42-ILIBI  PICTURE  X.                       CI0103
      *
      *
      *
      *
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
       01                 NS00.                                         CI0103
          05              NS00-00.                                      CI0103
            10            NS00-NS00K.                                   CI0103
            11            NS00-PRCSTK PICTURE  XX.                      CI0103
          05              NS00-SUITE.                                   CI0103
            15       FILLER         PICTURE  X(00078).                  CI0103
       01                 NS20  REDEFINES      NS00.                    CI0103
            10       FILLER         PICTURE  X(00002).                  CI0103
            10            NS20-DCACG  PICTURE  9(8).                    CI0103
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            NS20-CCDAT  PICTURE  X(8).                    CI0103
            10            NS20-DCALP  PICTURE  X(12).                   CI0103
            10            NS20-DNACG  PICTURE  9(8).                    CI0103
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            NS20-CNDAT  PICTURE  X(8).                    CI0103
            10            NS20-DNALP  PICTURE  X(12).                   CI0103
            10            NS20-DCACD  PICTURE  X(10).                   CI0103
            10            NS20-FILLER PICTURE  X(4).                    CI0103
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
                                                                        AM0033
      ******************************************************************AM0033
      **     PCB ADDRESS LIST FOR CI0033.  MODULE CI0033 WILL NEED     *AM0033
      **     PCB'S FOR:                                                *AM0033
      **                SHARK PRODUCT RULES DATABASE(SBUP)             *AM0033
      **                SHARK PRODUCT RULES DATABASE(SCOP)             *AM0033
      **                SHARK PRODUCT RULES DATABASE(SSSP)             *AM0033
      **                CONTRACT DATABASE(CT1P)                        *AM0033
      ******************************************************************AM0033
                                                                        AM0033
       01  CI0033-PCB-ADDRESS-LIST.                                     AM0033
           05  CI0033-PCB-SBUP-PTR1   POINTER.                          AM0033
           05  CI0033-PCB-SCOP-PTR1   POINTER.                          AM0033
           05  CI0033-PCB-SSPP-PTR1   POINTER.
           05  CI0033-PCB-CT1P-PTR1   POINTER.                          AM0033
      *>>>>>> UNIQUE SEGMENTS TO THIS ROUTINE
      *-----------------------------------------------------------------AAER85
      *WORKING STORAGE VARIABLES REQUIRED FOR THE BROKER CALL ERROR     AAER85
      *HANDLING MACRO                                                   AAER85
      *-----------------------------------------------------------------AAER85
      *                                                                 AAER85
      *LENGTH OF THE FIXED AREA OF THE RESPONSE FROM DBI5000N           AAER85
      *!WI pl=SQ040                                                     AAER85
       01               WS-FIX-GELL    VALUE ZEROS                      AAER85
                        PICTURE 9(4)                                    CI0103
                          BINARY.                                       CI0103
      *                                                                 AAER85
      *LENGTH OF THE VARIABLE AREA OF THE RESPONSE FROM DBI5000N        AAER85
      *!WI pl=SQ055                                                     AAER85
       01               WS-VAR-GELL    VALUE ZEROS                      AAER85
                        PICTURE 9(4)                                    CI0103
                          BINARY.                                       CI0103
      *                                                                 AAER85
      *CALCULATED POSITION OF THE ERROR CODE IN THE RESPONSE SECTION    AAER85
      *!WI pl=SQ070                                                     AAER85
       01               WS-ERR-GELL    VALUE ZEROS                      AAER85
                        PICTURE 9(4)                                    CI0103
                          BINARY.                                       CI0103
      *                                                                 AAER85
      *TOTAL NUMBER OF ROWS RETURNED IN THE VIEW RESPONSE.              AAER85
      *!WI pl=SQ085                                                     AAER85
       01               WS-NRURO       VALUE ZEROS                      AAER85
                        PICTURE 9(3).                                   CI0103
      *                                                                 AAER85
      *ERROR CODE SET BY THE MACRO.                                     AAER85
      *!WI pl=SQ100                                                     AAER85
       01               WS00-IERRC       VALUE SPACES                   AAER85
                        PICTURE X.                                      CI0103
      *                                                                 AAER85
      *SEVERITY CODE SET BY THE MACRO.                                  AAER85
      *!WI pl=SQ115                                                     AAER85
       01               WS00-CSEVR       VALUE SPACES                   AAER85
                        PICTURE X.                                      CI0103
      *                                                                 AAER85
      *ERROR CODE FOR THE NON CRITICAL ERRORS NEED TO IGNORE.           AAER85
      *!WI pl=SQ130                                                     AAER85
       01               WS01-IERRC       VALUE SPACES                   AAER85
                        PICTURE X.                                      CI0103
      *                                                                 AAER85
      *ARRAY FOR STORING THE LIST OF THE ERROR CODES NEED TO IGNORE     AAER85
       01                 WE00-ERROR-TABLE.                             AAER85
         05               WE00            OCCURS 25 TIMES.              AAER85
      *!WI pl=SQ155                                                     AAER85
           10             WE00-CERRE1     VALUE SPACES                  AAER85
                        PICTURE X(5).                                   CI0103
      *                                                                 AAER85
      *VARIABLE FOR STORING THE TYPE OF ERROR HANDLING REQUIRED         AAER85
       01               WS00-ERROR-TYPE  PIC X(1)  VALUE SPACES.        AAER85
      *                                                                 AAER85
      *VARIABLE FOR STORING THE ERROR CODE                              AAER85
      *!WI pl=SQ185                                                     AAER85
       01               WS00-CERRE1      VALUE SPACES                   AAER85
                        PICTURE X(5).                                   CI0103
      *VARIABLE FOR STORING IGNORE ERROR INDICATOR                      AAER85
       01               WS00-IGNORE-ERROR PIC X VALUE 'N'.              AAER85
      *VARIABLE FOR STORING THE AREA OF THE ERROR.                      AAER85
       01               WS00-ERROR-AREA   PIC X VALUE SPACES.           AAER85
      ******************************************************************AAER85
      *INPUT AND OUTPUT SEGMENTS TO THE DST VIEWS FOR CALLING DBI5000N.*AAER85
      ******************************************************************AAER85
      *                  ACCOUNT VIEW - 2933                            AAER85
      ******************************************************************AAER85
      *          SQ1A    -    ACCOUNT INFO VIEW REQUEST                *AAER85
      *          SQ5A    -    ACCOUNT INFO VIEW RESPONSE               *AAER85
      ******************************************************************AAER85
      **                   GROUP VIEW - 2939                           *AAER85
      ******************************************************************AAER85
      *          SQ1G    -    GROUP INFO VIEW REQUEST                  *AAER85
      *          SQ5G    -    GROUP INFO VIEW RESPONSE                 *AAER85
      ******************************************************************AAER85
      *                 TRANSACTION HISTORY VIEWS                      *AAER85
      *       SINGLE TRANSACTION VIEW - 2934                           *AAER85
      *         LIST TRANSACTION VIEW - 2935                           *AAER85
      *   ASSOCIATED TRANSACTION VIEW - 2936                           *AAER85
      ******************************************************************AAER85
      *          SQ1S    -    SINGLE TRAN INFO VIEW REQUEST            *AAER85
      *          SQ5S    -    SINGLE TRAN INFO VIEW RESPONSE           *AAER85
      ******************************************************************AAER85
      *          SQ1C    -    LIST TRAN INFO VIEW REQUEST              *AAER85
      *          SQ5C    -    LIST TRAN INFO VIEW RESPONSE             *AAER85
      ******************************************************************AAER85
      *          SQ1R    -    ASSOCIATED TRAN INFO VIEW REQUEST       * AAER85
      *          SQ5R    -    ASSOCIATED TRAN INFO VIEW RESPONSE      * AAER85
      ***************************************************************** AAER85
      *                  CALCULATIONS  VIEW                           * AAER85
      *          ACCOUNT VALUE VIEW - 2949                            * AAER85
      *             GOOD FUNDS VIEW - 2940                            * AAER85
      ***************************************************************** AAER85
      *          SQ1E    -    ACCOUNT VALUE VIEW REQUEST              * AAER85
      *          SQ5E    -    ACCOUNT VALUE VIEW RESPONSE             * AAER85
      ***************************************************************** AAER85
      *          SQ1F    -    GOOF FUNDS VIEW REQUEST                 * AAER85
      *          SQ5F    -    GOOD FUNDS VIEW RESPONSE                * AAER85
      ***************************************************************** AAER85
      *                      CDSC   VIEWS                             * AAER85
      *                    FJX8 VIEW - 2903                           * AAER85
      *                    FJXC VIEW - 2907                           * AAER85
      ***************************************************************** AAER85
      *          SQ1Q    -    FJX8 HYPO CALC VIEW REQUEST             * AAER85
      *          SQ5Q    -    FJX8 HYPO CALC VIEW RESPONSE            * AAER85
      ***************************************************************** AAER85
      *          SQ1X    -    FJXC PAF HYPO CALC VIEW REQUEST         * AAER85
      *          SQ5X    -    FJXC PAF HYPO CALC VIEW RESPONSE        * AAER85
      ***************************************************************** AAER85
      *          SQ1T    -    TAX TRAN INFO VIEW REQUEST              * AAER85
      *          SQ5T    -    TAX TRAN INFO VIEW RESPONSE             * AAER85
      ***************************************************************** AAER85
      *          MCB ACCOUNT INFORMATION VIEW - 4838                  * AAER85
      ***************************************************************** AAER85
      *          SQ1Z    -    MCB ACCOUNT INFORMATION VIEW REQUEST    * AAER85
      *          SQ5Z    -    MCB ACCOUNT INFORMATION VIEW RESPONSE   * AAER85
      ***************************************************************** AAER85
      *          MCB GAIN LOSS INFORMATION VIEW 4865                  * AAER85
      ***************************************************************** AAER85
      *          SQ2A    -    MCB GAIN/LOSS INFORMATION VIEW REQUEST  * AAER85
      *          SQ6Z    -    MCB GAIN/LOSS INFORMATION VIEW RESPONSE * AAER85
      ***************************************************************** AAER85
      *!WF DSP=SQ DSL=SQ SEL=1F5F FOR=I DES=2 LEV=1                     AAER85
      ***************************************************************** AAER85
      *THIS MACRO INCLUDES THE I/O SEGMENTS FOR 2 VIEWS ONLY. IF THE    AAER85
      *PROGRAM CALLS MORE THAN 2 VIEWS, ADD THEM MANUALLY AFTER LINE 940AAER85
      ***************************************************************** AAER85
      ******************************************************************ADUTAB
      **              TABLE TF09 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TF09-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TF DSL=TF SEL=09 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TF09.                                                CI0103
           04    G-TF09-PARAM.                                          CI0103
             10  G-TF09-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0103
                        VALUE      +142.                                CI0103
             10  G-TF09-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0103
                        VALUE      +001.                                CI0103
             10  G-TF09-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0103
                        VALUE      +005.                                CI0103
             10  G-TF09-NUAPP  PICTURE 99                               CI0103
                        VALUE       0.                                  CI0103
             10  G-TF09-NUTAB  PICTURE X(6)                             CI0103
                        VALUE 'TF0009'.                                 CI0103
             10  G-TF09-TABFO  PICTURE XX                 VALUE SPACE.  CI0103
             10  G-TF09-TABCR  PICTURE XX                 VALUE SPACE.  CI0103
             10  G-TF09-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0103
             10  G-TF09-NUSSC  PICTURE X  VALUE   ' '.                  CI0103
             10  G-TF09-NUSSY  PICTURE X                  VALUE SPACE.  CI0103
             10  G-TF09-TRANID PICTURE X(4)               VALUE SPACE.  CI0103
             10  G-TF09-FILSYS.                                         CI0103
             15  G-TF09-USERC  PICTURE X(6)               VALUE SPACE.  CI0103
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0103
           04             TF09.                                         CI0103
            10            TF09-CERRE2 PICTURE  X(5)                     CI0103
                          VALUE                SPACE.                   CI0103
            10            TF09-CERRE3 PICTURE  X(5)                     CI0103
                          VALUE                SPACE.                   CI0103
            10            TF09-TERMT  PICTURE  X(66)                    CI0103
                          VALUE                SPACE.                   CI0103
            10            TF09-TERMT3 PICTURE  X(66)                    CI0103
                          VALUE                SPACE.                   CI0103
      **                                                                ADUTAB
      *-----------------------------------------------------------------
      **   WORK FIELDS
      *-----------------------------------------------------------------
       01  7-DATA-POINTER.
           05  7-ADDR-DL1UIB       USAGE IS POINTER.
      *
       01  WORK-FIELDS.

      *    * SYSTEM DATE (CCYYMMDD)
      *!WI
           05  7-CURR-DCACG
                        PICTURE 9(8).                                   CI0103

      *    * TWO WEEK AGO GREG DATE
      *!WI
           05  7-2WK-DCACG
                        PICTURE 9(8).                                   CI0103

      *    * TWO WEEK AGO GREG 9COMP DATE
      *!WI
           05  7-2WK-DCACG9
                        PICTURE 9(8).                                   CI0103

      *    * GC03 ENTRY DATE CONVERTED TO GREG FROM GREG 9COMP
      *!WI
           05  7-GX03-DCACG
                        PICTURE 9(8).                                   CI0103

      *    * WORK FIELD USED TO CONVERT 9COMP
           05  7-WS-9DAT8       PIC 9(08) VALUE 99999999.

      *    * WORK DOLLARS USED AS TEMP HOLDER FOR PENDING WITHDRAWALS
           05  7-PEND-DOLLARS   PIC S9(11)V99 COMP-3 VALUE ZEROS.

      *    * WORK QUANTITY OF CERT SHARES ISSUED
      *!WI
           05  7-WORK-QCSHIS
                        PICTURE S9(9)V999                               CI0103
                          COMPUTATIONAL-3.                              CI0103
      *
      *>>> FIELDS USED TO ISOLATE SPECIFIC VALUES <<<
      ** TRAN CODES FOR PENDING WITHDRAWAL *
       01  7-VALUE-FIELDS.
      *!WI
           05  7-GC03-CATRNC
                        PICTURE 9(6).                                   CI0103
               88  PEND-WITHDR-TRAN
                   VALUE  102000, 122000, 124000, 124001, 142000,
                          151000, 151001, 161001, 161002, 161003,
                          171001, 171002, 200000, 200001, 200002,
                          210001, 210002, 210003, 210004.
      *
      ** TRAN CODES FOR PENDING PAYMENTS AVAILABLE *
               88  PEND-PAY-TRAN
                   VALUE  101000, 121000, 123000, 141000, 153000,
                          153001, 163001, 163002, 163003, 173001,
                          173002, 180000, 190000, 190001, 190002,
                          230000.
      *
      *!WI
           05  7-GX06-CTYPE
                        PICTURE X.                                      CI0103
               88  VALID-TRANSF-TYPE         VALUE 'E' 'T' 'D'.
      *
      *!WI
           05  7-GC03-CCOLL
                        PICTURE X(3).                                   CI0103
               88  AVAIL-CALL-TYPE      VALUE 'CER' 'INS' 'IDS' 'WIR'.
               88  XAVAIL-CALL-TYPE     VALUE 'PER' 'PAC' 'ACH' 'CHG'.
      *
      *!WI
           05  7-GC03-CACTO
                        PICTURE 9(3).                                   CI0103
               88  OK-DUP-ORIG-CD      VALUE 001, 014, 020.
      *
      *!WI
           05  7-GC03-CTRTP
                        PICTURE X(2).                                   CI0103
               88  OK-RECENT-ENTRY-CD  VALUE 'S ' 'DR'.
      *
      *!WI
           05  7-GC03-CSYST
                        PICTURE 99.                                     CI0103
               88  OK-RECENT-SOURCE-CD VALUE 11, 13, 15, 20.
      *
      *!WI
           05  7-GC01-PRCOD
                        PICTURE 9(5).                                   CI0103
               88  VALID-PRODUCT-CD    VALUE 13, 16, 167.
      *
      *>>> SWITCHES <<<
      *
      ** THIS CONTROLS ACCUMULATION OF PENDING WITHDRAWAL FOR DISPLAY *
       01  7-MAX-VALUE         PIC X(1)  VALUE 'N'.
           88  MAX-VALUE                 VALUE 'Y'.
           88  NOT-MAX-VALUE             VALUE 'N'.
      *
       01  7-PEND-PAY-FOUND    PIC X(1)  VALUE 'N'.
           88  PEND-PAY-FOUND            VALUE 'Y'.
           88  PEND-PAY-NOTFOUND         VALUE 'N'.
      *
      ***************************************************
      *WORKING STORAGE FOR USER ID.
      ***************************************************
      *!WI
       01       WS00-NCUSR2     VALUE SPACES
                        PICTURE X(12).                                  CI0103
       01   DEBUT-WSS.                                                  CI0103
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0103
            05   IK     PICTURE X.                                      CI0103
       01  CONSTANTES-PAC.                                              CI0103
           05  FILLER  PICTURE X(87)   VALUE                            CI0103
                     '6015 CAT09/08/14CI0103ADMIN   14:34:46CI0103P AMERCI0103
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0103
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0103
           05  NUGNA   PICTURE X(5).                                    CI0103
           05  APPLI   PICTURE X(3).                                    CI0103
           05  DATGN   PICTURE X(8).                                    CI0103
           05  PROGR   PICTURE X(6).                                    CI0103
           05  CODUTI  PICTURE X(8).                                    CI0103
           05  TIMGN   PICTURE X(8).                                    CI0103
           05  PROGE   PICTURE X(8).                                    CI0103
           05  COBASE  PICTURE X(4).                                    CI0103
           05  DATGNC  PICTURE X(10).                                   CI0103
           05  RELEAS  PICTURE X(7).                                    CI0103
           05  DATGE   PICTURE X(10).                                   CI0103
           05  DATSQ   PICTURE X(10).                                   CI0103
       01  DATCE.                                                       CI0103
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0103
         05  DATOR.                                                     CI0103
           10  DATOA  PICTURE XX.                                       CI0103
           10  DATOM  PICTURE XX.                                       CI0103
           10  DATOJ  PICTURE XX.                                       CI0103
       01   VARIABLES-CONDITIONNELLES.                                  CI0103
            05                  FT      PICTURE X VALUE '0'.            CI0103
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0103
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0103
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU070
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           IWE00L PICTURE S9(4) VALUE  ZERO.              AAER85
            05           IWE00R PICTURE S9(4) VALUE  ZERO.              AAER85
            05           IWE00M PICTURE S9(4) VALUE +0025.              AAER85
            05           J55FGR PICTURE S9(4) VALUE  ZERO.
            05           J97FLR PICTURE S9(4) VALUE  ZERO.              AAER85
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0103
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0103
            05       5-GQ00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0103
            05       5-GX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0103
       01               S-CT01-SSA.                                     CI0103
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0103
                                      VALUE 'CT01    '.                 CI0103
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0103
            10          S-CT01-CCOD   PICTURE X(5)                      CI0103
                                      VALUE '-----'.                    CI0103
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0103
       01            S-CTU01-SSA.                                       CI0103
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'CT01    '.                 CI0103
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0103
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(CT01K'.                   CI0103
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0103
            10       S-CTU01-CT01K.                                     CI0103
            11       S-CTU01-C299.                                      CI0103
            12       S-CTU01-CTID.                                      CI0103
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0103
            13       S-CTU01-CTIDN.                                     CI0103
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0103
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0103
            10  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01               S-GQ01-SSA.                                     CI0103
            10         S1-GQ01-SEGNAM PICTURE X(8)                      CI0103
                                      VALUE 'GQ01    '.                 CI0103
            10         S1-GQ01-CCOM   PICTURE X VALUE '*'.              CI0103
            10          S-GQ01-CCOD   PICTURE X(5)                      CI0103
                                      VALUE '-----'.                    CI0103
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0103
       01            S-GQU01-SSA.                                       CI0103
            13      S1-GQU01-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GQ01    '.                 CI0103
            13      S1-GQU01-CCOM   PICTURE X VALUE '*'.                CI0103
            13       S-GQU01-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            13      S1-GQU01-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(GQ01K'.                   CI0103
            13       S-GQU01-OPER  PICTURE XX VALUE ' ='.               CI0103
            13       S-GQU01-GQ01K.                                     CI0103
            14       S-GQU01-CANUMB   PICTURE  X(27).                   CI0103
            14       S-GQU01-CAMCTR   PICTURE  9(5).                    CI0103
            14       S-GQU01-GESQ2    PICTURE  99.                      CI0103
            13  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GQ701-SSA.                                       CI0103
            14      S1-GQ701-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GQ01    '.                 CI0103
            14      S1-GQ701-CCOM   PICTURE X VALUE '*'.                CI0103
            14       S-GQ701-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            14      S1-GQ701-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(XCANUMB'.                 CI0103
            14       S-GQ701-OPER  PICTURE XX VALUE ' ='.               CI0103
            14       S-GQ701-CANUMB   PICTURE  X(27).                   CI0103
            14  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01               S-GX01-SSA.                                     CI0103
            10         S1-GX01-SEGNAM PICTURE X(8)                      CI0103
                                      VALUE 'GC01    '.                 CI0103
            10         S1-GX01-CCOM   PICTURE X VALUE '*'.              CI0103
            10          S-GX01-CCOD   PICTURE X(5)                      CI0103
                                      VALUE '-----'.                    CI0103
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0103
       01            S-GXU01-SSA.                                       CI0103
            10      S1-GXU01-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC01    '.                 CI0103
            10      S1-GXU01-CCOM   PICTURE X VALUE '*'.                CI0103
            10       S-GXU01-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            10      S1-GXU01-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(GC01K'.                   CI0103
            10       S-GXU01-OPER  PICTURE XX VALUE ' ='.               CI0103
            10       S-GXU01-GC01K.                                     CI0103
            11       S-GXU01-C299.                                      CI0103
            12       S-GXU01-CTID.                                      CI0103
            13       S-GXU01-CTIDA    PICTURE  9(3).                    CI0103
            13       S-GXU01-CTIDN.                                     CI0103
            14       S-GXU01-CTIDNP   PICTURE  X(13).                   CI0103
            14       S-GXU01-CTIDND   PICTURE  9(11).                   CI0103
            10  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01               S-GX03-SSA.                                     CI0103
            10         S1-GX03-SEGNAM PICTURE X(8)                      CI0103
                                      VALUE 'GC03    '.                 CI0103
            10         S1-GX03-CCOM   PICTURE X VALUE '*'.              CI0103
            10          S-GX03-CCOD   PICTURE X(5)                      CI0103
                                      VALUE '-----'.                    CI0103
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0103
       01            S-GXA03-SSA.                                       CI0103
            11      S1-GXA03-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GXA03-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXA03-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXA03-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(CAATY'.                   CI0103
            11       S-GXA03-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXA03-CAATY    PICTURE  9(3).                    CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GXB03-SSA.                                       CI0103
            11      S1-GXB03-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GXB03-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXB03-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXB03-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(CVSYS'.                   CI0103
            11       S-GXB03-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXB03-CVSYS    PICTURE  X(2).                    CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GXC03-SSA.                                       CI0103
            11      S1-GXC03-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GXC03-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXC03-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXC03-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(CASTC'.                   CI0103
            11       S-GXC03-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXC03-CASTC    PICTURE  99.                      CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GXD03-SSA.                                       CI0103
            11      S1-GXD03-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GXD03-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXD03-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXD03-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(CACTO'.                   CI0103
            11       S-GXD03-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXD03-CACTO    PICTURE  9(3).                    CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GXE03-SSA.                                       CI0103
            11      S1-GXE03-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GXE03-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXE03-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXE03-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(IPULL'.                   CI0103
            11       S-GXE03-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXE03-IPULL    PICTURE  X.                       CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GXF03-SSA.                                       CI0103
            11      S1-GXF03-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GXF03-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXF03-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXF03-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(DTRAC'.                   CI0103
            11       S-GXF03-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXF03-DTRAC    PICTURE  9(8).                    CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GXG03-SSA.                                       CI0103
            11      S1-GXG03-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GXG03-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXG03-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXG03-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(CTRSO'.                   CI0103
            11       S-GXG03-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXG03-CTRSO    PICTURE  9(02).                   CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GXH03-SSA.                                       CI0103
            11      S1-GXH03-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GXH03-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXH03-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXH03-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(NTRCE'.                   CI0103
            11       S-GXH03-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXH03-NTRCE    PICTURE  9(06).                   CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GXI03-SSA.                                       CI0103
            11      S1-GXI03-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GXI03-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXI03-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXI03-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(ITRAN'.                   CI0103
            11       S-GXI03-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXI03-ITRAN    PICTURE  X.                       CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GXJ03-SSA.                                       CI0103
            11      S1-GXJ03-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GXJ03-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXJ03-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXJ03-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(DEFFT'.                   CI0103
            11       S-GXJ03-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXJ03-DEFFT    PICTURE  9(8).                    CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GXK03-SSA.                                       CI0103
            11      S1-GXK03-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GXK03-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXK03-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXK03-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(CPMTCA'.                  CI0103
            11       S-GXK03-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXK03-CPMTCA   PICTURE  XXX.                     CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GXL03-SSA.                                       CI0103
            11      S1-GXL03-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GXL03-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXL03-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXL03-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(ACASH'.                   CI0103
            11       S-GXL03-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXL03-ACASH    PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GXN03-SSA.                                       CI0103
            11      S1-GXN03-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GXN03-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXN03-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXN03-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(CRREV'.                   CI0103
            11       S-GXN03-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXN03-CRREV    PICTURE  X(3).                    CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GXO03-SSA.                                       CI0103
            11      S1-GXO03-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GXO03-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXO03-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXO03-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(CSYST'.                   CI0103
            11       S-GXO03-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXO03-CSYST    PICTURE  99.                      CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GXU03-SSA.                                       CI0103
            11      S1-GXU03-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GXU03-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXU03-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXU03-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(GC03K'.                   CI0103
            11       S-GXU03-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXU03-GC03K.                                     CI0103
            12       S-GXU03-DCACG9   PICTURE  9(8).                    CI0103
            12       S-GXU03-NAASQ    PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GX103-SSA.                                       CI0103
            12      S1-GX103-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            12      S1-GX103-CCOM   PICTURE X VALUE '*'.                CI0103
            12       S-GX103-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            12      S1-GX103-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(XDCACG9'.                 CI0103
            12       S-GX103-OPER  PICTURE XX VALUE ' ='.               CI0103
            12       S-GX103-DCACG9   PICTURE  9(8).                    CI0103
            12  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GX203-SSA.                                       CI0103
            11      S1-GX203-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GX203-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GX203-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GX203-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(XGEAUN'.                  CI0103
            11       S-GX203-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GX203-GEAUN    PICTURE  9(5).                    CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GX303-SSA.                                       CI0103
            11      S1-GX303-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GX303-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GX303-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GX303-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(XGEOPD2'.                 CI0103
            11       S-GX303-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GX303-GEOPD2   PICTURE  X(8).                    CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GX403-SSA.                                       CI0103
            11      S1-GX403-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            11      S1-GX403-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GX403-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GX403-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(XNBTCH'.                  CI0103
            11       S-GX403-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GX403-NBTCH    PICTURE  9(4).                    CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GX803-SSA.                                       CI0103
            12      S1-GX803-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC03    '.                 CI0103
            12      S1-GX803-CCOM   PICTURE X VALUE '*'.                CI0103
            12       S-GX803-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            12      S1-GX803-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(XNAASQ'.                  CI0103
            12       S-GX803-OPER  PICTURE XX VALUE ' ='.               CI0103
            12       S-GX803-NAASQ    PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            12  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01               S-GX06-SSA.                                     CI0103
            10         S1-GX06-SEGNAM PICTURE X(8)                      CI0103
                                      VALUE 'GC06    '.                 CI0103
            10         S1-GX06-CCOM   PICTURE X VALUE '*'.              CI0103
            10          S-GX06-CCOD   PICTURE X(5)                      CI0103
                                      VALUE '-----'.                    CI0103
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0103
       01            S-GXF06-SSA.                                       CI0103
            11      S1-GXF06-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC06    '.                 CI0103
            11      S1-GXF06-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXF06-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXF06-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(PRCOD1'.                  CI0103
            11       S-GXF06-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXF06-PRCOD1   PICTURE  9(5).                    CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01            S-GXU06-SSA.                                       CI0103
            11      S1-GXU06-SEGNAM PICTURE X(8)                        CI0103
                                      VALUE 'GC06    '.                 CI0103
            11      S1-GXU06-CCOM   PICTURE X VALUE '*'.                CI0103
            11       S-GXU06-CCOD   PICTURE X(5)                        CI0103
                                      VALUE '-----'.                    CI0103
            11      S1-GXU06-FLDNAM PICTURE X(9)                        CI0103
                                      VALUE '(GC06K'.                   CI0103
            11       S-GXU06-OPER  PICTURE XX VALUE ' ='.               CI0103
            11       S-GXU06-GC06K.                                     CI0103
            12       S-GXU06-NPISQ    PICTURE  S9(3)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11  FILLER   PICTURE X    VALUE ')'.                        CI0103
       01   ZONES-UTILISATEUR PICTURE X.                                CI0103
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
      ** PCB POINTER FOR SBUP                                           ADU015
            05 PCB-SBUP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR SCOP                                           ADU015
            05 PCB-SCOP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR SSPP                                           ADU015
            05 PCB-SSPP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR ACAP                                           ADU015
            05 PCB-ACAP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR TR1P                                           ADU015
            05 PCB-TR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR SBUP                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0103
          05              PB00-SUITE.                                   CI0103
            15       FILLER         PICTURE  X(00106).                  CI0103
       01                 PB06  REDEFINES      PB00.                    CI0103
            10            PB06-XDBPCB.                                  CI0103
            11            PB06-XDBDNM PICTURE  X(08).                   CI0103
            11            PB06-XSEGLV PICTURE  X(02).                   CI0103
            11            PB06-XRC    PICTURE  X(02).                   CI0103
            11            PB06-XPROPT PICTURE  X(04).                   CI0103
            11            PB06-FILLER PICTURE  S9(5)                    CI0103
                          BINARY.                                       CI0103
            11            PB06-XSEGNM PICTURE  X(08).                   CI0103
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0103
                          BINARY.                                       CI0103
            11            PB06-XSEGNB PICTURE  9(05)                    CI0103
                          BINARY.                                       CI0103
            11            PB06-XCOKEY PICTURE  X(70).                   CI0103
      *** PCB MASK FOR SCOP                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0103
          05              PC00-SUITE.                                   CI0103
            15       FILLER         PICTURE  X(00106).                  CI0103
       01                 PC06  REDEFINES      PC00.                    CI0103
            10            PC06-XDBPCB.                                  CI0103
            11            PC06-XDBDNM PICTURE  X(08).                   CI0103
            11            PC06-XSEGLV PICTURE  X(02).                   CI0103
            11            PC06-XRC    PICTURE  X(02).                   CI0103
            11            PC06-XPROPT PICTURE  X(04).                   CI0103
            11            PC06-FILLER PICTURE  S9(5)                    CI0103
                          BINARY.                                       CI0103
            11            PC06-XSEGNM PICTURE  X(08).                   CI0103
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0103
                          BINARY.                                       CI0103
            11            PC06-XSEGNB PICTURE  9(05)                    CI0103
                          BINARY.                                       CI0103
            11            PC06-XCOKEY PICTURE  X(70).                   CI0103
      *** PCB MASK FOR SSPP                                             ADU015
      *!WF DSP=PE DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PE00.                                         CI0103
          05              PE00-SUITE.                                   CI0103
            15       FILLER         PICTURE  X(00106).                  CI0103
       01                 PE06  REDEFINES      PE00.                    CI0103
            10            PE06-XDBPCB.                                  CI0103
            11            PE06-XDBDNM PICTURE  X(08).                   CI0103
            11            PE06-XSEGLV PICTURE  X(02).                   CI0103
            11            PE06-XRC    PICTURE  X(02).                   CI0103
            11            PE06-XPROPT PICTURE  X(04).                   CI0103
            11            PE06-FILLER PICTURE  S9(5)                    CI0103
                          BINARY.                                       CI0103
            11            PE06-XSEGNM PICTURE  X(08).                   CI0103
            11            PE06-XKEYLN PICTURE  S9(05)                   CI0103
                          BINARY.                                       CI0103
            11            PE06-XSEGNB PICTURE  9(05)                    CI0103
                          BINARY.                                       CI0103
            11            PE06-XCOKEY PICTURE  X(70).                   CI0103
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PF DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PF00.                                         CI0103
          05              PF00-SUITE.                                   CI0103
            15       FILLER         PICTURE  X(00106).                  CI0103
       01                 PF06  REDEFINES      PF00.                    CI0103
            10            PF06-XDBPCB.                                  CI0103
            11            PF06-XDBDNM PICTURE  X(08).                   CI0103
            11            PF06-XSEGLV PICTURE  X(02).                   CI0103
            11            PF06-XRC    PICTURE  X(02).                   CI0103
            11            PF06-XPROPT PICTURE  X(04).                   CI0103
            11            PF06-FILLER PICTURE  S9(5)                    CI0103
                          BINARY.                                       CI0103
            11            PF06-XSEGNM PICTURE  X(08).                   CI0103
            11            PF06-XKEYLN PICTURE  S9(05)                   CI0103
                          BINARY.                                       CI0103
            11            PF06-XSEGNB PICTURE  9(05)                    CI0103
                          BINARY.                                       CI0103
            11            PF06-XCOKEY PICTURE  X(70).                   CI0103
      *** PCB MASK FOR ACAP                                             ADU015
      *!WF DSP=PG DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PG00.                                         CI0103
          05              PG00-SUITE.                                   CI0103
            15       FILLER         PICTURE  X(00106).                  CI0103
       01                 PG06  REDEFINES      PG00.                    CI0103
            10            PG06-XDBPCB.                                  CI0103
            11            PG06-XDBDNM PICTURE  X(08).                   CI0103
            11            PG06-XSEGLV PICTURE  X(02).                   CI0103
            11            PG06-XRC    PICTURE  X(02).                   CI0103
            11            PG06-XPROPT PICTURE  X(04).                   CI0103
            11            PG06-FILLER PICTURE  S9(5)                    CI0103
                          BINARY.                                       CI0103
            11            PG06-XSEGNM PICTURE  X(08).                   CI0103
            11            PG06-XKEYLN PICTURE  S9(05)                   CI0103
                          BINARY.                                       CI0103
            11            PG06-XSEGNB PICTURE  9(05)                    CI0103
                          BINARY.                                       CI0103
            11            PG06-XCOKEY PICTURE  X(70).                   CI0103
      *** PCB MASK FOR TR1P                                             ADU015
      *!WF DSP=PH DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PH00.                                         CI0103
          05              PH00-SUITE.                                   CI0103
            15       FILLER         PICTURE  X(00106).                  CI0103
       01                 PH06  REDEFINES      PH00.                    CI0103
            10            PH06-XDBPCB.                                  CI0103
            11            PH06-XDBDNM PICTURE  X(08).                   CI0103
            11            PH06-XSEGLV PICTURE  X(02).                   CI0103
            11            PH06-XRC    PICTURE  X(02).                   CI0103
            11            PH06-XPROPT PICTURE  X(04).                   CI0103
            11            PH06-FILLER PICTURE  S9(5)                    CI0103
                          BINARY.                                       CI0103
            11            PH06-XSEGNM PICTURE  X(08).                   CI0103
            11            PH06-XKEYLN PICTURE  S9(05)                   CI0103
                          BINARY.                                       CI0103
            11            PH06-XSEGNB PICTURE  9(05)                    CI0103
                          BINARY.                                       CI0103
            11            PH06-XCOKEY PICTURE  X(70).                   CI0103

      *PASS AREA TO CL0103
      *!WF DSP=PJ DSL=PJ SEL=51 FOR=I DES=1 LEV=1 PLT=75
       01                 PJ51.                                         CI0103
            10            PJ51-MAPPN  PICTURE  X(10).                   CI0103
            10            PJ51-C299.                                    CI0103
            11            PJ51-CTID.                                    CI0103
            12            PJ51-CTIDA  PICTURE  9(3).                    CI0103
            12            PJ51-CTIDN.                                   CI0103
            13            PJ51-CTIDNP PICTURE  X(13).                   CI0103
            13            PJ51-CTIDND PICTURE  9(11).                   CI0103
            10            PJ51-IGOTY  PICTURE  X.                       CI0103
            10            PJ51-FILLER PICTURE  X(30).                   CI0103
       01                 PJ52.                                         CI0103
            10            PJ52-AACTV  PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            PJ52-ADDAC  PICTURE  S9(7)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            PJ52-AGRPV  PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            PJ52-QSHOW  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            PJ52-AFAVP  PICTURE  S9(4)V9(3)               CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            PJ52-DASOF  PICTURE  9(8).                    CI0103
            10            PJ52-QDHGF  PICTURE  9(2).                    CI0103
            10            PJ52-QSHIS  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            PJ52-CPORT  PICTURE  X.                       CI0103
            10            PJ52-AWDRTP PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            PJ52-AWDRTC PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            PJ52-APPAYA PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            PJ52-APPAYN PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            PJ52-IGOTYA PICTURE  X.                       CI0103
            10            PJ52-QTYUD1 PICTURE  9(5).                    CI0103
            10            PJ52-QTYUD2 PICTURE  9(5).                    CI0103
            10            PJ52-AGOFD  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            PJ52-AGOFD1 PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            PJ52-ANGOF  PICTURE  S9(9)V99                 CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            PJ52-AATOTI PICTURE  S9(11)V99                CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            PJ52-QSHOM  PICTURE  S9(10)V999               CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            PJ52-FILLER PICTURE  X(43).                   CI0103

      *PASS AREA FROM CL0103
      *!WF DSP=PJ DSL=PJ SEL=52 FOR=I DES=1 LEV=1 PLT=75

      ******************************************************************
      **    DU - DL1 ERROR PROCESSING SEGMENT
      ******************************************************************
      **
      *!WF DSP=DE DSL=DU SEL=10 FOR=I DES=1 LEV=1 PLT=75
       01                 DE10.                                         CI0103
            10            DE10-DU11.                                    CI0103
            11            DE10-XFONC  PICTURE  X(4).                    CI0103
            11            DE10-MPSBN  PICTURE  X(8).                    CI0103
            11            DE10-XDBDNM PICTURE  X(08).                   CI0103
            11            DE10-XSEGNM PICTURE  X(08).                   CI0103
            11            DE10-XRC    PICTURE  X(02).                   CI0103
            11            DE10-MSEG   PICTURE  X(08).                   CI0103
            11            DE10-XCOKEY PICTURE  X(70).                   CI0103
            11            DE10-CUIBR  PICTURE  X(01).                   CI0103
            11            DE10-CUIBA  PICTURE  X(01).                   CI0103
            11            DE10-IPBIK  PICTURE  X(1).                    CI0103
            10            DE10-DU03.                                    CI0103
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            DE10-CMSSF  PICTURE  XX.                      CI0103
            11            DE10-DU09.                                    CI0103
            12            DE10-CMESA  PICTURE  S9(9)                    CI0103
                          BINARY.                                       CI0103
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0103
                          BINARY.                                       CI0103
            12            DE10-CMESB  PICTURE  S9(9)                    CI0103
                          BINARY.                                       CI0103
            12            DE10-CMSST  PICTURE  S9(9)                    CI0103
                          BINARY.                                       CI0103
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0103
                          BINARY.                                       CI0103
            12            DE10-QELLAA PICTURE  S9(9)                    CI0103
                          BINARY.                                       CI0103
            12            DE10-TMESS4 PICTURE  X(512).                  CI0103
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0103
          05              MS00-SUITE.                                   CI0103
            15       FILLER         PICTURE  X(00542).                  CI0103
       01                 MS03  REDEFINES      MS00.                    CI0103
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            10            MS03-CMSSF  PICTURE  XX.                      CI0103
            10            MS03-DU09.                                    CI0103
            11            MS03-CMESA  PICTURE  S9(9)                    CI0103
                          BINARY.                                       CI0103
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0103
                          BINARY.                                       CI0103
            11            MS03-CMESB  PICTURE  S9(9)                    CI0103
                          BINARY.                                       CI0103
            11            MS03-CMSST  PICTURE  S9(9)                    CI0103
                          BINARY.                                       CI0103
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0103
                          BINARY.                                       CI0103
            11            MS03-QELLAA PICTURE  S9(9)                    CI0103
                          BINARY.                                       CI0103
            11            MS03-TMESS4 PICTURE  X(512).                  CI0103
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0103
            10            MX11-QMSGS  PICTURE  9(03).                   CI0103
            10            MX11-PJ09                                     CI0103
                          OCCURS       025     TIMES.                   CI0103
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0103
                          COMPUTATIONAL-3.                              CI0103
            11            MX11-CMESB  PICTURE  S9(9)                    CI0103
                          BINARY.                                       CI0103
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PJ51
                                PJ52
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0103
      *               *                                   *             CI0103
      *               *INITIALISATIONS                    *             CI0103
      *               *                                   *             CI0103
      *               *************************************.            CI0103
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
      *N02CA.    NOTE *---> INITIALIZE MESSAGE SEGMENT    *.
       F02CA.                                                           lv10
           INITIALIZE  MS03.
       F02CA-FN. EXIT.
      *N02DA.    NOTE *---> INIT OUTPUT & WORK AREA       *.
       F02DA.                                                           lv10
           INITIALIZE  PJ52
           INITIALIZE  WORK-FIELDS
           MOVE        'N' TO PJ52-CPORT
           MOVE        'N' TO PJ52-IGOTYA
           MOVE        'N' TO WJ41-IENDP
           MOVE        ZEROS TO 7-PEND-DOLLARS
           7-GC03-CATRNC
           MOVE        'N' TO 7-MAX-VALUE
           MOVE        'N' TO 7-PEND-PAY-FOUND
           MOVE        ZEROS TO 7-WORK-QCSHIS.
       F02DA-FN. EXIT.
      *N02XA.    NOTE *SET POINTERS FOR DB ACCESS         *.
       F02XA.                                                           lv10
      *SET ADDRESS FOR SBUP                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-SBUP-PTR1.                                          ADU015
      *SET ADDRESS FOR SCOP                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-SCOP-PTR1.                                          ADU015
      *SET ADDRESS FOR SSPP                                             DOT
           SET ADDRESS OF PE06 TO                                       ADU015
                PCB-SSPP-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PF06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR ACAP                                             DOT
           SET ADDRESS OF PG06 TO                                       ADU015
                PCB-ACAP-PTR1.                                          ADU015
      *SET ADDRESS FOR TR1P                                             DOT
           SET ADDRESS OF PH06 TO                                       ADU015
                PCB-TR1P-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0103
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0103
      *               *                                   *             CI0103
      *               *FIN DE TRAITEMENT                  *             CI0103
      *               *                                   *             CI0103
      *               *************************************.            CI0103
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0103
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATE INPUT PARMS               *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *
      *********************************
      **  ENSURE PARMS HAVE THE       *
      **  CORRECT CONTENTS BASED ON   *
      **  FIELD CLASS AND CONTENTS    *
      *********************************
      *N40BB.    NOTE *VALIDATE CTID ID                   *.
       F40BB.    IF    PJ51-CTID NOT NUMERIC                            lv15
                 NEXT SENTENCE ELSE GO TO     F40BB-FN.
       F40BB-FN. EXIT.
      *N40BG.    NOTE *VALIDATE ADIMIN CODE               *.
       F40BG.    IF    PJ51-CTIDA NOT = 002                             lv15
                 NEXT SENTENCE ELSE GO TO     F40BG-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012263 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40BG-FN. EXIT.
      *N40BK.    NOTE *POPULATE THE USER ID               *.
       F40BK.                                                           lv10
           EXEC CICS   ASSIGN USERID (WS00-NCUSR2)           END-EXEC.
       F40BK-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *GET WORKING DATES                  *
      *               *                                   *
      *               *************************************.
       F45.                                                             lv05
      *********************************
      **  ALWAYS GET                  *
      **    CAMS ACCOUNTING DATE      *
      **     (NS20-DCACG) GREG        *
      **  IF AUDIT REQUESTED ALSO GET *
      **    ACCT DATE LESS 14 DAYS    *
      **     (7-2WK-DCACG) GREG       *
      **     (7-2WK-DCACG9) GREG 9COMP*
      **  GET SYSTEM DATE             *
      *********************************
      *N45BB.    NOTE *CALL CI0020 - CAMS ACCTG DATE      *.            AM0020
       F45BB.                                                           lv10
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
       F45BB-FN. EXIT.
      *N45BG.    NOTE *NON-DL1 ERROR                      *.            ADU070
       F45BG.    IF    MS03-NMESS2 > ZERO                               lv10
                 AND   MS03-CMESB > 10                                  ADU070
                 NEXT SENTENCE ELSE GO TO     F45BG-FN.                 ADU070
      *OF A CERTAIN SEVERITY                                            ADU070
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU070
           MOVE        CI0020 TO MS03-TMESS4 (IMS03R : 6)               ADU070
           ADD         +7 TO MS03-QELLAA                                ADU070
           MOVE                     ALL '1' TO FT GO TO F20.            ADU070
       F45BG-900. GO TO F45BI-FN.
       F45BG-FN. EXIT.
      *N45BI.    NOTE *NO ERRORS                          *.            ADU070
       F45BI.                                                           lv10
           INITIALIZE  MS03.                                            ADU070
       F45BI-FN. EXIT.
      *N45BL.    NOTE *GET 14 DAYS (2WK)AGO DATE          *.
       F45BL.    IF    PJ51-IGOTY = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F45BL-FN.
           MOVE        NS20-DCACG TO DD34-DTGRGA
           MOVE        0 TO DD34-CDTUC
           MOVE        -014 TO DD34-NDTUN
           MOVE        9 TO DD30-CDTSF
           PERFORM     F95BB THRU F95BB-FN.
      *N45DB.    NOTE *IF SUCCESSFUL RETURN OF DATE       *.
       F45DB.    IF    DD30-CDTSC = 0                                   lv15
                 NEXT SENTENCE ELSE GO TO     F45DB-FN.
      *********************************
      *TAKE THE 2WK GREG DATE
      *AND CONVERT IT TO NINES COMP
      *  7-2WK-DCACG  - GREG (CCYYMMDD)
      *  7-2WK-DCACG9 - GREG 9S COMP
      *********************************
           MOVE        DD34-DTGRGB TO 7-2WK-DCACG
           MOVE        99999999 TO 7-WS-9DAT8
           COMPUTE     7-2WK-DCACG9 =
           7-WS-9DAT8 - 7-2WK-DCACG.
       F45DB-900. GO TO F45DG-FN.
       F45DB-FN. EXIT.
      *N45DG.    NOTE *BAD DATE RETURN                    *.
       F45DG.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013154 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45DG-FN. EXIT.
       F45BL-FN. EXIT.
      *N45FB.    NOTE *GET SYSTEM DATE (CCYYMMDD)         *.
       F45FB.                                                           lv10
           EXEC CICS   ASKTIME ABSTIME (DT01-XMSTS)          END-EXEC.  ADU155
           EXEC CICS   FORMATTIME ABSTIME (DT01-XMSTS)                  ADU155
                       YYMMDD (DT01-XDAT69)                             ADU155
                       YEAR (DT01-F2CCYY)                    END-EXEC.  ADU155
           COMPUTE     DT01-YEAR = DT01-F2CCYY                          ADU155
      ** MOVE DT01-UDATE TO YOUR FIELD                                  ADU155
           MOVE        DT01-XDAT69 (3:4) TO DT01-MMDD                   ADU155
           MOVE        DT01-XDATCU TO 7-CURR-DCACG.
       F45FB-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *GET GROSS VALUE OF ACCOUNT         *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *
      *********************************
      ** THIS PROCESSING WILL:        *
      ** CALL CI0033 AND RETURN WZ52  *
      **  - TAKE THESE FIELDS:        *
      **    AACTV - GROSS VALUE       *
      **    QSHOW - QNTY SHARES OWNED *
      **    ADDAC - DAILY DIVIDEND    *
      **            ACCRUED AMOUNT    *
      **    AGRPV - PRINCIPAL ACCOUNT *
      **            VALUE AMOUNT      *
      **    AFAVP - FUND ASSET VALUE  *
      **            PRICE AMOUNT      *
      **    QSHIS - QUANTITY OF CERT  *
      **            SHARES ISSUED     *
      **    DASOF - SHARK AS OF RQST  *
      **            DATE.             *
      **    QSHOM - OLD MONEY SHARES  *
      **            OWNED             *
      **    QDHGF - NBR OF GOOD DAYS  *
      **            BEFORE GOOD FUNDS *
      *********************************
      *N50BB.    NOTE *SET UP CI0033 PARM PASS AREAS      *.
       F50BB.                                                           lv10
           INITIALIZE  WZ52
           MOVE        PJ51-CTID TO WZ52-CTID.
       F50BB-FN. EXIT.
      *N50BG.    NOTE *CALL CI0033 - ACCOUNT VALUE        *.            AM0033
       F50BG.                                                           lv10
      *                                                                 AM0033
      *********************************                                 AM0033
      ** THIS MODULE WILL READ THE    *                                 AM0033
      ** SHARK DATABASES NEEDED FOR   *                                 AM0033
      ** ACCOUNT VALUE, CALL THE      *                                 AM0033
      ** SHARK ACCOUNT VALUE MODULE   *                                 AM0033
      ** FRFT11, AND RETURN THE VALUES*                                 AM0033
      ** FOR A MUTUAL FUND ACCOUNT    *                                 AM0033
      *********************************                                 AM0033
      *                                                                 AM0033
           SET CI0033-PCB-SBUP-PTR1 TO                                  AM0033
                      PCB-SBUP-PTR1                                     AM0033
           SET CI0033-PCB-SCOP-PTR1 TO                                  AM0033
                      PCB-SCOP-PTR1                                     AM0033
           SET CI0033-PCB-SSPP-PTR1 TO
                      PCB-SSPP-PTR1
           SET CI0033-PCB-CT1P-PTR1 TO                                  AM0033
                      PCB-CT1P-PTR1                                     AM0033
           INITIALIZE DE10-DU03                                         AM0033
           CALL        CI0033 USING                                     AM0033
           DFHEIBLK                                                     AM0033
           DFHCOMMAREA                                                  AM0033
           DLIUIBII                                                     AM0033
           CI0033-PCB-ADDRESS-LIST                                      AM0033
           WZ52                                                         AM0033
           DE10                                                         AM0033
           MS03.                                                        AM0033
       F50BG-FN. EXIT.
      *N50BJ.    NOTE *NON-DL1 DST ERROR                  *.
       F50BJ.    IF    (MS03-NMESS2 > 14515                             lv10
                 AND   MS03-NMESS2 < 14586)
                 NEXT SENTENCE ELSE GO TO     F50BJ-FN.
      *TERMINATE THE PROGRAM
           MOVE                     ALL '1' TO FT GO TO F20.
       F50BJ-FN. EXIT.
      *N50BL.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F50BL.    IF    (MS03-NMESS2 > ZERO                              lv10
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F50BL-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0033 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0033 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F50BL-900. GO TO F50BN-FN.
       F50BL-FN. EXIT.
      *N50BN.    NOTE *NO ERRORS                          *.            ADU071
       F50BN.                                                           lv10
           INITIALIZE  MS03.                                            ADU071
       F50BN-FN. EXIT.
      *N50BQ.    NOTE *CHECK ERROR CODES FROM CI0033P     *.
       F50BQ.    IF    WZ52-CMESS > ZERO                                lv10
                 AND   WZ52-NMESS > ZERO
                 NEXT SENTENCE ELSE GO TO     F50BQ-FN.
      *---> Send INFO Message                                           ADU119
      *      and CONTINUE                                               ADU119
           MOVE        012022 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN.                             ADU119
       F50BQ-900. GO TO F50DB-FN.
       F50BQ-FN. EXIT.
      *N50DB.    NOTE *TAKE CI0033P MUTUAL FUND DATA      *.
       F50DB.                                                           lv10
                 IF    WZ52-AACTV NUMERIC                               DOT
      *==> GROSS VALUE
           MOVE        WZ52-AACTV TO PJ52-AACTV.
                 IF    WZ52-ADDAC NUMERIC                               DOT
      *==> DAILY DIVIDEND ACCRUED AMT
           MOVE        WZ52-ADDAC TO PJ52-ADDAC.
                 IF    WZ52-AGRPV NUMERIC                               DOT
      *==> PRINCIPAL ACCT VALUE
           MOVE        WZ52-AGRPV TO PJ52-AGRPV.
                 IF    WZ52-QSHOW NUMERIC                               DOT
      *==> QUANTITY OF SHARES OWNED
           MOVE        WZ52-QSHOW TO PJ52-QSHOW.
                 IF    WZ52-AFAVP NUMERIC                               DOT
      *==> FUND ASSET VALUE PRICE
           MOVE        WZ52-AFAVP TO PJ52-AFAVP.
                 IF    WZ52-QDHGF NUMERIC                               DOT
      *==> # OF DAYS B4 GOOD FUNDS
           MOVE        WZ52-QDHGF TO PJ52-QDHGF.
                 IF    WZ52-QSHOM NUMERIC                               DOT
      *==> OLD MONEY SHARES OWNED
           MOVE        WZ52-QSHOM TO PJ52-QSHOM.
                 IF    WZ52-QSHIS NUMERIC                               DOT
      *==> QNT CERT SHARES ISSUED
           MOVE        WZ52-QSHIS TO PJ52-QSHIS
           7-WORK-QCSHIS.
           MOVE        WZ52-DASOF TO PJ52-DASOF.                        DOT
       F50DB-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *GET PENDING VALUES                 *
      *               *                                   *
      *               *************************************.
       F55.                                                             lv05
      *
      *********************************
      ** THIS PROCESSING WILL:        *
      ** GET PENDING VALUES           *
      **  - PROCESSING WILL VARY      *
      **    DEPENDING ON THE ACTIVITY *
      **    TYPE CODE.                *
      **       DISBURSEMENTS 001      *
      **       COLLECTIONS   002      *
      **       ADJUSTMENTS   003      *
      **                              *
      ** PENDING VALUES INCLUDE:      *
      **  - PENDING WITHDRAWALS (FOR  *
      **    DISPLAY OR CALCULATION)   *
      **    COULD COME FROM DISBURS   *
      **    OR ADUJUSTMENTS.          *
      **    ACCUMULATE AS FOLLOWS:    *
      **                              *
      **   1. (AWDRTP) PEND WITHDRAWAL*
      **      FOR DISPLAY WILL EQUAL  *
      **      THE GROSS VALUE WHEN IT *
      **      IS A FULL DISBUSEMENT OR*
      **      TOTAL BECOMES GREATER   *
      **      THAN GROSS VALUE.       *
      **      ELSE IT WILL EQUAL THE  *
      **      PEND PARTIAL DISB +     *
      **      PEND NEG ADJ + SUSPENDED*
      **      TRANS (DISB AND/OR NEG  *
      **      ADJUSTMENTS.            *
      **   2. (AWDRTC) PEND WITHDRAWAL*
      **      FOR CALC DOES NOT       *
      **      INCLUDE ANY FULL DISB   *
      **      BUT CAN BE GREATER THAN *
      **      GROSS VALUE. OTHERWISE  *
      **      THE SAME AS DISPLAY.    *
      **  - (APPAYA) PENDING PAYMENTS *
      **    AVAILABLE COULD COME FROM *
      **    COLLECTION OR ADJUSTMENT  *
      **  - (APPAYN) PENDING PAYMENTS *
      **    NOT AVAILABLE COULD ONLY  *
      **    COME FROM COLLECTIONS.    *
      **                              *
      *********************************
      *N55BB.    NOTE *SET UP CI0100 PARM PASS AREAS      *.
       F55BB.                                                           lv10
      *********************************
      ** CI0100 WILL BE CALLED TO GET *
      ** ACCESS AND RETURN DATA FROM  *
      ** THE ACTIVITY DATABASE ACAP,  *
      ** SEGMENTS GC01,03,06,21.      *
      ** THE PENDING VALUES WILL BE   *
      ** DETERMINED FROM THIS DATA.   *
      *********************************
           INITIALIZE  7-GC00-AREA
           MOVE        PJ51-MAPPN TO 7-GC00-MAPPN
           MOVE        'GNP' TO 7-GC00-CFUNC
           MOVE        001 TO 7-GC00-CAATY (1)
           MOVE        002 TO 7-GC00-CAATY (2)
           MOVE        003 TO 7-GC00-CAATY (3)
           MOVE        PJ51-CTID TO 7-GC00-CTID
           MOVE        01 TO 7-GC00-CASTC (1)
           MOVE        02 TO 7-GC00-CASTC (2)
           MOVE        04 TO 7-GC00-CASTC (3).
                 IF    PJ51-IGOTY = 'Y'                                 DOT
      *-> ACTIVITY AUDIT REQUESTED <-
      *NORMALLY SET TO 'N' BUT WHEN
      *SET TO 'Y' WILL ALSO REQUEST
      *PROCESSED ACTIVITY.
      **   *   *   *   *   *   *   *
           MOVE        03 TO 7-GC00-CASTC (4).
       F55BB-FN. EXIT.
      *N55BG.    NOTE *TOP OF PENDING VALUES LOOP         *.
       F55BG.                       GO TO     F55BG-B.                  lv10
       F55BG-A.
                 IF    WJ41-IENDP = 'Y'
                                    GO TO     F55BG-FN.
       F55BG-B.
      *********************************
      ** > DO UNTIL CI0100 HAS        *
      **   RETIEVED ALL UD ROWS.      *
      ** > MAY CALL CI0100 MORE THAN  *
      **   ONCE TO GET ALL ROWS.      *
      ** > AT BOTTOM OF LOOP IENDP    *
      **   IS CHECKED TO DETERMINE IF *
      **   ANOTHER CALL IS NEEDED AND *
      **   SET UP RECALL KEY.         *
      *N55DG.    NOTE *CALL CI0100 - ACCESS ACTIVITY      *.            AM0100
       F55DG.                                                           lv15
      *********************************                                 AM0100
      ** THIS MODULE WILL ACCESS THE  *                                 AM0100
      ** ACTIVITY DATABASE AND        *                                 AM0100
      ** RETRIEVE 1 TO 10 ACTIVITIES  *                                 AM0100
      *********************************                                 AM0100
           INITIALIZE  WJ40                                             AM0100
           MOVE        7-GC00-MAPPN TO WJ40-MAPPN                       AM0100
           MOVE        7-GC00-CFUNC TO WJ40-CFUNC                       AM0100
           MOVE        7-GC00-CASTC (1) TO WJ40-CASTC (1)               AM0100
           MOVE        7-GC00-CASTC (2) TO WJ40-CASTC (2)               AM0100
           MOVE        7-GC00-CASTC (3) TO WJ40-CASTC (3)               AM0100
           MOVE        7-GC00-CASTC (4) TO WJ40-CASTC (4)               AM0100
           MOVE        7-GC00-CASTC (5) TO WJ40-CASTC (5)               AM0100
           MOVE        7-GC00-CASTC (6) TO WJ40-CASTC (6)               AM0100
           MOVE        7-GC00-CAATY (1) TO WJ40-CAATY (1)               AM0100
           MOVE        7-GC00-CAATY (2) TO WJ40-CAATY (2)               AM0100
           MOVE        7-GC00-CAATY (3) TO WJ40-CAATY (3)               AM0100
           MOVE        7-GC00-C299 TO WJ40-C299                         AM0100
           MOVE        7-GC00-DCACG9 TO WJ40-DCACG9                     AM0100
           MOVE        7-GC00-NAASQ TO WJ40-NAASQ                       AM0100
           MOVE        7-GC00-NPISQ TO WJ40-NPISQ                       AM0100
           MOVE        7-GC00-CIRAP TO WJ40-CIRAP                       AM0100
           MOVE        7-GC00-IPERT TO WJ40-IPERT                       AM0100
           MOVE        7-GC00-NEIBT TO WJ40-NEIBT                       AM0100
           MOVE        7-GC00-GESQ2C TO WJ40-GESQ2C                     AM0100
           MOVE        7-GC00-MIPPS TO WJ40-MIPPS                       AM0100
           MOVE        7-GC00-IENDP TO WJ40-IENDP                       AM0100
           SET CI0100GC-PCB-ACAP-PTR1 TO                                AM0100
                        PCB-ACAP-PTR1                                   AM0100
           INITIALIZE  DE10-DU03                                        AM0100
           CALL        CI0100 USING                                     AM0100
           DFHEIBLK                                                     AM0100
           DFHCOMMAREA                                                  AM0100
           DLIUIBII                                                     AM0100
           CI0100GC-PCB-ADDRESS-LIST                                    AM0100
           WJ40                                                         AM0100
           WJ41                                                         AM0100
           DE10                                                         AM0100
           MS03                                                         AM0100
           MX11.                                                        AM0100
       F55DG-FN. EXIT.
      *N55DL.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F55DL.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F55DL-FN.                 ADU071
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
       F55DL-900. GO TO F55DN-FN.
       F55DL-FN. EXIT.
      *N55DN.    NOTE *NO ERRORS                          *.            ADU071
       F55DN.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F55DN-FN. EXIT.
      *N55FB.    NOTE *CHECK FOR ACTIVITY ON GC01         *.
       F55FB.                                                           lv15
                 IF    WJ41-IGC01 = 'Y'                                 DOT
      *TAKE ACTIVITY ACCOUNT DATA
           MOVE        WJ41-GC01 TO GC01
                 ELSE
      *NO ACTIVITY, SO NO PENDING
      *VALUES ARE AVAILABLE,
      *SKIP TO GOOD FUNDS PROCESS
               GO TO     F55-FN.
       F55FB-FN. EXIT.
      *N55FG.    NOTE *CHECK FOR DETAIL ON GC03           *.
       F55FG.                                                           lv15
           MOVE        1                        TO J55FGR
                                    GO TO     F55FG-B.
       F55FG-A.
           ADD         1                        TO J55FGR.
       F55FG-B.
           IF          J55FGR                   >  WJ41-QDECT9
                                    GO TO     F55FG-FN.
                 IF    WJ41-IGC03 (J55FGR) = 'Y'                        DOT
      *TAKE ACTIVITY DETAIL DATA
           MOVE        WJ41-GC03 (J55FGR) TO GC03
      *********************************
      **  SET UP 88 LEVELS FOR        *
      **    CALL TYPE CODE            *
      **    ACTIVITY TRAN CODE        *
      **    PRODUCT CODE.             *
      **  INITIALIZE PEND PAYMENT     *
      **    DOLLARS AND SWITCH.       *
      *********************************
           MOVE        GC03-CCOLL TO 7-GC03-CCOLL
           MOVE        GC03-CATRNC TO 7-GC03-CATRNC
           MOVE        GC01-PRCOD TO 7-GC01-PRCOD
           MOVE        ZEROS TO 7-PEND-DOLLARS
           MOVE        'N' TO 7-PEND-PAY-FOUND
                 ELSE
      *ACTIVITY DETAIL NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013421 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N55FL.    NOTE *AUDIT ACCOUNT ACTIVITY             *.
       F55FL.    IF    PJ51-IGOTY = 'Y'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F55FL-FN.
      *********************************
      **  WHEN REQUESTED, PERFORM     *
      **  AUDITING OF ACCOUNT         *
      **  ACTIVITY ON RECENT AND      *
      **  DUPLICATE TRANSACTIONS.     *
      *********************************
           PERFORM     F95DB THRU F95DB-FN.
       F55FL-FN. EXIT.
      *N55HB.    NOTE *PENDING ACTIVITY PROCESSING        *.
       F55HB.                                                           lv20
      *********************************
      ** CAN BE EITHER                *
      **    DISBURSEMENT 001          *
      **    COLLECTION   002          *
      **    ADJUSTMENT   003          *
      *********************************
      *N55HG.    NOTE *DISBURSEMENT PROCESSING            *.
       F55HG.    IF    GC03-CAATY =                                     lv25
                       001
                 NEXT SENTENCE ELSE GO TO     F55HG-FN.
      *********************************
      **  ACCUMULATE EITHER PENDING   *
      **  WITHDRAWALS FOR DISPLAY     *
      **  OR FOR CALCULATION.         *
      *********************************
      *N55HL.    NOTE *ACCEPT THESE CONDITIONS            *.
       F55HL.    IF    GC03-IPULL = 'Y'                                 lv30
                 AND   (GC03-CASTC = 1
                 OR    GC03-CASTC = 2
                 OR    GC03-CASTC = 4)
                 NEXT SENTENCE ELSE GO TO     F55HL-FN.
      *********************************
      *TRANSACTION TO BE PULLED AND
      *IS UNPROCESSED, PULLED FOR PROD
      *SYSTEMS, OR SUSPENSED.
      *********************************
      *N55HQ.    NOTE *DETERMINE SOURCE OF DOLLARS        *.
       F55HQ.                                                           lv35
      *********************************
      *MOVE THE APPROPRIATE DOLLARS
      *TO A WORKING FIELD
      *********************************
                 IF    GC03-ADBRQ NOT = ZERO                            DOT
      *USE TOTAL DISB AMT REQUESTED
           MOVE        GC03-ADBRQ TO 7-PEND-DOLLARS
                 ELSE
      *USE ESTIMATED DISB AMT REQUESTED
           MOVE        GC03-AEDRQ TO 7-PEND-DOLLARS.
       F55HQ-FN. EXIT.
      *N55JB.    NOTE *ACCUMULATE WITHDRAWAL (DISPLAY)    *.
       F55JB.    IF    NOT-MAX-VALUE                                    lv35
                 NEXT SENTENCE ELSE GO TO     F55JB-FN.
      *********************************
      *PENDING WITHDRAWAL FOR DISPLAY
      *IS ONLY USED FOR DISPLAY.
      *NEVER GREATER THAN GROSS VALUE.
      *********************************
      *N55JG.    NOTE *FOUND A FULL DISBURSEMENT          *.
       F55JG.    IF    GC03-CPORT = 'F'                                 lv40
                 NEXT SENTENCE ELSE GO TO     F55JG-FN.
      *********************************
      ** MOVE GROSS VALUE TO PENDING
      *  WITHDRAWAL FOR DISPLAY.
      ** SET FULL DISB INDICATOR.
      ** SET TO MAXIMUM VALUE TO STOP
      *  PROCESSING ANY MORE PENDING
      *  WITHDRAWALS FOR DISPLAY.
      *********************************
           MOVE        PJ52-AACTV TO PJ52-AWDRTP
           MOVE        'F' TO PJ52-CPORT
           MOVE        'Y' TO 7-MAX-VALUE.
       F55JG-900. GO TO F55JL-FN.
       F55JG-FN. EXIT.
      *N55JL.    NOTE *ADD PEND WITHDRAWAL FOR DISPLAY    *.
       F55JL.                                                           lv40
           ADD         7-PEND-DOLLARS TO PJ52-AWDRTP.
       F55JL-FN. EXIT.
       F55JB-FN. EXIT.
      *N55JQ.    NOTE *ACCUMULATE WITHDRAWAL (CALC)       *.
       F55JQ.    IF    GC03-CPORT NOT = 'F'                             lv35
                 NEXT SENTENCE ELSE GO TO     F55JQ-FN.
      *********************************
      *IF NOT FULL DISBURSMENT ADD TO
      *PEND WITHDRAWAL FOR CALCULATION
      *********************************
           ADD         7-PEND-DOLLARS TO PJ52-AWDRTC.
       F55JQ-FN. EXIT.
       F55HL-FN. EXIT.
       F55HG-900. GO TO F55HB-FN.
       F55HG-FN. EXIT.
      *N55LB.    NOTE *COLLECTION PROCESSING              *.
       F55LB.    IF    GC03-CAATY =                                     lv25
                       002
                 NEXT SENTENCE ELSE GO TO     F55LB-FN.
      *********************************
      *ACCUMULATE PENDING PAYMENTS
      *AVAILABLE OR UNAVAIL.
      *********************************
      *N55LG.    NOTE *CHECK UNDISBURSED COLLECTION       *.
       F55LG.    IF    GC03-IPULL = 'Y'                                 lv30
                 AND   (GC03-CASTC = 1
                 OR    GC03-CASTC = 4)
                 NEXT SENTENCE ELSE GO TO     F55LG-FN.
      *********************************
      *IF THIS COLLECTION TRANSACTION
      *  IS TO BE PULLED
      *  AND IT IS EITHER
      *  UNPROCESSED OR SUSPENSED.
      ** *
      *IT COULD BE AN UNDISBURSED
      *COLLECTION, ONE WITHOUT A
      *DISBURSEMENT.
      ** *                          * *
      *IT COULD BE EITHER A PENDING
      *PAYMENT AVAILABLE OR UNAVAIL.
      *********************************
           PERFORM     F96BB THRU F96BB-FN.
       F55LG-FN. EXIT.
      *N55LL.    NOTE *PEND PAYMENT NOT FOUND YET         *.
       F55LL.    IF    PEND-PAY-NOTFOUND                                lv30
                 AND   (GC03-CASTC = 1
                 OR    GC03-CASTC = 4)
                 NEXT SENTENCE ELSE GO TO     F55LL-FN.
      *********************************
      *CHECK COLLECTIONS THAT MAY HAVE
      *DISBURSEMENTS (HAVE GC21 RECS).
      *IF UNPROCESSED OR SUSPENSED.
      *********************************
      *N55LQ.    NOTE *GET DISBURSEMENT (GC21 SEGMENT)    *.
       F55LQ.    IF    WJ41-IGC21 (J55FGR) = 'Y'                        lv35
                 NEXT SENTENCE ELSE GO TO     F55LQ-FN.
           MOVE        WJ41-GC21 (J55FGR) TO GC21.
       F55LQ-900. GO TO F55NB-FN.
       F55LQ-FN. EXIT.
      *N55NB.    NOTE *ELSE... NO DISBURSEMENT RECORD     *.
       F55NB.                                                           lv35
               GO TO     F55FG-900.
       F55NB-FN. EXIT.
      *N55NG.    NOTE *READ DISB GC01/03 INTO GX03        *.
       F55NG.                                                           lv35
      *********************************
      *MUST GET THE DISBURSEMENT DATA
      *THAT RELATES TO THE COLLECTION
      *SO THAT IT CAN BE INTARIGATED
      *TO HELP DETERMINE WHERE TO
      *ADD THE COLLETION DOLLARS.
      *********************************
           MOVE        GC21-CTID TO S-GXU01-CTID
           MOVE        GC21-DCACG9 TO S-GXU03-DCACG9
           MOVE        GC21-NAASQ TO S-GXU03-NAASQ
           MOVE        '= ' TO S-GXU03-OPER
           PERFORM     F94G2 THRU F94G2-FN.
       F55NG-FN. EXIT.
      *N55NL.    NOTE *FOUND DISBURSEMENT GX03            *.
       F55NL.    IF    IK = '0'                                         lv35
                 NEXT SENTENCE ELSE GO TO     F55NL-FN.
      *N55PB.    NOTE *ELIMINATE UNPROCESSED NOT PULLED   *.
       F55PB.    IF    GX03-CASTC = 01                                  lv40
                 AND   GX03-IPULL = 'N'
                 NEXT SENTENCE ELSE GO TO     F55PB-FN.
               GO TO     F55FG-900.
       F55PB-FN. EXIT.
      *N55PG.    NOTE *READ RELATED GC06 INTO GX06        *.
       F55PG.                                                           lv40
      *********************************
      *THIS WILL BE USED TO DETERMINE
      *IF THE DESTINATION TRANSFER
      *TYPE IS APLICABLE.  MUST BE
      *EXCHANGE, TRANSFER, OR DOUBLE
      *TRANSFER.
      *********************************
           PERFORM     F94G3 THRU F94G3-FN.
                 IF    IK = '0'                                         DOT
      *GX06 FOUND
      *SET 88 LEVEL (TRANSF TYPE CD)
           MOVE        GX06-CTYPE TO 7-GX06-CTYPE.
                 IF    IK NOT = '0'                                     DOT
      *GC06 DATA NOT FOUND
               GO TO     F55FG-900.
       F55PG-FN. EXIT.
      *N55PL.    NOTE *PERFORM PEND PAY LOGIC             *.
       F55PL.                                                           lv40
      *********************************
      *PERFORM LOGIC TO DETERMINE IF
      *THIS IS A PENDING PAYMENT
      *AVAILABLE OR UNAVAILABLE.
      *********************************
           PERFORM     F96PB THRU F96PB-FN.
       F55PL-FN. EXIT.
       F55NL-FN. EXIT.
       F55LL-FN. EXIT.
       F55LB-900. GO TO F55HB-FN.
       F55LB-FN. EXIT.
      *N55TB.    NOTE *ADJUSTMENT PROCESSING              *.
       F55TB.    IF    GC03-CAATY =                                     lv25
                       003
                 NEXT SENTENCE ELSE GO TO     F55TB-FN.
      *********************************
      *ACCUMULATE EITHER PENDING
      *WITHDRAWALS OR PENDING
      *PAYMENTS AVAILABLE.
      *********************************
      *N55TG.    NOTE *ACCEPT THESE CONDITIONS            *.
       F55TG.    IF    GC03-IPULL = 'Y'                                 lv30
                 AND   (GC03-CASTC = 1
                 OR    GC03-CASTC = 4)
                 NEXT SENTENCE ELSE GO TO     F55TG-FN.
      *********************************
      *TRANSACTION TO BE PULLED AND
      *EITHER UNPROCESSED OR SUSPENSED
      *********************************
      *N55TL.    NOTE *DETERMINE SOURCE OF VALUE          *.
       F55TL.                                                           lv35
      *********************************
      *COULD BE FROM DOLLARS OR SHARES.
      *IF DOLLARS JUST TAKE IT.
      *IF SHARES CONVERT TO DOLLARS.
      *********************************
                 IF    GC03-APRNT1 NOT = ZERO                           DOT
      *TAKE FROM NET TRAN PROCEEDS AMT
           MOVE        GC03-APRNT1 TO 7-PEND-DOLLARS
                 ELSE
      *CONVERT QUANTITY OF SHARES
           COMPUTE     7-PEND-DOLLARS =
           GC03-QSHOWT * PJ52-AFAVP.
                 IF    7-PEND-DOLLARS < ZERO                            DOT
      *REVERSE NEGATIVE DOLLARS
           MULTIPLY    -1 BY 7-PEND-DOLLARS.
       F55TL-FN. EXIT.
      *N55UB.    NOTE *THIS IS A PENDING PAYMENT AVAIL    *.
       F55UB.    IF    PEND-PAY-TRAN                                    lv35
                 NEXT SENTENCE ELSE GO TO     F55UB-FN.
           ADD         7-PEND-DOLLARS TO PJ52-APPAYA.
       F55UB-FN. EXIT.
      *N55UG.    NOTE *THIS IS A PENDING WITHDRAWAL       *.
       F55UG.    IF    PEND-WITHDR-TRAN                                 lv35
                 NEXT SENTENCE ELSE GO TO     F55UG-FN.
      *N55UL.    NOTE *ACCUMULATE WITHDRAWAL (DISPLAY)    *.
       F55UL.    IF    NOT-MAX-VALUE                                    lv40
                 NEXT SENTENCE ELSE GO TO     F55UL-FN.
      *N55UQ.    NOTE *FOUND A FULL DISBURSEMENT          *.
       F55UQ.    IF    GC03-CPORT = 'F'                                 lv45
                 NEXT SENTENCE ELSE GO TO     F55UQ-FN.
      *********************************
      *MOVE GROSS VALUE TO PENDING
      *WITHDRAWAL FOR DISPLAY.
      *SET TO MAXIMUM VALUE AND SET
      *OUTPUT FULL DISB INDICATOR.
      *********************************
           MOVE        PJ52-AACTV TO PJ52-AWDRTP
           MOVE        'F' TO PJ52-CPORT
           MOVE        'Y' TO 7-MAX-VALUE
               GO TO     F55TG-FN.
       F55UQ-900. GO TO F55WB-FN.
       F55UQ-FN. EXIT.
      *N55WB.    NOTE *ADD PEND WITHDRAWAL FOR DISPLAY    *.
       F55WB.                                                           lv45
           ADD         7-PEND-DOLLARS TO PJ52-AWDRTP.
       F55WB-FN. EXIT.
       F55UL-FN. EXIT.
      *N55WG.    NOTE *ACCUMULATE WITHDRAWAL (CALC)       *.
       F55WG.    IF    GC03-CPORT NOT = 'F'                             lv40
                 NEXT SENTENCE ELSE GO TO     F55WG-FN.
      *********************************
      *IF NOT FULL DISBURSMENT ADD TO
      *PEND WITHDRAWAL FOR CALCULATION
      *********************************
           ADD         7-PEND-DOLLARS TO PJ52-AWDRTC.
       F55WG-FN. EXIT.
       F55UG-FN. EXIT.
       F55TG-FN. EXIT.
       F55TB-900. GO TO F55HB-FN.
       F55TB-FN. EXIT.
      *N55WL.    NOTE *END OF CASE FOR GC03-CAATY         *.
       F55WL.         EXIT.                                             lv25
       F55WL-FN. EXIT.
       F55HB-FN. EXIT.
       F55FG-900. GO TO F55FG-A.
       F55FG-FN. EXIT.
      *N55YB.    NOTE *SET UP RESTART FIELDS              *.
       F55YB.    IF    WJ41-IENDP = 'N'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F55YB-FN.
      *TAKE KEYS FROM 10TH ROW OF LAST
      *CALL AND PASS THEM AS INPUT TO
      *THE RESTART CALL.
           MOVE        WJ41-IENDP TO 7-GC00-IENDP
           MOVE        WJ41-MIPPS TO 7-GC00-MIPPS
           MOVE        GC03-DCACG9 TO 7-GC00-DCACG9
           MOVE        GC03-NAASQ TO 7-GC00-NAASQ.
                 IF    WJ41-NPISQ (10) NOT NUMERIC                      DOT
      *CHECK FOR NON NUMERIC VALUE
           MOVE        ZEROES TO 7-GC00-NPISQ
                 ELSE
      *CHECK FOR NUMERIC
           MOVE        WJ41-NPISQ (10) TO 7-GC00-NPISQ.
       F55YB-FN. EXIT.
       F55BG-900. GO TO F55BG-A.
       F55BG-FN. EXIT.
      *N55YD.    NOTE *ADDING PENDING PAYMNT AVAILABLE    *.
       F55YD.                                                           lv10
      *AMOUNT TO PENDING PAYEMENT
      *NOT AVAILABLE, AS NO PENDING
      *PAYEMENTS ARE AVAILABLE FOR
      *REDEMPTION ON TA2000
           COMPUTE     PJ52-APPAYN = PJ52-APPAYA +
           PJ52-APPAYN
           MOVE        ZEROES TO PJ52-APPAYA.
       F55YD-FN. EXIT.
       F55-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *GET GOOD FUNDS                     *
      *               *                                   *
      *               *************************************.
       F60.                                                             lv05
      *
      *********************************
      **                              *
      ** THIS PROCESSING WILL:        *
      ** GET GOOD FUNDS VALUE         *
      **  - CALL DST'S VIEW           *
      **  - USE RETURNED VALUE TO     *
      **    CALCULATE ADVANCED        *
      **    GOOD FUNDS VALUE          *
      **                              *
      *********************************
      *N60BB.    NOTE *CHECK ERROR CODES FROM CI0033P     *.
       F60BB.    IF    WZ52-CMESS = ZERO                                lv10
                 AND   WZ52-NMESS = ZERO
                 NEXT SENTENCE ELSE GO TO     F60BB-FN.
      *********************************
      *ONLY PROCESS GOOD FUNDS MODULE
      *IF ACCOUNT VALUE DETERMINED TO
      *BE ERROR-FREE; MUST CHECK BOTH
      *RETURN CODE FIELDS.
      *********************************
      *N60BE.    NOTE *GET CT01 SEGMENT TO GET PRODUCT    *.
       F60BE.                                                           lv15
      *AND SUB-PRODUCT CODE-END IF NONE
      *********************************
           MOVE        PJ51-CTID TO S-CTU01-CTID
           PERFORM     F94CT THRU F94CT-FN.
                 IF    IK NOT = '0'                                     DOT
      *CT01 NOT FOUND. SEND ERROR
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012011 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F60BE-FN. EXIT.
      *N60BH.    NOTE *INITIALISE THE REQUEST AND         *.
       F60BH.                                                           lv15
      *RESPONSE SEGMENT FOR THE
      *GOOD FUND VIEW
      *********************************
           MOVE        SPACES TO SQ1F
           SQ5F
           MOVE        1 TO SQ1F-NRERO
           SQ5F-NRURO
      *********************************
      *POPULATE THE ACCOUNT NUMBER
      *PRODUCT CODE AND SUBPRODUCT CODE
      *********************************
           MOVE        CT01-CTID TO SQ1F-CTID (1)
           MOVE        CT01-PRCOD TO SQ1F-PRCOD (1)
           MOVE        CT01-PRSCD TO SQ1F-CPRSCN (1).
       F60BH-FN. EXIT.
      *N60BL.    NOTE *LOAD THE VALUES TO THE INPUT       *.
       F60BL.                                                           lv15
      *LINKAGE SEGMENT OF THE BROKER
      ********************************
           MOVE        SPACES TO WL00-RESPONSE                          AADBI5
           WL00-REQUEST                                                 AADBI5
           INITIALIZE  SQ1L SQ2L SQ3L                                   AADBI5
           MOVE        001 TO SQ1L-NPVERH                               AADBI5
           MOVE        001 TO SQ1L-NPVERC                               AADBI5
           MOVE        001 TO SQ1L-NPVERD                               AADBI5
           MOVE        LENGTH OF SQ1F TO                                AADBI5
           SQ1L-GELLP1                                                  AADBI5
           MOVE        LENGTH OF SQ5F TO                                AADBI5
           SQ1L-GELLP2                                                  AADBI5
           MOVE        '2940' TO SQ1L-NVIEW                             AADBI5
           MOVE        WS00-NCUSR2 TO SQ1L-NCUSR2                       AADBI5
           MOVE        'U' TO SQ1L-CPRT2                                AADBI5
           MOVE        PROGE TO SQ1L-MRPIB1                             AADBI5
           MOVE        'N' TO SQ1L-CLOGY                                AADBI5
           MOVE        ZERO TO SQ1L-QTOUT                               AADBI5
           MOVE        'M' TO SQ1L-CVSIZ                                AADBI5
           MOVE        SQ1F TO WL00-REQUEST                             AADBI5
      *********************************
      *CALL DBI5000N TO ACCESS THE
      *DST'S GOOD FUNDS VIEW
      *********************************
           PERFORM     F98DS THRU F98DS-FN
      *********************************
      *CHECK FOR ERROR
      *********************************
           MOVE        LENGTH OF SQ5F-GRFIX TO
           WS-FIX-GELL
           MOVE        LENGTH OF SQ5F-GRVAR (1) TO
           WS-VAR-GELL
           PERFORM     F97BC THRU F97BC-FN.
       F60BL-FN. EXIT.
      *N60BQ.    NOTE *SEND INFORMATIONAL MESSAGE         *.
       F60BQ.    IF    WS00-IERRC NOT = SPACES                          lv15
                 NEXT SENTENCE ELSE GO TO     F60BQ-FN.
      *CONTINUE.IN CASE OF DST ERROR OR
      *PB0251 ERROR OR VIEW RESPONSE
      *ERROR
      *---> Send INFO Message                                           ADU119
      *      and CONTINUE                                               ADU119
           MOVE        012350 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN.                             ADU119
       F60BQ-900. GO TO F60DB-FN.
       F60BQ-FN. EXIT.
      *N60DB.    NOTE *TAKE GOOD FUNDS DATA FROM          *.
       F60DB.                                                           lv15
      *THE DST VIEW
           MOVE        WL00-RESPONSE TO SQ5F
      *==> GOOD FUNDS AMOUNT
           MOVE        SQ5F-AGOFD2 (1) TO PJ52-AGOFD
      *==> RISK AMOUNT
           MOVE        SQ5F-ANGOF1 (1) TO PJ52-ANGOF.
       F60DB-FN. EXIT.
       F60BB-FN. EXIT.
      *N60DG.    NOTE *CALCULATE NEW GOOD FUNDS AMOUNT    *.
       F60DG.                                                           lv10
      *********************************
      *CALCULATE AS FOLLOWS:
      *  OLD GOOD FUNDS + PENDING
      *  PAYMENTS AVAILABLE - PENDING
      *  WITHDRAWALS FOR CALCULATION.
      *********************************
      *TEMP CHANGE TO ALLOW NEGATIVE
      *DO NOT PASS NEGATIVE NUMBER
      *99IT PJ52-AGOFD1 < ZERO
      *ZEROES      PJ52-AGOFD1
      *********************************
           COMPUTE     PJ52-AGOFD1 = PJ52-AGOFD +
           PJ52-APPAYA - PJ52-AWDRTC.
       F60DG-FN. EXIT.
      *N60FG.    NOTE *ADJUST QSHIS VALUE                 *.
       F60FG.    IF    WZ52-CMESS = ZERO                                lv10
                 AND   WZ52-NMESS = ZERO
                 NEXT SENTENCE ELSE GO TO     F60FG-FN.
      *********************************
      *SET UP KEY TO READ MISC TRAN DB
      *GQ01 WITH ACCOUNT AND TRAN NBR.
      *
      *THEN DO UNIQUE READ TO GET THE
      *FIRST, IF ANY, STOCK CERTS
      *RETURNED DURING THE DAY.
      *********************************
           MOVE        PJ51-CTID TO S-GQU01-CANUMB
           MOVE        00042 TO S-GQU01-CAMCTR
           MOVE        ZEROS TO S-GQU01-GESQ2
           MOVE        '=>' TO S-GQU01-OPER
           PERFORM     F94G5 THRU F94G5-FN.
      *N60FL.    NOTE *LOOP THROUGH ALL GQ01'S            *.
       F60FL.    IF    IK = '0'                                         lv15
                 AND   GQ01-CANUMB = PJ51-CTID
                 AND   GQ01-CAMCTR = 00042
                 NEXT SENTENCE ELSE GO TO     F60FL-FN.
      *********************************
      *ADJUST CERT SHR QUANTITY WHEN
      *A RETURNED STOCK CERT IS FOUND.
      ** *
      *QUANTITY COULD GO UP OR DOWN
      *DEPENDING ON THE TRAN CODE.
      *********************************
           MOVE        GQ01-XMISL TO GS42.
                 IF    GS42-CATRN = 900000                              DOT
                 OR    901000
                 OR    098000
      *ISSUE CERT
      *REVERSE CERT CANCELLATION
      *ADJUST CERT SHARES
           COMPUTE     7-WORK-QCSHIS =
           7-WORK-QCSHIS + GS42-ATDVS.
                 IF    GS42-CATRN = 099000                              DOT
      *CANCEL CERT
           COMPUTE     7-WORK-QCSHIS =
           7-WORK-QCSHIS - GS42-ATDVS.
           PERFORM     F94G6 THRU F94G6-FN.                             DOT
       F60FL-900. GO TO F60FL.
       F60FL-FN. EXIT.
      *N60FQ.    NOTE *MOVE ADJUSTED CERT SHR TO OUTPUT   *.
       F60FQ.                                                           lv15
           MOVE        7-WORK-QCSHIS TO PJ52-QSHIS.
       F60FQ-FN. EXIT.
       F60FG-FN. EXIT.
      *N60HB.    NOTE *CALCULATE ASSETS AVAILABLE AMT     *.
       F60HB.                                                           lv10
      *********************************
      *CALCULATE AS FOLLOWS:
      *  GROSS VALUE + PENDING PAYMENTS
      *  AVAILABLE - PENDING WITHDRAWAL
      *  FOR CALCULATION.
      *********************************
           COMPUTE     PJ52-AATOTI = PJ52-AACTV +
           PJ52-APPAYA - PJ52-AWDRTC.
                 IF    PJ52-AATOTI < ZERO                               DOT
      *DO NOT PASS NEGATIVE NUMBER
           MOVE        ZEROES TO PJ52-AATOTI.
       F60HB-FN. EXIT.
       F60-FN.   EXIT.
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
      *N92TF.    NOTE *RANDOM TABLE READ FOR TF09         *.            ADUTAB
       F92TF.                                                           lv10
           MOVE        'R1' TO G-TF09-TABFO                             ADUTAB
           COMPUTE     G-TF09-LTH = 60 + G-TF09-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TF09-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TF09)                                ADUTAB
                       LENGTH (G-TF09-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TF09-TABCR NOT = '00'                          DOT
           MOVE        '1' TO IK
                 ELSE
           MOVE        '0' TO IK.
       F92TF-FN. EXIT.
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
      *               *DB CALLS                           *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94CT.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94CT.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PF06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CT-FN. EXIT.
      *N94G2.    NOTE *CALL GU ON GX03                    *.            ADU026
       F94G2.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PG06 GX03                                                    ADU026
           S-GXU01-SSA S-GXU03-SSA                                      ADU026
           MOVE        PG06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G2-FN. EXIT.
      *N94G3.    NOTE *CALL GN ON GX06                    *.            ADU026
       F94G3.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PG06 GX06                                                    ADU026
           S-GX01-SSA S-GX03-SSA S-GX06-SSA                             ADU026
           MOVE        PG06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G3-FN. EXIT.
      *N94G5.    NOTE *CALL GU ON GQ01                    *.            ADU026
       F94G5.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PH06 GQ01                                                    ADU026
           S-GQU01-SSA                                                  ADU026
           MOVE        PH06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G5-FN. EXIT.
      *N94G6.    NOTE *CALL GN ON GQ01                    *.            ADU026
       F94G6.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PH06 GQ01                                                    ADU026
           S-GQ01-SSA                                                   ADU026
           MOVE        PH06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G6-FN. EXIT.
       F94-FN.   EXIT.
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *MISC CALL ROUTINES                 *
      *               *                                   *
      *               *************************************.
       F95.           EXIT.                                             lv05
      *N95BB.    NOTE *CDU - DATE DIFF/CALCULATION        *.            AADA82
       F95BB.                                                           lv10
      ** * * * * * * * * * * * * * * *                                  AADA82
      *This code calls the common date                                  AADA82
      *utility MWS100EX to calculate                                    AADA82
      *the difference between 2 dates                                   AADA82
      *or calculate a new date (add/                                    AADA82
      *subtract days). It uses a                                        AADA82
      *dynamic call.                                                    AADA82
      ** * * * * * * * * * * * * * * *                                  AADA82
      *Before the call set the subfunc                                  AADA82
      *request code DD30-CDTSF:                                         AADA82
      *  8 = date difference                                            AADA82
      *  9 = date add/subtract days                                     AADA82
      ** * * * * * * * * * * * * * * *                                  AADA82
      *Check return code DD30-CDTSC                                     AADA82
      *after the call.                                                  AADA82
      *    0 = Error Free                                               AADA82
      *    3 = Invalid Date                                             AADA82
      *    5 = Invalid Day                                              AADA82
      *    6 = Invalid Month                                            AADA82
      ** * * * * * * * * * * * * * * *                                  AADA82
           MOVE        4 TO DD30-CDTFN                                  AADA82
           CALL        MWS100EX USING DD30                              AADA82
           DD34.                                                        AADA82
       F95BB-FN. EXIT.
      *N95DB.    NOTE *AUDIT ACCOUNT ACTIVITY             *.
       F95DB.         EXIT.                                             lv10
      *N95DG.    NOTE *LOOK FOR DUPLICATE DISBURSEMENTS   *.
       F95DG.    IF    GC03-CAATY = 001                                 lv15
                 AND   PJ52-IGOTYA = 'N'
                 AND   (GC03-DCACG9 = 7-2WK-DCACG9
                 OR    GC03-DCACG9 < 7-2WK-DCACG9)
                 NEXT SENTENCE ELSE GO TO     F95DG-FN.
      *********************************
      *DISBURSEMENT HAS OCCURED
      *WITHIN THE LAST 14 DAYS
      *********************************
      *N95DL.    NOTE *CHECK ACTIVITY STATUS              *.
       F95DL.    IF    GC03-CASTC = 03                                  lv20
                 OR    ((GC03-CASTC = 01
                 OR    GC03-CASTC = 04)
                 AND   (GC03-IPULL = 'Y'))
                 NEXT SENTENCE ELSE GO TO     F95DL-FN.
      *********************************
      ** ALREADY PROCESSED OR
      ** ((UNPROCESSED OR SUSPENSED)
      ** AND READY TO PULL))
      *SET 88 LEVEL (ORIGINATOR CODE)
           MOVE        GC03-CACTO TO 7-GC03-CACTO.
      *N95DQ.    NOTE *CHECK ORIGINATOR CODE (CACTO)      *.
       F95DQ.    IF    OK-DUP-ORIG-CD                                   lv25
                 NEXT SENTENCE ELSE GO TO     F95DQ-FN.
      *********************************
      *SYSTEM THAT CREATED ACTIVITY
      *   001 ONLINE UD/UT
      *   014 FIELD DATA CAPTURE UD
      *   020 EZ TRAN UD
      *********************************
      *N95DV.    NOTE *EXCLUDE DRAFTS                     *.
       F95DV.    IF    GC03-CTRTP NOT = 'DR'                            lv30
                 NEXT SENTENCE ELSE GO TO     F95DV-FN.
      *********************************
      *AN UNSCHEDULED DISBURSMENT HAS
      *OCCURED WITHIN THE LAST 14 DAYS
      *   SET IGOTYA SWITCH.
      *********************************
           MOVE        'Y' TO PJ52-IGOTYA.
       F95DV-FN. EXIT.
       F95DQ-FN. EXIT.
       F95DL-FN. EXIT.
       F95DG-FN. EXIT.
      *N95FB.    NOTE *CHECK RECENT ACCOUNT ACTIVITY      *.
       F95FB.                                                           lv15
      *********************************
      *==> MONEY LAUNDERING CHECK <==
      *ACTIVITY WITHIN LAST 30 DAYS.
      *TWO TOTALS PROVIDED.  ONE FOR
      *DISBURSEMENTS AND ONE FOR
      *COLLECTIONS.
      *********************************
      *N95FG.    NOTE *AUDIT DISBURSEMENTS                *.
       F95FG.    IF    GC03-CAATY = 001                                 lv20
                 NEXT SENTENCE ELSE GO TO     F95FG-FN.
      *N95FL.    NOTE *CHECK ACTIVITY STATUS              *.
       F95FL.    IF    GC03-CASTC = 03                                  lv25
                 OR    ((GC03-CASTC = 01
                 OR    GC03-CASTC = 04)
                 AND   (GC03-IPULL = 'Y'))
                 NEXT SENTENCE ELSE GO TO     F95FL-FN.
      *********************************
      *ALREADY PROCESSED OR
      *UNPROCESSED OR SUSPENSED
      *AND READY TO PULL
      *********************************
      *SET 88 LEVEL(TRAN TYPE ENTRY CD)
           MOVE        GC03-CTRTP TO 7-GC03-CTRTP.
      *N95FQ.    NOTE *CHECK ENTRY CODE                   *.
       F95FQ.    IF    OK-RECENT-ENTRY-CD                               lv30
                 NEXT SENTENCE ELSE GO TO     F95FQ-FN.
      *********************************
      *TYPE OF TRANSACTION REQUESTED
      * 'S ' SURRENDER/REDEMPTION
      * 'DR' OR DRAFT
      *********************************
      *ANCREMENT 30 DAY DISB COUNTER
           ADD         1 TO PJ52-QTYUD1.
       F95FQ-FN. EXIT.
       F95FL-FN. EXIT.
       F95FG-FN. EXIT.
      *N95HB.    NOTE *CHECK 30 DAY COLLECTIONS           *.
       F95HB.    IF    GC03-CAATY = 002                                 lv20
                 NEXT SENTENCE ELSE GO TO     F95HB-FN.
      *********************************
      *DETERMINE IF THESE COLLECTIONS
      *SHOULD BE COUNTED
      *********************************
      *N95HG.    NOTE *ELIMINATE UNPROCESSED NOT PULLED   *.
       F95HG.    IF    GX03-CASTC = 01                                  lv25
                 AND   GX03-IPULL = 'N'
                 NEXT SENTENCE ELSE GO TO     F95HG-FN.
               GO TO     F95DB-FN.
       F95HG-FN. EXIT.
      *N95HL.    NOTE *GOOD ACTIVITY STATUS CODES         *.
       F95HL.    IF    GX03-CASTC = 01                                  lv25
                 OR    GC03-CASTC = 03
                 OR    GC03-CASTC = 04
                 NEXT SENTENCE ELSE GO TO     F95HL-FN.
      *********************************
      *UNPROCESSED, PROCESSED, SUSPENSE
      *********************************
      *SET 88 LEVEL (SYSTEM SOURCE CD)
           MOVE        GC03-CSYST TO 7-GC03-CSYST.
      *N95HQ.    NOTE *CHECK SYSTEM SOURCE CODE           *.
       F95HQ.    IF    OK-RECENT-SOURCE-CD                              lv30
                 NEXT SENTENCE ELSE GO TO     F95HQ-FN.
      *********************************
      *VALID  GC03-CSYST VALUES
      *11  BANK AUTHORIZATION
      *13  CATS UNSCHEDULED COLLECTIONS
      *15  UNSCHEDULED ASSET TRANSFERS
      *20  CATS SCHEDULED TRANSFERS
      *********************************
      *INCREMENT 30 DAY COLLECT COUNTER
           ADD         1 TO PJ52-QTYUD2.
       F95HQ-FN. EXIT.
       F95HL-FN. EXIT.
       F95HB-FN. EXIT.
       F95FB-FN. EXIT.
       F95DB-FN. EXIT.
       F95-FN.   EXIT.
      *N96.      NOTE *************************************.
      *               *                                   *
      *               *COLLECTION ACTIVITY ROUTINES       *
      *               *                                   *
      *               *************************************.
       F96.           EXIT.                                             lv05
      *N96BB.    NOTE *PENDING PAYMENT                    *.
       F96BB.         EXIT.                                             lv10
      *N96BG.    NOTE *COLLECTION IS NOT ASSET TRANSFER   *.
       F96BG.    IF    GC03-CAATY = 002                                 lv15
                 AND   GC03-ITRAN = 'N'
                 NEXT SENTENCE ELSE GO TO     F96BG-FN.
      *********************************
      *THIS THE PENDING ENTRY TEST FOR
      *KD90 WITH '  COLL ' IN THE
      *TRAN CODE FIELD.
      *********************************
      *N96BL.    NOTE *TAX FREE OR MONEY MARKET FUND      *.
       F96BL.    IF    VALID-PRODUCT-CD                                 lv20
                 NEXT SENTENCE ELSE GO TO     F96BL-FN.
      *********************************
      *GC01-PRCOD = 13 OR 16 OR 167
      *********************************
      *N96DB.    NOTE *PENDING PAYMENT AVAILABLE          *.
       F96DB.    IF    (AVAIL-CALL-TYPE                                 lv25
                 OR    GC03-CCOLL = SPACES)
                 AND   GC03-DEFFT < NS20-DCACG
                 NEXT SENTENCE ELSE GO TO     F96DB-FN.
      *********************************
      *FOR A MONEY MARKET ACCOUNT
      *COLLECTION TYPE (GC03-CCOLL) IS
      *BLANK OR ONE OF THE FOLLOWING
      *'CER'  FROM CERTIFIED CHECK
      *'INS'  CHECK FROM OTHER INS CO
      *'IDS'  FROM OTHER IDS PRODUCT
      *'WIR'  WIRE TRANSFER
      *TRAN DATE < ACCT DATE (CCYYMMDD)
      *********************************
                 IF    GC03-ACASH NUMERIC                               DOT
      *ADD PEND PAY AVAILABLE
           ADD         GC03-ACASH TO PJ52-APPAYA
           MOVE        'Y' TO 7-PEND-PAY-FOUND
               GO TO     F96BB-FN.
       F96DB-FN. EXIT.
      *N96DG.    NOTE *PENDING PAYMENT NOT AVAILABLE      *.
       F96DG.    IF    (AVAIL-CALL-TYPE                                 lv25
                 OR    GC03-CCOLL = SPACES)
                 AND   (GC03-DEFFT = NS20-DCACG
                 OR    GC03-DEFFT > NS20-DCACG)
                 NEXT SENTENCE ELSE GO TO     F96DG-FN.
      *********************************
      *FOR A MONEY MARKET ACCOUNT
      *COLLECTION TYPE (GC03-CCOLL) IS
      *BLANK OR ONE OF THE FOLLOWING
      *********************************
      *TRAN DATE = OR > ACCT DATE
      *CALL TYPE NOT CHECKED
      *********************************
                 IF    GC03-ACASH NUMERIC                               DOT
      *ADD PEND PAY NOT AVAILABLE
           ADD         GC03-ACASH TO PJ52-APPAYN
           MOVE        'Y' TO 7-PEND-PAY-FOUND.
       F96DG-FN. EXIT.
       F96BL-900. GO TO F96DL-FN.
       F96BL-FN. EXIT.
      *N96DL.    NOTE *NOT TAX FREE OR MONEY MARKET       *.
       F96DL.         EXIT.                                             lv20
      *N96FB.    NOTE *PENDING PAYMENT AVAILABLE          *.
       F96FB.    IF    AVAIL-CALL-TYPE                                  lv25
                 NEXT SENTENCE ELSE GO TO     F96FB-FN.
      *********************************
      *VALID CALL TYPES:
      *  FROM CERTIFIED CHECK
      *  CHECK FROM OTHER INS CO
      *  FROM OTHER IDS PRODUCT
      *  OR WIRE TRANSFER
      *********************************
                 IF    GC03-ACASH NUMERIC                               DOT
      *ADD THE PEND PAY AVAILABLE
           ADD         GC03-ACASH TO PJ52-APPAYA
           MOVE        'Y' TO 7-PEND-PAY-FOUND.
       F96FB-FN. EXIT.
      *N96FG.    NOTE *PENDING PAYMENT NOT AVAILABLE      *.
       F96FG.    IF    (XAVAIL-CALL-TYPE                                lv25
                 OR    GC03-CCOLL = SPACES)
                 NEXT SENTENCE ELSE GO TO     F96FG-FN.
      *********************************
      *VALID CALL TYPES:
      *  PERSONAL CHECK
      *  PRE-AUTHORIZED CHECK
      *  AUTOMATED CLEARING HOUSE
      *  CHARGE
      *********************************
                 IF    GC03-ACASH NUMERIC                               DOT
      *ADD THE PEND PAY NOT AVAILABLE
           ADD         GC03-ACASH TO PJ52-APPAYN
           MOVE        'Y' TO 7-PEND-PAY-FOUND.
       F96FG-FN. EXIT.
       F96DL-FN. EXIT.
       F96BG-FN. EXIT.
      *N96FL.    NOTE *PENDING PAYMENT NOT AVAILABLE      *.
       F96FL.    IF    GC03-CAATY = 002                                 lv15
                 AND   GC03-CASTC = 4
                 NEXT SENTENCE ELSE GO TO     F96FL-FN.
      *********************************
      *SUSPENDED TRANSACTION
      ** *
      *THIS IS THE TEST FOR SUSPENDED
      *ENTRYS ON KD90 WITH ' COLL ' OR
      *'TRF IN' THE TRAN CODE FIELD.
      *********************************
                 IF    GC03-ACASH NUMERIC                               DOT
      *ADD THE PEND PAY NOT AVAILABLE
           ADD         GC03-ACASH TO PJ52-APPAYN
           MOVE        'Y' TO 7-PEND-PAY-FOUND.
       F96FL-FN. EXIT.
       F96BB-FN. EXIT.
      *N96PB.    NOTE *GET PENDING PAYMENT                *.
       F96PB.    IF    VALID-TRANSF-TYPE                                lv10
                 NEXT SENTENCE ELSE GO TO     F96PB-FN.
      *********************************
      *IF DESTINATION TRANSFER TYPE =
      *   EXCHANGE  OR  TRANSFER
      *   OR DOUBLE TRANSFER
      *********************************
      *N96PG.    NOTE *THE DISBURSEMENT IS A FUND         *.
       F96PG.    IF    GC21-CTIDA = 002                                 lv15
                 NEXT SENTENCE ELSE GO TO     F96PG-FN.
      *N96PL.    NOTE *PENDING PAYMENT AVAILABLE          *.
       F96PL.    IF    (GX03-DEFFT = NS20-DCACG                         lv20
                 OR    GX03-DEFFT < NS20-DCACG)
                 NEXT SENTENCE ELSE GO TO     F96PL-FN.
      *********************************
      *TRAN DATE = OR < ACCT DATE
      *********************************
                 IF    GC03-ACASH NUMERIC                               DOT
      *ADD PEND PAY AVAILABLE
           ADD         GC03-ACASH TO PJ52-APPAYA
           MOVE        'Y' TO 7-PEND-PAY-FOUND
               GO TO     F96PB-FN.
       F96PL-FN. EXIT.
      *N96PQ.    NOTE *PENDING PAYMENT NOT AVAILABLE      *.
       F96PQ.    IF    GX03-DEFFT > NS20-DCACG                          lv20
                 NEXT SENTENCE ELSE GO TO     F96PQ-FN.
      *********************************
      *TRAN DATE > ACCT DATE
      *********************************
                 IF    GC03-ACASH NUMERIC                               DOT
      *ADD PEND PAY NOT AVAILABLE
           ADD         GC03-ACASH TO PJ52-APPAYN
           MOVE        'Y' TO 7-PEND-PAY-FOUND.
       F96PQ-FN. EXIT.
       F96PG-FN. EXIT.
      *N96RB.    NOTE *NOT A FUND DISBURSEMENT            *.
       F96RB.    IF    GC21-CTIDA NOT = 002                             lv15
                 NEXT SENTENCE ELSE GO TO     F96RB-FN.
      *********************************
      *CONVERT ENTRY DATE TO GREG
      *********************************
           MOVE        99999999 TO 7-WS-9DAT8
           COMPUTE     7-GX03-DCACG =
           7-WS-9DAT8 - GX03-DCACG9.
      *N96RG.    NOTE *PENDING PAYMENT AVAILABLE          *.
       F96RG.    IF    7-GX03-DCACG < NS20-DCACG                        lv20
                 AND   GX03-DEFFT < NS20-DCACG
                 NEXT SENTENCE ELSE GO TO     F96RG-FN.
      *********************************
      *ENTRY AND TRAN DATE < ACCT DATE
      *TRANSACTION DATE < ACCT DATE
      *********************************
                 IF    GC03-ACASH NUMERIC                               DOT
      *ADD PEND PAY AVAILABLE
           ADD         GC03-ACASH TO PJ52-APPAYA
           MOVE        'Y' TO 7-PEND-PAY-FOUND
               GO TO     F96PB-FN.
       F96RG-FN. EXIT.
      *N96RL.    NOTE *PEND PAY NOT AVAILABLE             *.
       F96RL.    IF    (7-GX03-DCACG = NS20-DCACG                       lv20
                 OR    7-GX03-DCACG < NS20-DCACG)
                 AND   (GX03-DEFFT = NS20-DCACG
                 OR    GX03-DEFFT > NS20-DCACG)
                 NEXT SENTENCE ELSE GO TO     F96RL-FN.
      *********************************
      *********************************
      *ENTRY DATE <= ACCT DATE (DCACG)
      *TRANS DATE >= ACCT DATE (DEFFT)
      *********************************
                 IF    GC03-ACASH NUMERIC                               DOT
      *ADD PEND PAY NOT AVAILABLE
           ADD         GC03-ACASH TO PJ52-APPAYN
           MOVE        'Y' TO 7-PEND-PAY-FOUND.
       F96RL-FN. EXIT.
       F96RB-FN. EXIT.
       F96PB-FN. EXIT.
       F96-FN.   EXIT.
      *N97.      NOTE *************************************.
      *               *                                   *
      *               *ERROR HANDLING FOR BROKER CALL     *
      *               *                                   *
      *               *************************************.
       F97.           EXIT.                                             lv05
      *N97BC.    NOTE *DO THE ERROR HANDLING FOR THE      *.            AAER85
       F97BC.                                                           lv10
      *BROKER CALL DONE AS A PART OF                                    AAER85
      *MFTA PROJECT.                                                    AAER85
      *--------------------------------                                 AAER85
      *INITIALIZE THE VARIABLES USED                                    AAER85
      *TO INDICATE ERROR.                                               AAER85
      *--------------------------------                                 AAER85
           INITIALIZE  WS00-IERRC                                       AAER85
           WS00-CSEVR                                                   AAER85
           WS01-IERRC.                                                  AAER85
      *N97BH.    NOTE *CHECK THE BROKER ERROR.            *.            AAER85
       F97BH.    IF    SQ2L-CSEVR1 NOT = '00'                           lv15
                 AND   SQ2L-CSEVR1 NOT = '04'                           AAER85
                 NEXT SENTENCE ELSE GO TO     F97BH-FN.                 AAER85
      *A RETURN CODE OF '00' IS GOOD.A                                  AAER85
      *RETURN CODE OF '04' IS A WARNING                                 AAER85
      *ON A CLOSE CALL, BUT DATA GETS                                   AAER85
      *RETURNED. IN CASE OF ANY OTHER                                   AAER85
      *RETURN CODE PREPARE ERROR MESG.                                  AAER85
      *--------------------------------                                 AAER85
      *CHECK IF IGNORABLE ERROR                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        SQ2L-CERRE1 TO WS00-CERRE1                       AAER85
           MOVE        'B' TO WS00-ERROR-AREA                           AAER85
           PERFORM     F97TB THRU F97TB-FN.                             AAER85
      *N97BM.    NOTE *NOT IGNORABLE ERROR, PREPARE THE   *.            AAER85
       F97BM.    IF    WS00-IGNORE-ERROR = 'N'                          lv20
                 NEXT SENTENCE ELSE GO TO     F97BM-FN.                 AAER85
      *CORRESPONDING DST VIEW.                                          AAER85
      *--------------------------------                                 AAER85
           MOVE        'B' TO WS00-IERRC                                AAER85
           MOVE        'S' TO WS00-CSEVR                                AAER85
           PERFORM     F97KB THRU F97KB-FN.                             AAER85
      *N97BR.    NOTE *APPEND THE BROKER ERROR AT THE     *.            AAER85
       F97BR.    IF    SQ2L-CERRE1 = '01093'                            lv25
                 NEXT SENTENCE ELSE GO TO     F97BR-FN.                 AAER85
      *END OF THE ERROR MESSAGE. FOR                                    AAER85
      *THE DST ERROR, GET THE TEXT                                      AAER85
      *FROM THE PACTABLE TF0009                                         AAER85
      *--------------------------------                                 AAER85
      *GET THE ERROR TEXT FOR THE                                       AAER85
      *ERROR RETURNED BY DST.                                           AAER85
      *--------------------------------                                 AAER85
           MOVE        SQ2L-CERRE3 TO TF09-CERRE2                       AAER85
           PERFORM     F92TF THRU F92TF-FN                              AAER85
      *--------------------------------                                 AAER85
                 IF    IK NOT = '0'                                     DOT
      *IF PACTABLE READ FAILS, POPULATE                                 AAER85
      *DEFAULT MESSAGE                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE                         'Message information could not bAAER85
      -                'e found.' TO TF09-TERMT3.                       AAER85
      *--------------------------------                                 DOT
      *APPEND THE PB0251 ERROR AT THE                                   AAER85
      *END OF THE ERROR MESSAGE                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        TF09-CERRE2 TO MS03-TMESS4 (55:5)                AAER85
           MOVE        ' - ' TO MS03-TMESS4 (60:3)                      AAER85
           MOVE        TF09-TERMT3 TO MS03-TMESS4 (63:66).              AAER85
       F97BR-900. GO TO F97BV-FN.
       F97BR-FN. EXIT.
      *N97BV.    NOTE *APPEND THE BROKER ERROR AT THE     *.            AAER85
       F97BV.                                                           lv25
      *END OF THE ERROR MESSAGE                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        SQ2L-CERRE1 TO MS03-TMESS4 (55:5)                AAER85
           MOVE        ' - ' TO MS03-TMESS4 (60:3)                      AAER85
           MOVE        SQ2L-TERMT TO MS03-TMESS4 (63:66).               AAER85
       F97BV-FN. EXIT.
       F97BM-900. GO TO F97BY-FN.
       F97BM-FN. EXIT.
      *N97BY.    NOTE *IGNORABLE ERROR. SPACE OUT THE     *.            AAER85
       F97BY.                                                           lv20
      *ERROR IN BROKER AREA.                                            AAER85
      *--------------------------------                                 AAER85
           MOVE ALL    ZERO TO SQ2L-CSEVR1                              AAER85
           INITIALIZE  SQ2L-CERRE1                                      AAER85
      *--------------------------------                                 AAER85
      *POPULATE THE VARIABLE FOR                                        AAER85
      *IGNORABLE BROKER ERROR                                           AAER85
      *--------------------------------                                 AAER85
           MOVE        'B' TO WS01-IERRC                                AAER85
           MOVE        'N' TO WS00-CSEVR.                               AAER85
       F97BY-FN. EXIT.
       F97BH-FN. EXIT.
      *N97CB.    NOTE *CHECK IF ERROR IN THE PB0251       *.            AAER85
       F97CB.                                                           lv15
      *AREA.                                                            AAER85
      *N97CH.    NOTE *IF ERROR FOUND IN PB0251 PREPARE   *.            AAER85
       F97CH.    IF    SQ3L-CAPIR1 NOT = ZERO                           lv20
                 AND   SQ3L-CAPIR1 NOT = 11                             AAER85
                 NEXT SENTENCE ELSE GO TO     F97CH-FN.                 AAER85
      *THE CORRESPONDING ERROR MESSAGE                                  AAER85
      *--------------------------------                                 AAER85
      *CHECK OF IGNORABLE ERROR                                         AAER85
      *--------------------------------                                 AAER85
           MOVE ALL    ZEROS TO WS00-CERRE1                             AAER85
           MOVE        SQ3L-CAPIR1 TO WS00-CERRE1 (3:3)                 AAER85
           MOVE        'P' TO WS00-ERROR-AREA                           AAER85
           PERFORM     F97TB THRU F97TB-FN.                             AAER85
      *N97CM.    NOTE *IF FIRST NON-IGNORABLE ERROR GET   *.            AAER85
       F97CM.    IF    WS00-IGNORE-ERROR = 'N'                          lv25
                 AND   WS00-IERRC = SPACES                              AAER85
                 NEXT SENTENCE ELSE GO TO     F97CM-FN.                 AAER85
      *THE ERROR MESSAGE                                                AAER85
      *--------------------------------                                 AAER85
           MOVE        'P' TO WS00-IERRC                                AAER85
           MOVE        'S' TO WS00-CSEVR                                AAER85
           PERFORM     F97KB THRU F97KB-FN.                             AAER85
      *N97CT.    NOTE *PREPARE THE ERROR MESSAGE          *.            AAER85
       F97CT.                                                           lv30
      *--------------------------------                                 AAER85
      *GET THE ERROR TEXT FOR THE                                       AAER85
      *PB0251 ERROR.                                                    AAER85
      *--------------------------------                                 AAER85
           MOVE        WS00-CERRE1 TO TF09-CERRE2                       AAER85
           PERFORM     F92TF THRU F92TF-FN                              AAER85
      *--------------------------------                                 AAER85
                 IF    IK NOT = '0'                                     DOT
      *IF PACTABLE READ FAILS, POPULATE                                 AAER85
      *DEFAULT MESSAGE                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE                         'Message information could not bAAER85
      -                'e found.' TO TF09-TERMT3.                       AAER85
      *--------------------------------                                 DOT
      *APPEND THE PB0251 ERROR AT THE                                   AAER85
      *END OF THE ERROR MESSAGE                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        TF09-CERRE2 TO MS03-TMESS4 (55:5)                AAER85
           MOVE        ' - ' TO MS03-TMESS4 (60:3)                      AAER85
           MOVE        TF09-TERMT3 TO MS03-TMESS4 (63:66).              AAER85
       F97CT-FN. EXIT.
       F97CM-FN. EXIT.
      *N97CX.    NOTE *IGNORABLE ERROR. SPACE OUT THE     *.            AAER85
       F97CX.    IF    WS00-IGNORE-ERROR NOT = 'N'                      lv25
                 NEXT SENTENCE ELSE GO TO     F97CX-FN.                 AAER85
      *ERROR IN PB251 AREA.                                             AAER85
      *--------------------------------                                 AAER85
           INITIALIZE  SQ3L-CAPIR1.                                     AAER85
                 IF    WS01-IERRC = SPACES                              DOT
      *--------------------------------                                 AAER85
      *IF THIS IS THE FIRST IGNORABLE                                   AAER85
      *ERROR, POPULATE THE INDICATOR                                    AAER85
      *WITH ERROR CODE                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE        'P' TO WS01-IERRC                                AAER85
           MOVE        'N' TO WS00-CSEVR.                               AAER85
       F97CX-FN. EXIT.
       F97CH-FN. EXIT.
       F97CB-FN. EXIT.
      *N97DB.    NOTE *CHECK FOR THE ERROR IN THE         *.            AAER85
       F97DB.                                                           lv15
      *RESPONSE SECTION.                                                AAER85
      *N97DM.    NOTE *IF ERROR IN THE FIXED AREA OF      *.            AAER85
       F97DM.    IF    WL00-RESPONSE (1:5)                              lv20
                       NOT = SPACES                                     AAER85
                 NEXT SENTENCE ELSE GO TO     F97DM-FN.                 AAER85
      *THE RESPONSE SECTION.                                            AAER85
      *--------------------------------                                 AAER85
      *CHECK IF IGNORABLE ERROR.                                        AAER85
      *--------------------------------                                 AAER85
           MOVE        WL00-RESPONSE (1:5) TO WS00-CERRE1               AAER85
           MOVE        'F' TO WS00-ERROR-AREA                           AAER85
           PERFORM     F97TB THRU F97TB-FN.                             AAER85
      *N97DR.    NOTE *IF FIRST NON-IGNORABLE ERROR       *.            AAER85
       F97DR.    IF    WS00-IGNORE-ERROR = 'N'                          lv25
                 AND   WS00-IERRC = SPACES                              AAER85
                 NEXT SENTENCE ELSE GO TO     F97DR-FN.                 AAER85
      *PREPARE THE ERROR MESSAGE                                        AAER85
      *--------------------------------                                 AAER85
           MOVE        'F' TO WS00-IERRC                                AAER85
           MOVE        'S' TO WS00-CSEVR                                AAER85
           PERFORM     F97KB THRU F97KB-FN.                             AAER85
      *N97EB.    NOTE *PREPARE THE ERROR MESSAGE          *.            AAER85
       F97EB.                                                           lv30
      *--------------------------------                                 AAER85
      *GET THE ERROR TEXT FOR THE FIXED                                 AAER85
      *AREA ERROR                                                       AAER85
      *--------------------------------                                 AAER85
           MOVE        WL00-RESPONSE (1:5) TO TF09-CERRE2               AAER85
           PERFORM     F92TF THRU F92TF-FN.                             AAER85
                 IF    IK NOT = '0'                                     DOT
      *--------------------------------                                 AAER85
      *IF PACTABLE READ FAILS, POPULATE                                 AAER85
      *DEFAULT MESSAGE                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE                         'Message information could not bAAER85
      -                'e found.' TO TF09-TERMT3.                       AAER85
      *--------------------------------                                 DOT
      *APPEND THE FIXED AREA ERROR AT                                   AAER85
      *THE END OF THE ERROR MESSAGE                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        TF09-CERRE2 TO MS03-TMESS4 (55:5)                AAER85
           MOVE        ' - ' TO MS03-TMESS4 (60:3)                      AAER85
           MOVE        TF09-TERMT3 TO MS03-TMESS4 (63:66).              AAER85
       F97EB-FN. EXIT.
       F97DR-FN. EXIT.
      *N97EF.    NOTE *IGNORABLE ERROR. SPACE OUT THE     *.            AAER85
       F97EF.    IF    WS00-IGNORE-ERROR NOT = 'N'                      lv25
                 NEXT SENTENCE ELSE GO TO     F97EF-FN.                 AAER85
      *ERROR IN FIXED RESPONSE AREA.                                    AAER85
      *--------------------------------                                 AAER85
           MOVE        SPACES TO WL00-RESPONSE (1:5).                   AAER85
                 IF    WS01-IERRC = SPACES                              DOT
      *--------------------------------                                 AAER85
      *IF THIS IS THE FIRST IGNORABLE                                   AAER85
      *ERROR, POPULATE THE INDICATOR                                    AAER85
      *WITH ERROR CODE                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE        'F' TO WS01-IERRC                                AAER85
           MOVE        'N' TO WS00-CSEVR.                               AAER85
       F97EF-FN. EXIT.
       F97DM-FN. EXIT.
       F97DB-FN. EXIT.
      *N97FB.    NOTE *IF VARIABLE REPEATABLE AREA IS     *.            AAER85
       F97FB.    IF    WS-VAR-GELL > 0                                  lv15
                 NEXT SENTENCE ELSE GO TO     F97FB-FN.                 AAER85
      *PRESENT IN THE RESPONSE SECTION                                  AAER85
      *CHECK FOR THE ERRORS                                             AAER85
      *N97FG.    NOTE *SET THE VARIABLES TO IDENTIFY      *.            AAER85
       F97FG.                                                           lv20
      *THE POSITION OF THE ERROR CODES                                  AAER85
      *IN THE REPEATABLE AREA OF                                        AAER85
      *RESPONSE                                                         AAER85
      *--------------------------------                                 AAER85
           COMPUTE     WS-ERR-GELL = WS-FIX-GELL + 1                    AAER85
      *--------------------------------                                 AAER85
      *GET THE NUMBER OF ROWS RETURNED                                  AAER85
      *--------------------------------                                 AAER85
           MOVE        WL00-RESPONSE (6:3) TO WS-NRURO.                 AAER85
      *N97FL.    NOTE *CHECK FOR ALL THE ROWS IN THE      *.            AAER85
       F97FL.                                                           lv25
           MOVE        1                        TO J97FLR               AAER85
                                    GO TO     F97FL-B.                  AAER85
       F97FL-A.
           ADD         1                        TO J97FLR.              AAER85
       F97FL-B.
           IF          J97FLR                   >  WS-NRURO             AAER85
                                    GO TO     F97FL-FN.                 AAER85
      *VARIABLE REPEATABLE AREA.                                        AAER85
      *--------------------------------                                 AAER85
           MOVE        'V' TO WS00-ERROR-AREA.                          AAER85
      *N97FQ.    NOTE *ERROR IN IN THE RESPONSE SECTION   *.            AAER85
       F97FQ.    IF    WL00-RESPONSE (WS-ERR-GELL:5)                    lv30
                       NOT = SPACES                                     AAER85
                 NEXT SENTENCE ELSE GO TO     F97FQ-FN.                 AAER85
      *--------------------------------                                 AAER85
      *CHECK IF IGNORABLE ERROR                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        WL00-RESPONSE (WS-ERR-GELL:5) TO                 AAER85
           WS00-CERRE1                                                  AAER85
           PERFORM     F97TB THRU F97TB-FN.                             AAER85
      *N97FV.    NOTE *GET THE ERROR MESSAGE FOR NON      *.            AAER85
       F97FV.    IF    WS00-IGNORE-ERROR = 'N'                          lv35
                 AND   WS00-IERRC = SPACES                              AAER85
                 NEXT SENTENCE ELSE GO TO     F97FV-FN.                 AAER85
      *IGNORABLE ERROR RETURNED BY THE                                  AAER85
      *DST VIEW.                                                        AAER85
      *--------------------------------                                 AAER85
           MOVE        'V' TO WS00-IERRC                                AAER85
           MOVE        'S' TO WS00-CSEVR                                AAER85
           PERFORM     F97KB THRU F97KB-FN.                             AAER85
      *N97GB.    NOTE *PREPARE THE ERROR MESSAGE          *.            AAER85
       F97GB.                                                           lv40
      *--------------------------------                                 AAER85
      *GET THE ERROR TEXT FOR THE                                       AAER85
      *VARIABLE AREA ERROR                                              AAER85
      *--------------------------------                                 AAER85
           MOVE        WL00-RESPONSE (WS-ERR-GELL:5) TO                 AAER85
           TF09-CERRE2                                                  AAER85
           PERFORM     F92TF THRU F92TF-FN.                             AAER85
                 IF    IK NOT = '0'                                     DOT
      *--------------------------------                                 AAER85
      *IF PACTABLE READ FAILS, POPULATE                                 AAER85
      *DEFAULT MESSAGE                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE                         'Message information could not bAAER85
      -                'e found.' TO TF09-TERMT3.                       AAER85
      *--------------------------------                                 DOT
      *APPEND THE REPEATABLE AREA ERROR                                 AAER85
      *AT THE END OF THE ERROR MESSAGE                                  AAER85
      *--------------------------------                                 AAER85
           MOVE        TF09-CERRE2 TO MS03-TMESS4 (55:5)                AAER85
           MOVE        ' - ' TO MS03-TMESS4 (60:3)                      AAER85
           MOVE        TF09-TERMT3 TO MS03-TMESS4 (63:66).              AAER85
       F97GB-FN. EXIT.
       F97FV-FN. EXIT.
      *N97GH.    NOTE *IGNORABLE ERROR. SPACE OUT THE     *.            AAER85
       F97GH.    IF    WS00-IGNORE-ERROR NOT = 'N'                      lv35
                 NEXT SENTENCE ELSE GO TO     F97GH-FN.                 AAER85
      *ERROR IN FIXED RESPONSE AREA.                                    AAER85
      *--------------------------------                                 AAER85
           MOVE        SPACES TO                                        AAER85
           WL00-RESPONSE (WS-ERR-GELL:5).                               AAER85
                 IF    WS01-IERRC = SPACES                              DOT
      *--------------------------------                                 AAER85
      *IF THIS IS THE FIRST IGNORABLE                                   AAER85
      *ERROR, POPULATE THE INDICATOR                                    AAER85
      *WITH ERROR CODE                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE        'V' TO WS01-IERRC                                AAER85
           MOVE        'N' TO WS00-CSEVR.                               AAER85
       F97GH-FN. EXIT.
       F97FQ-FN. EXIT.
      *N97GM.    NOTE *ADD THE LENGTH OF THE VARIABLE     *.            AAER85
       F97GM.                                                           lv30
      *AREA ROW TO GET THE POSITION OF                                  AAER85
      *NEXT ERROR FIELD.                                                AAER85
      *--------------------------------                                 AAER85
           ADD         WS-VAR-GELL TO WS-ERR-GELL.                      AAER85
       F97GM-FN. EXIT.
       F97FL-900. GO TO F97FL-A.
       F97FL-FN. EXIT.
       F97FG-FN. EXIT.
       F97FB-FN. EXIT.
       F97BC-FN. EXIT.
      *N97KB.    NOTE *GET THE GENERIC ERROR MESSAGE      *.            AAER85
       F97KB.                                                           lv10
      *FOR THE DST VIEW.                                                AAER85
      *N97KH.    NOTE *GET THE ERROR NUMBER FOR THE       *.            AAER85
       F97KH.                                                           lv15
      *GENERIC ERROR MESSAGE FOR THE                                    AAER85
      *DST VIEW.                                                        AAER85
      *N97KM.    NOTE *ERROR IN THE ACCOUNT INFO. VIEW    *.            AAER85
       F97KM.    IF    SQ1L-NVIEW =                                     lv20
                       '2933'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F97KM-FN.                 AAER85
      *CALL.                                                            AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *ACCOUNT INFO. VIEW ERROR.                                        AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14516 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14551 TO MS03-NMESS2.                            AAER85
       F97KM-900. GO TO F97KH-FN.
       F97KM-FN. EXIT.
      *N97KO.    NOTE *ERROR IN THE MCB ACCOUNT INFO      *.            AAER85
       F97KO.    IF    SQ1L-NVIEW =                                     lv20
                       '4838'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F97KO-FN.                 AAER85
      *VIEW - 4838 CALL.                                                AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *MCB ACCOUNT INFO VIEW ERROR.                                     AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        15611 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        15612 TO MS03-NMESS2.                            AAER85
       F97KO-900. GO TO F97KH-FN.
       F97KO-FN. EXIT.
      *N97KQ.    NOTE *ERROR IN THE MCB GAIN/LOSS         *.            AAER85
       F97KQ.    IF    SQ1L-NVIEW =                                     lv20
                       '4865'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F97KQ-FN.                 AAER85
      *INFORMATION VIEW - 4865 CALL.                                    AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *MCB GAIN/LOSS INFORMATION VIEW                                   AAER85
      *ERROR.                                                           AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14536 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14571 TO MS03-NMESS2.                            AAER85
       F97KQ-900. GO TO F97KH-FN.
       F97KQ-FN. EXIT.
      *N97KR.    NOTE *ERROR IN THE TRAN. INFO (SINGLE)   *.            AAER85
       F97KR.    IF    SQ1L-NVIEW =                                     lv20
                       '2934'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F97KR-FN.                 AAER85
      *VIEW.                                                            AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *TRAN. INFO. (SINGLE) VIEW.                                       AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14517 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14552 TO MS03-NMESS2.                            AAER85
       F97KR-900. GO TO F97KH-FN.
       F97KR-FN. EXIT.
      *N97KW.    NOTE *ERROR IN THE TRAN. INFO (LIST)     *.            AAER85
       F97KW.    IF    SQ1L-NVIEW =                                     lv20
                       '2935'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F97KW-FN.                 AAER85
      *VIEW.                                                            AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *TRAN. INFO. (LIST) VIEW.                                         AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14518 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14553 TO MS03-NMESS2.                            AAER85
       F97KW-900. GO TO F97KH-FN.
       F97KW-FN. EXIT.
      *N97LC.    NOTE *ERROR IN THE GROUP INFO. VIEW      *.            AAER85
       F97LC.    IF    SQ1L-NVIEW =                                     lv20
                       '2939'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F97LC-FN.                 AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *GROUP INFO. VIEW.                                                AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14520 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14555 TO MS03-NMESS2.                            AAER85
       F97LC-900. GO TO F97KH-FN.
       F97LC-FN. EXIT.
      *N97LH.    NOTE *ERROR IN THE ACCOUNT VALUE VIEW    *.            AAER85
       F97LH.    IF    SQ1L-NVIEW =                                     lv20
                       '2949'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F97LH-FN.                 AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *ACCOUNT VALUE VIEW.                                              AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14533 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14568 TO MS03-NMESS2.                            AAER85
       F97LH-900. GO TO F97KH-FN.
       F97LH-FN. EXIT.
      *N97LM.    NOTE *ERROR IN THE GOOD FUNDS VIEW       *.            AAER85
       F97LM.    IF    SQ1L-NVIEW =                                     lv20
                       '2940'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F97LM-FN.                 AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *GOOD FUNDS VIEW.                                                 AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14534 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14569 TO MS03-NMESS2.                            AAER85
       F97LM-900. GO TO F97KH-FN.
       F97LM-FN. EXIT.
      *N97LR.    NOTE *ERROR IN THE CDSC FJ8X VIEW        *.            AAER85
       F97LR.    IF    SQ1L-NVIEW =                                     lv20
                       '2903'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F97LR-FN.                 AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *CDSC FJ8X VIEW.                                                  AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14528 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14563 TO MS03-NMESS2.                            AAER85
       F97LR-900. GO TO F97KH-FN.
       F97LR-FN. EXIT.
      *N97LW.    NOTE *ERROR IN THE CDSC FJXC VIEW        *.            AAER85
       F97LW.    IF    SQ1L-NVIEW =                                     lv20
                       '2907'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F97LW-FN.                 AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *CDSC FJXC VIEW.                                                  AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14532 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14567 TO MS03-NMESS2.                            AAER85
       F97LW-900. GO TO F97KH-FN.
       F97LW-FN. EXIT.
      *N97PB.    NOTE *OTHER ERROR                        *.            AAER85
       F97PB.                                                           lv20
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *INVALID VIEW.                                                    AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14550 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14585 TO MS03-NMESS2.                            AAER85
       F97PB-FN. EXIT.
       F97KH-FN. EXIT.
      *N97PH.    NOTE *PREPARE THE ERROR MESSAGE          *.            AAER85
       F97PH.                                                           lv15
      *--------------------------------                                 AAER85
      *    SET MESSAGE SEVERITY                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        11 TO MS03-CMESB                                 AAER85
      *--------------------------------                                 AAER85
      *GET THE ERROR MESSAGE TEXT                                       AAER85
      *--------------------------------                                 AAER85
           PERFORM     F98GM THRU F98GM-FN.                             AAER85
      *N97PM.    NOTE *IF ERROR MESSAGE NOT FOUND IN      *.            AAER85
       F97PM.    IF    MS03-CMSSF = 'UN'                                lv20
                 NEXT SENTENCE ELSE GO TO     F97PM-FN.                 AAER85
      *TBDU93, POPULATE DEFAULT ERROR                                   AAER85
      *MESSAGE'                                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        'Message info. not found' TO                     AAER85
           MS03-TMESS4.                                                 AAER85
       F97PM-FN. EXIT.
       F97PH-FN. EXIT.
      *N97PR.    NOTE *SET THE MESSAGE LENGTH AND         *.            AAER85
       F97PR.                                                           lv15
      *APPEND THE PROGRAM NAME AT THE                                   AAER85
      *END OF THE MESSAGE.                                              AAER85
      *--------------------------------                                 AAER85
           MOVE        128 TO MS03-QELLAA                               AAER85
           MOVE        ' Program : ' TO                                 AAER85
           MS03-TMESS4 (28:11)                                          AAER85
           MOVE        PROGE TO MS03-TMESS4 (39:8)                      AAER85
           MOVE        ' Reason ' TO MS03-TMESS4 (47:8).                AAER85
       F97PR-FN. EXIT.
       F97KB-FN. EXIT.
      *N97TB.    NOTE *DEPENDING ON THE TYPE OF ERROR     *.            AAER85
       F97TB.                                                           lv10
      *HANDLING REQUIRED, POPULATE THE                                  AAER85
      *ERROR CODE IN APPROPRIATE FIELD.                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        'N' TO WS00-IGNORE-ERROR.                        AAER85
      *N97TG.    NOTE *FOR REQUEST TO IGNORE THE LIST     *.            AAER85
       F97TG.    IF    WS00-ERROR-TYPE =                                lv15
                       'L'                                              AAER85
                 NEXT SENTENCE ELSE GO TO     F97TG-FN.                 AAER85
      *OF THE ERRORS, SEARCH THE ERROR                                  AAER85
      *IN THE LIST.                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE 1 TO     IWE00R.                                        AAER85
       F97TG-080. IF     IWE00R NOT >    IWE00L
           AND           WE00-CERRE1    (IWE00R)                        AAER85
           NOT =           WS00-CERRE1                                  AAER85
           ADD 1 TO      IWE00R    GO TO F97TG-080.                     AAER85
      *--------------------------------                                 AAER85
                 IF    IWE00R <= IWE00L                                 DOT
      *IF THE ERROR NEEDS TO IGNORE                                     AAER85
      *POPULATE THE WORKING STORAGE                                     AAER85
      *VARIABLE TO INDICATE IT.                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        'Y' TO WS00-IGNORE-ERROR.                        AAER85
      *--------------------------------                                 DOT
       F97TG-900. GO TO F97TB-FN.
       F97TG-FN. EXIT.
      *N97TL.    NOTE *FOR IGNORE ALL THE ERROR           *.            AAER85
       F97TL.    IF    WS00-ERROR-TYPE =                                lv15
                       'A'                                              AAER85
                 NEXT SENTENCE ELSE GO TO     F97TL-FN.                 AAER85
      *REQUEST POPULATE THE WORKING                                     AAER85
      *STORAGE VARIABLE TO INDICATE                                     AAER85
      *IGNORABLE ERROR                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE        'Y' TO WS00-IGNORE-ERROR.                        AAER85
       F97TL-900. GO TO F97TB-FN.
       F97TL-FN. EXIT.
      *N97TS.    NOTE *FOR IGNORE ALL APPLICATION AREA    *.            AAER85
       F97TS.    IF    WS00-ERROR-TYPE =                                lv15
                       'R'                                              AAER85
                 NEXT SENTENCE ELSE GO TO     F97TS-FN.                 AAER85
      *(RESPONSE) ERRORS.                                               AAER85
                 IF    WS00-ERROR-AREA NOT = 'B'                        DOT
                 AND   WS00-ERROR-AREA NOT = 'P'                        AAER85
      *--------------------------------                                 AAER85
      *FOR THE RESPONSE AREA ERRORS                                     AAER85
      *POPULATE THE WORKING STORAGE                                     AAER85
      *VARIABLE FOR IGNORABLE ERROR                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        'Y' TO WS00-IGNORE-ERROR.                        AAER85
       F97TS-900. GO TO F97TB-FN.
       F97TS-FN. EXIT.
      *N97VB.    NOTE *FOR ANY OTHER TYPE OF ERRORS       *.            AAER85
       F97VB.                                                           lv15
      *HANDLING REQUEST DON'T IGNORE                                    AAER85
      *ERROR                                                            AAER85
       F97VB-FN. EXIT.
       F97TB-FN. EXIT.
       F97-FN.   EXIT.
      *N98.      NOTE *************************************.            ADU102
      *               *                                   *             ADU102
      *               *---> Common Performed Routines     *             ADU102
      *               *                                   *             ADU102
      *               *************************************.            ADU102
       F98.           EXIT.                                             lv05
      *N98DS.    NOTE *CALL DBI5000N                      *.
       F98DS.                                                           lv10
           CALL        DBI5000N USING                                   ACLDBI
           DFHEIBLK DFHCOMMAREA                                         ACLDBI
           WL00-REQUEST SQ1L                                            ACLDBI
           WL00-RESPONSE                                                ACLDBI
           SQ2L SQ3L.                                                   ACLDBI
       F98DS-FN. EXIT.
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
