       IDENTIFICATION DIVISION.                                         CI0080
       PROGRAM-ID.  CI0080P.                                            CI0080
      *AUTHOR.         M\M - UPDATE CONTRACT LEVEL.                     CI0080
      *DATE-COMPILED.   09/08/14.                                       CI0080
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
       ENVIRONMENT DIVISION.                                            CI0080
       CONFIGURATION SECTION.                                           CI0080
       SOURCE-COMPUTER. IBM-370.                                        CI0080
       OBJECT-COMPUTER. IBM-370.                                        CI0080
       DATA DIVISION.                                                   CI0080
       WORKING-STORAGE SECTION.                                         CI0080
       01  7-AOACFM-WORKAREA.                                           AOACFM
      *!WI pl=CF020                                                     AOACFM
           05  7-AOACFM-CLID                                            AOACFM
                        PICTURE X(23).                                  CI0080
      *!WI pl=CF030                                                     AOACFM
           05  7-AOACFM-CACKD                                           AOACFM
                        PICTURE 9.                                      CI0080
      *!WI pl=CF040                                                     AOACFM
           05  7-AOACFM-DCACG                                           AOACFM
                        PICTURE 9(8).                                   CI0080
      *!WI pl=CF050                                                     AOACFM
           05  7-AOACFM-CRETP                                           AOACFM
                        PICTURE X.                                      CI0080
      *INITIALIZE GQ01 AND GS51 AREAS                                   AOACFM
      *!WF DSP=II DSL=GQ SEL=01 FOR=I DES=2 LEV=1                       AOACFM
       01                 II01.                                         CI0080
            10            II01-GELL   PICTURE  9(4)                     CI0080
                          VALUE                ZERO                     CI0080
                          BINARY.                                       CI0080
            10            II01-GMISC.                                   CI0080
            11            II01-GS00.                                    CI0080
            12            II01-GT01.                                    CI0080
            13            II01-GQ01K.                                   CI0080
            14            II01-CANUMB PICTURE  X(27)                    CI0080
                          VALUE                SPACE.                   CI0080
            14            II01-CAMCTR PICTURE  9(5)                     CI0080
                          VALUE                ZERO.                    CI0080
            14            II01-GESQ2  PICTURE  99                       CI0080
                          VALUE                ZERO.                    CI0080
            12            II01-GT02                                     CI0080
                          REDEFINES            II01-GT01.               CI0080
            13            II01-C199.                                    CI0080
            14            II01-CLID.                                    CI0080
            15            II01-CLIDO  PICTURE  9(3).                    CI0080
            15            II01-CLIDN.                                   CI0080
            16            II01-CLIDNP PICTURE  X(12).                   CI0080
            16            II01-CLIDND PICTURE  9(8).                    CI0080
            12            II01-GT03                                     CI0080
                          REDEFINES            II01-GT01.               CI0080
            13            II01-C299.                                    CI0080
            14            II01-CTID.                                    CI0080
            15            II01-CTIDA  PICTURE  9(3).                    CI0080
            15            II01-CTIDN.                                   CI0080
            16            II01-CTIDNP PICTURE  X(13).                   CI0080
            16            II01-CTIDND PICTURE  9(11).                   CI0080
            12            II01-GT04                                     CI0080
                          REDEFINES            II01-GT01.               CI0080
            13            II01-NPBN   PICTURE  X(20).                   CI0080
            12            II01-GT05                                     CI0080
                          REDEFINES            II01-GT01.               CI0080
            13            II01-GR98.                                    CI0080
            14            II01-GRID.                                    CI0080
            15            II01-GRIDC  PICTURE  9(3).                    CI0080
            15            II01-GRIDN.                                   CI0080
            16            II01-GRIDNP PICTURE  99.                      CI0080
            16            II01-GRIDND PICTURE  9(8).                    CI0080
            12            II01-GT06                                     CI0080
                          REDEFINES            II01-GT01.               CI0080
            13            II01-NTR    PICTURE  9(8).                    CI0080
            12            II01-GT07                                     CI0080
                          REDEFINES            II01-GT01.               CI0080
            13            II01-NTRAC  PICTURE  9(14).                   CI0080
            12            II01-GT08                                     CI0080
                          REDEFINES            II01-GT01.               CI0080
            13            II01-NSRAB  PICTURE  9(7).                    CI0080
            13            II01-GECKD  PICTURE  9.                       CI0080
            13            II01-NBLCK  PICTURE  9(5).                    CI0080
            13            II01-CTRID  PICTURE  X(4).                    CI0080
            12            II01-GT19                                     CI0080
                          REDEFINES            II01-GT01.               CI0080
            13            II01-GEOPD2 PICTURE  X(8).                    CI0080
            12            II01-CACKD  PICTURE  9                        CI0080
                          VALUE                ZERO.                    CI0080
            12            II01-CENTT  PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            12            II01-CADATE PICTURE  X(8)                     CI0080
                          VALUE                SPACE.                   CI0080
            12            II01-GETIM  PICTURE  S9(7)                    CI0080
                          VALUE                ZERO                     CI0080
                          COMPUTATIONAL-3.                              CI0080
            12            II01-GEOPID PICTURE  X(6)                     CI0080
                          VALUE                SPACE.                   CI0080
            12            II01-CAUNIT PICTURE  X(4)                     CI0080
                          VALUE                SPACE.                   CI0080
            12            II01-XTERMI PICTURE  X(08)                    CI0080
                          VALUE                SPACE.                   CI0080
            12            II01-CAPPL  PICTURE  X(8)                     CI0080
                          VALUE                SPACE.                   CI0080
            12            II01-CSYS   PICTURE  X(4)                     CI0080
                          VALUE                SPACE.                   CI0080
            12            II01-NTRSU  PICTURE  999                      CI0080
                          VALUE                ZERO.                    CI0080
            12            II01-FILLER PICTURE  X(20)                    CI0080
                          VALUE                SPACE.                   CI0080
            11            II01-XMISL  PICTURE  X(599)                   CI0080
                          VALUE                SPACE.                   CI0080
       01                 II51.                                         CI0080
            10            II51-CRETP  PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            II51-ICSWD  PICTURE  X(1)                     CI0080
                          VALUE                SPACE.                   CI0080
            10            II51-FILLER PICTURE  X(09)                    CI0080
                          VALUE                SPACE.                   CI0080
      *!WF DSP=II DSL=GS SEL=51 FOR=I DES=2 LEV=1                       AOACFM
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01                 CT01.                                         CI0080
            10            CT01-CT01K.                                   CI0080
            11            CT01-C299.                                    CI0080
            12            CT01-CTID.                                    CI0080
            13            CT01-CTIDA  PICTURE  9(3).                    CI0080
            13            CT01-CTIDN.                                   CI0080
            14            CT01-CTIDNP PICTURE  X(13).                   CI0080
            14            CT01-CTIDND PICTURE  9(11).                   CI0080
            10            CT01-GECKD  PICTURE  9.                       CI0080
            10            CT01-GEMDA  PICTURE  9(8).                    CI0080
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0080
                          BINARY.                                       CI0080
            10            CT01-GECUC  PICTURE  99.                      CI0080
            10            CT01-CTAUL  PICTURE  9(3).                    CI0080
            10            CT01-DIRAC  PICTURE  9(4).                    CI0080
            10            CT01-CTCCI  PICTURE  X.                       CI0080
            10            CT01-CTCUS  PICTURE  999.                     CI0080
            10            CT01-CTEFD  PICTURE  9(8).                    CI0080
            10            CT01-CTIAD  PICTURE  9(8).                    CI0080
            10            CT01-CLCUS  PICTURE  99.                      CI0080
            10            CT01-CAMMB  PICTURE  X(3).                    CI0080
            10            CT01-CKPMM  PICTURE  X.                       CI0080
            10            CT01-CTLAD  PICTURE  9(8).                    CI0080
            10            CT01-IPERS  PICTURE  X.                       CI0080
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            CT01-CTLAT  PICTURE  9(8).                    CI0080
            10            CT01-CTLATC PICTURE  9(6).                    CI0080
            10            CT01-IMEGA  PICTURE  X.                       CI0080
            10            CT01-DIRAB  PICTURE  9(8).                    CI0080
            10            CT01-COLRQ  PICTURE  X.                       CI0080
            10            CT01-ZDA04  PICTURE  X(4).                    CI0080
            10            CT01-CTLPD  PICTURE  9(8).                    CI0080
            10            CT01-CIRASP PICTURE  9.                       CI0080
            10            CT01-CIRATP PICTURE  99.                      CI0080
            10            CT01-DRTHC  PICTURE  9(8).                    CI0080
            10            CT01-CPPTC  PICTURE  X.                       CI0080
            10            CT01-ZDA06  PICTURE  X(6).                    CI0080
            10            CT01-CTACD  PICTURE  9(8).                    CI0080
            10            CT01-CTNLI  PICTURE  X.                       CI0080
            10            CT01-CTRHO  PICTURE  9(8).                    CI0080
            10            CT01-CTSGD  PICTURE  9(8).                    CI0080
            10            CT01-CPATP  PICTURE  X(1).                    CI0080
            10            CT01-IRSTA  PICTURE  X.                       CI0080
            10            CT01-CTSTA  PICTURE  99.                      CI0080
            10            CT01-CTSSC  PICTURE  99.                      CI0080
            10            CT01-PRLIN  PICTURE  9(3).                    CI0080
            10            CT01-PRCOD  PICTURE  9(5).                    CI0080
            10            CT01-PRSCD  PICTURE  X(9).                    CI0080
            10            CT01-CTLNI  PICTURE  X.                       CI0080
            10            CT01-AYSIDA PICTURE  9(3).                    CI0080
            10            CT01-AYSID  PICTURE  9(5).                    CI0080
            10            CT01-CTBMC  PICTURE  99.                      CI0080
            10            CT01-CINAR  PICTURE  99.                      CI0080
            10            CT01-CPHTR  PICTURE  X.                       CI0080
            10            CT01-CDSTR  PICTURE  XX.                      CI0080
            10            CT01-CQACT  PICTURE  999.                     CI0080
            10            CT01-CIRAS  PICTURE  999.                     CI0080
            10            CT01-CIRAT  PICTURE  999.                     CI0080
            10            CT01-CLRAY  PICTURE  9(5).                    CI0080
            10            CT01-CATTP  PICTURE  X.                       CI0080
       01                 CX01.                                         CI0080
            10            CX01-CX01K.                                   CI0080
            11            CX01-C199.                                    CI0080
            12            CX01-CLID.                                    CI0080
            13            CX01-CLIDO  PICTURE  9(3).                    CI0080
            13            CX01-CLIDN.                                   CI0080
            14            CX01-CLIDNP PICTURE  X(12).                   CI0080
            14            CX01-CLIDND PICTURE  9(8).                    CI0080
            10            CX01-GEMDA  PICTURE  9(8).                    CI0080
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0080
                          BINARY.                                       CI0080
            10            CX01-FILLER PICTURE  X(5).                    CI0080
       01                 CX03.                                         CI0080
            10            CX03-GELL   PICTURE  9(4)                     CI0080
                          BINARY.                                       CI0080
            10            CX03-CY00.                                    CI0080
            11            CX03-CX03K.                                   CI0080
            12            CX03-CARTY  PICTURE  99.                      CI0080
            12            CX03-NARRS  PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX03-CARST  PICTURE  99.                      CI0080
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX03-CPMTG  PICTURE  99.                      CI0080
            11            CX03-GRCRNG PICTURE  9(3).                    CI0080
            11            CX03-DEXDT  PICTURE  9(8).                    CI0080
            11            CX03-DASUP  PICTURE  9(8).                    CI0080
            11            CX03-CSTEC  PICTURE  X(3).                    CI0080
            11            CX03-FILLER PICTURE  X(17).                   CI0080
            11            CX03-CY50.                                    CI0080
            12            CX03-NARID  PICTURE  X(30).                   CI0080
            11            CX03-CY51                                     CI0080
                          REDEFINES            CX03-CY50.               CI0080
            12            CX03-NDIDN  PICTURE  9(12).                   CI0080
            12            CX03-FILLER PICTURE  X(18).                   CI0080
            11            CX03-CY52                                     CI0080
                          REDEFINES            CX03-CY50.               CI0080
            12            CX03-NAIDC  PICTURE  9(12).                   CI0080
            12            CX03-FILLER PICTURE  X(18).                   CI0080
            11            CX03-CY53                                     CI0080
                          REDEFINES            CX03-CY50.               CI0080
            12            CX03-NAMEXB PICTURE  9(15).                   CI0080
            12            CX03-FILLER PICTURE  X(15).                   CI0080
            10            CX03-CY99.                                    CI0080
            11            CX03-FILLER PICTURE  X(109).                  CI0080
            10            CX03-CY01                                     CI0080
                          REDEFINES            CX03-CY99.               CI0080
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX03-ICPCI  PICTURE  X.                       CI0080
            11            CX03-CLUPD  PICTURE  9(3).                    CI0080
            11            CX03-DLAUP  PICTURE  9(8).                    CI0080
            11            CX03-CWRC   PICTURE  99.                      CI0080
            11            CX03-CHCR   PICTURE  99.                      CI0080
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0080
            11            CX03-GEAUN  PICTURE  9(5).                    CI0080
            11            CX03-DPCHD  PICTURE  9(8).                    CI0080
            11            CX03-DLRCHK PICTURE  9(8).                    CI0080
            11            CX03-QTRCHK PICTURE  9(2).                    CI0080
            11            CX03-DNPMT  PICTURE  9(8).                    CI0080
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            CX03-CY02                                     CI0080
                          REDEFINES            CX03-CY99.               CI0080
            11            CX03-QSIRQ  PICTURE  99.                      CI0080
            11            CX03-QDRMN  PICTURE  9(2)                     CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX03-DDPRE  PICTURE  9(8).                    CI0080
            11            CX03-DDSHP  PICTURE  9(8).                    CI0080
            11            CX03-NDRFTB PICTURE  9(5).                    CI0080
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0080
            11            CX03-DDSHPA PICTURE  9(8).                    CI0080
            11            CX03-NDRFTF PICTURE  9(5).                    CI0080
            11            CX03-QDIPBK PICTURE  9(3).                    CI0080
            11            CX03-CREOR  PICTURE  X(1).                    CI0080
            11            CX03-CREOR1 PICTURE  X(1).                    CI0080
            11            CX03-DDASC  PICTURE  9(8).                    CI0080
            11            CX03-FILLER PICTURE  X(7).                    CI0080
            10            CX03-CY03                                     CI0080
                          REDEFINES            CX03-CY99.               CI0080
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0080
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0080
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0080
            11            CX03-DOPDA  PICTURE  99.                      CI0080
            11            CX03-CPMTF  PICTURE  99.                      CI0080
            11            CX03-CIRMO  PICTURE  X(12).                   CI0080
            11            CX03-CPALL  PICTURE  X(1).                    CI0080
            11            CX03-CCOLM  PICTURE  9(2).                    CI0080
            11            CX03-CBLTP  PICTURE  X(1).                    CI0080
            11            CX03-CASUB  PICTURE  9(2).                    CI0080
            11            CX03-CBLFM  PICTURE  9(2).                    CI0080
            11            CX03-IBILS  PICTURE  X.                       CI0080
            11            CX03-IPAOS  PICTURE  X.                       CI0080
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0080
            11            CX03-DLBPD  PICTURE  9(8).                    CI0080
            11            CX03-DNBPD  PICTURE  9(8).                    CI0080
            11            CX03-DODBD  PICTURE  9(8).                    CI0080
            11            CX03-CPSRE  PICTURE  99.                      CI0080
            11            CX03-ISPHN  PICTURE  X.                       CI0080
            11            CX03-TCARR  PICTURE  X(6).                    CI0080
            11            CX03-CBKPT  PICTURE  9(2).                    CI0080
            11            CX03-IECNT  PICTURE  X.                       CI0080
            11            CX03-ICONV  PICTURE  X(1).                    CI0080
            11            CX03-FILLER PICTURE  X(4).                    CI0080
            10            CX03-CY04                                     CI0080
                          REDEFINES            CX03-CY99.               CI0080
            11            CX03-CCARD  PICTURE  X(02).                   CI0080
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0080
            11            CX03-IREMT  PICTURE  X(01).                   CI0080
            11            CX03-ISBILA PICTURE  X.                       CI0080
            11            CX03-DLBPDA PICTURE  9(8).                    CI0080
            11            CX03-DNBPDA.                                  CI0080
            12            CX03-DNCYM  PICTURE  9(6).                    CI0080
            12            CX03-CEDTD  PICTURE  9(2).                    CI0080
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX03-DREMT  PICTURE  9(8).                    CI0080
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0080
            11            CX03-CWRC2  PICTURE  99.                      CI0080
            11            CX03-CHCR2  PICTURE  99.                      CI0080
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0080
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0080
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0080
       01                 CX06.                                         CI0080
            10            CX06-CX06K.                                   CI0080
            11            CX06-C299.                                    CI0080
            12            CX06-CTID.                                    CI0080
            13            CX06-CTIDA  PICTURE  9(3).                    CI0080
            13            CX06-CTIDN.                                   CI0080
            14            CX06-CTIDNP PICTURE  X(13).                   CI0080
            14            CX06-CTIDND PICTURE  9(11).                   CI0080
            10            CX06-NPECK  PICTURE  9(02).                   CI0080
            10            CX06-FILLER PICTURE  X.                       CI0080
       01                 CX13.                                         CI0080
            10            CX13-GELL   PICTURE  9(4)                     CI0080
                          BINARY.                                       CI0080
            10            CX13-CY20.                                    CI0080
            11            CX13-CX13K.                                   CI0080
            12            CX13-CARTZ  PICTURE  99.                      CI0080
            12            CX13-NAPDS  PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX13-GESTD  PICTURE  9(8).                    CI0080
            11            CX13-GEEND  PICTURE  9(8).                    CI0080
            11            CX13-DASUQ  PICTURE  9(8).                    CI0080
            11            CX13-CDEST  PICTURE  99.                      CI0080
            11            CX13-IIARR  PICTURE  X.                       CI0080
            11            CX13-DLAUP  PICTURE  9(8).                    CI0080
            11            CX13-GEOPD2 PICTURE  X(8).                    CI0080
            11            CX13-GEAUN  PICTURE  9(5).                    CI0080
            11            CX13-DPCHD  PICTURE  9(8).                    CI0080
            11            CX13-PPOT1  PICTURE  S9(3)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX13-ACOT1  PICTURE  S9(9)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX13-QPST1  PICTURE  S9(7)V999                CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX13-FILLER PICTURE  X(03).                   CI0080
            10            CX13-CY96.                                    CI0080
            11            CX13-FILLER PICTURE  X(50).                   CI0080
            10            CX13-CY21                                     CI0080
                          REDEFINES            CX13-CY96.               CI0080
            11            CX13-DNPMT  PICTURE  9(8).                    CI0080
            11            CX13-CPMTF  PICTURE  99.                      CI0080
            11            CX13-ADBRQ  PICTURE  S9(11)V99                CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX13-QSHOWQ PICTURE  S9(9)V999                CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX13-PACT1  PICTURE  S999V999                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX13-DOPDA  PICTURE  99.                      CI0080
            11            CX13-DNEXE  PICTURE  9(8).                    CI0080
            11            CX13-CIRMO  PICTURE  X(12).                   CI0080
            10            CX13-CY98.                                    CI0080
            11            CX13-FILLER PICTURE  X(120).                  CI0080
            10            CX13-CY25                                     CI0080
                          REDEFINES            CX13-CY98.               CI0080
            11            CX13-COPTC  PICTURE  9(1).                    CI0080
            11            CX13-ILPOI  PICTURE  X(1).                    CI0080
            11            CX13-CATOC  PICTURE  X(1).                    CI0080
            11            CX13-CEOIA  PICTURE  S9(7)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX13-ACOAR  PICTURE  S9(9)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX13-CEOTR  PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX13-DSTMO  PICTURE  99.                      CI0080
            10            CX13-CY27                                     CI0080
                          REDEFINES            CX13-CY98.               CI0080
            11            CX13-QMTH1  PICTURE  9(3).                    CI0080
            11            CX13-IDRMD  PICTURE  X.                       CI0080
            10            CX13-CY28                                     CI0080
                          REDEFINES            CX13-CY98.               CI0080
            11            CX13-AALLBL PICTURE  S9(8)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX13-PSURR  PICTURE  S9(3)V999                CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX13-DFPMT  PICTURE  9(8).                    CI0080
            11            CX13-QMTHLA PICTURE  9(3).                    CI0080
            11            CX13-PWHLDS PICTURE  S999V9(5)                CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX13-ISWHO  PICTURE  X(1).                    CI0080
            10            CX13-CY29                                     CI0080
                          REDEFINES            CX13-CY98.               CI0080
            11            CX13-IINDI1 PICTURE  X(1).                    CI0080
            11            CX13-IINDI2 PICTURE  X(1).                    CI0080
            11            CX13-IINDI3 PICTURE  X(1).                    CI0080
            11            CX13-PWHLD5 PICTURE  S999V99                  CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX13-CCSMQ  PICTURE  X.                       CI0080
            11            CX13-CPLEC  PICTURE  XX.                      CI0080
            11            CX13-IPTRDA PICTURE  X(01).                   CI0080
            11            CX13-GCUSPY PICTURE  X(12).                   CI0080
            11            CX13-ALOIDA PICTURE  S9(11)V99                CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            CX13-DELOI  PICTURE  9(8).                    CI0080
            11            CX13-CLGND  PICTURE  X.                       CI0080
            11            CX13-CORTYA PICTURE  X(3).                    CI0080
            11            CX13-CPH3U  PICTURE  X.                       CI0080
            11            CX13-CNAVR  PICTURE  X(1).                    CI0080
            11            CX13-NEXEC  PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
       01                 CX14.                                         CI0080
            10            CX14-GELL   PICTURE  9(4)                     CI0080
                          BINARY.                                       CI0080
            10            CX14-CX14K.                                   CI0080
            11            CX14-NPISQ  PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            CX14-ACOTD  PICTURE  S9(9)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            CX14-PPOTD  PICTURE  S9(3)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            CX14-QPSTD  PICTURE  S9(7)V999                CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            CX14-CPITC  PICTURE  99.                      CI0080
            10            CX14-FILLER PICTURE  X(04).                   CI0080
            10            CX14-CY97.                                    CI0080
            11            CX14-FILLER PICTURE  X(32).                   CI0080
            10            CX14-CY30                                     CI0080
                          REDEFINES            CX14-CY97.               CI0080
            11            CX14-IOWNC  PICTURE  X.                       CI0080
            11            CX14-CTYPE  PICTURE  X.                       CI0080
            11            CX14-C299.                                    CI0080
            12            CX14-CTID.                                    CI0080
            13            CX14-CTIDA  PICTURE  9(3).                    CI0080
            13            CX14-CTIDN.                                   CI0080
            14            CX14-CTIDNP PICTURE  X(13).                   CI0080
            14            CX14-CTIDND PICTURE  9(11).                   CI0080
            11            CX14-CPMTC  PICTURE  99.                      CI0080
            11            CX14-IACSD  PICTURE  X.                       CI0080
            10            CX14-CY31                                     CI0080
                          REDEFINES            CX14-CY97.               CI0080
            11            CX14-FILLER PICTURE  X(2).                    CI0080
            11            CX14-IDELI  PICTURE  X.                       CI0080
            11            CX14-CDEL1  PICTURE  9(3).                    CI0080
            11            CX14-NDELS  PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            CX14-CY32                                     CI0080
                          REDEFINES            CX14-CY97.               CI0080
            11            CX14-GCUSPZ PICTURE  X(12).                   CI0080
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0080
            10            XW05-XW06.                                    CI0080
            11            XW05-XDBPCB.                                  CI0080
            12            XW05-XDBDNM PICTURE  X(08)                    CI0080
                          VALUE                SPACE.                   CI0080
            12            XW05-XSEGLV PICTURE  X(02)                    CI0080
                          VALUE                SPACE.                   CI0080
            12            XW05-XRC    PICTURE  X(02)                    CI0080
                          VALUE                SPACE.                   CI0080
            12            XW05-XPROPT PICTURE  X(04)                    CI0080
                          VALUE                SPACE.                   CI0080
            12            XW05-FILLER PICTURE  S9(5)                    CI0080
                          VALUE                ZERO                     CI0080
                          BINARY.                                       CI0080
            12            XW05-XSEGNM PICTURE  X(08)                    CI0080
                          VALUE                SPACE.                   CI0080
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0080
                          VALUE                ZERO                     CI0080
                          BINARY.                                       CI0080
            12            XW05-XSEGNB PICTURE  9(05)                    CI0080
                          VALUE                ZERO                     CI0080
                          BINARY.                                       CI0080
            12            XW05-XCOKEY PICTURE  X(70)                    CI0080
                          VALUE                SPACE.                   CI0080
            10            XW05-XW07.                                    CI0080
            11            XW05-XIOPCB.                                  CI0080
            12            XW05-XTERMI PICTURE  X(08)                    CI0080
                          VALUE                SPACE.                   CI0080
            12            XW05-FILLER PICTURE  XX                       CI0080
                          VALUE                SPACE.                   CI0080
            12            XW05-XRC1   PICTURE  X(02)                    CI0080
                          VALUE                SPACE.                   CI0080
            12            XW05-FILLER PICTURE  X(12)                    CI0080
                          VALUE                SPACE.                   CI0080
            12            XW05-XMODNM PICTURE  X(8)                     CI0080
                          VALUE                SPACE.                   CI0080
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0080
                          VALUE                ZERO.                    CI0080
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0080
                          VALUE                ZERO.                    CI0080
            10            XW05-XGU    PICTURE  X(4)                     CI0080
                          VALUE                'GU  '.                  CI0080
            10            XW05-XGHU   PICTURE  X(4)                     CI0080
                          VALUE                'GHU '.                  CI0080
            10            XW05-XGN    PICTURE  X(4)                     CI0080
                          VALUE                'GN  '.                  CI0080
            10            XW05-XGHN   PICTURE  X(4)                     CI0080
                          VALUE                'GHN '.                  CI0080
            10            XW05-XGNP   PICTURE  X(4)                     CI0080
                          VALUE                'GNP '.                  CI0080
            10            XW05-XGHNP  PICTURE  X(4)                     CI0080
                          VALUE                'GHNP'.                  CI0080
            10            XW05-XREPL  PICTURE  XXXX                     CI0080
                          VALUE                'REPL'.                  CI0080
            10            XW05-XISRT  PICTURE  X(4)                     CI0080
                          VALUE                'ISRT'.                  CI0080
            10            XW05-XDLET  PICTURE  X(4)                     CI0080
                          VALUE                'DLET'.                  CI0080
            10            XW05-XOPEN  PICTURE  X(4)                     CI0080
                          VALUE                'OPEN'.                  CI0080
            10            XW05-XCLSE  PICTURE  X(4)                     CI0080
                          VALUE                'CLSE'.                  CI0080
            10            XW05-XCHKP  PICTURE  X(4)                     CI0080
                          VALUE                'CHKP'.                  CI0080
            10            XW05-XXRST  PICTURE  X(4)                     CI0080
                          VALUE                'XRST'.                  CI0080
            10            XW05-XTERM  PICTURE  X(4)                     CI0080
                          VALUE                'TERM'.                  CI0080
            10            XW05-XNFPAC PICTURE  X(13)                    CI0080
                          VALUE                SPACE.                   CI0080
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0080
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0080
       01                 GQ00.                                         CI0080
            02            GQ01.                                         CI0080
            10            GQ01-GELL   PICTURE  9(4)                     CI0080
                          BINARY.                                       CI0080
            10            GQ01-GMISC.                                   CI0080
            11            GQ01-GS00.                                    CI0080
            12            GQ01-GT01.                                    CI0080
            13            GQ01-GQ01K.                                   CI0080
            14            GQ01-CANUMB PICTURE  X(27).                   CI0080
            14            GQ01-CAMCTR PICTURE  9(5).                    CI0080
            14            GQ01-GESQ2  PICTURE  99.                      CI0080
            12            GQ01-GT02                                     CI0080
                          REDEFINES            GQ01-GT01.               CI0080
            13            GQ01-C199.                                    CI0080
            14            GQ01-CLID.                                    CI0080
            15            GQ01-CLIDO  PICTURE  9(3).                    CI0080
            15            GQ01-CLIDN.                                   CI0080
            16            GQ01-CLIDNP PICTURE  X(12).                   CI0080
            16            GQ01-CLIDND PICTURE  9(8).                    CI0080
            12            GQ01-GT03                                     CI0080
                          REDEFINES            GQ01-GT01.               CI0080
            13            GQ01-C299.                                    CI0080
            14            GQ01-CTID.                                    CI0080
            15            GQ01-CTIDA  PICTURE  9(3).                    CI0080
            15            GQ01-CTIDN.                                   CI0080
            16            GQ01-CTIDNP PICTURE  X(13).                   CI0080
            16            GQ01-CTIDND PICTURE  9(11).                   CI0080
            12            GQ01-GT04                                     CI0080
                          REDEFINES            GQ01-GT01.               CI0080
            13            GQ01-NPBN   PICTURE  X(20).                   CI0080
            12            GQ01-GT05                                     CI0080
                          REDEFINES            GQ01-GT01.               CI0080
            13            GQ01-GR98.                                    CI0080
            14            GQ01-GRID.                                    CI0080
            15            GQ01-GRIDC  PICTURE  9(3).                    CI0080
            15            GQ01-GRIDN.                                   CI0080
            16            GQ01-GRIDNP PICTURE  99.                      CI0080
            16            GQ01-GRIDND PICTURE  9(8).                    CI0080
            12            GQ01-GT06                                     CI0080
                          REDEFINES            GQ01-GT01.               CI0080
            13            GQ01-NTR    PICTURE  9(8).                    CI0080
            12            GQ01-GT07                                     CI0080
                          REDEFINES            GQ01-GT01.               CI0080
            13            GQ01-NTRAC  PICTURE  9(14).                   CI0080
            12            GQ01-GT08                                     CI0080
                          REDEFINES            GQ01-GT01.               CI0080
            13            GQ01-NSRAB  PICTURE  9(7).                    CI0080
            13            GQ01-GECKD  PICTURE  9.                       CI0080
            13            GQ01-NBLCK  PICTURE  9(5).                    CI0080
            13            GQ01-CTRID  PICTURE  X(4).                    CI0080
            12            GQ01-GT19                                     CI0080
                          REDEFINES            GQ01-GT01.               CI0080
            13            GQ01-GEOPD2 PICTURE  X(8).                    CI0080
            12            GQ01-CACKD  PICTURE  9.                       CI0080
            12            GQ01-CENTT  PICTURE  X.                       CI0080
            12            GQ01-CADATE PICTURE  X(8).                    CI0080
            12            GQ01-GETIM  PICTURE  S9(7)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            12            GQ01-GEOPID PICTURE  X(6).                    CI0080
            12            GQ01-CAUNIT PICTURE  X(4).                    CI0080
            12            GQ01-XTERMI PICTURE  X(08).                   CI0080
            12            GQ01-CAPPL  PICTURE  X(8).                    CI0080
            12            GQ01-CSYS   PICTURE  X(4).                    CI0080
            12            GQ01-NTRSU  PICTURE  999.                     CI0080
            12            GQ01-FILLER PICTURE  X(20).                   CI0080
            11            GQ01-XMISL  PICTURE  X(599).                  CI0080
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
      ******************************************************************ADUTAB
      **              TABLE TA75 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA75-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=75 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA75.                                                CI0080
           04    G-TA75-PARAM.                                          CI0080
             10  G-TA75-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0080
                        VALUE      +060.                                CI0080
             10  G-TA75-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0080
                        VALUE      +001.                                CI0080
             10  G-TA75-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0080
                        VALUE      +005.                                CI0080
             10  G-TA75-NUAPP  PICTURE 99                               CI0080
                        VALUE       0.                                  CI0080
             10  G-TA75-NUTAB  PICTURE X(6)                             CI0080
                        VALUE 'CAMCTR'.                                 CI0080
             10  G-TA75-TABFO  PICTURE XX                 VALUE SPACE.  CI0080
             10  G-TA75-TABCR  PICTURE XX                 VALUE SPACE.  CI0080
             10  G-TA75-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0080
             10  G-TA75-NUSSC  PICTURE X  VALUE   ' '.                  CI0080
             10  G-TA75-NUSSY  PICTURE X                  VALUE SPACE.  CI0080
             10  G-TA75-TRANID PICTURE X(4)               VALUE SPACE.  CI0080
             10  G-TA75-FILSYS.                                         CI0080
             15  G-TA75-USERC  PICTURE X(6)               VALUE SPACE.  CI0080
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0080
           04             TA75.                                         CI0080
            10            TA75-CAMCTR PICTURE  9(5)                     CI0080
                          VALUE                ZERO.                    CI0080
            10            TA75-TTDES  PICTURE  X(36)                    CI0080
                          VALUE                SPACE.                   CI0080
            10            TA75-MSYSID PICTURE  X(8)                     CI0080
                          VALUE                SPACE.                   CI0080
            10            TA75-NDLEN  PICTURE  S9(4)                    CI0080
                          VALUE                ZERO.                    CI0080
            10            TA75-IMIND1 PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            TA75-IMIND2 PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            TA75-IMIND3 PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            TA75-IMIND5 PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            TA75-IMIND7 PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            TA75-IMIND8 PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            TA75-IMINE  PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
      **                                                                ADUTAB

      *****************************************************************
      ** THE FOLLOWING SEGMENTS GET MOVED TO THE VARIABLE PORTION OF  *
      ** THE MISC TRAN DB SEGMENT: GQ01-XMISL                         *
      *****************************************************************

      *GS51 - PAYOUT CONFIRMATION TRIGGER
      *!WF DSP=GS DSL=GS SEL=51 FOR=I DES=2 LEV=1 PLT=VA
       01                 GS00.                                         CI0080
            10            GS00-GT01.                                    CI0080
            11            GS00-GQ01K.                                   CI0080
            12            GS00-CANUMB PICTURE  X(27)                    CI0080
                          VALUE                SPACE.                   CI0080
            12            GS00-CAMCTR PICTURE  9(5)                     CI0080
                          VALUE                ZERO.                    CI0080
            12            GS00-GESQ2  PICTURE  99                       CI0080
                          VALUE                ZERO.                    CI0080
            10            GS00-GT02                                     CI0080
                          REDEFINES            GS00-GT01.               CI0080
            11            GS00-C199.                                    CI0080
            12            GS00-CLID.                                    CI0080
            13            GS00-CLIDO  PICTURE  9(3).                    CI0080
            13            GS00-CLIDN.                                   CI0080
            14            GS00-CLIDNP PICTURE  X(12).                   CI0080
            14            GS00-CLIDND PICTURE  9(8).                    CI0080
            10            GS00-GT03                                     CI0080
                          REDEFINES            GS00-GT01.               CI0080
            11            GS00-C299.                                    CI0080
            12            GS00-CTID.                                    CI0080
            13            GS00-CTIDA  PICTURE  9(3).                    CI0080
            13            GS00-CTIDN.                                   CI0080
            14            GS00-CTIDNP PICTURE  X(13).                   CI0080
            14            GS00-CTIDND PICTURE  9(11).                   CI0080
            10            GS00-GT04                                     CI0080
                          REDEFINES            GS00-GT01.               CI0080
            11            GS00-NPBN   PICTURE  X(20).                   CI0080
            10            GS00-GT05                                     CI0080
                          REDEFINES            GS00-GT01.               CI0080
            11            GS00-GR98.                                    CI0080
            12            GS00-GRID.                                    CI0080
            13            GS00-GRIDC  PICTURE  9(3).                    CI0080
            13            GS00-GRIDN.                                   CI0080
            14            GS00-GRIDNP PICTURE  99.                      CI0080
            14            GS00-GRIDND PICTURE  9(8).                    CI0080
            10            GS00-GT06                                     CI0080
                          REDEFINES            GS00-GT01.               CI0080
            11            GS00-NTR    PICTURE  9(8).                    CI0080
            10            GS00-GT07                                     CI0080
                          REDEFINES            GS00-GT01.               CI0080
            11            GS00-NTRAC  PICTURE  9(14).                   CI0080
            10            GS00-GT08                                     CI0080
                          REDEFINES            GS00-GT01.               CI0080
            11            GS00-NSRAB  PICTURE  9(7).                    CI0080
            11            GS00-GECKD  PICTURE  9.                       CI0080
            11            GS00-NBLCK  PICTURE  9(5).                    CI0080
            11            GS00-CTRID  PICTURE  X(4).                    CI0080
            10            GS00-GT19                                     CI0080
                          REDEFINES            GS00-GT01.               CI0080
            11            GS00-GEOPD2 PICTURE  X(8).                    CI0080
            10            GS00-CACKD  PICTURE  9                        CI0080
                          VALUE                ZERO.                    CI0080
            10            GS00-CENTT  PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            GS00-CADATE PICTURE  X(8)                     CI0080
                          VALUE                SPACE.                   CI0080
            10            GS00-GETIM  PICTURE  S9(7)                    CI0080
                          VALUE                ZERO                     CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            GS00-GEOPID PICTURE  X(6)                     CI0080
                          VALUE                SPACE.                   CI0080
            10            GS00-CAUNIT PICTURE  X(4)                     CI0080
                          VALUE                SPACE.                   CI0080
            10            GS00-XTERMI PICTURE  X(08)                    CI0080
                          VALUE                SPACE.                   CI0080
            10            GS00-CAPPL  PICTURE  X(8)                     CI0080
                          VALUE                SPACE.                   CI0080
            10            GS00-CSYS   PICTURE  X(4)                     CI0080
                          VALUE                SPACE.                   CI0080
            10            GS00-NTRSU  PICTURE  999                      CI0080
                          VALUE                ZERO.                    CI0080
            10            GS00-FILLER PICTURE  X(20)                    CI0080
                          VALUE                SPACE.                   CI0080
       01                 GS43.                                         CI0080
            10            GS43-COCLNS PICTURE  X(30)                    CI0080
                          OCCURS       003     TIMES                    CI0080
                          VALUE                SPACE.                   CI0080
            10            GS43-GESAD  PICTURE  X(30)                    CI0080
                          OCCURS       004     TIMES                    CI0080
                          VALUE                SPACE.                   CI0080
            10            GS43-CTTBO  PICTURE  X(45)                    CI0080
                          OCCURS       002     TIMES                    CI0080
                          VALUE                SPACE.                   CI0080
            10            GS43-CLTIN  PICTURE  9(12)                    CI0080
                          VALUE                ZERO.                    CI0080
            10            GS43-CTINA  PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            GS43-GETINH PICTURE  99                       CI0080
                          VALUE                ZERO.                    CI0080
            10            GS43-CLDOB  PICTURE  9(8)                     CI0080
                          VALUE                ZERO.                    CI0080
            10            GS43-CTNLI  PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            GS43-CTCUS  PICTURE  999                      CI0080
                          VALUE                ZERO.                    CI0080
            10            GS43-GECTY  PICTURE  9(3)                     CI0080
                          VALUE                ZERO.                    CI0080
            10            GS43-CLRAY  PICTURE  9(5)                     CI0080
                          VALUE                ZERO.                    CI0080
            10            GS43-CTOWN  PICTURE  9(3)                     CI0080
                          VALUE                ZERO.                    CI0080
            10            GS43-GESTN  PICTURE  9(2)                     CI0080
                          VALUE                ZERO.                    CI0080
            10            GS43-CTSTS  PICTURE  9(2)                     CI0080
                          VALUE                ZERO.                    CI0080
            10            GS43-PRCOD  PICTURE  9(5)                     CI0080
                          VALUE                ZERO.                    CI0080
            10            GS43-PRSCD  PICTURE  X(9)                     CI0080
                          VALUE                SPACE.                   CI0080
            10            GS43-CTEFD  PICTURE  9(8)                     CI0080
                          VALUE                ZERO.                    CI0080
            10            GS43-CEIPN  PICTURE  9(1)                     CI0080
                          VALUE                ZERO.                    CI0080
            10            GS43-CECLO  PICTURE  9(1)                     CI0080
                          VALUE                ZERO.                    CI0080
            10            GS43-CEIT   PICTURE  9(3)                     CI0080
                          VALUE                ZERO.                    CI0080
            10            GS43-CEFAC  PICTURE  S9(7)V99                 CI0080
                          VALUE                ZERO                     CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            GS43-CDSTR  PICTURE  XX                       CI0080
                          VALUE                SPACE.                   CI0080
            10            GS43-ICUSC  PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            GS43-IFORG  PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            GS43-GRID   PICTURE  X(13)                    CI0080
                          VALUE                SPACE.                   CI0080
            10            GS43-AEFACA PICTURE  9(7)                     CI0080
                          VALUE                ZERO                     CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            GS43-IDPML  PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            GS43-XZ6    PICTURE  X(6)                     CI0080
                          VALUE                SPACE.                   CI0080
       01                 GS45.                                         CI0080
            10            GS45-CDSTR  PICTURE  XX                       CI0080
                          VALUE                SPACE.                   CI0080
            10            GS45-CPITC  PICTURE  99                       CI0080
                          VALUE                ZERO.                    CI0080
            10            GS45-INACT  PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            GS45-GT12.                                    CI0080
            11            GS45-FILLER PICTURE  X(213)                   CI0080
                          VALUE                SPACE.                   CI0080
            10            GS45-GT13                                     CI0080
                          REDEFINES            GS45-GT12.               CI0080
            11            GS45-CTID   PICTURE  X(27).                   CI0080
            11            GS45-CPMTC  PICTURE  99.                      CI0080
            10            GS45-GT14                                     CI0080
                          REDEFINES            GS45-GT12.               CI0080
            11            GS45-CPAY1  PICTURE  X(2).                    CI0080
            11            GS45-GT15.                                    CI0080
            12            GS45-FILLER PICTURE  X(211).                  CI0080
            11            GS45-GT16                                     CI0080
                          REDEFINES            GS45-GT15.               CI0080
            12            GS45-COCLNS PICTURE  X(30).                   CI0080
            12            GS45-TDELI  PICTURE  X(30).                   CI0080
            12            GS45-COSAD  PICTURE  X(30).                   CI0080
            12            GS45-COSAD2 PICTURE  X(30).                   CI0080
            12            GS45-COSAD3 PICTURE  X(30).                   CI0080
            12            GS45-COSAD4 PICTURE  X(30).                   CI0080
            11            GS45-GT17                                     CI0080
                          REDEFINES            GS45-GT15.               CI0080
            12            GS45-NTR    PICTURE  9(8).                    CI0080
            12            GS45-GECKD  PICTURE  9.                       CI0080
            12            GS45-NPBN   PICTURE  X(20).                   CI0080
            12            GS45-CCBAT  PICTURE  99.                      CI0080
            12            GS45-MCSIG  PICTURE  X(30).                   CI0080
            12            GS45-GENAL  PICTURE  X(30).                   CI0080
            12            GS45-CESNA1 PICTURE  X(30).                   CI0080
            12            GS45-CESNA2 PICTURE  X(30).                   CI0080
            12            GS45-CESNA3 PICTURE  X(30).                   CI0080
            12            GS45-CESNA4 PICTURE  X(30).                   CI0080
            11            GS45-GT18                                     CI0080
                          REDEFINES            GS45-GT15.               CI0080
            12            GS45-CTTLN1 PICTURE  X(30).                   CI0080
            12            GS45-CTTLN2 PICTURE  X(30).                   CI0080
            12            GS45-CTTLN3 PICTURE  X(30).                   CI0080
            12            GS45-GESAD1 PICTURE  X(30).                   CI0080
            12            GS45-GESAD2 PICTURE  X(30).                   CI0080
            12            GS45-GESAD3 PICTURE  X(30).                   CI0080
            12            GS45-GESAD4 PICTURE  X(30).                   CI0080
       01                 GS51.                                         CI0080
            10            GS51-CRETP  PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            GS51-ICSWD  PICTURE  X(1)                     CI0080
                          VALUE                SPACE.                   CI0080
            10            GS51-FILLER PICTURE  X(09)                    CI0080
                          VALUE                SPACE.                   CI0080
       01                 GS52.                                         CI0080
            10            GS52-CLID   PICTURE  X(23)                    CI0080
                          VALUE                SPACE.                   CI0080
            10            GS52-CDSTR  PICTURE  XX                       CI0080
                          VALUE                SPACE.                   CI0080
            10            GS52-CRETP  PICTURE  X                        CI0080
                          VALUE                SPACE.                   CI0080
            10            GS52-FILLER PICTURE  X(09)                    CI0080
                          VALUE                SPACE.                   CI0080
       01                 GS55.                                         CI0080
            10            GS55-CPMTF  PICTURE  99                       CI0080
                          VALUE                ZERO.                    CI0080
            10            GS55-FILLER PICTURE  X(8)                     CI0080
                          VALUE                SPACE.                   CI0080
       01                 GS56.                                         CI0080
            10            GS56-CPITC  PICTURE  99                       CI0080
                          VALUE                ZERO.                    CI0080
            10            GS56-CDEST  PICTURE  99                       CI0080
                          VALUE                ZERO.                    CI0080
            10            GS56-CDELI  PICTURE  9(3)                     CI0080
                          VALUE                ZERO.                    CI0080
            10            GS56-FILLER PICTURE  X(3)                     CI0080
                          VALUE                SPACE.                   CI0080

      *GS52 - DIVIDEND CONFIRMATION TRIGGER
      *!WF DSP=GS DSL=GS SEL=52 FOR=I DES=2 LEV=1 PLT=VA

      *GS55 - CERT INT PMT SCHEDULING
      *!WF DSP=GS DSL=GS SEL=55 FOR=I DES=2 LEV=1 PLT=VA

      *GS56 - CERT INT PMT DELIVERY
      *!WF DSP=GS DSL=GS SEL=56 FOR=I DES=2 LEV=1 PLT=VA

      *GS43 - NEW ACCOUNT
      *!WF DSP=GS DSL=GS SEL=43 FOR=I DES=2 LEV=1 PLT=VA

      *GS45 - DIVIDEND DISTRIBUTIONS
      *!WF DSP=GS DSL=GS SEL=45 FOR=I DES=2 LEV=1 PLT=VA


      *DATABASE ACCESS INDICATORS
      *'0' = SUCCESSFUL
      *'1' = UNSUCCESSFUL
       01  INDICATORS.
           05  CX13-IK           PIC X.
           05  CX14-IK           PIC X.
           05  CT01-IK           PIC X.
           05  GQ01-IK           PIC X.

      *USED FOR FUND DIVIDENDS. SET TO 'Y' WHEN DISTRIBUTION CODE
      *(A.K.A. 'REINVEST CODE') PASSED TO THIS MODULE (DS80-CDSTR)
      *IS NOT = TO THAT ON CT01. CT01-CDSTR WILL GET UPDATED.
       01  W-CT01-CDSTR          PIC X.

       01   DEBUT-WSS.                                                  CI0080
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0080
            05   IK     PICTURE X.                                      CI0080
       01  CONSTANTES-PAC.                                              CI0080
           05  FILLER  PICTURE X(87)   VALUE                            CI0080
                     '6015 CAT09/08/14CI0080ADMIN   14:34:40CI0080P AMERCI0080
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0080
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0080
           05  NUGNA   PICTURE X(5).                                    CI0080
           05  APPLI   PICTURE X(3).                                    CI0080
           05  DATGN   PICTURE X(8).                                    CI0080
           05  PROGR   PICTURE X(6).                                    CI0080
           05  CODUTI  PICTURE X(8).                                    CI0080
           05  TIMGN   PICTURE X(8).                                    CI0080
           05  PROGE   PICTURE X(8).                                    CI0080
           05  COBASE  PICTURE X(4).                                    CI0080
           05  DATGNC  PICTURE X(10).                                   CI0080
           05  RELEAS  PICTURE X(7).                                    CI0080
           05  DATGE   PICTURE X(10).                                   CI0080
           05  DATSQ   PICTURE X(10).                                   CI0080
       01  DATCE.                                                       CI0080
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0080
         05  DATOR.                                                     CI0080
           10  DATOA  PICTURE XX.                                       CI0080
           10  DATOM  PICTURE XX.                                       CI0080
           10  DATOJ  PICTURE XX.                                       CI0080
       01   VARIABLES-CONDITIONNELLES.                                  CI0080
            05                  FT      PICTURE X VALUE '0'.            CI0080
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0080
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0080
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0080
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0080
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0080
            05       5-GQ00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0080
       01               S-CT01-SSA.                                     CI0080
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0080
                                      VALUE 'CT01    '.                 CI0080
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0080
            10          S-CT01-CCOD   PICTURE X(5)                      CI0080
                                      VALUE '-----'.                    CI0080
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0080
       01            S-CTU01-SSA.                                       CI0080
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CT01    '.                 CI0080
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0080
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(CT01K'.                   CI0080
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0080
            10       S-CTU01-CT01K.                                     CI0080
            11       S-CTU01-C299.                                      CI0080
            12       S-CTU01-CTID.                                      CI0080
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0080
            13       S-CTU01-CTIDN.                                     CI0080
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0080
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0080
            10  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01               S-CX01-SSA.                                     CI0080
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0080
                                      VALUE 'CX01    '.                 CI0080
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0080
            10          S-CX01-CCOD   PICTURE X(5)                      CI0080
                                      VALUE '-----'.                    CI0080
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0080
       01            S-CXU01-SSA.                                       CI0080
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX01    '.                 CI0080
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0080
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(CX01K'.                   CI0080
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0080
            10       S-CXU01-CX01K.                                     CI0080
            11       S-CXU01-C199.                                      CI0080
            12       S-CXU01-CLID.                                      CI0080
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0080
            13       S-CXU01-CLIDN.                                     CI0080
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0080
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0080
            10  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01               S-CX03-SSA.                                     CI0080
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0080
                                      VALUE 'CX03    '.                 CI0080
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0080
            10          S-CX03-CCOD   PICTURE X(5)                      CI0080
                                      VALUE '-----'.                    CI0080
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0080
       01            S-CXA03-SSA.                                       CI0080
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX03    '.                 CI0080
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0080
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(CARTY'.                   CI0080
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0080
            12       S-CXA03-CARTY    PICTURE  99.                      CI0080
            12  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01            S-CXB03-SSA.                                       CI0080
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX03    '.                 CI0080
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0080
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(NARRS'.                   CI0080
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0080
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            12  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01            S-CXC03-SSA.                                       CI0080
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX03    '.                 CI0080
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0080
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(CPMTG'.                   CI0080
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0080
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0080
            11  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01            S-CXD03-SSA.                                       CI0080
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX03    '.                 CI0080
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0080
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(GRCRNG'.                  CI0080
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0080
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0080
            11  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01            S-CXE03-SSA.                                       CI0080
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX03    '.                 CI0080
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0080
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(DEXDT'.                   CI0080
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0080
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0080
            11  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01            S-CXF03-SSA.                                       CI0080
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX03    '.                 CI0080
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0080
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(CY50'.                    CI0080
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0080
            11       S-CXF03-CY50.                                      CI0080
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0080
            11  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01            S-CXG03-SSA.                                       CI0080
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX03    '.                 CI0080
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0080
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(NBASQ'.                   CI0080
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0080
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            11  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01            S-CXH03-SSA.                                       CI0080
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX03    '.                 CI0080
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0080
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(NARID'.                   CI0080
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0080
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0080
            12  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01            S-CXU03-SSA.                                       CI0080
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX03    '.                 CI0080
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0080
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(CX03K'.                   CI0080
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0080
            11       S-CXU03-CX03K.                                     CI0080
            12       S-CXU03-CARTY    PICTURE  99.                      CI0080
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            11  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01               S-CX06-SSA.                                     CI0080
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0080
                                      VALUE 'CX06    '.                 CI0080
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0080
            10          S-CX06-CCOD   PICTURE X(5)                      CI0080
                                      VALUE '-----'.                    CI0080
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0080
       01            S-CXU06-SSA.                                       CI0080
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX06    '.                 CI0080
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0080
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(CX06K'.                   CI0080
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0080
            10       S-CXU06-CX06K.                                     CI0080
            11       S-CXU06-C299.                                      CI0080
            12       S-CXU06-CTID.                                      CI0080
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0080
            13       S-CXU06-CTIDN.                                     CI0080
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0080
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0080
            10  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01               S-CX13-SSA.                                     CI0080
            10         S1-CX13-SEGNAM PICTURE X(8)                      CI0080
                                      VALUE 'CX13    '.                 CI0080
            10         S1-CX13-CCOM   PICTURE X VALUE '*'.              CI0080
            10          S-CX13-CCOD   PICTURE X(5)                      CI0080
                                      VALUE '-----'.                    CI0080
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0080
       01            S-CXA13-SSA.                                       CI0080
            11      S1-CXA13-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX13    '.                 CI0080
            11      S1-CXA13-CCOM   PICTURE X VALUE '*'.                CI0080
            11       S-CXA13-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            11      S1-CXA13-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(CDEST'.                   CI0080
            11       S-CXA13-OPER  PICTURE XX VALUE ' ='.               CI0080
            11       S-CXA13-CDEST    PICTURE  99.                      CI0080
            11  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01            S-CXB13-SSA.                                       CI0080
            12      S1-CXB13-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX13    '.                 CI0080
            12      S1-CXB13-CCOM   PICTURE X VALUE '*'.                CI0080
            12       S-CXB13-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            12      S1-CXB13-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(CARTZ'.                   CI0080
            12       S-CXB13-OPER  PICTURE XX VALUE ' ='.               CI0080
            12       S-CXB13-CARTZ    PICTURE  99.                      CI0080
            12  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01            S-CXC13-SSA.                                       CI0080
            12      S1-CXC13-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX13    '.                 CI0080
            12      S1-CXC13-CCOM   PICTURE X VALUE '*'.                CI0080
            12       S-CXC13-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            12      S1-CXC13-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(NAPDS'.                   CI0080
            12       S-CXC13-OPER  PICTURE XX VALUE ' ='.               CI0080
            12       S-CXC13-NAPDS    PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            12  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01            S-CXU13-SSA.                                       CI0080
            11      S1-CXU13-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX13    '.                 CI0080
            11      S1-CXU13-CCOM   PICTURE X VALUE '*'.                CI0080
            11       S-CXU13-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            11      S1-CXU13-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(CX13K'.                   CI0080
            11       S-CXU13-OPER  PICTURE XX VALUE ' ='.               CI0080
            11       S-CXU13-CX13K.                                     CI0080
            12       S-CXU13-CARTZ    PICTURE  99.                      CI0080
            12       S-CXU13-NAPDS    PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            11  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01            S-CX113-SSA.                                       CI0080
            11      S1-CX113-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX13    '.                 CI0080
            11      S1-CX113-CCOM   PICTURE X VALUE '*'.                CI0080
            11       S-CX113-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            11      S1-CX113-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(XGCUSPY'.                 CI0080
            11       S-CX113-OPER  PICTURE XX VALUE ' ='.               CI0080
            11       S-CX113-GCUSPY   PICTURE  X(12).                   CI0080
            11  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01               S-CX14-SSA.                                     CI0080
            10         S1-CX14-SEGNAM PICTURE X(8)                      CI0080
                                      VALUE 'CX14    '.                 CI0080
            10         S1-CX14-CCOM   PICTURE X VALUE '*'.              CI0080
            10          S-CX14-CCOD   PICTURE X(5)                      CI0080
                                      VALUE '-----'.                    CI0080
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0080
       01            S-CXU14-SSA.                                       CI0080
            10      S1-CXU14-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX14    '.                 CI0080
            10      S1-CXU14-CCOM   PICTURE X VALUE '*'.                CI0080
            10       S-CXU14-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            10      S1-CXU14-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(CX14K'.                   CI0080
            10       S-CXU14-OPER  PICTURE XX VALUE ' ='.               CI0080
            10       S-CXU14-CX14K.                                     CI0080
            11       S-CXU14-NPISQ    PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            10  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01            S-CX114-SSA.                                       CI0080
            11      S1-CX114-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'CX14    '.                 CI0080
            11      S1-CX114-CCOM   PICTURE X VALUE '*'.                CI0080
            11       S-CX114-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            11      S1-CX114-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(XGCUSPZ'.                 CI0080
            11       S-CX114-OPER  PICTURE XX VALUE ' ='.               CI0080
            11       S-CX114-GCUSPZ   PICTURE  X(12).                   CI0080
            11  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01               S-GQ01-SSA.                                     CI0080
            10         S1-GQ01-SEGNAM PICTURE X(8)                      CI0080
                                      VALUE 'GQ01    '.                 CI0080
            10         S1-GQ01-CCOM   PICTURE X VALUE '*'.              CI0080
            10          S-GQ01-CCOD   PICTURE X(5)                      CI0080
                                      VALUE '-----'.                    CI0080
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0080
       01            S-GQU01-SSA.                                       CI0080
            13      S1-GQU01-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'GQ01    '.                 CI0080
            13      S1-GQU01-CCOM   PICTURE X VALUE '*'.                CI0080
            13       S-GQU01-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            13      S1-GQU01-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(GQ01K'.                   CI0080
            13       S-GQU01-OPER  PICTURE XX VALUE ' ='.               CI0080
            13       S-GQU01-GQ01K.                                     CI0080
            14       S-GQU01-CANUMB   PICTURE  X(27).                   CI0080
            14       S-GQU01-CAMCTR   PICTURE  9(5).                    CI0080
            14       S-GQU01-GESQ2    PICTURE  99.                      CI0080
            13  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01            S-GQ701-SSA.                                       CI0080
            14      S1-GQ701-SEGNAM PICTURE X(8)                        CI0080
                                      VALUE 'GQ01    '.                 CI0080
            14      S1-GQ701-CCOM   PICTURE X VALUE '*'.                CI0080
            14       S-GQ701-CCOD   PICTURE X(5)                        CI0080
                                      VALUE '-----'.                    CI0080
            14      S1-GQ701-FLDNAM PICTURE X(9)                        CI0080
                                      VALUE '(XCANUMB'.                 CI0080
            14       S-GQ701-OPER  PICTURE XX VALUE ' ='.               CI0080
            14       S-GQ701-CANUMB   PICTURE  X(27).                   CI0080
            14  FILLER   PICTURE X    VALUE ')'.                        CI0080
       01   ZONES-UTILISATEUR PICTURE X.                                CI0080
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
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR TR1P                                           ADU015
            05 PCB-TR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=XA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XA00.                                         CI0080
          05              XA00-SUITE.                                   CI0080
            15       FILLER         PICTURE  X(00106).                  CI0080
       01                 XA06  REDEFINES      XA00.                    CI0080
            10            XA06-XDBPCB.                                  CI0080
            11            XA06-XDBDNM PICTURE  X(08).                   CI0080
            11            XA06-XSEGLV PICTURE  X(02).                   CI0080
            11            XA06-XRC    PICTURE  X(02).                   CI0080
            11            XA06-XPROPT PICTURE  X(04).                   CI0080
            11            XA06-FILLER PICTURE  S9(5)                    CI0080
                          BINARY.                                       CI0080
            11            XA06-XSEGNM PICTURE  X(08).                   CI0080
            11            XA06-XKEYLN PICTURE  S9(05)                   CI0080
                          BINARY.                                       CI0080
            11            XA06-XSEGNB PICTURE  9(05)                    CI0080
                          BINARY.                                       CI0080
            11            XA06-XCOKEY PICTURE  X(70).                   CI0080
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=XB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XB00.                                         CI0080
          05              XB00-SUITE.                                   CI0080
            15       FILLER         PICTURE  X(00106).                  CI0080
       01                 XB06  REDEFINES      XB00.                    CI0080
            10            XB06-XDBPCB.                                  CI0080
            11            XB06-XDBDNM PICTURE  X(08).                   CI0080
            11            XB06-XSEGLV PICTURE  X(02).                   CI0080
            11            XB06-XRC    PICTURE  X(02).                   CI0080
            11            XB06-XPROPT PICTURE  X(04).                   CI0080
            11            XB06-FILLER PICTURE  S9(5)                    CI0080
                          BINARY.                                       CI0080
            11            XB06-XSEGNM PICTURE  X(08).                   CI0080
            11            XB06-XKEYLN PICTURE  S9(05)                   CI0080
                          BINARY.                                       CI0080
            11            XB06-XSEGNB PICTURE  9(05)                    CI0080
                          BINARY.                                       CI0080
            11            XB06-XCOKEY PICTURE  X(70).                   CI0080
      *** PCB MASK FOR TR1P                                             ADU015
      *!WF DSP=XC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XC00.                                         CI0080
          05              XC00-SUITE.                                   CI0080
            15       FILLER         PICTURE  X(00106).                  CI0080
       01                 XC06  REDEFINES      XC00.                    CI0080
            10            XC06-XDBPCB.                                  CI0080
            11            XC06-XDBDNM PICTURE  X(08).                   CI0080
            11            XC06-XSEGLV PICTURE  X(02).                   CI0080
            11            XC06-XRC    PICTURE  X(02).                   CI0080
            11            XC06-XPROPT PICTURE  X(04).                   CI0080
            11            XC06-FILLER PICTURE  S9(5)                    CI0080
                          BINARY.                                       CI0080
            11            XC06-XSEGNM PICTURE  X(08).                   CI0080
            11            XC06-XKEYLN PICTURE  S9(05)                   CI0080
                          BINARY.                                       CI0080
            11            XC06-XSEGNB PICTURE  9(05)                    CI0080
                          BINARY.                                       CI0080
            11            XC06-XCOKEY PICTURE  X(70).                   CI0080

      *'INPUT' TO THIS MODULE.
      *LX13 & LX14 CONTAIN THE OLD CX13 & CX14.
      *!WF DSP=DS DSL=DU SEL=80 FOR=I DES=1 LEV=1 PLT=75
       01                 DS80.                                         CI0080
            10            DS80-MAPPN  PICTURE  X(10).                   CI0080
            10            DS80-PROGR  PICTURE  X(06).                   CI0080
            10            DS80-CACTA  PICTURE  X(1).                    CI0080
            10            DS80-CLID   PICTURE  X(23).                   CI0080
            10            DS80-CX03K  PICTURE  X(4).                    CI0080
            10            DS80-C299.                                    CI0080
            11            DS80-CTID.                                    CI0080
            12            DS80-CTIDA  PICTURE  9(3).                    CI0080
            12            DS80-CTIDN.                                   CI0080
            13            DS80-CTIDNP PICTURE  X(13).                   CI0080
            13            DS80-CTIDND PICTURE  9(11).                   CI0080
            10            DS80-CARTZ  PICTURE  99.                      CI0080
            10            DS80-NAPDS  PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            DS80-CDSTR  PICTURE  XX.                      CI0080
            10            DS80-DCACG  PICTURE  9(8).                    CI0080
            10            DS80-GEOPD2 PICTURE  X(8).                    CI0080
            10            DS80-CAUNIT PICTURE  X(4).                    CI0080
            10            DS80-CPAYC  PICTURE  X(2).                    CI0080
            10            DS80-NPISQ  PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            DS80-CPMTF  PICTURE  99.                      CI0080
            10            DS80-CPMTG  PICTURE  99.                      CI0080
      *!WF DSP=LX DSL=CX SEL=1314 FOR=I DES=1 LEV=1
      * PLT=75
       01                 LX13.                                         CI0080
            10            LX13-GELL   PICTURE  9(4)                     CI0080
                          BINARY.                                       CI0080
            10            LX13-CY20.                                    CI0080
            11            LX13-CX13K.                                   CI0080
            12            LX13-CARTZ  PICTURE  99.                      CI0080
            12            LX13-NAPDS  PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            LX13-GESTD  PICTURE  9(8).                    CI0080
            11            LX13-GEEND  PICTURE  9(8).                    CI0080
            11            LX13-DASUQ  PICTURE  9(8).                    CI0080
            11            LX13-CDEST  PICTURE  99.                      CI0080
            11            LX13-IIARR  PICTURE  X.                       CI0080
            11            LX13-DLAUP  PICTURE  9(8).                    CI0080
            11            LX13-GEOPD2 PICTURE  X(8).                    CI0080
            11            LX13-GEAUN  PICTURE  9(5).                    CI0080
            11            LX13-DPCHD  PICTURE  9(8).                    CI0080
            11            LX13-PPOT1  PICTURE  S9(3)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            LX13-ACOT1  PICTURE  S9(9)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            LX13-QPST1  PICTURE  S9(7)V999                CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            LX13-FILLER PICTURE  X(03).                   CI0080
            10            LX13-CY96.                                    CI0080
            11            LX13-FILLER PICTURE  X(50).                   CI0080
            10            LX13-CY21                                     CI0080
                          REDEFINES            LX13-CY96.               CI0080
            11            LX13-DNPMT  PICTURE  9(8).                    CI0080
            11            LX13-CPMTF  PICTURE  99.                      CI0080
            11            LX13-ADBRQ  PICTURE  S9(11)V99                CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            LX13-QSHOWQ PICTURE  S9(9)V999                CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            LX13-PACT1  PICTURE  S999V999                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            LX13-DOPDA  PICTURE  99.                      CI0080
            11            LX13-DNEXE  PICTURE  9(8).                    CI0080
            11            LX13-CIRMO  PICTURE  X(12).                   CI0080
            10            LX13-CY98.                                    CI0080
            11            LX13-FILLER PICTURE  X(120).                  CI0080
            10            LX13-CY25                                     CI0080
                          REDEFINES            LX13-CY98.               CI0080
            11            LX13-COPTC  PICTURE  9(1).                    CI0080
            11            LX13-ILPOI  PICTURE  X(1).                    CI0080
            11            LX13-CATOC  PICTURE  X(1).                    CI0080
            11            LX13-CEOIA  PICTURE  S9(7)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            LX13-ACOAR  PICTURE  S9(9)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            LX13-CEOTR  PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            LX13-DSTMO  PICTURE  99.                      CI0080
            10            LX13-CY27                                     CI0080
                          REDEFINES            LX13-CY98.               CI0080
            11            LX13-QMTH1  PICTURE  9(3).                    CI0080
            11            LX13-IDRMD  PICTURE  X.                       CI0080
            10            LX13-CY28                                     CI0080
                          REDEFINES            LX13-CY98.               CI0080
            11            LX13-AALLBL PICTURE  S9(8)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            LX13-PSURR  PICTURE  S9(3)V999                CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            LX13-DFPMT  PICTURE  9(8).                    CI0080
            11            LX13-QMTHLA PICTURE  9(3).                    CI0080
            11            LX13-PWHLDS PICTURE  S999V9(5)                CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            LX13-ISWHO  PICTURE  X(1).                    CI0080
            10            LX13-CY29                                     CI0080
                          REDEFINES            LX13-CY98.               CI0080
            11            LX13-IINDI1 PICTURE  X(1).                    CI0080
            11            LX13-IINDI2 PICTURE  X(1).                    CI0080
            11            LX13-IINDI3 PICTURE  X(1).                    CI0080
            11            LX13-PWHLD5 PICTURE  S999V99                  CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            LX13-CCSMQ  PICTURE  X.                       CI0080
            11            LX13-CPLEC  PICTURE  XX.                      CI0080
            11            LX13-IPTRDA PICTURE  X(01).                   CI0080
            11            LX13-GCUSPY PICTURE  X(12).                   CI0080
            11            LX13-ALOIDA PICTURE  S9(11)V99                CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            LX13-DELOI  PICTURE  9(8).                    CI0080
            11            LX13-CLGND  PICTURE  X.                       CI0080
            11            LX13-CORTYA PICTURE  X(3).                    CI0080
            11            LX13-CPH3U  PICTURE  X.                       CI0080
            11            LX13-CNAVR  PICTURE  X(1).                    CI0080
            11            LX13-NEXEC  PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
       01                 LX14.                                         CI0080
            10            LX14-GELL   PICTURE  9(4)                     CI0080
                          BINARY.                                       CI0080
            10            LX14-CX14K.                                   CI0080
            11            LX14-NPISQ  PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            LX14-ACOTD  PICTURE  S9(9)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            LX14-PPOTD  PICTURE  S9(3)V99                 CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            LX14-QPSTD  PICTURE  S9(7)V999                CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            LX14-CPITC  PICTURE  99.                      CI0080
            10            LX14-FILLER PICTURE  X(04).                   CI0080
            10            LX14-CY97.                                    CI0080
            11            LX14-FILLER PICTURE  X(32).                   CI0080
            10            LX14-CY30                                     CI0080
                          REDEFINES            LX14-CY97.               CI0080
            11            LX14-IOWNC  PICTURE  X.                       CI0080
            11            LX14-CTYPE  PICTURE  X.                       CI0080
            11            LX14-C299.                                    CI0080
            12            LX14-CTID.                                    CI0080
            13            LX14-CTIDA  PICTURE  9(3).                    CI0080
            13            LX14-CTIDN.                                   CI0080
            14            LX14-CTIDNP PICTURE  X(13).                   CI0080
            14            LX14-CTIDND PICTURE  9(11).                   CI0080
            11            LX14-CPMTC  PICTURE  99.                      CI0080
            11            LX14-IACSD  PICTURE  X.                       CI0080
            10            LX14-CY31                                     CI0080
                          REDEFINES            LX14-CY97.               CI0080
            11            LX14-FILLER PICTURE  X(2).                    CI0080
            11            LX14-IDELI  PICTURE  X.                       CI0080
            11            LX14-CDEL1  PICTURE  9(3).                    CI0080
            11            LX14-NDELS  PICTURE  S9(3)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            LX14-CY32                                     CI0080
                          REDEFINES            LX14-CY97.               CI0080
            11            LX14-GCUSPZ PICTURE  X(12).                   CI0080


      ******************************************************************
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *
      ******************************************************************
      *
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0080
          05              DE00-SUITE.                                   CI0080
            15       FILLER         PICTURE  X(00653).                  CI0080
       01                 DE10  REDEFINES      DE00.                    CI0080
            10            DE10-DU11.                                    CI0080
            11            DE10-XFONC  PICTURE  X(4).                    CI0080
            11            DE10-MPSBN  PICTURE  X(8).                    CI0080
            11            DE10-XDBDNM PICTURE  X(08).                   CI0080
            11            DE10-XSEGNM PICTURE  X(08).                   CI0080
            11            DE10-XRC    PICTURE  X(02).                   CI0080
            11            DE10-MSEG   PICTURE  X(08).                   CI0080
            11            DE10-XCOKEY PICTURE  X(70).                   CI0080
            11            DE10-CUIBR  PICTURE  X(01).                   CI0080
            11            DE10-CUIBA  PICTURE  X(01).                   CI0080
            11            DE10-IPBIK  PICTURE  X(1).                    CI0080
            10            DE10-DU03.                                    CI0080
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            DE10-CMSSF  PICTURE  XX.                      CI0080
            11            DE10-DU09.                                    CI0080
            12            DE10-CMESA  PICTURE  S9(9)                    CI0080
                          BINARY.                                       CI0080
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0080
                          BINARY.                                       CI0080
            12            DE10-CMESB  PICTURE  S9(9)                    CI0080
                          BINARY.                                       CI0080
            12            DE10-CMSST  PICTURE  S9(9)                    CI0080
                          BINARY.                                       CI0080
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0080
                          BINARY.                                       CI0080
            12            DE10-QELLAA PICTURE  S9(9)                    CI0080
                          BINARY.                                       CI0080
            12            DE10-TMESS4 PICTURE  X(512).                  CI0080

      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0080
          05              MS00-SUITE.                                   CI0080
            15       FILLER         PICTURE  X(00542).                  CI0080
       01                 MS03  REDEFINES      MS00.                    CI0080
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            10            MS03-CMSSF  PICTURE  XX.                      CI0080
            10            MS03-DU09.                                    CI0080
            11            MS03-CMESA  PICTURE  S9(9)                    CI0080
                          BINARY.                                       CI0080
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0080
                          BINARY.                                       CI0080
            11            MS03-CMESB  PICTURE  S9(9)                    CI0080
                          BINARY.                                       CI0080
            11            MS03-CMSST  PICTURE  S9(9)                    CI0080
                          BINARY.                                       CI0080
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0080
                          BINARY.                                       CI0080
            11            MS03-QELLAA PICTURE  S9(9)                    CI0080
                          BINARY.                                       CI0080
            11            MS03-TMESS4 PICTURE  X(512).                  CI0080
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0080
            10            MX11-QMSGS  PICTURE  9(03).                   CI0080
            10            MX11-PJ09                                     CI0080
                          OCCURS       025     TIMES.                   CI0080
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0080
                          COMPUTATIONAL-3.                              CI0080
            11            MX11-CMESB  PICTURE  S9(9)                    CI0080
                          BINARY.                                       CI0080
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                DS80
                                LX13
                                LX14
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0080
      *               *                                   *             CI0080
      *               *INITIALISATIONS                    *             CI0080
      *               *                                   *             CI0080
      *               *************************************.            CI0080
       F01.      EXIT.
       F01-FN.   EXIT.
      *N02.      NOTE *************************************.            ADU102
      *               *                                   *             ADU102
      *               *---> Module Initializations        *             ADU102
      *               *                                   *             ADU102
      *               *************************************.            ADU102
       F02.                                                             lv05
      *                                                                 ADU102
      *N02AA.    NOTE *SET ADDRESSES                      *.
       F02AA.                                                           lv10
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF XA06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF XB06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR TR1P                                             DOT
           SET ADDRESS OF XC06 TO                                       ADU015
                PCB-TR1P-PTR1.                                          ADU015
       F02AA-FN. EXIT.
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
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0080
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0080
      *               *                                   *             CI0080
      *               *FIN DE TRAITEMENT                  *             CI0080
      *               *                                   *             CI0080
      *               *************************************.            CI0080
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0080
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *EDIT INPUT DATA                    *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40BA.    NOTE *CK DS80 FIELDS FOR NUMERICS        *.
       F40BA.    IF    DS80-CARTZ NOT NUMERIC                           lv10
                 OR    DS80-NAPDS NOT NUMERIC
                 OR    DS80-DCACG NOT NUMERIC
                 OR    DS80-CAUNIT NOT NUMERIC
                 OR    DS80-NPISQ NOT NUMERIC
                 OR    DS80-CPMTF NOT NUMERIC
                 OR    DS80-CPMTG NOT NUMERIC
                 NEXT SENTENCE ELSE GO TO     F40BA-FN.
           PERFORM     F92ZA THRU F92ZA-FN.
       F40BA-FN. EXIT.
      *N40CA.    NOTE *CK LX13 COMMON FIELDS FOR NUM      *.
       F40CA.    IF    LX13-CARTZ NOT NUMERIC                           lv10
                 OR    LX13-NAPDS NOT NUMERIC
                 OR    LX13-GESTD NOT NUMERIC
                 OR    LX13-GEEND NOT NUMERIC
                 OR    LX13-DASUQ NOT NUMERIC
                 OR    LX13-CDEST NOT NUMERIC
                 OR    LX13-DLAUP NOT NUMERIC
                 OR    LX13-GEAUN NOT NUMERIC
                 OR    LX13-DPCHD NOT NUMERIC
                 OR    LX13-PPOT1 NOT NUMERIC
                 OR    LX13-ACOT1 NOT NUMERIC
                 OR    LX13-QPST1 NOT NUMERIC
                 NEXT SENTENCE ELSE GO TO     F40CA-FN.
           PERFORM     F92ZA THRU F92ZA-FN.
       F40CA-FN. EXIT.
      *N40DA.    NOTE *FUND SP                            *.
       F40DA.    IF    LX13-CARTZ = 01                                  lv10
                 OR    LX13-CARTZ = 02
                 OR    LX13-CARTZ = 04
                 OR    LX13-CARTZ = 07
                 OR    LX13-CARTZ = 08
                 OR    LX13-CARTZ = 09
                 NEXT SENTENCE ELSE GO TO     F40DA-FN.
      *CERT CP
      *ANNUITY PMT (AP)
      *ANNUITY DISTRIB (AD)
      *LIFE CLAIM DISTRIB (CD)
      *INT ONLY ANNUITY DISTRIB (IO)
      *N40EA.    NOTE *CK CY21 FIELDS (REDEFINES CY96)    *.
       F40EA.    IF    LX13-DNPMT NOT NUMERIC                           lv15
                 OR    LX13-CPMTF NOT NUMERIC
                 OR    LX13-ADBRQ NOT NUMERIC
                 OR    LX13-QSHOWQ NOT NUMERIC
                 OR    LX13-PACT1 NOT NUMERIC
                 OR    LX13-DOPDA NOT NUMERIC
                 OR    LX13-DNEXE NOT NUMERIC
                 NEXT SENTENCE ELSE GO TO     F40EA-FN.
           PERFORM     F92ZA THRU F92ZA-FN.
       F40EA-FN. EXIT.
       F40DA-FN. EXIT.
      *N40FA.    NOTE *ANNUITY DISTRIB - INT ONLY         *.
       F40FA.    IF    LX13-CARTZ = 09                                  lv10
                 NEXT SENTENCE ELSE GO TO     F40FA-FN.
      *N40FB.    NOTE *CK CY28 FIELDS (REDEFINES CY98)    *.
       F40FB.    IF    LX13-AALLBL NOT NUMERIC                          lv15
                 OR    LX13-PSURR NOT NUMERIC
                 NEXT SENTENCE ELSE GO TO     F40FB-FN.
           PERFORM     F92ZA THRU F92ZA-FN.
       F40FB-FN. EXIT.
       F40FA-FN. EXIT.
      *N40HA.    NOTE *FUND SP                            *.
       F40HA.    IF    LX13-CARTZ = 01                                  lv10
                 NEXT SENTENCE ELSE GO TO     F40HA-FN.
      *N40HB.    NOTE *CK CY27 FIELDS (REDEFINES CY98)    *.
       F40HB.    IF    LX13-QMTH1 NOT NUMERIC                           lv15
                 NEXT SENTENCE ELSE GO TO     F40HB-FN.
           PERFORM     F92ZA THRU F92ZA-FN.
       F40HB-FN. EXIT.
       F40HA-FN. EXIT.
      *N40OA.    NOTE *ARRANGEMENT ACTION CODE NOT = D    *.
       F40OA.    IF    DS80-CACTA NOT = 'D'                             lv10
                 NEXT SENTENCE ELSE GO TO     F40OA-FN.
      *N40PA.    NOTE *CK LX14 COMMON FIELDS FOR NUM      *.
       F40PA.    IF    LX14-NPISQ NOT NUMERIC                           lv15
                 OR    LX14-ACOTD NOT NUMERIC
                 OR    LX14-PPOTD NOT NUMERIC
                 OR    LX14-QPSTD NOT NUMERIC
                 OR    LX14-CPITC NOT NUMERIC
                 NEXT SENTENCE ELSE GO TO     F40PA-FN.
           PERFORM     F92ZA THRU F92ZA-FN.
       F40PA-FN. EXIT.
      *N40QA.    NOTE *DESTINATION IS AEFA ACCOUNT        *.
       F40QA.    IF    LX14-CPITC = 02                                  lv15
                 NEXT SENTENCE ELSE GO TO     F40QA-FN.
      *N40RA.    NOTE *CK CY30 FIELDS (REDEFINES CY97)    *.
       F40RA.    IF    LX14-CTID NOT NUMERIC                            lv20
                 OR    LX14-CPMTC NOT NUMERIC
                 NEXT SENTENCE ELSE GO TO     F40RA-FN.
           PERFORM     F92ZA THRU F92ZA-FN.
       F40RA-FN. EXIT.
       F40QA-FN. EXIT.
      *N40SA.    NOTE *DESTINATION IS PAYEE               *.
       F40SA.    IF    LX14-CPITC = 01                                  lv15
                 NEXT SENTENCE ELSE GO TO     F40SA-FN.
      *N40TA.    NOTE *CK CY31 FIELDS (REDEFINES CY97)    *.
       F40TA.    IF    LX14-CDEL1 NOT NUMERIC                           lv20
                 OR    LX14-NDELS NOT NUMERIC
                 NEXT SENTENCE ELSE GO TO     F40TA-FN.
           PERFORM     F92ZA THRU F92ZA-FN.
       F40TA-FN. EXIT.
       F40SA-FN. EXIT.
       F40OA-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *CT01, CX13 & CX14 PROCESSING       *
      *               *                                   *
      *               *************************************.
       F45.           EXIT.                                             lv05
      *N45BA.    NOTE *ACTION NOT DELETE OR INACTIVATE    *.
       F45BA.    IF    DS80-CACTA NOT = 'D'                             lv10
                 AND   DS80-CACTA NOT = 'I'
                 NEXT SENTENCE ELSE GO TO     F45BA-FN.
           MOVE        DS80-CLID TO S-CXU01-CLID
           MOVE        DS80-CX03K TO S-CXU03-CX03K
           MOVE        DS80-CTID TO S-CXU06-CTID
           MOVE        DS80-CARTZ TO S-CXU13-CARTZ
           MOVE        DS80-NAPDS TO S-CXU13-NAPDS
      *GU CX13
           MOVE        SPACE TO CX13-IK
           PERFORM     F94C1 THRU F94C1-FN.
      *N45CA.    NOTE *CX13 FOUND                         *.
       F45CA.    IF    CX13-IK = '0'                                    lv15
                 NEXT SENTENCE ELSE GO TO     F45CA-FN.
           MOVE        DS80-NPISQ TO S-CXU14-NPISQ
      *GU CX14
           MOVE        SPACE TO CX14-IK
           PERFORM     F94C2 THRU F94C2-FN.
      *N45DA.    NOTE *CX14 NOT FOUND                     *.
       F45DA.    IF    CX14-IK = '1'                                    lv20
                 NEXT SENTENCE ELSE GO TO     F45DA-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012026 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45DA-FN. EXIT.
       F45CA-900. GO TO F45EA-FN.
       F45CA-FN. EXIT.
      *N45EA.    NOTE *CX13 NOT FOUND                     *.
       F45EA.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012009 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45EA-FN. EXIT.
       F45BA-FN. EXIT.
      *N45FA.    NOTE *GHU CT01                           *.
       F45FA.                                                           lv10
           MOVE        'N' TO W-CT01-CDSTR
           MOVE        DS80-CTID TO S-CTU01-CTID
           MOVE        SPACE TO CT01-IK
           PERFORM     F94C3 THRU F94C3-FN.
       F45FA-FN. EXIT.
      *N45GA.    NOTE *GHU ON CT01 SUCCESSFUL             *.
       F45GA.    IF    CT01-IK = '0'                                    lv10
                 NEXT SENTENCE ELSE GO TO     F45GA-FN.
      *N45HA.    NOTE *DIVIDEND ARRANGEMENT               *.
       F45HA.    IF    DS80-CARTZ = 05                                  lv15
                 NEXT SENTENCE ELSE GO TO     F45HA-FN.
      *N45IA.    NOTE *DELETE OR INACTIVATE               *.
       F45IA.    IF    DS80-CACTA = 'D' OR 'I'                          lv20
                 NEXT SENTENCE ELSE GO TO     F45IA-FN.
      *SET DIST CODE TO REINVEST                                        DOT
           MOVE        'R' TO DS80-CDSTR.
       F45IA-900. GO TO F45IH-FN.
       F45IA-FN. EXIT.
      *N45IH.    NOTE *ADD, CHANGE, REACTIVATE            *.
       F45IH.         EXIT.                                             lv20
      *N45IJ.    NOTE *DISTRIBUTION CODE IS REINVEST      *.
       F45IJ.    IF    DS80-CDSTR = 'R'                                 lv25
                 NEXT SENTENCE ELSE GO TO     F45IJ-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012839 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45IJ-FN. EXIT.
       F45IH-FN. EXIT.
      *N45JA.    NOTE *DIST CODE NOT SAME AS ON CT01      *.
       F45JA.    IF    DS80-CDSTR NOT = CT01-CDSTR                      lv20
                 NEXT SENTENCE ELSE GO TO     F45JA-FN.
           MOVE        'Y' TO W-CT01-CDSTR
           MOVE        DS80-CDSTR TO CT01-CDSTR
           MOVE        DS80-DCACG TO CT01-GEMDA
           ADD         +1 TO CT01-NSEQ4B
      *REPL CT01
           MOVE        SPACE TO CT01-IK
           PERFORM     F94C4 THRU F94C4-FN.
       F45JA-FN. EXIT.
       F45HA-FN. EXIT.
       F45GA-900. GO TO F45KA-FN.
       F45GA-FN. EXIT.
      *N45KA.    NOTE *CT01 NOT FOUND                     *.
       F45KA.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012011 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45KA-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *FUNDS                              *
      *               *                                   *
      *               *************************************.
       F50.      IF    DS80-CTIDA = 002                                 lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *N50CA.    NOTE *FUND SCHEDULED DISBURSEMENT        *.
       F50CA.    IF    DS80-CARTZ = 01                                  lv10
                 NEXT SENTENCE ELSE GO TO     F50CA-FN.
      *N50DA.    NOTE *ADD                                *.
       F50DA.    IF    DS80-CACTA = 'A'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50DA-FN.
           MOVE        DS80-CLID TO 7-AOACFM-CLID
           MOVE        ZERO TO 7-AOACFM-CACKD
           MOVE        DS80-DCACG TO 7-AOACFM-DCACG
           MOVE        'S' TO 7-AOACFM-CRETP
           MOVE        SPACE TO GQ01-IK
      *WRITE MISC TRAN 51
      *'PAYOUT CONFIRMATION TRIGGER'
           PERFORM     F91LM THRU F91LM-FN.
       F50DA-FN. EXIT.
      *N50EA.    NOTE *CHANGE OR REACTIVATE               *.
       F50EA.    IF    DS80-CACTA = 'C' OR 'R'                          lv15
                 NEXT SENTENCE ELSE GO TO     F50EA-FN.
      *N50FA.    NOTE *NEW CX13 OR CX14 NOT = OLD         *.
       F50FA.    IF    CX13-CPMTF NOT = LX13-CPMTF                      lv20
                 OR    CX13-DNPMT NOT = LX13-DNPMT
                 OR    CX13-ADBRQ NOT = LX13-ADBRQ
                 OR    CX13-QSHOWQ NOT =
                       LX13-QSHOWQ
                 OR    CX13-PACT1 NOT = LX13-PACT1
                 OR    CX13-QMTH1 NOT = LX13-QMTH1
                 OR    CX14 NOT = LX14
                 NEXT SENTENCE ELSE GO TO     F50FA-FN.
      *N50FD.    NOTE *DESTINATION IS AEFA ACCOUNT        *.
       F50FD.    IF    CX14-CPITC = 02                                  lv25
                 NEXT SENTENCE ELSE GO TO     F50FD-FN.
      *N50FG.    NOTE *NEW DEST ACCT ID NOT = OLD         *.
       F50FG.    IF    CX14-CTID NOT = LX14-CTID                        lv30
                 NEXT SENTENCE ELSE GO TO     F50FG-FN.
           MOVE        DS80-CLID TO 7-AOACFM-CLID
           MOVE        ZERO TO 7-AOACFM-CACKD
           MOVE        DS80-DCACG TO 7-AOACFM-DCACG
           MOVE        'C' TO 7-AOACFM-CRETP
           MOVE        SPACE TO GQ01-IK
      *WRITE MISC TRAN 51
      *'PAYOUT CONFIRMATION TRIGGER'
           PERFORM     F91LM THRU F91LM-FN.
       F50FG-FN. EXIT.
       F50FD-FN. EXIT.
      *N50FJ.    NOTE *DESTINATION IS PAYEE               *.
       F50FJ.    IF    CX14-CPITC = 01                                  lv25
                 NEXT SENTENCE ELSE GO TO     F50FJ-FN.
      *N50FM.    NOTE *NEW DELIV INSTR FIELDS NOT = OLD   *.
       F50FM.    IF    CX14-IDELI NOT = LX14-IDELI                      lv30
                 OR    CX14-CDEL1 NOT = LX14-CDEL1
                 OR    CX14-NDELS NOT = LX14-NDELS
                 NEXT SENTENCE ELSE GO TO     F50FM-FN.
           MOVE        DS80-CLID TO 7-AOACFM-CLID
           MOVE        ZERO TO 7-AOACFM-CACKD
           MOVE        DS80-DCACG TO 7-AOACFM-DCACG
           MOVE        'C' TO 7-AOACFM-CRETP
           MOVE        SPACE TO GQ01-IK
      *WRITE MISC 51
      *'PAYOUT CONFIRMATION TRIGGER'
           PERFORM     F91LM THRU F91LM-FN.
       F50FM-FN. EXIT.
       F50FJ-FN. EXIT.
       F50FA-FN. EXIT.
       F50EA-FN. EXIT.
      *N50GA.    NOTE *DELETE OR INACTIVATE               *.
       F50GA.    IF    DS80-CACTA = 'D' OR 'I'                          lv15
                 NEXT SENTENCE ELSE GO TO     F50GA-FN.
           MOVE        DS80-CLID TO 7-AOACFM-CLID
           MOVE        DS80-DCACG TO 7-AOACFM-DCACG
           MOVE        'C' TO 7-AOACFM-CRETP
           MOVE        SPACE TO GQ01-IK
      *WRITE MISC TRAN 51
      *'PAYOUT CONFIRMATION TRIGGER'
           PERFORM     F91LM THRU F91LM-FN.
       F50GA-FN. EXIT.
       F50CA-FN. EXIT.
      *N50MA.    NOTE *FUND DIVIDEND ARRANGEMENT          *.
       F50MA.    IF    DS80-CARTZ = 05                                  lv10
                 NEXT SENTENCE ELSE GO TO     F50MA-FN.
      *N50NA.    NOTE *ADD                                *.
       F50NA.    IF    DS80-CACTA = 'A'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50NA-FN.
           MOVE        DS80-CTID TO S-GQU01-CANUMB
           MOVE        00043 TO S-GQU01-CAMCTR
           MOVE        1 TO S-GQU01-GESQ2
      *GHU GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C5 THRU F94C5-FN.
      *N50ND.    NOTE *GHU ON GQ01 SUCCESSFUL             *.
       F50ND.    IF    GQ01-IK = '0'                                    lv20
                 NEXT SENTENCE ELSE GO TO     F50ND-FN.
           MOVE        GQ01-XMISL TO GS43.
      *N50NG.    NOTE *REINV CD NOT SAME AS ON MISC TRN   *.
       F50NG.    IF    DS80-CDSTR NOT = GS43-CDSTR                      lv25
                 NEXT SENTENCE ELSE GO TO     F50NG-FN.
           MOVE        DS80-CDSTR TO GS43-CDSTR
           MOVE        GS43 TO GQ01-XMISL
      *REPL GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C6 THRU F94C6-FN.
       F50NG-FN. EXIT.
       F50ND-FN. EXIT.
      *N50NK.    NOTE *WRITE MISC TRAN 52 AND 45          *.
       F50NK.                                                           lv20
      *'DIVIDEND CONFIRMATION TRIGGER'
           PERFORM     F91BA THRU F91BA-FN
      *'NOTIFY OF DISTRIBUTION CHANGE'
           PERFORM     F91CA THRU F91CA-FN.
       F50NK-FN. EXIT.
       F50NA-FN. EXIT.
      *N50OA.    NOTE *CHANGE                             *.
       F50OA.    IF    DS80-CACTA = 'C'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50OA-FN.
      *N50OD.    NOTE *REINVEST CODE HAS CHANGED          *.
       F50OD.    IF    W-CT01-CDSTR = 'Y'                               lv20
                 NEXT SENTENCE ELSE GO TO     F50OD-FN.
           MOVE        DS80-CTID TO S-GQU01-CANUMB
           MOVE        00043 TO S-GQU01-CAMCTR
           MOVE        1 TO S-GQU01-GESQ2
      *GHU GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C5 THRU F94C5-FN.
      *N50OG.    NOTE *GHU ON GQ01 SUCCESSFUL             *.
       F50OG.    IF    GQ01-IK = '0'                                    lv25
                 NEXT SENTENCE ELSE GO TO     F50OG-FN.
           MOVE        GQ01-XMISL TO GS43.
      *N50OK.    NOTE *REINV CD NOT SAME AS ON MISC TRN   *.
       F50OK.    IF    DS80-CDSTR NOT = GS43-CDSTR                      lv30
                 NEXT SENTENCE ELSE GO TO     F50OK-FN.
           MOVE        DS80-CDSTR TO GS43-CDSTR
           MOVE        GS43 TO GQ01-XMISL
      *REPL GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C6 THRU F94C6-FN.
       F50OK-FN. EXIT.
       F50OG-FN. EXIT.
       F50OD-FN. EXIT.
      *N50OO.    NOTE *REINVEST CODE CHG OR CX14 DID      *.
       F50OO.    IF    W-CT01-CDSTR = 'Y'                               lv20
                 OR    CX14 NOT = LX14
                 NEXT SENTENCE ELSE GO TO     F50OO-FN.
      *WRITE MISC TRAN 52
      *'DIVIDEND CONFIRMATION TRIGGER'
           PERFORM     F91BA THRU F91BA-FN
      *'NOTIFY OF DISTRIBUTION CHANGE'
           PERFORM     F91CA THRU F91CA-FN.
       F50OO-FN. EXIT.
       F50OA-FN. EXIT.
      *N50PA.    NOTE *DELETE                             *.
       F50PA.    IF    DS80-CACTA = 'D'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50PA-FN.
      *N50PD.    NOTE *REINVEST CODE HAS CHANGED          *.
       F50PD.    IF    W-CT01-CDSTR = 'Y'                               lv20
                 NEXT SENTENCE ELSE GO TO     F50PD-FN.
           MOVE        DS80-CTID TO S-GQU01-CANUMB
           MOVE        00043 TO S-GQU01-CAMCTR
           MOVE        1 TO S-GQU01-GESQ2
      *GHU GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C5 THRU F94C5-FN.
      *N50PG.    NOTE *GHU ON GQ01 SUCCESSFUL             *.
       F50PG.    IF    GQ01-IK = '0'                                    lv25
                 NEXT SENTENCE ELSE GO TO     F50PG-FN.
           MOVE        GQ01-XMISL TO GS43.
      *N50PK.    NOTE *REINV CD NOT SAME AS ON MISC TRN   *.
       F50PK.    IF    DS80-CDSTR NOT = GS43-CDSTR                      lv30
                 NEXT SENTENCE ELSE GO TO     F50PK-FN.
           MOVE        DS80-CDSTR TO GS43-CDSTR
           MOVE        GS43 TO GQ01-XMISL
      *REPL GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C6 THRU F94C6-FN.
       F50PK-FN. EXIT.
       F50PG-FN. EXIT.
      *N50PO.    NOTE *ACTIVE ARR - WRITE MISC TRAN 52    *.
       F50PO.    IF    LX13-CDEST = 01                                  lv25
                 NEXT SENTENCE ELSE GO TO     F50PO-FN.
      *'DIVIDEND CONFIRMATION TRIGGER'
           PERFORM     F91BA THRU F91BA-FN
      *'NOTIFY OF DISTRIBUTION CHANGE'
           PERFORM     F91CA THRU F91CA-FN.
       F50PO-FN. EXIT.
       F50PD-FN. EXIT.
       F50PA-FN. EXIT.
      *N50QA.    NOTE *INACTIVATE                         *.
       F50QA.    IF    DS80-CACTA = 'I'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50QA-FN.
      *N50QD.    NOTE *REINVEST CODE HAS CHANGED          *.
       F50QD.    IF    W-CT01-CDSTR = 'Y'                               lv20
                 NEXT SENTENCE ELSE GO TO     F50QD-FN.
           MOVE        DS80-CTID TO S-GQU01-CANUMB
           MOVE        00043 TO S-GQU01-CAMCTR
           MOVE        1 TO S-GQU01-GESQ2
      *GHU GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C5 THRU F94C5-FN.
      *N50QG.    NOTE *GHU ON GQ01 SUCCESSFUL             *.
       F50QG.    IF    GQ01-IK = '0'                                    lv25
                 NEXT SENTENCE ELSE GO TO     F50QG-FN.
           MOVE        GQ01-XMISL TO GS43.
      *N50QK.    NOTE *REINV CD NOT SAME AS ON MISC TRN   *.
       F50QK.    IF    DS80-CDSTR NOT = GS43-CDSTR                      lv30
                 NEXT SENTENCE ELSE GO TO     F50QK-FN.
           MOVE        DS80-CDSTR TO GS43-CDSTR
           MOVE        GS43 TO GQ01-XMISL
      *REPL GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C6 THRU F94C6-FN.
       F50QK-FN. EXIT.
       F50QG-FN. EXIT.
      *N50QO.    NOTE *WRITE MISC TRAN 52                 *.
       F50QO.                                                           lv25
      *'DIVIDEND CONFIRMATION TRIGGER'
           PERFORM     F91BA THRU F91BA-FN
      *'NOTIFY OF DISTRIBUTION CHANGE'
           PERFORM     F91CA THRU F91CA-FN.
       F50QO-FN. EXIT.
       F50QD-FN. EXIT.
       F50QA-FN. EXIT.
      *N50RA.    NOTE *REACTIVATE                         *.
       F50RA.    IF    DS80-CACTA = 'R'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50RA-FN.
      *N50RD.    NOTE *REINVEST CODE HAS CHANGED          *.
       F50RD.    IF    W-CT01-CDSTR = 'Y'                               lv20
                 NEXT SENTENCE ELSE GO TO     F50RD-FN.
           MOVE        DS80-CTID TO S-GQU01-CANUMB
           MOVE        00043 TO S-GQU01-CAMCTR
           MOVE        1 TO S-GQU01-GESQ2
      *GHU GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C5 THRU F94C5-FN.
      *N50RG.    NOTE *GHU ON GQ01 SUCCESSFUL             *.
       F50RG.    IF    GQ01-IK = '0'                                    lv25
                 NEXT SENTENCE ELSE GO TO     F50RG-FN.
           MOVE        GQ01-XMISL TO GS43.
      *N50RK.    NOTE *REINV CD NOT SAME AS ON MISC TRN   *.
       F50RK.    IF    DS80-CDSTR NOT = GS43-CDSTR                      lv30
                 NEXT SENTENCE ELSE GO TO     F50RK-FN.
           MOVE        DS80-CDSTR TO GS43-CDSTR
           MOVE        GS43 TO GQ01-XMISL
      *REPL GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C6 THRU F94C6-FN.
       F50RK-FN. EXIT.
       F50RG-FN. EXIT.
      *N50RO.    NOTE *WRITE MISC TRAN 52                 *.
       F50RO.                                                           lv25
      *'DIVIDEND CONFIRMATION TRIGGER'
           PERFORM     F91BA THRU F91BA-FN
      *'NOTIFY OF DISTRIBUTION CHANGE'
           PERFORM     F91CA THRU F91CA-FN.
       F50RO-FN. EXIT.
       F50RD-FN. EXIT.
       F50RA-FN. EXIT.
       F50MA-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *CERTS                              *
      *               *                                   *
      *               *************************************.
       F55.      IF    DS80-CTIDA = 001                                 lv05
                 NEXT SENTENCE ELSE GO TO     F55-FN.
      *N55CA.    NOTE *CERT SCHEDULED DISBURSEMENT        *.
       F55CA.    IF    DS80-CARTZ = 02                                  lv10
                 NEXT SENTENCE ELSE GO TO     F55CA-FN.
      *N55DA.    NOTE *ADD                                *.
       F55DA.    IF    DS80-CACTA = 'A'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F55DA-FN.
           MOVE        DS80-CLID TO 7-AOACFM-CLID
           MOVE        ZERO TO 7-AOACFM-CACKD
           MOVE        DS80-DCACG TO 7-AOACFM-DCACG
           MOVE        'S' TO 7-AOACFM-CRETP
           MOVE        SPACE TO GQ01-IK
      *WRITE MISC TRAN 51
      *'PAYOUT CONFIRMATION TRIGGER'
           PERFORM     F91LM THRU F91LM-FN.
       F55DA-FN. EXIT.
      *N55EA.    NOTE *CHANGE                             *.
       F55EA.    IF    DS80-CACTA = 'C' OR 'R'                          lv15
                 NEXT SENTENCE ELSE GO TO     F55EA-FN.
      *N55FG.    NOTE *NEW NOT = OLD ON ANY OF FREQ,      *.
       F55FG.    IF    CX13-CPMTF NOT = LX13-CPMTF                      lv20
                 OR    CX13-DNEXE NOT = LX13-DNEXE
                 OR    CX13-ACOT1 NOT = LX13-ACOT1
                 OR    CX14-CY97 NOT = LX14-CY97
                 NEXT SENTENCE ELSE GO TO     F55FG-FN.
      *NXT EXEC DT, AMOUNT OR DEST CHGD
           MOVE        DS80-CLID TO 7-AOACFM-CLID
           MOVE        ZERO TO 7-AOACFM-CACKD
           MOVE        DS80-DCACG TO 7-AOACFM-DCACG.
                 IF    CX13-ACOT1 NOT = LX13-ACOT1                      DOT
           MOVE        'S' TO 7-AOACFM-CRETP
                 ELSE
           MOVE        'C' TO 7-AOACFM-CRETP.
           MOVE        SPACE TO GQ01-IK                                 DOT
      *WRITE MISC TRAN 51
      *'PAYOUT CONFIRMATION TRIGGER'
           PERFORM     F91LM THRU F91LM-FN.
       F55FG-FN. EXIT.
       F55EA-FN. EXIT.
      *N55GA.    NOTE *DELETE OR INACTIVATE               *.
       F55GA.    IF    DS80-CACTA = 'D' OR 'I'                          lv15
                 NEXT SENTENCE ELSE GO TO     F55GA-FN.
           MOVE        DS80-CLID TO 7-AOACFM-CLID
           MOVE        DS80-DCACG TO 7-AOACFM-DCACG
           MOVE        'C' TO 7-AOACFM-CRETP
           MOVE        SPACE TO GQ01-IK
      *WRITE MISC TRAN 51
      *PAYOUT CONFIRMATION TRIGGER
           PERFORM     F91LM THRU F91LM-FN.
       F55GA-FN. EXIT.
       F55CA-FN. EXIT.
      *N55MA.    NOTE *CERT INTEREST ARRANGEMENT          *.
       F55MA.    IF    DS80-CARTZ = 06                                  lv10
                 NEXT SENTENCE ELSE GO TO     F55MA-FN.
      *N55NA.    NOTE *ADD                                *.
       F55NA.    IF    DS80-CACTA = 'A'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F55NA-FN.
      *N55ND.    NOTE *WRITE MISC TRAN 55                 *.
       F55ND.                                                           lv20
      *'CERT INTPAY SCHEDULING'
           PERFORM     F91DA THRU F91DA-FN.
       F55ND-FN. EXIT.
      *N55NG.    NOTE *WRITE MISC TRN 52 (NOT FOR AEBI)   *.
       F55NG.    IF    CT01-PRCOD NOT = 00201                           lv20
                 AND   CT01-PRCOD NOT = 00202
                 AND   CT01-PRCOD NOT = 00203
                 AND   CT01-PRCOD NOT = 00204
                 AND   CT01-PRCOD NOT = 00205
                 AND   CT01-PRCOD NOT = 00301
                 AND   CT01-PRCOD NOT = 00302
                 AND   CT01-PRCOD NOT = 00303
                 AND   CT01-PRCOD NOT = 00304
                 AND   CT01-PRCOD NOT = 00305
                 NEXT SENTENCE ELSE GO TO     F55NG-FN.
      *'DIVIDEND CONFIRMATION TRIGGER'
           PERFORM     F91BA THRU F91BA-FN.
       F55NG-FN. EXIT.
      *N55NJ.    NOTE *WRITE MISC TRAN 56                 *.
       F55NJ.                                                           lv20
      *'CERT INTPAY DELIVERY'
           PERFORM     F91GA THRU F91GA-FN.
       F55NJ-FN. EXIT.
       F55NA-FN. EXIT.
      *N55OA.    NOTE *CHANGE                             *.
       F55OA.    IF    DS80-CACTA = 'C'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F55OA-FN.
      *N55OD.    NOTE *FREQ CHG - WRITE MISC TRAN 55      *.
       F55OD.    IF    DS80-CPMTF NOT = DS80-CPMTG                      lv20
                 NEXT SENTENCE ELSE GO TO     F55OD-FN.
      *'CERT INTPAY SCHEDULING'
           PERFORM     F91DA THRU F91DA-FN.
       F55OD-FN. EXIT.
      *N55OG.    NOTE *WRITE MISC TRAN 56 IF DEST CHNG    *.
       F55OG.    IF    CX14-CY97 NOT = LX14-CY97                        lv20
                 NEXT SENTENCE ELSE GO TO     F55OG-FN.
      *'CERT INTPAY DELIVERY'
           PERFORM     F91GA THRU F91GA-FN.
       F55OG-FN. EXIT.
       F55OA-FN. EXIT.
      *N55PA.    NOTE *DELETE                             *.
       F55PA.    IF    DS80-CACTA = 'D'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F55PA-FN.
      *N55PB.    NOTE *WRITE MISC TRN 52 (NOT FOR AEBI)   *.
       F55PB.    IF    CT01-PRCOD NOT = 00201                           lv20
                 AND   CT01-PRCOD NOT = 00202
                 AND   CT01-PRCOD NOT = 00203
                 AND   CT01-PRCOD NOT = 00204
                 AND   CT01-PRCOD NOT = 00205
                 AND   CT01-PRCOD NOT = 00301
                 AND   CT01-PRCOD NOT = 00302
                 AND   CT01-PRCOD NOT = 00303
                 AND   CT01-PRCOD NOT = 00304
                 AND   CT01-PRCOD NOT = 00305
                 NEXT SENTENCE ELSE GO TO     F55PB-FN.
      *'DIVIDEND CONFIRMATION TRIGGER'
           PERFORM     F91BA THRU F91BA-FN.
       F55PB-FN. EXIT.
      *N55PD.    NOTE *WRITE MISC TRAN 55                 *.
       F55PD.                                                           lv20
      *'CERT INTPAY SCHEDULING'
           PERFORM     F91DA THRU F91DA-FN.
       F55PD-FN. EXIT.
       F55PA-FN. EXIT.
      *N55QA.    NOTE *INACTIVATE; NOT VALID FOR CI       *.
       F55QA.    IF    DS80-CACTA = 'I'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F55QA-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012835 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F55QA-FN. EXIT.
       F55MA-FN. EXIT.
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
      *N91BA.    NOTE *MISC TRAN 00052                    *.
       F91BA.                                                           lv10
      *'DIVIDEND CONFIRMATION TRIGGER'
           INITIALIZE  GQ01 GS52
           MOVE        DS80-CTID TO GQ01-CANUMB
           MOVE        00052 TO GQ01-CAMCTR
           MOVE        1 TO GQ01-GESQ2
      *FILL IN COMMON MISC TRAN FIELDS
           MOVE        00052 TO TA75-CAMCTR
           PERFORM     F91JD THRU F91JD-FN
      *FILL IN VARIABLE MISC TRN FIELDS
           MOVE        DS80-CLID TO GS52-CLID
           MOVE        DS80-CDSTR TO GS52-CDSTR.
                 IF    DS80-CACTA = 'C'                                 DOT
                 OR    DS80-CACTA = 'D'
                 OR    DS80-CACTA = 'I'
      *CHANGE, DELETE, INACTIVATE
           MOVE        'C' TO GS52-CRETP.
                 IF    DS80-CACTA = 'A'                                 DOT
      *ADD
           MOVE        'S' TO GS52-CRETP.
           MOVE        GS52 TO GQ01-XMISL                               DOT
      *TRY TO INSERT MISC TRAN RECORD
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C7 THRU F94C7-FN.
      *N91BB.    NOTE *SEGM ALREADY EXISTS; REPLACE IT    *.
       F91BB.    IF    XW05-XRC = 'II'                                  lv15
                 NEXT SENTENCE ELSE GO TO     F91BB-FN.
           MOVE        GQ01-CANUMB TO S-GQU01-CANUMB
           MOVE        GQ01-CAMCTR TO S-GQU01-CAMCTR
           MOVE        GQ01-GESQ2 TO S-GQU01-GESQ2
      *GHU GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C5 THRU F94C5-FN.
      *N91BC.    NOTE *GHU ON GQ01 SUCCESSFUL             *.
       F91BC.    IF    GQ01-IK = '0'                                    lv20
                 NEXT SENTENCE ELSE GO TO     F91BC-FN.
      *FILL IN COMMON MISC TRAN FIELDS
           MOVE        00052 TO TA75-CAMCTR
           PERFORM     F91JD THRU F91JD-FN
      *FILL IN VARIABLE MISC TRN FIELDS
      *(LEAVE CRETP AS IT WAS)
           MOVE        GQ01-XMISL TO GS52
           MOVE        DS80-CLID TO GS52-CLID
           MOVE        DS80-CDSTR TO GS52-CDSTR
           MOVE        GS52 TO GQ01-XMISL
      *REPLACE GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C6 THRU F94C6-FN.
       F91BC-FN. EXIT.
       F91BB-FN. EXIT.
       F91BA-FN. EXIT.
      *N91CA.    NOTE *MISC TRAN 00045                    *.
       F91CA.                                                           lv10
      *'DIVIDEND CONFIRMATION TRIGGER'
           INITIALIZE  GQ01 GS45
           MOVE        DS80-CTID TO GQ01-CANUMB
           MOVE        00045 TO GQ01-CAMCTR
           MOVE        1 TO GQ01-GESQ2
      *FILL IN COMMON MISC TRAN FIELDS
           MOVE        00045 TO TA75-CAMCTR
           PERFORM     F91JD THRU F91JD-FN
      *FILL IN VARIABLE MISC TRN FIELDS
           MOVE        DS80-CDSTR TO GS45-CDSTR
           MOVE        GS45 TO GQ01-XMISL
      *TRY TO INSERT MISC TRAN RECORD
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C7 THRU F94C7-FN.
      *N91CB.    NOTE *SEGM ALREADY EXISTS; REPLACE IT    *.
       F91CB.    IF    XW05-XRC = 'II'                                  lv15
                 NEXT SENTENCE ELSE GO TO     F91CB-FN.
           MOVE        GQ01-CANUMB TO S-GQU01-CANUMB
           MOVE        GQ01-CAMCTR TO S-GQU01-CAMCTR
           MOVE        GQ01-GESQ2 TO S-GQU01-GESQ2
      *GHU GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C5 THRU F94C5-FN.
      *N91CC.    NOTE *GHU ON GQ01 SUCCESSFUL             *.
       F91CC.    IF    GQ01-IK = '0'                                    lv20
                 NEXT SENTENCE ELSE GO TO     F91CC-FN.
      *FILL IN COMMON MISC TRAN FIELDS
           MOVE        00045 TO TA75-CAMCTR
           PERFORM     F91JD THRU F91JD-FN
      *FILL IN VARIABLE MISC TRN FIELDS
           MOVE        DS80-CDSTR TO GS45-CDSTR
           MOVE        GS45 TO GQ01-XMISL
      *REPLACE GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C6 THRU F94C6-FN.
       F91CC-FN. EXIT.
       F91CB-FN. EXIT.
       F91CA-FN. EXIT.
      *N91DA.    NOTE *MISC TRAN 00055                    *.
       F91DA.                                                           lv10
      *'CERT INTPAY SCHEDULING'
           INITIALIZE  GQ01 GS55
           MOVE        DS80-CTID TO GQ01-CANUMB
           MOVE        00055 TO GQ01-CAMCTR
           MOVE        01 TO GQ01-GESQ2
      *FILL IN COMMON MISC TRAN FIELDS
           MOVE        00055 TO TA75-CAMCTR
           PERFORM     F91JD THRU F91JD-FN.
      *FILL IN VARIABLE MISC TRN FIELDS                                 DOT
                 IF    DS80-CACTA = 'A' OR 'C'                          DOT
           MOVE        DS80-CPMTG TO GS55-CPMTF.
           MOVE        GS55 TO GQ01-XMISL                               DOT
      *TRY TO INSERT MISC TRAN RECORD
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C7 THRU F94C7-FN.
      *N91DB.    NOTE *SEGM ALREADY EXISTS; REPLACE IT    *.
       F91DB.    IF    XW05-XRC = 'II'                                  lv15
                 NEXT SENTENCE ELSE GO TO     F91DB-FN.
           MOVE        GQ01-CANUMB TO S-GQU01-CANUMB
           MOVE        GQ01-CAMCTR TO S-GQU01-CAMCTR
           MOVE        GQ01-GESQ2 TO S-GQU01-GESQ2
      *GHU GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C5 THRU F94C5-FN.
      *N91DC.    NOTE *GHU ON GQ01 SUCCESSFUL             *.
       F91DC.    IF    GQ01-IK = '0'                                    lv20
                 NEXT SENTENCE ELSE GO TO     F91DC-FN.
      *FILL IN COMMON MISC TRAN FIELDS
           MOVE        00055 TO TA75-CAMCTR
           PERFORM     F91JD THRU F91JD-FN
      *FILL IN VARIABLE MISC TRN FIELDS
           MOVE        GS55 TO GQ01-XMISL
      *REPLACE GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C6 THRU F94C6-FN.
       F91DC-FN. EXIT.
       F91DB-FN. EXIT.
       F91DA-FN. EXIT.
      *N91GA.    NOTE *MISC TRAN 00056                    *.
       F91GA.                                                           lv10
      *'CERT INTPAY DELIVERY'
           INITIALIZE  GQ01 GS56
           MOVE        DS80-CTID TO GQ01-CANUMB
           MOVE        00056 TO GQ01-CAMCTR
           MOVE        1 TO GQ01-GESQ2
      *FILL IN COMMON MISC TRAN FIELDS
           MOVE        00056 TO TA75-CAMCTR
           PERFORM     F91JD THRU F91JD-FN
      *FILL IN VARIABLE MISC TRN FIELDS
           MOVE        CX14-CPITC TO GS56-CPITC
      *(LEAVE GS56-CDEST INITIALIZED AT
      * ZERO. ONLY USED FOR CERT OPTION
      * WHICH WE DON'T PROCESS)
                 IF    CX14-IDELI = 'Y'                                 DOT
                 AND   DS80-CPAYC = 'O '
           MOVE        003 TO GS56-CDELI.
                 IF    CX14-CDEL1 = 002                                 DOT
                 AND   DS80-CPAYC = 'B '
           MOVE        002 TO GS56-CDELI.
                 IF    CX14-IDELI = 'N'                                 DOT
                 AND   DS80-CPAYC = 'O '
           MOVE        004 TO GS56-CDELI.
                 IF    DS80-CPAYC = 'S '                                DOT
           MOVE        004 TO GS56-CDELI.
                 IF    CX14-CDEL1 = 003                                 DOT
                 AND   DS80-CPAYC = 'B '
           MOVE        006 TO GS56-CDELI.
           MOVE        GS56 TO GQ01-XMISL                               DOT
      *TRY TO INSERT MISC TRAN RECORD
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C7 THRU F94C7-FN.
      *N91GB.    NOTE *SEGM ALREADY EXISTS; REPLACE IT    *.
       F91GB.    IF    XW05-XRC = 'II'                                  lv15
                 NEXT SENTENCE ELSE GO TO     F91GB-FN.
           MOVE        GQ01-CANUMB TO S-GQU01-CANUMB
           MOVE        GQ01-CAMCTR TO S-GQU01-CAMCTR
           MOVE        GQ01-GESQ2 TO S-GQU01-GESQ2
      *GHU GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C5 THRU F94C5-FN.
      *N91GC.    NOTE *GHU ON GQ01 SUCCESSFUL             *.
       F91GC.    IF    GQ01-IK = '0'                                    lv20
                 NEXT SENTENCE ELSE GO TO     F91GC-FN.
      *FILL IN COMMON MISC TRAN FIELDS
           MOVE        00056 TO TA75-CAMCTR
           PERFORM     F91JD THRU F91JD-FN
      *FILL IN VARIABLE MISC TRN FIELDS
           MOVE        GS56 TO GQ01-XMISL
      *REPLACE GQ01
           MOVE        SPACE TO GQ01-IK
           PERFORM     F94C6 THRU F94C6-FN.
       F91GC-FN. EXIT.
       F91GB-FN. EXIT.
       F91GA-FN. EXIT.
      *N91JD.    NOTE *FILL IN COMMON MISC TRAN FIELDS    *.
       F91JD.                                                           lv10
      *READ TA75
           PERFORM     F94TA THRU F94TA-FN
           MOVE        TA75-NDLEN TO GQ01-GELL
           MOVE        'A' TO GQ01-CENTT
           MOVE        ZERO TO GQ01-CACKD
           MOVE        DS80-DCACG TO GQ01-CADATE
           MOVE        EIBTIME TO GQ01-GETIM
           MOVE        DS80-GEOPD2 TO GQ01-GEOPID
           MOVE        DS80-CAUNIT TO GQ01-CAUNIT
           MOVE        EIBTRMID TO GQ01-XTERMI
           MOVE        DS80-PROGR TO GQ01-CAPPL
           MOVE        'CATS' TO GQ01-CSYS.
       F91JD-FN. EXIT.
      *N91LM.    NOTE *LOAD MISCELLANEOUS TRAN FIELDS     *.            AOACFM
       F91LM.                                                           lv10
      *INITIALIZE MISC TRAN FIELDS                                      AOACFM
           MOVE        II01 TO GQ01                                     AOACFM
           MOVE        II51 TO GS51                                     AOACFM
           MOVE        7-AOACFM-CLID TO GQ01-CANUMB                     AOACFM
           MOVE        00051 TO GQ01-CAMCTR                             AOACFM
           MOVE        1 TO GQ01-GESQ2                                  AOACFM
      *LOAD COMMON MISC TRAN FIELD                                      AOACFM
           PERFORM     F91LZ THRU F91LZ-FN                              AOACFM
           MOVE        7-AOACFM-CRETP TO GS51-CRETP                     AOACFM
           MOVE        GS51 TO GQ01-XMISL                               AOACFM
           PERFORM     F94C7 THRU F94C7-FN.
      *N91LQ.    NOTE *IS THERE ALREADY A MISC TRAN       *.
       F91LQ.    IF    XW05-XRC = 'II'                                  lv15
                 AND   7-AOACFM-CRETP NOT = 'C'
                 NEXT SENTENCE ELSE GO TO     F91LQ-FN.
      *AND THE TRIGGER NOT  "CHANGE"                                    AOACFM
      *READ THE MISC TRAN FOR UPDATE                                    AOACFM
           MOVE        GQ01-CANUMB TO S-GQU01-CANUMB                    AOACFM
           MOVE        00051 TO S-GQU01-CAMCTR                          AOACFM
           MOVE        1 TO S-GQU01-GESQ2                               AOACFM
           PERFORM     F94C5 THRU F94C5-FN
           MOVE        GQ01-XMISL TO GS51.                              AOACFM
      *N91LT.    NOTE *IF TRIGGER TYPE = SET-UP           *.            AOACFM
       F91LT.    IF    7-AOACFM-CRETP = 'S'                             lv20
                 AND   GS51-CRETP = 'C'                                 AOACFM
                 NEXT SENTENCE ELSE GO TO     F91LT-FN.                 AOACFM
      *AND PREVIOUS TRIGGER = "CHANGE"                                  AOACFM
           PERFORM     F91LZ THRU F91LZ-FN                              AOACFM
           MOVE        7-AOACFM-CRETP TO GS51-CRETP                     AOACFM
           MOVE        GS51 TO GQ01-XMISL                               AOACFM
           PERFORM     F94C6 THRU F94C6-FN
               GO TO     F91LM-FN.                                      AOACFM
       F91LT-FN. EXIT.
      *N91LV.    NOTE *IF TRIGGER TYPE = REQUEST AND      *.            AOACFM
       F91LV.    IF    7-AOACFM-CRETP = 'R'                             lv20
                 AND   GS51-CRETP NOT = 'R'                             AOACFM
                 NEXT SENTENCE ELSE GO TO     F91LV-FN.                 AOACFM
      *PREVIOUS TRIGGER NOT "REQUEST"                                   AOACFM
           PERFORM     F91LZ THRU F91LZ-FN                              AOACFM
           MOVE        7-AOACFM-CRETP TO GS51-CRETP                     AOACFM
           MOVE        GS51 TO GQ01-XMISL                               AOACFM
           PERFORM     F94C6 THRU F94C6-FN
               GO TO     F91LM-FN.                                      AOACFM
       F91LV-FN. EXIT.
       F91LQ-FN. EXIT.
       F91LM-FN. EXIT.
      *N91LZ.    NOTE *FILL IN COMMON MISC TRAN FIELDS    *.            AOACFM
       F91LZ.                                                           lv10
           MOVE        00051 TO TA75-CAMCTR                             AOACFM
           PERFORM     F94TA THRU F94TA-FN
           MOVE        TA75-NDLEN TO GQ01-GELL                          AOACFM
           MOVE        'C' TO GQ01-CENTT                                AOACFM
           MOVE        7-AOACFM-CACKD TO GQ01-CACKD                     AOACFM
           MOVE        7-AOACFM-DCACG TO GQ01-CADATE                    AOACFM
           MOVE        EIBTIME TO GQ01-GETIM                            AOACFM
           MOVE        DS80-GEOPD2 TO GQ01-GEOPID
           MOVE        DS80-CAUNIT TO GQ01-CAUNIT
           MOVE        EIBTRMID TO GQ01-XTERMI
           MOVE        DS80-PROGR TO GQ01-CAPPL
           MOVE        'CATS' TO GQ01-CSYS.                             AOACFM
       F91LZ-FN. EXIT.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *INPUT PARM ERROR                   *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92ZA.    NOTE *INPUT PARM MISSING OR INVALID      *.
       F92ZA.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012309 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F92ZA-FN. EXIT.
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
      *N94C1.    NOTE *CALL GU ON CX13                    *.            ADU026
       F94C1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XA06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CXU06-SSA                          ADU026
           S-CXU13-SSA
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN                              ADU026
           MOVE        IK TO CX13-IK.
       F94C1-FN. EXIT.
      *N94C2.    NOTE *CALL GU ON CX14                    *.            ADU026
       F94C2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX14' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XA06 CX14                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CXU06-SSA                          ADU026
           S-CXU13-SSA S-CXU14-SSA
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN                              ADU026
           MOVE        IK TO CX14-IK.
       F94C2-FN. EXIT.
      *N94C3.    NOTE *CALL GHU ON CT01                   *.            ADU026
       F94C3.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           XB06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        XB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN                              ADU026
           MOVE        IK TO CT01-IK.
       F94C3-FN. EXIT.
      *N94C4.    NOTE *CALL REPL ON CT01                  *.            ADU026
       F94C4.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           XB06 CT01                                                    ADU026
           MOVE        XB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN                              ADU026
           MOVE        IK TO CT01-IK.
       F94C4-FN. EXIT.
      *N94C5.    NOTE *CALL GHU ON GQ01                   *.            ADU026
       F94C5.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           XC06 GQ01                                                    ADU026
           S-GQU01-SSA                                                  ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN                              ADU026
           MOVE        IK TO GQ01-IK.
       F94C5-FN. EXIT.
      *N94C6.    NOTE *CALL REPL ON GQ01                  *.            ADU026
       F94C6.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           XC06 GQ01                                                    ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN                              ADU026
           MOVE        IK TO GQ01-IK.
       F94C6-FN. EXIT.
      *N94C7.    NOTE *CALL ISRT ON GQ01                  *.            ADU026
       F94C7.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XC06 GQ01                                                    ADU026
           S-GQ01-SSA                                                   ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN                              ADU026
           MOVE        IK TO GQ01-IK.
       F94C7-FN. EXIT.
      *N94TA.    NOTE *RANDOM TABLE READ FOR TA75         *.            ADUTAB
       F94TA.                                                           lv10
           MOVE        'R1' TO G-TA75-TABFO                             ADUTAB
           COMPUTE     G-TA75-LTH = 60 + G-TA75-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA75-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA75)                                ADUTAB
                       LENGTH (G-TA75-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA75-TABCR NOT = '00'                          DOT
           PERFORM     F94TB THRU F94TB-FN.                             ADUTAB
       F94TA-FN. EXIT.
      *N94TB.    NOTE *ERR: MISC TRAN NOT ON TABLE TA75   *.
       F94TB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012207 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F94TB-FN. EXIT.
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
