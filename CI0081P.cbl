       IDENTIFICATION DIVISION.                                         CI0081
       PROGRAM-ID.  CI0081P.                                            CI0081
      *AUTHOR.         M\M - CATS SELECT ARR CLIENT.                    CI0081
      *DATE-COMPILED.   09/08/14.                                       CI0081
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 1998                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE CAMS   SYSTEM AND ALL INFORMATION RELATING THERETO,    *ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE CAMS   SYSTEM AND ALL            *ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE CAMS         *ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 1998                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0081
       CONFIGURATION SECTION.                                           CI0081
       SOURCE-COMPUTER. IBM-370.                                        CI0081
       OBJECT-COMPUTER. IBM-370.                                        CI0081
       DATA DIVISION.                                                   CI0081
       WORKING-STORAGE SECTION.                                         CI0081
       01                 CX01.                                         CI0081
            10            CX01-CX01K.                                   CI0081
            11            CX01-C199.                                    CI0081
            12            CX01-CLID.                                    CI0081
            13            CX01-CLIDO  PICTURE  9(3).                    CI0081
            13            CX01-CLIDN.                                   CI0081
            14            CX01-CLIDNP PICTURE  X(12).                   CI0081
            14            CX01-CLIDND PICTURE  9(8).                    CI0081
            10            CX01-GEMDA  PICTURE  9(8).                    CI0081
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0081
                          BINARY.                                       CI0081
            10            CX01-FILLER PICTURE  X(5).                    CI0081
       01                 CX03.                                         CI0081
            10            CX03-GELL   PICTURE  9(4)                     CI0081
                          BINARY.                                       CI0081
            10            CX03-CY00.                                    CI0081
            11            CX03-CX03K.                                   CI0081
            12            CX03-CARTY  PICTURE  99.                      CI0081
            12            CX03-NARRS  PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            11            CX03-CARST  PICTURE  99.                      CI0081
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            11            CX03-CPMTG  PICTURE  99.                      CI0081
            11            CX03-GRCRNG PICTURE  9(3).                    CI0081
            11            CX03-DEXDT  PICTURE  9(8).                    CI0081
            11            CX03-DASUP  PICTURE  9(8).                    CI0081
            11            CX03-CSTEC  PICTURE  X(3).                    CI0081
            11            CX03-FILLER PICTURE  X(17).                   CI0081
            11            CX03-CY50.                                    CI0081
            12            CX03-NARID  PICTURE  X(30).                   CI0081
            11            CX03-CY51                                     CI0081
                          REDEFINES            CX03-CY50.               CI0081
            12            CX03-NDIDN  PICTURE  9(12).                   CI0081
            12            CX03-FILLER PICTURE  X(18).                   CI0081
            11            CX03-CY52                                     CI0081
                          REDEFINES            CX03-CY50.               CI0081
            12            CX03-NAIDC  PICTURE  9(12).                   CI0081
            12            CX03-FILLER PICTURE  X(18).                   CI0081
            11            CX03-CY53                                     CI0081
                          REDEFINES            CX03-CY50.               CI0081
            12            CX03-NAMEXB PICTURE  9(15).                   CI0081
            12            CX03-FILLER PICTURE  X(15).                   CI0081
            10            CX03-CY99.                                    CI0081
            11            CX03-FILLER PICTURE  X(109).                  CI0081
            10            CX03-CY01                                     CI0081
                          REDEFINES            CX03-CY99.               CI0081
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            11            CX03-ICPCI  PICTURE  X.                       CI0081
            11            CX03-CLUPD  PICTURE  9(3).                    CI0081
            11            CX03-DLAUP  PICTURE  9(8).                    CI0081
            11            CX03-CWRC   PICTURE  99.                      CI0081
            11            CX03-CHCR   PICTURE  99.                      CI0081
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0081
            11            CX03-GEAUN  PICTURE  9(5).                    CI0081
            11            CX03-DPCHD  PICTURE  9(8).                    CI0081
            11            CX03-DLRCHK PICTURE  9(8).                    CI0081
            11            CX03-QTRCHK PICTURE  9(2).                    CI0081
            11            CX03-DNPMT  PICTURE  9(8).                    CI0081
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0081
                          COMPUTATIONAL-3.                              CI0081
            10            CX03-CY02                                     CI0081
                          REDEFINES            CX03-CY99.               CI0081
            11            CX03-QSIRQ  PICTURE  99.                      CI0081
            11            CX03-QDRMN  PICTURE  9(2)                     CI0081
                          COMPUTATIONAL-3.                              CI0081
            11            CX03-DDPRE  PICTURE  9(8).                    CI0081
            11            CX03-DDSHP  PICTURE  9(8).                    CI0081
            11            CX03-NDRFTB PICTURE  9(5).                    CI0081
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0081
            11            CX03-DDSHPA PICTURE  9(8).                    CI0081
            11            CX03-NDRFTF PICTURE  9(5).                    CI0081
            11            CX03-QDIPBK PICTURE  9(3).                    CI0081
            11            CX03-CREOR  PICTURE  X(1).                    CI0081
            11            CX03-CREOR1 PICTURE  X(1).                    CI0081
            11            CX03-DDASC  PICTURE  9(8).                    CI0081
            11            CX03-FILLER PICTURE  X(7).                    CI0081
            10            CX03-CY03                                     CI0081
                          REDEFINES            CX03-CY99.               CI0081
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0081
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0081
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0081
            11            CX03-DOPDA  PICTURE  99.                      CI0081
            11            CX03-CPMTF  PICTURE  99.                      CI0081
            11            CX03-CIRMO  PICTURE  X(12).                   CI0081
            11            CX03-CPALL  PICTURE  X(1).                    CI0081
            11            CX03-CCOLM  PICTURE  9(2).                    CI0081
            11            CX03-CBLTP  PICTURE  X(1).                    CI0081
            11            CX03-CASUB  PICTURE  9(2).                    CI0081
            11            CX03-CBLFM  PICTURE  9(2).                    CI0081
            11            CX03-IBILS  PICTURE  X.                       CI0081
            11            CX03-IPAOS  PICTURE  X.                       CI0081
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0081
            11            CX03-DLBPD  PICTURE  9(8).                    CI0081
            11            CX03-DNBPD  PICTURE  9(8).                    CI0081
            11            CX03-DODBD  PICTURE  9(8).                    CI0081
            11            CX03-CPSRE  PICTURE  99.                      CI0081
            11            CX03-ISPHN  PICTURE  X.                       CI0081
            11            CX03-TCARR  PICTURE  X(6).                    CI0081
            11            CX03-CBKPT  PICTURE  9(2).                    CI0081
            11            CX03-IECNT  PICTURE  X.                       CI0081
            11            CX03-ICONV  PICTURE  X(1).                    CI0081
            11            CX03-FILLER PICTURE  X(4).                    CI0081
            10            CX03-CY04                                     CI0081
                          REDEFINES            CX03-CY99.               CI0081
            11            CX03-CCARD  PICTURE  X(02).                   CI0081
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0081
            11            CX03-IREMT  PICTURE  X(01).                   CI0081
            11            CX03-ISBILA PICTURE  X.                       CI0081
            11            CX03-DLBPDA PICTURE  9(8).                    CI0081
            11            CX03-DNBPDA.                                  CI0081
            12            CX03-DNCYM  PICTURE  9(6).                    CI0081
            12            CX03-CEDTD  PICTURE  9(2).                    CI0081
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0081
                          COMPUTATIONAL-3.                              CI0081
            11            CX03-DREMT  PICTURE  9(8).                    CI0081
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0081
                          COMPUTATIONAL-3.                              CI0081
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0081
            11            CX03-CWRC2  PICTURE  99.                      CI0081
            11            CX03-CHCR2  PICTURE  99.                      CI0081
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0081
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0081
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0081
       01                 CX2Y.                                         CI0081
            10            CX2Y-CX2YK.                                   CI0081
            11            CX2Y-C299.                                    CI0081
            12            CX2Y-CTID.                                    CI0081
            13            CX2Y-CTIDA  PICTURE  9(3).                    CI0081
            13            CX2Y-CTIDN.                                   CI0081
            14            CX2Y-CTIDNP PICTURE  X(13).                   CI0081
            14            CX2Y-CTIDND PICTURE  9(11).                   CI0081
            11            CX2Y-C199.                                    CI0081
            12            CX2Y-CLID.                                    CI0081
            13            CX2Y-CLIDO  PICTURE  9(3).                    CI0081
            13            CX2Y-CLIDN.                                   CI0081
            14            CX2Y-CLIDNP PICTURE  X(12).                   CI0081
            14            CX2Y-CLIDND PICTURE  9(8).                    CI0081
            11            CX2Y-CARTY  PICTURE  99.                      CI0081
            11            CX2Y-NARRS  PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
       01                 CL01.                                         CI0081
            10            CL01-CL01K.                                   CI0081
            11            CL01-C199.                                    CI0081
            12            CL01-CLID.                                    CI0081
            13            CL01-CLIDO  PICTURE  9(3).                    CI0081
            13            CL01-CLIDN.                                   CI0081
            14            CL01-CLIDNP PICTURE  X(12).                   CI0081
            14            CL01-CLIDND PICTURE  9(8).                    CI0081
            10            CL01-GECKD  PICTURE  9.                       CI0081
            10            CL01-GEMDA  PICTURE  9(8).                    CI0081
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0081
                          BINARY.                                       CI0081
            10            CL01-GECUC  PICTURE  99.                      CI0081
            10            CL01-CLDOR  PICTURE  9(8).                    CI0081
            10            CL01-CLLNG  PICTURE  XX.                      CI0081
            10            CL01-GESLC  PICTURE  99.                      CI0081
            10            CL01-CLTYP  PICTURE  X.                       CI0081
            10            CL01-CLCLS  PICTURE  9(3).                    CI0081
            10            CL01-CLTWRC PICTURE  99.                      CI0081
            10            CL01-CLPVC  PICTURE  99.                      CI0081
            10            CL01-CLIND  PICTURE  9(3).                    CI0081
            10            CL01-CLTRC  PICTURE  99.                      CI0081
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0081
                          COMPUTATIONAL-3.                              CI0081
            10            CL01-AYSIDA PICTURE  9(3).                    CI0081
            10            CL01-AYSID  PICTURE  9(5).                    CI0081
            10            CL01-CLSTR  PICTURE  9(2).                    CI0081
            10            CL01-CLC11  PICTURE  X.                       CI0081
            10            CL01-CLTIN  PICTURE  9(12).                   CI0081
            10            CL01-CLTND  PICTURE  9(8).                    CI0081
            10            CL01-CLTINC PICTURE  9.                       CI0081
            10            CL01-CCDWA  PICTURE  9.                       CI0081
            10            CL01-CICES  PICTURE  X.                       CI0081
            10            CL01-CLTRA  PICTURE  9(2).                    CI0081
            10            CL01-DIRSY  PICTURE  9(4)                     CI0081
                          COMPUTATIONAL-3.                              CI0081
            10            CL01-CFEDS  PICTURE  X.                       CI0081
            10            CL01-FILLER PICTURE  X(06).                   CI0081
       01                 CL24.                                         CI0081
            10            CL24-GELL   PICTURE  9(4)                     CI0081
                          BINARY.                                       CI0081
            10            CL24-CL24K.                                   CI0081
            11            CL24-GECSQ  PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            10            CL24-GECSD  PICTURE  9(8).                    CI0081
            10            CL24-GECED  PICTURE  9(8).                    CI0081
            10            CL24-CREQ2  PICTURE  X.                       CI0081
            10            CL24-FILLER PICTURE  X(4).                    CI0081
            10            CL24-GECTA  PICTURE  X.                       CI0081
            10            CL24-GELCD  PICTURE  9(8).                    CI0081
            10            CL24-GEADS  PICTURE  9.                       CI0081
            10            CL24-GECIT  PICTURE  X(25).                   CI0081
            10            CL24-GECTRY PICTURE  X(20).                   CI0081
            10            CL24-GECTY  PICTURE  9(3).                    CI0081
            10            CL24-GEPCD  PICTURE  X(12).                   CI0081
            10            CL24-GEST   PICTURE  X(8).                    CI0081
            10            CL24-IRESA  PICTURE  X.                       CI0081
            10            CL24-FILLER PICTURE  X(8).                    CI0081
            10            CL24-GESAD  PICTURE  X(30)                    CI0081
                          OCCURS       003     TIMES.                   CI0081
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01                 CT01.                                         CI0081
            10            CT01-CT01K.                                   CI0081
            11            CT01-C299.                                    CI0081
            12            CT01-CTID.                                    CI0081
            13            CT01-CTIDA  PICTURE  9(3).                    CI0081
            13            CT01-CTIDN.                                   CI0081
            14            CT01-CTIDNP PICTURE  X(13).                   CI0081
            14            CT01-CTIDND PICTURE  9(11).                   CI0081
            10            CT01-GECKD  PICTURE  9.                       CI0081
            10            CT01-GEMDA  PICTURE  9(8).                    CI0081
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0081
                          BINARY.                                       CI0081
            10            CT01-GECUC  PICTURE  99.                      CI0081
            10            CT01-CTAUL  PICTURE  9(3).                    CI0081
            10            CT01-DIRAC  PICTURE  9(4).                    CI0081
            10            CT01-CTCCI  PICTURE  X.                       CI0081
            10            CT01-CTCUS  PICTURE  999.                     CI0081
            10            CT01-CTEFD  PICTURE  9(8).                    CI0081
            10            CT01-CTIAD  PICTURE  9(8).                    CI0081
            10            CT01-CLCUS  PICTURE  99.                      CI0081
            10            CT01-CAMMB  PICTURE  X(3).                    CI0081
            10            CT01-CKPMM  PICTURE  X.                       CI0081
            10            CT01-CTLAD  PICTURE  9(8).                    CI0081
            10            CT01-IPERS  PICTURE  X.                       CI0081
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0081
                          COMPUTATIONAL-3.                              CI0081
            10            CT01-CTLAT  PICTURE  9(8).                    CI0081
            10            CT01-CTLATC PICTURE  9(6).                    CI0081
            10            CT01-IMEGA  PICTURE  X.                       CI0081
            10            CT01-DIRAB  PICTURE  9(8).                    CI0081
            10            CT01-COLRQ  PICTURE  X.                       CI0081
            10            CT01-ZDA04  PICTURE  X(4).                    CI0081
            10            CT01-CTLPD  PICTURE  9(8).                    CI0081
            10            CT01-CIRASP PICTURE  9.                       CI0081
            10            CT01-CIRATP PICTURE  99.                      CI0081
            10            CT01-DRTHC  PICTURE  9(8).                    CI0081
            10            CT01-CPPTC  PICTURE  X.                       CI0081
            10            CT01-ZDA06  PICTURE  X(6).                    CI0081
            10            CT01-CTACD  PICTURE  9(8).                    CI0081
            10            CT01-CTNLI  PICTURE  X.                       CI0081
            10            CT01-CTRHO  PICTURE  9(8).                    CI0081
            10            CT01-CTSGD  PICTURE  9(8).                    CI0081
            10            CT01-CPATP  PICTURE  X(1).                    CI0081
            10            CT01-IRSTA  PICTURE  X.                       CI0081
            10            CT01-CTSTA  PICTURE  99.                      CI0081
            10            CT01-CTSSC  PICTURE  99.                      CI0081
            10            CT01-PRLIN  PICTURE  9(3).                    CI0081
            10            CT01-PRCOD  PICTURE  9(5).                    CI0081
            10            CT01-PRSCD  PICTURE  X(9).                    CI0081
            10            CT01-CTLNI  PICTURE  X.                       CI0081
            10            CT01-AYSIDA PICTURE  9(3).                    CI0081
            10            CT01-AYSID  PICTURE  9(5).                    CI0081
            10            CT01-CTBMC  PICTURE  99.                      CI0081
            10            CT01-CINAR  PICTURE  99.                      CI0081
            10            CT01-CPHTR  PICTURE  X.                       CI0081
            10            CT01-CDSTR  PICTURE  XX.                      CI0081
            10            CT01-CQACT  PICTURE  999.                     CI0081
            10            CT01-CIRAS  PICTURE  999.                     CI0081
            10            CT01-CIRAT  PICTURE  999.                     CI0081
            10            CT01-CLRAY  PICTURE  9(5).                    CI0081
            10            CT01-CATTP  PICTURE  X.                       CI0081
       01                 CT07.                                         CI0081
            10            CT07-CT07K.                                   CI0081
            11            CT07-C199.                                    CI0081
            12            CT07-CLID.                                    CI0081
            13            CT07-CLIDO  PICTURE  9(3).                    CI0081
            13            CT07-CLIDN.                                   CI0081
            14            CT07-CLIDNP PICTURE  X(12).                   CI0081
            14            CT07-CLIDND PICTURE  9(8).                    CI0081
       01                 CT09.                                         CI0081
            10            CT09-A100.                                    CI0081
            11            CT09-GELL   PICTURE  9(4)                     CI0081
                          BINARY.                                       CI0081
            11            CT09-CT09K.                                   CI0081
            12            CT09-CLCTRC PICTURE  9(3).                    CI0081
            11            CT09-GERSD  PICTURE  9(8).                    CI0081
            11            CT09-GERED  PICTURE  9(8).                    CI0081
            10            CT09-A199.                                    CI0081
            11            CT09-FILLER PICTURE  X(20).                   CI0081
            10            CT09-A101                                     CI0081
                          REDEFINES            CT09-A199.               CI0081
            11            CT09-GECSQ  PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            11            CT09-CTAXR  PICTURE  X.                       CI0081
            11            CT09-GETAI  PICTURE  X.                       CI0081
            11            CT09-CTLACD PICTURE  9(8).                    CI0081
            11            CT09-GEPCS  PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            10            CT09-A102                                     CI0081
                          REDEFINES            CT09-A199.               CI0081
            11            CT09-CLPID  PICTURE  9(9).                    CI0081
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0081
            10            XW05-XW06.                                    CI0081
            11            XW05-XDBPCB.                                  CI0081
            12            XW05-XDBDNM PICTURE  X(08)                    CI0081
                          VALUE                SPACE.                   CI0081
            12            XW05-XSEGLV PICTURE  X(02)                    CI0081
                          VALUE                SPACE.                   CI0081
            12            XW05-XRC    PICTURE  X(02)                    CI0081
                          VALUE                SPACE.                   CI0081
            12            XW05-XPROPT PICTURE  X(04)                    CI0081
                          VALUE                SPACE.                   CI0081
            12            XW05-FILLER PICTURE  S9(5)                    CI0081
                          VALUE                ZERO                     CI0081
                          BINARY.                                       CI0081
            12            XW05-XSEGNM PICTURE  X(08)                    CI0081
                          VALUE                SPACE.                   CI0081
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0081
                          VALUE                ZERO                     CI0081
                          BINARY.                                       CI0081
            12            XW05-XSEGNB PICTURE  9(05)                    CI0081
                          VALUE                ZERO                     CI0081
                          BINARY.                                       CI0081
            12            XW05-XCOKEY PICTURE  X(70)                    CI0081
                          VALUE                SPACE.                   CI0081
            10            XW05-XW07.                                    CI0081
            11            XW05-XIOPCB.                                  CI0081
            12            XW05-XTERMI PICTURE  X(08)                    CI0081
                          VALUE                SPACE.                   CI0081
            12            XW05-FILLER PICTURE  XX                       CI0081
                          VALUE                SPACE.                   CI0081
            12            XW05-XRC1   PICTURE  X(02)                    CI0081
                          VALUE                SPACE.                   CI0081
            12            XW05-FILLER PICTURE  X(12)                    CI0081
                          VALUE                SPACE.                   CI0081
            12            XW05-XMODNM PICTURE  X(8)                     CI0081
                          VALUE                SPACE.                   CI0081
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0081
                          VALUE                ZERO.                    CI0081
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0081
                          VALUE                ZERO.                    CI0081
            10            XW05-XGU    PICTURE  X(4)                     CI0081
                          VALUE                'GU  '.                  CI0081
            10            XW05-XGHU   PICTURE  X(4)                     CI0081
                          VALUE                'GHU '.                  CI0081
            10            XW05-XGN    PICTURE  X(4)                     CI0081
                          VALUE                'GN  '.                  CI0081
            10            XW05-XGHN   PICTURE  X(4)                     CI0081
                          VALUE                'GHN '.                  CI0081
            10            XW05-XGNP   PICTURE  X(4)                     CI0081
                          VALUE                'GNP '.                  CI0081
            10            XW05-XGHNP  PICTURE  X(4)                     CI0081
                          VALUE                'GHNP'.                  CI0081
            10            XW05-XREPL  PICTURE  XXXX                     CI0081
                          VALUE                'REPL'.                  CI0081
            10            XW05-XISRT  PICTURE  X(4)                     CI0081
                          VALUE                'ISRT'.                  CI0081
            10            XW05-XDLET  PICTURE  X(4)                     CI0081
                          VALUE                'DLET'.                  CI0081
            10            XW05-XOPEN  PICTURE  X(4)                     CI0081
                          VALUE                'OPEN'.                  CI0081
            10            XW05-XCLSE  PICTURE  X(4)                     CI0081
                          VALUE                'CLSE'.                  CI0081
            10            XW05-XCHKP  PICTURE  X(4)                     CI0081
                          VALUE                'CHKP'.                  CI0081
            10            XW05-XXRST  PICTURE  X(4)                     CI0081
                          VALUE                'XRST'.                  CI0081
            10            XW05-XTERM  PICTURE  X(4)                     CI0081
                          VALUE                'TERM'.                  CI0081
            10            XW05-XNFPAC PICTURE  X(13)                    CI0081
                          VALUE                SPACE.                   CI0081
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0081
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0081
       01 7-MISC-FIELDS.
          05  CX2Y-CF                 PIC X(01) VALUE '0'.
          05  CT07-CF                 PIC X(01) VALUE '0'.
          05  7-ARR-CLIENT-FOUND      PIC X(01) VALUE 'N'.
      *!WI
          05  7-SEL1-CLID
                        PICTURE X(23).                                  CI0081
      *!WI
          05  7-SEL2-CLID
                        PICTURE X(23).                                  CI0081
      *!WI
          05  7-SEL3-CLID
                        PICTURE X(23).                                  CI0081
      *!WI
          05  7-SEL4-CLID
                        PICTURE X(23).                                  CI0081
      *!WI
          05  7-SEL5-CLID
                        PICTURE X(23).                                  CI0081
      *!WI
          05  7-SEL6-CLID
                        PICTURE X(23).                                  CI0081
      *!WI
          05  7-SEL1-GECSQ
                        PICTURE S9(3)                                   CI0081
                          COMPUTATIONAL-3.                              CI0081
      *!WI
          05  7-SEL2-GECSQ
                        PICTURE S9(3)                                   CI0081
                          COMPUTATIONAL-3.                              CI0081
      *!WI
          05  7-SEL3-GECSQ
                        PICTURE S9(3)                                   CI0081
                          COMPUTATIONAL-3.                              CI0081
      *!WI
          05  7-SEL4-GECSQ
                        PICTURE S9(3)                                   CI0081
                          COMPUTATIONAL-3.                              CI0081
      *!WI
          05  7-SEL5-GECSQ
                        PICTURE S9(3)                                   CI0081
                          COMPUTATIONAL-3.                              CI0081
      *!WI
          05  7-SEL6-GECSQ
                        PICTURE S9(3)                                   CI0081
                          COMPUTATIONAL-3.                              CI0081
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
       01   DEBUT-WSS.                                                  CI0081
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0081
            05   IK     PICTURE X.                                      CI0081
       01  CONSTANTES-PAC.                                              CI0081
           05  FILLER  PICTURE X(87)   VALUE                            CI0081
                     '6015 CAT09/08/14CI0081ADMIN   14:34:41CI0081P AMERCI0081
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0081
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0081
           05  NUGNA   PICTURE X(5).                                    CI0081
           05  APPLI   PICTURE X(3).                                    CI0081
           05  DATGN   PICTURE X(8).                                    CI0081
           05  PROGR   PICTURE X(6).                                    CI0081
           05  CODUTI  PICTURE X(8).                                    CI0081
           05  TIMGN   PICTURE X(8).                                    CI0081
           05  PROGE   PICTURE X(8).                                    CI0081
           05  COBASE  PICTURE X(4).                                    CI0081
           05  DATGNC  PICTURE X(10).                                   CI0081
           05  RELEAS  PICTURE X(7).                                    CI0081
           05  DATGE   PICTURE X(10).                                   CI0081
           05  DATSQ   PICTURE X(10).                                   CI0081
       01  DATCE.                                                       CI0081
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0081
         05  DATOR.                                                     CI0081
           10  DATOA  PICTURE XX.                                       CI0081
           10  DATOM  PICTURE XX.                                       CI0081
           10  DATOJ  PICTURE XX.                                       CI0081
       01   VARIABLES-CONDITIONNELLES.                                  CI0081
            05                  FT      PICTURE X VALUE '0'.            CI0081
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0081
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0081
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0081
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0081
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0081
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0081
       01               S-CL01-SSA.                                     CI0081
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0081
                                      VALUE 'CL01    '.                 CI0081
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0081
            10          S-CL01-CCOD   PICTURE X(5)                      CI0081
                                      VALUE '-----'.                    CI0081
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0081
       01            S-CLU01-SSA.                                       CI0081
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CL01    '.                 CI0081
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0081
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(CL01K'.                   CI0081
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0081
            10       S-CLU01-CL01K.                                     CI0081
            11       S-CLU01-C199.                                      CI0081
            12       S-CLU01-CLID.                                      CI0081
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0081
            13       S-CLU01-CLIDN.                                     CI0081
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0081
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0081
            10  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01               S-CL24-SSA.                                     CI0081
            10         S1-CL24-SEGNAM PICTURE X(8)                      CI0081
                                      VALUE 'CL24    '.                 CI0081
            10         S1-CL24-CCOM   PICTURE X VALUE '*'.              CI0081
            10          S-CL24-CCOD   PICTURE X(5)                      CI0081
                                      VALUE '-----'.                    CI0081
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0081
       01            S-CLA24-SSA.                                       CI0081
            10      S1-CLA24-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CL24    '.                 CI0081
            10      S1-CLA24-CCOM   PICTURE X VALUE '*'.                CI0081
            10       S-CLA24-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            10      S1-CLA24-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(GECED'.                   CI0081
            10       S-CLA24-OPER  PICTURE XX VALUE ' ='.               CI0081
            10       S-CLA24-GECED    PICTURE  9(8).                    CI0081
            10  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CLB24-SSA.                                       CI0081
            10      S1-CLB24-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CL24    '.                 CI0081
            10      S1-CLB24-CCOM   PICTURE X VALUE '*'.                CI0081
            10       S-CLB24-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            10      S1-CLB24-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(IRESA'.                   CI0081
            10       S-CLB24-OPER  PICTURE XX VALUE ' ='.               CI0081
            10       S-CLB24-IRESA    PICTURE  X.                       CI0081
            10  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CLC24-SSA.                                       CI0081
            10      S1-CLC24-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CL24    '.                 CI0081
            10      S1-CLC24-CCOM   PICTURE X VALUE '*'.                CI0081
            10       S-CLC24-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            10      S1-CLC24-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(GEADS'.                   CI0081
            10       S-CLC24-OPER  PICTURE XX VALUE ' ='.               CI0081
            10       S-CLC24-GEADS    PICTURE  9.                       CI0081
            10  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CLD24-SSA.                                       CI0081
            10      S1-CLD24-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CL24    '.                 CI0081
            10      S1-CLD24-CCOM   PICTURE X VALUE '*'.                CI0081
            10       S-CLD24-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            10      S1-CLD24-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(GEST'.                    CI0081
            10       S-CLD24-OPER  PICTURE XX VALUE ' ='.               CI0081
            10       S-CLD24-GEST     PICTURE  X(8).                    CI0081
            10  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CLU24-SSA.                                       CI0081
            10      S1-CLU24-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CL24    '.                 CI0081
            10      S1-CLU24-CCOM   PICTURE X VALUE '*'.                CI0081
            10       S-CLU24-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            10      S1-CLU24-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(CL24K'.                   CI0081
            10       S-CLU24-OPER  PICTURE XX VALUE ' ='.               CI0081
            10       S-CLU24-CL24K.                                     CI0081
            11       S-CLU24-GECSQ    PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            10  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01               S-CT01-SSA.                                     CI0081
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0081
                                      VALUE 'CT01    '.                 CI0081
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0081
            10          S-CT01-CCOD   PICTURE X(5)                      CI0081
                                      VALUE '-----'.                    CI0081
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0081
       01            S-CTU01-SSA.                                       CI0081
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CT01    '.                 CI0081
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0081
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(CT01K'.                   CI0081
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0081
            10       S-CTU01-CT01K.                                     CI0081
            11       S-CTU01-C299.                                      CI0081
            12       S-CTU01-CTID.                                      CI0081
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0081
            13       S-CTU01-CTIDN.                                     CI0081
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0081
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0081
            10  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01               S-CT07-SSA.                                     CI0081
            10         S1-CT07-SEGNAM PICTURE X(8)                      CI0081
                                      VALUE 'CT07    '.                 CI0081
            10         S1-CT07-CCOM   PICTURE X VALUE '*'.              CI0081
            10          S-CT07-CCOD   PICTURE X(5)                      CI0081
                                      VALUE '-----'.                    CI0081
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0081
       01            S-CTU07-SSA.                                       CI0081
            10      S1-CTU07-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CT07    '.                 CI0081
            10      S1-CTU07-CCOM   PICTURE X VALUE '*'.                CI0081
            10       S-CTU07-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            10      S1-CTU07-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(CT07K'.                   CI0081
            10       S-CTU07-OPER  PICTURE XX VALUE ' ='.               CI0081
            10       S-CTU07-CT07K.                                     CI0081
            11       S-CTU07-C199.                                      CI0081
            12       S-CTU07-CLID.                                      CI0081
            13       S-CTU07-CLIDO    PICTURE  9(3).                    CI0081
            13       S-CTU07-CLIDN.                                     CI0081
            14       S-CTU07-CLIDNP   PICTURE  X(12).                   CI0081
            14       S-CTU07-CLIDND   PICTURE  9(8).                    CI0081
            10  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01               S-CT09-SSA.                                     CI0081
            10         S1-CT09-SEGNAM PICTURE X(8)                      CI0081
                                      VALUE 'CT09    '.                 CI0081
            10         S1-CT09-CCOM   PICTURE X VALUE '*'.              CI0081
            10          S-CT09-CCOD   PICTURE X(5)                      CI0081
                                      VALUE '-----'.                    CI0081
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0081
       01            S-CTA09-SSA.                                       CI0081
            11      S1-CTA09-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CT09    '.                 CI0081
            11      S1-CTA09-CCOM   PICTURE X VALUE '*'.                CI0081
            11       S-CTA09-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            11      S1-CTA09-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(GERED'.                   CI0081
            11       S-CTA09-OPER  PICTURE XX VALUE ' ='.               CI0081
            11       S-CTA09-GERED    PICTURE  9(8).                    CI0081
            11  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CTB09-SSA.                                       CI0081
            11      S1-CTB09-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CT09    '.                 CI0081
            11      S1-CTB09-CCOM   PICTURE X VALUE '*'.                CI0081
            11       S-CTB09-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            11      S1-CTB09-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(GECSQ'.                   CI0081
            11       S-CTB09-OPER  PICTURE XX VALUE ' ='.               CI0081
            11       S-CTB09-GECSQ    PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            11  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CTU09-SSA.                                       CI0081
            11      S1-CTU09-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CT09    '.                 CI0081
            11      S1-CTU09-CCOM   PICTURE X VALUE '*'.                CI0081
            11       S-CTU09-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            11      S1-CTU09-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(CT09K'.                   CI0081
            11       S-CTU09-OPER  PICTURE XX VALUE ' ='.               CI0081
            11       S-CTU09-CT09K.                                     CI0081
            12       S-CTU09-CLCTRC   PICTURE  9(3).                    CI0081
            11  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01               S-CX01-SSA.                                     CI0081
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0081
                                      VALUE 'CX01    '.                 CI0081
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0081
            10          S-CX01-CCOD   PICTURE X(5)                      CI0081
                                      VALUE '-----'.                    CI0081
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0081
       01            S-CXU01-SSA.                                       CI0081
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CX01    '.                 CI0081
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0081
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(CX01K'.                   CI0081
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0081
            10       S-CXU01-CX01K.                                     CI0081
            11       S-CXU01-C199.                                      CI0081
            12       S-CXU01-CLID.                                      CI0081
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0081
            13       S-CXU01-CLIDN.                                     CI0081
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0081
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0081
            10  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01               S-CX03-SSA.                                     CI0081
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0081
                                      VALUE 'CX03    '.                 CI0081
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0081
            10          S-CX03-CCOD   PICTURE X(5)                      CI0081
                                      VALUE '-----'.                    CI0081
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0081
       01            S-CXA03-SSA.                                       CI0081
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CX03    '.                 CI0081
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0081
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(CARTY'.                   CI0081
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0081
            12       S-CXA03-CARTY    PICTURE  99.                      CI0081
            12  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CXB03-SSA.                                       CI0081
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CX03    '.                 CI0081
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0081
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(NARRS'.                   CI0081
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0081
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            12  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CXC03-SSA.                                       CI0081
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CX03    '.                 CI0081
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0081
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(CPMTG'.                   CI0081
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0081
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0081
            11  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CXD03-SSA.                                       CI0081
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CX03    '.                 CI0081
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0081
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(GRCRNG'.                  CI0081
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0081
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0081
            11  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CXE03-SSA.                                       CI0081
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CX03    '.                 CI0081
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0081
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(DEXDT'.                   CI0081
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0081
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0081
            11  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CXF03-SSA.                                       CI0081
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CX03    '.                 CI0081
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0081
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(CY50'.                    CI0081
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0081
            11       S-CXF03-CY50.                                      CI0081
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0081
            11  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CXG03-SSA.                                       CI0081
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CX03    '.                 CI0081
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0081
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(NBASQ'.                   CI0081
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0081
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            11  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CXH03-SSA.                                       CI0081
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CX03    '.                 CI0081
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0081
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(NARID'.                   CI0081
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0081
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0081
            12  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CXU03-SSA.                                       CI0081
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CX03    '.                 CI0081
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0081
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(CX03K'.                   CI0081
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0081
            11       S-CXU03-CX03K.                                     CI0081
            12       S-CXU03-CARTY    PICTURE  99.                      CI0081
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            11  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01               S-CX2Y-SSA.                                     CI0081
            10         S1-CX2Y-SEGNAM PICTURE X(8)                      CI0081
                                      VALUE 'CX2Y    '.                 CI0081
            10         S1-CX2Y-CCOM   PICTURE X VALUE '*'.              CI0081
            10          S-CX2Y-CCOD   PICTURE X(5)                      CI0081
                                      VALUE '-----'.                    CI0081
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0081
       01            S-CXA2Y-SSA.                                       CI0081
            11      S1-CXA2Y-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CX2Y    '.                 CI0081
            11      S1-CXA2Y-CCOM   PICTURE X VALUE '*'.                CI0081
            11       S-CXA2Y-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            11      S1-CXA2Y-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(CARTY'.                   CI0081
            11       S-CXA2Y-OPER  PICTURE XX VALUE ' ='.               CI0081
            11       S-CXA2Y-CARTY    PICTURE  99.                      CI0081
            11  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CXB2Y-SSA.                                       CI0081
            11      S1-CXB2Y-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CX2Y    '.                 CI0081
            11      S1-CXB2Y-CCOM   PICTURE X VALUE '*'.                CI0081
            11       S-CXB2Y-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            11      S1-CXB2Y-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(C299'.                    CI0081
            11       S-CXB2Y-OPER  PICTURE XX VALUE ' ='.               CI0081
            11       S-CXB2Y-C299.                                      CI0081
            12       S-CXB2Y-CTID.                                      CI0081
            13       S-CXB2Y-CTIDA    PICTURE  9(3).                    CI0081
            13       S-CXB2Y-CTIDN.                                     CI0081
            14       S-CXB2Y-CTIDNP   PICTURE  X(13).                   CI0081
            14       S-CXB2Y-CTIDND   PICTURE  9(11).                   CI0081
            11  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01            S-CXU2Y-SSA.                                       CI0081
            10      S1-CXU2Y-SEGNAM PICTURE X(8)                        CI0081
                                      VALUE 'CX2Y    '.                 CI0081
            10      S1-CXU2Y-CCOM   PICTURE X VALUE '*'.                CI0081
            10       S-CXU2Y-CCOD   PICTURE X(5)                        CI0081
                                      VALUE '-----'.                    CI0081
            10      S1-CXU2Y-FLDNAM PICTURE X(9)                        CI0081
                                      VALUE '(CX2YK'.                   CI0081
            10       S-CXU2Y-OPER  PICTURE XX VALUE ' ='.               CI0081
            10       S-CXU2Y-CX2YK.                                     CI0081
            11       S-CXU2Y-C299.                                      CI0081
            12       S-CXU2Y-CTID.                                      CI0081
            13       S-CXU2Y-CTIDA    PICTURE  9(3).                    CI0081
            13       S-CXU2Y-CTIDN.                                     CI0081
            14       S-CXU2Y-CTIDNP   PICTURE  X(13).                   CI0081
            14       S-CXU2Y-CTIDND   PICTURE  9(11).                   CI0081
            11       S-CXU2Y-C199.                                      CI0081
            12       S-CXU2Y-CLID.                                      CI0081
            13       S-CXU2Y-CLIDO    PICTURE  9(3).                    CI0081
            13       S-CXU2Y-CLIDN.                                     CI0081
            14       S-CXU2Y-CLIDNP   PICTURE  X(12).                   CI0081
            14       S-CXU2Y-CLIDND   PICTURE  9(8).                    CI0081
            11       S-CXU2Y-CARTY    PICTURE  99.                      CI0081
            11       S-CXU2Y-NARRS    PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            10  FILLER   PICTURE X    VALUE ')'.                        CI0081
       01   ZONES-UTILISATEUR PICTURE X.                                CI0081
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
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR ARAY                                           ADU015
            05 PCB-ARAY-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0081
          05              PA00-SUITE.                                   CI0081
            15       FILLER         PICTURE  X(00106).                  CI0081
       01                 PA06  REDEFINES      PA00.                    CI0081
            10            PA06-XDBPCB.                                  CI0081
            11            PA06-XDBDNM PICTURE  X(08).                   CI0081
            11            PA06-XSEGLV PICTURE  X(02).                   CI0081
            11            PA06-XRC    PICTURE  X(02).                   CI0081
            11            PA06-XPROPT PICTURE  X(04).                   CI0081
            11            PA06-FILLER PICTURE  S9(5)                    CI0081
                          BINARY.                                       CI0081
            11            PA06-XSEGNM PICTURE  X(08).                   CI0081
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0081
                          BINARY.                                       CI0081
            11            PA06-XSEGNB PICTURE  9(05)                    CI0081
                          BINARY.                                       CI0081
            11            PA06-XCOKEY PICTURE  X(70).                   CI0081
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0081
          05              PB00-SUITE.                                   CI0081
            15       FILLER         PICTURE  X(00106).                  CI0081
       01                 PB06  REDEFINES      PB00.                    CI0081
            10            PB06-XDBPCB.                                  CI0081
            11            PB06-XDBDNM PICTURE  X(08).                   CI0081
            11            PB06-XSEGLV PICTURE  X(02).                   CI0081
            11            PB06-XRC    PICTURE  X(02).                   CI0081
            11            PB06-XPROPT PICTURE  X(04).                   CI0081
            11            PB06-FILLER PICTURE  S9(5)                    CI0081
                          BINARY.                                       CI0081
            11            PB06-XSEGNM PICTURE  X(08).                   CI0081
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0081
                          BINARY.                                       CI0081
            11            PB06-XSEGNB PICTURE  9(05)                    CI0081
                          BINARY.                                       CI0081
            11            PB06-XCOKEY PICTURE  X(70).                   CI0081
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0081
          05              PC00-SUITE.                                   CI0081
            15       FILLER         PICTURE  X(00106).                  CI0081
       01                 PC06  REDEFINES      PC00.                    CI0081
            10            PC06-XDBPCB.                                  CI0081
            11            PC06-XDBDNM PICTURE  X(08).                   CI0081
            11            PC06-XSEGLV PICTURE  X(02).                   CI0081
            11            PC06-XRC    PICTURE  X(02).                   CI0081
            11            PC06-XPROPT PICTURE  X(04).                   CI0081
            11            PC06-FILLER PICTURE  S9(5)                    CI0081
                          BINARY.                                       CI0081
            11            PC06-XSEGNM PICTURE  X(08).                   CI0081
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0081
                          BINARY.                                       CI0081
            11            PC06-XSEGNB PICTURE  9(05)                    CI0081
                          BINARY.                                       CI0081
            11            PC06-XCOKEY PICTURE  X(70).                   CI0081
      *** PCB MASK FOR ARAY                                             ADU015
      *!WF DSP=PD DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PD00.                                         CI0081
          05              PD00-SUITE.                                   CI0081
            15       FILLER         PICTURE  X(00106).                  CI0081
       01                 PD06  REDEFINES      PD00.                    CI0081
            10            PD06-XDBPCB.                                  CI0081
            11            PD06-XDBDNM PICTURE  X(08).                   CI0081
            11            PD06-XSEGLV PICTURE  X(02).                   CI0081
            11            PD06-XRC    PICTURE  X(02).                   CI0081
            11            PD06-XPROPT PICTURE  X(04).                   CI0081
            11            PD06-FILLER PICTURE  S9(5)                    CI0081
                          BINARY.                                       CI0081
            11            PD06-XSEGNM PICTURE  X(08).                   CI0081
            11            PD06-XKEYLN PICTURE  S9(05)                   CI0081
                          BINARY.                                       CI0081
            11            PD06-XSEGNB PICTURE  9(05)                    CI0081
                          BINARY.                                       CI0081
            11            PD06-XCOKEY PICTURE  X(70).                   CI0081
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED FOR CI0081    *
      ******************************************************************
      *
      *!WF DSP=WZ DSL=WZ SEL=05 FOR=I LEV=1 PLT=10
       01                 WZ00.                                         CI0081
          05              WZ00-00.                                      CI0081
            10            WZ00-DCACG  PICTURE  9(8).                    CI0081
            10            WZ00-GEAUN  PICTURE  9(5).                    CI0081
            10            WZ00-GEOPD2 PICTURE  X(8).                    CI0081
            10            WZ00-CAATY  PICTURE  9(3).                    CI0081
            10            WZ00-NBTCH  PICTURE  9(4).                    CI0081
            10            WZ00-CTIDA  PICTURE  9(3).                    CI0081
            10            WZ00-CTIDNP PICTURE  X(13).                   CI0081
            10            WZ00-CTIDND PICTURE  9(11).                   CI0081
            10            WZ00-GECKD  PICTURE  9.                       CI0081
            10            WZ00-PRCOD  PICTURE  9(5).                    CI0081
            10            WZ00-PRSCD  PICTURE  X(9).                    CI0081
            10            WZ00-ICUST  PICTURE  X.                       CI0081
            10            WZ00-NAASQ  PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            10            WZ00-CRECTU PICTURE  99.                      CI0081
          05              WZ00-SUITE.                                   CI0081
            15       FILLER         PICTURE  X(00165).                  CI0081
       01                 WZ05  REDEFINES      WZ00.                    CI0081
            10       FILLER         PICTURE  X(00075).                  CI0081
            10            WZ05-MAPPN  PICTURE  X(10).                   CI0081
            10            WZ05-CTID   PICTURE  X(27).                   CI0081
            10            WZ05-FILLER PICTURE  X(50).                   CI0081
            10            WZ05-CLID   PICTURE  X(23).                   CI0081
            10            WZ05-GECSQ  PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            10            WZ05-ICLID  PICTURE  X.                       CI0081
            10            WZ05-NARRS  PICTURE  S9(3)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            10            WZ05-FILLER PICTURE  X(50).                   CI0081
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0081
          05              DE00-SUITE.                                   CI0081
            15       FILLER         PICTURE  X(00653).                  CI0081
       01                 DE10  REDEFINES      DE00.                    CI0081
            10            DE10-DU11.                                    CI0081
            11            DE10-XFONC  PICTURE  X(4).                    CI0081
            11            DE10-MPSBN  PICTURE  X(8).                    CI0081
            11            DE10-XDBDNM PICTURE  X(08).                   CI0081
            11            DE10-XSEGNM PICTURE  X(08).                   CI0081
            11            DE10-XRC    PICTURE  X(02).                   CI0081
            11            DE10-MSEG   PICTURE  X(08).                   CI0081
            11            DE10-XCOKEY PICTURE  X(70).                   CI0081
            11            DE10-CUIBR  PICTURE  X(01).                   CI0081
            11            DE10-CUIBA  PICTURE  X(01).                   CI0081
            11            DE10-IPBIK  PICTURE  X(1).                    CI0081
            10            DE10-DU03.                                    CI0081
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            11            DE10-CMSSF  PICTURE  XX.                      CI0081
            11            DE10-DU09.                                    CI0081
            12            DE10-CMESA  PICTURE  S9(9)                    CI0081
                          BINARY.                                       CI0081
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0081
                          BINARY.                                       CI0081
            12            DE10-CMESB  PICTURE  S9(9)                    CI0081
                          BINARY.                                       CI0081
            12            DE10-CMSST  PICTURE  S9(9)                    CI0081
                          BINARY.                                       CI0081
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0081
                          BINARY.                                       CI0081
            12            DE10-QELLAA PICTURE  S9(9)                    CI0081
                          BINARY.                                       CI0081
            12            DE10-TMESS4 PICTURE  X(512).                  CI0081
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
       01                 MS00.                                         CI0081
          05              MS00-SUITE.                                   CI0081
            15       FILLER         PICTURE  X(00542).                  CI0081
       01                 MS03  REDEFINES      MS00.                    CI0081
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            10            MS03-CMSSF  PICTURE  XX.                      CI0081
            10            MS03-DU09.                                    CI0081
            11            MS03-CMESA  PICTURE  S9(9)                    CI0081
                          BINARY.                                       CI0081
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0081
                          BINARY.                                       CI0081
            11            MS03-CMESB  PICTURE  S9(9)                    CI0081
                          BINARY.                                       CI0081
            11            MS03-CMSST  PICTURE  S9(9)                    CI0081
                          BINARY.                                       CI0081
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0081
                          BINARY.                                       CI0081
            11            MS03-QELLAA PICTURE  S9(9)                    CI0081
                          BINARY.                                       CI0081
            11            MS03-TMESS4 PICTURE  X(512).                  CI0081
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0081
            10            MX11-QMSGS  PICTURE  9(03).                   CI0081
            10            MX11-PJ09                                     CI0081
                          OCCURS       025     TIMES.                   CI0081
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0081
                          COMPUTATIONAL-3.                              CI0081
            11            MX11-CMESB  PICTURE  S9(9)                    CI0081
                          BINARY.                                       CI0081
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                WZ05
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0081
      *               *                                   *             CI0081
      *               *INITIALISATIONS                    *             CI0081
      *               *                                   *             CI0081
      *               *************************************.            CI0081
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
       F02XA.                                                           lv10
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR ARAY                                             DOT
           SET ADDRESS OF PD06 TO                                       ADU015
                PCB-ARAY-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0081
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0081
      *               *                                   *             CI0081
      *               *FIN DE TRAITEMENT                  *             CI0081
      *               *                                   *             CI0081
      *               *************************************.            CI0081
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0081
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *MAIN PROCESSING                    *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50DD.    NOTE *CHECK THE CONTRACT                 *.
       F50DD.                                                           lv10
      *SEE IF THE CONTRACT HAS AN
      *ARRANGEMENT SET UP
      *
      *N50DJ.    NOTE *INITIALIZE STUFF                   *.
       F50DJ.                                                           lv15
           MOVE        '0' TO CX2Y-CF
           MOVE        'N' TO WZ05-ICLID
           MOVE        WZ05-CTID TO S-CXU2Y-CTID
           MOVE        ZERO TO S-CXU2Y-CLID
           S-CXU2Y-CARTY
           S-CXU2Y-NARRS
           MOVE        '>' TO S-CXU2Y-OPER
      *
      *DO THE FIRST CX2Y READ (GU)
      *
           PERFORM     F94XS THRU F94XS-FN
           MOVE        '=' TO S-CXU2Y-OPER.
       F50DJ-FN. EXIT.
      *N50DN.    NOTE *LOOP THROUGH CX2Y                  *.
       F50DN.                       GO TO     F50DN-B.                  lv15
       F50DN-A.
                 IF    CX2Y-CF = '1'
                 OR    IK = '1'
                                    GO TO     F50DN-FN.
       F50DN-B.       EXIT.
      *N50DT.    NOTE *IF THE READ IS SUCCESSFUL          *.
       F50DT.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F50DT-FN.
      *N50DZ.    NOTE *IF THE ACCOUNT NBR IS THE SAME     *.
       F50DZ.    IF    CX2Y-CTID = WZ05-CTID                            lv25
                 NEXT SENTENCE ELSE GO TO     F50DZ-FN.
      *N50HD.    NOTE *IF SD AND SD ARR FOUND             *.
       F50HD.    IF    (WZ05-MAPPN = 'SD'                               lv30
                 AND   CX2Y-CARTY = 10)
                 OR    ((WZ05-MAPPN = 'UD' OR
                       'FDC')
                 AND   (CX2Y-CARTY = 02))
                 NEXT SENTENCE ELSE GO TO     F50HD-FN.
      *OR UD/FDC & UD ARR FOUND
           MOVE        '1' TO CX2Y-CF.
       F50HD-900. GO TO F50HJ-FN.
       F50HD-FN. EXIT.
      *N50HJ.    NOTE *IF NOT, READ ANOTHER               *.
       F50HJ.                                                           lv30
           PERFORM     F94XR THRU F94XR-FN.
       F50HJ-FN. EXIT.
       F50DZ-900. GO TO F50JN-FN.
       F50DZ-FN. EXIT.
      *N50JN.    NOTE *IF THE ACCOUNT NBR IS DIFERENT     *.
       F50JN.                                                           lv25
      *THE ACCOUNT DOESN'T HAVA AN SD
      *
           MOVE        '1' TO IK.
       F50JN-FN. EXIT.
       F50DT-FN. EXIT.
       F50DN-900. GO TO F50DN-A.
       F50DN-FN. EXIT.
      *N50JR.    NOTE *IF SD AND SD WAS FOUND             *.
       F50JR.    IF    CX2Y-CF = '1'                                    lv15
                 NEXT SENTENCE ELSE GO TO     F50JR-FN.
      *OR UD/FDC AND UD WAS FOUND..
      *FILL IN THE LINKAGE AREA AND
      *RETURN.
      *
           MOVE        CX2Y-CLID TO WZ05-CLID
           MOVE        'N' TO WZ05-ICLID
           MOVE        CX2Y-NARRS TO WZ05-NARRS
      *
      *ALSO GET THE CX03-GECSQ (GU)
      *
           MOVE        CX2Y-CLID TO S-CXU01-CLID
           MOVE        CX2Y-CARTY TO S-CXU03-CARTY
           MOVE        CX2Y-NARRS TO S-CXU03-NARRS
           PERFORM     F94CR THRU F94CR-FN.
      *N50JV.    NOTE *IF THE CX03 READ WAS GOOD          *.
       F50JV.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F50JV-FN.
           MOVE        CX03-GECSQ TO WZ05-GECSQ.
       F50JV-FN. EXIT.
       F50JR-FN. EXIT.
       F50DD-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *MAIN PROCESSING DEUX               *
      *               *                                   *
      *               *************************************.
       F55.      IF    CX2Y-CF = '0'                                    lv05
                 NEXT SENTENCE ELSE GO TO     F55-FN.
      *
      *IF THE ACCOUNT DIDN'T HAVE AN
      *SD ARRANGEMENT, SEE IF THE OWNER
      *OF THE ACOUNT HAS ONE
      *
      *N55DD.    NOTE *INITIALIZE SOME STUFF              *.
       F55DD.         EXIT.                                             lv10
       F55DD-FN. EXIT.
      *N55DH.    NOTE *SELECT THE ARRANGEMENT CLIENT      *.
       F55DH.                                                           lv10
      *THIS SECTION WAS COPIED FROM
      *SDKA50 F87CC
      *
      *INITIALIZE SWITCH AND
      *SELECTION FIELDS
      *
           MOVE        'N' TO 7-ARR-CLIENT-FOUND
           MOVE        ZEROS TO 7-SEL1-CLID
           7-SEL2-CLID
           7-SEL3-CLID
           7-SEL4-CLID
           7-SEL5-CLID
           7-SEL6-CLID
           MOVE        ZEROES TO 7-SEL1-GECSQ
           7-SEL2-GECSQ
           7-SEL3-GECSQ
           7-SEL4-GECSQ
           7-SEL5-GECSQ
           7-SEL6-GECSQ.
      *N55DJ.    NOTE *READ THE CT01 (GU) & CT07 (GN)     *.
       F55DJ.                                                           lv15
           MOVE        '0' TO CT07-CF
           MOVE        WZ05-CTID TO S-CTU01-CT01K
           PERFORM     F94DU THRU F94DU-FN.
      *N55DK.    NOTE *READ THE CT07  (GN)                *.
       F55DK.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F55DK-FN.
           PERFORM     F94TN THRU F94TN-FN.
                 IF    IK = '0'                                         DOT
           MOVE        '1' TO CT07-CF.
       F55DK-FN. EXIT.
       F55DJ-FN. EXIT.
      *N55DL.    NOTE *LOOK FOR CLIENT TO USE             *.
       F55DL.    IF    CT07-CF = '1'                                    lv15
                 NEXT SENTENCE ELSE GO TO     F55DL-FN.
           MOVE        CT07-CLID TO S-CTU07-CT07K
      *USE OWNER ROLE CODE AS KEY
           MOVE        '001' TO S-CTU09-CT09K
           PERFORM     F94TQ THRU F94TQ-FN.
      *N55DQ.    NOTE *IF OWNER ROLE FOUND                *.
       F55DQ.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F55DQ-FN.
      *N55DV.    NOTE *MUST BE AN ACTIVE OWNER            *.
       F55DV.    IF    CT09-GERED = ZERO                                lv25
                 NEXT SENTENCE ELSE GO TO     F55DV-FN.
                 IF    7-SEL6-CLID = ZEROS                              DOT
      *THIS CLIENT MEETS CRITERIA #6
      *IF IT'S THE 1ST ONE
           MOVE        CT07-CLID TO 7-SEL6-CLID
           MOVE        CT09-GECSQ TO 7-SEL6-GECSQ.
                 IF    7-SEL3-CLID = ZEROS                              DOT
                 AND   CT09-GECSQ > ZERO
      *THIS CLIENT MEETS CRITERIA #3
      *IF IT HAS THE ADDRESS FOR THE
      *ACCOUNT AND IT'S THE 1ST ONE
           MOVE        CT07-CLID TO 7-SEL3-CLID
           MOVE        CT09-GECSQ TO 7-SEL3-GECSQ.
      *LOOK FOR ANY ARRANGEMENTS                                        DOT
           MOVE        CT07-CLID TO S-CXU01-CX01K
           MOVE        ZERO TO S-CXU03-CARTY
           S-CXU03-NARRS
           MOVE        '>' TO S-CXU03-OPER
           PERFORM     F94CR THRU F94CR-FN
           MOVE        '=' TO S-CXU03-OPER.
      *N55HD.    NOTE *FOUND AN ARRANGEMENT SEGMENT       *.
       F55HD.    IF    IK = '0'                                         lv30
                 NEXT SENTENCE ELSE GO TO     F55HD-FN.
      *N55HI.    NOTE *FOUND AN SD ARRANGEMENT            *.
       F55HI.    IF    CX03-CARTY = 10                                  lv35
                 NEXT SENTENCE ELSE GO TO     F55HI-FN.
      *THIS CLIENT MEETS CRITERIA #1
           MOVE        CT07-CLID TO 7-SEL1-CLID
           MOVE        CX03-GECSQ TO 7-SEL1-GECSQ
      *SET SWITCH
           MOVE        'Y' TO 7-ARR-CLIENT-FOUND
               GO TO     F55DL-FN.
       F55HI-FN. EXIT.
      *N55HN.    NOTE *LOOK AGAIN IF TYPE < 10 (SD)       *.
       F55HN.    IF    CX03-CARTY < 10                                  lv35
                 NEXT SENTENCE ELSE GO TO     F55HN-FN.
           MOVE        10 TO S-CXU03-CARTY
           MOVE        +1 TO S-CXU03-NARRS
           PERFORM     F94CS THRU F94CS-FN.
      *N55HS.    NOTE *FOUND AN SD ARRANGEMENT            *.
       F55HS.    IF    IK = '0'                                         lv40
                 NEXT SENTENCE ELSE GO TO     F55HS-FN.
      *THIS CLIENT MEETS CRITERIA #1
           MOVE        CT07-CLID TO 7-SEL1-CLID
           MOVE        CX03-GECSQ TO 7-SEL1-GECSQ
      *SET SWITCH
           MOVE        'Y' TO 7-ARR-CLIENT-FOUND
               GO TO     F55DL-FN.
       F55HS-FN. EXIT.
       F55HN-FN. EXIT.
      *N55HX.    NOTE *CHECK FOR CRITERIA #2              *.
       F55HX.    IF    7-SEL2-CLID = ZEROS                              lv35
                 AND   CT09-GECSQ > ZERO
                 NEXT SENTENCE ELSE GO TO     F55HX-FN.
           MOVE        CT07-CLID TO 7-SEL2-CLID
           MOVE        CT09-GECSQ TO 7-SEL2-GECSQ
      *AT THIS POINT, CAN'T DO ANY
      *BETTER WITH THIS CLIENT SO JUST
      *READ THE NEXT CT07
           MOVE        'Y' TO 7-ARR-CLIENT-FOUND
               GO TO     F55DL-FN.
       F55HX-FN. EXIT.
      *N55LD.    NOTE *RULES TO LOOK FOR CRITERIA #4      *.
       F55LD.    IF    7-SEL2-CLID = ZEROS                              lv35
                 AND   7-SEL3-CLID = ZEROS
                 AND   7-SEL4-CLID = ZEROS
                 NEXT SENTENCE ELSE GO TO     F55LD-FN.
      *THERE IS NO REASON TO LOOK FOR
      *CRITERIA #4 IF IT'S ALREADY BEEN
      *FOUND OR THERE HAS BEEN A CLIENT
      *FOUND THAT FIT A HIGHER CRITERIA
           MOVE        CT07-CLID TO S-CLU01-CLID
           MOVE        +1 TO S-CLU24-GECSQ
           MOVE        '>=' TO S-CLU24-OPER
           MOVE        LOW-VALUES TO CL24
           PERFORM     F94LN THRU F94LN-FN
           MOVE        '=' TO S-CLU24-OPER.
      *N55LI.    NOTE *LOOK FOR ACTIVE ADDRESS            *.
       F55LI.    IF    IK = '0'                                         lv40
                 NEXT SENTENCE ELSE GO TO     F55LI-FN.
                 IF    CL24-GECED = ZERO                                DOT
      *IF ACTIVE, IT MEETS CRITERIA #4
           MOVE        CT07-CLID TO 7-SEL4-CLID
           MOVE        CL24-GECSQ TO 7-SEL4-GECSQ
           MOVE        '1' TO IK
                 ELSE
      *IF INACTIVE, GET NEXT CL24
           MOVE        LOW-VALUES TO CL24
           PERFORM     F94LP THRU F94LP-FN.
       F55LI-900. GO TO F55LI.
       F55LI-FN. EXIT.
       F55LD-FN. EXIT.
       F55HD-900. GO TO F55LN-FN.
       F55HD-FN. EXIT.
      *N55LN.    NOTE *CLIENT HAS NO ARRANGEMENTS         *.
       F55LN.         EXIT.                                             lv30
      *N55LS.    NOTE *RULES FOR CRITERIA #5              *.
       F55LS.    IF    7-SEL2-CLID = ZEROS                              lv35
                 AND   7-SEL3-CLID = ZEROS
                 AND   7-SEL4-CLID = ZEROS
                 AND   7-SEL5-CLID = ZEROS
                 NEXT SENTENCE ELSE GO TO     F55LS-FN.
      *THERE IS NO REASON TO LOOK FOR
      *CRITERIA #5 IF IT'S ALREADY BEEN
      *FOUND OR THERE HAS BEEN A CLIENT
      *FOUND THAT FIT A HIGHER CRITERIA
           MOVE        CT07-CLID TO S-CLU01-CLID
           MOVE        +1 TO S-CLU24-GECSQ
           MOVE        '>=' TO S-CLU24-OPER
           PERFORM     F94LN THRU F94LN-FN
           MOVE        '=' TO S-CLU24-OPER.
      *N55LX.    NOTE *LOOK FOR ACTIVE ADDRESS            *.
       F55LX.    IF    IK = '0'                                         lv40
                 NEXT SENTENCE ELSE GO TO     F55LX-FN.
                 IF    CL24-GECED = ZERO                                DOT
      *IF ACTIVE, IT MEETS CRITERIA #5
           MOVE        CT07-CLID TO 7-SEL5-CLID
           MOVE        CL24-GECSQ TO 7-SEL5-GECSQ
           MOVE        '1' TO IK
                 ELSE
      *IF INACTIVE, GET NEXT CL24
           MOVE        LOW-VALUES TO CL24
           PERFORM     F94LP THRU F94LP-FN.
       F55LX-900. GO TO F55LX.
       F55LX-FN. EXIT.
       F55LS-FN. EXIT.
       F55LN-FN. EXIT.
       F55DV-FN. EXIT.
       F55DQ-FN. EXIT.
      *N55PD.    NOTE *GET THE NEXT CLIENT                *.
       F55PD.                                                           lv20
           PERFORM     F94TN THRU F94TN-FN.
                 IF    IK = '1'                                         DOT
           MOVE        '0' TO CT07-CF.
       F55PD-FN. EXIT.
       F55DL-900. GO TO F55DL.
       F55DL-FN. EXIT.
      *N55PI.    NOTE *SELECT HIGHEST CRITERIA            *.
       F55PI.                                                           lv15
                 IF    7-SEL1-CLID NOT = ZEROS                          DOT
           MOVE        7-SEL1-CLID TO WZ05-CLID
           MOVE        7-SEL1-GECSQ TO WZ05-GECSQ
           MOVE        'Y' TO WZ05-ICLID
               GO TO     F55PI-FN.
                 IF    7-SEL2-CLID NOT = ZEROS                          DOT
           MOVE        7-SEL2-CLID TO WZ05-CLID
           MOVE        7-SEL2-GECSQ TO WZ05-GECSQ
           MOVE        'Y' TO WZ05-ICLID
               GO TO     F55PI-FN.
                 IF    7-SEL3-CLID NOT = ZEROS                          DOT
           MOVE        7-SEL3-CLID TO WZ05-CLID
           MOVE        7-SEL3-GECSQ TO WZ05-GECSQ
           MOVE        'Y' TO WZ05-ICLID
               GO TO     F55PI-FN.
                 IF    7-SEL4-CLID NOT = ZEROS                          DOT
           MOVE        7-SEL4-CLID TO WZ05-CLID
           MOVE        7-SEL4-GECSQ TO WZ05-GECSQ
           MOVE        'Y' TO WZ05-ICLID
               GO TO     F55PI-FN.
                 IF    7-SEL5-CLID NOT = ZEROS                          DOT
           MOVE        7-SEL5-CLID TO WZ05-CLID
           MOVE        7-SEL5-GECSQ TO WZ05-GECSQ
           MOVE        'Y' TO WZ05-ICLID
               GO TO     F55PI-FN.
                 IF    7-SEL6-CLID NOT = ZEROS                          DOT
           MOVE        7-SEL6-CLID TO WZ05-CLID
           MOVE        7-SEL6-GECSQ TO WZ05-GECSQ
           MOVE        'Y' TO WZ05-ICLID.
       F55PI-FN. EXIT.
       F55DH-FN. EXIT.
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
      *N94CR.    NOTE *CALL GU ON CX03                    *.            ADU026
       F94CR.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PC06 CX03                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CR-FN. EXIT.
      *N94CS.    NOTE *CALL GN ON CX03                    *.            ADU026
       F94CS.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PC06 CX03                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CS-FN. EXIT.
      *N94DU.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94DU.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DU-FN. EXIT.
      *N94LN.    NOTE *CALL GU ON CL24                    *.            ADU026
       F94LN.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL24' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 CL24                                                    ADU026
           S-CLU01-SSA S-CLU24-SSA                                      ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94LN-FN. EXIT.
      *N94LP.    NOTE *CALL GN ON CL24                    *.            ADU026
       F94LP.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL24' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PB06 CL24                                                    ADU026
           S-CLU01-SSA S-CL24-SSA                                       ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94LP-FN. EXIT.
      *N94TN.    NOTE *CALL GN ON CT07                    *.            ADU026
       F94TN.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT07' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CT07                                                    ADU026
           S-CTU01-SSA S-CT07-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94TN-FN. EXIT.
      *N94TQ.    NOTE *CALL GN ON CT09                    *.            ADU026
       F94TQ.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT09' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CT09                                                    ADU026
           S-CTU01-SSA S-CTU07-SSA                                      ADU026
           S-CTU09-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94TQ-FN. EXIT.
      *N94XR.    NOTE *CALL GN ON CX2Y                    *.            ADU026
       F94XR.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PD06 CX2Y                                                    ADU026
           S-CX2Y-SSA                                                   ADU026
           MOVE        PD06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94XR-FN. EXIT.
      *N94XS.    NOTE *CALL GU ON CX2Y                    *.            ADU026
       F94XS.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PD06 CX2Y                                                    ADU026
           S-CXU2Y-SSA                                                  ADU026
           MOVE        PD06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94XS-FN. EXIT.
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
