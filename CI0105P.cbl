       IDENTIFICATION DIVISION.                                         CI0105
       PROGRAM-ID.  CI0105P.                                            CI0105
      *AUTHOR.         ADD UD BANK AND ARRANGEMENT.                     CI0105
      *DATE-COMPILED.   09/08/14.                                       CI0105
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
       ENVIRONMENT DIVISION.                                            CI0105
       CONFIGURATION SECTION.                                           CI0105
       SOURCE-COMPUTER. IBM-370.                                        CI0105
       OBJECT-COMPUTER. IBM-370.                                        CI0105
       DATA DIVISION.                                                   CI0105
       WORKING-STORAGE SECTION.                                         CI0105
      *>>>>>>> Audit Log Work Area                                      ADU165
                                                                        ADU165
       01               AL00-ADDR.                                      ADU165
              05        AL00-NPNTR     USAGE IS POINTER.                ADU165
                                                                        ADU165
      *!WI pl=AL005                                                     ADU165
       01               AL00-NSEQ2P    VALUE ZERO                       ADU165
                        PICTURE S9(3)                                   CI0105
                          COMPUTATIONAL-3.                              CI0105
                                                                        ADU165
      *>>>>>>> Linkage Area for Logger Program DBI110                   ADU165
      *!WF DSP=DH DSL=DH SEL=10 FOR=I DES=2 LEV=1                       ADU165
       01                 DH10.                                         CI0105
            10            DH10-GERTC  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            DH10-XUIBP  PICTURE  S9(8)                    CI0105
                          VALUE                ZERO                     CI0105
                          BINARY.                                       CI0105
            10            DH10-NSEQ2P PICTURE  S9(3)                    CI0105
                          VALUE                ZERO                     CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            DH10-CAUL   PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            DH10-MAUSB  PICTURE  X(8)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            DH10-NAUSK  PICTURE  X(50)                    CI0105
                          VALUE                SPACE.                   CI0105
            10            DH10-CSYS   PICTURE  X(4)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            DH10-CAPPL  PICTURE  X(8)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            DH10-CAUSR  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            DH10-CAUFR  PICTURE  S9(5)                    CI0105
                          VALUE                ZERO                     CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            DH10-CAUAC  PICTURE  S9(5)                    CI0105
                          VALUE                ZERO                     CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            DH10-GEOPID PICTURE  X(6)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            DH10-CAUNIT PICTURE  X(4)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            DH10-GAUVR  PICTURE  X(400)                   CI0105
                          VALUE                SPACE.                   CI0105
       01               CL-CF.
             05         CL01-CF          PIC X.
             05         CL24-CF          PIC X.
       01                 CL00.                                         CI0105
            02            CL01.                                         CI0105
            10            CL01-CL01K.                                   CI0105
            11            CL01-C199.                                    CI0105
            12            CL01-CLID.                                    CI0105
            13            CL01-CLIDO  PICTURE  9(3).                    CI0105
            13            CL01-CLIDN.                                   CI0105
            14            CL01-CLIDNP PICTURE  X(12).                   CI0105
            14            CL01-CLIDND PICTURE  9(8).                    CI0105
            10            CL01-GECKD  PICTURE  9.                       CI0105
            10            CL01-GEMDA  PICTURE  9(8).                    CI0105
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0105
                          BINARY.                                       CI0105
            10            CL01-GECUC  PICTURE  99.                      CI0105
            10            CL01-CLDOR  PICTURE  9(8).                    CI0105
            10            CL01-CLLNG  PICTURE  XX.                      CI0105
            10            CL01-GESLC  PICTURE  99.                      CI0105
            10            CL01-CLTYP  PICTURE  X.                       CI0105
            10            CL01-CLCLS  PICTURE  9(3).                    CI0105
            10            CL01-CLTWRC PICTURE  99.                      CI0105
            10            CL01-CLPVC  PICTURE  99.                      CI0105
            10            CL01-CLIND  PICTURE  9(3).                    CI0105
            10            CL01-CLTRC  PICTURE  99.                      CI0105
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            CL01-AYSIDA PICTURE  9(3).                    CI0105
            10            CL01-AYSID  PICTURE  9(5).                    CI0105
            10            CL01-CLSTR  PICTURE  9(2).                    CI0105
            10            CL01-CLC11  PICTURE  X.                       CI0105
            10            CL01-CLTIN  PICTURE  9(12).                   CI0105
            10            CL01-CLTND  PICTURE  9(8).                    CI0105
            10            CL01-CLTINC PICTURE  9.                       CI0105
            10            CL01-CCDWA  PICTURE  9.                       CI0105
            10            CL01-CICES  PICTURE  X.                       CI0105
            10            CL01-CLTRA  PICTURE  9(2).                    CI0105
            10            CL01-DIRSY  PICTURE  9(4)                     CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            CL01-CFEDS  PICTURE  X.                       CI0105
            10            CL01-FILLER PICTURE  X(06).                   CI0105
            02            CL24.                                         CI0105
            10            CL24-GELL   PICTURE  9(4)                     CI0105
                          BINARY.                                       CI0105
            10            CL24-CL24K.                                   CI0105
            11            CL24-GECSQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            CL24-GECSD  PICTURE  9(8).                    CI0105
            10            CL24-GECED  PICTURE  9(8).                    CI0105
            10            CL24-CREQ2  PICTURE  X.                       CI0105
            10            CL24-FILLER PICTURE  X(4).                    CI0105
            10            CL24-GECTA  PICTURE  X.                       CI0105
            10            CL24-GELCD  PICTURE  9(8).                    CI0105
            10            CL24-GEADS  PICTURE  9.                       CI0105
            10            CL24-GECIT  PICTURE  X(25).                   CI0105
            10            CL24-GECTRY PICTURE  X(20).                   CI0105
            10            CL24-GECTY  PICTURE  9(3).                    CI0105
            10            CL24-GEPCD  PICTURE  X(12).                   CI0105
            10            CL24-GEST   PICTURE  X(8).                    CI0105
            10            CL24-IRESA  PICTURE  X.                       CI0105
            10            CL24-FILLER PICTURE  X(8).                    CI0105
            10            CL24-GESAD  PICTURE  X(30)                    CI0105
                          OCCURS       003     TIMES.                   CI0105
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0019           PIC X(8) VALUE 'CI0019P '.                  AM0019
       01  CI0025           PIC X(8) VALUE 'CI0025P '.                  AM0025
       01                 CT00.                                         CI0105
            02            CT01.                                         CI0105
            10            CT01-CT01K.                                   CI0105
            11            CT01-C299.                                    CI0105
            12            CT01-CTID.                                    CI0105
            13            CT01-CTIDA  PICTURE  9(3).                    CI0105
            13            CT01-CTIDN.                                   CI0105
            14            CT01-CTIDNP PICTURE  X(13).                   CI0105
            14            CT01-CTIDND PICTURE  9(11).                   CI0105
            10            CT01-GECKD  PICTURE  9.                       CI0105
            10            CT01-GEMDA  PICTURE  9(8).                    CI0105
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0105
                          BINARY.                                       CI0105
            10            CT01-GECUC  PICTURE  99.                      CI0105
            10            CT01-CTAUL  PICTURE  9(3).                    CI0105
            10            CT01-DIRAC  PICTURE  9(4).                    CI0105
            10            CT01-CTCCI  PICTURE  X.                       CI0105
            10            CT01-CTCUS  PICTURE  999.                     CI0105
            10            CT01-CTEFD  PICTURE  9(8).                    CI0105
            10            CT01-CTIAD  PICTURE  9(8).                    CI0105
            10            CT01-CLCUS  PICTURE  99.                      CI0105
            10            CT01-CAMMB  PICTURE  X(3).                    CI0105
            10            CT01-CKPMM  PICTURE  X.                       CI0105
            10            CT01-CTLAD  PICTURE  9(8).                    CI0105
            10            CT01-IPERS  PICTURE  X.                       CI0105
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            CT01-CTLAT  PICTURE  9(8).                    CI0105
            10            CT01-CTLATC PICTURE  9(6).                    CI0105
            10            CT01-IMEGA  PICTURE  X.                       CI0105
            10            CT01-DIRAB  PICTURE  9(8).                    CI0105
            10            CT01-COLRQ  PICTURE  X.                       CI0105
            10            CT01-ZDA04  PICTURE  X(4).                    CI0105
            10            CT01-CTLPD  PICTURE  9(8).                    CI0105
            10            CT01-CIRASP PICTURE  9.                       CI0105
            10            CT01-CIRATP PICTURE  99.                      CI0105
            10            CT01-DRTHC  PICTURE  9(8).                    CI0105
            10            CT01-CPPTC  PICTURE  X.                       CI0105
            10            CT01-ZDA06  PICTURE  X(6).                    CI0105
            10            CT01-CTACD  PICTURE  9(8).                    CI0105
            10            CT01-CTNLI  PICTURE  X.                       CI0105
            10            CT01-CTRHO  PICTURE  9(8).                    CI0105
            10            CT01-CTSGD  PICTURE  9(8).                    CI0105
            10            CT01-CPATP  PICTURE  X(1).                    CI0105
            10            CT01-IRSTA  PICTURE  X.                       CI0105
            10            CT01-CTSTA  PICTURE  99.                      CI0105
            10            CT01-CTSSC  PICTURE  99.                      CI0105
            10            CT01-PRLIN  PICTURE  9(3).                    CI0105
            10            CT01-PRCOD  PICTURE  9(5).                    CI0105
            10            CT01-PRSCD  PICTURE  X(9).                    CI0105
            10            CT01-CTLNI  PICTURE  X.                       CI0105
            10            CT01-AYSIDA PICTURE  9(3).                    CI0105
            10            CT01-AYSID  PICTURE  9(5).                    CI0105
            10            CT01-CTBMC  PICTURE  99.                      CI0105
            10            CT01-CINAR  PICTURE  99.                      CI0105
            10            CT01-CPHTR  PICTURE  X.                       CI0105
            10            CT01-CDSTR  PICTURE  XX.                      CI0105
            10            CT01-CQACT  PICTURE  999.                     CI0105
            10            CT01-CIRAS  PICTURE  999.                     CI0105
            10            CT01-CIRAT  PICTURE  999.                     CI0105
            10            CT01-CLRAY  PICTURE  9(5).                    CI0105
            10            CT01-CATTP  PICTURE  X.                       CI0105
      *Segment Access flags
       01               CX-CF.
             05         CX01-CF          PIC X.
             05         CX03-CF          PIC X.
             05         CX06-CF          PIC X.
             05         CX09-CF          PIC X.
             05         CX18-CF          PIC X.
             05         CX21-CF          PIC X.
             05         CX2Y-CF          PIC X.
       01                 CX01.                                         CI0105
            10            CX01-CX01K.                                   CI0105
            11            CX01-C199.                                    CI0105
            12            CX01-CLID.                                    CI0105
            13            CX01-CLIDO  PICTURE  9(3).                    CI0105
            13            CX01-CLIDN.                                   CI0105
            14            CX01-CLIDNP PICTURE  X(12).                   CI0105
            14            CX01-CLIDND PICTURE  9(8).                    CI0105
            10            CX01-GEMDA  PICTURE  9(8).                    CI0105
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0105
                          BINARY.                                       CI0105
            10            CX01-FILLER PICTURE  X(5).                    CI0105
       01                 CX03.                                         CI0105
            10            CX03-GELL   PICTURE  9(4)                     CI0105
                          BINARY.                                       CI0105
            10            CX03-CY00.                                    CI0105
            11            CX03-CX03K.                                   CI0105
            12            CX03-CARTY  PICTURE  99.                      CI0105
            12            CX03-NARRS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            CX03-CARST  PICTURE  99.                      CI0105
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            CX03-CPMTG  PICTURE  99.                      CI0105
            11            CX03-GRCRNG PICTURE  9(3).                    CI0105
            11            CX03-DEXDT  PICTURE  9(8).                    CI0105
            11            CX03-DASUP  PICTURE  9(8).                    CI0105
            11            CX03-CSTEC  PICTURE  X(3).                    CI0105
            11            CX03-FILLER PICTURE  X(17).                   CI0105
            11            CX03-CY50.                                    CI0105
            12            CX03-NARID  PICTURE  X(30).                   CI0105
            11            CX03-CY51                                     CI0105
                          REDEFINES            CX03-CY50.               CI0105
            12            CX03-NDIDN  PICTURE  9(12).                   CI0105
            12            CX03-FILLER PICTURE  X(18).                   CI0105
            11            CX03-CY52                                     CI0105
                          REDEFINES            CX03-CY50.               CI0105
            12            CX03-NAIDC  PICTURE  9(12).                   CI0105
            12            CX03-FILLER PICTURE  X(18).                   CI0105
            11            CX03-CY53                                     CI0105
                          REDEFINES            CX03-CY50.               CI0105
            12            CX03-NAMEXB PICTURE  9(15).                   CI0105
            12            CX03-FILLER PICTURE  X(15).                   CI0105
            10            CX03-CY99.                                    CI0105
            11            CX03-FILLER PICTURE  X(109).                  CI0105
            10            CX03-CY01                                     CI0105
                          REDEFINES            CX03-CY99.               CI0105
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            CX03-ICPCI  PICTURE  X.                       CI0105
            11            CX03-CLUPD  PICTURE  9(3).                    CI0105
            11            CX03-DLAUP  PICTURE  9(8).                    CI0105
            11            CX03-CWRC   PICTURE  99.                      CI0105
            11            CX03-CHCR   PICTURE  99.                      CI0105
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0105
            11            CX03-GEAUN  PICTURE  9(5).                    CI0105
            11            CX03-DPCHD  PICTURE  9(8).                    CI0105
            11            CX03-DLRCHK PICTURE  9(8).                    CI0105
            11            CX03-QTRCHK PICTURE  9(2).                    CI0105
            11            CX03-DNPMT  PICTURE  9(8).                    CI0105
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            CX03-CY02                                     CI0105
                          REDEFINES            CX03-CY99.               CI0105
            11            CX03-QSIRQ  PICTURE  99.                      CI0105
            11            CX03-QDRMN  PICTURE  9(2)                     CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            CX03-DDPRE  PICTURE  9(8).                    CI0105
            11            CX03-DDSHP  PICTURE  9(8).                    CI0105
            11            CX03-NDRFTB PICTURE  9(5).                    CI0105
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0105
            11            CX03-DDSHPA PICTURE  9(8).                    CI0105
            11            CX03-NDRFTF PICTURE  9(5).                    CI0105
            11            CX03-QDIPBK PICTURE  9(3).                    CI0105
            11            CX03-CREOR  PICTURE  X(1).                    CI0105
            11            CX03-CREOR1 PICTURE  X(1).                    CI0105
            11            CX03-DDASC  PICTURE  9(8).                    CI0105
            11            CX03-FILLER PICTURE  X(7).                    CI0105
            10            CX03-CY03                                     CI0105
                          REDEFINES            CX03-CY99.               CI0105
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0105
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0105
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0105
            11            CX03-DOPDA  PICTURE  99.                      CI0105
            11            CX03-CPMTF  PICTURE  99.                      CI0105
            11            CX03-CIRMO  PICTURE  X(12).                   CI0105
            11            CX03-CPALL  PICTURE  X(1).                    CI0105
            11            CX03-CCOLM  PICTURE  9(2).                    CI0105
            11            CX03-CBLTP  PICTURE  X(1).                    CI0105
            11            CX03-CASUB  PICTURE  9(2).                    CI0105
            11            CX03-CBLFM  PICTURE  9(2).                    CI0105
            11            CX03-IBILS  PICTURE  X.                       CI0105
            11            CX03-IPAOS  PICTURE  X.                       CI0105
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0105
            11            CX03-DLBPD  PICTURE  9(8).                    CI0105
            11            CX03-DNBPD  PICTURE  9(8).                    CI0105
            11            CX03-DODBD  PICTURE  9(8).                    CI0105
            11            CX03-CPSRE  PICTURE  99.                      CI0105
            11            CX03-ISPHN  PICTURE  X.                       CI0105
            11            CX03-TCARR  PICTURE  X(6).                    CI0105
            11            CX03-CBKPT  PICTURE  9(2).                    CI0105
            11            CX03-IECNT  PICTURE  X.                       CI0105
            11            CX03-ICONV  PICTURE  X(1).                    CI0105
            11            CX03-FILLER PICTURE  X(4).                    CI0105
            10            CX03-CY04                                     CI0105
                          REDEFINES            CX03-CY99.               CI0105
            11            CX03-CCARD  PICTURE  X(02).                   CI0105
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0105
            11            CX03-IREMT  PICTURE  X(01).                   CI0105
            11            CX03-ISBILA PICTURE  X.                       CI0105
            11            CX03-DLBPDA PICTURE  9(8).                    CI0105
            11            CX03-DNBPDA.                                  CI0105
            12            CX03-DNCYM  PICTURE  9(6).                    CI0105
            12            CX03-CEDTD  PICTURE  9(2).                    CI0105
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            CX03-DREMT  PICTURE  9(8).                    CI0105
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0105
            11            CX03-CWRC2  PICTURE  99.                      CI0105
            11            CX03-CHCR2  PICTURE  99.                      CI0105
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0105
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0105
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0105
       01                 CX06.                                         CI0105
            10            CX06-CX06K.                                   CI0105
            11            CX06-C299.                                    CI0105
            12            CX06-CTID.                                    CI0105
            13            CX06-CTIDA  PICTURE  9(3).                    CI0105
            13            CX06-CTIDN.                                   CI0105
            14            CX06-CTIDNP PICTURE  X(13).                   CI0105
            14            CX06-CTIDND PICTURE  9(11).                   CI0105
            10            CX06-NPECK  PICTURE  9(02).                   CI0105
            10            CX06-FILLER PICTURE  X.                       CI0105
       01                 CX09.                                         CI0105
            10            CX09-CX09K.                                   CI0105
            11            CX09-NPAIS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            CX09-CDEL1  PICTURE  9(3).                    CI0105
            10            CX09-NDELS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            CX09-CDEST  PICTURE  99.                      CI0105
            10            CX09-DISUP  PICTURE  9(8).                    CI0105
            10            CX09-CLUPD  PICTURE  9(3).                    CI0105
            10            CX09-DLAUP  PICTURE  9(8).                    CI0105
            10            CX09-GEOPD2 PICTURE  X(8).                    CI0105
            10            CX09-DPCHD  PICTURE  9(8).                    CI0105
            10            CX09-FILLER PICTURE  X(06).                   CI0105
       01                 CX18.                                         CI0105
            10            CX18-CX18K.                                   CI0105
            11            CX18-NBASQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            CX18-NPBN   PICTURE  X(20).                   CI0105
            10            CX18-CCBAT  PICTURE  99.                      CI0105
            10            CX18-DACHP  PICTURE  9(8).                    CI0105
            10            CX18-CSTPRE PICTURE  99.                      CI0105
            10            CX18-C199.                                    CI0105
            11            CX18-CLID.                                    CI0105
            12            CX18-CLIDO  PICTURE  9(3).                    CI0105
            12            CX18-CLIDN.                                   CI0105
            13            CX18-CLIDNP PICTURE  X(12).                   CI0105
            13            CX18-CLIDND PICTURE  9(8).                    CI0105
            10            CX18-MCSIG  PICTURE  X(30).                   CI0105
            10            CX18-CPBNU  PICTURE  X.                       CI0105
            10            CX18-CSPCR  PICTURE  99.                      CI0105
            10            CX18-DAPCR  PICTURE  9(8).                    CI0105
            10            CX18-FILLER PICTURE  XX.                      CI0105
       01                 CX19.                                         CI0105
            10            CX19-DAUDT  PICTURE  9(8).                    CI0105
            10            CX19-FILLER PICTURE  X(20).                   CI0105
       01                 CX2Y.                                         CI0105
            10            CX2Y-CX2YK.                                   CI0105
            11            CX2Y-C299.                                    CI0105
            12            CX2Y-CTID.                                    CI0105
            13            CX2Y-CTIDA  PICTURE  9(3).                    CI0105
            13            CX2Y-CTIDN.                                   CI0105
            14            CX2Y-CTIDNP PICTURE  X(13).                   CI0105
            14            CX2Y-CTIDND PICTURE  9(11).                   CI0105
            11            CX2Y-C199.                                    CI0105
            12            CX2Y-CLID.                                    CI0105
            13            CX2Y-CLIDO  PICTURE  9(3).                    CI0105
            13            CX2Y-CLIDN.                                   CI0105
            14            CX2Y-CLIDNP PICTURE  X(12).                   CI0105
            14            CX2Y-CLIDND PICTURE  9(8).                    CI0105
            11            CX2Y-CARTY  PICTURE  99.                      CI0105
            11            CX2Y-NARRS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
       01                 CX21.                                         CI0105
            10            CX21-GELL   PICTURE  9(4)                     CI0105
                          BINARY.                                       CI0105
            10            CX21-CZ00.                                    CI0105
            11            CX21-CX21K.                                   CI0105
            12            CX21-CDEL1  PICTURE  9(3).                    CI0105
            12            CX21-NDELS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            CX21-CZ99.                                    CI0105
            11            CX21-FILLER PICTURE  X(165).                  CI0105
            10            CX21-CZ01                                     CI0105
                          REDEFINES            CX21-CZ99.               CI0105
            11            CX21-NBASQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            CX21-GECSQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            CX21-CZ02                                     CI0105
                          REDEFINES            CX21-CZ99.               CI0105
            11            CX21-CPAYE  PICTURE  9(2).                    CI0105
            11            CX21-C199.                                    CI0105
            12            CX21-CLID.                                    CI0105
            13            CX21-CLIDO  PICTURE  9(3).                    CI0105
            13            CX21-CLIDN.                                   CI0105
            14            CX21-CLIDNP PICTURE  X(12).                   CI0105
            14            CX21-CLIDND PICTURE  9(8).                    CI0105
            11            CX21-GECSQ1 PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            CX21-NBASQT PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            CX21-TDELI  PICTURE  X(30).                   CI0105
      ******************************************************            AADA81
      ****      WORK AREAS FOR COMMON DATE UTILITY       ***            AADA81
      ******************************************************            AADA81
      **                                                                AADA81
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA81
      **                                                                AADA81
      **   SEGMENT DD30 - FUNCTION LAYOUT                               AADA81
      **                                                                AADA81
      *!WF DSP=DD DSL=DD SEL=30 FOR=I DES=2 LEV=1                       AADA81
       01                 DD30.                                         CI0105
            10            DD30-CDTFN  PICTURE  9(4)                     CI0105
                          VALUE                ZERO.                    CI0105
            10            DD30-CDTSF  PICTURE  9(4)                     CI0105
                          VALUE                ZERO.                    CI0105
            10            DD30-CDTSC  PICTURE  9(4)                     CI0105
                          VALUE                ZERO.                    CI0105
            10            DD30-FILLER PICTURE  X(40)                    CI0105
                          VALUE                SPACE.                   CI0105
       01                 DD33.                                         CI0105
            10            DD33-DTGRG.                                   CI0105
            11            DD33-DTGCY.                                   CI0105
            12            DD33-DTGCC  PICTURE  9(2)                     CI0105
                          VALUE                ZERO.                    CI0105
            12            DD33-DTGYY  PICTURE  9(2)                     CI0105
                          VALUE                ZERO.                    CI0105
            11            DD33-DTGMM  PICTURE  9(2)                     CI0105
                          VALUE                ZERO.                    CI0105
            11            DD33-DTGDD  PICTURE  9(2)                     CI0105
                          VALUE                ZERO.                    CI0105
            10            DD33-DTJULC.                                  CI0105
            11            DD33-DTJCY.                                   CI0105
            12            DD33-DTJCC  PICTURE  9(2)                     CI0105
                          VALUE                ZERO.                    CI0105
            12            DD33-DTJYY  PICTURE  9(2)                     CI0105
                          VALUE                ZERO.                    CI0105
            11            DD33-DTJDDC PICTURE  S9(3)                    CI0105
                          VALUE                ZERO.                    CI0105
            11            DD33-DTJDD                                    CI0105
                          REDEFINES            DD33-DTJDDC              CI0105
               PICTURE    9(3).                                         CI0105
            10            DD33-DTJUL                                    CI0105
                          REDEFINES            DD33-DTJULC              CI0105
               PICTURE    9(7).                                         CI0105
            10            DD33-DTDYR  PICTURE  9(3)                     CI0105
                          VALUE                ZERO.                    CI0105
            10            DD33-DTDMO  PICTURE  9(2)                     CI0105
                          VALUE                ZERO.                    CI0105
            10            DD33-FILLER PICTURE  X(18)                    CI0105
                          VALUE                SPACE.                   CI0105
      **                                                                AADA81
      **   SEGMENT DD33 - CONVERT DATE LAYOUT                           AADA81
      **                                                                AADA81
      *!WF DSP=DD DSL=DD SEL=33 FOR=I DES=2 LEV=1                       AADA81
      **                                                                AADA81
       01                 DI00.                                         CI0105
            02            DI01.                                         CI0105
            10            DI01-DH01.                                    CI0105
            11            DI01-GELL   PICTURE  9(4)                     CI0105
                          BINARY.                                       CI0105
            11            DI01-DI01K.                                   CI0105
            12            DI01-MAUSB  PICTURE  X(8).                    CI0105
            12            DI01-CSYS   PICTURE  X(4).                    CI0105
            12            DI01-CAPPL  PICTURE  X(8).                    CI0105
            12            DI01-CADATE PICTURE  X(8).                    CI0105
            12            DI01-GETIM  PICTURE  S9(7)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            12            DI01-XTERMI PICTURE  X(08).                   CI0105
            12            DI01-NSEQ2P PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            DI01-GEOPID PICTURE  X(6).                    CI0105
            11            DI01-CAUNIT PICTURE  X(4).                    CI0105
            11            DI01-CAUSR  PICTURE  X.                       CI0105
            11            DI01-CAUFR  PICTURE  S9(5)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            DI01-CAUAC  PICTURE  S9(5)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            DI01-NAUSK  PICTURE  X(50).                   CI0105
            11            DI01-GELL1  PICTURE  9(4)                     CI0105
                          BINARY.                                       CI0105
            10            DI01-DH99.                                    CI0105
            11            DI01-GAUVR  PICTURE  X(400).                  CI0105
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0105
            10            XW05-XW06.                                    CI0105
            11            XW05-XDBPCB.                                  CI0105
            12            XW05-XDBDNM PICTURE  X(08)                    CI0105
                          VALUE                SPACE.                   CI0105
            12            XW05-XSEGLV PICTURE  X(02)                    CI0105
                          VALUE                SPACE.                   CI0105
            12            XW05-XRC    PICTURE  X(02)                    CI0105
                          VALUE                SPACE.                   CI0105
            12            XW05-XPROPT PICTURE  X(04)                    CI0105
                          VALUE                SPACE.                   CI0105
            12            XW05-FILLER PICTURE  S9(5)                    CI0105
                          VALUE                ZERO                     CI0105
                          BINARY.                                       CI0105
            12            XW05-XSEGNM PICTURE  X(08)                    CI0105
                          VALUE                SPACE.                   CI0105
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0105
                          VALUE                ZERO                     CI0105
                          BINARY.                                       CI0105
            12            XW05-XSEGNB PICTURE  9(05)                    CI0105
                          VALUE                ZERO                     CI0105
                          BINARY.                                       CI0105
            12            XW05-XCOKEY PICTURE  X(70)                    CI0105
                          VALUE                SPACE.                   CI0105
            10            XW05-XW07.                                    CI0105
            11            XW05-XIOPCB.                                  CI0105
            12            XW05-XTERMI PICTURE  X(08)                    CI0105
                          VALUE                SPACE.                   CI0105
            12            XW05-FILLER PICTURE  XX                       CI0105
                          VALUE                SPACE.                   CI0105
            12            XW05-XRC1   PICTURE  X(02)                    CI0105
                          VALUE                SPACE.                   CI0105
            12            XW05-FILLER PICTURE  X(12)                    CI0105
                          VALUE                SPACE.                   CI0105
            12            XW05-XMODNM PICTURE  X(8)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0105
                          VALUE                ZERO.                    CI0105
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0105
                          VALUE                ZERO.                    CI0105
            10            XW05-XGU    PICTURE  X(4)                     CI0105
                          VALUE                'GU  '.                  CI0105
            10            XW05-XGHU   PICTURE  X(4)                     CI0105
                          VALUE                'GHU '.                  CI0105
            10            XW05-XGN    PICTURE  X(4)                     CI0105
                          VALUE                'GN  '.                  CI0105
            10            XW05-XGHN   PICTURE  X(4)                     CI0105
                          VALUE                'GHN '.                  CI0105
            10            XW05-XGNP   PICTURE  X(4)                     CI0105
                          VALUE                'GNP '.                  CI0105
            10            XW05-XGHNP  PICTURE  X(4)                     CI0105
                          VALUE                'GHNP'.                  CI0105
            10            XW05-XREPL  PICTURE  XXXX                     CI0105
                          VALUE                'REPL'.                  CI0105
            10            XW05-XISRT  PICTURE  X(4)                     CI0105
                          VALUE                'ISRT'.                  CI0105
            10            XW05-XDLET  PICTURE  X(4)                     CI0105
                          VALUE                'DLET'.                  CI0105
            10            XW05-XOPEN  PICTURE  X(4)                     CI0105
                          VALUE                'OPEN'.                  CI0105
            10            XW05-XCLSE  PICTURE  X(4)                     CI0105
                          VALUE                'CLSE'.                  CI0105
            10            XW05-XCHKP  PICTURE  X(4)                     CI0105
                          VALUE                'CHKP'.                  CI0105
            10            XW05-XXRST  PICTURE  X(4)                     CI0105
                          VALUE                'XRST'.                  CI0105
            10            XW05-XTERM  PICTURE  X(4)                     CI0105
                          VALUE                'TERM'.                  CI0105
            10            XW05-XNFPAC PICTURE  X(13)                    CI0105
                          VALUE                SPACE.                   CI0105
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0105
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0105
      *Miscellaneous DU linkage segments for:
      *CI0019,CI0025
      *!WF DSP=DU DSL=DU SEL=1530 FOR=I DES=1 LEV=1
      * PLT=DU
       01                 DU15.                                         CI0105
            10            DU15-C299.                                    CI0105
            11            DU15-CTID.                                    CI0105
            12            DU15-CTIDA  PICTURE  9(3).                    CI0105
            12            DU15-CTIDN.                                   CI0105
            13            DU15-CTIDNP PICTURE  X(13).                   CI0105
            13            DU15-CTIDND PICTURE  9(11).                   CI0105
            10            DU15-DCACG  PICTURE  9(8).                    CI0105
            10            DU15-IPOCH  PICTURE  X.                       CI0105
            10            DU15-FILLER PICTURE  X(100).                  CI0105
            10            DU15-DU18                                     CI0105
                          OCCURS       010     TIMES.                   CI0105
            11            DU15-CT10.                                    CI0105
            12            DU15-CT10K.                                   CI0105
            13            DU15-GR98.                                    CI0105
            14            DU15-GRID.                                    CI0105
            15            DU15-GRIDC  PICTURE  9(3).                    CI0105
            15            DU15-GRIDN.                                   CI0105
            16            DU15-GRIDNP PICTURE  99.                      CI0105
            16            DU15-GRIDND PICTURE  9(8).                    CI0105
            12            DU15-GR97                                     CI0105
                          REDEFINES            DU15-CT10K.              CI0105
            13            DU15-GRIDCB PICTURE  9(3).                    CI0105
            13            DU15-FILLER PICTURE  X(10).                   CI0105
            12            DU15-GERSD  PICTURE  9(8).                    CI0105
            12            DU15-GERED  PICTURE  9(8).                    CI0105
            12            DU15-GRCSI  PICTURE  X.                       CI0105
            11            DU15-GR01.                                    CI0105
            12            DU15-GR01K.                                   CI0105
            13            DU15-GR98.                                    CI0105
            14            DU15-GRID.                                    CI0105
            15            DU15-GRIDC  PICTURE  9(3).                    CI0105
            15            DU15-GRIDN.                                   CI0105
            16            DU15-GRIDNP PICTURE  99.                      CI0105
            16            DU15-GRIDND PICTURE  9(8).                    CI0105
            12            DU15-GECKD  PICTURE  9.                       CI0105
            12            DU15-GEMDA  PICTURE  9(8).                    CI0105
            12            DU15-NSEQ4B PICTURE  9(8)                     CI0105
                          BINARY.                                       CI0105
            12            DU15-GRDOR  PICTURE  9(8).                    CI0105
            12            DU15-GRIAD  PICTURE  9(8).                    CI0105
            12            DU15-GECUC  PICTURE  99.                      CI0105
            12            DU15-GRLNG  PICTURE  99.                      CI0105
            12            DU15-GESLC  PICTURE  99.                      CI0105
            12            DU15-AYSIDA PICTURE  9(3).                    CI0105
            12            DU15-AYSID  PICTURE  9(5).                    CI0105
            12            DU15-GRCSD  PICTURE  9(8).                    CI0105
            12            DU15-GRCFD  PICTURE  9(8).                    CI0105
            12            DU15-GRNCL  PICTURE  S9(5)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            12            DU15-GRNCT  PICTURE  S9(5)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            12            DU15-GRSFC  PICTURE  99.                      CI0105
            12            DU15-GRCRN  PICTURE  9(3).                    CI0105
            12            DU15-GRCSS  PICTURE  X.                       CI0105
            12            DU15-MKSRC  PICTURE  99                       CI0105
                          OCCURS       010     TIMES.                   CI0105
            12            DU15-NEFPS  PICTURE  X(5).                    CI0105
            12            DU15-DEFPS  PICTURE  9(8).                    CI0105
            12            DU15-DLSRV  PICTURE  9(8).                    CI0105
            12            DU15-CTLNI  PICTURE  X.                       CI0105
            12            DU15-CGRLI  PICTURE  X.                       CI0105
            12            DU15-CAMGR  PICTURE  9(5)                     CI0105
                          COMPUTATIONAL-3.                              CI0105
            12            DU15-CAMGS  PICTURE  9(5)                     CI0105
                          COMPUTATIONAL-3.                              CI0105
            12            DU15-CAMGN  PICTURE  9(3)                     CI0105
                          COMPUTATIONAL-3.                              CI0105
            12            DU15-CGRMF  PICTURE  X.                       CI0105
            12            DU15-FILLER PICTURE  X(08).                   CI0105
            11            DU15-GR07.                                    CI0105
            12            DU15-GEDLA  PICTURE  9(8).                    CI0105
            12            DU15-GRAID  PICTURE  X(12).                   CI0105
            12            DU15-GRPAP  PICTURE  X(14).                   CI0105
            12            DU15-GEPHNX PICTURE  9(4).                    CI0105
            12            DU15-DPLEF  PICTURE  9(8).                    CI0105
            12            DU15-DPLAM  PICTURE  9(8).                    CI0105
            12            DU15-NCPFN  PICTURE  9(6).                    CI0105
            12            DU15-GEFYE  PICTURE  9(4).                    CI0105
            12            DU15-FILLER PICTURE  X(06).                   CI0105
            12            DU15-GRPAN  PICTURE  X(45).                   CI0105
            12            DU15-CGRPA  PICTURE  99.                      CI0105
            12            DU15-IPRTT7 PICTURE  X.                       CI0105
            12            DU15-GRPED  PICTURE  9(8).                    CI0105
            12            DU15-FILLER PICTURE  X(05).                   CI0105
            12            DU15-GRPLC  PICTURE  99.                      CI0105
            12            DU15-GRPLT  PICTURE  99.                      CI0105
            12            DU15-FILLER PICTURE  X(04).                   CI0105
            12            DU15-GEADI  PICTURE  X.                       CI0105
            12            DU15-GRCFA  PICTURE  S9(11)V99                CI0105
                          COMPUTATIONAL-3.                              CI0105
            12            DU15-GECFY  PICTURE  9(4).                    CI0105
            12            DU15-GECFC  PICTURE  99.                      CI0105
            12            DU15-MEMPL  PICTURE  X(20).                   CI0105
            12            DU15-CAUNIT PICTURE  X(4).                    CI0105
            12            DU15-FILLER PICTURE  X(21).                   CI0105
            12            DU15-GRPPP  PICTURE  999.                     CI0105
            12            DU15-CCORT  PICTURE  9(3).                    CI0105
            12            DU15-CIDRP  PICTURE  99.                      CI0105
            12            DU15-CCDWA  PICTURE  9.                       CI0105
            12            DU15-IERSA  PICTURE  X.                       CI0105
            12            DU15-DERSA  PICTURE  9(8).                    CI0105
            12            DU15-FILLER PICTURE  X(04).                   CI0105
            10            DU15-QITEM  PICTURE  9(3).                    CI0105
            10            DU15-XIMAX  PICTURE  S9(4)                    CI0105
                          BINARY.                                       CI0105
            10            DU15-FILLER PICTURE  X(100).                  CI0105
       01                 DU30.                                         CI0105
            10            DU30-C199.                                    CI0105
            11            DU30-CLID.                                    CI0105
            12            DU30-CLIDO  PICTURE  9(3).                    CI0105
            12            DU30-CLIDN.                                   CI0105
            13            DU30-CLIDNP PICTURE  X(12).                   CI0105
            13            DU30-CLIDND PICTURE  9(8).                    CI0105
            10            DU30-CDEL1  PICTURE  9(3).                    CI0105
            10            DU30-DCACG  PICTURE  9(8).                    CI0105
            10            DU30-NRTSQ1 PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            DU30-FILLER PICTURE  X(100).                  CI0105
            10            DU30-CLORN  PICTURE  X(45).                   CI0105
            10            DU30-CL18.                                    CI0105
            11            DU30-CL18K.                                   CI0105
            12            DU30-NRTSQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            DU30-NTR    PICTURE  9(8).                    CI0105
            11            DU30-GECKD  PICTURE  9.                       CI0105
            11            DU30-GEEND  PICTURE  9(8).                    CI0105
            11            DU30-NPDIN  PICTURE  X(4).                    CI0105
            11            DU30-IRTNA  PICTURE  X.                       CI0105
            11            DU30-IRTNP  PICTURE  X.                       CI0105
            11            DU30-IRTNW  PICTURE  X.                       CI0105
            10            DU30-IBNKI  PICTURE  X.                       CI0105
            10            DU30-FILLER PICTURE  X(100).                  CI0105
      *Used to determine if audit logs have been written.
      *!WI
       01                 FL01-IAIND
                        PICTURE X.                                      CI0105
            88            AUDIT-LOG-WRITTEN    VALUE 'Y'.
            88            NO-AUDIT-LOGS        VALUE SPACE.
       01                 GU00.                                         CI0105
            02            GU01.                                         CI0105
            10            GU01-GELL   PICTURE  9(4)                     CI0105
                          BINARY.                                       CI0105
            10            GU01-GV00.                                    CI0105
            11            GU01-GU01K.                                   CI0105
            12            GU01-CANUMB PICTURE  X(27).                   CI0105
            12            GU01-CREQT  PICTURE  9(04).                   CI0105
            12            GU01-NSEQ2P PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            GU01-DCACG  PICTURE  9(8).                    CI0105
            11            GU01-IDTYP  PICTURE  9.                       CI0105
            11            GU01-CRSTA  PICTURE  X(01).                   CI0105
            11            GU01-CCOPY  PICTURE  9(02).                   CI0105
            11            GU01-MRPID  PICTURE  X(06).                   CI0105
            10            GU01-GV99.                                    CI0105
            11            GU01-FILLER PICTURE  X(347).                  CI0105
            10            GU01-GV01                                     CI0105
                          REDEFINES            GU01-GV99.               CI0105
            11            GU01-GR98.                                    CI0105
            12            GU01-GRID.                                    CI0105
            13            GU01-GRIDC  PICTURE  9(3).                    CI0105
            13            GU01-GRIDN.                                   CI0105
            14            GU01-GRIDNP PICTURE  99.                      CI0105
            14            GU01-GRIDND PICTURE  9(8).                    CI0105
            11            GU01-AYSID  PICTURE  9(5).                    CI0105
            11            GU01-ATID   PICTURE  9(7).                    CI0105
            11            GU01-GEAEN  PICTURE  X(12).                   CI0105
            11            GU01-GEAUN  PICTURE  9(5).                    CI0105
            11            GU01-CBLDG  PICTURE  X.                       CI0105
            11            GU01-CENTT  PICTURE  X.                       CI0105
            10            GU01-GV02                                     CI0105
                          REDEFINES            GU01-GV99.               CI0105
            11            GU01-NARRS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            GU01-CARTY  PICTURE  99.                      CI0105
            11            GU01-NLPGH  PICTURE  99.                      CI0105
            10            GU01-GV04                                     CI0105
                          REDEFINES            GU01-GV99.               CI0105
            11            GU01-CLNMX2 PICTURE  X(60).                   CI0105
            11            GU01-IUGMA  PICTURE  X.                       CI0105
            10            GU01-GV05                                     CI0105
                          REDEFINES            GU01-GV99.               CI0105
            11            GU01-CANUMV PICTURE  X(27).                   CI0105
            11            GU01-CLNMX3 PICTURE  X(60).                   CI0105
            11            GU01-CMINR  PICTURE  9.                       CI0105
            10            GU01-GV06                                     CI0105
                          REDEFINES            GU01-GV99.               CI0105
            11            GU01-C299.                                    CI0105
            12            GU01-CTID.                                    CI0105
            13            GU01-CTIDA  PICTURE  9(3).                    CI0105
            13            GU01-CTIDN.                                   CI0105
            14            GU01-CTIDNP PICTURE  X(13).                   CI0105
            14            GU01-CTIDND PICTURE  9(11).                   CI0105
            11            GU01-CAGRID PICTURE  9(13).                   CI0105
            10            GU01-GV07                                     CI0105
                          REDEFINES            GU01-GV99.               CI0105
            11            GU01-CACLID PICTURE  X(23).                   CI0105
            11            GU01-NSORG  PICTURE  9(3).                    CI0105
            11            GU01-NSM    PICTURE  9(10).                   CI0105
            10            GU01-GV08                                     CI0105
                          REDEFINES            GU01-GV99.               CI0105
            11            GU01-GRIDA.                                   CI0105
            12            GU01-GRIDCB PICTURE  9(3).                    CI0105
            12            GU01-GRIDNB.                                  CI0105
            13            GU01-GRIDNG PICTURE  99.                      CI0105
            13            GU01-GRIDNA PICTURE  9(8).                    CI0105
            11            GU01-AYIDD  PICTURE  9(5).                    CI0105
            11            GU01-ATID1  PICTURE  9(7).                    CI0105
            11            GU01-GEAEN2 PICTURE  X(12).                   CI0105
            11            GU01-GEAUN2 PICTURE  9(5).                    CI0105
            11            GU01-CBLDGA PICTURE  X.                       CI0105
            11            GU01-CVLID1 PICTURE  X.                       CI0105
            11            GU01-TGMSG  PICTURE  X(10).                   CI0105
            11            GU01-CGCRD  PICTURE  X(2).                    CI0105
            10            GU01-GV10                                     CI0105
                          REDEFINES            GU01-GV99.               CI0105
            11            GU01-NPTRN  PICTURE  9(06).                   CI0105
            11            GU01-CPFMT  PICTURE  9(02).                   CI0105
            11            GU01-NTPN   PICTURE  9(02).                   CI0105
            11            GU01-NLPN   PICTURE  9(02).                   CI0105
            11            GU01-NLIN   PICTURE  9(07).                   CI0105
            11            GU01-CAIF   PICTURE  9(02).                   CI0105
            11            GU01-XZ23.                                    CI0105
            12            GU01-GECSQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            12            GU01-CKPMM  PICTURE  X.                       CI0105
            12            GU01-CTTLC  PICTURE  X.                       CI0105
            12            GU01-XZ19   PICTURE  X(19).                   CI0105
            11            GU01-DPSUM  PICTURE  X(18).                   CI0105
            11            GU01-IPIRA  PICTURE  X(1).                    CI0105
            11            GU01-CPVER  PICTURE  9(1).                    CI0105
            11            GU01-QPMXL  PICTURE  9(03).                   CI0105
            11            GU01-PRCOD3 PICTURE  9(5).                    CI0105
            11            GU01-PRSCD  PICTURE  X(9).                    CI0105
            11            GU01-CFING  PICTURE  X.                       CI0105
            11            GU01-IQACU  PICTURE  X.                       CI0105
            11            GU01-IADIA  PICTURE  X(01).                   CI0105
            11            GU01-IADIB  PICTURE  X(01).                   CI0105
            11            GU01-IADIC  PICTURE  X(01).                   CI0105
            11            GU01-IADID  PICTURE  X(01).                   CI0105
            11            GU01-IADIE  PICTURE  X(01).                   CI0105
            11            GU01-IADIF  PICTURE  X(01).                   CI0105
            11            GU01-FILLER PICTURE  X(258).                  CI0105
            10            GU01-GV11                                     CI0105
                          REDEFINES            GU01-GV99.               CI0105
            11            GU01-GEAEN1 PICTURE  X(12).                   CI0105
            11            GU01-GEAUN1 PICTURE  9(5).                    CI0105
            11            GU01-MEMPL  PICTURE  X(20).                   CI0105
            11            GU01-CSHRC  PICTURE  9(03).                   CI0105
            11            GU01-ISHVR  PICTURE  X(1).                    CI0105
            11            GU01-CSHTC  PICTURE  X(2).                    CI0105
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
                                                                        AM0019
      ******************************************************************AM0019
      **     PCB ADDRESS LIST FOR CI0019.  MODULE CI0019 WILL NEED     *AM0019
      **     PCB'S FOR:                                                *AM0019
      **                CONTRACT DATABASE(CT1P)                        *AM0019
      **                GROUP DATABASE(GR1P)                           *AM0019
      ******************************************************************AM0019
                                                                        AM0019
       01  CI0019B-PCB-ADDRESS-LIST.                                    AM0019
           05  CI0019B-PCB-CT1P-PTR1      POINTER.                      AM0019
           05  CI0019B-PCB-GR1P-PTR1      POINTER.                      AM0019
                                                                        AM0025
      ******************************************************************AM0025
      **     PCB ADDRESS LIST FOR CI0025.  MODULE CI0025 WILL NEED     *AM0025
      **     PCB'S FOR:                                                *AM0025
      **                CLIENT DATABASE(CL1P)                          *AM0025
      ******************************************************************AM0025
                                                                        AM0025
       01  CI0025B-PCB-ADDRESS-LIST.                                    AM0025
           05  CI0025B-PCB-CL1P-PTR1      POINTER.                      AM0025
      *Copy segments used for audit logs
      *!WF DSP=SV DSL=CX SEL=091821 FOR=I DES=1 LEV=1
      * PLT=SV
       01                 SV09.                                         CI0105
            10            SV09-CX09K.                                   CI0105
            11            SV09-NPAIS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            SV09-CDEL1  PICTURE  9(3).                    CI0105
            10            SV09-NDELS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            SV09-CDEST  PICTURE  99.                      CI0105
            10            SV09-DISUP  PICTURE  9(8).                    CI0105
            10            SV09-CLUPD  PICTURE  9(3).                    CI0105
            10            SV09-DLAUP  PICTURE  9(8).                    CI0105
            10            SV09-GEOPD2 PICTURE  X(8).                    CI0105
            10            SV09-DPCHD  PICTURE  9(8).                    CI0105
            10            SV09-FILLER PICTURE  X(06).                   CI0105
       01                 SV18.                                         CI0105
            10            SV18-CX18K.                                   CI0105
            11            SV18-NBASQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            SV18-NPBN   PICTURE  X(20).                   CI0105
            10            SV18-CCBAT  PICTURE  99.                      CI0105
            10            SV18-DACHP  PICTURE  9(8).                    CI0105
            10            SV18-CSTPRE PICTURE  99.                      CI0105
            10            SV18-C199.                                    CI0105
            11            SV18-CLID.                                    CI0105
            12            SV18-CLIDO  PICTURE  9(3).                    CI0105
            12            SV18-CLIDN.                                   CI0105
            13            SV18-CLIDNP PICTURE  X(12).                   CI0105
            13            SV18-CLIDND PICTURE  9(8).                    CI0105
            10            SV18-MCSIG  PICTURE  X(30).                   CI0105
            10            SV18-CPBNU  PICTURE  X.                       CI0105
            10            SV18-CSPCR  PICTURE  99.                      CI0105
            10            SV18-DAPCR  PICTURE  9(8).                    CI0105
            10            SV18-FILLER PICTURE  XX.                      CI0105
       01                 SV21.                                         CI0105
            10            SV21-GELL   PICTURE  9(4)                     CI0105
                          BINARY.                                       CI0105
            10            SV21-CZ00.                                    CI0105
            11            SV21-CX21K.                                   CI0105
            12            SV21-CDEL1  PICTURE  9(3).                    CI0105
            12            SV21-NDELS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            SV21-CZ99.                                    CI0105
            11            SV21-FILLER PICTURE  X(165).                  CI0105
            10            SV21-CZ01                                     CI0105
                          REDEFINES            SV21-CZ99.               CI0105
            11            SV21-NBASQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            SV21-GECSQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            SV21-CZ02                                     CI0105
                          REDEFINES            SV21-CZ99.               CI0105
            11            SV21-CPAYE  PICTURE  9(2).                    CI0105
            11            SV21-C199.                                    CI0105
            12            SV21-CLID.                                    CI0105
            13            SV21-CLIDO  PICTURE  9(3).                    CI0105
            13            SV21-CLIDN.                                   CI0105
            14            SV21-CLIDNP PICTURE  X(12).                   CI0105
            14            SV21-CLIDND PICTURE  9(8).                    CI0105
            11            SV21-GECSQ1 PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            SV21-NBASQT PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            SV21-TDELI  PICTURE  X(30).                   CI0105
      ******************************************************************ADUTAB
      **              TABLE TA5B ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA5B-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=5B FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA5B.                                                CI0105
           04    G-TA5B-PARAM.                                          CI0105
             10  G-TA5B-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0105
                        VALUE      +154.                                CI0105
             10  G-TA5B-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0105
                        VALUE      +001.                                CI0105
             10  G-TA5B-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0105
                        VALUE      +017.                                CI0105
             10  G-TA5B-NUAPP  PICTURE 99                               CI0105
                        VALUE       0.                                  CI0105
             10  G-TA5B-NUTAB  PICTURE X(6)                             CI0105
                        VALUE 'TA005B'.                                 CI0105
             10  G-TA5B-TABFO  PICTURE XX                 VALUE SPACE.  CI0105
             10  G-TA5B-TABCR  PICTURE XX                 VALUE SPACE.  CI0105
             10  G-TA5B-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0105
             10  G-TA5B-NUSSC  PICTURE X  VALUE   ' '.                  CI0105
             10  G-TA5B-NUSSY  PICTURE X                  VALUE SPACE.  CI0105
             10  G-TA5B-TRANID PICTURE X(4)               VALUE SPACE.  CI0105
             10  G-TA5B-FILSYS.                                         CI0105
             15  G-TA5B-USERC  PICTURE X(6)               VALUE SPACE.  CI0105
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0105
           04             TA5B.                                         CI0105
            10            TA5B-GAPSC.                                   CI0105
            11            TA5B-CTIDA  PICTURE  9(3)                     CI0105
                          VALUE                ZERO.                    CI0105
            11            TA5B-PRCOD  PICTURE  9(5)                     CI0105
                          VALUE                ZERO.                    CI0105
            11            TA5B-PRSCD  PICTURE  X(9)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-PRCODX PICTURE  9(5)                     CI0105
                          VALUE                ZERO.                    CI0105
            10            TA5B-PRCSUB PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-PRCAUT PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-PRCBAS PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-PRCSTK PICTURE  XX                       CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-PRCPRE PICTURE  X(4)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-IBDUP  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-IUSPR  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-CVSYS  PICTURE  X(2)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-IDTOD  PICTURE  X(1)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-GRSFC  PICTURE  99                       CI0105
                          VALUE                ZERO.                    CI0105
            10            TA5B-ZDA18  PICTURE  X(18)                    CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-CMPCTB PICTURE  X(4)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-ITERM  PICTURE  X(1)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-AMFAC  PICTURE  S9(7)                    CI0105
                          VALUE                ZERO.                    CI0105
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-CPRBK  PICTURE  X(3)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-CFXDM  PICTURE  99                       CI0105
                          VALUE                ZERO.                    CI0105
            10            TA5B-NGLCS  PICTURE  X(5)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-NDFCS  PICTURE  X(5)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-CTNLI  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-CBANK  PICTURE  X(03)                    CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-ISYPO  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-ISYPP  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-ICOPT  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-IANPY  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-IDSAR  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-ICIPT  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-IANDS  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-IKPMA  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-INMWT  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-IVANT  PICTURE  X(1)                     CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-ISDAV  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-IUDAV  PICTURE  X                        CI0105
                          VALUE                SPACE.                   CI0105
            10            TA5B-ZDA15  PICTURE  X(15)                    CI0105
                          VALUE                SPACE.                   CI0105
      **                                                                ADUTAB
      *Audit log after segment
      *!WF DSP=VA DSL=VA SEL=04 FOR=I DES=1 LEV=1 PLT=VA
       01                 VA04.                                         CI0105
            10            VA04-NARRS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            VA04-CARTY  PICTURE  99.                      CI0105
            10            VA04-DASUP  PICTURE  9(8).                    CI0105
            10            VA04-C299.                                    CI0105
            11            VA04-CTID.                                    CI0105
            12            VA04-CTIDA  PICTURE  9(3).                    CI0105
            12            VA04-CTIDN.                                   CI0105
            13            VA04-CTIDNP PICTURE  X(13).                   CI0105
            13            VA04-CTIDND PICTURE  9(11).                   CI0105
            10            VA04-CDELI  PICTURE  9(3).                    CI0105
            10            VA04-NPBN   PICTURE  X(20).                   CI0105
            10            VA04-CCBAT  PICTURE  99.                      CI0105
            10            VA04-CLID4.                                   CI0105
            11            VA04-C199.                                    CI0105
            12            VA04-CLID.                                    CI0105
            13            VA04-CLIDO  PICTURE  9(3).                    CI0105
            13            VA04-CLIDN.                                   CI0105
            14            VA04-CLIDNP PICTURE  X(12).                   CI0105
            14            VA04-CLIDND PICTURE  9(8).                    CI0105
            10            VA04-MCSIG  PICTURE  X(30).                   CI0105
            10            VA04-CTSTA  PICTURE  99.                      CI0105
            10            VA04-CDEST  PICTURE  99.                      CI0105
      *Audit log before segment
      *!WF DSP=VB DSL=VA SEL=04 FOR=I DES=1 LEV=1 PLT=VB
       01                 VB04.                                         CI0105
            10            VB04-NARRS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            VB04-CARTY  PICTURE  99.                      CI0105
            10            VB04-DASUP  PICTURE  9(8).                    CI0105
            10            VB04-C299.                                    CI0105
            11            VB04-CTID.                                    CI0105
            12            VB04-CTIDA  PICTURE  9(3).                    CI0105
            12            VB04-CTIDN.                                   CI0105
            13            VB04-CTIDNP PICTURE  X(13).                   CI0105
            13            VB04-CTIDND PICTURE  9(11).                   CI0105
            10            VB04-CDELI  PICTURE  9(3).                    CI0105
            10            VB04-NPBN   PICTURE  X(20).                   CI0105
            10            VB04-CCBAT  PICTURE  99.                      CI0105
            10            VB04-CLID4.                                   CI0105
            11            VB04-C199.                                    CI0105
            12            VB04-CLID.                                    CI0105
            13            VB04-CLIDO  PICTURE  9(3).                    CI0105
            13            VB04-CLIDN.                                   CI0105
            14            VB04-CLIDNP PICTURE  X(12).                   CI0105
            14            VB04-CLIDND PICTURE  9(8).                    CI0105
            10            VB04-MCSIG  PICTURE  X(30).                   CI0105
            10            VB04-CTSTA  PICTURE  99.                      CI0105
            10            VB04-CDEST  PICTURE  99.                      CI0105
      *Group Copy segments used for results from CI0019
      *!WF DSP=WG DSL=GR SEL=0107 FOR=I DES=1 LEV=1
      * PLT=WG
       01                 WG01.                                         CI0105
            10            WG01-GR01K.                                   CI0105
            11            WG01-GR98.                                    CI0105
            12            WG01-GRID.                                    CI0105
            13            WG01-GRIDC  PICTURE  9(3).                    CI0105
            13            WG01-GRIDN.                                   CI0105
            14            WG01-GRIDNP PICTURE  99.                      CI0105
            14            WG01-GRIDND PICTURE  9(8).                    CI0105
            10            WG01-GECKD  PICTURE  9.                       CI0105
            10            WG01-GEMDA  PICTURE  9(8).                    CI0105
            10            WG01-NSEQ4B PICTURE  9(8)                     CI0105
                          BINARY.                                       CI0105
            10            WG01-GRDOR  PICTURE  9(8).                    CI0105
            10            WG01-GRIAD  PICTURE  9(8).                    CI0105
            10            WG01-GECUC  PICTURE  99.                      CI0105
            10            WG01-GRLNG  PICTURE  99.                      CI0105
            10            WG01-GESLC  PICTURE  99.                      CI0105
            10            WG01-AYSIDA PICTURE  9(3).                    CI0105
            10            WG01-AYSID  PICTURE  9(5).                    CI0105
            10            WG01-GRCSD  PICTURE  9(8).                    CI0105
            10            WG01-GRCFD  PICTURE  9(8).                    CI0105
            10            WG01-GRNCL  PICTURE  S9(5)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            WG01-GRNCT  PICTURE  S9(5)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            WG01-GRSFC  PICTURE  99.                      CI0105
            10            WG01-GRCRN  PICTURE  9(3).                    CI0105
            10            WG01-GRCSS  PICTURE  X.                       CI0105
            10            WG01-MKSRC  PICTURE  99                       CI0105
                          OCCURS       010     TIMES.                   CI0105
            10            WG01-NEFPS  PICTURE  X(5).                    CI0105
            10            WG01-DEFPS  PICTURE  9(8).                    CI0105
            10            WG01-DLSRV  PICTURE  9(8).                    CI0105
            10            WG01-CTLNI  PICTURE  X.                       CI0105
            10            WG01-CGRLI  PICTURE  X.                       CI0105
            10            WG01-CAMGR  PICTURE  9(5)                     CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            WG01-CAMGS  PICTURE  9(5)                     CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            WG01-CAMGN  PICTURE  9(3)                     CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            WG01-CGRMF  PICTURE  X.                       CI0105
            10            WG01-FILLER PICTURE  X(08).                   CI0105
       01                 WG07.                                         CI0105
            10            WG07-GEDLA  PICTURE  9(8).                    CI0105
            10            WG07-GRAID  PICTURE  X(12).                   CI0105
            10            WG07-GRPAP  PICTURE  X(14).                   CI0105
            10            WG07-GEPHNX PICTURE  9(4).                    CI0105
            10            WG07-DPLEF  PICTURE  9(8).                    CI0105
            10            WG07-DPLAM  PICTURE  9(8).                    CI0105
            10            WG07-NCPFN  PICTURE  9(6).                    CI0105
            10            WG07-GEFYE  PICTURE  9(4).                    CI0105
            10            WG07-FILLER PICTURE  X(06).                   CI0105
            10            WG07-GRPAN  PICTURE  X(45).                   CI0105
            10            WG07-CGRPA  PICTURE  99.                      CI0105
            10            WG07-IPRTT7 PICTURE  X.                       CI0105
            10            WG07-GRPED  PICTURE  9(8).                    CI0105
            10            WG07-FILLER PICTURE  X(05).                   CI0105
            10            WG07-GRPLC  PICTURE  99.                      CI0105
            10            WG07-GRPLT  PICTURE  99.                      CI0105
            10            WG07-FILLER PICTURE  X(04).                   CI0105
            10            WG07-GEADI  PICTURE  X.                       CI0105
            10            WG07-GRCFA  PICTURE  S9(11)V99                CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            WG07-GECFY  PICTURE  9(4).                    CI0105
            10            WG07-GECFC  PICTURE  99.                      CI0105
            10            WG07-MEMPL  PICTURE  X(20).                   CI0105
            10            WG07-CAUNIT PICTURE  X(4).                    CI0105
            10            WG07-FILLER PICTURE  X(21).                   CI0105
            10            WG07-GRPPP  PICTURE  999.                     CI0105
            10            WG07-CCORT  PICTURE  9(3).                    CI0105
            10            WG07-CIDRP  PICTURE  99.                      CI0105
            10            WG07-CCDWA  PICTURE  9.                       CI0105
            10            WG07-IERSA  PICTURE  X.                       CI0105
            10            WG07-DERSA  PICTURE  9(8).                    CI0105
            10            WG07-FILLER PICTURE  X(04).                   CI0105
      *General W/S items
       01                 WS01-UPDCNT    PIC 99.
      *!WI
       01                 WS01-GRPLC
                        PICTURE 99.                                     CI0105
      *Contract Copy segments used for results from CI0019
      *!WF DSP=WT DSL=CT SEL=10 FOR=I DES=1 LEV=1 PLT=WT
       01                 WT10.                                         CI0105
            10            WT10-CT10K.                                   CI0105
            11            WT10-GR98.                                    CI0105
            12            WT10-GRID.                                    CI0105
            13            WT10-GRIDC  PICTURE  9(3).                    CI0105
            13            WT10-GRIDN.                                   CI0105
            14            WT10-GRIDNP PICTURE  99.                      CI0105
            14            WT10-GRIDND PICTURE  9(8).                    CI0105
            10            WT10-GR97                                     CI0105
                          REDEFINES            WT10-CT10K.              CI0105
            11            WT10-GRIDCB PICTURE  9(3).                    CI0105
            11            WT10-FILLER PICTURE  X(10).                   CI0105
            10            WT10-GERSD  PICTURE  9(8).                    CI0105
            10            WT10-GERED  PICTURE  9(8).                    CI0105
            10            WT10-GRCSI  PICTURE  X.                       CI0105
      *Working storage copy segments
      *!WF DSP=WX DSL=CX SEL=1821 FOR=I DES=1 LEV=1
      * PLT=WX
       01                 WX18.                                         CI0105
            10            WX18-CX18K.                                   CI0105
            11            WX18-NBASQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            WX18-NPBN   PICTURE  X(20).                   CI0105
            10            WX18-CCBAT  PICTURE  99.                      CI0105
            10            WX18-DACHP  PICTURE  9(8).                    CI0105
            10            WX18-CSTPRE PICTURE  99.                      CI0105
            10            WX18-C199.                                    CI0105
            11            WX18-CLID.                                    CI0105
            12            WX18-CLIDO  PICTURE  9(3).                    CI0105
            12            WX18-CLIDN.                                   CI0105
            13            WX18-CLIDNP PICTURE  X(12).                   CI0105
            13            WX18-CLIDND PICTURE  9(8).                    CI0105
            10            WX18-MCSIG  PICTURE  X(30).                   CI0105
            10            WX18-CPBNU  PICTURE  X.                       CI0105
            10            WX18-CSPCR  PICTURE  99.                      CI0105
            10            WX18-DAPCR  PICTURE  9(8).                    CI0105
            10            WX18-FILLER PICTURE  XX.                      CI0105
       01                 WX21.                                         CI0105
            10            WX21-GELL   PICTURE  9(4)                     CI0105
                          BINARY.                                       CI0105
            10            WX21-CZ00.                                    CI0105
            11            WX21-CX21K.                                   CI0105
            12            WX21-CDEL1  PICTURE  9(3).                    CI0105
            12            WX21-NDELS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            WX21-CZ99.                                    CI0105
            11            WX21-FILLER PICTURE  X(165).                  CI0105
            10            WX21-CZ01                                     CI0105
                          REDEFINES            WX21-CZ99.               CI0105
            11            WX21-NBASQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            WX21-GECSQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            WX21-CZ02                                     CI0105
                          REDEFINES            WX21-CZ99.               CI0105
            11            WX21-CPAYE  PICTURE  9(2).                    CI0105
            11            WX21-C199.                                    CI0105
            12            WX21-CLID.                                    CI0105
            13            WX21-CLIDO  PICTURE  9(3).                    CI0105
            13            WX21-CLIDN.                                   CI0105
            14            WX21-CLIDNP PICTURE  X(12).                   CI0105
            14            WX21-CLIDND PICTURE  9(8).                    CI0105
            11            WX21-GECSQ1 PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            WX21-NBASQT PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            WX21-TDELI  PICTURE  X(30).                   CI0105
       01   DEBUT-WSS.                                                  CI0105
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0105
            05   IK     PICTURE X.                                      CI0105
       01  CONSTANTES-PAC.                                              CI0105
           05  FILLER  PICTURE X(87)   VALUE                            CI0105
                     '6015 CAT09/08/14CI0105ADMIN   14:34:47CI0105P AMERCI0105
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0105
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0105
           05  NUGNA   PICTURE X(5).                                    CI0105
           05  APPLI   PICTURE X(3).                                    CI0105
           05  DATGN   PICTURE X(8).                                    CI0105
           05  PROGR   PICTURE X(6).                                    CI0105
           05  CODUTI  PICTURE X(8).                                    CI0105
           05  TIMGN   PICTURE X(8).                                    CI0105
           05  PROGE   PICTURE X(8).                                    CI0105
           05  COBASE  PICTURE X(4).                                    CI0105
           05  DATGNC  PICTURE X(10).                                   CI0105
           05  RELEAS  PICTURE X(7).                                    CI0105
           05  DATGE   PICTURE X(10).                                   CI0105
           05  DATSQ   PICTURE X(10).                                   CI0105
       01  DATCE.                                                       CI0105
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0105
         05  DATOR.                                                     CI0105
           10  DATOA  PICTURE XX.                                       CI0105
           10  DATOM  PICTURE XX.                                       CI0105
           10  DATOJ  PICTURE XX.                                       CI0105
       01   VARIABLES-CONDITIONNELLES.                                  CI0105
            05                  FT      PICTURE X VALUE '0'.            CI0105
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0105
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0105
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J45CBR PICTURE S9(4) VALUE  ZERO.
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0105
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0105
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0105
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0105
            05       5-DI00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0105
            05       5-GU00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0105
       01               S-CL01-SSA.                                     CI0105
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0105
                                      VALUE 'CL01    '.                 CI0105
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0105
            10          S-CL01-CCOD   PICTURE X(5)                      CI0105
                                      VALUE '-----'.                    CI0105
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0105
       01            S-CLU01-SSA.                                       CI0105
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CL01    '.                 CI0105
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0105
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CL01K'.                   CI0105
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0105
            10       S-CLU01-CL01K.                                     CI0105
            11       S-CLU01-C199.                                      CI0105
            12       S-CLU01-CLID.                                      CI0105
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0105
            13       S-CLU01-CLIDN.                                     CI0105
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0105
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0105
            10  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01               S-CL24-SSA.                                     CI0105
            10         S1-CL24-SEGNAM PICTURE X(8)                      CI0105
                                      VALUE 'CL24    '.                 CI0105
            10         S1-CL24-CCOM   PICTURE X VALUE '*'.              CI0105
            10          S-CL24-CCOD   PICTURE X(5)                      CI0105
                                      VALUE '-----'.                    CI0105
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0105
       01            S-CLA24-SSA.                                       CI0105
            10      S1-CLA24-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CL24    '.                 CI0105
            10      S1-CLA24-CCOM   PICTURE X VALUE '*'.                CI0105
            10       S-CLA24-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            10      S1-CLA24-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(GECED'.                   CI0105
            10       S-CLA24-OPER  PICTURE XX VALUE ' ='.               CI0105
            10       S-CLA24-GECED    PICTURE  9(8).                    CI0105
            10  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CLB24-SSA.                                       CI0105
            10      S1-CLB24-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CL24    '.                 CI0105
            10      S1-CLB24-CCOM   PICTURE X VALUE '*'.                CI0105
            10       S-CLB24-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            10      S1-CLB24-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(IRESA'.                   CI0105
            10       S-CLB24-OPER  PICTURE XX VALUE ' ='.               CI0105
            10       S-CLB24-IRESA    PICTURE  X.                       CI0105
            10  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CLC24-SSA.                                       CI0105
            10      S1-CLC24-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CL24    '.                 CI0105
            10      S1-CLC24-CCOM   PICTURE X VALUE '*'.                CI0105
            10       S-CLC24-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            10      S1-CLC24-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(GEADS'.                   CI0105
            10       S-CLC24-OPER  PICTURE XX VALUE ' ='.               CI0105
            10       S-CLC24-GEADS    PICTURE  9.                       CI0105
            10  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CLD24-SSA.                                       CI0105
            10      S1-CLD24-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CL24    '.                 CI0105
            10      S1-CLD24-CCOM   PICTURE X VALUE '*'.                CI0105
            10       S-CLD24-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            10      S1-CLD24-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(GEST'.                    CI0105
            10       S-CLD24-OPER  PICTURE XX VALUE ' ='.               CI0105
            10       S-CLD24-GEST     PICTURE  X(8).                    CI0105
            10  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CLU24-SSA.                                       CI0105
            10      S1-CLU24-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CL24    '.                 CI0105
            10      S1-CLU24-CCOM   PICTURE X VALUE '*'.                CI0105
            10       S-CLU24-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            10      S1-CLU24-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CL24K'.                   CI0105
            10       S-CLU24-OPER  PICTURE XX VALUE ' ='.               CI0105
            10       S-CLU24-CL24K.                                     CI0105
            11       S-CLU24-GECSQ    PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01               S-CT01-SSA.                                     CI0105
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0105
                                      VALUE 'CT01    '.                 CI0105
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0105
            10          S-CT01-CCOD   PICTURE X(5)                      CI0105
                                      VALUE '-----'.                    CI0105
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0105
       01            S-CTU01-SSA.                                       CI0105
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CT01    '.                 CI0105
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0105
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CT01K'.                   CI0105
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0105
            10       S-CTU01-CT01K.                                     CI0105
            11       S-CTU01-C299.                                      CI0105
            12       S-CTU01-CTID.                                      CI0105
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0105
            13       S-CTU01-CTIDN.                                     CI0105
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0105
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0105
            10  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01               S-CX01-SSA.                                     CI0105
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0105
                                      VALUE 'CX01    '.                 CI0105
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0105
            10          S-CX01-CCOD   PICTURE X(5)                      CI0105
                                      VALUE '-----'.                    CI0105
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0105
       01            S-CXU01-SSA.                                       CI0105
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX01    '.                 CI0105
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0105
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CX01K'.                   CI0105
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0105
            10       S-CXU01-CX01K.                                     CI0105
            11       S-CXU01-C199.                                      CI0105
            12       S-CXU01-CLID.                                      CI0105
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0105
            13       S-CXU01-CLIDN.                                     CI0105
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0105
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0105
            10  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01               S-CX03-SSA.                                     CI0105
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0105
                                      VALUE 'CX03    '.                 CI0105
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0105
            10          S-CX03-CCOD   PICTURE X(5)                      CI0105
                                      VALUE '-----'.                    CI0105
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0105
       01            S-CXA03-SSA.                                       CI0105
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX03    '.                 CI0105
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0105
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CARTY'.                   CI0105
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0105
            12       S-CXA03-CARTY    PICTURE  99.                      CI0105
            12  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CXB03-SSA.                                       CI0105
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX03    '.                 CI0105
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0105
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(NARRS'.                   CI0105
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0105
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            12  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CXC03-SSA.                                       CI0105
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX03    '.                 CI0105
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CPMTG'.                   CI0105
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CXD03-SSA.                                       CI0105
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX03    '.                 CI0105
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(GRCRNG'.                  CI0105
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CXE03-SSA.                                       CI0105
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX03    '.                 CI0105
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(DEXDT'.                   CI0105
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CXF03-SSA.                                       CI0105
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX03    '.                 CI0105
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CY50'.                    CI0105
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-CXF03-CY50.                                      CI0105
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CXG03-SSA.                                       CI0105
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX03    '.                 CI0105
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(NBASQ'.                   CI0105
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CXH03-SSA.                                       CI0105
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX03    '.                 CI0105
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0105
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(NARID'.                   CI0105
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0105
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0105
            12  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CXU03-SSA.                                       CI0105
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX03    '.                 CI0105
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CX03K'.                   CI0105
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-CXU03-CX03K.                                     CI0105
            12       S-CXU03-CARTY    PICTURE  99.                      CI0105
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01               S-CX06-SSA.                                     CI0105
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0105
                                      VALUE 'CX06    '.                 CI0105
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0105
            10          S-CX06-CCOD   PICTURE X(5)                      CI0105
                                      VALUE '-----'.                    CI0105
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0105
       01            S-CXU06-SSA.                                       CI0105
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX06    '.                 CI0105
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0105
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CX06K'.                   CI0105
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0105
            10       S-CXU06-CX06K.                                     CI0105
            11       S-CXU06-C299.                                      CI0105
            12       S-CXU06-CTID.                                      CI0105
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0105
            13       S-CXU06-CTIDN.                                     CI0105
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0105
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0105
            10  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01               S-CX09-SSA.                                     CI0105
            10         S1-CX09-SEGNAM PICTURE X(8)                      CI0105
                                      VALUE 'CX09    '.                 CI0105
            10         S1-CX09-CCOM   PICTURE X VALUE '*'.              CI0105
            10          S-CX09-CCOD   PICTURE X(5)                      CI0105
                                      VALUE '-----'.                    CI0105
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0105
       01            S-CXU09-SSA.                                       CI0105
            10      S1-CXU09-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX09    '.                 CI0105
            10      S1-CXU09-CCOM   PICTURE X VALUE '*'.                CI0105
            10       S-CXU09-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            10      S1-CXU09-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CX09K'.                   CI0105
            10       S-CXU09-OPER  PICTURE XX VALUE ' ='.               CI0105
            10       S-CXU09-CX09K.                                     CI0105
            11       S-CXU09-NPAIS    PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01               S-CX18-SSA.                                     CI0105
            10         S1-CX18-SEGNAM PICTURE X(8)                      CI0105
                                      VALUE 'CX18    '.                 CI0105
            10         S1-CX18-CCOM   PICTURE X VALUE '*'.              CI0105
            10          S-CX18-CCOD   PICTURE X(5)                      CI0105
                                      VALUE '-----'.                    CI0105
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0105
       01            S-CXA18-SSA.                                       CI0105
            10      S1-CXA18-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX18    '.                 CI0105
            10      S1-CXA18-CCOM   PICTURE X VALUE '*'.                CI0105
            10       S-CXA18-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            10      S1-CXA18-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CSTPRE'.                  CI0105
            10       S-CXA18-OPER  PICTURE XX VALUE ' ='.               CI0105
            10       S-CXA18-CSTPRE   PICTURE  99.                      CI0105
            10  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CXB18-SSA.                                       CI0105
            10      S1-CXB18-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX18    '.                 CI0105
            10      S1-CXB18-CCOM   PICTURE X VALUE '*'.                CI0105
            10       S-CXB18-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            10      S1-CXB18-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CSPCR'.                   CI0105
            10       S-CXB18-OPER  PICTURE XX VALUE ' ='.               CI0105
            10       S-CXB18-CSPCR    PICTURE  99.                      CI0105
            10  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CXU18-SSA.                                       CI0105
            10      S1-CXU18-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX18    '.                 CI0105
            10      S1-CXU18-CCOM   PICTURE X VALUE '*'.                CI0105
            10       S-CXU18-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            10      S1-CXU18-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CX18K'.                   CI0105
            10       S-CXU18-OPER  PICTURE XX VALUE ' ='.               CI0105
            10       S-CXU18-CX18K.                                     CI0105
            11       S-CXU18-NBASQ    PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01               S-CX19-SSA.                                     CI0105
            10         S1-CX19-SEGNAM PICTURE X(8)                      CI0105
                                      VALUE 'CX19    '.                 CI0105
            10         S1-CX19-CCOM   PICTURE X VALUE '*'.              CI0105
            10          S-CX19-CCOD   PICTURE X(5)                      CI0105
                                      VALUE '-----'.                    CI0105
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0105
       01               S-CX2Y-SSA.                                     CI0105
            10         S1-CX2Y-SEGNAM PICTURE X(8)                      CI0105
                                      VALUE 'CX2Y    '.                 CI0105
            10         S1-CX2Y-CCOM   PICTURE X VALUE '*'.              CI0105
            10          S-CX2Y-CCOD   PICTURE X(5)                      CI0105
                                      VALUE '-----'.                    CI0105
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0105
       01            S-CXA2Y-SSA.                                       CI0105
            11      S1-CXA2Y-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX2Y    '.                 CI0105
            11      S1-CXA2Y-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-CXA2Y-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-CXA2Y-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CARTY'.                   CI0105
            11       S-CXA2Y-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-CXA2Y-CARTY    PICTURE  99.                      CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CXB2Y-SSA.                                       CI0105
            11      S1-CXB2Y-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX2Y    '.                 CI0105
            11      S1-CXB2Y-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-CXB2Y-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-CXB2Y-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(C299'.                    CI0105
            11       S-CXB2Y-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-CXB2Y-C299.                                      CI0105
            12       S-CXB2Y-CTID.                                      CI0105
            13       S-CXB2Y-CTIDA    PICTURE  9(3).                    CI0105
            13       S-CXB2Y-CTIDN.                                     CI0105
            14       S-CXB2Y-CTIDNP   PICTURE  X(13).                   CI0105
            14       S-CXB2Y-CTIDND   PICTURE  9(11).                   CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CXU2Y-SSA.                                       CI0105
            10      S1-CXU2Y-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX2Y    '.                 CI0105
            10      S1-CXU2Y-CCOM   PICTURE X VALUE '*'.                CI0105
            10       S-CXU2Y-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            10      S1-CXU2Y-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CX2YK'.                   CI0105
            10       S-CXU2Y-OPER  PICTURE XX VALUE ' ='.               CI0105
            10       S-CXU2Y-CX2YK.                                     CI0105
            11       S-CXU2Y-C299.                                      CI0105
            12       S-CXU2Y-CTID.                                      CI0105
            13       S-CXU2Y-CTIDA    PICTURE  9(3).                    CI0105
            13       S-CXU2Y-CTIDN.                                     CI0105
            14       S-CXU2Y-CTIDNP   PICTURE  X(13).                   CI0105
            14       S-CXU2Y-CTIDND   PICTURE  9(11).                   CI0105
            11       S-CXU2Y-C199.                                      CI0105
            12       S-CXU2Y-CLID.                                      CI0105
            13       S-CXU2Y-CLIDO    PICTURE  9(3).                    CI0105
            13       S-CXU2Y-CLIDN.                                     CI0105
            14       S-CXU2Y-CLIDNP   PICTURE  X(12).                   CI0105
            14       S-CXU2Y-CLIDND   PICTURE  9(8).                    CI0105
            11       S-CXU2Y-CARTY    PICTURE  99.                      CI0105
            11       S-CXU2Y-NARRS    PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01               S-CX21-SSA.                                     CI0105
            10         S1-CX21-SEGNAM PICTURE X(8)                      CI0105
                                      VALUE 'CX21    '.                 CI0105
            10         S1-CX21-CCOM   PICTURE X VALUE '*'.              CI0105
            10          S-CX21-CCOD   PICTURE X(5)                      CI0105
                                      VALUE '-----'.                    CI0105
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0105
       01            S-CXA21-SSA.                                       CI0105
            11      S1-CXA21-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX21    '.                 CI0105
            11      S1-CXA21-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-CXA21-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-CXA21-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(GECSQ1'.                  CI0105
            11       S-CXA21-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-CXA21-GECSQ1   PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-CXU21-SSA.                                       CI0105
            11      S1-CXU21-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'CX21    '.                 CI0105
            11      S1-CXU21-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-CXU21-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-CXU21-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CX21K'.                   CI0105
            11       S-CXU21-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-CXU21-CX21K.                                     CI0105
            12       S-CXU21-CDEL1    PICTURE  9(3).                    CI0105
            12       S-CXU21-NDELS    PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01               S-DI01-SSA.                                     CI0105
            10         S1-DI01-SEGNAM PICTURE X(8)                      CI0105
                                      VALUE 'DI01    '.                 CI0105
            10         S1-DI01-CCOM   PICTURE X VALUE '*'.              CI0105
            10          S-DI01-CCOD   PICTURE X(5)                      CI0105
                                      VALUE '-----'.                    CI0105
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0105
       01            S-DIA01-SSA.                                       CI0105
            11      S1-DIA01-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'DI01    '.                 CI0105
            11      S1-DIA01-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-DIA01-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-DIA01-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(GEOPID'.                  CI0105
            11       S-DIA01-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-DIA01-GEOPID   PICTURE  X(6).                    CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-DIB01-SSA.                                       CI0105
            11      S1-DIB01-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'DI01    '.                 CI0105
            11      S1-DIB01-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-DIB01-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-DIB01-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CAUNIT'.                  CI0105
            11       S-DIB01-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-DIB01-CAUNIT   PICTURE  X(4).                    CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-DID01-SSA.                                       CI0105
            12      S1-DID01-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'DI01    '.                 CI0105
            12      S1-DID01-CCOM   PICTURE X VALUE '*'.                CI0105
            12       S-DID01-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            12      S1-DID01-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CADATE'.                  CI0105
            12       S-DID01-OPER  PICTURE XX VALUE ' ='.               CI0105
            12       S-DID01-CADATE   PICTURE  X(8).                    CI0105
            12  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-DIE01-SSA.                                       CI0105
            12      S1-DIE01-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'DI01    '.                 CI0105
            12      S1-DIE01-CCOM   PICTURE X VALUE '*'.                CI0105
            12       S-DIE01-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            12      S1-DIE01-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CSYS'.                    CI0105
            12       S-DIE01-OPER  PICTURE XX VALUE ' ='.               CI0105
            12       S-DIE01-CSYS     PICTURE  X(4).                    CI0105
            12  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-DIF01-SSA.                                       CI0105
            12      S1-DIF01-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'DI01    '.                 CI0105
            12      S1-DIF01-CCOM   PICTURE X VALUE '*'.                CI0105
            12       S-DIF01-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            12      S1-DIF01-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(MAUSB'.                   CI0105
            12       S-DIF01-OPER  PICTURE XX VALUE ' ='.               CI0105
            12       S-DIF01-MAUSB    PICTURE  X(8).                    CI0105
            12  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-DIG01-SSA.                                       CI0105
            11      S1-DIG01-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'DI01    '.                 CI0105
            11      S1-DIG01-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-DIG01-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-DIG01-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(NAUSK'.                   CI0105
            11       S-DIG01-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-DIG01-NAUSK    PICTURE  X(50).                   CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-DIU01-SSA.                                       CI0105
            11      S1-DIU01-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'DI01    '.                 CI0105
            11      S1-DIU01-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-DIU01-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-DIU01-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(DI01K'.                   CI0105
            11       S-DIU01-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-DIU01-DI01K.                                     CI0105
            12       S-DIU01-MAUSB    PICTURE  X(8).                    CI0105
            12       S-DIU01-CSYS     PICTURE  X(4).                    CI0105
            12       S-DIU01-CAPPL    PICTURE  X(8).                    CI0105
            12       S-DIU01-CADATE   PICTURE  X(8).                    CI0105
            12       S-DIU01-GETIM    PICTURE  S9(7)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            12       S-DIU01-XTERMI   PICTURE  X(08).                   CI0105
            12       S-DIU01-NSEQ2P   PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-DI101-SSA.                                       CI0105
            11      S1-DI101-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'DI01    '.                 CI0105
            11      S1-DI101-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-DI101-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-DI101-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(XCAUFR'.                  CI0105
            11       S-DI101-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-DI101-CAUFR    PICTURE  S9(5)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-DI201-SSA.                                       CI0105
            11      S1-DI201-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'DI01    '.                 CI0105
            11      S1-DI201-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-DI201-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-DI201-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(XCAUAC'.                  CI0105
            11       S-DI201-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-DI201-CAUAC    PICTURE  S9(5)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01               S-GU01-SSA.                                     CI0105
            10         S1-GU01-SEGNAM PICTURE X(8)                      CI0105
                                      VALUE 'GU01    '.                 CI0105
            10         S1-GU01-CCOM   PICTURE X VALUE '*'.              CI0105
            10          S-GU01-CCOD   PICTURE X(5)                      CI0105
                                      VALUE '-----'.                    CI0105
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0105
       01            S-GUG01-SSA.                                       CI0105
            12      S1-GUG01-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'GU01    '.                 CI0105
            12      S1-GUG01-CCOM   PICTURE X VALUE '*'.                CI0105
            12       S-GUG01-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            12      S1-GUG01-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(CANUMB'.                  CI0105
            12       S-GUG01-OPER  PICTURE XX VALUE ' ='.               CI0105
            12       S-GUG01-CANUMB   PICTURE  X(27).                   CI0105
            12  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-GUH01-SSA.                                       CI0105
            11      S1-GUH01-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'GU01    '.                 CI0105
            11      S1-GUH01-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-GUH01-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-GUH01-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(IDTYP'.                   CI0105
            11       S-GUH01-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-GUH01-IDTYP    PICTURE  9.                       CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01            S-GUU01-SSA.                                       CI0105
            11      S1-GUU01-SEGNAM PICTURE X(8)                        CI0105
                                      VALUE 'GU01    '.                 CI0105
            11      S1-GUU01-CCOM   PICTURE X VALUE '*'.                CI0105
            11       S-GUU01-CCOD   PICTURE X(5)                        CI0105
                                      VALUE '-----'.                    CI0105
            11      S1-GUU01-FLDNAM PICTURE X(9)                        CI0105
                                      VALUE '(GU01K'.                   CI0105
            11       S-GUU01-OPER  PICTURE XX VALUE ' ='.               CI0105
            11       S-GUU01-GU01K.                                     CI0105
            12       S-GUU01-CANUMB   PICTURE  X(27).                   CI0105
            12       S-GUU01-CREQT    PICTURE  9(04).                   CI0105
            12       S-GUU01-NSEQ2P   PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11  FILLER   PICTURE X    VALUE ')'.                        CI0105
       01   ZONES-UTILISATEUR PICTURE X.                                CI0105
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
      ** PCB POINTER FOR ARAY                                           ADU015
            05 PCB-ARAY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR DATP                                           ADU015
            05 PCB-DATP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AGCP                                           ADU015
            05 PCB-AGCP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR GR1P                                           ADU015
            05 PCB-GR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0105
          05              PA00-SUITE.                                   CI0105
            15       FILLER         PICTURE  X(00106).                  CI0105
       01                 PA06  REDEFINES      PA00.                    CI0105
            10            PA06-XDBPCB.                                  CI0105
            11            PA06-XDBDNM PICTURE  X(08).                   CI0105
            11            PA06-XSEGLV PICTURE  X(02).                   CI0105
            11            PA06-XRC    PICTURE  X(02).                   CI0105
            11            PA06-XPROPT PICTURE  X(04).                   CI0105
            11            PA06-FILLER PICTURE  S9(5)                    CI0105
                          BINARY.                                       CI0105
            11            PA06-XSEGNM PICTURE  X(08).                   CI0105
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0105
                          BINARY.                                       CI0105
            11            PA06-XSEGNB PICTURE  9(05)                    CI0105
                          BINARY.                                       CI0105
            11            PA06-XCOKEY PICTURE  X(70).                   CI0105
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0105
          05              PB00-SUITE.                                   CI0105
            15       FILLER         PICTURE  X(00106).                  CI0105
       01                 PB06  REDEFINES      PB00.                    CI0105
            10            PB06-XDBPCB.                                  CI0105
            11            PB06-XDBDNM PICTURE  X(08).                   CI0105
            11            PB06-XSEGLV PICTURE  X(02).                   CI0105
            11            PB06-XRC    PICTURE  X(02).                   CI0105
            11            PB06-XPROPT PICTURE  X(04).                   CI0105
            11            PB06-FILLER PICTURE  S9(5)                    CI0105
                          BINARY.                                       CI0105
            11            PB06-XSEGNM PICTURE  X(08).                   CI0105
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0105
                          BINARY.                                       CI0105
            11            PB06-XSEGNB PICTURE  9(05)                    CI0105
                          BINARY.                                       CI0105
            11            PB06-XCOKEY PICTURE  X(70).                   CI0105
      *** PCB MASK FOR ARAY                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0105
          05              PC00-SUITE.                                   CI0105
            15       FILLER         PICTURE  X(00106).                  CI0105
       01                 PC06  REDEFINES      PC00.                    CI0105
            10            PC06-XDBPCB.                                  CI0105
            11            PC06-XDBDNM PICTURE  X(08).                   CI0105
            11            PC06-XSEGLV PICTURE  X(02).                   CI0105
            11            PC06-XRC    PICTURE  X(02).                   CI0105
            11            PC06-XPROPT PICTURE  X(04).                   CI0105
            11            PC06-FILLER PICTURE  S9(5)                    CI0105
                          BINARY.                                       CI0105
            11            PC06-XSEGNM PICTURE  X(08).                   CI0105
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0105
                          BINARY.                                       CI0105
            11            PC06-XSEGNB PICTURE  9(05)                    CI0105
                          BINARY.                                       CI0105
            11            PC06-XCOKEY PICTURE  X(70).                   CI0105
      *** PCB MASK FOR DATP                                             ADU015
      *!WF DSP=PD DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PD00.                                         CI0105
          05              PD00-SUITE.                                   CI0105
            15       FILLER         PICTURE  X(00106).                  CI0105
       01                 PD06  REDEFINES      PD00.                    CI0105
            10            PD06-XDBPCB.                                  CI0105
            11            PD06-XDBDNM PICTURE  X(08).                   CI0105
            11            PD06-XSEGLV PICTURE  X(02).                   CI0105
            11            PD06-XRC    PICTURE  X(02).                   CI0105
            11            PD06-XPROPT PICTURE  X(04).                   CI0105
            11            PD06-FILLER PICTURE  S9(5)                    CI0105
                          BINARY.                                       CI0105
            11            PD06-XSEGNM PICTURE  X(08).                   CI0105
            11            PD06-XKEYLN PICTURE  S9(05)                   CI0105
                          BINARY.                                       CI0105
            11            PD06-XSEGNB PICTURE  9(05)                    CI0105
                          BINARY.                                       CI0105
            11            PD06-XCOKEY PICTURE  X(70).                   CI0105
      *** PCB MASK FOR AGCP                                             ADU015
      *!WF DSP=PE DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PE00.                                         CI0105
          05              PE00-SUITE.                                   CI0105
            15       FILLER         PICTURE  X(00106).                  CI0105
       01                 PE06  REDEFINES      PE00.                    CI0105
            10            PE06-XDBPCB.                                  CI0105
            11            PE06-XDBDNM PICTURE  X(08).                   CI0105
            11            PE06-XSEGLV PICTURE  X(02).                   CI0105
            11            PE06-XRC    PICTURE  X(02).                   CI0105
            11            PE06-XPROPT PICTURE  X(04).                   CI0105
            11            PE06-FILLER PICTURE  S9(5)                    CI0105
                          BINARY.                                       CI0105
            11            PE06-XSEGNM PICTURE  X(08).                   CI0105
            11            PE06-XKEYLN PICTURE  S9(05)                   CI0105
                          BINARY.                                       CI0105
            11            PE06-XSEGNB PICTURE  9(05)                    CI0105
                          BINARY.                                       CI0105
            11            PE06-XCOKEY PICTURE  X(70).                   CI0105
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=PF DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PF00.                                         CI0105
          05              PF00-SUITE.                                   CI0105
            15       FILLER         PICTURE  X(00106).                  CI0105
       01                 PF06  REDEFINES      PF00.                    CI0105
            10            PF06-XDBPCB.                                  CI0105
            11            PF06-XDBDNM PICTURE  X(08).                   CI0105
            11            PF06-XSEGLV PICTURE  X(02).                   CI0105
            11            PF06-XRC    PICTURE  X(02).                   CI0105
            11            PF06-XPROPT PICTURE  X(04).                   CI0105
            11            PF06-FILLER PICTURE  S9(5)                    CI0105
                          BINARY.                                       CI0105
            11            PF06-XSEGNM PICTURE  X(08).                   CI0105
            11            PF06-XKEYLN PICTURE  S9(05)                   CI0105
                          BINARY.                                       CI0105
            11            PF06-XSEGNB PICTURE  9(05)                    CI0105
                          BINARY.                                       CI0105
            11            PF06-XCOKEY PICTURE  X(70).                   CI0105
      *** PCB MASK FOR GR1P                                             ADU015
      *!WF DSP=PG DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PG00.                                         CI0105
          05              PG00-SUITE.                                   CI0105
            15       FILLER         PICTURE  X(00106).                  CI0105
       01                 PG06  REDEFINES      PG00.                    CI0105
            10            PG06-XDBPCB.                                  CI0105
            11            PG06-XDBDNM PICTURE  X(08).                   CI0105
            11            PG06-XSEGLV PICTURE  X(02).                   CI0105
            11            PG06-XRC    PICTURE  X(02).                   CI0105
            11            PG06-XPROPT PICTURE  X(04).                   CI0105
            11            PG06-FILLER PICTURE  S9(5)                    CI0105
                          BINARY.                                       CI0105
            11            PG06-XSEGNM PICTURE  X(08).                   CI0105
            11            PG06-XKEYLN PICTURE  S9(05)                   CI0105
                          BINARY.                                       CI0105
            11            PG06-XSEGNB PICTURE  9(05)                    CI0105
                          BINARY.                                       CI0105
            11            PG06-XCOKEY PICTURE  X(70).                   CI0105
      *I-O linkage segments
      *!WF DSP=WM DSL=WM SEL=7172 FOR=I DES=1 LEV=1
      * PLT=80
       01                 WM71.                                         CI0105
            10            WM71-MAPPN  PICTURE  X(10).                   CI0105
            10            WM71-CLID   PICTURE  X(23).                   CI0105
            10            WM71-C299.                                    CI0105
            11            WM71-CTID.                                    CI0105
            12            WM71-CTIDA  PICTURE  9(3).                    CI0105
            12            WM71-CTIDN.                                   CI0105
            13            WM71-CTIDNP PICTURE  X(13).                   CI0105
            13            WM71-CTIDND PICTURE  9(11).                   CI0105
            10            WM71-GECSQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            WM71-CHCR   PICTURE  99.                      CI0105
            10            WM71-IOWNG  PICTURE  X.                       CI0105
            10            WM71-NPBN   PICTURE  X(20).                   CI0105
            10            WM71-NTR    PICTURE  9(8).                    CI0105
            10            WM71-CCBAT  PICTURE  99.                      CI0105
            10            WM71-CLID4  PICTURE  X(23).                   CI0105
            10            WM71-MCSIG  PICTURE  X(30).                   CI0105
            10            WM71-CDEL1  PICTURE  9(3).                    CI0105
            10            WM71-DCACG  PICTURE  9(8).                    CI0105
            10            WM71-GEOPD2 PICTURE  X(8).                    CI0105
            10            WM71-CAUNIT PICTURE  X(4).                    CI0105
            10            WM71-FILLER PICTURE  X(50).                   CI0105
       01                 WM72.                                         CI0105
            10            WM72-ICX01  PICTURE  X.                       CI0105
            10            WM72-ICX03  PICTURE  X.                       CI0105
            10            WM72-ICX06  PICTURE  X.                       CI0105
            10            WM72-ICX09  PICTURE  X.                       CI0105
            10            WM72-ICX18  PICTURE  X.                       CI0105
            10            WM72-ICX21  PICTURE  X.                       CI0105
            10            WM72-FILLER PICTURE  X(50).                   CI0105
      *!WF DSP=LX DSL=CX SEL=01030609 FOR=I DES=1 LEV=1
      * PLT=80
       01                 LX01.                                         CI0105
            10            LX01-CX01K.                                   CI0105
            11            LX01-C199.                                    CI0105
            12            LX01-CLID.                                    CI0105
            13            LX01-CLIDO  PICTURE  9(3).                    CI0105
            13            LX01-CLIDN.                                   CI0105
            14            LX01-CLIDNP PICTURE  X(12).                   CI0105
            14            LX01-CLIDND PICTURE  9(8).                    CI0105
            10            LX01-GEMDA  PICTURE  9(8).                    CI0105
            10            LX01-NSEQ4B PICTURE  9(8)                     CI0105
                          BINARY.                                       CI0105
            10            LX01-FILLER PICTURE  X(5).                    CI0105
       01                 LX03.                                         CI0105
            10            LX03-GELL   PICTURE  9(4)                     CI0105
                          BINARY.                                       CI0105
            10            LX03-CY00.                                    CI0105
            11            LX03-CX03K.                                   CI0105
            12            LX03-CARTY  PICTURE  99.                      CI0105
            12            LX03-NARRS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            LX03-CARST  PICTURE  99.                      CI0105
            11            LX03-GECSQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            LX03-CPMTG  PICTURE  99.                      CI0105
            11            LX03-GRCRNG PICTURE  9(3).                    CI0105
            11            LX03-DEXDT  PICTURE  9(8).                    CI0105
            11            LX03-DASUP  PICTURE  9(8).                    CI0105
            11            LX03-CSTEC  PICTURE  X(3).                    CI0105
            11            LX03-FILLER PICTURE  X(17).                   CI0105
            11            LX03-CY50.                                    CI0105
            12            LX03-NARID  PICTURE  X(30).                   CI0105
            11            LX03-CY51                                     CI0105
                          REDEFINES            LX03-CY50.               CI0105
            12            LX03-NDIDN  PICTURE  9(12).                   CI0105
            12            LX03-FILLER PICTURE  X(18).                   CI0105
            11            LX03-CY52                                     CI0105
                          REDEFINES            LX03-CY50.               CI0105
            12            LX03-NAIDC  PICTURE  9(12).                   CI0105
            12            LX03-FILLER PICTURE  X(18).                   CI0105
            11            LX03-CY53                                     CI0105
                          REDEFINES            LX03-CY50.               CI0105
            12            LX03-NAMEXB PICTURE  9(15).                   CI0105
            12            LX03-FILLER PICTURE  X(15).                   CI0105
            10            LX03-CY99.                                    CI0105
            11            LX03-FILLER PICTURE  X(109).                  CI0105
            10            LX03-CY01                                     CI0105
                          REDEFINES            LX03-CY99.               CI0105
            11            LX03-NBASQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            LX03-ICPCI  PICTURE  X.                       CI0105
            11            LX03-CLUPD  PICTURE  9(3).                    CI0105
            11            LX03-DLAUP  PICTURE  9(8).                    CI0105
            11            LX03-CWRC   PICTURE  99.                      CI0105
            11            LX03-CHCR   PICTURE  99.                      CI0105
            11            LX03-GEOPD2 PICTURE  X(8).                    CI0105
            11            LX03-GEAUN  PICTURE  9(5).                    CI0105
            11            LX03-DPCHD  PICTURE  9(8).                    CI0105
            11            LX03-DLRCHK PICTURE  9(8).                    CI0105
            11            LX03-QTRCHK PICTURE  9(2).                    CI0105
            11            LX03-DNPMT  PICTURE  9(8).                    CI0105
            11            LX03-APMTLA PICTURE  S9(9)V99                 CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            LX03-CY02                                     CI0105
                          REDEFINES            LX03-CY99.               CI0105
            11            LX03-QSIRQ  PICTURE  99.                      CI0105
            11            LX03-QDRMN  PICTURE  9(2)                     CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            LX03-DDPRE  PICTURE  9(8).                    CI0105
            11            LX03-DDSHP  PICTURE  9(8).                    CI0105
            11            LX03-NDRFTB PICTURE  9(5).                    CI0105
            11            LX03-QDIPBJ PICTURE  9(3).                    CI0105
            11            LX03-DDSHPA PICTURE  9(8).                    CI0105
            11            LX03-NDRFTF PICTURE  9(5).                    CI0105
            11            LX03-QDIPBK PICTURE  9(3).                    CI0105
            11            LX03-CREOR  PICTURE  X(1).                    CI0105
            11            LX03-CREOR1 PICTURE  X(1).                    CI0105
            11            LX03-DDASC  PICTURE  9(8).                    CI0105
            11            LX03-FILLER PICTURE  X(7).                    CI0105
            10            LX03-CY03                                     CI0105
                          REDEFINES            LX03-CY99.               CI0105
            11            LX03-DLAUP1 PICTURE  9(8).                    CI0105
            11            LX03-GEOPD3 PICTURE  X(8).                    CI0105
            11            LX03-DNPMT1 PICTURE  9(8).                    CI0105
            11            LX03-DOPDA  PICTURE  99.                      CI0105
            11            LX03-CPMTF  PICTURE  99.                      CI0105
            11            LX03-CIRMO  PICTURE  X(12).                   CI0105
            11            LX03-CPALL  PICTURE  X(1).                    CI0105
            11            LX03-CCOLM  PICTURE  9(2).                    CI0105
            11            LX03-CBLTP  PICTURE  X(1).                    CI0105
            11            LX03-CASUB  PICTURE  9(2).                    CI0105
            11            LX03-CBLFM  PICTURE  9(2).                    CI0105
            11            LX03-IBILS  PICTURE  X.                       CI0105
            11            LX03-IPAOS  PICTURE  X.                       CI0105
            11            LX03-CBLSQ  PICTURE  X(4).                    CI0105
            11            LX03-DLBPD  PICTURE  9(8).                    CI0105
            11            LX03-DNBPD  PICTURE  9(8).                    CI0105
            11            LX03-DODBD  PICTURE  9(8).                    CI0105
            11            LX03-CPSRE  PICTURE  99.                      CI0105
            11            LX03-ISPHN  PICTURE  X.                       CI0105
            11            LX03-TCARR  PICTURE  X(6).                    CI0105
            11            LX03-CBKPT  PICTURE  9(2).                    CI0105
            11            LX03-IECNT  PICTURE  X.                       CI0105
            11            LX03-ICONV  PICTURE  X(1).                    CI0105
            11            LX03-FILLER PICTURE  X(4).                    CI0105
            10            LX03-CY04                                     CI0105
                          REDEFINES            LX03-CY99.               CI0105
            11            LX03-CCARD  PICTURE  X(02).                   CI0105
            11            LX03-MCSIG4 PICTURE  X(20).                   CI0105
            11            LX03-IREMT  PICTURE  X(01).                   CI0105
            11            LX03-ISBILA PICTURE  X.                       CI0105
            11            LX03-DLBPDA PICTURE  9(8).                    CI0105
            11            LX03-DNBPDA.                                  CI0105
            12            LX03-DNCYM  PICTURE  9(6).                    CI0105
            12            LX03-CEDTD  PICTURE  9(2).                    CI0105
            11            LX03-AREMT  PICTURE  S9(7)V99                 CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            LX03-DREMT  PICTURE  9(8).                    CI0105
            11            LX03-ADBRQ  PICTURE  S9(11)V99                CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            LX03-CLUPD1 PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            LX03-DLAUP3 PICTURE  9(8).                    CI0105
            11            LX03-CWRC2  PICTURE  99.                      CI0105
            11            LX03-CHCR2  PICTURE  99.                      CI0105
            11            LX03-GEOPD9 PICTURE  X(8).                    CI0105
            11            LX03-GEAUN1 PICTURE  9(5).                    CI0105
            11            LX03-DPCHD1 PICTURE  9(8).                    CI0105
       01                 LX06.                                         CI0105
            10            LX06-CX06K.                                   CI0105
            11            LX06-C299.                                    CI0105
            12            LX06-CTID.                                    CI0105
            13            LX06-CTIDA  PICTURE  9(3).                    CI0105
            13            LX06-CTIDN.                                   CI0105
            14            LX06-CTIDNP PICTURE  X(13).                   CI0105
            14            LX06-CTIDND PICTURE  9(11).                   CI0105
            10            LX06-NPECK  PICTURE  9(02).                   CI0105
            10            LX06-FILLER PICTURE  X.                       CI0105
       01                 LX09.                                         CI0105
            10            LX09-CX09K.                                   CI0105
            11            LX09-NPAIS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            LX09-CDEL1  PICTURE  9(3).                    CI0105
            10            LX09-NDELS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            LX09-CDEST  PICTURE  99.                      CI0105
            10            LX09-DISUP  PICTURE  9(8).                    CI0105
            10            LX09-CLUPD  PICTURE  9(3).                    CI0105
            10            LX09-DLAUP  PICTURE  9(8).                    CI0105
            10            LX09-GEOPD2 PICTURE  X(8).                    CI0105
            10            LX09-DPCHD  PICTURE  9(8).                    CI0105
            10            LX09-FILLER PICTURE  X(06).                   CI0105
       01                 LX18.                                         CI0105
            10            LX18-CX18K.                                   CI0105
            11            LX18-NBASQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            LX18-NPBN   PICTURE  X(20).                   CI0105
            10            LX18-CCBAT  PICTURE  99.                      CI0105
            10            LX18-DACHP  PICTURE  9(8).                    CI0105
            10            LX18-CSTPRE PICTURE  99.                      CI0105
            10            LX18-C199.                                    CI0105
            11            LX18-CLID.                                    CI0105
            12            LX18-CLIDO  PICTURE  9(3).                    CI0105
            12            LX18-CLIDN.                                   CI0105
            13            LX18-CLIDNP PICTURE  X(12).                   CI0105
            13            LX18-CLIDND PICTURE  9(8).                    CI0105
            10            LX18-MCSIG  PICTURE  X(30).                   CI0105
            10            LX18-CPBNU  PICTURE  X.                       CI0105
            10            LX18-CSPCR  PICTURE  99.                      CI0105
            10            LX18-DAPCR  PICTURE  9(8).                    CI0105
            10            LX18-FILLER PICTURE  XX.                      CI0105
       01                 LX21.                                         CI0105
            10            LX21-GELL   PICTURE  9(4)                     CI0105
                          BINARY.                                       CI0105
            10            LX21-CZ00.                                    CI0105
            11            LX21-CX21K.                                   CI0105
            12            LX21-CDEL1  PICTURE  9(3).                    CI0105
            12            LX21-NDELS  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            LX21-CZ99.                                    CI0105
            11            LX21-FILLER PICTURE  X(165).                  CI0105
            10            LX21-CZ01                                     CI0105
                          REDEFINES            LX21-CZ99.               CI0105
            11            LX21-NBASQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            LX21-GECSQ  PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            LX21-CZ02                                     CI0105
                          REDEFINES            LX21-CZ99.               CI0105
            11            LX21-CPAYE  PICTURE  9(2).                    CI0105
            11            LX21-C199.                                    CI0105
            12            LX21-CLID.                                    CI0105
            13            LX21-CLIDO  PICTURE  9(3).                    CI0105
            13            LX21-CLIDN.                                   CI0105
            14            LX21-CLIDNP PICTURE  X(12).                   CI0105
            14            LX21-CLIDND PICTURE  9(8).                    CI0105
            11            LX21-GECSQ1 PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            LX21-NBASQT PICTURE  S9(3)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            LX21-TDELI  PICTURE  X(30).                   CI0105
      *!WF DSP=LX DSL=CX SEL=1821 FOR=I DES=1 LEV=1
      * PLT=80
      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0105
          05              DE00-SUITE.                                   CI0105
            15       FILLER         PICTURE  X(00653).                  CI0105
       01                 DE10  REDEFINES      DE00.                    CI0105
            10            DE10-DU11.                                    CI0105
            11            DE10-XFONC  PICTURE  X(4).                    CI0105
            11            DE10-MPSBN  PICTURE  X(8).                    CI0105
            11            DE10-XDBDNM PICTURE  X(08).                   CI0105
            11            DE10-XSEGNM PICTURE  X(08).                   CI0105
            11            DE10-XRC    PICTURE  X(02).                   CI0105
            11            DE10-MSEG   PICTURE  X(08).                   CI0105
            11            DE10-XCOKEY PICTURE  X(70).                   CI0105
            11            DE10-CUIBR  PICTURE  X(01).                   CI0105
            11            DE10-CUIBA  PICTURE  X(01).                   CI0105
            11            DE10-IPBIK  PICTURE  X(1).                    CI0105
            10            DE10-DU03.                                    CI0105
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            DE10-CMSSF  PICTURE  XX.                      CI0105
            11            DE10-DU09.                                    CI0105
            12            DE10-CMESA  PICTURE  S9(9)                    CI0105
                          BINARY.                                       CI0105
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0105
                          BINARY.                                       CI0105
            12            DE10-CMESB  PICTURE  S9(9)                    CI0105
                          BINARY.                                       CI0105
            12            DE10-CMSST  PICTURE  S9(9)                    CI0105
                          BINARY.                                       CI0105
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0105
                          BINARY.                                       CI0105
            12            DE10-QELLAA PICTURE  S9(9)                    CI0105
                          BINARY.                                       CI0105
            12            DE10-TMESS4 PICTURE  X(512).                  CI0105
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0105
          05              MS00-SUITE.                                   CI0105
            15       FILLER         PICTURE  X(00542).                  CI0105
       01                 MS03  REDEFINES      MS00.                    CI0105
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            10            MS03-CMSSF  PICTURE  XX.                      CI0105
            10            MS03-DU09.                                    CI0105
            11            MS03-CMESA  PICTURE  S9(9)                    CI0105
                          BINARY.                                       CI0105
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0105
                          BINARY.                                       CI0105
            11            MS03-CMESB  PICTURE  S9(9)                    CI0105
                          BINARY.                                       CI0105
            11            MS03-CMSST  PICTURE  S9(9)                    CI0105
                          BINARY.                                       CI0105
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0105
                          BINARY.                                       CI0105
            11            MS03-QELLAA PICTURE  S9(9)                    CI0105
                          BINARY.                                       CI0105
            11            MS03-TMESS4 PICTURE  X(512).                  CI0105
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0105
            10            MX11-QMSGS  PICTURE  9(03).                   CI0105
            10            MX11-PJ09                                     CI0105
                          OCCURS       025     TIMES.                   CI0105
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0105
                          COMPUTATIONAL-3.                              CI0105
            11            MX11-CMESB  PICTURE  S9(9)                    CI0105
                          BINARY.                                       CI0105
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                WM71
                                WM72
                                LX01
                                LX03
                                LX06
                                LX09
                                LX18
                                LX21
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N0TSC.    NOTE *PCB stuff                          *.
       F0TSC.                                                           lv10
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR ARAY                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-ARAY-PTR1.                                          ADU015
      *SET ADDRESS FOR DATP                                             DOT
           SET ADDRESS OF PD06 TO                                       ADU015
                PCB-DATP-PTR1.                                          ADU015
      *SET ADDRESS FOR AGCP                                             DOT
           SET ADDRESS OF PE06 TO                                       ADU015
                PCB-AGCP-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF PF06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR GR1P                                             DOT
           SET ADDRESS OF PG06 TO                                       ADU015
                PCB-GR1P-PTR1.                                          ADU015
       F0TSC-FN. EXIT.
      *N01.      NOTE *************************************.            CI0105
      *               *                                   *             CI0105
      *               *INITIALISATIONS                    *             CI0105
      *               *                                   *             CI0105
      *               *************************************.            CI0105
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
      *N02DB.    NOTE *Output segment                     *.
       F02DB.                                                           lv10
           INITIALIZE  WM72
           LX01
           LX03
           LX06
           LX09
           LX18
           LX21.
       F02DB-FN. EXIT.
      *N02EB.    NOTE *Program variables and copy segs    *.
       F02EB.                                                           lv10
           MOVE ALL    '0' TO CX-CF
           CL-CF
           INITIALIZE  MS03
           SV09
           SV18
           SV21
           SET NO-AUDIT-LOGS TO TRUE.
       F02EB-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0105
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0105
      *               *                                   *             CI0105
      *               *FIN DE TRAITEMENT                  *             CI0105
      *               *                                   *             CI0105
      *               *************************************.            CI0105
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0105
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *Basic validation                   *
      *               *                                   *
      *               *************************************.
       F35.           EXIT.                                             lv05
      *N35BB.    NOTE *Contract id                        *.
       F35BB.    IF    WM71-CTID NOT NUMERIC                            lv10
                 OR    WM71-CTID NOT > ZERO
                 NEXT SENTENCE ELSE GO TO     F35BB-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013140 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35BB-FN. EXIT.
      *N35BD.    NOTE *Telephone or written request       *.
       F35BD.    IF    WM71-CHCR NOT NUMERIC                            lv10
                 OR    (WM71-CHCR NOT = 02
                 AND   WM71-CHCR NOT = 03)
                 NEXT SENTENCE ELSE GO TO     F35BD-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013322 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35BD-FN. EXIT.
      *N35BF.    NOTE *Bank account client id             *.
       F35BF.    IF    WM71-CLID4 NOT NUMERIC                           lv10
                 OR    WM71-CLID4 NOT > ZERO
                 NEXT SENTENCE ELSE GO TO     F35BF-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012612 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35BF-FN. EXIT.
      *N35BH.    NOTE *Delivery type                      *.
       F35BH.    IF    WM71-CDEL1 NOT NUMERIC                           lv10
                 OR    (WM71-CDEL1 NOT = 01
                 AND   WM71-CDEL1 NOT = 03)
                 NEXT SENTENCE ELSE GO TO     F35BH-FN.
      *Must be wire or ACH
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012158 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35BH-FN. EXIT.
      *N35BK.    NOTE *Default client id                  *.
       F35BK.    IF    WM71-CLID NOT NUMERIC                            lv10
                 OR    WM71-CLID NOT > ZERO
                 NEXT SENTENCE ELSE GO TO     F35BK-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012058 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35BK-FN. EXIT.
      *N35BM.    NOTE *Client Communication Seq No        *.
       F35BM.    IF    WM71-GECSQ NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F35BM-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012153 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35BM-FN. EXIT.
      *N35CJ.    NOTE *Ownership guidlines Yes or No      *.
       F35CJ.    IF    WM71-IOWNG NOT = 'Y'                             lv10
                 AND   WM71-IOWNG NOT = 'N'
                 NEXT SENTENCE ELSE GO TO     F35CJ-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013324 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35CJ-FN. EXIT.
      *N35CM.    NOTE *Bank RTN must be numeric           *.
       F35CM.    IF    WM71-NTR NOT NUMERIC                             lv10
                 OR    WM71-NTR NOT > ZERO
                 NEXT SENTENCE ELSE GO TO     F35CM-FN.
      *AND NON ZERO
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012159 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35CM-FN. EXIT.
      *N35DB.    NOTE *Application name                   *.
       F35DB.    IF    WM71-MAPPN NOT = 'UD'                            lv10
                 NEXT SENTENCE ELSE GO TO     F35DB-FN.
      *Only allow 'UD' at present
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012734 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35DB-FN. EXIT.
      *N35DF.    NOTE *Personal Bank Account Number       *.
       F35DF.    IF    WM71-NPBN = SPACES                               lv10
                 NEXT SENTENCE ELSE GO TO     F35DF-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012605 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35DF-FN. EXIT.
      *N35DK.    NOTE *Bank account Signature Line        *.
       F35DK.    IF    WM71-MCSIG = SPACES                              lv10
                 NEXT SENTENCE ELSE GO TO     F35DK-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012606 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35DK-FN. EXIT.
      *N35EB.    NOTE *Validate accounting date           *.
       F35EB.                                                           lv10
           INITIALIZE  DD30
           DD33.
      *N35ED.    NOTE *Validate via CDU                   *.
       F35ED.    IF    WM71-DCACG NUMERIC                               lv15
                 NEXT SENTENCE ELSE GO TO     F35ED-FN.
           MOVE        4 TO DD30-CDTSF
           MOVE        WM71-DCACG TO DD33-DTGRG
           PERFORM     F95DD THRU F95DD-FN.
       F35ED-FN. EXIT.
      *N35EG.    NOTE *Bad date found                     *.
       F35EG.    IF    WM71-DCACG NOT NUMERIC                           lv15
                 OR    DD30-CDTSC NOT = ZERO
                 NEXT SENTENCE ELSE GO TO     F35EG-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012786 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35EG-FN. EXIT.
       F35EB-FN. EXIT.
      *N35FB.    NOTE *Validate ACF2 parameters...        *.
       F35FB.    IF    WM71-GEOPD2 = SPACES                             lv10
                 OR    WM71-CAUNIT = SPACES
                 NEXT SENTENCE ELSE GO TO     F35FB-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013356 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35FB-FN. EXIT.
       F35-FN.   EXIT.
      *N36.      NOTE *************************************.
      *               *                                   *
      *               *Cross validation                   *
      *               *                                   *
      *               *************************************.
       F36.           EXIT.                                             lv05
      *N36BB.    NOTE *Ownership must be followed for     *.
       F36BB.    IF    WM71-IOWNG = 'N'                                 lv10
                 AND   WM71-CHCR = 03
                 NEXT SENTENCE ELSE GO TO     F36BB-FN.
      *telephone transactions...
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013324 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F36BB-FN. EXIT.
       F36-FN.   EXIT.
      *N38.      NOTE *************************************.
      *               *                                   *
      *               *Re-validate if transaction is      *
      *               *                                   *
      *               *************************************.
       F38.      IF    EIBTRNID NOT = 'MD20'                            lv05
                 OR    EIBTRNID NOT = 'XD20'
                 NEXT SENTENCE ELSE GO TO     F38-FN.
      *OTHER THAN 'MD20'
      *N38CA.    NOTE *Get Bank details                   *.
       F38CA.         EXIT.                                             lv10
      *N38CC.    NOTE *CALL CI0025 - BANK'S NAME/RTN      *.            AM0025
       F38CC.                                                           lv15
      *                                                                 AM0025
      *********************************                                 AM0025
      ** THIS MODULE WILL READ THE    *                                 AM0025
      ** CLIENT DATABASE TO GET THE   *                                 AM0025
      ** BANK'S NAME AND RTN USING THE*                                 AM0025
      ** BANK CLIENT ID NUMBER PASSED.                                  AM0025
      *********************************                                 AM0025
      *                                                                 AM0025
           INITIALIZE      DU30                                         AM0025
           MOVE        WM71-CLID4 TO DU30-CLID                          AM0025
           MOVE        WM71-CDEL1 TO DU30-CDEL1
           MOVE        WM71-DCACG TO DU30-DCACG                         AM0025
           MOVE        ZEROS TO DU30-NRTSQ1                             AM0025
           SET CI0025B-PCB-CL1P-PTR1 TO                                 AM0025
                       PCB-CL1P-PTR1                                    AM0025
           INITIALIZE      DE10-DU03                                    AM0025
           CALL        CI0025 USING                                     AM0025
           DFHEIBLK                                                     AM0025
           DFHCOMMAREA                                                  AM0025
           DLIUIBII                                                     AM0025
           CI0025B-PCB-ADDRESS-LIST                                     AM0025
           DU30                                                         AM0025
           DE10                                                         AM0025
           MS03.                                                        AM0025
       F38CC-FN. EXIT.
      *N38CG.    NOTE *Check for Errors from CI0025       *.            ADU074
       F38CG.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU074
                 NEXT SENTENCE ELSE GO TO     F38CG-FN.                 ADU074
      *above severity level 10                                          ADU074
           MOVE                     ALL '1' TO FT GO TO F20.            ADU074
       F38CG-FN. EXIT.
      *N38CH.    NOTE *Check DL/1 Errors from CI0025      *.            ADU074
       F38CH.    IF    DE10-NMESS2 > ZERO                               lv15
                 NEXT SENTENCE ELSE GO TO     F38CH-FN.                 ADU074
           MOVE                     ALL '1' TO FT GO TO F20.            ADU074
       F38CH-FN. EXIT.
      *N38CI.    NOTE *CI0025 executed OK...              *.            ADU074
       F38CI.                                                           lv15
           INITIALIZE  MS03.                                            ADU074
       F38CI-FN. EXIT.
       F38CA-FN. EXIT.
      *N38EB.    NOTE *Is bank valid for payment method   *.
       F38EB.                                                           lv10
      *Note: Check is not valid for
      *unscheduled disbursements
      *N38ED.    NOTE *Wire selected                      *.
       F38ED.    IF    WM71-CDEL1 = 001                                 lv15
                 AND   DU30-IRTNA NOT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F38ED-FN.
      *(Wire now looks at ACH flag!)
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012832 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F38ED-FN. EXIT.
      *N38EJ.    NOTE *ACH selected                       *.
       F38EJ.    IF    WM71-CDEL1 = 003                                 lv15
                 AND   DU30-IRTNA NOT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F38EJ-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012832 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F38EJ-FN. EXIT.
       F38EB-FN. EXIT.
       F38-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *Database Validation                *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40BB.    NOTE *Locate the contract                *.
       F40BB.                                                           lv10
           MOVE        WM71-CTID TO S-CTU01-CTID
           PERFORM     F94CT THRU F94CT-FN.
                 IF    IK = '1'                                         DOT
      *Not found
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012077 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40BB-FN. EXIT.
      *N40CC.    NOTE *Locate Client Communication Addr   *.
       F40CC.                                                           lv10
           MOVE        WM71-CLID TO S-CLU01-CLID
           MOVE        WM71-GECSQ TO S-CLU24-GECSQ
           PERFORM     F94C2 THRU F94C2-FN.
                 IF    IK = '1'                                         DOT
      *Not found
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012153 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40CC-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *Telephone Validation               *
      *               *                                   *
      *               *************************************.
       F45.      IF    WM71-CHCR = 03                                   lv05
                 NEXT SENTENCE ELSE GO TO     F45-FN.
      *N45BB.    NOTE *Account type invalid for phone     *.
       F45BB.    IF    WM71-CTIDA NOT = 001                             lv10
                 AND   WM71-CTIDA NOT = 002
                 AND   WM71-CTIDA NOT = 004
                 AND   WM71-CTIDA NOT = 005
                 AND   WM71-CTIDA NOT = 021
                 NEXT SENTENCE ELSE GO TO     F45BB-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013418 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45BB-FN. EXIT.
      *N45BD.    NOTE *CALL CI0019 - ACCOUNT GROUPS       *.            AM0019
       F45BD.                                                           lv10
      *                                                                 AM0019
      *********************************                                 AM0019
      ** THIS MODULE WILL READ THE    *                                 AM0019
      ** CONTRACT DATABASE TO GET ALL *                                 AM0019
      ** THE GROUPS FOR THE ACCOUNT   *                                 AM0019
      ** NUMBER.                      *                                 AM0019
      *********************************                                 AM0019
      *                                                                 AM0019
           INITIALIZE      DU15                                         AM0019
           MOVE        WM71-CTID TO DU15-CTID                           AM0019
           MOVE        WM71-DCACG TO DU15-DCACG                         AM0019
           MOVE        10 TO DU15-XIMAX                                 AM0019
           MOVE        'Y' TO DU15-IPOCH                                AM0019
           SET CI0019B-PCB-CT1P-PTR1 TO                                 AM0019
                       PCB-CT1P-PTR1                                    AM0019
           SET CI0019B-PCB-GR1P-PTR1 TO                                 AM0019
                       PCB-GR1P-PTR1                                    AM0019
           INITIALIZE      DE10-DU03                                    AM0019
           CALL        CI0019 USING                                     AM0019
           DFHEIBLK                                                     AM0019
           DFHCOMMAREA                                                  AM0019
           DLIUIBII                                                     AM0019
           CI0019B-PCB-ADDRESS-LIST                                     AM0019
           DU15                                                         AM0019
           DE10                                                         AM0019
           MS03.                                                        AM0019
      *N45BE.    NOTE *Check for Errors from CI0019       *.            ADU074
       F45BE.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU074
                 NEXT SENTENCE ELSE GO TO     F45BE-FN.                 ADU074
      *above severity level 10                                          ADU074
           MOVE                     ALL '1' TO FT GO TO F20.            ADU074
       F45BE-FN. EXIT.
      *N45BF.    NOTE *Check DL/1 Errors from CI0019      *.            ADU074
       F45BF.    IF    DE10-NMESS2 > ZERO                               lv15
                 NEXT SENTENCE ELSE GO TO     F45BF-FN.                 ADU074
           MOVE                     ALL '1' TO FT GO TO F20.            ADU074
       F45BF-FN. EXIT.
      *N45BG.    NOTE *CI0019 executed OK...              *.            ADU074
       F45BG.                                                           lv15
           INITIALIZE  MS03.                                            ADU074
       F45BG-FN. EXIT.
       F45BD-FN. EXIT.
      *N45CA.    NOTE *Initialize flags                   *.
       F45CA.                                                           lv10
           INITIALIZE  WS01-GRPLC.
       F45CA-FN. EXIT.
      *N45CB.    NOTE *Examine all groups found           *.
       F45CB.                                                           lv10
           MOVE        1                        TO J45CBR
                                    GO TO     F45CB-B.
       F45CB-A.
           ADD         1                        TO J45CBR.
       F45CB-B.
           IF          J45CBR                   >  DU15-QITEM
                                    GO TO     F45CB-FN.
           MOVE        DU15-CT10 (J45CBR) TO WT10.
      *N45CD.    NOTE *Group type is Pension              *.
       F45CD.    IF    WT10-GRIDC = 002                                 lv15
                 AND   WT10-GERED = ZERO
                 NEXT SENTENCE ELSE GO TO     F45CD-FN.
           MOVE        DU15-GR07 (J45CBR) TO WG07.
                 IF    WT10-GRIDC = 002                                 DOT
                 AND   WG07-GRPLC = 02
      *Retain group type when pension
           MOVE        WG07-GRPLC TO WS01-GRPLC.
      *N45CN.    NOTE *Update is not allowed for group    *.
       F45CN.    IF    WG07-CIDRP = 02                                  lv20
                 AND   (WG07-GRPLT = 08
                 OR    WG07-GRPLT = 09)
                 NEXT SENTENCE ELSE GO TO     F45CN-FN.
      *where IDS Role to code Sponser
      *and Plan type is either
      *IDS incentive or divisional mgr
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013325 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45CN-FN. EXIT.
       F45CD-FN. EXIT.
       F45CB-900. GO TO F45CB-A.
       F45CB-FN. EXIT.
       F45-FN.   EXIT.
      *N46.      NOTE *************************************.
      *               *                                   *
      *               *Pactable Validation                *
      *               *                                   *
      *               *************************************.
       F46.           EXIT.                                             lv05
      *N46BB.    NOTE *TA5B Access                        *.
       F46BB.                                                           lv10
           MOVE        CT01-CTIDA TO TA5B-CTIDA
           MOVE        CT01-PRCOD TO TA5B-PRCOD.
                 IF    CT01-CTIDA = 002                                 DOT
      *Sub account is strange for fund
           MOVE        CT01-PRSCD TO TA5B-PRSCD
                 ELSE
      *Otherwise it's space filled
           MOVE        SPACES TO TA5B-PRSCD.
      *Endif                                                            DOT
           PERFORM     F92TB THRU F92TB-FN.
       F46BB-FN. EXIT.
      *N46BE.    NOTE *Terminated Fund trans invalid      *.
       F46BE.    IF    TA5B-PRCAUT = 'T'                                lv10
                 AND   CT01-CTIDA = 002
                 NEXT SENTENCE ELSE GO TO     F46BE-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013327 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F46BE-FN. EXIT.
       F46-FN.   EXIT.
      *N47.      NOTE *************************************.
      *               *                                   *
      *               *Arrangement Validation             *
      *               *                                   *
      *               *************************************.
       F47.           EXIT.                                             lv05
      *N47BB.    NOTE *Look for exisiting arrangement     *.
       F47BB.                                                           lv10
           MOVE        WM71-CLID TO S-CXU2Y-CLID
           MOVE        02 TO S-CXU2Y-CARTY
           MOVE        ZERO TO S-CXU2Y-NARRS
           MOVE        WM71-CTID TO S-CXU2Y-CTID
           MOVE        '>' TO S-CXU2Y-OPER
           PERFORM     F94CY THRU F94CY-FN.
      *N47BJ.    NOTE *Found an exisiting arrangement     *.
       F47BJ.    IF    IK = '0'                                         lv15
                 AND   CX2Y-CARTY = 02
                 AND   WM71-CTID = CX2Y-CTID
                 AND   CX2Y-CLID = WM71-CLID
                 NEXT SENTENCE ELSE GO TO     F47BJ-FN.
      *for UD....
           MOVE        '1' TO CX2Y-CF.
      *N47BO.    NOTE *Retrieve Arrangement details       *.
       F47BO.                                                           lv20
      *Get CX01
           MOVE        CX2Y-CLID TO S-CXU01-CLID
           PERFORM     F94U1 THRU F94U1-FN.
                 IF    IK = '0'                                         DOT
      *Access OK
           MOVE        '1' TO CX01-CF.
      *N47BP.    NOTE *Get CX03                           *.
       F47BP.                                                           lv25
           MOVE        CX2Y-CARTY TO S-CXU03-CARTY
           MOVE        CX2Y-NARRS TO S-CXU03-NARRS
           PERFORM     F94U2 THRU F94U2-FN.
                 IF    IK = '0'                                         DOT
      *Access OK
           MOVE        '1' TO CX03-CF.
       F47BP-FN. EXIT.
      *N47BR.    NOTE *Get CX06                           *.
       F47BR.                                                           lv25
           MOVE        CX2Y-CTID TO S-CXU06-CTID
           PERFORM     F94U3 THRU F94U3-FN.
                 IF    IK = '0'                                         DOT
      *Access OK
           MOVE        '1' TO CX06-CF.
       F47BR-FN. EXIT.
       F47BO-FN. EXIT.
       F47BJ-900. GO TO F47CB-FN.
       F47BJ-FN. EXIT.
      *N47CB.    NOTE *Otherwise account not set up       *.
       F47CB.                                                           lv15
      *for this client.... but client
      *may have other telephone
      *arrangements...
      *Attempt to locate CX01, CX03
      *N47CE.    NOTE *Retrieve Arrangement details       *.
       F47CE.                                                           lv20
      *Get CX01
           MOVE        WM71-CLID TO S-CXU01-CLID
           PERFORM     F94U1 THRU F94U1-FN.
                 IF    IK = '0'                                         DOT
      *Access OK
           MOVE        '1' TO CX01-CF.
      *N47CG.    NOTE *Get first CX03                     *.
       F47CG.    IF    CX01-CF = '1'                                    lv25
                 NEXT SENTENCE ELSE GO TO     F47CG-FN.
           PERFORM     F94N3 THRU F94N3-FN.
      *N47CH.    NOTE *Look for telephone arrangement     *.
       F47CH.    IF    IK = '0'                                         lv30
                 AND   CX03-CARTY NOT = 02
                 NEXT SENTENCE ELSE GO TO     F47CH-FN.
           PERFORM     F94N3 THRU F94N3-FN.
       F47CH-900. GO TO F47CH.
       F47CH-FN. EXIT.
      *N47CL.    NOTE *Found the CX03                     *.
       F47CL.    IF    IK = '0'                                         lv30
                 NEXT SENTENCE ELSE GO TO     F47CL-FN.
           MOVE        '1' TO CX03-CF.
       F47CL-FN. EXIT.
       F47CG-FN. EXIT.
       F47CE-FN. EXIT.
       F47CB-FN. EXIT.
       F47BB-FN. EXIT.
       F47-FN.   EXIT.
      *N48.      NOTE *************************************.
      *               *                                   *
      *               *Bank Existence Processing          *
      *               *                                   *
      *               *************************************.
       F48.           EXIT.                                             lv05
      *N48AC.    NOTE *Locate the Bank Address Sequ No    *.
       F48AC.                                                           lv10
      *>> Bank Client Root Segment CL01
           MOVE        WM71-CLID4 TO S-CLU01-CLID
           PERFORM     F94C3 THRU F94C3-FN.
                 IF    IK = '1'                                         DOT
      *Bank Client ID Not found
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012612 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F48AC-FN. EXIT.
      *N48AF.    NOTE *Search for current Bank Address    *.
       F48AF.                                                           lv10
      *Locate first CL24...
           PERFORM     F94C4 THRU F94C4-FN.
      *N48AJ.    NOTE *Find current address               *.
       F48AJ.    IF    IK = '0'                                         lv15
                 AND   CL24-GECED NOT = ZEROES
                 NEXT SENTENCE ELSE GO TO     F48AJ-FN.
           PERFORM     F94C4 THRU F94C4-FN.
       F48AJ-900. GO TO F48AJ.
       F48AJ-FN. EXIT.
      *N48AM.    NOTE *Current Address Found              *.
       F48AM.    IF    IK = '0'                                         lv15
                 AND   CL24-GECED = ZEROES
                 NEXT SENTENCE ELSE GO TO     F48AM-FN.
           MOVE        '1' TO CL24-CF.
       F48AM-900. GO TO F48AN-FN.
       F48AM-FN. EXIT.
      *N48AN.    NOTE *Ooops a bank without an address    *.
       F48AN.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013499 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F48AN-FN. EXIT.
       F48AF-FN. EXIT.
      *N48BB.    NOTE *Set up CX18 Bank Details           *.
       F48BB.                                                           lv10
           INITIALIZE  WX18
           MOVE        +1 TO WX18-NBASQ
           MOVE        WM71-NPBN TO WX18-NPBN
           MOVE        WM71-CCBAT TO WX18-CCBAT
           MOVE        ZEROS TO WX18-DACHP
           MOVE        03 TO WX18-CSTPRE
           MOVE        WM71-CLID4 TO WX18-CLID
           MOVE        WM71-MCSIG TO WX18-MCSIG
           MOVE        SPACES TO WX18-CPBNU
           MOVE        01 TO WX18-CSPCR
           MOVE        ZEROS TO WX18-DAPCR.
       F48BB-FN. EXIT.
      *N48CA.    NOTE *Set up CX21 for Wire/ACH           *.
       F48CA.                                                           lv10
      *Note: CX21's for UD are set up
      *all as WIRE and not as ACH
      *SD and UD do NOT share CX21 recs
           INITIALIZE  WX21
           MOVE        +11 TO WX21-GELL
           MOVE        001 TO WX21-CDEL1
           MOVE        001 TO WX21-NDELS
           MOVE        001 TO WX21-NBASQ
           MOVE        CL24-GECSQ TO WX21-GECSQ.
       F48CA-FN. EXIT.
      *N48DA.    NOTE *Successful CX01 access required    *.
       F48DA.    IF    CX01-CF = '0'                                    lv10
                 NEXT SENTENCE ELSE GO TO     F48DA-FN.
           MOVE        WM71-CLID TO S-CXU01-CLID
           PERFORM     F94U1 THRU F94U1-FN.
                 IF    IK = '0'                                         DOT
      *Client has arrangement(s)
           MOVE        '1' TO CX01-CF.
       F48DA-FN. EXIT.
      *N48DC.    NOTE *Find bank for existing client      *.
       F48DC.    IF    CX01-CF = '1'                                    lv10
                 NEXT SENTENCE ELSE GO TO     F48DC-FN.
      *Read first bank..
           PERFORM     F94N1 THRU F94N1-FN.
      *N48DF.    NOTE *Look at all the banks for match    *.
       F48DF.    IF    IK = '0'                                         lv15
                 AND   (CX18-CLID NOT = WX18-CLID
                 OR    CX18-NPBN NOT = WX18-NPBN)
                 NEXT SENTENCE ELSE GO TO     F48DF-FN.
           PERFORM     F94N1 THRU F94N1-FN.
       F48DF-900. GO TO F48DF.
       F48DF-FN. EXIT.
      *N48DH.    NOTE *A match has been found             *.
       F48DH.    IF    IK = '0'                                         lv15
                 AND   CX18-CLID = WX18-CLID
                 AND   CX18-NPBN = WX18-NPBN
                 NEXT SENTENCE ELSE GO TO     F48DH-FN.
      *Update W/S segment
           MOVE        CX18-NBASQ TO WX18-NBASQ
           MOVE        CX18-DACHP TO WX18-DACHP
           MOVE        CX18-CSTPRE TO WX18-CSTPRE
           MOVE        CX18-CPBNU TO WX18-CPBNU
           MOVE        CX18-CSPCR TO WX18-CSPCR
           MOVE        CX18-DAPCR TO WX18-DAPCR
           MOVE        '1' TO CX18-CF
           MOVE        CX18 TO SV18.
      *N48DL.    NOTE *Account type or Signature change   *.
       F48DL.    IF    CX18-CCBAT NOT = WX18-CCBAT                      lv20
                 OR    CX18-MCSIG NOT = WX18-MCSIG
                 OR    CX18-CSPCR = 03
                 NEXT SENTENCE ELSE GO TO     F48DL-FN.
           MOVE        01 TO WX18-CSPCR.
       F48DL-FN. EXIT.
       F48DH-FN. EXIT.
       F48DC-FN. EXIT.
      *N48FD.    NOTE *Bank found..                       *.
       F48FD.    IF    CX18-CF = '1'                                    lv10
                 NEXT SENTENCE ELSE GO TO     F48FD-FN.
           MOVE        CX18-NBASQ TO WX21-NBASQ.
      *N48FJ.    NOTE *Read first CX21                    *.
       F48FJ.                                                           lv15
           PERFORM     F94C1 THRU F94C1-FN.
       F48FJ-FN. EXIT.
      *N48FL.    NOTE *Examine all CX21's for match       *.
       F48FL.    IF    IK = '0'                                         lv15
                 AND   (CX21-CDEL1 NOT = WX21-CDEL1
                 OR    CX21-NBASQ NOT = WX21-NBASQ)
                 NEXT SENTENCE ELSE GO TO     F48FL-FN.
      *on arrangment type and bank
      *sequence number
      *Note: No need to check for
      *      different layouts as the
      *      Delivery type can only be
      *      that of wire of ACH...
           PERFORM     F94C1 THRU F94C1-FN.
       F48FL-900. GO TO F48FL.
       F48FL-FN. EXIT.
      *N48FN.    NOTE *CX21 Found                         *.
       F48FN.    IF    IK = '0'                                         lv15
                 AND   CX21-CDEL1 = WX21-CDEL1
                 AND   CX21-NBASQ = WX21-NBASQ
                 NEXT SENTENCE ELSE GO TO     F48FN-FN.
           MOVE        '1' TO CX21-CF.
       F48FN-FN. EXIT.
       F48FD-FN. EXIT.
      *N48HB.    NOTE *Client Delivery Instr seg found    *.
       F48HB.    IF    CX21-CF = '1'                                    lv10
                 NEXT SENTENCE ELSE GO TO     F48HB-FN.
      *then look for exisiting UD
           MOVE        'F----' TO S-CXU03-CCOD
           MOVE        WM71-CTID TO S-CXU06-CTID
           PERFORM     F94N2 THRU F94N2-FN
           MOVE        '-----' TO S-CXU03-CCOD.
      *N48HE.    NOTE *Find matching CX09 for CX21        *.
       F48HE.    IF    IK = '0'                                         lv15
                 AND   (CX09-CDEL1 NOT = CX21-CDEL1
                 OR    CX09-NDELS NOT = CX21-NDELS)
                 NEXT SENTENCE ELSE GO TO     F48HE-FN.
           PERFORM     F94N2 THRU F94N2-FN.
       F48HE-900. GO TO F48HE.
       F48HE-FN. EXIT.
      *N48HK.    NOTE *Found an exisiting arrangement     *.
       F48HK.    IF    IK = '0'                                         lv15
                 AND   CX09-CDEL1 = CX21-CDEL1
                 AND   CX09-NDELS = CX21-NDELS
                 NEXT SENTENCE ELSE GO TO     F48HK-FN.
           MOVE        '1' TO CX09-CF.
       F48HK-FN. EXIT.
       F48HB-FN. EXIT.
       F48-FN.   EXIT.
      *N52.      NOTE *************************************.
      *               *                                   *
      *               *Update to CX01                     *
      *               *                                   *
      *               *************************************.
       F52.           EXIT.                                             lv05
      *N52DA.    NOTE *No CX01 found... create one        *.
       F52DA.    IF    CX01-CF = '0'                                    lv10
                 NEXT SENTENCE ELSE GO TO     F52DA-FN.
           INITIALIZE  CX01
           MOVE        WM71-CLID TO CX01-CLID
           MOVE        +1 TO CX01-NSEQ4B
           MOVE        WM71-DCACG TO CX01-GEMDA
           PERFORM     F94IA THRU F94IA-FN.
       F52DA-900. GO TO F52HA-FN.
       F52DA-FN. EXIT.
      *N52HA.    NOTE *Update CX01 timestamp              *.
       F52HA.                                                           lv10
           PERFORM     F94UA THRU F94UA-FN
           ADD         +1 TO CX01-NSEQ4B
           SIZE ERROR
           MOVE        +1 TO CX01-NSEQ4B.
      *Replace the CX01                                                 DOT
           PERFORM     F94RA THRU F94RA-FN.
       F52HA-FN. EXIT.
      *N52LA.    NOTE *Insert or Replace failed           *.
       F52LA.    IF    IK = '1'                                         lv10
                 NEXT SENTENCE ELSE GO TO     F52LA-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012073 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52LA-FN. EXIT.
       F52-FN.   EXIT.
      *N54.      NOTE *************************************.
      *               *                                   *
      *               *CX18 Updates                       *
      *               *                                   *
      *               *************************************.
       F54.           EXIT.                                             lv05
      *N54AB.    NOTE *No same day updates for            *.
       F54AB.    IF    WM71-CDEL1 = 03                                  lv10
                 AND   WM71-CHCR = 03
                 AND   (CX18-CF = '1'
                 AND   CX18-CX18K NOT = WX18-CX18K
                 AND   CX18-NPBN NOT = WX18-NPBN
                 AND   CX18-CCBAT NOT = WX18-CCBAT
                 AND   CX18-DACHP NOT = WX18-DACHP
                 AND   CX18-CSTPRE NOT =
                       WX18-CSTPRE
                 AND   CX18-C199 NOT = WX18-C199
                 AND   CX18-MCSIG NOT = WX18-MCSIG
                 AND   CX18-CPBNU NOT = WX18-CPBNU
                 AND   CX18-DAPCR NOT =
                       WX18-DAPCR)
                 NEXT SENTENCE ELSE GO TO     F54AB-FN.
      *DIRECT DEPOSIT
      *NOTE: CHANGES TO ALL THE FIELDS
      *OTHER THAN STATUS CODE OF
      *PRENOTE TO A CX18 ARE REGRADED
      *AS CHANGES
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013357 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54AB-FN. EXIT.
      *N54BA.    NOTE *CX18 exists and updates required   *.
       F54BA.    IF    CX18-CF = '1'                                    lv10
                 AND   CX18 NOT = WX18
                 NEXT SENTENCE ELSE GO TO     F54BA-FN.
      *Re-establish hold on CX18 (GHU)
           MOVE        CX18-NBASQ TO S-CXU18-NBASQ
           PERFORM     F94UB THRU F94UB-FN.
      *N54BC.    NOTE *Replace the record                 *.
       F54BC.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F54BC-FN.
           MOVE        WX18 TO CX18
           PERFORM     F94RB THRU F94RB-FN.
       F54BC-FN. EXIT.
       F54BA-FN. EXIT.
      *N54EA.    NOTE *CX18 needs to be inserted          *.
       F54EA.    IF    CX18-CF = '0'                                    lv10
                 NEXT SENTENCE ELSE GO TO     F54EA-FN.
           MOVE        WX18 TO CX18
           PERFORM     F94IB THRU F94IB-FN.
      *N54EC.    NOTE *Insert did not work - repeat       *.
       F54EC.    IF    IK = '1'                                         lv15
                 AND   XW05-XRC = 'II'
                 NEXT SENTENCE ELSE GO TO     F54EC-FN.
      *(Duplicate insert only)
           ADD         +1 TO CX18-NBASQ
           PERFORM     F94IB THRU F94IB-FN.
       F54EC-900. GO TO F54EC.
       F54EC-FN. EXIT.
      *N54EF.    NOTE *Insert failed                      *.
       F54EF.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F54EF-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012626 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54EF-FN. EXIT.
      *N54EH.    NOTE *Insert Successful                  *.
       F54EH.                                                           lv15
      *Insert CX19
           MOVE        CX18-NBASQ TO S-CXU18-CX18K
           INITIALIZE  CX19
           PERFORM     F94IH THRU F94IH-FN.
       F54EH-FN. EXIT.
      *N54EI.    NOTE *Insert failed                      *.
       F54EI.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F54EI-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012626 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54EI-FN. EXIT.
       F54EA-FN. EXIT.
       F54-FN.   EXIT.
      *N56.      NOTE *************************************.
      *               *                                   *
      *               *CX21 Updates                       *
      *               *                                   *
      *               *************************************.
       F56.           EXIT.                                             lv05
      *N56BA.    NOTE *Locate CX21 for CX18               *.
       F56BA.    IF    CX01-CF = '1'                                    lv10
                 NEXT SENTENCE ELSE GO TO     F56BA-FN.
           MOVE        CX18-NBASQ TO WX21-NBASQ
      *Read first CX21
           PERFORM     F94C1 THRU F94C1-FN.
      *N56BL.    NOTE *Examine all CX21's for match       *.
       F56BL.    IF    IK = '0'                                         lv15
                 AND   (CX21-CDEL1 NOT = WX21-CDEL1
                 OR    CX21-NBASQ NOT = WX21-NBASQ)
                 NEXT SENTENCE ELSE GO TO     F56BL-FN.
      *on arrangment type and bank
      *sequence number
           PERFORM     F94C1 THRU F94C1-FN.
       F56BL-900. GO TO F56BL.
       F56BL-FN. EXIT.
      *N56BN.    NOTE *CX21 found                         *.
       F56BN.    IF    IK = '0'                                         lv15
                 AND   CX21-CDEL1 = WX21-CDEL1
                 AND   CX21-NBASQ = WX21-NBASQ
                 NEXT SENTENCE ELSE GO TO     F56BN-FN.
           MOVE        '1' TO CX21-CF.
       F56BN-FN. EXIT.
       F56BA-FN. EXIT.
      *N56DA.    NOTE *No CX21 found - create one         *.
       F56DA.    IF    CX21-CF = '0'                                    lv10
                 NEXT SENTENCE ELSE GO TO     F56DA-FN.
           MOVE        WX21 TO CX21
           PERFORM     F94IC THRU F94IC-FN.
      *N56DD.    NOTE *Didn't work... try again           *.
       F56DD.    IF    IK NOT = ZERO                                    lv15
                 AND   XW05-XRC = 'II'
                 NEXT SENTENCE ELSE GO TO     F56DD-FN.
           ADD         1 TO CX21-NDELS
           PERFORM     F94IC THRU F94IC-FN.
       F56DD-900. GO TO F56DD.
       F56DD-FN. EXIT.
      *N56DG.    NOTE *It really did not work...          *.
       F56DG.    IF    IK NOT = ZERO                                    lv15
                 NEXT SENTENCE ELSE GO TO     F56DG-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012626 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F56DG-FN. EXIT.
       F56DA-FN. EXIT.
       F56-FN.   EXIT.
      *N58.      NOTE *************************************.
      *               *                                   *
      *               *Arrangment update processing       *
      *               *                                   *
      *               *************************************.
       F58.                                                             lv05
      *Creation of CX03, CX06, CX09
      *depending on what already
      *exists for the client and
      *account........
      *N58BA.    NOTE *CX03 needed                        *.
       F58BA.    IF    CX03-CF = '0'                                    lv10
                 NEXT SENTENCE ELSE GO TO     F58BA-FN.
           INITIALIZE  CX03
           MOVE        99 TO CX03-GELL
           MOVE        02 TO CX03-CARTY
           MOVE        01 TO CX03-NARRS
           MOVE        01 TO CX03-CARST
           MOVE        WM71-GECSQ TO CX03-GECSQ
           MOVE        ZERO TO CX03-CPMTG
           CX03-GRCRNG
           CX03-DEXDT
           MOVE        WM71-DCACG TO CX03-DASUP.
      *N58BD.    NOTE *Insert the record                  *.
       F58BD.                                                           lv15
           PERFORM     F94ID THRU F94ID-FN.
       F58BD-FN. EXIT.
      *N58BF.    NOTE *Insert failed                      *.
       F58BF.    IF    IK NOT = '0'                                     lv15
                 NEXT SENTENCE ELSE GO TO     F58BF-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012626 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F58BF-FN. EXIT.
       F58BA-FN. EXIT.
      *N58DA.    NOTE *CX06 Needed                        *.
       F58DA.    IF    CX06-CF = '0'                                    lv10
                 NEXT SENTENCE ELSE GO TO     F58DA-FN.
           INITIALIZE  CX06
           MOVE        WM71-CTID TO CX06-CTID
           MOVE        CX03-CX03K TO S-CXU03-CX03K.
      *N58DD.    NOTE *Insert the record                  *.
       F58DD.                                                           lv15
           PERFORM     F94IE THRU F94IE-FN
           MOVE        CX06-CX06K TO S-CXU06-CX06K.
       F58DD-FN. EXIT.
      *N58DF.    NOTE *Insert failed                      *.
       F58DF.    IF    IK NOT = '0'                                     lv15
                 NEXT SENTENCE ELSE GO TO     F58DF-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012626 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F58DF-FN. EXIT.
       F58DA-FN. EXIT.
      *N58GA.    NOTE *No previous CX09 found             *.
       F58GA.    IF    CX09-CF = '0'                                    lv10
                 NEXT SENTENCE ELSE GO TO     F58GA-FN.
           INITIALIZE  CX09
           MOVE        ZERO TO S-CXU06-CX06K
           MOVE        WM71-CTID TO S-CXU06-CTID
           MOVE        001 TO CX09-NPAIS
           MOVE        CX21-CDEL1 TO CX09-CDEL1
           MOVE        CX21-NDELS TO CX09-NDELS
           MOVE        01 TO CX09-CDEST
           MOVE        WM71-DCACG TO CX09-DISUP
           MOVE        001 TO CX09-CLUPD
           MOVE        WM71-DCACG TO CX09-DLAUP
           MOVE        WM71-GEOPD2 TO CX09-GEOPD2
           MOVE        ZERO TO CX09-DPCHD
      *Set up SSA's
           MOVE        CX01-CX01K TO S-CXU01-CX01K
           MOVE        CX03-CX03K TO S-CXU03-CX03K
           MOVE        CX06-CX06K TO S-CXU06-CX06K
      *Insert
           PERFORM     F94IF THRU F94IF-FN.
      *N58GD.    NOTE *Insert the CX09                    *.
       F58GD.    IF    IK = '1'                                         lv15
                 AND   XW05-XRC = 'II'
                 NEXT SENTENCE ELSE GO TO     F58GD-FN.
      *and duplicate...
           ADD         +1 TO CX09-NPAIS
           PERFORM     F94IF THRU F94IF-FN.
       F58GD-900. GO TO F58GD.
       F58GD-FN. EXIT.
      *N58GH.    NOTE *Insert failed....                  *.
       F58GH.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F58GH-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012626 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F58GH-FN. EXIT.
       F58GA-900. GO TO F58IA-FN.
       F58GA-FN. EXIT.
      *N58IA.    NOTE *Previous CX09 found                *.
       F58IA.                                                           lv10
      *Save for audit log purposes...
           MOVE        CX09 TO SV09
      *Path call to GHU CX09
           MOVE        CX01-CX01K TO S-CXU01-CX01K
           MOVE        CX03-CX03K TO S-CXU03-CX03K
           MOVE        CX06-CX06K TO S-CXU06-CX06K
           MOVE        CX09-CX09K TO S-CXU09-CX09K
           PERFORM     F94UD THRU F94UD-FN.
                 IF    IK NOT = '0'                                     DOT
      *Access failed... msg nbr wrong
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012626 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *Endif                                                            DOT
      *N58ID.    NOTE *Determine the changes              *.
       F58ID.                                                           lv15
      *Detetect multiple changes
           MOVE        0 TO WS01-UPDCNT.
       F58ID-FN. EXIT.
      *N58IH.    NOTE *Account Type                       *.
       F58IH.    IF    WM71-CCBAT NOT = SV18-CCBAT                      lv15
                 NEXT SENTENCE ELSE GO TO     F58IH-FN.
           MOVE        004 TO CX09-CLUPD
           ADD         +1 TO WS01-UPDCNT.
       F58IH-FN. EXIT.
      *N58IJ.    NOTE *Status Code not active - reset     *.
       F58IJ.    IF    CX09-CDEST NOT = 01                              lv15
                 NEXT SENTENCE ELSE GO TO     F58IJ-FN.
           MOVE        007 TO CX09-CLUPD
           MOVE        01 TO CX09-CDEST
           ADD         +1 TO WS01-UPDCNT.
       F58IJ-FN. EXIT.
      *N58IM.    NOTE *Signature Line                     *.
       F58IM.    IF    WM71-MCSIG NOT = SV18-MCSIG                      lv15
                 NEXT SENTENCE ELSE GO TO     F58IM-FN.
           MOVE        009 TO CX09-CLUPD
           ADD         +1 TO WS01-UPDCNT.
       F58IM-FN. EXIT.
      *N58IO.    NOTE *Check for multiple changes         *.
       F58IO.    IF    WS01-UPDCNT > 1                                  lv15
                 NEXT SENTENCE ELSE GO TO     F58IO-FN.
           MOVE        002 TO CX09-CLUPD.
       F58IO-FN. EXIT.
      *N58KA.    NOTE *Replace the CX09 record            *.
       F58KA.    IF    WS01-UPDCNT > ZERO                               lv15
                 NEXT SENTENCE ELSE GO TO     F58KA-FN.
      *Update basics
           MOVE        WM71-GEOPD2 TO CX09-GEOPD2.
                 IF    CX09-DLAUP NOT = WM71-DCACG                      DOT
      *Record previous update
           MOVE        CX09-DLAUP TO CX09-DPCHD
           MOVE        WM71-DCACG TO CX09-DLAUP.
      *Endif                                                            DOT
           PERFORM     F94RD THRU F94RD-FN.
       F58KA-FN. EXIT.
       F58IA-FN. EXIT.
       F58-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *Audit Log Processing               *
      *               *                                   *
      *               *************************************.
       F60.           EXIT.                                             lv05
      *N60AB.    NOTE *Audit log initialisations          *.
       F60AB.                                                           lv10
           INITIALIZE  DH10
           VA04
           VB04.
       F60AB-FN. EXIT.
      *N60AL.    NOTE *Set up audit log info              *.
       F60AL.                                                           lv10
           MOVE        'CLIENT' TO DH10-MAUSB
           MOVE        WM71-CLID TO DH10-NAUSK
           MOVE        CX03-CARTY TO VA04-CARTY
           VB04-CARTY
           MOVE        CX03-NARRS TO VA04-NARRS
           VB04-NARRS
           MOVE        CX03-DASUP TO VA04-DASUP
           VB04-DASUP
           MOVE        CT01-CTSTA TO VA04-CTSTA
           VB04-CTSTA.
       F60AL-FN. EXIT.
      *N60BA.    NOTE *Either a bank or arrangement       *.
       F60BA.    IF    CX18-CF = '0'                                    lv10
                 OR    CX06-CF = '0'
                 NEXT SENTENCE ELSE GO TO     F60BA-FN.
      *has been added
           MOVE        70006 TO DH10-CAUFR
           MOVE        CT01-CTID TO VA04-CTID
           VB04-CTID
           MOVE        CX21-CDEL1 TO VB04-CDELI
           MOVE        CX18-NPBN TO VB04-NPBN
           MOVE        CX18-CCBAT TO VB04-CCBAT
           MOVE        CX18-CLID TO VB04-CLID
           MOVE        CX18-MCSIG TO VB04-MCSIG
           MOVE        CX09-CDEST TO VB04-CDEST.
      *N60BD.    NOTE *VA04 is for adding an account      *.
       F60BD.    IF    CX06-CF = '0'                                    lv15
                 NEXT SENTENCE ELSE GO TO     F60BD-FN.
           MOVE        70006 TO DH10-CAUFR
           MOVE        1 TO DH10-CAUAC
           MOVE        VA04 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F60BD-FN. EXIT.
      *N60BG.    NOTE *VB is for adding a bank            *.
       F60BG.    IF    CX18-CF = '0'                                    lv15
                 OR    (CX06-CF = '1'
                 AND   CX09-CF = '0'
                 AND   CX18-CF = '1')
                 NEXT SENTENCE ELSE GO TO     F60BG-FN.
      *Note: The account (CX06) may
      *already exist as may the bank
      *(CX18). However the two may not
      *be connected. Therefore where
      *only a CX09 is inserted the
      *audit log process should be the
      *same as if the bank was being
      *added. This is to ensure the
      *CX09 creation is recorded...
           MOVE        70006 TO DH10-CAUFR
           MOVE        3 TO DH10-CAUAC
           MOVE        VB04 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F60BG-FN. EXIT.
       F60BA-FN. EXIT.
      *N60DA.    NOTE *Either the bank details or the     *.
       F60DA.    IF    (CX18-CF = '1'                                   lv10
                 AND   CX18 NOT = SV18)
                 OR    (CX09-CF = '1'
                 AND   CX09 NOT = SV09)
                 NEXT SENTENCE ELSE GO TO     F60DA-FN.
      *arrangement details have changed
      *(Arr detail changes would be the
      *reactivation of an old CX09)
      *N60DC.    NOTE *Build after image                  *.
       F60DC.                                                           lv15
           MOVE        70006 TO DH10-CAUFR
           MOVE        WM71-CTID TO VA04-CTID
           VB04-CTID
           MOVE        CX21-CDEL1 TO VA04-CDELI
           MOVE        CX18-NPBN TO VA04-NPBN
           MOVE        CX18-CCBAT TO VA04-CCBAT
           MOVE        CX18-CLID TO VA04-CLID
           MOVE        CX18-MCSIG TO VA04-MCSIG
           MOVE        CX09-CDEST TO VA04-CDEST.
      *N60DD.    NOTE *Bank change: Format before image   *.
       F60DD.    IF    CX18 NOT = SV18                                  lv20
                 NEXT SENTENCE ELSE GO TO     F60DD-FN.
           MOVE        CX21-CDEL1 TO VB04-CDELI
           MOVE        SV18-NPBN TO VB04-NPBN
           MOVE        SV18-CCBAT TO VB04-CCBAT
           MOVE        SV18-CLID TO VB04-CLID
           MOVE        SV18-MCSIG TO VB04-MCSIG
           MOVE        SV09-CDEST TO VB04-CDEST.
       F60DD-900. GO TO F60DH-FN.
       F60DD-FN. EXIT.
      *N60DH.    NOTE *Status change: fmt before image    *.
       F60DH.                                                           lv20
           MOVE        CX21-CDEL1 TO VB04-CDELI
           MOVE        CX18-NPBN TO VB04-NPBN
           MOVE        CX18-CLID TO VB04-CLID
           MOVE        SV09-CDEST TO VB04-CDEST.
       F60DH-FN. EXIT.
      *N60DR.    NOTE *Audit Log: Before image            *.
       F60DR.                                                           lv20
           MOVE        70006 TO DH10-CAUFR
           MOVE        5 TO DH10-CAUAC
           MOVE        VB04 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F60DR-FN. EXIT.
      *N60DU.    NOTE *Audit Log: After image             *.
       F60DU.                                                           lv20
           MOVE        70006 TO DH10-CAUFR
           MOVE        6 TO DH10-CAUAC
           MOVE        VA04 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F60DU-FN. EXIT.
       F60DC-FN. EXIT.
       F60DA-FN. EXIT.
       F60-FN.   EXIT.
      *N62.      NOTE *************************************.
      *               *                                   *
      *               *Client Communications              *
      *               *                                   *
      *               *************************************.
       F62.           EXIT.                                             lv05
      *N62BA.    NOTE *Create confirmation whenever an    *.
       F62BA.    IF    AUDIT-LOG-WRITTEN                                lv10
                 NEXT SENTENCE ELSE GO TO     F62BA-FN.
      *audit log entry has been made
           MOVE        SPACES TO GU01
           MOVE        +59 TO GU01-GELL
           MOVE        WM71-CLID TO GU01-CANUMB
           MOVE        202 TO GU01-CREQT
           MOVE        1 TO GU01-NSEQ2P
           GU01-IDTYP
           GU01-CCOPY
           MOVE        WM71-DCACG TO GU01-DCACG
           MOVE        CX03-CARTY TO GU01-CARTY
           MOVE        CX03-NARRS TO GU01-NARRS
           MOVE        PROGR TO GU01-MRPID.
                 IF    CX03-CF = '1'                                    DOT
      *Arrangement existed
           MOVE        02 TO GU01-NLPGH
                 ELSE
      *Arrangement not found
           MOVE        03 TO GU01-NLPGH.
      *Endif                                                            DOT
      *N62BD.    NOTE *Confirm creation of a new          *.
       F62BD.                                                           lv15
      *arrangement for an account...
      *Or... new bank details
      *Attempt to insert GU01
           PERFORM     F94IG THRU F94IG-FN.
       F62BD-FN. EXIT.
      *N62BG.    NOTE *Insert failed... overwrite         *.
       F62BG.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F62BG-FN.
           MOVE        GU01-GU01K TO S-GUU01-GU01K
           PERFORM     F94HE THRU F94HE-FN.
                 IF    IK = '0'                                         DOT
      *Found GU01 then update date
           MOVE        WM71-DCACG TO GU01-DCACG
           PERFORM     F94RE THRU F94RE-FN.
      *Endif                                                            DOT
       F62BG-FN. EXIT.
      *N62BK.    NOTE *Update still failed...             *.
       F62BK.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F62BK-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012626 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F62BK-FN. EXIT.
       F62BA-FN. EXIT.
       F62-FN.   EXIT.
      *N64.      NOTE *************************************.
      *               *                                   *
      *               *Update output segments             *
      *               *                                   *
      *               *************************************.
       F64.           EXIT.                                             lv05
      *N64BA.    NOTE *Default indicators...              *.
       F64BA.                                                           lv10
      *Note: As this module will
      *either update records or
      *return an error message...
      *Linkage is only updated once
      *all updates are complete.....
           MOVE        CX01 TO LX01
           MOVE        CX03 TO LX03
           MOVE        CX06 TO LX06
           MOVE        CX09 TO LX09
           MOVE        CX18 TO LX18
           MOVE        CX21 TO LX21
           MOVE        'Y' TO WM72-ICX01
           MOVE        'Y' TO WM72-ICX03
           MOVE        'Y' TO WM72-ICX06
           MOVE        'Y' TO WM72-ICX09
           MOVE        'Y' TO WM72-ICX18
           MOVE        'Y' TO WM72-ICX21.
       F64BA-FN. EXIT.
       F64-FN.   EXIT.
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
      *N92TB.    NOTE *RANDOM TABLE READ FOR TA5B         *.            ADUTAB
       F92TB.                                                           lv10
           MOVE        'R1' TO G-TA5B-TABFO                             ADUTAB
           COMPUTE     G-TA5B-LTH = 60 + G-TA5B-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA5B-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA5B)                                ADUTAB
                       LENGTH (G-TA5B-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA5B-TABCR NOT = '00'                          DOT
           PERFORM     F92TC THRU F92TC-FN.                             ADUTAB
       F92TB-FN. EXIT.
      *N92TC.    NOTE *Error processing for TB5B          *.
       F92TC.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012405 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F92TC-FN. EXIT.
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
      *               *DL/I Calls                         *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94CT.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94CT.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CT-FN. EXIT.
      *N94CY.    NOTE *CALL GN ON CX2Y                    *.            ADU026
       F94CY.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PC06 CX2Y                                                    ADU026
           S-CXU2Y-SSA                                                  ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CY-FN. EXIT.
      *N94C1.    NOTE *CALL GN ON CX21                    *.            ADU026
       F94C1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX21' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CX21                                                    ADU026
           S-CXU01-SSA S-CX21-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C1-FN. EXIT.
      *N94C2.    NOTE *CALL GU ON CL24                    *.            ADU026
       F94C2.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL24' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PF06 CL24                                                    ADU026
           S-CLU01-SSA S-CLU24-SSA                                      ADU026
           MOVE        PF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C2-FN. EXIT.
      *N94C3.    NOTE *CALL GU ON CL01                    *.            ADU026
       F94C3.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PF06 CL01                                                    ADU026
           S-CLU01-SSA                                                  ADU026
           MOVE        PF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C3-FN. EXIT.
      *N94C4.    NOTE *CALL GNP ON CL24                   *.            ADU026
       F94C4.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL24' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGNP                        ADU026
           PF06 CL24                                                    ADU026
           S-CLU01-SSA S-CL24-SSA                                       ADU026
           MOVE        PF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGNP TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C4-FN. EXIT.
      *N94HE.    NOTE *CALL GHU ON GU01                   *.            ADU026
       F94HE.                                                           lv10
           MOVE        'AGCP' TO DE10-XDBDNM                            ADU026
           MOVE        'GU01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PE06 GU01                                                    ADU026
           S-GUU01-SSA                                                  ADU026
           MOVE        PE06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94HE-FN. EXIT.
      *N94IA.    NOTE *CALL ISRT ON CX01                  *.            ADU026
       F94IA.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 CX01                                                    ADU026
           S-CX01-SSA                                                   ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94IA-FN. EXIT.
      *N94IB.    NOTE *CALL ISRT ON CX18                  *.            ADU026
       F94IB.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 CX18                                                    ADU026
           S-CXU01-SSA S-CX18-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94IB-FN. EXIT.
      *N94IC.    NOTE *CALL ISRT ON CX21                  *.            ADU026
       F94IC.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX21' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 CX21                                                    ADU026
           S-CXU01-SSA S-CX21-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94IC-FN. EXIT.
      *N94ID.    NOTE *CALL ISRT ON CX03                  *.            ADU026
       F94ID.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 CX03                                                    ADU026
           S-CXU01-SSA S-CX03-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94ID-FN. EXIT.
      *N94IE.    NOTE *CALL ISRT ON CX06                  *.            ADU026
       F94IE.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 CX06                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CX06-SSA                           ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94IE-FN. EXIT.
      *N94IF.    NOTE *CALL ISRT ON CX09                  *.            ADU026
       F94IF.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX09' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 CX09                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CXU06-SSA                          ADU026
           S-CX09-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94IF-FN. EXIT.
      *N94IG.    NOTE *CALL ISRT ON GU01                  *.            ADU026
       F94IG.                                                           lv10
           MOVE        'AGCP' TO DE10-XDBDNM                            ADU026
           MOVE        'GU01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PE06 GU01                                                    ADU026
           S-GU01-SSA                                                   ADU026
           MOVE        PE06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94IG-FN. EXIT.
      *N94IH.    NOTE *CALL ISRT ON CX19                  *.            ADU026
       F94IH.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX19' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 CX19                                                    ADU026
           S-CXU01-SSA S-CXU18-SSA                                      ADU026
           S-CX19-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94IH-FN. EXIT.
      *N94N1.    NOTE *CALL GN ON CX18                    *.            ADU026
       F94N1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CX18                                                    ADU026
           S-CXU01-SSA S-CX18-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94N1-FN. EXIT.
      *N94N2.    NOTE *CALL GN ON CX09                    *.            ADU026
       F94N2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX09' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CX09                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CXU06-SSA                          ADU026
           S-CX09-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94N2-FN. EXIT.
      *N94N3.    NOTE *CALL GN ON CX03                    *.            ADU026
       F94N3.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CX03                                                    ADU026
           S-CXU01-SSA S-CX03-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94N3-FN. EXIT.
      *N94RA.    NOTE *CALL REPL ON CX01                  *.            ADU026
       F94RA.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PA06 CX01                                                    ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94RA-FN. EXIT.
      *N94RB.    NOTE *CALL REPL ON CX18                  *.            ADU026
       F94RB.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PA06 CX18                                                    ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94RB-FN. EXIT.
      *N94RD.    NOTE *CALL REPL ON CX09                  *.            ADU026
       F94RD.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX09' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PA06 CX09                                                    ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94RD-FN. EXIT.
      *N94RE.    NOTE *CALL REPL ON GU01                  *.            ADU026
       F94RE.                                                           lv10
           MOVE        'AGCP' TO DE10-XDBDNM                            ADU026
           MOVE        'GU01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PE06 GU01                                                    ADU026
           MOVE        PE06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94RE-FN. EXIT.
      *N94UA.    NOTE *CALL GHU ON CX01                   *.            ADU026
       F94UA.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PA06 CX01                                                    ADU026
           S-CXU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94UA-FN. EXIT.
      *N94UB.    NOTE *CALL GHU ON CX18                   *.            ADU026
       F94UB.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PA06 CX18                                                    ADU026
           S-CXU01-SSA S-CXU18-SSA                                      ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94UB-FN. EXIT.
      *N94UD.    NOTE *CALL GHU ON CX09                   *.            ADU026
       F94UD.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX09' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PA06 CX09                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CXU06-SSA                          ADU026
           S-CXU09-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94UD-FN. EXIT.
      *N94U1.    NOTE *CALL GU ON CX01                    *.            ADU026
       F94U1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX01                                                    ADU026
           S-CXU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94U1-FN. EXIT.
      *N94U2.    NOTE *CALL GU ON CX03                    *.            ADU026
       F94U2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX03                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94U2-FN. EXIT.
      *N94U3.    NOTE *CALL GU ON CX06                    *.            ADU026
       F94U3.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX06                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CXU06-SSA                          ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94U3-FN. EXIT.
       F94-FN.   EXIT.
      *N95DD.    NOTE *CDU - DATE VALIDATE/CONVERT        *.            AADA81
       F95DD.                                                           lv10
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
       F95DD-FN. EXIT.
      *N96AL.    NOTE *---> Audit Log Process             *.            ADU165
       F96AL.                                                           lv10
           SET AUDIT-LOG-WRITTEN TO TRUE.
      *N96AN.    NOTE *---> Format Audit Log Data         *.            ADU165
       F96AN.                                                           lv15
           SET AL00-NPNTR                                               ADU165
           TO ADDRESS OF DLIUIBII                                       ADU165
           MOVE        AL00-ADDR TO DH10-XUIBP                          ADU165
           MOVE        AL00-NSEQ2P TO DH10-NSEQ2P                       ADU165
           MOVE        'E' TO DH10-CAUL                                 ADU165
           MOVE        'CLIENT' TO DH10-MAUSB                           ADU165
           MOVE        WM71-CLID TO DH10-NAUSK                          ADU165
           MOVE        'CATS' TO DH10-CSYS                              ADU165
           MOVE        EIBTRNID TO DH10-CAPPL                           ADU165
           MOVE        'C' TO DH10-CAUSR                                ADU165
           MOVE        WM71-GEOPD2 TO DH10-GEOPID
           MOVE        WM71-CAUNIT TO DH10-CAUNIT
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
