       IDENTIFICATION DIVISION.                                         CI0022
       PROGRAM-ID.  CI0022P.                                            CI0022
      *AUTHOR.         M\M - BA/BILLING EDITS MODULE.                   CI0022
      *DATE-COMPILED.   09/08/14.                                       CI0022
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
       ENVIRONMENT DIVISION.                                            CI0022
       CONFIGURATION SECTION.                                           CI0022
       SOURCE-COMPUTER. IBM-370.                                        CI0022
       OBJECT-COMPUTER. IBM-370.                                        CI0022
       DATA DIVISION.                                                   CI0022
       WORKING-STORAGE SECTION.                                         CI0022
      *                                                                 AM0023
      ******************************************************************AM0023
      **     SEGMENT THAT CONTAINS THE FUNDS BILLING RECORD            *AM0023
      ******************************************************************AM0023
      *                                                                 AM0023
      *!WF DSP=BP DSL=BP SEL=0001 FOR=I LEV=1                           AM0023
       01                 BP00.                                         CI0022
          05              BP00-00.                                      CI0022
            10            BP00-C299.                                    CI0022
            11            BP00-CTID.                                    CI0022
            12            BP00-CTIDA  PICTURE  9(3).                    CI0022
            12            BP00-CTIDN.                                   CI0022
            13            BP00-CTIDNP PICTURE  X(13).                   CI0022
            13            BP00-CTIDND PICTURE  9(11).                   CI0022
          05              BP00-SUITE.                                   CI0022
            15       FILLER         PICTURE  X(00185).                  CI0022
       01                 BP01  REDEFINES      BP00.                    CI0022
            10       FILLER         PICTURE  X(00027).                  CI0022
            10            BP01-DNXBL  PICTURE  9(8).                    CI0022
            10            BP01-DLPUR  PICTURE  9(8).                    CI0022
            10            BP01-CEBAM  PICTURE  S9(7)V99                 CI0022
                          COMPUTATIONAL-3.                              CI0022
            10            BP01-CBILM  PICTURE  9(2).                    CI0022
            10            BP01-ABILL  PICTURE  S9(7)V99                 CI0022
                          COMPUTATIONAL-3.                              CI0022
            10            BP01-CEBTP  PICTURE  9(2).                    CI0022
            10            BP01-NCBIL  PICTURE  9(5).                    CI0022
            10            BP01-DL33.                                    CI0022
            11            BP01-CESNA1 PICTURE  X(30).                   CI0022
            11            BP01-CESNA2 PICTURE  X(30).                   CI0022
            11            BP01-CESNA3 PICTURE  X(30).                   CI0022
            11            BP01-CESNA4 PICTURE  X(30).                   CI0022
            11            BP01-CESNA5 PICTURE  X(30).                   CI0022
            10            BP01-DL34                                     CI0022
                          REDEFINES            BP01-DL33.               CI0022
            11            BP01-CESNA  PICTURE  X(30)                    CI0022
                          OCCURS       005     TIMES.                   CI0022
      *                                                                 AM0023
      *                                                                 AM0023
      *                                                                 AM0023
      *                                                                 AM0023
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0023           PIC X(8) VALUE 'CI0023P '.                  AM0023
       01  CI0975           PIC X(08) VALUE 'CI0975P'.                  AM0975
       01                 CX00.                                         CI0022
            02            CX01.                                         CI0022
            10            CX01-CX01K.                                   CI0022
            11            CX01-C199.                                    CI0022
            12            CX01-CLID.                                    CI0022
            13            CX01-CLIDO  PICTURE  9(3).                    CI0022
            13            CX01-CLIDN.                                   CI0022
            14            CX01-CLIDNP PICTURE  X(12).                   CI0022
            14            CX01-CLIDND PICTURE  9(8).                    CI0022
            10            CX01-GEMDA  PICTURE  9(8).                    CI0022
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0022
                          BINARY.                                       CI0022
            10            CX01-FILLER PICTURE  X(5).                    CI0022
            02            CX03.                                         CI0022
            10            CX03-GELL   PICTURE  9(4)                     CI0022
                          BINARY.                                       CI0022
            10            CX03-CY00.                                    CI0022
            11            CX03-CX03K.                                   CI0022
            12            CX03-CARTY  PICTURE  99.                      CI0022
            12            CX03-NARRS  PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX03-CARST  PICTURE  99.                      CI0022
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX03-CPMTG  PICTURE  99.                      CI0022
            11            CX03-GRCRNG PICTURE  9(3).                    CI0022
            11            CX03-DEXDT  PICTURE  9(8).                    CI0022
            11            CX03-DASUP  PICTURE  9(8).                    CI0022
            11            CX03-CSTEC  PICTURE  X(3).                    CI0022
            11            CX03-FILLER PICTURE  X(17).                   CI0022
            11            CX03-CY50.                                    CI0022
            12            CX03-NARID  PICTURE  X(30).                   CI0022
            11            CX03-CY51                                     CI0022
                          REDEFINES            CX03-CY50.               CI0022
            12            CX03-NDIDN  PICTURE  9(12).                   CI0022
            12            CX03-FILLER PICTURE  X(18).                   CI0022
            11            CX03-CY52                                     CI0022
                          REDEFINES            CX03-CY50.               CI0022
            12            CX03-NAIDC  PICTURE  9(12).                   CI0022
            12            CX03-FILLER PICTURE  X(18).                   CI0022
            11            CX03-CY53                                     CI0022
                          REDEFINES            CX03-CY50.               CI0022
            12            CX03-NAMEXB PICTURE  9(15).                   CI0022
            12            CX03-FILLER PICTURE  X(15).                   CI0022
            10            CX03-CY99.                                    CI0022
            11            CX03-FILLER PICTURE  X(109).                  CI0022
            10            CX03-CY01                                     CI0022
                          REDEFINES            CX03-CY99.               CI0022
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX03-ICPCI  PICTURE  X.                       CI0022
            11            CX03-CLUPD  PICTURE  9(3).                    CI0022
            11            CX03-DLAUP  PICTURE  9(8).                    CI0022
            11            CX03-CWRC   PICTURE  99.                      CI0022
            11            CX03-CHCR   PICTURE  99.                      CI0022
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0022
            11            CX03-GEAUN  PICTURE  9(5).                    CI0022
            11            CX03-DPCHD  PICTURE  9(8).                    CI0022
            11            CX03-DLRCHK PICTURE  9(8).                    CI0022
            11            CX03-QTRCHK PICTURE  9(2).                    CI0022
            11            CX03-DNPMT  PICTURE  9(8).                    CI0022
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0022
                          COMPUTATIONAL-3.                              CI0022
            10            CX03-CY02                                     CI0022
                          REDEFINES            CX03-CY99.               CI0022
            11            CX03-QSIRQ  PICTURE  99.                      CI0022
            11            CX03-QDRMN  PICTURE  9(2)                     CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX03-DDPRE  PICTURE  9(8).                    CI0022
            11            CX03-DDSHP  PICTURE  9(8).                    CI0022
            11            CX03-NDRFTB PICTURE  9(5).                    CI0022
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0022
            11            CX03-DDSHPA PICTURE  9(8).                    CI0022
            11            CX03-NDRFTF PICTURE  9(5).                    CI0022
            11            CX03-QDIPBK PICTURE  9(3).                    CI0022
            11            CX03-CREOR  PICTURE  X(1).                    CI0022
            11            CX03-CREOR1 PICTURE  X(1).                    CI0022
            11            CX03-DDASC  PICTURE  9(8).                    CI0022
            11            CX03-FILLER PICTURE  X(7).                    CI0022
            10            CX03-CY03                                     CI0022
                          REDEFINES            CX03-CY99.               CI0022
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0022
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0022
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0022
            11            CX03-DOPDA  PICTURE  99.                      CI0022
            11            CX03-CPMTF  PICTURE  99.                      CI0022
            11            CX03-CIRMO  PICTURE  X(12).                   CI0022
            11            CX03-CPALL  PICTURE  X(1).                    CI0022
            11            CX03-CCOLM  PICTURE  9(2).                    CI0022
            11            CX03-CBLTP  PICTURE  X(1).                    CI0022
            11            CX03-CASUB  PICTURE  9(2).                    CI0022
            11            CX03-CBLFM  PICTURE  9(2).                    CI0022
            11            CX03-IBILS  PICTURE  X.                       CI0022
            11            CX03-IPAOS  PICTURE  X.                       CI0022
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0022
            11            CX03-DLBPD  PICTURE  9(8).                    CI0022
            11            CX03-DNBPD  PICTURE  9(8).                    CI0022
            11            CX03-DODBD  PICTURE  9(8).                    CI0022
            11            CX03-CPSRE  PICTURE  99.                      CI0022
            11            CX03-ISPHN  PICTURE  X.                       CI0022
            11            CX03-TCARR  PICTURE  X(6).                    CI0022
            11            CX03-CBKPT  PICTURE  9(2).                    CI0022
            11            CX03-IECNT  PICTURE  X.                       CI0022
            11            CX03-ICONV  PICTURE  X(1).                    CI0022
            11            CX03-FILLER PICTURE  X(4).                    CI0022
            10            CX03-CY04                                     CI0022
                          REDEFINES            CX03-CY99.               CI0022
            11            CX03-CCARD  PICTURE  X(02).                   CI0022
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0022
            11            CX03-IREMT  PICTURE  X(01).                   CI0022
            11            CX03-ISBILA PICTURE  X.                       CI0022
            11            CX03-DLBPDA PICTURE  9(8).                    CI0022
            11            CX03-DNBPDA.                                  CI0022
            12            CX03-DNCYM  PICTURE  9(6).                    CI0022
            12            CX03-CEDTD  PICTURE  9(2).                    CI0022
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX03-DREMT  PICTURE  9(8).                    CI0022
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0022
            11            CX03-CWRC2  PICTURE  99.                      CI0022
            11            CX03-CHCR2  PICTURE  99.                      CI0022
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0022
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0022
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0022
            02            CX06.                                         CI0022
            10            CX06-CX06K.                                   CI0022
            11            CX06-C299.                                    CI0022
            12            CX06-CTID.                                    CI0022
            13            CX06-CTIDA  PICTURE  9(3).                    CI0022
            13            CX06-CTIDN.                                   CI0022
            14            CX06-CTIDNP PICTURE  X(13).                   CI0022
            14            CX06-CTIDND PICTURE  9(11).                   CI0022
            10            CX06-NPECK  PICTURE  9(02).                   CI0022
            10            CX06-FILLER PICTURE  X.                       CI0022
            02            CX12.                                         CI0022
            10            CX12-CX12K.                                   CI0022
            11            CX12-CPMTC  PICTURE  99.                      CI0022
            11            CX12-NAPDS  PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX12-GESTD  PICTURE  9(8).                    CI0022
            10            CX12-GEEND  PICTURE  9(8).                    CI0022
            10            CX12-CIRMO  PICTURE  X(12).                   CI0022
            10            CX12-CDEST  PICTURE  99.                      CI0022
            10            CX12-APMTL  PICTURE  S9(9)V99                 CI0022
                          COMPUTATIONAL-3.                              CI0022
            10            CX12-DNPMT  PICTURE  9(8).                    CI0022
            10            CX12-NIRACM PICTURE  9(2).                    CI0022
            10            CX12-CPMTF  PICTURE  99.                      CI0022
            10            CX12-IPODM  PICTURE  X.                       CI0022
            10            CX12-CLUPD  PICTURE  9(3).                    CI0022
            10            CX12-DLAUP  PICTURE  9(8).                    CI0022
            10            CX12-CWRC   PICTURE  99.                      CI0022
            10            CX12-CHCR   PICTURE  99.                      CI0022
            10            CX12-GEOPD2 PICTURE  X(8).                    CI0022
            10            CX12-GEAUN  PICTURE  9(5).                    CI0022
            10            CX12-DPCHD  PICTURE  9(8).                    CI0022
            10            CX12-DNEXE  PICTURE  9(8).                    CI0022
            10            CX12-CCSMQ  PICTURE  X.                       CI0022
            10            CX12-GCUSPZ PICTURE  X(12).                   CI0022
            10            CX12-CORTY  PICTURE  X.                       CI0022
            10            CX12-CNAVR  PICTURE  X(1).                    CI0022
            10            CX12-DELOI3 PICTURE  9(6).                    CI0022
            10            CX12-ALOIDD PICTURE  9(9)V99                  CI0022
                          COMPUTATIONAL-3.                              CI0022
            10            CX12-FILLER PICTURE  X(5).                    CI0022
            02            CX13.                                         CI0022
            10            CX13-GELL   PICTURE  9(4)                     CI0022
                          BINARY.                                       CI0022
            10            CX13-CY20.                                    CI0022
            11            CX13-CX13K.                                   CI0022
            12            CX13-CARTZ  PICTURE  99.                      CI0022
            12            CX13-NAPDS  PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX13-GESTD  PICTURE  9(8).                    CI0022
            11            CX13-GEEND  PICTURE  9(8).                    CI0022
            11            CX13-DASUQ  PICTURE  9(8).                    CI0022
            11            CX13-CDEST  PICTURE  99.                      CI0022
            11            CX13-IIARR  PICTURE  X.                       CI0022
            11            CX13-DLAUP  PICTURE  9(8).                    CI0022
            11            CX13-GEOPD2 PICTURE  X(8).                    CI0022
            11            CX13-GEAUN  PICTURE  9(5).                    CI0022
            11            CX13-DPCHD  PICTURE  9(8).                    CI0022
            11            CX13-PPOT1  PICTURE  S9(3)V99                 CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX13-ACOT1  PICTURE  S9(9)V99                 CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX13-QPST1  PICTURE  S9(7)V999                CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX13-FILLER PICTURE  X(03).                   CI0022
            10            CX13-CY96.                                    CI0022
            11            CX13-FILLER PICTURE  X(50).                   CI0022
            10            CX13-CY21                                     CI0022
                          REDEFINES            CX13-CY96.               CI0022
            11            CX13-DNPMT  PICTURE  9(8).                    CI0022
            11            CX13-CPMTF  PICTURE  99.                      CI0022
            11            CX13-ADBRQ  PICTURE  S9(11)V99                CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX13-QSHOWQ PICTURE  S9(9)V999                CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX13-PACT1  PICTURE  S999V999                 CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX13-DOPDA  PICTURE  99.                      CI0022
            11            CX13-DNEXE  PICTURE  9(8).                    CI0022
            11            CX13-CIRMO  PICTURE  X(12).                   CI0022
            10            CX13-CY98.                                    CI0022
            11            CX13-FILLER PICTURE  X(120).                  CI0022
            10            CX13-CY25                                     CI0022
                          REDEFINES            CX13-CY98.               CI0022
            11            CX13-COPTC  PICTURE  9(1).                    CI0022
            11            CX13-ILPOI  PICTURE  X(1).                    CI0022
            11            CX13-CATOC  PICTURE  X(1).                    CI0022
            11            CX13-CEOIA  PICTURE  S9(7)V99                 CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX13-ACOAR  PICTURE  S9(9)V99                 CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX13-CEOTR  PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX13-DSTMO  PICTURE  99.                      CI0022
            10            CX13-CY27                                     CI0022
                          REDEFINES            CX13-CY98.               CI0022
            11            CX13-QMTH1  PICTURE  9(3).                    CI0022
            11            CX13-IDRMD  PICTURE  X.                       CI0022
            10            CX13-CY28                                     CI0022
                          REDEFINES            CX13-CY98.               CI0022
            11            CX13-AALLBL PICTURE  S9(8)V99                 CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX13-PSURR  PICTURE  S9(3)V999                CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX13-DFPMT  PICTURE  9(8).                    CI0022
            11            CX13-QMTHLA PICTURE  9(3).                    CI0022
            11            CX13-PWHLDS PICTURE  S999V9(5)                CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX13-ISWHO  PICTURE  X(1).                    CI0022
            10            CX13-CY29                                     CI0022
                          REDEFINES            CX13-CY98.               CI0022
            11            CX13-IINDI1 PICTURE  X(1).                    CI0022
            11            CX13-IINDI2 PICTURE  X(1).                    CI0022
            11            CX13-IINDI3 PICTURE  X(1).                    CI0022
            11            CX13-PWHLD5 PICTURE  S999V99                  CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX13-CCSMQ  PICTURE  X.                       CI0022
            11            CX13-CPLEC  PICTURE  XX.                      CI0022
            11            CX13-IPTRDA PICTURE  X(01).                   CI0022
            11            CX13-GCUSPY PICTURE  X(12).                   CI0022
            11            CX13-ALOIDA PICTURE  S9(11)V99                CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX13-DELOI  PICTURE  9(8).                    CI0022
            11            CX13-CLGND  PICTURE  X.                       CI0022
            11            CX13-CORTYA PICTURE  X(3).                    CI0022
            11            CX13-CPH3U  PICTURE  X.                       CI0022
            11            CX13-CNAVR  PICTURE  X(1).                    CI0022
            11            CX13-NEXEC  PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            02            CX2Y.                                         CI0022
            10            CX2Y-CX2YK.                                   CI0022
            11            CX2Y-C299.                                    CI0022
            12            CX2Y-CTID.                                    CI0022
            13            CX2Y-CTIDA  PICTURE  9(3).                    CI0022
            13            CX2Y-CTIDN.                                   CI0022
            14            CX2Y-CTIDNP PICTURE  X(13).                   CI0022
            14            CX2Y-CTIDND PICTURE  9(11).                   CI0022
            11            CX2Y-C199.                                    CI0022
            12            CX2Y-CLID.                                    CI0022
            13            CX2Y-CLIDO  PICTURE  9(3).                    CI0022
            13            CX2Y-CLIDN.                                   CI0022
            14            CX2Y-CLIDNP PICTURE  X(12).                   CI0022
            14            CX2Y-CLIDND PICTURE  9(8).                    CI0022
            11            CX2Y-CARTY  PICTURE  99.                      CI0022
            11            CX2Y-NARRS  PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            02            CX6Y.                                         CI0022
            10            CX6Y-CX6YK.                                   CI0022
            11            CX6Y-C299.                                    CI0022
            12            CX6Y-CTID.                                    CI0022
            13            CX6Y-CTIDA  PICTURE  9(3).                    CI0022
            13            CX6Y-CTIDN.                                   CI0022
            14            CX6Y-CTIDNP PICTURE  X(13).                   CI0022
            14            CX6Y-CTIDND PICTURE  9(11).                   CI0022
            11            CX6Y-C199.                                    CI0022
            12            CX6Y-CLID.                                    CI0022
            13            CX6Y-CLIDO  PICTURE  9(3).                    CI0022
            13            CX6Y-CLIDN.                                   CI0022
            14            CX6Y-CLIDNP PICTURE  X(12).                   CI0022
            14            CX6Y-CLIDND PICTURE  9(8).                    CI0022
            11            CX6Y-CARTY  PICTURE  99.                      CI0022
            11            CX6Y-NARRS  PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX6Y-CTID1  PICTURE  X(27).                   CI0022
            11            CX6Y-CARTZ  PICTURE  99.                      CI0022
            11            CX6Y-NAPDS  PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            CX6Y-NPISQ  PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
      ******************************************************************AM0975
      *  LINKAGE SEGMENTS FOR CI0975                                    AM0975
      ******************************************************************AM0975
      *                                                                 AM0975
      *!WF DSP=DA DSL=QT SEL=8G FOR=I DES=2 LEV=1                       AM0975
       01                 DA8G.                                         CI0022
            10            DA8G-CFAUL1 PICTURE  X(4)                     CI0022
                          VALUE                SPACE.                   CI0022
            10            DA8G-TFACT1 PICTURE  X(20)                    CI0022
                          VALUE                SPACE.                   CI0022
            10            DA8G-CFAUL2 PICTURE  X(4)                     CI0022
                          VALUE                SPACE.                   CI0022
            10            DA8G-TFACT2 PICTURE  X(20)                    CI0022
                          VALUE                SPACE.                   CI0022
            10            DA8G-CFAUL3 PICTURE  X(4)                     CI0022
                          VALUE                SPACE.                   CI0022
            10            DA8G-TFACT3 PICTURE  X(20)                    CI0022
                          VALUE                SPACE.                   CI0022
            10            DA8G-CARTY  PICTURE  99                       CI0022
                          VALUE                ZERO.                    CI0022
            10            DA8G-CSYS   PICTURE  X(4)                     CI0022
                          VALUE                SPACE.                   CI0022
            10            DA8G-GERTC  PICTURE  X                        CI0022
                          VALUE                SPACE.                   CI0022
            10            DA8G-CERRE  PICTURE  X(4)                     CI0022
                          VALUE                SPACE.                   CI0022
            10            DA8G-QITEM  PICTURE  9(3)                     CI0022
                          VALUE                ZERO.                    CI0022
            10            DA8G-FILLER PICTURE  X(32400)                 CI0022
                          VALUE                SPACE.                   CI0022
            10            DA8G-QT8M                                     CI0022
                          REDEFINES            DA8G-FILLER.             CI0022
            11            DA8G-QT8I                                     CI0022
                          OCCURS       090     TIMES.                   CI0022
            12            DA8G-CTID   PICTURE  X(27).                   CI0022
            12            DA8G-CARST  PICTURE  99.                      CI0022
            12            DA8G-CPMTF  PICTURE  99.                      CI0022
            12            DA8G-CPALL  PICTURE  X(1).                    CI0022
            12            DA8G-PPOTD  PICTURE  S9(3)V99                 CI0022
                          COMPUTATIONAL-3.                              CI0022
            12            DA8G-ACOTD  PICTURE  S9(9)V99                 CI0022
                          COMPUTATIONAL-3.                              CI0022
            12            DA8G-GESTD  PICTURE  9(8).                    CI0022
            12            DA8G-GEEND  PICTURE  9(8).                    CI0022
            12            DA8G-GEMDA  PICTURE  9(8).                    CI0022
            12            DA8G-DNPMT1 PICTURE  9(8).                    CI0022
            12            DA8G-MVSYS  PICTURE  X(6).                    CI0022
            12            DA8G-NAIDC  PICTURE  9(12).                   CI0022
            12            DA8G-CIRMO  PICTURE  X(12).                   CI0022
            12            DA8G-CEBTP  PICTURE  9(2).                    CI0022
            12            DA8G-CLTIN  PICTURE  9(12).                   CI0022
            12            DA8G-CLORN  PICTURE  X(45).                   CI0022
            12            DA8G-GESAD1 PICTURE  X(30).                   CI0022
            12            DA8G-GESAD2 PICTURE  X(30).                   CI0022
            12            DA8G-GESAD3 PICTURE  X(30).                   CI0022
            12            DA8G-GECIT  PICTURE  X(25).                   CI0022
            12            DA8G-GEST   PICTURE  X(8).                    CI0022
            12            DA8G-GEPCD  PICTURE  X(12).                   CI0022
            12            DA8G-CLNMF  PICTURE  X(20).                   CI0022
            12            DA8G-CLNML  PICTURE  X(25).                   CI0022
            12            DA8G-CLTIN1 PICTURE  9(12).                   CI0022
            12            DA8G-FILLER PICTURE  X(05).                   CI0022
            10            DA8G-FILLER PICTURE  X(100)                   CI0022
                          VALUE                SPACE.                   CI0022
       01                 DA8H.                                         CI0022
            10            DA8H-QITEM  PICTURE  9(3)                     CI0022
                          VALUE                ZERO.                    CI0022
            10            DA8H-CTID   PICTURE  X(27)                    CI0022
                          OCCURS       030     TIMES                    CI0022
                          VALUE                SPACE.                   CI0022
            10            DA8H-CRQAS  PICTURE  X                        CI0022
                          VALUE                SPACE.                   CI0022
            10            DA8H-FILLER PICTURE  X(299)                   CI0022
                          VALUE                SPACE.                   CI0022
      *!WF DSP=DA DSL=QT SEL=8H FOR=I DES=2 LEV=1                       AM0975
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0022
            10            XW05-XW06.                                    CI0022
            11            XW05-XDBPCB.                                  CI0022
            12            XW05-XDBDNM PICTURE  X(08)                    CI0022
                          VALUE                SPACE.                   CI0022
            12            XW05-XSEGLV PICTURE  X(02)                    CI0022
                          VALUE                SPACE.                   CI0022
            12            XW05-XRC    PICTURE  X(02)                    CI0022
                          VALUE                SPACE.                   CI0022
            12            XW05-XPROPT PICTURE  X(04)                    CI0022
                          VALUE                SPACE.                   CI0022
            12            XW05-FILLER PICTURE  S9(5)                    CI0022
                          VALUE                ZERO                     CI0022
                          BINARY.                                       CI0022
            12            XW05-XSEGNM PICTURE  X(08)                    CI0022
                          VALUE                SPACE.                   CI0022
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0022
                          VALUE                ZERO                     CI0022
                          BINARY.                                       CI0022
            12            XW05-XSEGNB PICTURE  9(05)                    CI0022
                          VALUE                ZERO                     CI0022
                          BINARY.                                       CI0022
            12            XW05-XCOKEY PICTURE  X(70)                    CI0022
                          VALUE                SPACE.                   CI0022
            10            XW05-XW07.                                    CI0022
            11            XW05-XIOPCB.                                  CI0022
            12            XW05-XTERMI PICTURE  X(08)                    CI0022
                          VALUE                SPACE.                   CI0022
            12            XW05-FILLER PICTURE  XX                       CI0022
                          VALUE                SPACE.                   CI0022
            12            XW05-XRC1   PICTURE  X(02)                    CI0022
                          VALUE                SPACE.                   CI0022
            12            XW05-FILLER PICTURE  X(12)                    CI0022
                          VALUE                SPACE.                   CI0022
            12            XW05-XMODNM PICTURE  X(8)                     CI0022
                          VALUE                SPACE.                   CI0022
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0022
                          VALUE                ZERO.                    CI0022
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0022
                          VALUE                ZERO.                    CI0022
            10            XW05-XGU    PICTURE  X(4)                     CI0022
                          VALUE                'GU  '.                  CI0022
            10            XW05-XGHU   PICTURE  X(4)                     CI0022
                          VALUE                'GHU '.                  CI0022
            10            XW05-XGN    PICTURE  X(4)                     CI0022
                          VALUE                'GN  '.                  CI0022
            10            XW05-XGHN   PICTURE  X(4)                     CI0022
                          VALUE                'GHN '.                  CI0022
            10            XW05-XGNP   PICTURE  X(4)                     CI0022
                          VALUE                'GNP '.                  CI0022
            10            XW05-XGHNP  PICTURE  X(4)                     CI0022
                          VALUE                'GHNP'.                  CI0022
            10            XW05-XREPL  PICTURE  XXXX                     CI0022
                          VALUE                'REPL'.                  CI0022
            10            XW05-XISRT  PICTURE  X(4)                     CI0022
                          VALUE                'ISRT'.                  CI0022
            10            XW05-XDLET  PICTURE  X(4)                     CI0022
                          VALUE                'DLET'.                  CI0022
            10            XW05-XOPEN  PICTURE  X(4)                     CI0022
                          VALUE                'OPEN'.                  CI0022
            10            XW05-XCLSE  PICTURE  X(4)                     CI0022
                          VALUE                'CLSE'.                  CI0022
            10            XW05-XCHKP  PICTURE  X(4)                     CI0022
                          VALUE                'CHKP'.                  CI0022
            10            XW05-XXRST  PICTURE  X(4)                     CI0022
                          VALUE                'XRST'.                  CI0022
            10            XW05-XTERM  PICTURE  X(4)                     CI0022
                          VALUE                'TERM'.                  CI0022
            10            XW05-XNFPAC PICTURE  X(13)                    CI0022
                          VALUE                SPACE.                   CI0022
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0022
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0022
      *                                                                 AMDU27
      ******************************************************************AMDU27
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED FOR FUNDS     *AMDU27
      **     BILLING VSAM FILE READ FOR AN ACCOUNT NUMBER              *AMDU27
      ******************************************************************AMDU27
      *                                                                 AMDU27
      *!WF DSP=FB DSL=DU SEL=27 FOR=I LEV=1                             AMDU27
       01                 FB00.                                         CI0022
          05              FB00-SUITE.                                   CI0022
            15       FILLER         PICTURE  X(00228).                  CI0022
       01                 FB27  REDEFINES      FB00.                    CI0022
            10            FB27-C299.                                    CI0022
            11            FB27-CTID.                                    CI0022
            12            FB27-CTIDA  PICTURE  9(3).                    CI0022
            12            FB27-CTIDN.                                   CI0022
            13            FB27-CTIDNP PICTURE  X(13).                   CI0022
            13            FB27-CTIDND PICTURE  9(11).                   CI0022
            10            FB27-FILLER PICTURE  X(100).                  CI0022
            10            FB27-IFDAP  PICTURE  X.                       CI0022
            10            FB27-FILLER PICTURE  X(100).                  CI0022
      *                                                                 AMDU27
      *                                                                 AMDU27
      *                                                                 AMDU27
      *                                                                 AMDU27
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
       01   DEBUT-WSS.                                                  CI0022
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0022
            05   IK     PICTURE X.                                      CI0022
       01  CONSTANTES-PAC.                                              CI0022
           05  FILLER  PICTURE X(87)   VALUE                            CI0022
                     '6015 CAT09/08/14CI0022ADMIN   14:34:17CI0022P AMERCI0022
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0022
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0022
           05  NUGNA   PICTURE X(5).                                    CI0022
           05  APPLI   PICTURE X(3).                                    CI0022
           05  DATGN   PICTURE X(8).                                    CI0022
           05  PROGR   PICTURE X(6).                                    CI0022
           05  CODUTI  PICTURE X(8).                                    CI0022
           05  TIMGN   PICTURE X(8).                                    CI0022
           05  PROGE   PICTURE X(8).                                    CI0022
           05  COBASE  PICTURE X(4).                                    CI0022
           05  DATGNC  PICTURE X(10).                                   CI0022
           05  RELEAS  PICTURE X(7).                                    CI0022
           05  DATGE   PICTURE X(10).                                   CI0022
           05  DATSQ   PICTURE X(10).                                   CI0022
       01  DATCE.                                                       CI0022
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0022
         05  DATOR.                                                     CI0022
           10  DATOA  PICTURE XX.                                       CI0022
           10  DATOM  PICTURE XX.                                       CI0022
           10  DATOJ  PICTURE XX.                                       CI0022
       01   VARIABLES-CONDITIONNELLES.                                  CI0022
            05                  FT      PICTURE X VALUE '0'.            CI0022
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0022
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0022
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU070
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0022
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0022
       01               S-CX01-SSA.                                     CI0022
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0022
                                      VALUE 'CX01    '.                 CI0022
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0022
            10          S-CX01-CCOD   PICTURE X(5)                      CI0022
                                      VALUE '-----'.                    CI0022
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0022
       01            S-CXU01-SSA.                                       CI0022
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX01    '.                 CI0022
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0022
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(CX01K'.                   CI0022
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0022
            10       S-CXU01-CX01K.                                     CI0022
            11       S-CXU01-C199.                                      CI0022
            12       S-CXU01-CLID.                                      CI0022
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0022
            13       S-CXU01-CLIDN.                                     CI0022
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0022
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0022
            10  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01               S-CX03-SSA.                                     CI0022
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0022
                                      VALUE 'CX03    '.                 CI0022
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0022
            10          S-CX03-CCOD   PICTURE X(5)                      CI0022
                                      VALUE '-----'.                    CI0022
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0022
       01            S-CXA03-SSA.                                       CI0022
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX03    '.                 CI0022
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0022
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(CARTY'.                   CI0022
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0022
            12       S-CXA03-CARTY    PICTURE  99.                      CI0022
            12  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXB03-SSA.                                       CI0022
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX03    '.                 CI0022
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0022
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(NARRS'.                   CI0022
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0022
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            12  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXC03-SSA.                                       CI0022
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX03    '.                 CI0022
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0022
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(CPMTG'.                   CI0022
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0022
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0022
            11  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXD03-SSA.                                       CI0022
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX03    '.                 CI0022
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0022
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(GRCRNG'.                  CI0022
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0022
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0022
            11  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXE03-SSA.                                       CI0022
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX03    '.                 CI0022
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0022
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(DEXDT'.                   CI0022
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0022
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0022
            11  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXF03-SSA.                                       CI0022
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX03    '.                 CI0022
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0022
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(CY50'.                    CI0022
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0022
            11       S-CXF03-CY50.                                      CI0022
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0022
            11  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXG03-SSA.                                       CI0022
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX03    '.                 CI0022
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0022
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(NBASQ'.                   CI0022
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0022
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXH03-SSA.                                       CI0022
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX03    '.                 CI0022
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0022
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(NARID'.                   CI0022
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0022
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0022
            12  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXU03-SSA.                                       CI0022
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX03    '.                 CI0022
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0022
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(CX03K'.                   CI0022
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0022
            11       S-CXU03-CX03K.                                     CI0022
            12       S-CXU03-CARTY    PICTURE  99.                      CI0022
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01               S-CX06-SSA.                                     CI0022
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0022
                                      VALUE 'CX06    '.                 CI0022
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0022
            10          S-CX06-CCOD   PICTURE X(5)                      CI0022
                                      VALUE '-----'.                    CI0022
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0022
       01            S-CXU06-SSA.                                       CI0022
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX06    '.                 CI0022
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0022
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(CX06K'.                   CI0022
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0022
            10       S-CXU06-CX06K.                                     CI0022
            11       S-CXU06-C299.                                      CI0022
            12       S-CXU06-CTID.                                      CI0022
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0022
            13       S-CXU06-CTIDN.                                     CI0022
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0022
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0022
            10  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01               S-CX12-SSA.                                     CI0022
            10         S1-CX12-SEGNAM PICTURE X(8)                      CI0022
                                      VALUE 'CX12    '.                 CI0022
            10         S1-CX12-CCOM   PICTURE X VALUE '*'.              CI0022
            10          S-CX12-CCOD   PICTURE X(5)                      CI0022
                                      VALUE '-----'.                    CI0022
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0022
       01            S-CXA12-SSA.                                       CI0022
            10      S1-CXA12-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX12    '.                 CI0022
            10      S1-CXA12-CCOM   PICTURE X VALUE '*'.                CI0022
            10       S-CXA12-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            10      S1-CXA12-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(CDEST'.                   CI0022
            10       S-CXA12-OPER  PICTURE XX VALUE ' ='.               CI0022
            10       S-CXA12-CDEST    PICTURE  99.                      CI0022
            10  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXB12-SSA.                                       CI0022
            10      S1-CXB12-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX12    '.                 CI0022
            10      S1-CXB12-CCOM   PICTURE X VALUE '*'.                CI0022
            10       S-CXB12-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            10      S1-CXB12-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(DNPMT'.                   CI0022
            10       S-CXB12-OPER  PICTURE XX VALUE ' ='.               CI0022
            10       S-CXB12-DNPMT    PICTURE  9(8).                    CI0022
            10  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXC12-SSA.                                       CI0022
            11      S1-CXC12-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX12    '.                 CI0022
            11      S1-CXC12-CCOM   PICTURE X VALUE '*'.                CI0022
            11       S-CXC12-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            11      S1-CXC12-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(NAPDS'.                   CI0022
            11       S-CXC12-OPER  PICTURE XX VALUE ' ='.               CI0022
            11       S-CXC12-NAPDS    PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXU12-SSA.                                       CI0022
            10      S1-CXU12-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX12    '.                 CI0022
            10      S1-CXU12-CCOM   PICTURE X VALUE '*'.                CI0022
            10       S-CXU12-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            10      S1-CXU12-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(CX12K'.                   CI0022
            10       S-CXU12-OPER  PICTURE XX VALUE ' ='.               CI0022
            10       S-CXU12-CX12K.                                     CI0022
            11       S-CXU12-CPMTC    PICTURE  99.                      CI0022
            11       S-CXU12-NAPDS    PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11       S-CXU12-GESTD    PICTURE  9(8).                    CI0022
            10  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01               S-CX13-SSA.                                     CI0022
            10         S1-CX13-SEGNAM PICTURE X(8)                      CI0022
                                      VALUE 'CX13    '.                 CI0022
            10         S1-CX13-CCOM   PICTURE X VALUE '*'.              CI0022
            10          S-CX13-CCOD   PICTURE X(5)                      CI0022
                                      VALUE '-----'.                    CI0022
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0022
       01            S-CXA13-SSA.                                       CI0022
            11      S1-CXA13-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX13    '.                 CI0022
            11      S1-CXA13-CCOM   PICTURE X VALUE '*'.                CI0022
            11       S-CXA13-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            11      S1-CXA13-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(CDEST'.                   CI0022
            11       S-CXA13-OPER  PICTURE XX VALUE ' ='.               CI0022
            11       S-CXA13-CDEST    PICTURE  99.                      CI0022
            11  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXB13-SSA.                                       CI0022
            12      S1-CXB13-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX13    '.                 CI0022
            12      S1-CXB13-CCOM   PICTURE X VALUE '*'.                CI0022
            12       S-CXB13-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            12      S1-CXB13-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(CARTZ'.                   CI0022
            12       S-CXB13-OPER  PICTURE XX VALUE ' ='.               CI0022
            12       S-CXB13-CARTZ    PICTURE  99.                      CI0022
            12  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXC13-SSA.                                       CI0022
            12      S1-CXC13-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX13    '.                 CI0022
            12      S1-CXC13-CCOM   PICTURE X VALUE '*'.                CI0022
            12       S-CXC13-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            12      S1-CXC13-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(NAPDS'.                   CI0022
            12       S-CXC13-OPER  PICTURE XX VALUE ' ='.               CI0022
            12       S-CXC13-NAPDS    PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            12  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXU13-SSA.                                       CI0022
            11      S1-CXU13-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX13    '.                 CI0022
            11      S1-CXU13-CCOM   PICTURE X VALUE '*'.                CI0022
            11       S-CXU13-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            11      S1-CXU13-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(CX13K'.                   CI0022
            11       S-CXU13-OPER  PICTURE XX VALUE ' ='.               CI0022
            11       S-CXU13-CX13K.                                     CI0022
            12       S-CXU13-CARTZ    PICTURE  99.                      CI0022
            12       S-CXU13-NAPDS    PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CX113-SSA.                                       CI0022
            11      S1-CX113-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX13    '.                 CI0022
            11      S1-CX113-CCOM   PICTURE X VALUE '*'.                CI0022
            11       S-CX113-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            11      S1-CX113-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(XGCUSPY'.                 CI0022
            11       S-CX113-OPER  PICTURE XX VALUE ' ='.               CI0022
            11       S-CX113-GCUSPY   PICTURE  X(12).                   CI0022
            11  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01               S-CX2Y-SSA.                                     CI0022
            10         S1-CX2Y-SEGNAM PICTURE X(8)                      CI0022
                                      VALUE 'CX2Y    '.                 CI0022
            10         S1-CX2Y-CCOM   PICTURE X VALUE '*'.              CI0022
            10          S-CX2Y-CCOD   PICTURE X(5)                      CI0022
                                      VALUE '-----'.                    CI0022
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0022
       01            S-CXA2Y-SSA.                                       CI0022
            11      S1-CXA2Y-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX2Y    '.                 CI0022
            11      S1-CXA2Y-CCOM   PICTURE X VALUE '*'.                CI0022
            11       S-CXA2Y-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            11      S1-CXA2Y-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(CARTY'.                   CI0022
            11       S-CXA2Y-OPER  PICTURE XX VALUE ' ='.               CI0022
            11       S-CXA2Y-CARTY    PICTURE  99.                      CI0022
            11  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXB2Y-SSA.                                       CI0022
            11      S1-CXB2Y-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX2Y    '.                 CI0022
            11      S1-CXB2Y-CCOM   PICTURE X VALUE '*'.                CI0022
            11       S-CXB2Y-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            11      S1-CXB2Y-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(C299'.                    CI0022
            11       S-CXB2Y-OPER  PICTURE XX VALUE ' ='.               CI0022
            11       S-CXB2Y-C299.                                      CI0022
            12       S-CXB2Y-CTID.                                      CI0022
            13       S-CXB2Y-CTIDA    PICTURE  9(3).                    CI0022
            13       S-CXB2Y-CTIDN.                                     CI0022
            14       S-CXB2Y-CTIDNP   PICTURE  X(13).                   CI0022
            14       S-CXB2Y-CTIDND   PICTURE  9(11).                   CI0022
            11  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01            S-CXU2Y-SSA.                                       CI0022
            10      S1-CXU2Y-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX2Y    '.                 CI0022
            10      S1-CXU2Y-CCOM   PICTURE X VALUE '*'.                CI0022
            10       S-CXU2Y-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            10      S1-CXU2Y-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(CX2YK'.                   CI0022
            10       S-CXU2Y-OPER  PICTURE XX VALUE ' ='.               CI0022
            10       S-CXU2Y-CX2YK.                                     CI0022
            11       S-CXU2Y-C299.                                      CI0022
            12       S-CXU2Y-CTID.                                      CI0022
            13       S-CXU2Y-CTIDA    PICTURE  9(3).                    CI0022
            13       S-CXU2Y-CTIDN.                                     CI0022
            14       S-CXU2Y-CTIDNP   PICTURE  X(13).                   CI0022
            14       S-CXU2Y-CTIDND   PICTURE  9(11).                   CI0022
            11       S-CXU2Y-C199.                                      CI0022
            12       S-CXU2Y-CLID.                                      CI0022
            13       S-CXU2Y-CLIDO    PICTURE  9(3).                    CI0022
            13       S-CXU2Y-CLIDN.                                     CI0022
            14       S-CXU2Y-CLIDNP   PICTURE  X(12).                   CI0022
            14       S-CXU2Y-CLIDND   PICTURE  9(8).                    CI0022
            11       S-CXU2Y-CARTY    PICTURE  99.                      CI0022
            11       S-CXU2Y-NARRS    PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            10  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01               S-CX6Y-SSA.                                     CI0022
            10         S1-CX6Y-SEGNAM PICTURE X(8)                      CI0022
                                      VALUE 'CX6Y    '.                 CI0022
            10         S1-CX6Y-CCOM   PICTURE X VALUE '*'.              CI0022
            10          S-CX6Y-CCOD   PICTURE X(5)                      CI0022
                                      VALUE '-----'.                    CI0022
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0022
       01            S-CXU6Y-SSA.                                       CI0022
            10      S1-CXU6Y-SEGNAM PICTURE X(8)                        CI0022
                                      VALUE 'CX6Y    '.                 CI0022
            10      S1-CXU6Y-CCOM   PICTURE X VALUE '*'.                CI0022
            10       S-CXU6Y-CCOD   PICTURE X(5)                        CI0022
                                      VALUE '-----'.                    CI0022
            10      S1-CXU6Y-FLDNAM PICTURE X(9)                        CI0022
                                      VALUE '(CX6YK'.                   CI0022
            10       S-CXU6Y-OPER  PICTURE XX VALUE ' ='.               CI0022
            10       S-CXU6Y-CX6YK.                                     CI0022
            11       S-CXU6Y-C299.                                      CI0022
            12       S-CXU6Y-CTID.                                      CI0022
            13       S-CXU6Y-CTIDA    PICTURE  9(3).                    CI0022
            13       S-CXU6Y-CTIDN.                                     CI0022
            14       S-CXU6Y-CTIDNP   PICTURE  X(13).                   CI0022
            14       S-CXU6Y-CTIDND   PICTURE  9(11).                   CI0022
            11       S-CXU6Y-C199.                                      CI0022
            12       S-CXU6Y-CLID.                                      CI0022
            13       S-CXU6Y-CLIDO    PICTURE  9(3).                    CI0022
            13       S-CXU6Y-CLIDN.                                     CI0022
            14       S-CXU6Y-CLIDNP   PICTURE  X(12).                   CI0022
            14       S-CXU6Y-CLIDND   PICTURE  9(8).                    CI0022
            11       S-CXU6Y-CARTY    PICTURE  99.                      CI0022
            11       S-CXU6Y-NARRS    PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11       S-CXU6Y-CTID1    PICTURE  X(27).                   CI0022
            11       S-CXU6Y-CARTZ    PICTURE  99.                      CI0022
            11       S-CXU6Y-NAPDS    PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11       S-CXU6Y-NPISQ    PICTURE  S9(3)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            10  FILLER   PICTURE X    VALUE ')'.                        CI0022
       01   ZONES-UTILISATEUR PICTURE X.                                CI0022
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
      ** PCB POINTER FOR ARAY                                           ADU015
            05 PCB-ARAY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AREY                                           ADU015
            05 PCB-AREY-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR ARAY                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0022
          05              PA00-SUITE.                                   CI0022
            15       FILLER         PICTURE  X(00106).                  CI0022
       01                 PA06  REDEFINES      PA00.                    CI0022
            10            PA06-XDBPCB.                                  CI0022
            11            PA06-XDBDNM PICTURE  X(08).                   CI0022
            11            PA06-XSEGLV PICTURE  X(02).                   CI0022
            11            PA06-XRC    PICTURE  X(02).                   CI0022
            11            PA06-XPROPT PICTURE  X(04).                   CI0022
            11            PA06-FILLER PICTURE  S9(5)                    CI0022
                          BINARY.                                       CI0022
            11            PA06-XSEGNM PICTURE  X(08).                   CI0022
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0022
                          BINARY.                                       CI0022
            11            PA06-XSEGNB PICTURE  9(05)                    CI0022
                          BINARY.                                       CI0022
            11            PA06-XCOKEY PICTURE  X(70).                   CI0022
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0022
          05              PB00-SUITE.                                   CI0022
            15       FILLER         PICTURE  X(00106).                  CI0022
       01                 PB06  REDEFINES      PB00.                    CI0022
            10            PB06-XDBPCB.                                  CI0022
            11            PB06-XDBDNM PICTURE  X(08).                   CI0022
            11            PB06-XSEGLV PICTURE  X(02).                   CI0022
            11            PB06-XRC    PICTURE  X(02).                   CI0022
            11            PB06-XPROPT PICTURE  X(04).                   CI0022
            11            PB06-FILLER PICTURE  S9(5)                    CI0022
                          BINARY.                                       CI0022
            11            PB06-XSEGNM PICTURE  X(08).                   CI0022
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0022
                          BINARY.                                       CI0022
            11            PB06-XSEGNB PICTURE  9(05)                    CI0022
                          BINARY.                                       CI0022
            11            PB06-XCOKEY PICTURE  X(70).                   CI0022
      *** PCB MASK FOR AREY                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0022
          05              PC00-SUITE.                                   CI0022
            15       FILLER         PICTURE  X(00106).                  CI0022
       01                 PC06  REDEFINES      PC00.                    CI0022
            10            PC06-XDBPCB.                                  CI0022
            11            PC06-XDBDNM PICTURE  X(08).                   CI0022
            11            PC06-XSEGLV PICTURE  X(02).                   CI0022
            11            PC06-XRC    PICTURE  X(02).                   CI0022
            11            PC06-XPROPT PICTURE  X(04).                   CI0022
            11            PC06-FILLER PICTURE  S9(5)                    CI0022
                          BINARY.                                       CI0022
            11            PC06-XSEGNM PICTURE  X(08).                   CI0022
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0022
                          BINARY.                                       CI0022
            11            PC06-XSEGNB PICTURE  9(05)                    CI0022
                          BINARY.                                       CI0022
            11            PC06-XCOKEY PICTURE  X(70).                   CI0022

      *PASS LIST TO/FROM CI0022
      *!WF DSP=BE DSL=DU SEL=26 FOR=I DES=1 LEV=1 PLT=05
       01                 BE26.                                         CI0022
            10            BE26-C299.                                    CI0022
            11            BE26-CTID.                                    CI0022
            12            BE26-CTIDA  PICTURE  9(3).                    CI0022
            12            BE26-CTIDN.                                   CI0022
            13            BE26-CTIDNP PICTURE  X(13).                   CI0022
            13            BE26-CTIDND PICTURE  9(11).                   CI0022
            10            BE26-DCACG  PICTURE  9(8).                    CI0022
            10            BE26-MAPPN  PICTURE  X(10).                   CI0022
            10            BE26-FILLER PICTURE  X(90).                   CI0022
            10            BE26-IFDAB  PICTURE  X.                       CI0022
            10            BE26-IFDAG  PICTURE  X.                       CI0022
            10            BE26-IFDAP  PICTURE  X.                       CI0022
            10            BE26-IFDAS  PICTURE  X(01).                   CI0022
            10            BE26-ISINF  PICTURE  X(01).                   CI0022
            10            BE26-ISOUT  PICTURE  X(01).                   CI0022
            10            BE26-ISINO  PICTURE  X.                       CI0022
            10            BE26-IGBAR  PICTURE  X.                       CI0022
            10            BE26-FILLER PICTURE  X(95).                   CI0022

      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0022
          05              DE00-SUITE.                                   CI0022
            15       FILLER         PICTURE  X(00653).                  CI0022
       01                 DE10  REDEFINES      DE00.                    CI0022
            10            DE10-DU11.                                    CI0022
            11            DE10-XFONC  PICTURE  X(4).                    CI0022
            11            DE10-MPSBN  PICTURE  X(8).                    CI0022
            11            DE10-XDBDNM PICTURE  X(08).                   CI0022
            11            DE10-XSEGNM PICTURE  X(08).                   CI0022
            11            DE10-XRC    PICTURE  X(02).                   CI0022
            11            DE10-MSEG   PICTURE  X(08).                   CI0022
            11            DE10-XCOKEY PICTURE  X(70).                   CI0022
            11            DE10-CUIBR  PICTURE  X(01).                   CI0022
            11            DE10-CUIBA  PICTURE  X(01).                   CI0022
            11            DE10-IPBIK  PICTURE  X(1).                    CI0022
            10            DE10-DU03.                                    CI0022
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            DE10-CMSSF  PICTURE  XX.                      CI0022
            11            DE10-DU09.                                    CI0022
            12            DE10-CMESA  PICTURE  S9(9)                    CI0022
                          BINARY.                                       CI0022
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0022
                          BINARY.                                       CI0022
            12            DE10-CMESB  PICTURE  S9(9)                    CI0022
                          BINARY.                                       CI0022
            12            DE10-CMSST  PICTURE  S9(9)                    CI0022
                          BINARY.                                       CI0022
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0022
                          BINARY.                                       CI0022
            12            DE10-QELLAA PICTURE  S9(9)                    CI0022
                          BINARY.                                       CI0022
            12            DE10-TMESS4 PICTURE  X(512).                  CI0022
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
       01                 MS00.                                         CI0022
          05              MS00-SUITE.                                   CI0022
            15       FILLER         PICTURE  X(00542).                  CI0022
       01                 MS03  REDEFINES      MS00.                    CI0022
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            10            MS03-CMSSF  PICTURE  XX.                      CI0022
            10            MS03-DU09.                                    CI0022
            11            MS03-CMESA  PICTURE  S9(9)                    CI0022
                          BINARY.                                       CI0022
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0022
                          BINARY.                                       CI0022
            11            MS03-CMESB  PICTURE  S9(9)                    CI0022
                          BINARY.                                       CI0022
            11            MS03-CMSST  PICTURE  S9(9)                    CI0022
                          BINARY.                                       CI0022
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0022
                          BINARY.                                       CI0022
            11            MS03-QELLAA PICTURE  S9(9)                    CI0022
                          BINARY.                                       CI0022
            11            MS03-TMESS4 PICTURE  X(512).                  CI0022
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0022
            10            MX11-QMSGS  PICTURE  9(03).                   CI0022
            10            MX11-PJ09                                     CI0022
                          OCCURS       025     TIMES.                   CI0022
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0022
                          COMPUTATIONAL-3.                              CI0022
            11            MX11-CMESB  PICTURE  S9(9)                    CI0022
                          BINARY.                                       CI0022
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                BE26
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0022
      *               *                                   *             CI0022
      *               *INITIALISATIONS                    *             CI0022
      *               *                                   *             CI0022
      *               *************************************.            CI0022
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
      *N02CA.    NOTE *INIT ARNG FOUND INDICATRS TO 'N'   *.
       F02CA.                                                           lv10
      *MONEY INTO ACCT INDICATORS:
      *- BA
      *- GROUP BILL (CONSIDER START DT)
      *- GROUP BILL (IGNORE START DT)
      *- PRODUCT BILLING
      *- SD
      *- SD FROM MUTUAL FUND
      *- SD OTHER THAN MUT FND SP
      *MONEY OUT OF ACCT INDICATOR:
      *- SD
           MOVE        'N' TO BE26-IFDAB
           BE26-IFDAG
           BE26-IGBAR
           BE26-IFDAP
           BE26-IFDAS
           BE26-ISINF
           BE26-ISINO
           BE26-ISOUT.
       F02CA-FN. EXIT.
      *N02XA.    NOTE *SET ADDRESSES FOR DATABASES        *.
       F02XA.                                                           lv10
      *SET ADDRESS FOR ARAY                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-ARAY-PTR1.                                          ADU015
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR AREY                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-AREY-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0022
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0022
      *               *                                   *             CI0022
      *               *FIN DE TRAITEMENT                  *             CI0022
      *               *                                   *             CI0022
      *               *************************************.            CI0022
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0022
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATE INPUT PARAMETERS          *
      *               *                                   *
      *               *************************************.
       F30.           EXIT.                                             lv05
      *N30BA.    NOTE *IF BAD ADMINISTRATOR               *.
       F30BA.    IF    BE26-CTIDA NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F30BA-FN.
      *---> Send BAD ACCT Message                                       ADU119
      *      and EXIT                                                   ADU119
           MOVE        012004 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30BA-FN. EXIT.
      *N30CA.    NOTE *IF BAD ACCTG DATE                  *.
       F30CA.    IF    BE26-DCACG NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F30CA-FN.
      *---> Send BAD DATE Message                                       ADU119
      *      and EXIT                                                   ADU119
           MOVE        012786 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CA-FN. EXIT.
       F30-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *READ DATABASE FOR BA/BILLING       *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *********************************
      ** READ THE ARRANGEMENT         *
      ** DATABASES TO SEE IF THE ACCT *
      ** NUMBER PASSED HAS A BA OR    *
      ** AN SD TAKING MONEY OUT OF IT *
      *********************************
      *N50BA.    NOTE *READ CX2Y                          *.
       F50BA.                                                           lv10
      *SET UP SSA
           MOVE        LOW-VALUES TO S-CXU2Y-CX2YK
           MOVE        BE26-CTID TO S-CXU2Y-CTID
           MOVE        'GE' TO S-CXU2Y-OPER
      *GU CX2Y
           PERFORM     F94CX THRU F94CX-FN.
       F50BA-FN. EXIT.
      *N50DA.    NOTE *LOOP THRU CX2Y SEGMENTS            *.
       F50DA.    IF    IK = '0'                                         lv10
                 AND   CX2Y-CTID = BE26-CTID
                 NEXT SENTENCE ELSE GO TO     F50DA-FN.
      *N50EA.    NOTE *CASE OF STRUCTURE FOR ARR TYPE     *.
       F50EA.         EXIT.                                             lv15
      *N50FA.    NOTE *BA ARRANGEMENT                     *.
       F50FA.    IF    CX2Y-CARTY =                                     lv20
                       01
                 NEXT SENTENCE ELSE GO TO     F50FA-FN.
      *N50GA.    NOTE *READ CX06                          *.
       F50GA.                                                           lv25
      *SET UP SSA'S
           MOVE        CX2Y-CLID TO S-CXU01-CLID
           MOVE        CX2Y-CARTY TO S-CXU03-CARTY
           MOVE        CX2Y-NARRS TO S-CXU03-NARRS
           MOVE        CX2Y-CTID TO S-CXU06-CTID
      *GU CX06
           PERFORM     F94C2 THRU F94C2-FN.
       F50GA-FN. EXIT.
      *N50IA.    NOTE *CX06 FOUND                         *.
       F50IA.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F50IA-FN.
      *N50JA.    NOTE *READ FIRST CX12 SEGMENT (GN)       *.
       F50JA.                                                           lv30
      *(BA COLLECTION DETAIL)
           PERFORM     F94C4 THRU F94C4-FN.
       F50JA-FN. EXIT.
      *N50KA.    NOTE *LOOP THRU CX12'S                   *.
       F50KA.    IF    IK = '0'                                         lv30
                 AND   BE26-IFDAB = 'N'
                 NEXT SENTENCE ELSE GO TO     F50KA-FN.
      *N50LA.    NOTE *ARR START DT IS TODAY OR IN PAST   *.
       F50LA.    IF    ((CX12-GESTD NOT >                               lv35
                       BE26-DCACG)
                 AND   (CX12-GEEND = ZEROS OR
                       CX12-GEEND NOT <
                       BE26-DCACG))
                 OR    (CX12-GESTD > BE26-DCACG AND
                       CX12-CDEST NOT = 03)
                 NEXT SENTENCE ELSE GO TO     F50LA-FN.
      *ARR END DT IS 0 OR
      *TODAY OR IN FUTURE
      *ARR START DT IS IN FUTURE AND
      *ARR NOT INACTIVE
      *SET INDICATOR TO 'Y' & EXIT LOOP
           MOVE        'Y' TO BE26-IFDAB
               GO TO     F50KA-900.
       F50LA-FN. EXIT.
      *N50MA.    NOTE *READ NEXT CX12 SEGMENT             *.
       F50MA.                                                           lv35
           PERFORM     F94C4 THRU F94C4-FN.
       F50MA-FN. EXIT.
       F50KA-900. GO TO F50KA.
       F50KA-FN. EXIT.
       F50IA-FN. EXIT.
       F50FA-900. GO TO F50EA-FN.
       F50FA-FN. EXIT.
      *N50UA.    NOTE *SD TAKING MONEY OUT OF ACCOUNT     *.
       F50UA.    IF    CX2Y-CARTY =                                     lv20
                       10
                 NEXT SENTENCE ELSE GO TO     F50UA-FN.
      *N50UC.    NOTE *READ CX06                          *.
       F50UC.                                                           lv25
      *SET UP SSA'S
           MOVE        CX2Y-CLID TO S-CXU01-CLID
           MOVE        CX2Y-CARTY TO S-CXU03-CARTY
           MOVE        CX2Y-NARRS TO S-CXU03-NARRS
           MOVE        CX2Y-CTID TO S-CXU06-CTID
      *GU CX06
           PERFORM     F94C2 THRU F94C2-FN.
       F50UC-FN. EXIT.
      *N50UG.    NOTE *CX06 FOUND                         *.
       F50UG.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F50UG-FN.
      *N50UI.    NOTE *READ FIRST CX13 (GN)               *.
       F50UI.                                                           lv30
      *DISBURSEMENT DETAIL
           PERFORM     F94X4 THRU F94X4-FN.
       F50UI-FN. EXIT.
      *N50UK.    NOTE *READ THRU CX13 SEGMENTS            *.
       F50UK.    IF    IK = '0'                                         lv30
                 AND   BE26-ISOUT = 'N'
                 NEXT SENTENCE ELSE GO TO     F50UK-FN.
      *N50UM.    NOTE *- ARR END DT IS 0 OR               *.
       F50UM.    IF    (CX13-GEEND = ZEROS OR                           lv35
                       CX13-GEEND NOT < BE26-DCACG)
                 AND   CX13-CDEST NOT = 03
                 AND   CX13-IIARR = 'Y'
                 NEXT SENTENCE ELSE GO TO     F50UM-FN.
      *  TODAY OR IN FUTURE
      *- ARR NOT INACTIVE
      *- ARR COMPLETE
      *SET INDICATOR TO 'Y' & EXIT LOOP
           MOVE        'Y' TO BE26-ISOUT
               GO TO     F50UK-900.
       F50UM-FN. EXIT.
      *N50UP.    NOTE *READ NEXT CX13                     *.
       F50UP.                                                           lv35
           PERFORM     F94X4 THRU F94X4-FN.
       F50UP-FN. EXIT.
       F50UK-900. GO TO F50UK.
       F50UK-FN. EXIT.
       F50UG-FN. EXIT.
       F50UA-900. GO TO F50EA-FN.
       F50UA-FN. EXIT.
      *N50VA.    NOTE *END OF CASE OF STRUCTURE           *.
       F50VA.         EXIT.                                             lv20
       F50VA-FN. EXIT.
       F50EA-FN. EXIT.
      *N50WA.    NOTE *READ NEXT CX2Y SEGMENT             *.
       F50WA.                                                           lv15
           PERFORM     F94C1 THRU F94C1-FN.
       F50WA-FN. EXIT.
       F50DA-900. GO TO F50DA.
       F50DA-FN. EXIT.
       F50-FN.   EXIT.
      *N51.      NOTE *************************************.
      *               *                                   *
      *               *PROCESSING FOR GROUP BILL ARR      *
      *               *                                   *
      *               *************************************.
       F51.                                                             lv05
      *********************************
      *N51BA.    NOTE *CHECK WHETHER A GROUP BILL ARR     *.
       F51BA.                                                           lv10
      *IS EXISTING OR NOT FOR AN ACCT
      *
      *N51BD.    NOTE *CALL EODS VIA CALLING CI0975       *.
       F51BD.                                                           lv15
           INITIALIZE  DA8G
           DA8H
           MS03
           MOVE        14 TO DA8G-CARTY
           MOVE        BE26-CTID TO DA8H-CTID (1)
           MOVE        1 TO DA8H-QITEM
           MOVE        'RPC' TO DA8G-CSYS
           PERFORM     F91BA THRU F91BA-FN.
      *N51BG.    NOTE *GET THE RESPONSE QUEUE FROM EODS   *.
       F51BG.    IF    (DA8G-CERRE = SPACES                             lv25
                 OR    DA8G-CERRE = ZEROES)
                 AND   MS03-NMESS2 = ZEROES
                 AND   DA8G-GERTC = 'Y'
                 NEXT SENTENCE ELSE GO TO     F51BG-FN.
      *SUCCESSFULLY
      *N51BK.    NOTE *IF CALL SERVICE SUCCESSFUL         *.
       F51BK.    IF    DA8G-CFAUL1 = SPACES                             lv30
                 AND   DA8G-TFACT1 = SPACES
                 NEXT SENTENCE ELSE GO TO     F51BK-FN.
      *N51BM.    NOTE *SET THE GROUP BILL ARR INDICATOR   *.
       F51BM.    IF    BE26-CTID = DA8G-CTID (1)                        lv35
                 AND   DA8G-QITEM > 0
                 NEXT SENTENCE ELSE GO TO     F51BM-FN.
      *AS 'Y'.
           MOVE        'Y' TO BE26-IFDAG
           MOVE        'Y' TO BE26-IGBAR.
       F51BM-900. GO TO F51BN-FN.
       F51BM-FN. EXIT.
      *N51BN.    NOTE *NO GROUP BILL ARRANGEMENT RETURN   *.
       F51BN.                                                           lv35
      *DUE TO FOLLOW REASON:
                 IF    DA8G-CFAUL2 = '0102'                             DOT
                 OR    DA8G-CFAUL3 = '1000'
      *1. REQUEST ACCOUNT NOT FOUND OR
      *2. NO RECORDS FOUND
           MOVE        'N' TO BE26-IFDAG
           MOVE        'N' TO BE26-IGBAR
                 ELSE
      *EODS SERVICE ERROR
      *---> Send SERVICE FAILED Message                                 ADU119
      *      and EXIT                                                   ADU119
           MOVE        015017 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F51BN-FN. EXIT.
       F51BK-900. GO TO F51BO-FN.
       F51BK-FN. EXIT.
      *N51BO.    NOTE *CALL SERVICE FAILED                *.
       F51BO.                                                           lv30
      *POPULATED THE ERROR MESSAGE
      *
      *---> Send SERVICE FAILED Message                                 ADU119
      *      and EXIT                                                   ADU119
           MOVE        015017 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F51BO-FN. EXIT.
       F51BG-900. GO TO F51BP-FN.
       F51BG-FN. EXIT.
      *N51BP.    NOTE *IF GET QUEUE FAILED                *.
       F51BP.                                                           lv25
      *POPULATED THE ERROR MESSAGE
      *
      *---> Send CALL FAILED Message                                    ADU119
      *      and EXIT                                                   ADU119
           MOVE        015005 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F51BP-FN. EXIT.
       F51BD-FN. EXIT.
       F51BA-FN. EXIT.
       F51-FN.   EXIT.
      *N52.      NOTE *************************************.
      *               *                                   *
      *               *CHECK IF MUTUAL FUNDS ACCOUNT      *
      *               *                                   *
      *               *************************************.
       F52.      IF    BE26-CTIDA = 002                                 lv05
                 NEXT SENTENCE ELSE GO TO     F52-FN.
      *********************************
      ** CALL CI0023 TO SEE IF THERE  *
      ** IS FUNDS BILLING FOR THIS    *
      ** ACCOUNT NUMBER.              *
      *********************************
      *N52BA.    NOTE *CALL CI0023 - FUNDS BILLING        *.            AM0023
       F52BA.                                                           lv10
      *                                                                 AM0023
      *********************************                                 AM0023
      ** THIS MODULE WILL READ THE    *                                 AM0023
      ** FUNDS BILLING VSAM FILE      *                                 AM0023
      ** (FVSBIL) FOR THE ACCOUNT     *                                 AM0023
      ** NUMBER.                      *                                 AM0023
      *********************************                                 AM0023
      *                                                                 AM0023
           MOVE        BE26-CTID TO FB27-CTID                           AM0023
           CALL        CI0023 USING                                     AM0023
           DFHEIBLK                                                     AM0023
           DFHCOMMAREA                                                  AM0023
           BP00                                                         AM0023
           FB27                                                         AM0023
           MS03.                                                        AM0023
      *N52BB.    NOTE *NON-DL1 ERROR                      *.            ADU070
       F52BB.    IF    MS03-NMESS2 > ZERO                               lv15
                 AND   MS03-CMESB > 10                                  ADU070
                 NEXT SENTENCE ELSE GO TO     F52BB-FN.                 ADU070
      *OF A CERTAIN SEVERITY                                            ADU070
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU070
           MOVE        CI0023 TO MS03-TMESS4 (IMS03R : 6)               ADU070
           ADD         +7 TO MS03-QELLAA                                ADU070
           MOVE                     ALL '1' TO FT GO TO F20.            ADU070
       F52BB-900. GO TO F52BC-FN.
       F52BB-FN. EXIT.
      *N52BC.    NOTE *NO ERRORS                          *.            ADU070
       F52BC.                                                           lv15
           INITIALIZE  MS03                                             ADU070
           MOVE        FB27-IFDAP TO BE26-IFDAP.
       F52BC-FN. EXIT.
       F52BA-FN. EXIT.
       F52-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *UD AND FDC: LOOK FOR               *
      *               *                                   *
      *               *************************************.
       F60.      IF    BE26-MAPPN = 'UD        '                        lv05
                 OR    BE26-MAPPN = 'FDC       '
                 NEXT SENTENCE ELSE GO TO     F60-FN.
      *SD PUTTING MONEY INTO ACCOUNT
      *********************************
      ** THIS LOGIC WILL DETERMINE IF *
      ** THE REQUESTED ACCOUNT IS     *
      ** BEING FUNDED BY ANOTHER      *
      ** ACCOUNT ON A SCHEDULED BASIS *
      *********************************
      *N60CA.    NOTE *READ FIRST CX6Y                    *.
       F60CA.                                                           lv10
      *CX6Y IS THE INDEX INTO THE DEST
      *ACCOUNT ON CX14
      *SET UP SSA
           INITIALIZE  S-CXU6Y-CX6YK
           MOVE        BE26-CTID TO S-CXU6Y-CTID
           MOVE        'GT' TO S-CXU6Y-OPER
      *GU ON CX6Y
           PERFORM     F94X1 THRU F94X1-FN.
       F60CA-FN. EXIT.
      *N60DA.    NOTE *LOOP THROUGH CX6Y                  *.
       F60DA.    IF    BE26-CTID = CX6Y-CTID                            lv10
                 AND   IK = '0'
                 NEXT SENTENCE ELSE GO TO     F60DA-FN.
      *N60FA.    NOTE *SD - ACCESS THE ARNG (READ CX13)   *.
       F60FA.    IF    CX6Y-CARTY = 10                                  lv15
                 NEXT SENTENCE ELSE GO TO     F60FA-FN.
      *SET UP SSA'S
           MOVE        CX6Y-CLID TO S-CXU01-CLID
           MOVE        CX6Y-CARTY TO S-CXU03-CARTY
           MOVE        CX6Y-NARRS TO S-CXU03-NARRS
           MOVE        CX6Y-CTID1 TO S-CXU06-CTID
           MOVE        CX6Y-CARTZ TO S-CXU13-CARTZ
           MOVE        CX6Y-NAPDS TO S-CXU13-NAPDS
      *GU CX13
           PERFORM     F94X3 THRU F94X3-FN.
      *N60GA.    NOTE *CX13 FOUND                         *.
       F60GA.    IF    IK = '0'                                         lv20
                 AND   (CX13-GEEND = ZEROS OR
                       CX13-GEEND NOT < BE26-DCACG)
                 AND   CX13-CDEST NOT = 03
                 AND   CX13-IIARR = 'Y'
                 NEXT SENTENCE ELSE GO TO     F60GA-FN.
      *- ARR END DT IS 0 OR
      *  TODAY OR IN FUTURE
      *- ARR NOT INACTIVE
      *- ARR COMPLETE
      *N60GB.    NOTE *NOT A DIVIDEND OR INTEREST ARNG    *.
       F60GB.    IF    CX13-CARTZ NOT = 05                              lv25
                 AND   CX13-CARTZ NOT = 06
                 AND   CX13-CARTZ NOT = 09
                 NEXT SENTENCE ELSE GO TO     F60GB-FN.
      *N60GC.    NOTE *ARR START DT TODAY OR IN PAST      *.
       F60GC.    IF    CX13-GESTD NOT > BE26-DCACG                      lv30
                 NEXT SENTENCE ELSE GO TO     F60GC-FN.
           MOVE        'Y' TO BE26-IFDAS.
       F60GC-FN. EXIT.
      *N60GG.    NOTE *SD MONEY IN IS FROM A MUT FUND     *.
       F60GG.    IF    S-CXU06-CTIDA = 002                              lv30
                 NEXT SENTENCE ELSE GO TO     F60GG-FN.
           MOVE        'Y' TO BE26-ISINF.
       F60GG-FN. EXIT.
       F60GB-FN. EXIT.
      *N60HB.    NOTE *ALL BUT FUND SP'S                  *.
       F60HB.    IF    CX13-CARTZ NOT = 01                              lv25
                 NEXT SENTENCE ELSE GO TO     F60HB-FN.
           MOVE        'Y' TO BE26-ISINO.
       F60HB-FN. EXIT.
       F60GA-FN. EXIT.
       F60FA-FN. EXIT.
      *N60MA.    NOTE *GET THE NEXT CX6Y SEGMENT          *.
       F60MA.                                                           lv15
           PERFORM     F94X2 THRU F94X2-FN.
       F60MA-FN. EXIT.
       F60DA-900. GO TO F60DA.
       F60DA-FN. EXIT.
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
      *N91.      NOTE *************************************.
      *               *                                   *
      *               *CALLEN MODULES                     *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91BA.    NOTE *CALL CI0975 TO GET ARRANGEMENT     *.            AM0975
       F91BA.                                                           lv10
      *DETAILS FOR A LIST OF ACCOUNTS,                                  AM0975
      *WHICH IS UP TO 30.                                               AM0975
      *                                                                 AM0975
           CALL        CI0975 USING                                     AM0975
           DFHEIBLK                                                     AM0975
           DFHCOMMAREA                                                  AM0975
           DA8G                                                         AM0975
           DA8H                                                         AM0975
           MS03                                                         AM0975
           MX11.                                                        AM0975
       F91BA-FN. EXIT.
       F91-FN.   EXIT.
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
      *               *DL/I FUNCTIONS                     *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94CX.    NOTE *CALL GU ON CX2Y                    *.            ADU026
       F94CX.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX2Y                                                    ADU026
           S-CXU2Y-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CX-FN. EXIT.
      *N94C1.    NOTE *CALL GN ON CX2Y                    *.            ADU026
       F94C1.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CX2Y                                                    ADU026
           S-CXU2Y-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C1-FN. EXIT.
      *N94C2.    NOTE *CALL GU ON CX06                    *.            ADU026
       F94C2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 CX06                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CXU06-SSA                          ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C2-FN. EXIT.
      *N94C4.    NOTE *CALL GN ON CX12                    *.            ADU026
       F94C4.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PB06 CX12                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CXU06-SSA                          ADU026
           S-CX12-SSA
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C4-FN. EXIT.
      *N94X1.    NOTE *CALL GU ON CX6Y                    *.            ADU026
       F94X1.                                                           lv10
           MOVE        'AREY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX6Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PC06 CX6Y                                                    ADU026
           S-CXU6Y-SSA                                                  ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94X1-FN. EXIT.
      *N94X2.    NOTE *CALL GN ON CX6Y                    *.            ADU026
       F94X2.                                                           lv10
           MOVE        'AREY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX6Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PC06 CX6Y                                                    ADU026
           S-CX6Y-SSA                                                   ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94X2-FN. EXIT.
      *N94X3.    NOTE *CALL GU ON CX13                    *.            ADU026
       F94X3.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CXU06-SSA                          ADU026
           S-CXU13-SSA
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94X3-FN. EXIT.
      *N94X4.    NOTE *CALL GN ON CX13                    *.            ADU026
       F94X4.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PB06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CXU06-SSA                          ADU026
           S-CX13-SSA
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94X4-FN. EXIT.
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
