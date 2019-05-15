       IDENTIFICATION DIVISION.                                         CI0072
       PROGRAM-ID.  CI0072P.                                            CI0072
      *AUTHOR.         M\M - VALIDATE ADD/CHG OF SD.                    CI0072
      *DATE-COMPILED.   09/08/14.                                       CI0072
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
       ENVIRONMENT DIVISION.                                            CI0072
       CONFIGURATION SECTION.                                           CI0072
       SOURCE-COMPUTER. IBM-370.                                        CI0072
       OBJECT-COMPUTER. IBM-370.                                        CI0072
       DATA DIVISION.                                                   CI0072
       WORKING-STORAGE SECTION.                                         CI0072
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0003           PIC X(8) VALUE 'CI0003P '.                  AM0003
       01  CI0083           PIC X(8) VALUE 'CI0083P '.                  AM0083
       01  CI0090           PIC X(8) VALUE 'CI0090P '.                  AM0090
       01  CI0135           PIC X(8) VALUE 'CI0135P '.                  AM0135
       01                 CT01.                                         CI0072
            10            CT01-CT01K.                                   CI0072
            11            CT01-C299.                                    CI0072
            12            CT01-CTID.                                    CI0072
            13            CT01-CTIDA  PICTURE  9(3).                    CI0072
            13            CT01-CTIDN.                                   CI0072
            14            CT01-CTIDNP PICTURE  X(13).                   CI0072
            14            CT01-CTIDND PICTURE  9(11).                   CI0072
            10            CT01-GECKD  PICTURE  9.                       CI0072
            10            CT01-GEMDA  PICTURE  9(8).                    CI0072
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0072
                          BINARY.                                       CI0072
            10            CT01-GECUC  PICTURE  99.                      CI0072
            10            CT01-CTAUL  PICTURE  9(3).                    CI0072
            10            CT01-DIRAC  PICTURE  9(4).                    CI0072
            10            CT01-CTCCI  PICTURE  X.                       CI0072
            10            CT01-CTCUS  PICTURE  999.                     CI0072
            10            CT01-CTEFD  PICTURE  9(8).                    CI0072
            10            CT01-CTIAD  PICTURE  9(8).                    CI0072
            10            CT01-CLCUS  PICTURE  99.                      CI0072
            10            CT01-CAMMB  PICTURE  X(3).                    CI0072
            10            CT01-CKPMM  PICTURE  X.                       CI0072
            10            CT01-CTLAD  PICTURE  9(8).                    CI0072
            10            CT01-IPERS  PICTURE  X.                       CI0072
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            CT01-CTLAT  PICTURE  9(8).                    CI0072
            10            CT01-CTLATC PICTURE  9(6).                    CI0072
            10            CT01-IMEGA  PICTURE  X.                       CI0072
            10            CT01-DIRAB  PICTURE  9(8).                    CI0072
            10            CT01-COLRQ  PICTURE  X.                       CI0072
            10            CT01-ZDA04  PICTURE  X(4).                    CI0072
            10            CT01-CTLPD  PICTURE  9(8).                    CI0072
            10            CT01-CIRASP PICTURE  9.                       CI0072
            10            CT01-CIRATP PICTURE  99.                      CI0072
            10            CT01-DRTHC  PICTURE  9(8).                    CI0072
            10            CT01-CPPTC  PICTURE  X.                       CI0072
            10            CT01-ZDA06  PICTURE  X(6).                    CI0072
            10            CT01-CTACD  PICTURE  9(8).                    CI0072
            10            CT01-CTNLI  PICTURE  X.                       CI0072
            10            CT01-CTRHO  PICTURE  9(8).                    CI0072
            10            CT01-CTSGD  PICTURE  9(8).                    CI0072
            10            CT01-CPATP  PICTURE  X(1).                    CI0072
            10            CT01-IRSTA  PICTURE  X.                       CI0072
            10            CT01-CTSTA  PICTURE  99.                      CI0072
            10            CT01-CTSSC  PICTURE  99.                      CI0072
            10            CT01-PRLIN  PICTURE  9(3).                    CI0072
            10            CT01-PRCOD  PICTURE  9(5).                    CI0072
            10            CT01-PRSCD  PICTURE  X(9).                    CI0072
            10            CT01-CTLNI  PICTURE  X.                       CI0072
            10            CT01-AYSIDA PICTURE  9(3).                    CI0072
            10            CT01-AYSID  PICTURE  9(5).                    CI0072
            10            CT01-CTBMC  PICTURE  99.                      CI0072
            10            CT01-CINAR  PICTURE  99.                      CI0072
            10            CT01-CPHTR  PICTURE  X.                       CI0072
            10            CT01-CDSTR  PICTURE  XX.                      CI0072
            10            CT01-CQACT  PICTURE  999.                     CI0072
            10            CT01-CIRAS  PICTURE  999.                     CI0072
            10            CT01-CIRAT  PICTURE  999.                     CI0072
            10            CT01-CLRAY  PICTURE  9(5).                    CI0072
            10            CT01-CATTP  PICTURE  X.                       CI0072
       01                 CT13.                                         CI0072
            10            CT13-CT13K.                                   CI0072
            11            CT13-GEHSD  PICTURE  9(8).                    CI0072
            11            CT13-GEHCD  PICTURE  9(3).                    CI0072
            11            CT13-GEHCSE PICTURE  X(12).                   CI0072
            11            CT13-GEHCSU PICTURE  9(5).                    CI0072
            10            CT13-GEHRD  PICTURE  9(8).                    CI0072
            10            CT13-GEHV   PICTURE  S9(7)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            CT13-GEDC   PICTURE  9(2).                    CI0072
       01                 CX01.                                         CI0072
            10            CX01-CX01K.                                   CI0072
            11            CX01-C199.                                    CI0072
            12            CX01-CLID.                                    CI0072
            13            CX01-CLIDO  PICTURE  9(3).                    CI0072
            13            CX01-CLIDN.                                   CI0072
            14            CX01-CLIDNP PICTURE  X(12).                   CI0072
            14            CX01-CLIDND PICTURE  9(8).                    CI0072
            10            CX01-GEMDA  PICTURE  9(8).                    CI0072
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0072
                          BINARY.                                       CI0072
            10            CX01-FILLER PICTURE  X(5).                    CI0072
       01                 CX03.                                         CI0072
            10            CX03-GELL   PICTURE  9(4)                     CI0072
                          BINARY.                                       CI0072
            10            CX03-CY00.                                    CI0072
            11            CX03-CX03K.                                   CI0072
            12            CX03-CARTY  PICTURE  99.                      CI0072
            12            CX03-NARRS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX03-CARST  PICTURE  99.                      CI0072
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX03-CPMTG  PICTURE  99.                      CI0072
            11            CX03-GRCRNG PICTURE  9(3).                    CI0072
            11            CX03-DEXDT  PICTURE  9(8).                    CI0072
            11            CX03-DASUP  PICTURE  9(8).                    CI0072
            11            CX03-CSTEC  PICTURE  X(3).                    CI0072
            11            CX03-FILLER PICTURE  X(17).                   CI0072
            11            CX03-CY50.                                    CI0072
            12            CX03-NARID  PICTURE  X(30).                   CI0072
            11            CX03-CY51                                     CI0072
                          REDEFINES            CX03-CY50.               CI0072
            12            CX03-NDIDN  PICTURE  9(12).                   CI0072
            12            CX03-FILLER PICTURE  X(18).                   CI0072
            11            CX03-CY52                                     CI0072
                          REDEFINES            CX03-CY50.               CI0072
            12            CX03-NAIDC  PICTURE  9(12).                   CI0072
            12            CX03-FILLER PICTURE  X(18).                   CI0072
            11            CX03-CY53                                     CI0072
                          REDEFINES            CX03-CY50.               CI0072
            12            CX03-NAMEXB PICTURE  9(15).                   CI0072
            12            CX03-FILLER PICTURE  X(15).                   CI0072
            10            CX03-CY99.                                    CI0072
            11            CX03-FILLER PICTURE  X(109).                  CI0072
            10            CX03-CY01                                     CI0072
                          REDEFINES            CX03-CY99.               CI0072
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX03-ICPCI  PICTURE  X.                       CI0072
            11            CX03-CLUPD  PICTURE  9(3).                    CI0072
            11            CX03-DLAUP  PICTURE  9(8).                    CI0072
            11            CX03-CWRC   PICTURE  99.                      CI0072
            11            CX03-CHCR   PICTURE  99.                      CI0072
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0072
            11            CX03-GEAUN  PICTURE  9(5).                    CI0072
            11            CX03-DPCHD  PICTURE  9(8).                    CI0072
            11            CX03-DLRCHK PICTURE  9(8).                    CI0072
            11            CX03-QTRCHK PICTURE  9(2).                    CI0072
            11            CX03-DNPMT  PICTURE  9(8).                    CI0072
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            CX03-CY02                                     CI0072
                          REDEFINES            CX03-CY99.               CI0072
            11            CX03-QSIRQ  PICTURE  99.                      CI0072
            11            CX03-QDRMN  PICTURE  9(2)                     CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX03-DDPRE  PICTURE  9(8).                    CI0072
            11            CX03-DDSHP  PICTURE  9(8).                    CI0072
            11            CX03-NDRFTB PICTURE  9(5).                    CI0072
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0072
            11            CX03-DDSHPA PICTURE  9(8).                    CI0072
            11            CX03-NDRFTF PICTURE  9(5).                    CI0072
            11            CX03-QDIPBK PICTURE  9(3).                    CI0072
            11            CX03-CREOR  PICTURE  X(1).                    CI0072
            11            CX03-CREOR1 PICTURE  X(1).                    CI0072
            11            CX03-DDASC  PICTURE  9(8).                    CI0072
            11            CX03-FILLER PICTURE  X(7).                    CI0072
            10            CX03-CY03                                     CI0072
                          REDEFINES            CX03-CY99.               CI0072
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0072
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0072
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0072
            11            CX03-DOPDA  PICTURE  99.                      CI0072
            11            CX03-CPMTF  PICTURE  99.                      CI0072
            11            CX03-CIRMO  PICTURE  X(12).                   CI0072
            11            CX03-CPALL  PICTURE  X(1).                    CI0072
            11            CX03-CCOLM  PICTURE  9(2).                    CI0072
            11            CX03-CBLTP  PICTURE  X(1).                    CI0072
            11            CX03-CASUB  PICTURE  9(2).                    CI0072
            11            CX03-CBLFM  PICTURE  9(2).                    CI0072
            11            CX03-IBILS  PICTURE  X.                       CI0072
            11            CX03-IPAOS  PICTURE  X.                       CI0072
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0072
            11            CX03-DLBPD  PICTURE  9(8).                    CI0072
            11            CX03-DNBPD  PICTURE  9(8).                    CI0072
            11            CX03-DODBD  PICTURE  9(8).                    CI0072
            11            CX03-CPSRE  PICTURE  99.                      CI0072
            11            CX03-ISPHN  PICTURE  X.                       CI0072
            11            CX03-TCARR  PICTURE  X(6).                    CI0072
            11            CX03-CBKPT  PICTURE  9(2).                    CI0072
            11            CX03-IECNT  PICTURE  X.                       CI0072
            11            CX03-ICONV  PICTURE  X(1).                    CI0072
            11            CX03-FILLER PICTURE  X(4).                    CI0072
            10            CX03-CY04                                     CI0072
                          REDEFINES            CX03-CY99.               CI0072
            11            CX03-CCARD  PICTURE  X(02).                   CI0072
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0072
            11            CX03-IREMT  PICTURE  X(01).                   CI0072
            11            CX03-ISBILA PICTURE  X.                       CI0072
            11            CX03-DLBPDA PICTURE  9(8).                    CI0072
            11            CX03-DNBPDA.                                  CI0072
            12            CX03-DNCYM  PICTURE  9(6).                    CI0072
            12            CX03-CEDTD  PICTURE  9(2).                    CI0072
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX03-DREMT  PICTURE  9(8).                    CI0072
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0072
            11            CX03-CWRC2  PICTURE  99.                      CI0072
            11            CX03-CHCR2  PICTURE  99.                      CI0072
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0072
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0072
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0072
       01                 CX06.                                         CI0072
            10            CX06-CX06K.                                   CI0072
            11            CX06-C299.                                    CI0072
            12            CX06-CTID.                                    CI0072
            13            CX06-CTIDA  PICTURE  9(3).                    CI0072
            13            CX06-CTIDN.                                   CI0072
            14            CX06-CTIDNP PICTURE  X(13).                   CI0072
            14            CX06-CTIDND PICTURE  9(11).                   CI0072
            10            CX06-NPECK  PICTURE  9(02).                   CI0072
            10            CX06-FILLER PICTURE  X.                       CI0072
       01                 CX09.                                         CI0072
            10            CX09-CX09K.                                   CI0072
            11            CX09-NPAIS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            CX09-CDEL1  PICTURE  9(3).                    CI0072
            10            CX09-NDELS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            CX09-CDEST  PICTURE  99.                      CI0072
            10            CX09-DISUP  PICTURE  9(8).                    CI0072
            10            CX09-CLUPD  PICTURE  9(3).                    CI0072
            10            CX09-DLAUP  PICTURE  9(8).                    CI0072
            10            CX09-GEOPD2 PICTURE  X(8).                    CI0072
            10            CX09-DPCHD  PICTURE  9(8).                    CI0072
            10            CX09-FILLER PICTURE  X(06).                   CI0072
       01                 CX13.                                         CI0072
            10            CX13-GELL   PICTURE  9(4)                     CI0072
                          BINARY.                                       CI0072
            10            CX13-CY20.                                    CI0072
            11            CX13-CX13K.                                   CI0072
            12            CX13-CARTZ  PICTURE  99.                      CI0072
            12            CX13-NAPDS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX13-GESTD  PICTURE  9(8).                    CI0072
            11            CX13-GEEND  PICTURE  9(8).                    CI0072
            11            CX13-DASUQ  PICTURE  9(8).                    CI0072
            11            CX13-CDEST  PICTURE  99.                      CI0072
            11            CX13-IIARR  PICTURE  X.                       CI0072
            11            CX13-DLAUP  PICTURE  9(8).                    CI0072
            11            CX13-GEOPD2 PICTURE  X(8).                    CI0072
            11            CX13-GEAUN  PICTURE  9(5).                    CI0072
            11            CX13-DPCHD  PICTURE  9(8).                    CI0072
            11            CX13-PPOT1  PICTURE  S9(3)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX13-ACOT1  PICTURE  S9(9)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX13-QPST1  PICTURE  S9(7)V999                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX13-FILLER PICTURE  X(03).                   CI0072
            10            CX13-CY96.                                    CI0072
            11            CX13-FILLER PICTURE  X(50).                   CI0072
            10            CX13-CY21                                     CI0072
                          REDEFINES            CX13-CY96.               CI0072
            11            CX13-DNPMT  PICTURE  9(8).                    CI0072
            11            CX13-CPMTF  PICTURE  99.                      CI0072
            11            CX13-ADBRQ  PICTURE  S9(11)V99                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX13-QSHOWQ PICTURE  S9(9)V999                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX13-PACT1  PICTURE  S999V999                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX13-DOPDA  PICTURE  99.                      CI0072
            11            CX13-DNEXE  PICTURE  9(8).                    CI0072
            11            CX13-CIRMO  PICTURE  X(12).                   CI0072
            10            CX13-CY98.                                    CI0072
            11            CX13-FILLER PICTURE  X(120).                  CI0072
            10            CX13-CY25                                     CI0072
                          REDEFINES            CX13-CY98.               CI0072
            11            CX13-COPTC  PICTURE  9(1).                    CI0072
            11            CX13-ILPOI  PICTURE  X(1).                    CI0072
            11            CX13-CATOC  PICTURE  X(1).                    CI0072
            11            CX13-CEOIA  PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX13-ACOAR  PICTURE  S9(9)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX13-CEOTR  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX13-DSTMO  PICTURE  99.                      CI0072
            10            CX13-CY27                                     CI0072
                          REDEFINES            CX13-CY98.               CI0072
            11            CX13-QMTH1  PICTURE  9(3).                    CI0072
            11            CX13-IDRMD  PICTURE  X.                       CI0072
            10            CX13-CY28                                     CI0072
                          REDEFINES            CX13-CY98.               CI0072
            11            CX13-AALLBL PICTURE  S9(8)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX13-PSURR  PICTURE  S9(3)V999                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX13-DFPMT  PICTURE  9(8).                    CI0072
            11            CX13-QMTHLA PICTURE  9(3).                    CI0072
            11            CX13-PWHLDS PICTURE  S999V9(5)                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX13-ISWHO  PICTURE  X(1).                    CI0072
            10            CX13-CY29                                     CI0072
                          REDEFINES            CX13-CY98.               CI0072
            11            CX13-IINDI1 PICTURE  X(1).                    CI0072
            11            CX13-IINDI2 PICTURE  X(1).                    CI0072
            11            CX13-IINDI3 PICTURE  X(1).                    CI0072
            11            CX13-PWHLD5 PICTURE  S999V99                  CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX13-CCSMQ  PICTURE  X.                       CI0072
            11            CX13-CPLEC  PICTURE  XX.                      CI0072
            11            CX13-IPTRDA PICTURE  X(01).                   CI0072
            11            CX13-GCUSPY PICTURE  X(12).                   CI0072
            11            CX13-ALOIDA PICTURE  S9(11)V99                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX13-DELOI  PICTURE  9(8).                    CI0072
            11            CX13-CLGND  PICTURE  X.                       CI0072
            11            CX13-CORTYA PICTURE  X(3).                    CI0072
            11            CX13-CPH3U  PICTURE  X.                       CI0072
            11            CX13-CNAVR  PICTURE  X(1).                    CI0072
            11            CX13-NEXEC  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
       01                 CX14.                                         CI0072
            10            CX14-GELL   PICTURE  9(4)                     CI0072
                          BINARY.                                       CI0072
            10            CX14-CX14K.                                   CI0072
            11            CX14-NPISQ  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            CX14-ACOTD  PICTURE  S9(9)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            CX14-PPOTD  PICTURE  S9(3)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            CX14-QPSTD  PICTURE  S9(7)V999                CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            CX14-CPITC  PICTURE  99.                      CI0072
            10            CX14-FILLER PICTURE  X(04).                   CI0072
            10            CX14-CY97.                                    CI0072
            11            CX14-FILLER PICTURE  X(32).                   CI0072
            10            CX14-CY30                                     CI0072
                          REDEFINES            CX14-CY97.               CI0072
            11            CX14-IOWNC  PICTURE  X.                       CI0072
            11            CX14-CTYPE  PICTURE  X.                       CI0072
            11            CX14-C299.                                    CI0072
            12            CX14-CTID.                                    CI0072
            13            CX14-CTIDA  PICTURE  9(3).                    CI0072
            13            CX14-CTIDN.                                   CI0072
            14            CX14-CTIDNP PICTURE  X(13).                   CI0072
            14            CX14-CTIDND PICTURE  9(11).                   CI0072
            11            CX14-CPMTC  PICTURE  99.                      CI0072
            11            CX14-IACSD  PICTURE  X.                       CI0072
            10            CX14-CY31                                     CI0072
                          REDEFINES            CX14-CY97.               CI0072
            11            CX14-FILLER PICTURE  X(2).                    CI0072
            11            CX14-IDELI  PICTURE  X.                       CI0072
            11            CX14-CDEL1  PICTURE  9(3).                    CI0072
            11            CX14-NDELS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            CX14-CY32                                     CI0072
                          REDEFINES            CX14-CY97.               CI0072
            11            CX14-GCUSPZ PICTURE  X(12).                   CI0072
       01                 CX2Y.                                         CI0072
            10            CX2Y-CX2YK.                                   CI0072
            11            CX2Y-C299.                                    CI0072
            12            CX2Y-CTID.                                    CI0072
            13            CX2Y-CTIDA  PICTURE  9(3).                    CI0072
            13            CX2Y-CTIDN.                                   CI0072
            14            CX2Y-CTIDNP PICTURE  X(13).                   CI0072
            14            CX2Y-CTIDND PICTURE  9(11).                   CI0072
            11            CX2Y-C199.                                    CI0072
            12            CX2Y-CLID.                                    CI0072
            13            CX2Y-CLIDO  PICTURE  9(3).                    CI0072
            13            CX2Y-CLIDN.                                   CI0072
            14            CX2Y-CLIDNP PICTURE  X(12).                   CI0072
            14            CX2Y-CLIDND PICTURE  9(8).                    CI0072
            11            CX2Y-CARTY  PICTURE  99.                      CI0072
            11            CX2Y-NARRS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
       01                 CX6Y.                                         CI0072
            10            CX6Y-CX6YK.                                   CI0072
            11            CX6Y-C299.                                    CI0072
            12            CX6Y-CTID.                                    CI0072
            13            CX6Y-CTIDA  PICTURE  9(3).                    CI0072
            13            CX6Y-CTIDN.                                   CI0072
            14            CX6Y-CTIDNP PICTURE  X(13).                   CI0072
            14            CX6Y-CTIDND PICTURE  9(11).                   CI0072
            11            CX6Y-C199.                                    CI0072
            12            CX6Y-CLID.                                    CI0072
            13            CX6Y-CLIDO  PICTURE  9(3).                    CI0072
            13            CX6Y-CLIDN.                                   CI0072
            14            CX6Y-CLIDNP PICTURE  X(12).                   CI0072
            14            CX6Y-CLIDND PICTURE  9(8).                    CI0072
            11            CX6Y-CARTY  PICTURE  99.                      CI0072
            11            CX6Y-NARRS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX6Y-CTID1  PICTURE  X(27).                   CI0072
            11            CX6Y-CARTZ  PICTURE  99.                      CI0072
            11            CX6Y-NAPDS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            CX6Y-NPISQ  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0072
            10            XW05-XW06.                                    CI0072
            11            XW05-XDBPCB.                                  CI0072
            12            XW05-XDBDNM PICTURE  X(08)                    CI0072
                          VALUE                SPACE.                   CI0072
            12            XW05-XSEGLV PICTURE  X(02)                    CI0072
                          VALUE                SPACE.                   CI0072
            12            XW05-XRC    PICTURE  X(02)                    CI0072
                          VALUE                SPACE.                   CI0072
            12            XW05-XPROPT PICTURE  X(04)                    CI0072
                          VALUE                SPACE.                   CI0072
            12            XW05-FILLER PICTURE  S9(5)                    CI0072
                          VALUE                ZERO                     CI0072
                          BINARY.                                       CI0072
            12            XW05-XSEGNM PICTURE  X(08)                    CI0072
                          VALUE                SPACE.                   CI0072
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0072
                          VALUE                ZERO                     CI0072
                          BINARY.                                       CI0072
            12            XW05-XSEGNB PICTURE  9(05)                    CI0072
                          VALUE                ZERO                     CI0072
                          BINARY.                                       CI0072
            12            XW05-XCOKEY PICTURE  X(70)                    CI0072
                          VALUE                SPACE.                   CI0072
            10            XW05-XW07.                                    CI0072
            11            XW05-XIOPCB.                                  CI0072
            12            XW05-XTERMI PICTURE  X(08)                    CI0072
                          VALUE                SPACE.                   CI0072
            12            XW05-FILLER PICTURE  XX                       CI0072
                          VALUE                SPACE.                   CI0072
            12            XW05-XRC1   PICTURE  X(02)                    CI0072
                          VALUE                SPACE.                   CI0072
            12            XW05-FILLER PICTURE  X(12)                    CI0072
                          VALUE                SPACE.                   CI0072
            12            XW05-XMODNM PICTURE  X(8)                     CI0072
                          VALUE                SPACE.                   CI0072
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0072
                          VALUE                ZERO.                    CI0072
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0072
                          VALUE                ZERO.                    CI0072
            10            XW05-XGU    PICTURE  X(4)                     CI0072
                          VALUE                'GU  '.                  CI0072
            10            XW05-XGHU   PICTURE  X(4)                     CI0072
                          VALUE                'GHU '.                  CI0072
            10            XW05-XGN    PICTURE  X(4)                     CI0072
                          VALUE                'GN  '.                  CI0072
            10            XW05-XGHN   PICTURE  X(4)                     CI0072
                          VALUE                'GHN '.                  CI0072
            10            XW05-XGNP   PICTURE  X(4)                     CI0072
                          VALUE                'GNP '.                  CI0072
            10            XW05-XGHNP  PICTURE  X(4)                     CI0072
                          VALUE                'GHNP'.                  CI0072
            10            XW05-XREPL  PICTURE  XXXX                     CI0072
                          VALUE                'REPL'.                  CI0072
            10            XW05-XISRT  PICTURE  X(4)                     CI0072
                          VALUE                'ISRT'.                  CI0072
            10            XW05-XDLET  PICTURE  X(4)                     CI0072
                          VALUE                'DLET'.                  CI0072
            10            XW05-XOPEN  PICTURE  X(4)                     CI0072
                          VALUE                'OPEN'.                  CI0072
            10            XW05-XCLSE  PICTURE  X(4)                     CI0072
                          VALUE                'CLSE'.                  CI0072
            10            XW05-XCHKP  PICTURE  X(4)                     CI0072
                          VALUE                'CHKP'.                  CI0072
            10            XW05-XXRST  PICTURE  X(4)                     CI0072
                          VALUE                'XRST'.                  CI0072
            10            XW05-XTERM  PICTURE  X(4)                     CI0072
                          VALUE                'TERM'.                  CI0072
            10            XW05-XNFPAC PICTURE  X(13)                    CI0072
                          VALUE                SPACE.                   CI0072
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0072
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0072
      *
      ******************************************************************
      **     SEGMENT THAT CONTAINS THE ACCOUNT OWNERSHIP AND           *
      **     BENEFICIARY FOR REQUESTED ACCOUNT ID NUMBER               *
      ******************************************************************
      *
      *!WF DSP=FA DSL=DU SEL=04 FOR=I LEV=1 PLT=FA
       01                 FA00.                                         CI0072
          05              FA00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00407).                  CI0072
       01                 FA04  REDEFINES      FA00.                    CI0072
            10            FA04-C299.                                    CI0072
            11            FA04-CTID.                                    CI0072
            12            FA04-CTIDA  PICTURE  9(3).                    CI0072
            12            FA04-CTIDN.                                   CI0072
            13            FA04-CTIDNP PICTURE  X(13).                   CI0072
            13            FA04-CTIDND PICTURE  9(11).                   CI0072
            10            FA04-IPOCH  PICTURE  X.                       CI0072
            10            FA04-FILLER PICTURE  X(099).                  CI0072
            10            FA04-CTTLN1 PICTURE  X(30).                   CI0072
            10            FA04-CTTLN2 PICTURE  X(30).                   CI0072
            10            FA04-CTTLN3 PICTURE  X(30).                   CI0072
            10            FA04-CTTBO1 PICTURE  X(45).                   CI0072
            10            FA04-CTTBO2 PICTURE  X(45).                   CI0072
            10            FA04-CTOWN  PICTURE  9(3).                    CI0072
            10            FA04-IUGMA  PICTURE  X.                       CI0072
            10            FA04-FILLER PICTURE  X(096).                  CI0072
      *
      ******************************************************************
      ** THIS SEGMENT IS THE INPUT / OUTPUT SEGMENT FOR MODULE CI0135  *
      ******************************************************************
      *
      *!WF DSP=JP DSL=PJ SEL=02 FOR=I LEV=1 PLT=JP
       01                 JP00.                                         CI0072
          05              JP00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00463).                  CI0072
       01                 JP02  REDEFINES      JP00.                    CI0072
            10            JP02-CTID   PICTURE  X(27).                   CI0072
            10            JP02-DCACG  PICTURE  9(8).                    CI0072
            10            JP02-ACCTV8 PICTURE  S9(9)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-AIDOL1 PICTURE  S9(9)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-AUINT1 PICTURE  S9(9)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-CXCSV  PICTURE  S9(7)V9(2)               CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-PCIRB5 PICTURE  S9(3)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-PANYDD PICTURE  S9(3)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-PCIRA5 PICTURE  S9(3)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-PANYDF PICTURE  9(3)V99                  CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-PCIRCB PICTURE  S9(3)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-PANYDG PICTURE  S9(3)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-PPART  PICTURE  9(3)V99                  CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-PMRTN  PICTURE  9(3)V99                  CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-PMRTEB PICTURE  S9(3)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-PBRITD PICTURE  S9(3)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-CEIAPI PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-CEIRND PICTURE  9(8).                    CI0072
            10            JP02-CEIT   PICTURE  9(3).                    CI0072
            10            JP02-DMATUR PICTURE  9(8).                    CI0072
            10            JP02-AMTUR  PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-CELBDT PICTURE  9(8).                    CI0072
            10            JP02-DTRME  PICTURE  9(8).                    CI0072
            10            JP02-NBSEI  PICTURE  999V99                   CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-NBSEIC PICTURE  S9(3)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-TRPTH  PICTURE  X(30).                   CI0072
            10            JP02-CELBL  PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-ALINT  PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-PELIRB PICTURE  S9(3)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-ASANP  PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-AAPAA  PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-IQLIF  PICTURE  X.                       CI0072
            10            JP02-QMTHAA PICTURE  9(2).                    CI0072
            10            JP02-QMTHCC PICTURE  9(2).                    CI0072
            10            JP02-QYEARA PICTURE  9(2).                    CI0072
            10            JP02-DANNIA PICTURE  9(8).                    CI0072
            10            JP02-PBONS  PICTURE  9(2).                    CI0072
            10            JP02-AARQDA PICTURE  S9(5)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-AACFA  PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-AIEPAA PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-CVSUR  PICTURE  X(30).                   CI0072
            10            JP02-CPRDA1 PICTURE  9(3).                    CI0072
            10            JP02-DFYR   PICTURE  9(4).                    CI0072
            10            JP02-DFYRB  PICTURE  9(4).                    CI0072
            10            JP02-DVALU  PICTURE  9(8).                    CI0072
            10            JP02-DNIPM  PICTURE  9(8).                    CI0072
            10            JP02-CIPFM  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-CESLD  PICTURE  9(8).                    CI0072
            10            JP02-CEHCD  PICTURE  9(3)                     CI0072
                          OCCURS       006     TIMES.                   CI0072
            10            JP02-CETYPC PICTURE  9(2).                    CI0072
            10            JP02-CEOTP  PICTURE  9(1).                    CI0072
            10            JP02-CEIIS  PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-DTRME1 PICTURE  9(8).                    CI0072
            10            JP02-CEFOIM PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-CEIPDA PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-GECTR  PICTURE  99.                      CI0072
            10            JP02-GMKTS.                                   CI0072
            11            JP02-DTRME2 PICTURE  9(8)                     CI0072
                          OCCURS       005     TIMES.                   CI0072
            11            JP02-DTRME3 PICTURE  9(8)                     CI0072
                          OCCURS       005     TIMES.                   CI0072
            10            JP02-PRCOD  PICTURE  9(5).                    CI0072
            10            JP02-CEFOTR PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            JP02-DGPED  PICTURE  9(8).                    CI0072
            10            JP02-DIPED  PICTURE  9(8).                    CI0072
            10            JP02-FILLER PICTURE  X(27).                   CI0072
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
                                                                        AM0003
      ******************************************************************AM0003
      **     PCB ADDRESS LIST FOR CI0003.  MODULE CI0003 WILL NEED     *AM0003
      **     PCB'S FOR:                                                *AM0003
      **                CONTRACT DATABASE(CT1P)                        *AM0003
      ******************************************************************AM0003
                                                                        AM0003
       01  CI0003A-PCB-ADDRESS-LIST.                                    AM0003
           05  CI0003A-PCB-CT1P-PTR1      POINTER.                      AM0003
                                                                        AM0135
      *-----> PCB address list for calling CI0135...                    AM0135
      *                                                                 AM0135
       01                 CI0135-PCB-ADDRESS-LIST.                      AM0135
           05             CI0135-PCB-CH1P-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CCRP-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CPRP-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CBTP-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CA1P-PTR1      POINTER.            AM0135
       01                 ST01.                                         CI0072
            10            ST01-CT01K.                                   CI0072
            11            ST01-C299.                                    CI0072
            12            ST01-CTID.                                    CI0072
            13            ST01-CTIDA  PICTURE  9(3).                    CI0072
            13            ST01-CTIDN.                                   CI0072
            14            ST01-CTIDNP PICTURE  X(13).                   CI0072
            14            ST01-CTIDND PICTURE  9(11).                   CI0072
            10            ST01-GECKD  PICTURE  9.                       CI0072
            10            ST01-GEMDA  PICTURE  9(8).                    CI0072
            10            ST01-NSEQ4B PICTURE  9(8)                     CI0072
                          BINARY.                                       CI0072
            10            ST01-GECUC  PICTURE  99.                      CI0072
            10            ST01-CTAUL  PICTURE  9(3).                    CI0072
            10            ST01-DIRAC  PICTURE  9(4).                    CI0072
            10            ST01-CTCCI  PICTURE  X.                       CI0072
            10            ST01-CTCUS  PICTURE  999.                     CI0072
            10            ST01-CTEFD  PICTURE  9(8).                    CI0072
            10            ST01-CTIAD  PICTURE  9(8).                    CI0072
            10            ST01-CLCUS  PICTURE  99.                      CI0072
            10            ST01-CAMMB  PICTURE  X(3).                    CI0072
            10            ST01-CKPMM  PICTURE  X.                       CI0072
            10            ST01-CTLAD  PICTURE  9(8).                    CI0072
            10            ST01-IPERS  PICTURE  X.                       CI0072
            10            ST01-AUNCB  PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            ST01-CTLAT  PICTURE  9(8).                    CI0072
            10            ST01-CTLATC PICTURE  9(6).                    CI0072
            10            ST01-IMEGA  PICTURE  X.                       CI0072
            10            ST01-DIRAB  PICTURE  9(8).                    CI0072
            10            ST01-COLRQ  PICTURE  X.                       CI0072
            10            ST01-ZDA04  PICTURE  X(4).                    CI0072
            10            ST01-CTLPD  PICTURE  9(8).                    CI0072
            10            ST01-CIRASP PICTURE  9.                       CI0072
            10            ST01-CIRATP PICTURE  99.                      CI0072
            10            ST01-DRTHC  PICTURE  9(8).                    CI0072
            10            ST01-CPPTC  PICTURE  X.                       CI0072
            10            ST01-ZDA06  PICTURE  X(6).                    CI0072
            10            ST01-CTACD  PICTURE  9(8).                    CI0072
            10            ST01-CTNLI  PICTURE  X.                       CI0072
            10            ST01-CTRHO  PICTURE  9(8).                    CI0072
            10            ST01-CTSGD  PICTURE  9(8).                    CI0072
            10            ST01-CPATP  PICTURE  X(1).                    CI0072
            10            ST01-IRSTA  PICTURE  X.                       CI0072
            10            ST01-CTSTA  PICTURE  99.                      CI0072
            10            ST01-CTSSC  PICTURE  99.                      CI0072
            10            ST01-PRLIN  PICTURE  9(3).                    CI0072
            10            ST01-PRCOD  PICTURE  9(5).                    CI0072
            10            ST01-PRSCD  PICTURE  X(9).                    CI0072
            10            ST01-CTLNI  PICTURE  X.                       CI0072
            10            ST01-AYSIDA PICTURE  9(3).                    CI0072
            10            ST01-AYSID  PICTURE  9(5).                    CI0072
            10            ST01-CTBMC  PICTURE  99.                      CI0072
            10            ST01-CINAR  PICTURE  99.                      CI0072
            10            ST01-CPHTR  PICTURE  X.                       CI0072
            10            ST01-CDSTR  PICTURE  XX.                      CI0072
            10            ST01-CQACT  PICTURE  999.                     CI0072
            10            ST01-CIRAS  PICTURE  999.                     CI0072
            10            ST01-CIRAT  PICTURE  999.                     CI0072
            10            ST01-CLRAY  PICTURE  9(5).                    CI0072
            10            ST01-CATTP  PICTURE  X.                       CI0072
      ******************************************************************ADUTAB
      **              TABLE TA5B ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA5B-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=5B FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA5B.                                                CI0072
           04    G-TA5B-PARAM.                                          CI0072
             10  G-TA5B-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0072
                        VALUE      +154.                                CI0072
             10  G-TA5B-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0072
                        VALUE      +001.                                CI0072
             10  G-TA5B-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0072
                        VALUE      +017.                                CI0072
             10  G-TA5B-NUAPP  PICTURE 99                               CI0072
                        VALUE       0.                                  CI0072
             10  G-TA5B-NUTAB  PICTURE X(6)                             CI0072
                        VALUE 'TA005B'.                                 CI0072
             10  G-TA5B-TABFO  PICTURE XX                 VALUE SPACE.  CI0072
             10  G-TA5B-TABCR  PICTURE XX                 VALUE SPACE.  CI0072
             10  G-TA5B-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0072
             10  G-TA5B-NUSSC  PICTURE X  VALUE   ' '.                  CI0072
             10  G-TA5B-NUSSY  PICTURE X                  VALUE SPACE.  CI0072
             10  G-TA5B-TRANID PICTURE X(4)               VALUE SPACE.  CI0072
             10  G-TA5B-FILSYS.                                         CI0072
             15  G-TA5B-USERC  PICTURE X(6)               VALUE SPACE.  CI0072
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0072
           04             TA5B.                                         CI0072
            10            TA5B-GAPSC.                                   CI0072
            11            TA5B-CTIDA  PICTURE  9(3)                     CI0072
                          VALUE                ZERO.                    CI0072
            11            TA5B-PRCOD  PICTURE  9(5)                     CI0072
                          VALUE                ZERO.                    CI0072
            11            TA5B-PRSCD  PICTURE  X(9)                     CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-PRCODX PICTURE  9(5)                     CI0072
                          VALUE                ZERO.                    CI0072
            10            TA5B-PRCSUB PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-PRCAUT PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-PRCBAS PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-PRCSTK PICTURE  XX                       CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-PRCPRE PICTURE  X(4)                     CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-IBDUP  PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-IUSPR  PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-CVSYS  PICTURE  X(2)                     CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-IDTOD  PICTURE  X(1)                     CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-GRSFC  PICTURE  99                       CI0072
                          VALUE                ZERO.                    CI0072
            10            TA5B-ZDA18  PICTURE  X(18)                    CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-CMPCTB PICTURE  X(4)                     CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-ITERM  PICTURE  X(1)                     CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-AMFAC  PICTURE  S9(7)                    CI0072
                          VALUE                ZERO.                    CI0072
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-CPRBK  PICTURE  X(3)                     CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-CFXDM  PICTURE  99                       CI0072
                          VALUE                ZERO.                    CI0072
            10            TA5B-NGLCS  PICTURE  X(5)                     CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-NDFCS  PICTURE  X(5)                     CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-CTNLI  PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-CBANK  PICTURE  X(03)                    CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-ISYPO  PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-ISYPP  PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-ICOPT  PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-IANPY  PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-IDSAR  PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-ICIPT  PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-IANDS  PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-IKPMA  PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-INMWT  PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-IVANT  PICTURE  X(1)                     CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-ISDAV  PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-IUDAV  PICTURE  X                        CI0072
                          VALUE                SPACE.                   CI0072
            10            TA5B-ZDA15  PICTURE  X(15)                    CI0072
                          VALUE                SPACE.                   CI0072
      **                                                                ADUTAB
      *                                                                 AM0090
      ******************************************************************AM0090
      ** WORKING STORAGE SEGMENT CI0090                                *AM0090
      ******************************************************************AM0090
      *                                                                 AM0090
      *!WF DSP=WA DSL=PJ SEL=08 FOR=I DES=1 LEV=1                       AM0090
       01                 WA08.                                         CI0072
            10            WA08-MAPPN  PICTURE  X(10).                   CI0072
            10            WA08-CLID   PICTURE  X(23).                   CI0072
            10            WA08-CARTY  PICTURE  99.                      CI0072
            10            WA08-NARRS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WA08-CTID   PICTURE  X(27).                   CI0072
            10            WA08-CARTZ  PICTURE  99.                      CI0072
            10            WA08-NAPDS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WA08-NPISQ  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WA08-NBASQ  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WA08-CDEL1  PICTURE  9(3).                    CI0072
            10            WA08-NDELS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WA08-IPERT  PICTURE  X.                       CI0072
            10            WA08-NEIBT  PICTURE  X(7).                    CI0072
            10            WA08-GESQ2C PICTURE  S99                      CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WA08-CACTM  PICTURE  X(1).                    CI0072
            10            WA08-CPROT  PICTURE  X(02).                   CI0072
            10            WA08-DCACG  PICTURE  9(8).                    CI0072
            10            WA08-GECSQ  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WA08-ICX01  PICTURE  X.                       CI0072
            10            WA08-ICX03  PICTURE  X.                       CI0072
            10            WA08-ICX06  PICTURE  X.                       CI0072
            10            WA08-ICX13  PICTURE  X.                       CI0072
            10            WA08-ICX14  PICTURE  X.                       CI0072
            10            WA08-ICX18  PICTURE  X.                       CI0072
            10            WA08-ICX21  PICTURE  X.                       CI0072
            10            WA08-CPMTF  PICTURE  99.                      CI0072
            10            WA08-NPAIS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WA08-ICX09  PICTURE  X.                       CI0072
            10            WA08-FILLER PICTURE  X(22).                   CI0072
      *                                                                 AM0090
       01  7-WA00-STAGING.                                              AM0090
      *!WI pl=WA080                                                     AM0090
           05  7-WA00-MAPPN                                             AM0090
                        PICTURE X(10).                                  CI0072
      *!WI pl=WA090                                                     AM0090
           05  7-WA00-CLID                                              AM0090
                        PICTURE X(23).                                  CI0072
      *!WI pl=WA095                                                     AM0090
           05  7-WA00-CARTY                                             AM0090
                        PICTURE 99.                                     CI0072
      *!WI pl=WA097                                                     AM0090
           05  7-WA00-NARRS                                             AM0090
                        PICTURE S9(3)                                   CI0072
                          COMPUTATIONAL-3.                              CI0072
      *!WI pl=WA100                                                     AM0090
           05  7-WA00-CTID                                              AM0090
                        PICTURE X(27).                                  CI0072
      *!WI pl=WA102                                                     AM0090
           05  7-WA00-CARTZ                                             AM0090
                        PICTURE 99.                                     CI0072
      *!WI pl=WA104                                                     AM0090
           05  7-WA00-NAPDS                                             AM0090
                        PICTURE S9(3)                                   CI0072
                          COMPUTATIONAL-3.                              CI0072
      *!WI pl=WA120                                                     AM0090
           05  7-WA00-NPISQ                                             AM0090
                        PICTURE S9(3)                                   CI0072
                          COMPUTATIONAL-3.                              CI0072
      *!WI pl=WA130                                                     AM0090
           05  7-WA00-NBASQ                                             AM0090
                        PICTURE S9(3)                                   CI0072
                          COMPUTATIONAL-3.                              CI0072
      *!WI pl=WA140                                                     AM0090
           05  7-WA00-CDEL1                                             AM0090
                        PICTURE 9(3).                                   CI0072
      *!WI pl=WA150                                                     AM0090
           05  7-WA00-NDELS                                             AM0090
                        PICTURE S9(3)                                   CI0072
                          COMPUTATIONAL-3.                              CI0072
      *!WI pl=WA160                                                     AM0090
           05  7-WA00-IPERT                                             AM0090
                        PICTURE X.                                      CI0072
      *!WI pl=WA170                                                     AM0090
           05  7-WA00-NEIBT                                             AM0090
                        PICTURE X(7).                                   CI0072
      *!WI pl=WA180                                                     AM0090
           05  7-WA00-GESQ2C                                            AM0090
                        PICTURE S99                                     CI0072
                          COMPUTATIONAL-3.                              CI0072
      *!WI pl=WA190                                                     AM0090
           05  7-WA00-NPAIS                                             AM0090
                        PICTURE S9(3)                                   CI0072
                          COMPUTATIONAL-3.                              CI0072
      *                                                                 AM0090
      *                                                                 AM0090
      ******************************************************************AM0090
      **     PCB ADDRESS LIST FOR CI0090.                              *AM0090
      ******************************************************************AM0090
      *                                                                 AM0090
      *-----> PCB ADDRESS LIST FOR CALLING MODULE CI0090                AM0090
      *       SEE -CD FOR CI0093 AND ALSO CI0090 FOR DB'S ACCESSED      AM0090
       01               CI0090-PCB-ADDRESS-LIST.                        AM0090
            05          CI0090-PCB-AR1P-PTR1        POINTER.            AM0090
            05          CI0090-PCB-CA1P-PTR1        POINTER.            AM0090
            05          CI0090-PCB-TR1P-PTR1        POINTER.            AM0090

       01   7-WA00-NOINST PIC X.
      ******************************************************************AM0083
      ** USE THE 88 FIELDS TO BREAK DOWN ARRAY FROM CI0083             *AM0083
      ******************************************************************AM0083
      **                                                                AM0083
      **                                                                AM0083
      *!WF DSP=WP DSL=K1 SEL=1F FOR=I LEV=1                             AM0083
       01                 WP00.                                         CI0072
          05              WP00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00266).                  CI0072
       01                 WP1F  REDEFINES      WP00.                    CI0072
            10            WP1F-INPUT.                                   CI0072
            11            WP1F-MAPPN  PICTURE  X(10).                   CI0072
            11            WP1F-PROGR  PICTURE  X(06).                   CI0072
            11            WP1F-ADDRLN.                                  CI0072
            12            WP1F-GESAD1 PICTURE  X(30).                   CI0072
            12            WP1F-GESAD2 PICTURE  X(30).                   CI0072
            12            WP1F-GESAD3 PICTURE  X(30).                   CI0072
            11            WP1F-FILLER PICTURE  X(100).                  CI0072
            10            WP1F-OUTPUT.                                  CI0072
            11            WP1F-IOWNC  PICTURE  X                        CI0072
                          OCCURS       060     TIMES.                   CI0072
      **                                                                AM0083
      *!WI pl=WP070                                                     AM0083
       01                 W-WP00-MAPPN                                  AM0083
                        PICTURE X(10).                                  CI0072
       01                 W-WP00-DECIPHER.                              AM0083
      *!WI pl=WP090                                                     AM0083
         05               W-WP01-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  CHARITABLE-REMAINDER      VALUE 'Y'.                     AM0083
      *!WI pl=WP110                                                     AM0083
         05               W-WP02-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  CONDITIONAL-MINOR         VALUE 'Y'.                     AM0083
      *!WI pl=WP130                                                     AM0083
         05               W-WP03-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  CORPORATION               VALUE 'Y'.                     AM0083
      *!WI pl=WP150                                                     AM0083
         05               W-WP04-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  CORPORATE-TRUSTEE         VALUE 'Y'.                     AM0083
      *!WI pl=WP170                                                     AM0083
         05               W-WP05-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  CUSTODIAL-TSCA            VALUE 'Y'.                     AM0083
      *!WI pl=WP190                                                     AM0083
         05               W-WP06-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  CUSTODIAL-KEOGH           VALUE 'Y'.                     AM0083
      *!WI pl=WP210                                                     AM0083
         05               W-WP07-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  DEFERRED-COMP-CORP        VALUE 'Y'.                     AM0083
      *!WI pl=WP230                                                     AM0083
         05               W-WP08-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  DEFERRED-COMP-GOVT        VALUE 'Y'.                     AM0083
      *!WI pl=WP250                                                     AM0083
         05               W-WP09-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  EXECUTOR                  VALUE 'Y'.                     AM0083
      *!WI pl=WP270                                                     AM0083
         05               W-WP10-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  GUARDIAN                  VALUE 'Y'.                     AM0083
      *!WI pl=WP290                                                     AM0083
         05               W-WP11-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  IRA-NON-AEFA              VALUE 'Y'.                     AM0083
      *!WI pl=WP310                                                     AM0083
         05               W-WP12-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  LIFE-TENANT               VALUE 'Y'.                     AM0083
      *!WI pl=WP330                                                     AM0083
         05               W-WP13-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  PARTNERSHIP               VALUE 'Y'.                     AM0083
      *!WI pl=WP350                                                     AM0083
         05               W-WP14-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  SOLE-PROPRIETOR           VALUE 'Y'.                     AM0083
      *!WI pl=WP370                                                     AM0083
         05               W-WP15-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  TENANTS-IN-COMMON         VALUE 'Y'.                     AM0083
      *!WI pl=WP390                                                     AM0083
         05               W-WP16-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  TRUST                     VALUE 'Y'.                     AM0083
      *!WI pl=WP410                                                     AM0083
         05               W-WP17-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  TRUSTEED-QUALIFIED-PLAN   VALUE 'Y'.                     AM0083
      *!WI pl=WP430                                                     AM0083
         05               W-WP18-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  USUFRUCTORY               VALUE 'Y'.                     AM0083
      *!WI pl=WP450                                                     AM0083
         05               W-WP19-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  A-401K                    VALUE 'Y'.                     AM0083
      *!WI pl=WP470                                                     AM0083
         05               W-WP20-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  TRANSFER-ON-DEATH         VALUE 'Y'.                     AM0083
      *!WI pl=WP490                                                     AM0083
         05               W-WP21-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  TENANTS-BY-ENTIRETY       VALUE 'Y'.                     AM0083
      *!WI pl=WP510                                                     AM0083
         05               W-WP22-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  CHURH                     VALUE 'Y'.                     AM0083
      *!WI pl=WP530                                                     AM0083
         05               W-WP23-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  COMMUNITY-PROPERTY        VALUE 'Y'.                     AM0083
      *!WI pl=WP550                                                     AM0083
         05               W-WP24-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  IRREVOCABLE               VALUE 'Y'.                     AM0083
      *!WI pl=WP570                                                     AM0083
         05               W-WP25-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  LIVING                    VALUE 'Y'.                     AM0083
      *!WI pl=WP590                                                     AM0083
         05               W-WP26-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  LOVING                    VALUE 'Y'.                     AM0083
      *!WI pl=WP610                                                     AM0083
         05               W-WP27-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  MARITAL                   VALUE 'Y'.                     AM0083
      *!WI pl=WP630                                                     AM0083
         05               W-WP28-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  REVOCABLE                 VALUE 'Y'.                     AM0083
      *!WI pl=WP650                                                     AM0083
         05               W-WP29-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  TESTAMENTARY              VALUE 'Y'.                     AM0083
      *!WI pl=WP670                                                     AM0083
         05               W-WP30-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  SSI-REPRESENTATIVE        VALUE 'Y'.                     AM0083
      *!WI pl=WP690                                                     AM0083
         05               W-WP31-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  LIMITED-LIABILITY         VALUE 'Y'.                     AM0083
      *!WI pl=WP710                                                     AM0083
         05               W-WP32-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  SEPARATE-PROPERTY         VALUE 'Y'.                     AM0083
      *!WI pl=WP730                                                     AM0083
         05               W-WP33-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  POA                       VALUE 'Y'.                     AM0083
      *!WI pl=WP750                                                     AM0083
         05               W-WP34-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  UTMA                      VALUE 'Y'.                     AM0083
      *!WI pl=WP770                                                     AM0083
         05               W-WP35-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  UGMA                      VALUE 'Y'.                     AM0083
      *!WI pl=WP790                                                     AM0083
         05               W-WP36-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  CURATOR                   VALUE 'Y'.                     AM0083
      *!WI pl=WP810                                                     AM0083
         05               W-WP37-IOWNC                                  AM0083
                        PICTURE X.                                      CI0072
           88  TUTOR                     VALUE 'Y'.                     AM0083
         05               FILLER                    PIC X(23).          AM0083
      *
      ******************************************************************
      ** WORKING STORAGE AREA FOR CI0072                               *
      ******************************************************************
      *
       01  7-WS-CTID.
      *!WI
           05  7-WS-CTIDA
                        PICTURE 9(3).                                   CI0072
           05  7-WS-CTIDN.
      *!WI
               10 7-WS-CTIDNP
                        PICTURE X(13).                                  CI0072
      *!WI
               10 7-WS-CTIDND
                        PICTURE 9(11).                                  CI0072
       01  WORK-CF.
           05  CX6Y-CF        PIC X.
       01  WORK-IK.
           05  TA5B-IK        PIC X.
           05  CX13-IK        PIC X.
       01  WORK-FLAGS.
           05  WS-DIRECT-DEPOSIT-IND       PIC X    VALUE 'N'.
               88  WS-DIRECT-DEPOSIT-FOUND          VALUE 'Y'.
           05  WS-SD-FOUND-IND             PIC X    VALUE 'N'.
               88  WS-SD-FOUND                      VALUE 'Y'.
           05  WS-CI0135-CALL-MADE-IND     PIC X    VALUE 'N'.
               88  WS-CI0135-CALL-MADE              VALUE 'Y'.
      **** SPECIFIED MUTUAL FUND ACCOUNTS(SOURCE)*************
      *!WI
       01  WS00-FR-PRCOD
                        PICTURE 9(5).                                   CI0072
           88 SPECIFIED-MF  VALUE 13, 16, 167.
      **** CLASS OF MUTUAL FUND ACCOUNTS(SOURCE)**************
      *!WI
       01  WS00-FR-PRSCD
                        PICTURE X(9).                                   CI0072
           88 FR-CLASS-A  VALUE '000000001'.
      **** B-SHARES TO BE CLOSED(DESTINATION)*****************
      *!WI
       01  WS00-TO-PRCOD
                        PICTURE 9(5).                                   CI0072
           88 CLOSE-B-SHARE VALUE 24, 42, 67, 102, 106, 107.
      **** CLASS OF MUTUAL FUND ACCOUNTS(DESTINATION)*********
      *!WI
       01  WS00-TO-PRSCD
                        PICTURE X(9).                                   CI0072
           88 TO-CLASS-B VALUE '000000002'.
       01                 WX01.                                         CI0072
            10            WX01-CX01K.                                   CI0072
            11            WX01-C199.                                    CI0072
            12            WX01-CLID.                                    CI0072
            13            WX01-CLIDO  PICTURE  9(3).                    CI0072
            13            WX01-CLIDN.                                   CI0072
            14            WX01-CLIDNP PICTURE  X(12).                   CI0072
            14            WX01-CLIDND PICTURE  9(8).                    CI0072
            10            WX01-GEMDA  PICTURE  9(8).                    CI0072
            10            WX01-NSEQ4B PICTURE  9(8)                     CI0072
                          BINARY.                                       CI0072
            10            WX01-FILLER PICTURE  X(5).                    CI0072
       01                 WX03.                                         CI0072
            10            WX03-GELL   PICTURE  9(4)                     CI0072
                          BINARY.                                       CI0072
            10            WX03-CY00.                                    CI0072
            11            WX03-CX03K.                                   CI0072
            12            WX03-CARTY  PICTURE  99.                      CI0072
            12            WX03-NARRS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX03-CARST  PICTURE  99.                      CI0072
            11            WX03-GECSQ  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX03-CPMTG  PICTURE  99.                      CI0072
            11            WX03-GRCRNG PICTURE  9(3).                    CI0072
            11            WX03-DEXDT  PICTURE  9(8).                    CI0072
            11            WX03-DASUP  PICTURE  9(8).                    CI0072
            11            WX03-CSTEC  PICTURE  X(3).                    CI0072
            11            WX03-FILLER PICTURE  X(17).                   CI0072
            11            WX03-CY50.                                    CI0072
            12            WX03-NARID  PICTURE  X(30).                   CI0072
            11            WX03-CY51                                     CI0072
                          REDEFINES            WX03-CY50.               CI0072
            12            WX03-NDIDN  PICTURE  9(12).                   CI0072
            12            WX03-FILLER PICTURE  X(18).                   CI0072
            11            WX03-CY52                                     CI0072
                          REDEFINES            WX03-CY50.               CI0072
            12            WX03-NAIDC  PICTURE  9(12).                   CI0072
            12            WX03-FILLER PICTURE  X(18).                   CI0072
            11            WX03-CY53                                     CI0072
                          REDEFINES            WX03-CY50.               CI0072
            12            WX03-NAMEXB PICTURE  9(15).                   CI0072
            12            WX03-FILLER PICTURE  X(15).                   CI0072
            10            WX03-CY99.                                    CI0072
            11            WX03-FILLER PICTURE  X(109).                  CI0072
            10            WX03-CY01                                     CI0072
                          REDEFINES            WX03-CY99.               CI0072
            11            WX03-NBASQ  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX03-ICPCI  PICTURE  X.                       CI0072
            11            WX03-CLUPD  PICTURE  9(3).                    CI0072
            11            WX03-DLAUP  PICTURE  9(8).                    CI0072
            11            WX03-CWRC   PICTURE  99.                      CI0072
            11            WX03-CHCR   PICTURE  99.                      CI0072
            11            WX03-GEOPD2 PICTURE  X(8).                    CI0072
            11            WX03-GEAUN  PICTURE  9(5).                    CI0072
            11            WX03-DPCHD  PICTURE  9(8).                    CI0072
            11            WX03-DLRCHK PICTURE  9(8).                    CI0072
            11            WX03-QTRCHK PICTURE  9(2).                    CI0072
            11            WX03-DNPMT  PICTURE  9(8).                    CI0072
            11            WX03-APMTLA PICTURE  S9(9)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WX03-CY02                                     CI0072
                          REDEFINES            WX03-CY99.               CI0072
            11            WX03-QSIRQ  PICTURE  99.                      CI0072
            11            WX03-QDRMN  PICTURE  9(2)                     CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX03-DDPRE  PICTURE  9(8).                    CI0072
            11            WX03-DDSHP  PICTURE  9(8).                    CI0072
            11            WX03-NDRFTB PICTURE  9(5).                    CI0072
            11            WX03-QDIPBJ PICTURE  9(3).                    CI0072
            11            WX03-DDSHPA PICTURE  9(8).                    CI0072
            11            WX03-NDRFTF PICTURE  9(5).                    CI0072
            11            WX03-QDIPBK PICTURE  9(3).                    CI0072
            11            WX03-CREOR  PICTURE  X(1).                    CI0072
            11            WX03-CREOR1 PICTURE  X(1).                    CI0072
            11            WX03-DDASC  PICTURE  9(8).                    CI0072
            11            WX03-FILLER PICTURE  X(7).                    CI0072
            10            WX03-CY03                                     CI0072
                          REDEFINES            WX03-CY99.               CI0072
            11            WX03-DLAUP1 PICTURE  9(8).                    CI0072
            11            WX03-GEOPD3 PICTURE  X(8).                    CI0072
            11            WX03-DNPMT1 PICTURE  9(8).                    CI0072
            11            WX03-DOPDA  PICTURE  99.                      CI0072
            11            WX03-CPMTF  PICTURE  99.                      CI0072
            11            WX03-CIRMO  PICTURE  X(12).                   CI0072
            11            WX03-CPALL  PICTURE  X(1).                    CI0072
            11            WX03-CCOLM  PICTURE  9(2).                    CI0072
            11            WX03-CBLTP  PICTURE  X(1).                    CI0072
            11            WX03-CASUB  PICTURE  9(2).                    CI0072
            11            WX03-CBLFM  PICTURE  9(2).                    CI0072
            11            WX03-IBILS  PICTURE  X.                       CI0072
            11            WX03-IPAOS  PICTURE  X.                       CI0072
            11            WX03-CBLSQ  PICTURE  X(4).                    CI0072
            11            WX03-DLBPD  PICTURE  9(8).                    CI0072
            11            WX03-DNBPD  PICTURE  9(8).                    CI0072
            11            WX03-DODBD  PICTURE  9(8).                    CI0072
            11            WX03-CPSRE  PICTURE  99.                      CI0072
            11            WX03-ISPHN  PICTURE  X.                       CI0072
            11            WX03-TCARR  PICTURE  X(6).                    CI0072
            11            WX03-CBKPT  PICTURE  9(2).                    CI0072
            11            WX03-IECNT  PICTURE  X.                       CI0072
            11            WX03-ICONV  PICTURE  X(1).                    CI0072
            11            WX03-FILLER PICTURE  X(4).                    CI0072
            10            WX03-CY04                                     CI0072
                          REDEFINES            WX03-CY99.               CI0072
            11            WX03-CCARD  PICTURE  X(02).                   CI0072
            11            WX03-MCSIG4 PICTURE  X(20).                   CI0072
            11            WX03-IREMT  PICTURE  X(01).                   CI0072
            11            WX03-ISBILA PICTURE  X.                       CI0072
            11            WX03-DLBPDA PICTURE  9(8).                    CI0072
            11            WX03-DNBPDA.                                  CI0072
            12            WX03-DNCYM  PICTURE  9(6).                    CI0072
            12            WX03-CEDTD  PICTURE  9(2).                    CI0072
            11            WX03-AREMT  PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX03-DREMT  PICTURE  9(8).                    CI0072
            11            WX03-ADBRQ  PICTURE  S9(11)V99                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX03-CLUPD1 PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX03-DLAUP3 PICTURE  9(8).                    CI0072
            11            WX03-CWRC2  PICTURE  99.                      CI0072
            11            WX03-CHCR2  PICTURE  99.                      CI0072
            11            WX03-GEOPD9 PICTURE  X(8).                    CI0072
            11            WX03-GEAUN1 PICTURE  9(5).                    CI0072
            11            WX03-DPCHD1 PICTURE  9(8).                    CI0072
       01                 WX06.                                         CI0072
            10            WX06-CX06K.                                   CI0072
            11            WX06-C299.                                    CI0072
            12            WX06-CTID.                                    CI0072
            13            WX06-CTIDA  PICTURE  9(3).                    CI0072
            13            WX06-CTIDN.                                   CI0072
            14            WX06-CTIDNP PICTURE  X(13).                   CI0072
            14            WX06-CTIDND PICTURE  9(11).                   CI0072
            10            WX06-NPECK  PICTURE  9(02).                   CI0072
            10            WX06-FILLER PICTURE  X.                       CI0072
       01                 WX09.                                         CI0072
            10            WX09-CX09K.                                   CI0072
            11            WX09-NPAIS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WX09-CDEL1  PICTURE  9(3).                    CI0072
            10            WX09-NDELS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WX09-CDEST  PICTURE  99.                      CI0072
            10            WX09-DISUP  PICTURE  9(8).                    CI0072
            10            WX09-CLUPD  PICTURE  9(3).                    CI0072
            10            WX09-DLAUP  PICTURE  9(8).                    CI0072
            10            WX09-GEOPD2 PICTURE  X(8).                    CI0072
            10            WX09-DPCHD  PICTURE  9(8).                    CI0072
            10            WX09-FILLER PICTURE  X(06).                   CI0072
       01                 WX13.                                         CI0072
            10            WX13-GELL   PICTURE  9(4)                     CI0072
                          BINARY.                                       CI0072
            10            WX13-CY20.                                    CI0072
            11            WX13-CX13K.                                   CI0072
            12            WX13-CARTZ  PICTURE  99.                      CI0072
            12            WX13-NAPDS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX13-GESTD  PICTURE  9(8).                    CI0072
            11            WX13-GEEND  PICTURE  9(8).                    CI0072
            11            WX13-DASUQ  PICTURE  9(8).                    CI0072
            11            WX13-CDEST  PICTURE  99.                      CI0072
            11            WX13-IIARR  PICTURE  X.                       CI0072
            11            WX13-DLAUP  PICTURE  9(8).                    CI0072
            11            WX13-GEOPD2 PICTURE  X(8).                    CI0072
            11            WX13-GEAUN  PICTURE  9(5).                    CI0072
            11            WX13-DPCHD  PICTURE  9(8).                    CI0072
            11            WX13-PPOT1  PICTURE  S9(3)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX13-ACOT1  PICTURE  S9(9)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX13-QPST1  PICTURE  S9(7)V999                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX13-FILLER PICTURE  X(03).                   CI0072
            10            WX13-CY96.                                    CI0072
            11            WX13-FILLER PICTURE  X(50).                   CI0072
            10            WX13-CY21                                     CI0072
                          REDEFINES            WX13-CY96.               CI0072
            11            WX13-DNPMT  PICTURE  9(8).                    CI0072
            11            WX13-CPMTF  PICTURE  99.                      CI0072
            11            WX13-ADBRQ  PICTURE  S9(11)V99                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX13-QSHOWQ PICTURE  S9(9)V999                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX13-PACT1  PICTURE  S999V999                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX13-DOPDA  PICTURE  99.                      CI0072
            11            WX13-DNEXE  PICTURE  9(8).                    CI0072
            11            WX13-CIRMO  PICTURE  X(12).                   CI0072
            10            WX13-CY98.                                    CI0072
            11            WX13-FILLER PICTURE  X(120).                  CI0072
            10            WX13-CY25                                     CI0072
                          REDEFINES            WX13-CY98.               CI0072
            11            WX13-COPTC  PICTURE  9(1).                    CI0072
            11            WX13-ILPOI  PICTURE  X(1).                    CI0072
            11            WX13-CATOC  PICTURE  X(1).                    CI0072
            11            WX13-CEOIA  PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX13-ACOAR  PICTURE  S9(9)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX13-CEOTR  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX13-DSTMO  PICTURE  99.                      CI0072
            10            WX13-CY27                                     CI0072
                          REDEFINES            WX13-CY98.               CI0072
            11            WX13-QMTH1  PICTURE  9(3).                    CI0072
            11            WX13-IDRMD  PICTURE  X.                       CI0072
            10            WX13-CY28                                     CI0072
                          REDEFINES            WX13-CY98.               CI0072
            11            WX13-AALLBL PICTURE  S9(8)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX13-PSURR  PICTURE  S9(3)V999                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX13-DFPMT  PICTURE  9(8).                    CI0072
            11            WX13-QMTHLA PICTURE  9(3).                    CI0072
            11            WX13-PWHLDS PICTURE  S999V9(5)                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX13-ISWHO  PICTURE  X(1).                    CI0072
            10            WX13-CY29                                     CI0072
                          REDEFINES            WX13-CY98.               CI0072
            11            WX13-IINDI1 PICTURE  X(1).                    CI0072
            11            WX13-IINDI2 PICTURE  X(1).                    CI0072
            11            WX13-IINDI3 PICTURE  X(1).                    CI0072
            11            WX13-PWHLD5 PICTURE  S999V99                  CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX13-CCSMQ  PICTURE  X.                       CI0072
            11            WX13-CPLEC  PICTURE  XX.                      CI0072
            11            WX13-IPTRDA PICTURE  X(01).                   CI0072
            11            WX13-GCUSPY PICTURE  X(12).                   CI0072
            11            WX13-ALOIDA PICTURE  S9(11)V99                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX13-DELOI  PICTURE  9(8).                    CI0072
            11            WX13-CLGND  PICTURE  X.                       CI0072
            11            WX13-CORTYA PICTURE  X(3).                    CI0072
            11            WX13-CPH3U  PICTURE  X.                       CI0072
            11            WX13-CNAVR  PICTURE  X(1).                    CI0072
            11            WX13-NEXEC  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
       01                 WX14.                                         CI0072
            10            WX14-GELL   PICTURE  9(4)                     CI0072
                          BINARY.                                       CI0072
            10            WX14-CX14K.                                   CI0072
            11            WX14-NPISQ  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WX14-ACOTD  PICTURE  S9(9)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WX14-PPOTD  PICTURE  S9(3)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WX14-QPSTD  PICTURE  S9(7)V999                CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WX14-CPITC  PICTURE  99.                      CI0072
            10            WX14-FILLER PICTURE  X(04).                   CI0072
            10            WX14-CY97.                                    CI0072
            11            WX14-FILLER PICTURE  X(32).                   CI0072
            10            WX14-CY30                                     CI0072
                          REDEFINES            WX14-CY97.               CI0072
            11            WX14-IOWNC  PICTURE  X.                       CI0072
            11            WX14-CTYPE  PICTURE  X.                       CI0072
            11            WX14-C299.                                    CI0072
            12            WX14-CTID.                                    CI0072
            13            WX14-CTIDA  PICTURE  9(3).                    CI0072
            13            WX14-CTIDN.                                   CI0072
            14            WX14-CTIDNP PICTURE  X(13).                   CI0072
            14            WX14-CTIDND PICTURE  9(11).                   CI0072
            11            WX14-CPMTC  PICTURE  99.                      CI0072
            11            WX14-IACSD  PICTURE  X.                       CI0072
            10            WX14-CY31                                     CI0072
                          REDEFINES            WX14-CY97.               CI0072
            11            WX14-FILLER PICTURE  X(2).                    CI0072
            11            WX14-IDELI  PICTURE  X.                       CI0072
            11            WX14-CDEL1  PICTURE  9(3).                    CI0072
            11            WX14-NDELS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WX14-CY32                                     CI0072
                          REDEFINES            WX14-CY97.               CI0072
            11            WX14-GCUSPZ PICTURE  X(12).                   CI0072
       01                 WX18.                                         CI0072
            10            WX18-CX18K.                                   CI0072
            11            WX18-NBASQ  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WX18-NPBN   PICTURE  X(20).                   CI0072
            10            WX18-CCBAT  PICTURE  99.                      CI0072
            10            WX18-DACHP  PICTURE  9(8).                    CI0072
            10            WX18-CSTPRE PICTURE  99.                      CI0072
            10            WX18-C199.                                    CI0072
            11            WX18-CLID.                                    CI0072
            12            WX18-CLIDO  PICTURE  9(3).                    CI0072
            12            WX18-CLIDN.                                   CI0072
            13            WX18-CLIDNP PICTURE  X(12).                   CI0072
            13            WX18-CLIDND PICTURE  9(8).                    CI0072
            10            WX18-MCSIG  PICTURE  X(30).                   CI0072
            10            WX18-CPBNU  PICTURE  X.                       CI0072
            10            WX18-CSPCR  PICTURE  99.                      CI0072
            10            WX18-DAPCR  PICTURE  9(8).                    CI0072
            10            WX18-FILLER PICTURE  XX.                      CI0072
       01                 WX2Y.                                         CI0072
            10            WX2Y-CX2YK.                                   CI0072
            11            WX2Y-C299.                                    CI0072
            12            WX2Y-CTID.                                    CI0072
            13            WX2Y-CTIDA  PICTURE  9(3).                    CI0072
            13            WX2Y-CTIDN.                                   CI0072
            14            WX2Y-CTIDNP PICTURE  X(13).                   CI0072
            14            WX2Y-CTIDND PICTURE  9(11).                   CI0072
            11            WX2Y-C199.                                    CI0072
            12            WX2Y-CLID.                                    CI0072
            13            WX2Y-CLIDO  PICTURE  9(3).                    CI0072
            13            WX2Y-CLIDN.                                   CI0072
            14            WX2Y-CLIDNP PICTURE  X(12).                   CI0072
            14            WX2Y-CLIDND PICTURE  9(8).                    CI0072
            11            WX2Y-CARTY  PICTURE  99.                      CI0072
            11            WX2Y-NARRS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
       01                 WX21.                                         CI0072
            10            WX21-GELL   PICTURE  9(4)                     CI0072
                          BINARY.                                       CI0072
            10            WX21-CZ00.                                    CI0072
            11            WX21-CX21K.                                   CI0072
            12            WX21-CDEL1  PICTURE  9(3).                    CI0072
            12            WX21-NDELS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WX21-CZ99.                                    CI0072
            11            WX21-FILLER PICTURE  X(165).                  CI0072
            10            WX21-CZ01                                     CI0072
                          REDEFINES            WX21-CZ99.               CI0072
            11            WX21-NBASQ  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX21-GECSQ  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            WX21-CZ02                                     CI0072
                          REDEFINES            WX21-CZ99.               CI0072
            11            WX21-CPAYE  PICTURE  9(2).                    CI0072
            11            WX21-C199.                                    CI0072
            12            WX21-CLID.                                    CI0072
            13            WX21-CLIDO  PICTURE  9(3).                    CI0072
            13            WX21-CLIDN.                                   CI0072
            14            WX21-CLIDNP PICTURE  X(12).                   CI0072
            14            WX21-CLIDND PICTURE  9(8).                    CI0072
            11            WX21-GECSQ1 PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX21-NBASQT PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            WX21-TDELI  PICTURE  X(30).                   CI0072
       01   DEBUT-WSS.                                                  CI0072
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0072
            05   IK     PICTURE X.                                      CI0072
       01  CONSTANTES-PAC.                                              CI0072
           05  FILLER  PICTURE X(87)   VALUE                            CI0072
                     '6015 CAT09/08/14CI0072ADMIN   14:34:35CI0072P AMERCI0072
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0072
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0072
           05  NUGNA   PICTURE X(5).                                    CI0072
           05  APPLI   PICTURE X(3).                                    CI0072
           05  DATGN   PICTURE X(8).                                    CI0072
           05  PROGR   PICTURE X(6).                                    CI0072
           05  CODUTI  PICTURE X(8).                                    CI0072
           05  TIMGN   PICTURE X(8).                                    CI0072
           05  PROGE   PICTURE X(8).                                    CI0072
           05  COBASE  PICTURE X(4).                                    CI0072
           05  DATGNC  PICTURE X(10).                                   CI0072
           05  RELEAS  PICTURE X(7).                                    CI0072
           05  DATGE   PICTURE X(10).                                   CI0072
           05  DATSQ   PICTURE X(10).                                   CI0072
       01  DATCE.                                                       CI0072
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0072
         05  DATOR.                                                     CI0072
           10  DATOA  PICTURE XX.                                       CI0072
           10  DATOM  PICTURE XX.                                       CI0072
           10  DATOJ  PICTURE XX.                                       CI0072
       01   VARIABLES-CONDITIONNELLES.                                  CI0072
            05                  FT      PICTURE X VALUE '0'.            CI0072
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0072
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0072
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0072
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0072
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0072
            05       5-PS00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0072
            05       5-ST00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0072
            05       5-WX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0072
       01               S-CT01-SSA.                                     CI0072
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CT01    '.                 CI0072
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-CT01-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-CTU01-SSA.                                       CI0072
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CT01    '.                 CI0072
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CT01K'.                   CI0072
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-CTU01-CT01K.                                     CI0072
            11       S-CTU01-C299.                                      CI0072
            12       S-CTU01-CTID.                                      CI0072
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0072
            13       S-CTU01-CTIDN.                                     CI0072
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0072
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-CT13-SSA.                                     CI0072
            10         S1-CT13-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CT13    '.                 CI0072
            10         S1-CT13-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-CT13-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-CTU13-SSA.                                       CI0072
            10      S1-CTU13-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CT13    '.                 CI0072
            10      S1-CTU13-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-CTU13-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-CTU13-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CT13K'.                   CI0072
            10       S-CTU13-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-CTU13-CT13K.                                     CI0072
            11       S-CTU13-GEHSD    PICTURE  9(8).                    CI0072
            11       S-CTU13-GEHCD    PICTURE  9(3).                    CI0072
            11       S-CTU13-GEHCSE   PICTURE  X(12).                   CI0072
            11       S-CTU13-GEHCSU   PICTURE  9(5).                    CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-CX01-SSA.                                     CI0072
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX01    '.                 CI0072
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-CX01-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-CXU01-SSA.                                       CI0072
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX01    '.                 CI0072
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX01K'.                   CI0072
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-CXU01-CX01K.                                     CI0072
            11       S-CXU01-C199.                                      CI0072
            12       S-CXU01-CLID.                                      CI0072
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0072
            13       S-CXU01-CLIDN.                                     CI0072
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0072
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-CX03-SSA.                                     CI0072
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX03    '.                 CI0072
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-CX03-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-CXA03-SSA.                                       CI0072
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0072
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CARTY'.                   CI0072
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0072
            12       S-CXA03-CARTY    PICTURE  99.                      CI0072
            12  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-CXB03-SSA.                                       CI0072
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0072
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(NARRS'.                   CI0072
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0072
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            12  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-CXC03-SSA.                                       CI0072
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CPMTG'.                   CI0072
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-CXD03-SSA.                                       CI0072
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(GRCRNG'.                  CI0072
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-CXE03-SSA.                                       CI0072
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(DEXDT'.                   CI0072
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-CXF03-SSA.                                       CI0072
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CY50'.                    CI0072
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-CXF03-CY50.                                      CI0072
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-CXG03-SSA.                                       CI0072
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(NBASQ'.                   CI0072
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-CXH03-SSA.                                       CI0072
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0072
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(NARID'.                   CI0072
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0072
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0072
            12  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-CXU03-SSA.                                       CI0072
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX03K'.                   CI0072
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-CXU03-CX03K.                                     CI0072
            12       S-CXU03-CARTY    PICTURE  99.                      CI0072
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-CX06-SSA.                                     CI0072
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX06    '.                 CI0072
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-CX06-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-CXU06-SSA.                                       CI0072
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX06    '.                 CI0072
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX06K'.                   CI0072
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-CXU06-CX06K.                                     CI0072
            11       S-CXU06-C299.                                      CI0072
            12       S-CXU06-CTID.                                      CI0072
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0072
            13       S-CXU06-CTIDN.                                     CI0072
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0072
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-CX09-SSA.                                     CI0072
            10         S1-CX09-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX09    '.                 CI0072
            10         S1-CX09-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-CX09-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-CXU09-SSA.                                       CI0072
            10      S1-CXU09-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX09    '.                 CI0072
            10      S1-CXU09-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-CXU09-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-CXU09-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX09K'.                   CI0072
            10       S-CXU09-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-CXU09-CX09K.                                     CI0072
            11       S-CXU09-NPAIS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-CX13-SSA.                                     CI0072
            10         S1-CX13-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX13    '.                 CI0072
            10         S1-CX13-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-CX13-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-CXA13-SSA.                                       CI0072
            11      S1-CXA13-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX13    '.                 CI0072
            11      S1-CXA13-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-CXA13-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-CXA13-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CDEST'.                   CI0072
            11       S-CXA13-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-CXA13-CDEST    PICTURE  99.                      CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-CXB13-SSA.                                       CI0072
            12      S1-CXB13-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX13    '.                 CI0072
            12      S1-CXB13-CCOM   PICTURE X VALUE '*'.                CI0072
            12       S-CXB13-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            12      S1-CXB13-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CARTZ'.                   CI0072
            12       S-CXB13-OPER  PICTURE XX VALUE ' ='.               CI0072
            12       S-CXB13-CARTZ    PICTURE  99.                      CI0072
            12  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-CXC13-SSA.                                       CI0072
            12      S1-CXC13-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX13    '.                 CI0072
            12      S1-CXC13-CCOM   PICTURE X VALUE '*'.                CI0072
            12       S-CXC13-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            12      S1-CXC13-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(NAPDS'.                   CI0072
            12       S-CXC13-OPER  PICTURE XX VALUE ' ='.               CI0072
            12       S-CXC13-NAPDS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            12  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-CXU13-SSA.                                       CI0072
            11      S1-CXU13-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX13    '.                 CI0072
            11      S1-CXU13-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-CXU13-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-CXU13-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX13K'.                   CI0072
            11       S-CXU13-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-CXU13-CX13K.                                     CI0072
            12       S-CXU13-CARTZ    PICTURE  99.                      CI0072
            12       S-CXU13-NAPDS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-CX113-SSA.                                       CI0072
            11      S1-CX113-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX13    '.                 CI0072
            11      S1-CX113-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-CX113-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-CX113-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(XGCUSPY'.                 CI0072
            11       S-CX113-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-CX113-GCUSPY   PICTURE  X(12).                   CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-CX14-SSA.                                     CI0072
            10         S1-CX14-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX14    '.                 CI0072
            10         S1-CX14-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-CX14-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-CXU14-SSA.                                       CI0072
            10      S1-CXU14-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX14    '.                 CI0072
            10      S1-CXU14-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-CXU14-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-CXU14-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX14K'.                   CI0072
            10       S-CXU14-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-CXU14-CX14K.                                     CI0072
            11       S-CXU14-NPISQ    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-CX114-SSA.                                       CI0072
            11      S1-CX114-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX14    '.                 CI0072
            11      S1-CX114-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-CX114-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-CX114-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(XGCUSPZ'.                 CI0072
            11       S-CX114-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-CX114-GCUSPZ   PICTURE  X(12).                   CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-CX2Y-SSA.                                     CI0072
            10         S1-CX2Y-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX2Y    '.                 CI0072
            10         S1-CX2Y-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-CX2Y-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-CXA2Y-SSA.                                       CI0072
            11      S1-CXA2Y-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX2Y    '.                 CI0072
            11      S1-CXA2Y-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-CXA2Y-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-CXA2Y-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CARTY'.                   CI0072
            11       S-CXA2Y-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-CXA2Y-CARTY    PICTURE  99.                      CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-CXB2Y-SSA.                                       CI0072
            11      S1-CXB2Y-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX2Y    '.                 CI0072
            11      S1-CXB2Y-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-CXB2Y-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-CXB2Y-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(C299'.                    CI0072
            11       S-CXB2Y-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-CXB2Y-C299.                                      CI0072
            12       S-CXB2Y-CTID.                                      CI0072
            13       S-CXB2Y-CTIDA    PICTURE  9(3).                    CI0072
            13       S-CXB2Y-CTIDN.                                     CI0072
            14       S-CXB2Y-CTIDNP   PICTURE  X(13).                   CI0072
            14       S-CXB2Y-CTIDND   PICTURE  9(11).                   CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-CXU2Y-SSA.                                       CI0072
            10      S1-CXU2Y-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX2Y    '.                 CI0072
            10      S1-CXU2Y-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-CXU2Y-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-CXU2Y-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX2YK'.                   CI0072
            10       S-CXU2Y-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-CXU2Y-CX2YK.                                     CI0072
            11       S-CXU2Y-C299.                                      CI0072
            12       S-CXU2Y-CTID.                                      CI0072
            13       S-CXU2Y-CTIDA    PICTURE  9(3).                    CI0072
            13       S-CXU2Y-CTIDN.                                     CI0072
            14       S-CXU2Y-CTIDNP   PICTURE  X(13).                   CI0072
            14       S-CXU2Y-CTIDND   PICTURE  9(11).                   CI0072
            11       S-CXU2Y-C199.                                      CI0072
            12       S-CXU2Y-CLID.                                      CI0072
            13       S-CXU2Y-CLIDO    PICTURE  9(3).                    CI0072
            13       S-CXU2Y-CLIDN.                                     CI0072
            14       S-CXU2Y-CLIDNP   PICTURE  X(12).                   CI0072
            14       S-CXU2Y-CLIDND   PICTURE  9(8).                    CI0072
            11       S-CXU2Y-CARTY    PICTURE  99.                      CI0072
            11       S-CXU2Y-NARRS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-CX6Y-SSA.                                     CI0072
            10         S1-CX6Y-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX6Y    '.                 CI0072
            10         S1-CX6Y-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-CX6Y-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-CXU6Y-SSA.                                       CI0072
            10      S1-CXU6Y-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX6Y    '.                 CI0072
            10      S1-CXU6Y-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-CXU6Y-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-CXU6Y-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX6YK'.                   CI0072
            10       S-CXU6Y-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-CXU6Y-CX6YK.                                     CI0072
            11       S-CXU6Y-C299.                                      CI0072
            12       S-CXU6Y-CTID.                                      CI0072
            13       S-CXU6Y-CTIDA    PICTURE  9(3).                    CI0072
            13       S-CXU6Y-CTIDN.                                     CI0072
            14       S-CXU6Y-CTIDNP   PICTURE  X(13).                   CI0072
            14       S-CXU6Y-CTIDND   PICTURE  9(11).                   CI0072
            11       S-CXU6Y-C199.                                      CI0072
            12       S-CXU6Y-CLID.                                      CI0072
            13       S-CXU6Y-CLIDO    PICTURE  9(3).                    CI0072
            13       S-CXU6Y-CLIDN.                                     CI0072
            14       S-CXU6Y-CLIDNP   PICTURE  X(12).                   CI0072
            14       S-CXU6Y-CLIDND   PICTURE  9(8).                    CI0072
            11       S-CXU6Y-CARTY    PICTURE  99.                      CI0072
            11       S-CXU6Y-NARRS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11       S-CXU6Y-CTID1    PICTURE  X(27).                   CI0072
            11       S-CXU6Y-CARTZ    PICTURE  99.                      CI0072
            11       S-CXU6Y-NAPDS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11       S-CXU6Y-NPISQ    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-PS13-SSA.                                     CI0072
            10         S1-PS13-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX13    '.                 CI0072
            10         S1-PS13-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-PS13-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-PSA13-SSA.                                       CI0072
            11      S1-PSA13-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX13    '.                 CI0072
            11      S1-PSA13-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-PSA13-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-PSA13-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CDEST'.                   CI0072
            11       S-PSA13-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-PSA13-CDEST    PICTURE  99.                      CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-PSB13-SSA.                                       CI0072
            12      S1-PSB13-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX13    '.                 CI0072
            12      S1-PSB13-CCOM   PICTURE X VALUE '*'.                CI0072
            12       S-PSB13-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            12      S1-PSB13-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CARTZ'.                   CI0072
            12       S-PSB13-OPER  PICTURE XX VALUE ' ='.               CI0072
            12       S-PSB13-CARTZ    PICTURE  99.                      CI0072
            12  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-PSC13-SSA.                                       CI0072
            12      S1-PSC13-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX13    '.                 CI0072
            12      S1-PSC13-CCOM   PICTURE X VALUE '*'.                CI0072
            12       S-PSC13-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            12      S1-PSC13-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(NAPDS'.                   CI0072
            12       S-PSC13-OPER  PICTURE XX VALUE ' ='.               CI0072
            12       S-PSC13-NAPDS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            12  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-PSU13-SSA.                                       CI0072
            11      S1-PSU13-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX13    '.                 CI0072
            11      S1-PSU13-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-PSU13-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-PSU13-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX13K'.                   CI0072
            11       S-PSU13-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-PSU13-CX13K.                                     CI0072
            12       S-PSU13-CARTZ    PICTURE  99.                      CI0072
            12       S-PSU13-NAPDS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-PS113-SSA.                                       CI0072
            11      S1-PS113-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX13    '.                 CI0072
            11      S1-PS113-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-PS113-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-PS113-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(XGCUSPY'.                 CI0072
            11       S-PS113-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-PS113-GCUSPY   PICTURE  X(12).                   CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-PS14-SSA.                                     CI0072
            10         S1-PS14-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX14    '.                 CI0072
            10         S1-PS14-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-PS14-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-PSU14-SSA.                                       CI0072
            10      S1-PSU14-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX14    '.                 CI0072
            10      S1-PSU14-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-PSU14-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-PSU14-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX14K'.                   CI0072
            10       S-PSU14-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-PSU14-CX14K.                                     CI0072
            11       S-PSU14-NPISQ    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-PS114-SSA.                                       CI0072
            11      S1-PS114-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX14    '.                 CI0072
            11      S1-PS114-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-PS114-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-PS114-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(XGCUSPZ'.                 CI0072
            11       S-PS114-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-PS114-GCUSPZ   PICTURE  X(12).                   CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-ST01-SSA.                                     CI0072
            10         S1-ST01-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CT01    '.                 CI0072
            10         S1-ST01-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-ST01-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-STU01-SSA.                                       CI0072
            10      S1-STU01-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CT01    '.                 CI0072
            10      S1-STU01-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-STU01-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-STU01-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CT01K'.                   CI0072
            10       S-STU01-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-STU01-CT01K.                                     CI0072
            11       S-STU01-C299.                                      CI0072
            12       S-STU01-CTID.                                      CI0072
            13       S-STU01-CTIDA    PICTURE  9(3).                    CI0072
            13       S-STU01-CTIDN.                                     CI0072
            14       S-STU01-CTIDNP   PICTURE  X(13).                   CI0072
            14       S-STU01-CTIDND   PICTURE  9(11).                   CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-WX01-SSA.                                     CI0072
            10         S1-WX01-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX01    '.                 CI0072
            10         S1-WX01-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-WX01-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-WXU01-SSA.                                       CI0072
            10      S1-WXU01-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX01    '.                 CI0072
            10      S1-WXU01-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-WXU01-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-WXU01-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX01K'.                   CI0072
            10       S-WXU01-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-WXU01-CX01K.                                     CI0072
            11       S-WXU01-C199.                                      CI0072
            12       S-WXU01-CLID.                                      CI0072
            13       S-WXU01-CLIDO    PICTURE  9(3).                    CI0072
            13       S-WXU01-CLIDN.                                     CI0072
            14       S-WXU01-CLIDNP   PICTURE  X(12).                   CI0072
            14       S-WXU01-CLIDND   PICTURE  9(8).                    CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-WX03-SSA.                                     CI0072
            10         S1-WX03-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX03    '.                 CI0072
            10         S1-WX03-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-WX03-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-WXA03-SSA.                                       CI0072
            12      S1-WXA03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            12      S1-WXA03-CCOM   PICTURE X VALUE '*'.                CI0072
            12       S-WXA03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            12      S1-WXA03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CARTY'.                   CI0072
            12       S-WXA03-OPER  PICTURE XX VALUE ' ='.               CI0072
            12       S-WXA03-CARTY    PICTURE  99.                      CI0072
            12  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXB03-SSA.                                       CI0072
            12      S1-WXB03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            12      S1-WXB03-CCOM   PICTURE X VALUE '*'.                CI0072
            12       S-WXB03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            12      S1-WXB03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(NARRS'.                   CI0072
            12       S-WXB03-OPER  PICTURE XX VALUE ' ='.               CI0072
            12       S-WXB03-NARRS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            12  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXC03-SSA.                                       CI0072
            11      S1-WXC03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            11      S1-WXC03-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-WXC03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-WXC03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CPMTG'.                   CI0072
            11       S-WXC03-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-WXC03-CPMTG    PICTURE  99.                      CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXD03-SSA.                                       CI0072
            11      S1-WXD03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            11      S1-WXD03-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-WXD03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-WXD03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(GRCRNG'.                  CI0072
            11       S-WXD03-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-WXD03-GRCRNG   PICTURE  9(3).                    CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXE03-SSA.                                       CI0072
            11      S1-WXE03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            11      S1-WXE03-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-WXE03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-WXE03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(DEXDT'.                   CI0072
            11       S-WXE03-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-WXE03-DEXDT    PICTURE  9(8).                    CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXF03-SSA.                                       CI0072
            11      S1-WXF03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            11      S1-WXF03-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-WXF03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-WXF03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CY50'.                    CI0072
            11       S-WXF03-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-WXF03-CY50.                                      CI0072
            12       S-WXF03-NARID    PICTURE  X(30).                   CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXG03-SSA.                                       CI0072
            11      S1-WXG03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            11      S1-WXG03-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-WXG03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-WXG03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(NBASQ'.                   CI0072
            11       S-WXG03-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-WXG03-NBASQ    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXH03-SSA.                                       CI0072
            12      S1-WXH03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            12      S1-WXH03-CCOM   PICTURE X VALUE '*'.                CI0072
            12       S-WXH03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            12      S1-WXH03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(NARID'.                   CI0072
            12       S-WXH03-OPER  PICTURE XX VALUE ' ='.               CI0072
            12       S-WXH03-NARID    PICTURE  X(30).                   CI0072
            12  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXU03-SSA.                                       CI0072
            11      S1-WXU03-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX03    '.                 CI0072
            11      S1-WXU03-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-WXU03-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-WXU03-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX03K'.                   CI0072
            11       S-WXU03-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-WXU03-CX03K.                                     CI0072
            12       S-WXU03-CARTY    PICTURE  99.                      CI0072
            12       S-WXU03-NARRS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-WX06-SSA.                                     CI0072
            10         S1-WX06-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX06    '.                 CI0072
            10         S1-WX06-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-WX06-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-WXU06-SSA.                                       CI0072
            10      S1-WXU06-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX06    '.                 CI0072
            10      S1-WXU06-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-WXU06-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-WXU06-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX06K'.                   CI0072
            10       S-WXU06-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-WXU06-CX06K.                                     CI0072
            11       S-WXU06-C299.                                      CI0072
            12       S-WXU06-CTID.                                      CI0072
            13       S-WXU06-CTIDA    PICTURE  9(3).                    CI0072
            13       S-WXU06-CTIDN.                                     CI0072
            14       S-WXU06-CTIDNP   PICTURE  X(13).                   CI0072
            14       S-WXU06-CTIDND   PICTURE  9(11).                   CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-WX09-SSA.                                     CI0072
            10         S1-WX09-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX09    '.                 CI0072
            10         S1-WX09-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-WX09-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-WXU09-SSA.                                       CI0072
            10      S1-WXU09-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX09    '.                 CI0072
            10      S1-WXU09-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-WXU09-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-WXU09-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX09K'.                   CI0072
            10       S-WXU09-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-WXU09-CX09K.                                     CI0072
            11       S-WXU09-NPAIS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-WX13-SSA.                                     CI0072
            10         S1-WX13-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX13    '.                 CI0072
            10         S1-WX13-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-WX13-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-WXA13-SSA.                                       CI0072
            11      S1-WXA13-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX13    '.                 CI0072
            11      S1-WXA13-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-WXA13-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-WXA13-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CDEST'.                   CI0072
            11       S-WXA13-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-WXA13-CDEST    PICTURE  99.                      CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXB13-SSA.                                       CI0072
            12      S1-WXB13-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX13    '.                 CI0072
            12      S1-WXB13-CCOM   PICTURE X VALUE '*'.                CI0072
            12       S-WXB13-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            12      S1-WXB13-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CARTZ'.                   CI0072
            12       S-WXB13-OPER  PICTURE XX VALUE ' ='.               CI0072
            12       S-WXB13-CARTZ    PICTURE  99.                      CI0072
            12  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXC13-SSA.                                       CI0072
            12      S1-WXC13-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX13    '.                 CI0072
            12      S1-WXC13-CCOM   PICTURE X VALUE '*'.                CI0072
            12       S-WXC13-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            12      S1-WXC13-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(NAPDS'.                   CI0072
            12       S-WXC13-OPER  PICTURE XX VALUE ' ='.               CI0072
            12       S-WXC13-NAPDS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            12  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXU13-SSA.                                       CI0072
            11      S1-WXU13-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX13    '.                 CI0072
            11      S1-WXU13-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-WXU13-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-WXU13-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX13K'.                   CI0072
            11       S-WXU13-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-WXU13-CX13K.                                     CI0072
            12       S-WXU13-CARTZ    PICTURE  99.                      CI0072
            12       S-WXU13-NAPDS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WX113-SSA.                                       CI0072
            11      S1-WX113-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX13    '.                 CI0072
            11      S1-WX113-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-WX113-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-WX113-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(XGCUSPY'.                 CI0072
            11       S-WX113-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-WX113-GCUSPY   PICTURE  X(12).                   CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-WX14-SSA.                                     CI0072
            10         S1-WX14-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX14    '.                 CI0072
            10         S1-WX14-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-WX14-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-WXU14-SSA.                                       CI0072
            10      S1-WXU14-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX14    '.                 CI0072
            10      S1-WXU14-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-WXU14-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-WXU14-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX14K'.                   CI0072
            10       S-WXU14-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-WXU14-CX14K.                                     CI0072
            11       S-WXU14-NPISQ    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WX114-SSA.                                       CI0072
            11      S1-WX114-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX14    '.                 CI0072
            11      S1-WX114-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-WX114-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-WX114-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(XGCUSPZ'.                 CI0072
            11       S-WX114-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-WX114-GCUSPZ   PICTURE  X(12).                   CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-WX18-SSA.                                     CI0072
            10         S1-WX18-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX18    '.                 CI0072
            10         S1-WX18-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-WX18-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-WXA18-SSA.                                       CI0072
            10      S1-WXA18-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX18    '.                 CI0072
            10      S1-WXA18-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-WXA18-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-WXA18-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CSTPRE'.                  CI0072
            10       S-WXA18-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-WXA18-CSTPRE   PICTURE  99.                      CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXB18-SSA.                                       CI0072
            10      S1-WXB18-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX18    '.                 CI0072
            10      S1-WXB18-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-WXB18-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-WXB18-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CSPCR'.                   CI0072
            10       S-WXB18-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-WXB18-CSPCR    PICTURE  99.                      CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXU18-SSA.                                       CI0072
            10      S1-WXU18-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX18    '.                 CI0072
            10      S1-WXU18-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-WXU18-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-WXU18-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX18K'.                   CI0072
            10       S-WXU18-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-WXU18-CX18K.                                     CI0072
            11       S-WXU18-NBASQ    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-WX2Y-SSA.                                     CI0072
            10         S1-WX2Y-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX2Y    '.                 CI0072
            10         S1-WX2Y-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-WX2Y-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-WXA2Y-SSA.                                       CI0072
            11      S1-WXA2Y-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX2Y    '.                 CI0072
            11      S1-WXA2Y-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-WXA2Y-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-WXA2Y-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CARTY'.                   CI0072
            11       S-WXA2Y-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-WXA2Y-CARTY    PICTURE  99.                      CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXB2Y-SSA.                                       CI0072
            11      S1-WXB2Y-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX2Y    '.                 CI0072
            11      S1-WXB2Y-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-WXB2Y-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-WXB2Y-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(C299'.                    CI0072
            11       S-WXB2Y-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-WXB2Y-C299.                                      CI0072
            12       S-WXB2Y-CTID.                                      CI0072
            13       S-WXB2Y-CTIDA    PICTURE  9(3).                    CI0072
            13       S-WXB2Y-CTIDN.                                     CI0072
            14       S-WXB2Y-CTIDNP   PICTURE  X(13).                   CI0072
            14       S-WXB2Y-CTIDND   PICTURE  9(11).                   CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXU2Y-SSA.                                       CI0072
            10      S1-WXU2Y-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX2Y    '.                 CI0072
            10      S1-WXU2Y-CCOM   PICTURE X VALUE '*'.                CI0072
            10       S-WXU2Y-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            10      S1-WXU2Y-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX2YK'.                   CI0072
            10       S-WXU2Y-OPER  PICTURE XX VALUE ' ='.               CI0072
            10       S-WXU2Y-CX2YK.                                     CI0072
            11       S-WXU2Y-C299.                                      CI0072
            12       S-WXU2Y-CTID.                                      CI0072
            13       S-WXU2Y-CTIDA    PICTURE  9(3).                    CI0072
            13       S-WXU2Y-CTIDN.                                     CI0072
            14       S-WXU2Y-CTIDNP   PICTURE  X(13).                   CI0072
            14       S-WXU2Y-CTIDND   PICTURE  9(11).                   CI0072
            11       S-WXU2Y-C199.                                      CI0072
            12       S-WXU2Y-CLID.                                      CI0072
            13       S-WXU2Y-CLIDO    PICTURE  9(3).                    CI0072
            13       S-WXU2Y-CLIDN.                                     CI0072
            14       S-WXU2Y-CLIDNP   PICTURE  X(12).                   CI0072
            14       S-WXU2Y-CLIDND   PICTURE  9(8).                    CI0072
            11       S-WXU2Y-CARTY    PICTURE  99.                      CI0072
            11       S-WXU2Y-NARRS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01               S-WX21-SSA.                                     CI0072
            10         S1-WX21-SEGNAM PICTURE X(8)                      CI0072
                                      VALUE 'CX21    '.                 CI0072
            10         S1-WX21-CCOM   PICTURE X VALUE '*'.              CI0072
            10          S-WX21-CCOD   PICTURE X(5)                      CI0072
                                      VALUE '-----'.                    CI0072
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0072
       01            S-WXA21-SSA.                                       CI0072
            11      S1-WXA21-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX21    '.                 CI0072
            11      S1-WXA21-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-WXA21-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-WXA21-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(GECSQ1'.                  CI0072
            11       S-WXA21-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-WXA21-GECSQ1   PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01            S-WXU21-SSA.                                       CI0072
            11      S1-WXU21-SEGNAM PICTURE X(8)                        CI0072
                                      VALUE 'CX21    '.                 CI0072
            11      S1-WXU21-CCOM   PICTURE X VALUE '*'.                CI0072
            11       S-WXU21-CCOD   PICTURE X(5)                        CI0072
                                      VALUE '-----'.                    CI0072
            11      S1-WXU21-FLDNAM PICTURE X(9)                        CI0072
                                      VALUE '(CX21K'.                   CI0072
            11       S-WXU21-OPER  PICTURE XX VALUE ' ='.               CI0072
            11       S-WXU21-CX21K.                                     CI0072
            12       S-WXU21-CDEL1    PICTURE  9(3).                    CI0072
            12       S-WXU21-NDELS    PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11  FILLER   PICTURE X    VALUE ')'.                        CI0072
       01   ZONES-UTILISATEUR PICTURE X.                                CI0072
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
      ** PCB POINTER FOR ARAY                                           ADU015
            05 PCB-ARAY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CA1P                                           ADU015
            05 PCB-CA1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CBTP                                           ADU015
            05 PCB-CBTP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CH1P                                           ADU015
            05 PCB-CH1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CCRP                                           ADU015
            05 PCB-CCRP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CPRP                                           ADU015
            05 PCB-CPRP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR TR1P                                           ADU015
            05 PCB-TR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AREY                                           ADU015
            05 PCB-AREY-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0072
          05              PA00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00106).                  CI0072
       01                 PA06  REDEFINES      PA00.                    CI0072
            10            PA06-XDBPCB.                                  CI0072
            11            PA06-XDBDNM PICTURE  X(08).                   CI0072
            11            PA06-XSEGLV PICTURE  X(02).                   CI0072
            11            PA06-XRC    PICTURE  X(02).                   CI0072
            11            PA06-XPROPT PICTURE  X(04).                   CI0072
            11            PA06-FILLER PICTURE  S9(5)                    CI0072
                          BINARY.                                       CI0072
            11            PA06-XSEGNM PICTURE  X(08).                   CI0072
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0072
                          BINARY.                                       CI0072
            11            PA06-XSEGNB PICTURE  9(05)                    CI0072
                          BINARY.                                       CI0072
            11            PA06-XCOKEY PICTURE  X(70).                   CI0072
      *** PCB MASK FOR ARAY                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0072
          05              PB00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00106).                  CI0072
       01                 PB06  REDEFINES      PB00.                    CI0072
            10            PB06-XDBPCB.                                  CI0072
            11            PB06-XDBDNM PICTURE  X(08).                   CI0072
            11            PB06-XSEGLV PICTURE  X(02).                   CI0072
            11            PB06-XRC    PICTURE  X(02).                   CI0072
            11            PB06-XPROPT PICTURE  X(04).                   CI0072
            11            PB06-FILLER PICTURE  S9(5)                    CI0072
                          BINARY.                                       CI0072
            11            PB06-XSEGNM PICTURE  X(08).                   CI0072
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0072
                          BINARY.                                       CI0072
            11            PB06-XSEGNB PICTURE  9(05)                    CI0072
                          BINARY.                                       CI0072
            11            PB06-XCOKEY PICTURE  X(70).                   CI0072
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=PD DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PD00.                                         CI0072
          05              PD00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00106).                  CI0072
       01                 PD06  REDEFINES      PD00.                    CI0072
            10            PD06-XDBPCB.                                  CI0072
            11            PD06-XDBDNM PICTURE  X(08).                   CI0072
            11            PD06-XSEGLV PICTURE  X(02).                   CI0072
            11            PD06-XRC    PICTURE  X(02).                   CI0072
            11            PD06-XPROPT PICTURE  X(04).                   CI0072
            11            PD06-FILLER PICTURE  S9(5)                    CI0072
                          BINARY.                                       CI0072
            11            PD06-XSEGNM PICTURE  X(08).                   CI0072
            11            PD06-XKEYLN PICTURE  S9(05)                   CI0072
                          BINARY.                                       CI0072
            11            PD06-XSEGNB PICTURE  9(05)                    CI0072
                          BINARY.                                       CI0072
            11            PD06-XCOKEY PICTURE  X(70).                   CI0072
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PE DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PE00.                                         CI0072
          05              PE00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00106).                  CI0072
       01                 PE06  REDEFINES      PE00.                    CI0072
            10            PE06-XDBPCB.                                  CI0072
            11            PE06-XDBDNM PICTURE  X(08).                   CI0072
            11            PE06-XSEGLV PICTURE  X(02).                   CI0072
            11            PE06-XRC    PICTURE  X(02).                   CI0072
            11            PE06-XPROPT PICTURE  X(04).                   CI0072
            11            PE06-FILLER PICTURE  S9(5)                    CI0072
                          BINARY.                                       CI0072
            11            PE06-XSEGNM PICTURE  X(08).                   CI0072
            11            PE06-XKEYLN PICTURE  S9(05)                   CI0072
                          BINARY.                                       CI0072
            11            PE06-XSEGNB PICTURE  9(05)                    CI0072
                          BINARY.                                       CI0072
            11            PE06-XCOKEY PICTURE  X(70).                   CI0072
      *** PCB MASK FOR CA1P                                             ADU015
      *!WF DSP=PF DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PF00.                                         CI0072
          05              PF00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00106).                  CI0072
       01                 PF06  REDEFINES      PF00.                    CI0072
            10            PF06-XDBPCB.                                  CI0072
            11            PF06-XDBDNM PICTURE  X(08).                   CI0072
            11            PF06-XSEGLV PICTURE  X(02).                   CI0072
            11            PF06-XRC    PICTURE  X(02).                   CI0072
            11            PF06-XPROPT PICTURE  X(04).                   CI0072
            11            PF06-FILLER PICTURE  S9(5)                    CI0072
                          BINARY.                                       CI0072
            11            PF06-XSEGNM PICTURE  X(08).                   CI0072
            11            PF06-XKEYLN PICTURE  S9(05)                   CI0072
                          BINARY.                                       CI0072
            11            PF06-XSEGNB PICTURE  9(05)                    CI0072
                          BINARY.                                       CI0072
            11            PF06-XCOKEY PICTURE  X(70).                   CI0072
      *** PCB MASK FOR CBTP                                             ADU015
      *!WF DSP=PG DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PG00.                                         CI0072
          05              PG00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00106).                  CI0072
       01                 PG06  REDEFINES      PG00.                    CI0072
            10            PG06-XDBPCB.                                  CI0072
            11            PG06-XDBDNM PICTURE  X(08).                   CI0072
            11            PG06-XSEGLV PICTURE  X(02).                   CI0072
            11            PG06-XRC    PICTURE  X(02).                   CI0072
            11            PG06-XPROPT PICTURE  X(04).                   CI0072
            11            PG06-FILLER PICTURE  S9(5)                    CI0072
                          BINARY.                                       CI0072
            11            PG06-XSEGNM PICTURE  X(08).                   CI0072
            11            PG06-XKEYLN PICTURE  S9(05)                   CI0072
                          BINARY.                                       CI0072
            11            PG06-XSEGNB PICTURE  9(05)                    CI0072
                          BINARY.                                       CI0072
            11            PG06-XCOKEY PICTURE  X(70).                   CI0072
      *** PCB MASK FOR CH1P                                             ADU015
      *!WF DSP=PH DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PH00.                                         CI0072
          05              PH00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00106).                  CI0072
       01                 PH06  REDEFINES      PH00.                    CI0072
            10            PH06-XDBPCB.                                  CI0072
            11            PH06-XDBDNM PICTURE  X(08).                   CI0072
            11            PH06-XSEGLV PICTURE  X(02).                   CI0072
            11            PH06-XRC    PICTURE  X(02).                   CI0072
            11            PH06-XPROPT PICTURE  X(04).                   CI0072
            11            PH06-FILLER PICTURE  S9(5)                    CI0072
                          BINARY.                                       CI0072
            11            PH06-XSEGNM PICTURE  X(08).                   CI0072
            11            PH06-XKEYLN PICTURE  S9(05)                   CI0072
                          BINARY.                                       CI0072
            11            PH06-XSEGNB PICTURE  9(05)                    CI0072
                          BINARY.                                       CI0072
            11            PH06-XCOKEY PICTURE  X(70).                   CI0072
      *** PCB MASK FOR CCRP                                             ADU015
      *!WF DSP=PI DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PI00.                                         CI0072
          05              PI00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00106).                  CI0072
       01                 PI06  REDEFINES      PI00.                    CI0072
            10            PI06-XDBPCB.                                  CI0072
            11            PI06-XDBDNM PICTURE  X(08).                   CI0072
            11            PI06-XSEGLV PICTURE  X(02).                   CI0072
            11            PI06-XRC    PICTURE  X(02).                   CI0072
            11            PI06-XPROPT PICTURE  X(04).                   CI0072
            11            PI06-FILLER PICTURE  S9(5)                    CI0072
                          BINARY.                                       CI0072
            11            PI06-XSEGNM PICTURE  X(08).                   CI0072
            11            PI06-XKEYLN PICTURE  S9(05)                   CI0072
                          BINARY.                                       CI0072
            11            PI06-XSEGNB PICTURE  9(05)                    CI0072
                          BINARY.                                       CI0072
            11            PI06-XCOKEY PICTURE  X(70).                   CI0072
      *** PCB MASK FOR CPRP                                             ADU015
      *!WF DSP=PK DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PK00.                                         CI0072
          05              PK00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00106).                  CI0072
       01                 PK06  REDEFINES      PK00.                    CI0072
            10            PK06-XDBPCB.                                  CI0072
            11            PK06-XDBDNM PICTURE  X(08).                   CI0072
            11            PK06-XSEGLV PICTURE  X(02).                   CI0072
            11            PK06-XRC    PICTURE  X(02).                   CI0072
            11            PK06-XPROPT PICTURE  X(04).                   CI0072
            11            PK06-FILLER PICTURE  S9(5)                    CI0072
                          BINARY.                                       CI0072
            11            PK06-XSEGNM PICTURE  X(08).                   CI0072
            11            PK06-XKEYLN PICTURE  S9(05)                   CI0072
                          BINARY.                                       CI0072
            11            PK06-XSEGNB PICTURE  9(05)                    CI0072
                          BINARY.                                       CI0072
            11            PK06-XCOKEY PICTURE  X(70).                   CI0072
      *** PCB MASK FOR TR1P                                             ADU015
      *!WF DSP=PM DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PM00.                                         CI0072
          05              PM00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00106).                  CI0072
       01                 PM06  REDEFINES      PM00.                    CI0072
            10            PM06-XDBPCB.                                  CI0072
            11            PM06-XDBDNM PICTURE  X(08).                   CI0072
            11            PM06-XSEGLV PICTURE  X(02).                   CI0072
            11            PM06-XRC    PICTURE  X(02).                   CI0072
            11            PM06-XPROPT PICTURE  X(04).                   CI0072
            11            PM06-FILLER PICTURE  S9(5)                    CI0072
                          BINARY.                                       CI0072
            11            PM06-XSEGNM PICTURE  X(08).                   CI0072
            11            PM06-XKEYLN PICTURE  S9(05)                   CI0072
                          BINARY.                                       CI0072
            11            PM06-XSEGNB PICTURE  9(05)                    CI0072
                          BINARY.                                       CI0072
            11            PM06-XCOKEY PICTURE  X(70).                   CI0072
      *** PCB MASK FOR AREY                                             ADU015
      *!WF DSP=PN DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PN00.                                         CI0072
          05              PN00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00106).                  CI0072
       01                 PN06  REDEFINES      PN00.                    CI0072
            10            PN06-XDBPCB.                                  CI0072
            11            PN06-XDBDNM PICTURE  X(08).                   CI0072
            11            PN06-XSEGLV PICTURE  X(02).                   CI0072
            11            PN06-XRC    PICTURE  X(02).                   CI0072
            11            PN06-XPROPT PICTURE  X(04).                   CI0072
            11            PN06-FILLER PICTURE  S9(5)                    CI0072
                          BINARY.                                       CI0072
            11            PN06-XSEGNM PICTURE  X(08).                   CI0072
            11            PN06-XKEYLN PICTURE  S9(05)                   CI0072
                          BINARY.                                       CI0072
            11            PN06-XSEGNB PICTURE  9(05)                    CI0072
                          BINARY.                                       CI0072
            11            PN06-XCOKEY PICTURE  X(70).                   CI0072
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED FOR CI0072    *
      ******************************************************************
      *
      *!WF DSP=PJ DSL=PJ SEL=14 FOR=I LEV=1 PLT=80
       01                 PJ00.                                         CI0072
          05              PJ00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00206).                  CI0072
       01                 PJ14  REDEFINES      PJ00.                    CI0072
            10            PJ14-PJ13.                                    CI0072
            11            PJ14-MAPPN  PICTURE  X(10).                   CI0072
            11            PJ14-CHCR   PICTURE  99.                      CI0072
            11            PJ14-CACTS  PICTURE  X.                       CI0072
            11            PJ14-CACTA  PICTURE  X(1).                    CI0072
            11            PJ14-CLIDP  PICTURE  X(23).                   CI0072
            11            PJ14-CTID01 PICTURE  X(27).                   CI0072
            11            PJ14-CARTZF PICTURE  99.                      CI0072
            11            PJ14-CLID   PICTURE  X(23).                   CI0072
            11            PJ14-CARTYK PICTURE  99.                      CI0072
            11            PJ14-NARRSK PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PJ14-NAPDS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PJ14-NPISQ1 PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PJ14-GECSQ  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PJ14-FILLER PICTURE  X(50).                   CI0072
            10            PJ14-CUPIQ  PICTURE  X.                       CI0072
            10            PJ14-PROGR  PICTURE  X(06).                   CI0072
            10            PJ14-FILLER PICTURE  X(50).                   CI0072
      *
      *
       01                 PS13.                                         CI0072
            10            PS13-GELL   PICTURE  9(4)                     CI0072
                          BINARY.                                       CI0072
            10            PS13-CY20.                                    CI0072
            11            PS13-CX13K.                                   CI0072
            12            PS13-CARTZ  PICTURE  99.                      CI0072
            12            PS13-NAPDS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PS13-GESTD  PICTURE  9(8).                    CI0072
            11            PS13-GEEND  PICTURE  9(8).                    CI0072
            11            PS13-DASUQ  PICTURE  9(8).                    CI0072
            11            PS13-CDEST  PICTURE  99.                      CI0072
            11            PS13-IIARR  PICTURE  X.                       CI0072
            11            PS13-DLAUP  PICTURE  9(8).                    CI0072
            11            PS13-GEOPD2 PICTURE  X(8).                    CI0072
            11            PS13-GEAUN  PICTURE  9(5).                    CI0072
            11            PS13-DPCHD  PICTURE  9(8).                    CI0072
            11            PS13-PPOT1  PICTURE  S9(3)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PS13-ACOT1  PICTURE  S9(9)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PS13-QPST1  PICTURE  S9(7)V999                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PS13-FILLER PICTURE  X(03).                   CI0072
            10            PS13-CY96.                                    CI0072
            11            PS13-FILLER PICTURE  X(50).                   CI0072
            10            PS13-CY21                                     CI0072
                          REDEFINES            PS13-CY96.               CI0072
            11            PS13-DNPMT  PICTURE  9(8).                    CI0072
            11            PS13-CPMTF  PICTURE  99.                      CI0072
            11            PS13-ADBRQ  PICTURE  S9(11)V99                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PS13-QSHOWQ PICTURE  S9(9)V999                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PS13-PACT1  PICTURE  S999V999                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PS13-DOPDA  PICTURE  99.                      CI0072
            11            PS13-DNEXE  PICTURE  9(8).                    CI0072
            11            PS13-CIRMO  PICTURE  X(12).                   CI0072
            10            PS13-CY98.                                    CI0072
            11            PS13-FILLER PICTURE  X(120).                  CI0072
            10            PS13-CY25                                     CI0072
                          REDEFINES            PS13-CY98.               CI0072
            11            PS13-COPTC  PICTURE  9(1).                    CI0072
            11            PS13-ILPOI  PICTURE  X(1).                    CI0072
            11            PS13-CATOC  PICTURE  X(1).                    CI0072
            11            PS13-CEOIA  PICTURE  S9(7)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PS13-ACOAR  PICTURE  S9(9)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PS13-CEOTR  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PS13-DSTMO  PICTURE  99.                      CI0072
            10            PS13-CY27                                     CI0072
                          REDEFINES            PS13-CY98.               CI0072
            11            PS13-QMTH1  PICTURE  9(3).                    CI0072
            11            PS13-IDRMD  PICTURE  X.                       CI0072
            10            PS13-CY28                                     CI0072
                          REDEFINES            PS13-CY98.               CI0072
            11            PS13-AALLBL PICTURE  S9(8)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PS13-PSURR  PICTURE  S9(3)V999                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PS13-DFPMT  PICTURE  9(8).                    CI0072
            11            PS13-QMTHLA PICTURE  9(3).                    CI0072
            11            PS13-PWHLDS PICTURE  S999V9(5)                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PS13-ISWHO  PICTURE  X(1).                    CI0072
            10            PS13-CY29                                     CI0072
                          REDEFINES            PS13-CY98.               CI0072
            11            PS13-IINDI1 PICTURE  X(1).                    CI0072
            11            PS13-IINDI2 PICTURE  X(1).                    CI0072
            11            PS13-IINDI3 PICTURE  X(1).                    CI0072
            11            PS13-PWHLD5 PICTURE  S999V99                  CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PS13-CCSMQ  PICTURE  X.                       CI0072
            11            PS13-CPLEC  PICTURE  XX.                      CI0072
            11            PS13-IPTRDA PICTURE  X(01).                   CI0072
            11            PS13-GCUSPY PICTURE  X(12).                   CI0072
            11            PS13-ALOIDA PICTURE  S9(11)V99                CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            PS13-DELOI  PICTURE  9(8).                    CI0072
            11            PS13-CLGND  PICTURE  X.                       CI0072
            11            PS13-CORTYA PICTURE  X(3).                    CI0072
            11            PS13-CPH3U  PICTURE  X.                       CI0072
            11            PS13-CNAVR  PICTURE  X(1).                    CI0072
            11            PS13-NEXEC  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
       01                 PS14.                                         CI0072
            10            PS14-GELL   PICTURE  9(4)                     CI0072
                          BINARY.                                       CI0072
            10            PS14-CX14K.                                   CI0072
            11            PS14-NPISQ  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            PS14-ACOTD  PICTURE  S9(9)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            PS14-PPOTD  PICTURE  S9(3)V99                 CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            PS14-QPSTD  PICTURE  S9(7)V999                CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            PS14-CPITC  PICTURE  99.                      CI0072
            10            PS14-FILLER PICTURE  X(04).                   CI0072
            10            PS14-CY97.                                    CI0072
            11            PS14-FILLER PICTURE  X(32).                   CI0072
            10            PS14-CY30                                     CI0072
                          REDEFINES            PS14-CY97.               CI0072
            11            PS14-IOWNC  PICTURE  X.                       CI0072
            11            PS14-CTYPE  PICTURE  X.                       CI0072
            11            PS14-C299.                                    CI0072
            12            PS14-CTID.                                    CI0072
            13            PS14-CTIDA  PICTURE  9(3).                    CI0072
            13            PS14-CTIDN.                                   CI0072
            14            PS14-CTIDNP PICTURE  X(13).                   CI0072
            14            PS14-CTIDND PICTURE  9(11).                   CI0072
            11            PS14-CPMTC  PICTURE  99.                      CI0072
            11            PS14-IACSD  PICTURE  X.                       CI0072
            10            PS14-CY31                                     CI0072
                          REDEFINES            PS14-CY97.               CI0072
            11            PS14-FILLER PICTURE  X(2).                    CI0072
            11            PS14-IDELI  PICTURE  X.                       CI0072
            11            PS14-CDEL1  PICTURE  9(3).                    CI0072
            11            PS14-NDELS  PICTURE  S9(3)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            PS14-CY32                                     CI0072
                          REDEFINES            PS14-CY97.               CI0072
            11            PS14-GCUSPZ PICTURE  X(12).                   CI0072
      *
      ******************************************************************
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *
      ******************************************************************
      *
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0072
          05              DE00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00653).                  CI0072
       01                 DE10  REDEFINES      DE00.                    CI0072
            10            DE10-DU11.                                    CI0072
            11            DE10-XFONC  PICTURE  X(4).                    CI0072
            11            DE10-MPSBN  PICTURE  X(8).                    CI0072
            11            DE10-XDBDNM PICTURE  X(08).                   CI0072
            11            DE10-XSEGNM PICTURE  X(08).                   CI0072
            11            DE10-XRC    PICTURE  X(02).                   CI0072
            11            DE10-MSEG   PICTURE  X(08).                   CI0072
            11            DE10-XCOKEY PICTURE  X(70).                   CI0072
            11            DE10-CUIBR  PICTURE  X(01).                   CI0072
            11            DE10-CUIBA  PICTURE  X(01).                   CI0072
            11            DE10-IPBIK  PICTURE  X(1).                    CI0072
            10            DE10-DU03.                                    CI0072
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            DE10-CMSSF  PICTURE  XX.                      CI0072
            11            DE10-DU09.                                    CI0072
            12            DE10-CMESA  PICTURE  S9(9)                    CI0072
                          BINARY.                                       CI0072
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0072
                          BINARY.                                       CI0072
            12            DE10-CMESB  PICTURE  S9(9)                    CI0072
                          BINARY.                                       CI0072
            12            DE10-CMSST  PICTURE  S9(9)                    CI0072
                          BINARY.                                       CI0072
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0072
                          BINARY.                                       CI0072
            12            DE10-QELLAA PICTURE  S9(9)                    CI0072
                          BINARY.                                       CI0072
            12            DE10-TMESS4 PICTURE  X(512).                  CI0072
      *
      *
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0072
          05              MS00-SUITE.                                   CI0072
            15       FILLER         PICTURE  X(00542).                  CI0072
       01                 MS03  REDEFINES      MS00.                    CI0072
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            10            MS03-CMSSF  PICTURE  XX.                      CI0072
            10            MS03-DU09.                                    CI0072
            11            MS03-CMESA  PICTURE  S9(9)                    CI0072
                          BINARY.                                       CI0072
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0072
                          BINARY.                                       CI0072
            11            MS03-CMESB  PICTURE  S9(9)                    CI0072
                          BINARY.                                       CI0072
            11            MS03-CMSST  PICTURE  S9(9)                    CI0072
                          BINARY.                                       CI0072
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0072
                          BINARY.                                       CI0072
            11            MS03-QELLAA PICTURE  S9(9)                    CI0072
                          BINARY.                                       CI0072
            11            MS03-TMESS4 PICTURE  X(512).                  CI0072
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0072
            10            MX11-QMSGS  PICTURE  9(03).                   CI0072
            10            MX11-PJ09                                     CI0072
                          OCCURS       025     TIMES.                   CI0072
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0072
                          COMPUTATIONAL-3.                              CI0072
            11            MX11-CMESB  PICTURE  S9(9)                    CI0072
                          BINARY.                                       CI0072
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PJ14
                                PS13
                                PS14
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0072
      *               *                                   *             CI0072
      *               *INITIALISATIONS                    *             CI0072
      *               *                                   *             CI0072
      *               *************************************.            CI0072
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
      *N02DC.    NOTE *---> SET PCB ADDRESSES             *.
       F02DC.                                                           lv10
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR ARAY                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-ARAY-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF PD06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PE06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CA1P                                             DOT
           SET ADDRESS OF PF06 TO                                       ADU015
                PCB-CA1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CBTP                                             DOT
           SET ADDRESS OF PG06 TO                                       ADU015
                PCB-CBTP-PTR1.                                          ADU015
      *SET ADDRESS FOR CH1P                                             DOT
           SET ADDRESS OF PH06 TO                                       ADU015
                PCB-CH1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CCRP                                             DOT
           SET ADDRESS OF PI06 TO                                       ADU015
                PCB-CCRP-PTR1.                                          ADU015
      *SET ADDRESS FOR CPRP                                             DOT
           SET ADDRESS OF PK06 TO                                       ADU015
                PCB-CPRP-PTR1.                                          ADU015
      *SET ADDRESS FOR TR1P                                             DOT
           SET ADDRESS OF PM06 TO                                       ADU015
                PCB-TR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR AREY                                             DOT
           SET ADDRESS OF PN06 TO                                       ADU015
                PCB-AREY-PTR1.                                          ADU015
       F02DC-FN. EXIT.
      *N02FA.    NOTE *SEGMENT RE-INITIALIZATION          *.
       F02FA.                                                           lv10
           INITIALIZE  MS03.
       F02FA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0072
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0072
      *               *                                   *             CI0072
      *               *FIN DE TRAITEMENT                  *             CI0072
      *               *                                   *             CI0072
      *               *************************************.            CI0072
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0072
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
           MOVE        'N' TO WS-CI0135-CALL-MADE-IND.
      *N30BA.    NOTE *IF FUNCTION IS ADD, CHG, STATUS    *.
       F30BA.    IF    PJ14-CACTS = 'A'                                 lv10
                 OR    PJ14-CACTS = 'C'
                 OR    PJ14-CACTS = 'S'
                 NEXT SENTENCE ELSE GO TO     F30BA-FN.
       F30BA-900. GO TO F30BM-FN.
       F30BA-FN. EXIT.
      *N30BM.    NOTE *ELSE... ERROR & EXIT               *.
       F30BM.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012595 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30BM-FN. EXIT.
      *N30EA.    NOTE *IF ACTION IS ADD OR CHANGE         *.
       F30EA.    IF    PJ14-CACTA = 'A'                                 lv10
                 OR    PJ14-CACTA = 'C'
                 OR    PJ14-CACTA = 'R'
                 OR    PJ14-CACTA = 'I'
                 NEXT SENTENCE ELSE GO TO     F30EA-FN.
      *OR REACTIVATE OR INACTIVATE
      *-- CONTINUE
       F30EA-900. GO TO F30EF-FN.
       F30EA-FN. EXIT.
      *N30EF.    NOTE *ELSE... ERROR & EXIT               *.
       F30EF.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012382 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30EF-FN. EXIT.
      *N30EM.    NOTE *IF VALID PARM COMBO...  CONTINUE   *.
       F30EM.    IF    ((PJ14-CACTS = 'A'                               lv10
                 AND   PJ14-CACTA = 'A')
                 OR    (PJ14-CACTS = 'C'
                 AND   PJ14-CACTA = 'C')
                 OR    (PJ14-CACTS = 'S'
                 AND   (PJ14-CACTA = 'R'
                 OR    PJ14-CACTA = 'I')))
                 NEXT SENTENCE ELSE GO TO     F30EM-FN.
      *ADD - ADD
      *CHG - CHG
      *STATUS  CHG REQUESTED
      *        - REACTIVATE
      *        - INACTIVATE
      *CONTINUE NEXT LINE
       F30EM-900. GO TO F30EP-FN.
       F30EM-FN. EXIT.
      *N30EP.    NOTE *ELSE...............    ERROR OUT   *.
       F30EP.                                                           lv10
      *INVALID PARAMETER COMBINTATION
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012613 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30EP-FN. EXIT.
      *N30FA.    NOTE *ENSURE HOW IS WRITTEN OR TELE      *.
       F30FA.    IF    PJ14-CHCR = 02                                   lv10
                 OR    PJ14-CHCR = 03
                 NEXT SENTENCE ELSE GO TO     F30FA-FN.
       F30FA-900. GO TO F30FM-FN.
       F30FA-FN. EXIT.
      *N30FM.    NOTE *ELSE... ERROR & EXIT               *.
       F30FM.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012053 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30FM-FN. EXIT.
      *N30GA.    NOTE *NUMERIC CHECK ON CX01 CLIENT ID    *.
       F30GA.    IF    PJ14-CLID NUMERIC                                lv10
                 NEXT SENTENCE ELSE GO TO     F30GA-FN.
       F30GA-900. GO TO F30GG-FN.
       F30GA-FN. EXIT.
      *N30GG.    NOTE *ELSE... ERROR & EXIT               *.
       F30GG.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012002 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30GG-FN. EXIT.
      *N30GM.    NOTE *NUM CHECK ON REQUESTING CLIENTID   *.
       F30GM.    IF    PJ14-CLIDP NUMERIC                               lv10
                 NEXT SENTENCE ELSE GO TO     F30GM-FN.
       F30GM-900. GO TO F30GS-FN.
       F30GM-FN. EXIT.
      *N30GS.    NOTE *ELSE... ERROR & EXIT               *.
       F30GS.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012002 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30GS-FN. EXIT.
      *N30HA.    NOTE *ENSURE CONTRACT IS NUMERIC         *.
       F30HA.    IF    PJ14-CTID01 NUMERIC                              lv10
                 NEXT SENTENCE ELSE GO TO     F30HA-FN.
       F30HA-900. GO TO F30HM-FN.
       F30HA-FN. EXIT.
      *N30HM.    NOTE *ELSE... ERROR & EXIT               *.
       F30HM.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012004 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30HM-FN. EXIT.
      *N30IA.    NOTE *IF ARR TYPE DETAIL CODE NUMERIC    *.
       F30IA.    IF    PJ14-CARTZF NUMERIC                              lv10
                 NEXT SENTENCE ELSE GO TO     F30IA-FN.
       F30IA-900. GO TO F30IG-FN.
       F30IA-FN. EXIT.
      *N30IG.    NOTE *ELSE... ERROR & EXIT               *.
       F30IG.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012054 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30IG-FN. EXIT.
      *N30IM.    NOTE *IF ARR TYPE DETAIL CODE VALID      *.
       F30IM.    IF    PJ14-CARTZF = 01                                 lv10
                 OR    PJ14-CARTZF = 02
                 OR    PJ14-CARTZF = 05
                 OR    PJ14-CARTZF = 06
                 NEXT SENTENCE ELSE GO TO     F30IM-FN.
      *01= FUNDS SP; 02 = CERTS PARTIAL
      *05 = FUNDS DIVI; 06 = CERTS INT
       F30IM-900. GO TO F30IS-FN.
       F30IM-FN. EXIT.
      *N30IS.    NOTE *ELSE... ERROR & EXIT               *.
       F30IS.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012054 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30IS-FN. EXIT.
      *N30KA.    NOTE *IF ARR SEQUENCE NUMBER NUMERIC     *.
       F30KA.    IF    PJ14-NARRSK NUMERIC                              lv10
                 NEXT SENTENCE ELSE GO TO     F30KA-FN.
       F30KA-900. GO TO F30KG-FN.
       F30KA-FN. EXIT.
      *N30KG.    NOTE *ELSE... ERROR & EXIT               *.
       F30KG.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012034 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30KG-FN. EXIT.
      *N30KM.    NOTE *IF ARR ACCT PYMT SEQ NBR NUMERIC   *.
       F30KM.    IF    PJ14-NAPDS NUMERIC                               lv10
                 NEXT SENTENCE ELSE GO TO     F30KM-FN.
       F30KM-900. GO TO F30KS-FN.
       F30KM-FN. EXIT.
      *N30KS.    NOTE *ELSE... ERROR & EXIT               *.
       F30KS.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012036 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30KS-FN. EXIT.
      *N30MA.    NOTE *IF DESTINATION SEQUENCE NUMBER     *.
       F30MA.    IF    PJ14-NPISQ1 NUMERIC                              lv10
                 NEXT SENTENCE ELSE GO TO     F30MA-FN.
       F30MA-900. GO TO F30MG-FN.
       F30MA-FN. EXIT.
      *N30MG.    NOTE *ELSE... ERROR & EXIT               *.
       F30MG.                                                           lv10
           MOVE        ZERO TO PJ14-NPISQ1.
       F30MG-FN. EXIT.
      *N30MM.    NOTE *IF REQUEST MODE IS VALID  -        *.
       F30MM.    IF    PJ14-CUPIQ = 'U'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F30MM-FN.
      *"UPDATE" ONLY CURRENTLY
       F30MM-900. GO TO F30MQ-FN.
       F30MM-FN. EXIT.
      *N30MQ.    NOTE *ELSE... ERROR & EXIT               *.
       F30MQ.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012596 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30MQ-FN. EXIT.
      *N30PC.    NOTE *VALIDATE ADMIN AGAINST ARR TYPE    *.
       F30PC.                                                           lv10
      *- ADMINS ONLY ALLOW CERTAIN TYPE
      **
           MOVE        PJ14-CTID01 TO 7-WS-CTID.
      *N30PF.    NOTE *VALIDATE ADMIN                     *.
       F30PF.         EXIT.                                             lv15
      *N30PH.    NOTE *VALIDATE ADMIN 001 (CERTS)         *.
       F30PH.    IF    7-WS-CTIDA =                                     lv20
                       001
                 NEXT SENTENCE ELSE GO TO     F30PH-FN.
                 IF    (PJ14-CARTZF NOT = 02                            DOT
                 AND   PJ14-CARTZF NOT = 06)
      *IF NOT (02 SYS PAYOUT   06  INT)
      *-- ERROR OUT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012824 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30PH-900. GO TO F30PF-FN.
       F30PH-FN. EXIT.
      *N30PM.    NOTE *VALIDATE ADMIN 002 (FUNDS)         *.
       F30PM.    IF    7-WS-CTIDA =                                     lv20
                       002
                 NEXT SENTENCE ELSE GO TO     F30PM-FN.
                 IF    (PJ14-CARTZF NOT = 01                            DOT
                 AND   PJ14-CARTZF NOT = 05)
      *IF NOT (01 SYS PAYOUT OR 05 DIV)
      *--  ERROR OUT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012824 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30PM-900. GO TO F30PF-FN.
       F30PM-FN. EXIT.
      *N30PQ.    NOTE *ELSE INVALID ADMINISTRATOR         *.
       F30PQ.                                                           lv20
      *     - ERROR OUT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012104 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30PQ-FN. EXIT.
       F30PF-FN. EXIT.
       F30PC-FN. EXIT.
       F30-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *READ FOR SOURCE CONTRACT           *
      *               *                                   *
      *               *************************************.
       F35.                                                             lv05
      *
      *********************************
      ** READ AND STORE OFF THE SOURCE*
      ** CONTRACT.                    *
      *********************************
      *
      *N35BC.    NOTE *READ SOURCE CONTRACT               *.
       F35BC.                                                           lv10
      *
           MOVE        PJ14-CTID01 TO S-CTU01-CTID
      *
           PERFORM     F94CT THRU F94CT-FN.
       F35BC-FN. EXIT.
      *N35BF.    NOTE *IF CONTRACT WAS NOT FOUND          *.
       F35BF.    IF    IK = '1'                                         lv10
                 NEXT SENTENCE ELSE GO TO     F35BF-FN.
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012011 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35BF-900. GO TO F35BH-FN.
       F35BF-FN. EXIT.
      *N35BH.    NOTE *ELSE - CNTRCT FOUND - SAVE IT      *.
       F35BH.                                                           lv10
      *
                 IF    CT01-CTIDA = 002                                 DOT
                 AND   CT01-PRSCD = SPACES
      *RESET PRSCD FOR FUNDS
           MOVE        '000000001' TO CT01-PRSCD.
           MOVE        CT01 TO ST01.                                    DOT
       F35BH-FN. EXIT.
      *N35CD.    NOTE *EDIT TO STOP DISBURSEMENTS FROM    *.
       F35CD.    IF    CT01-CTIDA = 001                                 lv10
                 AND   CT01-PRCOD = 180
                 AND   CT01-PRSCD = 000000001
                 NEXT SENTENCE ELSE GO TO     F35CD-FN.
      *SAI CERTIFICATES
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013624 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35CD-FN. EXIT.
      *N35HM.    NOTE *READ THE TA5B TABLE                *.
       F35HM.                                                           lv10
      *
      *********************************
      ** READ THE TA5B TABLE FOR THE  *
      ** PRCAUT VALUE TO SEE IF THE   *
      ** 'FROM' ACCOUNT IS TERMINATED.*
      *********************************
           MOVE        '0' TO TA5B-IK
           MOVE        ST01-CTIDA TO TA5B-CTIDA
           MOVE        ST01-PRCOD TO TA5B-PRCOD.
                 IF    ST01-CTIDA NOT = 002                             DOT
           MOVE        SPACES TO TA5B-PRSCD
                 ELSE
           MOVE        ST01-PRSCD TO TA5B-PRSCD.
           PERFORM     F92TA THRU F92TA-FN.                             DOT
                 IF    TA5B-IK = '1'                                    DOT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012405 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35HM-FN. EXIT.
      *N35MC.    NOTE *DETERMINE IF ARR TYPE ALLOWED      *.
       F35MC.                                                           lv10
      *BASED ON TA5B REC FOR CONTRACT.
      *N35MF.    NOTE *IF FUND SYSTEMATIC PAYOUT          *.
       F35MF.    IF    PJ14-CARTZF =                                    lv15
                       01
                 NEXT SENTENCE ELSE GO TO     F35MF-FN.
      *N35MH.    NOTE *IF PAYOUT NOT ALLOWED - ERROR      *.
       F35MH.    IF    TA5B-ISYPO = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F35MH-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012829 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35MH-FN. EXIT.
       F35MF-900. GO TO F35MC-FN.
       F35MF-FN. EXIT.
      *N35MJ.    NOTE *IF CERT PARTIAL (SYS PAYOUT)       *.
       F35MJ.    IF    PJ14-CARTZF =                                    lv15
                       02
                 NEXT SENTENCE ELSE GO TO     F35MJ-FN.
      *N35ML.    NOTE *IF PAYOUT NOT ALLOWED - ERROR      *.
       F35ML.    IF    TA5B-ISYPP = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F35ML-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012829 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35ML-FN. EXIT.
       F35MJ-900. GO TO F35MC-FN.
       F35MJ-FN. EXIT.
      *N35MN.    NOTE *IF FUND DIVIDEND                   *.
       F35MN.    IF    PJ14-CARTZF =                                    lv15
                       05
                 NEXT SENTENCE ELSE GO TO     F35MN-FN.
      *N35MP.    NOTE *IF DIVIDEND NOT ALLOWED - ERROR    *.
       F35MP.    IF    TA5B-IDSAR = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F35MP-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012830 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35MP-FN. EXIT.
       F35MN-900. GO TO F35MC-FN.
       F35MN-FN. EXIT.
      *N35MS.    NOTE *IF CERT INTEREST                   *.
       F35MS.    IF    PJ14-CARTZF =                                    lv15
                       06
                 NEXT SENTENCE ELSE GO TO     F35MS-FN.
      *N35MV.    NOTE *IF INTEREST NOT ALLOWED - ERROR    *.
       F35MV.    IF    TA5B-ICIPT = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F35MV-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012831 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35MV-FN. EXIT.
       F35MS-900. GO TO F35MC-FN.
       F35MS-FN. EXIT.
       F35MC-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *IF CHG/CHG STATUS GET EXISTING     *
      *               *                                   *
      *               *************************************.
       F40.      IF    PJ14-CACTS = 'C'                                 lv05
                 OR    PJ14-CACTS = 'S'
                 NEXT SENTENCE ELSE GO TO     F40-FN.
      *SEGS CX01/CX03/CX06/CX13/CX14
      *N40BC.    NOTE *MORE PARM EDITS - CHG/CHG STATUS   *.
       F40BC.                                                           lv10
      *  & GET EXISTING SEGMENTS
      *
      ** INIT "NO INSTRUCTIONS"  TO 'N'
           MOVE        'N' TO 7-WA00-NOINST.
      *N40BE.    NOTE *IF INVALID CLID FOR ARR CLIENT     *.
       F40BE.    IF    PJ14-CLID NOT NUMERIC                            lv15
                 NEXT SENTENCE ELSE GO TO     F40BE-FN.
      **
           MOVE        12596 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F40BE-FN. EXIT.
      *N40BG.    NOTE *IF INVALID ARRANGEMENT TYPE        *.
       F40BG.    IF    PJ14-CARTYK NOT = '10'                           lv15
                 NEXT SENTENCE ELSE GO TO     F40BG-FN.
      **
           MOVE        12033 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F40BG-FN. EXIT.
      *N40BJ.    NOTE *IF INVALID ARR SEQUENCE NUMBER     *.
       F40BJ.    IF    PJ14-NARRSK NOT NUMERIC                          lv15
                 NEXT SENTENCE ELSE GO TO     F40BJ-FN.
      **
           MOVE        12034 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F40BJ-FN. EXIT.
      *N40BM.    NOTE *IF INVALID ARR PAYMENT SEQ NBR     *.
       F40BM.    IF    PJ14-NAPDS NOT NUMERIC                           lv15
                 NEXT SENTENCE ELSE GO TO     F40BM-FN.
      **
           MOVE        12036 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F40BM-FN. EXIT.
       F40BC-FN. EXIT.
      *N40CC.    NOTE *SET UP TO CALL CI0090              *.
       F40CC.                                                           lv10
      *
           INITIALIZE  7-WA00-STAGING
      *
      ********************************
      *MOVE CI0072'S LINKAGE FIELDS TO
      *THESE TEMP FIELDS WHICH MOVE TO
      *CI0090'S LINKAGE AS PART OF THE
      *AM0090 MACRO.
      ********************************
      *
      *PERTINENCE PROCESSING FIELDS...
           MOVE        'N' TO 7-WA00-IPERT
      *
      *OTHER KEY FIELDS....
           MOVE        PJ14-MAPPN TO 7-WA00-MAPPN
           MOVE        PJ14-CLID TO 7-WA00-CLID
           MOVE        PJ14-CARTYK TO 7-WA00-CARTY
           MOVE        PJ14-NARRSK TO 7-WA00-NARRS
           MOVE        PJ14-CTID01 TO 7-WA00-CTID
           MOVE        PJ14-CARTZF TO 7-WA00-CARTZ
           MOVE        PJ14-NAPDS TO 7-WA00-NAPDS
           MOVE        PJ14-NPISQ1 TO 7-WA00-NPISQ
      *
           INITIALIZE  MS03.
       F40CC-FN. EXIT.
      *N40EC.    NOTE *---> CALL CI0090                   *.            AM0090
       F40EC.                                                           lv10
      *     GET CLIENT ARRANGEMENT                                      AM0090
      *     DETAILS...                                                  AM0090
      *                                                                 AM0090
           INITIALIZE  WA08                                             AM0090
           DE10-DU03                                                    AM0090
      *                                                                 AM0090
      *PERTINENCE PROCESSING FIELDS...                                  AM0090
           MOVE        7-WA00-IPERT TO WA08-IPERT                       AM0090
           MOVE        7-WA00-NEIBT TO WA08-NEIBT                       AM0090
           MOVE        7-WA00-GESQ2C TO WA08-GESQ2C                     AM0090
      *                                                                 AM0090
      *OTHER KEY FIELDS...                                              AM0090
           MOVE        7-WA00-MAPPN TO WA08-MAPPN                       AM0090
           MOVE        7-WA00-CLID TO WA08-CLID                         AM0090
           MOVE        7-WA00-CARTY TO WA08-CARTY                       AM0090
           MOVE        7-WA00-NARRS TO WA08-NARRS                       AM0090
           MOVE        7-WA00-CTID TO WA08-CTID                         AM0090
           MOVE        7-WA00-CARTZ TO WA08-CARTZ                       AM0090
           MOVE        7-WA00-NAPDS TO WA08-NAPDS                       AM0090
           MOVE        7-WA00-NPISQ TO WA08-NPISQ                       AM0090
           MOVE        7-WA00-NBASQ TO WA08-NBASQ                       AM0090
           MOVE        7-WA00-CDEL1 TO WA08-CDEL1                       AM0090
           MOVE        7-WA00-NDELS TO WA08-NDELS                       AM0090
           MOVE        7-WA00-NPAIS TO WA08-NPAIS                       AM0090
           SET CI0090-PCB-AR1P-PTR1 TO                                  AM0090
                      PCB-AR1P-PTR1                                     AM0090
           SET CI0090-PCB-CA1P-PTR1 TO                                  AM0090
                      PCB-CA1P-PTR1                                     AM0090
           SET CI0090-PCB-TR1P-PTR1 TO                                  AM0090
                      PCB-TR1P-PTR1                                     AM0090
           CALL        CI0090 USING                                     AM0090
           DFHEIBLK                                                     AM0090
           DFHCOMMAREA                                                  AM0090
           DLIUIBII                                                     AM0090
           CI0090-PCB-ADDRESS-LIST                                      AM0090
           WA08                                                         AM0090
           WX01                                                         AM0090
           WX03                                                         AM0090
           WX06                                                         AM0090
           WX09                                                         AM0090
           WX13                                                         AM0090
           WX14                                                         AM0090
           WX18                                                         AM0090
           WX21                                                         AM0090
           DE10                                                         AM0090
           MS03                                                         AM0090
           MX11.                                                        AM0090
      *--->   (REPLACE MACRO CODE)                                      DOT
      *N40EF.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F40EF.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F40EF-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0090 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0090 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40EF-900. GO TO F40EG-FN.
       F40EF-FN. EXIT.
      *N40EG.    NOTE *NO ERRORS                          *.            ADU071
       F40EG.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F40EG-FN. EXIT.
       F40EC-FN. EXIT.
      *N40GC.    NOTE *IF CX01/CX03/CX06/CX13 NOT FOUND   *.
       F40GC.    IF    WA08-ICX01 = 'N'                                 lv10
                 OR    WA08-ICX03 = 'N'
                 OR    WA08-ICX06 = 'N'
                 OR    WA08-ICX13 = 'N'
                 NEXT SENTENCE ELSE GO TO     F40GC-FN.
      ** MUST FIND THESE FOR CHG/STATUS
      **
      **   SET ERROR & GO BACK
      **
           MOVE        12756 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F40GC-FN. EXIT.
      *N40GG.    NOTE *IF CX14 NOT FOUND                  *.
       F40GG.    IF    WA08-ICX14 = 'N'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F40GG-FN.
      *N40GI.    NOTE *IF CX14 WAS SPECIFIED - ERROR      *.
       F40GI.    IF    PJ14-NPISQ1 NOT ZERO                             lv15
                 NEXT SENTENCE ELSE GO TO     F40GI-FN.
      **
      **   SET ERROR & GO BACK
      **
           MOVE        12026 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F40GI-900. GO TO F40GL-FN.
       F40GI-FN. EXIT.
      *N40GL.    NOTE *ELSE - SET "NO INSTRUCTIONS"       *.
       F40GL.                                                           lv15
      **
           MOVE        'Y' TO 7-WA00-NOINST.
       F40GL-FN. EXIT.
       F40GG-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *IF "INACTIVATING" - DO EDITING     *
      *               *                                   *
      *               *************************************.
       F45.      IF    PJ14-CACTS = 'S'                                 lv05
                 AND   PJ14-CACTA = 'I'
                 NEXT SENTENCE ELSE GO TO     F45-FN.
      *  & AND EXIT TO CALLING PROGRAM
      *N45CC.    NOTE *IF CX13 ALREADY INACTIVE - ERROR   *.
       F45CC.    IF    WX13-CDEST = 03                                  lv10
                 NEXT SENTENCE ELSE GO TO     F45CC-FN.
      **
           MOVE        12608 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F45CC-FN. EXIT.
      *N45EC.    NOTE *IF "INACTIVATING" - SET INACTIVE   *.
       F45EC.    IF    PS13-CDEST NOT = 03                              lv10
                 NEXT SENTENCE ELSE GO TO     F45EC-FN.
      **
           MOVE        12837 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F45EC-FN. EXIT.
      *N45GC.    NOTE *CASE OF EDITS ON ARR DETAIL TYPE   *.
       F45GC.                                                           lv10
      **
      *N45GF.    NOTE *IF CERTIFICATE INTEREST DETAIL     *.
       F45GF.    IF    PJ14-CARTZF =                                    lv15
                       06
                 NEXT SENTENCE ELSE GO TO     F45GF-FN.
      **
      ** CAN'T INACTIVATE CERT INT ARR
      **
           MOVE        12835 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F45GF-900. GO TO F45GC-FN.
       F45GF-FN. EXIT.
      *N45GH.    NOTE *IF FUND SYSTEMATIC PAYOUT          *.
       F45GH.    IF    PJ14-CARTZF =                                    lv15
                       01
                 NEXT SENTENCE ELSE GO TO     F45GH-FN.
      **
      *N45GK.    NOTE *IF LIQUIDATE/LIFE EXPECTANCY       *.
       F45GK.    IF    WX13-QMTH1 > 0                                   lv20
                 NEXT SENTENCE ELSE GO TO     F45GK-FN.
      **
      ** CAN'T INACTIVATE FUND SD WITH
      ** LIQUIDATE OR LIFE EXP ON IT
      **
           MOVE        12836 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F45GK-FN. EXIT.
       F45GH-900. GO TO F45GC-FN.
       F45GH-FN. EXIT.
       F45GC-FN. EXIT.
      *N45GM.    NOTE *RETURN TO CALLING PROGRAM          *.
       F45GM.                                                           lv10
      **
      *********************************
      **  FOR INACTIVATE REQUESTS THE *
      **  EDITING IS OVER -           *
      **                              *
      **   * RETURN TO CALLING PGM *  *
      **                              *
      *********************************
      **
           MOVE                     ALL '1' TO FT GO TO F20.
       F45GM-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *FURTHER VALIDATION OF              *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      * - SOURCE ACCT PRODUCT INFO
      * - SOURCE ACCT
      * - FUNCTION REQUESTED
      *N50CC.    NOTE *IF TA5B - "PRODUCT IS TERMINATED   *.
       F50CC.    IF    TA5B-PRCAUT = 'T'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50CC-FN.
      **
           MOVE        12807 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50CC-FN. EXIT.
      *N50GC.    NOTE *CHECK FOR INACT SOURCE FOR CERTS   *.
       F50GC.                                                           lv10
      *
           MOVE        PJ14-CTID01 TO 7-WS-CTID.
                 IF    7-WS-CTIDA = 001                                 DOT
                 AND   ST01-CTSTA = 03
           MOVE        12840 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50GC-FN. EXIT.
      *N50IC.    NOTE *READ FOR A CT13 - ACCT ON HOLD     *.
       F50IC.    IF    PJ14-CARTZF = 01                                 lv10
                 NEXT SENTENCE ELSE GO TO     F50IC-FN.
      *** DON'T EDIT IF REQUEST IS DIV*
      ** IF A CT13 IS FOUND THE ACCOUNT
      ** IS CONSIDERED "ON HOLD" NO
      ** PROCESSING IS ALLOWED
      *
           MOVE        PJ14-CTID01 TO S-CTU01-CTID
      *
           PERFORM     F94CU THRU F94CU-FN.
                 IF    IK = '0'                                         DOT
                 AND   CT13-GEHCD = 045
      **  AML HOLD CODE
           MOVE        15582 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
                 IF    IK = '0'                                         DOT
      **  ERROR IF "FOUND"
           MOVE        12841 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50IC-FN. EXIT.
      *N50ID.    NOTE *CALL CI0135 TO GET CERT HOLD CD    *.
       F50ID.    IF    PJ14-CARTZF = 02 OR 06                           lv10
                 NEXT SENTENCE ELSE GO TO     F50ID-FN.
      ** IF ANY OF THE JP02-CEHCD(*)
      ** VALUES >0 ACCT IS "ON HOLD"
      ** NO PROCESSING IS ALLOWED
      **  ERROR IF "FOUND"
           PERFORM     F95FA THRU F95FA-FN
           MOVE        'Y' TO WS-CI0135-CALL-MADE-IND.
                 IF    JP02-CEHCD (1) > ZERO                            DOT
                 OR    JP02-CEHCD (2) > ZERO
                 OR    JP02-CEHCD (3) > ZERO
                 OR    JP02-CEHCD (4) > ZERO
                 OR    JP02-CEHCD (5) > ZERO
                 OR    JP02-CEHCD (6) > ZERO
           MOVE        13174 TO MS03-NMESS2
           PERFORM     F98MX THRU F98MX-FN.
       F50ID-FN. EXIT.
      *N50KC.    NOTE *IF REQUESTING A CHANGE             *.
       F50KC.    IF    PJ14-CACTS = 'C'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F50KC-FN.
      *
      *N50KE.    NOTE *IF CHG OF CERT INTEREST -          *.
       F50KE.    IF    WX13-CARTZ = 06                                  lv15
                 NEXT SENTENCE ELSE GO TO     F50KE-FN.
      *********************************
      ** DETERMINE IF MULTIPLE CX14S  *
      ** EXIST. EZ-TRANS CANNOT PROCESS
      ** CERT ARR WITH MULTIPLE CX14S.*
      *********************************
      **
           PERFORM     F95JC THRU F95JC-FN.
       F50KE-FN. EXIT.
      *N50KN.    NOTE *IF CHGING AN INACTIVE DETAIL       *.
       F50KN.    IF    WX13-CDEST = 03                                  lv15
                 NEXT SENTENCE ELSE GO TO     F50KN-FN.
      *
           MOVE        12844 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50KN-FN. EXIT.
      *N50KQ.    NOTE *IF CHGING FUND SD WITH LIQUIDATE   *.
       F50KQ.    IF    WX13-CARTZ = 01                                  lv15
                 AND   WX13-QMTH1 > 0
                 NEXT SENTENCE ELSE GO TO     F50KQ-FN.
      *OR LIFE EXPECTENCY - ERROR
      *
           MOVE        12842 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50KQ-FN. EXIT.
       F50KC-FN. EXIT.
      *N50PC.    NOTE *IF REACTIVATING & CX14 EXIST       *.
       F50PC.    IF    PJ14-CACTA = 'R'                                 lv10
                 AND   7-WA00-NOINST = 'N'
                 NEXT SENTENCE ELSE GO TO     F50PC-FN.
      *IF CX14 IS A "TRANSFER" READ
      *THE CONTRACT AND TA5B FOR THE
      *CONTRACT ID ON THE CX14
      *
      *MAY ISSUE "INFO" ERROR MSGS
      *
      *N50PF.    NOTE *IF CX14 IS A TRANSFER              *.
       F50PF.    IF    WX14-CPITC = 02                                  lv15
                 NEXT SENTENCE ELSE GO TO     F50PF-FN.
      *
      *READ CONTRACT DB FOR DEST ACCT
      *
           MOVE        WX14-CTID TO S-CTU01-CTID
      *
           PERFORM     F94CT THRU F94CT-FN.
      *N50PG.    NOTE *IF DEST CONTRACT NOT FOUND         *.
       F50PG.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F50PG-FN.
      *
           MOVE        12011 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50PG-FN. EXIT.
      *N50PJ.    NOTE *IF DEST CONTRACT IS INACTIVE       *.
       F50PJ.    IF    CT01-CTSTA = 03                                  lv20
                 NEXT SENTENCE ELSE GO TO     F50PJ-FN.
      *
      *  *** INFORMATIONAL ERROR ***
      *
           MOVE        12843 TO MS03-NMESS2
           PERFORM     F98MX THRU F98MX-FN.
       F50PJ-FN. EXIT.
      *N50PL.    NOTE *POPULATE VALID PRODUCT CODES       *.
       F50PL.                                                           lv20
           MOVE        ST01-PRCOD TO WS00-FR-PRCOD
           MOVE        ST01-PRSCD TO WS00-FR-PRSCD
           MOVE        CT01-PRCOD TO WS00-TO-PRCOD
           MOVE        CT01-PRSCD TO WS00-TO-PRSCD.
       F50PL-FN. EXIT.
      *N50RC.    NOTE *IF FUND - SET PRSCD                *.
       F50RC.    IF    CT01-CTIDA = 002                                 lv20
                 AND   CT01-PRSCD = SPACES
                 NEXT SENTENCE ELSE GO TO     F50RC-FN.
      *  *** SET DEFUALT FOR FUNDS ***
      *
           MOVE        '000000001' TO CT01-PRSCD.
       F50RC-FN. EXIT.
      *N50RL.    NOTE *READ THE TA5B TABLE                *.
       F50RL.                                                           lv20
      *
      *********************************
      ** READ THE TA5B TABLE FOR THE  *
      ** PRCAUT VALUE TO SEE IF THE   *
      **  'TO'  ACCOUNT IS TERMINATED.*
      *********************************
           MOVE        '0' TO TA5B-IK
           MOVE        CT01-CTIDA TO TA5B-CTIDA
           MOVE        CT01-PRCOD TO TA5B-PRCOD.
                 IF    CT01-CTIDA NOT = 002                             DOT
           MOVE        SPACES TO TA5B-PRSCD
                 ELSE
           MOVE        CT01-PRSCD TO TA5B-PRSCD.
           PERFORM     F92TA THRU F92TA-FN                              DOT
      **
                 IF    TA5B-IK = '1'                                    DOT
           MOVE        12405 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50RL-FN. EXIT.
      *N50RR.    NOTE *IF TA5B - "PRODUCT IS TERMINATED   *.
       F50RR.    IF    TA5B-PRCAUT = 'T'                                lv20
                 NEXT SENTENCE ELSE GO TO     F50RR-FN.
      **
      *  *** INFORMATIONAL ERROR ***
      **
           MOVE        12838 TO MS03-NMESS2
           PERFORM     F98MX THRU F98MX-FN.
       F50RR-FN. EXIT.
      *N50RU.    NOTE *RESTRICT  CSH MGMT AND TAX EXEMP   *.
       F50RU.    IF    ((ST01-CTIDA = 002                               lv20
                 AND   SPECIFIED-MF
                 AND   FR-CLASS-A)
                 OR    ST01-CTIDA NOT = 002)
                 AND   (CT01-CTIDA = 002
                 AND   TO-CLASS-B
                 AND   CLOSE-B-SHARE)
                 NEXT SENTENCE ELSE GO TO     F50RU-FN.
      *CLASS A FUND
      *CLOSE SPECIFIC B-FUNDS TO ALLOW
      *TRNSFER ONLY MF SOURCE ACCTS
      *---> SEND ERROR MESSAGE
      *      AND TERMINATE
           MOVE        15320 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50RU-FN. EXIT.
       F50PF-FN. EXIT.
       F50PC-FN. EXIT.
       F50-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *IF PROCESSING AN "ADD"             *
      *               *                                   *
      *               *************************************.
       F60.      IF    PJ14-CACTS = 'A'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F60-FN.
      *
      *
      *N60AM.    NOTE *"OWNERSHIP" EDITS FOR ADDS OF      *.
       F60AM.    IF    PJ14-CARTZF = 01                                 lv10
                 OR    PJ14-CARTZF = 02
                 NEXT SENTENCE ELSE GO TO     F60AM-FN.
      *FUND/CERT SYSTEMATIC PAYOUTS
      *
      *
      *GET ACCOUNT OWNERSHIP   CI0003
      *PERFORM OWNERSHIP EDITS CI0083
      *
           INITIALIZE  MS03.
      *N60BC.    NOTE *CALL CI0003 - ACCT OWNER/BENE      *.            AM0003
       F60BC.                                                           lv15
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
           MOVE        PJ14-CTID01 TO FA04-CTID                         AM0003
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
      *N60BD.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F60BD.    IF    (MS03-NMESS2 > ZERO                              lv20
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F60BD-FN.                 ADU071
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
       F60BD-900. GO TO F60BE-FN.
       F60BD-FN. EXIT.
      *N60BE.    NOTE *NO ERRORS                          *.            ADU071
       F60BE.                                                           lv20
           INITIALIZE  MS03.                                            ADU071
       F60BE-FN. EXIT.
       F60BC-FN. EXIT.
      *N60BM.    NOTE *CALL CI0083 - DECIPHER OWNERSHIP   *.            AM0083
       F60BM.                                                           lv15
      *********************************.                                DOT
      ** THIS MODULE WILL READ THE    *.                                DOT
      ** OWNERSHIP LINES ACCESSED BY  *.                                DOT
      ** CI0003 AND SEARCH THROUGH    *.                                DOT
      ** THE TEXT LOOKING FOR ACCT    *.                                DOT
      ** CLASSIFICATION               *.                                DOT
      *********************************                                 DOT
           INITIALIZE   WP1F                                            AM0083
           MOVE        PROGR TO WP1F-PROGR.                             AM0083
      *INSERT ADDITIONAL MOVES BELOW.                                   DOT
      *THINGS LIKE MAPPN AND ADDR LINES.                                DOT
      *INSERT ADDITIONAL MOVES ABOVE                                    DOT
           CALL        CI0083 USING                                     AM0083
           DFHEIBLK                                                     AM0083
           DFHCOMMAREA                                                  AM0083
           ST01                                                         AM0083
           FA04                                                         AM0083
           WP1F                                                         AM0083
           MS03                                                         AM0083
           MX11.                                                        AM0083
       F60BM-FN. EXIT.
      *N60BP.    NOTE *IF NO ERRORS - BREAK DOWN WP1F     *.            AM0083
       F60BP.    IF    (MS03-NMESS2 = ZEROS                             lv15
                 OR    (MS03-NMESS2 NOT = ZEROS                         AM0083
                 AND   MS03-CMESB < 11))                                AM0083
                 NEXT SENTENCE ELSE GO TO     F60BP-FN.                 AM0083
           MOVE        WP1F-OUTPUT TO W-WP00-DECIPHER.                  AM0083
      *TALLY NUMBER OF 'Y' (TRUE) FLAGS                                 DOT
           INITIALIZE  TALLI                                            AM0083
           INSPECT     WP1F-OUTPUT TALLYING TALLI                       AM0083
           FOR ALL 'Y'.                                                 AM0083
      *N60BR.    NOTE *IF RESTRICTED OWNERSHIPS           *.
       F60BR.    IF    USUFRUCTORY                                      lv20
                 OR    LIFE-TENANT
                 NEXT SENTENCE ELSE GO TO     F60BR-FN.
      *** ERROR OUT IF THEY EXIST **
           MOVE        12845 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60BR-FN. EXIT.
       F60BP-900. GO TO F60BV-FN.
       F60BP-FN. EXIT.
      *N60BV.    NOTE *ELSE - THERE WAS AN ERROR          *.
       F60BV.                                                           lv15
      *********************************
      ** THERE WAS AN ERROR IN TRYING *
      ** TO SET UP OWNERSIP EDITING.  *
      **                              *
      ** RETURN TO CALLING MODULE     *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F60BV-FN. EXIT.
       F60AM-FN. EXIT.
      *N60CA.    NOTE *GET SURRENDER AMOUNT FOR CERT      *.
       F60CA.    IF    (PJ14-CARTZF = 06                                lv10
                 AND   (ST01-PRCOD = 00662 OR
                       00972 OR 00660 OR 00970))
                 OR    (PJ14-CARTZF = 02)
                 NEXT SENTENCE ELSE GO TO     F60CA-FN.
      *CASH RESERVE-3 & CASH RESERVE
      *PRODUCTS IF ADDING AN INT ARR.
      *CALL CI0135 TO GET VALUES
      *
                 IF    NOT WS-CI0135-CALL-MADE                          DOT
           PERFORM     F95FA THRU F95FA-FN.
      *N60CB.    NOTE *IF CERT SD, LOOK FOR (CO)          *.
       F60CB.    IF    PJ14-CARTZF = 02                                 lv15
                 NEXT SENTENCE ELSE GO TO     F60CB-FN.
      *IF CX13 (CO) FOUND W/ ANNUITY
      *PAYOUT OPTION, THEN ERROR OUT
                 IF    JP02-CETYPC = 05                                 DOT
                 AND   JP02-CEOTP = 4
           MOVE        013156 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60CB-FN. EXIT.
      *N60CC.    NOTE *CERTS INTEREST ARRANGEMENT EDIT    *.
       F60CC.    IF    PJ14-CARTZF = 06                                 lv15
                 NEXT SENTENCE ELSE GO TO     F60CC-FN.
      *N60CE.    NOTE *IF CASH RESERVE-3 & $ < $1000      *.
       F60CE.    IF    (ST01-PRCOD = 662 OR 972)                        lv20
                 AND   JP02-ACCTV8 < 1000
                 NEXT SENTENCE ELSE GO TO     F60CE-FN.
      *- ERROR OUT
      *
           MOVE        13159 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60CE-FN. EXIT.
      *N60CG.    NOTE *IF CASH RESERVE   & $ < $10000     *.
       F60CG.    IF    (ST01-PRCOD = 660 OR 970)                        lv20
                 AND   JP02-ACCTV8 < 10000
                 NEXT SENTENCE ELSE GO TO     F60CG-FN.
      *- ERROR OUT
      *
           MOVE        13160 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60CG-FN. EXIT.
       F60CC-FN. EXIT.
       F60CA-FN. EXIT.
      *N60CV.    NOTE *EDITS FOR "ADDS"                   *.
       F60CV.    IF    PJ14-CARTZF = 05                                 lv10
                 OR    PJ14-CARTZF = 06
                 OR    PJ14-CARTZF = 02
                 OR    PJ14-CARTZF = 01
                 NEXT SENTENCE ELSE GO TO     F60CV-FN.
      *- CAN'T ADD (CI) IF ONE EXISTS
      *- CAN'T ADD (CP) IF (CO) W/ANN
      *- CAN'T ADD (CP) DIRECT DEPOSIT
      *        IF DIRECT DEPOSIT EXISTS
      *N60DA.    NOTE *READ ARRANGEMNET DATABASE          *.
       F60DA.                                                           lv15
      *-LOOP THRU CX13S
      *
           MOVE        PJ14-CTID01 TO S-CXU2Y-CTID
           MOVE        ZERO TO S-CXU2Y-CLID
           S-CXU2Y-CARTY
           S-CXU2Y-NARRS
           MOVE        'GE' TO S-CXU2Y-OPER
      *
      *- READ CX2Y WITH BOOLEAN OPER
      *
           PERFORM     F94X2 THRU F94X2-FN.
       F60DA-FN. EXIT.
      *N60DC.    NOTE *INITIALIZE FLAGS                   *.
       F60DC.                                                           lv15
           MOVE        'N' TO WS-DIRECT-DEPOSIT-IND
           MOVE        'N' TO WS-SD-FOUND-IND.
       F60DC-FN. EXIT.
      *N60DD.    NOTE *LOOP THRU UNTIL SD FOUND; OR NOT   *.
       F60DD.    IF    IK = '0'                                         lv15
                 AND   CX2Y-CTID = PJ14-CTID01
                 NEXT SENTENCE ELSE GO TO     F60DD-FN.
      *
      *
      *N60DG.    NOTE *IF SD ARRANGEMENT; CLIENT FOUND    *.
       F60DG.    IF    CX2Y-CTID = PJ14-CTID01                          lv20
                 AND   CX2Y-CARTY = '10'
                 AND   IK = '0'
                 NEXT SENTENCE ELSE GO TO     F60DG-FN.
      *N60FA.    NOTE *PROCESS DETAIL                     *.
       F60FA.                                                           lv25
      *********************************
      ** READ THE SD ARRANGEMENT DOWN *
      ** TO THE CX06.  THEN LOOK TO   *
      ** SEE IF CX13S EXIST WHICH     *
      ** WOULD PREVENT FURTHER ADDS   *
      *********************************
      *
           MOVE        CX2Y-CLID TO S-CXU01-CLID
           MOVE        CX2Y-CARTY TO S-CXU03-CARTY
           MOVE        CX2Y-NARRS TO S-CXU03-NARRS
           MOVE        CX2Y-CTID TO S-CXU06-CTID
      *
      * - READ CX06 WITH BOOLEAN OPER
      *
           PERFORM     F94C6 THRU F94C6-FN.
       F60FA-FN. EXIT.
      *N60FC.    NOTE *IF CX06 ISN'T FOUND                *.
       F60FC.    IF    IK NOT = '0'                                     lv25
                 NEXT SENTENCE ELSE GO TO     F60FC-FN.
           MOVE        012008 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60FC-FN. EXIT.
      *N60FF.    NOTE *IF CX06 FOUND; IT SHOULD BE        *.
       F60FF.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F60FF-FN.
           MOVE        PJ14-CARTZF TO S-CXU13-CARTZ
           MOVE        ZERO TO S-CXU13-NAPDS
           MOVE        '>=' TO S-CXU13-OPER
      *
      * - READ CX13 WITH BOOLEAN OPER
      *
           PERFORM     F94C3 THRU F94C3-FN
           MOVE        IK TO CX13-IK.
      *N60FH.    NOTE *LOOP THROUGH THE CX13S WHILE       *.
       F60FH.    IF    CX13-IK = '0'                                    lv30
                 NEXT SENTENCE ELSE GO TO     F60FH-FN.
      *N60FI.    NOTE *IF SD EXISTS, SET SD FOUND TO Y    *.
       F60FI.    IF    CX13-CARTZ = 01 OR 02                            lv35
                 NEXT SENTENCE ELSE GO TO     F60FI-FN.
           MOVE        'Y' TO WS-SD-FOUND-IND
      *IF FUND SD EXISTS, SET FLAG
      *AND GET OUT OF THIS LOOP
                 IF    PJ14-CARTZF = 01                                 DOT
               GO TO     F60DD-FN.
       F60FI-FN. EXIT.
      *N60FP.    NOTE *IF MATCHING CX13 FOUND             *.
       F60FP.    IF    CX13-CARTZ = PJ14-CARTZF                         lv35
                 NEXT SENTENCE ELSE GO TO     F60FP-FN.
                 IF    PJ14-CARTZF = 05                                 DOT
      *IF CX13 FOUND IS A DIVIDEND
           MOVE        012846 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
                 IF    PJ14-CARTZF = 06                                 DOT
      *IF CX13 FOUND IS AN INTEREST ARR
           MOVE        013157 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
      *N60FQ.    NOTE *CERT SP - CHK FOR DIRECT DEPOSIT   *.
       F60FQ.    IF    PJ14-CARTZF = 02                                 lv40
                 NEXT SENTENCE ELSE GO TO     F60FQ-FN.
                 IF    NOT WS-DIRECT-DEPOSIT-FOUND                      DOT
           PERFORM     F95CP THRU F95CP-FN.
      *GET THE NEXT CX13                                                DOT
           PERFORM     F94C4 THRU F94C4-FN
           MOVE        IK TO CX13-IK.
       F60FQ-FN. EXIT.
       F60FP-900. GO TO F60FT-FN.
       F60FP-FN. EXIT.
      *N60FT.    NOTE *ELSE -  READ FOR THE NEXT CX13     *.
       F60FT.                                                           lv35
      **
           PERFORM     F94C4 THRU F94C4-FN
           MOVE        IK TO CX13-IK.
       F60FT-FN. EXIT.
       F60FH-900. GO TO F60FH.
       F60FH-FN. EXIT.
      *N60FW.    NOTE *READ THE NEXT CX2Y                 *.
       F60FW.                                                           lv30
      *
      * - READ CX2Y WITH BOOLEAN OPER
      *
           PERFORM     F94Y2 THRU F94Y2-FN.
       F60FW-FN. EXIT.
       F60FF-FN. EXIT.
       F60DG-900. GO TO F60HC-FN.
       F60DG-FN. EXIT.
      *N60HC.    NOTE *READ THE NEXT CX2Y                 *.
       F60HC.                                                           lv20
      *
      * - READ CX2Y WITH BOOLEAN OPER
      *
           PERFORM     F94Y2 THRU F94Y2-FN.
       F60HC-FN. EXIT.
       F60DD-900. GO TO F60DD.
       F60DD-FN. EXIT.
      *N60KC.    NOTE *PRX EDITING - 'ADD' INFO MSG       *.
       F60KC.    IF    WS-SD-FOUND                                      lv15
                 AND   ST01-CLCUS = 02
                 AND   ST01-CQACT = 001
                 AND   ST01-CTCCI = '1'
                 AND   (PJ14-CARTZF = 01 OR 02)
                 NEXT SENTENCE ELSE GO TO     F60KC-FN.
      * O - WHERE PRX ALREADY APPLIES
      * O - ON AN IRA ACCOUNT
      * O - WHERE AMEX IS CUSTODIAN
      * O - ADDING SYS PAYOUT CERT/FUND
      *
      *---> Send INFO ERR Message                                       ADU119
      *      and CONTINUE                                               ADU119
           MOVE        012828 TO MS03-NMESS2                            ADU119
           PERFORM     F98MX THRU F98MX-FN.                             ADU119
       F60KC-FN. EXIT.
       F60CV-FN. EXIT.
       F60-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *IF "REACTIVATING"                  *
      *               *                                   *
      *               *************************************.
       F65.      IF    PJ14-CACTA = 'R'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F65-FN.
      *
      *N65BA.    NOTE *IF CERTAIN CALLING PGM - BYPASS    *.
       F65BA.    IF    PJ14-PROGR = 'CI9003'                            lv10
                 NEXT SENTENCE ELSE GO TO     F65BA-FN.
      *"REACTIVATING" EDITS.  FOR THE
      *SD PHASE OF TRANSACTION RE-ENG
      *THE FOLLOWING EDITS DON'T APPLY
      *TO CI9003.
      *PERFORM OWNERSHIP EDITS CI0083
      **** CONTINUE ***
       F65BA-900. GO TO F65BC-FN.
       F65BA-FN. EXIT.
      *N65BC.    NOTE *ELSE -  PERFORM EDITS              *.
       F65BC.         EXIT.                                             lv10
      *N65BF.    NOTE *IF DETAIL ALREADY ACTIVE           *.
       F65BF.    IF    WX13-CDEST = 01                                  lv15
                 NEXT SENTENCE ELSE GO TO     F65BF-FN.
      ** REACTIVATING BUT ARR DETAIL
      ** IS ALREADY ACTIVE - ERROR
      **
           MOVE        12608 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65BF-FN. EXIT.
      *N65BI.    NOTE *IF FUND SD OR DIV OR CERT SD       *.
       F65BI.    IF    PJ14-CARTZF = 01                                 lv15
                 AND   PJ14-CARTZF = 02
                 AND   PJ14-CARTZF = 05
                 NEXT SENTENCE ELSE GO TO     F65BI-FN.
      ** PERFORM EDITS BASED ON THE
      ** STATUS OF THE CONTRACT
      **
      *********************************
      ** THESE EDITS ENSURE THE PGM   *
      ** CALLING HAS SET THE STATUS   *
      ** CORRECTLY - THE USERS OF THE *
      ** APP PUSH A "CHANGE STATUS";  *
      ** THEY DON'T TELL WHAT TO SET  *
      ** IT TO.                       *
      *********************************
      **
      *N65BM.    NOTE *CASE OF - SOURCE CONTRACT STATUS   *.
       F65BM.         EXIT.                                             lv20
      *N65CC.    NOTE *IF CONTRACT IS ACTIVE              *.
       F65CC.    IF    ST01-CTSTA =                                     lv25
                       02
                 NEXT SENTENCE ELSE GO TO     F65CC-FN.
      *N65CG.    NOTE *IF PASSED DETAIL STATUS NOT        *.
       F65CG.    IF    PS13-CDEST NOT = 01                              lv30
                 NEXT SENTENCE ELSE GO TO     F65CG-FN.
      ** SET TO ACTIVE
      **
           MOVE        12847 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65CG-FN. EXIT.
       F65CC-900. GO TO F65BM-FN.
       F65CC-FN. EXIT.
      *N65EC.    NOTE *IF CONTRACT IS PENDING             *.
       F65EC.    IF    ST01-CTSTA =                                     lv25
                       01
                 NEXT SENTENCE ELSE GO TO     F65EC-FN.
      *N65EG.    NOTE *IF PASSED DETAIL STATUS NOT        *.
       F65EG.    IF    PS13-CDEST NOT = 02                              lv30
                 NEXT SENTENCE ELSE GO TO     F65EG-FN.
      ** SET TO PENDING
      **
           MOVE        12848 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65EG-FN. EXIT.
       F65EC-900. GO TO F65BM-FN.
       F65EC-FN. EXIT.
      *N65GC.    NOTE *IF CONTRACT IS INACTIVE            *.
       F65GC.    IF    ST01-CTSTA =                                     lv25
                       03
                 NEXT SENTENCE ELSE GO TO     F65GC-FN.
      **
      ** BREAK DOWN SOURCE CONTRACT ID
           MOVE        ST01-CTID TO 7-WS-CTID.
      *N65GF.    NOTE *IF CONTRACT ADMIN IS A FUND        *.
       F65GF.    IF    ST01-CTIDA = 002                                 lv30
                 NEXT SENTENCE ELSE GO TO     F65GF-FN.
      **
      *N65GJ.    NOTE *IF PASSED DETAIL STATUS NOT        *.
       F65GJ.    IF    PS13-CDEST NOT = 02                              lv35
                 NEXT SENTENCE ELSE GO TO     F65GJ-FN.
      ** SET TO PENDING
      **
           MOVE        12849 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65GJ-FN. EXIT.
       F65GF-900. GO TO F65GL-FN.
       F65GF-FN. EXIT.
      *N65GL.    NOTE *ELSE - INVALID ADMIN/CTCT STATUS   *.
       F65GL.                                                           lv30
      **
           MOVE        12850 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65GL-FN. EXIT.
       F65GC-900. GO TO F65BM-FN.
       F65GC-FN. EXIT.
      *N65GP.    NOTE *ELSE -UNKNOWN STATUS FOR REACTIV   *.
       F65GP.    IF    ST01-CTSTA =                                     lv25
                       00 OR 04
                 NEXT SENTENCE ELSE GO TO     F65GP-FN.
      **
           MOVE        12851 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65GP-900. GO TO F65BM-FN.
       F65GP-FN. EXIT.
      *N65GS.    NOTE *ELSE -CAN'T REACTIVATE FOR ARR     *.
       F65GS.                                                           lv25
      **     DETAIL TYPE
           MOVE        12852 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65GS-FN. EXIT.
       F65BM-FN. EXIT.
       F65BI-FN. EXIT.
       F65BC-FN. EXIT.
       F65-FN.   EXIT.
      *N67.      NOTE *************************************.
      *               *                                   *
      *               *EDITS FOR FINANCIAL PLANS ON A     *
      *               *                                   *
      *               *************************************.
       F67.                                                             lv05
      *A REACTIVATE MODE
      *N67BB.    NOTE *ON A REACTIVATE MODE               *.
       F67BB.    IF    PJ14-CACTA = 'R'                                 lv10
                 AND   PJ14-MAPPN = 'SD'
                 AND   CT01-CTIDA = 013
                 AND   CT01-PRCOD = 26
                 NEXT SENTENCE ELSE GO TO     F67BB-FN.
      *IF SD
      *&  FP
      *&  GTCC
      *N67BD.    NOTE *CHECK THE ARRANGEMENT DATABASE     *.
       F67BD.                                                           lv15
      *FOR ANOTHER ACTIVE SPO TO A GTCC
      *FOR THE CLIENT
      *N67BF.    NOTE *INITIALIZE FOR CX6Y READ           *.
       F67BF.                                                           lv20
           MOVE        '0' TO CX6Y-CF
           MOVE        ZERO TO CX6Y-CX6YK
           MOVE        CT01-CTID TO S-CXU6Y-CTID
           MOVE        '>=' TO S-CXU6Y-OPER
      *
      *DO THE FIRST CX6Y READ (GU)
      *
           PERFORM     F94XS THRU F94XS-FN.
       F67BF-FN. EXIT.
      *N67BH.    NOTE *LOOP THROUGH CX6Y UNTIL A MATCH    *.
       F67BH.                       GO TO     F67BH-B.                  lv20
       F67BH-A.
                 IF    CX6Y-CF = '1'
                 OR    IK = '1'
                                    GO TO     F67BH-FN.
       F67BH-B.
      *IS FOUND FOR THE DESTINATION
      *
      *N67BJ.    NOTE *IF THE READ IS SUCCESSFUL          *.
       F67BJ.    IF    IK = '0'                                         lv25
                 AND   CT01-CTID = CX6Y-CTID
                 AND   PJ14-CLID = CX6Y-CLID
                 NEXT SENTENCE ELSE GO TO     F67BJ-FN.
      *SAME DESTINATION &
      *SAME CLIENT
      *N67BL.    NOTE *ITS AN SD ARRANGEMENT              *.
       F67BL.    IF    CX6Y-CARTY = 10                                  lv30
                 NEXT SENTENCE ELSE GO TO     F67BL-FN.
      *N67BN.    NOTE *IF NOT POINTING TO THE CHANGING    *.
       F67BN.    IF    PJ14-NAPDS NOT = CX6Y-NAPDS                      lv35
                 AND   PJ14-CARTZF = CX6Y-CARTZ
                 NEXT SENTENCE ELSE GO TO     F67BN-FN.
      *ARRANGEMENT
      *
           PERFORM     F91BB THRU F91BB-FN.
       F67BN-900. GO TO F67CB-FN.
       F67BN-FN. EXIT.
      *N67CB.    NOTE *IF CX6Y READ POINTING TO THE ONE   *.
       F67CB.                                                           lv35
      *SELECTED TO CHANGE, READ TO SEE
      *IF ANY OTHER MATCHING CX6Y RECS
      *ARE FOUND
      *
           PERFORM     F94XR THRU F94XR-FN.
       F67CB-FN. EXIT.
       F67BL-900. GO TO F67DB-FN.
       F67BL-FN. EXIT.
      *N67DB.    NOTE *ITS NOT AN SD ARRANGEMENT,         *.
       F67DB.                                                           lv30
      *READ ANOTHER
      *
           PERFORM     F94XR THRU F94XR-FN.
       F67DB-FN. EXIT.
       F67BJ-900. GO TO F67DG-FN.
       F67BJ-FN. EXIT.
      *N67DG.    NOTE *IF CTID OR CLID DID NOT MATCH      *.
       F67DG.                                                           lv25
      *
           MOVE        '1' TO IK.
       F67DG-FN. EXIT.
       F67BH-900. GO TO F67BH-A.
       F67BH-FN. EXIT.
      *N67JB.    NOTE *IF AN SD WAS FOUND                 *.
       F67JB.    IF    CX6Y-CF = '1'                                    lv20
                 NEXT SENTENCE ELSE GO TO     F67JB-FN.
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        013685 TO MS03-NMESS2                            ADU119
           PERFORM     F98MX THRU F98MX-FN.                             ADU119
       F67JB-FN. EXIT.
       F67BD-FN. EXIT.
       F67BB-FN. EXIT.
       F67-FN.   EXIT.
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
      *               *PERFORMED FUNCTIONS                *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91BB.    NOTE *IF A MATCH IS FOUND, THEN CHECK    *.
       F91BB.                                                           lv10
      *THE STATUS OF THE ARRANGEMENT
      *
      *N91BF.    NOTE *READ CX13 FOR THE STATUS OF THE    *.
       F91BF.                                                           lv15
      *ARRANGEMENT FOUND
      *
      ** MOVE CX01 KEYS *
      *
           MOVE        ZERO TO CX01-CX01K
           MOVE        CX6Y-CLID TO S-CXU01-CLID
      *
      ** MOVE CX03 KEYS *
      *
           MOVE        ZERO TO CX03-CX03K
           MOVE        CX6Y-CARTY TO S-CXU03-CARTY
           MOVE        CX6Y-NARRS TO S-CXU03-NARRS
      *
      ** MOVE CX06 KEYS *
      *
           MOVE        ZERO TO CX06-CX06K
           MOVE        CX6Y-CTID1 TO S-CXU06-CTID
      *
      ** MOVE CX13 KEYS *
      *
           MOVE        ZERO TO CX13-CX13K
           MOVE        CX6Y-CARTZ TO S-CXU13-CARTZ
           MOVE        CX6Y-NAPDS TO S-CXU13-NAPDS
           MOVE        '>=' TO S-CXU13-OPER
           PERFORM     F94C3 THRU F94C3-FN.
      *N91BH.    NOTE *IF A MATCH WAS FOUND & IF STATUS   *.
       F91BH.    IF    IK = '0'                                         lv20
                 AND   CX13-CDEST = '03'
                 NEXT SENTENCE ELSE GO TO     F91BH-FN.
      *OF THE ARR IS
      *--> INACTIVE DO NOT MESSAGE &
      *READ NEXT TO FIND ANOTHER MATCH
      *
           PERFORM     F94XR THRU F94XR-FN.
       F91BH-900. GO TO F91BJ-FN.
       F91BH-FN. EXIT.
      *N91BJ.    NOTE *IF STATUS IS ACTIVE OR PENDING     *.
       F91BJ.                                                           lv20
      *
           MOVE        '1' TO CX6Y-CF.
       F91BJ-FN. EXIT.
       F91BF-FN. EXIT.
       F91BB-FN. EXIT.
       F91-FN.   EXIT.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *PACBASE TABLE ACCESS ROUTINES      *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92EB.    NOTE *ERROR ON TABLE READ FOR TB5B       *.
       F92EB.                                                           lv10
           MOVE        '1' TO TA5B-IK.
       F92EB-FN. EXIT.
      *N92TA.    NOTE *RANDOM TABLE READ FOR TA5B         *.            ADUTAB
       F92TA.                                                           lv10
           MOVE        'R1' TO G-TA5B-TABFO                             ADUTAB
           COMPUTE     G-TA5B-LTH = 60 + G-TA5B-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA5B-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA5B)                                ADUTAB
                       LENGTH (G-TA5B-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA5B-TABCR NOT = '00'                          DOT
           PERFORM     F92EB THRU F92EB-FN.                             ADUTAB
       F92TA-FN. EXIT.
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
      *N94CT.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94CT.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PE06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PE06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CT-FN. EXIT.
      *N94CU.    NOTE *CALL GN ON CT13                    *.            ADU026
       F94CU.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PE06 CT13                                                    ADU026
           S-CTU01-SSA S-CT13-SSA                                       ADU026
           MOVE        PE06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CU-FN. EXIT.
      *N94C3.    NOTE *CALL GU ON CX13                    *.            ADU026
       F94C3.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C3-FN. EXIT.
      *N94C4.    NOTE *CALL GN ON CX13                    *.            ADU026
       F94C4.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CX13-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C4-FN. EXIT.
      *N94C6.    NOTE *CALL GU ON CX06                    *.            ADU026
       F94C6.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX06                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C6-FN. EXIT.
      *N94C8.    NOTE *CALL GU ON CX14                    *.            ADU026
       F94C8.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX14' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX14                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           S-CXU14-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C8-FN. EXIT.
      *N94XR.    NOTE *CALL GN ON CX6Y                    *.            ADU026
       F94XR.                                                           lv10
           MOVE        'AREY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX6Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PN06 CX6Y                                                    ADU026
           S-CXU6Y-SSA                                                  ADU026
           MOVE        PN06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94XR-FN. EXIT.
      *N94XS.    NOTE *CALL GU ON CX6Y                    *.            ADU026
       F94XS.                                                           lv10
           MOVE        'AREY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX6Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PN06 CX6Y                                                    ADU026
           S-CXU6Y-SSA                                                  ADU026
           MOVE        PN06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94XS-FN. EXIT.
      *N94X2.    NOTE *CALL GU ON CX2Y                    *.            ADU026
       F94X2.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 CX2Y                                                    ADU026
           S-CXU2Y-SSA                                                  ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94X2-FN. EXIT.
      *N94YN.    NOTE *CALL GN ON CX14                    *.            ADU026
       F94YN.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX14' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CX14                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           S-CX14-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94YN-FN. EXIT.
      *N94Y2.    NOTE *CALL GN ON CX2Y                    *.            ADU026
       F94Y2.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PB06 CX2Y                                                    ADU026
           S-CX2Y-SSA                                                   ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94Y2-FN. EXIT.
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *PERFORMED EDITING FUNCTIONS        *
      *               *                                   *
      *               *************************************.
       F95.           EXIT.                                             lv05
      *N95CP.    NOTE *CERTS SD CAN ONLY HAVE 1 BANK      *.
       F95CP.                                                           lv10
      **   DIRECT DEPOSIT DESTINATION
      **   GET CX14S
           MOVE        '>=' TO S-CXU14-OPER
           PERFORM     F94C8 THRU F94C8-FN.
      *N95CQ.    NOTE *LOOP THRU THE CX14S                *.
       F95CQ.                       GO TO     F95CQ-B.                  lv15
       F95CQ-A.
                 IF    WS-DIRECT-DEPOSIT-IND = 'Y'
                 OR    IK = '1'
                                    GO TO     F95CQ-FN.
       F95CQ-B.
      *TIL FLAG IS SET OR NO MORE 14S
      *N95CR.    NOTE *DIRECT DEPOSIT FOUND  (ACH)        *.
       F95CR.    IF    CX14-CDEL1 = 003                                 lv20
                 NEXT SENTENCE ELSE GO TO     F95CR-FN.
      *SO  ERROR OUT
           MOVE        'Y' TO WS-DIRECT-DEPOSIT-IND
           MOVE        013158 TO MS03-NMESS2
           PERFORM     F98MX THRU F98MX-FN
           PERFORM     F94YN THRU F94YN-FN.
       F95CR-900. GO TO F95CT-FN.
       F95CR-FN. EXIT.
      *N95CT.    NOTE *ELSE    READ NEXT CX14             *.
       F95CT.                                                           lv20
           PERFORM     F94YN THRU F94YN-FN.
       F95CT-FN. EXIT.
       F95CQ-900. GO TO F95CQ-A.
       F95CQ-FN. EXIT.
       F95CP-FN. EXIT.
      *N95FA.    NOTE *GET CERT ACCOUNT VALUE             *.
       F95FA.                                                           lv10
      *HOLD CODE DATA & OPTION TYPE
           INITIALIZE  MS03.
      *N95FC.    NOTE *---> Call CI0135                   *.            AM0135
       F95FC.                                                           lv15
      *     Get Cert Account Info                                       AM0135
      *                                                                 AM0135
           INITIALIZE  JP02                                             AM0135
           DE10-DU03                                                    AM0135
           MOVE        ST01-CTID TO JP02-CTID                           AM0135
           SET CI0135-PCB-CH1P-PTR1 TO                                  AM0135
                      PCB-CH1P-PTR1                                     AM0135
           SET CI0135-PCB-CCRP-PTR1 TO                                  AM0135
                      PCB-CCRP-PTR1                                     AM0135
           SET CI0135-PCB-CPRP-PTR1 TO                                  AM0135
                      PCB-CPRP-PTR1                                     AM0135
           SET CI0135-PCB-CBTP-PTR1 TO                                  AM0135
                      PCB-CBTP-PTR1                                     AM0135
           SET CI0135-PCB-CA1P-PTR1 TO                                  AM0135
                      PCB-CA1P-PTR1                                     AM0135
           CALL        CI0135 USING                                     AM0135
           DFHEIBLK                                                     AM0135
           DFHCOMMAREA                                                  AM0135
           DLIUIBII                                                     AM0135
           CI0135-PCB-ADDRESS-LIST                                      AM0135
           JP02                                                         AM0135
           DE10                                                         AM0135
           MS03                                                         AM0135
           MX11.                                                        AM0135
      *N95FM.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F95FM.    IF    (MS03-NMESS2 > ZERO                              lv20
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F95FM-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0135 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0135 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F95FM-900. GO TO F95FN-FN.
       F95FM-FN. EXIT.
      *N95FN.    NOTE *NO ERRORS                          *.            ADU071
       F95FN.                                                           lv20
           INITIALIZE  MS03.                                            ADU071
       F95FN-FN. EXIT.
       F95FC-FN. EXIT.
       F95FA-FN. EXIT.
      *N95JC.    NOTE *DETERMINE IF MULT CX14S EXIST      *.
       F95JC.                                                           lv10
      **   READ TO THE CX13 THEN READ
      **   FOR CX14S
           MOVE        PJ14-CLID TO S-CXU01-CLID
           MOVE        PJ14-CARTYK TO S-CXU03-CARTY
           MOVE        PJ14-NARRSK TO S-CXU03-NARRS
           MOVE        PJ14-CTID01 TO S-CXU06-CTID
           MOVE        PJ14-CARTZF TO S-CXU13-CARTZ
           MOVE        PJ14-NAPDS TO S-CXU13-NAPDS
      **
           PERFORM     F94C3 THRU F94C3-FN.
      *N95JE.    NOTE *IF CX13 FND - READ FOR 1ST CX14    *.
       F95JE.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F95JE-FN.
           PERFORM     F94YN THRU F94YN-FN.
      *N95JH.    NOTE *IF CX14 FND (GN) FOR MORE CX14S    *.
       F95JH.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F95JH-FN.
           PERFORM     F94YN THRU F94YN-FN.
      *N95JJ.    NOTE *IF 2ND CX14 FOUND - ERROR          *.
       F95JJ.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F95JJ-FN.
      *
           MOVE        13161 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F95JJ-FN. EXIT.
       F95JH-FN. EXIT.
       F95JE-FN. EXIT.
       F95JC-FN. EXIT.
       F95-FN.   EXIT.
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
