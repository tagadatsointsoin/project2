       IDENTIFICATION DIVISION.                                         CI0090
       PROGRAM-ID.  CI0090P.                                            CI0090
      *AUTHOR.         CLIENT ARR DISB ACCOUNT ACCESS.                  CI0090
      *DATE-COMPILED.   09/08/14.                                       CI0090
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
       ENVIRONMENT DIVISION.                                            CI0090
       CONFIGURATION SECTION.                                           CI0090
       SOURCE-COMPUTER. IBM-370.                                        CI0090
       OBJECT-COMPUTER. IBM-370.                                        CI0090
       DATA DIVISION.                                                   CI0090
       WORKING-STORAGE SECTION.                                         CI0090
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0093           PIC X(8) VALUE 'CI0093P '.                  AM0093
       01                 CX00.                                         CI0090
            02            CX01.                                         CI0090
            10            CX01-CX01K.                                   CI0090
            11            CX01-C199.                                    CI0090
            12            CX01-CLID.                                    CI0090
            13            CX01-CLIDO  PICTURE  9(3).                    CI0090
            13            CX01-CLIDN.                                   CI0090
            14            CX01-CLIDNP PICTURE  X(12).                   CI0090
            14            CX01-CLIDND PICTURE  9(8).                    CI0090
            10            CX01-GEMDA  PICTURE  9(8).                    CI0090
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0090
                          BINARY.                                       CI0090
            10            CX01-FILLER PICTURE  X(5).                    CI0090
            02            CX03.                                         CI0090
            10            CX03-GELL   PICTURE  9(4)                     CI0090
                          BINARY.                                       CI0090
            10            CX03-CY00.                                    CI0090
            11            CX03-CX03K.                                   CI0090
            12            CX03-CARTY  PICTURE  99.                      CI0090
            12            CX03-NARRS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX03-CARST  PICTURE  99.                      CI0090
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX03-CPMTG  PICTURE  99.                      CI0090
            11            CX03-GRCRNG PICTURE  9(3).                    CI0090
            11            CX03-DEXDT  PICTURE  9(8).                    CI0090
            11            CX03-DASUP  PICTURE  9(8).                    CI0090
            11            CX03-CSTEC  PICTURE  X(3).                    CI0090
            11            CX03-FILLER PICTURE  X(17).                   CI0090
            11            CX03-CY50.                                    CI0090
            12            CX03-NARID  PICTURE  X(30).                   CI0090
            11            CX03-CY51                                     CI0090
                          REDEFINES            CX03-CY50.               CI0090
            12            CX03-NDIDN  PICTURE  9(12).                   CI0090
            12            CX03-FILLER PICTURE  X(18).                   CI0090
            11            CX03-CY52                                     CI0090
                          REDEFINES            CX03-CY50.               CI0090
            12            CX03-NAIDC  PICTURE  9(12).                   CI0090
            12            CX03-FILLER PICTURE  X(18).                   CI0090
            11            CX03-CY53                                     CI0090
                          REDEFINES            CX03-CY50.               CI0090
            12            CX03-NAMEXB PICTURE  9(15).                   CI0090
            12            CX03-FILLER PICTURE  X(15).                   CI0090
            10            CX03-CY99.                                    CI0090
            11            CX03-FILLER PICTURE  X(109).                  CI0090
            10            CX03-CY01                                     CI0090
                          REDEFINES            CX03-CY99.               CI0090
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX03-ICPCI  PICTURE  X.                       CI0090
            11            CX03-CLUPD  PICTURE  9(3).                    CI0090
            11            CX03-DLAUP  PICTURE  9(8).                    CI0090
            11            CX03-CWRC   PICTURE  99.                      CI0090
            11            CX03-CHCR   PICTURE  99.                      CI0090
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0090
            11            CX03-GEAUN  PICTURE  9(5).                    CI0090
            11            CX03-DPCHD  PICTURE  9(8).                    CI0090
            11            CX03-DLRCHK PICTURE  9(8).                    CI0090
            11            CX03-QTRCHK PICTURE  9(2).                    CI0090
            11            CX03-DNPMT  PICTURE  9(8).                    CI0090
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            CX03-CY02                                     CI0090
                          REDEFINES            CX03-CY99.               CI0090
            11            CX03-QSIRQ  PICTURE  99.                      CI0090
            11            CX03-QDRMN  PICTURE  9(2)                     CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX03-DDPRE  PICTURE  9(8).                    CI0090
            11            CX03-DDSHP  PICTURE  9(8).                    CI0090
            11            CX03-NDRFTB PICTURE  9(5).                    CI0090
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0090
            11            CX03-DDSHPA PICTURE  9(8).                    CI0090
            11            CX03-NDRFTF PICTURE  9(5).                    CI0090
            11            CX03-QDIPBK PICTURE  9(3).                    CI0090
            11            CX03-CREOR  PICTURE  X(1).                    CI0090
            11            CX03-CREOR1 PICTURE  X(1).                    CI0090
            11            CX03-DDASC  PICTURE  9(8).                    CI0090
            11            CX03-FILLER PICTURE  X(7).                    CI0090
            10            CX03-CY03                                     CI0090
                          REDEFINES            CX03-CY99.               CI0090
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0090
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0090
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0090
            11            CX03-DOPDA  PICTURE  99.                      CI0090
            11            CX03-CPMTF  PICTURE  99.                      CI0090
            11            CX03-CIRMO  PICTURE  X(12).                   CI0090
            11            CX03-CPALL  PICTURE  X(1).                    CI0090
            11            CX03-CCOLM  PICTURE  9(2).                    CI0090
            11            CX03-CBLTP  PICTURE  X(1).                    CI0090
            11            CX03-CASUB  PICTURE  9(2).                    CI0090
            11            CX03-CBLFM  PICTURE  9(2).                    CI0090
            11            CX03-IBILS  PICTURE  X.                       CI0090
            11            CX03-IPAOS  PICTURE  X.                       CI0090
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0090
            11            CX03-DLBPD  PICTURE  9(8).                    CI0090
            11            CX03-DNBPD  PICTURE  9(8).                    CI0090
            11            CX03-DODBD  PICTURE  9(8).                    CI0090
            11            CX03-CPSRE  PICTURE  99.                      CI0090
            11            CX03-ISPHN  PICTURE  X.                       CI0090
            11            CX03-TCARR  PICTURE  X(6).                    CI0090
            11            CX03-CBKPT  PICTURE  9(2).                    CI0090
            11            CX03-IECNT  PICTURE  X.                       CI0090
            11            CX03-ICONV  PICTURE  X(1).                    CI0090
            11            CX03-FILLER PICTURE  X(4).                    CI0090
            10            CX03-CY04                                     CI0090
                          REDEFINES            CX03-CY99.               CI0090
            11            CX03-CCARD  PICTURE  X(02).                   CI0090
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0090
            11            CX03-IREMT  PICTURE  X(01).                   CI0090
            11            CX03-ISBILA PICTURE  X.                       CI0090
            11            CX03-DLBPDA PICTURE  9(8).                    CI0090
            11            CX03-DNBPDA.                                  CI0090
            12            CX03-DNCYM  PICTURE  9(6).                    CI0090
            12            CX03-CEDTD  PICTURE  9(2).                    CI0090
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX03-DREMT  PICTURE  9(8).                    CI0090
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0090
            11            CX03-CWRC2  PICTURE  99.                      CI0090
            11            CX03-CHCR2  PICTURE  99.                      CI0090
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0090
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0090
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0090
            02            CX06.                                         CI0090
            10            CX06-CX06K.                                   CI0090
            11            CX06-C299.                                    CI0090
            12            CX06-CTID.                                    CI0090
            13            CX06-CTIDA  PICTURE  9(3).                    CI0090
            13            CX06-CTIDN.                                   CI0090
            14            CX06-CTIDNP PICTURE  X(13).                   CI0090
            14            CX06-CTIDND PICTURE  9(11).                   CI0090
            10            CX06-NPECK  PICTURE  9(02).                   CI0090
            10            CX06-FILLER PICTURE  X.                       CI0090
            02            CX09.                                         CI0090
            10            CX09-CX09K.                                   CI0090
            11            CX09-NPAIS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            CX09-CDEL1  PICTURE  9(3).                    CI0090
            10            CX09-NDELS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            CX09-CDEST  PICTURE  99.                      CI0090
            10            CX09-DISUP  PICTURE  9(8).                    CI0090
            10            CX09-CLUPD  PICTURE  9(3).                    CI0090
            10            CX09-DLAUP  PICTURE  9(8).                    CI0090
            10            CX09-GEOPD2 PICTURE  X(8).                    CI0090
            10            CX09-DPCHD  PICTURE  9(8).                    CI0090
            10            CX09-FILLER PICTURE  X(06).                   CI0090
            02            CX13.                                         CI0090
            10            CX13-GELL   PICTURE  9(4)                     CI0090
                          BINARY.                                       CI0090
            10            CX13-CY20.                                    CI0090
            11            CX13-CX13K.                                   CI0090
            12            CX13-CARTZ  PICTURE  99.                      CI0090
            12            CX13-NAPDS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX13-GESTD  PICTURE  9(8).                    CI0090
            11            CX13-GEEND  PICTURE  9(8).                    CI0090
            11            CX13-DASUQ  PICTURE  9(8).                    CI0090
            11            CX13-CDEST  PICTURE  99.                      CI0090
            11            CX13-IIARR  PICTURE  X.                       CI0090
            11            CX13-DLAUP  PICTURE  9(8).                    CI0090
            11            CX13-GEOPD2 PICTURE  X(8).                    CI0090
            11            CX13-GEAUN  PICTURE  9(5).                    CI0090
            11            CX13-DPCHD  PICTURE  9(8).                    CI0090
            11            CX13-PPOT1  PICTURE  S9(3)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX13-ACOT1  PICTURE  S9(9)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX13-QPST1  PICTURE  S9(7)V999                CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX13-FILLER PICTURE  X(03).                   CI0090
            10            CX13-CY96.                                    CI0090
            11            CX13-FILLER PICTURE  X(50).                   CI0090
            10            CX13-CY21                                     CI0090
                          REDEFINES            CX13-CY96.               CI0090
            11            CX13-DNPMT  PICTURE  9(8).                    CI0090
            11            CX13-CPMTF  PICTURE  99.                      CI0090
            11            CX13-ADBRQ  PICTURE  S9(11)V99                CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX13-QSHOWQ PICTURE  S9(9)V999                CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX13-PACT1  PICTURE  S999V999                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX13-DOPDA  PICTURE  99.                      CI0090
            11            CX13-DNEXE  PICTURE  9(8).                    CI0090
            11            CX13-CIRMO  PICTURE  X(12).                   CI0090
            10            CX13-CY98.                                    CI0090
            11            CX13-FILLER PICTURE  X(120).                  CI0090
            10            CX13-CY25                                     CI0090
                          REDEFINES            CX13-CY98.               CI0090
            11            CX13-COPTC  PICTURE  9(1).                    CI0090
            11            CX13-ILPOI  PICTURE  X(1).                    CI0090
            11            CX13-CATOC  PICTURE  X(1).                    CI0090
            11            CX13-CEOIA  PICTURE  S9(7)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX13-ACOAR  PICTURE  S9(9)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX13-CEOTR  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX13-DSTMO  PICTURE  99.                      CI0090
            10            CX13-CY27                                     CI0090
                          REDEFINES            CX13-CY98.               CI0090
            11            CX13-QMTH1  PICTURE  9(3).                    CI0090
            11            CX13-IDRMD  PICTURE  X.                       CI0090
            10            CX13-CY28                                     CI0090
                          REDEFINES            CX13-CY98.               CI0090
            11            CX13-AALLBL PICTURE  S9(8)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX13-PSURR  PICTURE  S9(3)V999                CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX13-DFPMT  PICTURE  9(8).                    CI0090
            11            CX13-QMTHLA PICTURE  9(3).                    CI0090
            11            CX13-PWHLDS PICTURE  S999V9(5)                CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX13-ISWHO  PICTURE  X(1).                    CI0090
            10            CX13-CY29                                     CI0090
                          REDEFINES            CX13-CY98.               CI0090
            11            CX13-IINDI1 PICTURE  X(1).                    CI0090
            11            CX13-IINDI2 PICTURE  X(1).                    CI0090
            11            CX13-IINDI3 PICTURE  X(1).                    CI0090
            11            CX13-PWHLD5 PICTURE  S999V99                  CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX13-CCSMQ  PICTURE  X.                       CI0090
            11            CX13-CPLEC  PICTURE  XX.                      CI0090
            11            CX13-IPTRDA PICTURE  X(01).                   CI0090
            11            CX13-GCUSPY PICTURE  X(12).                   CI0090
            11            CX13-ALOIDA PICTURE  S9(11)V99                CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX13-DELOI  PICTURE  9(8).                    CI0090
            11            CX13-CLGND  PICTURE  X.                       CI0090
            11            CX13-CORTYA PICTURE  X(3).                    CI0090
            11            CX13-CPH3U  PICTURE  X.                       CI0090
            11            CX13-CNAVR  PICTURE  X(1).                    CI0090
            11            CX13-NEXEC  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            02            CX14.                                         CI0090
            10            CX14-GELL   PICTURE  9(4)                     CI0090
                          BINARY.                                       CI0090
            10            CX14-CX14K.                                   CI0090
            11            CX14-NPISQ  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            CX14-ACOTD  PICTURE  S9(9)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            CX14-PPOTD  PICTURE  S9(3)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            CX14-QPSTD  PICTURE  S9(7)V999                CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            CX14-CPITC  PICTURE  99.                      CI0090
            10            CX14-FILLER PICTURE  X(04).                   CI0090
            10            CX14-CY97.                                    CI0090
            11            CX14-FILLER PICTURE  X(32).                   CI0090
            10            CX14-CY30                                     CI0090
                          REDEFINES            CX14-CY97.               CI0090
            11            CX14-IOWNC  PICTURE  X.                       CI0090
            11            CX14-CTYPE  PICTURE  X.                       CI0090
            11            CX14-C299.                                    CI0090
            12            CX14-CTID.                                    CI0090
            13            CX14-CTIDA  PICTURE  9(3).                    CI0090
            13            CX14-CTIDN.                                   CI0090
            14            CX14-CTIDNP PICTURE  X(13).                   CI0090
            14            CX14-CTIDND PICTURE  9(11).                   CI0090
            11            CX14-CPMTC  PICTURE  99.                      CI0090
            11            CX14-IACSD  PICTURE  X.                       CI0090
            10            CX14-CY31                                     CI0090
                          REDEFINES            CX14-CY97.               CI0090
            11            CX14-FILLER PICTURE  X(2).                    CI0090
            11            CX14-IDELI  PICTURE  X.                       CI0090
            11            CX14-CDEL1  PICTURE  9(3).                    CI0090
            11            CX14-NDELS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            CX14-CY32                                     CI0090
                          REDEFINES            CX14-CY97.               CI0090
            11            CX14-GCUSPZ PICTURE  X(12).                   CI0090
            02            CX18.                                         CI0090
            10            CX18-CX18K.                                   CI0090
            11            CX18-NBASQ  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            CX18-NPBN   PICTURE  X(20).                   CI0090
            10            CX18-CCBAT  PICTURE  99.                      CI0090
            10            CX18-DACHP  PICTURE  9(8).                    CI0090
            10            CX18-CSTPRE PICTURE  99.                      CI0090
            10            CX18-C199.                                    CI0090
            11            CX18-CLID.                                    CI0090
            12            CX18-CLIDO  PICTURE  9(3).                    CI0090
            12            CX18-CLIDN.                                   CI0090
            13            CX18-CLIDNP PICTURE  X(12).                   CI0090
            13            CX18-CLIDND PICTURE  9(8).                    CI0090
            10            CX18-MCSIG  PICTURE  X(30).                   CI0090
            10            CX18-CPBNU  PICTURE  X.                       CI0090
            10            CX18-CSPCR  PICTURE  99.                      CI0090
            10            CX18-DAPCR  PICTURE  9(8).                    CI0090
            10            CX18-FILLER PICTURE  XX.                      CI0090
            02            CX21.                                         CI0090
            10            CX21-GELL   PICTURE  9(4)                     CI0090
                          BINARY.                                       CI0090
            10            CX21-CZ00.                                    CI0090
            11            CX21-CX21K.                                   CI0090
            12            CX21-CDEL1  PICTURE  9(3).                    CI0090
            12            CX21-NDELS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            CX21-CZ99.                                    CI0090
            11            CX21-FILLER PICTURE  X(165).                  CI0090
            10            CX21-CZ01                                     CI0090
                          REDEFINES            CX21-CZ99.               CI0090
            11            CX21-NBASQ  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX21-GECSQ  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            CX21-CZ02                                     CI0090
                          REDEFINES            CX21-CZ99.               CI0090
            11            CX21-CPAYE  PICTURE  9(2).                    CI0090
            11            CX21-C199.                                    CI0090
            12            CX21-CLID.                                    CI0090
            13            CX21-CLIDO  PICTURE  9(3).                    CI0090
            13            CX21-CLIDN.                                   CI0090
            14            CX21-CLIDNP PICTURE  X(12).                   CI0090
            14            CX21-CLIDND PICTURE  9(8).                    CI0090
            11            CX21-GECSQ1 PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX21-NBASQT PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            CX21-TDELI  PICTURE  X(30).                   CI0090
      *                                                                 ADU155
      ******************************************************************ADU155
      ** WORK AREA NEEDED FOR MACRO ADU155                             *ADU155
      **        DATE COMMON AREA FOR EXECUTING CICS ASKTIME/FORMATTIME *ADU155
      ******************************************************************ADU155
      *                                                                 ADU155
      *!WI pl=DD100                                                     ADU155
       01  DD01-XMSTS                                                   ADU155
                        PICTURE S9(15)                                  CI0090
                          COMPUTATIONAL-3.                              CI0090
       01  DD01-F2CCYY             PIC S9(08) COMP.                     ADU155
      *!WI pl=DD200                                                     ADU155
       01  DD01-XDAT69                                                  ADU155
                        PICTURE 9(6).                                   CI0090
       01  DD01-UDATE.                                                  ADU155
           05  DD01-YEAR           PIC  9(04).                          ADU155
           05  DD01-MMDD           PIC  9(04).                          ADU155
      *!WI pl=DD280                                                     ADU155
       01  DD01-XDATCU REDEFINES DD01-UDATE                             ADU155
                        PICTURE X(8).                                   CI0090
      *                                                                 ADU155
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0090
            10            XW05-XW06.                                    CI0090
            11            XW05-XDBPCB.                                  CI0090
            12            XW05-XDBDNM PICTURE  X(08)                    CI0090
                          VALUE                SPACE.                   CI0090
            12            XW05-XSEGLV PICTURE  X(02)                    CI0090
                          VALUE                SPACE.                   CI0090
            12            XW05-XRC    PICTURE  X(02)                    CI0090
                          VALUE                SPACE.                   CI0090
            12            XW05-XPROPT PICTURE  X(04)                    CI0090
                          VALUE                SPACE.                   CI0090
            12            XW05-FILLER PICTURE  S9(5)                    CI0090
                          VALUE                ZERO                     CI0090
                          BINARY.                                       CI0090
            12            XW05-XSEGNM PICTURE  X(08)                    CI0090
                          VALUE                SPACE.                   CI0090
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0090
                          VALUE                ZERO                     CI0090
                          BINARY.                                       CI0090
            12            XW05-XSEGNB PICTURE  9(05)                    CI0090
                          VALUE                ZERO                     CI0090
                          BINARY.                                       CI0090
            12            XW05-XCOKEY PICTURE  X(70)                    CI0090
                          VALUE                SPACE.                   CI0090
            10            XW05-XW07.                                    CI0090
            11            XW05-XIOPCB.                                  CI0090
            12            XW05-XTERMI PICTURE  X(08)                    CI0090
                          VALUE                SPACE.                   CI0090
            12            XW05-FILLER PICTURE  XX                       CI0090
                          VALUE                SPACE.                   CI0090
            12            XW05-XRC1   PICTURE  X(02)                    CI0090
                          VALUE                SPACE.                   CI0090
            12            XW05-FILLER PICTURE  X(12)                    CI0090
                          VALUE                SPACE.                   CI0090
            12            XW05-XMODNM PICTURE  X(8)                     CI0090
                          VALUE                SPACE.                   CI0090
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0090
                          VALUE                ZERO.                    CI0090
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0090
                          VALUE                ZERO.                    CI0090
            10            XW05-XGU    PICTURE  X(4)                     CI0090
                          VALUE                'GU  '.                  CI0090
            10            XW05-XGHU   PICTURE  X(4)                     CI0090
                          VALUE                'GHU '.                  CI0090
            10            XW05-XGN    PICTURE  X(4)                     CI0090
                          VALUE                'GN  '.                  CI0090
            10            XW05-XGHN   PICTURE  X(4)                     CI0090
                          VALUE                'GHN '.                  CI0090
            10            XW05-XGNP   PICTURE  X(4)                     CI0090
                          VALUE                'GNP '.                  CI0090
            10            XW05-XGHNP  PICTURE  X(4)                     CI0090
                          VALUE                'GHNP'.                  CI0090
            10            XW05-XREPL  PICTURE  XXXX                     CI0090
                          VALUE                'REPL'.                  CI0090
            10            XW05-XISRT  PICTURE  X(4)                     CI0090
                          VALUE                'ISRT'.                  CI0090
            10            XW05-XDLET  PICTURE  X(4)                     CI0090
                          VALUE                'DLET'.                  CI0090
            10            XW05-XOPEN  PICTURE  X(4)                     CI0090
                          VALUE                'OPEN'.                  CI0090
            10            XW05-XCLSE  PICTURE  X(4)                     CI0090
                          VALUE                'CLSE'.                  CI0090
            10            XW05-XCHKP  PICTURE  X(4)                     CI0090
                          VALUE                'CHKP'.                  CI0090
            10            XW05-XXRST  PICTURE  X(4)                     CI0090
                          VALUE                'XRST'.                  CI0090
            10            XW05-XTERM  PICTURE  X(4)                     CI0090
                          VALUE                'TERM'.                  CI0090
            10            XW05-XNFPAC PICTURE  X(13)                    CI0090
                          VALUE                SPACE.                   CI0090
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0090
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0090
      ******************************************************************ADU035
      ***      CORPORATE INTERFACE APPLICATION CONTROL BLOCK - UPDATE **ADU035
      ******************************************************************ADU035
         COPY DBI3006F.                                                 ADU035
      ******************************************************************ADU035
      *** COPYBOOKS FOR THE TBL-HDR AND PASS-AREA FOR PERTINENT FIELDS  ADU035
      ******************************************************************ADU035
         COPY CIMD28PE.                                                 ADU035
         COPY CIMD28PI.                                                 ADU035
      *                                                                 ADU035
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
      *-----> PCB address list for calling module CI0093                AM0093
      *       Requires access to CM01, CM18 and GQ01                    AM0093
       01               CI0093-PCB-ADDRESS-LIST.                        AM0093
            05          CI0093-PCB-CA1P-PTR1        POINTER.            AM0093
            05          CI0093-PCB-TR1P-PTR1        POINTER.            AM0093
      *>>>>>> COPY SEGMENTS FOR CALLING CI0093

      *!WF DSP=PQ DSL=PJ SEL=22 FOR=I DES=1 LEV=1 PLT=PQ
       01                 PQ22.                                         CI0090
            10            PQ22-CTID   PICTURE  X(27).                   CI0090
            10            PQ22-CPMTF  PICTURE  99.                      CI0090
            10            PQ22-CEIAM  PICTURE  9(1).                    CI0090
            10            PQ22-FILLER PICTURE  X(99).                   CI0090
      *>>>>>> WORKING STORAGE AREA FOR CONVERTING FIELDS TO COPYBOOK
      *       FORMAT...
        01              WS01.
      *!WE
              05        WS01-GESQ2C
                        PICTURE 99.                                     CI0090
              05        WS01-XESQ2C  REDEFINES WS01-GESQ2C PIC X(02).
      *
       01   DEBUT-WSS.                                                  CI0090
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0090
            05   IK     PICTURE X.                                      CI0090
       01  CONSTANTES-PAC.                                              CI0090
           05  FILLER  PICTURE X(87)   VALUE                            CI0090
                     '6015 CAT09/08/14CI0090ADMIN   14:34:43CI0090P AMERCI0090
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0090
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0090
           05  NUGNA   PICTURE X(5).                                    CI0090
           05  APPLI   PICTURE X(3).                                    CI0090
           05  DATGN   PICTURE X(8).                                    CI0090
           05  PROGR   PICTURE X(6).                                    CI0090
           05  CODUTI  PICTURE X(8).                                    CI0090
           05  TIMGN   PICTURE X(8).                                    CI0090
           05  PROGE   PICTURE X(8).                                    CI0090
           05  COBASE  PICTURE X(4).                                    CI0090
           05  DATGNC  PICTURE X(10).                                   CI0090
           05  RELEAS  PICTURE X(7).                                    CI0090
           05  DATGE   PICTURE X(10).                                   CI0090
           05  DATSQ   PICTURE X(10).                                   CI0090
       01  DATCE.                                                       CI0090
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0090
         05  DATOR.                                                     CI0090
           10  DATOA  PICTURE XX.                                       CI0090
           10  DATOM  PICTURE XX.                                       CI0090
           10  DATOJ  PICTURE XX.                                       CI0090
       01   VARIABLES-CONDITIONNELLES.                                  CI0090
            05                  FT      PICTURE X VALUE '0'.            CI0090
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0090
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0090
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0090
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0090
       01               S-CX01-SSA.                                     CI0090
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0090
                                      VALUE 'CX01    '.                 CI0090
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0090
            10          S-CX01-CCOD   PICTURE X(5)                      CI0090
                                      VALUE '-----'.                    CI0090
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0090
       01            S-CXU01-SSA.                                       CI0090
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX01    '.                 CI0090
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0090
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(CX01K'.                   CI0090
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0090
            10       S-CXU01-CX01K.                                     CI0090
            11       S-CXU01-C199.                                      CI0090
            12       S-CXU01-CLID.                                      CI0090
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0090
            13       S-CXU01-CLIDN.                                     CI0090
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0090
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0090
            10  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01               S-CX03-SSA.                                     CI0090
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0090
                                      VALUE 'CX03    '.                 CI0090
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0090
            10          S-CX03-CCOD   PICTURE X(5)                      CI0090
                                      VALUE '-----'.                    CI0090
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0090
       01            S-CXA03-SSA.                                       CI0090
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX03    '.                 CI0090
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0090
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(CARTY'.                   CI0090
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0090
            12       S-CXA03-CARTY    PICTURE  99.                      CI0090
            12  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CXB03-SSA.                                       CI0090
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX03    '.                 CI0090
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0090
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(NARRS'.                   CI0090
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0090
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            12  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CXC03-SSA.                                       CI0090
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX03    '.                 CI0090
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0090
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(CPMTG'.                   CI0090
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0090
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0090
            11  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CXD03-SSA.                                       CI0090
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX03    '.                 CI0090
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0090
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(GRCRNG'.                  CI0090
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0090
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0090
            11  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CXE03-SSA.                                       CI0090
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX03    '.                 CI0090
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0090
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(DEXDT'.                   CI0090
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0090
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0090
            11  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CXF03-SSA.                                       CI0090
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX03    '.                 CI0090
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0090
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(CY50'.                    CI0090
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0090
            11       S-CXF03-CY50.                                      CI0090
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0090
            11  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CXG03-SSA.                                       CI0090
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX03    '.                 CI0090
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0090
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(NBASQ'.                   CI0090
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0090
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CXH03-SSA.                                       CI0090
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX03    '.                 CI0090
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0090
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(NARID'.                   CI0090
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0090
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0090
            12  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CXU03-SSA.                                       CI0090
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX03    '.                 CI0090
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0090
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(CX03K'.                   CI0090
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0090
            11       S-CXU03-CX03K.                                     CI0090
            12       S-CXU03-CARTY    PICTURE  99.                      CI0090
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01               S-CX06-SSA.                                     CI0090
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0090
                                      VALUE 'CX06    '.                 CI0090
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0090
            10          S-CX06-CCOD   PICTURE X(5)                      CI0090
                                      VALUE '-----'.                    CI0090
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0090
       01            S-CXU06-SSA.                                       CI0090
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX06    '.                 CI0090
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0090
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(CX06K'.                   CI0090
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0090
            10       S-CXU06-CX06K.                                     CI0090
            11       S-CXU06-C299.                                      CI0090
            12       S-CXU06-CTID.                                      CI0090
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0090
            13       S-CXU06-CTIDN.                                     CI0090
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0090
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0090
            10  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01               S-CX09-SSA.                                     CI0090
            10         S1-CX09-SEGNAM PICTURE X(8)                      CI0090
                                      VALUE 'CX09    '.                 CI0090
            10         S1-CX09-CCOM   PICTURE X VALUE '*'.              CI0090
            10          S-CX09-CCOD   PICTURE X(5)                      CI0090
                                      VALUE '-----'.                    CI0090
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0090
       01            S-CXU09-SSA.                                       CI0090
            10      S1-CXU09-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX09    '.                 CI0090
            10      S1-CXU09-CCOM   PICTURE X VALUE '*'.                CI0090
            10       S-CXU09-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            10      S1-CXU09-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(CX09K'.                   CI0090
            10       S-CXU09-OPER  PICTURE XX VALUE ' ='.               CI0090
            10       S-CXU09-CX09K.                                     CI0090
            11       S-CXU09-NPAIS    PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01               S-CX13-SSA.                                     CI0090
            10         S1-CX13-SEGNAM PICTURE X(8)                      CI0090
                                      VALUE 'CX13    '.                 CI0090
            10         S1-CX13-CCOM   PICTURE X VALUE '*'.              CI0090
            10          S-CX13-CCOD   PICTURE X(5)                      CI0090
                                      VALUE '-----'.                    CI0090
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0090
       01            S-CXA13-SSA.                                       CI0090
            11      S1-CXA13-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX13    '.                 CI0090
            11      S1-CXA13-CCOM   PICTURE X VALUE '*'.                CI0090
            11       S-CXA13-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            11      S1-CXA13-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(CDEST'.                   CI0090
            11       S-CXA13-OPER  PICTURE XX VALUE ' ='.               CI0090
            11       S-CXA13-CDEST    PICTURE  99.                      CI0090
            11  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CXB13-SSA.                                       CI0090
            12      S1-CXB13-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX13    '.                 CI0090
            12      S1-CXB13-CCOM   PICTURE X VALUE '*'.                CI0090
            12       S-CXB13-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            12      S1-CXB13-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(CARTZ'.                   CI0090
            12       S-CXB13-OPER  PICTURE XX VALUE ' ='.               CI0090
            12       S-CXB13-CARTZ    PICTURE  99.                      CI0090
            12  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CXC13-SSA.                                       CI0090
            12      S1-CXC13-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX13    '.                 CI0090
            12      S1-CXC13-CCOM   PICTURE X VALUE '*'.                CI0090
            12       S-CXC13-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            12      S1-CXC13-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(NAPDS'.                   CI0090
            12       S-CXC13-OPER  PICTURE XX VALUE ' ='.               CI0090
            12       S-CXC13-NAPDS    PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            12  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CXU13-SSA.                                       CI0090
            11      S1-CXU13-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX13    '.                 CI0090
            11      S1-CXU13-CCOM   PICTURE X VALUE '*'.                CI0090
            11       S-CXU13-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            11      S1-CXU13-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(CX13K'.                   CI0090
            11       S-CXU13-OPER  PICTURE XX VALUE ' ='.               CI0090
            11       S-CXU13-CX13K.                                     CI0090
            12       S-CXU13-CARTZ    PICTURE  99.                      CI0090
            12       S-CXU13-NAPDS    PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CX113-SSA.                                       CI0090
            11      S1-CX113-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX13    '.                 CI0090
            11      S1-CX113-CCOM   PICTURE X VALUE '*'.                CI0090
            11       S-CX113-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            11      S1-CX113-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(XGCUSPY'.                 CI0090
            11       S-CX113-OPER  PICTURE XX VALUE ' ='.               CI0090
            11       S-CX113-GCUSPY   PICTURE  X(12).                   CI0090
            11  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01               S-CX14-SSA.                                     CI0090
            10         S1-CX14-SEGNAM PICTURE X(8)                      CI0090
                                      VALUE 'CX14    '.                 CI0090
            10         S1-CX14-CCOM   PICTURE X VALUE '*'.              CI0090
            10          S-CX14-CCOD   PICTURE X(5)                      CI0090
                                      VALUE '-----'.                    CI0090
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0090
       01            S-CXU14-SSA.                                       CI0090
            10      S1-CXU14-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX14    '.                 CI0090
            10      S1-CXU14-CCOM   PICTURE X VALUE '*'.                CI0090
            10       S-CXU14-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            10      S1-CXU14-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(CX14K'.                   CI0090
            10       S-CXU14-OPER  PICTURE XX VALUE ' ='.               CI0090
            10       S-CXU14-CX14K.                                     CI0090
            11       S-CXU14-NPISQ    PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CX114-SSA.                                       CI0090
            11      S1-CX114-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX14    '.                 CI0090
            11      S1-CX114-CCOM   PICTURE X VALUE '*'.                CI0090
            11       S-CX114-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            11      S1-CX114-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(XGCUSPZ'.                 CI0090
            11       S-CX114-OPER  PICTURE XX VALUE ' ='.               CI0090
            11       S-CX114-GCUSPZ   PICTURE  X(12).                   CI0090
            11  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01               S-CX18-SSA.                                     CI0090
            10         S1-CX18-SEGNAM PICTURE X(8)                      CI0090
                                      VALUE 'CX18    '.                 CI0090
            10         S1-CX18-CCOM   PICTURE X VALUE '*'.              CI0090
            10          S-CX18-CCOD   PICTURE X(5)                      CI0090
                                      VALUE '-----'.                    CI0090
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0090
       01            S-CXA18-SSA.                                       CI0090
            10      S1-CXA18-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX18    '.                 CI0090
            10      S1-CXA18-CCOM   PICTURE X VALUE '*'.                CI0090
            10       S-CXA18-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            10      S1-CXA18-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(CSTPRE'.                  CI0090
            10       S-CXA18-OPER  PICTURE XX VALUE ' ='.               CI0090
            10       S-CXA18-CSTPRE   PICTURE  99.                      CI0090
            10  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CXB18-SSA.                                       CI0090
            10      S1-CXB18-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX18    '.                 CI0090
            10      S1-CXB18-CCOM   PICTURE X VALUE '*'.                CI0090
            10       S-CXB18-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            10      S1-CXB18-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(CSPCR'.                   CI0090
            10       S-CXB18-OPER  PICTURE XX VALUE ' ='.               CI0090
            10       S-CXB18-CSPCR    PICTURE  99.                      CI0090
            10  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CXU18-SSA.                                       CI0090
            10      S1-CXU18-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX18    '.                 CI0090
            10      S1-CXU18-CCOM   PICTURE X VALUE '*'.                CI0090
            10       S-CXU18-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            10      S1-CXU18-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(CX18K'.                   CI0090
            10       S-CXU18-OPER  PICTURE XX VALUE ' ='.               CI0090
            10       S-CXU18-CX18K.                                     CI0090
            11       S-CXU18-NBASQ    PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01               S-CX21-SSA.                                     CI0090
            10         S1-CX21-SEGNAM PICTURE X(8)                      CI0090
                                      VALUE 'CX21    '.                 CI0090
            10         S1-CX21-CCOM   PICTURE X VALUE '*'.              CI0090
            10          S-CX21-CCOD   PICTURE X(5)                      CI0090
                                      VALUE '-----'.                    CI0090
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0090
       01            S-CXA21-SSA.                                       CI0090
            11      S1-CXA21-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX21    '.                 CI0090
            11      S1-CXA21-CCOM   PICTURE X VALUE '*'.                CI0090
            11       S-CXA21-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            11      S1-CXA21-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(GECSQ1'.                  CI0090
            11       S-CXA21-OPER  PICTURE XX VALUE ' ='.               CI0090
            11       S-CXA21-GECSQ1   PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01            S-CXU21-SSA.                                       CI0090
            11      S1-CXU21-SEGNAM PICTURE X(8)                        CI0090
                                      VALUE 'CX21    '.                 CI0090
            11      S1-CXU21-CCOM   PICTURE X VALUE '*'.                CI0090
            11       S-CXU21-CCOD   PICTURE X(5)                        CI0090
                                      VALUE '-----'.                    CI0090
            11      S1-CXU21-FLDNAM PICTURE X(9)                        CI0090
                                      VALUE '(CX21K'.                   CI0090
            11       S-CXU21-OPER  PICTURE XX VALUE ' ='.               CI0090
            11       S-CXU21-CX21K.                                     CI0090
            12       S-CXU21-CDEL1    PICTURE  9(3).                    CI0090
            12       S-CXU21-NDELS    PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11  FILLER   PICTURE X    VALUE ')'.                        CI0090
       01   ZONES-UTILISATEUR PICTURE X.                                CI0090
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
      ** PCB POINTER FOR CA1P                                           ADU015
            05 PCB-CA1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR TR1P                                           ADU015
            05 PCB-TR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0090
          05              PA00-SUITE.                                   CI0090
            15       FILLER         PICTURE  X(00106).                  CI0090
       01                 PA06  REDEFINES      PA00.                    CI0090
            10            PA06-XDBPCB.                                  CI0090
            11            PA06-XDBDNM PICTURE  X(08).                   CI0090
            11            PA06-XSEGLV PICTURE  X(02).                   CI0090
            11            PA06-XRC    PICTURE  X(02).                   CI0090
            11            PA06-XPROPT PICTURE  X(04).                   CI0090
            11            PA06-FILLER PICTURE  S9(5)                    CI0090
                          BINARY.                                       CI0090
            11            PA06-XSEGNM PICTURE  X(08).                   CI0090
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0090
                          BINARY.                                       CI0090
            11            PA06-XSEGNB PICTURE  9(05)                    CI0090
                          BINARY.                                       CI0090
            11            PA06-XCOKEY PICTURE  X(70).                   CI0090
      *** PCB MASK FOR CA1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0090
          05              PB00-SUITE.                                   CI0090
            15       FILLER         PICTURE  X(00106).                  CI0090
       01                 PB06  REDEFINES      PB00.                    CI0090
            10            PB06-XDBPCB.                                  CI0090
            11            PB06-XDBDNM PICTURE  X(08).                   CI0090
            11            PB06-XSEGLV PICTURE  X(02).                   CI0090
            11            PB06-XRC    PICTURE  X(02).                   CI0090
            11            PB06-XPROPT PICTURE  X(04).                   CI0090
            11            PB06-FILLER PICTURE  S9(5)                    CI0090
                          BINARY.                                       CI0090
            11            PB06-XSEGNM PICTURE  X(08).                   CI0090
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0090
                          BINARY.                                       CI0090
            11            PB06-XSEGNB PICTURE  9(05)                    CI0090
                          BINARY.                                       CI0090
            11            PB06-XCOKEY PICTURE  X(70).                   CI0090
      *** PCB MASK FOR TR1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0090
          05              PC00-SUITE.                                   CI0090
            15       FILLER         PICTURE  X(00106).                  CI0090
       01                 PC06  REDEFINES      PC00.                    CI0090
            10            PC06-XDBPCB.                                  CI0090
            11            PC06-XDBDNM PICTURE  X(08).                   CI0090
            11            PC06-XSEGLV PICTURE  X(02).                   CI0090
            11            PC06-XRC    PICTURE  X(02).                   CI0090
            11            PC06-XPROPT PICTURE  X(04).                   CI0090
            11            PC06-FILLER PICTURE  S9(5)                    CI0090
                          BINARY.                                       CI0090
            11            PC06-XSEGNM PICTURE  X(08).                   CI0090
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0090
                          BINARY.                                       CI0090
            11            PC06-XSEGNB PICTURE  9(05)                    CI0090
                          BINARY.                                       CI0090
            11            PC06-XCOKEY PICTURE  X(70).                   CI0090
      *>>>>>> UNIQUE SEGMENTS TO THIS ROUTINE
      *!WF DSP=PJ DSL=PJ SEL=08 FOR=I DES=1 LEV=1 PLT=75
       01                 PJ08.                                         CI0090
            10            PJ08-MAPPN  PICTURE  X(10).                   CI0090
            10            PJ08-CLID   PICTURE  X(23).                   CI0090
            10            PJ08-CARTY  PICTURE  99.                      CI0090
            10            PJ08-NARRS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            PJ08-CTID   PICTURE  X(27).                   CI0090
            10            PJ08-CARTZ  PICTURE  99.                      CI0090
            10            PJ08-NAPDS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            PJ08-NPISQ  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            PJ08-NBASQ  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            PJ08-CDEL1  PICTURE  9(3).                    CI0090
            10            PJ08-NDELS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            PJ08-IPERT  PICTURE  X.                       CI0090
            10            PJ08-NEIBT  PICTURE  X(7).                    CI0090
            10            PJ08-GESQ2C PICTURE  S99                      CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            PJ08-CACTM  PICTURE  X(1).                    CI0090
            10            PJ08-CPROT  PICTURE  X(02).                   CI0090
            10            PJ08-DCACG  PICTURE  9(8).                    CI0090
            10            PJ08-GECSQ  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            PJ08-ICX01  PICTURE  X.                       CI0090
            10            PJ08-ICX03  PICTURE  X.                       CI0090
            10            PJ08-ICX06  PICTURE  X.                       CI0090
            10            PJ08-ICX13  PICTURE  X.                       CI0090
            10            PJ08-ICX14  PICTURE  X.                       CI0090
            10            PJ08-ICX18  PICTURE  X.                       CI0090
            10            PJ08-ICX21  PICTURE  X.                       CI0090
            10            PJ08-CPMTF  PICTURE  99.                      CI0090
            10            PJ08-NPAIS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            PJ08-ICX09  PICTURE  X.                       CI0090
            10            PJ08-FILLER PICTURE  X(22).                   CI0090
      *!WF DSP=LX DSL=CX SEL=01030609 FOR=I DES=1 LEV=1
      * PLT=75
       01                 LX01.                                         CI0090
            10            LX01-CX01K.                                   CI0090
            11            LX01-C199.                                    CI0090
            12            LX01-CLID.                                    CI0090
            13            LX01-CLIDO  PICTURE  9(3).                    CI0090
            13            LX01-CLIDN.                                   CI0090
            14            LX01-CLIDNP PICTURE  X(12).                   CI0090
            14            LX01-CLIDND PICTURE  9(8).                    CI0090
            10            LX01-GEMDA  PICTURE  9(8).                    CI0090
            10            LX01-NSEQ4B PICTURE  9(8)                     CI0090
                          BINARY.                                       CI0090
            10            LX01-FILLER PICTURE  X(5).                    CI0090
       01                 LX03.                                         CI0090
            10            LX03-GELL   PICTURE  9(4)                     CI0090
                          BINARY.                                       CI0090
            10            LX03-CY00.                                    CI0090
            11            LX03-CX03K.                                   CI0090
            12            LX03-CARTY  PICTURE  99.                      CI0090
            12            LX03-NARRS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX03-CARST  PICTURE  99.                      CI0090
            11            LX03-GECSQ  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX03-CPMTG  PICTURE  99.                      CI0090
            11            LX03-GRCRNG PICTURE  9(3).                    CI0090
            11            LX03-DEXDT  PICTURE  9(8).                    CI0090
            11            LX03-DASUP  PICTURE  9(8).                    CI0090
            11            LX03-CSTEC  PICTURE  X(3).                    CI0090
            11            LX03-FILLER PICTURE  X(17).                   CI0090
            11            LX03-CY50.                                    CI0090
            12            LX03-NARID  PICTURE  X(30).                   CI0090
            11            LX03-CY51                                     CI0090
                          REDEFINES            LX03-CY50.               CI0090
            12            LX03-NDIDN  PICTURE  9(12).                   CI0090
            12            LX03-FILLER PICTURE  X(18).                   CI0090
            11            LX03-CY52                                     CI0090
                          REDEFINES            LX03-CY50.               CI0090
            12            LX03-NAIDC  PICTURE  9(12).                   CI0090
            12            LX03-FILLER PICTURE  X(18).                   CI0090
            11            LX03-CY53                                     CI0090
                          REDEFINES            LX03-CY50.               CI0090
            12            LX03-NAMEXB PICTURE  9(15).                   CI0090
            12            LX03-FILLER PICTURE  X(15).                   CI0090
            10            LX03-CY99.                                    CI0090
            11            LX03-FILLER PICTURE  X(109).                  CI0090
            10            LX03-CY01                                     CI0090
                          REDEFINES            LX03-CY99.               CI0090
            11            LX03-NBASQ  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX03-ICPCI  PICTURE  X.                       CI0090
            11            LX03-CLUPD  PICTURE  9(3).                    CI0090
            11            LX03-DLAUP  PICTURE  9(8).                    CI0090
            11            LX03-CWRC   PICTURE  99.                      CI0090
            11            LX03-CHCR   PICTURE  99.                      CI0090
            11            LX03-GEOPD2 PICTURE  X(8).                    CI0090
            11            LX03-GEAUN  PICTURE  9(5).                    CI0090
            11            LX03-DPCHD  PICTURE  9(8).                    CI0090
            11            LX03-DLRCHK PICTURE  9(8).                    CI0090
            11            LX03-QTRCHK PICTURE  9(2).                    CI0090
            11            LX03-DNPMT  PICTURE  9(8).                    CI0090
            11            LX03-APMTLA PICTURE  S9(9)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            LX03-CY02                                     CI0090
                          REDEFINES            LX03-CY99.               CI0090
            11            LX03-QSIRQ  PICTURE  99.                      CI0090
            11            LX03-QDRMN  PICTURE  9(2)                     CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX03-DDPRE  PICTURE  9(8).                    CI0090
            11            LX03-DDSHP  PICTURE  9(8).                    CI0090
            11            LX03-NDRFTB PICTURE  9(5).                    CI0090
            11            LX03-QDIPBJ PICTURE  9(3).                    CI0090
            11            LX03-DDSHPA PICTURE  9(8).                    CI0090
            11            LX03-NDRFTF PICTURE  9(5).                    CI0090
            11            LX03-QDIPBK PICTURE  9(3).                    CI0090
            11            LX03-CREOR  PICTURE  X(1).                    CI0090
            11            LX03-CREOR1 PICTURE  X(1).                    CI0090
            11            LX03-DDASC  PICTURE  9(8).                    CI0090
            11            LX03-FILLER PICTURE  X(7).                    CI0090
            10            LX03-CY03                                     CI0090
                          REDEFINES            LX03-CY99.               CI0090
            11            LX03-DLAUP1 PICTURE  9(8).                    CI0090
            11            LX03-GEOPD3 PICTURE  X(8).                    CI0090
            11            LX03-DNPMT1 PICTURE  9(8).                    CI0090
            11            LX03-DOPDA  PICTURE  99.                      CI0090
            11            LX03-CPMTF  PICTURE  99.                      CI0090
            11            LX03-CIRMO  PICTURE  X(12).                   CI0090
            11            LX03-CPALL  PICTURE  X(1).                    CI0090
            11            LX03-CCOLM  PICTURE  9(2).                    CI0090
            11            LX03-CBLTP  PICTURE  X(1).                    CI0090
            11            LX03-CASUB  PICTURE  9(2).                    CI0090
            11            LX03-CBLFM  PICTURE  9(2).                    CI0090
            11            LX03-IBILS  PICTURE  X.                       CI0090
            11            LX03-IPAOS  PICTURE  X.                       CI0090
            11            LX03-CBLSQ  PICTURE  X(4).                    CI0090
            11            LX03-DLBPD  PICTURE  9(8).                    CI0090
            11            LX03-DNBPD  PICTURE  9(8).                    CI0090
            11            LX03-DODBD  PICTURE  9(8).                    CI0090
            11            LX03-CPSRE  PICTURE  99.                      CI0090
            11            LX03-ISPHN  PICTURE  X.                       CI0090
            11            LX03-TCARR  PICTURE  X(6).                    CI0090
            11            LX03-CBKPT  PICTURE  9(2).                    CI0090
            11            LX03-IECNT  PICTURE  X.                       CI0090
            11            LX03-ICONV  PICTURE  X(1).                    CI0090
            11            LX03-FILLER PICTURE  X(4).                    CI0090
            10            LX03-CY04                                     CI0090
                          REDEFINES            LX03-CY99.               CI0090
            11            LX03-CCARD  PICTURE  X(02).                   CI0090
            11            LX03-MCSIG4 PICTURE  X(20).                   CI0090
            11            LX03-IREMT  PICTURE  X(01).                   CI0090
            11            LX03-ISBILA PICTURE  X.                       CI0090
            11            LX03-DLBPDA PICTURE  9(8).                    CI0090
            11            LX03-DNBPDA.                                  CI0090
            12            LX03-DNCYM  PICTURE  9(6).                    CI0090
            12            LX03-CEDTD  PICTURE  9(2).                    CI0090
            11            LX03-AREMT  PICTURE  S9(7)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX03-DREMT  PICTURE  9(8).                    CI0090
            11            LX03-ADBRQ  PICTURE  S9(11)V99                CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX03-CLUPD1 PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX03-DLAUP3 PICTURE  9(8).                    CI0090
            11            LX03-CWRC2  PICTURE  99.                      CI0090
            11            LX03-CHCR2  PICTURE  99.                      CI0090
            11            LX03-GEOPD9 PICTURE  X(8).                    CI0090
            11            LX03-GEAUN1 PICTURE  9(5).                    CI0090
            11            LX03-DPCHD1 PICTURE  9(8).                    CI0090
       01                 LX06.                                         CI0090
            10            LX06-CX06K.                                   CI0090
            11            LX06-C299.                                    CI0090
            12            LX06-CTID.                                    CI0090
            13            LX06-CTIDA  PICTURE  9(3).                    CI0090
            13            LX06-CTIDN.                                   CI0090
            14            LX06-CTIDNP PICTURE  X(13).                   CI0090
            14            LX06-CTIDND PICTURE  9(11).                   CI0090
            10            LX06-NPECK  PICTURE  9(02).                   CI0090
            10            LX06-FILLER PICTURE  X.                       CI0090
       01                 LX09.                                         CI0090
            10            LX09-CX09K.                                   CI0090
            11            LX09-NPAIS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            LX09-CDEL1  PICTURE  9(3).                    CI0090
            10            LX09-NDELS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            LX09-CDEST  PICTURE  99.                      CI0090
            10            LX09-DISUP  PICTURE  9(8).                    CI0090
            10            LX09-CLUPD  PICTURE  9(3).                    CI0090
            10            LX09-DLAUP  PICTURE  9(8).                    CI0090
            10            LX09-GEOPD2 PICTURE  X(8).                    CI0090
            10            LX09-DPCHD  PICTURE  9(8).                    CI0090
            10            LX09-FILLER PICTURE  X(06).                   CI0090
       01                 LX13.                                         CI0090
            10            LX13-GELL   PICTURE  9(4)                     CI0090
                          BINARY.                                       CI0090
            10            LX13-CY20.                                    CI0090
            11            LX13-CX13K.                                   CI0090
            12            LX13-CARTZ  PICTURE  99.                      CI0090
            12            LX13-NAPDS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX13-GESTD  PICTURE  9(8).                    CI0090
            11            LX13-GEEND  PICTURE  9(8).                    CI0090
            11            LX13-DASUQ  PICTURE  9(8).                    CI0090
            11            LX13-CDEST  PICTURE  99.                      CI0090
            11            LX13-IIARR  PICTURE  X.                       CI0090
            11            LX13-DLAUP  PICTURE  9(8).                    CI0090
            11            LX13-GEOPD2 PICTURE  X(8).                    CI0090
            11            LX13-GEAUN  PICTURE  9(5).                    CI0090
            11            LX13-DPCHD  PICTURE  9(8).                    CI0090
            11            LX13-PPOT1  PICTURE  S9(3)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX13-ACOT1  PICTURE  S9(9)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX13-QPST1  PICTURE  S9(7)V999                CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX13-FILLER PICTURE  X(03).                   CI0090
            10            LX13-CY96.                                    CI0090
            11            LX13-FILLER PICTURE  X(50).                   CI0090
            10            LX13-CY21                                     CI0090
                          REDEFINES            LX13-CY96.               CI0090
            11            LX13-DNPMT  PICTURE  9(8).                    CI0090
            11            LX13-CPMTF  PICTURE  99.                      CI0090
            11            LX13-ADBRQ  PICTURE  S9(11)V99                CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX13-QSHOWQ PICTURE  S9(9)V999                CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX13-PACT1  PICTURE  S999V999                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX13-DOPDA  PICTURE  99.                      CI0090
            11            LX13-DNEXE  PICTURE  9(8).                    CI0090
            11            LX13-CIRMO  PICTURE  X(12).                   CI0090
            10            LX13-CY98.                                    CI0090
            11            LX13-FILLER PICTURE  X(120).                  CI0090
            10            LX13-CY25                                     CI0090
                          REDEFINES            LX13-CY98.               CI0090
            11            LX13-COPTC  PICTURE  9(1).                    CI0090
            11            LX13-ILPOI  PICTURE  X(1).                    CI0090
            11            LX13-CATOC  PICTURE  X(1).                    CI0090
            11            LX13-CEOIA  PICTURE  S9(7)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX13-ACOAR  PICTURE  S9(9)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX13-CEOTR  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX13-DSTMO  PICTURE  99.                      CI0090
            10            LX13-CY27                                     CI0090
                          REDEFINES            LX13-CY98.               CI0090
            11            LX13-QMTH1  PICTURE  9(3).                    CI0090
            11            LX13-IDRMD  PICTURE  X.                       CI0090
            10            LX13-CY28                                     CI0090
                          REDEFINES            LX13-CY98.               CI0090
            11            LX13-AALLBL PICTURE  S9(8)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX13-PSURR  PICTURE  S9(3)V999                CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX13-DFPMT  PICTURE  9(8).                    CI0090
            11            LX13-QMTHLA PICTURE  9(3).                    CI0090
            11            LX13-PWHLDS PICTURE  S999V9(5)                CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX13-ISWHO  PICTURE  X(1).                    CI0090
            10            LX13-CY29                                     CI0090
                          REDEFINES            LX13-CY98.               CI0090
            11            LX13-IINDI1 PICTURE  X(1).                    CI0090
            11            LX13-IINDI2 PICTURE  X(1).                    CI0090
            11            LX13-IINDI3 PICTURE  X(1).                    CI0090
            11            LX13-PWHLD5 PICTURE  S999V99                  CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX13-CCSMQ  PICTURE  X.                       CI0090
            11            LX13-CPLEC  PICTURE  XX.                      CI0090
            11            LX13-IPTRDA PICTURE  X(01).                   CI0090
            11            LX13-GCUSPY PICTURE  X(12).                   CI0090
            11            LX13-ALOIDA PICTURE  S9(11)V99                CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX13-DELOI  PICTURE  9(8).                    CI0090
            11            LX13-CLGND  PICTURE  X.                       CI0090
            11            LX13-CORTYA PICTURE  X(3).                    CI0090
            11            LX13-CPH3U  PICTURE  X.                       CI0090
            11            LX13-CNAVR  PICTURE  X(1).                    CI0090
            11            LX13-NEXEC  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
       01                 LX14.                                         CI0090
            10            LX14-GELL   PICTURE  9(4)                     CI0090
                          BINARY.                                       CI0090
            10            LX14-CX14K.                                   CI0090
            11            LX14-NPISQ  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            LX14-ACOTD  PICTURE  S9(9)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            LX14-PPOTD  PICTURE  S9(3)V99                 CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            LX14-QPSTD  PICTURE  S9(7)V999                CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            LX14-CPITC  PICTURE  99.                      CI0090
            10            LX14-FILLER PICTURE  X(04).                   CI0090
            10            LX14-CY97.                                    CI0090
            11            LX14-FILLER PICTURE  X(32).                   CI0090
            10            LX14-CY30                                     CI0090
                          REDEFINES            LX14-CY97.               CI0090
            11            LX14-IOWNC  PICTURE  X.                       CI0090
            11            LX14-CTYPE  PICTURE  X.                       CI0090
            11            LX14-C299.                                    CI0090
            12            LX14-CTID.                                    CI0090
            13            LX14-CTIDA  PICTURE  9(3).                    CI0090
            13            LX14-CTIDN.                                   CI0090
            14            LX14-CTIDNP PICTURE  X(13).                   CI0090
            14            LX14-CTIDND PICTURE  9(11).                   CI0090
            11            LX14-CPMTC  PICTURE  99.                      CI0090
            11            LX14-IACSD  PICTURE  X.                       CI0090
            10            LX14-CY31                                     CI0090
                          REDEFINES            LX14-CY97.               CI0090
            11            LX14-FILLER PICTURE  X(2).                    CI0090
            11            LX14-IDELI  PICTURE  X.                       CI0090
            11            LX14-CDEL1  PICTURE  9(3).                    CI0090
            11            LX14-NDELS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            LX14-CY32                                     CI0090
                          REDEFINES            LX14-CY97.               CI0090
            11            LX14-GCUSPZ PICTURE  X(12).                   CI0090
       01                 LX18.                                         CI0090
            10            LX18-CX18K.                                   CI0090
            11            LX18-NBASQ  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            LX18-NPBN   PICTURE  X(20).                   CI0090
            10            LX18-CCBAT  PICTURE  99.                      CI0090
            10            LX18-DACHP  PICTURE  9(8).                    CI0090
            10            LX18-CSTPRE PICTURE  99.                      CI0090
            10            LX18-C199.                                    CI0090
            11            LX18-CLID.                                    CI0090
            12            LX18-CLIDO  PICTURE  9(3).                    CI0090
            12            LX18-CLIDN.                                   CI0090
            13            LX18-CLIDNP PICTURE  X(12).                   CI0090
            13            LX18-CLIDND PICTURE  9(8).                    CI0090
            10            LX18-MCSIG  PICTURE  X(30).                   CI0090
            10            LX18-CPBNU  PICTURE  X.                       CI0090
            10            LX18-CSPCR  PICTURE  99.                      CI0090
            10            LX18-DAPCR  PICTURE  9(8).                    CI0090
            10            LX18-FILLER PICTURE  XX.                      CI0090
       01                 LX21.                                         CI0090
            10            LX21-GELL   PICTURE  9(4)                     CI0090
                          BINARY.                                       CI0090
            10            LX21-CZ00.                                    CI0090
            11            LX21-CX21K.                                   CI0090
            12            LX21-CDEL1  PICTURE  9(3).                    CI0090
            12            LX21-NDELS  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            LX21-CZ99.                                    CI0090
            11            LX21-FILLER PICTURE  X(165).                  CI0090
            10            LX21-CZ01                                     CI0090
                          REDEFINES            LX21-CZ99.               CI0090
            11            LX21-NBASQ  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX21-GECSQ  PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            LX21-CZ02                                     CI0090
                          REDEFINES            LX21-CZ99.               CI0090
            11            LX21-CPAYE  PICTURE  9(2).                    CI0090
            11            LX21-C199.                                    CI0090
            12            LX21-CLID.                                    CI0090
            13            LX21-CLIDO  PICTURE  9(3).                    CI0090
            13            LX21-CLIDN.                                   CI0090
            14            LX21-CLIDNP PICTURE  X(12).                   CI0090
            14            LX21-CLIDND PICTURE  9(8).                    CI0090
            11            LX21-GECSQ1 PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX21-NBASQT PICTURE  S9(3)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            LX21-TDELI  PICTURE  X(30).                   CI0090
      *!WF DSP=LX DSL=CX SEL=13141821 FOR=I DES=1 LEV=1
      * PLT=75
      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0090
          05              DE00-SUITE.                                   CI0090
            15       FILLER         PICTURE  X(00653).                  CI0090
       01                 DE10  REDEFINES      DE00.                    CI0090
            10            DE10-DU11.                                    CI0090
            11            DE10-XFONC  PICTURE  X(4).                    CI0090
            11            DE10-MPSBN  PICTURE  X(8).                    CI0090
            11            DE10-XDBDNM PICTURE  X(08).                   CI0090
            11            DE10-XSEGNM PICTURE  X(08).                   CI0090
            11            DE10-XRC    PICTURE  X(02).                   CI0090
            11            DE10-MSEG   PICTURE  X(08).                   CI0090
            11            DE10-XCOKEY PICTURE  X(70).                   CI0090
            11            DE10-CUIBR  PICTURE  X(01).                   CI0090
            11            DE10-CUIBA  PICTURE  X(01).                   CI0090
            11            DE10-IPBIK  PICTURE  X(1).                    CI0090
            10            DE10-DU03.                                    CI0090
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            DE10-CMSSF  PICTURE  XX.                      CI0090
            11            DE10-DU09.                                    CI0090
            12            DE10-CMESA  PICTURE  S9(9)                    CI0090
                          BINARY.                                       CI0090
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0090
                          BINARY.                                       CI0090
            12            DE10-CMESB  PICTURE  S9(9)                    CI0090
                          BINARY.                                       CI0090
            12            DE10-CMSST  PICTURE  S9(9)                    CI0090
                          BINARY.                                       CI0090
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0090
                          BINARY.                                       CI0090
            12            DE10-QELLAA PICTURE  S9(9)                    CI0090
                          BINARY.                                       CI0090
            12            DE10-TMESS4 PICTURE  X(512).                  CI0090
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0090
          05              MS00-SUITE.                                   CI0090
            15       FILLER         PICTURE  X(00542).                  CI0090
       01                 MS03  REDEFINES      MS00.                    CI0090
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            10            MS03-CMSSF  PICTURE  XX.                      CI0090
            10            MS03-DU09.                                    CI0090
            11            MS03-CMESA  PICTURE  S9(9)                    CI0090
                          BINARY.                                       CI0090
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0090
                          BINARY.                                       CI0090
            11            MS03-CMESB  PICTURE  S9(9)                    CI0090
                          BINARY.                                       CI0090
            11            MS03-CMSST  PICTURE  S9(9)                    CI0090
                          BINARY.                                       CI0090
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0090
                          BINARY.                                       CI0090
            11            MS03-QELLAA PICTURE  S9(9)                    CI0090
                          BINARY.                                       CI0090
            11            MS03-TMESS4 PICTURE  X(512).                  CI0090
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0090
            10            MX11-QMSGS  PICTURE  9(03).                   CI0090
            10            MX11-PJ09                                     CI0090
                          OCCURS       025     TIMES.                   CI0090
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0090
                          COMPUTATIONAL-3.                              CI0090
            11            MX11-CMESB  PICTURE  S9(9)                    CI0090
                          BINARY.                                       CI0090
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PJ08
                                LX01
                                LX03
                                LX06
                                LX09
                                LX13
                                LX14
                                LX18
                                LX21
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0090
      *               *                                   *             CI0090
      *               *INITIALISATIONS                    *             CI0090
      *               *                                   *             CI0090
      *               *************************************.            CI0090
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
      *N02CA.    NOTE *---> Return switches               *.
       F02CA.                                                           lv10
           MOVE        'N' TO PJ08-ICX01
           MOVE        'N' TO PJ08-ICX03
           MOVE        'N' TO PJ08-ICX06
           MOVE        'N' TO PJ08-ICX09
           MOVE        'N' TO PJ08-ICX13
           MOVE        'N' TO PJ08-ICX14
           MOVE        'N' TO PJ08-ICX21
           MOVE        'N' TO PJ08-ICX18.
       F02CA-FN. EXIT.
      *N02DA.    NOTE *INITIALIZE OUTPUT LINKAGE          *.
       F02DA.                                                           lv10
      *SEGMENTS
           INITIALIZE  LX01
           LX03
           LX06
           LX09
           LX13
           LX14
           LX18
           LX21.
       F02DA-FN. EXIT.
      *N02XA.    NOTE *SET POINTERS FOR DB ACCESS         *.
       F02XA.                                                           lv10
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CA1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-CA1P-PTR1.                                          ADU015
      *SET ADDRESS FOR TR1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-TR1P-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0090
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0090
      *               *                                   *             CI0090
      *               *FIN DE TRAITEMENT                  *             CI0090
      *               *                                   *             CI0090
      *               *************************************.            CI0090
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0090
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N50BB.    NOTE *---> Get Unique CX01               *.
       F50BB.                                                           lv10
      *
           MOVE        PJ08-CLID TO S-CXU01-CLID
           PERFORM     F94G1 THRU F94G1-FN.
      *N50BD.    NOTE *---> Get Unique CX03               *.
       F50BD.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50BD-FN.
      *     and
      *     Update CX01 Access
           MOVE        CX01 TO LX01
           MOVE        'Y' TO PJ08-ICX01
      *
           MOVE        PJ08-CARTY TO S-CXU03-CARTY
           MOVE        PJ08-NARRS TO S-CXU03-NARRS
           PERFORM     F94G2 THRU F94G2-FN.
      *N50BF.    NOTE *---> Get Unique CX06               *.
       F50BF.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F50BF-FN.
      *     and
      *     Update CX03 Access
           MOVE        CX03 TO LX03
           MOVE        'Y' TO PJ08-ICX03
      *
           MOVE        PJ08-CTID TO S-CXU06-CTID
           PERFORM     F94G3 THRU F94G3-FN.
      *N50BH.    NOTE *PROCESS ONLY FOR SD                *.
       F50BH.    IF    PJ08-MAPPN = 'SD'                                lv25
                 NEXT SENTENCE ELSE GO TO     F50BH-FN.
      *N50BI.    NOTE *---> GET UNIQUE CX13               *.
       F50BI.    IF    IK = '0'                                         lv30
                 NEXT SENTENCE ELSE GO TO     F50BI-FN.
      *     AND
      *     UPDATE CX06 ACCESS
           MOVE        CX06 TO LX06
           MOVE        'Y' TO PJ08-ICX06
      *
           MOVE        PJ08-CARTZ TO S-CXU13-CARTZ
           MOVE        PJ08-NAPDS TO S-CXU13-NAPDS
           PERFORM     F94G4 THRU F94G4-FN.
      *N50BJ.    NOTE *---> GET UNIQUE CX14               *.
       F50BJ.    IF    IK = '0'                                         lv35
                 NEXT SENTENCE ELSE GO TO     F50BJ-FN.
      * and update CX13 Access
      * and update CX14 Access
           MOVE        CX13 TO LX13
           MOVE        'Y' TO PJ08-ICX13
      *
           MOVE        PJ08-NPISQ TO S-CXU14-NPISQ
           PERFORM     F94G5 THRU F94G5-FN
      *
                 IF    IK = '0'                                         DOT
      *---> CX14 Access OK...
           MOVE        CX14 TO LX14
           MOVE        'Y' TO PJ08-ICX14.
      *---  Endif  ---                                                  DOT
       F50BJ-FN. EXIT.
       F50BI-FN. EXIT.
       F50BH-FN. EXIT.
      *N50BM.    NOTE *---> GET UNIQUE CX09               *.
       F50BM.    IF    IK = '0'                                         lv25
                 AND   (PJ08-MAPPN = 'UD'
                 OR    PJ08-MAPPN = 'FDC')
                 NEXT SENTENCE ELSE GO TO     F50BM-FN.
      *     AND
      *     UPDATE CX06 ACCESS
           MOVE        CX06 TO LX06
           MOVE        'Y' TO PJ08-ICX06
      *
           MOVE        PJ08-NPAIS TO S-CXU09-NPAIS
           PERFORM     F94G8 THRU F94G8-FN.
      *N50BN.    NOTE *  UPDATE CX09 ACCESS               *.
       F50BN.    IF    IK = '0'                                         lv30
                 NEXT SENTENCE ELSE GO TO     F50BN-FN.
           MOVE        CX09 TO LX09
           MOVE        'Y' TO PJ08-ICX09.
       F50BN-FN. EXIT.
       F50BM-FN. EXIT.
       F50BF-FN. EXIT.
       F50BD-FN. EXIT.
       F50BB-FN. EXIT.
      *N50CB.    NOTE *ACCESS CX21 WHEN PROCESSING SD     *.
       F50CB.    IF    PJ08-MAPPN = 'SD'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50CB-FN.
      *
      *N50CC.    NOTE *---> ACCESS A CX21 SEGMENT         *.
       F50CC.    IF    (PJ08-ICX14 = 'Y'                                lv15
                 AND   CX14-CPITC = 1)
                 OR    PJ08-NDELS NOT = ZERO
                 NEXT SENTENCE ELSE GO TO     F50CC-FN.
      *     TRANSFER...
      *     IF CX14 FOUND OR REQUESTED
      *
                 IF    PJ08-NDELS NOT = ZERO                            DOT
      *---> KEY HAS BEEN ENTERED...
           MOVE        PJ08-NDELS TO S-CXU21-NDELS
           MOVE        PJ08-CDEL1 TO S-CXU21-CDEL1
                 ELSE
      *---  OTHERWISE USE CX14 VALUE...
           MOVE        CX14-NDELS TO S-CXU21-NDELS
           MOVE        CX14-CDEL1 TO S-CXU21-CDEL1.
      *---  ENDIF  ---                                                  DOT
      *
      *---> READ THE CX21...
           PERFORM     F94G6 THRU F94G6-FN
      *
                 IF    IK = '0'                                         DOT
      *---> IF ACCESS OK
           MOVE        CX21 TO LX21
           MOVE        'Y' TO PJ08-ICX21.
      *---  ENDIF  ---                                                  DOT
       F50CC-FN. EXIT.
       F50CB-FN. EXIT.
      *N50CD.    NOTE *ACCESS CX21 WHEN PROCESSING UD     *.
       F50CD.    IF    PJ08-MAPPN = 'UD'                                lv10
                 OR    PJ08-MAPPN = 'FDC'
                 NEXT SENTENCE ELSE GO TO     F50CD-FN.
      *N50CF.    NOTE *---> ACCESS A CX21 SEGMENT         *.
       F50CF.    IF    (PJ08-ICX09 = 'Y'                                lv15
                 OR    PJ08-NDELS NOT = ZERO)
                 NEXT SENTENCE ELSE GO TO     F50CF-FN.
      *     TRANSFER...
      *     IF CX09 FOUND OR REQUESTED
      *
                 IF    PJ08-NDELS NOT = ZERO                            DOT
      *---> KEY HAS BEEN ENTERED...
           MOVE        PJ08-NDELS TO S-CXU21-NDELS
           MOVE        PJ08-CDEL1 TO S-CXU21-CDEL1
                 ELSE
      *---  OTHERWISE USE CX09 VALUE...
           MOVE        CX09-NDELS TO S-CXU21-NDELS
           MOVE        CX09-CDEL1 TO S-CXU21-CDEL1.
      *---  ENDIF  ---                                                  DOT
      *
      *---> READ THE CX21...
           PERFORM     F94G6 THRU F94G6-FN
      *
                 IF    IK = '0'                                         DOT
      *---> IF ACCESS OK
           MOVE        CX21 TO LX21
           MOVE        'Y' TO PJ08-ICX21.
      *---  ENDIF  ---                                                  DOT
       F50CF-FN. EXIT.
       F50CD-FN. EXIT.
      *N50DB.    NOTE *---> Access a CX18 Segment         *.
       F50DB.    IF    PJ08-ICX21 = 'Y'                                 lv10
                 OR    PJ08-NBASQ NOT = ZERO
                 NEXT SENTENCE ELSE GO TO     F50DB-FN.
      *     if CX21 found or requested
      *N50DD.    NOTE *---> Key has been entered...       *.
       F50DD.    IF    PJ08-NBASQ NOT = ZERO                            lv15
                 NEXT SENTENCE ELSE GO TO     F50DD-FN.
           MOVE        PJ08-NBASQ TO S-CXU18-NBASQ.
       F50DD-900. GO TO F50DF-FN.
       F50DD-FN. EXIT.
      *N50DF.    NOTE *---  Otherwise use CX21 value...   *.
       F50DF.                                                           lv15
                 IF    CX21-CDEL1 = 001                                 DOT
                 OR    CX21-CDEL1 = 003
      *---> CX21 found is that of
      *     WIRE OR ACH
           MOVE        CX21-NBASQ TO S-CXU18-NBASQ
                 ELSE
      *---  Otherwise use other key
           MOVE        CX21-NBASQT TO S-CXU18-NBASQ.
      *--- ENDIF ---                                                    DOT
       F50DF-FN. EXIT.
      *N50DG.    NOTE *---> Read the CX18...              *.
       F50DG.                                                           lv15
           PERFORM     F94G7 THRU F94G7-FN
      *
                 IF    IK = '0'                                         DOT
      *---> If access OK
           MOVE        CX18 TO LX18
           MOVE        'Y' TO PJ08-ICX18.
      *---  Endif  ---                                                  DOT
       F50DG-FN. EXIT.
       F50DB-FN. EXIT.
      *N51BA.    NOTE *---> Pertinance processing...      *.
       F51BA.    IF    PJ08-IPERT = 'Y'                                 lv10
                 AND   PJ08-MAPPN = 'SD'
                 NEXT SENTENCE ELSE GO TO     F51BA-FN.
      *---> This chunk of code will
      *     determine if the data
      *     previously has been
      *     marked pertiant by MD28...
      *     Verify if any changes have
      *     occured and remove the
      *     data from the DB2 tables...
      *N51CB.    NOTE *---> Build MD28 Copy Book          *.
       F51CB.                                                           lv15
      *     Initialize values...
           INITIALIZE  MD28-PASS-AREA
           MOVE        PJ08-NEIBT TO MD28-EIBTASKN
           MOVE        PJ08-GESQ2C TO WS01-GESQ2C
           MOVE        WS01-XESQ2C TO MD28-GESQ2C.
       F51CB-FN. EXIT.
      *N51CD.    NOTE *---> BUILD MD28 CX13 VALUES...     *.
       F51CD.    IF    PJ08-ICX13 = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F51CD-FN.
           MOVE        CX13-CDEST TO MD28-CDEST.
      *N51CH.    NOTE *---> Determine values or zeroes    *.
       F51CH.    IF    CX13-CARTZ = 05                                  lv20
                 OR    CX13-CARTZ = 06
                 NEXT SENTENCE ELSE GO TO     F51CH-FN.
           MOVE        ZEROES TO MD28-CPMTF
           MD28-DNPMT
           PJ08-CPMTF.
       F51CH-900. GO TO F51CK-FN.
       F51CH-FN. EXIT.
      *N51CK.    NOTE *---> Otherwise actual values       *.
       F51CK.                                                           lv20
           MOVE        CX13-CPMTF TO MD28-CPMTF
           MOVE        CX13-CPMTF TO PJ08-CPMTF
           MOVE        CX13-DNPMT TO MD28-DNPMT.
       F51CK-FN. EXIT.
       F51CD-FN. EXIT.
      *N51DB.    NOTE *---> Build MD28 CX14 values...     *.
       F51DB.    IF    PJ08-ICX14 = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F51DB-FN.
           MOVE        CX14-NPISQ TO MD28-NPISQ
           MOVE        CX14-ACOTD TO MD28-ACOTD
           MOVE        CX14-PPOTD TO MD28-PPOTD
           MOVE        CX14-QPSTD TO MD28-QPSTD
           MOVE        CX14-CPITC TO MD28-CPITC.
      *N51EA.    NOTE *IF SD IS TO EXTERNAL DESTINATON    *.
       F51EA.    IF    MD28-CPITC = 01                                  lv20
                 NEXT SENTENCE ELSE GO TO     F51EA-FN.
           MOVE        SPACE TO MD28-IOWNC
           MOVE        SPACE TO MD28-CTYPE
           MOVE        SPACES TO MD28-CTID01
           MOVE        ZEROS TO MD28-CPMTC
           MOVE        SPACE TO MD28-IACSD
           MOVE        CX14-IDELI TO MD28-IDELI
           MOVE        CX14-CDEL1 TO MD28-CDEL1
           MOVE        CX14-NDELS TO MD28-NDELS.
       F51EA-FN. EXIT.
      *N51EG.    NOTE *IF SD IS A TRANSFER                *.
       F51EG.    IF    MD28-CPITC = 02                                  lv20
                 NEXT SENTENCE ELSE GO TO     F51EG-FN.
           MOVE        CX14-IOWNC TO MD28-IOWNC
           MOVE        CX14-CTYPE TO MD28-CTYPE
           MOVE        CX14-CTID TO MD28-CTID01
           MOVE        CX14-CPMTC TO MD28-CPMTC
           MOVE        CX14-IACSD TO MD28-IACSD
           MOVE        SPACE TO MD28-IDELI
           MOVE        ZEROS TO MD28-CDEL1
           MOVE        ZEROS TO MD28-NDELS.
       F51EG-FN. EXIT.
       F51DB-900. GO TO F51EJ-FN.
       F51DB-FN. EXIT.
      *N51EJ.    NOTE *---> No CX14 found                 *.
       F51EJ.                                                           lv15
           MOVE ALL    '0' TO MD28-CPMTC
           MD28-CDEL1
           MD28-NDELS
           MD28-NPISQ
           MD28-CPITC.
       F51EJ-FN. EXIT.
      *N51HA.    NOTE *---> SPECIAL CERTS PROCESSING      *.
       F51HA.    IF    PJ08-ICX06 = 'Y'                                 lv15
                 AND   CX06-CTIDA = 001
                 AND   CX13-CARTZ = 06
                 NEXT SENTENCE ELSE GO TO     F51HA-FN.
      *Applicable to
      *IDSC Interest Arrangements only
           PERFORM     F92HP THRU F92HP-FN.
      *N51HQ.    NOTE *---> CHECK FOR BAD RETURN CODE     *.
       F51HQ.    IF    MS03-NMESS2 NOT = ZEROES                         lv20
                 OR    DE10-NMESS2 NOT = ZEROES
                 NEXT SENTENCE ELSE GO TO     F51HQ-FN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F51HQ-900. GO TO F51HS-FN.
       F51HQ-FN. EXIT.
      *N51HS.    NOTE *---> OTHERWISE UPDATE COPYBOOK     *.
       F51HS.                                                           lv20
           MOVE        ZEROES TO MD28-DNPMT
           MOVE        PQ22-CPMTF TO MD28-CPMTF
           PJ08-CPMTF.
       F51HS-FN. EXIT.
       F51HA-FN. EXIT.
      *N51LA.    NOTE *LOAD KEYS FOR UPDATE INTERFACE     *.
       F51LA.                                                           lv15
           MOVE        MD28-TBL-SIZE TO UPD-COL-PASS
           MOVE        PJ08-NEIBT TO UPD-EIBTASKN
           MOVE        PJ08-GESQ2C TO UPD-GESQ2C
           MOVE        'MD28' TO UPD-EIBTRNID.
                 IF    EIBTRNID (1:1) = 'X'                             DOT
           MOVE        'XD28' TO UPD-EIBTRNID.
       F51LA-FN. EXIT.
      *N51MB.    NOTE *---> PERFORM THE PERTIANANCE       *.
       F51MB.                                                           lv15
      *     CHECK
           PERFORM     F98UC THRU F98UC-FN.
       F51MB-FN. EXIT.
      *N51MD.    NOTE *---> PERTINANCE CHECK FAILED       *.
       F51MD.    IF    UPD-RETURN-LEVEL = 'E'                           lv15
                 NEXT SENTENCE ELSE GO TO     F51MD-FN.
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012758 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F51MD-FN. EXIT.
       F51BA-FN. EXIT.
      *N52.      NOTE *************************************.
      *               *                                   *
      *               *CERTIFICATE PAYMENT FREQUENCY      *
      *               *                                   *
      *               *************************************.
       F52.      IF    PJ08-MAPPN = 'SD'                                lv05
                 NEXT SENTENCE ELSE GO TO     F52-FN.
      *N52BB.    NOTE *Only execute when no pertinance    *.
       F52BB.    IF    PJ08-IPERT NOT = 'Y'                             lv10
                 AND   PJ08-ICX06 = 'Y'
                 AND   CX06-CTIDA = 001
                 AND   CX13-CARTZ = 06
                 NEXT SENTENCE ELSE GO TO     F52BB-FN.
      *and IDSC Interest Arrangement
      *  - CALL CI0093
           PERFORM     F92HP THRU F92HP-FN.
      *N52HQ.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F52HQ.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F52HQ-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0093 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0093 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F52HQ-900. GO TO F52HS-FN.
       F52HQ-FN. EXIT.
      *N52HS.    NOTE *NO ERRORS                          *.            ADU071
       F52HS.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
      *N52HT.    NOTE *IF NO ERRORS AFTER CALL            *.
       F52HT.                                                           lv20
      *                   TO CI0093
      *
           MOVE        PQ22-CPMTF TO PJ08-CPMTF.
       F52HT-FN. EXIT.
       F52HS-FN. EXIT.
       F52BB-FN. EXIT.
       F52-FN.   EXIT.
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
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *PERFORMED FUNCTIONS                *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92DA.    NOTE *FORMAT CURRENT DATE                *.
       F92DA.                                                           lv10
           EXEC CICS   ASKTIME ABSTIME (DD01-XMSTS)          END-EXEC.  ADU155
           EXEC CICS   FORMATTIME ABSTIME (DD01-XMSTS)                  ADU155
                       YYMMDD (DD01-XDAT69)                             ADU155
                       YEAR (DD01-F2CCYY)                    END-EXEC.  ADU155
           COMPUTE     DD01-YEAR = DD01-F2CCYY                          ADU155
      ** MOVE DD01-UDATE TO YOUR FIELD                                  ADU155
           MOVE        DD01-XDAT69 (3:4) TO DD01-MMDD.                  ADU155
       F92DA-FN. EXIT.
      *N92HP.    NOTE *---> Call CI0093 to get            *.            AM0093
       F92HP.                                                           lv10
      *     Cert Interest Payment Freq                                  AM0093
           INITIALIZE  PQ22                                             AM0093
           DE10-DU03                                                    AM0093
      *Input only requires Contract ID                                  AM0093
           MOVE        LX06-CTID TO PQ22-CTID                           AM0093
           SET CI0093-PCB-CA1P-PTR1 TO                                  AM0093
                      PCB-CA1P-PTR1                                     AM0093
           SET CI0093-PCB-TR1P-PTR1 TO                                  AM0093
                      PCB-TR1P-PTR1                                     AM0093
           CALL        CI0093 USING                                     AM0093
           DFHEIBLK                                                     AM0093
           DFHCOMMAREA                                                  AM0093
           DLIUIBII                                                     AM0093
           CI0093-PCB-ADDRESS-LIST                                      AM0093
           PQ22                                                         AM0093
           DE10                                                         AM0093
           MS03                                                         AM0093
           MX11.                                                        AM0093
      *---> End of macro AM0093                                         DOT
       F92HP-FN. EXIT.
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
      *N94G1.    NOTE *CALL GU ON CX01                    *.            ADU026
       F94G1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX01                                                    ADU026
           S-CXU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G1-FN. EXIT.
      *N94G2.    NOTE *CALL GU ON CX03                    *.            ADU026
       F94G2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX03                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G2-FN. EXIT.
      *N94G3.    NOTE *CALL GU ON CX06                    *.            ADU026
       F94G3.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX06                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G3-FN. EXIT.
      *N94G4.    NOTE *CALL GU ON CX13                    *.            ADU026
       F94G4.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G4-FN. EXIT.
      *N94G5.    NOTE *CALL GU ON CX14                    *.            ADU026
       F94G5.                                                           lv10
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
       F94G5-FN. EXIT.
      *N94G6.    NOTE *CALL GU ON CX21                    *.            ADU026
       F94G6.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX21' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX21                                                    ADU026
           S-CXU01-SSA S-CXU21-SSA                                      ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G6-FN. EXIT.
      *N94G7.    NOTE *CALL GU ON CX18                    *.            ADU026
       F94G7.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX18                                                    ADU026
           S-CXU01-SSA S-CXU18-SSA                                      ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G7-FN. EXIT.
      *N94G8.    NOTE *CALL GU ON CX09                    *.            ADU026
       F94G8.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX09' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX09                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU09-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G8-FN. EXIT.
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
      *N98UC.    NOTE *UPDATE INTERFACE - COMPARE         *.            ADU035
       F98UC.                                                           lv10
      *********************************                                 ADU035
      *COMPARE CALL - UPDATE INTERFACE                                  ADU035
      *********************************                                 ADU035
           MOVE        999 TO UPD-FUNCTION-CODE                         ADU035
           PERFORM     F98UI THRU F98UI-FN.                             ADU035
       F98UC-FN. EXIT.
      *N98UD.    NOTE *UPDATE INTERFACE - DELETE          *.            ADU035
       F98UD.                                                           lv10
      *********************************                                 ADU035
      *DELETE TASK NUMBERS - UPDATE                                     ADU035
      *INTERFACE                                                        ADU035
      *********************************                                 ADU035
           MOVE        050 TO UPD-FUNCTION-CODE                         ADU035
           PERFORM     F98UI THRU F98UI-FN.                             ADU035
       F98UD-FN. EXIT.
      *N98UI.    NOTE *CALL UPDATE INTERFACE DBI30060     *.            ADU035
       F98UI.                                                           lv10
      *********************************                                 ADU035
      *THIS ROUTINE CALLS THE CORPORATE                                 ADU035
      *UPDATE INTERFACE MODULE USED TO                                  ADU035
      *RETRIEVE AND COMPARE ROWS AND                                    ADU035
      *DETERMINE IF DATA HAS CHANGED.                                   ADU035
      *********************************                                 ADU035
           MOVE        'DBI30060' TO UPD-PROGRAM-NAME                   ADU035
           CALL        UPD-PROGRAM-NAME                                 ADU035
           USING                                                        ADU035
           DFHEIBLK                                                     ADU035
           DFHCOMMAREA                                                  ADU035
           UPD-CNTL-FIELDS                                              ADU035
           MD28-TBL-HDR                                                 ADU035
           MD28-PASS-AREA.                                              ADU035
       F98UI-FN. EXIT.
      *N98UT.    NOTE *UPDATE INTERFACE - TRAN ID         *.            ADU035
       F98UT.                                                           lv10
      *********************************                                 ADU035
      *TRAN ID CALL - UPDATE INTERFACE                                  ADU035
      *********************************                                 ADU035
           MOVE        010 TO UPD-FUNCTION-CODE                         ADU035
           PERFORM     F98UI THRU F98UI-FN.                             ADU035
       F98UT-FN. EXIT.
       F98-FN.   EXIT.
