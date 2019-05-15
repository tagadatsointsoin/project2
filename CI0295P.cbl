       IDENTIFICATION DIVISION.                                         CI0295
       PROGRAM-ID.  CI0295P.                                            CI0295
      *AUTHOR.         FA CHECK PENDING CERTS TRNSFR.                   CI0295
      *DATE-COMPILED.   09/08/14.                                       CI0295
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT                           *    ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE RPC    SYSTEM AND ALL INFORMATION RELATING THERETO,    *ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE RPC    SYSTEM AND ALL            *ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE RPC          *ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR.                                                  *    ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0295
       CONFIGURATION SECTION.                                           CI0295
       SOURCE-COMPUTER. IBM-370.                                        CI0295
       OBJECT-COMPUTER. IBM-370.                                        CI0295
       DATA DIVISION.                                                   CI0295
       WORKING-STORAGE SECTION.                                         CI0295
      ******************************************************************
      ** SWITCHES TO INDICATE WHETHER OR NOT A SEGMENT WAS FOUND       *
      ******************************************************************
       01  GC03-CF              PIC X  VALUE ZEROS.
       01  CX03-CF              PIC X  VALUE ZEROS.
       01  CX13-CF              PIC X  VALUE ZEROS.
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *!WF DSP=CT DSL=CT SEL=1V FOR=I DES=2 LEV=1 PLT=CT
       01                 CT1V.                                         CI0295
            10            CT1V-CTID   PICTURE  X(27)                    CI0295
                          VALUE                SPACE.                   CI0295
            10            CT1V-NPBN   PICTURE  X(20)                    CI0295
                          VALUE                SPACE.                   CI0295
            10            CT1V-DCACD7 PICTURE  X(10)                    CI0295
                          VALUE                SPACE.                   CI0295
            10            CT1V-CAACTH PICTURE  X                        CI0295
                          VALUE                SPACE.                   CI0295
            10            CT1V-APMTO1 PICTURE  S9(11)V99                CI0295
                          VALUE                ZERO                     CI0295
                          COMPUTATIONAL-3.                              CI0295
            10            CT1V-NTRSN3 PICTURE  S9(8)                    CI0295
                          VALUE                ZERO                     CI0295
                          COMPUTATIONAL-3.                              CI0295
            10            CT1V-CTBAC  PICTURE  X(03)                    CI0295
                          VALUE                SPACE.                   CI0295
            10            CT1V-DXTMST PICTURE  X(26)                    CI0295
                          VALUE                SPACE.                   CI0295
            10            CT1V-GEOPDC PICTURE  X(8)                     CI0295
                          VALUE                SPACE.                   CI0295
       01                 CX01.                                         CI0295
            10            CX01-CX01K.                                   CI0295
            11            CX01-C199.                                    CI0295
            12            CX01-CLID.                                    CI0295
            13            CX01-CLIDO  PICTURE  9(3).                    CI0295
            13            CX01-CLIDN.                                   CI0295
            14            CX01-CLIDNP PICTURE  X(12).                   CI0295
            14            CX01-CLIDND PICTURE  9(8).                    CI0295
            10            CX01-GEMDA  PICTURE  9(8).                    CI0295
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0295
                          BINARY.                                       CI0295
            10            CX01-FILLER PICTURE  X(5).                    CI0295
       01                 CX03.                                         CI0295
            10            CX03-GELL   PICTURE  9(4)                     CI0295
                          BINARY.                                       CI0295
            10            CX03-CY00.                                    CI0295
            11            CX03-CX03K.                                   CI0295
            12            CX03-CARTY  PICTURE  99.                      CI0295
            12            CX03-NARRS  PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX03-CARST  PICTURE  99.                      CI0295
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX03-CPMTG  PICTURE  99.                      CI0295
            11            CX03-GRCRNG PICTURE  9(3).                    CI0295
            11            CX03-DEXDT  PICTURE  9(8).                    CI0295
            11            CX03-DASUP  PICTURE  9(8).                    CI0295
            11            CX03-CSTEC  PICTURE  X(3).                    CI0295
            11            CX03-FILLER PICTURE  X(17).                   CI0295
            11            CX03-CY50.                                    CI0295
            12            CX03-NARID  PICTURE  X(30).                   CI0295
            11            CX03-CY51                                     CI0295
                          REDEFINES            CX03-CY50.               CI0295
            12            CX03-NDIDN  PICTURE  9(12).                   CI0295
            12            CX03-FILLER PICTURE  X(18).                   CI0295
            11            CX03-CY52                                     CI0295
                          REDEFINES            CX03-CY50.               CI0295
            12            CX03-NAIDC  PICTURE  9(12).                   CI0295
            12            CX03-FILLER PICTURE  X(18).                   CI0295
            11            CX03-CY53                                     CI0295
                          REDEFINES            CX03-CY50.               CI0295
            12            CX03-NAMEXB PICTURE  9(15).                   CI0295
            12            CX03-FILLER PICTURE  X(15).                   CI0295
            10            CX03-CY99.                                    CI0295
            11            CX03-FILLER PICTURE  X(109).                  CI0295
            10            CX03-CY01                                     CI0295
                          REDEFINES            CX03-CY99.               CI0295
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX03-ICPCI  PICTURE  X.                       CI0295
            11            CX03-CLUPD  PICTURE  9(3).                    CI0295
            11            CX03-DLAUP  PICTURE  9(8).                    CI0295
            11            CX03-CWRC   PICTURE  99.                      CI0295
            11            CX03-CHCR   PICTURE  99.                      CI0295
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0295
            11            CX03-GEAUN  PICTURE  9(5).                    CI0295
            11            CX03-DPCHD  PICTURE  9(8).                    CI0295
            11            CX03-DLRCHK PICTURE  9(8).                    CI0295
            11            CX03-QTRCHK PICTURE  9(2).                    CI0295
            11            CX03-DNPMT  PICTURE  9(8).                    CI0295
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            10            CX03-CY02                                     CI0295
                          REDEFINES            CX03-CY99.               CI0295
            11            CX03-QSIRQ  PICTURE  99.                      CI0295
            11            CX03-QDRMN  PICTURE  9(2)                     CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX03-DDPRE  PICTURE  9(8).                    CI0295
            11            CX03-DDSHP  PICTURE  9(8).                    CI0295
            11            CX03-NDRFTB PICTURE  9(5).                    CI0295
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0295
            11            CX03-DDSHPA PICTURE  9(8).                    CI0295
            11            CX03-NDRFTF PICTURE  9(5).                    CI0295
            11            CX03-QDIPBK PICTURE  9(3).                    CI0295
            11            CX03-CREOR  PICTURE  X(1).                    CI0295
            11            CX03-CREOR1 PICTURE  X(1).                    CI0295
            11            CX03-DDASC  PICTURE  9(8).                    CI0295
            11            CX03-FILLER PICTURE  X(7).                    CI0295
            10            CX03-CY03                                     CI0295
                          REDEFINES            CX03-CY99.               CI0295
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0295
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0295
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0295
            11            CX03-DOPDA  PICTURE  99.                      CI0295
            11            CX03-CPMTF  PICTURE  99.                      CI0295
            11            CX03-CIRMO  PICTURE  X(12).                   CI0295
            11            CX03-CPALL  PICTURE  X(1).                    CI0295
            11            CX03-CCOLM  PICTURE  9(2).                    CI0295
            11            CX03-CBLTP  PICTURE  X(1).                    CI0295
            11            CX03-CASUB  PICTURE  9(2).                    CI0295
            11            CX03-CBLFM  PICTURE  9(2).                    CI0295
            11            CX03-IBILS  PICTURE  X.                       CI0295
            11            CX03-IPAOS  PICTURE  X.                       CI0295
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0295
            11            CX03-DLBPD  PICTURE  9(8).                    CI0295
            11            CX03-DNBPD  PICTURE  9(8).                    CI0295
            11            CX03-DODBD  PICTURE  9(8).                    CI0295
            11            CX03-CPSRE  PICTURE  99.                      CI0295
            11            CX03-ISPHN  PICTURE  X.                       CI0295
            11            CX03-TCARR  PICTURE  X(6).                    CI0295
            11            CX03-CBKPT  PICTURE  9(2).                    CI0295
            11            CX03-IECNT  PICTURE  X.                       CI0295
            11            CX03-ICONV  PICTURE  X(1).                    CI0295
            11            CX03-FILLER PICTURE  X(4).                    CI0295
            10            CX03-CY04                                     CI0295
                          REDEFINES            CX03-CY99.               CI0295
            11            CX03-CCARD  PICTURE  X(02).                   CI0295
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0295
            11            CX03-IREMT  PICTURE  X(01).                   CI0295
            11            CX03-ISBILA PICTURE  X.                       CI0295
            11            CX03-DLBPDA PICTURE  9(8).                    CI0295
            11            CX03-DNBPDA.                                  CI0295
            12            CX03-DNCYM  PICTURE  9(6).                    CI0295
            12            CX03-CEDTD  PICTURE  9(2).                    CI0295
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX03-DREMT  PICTURE  9(8).                    CI0295
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0295
            11            CX03-CWRC2  PICTURE  99.                      CI0295
            11            CX03-CHCR2  PICTURE  99.                      CI0295
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0295
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0295
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0295
       01                 CX06.                                         CI0295
            10            CX06-CX06K.                                   CI0295
            11            CX06-C299.                                    CI0295
            12            CX06-CTID.                                    CI0295
            13            CX06-CTIDA  PICTURE  9(3).                    CI0295
            13            CX06-CTIDN.                                   CI0295
            14            CX06-CTIDNP PICTURE  X(13).                   CI0295
            14            CX06-CTIDND PICTURE  9(11).                   CI0295
            10            CX06-NPECK  PICTURE  9(02).                   CI0295
            10            CX06-FILLER PICTURE  X.                       CI0295
       01                 CX13.                                         CI0295
            10            CX13-GELL   PICTURE  9(4)                     CI0295
                          BINARY.                                       CI0295
            10            CX13-CY20.                                    CI0295
            11            CX13-CX13K.                                   CI0295
            12            CX13-CARTZ  PICTURE  99.                      CI0295
            12            CX13-NAPDS  PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX13-GESTD  PICTURE  9(8).                    CI0295
            11            CX13-GEEND  PICTURE  9(8).                    CI0295
            11            CX13-DASUQ  PICTURE  9(8).                    CI0295
            11            CX13-CDEST  PICTURE  99.                      CI0295
            11            CX13-IIARR  PICTURE  X.                       CI0295
            11            CX13-DLAUP  PICTURE  9(8).                    CI0295
            11            CX13-GEOPD2 PICTURE  X(8).                    CI0295
            11            CX13-GEAUN  PICTURE  9(5).                    CI0295
            11            CX13-DPCHD  PICTURE  9(8).                    CI0295
            11            CX13-PPOT1  PICTURE  S9(3)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX13-ACOT1  PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX13-QPST1  PICTURE  S9(7)V999                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX13-FILLER PICTURE  X(03).                   CI0295
            10            CX13-CY96.                                    CI0295
            11            CX13-FILLER PICTURE  X(50).                   CI0295
            10            CX13-CY21                                     CI0295
                          REDEFINES            CX13-CY96.               CI0295
            11            CX13-DNPMT  PICTURE  9(8).                    CI0295
            11            CX13-CPMTF  PICTURE  99.                      CI0295
            11            CX13-ADBRQ  PICTURE  S9(11)V99                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX13-QSHOWQ PICTURE  S9(9)V999                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX13-PACT1  PICTURE  S999V999                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX13-DOPDA  PICTURE  99.                      CI0295
            11            CX13-DNEXE  PICTURE  9(8).                    CI0295
            11            CX13-CIRMO  PICTURE  X(12).                   CI0295
            10            CX13-CY98.                                    CI0295
            11            CX13-FILLER PICTURE  X(120).                  CI0295
            10            CX13-CY25                                     CI0295
                          REDEFINES            CX13-CY98.               CI0295
            11            CX13-COPTC  PICTURE  9(1).                    CI0295
            11            CX13-ILPOI  PICTURE  X(1).                    CI0295
            11            CX13-CATOC  PICTURE  X(1).                    CI0295
            11            CX13-CEOIA  PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX13-ACOAR  PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX13-CEOTR  PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX13-DSTMO  PICTURE  99.                      CI0295
            10            CX13-CY27                                     CI0295
                          REDEFINES            CX13-CY98.               CI0295
            11            CX13-QMTH1  PICTURE  9(3).                    CI0295
            11            CX13-IDRMD  PICTURE  X.                       CI0295
            10            CX13-CY28                                     CI0295
                          REDEFINES            CX13-CY98.               CI0295
            11            CX13-AALLBL PICTURE  S9(8)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX13-PSURR  PICTURE  S9(3)V999                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX13-DFPMT  PICTURE  9(8).                    CI0295
            11            CX13-QMTHLA PICTURE  9(3).                    CI0295
            11            CX13-PWHLDS PICTURE  S999V9(5)                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX13-ISWHO  PICTURE  X(1).                    CI0295
            10            CX13-CY29                                     CI0295
                          REDEFINES            CX13-CY98.               CI0295
            11            CX13-IINDI1 PICTURE  X(1).                    CI0295
            11            CX13-IINDI2 PICTURE  X(1).                    CI0295
            11            CX13-IINDI3 PICTURE  X(1).                    CI0295
            11            CX13-PWHLD5 PICTURE  S999V99                  CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX13-CCSMQ  PICTURE  X.                       CI0295
            11            CX13-CPLEC  PICTURE  XX.                      CI0295
            11            CX13-IPTRDA PICTURE  X(01).                   CI0295
            11            CX13-GCUSPY PICTURE  X(12).                   CI0295
            11            CX13-ALOIDA PICTURE  S9(11)V99                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            CX13-DELOI  PICTURE  9(8).                    CI0295
            11            CX13-CLGND  PICTURE  X.                       CI0295
            11            CX13-CORTYA PICTURE  X(3).                    CI0295
            11            CX13-CPH3U  PICTURE  X.                       CI0295
            11            CX13-CNAVR  PICTURE  X(1).                    CI0295
            11            CX13-NEXEC  PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
       01                 CX2Y.                                         CI0295
            10            CX2Y-CX2YK.                                   CI0295
            11            CX2Y-C299.                                    CI0295
            12            CX2Y-CTID.                                    CI0295
            13            CX2Y-CTIDA  PICTURE  9(3).                    CI0295
            13            CX2Y-CTIDN.                                   CI0295
            14            CX2Y-CTIDNP PICTURE  X(13).                   CI0295
            14            CX2Y-CTIDND PICTURE  9(11).                   CI0295
            11            CX2Y-C199.                                    CI0295
            12            CX2Y-CLID.                                    CI0295
            13            CX2Y-CLIDO  PICTURE  9(3).                    CI0295
            13            CX2Y-CLIDN.                                   CI0295
            14            CX2Y-CLIDNP PICTURE  X(12).                   CI0295
            14            CX2Y-CLIDND PICTURE  9(8).                    CI0295
            11            CX2Y-CARTY  PICTURE  99.                      CI0295
            11            CX2Y-NARRS  PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
      ******************************************************            AADA82
      ****      WORK AREAS FOR COMMON DATE UTILITY       ***            AADA82
      ******************************************************            AADA82
      **                                                                AADA82
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA82
      **                                                                AADA82
      **   SEGMENT DD30 - FUNCTION LAYOUT                               AADA82
      **                                                                AADA82
      *!WF DSP=DD DSL=DD SEL=30 FOR=I DES=2 LEV=1                       AADA82
       01                 DD30.                                         CI0295
            10            DD30-CDTFN  PICTURE  9(4)                     CI0295
                          VALUE                ZERO.                    CI0295
            10            DD30-CDTSF  PICTURE  9(4)                     CI0295
                          VALUE                ZERO.                    CI0295
            10            DD30-CDTSC  PICTURE  9(4)                     CI0295
                          VALUE                ZERO.                    CI0295
            10            DD30-FILLER PICTURE  X(40)                    CI0295
                          VALUE                SPACE.                   CI0295
       01                 DD34.                                         CI0295
            10            DD34-CAINS  PICTURE  X(03)                    CI0295
                          VALUE                SPACE.                   CI0295
            10            DD34-CDTUC  PICTURE  9                        CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-NDTUN  PICTURE  S9(05)                   CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-FILLER PICTURE  X(162)                   CI0295
                          VALUE                SPACE.                   CI0295
            10            DD34-DTGRG.                                   CI0295
            11            DD34-DTGCY.                                   CI0295
            12            DD34-DTGCC  PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            12            DD34-DTGYY  PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            11            DD34-DTGMM  PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            11            DD34-DTGDD  PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-DTJUL.                                   CI0295
            11            DD34-DTJCY.                                   CI0295
            12            DD34-DTJCC  PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            12            DD34-DTJYY  PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            11            DD34-DTJDD  PICTURE  9(3)                     CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTFM  PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTLM  PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTFF  PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTLF  PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTFW  PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTLW  PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CCDOWA PICTURE  9                        CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CCDRW  PICTURE  9                        CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-FILLER PICTURE  X(58)                    CI0295
                          VALUE                SPACE.                   CI0295
            10            DD34-DTGRGA.                                  CI0295
            11            DD34-DTGCYA.                                  CI0295
            12            DD34-DTGCCA PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            12            DD34-DTGYYA PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            11            DD34-DTGMMA PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            11            DD34-DTGDDA PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-DTJULA.                                  CI0295
            11            DD34-DTJCYA.                                  CI0295
            12            DD34-DTJCCA PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            12            DD34-DTJYYA PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            11            DD34-DTJDDA PICTURE  9(3)                     CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTFMA PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTLMA PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTFFA PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTLFA PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTFWA PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTLWA PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CCDOWB PICTURE  9                        CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CCDRWA PICTURE  9                        CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-FILLER PICTURE  X(58)                    CI0295
                          VALUE                SPACE.                   CI0295
            10            DD34-DTGRGB.                                  CI0295
            11            DD34-DTGCYB.                                  CI0295
            12            DD34-DTGCCB PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            12            DD34-DTGYYB PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            11            DD34-DTGMMB PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            11            DD34-DTGDDB PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-DTJULB.                                  CI0295
            11            DD34-DTJCYB.                                  CI0295
            12            DD34-DTJCCB PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            12            DD34-DTJYYB PICTURE  9(2)                     CI0295
                          VALUE                ZERO.                    CI0295
            11            DD34-DTJDDB PICTURE  9(3)                     CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTFMB PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTLMB PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTFFB PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTLFB PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTFWB PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CDTLWB PICTURE  9(01)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CCDOWC PICTURE  9                        CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-CCDRWB PICTURE  9                        CI0295
                          VALUE                ZERO.                    CI0295
            10            DD34-FILLER PICTURE  X(58)                    CI0295
                          VALUE                SPACE.                   CI0295
            10            DD34-FILLER PICTURE  X(40)                    CI0295
                          VALUE                SPACE.                   CI0295
      **                                                                AADA82
      **   SEGMENT DD34 - CONVERT DATE LAYOUT                           AADA82
      **                                                                AADA82
      *!WF DSP=DD DSL=DD SEL=34 FOR=I DES=2 LEV=1                       AADA82
      **                                                                AADA82
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0295
            10            XW05-XW06.                                    CI0295
            11            XW05-XDBPCB.                                  CI0295
            12            XW05-XDBDNM PICTURE  X(08)                    CI0295
                          VALUE                SPACE.                   CI0295
            12            XW05-XSEGLV PICTURE  X(02)                    CI0295
                          VALUE                SPACE.                   CI0295
            12            XW05-XRC    PICTURE  X(02)                    CI0295
                          VALUE                SPACE.                   CI0295
            12            XW05-XPROPT PICTURE  X(04)                    CI0295
                          VALUE                SPACE.                   CI0295
            12            XW05-FILLER PICTURE  S9(5)                    CI0295
                          VALUE                ZERO                     CI0295
                          BINARY.                                       CI0295
            12            XW05-XSEGNM PICTURE  X(08)                    CI0295
                          VALUE                SPACE.                   CI0295
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0295
                          VALUE                ZERO                     CI0295
                          BINARY.                                       CI0295
            12            XW05-XSEGNB PICTURE  9(05)                    CI0295
                          VALUE                ZERO                     CI0295
                          BINARY.                                       CI0295
            12            XW05-XCOKEY PICTURE  X(70)                    CI0295
                          VALUE                SPACE.                   CI0295
            10            XW05-XW07.                                    CI0295
            11            XW05-XIOPCB.                                  CI0295
            12            XW05-XTERMI PICTURE  X(08)                    CI0295
                          VALUE                SPACE.                   CI0295
            12            XW05-FILLER PICTURE  XX                       CI0295
                          VALUE                SPACE.                   CI0295
            12            XW05-XRC1   PICTURE  X(02)                    CI0295
                          VALUE                SPACE.                   CI0295
            12            XW05-FILLER PICTURE  X(12)                    CI0295
                          VALUE                SPACE.                   CI0295
            12            XW05-XMODNM PICTURE  X(8)                     CI0295
                          VALUE                SPACE.                   CI0295
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0295
                          VALUE                ZERO.                    CI0295
            10            XW05-XGU    PICTURE  X(4)                     CI0295
                          VALUE                'GU  '.                  CI0295
            10            XW05-XGHU   PICTURE  X(4)                     CI0295
                          VALUE                'GHU '.                  CI0295
            10            XW05-XGN    PICTURE  X(4)                     CI0295
                          VALUE                'GN  '.                  CI0295
            10            XW05-XGHN   PICTURE  X(4)                     CI0295
                          VALUE                'GHN '.                  CI0295
            10            XW05-XGNP   PICTURE  X(4)                     CI0295
                          VALUE                'GNP '.                  CI0295
            10            XW05-XGHNP  PICTURE  X(4)                     CI0295
                          VALUE                'GHNP'.                  CI0295
            10            XW05-XREPL  PICTURE  XXXX                     CI0295
                          VALUE                'REPL'.                  CI0295
            10            XW05-XISRT  PICTURE  X(4)                     CI0295
                          VALUE                'ISRT'.                  CI0295
            10            XW05-XDLET  PICTURE  X(4)                     CI0295
                          VALUE                'DLET'.                  CI0295
            10            XW05-XOPEN  PICTURE  X(4)                     CI0295
                          VALUE                'OPEN'.                  CI0295
            10            XW05-XCLSE  PICTURE  X(4)                     CI0295
                          VALUE                'CLSE'.                  CI0295
            10            XW05-XCHKP  PICTURE  X(4)                     CI0295
                          VALUE                'CHKP'.                  CI0295
            10            XW05-XXRST  PICTURE  X(4)                     CI0295
                          VALUE                'XRST'.                  CI0295
            10            XW05-XTERM  PICTURE  X(4)                     CI0295
                          VALUE                'TERM'.                  CI0295
            10            XW05-XNFPAC PICTURE  X(13)                    CI0295
                          VALUE                SPACE.                   CI0295
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0295
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0295
       01               7-DTIN-DATE.                                    ADB2DT
      *!WI pl=DT011                                                     ADB2DT
         05             7-DTIN-XDATC                                    ADB2DT
                        PICTURE XX.                                     CI0295
      *!WI pl=DT012                                                     ADB2DT
         05             7-DTIN-XDATY                                    ADB2DT
                        PICTURE XX.                                     CI0295
      *!WI pl=DT013                                                     ADB2DT
         05             7-DTIN-XDATM                                    ADB2DT
                        PICTURE XX.                                     CI0295
      *!WI pl=DT014                                                     ADB2DT
         05             7-DTIN-XDATD                                    ADB2DT
                        PICTURE XX.                                     CI0295
       01               7-DTOT-DATE.                                    ADB2DT
      *!WI pl=DT021                                                     ADB2DT
         05             7-DTOT-XDATC                                    ADB2DT
                        PICTURE XX.                                     CI0295
      *!WI pl=DT022                                                     ADB2DT
         05             7-DTOT-XDATY                                    ADB2DT
                        PICTURE XX.                                     CI0295
         05             FILLER PIC X                                    ADB2DT
                                            VALUE '-'.                  ADB2DT
      *!WI pl=DT025                                                     ADB2DT
         05             7-DTOT-XDATM                                    ADB2DT
                        PICTURE XX.                                     CI0295
         05             FILLER PIC X                                    ADB2DT
                                            VALUE '-'.                  ADB2DT
      *!WI pl=DT028                                                     ADB2DT
         05             7-DTOT-XDATD                                    ADB2DT
                        PICTURE XX.                                     CI0295
      *CCYYMMDD DATE IN
      *!WI
       01 DT01-DEFFT
                        PICTURE 9(8).                                   CI0295
      *CCYY-MM-DD DATE OUT
      *!WI
       01  DT01-DCACD
                        PICTURE X(10).                                  CI0295
       01                 GC01.                                         CI0295
            10            GC01-GC01K.                                   CI0295
            11            GC01-C299.                                    CI0295
            12            GC01-CTID.                                    CI0295
            13            GC01-CTIDA  PICTURE  9(3).                    CI0295
            13            GC01-CTIDN.                                   CI0295
            14            GC01-CTIDNP PICTURE  X(13).                   CI0295
            14            GC01-CTIDND PICTURE  9(11).                   CI0295
            10            GC01-DCAG9L PICTURE  9(8).                    CI0295
            10            GC01-NAASQL PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            10            GC01-ICUST  PICTURE  X.                       CI0295
            10            GC01-NSEQ4B PICTURE  9(8)                     CI0295
                          BINARY.                                       CI0295
            10            GC01-PRCOD  PICTURE  9(5).                    CI0295
            10            GC01-PRSCD  PICTURE  X(9).                    CI0295
            10            GC01-FILLER PICTURE  X(8).                    CI0295
       01                 GC03.                                         CI0295
            10            GC03-GELL   PICTURE  9(4)                     CI0295
                          BINARY.                                       CI0295
            10            GC03-GD00.                                    CI0295
            11            GC03-GC03K.                                   CI0295
            12            GC03-DCACG9 PICTURE  9(8).                    CI0295
            12            GC03-NAASQ  PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-CAATY  PICTURE  9(3).                    CI0295
            11            GC03-CVSYS  PICTURE  X(2).                    CI0295
            11            GC03-CACTO  PICTURE  9(3).                    CI0295
            11            GC03-CATRN.                                   CI0295
            12            GC03-CATRF  PICTURE  9(3).                    CI0295
            12            GC03-CATRS  PICTURE  9(3).                    CI0295
            11            GC03-CASTC  PICTURE  99.                      CI0295
            11            GC03-IPULL  PICTURE  X.                       CI0295
            11            GC03-GEAUN  PICTURE  9(5).                    CI0295
            11            GC03-GEOPD2 PICTURE  X(8).                    CI0295
            11            GC03-NBTCH  PICTURE  9(4).                    CI0295
            11            GC03-DEFFT  PICTURE  9(8).                    CI0295
            11            GC03-NSUNT  PICTURE  9(4).                    CI0295
            11            GC03-ITRAN  PICTURE  X.                       CI0295
            11            GC03-DLAUP1 PICTURE  9(8).                    CI0295
            11            GC03-ADRET  PICTURE  S9(11)V99                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-TTRMS  PICTURE  X(12).                   CI0295
            11            GC03-IDELT  PICTURE  X.                       CI0295
            11            GC03-GEOPDM PICTURE  X(8).                    CI0295
            11            GC03-FILLER PICTURE  X(07).                   CI0295
            10            GC03-GD09.                                    CI0295
            11            GC03-FILLER PICTURE  X(70).                   CI0295
            10            GC03-GD01                                     CI0295
                          REDEFINES            GC03-GD09.               CI0295
            11            GC03-ADBRQ  PICTURE  S9(11)V99                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-CTRTP  PICTURE  X(2).                    CI0295
            11            GC03-CPORT  PICTURE  X.                       CI0295
            11            GC03-CSCRNU PICTURE  X(4).                    CI0295
            11            GC03-DLAUP  PICTURE  9(8).                    CI0295
            11            GC03-CTWHAT PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-PWHLD  PICTURE  S999V9(5)                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-IWTHH  PICTURE  X.                       CI0295
            11            GC03-NDRFT  PICTURE  9(5).                    CI0295
            11            GC03-IDPAP  PICTURE  X.                       CI0295
            11            GC03-GETIM  PICTURE  S9(7)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-QNACT  PICTURE  9(3).                    CI0295
            11            GC03-AEDRQ  PICTURE  S9(11)V99                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-IPLIN  PICTURE  X.                       CI0295
            11            GC03-CLIDNB PICTURE  9(8).                    CI0295
            11            GC03-CSLCT  PICTURE  X.                       CI0295
            11            GC03-ITELE  PICTURE  X.                       CI0295
            11            GC03-FILLER PICTURE  X(06).                   CI0295
            10            GC03-GD02                                     CI0295
                          REDEFINES            GC03-GD09.               CI0295
            11            GC03-CSYST  PICTURE  99.                      CI0295
            11            GC03-FILLER PICTURE  X.                       CI0295
            11            GC03-ACASH  PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-DTRAC  PICTURE  9(8).                    CI0295
            11            GC03-CTRSO  PICTURE  9(02).                   CI0295
            11            GC03-NTRCE  PICTURE  9(06).                   CI0295
            11            GC03-GECKD1 PICTURE  9.                       CI0295
            11            GC03-CCOLL  PICTURE  X(3).                    CI0295
            11            GC03-CLTDP  PICTURE  X(3).                    CI0295
            11            GC03-PSLLD  PICTURE  S99V999                  CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-ISLOR  PICTURE  X.                       CI0295
            11            GC03-ITPAC  PICTURE  X.                       CI0295
            11            GC03-CPMTCA PICTURE  XXX.                     CI0295
            11            GC03-CSERV  PICTURE  X(3).                    CI0295
            11            GC03-ACOMO  PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-IPLIN1 PICTURE  X.                       CI0295
            11            GC03-INQEX  PICTURE  X.                       CI0295
            11            GC03-CTKRAA PICTURE  X(12).                   CI0295
            11            GC03-CCSMQ  PICTURE  X.                       CI0295
            11            GC03-IVAEX1 PICTURE  X.                       CI0295
            11            GC03-IHPMT  PICTURE  X(1).                    CI0295
            11            GC03-GETIM3 PICTURE  S9(7)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            10            GC03-GD03                                     CI0295
                          REDEFINES            GC03-GD09.               CI0295
            11            GC03-CATRNC PICTURE  9(6).                    CI0295
            11            GC03-APRNT1 PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-QSHOWT PICTURE  S9(10)V999               CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-ACINVT PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-ACOMO7 PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-QSHOMW PICTURE  S9(10)V999               CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-ATAXT3 PICTURE  S9(11)V99                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-CTSTR  PICTURE  9(2).                    CI0295
            11            GC03-ICIRA  PICTURE  X.                       CI0295
            11            GC03-GETIM2 PICTURE  S9(7)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-CPMTCX PICTURE  XX.                      CI0295
            11            GC03-FILLER PICTURE  X(16).                   CI0295
            10            GC03-GD99.                                    CI0295
            11            GC03-FILLER PICTURE  X(248).                  CI0295
            10            GC03-GD10                                     CI0295
                          REDEFINES            GC03-GD99.               CI0295
            11            GC03-MROTC  PICTURE  X(7).                    CI0295
            11            GC03-CEDSC  PICTURE  9(1).                    CI0295
            11            GC03-ILPOI  PICTURE  X(1).                    CI0295
            11            GC03-AWRCH  PICTURE  S9(3)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-CHCOC1 PICTURE  9(2).                    CI0295
            11            GC03-CHCOC2 PICTURE  9(2).                    CI0295
            11            GC03-CHCOC3 PICTURE  9(2).                    CI0295
            11            GC03-CHCOC4 PICTURE  9(2).                    CI0295
            11            GC03-CMCOC1 PICTURE  9(3).                    CI0295
            11            GC03-CMCOC2 PICTURE  9(3).                    CI0295
            11            GC03-CMCOC3 PICTURE  9(3).                    CI0295
            11            GC03-GD11.                                    CI0295
            12            GC03-FILLER PICTURE  X(219).                  CI0295
            11            GC03-GD12                                     CI0295
                          REDEFINES            GC03-GD11.               CI0295
            12            GC03-CELLO  PICTURE  9(1).                    CI0295
            12            GC03-CECLO  PICTURE  9(1).                    CI0295
            12            GC03-AEXML  PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-CEPI   PICTURE  X(1).                    CI0295
            12            GC03-CEXTY  PICTURE  X.                       CI0295
            12            GC03-CROPC  PICTURE  9(1).                    CI0295
            12            GC03-CPUTY  PICTURE  9(1).                    CI0295
            12            GC03-IMCII  PICTURE  X(1).                    CI0295
            12            GC03-GEMISC                                   CI0295
                          OCCURS       010     TIMES.                   CI0295
            13            GC03-AMGLA  PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            13            GC03-CMGLC  PICTURE  9(1).                    CI0295
            13            GC03-NMGLN  PICTURE  9(4).                    CI0295
            12            GC03-ACTRN  PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-IWRBK  PICTURE  X.                       CI0295
            12            GC03-IFEDX  PICTURE  X.                       CI0295
            12            GC03-ICNTR  PICTURE  X.                       CI0295
            12            GC03-IOCKH  PICTURE  X.                       CI0295
            12            GC03-ICRCK  PICTURE  X.                       CI0295
            12            GC03-NHMPN  PICTURE  S9(10)                   CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-ITELR1 PICTURE  X.                       CI0295
            11            GC03-GD13                                     CI0295
                          REDEFINES            GC03-GD11.               CI0295
            12            GC03-DREDO  PICTURE  9(8).                    CI0295
            12            GC03-CATRNR PICTURE  9(6).                    CI0295
            12            GC03-CEVN   PICTURE  9(9).                    CI0295
            12            GC03-ISUSP  PICTURE  X(1).                    CI0295
            11            GC03-GD15                                     CI0295
                          REDEFINES            GC03-GD11.               CI0295
            12            GC03-CPUTZ  PICTURE  9(1).                    CI0295
            12            GC03-CETLB  PICTURE  9(3).                    CI0295
            12            GC03-QTRMC  PICTURE  9(3).                    CI0295
            12            GC03-DEFFTE PICTURE  9(8).                    CI0295
            12            GC03-DEFFTF PICTURE  9(8).                    CI0295
            12            GC03-DEFFTG PICTURE  9(8).                    CI0295
            12            GC03-XZ1A   PICTURE  X.                       CI0295
            12            GC03-XZ1B   PICTURE  X.                       CI0295
            12            GC03-XZ1C   PICTURE  X.                       CI0295
            12            GC03-XZ1D   PICTURE  X.                       CI0295
            12            GC03-XZ1E   PICTURE  X.                       CI0295
            12            GC03-XZ1F   PICTURE  X.                       CI0295
            12            GC03-XZ1G   PICTURE  X.                       CI0295
            12            GC03-XZ1H   PICTURE  X.                       CI0295
            12            GC03-XZ1I   PICTURE  X.                       CI0295
            12            GC03-DEFFTH PICTURE  9(8).                    CI0295
            11            GC03-GD19                                     CI0295
                          REDEFINES            GC03-GD11.               CI0295
            12            GC03-GD11.                                    CI0295
            13            GC03-FILLER PICTURE  X(219).                  CI0295
            10            GC03-GD20                                     CI0295
                          REDEFINES            GC03-GD99.               CI0295
            11            GC03-ADDACT PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-ISIGV  PICTURE  X.                       CI0295
            11            GC03-IALLF  PICTURE  X.                       CI0295
            11            GC03-QSHOWQ PICTURE  S9(9)V999                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-CCDSCW PICTURE  9(2).                    CI0295
            11            GC03-IDWRL  PICTURE  X.                       CI0295
            11            GC03-ITELR  PICTURE  X.                       CI0295
            11            GC03-IABIN  PICTURE  X.                       CI0295
            11            GC03-PACT1  PICTURE  S999V999                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-IBFAF  PICTURE  X.                       CI0295
            11            GC03-IFRSA  PICTURE  X.                       CI0295
            11            GC03-ICRCAN PICTURE  X.                       CI0295
            11            GC03-ACACTV PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-AGFND  PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-QCSHOW PICTURE  S9(9)V999                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-QCSHIS PICTURE  S9(9)V999                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-NDTRC  PICTURE  9(8).                    CI0295
            11            GC03-CAERU  PICTURE  X(4).                    CI0295
            11            GC03-IFDGO  PICTURE  X.                       CI0295
            11            GC03-PSLLD2 PICTURE  S99V999                  CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-ISLOR2 PICTURE  X.                       CI0295
            11            GC03-QSFIO  PICTURE  S9(10)V999               CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-QSFID  PICTURE  S9(10)V999               CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-CGDIN  PICTURE  X.                       CI0295
            11            GC03-DGDIN  PICTURE  9(8).                    CI0295
            10            GC03-GD30                                     CI0295
                          REDEFINES            GC03-GD99.               CI0295
            11            GC03-ISKED  PICTURE  X.                       CI0295
            11            GC03-CENXC  PICTURE  9(2).                    CI0295
            11            GC03-GD31.                                    CI0295
            12            GC03-FILLER PICTURE  X(245).                  CI0295
            11            GC03-GD32                                     CI0295
                          REDEFINES            GC03-GD31.               CI0295
            12            GC03-IABIN1 PICTURE  X.                       CI0295
            12            GC03-CLDOD  PICTURE  9(8).                    CI0295
            12            GC03-NCLAM  PICTURE  9(5)                     CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-ISURR  PICTURE  X.                       CI0295
            12            GC03-GEHCD  PICTURE  9(3).                    CI0295
            12            GC03-CRATC  PICTURE  9(4).                    CI0295
            12            GC03-AMAXD  PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-ASCHGA PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-APYOM  PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-IWTHH1 PICTURE  X.                       CI0295
            12            GC03-CPAYCL PICTURE  X(2).                    CI0295
            12            GC03-CTSAO  PICTURE  X.                       CI0295
            12            GC03-NCONF  PICTURE  9(08).                   CI0295
            12            GC03-CLID   PICTURE  X(23).                   CI0295
            12            GC03-CARTY  PICTURE  99.                      CI0295
            12            GC03-NARRS  PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-CARTZ  PICTURE  99.                      CI0295
            12            GC03-NAPDS  PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-CPMTO  PICTURE  X.                       CI0295
            12            GC03-DNPMT  PICTURE  9(8).                    CI0295
            12            GC03-IPCTV  PICTURE  X.                       CI0295
            12            GC03-IMECH  PICTURE  X(01).                   CI0295
            12            GC03-IMVAO  PICTURE  X(1).                    CI0295
            12            GC03-AMVA1  PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-CACTS  PICTURE  X.                       CI0295
            12            GC03-CTSPP  PICTURE  X(1).                    CI0295
            12            GC03-CACT4  PICTURE  X(2).                    CI0295
            12            GC03-IVAEX  PICTURE  X.                       CI0295
            12            GC03-DFPMT  PICTURE  9(8).                    CI0295
            12            GC03-IDEMD  PICTURE  X.                       CI0295
            12            GC03-IOFST  PICTURE  X.                       CI0295
            12            GC03-AMXLB  PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-ACULB  PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-DEIRNB PICTURE  9(8).                    CI0295
            12            GC03-DEFFE  PICTURE  9(8).                    CI0295
            12            GC03-DEFFR  PICTURE  9(8).                    CI0295
            12            GC03-ISPUP  PICTURE  X.                       CI0295
            12            GC03-CPNCG  PICTURE  X.                       CI0295
            12            GC03-IEXPU  PICTURE  X.                       CI0295
            12            GC03-IPPCF  PICTURE  X.                       CI0295
            12            GC03-NAAPT  PICTURE  9(2).                    CI0295
            12            GC03-PWHLDS PICTURE  S999V9(5)                CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-ISWHO  PICTURE  X(1).                    CI0295
            11            GC03-GD33                                     CI0295
                          REDEFINES            GC03-GD31.               CI0295
            12            GC03-CPAYC  PICTURE  X(2).                    CI0295
            12            GC03-ADBRQX PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-ADBRQV PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-APTXR  PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-CTRTPE PICTURE  X(2).                    CI0295
            12            GC03-NCLAMI PICTURE  S9(9)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-CLIDO8 PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-CLIDN  PICTURE  X(20).                   CI0295
            12            GC03-DSET01 PICTURE  S9(8)                    CI0295
                          BINARY.                                       CI0295
            12            GC03-CTSET1 PICTURE  S9(6)                    CI0295
                          BINARY.                                       CI0295
            12            GC03-DSET02 PICTURE  S9(8)                    CI0295
                          BINARY.                                       CI0295
            12            GC03-CTSET2 PICTURE  S9(6)                    CI0295
                          BINARY.                                       CI0295
            11            GC03-GD34                                     CI0295
                          REDEFINES            GC03-GD31.               CI0295
            12            GC03-QNOFM  PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-CLTRM  PICTURE  99.                      CI0295
            12            GC03-AMXLN  PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-ALADJ  PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-ACHK   PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-APRMO  PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-IMECH1 PICTURE  X(01).                   CI0295
            12            GC03-CACT41 PICTURE  X(2).                    CI0295
            12            GC03-ACDSCC PICTURE  S9(05)V99                CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-ACDSCD PICTURE  S9(05)V99                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-GD39                                     CI0295
                          REDEFINES            GC03-GD31.               CI0295
            12            GC03-GD31.                                    CI0295
            13            GC03-FILLER PICTURE  X(245).                  CI0295
            10            GC03-GD40                                     CI0295
                          REDEFINES            GC03-GD99.               CI0295
            11            GC03-NTR    PICTURE  9(8).                    CI0295
            11            GC03-NPBNC  PICTURE  X(24).                   CI0295
            11            GC03-CRREV  PICTURE  X(3).                    CI0295
            11            GC03-CSUSL  PICTURE  S9.                      CI0295
            11            GC03-NMGLN1 PICTURE  9(4).                    CI0295
            11            GC03-DCAC92 PICTURE  9(8).                    CI0295
            11            GC03-NAASQ3 PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-GD49.                                    CI0295
            12            GC03-FILLER PICTURE  X(198).                  CI0295
            11            GC03-GD41                                     CI0295
                          REDEFINES            GC03-GD49.               CI0295
            12            GC03-CRREF  PICTURE  9(2).                    CI0295
            12            GC03-CORIR  PICTURE  X(02).                   CI0295
            12            GC03-CIPDB  PICTURE  X(03).                   CI0295
            12            GC03-CPAYH  PICTURE  X(02).                   CI0295
            12            GC03-NAMEX  PICTURE  9(15)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            12            GC03-DCHAE  PICTURE  9(4).                    CI0295
            12            GC03-DRQST  PICTURE  S9(8)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-GD42                                     CI0295
                          REDEFINES            GC03-GD49.               CI0295
            12            GC03-CPMTCB PICTURE  X(3).                    CI0295
            10            GC03-GD50                                     CI0295
                          REDEFINES            GC03-GD99.               CI0295
            11            GC03-ALOAD  PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-PSLLD4 PICTURE  S99V999                  CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-CSUSL1 PICTURE  S9.                      CI0295
            11            GC03-CRREV1 PICTURE  X(3).                    CI0295
            11            GC03-ADDAC  PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-DL13.                                    CI0295
            12            GC03-GEYR   PICTURE  9(4).                    CI0295
            12            GC03-GEMTH  PICTURE  99.                      CI0295
            12            GC03-NDAY   PICTURE  99.                      CI0295
            11            GC03-NSEQ3P PICTURE  S9(5)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-XZ6A   PICTURE  X(6).                    CI0295
            11            GC03-XZ7    PICTURE  X(7).                    CI0295
            11            GC03-XZ6B   PICTURE  X(6).                    CI0295
            11            GC03-XZ6    PICTURE  X(6).                    CI0295
            11            GC03-XZ6C   PICTURE  X(6).                    CI0295
            11            GC03-XZ20   PICTURE  X(20).                   CI0295
            11            GC03-CATRN1 PICTURE  9(6).                    CI0295
            11            GC03-ADDAC2 PICTURE  S9(7)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-ATAXT2 PICTURE  S9(11)V99                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-ACOMOT PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-XZ5    PICTURE  X(5).                    CI0295
            11            GC03-IREVD  PICTURE  X(1).                    CI0295
            11            GC03-ISUSP1 PICTURE  X(1).                    CI0295
            11            GC03-XZ6D   PICTURE  X(6).                    CI0295
            11            GC03-XZ13   PICTURE  X(13).                   CI0295
            11            GC03-CWHTP2 PICTURE  X(3).                    CI0295
            11            GC03-CWHTP3 PICTURE  X(3).                    CI0295
            11            GC03-DTREN  PICTURE  9(8).                    CI0295
            11            GC03-NAASQ1 PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            10            GC03-GD51                                     CI0295
                          REDEFINES            GC03-GD99.               CI0295
            11            GC03-ADOMOT PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-ACGLT  PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-ACGST  PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-CTXMT  PICTURE  9(2).                    CI0295
            11            GC03-ALOAD3 PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-FILLER PICTURE  X(31).                   CI0295
            10            GC03-GD52                                     CI0295
                          REDEFINES            GC03-GD99.               CI0295
            11            GC03-DEFFT5 PICTURE  9(8).                    CI0295
            11            GC03-PSLLD5 PICTURE  S99V999                  CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-CSUSL2 PICTURE  S9.                      CI0295
            11            GC03-ALOAD2 PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-DL22.                                    CI0295
            12            GC03-NYEAR1 PICTURE  9(4).                    CI0295
            12            GC03-GEMTHA PICTURE  99.                      CI0295
            12            GC03-NDAY01 PICTURE  99.                      CI0295
            11            GC03-NSEQ3R PICTURE  S9(5)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-CWHTP  PICTURE  X(3).                    CI0295
            11            GC03-CWHFR  PICTURE  X(3).                    CI0295
            11            GC03-CATRN7 PICTURE  9(6).                    CI0295
            11            GC03-ATAXT5 PICTURE  S9(11)V99                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-QSHOT  PICTURE  S9(10)V999               CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-ACINT3 PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-CWHTP1 PICTURE  X(3).                    CI0295
            11            GC03-CWHFR1 PICTURE  X(3).                    CI0295
            11            GC03-ACOMO5 PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-QSHOMU PICTURE  S9(10)V999               CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-ACASH1 PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-FILLER PICTURE  X(04).                   CI0295
            11            GC03-CATRN8 PICTURE  9(6).                    CI0295
            11            GC03-ALOAD1 PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-PSLLD1 PICTURE  S99V999                  CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-QSHOT1 PICTURE  S9(10)V999               CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-ACINT4 PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-CSUSL4 PICTURE  S9.                      CI0295
            11            GC03-ACOMO4 PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            10            GC03-GD60                                     CI0295
                          REDEFINES            GC03-GD99.               CI0295
            11            GC03-GEOPDD PICTURE  X(8)                     CI0295
                          OCCURS       005     TIMES.                   CI0295
            11            GC03-DLAUP3 PICTURE  9(8)                     CI0295
                          OCCURS       005     TIMES.                   CI0295
            11            GC03-GEOPDB PICTURE  X(8).                    CI0295
            11            GC03-DLAUP4 PICTURE  9(8).                    CI0295
            11            GC03-ITELR2 PICTURE  X.                       CI0295
            11            GC03-IPMTA  PICTURE  X.                       CI0295
            11            GC03-CCSMG  PICTURE  X.                       CI0295
            11            GC03-CPLEC  PICTURE  XX.                      CI0295
            11            GC03-CORTYA PICTURE  X(3).                    CI0295
            11            GC03-CACTBC PICTURE  X(1).                    CI0295
            11            GC03-CGSPIA PICTURE  X.                       CI0295
            11            GC03-IPTRDA PICTURE  X(01).                   CI0295
            11            GC03-GCUSPY PICTURE  X(12).                   CI0295
            11            GC03-CPALLA PICTURE  X(1).                    CI0295
            11            GC03-QSHO5A PICTURE  S9(9)V999                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-IFRSAB PICTURE  X.                       CI0295
            11            GC03-DELOI  PICTURE  9(8).                    CI0295
            11            GC03-IAROAA PICTURE  X.                       CI0295
            11            GC03-ACINVR PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-ILTINA PICTURE  X.                       CI0295
            11            GC03-ALOIDA PICTURE  S9(11)V99                CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            GC03-CFUNTA PICTURE  X(2).                    CI0295
            11            GC03-CLGND  PICTURE  X.                       CI0295
            11            GC03-CPH3U  PICTURE  X.                       CI0295
            11            GC03-GESTD  PICTURE  9(8).                    CI0295
            11            GC03-GEEND  PICTURE  9(8).                    CI0295
            11            GC03-CPMTF  PICTURE  99.                      CI0295
            11            GC03-CNAVR  PICTURE  X(1).                    CI0295
            10            GC03-GD70                                     CI0295
                          REDEFINES            GC03-GD99.               CI0295
            11            GC03-CMEMO  PICTURE  X(2).                    CI0295
            11            GC03-ALPLDT PICTURE  9(8).                    CI0295
            11            GC03-CTLPD  PICTURE  9(8).                    CI0295
            11            GC03-CPAYCM PICTURE  X(2).                    CI0295
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
      *---------------  SQL INCLUDE STATEMENTS ---------------------    ADB221
           EXEC SQL     INCLUDE SQLCA             END-EXEC.             ADB221
      *                                                                 ADB221
      *--------------  ERROR HANDLING VARIABLES --------------------    ADB221
       01               7-DB2-FUNCT      PIC X(35) VALUE SPACES.        ADB221
       01               7-SQLR.                                         ADB221
         05             7-SQLR-TEXT-LEN  PIC S9(9) COMP VALUE +80.      ADB221
         05             7-SQLR-MESSAGE.                                 ADB221
           10           7-SQLR-LEN       PIC S9(4) COMP VALUE +960.     ADB221
           10           7-SQLR-TEXT      PIC X(80) OCCURS 12 TIMES.     ADB221
       01               7-DB2-ABEND      PIC 9(4)  VALUE ZERO.          ADB221
       01               7-DB2-ABENDX     REDEFINES 7-DB2-ABEND.         ADB221
           05           7-DB2-FIRST      PIC X.                         ADB221
           05           FILLER           PIC X(3).                      ADB221
       01               7-TEST-SQLCODE   PIC S9(9) COMP.                ADB221
           88           ROW-NOT-FOUND              VALUE +100.          ADB221
           88           DUPLICATE-KEY              VALUE -803.          ADB221
           88           MULTIPLE-ROWS-FOUND        VALUE -811.          ADB221
           88           RESOURCE-NOT-AVAILABLE     VALUE -904.          ADB221
           88           RESOURCE-IN-USE            VALUE -913.          ADB221
       01               7-Q913-COUNT     PIC S9(3) COMP-3               ADB221
                                                   VALUE ZERO.          ADB221
       01               7-Q913-LNGTH     PIC S9(4) COMP                 ADB221
                                                   VALUE +66.           ADB221
      *!WI pl=SQ430                                                     ADB221
       01               7-Q913-TMSGV5                                   ADB221
                        PICTURE X(66)                                   CI0295
                           VALUE 'DB2 RESOURCE IN USE. TRY AGAIN '.     ADB221
       01               7-MAXM-RETRY     PIC S9(3) COMP-3               ADB221
                                                   VALUE +001.          ADB221
       01  WS00.
      *!WI
           05  WS00-DEFFT
                        PICTURE 9(8).                                   CI0295
      *!WI
           05  WS00-DNPMT
                        PICTURE 9(8).                                   CI0295
      *!WI
           05  WS00-DCACG
                        PICTURE 9(8).                                   CI0295
      *!WI
           05  WS00-DNACG
                        PICTURE 9(8).                                   CI0295
      *!WI
           05  WS00-NDTUN
                        PICTURE S9(05).                                 CI0295
           05  WS00-DCACG1   PIC 9(8) VALUE ZEROES.
           05  WS00-DNPMT1   PIC 9(8) VALUE ZEROES.
           05  WS00-COUNT    PIC S9(4) COMP.
       01   DEBUT-WSS.                                                  CI0295
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0295
            05   IK     PICTURE X.                                      CI0295
       01  CONSTANTES-PAC.                                              CI0295
           05  FILLER  PICTURE X(87)   VALUE                            CI0295
                     '6015 CAT09/08/14CI0295ADMIN   14:35:18CI0295P AMERCI0295
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0295
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0295
           05  NUGNA   PICTURE X(5).                                    CI0295
           05  APPLI   PICTURE X(3).                                    CI0295
           05  DATGN   PICTURE X(8).                                    CI0295
           05  PROGR   PICTURE X(6).                                    CI0295
           05  CODUTI  PICTURE X(8).                                    CI0295
           05  TIMGN   PICTURE X(8).                                    CI0295
           05  PROGE   PICTURE X(8).                                    CI0295
           05  COBASE  PICTURE X(4).                                    CI0295
           05  DATGNC  PICTURE X(10).                                   CI0295
           05  RELEAS  PICTURE X(7).                                    CI0295
           05  DATGE   PICTURE X(10).                                   CI0295
           05  DATSQ   PICTURE X(10).                                   CI0295
       01  DATCE.                                                       CI0295
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0295
         05  DATOR.                                                     CI0295
           10  DATOA  PICTURE XX.                                       CI0295
           10  DATOM  PICTURE XX.                                       CI0295
           10  DATOJ  PICTURE XX.                                       CI0295
       01   VARIABLES-CONDITIONNELLES.                                  CI0295
            05                  FT      PICTURE X VALUE '0'.            CI0295
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0295
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0295
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0295
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0295
            05       5-GC00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0295
       01               S-CX01-SSA.                                     CI0295
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0295
                                      VALUE 'CX01    '.                 CI0295
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0295
            10          S-CX01-CCOD   PICTURE X(5)                      CI0295
                                      VALUE '-----'.                    CI0295
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0295
       01            S-CXU01-SSA.                                       CI0295
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX01    '.                 CI0295
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0295
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CX01K'.                   CI0295
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0295
            10       S-CXU01-CX01K.                                     CI0295
            11       S-CXU01-C199.                                      CI0295
            12       S-CXU01-CLID.                                      CI0295
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0295
            13       S-CXU01-CLIDN.                                     CI0295
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0295
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0295
            10  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01               S-CX03-SSA.                                     CI0295
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0295
                                      VALUE 'CX03    '.                 CI0295
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0295
            10          S-CX03-CCOD   PICTURE X(5)                      CI0295
                                      VALUE '-----'.                    CI0295
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0295
       01            S-CXA03-SSA.                                       CI0295
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX03    '.                 CI0295
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0295
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CARTY'.                   CI0295
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0295
            12       S-CXA03-CARTY    PICTURE  99.                      CI0295
            12  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-CXB03-SSA.                                       CI0295
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX03    '.                 CI0295
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0295
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(NARRS'.                   CI0295
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0295
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            12  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-CXC03-SSA.                                       CI0295
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX03    '.                 CI0295
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CPMTG'.                   CI0295
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-CXD03-SSA.                                       CI0295
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX03    '.                 CI0295
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(GRCRNG'.                  CI0295
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-CXE03-SSA.                                       CI0295
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX03    '.                 CI0295
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(DEXDT'.                   CI0295
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-CXF03-SSA.                                       CI0295
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX03    '.                 CI0295
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CY50'.                    CI0295
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-CXF03-CY50.                                      CI0295
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-CXG03-SSA.                                       CI0295
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX03    '.                 CI0295
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(NBASQ'.                   CI0295
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-CXH03-SSA.                                       CI0295
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX03    '.                 CI0295
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0295
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(NARID'.                   CI0295
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0295
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0295
            12  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-CXU03-SSA.                                       CI0295
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX03    '.                 CI0295
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CX03K'.                   CI0295
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-CXU03-CX03K.                                     CI0295
            12       S-CXU03-CARTY    PICTURE  99.                      CI0295
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01               S-CX06-SSA.                                     CI0295
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0295
                                      VALUE 'CX06    '.                 CI0295
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0295
            10          S-CX06-CCOD   PICTURE X(5)                      CI0295
                                      VALUE '-----'.                    CI0295
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0295
       01            S-CXU06-SSA.                                       CI0295
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX06    '.                 CI0295
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0295
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CX06K'.                   CI0295
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0295
            10       S-CXU06-CX06K.                                     CI0295
            11       S-CXU06-C299.                                      CI0295
            12       S-CXU06-CTID.                                      CI0295
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0295
            13       S-CXU06-CTIDN.                                     CI0295
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0295
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0295
            10  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01               S-CX13-SSA.                                     CI0295
            10         S1-CX13-SEGNAM PICTURE X(8)                      CI0295
                                      VALUE 'CX13    '.                 CI0295
            10         S1-CX13-CCOM   PICTURE X VALUE '*'.              CI0295
            10          S-CX13-CCOD   PICTURE X(5)                      CI0295
                                      VALUE '-----'.                    CI0295
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0295
       01            S-CXA13-SSA.                                       CI0295
            11      S1-CXA13-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX13    '.                 CI0295
            11      S1-CXA13-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-CXA13-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-CXA13-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CDEST'.                   CI0295
            11       S-CXA13-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-CXA13-CDEST    PICTURE  99.                      CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-CXB13-SSA.                                       CI0295
            12      S1-CXB13-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX13    '.                 CI0295
            12      S1-CXB13-CCOM   PICTURE X VALUE '*'.                CI0295
            12       S-CXB13-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            12      S1-CXB13-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CARTZ'.                   CI0295
            12       S-CXB13-OPER  PICTURE XX VALUE ' ='.               CI0295
            12       S-CXB13-CARTZ    PICTURE  99.                      CI0295
            12  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-CXC13-SSA.                                       CI0295
            12      S1-CXC13-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX13    '.                 CI0295
            12      S1-CXC13-CCOM   PICTURE X VALUE '*'.                CI0295
            12       S-CXC13-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            12      S1-CXC13-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(NAPDS'.                   CI0295
            12       S-CXC13-OPER  PICTURE XX VALUE ' ='.               CI0295
            12       S-CXC13-NAPDS    PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            12  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-CXU13-SSA.                                       CI0295
            11      S1-CXU13-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX13    '.                 CI0295
            11      S1-CXU13-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-CXU13-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-CXU13-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CX13K'.                   CI0295
            11       S-CXU13-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-CXU13-CX13K.                                     CI0295
            12       S-CXU13-CARTZ    PICTURE  99.                      CI0295
            12       S-CXU13-NAPDS    PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-CX113-SSA.                                       CI0295
            11      S1-CX113-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX13    '.                 CI0295
            11      S1-CX113-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-CX113-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-CX113-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(XGCUSPY'.                 CI0295
            11       S-CX113-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-CX113-GCUSPY   PICTURE  X(12).                   CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01               S-CX2Y-SSA.                                     CI0295
            10         S1-CX2Y-SEGNAM PICTURE X(8)                      CI0295
                                      VALUE 'CX2Y    '.                 CI0295
            10         S1-CX2Y-CCOM   PICTURE X VALUE '*'.              CI0295
            10          S-CX2Y-CCOD   PICTURE X(5)                      CI0295
                                      VALUE '-----'.                    CI0295
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0295
       01            S-CXA2Y-SSA.                                       CI0295
            11      S1-CXA2Y-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX2Y    '.                 CI0295
            11      S1-CXA2Y-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-CXA2Y-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-CXA2Y-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CARTY'.                   CI0295
            11       S-CXA2Y-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-CXA2Y-CARTY    PICTURE  99.                      CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-CXB2Y-SSA.                                       CI0295
            11      S1-CXB2Y-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX2Y    '.                 CI0295
            11      S1-CXB2Y-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-CXB2Y-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-CXB2Y-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(C299'.                    CI0295
            11       S-CXB2Y-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-CXB2Y-C299.                                      CI0295
            12       S-CXB2Y-CTID.                                      CI0295
            13       S-CXB2Y-CTIDA    PICTURE  9(3).                    CI0295
            13       S-CXB2Y-CTIDN.                                     CI0295
            14       S-CXB2Y-CTIDNP   PICTURE  X(13).                   CI0295
            14       S-CXB2Y-CTIDND   PICTURE  9(11).                   CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-CXU2Y-SSA.                                       CI0295
            10      S1-CXU2Y-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'CX2Y    '.                 CI0295
            10      S1-CXU2Y-CCOM   PICTURE X VALUE '*'.                CI0295
            10       S-CXU2Y-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            10      S1-CXU2Y-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CX2YK'.                   CI0295
            10       S-CXU2Y-OPER  PICTURE XX VALUE ' ='.               CI0295
            10       S-CXU2Y-CX2YK.                                     CI0295
            11       S-CXU2Y-C299.                                      CI0295
            12       S-CXU2Y-CTID.                                      CI0295
            13       S-CXU2Y-CTIDA    PICTURE  9(3).                    CI0295
            13       S-CXU2Y-CTIDN.                                     CI0295
            14       S-CXU2Y-CTIDNP   PICTURE  X(13).                   CI0295
            14       S-CXU2Y-CTIDND   PICTURE  9(11).                   CI0295
            11       S-CXU2Y-C199.                                      CI0295
            12       S-CXU2Y-CLID.                                      CI0295
            13       S-CXU2Y-CLIDO    PICTURE  9(3).                    CI0295
            13       S-CXU2Y-CLIDN.                                     CI0295
            14       S-CXU2Y-CLIDNP   PICTURE  X(12).                   CI0295
            14       S-CXU2Y-CLIDND   PICTURE  9(8).                    CI0295
            11       S-CXU2Y-CARTY    PICTURE  99.                      CI0295
            11       S-CXU2Y-NARRS    PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            10  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01               S-GC01-SSA.                                     CI0295
            10         S1-GC01-SEGNAM PICTURE X(8)                      CI0295
                                      VALUE 'GC01    '.                 CI0295
            10         S1-GC01-CCOM   PICTURE X VALUE '*'.              CI0295
            10          S-GC01-CCOD   PICTURE X(5)                      CI0295
                                      VALUE '-----'.                    CI0295
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0295
       01            S-GCU01-SSA.                                       CI0295
            10      S1-GCU01-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC01    '.                 CI0295
            10      S1-GCU01-CCOM   PICTURE X VALUE '*'.                CI0295
            10       S-GCU01-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            10      S1-GCU01-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(GC01K'.                   CI0295
            10       S-GCU01-OPER  PICTURE XX VALUE ' ='.               CI0295
            10       S-GCU01-GC01K.                                     CI0295
            11       S-GCU01-C299.                                      CI0295
            12       S-GCU01-CTID.                                      CI0295
            13       S-GCU01-CTIDA    PICTURE  9(3).                    CI0295
            13       S-GCU01-CTIDN.                                     CI0295
            14       S-GCU01-CTIDNP   PICTURE  X(13).                   CI0295
            14       S-GCU01-CTIDND   PICTURE  9(11).                   CI0295
            10  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01               S-GC03-SSA.                                     CI0295
            10         S1-GC03-SEGNAM PICTURE X(8)                      CI0295
                                      VALUE 'GC03    '.                 CI0295
            10         S1-GC03-CCOM   PICTURE X VALUE '*'.              CI0295
            10          S-GC03-CCOD   PICTURE X(5)                      CI0295
                                      VALUE '-----'.                    CI0295
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0295
       01            S-GCA03-SSA.                                       CI0295
            11      S1-GCA03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GCA03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GCA03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GCA03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CAATY'.                   CI0295
            11       S-GCA03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GCA03-CAATY    PICTURE  9(3).                    CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GCB03-SSA.                                       CI0295
            11      S1-GCB03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GCB03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GCB03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GCB03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CVSYS'.                   CI0295
            11       S-GCB03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GCB03-CVSYS    PICTURE  X(2).                    CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GCC03-SSA.                                       CI0295
            11      S1-GCC03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GCC03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GCC03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GCC03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CASTC'.                   CI0295
            11       S-GCC03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GCC03-CASTC    PICTURE  99.                      CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GCD03-SSA.                                       CI0295
            11      S1-GCD03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GCD03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GCD03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GCD03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CACTO'.                   CI0295
            11       S-GCD03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GCD03-CACTO    PICTURE  9(3).                    CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GCE03-SSA.                                       CI0295
            11      S1-GCE03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GCE03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GCE03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GCE03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(IPULL'.                   CI0295
            11       S-GCE03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GCE03-IPULL    PICTURE  X.                       CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GCF03-SSA.                                       CI0295
            11      S1-GCF03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GCF03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GCF03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GCF03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(DTRAC'.                   CI0295
            11       S-GCF03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GCF03-DTRAC    PICTURE  9(8).                    CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GCG03-SSA.                                       CI0295
            11      S1-GCG03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GCG03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GCG03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GCG03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CTRSO'.                   CI0295
            11       S-GCG03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GCG03-CTRSO    PICTURE  9(02).                   CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GCH03-SSA.                                       CI0295
            11      S1-GCH03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GCH03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GCH03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GCH03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(NTRCE'.                   CI0295
            11       S-GCH03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GCH03-NTRCE    PICTURE  9(06).                   CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GCI03-SSA.                                       CI0295
            11      S1-GCI03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GCI03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GCI03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GCI03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(ITRAN'.                   CI0295
            11       S-GCI03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GCI03-ITRAN    PICTURE  X.                       CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GCJ03-SSA.                                       CI0295
            11      S1-GCJ03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GCJ03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GCJ03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GCJ03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(DEFFT'.                   CI0295
            11       S-GCJ03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GCJ03-DEFFT    PICTURE  9(8).                    CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GCK03-SSA.                                       CI0295
            11      S1-GCK03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GCK03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GCK03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GCK03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CPMTCA'.                  CI0295
            11       S-GCK03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GCK03-CPMTCA   PICTURE  XXX.                     CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GCL03-SSA.                                       CI0295
            11      S1-GCL03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GCL03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GCL03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GCL03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(ACASH'.                   CI0295
            11       S-GCL03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GCL03-ACASH    PICTURE  S9(9)V99                 CI0295
                          COMPUTATIONAL-3.                              CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GCN03-SSA.                                       CI0295
            11      S1-GCN03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GCN03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GCN03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GCN03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CRREV'.                   CI0295
            11       S-GCN03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GCN03-CRREV    PICTURE  X(3).                    CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GCO03-SSA.                                       CI0295
            11      S1-GCO03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GCO03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GCO03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GCO03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(CSYST'.                   CI0295
            11       S-GCO03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GCO03-CSYST    PICTURE  99.                      CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GCU03-SSA.                                       CI0295
            11      S1-GCU03-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GCU03-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GCU03-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GCU03-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(GC03K'.                   CI0295
            11       S-GCU03-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GCU03-GC03K.                                     CI0295
            12       S-GCU03-DCACG9   PICTURE  9(8).                    CI0295
            12       S-GCU03-NAASQ    PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GC103-SSA.                                       CI0295
            12      S1-GC103-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            12      S1-GC103-CCOM   PICTURE X VALUE '*'.                CI0295
            12       S-GC103-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            12      S1-GC103-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(XDCACG9'.                 CI0295
            12       S-GC103-OPER  PICTURE XX VALUE ' ='.               CI0295
            12       S-GC103-DCACG9   PICTURE  9(8).                    CI0295
            12  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GC203-SSA.                                       CI0295
            11      S1-GC203-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GC203-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GC203-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GC203-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(XGEAUN'.                  CI0295
            11       S-GC203-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GC203-GEAUN    PICTURE  9(5).                    CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GC303-SSA.                                       CI0295
            11      S1-GC303-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GC303-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GC303-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GC303-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(XGEOPD2'.                 CI0295
            11       S-GC303-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GC303-GEOPD2   PICTURE  X(8).                    CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GC403-SSA.                                       CI0295
            11      S1-GC403-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            11      S1-GC403-CCOM   PICTURE X VALUE '*'.                CI0295
            11       S-GC403-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            11      S1-GC403-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(XNBTCH'.                  CI0295
            11       S-GC403-OPER  PICTURE XX VALUE ' ='.               CI0295
            11       S-GC403-NBTCH    PICTURE  9(4).                    CI0295
            11  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01            S-GC803-SSA.                                       CI0295
            12      S1-GC803-SEGNAM PICTURE X(8)                        CI0295
                                      VALUE 'GC03    '.                 CI0295
            12      S1-GC803-CCOM   PICTURE X VALUE '*'.                CI0295
            12       S-GC803-CCOD   PICTURE X(5)                        CI0295
                                      VALUE '-----'.                    CI0295
            12      S1-GC803-FLDNAM PICTURE X(9)                        CI0295
                                      VALUE '(XNAASQ'.                  CI0295
            12       S-GC803-OPER  PICTURE XX VALUE ' ='.               CI0295
            12       S-GC803-NAASQ    PICTURE  S9(3)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            12  FILLER   PICTURE X    VALUE ')'.                        CI0295
       01   ZONES-UTILISATEUR PICTURE X.                                CI0295
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
      ** PCB POINTER FOR ARAY                                           ADU015
            05 PCB-ARAY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR ACAP                                             ADU015
      *!WF DSP=XB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XB00.                                         CI0295
          05              XB00-SUITE.                                   CI0295
            15       FILLER         PICTURE  X(00106).                  CI0295
       01                 XB06  REDEFINES      XB00.                    CI0295
            10            XB06-XDBPCB.                                  CI0295
            11            XB06-XDBDNM PICTURE  X(08).                   CI0295
            11            XB06-XSEGLV PICTURE  X(02).                   CI0295
            11            XB06-XRC    PICTURE  X(02).                   CI0295
            11            XB06-XPROPT PICTURE  X(04).                   CI0295
            11            XB06-FILLER PICTURE  S9(5)                    CI0295
                          BINARY.                                       CI0295
            11            XB06-XSEGNM PICTURE  X(08).                   CI0295
            11            XB06-XKEYLN PICTURE  S9(05)                   CI0295
                          BINARY.                                       CI0295
            11            XB06-XSEGNB PICTURE  9(05)                    CI0295
                          BINARY.                                       CI0295
            11            XB06-XCOKEY PICTURE  X(70).                   CI0295
      *** PCB MASK FOR ARAY                                             ADU015
      *!WF DSP=XC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XC00.                                         CI0295
          05              XC00-SUITE.                                   CI0295
            15       FILLER         PICTURE  X(00106).                  CI0295
       01                 XC06  REDEFINES      XC00.                    CI0295
            10            XC06-XDBPCB.                                  CI0295
            11            XC06-XDBDNM PICTURE  X(08).                   CI0295
            11            XC06-XSEGLV PICTURE  X(02).                   CI0295
            11            XC06-XRC    PICTURE  X(02).                   CI0295
            11            XC06-XPROPT PICTURE  X(04).                   CI0295
            11            XC06-FILLER PICTURE  S9(5)                    CI0295
                          BINARY.                                       CI0295
            11            XC06-XSEGNM PICTURE  X(08).                   CI0295
            11            XC06-XKEYLN PICTURE  S9(05)                   CI0295
                          BINARY.                                       CI0295
            11            XC06-XSEGNB PICTURE  9(05)                    CI0295
                          BINARY.                                       CI0295
            11            XC06-XCOKEY PICTURE  X(70).                   CI0295
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=XD DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XD00.                                         CI0295
          05              XD00-SUITE.                                   CI0295
            15       FILLER         PICTURE  X(00106).                  CI0295
       01                 XD06  REDEFINES      XD00.                    CI0295
            10            XD06-XDBPCB.                                  CI0295
            11            XD06-XDBDNM PICTURE  X(08).                   CI0295
            11            XD06-XSEGLV PICTURE  X(02).                   CI0295
            11            XD06-XRC    PICTURE  X(02).                   CI0295
            11            XD06-XPROPT PICTURE  X(04).                   CI0295
            11            XD06-FILLER PICTURE  S9(5)                    CI0295
                          BINARY.                                       CI0295
            11            XD06-XSEGNM PICTURE  X(08).                   CI0295
            11            XD06-XKEYLN PICTURE  S9(05)                   CI0295
                          BINARY.                                       CI0295
            11            XD06-XSEGNB PICTURE  9(05)                    CI0295
                          BINARY.                                       CI0295
            11            XD06-XCOKEY PICTURE  X(70).                   CI0295
      *!WF DSP=LK DSL=V2 SEL=10 FOR=I DES=2 LEV=1 PLT=10
       01                 LK10.                                         CI0295
            10            LK10-MAPPN  PICTURE  X(10)                    CI0295
                          VALUE                SPACE.                   CI0295
            10            LK10-CTID   PICTURE  X(27)                    CI0295
                          VALUE                SPACE.                   CI0295
            10            LK10-DEFFT  PICTURE  9(8)                     CI0295
                          VALUE                ZERO.                    CI0295
            10            LK10-IAIND  PICTURE  X                        CI0295
                          VALUE                SPACE.                   CI0295
            10            LK10-NMESS2 PICTURE  S9(6)                    CI0295
                          VALUE                ZERO                     CI0295
                          COMPUTATIONAL-3.                              CI0295
            10            LK10-TMESS4 PICTURE  X(512)                   CI0295
                          VALUE                SPACE.                   CI0295
            10            LK10-FILLER PICTURE  X(200)                   CI0295
                          VALUE                SPACE.                   CI0295
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0295
          05              DE00-SUITE.                                   CI0295
            15       FILLER         PICTURE  X(00653).                  CI0295
       01                 DE10  REDEFINES      DE00.                    CI0295
            10            DE10-DU11.                                    CI0295
            11            DE10-XFONC  PICTURE  X(4).                    CI0295
            11            DE10-MPSBN  PICTURE  X(8).                    CI0295
            11            DE10-XDBDNM PICTURE  X(08).                   CI0295
            11            DE10-XSEGNM PICTURE  X(08).                   CI0295
            11            DE10-XRC    PICTURE  X(02).                   CI0295
            11            DE10-MSEG   PICTURE  X(08).                   CI0295
            11            DE10-XCOKEY PICTURE  X(70).                   CI0295
            11            DE10-CUIBR  PICTURE  X(01).                   CI0295
            11            DE10-CUIBA  PICTURE  X(01).                   CI0295
            11            DE10-IPBIK  PICTURE  X(1).                    CI0295
            10            DE10-DU03.                                    CI0295
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            DE10-CMSSF  PICTURE  XX.                      CI0295
            11            DE10-DU09.                                    CI0295
            12            DE10-CMESA  PICTURE  S9(9)                    CI0295
                          BINARY.                                       CI0295
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0295
                          BINARY.                                       CI0295
            12            DE10-CMESB  PICTURE  S9(9)                    CI0295
                          BINARY.                                       CI0295
            12            DE10-CMSST  PICTURE  S9(9)                    CI0295
                          BINARY.                                       CI0295
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0295
                          BINARY.                                       CI0295
            12            DE10-QELLAA PICTURE  S9(9)                    CI0295
                          BINARY.                                       CI0295
            12            DE10-TMESS4 PICTURE  X(512).                  CI0295
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
       01                 MS00.                                         CI0295
          05              MS00-SUITE.                                   CI0295
            15       FILLER         PICTURE  X(00542).                  CI0295
       01                 MS03  REDEFINES      MS00.                    CI0295
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            10            MS03-CMSSF  PICTURE  XX.                      CI0295
            10            MS03-DU09.                                    CI0295
            11            MS03-CMESA  PICTURE  S9(9)                    CI0295
                          BINARY.                                       CI0295
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0295
                          BINARY.                                       CI0295
            11            MS03-CMESB  PICTURE  S9(9)                    CI0295
                          BINARY.                                       CI0295
            11            MS03-CMSST  PICTURE  S9(9)                    CI0295
                          BINARY.                                       CI0295
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0295
                          BINARY.                                       CI0295
            11            MS03-QELLAA PICTURE  S9(9)                    CI0295
                          BINARY.                                       CI0295
            11            MS03-TMESS4 PICTURE  X(512).                  CI0295
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0295
            10            MX11-QMSGS  PICTURE  9(03).                   CI0295
            10            MX11-PJ09                                     CI0295
                          OCCURS       025     TIMES.                   CI0295
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0295
                          COMPUTATIONAL-3.                              CI0295
            11            MX11-CMESB  PICTURE  S9(9)                    CI0295
                          BINARY.                                       CI0295
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                LK10
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N0T.      NOTE *************************************.
      *               *                                   *
      *               *SET ADDRESSES FOR PCB LINKAGE      *
      *               *                                   *
      *               *************************************.
       F0T.           EXIT.                                             lv05
      *N0TSC.    NOTE *SET ADDRESSES FOR PCB LINKAGE      *.
       F0TSC.                                                           lv10
      *SET ADDRESS FOR ACAP                                             DOT
           SET ADDRESS OF XB06 TO                                       ADU015
                PCB-ACAP-PTR1.                                          ADU015
      *SET ADDRESS FOR ARAY                                             DOT
           SET ADDRESS OF XC06 TO                                       ADU015
                PCB-ARAY-PTR1.                                          ADU015
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF XD06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
       F0TSC-FN. EXIT.
       F0T-FN.   EXIT.
      *N01.      NOTE *************************************.            CI0295
      *               *                                   *             CI0295
      *               *INITIALISATIONS                    *             CI0295
      *               *                                   *             CI0295
      *               *************************************.            CI0295
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
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0295
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0295
      *               *                                   *             CI0295
      *               *FIN DE TRAITEMENT                  *             CI0295
      *               *                                   *             CI0295
      *               *************************************.            CI0295
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0295
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *SET VALID EFFECTIVE DATE           *
      *               *                                   *
      *               *************************************.
       F45.           EXIT.                                             lv05
      *N45AD.    NOTE *CHECK WHETHER THE INPUT TRANSFER   *.
       F45AD.                                                           lv10
      *DATE IS A BUSINESS DAY
           INITIALIZE  WS00
           MOVE        LK10-DEFFT TO WS00-DCACG1
           PERFORM     F92NB THRU F92NB-FN.
       F45AD-FN. EXIT.
      *N45BA.    NOTE *SET THE EFFECTIVE DATE             *.
       F45BA.                                                           lv10
           MOVE        WS00-DCACG TO WS00-DEFFT.
       F45BA-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *CHECK IF PENDING TRANSFER EXISTS   *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *IN CATS ACTIVITY DATABASE
      *N50AB.    NOTE *READ GC01                          *.
       F50AB.                                                           lv10
           MOVE        LK10-CTID TO S-GCU01-GC01K
           PERFORM     F94GB THRU F94GB-FN.
      *N50AE.    NOTE *READ GC03 IF READING GC01          *.
       F50AE.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50AE-FN.
      *SUCCESSFULLY
           MOVE        '1' TO GC03-CF.
      *N50AH.    NOTE *LOOP THROUGH ACTIVITY              *.
       F50AH.                       GO TO     F50AH-B.                  lv20
       F50AH-A.
                 IF    GC03-CF = '0'
                                    GO TO     F50AH-FN.
       F50AH-B.
      *********************************
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           PERFORM     F94GC THRU F94GC-FN.
                 IF    GC03-CAATY = 001                                 DOT
                 AND   GC03-CF = '1'
                 AND   ((GC03-CASTC = 01
                 AND   GC03-IPULL = 'Y')
                 OR    (GC03-CASTC = 04
                 AND   GC03-IPULL = 'N'))
                 AND   GC03-DEFFT = WS00-DEFFT
      *CHECK IF AN EXISTING PENDING
      *DISBURSEMENT TRANSACTION INCLUDE
      *UNPROCESSED AND SUSPENSE TRAN IS
      *SCHED ON THE GIVEN TRANSFER DATE
      *IF TRANSACTION FOUND, RETURN ERR
      *********************************
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        015663 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN                              ADU119
           PERFORM     F92MC THRU F92MC-FN
           MOVE                     ALL '1' TO FT GO TO F20.
       F50AH-900. GO TO F50AH-A.
       F50AH-FN. EXIT.
       F50AE-FN. EXIT.
       F50AB-FN. EXIT.
       F50-FN.   EXIT.
      *N52.      NOTE *************************************.
      *               *                                   *
      *               *CHECK IF PENDING TARNSFER EXISTS   *
      *               *                                   *
      *               *************************************.
       F52.                                                             lv05
      *IN CATS ARRANGEMENT DATABASE
      *N52AB.    NOTE *READ ARRANGEMENT DB VIA CX2Y       *.
       F52AB.                                                           lv10
           MOVE        LOW-VALUES TO S-CXU2Y-CX2YK
           MOVE        LK10-CTID TO S-CXU2Y-CTID
           MOVE        'GE' TO S-CXU2Y-OPER
      *GU >= ON CX2Y
           PERFORM     F94GD THRU F94GD-FN.
      *N52AE.    NOTE *GET ARRANGEMENT DETAILS IF FOUND   *.
       F52AE.    IF    IK = '0'                                         lv15
                 AND   CX2Y-CTID = LK10-CTID
                 NEXT SENTENCE ELSE GO TO     F52AE-FN.
      *A SOURCE ACCOUNT MATCHES THE
      *INPUT CERTS ACCOUNT
      *N52AH.    NOTE *GET CX03                           *.
       F52AH.    IF    CX2Y-CARTY = 10                                  lv20
                 NEXT SENTENCE ELSE GO TO     F52AH-FN.
           MOVE        CX2Y-CLID TO S-CXU01-CX01K
      *READ THE CX03 ARRANGEMENT INFO
           MOVE        CX2Y-CARTY TO S-CXU03-CARTY
           MOVE        CX2Y-NARRS TO S-CXU03-NARRS
           PERFORM     F94GF THRU F94GF-FN.
                 IF    CX03-CF = '0'                                    DOT
      *SEGMENT CX03 NOT FOUND
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012007 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N52BC.    NOTE *GET FIRST CX13                     *.
       F52BC.                                                           lv25
           INITIALIZE  CX13
           MOVE        CX2Y-CTID TO S-CXU06-CX06K
           PERFORM     F94GH THRU F94GH-FN.
                 IF    CX13-CF = '0'                                    DOT
      *SEGMENT CX13 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012009 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N52BE.    NOTE *LOOP THRU ALL CX13 SEGMENT         *.
       F52BE.    IF    CX13-CF = '1'                                    lv30
                 NEXT SENTENCE ELSE GO TO     F52BE-FN.
      *N52BG.    NOTE *CHECK WHETHER AN ACTIVE            *.
       F52BG.    IF    CX13-CARTZ = 02                                  lv35
                 AND   CX13-CDEST = 01
                 AND   CX13-DNPMT <= WS00-DEFFT
                 NEXT SENTENCE ELSE GO TO     F52BG-FN.
      *SYSTEMATIC PARTIAL PAYMENT IS
      *SCHEDULED ON THE GIVEN TRANSFER
      *DATE
           MOVE        CX13-DNPMT TO WS00-DCACG1
           PERFORM     F92NB THRU F92NB-FN.
      *N52BT.    NOTE *SET EFFECTIVE DATE FOR NEXT        *.
       F52BT.                                                           lv40
      *SYSTEMATIC PARTIAL PAYMENT DATE
           MOVE        WS00-DCACG TO WS00-DNPMT.
      *N52BW.    NOTE *IF AN ACTIVE SYSTEMATIC PARTIAL    *.
       F52BW.    IF    WS00-DNPMT = WS00-DEFFT                          lv45
                 NEXT SENTENCE ELSE GO TO     F52BW-FN.
      *PAYMENT IS SCHEDULED ON THE
      *GIVEN TRANSFER DATE, RETURN ERR
      *********************************
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        015663 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN                              ADU119
           PERFORM     F92MC THRU F92MC-FN
           MOVE                     ALL '1' TO FT GO TO F20.
       F52BW-FN. EXIT.
      *N52CA.    NOTE *WHEN THE NEXT PAYMENT BUSNIESS     *.
       F52CA.    IF    WS00-DNPMT < WS00-DEFFT                          lv45
                 NEXT SENTENCE ELSE GO TO     F52CA-FN.
      *DATE IS LESS THAN THE GIVEN
      *TRANSFER EFFECTIVE DATE, CHECK
      *IF THE ARRANGEMENT FREQUENCY IS
      *WEEKLY,BI-WEEKLY OR SEMI-MONTHLY
      *N52CE.    NOTE *CHECK CURRENT NEXT PAYMENT DATE    *.
       F52CE.    IF    CX13-CPMTF = 52                                  lv50
                 OR    CX13-CPMTF = 26
                 OR    CX13-CPMTF = 24
                 NEXT SENTENCE ELSE GO TO     F52CE-FN.
      *+ FREQUENCY DAY IS SAME AS THE
      *GIVEN TRANSFER DATE
      *********************************
           MOVE        ZEROES TO WS00-NDTUN.
                 IF    CX13-CPMTF = 52                                  DOT
           MOVE        +7 TO WS00-NDTUN.
                 IF    CX13-CPMTF = 26                                  DOT
           MOVE        +14 TO WS00-NDTUN.
                 IF    CX13-CPMTF = 24                                  DOT
           MOVE        +15 TO WS00-NDTUN.
           PERFORM     F91 THRU F91-FN.                                 DOT
      *N52CI.    NOTE *WHEN THE FREQUENCY IS WEEKLY,      *.
       F52CI.    IF    CX13-CPMTF = 52                                  lv55
                 AND   WS00-DCACG NOT = ZEROES
                 AND   WS00-DCACG < WS00-DEFFT
                 NEXT SENTENCE ELSE GO TO     F52CI-FN.
      *IF THE CURRENT NEXT PAYMENT DATE
      *+ 7 IS STILL LESS THAN THE GIVEN
      *TRANSFER DATE , CHECK THE DATE
      *+14 WHICH IS TWO WEEK AFTER THE
      *CURRENT NEXT PAYMENT DATE
           MOVE        ZEROES TO WS00-NDTUN
           MOVE        +14 TO WS00-NDTUN
           PERFORM     F91 THRU F91-FN.
       F52CI-FN. EXIT.
       F52CE-FN. EXIT.
       F52CA-FN. EXIT.
       F52BT-FN. EXIT.
       F52BG-FN. EXIT.
      *N52EF.    NOTE *GET NEXT CX13                      *.
       F52EF.                                                           lv35
           INITIALIZE  CX13
           PERFORM     F94GH THRU F94GH-FN.
       F52EF-FN. EXIT.
       F52BE-900. GO TO F52BE.
       F52BE-FN. EXIT.
       F52BC-FN. EXIT.
       F52AH-FN. EXIT.
      *N52EX.    NOTE *GET NEXT CX2Y                      *.
       F52EX.                                                           lv20
           INITIALIZE  CX2Y
           PERFORM     F94GE THRU F94GE-FN.
       F52EX-FN. EXIT.
       F52AE-900. GO TO F52AE.
       F52AE-FN. EXIT.
       F52AB-FN. EXIT.
       F52-FN.   EXIT.
      *N54.      NOTE *************************************.
      *               *                                   *
      *               *CHECK IF PENDING TRANSFER EXISTS   *
      *               *                                   *
      *               *************************************.
       F54.                                                             lv05
      *IN CT1V DATABASE
      *N54CB.    NOTE *INITIALIZE DB SEGMENTS AND MOVE    *.
       F54CB.                                                           lv10
      *ACCT NUMBER FROM LINKAGE SEGMENT
           INITIALIZE  CT1V
           MOVE        LK10-CTID TO CT1V-CTID.
       F54CB-FN. EXIT.
      *N54EC.    NOTE *CONVERT DATES TO DB2 FORMAT        *.
       F54EC.                                                           lv10
      *PROCESSING DATE
           MOVE        WS00-DEFFT TO DT01-DEFFT
           PERFORM     F92DT THRU F92DT-FN
           MOVE        DT01-DCACD TO CT1V-DCACD7.
      *N54EH.    NOTE *CHECK FOR PENDING TRANS IS SCHED   *.
       F54EH.                                                           lv15
      *ON THE GIVEN TRANSFER DATE
           MOVE        0 TO WS00-COUNT
           PERFORM     F94RC THRU F94RC-FN.
      *N54HI.    NOTE *PENDING TRANSFER FOUND             *.
       F54HI.    IF    WS00-COUNT NOT = ZEROES                          lv20
                 NEXT SENTENCE ELSE GO TO     F54HI-FN.
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        015663 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN                              ADU119
           PERFORM     F92MC THRU F92MC-FN
           MOVE                     ALL '1' TO FT GO TO F20.
       F54HI-FN. EXIT.
      *N54HK.    NOTE *PENDING TRANSFER NOT FOUND         *.
       F54HK.                                                           lv20
           MOVE        'N' TO LK10-IAIND.
       F54HK-FN. EXIT.
       F54EH-FN. EXIT.
       F54EC-FN. EXIT.
       F54-FN.   EXIT.
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
      *               *COMPARE THE FUTURE ARRANGEMENT     *
      *               *                                   *
      *               *************************************.
       F91.                                                             lv05
      *PAYMENT DATE WITH THE GIVEN
      *TRANSFER DATE
      *N91BA.    NOTE *CALCULATED THE FUTURE PAYMENT      *.
       F91BA.                                                           lv10
      *DATE BASED ON THE FREQUENCY
           INITIALIZE  DD34
           MOVE        WS00-NDTUN TO DD34-NDTUN
           MOVE        CX13-DNPMT TO DD34-DTGRGA
           MOVE        ZEROES TO WS00-DCACG1
           MOVE        0 TO DD34-CDTUC
           MOVE        9 TO DD30-CDTSF
           PERFORM     F92BR THRU F92BR-FN
           MOVE        DD34-DTGRGB TO WS00-DCACG1
           MOVE        ZEROES TO WS00-DCACG.
      *N91BF.    NOTE *THE CALCULATED FUTURE PAYMENT      *.
       F91BF.    IF    (WS00-DCACG1 <= CX13-GEEND                       lv15
                 OR    CX13-GEEND = ZEROES)
                 AND   WS00-DCACG1 <= WS00-DEFFT
                 NEXT SENTENCE ELSE GO TO     F91BF-FN.
      *DATE IS NOT GREATER THAN THE
      *END DATE
      *CHECK IF THE CALCULATED DATE IS
      *A BUSINESS DAY, IF NOT CONVERT
      *TO THE NEXT BUSINESS DAY
           PERFORM     F92NB THRU F92NB-FN.
      *N91CA.    NOTE *IF ACTIVE SYSTEMATIC PARTIAL ARR   *.
       F91CA.    IF    WS00-DCACG = WS00-DEFFT                          lv20
                 NEXT SENTENCE ELSE GO TO     F91CA-FN.
      *IS SCHEDULED ON THE GIVEN
      *TRANSFER DATE, RETURN ERROR
      *TRANSFER DATE
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        015663 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN                              ADU119
           PERFORM     F92MC THRU F92MC-FN
           MOVE                     ALL '1' TO FT GO TO F20.
       F91CA-FN. EXIT.
       F91BF-FN. EXIT.
       F91BA-FN. EXIT.
       F91-FN.   EXIT.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *DATE CALCULATION                   *
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
       F92BR-FN. EXIT.
      *N92DT.    NOTE *DB2 DATE CONVERSION                *.
       F92DT.                                                           lv10
      *REFORMAT CCYYMMDD USING DASHES                                   ADB2DT
           MOVE        DT01-DEFFT TO                                    ADB2DT
           7-DTIN-DATE                                                  ADB2DT
           MOVE        7-DTIN-XDATC TO 7-DTOT-XDATC                     ADB2DT
           MOVE        7-DTIN-XDATY TO 7-DTOT-XDATY                     ADB2DT
           MOVE        7-DTIN-XDATM TO 7-DTOT-XDATM                     ADB2DT
           MOVE        7-DTIN-XDATD TO 7-DTOT-XDATD                     ADB2DT
           MOVE        7-DTOT-DATE TO                                   ADB2DT
           DT01-DCACD.                                                  ADB2DT
       F92DT-FN. EXIT.
      *N92MC.    NOTE *RETURN PENDING TRANS FOUND ERROR   *.
       F92MC.                                                           lv10
      *MESSAGE
      *********************************
           MOVE        'Y' TO LK10-IAIND
           MOVE        MS03-NMESS2 TO LK10-NMESS2
           MOVE        MS03-TMESS4 TO LK10-TMESS4
           MOVE        +0 TO MS03-NMESS2.
       F92MC-FN. EXIT.
      *N92NA.    NOTE *VALIDATE OR GET NEXT ACCTG DATE    *.
       F92NA.                                                           lv10
           MOVE        WS00-DCACG TO 7-XX01-PCKDAT                      $AACTG
           COMPUTE     7-XX01-PUDAT =                                   $AACTG
           (7-XX01-PCKDAT * 10)                                         $AACTG
           MOVE        7-XX01-UNSDAT TO 7-XX01-ICURR                    $AACTG
           CALL        7-XX01-DATMOD USING                              $AACTG
           7-XX01-IDTFLD                                                $AACTG
           7-XX01-RDTFLD                                                $AACTG
           MOVE        7-XX01-RCDATE TO 7-XX01-CHKDAT.                  $AACTG
                 IF    7-XX01-CHKPDT = +177607040                       DOT
           MOVE        ZEROES TO WS00-DNACG                             $AACTG
                 ELSE                                                   $AACTG
           MOVE        7-XX01-RNDATE TO 7-XX01-CHKDAT                   $AACTG
           COMPUTE     7-XX01-NEXTDT =                                  $AACTG
           (7-XX01-CHKPDT / 10)                                         $AACTG
           MOVE        7-XX01-NEXTDT TO WS00-DNACG.                     $AACTG
       F92NA-FN. EXIT.
      *N92NB.    NOTE *CHECK WHETHER THE DATE IS A        *.
       F92NB.                                                           lv10
      *BUSINESS DAY
           INITIALIZE  DD34
           MOVE        ZEROES TO WS00-DCACG
           MOVE        ZEROES TO WS00-DNACG
           MOVE        WS00-DCACG1 TO WS00-DCACG
           MOVE        +0 TO DD34-NDTUN
           PERFORM     F92NA THRU F92NA-FN.
      *N92NE.    NOTE *CONVERT TO THE NEXT BUSINESS DAY   *.
       F92NE.    IF    WS00-DNACG = ZEROES                              lv15
                 NEXT SENTENCE ELSE GO TO     F92NE-FN.
      *IF THE DATE IS NOT AN ACCOUNTG
      *DATE
           ADD         +1 TO DD34-NDTUN
           MOVE        WS00-DCACG1 TO DD34-DTGRGA
           MOVE        0 TO DD34-CDTUC
           MOVE        9 TO DD30-CDTSF
           PERFORM     F92BR THRU F92BR-FN.
      *N92NI.    NOTE *CHECK WHETHER THE NEXT DATE IS A   *.
       F92NI.                                                           lv20
      *BUSINESS DAY
           MOVE        DD34-DTGRGB TO WS00-DCACG
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
      *N93SQ.    NOTE *SQL ERROR HANDLING                 *.            ADB221
       F93SQ.                                                           lv10
           MOVE        SQLCODE TO 7-TEST-SQLCODE.                       ADB221
      *N93SR.    NOTE *TEST FOR NORMAL PROCESSING CODE    *.            ADB221
       F93SR.    IF    SQLCODE = +0                                     lv15
                 NEXT SENTENCE ELSE GO TO     F93SR-FN.                 ADB221
           MOVE        '0' TO IK                                        ADB221
           MOVE        ZERO TO 7-Q913-COUNT                             ADB221
               GO TO     F93SQ-FN.                                      ADB221
       F93SR-FN. EXIT.
      *N93SS.    NOTE *CHECK FOR NON-CRITICAL SQLCODE     *.            ADB221
       F93SS.    IF    SQLCODE = +100                                   lv15
                 OR    SQLCODE = -803                                   ADB221
                 OR    SQLCODE = -811                                   ADB221
                 OR    SQLCODE = -904                                   ADB221
                 NEXT SENTENCE ELSE GO TO     F93SS-FN.                 ADB221
           MOVE        ZERO TO 7-Q913-COUNT                             ADB221
           MOVE        '1' TO IK                                        ADB221
               GO TO     F93SQ-FN.                                      ADB221
       F93SS-FN. EXIT.
      *N93ST.    NOTE *CHECK FOR RESOURCE-IN-USE          *.            ADB221
       F93ST.    IF    SQLCODE = -913                                   lv15
                 NEXT SENTENCE ELSE GO TO     F93ST-FN.                 ADB221
      *N93SU.    NOTE *CHECK TO SEE IF ATTEMPT RETRY      *.            ADB221
       F93SU.    IF    7-Q913-COUNT < +0                                lv20
                 AND   7-Q913-COUNT < 7-MAXM-RETRY                      ADB221
                 NEXT SENTENCE ELSE GO TO     F93SU-FN.                 ADB221
           ADD         +1 TO 7-Q913-COUNT                               ADB221
           MOVE        '1' TO IK                                        ADB221
               GO TO     F93SQ-FN.                                      ADB221
       F93SU-FN. EXIT.
       F93ST-FN. EXIT.
      *N93SX.    NOTE **** CRITICAL SQLCODE ** ABEND **   *.            ADB221
       F93SX.                                                           lv15
      *COMMENTED OUT UNTIL PROBLEMS                                     ADB221
      *WITH THIS ARE RESOLVED!!!!!!                                     ADB221
      *CAL DSNTIAR FOR TEXT EXPLANATION                                 ADB221
      *CAL 'DSNTIAR' USING SQLCA                                        ADB221
      *                7-SQLR-MESSAGE                                   ADB221
      *                7-SQLR-TEXT-LEN.                                 ADB221
      *FORMAT CICS ABEND CODE AND ABEND                                 ADB221
           MOVE        SQLCODE TO 7-DB2-ABEND.                          ADB221
                 IF    SQLCODE NEGATIVE                                 DOT
           MOVE        '-' TO 7-DB2-FIRST                               ADB221
                 ELSE                                                   ADB221
           MOVE        '+' TO 7-DB2-FIRST.                              ADB221
           EXEC CICS   ABEND ABCODE (7-DB2-ABEND)                       DOT
                       CANCEL                                END-EXEC.  ADB221
       F93SX-FN. EXIT.
       F93SQ-FN. EXIT.
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
      *               *DATABASE PROCESS                   *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94GB.    NOTE *CALL GU ON GC01                    *.            ADU026
       F94GB.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XB06 GC01                                                    ADU026
           S-GCU01-SSA                                                  ADU026
           MOVE        XB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GB-FN. EXIT.
      *N94GC.    NOTE *CALL GN ON GC03                    *.            ADU026
       F94GC.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XB06 GC03                                                    ADU026
           S-GCU01-SSA S-GC03-SSA                                       ADU026
           MOVE        XB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
                 IF    IK = '0'                                         DOT
           MOVE        '1' TO GC03-CF
                 ELSE
           MOVE        '0' TO GC03-CF.
       F94GC-FN. EXIT.
      *N94GD.    NOTE *CALL GU ON CX2Y                    *.            ADU026
       F94GD.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XC06 CX2Y                                                    ADU026
           S-CXU2Y-SSA                                                  ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GD-FN. EXIT.
      *N94GE.    NOTE *CALL GN ON CX2Y                    *.            ADU026
       F94GE.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XC06 CX2Y                                                    ADU026
           S-CX2Y-SSA                                                   ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GE-FN. EXIT.
      *N94GF.    NOTE *CALL GU ON CX03                    *.            ADU026
       F94GF.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XD06 CX03                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           MOVE        XD06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
                 IF    IK = '0'                                         DOT
           MOVE        '1' TO CX03-CF
                 ELSE
           MOVE        '0' TO CX03-CF.
       F94GF-FN. EXIT.
      *N94GG.    NOTE *CALL GU ON CX06                    *.            ADU026
       F94GG.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XD06 CX06                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA
           MOVE        XD06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GG-FN. EXIT.
      *N94GH.    NOTE *CALL GN ON CX13                    *.            ADU026
       F94GH.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XD06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CX13-SSA
           MOVE        XD06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
                 IF    IK = '0'                                         DOT
           MOVE        '1' TO CX13-CF
                 ELSE
           MOVE        '0' TO CX13-CF.
       F94GH-FN. EXIT.
      *N94RC.    NOTE *CHECK FOR PENDING TRANSACTION      *.
       F94RC.                                                           lv10
           MOVE        'F94RC - SELECT' TO 7-DB2-FUNCT                  ADB226
           EXEC SQL    SELECT                                           ADB226
                       COUNT(*)
                       INTO
                         :WS00-COUNT
                       FROM
                          CORP.TBCT1V
                       WHERE
                           CTID   = :CT1V-CTID
                       AND DCACD7 = :CT1V-DCACD7             END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB226
       F94RC-FN. EXIT.
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
