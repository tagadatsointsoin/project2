       IDENTIFICATION DIVISION.                                         CI0073
       PROGRAM-ID.  CI0073P.                                            CI0073
      *AUTHOR.         M\M - DESTINATION UPDATE.                        CI0073
      *DATE-COMPILED.   09/08/14.                                       CI0073
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
       ENVIRONMENT DIVISION.                                            CI0073
       CONFIGURATION SECTION.                                           CI0073
       SOURCE-COMPUTER. IBM-370.                                        CI0073
       OBJECT-COMPUTER. IBM-370.                                        CI0073
       DATA DIVISION.                                                   CI0073
       WORKING-STORAGE SECTION.                                         CI0073
      *                                                                 ADU031
      ******************************************************************ADU031
      **        ACF2 COMMON AREA FOR CALLING ACF2 MODULES              *ADU031
      ******************************************************************ADU031
      *                                                                 ADU031
         COPY ACFUAREA.                                                 ADU031
      *                                                                 ADU031
      *                                                                 ADU031
      *                                                                 ADU031
      *                                                                 ADU031
      *>>>>>>> Audit Log Work Area                                      ADU165
                                                                        ADU165
       01               AL00-ADDR.                                      ADU165
              05        AL00-NPNTR     USAGE IS POINTER.                ADU165
                                                                        ADU165
      *!WI pl=AL005                                                     ADU165
       01               AL00-NSEQ2P    VALUE ZERO                       ADU165
                        PICTURE S9(3)                                   CI0073
                          COMPUTATIONAL-3.                              CI0073
                                                                        ADU165
      *>>>>>>> Linkage Area for Logger Program DBI110                   ADU165
      *!WF DSP=DH DSL=DH SEL=10 FOR=I DES=2 LEV=1                       ADU165
       01                 DH10.                                         CI0073
            10            DH10-GERTC  PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            DH10-XUIBP  PICTURE  S9(8)                    CI0073
                          VALUE                ZERO                     CI0073
                          BINARY.                                       CI0073
            10            DH10-NSEQ2P PICTURE  S9(3)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            DH10-CAUL   PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            DH10-MAUSB  PICTURE  X(8)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            DH10-NAUSK  PICTURE  X(50)                    CI0073
                          VALUE                SPACE.                   CI0073
            10            DH10-CSYS   PICTURE  X(4)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            DH10-CAPPL  PICTURE  X(8)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            DH10-CAUSR  PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            DH10-CAUFR  PICTURE  S9(5)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            DH10-CAUAC  PICTURE  S9(5)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            DH10-GEOPID PICTURE  X(6)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            DH10-CAUNIT PICTURE  X(4)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            DH10-GAUVR  PICTURE  X(400)                   CI0073
                          VALUE                SPACE.                   CI0073
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0091           PIC X(8) VALUE 'CI0091P '.                  AM0091
       01                 CX01.                                         CI0073
            10            CX01-CX01K.                                   CI0073
            11            CX01-C199.                                    CI0073
            12            CX01-CLID.                                    CI0073
            13            CX01-CLIDO  PICTURE  9(3).                    CI0073
            13            CX01-CLIDN.                                   CI0073
            14            CX01-CLIDNP PICTURE  X(12).                   CI0073
            14            CX01-CLIDND PICTURE  9(8).                    CI0073
            10            CX01-GEMDA  PICTURE  9(8).                    CI0073
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0073
                          BINARY.                                       CI0073
            10            CX01-FILLER PICTURE  X(5).                    CI0073
       01                 CX03.                                         CI0073
            10            CX03-GELL   PICTURE  9(4)                     CI0073
                          BINARY.                                       CI0073
            10            CX03-CY00.                                    CI0073
            11            CX03-CX03K.                                   CI0073
            12            CX03-CARTY  PICTURE  99.                      CI0073
            12            CX03-NARRS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX03-CARST  PICTURE  99.                      CI0073
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX03-CPMTG  PICTURE  99.                      CI0073
            11            CX03-GRCRNG PICTURE  9(3).                    CI0073
            11            CX03-DEXDT  PICTURE  9(8).                    CI0073
            11            CX03-DASUP  PICTURE  9(8).                    CI0073
            11            CX03-CSTEC  PICTURE  X(3).                    CI0073
            11            CX03-FILLER PICTURE  X(17).                   CI0073
            11            CX03-CY50.                                    CI0073
            12            CX03-NARID  PICTURE  X(30).                   CI0073
            11            CX03-CY51                                     CI0073
                          REDEFINES            CX03-CY50.               CI0073
            12            CX03-NDIDN  PICTURE  9(12).                   CI0073
            12            CX03-FILLER PICTURE  X(18).                   CI0073
            11            CX03-CY52                                     CI0073
                          REDEFINES            CX03-CY50.               CI0073
            12            CX03-NAIDC  PICTURE  9(12).                   CI0073
            12            CX03-FILLER PICTURE  X(18).                   CI0073
            11            CX03-CY53                                     CI0073
                          REDEFINES            CX03-CY50.               CI0073
            12            CX03-NAMEXB PICTURE  9(15).                   CI0073
            12            CX03-FILLER PICTURE  X(15).                   CI0073
            10            CX03-CY99.                                    CI0073
            11            CX03-FILLER PICTURE  X(109).                  CI0073
            10            CX03-CY01                                     CI0073
                          REDEFINES            CX03-CY99.               CI0073
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX03-ICPCI  PICTURE  X.                       CI0073
            11            CX03-CLUPD  PICTURE  9(3).                    CI0073
            11            CX03-DLAUP  PICTURE  9(8).                    CI0073
            11            CX03-CWRC   PICTURE  99.                      CI0073
            11            CX03-CHCR   PICTURE  99.                      CI0073
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0073
            11            CX03-GEAUN  PICTURE  9(5).                    CI0073
            11            CX03-DPCHD  PICTURE  9(8).                    CI0073
            11            CX03-DLRCHK PICTURE  9(8).                    CI0073
            11            CX03-QTRCHK PICTURE  9(2).                    CI0073
            11            CX03-DNPMT  PICTURE  9(8).                    CI0073
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            CX03-CY02                                     CI0073
                          REDEFINES            CX03-CY99.               CI0073
            11            CX03-QSIRQ  PICTURE  99.                      CI0073
            11            CX03-QDRMN  PICTURE  9(2)                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX03-DDPRE  PICTURE  9(8).                    CI0073
            11            CX03-DDSHP  PICTURE  9(8).                    CI0073
            11            CX03-NDRFTB PICTURE  9(5).                    CI0073
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0073
            11            CX03-DDSHPA PICTURE  9(8).                    CI0073
            11            CX03-NDRFTF PICTURE  9(5).                    CI0073
            11            CX03-QDIPBK PICTURE  9(3).                    CI0073
            11            CX03-CREOR  PICTURE  X(1).                    CI0073
            11            CX03-CREOR1 PICTURE  X(1).                    CI0073
            11            CX03-DDASC  PICTURE  9(8).                    CI0073
            11            CX03-FILLER PICTURE  X(7).                    CI0073
            10            CX03-CY03                                     CI0073
                          REDEFINES            CX03-CY99.               CI0073
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0073
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0073
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0073
            11            CX03-DOPDA  PICTURE  99.                      CI0073
            11            CX03-CPMTF  PICTURE  99.                      CI0073
            11            CX03-CIRMO  PICTURE  X(12).                   CI0073
            11            CX03-CPALL  PICTURE  X(1).                    CI0073
            11            CX03-CCOLM  PICTURE  9(2).                    CI0073
            11            CX03-CBLTP  PICTURE  X(1).                    CI0073
            11            CX03-CASUB  PICTURE  9(2).                    CI0073
            11            CX03-CBLFM  PICTURE  9(2).                    CI0073
            11            CX03-IBILS  PICTURE  X.                       CI0073
            11            CX03-IPAOS  PICTURE  X.                       CI0073
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0073
            11            CX03-DLBPD  PICTURE  9(8).                    CI0073
            11            CX03-DNBPD  PICTURE  9(8).                    CI0073
            11            CX03-DODBD  PICTURE  9(8).                    CI0073
            11            CX03-CPSRE  PICTURE  99.                      CI0073
            11            CX03-ISPHN  PICTURE  X.                       CI0073
            11            CX03-TCARR  PICTURE  X(6).                    CI0073
            11            CX03-CBKPT  PICTURE  9(2).                    CI0073
            11            CX03-IECNT  PICTURE  X.                       CI0073
            11            CX03-ICONV  PICTURE  X(1).                    CI0073
            11            CX03-FILLER PICTURE  X(4).                    CI0073
            10            CX03-CY04                                     CI0073
                          REDEFINES            CX03-CY99.               CI0073
            11            CX03-CCARD  PICTURE  X(02).                   CI0073
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0073
            11            CX03-IREMT  PICTURE  X(01).                   CI0073
            11            CX03-ISBILA PICTURE  X.                       CI0073
            11            CX03-DLBPDA PICTURE  9(8).                    CI0073
            11            CX03-DNBPDA.                                  CI0073
            12            CX03-DNCYM  PICTURE  9(6).                    CI0073
            12            CX03-CEDTD  PICTURE  9(2).                    CI0073
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX03-DREMT  PICTURE  9(8).                    CI0073
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0073
            11            CX03-CWRC2  PICTURE  99.                      CI0073
            11            CX03-CHCR2  PICTURE  99.                      CI0073
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0073
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0073
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0073
       01                 CX06.                                         CI0073
            10            CX06-CX06K.                                   CI0073
            11            CX06-C299.                                    CI0073
            12            CX06-CTID.                                    CI0073
            13            CX06-CTIDA  PICTURE  9(3).                    CI0073
            13            CX06-CTIDN.                                   CI0073
            14            CX06-CTIDNP PICTURE  X(13).                   CI0073
            14            CX06-CTIDND PICTURE  9(11).                   CI0073
            10            CX06-NPECK  PICTURE  9(02).                   CI0073
            10            CX06-FILLER PICTURE  X.                       CI0073
       01                 CX13.                                         CI0073
            10            CX13-GELL   PICTURE  9(4)                     CI0073
                          BINARY.                                       CI0073
            10            CX13-CY20.                                    CI0073
            11            CX13-CX13K.                                   CI0073
            12            CX13-CARTZ  PICTURE  99.                      CI0073
            12            CX13-NAPDS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX13-GESTD  PICTURE  9(8).                    CI0073
            11            CX13-GEEND  PICTURE  9(8).                    CI0073
            11            CX13-DASUQ  PICTURE  9(8).                    CI0073
            11            CX13-CDEST  PICTURE  99.                      CI0073
            11            CX13-IIARR  PICTURE  X.                       CI0073
            11            CX13-DLAUP  PICTURE  9(8).                    CI0073
            11            CX13-GEOPD2 PICTURE  X(8).                    CI0073
            11            CX13-GEAUN  PICTURE  9(5).                    CI0073
            11            CX13-DPCHD  PICTURE  9(8).                    CI0073
            11            CX13-PPOT1  PICTURE  S9(3)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX13-ACOT1  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX13-QPST1  PICTURE  S9(7)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX13-FILLER PICTURE  X(03).                   CI0073
            10            CX13-CY96.                                    CI0073
            11            CX13-FILLER PICTURE  X(50).                   CI0073
            10            CX13-CY21                                     CI0073
                          REDEFINES            CX13-CY96.               CI0073
            11            CX13-DNPMT  PICTURE  9(8).                    CI0073
            11            CX13-CPMTF  PICTURE  99.                      CI0073
            11            CX13-ADBRQ  PICTURE  S9(11)V99                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX13-QSHOWQ PICTURE  S9(9)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX13-PACT1  PICTURE  S999V999                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX13-DOPDA  PICTURE  99.                      CI0073
            11            CX13-DNEXE  PICTURE  9(8).                    CI0073
            11            CX13-CIRMO  PICTURE  X(12).                   CI0073
            10            CX13-CY98.                                    CI0073
            11            CX13-FILLER PICTURE  X(120).                  CI0073
            10            CX13-CY25                                     CI0073
                          REDEFINES            CX13-CY98.               CI0073
            11            CX13-COPTC  PICTURE  9(1).                    CI0073
            11            CX13-ILPOI  PICTURE  X(1).                    CI0073
            11            CX13-CATOC  PICTURE  X(1).                    CI0073
            11            CX13-CEOIA  PICTURE  S9(7)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX13-ACOAR  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX13-CEOTR  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX13-DSTMO  PICTURE  99.                      CI0073
            10            CX13-CY27                                     CI0073
                          REDEFINES            CX13-CY98.               CI0073
            11            CX13-QMTH1  PICTURE  9(3).                    CI0073
            11            CX13-IDRMD  PICTURE  X.                       CI0073
            10            CX13-CY28                                     CI0073
                          REDEFINES            CX13-CY98.               CI0073
            11            CX13-AALLBL PICTURE  S9(8)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX13-PSURR  PICTURE  S9(3)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX13-DFPMT  PICTURE  9(8).                    CI0073
            11            CX13-QMTHLA PICTURE  9(3).                    CI0073
            11            CX13-PWHLDS PICTURE  S999V9(5)                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX13-ISWHO  PICTURE  X(1).                    CI0073
            10            CX13-CY29                                     CI0073
                          REDEFINES            CX13-CY98.               CI0073
            11            CX13-IINDI1 PICTURE  X(1).                    CI0073
            11            CX13-IINDI2 PICTURE  X(1).                    CI0073
            11            CX13-IINDI3 PICTURE  X(1).                    CI0073
            11            CX13-PWHLD5 PICTURE  S999V99                  CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX13-CCSMQ  PICTURE  X.                       CI0073
            11            CX13-CPLEC  PICTURE  XX.                      CI0073
            11            CX13-IPTRDA PICTURE  X(01).                   CI0073
            11            CX13-GCUSPY PICTURE  X(12).                   CI0073
            11            CX13-ALOIDA PICTURE  S9(11)V99                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            CX13-DELOI  PICTURE  9(8).                    CI0073
            11            CX13-CLGND  PICTURE  X.                       CI0073
            11            CX13-CORTYA PICTURE  X(3).                    CI0073
            11            CX13-CPH3U  PICTURE  X.                       CI0073
            11            CX13-CNAVR  PICTURE  X(1).                    CI0073
            11            CX13-NEXEC  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
       01                 CX14.                                         CI0073
            10            CX14-GELL   PICTURE  9(4)                     CI0073
                          BINARY.                                       CI0073
            10            CX14-CX14K.                                   CI0073
            11            CX14-NPISQ  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            CX14-ACOTD  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            CX14-PPOTD  PICTURE  S9(3)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            CX14-QPSTD  PICTURE  S9(7)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            CX14-CPITC  PICTURE  99.                      CI0073
            10            CX14-FILLER PICTURE  X(04).                   CI0073
            10            CX14-CY97.                                    CI0073
            11            CX14-FILLER PICTURE  X(32).                   CI0073
            10            CX14-CY30                                     CI0073
                          REDEFINES            CX14-CY97.               CI0073
            11            CX14-IOWNC  PICTURE  X.                       CI0073
            11            CX14-CTYPE  PICTURE  X.                       CI0073
            11            CX14-C299.                                    CI0073
            12            CX14-CTID.                                    CI0073
            13            CX14-CTIDA  PICTURE  9(3).                    CI0073
            13            CX14-CTIDN.                                   CI0073
            14            CX14-CTIDNP PICTURE  X(13).                   CI0073
            14            CX14-CTIDND PICTURE  9(11).                   CI0073
            11            CX14-CPMTC  PICTURE  99.                      CI0073
            11            CX14-IACSD  PICTURE  X.                       CI0073
            10            CX14-CY31                                     CI0073
                          REDEFINES            CX14-CY97.               CI0073
            11            CX14-FILLER PICTURE  X(2).                    CI0073
            11            CX14-IDELI  PICTURE  X.                       CI0073
            11            CX14-CDEL1  PICTURE  9(3).                    CI0073
            11            CX14-NDELS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            CX14-CY32                                     CI0073
                          REDEFINES            CX14-CY97.               CI0073
            11            CX14-GCUSPZ PICTURE  X(12).                   CI0073
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0073
            10            XW05-XW06.                                    CI0073
            11            XW05-XDBPCB.                                  CI0073
            12            XW05-XDBDNM PICTURE  X(08)                    CI0073
                          VALUE                SPACE.                   CI0073
            12            XW05-XSEGLV PICTURE  X(02)                    CI0073
                          VALUE                SPACE.                   CI0073
            12            XW05-XRC    PICTURE  X(02)                    CI0073
                          VALUE                SPACE.                   CI0073
            12            XW05-XPROPT PICTURE  X(04)                    CI0073
                          VALUE                SPACE.                   CI0073
            12            XW05-FILLER PICTURE  S9(5)                    CI0073
                          VALUE                ZERO                     CI0073
                          BINARY.                                       CI0073
            12            XW05-XSEGNM PICTURE  X(08)                    CI0073
                          VALUE                SPACE.                   CI0073
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0073
                          VALUE                ZERO                     CI0073
                          BINARY.                                       CI0073
            12            XW05-XSEGNB PICTURE  9(05)                    CI0073
                          VALUE                ZERO                     CI0073
                          BINARY.                                       CI0073
            12            XW05-XCOKEY PICTURE  X(70)                    CI0073
                          VALUE                SPACE.                   CI0073
            10            XW05-XW07.                                    CI0073
            11            XW05-XIOPCB.                                  CI0073
            12            XW05-XTERMI PICTURE  X(08)                    CI0073
                          VALUE                SPACE.                   CI0073
            12            XW05-FILLER PICTURE  XX                       CI0073
                          VALUE                SPACE.                   CI0073
            12            XW05-XRC1   PICTURE  X(02)                    CI0073
                          VALUE                SPACE.                   CI0073
            12            XW05-FILLER PICTURE  X(12)                    CI0073
                          VALUE                SPACE.                   CI0073
            12            XW05-XMODNM PICTURE  X(8)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0073
                          VALUE                ZERO.                    CI0073
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0073
                          VALUE                ZERO.                    CI0073
            10            XW05-XGU    PICTURE  X(4)                     CI0073
                          VALUE                'GU  '.                  CI0073
            10            XW05-XGHU   PICTURE  X(4)                     CI0073
                          VALUE                'GHU '.                  CI0073
            10            XW05-XGN    PICTURE  X(4)                     CI0073
                          VALUE                'GN  '.                  CI0073
            10            XW05-XGHN   PICTURE  X(4)                     CI0073
                          VALUE                'GHN '.                  CI0073
            10            XW05-XGNP   PICTURE  X(4)                     CI0073
                          VALUE                'GNP '.                  CI0073
            10            XW05-XGHNP  PICTURE  X(4)                     CI0073
                          VALUE                'GHNP'.                  CI0073
            10            XW05-XREPL  PICTURE  XXXX                     CI0073
                          VALUE                'REPL'.                  CI0073
            10            XW05-XISRT  PICTURE  X(4)                     CI0073
                          VALUE                'ISRT'.                  CI0073
            10            XW05-XDLET  PICTURE  X(4)                     CI0073
                          VALUE                'DLET'.                  CI0073
            10            XW05-XOPEN  PICTURE  X(4)                     CI0073
                          VALUE                'OPEN'.                  CI0073
            10            XW05-XCLSE  PICTURE  X(4)                     CI0073
                          VALUE                'CLSE'.                  CI0073
            10            XW05-XCHKP  PICTURE  X(4)                     CI0073
                          VALUE                'CHKP'.                  CI0073
            10            XW05-XXRST  PICTURE  X(4)                     CI0073
                          VALUE                'XRST'.                  CI0073
            10            XW05-XTERM  PICTURE  X(4)                     CI0073
                          VALUE                'TERM'.                  CI0073
            10            XW05-XNFPAC PICTURE  X(13)                    CI0073
                          VALUE                SPACE.                   CI0073
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0073
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0073

      *ORIGINAL CX13, CX14 READ IN THIS MODULE USING KEYS PASSED IN WZ7D
      *!WF DSP=OR DSL=CX SEL=1314 FOR=I DES=1 LEV=1
      * PLT=FM
       01                 OR13.                                         CI0073
            10            OR13-GELL   PICTURE  9(4)                     CI0073
                          BINARY.                                       CI0073
            10            OR13-CY20.                                    CI0073
            11            OR13-CX13K.                                   CI0073
            12            OR13-CARTZ  PICTURE  99.                      CI0073
            12            OR13-NAPDS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            OR13-GESTD  PICTURE  9(8).                    CI0073
            11            OR13-GEEND  PICTURE  9(8).                    CI0073
            11            OR13-DASUQ  PICTURE  9(8).                    CI0073
            11            OR13-CDEST  PICTURE  99.                      CI0073
            11            OR13-IIARR  PICTURE  X.                       CI0073
            11            OR13-DLAUP  PICTURE  9(8).                    CI0073
            11            OR13-GEOPD2 PICTURE  X(8).                    CI0073
            11            OR13-GEAUN  PICTURE  9(5).                    CI0073
            11            OR13-DPCHD  PICTURE  9(8).                    CI0073
            11            OR13-PPOT1  PICTURE  S9(3)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            OR13-ACOT1  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            OR13-QPST1  PICTURE  S9(7)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            OR13-FILLER PICTURE  X(03).                   CI0073
            10            OR13-CY96.                                    CI0073
            11            OR13-FILLER PICTURE  X(50).                   CI0073
            10            OR13-CY21                                     CI0073
                          REDEFINES            OR13-CY96.               CI0073
            11            OR13-DNPMT  PICTURE  9(8).                    CI0073
            11            OR13-CPMTF  PICTURE  99.                      CI0073
            11            OR13-ADBRQ  PICTURE  S9(11)V99                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            OR13-QSHOWQ PICTURE  S9(9)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            OR13-PACT1  PICTURE  S999V999                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            OR13-DOPDA  PICTURE  99.                      CI0073
            11            OR13-DNEXE  PICTURE  9(8).                    CI0073
            11            OR13-CIRMO  PICTURE  X(12).                   CI0073
            10            OR13-CY98.                                    CI0073
            11            OR13-FILLER PICTURE  X(120).                  CI0073
            10            OR13-CY25                                     CI0073
                          REDEFINES            OR13-CY98.               CI0073
            11            OR13-COPTC  PICTURE  9(1).                    CI0073
            11            OR13-ILPOI  PICTURE  X(1).                    CI0073
            11            OR13-CATOC  PICTURE  X(1).                    CI0073
            11            OR13-CEOIA  PICTURE  S9(7)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            OR13-ACOAR  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            OR13-CEOTR  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            OR13-DSTMO  PICTURE  99.                      CI0073
            10            OR13-CY27                                     CI0073
                          REDEFINES            OR13-CY98.               CI0073
            11            OR13-QMTH1  PICTURE  9(3).                    CI0073
            11            OR13-IDRMD  PICTURE  X.                       CI0073
            10            OR13-CY28                                     CI0073
                          REDEFINES            OR13-CY98.               CI0073
            11            OR13-AALLBL PICTURE  S9(8)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            OR13-PSURR  PICTURE  S9(3)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            OR13-DFPMT  PICTURE  9(8).                    CI0073
            11            OR13-QMTHLA PICTURE  9(3).                    CI0073
            11            OR13-PWHLDS PICTURE  S999V9(5)                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            OR13-ISWHO  PICTURE  X(1).                    CI0073
            10            OR13-CY29                                     CI0073
                          REDEFINES            OR13-CY98.               CI0073
            11            OR13-IINDI1 PICTURE  X(1).                    CI0073
            11            OR13-IINDI2 PICTURE  X(1).                    CI0073
            11            OR13-IINDI3 PICTURE  X(1).                    CI0073
            11            OR13-PWHLD5 PICTURE  S999V99                  CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            OR13-CCSMQ  PICTURE  X.                       CI0073
            11            OR13-CPLEC  PICTURE  XX.                      CI0073
            11            OR13-IPTRDA PICTURE  X(01).                   CI0073
            11            OR13-GCUSPY PICTURE  X(12).                   CI0073
            11            OR13-ALOIDA PICTURE  S9(11)V99                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            OR13-DELOI  PICTURE  9(8).                    CI0073
            11            OR13-CLGND  PICTURE  X.                       CI0073
            11            OR13-CORTYA PICTURE  X(3).                    CI0073
            11            OR13-CPH3U  PICTURE  X.                       CI0073
            11            OR13-CNAVR  PICTURE  X(1).                    CI0073
            11            OR13-NEXEC  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
       01                 OR14.                                         CI0073
            10            OR14-GELL   PICTURE  9(4)                     CI0073
                          BINARY.                                       CI0073
            10            OR14-CX14K.                                   CI0073
            11            OR14-NPISQ  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            OR14-ACOTD  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            OR14-PPOTD  PICTURE  S9(3)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            OR14-QPSTD  PICTURE  S9(7)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            OR14-CPITC  PICTURE  99.                      CI0073
            10            OR14-FILLER PICTURE  X(04).                   CI0073
            10            OR14-CY97.                                    CI0073
            11            OR14-FILLER PICTURE  X(32).                   CI0073
            10            OR14-CY30                                     CI0073
                          REDEFINES            OR14-CY97.               CI0073
            11            OR14-IOWNC  PICTURE  X.                       CI0073
            11            OR14-CTYPE  PICTURE  X.                       CI0073
            11            OR14-C299.                                    CI0073
            12            OR14-CTID.                                    CI0073
            13            OR14-CTIDA  PICTURE  9(3).                    CI0073
            13            OR14-CTIDN.                                   CI0073
            14            OR14-CTIDNP PICTURE  X(13).                   CI0073
            14            OR14-CTIDND PICTURE  9(11).                   CI0073
            11            OR14-CPMTC  PICTURE  99.                      CI0073
            11            OR14-IACSD  PICTURE  X.                       CI0073
            10            OR14-CY31                                     CI0073
                          REDEFINES            OR14-CY97.               CI0073
            11            OR14-FILLER PICTURE  X(2).                    CI0073
            11            OR14-IDELI  PICTURE  X.                       CI0073
            11            OR14-CDEL1  PICTURE  9(3).                    CI0073
            11            OR14-NDELS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            OR14-CY32                                     CI0073
                          REDEFINES            OR14-CY97.               CI0073
            11            OR14-GCUSPZ PICTURE  X(12).                   CI0073

      *TEMPORARY SAVE AREA FOR OR13, OR14
      *!WF DSP=SV DSL=CX SEL=1314 FOR=I DES=1 LEV=1
      * PLT=FM
       01                 SV13.                                         CI0073
            10            SV13-GELL   PICTURE  9(4)                     CI0073
                          BINARY.                                       CI0073
            10            SV13-CY20.                                    CI0073
            11            SV13-CX13K.                                   CI0073
            12            SV13-CARTZ  PICTURE  99.                      CI0073
            12            SV13-NAPDS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            SV13-GESTD  PICTURE  9(8).                    CI0073
            11            SV13-GEEND  PICTURE  9(8).                    CI0073
            11            SV13-DASUQ  PICTURE  9(8).                    CI0073
            11            SV13-CDEST  PICTURE  99.                      CI0073
            11            SV13-IIARR  PICTURE  X.                       CI0073
            11            SV13-DLAUP  PICTURE  9(8).                    CI0073
            11            SV13-GEOPD2 PICTURE  X(8).                    CI0073
            11            SV13-GEAUN  PICTURE  9(5).                    CI0073
            11            SV13-DPCHD  PICTURE  9(8).                    CI0073
            11            SV13-PPOT1  PICTURE  S9(3)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            SV13-ACOT1  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            SV13-QPST1  PICTURE  S9(7)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            SV13-FILLER PICTURE  X(03).                   CI0073
            10            SV13-CY96.                                    CI0073
            11            SV13-FILLER PICTURE  X(50).                   CI0073
            10            SV13-CY21                                     CI0073
                          REDEFINES            SV13-CY96.               CI0073
            11            SV13-DNPMT  PICTURE  9(8).                    CI0073
            11            SV13-CPMTF  PICTURE  99.                      CI0073
            11            SV13-ADBRQ  PICTURE  S9(11)V99                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            SV13-QSHOWQ PICTURE  S9(9)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            SV13-PACT1  PICTURE  S999V999                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            SV13-DOPDA  PICTURE  99.                      CI0073
            11            SV13-DNEXE  PICTURE  9(8).                    CI0073
            11            SV13-CIRMO  PICTURE  X(12).                   CI0073
            10            SV13-CY98.                                    CI0073
            11            SV13-FILLER PICTURE  X(120).                  CI0073
            10            SV13-CY25                                     CI0073
                          REDEFINES            SV13-CY98.               CI0073
            11            SV13-COPTC  PICTURE  9(1).                    CI0073
            11            SV13-ILPOI  PICTURE  X(1).                    CI0073
            11            SV13-CATOC  PICTURE  X(1).                    CI0073
            11            SV13-CEOIA  PICTURE  S9(7)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            SV13-ACOAR  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            SV13-CEOTR  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            SV13-DSTMO  PICTURE  99.                      CI0073
            10            SV13-CY27                                     CI0073
                          REDEFINES            SV13-CY98.               CI0073
            11            SV13-QMTH1  PICTURE  9(3).                    CI0073
            11            SV13-IDRMD  PICTURE  X.                       CI0073
            10            SV13-CY28                                     CI0073
                          REDEFINES            SV13-CY98.               CI0073
            11            SV13-AALLBL PICTURE  S9(8)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            SV13-PSURR  PICTURE  S9(3)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            SV13-DFPMT  PICTURE  9(8).                    CI0073
            11            SV13-QMTHLA PICTURE  9(3).                    CI0073
            11            SV13-PWHLDS PICTURE  S999V9(5)                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            SV13-ISWHO  PICTURE  X(1).                    CI0073
            10            SV13-CY29                                     CI0073
                          REDEFINES            SV13-CY98.               CI0073
            11            SV13-IINDI1 PICTURE  X(1).                    CI0073
            11            SV13-IINDI2 PICTURE  X(1).                    CI0073
            11            SV13-IINDI3 PICTURE  X(1).                    CI0073
            11            SV13-PWHLD5 PICTURE  S999V99                  CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            SV13-CCSMQ  PICTURE  X.                       CI0073
            11            SV13-CPLEC  PICTURE  XX.                      CI0073
            11            SV13-IPTRDA PICTURE  X(01).                   CI0073
            11            SV13-GCUSPY PICTURE  X(12).                   CI0073
            11            SV13-ALOIDA PICTURE  S9(11)V99                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            SV13-DELOI  PICTURE  9(8).                    CI0073
            11            SV13-CLGND  PICTURE  X.                       CI0073
            11            SV13-CORTYA PICTURE  X(3).                    CI0073
            11            SV13-CPH3U  PICTURE  X.                       CI0073
            11            SV13-CNAVR  PICTURE  X(1).                    CI0073
            11            SV13-NEXEC  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
       01                 SV14.                                         CI0073
            10            SV14-GELL   PICTURE  9(4)                     CI0073
                          BINARY.                                       CI0073
            10            SV14-CX14K.                                   CI0073
            11            SV14-NPISQ  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            SV14-ACOTD  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            SV14-PPOTD  PICTURE  S9(3)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            SV14-QPSTD  PICTURE  S9(7)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            SV14-CPITC  PICTURE  99.                      CI0073
            10            SV14-FILLER PICTURE  X(04).                   CI0073
            10            SV14-CY97.                                    CI0073
            11            SV14-FILLER PICTURE  X(32).                   CI0073
            10            SV14-CY30                                     CI0073
                          REDEFINES            SV14-CY97.               CI0073
            11            SV14-IOWNC  PICTURE  X.                       CI0073
            11            SV14-CTYPE  PICTURE  X.                       CI0073
            11            SV14-C299.                                    CI0073
            12            SV14-CTID.                                    CI0073
            13            SV14-CTIDA  PICTURE  9(3).                    CI0073
            13            SV14-CTIDN.                                   CI0073
            14            SV14-CTIDNP PICTURE  X(13).                   CI0073
            14            SV14-CTIDND PICTURE  9(11).                   CI0073
            11            SV14-CPMTC  PICTURE  99.                      CI0073
            11            SV14-IACSD  PICTURE  X.                       CI0073
            10            SV14-CY31                                     CI0073
                          REDEFINES            SV14-CY97.               CI0073
            11            SV14-FILLER PICTURE  X(2).                    CI0073
            11            SV14-IDELI  PICTURE  X.                       CI0073
            11            SV14-CDEL1  PICTURE  9(3).                    CI0073
            11            SV14-NDELS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            SV14-CY32                                     CI0073
                          REDEFINES            SV14-CY97.               CI0073
            11            SV14-GCUSPZ PICTURE  X(12).                   CI0073

      *AUDIT LOG - SD ARRANGEMENT DETAIL & SD DESTINATION INSTR;
      *GETS MOVED TO GAUVR
      *!WF DSP=VA DSL=VA SEL=1314 FOR=I DES=1 LEV=1
      * PLT=FM
       01                 VA13.                                         CI0073
            10            VA13-K11A.                                    CI0073
            11            VA13-CX03K.                                   CI0073
            12            VA13-CARTY  PICTURE  99.                      CI0073
            12            VA13-NARRS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-CX06K.                                   CI0073
            12            VA13-CTIDA1 PICTURE  9(3).                    CI0073
            12            VA13-NACID1 PICTURE  X(24).                   CI0073
            10            VA13-CY20.                                    CI0073
            11            VA13-CX13K.                                   CI0073
            12            VA13-CARTZ  PICTURE  99.                      CI0073
            12            VA13-NAPDS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-GESTD  PICTURE  9(8).                    CI0073
            11            VA13-GEEND  PICTURE  9(8).                    CI0073
            11            VA13-DASUQ  PICTURE  9(8).                    CI0073
            11            VA13-CDEST  PICTURE  99.                      CI0073
            11            VA13-IIARR  PICTURE  X.                       CI0073
            11            VA13-DLAUP  PICTURE  9(8).                    CI0073
            11            VA13-GEOPD2 PICTURE  X(8).                    CI0073
            11            VA13-GEAUN  PICTURE  9(5).                    CI0073
            11            VA13-DPCHD  PICTURE  9(8).                    CI0073
            11            VA13-PPOT1  PICTURE  S9(3)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-ACOT1  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-QPST1  PICTURE  S9(7)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-FILLER PICTURE  X(03).                   CI0073
            10            VA13-CY96.                                    CI0073
            11            VA13-FILLER PICTURE  X(50).                   CI0073
            10            VA13-CY21                                     CI0073
                          REDEFINES            VA13-CY96.               CI0073
            11            VA13-DNPMT  PICTURE  9(8).                    CI0073
            11            VA13-CPMTF  PICTURE  99.                      CI0073
            11            VA13-ADBRQ  PICTURE  S9(11)V99                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-QSHOWQ PICTURE  S9(9)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-PACT1  PICTURE  S999V999                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-DOPDA  PICTURE  99.                      CI0073
            11            VA13-DNEXE  PICTURE  9(8).                    CI0073
            11            VA13-CIRMO  PICTURE  X(12).                   CI0073
            10            VA13-CY98.                                    CI0073
            11            VA13-FILLER PICTURE  X(120).                  CI0073
            10            VA13-CY25                                     CI0073
                          REDEFINES            VA13-CY98.               CI0073
            11            VA13-COPTC  PICTURE  9(1).                    CI0073
            11            VA13-ILPOI  PICTURE  X(1).                    CI0073
            11            VA13-CATOC  PICTURE  X(1).                    CI0073
            11            VA13-CEOIA  PICTURE  S9(7)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-ACOAR  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-CEOTR  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-DSTMO  PICTURE  99.                      CI0073
            10            VA13-CY27                                     CI0073
                          REDEFINES            VA13-CY98.               CI0073
            11            VA13-QMTH1  PICTURE  9(3).                    CI0073
            11            VA13-IDRMD  PICTURE  X.                       CI0073
            10            VA13-CY28                                     CI0073
                          REDEFINES            VA13-CY98.               CI0073
            11            VA13-AALLBL PICTURE  S9(8)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-PSURR  PICTURE  S9(3)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-DFPMT  PICTURE  9(8).                    CI0073
            11            VA13-QMTHLA PICTURE  9(3).                    CI0073
            11            VA13-PWHLDS PICTURE  S999V9(5)                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-ISWHO  PICTURE  X(1).                    CI0073
            10            VA13-CY29                                     CI0073
                          REDEFINES            VA13-CY98.               CI0073
            11            VA13-IINDI1 PICTURE  X(1).                    CI0073
            11            VA13-IINDI2 PICTURE  X(1).                    CI0073
            11            VA13-IINDI3 PICTURE  X(1).                    CI0073
            11            VA13-PWHLD5 PICTURE  S999V99                  CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-CCSMQ  PICTURE  X.                       CI0073
            11            VA13-CPLEC  PICTURE  XX.                      CI0073
            11            VA13-IPTRDA PICTURE  X(01).                   CI0073
            11            VA13-GCUSPY PICTURE  X(12).                   CI0073
            11            VA13-ALOIDA PICTURE  S9(11)V99                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA13-DELOI  PICTURE  9(8).                    CI0073
            11            VA13-CLGND  PICTURE  X.                       CI0073
            11            VA13-CORTYA PICTURE  X(3).                    CI0073
            11            VA13-CPH3U  PICTURE  X.                       CI0073
            11            VA13-CNAVR  PICTURE  X(1).                    CI0073
            11            VA13-NEXEC  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
       01                 VA14.                                         CI0073
            10            VA14-K11A.                                    CI0073
            11            VA14-CX03K.                                   CI0073
            12            VA14-CARTY  PICTURE  99.                      CI0073
            12            VA14-NARRS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            VA14-CX06K.                                   CI0073
            12            VA14-CTIDA1 PICTURE  9(3).                    CI0073
            12            VA14-NACID1 PICTURE  X(24).                   CI0073
            10            VA14-CX13K.                                   CI0073
            11            VA14-CARTZ  PICTURE  99.                      CI0073
            11            VA14-NAPDS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            VA14-CX14K.                                   CI0073
            11            VA14-NPISQ  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            VA14-ACOTD  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            VA14-PPOTD  PICTURE  S9(3)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            VA14-QPSTD  PICTURE  S9(7)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            VA14-CPITC  PICTURE  99.                      CI0073
            10            VA14-FILLER PICTURE  X(32).                   CI0073
            10            VA14-CY30                                     CI0073
                          REDEFINES            VA14-FILLER.             CI0073
            11            VA14-IOWNC  PICTURE  X.                       CI0073
            11            VA14-CTYPE  PICTURE  X.                       CI0073
            11            VA14-C299.                                    CI0073
            12            VA14-CTID.                                    CI0073
            13            VA14-CTIDA  PICTURE  9(3).                    CI0073
            13            VA14-CTIDN.                                   CI0073
            14            VA14-CTIDNP PICTURE  X(13).                   CI0073
            14            VA14-CTIDND PICTURE  9(11).                   CI0073
            11            VA14-CPMTC  PICTURE  99.                      CI0073
            11            VA14-IACSD  PICTURE  X.                       CI0073
            10            VA14-CY31                                     CI0073
                          REDEFINES            VA14-FILLER.             CI0073
            11            VA14-FILLER PICTURE  X(2).                    CI0073
            11            VA14-IDELI  PICTURE  X.                       CI0073
            11            VA14-CDEL1  PICTURE  9(3).                    CI0073
            11            VA14-NDELS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            VA14-CY32                                     CI0073
                          REDEFINES            VA14-FILLER.             CI0073
            11            VA14-GCUSPZ PICTURE  X(12).                   CI0073

       01                 GQ01.                                         CI0073
            10            GQ01-GELL   PICTURE  9(4)                     CI0073
                          BINARY.                                       CI0073
            10            GQ01-GMISC.                                   CI0073
            11            GQ01-GS00.                                    CI0073
            12            GQ01-GT01.                                    CI0073
            13            GQ01-GQ01K.                                   CI0073
            14            GQ01-CANUMB PICTURE  X(27).                   CI0073
            14            GQ01-CAMCTR PICTURE  9(5).                    CI0073
            14            GQ01-GESQ2  PICTURE  99.                      CI0073
            12            GQ01-GT02                                     CI0073
                          REDEFINES            GQ01-GT01.               CI0073
            13            GQ01-C199.                                    CI0073
            14            GQ01-CLID.                                    CI0073
            15            GQ01-CLIDO  PICTURE  9(3).                    CI0073
            15            GQ01-CLIDN.                                   CI0073
            16            GQ01-CLIDNP PICTURE  X(12).                   CI0073
            16            GQ01-CLIDND PICTURE  9(8).                    CI0073
            12            GQ01-GT03                                     CI0073
                          REDEFINES            GQ01-GT01.               CI0073
            13            GQ01-C299.                                    CI0073
            14            GQ01-CTID.                                    CI0073
            15            GQ01-CTIDA  PICTURE  9(3).                    CI0073
            15            GQ01-CTIDN.                                   CI0073
            16            GQ01-CTIDNP PICTURE  X(13).                   CI0073
            16            GQ01-CTIDND PICTURE  9(11).                   CI0073
            12            GQ01-GT04                                     CI0073
                          REDEFINES            GQ01-GT01.               CI0073
            13            GQ01-NPBN   PICTURE  X(20).                   CI0073
            12            GQ01-GT05                                     CI0073
                          REDEFINES            GQ01-GT01.               CI0073
            13            GQ01-GR98.                                    CI0073
            14            GQ01-GRID.                                    CI0073
            15            GQ01-GRIDC  PICTURE  9(3).                    CI0073
            15            GQ01-GRIDN.                                   CI0073
            16            GQ01-GRIDNP PICTURE  99.                      CI0073
            16            GQ01-GRIDND PICTURE  9(8).                    CI0073
            12            GQ01-GT06                                     CI0073
                          REDEFINES            GQ01-GT01.               CI0073
            13            GQ01-NTR    PICTURE  9(8).                    CI0073
            12            GQ01-GT07                                     CI0073
                          REDEFINES            GQ01-GT01.               CI0073
            13            GQ01-NTRAC  PICTURE  9(14).                   CI0073
            12            GQ01-GT08                                     CI0073
                          REDEFINES            GQ01-GT01.               CI0073
            13            GQ01-NSRAB  PICTURE  9(7).                    CI0073
            13            GQ01-GECKD  PICTURE  9.                       CI0073
            13            GQ01-NBLCK  PICTURE  9(5).                    CI0073
            13            GQ01-CTRID  PICTURE  X(4).                    CI0073
            12            GQ01-GT19                                     CI0073
                          REDEFINES            GQ01-GT01.               CI0073
            13            GQ01-GEOPD2 PICTURE  X(8).                    CI0073
            12            GQ01-CACKD  PICTURE  9.                       CI0073
            12            GQ01-CENTT  PICTURE  X.                       CI0073
            12            GQ01-CADATE PICTURE  X(8).                    CI0073
            12            GQ01-GETIM  PICTURE  S9(7)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            GQ01-GEOPID PICTURE  X(6).                    CI0073
            12            GQ01-CAUNIT PICTURE  X(4).                    CI0073
            12            GQ01-XTERMI PICTURE  X(08).                   CI0073
            12            GQ01-CAPPL  PICTURE  X(8).                    CI0073
            12            GQ01-CSYS   PICTURE  X(4).                    CI0073
            12            GQ01-NTRSU  PICTURE  999.                     CI0073
            12            GQ01-FILLER PICTURE  X(20).                   CI0073
            11            GQ01-XMISL  PICTURE  X(599).                  CI0073
      *!WF DSP=GS DSL=GS SEL=1A FOR=I DES=2 LEV=1 PLT=GS
       01                 GS00.                                         CI0073
            10            GS00-GT01.                                    CI0073
            11            GS00-GQ01K.                                   CI0073
            12            GS00-CANUMB PICTURE  X(27)                    CI0073
                          VALUE                SPACE.                   CI0073
            12            GS00-CAMCTR PICTURE  9(5)                     CI0073
                          VALUE                ZERO.                    CI0073
            12            GS00-GESQ2  PICTURE  99                       CI0073
                          VALUE                ZERO.                    CI0073
            10            GS00-GT02                                     CI0073
                          REDEFINES            GS00-GT01.               CI0073
            11            GS00-C199.                                    CI0073
            12            GS00-CLID.                                    CI0073
            13            GS00-CLIDO  PICTURE  9(3).                    CI0073
            13            GS00-CLIDN.                                   CI0073
            14            GS00-CLIDNP PICTURE  X(12).                   CI0073
            14            GS00-CLIDND PICTURE  9(8).                    CI0073
            10            GS00-GT03                                     CI0073
                          REDEFINES            GS00-GT01.               CI0073
            11            GS00-C299.                                    CI0073
            12            GS00-CTID.                                    CI0073
            13            GS00-CTIDA  PICTURE  9(3).                    CI0073
            13            GS00-CTIDN.                                   CI0073
            14            GS00-CTIDNP PICTURE  X(13).                   CI0073
            14            GS00-CTIDND PICTURE  9(11).                   CI0073
            10            GS00-GT04                                     CI0073
                          REDEFINES            GS00-GT01.               CI0073
            11            GS00-NPBN   PICTURE  X(20).                   CI0073
            10            GS00-GT05                                     CI0073
                          REDEFINES            GS00-GT01.               CI0073
            11            GS00-GR98.                                    CI0073
            12            GS00-GRID.                                    CI0073
            13            GS00-GRIDC  PICTURE  9(3).                    CI0073
            13            GS00-GRIDN.                                   CI0073
            14            GS00-GRIDNP PICTURE  99.                      CI0073
            14            GS00-GRIDND PICTURE  9(8).                    CI0073
            10            GS00-GT06                                     CI0073
                          REDEFINES            GS00-GT01.               CI0073
            11            GS00-NTR    PICTURE  9(8).                    CI0073
            10            GS00-GT07                                     CI0073
                          REDEFINES            GS00-GT01.               CI0073
            11            GS00-NTRAC  PICTURE  9(14).                   CI0073
            10            GS00-GT08                                     CI0073
                          REDEFINES            GS00-GT01.               CI0073
            11            GS00-NSRAB  PICTURE  9(7).                    CI0073
            11            GS00-GECKD  PICTURE  9.                       CI0073
            11            GS00-NBLCK  PICTURE  9(5).                    CI0073
            11            GS00-CTRID  PICTURE  X(4).                    CI0073
            10            GS00-GT19                                     CI0073
                          REDEFINES            GS00-GT01.               CI0073
            11            GS00-GEOPD2 PICTURE  X(8).                    CI0073
            10            GS00-CACKD  PICTURE  9                        CI0073
                          VALUE                ZERO.                    CI0073
            10            GS00-CENTT  PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            GS00-CADATE PICTURE  X(8)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            GS00-GETIM  PICTURE  S9(7)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            GS00-GEOPID PICTURE  X(6)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            GS00-CAUNIT PICTURE  X(4)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            GS00-XTERMI PICTURE  X(08)                    CI0073
                          VALUE                SPACE.                   CI0073
            10            GS00-CAPPL  PICTURE  X(8)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            GS00-CSYS   PICTURE  X(4)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            GS00-NTRSU  PICTURE  999                      CI0073
                          VALUE                ZERO.                    CI0073
            10            GS00-FILLER PICTURE  X(20)                    CI0073
                          VALUE                SPACE.                   CI0073
       01                 GS1A.                                         CI0073
            10            GS1A-NGEOPA PICTURE  X(08)                    CI0073
                          VALUE                SPACE.                   CI0073
            10            GS1A-CACLS1 PICTURE  X(20)                    CI0073
                          VALUE                SPACE.                   CI0073
            10            GS1A-CTRHO  PICTURE  9(8)                     CI0073
                          VALUE                ZERO.                    CI0073
            10            GS1A-GETIM3 PICTURE  S9(7)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            GS1A-CSLCT  PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            GS1A-GEOPD9 PICTURE  X(8)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            GS1A-GETIM  PICTURE  S9(7)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            GS1A-DCACG1 PICTURE  9(8)                     CI0073
                          VALUE                ZERO.                    CI0073
            10            GS1A-GEOPD2 PICTURE  X(8)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            GS1A-DCACG  PICTURE  9(8)                     CI0073
                          VALUE                ZERO.                    CI0073
            10            GS1A-GETIM2 PICTURE  S9(7)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            GS1A-CAVER  PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            GS1A-IWEBBT PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            GS1A-FILLER PICTURE  X(17)                    CI0073
                          VALUE                SPACE.                   CI0073
            10            GS1A-NGEOR  PICTURE  9(08)                    CI0073
                          VALUE                ZERO.                    CI0073
            10            GS1A-CACLS2 PICTURE  X(20)                    CI0073
                          VALUE                SPACE.                   CI0073
            10            GS1A-CAPID  PICTURE  9(2)                     CI0073
                          VALUE                ZERO.                    CI0073
            10            GS1A-CX01K.                                   CI0073
            11            GS1A-C199.                                    CI0073
            12            GS1A-CLID.                                    CI0073
            13            GS1A-CLIDO  PICTURE  9(3)                     CI0073
                          VALUE                ZERO.                    CI0073
            13            GS1A-CLIDN.                                   CI0073
            14            GS1A-CLIDNP PICTURE  X(12)                    CI0073
                          VALUE                SPACE.                   CI0073
            14            GS1A-CLIDND PICTURE  9(8)                     CI0073
                          VALUE                ZERO.                    CI0073
            10            GS1A-CX03K.                                   CI0073
            11            GS1A-CARTY  PICTURE  99                       CI0073
                          VALUE                ZERO.                    CI0073
            11            GS1A-NARRS  PICTURE  S9(3)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            GS1A-CX06K.                                   CI0073
            11            GS1A-C299.                                    CI0073
            12            GS1A-CTID.                                    CI0073
            13            GS1A-CTIDA  PICTURE  9(3)                     CI0073
                          VALUE                ZERO.                    CI0073
            13            GS1A-CTIDN.                                   CI0073
            14            GS1A-CTIDNP PICTURE  X(13)                    CI0073
                          VALUE                SPACE.                   CI0073
            14            GS1A-CTIDND PICTURE  9(11)                    CI0073
                          VALUE                ZERO.                    CI0073
            10            GS1A-CX12K.                                   CI0073
            11            GS1A-CPMTC  PICTURE  99                       CI0073
                          VALUE                ZERO.                    CI0073
            11            GS1A-NAPDS  PICTURE  S9(3)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            GS1A-GESTD  PICTURE  9(8)                     CI0073
                          VALUE                ZERO.                    CI0073
            10            GS1A-CX13K.                                   CI0073
            11            GS1A-CARTZ  PICTURE  99                       CI0073
                          VALUE                ZERO.                    CI0073
            11            GS1A-NAPDS  PICTURE  S9(3)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
      *!WF DSP=WS DSL=GS SEL=1A FOR=I DES=2 LEV=1 PLT=GS
       01                 WS00.                                         CI0073
            10            WS00-GT01.                                    CI0073
            11            WS00-GQ01K.                                   CI0073
            12            WS00-CANUMB PICTURE  X(27)                    CI0073
                          VALUE                SPACE.                   CI0073
            12            WS00-CAMCTR PICTURE  9(5)                     CI0073
                          VALUE                ZERO.                    CI0073
            12            WS00-GESQ2  PICTURE  99                       CI0073
                          VALUE                ZERO.                    CI0073
            10            WS00-GT02                                     CI0073
                          REDEFINES            WS00-GT01.               CI0073
            11            WS00-C199.                                    CI0073
            12            WS00-CLID.                                    CI0073
            13            WS00-CLIDO  PICTURE  9(3).                    CI0073
            13            WS00-CLIDN.                                   CI0073
            14            WS00-CLIDNP PICTURE  X(12).                   CI0073
            14            WS00-CLIDND PICTURE  9(8).                    CI0073
            10            WS00-GT03                                     CI0073
                          REDEFINES            WS00-GT01.               CI0073
            11            WS00-C299.                                    CI0073
            12            WS00-CTID.                                    CI0073
            13            WS00-CTIDA  PICTURE  9(3).                    CI0073
            13            WS00-CTIDN.                                   CI0073
            14            WS00-CTIDNP PICTURE  X(13).                   CI0073
            14            WS00-CTIDND PICTURE  9(11).                   CI0073
            10            WS00-GT04                                     CI0073
                          REDEFINES            WS00-GT01.               CI0073
            11            WS00-NPBN   PICTURE  X(20).                   CI0073
            10            WS00-GT05                                     CI0073
                          REDEFINES            WS00-GT01.               CI0073
            11            WS00-GR98.                                    CI0073
            12            WS00-GRID.                                    CI0073
            13            WS00-GRIDC  PICTURE  9(3).                    CI0073
            13            WS00-GRIDN.                                   CI0073
            14            WS00-GRIDNP PICTURE  99.                      CI0073
            14            WS00-GRIDND PICTURE  9(8).                    CI0073
            10            WS00-GT06                                     CI0073
                          REDEFINES            WS00-GT01.               CI0073
            11            WS00-NTR    PICTURE  9(8).                    CI0073
            10            WS00-GT07                                     CI0073
                          REDEFINES            WS00-GT01.               CI0073
            11            WS00-NTRAC  PICTURE  9(14).                   CI0073
            10            WS00-GT08                                     CI0073
                          REDEFINES            WS00-GT01.               CI0073
            11            WS00-NSRAB  PICTURE  9(7).                    CI0073
            11            WS00-GECKD  PICTURE  9.                       CI0073
            11            WS00-NBLCK  PICTURE  9(5).                    CI0073
            11            WS00-CTRID  PICTURE  X(4).                    CI0073
            10            WS00-GT19                                     CI0073
                          REDEFINES            WS00-GT01.               CI0073
            11            WS00-GEOPD2 PICTURE  X(8).                    CI0073
            10            WS00-CACKD  PICTURE  9                        CI0073
                          VALUE                ZERO.                    CI0073
            10            WS00-CENTT  PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            WS00-CADATE PICTURE  X(8)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            WS00-GETIM  PICTURE  S9(7)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            WS00-GEOPID PICTURE  X(6)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            WS00-CAUNIT PICTURE  X(4)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            WS00-XTERMI PICTURE  X(08)                    CI0073
                          VALUE                SPACE.                   CI0073
            10            WS00-CAPPL  PICTURE  X(8)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            WS00-CSYS   PICTURE  X(4)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            WS00-NTRSU  PICTURE  999                      CI0073
                          VALUE                ZERO.                    CI0073
            10            WS00-FILLER PICTURE  X(20)                    CI0073
                          VALUE                SPACE.                   CI0073
       01                 WS1A.                                         CI0073
            10            WS1A-NGEOPA PICTURE  X(08)                    CI0073
                          VALUE                SPACE.                   CI0073
            10            WS1A-CACLS1 PICTURE  X(20)                    CI0073
                          VALUE                SPACE.                   CI0073
            10            WS1A-CTRHO  PICTURE  9(8)                     CI0073
                          VALUE                ZERO.                    CI0073
            10            WS1A-GETIM3 PICTURE  S9(7)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            WS1A-CSLCT  PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            WS1A-GEOPD9 PICTURE  X(8)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            WS1A-GETIM  PICTURE  S9(7)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            WS1A-DCACG1 PICTURE  9(8)                     CI0073
                          VALUE                ZERO.                    CI0073
            10            WS1A-GEOPD2 PICTURE  X(8)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            WS1A-DCACG  PICTURE  9(8)                     CI0073
                          VALUE                ZERO.                    CI0073
            10            WS1A-GETIM2 PICTURE  S9(7)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            WS1A-CAVER  PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            WS1A-IWEBBT PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            WS1A-FILLER PICTURE  X(17)                    CI0073
                          VALUE                SPACE.                   CI0073
            10            WS1A-NGEOR  PICTURE  9(08)                    CI0073
                          VALUE                ZERO.                    CI0073
            10            WS1A-CACLS2 PICTURE  X(20)                    CI0073
                          VALUE                SPACE.                   CI0073
            10            WS1A-CAPID  PICTURE  9(2)                     CI0073
                          VALUE                ZERO.                    CI0073
            10            WS1A-CX01K.                                   CI0073
            11            WS1A-C199.                                    CI0073
            12            WS1A-CLID.                                    CI0073
            13            WS1A-CLIDO  PICTURE  9(3)                     CI0073
                          VALUE                ZERO.                    CI0073
            13            WS1A-CLIDN.                                   CI0073
            14            WS1A-CLIDNP PICTURE  X(12)                    CI0073
                          VALUE                SPACE.                   CI0073
            14            WS1A-CLIDND PICTURE  9(8)                     CI0073
                          VALUE                ZERO.                    CI0073
            10            WS1A-CX03K.                                   CI0073
            11            WS1A-CARTY  PICTURE  99                       CI0073
                          VALUE                ZERO.                    CI0073
            11            WS1A-NARRS  PICTURE  S9(3)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            WS1A-CX06K.                                   CI0073
            11            WS1A-C299.                                    CI0073
            12            WS1A-CTID.                                    CI0073
            13            WS1A-CTIDA  PICTURE  9(3)                     CI0073
                          VALUE                ZERO.                    CI0073
            13            WS1A-CTIDN.                                   CI0073
            14            WS1A-CTIDNP PICTURE  X(13)                    CI0073
                          VALUE                SPACE.                   CI0073
            14            WS1A-CTIDND PICTURE  9(11)                    CI0073
                          VALUE                ZERO.                    CI0073
            10            WS1A-CX12K.                                   CI0073
            11            WS1A-CPMTC  PICTURE  99                       CI0073
                          VALUE                ZERO.                    CI0073
            11            WS1A-NAPDS  PICTURE  S9(3)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            WS1A-GESTD  PICTURE  9(8)                     CI0073
                          VALUE                ZERO.                    CI0073
            10            WS1A-CX13K.                                   CI0073
            11            WS1A-CARTZ  PICTURE  99                       CI0073
                          VALUE                ZERO.                    CI0073
            11            WS1A-NAPDS  PICTURE  S9(3)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
      ******************************************************************
      **  MISCELLANEOUS TRANSACTION BUILD AREA                       ***
      ******************************************************************
      *!WF DSP=KY DSL=KY FOR=I LEV=1 PLT=KY
       01                 KY00.                                         CI0073
          05              KY00-00.                                      CI0073
            10            KY00-GS00.                                    CI0073
            11            KY00-GT01.                                    CI0073
            12            KY00-GQ01K.                                   CI0073
            13            KY00-CANUMB PICTURE  X(27).                   CI0073
            13            KY00-CAMCTR PICTURE  9(5).                    CI0073
            13            KY00-GESQ2  PICTURE  99.                      CI0073
            11            KY00-GT02                                     CI0073
                          REDEFINES            KY00-GT01.               CI0073
            12            KY00-C199.                                    CI0073
            13            KY00-CLID.                                    CI0073
            14            KY00-CLIDO  PICTURE  9(3).                    CI0073
            14            KY00-CLIDN.                                   CI0073
            15            KY00-CLIDNP PICTURE  X(12).                   CI0073
            15            KY00-CLIDND PICTURE  9(8).                    CI0073
            11            KY00-GT03                                     CI0073
                          REDEFINES            KY00-GT01.               CI0073
            12            KY00-C299.                                    CI0073
            13            KY00-CTID.                                    CI0073
            14            KY00-CTIDA  PICTURE  9(3).                    CI0073
            14            KY00-CTIDN.                                   CI0073
            15            KY00-CTIDNP PICTURE  X(13).                   CI0073
            15            KY00-CTIDND PICTURE  9(11).                   CI0073
            11            KY00-GT04                                     CI0073
                          REDEFINES            KY00-GT01.               CI0073
            12            KY00-NPBN   PICTURE  X(20).                   CI0073
            11            KY00-GT05                                     CI0073
                          REDEFINES            KY00-GT01.               CI0073
            12            KY00-GR98.                                    CI0073
            13            KY00-GRID.                                    CI0073
            14            KY00-GRIDC  PICTURE  9(3).                    CI0073
            14            KY00-GRIDN.                                   CI0073
            15            KY00-GRIDNP PICTURE  99.                      CI0073
            15            KY00-GRIDND PICTURE  9(8).                    CI0073
            11            KY00-GT06                                     CI0073
                          REDEFINES            KY00-GT01.               CI0073
            12            KY00-NTR    PICTURE  9(8).                    CI0073
            11            KY00-GT07                                     CI0073
                          REDEFINES            KY00-GT01.               CI0073
            12            KY00-NTRAC  PICTURE  9(14).                   CI0073
            11            KY00-GT08                                     CI0073
                          REDEFINES            KY00-GT01.               CI0073
            12            KY00-NSRAB  PICTURE  9(7).                    CI0073
            12            KY00-GECKD  PICTURE  9.                       CI0073
            12            KY00-NBLCK  PICTURE  9(5).                    CI0073
            12            KY00-CTRID  PICTURE  X(4).                    CI0073
            11            KY00-GT19                                     CI0073
                          REDEFINES            KY00-GT01.               CI0073
            12            KY00-GEOPD2 PICTURE  X(8).                    CI0073
            11            KY00-CACKD  PICTURE  9.                       CI0073
            11            KY00-CENTT  PICTURE  X.                       CI0073
            11            KY00-CADATE PICTURE  X(8).                    CI0073
            11            KY00-GETIM  PICTURE  S9(7)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            KY00-GEOPID PICTURE  X(6).                    CI0073
            11            KY00-CAUNIT PICTURE  X(4).                    CI0073
            11            KY00-XTERMI PICTURE  X(08).                   CI0073
            11            KY00-CAPPL  PICTURE  X(8).                    CI0073
            11            KY00-CSYS   PICTURE  X(4).                    CI0073
            11            KY00-NTRSU  PICTURE  999.                     CI0073
            11            KY00-FILLER PICTURE  X(20).                   CI0073
          05              KY00-SUITE.                                   CI0073
            15       FILLER         PICTURE  X(00599).                  CI0073
       01                 KY11  REDEFINES      KY00.                    CI0073
            10       FILLER         PICTURE  X(00101).                  CI0073
            10            KY11-GS11.                                    CI0073
            11            KY11-IBASU  PICTURE  X.                       CI0073
            11            KY11-CTRLR  PICTURE  9.                       CI0073
            11            KY11-CDEST  PICTURE  99.                      CI0073
            11            KY11-APMT   PICTURE  S9(5)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            KY11-CPMTC  PICTURE  99.                      CI0073
            11            KY11-CPMTF  PICTURE  99.                      CI0073
            11            KY11-DNPMT  PICTURE  9(8).                    CI0073
            10       FILLER         PICTURE  X(00579).                  CI0073
       01                 KY12  REDEFINES      KY00.                    CI0073
            10       FILLER         PICTURE  X(00101).                  CI0073
            10            KY12-GS12.                                    CI0073
            11            KY12-CINQD  PICTURE  X.                       CI0073
            10       FILLER         PICTURE  X(00598).                  CI0073
       01                 KY13  REDEFINES      KY00.                    CI0073
            10       FILLER         PICTURE  X(00101).                  CI0073
            10            KY13-GS13.                                    CI0073
            11            KY13-CDIRE  PICTURE  99.                      CI0073
            11            KY13-APBC2  PICTURE  9(8)V99                  CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            KY13-NBTCH8 PICTURE  9(8).                    CI0073
            11            KY13-IPACR  PICTURE  X.                       CI0073
            10            KY13-FILLER PICTURE  X(582).                  CI0073
       01                 KY52  REDEFINES      KY00.                    CI0073
            10       FILLER         PICTURE  X(00101).                  CI0073
            10            KY52-NAIDC  PICTURE  9(12).                   CI0073
            10            KY52-FILLER PICTURE  X(18).                   CI0073
            10       FILLER         PICTURE  X(00569).                  CI0073
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
      *-----> PCB Address list for calling module CI0091                AM0091
      *       Access to the Arrangement Database is required            AM0091
      *       AR1P - See -CD screen of module for segments accessed     AM0091
      *                                                                 AM0091
       01               CI0091-PCB-ADDRESS-LIST.                        AM0091
            05          CI0091-PCB-AR1P-PTR1        POINTER.            AM0091
            05          CI0091-PCB-DATP-PTR1        POINTER.            AM0091

      ******************************************************************
      *PARMS PASSED TO CI0091
      ******************************************************************
      *!WF DSP=PJ DSL=PJ SEL=08 FOR=I DES=1 LEV=1 PLT=PJ
       01                 PJ08.                                         CI0073
            10            PJ08-MAPPN  PICTURE  X(10).                   CI0073
            10            PJ08-CLID   PICTURE  X(23).                   CI0073
            10            PJ08-CARTY  PICTURE  99.                      CI0073
            10            PJ08-NARRS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            PJ08-CTID   PICTURE  X(27).                   CI0073
            10            PJ08-CARTZ  PICTURE  99.                      CI0073
            10            PJ08-NAPDS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            PJ08-NPISQ  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            PJ08-NBASQ  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            PJ08-CDEL1  PICTURE  9(3).                    CI0073
            10            PJ08-NDELS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            PJ08-IPERT  PICTURE  X.                       CI0073
            10            PJ08-NEIBT  PICTURE  X(7).                    CI0073
            10            PJ08-GESQ2C PICTURE  S99                      CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            PJ08-CACTM  PICTURE  X(1).                    CI0073
            10            PJ08-CPROT  PICTURE  X(02).                   CI0073
            10            PJ08-DCACG  PICTURE  9(8).                    CI0073
            10            PJ08-GECSQ  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            PJ08-ICX01  PICTURE  X.                       CI0073
            10            PJ08-ICX03  PICTURE  X.                       CI0073
            10            PJ08-ICX06  PICTURE  X.                       CI0073
            10            PJ08-ICX13  PICTURE  X.                       CI0073
            10            PJ08-ICX14  PICTURE  X.                       CI0073
            10            PJ08-ICX18  PICTURE  X.                       CI0073
            10            PJ08-ICX21  PICTURE  X.                       CI0073
            10            PJ08-CPMTF  PICTURE  99.                      CI0073
            10            PJ08-NPAIS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            PJ08-ICX09  PICTURE  X.                       CI0073
            10            PJ08-FILLER PICTURE  X(22).                   CI0073

      *HOLDING AREA FOR PARMS TO BE MOVED TO PJ08 (CI0091 PASS AREA)
       01  WS02.
      *!WI
           05  WS02-GECSQ
                        PICTURE S9(3)                                   CI0073
                          COMPUTATIONAL-3.                              CI0073
      *!WI
           05  WS02-CARTY
                        PICTURE 99.                                     CI0073
      *!WI
           05  WS02-NARRS
                        PICTURE S9(3)                                   CI0073
                          COMPUTATIONAL-3.                              CI0073
      *!WI
           05  WS02-CACTM
                        PICTURE X(1).                                   CI0073
      *!WI
           05  WS02-CPROT
                        PICTURE X(02).                                  CI0073
      *!WI
           05  WS02-DCACG
                        PICTURE 9(8).                                   CI0073
      ******************************************************************ADUTAB
      **              TABLE TC75 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TC75-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TC DSL=TA SEL=75 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TC75.                                                CI0073
           04    G-TC75-PARAM.                                          CI0073
             10  G-TC75-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0073
                        VALUE      +060.                                CI0073
             10  G-TC75-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0073
                        VALUE      +001.                                CI0073
             10  G-TC75-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0073
                        VALUE      +005.                                CI0073
             10  G-TC75-NUAPP  PICTURE 99                               CI0073
                        VALUE       0.                                  CI0073
             10  G-TC75-NUTAB  PICTURE X(6)                             CI0073
                        VALUE 'CAMCTR'.                                 CI0073
             10  G-TC75-TABFO  PICTURE XX                 VALUE SPACE.  CI0073
             10  G-TC75-TABCR  PICTURE XX                 VALUE SPACE.  CI0073
             10  G-TC75-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0073
             10  G-TC75-NUSSC  PICTURE X  VALUE   ' '.                  CI0073
             10  G-TC75-NUSSY  PICTURE X                  VALUE SPACE.  CI0073
             10  G-TC75-TRANID PICTURE X(4)               VALUE SPACE.  CI0073
             10  G-TC75-FILSYS.                                         CI0073
             15  G-TC75-USERC  PICTURE X(6)               VALUE SPACE.  CI0073
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0073
           04             TC75.                                         CI0073
            10            TC75-CAMCTR PICTURE  9(5)                     CI0073
                          VALUE                ZERO.                    CI0073
            10            TC75-TTDES  PICTURE  X(36)                    CI0073
                          VALUE                SPACE.                   CI0073
            10            TC75-MSYSID PICTURE  X(8)                     CI0073
                          VALUE                SPACE.                   CI0073
            10            TC75-NDLEN  PICTURE  S9(4)                    CI0073
                          VALUE                ZERO.                    CI0073
            10            TC75-IMIND1 PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            TC75-IMIND2 PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            TC75-IMIND3 PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            TC75-IMIND5 PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            TC75-IMIND7 PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            TC75-IMIND8 PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            10            TC75-IMINE  PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
      **                                                                ADUTAB
       01                 WQ01.                                         CI0073
            10            WQ01-GELL   PICTURE  9(4)                     CI0073
                          VALUE                ZERO                     CI0073
                          BINARY.                                       CI0073
            10            WQ01-GMISC.                                   CI0073
            11            WQ01-GS00.                                    CI0073
            12            WQ01-GT01.                                    CI0073
            13            WQ01-GQ01K.                                   CI0073
            14            WQ01-CANUMB PICTURE  X(27)                    CI0073
                          VALUE                SPACE.                   CI0073
            14            WQ01-CAMCTR PICTURE  9(5)                     CI0073
                          VALUE                ZERO.                    CI0073
            14            WQ01-GESQ2  PICTURE  99                       CI0073
                          VALUE                ZERO.                    CI0073
            12            WQ01-GT02                                     CI0073
                          REDEFINES            WQ01-GT01.               CI0073
            13            WQ01-C199.                                    CI0073
            14            WQ01-CLID.                                    CI0073
            15            WQ01-CLIDO  PICTURE  9(3).                    CI0073
            15            WQ01-CLIDN.                                   CI0073
            16            WQ01-CLIDNP PICTURE  X(12).                   CI0073
            16            WQ01-CLIDND PICTURE  9(8).                    CI0073
            12            WQ01-GT03                                     CI0073
                          REDEFINES            WQ01-GT01.               CI0073
            13            WQ01-C299.                                    CI0073
            14            WQ01-CTID.                                    CI0073
            15            WQ01-CTIDA  PICTURE  9(3).                    CI0073
            15            WQ01-CTIDN.                                   CI0073
            16            WQ01-CTIDNP PICTURE  X(13).                   CI0073
            16            WQ01-CTIDND PICTURE  9(11).                   CI0073
            12            WQ01-GT04                                     CI0073
                          REDEFINES            WQ01-GT01.               CI0073
            13            WQ01-NPBN   PICTURE  X(20).                   CI0073
            12            WQ01-GT05                                     CI0073
                          REDEFINES            WQ01-GT01.               CI0073
            13            WQ01-GR98.                                    CI0073
            14            WQ01-GRID.                                    CI0073
            15            WQ01-GRIDC  PICTURE  9(3).                    CI0073
            15            WQ01-GRIDN.                                   CI0073
            16            WQ01-GRIDNP PICTURE  99.                      CI0073
            16            WQ01-GRIDND PICTURE  9(8).                    CI0073
            12            WQ01-GT06                                     CI0073
                          REDEFINES            WQ01-GT01.               CI0073
            13            WQ01-NTR    PICTURE  9(8).                    CI0073
            12            WQ01-GT07                                     CI0073
                          REDEFINES            WQ01-GT01.               CI0073
            13            WQ01-NTRAC  PICTURE  9(14).                   CI0073
            12            WQ01-GT08                                     CI0073
                          REDEFINES            WQ01-GT01.               CI0073
            13            WQ01-NSRAB  PICTURE  9(7).                    CI0073
            13            WQ01-GECKD  PICTURE  9.                       CI0073
            13            WQ01-NBLCK  PICTURE  9(5).                    CI0073
            13            WQ01-CTRID  PICTURE  X(4).                    CI0073
            12            WQ01-GT19                                     CI0073
                          REDEFINES            WQ01-GT01.               CI0073
            13            WQ01-GEOPD2 PICTURE  X(8).                    CI0073
            12            WQ01-CACKD  PICTURE  9                        CI0073
                          VALUE                ZERO.                    CI0073
            12            WQ01-CENTT  PICTURE  X                        CI0073
                          VALUE                SPACE.                   CI0073
            12            WQ01-CADATE PICTURE  X(8)                     CI0073
                          VALUE                SPACE.                   CI0073
            12            WQ01-GETIM  PICTURE  S9(7)                    CI0073
                          VALUE                ZERO                     CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WQ01-GEOPID PICTURE  X(6)                     CI0073
                          VALUE                SPACE.                   CI0073
            12            WQ01-CAUNIT PICTURE  X(4)                     CI0073
                          VALUE                SPACE.                   CI0073
            12            WQ01-XTERMI PICTURE  X(08)                    CI0073
                          VALUE                SPACE.                   CI0073
            12            WQ01-CAPPL  PICTURE  X(8)                     CI0073
                          VALUE                SPACE.                   CI0073
            12            WQ01-CSYS   PICTURE  X(4)                     CI0073
                          VALUE                SPACE.                   CI0073
            12            WQ01-NTRSU  PICTURE  999                      CI0073
                          VALUE                ZERO.                    CI0073
            12            WQ01-FILLER PICTURE  X(20)                    CI0073
                          VALUE                SPACE.                   CI0073
            11            WQ01-XMISL  PICTURE  X(599)                   CI0073
                          VALUE                SPACE.                   CI0073

      ******************************************************************
      **     WORKING STORAGE MISC FIELDS                               *
      ******************************************************************

      *ARRANGEMENT ACTION CODE; INPUT WZ7D-CACTA IS MOVED TO HERE
       01  7-CACTA                PIC X(01).
           88  7-CACTA-VALID      VALUE 'A', 'C', 'D', 'I', 'R'.
           88  7-CACTA-ADD        VALUE 'A'.
           88  7-CACTA-CHANGE     VALUE 'C'.
           88  7-CACTA-DELETE     VALUE 'D'.
           88  7-CACTA-INACTIVATE VALUE 'I'.
           88  7-CACTA-REACTIVATE VALUE 'R'.

      *ARRANGEMENT DETAIL TYPE CODE; INPUT WZ7D-CARTZF IS MOVED TO HERE
       01  7-CARTZ                PIC 9(02).
           88  7-CARTZ-VALID      VALUE 01, 02, 05, 06, 07, 10, 14.
           88  7-CARTZ-SP         VALUE 01.
           88  7-CARTZ-CP         VALUE 02.
           88  7-CARTZ-DIV        VALUE 05.
           88  7-CARTZ-CI         VALUE 06.
           88  7-CARTZ-FUNDS      VALUE 01, 05.
           88  7-CARTZ-CERTS      VALUE 02, 06.
           88  7-CARTZ-BROK       VALUE 10, 14.
           88  7-CARTZ-AP         VALUE 07.

       01  7-MISC-FIELDS.

      *    COUNT OF CX14S READ AT BEGINNING OF THIS MODULE
           05  7-CX14S-READ                PIC S9(03) VALUE ZERO.

      *    USED TO COUNT # OF UNSUCCESSFUL INSERT ATTEMPTS FOR
      *    CX13 & CX14
           05  7-NBR-OF-INSERT-TRIES       PIC S9(03) VALUE ZERO.

      *    USED TO LIMIT # OF CX13 & CX14 INSERT ATTEMPTS
           05  7-MAX-NBR-OF-INSERT-TRIES   PIC S9(03) VALUE 998.

      *    SET TO 'Y' WHEN CX13 INSERT SUCCESSFUL
           05  7-CX13-INSERTED             PIC  X(01) VALUE 'N'.

      *    SET TO 'Y' WHEN CX14 INSERT SUCCESSFUL
           05  7-CX14-INSERTED             PIC  X(01) VALUE 'N'.

      *    SET TO 'Y' WHEN AN EXISTING CX13 IS FOUND ON THE DATABASE
      *    WHICH IS MARKED 'COMPLETE' AND MATCHES THE CX13 PASSED TO
      *    TO THIS MODULE (IN WZ7D) IN THE FOLLOWING FIELDS:
      *      CARTZ - ARR TYPE              DNPMT - NEXT PMT DT
      *      GEEND - ARR END DT            CPMTF - PMT FREQ
      *      CDEST - STAT OF ACCT TO ARR   CIRMO - IRREG MO PMT
           05  7-CX13-MATCH                PIC  X(01) VALUE 'N'.

      *    USED FOR CERT SYS PARTIAL PMTS.
      *    SET TO 'Y' WHEN A CX13 FIELD HAS CHANGED WHICH REQUIRES
      *    YOU TO MOVE THE CX14 TO ANOTHER CX13.
           05  7-CP-PROCESSED              PIC  X(01) VALUE 'N'.
       01   WS-CF   PIC X(1)  VALUE '0'.
      *!WI
       01 WS-CX01K
                        PICTURE X(23).                                  CI0073
      *!WI
       01 WS-CX03K
                        PICTURE X(4).                                   CI0073
      *!WI
       01 WS-CX06K
                        PICTURE X(27).                                  CI0073
      *!WI
       01 WS-CX13K
                        PICTURE X(04).                                  CI0073
      *!WI
       01 WS-GELL
                        PICTURE 9(4)                                    CI0073
                          BINARY.                                       CI0073

       01   DEBUT-WSS.                                                  CI0073
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0073
            05   IK     PICTURE X.                                      CI0073
       01  CONSTANTES-PAC.                                              CI0073
           05  FILLER  PICTURE X(87)   VALUE                            CI0073
                     '6015 CAT09/08/14CI0073ADMIN   14:34:36CI0073P AMERCI0073
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0073
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0073
           05  NUGNA   PICTURE X(5).                                    CI0073
           05  APPLI   PICTURE X(3).                                    CI0073
           05  DATGN   PICTURE X(8).                                    CI0073
           05  PROGR   PICTURE X(6).                                    CI0073
           05  CODUTI  PICTURE X(8).                                    CI0073
           05  TIMGN   PICTURE X(8).                                    CI0073
           05  PROGE   PICTURE X(8).                                    CI0073
           05  COBASE  PICTURE X(4).                                    CI0073
           05  DATGNC  PICTURE X(10).                                   CI0073
           05  RELEAS  PICTURE X(7).                                    CI0073
           05  DATGE   PICTURE X(10).                                   CI0073
           05  DATSQ   PICTURE X(10).                                   CI0073
       01  DATCE.                                                       CI0073
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0073
         05  DATOR.                                                     CI0073
           10  DATOA  PICTURE XX.                                       CI0073
           10  DATOM  PICTURE XX.                                       CI0073
           10  DATOJ  PICTURE XX.                                       CI0073
       01   VARIABLES-CONDITIONNELLES.                                  CI0073
            05                  FT      PICTURE X VALUE '0'.            CI0073
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0073
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0073
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0073
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0073
            05       5-GQ00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0073
            05       5-WQ00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0073
       01               S-CX01-SSA.                                     CI0073
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0073
                                      VALUE 'CX01    '.                 CI0073
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0073
            10          S-CX01-CCOD   PICTURE X(5)                      CI0073
                                      VALUE '-----'.                    CI0073
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0073
       01            S-CXU01-SSA.                                       CI0073
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX01    '.                 CI0073
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0073
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(CX01K'.                   CI0073
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0073
            10       S-CXU01-CX01K.                                     CI0073
            11       S-CXU01-C199.                                      CI0073
            12       S-CXU01-CLID.                                      CI0073
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0073
            13       S-CXU01-CLIDN.                                     CI0073
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0073
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0073
            10  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01               S-CX03-SSA.                                     CI0073
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0073
                                      VALUE 'CX03    '.                 CI0073
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0073
            10          S-CX03-CCOD   PICTURE X(5)                      CI0073
                                      VALUE '-----'.                    CI0073
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0073
       01            S-CXA03-SSA.                                       CI0073
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX03    '.                 CI0073
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0073
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(CARTY'.                   CI0073
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0073
            12       S-CXA03-CARTY    PICTURE  99.                      CI0073
            12  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01            S-CXB03-SSA.                                       CI0073
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX03    '.                 CI0073
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0073
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(NARRS'.                   CI0073
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0073
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            12  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01            S-CXC03-SSA.                                       CI0073
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX03    '.                 CI0073
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0073
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(CPMTG'.                   CI0073
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0073
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0073
            11  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01            S-CXD03-SSA.                                       CI0073
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX03    '.                 CI0073
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0073
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(GRCRNG'.                  CI0073
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0073
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0073
            11  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01            S-CXE03-SSA.                                       CI0073
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX03    '.                 CI0073
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0073
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(DEXDT'.                   CI0073
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0073
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0073
            11  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01            S-CXF03-SSA.                                       CI0073
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX03    '.                 CI0073
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0073
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(CY50'.                    CI0073
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0073
            11       S-CXF03-CY50.                                      CI0073
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0073
            11  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01            S-CXG03-SSA.                                       CI0073
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX03    '.                 CI0073
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0073
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(NBASQ'.                   CI0073
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0073
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01            S-CXH03-SSA.                                       CI0073
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX03    '.                 CI0073
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0073
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(NARID'.                   CI0073
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0073
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0073
            12  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01            S-CXU03-SSA.                                       CI0073
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX03    '.                 CI0073
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0073
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(CX03K'.                   CI0073
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0073
            11       S-CXU03-CX03K.                                     CI0073
            12       S-CXU03-CARTY    PICTURE  99.                      CI0073
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01               S-CX06-SSA.                                     CI0073
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0073
                                      VALUE 'CX06    '.                 CI0073
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0073
            10          S-CX06-CCOD   PICTURE X(5)                      CI0073
                                      VALUE '-----'.                    CI0073
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0073
       01            S-CXU06-SSA.                                       CI0073
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX06    '.                 CI0073
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0073
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(CX06K'.                   CI0073
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0073
            10       S-CXU06-CX06K.                                     CI0073
            11       S-CXU06-C299.                                      CI0073
            12       S-CXU06-CTID.                                      CI0073
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0073
            13       S-CXU06-CTIDN.                                     CI0073
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0073
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0073
            10  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01               S-CX13-SSA.                                     CI0073
            10         S1-CX13-SEGNAM PICTURE X(8)                      CI0073
                                      VALUE 'CX13    '.                 CI0073
            10         S1-CX13-CCOM   PICTURE X VALUE '*'.              CI0073
            10          S-CX13-CCOD   PICTURE X(5)                      CI0073
                                      VALUE '-----'.                    CI0073
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0073
       01            S-CXA13-SSA.                                       CI0073
            11      S1-CXA13-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX13    '.                 CI0073
            11      S1-CXA13-CCOM   PICTURE X VALUE '*'.                CI0073
            11       S-CXA13-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            11      S1-CXA13-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(CDEST'.                   CI0073
            11       S-CXA13-OPER  PICTURE XX VALUE ' ='.               CI0073
            11       S-CXA13-CDEST    PICTURE  99.                      CI0073
            11  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01            S-CXB13-SSA.                                       CI0073
            12      S1-CXB13-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX13    '.                 CI0073
            12      S1-CXB13-CCOM   PICTURE X VALUE '*'.                CI0073
            12       S-CXB13-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            12      S1-CXB13-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(CARTZ'.                   CI0073
            12       S-CXB13-OPER  PICTURE XX VALUE ' ='.               CI0073
            12       S-CXB13-CARTZ    PICTURE  99.                      CI0073
            12  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01            S-CXC13-SSA.                                       CI0073
            12      S1-CXC13-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX13    '.                 CI0073
            12      S1-CXC13-CCOM   PICTURE X VALUE '*'.                CI0073
            12       S-CXC13-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            12      S1-CXC13-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(NAPDS'.                   CI0073
            12       S-CXC13-OPER  PICTURE XX VALUE ' ='.               CI0073
            12       S-CXC13-NAPDS    PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            12  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01            S-CXU13-SSA.                                       CI0073
            11      S1-CXU13-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX13    '.                 CI0073
            11      S1-CXU13-CCOM   PICTURE X VALUE '*'.                CI0073
            11       S-CXU13-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            11      S1-CXU13-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(CX13K'.                   CI0073
            11       S-CXU13-OPER  PICTURE XX VALUE ' ='.               CI0073
            11       S-CXU13-CX13K.                                     CI0073
            12       S-CXU13-CARTZ    PICTURE  99.                      CI0073
            12       S-CXU13-NAPDS    PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01            S-CX113-SSA.                                       CI0073
            11      S1-CX113-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX13    '.                 CI0073
            11      S1-CX113-CCOM   PICTURE X VALUE '*'.                CI0073
            11       S-CX113-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            11      S1-CX113-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(XGCUSPY'.                 CI0073
            11       S-CX113-OPER  PICTURE XX VALUE ' ='.               CI0073
            11       S-CX113-GCUSPY   PICTURE  X(12).                   CI0073
            11  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01               S-CX14-SSA.                                     CI0073
            10         S1-CX14-SEGNAM PICTURE X(8)                      CI0073
                                      VALUE 'CX14    '.                 CI0073
            10         S1-CX14-CCOM   PICTURE X VALUE '*'.              CI0073
            10          S-CX14-CCOD   PICTURE X(5)                      CI0073
                                      VALUE '-----'.                    CI0073
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0073
       01            S-CXU14-SSA.                                       CI0073
            10      S1-CXU14-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX14    '.                 CI0073
            10      S1-CXU14-CCOM   PICTURE X VALUE '*'.                CI0073
            10       S-CXU14-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            10      S1-CXU14-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(CX14K'.                   CI0073
            10       S-CXU14-OPER  PICTURE XX VALUE ' ='.               CI0073
            10       S-CXU14-CX14K.                                     CI0073
            11       S-CXU14-NPISQ    PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01            S-CX114-SSA.                                       CI0073
            11      S1-CX114-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'CX14    '.                 CI0073
            11      S1-CX114-CCOM   PICTURE X VALUE '*'.                CI0073
            11       S-CX114-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            11      S1-CX114-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(XGCUSPZ'.                 CI0073
            11       S-CX114-OPER  PICTURE XX VALUE ' ='.               CI0073
            11       S-CX114-GCUSPZ   PICTURE  X(12).                   CI0073
            11  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01               S-GQ01-SSA.                                     CI0073
            10         S1-GQ01-SEGNAM PICTURE X(8)                      CI0073
                                      VALUE 'GQ01    '.                 CI0073
            10         S1-GQ01-CCOM   PICTURE X VALUE '*'.              CI0073
            10          S-GQ01-CCOD   PICTURE X(5)                      CI0073
                                      VALUE '-----'.                    CI0073
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0073
       01            S-GQU01-SSA.                                       CI0073
            13      S1-GQU01-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'GQ01    '.                 CI0073
            13      S1-GQU01-CCOM   PICTURE X VALUE '*'.                CI0073
            13       S-GQU01-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            13      S1-GQU01-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(GQ01K'.                   CI0073
            13       S-GQU01-OPER  PICTURE XX VALUE ' ='.               CI0073
            13       S-GQU01-GQ01K.                                     CI0073
            14       S-GQU01-CANUMB   PICTURE  X(27).                   CI0073
            14       S-GQU01-CAMCTR   PICTURE  9(5).                    CI0073
            14       S-GQU01-GESQ2    PICTURE  99.                      CI0073
            13  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01            S-GQ701-SSA.                                       CI0073
            14      S1-GQ701-SEGNAM PICTURE X(8)                        CI0073
                                      VALUE 'GQ01    '.                 CI0073
            14      S1-GQ701-CCOM   PICTURE X VALUE '*'.                CI0073
            14       S-GQ701-CCOD   PICTURE X(5)                        CI0073
                                      VALUE '-----'.                    CI0073
            14      S1-GQ701-FLDNAM PICTURE X(9)                        CI0073
                                      VALUE '(XCANUMB'.                 CI0073
            14       S-GQ701-OPER  PICTURE XX VALUE ' ='.               CI0073
            14       S-GQ701-CANUMB   PICTURE  X(27).                   CI0073
            14  FILLER   PICTURE X    VALUE ')'.                        CI0073
       01   ZONES-UTILISATEUR PICTURE X.                                CI0073
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
      ** PCB POINTER FOR DATP                                           ADU015
            05 PCB-DATP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR TR1P                                           ADU015
            05 PCB-TR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0073
          05              PA00-SUITE.                                   CI0073
            15       FILLER         PICTURE  X(00106).                  CI0073
       01                 PA06  REDEFINES      PA00.                    CI0073
            10            PA06-XDBPCB.                                  CI0073
            11            PA06-XDBDNM PICTURE  X(08).                   CI0073
            11            PA06-XSEGLV PICTURE  X(02).                   CI0073
            11            PA06-XRC    PICTURE  X(02).                   CI0073
            11            PA06-XPROPT PICTURE  X(04).                   CI0073
            11            PA06-FILLER PICTURE  S9(5)                    CI0073
                          BINARY.                                       CI0073
            11            PA06-XSEGNM PICTURE  X(08).                   CI0073
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0073
                          BINARY.                                       CI0073
            11            PA06-XSEGNB PICTURE  9(05)                    CI0073
                          BINARY.                                       CI0073
            11            PA06-XCOKEY PICTURE  X(70).                   CI0073
      *** PCB MASK FOR DATP                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0073
          05              PB00-SUITE.                                   CI0073
            15       FILLER         PICTURE  X(00106).                  CI0073
       01                 PB06  REDEFINES      PB00.                    CI0073
            10            PB06-XDBPCB.                                  CI0073
            11            PB06-XDBDNM PICTURE  X(08).                   CI0073
            11            PB06-XSEGLV PICTURE  X(02).                   CI0073
            11            PB06-XRC    PICTURE  X(02).                   CI0073
            11            PB06-XPROPT PICTURE  X(04).                   CI0073
            11            PB06-FILLER PICTURE  S9(5)                    CI0073
                          BINARY.                                       CI0073
            11            PB06-XSEGNM PICTURE  X(08).                   CI0073
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0073
                          BINARY.                                       CI0073
            11            PB06-XSEGNB PICTURE  9(05)                    CI0073
                          BINARY.                                       CI0073
            11            PB06-XCOKEY PICTURE  X(70).                   CI0073
      *** PCB MASK FOR TR1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0073
          05              PC00-SUITE.                                   CI0073
            15       FILLER         PICTURE  X(00106).                  CI0073
       01                 PC06  REDEFINES      PC00.                    CI0073
            10            PC06-XDBPCB.                                  CI0073
            11            PC06-XDBDNM PICTURE  X(08).                   CI0073
            11            PC06-XSEGLV PICTURE  X(02).                   CI0073
            11            PC06-XRC    PICTURE  X(02).                   CI0073
            11            PC06-XPROPT PICTURE  X(04).                   CI0073
            11            PC06-FILLER PICTURE  S9(5)                    CI0073
                          BINARY.                                       CI0073
            11            PC06-XSEGNM PICTURE  X(08).                   CI0073
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0073
                          BINARY.                                       CI0073
            11            PC06-XSEGNB PICTURE  9(05)                    CI0073
                          BINARY.                                       CI0073
            11            PC06-XCOKEY PICTURE  X(70).                   CI0073

      ******************************************************************
      *'INPUT' TO CI0073
      ******************************************************************
      *!WF DSP=WZ DSL=DU SEL=7D FOR=I DES=1 LEV=1 PLT=10
       01                 WZ7D.                                         CI0073
            10            WZ7D-MAPPN  PICTURE  X(10).                   CI0073
            10            WZ7D-CACTA  PICTURE  X(1).                    CI0073
            10            WZ7D-CLID   PICTURE  X(23).                   CI0073
            10            WZ7D-CARTY  PICTURE  99.                      CI0073
            10            WZ7D-NARRS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            WZ7D-CTID01 PICTURE  X(27).                   CI0073
            10            WZ7D-CARTZF PICTURE  99.                      CI0073
            10            WZ7D-NAPDSF PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            WZ7D-NPISQF PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            WZ7D-GETOD  PICTURE  9(6).                    CI0073
            10            WZ7D-CTRHO  PICTURE  9(8).                    CI0073
            10            WZ7D-GEOPD3 PICTURE  X(8).                    CI0073
            10            WZ7D-CX13.                                    CI0073
            11            WZ7D-GELL   PICTURE  9(4)                     CI0073
                          BINARY.                                       CI0073
            11            WZ7D-CY20.                                    CI0073
            12            WZ7D-CX13K.                                   CI0073
            13            WZ7D-CARTZ  PICTURE  99.                      CI0073
            13            WZ7D-NAPDS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7D-GESTD  PICTURE  9(8).                    CI0073
            12            WZ7D-GEEND  PICTURE  9(8).                    CI0073
            12            WZ7D-DASUQ  PICTURE  9(8).                    CI0073
            12            WZ7D-CDEST  PICTURE  99.                      CI0073
            12            WZ7D-IIARR  PICTURE  X.                       CI0073
            12            WZ7D-DLAUP  PICTURE  9(8).                    CI0073
            12            WZ7D-GEOPD2 PICTURE  X(8).                    CI0073
            12            WZ7D-GEAUN  PICTURE  9(5).                    CI0073
            12            WZ7D-DPCHD  PICTURE  9(8).                    CI0073
            12            WZ7D-PPOT1  PICTURE  S9(3)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7D-ACOT1  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7D-QPST1  PICTURE  S9(7)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7D-FILLER PICTURE  X(03).                   CI0073
            11            WZ7D-CY96.                                    CI0073
            12            WZ7D-FILLER PICTURE  X(50).                   CI0073
            11            WZ7D-CY21                                     CI0073
                          REDEFINES            WZ7D-CY96.               CI0073
            12            WZ7D-DNPMT  PICTURE  9(8).                    CI0073
            12            WZ7D-CPMTF  PICTURE  99.                      CI0073
            12            WZ7D-ADBRQ  PICTURE  S9(11)V99                CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7D-QSHOWQ PICTURE  S9(9)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7D-PACT1  PICTURE  S999V999                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7D-DOPDA  PICTURE  99.                      CI0073
            12            WZ7D-DNEXE  PICTURE  9(8).                    CI0073
            12            WZ7D-CIRMO  PICTURE  X(12).                   CI0073
            11            WZ7D-CY98.                                    CI0073
            12            WZ7D-FILLER PICTURE  X(120).                  CI0073
            11            WZ7D-CY25                                     CI0073
                          REDEFINES            WZ7D-CY98.               CI0073
            12            WZ7D-COPTC  PICTURE  9(1).                    CI0073
            12            WZ7D-ILPOI  PICTURE  X(1).                    CI0073
            12            WZ7D-CATOC  PICTURE  X(1).                    CI0073
            12            WZ7D-CEOIA  PICTURE  S9(7)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7D-ACOAR  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7D-CEOTR  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7D-DSTMO  PICTURE  99.                      CI0073
            11            WZ7D-CY27                                     CI0073
                          REDEFINES            WZ7D-CY98.               CI0073
            12            WZ7D-QMTH1  PICTURE  9(3).                    CI0073
            12            WZ7D-IDRMD  PICTURE  X.                       CI0073
            11            WZ7D-CY28                                     CI0073
                          REDEFINES            WZ7D-CY98.               CI0073
            12            WZ7D-AALLBL PICTURE  S9(8)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7D-PSURR  PICTURE  S9(3)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7D-DFPMT  PICTURE  9(8).                    CI0073
            12            WZ7D-QMTHLA PICTURE  9(3).                    CI0073
            12            WZ7D-PWHLDS PICTURE  S999V9(5)                CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7D-ISWHO  PICTURE  X(1).                    CI0073
            11            WZ7D-CY29                                     CI0073
                          REDEFINES            WZ7D-CY98.               CI0073
            12            WZ7D-IINDI1 PICTURE  X(1).                    CI0073
            12            WZ7D-IINDI2 PICTURE  X(1).                    CI0073
            12            WZ7D-IINDI3 PICTURE  X(1).                    CI0073
            12            WZ7D-PWHLD5 PICTURE  S999V99                  CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7D-CCSMQ  PICTURE  X.                       CI0073
            12            WZ7D-CPLEC  PICTURE  XX.                      CI0073
            12            WZ7D-IPTRDA PICTURE  X(01).                   CI0073
            12            WZ7D-GCUSPY PICTURE  X(12).                   CI0073
            12            WZ7D-ALOIDA PICTURE  S9(11)V99                CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7D-DELOI  PICTURE  9(8).                    CI0073
            12            WZ7D-CLGND  PICTURE  X.                       CI0073
            12            WZ7D-CORTYA PICTURE  X(3).                    CI0073
            12            WZ7D-CPH3U  PICTURE  X.                       CI0073
            12            WZ7D-CNAVR  PICTURE  X(1).                    CI0073
            12            WZ7D-NEXEC  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            WZ7D-CX14.                                    CI0073
            11            WZ7D-GELL   PICTURE  9(4)                     CI0073
                          BINARY.                                       CI0073
            11            WZ7D-CX14K.                                   CI0073
            12            WZ7D-NPISQ  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            WZ7D-ACOTD  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            WZ7D-PPOTD  PICTURE  S9(3)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            WZ7D-QPSTD  PICTURE  S9(7)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            WZ7D-CPITC  PICTURE  99.                      CI0073
            11            WZ7D-FILLER PICTURE  X(04).                   CI0073
            11            WZ7D-CY97.                                    CI0073
            12            WZ7D-FILLER PICTURE  X(32).                   CI0073
            11            WZ7D-CY30                                     CI0073
                          REDEFINES            WZ7D-CY97.               CI0073
            12            WZ7D-IOWNC  PICTURE  X.                       CI0073
            12            WZ7D-CTYPE  PICTURE  X.                       CI0073
            12            WZ7D-C299.                                    CI0073
            13            WZ7D-CTID.                                    CI0073
            14            WZ7D-CTIDA  PICTURE  9(3).                    CI0073
            14            WZ7D-CTIDN.                                   CI0073
            15            WZ7D-CTIDNP PICTURE  X(13).                   CI0073
            15            WZ7D-CTIDND PICTURE  9(11).                   CI0073
            12            WZ7D-CPMTC  PICTURE  99.                      CI0073
            12            WZ7D-IACSD  PICTURE  X.                       CI0073
            11            WZ7D-CY31                                     CI0073
                          REDEFINES            WZ7D-CY97.               CI0073
            12            WZ7D-FILLER PICTURE  X(2).                    CI0073
            12            WZ7D-IDELI  PICTURE  X.                       CI0073
            12            WZ7D-CDEL1  PICTURE  9(3).                    CI0073
            12            WZ7D-NDELS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            WZ7D-CY32                                     CI0073
                          REDEFINES            WZ7D-CY97.               CI0073
            12            WZ7D-GCUSPZ PICTURE  X(12).                   CI0073
       01                 WZ7E.                                         CI0073
            10            WZ7E-CX13.                                    CI0073
            11            WZ7E-GELL   PICTURE  9(4)                     CI0073
                          BINARY.                                       CI0073
            11            WZ7E-CY20.                                    CI0073
            12            WZ7E-CX13K.                                   CI0073
            13            WZ7E-CARTZ  PICTURE  99.                      CI0073
            13            WZ7E-NAPDS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7E-GESTD  PICTURE  9(8).                    CI0073
            12            WZ7E-GEEND  PICTURE  9(8).                    CI0073
            12            WZ7E-DASUQ  PICTURE  9(8).                    CI0073
            12            WZ7E-CDEST  PICTURE  99.                      CI0073
            12            WZ7E-IIARR  PICTURE  X.                       CI0073
            12            WZ7E-DLAUP  PICTURE  9(8).                    CI0073
            12            WZ7E-GEOPD2 PICTURE  X(8).                    CI0073
            12            WZ7E-GEAUN  PICTURE  9(5).                    CI0073
            12            WZ7E-DPCHD  PICTURE  9(8).                    CI0073
            12            WZ7E-PPOT1  PICTURE  S9(3)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7E-ACOT1  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7E-QPST1  PICTURE  S9(7)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7E-FILLER PICTURE  X(03).                   CI0073
            11            WZ7E-CY96.                                    CI0073
            12            WZ7E-FILLER PICTURE  X(50).                   CI0073
            11            WZ7E-CY21                                     CI0073
                          REDEFINES            WZ7E-CY96.               CI0073
            12            WZ7E-DNPMT  PICTURE  9(8).                    CI0073
            12            WZ7E-CPMTF  PICTURE  99.                      CI0073
            12            WZ7E-ADBRQ  PICTURE  S9(11)V99                CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7E-QSHOWQ PICTURE  S9(9)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7E-PACT1  PICTURE  S999V999                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7E-DOPDA  PICTURE  99.                      CI0073
            12            WZ7E-DNEXE  PICTURE  9(8).                    CI0073
            12            WZ7E-CIRMO  PICTURE  X(12).                   CI0073
            11            WZ7E-CY98.                                    CI0073
            12            WZ7E-FILLER PICTURE  X(120).                  CI0073
            11            WZ7E-CY25                                     CI0073
                          REDEFINES            WZ7E-CY98.               CI0073
            12            WZ7E-COPTC  PICTURE  9(1).                    CI0073
            12            WZ7E-ILPOI  PICTURE  X(1).                    CI0073
            12            WZ7E-CATOC  PICTURE  X(1).                    CI0073
            12            WZ7E-CEOIA  PICTURE  S9(7)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7E-ACOAR  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7E-CEOTR  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7E-DSTMO  PICTURE  99.                      CI0073
            11            WZ7E-CY27                                     CI0073
                          REDEFINES            WZ7E-CY98.               CI0073
            12            WZ7E-QMTH1  PICTURE  9(3).                    CI0073
            12            WZ7E-IDRMD  PICTURE  X.                       CI0073
            11            WZ7E-CY28                                     CI0073
                          REDEFINES            WZ7E-CY98.               CI0073
            12            WZ7E-AALLBL PICTURE  S9(8)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7E-PSURR  PICTURE  S9(3)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7E-DFPMT  PICTURE  9(8).                    CI0073
            12            WZ7E-QMTHLA PICTURE  9(3).                    CI0073
            12            WZ7E-PWHLDS PICTURE  S999V9(5)                CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7E-ISWHO  PICTURE  X(1).                    CI0073
            11            WZ7E-CY29                                     CI0073
                          REDEFINES            WZ7E-CY98.               CI0073
            12            WZ7E-IINDI1 PICTURE  X(1).                    CI0073
            12            WZ7E-IINDI2 PICTURE  X(1).                    CI0073
            12            WZ7E-IINDI3 PICTURE  X(1).                    CI0073
            12            WZ7E-PWHLD5 PICTURE  S999V99                  CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7E-CCSMQ  PICTURE  X.                       CI0073
            12            WZ7E-CPLEC  PICTURE  XX.                      CI0073
            12            WZ7E-IPTRDA PICTURE  X(01).                   CI0073
            12            WZ7E-GCUSPY PICTURE  X(12).                   CI0073
            12            WZ7E-ALOIDA PICTURE  S9(11)V99                CI0073
                          COMPUTATIONAL-3.                              CI0073
            12            WZ7E-DELOI  PICTURE  9(8).                    CI0073
            12            WZ7E-CLGND  PICTURE  X.                       CI0073
            12            WZ7E-CORTYA PICTURE  X(3).                    CI0073
            12            WZ7E-CPH3U  PICTURE  X.                       CI0073
            12            WZ7E-CNAVR  PICTURE  X(1).                    CI0073
            12            WZ7E-NEXEC  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            WZ7E-CX14.                                    CI0073
            11            WZ7E-GELL   PICTURE  9(4)                     CI0073
                          BINARY.                                       CI0073
            11            WZ7E-CX14K.                                   CI0073
            12            WZ7E-NPISQ  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            WZ7E-ACOTD  PICTURE  S9(9)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            WZ7E-PPOTD  PICTURE  S9(3)V99                 CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            WZ7E-QPSTD  PICTURE  S9(7)V999                CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            WZ7E-CPITC  PICTURE  99.                      CI0073
            11            WZ7E-FILLER PICTURE  X(04).                   CI0073
            11            WZ7E-CY97.                                    CI0073
            12            WZ7E-FILLER PICTURE  X(32).                   CI0073
            11            WZ7E-CY30                                     CI0073
                          REDEFINES            WZ7E-CY97.               CI0073
            12            WZ7E-IOWNC  PICTURE  X.                       CI0073
            12            WZ7E-CTYPE  PICTURE  X.                       CI0073
            12            WZ7E-C299.                                    CI0073
            13            WZ7E-CTID.                                    CI0073
            14            WZ7E-CTIDA  PICTURE  9(3).                    CI0073
            14            WZ7E-CTIDN.                                   CI0073
            15            WZ7E-CTIDNP PICTURE  X(13).                   CI0073
            15            WZ7E-CTIDND PICTURE  9(11).                   CI0073
            12            WZ7E-CPMTC  PICTURE  99.                      CI0073
            12            WZ7E-IACSD  PICTURE  X.                       CI0073
            11            WZ7E-CY31                                     CI0073
                          REDEFINES            WZ7E-CY97.               CI0073
            12            WZ7E-FILLER PICTURE  X(2).                    CI0073
            12            WZ7E-IDELI  PICTURE  X.                       CI0073
            12            WZ7E-CDEL1  PICTURE  9(3).                    CI0073
            12            WZ7E-NDELS  PICTURE  S9(3)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            WZ7E-CY32                                     CI0073
                          REDEFINES            WZ7E-CY97.               CI0073
            12            WZ7E-GCUSPZ PICTURE  X(12).                   CI0073

      ******************************************************************
      *'OUTPUT' FROM CI0073.
      *INITIALIZED TO THE INPUT VALUES AT BEGINNING OF PROGRAM.
      *WHEN A CX13 OR CX14 IS REPLACED OR INSERTED, IT GETS MOVED ON TOP
      ******************************************************************
      *!WF DSP=WZ DSL=DU SEL=7E FOR=I DES=1 LEV=1 PLT=10

      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0073
          05              DE00-SUITE.                                   CI0073
            15       FILLER         PICTURE  X(00653).                  CI0073
       01                 DE10  REDEFINES      DE00.                    CI0073
            10            DE10-DU11.                                    CI0073
            11            DE10-XFONC  PICTURE  X(4).                    CI0073
            11            DE10-MPSBN  PICTURE  X(8).                    CI0073
            11            DE10-XDBDNM PICTURE  X(08).                   CI0073
            11            DE10-XSEGNM PICTURE  X(08).                   CI0073
            11            DE10-XRC    PICTURE  X(02).                   CI0073
            11            DE10-MSEG   PICTURE  X(08).                   CI0073
            11            DE10-XCOKEY PICTURE  X(70).                   CI0073
            11            DE10-CUIBR  PICTURE  X(01).                   CI0073
            11            DE10-CUIBA  PICTURE  X(01).                   CI0073
            11            DE10-IPBIK  PICTURE  X(1).                    CI0073
            10            DE10-DU03.                                    CI0073
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            DE10-CMSSF  PICTURE  XX.                      CI0073
            11            DE10-DU09.                                    CI0073
            12            DE10-CMESA  PICTURE  S9(9)                    CI0073
                          BINARY.                                       CI0073
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0073
                          BINARY.                                       CI0073
            12            DE10-CMESB  PICTURE  S9(9)                    CI0073
                          BINARY.                                       CI0073
            12            DE10-CMSST  PICTURE  S9(9)                    CI0073
                          BINARY.                                       CI0073
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0073
                          BINARY.                                       CI0073
            12            DE10-QELLAA PICTURE  S9(9)                    CI0073
                          BINARY.                                       CI0073
            12            DE10-TMESS4 PICTURE  X(512).                  CI0073
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
       01                 MS00.                                         CI0073
          05              MS00-SUITE.                                   CI0073
            15       FILLER         PICTURE  X(00542).                  CI0073
       01                 MS03  REDEFINES      MS00.                    CI0073
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            10            MS03-CMSSF  PICTURE  XX.                      CI0073
            10            MS03-DU09.                                    CI0073
            11            MS03-CMESA  PICTURE  S9(9)                    CI0073
                          BINARY.                                       CI0073
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0073
                          BINARY.                                       CI0073
            11            MS03-CMESB  PICTURE  S9(9)                    CI0073
                          BINARY.                                       CI0073
            11            MS03-CMSST  PICTURE  S9(9)                    CI0073
                          BINARY.                                       CI0073
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0073
                          BINARY.                                       CI0073
            11            MS03-QELLAA PICTURE  S9(9)                    CI0073
                          BINARY.                                       CI0073
            11            MS03-TMESS4 PICTURE  X(512).                  CI0073
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0073
            10            MX11-QMSGS  PICTURE  9(03).                   CI0073
            10            MX11-PJ09                                     CI0073
                          OCCURS       025     TIMES.                   CI0073
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0073
                          COMPUTATIONAL-3.                              CI0073
            11            MX11-CMESB  PICTURE  S9(9)                    CI0073
                          BINARY.                                       CI0073
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                WZ7D
                                WZ7E
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0073
      *               *                                   *             CI0073
      *               *INITIALISATIONS                    *             CI0073
      *               *                                   *             CI0073
      *               *************************************.            CI0073
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
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR DATP                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-DATP-PTR1.                                          ADU015
      *SET ADDRESS FOR TR1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-TR1P-PTR1.                                          ADU015
       F02DC-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0073
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0073
      *               *                                   *             CI0073
      *               *FIN DE TRAITEMENT                  *             CI0073
      *               *                                   *             CI0073
      *               *************************************.            CI0073
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0073
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *EDIT INCOMING PARAMETERS           *
      *               *                                   *
      *               *************************************.
       F35.           EXIT.                                             lv05
      *N35AB.    NOTE *MOVE PARM FIELDS TO WORKING STOR   *.
       F35AB.                                                           lv10
           MOVE        WZ7D-CACTA TO 7-CACTA
           MOVE        WZ7D-CARTZF TO 7-CARTZ.
       F35AB-FN. EXIT.
      *N35BA.    NOTE *DOES PROGRAM RECOGNIZE SOURCE      *.
       F35BA.    IF    WZ7D-MAPPN = 'SD        '                        lv10
                 OR    WZ7D-MAPPN = 'FDC       '
                 NEXT SENTENCE ELSE GO TO     F35BA-FN.
       F35BA-900. GO TO F35BB-FN.
       F35BA-FN. EXIT.
      *N35BB.    NOTE *ELSE ERROR                         *.
       F35BB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012775 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35BB-FN. EXIT.
      *N35CA.    NOTE *VALIDATE ACTION CODES              *.
       F35CA.    IF    7-CACTA-VALID                                    lv10
                 NEXT SENTENCE ELSE GO TO     F35CA-FN.
       F35CA-900. GO TO F35CB-FN.
       F35CA-FN. EXIT.
      *N35CB.    NOTE *ELSE ERROR                         *.
       F35CB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012382 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35CB-FN. EXIT.
      *N35DA.    NOTE *VERIFY CLIENT ID NUMBER            *.
       F35DA.    IF    WZ7D-CLID NUMERIC                                lv10
                 AND   WZ7D-CLID > ZERO
                 NEXT SENTENCE ELSE GO TO     F35DA-FN.
       F35DA-900. GO TO F35DB-FN.
       F35DA-FN. EXIT.
      *N35DB.    NOTE *ELSE ERROR                         *.
       F35DB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012002 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35DB-FN. EXIT.
      *N35EA.    NOTE *VERIFY ARR TYPE CODE = SD          *.
       F35EA.    IF    WZ7D-CARTY = 10                                  lv10
                 NEXT SENTENCE ELSE GO TO     F35EA-FN.
       F35EA-900. GO TO F35EB-FN.
       F35EA-FN. EXIT.
      *N35EB.    NOTE *ELSE ERROR                         *.
       F35EB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012033 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35EB-FN. EXIT.
      *N35FA.    NOTE *VERIFY ARRANGEMENT SEQUENCE NBR    *.
       F35FA.    IF    WZ7D-NARRS NUMERIC                               lv10
                 NEXT SENTENCE ELSE GO TO     F35FA-FN.
       F35FA-900. GO TO F35FB-FN.
       F35FA-FN. EXIT.
      *N35FB.    NOTE *ELSE ERROR                         *.
       F35FB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012034 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35FB-FN. EXIT.
      *N35GA.    NOTE *VERIFY CONTRACT ID NUMBER          *.
       F35GA.    IF    WZ7D-CTID01 NUMERIC                              lv10
                 AND   WZ7D-CTID01 > ZERO
                 NEXT SENTENCE ELSE GO TO     F35GA-FN.
       F35GA-900. GO TO F35GB-FN.
       F35GA-FN. EXIT.
      *N35GB.    NOTE *ELSE ERROR                         *.
       F35GB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012004 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35GB-FN. EXIT.
      *N35HA.    NOTE *'ADD' ARRANGEMENT                  *.
       F35HA.    IF    7-CACTA-ADD                                      lv10
                 NEXT SENTENCE ELSE GO TO     F35HA-FN.
      *N35HB.    NOTE *VERIFY CX13 SEGMENT PRESENT        *.
       F35HB.    IF    WZ7D-GELL OF WZ7D-CX13                           lv15
                       > ZERO
                 NEXT SENTENCE ELSE GO TO     F35HB-FN.
       F35HB-900. GO TO F35HC-FN.
       F35HB-FN. EXIT.
      *N35HC.    NOTE *ELSE ERROR                         *.
       F35HC.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012309 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35HC-FN. EXIT.
      *N35HF.    NOTE *VERIFY CX14 SEGMENT PRESENT        *.
       F35HF.    IF    WZ7D-GELL OF WZ7D-CX14                           lv15
                       > ZERO
                 NEXT SENTENCE ELSE GO TO     F35HF-FN.
       F35HF-900. GO TO F35HG-FN.
       F35HF-FN. EXIT.
      *N35HG.    NOTE *ELSE ERROR                         *.
       F35HG.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012309 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35HG-FN. EXIT.
       F35HA-FN. EXIT.
      *N35IA.    NOTE *CHANGE/DELETE ARRANGEMENT          *.
       F35IA.    IF    7-CACTA-CHANGE                                   lv10
                 OR    7-CACTA-DELETE
                 NEXT SENTENCE ELSE GO TO     F35IA-FN.
      *N35IB.    NOTE *VERIFY CX13 ARR DETAIL TYPE CODE   *.
       F35IB.    IF    7-CARTZ-VALID                                    lv15
                 NEXT SENTENCE ELSE GO TO     F35IB-FN.
       F35IB-900. GO TO F35IC-FN.
       F35IB-FN. EXIT.
      *N35IC.    NOTE *ELSE ERROR                         *.
       F35IC.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012035 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35IC-FN. EXIT.
      *N35IF.    NOTE *VERIFY CX13 ARR ACCT PYMT SEQ NB   *.
       F35IF.    IF    WZ7D-NAPDSF NUMERIC                              lv15
                 AND   WZ7D-NAPDSF > 0
                 NEXT SENTENCE ELSE GO TO     F35IF-FN.
       F35IF-900. GO TO F35IG-FN.
       F35IF-FN. EXIT.
      *N35IG.    NOTE *ELSE ERROR                         *.
       F35IG.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012036 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35IG-FN. EXIT.
      *N35IJ.    NOTE *VERIFY CX14 DESTINATION SEQ NBR    *.
       F35IJ.    IF    WZ7D-NPISQF NUMERIC                              lv15
                 NEXT SENTENCE ELSE GO TO     F35IJ-FN.
       F35IJ-900. GO TO F35IK-FN.
       F35IJ-FN. EXIT.
      *N35IK.    NOTE *ELSE ERROR                         *.
       F35IK.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012040 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35IK-FN. EXIT.
       F35IA-FN. EXIT.
      *N35JA.    NOTE *REACTIVATE/INACTIVATE ARRANGEMNT   *.
       F35JA.    IF    7-CACTA-REACTIVATE                               lv10
                 OR    7-CACTA-INACTIVATE
                 NEXT SENTENCE ELSE GO TO     F35JA-FN.
      *N35JB.    NOTE *NOT VALID FOR CERT INTEREST        *.
       F35JB.    IF    7-CARTZ-CI                                       lv15
                 NEXT SENTENCE ELSE GO TO     F35JB-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012860 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35JB-FN. EXIT.
       F35JA-FN. EXIT.
       F35-FN.   EXIT.
      *N38.      NOTE *************************************.
      *               *                                   *
      *               *MAINLINE INITIALIZATIONS           *
      *               *                                   *
      *               *************************************.
       F38.           EXIT.                                             lv05
      *N38AB.    NOTE *INITIALIZE ARRANGEMENT KEY FIELD   *.
       F38AB.                                                           lv10
           INITIALIZE  WS-CX01K
           WS-CX03K
           WS-CX06K
           WS-CX13K.
       F38AB-FN. EXIT.
      *N38BA.    NOTE *SET 'OUTPUT' TO INPUT VALUES       *.
       F38BA.                                                           lv10
           MOVE        WZ7D-CX13 TO WZ7E-CX13
           MOVE        WZ7D-CX14 TO WZ7E-CX14.
       F38BA-FN. EXIT.
      *N38CA.    NOTE *SET CI0091 PARAMETERS              *.
       F38CA.                                                           lv10
           MOVE        ZEROES TO WS02-GECSQ
           MOVE        WZ7D-CARTY TO WS02-CARTY
           MOVE        WZ7D-NARRS TO WS02-NARRS
           MOVE        'M' TO WS02-CACTM.
                 IF    7-CARTZ-DIV                                      DOT
      *FUND DIVIDEND: PROCESS CX01
           MOVE        '01' TO WS02-CPROT
                 ELSE
      *ELSE PROCESS CX03
           MOVE        '02' TO WS02-CPROT.
           MOVE        ZEROES TO WS02-DCACG.                            DOT
       F38CA-FN. EXIT.
      *N38GA.    NOTE *CALL ACF EXIT MODULE               *.            ADU031
       F38GA.                                                           lv10
           EXEC CICS   LINK PROGRAM (ACF-PROG)                          ADU031
                       COMMAREA (ACF-USER-AREA)                         ADU031
                       LENGTH (ACF-AREA-LEN)                 END-EXEC.  ADU031
       F38GA-FN. EXIT.
       F38-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *GET ORIGINAL SD INSTRUCTIONS       *
      *               *                                   *
      *               *************************************.
       F40.      IF    7-CACTA-CHANGE                                   lv05
                 OR    7-CACTA-REACTIVATE
                 OR    7-CACTA-INACTIVATE
                 OR    7-CACTA-DELETE
                 NEXT SENTENCE ELSE GO TO     F40-FN.
      *N40DD.    NOTE *READ (GU) THE CX13                 *.
       F40DD.                                                           lv10
           INITIALIZE  OR13
           INITIALIZE  OR14
           MOVE        WZ7D-CLID TO S-CXU01-CLID
           MOVE        WZ7D-CARTY TO S-CXU03-CARTY
           MOVE        WZ7D-NARRS TO S-CXU03-NARRS
           MOVE        WZ7D-CTID01 TO S-CXU06-CTID
           MOVE        WZ7D-CARTZF TO S-CXU13-CARTZ
           MOVE        WZ7D-NAPDSF TO S-CXU13-NAPDS
           PERFORM     F94XR THRU F94XR-FN.
      *N40DJ.    NOTE *CX13 FOUND                         *.
       F40DJ.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F40DJ-FN.
           MOVE        CX13 TO OR13.
      *N40DN.    NOTE *READ (GN) ALL CX14S UNDER CX13     *.
       F40DN.                       GO TO     F40DN-B.                  lv20
       F40DN-A.
                 IF    IK = '1'
                                    GO TO     F40DN-FN.
       F40DN-B.
           PERFORM     F94YN THRU F94YN-FN.
      *N40DS.    NOTE *CX14 FOUND                         *.
       F40DS.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F40DS-FN.
           ADD         1 TO 7-CX14S-READ.
                 IF    CX14-NPISQ = WZ7D-NPISQF                         DOT
      *IT'S THE CX14 PASSED TO THIS MOD
           MOVE        CX14 TO OR14.
      *N40DV.    NOTE *DELETE FOR FUNDS OR CERT OR BROK   *.
       F40DV.    IF    7-CACTA-DELETE                                   lv30
                 AND   (7-CARTZ-FUNDS OR
                       7-CARTZ-CI OR
                       7-CARTZ-BROK)
                 NEXT SENTENCE ELSE GO TO     F40DV-FN.
      *INT
      *WE KNOW AT THIS POINT THAT A
      *CX13 AND ANY/ALL CX14S UNDER IT
      *WILL BE DELETED.
      *WRITE CX14 TO AUDIT LOG NOW SO
      *THAT LATER WE CAN JUST DELETE
      *THE CX13. ALL THE CX14S UNDER IT
      *WOULD ALSO BE DELETED.
      *SAVE OFF OR14; MOVE IN CX14
           MOVE        OR14 TO SV14
           MOVE        CX14 TO OR14
      *WRITE 'BEFORE' CX14 TO AUDIT LOG
           PERFORM     F96HN THRU F96HN-FN
      *RESTORE OR14
           MOVE        SV14 TO OR14.
       F40DV-FN. EXIT.
       F40DS-FN. EXIT.
       F40DN-900. GO TO F40DN-A.
       F40DN-FN. EXIT.
       F40DJ-900. GO TO F40HD-FN.
       F40DJ-FN. EXIT.
      *N40HD.    NOTE *CX13 NOT FOUND                     *.
       F40HD.                                                           lv15
           MOVE        12009 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F40HD-FN. EXIT.
       F40DD-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *PROCESS CHANGES                    *
      *               *                                   *
      *               *************************************.
       F45.      IF    7-CACTA-CHANGE                                   lv05
                 OR    7-CACTA-REACTIVATE
                 OR    7-CACTA-INACTIVATE
                 NEXT SENTENCE ELSE GO TO     F45-FN.
      *N45EA.    NOTE *CERTS SYS PARTIAL PMT              *.
       F45EA.    IF    7-CARTZ-CP                                       lv10
                 NEXT SENTENCE ELSE GO TO     F45EA-FN.
           MOVE        'N' TO 7-CP-PROCESSED.
      *N45EC.    NOTE *A CX13 FIELD CHANGED               *.
       F45EC.    IF    OR13-DNPMT NOT = WZ7D-DNPMT                      lv15
                 OR    OR13-CPMTF NOT = WZ7D-CPMTF
                 OR    OR13-CIRMO NOT = WZ7D-CIRMO
                 OR    OR13-CDEST NOT = WZ7D-CDEST
                 OR    OR13-GEEND NOT = WZ7D-GEEND
                 OR    OR13-IIARR NOT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F45EC-FN.
      *THIS REQUIRES YOU TO MOVE THE
      *CX14 TO ANOTHER CX13 (EITHER
      *EXISTING OR NEW).
           MOVE        'Y' TO 7-CP-PROCESSED.
      *N45EE.    NOTE *1:MANY RELATIONSHIP EXISTS         *.
       F45EE.    IF    7-CX14S-READ > 1                                 lv20
                 NEXT SENTENCE ELSE GO TO     F45EE-FN.
      *N45EF.    NOTE *SAME DT NEXT PMT & STATUS AND      *.
       F45EF.    IF    OR13-DNPMT = WZ7D-DNPMT                          lv25
                 AND   OR13-CDEST = WZ7D-CDEST
                 AND   OR13-IIARR = 'Y'
                 NEXT SENTENCE ELSE GO TO     F45EF-FN.
      *COMPLETE, SO FREQ OR END DT MUST
      *BE DIFF.
      *MULT ARR ON SAME DT NOT ALLOWED.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013175 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45EF-FN. EXIT.
      *N45EG.    NOTE *DIFF DT OR STATUS OR INCOMPLETE;   *.
       F45EG.                                                           lv25
      *CONTINUE
      *UPDATE CX13
           PERFORM     F95RA THRU F95RA-FN
      *DELETE CX14
           PERFORM     F95MA THRU F95MA-FN.
       F45EG-FN. EXIT.
       F45EE-900. GO TO F45EH-FN.
       F45EE-FN. EXIT.
      *N45EH.    NOTE *1:1 RELATIONSHIP EXISTS            *.
       F45EH.                                                           lv20
      *DELETE CX14
           PERFORM     F95MA THRU F95MA-FN
      *DELETE CX13
           PERFORM     F95KA THRU F95KA-FN.
       F45EH-FN. EXIT.
      *N45ET.    NOTE *INSERT CX14 UNDER EXISTING OR      *.
       F45ET.                                                           lv20
      *NEW CX13
           PERFORM     F92 THRU F92-FN.
       F45ET-FN. EXIT.
       F45EC-FN. EXIT.
       F45EA-FN. EXIT.
      *N45JA.    NOTE *FUNDS, CERT INT, SOME CERT PART    *.
       F45JA.    IF    7-CARTZ-FUNDS                                    lv10
                 OR    7-CARTZ-CI
                 OR    (7-CARTZ-CP AND
                       7-CP-PROCESSED = 'N')
                 OR    7-CARTZ-BROK
                 OR    7-CARTZ-AP
                 NEXT SENTENCE ELSE GO TO     F45JA-FN.
      *BROK,ANNT
      *ON CERT PART, CX13 DATA DID
      *NOT CHANGE. THE CX14 CAN REMAIN
      *WHERE IT IS (DOESN'T NEED TO BE
      *MOVED TO ANOTHER CX13).
      *N45JC.    NOTE *DO GHU ON CX13                     *.
       F45JC.                                                           lv15
           MOVE        WZ7D-CLID TO S-CXU01-CLID
           MOVE        WZ7D-CARTY TO S-CXU03-CARTY
           MOVE        WZ7D-NARRS TO S-CXU03-NARRS
           MOVE        WZ7D-CTID01 TO S-CXU06-CTID
           MOVE        WZ7D-CARTZF TO S-CXU13-CARTZ
           MOVE        WZ7D-NAPDSF TO S-CXU13-NAPDS
           PERFORM     F94XS THRU F94XS-FN.
      *N45JE.    NOTE *CX13 GHU SUCCESSFUL                *.
       F45JE.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F45JE-FN.
      *WRITE 'BEFORE' CX13 TO AUDIT LOG
           MOVE        +00002 TO DH10-CAUAC
           PERFORM     F96DN THRU F96DN-FN.
      *N45JG.    NOTE *REPLACE THE CX13                   *.
       F45JG.                                                           lv25
           MOVE        WZ7D-CX13 TO CX13
           MOVE        'N' TO CX13-IIARR
           PERFORM     F94XT THRU F94XT-FN
           PERFORM     F95AA THRU F95AA-FN.
       F45JG-FN. EXIT.
      *N45JI.    NOTE *CX13 REPLACE SUCCESSFUL            *.
       F45JI.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F45JI-FN.
      *PUT CX13 SEGMENT IN I/O AREA
           MOVE        CX13 TO WZ7E-CX13
      *WRITE 'AFTER' CX13 TO AUDIT LOG
           PERFORM     F96DR THRU F96DR-FN.
       F45JI-900. GO TO F45JK-FN.
       F45JI-FN. EXIT.
      *N45JK.    NOTE *CX13 REPLACE FAILED                *.
       F45JK.                                                           lv25
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012075 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45JK-FN. EXIT.
       F45JE-900. GO TO F45JM-FN.
       F45JE-FN. EXIT.
      *N45JM.    NOTE *CX13 GHU FAILED                    *.
       F45JM.                                                           lv20
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012009 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45JM-FN. EXIT.
       F45JC-FN. EXIT.
      *N45KD.    NOTE *PROCESS THE CX14                   *.
       F45KD.                                                           lv15
      *READ (GHU) THE CX14
           MOVE        WZ7D-NPISQF TO S-CXU14-NPISQ
           PERFORM     F94YS THRU F94YS-FN.
      *N45KH.    NOTE *CX14 GHU SUCCESSFUL                *.
       F45KH.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F45KH-FN.
      *WRITE 'BEFORE' CX14 TO AUDIT LOG
           PERFORM     F96HN THRU F96HN-FN.
      *N45KK.    NOTE *REPLACE THE CX14                   *.
       F45KK.                                                           lv25
           MOVE        WZ7D-CX14 TO CX14
           PERFORM     F94YT THRU F94YT-FN
           PERFORM     F95AA THRU F95AA-FN.
       F45KK-FN. EXIT.
      *N45KN.    NOTE *CX14 REPLACE SUCCESSFUL            *.
       F45KN.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F45KN-FN.
      *PUT CX14 SEGMENT IN I/O AREA
           MOVE        CX14 TO WZ7E-CX14
      *WRITE 'AFTER' CX14 TO AUDIT LOG
           PERFORM     F96HR THRU F96HR-FN.
       F45KN-900. GO TO F45KQ-FN.
       F45KN-FN. EXIT.
      *N45KQ.    NOTE *CX14 REPLACE FAILED                *.
       F45KQ.                                                           lv25
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012076 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45KQ-FN. EXIT.
       F45KH-900. GO TO F45KT-FN.
       F45KH-FN. EXIT.
      *N45KT.    NOTE *CX14 GHU FAILED; INSERT CX14       *.
       F45KT.                                                           lv20
           PERFORM     F95GA THRU F95GA-FN.
       F45KT-FN. EXIT.
       F45KD-FN. EXIT.
       F45JA-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *PROCESS ADDS                       *
      *               *                                   *
      *               *************************************.
       F50.      IF    7-CACTA-ADD                                      lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *N50CA.    NOTE *FUNDS, CERT INTEREST,ANNT SPO      *.
       F50CA.    IF    7-CARTZ-FUNDS                                    lv10
                 OR    7-CARTZ-CI
                 OR    7-CARTZ-BROK
                 OR    7-CARTZ-AP
                 NEXT SENTENCE ELSE GO TO     F50CA-FN.
      *INSERT CX13
           PERFORM     F95BA THRU F95BA-FN
      *INSERT CX14
           PERFORM     F95GA THRU F95GA-FN
           PERFORM     F95AA THRU F95AA-FN.
       F50CA-FN. EXIT.
      *N50NA.    NOTE *CERTS SYS PARTIAL PMT              *.
       F50NA.    IF    7-CARTZ-CP                                       lv10
                 NEXT SENTENCE ELSE GO TO     F50NA-FN.
      *INSERT CX14 UNDER EXISTING OR
      *NEW CX13
           PERFORM     F92 THRU F92-FN
           PERFORM     F95AA THRU F95AA-FN.
       F50NA-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *PROCESS DELETES                    *
      *               *                                   *
      *               *************************************.
       F55.      IF    7-CACTA-DELETE                                   lv05
                 NEXT SENTENCE ELSE GO TO     F55-FN.
      *N55BA.    NOTE *FUNDS OR BROK                      *.
       F55BA.    IF    7-CARTZ-FUNDS                                    lv10
                 OR    7-CARTZ-BROK
                 NEXT SENTENCE ELSE GO TO     F55BA-FN.
      *FUNDS/BROK WILL ALWAYS HAVE 1:1.
      *CX13 TO CX14 RELATIONSHIP                                        DOT
      *DELETE CX13 & ITS CHILD CX14
           PERFORM     F95KA THRU F95KA-FN.
       F55BA-FN. EXIT.
      *N55IA.    NOTE *CERTS SYS PARTIAL PMT              *.
       F55IA.    IF    7-CARTZ-CP                                       lv10
                 NEXT SENTENCE ELSE GO TO     F55IA-FN.
      *CAN HAVE 1:1 OR 1:MANY                                           DOT
      *CX13 TO CX14 RELATIONSHIPS
      *N55IC.    NOTE *1:1 RELATIONSHIP                   *.
       F55IC.    IF    7-CX14S-READ NOT > 1                             lv15
                 NEXT SENTENCE ELSE GO TO     F55IC-FN.
      *DELETE CX14
           PERFORM     F95MA THRU F95MA-FN
      *DELETE CX13
           PERFORM     F95KA THRU F95KA-FN.
       F55IC-FN. EXIT.
      *N55IG.    NOTE *1:MANY RELATIONSHIP                *.
       F55IG.    IF    7-CX14S-READ > 1                                 lv15
                 NEXT SENTENCE ELSE GO TO     F55IG-FN.
      *UPDATE CX13
           PERFORM     F95RA THRU F95RA-FN
      *DELETE CX14
           PERFORM     F95MA THRU F95MA-FN.
       F55IG-FN. EXIT.
       F55IA-FN. EXIT.
      *N55MA.    NOTE *CERTS INTEREST                     *.
       F55MA.    IF    7-CARTZ-CI                                       lv10
                 NEXT SENTENCE ELSE GO TO     F55MA-FN.
      *DELETE CX13 & ITS CHILDREN
           PERFORM     F95KA THRU F95KA-FN.
       F55MA-FN. EXIT.
       F55-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *UPDATE CX01, CX03                  *
      *               *                                   *
      *               *************************************.
       F60.           EXIT.                                             lv05
      *N60DD.    NOTE *---> Call CI0091                   *.            AM0091
       F60DD.                                                           lv10
      *     Get Client Arrangement                                      AM0091
      *     Details...                                                  AM0091
      *                                                                 AM0091
           INITIALIZE  PJ08                                             AM0091
           DE10-DU03                                                    AM0091
      *                                                                 AM0091
           MOVE        WZ7D-MAPPN TO PJ08-MAPPN                         AM0091
           MOVE        WZ7D-CLID TO PJ08-CLID                           AM0091
           MOVE        WS02-CARTY TO PJ08-CARTY                         AM0091
           MOVE        WS02-GECSQ TO PJ08-GECSQ                         AM0091
           MOVE        WS02-NARRS TO PJ08-NARRS                         AM0091
           MOVE        WS02-CACTM TO PJ08-CACTM                         AM0091
           MOVE        WS02-DCACG TO PJ08-DCACG                         AM0091
           MOVE        WS02-CPROT TO PJ08-CPROT                         AM0091
           MOVE        WZ7D-CTID01 TO PJ08-CTID
      *                                                                 AM0091
           SET CI0091-PCB-AR1P-PTR1 TO                                  AM0091
                      PCB-AR1P-PTR1                                     AM0091
           SET CI0091-PCB-DATP-PTR1 TO                                  AM0091
                      PCB-DATP-PTR1                                     AM0091
      *                                                                 AM0091
           CALL        CI0091 USING                                     AM0091
           DFHEIBLK                                                     AM0091
           DFHCOMMAREA                                                  AM0091
           DLIUIBII                                                     AM0091
           CI0091-PCB-ADDRESS-LIST                                      AM0091
           PJ08                                                         AM0091
           CX01                                                         AM0091
           CX03                                                         AM0091
           CX06                                                         AM0091
           DE10                                                         AM0091
           MS03                                                         AM0091
           MX11.                                                        AM0091
      *N60DJ.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F60DJ.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F60DJ-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0091 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0091 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F60DJ-900. GO TO F60DK-FN.
       F60DJ-FN. EXIT.
      *N60DK.    NOTE *NO ERRORS                          *.            ADU071
       F60DK.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F60DK-FN. EXIT.
       F60DD-FN. EXIT.
       F60-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *INSERT GQ01 WITH ARRANGEMENT KEY   *
      *               *                                   *
      *               *************************************.
       F65.           EXIT.                                             lv05
      *N65AB.    NOTE *GET GQ01-GELL VALUE FROM           *.
       F65AB.                                                           lv10
      *PACTABLE TA75
           PERFORM     F95AD THRU F95AD-FN
           MOVE        ZEROS TO S-GQU01-GESQ2
           IK.
       F65AB-FN. EXIT.
      *N65DA.    NOTE *READ ORDER TICKET RECORDS          *.
       F65DA.                       GO TO     F65DA-B.                  lv10
       F65DA-A.
                 IF    IK = '1'
                 OR    WS-CF = '1'
                                    GO TO     F65DA-FN.
       F65DA-B.
           MOVE        00100 TO S-GQU01-CAMCTR
           MOVE        WZ7D-CTID01 TO S-GQU01-CANUMB
           ADD         01 TO S-GQU01-GESQ2
           PERFORM     F94R3 THRU F94R3-FN
           MOVE        GQ01-XMISL TO WS1A.
      *N65DD.    NOTE *READ GQ01 WITH ARRANGEMENT         *.
       F65DD.    IF    GQ01-GEOPID = WZ7D-GEOPD3                        lv15
                 AND   WS1A-GETIM3 = WZ7D-GETOD
                 AND   WS1A-CTRHO = WZ7D-CTRHO
                 AND   WS1A-CLIDND = ZEROES
                 NEXT SENTENCE ELSE GO TO     F65DD-FN.
      *KEYS ADDED IN XMISL
           MOVE        '1' TO WS-CF
           MOVE        WS-GELL TO GQ01-GELL
           MOVE        GQ01-XMISL TO GS1A
           MOVE        WS-CX01K TO GS1A-CX01K
           MOVE        WS-CX03K TO GS1A-CX03K
           MOVE        WS-CX06K TO GS1A-CX06K
           MOVE        WS-CX13K TO GS1A-CX13K
           MOVE        GS1A TO GQ01-XMISL
           PERFORM     F94R5 THRU F94R5-FN.
                 IF    IK = '0'                                         DOT
           PERFORM     F94R4 THRU F94R4-FN.
       F65DD-FN. EXIT.
       F65DA-900. GO TO F65DA-A.
       F65DA-FN. EXIT.
       F65-FN.   EXIT.
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
      *               *COMMON ROUTINES                    *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91TA.    NOTE *RANDOM TABLE READ FOR TC75         *.            ADUTAB
       F91TA.                                                           lv10
           MOVE        'R1' TO G-TC75-TABFO                             ADUTAB
           COMPUTE     G-TC75-LTH = 60 + G-TC75-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TC75-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TC75)                                ADUTAB
                       LENGTH (G-TC75-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TC75-TABCR NOT = '00'                          DOT
           PERFORM     F91TV THRU F91TV-FN.                             ADUTAB
       F91TA-FN. EXIT.
      *N91TV.    NOTE *TABLE READ FOR TA75 FAILED         *.
       F91TV.                                                           lv10
      *
      *********************************
      ** SEND MESSAGE AND TERMINATE   *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012207 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F91TV-FN. EXIT.
       F91-FN.   EXIT.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *ISRT CX14 UNDER OLD OR NEW CX13    *
      *               *                                   *
      *               *************************************.
       F92.                                                             lv05
      *SEARCHES FOR MATCHING CX13.
      *
      *IF FOUND:
      *- CX13 IS UPDATED
      *- CX14 IS INSERTED
      *
      *IF NOT FOUND:
      *- CX13 IS INSERTED
      *- CX14 IS INSERTED
      *
      *PERFORMED ONLY FOR CERT PARTIAL
      *CHANGES AND ADDS
      *N92BA.    NOTE *DO GU ON CX06                      *.
       F92BA.                                                           lv10
           MOVE        WZ7D-CLID TO S-CXU01-CLID
           MOVE        WZ7D-CARTY TO S-CXU03-CARTY
           MOVE        WZ7D-NARRS TO S-CXU03-NARRS
           MOVE        WZ7D-CTID01 TO S-CXU06-CTID
           PERFORM     F94XA THRU F94XA-FN.
      *N92BD.    NOTE *CX06 FOUND                         *.
       F92BD.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F92BD-FN.
           MOVE        'N' TO 7-CX13-MATCH.
      *N92BG.    NOTE *READ CX13S UNTIL MATCH FOUND       *.
       F92BG.                       GO TO     F92BG-B.                  lv20
       F92BG-A.
                 IF    IK = '1'
                 OR    7-CX13-MATCH = 'Y'
                                    GO TO     F92BG-FN.
       F92BG-B.       EXIT.
      *N92BI.    NOTE *GET NEXT CX13                      *.
       F92BI.                                                           lv25
           PERFORM     F94XB THRU F94XB-FN.
                 IF    IK = '1'                                         DOT
      *NOT FOUND
               GO TO     F92BG-900.
       F92BI-FN. EXIT.
      *N92BK.    NOTE *CX13 = INPUT ON ARR TYPE,          *.
       F92BK.    IF    CX13-CARTZ = WZ7D-CARTZ                          lv25
                 AND   CX13-DNPMT = WZ7D-DNPMT
                 AND   CX13-CDEST = WZ7D-CDEST
                 AND   CX13-IIARR = 'Y'
                 NEXT SENTENCE ELSE GO TO     F92BK-FN.
      *                DT NEXT PMT,
      *                ARR DETAIL STAT
      *& ARR IS COMPLETE
      *N92BL.    NOTE *FREQ, END DATE ALSO SAME;          *.
       F92BL.    IF    CX13-CPMTF = WZ7D-CPMTF                          lv30
                 AND   CX13-CIRMO = WZ7D-CIRMO
                 AND   CX13-GEEND = WZ7D-GEEND
                 NEXT SENTENCE ELSE GO TO     F92BL-FN.
      *IT'S A MATCH
           MOVE        'Y' TO 7-CX13-MATCH
           MOVE        CX13-CARTZ TO S-CXU13-CARTZ
           MOVE        CX13-NAPDS TO S-CXU13-NAPDS
      *DO GHU ON CX13
           PERFORM     F94XS THRU F94XS-FN.
      *N92BM.    NOTE *CX13 GHU SUCCESSFUL                *.
       F92BM.    IF    IK = '0'                                         lv35
                 NEXT SENTENCE ELSE GO TO     F92BM-FN.
      *SAVE OFF OR13; MOVE IN CX13
           MOVE        OR13 TO SV13
           MOVE        CX13 TO OR13
      *WRITE 'BEFORE' CX13 TO AUDIT LOG
           MOVE        +00002 TO DH10-CAUAC
           PERFORM     F96DN THRU F96DN-FN
      *RESTORE OR13
           MOVE        SV13 TO OR13.
      *N92BP.    NOTE *REPLACE THE CX13 SEGMENT           *.
       F92BP.                                                           lv40
      *UPDATE AMOUNTS:
      *START WITH CX13 AMTS FROM DB
      *AND ADD CX14 AMOUNTS
           COMPUTE     WZ7D-ACOT1 = CX13-ACOT1 +
           WZ7D-ACOTD
           COMPUTE     WZ7D-ADBRQ = CX13-ADBRQ +
           WZ7D-ACOTD
      *USE KEY, START DT, ARR DTL SETUP
      *DT FROM DB; SET CMPL IND TO 'N'
           MOVE        CX13-CX13K TO WZ7D-CX13K
           MOVE        CX13-GESTD TO WZ7D-GESTD
           MOVE        CX13-DASUQ TO WZ7D-DASUQ
           MOVE        'N' TO WZ7D-IIARR
           MOVE        WZ7D-CX13 TO CX13
      *REPLACE CX13
           PERFORM     F94XT THRU F94XT-FN
           PERFORM     F95AA THRU F95AA-FN.
       F92BP-FN. EXIT.
      *N92BS.    NOTE *CX13 REPLACE SUCCESSFUL            *.
       F92BS.    IF    IK = '0'                                         lv40
                 NEXT SENTENCE ELSE GO TO     F92BS-FN.
      *PUT CX13 SEGMENT IN I/O AREA
           MOVE        CX13 TO WZ7E-CX13
      *WRITE 'AFTER' CX13 TO AUDIT LOG
           PERFORM     F96DR THRU F96DR-FN
      *INSERT CX14 SEGMENT
           PERFORM     F95GA THRU F95GA-FN.
       F92BS-900. GO TO F92BU-FN.
       F92BS-FN. EXIT.
      *N92BU.    NOTE *CX13 REPLACE FAILED                *.
       F92BU.                                                           lv40
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012075 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F92BU-FN. EXIT.
       F92BM-900. GO TO F92BY-FN.
       F92BM-FN. EXIT.
      *N92BY.    NOTE *CX13 GHU FAILED                    *.
       F92BY.                                                           lv35
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012009 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F92BY-FN. EXIT.
       F92BL-900. GO TO F92BZ-FN.
       F92BL-FN. EXIT.
      *N92BZ.    NOTE *FREQ &/OR END DATE DIFFERENT       *.
       F92BZ.                                                           lv30
      *MULT ARR ON SAME DT NOT ALLOWED.                                 DOT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013175 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F92BZ-FN. EXIT.
       F92BK-FN. EXIT.
       F92BG-900. GO TO F92BG-A.
       F92BG-FN. EXIT.
      *N92DA.    NOTE *NO MATCH FOUND; INSERT CX13,CX14   *.
       F92DA.    IF    7-CX13-MATCH = 'N'                               lv20
                 NEXT SENTENCE ELSE GO TO     F92DA-FN.
      *INSERT CX13
           PERFORM     F95BA THRU F95BA-FN
      *INSERT CX14
           PERFORM     F95GA THRU F95GA-FN.
       F92DA-FN. EXIT.
       F92BD-FN. EXIT.
       F92BA-FN. EXIT.
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
      *N94R3.    NOTE *CALL GN ON GQ01                    *.            ADU026
       F94R3.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PC06 GQ01                                                    ADU026
           S-GQU01-SSA                                                  ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94R3-FN. EXIT.
      *N94R4.    NOTE *CALL REPL ON GQ01                  *.            ADU026
       F94R4.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PC06 GQ01                                                    ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94R4-FN. EXIT.
      *N94R5.    NOTE *CALL GHU ON WQ01                   *.            ADU026
       F94R5.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'WQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PC06 WQ01                                                    ADU026
           S-GQU01-SSA                                                  ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94R5-FN. EXIT.
      *N94XA.    NOTE *CALL GU ON CX06                    *.            ADU026
       F94XA.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX06                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CXU06-SSA                          ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94XA-FN. EXIT.
      *N94XB.    NOTE *CALL GN ON CX13                    *.            ADU026
       F94XB.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CX13-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94XB-FN. EXIT.
      *N94XR.    NOTE *CALL GU ON CX13                    *.            ADU026
       F94XR.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94XR-FN. EXIT.
      *N94XS.    NOTE *CALL GHU ON CX13                   *.            ADU026
       F94XS.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PA06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94XS-FN. EXIT.
      *N94XT.    NOTE *CALL REPL ON CX13                  *.            ADU026
       F94XT.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PA06 CX13                                                    ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94XT-FN. EXIT.
      *N94XU.    NOTE *CALL DLET ON CX13                  *.            ADU026
       F94XU.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XDLET                       ADU026
           PA06 CX13                                                    ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XDLET TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94XU-FN. EXIT.
      *N94XV.    NOTE *CALL ISRT ON CX13                  *.            ADU026
       F94XV.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CX13-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94XV-FN. EXIT.
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
      *N94YS.    NOTE *CALL GHU ON CX14                   *.            ADU026
       F94YS.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX14' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PA06 CX14                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           S-CXU14-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94YS-FN. EXIT.
      *N94YT.    NOTE *CALL REPL ON CX14                  *.            ADU026
       F94YT.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX14' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PA06 CX14                                                    ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94YT-FN. EXIT.
      *N94YU.    NOTE *CALL DLET ON CX14                  *.            ADU026
       F94YU.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX14' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XDLET                       ADU026
           PA06 CX14                                                    ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XDLET TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94YU-FN. EXIT.
      *N94YV.    NOTE *CALL ISRT ON CX14                  *.            ADU026
       F94YV.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX14' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 CX14                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           S-CX14-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94YV-FN. EXIT.
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *COMMON DATABASE TASKS              *
      *               *                                   *
      *               *************************************.
       F95.           EXIT.                                             lv05
      *N95AA.    NOTE *MOVE THE ARRANGEMENT KEYS          *.
       F95AA.                                                           lv10
      *
           MOVE        XW05-XCOKEY (1:23) TO WS-CX01K
           MOVE        XW05-XCOKEY (24:4) TO WS-CX03K
           MOVE        XW05-XCOKEY (28:27) TO WS-CX06K
           MOVE        XW05-XCOKEY (55:4) TO WS-CX13K.
       F95AA-FN. EXIT.
      *N95AD.    NOTE *MISC. TRANSACTION INITIALIZATION   *.
       F95AD.                                                           lv10
      *
           INITIALIZE  GQ01
           KY00
           MOVE        00100 TO TC75-CAMCTR
           PERFORM     F91TA THRU F91TA-FN
           MOVE        TC75-NDLEN TO WS-GELL.
       F95AD-FN. EXIT.
      *N95BA.    NOTE *INSERT CX13 SEGMENT                *.
       F95BA.         EXIT.                                             lv10
      *N95BD.    NOTE *PREP FOR CX13 INSERT               *.
       F95BD.                                                           lv15
      *INITIALIZE WORK FIELDS
           MOVE        'N' TO 7-CX13-INSERTED
           MOVE        ZERO TO 7-NBR-OF-INSERT-TRIES
      *SET UP SSA'S
           MOVE        WZ7D-CLID TO S-CXU01-CLID
           MOVE        WZ7D-CARTY TO S-CXU03-CARTY
           MOVE        WZ7D-NARRS TO S-CXU03-NARRS
           MOVE        WZ7D-CTID01 TO S-CXU06-CTID
      *FILL IN I/0 AREA
           MOVE        WZ7D-ACOTD TO WZ7D-ACOT1
           WZ7D-ADBRQ
           MOVE        WZ7D-CX13 TO CX13
           MOVE        'N' TO CX13-IIARR.
       F95BD-FN. EXIT.
      *N95BH.    NOTE *ATTEMPT TO INSERT CX13             *.
       F95BH.                       GO TO     F95BH-B.                  lv15
       F95BH-A.
                 IF    7-CX13-INSERTED = 'Y'
                 OR    7-NBR-OF-INSERT-TRIES >
                       7-MAX-NBR-OF-INSERT-TRIES
                                    GO TO     F95BH-FN.
       F95BH-B.
           PERFORM     F94XV THRU F94XV-FN
           PERFORM     F95AA THRU F95AA-FN.
      *N95BK.    NOTE *CX13 INSERT SUCCESSFUL             *.
       F95BK.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F95BK-FN.
           MOVE        'Y' TO 7-CX13-INSERTED
      *MOVE TO OUTPUT
           MOVE        CX13 TO WZ7E-CX13
      *WRITE 'AFTER' CX13 TO AUDIT LOG
           PERFORM     F96DR THRU F96DR-FN.
       F95BK-900. GO TO F95BN-FN.
       F95BK-FN. EXIT.
      *N95BN.    NOTE *CX13 INSERT FAILED                 *.
       F95BN.                                                           lv20
      *INCREMENT # OF TRIES, CX13 SEQ
           ADD         1 TO 7-NBR-OF-INSERT-TRIES
           ADD         1 TO CX13-NAPDS.
                 IF    7-NBR-OF-INSERT-TRIES >                          DOT
                       7-MAX-NBR-OF-INSERT-TRIES
      *MAX # OF TRIES REACHED
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012865 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F95BN-FN. EXIT.
       F95BH-900. GO TO F95BH-A.
       F95BH-FN. EXIT.
       F95BA-FN. EXIT.
      *N95GA.    NOTE *INSERT CX14 SEGMENT                *.
       F95GA.         EXIT.                                             lv10
      *N95GD.    NOTE *PREP FOR CX14 INSERT               *.
       F95GD.                                                           lv15
      *INITIALIZE WORK FIELDS
           MOVE        'N' TO 7-CX14-INSERTED
           MOVE        ZERO TO 7-NBR-OF-INSERT-TRIES
      *SET UP SSA'S
           MOVE        WZ7D-CLID TO S-CXU01-CLID
           MOVE        WZ7D-CARTY TO S-CXU03-CARTY
           MOVE        WZ7D-NARRS TO S-CXU03-NARRS
           MOVE        WZ7D-CTID01 TO S-CXU06-CTID
           MOVE        CX13-CARTZ TO S-CXU13-CARTZ
           MOVE        CX13-NAPDS TO S-CXU13-NAPDS
      *FILL IN I/0 AREA
           MOVE        WZ7D-CX14 TO CX14.
                 IF    CX14-NPISQ = ZERO                                DOT
           MOVE        +1 TO CX14-NPISQ.
       F95GD-FN. EXIT.
      *N95GH.    NOTE *ATTEMPT TO INSERT CX14             *.
       F95GH.                       GO TO     F95GH-B.                  lv15
       F95GH-A.
                 IF    7-CX14-INSERTED = 'Y'
                 OR    7-NBR-OF-INSERT-TRIES >
                       7-MAX-NBR-OF-INSERT-TRIES
                                    GO TO     F95GH-FN.
       F95GH-B.
           PERFORM     F94YV THRU F94YV-FN.
      *N95GK.    NOTE *CX14 INSERT SUCCESSFUL             *.
       F95GK.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F95GK-FN.
           MOVE        'Y' TO 7-CX14-INSERTED
      *MOVE TO OUTPUT
           MOVE        CX14 TO WZ7E-CX14
      *WRITE 'AFTER' CX14 TO AUDIT LOG
           PERFORM     F96HR THRU F96HR-FN.
       F95GK-900. GO TO F95GN-FN.
       F95GK-FN. EXIT.
      *N95GN.    NOTE *CX14 INSERT FAILED                 *.
       F95GN.                                                           lv20
      *INCREMENT # OF TRIES, CX14 SEQ
           ADD         1 TO 7-NBR-OF-INSERT-TRIES
           ADD         1 TO CX14-NPISQ.
                 IF    7-NBR-OF-INSERT-TRIES >                          DOT
                       7-MAX-NBR-OF-INSERT-TRIES
      *MAX # OF TRIES REACHED
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012866 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F95GN-FN. EXIT.
       F95GH-900. GO TO F95GH-A.
       F95GH-FN. EXIT.
       F95GA-FN. EXIT.
      *N95KA.    NOTE *DELETE CX13 SEGMENT                *.
       F95KA.                                                           lv10
      *NOTE THAT ANY CX14S UNDER THE
      *CX13 WILL ALSO BE DELETED
      *N95KD.    NOTE *DO GHU ON CX13                     *.
       F95KD.                                                           lv15
           MOVE        WZ7D-CLID TO S-CXU01-CLID
           MOVE        WZ7D-CARTY TO S-CXU03-CARTY
           MOVE        WZ7D-NARRS TO S-CXU03-NARRS
           MOVE        WZ7D-CTID01 TO S-CXU06-CTID
           MOVE        WZ7D-CARTZF TO S-CXU13-CARTZ
           MOVE        WZ7D-NAPDSF TO S-CXU13-NAPDS
           PERFORM     F94XS THRU F94XS-FN.
      *N95KG.    NOTE *CX13 GHU FAILED                    *.
       F95KG.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F95KG-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012009 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F95KG-900. GO TO F95KM-FN.
       F95KG-FN. EXIT.
      *N95KM.    NOTE *CX13 GHU SUCCESSFUL                *.
       F95KM.                                                           lv20
      *WRITE 'BEFORE' CX13 TO AUDIT LOG
           MOVE        +00004 TO DH10-CAUAC
           PERFORM     F96DN THRU F96DN-FN.
      *N95KO.    NOTE *DELETE CX13 (& ANY CHILDREN)       *.
       F95KO.                                                           lv25
           PERFORM     F94XU THRU F94XU-FN.
      *N95KP.    NOTE *CX13 DELETE FAILED                 *.
       F95KP.    IF    IK = '1'                                         lv30
                 NEXT SENTENCE ELSE GO TO     F95KP-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012868 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F95KP-FN. EXIT.
       F95KO-FN. EXIT.
       F95KM-FN. EXIT.
       F95KD-FN. EXIT.
       F95KA-FN. EXIT.
      *N95MA.    NOTE *DELETE CX14 SEGMENT                *.
       F95MA.         EXIT.                                             lv10
      *N95MD.    NOTE *DO GHU ON CX14                     *.
       F95MD.                                                           lv15
           MOVE        WZ7D-CLID TO S-CXU01-CLID
           MOVE        WZ7D-CARTY TO S-CXU03-CARTY
           MOVE        WZ7D-NARRS TO S-CXU03-NARRS
           MOVE        WZ7D-CTID01 TO S-CXU06-CTID
           MOVE        WZ7D-CARTZF TO S-CXU13-CARTZ
           MOVE        WZ7D-NAPDSF TO S-CXU13-NAPDS
           MOVE        WZ7D-NPISQF TO S-CXU14-NPISQ
           PERFORM     F94YS THRU F94YS-FN.
      *N95MM.    NOTE *CX14 GHU SUCCESSFUL                *.
       F95MM.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F95MM-FN.
      *WRITE 'BEFORE' CX14 TO AUDIT LOG
           PERFORM     F96HN THRU F96HN-FN.
      *N95MO.    NOTE *DELETE CX14                        *.
       F95MO.                                                           lv25
           PERFORM     F94YU THRU F94YU-FN.
      *N95MP.    NOTE *CX14 DELETE FAILED                 *.
       F95MP.    IF    IK = '1'                                         lv30
                 NEXT SENTENCE ELSE GO TO     F95MP-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012867 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F95MP-FN. EXIT.
       F95MO-FN. EXIT.
       F95MM-900. GO TO F95MS-FN.
       F95MM-FN. EXIT.
      *N95MS.    NOTE *CX14 GHU FAILED                    *.
       F95MS.                                                           lv20
                 IF    7-CARTZ-CP                                       DOT
                 AND   7-CX14S-READ > 1
      *ONLY ABEND WHEN IT'S A CERT
      *PARTIAL & > 1 CX14
      *(CX13 AMT WAS ALREADY UPDATED,
      *ASSUMING CX14 WOULD BE DELETED)
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012759 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F95MS-FN. EXIT.
       F95MD-FN. EXIT.
       F95MA-FN. EXIT.
      *N95RA.    NOTE *UPDATE CX13 WHEN CX14 DELETED      *.
       F95RA.         EXIT.                                             lv10
      *N95RJ.    NOTE *DO GHU ON CX13                     *.
       F95RJ.                                                           lv20
           MOVE        WZ7D-CLID TO S-CXU01-CLID
           MOVE        WZ7D-CARTY TO S-CXU03-CARTY
           MOVE        WZ7D-NARRS TO S-CXU03-NARRS
           MOVE        WZ7D-CTID01 TO S-CXU06-CTID
           MOVE        WZ7D-CARTZF TO S-CXU13-CARTZ
           MOVE        WZ7D-NAPDSF TO S-CXU13-NAPDS
           PERFORM     F94XS THRU F94XS-FN.
      *N95RL.    NOTE *CX13 GHU FAILED                    *.
       F95RL.    IF    IK = '1'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F95RL-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012009 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F95RL-900. GO TO F95RM-FN.
       F95RL-FN. EXIT.
      *N95RM.    NOTE *CX13 GHU SUCCESSFUL                *.
       F95RM.                                                           lv25
      *WRITE 'BEFORE' CX13 TO AUDIT LOG
           MOVE        +00002 TO DH10-CAUAC
           PERFORM     F96DN THRU F96DN-FN.
      *N95RN.    NOTE *REPLACE CX13                       *.
       F95RN.                                                           lv30
      *SUBTRACT CX14 $ FROM CX13
      *(CP HAS ONLY $, NOT % OR SHRS)
           COMPUTE     CX13-ACOT1 =
           CX13-ACOT1 - OR14-ACOTD
           COMPUTE     CX13-ADBRQ =
           CX13-ADBRQ - OR14-ACOTD.
                 IF    CX13-IIARR = 'N'                                 DOT
      *IF ARR IS ALREADY INCOMPLETE,
      *CHANGE STATUS TO INACTIVE
           MOVE        3 TO CX13-CDEST.
      *SET ARR COMPLETE INDICATOR OFF                                   DOT
           MOVE        'N' TO CX13-IIARR
      *REPLACE CX13
           PERFORM     F94XT THRU F94XT-FN
           PERFORM     F95AA THRU F95AA-FN.
      *N95RS.    NOTE *CX13 REPLACE SUCCESSFUL            *.
       F95RS.    IF    IK = '0'                                         lv35
                 NEXT SENTENCE ELSE GO TO     F95RS-FN.
      *PUT CX13 SEGMENT IN I/O AREA
           MOVE        CX13 TO WZ7E-CX13
      *WRITE 'AFTER' CX13 TO AUDIT LOG
           PERFORM     F96DR THRU F96DR-FN.
       F95RS-900. GO TO F95RV-FN.
       F95RS-FN. EXIT.
      *N95RV.    NOTE *CX13 REPLACE FAILED                *.
       F95RV.                                                           lv35
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012075 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F95RV-FN. EXIT.
       F95RN-FN. EXIT.
       F95RM-FN. EXIT.
       F95RJ-FN. EXIT.
       F95RA-FN. EXIT.
       F95-FN.   EXIT.
      *N96AL.    NOTE *---> Audit Log Process             *.            ADU165
       F96AL.         EXIT.                                             lv10
      *N96AN.    NOTE *---> Format Audit Log Data         *.            ADU165
       F96AN.                                                           lv15
           SET AL00-NPNTR                                               ADU165
           TO ADDRESS OF DLIUIBII                                       ADU165
           MOVE        AL00-ADDR TO DH10-XUIBP                          ADU165
           MOVE        AL00-NSEQ2P TO DH10-NSEQ2P                       ADU165
           MOVE        'E' TO DH10-CAUL                                 ADU165
           MOVE        'CLIENT1' TO DH10-MAUSB                          ADU165
           MOVE        WZ7D-CLID TO DH10-NAUSK                          ADU165
           MOVE        'CATS' TO DH10-CSYS                              ADU165
           MOVE        EIBTRNID TO DH10-CAPPL                           ADU165
           MOVE        'C' TO DH10-CAUSR                                ADU165
           MOVE        ACF-USER-ID TO DH10-GEOPID                       ADU165
           MOVE        ACF-USER-UNIT TO DH10-CAUNIT                     ADU165
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
      *N96DD.    NOTE *AUDIT LOGGING FUNCTIONS            *.
       F96DD.                                                           lv10
      *I PERFORM ALL THE AUDIT LOGGING
      *FUNCTIONS FOR THIS MODULE.  ALL
      *CALLS TO THE AUDIT LOGGER SHOULD
      *GO THROUGH ME.
      *
      *I ASSUME THAT ALL 'AFTER' IMAGES
      *OF SEGMENTS ARE IN WZ7D.  ALL
      *'BEFORE' IMAGES ARE IN OR13 OR
      *OR14.
      *
      *N96DN.    NOTE *LOG CX13 'BEFORE' ON CHG OR DEL    *.
       F96DN.                                                           lv15
           MOVE        +70022 TO DH10-CAUFR
      *(DH10-CAUAC GETS SET WHEREVER
      *F96DN IS PERFORMED FROM)
      *FILL IN VA13-K11A (CX03K,CX06K)
           MOVE        WZ7D-CARTY TO VA13-CARTY
           MOVE        WZ7D-NARRS TO VA13-NARRS
           MOVE        WZ7D-CTID01 TO VA13-CX06K
      *FILL IN CX13 DATA
           MOVE        OR13-CY20 TO VA13-CY20
           MOVE        OR13-CY96 TO VA13-CY96
           MOVE        OR13-CY98 TO VA13-CY98
           MOVE        VA13 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F96DN-FN. EXIT.
      *N96DR.    NOTE *LOG CX13 'AFTER' AND 'INSERT'      *.
       F96DR.                                                           lv15
           MOVE        +70022 TO DH10-CAUFR.
                 IF    OR13-GELL = ZERO                                 DOT
      *INSERT SITUATION
           MOVE        +00001 TO DH10-CAUAC
                 ELSE
      *'AFTER' ON CHANGE SITUATION
           MOVE        +00003 TO DH10-CAUAC.
      *FILL IN VA13-K11A (CX03K,CX06K)                                  DOT
           MOVE        WZ7D-CARTY TO VA13-CARTY
           MOVE        WZ7D-NARRS TO VA13-NARRS
           MOVE        WZ7D-CTID01 TO VA13-CX06K
      *FILL IN CX13 DATA
           MOVE        WZ7E-CY20 TO VA13-CY20
           MOVE        'N' TO VA13-IIARR
           MOVE        WZ7E-CY96 TO VA13-CY96
           MOVE        WZ7E-CY98 TO VA13-CY98
           MOVE        VA13 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F96DR-FN. EXIT.
      *N96HN.    NOTE *LOG CX14 'BEFORE' ON CHG OR DEL    *.
       F96HN.                                                           lv15
           MOVE        +70023 TO DH10-CAUFR
      *ALWAYS LOG AS 'DELETE'
           MOVE        +00004 TO DH10-CAUAC
      *FILL IN VA14-K11A (CX03K,CX06K)
           MOVE        WZ7D-CARTY TO VA14-CARTY
           MOVE        WZ7D-NARRS TO VA14-NARRS
           MOVE        WZ7D-CTID01 TO VA14-CX06K
      *FILL IN CX13K
           MOVE        WZ7D-CX13K TO VA14-CX13K
      *FILL IN CX14 DATA
           MOVE        OR14-NPISQ TO VA14-NPISQ
           MOVE        OR14-ACOTD TO VA14-ACOTD
           MOVE        OR14-PPOTD TO VA14-PPOTD
           MOVE        OR14-QPSTD TO VA14-QPSTD
           MOVE        OR14-CPITC TO VA14-CPITC
           MOVE        OR14-CY30 TO VA14-CY30
           MOVE        VA14 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F96HN-FN. EXIT.
      *N96HR.    NOTE *LOG CX14 'AFTER' AND 'INSERT'      *.
       F96HR.                                                           lv15
           MOVE        +70023 TO DH10-CAUFR
      *ALWAYS LOG AS 'INSERT'
           MOVE        +00001 TO DH10-CAUAC
      *FILL IN VA14-K11A (CX03K,CX06K)
           MOVE        WZ7D-CARTY TO VA14-CARTY
           MOVE        WZ7D-NARRS TO VA14-NARRS
           MOVE        WZ7D-CTID01 TO VA14-CX06K
      *FILL IN CX13K
           MOVE        WZ7E-CX13K TO VA14-CX13K
      *FILL IN CX14 DATA
           MOVE        WZ7E-NPISQ TO VA14-NPISQ
           MOVE        WZ7E-ACOTD TO VA14-ACOTD
           MOVE        WZ7E-PPOTD TO VA14-PPOTD
           MOVE        WZ7E-QPSTD TO VA14-QPSTD
           MOVE        WZ7E-CPITC TO VA14-CPITC
           MOVE        WZ7E-CY30 TO VA14-CY30
           MOVE        VA14 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F96HR-FN. EXIT.
       F96DD-FN. EXIT.
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
