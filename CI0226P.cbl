       IDENTIFICATION DIVISION.                                         CI0226
       PROGRAM-ID.  CI0226P.                                            CI0226
      *AUTHOR.         VALIDATE ACH IN AMOUNT.                          CI0226
      *DATE-COMPILED.   09/08/14.                                       CI0226
       ENVIRONMENT DIVISION.                                            CI0226
       CONFIGURATION SECTION.                                           CI0226
       SOURCE-COMPUTER. IBM-370.                                        CI0226
       OBJECT-COMPUTER. IBM-370.                                        CI0226
       DATA DIVISION.                                                   CI0226
       WORKING-STORAGE SECTION.                                         CI0226
      *-----------------------------------------------------------------
      *>>>>>> USED TO GET ARRANGEMENT DETAILS
      *-----------------------------------------------------------------
      *
      *!WF DSP=AR DSL=DU SEL=61 FOR=I LEV=1 PLT=AR
       01                 AR00.                                         CI0226
          05              AR00-SUITE.                                   CI0226
            15       FILLER         PICTURE  X(29071).                  CI0226
       01                 AR61  REDEFINES      AR00.                    CI0226
            10            AR61-CLID   PICTURE  X(23).                   CI0226
            10            AR61-CARTY  PICTURE  99.                      CI0226
            10            AR61-NARRS  PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            10            AR61-MIPPS  PICTURE  X(4).                    CI0226
            10            AR61-CTID3  PICTURE  X(27).                   CI0226
            10            AR61-CPMTCX PICTURE  XX.                      CI0226
            10            AR61-NAPDSK PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            10            AR61-GESTD1 PICTURE  9(8).                    CI0226
            10            AR61-FILLER PICTURE  X(100).                  CI0226
            10            AR61-IENDP  PICTURE  X.                       CI0226
            10            AR61-DU9E                                     CI0226
                          OCCURS       100     TIMES.                   CI0226
            11            AR61-CX06.                                    CI0226
            12            AR61-CX06K.                                   CI0226
            13            AR61-C299.                                    CI0226
            14            AR61-CTID.                                    CI0226
            15            AR61-CTIDA  PICTURE  9(3).                    CI0226
            15            AR61-CTIDN.                                   CI0226
            16            AR61-CTIDNP PICTURE  X(13).                   CI0226
            16            AR61-CTIDND PICTURE  9(11).                   CI0226
            12            AR61-NPECK  PICTURE  9(02).                   CI0226
            12            AR61-FILLER PICTURE  X.                       CI0226
            11            AR61-CX12.                                    CI0226
            12            AR61-CX12K.                                   CI0226
            13            AR61-CPMTC  PICTURE  99.                      CI0226
            13            AR61-NAPDS  PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            13            AR61-GESTD  PICTURE  9(8).                    CI0226
            12            AR61-GEEND  PICTURE  9(8).                    CI0226
            12            AR61-CIRMO  PICTURE  X(12).                   CI0226
            12            AR61-CDEST  PICTURE  99.                      CI0226
            12            AR61-APMTL  PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            12            AR61-DNPMT  PICTURE  9(8).                    CI0226
            12            AR61-NIRACM PICTURE  9(2).                    CI0226
            12            AR61-CPMTF  PICTURE  99.                      CI0226
            12            AR61-IPODM  PICTURE  X.                       CI0226
            12            AR61-CLUPD  PICTURE  9(3).                    CI0226
            12            AR61-DLAUP  PICTURE  9(8).                    CI0226
            12            AR61-CWRC   PICTURE  99.                      CI0226
            12            AR61-CHCR   PICTURE  99.                      CI0226
            12            AR61-GEOPD2 PICTURE  X(8).                    CI0226
            12            AR61-GEAUN  PICTURE  9(5).                    CI0226
            12            AR61-DPCHD  PICTURE  9(8).                    CI0226
            12            AR61-DNEXE  PICTURE  9(8).                    CI0226
            12            AR61-CCSMQ  PICTURE  X.                       CI0226
            12            AR61-GCUSPZ PICTURE  X(12).                   CI0226
            12            AR61-CORTY  PICTURE  X.                       CI0226
            12            AR61-CNAVR  PICTURE  X(1).                    CI0226
            12            AR61-DELOI3 PICTURE  9(6).                    CI0226
            12            AR61-ALOIDD PICTURE  9(9)V99                  CI0226
                          COMPUTATIONAL-3.                              CI0226
            12            AR61-FILLER PICTURE  X(5).                    CI0226
            11            AR61-MSP03A PICTURE  X(1).                    CI0226
            11            AR61-FILLER PICTURE  X(129).                  CI0226
                                                                        AM0060
      ******************************************************************AM0060
      **     FIELDS USED IN THE PARAMETER LIST OF CI0060.  THESE WILL  *AM0060
      **     BE VALUED AND PASSED IN THE CALLING PROGRAM.              *AM0060
      ******************************************************************AM0060
                                                                        AM0060
      *!WI pl=BA100                                                     AM0060
       01  7-WORK-CFUNC                                                 AM0060
                        PICTURE X(3).                                   CI0226
      *!WI pl=BA120                                                     AM0060
       01  7-WORK-NARRSK                                                AM0060
                        PICTURE S9(3)                                   CI0226
                          COMPUTATIONAL-3.                              CI0226
                                                                        AM0060
                                                                        AM0060
      *!WI
       01  7-WORK-CARTY
                        PICTURE 99.                                     CI0226
      *-----------------------------------------------------------------
      *>>>>>> USED TO GET CLIENT BANKS
      *-----------------------------------------------------------------
      *
      *!WF DSP=CB DSL=DU SEL=60 FOR=I LEV=1 PLT=CB
       01                 CB00.                                         CI0226
          05              CB00-SUITE.                                   CI0226
            15       FILLER         PICTURE  X(05884).                  CI0226
       01                 CB60  REDEFINES      CB00.                    CI0226
            10            CB60-CLID01 PICTURE  X(23).                   CI0226
            10            CB60-CARTYK PICTURE  99.                      CI0226
            10            CB60-CFUNC  PICTURE  X(3).                    CI0226
            10            CB60-NARRSK PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            10            CB60-GECTR  PICTURE  99.                      CI0226
            10            CB60-NARRSB PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            10            CB60-NSEQ4B PICTURE  9(8)                     CI0226
                          BINARY.                                       CI0226
            10            CB60-FILLER PICTURE  X(46).                   CI0226
            10            CB60-DU9D                                     CI0226
                          OCCURS       020     TIMES.                   CI0226
            11            CB60-CX03.                                    CI0226
            12            CB60-GELL   PICTURE  9(4)                     CI0226
                          BINARY.                                       CI0226
            12            CB60-CY00.                                    CI0226
            13            CB60-CX03K.                                   CI0226
            14            CB60-CARTY  PICTURE  99.                      CI0226
            14            CB60-NARRS  PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            13            CB60-CARST  PICTURE  99.                      CI0226
            13            CB60-GECSQ  PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            13            CB60-CPMTG  PICTURE  99.                      CI0226
            13            CB60-GRCRNG PICTURE  9(3).                    CI0226
            13            CB60-DEXDT  PICTURE  9(8).                    CI0226
            13            CB60-DASUP  PICTURE  9(8).                    CI0226
            13            CB60-CSTEC  PICTURE  X(3).                    CI0226
            13            CB60-FILLER PICTURE  X(17).                   CI0226
            13            CB60-CY50.                                    CI0226
            14            CB60-NARID  PICTURE  X(30).                   CI0226
            13            CB60-CY51                                     CI0226
                          REDEFINES            CB60-CY50.               CI0226
            14            CB60-NDIDN  PICTURE  9(12).                   CI0226
            14            CB60-FILLER PICTURE  X(18).                   CI0226
            13            CB60-CY52                                     CI0226
                          REDEFINES            CB60-CY50.               CI0226
            14            CB60-NAIDC  PICTURE  9(12).                   CI0226
            14            CB60-FILLER PICTURE  X(18).                   CI0226
            13            CB60-CY53                                     CI0226
                          REDEFINES            CB60-CY50.               CI0226
            14            CB60-NAMEXB PICTURE  9(15).                   CI0226
            14            CB60-FILLER PICTURE  X(15).                   CI0226
            12            CB60-CY99.                                    CI0226
            13            CB60-FILLER PICTURE  X(109).                  CI0226
            12            CB60-CY01                                     CI0226
                          REDEFINES            CB60-CY99.               CI0226
            13            CB60-NBASQ  PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            13            CB60-ICPCI  PICTURE  X.                       CI0226
            13            CB60-CLUPD  PICTURE  9(3).                    CI0226
            13            CB60-DLAUP  PICTURE  9(8).                    CI0226
            13            CB60-CWRC   PICTURE  99.                      CI0226
            13            CB60-CHCR   PICTURE  99.                      CI0226
            13            CB60-GEOPD2 PICTURE  X(8).                    CI0226
            13            CB60-GEAUN  PICTURE  9(5).                    CI0226
            13            CB60-DPCHD  PICTURE  9(8).                    CI0226
            13            CB60-DLRCHK PICTURE  9(8).                    CI0226
            13            CB60-QTRCHK PICTURE  9(2).                    CI0226
            13            CB60-DNPMT  PICTURE  9(8).                    CI0226
            13            CB60-APMTLA PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            12            CB60-CY02                                     CI0226
                          REDEFINES            CB60-CY99.               CI0226
            13            CB60-QSIRQ  PICTURE  99.                      CI0226
            13            CB60-QDRMN  PICTURE  9(2)                     CI0226
                          COMPUTATIONAL-3.                              CI0226
            13            CB60-DDPRE  PICTURE  9(8).                    CI0226
            13            CB60-DDSHP  PICTURE  9(8).                    CI0226
            13            CB60-NDRFTB PICTURE  9(5).                    CI0226
            13            CB60-QDIPBJ PICTURE  9(3).                    CI0226
            13            CB60-DDSHPA PICTURE  9(8).                    CI0226
            13            CB60-NDRFTF PICTURE  9(5).                    CI0226
            13            CB60-QDIPBK PICTURE  9(3).                    CI0226
            13            CB60-CREOR  PICTURE  X(1).                    CI0226
            13            CB60-CREOR1 PICTURE  X(1).                    CI0226
            13            CB60-DDASC  PICTURE  9(8).                    CI0226
            13            CB60-FILLER PICTURE  X(7).                    CI0226
            12            CB60-CY03                                     CI0226
                          REDEFINES            CB60-CY99.               CI0226
            13            CB60-DLAUP1 PICTURE  9(8).                    CI0226
            13            CB60-GEOPD3 PICTURE  X(8).                    CI0226
            13            CB60-DNPMT1 PICTURE  9(8).                    CI0226
            13            CB60-DOPDA  PICTURE  99.                      CI0226
            13            CB60-CPMTF  PICTURE  99.                      CI0226
            13            CB60-CIRMO  PICTURE  X(12).                   CI0226
            13            CB60-CPALL  PICTURE  X(1).                    CI0226
            13            CB60-CCOLM  PICTURE  9(2).                    CI0226
            13            CB60-CBLTP  PICTURE  X(1).                    CI0226
            13            CB60-CASUB  PICTURE  9(2).                    CI0226
            13            CB60-CBLFM  PICTURE  9(2).                    CI0226
            13            CB60-IBILS  PICTURE  X.                       CI0226
            13            CB60-IPAOS  PICTURE  X.                       CI0226
            13            CB60-CBLSQ  PICTURE  X(4).                    CI0226
            13            CB60-DLBPD  PICTURE  9(8).                    CI0226
            13            CB60-DNBPD  PICTURE  9(8).                    CI0226
            13            CB60-DODBD  PICTURE  9(8).                    CI0226
            13            CB60-CPSRE  PICTURE  99.                      CI0226
            13            CB60-ISPHN  PICTURE  X.                       CI0226
            13            CB60-TCARR  PICTURE  X(6).                    CI0226
            13            CB60-CBKPT  PICTURE  9(2).                    CI0226
            13            CB60-IECNT  PICTURE  X.                       CI0226
            13            CB60-ICONV  PICTURE  X(1).                    CI0226
            13            CB60-FILLER PICTURE  X(4).                    CI0226
            12            CB60-CY04                                     CI0226
                          REDEFINES            CB60-CY99.               CI0226
            13            CB60-CCARD  PICTURE  X(02).                   CI0226
            13            CB60-MCSIG4 PICTURE  X(20).                   CI0226
            13            CB60-IREMT  PICTURE  X(01).                   CI0226
            13            CB60-ISBILA PICTURE  X.                       CI0226
            13            CB60-DLBPDA PICTURE  9(8).                    CI0226
            13            CB60-DNBPDA.                                  CI0226
            14            CB60-DNCYM  PICTURE  9(6).                    CI0226
            14            CB60-CEDTD  PICTURE  9(2).                    CI0226
            13            CB60-AREMT  PICTURE  S9(7)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            13            CB60-DREMT  PICTURE  9(8).                    CI0226
            13            CB60-ADBRQ  PICTURE  S9(11)V99                CI0226
                          COMPUTATIONAL-3.                              CI0226
            13            CB60-CLUPD1 PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            13            CB60-DLAUP3 PICTURE  9(8).                    CI0226
            13            CB60-CWRC2  PICTURE  99.                      CI0226
            13            CB60-CHCR2  PICTURE  99.                      CI0226
            13            CB60-GEOPD9 PICTURE  X(8).                    CI0226
            13            CB60-GEAUN1 PICTURE  9(5).                    CI0226
            13            CB60-DPCHD1 PICTURE  9(8).                    CI0226
            11            CB60-CX18.                                    CI0226
            12            CB60-CX18K.                                   CI0226
            13            CB60-NBASQ  PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            12            CB60-NPBN   PICTURE  X(20).                   CI0226
            12            CB60-CCBAT  PICTURE  99.                      CI0226
            12            CB60-DACHP  PICTURE  9(8).                    CI0226
            12            CB60-CSTPRE PICTURE  99.                      CI0226
            12            CB60-C199.                                    CI0226
            13            CB60-CLID.                                    CI0226
            14            CB60-CLIDO  PICTURE  9(3).                    CI0226
            14            CB60-CLIDN.                                   CI0226
            15            CB60-CLIDNP PICTURE  X(12).                   CI0226
            15            CB60-CLIDND PICTURE  9(8).                    CI0226
            12            CB60-MCSIG  PICTURE  X(30).                   CI0226
            12            CB60-CPBNU  PICTURE  X.                       CI0226
            12            CB60-CSPCR  PICTURE  99.                      CI0226
            12            CB60-DAPCR  PICTURE  9(8).                    CI0226
            12            CB60-FILLER PICTURE  XX.                      CI0226
      *=================================================================
      *= FLAGS USED IN PROCESSING DATE OF NEXT PAYMENT                 =
      *=================================================================
      *
      *!WI
       01 WS-MATCH-IAIND
                        PICTURE X.                                      CI0226
          88 NO-MATCH    VALUE 'N'.
          88 YES-MATCH   VALUE 'Y'.
      *
      *!WI
       01 WS-DONE-IAIND
                        PICTURE X.                                      CI0226
          88 NOT-DONE    VALUE 'N'.
          88 YES-DONE    VALUE 'Y'.
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0060           PIC X(8) VALUE 'CI0060P '.                  AM0060
       01  CI0061           PIC X(8) VALUE 'CI0061P '.                  AM0061
      *****************************************************************
      ** HOST VARIABLES FOR DB2 TABLES                                *
      *****************************************************************
      *!WF DSP=CT DSL=CT SEL=1S FOR=I DES=2 LEV=1 PLT=CT
       01                 CT1S.                                         CI0226
            10            CT1S-DXTMS  PICTURE  X(26)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1S-NUCLI  PICTURE  X(32)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1S-CARTYA PICTURE  XX                       CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1S-APMTO  PICTURE  S9(11)V99                CI0226
                          VALUE                ZERO                     CI0226
                          COMPUTATIONAL-3.                              CI0226
            10            CT1S-CASTA  PICTURE  X                        CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1S-CPMTFA PICTURE  X(2)                     CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1S-DSETU  PICTURE  X(10)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1S-DATSD  PICTURE  X(10)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1S-DTEND  PICTURE  X(10)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1S-DACPR  PICTURE  X(10)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1S-GEOPDF PICTURE  X(16)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1S-DXTMST PICTURE  X(26)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1S-GEOPDG PICTURE  X(16)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1S-DXTMSD PICTURE  X(26)                    CI0226
                          VALUE                SPACE.                   CI0226
       01                 CT1T.                                         CI0226
            10            CT1T-DXTMS  PICTURE  X(26)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1T-NAGTB  PICTURE  X(8)                     CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1T-CTBAC  PICTURE  X(03)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1T-NPBN   PICTURE  X(20)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1T-IAPRV  PICTURE  X                        CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1T-CPROCM PICTURE  X                        CI0226
                          VALUE                SPACE.                   CI0226
       01                 CT1U.                                         CI0226
            10            CT1U-DXTMS  PICTURE  X(26)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1U-CNTXC  PICTURE  X(10)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1U-NNANI  PICTURE  X(40)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1U-CACCT  PICTURE  X                        CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1U-CPMTCX PICTURE  XX                       CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1U-APMTD  PICTURE  S9(11)V99                CI0226
                          VALUE                ZERO                     CI0226
                          COMPUTATIONAL-3.                              CI0226
            10            CT1U-DDATYA PICTURE  XXXX                     CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1U-DDATM  PICTURE  XX                       CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1U-GEOPDF PICTURE  X(16)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1U-DXTMST PICTURE  X(26)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1U-GEOPDG PICTURE  X(16)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            CT1U-DXTMSD PICTURE  X(26)                    CI0226
                          VALUE                SPACE.                   CI0226
      *!WF DSP=CT DSL=CT SEL=1T FOR=I DES=2 LEV=1 PLT=CT
      *!WF DSP=CT DSL=CT SEL=1U FOR=I DES=2 LEV=1 PLT=CT
       01                 CX01.                                         CI0226
            10            CX01-CX01K.                                   CI0226
            11            CX01-C199.                                    CI0226
            12            CX01-CLID.                                    CI0226
            13            CX01-CLIDO  PICTURE  9(3).                    CI0226
            13            CX01-CLIDN.                                   CI0226
            14            CX01-CLIDNP PICTURE  X(12).                   CI0226
            14            CX01-CLIDND PICTURE  9(8).                    CI0226
            10            CX01-GEMDA  PICTURE  9(8).                    CI0226
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0226
                          BINARY.                                       CI0226
            10            CX01-FILLER PICTURE  X(5).                    CI0226
       01                 CX03.                                         CI0226
            10            CX03-GELL   PICTURE  9(4)                     CI0226
                          BINARY.                                       CI0226
            10            CX03-CY00.                                    CI0226
            11            CX03-CX03K.                                   CI0226
            12            CX03-CARTY  PICTURE  99.                      CI0226
            12            CX03-NARRS  PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            CX03-CARST  PICTURE  99.                      CI0226
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            CX03-CPMTG  PICTURE  99.                      CI0226
            11            CX03-GRCRNG PICTURE  9(3).                    CI0226
            11            CX03-DEXDT  PICTURE  9(8).                    CI0226
            11            CX03-DASUP  PICTURE  9(8).                    CI0226
            11            CX03-CSTEC  PICTURE  X(3).                    CI0226
            11            CX03-FILLER PICTURE  X(17).                   CI0226
            11            CX03-CY50.                                    CI0226
            12            CX03-NARID  PICTURE  X(30).                   CI0226
            11            CX03-CY51                                     CI0226
                          REDEFINES            CX03-CY50.               CI0226
            12            CX03-NDIDN  PICTURE  9(12).                   CI0226
            12            CX03-FILLER PICTURE  X(18).                   CI0226
            11            CX03-CY52                                     CI0226
                          REDEFINES            CX03-CY50.               CI0226
            12            CX03-NAIDC  PICTURE  9(12).                   CI0226
            12            CX03-FILLER PICTURE  X(18).                   CI0226
            11            CX03-CY53                                     CI0226
                          REDEFINES            CX03-CY50.               CI0226
            12            CX03-NAMEXB PICTURE  9(15).                   CI0226
            12            CX03-FILLER PICTURE  X(15).                   CI0226
            10            CX03-CY99.                                    CI0226
            11            CX03-FILLER PICTURE  X(109).                  CI0226
            10            CX03-CY01                                     CI0226
                          REDEFINES            CX03-CY99.               CI0226
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            CX03-ICPCI  PICTURE  X.                       CI0226
            11            CX03-CLUPD  PICTURE  9(3).                    CI0226
            11            CX03-DLAUP  PICTURE  9(8).                    CI0226
            11            CX03-CWRC   PICTURE  99.                      CI0226
            11            CX03-CHCR   PICTURE  99.                      CI0226
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0226
            11            CX03-GEAUN  PICTURE  9(5).                    CI0226
            11            CX03-DPCHD  PICTURE  9(8).                    CI0226
            11            CX03-DLRCHK PICTURE  9(8).                    CI0226
            11            CX03-QTRCHK PICTURE  9(2).                    CI0226
            11            CX03-DNPMT  PICTURE  9(8).                    CI0226
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            10            CX03-CY02                                     CI0226
                          REDEFINES            CX03-CY99.               CI0226
            11            CX03-QSIRQ  PICTURE  99.                      CI0226
            11            CX03-QDRMN  PICTURE  9(2)                     CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            CX03-DDPRE  PICTURE  9(8).                    CI0226
            11            CX03-DDSHP  PICTURE  9(8).                    CI0226
            11            CX03-NDRFTB PICTURE  9(5).                    CI0226
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0226
            11            CX03-DDSHPA PICTURE  9(8).                    CI0226
            11            CX03-NDRFTF PICTURE  9(5).                    CI0226
            11            CX03-QDIPBK PICTURE  9(3).                    CI0226
            11            CX03-CREOR  PICTURE  X(1).                    CI0226
            11            CX03-CREOR1 PICTURE  X(1).                    CI0226
            11            CX03-DDASC  PICTURE  9(8).                    CI0226
            11            CX03-FILLER PICTURE  X(7).                    CI0226
            10            CX03-CY03                                     CI0226
                          REDEFINES            CX03-CY99.               CI0226
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0226
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0226
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0226
            11            CX03-DOPDA  PICTURE  99.                      CI0226
            11            CX03-CPMTF  PICTURE  99.                      CI0226
            11            CX03-CIRMO  PICTURE  X(12).                   CI0226
            11            CX03-CPALL  PICTURE  X(1).                    CI0226
            11            CX03-CCOLM  PICTURE  9(2).                    CI0226
            11            CX03-CBLTP  PICTURE  X(1).                    CI0226
            11            CX03-CASUB  PICTURE  9(2).                    CI0226
            11            CX03-CBLFM  PICTURE  9(2).                    CI0226
            11            CX03-IBILS  PICTURE  X.                       CI0226
            11            CX03-IPAOS  PICTURE  X.                       CI0226
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0226
            11            CX03-DLBPD  PICTURE  9(8).                    CI0226
            11            CX03-DNBPD  PICTURE  9(8).                    CI0226
            11            CX03-DODBD  PICTURE  9(8).                    CI0226
            11            CX03-CPSRE  PICTURE  99.                      CI0226
            11            CX03-ISPHN  PICTURE  X.                       CI0226
            11            CX03-TCARR  PICTURE  X(6).                    CI0226
            11            CX03-CBKPT  PICTURE  9(2).                    CI0226
            11            CX03-IECNT  PICTURE  X.                       CI0226
            11            CX03-ICONV  PICTURE  X(1).                    CI0226
            11            CX03-FILLER PICTURE  X(4).                    CI0226
            10            CX03-CY04                                     CI0226
                          REDEFINES            CX03-CY99.               CI0226
            11            CX03-CCARD  PICTURE  X(02).                   CI0226
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0226
            11            CX03-IREMT  PICTURE  X(01).                   CI0226
            11            CX03-ISBILA PICTURE  X.                       CI0226
            11            CX03-DLBPDA PICTURE  9(8).                    CI0226
            11            CX03-DNBPDA.                                  CI0226
            12            CX03-DNCYM  PICTURE  9(6).                    CI0226
            12            CX03-CEDTD  PICTURE  9(2).                    CI0226
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            CX03-DREMT  PICTURE  9(8).                    CI0226
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0226
            11            CX03-CWRC2  PICTURE  99.                      CI0226
            11            CX03-CHCR2  PICTURE  99.                      CI0226
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0226
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0226
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0226
       01                 CX06.                                         CI0226
            10            CX06-CX06K.                                   CI0226
            11            CX06-C299.                                    CI0226
            12            CX06-CTID.                                    CI0226
            13            CX06-CTIDA  PICTURE  9(3).                    CI0226
            13            CX06-CTIDN.                                   CI0226
            14            CX06-CTIDNP PICTURE  X(13).                   CI0226
            14            CX06-CTIDND PICTURE  9(11).                   CI0226
            10            CX06-NPECK  PICTURE  9(02).                   CI0226
            10            CX06-FILLER PICTURE  X.                       CI0226
       01                 CX12.                                         CI0226
            10            CX12-CX12K.                                   CI0226
            11            CX12-CPMTC  PICTURE  99.                      CI0226
            11            CX12-NAPDS  PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            CX12-GESTD  PICTURE  9(8).                    CI0226
            10            CX12-GEEND  PICTURE  9(8).                    CI0226
            10            CX12-CIRMO  PICTURE  X(12).                   CI0226
            10            CX12-CDEST  PICTURE  99.                      CI0226
            10            CX12-APMTL  PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            10            CX12-DNPMT  PICTURE  9(8).                    CI0226
            10            CX12-NIRACM PICTURE  9(2).                    CI0226
            10            CX12-CPMTF  PICTURE  99.                      CI0226
            10            CX12-IPODM  PICTURE  X.                       CI0226
            10            CX12-CLUPD  PICTURE  9(3).                    CI0226
            10            CX12-DLAUP  PICTURE  9(8).                    CI0226
            10            CX12-CWRC   PICTURE  99.                      CI0226
            10            CX12-CHCR   PICTURE  99.                      CI0226
            10            CX12-GEOPD2 PICTURE  X(8).                    CI0226
            10            CX12-GEAUN  PICTURE  9(5).                    CI0226
            10            CX12-DPCHD  PICTURE  9(8).                    CI0226
            10            CX12-DNEXE  PICTURE  9(8).                    CI0226
            10            CX12-CCSMQ  PICTURE  X.                       CI0226
            10            CX12-GCUSPZ PICTURE  X(12).                   CI0226
            10            CX12-CORTY  PICTURE  X.                       CI0226
            10            CX12-CNAVR  PICTURE  X(1).                    CI0226
            10            CX12-DELOI3 PICTURE  9(6).                    CI0226
            10            CX12-ALOIDD PICTURE  9(9)V99                  CI0226
                          COMPUTATIONAL-3.                              CI0226
            10            CX12-FILLER PICTURE  X(5).                    CI0226
       01                 CX18.                                         CI0226
            10            CX18-CX18K.                                   CI0226
            11            CX18-NBASQ  PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            10            CX18-NPBN   PICTURE  X(20).                   CI0226
            10            CX18-CCBAT  PICTURE  99.                      CI0226
            10            CX18-DACHP  PICTURE  9(8).                    CI0226
            10            CX18-CSTPRE PICTURE  99.                      CI0226
            10            CX18-C199.                                    CI0226
            11            CX18-CLID.                                    CI0226
            12            CX18-CLIDO  PICTURE  9(3).                    CI0226
            12            CX18-CLIDN.                                   CI0226
            13            CX18-CLIDNP PICTURE  X(12).                   CI0226
            13            CX18-CLIDND PICTURE  9(8).                    CI0226
            10            CX18-MCSIG  PICTURE  X(30).                   CI0226
            10            CX18-CPBNU  PICTURE  X.                       CI0226
            10            CX18-CSPCR  PICTURE  99.                      CI0226
            10            CX18-DAPCR  PICTURE  9(8).                    CI0226
            10            CX18-FILLER PICTURE  XX.                      CI0226
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0226
            10            XW05-XW06.                                    CI0226
            11            XW05-XDBPCB.                                  CI0226
            12            XW05-XDBDNM PICTURE  X(08)                    CI0226
                          VALUE                SPACE.                   CI0226
            12            XW05-XSEGLV PICTURE  X(02)                    CI0226
                          VALUE                SPACE.                   CI0226
            12            XW05-XRC    PICTURE  X(02)                    CI0226
                          VALUE                SPACE.                   CI0226
            12            XW05-XPROPT PICTURE  X(04)                    CI0226
                          VALUE                SPACE.                   CI0226
            12            XW05-FILLER PICTURE  S9(5)                    CI0226
                          VALUE                ZERO                     CI0226
                          BINARY.                                       CI0226
            12            XW05-XSEGNM PICTURE  X(08)                    CI0226
                          VALUE                SPACE.                   CI0226
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0226
                          VALUE                ZERO                     CI0226
                          BINARY.                                       CI0226
            12            XW05-XSEGNB PICTURE  9(05)                    CI0226
                          VALUE                ZERO                     CI0226
                          BINARY.                                       CI0226
            12            XW05-XCOKEY PICTURE  X(70)                    CI0226
                          VALUE                SPACE.                   CI0226
            10            XW05-XW07.                                    CI0226
            11            XW05-XIOPCB.                                  CI0226
            12            XW05-XTERMI PICTURE  X(08)                    CI0226
                          VALUE                SPACE.                   CI0226
            12            XW05-FILLER PICTURE  XX                       CI0226
                          VALUE                SPACE.                   CI0226
            12            XW05-XRC1   PICTURE  X(02)                    CI0226
                          VALUE                SPACE.                   CI0226
            12            XW05-FILLER PICTURE  X(12)                    CI0226
                          VALUE                SPACE.                   CI0226
            12            XW05-XMODNM PICTURE  X(8)                     CI0226
                          VALUE                SPACE.                   CI0226
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0226
                          VALUE                ZERO.                    CI0226
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0226
                          VALUE                ZERO.                    CI0226
            10            XW05-XGU    PICTURE  X(4)                     CI0226
                          VALUE                'GU  '.                  CI0226
            10            XW05-XGHU   PICTURE  X(4)                     CI0226
                          VALUE                'GHU '.                  CI0226
            10            XW05-XGN    PICTURE  X(4)                     CI0226
                          VALUE                'GN  '.                  CI0226
            10            XW05-XGHN   PICTURE  X(4)                     CI0226
                          VALUE                'GHN '.                  CI0226
            10            XW05-XGNP   PICTURE  X(4)                     CI0226
                          VALUE                'GNP '.                  CI0226
            10            XW05-XGHNP  PICTURE  X(4)                     CI0226
                          VALUE                'GHNP'.                  CI0226
            10            XW05-XREPL  PICTURE  XXXX                     CI0226
                          VALUE                'REPL'.                  CI0226
            10            XW05-XISRT  PICTURE  X(4)                     CI0226
                          VALUE                'ISRT'.                  CI0226
            10            XW05-XDLET  PICTURE  X(4)                     CI0226
                          VALUE                'DLET'.                  CI0226
            10            XW05-XOPEN  PICTURE  X(4)                     CI0226
                          VALUE                'OPEN'.                  CI0226
            10            XW05-XCLSE  PICTURE  X(4)                     CI0226
                          VALUE                'CLSE'.                  CI0226
            10            XW05-XCHKP  PICTURE  X(4)                     CI0226
                          VALUE                'CHKP'.                  CI0226
            10            XW05-XXRST  PICTURE  X(4)                     CI0226
                          VALUE                'XRST'.                  CI0226
            10            XW05-XTERM  PICTURE  X(4)                     CI0226
                          VALUE                'TERM'.                  CI0226
            10            XW05-XNFPAC PICTURE  X(13)                    CI0226
                          VALUE                SPACE.                   CI0226
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0226
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0226
      *WORK AREA FOR ADB2DT
       01               7-DTIN-DATE.                                    ADB2DT
      *!WI pl=DT011                                                     ADB2DT
         05             7-DTIN-XDATC                                    ADB2DT
                        PICTURE XX.                                     CI0226
      *!WI pl=DT012                                                     ADB2DT
         05             7-DTIN-XDATY                                    ADB2DT
                        PICTURE XX.                                     CI0226
      *!WI pl=DT013                                                     ADB2DT
         05             7-DTIN-XDATM                                    ADB2DT
                        PICTURE XX.                                     CI0226
      *!WI pl=DT014                                                     ADB2DT
         05             7-DTIN-XDATD                                    ADB2DT
                        PICTURE XX.                                     CI0226
       01               7-DTOT-DATE.                                    ADB2DT
      *!WI pl=DT021                                                     ADB2DT
         05             7-DTOT-XDATC                                    ADB2DT
                        PICTURE XX.                                     CI0226
      *!WI pl=DT022                                                     ADB2DT
         05             7-DTOT-XDATY                                    ADB2DT
                        PICTURE XX.                                     CI0226
         05             FILLER PIC X                                    ADB2DT
                                            VALUE '-'.                  ADB2DT
      *!WI pl=DT025                                                     ADB2DT
         05             7-DTOT-XDATM                                    ADB2DT
                        PICTURE XX.                                     CI0226
         05             FILLER PIC X                                    ADB2DT
                                            VALUE '-'.                  ADB2DT
      *!WI pl=DT028                                                     ADB2DT
         05             7-DTOT-XDATD                                    ADB2DT
                        PICTURE XX.                                     CI0226
      *CCYYMMDD DATE IN
      *!WI
       01  DT01-DCACG
                        PICTURE 9(8).                                   CI0226
      *CCYY-MM-DD DATE OUT
      *!WI
       01  DT01-DCACD
                        PICTURE X(10).                                  CI0226
                                                                        AM0061
      ******************************************************************AM0061
      **     PCB ADDRESS LIST FOR CI0061.  MODULE CI0061 WILL NEED     *AM0061
      **     PCB'S FOR:                                                *AM0061
      **             ARRANGEMENT DATABASE(AR1P)                        *AM0061
      **             CONTRACT    DATABASE(CT1P)                        *AM0061
      ******************************************************************AM0061
                                                                        AM0061
       01  CI0061-PCB-ADDRESS-LIST.                                     AM0061
           05  CI0061-PCB-AR1P-PTR1      POINTER.                       AM0061
           05  CI0061-PCB-CT1P-PTR1      POINTER.                       AM0061
      *GENERATE INDEX FOR MESSAGE TEXT (ONLY IF 'OCCURS' SPECIFIED)     ADU071
      *                   MS03                                          ADU071
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
                                                                        AM0060
      ******************************************************************AM0060
      **     PCB ADDRESS LIST FOR CI0060.  MODULE CI0060 WILL NEED     *AM0060
      **     PCB'S FOR:                                                *AM0060
      **             ARRANGEMENT DATABASE(AR1P)                        *AM0060
      ******************************************************************AM0060
                                                                        AM0060
       01  CI0060-PCB-ADDRESS-LIST.                                     AM0060
           05  CI0060-PCB-AR1P-PTR1      POINTER.                       AM0060
      *=================================================================
      *WORKING STORAGE FOR DATE OF NEXT PAYMENT CALCULATIONS
      *=================================================================
       01  7-RA-DNPMT-AREA.                                             $DNPMT
           05  7-RA-DATE-TEMP.                                          $DNPMT
               10 7-RA-YR            PIC 9(4).                          $DNPMT
               10 7-RA-YR-X REDEFINES 7-RA-YR.                          $DNPMT
                  15 7-RA-YR-X-CC    PIC 9(2).                          $DNPMT
                  15 7-RA-YR-X-YY    PIC 9(2).                          $DNPMT
               10 7-RA-MO            PIC 99.                            $DNPMT
               10 7-RA-DA            PIC 99.                            $DNPMT
           05  7-RA-NUMB-OF-MOS      PIC 99.                            $DNPMT
           05  7-RA-NUMB-OF-DAYS     PIC 99.                            $DNPMT
           05  7-RA-LOOP-END         PIC X VALUE 'Y'.                   $DNPMT
           05  7-RA-7-WS-CIRMO-IN            PIC X(12).                 $DNPMT
           05  7-RA-CIRMO-YR REDEFINES 7-RA-7-WS-CIRMO-IN.              $DNPMT
               10  7-RA-CIRMO        PIC X                              $DNPMT
                             OCCURS 12.                                 $DNPMT
           05  7-RA-MONTH-LOAD       PIC X(24).                         $DNPMT
           05  7-RA-MONTH-TABLE REDEFINES 7-RA-MONTH-LOAD.              $DNPMT
               10 7-RA-MONTH  OCCURS 12 PIC 99.                         $DNPMT
           05 7-RA-T0                PIC 99V9(9).                       $DNPMT
           05 7-RA-T1 REDEFINES 7-RA-T0.                                $DNPMT
              10 7-RA-T1-INT       PIC 99.                              $DNPMT
              10 7-RA-T1-DEC       PIC V9(9).                           $DNPMT
           05 7-RA-ERR-FLAG        PIC X VALUE 'Y'.                     $DNPMT
      *
      *!WI
       01  7-WS-DNPMT
                        PICTURE 9(8).                                   CI0226
      *!WI
       01  7-WS-CPMTF
                        PICTURE 99.                                     CI0226
      *!WI
       01  7-WS-CIRMO
                        PICTURE X(12).                                  CI0226
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
                        PICTURE X(66)                                   CI0226
                           VALUE 'DB2 RESOURCE IN USE. TRY AGAIN '.     ADB221
       01               7-MAXM-RETRY     PIC S9(3) COMP-3               ADB221
                                                   VALUE +001.          ADB221
      ******************************************************************
      **                MISCELLANEOUS WORKING STORAGE                  *
      ******************************************************************
      **
      *TOTAL OF CATS ARRANGEMENTS AND PENDING AMOUNTS
      *!WI
       01 WS-APMTLT
                        PICTURE S9(9)V99                                CI0226
                          COMPUTATIONAL-3.                              CI0226
      **
      *AMOUNT REMAINING TO LIMIT
      *!WI
       01 WS-AMAXA2
                        PICTURE S9(7)V99.                               CI0226
      **
      *!WI
       01 WS-MORE-ARRANGEMENTS-IAIND    VALUE 'N'
                        PICTURE X.                                      CI0226
          88 MORE-ARRANGEMENTS          VALUE 'Y'.
          88 NO-MORE-ARRANGEMENTS       VALUE 'N'.
      **
      *!WI
       01 WS-MORE-ACCOUNTS-IAIND        VALUE 'N'
                        PICTURE X.                                      CI0226
          88 MORE-ACCOUNTS              VALUE 'Y'.
          88 NO-MORE-ACCOUNTS           VALUE 'N'.
      **
       01 WS-1-PAYMENT-AREA.
      *!WI
          05  WS-1-CPMTF
                        PICTURE 99.                                     CI0226
      *!WI
          05  WS-1-DNPMT
                        PICTURE 9(8).                                   CI0226
      *!WI
          05  WS-1-GESTD
                        PICTURE 9(8).                                   CI0226
      *!WI
          05  WS-1-GEEND
                        PICTURE 9(8).                                   CI0226
      *!WI
          05  WS-1-CIRMO
                        PICTURE X(12).                                  CI0226
      *!WI
          05  WS-1-APMTL
                        PICTURE S9(9)V99                                CI0226
                          COMPUTATIONAL-3.                              CI0226
      **
       01 WS-2-PAYMENT-AREA.
      *!WI
          05  WS-2-CPMTF
                        PICTURE 99.                                     CI0226
      *!WI
          05  WS-2-DNPMT
                        PICTURE 9(8).                                   CI0226
      *!WI
          05  WS-2-GESTD
                        PICTURE 9(8).                                   CI0226
      *!WI
          05  WS-2-GEEND
                        PICTURE 9(8).                                   CI0226
      *!WI
          05  WS-2-CIRMO
                        PICTURE X(12).                                  CI0226
      *!WI
          05  WS-2-APMTL
                        PICTURE S9(9)V99                                CI0226
                          COMPUTATIONAL-3.                              CI0226
      **
      *!WI
       01 WS-GEEND
                        PICTURE 9(8).                                   CI0226
      **
      *   USED TO DETERMINE IF TRANSACTION AMOUNT WILL
      *   INCREASE OR DECREASE TOTAL FOR THE DAY
      *!WI
       01 WS-CPROCM
                        PICTURE X.                                      CI0226
          88  WS-ADD         VALUE 'A'.
          88  WS-MODIFY      VALUE 'M'.
          88  WS-INACTIVATE  VALUE 'I'.
          88  WS-REACTIVATE  VALUE 'R'.
      **
      *!WI
       01 WS-APMTL
                        PICTURE S9(9)V99                                CI0226
                          COMPUTATIONAL-3.                              CI0226
      *USED TO CHECK FOR PENDING OD TRANSACTIONS
      *!WI
       01 WS-IAIND.                VALUE 'N'
                        PICTURE X.                                      CI0226
          88 NO-PENDING-OD         VALUE 'N'.
          88 PENDING-OD            VALUE 'Y'.
      *!WI
       01 WS-XNULL
                        PICTURE S9(4)                                   CI0226
                          BINARY.                                       CI0226
       01   DEBUT-WSS.                                                  CI0226
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0226
            05   IK     PICTURE X.                                      CI0226
       01  CONSTANTES-PAC.                                              CI0226
           05  FILLER  PICTURE X(87)   VALUE                            CI0226
                     '6015 CAT09/08/14CI0226ADMIN   14:35:05CI0226P AMERCI0226
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0226
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0226
           05  NUGNA   PICTURE X(5).                                    CI0226
           05  APPLI   PICTURE X(3).                                    CI0226
           05  DATGN   PICTURE X(8).                                    CI0226
           05  PROGR   PICTURE X(6).                                    CI0226
           05  CODUTI  PICTURE X(8).                                    CI0226
           05  TIMGN   PICTURE X(8).                                    CI0226
           05  PROGE   PICTURE X(8).                                    CI0226
           05  COBASE  PICTURE X(4).                                    CI0226
           05  DATGNC  PICTURE X(10).                                   CI0226
           05  RELEAS  PICTURE X(7).                                    CI0226
           05  DATGE   PICTURE X(10).                                   CI0226
           05  DATSQ   PICTURE X(10).                                   CI0226
       01  DATCE.                                                       CI0226
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0226
         05  DATOR.                                                     CI0226
           10  DATOA  PICTURE XX.                                       CI0226
           10  DATOM  PICTURE XX.                                       CI0226
           10  DATOJ  PICTURE XX.                                       CI0226
       01   VARIABLES-CONDITIONNELLES.                                  CI0226
            05                  FT      PICTURE X VALUE '0'.            CI0226
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0226
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0226
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J30GCR PICTURE S9(4) VALUE  ZERO.
            05           J30GJR PICTURE S9(4) VALUE  ZERO.
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0226
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0226
       01               S-CX01-SSA.                                     CI0226
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0226
                                      VALUE 'CX01    '.                 CI0226
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0226
            10          S-CX01-CCOD   PICTURE X(5)                      CI0226
                                      VALUE '-----'.                    CI0226
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0226
       01            S-CXU01-SSA.                                       CI0226
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX01    '.                 CI0226
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0226
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(CX01K'.                   CI0226
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0226
            10       S-CXU01-CX01K.                                     CI0226
            11       S-CXU01-C199.                                      CI0226
            12       S-CXU01-CLID.                                      CI0226
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0226
            13       S-CXU01-CLIDN.                                     CI0226
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0226
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0226
            10  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01               S-CX03-SSA.                                     CI0226
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0226
                                      VALUE 'CX03    '.                 CI0226
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0226
            10          S-CX03-CCOD   PICTURE X(5)                      CI0226
                                      VALUE '-----'.                    CI0226
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0226
       01            S-CXA03-SSA.                                       CI0226
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX03    '.                 CI0226
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0226
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(CARTY'.                   CI0226
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0226
            12       S-CXA03-CARTY    PICTURE  99.                      CI0226
            12  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01            S-CXB03-SSA.                                       CI0226
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX03    '.                 CI0226
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0226
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(NARRS'.                   CI0226
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0226
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            12  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01            S-CXC03-SSA.                                       CI0226
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX03    '.                 CI0226
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0226
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(CPMTG'.                   CI0226
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0226
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0226
            11  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01            S-CXD03-SSA.                                       CI0226
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX03    '.                 CI0226
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0226
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(GRCRNG'.                  CI0226
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0226
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0226
            11  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01            S-CXE03-SSA.                                       CI0226
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX03    '.                 CI0226
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0226
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(DEXDT'.                   CI0226
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0226
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0226
            11  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01            S-CXF03-SSA.                                       CI0226
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX03    '.                 CI0226
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0226
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(CY50'.                    CI0226
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0226
            11       S-CXF03-CY50.                                      CI0226
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0226
            11  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01            S-CXG03-SSA.                                       CI0226
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX03    '.                 CI0226
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0226
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(NBASQ'.                   CI0226
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0226
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01            S-CXH03-SSA.                                       CI0226
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX03    '.                 CI0226
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0226
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(NARID'.                   CI0226
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0226
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0226
            12  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01            S-CXU03-SSA.                                       CI0226
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX03    '.                 CI0226
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0226
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(CX03K'.                   CI0226
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0226
            11       S-CXU03-CX03K.                                     CI0226
            12       S-CXU03-CARTY    PICTURE  99.                      CI0226
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01               S-CX06-SSA.                                     CI0226
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0226
                                      VALUE 'CX06    '.                 CI0226
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0226
            10          S-CX06-CCOD   PICTURE X(5)                      CI0226
                                      VALUE '-----'.                    CI0226
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0226
       01            S-CXU06-SSA.                                       CI0226
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX06    '.                 CI0226
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0226
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(CX06K'.                   CI0226
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0226
            10       S-CXU06-CX06K.                                     CI0226
            11       S-CXU06-C299.                                      CI0226
            12       S-CXU06-CTID.                                      CI0226
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0226
            13       S-CXU06-CTIDN.                                     CI0226
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0226
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0226
            10  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01               S-CX12-SSA.                                     CI0226
            10         S1-CX12-SEGNAM PICTURE X(8)                      CI0226
                                      VALUE 'CX12    '.                 CI0226
            10         S1-CX12-CCOM   PICTURE X VALUE '*'.              CI0226
            10          S-CX12-CCOD   PICTURE X(5)                      CI0226
                                      VALUE '-----'.                    CI0226
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0226
       01            S-CXA12-SSA.                                       CI0226
            10      S1-CXA12-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX12    '.                 CI0226
            10      S1-CXA12-CCOM   PICTURE X VALUE '*'.                CI0226
            10       S-CXA12-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            10      S1-CXA12-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(CDEST'.                   CI0226
            10       S-CXA12-OPER  PICTURE XX VALUE ' ='.               CI0226
            10       S-CXA12-CDEST    PICTURE  99.                      CI0226
            10  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01            S-CXB12-SSA.                                       CI0226
            10      S1-CXB12-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX12    '.                 CI0226
            10      S1-CXB12-CCOM   PICTURE X VALUE '*'.                CI0226
            10       S-CXB12-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            10      S1-CXB12-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(DNPMT'.                   CI0226
            10       S-CXB12-OPER  PICTURE XX VALUE ' ='.               CI0226
            10       S-CXB12-DNPMT    PICTURE  9(8).                    CI0226
            10  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01            S-CXC12-SSA.                                       CI0226
            11      S1-CXC12-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX12    '.                 CI0226
            11      S1-CXC12-CCOM   PICTURE X VALUE '*'.                CI0226
            11       S-CXC12-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            11      S1-CXC12-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(NAPDS'.                   CI0226
            11       S-CXC12-OPER  PICTURE XX VALUE ' ='.               CI0226
            11       S-CXC12-NAPDS    PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01            S-CXU12-SSA.                                       CI0226
            10      S1-CXU12-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX12    '.                 CI0226
            10      S1-CXU12-CCOM   PICTURE X VALUE '*'.                CI0226
            10       S-CXU12-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            10      S1-CXU12-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(CX12K'.                   CI0226
            10       S-CXU12-OPER  PICTURE XX VALUE ' ='.               CI0226
            10       S-CXU12-CX12K.                                     CI0226
            11       S-CXU12-CPMTC    PICTURE  99.                      CI0226
            11       S-CXU12-NAPDS    PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11       S-CXU12-GESTD    PICTURE  9(8).                    CI0226
            10  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01               S-CX18-SSA.                                     CI0226
            10         S1-CX18-SEGNAM PICTURE X(8)                      CI0226
                                      VALUE 'CX18    '.                 CI0226
            10         S1-CX18-CCOM   PICTURE X VALUE '*'.              CI0226
            10          S-CX18-CCOD   PICTURE X(5)                      CI0226
                                      VALUE '-----'.                    CI0226
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0226
       01            S-CXA18-SSA.                                       CI0226
            10      S1-CXA18-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX18    '.                 CI0226
            10      S1-CXA18-CCOM   PICTURE X VALUE '*'.                CI0226
            10       S-CXA18-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            10      S1-CXA18-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(CSTPRE'.                  CI0226
            10       S-CXA18-OPER  PICTURE XX VALUE ' ='.               CI0226
            10       S-CXA18-CSTPRE   PICTURE  99.                      CI0226
            10  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01            S-CXB18-SSA.                                       CI0226
            10      S1-CXB18-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX18    '.                 CI0226
            10      S1-CXB18-CCOM   PICTURE X VALUE '*'.                CI0226
            10       S-CXB18-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            10      S1-CXB18-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(CSPCR'.                   CI0226
            10       S-CXB18-OPER  PICTURE XX VALUE ' ='.               CI0226
            10       S-CXB18-CSPCR    PICTURE  99.                      CI0226
            10  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01            S-CXU18-SSA.                                       CI0226
            10      S1-CXU18-SEGNAM PICTURE X(8)                        CI0226
                                      VALUE 'CX18    '.                 CI0226
            10      S1-CXU18-CCOM   PICTURE X VALUE '*'.                CI0226
            10       S-CXU18-CCOD   PICTURE X(5)                        CI0226
                                      VALUE '-----'.                    CI0226
            10      S1-CXU18-FLDNAM PICTURE X(9)                        CI0226
                                      VALUE '(CX18K'.                   CI0226
            10       S-CXU18-OPER  PICTURE XX VALUE ' ='.               CI0226
            10       S-CXU18-CX18K.                                     CI0226
            11       S-CXU18-NBASQ    PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            10  FILLER   PICTURE X    VALUE ')'.                        CI0226
       01   ZONES-UTILISATEUR PICTURE X.                                CI0226
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
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=XA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XA00.                                         CI0226
          05              XA00-SUITE.                                   CI0226
            15       FILLER         PICTURE  X(00106).                  CI0226
       01                 XA06  REDEFINES      XA00.                    CI0226
            10            XA06-XDBPCB.                                  CI0226
            11            XA06-XDBDNM PICTURE  X(08).                   CI0226
            11            XA06-XSEGLV PICTURE  X(02).                   CI0226
            11            XA06-XRC    PICTURE  X(02).                   CI0226
            11            XA06-XPROPT PICTURE  X(04).                   CI0226
            11            XA06-FILLER PICTURE  S9(5)                    CI0226
                          BINARY.                                       CI0226
            11            XA06-XSEGNM PICTURE  X(08).                   CI0226
            11            XA06-XKEYLN PICTURE  S9(05)                   CI0226
                          BINARY.                                       CI0226
            11            XA06-XSEGNB PICTURE  9(05)                    CI0226
                          BINARY.                                       CI0226
            11            XA06-XCOKEY PICTURE  X(70).                   CI0226
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=XB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XB00.                                         CI0226
          05              XB00-SUITE.                                   CI0226
            15       FILLER         PICTURE  X(00106).                  CI0226
       01                 XB06  REDEFINES      XB00.                    CI0226
            10            XB06-XDBPCB.                                  CI0226
            11            XB06-XDBDNM PICTURE  X(08).                   CI0226
            11            XB06-XSEGLV PICTURE  X(02).                   CI0226
            11            XB06-XRC    PICTURE  X(02).                   CI0226
            11            XB06-XPROPT PICTURE  X(04).                   CI0226
            11            XB06-FILLER PICTURE  S9(5)                    CI0226
                          BINARY.                                       CI0226
            11            XB06-XSEGNM PICTURE  X(08).                   CI0226
            11            XB06-XKEYLN PICTURE  S9(05)                   CI0226
                          BINARY.                                       CI0226
            11            XB06-XSEGNB PICTURE  9(05)                    CI0226
                          BINARY.                                       CI0226
            11            XB06-XCOKEY PICTURE  X(70).                   CI0226

      *PASS AREAS FROM CALLING MODULE

      *!WF DSP=QT DSL=QT SEL=92 FOR=I DES=1 LEV=1 PLT=10
       01                 QT12.                                         CI0226
            10            QT12-CX12K.                                   CI0226
            11            QT12-CPMTC  PICTURE  99.                      CI0226
            11            QT12-NAPDS  PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT12-GESTD  PICTURE  9(8).                    CI0226
            10            QT12-GEEND  PICTURE  9(8).                    CI0226
            10            QT12-CIRMO  PICTURE  X(12).                   CI0226
            10            QT12-CDEST  PICTURE  99.                      CI0226
            10            QT12-APMTL  PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            10            QT12-DNPMT  PICTURE  9(8).                    CI0226
            10            QT12-NIRACM PICTURE  9(2).                    CI0226
            10            QT12-CPMTF  PICTURE  99.                      CI0226
            10            QT12-IPODM  PICTURE  X.                       CI0226
            10            QT12-CLUPD  PICTURE  9(3).                    CI0226
            10            QT12-DLAUP  PICTURE  9(8).                    CI0226
            10            QT12-CWRC   PICTURE  99.                      CI0226
            10            QT12-CHCR   PICTURE  99.                      CI0226
            10            QT12-GEOPD2 PICTURE  X(8).                    CI0226
            10            QT12-GEAUN  PICTURE  9(5).                    CI0226
            10            QT12-DPCHD  PICTURE  9(8).                    CI0226
            10            QT12-DNEXE  PICTURE  9(8).                    CI0226
            10            QT12-CCSMQ  PICTURE  X.                       CI0226
            10            QT12-GCUSPZ PICTURE  X(12).                   CI0226
            10            QT12-CORTY  PICTURE  X.                       CI0226
            10            QT12-CNAVR  PICTURE  X(1).                    CI0226
            10            QT12-DELOI3 PICTURE  9(6).                    CI0226
            10            QT12-ALOIDD PICTURE  9(9)V99                  CI0226
                          COMPUTATIONAL-3.                              CI0226
            10            QT12-FILLER PICTURE  X(5).                    CI0226
       01                 QT63.                                         CI0226
            10            QT63-INPUT.                                   CI0226
            11            QT63-GEMDA  PICTURE  9(8).                    CI0226
            11            QT63-NSEQ4B PICTURE  9(8)                     CI0226
                          BINARY.                                       CI0226
            11            QT63-DCACG  PICTURE  9(8).                    CI0226
            11            QT63-C199.                                    CI0226
            12            QT63-CLID.                                    CI0226
            13            QT63-CLIDO  PICTURE  9(3).                    CI0226
            13            QT63-CLIDN.                                   CI0226
            14            QT63-CLIDNP PICTURE  X(12).                   CI0226
            14            QT63-CLIDND PICTURE  9(8).                    CI0226
            11            QT63-NARRS  PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-AACTV  PICTURE  S9(11)V99                CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-CTCUS  PICTURE  999.                     CI0226
            11            QT63-CTOWN  PICTURE  9(3).                    CI0226
            11            QT63-CTKRAA PICTURE  X(12).                   CI0226
            11            QT63-AMAXA2 PICTURE  S9(7)V99.                CI0226
            11            QT63-AMAXAL PICTURE  S9(7)V99.                CI0226
            10            QT63-OUTPT1.                                  CI0226
            11            QT63-IARTYA PICTURE  X.                       CI0226
            11            QT63-NMESAA PICTURE  9(6).                    CI0226
            11            QT63-IACHI  PICTURE  X.                       CI0226
            11            QT63-NMESAC PICTURE  9(6).                    CI0226
            11            QT63-IAIND2 PICTURE  X.                       CI0226
            11            QT63-NMESAR PICTURE  9(6).                    CI0226
            11            QT63-IARRGA PICTURE  X.                       CI0226
            11            QT63-NMESA0 PICTURE  9(6).                    CI0226
            11            QT63-IARLNA PICTURE  X.                       CI0226
            11            QT63-NMESA1 PICTURE  9(6).                    CI0226
            11            QT63-IARCDA PICTURE  X.                       CI0226
            11            QT63-NMESA2 PICTURE  9(6).                    CI0226
            11            QT63-IARCPA PICTURE  X.                       CI0226
            11            QT63-NMESA3 PICTURE  9(6).                    CI0226
            11            QT63-IARRG1 PICTURE  X.                       CI0226
            11            QT63-NMESA4 PICTURE  9(6).                    CI0226
            11            QT63-IARLN1 PICTURE  X.                       CI0226
            11            QT63-NMESA5 PICTURE  9(6).                    CI0226
            11            QT63-IARCD1 PICTURE  X.                       CI0226
            11            QT63-NMESA6 PICTURE  9(6).                    CI0226
            11            QT63-IARCP1 PICTURE  X.                       CI0226
            11            QT63-NMESA7 PICTURE  9(6).                    CI0226
            11            QT63-IAINDA PICTURE  X.                       CI0226
            11            QT63-NAPDSK PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-CDEST1 PICTURE  99.                      CI0226
            11            QT63-CPCCDE PICTURE  99.                      CI0226
            11            QT63-IFQAN  PICTURE  X.                       CI0226
            11            QT63-IFQSA  PICTURE  X.                       CI0226
            11            QT63-IFQQT  PICTURE  X.                       CI0226
            11            QT63-IFQBM  PICTURE  X.                       CI0226
            11            QT63-IFQMO  PICTURE  X.                       CI0226
            11            QT63-IFQSM  PICTURE  X.                       CI0226
            11            QT63-IFQBW  PICTURE  X.                       CI0226
            11            QT63-IFQWK  PICTURE  X.                       CI0226
            11            QT63-IFQOD  PICTURE  X.                       CI0226
            11            QT63-ALMIN  PICTURE  S9(5)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-ALMIN3 PICTURE  S9(5)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-ALPAGQ PICTURE  S9(7)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-ALPAGM PICTURE  S9(7)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-ALPLDT PICTURE  9(8).                    CI0226
            11            QT63-ALDDUE PICTURE  9(08).                   CI0226
            11            QT63-DGRAC  PICTURE  9(08).                   CI0226
            11            QT63-GESTE  PICTURE  9(8).                    CI0226
            11            QT63-GESTL  PICTURE  9(8).                    CI0226
            11            QT63-GEENE  PICTURE  9(8).                    CI0226
            11            QT63-GEENL  PICTURE  9(8).                    CI0226
            11            QT63-GESTD1 PICTURE  9(8).                    CI0226
            11            QT63-DSKIP  PICTURE  9(8).                    CI0226
            11            QT63-DSKIP1 PICTURE  9(8).                    CI0226
            11            QT63-DSKIP2 PICTURE  9(8).                    CI0226
            11            QT63-DIRAC1 PICTURE  XX.                      CI0226
            11            QT63-CIRAT  PICTURE  999.                     CI0226
            11            QT63-CIRAS  PICTURE  999.                     CI0226
            11            QT63-CQACT  PICTURE  999.                     CI0226
            11            QT63-IERRC  PICTURE  X.                       CI0226
            11            QT63-NMESA  PICTURE  9(6).                    CI0226
            10            QT63-OUTPT2.                                  CI0226
            11            QT63-CPMTG1 PICTURE  99.                      CI0226
            11            QT63-MPMTF1 PICTURE  X(24).                   CI0226
            11            QT63-ACOTL1 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-ACOTU1 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-CPMTG2 PICTURE  99.                      CI0226
            11            QT63-MPMTF2 PICTURE  X(24).                   CI0226
            11            QT63-ACOTL2 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-ACOTU2 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-CPMTG3 PICTURE  99.                      CI0226
            11            QT63-MPMTF3 PICTURE  X(24).                   CI0226
            11            QT63-ACOTL3 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-ACOTU3 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-CPMTG4 PICTURE  99.                      CI0226
            11            QT63-MPMTF4 PICTURE  X(24).                   CI0226
            11            QT63-ACOTL4 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-ACOTU4 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-CPMTG5 PICTURE  99.                      CI0226
            11            QT63-MPMTF5 PICTURE  X(24).                   CI0226
            11            QT63-ACOTL5 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-ACOTU5 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-CPMTG6 PICTURE  99.                      CI0226
            11            QT63-MPMTF6 PICTURE  X(24).                   CI0226
            11            QT63-ACOTL6 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-ACOTU6 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-CPMTG7 PICTURE  99.                      CI0226
            11            QT63-MPMTF7 PICTURE  X(24).                   CI0226
            11            QT63-ACOTL7 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-ACOTU7 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-CPMTG8 PICTURE  99.                      CI0226
            11            QT63-MPMTF8 PICTURE  X(24).                   CI0226
            11            QT63-ACOTL8 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-ACOTU8 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-CPMTG9 PICTURE  99.                      CI0226
            11            QT63-MPMTF9 PICTURE  X(24).                   CI0226
            11            QT63-ACOTL9 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-ACOTU9 PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-MPMTT1 PICTURE  X(20).                   CI0226
            11            QT63-MPMTT2 PICTURE  X(20).                   CI0226
            11            QT63-MPMTT3 PICTURE  X(20).                   CI0226
            11            QT63-MPMTT4 PICTURE  X(20).                   CI0226
            11            QT63-MPMTT5 PICTURE  X(20).                   CI0226
            11            QT63-IABAA  PICTURE  X(01).                   CI0226
            11            QT63-IIBAA  PICTURE  X(01).                   CI0226
            11            QT63-IDBMO  PICTURE  X.                       CI0226
            11            QT63-IDBQT  PICTURE  X.                       CI0226
            11            QT63-IDBSA  PICTURE  X.                       CI0226
            11            QT63-IDBAN  PICTURE  X.                       CI0226
            11            QT63-CPCCD1 PICTURE  9(5).                    CI0226
            11            QT63-PRCOD  PICTURE  9(5).                    CI0226
            11            QT63-OWNOUT PICTURE  X(60).                   CI0226
            11            QT63-TSECD  PICTURE  X(30).                   CI0226
            11            QT63-CSPRP  PICTURE  X(04).                   CI0226
            11            QT63-QSTSO  PICTURE  S999                     CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-QSTSM  PICTURE  S999                     CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-INROA  PICTURE  X(1).                    CI0226
            11            QT63-FILLER PICTURE  X(19).                   CI0226
            10            QT63-LIMITS                                   CI0226
                          REDEFINES            QT63-OUTPT2.             CI0226
            11            QT63-QT6R                                     CI0226
                          OCCURS       009     TIMES.                   CI0226
            12            QT63-CPMTFA PICTURE  X(2).                    CI0226
            12            QT63-MPMTFL PICTURE  X(24).                   CI0226
            12            QT63-ACOTL  PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            12            QT63-ACOTU  PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-MPMTT  PICTURE  X(20)                    CI0226
                          OCCURS       005     TIMES.                   CI0226
            10            QT63-CX06.                                    CI0226
            11            QT63-CX06K.                                   CI0226
            12            QT63-C299.                                    CI0226
            13            QT63-CTID.                                    CI0226
            14            QT63-CTIDA  PICTURE  9(3).                    CI0226
            14            QT63-CTIDN.                                   CI0226
            15            QT63-CTIDNP PICTURE  X(13).                   CI0226
            15            QT63-CTIDND PICTURE  9(11).                   CI0226
            11            QT63-NPECK  PICTURE  9(02).                   CI0226
            11            QT63-FILLER PICTURE  X.                       CI0226
            10            QT63-CX12.                                    CI0226
            11            QT63-CX12K.                                   CI0226
            12            QT63-CPMTC  PICTURE  99.                      CI0226
            12            QT63-NAPDS  PICTURE  S9(3)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            12            QT63-GESTD  PICTURE  9(8).                    CI0226
            11            QT63-GEEND  PICTURE  9(8).                    CI0226
            11            QT63-CIRMO  PICTURE  X(12).                   CI0226
            11            QT63-CDEST  PICTURE  99.                      CI0226
            11            QT63-APMTL  PICTURE  S9(9)V99                 CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-DNPMT  PICTURE  9(8).                    CI0226
            11            QT63-NIRACM PICTURE  9(2).                    CI0226
            11            QT63-CPMTF  PICTURE  99.                      CI0226
            11            QT63-IPODM  PICTURE  X.                       CI0226
            11            QT63-CLUPD  PICTURE  9(3).                    CI0226
            11            QT63-DLAUP  PICTURE  9(8).                    CI0226
            11            QT63-CWRC   PICTURE  99.                      CI0226
            11            QT63-CHCR   PICTURE  99.                      CI0226
            11            QT63-GEOPD2 PICTURE  X(8).                    CI0226
            11            QT63-GEAUN  PICTURE  9(5).                    CI0226
            11            QT63-DPCHD  PICTURE  9(8).                    CI0226
            11            QT63-DNEXE  PICTURE  9(8).                    CI0226
            11            QT63-CCSMQ  PICTURE  X.                       CI0226
            11            QT63-GCUSPZ PICTURE  X(12).                   CI0226
            11            QT63-CORTY  PICTURE  X.                       CI0226
            11            QT63-CNAVR  PICTURE  X(1).                    CI0226
            11            QT63-DELOI3 PICTURE  9(6).                    CI0226
            11            QT63-ALOIDD PICTURE  9(9)V99                  CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            QT63-FILLER PICTURE  X(5).                    CI0226
       01                 QT92.                                         CI0226
            10            QT92-CUPIQ  PICTURE  X.                       CI0226
            10            QT92-CPROCM PICTURE  X.                       CI0226
            10            QT92-IMQMG  PICTURE  X.                       CI0226
            10            QT92-C199.                                    CI0226
            11            QT92-CLID.                                    CI0226
            12            QT92-CLIDO  PICTURE  9(3).                    CI0226
            12            QT92-CLIDN.                                   CI0226
            13            QT92-CLIDNP PICTURE  X(12).                   CI0226
            13            QT92-CLIDND PICTURE  9(8).                    CI0226
            10            QT92-CCONF  PICTURE  X(25).                   CI0226
            10            QT92-NIPAD  PICTURE  X(15).                   CI0226
            10            QT92-CSLCT  PICTURE  X.                       CI0226
            10            QT92-PRBAR  PICTURE  999V999.                 CI0226
            10            QT92-CCSDSA PICTURE  X(1).                    CI0226
            10            QT92-DXTMSA PICTURE  X(26).                   CI0226
            10            QT92-DXTMS2 PICTURE  X(26).                   CI0226
            10            QT92-C198.                                    CI0226
            11            QT92-CLNAM.                                   CI0226
            12            QT92-CLNAMH PICTURE  X(6).                    CI0226
            12            QT92-CLNAMF PICTURE  X(20).                   CI0226
            12            QT92-CLNAMM.                                  CI0226
            13            QT92-CLNAMI PICTURE  X.                       CI0226
            13            QT92-CLNAMR PICTURE  X(14).                   CI0226
            12            QT92-CLNAML PICTURE  X(25).                   CI0226
            12            QT92-CLNAMS PICTURE  X(4).                    CI0226
            10            QT92-GECKD2 PICTURE  9.                       CI0226
            10            QT92-CLTIN  PICTURE  9(12).                   CI0226
            10            QT92-CLID01 PICTURE  X(23).                   CI0226
            10            QT92-NARRSA PICTURE  X(3).                    CI0226
            10            QT92-GECSQ2 PICTURE  9(3).                    CI0226
            10            QT92-NAPDSA PICTURE  X(3).                    CI0226
            10            QT92-CLID4  PICTURE  X(23).                   CI0226
            10            QT92-CLORN  PICTURE  X(45).                   CI0226
            10            QT92-NTR    PICTURE  9(8).                    CI0226
            10            QT92-GECKD1 PICTURE  9.                       CI0226
            10            QT92-NPBN   PICTURE  X(20).                   CI0226
            10            QT92-CCBAT  PICTURE  99.                      CI0226
            10            QT92-TTBAL  PICTURE  X(15).                   CI0226
            10            QT92-MCSIG  PICTURE  X(30).                   CI0226
            10            QT92-CPMTF  PICTURE  99.                      CI0226
            10            QT92-MPMTFL PICTURE  X(24).                   CI0226
            10            QT92-CPMTC  PICTURE  99.                      CI0226
            10            QT92-MPMTT  PICTURE  X(20).                   CI0226
            10            QT92-APMTF3 PICTURE  9(09)V99.                CI0226
            10            QT92-DNPMT  PICTURE  9(8).                    CI0226
            10            QT92-GESTD  PICTURE  9(8).                    CI0226
            10            QT92-GEEND  PICTURE  9(8).                    CI0226
            10            QT92-CDEST  PICTURE  99.                      CI0226
            10            QT92-TARST  PICTURE  X(10).                   CI0226
            10            QT92-NIRACM PICTURE  9(2).                    CI0226
            10            QT92-DIRAYR PICTURE  9(4).                    CI0226
            10            QT92-C299.                                    CI0226
            11            QT92-CTID.                                    CI0226
            12            QT92-CTIDA  PICTURE  9(3).                    CI0226
            12            QT92-CTIDN.                                   CI0226
            13            QT92-CTIDNP PICTURE  X(13).                   CI0226
            13            QT92-CTIDND PICTURE  9(11).                   CI0226
            10            QT92-GECKD  PICTURE  9.                       CI0226
            10            QT92-PRCLN  PICTURE  X(60).                   CI0226
            10            QT92-MRPSN  PICTURE  X(12).                   CI0226
            10            QT92-CTTLN1 PICTURE  X(30).                   CI0226
            10            QT92-CTTLN2 PICTURE  X(30).                   CI0226
            10            QT92-CTTLN3 PICTURE  X(30).                   CI0226
            10            QT92-CTTBO1 PICTURE  X(45).                   CI0226
            10            QT92-CTTBO2 PICTURE  X(45).                   CI0226
            10            QT92-CPCCDE PICTURE  99.                      CI0226
            10            QT92-CPCCDF PICTURE  99.                      CI0226
            10            QT92-GCUSPZ PICTURE  X(12).                   CI0226
            10            QT92-TSECD  PICTURE  X(30).                   CI0226
            10            QT92-PRCODA PICTURE  X(5).                    CI0226
            10            QT92-PRSCD  PICTURE  X(9).                    CI0226
            10            QT92-MRGNN  PICTURE  X(8).                    CI0226
            10            QT92-GEAUN  PICTURE  9(5).                    CI0226
            10            QT92-GEOPD2 PICTURE  X(8).                    CI0226
            10            QT92-CTTYPG PICTURE  X(04).                   CI0226
            10            QT92-FILLER PICTURE  X(52).                   CI0226
            10            QT92-QITEM  PICTURE  9(3).                    CI0226
            10            QT92-NMESA  PICTURE  9(6)                     CI0226
                          OCCURS       005     TIMES.                   CI0226
            10            QT92-NGEOR  PICTURE  9(08).                   CI0226
            10            QT92-FILLER PICTURE  X(56).                   CI0226
      *!WF DSP=QT DSL=QT SEL=63 FOR=I DES=1 LEV=1 PLT=10
      *!WF DSP=QT DSL=CX SEL=12 FOR=I DES=1 LEV=1 PLT=10

      *RETURN CODE TO CALLING MODULE

       01  LK01.
      *!WI
         05  LK01-IAIND
                        PICTURE X.                                      CI0226
      *!WI
         05  LK01-AMAXAN
                        PICTURE S9(7)V99                                CI0226
                          COMPUTATIONAL-3.                              CI0226
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0226
          05              DE00-SUITE.                                   CI0226
            15       FILLER         PICTURE  X(00653).                  CI0226
       01                 DE10  REDEFINES      DE00.                    CI0226
            10            DE10-DU11.                                    CI0226
            11            DE10-XFONC  PICTURE  X(4).                    CI0226
            11            DE10-MPSBN  PICTURE  X(8).                    CI0226
            11            DE10-XDBDNM PICTURE  X(08).                   CI0226
            11            DE10-XSEGNM PICTURE  X(08).                   CI0226
            11            DE10-XRC    PICTURE  X(02).                   CI0226
            11            DE10-MSEG   PICTURE  X(08).                   CI0226
            11            DE10-XCOKEY PICTURE  X(70).                   CI0226
            11            DE10-CUIBR  PICTURE  X(01).                   CI0226
            11            DE10-CUIBA  PICTURE  X(01).                   CI0226
            11            DE10-IPBIK  PICTURE  X(1).                    CI0226
            10            DE10-DU03.                                    CI0226
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            DE10-CMSSF  PICTURE  XX.                      CI0226
            11            DE10-DU09.                                    CI0226
            12            DE10-CMESA  PICTURE  S9(9)                    CI0226
                          BINARY.                                       CI0226
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0226
                          BINARY.                                       CI0226
            12            DE10-CMESB  PICTURE  S9(9)                    CI0226
                          BINARY.                                       CI0226
            12            DE10-CMSST  PICTURE  S9(9)                    CI0226
                          BINARY.                                       CI0226
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0226
                          BINARY.                                       CI0226
            12            DE10-QELLAA PICTURE  S9(9)                    CI0226
                          BINARY.                                       CI0226
            12            DE10-TMESS4 PICTURE  X(512).                  CI0226
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
       01                 MS00.                                         CI0226
          05              MS00-SUITE.                                   CI0226
            15       FILLER         PICTURE  X(00542).                  CI0226
       01                 MS03  REDEFINES      MS00.                    CI0226
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            10            MS03-CMSSF  PICTURE  XX.                      CI0226
            10            MS03-DU09.                                    CI0226
            11            MS03-CMESA  PICTURE  S9(9)                    CI0226
                          BINARY.                                       CI0226
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0226
                          BINARY.                                       CI0226
            11            MS03-CMESB  PICTURE  S9(9)                    CI0226
                          BINARY.                                       CI0226
            11            MS03-CMSST  PICTURE  S9(9)                    CI0226
                          BINARY.                                       CI0226
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0226
                          BINARY.                                       CI0226
            11            MS03-QELLAA PICTURE  S9(9)                    CI0226
                          BINARY.                                       CI0226
            11            MS03-TMESS4 PICTURE  X(512).                  CI0226
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0226
            10            MX11-QMSGS  PICTURE  9(03).                   CI0226
            10            MX11-PJ09                                     CI0226
                          OCCURS       025     TIMES.                   CI0226
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0226
                          COMPUTATIONAL-3.                              CI0226
            11            MX11-CMESB  PICTURE  S9(9)                    CI0226
                          BINARY.                                       CI0226
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                QT92
                                QT63
                                QT12
                                LK01
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
       F0TSC.                                                           lv10
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF XA06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF XB06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
       F0TSC-FN. EXIT.
      *N01.      NOTE *************************************.            CI0226
      *               *                                   *             CI0226
      *               *INITIALISATIONS                    *             CI0226
      *               *                                   *             CI0226
      *               *************************************.            CI0226
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
      *N02CA.    NOTE *---> INITIALIZE RETURN VARIABLES   *.
       F02CA.                                                           lv10
      *
           MOVE        'Y' TO LK01-IAIND
           MOVE        0 TO LK01-AMAXAN
           INITIALIZE  WS-APMTLT.
       F02CA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0226
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0226
      *               *                                   *             CI0226
      *               *FIN DE TRAITEMENT                  *             CI0226
      *               *                                   *             CI0226
      *               *************************************.            CI0226
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0226
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *DETERMINE EXISTING AR1P AMOUNTS    *
      *               *                                   *
      *               *************************************.
       F30.           EXIT.                                             lv05
      *N30CC.    NOTE *GET ALL BAS FOR THE CLIENT         *.
       F30CC.                                                           lv10
      *
           SET MORE-ARRANGEMENTS TO TRUE
           MOVE        01 TO 7-WORK-CARTY
           MOVE        'GN' TO 7-WORK-CFUNC
           MOVE        000 TO 7-WORK-NARRSK
      *
           PERFORM     F91BA THRU F91BA-FN.
       F30CC-FN. EXIT.
      *N30CE.    NOTE *RESET POSITION - GET CX01 ROOT     *.
       F30CE.                                                           lv10
           INITIALIZE  CX01
           MOVE        QT92-CLID01 TO S-CXU01-CLID
           PERFORM     F94CX THRU F94CX-FN.
       F30CE-FN. EXIT.
      *N30EC.    NOTE *PROCESS ALL BANKS & ARRANGEMENTS   *.
       F30EC.                       GO TO     F30EC-B.                  lv10
       F30EC-A.
                 IF    NO-MORE-ARRANGEMENTS
                                    GO TO     F30EC-FN.
       F30EC-B.
      *FOR THE CLIENT
      *N30GC.    NOTE *LOOP THRU RCDS CI0060 RETURNED     *.
       F30GC.                                                           lv15
           MOVE        1                        TO J30GCR
                                    GO TO     F30GC-B.
       F30GC-A.
           ADD         1                        TO J30GCR.
       F30GC-B.
           IF          J30GCR                   >  CB60-GECTR
                                    GO TO     F30GC-FN.
           MOVE        CB60-CLID01 TO CX01-CLID
           MOVE        CB60-CX03 (J30GCR) TO CX03
           MOVE        CB60-CX18 (J30GCR) TO CX18
           SET MORE-ACCOUNTS TO TRUE.
      *N30GE.    NOTE *ARRANGEMENT IS ACTIVE              *.
       F30GE.    IF    CX03-CARST = 01                                  lv20
                 AND   CX18-CLID = QT92-CLID4
                 AND   CX18-CCBAT = QT92-CCBAT
                 AND   CX18-NPBN = QT92-NPBN
                 NEXT SENTENCE ELSE GO TO     F30GE-FN.
      *BANK IS THE SAME
      *ACCOUNT TYPE IS THE SAME
      *ACCOUNT NUMBER IS THE SAME
      *N30GH.    NOTE *GET ALL ACCOUNTS / AMOUNTS INFO    *.
       F30GH.                       GO TO     F30GH-B.                  lv25
       F30GH-A.
                 IF    NO-MORE-ACCOUNTS
                                    GO TO     F30GH-FN.
       F30GH-B.
      *FOR THE ARRANGEMENT
      *
           PERFORM     F91BR THRU F91BR-FN.
      *N30GJ.    NOTE *LOOP ON POSITIONS RETURNED         *.
       F30GJ.                                                           lv30
           MOVE        1                        TO J30GJR
                                    GO TO     F30GJ-B.
       F30GJ-A.
           ADD         1                        TO J30GJR.
       F30GJ-B.
           IF          J30GJR                   >  100
                                    GO TO     F30GJ-FN.
      *N30GN.    NOTE *MOVE TO SEGMENT WS                 *.
       F30GN.    IF    AR61-CTIDA (J30GJR) > 000                        lv35
                 NEXT SENTENCE ELSE GO TO     F30GN-FN.
           MOVE        AR61-CX06 (J30GJR) TO CX06
           MOVE        AR61-CX12 (J30GJR) TO CX12.
      *N30IC.    NOTE *CX12 IS ACTIVE                     *.
       F30IC.    IF    CX12-CDEST = 01                                  lv40
                 NEXT SENTENCE ELSE GO TO     F30IC-FN.
      *N30IG.    NOTE *CHECK IF PAYMENT DATES MATCH UP    *.
       F30IG.                                                           lv45
      *
           MOVE        QT92-CPMTF TO WS-1-CPMTF
           MOVE        QT92-DNPMT TO WS-1-DNPMT
           MOVE        QT92-GESTD TO WS-1-GESTD
           MOVE        QT92-GEEND TO WS-1-GEEND
           MOVE ALL    'X' TO WS-1-CIRMO
           MOVE        QT92-APMTF3 TO WS-1-APMTL
      *
           MOVE        CX12-CPMTF TO WS-2-CPMTF
           MOVE        CX12-DNPMT TO WS-2-DNPMT
           MOVE        CX12-GESTD TO WS-2-GESTD
           MOVE        CX12-GEEND TO WS-2-GEEND
           MOVE        CX12-CIRMO TO WS-2-CIRMO
           MOVE        CX12-APMTL TO WS-2-APMTL
      *
           PERFORM     F96CA THRU F96CA-FN.
       F30IG-FN. EXIT.
       F30IC-FN. EXIT.
       F30GN-900. GO TO F30OA-FN.
       F30GN-FN. EXIT.
      *N30OA.    NOTE *NO MORE POSITIONS                  *.
       F30OA.                                                           lv35
      *
           SET NO-MORE-ACCOUNTS TO TRUE.
       F30OA-FN. EXIT.
       F30GJ-900. GO TO F30GJ-A.
       F30GJ-FN. EXIT.
       F30GH-900. GO TO F30GH-A.
       F30GH-FN. EXIT.
       F30GE-FN. EXIT.
       F30GC-900. GO TO F30GC-A.
       F30GC-FN. EXIT.
      *N30PA.    NOTE *IF MORE ARR EXIST SET UP TO GET    *.
       F30PA.    IF    CB60-NARRSB NOT = ZERO                           lv15
                 NEXT SENTENCE ELSE GO TO     F30PA-FN.
      *ADDITIONAL ARRANGEMENTS
           MOVE        CB60-NARRSB TO 7-WORK-NARRSK.
       F30PA-900. GO TO F30QA-FN.
       F30PA-FN. EXIT.
      *N30QA.    NOTE *ELSE... NO MORE ARR EXIST          *.
       F30QA.                                                           lv15
      *
           SET NO-MORE-ARRANGEMENTS TO TRUE.
       F30QA-FN. EXIT.
       F30EC-900. GO TO F30EC-A.
       F30EC-FN. EXIT.
       F30-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *DETERMINE PENDING TRAN AMOUNTS     *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40CB.    NOTE *INITIALIZE DB SEGMENTS WS          *.
       F40CB.                                                           lv10
      *
           INITIALIZE  CT1S CT1T
           SET MORE-ARRANGEMENTS TO TRUE.
       F40CB-FN. EXIT.
      *N40EC.    NOTE *SET DB KEYS                        *.
       F40EC.                                                           lv10
      *
           MOVE        QT92-CLID TO CT1S-NUCLI
           MOVE        '01' TO CT1S-CARTYA
           MOVE        QT92-NTR TO CT1T-NAGTB
           MOVE        QT92-CCBAT TO CT1T-CTBAC
           MOVE        QT92-NPBN TO CT1T-NPBN.
       F40EC-FN. EXIT.
      *N40EE.    NOTE *OPEN CURSOR OF PENDING TRANS       *.
       F40EE.                                                           lv10
      *
           PERFORM     F94RO THRU F94RO-FN.
       F40EE-FN. EXIT.
      *N40EG.    NOTE *PROCESS CURSOR OF PENDING TRANS    *.
       F40EG.                       GO TO     F40EG-B.                  lv10
       F40EG-A.
                 IF    NO-MORE-ARRANGEMENTS
                                    GO TO     F40EG-FN.
       F40EG-B.
      *
           PERFORM     F94RF THRU F94RF-FN.
      *N40IC.    NOTE *PENDING TRAN FOUND                 *.
       F40IC.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F40IC-FN.
      *N40IE.    NOTE *CONVERT DATES FROM DB2 TO GREG     *.
       F40IE.                                                           lv20
      *
           MOVE        CT1S-DATSD TO DT01-DCACD
           PERFORM     F91DG THRU F91DG-FN
           MOVE        DT01-DCACG TO WS-2-GESTD
      *
           MOVE        CT1S-DACPR TO DT01-DCACD
           PERFORM     F91DG THRU F91DG-FN
           MOVE        DT01-DCACG TO WS-2-DNPMT
      *
                 IF    CT1S-DTEND < '9999-12-31'                        DOT
           MOVE        CT1S-DACPR TO DT01-DCACD
           PERFORM     F91DG THRU F91DG-FN
           MOVE        DT01-DCACG TO WS-2-GEEND
                 ELSE
           MOVE        ZEROS TO WS-2-GEEND.
       F40IE-FN. EXIT.
      *N40IG.    NOTE *CHECK IF PAYMENT DATES MATCH UP    *.
       F40IG.                                                           lv20
      *
           MOVE        QT92-CPMTF TO WS-1-CPMTF
           MOVE        QT92-DNPMT TO WS-1-DNPMT
           MOVE        QT92-GESTD TO WS-1-GESTD
           MOVE        QT92-GEEND TO WS-1-GEEND
           MOVE ALL    'X' TO WS-1-CIRMO
           MOVE        QT92-APMTF3 TO WS-1-APMTL
      *
           MOVE        CT1S-CPMTFA TO WS-2-CPMTF
           MOVE ALL    'X' TO WS-2-CIRMO
           MOVE        CT1S-APMTO TO WS-2-APMTL
      *
           PERFORM     F96CA THRU F96CA-FN.
       F40IG-FN. EXIT.
       F40IC-900. GO TO F40QA-FN.
       F40IC-FN. EXIT.
      *N40QA.    NOTE *ELSE... NO MORE ARR EXIST          *.
       F40QA.                                                           lv15
      *
           SET NO-MORE-ARRANGEMENTS TO TRUE.
       F40QA-FN. EXIT.
       F40EG-900. GO TO F40EG-A.
       F40EG-FN. EXIT.
      *N40ZE.    NOTE *CLOSE CURSOR OF PENDING TRANS      *.
       F40ZE.                                                           lv10
      *
           PERFORM     F94RX THRU F94RX-FN.
       F40ZE-FN. EXIT.
       F40-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *LOOK FOR PENDING OD TRAN           *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50CB.    NOTE *INITIALIZE DB SEGMENTS WS          *.
       F50CB.                                                           lv10
      *
           INITIALIZE  CT1S CT1T CT1U
           SET NO-PENDING-OD TO TRUE.
       F50CB-FN. EXIT.
      *N50EA.    NOTE *ONLY IF A NEW OD TRANSACTION       *.
       F50EA.    IF    QT92-CPMTF = 99                                  lv10
                 NEXT SENTENCE ELSE GO TO     F50EA-FN.
      *N50EC.    NOTE *SET DB KEYS                        *.
       F50EC.                                                           lv15
      *
           MOVE        QT92-CLID TO CT1S-NUCLI
           MOVE        '01' TO CT1S-CARTYA
           MOVE        '99' TO CT1S-CPMTFA
           MOVE        QT92-NTR TO CT1T-NAGTB
           MOVE        QT92-CCBAT TO CT1T-CTBAC
           MOVE        QT92-NPBN TO CT1T-NPBN
           MOVE        'AMPF' TO CT1U-CNTXC
           MOVE        QT92-CTID TO CT1U-NNANI.
       F50EC-FN. EXIT.
      *N50EE.    NOTE *CHECK FOR PENDING OD               *.
       F50EE.                                                           lv15
      *
           PERFORM     F94RA THRU F94RA-FN.
       F50EE-FN. EXIT.
       F50EA-FN. EXIT.
       F50-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *DETERMINE IF BA ALLOWED            *
      *               *                                   *
      *               *************************************.
       F60.           EXIT.                                             lv05
      *N60BE.    NOTE *CONVERT CPROCM                     *.
       F60BE.                                                           lv10
                 IF    QT92-CPROCM = 'A'                                DOT
      *ADD
           MOVE        'A' TO WS-CPROCM.
                 IF    QT92-CPROCM = 'M'                                DOT
                 AND   QT92-CDEST = QT12-CDEST
      *MODIFY - NOT STATUS CHANGE
           MOVE        'M' TO WS-CPROCM.
                 IF    QT92-CPROCM = 'M'                                DOT
                 AND   QT92-CDEST = '01'
                 AND   QT92-CDEST NOT = QT12-CDEST
      *MODIFY - REACTIVATE
           MOVE        'R' TO WS-CPROCM.
                 IF    QT92-CPROCM = 'M'                                DOT
                 AND   QT92-CDEST = '03'
                 AND   QT92-CDEST NOT = QT12-CDEST
      *MODIFY - INACTIVATE
           MOVE        'I' TO WS-CPROCM.
       F60BE-FN. EXIT.
      *N60BG.    NOTE *COMPUTE ADDITIONAL AMOUNT          *.
       F60BG.                                                           lv10
      *
                 IF    WS-ADD OR WS-REACTIVATE                          DOT
           COMPUTE     WS-APMTL = QT92-APMTF3.
                 IF    WS-INACTIVATE                                    DOT
           COMPUTE     WS-APMTL = 0
           - QT92-APMTF3.
                 IF    WS-MODIFY                                        DOT
           COMPUTE     WS-APMTL = QT92-APMTF3
           - QT12-APMTL.
       F60BG-FN. EXIT.
      *N60CE.    NOTE *COMPUTE AMT REMAINING TO LIMIT     *.
       F60CE.                                                           lv10
      *
           COMPUTE     WS-AMAXA2 = QT63-AMAXA2
           - WS-APMTLT.
       F60CE-FN. EXIT.
      *N60EC.    NOTE *ADDITION WILL NOT EXCEED MAXIMUM   *.
       F60EC.    IF    WS-APMTL NOT > WS-AMAXA2                         lv10
                 NEXT SENTENCE ELSE GO TO     F60EC-FN.
      *
           MOVE        'Y' TO LK01-IAIND.
       F60EC-900. GO TO F60EE-FN.
       F60EC-FN. EXIT.
      *N60EE.    NOTE *OVER THE MAXIMUM                   *.
       F60EE.                                                           lv10
      *
           MOVE        'N' TO LK01-IAIND.
                 IF    WS-AMAXA2 > 0                                    DOT
           MOVE        WS-AMAXA2 TO LK01-AMAXAN
                 ELSE
           MOVE        0 TO LK01-AMAXAN.
       F60EE-FN. EXIT.
      *N60GC.    NOTE *A PENDING OD EXISTS                *.
       F60GC.    IF    PENDING-OD                                       lv10
                 NEXT SENTENCE ELSE GO TO     F60GC-FN.
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014345 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F60GC-FN. EXIT.
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
      *               *CALLED MODULES                     *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91BA.    NOTE *CALL CI0060 - BANK READ MODULE     *.            AM0060
       F91BA.                                                           lv10
      *                                                                 AM0060
      *********************************                                 AM0060
      ** THIS MODULE WILL READ THE    *                                 AM0060
      ** CLIENT ARRANGEMENT CX03 &    *                                 AM0060
      ** CX18 SEGMENTS FOR THE CLID   *                                 AM0060
      ** AND CARTY PASSED.            *                                 AM0060
      *********************************                                 AM0060
      *                                                                 AM0060
           INITIALIZE  CB60                                             AM0060
           MOVE        QT92-CLID01 TO CB60-CLID01                       AM0060
           MOVE        7-WORK-CARTY TO CB60-CARTYK                      AM0060
           MOVE        7-WORK-CFUNC TO CB60-CFUNC                       AM0060
           MOVE        7-WORK-NARRSK TO CB60-NARRSK                     AM0060
           SET CI0060-PCB-AR1P-PTR1 TO                                  AM0060
                       PCB-AR1P-PTR1                                    AM0060
           INITIALIZE  DE10-DU03                                        AM0060
           CALL        CI0060 USING                                     AM0060
           DFHEIBLK                                                     AM0060
           DFHCOMMAREA                                                  AM0060
           DLIUIBII                                                     AM0060
           CI0060-PCB-ADDRESS-LIST                                      AM0060
           CB60                                                         AM0060
           DE10                                                         AM0060
           MS03.                                                        AM0060
                 IF    MS03-NMESS2 = 12006                              DOT
           INITIALIZE  MS03.
      *N91BC.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F91BC.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F91BC-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        'CI0060' TO MS03-TMESS4 (IMS03R : 6)             ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        'CI0060' TO DE10-TMESS4 (IMS03R : 6)             ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F91BC-900. GO TO F91BD-FN.
       F91BC-FN. EXIT.
      *N91BD.    NOTE *NO ERRORS                          *.            ADU071
       F91BD.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F91BD-FN. EXIT.
       F91BA-FN. EXIT.
      *N91BR.    NOTE *CALL CI0061 - ACCT DETAIL READS    *.            AM0061
       F91BR.                                                           lv10
      *                                                                 AM0061
      *********************************                                 AM0061
      ** THIS MODULE WILL READ THE    *                                 AM0061
      ** CONTRACT ARRANGEMENT CX06    *                                 AM0061
      ** SEGMENT, AND IF REQUESTED,   *                                 AM0061
      ** A SPECIFIC CHILD PATH OF THE *                                 AM0061
      ** CX06 (SUCH AS CX09, CX10,    *                                 AM0061
      ** CX12, ETC.).  THE CX01 AND   *                                 AM0061
      ** CX03 KEYS ARE NEEDED, PLUS   *                                 AM0061
      ** THE KEYS FOR THE PAST SEGS   *                                 AM0061
      ** RETRIEVED.  IF JUST          *                                 AM0061
      ** THE CX06 SEGMENTS ARE WANTED *                                 AM0061
      ** MIPPS SHOULD BE SPACES.      *                                 AM0061
      **   THE SEGMENT NAME FOR THE   *                                 AM0061
      ** PASS AREA IS AR61.             *                               AM0061
      **   THE CONTRACT DB IS READ FOR*                                 AM0061
      ** EACH CX06 THAT HAS THE CX12S *                                 AM0061
      ** BEING RETURNED; FOR IRA MTH. *                                 AM0061
      *********************************                                 AM0061
      *                                                                 AM0061
           INITIALIZE  AR61                                             AM0061
           MOVE        CX01-CLID TO AR61-CLID                           AM0061
           MOVE        CX03-CARTY TO AR61-CARTY                         AM0061
           MOVE        CX03-NARRS TO AR61-NARRS                         AM0061
           MOVE        'CX12' TO AR61-MIPPS                             AM0061
      ***************************                                       AM0061
           SET CI0061-PCB-AR1P-PTR1 TO                                  AM0061
                       PCB-AR1P-PTR1                                    AM0061
           SET CI0061-PCB-CT1P-PTR1 TO                                  AM0061
                       PCB-CT1P-PTR1                                    AM0061
           INITIALIZE  DE10-DU03                                        AM0061
           CALL        CI0061 USING                                     AM0061
           DFHEIBLK                                                     AM0061
           DFHCOMMAREA                                                  AM0061
           DLIUIBII                                                     AM0061
           CI0061-PCB-ADDRESS-LIST                                      AM0061
           AR61                                                         AM0061
           DE10                                                         AM0061
           MS03.                                                        AM0061
      *N91BS.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F91BS.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F91BS-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        'CI0061' TO MS03-TMESS4 (IMS03R : 6)             ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        'CI0061' TO DE10-TMESS4 (IMS03R : 6)             ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F91BS-900. GO TO F91BT-FN.
       F91BS-FN. EXIT.
      *N91BT.    NOTE *NO ERRORS                          *.            ADU071
       F91BT.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F91BT-FN. EXIT.
       F91BR-FN. EXIT.
      *N91DG.    NOTE *GREG DATE CONVERSION               *.
       F91DG.                                                           lv10
      *REFORMAT CCYY-MM-DD MINUS DASHES
           MOVE        DT01-DCACD TO
           7-DTOT-DATE
           MOVE        7-DTOT-XDATC TO 7-DTIN-XDATC
           MOVE        7-DTOT-XDATY TO 7-DTIN-XDATY
           MOVE        7-DTOT-XDATM TO 7-DTIN-XDATM
           MOVE        7-DTOT-XDATD TO 7-DTIN-XDATD
           MOVE        7-DTIN-DATE TO
           DT01-DCACG.
       F91DG-FN. EXIT.
      *N91DT.    NOTE *DB2 DATE CONVERSION                *.
       F91DT.                                                           lv10
      *REFORMAT CCYYMMDD USING DASHES                                   ADB2DT
           MOVE        DT01-DCACG TO                                    ADB2DT
           7-DTIN-DATE                                                  ADB2DT
           MOVE        7-DTIN-XDATC TO 7-DTOT-XDATC                     ADB2DT
           MOVE        7-DTIN-XDATY TO 7-DTOT-XDATY                     ADB2DT
           MOVE        7-DTIN-XDATM TO 7-DTOT-XDATM                     ADB2DT
           MOVE        7-DTIN-XDATD TO 7-DTOT-XDATD                     ADB2DT
           MOVE        7-DTOT-DATE TO                                   ADB2DT
           DT01-DCACD.                                                  ADB2DT
       F91DT-FN. EXIT.
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
      *               *DATABASE CALLS                     *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94CX.    NOTE *CALL GU ON CX01                    *.            ADU026
       F94CX.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XA06 CX01                                                    ADU026
           S-CXU01-SSA                                                  ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CX-FN. EXIT.
      *N94RA.    NOTE *CHECK FOR PENDING OD TRANSACTION   *.
       F94RA.                                                           lv10
           MOVE        'F94RA - SELECT' TO 7-DB2-FUNCT                  ADB226
           EXEC SQL    SELECT                                           ADB226
                           IBMREQD
                       INTO
                          :WS-IAIND:WS-XNULL
                       FROM
                           SYSIBM.SYSDUMMY1
                       WHERE EXISTS (
                          SELECT *
                          FROM
                              CORP.TBCT1S A,
                              CORP.TBCT1T B,
                              CORP.TBCT1U C
                          WHERE
                              A.NUCLI  = :CT1S-NUCLI
                          AND A.CARTYA = :CT1S-CARTYA
                          AND A.CPMTFA = :CT1S-CPMTFA
                          AND B.DXTMS  = A.DXTMS
                          AND B.NAGTB  = :CT1T-NAGTB
                          AND B.NPBN   = :CT1T-NPBN
                          AND B.CTBAC  = :CT1T-CTBAC
                          AND C.DXTMS  = B.DXTMS
                          AND C.DXTMS  = A.DXTMS
                          AND C.CNTXC  = :CT1U-CNTXC
                          AND C.NNANI  = :CT1U-NNANI
                                    )                        END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB226
       F94RA-FN. EXIT.
      *N94RD.    NOTE *DECLARE CURSOR                     *.
       F94RD.                                                           lv10
           EXEC SQL    DECLARE PEND-CURSOR CURSOR FOR                   ADB201
                       SELECT                                           ADB201
                           A.APMTO
                          ,A.CASTA
                          ,A.CPMTFA
                          ,A.DATSD
                          ,A.DTEND
                          ,A.DACPR
                          ,B.CPROCM
                       FROM                                             ADB201
                           CORP.TBCT1S A
                          ,CORP.TBCT1T B
                       WHERE                                            ADB201
                           A.NUCLI  = :CT1S-NUCLI
                       AND A.CARTYA = :CT1S-CARTYA
                       AND B.DXTMS  = A.DXTMS
                       AND B.NAGTB  = :CT1T-NAGTB
                       AND B.CTBAC  = :CT1T-CTBAC
                       AND B.NPBN   = :CT1T-NPBN             END-EXEC.
       F94RD-FN. EXIT.
      *N94RF.    NOTE *FETCH FROM CURSOR                  *.
       F94RF.                                                           lv10
           MOVE        'F94RF - FETCH' TO 7-DB2-FUNCT                   ADB224
           EXEC SQL    FETCH PEND-CURSOR INTO                           ADB224
                            :CT1S-APMTO
                           ,:CT1S-CASTA
                           ,:CT1S-CPMTFA
                           ,:CT1S-DATSD
                           ,:CT1S-DTEND
                           ,:CT1S-DACPR
                           ,:CT1T-CPROCM                     END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB224
       F94RF-FN. EXIT.
      *N94RO.    NOTE *OPEN CURSOR                        *.
       F94RO.                                                           lv10
           MOVE        'F94RO - OPEN' TO 7-DB2-FUNCT                    ADB223
           EXEC SQL    OPEN PEND-CURSOR                      END-EXEC.  ADB223
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB223
       F94RO-FN. EXIT.
      *N94RX.    NOTE *CLOSE CURSOR                       *.
       F94RX.                                                           lv10
           MOVE        'F94RX - CLOSE' TO 7-DB2-FUNCT                   ADB225
           EXEC SQL    CLOSE PEND-CURSOR                     END-EXEC.  ADB225
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB225
       F94RX-FN. EXIT.
       F94-FN.   EXIT.
      *N96.      NOTE *************************************.
      *               *                                   *
      *               *NEXT PMT DATE CALCULATIONS         *
      *               *                                   *
      *               *************************************.
       F96.           EXIT.                                             lv05
      *N96CA.    NOTE *SEE IF PAYMENTS MATCH UP           *.
       F96CA.         EXIT.                                             lv10
      *N96EC.    NOTE *LOOP THRU PAYMENT DATES TO SEE     *.
       F96EC.                                                           lv15
      *IF THEY MATCH UP ANY TIME
      *
           SET NO-MATCH TO TRUE
           SET NOT-DONE TO TRUE
      *
      *CALCULATE 1 YEAR AHEAD
           COMPUTE     WS-GEEND = WS-1-DNPMT + 10000.
                 IF    WS-1-DNPMT = WS-2-DNPMT                          DOT
           SET YES-MATCH TO TRUE.
                 IF    WS-2-DNPMT > WS-GEEND                            DOT
           SET YES-DONE  TO TRUE.
                 IF    WS-2-GEEND > 0                                   DOT
                 AND   WS-2-GEEND < WS-1-GESTD
           SET YES-DONE  TO TRUE.
                 IF    WS-1-GEEND > 0                                   DOT
                 AND   WS-1-GEEND > WS-2-GESTD
           SET YES-DONE  TO TRUE.
      *N96EE.    NOTE *LOOP UNTIL THEY MATCH              *.
       F96EE.    IF    NO-MATCH                                         lv20
                 AND   NOT-DONE
                 NEXT SENTENCE ELSE GO TO     F96EE-FN.
      *OR END DATE REACHED
      *OR MORE THAN 1 YEAR IN FUTURE
      *N96EG.    NOTE *INCREMENT ARR NEXT PAYMENT DATE    *.
       F96EG.    IF    WS-1-DNPMT < WS-2-DNPMT                          lv25
                 NEXT SENTENCE ELSE GO TO     F96EG-FN.
      *
           MOVE        WS-1-DNPMT TO 7-WS-DNPMT
           MOVE        WS-1-CPMTF TO 7-WS-CPMTF
           MOVE        WS-1-CIRMO TO 7-WS-CIRMO
           PERFORM     F96RA THRU F96RA-FN
           MOVE        7-WS-DNPMT TO WS-1-DNPMT.
                 IF    (WS-1-GEEND > 0                                  DOT
                 AND   WS-1-GEEND < WS-1-DNPMT)
                 OR    WS-1-DNPMT > WS-GEEND
      *PAST THE END DATE
      *OR
      *PAST 1 YEAR
           SET YES-DONE TO TRUE.
       F96EG-FN. EXIT.
      *N96EI.    NOTE *INCREMENT NEW NEXT PAYMENT DATE    *.
       F96EI.    IF    WS-2-DNPMT < WS-1-DNPMT                          lv25
                 NEXT SENTENCE ELSE GO TO     F96EI-FN.
      *
           MOVE        WS-2-DNPMT TO 7-WS-DNPMT
           MOVE        WS-2-CPMTF TO 7-WS-CPMTF
           MOVE        WS-2-CIRMO TO 7-WS-CIRMO
           PERFORM     F96RA THRU F96RA-FN
           MOVE        7-WS-DNPMT TO WS-2-DNPMT.
                 IF    (WS-2-GEEND > 0                                  DOT
                 AND   WS-2-GEEND < WS-2-DNPMT)
                 OR    WS-2-DNPMT > WS-GEEND
      *PAST THE END DATE
      *OR
      *PAST 1 YEAR
           SET YES-DONE TO TRUE.
       F96EI-FN. EXIT.
      *N96EK.    NOTE *DATES MATCH                        *.
       F96EK.    IF    WS-2-DNPMT = WS-1-DNPMT                          lv25
                 NEXT SENTENCE ELSE GO TO     F96EK-FN.
      *
           SET YES-MATCH TO TRUE.
       F96EK-FN. EXIT.
      *N96EM.    NOTE *ON-DEMAND AND EARLIER - DONE       *.
       F96EM.    IF    WS-1-CPMTF = 99                                  lv25
                 AND   WS-1-DNPMT < WS-2-DNPMT
                 NEXT SENTENCE ELSE GO TO     F96EM-FN.
           SET YES-DONE TO TRUE.
       F96EM-FN. EXIT.
      *N96EN.    NOTE *ON-DEMAND AND EARLIER - DONE       *.
       F96EN.    IF    WS-2-CPMTF = 99                                  lv25
                 AND   WS-2-DNPMT < WS-1-DNPMT
                 NEXT SENTENCE ELSE GO TO     F96EN-FN.
           SET YES-DONE TO TRUE.
       F96EN-FN. EXIT.
       F96EE-900. GO TO F96EE.
       F96EE-FN. EXIT.
      *N96EP.    NOTE *HAVE A MATCH                       *.
       F96EP.    IF    YES-MATCH                                        lv20
                 NEXT SENTENCE ELSE GO TO     F96EP-FN.
      *
           ADD         WS-2-APMTL TO WS-APMTLT.
       F96EP-FN. EXIT.
       F96EC-FN. EXIT.
       F96CA-FN. EXIT.
      *N96RA.    NOTE *CALCULATE NEXT PMT DATE            *.            $DNPMT
       F96RA.                                                           lv10
      *MOVE IN PMT DATE                                                 $DNPMT
           MOVE        7-WS-DNPMT TO 7-RA-DATE-TEMP                     $DNPMT
      *MOVE IN IRREG MONTHS                                             $DNPMT
           MOVE        7-WS-CIRMO TO 7-RA-7-WS-CIRMO-IN                 $DNPMT
      *INIT ERROR FLAG                                                  $DNPMT
           MOVE        'Y' TO 7-RA-ERR-FLAG                             $DNPMT
      *SET UP DAYS IN MONTH                                             $DNPMT
           MOVE        '312831303130313130313031' TO                    $DNPMT
           7-RA-MONTH-LOAD                                              $DNPMT
      *CALC LEAP YEAR                                                   $DNPMT
           PERFORM     F96RV THRU F96RV-FN.                             $DNPMT
      *N96RB.    NOTE *CHECK FOR INPUT ERRORS             *.            $DNPMT
       F96RB.    IF    7-RA-DA > 31                                     lv15
                 OR    7-RA-7-WS-CIRMO-IN = SPACES                      $DNPMT
                 OR    (7-WS-CPMTF < 90                                 $DNPMT
                 AND   7-RA-7-WS-CIRMO-IN NOT =                         $DNPMT
                       'XXXXXXXXXXXX')                                  $DNPMT
                 NEXT SENTENCE ELSE GO TO     F96RB-FN.                 $DNPMT
           MOVE        'Y' TO 7-RA-ERR-FLAG                             $DNPMT
               GO TO     F96RA-FN.                                      $DNPMT
       F96RB-FN. EXIT.
      *N96RC.    NOTE *FOR 01,02,03,04,06,91              *.            $DNPMT
       F96RC.    IF    7-WS-CPMTF = 01 OR 02                            lv15
                 OR    03 OR 04 OR 06 OR 12 OR 91                       $DNPMT
                 NEXT SENTENCE ELSE GO TO     F96RC-FN.                 $DNPMT
           DIVIDE 12 BY 7-WS-CPMTF                                      $DNPMT
           GIVING 7-RA-NUMB-OF-MOS                                      $DNPMT
           MOVE        'Y' TO 7-RA-LOOP-END                             $DNPMT
           MOVE        'N' TO 7-RA-ERR-FLAG.                            $DNPMT
                 IF    7-WS-CPMTF = 91                                  DOT
           MOVE        1 TO 7-RA-NUMB-OF-MOS                            $DNPMT
           MOVE        'N' TO 7-RA-LOOP-END.                            $DNPMT
      *N96RE.    NOTE *LOOP IF IRREG                      *.            $DNPMT
       F96RE.                       GO TO     F96RE-B.                  lv20
       F96RE-A.
                 IF    7-RA-LOOP-END = 'Y'                              $DNPMT
                                    GO TO     F96RE-FN.                 $DNPMT
       F96RE-B.
           ADD         7-RA-NUMB-OF-MOS TO 7-RA-MO.                     $DNPMT
                 IF    7-RA-MO > 12                                     DOT
           ADD         1 TO 7-RA-YR                                     $DNPMT
           SUBTRACT    12 FROM 7-RA-MO                                  $DNPMT
      *CHECK FOR LEAP YEAR                                              $DNPMT
           PERFORM     F96RV THRU F96RV-FN.                             $DNPMT
                 IF    7-RA-CIRMO (7-RA-MO) = 'X'                       DOT
           MOVE        'Y' TO 7-RA-LOOP-END.                            $DNPMT
       F96RE-900. GO TO F96RE-A.
       F96RE-FN. EXIT.
       F96RC-FN. EXIT.
      *N96RH.    NOTE *FOR 13, 26, 52, 92, 94, 95         *.            $DNPMT
       F96RH.    IF    7-WS-CPMTF = 13 OR 92                            lv15
                 OR    26 OR 94 OR 52 OR 95                             $DNPMT
                 NEXT SENTENCE ELSE GO TO     F96RH-FN.                 $DNPMT
           MOVE        'N' TO 7-RA-ERR-FLAG.                            $DNPMT
       F96RI.                                                           lv20
                 IF    7-WS-CPMTF = 13 OR 92                            DOT
           MOVE        28 TO 7-RA-NUMB-OF-DAYS.                         $DNPMT
                 IF    7-WS-CPMTF = 26 OR 94                            DOT
           MOVE        14 TO 7-RA-NUMB-OF-DAYS.                         $DNPMT
                 IF    7-WS-CPMTF = 52 OR 95                            DOT
           MOVE        7 TO 7-RA-NUMB-OF-DAYS.                          $DNPMT
                 IF    7-WS-CPMTF = 92 OR 94 OR 95                      DOT
           MOVE        'N' TO 7-RA-LOOP-END                             $DNPMT
                 ELSE                                                   $DNPMT
           MOVE        'Y' TO 7-RA-LOOP-END.                            $DNPMT
       F96RI-FN. EXIT.
      *N96RJ.    NOTE *LOOP FOR IRREGS                    *.            $DNPMT
       F96RJ.                       GO TO     F96RJ-B.                  lv20
       F96RJ-A.
                 IF    7-RA-LOOP-END = 'Y'                              $DNPMT
                                    GO TO     F96RJ-FN.                 $DNPMT
       F96RJ-B.
           ADD         7-RA-NUMB-OF-DAYS TO 7-RA-DA.                    $DNPMT
       F96RL.    IF    7-RA-DA >                                        lv25
                       7-RA-MONTH (7-RA-MO)                             $DNPMT
                 NEXT SENTENCE ELSE GO TO     F96RL-FN.                 $DNPMT
           SUBTRACT    7-RA-MONTH (7-RA-MO) FROM 7-RA-DA                $DNPMT
           ADD         1 TO 7-RA-MO.                                    $DNPMT
                 IF    7-RA-MO > 12                                     DOT
           ADD         1 TO 7-RA-YR                                     $DNPMT
           SUBTRACT    12 FROM 7-RA-MO                                  $DNPMT
      *RE-CHECK FOR LEAP YEAR                                           $DNPMT
           PERFORM     F96RV THRU F96RV-FN.                             $DNPMT
       F96RL-FN. EXIT.
       F96RM.    IF    7-RA-CIRMO (7-RA-MO) = 'X'                       lv25
                 NEXT SENTENCE ELSE GO TO     F96RM-FN.                 $DNPMT
           MOVE        'Y' TO 7-RA-LOOP-END.                            $DNPMT
       F96RM-FN. EXIT.
       F96RJ-900. GO TO F96RJ-A.
       F96RJ-FN. EXIT.
       F96RH-FN. EXIT.
      *N96RP.    NOTE *FOR 24,93                          *.            $DNPMT
       F96RP.    IF    7-WS-CPMTF = 24 OR 93                            lv15
                 NEXT SENTENCE ELSE GO TO     F96RP-FN.                 $DNPMT
           MOVE        'N' TO 7-RA-ERR-FLAG.                            $DNPMT
                 IF    7-WS-CPMTF = 93                                  DOT
           MOVE        'N' TO 7-RA-LOOP-END                             $DNPMT
                 ELSE                                                   $DNPMT
           MOVE        'Y' TO 7-RA-LOOP-END.                            $DNPMT
      *N96RQ.    NOTE *LOOP FOR IRREG                     *.            $DNPMT
       F96RQ.                       GO TO     F96RQ-B.                  lv20
       F96RQ-A.
                 IF    7-RA-LOOP-END = 'Y'                              $DNPMT
                                    GO TO     F96RQ-FN.                 $DNPMT
       F96RQ-B.       EXIT.
       F96RR.    IF    7-RA-DA > 15                                     lv25
                 NEXT SENTENCE ELSE GO TO     F96RR-FN.                 $DNPMT
           SUBTRACT    15 FROM 7-RA-DA                                  $DNPMT
           ADD         1 TO 7-RA-MO.                                    $DNPMT
                 IF    7-RA-MO > 12                                     DOT
           MOVE        1 TO 7-RA-MO                                     $DNPMT
           ADD         1 TO 7-RA-YR.                                    $DNPMT
       F96RR-900. GO TO F96RS-FN.
       F96RR-FN. EXIT.
       F96RS.                                                           lv25
           ADD         15 TO 7-RA-DA.                                   $DNPMT
       F96RS-FN. EXIT.
       F96RT.    IF    7-RA-CIRMO (7-RA-MO) = 'X'                       lv25
                 NEXT SENTENCE ELSE GO TO     F96RT-FN.                 $DNPMT
           MOVE        'Y' TO 7-RA-LOOP-END.                            $DNPMT
       F96RT-FN. EXIT.
       F96RQ-900. GO TO F96RQ-A.
       F96RQ-FN. EXIT.
       F96RP-FN. EXIT.
       F96RU.                                                           lv15
           MOVE        7-RA-DATE-TEMP TO 7-WS-DNPMT                     $DNPMT
               GO TO     F96RA-FN.                                      $DNPMT
       F96RU-FN. EXIT.
      *N96RV.    NOTE *LEAP YEAR CALC                     *.            $DNPMT
       F96RV.                                                           lv15
           DIVIDE 7-RA-YR BY 4                                          $DNPMT
           GIVING 7-RA-T0.                                              $DNPMT
                 IF    7-RA-T1-DEC > 0                                  DOT
           MOVE        28 TO 7-RA-MONTH (2)                             $DNPMT
               GO TO     F96RV-FN.                                      $DNPMT
                 IF    7-RA-YR-X-YY > 0                                 DOT
           MOVE        29 TO 7-RA-MONTH (2)                             $DNPMT
               GO TO     F96RV-FN.                                      $DNPMT
           DIVIDE 7-RA-YR-X-CC BY 4                                     $DNPMT
           GIVING 7-RA-T0.                                              $DNPMT
                 IF    7-RA-T1-DEC > 0                                  DOT
           MOVE        28 TO 7-RA-MONTH (2)                             $DNPMT
                 ELSE                                                   $DNPMT
           MOVE        29 TO 7-RA-MONTH (2).                            $DNPMT
       F96RV-FN. EXIT.
       F96RA-FN. EXIT.
       F96-FN.   EXIT.
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
