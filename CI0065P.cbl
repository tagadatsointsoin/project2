       IDENTIFICATION DIVISION.                                         CI0065
       PROGRAM-ID.  CI0065P.                                            CI0065
      *AUTHOR.         SPO - GETTING VALID VALUES.                      CI0065
      *DATE-COMPILED.   09/08/14.                                       CI0065
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2007                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE FDC    SYSTEM AND ALL INFORMATION RELATING THERETO,    *ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE FDC    SYSTEM AND ALL            *ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE FDC          *ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 2007                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0065
       CONFIGURATION SECTION.                                           CI0065
       SOURCE-COMPUTER. IBM-370.                                        CI0065
       OBJECT-COMPUTER. IBM-370.                                        CI0065
       DATA DIVISION.                                                   CI0065
       WORKING-STORAGE SECTION.                                         CI0065
       01                 AA10.                                         CI0065
            10            AA10-AE00.                                    CI0065
            11            AA10-ALCIDN PICTURE  9(11).                   CI0065
            10            AA10-AE01.                                    CI0065
            11            AA10-FILLER PICTURE  X(12).                   CI0065
            11            AA10-DLAUP  PICTURE  9(8).                    CI0065
            11            AA10-FILLER PICTURE  S9(07)                   CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-FILLER PICTURE  9(8).                    CI0065
            11            AA10-FILLER PICTURE  S9(07)                   CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-FILLER PICTURE  9(8).                    CI0065
            11            AA10-FILLER PICTURE  S9(07)                   CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-FILLER PICTURE  X(311).                  CI0065
            10            AA10-AE02                                     CI0065
                          REDEFINES            AA10-AE01.               CI0065
            11            AA10-FILLER PICTURE  9.                       CI0065
            11            AA10-ALCOMP PICTURE  99.                      CI0065
            11            AA10-CRTYP  PICTURE  9(4).                    CI0065
            11            AA10-FILLER PICTURE  9.                       CI0065
            11            AA10-CPOST  PICTURE  99.                      CI0065
            11            AA10-GEHCDI PICTURE  9(3).                    CI0065
            11            AA10-FILLER PICTURE  9(8).                    CI0065
            11            AA10-FILLER PICTURE  9(7).                    CI0065
            11            AA10-FILLER PICTURE  X.                       CI0065
            11            AA10-ALPLDT PICTURE  9(8).                    CI0065
            11            AA10-DENEX  PICTURE  9(8).                    CI0065
            11            AA10-CENXC1 PICTURE  9(3).                    CI0065
            11            AA10-FILLER PICTURE  99.                      CI0065
            11            AA10-CROOR  PICTURE  99.                      CI0065
            11            AA10-CREIN  PICTURE  99.                      CI0065
            11            AA10-FILLER PICTURE  X.                       CI0065
            11            AA10-ALAPST PICTURE  99.                      CI0065
            11            AA10-ALSTSA PICTURE  XX.                      CI0065
            11            AA10-FILLER PICTURE  99.                      CI0065
            11            AA10-CPCAL  PICTURE  9.                       CI0065
            11            AA10-CNAEX  PICTURE  9.                       CI0065
            11            AA10-CSUSI  PICTURE  99.                      CI0065
            11            AA10-FILLER PICTURE  99.                      CI0065
            11            AA10-FILLER PICTURE  99.                      CI0065
            11            AA10-FILLER PICTURE  9(8).                    CI0065
            11            AA10-FILLER PICTURE  9.                       CI0065
            11            AA10-FILLER PICTURE  X(10).                   CI0065
            11            AA10-FILLER PICTURE  99.                      CI0065
            11            AA10-FILLER PICTURE  X.                       CI0065
            11            AA10-FILLER PICTURE  99.                      CI0065
            11            AA10-CAPL   PICTURE  9.                       CI0065
            11            AA10-FILLER PICTURE  9.                       CI0065
            11            AA10-FILLER PICTURE  999.                     CI0065
            11            AA10-FILLER PICTURE  999.                     CI0065
            11            AA10-FILLER PICTURE  999.                     CI0065
            11            AA10-CSTWH  PICTURE  9(8).                    CI0065
            11            AA10-FILLER PICTURE  9(8).                    CI0065
            11            AA10-FILLER PICTURE  99.                      CI0065
            11            AA10-FILLER PICTURE  9.                       CI0065
            11            AA10-FILLER PICTURE  9.                       CI0065
            11            AA10-FILLER PICTURE  99.                      CI0065
            11            AA10-FILLER PICTURE  999.                     CI0065
            11            AA10-FILLER PICTURE  999.                     CI0065
            11            AA10-CPRPM  PICTURE  9(3).                    CI0065
            11            AA10-FILLER PICTURE  99.                      CI0065
            11            AA10-FILLER PICTURE  9(6).                    CI0065
            11            AA10-FILLER PICTURE  99.                      CI0065
            11            AA10-FILLER PICTURE  999.                     CI0065
            11            AA10-DTRCM  PICTURE  9(8).                    CI0065
            11            AA10-DLATR  PICTURE  9(8).                    CI0065
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-ALPMOD PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-CRSBN  PICTURE  99.                      CI0065
            11            AA10-FILLER PICTURE  X.                       CI0065
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-FILLER PICTURE  9.                       CI0065
            11            AA10-FILLER PICTURE  X.                       CI0065
            11            AA10-FILLER PICTURE  XX.                      CI0065
            11            AA10-FILLER PICTURE  99.                      CI0065
            11            AA10-CTRHO  PICTURE  9(8).                    CI0065
            11            AA10-FILLER PICTURE  9.                       CI0065
            11            AA10-FILLER PICTURE  99.                      CI0065
            11            AA10-FILLER PICTURE  99.                      CI0065
            11            AA10-CTSGD  PICTURE  9(8).                    CI0065
            11            AA10-IANRD  PICTURE  9.                       CI0065
            11            AA10-ALINNO PICTURE  99.                      CI0065
            11            AA10-ALSANN PICTURE  9(5).                    CI0065
            11            AA10-FILLER PICTURE  S9(9)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-ALPAGR PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-ALRISK PICTURE  S9(9)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-ALAPIT PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-FILLER PICTURE  X.                       CI0065
            11            AA10-CADPR  PICTURE  9.                       CI0065
            11            AA10-AAPRT  PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-NREPN1 PICTURE  9(06).                   CI0065
            11            AA10-CCST1  PICTURE  9.                       CI0065
            11            AA10-CESRD  PICTURE  9(3).                    CI0065
            11            AA10-ALLRT  PICTURE  S9V99                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-ANTPAA PICTURE  S9(5)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-ALDDUE PICTURE  9(08).                   CI0065
            11            AA10-ALMODE PICTURE  99.                      CI0065
            11            AA10-FILLER PICTURE  X(08).                   CI0065
            11            AA10-CTCUS1 PICTURE  99.                      CI0065
            11            AA10-CNPPR  PICTURE  9(03).                   CI0065
            11            AA10-FILLER PICTURE  9.                       CI0065
            11            AA10-FILLER PICTURE  9(03).                   CI0065
            11            AA10-ITMEC  PICTURE  X(1).                    CI0065
            11            AA10-IMCDI  PICTURE  X.                       CI0065
            11            AA10-LSIDTE PICTURE  9(08).                   CI0065
            11            AA10-PLINE  PICTURE  S9V99                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-ATSA8  PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-ATSA9  PICTURE  S9(05)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-CRATS  PICTURE  X.                       CI0065
            11            AA10-PPTKN  PICTURE  S9(3)V9(6)               CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            AA10-FILLER PICTURE  X(24).                   CI0065
       01                 AA85.                                         CI0065
            10            AA85-DUWAC  PICTURE  9(8).                    CI0065
            10            AA85-CUWAC  PICTURE  99.                      CI0065
            10            AA85-CRFAC  PICTURE  9(6).                    CI0065
            10            AA85-IIAAF  PICTURE  9.                       CI0065
            10            AA85-DPOLI  PICTURE  9(8).                    CI0065
            10            AA85-DCONM  PICTURE  9(8).                    CI0065
            10            AA85-DBYR   PICTURE  99.                      CI0065
            10            AA85-DLMED  PICTURE  9(8).                    CI0065
            10            AA85-LSIDTE PICTURE  9(08).                   CI0065
            10            AA85-GESTD  PICTURE  9(8).                    CI0065
            10            AA85-CRBTR  PICTURE  9(6).                    CI0065
            10            AA85-CBTR   PICTURE  X.                       CI0065
            10            AA85-PSBTR  PICTURE  S9(3)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            AA85-ANTPF  PICTURE  S9(3)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            AA85-QDYT1  PICTURE  99.                      CI0065
            10            AA85-AEPT1  PICTURE  S9(3)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            AA85-QDYT2  PICTURE  99.                      CI0065
            10            AA85-AEPT2  PICTURE  S9(3)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            AA85-CRFER  PICTURE  9(6).                    CI0065
            10            AA85-PRADB  PICTURE  S99V9                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            AA85-PRWP   PICTURE  S99V9                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            AA85-PRNOM  PICTURE  S99V9                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            AA85-CDETH  PICTURE  X.                       CI0065
            10            AA85-CRFSP  PICTURE  X(6).                    CI0065
            10            AA85-CREDI  PICTURE  X(16).                   CI0065
            10            AA85-CRBD   PICTURE  X(6).                    CI0065
            10            AA85-CFRTR  PICTURE  X.                       CI0065
            10            AA85-ANTPG  PICTURE  S9(3)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            AA85-ATEPF  PICTURE  S9(3)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            AA85-CRFRT  PICTURE  X(6).                    CI0065
            10            AA85-QDYT3  PICTURE  99.                      CI0065
            10            AA85-CRFEX  PICTURE  9(6).                    CI0065
            10            AA85-AAPEB  PICTURE  S9(3)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            AA85-AAPER  PICTURE  S9(3)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            AA85-ARGAP  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            AA85-CWAE   PICTURE  XX.                      CI0065
            10            AA85-IMIBP  PICTURE  X.                       CI0065
            10            AA85-CNIR   PICTURE  XX.                      CI0065
            10            AA85-CMINM  PICTURE  XX.                      CI0065
      ******************************************************************
      ** ACCOUNT AND ARRANGEMENT SWITCHES AND COUNTERS                 *
      ******************************************************************
        01  ACCT-MONTHLY            PIC S9(7)V99 COMP-3.
        01  ACCT-QUARTERLY          PIC S9(7)V99 COMP-3.
        01  ACCT-SEMIANNUAL         PIC S9(7)V99 COMP-3.
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0020           PIC X(8) VALUE 'CI0020P '.                  AM0020
       01  CI0140           PIC X(8) VALUE 'CI0140P '.                  AM0140
       01  CI0141           PIC X(8) VALUE 'CI0141P '.                  AM0141
       01                 CT01.                                         CI0065
            10            CT01-CT01K.                                   CI0065
            11            CT01-C299.                                    CI0065
            12            CT01-CTID.                                    CI0065
            13            CT01-CTIDA  PICTURE  9(3).                    CI0065
            13            CT01-CTIDN.                                   CI0065
            14            CT01-CTIDNP PICTURE  X(13).                   CI0065
            14            CT01-CTIDND PICTURE  9(11).                   CI0065
            10            CT01-GECKD  PICTURE  9.                       CI0065
            10            CT01-GEMDA  PICTURE  9(8).                    CI0065
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0065
                          BINARY.                                       CI0065
            10            CT01-GECUC  PICTURE  99.                      CI0065
            10            CT01-CTAUL  PICTURE  9(3).                    CI0065
            10            CT01-DIRAC  PICTURE  9(4).                    CI0065
            10            CT01-CTCCI  PICTURE  X.                       CI0065
            10            CT01-CTCUS  PICTURE  999.                     CI0065
            10            CT01-CTEFD  PICTURE  9(8).                    CI0065
            10            CT01-CTIAD  PICTURE  9(8).                    CI0065
            10            CT01-CLCUS  PICTURE  99.                      CI0065
            10            CT01-CAMMB  PICTURE  X(3).                    CI0065
            10            CT01-CKPMM  PICTURE  X.                       CI0065
            10            CT01-CTLAD  PICTURE  9(8).                    CI0065
            10            CT01-IPERS  PICTURE  X.                       CI0065
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            CT01-CTLAT  PICTURE  9(8).                    CI0065
            10            CT01-CTLATC PICTURE  9(6).                    CI0065
            10            CT01-IMEGA  PICTURE  X.                       CI0065
            10            CT01-DIRAB  PICTURE  9(8).                    CI0065
            10            CT01-COLRQ  PICTURE  X.                       CI0065
            10            CT01-ZDA04  PICTURE  X(4).                    CI0065
            10            CT01-CTLPD  PICTURE  9(8).                    CI0065
            10            CT01-CIRASP PICTURE  9.                       CI0065
            10            CT01-CIRATP PICTURE  99.                      CI0065
            10            CT01-DRTHC  PICTURE  9(8).                    CI0065
            10            CT01-CPPTC  PICTURE  X.                       CI0065
            10            CT01-ZDA06  PICTURE  X(6).                    CI0065
            10            CT01-CTACD  PICTURE  9(8).                    CI0065
            10            CT01-CTNLI  PICTURE  X.                       CI0065
            10            CT01-CTRHO  PICTURE  9(8).                    CI0065
            10            CT01-CTSGD  PICTURE  9(8).                    CI0065
            10            CT01-CPATP  PICTURE  X(1).                    CI0065
            10            CT01-IRSTA  PICTURE  X.                       CI0065
            10            CT01-CTSTA  PICTURE  99.                      CI0065
            10            CT01-CTSSC  PICTURE  99.                      CI0065
            10            CT01-PRLIN  PICTURE  9(3).                    CI0065
            10            CT01-PRCOD  PICTURE  9(5).                    CI0065
            10            CT01-PRSCD  PICTURE  X(9).                    CI0065
            10            CT01-CTLNI  PICTURE  X.                       CI0065
            10            CT01-AYSIDA PICTURE  9(3).                    CI0065
            10            CT01-AYSID  PICTURE  9(5).                    CI0065
            10            CT01-CTBMC  PICTURE  99.                      CI0065
            10            CT01-CINAR  PICTURE  99.                      CI0065
            10            CT01-CPHTR  PICTURE  X.                       CI0065
            10            CT01-CDSTR  PICTURE  XX.                      CI0065
            10            CT01-CQACT  PICTURE  999.                     CI0065
            10            CT01-CIRAS  PICTURE  999.                     CI0065
            10            CT01-CIRAT  PICTURE  999.                     CI0065
            10            CT01-CLRAY  PICTURE  9(5).                    CI0065
            10            CT01-CATTP  PICTURE  X.                       CI0065
       01                 CT22.                                         CI0065
            10            CT22-CT22K.                                   CI0065
            11            CT22-CGVEN  PICTURE  X(2).                    CI0065
            11            CT22-CTWHC  PICTURE  9(2).                    CI0065
            10            CT22-CFCNTY PICTURE  X(2).                    CI0065
            10            CT22-DLAUP  PICTURE  9(8).                    CI0065
            10            CT22-CTWTC  PICTURE  9(2).                    CI0065
            10            CT22-CTWHAT PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            CT22-CTWHP  PICTURE  9(3)V99                  CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            CT22-FILLER PICTURE  X(06).                   CI0065
       01                 CX01.                                         CI0065
            10            CX01-CX01K.                                   CI0065
            11            CX01-C199.                                    CI0065
            12            CX01-CLID.                                    CI0065
            13            CX01-CLIDO  PICTURE  9(3).                    CI0065
            13            CX01-CLIDN.                                   CI0065
            14            CX01-CLIDNP PICTURE  X(12).                   CI0065
            14            CX01-CLIDND PICTURE  9(8).                    CI0065
            10            CX01-GEMDA  PICTURE  9(8).                    CI0065
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0065
                          BINARY.                                       CI0065
            10            CX01-FILLER PICTURE  X(5).                    CI0065
       01                 CX03.                                         CI0065
            10            CX03-GELL   PICTURE  9(4)                     CI0065
                          BINARY.                                       CI0065
            10            CX03-CY00.                                    CI0065
            11            CX03-CX03K.                                   CI0065
            12            CX03-CARTY  PICTURE  99.                      CI0065
            12            CX03-NARRS  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX03-CARST  PICTURE  99.                      CI0065
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX03-CPMTG  PICTURE  99.                      CI0065
            11            CX03-GRCRNG PICTURE  9(3).                    CI0065
            11            CX03-DEXDT  PICTURE  9(8).                    CI0065
            11            CX03-DASUP  PICTURE  9(8).                    CI0065
            11            CX03-CSTEC  PICTURE  X(3).                    CI0065
            11            CX03-FILLER PICTURE  X(17).                   CI0065
            11            CX03-CY50.                                    CI0065
            12            CX03-NARID  PICTURE  X(30).                   CI0065
            11            CX03-CY51                                     CI0065
                          REDEFINES            CX03-CY50.               CI0065
            12            CX03-NDIDN  PICTURE  9(12).                   CI0065
            12            CX03-FILLER PICTURE  X(18).                   CI0065
            11            CX03-CY52                                     CI0065
                          REDEFINES            CX03-CY50.               CI0065
            12            CX03-NAIDC  PICTURE  9(12).                   CI0065
            12            CX03-FILLER PICTURE  X(18).                   CI0065
            11            CX03-CY53                                     CI0065
                          REDEFINES            CX03-CY50.               CI0065
            12            CX03-NAMEXB PICTURE  9(15).                   CI0065
            12            CX03-FILLER PICTURE  X(15).                   CI0065
            10            CX03-CY99.                                    CI0065
            11            CX03-FILLER PICTURE  X(109).                  CI0065
            10            CX03-CY01                                     CI0065
                          REDEFINES            CX03-CY99.               CI0065
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX03-ICPCI  PICTURE  X.                       CI0065
            11            CX03-CLUPD  PICTURE  9(3).                    CI0065
            11            CX03-DLAUP  PICTURE  9(8).                    CI0065
            11            CX03-CWRC   PICTURE  99.                      CI0065
            11            CX03-CHCR   PICTURE  99.                      CI0065
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0065
            11            CX03-GEAUN  PICTURE  9(5).                    CI0065
            11            CX03-DPCHD  PICTURE  9(8).                    CI0065
            11            CX03-DLRCHK PICTURE  9(8).                    CI0065
            11            CX03-QTRCHK PICTURE  9(2).                    CI0065
            11            CX03-DNPMT  PICTURE  9(8).                    CI0065
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            CX03-CY02                                     CI0065
                          REDEFINES            CX03-CY99.               CI0065
            11            CX03-QSIRQ  PICTURE  99.                      CI0065
            11            CX03-QDRMN  PICTURE  9(2)                     CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX03-DDPRE  PICTURE  9(8).                    CI0065
            11            CX03-DDSHP  PICTURE  9(8).                    CI0065
            11            CX03-NDRFTB PICTURE  9(5).                    CI0065
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0065
            11            CX03-DDSHPA PICTURE  9(8).                    CI0065
            11            CX03-NDRFTF PICTURE  9(5).                    CI0065
            11            CX03-QDIPBK PICTURE  9(3).                    CI0065
            11            CX03-CREOR  PICTURE  X(1).                    CI0065
            11            CX03-CREOR1 PICTURE  X(1).                    CI0065
            11            CX03-DDASC  PICTURE  9(8).                    CI0065
            11            CX03-FILLER PICTURE  X(7).                    CI0065
            10            CX03-CY03                                     CI0065
                          REDEFINES            CX03-CY99.               CI0065
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0065
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0065
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0065
            11            CX03-DOPDA  PICTURE  99.                      CI0065
            11            CX03-CPMTF  PICTURE  99.                      CI0065
            11            CX03-CIRMO  PICTURE  X(12).                   CI0065
            11            CX03-CPALL  PICTURE  X(1).                    CI0065
            11            CX03-CCOLM  PICTURE  9(2).                    CI0065
            11            CX03-CBLTP  PICTURE  X(1).                    CI0065
            11            CX03-CASUB  PICTURE  9(2).                    CI0065
            11            CX03-CBLFM  PICTURE  9(2).                    CI0065
            11            CX03-IBILS  PICTURE  X.                       CI0065
            11            CX03-IPAOS  PICTURE  X.                       CI0065
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0065
            11            CX03-DLBPD  PICTURE  9(8).                    CI0065
            11            CX03-DNBPD  PICTURE  9(8).                    CI0065
            11            CX03-DODBD  PICTURE  9(8).                    CI0065
            11            CX03-CPSRE  PICTURE  99.                      CI0065
            11            CX03-ISPHN  PICTURE  X.                       CI0065
            11            CX03-TCARR  PICTURE  X(6).                    CI0065
            11            CX03-CBKPT  PICTURE  9(2).                    CI0065
            11            CX03-IECNT  PICTURE  X.                       CI0065
            11            CX03-ICONV  PICTURE  X(1).                    CI0065
            11            CX03-FILLER PICTURE  X(4).                    CI0065
            10            CX03-CY04                                     CI0065
                          REDEFINES            CX03-CY99.               CI0065
            11            CX03-CCARD  PICTURE  X(02).                   CI0065
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0065
            11            CX03-IREMT  PICTURE  X(01).                   CI0065
            11            CX03-ISBILA PICTURE  X.                       CI0065
            11            CX03-DLBPDA PICTURE  9(8).                    CI0065
            11            CX03-DNBPDA.                                  CI0065
            12            CX03-DNCYM  PICTURE  9(6).                    CI0065
            12            CX03-CEDTD  PICTURE  9(2).                    CI0065
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX03-DREMT  PICTURE  9(8).                    CI0065
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0065
            11            CX03-CWRC2  PICTURE  99.                      CI0065
            11            CX03-CHCR2  PICTURE  99.                      CI0065
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0065
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0065
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0065
       01                 CX06.                                         CI0065
            10            CX06-CX06K.                                   CI0065
            11            CX06-C299.                                    CI0065
            12            CX06-CTID.                                    CI0065
            13            CX06-CTIDA  PICTURE  9(3).                    CI0065
            13            CX06-CTIDN.                                   CI0065
            14            CX06-CTIDNP PICTURE  X(13).                   CI0065
            14            CX06-CTIDND PICTURE  9(11).                   CI0065
            10            CX06-NPECK  PICTURE  9(02).                   CI0065
            10            CX06-FILLER PICTURE  X.                       CI0065
       01                 CX12.                                         CI0065
            10            CX12-CX12K.                                   CI0065
            11            CX12-CPMTC  PICTURE  99.                      CI0065
            11            CX12-NAPDS  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX12-GESTD  PICTURE  9(8).                    CI0065
            10            CX12-GEEND  PICTURE  9(8).                    CI0065
            10            CX12-CIRMO  PICTURE  X(12).                   CI0065
            10            CX12-CDEST  PICTURE  99.                      CI0065
            10            CX12-APMTL  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            CX12-DNPMT  PICTURE  9(8).                    CI0065
            10            CX12-NIRACM PICTURE  9(2).                    CI0065
            10            CX12-CPMTF  PICTURE  99.                      CI0065
            10            CX12-IPODM  PICTURE  X.                       CI0065
            10            CX12-CLUPD  PICTURE  9(3).                    CI0065
            10            CX12-DLAUP  PICTURE  9(8).                    CI0065
            10            CX12-CWRC   PICTURE  99.                      CI0065
            10            CX12-CHCR   PICTURE  99.                      CI0065
            10            CX12-GEOPD2 PICTURE  X(8).                    CI0065
            10            CX12-GEAUN  PICTURE  9(5).                    CI0065
            10            CX12-DPCHD  PICTURE  9(8).                    CI0065
            10            CX12-DNEXE  PICTURE  9(8).                    CI0065
            10            CX12-CCSMQ  PICTURE  X.                       CI0065
            10            CX12-GCUSPZ PICTURE  X(12).                   CI0065
            10            CX12-CORTY  PICTURE  X.                       CI0065
            10            CX12-CNAVR  PICTURE  X(1).                    CI0065
            10            CX12-DELOI3 PICTURE  9(6).                    CI0065
            10            CX12-ALOIDD PICTURE  9(9)V99                  CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            CX12-FILLER PICTURE  X(5).                    CI0065
       01                 CX13.                                         CI0065
            10            CX13-GELL   PICTURE  9(4)                     CI0065
                          BINARY.                                       CI0065
            10            CX13-CY20.                                    CI0065
            11            CX13-CX13K.                                   CI0065
            12            CX13-CARTZ  PICTURE  99.                      CI0065
            12            CX13-NAPDS  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX13-GESTD  PICTURE  9(8).                    CI0065
            11            CX13-GEEND  PICTURE  9(8).                    CI0065
            11            CX13-DASUQ  PICTURE  9(8).                    CI0065
            11            CX13-CDEST  PICTURE  99.                      CI0065
            11            CX13-IIARR  PICTURE  X.                       CI0065
            11            CX13-DLAUP  PICTURE  9(8).                    CI0065
            11            CX13-GEOPD2 PICTURE  X(8).                    CI0065
            11            CX13-GEAUN  PICTURE  9(5).                    CI0065
            11            CX13-DPCHD  PICTURE  9(8).                    CI0065
            11            CX13-PPOT1  PICTURE  S9(3)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX13-ACOT1  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX13-QPST1  PICTURE  S9(7)V999                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX13-FILLER PICTURE  X(03).                   CI0065
            10            CX13-CY96.                                    CI0065
            11            CX13-FILLER PICTURE  X(50).                   CI0065
            10            CX13-CY21                                     CI0065
                          REDEFINES            CX13-CY96.               CI0065
            11            CX13-DNPMT  PICTURE  9(8).                    CI0065
            11            CX13-CPMTF  PICTURE  99.                      CI0065
            11            CX13-ADBRQ  PICTURE  S9(11)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX13-QSHOWQ PICTURE  S9(9)V999                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX13-PACT1  PICTURE  S999V999                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX13-DOPDA  PICTURE  99.                      CI0065
            11            CX13-DNEXE  PICTURE  9(8).                    CI0065
            11            CX13-CIRMO  PICTURE  X(12).                   CI0065
            10            CX13-CY98.                                    CI0065
            11            CX13-FILLER PICTURE  X(120).                  CI0065
            10            CX13-CY25                                     CI0065
                          REDEFINES            CX13-CY98.               CI0065
            11            CX13-COPTC  PICTURE  9(1).                    CI0065
            11            CX13-ILPOI  PICTURE  X(1).                    CI0065
            11            CX13-CATOC  PICTURE  X(1).                    CI0065
            11            CX13-CEOIA  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX13-ACOAR  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX13-CEOTR  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX13-DSTMO  PICTURE  99.                      CI0065
            10            CX13-CY27                                     CI0065
                          REDEFINES            CX13-CY98.               CI0065
            11            CX13-QMTH1  PICTURE  9(3).                    CI0065
            11            CX13-IDRMD  PICTURE  X.                       CI0065
            10            CX13-CY28                                     CI0065
                          REDEFINES            CX13-CY98.               CI0065
            11            CX13-AALLBL PICTURE  S9(8)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX13-PSURR  PICTURE  S9(3)V999                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX13-DFPMT  PICTURE  9(8).                    CI0065
            11            CX13-QMTHLA PICTURE  9(3).                    CI0065
            11            CX13-PWHLDS PICTURE  S999V9(5)                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX13-ISWHO  PICTURE  X(1).                    CI0065
            10            CX13-CY29                                     CI0065
                          REDEFINES            CX13-CY98.               CI0065
            11            CX13-IINDI1 PICTURE  X(1).                    CI0065
            11            CX13-IINDI2 PICTURE  X(1).                    CI0065
            11            CX13-IINDI3 PICTURE  X(1).                    CI0065
            11            CX13-PWHLD5 PICTURE  S999V99                  CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX13-CCSMQ  PICTURE  X.                       CI0065
            11            CX13-CPLEC  PICTURE  XX.                      CI0065
            11            CX13-IPTRDA PICTURE  X(01).                   CI0065
            11            CX13-GCUSPY PICTURE  X(12).                   CI0065
            11            CX13-ALOIDA PICTURE  S9(11)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX13-DELOI  PICTURE  9(8).                    CI0065
            11            CX13-CLGND  PICTURE  X.                       CI0065
            11            CX13-CORTYA PICTURE  X(3).                    CI0065
            11            CX13-CPH3U  PICTURE  X.                       CI0065
            11            CX13-CNAVR  PICTURE  X(1).                    CI0065
            11            CX13-NEXEC  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
       01                 CX14.                                         CI0065
            10            CX14-GELL   PICTURE  9(4)                     CI0065
                          BINARY.                                       CI0065
            10            CX14-CX14K.                                   CI0065
            11            CX14-NPISQ  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            CX14-ACOTD  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            CX14-PPOTD  PICTURE  S9(3)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            CX14-QPSTD  PICTURE  S9(7)V999                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            CX14-CPITC  PICTURE  99.                      CI0065
            10            CX14-FILLER PICTURE  X(04).                   CI0065
            10            CX14-CY97.                                    CI0065
            11            CX14-FILLER PICTURE  X(32).                   CI0065
            10            CX14-CY30                                     CI0065
                          REDEFINES            CX14-CY97.               CI0065
            11            CX14-IOWNC  PICTURE  X.                       CI0065
            11            CX14-CTYPE  PICTURE  X.                       CI0065
            11            CX14-C299.                                    CI0065
            12            CX14-CTID.                                    CI0065
            13            CX14-CTIDA  PICTURE  9(3).                    CI0065
            13            CX14-CTIDN.                                   CI0065
            14            CX14-CTIDNP PICTURE  X(13).                   CI0065
            14            CX14-CTIDND PICTURE  9(11).                   CI0065
            11            CX14-CPMTC  PICTURE  99.                      CI0065
            11            CX14-IACSD  PICTURE  X.                       CI0065
            10            CX14-CY31                                     CI0065
                          REDEFINES            CX14-CY97.               CI0065
            11            CX14-FILLER PICTURE  X(2).                    CI0065
            11            CX14-IDELI  PICTURE  X.                       CI0065
            11            CX14-CDEL1  PICTURE  9(3).                    CI0065
            11            CX14-NDELS  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            CX14-CY32                                     CI0065
                          REDEFINES            CX14-CY97.               CI0065
            11            CX14-GCUSPZ PICTURE  X(12).                   CI0065
       01                 CX2Y.                                         CI0065
            10            CX2Y-CX2YK.                                   CI0065
            11            CX2Y-C299.                                    CI0065
            12            CX2Y-CTID.                                    CI0065
            13            CX2Y-CTIDA  PICTURE  9(3).                    CI0065
            13            CX2Y-CTIDN.                                   CI0065
            14            CX2Y-CTIDNP PICTURE  X(13).                   CI0065
            14            CX2Y-CTIDND PICTURE  9(11).                   CI0065
            11            CX2Y-C199.                                    CI0065
            12            CX2Y-CLID.                                    CI0065
            13            CX2Y-CLIDO  PICTURE  9(3).                    CI0065
            13            CX2Y-CLIDN.                                   CI0065
            14            CX2Y-CLIDNP PICTURE  X(12).                   CI0065
            14            CX2Y-CLIDND PICTURE  9(8).                    CI0065
            11            CX2Y-CARTY  PICTURE  99.                      CI0065
            11            CX2Y-NARRS  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
       01                 CX6Y.                                         CI0065
            10            CX6Y-CX6YK.                                   CI0065
            11            CX6Y-C299.                                    CI0065
            12            CX6Y-CTID.                                    CI0065
            13            CX6Y-CTIDA  PICTURE  9(3).                    CI0065
            13            CX6Y-CTIDN.                                   CI0065
            14            CX6Y-CTIDNP PICTURE  X(13).                   CI0065
            14            CX6Y-CTIDND PICTURE  9(11).                   CI0065
            11            CX6Y-C199.                                    CI0065
            12            CX6Y-CLID.                                    CI0065
            13            CX6Y-CLIDO  PICTURE  9(3).                    CI0065
            13            CX6Y-CLIDN.                                   CI0065
            14            CX6Y-CLIDNP PICTURE  X(12).                   CI0065
            14            CX6Y-CLIDND PICTURE  9(8).                    CI0065
            11            CX6Y-CARTY  PICTURE  99.                      CI0065
            11            CX6Y-NARRS  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX6Y-CTID1  PICTURE  X(27).                   CI0065
            11            CX6Y-CARTZ  PICTURE  99.                      CI0065
            11            CX6Y-NAPDS  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            CX6Y-NPISQ  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0065
            10            XW05-XW06.                                    CI0065
            11            XW05-XDBPCB.                                  CI0065
            12            XW05-XDBDNM PICTURE  X(08)                    CI0065
                          VALUE                SPACE.                   CI0065
            12            XW05-XSEGLV PICTURE  X(02)                    CI0065
                          VALUE                SPACE.                   CI0065
            12            XW05-XRC    PICTURE  X(02)                    CI0065
                          VALUE                SPACE.                   CI0065
            12            XW05-XPROPT PICTURE  X(04)                    CI0065
                          VALUE                SPACE.                   CI0065
            12            XW05-FILLER PICTURE  S9(5)                    CI0065
                          VALUE                ZERO                     CI0065
                          BINARY.                                       CI0065
            12            XW05-XSEGNM PICTURE  X(08)                    CI0065
                          VALUE                SPACE.                   CI0065
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0065
                          VALUE                ZERO                     CI0065
                          BINARY.                                       CI0065
            12            XW05-XSEGNB PICTURE  9(05)                    CI0065
                          VALUE                ZERO                     CI0065
                          BINARY.                                       CI0065
            12            XW05-XCOKEY PICTURE  X(70)                    CI0065
                          VALUE                SPACE.                   CI0065
            10            XW05-XW07.                                    CI0065
            11            XW05-XIOPCB.                                  CI0065
            12            XW05-XTERMI PICTURE  X(08)                    CI0065
                          VALUE                SPACE.                   CI0065
            12            XW05-FILLER PICTURE  XX                       CI0065
                          VALUE                SPACE.                   CI0065
            12            XW05-XRC1   PICTURE  X(02)                    CI0065
                          VALUE                SPACE.                   CI0065
            12            XW05-FILLER PICTURE  X(12)                    CI0065
                          VALUE                SPACE.                   CI0065
            12            XW05-XMODNM PICTURE  X(8)                     CI0065
                          VALUE                SPACE.                   CI0065
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0065
                          VALUE                ZERO.                    CI0065
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0065
                          VALUE                ZERO.                    CI0065
            10            XW05-XGU    PICTURE  X(4)                     CI0065
                          VALUE                'GU  '.                  CI0065
            10            XW05-XGHU   PICTURE  X(4)                     CI0065
                          VALUE                'GHU '.                  CI0065
            10            XW05-XGN    PICTURE  X(4)                     CI0065
                          VALUE                'GN  '.                  CI0065
            10            XW05-XGHN   PICTURE  X(4)                     CI0065
                          VALUE                'GHN '.                  CI0065
            10            XW05-XGNP   PICTURE  X(4)                     CI0065
                          VALUE                'GNP '.                  CI0065
            10            XW05-XGHNP  PICTURE  X(4)                     CI0065
                          VALUE                'GHNP'.                  CI0065
            10            XW05-XREPL  PICTURE  XXXX                     CI0065
                          VALUE                'REPL'.                  CI0065
            10            XW05-XISRT  PICTURE  X(4)                     CI0065
                          VALUE                'ISRT'.                  CI0065
            10            XW05-XDLET  PICTURE  X(4)                     CI0065
                          VALUE                'DLET'.                  CI0065
            10            XW05-XOPEN  PICTURE  X(4)                     CI0065
                          VALUE                'OPEN'.                  CI0065
            10            XW05-XCLSE  PICTURE  X(4)                     CI0065
                          VALUE                'CLSE'.                  CI0065
            10            XW05-XCHKP  PICTURE  X(4)                     CI0065
                          VALUE                'CHKP'.                  CI0065
            10            XW05-XXRST  PICTURE  X(4)                     CI0065
                          VALUE                'XRST'.                  CI0065
            10            XW05-XTERM  PICTURE  X(4)                     CI0065
                          VALUE                'TERM'.                  CI0065
            10            XW05-XNFPAC PICTURE  X(13)                    CI0065
                          VALUE                SPACE.                   CI0065
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0065
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0065
      *WORKING STORAGE FIELD FOR COUNTER AND THE DATES.                 AAACTG
        01 7-CNT-DATE      PIC 9(3) VALUE ZEROES.                       AAACTG
        01 7-CURR-DATE     PIC 9(8) VALUE ZEROES.                       AAACTG
        01 7-REQD-DATE     PIC 9(8) VALUE ZEROES.                       AAACTG
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
       01  7-82-WORK-DATE.                                              AAOLBB
           05  7-82-WORK-CC            PIC 9(02).                       AAOLBB
           05  7-82-WORK-YY            PIC 9(02).                       AAOLBB
           05  7-82-WORK-MM            PIC 9(02).                       AAOLBB
           05  7-82-WORK-DD            PIC 9(02).                       AAOLBB
       01  7-82-ANN-DATE.                                               AAOLBB
           05  7-82-ANN-CC            PIC 9(02).                        AAOLBB
           05  7-82-ANN-YY            PIC 9(02).                        AAOLBB
           05  7-82-ANN-MM            PIC 9(02).                        AAOLBB
           05  7-82-ANN-DD            PIC 9(02).                        AAOLBB
       01  7-82-RESULT            PIC 9(02).                            AAOLBB
       01  7-82-REM               PIC 9(02).                            AAOLBB
       01  7-82-PREMIUM           PIC S9(7)V99   VALUE ZEROES.          AAOLBB
       01  7-82-PREMIUM-T         PIC S9(7)V99   VALUE ZEROES.          AAOLBB
       01  7-82-PREMIUM-O         PIC S9(7)V99   VALUE ZEROES.          AAOLBB
       01  7-82-PREMIUMS          PIC 9(7)V99    VALUE ZEROES.          AAOLBB
       01  7-82-CALC  REDEFINES 7-82-PREMIUMS.                          AAOLBB
           05  FILLER             PIC 999999.                           AAOLBB
           05  7-82-ONE           PIC 9V99.                             AAOLBB
       01  7-82-TENS              PIC 9(7)V99.                          AAOLBB
       01  7-82-ONES.                                                   AAOLBB
           05  7-82-CONES         PIC 9V9.                              AAOLBB
           05  FILLER             PIC 9.                                AAOLBB
       01  7-82-FL-DI             PIC X      VALUE 'N'.                 AAOLBB
       01  7-82-ALCIDN.                                                 AAOLBB
           05  7-82-FIRST-4       PIC 9(4).                             AAOLBB
           05  7-82-REST          PIC 9(7).                             AAOLBB
       01  7-82-PRSCD.                                                  AAOLBB
           05  7-82-DE-CODE       PIC 9(2).                             AAOLBB
           05  7-82-FGH-CODE      PIC 9(3).                             AAOLBB
           05  7-82-REST          PIC 9(4).                             AAOLBB
       01  7-82-TL-2012           PIC X      VALUE 'N'.                 AAOLBB
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
       01                 NS00.                                         CI0065
          05              NS00-00.                                      CI0065
            10            NS00-NS00K.                                   CI0065
            11            NS00-PRCSTK PICTURE  XX.                      CI0065
          05              NS00-SUITE.                                   CI0065
            15       FILLER         PICTURE  X(00078).                  CI0065
       01                 NS20  REDEFINES      NS00.                    CI0065
            10       FILLER         PICTURE  X(00002).                  CI0065
            10            NS20-DCACG  PICTURE  9(8).                    CI0065
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            NS20-CCDAT  PICTURE  X(8).                    CI0065
            10            NS20-DCALP  PICTURE  X(12).                   CI0065
            10            NS20-DNACG  PICTURE  9(8).                    CI0065
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            NS20-CNDAT  PICTURE  X(8).                    CI0065
            10            NS20-DNALP  PICTURE  X(12).                   CI0065
            10            NS20-DCACD  PICTURE  X(10).                   CI0065
            10            NS20-FILLER PICTURE  X(4).                    CI0065
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      ******************************************************************AM0140
      **** PCB ADDRESS LIST FOR CI0140                                 *AM0140
      ******************************************************************AM0140
       01                 CI0140-PH-PCB-ADDR-LIST.                      AM0140
           05             CI0140-PH-PCB-LM1P-PTR1      POINTER.         AM0140
           05             CI0140-PH-PCB-LUVP-PTR1      POINTER.         AM0140
           05             CI0140-PH-PCB-LH1P-PTR1      POINTER.         AM0140
           05             CI0140-PH-PCB-LARP-PTR1      POINTER.         AM0140
           05             CI0140-PH-PCB-LPDP-PTR1      POINTER.         AM0140
           05             CI0140-PH-PCB-ARAY-PTR1      POINTER.         AM0140
           05             CI0140-PH-PCB-AR1P-PTR1      POINTER.         AM0140

       01  W-SW00-AREAS.

      *  MISCELLANEOUS CF SWITCHES USED FOR SEGMENT ACCESS
      *  '1' = FOUND
      *  '0' = NOT FOUND
           05  AA85-CF                 PIC X(01).

      ******************************************************************ADUTAB
      **              TABLE TA6A ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA6A-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=6A FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA6A.                                                CI0065
           04    G-TA6A-PARAM.                                          CI0065
             10  G-TA6A-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0065
                        VALUE      +028.                                CI0065
             10  G-TA6A-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0065
                        VALUE      +001.                                CI0065
             10  G-TA6A-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0065
                        VALUE      +008.                                CI0065
             10  G-TA6A-NUAPP  PICTURE 99                               CI0065
                        VALUE       0.                                  CI0065
             10  G-TA6A-NUTAB  PICTURE X(6)                             CI0065
                        VALUE 'TA006A'.                                 CI0065
             10  G-TA6A-TABFO  PICTURE XX                 VALUE SPACE.  CI0065
             10  G-TA6A-TABCR  PICTURE XX                 VALUE SPACE.  CI0065
             10  G-TA6A-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0065
             10  G-TA6A-NUSSC  PICTURE X  VALUE   ' '.                  CI0065
             10  G-TA6A-NUSSY  PICTURE X                  VALUE SPACE.  CI0065
             10  G-TA6A-TRANID PICTURE X(4)               VALUE SPACE.  CI0065
             10  G-TA6A-FILSYS.                                         CI0065
             15  G-TA6A-USERC  PICTURE X(6)               VALUE SPACE.  CI0065
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0065
           04             TA6A.                                         CI0065
            10            TA6A-GADPR.                                   CI0065
            11            TA6A-CTIDA  PICTURE  9(3)                     CI0065
                          VALUE                ZERO.                    CI0065
            11            TA6A-PRCOD  PICTURE  9(5)                     CI0065
                          VALUE                ZERO.                    CI0065
            10            TA6A-CSPOS  PICTURE  X(3)                     CI0065
                          VALUE                SPACE.                   CI0065
            10            TA6A-CSPOD  PICTURE  X(3)                     CI0065
                          VALUE                SPACE.                   CI0065
            10            TA6A-IARRGA PICTURE  X                        CI0065
                          VALUE                SPACE.                   CI0065
            10            TA6A-IARLNA PICTURE  X                        CI0065
                          VALUE                SPACE.                   CI0065
            10            TA6A-CADCA  PICTURE  X(3)                     CI0065
                          VALUE                SPACE.                   CI0065
            10            TA6A-CEAIA  PICTURE  X(3)                     CI0065
                          VALUE                SPACE.                   CI0065
            10            TA6A-CBDCA  PICTURE  X(3)                     CI0065
                          VALUE                SPACE.                   CI0065
            10            TA6A-CLIRB  PICTURE  X(3)                     CI0065
                          VALUE                SPACE.                   CI0065
      **                                                                ADUTAB
      ******************************************************************AM0140
      **** LIFE DETAIL INFO        PASS AREA (LINKAGE) *****************AM0140
      ******************************************************************AM0140
      *                                                                 AM0140
      *!WF DSP=WE DSL=K9 SEL=40 FOR=I DES=1 LEV=1                       AM0140
       01                 WE40.                                         CI0065
            10            WE40-C299.                                    CI0065
            11            WE40-CTID.                                    CI0065
            12            WE40-CTIDA  PICTURE  9(3).                    CI0065
            12            WE40-CTIDN.                                   CI0065
            13            WE40-CTIDNP PICTURE  X(13).                   CI0065
            13            WE40-CTIDND PICTURE  9(11).                   CI0065
            10            WE40-PRCOD  PICTURE  9(5).                    CI0065
            10            WE40-IANPY  PICTURE  X.                       CI0065
            10            WE40-DEFFT  PICTURE  9(8).                    CI0065
            10            WE40-ALPLDT PICTURE  9(8).                    CI0065
            10            WE40-TPLNL  PICTURE  X(30).                   CI0065
            10            WE40-MPLNA  PICTURE  X(19).                   CI0065
            10            WE40-ITMEC  PICTURE  X(1).                    CI0065
            10            WE40-ALPMOD PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ALMODE PICTURE  99.                      CI0065
            10            WE40-MPMTF  PICTURE  X(14).                   CI0065
            10            WE40-ALAPIT PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-AGSP   PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ALDBEN PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-CDBENE PICTURE  X.                       CI0065
            10            WE40-IDEBE  PICTURE  X.                       CI0065
            10            WE40-ALRISK PICTURE  S9(9)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-CFRQZ  PICTURE  X.                       CI0065
            10            WE40-ASBENA PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ASBENB PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ASBENC PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ASBENE PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ASBENF PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-AACTV  PICTURE  S9(11)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ANGOF  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ACCTVC PICTURE  X(20).                   CI0065
            10            WE40-ALLNB  PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-CBRIT  PICTURE  SV9(5)                   CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ASANP  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ALSURR PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ASURR  PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ASURRN PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ASURRW PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-PSURR  PICTURE  S9(3)V999                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-PCIRB5 PICTURE  S9(3)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ALVLFA PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ALVLVA PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ISUBA  PICTURE  X.                       CI0065
            10            WE40-ACVALB PICTURE  S9(11)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ITAXI  PICTURE  X.                       CI0065
            10            WE40-ATLTB  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-AEARN0 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ITAXD  PICTURE  X(1).                    CI0065
            10            WE40-ATFPI  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-AEARN1 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-AFETY  PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-CEYAW  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-APTXR  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ITAXN  PICTURE  X.                       CI0065
            10            WE40-IUTAX  PICTURE  X.                       CI0065
            10            WE40-CETRL  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-CSSVL  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ALTOT  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ALFGH  PICTURE  999.                     CI0065
            10            WE40-DSTT1  PICTURE  9(8).                    CI0065
            10            WE40-DNPMT  PICTURE  9(8).                    CI0065
            10            WE40-CTLPD  PICTURE  9(8).                    CI0065
            10            WE40-CTLPD2 PICTURE  S9(8)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ALFXPO PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ALINPU PICTURE  S9(8)V999                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-PFPAY  PICTURE  S9(3)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-PVPAY  PICTURE  S9(3)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ATWHDD PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ATWHDE PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ACVIU  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-MPMTT  PICTURE  X(20).                   CI0065
            10            WE40-GESTNS PICTURE  X(2).                    CI0065
            10            WE40-CTWHPB PICTURE  9(3)V999.                CI0065
            10            WE40-CTWHCB PICTURE  X.                       CI0065
            10            WE40-ACTCH  PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-CACTL1 PICTURE  X(04).                   CI0065
            10            WE40-ALPLNI PICTURE  9.                       CI0065
            10            WE40-ATSA8  PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-QEDAY  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ATIPA  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-AGENN  PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-AGENO  PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-APOCY  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-APOTD  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-CSUSI  PICTURE  99.                      CI0065
            10            WE40-ALINNO PICTURE  99.                      CI0065
            10            WE40-ALPLNJ PICTURE  9.                       CI0065
            10            WE40-COLPL  PICTURE  9(05).                   CI0065
            10            WE40-ALPAGR PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ALDDUE PICTURE  9(08).                   CI0065
            10            WE40-ALAPST PICTURE  99.                      CI0065
            10            WE40-CPCAL  PICTURE  9.                       CI0065
            10            WE40-CRTBK  PICTURE  99.                      CI0065
            10            WE40-ACGPO  PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-AMMP   PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-AAMFY  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-TBENPA PICTURE  X(30).                   CI0065
            10            WE40-TDE30  PICTURE  X(30).                   CI0065
            10            WE40-CROOR  PICTURE  99.                      CI0065
            10            WE40-AMXLN  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-ILNST  PICTURE  X.                       CI0065
            10            WE40-AENTI  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-APRYT  PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            WE40-FILLER PICTURE  X(409).                  CI0065
      ******************************************************************AM0141
      **** LIFE DETAIL INFO        PASS AREA (LINKAGE) *****************AM0141
      ******************************************************************AM0141
      *                                                                 AM0141
      *!WF DSP=WF DSL=K9 SEL=41 FOR=I DES=1 LEV=1                       AM0141
       01                 WF41.                                         CI0065
            10            WF41-FILLER PICTURE  X(2000).                 CI0065
            10            WF41-K94R                                     CI0065
                          REDEFINES            WF41-FILLER.             CI0065
            11            WF41-CTID   PICTURE  X(27).                   CI0065
            11            WF41-PRCOD  PICTURE  9(5).                    CI0065
            11            WF41-CVSTC  PICTURE  X(4).                    CI0065
            11            WF41-DVALU  PICTURE  9(8).                    CI0065
            11            WF41-AACTV  PICTURE  S9(11)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-PE65.                                    CI0065
            12            WF41-CVAPC  PICTURE  X(6).                    CI0065
            12            WF41-CVALB  PICTURE  X(3).                    CI0065
            12            WF41-CASTA  PICTURE  X.                       CI0065
            12            WF41-CTWST1 PICTURE  X(3).                    CI0065
            12            WF41-CPISC  PICTURE  X(3).                    CI0065
            12            WF41-ALPLDT PICTURE  9(8).                    CI0065
            12            WF41-DEFFT  PICTURE  9(8).                    CI0065
            12            WF41-DANNI  PICTURE  9(8).                    CI0065
            12            WF41-DTPMT  PICTURE  9(8).                    CI0065
            12            WF41-ITAMR  PICTURE  X(1).                    CI0065
            12            WF41-AGAPA  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-ATRPA  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-ATROP  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-ITMECC PICTURE  X(1).                    CI0065
            12            WF41-CBPLCA PICTURE  X.                       CI0065
            12            WF41-CPRCC  PICTURE  X.                       CI0065
            12            WF41-CLSEX  PICTURE  X.                       CI0065
            12            WF41-QPOIA  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-IREIA  PICTURE  X.                       CI0065
            12            WF41-APCUA  PICTURE  S9(6)V9(5)               CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-AGLPA  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-AGSPA  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-ATGPA  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-ACBIN1 PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-CABRC  PICTURE  X.                       CI0065
            12            WF41-CDSON  PICTURE  X(06).                   CI0065
            12            WF41-CADVN  PICTURE  X(10).                   CI0065
            12            WF41-CVOMC1 PICTURE  X(1).                    CI0065
            12            WF41-CSSUP2 PICTURE  X.                       CI0065
            12            WF41-ACECP  PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-CCOFE  PICTURE  X.                       CI0065
            12            WF41-GRIDN7 PICTURE  S9(7)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-QNZDS  PICTURE  S9(04)                   CI0065
                          COMPUTATIONAL   SYNC RIGHT.                   CI0065
            12            WF41-QTNOL  PICTURE  S9(05)                   CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-QTNPW  PICTURE  S9(05)                   CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-IREIN  PICTURE  X.                       CI0065
            12            WF41-DNRIP  PICTURE  9(8).                    CI0065
            12            WF41-ATDPA  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-CREPC  PICTURE  XX.                      CI0065
            12            WF41-CMLTP  PICTURE  X.                       CI0065
            12            WF41-IREPL6 PICTURE  X.                       CI0065
            12            WF41-CCLAC  PICTURE  X.                       CI0065
            12            WF41-ALNTI  PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-ATIPA  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-CVASC  PICTURE  XX.                      CI0065
            12            WF41-DRTHC  PICTURE  9(8).                    CI0065
            12            WF41-CENDO  PICTURE  X.                       CI0065
            12            WF41-ICOEX  PICTURE  X(1).                    CI0065
            12            WF41-INURS  PICTURE  X(1).                    CI0065
            12            WF41-ITRML  PICTURE  X(1).                    CI0065
            12            WF41-IBIRA  PICTURE  X(1).                    CI0065
            12            WF41-CCOUL  PICTURE  XX.                      CI0065
            12            WF41-ASGLP  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-DMATUR PICTURE  9(8).                    CI0065
            12            WF41-CJOIP  PICTURE  X.                       CI0065
            12            WF41-CPNOP  PICTURE  X(2).                    CI0065
            12            WF41-ARBRP  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-ACCHV  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-ASCHV  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-CGMBR  PICTURE  X.                       CI0065
            12            WF41-AMCTV  PICTURE  S9(7)V99.                CI0065
            12            WF41-ACBIN2 PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-PE1B                                     CI0065
                          REDEFINES            WF41-ACBIN2.             CI0065
            13            WF41-NAAMC  PICTURE  9(2).                    CI0065
            13            WF41-PNPCT  PICTURE  999.                     CI0065
            13            WF41-FILLER PICTURE  X(1).                    CI0065
            11            WF41-FILLER PICTURE  X.                       CI0065
            11            WF41-PE6B.                                    CI0065
            12            WF41-CEBMO  PICTURE  9(2).                    CI0065
            12            WF41-CBNBC1 PICTURE  X.                       CI0065
            12            WF41-AMDAR  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-ALDDUE PICTURE  9(08).                   CI0065
            12            WF41-ASPAM2 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-DRESE  PICTURE  9(8).                    CI0065
            12            WF41-DACUP  PICTURE  9(02).                   CI0065
            12            WF41-ACVAMG PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-ISTWH  PICTURE  X(1).                    CI0065
            12            WF41-AVAIP  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-FILLER PICTURE  X(63).                   CI0065
            11            WF41-MPMTF  PICTURE  X(14).                   CI0065
            11            WF41-PE86.                                    CI0065
            12            WF41-CVOOD  PICTURE  S9(5)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-DEIRNB PICTURE  9(8).                    CI0065
            12            WF41-QCRPD  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-CBRIT  PICTURE  SV9(5)                   CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-DEIRNA PICTURE  9(8).                    CI0065
            11            WF41-PE91.                                    CI0065
            12            WF41-CVOOD1 PICTURE  S9(5)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-DTRMSX PICTURE  X(8).                    CI0065
            12            WF41-DTRMEX PICTURE  X(8).                    CI0065
            12            WF41-ALPRUN PICTURE  S999V9(6)                CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-PSPSX                                    CI0065
                          REDEFINES            WF41-ALPRUN              CI0065
               PICTURE    S9(7)V99                                      CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-PIXPR  PICTURE  S9V9(4)                  CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-PE3V.                                    CI0065
            12            WF41-PE90.                                    CI0065
            13            WF41-CSTCVE PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ASURR1 PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ALSURR PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ASINTC PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-AMVA1  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ASPAM  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ACVAM  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-AMSBT  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ACVALC PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ATWS   PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ISELO  PICTURE  X.                       CI0065
            13            WF41-FILLER PICTURE  X(1).                    CI0065
            13            WF41-FILLER PICTURE  X(2).                    CI0065
            13            WF41-ASTCV1 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ATFCVC PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-FILLER PICTURE  X(1).                    CI0065
            13            WF41-FILLER PICTURE  X(2).                    CI0065
            13            WF41-AUINTA PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-FILLER PICTURE  X(1).                    CI0065
            13            WF41-IRCHG  PICTURE  X.                       CI0065
            13            WF41-ASTXW8 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ATFRA  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-APLIV  PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-PCIRB1 PICTURE  S9(2)V9(3)               CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ACVAMF PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-CSNCVE PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-PCIRB7 PICTURE  99V999.                  CI0065
            13            WF41-AMNSR  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-AMINL  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ASCHV  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-PRCHG  PICTURE  999V999                  CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ISTUR  PICTURE  X.                       CI0065
            13            WF41-CSTIM  PICTURE  X.                       CI0065
            13            WF41-AMCAV1 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ARCHG  PICTURE  9(7)V99                  CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-PE7C                                     CI0065
                          REDEFINES            WF41-PE90.               CI0065
            13            WF41-ACOGR  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ACONE  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ASURR3 PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-PSURR1 PICTURE  S9(3)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-ACOTX  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-TCOMG  PICTURE  X(30).                   CI0065
            13            WF41-CVCST  PICTURE  X(4).                    CI0065
            13            WF41-FILLER PICTURE  X(100).                  CI0065
            12            WF41-ALCCV  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-ATLPD  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-ARTLP  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-APRLP  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-ALPAY  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-DLSST  PICTURE  9(8).                    CI0065
            12            WF41-ACAUN  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-ITAXN  PICTURE  X.                       CI0065
            11            WF41-ITAXD  PICTURE  X(1).                    CI0065
            11            WF41-ITAXI  PICTURE  X.                       CI0065
            11            WF41-PCIRB  PICTURE  S99V999                  CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-ALBUL  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-ACGPA  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-ACACTV PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-ASURRN PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-ASURRW PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-PSURR  PICTURE  S9(3)V999                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-ANGOF  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-ACVIU  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-MPMTT  PICTURE  X(20).                   CI0065
            11            WF41-CETRL  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-AFETY  PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-GESTNS PICTURE  X(2).                    CI0065
            11            WF41-CTWHPB PICTURE  9(3)V999.                CI0065
            11            WF41-CTWHCB PICTURE  X.                       CI0065
            11            WF41-ACTCH  PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-AMXLN  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-CACTL1 PICTURE  X(04).                   CI0065
            11            WF41-AEARN0 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-AEARN1 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-APYMT  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-PCIRA  PICTURE  S99V999                  CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-ASTCVC PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-CDBNLG PICTURE  X(3).                    CI0065
            11            WF41-ILNST  PICTURE  X.                       CI0065
            11            WF41-AENTI  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-AAFEA  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-ATPWO  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-ACVAMD PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-TBENPA PICTURE  X(30).                   CI0065
            11            WF41-PE82.                                    CI0065
            12            WF41-CPRCC  PICTURE  X.                       CI0065
            12            WF41-CSPCL  PICTURE  XX.                      CI0065
            12            WF41-PE6F                                     CI0065
                          OCCURS       006     TIMES.                   CI0065
            13            WF41-AFLEX  PICTURE  S9(3)V9(2)               CI0065
                          COMPUTATIONAL-3.                              CI0065
            13            WF41-DFEED  PICTURE  9(8).                    CI0065
            11            WF41-DWSDT  PICTURE  9(8).                    CI0065
            11            WF41-IRDPH  PICTURE  X.                       CI0065
            11            WF41-DWAIT  PICTURE  9(8).                    CI0065
            11            WF41-IAPGP  PICTURE  X.                       CI0065
            11            WF41-APGBP  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-PE6T.                                    CI0065
            12            WF41-ISDIS  PICTURE  X.                       CI0065
            12            WF41-IBPER  PICTURE  X.                       CI0065
            12            WF41-PINFL  PICTURE  X(02).                   CI0065
            12            WF41-CINFT  PICTURE  X.                       CI0065
            12            WF41-ARODE  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-ARDBL  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-ARPSL  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            12            WF41-CLONT  PICTURE  X.                       CI0065
            12            WF41-CLONT1 PICTURE  X.                       CI0065
            12            WF41-CLONT2 PICTURE  X.                       CI0065
            12            WF41-DPRPA  PICTURE  9(8).                    CI0065
            12            WF41-ADBSU  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-ALDBEN PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-APRYT  PICTURE  S9(09)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-AMECP  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            WF41-FILLER PICTURE  X(989).                  CI0065

       01  W-WS00-AREAS.

      *!WE
           05  WS00-PWHLD
                        PICTURE 999V99999.                              CI0065
           05  WS00-PWHLDX REDEFINES WS00-PWHLD PIC X(8).
           05  WS00-DAYS PIC 9(3) VALUE ZEROES.
           05  WS00-CCYYMMDD.
              10  WS00-CCYY           PIC 9(04).
              10  WS00-MM             PIC 9(02).
              10  WS00-DD             PIC 9(02).
           05  WS00-MTHS PIC 9(2) VALUE ZEROES.
           05  WS00-DIV                PIC 99.
           05  WS00-OK                 PIC X.
           05  WS00-QT-OK              PIC X.
           05  WS00-SA-OK              PIC X.
           05  WS00-AN-OK              PIC X.
           05  WS00-IK                 PIC X.

       01   DEBUT-WSS.                                                  CI0065
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0065
            05   IK     PICTURE X.                                      CI0065
       01  CONSTANTES-PAC.                                              CI0065
           05  FILLER  PICTURE X(87)   VALUE                            CI0065
                     '6015 CAT09/08/14CI0065ADMIN   14:34:28CI0065P AMERCI0065
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0065
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0065
           05  NUGNA   PICTURE X(5).                                    CI0065
           05  APPLI   PICTURE X(3).                                    CI0065
           05  DATGN   PICTURE X(8).                                    CI0065
           05  PROGR   PICTURE X(6).                                    CI0065
           05  CODUTI  PICTURE X(8).                                    CI0065
           05  TIMGN   PICTURE X(8).                                    CI0065
           05  PROGE   PICTURE X(8).                                    CI0065
           05  COBASE  PICTURE X(4).                                    CI0065
           05  DATGNC  PICTURE X(10).                                   CI0065
           05  RELEAS  PICTURE X(7).                                    CI0065
           05  DATGE   PICTURE X(10).                                   CI0065
           05  DATSQ   PICTURE X(10).                                   CI0065
       01  DATCE.                                                       CI0065
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0065
         05  DATOR.                                                     CI0065
           10  DATOA  PICTURE XX.                                       CI0065
           10  DATOM  PICTURE XX.                                       CI0065
           10  DATOJ  PICTURE XX.                                       CI0065
       01   VARIABLES-CONDITIONNELLES.                                  CI0065
            05                  FT      PICTURE X VALUE '0'.            CI0065
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0065
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0065
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU070
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0065
            05       5-AA00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0065
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0065
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0065
       01               S-AA10-SSA.                                     CI0065
            10         S1-AA10-SEGNAM PICTURE X(8)                      CI0065
                                      VALUE 'LMSPCON '.                 CI0065
            10         S1-AA10-CCOM   PICTURE X VALUE '*'.              CI0065
            10          S-AA10-CCOD   PICTURE X(5)                      CI0065
                                      VALUE '-----'.                    CI0065
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0065
       01            S-AAU10-SSA.                                       CI0065
            11      S1-AAU10-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'LMSPCON '.                 CI0065
            11      S1-AAU10-CCOM   PICTURE X VALUE '*'.                CI0065
            11       S-AAU10-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            11      S1-AAU10-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(LMSPCONK'.                CI0065
            11       S-AAU10-OPER  PICTURE XX VALUE ' ='.               CI0065
            11       S-AAU10-ALCIDN   PICTURE  9(11).                   CI0065
            11  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01               S-AA85-SSA.                                     CI0065
            10         S1-AA85-SEGNAM PICTURE X(8)                      CI0065
                                      VALUE 'LMSPUWG '.                 CI0065
            10         S1-AA85-CCOM   PICTURE X VALUE '*'.              CI0065
            10          S-AA85-CCOD   PICTURE X(5)                      CI0065
                                      VALUE '-----'.                    CI0065
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0065
       01               S-CT01-SSA.                                     CI0065
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0065
                                      VALUE 'CT01    '.                 CI0065
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0065
            10          S-CT01-CCOD   PICTURE X(5)                      CI0065
                                      VALUE '-----'.                    CI0065
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0065
       01            S-CTU01-SSA.                                       CI0065
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CT01    '.                 CI0065
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0065
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CT01K'.                   CI0065
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0065
            10       S-CTU01-CT01K.                                     CI0065
            11       S-CTU01-C299.                                      CI0065
            12       S-CTU01-CTID.                                      CI0065
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0065
            13       S-CTU01-CTIDN.                                     CI0065
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0065
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0065
            10  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01               S-CT22-SSA.                                     CI0065
            10         S1-CT22-SEGNAM PICTURE X(8)                      CI0065
                                      VALUE 'CT22    '.                 CI0065
            10         S1-CT22-CCOM   PICTURE X VALUE '*'.              CI0065
            10          S-CT22-CCOD   PICTURE X(5)                      CI0065
                                      VALUE '-----'.                    CI0065
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0065
       01            S-CTU22-SSA.                                       CI0065
            10      S1-CTU22-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CT22    '.                 CI0065
            10      S1-CTU22-CCOM   PICTURE X VALUE '*'.                CI0065
            10       S-CTU22-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            10      S1-CTU22-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CT22K'.                   CI0065
            10       S-CTU22-OPER  PICTURE XX VALUE ' ='.               CI0065
            10       S-CTU22-CT22K.                                     CI0065
            11       S-CTU22-CGVEN    PICTURE  X(2).                    CI0065
            11       S-CTU22-CTWHC    PICTURE  9(2).                    CI0065
            10  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01               S-CX01-SSA.                                     CI0065
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0065
                                      VALUE 'CX01    '.                 CI0065
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0065
            10          S-CX01-CCOD   PICTURE X(5)                      CI0065
                                      VALUE '-----'.                    CI0065
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0065
       01            S-CXU01-SSA.                                       CI0065
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX01    '.                 CI0065
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0065
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CX01K'.                   CI0065
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0065
            10       S-CXU01-CX01K.                                     CI0065
            11       S-CXU01-C199.                                      CI0065
            12       S-CXU01-CLID.                                      CI0065
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0065
            13       S-CXU01-CLIDN.                                     CI0065
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0065
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0065
            10  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01               S-CX03-SSA.                                     CI0065
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0065
                                      VALUE 'CX03    '.                 CI0065
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0065
            10          S-CX03-CCOD   PICTURE X(5)                      CI0065
                                      VALUE '-----'.                    CI0065
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0065
       01            S-CXA03-SSA.                                       CI0065
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX03    '.                 CI0065
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0065
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CARTY'.                   CI0065
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0065
            12       S-CXA03-CARTY    PICTURE  99.                      CI0065
            12  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXB03-SSA.                                       CI0065
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX03    '.                 CI0065
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0065
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(NARRS'.                   CI0065
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0065
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            12  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXC03-SSA.                                       CI0065
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX03    '.                 CI0065
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0065
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CPMTG'.                   CI0065
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0065
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0065
            11  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXD03-SSA.                                       CI0065
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX03    '.                 CI0065
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0065
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(GRCRNG'.                  CI0065
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0065
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0065
            11  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXE03-SSA.                                       CI0065
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX03    '.                 CI0065
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0065
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(DEXDT'.                   CI0065
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0065
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0065
            11  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXF03-SSA.                                       CI0065
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX03    '.                 CI0065
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0065
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CY50'.                    CI0065
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0065
            11       S-CXF03-CY50.                                      CI0065
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0065
            11  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXG03-SSA.                                       CI0065
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX03    '.                 CI0065
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0065
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(NBASQ'.                   CI0065
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0065
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXH03-SSA.                                       CI0065
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX03    '.                 CI0065
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0065
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(NARID'.                   CI0065
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0065
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0065
            12  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXU03-SSA.                                       CI0065
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX03    '.                 CI0065
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0065
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CX03K'.                   CI0065
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0065
            11       S-CXU03-CX03K.                                     CI0065
            12       S-CXU03-CARTY    PICTURE  99.                      CI0065
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01               S-CX06-SSA.                                     CI0065
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0065
                                      VALUE 'CX06    '.                 CI0065
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0065
            10          S-CX06-CCOD   PICTURE X(5)                      CI0065
                                      VALUE '-----'.                    CI0065
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0065
       01            S-CXU06-SSA.                                       CI0065
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX06    '.                 CI0065
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0065
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CX06K'.                   CI0065
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0065
            10       S-CXU06-CX06K.                                     CI0065
            11       S-CXU06-C299.                                      CI0065
            12       S-CXU06-CTID.                                      CI0065
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0065
            13       S-CXU06-CTIDN.                                     CI0065
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0065
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0065
            10  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01               S-CX12-SSA.                                     CI0065
            10         S1-CX12-SEGNAM PICTURE X(8)                      CI0065
                                      VALUE 'CX12    '.                 CI0065
            10         S1-CX12-CCOM   PICTURE X VALUE '*'.              CI0065
            10          S-CX12-CCOD   PICTURE X(5)                      CI0065
                                      VALUE '-----'.                    CI0065
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0065
       01            S-CXA12-SSA.                                       CI0065
            10      S1-CXA12-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX12    '.                 CI0065
            10      S1-CXA12-CCOM   PICTURE X VALUE '*'.                CI0065
            10       S-CXA12-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            10      S1-CXA12-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CDEST'.                   CI0065
            10       S-CXA12-OPER  PICTURE XX VALUE ' ='.               CI0065
            10       S-CXA12-CDEST    PICTURE  99.                      CI0065
            10  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXB12-SSA.                                       CI0065
            10      S1-CXB12-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX12    '.                 CI0065
            10      S1-CXB12-CCOM   PICTURE X VALUE '*'.                CI0065
            10       S-CXB12-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            10      S1-CXB12-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(DNPMT'.                   CI0065
            10       S-CXB12-OPER  PICTURE XX VALUE ' ='.               CI0065
            10       S-CXB12-DNPMT    PICTURE  9(8).                    CI0065
            10  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXC12-SSA.                                       CI0065
            11      S1-CXC12-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX12    '.                 CI0065
            11      S1-CXC12-CCOM   PICTURE X VALUE '*'.                CI0065
            11       S-CXC12-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            11      S1-CXC12-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(NAPDS'.                   CI0065
            11       S-CXC12-OPER  PICTURE XX VALUE ' ='.               CI0065
            11       S-CXC12-NAPDS    PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXU12-SSA.                                       CI0065
            10      S1-CXU12-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX12    '.                 CI0065
            10      S1-CXU12-CCOM   PICTURE X VALUE '*'.                CI0065
            10       S-CXU12-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            10      S1-CXU12-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CX12K'.                   CI0065
            10       S-CXU12-OPER  PICTURE XX VALUE ' ='.               CI0065
            10       S-CXU12-CX12K.                                     CI0065
            11       S-CXU12-CPMTC    PICTURE  99.                      CI0065
            11       S-CXU12-NAPDS    PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11       S-CXU12-GESTD    PICTURE  9(8).                    CI0065
            10  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01               S-CX13-SSA.                                     CI0065
            10         S1-CX13-SEGNAM PICTURE X(8)                      CI0065
                                      VALUE 'CX13    '.                 CI0065
            10         S1-CX13-CCOM   PICTURE X VALUE '*'.              CI0065
            10          S-CX13-CCOD   PICTURE X(5)                      CI0065
                                      VALUE '-----'.                    CI0065
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0065
       01            S-CXA13-SSA.                                       CI0065
            11      S1-CXA13-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX13    '.                 CI0065
            11      S1-CXA13-CCOM   PICTURE X VALUE '*'.                CI0065
            11       S-CXA13-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            11      S1-CXA13-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CDEST'.                   CI0065
            11       S-CXA13-OPER  PICTURE XX VALUE ' ='.               CI0065
            11       S-CXA13-CDEST    PICTURE  99.                      CI0065
            11  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXB13-SSA.                                       CI0065
            12      S1-CXB13-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX13    '.                 CI0065
            12      S1-CXB13-CCOM   PICTURE X VALUE '*'.                CI0065
            12       S-CXB13-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            12      S1-CXB13-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CARTZ'.                   CI0065
            12       S-CXB13-OPER  PICTURE XX VALUE ' ='.               CI0065
            12       S-CXB13-CARTZ    PICTURE  99.                      CI0065
            12  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXC13-SSA.                                       CI0065
            12      S1-CXC13-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX13    '.                 CI0065
            12      S1-CXC13-CCOM   PICTURE X VALUE '*'.                CI0065
            12       S-CXC13-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            12      S1-CXC13-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(NAPDS'.                   CI0065
            12       S-CXC13-OPER  PICTURE XX VALUE ' ='.               CI0065
            12       S-CXC13-NAPDS    PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            12  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXU13-SSA.                                       CI0065
            11      S1-CXU13-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX13    '.                 CI0065
            11      S1-CXU13-CCOM   PICTURE X VALUE '*'.                CI0065
            11       S-CXU13-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            11      S1-CXU13-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CX13K'.                   CI0065
            11       S-CXU13-OPER  PICTURE XX VALUE ' ='.               CI0065
            11       S-CXU13-CX13K.                                     CI0065
            12       S-CXU13-CARTZ    PICTURE  99.                      CI0065
            12       S-CXU13-NAPDS    PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CX113-SSA.                                       CI0065
            11      S1-CX113-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX13    '.                 CI0065
            11      S1-CX113-CCOM   PICTURE X VALUE '*'.                CI0065
            11       S-CX113-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            11      S1-CX113-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(XGCUSPY'.                 CI0065
            11       S-CX113-OPER  PICTURE XX VALUE ' ='.               CI0065
            11       S-CX113-GCUSPY   PICTURE  X(12).                   CI0065
            11  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01               S-CX14-SSA.                                     CI0065
            10         S1-CX14-SEGNAM PICTURE X(8)                      CI0065
                                      VALUE 'CX14    '.                 CI0065
            10         S1-CX14-CCOM   PICTURE X VALUE '*'.              CI0065
            10          S-CX14-CCOD   PICTURE X(5)                      CI0065
                                      VALUE '-----'.                    CI0065
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0065
       01            S-CXU14-SSA.                                       CI0065
            10      S1-CXU14-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX14    '.                 CI0065
            10      S1-CXU14-CCOM   PICTURE X VALUE '*'.                CI0065
            10       S-CXU14-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            10      S1-CXU14-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CX14K'.                   CI0065
            10       S-CXU14-OPER  PICTURE XX VALUE ' ='.               CI0065
            10       S-CXU14-CX14K.                                     CI0065
            11       S-CXU14-NPISQ    PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CX114-SSA.                                       CI0065
            11      S1-CX114-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX14    '.                 CI0065
            11      S1-CX114-CCOM   PICTURE X VALUE '*'.                CI0065
            11       S-CX114-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            11      S1-CX114-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(XGCUSPZ'.                 CI0065
            11       S-CX114-OPER  PICTURE XX VALUE ' ='.               CI0065
            11       S-CX114-GCUSPZ   PICTURE  X(12).                   CI0065
            11  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01               S-CX2Y-SSA.                                     CI0065
            10         S1-CX2Y-SEGNAM PICTURE X(8)                      CI0065
                                      VALUE 'CX2Y    '.                 CI0065
            10         S1-CX2Y-CCOM   PICTURE X VALUE '*'.              CI0065
            10          S-CX2Y-CCOD   PICTURE X(5)                      CI0065
                                      VALUE '-----'.                    CI0065
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0065
       01            S-CXA2Y-SSA.                                       CI0065
            11      S1-CXA2Y-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX2Y    '.                 CI0065
            11      S1-CXA2Y-CCOM   PICTURE X VALUE '*'.                CI0065
            11       S-CXA2Y-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            11      S1-CXA2Y-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CARTY'.                   CI0065
            11       S-CXA2Y-OPER  PICTURE XX VALUE ' ='.               CI0065
            11       S-CXA2Y-CARTY    PICTURE  99.                      CI0065
            11  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXB2Y-SSA.                                       CI0065
            11      S1-CXB2Y-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX2Y    '.                 CI0065
            11      S1-CXB2Y-CCOM   PICTURE X VALUE '*'.                CI0065
            11       S-CXB2Y-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            11      S1-CXB2Y-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(C299'.                    CI0065
            11       S-CXB2Y-OPER  PICTURE XX VALUE ' ='.               CI0065
            11       S-CXB2Y-C299.                                      CI0065
            12       S-CXB2Y-CTID.                                      CI0065
            13       S-CXB2Y-CTIDA    PICTURE  9(3).                    CI0065
            13       S-CXB2Y-CTIDN.                                     CI0065
            14       S-CXB2Y-CTIDNP   PICTURE  X(13).                   CI0065
            14       S-CXB2Y-CTIDND   PICTURE  9(11).                   CI0065
            11  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01            S-CXU2Y-SSA.                                       CI0065
            10      S1-CXU2Y-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX2Y    '.                 CI0065
            10      S1-CXU2Y-CCOM   PICTURE X VALUE '*'.                CI0065
            10       S-CXU2Y-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            10      S1-CXU2Y-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CX2YK'.                   CI0065
            10       S-CXU2Y-OPER  PICTURE XX VALUE ' ='.               CI0065
            10       S-CXU2Y-CX2YK.                                     CI0065
            11       S-CXU2Y-C299.                                      CI0065
            12       S-CXU2Y-CTID.                                      CI0065
            13       S-CXU2Y-CTIDA    PICTURE  9(3).                    CI0065
            13       S-CXU2Y-CTIDN.                                     CI0065
            14       S-CXU2Y-CTIDNP   PICTURE  X(13).                   CI0065
            14       S-CXU2Y-CTIDND   PICTURE  9(11).                   CI0065
            11       S-CXU2Y-C199.                                      CI0065
            12       S-CXU2Y-CLID.                                      CI0065
            13       S-CXU2Y-CLIDO    PICTURE  9(3).                    CI0065
            13       S-CXU2Y-CLIDN.                                     CI0065
            14       S-CXU2Y-CLIDNP   PICTURE  X(12).                   CI0065
            14       S-CXU2Y-CLIDND   PICTURE  9(8).                    CI0065
            11       S-CXU2Y-CARTY    PICTURE  99.                      CI0065
            11       S-CXU2Y-NARRS    PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01               S-CX6Y-SSA.                                     CI0065
            10         S1-CX6Y-SEGNAM PICTURE X(8)                      CI0065
                                      VALUE 'CX6Y    '.                 CI0065
            10         S1-CX6Y-CCOM   PICTURE X VALUE '*'.              CI0065
            10          S-CX6Y-CCOD   PICTURE X(5)                      CI0065
                                      VALUE '-----'.                    CI0065
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0065
       01            S-CXU6Y-SSA.                                       CI0065
            10      S1-CXU6Y-SEGNAM PICTURE X(8)                        CI0065
                                      VALUE 'CX6Y    '.                 CI0065
            10      S1-CXU6Y-CCOM   PICTURE X VALUE '*'.                CI0065
            10       S-CXU6Y-CCOD   PICTURE X(5)                        CI0065
                                      VALUE '-----'.                    CI0065
            10      S1-CXU6Y-FLDNAM PICTURE X(9)                        CI0065
                                      VALUE '(CX6YK'.                   CI0065
            10       S-CXU6Y-OPER  PICTURE XX VALUE ' ='.               CI0065
            10       S-CXU6Y-CX6YK.                                     CI0065
            11       S-CXU6Y-C299.                                      CI0065
            12       S-CXU6Y-CTID.                                      CI0065
            13       S-CXU6Y-CTIDA    PICTURE  9(3).                    CI0065
            13       S-CXU6Y-CTIDN.                                     CI0065
            14       S-CXU6Y-CTIDNP   PICTURE  X(13).                   CI0065
            14       S-CXU6Y-CTIDND   PICTURE  9(11).                   CI0065
            11       S-CXU6Y-C199.                                      CI0065
            12       S-CXU6Y-CLID.                                      CI0065
            13       S-CXU6Y-CLIDO    PICTURE  9(3).                    CI0065
            13       S-CXU6Y-CLIDN.                                     CI0065
            14       S-CXU6Y-CLIDNP   PICTURE  X(12).                   CI0065
            14       S-CXU6Y-CLIDND   PICTURE  9(8).                    CI0065
            11       S-CXU6Y-CARTY    PICTURE  99.                      CI0065
            11       S-CXU6Y-NARRS    PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11       S-CXU6Y-CTID1    PICTURE  X(27).                   CI0065
            11       S-CXU6Y-CARTZ    PICTURE  99.                      CI0065
            11       S-CXU6Y-NAPDS    PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11       S-CXU6Y-NPISQ    PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10  FILLER   PICTURE X    VALUE ')'.                        CI0065
       01   ZONES-UTILISATEUR PICTURE X.                                CI0065
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
      ** PCB POINTER FOR LM1P                                           ADU015
            05 PCB-LM1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR ARAY                                           ADU015
            05 PCB-ARAY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LUVP                                           ADU015
            05 PCB-LUVP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LH1P                                           ADU015
            05 PCB-LH1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LARP                                           ADU015
            05 PCB-LARP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LPDP                                           ADU015
            05 PCB-LPDP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AREY                                           ADU015
            05 PCB-AREY-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=XA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XA00.                                         CI0065
          05              XA00-SUITE.                                   CI0065
            15       FILLER         PICTURE  X(00106).                  CI0065
       01                 XA06  REDEFINES      XA00.                    CI0065
            10            XA06-XDBPCB.                                  CI0065
            11            XA06-XDBDNM PICTURE  X(08).                   CI0065
            11            XA06-XSEGLV PICTURE  X(02).                   CI0065
            11            XA06-XRC    PICTURE  X(02).                   CI0065
            11            XA06-XPROPT PICTURE  X(04).                   CI0065
            11            XA06-FILLER PICTURE  S9(5)                    CI0065
                          BINARY.                                       CI0065
            11            XA06-XSEGNM PICTURE  X(08).                   CI0065
            11            XA06-XKEYLN PICTURE  S9(05)                   CI0065
                          BINARY.                                       CI0065
            11            XA06-XSEGNB PICTURE  9(05)                    CI0065
                          BINARY.                                       CI0065
            11            XA06-XCOKEY PICTURE  X(70).                   CI0065
      *** PCB MASK FOR LM1P                                             ADU015
      *!WF DSP=XB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XB00.                                         CI0065
          05              XB00-SUITE.                                   CI0065
            15       FILLER         PICTURE  X(00106).                  CI0065
       01                 XB06  REDEFINES      XB00.                    CI0065
            10            XB06-XDBPCB.                                  CI0065
            11            XB06-XDBDNM PICTURE  X(08).                   CI0065
            11            XB06-XSEGLV PICTURE  X(02).                   CI0065
            11            XB06-XRC    PICTURE  X(02).                   CI0065
            11            XB06-XPROPT PICTURE  X(04).                   CI0065
            11            XB06-FILLER PICTURE  S9(5)                    CI0065
                          BINARY.                                       CI0065
            11            XB06-XSEGNM PICTURE  X(08).                   CI0065
            11            XB06-XKEYLN PICTURE  S9(05)                   CI0065
                          BINARY.                                       CI0065
            11            XB06-XSEGNB PICTURE  9(05)                    CI0065
                          BINARY.                                       CI0065
            11            XB06-XCOKEY PICTURE  X(70).                   CI0065
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=XC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XC00.                                         CI0065
          05              XC00-SUITE.                                   CI0065
            15       FILLER         PICTURE  X(00106).                  CI0065
       01                 XC06  REDEFINES      XC00.                    CI0065
            10            XC06-XDBPCB.                                  CI0065
            11            XC06-XDBDNM PICTURE  X(08).                   CI0065
            11            XC06-XSEGLV PICTURE  X(02).                   CI0065
            11            XC06-XRC    PICTURE  X(02).                   CI0065
            11            XC06-XPROPT PICTURE  X(04).                   CI0065
            11            XC06-FILLER PICTURE  S9(5)                    CI0065
                          BINARY.                                       CI0065
            11            XC06-XSEGNM PICTURE  X(08).                   CI0065
            11            XC06-XKEYLN PICTURE  S9(05)                   CI0065
                          BINARY.                                       CI0065
            11            XC06-XSEGNB PICTURE  9(05)                    CI0065
                          BINARY.                                       CI0065
            11            XC06-XCOKEY PICTURE  X(70).                   CI0065
      *** PCB MASK FOR ARAY                                             ADU015
      *!WF DSP=XD DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XD00.                                         CI0065
          05              XD00-SUITE.                                   CI0065
            15       FILLER         PICTURE  X(00106).                  CI0065
       01                 XD06  REDEFINES      XD00.                    CI0065
            10            XD06-XDBPCB.                                  CI0065
            11            XD06-XDBDNM PICTURE  X(08).                   CI0065
            11            XD06-XSEGLV PICTURE  X(02).                   CI0065
            11            XD06-XRC    PICTURE  X(02).                   CI0065
            11            XD06-XPROPT PICTURE  X(04).                   CI0065
            11            XD06-FILLER PICTURE  S9(5)                    CI0065
                          BINARY.                                       CI0065
            11            XD06-XSEGNM PICTURE  X(08).                   CI0065
            11            XD06-XKEYLN PICTURE  S9(05)                   CI0065
                          BINARY.                                       CI0065
            11            XD06-XSEGNB PICTURE  9(05)                    CI0065
                          BINARY.                                       CI0065
            11            XD06-XCOKEY PICTURE  X(70).                   CI0065
      *** PCB MASK FOR LUVP                                             ADU015
      *!WF DSP=XE DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XE00.                                         CI0065
          05              XE00-SUITE.                                   CI0065
            15       FILLER         PICTURE  X(00106).                  CI0065
       01                 XE06  REDEFINES      XE00.                    CI0065
            10            XE06-XDBPCB.                                  CI0065
            11            XE06-XDBDNM PICTURE  X(08).                   CI0065
            11            XE06-XSEGLV PICTURE  X(02).                   CI0065
            11            XE06-XRC    PICTURE  X(02).                   CI0065
            11            XE06-XPROPT PICTURE  X(04).                   CI0065
            11            XE06-FILLER PICTURE  S9(5)                    CI0065
                          BINARY.                                       CI0065
            11            XE06-XSEGNM PICTURE  X(08).                   CI0065
            11            XE06-XKEYLN PICTURE  S9(05)                   CI0065
                          BINARY.                                       CI0065
            11            XE06-XSEGNB PICTURE  9(05)                    CI0065
                          BINARY.                                       CI0065
            11            XE06-XCOKEY PICTURE  X(70).                   CI0065
      *** PCB MASK FOR LH1P                                             ADU015
      *!WF DSP=XF DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XF00.                                         CI0065
          05              XF00-SUITE.                                   CI0065
            15       FILLER         PICTURE  X(00106).                  CI0065
       01                 XF06  REDEFINES      XF00.                    CI0065
            10            XF06-XDBPCB.                                  CI0065
            11            XF06-XDBDNM PICTURE  X(08).                   CI0065
            11            XF06-XSEGLV PICTURE  X(02).                   CI0065
            11            XF06-XRC    PICTURE  X(02).                   CI0065
            11            XF06-XPROPT PICTURE  X(04).                   CI0065
            11            XF06-FILLER PICTURE  S9(5)                    CI0065
                          BINARY.                                       CI0065
            11            XF06-XSEGNM PICTURE  X(08).                   CI0065
            11            XF06-XKEYLN PICTURE  S9(05)                   CI0065
                          BINARY.                                       CI0065
            11            XF06-XSEGNB PICTURE  9(05)                    CI0065
                          BINARY.                                       CI0065
            11            XF06-XCOKEY PICTURE  X(70).                   CI0065
      *** PCB MASK FOR LARP                                             ADU015
      *!WF DSP=XG DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XG00.                                         CI0065
          05              XG00-SUITE.                                   CI0065
            15       FILLER         PICTURE  X(00106).                  CI0065
       01                 XG06  REDEFINES      XG00.                    CI0065
            10            XG06-XDBPCB.                                  CI0065
            11            XG06-XDBDNM PICTURE  X(08).                   CI0065
            11            XG06-XSEGLV PICTURE  X(02).                   CI0065
            11            XG06-XRC    PICTURE  X(02).                   CI0065
            11            XG06-XPROPT PICTURE  X(04).                   CI0065
            11            XG06-FILLER PICTURE  S9(5)                    CI0065
                          BINARY.                                       CI0065
            11            XG06-XSEGNM PICTURE  X(08).                   CI0065
            11            XG06-XKEYLN PICTURE  S9(05)                   CI0065
                          BINARY.                                       CI0065
            11            XG06-XSEGNB PICTURE  9(05)                    CI0065
                          BINARY.                                       CI0065
            11            XG06-XCOKEY PICTURE  X(70).                   CI0065
      *** PCB MASK FOR LPDP                                             ADU015
      *!WF DSP=XH DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XH00.                                         CI0065
          05              XH00-SUITE.                                   CI0065
            15       FILLER         PICTURE  X(00106).                  CI0065
       01                 XH06  REDEFINES      XH00.                    CI0065
            10            XH06-XDBPCB.                                  CI0065
            11            XH06-XDBDNM PICTURE  X(08).                   CI0065
            11            XH06-XSEGLV PICTURE  X(02).                   CI0065
            11            XH06-XRC    PICTURE  X(02).                   CI0065
            11            XH06-XPROPT PICTURE  X(04).                   CI0065
            11            XH06-FILLER PICTURE  S9(5)                    CI0065
                          BINARY.                                       CI0065
            11            XH06-XSEGNM PICTURE  X(08).                   CI0065
            11            XH06-XKEYLN PICTURE  S9(05)                   CI0065
                          BINARY.                                       CI0065
            11            XH06-XSEGNB PICTURE  9(05)                    CI0065
                          BINARY.                                       CI0065
            11            XH06-XCOKEY PICTURE  X(70).                   CI0065
      *** PCB MASK FOR AREY                                             ADU015
      *!WF DSP=XI DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XI00.                                         CI0065
          05              XI00-SUITE.                                   CI0065
            15       FILLER         PICTURE  X(00106).                  CI0065
       01                 XI06  REDEFINES      XI00.                    CI0065
            10            XI06-XDBPCB.                                  CI0065
            11            XI06-XDBDNM PICTURE  X(08).                   CI0065
            11            XI06-XSEGLV PICTURE  X(02).                   CI0065
            11            XI06-XRC    PICTURE  X(02).                   CI0065
            11            XI06-XPROPT PICTURE  X(04).                   CI0065
            11            XI06-FILLER PICTURE  S9(5)                    CI0065
                          BINARY.                                       CI0065
            11            XI06-XSEGNM PICTURE  X(08).                   CI0065
            11            XI06-XKEYLN PICTURE  S9(05)                   CI0065
                          BINARY.                                       CI0065
            11            XI06-XSEGNB PICTURE  9(05)                    CI0065
                          BINARY.                                       CI0065
            11            XI06-XCOKEY PICTURE  X(70).                   CI0065
      ******************************************************************
      ** SEGMENT THAT WILL BE PASSED BACK TO CALLING ROGRAM.           *
      ******************************************************************
      *!WF DSP=PX DSL=V2 SEL=74 FOR=I DES=1 LEV=1 PLT=10
       01                 PX74.                                         CI0065
            10            PX74-MAPPN  PICTURE  X(10).                   CI0065
            10            PX74-NSSSI  PICTURE  X(24).                   CI0065
            10            PX74-CLID4  PICTURE  X(23).                   CI0065
            10            PX74-CLID7  PICTURE  X(23).                   CI0065
            10            PX74-CLID01 PICTURE  X(23).                   CI0065
            10            PX74-CLTIN  PICTURE  9(12).                   CI0065
            10            PX74-CLTINV PICTURE  9(12).                   CI0065
            10            PX74-NSEQ5  PICTURE  9(5).                    CI0065
            10            PX74-CTID.                                    CI0065
            11            PX74-CTIDA  PICTURE  9(3).                    CI0065
            11            PX74-CTIDN.                                   CI0065
            12            PX74-CTIDNP PICTURE  X(13).                   CI0065
            12            PX74-CTIDND PICTURE  9(11).                   CI0065
            10            PX74-GECKD1 PICTURE  9.                       CI0065
            10            PX74-AACTV  PICTURE  S9(11)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-DVALU  PICTURE  9(8).                    CI0065
            10            PX74-DVALU1 PICTURE  9(8).                    CI0065
            10            PX74-CTTLN1 PICTURE  X(30).                   CI0065
            10            PX74-CTTLN2 PICTURE  X(30).                   CI0065
            10            PX74-CTTLN3 PICTURE  X(30).                   CI0065
            10            PX74-CTTBO1 PICTURE  X(45).                   CI0065
            10            PX74-CTTBO2 PICTURE  X(45).                   CI0065
            10            PX74-PRCMN  PICTURE  X(20).                   CI0065
            10            PX74-PRCSN  PICTURE  X(9).                    CI0065
            10            PX74-PRCSN2 PICTURE  X(9).                    CI0065
            10            PX74-CTID01.                                  CI0065
            11            PX74-CACTID PICTURE  9(3).                    CI0065
            11            PX74-CTIDNB.                                  CI0065
            12            PX74-CTIDP1 PICTURE  X(13).                   CI0065
            12            PX74-CTIDNA PICTURE  9(11).                   CI0065
            10            PX74-GECKD2 PICTURE  9.                       CI0065
            10            PX74-CATLN1 PICTURE  X(30).                   CI0065
            10            PX74-CATLN2 PICTURE  X(30).                   CI0065
            10            PX74-CATLN3 PICTURE  X(30).                   CI0065
            10            PX74-CATBO1 PICTURE  X(45).                   CI0065
            10            PX74-CATBO2 PICTURE  X(45).                   CI0065
            10            PX74-PRCMN1 PICTURE  X(20).                   CI0065
            10            PX74-GESAD1 PICTURE  X(30).                   CI0065
            10            PX74-GESAD2 PICTURE  X(30).                   CI0065
            10            PX74-GESAD3 PICTURE  X(30).                   CI0065
            10            PX74-GECIT  PICTURE  X(25).                   CI0065
            10            PX74-GEST   PICTURE  X(8).                    CI0065
            10            PX74-GEPCD  PICTURE  X(12).                   CI0065
            10            PX74-CLORN  PICTURE  X(45).                   CI0065
            10            PX74-NTR    PICTURE  9(8).                    CI0065
            10            PX74-GECKD3 PICTURE  9.                       CI0065
            10            PX74-NPBN   PICTURE  X(20).                   CI0065
            10            PX74-TTBAL  PICTURE  X(15).                   CI0065
            10            PX74-MCSIG  PICTURE  X(30).                   CI0065
            10            PX74-TWITH  PICTURE  X(12).                   CI0065
            10            PX74-IAIND  PICTURE  X.                       CI0065
            10            PX74-IAIND5 PICTURE  X.                       CI0065
            10            PX74-GESTE  PICTURE  9(8).                    CI0065
            10            PX74-GESTL  PICTURE  9(8).                    CI0065
            10            PX74-MPMTT1 PICTURE  X(20).                   CI0065
            10            PX74-MPMTT2 PICTURE  X(20).                   CI0065
            10            PX74-MPMTF1 PICTURE  X(24).                   CI0065
            10            PX74-MPMTF2 PICTURE  X(24).                   CI0065
            10            PX74-MPMTF3 PICTURE  X(24).                   CI0065
            10            PX74-MPMTF4 PICTURE  X(24).                   CI0065
            10            PX74-MPMTF5 PICTURE  X(24).                   CI0065
            10            PX74-MPMTF6 PICTURE  X(24).                   CI0065
            10            PX74-MPMTF7 PICTURE  X(24).                   CI0065
            10            PX74-MPMTF8 PICTURE  X(24).                   CI0065
            10            PX74-ACOTL1 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ACOTL2 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ACOTL3 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ACOTL4 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ACOTL5 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ACOTL6 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ACOTL7 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ACOTL8 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ACOTU1 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ACOTU2 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ACOTU3 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ACOTU4 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ACOTU5 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ACOTU6 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ACOTU7 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ACOTU8 PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-AMINAL PICTURE  S9(7)V99.                CI0065
            10            PX74-AMAXAL PICTURE  S9(7)V99.                CI0065
            10            PX74-APMTD  PICTURE  S9(11)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-MPMTT  PICTURE  X(20).                   CI0065
            10            PX74-MPMTF  PICTURE  X(14).                   CI0065
            10            PX74-DNPMT  PICTURE  9(8).                    CI0065
            10            PX74-GEEND  PICTURE  9(8).                    CI0065
            10            PX74-MRPSN  PICTURE  X(12).                   CI0065
            10            PX74-MRPSN3 PICTURE  X(12).                   CI0065
            10            PX74-CARST  PICTURE  99.                      CI0065
            10            PX74-CQACT  PICTURE  999.                     CI0065
            10            PX74-CQACT1 PICTURE  999.                     CI0065
            10            PX74-GRID.                                    CI0065
            11            PX74-GRIDC  PICTURE  9(3).                    CI0065
            11            PX74-GRIDN  PICTURE  9(10).                   CI0065
            10            PX74-GRIDA.                                   CI0065
            11            PX74-GRIDCB PICTURE  9(3).                    CI0065
            11            PX74-GRIDNB PICTURE  9(10).                   CI0065
            10            PX74-PRCOD  PICTURE  9(5).                    CI0065
            10            PX74-PRCOD1 PICTURE  9(5).                    CI0065
            10            PX74-PRSCD  PICTURE  X(9).                    CI0065
            10            PX74-CPRSC2 PICTURE  X(9).                    CI0065
            10            PX74-CTSTA  PICTURE  99.                      CI0065
            10            PX74-CTSTAD PICTURE  99.                      CI0065
            10            PX74-CPRDG  PICTURE  9(2).                    CI0065
            10            PX74-CPRDGA PICTURE  9(2).                    CI0065
            10            PX74-CPRDA  PICTURE  9(3).                    CI0065
            10            PX74-CPRDA1 PICTURE  9(3).                    CI0065
            10            PX74-IMNPR  PICTURE  X.                       CI0065
            10            PX74-IMNPRA PICTURE  X.                       CI0065
            10            PX74-MPRN4  PICTURE  X(35).                   CI0065
            10            PX74-MPRN4X PICTURE  X(100).                  CI0065
            10            PX74-MPRN4B PICTURE  X(35).                   CI0065
            10            PX74-MPRN4Y PICTURE  X(100).                  CI0065
            10            PX74-CIRAT  PICTURE  999.                     CI0065
            10            PX74-CIRAT2 PICTURE  999.                     CI0065
            10            PX74-CIRAS  PICTURE  999.                     CI0065
            10            PX74-CIRAS2 PICTURE  999.                     CI0065
            10            PX74-IVANT  PICTURE  X(1).                    CI0065
            10            PX74-IVAEX  PICTURE  X.                       CI0065
            10            PX74-NSEQ5A PICTURE  9(5).                    CI0065
            10            PX74-NSEQ5B PICTURE  9(5).                    CI0065
            10            PX74-ALBL   PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-CELBL  PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-CIRAP1 PICTURE  XX.                      CI0065
            10            PX74-AACTVF PICTURE  S9(7)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-CDETY  PICTURE  XX.                      CI0065
            10            PX74-CDEST  PICTURE  99.                      CI0065
            10            PX74-ALPLDT PICTURE  9(8).                    CI0065
            10            PX74-CLDOB  PICTURE  9(8).                    CI0065
            10            PX74-CLDOB1 PICTURE  9(8).                    CI0065
            10            PX74-CTCUS  PICTURE  999.                     CI0065
            10            PX74-CLCUS  PICTURE  99.                      CI0065
            10            PX74-ASURR  PICTURE  S9(07)V99                CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ATERF  PICTURE  S9(5)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ALOAD  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ATFRA  PICTURE  S9(9)V99                 CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-ITRAL  PICTURE  X(1).                    CI0065
            10            PX74-CTCUS2 PICTURE  999.                     CI0065
            10            PX74-CTTYPG PICTURE  X(04).                   CI0065
            10            PX74-CPORTA PICTURE  X.                       CI0065
            10            PX74-CIRAP  PICTURE  XX.                      CI0065
            10            PX74-DEFFT  PICTURE  9(8).                    CI0065
            10            PX74-NARRS  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-CARTZ  PICTURE  99.                      CI0065
            10            PX74-NAPDS  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-NPISQ  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-NBASQ  PICTURE  S9(3)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            PX74-CPMTC  PICTURE  99.                      CI0065
            10            PX74-CPMTF  PICTURE  99.                      CI0065
            10            PX74-IGRACV PICTURE  X.                       CI0065
            10            PX74-CACTA  PICTURE  X(1).                    CI0065
            10            PX74-ALDDUE PICTURE  9(08).                   CI0065
            10            PX74-MCLNM5 PICTURE  X(45).                   CI0065
            10            PX74-IDRMD  PICTURE  X.                       CI0065
            10            PX74-FILLER PICTURE  X(127).                  CI0065
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0065
          05              DE00-SUITE.                                   CI0065
            15       FILLER         PICTURE  X(00653).                  CI0065
       01                 DE10  REDEFINES      DE00.                    CI0065
            10            DE10-DU11.                                    CI0065
            11            DE10-XFONC  PICTURE  X(4).                    CI0065
            11            DE10-MPSBN  PICTURE  X(8).                    CI0065
            11            DE10-XDBDNM PICTURE  X(08).                   CI0065
            11            DE10-XSEGNM PICTURE  X(08).                   CI0065
            11            DE10-XRC    PICTURE  X(02).                   CI0065
            11            DE10-MSEG   PICTURE  X(08).                   CI0065
            11            DE10-XCOKEY PICTURE  X(70).                   CI0065
            11            DE10-CUIBR  PICTURE  X(01).                   CI0065
            11            DE10-CUIBA  PICTURE  X(01).                   CI0065
            11            DE10-IPBIK  PICTURE  X(1).                    CI0065
            10            DE10-DU03.                                    CI0065
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            DE10-CMSSF  PICTURE  XX.                      CI0065
            11            DE10-DU09.                                    CI0065
            12            DE10-CMESA  PICTURE  S9(9)                    CI0065
                          BINARY.                                       CI0065
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0065
                          BINARY.                                       CI0065
            12            DE10-CMESB  PICTURE  S9(9)                    CI0065
                          BINARY.                                       CI0065
            12            DE10-CMSST  PICTURE  S9(9)                    CI0065
                          BINARY.                                       CI0065
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0065
                          BINARY.                                       CI0065
            12            DE10-QELLAA PICTURE  S9(9)                    CI0065
                          BINARY.                                       CI0065
            12            DE10-TMESS4 PICTURE  X(512).                  CI0065
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
       01                 MS00.                                         CI0065
          05              MS00-SUITE.                                   CI0065
            15       FILLER         PICTURE  X(00542).                  CI0065
       01                 MS03  REDEFINES      MS00.                    CI0065
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            10            MS03-CMSSF  PICTURE  XX.                      CI0065
            10            MS03-DU09.                                    CI0065
            11            MS03-CMESA  PICTURE  S9(9)                    CI0065
                          BINARY.                                       CI0065
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0065
                          BINARY.                                       CI0065
            11            MS03-CMESB  PICTURE  S9(9)                    CI0065
                          BINARY.                                       CI0065
            11            MS03-CMSST  PICTURE  S9(9)                    CI0065
                          BINARY.                                       CI0065
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0065
                          BINARY.                                       CI0065
            11            MS03-QELLAA PICTURE  S9(9)                    CI0065
                          BINARY.                                       CI0065
            11            MS03-TMESS4 PICTURE  X(512).                  CI0065
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0065
            10            MX11-QMSGS  PICTURE  9(03).                   CI0065
            10            MX11-PJ09                                     CI0065
                          OCCURS       025     TIMES.                   CI0065
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0065
                          COMPUTATIONAL-3.                              CI0065
            11            MX11-CMESB  PICTURE  S9(9)                    CI0065
                          BINARY.                                       CI0065
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PX74
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
       F0TSC.                                                           lv10
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF XA06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR LM1P                                             DOT
           SET ADDRESS OF XB06 TO                                       ADU015
                PCB-LM1P-PTR1.                                          ADU015
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF XC06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR ARAY                                             DOT
           SET ADDRESS OF XD06 TO                                       ADU015
                PCB-ARAY-PTR1.                                          ADU015
      *SET ADDRESS FOR LUVP                                             DOT
           SET ADDRESS OF XE06 TO                                       ADU015
                PCB-LUVP-PTR1.                                          ADU015
      *SET ADDRESS FOR LH1P                                             DOT
           SET ADDRESS OF XF06 TO                                       ADU015
                PCB-LH1P-PTR1.                                          ADU015
      *SET ADDRESS FOR LARP                                             DOT
           SET ADDRESS OF XG06 TO                                       ADU015
                PCB-LARP-PTR1.                                          ADU015
      *SET ADDRESS FOR LPDP                                             DOT
           SET ADDRESS OF XH06 TO                                       ADU015
                PCB-LPDP-PTR1.                                          ADU015
      *SET ADDRESS FOR AREY                                             DOT
           SET ADDRESS OF XI06 TO                                       ADU015
                PCB-AREY-PTR1.                                          ADU015
       F0TSC-FN. EXIT.
      *N01.      NOTE *************************************.            CI0065
      *               *                                   *             CI0065
      *               *INITIALISATIONS                    *             CI0065
      *               *                                   *             CI0065
      *               *************************************.            CI0065
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0065
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0065
      *               *                                   *             CI0065
      *               *FIN DE TRAITEMENT                  *             CI0065
      *               *                                   *             CI0065
      *               *************************************.            CI0065
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0065
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *GET VALID VALUES FOR SOURCE -      *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *DESTINATION COMBINATION.
      *********************************
           MOVE        'N' TO PX74-IAIND
           PX74-IAIND5
           PX74-IGRACV
           PX74-ITRAL.
      *N40AB.    NOTE *FOR TRAD LIFE ACCOUNTS SET THE     *.
       F40AB.    IF    (PX74-CPRDGA = 04)                               lv10
                 AND   (PX74-CPRDA1 = 401 OR 403
                       OR 404)
                 AND   ((PX74-PRCOD1 NOT > 199)
                 OR    (PX74-PRCOD1 > 299
                 AND   PX74-PRCOD1 < 599))
                 NEXT SENTENCE ELSE GO TO     F40AB-FN.
      *RESPECTIVE FLAG.
      *TRAD LIFE ACCOUNTS HAVE SEPERATE
      *CALCULATIONS FOR START DATE,
      *AMOUNT AND FREQUENCY.
      *********************************
           MOVE        'Y' TO PX74-ITRAL.
       F40AB-FN. EXIT.
      *N40AE.    NOTE *FOR INS AS DEST CALL PRODUCT       *.
       F40AE.    IF    (PX74-CACTID = 004 OR 005                        lv10
                 AND   PX74-PRCOD1 < 600)
                 NEXT SENTENCE ELSE GO TO     F40AE-FN.
      *MODULE TO GET THE DUE DATE AND
      *THE POLICY ANNIVERSARY DATE.
      *********************************
           INITIALIZE  7-82-WORK-DATE 7-82-ANN-DATE.
                 IF    PX74-ITRAL = 'Y'                                 DOT
      *IF IT IS A TRAD POLICY THEN CALL
      *THE TRAD PRODUCT DRIVER - CI0140
           PERFORM     F97JB THRU F97JB-FN.
                 IF    PX74-IVANT = 'Y'                                 DOT
      *IF IT IS VANTAGE THEN CALL
      *VANTAGE PRODUCT DRIVER - CI0141
           PERFORM     F97KB THRU F97KB-FN.
       F40AE-FN. EXIT.
      *N40BB.    NOTE *GET THE WITHHOLDING PERCENTAGE     *.
       F40BB.                                                           lv10
      *********************************
      *N40BF.    NOTE *TAKE THE SOURCE ACCOUNT ID,        *.
       F40BF.    IF    PX74-CTIDA = (001 OR 002)                        lv15
                 AND   PX74-CQACT NOT = ZEROS
                 AND   (PX74-CQACT1 = 000
                 OR    PX74-CDETY = 02 OR 03 OR 04
                       OR 05 OR 06)
                 NEXT SENTENCE ELSE GO TO     F40BF-FN.
      *ACCESS CT22 AND GET THE PERIODIC
      *WITHHOLDING PERCENTAGE.
      *********************************
      *WITHHOLDING IS APPLICABLE ONLY
      *FOR QUALIFIED SOURCE AND NON
      *QUALIFIED DESTINATION.
      *********************************
           INITIALIZE  WS00-PWHLD
           MOVE        PX74-CTID TO S-CTU01-CTID
           MOVE        'US' TO S-CTU22-CGVEN
           MOVE        02 TO S-CTU22-CTWHC
           PERFORM     F94AB THRU F94AB-FN.
                 IF    IK = '1'                                         DOT
           MOVE        'N' TO PX74-IAIND
           MOVE        '010.000' TO PX74-TWITH
                 ELSE
           MOVE        'Y' TO PX74-IAIND
           MOVE        CT22-CTWHP TO WS00-PWHLD
           MOVE        WS00-PWHLDX TO PX74-TWITH.
       F40BF-FN. EXIT.
      *N40BG.    NOTE *FOR BROKERAGE AS SOURCE, THE       *.
       F40BG.    IF    PX74-CTIDA = (021 OR 133)                        lv15
                 AND   PX74-NSEQ5 = ZEROES
                 AND   PX74-CQACT NOT = ZEROS
                 AND   (PX74-CQACT1 = 000
                 OR    PX74-CDETY = 02 OR 03 OR 04
                       OR 05 OR 06)
                 NEXT SENTENCE ELSE GO TO     F40BG-FN.
      *WITHHOLDING PERCENT WILL ALWAYS
      *BE 10 FOR ADD NEW SPO
      *********************************
      *WITHHOLDING IS APPLICABLE ONLY
      *FOR QUALIFIED SOURCE AND NON
      *QUALIFIED DESTINATION.
      *********************************
           MOVE        '010.000' TO PX74-TWITH
           MOVE        'Y' TO PX74-IAIND.
       F40BG-FN. EXIT.
      *N40BH.    NOTE *IF IT IS A TRANSFER FROM A         *.
       F40BH.    IF    PX74-CQACT = 000                                 lv15
                 OR    PX74-CQACT1 NOT = ZEROS
                 NEXT SENTENCE ELSE GO TO     F40BH-FN.
      *NON - QUALIFIED ACCOUNT OR INTO
      *A QUALIFIED ACCOUNT THEN
      *WITHHOLDING WILL NOT BE
      *APPLICABLE
      *********************************
           MOVE        'N/A' TO PX74-TWITH.
       F40BH-FN. EXIT.
      *N40BI.    NOTE *SET WITHHOLDING FOR ANNT SOURCE    *.
       F40BI.    IF    PX74-CTIDA = (004 OR 005)                        lv15
                 AND   PX74-CPRDA = 405
                 NEXT SENTENCE ELSE GO TO     F40BI-FN.
      *WITHHOLDING PERCENT WILL ALWAYS
      *BE 10 IF THE SOURCE AND DEST
      *NOT BOTH ARE QUALIFIED ACCT
           INITIALIZE  IK.
                 IF    PX74-CQACT NOT = ZEROS                           DOT
                 AND   PX74-CQACT1 NOT = ZEROS
           MOVE        'N/A' TO PX74-TWITH
           MOVE        'N' TO PX74-IAIND
                 ELSE
           INITIALIZE  WS00-PWHLD WS00-IK
           MOVE        PX74-CTID TO S-CTU01-CTID
           MOVE        'US' TO S-CTU22-CGVEN
           MOVE        03 TO S-CTU22-CTWHC
           MOVE        'Y' TO WS00-IK
           PERFORM     F94AB THRU F94AB-FN.
                 IF    IK = '1'                                         DOT
                 AND   WS00-IK = 'Y'
      *IF FAILED TO READ CT22
           MOVE        'N' TO PX74-IAIND
           MOVE        'N' TO WS00-IK
           MOVE        '010.000' TO PX74-TWITH.
                 IF    IK = '0'                                         DOT
                 AND   WS00-IK = 'Y'
      *IF READ CT22 SUCCESSFUL
           MOVE        'Y' TO PX74-IAIND
           MOVE        CT22-CTWHP TO WS00-PWHLD
           MOVE        WS00-PWHLDX TO PX74-TWITH.
       F40BI-FN. EXIT.
       F40BB-FN. EXIT.
      *N40BJ.    NOTE *CALL CI0020 - CAMS ACCTG DATE      *.            AM0020
       F40BJ.                                                           lv10
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
      *N40BN.    NOTE *NON-DL1 ERROR                      *.            ADU070
       F40BN.    IF    MS03-NMESS2 > ZERO                               lv15
                 AND   MS03-CMESB > 10                                  ADU070
                 NEXT SENTENCE ELSE GO TO     F40BN-FN.                 ADU070
      *OF A CERTAIN SEVERITY                                            ADU070
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU070
           MOVE        CI0020 TO MS03-TMESS4 (IMS03R : 6)               ADU070
           ADD         +7 TO MS03-QELLAA                                ADU070
           MOVE                     ALL '1' TO FT GO TO F20.            ADU070
       F40BN-900. GO TO F40BR-FN.
       F40BN-FN. EXIT.
      *N40BR.    NOTE *NO ERRORS                          *.            ADU070
       F40BR.                                                           lv15
           INITIALIZE  MS03.                                            ADU070
       F40BR-FN. EXIT.
       F40BJ-FN. EXIT.
      *N40BV.    NOTE *CALCULATE THE EARLIEST & LATEST    *.
       F40BV.                                                           lv10
      *START DATE.
      *********************************
      *EARLIEST START DATE IS ONE
      *BUSINESS DAY FROM THE CURRENT
      *BUSINESS DATE. FOR ACTIVE
      *PORTFOLIO AS SOURCE IT IS 6
      *BUSINESS DAYS FROM CURRENT DATE
      *********************************
           INITIALIZE  7-CNT-DATE 7-CURR-DATE
           7-REQD-DATE
           MOVE        1 TO WS00-DAYS
           PERFORM     F91JB THRU F91JB-FN.
                 IF    PX74-CTIDA = (021 OR 133)                        DOT
                 AND   PX74-PRCOD = 00022
      *FOR ACTIVE PORTFOLIOS
           MOVE        7 TO WS00-DAYS
           PERFORM     F91JB THRU F91JB-FN.
      *LATEST START DATE IS 10 MONTHS                                   DOT
      *FROM CURRENT DATE.
           MOVE        10 TO WS00-MTHS
           MOVE        NS20-DCACG TO WS00-CCYYMMDD
           PERFORM     F91JC THRU F91JC-FN.
      *N40BX.    NOTE *FOR INS AS DEST THE LATEST START   *.
       F40BX.    IF    (PX74-CACTID = 004 OR 005                        lv15
                 AND   PX74-PRCOD1 < 600)
                 NEXT SENTENCE ELSE GO TO     F40BX-FN.
      *DATE DEPENDS ON THE GRACE PERIOD
      *********************************
           INITIALIZE  WS00-CCYYMMDD.
                 IF    PX74-ITRAL = 'Y'                                 DOT
      *IF IT IS A TRAD POLICY THEN
      *LATEST START DATE IS 1 MONTH
      *FROM DUE DATE.
           MOVE        WE40-ALDDUE TO WS00-CCYYMMDD
           PX74-ALDDUE
           MOVE        1 TO WS00-MTHS
           PERFORM     F91JC THRU F91JC-FN.
                 IF    PX74-IVANT = 'Y'                                 DOT
                 AND   WF41-CASTA = 'P'
      *IF IT IS A VANTAGE POLICY THEN
      *LATEST START DATE IS 2 MONTHS
      *FROM DUE DATE
           MOVE        'Y' TO PX74-IGRACV
           MOVE        WF41-ALDDUE TO WS00-CCYYMMDD
           MOVE        2 TO WS00-MTHS
           PERFORM     F91JC THRU F91JC-FN.
       F40BX-FN. EXIT.
       F40BV-FN. EXIT.
      *N40CB.    NOTE *GET THE VALID PAYMENT TYPES        *.
       F40CB.    IF    PX74-CTID01 IS NUMERIC                           lv10
                 AND   PX74-CTID01 > ZEROS
                 NEXT SENTENCE ELSE GO TO     F40CB-FN.
      *BASED ON THE DESTINATION ACCOUNT
      *********************************
           MOVE        SPACES TO PX74-MPMTT1
           MOVE        SPACES TO PX74-MPMTT2.
      *N40CD.    NOTE *READ TA6A TABLE TO CHECK THE       *.
       F40CD.                                                           lv15
      *VALID PAYMENT TYPES THAT CAN BE
      *SET UP ON THE PRODUCT.
      *********************************
           MOVE        PX74-CACTID TO TA6A-CTIDA
           MOVE        PX74-PRCOD1 TO TA6A-PRCOD
           PERFORM     F92TA THRU F92TA-FN.
       F40CD-FN. EXIT.
      *N40CF.    NOTE *FOR FUNDS/FP AND ANNUITY AS DEST   *.
       F40CF.    IF    PX74-CACTID = 002                                lv15
                 OR    ((PX74-CACTID = 004 OR 005)
                 AND   PX74-PRCOD1 >= 600)
                 OR    (PX74-CACTID = 013
                 AND   (PX74-PRCOD1 = 00026
                       OR 00080))
                 AND   TA6A-IARRGA = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40CF-FN.
      *ONLY PAYMENT TYPE 'REGULAR' IS
      *ALLOWED.
           MOVE        'REGULAR' TO PX74-MPMTT1.
       F40CF-FN. EXIT.
      *N40CH.    NOTE *FOR CERTIFICATES AND INSURANCE     *.
       F40CH.    IF    PX74-CACTID = 001                                lv15
                 OR    ((PX74-CACTID = 004 OR 005)
                 AND   PX74-PRCOD1 < 600)
                 NEXT SENTENCE ELSE GO TO     F40CH-FN.
      *AS DESTINATION, PAYMENT TYPE
      *'LOAN' IS ALSO ALLOWED PROVIDED
      *THE LOAN BALANCE > 0.
                 IF    TA6A-IARRGA = 'Y'                                DOT
           MOVE        'REGULAR' TO PX74-MPMTT1.
                 IF    TA6A-IARLNA = 'Y'                                DOT
                 AND   PX74-CELBL > 0
           MOVE        'LOAN' TO PX74-MPMTT2.
       F40CH-FN. EXIT.
      *N40CI.    NOTE *READ ARRANGEMENT DB VIA CX2Y       *.
       F40CI.    IF    (PX74-CPRDGA = 04)                               lv15
                 AND   (PX74-CPRDA1 = 401 OR 403
                       OR 404)
                 AND   ((PX74-PRCOD1 NOT > 199)
                 OR    (PX74-PRCOD1 > 299
                 AND   PX74-PRCOD1 < 599))
                 NEXT SENTENCE ELSE GO TO     F40CI-FN.
      *FOR TRAD DESTINATION ACCOUNTS
           MOVE        LOW-VALUES TO S-CXU2Y-CX2YK
           MOVE        PX74-CTID01 TO S-CXU2Y-CTID
           MOVE        'GE' TO S-CXU2Y-OPER
      *GU >= ON CX2Y
           PERFORM     F94AI THRU F94AI-FN.
      *N40CK.    NOTE *PROCESS ARRANGEMENTS MATCHIN ON    *.
       F40CK.    IF    IK = '0'                                         lv20
                 AND   CX2Y-CTID = PX74-CTID01
                 NEXT SENTENCE ELSE GO TO     F40CK-FN.
      *THE ACCOUNT NUMBER
      *N40CM.    NOTE *GET CX03                           *.
       F40CM.    IF    CX2Y-CARTY = 01                                  lv25
                 NEXT SENTENCE ELSE GO TO     F40CM-FN.
           MOVE        CX2Y-CLID TO S-CXU01-CX01K
      *READ THE CX03 ARRANGEMENT INFO
           MOVE        CX2Y-CARTY TO S-CXU03-CARTY
           MOVE        CX2Y-NARRS TO S-CXU03-NARRS
           PERFORM     F94AJ THRU F94AJ-FN.
                 IF    IK = '1'                                         DOT
      *CX03 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012007 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N40CO.    NOTE *GET FIRST CX06                     *.
       F40CO.    IF    CX2Y-CARTY = 01                                  lv30
                 NEXT SENTENCE ELSE GO TO     F40CO-FN.
           INITIALIZE  CX06
           MOVE        CX2Y-CTID TO S-CXU06-CX06K
           PERFORM     F94AK THRU F94AK-FN.
       F40CO-FN. EXIT.
      *N40CQ.    NOTE *GET CX12 - BA SETUP                *.
       F40CQ.    IF    CX2Y-CARTY = 01                                  lv30
                 NEXT SENTENCE ELSE GO TO     F40CQ-FN.
           INITIALIZE  CX12
           MOVE        CX2Y-CTID TO S-CXU06-CX06K
           PERFORM     F94AL THRU F94AL-FN.
                 IF    IK = '1'                                         DOT
      *SEGMENT NOT FOUND. RETURN ERROR
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013413 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N40CR.    NOTE *LOOP THROUGH ALL THE ACCOUNTS      *.
       F40CR.    IF    IK = '0'                                         lv35
                 NEXT SENTENCE ELSE GO TO     F40CR-FN.
      *N40CS.    NOTE *IF AN ALREADY EXISTING PREMIUM     *.
       F40CS.    IF    CX12-CPMTC = 00                                  lv40
                 AND   CX12-CDEST = 01
                 AND   CX12-CPMTF NOT = 99
                 NEXT SENTENCE ELSE GO TO     F40CS-FN.
      *BA EXISTS A PREMIUM SPO CANNOT
      *BE SETUP
           MOVE        SPACES TO PX74-MPMTT1
               GO TO     F40CI-FN.
       F40CS-FN. EXIT.
      *N40CT.    NOTE *GET NEXT CX12                      *.
       F40CT.                                                           lv40
           INITIALIZE  CX12
           PERFORM     F94AL THRU F94AL-FN.
       F40CT-FN. EXIT.
       F40CR-900. GO TO F40CR.
       F40CR-FN. EXIT.
       F40CQ-FN. EXIT.
       F40CM-FN. EXIT.
      *N40CU.    NOTE *GET NEXT CX2Y                      *.
       F40CU.                                                           lv25
           INITIALIZE  CX2Y
           PERFORM     F94AM THRU F94AM-FN.
       F40CU-FN. EXIT.
       F40CK-900. GO TO F40CK.
       F40CK-FN. EXIT.
       F40CI-FN. EXIT.
      *N40CV.    NOTE *READ ARRANGEMENT DB VIA CX6Y       *.
       F40CV.    IF    (PX74-CPRDGA = 04)                               lv15
                 AND   (PX74-CPRDA1 = 401 OR 403
                       OR 404)
                 AND   ((PX74-PRCOD1 NOT > 199)
                 OR    (PX74-PRCOD1 > 299
                 AND   PX74-PRCOD1 < 599))
                 NEXT SENTENCE ELSE GO TO     F40CV-FN.
      *FOR TRAD ACCOUNTS
           INITIALIZE  CX6Y
           MOVE        PX74-CTID01 TO S-CXU6Y-CTID
           MOVE        ZEROES TO S-CXU6Y-CLID
           MOVE        '>=' TO S-CXU6Y-OPER
      *DO A GU CALL ON CX6Y
           PERFORM     F94AV THRU F94AV-FN.
      *N40CW.    NOTE *PROCESS ARRANGEMENTS MATCHING      *.
       F40CW.    IF    IK = '0'                                         lv20
                 AND   CX6Y-CTID = PX74-CTID01
                 NEXT SENTENCE ELSE GO TO     F40CW-FN.
      *ON THE ACCOUNT NUMBER
      *N40CX.    NOTE *GET CX13                           *.
       F40CX.    IF    CX6Y-CARTY = 10                                  lv25
                 NEXT SENTENCE ELSE GO TO     F40CX-FN.
           MOVE        CX6Y-CLID TO S-CXU01-CX01K
           MOVE        CX6Y-CARTY TO S-CXU03-CARTY
           MOVE        CX6Y-NARRS TO S-CXU03-NARRS
           MOVE        CX6Y-CTID1 TO S-CXU06-CTID
           MOVE        CX6Y-CARTZ TO S-CXU13-CARTZ
           MOVE        CX6Y-NAPDS TO S-CXU13-NAPDS
      *DO A GU ON CX13
           PERFORM     F94AR THRU F94AR-FN.
      *N40CY.    NOTE *IF CX13 FOUND                      *.
       F40CY.    IF    IK = '0'                                         lv30
                 NEXT SENTENCE ELSE GO TO     F40CY-FN.
      *N40CZ.    NOTE *GET NEXT CX14 FOR ARRANGEMENT      *.
       F40CZ.    IF    CX13-CDEST = 01                                  lv35
                 NEXT SENTENCE ELSE GO TO     F40CZ-FN.
           INITIALIZE  CX14
           MOVE        CX13-CARTZ TO S-CXU13-CARTZ
           MOVE        CX13-NAPDS TO S-CXU13-NAPDS
           PERFORM     F94AP THRU F94AP-FN.
      *N40DA.    NOTE *IF AN ALREADY EXISTING PREMIUM     *.
       F40DA.    IF    CX14-CPMTC = 00                                  lv40
                 AND   IK = '0'
                 AND   PX74-MPMTT NOT = 'REGULAR'
                 NEXT SENTENCE ELSE GO TO     F40DA-FN.
      *SPO EXISTS A PREMIUM SPO CANNOT
      *BE SETUP
           MOVE        SPACES TO PX74-MPMTT1
               GO TO     F40CV-FN.
       F40DA-FN. EXIT.
       F40CZ-FN. EXIT.
       F40CY-FN. EXIT.
       F40CX-FN. EXIT.
      *N40DB.    NOTE *READ NEXT CX6Y                     *.
       F40DB.                                                           lv25
           INITIALIZE  CX6Y
           PERFORM     F94AT THRU F94AT-FN.
       F40DB-FN. EXIT.
       F40CW-900. GO TO F40CW.
       F40CW-FN. EXIT.
       F40CV-FN. EXIT.
      *N40DC.    NOTE *FOR BROK AS DESTINATION THE ONLY   *.
       F40DC.    IF    PX74-CACTID = 021 OR 133                         lv15
                 NEXT SENTENCE ELSE GO TO     F40DC-FN.
      *POSSIBLE PAYMENT TYPE IS 'CASH
      *DEPOSIT'
      *********************************
           MOVE        'CASH DEPOSIT' TO PX74-MPMTT1
           MOVE        SPACES TO PX74-MPMTT2.
       F40DC-FN. EXIT.
       F40CB-FN. EXIT.
      *N40DD.    NOTE *FOR ACH, AOR,CHECK TO SPL PAYEE,   *.
       F40DD.    IF    PX74-CTID01 = SPACES                             lv10
                 OR    PX74-CDETY = ('02' OR '03'
                       OR '04' OR '05' OR '06')
                 NEXT SENTENCE ELSE GO TO     F40DD-FN.
      *CHECK TO OWNER AT ALT ADDR AND
      *CHECK TO BANK DESTINATION THE
      *PAYMENT TYPE IS 'N/A'
      *********************************
           MOVE        'N/A' TO PX74-MPMTT1
           MOVE        'N/A' TO PX74-MPMTT2.
       F40DD-FN. EXIT.
      *N40DE.    NOTE *GET THE VALID FREQUENCIES          *.
       F40DE.                                                           lv10
      *BASED ON THE SRC/DEST ACCOUNT
      *********************************
      *N40DF.    NOTE *THE VALID FREQUENCIES              *.
       F40DF.                                                           lv15
      *********************************
           MOVE        'Weekly' TO PX74-MPMTF1
           MOVE        'Bi-weekly' TO PX74-MPMTF2
           MOVE        'Semi-monthly' TO PX74-MPMTF3
           MOVE        'Monthly' TO PX74-MPMTF4
           MOVE        'Bi-monthly' TO PX74-MPMTF5
           MOVE        'Quarterly' TO PX74-MPMTF6
           MOVE        'Semi-annual' TO PX74-MPMTF7
           MOVE        'Annual' TO PX74-MPMTF8.
       F40DF-FN. EXIT.
      *N40DJ.    NOTE *FOR VANTAGE INSURANCE PRODUCT AS   *.
       F40DJ.    IF    (PX74-CACTID = 004 OR 005)                       lv15
                 AND   PX74-PRCOD1 < 600
                 AND   PX74-IVANT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40DJ-FN.
      *DESTINATION.
      *********************************
           MOVE        'Monthly' TO PX74-MPMTF1
           MOVE        'Quarterly' TO PX74-MPMTF2
           MOVE        'Semi-annual' TO PX74-MPMTF3
           MOVE        'Annual' TO PX74-MPMTF4
           MOVE        SPACES TO PX74-MPMTF5
           PX74-MPMTF6
           PX74-MPMTF7
           PX74-MPMTF8.
       F40DJ-FN. EXIT.
      *N40DN.    NOTE *FOR TRAD INSURANCE PRODUCT AS      *.
       F40DN.    IF    (PX74-CACTID = 004 OR 005)                       lv15
                 AND   PX74-PRCOD1 < 600
                 AND   PX74-ITRAL = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40DN-FN.
      *DESTINATION CHECK IF THE FREQ
      *ARE ALLOWED.
      *********************************
           MOVE        SPACES TO PX74-MPMTF1
           PX74-MPMTF2
           PX74-MPMTF3
           PX74-MPMTF4
           PERFORM     F80DB THRU F80DB-FN
           MOVE        'Monthly' TO PX74-MPMTF1.
                 IF    WS00-QT-OK = 'Y'                                 DOT
           MOVE        'Quarterly' TO PX74-MPMTF2.
                 IF    WS00-SA-OK = 'Y'                                 DOT
           MOVE        'Semi-annual' TO PX74-MPMTF3.
                 IF    WS00-AN-OK = 'Y'                                 DOT
           MOVE        'Annual' TO PX74-MPMTF4.
           MOVE        SPACES TO PX74-MPMTF5                            DOT
           PX74-MPMTF6
           PX74-MPMTF7
           PX74-MPMTF8.
       F40DN-FN. EXIT.
      *N40DQ.    NOTE *FOR BROKERAGE ACTIVE PORTFOLIO     *.
       F40DQ.    IF    PX74-CTIDA = (021 OR 133)                        lv15
                 AND   PX74-PRCOD = 00022
                 NEXT SENTENCE ELSE GO TO     F40DQ-FN.
      *PRODUCT AS SOURCE.
      *********************************
           MOVE        'Monthly' TO PX74-MPMTF1
           MOVE        'Quarterly' TO PX74-MPMTF2
           MOVE        'Semi-annual' TO PX74-MPMTF3
           MOVE        'Annual' TO PX74-MPMTF4
           MOVE        SPACES TO PX74-MPMTF5
           PX74-MPMTF6
           PX74-MPMTF7
           PX74-MPMTF8.
       F40DQ-FN. EXIT.
      *N40DT.    NOTE *RESET FREQUENCIES FOR ANNT SPO     *.
       F40DT.    IF    PX74-CTIDA = (004 OR 005)                        lv15
                 AND   PX74-CPRDA = 405
                 AND   PX74-ITRAL NOT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40DT-FN.
      *********************************
           MOVE        'Weekly' TO PX74-MPMTF1
           MOVE        'Bi-weekly' TO PX74-MPMTF2
           MOVE        'Semi-monthly' TO PX74-MPMTF3
           MOVE        'Monthly' TO PX74-MPMTF4
           MOVE        'Bi-monthly' TO PX74-MPMTF5
           MOVE        'Quarterly' TO PX74-MPMTF6
           MOVE        'Semi-annual' TO PX74-MPMTF7
           MOVE        'Annual' TO PX74-MPMTF8.
       F40DT-FN. EXIT.
       F40DE-FN. EXIT.
      *N40EB.    NOTE *GET THE MIN & MAX AMOUNTS FOR      *.
       F40EB.                                                           lv10
      *REGULAR TYPE OF PAYMENT.
      *********************************
      *N40EF.    NOTE *THE MINIMUM AMOUNT IS $100. AND    *.
       F40EF.                                                           lv15
      *THE MAX AMOUNT IS $100000.00.
      *FOR CERT/FUND ACCOUNTS, OVERRIDE
      *THE MINIMUM AMOUNT TO $50 FOR
      *QUALIFIED TO QUALIFIED TRANSFER
      *WITHIN THE SAME PLAN.
      *FOR FP ACCOUNTS, OVERRIDE THE
      *MINIMUM AMOUNT TO $25.
      *FOR BETA ACTIVE PORTFOLIOS
      *ACCOUNTS, OVERRIDE THE MINIMUM
      *AMOUNT TO $50.
      *********************************
           MOVE        100 TO PX74-ACOTL1
           PX74-ACOTL2
           PX74-ACOTL3
           PX74-ACOTL4
           PX74-ACOTL5
           PX74-ACOTL6
           PX74-ACOTL7
           PX74-ACOTL8
           MOVE        100000.00 TO PX74-ACOTU1
           PX74-ACOTU2
           PX74-ACOTU3
           PX74-ACOTU4
           PX74-ACOTU5
           PX74-ACOTU6
           PX74-ACOTU7
           PX74-ACOTU8.
                 IF    PX74-CTIDA = (001 OR 002)                        DOT
                 AND   PX74-CQACT NOT = ZEROS
                 AND   PX74-CQACT1 NOT = ZEROES
                 AND   PX74-CIRAT = PX74-CIRAT2
                 AND   PX74-CIRAS = PX74-CIRAS2
           MOVE        50 TO PX74-ACOTL1
           PX74-ACOTL2
           PX74-ACOTL3
           PX74-ACOTL4
           PX74-ACOTL5
           PX74-ACOTL6
           PX74-ACOTL7
           PX74-ACOTL8.
                 IF    PX74-CACTID = 013                                DOT
                 AND   (PX74-PRCOD1 = 00026
                       OR 00080)
      *MINIMUM FOR FP PRODUCTS
           MOVE        25 TO PX74-ACOTL1
           PX74-ACOTL2
           PX74-ACOTL3
           PX74-ACOTL4
           PX74-ACOTL5
           PX74-ACOTL6
           PX74-ACOTL7
           PX74-ACOTL8.
                 IF    PX74-CTIDA = 133                                 DOT
                 AND   PX74-PRCOD = 00022
      *SET MINIMUM AMOUNT FOR BETA
      *ACTIVE PORTFOLIOS ACCOUNTS
           MOVE        50 TO PX74-ACOTL1
           PX74-ACOTL2
           PX74-ACOTL3
           PX74-ACOTL4
           PX74-ACOTL5
           PX74-ACOTL6
           PX74-ACOTL7
           PX74-ACOTL8.
       F40EF-FN. EXIT.
      *N40EJ.    NOTE *FOR FUND AS DESTINATION WHEN       *.
       F40EJ.    IF    PX74-CACTID = 002                                lv15
                 AND   PX74-AACTVF < 2000
                 NEXT SENTENCE ELSE GO TO     F40EJ-FN.
      *ACCOUNT ACCOUNT BALANCE IS LESS
      *THAN $2000 THEN THE MIN VARIES
      *BASED ON THE FRQUENCY
      *WEEKLY, BI-WEEKLY, SEMI-MONTHLY
      *& MONTHLY = $100
      *BI-MONTHLY = $200
      *QAURTERLY = $300
      *SEMI-ANNUAL = $600
      *ANNUAL = $1200
      *********************************
           MOVE        100 TO PX74-ACOTL1
           PX74-ACOTL2
           PX74-ACOTL3
           PX74-ACOTL4
           MOVE        200 TO PX74-ACOTL5
           MOVE        300 TO PX74-ACOTL6
           MOVE        600 TO PX74-ACOTL7
           MOVE        1200 TO PX74-ACOTL8
           MOVE        'Y' TO PX74-IAIND5.
       F40EJ-FN. EXIT.
      *N40EN.    NOTE *FOR A FEW FUND PRODUCTS AS DEST    *.
       F40EN.    IF    PX74-CACTID = 002                                lv15
                 AND   (PX74-PRCOD1 = 102 OR 106
                       OR 108)
                 AND   (PX74-CTSTAD = 01 OR 03)
                 NEXT SENTENCE ELSE GO TO     F40EN-FN.
      *IF THE FUND IS INACTIVE OR
      *PENDING THEN THE MINIMUM INITIAL
      *PURCHASE AMOUNT WILL BE $5000
      *********************************
           MOVE        5000 TO PX74-ACOTL1
           PX74-ACOTL2
           PX74-ACOTL3
           PX74-ACOTL4
           PX74-ACOTL5
           PX74-ACOTL6
           PX74-ACOTL7
           PX74-ACOTL8
           MOVE        'Y' TO PX74-IAIND5.
       F40EN-FN. EXIT.
      *N40EP.    NOTE *FOR A FEW FUND PRODUCTS AS DEST    *.
       F40EP.    IF    PX74-CACTID = 002                                lv15
                 AND   (PX74-PRCOD1 = 107 OR 124
                       OR 125)
                 AND   (PX74-CTSTAD = 01 OR 03)
                 NEXT SENTENCE ELSE GO TO     F40EP-FN.
      *IF THE FUND IS INACTIVE OR
      *PENDING THEN THE MINIMUM INITIAL
      *PURCHASE AMOUNT WILL BE $10000
      *********************************
           MOVE        10000 TO PX74-ACOTL1
           PX74-ACOTL2
           PX74-ACOTL3
           PX74-ACOTL4
           PX74-ACOTL5
           PX74-ACOTL6
           PX74-ACOTL7
           PX74-ACOTL8
           MOVE        'Y' TO PX74-IAIND5.
       F40EP-FN. EXIT.
      *N40EQ.    NOTE *FOR ANNUITY SOURCE ACCOUNT         *.
       F40EQ.    IF    PX74-CTIDA = (004 OR 005)                        lv15
                 AND   PX74-CPRDA = 405
                 AND   PX74-ITRAL NOT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40EQ-FN.
      *THE MINIMUM AMOUNT IS $50
      *********************************
           MOVE        50 TO PX74-ACOTL1
           PX74-ACOTL2
           PX74-ACOTL3
           PX74-ACOTL4
           PX74-ACOTL5
           PX74-ACOTL6
           PX74-ACOTL7
           PX74-ACOTL8.
       F40EQ-FN. EXIT.
      *N40ER.    NOTE *TRADITIONAL LIFE ACCOUNTS HAVE     *.
       F40ER.    IF    PX74-ITRAL = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F40ER-FN.
      *EXACT PREMIUMS BASED ON
      *FREQUENCY.
      *FIRST ACCESS AA10 TO GET ANNUAL
      *GROSS AMOUNT.
      *********************************
           MOVE        PX74-CTIDNA TO S-AAU10-ALCIDN
           PERFORM     F94AE THRU F94AE-FN.
      *N40ET.    NOTE *AA10 NOT FOUND                     *.
       F40ET.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F40ET-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012265 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40ET-900. GO TO F40EW-FN.
       F40ET-FN. EXIT.
      *N40EW.    NOTE *IF AA10 FOUND THEN CALCULATE       *.
       F40EW.                                                           lv20
      *EXACT PREMIUM FOR EACH FREQUENCY
      *********************************
           MOVE        ZEROES TO PX74-ACOTL1
           PX74-ACOTU1
           PX74-ACOTL2
           PX74-ACOTU2
           PX74-ACOTL3
           PX74-ACOTU3
           PX74-ACOTL4
           PX74-ACOTU4
           PERFORM     F80BB THRU F80BB-FN
           MOVE        ACCT-MONTHLY TO PX74-ACOTL1
           PX74-ACOTU1.
                 IF    WS00-QT-OK = 'Y'                                 DOT
           MOVE        ACCT-QUARTERLY TO PX74-ACOTL2
           PX74-ACOTU2.
                 IF    WS00-SA-OK = 'Y'                                 DOT
           MOVE        ACCT-SEMIANNUAL TO PX74-ACOTL3
           PX74-ACOTU3.
                 IF    WS00-AN-OK = 'Y'                                 DOT
           MOVE        AA10-ALPAGR TO PX74-ACOTL4
           PX74-ACOTU4.
           MOVE        ZEROES TO PX74-ACOTL5                            DOT
           PX74-ACOTU5
           PX74-ACOTL6
           PX74-ACOTU6
           PX74-ACOTL7
           PX74-ACOTU7
           PX74-ACOTL8
           PX74-ACOTU8
           MOVE        'N' TO PX74-IAIND5.
       F40EW-FN. EXIT.
       F40ER-FN. EXIT.
       F40EB-FN. EXIT.
      *N40FB.    NOTE *GET THE MIN & MAX AMOUNTS FOR      *.
       F40FB.    IF    PX74-MPMTT2 = 'LOAN'                             lv10
                 NEXT SENTENCE ELSE GO TO     F40FB-FN.
      *LOAN TYPE OF PAYMENT.
      *********************************
      *N40FF.    NOTE *THE MINIMUM AMOUNT IS $0 FOR       *.
       F40FF.                                                           lv15
      *CERTS AS DESTINATION AND $20 FOR
      *INSURANCE AS DESTINATION.
      *THE MAX AMOUNT IS THE LOAN
      *BALANCE OR $100,000.00 WHICHEVER
      *IS LOWER.
      *********************************
           MOVE        20 TO PX74-AMINAL.
                 IF    PX74-CACTID = 001                                DOT
           MOVE        0 TO PX74-AMINAL.
           MOVE        PX74-CELBL TO PX74-AMAXAL.                       DOT
                 IF    PX74-CELBL > 100000.00                           DOT
           MOVE        100000.00 TO PX74-AMAXAL.
       F40FF-FN. EXIT.
       F40FB-FN. EXIT.
      *N40GG.    NOTE *SET PAYMENT TYPE FOR ANNUITY SPO   *.
       F40GG.    IF    PX74-CTIDA = (004 OR 005)                        lv10
                 AND   PX74-CPRDA = 405
                 NEXT SENTENCE ELSE GO TO     F40GG-FN.
                 IF    PX74-CACTID = 002                                DOT
                 AND   PX74-PRCOD1 = (13 OR 16)
      *REGULAR FOR FUND DEST ACCT
           MOVE        'REGULAR' TO PX74-MPMTT1.
                 IF    PX74-CACTID = (004 OR 005)                       DOT
                 AND   PX74-CPRDA = (401 OR 402
                       OR 403 OR 404)
      *REGULAR FOR INS AND TRAD ACCTS
           MOVE        'REGULAR' TO PX74-MPMTT1.
           MOVE        SPACES TO PX74-MPMTT2.                           DOT
       F40GG-FN. EXIT.
       F40-FN.   EXIT.
      *N79.      NOTE *************************************.            ADU102
      *               *                                   *             ADU102
      *               *---> Normal Termination            *             ADU102
      *               *                                   *             ADU102
      *               *************************************.            ADU102
       F79.                                                             lv05
      *     Return to Calling Module                                    ADU102
           MOVE                     ALL '1' TO FT GO TO F20.            ADU102
       F79-FN.   EXIT.
      *N80.      NOTE *************************************.
      *               *                                   *
      *               *MACRO AAOLBB CODE IS HERE          *
      *               *                                   *
      *               *************************************.
       F80.                                                             lv05
      *WITH APPROPRIATE MODIFICATIONS
      *N80BB.    NOTE *CHECK FOR FLORIDA DI POLICIES      *.
       F80BB.                                                           lv10
      *LOAD WS AND RESET SWITCHES
           MOVE        AA10-ALPAGR TO 7-82-PREMIUMS
           MOVE        7-82-ONE TO 7-82-ONES
           MOVE        'N' TO 7-82-FL-DI
      *SET SWITCH WHICH IS USED IN
      *LATER CALCULATIONS TO NOT CHARGE
      *MODAL PREMIUMS FOR FLORIDA DI
           MOVE        PX74-CTIDNA TO 7-82-ALCIDN
           MOVE        PX74-CPRSC2 TO 7-82-PRSCD.
                 IF    AA10-ALAPST = 15                                 DOT
                 AND   7-82-FIRST-4 = 9100
                 AND   7-82-DE-CODE = 22
                 AND   ((7-82-FGH-CODE = 160)
                 OR    (7-82-FGH-CODE < 140
                 OR    7-82-FGH-CODE > 184))
      *SET FLORIDA DI SWITCH
           MOVE        'Y' TO 7-82-FL-DI.
                 IF    PX74-PRCOD1 = 300                                DOT
                 AND   (7-82-FIRST-4 = 9000
                       OR 9700)
                 AND   7-82-DE-CODE = 19
                 AND   (7-82-FGH-CODE = 313
                       OR 316 OR 318)
      *SET 2012 NEW TERM SWITCH
           MOVE        'Y' TO 7-82-TL-2012
                 ELSE
      *IF NOT 2012 NEW TERM
           MOVE        'N' TO 7-82-TL-2012.
      *N80BE.    NOTE *CHECK POLICY DATE FOR FORMULA      *.
       F80BE.    IF    AA10-ALPLDT > 19661002                           lv20
                 NEXT SENTENCE ELSE GO TO     F80BE-FN.
           MOVE        ZERO TO AA85-CF
           MOVE        7-82-ALCIDN TO S-AAU10-ALCIDN
           PERFORM     F94AH THRU F94AH-FN.
                 IF    IK = ZERO                                        DOT
           MOVE        '1' TO AA85-CF.
      *N80BH.    NOTE *NEW CALCULATION  - FORMULA         *.
       F80BH.    IF    AA85-CF = '1'                                    lv25
                 AND   AA10-CPCAL = 1
                 NEXT SENTENCE ELSE GO TO     F80BH-FN.
      *MATCH LIC0025 LOGIC
      *DON'T CHARGE PREMIUM FOR FL DI
                 IF    7-82-FL-DI = 'Y'                                 DOT
           COMPUTE     ACCT-MONTHLY ROUNDED =
           (AA10-ALPAGR * 0.0868)
                 ELSE
           COMPUTE     ACCT-MONTHLY ROUNDED =
           ((AA10-ALPAGR * 0.0868) + 0.60).
                 IF    7-82-FL-DI = 'Y'                                 DOT
           COMPUTE     ACCT-QUARTERLY ROUNDED =
           (AA10-ALPAGR * 0.258)
                 ELSE
           COMPUTE     ACCT-QUARTERLY ROUNDED =
           ((AA10-ALPAGR * 0.258) + 0.50).
                 IF    7-82-FL-DI = 'Y'                                 DOT
           COMPUTE     ACCT-SEMIANNUAL ROUNDED =
           (AA10-ALPAGR * 0.502)
                 ELSE
           COMPUTE     ACCT-SEMIANNUAL ROUNDED =
           ((AA10-ALPAGR * 0.502) + 0.40).
                 IF    PX74-PRCOD1 = 310                                DOT
                 OR    7-82-TL-2012 = 'Y'
      *FOR IPE INSURED PRODUCTS
      *AND NEW TERM 2012
      *DON'T CHARGE PREMIUM FOR FL DI
           PERFORM     F80GB THRU F80GB-FN.
       F80BH-FN. EXIT.
      *N80BK.    NOTE *OLD CALCULATION - TABLE            *.
       F80BK.    IF    AA85-CF = ZERO                                   lv25
                 OR    AA10-CPCAL NOT = 1
                 NEXT SENTENCE ELSE GO TO     F80BK-FN.
      *MATCH LIC0025 LOGIC
      *MONTHLY
           COMPUTE     7-82-TENS =
           (AA10-ALPAGR - 7-82-ONE)
           COMPUTE     7-82-PREMIUM-T ROUNDED =
           (7-82-TENS * 0.0868)
           COMPUTE     7-82-PREMIUM-O ROUNDED =
           (7-82-CONES * 0.0868)
           COMPUTE     ACCT-MONTHLY =
           ((7-82-PREMIUM-T
           + 7-82-PREMIUM-O) + 0.60)
      *QUARTERLY
           COMPUTE     7-82-PREMIUM-T ROUNDED =
           (7-82-TENS * 0.258)
           COMPUTE     7-82-PREMIUM-O ROUNDED =
           (7-82-CONES * 0.258)
           COMPUTE     ACCT-QUARTERLY =
           ((7-82-PREMIUM-T
           + 7-82-PREMIUM-O) + 0.50)
      *SEMI-ANNUAL
           COMPUTE     7-82-PREMIUM-T ROUNDED =
           (7-82-TENS * 0.502)
           COMPUTE     7-82-PREMIUM-O ROUNDED =
           (7-82-CONES * 0.502)
           COMPUTE     ACCT-SEMIANNUAL =
           ((7-82-PREMIUM-T
           + 7-82-PREMIUM-O) + 0.40).
                 IF    PX74-PRCOD1 = 310                                DOT
                 OR    7-82-TL-2012 = 'Y'
      *FOR IPE INSURED PRODUCTS
      *AND NEW TERM 2012
           PERFORM     F80GB THRU F80GB-FN.
       F80BK-FN. EXIT.
       F80BE-900. GO TO F80BN-FN.
       F80BE-FN. EXIT.
      *N80BN.    NOTE *USE OLD PREMIUM CALC FORMULA       *.
       F80BN.                                                           lv20
      *MATCH LIC0025 LOGIC
           COMPUTE     ACCT-QUARTERLY ROUNDED =
           (AA10-ALPAGR * 0.260)
           COMPUTE     ACCT-MONTHLY ROUNDED =
           (AA10-ALPAGR * 0.0875)
           COMPUTE     ACCT-SEMIANNUAL ROUNDED =
           (AA10-ALPAGR * 0.510).
       F80BN-FN. EXIT.
       F80BB-FN. EXIT.
      *N80DB.    NOTE *CHECK IF THE FREQUENCY IS VALID    *.
       F80DB.                                                           lv10
      *FOR THE GIVEN POLICY ANNIVERSARY
      *DATE AND DUE DATE COMBINATION.
      *
      *CHECK IF QUARTERLY IS OK
           MOVE        WE40-ALDDUE TO 7-82-WORK-DATE
           MOVE        WE40-ALPLDT TO 7-82-ANN-DATE
           MOVE        'N' TO WS00-OK
           MOVE        3 TO WS00-DIV
           PERFORM     F80FB THRU F80FB-FN
           MOVE        WS00-OK TO WS00-QT-OK
      *CHECK IF SEMI-ANNUAL IS OK
           MOVE        WE40-ALDDUE TO 7-82-WORK-DATE
           MOVE        WE40-ALPLDT TO 7-82-ANN-DATE
           MOVE        'N' TO WS00-OK
           MOVE        6 TO WS00-DIV
           PERFORM     F80FB THRU F80FB-FN
           MOVE        WS00-OK TO WS00-SA-OK
      *CHECK IF ANNUAL IS OK
           MOVE        WE40-ALDDUE TO 7-82-WORK-DATE
           MOVE        WE40-ALPLDT TO 7-82-ANN-DATE
           MOVE        'N' TO WS00-OK.
                 IF    7-82-WORK-MM = 7-82-ANN-MM                       DOT
           MOVE        'Y' TO WS00-OK.
           MOVE        WS00-OK TO WS00-AN-OK.                           DOT
       F80DB-FN. EXIT.
      *N80FB.    NOTE *CHECK IF THE FREQUENCY IS VALID    *.
       F80FB.                                                           lv10
      *OR NOT.
      *TAKE THE DIFFERENCE BETWEEN THE
      *POLICY ANNIVERSARY MONTH AND DUE
      *MONTH, DIVIDE THAT BY NUMBER OF
      *MONTHS FOR THAT PARTICULAR FREQ.
      *IF THE REMAINDER IS ZERO THEN
      *THAT FREQUENCY IS VALID.
      *********************************
           MOVE        'N' TO WS00-OK.
                 IF    7-82-WORK-MM > 7-82-ANN-MM                       DOT
           SUBTRACT    7-82-ANN-MM FROM 7-82-WORK-MM
           DIVIDE      WS00-DIV INTO 7-82-WORK-MM GIVING
           7-82-RESULT REMAINDER 7-82-REM
                 ELSE
           SUBTRACT    7-82-WORK-MM FROM 7-82-ANN-MM
           DIVIDE      WS00-DIV INTO 7-82-ANN-MM GIVING
           7-82-RESULT REMAINDER 7-82-REM.
                 IF    7-82-REM = ZERO                                  DOT
           MOVE        'Y' TO WS00-OK.
       F80FB-FN. EXIT.
      *N80GB.    NOTE *CALCULATION  - FORMULA             *.
       F80GB.                                                           lv10
      *MATCH MACRO AAOLBB LOGIC CHANGE
      *ONLY FOR IPE INSURED PRODUCTS
           COMPUTE     ACCT-MONTHLY ROUNDED =
           (AA10-ALPAGR * 0.0875)
           COMPUTE     ACCT-QUARTERLY ROUNDED =
           (AA10-ALPAGR * 0.2625)
           COMPUTE     ACCT-SEMIANNUAL ROUNDED =
           (AA10-ALPAGR * 0.515).
       F80GB-FN. EXIT.
       F80-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N91ER.    NOTE *PACTABLE ERROR ENCOUNTERED         *.
       F91ER.                                                           lv10
      *******************************
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012617 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F91ER-FN. EXIT.
      *N91JB.    NOTE *POPULATION OF THE VALUES           *.            AAACTG
       F91JB.                                                           lv10
           MOVE        NS20-DCACG TO 7-CURR-DATE                        AAACTG
           PERFORM     F91JI THRU F91JI-FN                              AAACTG
           MOVE        7-REQD-DATE TO PX74-GESTE.                       AAACTG
       F91JB-FN. EXIT.
      *N91JC.    NOTE *FIGURE END DATE                    *.
       F91JC.                                                           lv10
           ADD         WS00-MTHS TO WS00-MM.
                 IF    WS00-MM > 12                                     DOT
           SUBTRACT    12 FROM WS00-MM
           ADD         1 TO WS00-CCYY.
           MOVE        WS00-CCYYMMDD TO PX74-GESTL.                     DOT
       F91JC-FN. EXIT.
      *N91JI.    NOTE *LOOP TO FIND THE REQD ACTG DATE    *.            AAACTG
       F91JI.                       GO TO     F91JI-B.                  lv10
       F91JI-A.
                 IF    7-CNT-DATE = WS00-DAYS                           AAACTG
                 OR    7-REQD-DATE = ZEROES                             AAACTG
                                    GO TO     F91JI-FN.                 AAACTG
       F91JI-B.
           ADD         1 TO 7-CNT-DATE.                                 AAACTG
      *N91JJ.    NOTE *CALCULATE THE NEXT ACCNT DATE      *.            AAACTG
       F91JJ.                                                           lv15
      *CODE FROM $AACTG IS PLACED BELOW                                 AAACTG
           MOVE        7-CURR-DATE TO 7-XX01-PCKDAT                     $AACTG
           COMPUTE     7-XX01-PUDAT =                                   $AACTG
           (7-XX01-PCKDAT * 10)                                         $AACTG
           MOVE        7-XX01-UNSDAT TO 7-XX01-ICURR                    $AACTG
           CALL        7-XX01-DATMOD USING                              $AACTG
           7-XX01-IDTFLD                                                $AACTG
           7-XX01-RDTFLD                                                $AACTG
           MOVE        7-XX01-RCDATE TO 7-XX01-CHKDAT.                  $AACTG
                 IF    7-XX01-CHKPDT = +177607040                       DOT
           MOVE        ZEROES TO 7-REQD-DATE                            $AACTG
                 ELSE                                                   $AACTG
           MOVE        7-XX01-RNDATE TO 7-XX01-CHKDAT                   $AACTG
           COMPUTE     7-XX01-NEXTDT =                                  $AACTG
           (7-XX01-CHKPDT / 10)                                         $AACTG
           MOVE        7-XX01-NEXTDT TO 7-REQD-DATE                     $AACTG
           MOVE        7-REQD-DATE TO 7-CURR-DATE.                      AAACTG
       F91JJ-FN. EXIT.
       F91JI-900. GO TO F91JI-A.
       F91JI-FN. EXIT.
      *N92TA.    NOTE *RANDOM TABLE READ FOR TA6A         *.            ADUTAB
       F92TA.                                                           lv10
           MOVE        'R1' TO G-TA6A-TABFO                             ADUTAB
           COMPUTE     G-TA6A-LTH = 60 + G-TA6A-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA6A-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA6A)                                ADUTAB
                       LENGTH (G-TA6A-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA6A-TABCR NOT = '00'                          DOT
           PERFORM     F91ER THRU F91ER-FN.                             ADUTAB
       F92TA-FN. EXIT.
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
      *N94AB.    NOTE *CALL GU ON CT22                    *.            ADU026
       F94AB.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT22' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XA06 CT22                                                    ADU026
           S-CTU01-SSA S-CTU22-SSA                                      ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94AB-FN. EXIT.
      *N94AE.    NOTE *CALL GU ON AA10                    *.            ADU026
       F94AE.                                                           lv10
           MOVE        'LM1P' TO DE10-XDBDNM                            ADU026
           MOVE        'AA10' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XB06 AA10                                                    ADU026
           S-AAU10-SSA                                                  ADU026
           MOVE        XB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94AE-FN. EXIT.
      *N94AH.    NOTE *CALL GU ON AA85                    *.            ADU026
       F94AH.                                                           lv10
           MOVE        'LM1P' TO DE10-XDBDNM                            ADU026
           MOVE        'AA85' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XB06 AA85                                                    ADU026
           S-AAU10-SSA S-AA85-SSA                                       ADU026
           MOVE        XB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94AH-FN. EXIT.
      *N94AI.    NOTE *CALL GU ON CX2Y                    *.            ADU026
       F94AI.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XD06 CX2Y                                                    ADU026
           S-CXU2Y-SSA                                                  ADU026
           MOVE        XD06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94AI-FN. EXIT.
      *N94AJ.    NOTE *CALL GU ON CX03                    *.            ADU026
       F94AJ.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XC06 CX03                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94AJ-FN. EXIT.
      *N94AK.    NOTE *CALL GU ON CX06                    *.            ADU026
       F94AK.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XC06 CX06                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94AK-FN. EXIT.
      *N94AL.    NOTE *CALL GN ON CX12                    *.            ADU026
       F94AL.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XC06 CX12                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CX12-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94AL-FN. EXIT.
      *N94AM.    NOTE *CALL GN ON CX2Y                    *.            ADU026
       F94AM.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XD06 CX2Y                                                    ADU026
           S-CX2Y-SSA                                                   ADU026
           MOVE        XD06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94AM-FN. EXIT.
      *N94AO.    NOTE *CALL GN ON CX13                    *.            ADU026
       F94AO.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XC06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CX13-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94AO-FN. EXIT.
      *N94AP.    NOTE *CALL GN ON CX14                    *.            ADU026
       F94AP.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX14' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XC06 CX14                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           S-CX14-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94AP-FN. EXIT.
      *N94AR.    NOTE *CALL GU ON CX13                    *.            ADU026
       F94AR.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XC06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94AR-FN. EXIT.
      *N94AT.    NOTE *CALL GN ON CX6Y                    *.            ADU026
       F94AT.                                                           lv10
           MOVE        'AREY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX6Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XI06 CX6Y                                                    ADU026
           S-CX6Y-SSA                                                   ADU026
           MOVE        XI06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94AT-FN. EXIT.
      *N94AV.    NOTE *CALL GU ON CX6Y                    *.            ADU026
       F94AV.                                                           lv10
           MOVE        'AREY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX6Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XI06 CX6Y                                                    ADU026
           S-CXU6Y-SSA                                                  ADU026
           MOVE        XI06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94AV-FN. EXIT.
      *N97.      NOTE *************************************.
      *               *                                   *
      *               *CALLS TO LIFE MODULES              *
      *               *                                   *
      *               *************************************.
       F97.           EXIT.                                             lv05
      *N97JB.    NOTE *CALL LIFE DETAIL INFO    MODULE    *.            AM0140
       F97JB.                                                           lv10
      *SET PCB POINTERS                                                 DOT
           SET CI0140-PH-PCB-LM1P-PTR1 TO                               AM0140
                         PCB-LM1P-PTR1                                  AM0140
           SET CI0140-PH-PCB-LUVP-PTR1 TO                               AM0140
                         PCB-LUVP-PTR1                                  AM0140
           SET CI0140-PH-PCB-LH1P-PTR1 TO                               AM0140
                         PCB-LH1P-PTR1                                  AM0140
           SET CI0140-PH-PCB-LARP-PTR1 TO                               AM0140
                         PCB-LARP-PTR1                                  AM0140
           SET CI0140-PH-PCB-LPDP-PTR1 TO                               AM0140
                         PCB-LPDP-PTR1                                  AM0140
           SET CI0140-PH-PCB-ARAY-PTR1 TO                               AM0140
                         PCB-ARAY-PTR1                                  AM0140
           SET CI0140-PH-PCB-AR1P-PTR1 TO                               AM0140
                         PCB-AR1P-PTR1.                                 AM0140
      *INITIALIZE OUTPUT PASS AREA                                      DOT
           INITIALIZE  DE10-DU03                                        AM0140
           INITIALIZE  WE40.                                            AM0140
      *LOAD INPUT PARMS  PASS AREA                                      DOT
           MOVE        PX74-CTID01 TO WE40-CTID
           MOVE        PX74-PRCOD1 TO WE40-PRCOD.
           CALL        CI0140 USING                                     DOT
           DFHEIBLK                                                     AM0140
           DFHCOMMAREA                                                  AM0140
           DLIUIBII                                                     AM0140
           CI0140-PH-PCB-ADDR-LIST                                      AM0140
           WE40                                                         AM0140
           DE10                                                         AM0140
           MS03                                                         AM0140
           MX11.                                                        AM0140
       F97JB-FN. EXIT.
      *N97KB.    NOTE *---> CALL CI0141                   *.            AM0141
       F97KB.                                                           lv10
      *     GET VANTAGE LIFE ACCT INFO                                  AM0141
      *.                                                                AM0141
      *INITIALIZE        PASS AREA                                      DOT
           INITIALIZE  WF41-K94R.                                       AM0141
      *LOAD INPUT PARMS  PASS AREA                                      DOT
           MOVE        PX74-CTID01 TO WF41-CTID
           MOVE        PX74-PRCOD1 TO WF41-PRCOD.
           CALL        CI0141 USING                                     DOT
           DFHEIBLK                                                     AM0141
           DFHCOMMAREA                                                  AM0141
           WF41                                                         AM0141
           MS03                                                         AM0141
           MX11.                                                        AM0141
       F97KB-FN. EXIT.
       F97-FN.   EXIT.
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
