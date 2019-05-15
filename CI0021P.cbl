       IDENTIFICATION DIVISION.                                         CI0021
       PROGRAM-ID.  CI0021P.                                            CI0021
      *AUTHOR.         M\M - PAYMENT EDITS MODULE.                      CI0021
      *DATE-COMPILED.   09/08/14.                                       CI0021
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
       ENVIRONMENT DIVISION.                                            CI0021
       CONFIGURATION SECTION.                                           CI0021
       SOURCE-COMPUTER. IBM-370.                                        CI0021
       OBJECT-COMPUTER. IBM-370.                                        CI0021
       DATA DIVISION.                                                   CI0021
       WORKING-STORAGE SECTION.                                         CI0021
       01                 AA10.                                         CI0021
            10            AA10-AE00.                                    CI0021
            11            AA10-ALCIDN PICTURE  9(11).                   CI0021
            10            AA10-AE01.                                    CI0021
            11            AA10-FILLER PICTURE  X(12).                   CI0021
            11            AA10-DLAUP  PICTURE  9(8).                    CI0021
            11            AA10-FILLER PICTURE  S9(07)                   CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-FILLER PICTURE  9(8).                    CI0021
            11            AA10-FILLER PICTURE  S9(07)                   CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-FILLER PICTURE  9(8).                    CI0021
            11            AA10-FILLER PICTURE  S9(07)                   CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-FILLER PICTURE  X(311).                  CI0021
            10            AA10-AE02                                     CI0021
                          REDEFINES            AA10-AE01.               CI0021
            11            AA10-FILLER PICTURE  9.                       CI0021
            11            AA10-ALCOMP PICTURE  99.                      CI0021
            11            AA10-CRTYP  PICTURE  9(4).                    CI0021
            11            AA10-FILLER PICTURE  9.                       CI0021
            11            AA10-CPOST  PICTURE  99.                      CI0021
            11            AA10-GEHCDI PICTURE  9(3).                    CI0021
            11            AA10-FILLER PICTURE  9(8).                    CI0021
            11            AA10-FILLER PICTURE  9(7).                    CI0021
            11            AA10-FILLER PICTURE  X.                       CI0021
            11            AA10-ALPLDT PICTURE  9(8).                    CI0021
            11            AA10-DENEX  PICTURE  9(8).                    CI0021
            11            AA10-CENXC1 PICTURE  9(3).                    CI0021
            11            AA10-FILLER PICTURE  99.                      CI0021
            11            AA10-CROOR  PICTURE  99.                      CI0021
            11            AA10-CREIN  PICTURE  99.                      CI0021
            11            AA10-FILLER PICTURE  X.                       CI0021
            11            AA10-ALAPST PICTURE  99.                      CI0021
            11            AA10-ALSTSA PICTURE  XX.                      CI0021
            11            AA10-FILLER PICTURE  99.                      CI0021
            11            AA10-CPCAL  PICTURE  9.                       CI0021
            11            AA10-CNAEX  PICTURE  9.                       CI0021
            11            AA10-CSUSI  PICTURE  99.                      CI0021
            11            AA10-FILLER PICTURE  99.                      CI0021
            11            AA10-FILLER PICTURE  99.                      CI0021
            11            AA10-FILLER PICTURE  9(8).                    CI0021
            11            AA10-FILLER PICTURE  9.                       CI0021
            11            AA10-FILLER PICTURE  X(10).                   CI0021
            11            AA10-FILLER PICTURE  99.                      CI0021
            11            AA10-FILLER PICTURE  X.                       CI0021
            11            AA10-FILLER PICTURE  99.                      CI0021
            11            AA10-CAPL   PICTURE  9.                       CI0021
            11            AA10-FILLER PICTURE  9.                       CI0021
            11            AA10-FILLER PICTURE  999.                     CI0021
            11            AA10-FILLER PICTURE  999.                     CI0021
            11            AA10-FILLER PICTURE  999.                     CI0021
            11            AA10-CSTWH  PICTURE  9(8).                    CI0021
            11            AA10-FILLER PICTURE  9(8).                    CI0021
            11            AA10-FILLER PICTURE  99.                      CI0021
            11            AA10-FILLER PICTURE  9.                       CI0021
            11            AA10-FILLER PICTURE  9.                       CI0021
            11            AA10-FILLER PICTURE  99.                      CI0021
            11            AA10-FILLER PICTURE  999.                     CI0021
            11            AA10-FILLER PICTURE  999.                     CI0021
            11            AA10-CPRPM  PICTURE  9(3).                    CI0021
            11            AA10-FILLER PICTURE  99.                      CI0021
            11            AA10-FILLER PICTURE  9(6).                    CI0021
            11            AA10-FILLER PICTURE  99.                      CI0021
            11            AA10-FILLER PICTURE  999.                     CI0021
            11            AA10-DTRCM  PICTURE  9(8).                    CI0021
            11            AA10-DLATR  PICTURE  9(8).                    CI0021
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-ALPMOD PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-CRSBN  PICTURE  99.                      CI0021
            11            AA10-FILLER PICTURE  X.                       CI0021
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-FILLER PICTURE  9.                       CI0021
            11            AA10-FILLER PICTURE  X.                       CI0021
            11            AA10-FILLER PICTURE  XX.                      CI0021
            11            AA10-FILLER PICTURE  99.                      CI0021
            11            AA10-CTRHO  PICTURE  9(8).                    CI0021
            11            AA10-FILLER PICTURE  9.                       CI0021
            11            AA10-FILLER PICTURE  99.                      CI0021
            11            AA10-FILLER PICTURE  99.                      CI0021
            11            AA10-CTSGD  PICTURE  9(8).                    CI0021
            11            AA10-IANRD  PICTURE  9.                       CI0021
            11            AA10-ALINNO PICTURE  99.                      CI0021
            11            AA10-ALSANN PICTURE  9(5).                    CI0021
            11            AA10-FILLER PICTURE  S9(9)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-ALPAGR PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-ALRISK PICTURE  S9(9)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-ALAPIT PICTURE  S9(07)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-FILLER PICTURE  X.                       CI0021
            11            AA10-CADPR  PICTURE  9.                       CI0021
            11            AA10-AAPRT  PICTURE  S9(07)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-NREPN1 PICTURE  9(06).                   CI0021
            11            AA10-CCST1  PICTURE  9.                       CI0021
            11            AA10-CESRD  PICTURE  9(3).                    CI0021
            11            AA10-ALLRT  PICTURE  S9V99                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-ANTPAA PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-ALDDUE PICTURE  9(08).                   CI0021
            11            AA10-ALMODE PICTURE  99.                      CI0021
            11            AA10-FILLER PICTURE  X(08).                   CI0021
            11            AA10-CTCUS1 PICTURE  99.                      CI0021
            11            AA10-CNPPR  PICTURE  9(03).                   CI0021
            11            AA10-FILLER PICTURE  9.                       CI0021
            11            AA10-FILLER PICTURE  9(03).                   CI0021
            11            AA10-ITMEC  PICTURE  X(1).                    CI0021
            11            AA10-IMCDI  PICTURE  X.                       CI0021
            11            AA10-LSIDTE PICTURE  9(08).                   CI0021
            11            AA10-PLINE  PICTURE  S9V99                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-ATSA8  PICTURE  S9(07)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-ATSA9  PICTURE  S9(05)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-CRATS  PICTURE  X.                       CI0021
            11            AA10-PPTKN  PICTURE  S9(3)V9(6)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            AA10-FILLER PICTURE  X(24).                   CI0021
       01                 AA20.                                         CI0021
            10            AA20-CLTRN  PICTURE  999.                     CI0021
            10            AA20-DLACC  PICTURE  9(8).                    CI0021
            10            AA20-ALDUED PICTURE  9(8).                    CI0021
            10            AA20-DLTRN  PICTURE  9(8).                    CI0021
            10            AA20-ALLNB  PICTURE  S9(07)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA20-ALLPA  PICTURE  S9(07)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA20-FILLER PICTURE  9(06).                   CI0021
            10            AA20-FILLER PICTURE  S9(05)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA20-FILLER PICTURE  S9(05)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA20-FILLER PICTURE  9(06).                   CI0021
            10            AA20-FILLER PICTURE  S9(07)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA20-FILLER PICTURE  S9(07)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
       01                 AA25.                                         CI0021
            10            AA25-ANPTT  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-APCLO  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-APTXL  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-AVARP  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-AVARN  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-APCLD  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-AVART  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-AVLSC  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-AFLSC  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ALUNIT PICTURE  S9(8)V999                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-CSURV  PICTURE  99.                      CI0021
            10            AA25-CFGLC  PICTURE  9(6).                    CI0021
            10            AA25-CMINP  PICTURE  9.                       CI0021
            10            AA25-APPYP  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-APPY2  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-APPY3  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-DSIDF  PICTURE  9(8).                    CI0021
            10            AA25-ALYTDF PICTURE  S9(07)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-DSIDV  PICTURE  9(8).                    CI0021
            10            AA25-ALYTDV PICTURE  S9(07)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-DTRNF  PICTURE  9(8).                    CI0021
            10            AA25-AREF   PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-DTRLA  PICTURE  9(8).                    CI0021
            10            AA25-DTRLT  PICTURE  9(8).                    CI0021
            10            AA25-CTRNL  PICTURE  9(4).                    CI0021
            10            AA25-ATRNL  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ATRCV  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ATRCI  PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-QTRAU  PICTURE  S9(8)V999                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ATRCT  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ATCTI  PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-QTRTA  PICTURE  S9(8)V999                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-IOMIT  PICTURE  9.                       CI0021
            10            AA25-DPAPR  PICTURE  9(8).                    CI0021
            10            AA25-DCAPR  PICTURE  9(8).                    CI0021
            10            AA25-PVAP   PICTURE  S9(3)V9(4)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ALPALC PICTURE  S9(04)V999               CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-PAACF  PICTURE  S9(2)V999                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-NBUID  PICTURE  S999                     CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ABUCV  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ANFMC  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ANFMP  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-DLACD  PICTURE  9(8).                    CI0021
            10            AA25-DPACD  PICTURE  9(8).                    CI0021
            10            AA25-CBASP  PICTURE  9.                       CI0021
            10            AA25-FILLER PICTURE  9.                       CI0021
            10            AA25-APYTI  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-FILLER PICTURE  X(2).                    CI0021
            10            AA25-GNPCA.                                   CI0021
            11            AA25-CPRDE  PICTURE  99.                      CI0021
            11            AA25-CPFGH  PICTURE  999.                     CI0021
            11            AA25-CPRI   PICTURE  9.                       CI0021
            11            AA25-CPRJ   PICTURE  9.                       CI0021
            11            AA25-CPKL   PICTURE  99.                      CI0021
            10            AA25-CLPRC  PICTURE  9(7).                    CI0021
            10            AA25-AQPAP  PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-AQPTC  PICTURE  S9(7)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ATPRC  PICTURE  S9(7)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ATASP  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ANPIP  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-DNPCA  PICTURE  9(8).                    CI0021
            10            AA25-DLAPC  PICTURE  9(8).                    CI0021
            10            AA25-APRCR  PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-AMATP  PICTURE  S9(7)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-AOGAP  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-APYCV  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-AACV   PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-AAICV  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ACVCY  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ACVPY  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-FILLER PICTURE  S9(6)V999                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ATACV  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ATCVI  PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ACCYV  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-APCYV  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-CES79  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ARAPI  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-CARDC  PICTURE  99.                      CI0021
            10            AA25-PGAIR  PICTURE  SV999                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ASPAP  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ATFCV  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ATFCI  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-QTFAU  PICTURE  S9(6)V999                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ATFPI  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-CWITH  PICTURE  99.                      CI0021
            10            AA25-ATCBC  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ATCEP  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ATLTC  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ATRCIT PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-QTRAUT PICTURE  S9(8)V999                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-DINNO  PICTURE  9(8).                    CI0021
            10            AA25-CSIRAC PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-CSIRAP PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-AIRALC PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-AIPYT  PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-DLDIC  PICTURE  9(4).                    CI0021
            10            AA25-AROLC  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-AROLP  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA25-ISEPP  PICTURE  X.                       CI0021
            10            AA25-IDEMR  PICTURE  X(1).                    CI0021
            10            AA25-FILLER PICTURE  X(139).                  CI0021
       01                 AA66.                                         CI0021
            10            AA66-CLTRN  PICTURE  999.                     CI0021
            10            AA66-DLACC  PICTURE  9(8).                    CI0021
            10            AA66-DLPAD  PICTURE  9(08).                   CI0021
            10            AA66-DLTRNL PICTURE  9(8).                    CI0021
            10            AA66-DLTRN  PICTURE  9(8).                    CI0021
            10            AA66-ALDUED PICTURE  9(8).                    CI0021
            10            AA66-ALLNB  PICTURE  S9(07)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA66-ALLPA  PICTURE  S9(07)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA66-ALLIP  PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA66-ALADJL PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA66-QLLNT  PICTURE  99.                      CI0021
            10            AA66-ILNST  PICTURE  X.                       CI0021
            10            AA66-PVPAP  PICTURE  S9(04)V999               CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA66-DCAPR  PICTURE  9(8).                    CI0021
            10            AA66-PVPAPP PICTURE  S9(4)V9(3)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA66-DPAPR  PICTURE  9(8).                    CI0021
            10            AA66-ALADJF PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA66-DTRNFD PICTURE  9(8).                    CI0021
            10            AA66-ANIFN  PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA66-AIREF  PICTURE  S9(5)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            AA66-FILLER PICTURE  X(8).                    CI0021
      *!WF DSP=PF DSL=PE SEL=3031 FOR=I DES=1 LEV=1                     AAZ000
       01                 PF30.                                         CI0021
            10            PF30-XPROPT PICTURE  X(04).                   CI0021
            10            PF30-GVSGK  PICTURE  X(87).                   CI0021
            10            PF30-CVACO  PICTURE  X(3).                    CI0021
            10            PF30-GVAID  PICTURE  X(15).                   CI0021
            10            PF30-CTHCO  PICTURE  X(4).                    CI0021
            10            PF30-IDBCD  PICTURE  X(1).                    CI0021
            10            PF30-IDBMF  PICTURE  X(1).                    CI0021
            10            PF30-IDBNO  PICTURE  X(1).                    CI0021
            10            PF30-IDBNI  PICTURE  X(1).                    CI0021
            10            PF30-IDBTH  PICTURE  X(1).                    CI0021
            10            PF30-IDBSW  PICTURE  X(1).                    CI0021
            10            PF30-CPOLV  PICTURE  X.                       CI0021
            10            PF30-DEFFT  PICTURE  9(8).                    CI0021
            10            PF30-IIRSO  PICTURE  X(1).                    CI0021
            10            PF30-IMVAO  PICTURE  X(1).                    CI0021
            10            PF30-CMEMO  PICTURE  X(2).                    CI0021
            10            PF30-IVOGN  PICTURE  X.                       CI0021
            10            PF30-APRGT  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            PF30-CDSTV  PICTURE  X.                       CI0021
            10            PF30-IDBUV  PICTURE  X(1).                    CI0021
            10            PF30-IDBAI  PICTURE  X(1).                    CI0021
            10            PF30-IDBRI  PICTURE  X(1).                    CI0021
            10            PF30-IDBCO  PICTURE  X(1).                    CI0021
            10            PF30-IDBCA  PICTURE  X(1).                    CI0021
            10            PF30-IDBBI  PICTURE  X(1).                    CI0021
            10            PF30-IDBLO  PICTURE  X(1).                    CI0021
            10            PF30-IDBFD  PICTURE  X(1).                    CI0021
            10            PF30-IDBPY  PICTURE  X(1).                    CI0021
            10            PF30-IDBRP  PICTURE  X(1).                    CI0021
            10            PF30-IDBRE  PICTURE  X(1).                    CI0021
            10            PF30-CFNDC  PICTURE  S9(3)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            PF30-CCLTP1 PICTURE  X(2).                    CI0021
            10            PF30-CDSTV3 PICTURE  X(3).                    CI0021
            10            PF30-IVIVS  PICTURE  X.                       CI0021
            10            PF30-IDBSR  PICTURE  X(2).                    CI0021
            10            PF30-CPNOP  PICTURE  X(2).                    CI0021
            10            PF30-IDDTH  PICTURE  X(1).                    CI0021
            10            PF30-CLDOD  PICTURE  9(8).                    CI0021
            10            PF30-IDBIF  PICTURE  X(1).                    CI0021
            10            PF30-IDBMA  PICTURE  X(1).                    CI0021
            10            PF30-CCOUL  PICTURE  XX.                      CI0021
            10            PF30-FILLER PICTURE  X(298).                  CI0021
       01                 PF31.                                         CI0021
            10            PF31-CVSTC  PICTURE  X(4).                    CI0021
            10            PF31-GVSGK  PICTURE  X(87).                   CI0021
            10            PF31-QFNDO  PICTURE  S9(4)                    CI0021
                          BINARY.                                       CI0021
            10            PF31-PE32.                                    CI0021
            11            PF31-PE65.                                    CI0021
            12            PF31-CVAPC  PICTURE  X(6).                    CI0021
            12            PF31-CVALB  PICTURE  X(3).                    CI0021
            12            PF31-CASTA  PICTURE  X.                       CI0021
            12            PF31-CTWST1 PICTURE  X(3).                    CI0021
            12            PF31-CPISC  PICTURE  X(3).                    CI0021
            12            PF31-ALPLDT PICTURE  9(8).                    CI0021
            12            PF31-DEFFT  PICTURE  9(8).                    CI0021
            12            PF31-DANNI  PICTURE  9(8).                    CI0021
            12            PF31-DTPMT  PICTURE  9(8).                    CI0021
            12            PF31-ITAMR  PICTURE  X(1).                    CI0021
            12            PF31-AGAPA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ATRPA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ATROP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ITMECC PICTURE  X(1).                    CI0021
            12            PF31-CBPLCA PICTURE  X.                       CI0021
            12            PF31-CPRCC  PICTURE  X.                       CI0021
            12            PF31-CLSEX  PICTURE  X.                       CI0021
            12            PF31-QPOIA  PICTURE  S9(3)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-IREIA  PICTURE  X.                       CI0021
            12            PF31-APCUA  PICTURE  S9(6)V9(5)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AGLPA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AGSPA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ATGPA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ACBIN1 PICTURE  S9(09)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CABRC  PICTURE  X.                       CI0021
            12            PF31-CDSON  PICTURE  X(06).                   CI0021
            12            PF31-CADVN  PICTURE  X(10).                   CI0021
            12            PF31-CVOMC1 PICTURE  X(1).                    CI0021
            12            PF31-CSSUP2 PICTURE  X.                       CI0021
            12            PF31-ACECP  PICTURE  S9(09)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CCOFE  PICTURE  X.                       CI0021
            12            PF31-GRIDN7 PICTURE  S9(7)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-QNZDS  PICTURE  S9(04)                   CI0021
                          COMPUTATIONAL   SYNC RIGHT.                   CI0021
            12            PF31-QTNOL  PICTURE  S9(05)                   CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-QTNPW  PICTURE  S9(05)                   CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-IREIN  PICTURE  X.                       CI0021
            12            PF31-DNRIP  PICTURE  9(8).                    CI0021
            12            PF31-ATDPA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CREPC  PICTURE  XX.                      CI0021
            12            PF31-CMLTP  PICTURE  X.                       CI0021
            12            PF31-IREPL6 PICTURE  X.                       CI0021
            12            PF31-CCLAC  PICTURE  X.                       CI0021
            12            PF31-ALNTI  PICTURE  S9(09)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ATIPA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CVASC  PICTURE  XX.                      CI0021
            12            PF31-DRTHC  PICTURE  9(8).                    CI0021
            12            PF31-CENDO  PICTURE  X.                       CI0021
            12            PF31-ICOEX  PICTURE  X(1).                    CI0021
            12            PF31-INURS  PICTURE  X(1).                    CI0021
            12            PF31-ITRML  PICTURE  X(1).                    CI0021
            12            PF31-IBIRA  PICTURE  X(1).                    CI0021
            12            PF31-CCOUL  PICTURE  XX.                      CI0021
            12            PF31-ASGLP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-DMATUR PICTURE  9(8).                    CI0021
            12            PF31-CJOIP  PICTURE  X.                       CI0021
            12            PF31-CPNOP  PICTURE  X(2).                    CI0021
            12            PF31-ARBRP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ACCHV  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ASCHV  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CGMBR  PICTURE  X.                       CI0021
            12            PF31-AMCTV  PICTURE  S9(7)V99.                CI0021
            12            PF31-ACBIN2 PICTURE  S9(09)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PE1B                                     CI0021
                          REDEFINES            PF31-ACBIN2.             CI0021
            13            PF31-NAAMC  PICTURE  9(2).                    CI0021
            13            PF31-PNPCT  PICTURE  999.                     CI0021
            13            PF31-FILLER PICTURE  X(1).                    CI0021
            11            PF31-PE75                                     CI0021
                          REDEFINES            PF31-PE65.               CI0021
            12            PF31-CASTAR PICTURE  X.                       CI0021
            12            PF31-DLPLDA PICTURE  9(8).                    CI0021
            12            PF31-ATRPAR PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-DSTT1  PICTURE  9(8).                    CI0021
            12            PF31-CTLPD  PICTURE  9(8).                    CI0021
            12            PF31-DNPMT  PICTURE  9(8).                    CI0021
            12            PF31-ALFXP  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CFRQZ  PICTURE  X.                       CI0021
            12            PF31-ASTXW1 PICTURE  S9(13)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ATWHDD PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-APOCY  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-APOPY  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-APOTD  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-TPLNL  PICTURE  X(30).                   CI0021
            12            PF31-NPSTA  PICTURE  X(11).                   CI0021
            12            PF31-CPSTP  PICTURE  X(6).                    CI0021
            12            PF31-DPSTI  PICTURE  9(8).                    CI0021
            12            PF31-IFXVR  PICTURE  X.                       CI0021
            12            PF31-IDTHI  PICTURE  X.                       CI0021
            12            PF31-PFPAY  PICTURE  S9(3)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PVPAY  PICTURE  S9(3)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CPURF  PICTURE  X(5).                    CI0021
            12            PF31-ATAXY  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CVALB1 PICTURE  X(4).                    CI0021
            12            PF31-CRATC1 PICTURE  X.                       CI0021
            12            PF31-PSTIN  PICTURE  9(3)V99                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PFTXW  PICTURE  9(3)V99                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PSTXW  PICTURE  9(3)V99                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PFERT  PICTURE  9(3)V99                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AFEDV  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PSERT  PICTURE  9(3)V99                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ASEDV  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-DPRPA  PICTURE  9(8).                    CI0021
            12            PF31-ALVXP  PICTURE  S9(7)V9(4)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ISTWH1 PICTURE  X(1).                    CI0021
            12            PF31-FILLER PICTURE  X(44).                   CI0021
            11            PF31-PE67.                                    CI0021
            12            PF31-DMDOB1 PICTURE  X(8).                    CI0021
            12            PF31-CTNAM  PICTURE  X(01).                   CI0021
            12            PF31-MCOMP                                    CI0021
                          OCCURS       002     TIMES.                   CI0021
            13            PF31-MINDL  PICTURE  X(35).                   CI0021
            13            PF31-MINDF  PICTURE  X(25).                   CI0021
            13            PF31-MINDP  PICTURE  X(10).                   CI0021
            13            PF31-MINDS  PICTURE  X(10).                   CI0021
            12            PF31-MADR1V PICTURE  X(35)                    CI0021
                          OCCURS       004     TIMES.                   CI0021
            12            PF31-MCIT   PICTURE  X(30).                   CI0021
            12            PF31-CPISC  PICTURE  X(3).                    CI0021
            12            PF31-CZIPC  PICTURE  X(15).                   CI0021
            12            PF31-CLSEX1 PICTURE  X.                       CI0021
            11            PF31-PE88.                                    CI0021
            12            PF31-QNSUR  PICTURE  S9(3)                    CI0021
                          BINARY.                                       CI0021
            12            PF31-QTSUR  PICTURE  S9(3)                    CI0021
                          BINARY.                                       CI0021
            12            PF31-ACGPA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ATOTA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PE8C                                     CI0021
                          OCCURS       010     TIMES.                   CI0021
            13            PF31-ALHDTE PICTURE  9(08).                   CI0021
            13            PF31-ATWHDE PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ASTXW  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ACHKJ  PICTURE  S9(09)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-CVTXN1 PICTURE  X(4).                    CI0021
            13            PF31-ACACTV PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ASPAM1 PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-CSSVL  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-PE86.                                    CI0021
            12            PF31-CVOOD  PICTURE  S9(5)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-DEIRNB PICTURE  9(8).                    CI0021
            12            PF31-QCRPD  PICTURE  S9(3)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CBRIT  PICTURE  SV9(5)                   CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-PE91.                                    CI0021
            12            PF31-CVOOD1 PICTURE  S9(5)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-DTRMSX PICTURE  X(8).                    CI0021
            12            PF31-DTRMEX PICTURE  X(8).                    CI0021
            12            PF31-ALPRUN PICTURE  S999V9(6)                CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PSPSX                                    CI0021
                          REDEFINES            PF31-ALPRUN              CI0021
               PICTURE    S9(7)V99                                      CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PIXPR  PICTURE  S9V9(4)                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-PE6B.                                    CI0021
            12            PF31-CEBMO  PICTURE  9(2).                    CI0021
            12            PF31-CBNBC1 PICTURE  X.                       CI0021
            12            PF31-AMDAR  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ALDDUE PICTURE  9(08).                   CI0021
            12            PF31-ASPAM2 PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-DRESE  PICTURE  9(8).                    CI0021
            12            PF31-DACUP  PICTURE  9(02).                   CI0021
            12            PF31-ACVAMG PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ISTWH  PICTURE  X(1).                    CI0021
            12            PF31-AVAIP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-FILLER PICTURE  X(63).                   CI0021
            11            PF31-PE6C.                                    CI0021
            12            PF31-ARGPA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AROGP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ACYTA  PICTURE  S9(3)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-FILLER PICTURE  X(050).                  CI0021
            11            PF31-PE6R.                                    CI0021
            12            PF31-CRTYC  PICTURE  X(1).                    CI0021
            12            PF31-CRICO  PICTURE  X(3).                    CI0021
            11            PF31-PE3V.                                    CI0021
            12            PF31-PE90.                                    CI0021
            13            PF31-CSTCVE PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ASURR1 PICTURE  S9(09)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ALSURR PICTURE  S9(09)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ASINTC PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-AMVA1  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ASPAM  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ACVAM  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-AMSBT  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ACVALC PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ATWS   PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ISELO  PICTURE  X.                       CI0021
            13            PF31-FILLER PICTURE  X(1).                    CI0021
            13            PF31-FILLER PICTURE  X(2).                    CI0021
            13            PF31-ASTCV1 PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ATFCVC PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-FILLER PICTURE  X(1).                    CI0021
            13            PF31-FILLER PICTURE  X(2).                    CI0021
            13            PF31-AUINTA PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-FILLER PICTURE  X(1).                    CI0021
            13            PF31-IRCHG  PICTURE  X.                       CI0021
            13            PF31-ASTXW8 PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ATFRA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-APLIV  PICTURE  S9(09)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-PCIRB1 PICTURE  S9(2)V9(3)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ACVAMF PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-CSNCVE PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-PCIRB7 PICTURE  99V999.                  CI0021
            13            PF31-AMNSR  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-AMINL  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ASCHV  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-PRCHG  PICTURE  999V999                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ISTUR  PICTURE  X.                       CI0021
            13            PF31-CSTIM  PICTURE  X.                       CI0021
            13            PF31-AMCAV1 PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ARCHG  PICTURE  9(7)V99                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PE7C                                     CI0021
                          REDEFINES            PF31-PE90.               CI0021
            13            PF31-ACOGR  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ACONE  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ASURR3 PICTURE  S9(07)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-PSURR1 PICTURE  S9(3)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ACOTX  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-TCOMG  PICTURE  X(30).                   CI0021
            13            PF31-CVCST  PICTURE  X(4).                    CI0021
            13            PF31-FILLER PICTURE  X(100).                  CI0021
            12            PF31-ALCCV  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ATLPD  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ARTLP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-APRLP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ALPAY  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-DLSST  PICTURE  9(8).                    CI0021
            12            PF31-ACAUN  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-PE6W.                                    CI0021
            12            PF31-PCIRA  PICTURE  S99V999                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PCIRB  PICTURE  S99V999                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PE6G                                     CI0021
                          OCCURS       099     TIMES.                   CI0021
            13            PF31-NFUNDV PICTURE  S999                     CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-CFUNT  PICTURE  X.                       CI0021
            13            PF31-AFUNA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-QFUNT                                    CI0021
                          REDEFINES            PF31-AFUNA               CI0021
               PICTURE    S9(7)V9(4)                                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-AFUUV  PICTURE  S9(3)V9(6)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-AFUNV  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-AFUEI  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-PE1L.                                    CI0021
            12            PF31-PE6L.                                    CI0021
            13            PF31-DLTRN  PICTURE  9(8).                    CI0021
            13            PF31-IVOGN  PICTURE  X.                       CI0021
            13            PF31-DTLPA  PICTURE  9(8).                    CI0021
            13            PF31-ARTLP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ALBUL  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-CLOFC  PICTURE  S9(5)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ILNST  PICTURE  X.                       CI0021
            12            PF31-AIELN  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-FILLER PICTURE  X(23).                   CI0021
            11            PF31-PE64.                                    CI0021
            12            PF31-AXUNVP PICTURE  S9(3)V9(6)               CI0021
                          OCCURS       998     TIMES                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-PE6A.                                    CI0021
            12            PF31-FILLER PICTURE  X(310).                  CI0021
            12            PF31-PE63                                     CI0021
                          OCCURS       099     TIMES.                   CI0021
            13            PF31-NVAFN  PICTURE  S9(3)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-CFUNT  PICTURE  X.                       CI0021
            13            PF31-PINFA6 PICTURE  S9(4)V9(1)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-FILLER PICTURE  X(18).                   CI0021
            13            PF31-ACTFA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-FILLER PICTURE  X(62).                   CI0021
            11            PF31-PE3D.                                    CI0021
            12            PF31-PE89.                                    CI0021
            13            PF31-CVOFC  PICTURE  S9(5)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-DEFFT  PICTURE  9(8).                    CI0021
            13            PF31-CVTXN  PICTURE  X(4).                    CI0021
            13            PF31-APRGT  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-CAORT  PICTURE  X(01).                   CI0021
            13            PF31-CVAST1 PICTURE  X.                       CI0021
            13            PF31-CVAST  PICTURE  X.                       CI0021
            13            PF31-CAMCN  PICTURE  X(8).                    CI0021
            13            PF31-CIRAR1 PICTURE  X.                       CI0021
            13            PF31-INQEX  PICTURE  X.                       CI0021
            13            PF31-CVAPC1 PICTURE  X(6).                    CI0021
            13            PF31-CMEMO  PICTURE  X(2).                    CI0021
            13            PF31-DEFFC  PICTURE  9(8).                    CI0021
            11            PF31-PE38.                                    CI0021
            12            PF31-ACPPA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-FILLER PICTURE  X(36).                   CI0021
            11            PF31-PE6P.                                    CI0021
            12            PF31-PCIRBA PICTURE  S99V999.                 CI0021
            12            PF31-PCIRBB PICTURE  S99V999.                 CI0021
            12            PF31-DCLWP  PICTURE  X(8).                    CI0021
            12            PF31-DGLWP  PICTURE  X(8).                    CI0021
            12            PF31-DCLNP  PICTURE  X(8).                    CI0021
            12            PF31-DGLNP  PICTURE  X(8).                    CI0021
            12            PF31-FILLER PICTURE  X(11).                   CI0021
            12            PF31-ACCHV  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ASCHV  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-DN1WP  PICTURE  X(8).                    CI0021
            12            PF31-DN1NP  PICTURE  X(8).                    CI0021
            12            PF31-DN2WP  PICTURE  X(8).                    CI0021
            12            PF31-DN2NP  PICTURE  X(8).                    CI0021
            12            PF31-CNLG1  PICTURE  X.                       CI0021
            12            PF31-CNLG2  PICTURE  X.                       CI0021
            12            PF31-CDMES  PICTURE  X(2)                     CI0021
                          OCCURS       015     TIMES.                   CI0021
            12            PF31-FILLER PICTURE  X(17).                   CI0021
            12            PF31-GDATA                                    CI0021
                          OCCURS       130     TIMES.                   CI0021
            13            PF31-ATPRP2 PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ACVWP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-AGVWP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ACVNP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-AGVNP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ACSWP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-AGSWP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-ACSNP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-AGSNP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-PE6X                                     CI0021
                          REDEFINES            PF31-PE6P.               CI0021
            12            PF31-CLFOI  PICTURE  X.                       CI0021
            12            PF31-AMECP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AGRPD  PICTURE  S9(9)V99                 CI0021
                          OCCURS       007     TIMES                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-DLMCH  PICTURE  9(8).                    CI0021
            12            PF31-APVTC  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ASMIP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ASMIPL PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AWDRTT PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CLFOI1 PICTURE  X.                       CI0021
            12            PF31-CLFOI2 PICTURE  X.                       CI0021
            12            PF31-APYMT1 PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-APYMT2 PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ASMIP1 PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ASMIP2 PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-IEXIC  PICTURE  X(01).                   CI0021
            12            PF31-DTODAF PICTURE  9(8).                    CI0021
            12            PF31-CLFOI3 PICTURE  X.                       CI0021
            12            PF31-CLFOI4 PICTURE  X.                       CI0021
            12            PF31-DEFFT  PICTURE  9(8).                    CI0021
            12            PF31-DLP20  PICTURE  9(8).                    CI0021
            12            PF31-DLP201 PICTURE  9(8).                    CI0021
            12            PF31-DLP202 PICTURE  9(8).                    CI0021
            12            PF31-AMRAC  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AMOPE  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CORAT  PICTURE  X(6).                    CI0021
            12            PF31-FILLER PICTURE  X(121).                  CI0021
            12            PF31-FILLER PICTURE  X(6873).                 CI0021
            11            PF31-PE7D.                                    CI0021
            12            PF31-DMAVS  PICTURE  9(8).                    CI0021
            12            PF31-DMAVE  PICTURE  9(8).                    CI0021
            12            PF31-CEBPR  PICTURE  X.                       CI0021
            12            PF31-DEBPS  PICTURE  9(8).                    CI0021
            12            PF31-DEBPE  PICTURE  9(8).                    CI0021
            12            PF31-ITOWN  PICTURE  X.                       CI0021
            12            PF31-AMAVR  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AEBPR  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AMDBG  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-IMAV5  PICTURE  X(1).                    CI0021
            12            PF31-DROPS  PICTURE  9(8).                    CI0021
            12            PF31-DROPE  PICTURE  9(8).                    CI0021
            12            PF31-APPCR  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AROPR  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PRCHGN PICTURE  999V999                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-FILLER PICTURE  X(15).                   CI0021
            11            PF31-PE7L.                                    CI0021
            12            PF31-APGBA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-APRBA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-APGBP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-APRBP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-DWSDT  PICTURE  9(8).                    CI0021
            12            PF31-DWAIT  PICTURE  9(8).                    CI0021
            12            PF31-CPNCG  PICTURE  X.                       CI0021
            12            PF31-DUVDT  PICTURE  9(8).                    CI0021
            11            PF31-PE66.                                    CI0021
            12            PF31-AENTI  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-DBFDT  PICTURE  9(8).                    CI0021
            12            PF31-DATFR  PICTURE  9(8).                    CI0021
            12            PF31-DATLT  PICTURE  9(8).                    CI0021
            12            PF31-ISTWH2 PICTURE  X(1).                    CI0021
            12            PF31-DRFID  PICTURE  9(8).                    CI0021
            12            PF31-DOPOD  PICTURE  9(8).                    CI0021
            12            PF31-COPIN  PICTURE  X(1).                    CI0021
            12            PF31-DPNPS  PICTURE  9(8).                    CI0021
            12            PF31-DRITR  PICTURE  9(8).                    CI0021
            12            PF31-CRIBT  PICTURE  X(1).                    CI0021
            12            PF31-INMVF  PICTURE  X(1).                    CI0021
            12            PF31-FILLER PICTURE  X(134).                  CI0021
            11            PF31-PE8L.                                    CI0021
            12            PF31-APALP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ARALP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-IRDPH  PICTURE  X.                       CI0021
            12            PF31-DEWCN  PICTURE  9(8).                    CI0021
            12            PF31-FILLER PICTURE  X(15).                   CI0021
            11            PF31-AAFEA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-ATPWO  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-SR02.                                    CI0021
            12            PF31-PTBDP  PICTURE  9(2)V99                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AMWAB  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PTALP  PICTURE  9(2)V99                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PTGBP  PICTURE  9(2)V99                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-IATAG  PICTURE  X.                       CI0021
            12            PF31-AGBAP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ARBAP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AGBPP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AALPP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AGBAS  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ARBAS  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ASGBP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AALPS  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-IAPGP  PICTURE  X.                       CI0021
            12            PF31-AALPR  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AGBPR  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ARLPR  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ARBPR  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-IASWA  PICTURE  X.                       CI0021
            12            PF31-DRFIL  PICTURE  9(8).                    CI0021
            11            PF31-PE6T.                                    CI0021
            12            PF31-ISDIS  PICTURE  X.                       CI0021
            12            PF31-IBPER  PICTURE  X.                       CI0021
            12            PF31-PINFL  PICTURE  X(02).                   CI0021
            12            PF31-CINFT  PICTURE  X.                       CI0021
            12            PF31-ARODE  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ARDBL  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ARPSL  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CLONT  PICTURE  X.                       CI0021
            12            PF31-CLONT1 PICTURE  X.                       CI0021
            12            PF31-CLONT2 PICTURE  X.                       CI0021
            12            PF31-DPRPA  PICTURE  9(8).                    CI0021
            12            PF31-ADBSU  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-FILLER PICTURE  X(2362).                 CI0021
            11            PF31-FILLER PICTURE  X(1).                    CI0021
            10            PF31-PE6Y                                     CI0021
                          REDEFINES            PF31-PE32.               CI0021
            11            PF31-PCTSW  PICTURE  S99V999                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-COLPI  PICTURE  X.                       CI0021
            11            PF31-FILLER PICTURE  X(135).                  CI0021
            10            PF31-PE69                                     CI0021
                          REDEFINES            PF31-PE32.               CI0021
            11            PF31-PE98.                                    CI0021
            12            PF31-NRCLN  PICTURE  S9(4)                    CI0021
                          BINARY.                                       CI0021
            12            PF31-FILLER PICTURE  X(2).                    CI0021
            12            PF31-CVACO1 PICTURE  X(3).                    CI0021
            12            PF31-XSW1   PICTURE  X.                       CI0021
            12            PF31-GVAID  PICTURE  X(15).                   CI0021
            12            PF31-DEFFT  PICTURE  9(8).                    CI0021
            12            PF31-FILLER PICTURE  X(2).                    CI0021
            12            PF31-CVTXN  PICTURE  X(4).                    CI0021
            12            PF31-DEFFU  PICTURE  9(8).                    CI0021
            12            PF31-FILLER PICTURE  X(15).                   CI0021
            12            PF31-CAMCN  PICTURE  X(8).                    CI0021
            12            PF31-FILLER PICTURE  X(6).                    CI0021
            12            PF31-CVOFC  PICTURE  S9(5)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CAORT  PICTURE  X(01).                   CI0021
            12            PF31-CVOFCA PICTURE  S9(5)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CVAST1 PICTURE  X.                       CI0021
            12            PF31-CVAST  PICTURE  X.                       CI0021
            12            PF31-FILLER PICTURE  X(2).                    CI0021
            12            PF31-CTRNTB PICTURE  XX.                      CI0021
            12            PF31-DEFFC  PICTURE  9(8).                    CI0021
            12            PF31-FILLER PICTURE  X(56).                   CI0021
            11            PF31-PE99.                                    CI0021
            12            PF31-FILLER PICTURE  X(19000).                CI0021
            10            PF31-PE7G                                     CI0021
                          REDEFINES            PF31-PE32.               CI0021
            11            PF31-PE79.                                    CI0021
            12            PF31-CVACO3 PICTURE  X(3).                    CI0021
            12            PF31-FILLER PICTURE  X(11).                   CI0021
            12            PF31-FILLER PICTURE  X(16).                   CI0021
            12            PF31-GVAID2 PICTURE  X(11).                   CI0021
            12            PF31-FILLER PICTURE  X(02).                   CI0021
            12            PF31-FILLER PICTURE  9(8).                    CI0021
            12            PF31-FILLER PICTURE  S9(7)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-FILLER PICTURE  X(44).                   CI0021
            12            PF31-FILLER PICTURE  X(8).                    CI0021
            12            PF31-DEFFT  PICTURE  9(8).                    CI0021
            12            PF31-DACTG  PICTURE  9(8).                    CI0021
            12            PF31-CRPAY  PICTURE  X(03).                   CI0021
            12            PF31-CHREV  PICTURE  X.                       CI0021
            12            PF31-CFEDD  PICTURE  X(02).                   CI0021
            12            PF31-ALWTAX PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-GESTNS PICTURE  X(2).                    CI0021
            12            PF31-ADSW   PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ASURR4 PICTURE  S9(07)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CSURR  PICTURE  X(01).                   CI0021
            12            PF31-ARPMT  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ALPMTR PICTURE  S9(07)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AFETX5 PICTURE  S9(07)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ANOTX  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-APAYR  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-FILLER PICTURE  X(200).                  CI0021
            11            PF31-PE84                                     CI0021
                          OCCURS       100     TIMES.                   CI0021
            12            PF31-MSACN  PICTURE  X(3).                    CI0021
            12            PF31-AFUNV3 PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-QFUUT  PICTURE  S9(9)V9(4)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AUPAM  PICTURE  S9(3)V9(7)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ITTFI  PICTURE  X.                       CI0021
            12            PF31-FILLER PICTURE  X(57).                   CI0021
            11            PF31-FILLER PICTURE  X(10778).                CI0021
            10            PF31-PE6D                                     CI0021
                          REDEFINES            PF31-PE32                CI0021
                          OCCURS       070     TIMES.                   CI0021
            11            PF31-CVSET  PICTURE  X(5).                    CI0021
            11            PF31-GVSCK.                                   CI0021
            12            PF31-MCLNM9 PICTURE  X(80).                   CI0021
            12            PF31-FILLER PICTURE  X(7).                    CI0021
            11            PF31-GDATA                                    CI0021
                          REDEFINES            PF31-GVSCK.              CI0021
            12            PF31-FILLER PICTURE  X(50).                   CI0021
            12            PF31-QPOIA2 PICTURE  S9(3)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-QPOIA3 PICTURE  S9(3)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CSPCL  PICTURE  XX.                      CI0021
            12            PF31-DSCCC  PICTURE  9(8).                    CI0021
            12            PF31-ABEPA                                    CI0021
                          REDEFINES            PF31-DSCCC               CI0021
               PICTURE    S9(9)V99                                      CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-QNORU  PICTURE  9(6)V9(5)                CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PCPUC  PICTURE  S9(3)V9(5)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-DLSTC  PICTURE  9(8).                    CI0021
            12            PF31-CTRSNV PICTURE  X(02).                   CI0021
            12            PF31-CCISR  PICTURE  X(02).                   CI0021
            11            PF31-DEFFT  PICTURE  9(8).                    CI0021
            11            PF31-CVAST  PICTURE  X.                       CI0021
            11            PF31-QPOIA  PICTURE  S9(3)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-CLSEX  PICTURE  X.                       CI0021
            11            PF31-QNOUN  PICTURE  9(6)V9(5)                CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-CPRCC  PICTURE  X.                       CI0021
            11            PF31-DPOLI  PICTURE  9(8).                    CI0021
            11            PF31-AGLPA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-CVAPC3 PICTURE  X(6).                    CI0021
            11            PF31-PPRRT2 PICTURE  S9(2)V9(3)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-DRAEN  PICTURE  9(8).                    CI0021
            11            PF31-PE6F                                     CI0021
                          OCCURS       006     TIMES.                   CI0021
            12            PF31-AFLEX  PICTURE  S9(3)V9(2)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-DFEED  PICTURE  9(8).                    CI0021
            11            PF31-APYCT  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-APYMT  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-ARGPA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-AROGP  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-ACYTA  PICTURE  S9(3)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-CRTYC  PICTURE  X(1).                    CI0021
            11            PF31-CRICO  PICTURE  X(3).                    CI0021
            11            PF31-CACOV  PICTURE  X(02).                   CI0021
            11            PF31-ACECP  PICTURE  S9(09)V99                CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-DMDOB2 PICTURE  X(8).                    CI0021
            11            PF31-DTERMG PICTURE  X(8).                    CI0021
            11            PF31-ASRBE  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-ATCDS                                    CI0021
                          REDEFINES            PF31-ASRBE               CI0021
               PICTURE    S9(9)V99                                      CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-ASRRM  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-AMMOB                                    CI0021
                          REDEFINES            PF31-ASRRM               CI0021
               PICTURE    S9(9)V99                                      CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-PASRM  PICTURE  999.                     CI0021
            11            PF31-ASRHL  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-AMMOI                                    CI0021
                          REDEFINES            PF31-ASRHL               CI0021
               PICTURE    S9(9)V99                                      CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-ALTCB  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-ALTCI  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-AAFAA  PICTURE  S9(3)V9(8)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-FILLER PICTURE  X(79).                   CI0021
            10            PF31-PE6S                                     CI0021
                          REDEFINES            PF31-PE32.               CI0021
            11            PF31-ISRRS  PICTURE  X(4).                    CI0021
            11            PF31-ISRFN  PICTURE  X.                       CI0021
            11            PF31-ISRRP  PICTURE  X.                       CI0021
            11            PF31-ISRGB  PICTURE  X.                       CI0021
            11            PF31-ISRRPN PICTURE  X.                       CI0021
            11            PF31-CSPPW  PICTURE  9(2).                    CI0021
            11            PF31-CSRPN  PICTURE  X(2).                    CI0021
            11            PF31-CSRPNB PICTURE  X(120).                  CI0021
            11            PF31-CSRPNA                                   CI0021
                          REDEFINES            PF31-CSRPNB              CI0021
                          OCCURS       020     TIMES.                   CI0021
            12            PF31-CSRPNR PICTURE  X(2).                    CI0021
            12            PF31-ARIPR  PICTURE  S9(2)V9(5)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-FILLER PICTURE  X(50).                   CI0021
            10            PF31-PE6E                                     CI0021
                          REDEFINES            PF31-PE32.               CI0021
            11            PF31-FILLER PICTURE  X(59).                   CI0021
            11            PF31-AGLPB  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-ASRCD  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-ASRCR  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-FILLER PICTURE  X(108).                  CI0021
            11            PF31-ASRHM  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            PF31-PE47                                     CI0021
                          REDEFINES            PF31-PE32.               CI0021
            11            PF31-CALRE  PICTURE  X.                       CI0021
            11            PF31-NFUIA.                                   CI0021
            12            PF31-NFUIO                                    CI0021
                          OCCURS       097     TIMES.                   CI0021
            13            PF31-NFUNR  PICTURE  S9(3)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            13            PF31-PDEDU  PICTURE  S9(4)V9                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-FILLER PICTURE  X(28674).                CI0021
            10            PF31-PE6K                                     CI0021
                          REDEFINES            PF31-PE32.               CI0021
            11            PF31-COTTC  PICTURE  X(2).                    CI0021
            11            PF31-DABAL  PICTURE  9(8).                    CI0021
            11            PF31-DAEAL  PICTURE  9(8).                    CI0021
            11            PF31-CDTFR  PICTURE  99.                      CI0021
            11            PF31-QTERS  PICTURE  9(3).                    CI0021
            11            PF31-DATRF  PICTURE  99.                      CI0021
            11            PF31-IOVRF  PICTURE  X.                       CI0021
            11            PF31-AOVFE  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            PF31-CSEMR  PICTURE  X.                       CI0021
            11            PF31-ILTOV  PICTURE  X(1).                    CI0021
            11            PF31-DATDT  PICTURE  9(8).                    CI0021
            11            PF31-DATDN  PICTURE  9(8).                    CI0021
            11            PF31-CMEMO  PICTURE  X(2).                    CI0021
            11            PF31-CAREA  PICTURE  X(8).                    CI0021
            11            PF31-CBADT  PICTURE  X(04).                   CI0021
            11            PF31-FILLER PICTURE  X(296).                  CI0021
            11            PF31-PE26                                     CI0021
                          OCCURS       097     TIMES.                   CI0021
            12            PF31-ITFTI  PICTURE  X.                       CI0021
            12            PF31-NFUNX  PICTURE  S9(3)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CAMTX  PICTURE  X.                       CI0021
            12            PF31-ACOTA  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-ACOTA1                                   CI0021
                          REDEFINES            PF31-ACOTA               CI0021
               PICTURE    S9(6)V9(5)                                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PALLO  PICTURE  S999V9                   CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PALLO1                                   CI0021
                          REDEFINES            PF31-PALLO               CI0021
               PICTURE    S9V999                                        CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-FILLER PICTURE  X(18).                   CI0021
            10            PF31-PE6M                                     CI0021
                          REDEFINES            PF31-PE32.               CI0021
            11            PF31-PE6J                                     CI0021
                          OCCURS       024     TIMES.                   CI0021
            12            PF31-CASTA6 PICTURE  X.                       CI0021
            12            PF31-CDPFC  PICTURE  S9(5)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CDPTC  PICTURE  X(4).                    CI0021
            12            PF31-CMEMO  PICTURE  X(2).                    CI0021
            12            PF31-IRENR  PICTURE  X(01).                   CI0021
            12            PF31-QDDAM  PICTURE  S9(3)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-DDEPO  PICTURE  9(8).                    CI0021
            12            PF31-DTIRNN PICTURE  9(8).                    CI0021
            12            PF31-DTIRNL PICTURE  9(8).                    CI0021
            12            PF31-DTIRN  PICTURE  9(8).                    CI0021
            12            PF31-DTERT  PICTURE  9(8).                    CI0021
            12            PF31-ADVAL  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PINRA  PICTURE  S9(3)V9(6)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PCTIU  PICTURE  S9V9(4)                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PCTCP  PICTURE  S9V9(4)                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PCTCPR                                   CI0021
                          REDEFINES            PF31-PCTCP               CI0021
               PICTURE    S9(3)V99                                      CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AGAPJ  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PPARN  PICTURE  S9V9(4)                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-CGNTC  PICTURE  X(4).                    CI0021
            12            PF31-CGNFC  PICTURE  S9(5)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-DINDL  PICTURE  9(8).                    CI0021
            12            PF31-IRENRV PICTURE  X(01).                   CI0021
            12            PF31-AMTRLC PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AMTRL  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AMTRLW PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AMTRLO PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AMTAV  PICTURE  S9(9)V9(2)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AMTAVM PICTURE  S9(9)V9(2)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AMTAVF PICTURE  S9(9)V9(2)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-PENRA  PICTURE  S9(3)V9(6)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AICRI  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-AROFA  PICTURE  S9V9(4)                  CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            PF31-FILLER PICTURE  X(70).                   CI0021
            11            PF31-CFNDC  PICTURE  S9(3)                    CI0021
                          OCCURS       024     TIMES                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            PF31-PE7S                                     CI0021
                          REDEFINES            PF31-PE32.               CI0021
            11            PF31-CVSTC1 PICTURE  X(4).                    CI0021
            11            PF31-GXRTN1                                   CI0021
                          OCCURS       250     TIMES.                   CI0021
            12            PF31-NFUNDL PICTURE  999.                     CI0021
            12            PF31-CFIDC4 PICTURE  X(3).                    CI0021
            12            PF31-CTFIN  PICTURE  X.                       CI0021
            12            PF31-NFDNM  PICTURE  X(20).                   CI0021
            12            PF31-FILLER PICTURE  X(40).                   CI0021
            10            PF31-INDSP  PICTURE  X.                       CI0021
       01          VANTAGE-ACCESS-MODULE  PIC X(8)                      AAZ000
                           VALUE         'LPAZ000 '.                    AAZ000
      *
      *ASSIGN POLICY NUMBER TO VANTAGE *
       01  7-PF30-GVAID.
           05  7-PF30-FILLER                PIC X(4)   VALUE SPACES.
           05  7-PF30-CTIDND                PIC X(11).

       01                 CM01.                                         CI0021
            10            CM01-CEKCNT.                                  CI0021
            11            CM01-CEGCN.                                   CI0021
            12            CM01-CEPRE  PICTURE  9(4).                    CI0021
            12            CM01-CEBAS  PICTURE  9(8).                    CI0021
            10            CM01-CEMAD  PICTURE  9(8).                    CI0021
            10            CM01-CEAUD  PICTURE  9(8).                    CI0021
            10            CM01-CEBKD  PICTURE  9(8).                    CI0021
            10            CM01-CELAD  PICTURE  9(8).                    CI0021
            10            CM01-CECSD  PICTURE  9(8).                    CI0021
            10            CM01-CECOM  PICTURE  9(2).                    CI0021
            10            CM01-CEPDT  PICTURE  9(4).                    CI0021
            10            CM01-CECHK  PICTURE  9(1).                    CI0021
            10            CM01-CECTC  PICTURE  X(1).                    CI0021
            10            CM01-CEST   PICTURE  9(1).                    CI0021
            10            CM01-CETYP  PICTURE  9(2).                    CI0021
            10            CM01-CECLO  PICTURE  9(1).                    CI0021
            10            CM01-CEAV   PICTURE  9(1).                    CI0021
            10            CM01-CE30M  PICTURE  X(1).                    CI0021
            10            CM01-CEBCD  PICTURE  S9(3)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-CEUNT  PICTURE  S9(5)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-CEFAC  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-CERD   PICTURE  9(3).                    CI0021
            10            CM01-CEDST  PICTURE  9(2).                    CI0021
            10            CM01-CEIPN  PICTURE  9(1).                    CI0021
            10            CM01-CEMAL  PICTURE  X(1).                    CI0021
            10            CM01-CEMALC PICTURE  9(2).                    CI0021
            10            CM01-CEUNL  PICTURE  X(1).                    CI0021
            10            CM01-CEMLN  PICTURE  X(1).                    CI0021
            10            CM01-CEYEX  PICTURE  9(2).                    CI0021
            10            CM01-CELLO  PICTURE  9(1).                    CI0021
            10            CM01-CESLD  PICTURE  9(8).                    CI0021
            10            CM01-CELAT.                                   CI0021
            11            CM01-CETMJ  PICTURE  9(3).                    CI0021
            11            CM01-CETMN  PICTURE  9(3).                    CI0021
            10            CM01-CELATD PICTURE  9(8).                    CI0021
            10            CM01-CEYER  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-CEPYR  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-CELIN  PICTURE  X(1).                    CI0021
            10            CM01-CEANP  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-FILLER PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-FILLER PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-CELBD  PICTURE  9(8).                    CI0021
            10            CM01-CELMC  PICTURE  9(3).                    CI0021
            10            CM01-CEXNO  PICTURE  S9(9)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-CEORS  PICTURE  9(2).                    CI0021
            10            CM01-CESST  PICTURE  9(2).                    CI0021
            10            CM01-CESAG  PICTURE  9(2).                    CI0021
            10            CM01-CEOSD  PICTURE  9(3).                    CI0021
            10            CM01-CEORD  PICTURE  9(3).                    CI0021
            10            CM01-CECLS  PICTURE  9(2).                    CI0021
            10            CM01-CEIND  PICTURE  9(3).                    CI0021
            10            CM01-CEOCC  PICTURE  9(3).                    CI0021
            10            CM01-CEASI  PICTURE  X(1).                    CI0021
            10            CM01-CESUB  PICTURE  9(3)                     CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-CERDC  PICTURE  9(1).                    CI0021
            10            CM01-CECEA  PICTURE  S9(13)                   CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-CETEA  PICTURE  S9(13)                   CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-FILLER PICTURE  X(1).                    CI0021
            10            CM01-COPDC  PICTURE  9.                       CI0021
            10            CM01-FILLER PICTURE  9(1).                    CI0021
            10            CM01-CEOWN  PICTURE  9(2).                    CI0021
            10            CM01-CESSC  PICTURE  9(2).                    CI0021
            10            CM01-CESSI  PICTURE  X(1).                    CI0021
            10            CM01-CECID  PICTURE  9(8).                    CI0021
            10            CM01-CECO   PICTURE  9(2).                    CI0021
            10            CM01-DSLRT  PICTURE  9(8).                    CI0021
            10            CM01-CEYPR  PICTURE  9(2).                    CI0021
            10            CM01-CEIPCB PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-CETTR  PICTURE  X(1).                    CI0021
            10            CM01-CESCM  PICTURE  9(2).                    CI0021
            10            CM01-CEBSA  PICTURE  X(1).                    CI0021
            10            CM01-CESX   PICTURE  9(1).                    CI0021
            10            CM01-CERZC  PICTURE  S9(9)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-CERBD  PICTURE  9(8).                    CI0021
            10            CM01-CECUS  PICTURE  9(2).                    CI0021
            10            CM01-CEDUW  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-CEDCB  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-CTIDA1 PICTURE  9(3).                    CI0021
            10            CM01-IBPR1  PICTURE  9.                       CI0021
            10            CM01-IBPR2  PICTURE  9.                       CI0021
            10            CM01-IBPR3  PICTURE  9.                       CI0021
            10            CM01-IBPR4  PICTURE  9.                       CI0021
            10            CM01-IBPR0  PICTURE  9.                       CI0021
            10            CM01-CEXP   PICTURE  9(2).                    CI0021
            10            CM01-FILLER PICTURE  S9(7)V99                 CI0021
                          OCCURS       004     TIMES                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM01-GRPLT  PICTURE  99.                      CI0021
            10            CM01-FILLER PICTURE  X(16).                   CI0021
       01                 CM24.                                         CI0021
            10            CM24-CELLT.                                   CI0021
            11            CM24-CETMJ  PICTURE  9(3).                    CI0021
            11            CM24-CETMN  PICTURE  9(3).                    CI0021
            10            CM24-CELLD  PICTURE  9(8).                    CI0021
            10            CM24-CELFD  PICTURE  9(8).                    CI0021
            10            CM24-CELPD  PICTURE  9(8).                    CI0021
            10            CM24-CELDD  PICTURE  9(8).                    CI0021
            10            CM24-CELBL  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM24-FILLER PICTURE  S9(6)V9(7)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM24-CELBD  PICTURE  9(8).                    CI0021
            10            CM24-FILLER PICTURE  S9(6)V9(7)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM24-CELIR  PICTURE  SV9(5)                   CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM24-CELLBA PICTURE  S9(7)V9(2)               CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM24-CEL12  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM24-FILLER PICTURE  S9(7)V99                 CI0021
                          OCCURS       002     TIMES                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            CM24-FILLER PICTURE  X(10).                   CI0021
       01                 CM54.                                         CI0021
            10            CM54-CEKLBL.                                  CI0021
            11            CM54-CESQ2  PICTURE  9(2).                    CI0021
            10            CM54-CEGLB                                    CI0021
                          OCCURS       010     TIMES.                   CI0021
            11            CM54-CELBDT PICTURE  9(8).                    CI0021
            11            CM54-CELBA  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            CM54-CEPTA  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            CM54-CEPTC1.                                  CI0021
            12            CM54-CEPTC.                                   CI0021
            13            CM54-CETMJ  PICTURE  9(3).                    CI0021
            13            CM54-CETMN  PICTURE  9(3).                    CI0021
            12            CM54-FILLER PICTURE  X(2).                    CI0021
            11            CM54-DTRME                                    CI0021
                          REDEFINES            CM54-CEPTC1              CI0021
               PICTURE    9(8).                                         CI0021
            11            CM54-FILLER                                   CI0021
                          REDEFINES            CM54-CEPTC1.             CI0021
            12            CM54-ACAPI  PICTURE  S9(9)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            12            CM54-FILLER PICTURE  X(2).                    CI0021
            11            CM54-NBSEI  PICTURE  999V99                   CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            CM54-FILLER PICTURE  X(2).                    CI0021
            11            CM54-NESEI  PICTURE  999V99                   CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            CM54-ACMTH                                    CI0021
                          REDEFINES            CM54-NESEI               CI0021
               PICTURE    S9(5)                                         CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            CM54-CETLB  PICTURE  9(3).                    CI0021
            11            CM54-FILLER PICTURE  X(3).                    CI0021
            10            CM54-FILLER PICTURE  X(1).                    CI0021
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0021
            10            XW05-XW06.                                    CI0021
            11            XW05-XDBPCB.                                  CI0021
            12            XW05-XDBDNM PICTURE  X(08)                    CI0021
                          VALUE                SPACE.                   CI0021
            12            XW05-XSEGLV PICTURE  X(02)                    CI0021
                          VALUE                SPACE.                   CI0021
            12            XW05-XRC    PICTURE  X(02)                    CI0021
                          VALUE                SPACE.                   CI0021
            12            XW05-XPROPT PICTURE  X(04)                    CI0021
                          VALUE                SPACE.                   CI0021
            12            XW05-FILLER PICTURE  S9(5)                    CI0021
                          VALUE                ZERO                     CI0021
                          BINARY.                                       CI0021
            12            XW05-XSEGNM PICTURE  X(08)                    CI0021
                          VALUE                SPACE.                   CI0021
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0021
                          VALUE                ZERO                     CI0021
                          BINARY.                                       CI0021
            12            XW05-XSEGNB PICTURE  9(05)                    CI0021
                          VALUE                ZERO                     CI0021
                          BINARY.                                       CI0021
            12            XW05-XCOKEY PICTURE  X(70)                    CI0021
                          VALUE                SPACE.                   CI0021
            10            XW05-XW07.                                    CI0021
            11            XW05-XIOPCB.                                  CI0021
            12            XW05-XTERMI PICTURE  X(08)                    CI0021
                          VALUE                SPACE.                   CI0021
            12            XW05-FILLER PICTURE  XX                       CI0021
                          VALUE                SPACE.                   CI0021
            12            XW05-XRC1   PICTURE  X(02)                    CI0021
                          VALUE                SPACE.                   CI0021
            12            XW05-FILLER PICTURE  X(12)                    CI0021
                          VALUE                SPACE.                   CI0021
            12            XW05-XMODNM PICTURE  X(8)                     CI0021
                          VALUE                SPACE.                   CI0021
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0021
                          VALUE                ZERO.                    CI0021
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0021
                          VALUE                ZERO.                    CI0021
            10            XW05-XGU    PICTURE  X(4)                     CI0021
                          VALUE                'GU  '.                  CI0021
            10            XW05-XGHU   PICTURE  X(4)                     CI0021
                          VALUE                'GHU '.                  CI0021
            10            XW05-XGN    PICTURE  X(4)                     CI0021
                          VALUE                'GN  '.                  CI0021
            10            XW05-XGHN   PICTURE  X(4)                     CI0021
                          VALUE                'GHN '.                  CI0021
            10            XW05-XGNP   PICTURE  X(4)                     CI0021
                          VALUE                'GNP '.                  CI0021
            10            XW05-XGHNP  PICTURE  X(4)                     CI0021
                          VALUE                'GHNP'.                  CI0021
            10            XW05-XREPL  PICTURE  XXXX                     CI0021
                          VALUE                'REPL'.                  CI0021
            10            XW05-XISRT  PICTURE  X(4)                     CI0021
                          VALUE                'ISRT'.                  CI0021
            10            XW05-XDLET  PICTURE  X(4)                     CI0021
                          VALUE                'DLET'.                  CI0021
            10            XW05-XOPEN  PICTURE  X(4)                     CI0021
                          VALUE                'OPEN'.                  CI0021
            10            XW05-XCLSE  PICTURE  X(4)                     CI0021
                          VALUE                'CLSE'.                  CI0021
            10            XW05-XCHKP  PICTURE  X(4)                     CI0021
                          VALUE                'CHKP'.                  CI0021
            10            XW05-XXRST  PICTURE  X(4)                     CI0021
                          VALUE                'XRST'.                  CI0021
            10            XW05-XTERM  PICTURE  X(4)                     CI0021
                          VALUE                'TERM'.                  CI0021
            10            XW05-XNFPAC PICTURE  X(13)                    CI0021
                          VALUE                SPACE.                   CI0021
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0021
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0021
      *
      ******************************************************************
      **     TABLE INDICES                                             *
      ******************************************************************
      *
      *                   CM54
      *
      *
      *
      *
       01  7-92-WORK-DATE.                                              AAOLBB
           05  7-92-WORK-CC            PIC 9(02).                       AAOLBB
           05  7-92-WORK-YY            PIC 9(02).                       AAOLBB
           05  7-92-WORK-MM            PIC 9(02).                       AAOLBB
           05  7-92-WORK-DD            PIC 9(02).                       AAOLBB
       01  7-92-ANN-DATE.                                               AAOLBB
           05  7-92-ANN-CC            PIC 9(02).                        AAOLBB
           05  7-92-ANN-YY            PIC 9(02).                        AAOLBB
           05  7-92-ANN-MM            PIC 9(02).                        AAOLBB
           05  7-92-ANN-DD            PIC 9(02).                        AAOLBB
       01  7-92-RESULT            PIC 9(02).                            AAOLBB
       01  7-92-REM               PIC 9(02).                            AAOLBB
       01  7-92-PREMIUM           PIC S9(7)V99   VALUE ZEROES.          AAOLBB
       01  7-92-PREMIUM-T         PIC S9(7)V99   VALUE ZEROES.          AAOLBB
       01  7-92-PREMIUM-O         PIC S9(7)V99   VALUE ZEROES.          AAOLBB
       01  7-92-PREMIUMS          PIC 9(7)V99    VALUE ZEROES.          AAOLBB
       01  7-92-CALC  REDEFINES 7-92-PREMIUMS.                          AAOLBB
           05  FILLER             PIC 999999.                           AAOLBB
           05  7-92-ONE           PIC 9V99.                             AAOLBB
       01  7-92-TENS              PIC 9(7)V99.                          AAOLBB
       01  7-92-ONES.                                                   AAOLBB
           05  7-92-CONES         PIC 9V9.                              AAOLBB
           05  FILLER             PIC 9.                                AAOLBB
       01  7-92-FL-DI             PIC X      VALUE 'N'.                 AAOLBB
       01  7-92-ALCIDN.                                                 AAOLBB
           05  7-92-FIRST-4       PIC 9(4).                             AAOLBB
           05  7-92-REST          PIC 9(7).                             AAOLBB
       01  7-92-PRSCD.                                                  AAOLBB
           05  7-92-DE-CODE       PIC 9(2).                             AAOLBB
           05  7-92-FGH-CODE      PIC 9(3).                             AAOLBB
           05  7-92-REST          PIC 9(4).                             AAOLBB
       01  7-92-TL-2012           PIC X      VALUE 'N'.                 AAOLBB
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
      **              TABLE TA98 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA98-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=98 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA98.                                                CI0021
           04    G-TA98-PARAM.                                          CI0021
             10  G-TA98-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0021
                        VALUE      +053.                                CI0021
             10  G-TA98-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0021
                        VALUE      +001.                                CI0021
             10  G-TA98-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0021
                        VALUE      +010.                                CI0021
             10  G-TA98-NUAPP  PICTURE 99                               CI0021
                        VALUE       0.                                  CI0021
             10  G-TA98-NUTAB  PICTURE X(6)                             CI0021
                        VALUE 'GCPRAR'.                                 CI0021
             10  G-TA98-TABFO  PICTURE XX                 VALUE SPACE.  CI0021
             10  G-TA98-TABCR  PICTURE XX                 VALUE SPACE.  CI0021
             10  G-TA98-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0021
             10  G-TA98-NUSSC  PICTURE X  VALUE   ' '.                  CI0021
             10  G-TA98-NUSSY  PICTURE X                  VALUE SPACE.  CI0021
             10  G-TA98-TRANID PICTURE X(4)               VALUE SPACE.  CI0021
             10  G-TA98-FILSYS.                                         CI0021
             15  G-TA98-USERC  PICTURE X(6)               VALUE SPACE.  CI0021
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0021
           04             TA98.                                         CI0021
            10            TA98-GCPRAR.                                  CI0021
            11            TA98-CTIDA  PICTURE  9(3)                     CI0021
                          VALUE                ZERO.                    CI0021
            11            TA98-PRCOD  PICTURE  9(5)                     CI0021
                          VALUE                ZERO.                    CI0021
            11            TA98-CARTY  PICTURE  99                       CI0021
                          VALUE                ZERO.                    CI0021
            10            TA98-IARTYA PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IARLNA PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQAN  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQSA  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQFM  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQQT  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQBM  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQMO  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQBF  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQSM  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQBW  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQWK  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQIR  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQIF  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQIS  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQIK  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQIW  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQOD  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IARPSA PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IARRGA PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IFQET  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IMLNA  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-IMPRA  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TA98-ZDA20  PICTURE  X(20)                    CI0021
                          VALUE                SPACE.                   CI0021
      **                                                                ADUTAB
      ******************************************************************ADUTAB
      **              TABLE TB96 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TB96-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TB DSL=TA SEL=96 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TB96.                                                CI0021
           04    G-TB96-PARAM.                                          CI0021
             10  G-TB96-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0021
                        VALUE      +029.                                CI0021
             10  G-TB96-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0021
                        VALUE      +001.                                CI0021
             10  G-TB96-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0021
                        VALUE      +011.                                CI0021
             10  G-TB96-NUAPP  PICTURE 99                               CI0021
                        VALUE       0.                                  CI0021
             10  G-TB96-NUTAB  PICTURE X(6)                             CI0021
                        VALUE 'TA0096'.                                 CI0021
             10  G-TB96-TABFO  PICTURE XX                 VALUE SPACE.  CI0021
             10  G-TB96-TABCR  PICTURE XX                 VALUE SPACE.  CI0021
             10  G-TB96-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0021
             10  G-TB96-NUSSC  PICTURE X  VALUE   ' '.                  CI0021
             10  G-TB96-NUSSY  PICTURE X                  VALUE SPACE.  CI0021
             10  G-TB96-TRANID PICTURE X(4)               VALUE SPACE.  CI0021
             10  G-TB96-FILSYS.                                         CI0021
             15  G-TB96-USERC  PICTURE X(6)               VALUE SPACE.  CI0021
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0021
           04             TB96.                                         CI0021
            10            TB96-GAPRT.                                   CI0021
            11            TB96-CTIDA  PICTURE  9(3)                     CI0021
                          VALUE                ZERO.                    CI0021
            11            TB96-PRCOD  PICTURE  9(5)                     CI0021
                          VALUE                ZERO.                    CI0021
            11            TB96-CPMTCA PICTURE  XXX                      CI0021
                          VALUE                SPACE.                   CI0021
            10            TB96-AMINA  PICTURE  S9(7)V99                 CI0021
                          VALUE                ZERO.                    CI0021
            10            TB96-AMAXA  PICTURE  S9(7)V99                 CI0021
                          VALUE                ZERO.                    CI0021
      **                                                                ADUTAB
      ******************************************************************ADUTAB
      **              TABLE TC96 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TC96-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TC DSL=TA SEL=96 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TC96.                                                CI0021
           04    G-TC96-PARAM.                                          CI0021
             10  G-TC96-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0021
                        VALUE      +029.                                CI0021
             10  G-TC96-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0021
                        VALUE      +001.                                CI0021
             10  G-TC96-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0021
                        VALUE      +011.                                CI0021
             10  G-TC96-NUAPP  PICTURE 99                               CI0021
                        VALUE       0.                                  CI0021
             10  G-TC96-NUTAB  PICTURE X(6)                             CI0021
                        VALUE 'TA0096'.                                 CI0021
             10  G-TC96-TABFO  PICTURE XX                 VALUE SPACE.  CI0021
             10  G-TC96-TABCR  PICTURE XX                 VALUE SPACE.  CI0021
             10  G-TC96-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0021
             10  G-TC96-NUSSC  PICTURE X  VALUE   ' '.                  CI0021
             10  G-TC96-NUSSY  PICTURE X                  VALUE SPACE.  CI0021
             10  G-TC96-TRANID PICTURE X(4)               VALUE SPACE.  CI0021
             10  G-TC96-FILSYS.                                         CI0021
             15  G-TC96-USERC  PICTURE X(6)               VALUE SPACE.  CI0021
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0021
           04             TC96.                                         CI0021
            10            TC96-GAPRT.                                   CI0021
            11            TC96-CTIDA  PICTURE  9(3)                     CI0021
                          VALUE                ZERO.                    CI0021
            11            TC96-PRCOD  PICTURE  9(5)                     CI0021
                          VALUE                ZERO.                    CI0021
            11            TC96-CPMTCA PICTURE  XXX                      CI0021
                          VALUE                SPACE.                   CI0021
            10            TC96-AMINA  PICTURE  S9(7)V99                 CI0021
                          VALUE                ZERO.                    CI0021
            10            TC96-AMAXA  PICTURE  S9(7)V99                 CI0021
                          VALUE                ZERO.                    CI0021
      **                                                                ADUTAB
      ******************************************************************ADUTAB
      **              TABLE TD96 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TD96-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TD DSL=TA SEL=96 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TD96.                                                CI0021
           04    G-TD96-PARAM.                                          CI0021
             10  G-TD96-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0021
                        VALUE      +029.                                CI0021
             10  G-TD96-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0021
                        VALUE      +001.                                CI0021
             10  G-TD96-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0021
                        VALUE      +011.                                CI0021
             10  G-TD96-NUAPP  PICTURE 99                               CI0021
                        VALUE       0.                                  CI0021
             10  G-TD96-NUTAB  PICTURE X(6)                             CI0021
                        VALUE 'TA0096'.                                 CI0021
             10  G-TD96-TABFO  PICTURE XX                 VALUE SPACE.  CI0021
             10  G-TD96-TABCR  PICTURE XX                 VALUE SPACE.  CI0021
             10  G-TD96-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0021
             10  G-TD96-NUSSC  PICTURE X  VALUE   ' '.                  CI0021
             10  G-TD96-NUSSY  PICTURE X                  VALUE SPACE.  CI0021
             10  G-TD96-TRANID PICTURE X(4)               VALUE SPACE.  CI0021
             10  G-TD96-FILSYS.                                         CI0021
             15  G-TD96-USERC  PICTURE X(6)               VALUE SPACE.  CI0021
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0021
           04             TD96.                                         CI0021
            10            TD96-GAPRT.                                   CI0021
            11            TD96-CTIDA  PICTURE  9(3)                     CI0021
                          VALUE                ZERO.                    CI0021
            11            TD96-PRCOD  PICTURE  9(5)                     CI0021
                          VALUE                ZERO.                    CI0021
            11            TD96-CPMTCA PICTURE  XXX                      CI0021
                          VALUE                SPACE.                   CI0021
            10            TD96-AMINA  PICTURE  S9(7)V99                 CI0021
                          VALUE                ZERO.                    CI0021
            10            TD96-AMAXA  PICTURE  S9(7)V99                 CI0021
                          VALUE                ZERO.                    CI0021
      **                                                                ADUTAB
      ******************************************************************ADUTAB
      **              TABLE TF5B ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TF5B-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TF DSL=TA SEL=5B FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TF5B.                                                CI0021
           04    G-TF5B-PARAM.                                          CI0021
             10  G-TF5B-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0021
                        VALUE      +154.                                CI0021
             10  G-TF5B-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0021
                        VALUE      +001.                                CI0021
             10  G-TF5B-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0021
                        VALUE      +017.                                CI0021
             10  G-TF5B-NUAPP  PICTURE 99                               CI0021
                        VALUE       0.                                  CI0021
             10  G-TF5B-NUTAB  PICTURE X(6)                             CI0021
                        VALUE 'TA005B'.                                 CI0021
             10  G-TF5B-TABFO  PICTURE XX                 VALUE SPACE.  CI0021
             10  G-TF5B-TABCR  PICTURE XX                 VALUE SPACE.  CI0021
             10  G-TF5B-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0021
             10  G-TF5B-NUSSC  PICTURE X  VALUE   ' '.                  CI0021
             10  G-TF5B-NUSSY  PICTURE X                  VALUE SPACE.  CI0021
             10  G-TF5B-TRANID PICTURE X(4)               VALUE SPACE.  CI0021
             10  G-TF5B-FILSYS.                                         CI0021
             15  G-TF5B-USERC  PICTURE X(6)               VALUE SPACE.  CI0021
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0021
           04             TF5B.                                         CI0021
            10            TF5B-GAPSC.                                   CI0021
            11            TF5B-CTIDA  PICTURE  9(3)                     CI0021
                          VALUE                ZERO.                    CI0021
            11            TF5B-PRCOD  PICTURE  9(5)                     CI0021
                          VALUE                ZERO.                    CI0021
            11            TF5B-PRSCD  PICTURE  X(9)                     CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-PRCODX PICTURE  9(5)                     CI0021
                          VALUE                ZERO.                    CI0021
            10            TF5B-PRCSUB PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-PRCAUT PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-PRCBAS PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-PRCSTK PICTURE  XX                       CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-PRCPRE PICTURE  X(4)                     CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-IBDUP  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-IUSPR  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-CVSYS  PICTURE  X(2)                     CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-IDTOD  PICTURE  X(1)                     CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-GRSFC  PICTURE  99                       CI0021
                          VALUE                ZERO.                    CI0021
            10            TF5B-ZDA18  PICTURE  X(18)                    CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-CMPCTB PICTURE  X(4)                     CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-ITERM  PICTURE  X(1)                     CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-AMFAC  PICTURE  S9(7)                    CI0021
                          VALUE                ZERO.                    CI0021
            10            TF5B-ZDA20  PICTURE  X(20)                    CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-CPRBK  PICTURE  X(3)                     CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-CFXDM  PICTURE  99                       CI0021
                          VALUE                ZERO.                    CI0021
            10            TF5B-NGLCS  PICTURE  X(5)                     CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-NDFCS  PICTURE  X(5)                     CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-ZDA20  PICTURE  X(20)                    CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-CTNLI  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-CBANK  PICTURE  X(03)                    CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-ISYPO  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-ISYPP  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-ICOPT  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-IANPY  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-IDSAR  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-ICIPT  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-IANDS  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-IKPMA  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-INMWT  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-IVANT  PICTURE  X(1)                     CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-ISDAV  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-IUDAV  PICTURE  X                        CI0021
                          VALUE                SPACE.                   CI0021
            10            TF5B-ZDA15  PICTURE  X(15)                    CI0021
                          VALUE                SPACE.                   CI0021
      **                                                                ADUTAB

      ******************************************************************
      **     WORKING STORAGE MISC FIELDS                               *
      ******************************************************************

       01  W-WORK-MISC.

      *    WORK AREA FOR REFORMATTING ACCT ID TO READ CERT MASTER
           05  W-WORK-CTIDND.
      *!WI
               10  W-WORK-NPREF
                        PICTURE 9(4).                                   CI0021
      *!WI
               10  W-WORK-NBASE
                        PICTURE 9(7).                                   CI0021

      *    *************************************
      *    INDICATORS FOR TA96 TABLE ENTRY FOUND
      *    '0' = FOUND
      *    '1' = NOT FOUND
      *    *************************************

      *    'LON'
           05  TB96-IK           PIC X.

      *    'ADD'
           05  TC96-IK           PIC X.

      *    'NEW'
           05  TD96-IK           PIC X.

      *    INDICATES IF TA5B TABLE ENTRY FOUND
      *    '0' = NOT FOUND
      *    '1' = FOUND
           05  TF5B-CF           PIC X.

       01   DEBUT-WSS.                                                  CI0021
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0021
            05   IK     PICTURE X.                                      CI0021
       01  CONSTANTES-PAC.                                              CI0021
           05  FILLER  PICTURE X(87)   VALUE                            CI0021
                     '6015 CAT09/08/14CI0021ADMIN   14:34:16CI0021P AMERCI0021
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0021
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0021
           05  NUGNA   PICTURE X(5).                                    CI0021
           05  APPLI   PICTURE X(3).                                    CI0021
           05  DATGN   PICTURE X(8).                                    CI0021
           05  PROGR   PICTURE X(6).                                    CI0021
           05  CODUTI  PICTURE X(8).                                    CI0021
           05  TIMGN   PICTURE X(8).                                    CI0021
           05  PROGE   PICTURE X(8).                                    CI0021
           05  COBASE  PICTURE X(4).                                    CI0021
           05  DATGNC  PICTURE X(10).                                   CI0021
           05  RELEAS  PICTURE X(7).                                    CI0021
           05  DATGE   PICTURE X(10).                                   CI0021
           05  DATSQ   PICTURE X(10).                                   CI0021
       01  DATCE.                                                       CI0021
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0021
         05  DATOR.                                                     CI0021
           10  DATOA  PICTURE XX.                                       CI0021
           10  DATOM  PICTURE XX.                                       CI0021
           10  DATOJ  PICTURE XX.                                       CI0021
       01   VARIABLES-CONDITIONNELLES.                                  CI0021
            05                  FT      PICTURE X VALUE '0'.            CI0021
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0021
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0021
            05           ICM54L PICTURE S9(4) VALUE  ZERO.
            05           ICM54R PICTURE S9(4) VALUE  ZERO.
            05           ICM54M PICTURE S9(4) VALUE +0010.
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0021
            05       5-AA00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0021
            05       5-CM00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0021
       01               S-AA10-SSA.                                     CI0021
            10         S1-AA10-SEGNAM PICTURE X(8)                      CI0021
                                      VALUE 'LMSPCON '.                 CI0021
            10         S1-AA10-CCOM   PICTURE X VALUE '*'.              CI0021
            10          S-AA10-CCOD   PICTURE X(5)                      CI0021
                                      VALUE '-----'.                    CI0021
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0021
       01            S-AAU10-SSA.                                       CI0021
            11      S1-AAU10-SEGNAM PICTURE X(8)                        CI0021
                                      VALUE 'LMSPCON '.                 CI0021
            11      S1-AAU10-CCOM   PICTURE X VALUE '*'.                CI0021
            11       S-AAU10-CCOD   PICTURE X(5)                        CI0021
                                      VALUE '-----'.                    CI0021
            11      S1-AAU10-FLDNAM PICTURE X(9)                        CI0021
                                      VALUE '(LMSPCONK'.                CI0021
            11       S-AAU10-OPER  PICTURE XX VALUE ' ='.               CI0021
            11       S-AAU10-ALCIDN   PICTURE  9(11).                   CI0021
            11  FILLER   PICTURE X    VALUE ')'.                        CI0021
       01               S-AA20-SSA.                                     CI0021
            10         S1-AA20-SEGNAM PICTURE X(8)                      CI0021
                                      VALUE 'LMSPLON '.                 CI0021
            10         S1-AA20-CCOM   PICTURE X VALUE '*'.              CI0021
            10          S-AA20-CCOD   PICTURE X(5)                      CI0021
                                      VALUE '-----'.                    CI0021
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0021
       01               S-AA25-SSA.                                     CI0021
            10         S1-AA25-SEGNAM PICTURE X(8)                      CI0021
                                      VALUE 'LMSPANA '.                 CI0021
            10         S1-AA25-CCOM   PICTURE X VALUE '*'.              CI0021
            10          S-AA25-CCOD   PICTURE X(5)                      CI0021
                                      VALUE '-----'.                    CI0021
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0021
       01               S-AA66-SSA.                                     CI0021
            10         S1-AA66-SEGNAM PICTURE X(8)                      CI0021
                                      VALUE 'LMSPTLN '.                 CI0021
            10         S1-AA66-CCOM   PICTURE X VALUE '*'.              CI0021
            10          S-AA66-CCOD   PICTURE X(5)                      CI0021
                                      VALUE '-----'.                    CI0021
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0021
       01               S-CM01-SSA.                                     CI0021
            10         S1-CM01-SEGNAM PICTURE X(8)                      CI0021
                                      VALUE 'CACPCNT'.                  CI0021
            10         S1-CM01-CCOM   PICTURE X VALUE '*'.              CI0021
            10          S-CM01-CCOD   PICTURE X(5)                      CI0021
                                      VALUE '-----'.                    CI0021
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0021
       01            S-CMA01-SSA.                                       CI0021
            10      S1-CMA01-SEGNAM PICTURE X(8)                        CI0021
                                      VALUE 'CACPCNT'.                  CI0021
            10      S1-CMA01-CCOM   PICTURE X VALUE '*'.                CI0021
            10       S-CMA01-CCOD   PICTURE X(5)                        CI0021
                                      VALUE '-----'.                    CI0021
            10      S1-CMA01-FLDNAM PICTURE X(9)                        CI0021
                                      VALUE '(CACPPRDK'.                CI0021
            10       S-CMA01-OPER  PICTURE XX VALUE ' ='.               CI0021
            10       S-CMA01-CEPDT    PICTURE  9(4).                    CI0021
            10  FILLER   PICTURE X    VALUE ')'.                        CI0021
       01            S-CMU01-SSA.                                       CI0021
            10      S1-CMU01-SEGNAM PICTURE X(8)                        CI0021
                                      VALUE 'CACPCNT'.                  CI0021
            10      S1-CMU01-CCOM   PICTURE X VALUE '*'.                CI0021
            10       S-CMU01-CCOD   PICTURE X(5)                        CI0021
                                      VALUE '-----'.                    CI0021
            10      S1-CMU01-FLDNAM PICTURE X(9)                        CI0021
                                      VALUE '(CACPCNTK'.                CI0021
            10       S-CMU01-OPER  PICTURE XX VALUE ' ='.               CI0021
            10       S-CMU01-CEKCNT.                                    CI0021
            11       S-CMU01-CEGCN.                                     CI0021
            12       S-CMU01-CEPRE    PICTURE  9(4).                    CI0021
            12       S-CMU01-CEBAS    PICTURE  9(8).                    CI0021
            10  FILLER   PICTURE X    VALUE ')'.                        CI0021
       01               S-CM24-SSA.                                     CI0021
            10         S1-CM24-SEGNAM PICTURE X(8)                      CI0021
                                      VALUE 'CACPLON'.                  CI0021
            10         S1-CM24-CCOM   PICTURE X VALUE '*'.              CI0021
            10          S-CM24-CCOD   PICTURE X(5)                      CI0021
                                      VALUE '-----'.                    CI0021
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0021
       01               S-CM54-SSA.                                     CI0021
            10         S1-CM54-SEGNAM PICTURE X(8)                      CI0021
                                      VALUE 'CACPLBL'.                  CI0021
            10         S1-CM54-CCOM   PICTURE X VALUE '*'.              CI0021
            10          S-CM54-CCOD   PICTURE X(5)                      CI0021
                                      VALUE '-----'.                    CI0021
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0021
       01            S-CMU54-SSA.                                       CI0021
            10      S1-CMU54-SEGNAM PICTURE X(8)                        CI0021
                                      VALUE 'CACPLBL'.                  CI0021
            10      S1-CMU54-CCOM   PICTURE X VALUE '*'.                CI0021
            10       S-CMU54-CCOD   PICTURE X(5)                        CI0021
                                      VALUE '-----'.                    CI0021
            10      S1-CMU54-FLDNAM PICTURE X(9)                        CI0021
                                      VALUE '(CACPLBLK'.                CI0021
            10       S-CMU54-OPER  PICTURE XX VALUE ' ='.               CI0021
            10       S-CMU54-CEKLBL.                                    CI0021
            11       S-CMU54-CESQ2    PICTURE  9(2).                    CI0021
            10  FILLER   PICTURE X    VALUE ')'.                        CI0021
       01   ZONES-UTILISATEUR PICTURE X.                                CI0021
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
      ** PCB POINTER FOR CA1P                                           ADU015
            05 PCB-CA1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LM1P                                           ADU015
            05 PCB-LM1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CA1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0021
          05              PA00-SUITE.                                   CI0021
            15       FILLER         PICTURE  X(00106).                  CI0021
       01                 PA06  REDEFINES      PA00.                    CI0021
            10            PA06-XDBPCB.                                  CI0021
            11            PA06-XDBDNM PICTURE  X(08).                   CI0021
            11            PA06-XSEGLV PICTURE  X(02).                   CI0021
            11            PA06-XRC    PICTURE  X(02).                   CI0021
            11            PA06-XPROPT PICTURE  X(04).                   CI0021
            11            PA06-FILLER PICTURE  S9(5)                    CI0021
                          BINARY.                                       CI0021
            11            PA06-XSEGNM PICTURE  X(08).                   CI0021
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0021
                          BINARY.                                       CI0021
            11            PA06-XSEGNB PICTURE  9(05)                    CI0021
                          BINARY.                                       CI0021
            11            PA06-XCOKEY PICTURE  X(70).                   CI0021
      *** PCB MASK FOR LM1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0021
          05              PB00-SUITE.                                   CI0021
            15       FILLER         PICTURE  X(00106).                  CI0021
       01                 PB06  REDEFINES      PB00.                    CI0021
            10            PB06-XDBPCB.                                  CI0021
            11            PB06-XDBDNM PICTURE  X(08).                   CI0021
            11            PB06-XSEGLV PICTURE  X(02).                   CI0021
            11            PB06-XRC    PICTURE  X(02).                   CI0021
            11            PB06-XPROPT PICTURE  X(04).                   CI0021
            11            PB06-FILLER PICTURE  S9(5)                    CI0021
                          BINARY.                                       CI0021
            11            PB06-XSEGNM PICTURE  X(08).                   CI0021
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0021
                          BINARY.                                       CI0021
            11            PB06-XSEGNB PICTURE  9(05)                    CI0021
                          BINARY.                                       CI0021
            11            PB06-XCOKEY PICTURE  X(70).                   CI0021

      *PASS AREA TO/FROM CI0021
      *!WF DSP=PE DSL=DU SEL=22 FOR=I DES=1 LEV=1 PLT=05
       01                 PE22.                                         CI0021
            10            PE22-C299.                                    CI0021
            11            PE22-CTID.                                    CI0021
            12            PE22-CTIDA  PICTURE  9(3).                    CI0021
            12            PE22-CTIDN.                                   CI0021
            13            PE22-CTIDNP PICTURE  X(13).                   CI0021
            13            PE22-CTIDND PICTURE  9(11).                   CI0021
            10            PE22-PRCOD  PICTURE  9(5).                    CI0021
            10            PE22-CTSTA  PICTURE  99.                      CI0021
            10            PE22-MAPPN  PICTURE  X(10).                   CI0021
            10            PE22-FILLER PICTURE  X(088).                  CI0021
            10            PE22-IARRGA PICTURE  X.                       CI0021
            10            PE22-IARLNA PICTURE  X.                       CI0021
            10            PE22-CELBL  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            PE22-AMINAL PICTURE  S9(7)V99.                CI0021
            10            PE22-AMAXAL PICTURE  S9(7)V99.                CI0021
            10            PE22-AMIND  PICTURE  S9(7)V99.                CI0021
            10            PE22-AMAXAR PICTURE  S9(7)V99.                CI0021
            10            PE22-IARPSA PICTURE  X.                       CI0021
            10            PE22-CELBA  PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            PE22-INPAY  PICTURE  X(01).                   CI0021
            10            PE22-AMINAN PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            PE22-AMAXAN PICTURE  S9(7)V99                 CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            PE22-FILLER PICTURE  X(042).                  CI0021

      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0021
          05              DE00-SUITE.                                   CI0021
            15       FILLER         PICTURE  X(00653).                  CI0021
       01                 DE10  REDEFINES      DE00.                    CI0021
            10            DE10-DU11.                                    CI0021
            11            DE10-XFONC  PICTURE  X(4).                    CI0021
            11            DE10-MPSBN  PICTURE  X(8).                    CI0021
            11            DE10-XDBDNM PICTURE  X(08).                   CI0021
            11            DE10-XSEGNM PICTURE  X(08).                   CI0021
            11            DE10-XRC    PICTURE  X(02).                   CI0021
            11            DE10-MSEG   PICTURE  X(08).                   CI0021
            11            DE10-XCOKEY PICTURE  X(70).                   CI0021
            11            DE10-CUIBR  PICTURE  X(01).                   CI0021
            11            DE10-CUIBA  PICTURE  X(01).                   CI0021
            11            DE10-IPBIK  PICTURE  X(1).                    CI0021
            10            DE10-DU03.                                    CI0021
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            DE10-CMSSF  PICTURE  XX.                      CI0021
            11            DE10-DU09.                                    CI0021
            12            DE10-CMESA  PICTURE  S9(9)                    CI0021
                          BINARY.                                       CI0021
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0021
                          BINARY.                                       CI0021
            12            DE10-CMESB  PICTURE  S9(9)                    CI0021
                          BINARY.                                       CI0021
            12            DE10-CMSST  PICTURE  S9(9)                    CI0021
                          BINARY.                                       CI0021
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0021
                          BINARY.                                       CI0021
            12            DE10-QELLAA PICTURE  S9(9)                    CI0021
                          BINARY.                                       CI0021
            12            DE10-TMESS4 PICTURE  X(512).                  CI0021
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
       01                 MS00.                                         CI0021
          05              MS00-SUITE.                                   CI0021
            15       FILLER         PICTURE  X(00542).                  CI0021
       01                 MS03  REDEFINES      MS00.                    CI0021
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            10            MS03-CMSSF  PICTURE  XX.                      CI0021
            10            MS03-DU09.                                    CI0021
            11            MS03-CMESA  PICTURE  S9(9)                    CI0021
                          BINARY.                                       CI0021
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0021
                          BINARY.                                       CI0021
            11            MS03-CMESB  PICTURE  S9(9)                    CI0021
                          BINARY.                                       CI0021
            11            MS03-CMSST  PICTURE  S9(9)                    CI0021
                          BINARY.                                       CI0021
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0021
                          BINARY.                                       CI0021
            11            MS03-QELLAA PICTURE  S9(9)                    CI0021
                          BINARY.                                       CI0021
            11            MS03-TMESS4 PICTURE  X(512).                  CI0021
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0021
            10            MX11-QMSGS  PICTURE  9(03).                   CI0021
            10            MX11-PJ09                                     CI0021
                          OCCURS       025     TIMES.                   CI0021
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0021
                          COMPUTATIONAL-3.                              CI0021
            11            MX11-CMESB  PICTURE  S9(9)                    CI0021
                          BINARY.                                       CI0021
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PE22
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0021
      *               *                                   *             CI0021
      *               *INITIALISATIONS                    *             CI0021
      *               *                                   *             CI0021
      *               *************************************.            CI0021
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
      *N02CA.    NOTE *SET OUTPUT FIELDS TO INIT VALUES   *.
       F02CA.                                                           lv10
      *'ALLOWED' INDICATORS
      *REG (ADD-ON) PMT
      *LOAN REPMT
      *PARTIAL SURR REPMT
      *NEW PMT
           MOVE        'N' TO PE22-IARRGA
           PE22-IARLNA
      *NOTE: PE22-IARPSA NOT FILLED IN
      *AFTER THIS POINT, AND NEVER WAS!
                           PE22-IARPSA
                           PE22-INPAY
      *PARTIAL SURR BALANCE (CERTS ONLY
      *LOAN BALANCE
           MOVE        ZERO TO PE22-CELBA
           PE22-CELBL
      *MAX PMT AMT - REG, LOAN, NEW
           MOVE        ZERO TO PE22-AMAXAR
           PE22-AMAXAL
           PE22-AMAXAN
      *MIN PMT AMT - REG, LOAN, NEW
           MOVE        9999999.99 TO PE22-AMIND
           PE22-AMINAL
           PE22-AMINAN.
       F02CA-FN. EXIT.
      *N02XA.    NOTE *SET ADDRESSES FOR DATABASES        *.
       F02XA.                                                           lv10
      *SET ADDRESS FOR CA1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CA1P-PTR1.                                          ADU015
      *SET ADDRESS FOR LM1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-LM1P-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0021
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0021
      *               *                                   *             CI0021
      *               *FIN DE TRAITEMENT                  *             CI0021
      *               *                                   *             CI0021
      *               *************************************.            CI0021
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0021
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
       F30BA.    IF    PE22-CTIDA NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F30BA-FN.
      *---> Send BAD ACCT Message                                       ADU119
      *      and EXIT                                                   ADU119
           MOVE        012004 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30BA-FN. EXIT.
      *N30CA.    NOTE *IF BAD PRODUCT                     *.
       F30CA.    IF    PE22-PRCOD NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F30CA-FN.
      *---> Send BAD PRODUCT Message                                    ADU119
      *      and EXIT                                                   ADU119
           MOVE        012266 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CA-FN. EXIT.
      *N30DA.    NOTE *IF BAD STATUS                      *.
       F30DA.    IF    PE22-CTSTA NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F30DA-FN.
      *---> Send BAD STATUS Message                                     ADU119
      *      and EXIT                                                   ADU119
           MOVE        012720 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DA-FN. EXIT.
      *N30FA.    NOTE *IF UNKNOWN MAPPN; DEFAULT SD       *.
       F30FA.    IF    PE22-MAPPN NOT = 'SD'                            lv10
                 AND   PE22-MAPPN NOT = 'FDC'
                 AND   PE22-MAPPN NOT = 'UD'
                 NEXT SENTENCE ELSE GO TO     F30FA-FN.
           MOVE        'SD' TO PE22-MAPPN.
       F30FA-FN. EXIT.
       F30-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *READ PACBASE TABLES                *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40BA.    NOTE *RANDOM TABLE READ FOR TA98         *.            ADUTAB
       F40BA.                                                           lv10
           MOVE        PE22-CTIDA TO TA98-CTIDA
           MOVE        PE22-PRCOD TO TA98-PRCOD
           MOVE        01 TO TA98-CARTY
           MOVE        'R1' TO G-TA98-TABFO                             ADUTAB
           COMPUTE     G-TA98-LTH = 60 + G-TA98-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA98-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA98)                                ADUTAB
                       LENGTH (G-TA98-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA98-TABCR NOT = '00'                          DOT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012092 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40BA-FN. EXIT.
      *N40CA.    NOTE *RANDOM TABLE READ FOR TB96         *.            ADUTAB
       F40CA.                                                           lv10
           MOVE        PE22-CTIDA TO TB96-CTIDA
           MOVE        PE22-PRCOD TO TB96-PRCOD
           MOVE        'LON' TO TB96-CPMTCA
           MOVE        'R1' TO G-TB96-TABFO                             ADUTAB
           COMPUTE     G-TB96-LTH = 60 + G-TB96-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TB96-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TB96)                                ADUTAB
                       LENGTH (G-TB96-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TB96-TABCR NOT = '00'                          DOT
           MOVE        '1' TO TB96-IK
                 ELSE
           MOVE        ZEROS TO TB96-IK.
       F40CA-FN. EXIT.
      *N40DA.    NOTE *RANDOM TABLE READ FOR TC96         *.            ADUTAB
       F40DA.                                                           lv10
           MOVE        PE22-CTIDA TO TC96-CTIDA
           MOVE        PE22-PRCOD TO TC96-PRCOD
           MOVE        'ADD' TO TC96-CPMTCA
           MOVE        'R1' TO G-TC96-TABFO                             ADUTAB
           COMPUTE     G-TC96-LTH = 60 + G-TC96-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TC96-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TC96)                                ADUTAB
                       LENGTH (G-TC96-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TC96-TABCR NOT = '00'                          DOT
           MOVE        '1' TO TC96-IK
                 ELSE
           MOVE        ZEROS TO TC96-IK.
       F40DA-FN. EXIT.
      *N40EA.    NOTE *RANDOM TABLE READ FOR TD96         *.            ADUTAB
       F40EA.                                                           lv10
           MOVE        PE22-CTIDA TO TD96-CTIDA
           MOVE        PE22-PRCOD TO TD96-PRCOD
           MOVE        'NEW' TO TD96-CPMTCA
           MOVE        'R1' TO G-TD96-TABFO                             ADUTAB
           COMPUTE     G-TD96-LTH = 60 + G-TD96-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TD96-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TD96)                                ADUTAB
                       LENGTH (G-TD96-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TD96-TABCR NOT = '00'                          DOT
           MOVE        '1' TO TD96-IK
                 ELSE
           MOVE        ZEROS TO TD96-IK.
       F40EA-FN. EXIT.
      *N40FA.    NOTE *RANDOM TABLE READ FOR TF5B         *.            ADUTAB
       F40FA.                                                           lv10
           MOVE        PE22-CTIDA TO TF5B-CTIDA
           MOVE        PE22-PRCOD TO TF5B-PRCOD
           MOVE        SPACES TO TF5B-PRSCD
           MOVE        'R1' TO G-TF5B-TABFO                             ADUTAB
           COMPUTE     G-TF5B-LTH = 60 + G-TF5B-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TF5B-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TF5B)                                ADUTAB
                       LENGTH (G-TF5B-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TF5B-TABCR NOT = '00'                          DOT
           MOVE        '0' TO TF5B-CF
                 ELSE
           MOVE        '1' TO TF5B-CF.
       F40FA-FN. EXIT.
       F40-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *LOAN REPAYMENT VALIDATION          *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *********************************
      ** CHECK TO SEE IF THE ACCOUNT  *
      ** NUMBER PASSED CAN HAVE LOAN  *
      ** REPAYMENTS TO IT.            *
      *********************************
      *N50AF.    NOTE *MOVE LOAN REPMT IND FROM TA98      *.
       F50AF.                                                           lv10
           MOVE        TA98-IARLNA TO PE22-IARLNA.
       F50AF-FN. EXIT.
      *N50AZ.    NOTE *TA96 ENTRY FOR 'LON' FOUND         *.
       F50AZ.    IF    TB96-IK = '0'                                    lv10
                 NEXT SENTENCE ELSE GO TO     F50AZ-FN.
           MOVE        TB96-AMINA TO PE22-AMINAL
           MOVE        TB96-AMAXA TO PE22-AMAXAL.
      *N50BA.    NOTE *MIN IS 9'S & MAX IS 0:             *.
       F50BA.    IF    TB96-AMINA = 9999999.99                          lv15
                 AND   TB96-AMAXA = ZEROS
                 NEXT SENTENCE ELSE GO TO     F50BA-FN.
      *LOAN REPMT NOT ALLOWED
      *SET LOAN REPMT INDICATOR TO 'N'
      *& GO TO END OF LOAN LOGIC
           MOVE        'N' TO PE22-IARLNA
               GO TO     F50-FN.
       F50BA-FN. EXIT.
       F50AZ-900. GO TO F50BZ-FN.
       F50AZ-FN. EXIT.
      *N50BZ.    NOTE *TA96 ENTRY FOR 'LON' NOT FOUND     *.
       F50BZ.         EXIT.                                             lv10
      *N50CA.    NOTE *CERTS                              *.
       F50CA.    IF    PE22-CTIDA = 001                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50CA-FN.
      *SET LOAN REPMT INDICATOR TO 'N'
      *& GO TO END OF LOAN LOGIC
           MOVE        'N' TO PE22-IARLNA
               GO TO     F50-FN.
       F50CA-FN. EXIT.
      *N50CF.    NOTE *LIFE OR LIFE/NY                    *.
       F50CF.    IF    (PE22-CTIDA = 004 OR 005)                        lv15
                 AND   PE22-IARLNA = 'Y'
                 NEXT SENTENCE ELSE GO TO     F50CF-FN.
      *AND LOAN REPAYMENTS ALLOWED
      *SOME LIFE PRODUCTS HAVE
      *TA98-IARLNA = 'Y' INDICATING
      *LOAN REPMTS ARE ALLOWED, BUT
      *THEN HAVE NO ENTRY FOR 'LON' ON
      *TA96.
           MOVE        ZERO TO PE22-AMINAL
           MOVE        9999999.99 TO PE22-AMAXAL.
       F50CF-FN. EXIT.
       F50BZ-FN. EXIT.
      *N50EA.    NOTE *LOAN REPMT INDICATOR IS YES        *.
       F50EA.    IF    PE22-IARLNA = 'Y'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50EA-FN.
      *N50FA.    NOTE *CERTS                              *.
       F50FA.    IF    PE22-CTIDA = 001                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50FA-FN.
      *********************************
      ** READ THE CERTS ACCOUNT       *
      ** DATABASE TO SEE IF THERE IS  *
      ** A LOAN BALANCE.              *
      *********************************
      *N50GA.    NOTE *GU CM01 (CERT DB ROOT SEGM)        *.
       F50GA.                                                           lv20
           MOVE        PE22-CTIDND TO W-WORK-CTIDND
           MOVE        W-WORK-NPREF TO S-CMU01-CEPRE
           MOVE        W-WORK-NBASE TO S-CMU01-CEBAS
           PERFORM     F94C1 THRU F94C1-FN.
      *N50IA.    NOTE *CM01 NOT FOUND                     *.
       F50IA.    IF    IK = '1'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F50IA-FN.
      *SET LOAN REPMT INDICATOR TO 'N'
      *& GO TO END OF LOAN LOGIC
           MOVE        'N' TO PE22-IARLNA
               GO TO     F50-FN.
       F50IA-FN. EXIT.
       F50GA-FN. EXIT.
      *N50JA.    NOTE *GN CM24 (CERT DB LOAN SEGM)        *.
       F50JA.                                                           lv20
           PERFORM     F94C2 THRU F94C2-FN.
      *N50KA.    NOTE *CM24 NOT FOUND                     *.
       F50KA.    IF    IK = '1'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F50KA-FN.
      *SET LOAN REPMT INDICATOR TO 'N'
      *& GO TO END OF LOAN LOGIC
           MOVE        'N' TO PE22-IARLNA
               GO TO     F50-FN.
       F50KA-FN. EXIT.
      *N50LA.    NOTE *LOAN BALANCE IS ZERO               *.
       F50LA.    IF    CM24-CELBL = ZEROS                               lv25
                 NEXT SENTENCE ELSE GO TO     F50LA-FN.
      *SET LOAN REPMT INDICATOR TO 'N'
      *& GO TO END OF LOAN LOGIC
           MOVE        'N' TO PE22-IARLNA
               GO TO     F50-FN.
       F50LA-900. GO TO F50LL-FN.
       F50LA-FN. EXIT.
      *N50LL.    NOTE *ELSE... STORE THE LOAN BALANCE     *.
       F50LL.                                                           lv25
           MOVE        CM24-CELBL TO PE22-CELBL.
       F50LL-FN. EXIT.
       F50JA-FN. EXIT.
       F50FA-FN. EXIT.
      *N50MA.    NOTE *LIFE OR LIFE/NY                    *.
       F50MA.    IF    PE22-CTIDA = 004                                 lv15
                 OR    PE22-CTIDA = 005
                 NEXT SENTENCE ELSE GO TO     F50MA-FN.
      *********************************
      *READ APPROPRIATE DATABASE TO SEE
      *IF THERE IS A LOAN BALANCE
      *********************************
      *N50MC.    NOTE *IF NOT A VANTAGE ACCOUNT           *.
       F50MC.    IF    TF5B-CF = '0'                                    lv20
                 OR    TF5B-IVANT = 'N'
                 NEXT SENTENCE ELSE GO TO     F50MC-FN.
      *GU AA10 (LIFE/ANNUITY DB ROOT)
           MOVE        PE22-CTIDND TO S-AAU10-ALCIDN
           PERFORM     F94A1 THRU F94A1-FN.
      *N50MD.    NOTE *AA10 NOT FOUND                     *.
       F50MD.    IF    IK = '1'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F50MD-FN.
      *COULD BE PENDING; NO LOAN
           MOVE        'N' TO PE22-IARLNA.
       F50MD-900. GO TO F50MH-FN.
       F50MD-FN. EXIT.
      *N50MH.    NOTE *AA10 FOUND                         *.
       F50MH.                                                           lv25
      *- PRODUCTS ARE NOT CHECKED HERE
      *  BECAUSE IF A LOAN EXISTS
      *  WE WANT TO LIST IT AND IF NOT
      *  WE DON"T WANT TO ALLOW IT.
      *  THERE SHOULD NEVER BE A
      *  SITUATION WHERE A LOAN SEGMENT
      *  WAS FOUND AND CONTAINED A
      *  VALUE.... BUT THERE WAS NO
      *  LOAN.
      *GN AA20 (LOAN SEG)
           PERFORM     F94A2 THRU F94A2-FN.
      *N50MM.    NOTE *AA20 FOUND (I.E. LOAN WAS FOUND)   *.
       F50MM.    IF    IK = '0'                                         lv30
                 NEXT SENTENCE ELSE GO TO     F50MM-FN.
           MOVE        AA20-ALLNB TO PE22-CELBL.
      *N50MP.    NOTE *IF LOAN BALANCE IS ZERO            *.
       F50MP.    IF    AA20-ALLNB = ZERO                                lv35
                 AND   PE22-IARLNA = 'Y'
                 NEXT SENTENCE ELSE GO TO     F50MP-FN.
           MOVE        'N' TO PE22-IARLNA.
       F50MP-FN. EXIT.
       F50MM-900. GO TO F50MT-FN.
       F50MM-FN. EXIT.
      *N50MT.    NOTE *AA20 NOT FOUND                     *.
       F50MT.                                                           lv30
      *GN AA66 (TSA LOAN SEG)
           PERFORM     F94A3 THRU F94A3-FN.
      *N50MV.    NOTE *AA66 FOUND (I.E. LOAN WAS FOUND)   *.
       F50MV.    IF    IK = '0'                                         lv35
                 NEXT SENTENCE ELSE GO TO     F50MV-FN.
           MOVE        AA66-ALLNB TO PE22-CELBL.
      *N50MX.    NOTE *LOAN BALANCE ON TSA SEG IS ZERO    *.
       F50MX.    IF    AA66-ALLNB = ZERO                                lv40
                 AND   PE22-IARLNA = 'Y'
                 NEXT SENTENCE ELSE GO TO     F50MX-FN.
           MOVE        'N' TO PE22-IARLNA.
       F50MX-FN. EXIT.
       F50MV-900. GO TO F50NA-FN.
       F50MV-FN. EXIT.
      *N50NA.    NOTE *AA66 NOT FOUND                     *.
       F50NA.                                                           lv35
           MOVE        'N' TO PE22-IARLNA.
       F50NA-FN. EXIT.
       F50MT-FN. EXIT.
       F50MH-FN. EXIT.
       F50MC-900. GO TO F50OA-FN.
       F50MC-FN. EXIT.
      *N50OA.    NOTE *VANTAGE ACCOUNT                    *.
       F50OA.         EXIT.                                             lv20
      *N50OD.    NOTE *ACCESS THE VANTAGE DATABASE        *.
       F50OD.                                                           lv25
           PERFORM     F92FA THRU F92FA-FN.
       F50OD-FN. EXIT.
      *N50PA.    NOTE *IF LOAN EXISTS; GET LOAN BALANCE   *.
       F50PA.    IF    PF31-QTNOL > ZEROES                              lv25
                 NEXT SENTENCE ELSE GO TO     F50PA-FN.
           INITIALIZE  PF30
           INITIALIZE  PF31
      *
      *LOAD ACCOUNT ID
      *
           MOVE        PE22-CTIDND TO 7-PF30-CTIDND
           MOVE        7-PF30-GVAID TO PF30-GVAID
      *
      *SET TO GET MOST RECENT LOAN INFO
      *
           MOVE        'GL' TO PF30-XPROPT
           MOVE        'Y' TO PF30-IDBLO.
      *N50PC.    NOTE *IDS LIFE                           *.
       F50PC.    IF    PE22-CTIDA = 004                                 lv30
                 NEXT SENTENCE ELSE GO TO     F50PC-FN.
           MOVE        'LIF' TO PF30-CVACO.
       F50PC-900. GO TO F50PE-FN.
       F50PC-FN. EXIT.
      *N50PE.    NOTE *IDS LIFE OF NY                     *.
       F50PE.                                                           lv30
           MOVE        'LNY' TO PF30-CVACO.
       F50PE-FN. EXIT.
      *N50PG.    NOTE *CALL PAZ000                        *.
       F50PG.                                                           lv30
           PERFORM     F98AZ THRU F98AZ-FN.
       F50PG-FN. EXIT.
      *N50PJ.    NOTE *IF BAD LOAN READ; TURN LOAN OFF    *.
       F50PJ.    IF    PF31-CVSTC NOT = SPACES                          lv30
                 NEXT SENTENCE ELSE GO TO     F50PJ-FN.
           MOVE        'N' TO PE22-IARLNA
           MOVE        9999999.99 TO PE22-AMINAL
           MOVE        ZERO TO PE22-AMAXAL.
       F50PJ-900. GO TO F50PM-FN.
       F50PJ-FN. EXIT.
      *N50PM.    NOTE *ELSE... LOAN WAS FOUND             *.
       F50PM.         EXIT.                                             lv30
      *N50PT.    NOTE *IF LOAN WAS PAID IN FULL           *.
       F50PT.    IF    PF31-DTLPA > ZERO                                lv35
                 AND   PF31-DTLPA NOT = 99999999
                 NEXT SENTENCE ELSE GO TO     F50PT-FN.
           MOVE        'N' TO PE22-IARLNA
           MOVE        9999999.99 TO PE22-AMINAL
           MOVE        ZERO TO PE22-AMAXAL.
       F50PT-900. GO TO F50PV-FN.
       F50PT-FN. EXIT.
      *N50PV.    NOTE *ELSE... SET LOAN AMOUNT            *.
       F50PV.                                                           lv35
           MOVE        PF31-ALBUL TO PE22-CELBL.
       F50PV-FN. EXIT.
       F50PM-FN. EXIT.
       F50PA-FN. EXIT.
       F50OA-FN. EXIT.
       F50MA-FN. EXIT.
       F50EA-FN. EXIT.
      *N50ZA.    NOTE *IF PRODUCT CARRIES LOAN AS NEG     *.
       F50ZA.    IF    PE22-IARLNA = 'Y'                                lv10
                 AND   PE22-CELBL < ZERO
                 NEXT SENTENCE ELSE GO TO     F50ZA-FN.
      *  - CHANGE TO POSITIVE NUMBER
           COMPUTE     PE22-CELBL = PE22-CELBL * -1.
       F50ZA-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *REGULAR PAYMENT VALIDATION         *
      *               *                                   *
      *               *************************************.
       F55.                                                             lv05
      *********************************
      ** CHECK TO SEE IF THE ACCOUNT  *
      ** NUMBER PASSED CAN HAVE       *
      ** REGULAR PAYMENTS TO IT.      *
      *********************************
      *N55AF.    NOTE *MOVE REG PMT IND FROM TA98         *.
       F55AF.                                                           lv10
           MOVE        TA98-IARRGA TO PE22-IARRGA.
       F55AF-FN. EXIT.
      *N55AZ.    NOTE *TA96 ENTRY FOR 'ADD' FOUND         *.
       F55AZ.    IF    TC96-IK = '0'                                    lv10
                 NEXT SENTENCE ELSE GO TO     F55AZ-FN.
           MOVE        TC96-AMINA TO PE22-AMIND
           MOVE        TC96-AMAXA TO PE22-AMAXAR.
      *N55BA.    NOTE *MIN IS 9'S & MAX IS 0:             *.
       F55BA.    IF    TC96-AMINA = 9999999.99                          lv15
                 AND   TC96-AMAXA = ZEROS
                 NEXT SENTENCE ELSE GO TO     F55BA-FN.
      *ADD-ON PMT IS NOT ALLOWED
      *SET REG PMT INDICATOR TO 'N'
      *& GO TO END OF REG PMT LOGIC
           MOVE        'N' TO PE22-IARRGA
               GO TO     F55-FN.
       F55BA-FN. EXIT.
       F55AZ-900. GO TO F55BZ-FN.
       F55AZ-FN. EXIT.
      *N55BZ.    NOTE *TA96 ENTRY FOR 'ADD' NOT FOUND     *.
       F55BZ.         EXIT.                                             lv10
      *N55CA.    NOTE *CERTS                              *.
       F55CA.    IF    PE22-CTIDA = 001                                 lv15
                 NEXT SENTENCE ELSE GO TO     F55CA-FN.
      *SET REG PMT INDICATOR TO 'N'
      *& GO TO END OF REG PMT LOGIC
           MOVE        'N' TO PE22-IARRGA
               GO TO     F55-FN.
       F55CA-FN. EXIT.
      *N55CF.    NOTE *LIFE OR LIFE/NY                    *.
       F55CF.    IF    (PE22-CTIDA = 004 OR 005)                        lv15
                 AND   PE22-IARRGA = 'Y'
                 NEXT SENTENCE ELSE GO TO     F55CF-FN.
      *AND REGULAR PAYMENTS ALLOWED
      *SOME LIFE PRODUCTS HAVE
      *TA98-IARRGA = 'Y' INDICATING
      *REG PMTS ARE ALLOWED, BUT
      *THEN HAVE NO ENTRY FOR 'ADD' ON
      *TA96.
           MOVE        ZERO TO PE22-AMIND
           MOVE        9999999.99 TO PE22-AMAXAR.
       F55CF-FN. EXIT.
       F55BZ-FN. EXIT.
      *N55DF.    NOTE *FUNDS (NOT ON TA96 TABLE)          *.
       F55DF.    IF    PE22-CTIDA = 002                                 lv10
                 NEXT SENTENCE ELSE GO TO     F55DF-FN.
           MOVE        100.00 TO PE22-AMIND
           MOVE        9999999.99 TO PE22-AMAXAR.
       F55DF-FN. EXIT.
      *N55EA.    NOTE *REG PMT ALLOWED INDICATOR IS NO    *.
       F55EA.    IF    PE22-IARRGA = 'N'                                lv10
                 AND   PE22-CTIDA = 001
                 AND   TA98-IARPSA = 'Y'
                 NEXT SENTENCE ELSE GO TO     F55EA-FN.
      *CERT ACCOUNT
      *ACCEPTS PARTIAL SURRENDERS
      *********************************
      ** READ THE CERTS ACCOUNT       *
      ** DATABASE TO SEE IF THERE IS  *
      ** AN AVERAGE LOAN BALANCE      *
      *********************************
      *GU CM01
           MOVE        PE22-CTIDND TO W-WORK-CTIDND
           MOVE        W-WORK-NPREF TO S-CMU01-CEPRE
           MOVE        W-WORK-NBASE TO S-CMU01-CEBAS
           PERFORM     F94C1 THRU F94C1-FN.
      *N55JA.    NOTE *CM01 FOUND;                        *.
       F55JA.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F55JA-FN.
      *READ LAST CM54 SEGMENT (GN)
           MOVE        'L----' TO S-CM54-CCOD
           PERFORM     F94C3 THRU F94C3-FN.
      *N55LA.    NOTE *CM54 FOUND;                        *.
       F55LA.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F55LA-FN.
      *INITIALIZE INDICES
           MOVE        +1 TO ICM54R
           ICM54L.
      *N55MA.    NOTE *LOOP THRU OCCURRENCES IN CM54      *.
       F55MA.    IF    ICM54R NOT > ICM54M                              lv25
                 NEXT SENTENCE ELSE GO TO     F55MA-FN.
      *LOOKING FOR THE LAST ACTIVITY
      *N55NA.    NOTE *NON-ZERO LOAN DATE; SAVE INDEX     *.
       F55NA.    IF    CM54-CELBDT (ICM54R) > ZEROS                     lv30
                 NEXT SENTENCE ELSE GO TO     F55NA-FN.
           MOVE        ICM54R TO ICM54L.
       F55NA-FN. EXIT.
      *N55OA.    NOTE *ADD 1 TO INDEX                     *.
       F55OA.                                                           lv30
           ADD         +1 TO ICM54R.
       F55OA-FN. EXIT.
       F55MA-900. GO TO F55MA.
       F55MA-FN. EXIT.
      *N55PM.    NOTE *ACCT HAS LOAN BALANCE              *.
       F55PM.    IF    CM54-CELBA (ICM54L) NOT = 0                      lv25
                 NEXT SENTENCE ELSE GO TO     F55PM-FN.
      *STORE THE AMOUNT
           MOVE        CM54-CELBA (ICM54L) TO PE22-CELBA
      *SET REG PMT ALLOWED IND TO 'Y'
           MOVE        'Y' TO PE22-IARRGA.
       F55PM-FN. EXIT.
       F55LA-FN. EXIT.
       F55JA-FN. EXIT.
       F55EA-FN. EXIT.
       F55-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *NEW PAYMENT VALIDATION             *
      *               *                                   *
      *               *************************************.
       F60.                                                             lv05
      *********************************
      ** CHECK TO SEE IF THE ACCOUNT  *
      ** NUMBER PASSED CAN HAVE NEW   *
      ** PAYMENTS.                    *
      *********************************
      *N60CA.    NOTE *TA96 ENTRY FOR 'NEW' FOUND         *.
       F60CA.    IF    TD96-IK = '0'                                    lv10
                 NEXT SENTENCE ELSE GO TO     F60CA-FN.
           MOVE        TD96-AMINA TO PE22-AMINAN
           MOVE        TD96-AMAXA TO PE22-AMAXAN.
       F60CA-900. GO TO F60EA-FN.
       F60CA-FN. EXIT.
      *N60EA.    NOTE *TA96 ENTRY FOR 'NEW' NOT FOUND     *.
       F60EA.         EXIT.                                             lv10
      *N60EE.    NOTE *FUNDS (NOT ON TABLE)               *.
       F60EE.    IF    PE22-CTIDA = 002                                 lv15
                 NEXT SENTENCE ELSE GO TO     F60EE-FN.
      *SET 'NEW' MIN PMT TO 2000
           MOVE        2000.00 TO PE22-AMINAN
           MOVE        9999999.99 TO PE22-AMAXAN.
                 IF    PE22-PRCOD = 102 OR 106                          DOT
                       OR 108
      *(INFLATION PROTECTED
      *SECURITIES FUND) OR (RIVERSOURCE
      *FLOATING RATE FUND - FRF) OR
      *(RIVERSOURCE ABSOLUTE RETURN -
      *CURRENCY- ARC) OR (RIVERSOURCE
      *DISCIPLINED SMALL CAP VALUE
      *FUND - DSV )
           MOVE        5000.00 TO PE22-AMINAN.
                 IF    PE22-PRCOD = 107 OR 125                          DOT
                 OR    124 OR 126
      *(INFLATION PROTECTED
      *SECURITIES FUND)
      *OR CONTRARIAN EQUITY FUND
      *OR US EQUITY FUND
      *OR THREADNEEDLE GLOBAL EXTENDED
      *ALPHA FUND
           MOVE        10000.00 TO PE22-AMINAN.
       F60EE-FN. EXIT.
      *N60EL.    NOTE *LIFE PRODUCTS                      *.
       F60EL.    IF    PE22-CTIDA = 004 OR 005                          lv15
                 NEXT SENTENCE ELSE GO TO     F60EL-FN.
      *ALLOW 'NEW' PMTS WHEN PRODUCT
      *NOT ON TA96
           MOVE        ZERO TO PE22-AMINAN
           MOVE        9999999.99 TO PE22-AMAXAN.
       F60EL-FN. EXIT.
       F60EA-FN. EXIT.
      *N60GA.    NOTE *IF NEW PAYMENT NOT ALLOWED         *.
       F60GA.    IF    PE22-AMINAN = 9999999.99                         lv10
                 AND   PE22-AMAXAN = ZEROS
                 NEXT SENTENCE ELSE GO TO     F60GA-FN.
      *SET NEW PMT INDICATOR TO 'N'
           MOVE        'N' TO PE22-INPAY.
       F60GA-900. GO TO F60GG-FN.
       F60GA-FN. EXIT.
      *N60GG.    NOTE *ELSE... NEW PMTS ALLOWED           *.
       F60GG.         EXIT.                                             lv10
      *N60HA.    NOTE *IF STATUS ALLOWS NEW PAYMENT       *.
       F60HA.    IF    PE22-CTSTA = 01                                  lv15
                 OR    (PE22-CTSTA = 03
                 AND   PE22-CTIDA = 002)
                 NEXT SENTENCE ELSE GO TO     F60HA-FN.
      *********************************
      ** - ALL PENDING ACCOUNTS AND   *
      **   FUND ACCOUNTS THAT ARE     *
      **   INACTIVE ARE CONSIDERED    *
      **   NEW                        *
      *********************************
           MOVE        'Y' TO PE22-INPAY.
       F60HA-900. GO TO F60HM-FN.
       F60HA-FN. EXIT.
      *N60HM.    NOTE *ELSE... NEW PAYMENT NOT ALLOWED    *.
       F60HM.                                                           lv15
           MOVE        'N' TO PE22-INPAY.
       F60HM-FN. EXIT.
       F60GG-FN. EXIT.
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
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *LIFE SPECIAL EDITS                 *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92FA.    NOTE *ACCESS VANTAGE BROKER              *.            AAOLBB
       F92FA.    IF    TF5B-CF = '1'                                    lv10
                 AND   TF5B-IVANT = 'Y'                                 AAOLBB
                 NEXT SENTENCE ELSE GO TO     F92FA-FN.                 AAOLBB
      *********************************                                 AAOLBB
      **  ACCESS VANTAGE MODULE HERE  *                                 AAOLBB
      *********************************                                 AAOLBB
      *POPULATE PF30 FIELDS                                             AAOLBB
           INITIALIZE  PF30                                             AAOLBB
           INITIALIZE  PF31                                             AAOLBB
      *LOAD COMPANY                                                     AAOLBB
                 IF    PE22-CTIDA = 005                                 DOT
           MOVE        'LNY' TO PF30-CVACO                              AAOLBB
                 ELSE                                                   AAOLBB
           MOVE        'LIF' TO PF30-CVACO.                             AAOLBB
      *LOAD ACCOUNT ID                                                  DOT
           MOVE        PE22-CTIDND TO 7-PF30-CTIDND                     AAOLBB
           MOVE        7-PF30-GVAID TO PF30-GVAID                       AAOLBB
      *SET THE CONTRACT INDICATOR                                       AAOLBB
           MOVE        'Y' TO PF30-IDBCD                                AAOLBB
      *SET THE BILLING INDICATOR                                        AAOLBB
           MOVE        'Y' TO PF30-IDBBI                                AAOLBB
           MOVE        '0' TO IK                                        AAOLBB
      *CALL PAZ000                                                      AAOLBB
           PERFORM     F98AZ THRU F98AZ-FN.                             AAOLBB
                 IF    IK = '1'                                         DOT
      *VANTAGE MODULE NOT FOUND, ERROR                                  AAOLBB
      *
           MOVE        'N' TO PE22-IARLNA
           MOVE        9999999.99 TO PE22-AMINAL
           MOVE        ZERO TO PE22-AMAXAL
               GO TO     F92FA-FN.
      *                                                                 DOT
                 IF    PE22-IARLNA = 'Y'                                DOT
                 AND   PF31-QTNOL = ZEROES
      *IF LOAN IS POSSIBLE IN EZ TRANS
      *AND TOTAL NUMBER OF LOANS ZERO                                   AAOLBB
      *DO NOT USE ERU
      *
           MOVE        'N' TO PE22-IARLNA
           MOVE        9999999.99 TO PE22-AMINAL
           MOVE        ZERO TO PE22-AMAXAL.
      *.                                                                DOT
      *                                                                 DOT
       F92FA-FN. EXIT.
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
      *N94.      NOTE *************************************.
      *               *                                   *
      *               *DL/I FUNCTIONS                     *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94A1.    NOTE *CALL GU ON AA10                    *.            ADU026
       F94A1.                                                           lv10
           MOVE        'LM1P' TO DE10-XDBDNM                            ADU026
           MOVE        'AA10' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 AA10                                                    ADU026
           S-AAU10-SSA                                                  ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94A1-FN. EXIT.
      *N94A2.    NOTE *CALL GN ON AA20                    *.            ADU026
       F94A2.                                                           lv10
           MOVE        'LM1P' TO DE10-XDBDNM                            ADU026
           MOVE        'AA20' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PB06 AA20                                                    ADU026
           S-AAU10-SSA S-AA20-SSA                                       ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94A2-FN. EXIT.
      *N94A3.    NOTE *CALL GN ON AA66                    *.            ADU026
       F94A3.                                                           lv10
           MOVE        'LM1P' TO DE10-XDBDNM                            ADU026
           MOVE        'AA66' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PB06 AA66                                                    ADU026
           S-AAU10-SSA S-AA25-SSA S-AA66-SSA                            ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94A3-FN. EXIT.
      *N94C1.    NOTE *CALL GU ON CM01                    *.            ADU026
       F94C1.                                                           lv10
           MOVE        'CA1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CM01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CM01                                                    ADU026
           S-CMU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C1-FN. EXIT.
      *N94C2.    NOTE *CALL GN ON CM24                    *.            ADU026
       F94C2.                                                           lv10
           MOVE        'CA1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CM24' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CM24                                                    ADU026
           S-CMU01-SSA S-CM24-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C2-FN. EXIT.
      *N94C3.    NOTE *CALL GN ON CM54                    *.            ADU026
       F94C3.                                                           lv10
           MOVE        'CA1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CM54' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CM54                                                    ADU026
           S-CMU01-SSA S-CM54-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C3-FN. EXIT.
       F94-FN.   EXIT.
      *N98.      NOTE *************************************.            ADU102
      *               *                                   *             ADU102
      *               *---> Common Performed Routines     *             ADU102
      *               *                                   *             ADU102
      *               *************************************.            ADU102
       F98.           EXIT.                                             lv05
      *N98AZ.    NOTE *---> CALL VANTAGE ACCESS MODULE    *.
       F98AZ.                                                           lv10
      *---> CALL VANTAGE ACCESS MODULE                                  AAZ000
           CALL        VANTAGE-ACCESS-MODULE                            AAZ000
           USING PF30 PF31.                                             AAZ000
                 IF    PF31-CVSTC NOT = SPACE                           DOT
      *---> CHECK FOR ERROR                                             AAZ000
           MOVE        '1' TO IK.
       F98AZ-FN. EXIT.
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
