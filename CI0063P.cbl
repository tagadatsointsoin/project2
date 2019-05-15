       IDENTIFICATION DIVISION.                                         CI0063
       PROGRAM-ID.  CI0063P.                                            CI0063
      *AUTHOR.         BA PAYMENT TERM EDITS/RULES.                     CI0063
      *DATE-COMPILED.   09/08/14.                                       CI0063
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
       ENVIRONMENT DIVISION.                                            CI0063
       CONFIGURATION SECTION.                                           CI0063
       SOURCE-COMPUTER. IBM-370.                                        CI0063
       OBJECT-COMPUTER. IBM-370.                                        CI0063
       DATA DIVISION.                                                   CI0063
       WORKING-STORAGE SECTION.                                         CI0063
       01                 AA00.                                         CI0063
            02            AA10.                                         CI0063
            10            AA10-AE00.                                    CI0063
            11            AA10-ALCIDN PICTURE  9(11).                   CI0063
            10            AA10-AE01.                                    CI0063
            11            AA10-FILLER PICTURE  X(12).                   CI0063
            11            AA10-DLAUP  PICTURE  9(8).                    CI0063
            11            AA10-FILLER PICTURE  S9(07)                   CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-FILLER PICTURE  9(8).                    CI0063
            11            AA10-FILLER PICTURE  S9(07)                   CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-FILLER PICTURE  9(8).                    CI0063
            11            AA10-FILLER PICTURE  S9(07)                   CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-FILLER PICTURE  X(311).                  CI0063
            10            AA10-AE02                                     CI0063
                          REDEFINES            AA10-AE01.               CI0063
            11            AA10-FILLER PICTURE  9.                       CI0063
            11            AA10-ALCOMP PICTURE  99.                      CI0063
            11            AA10-CRTYP  PICTURE  9(4).                    CI0063
            11            AA10-FILLER PICTURE  9.                       CI0063
            11            AA10-CPOST  PICTURE  99.                      CI0063
            11            AA10-GEHCDI PICTURE  9(3).                    CI0063
            11            AA10-FILLER PICTURE  9(8).                    CI0063
            11            AA10-FILLER PICTURE  9(7).                    CI0063
            11            AA10-FILLER PICTURE  X.                       CI0063
            11            AA10-ALPLDT PICTURE  9(8).                    CI0063
            11            AA10-DENEX  PICTURE  9(8).                    CI0063
            11            AA10-CENXC1 PICTURE  9(3).                    CI0063
            11            AA10-FILLER PICTURE  99.                      CI0063
            11            AA10-CROOR  PICTURE  99.                      CI0063
            11            AA10-CREIN  PICTURE  99.                      CI0063
            11            AA10-FILLER PICTURE  X.                       CI0063
            11            AA10-ALAPST PICTURE  99.                      CI0063
            11            AA10-ALSTSA PICTURE  XX.                      CI0063
            11            AA10-FILLER PICTURE  99.                      CI0063
            11            AA10-CPCAL  PICTURE  9.                       CI0063
            11            AA10-CNAEX  PICTURE  9.                       CI0063
            11            AA10-CSUSI  PICTURE  99.                      CI0063
            11            AA10-FILLER PICTURE  99.                      CI0063
            11            AA10-FILLER PICTURE  99.                      CI0063
            11            AA10-FILLER PICTURE  9(8).                    CI0063
            11            AA10-FILLER PICTURE  9.                       CI0063
            11            AA10-FILLER PICTURE  X(10).                   CI0063
            11            AA10-FILLER PICTURE  99.                      CI0063
            11            AA10-FILLER PICTURE  X.                       CI0063
            11            AA10-FILLER PICTURE  99.                      CI0063
            11            AA10-CAPL   PICTURE  9.                       CI0063
            11            AA10-FILLER PICTURE  9.                       CI0063
            11            AA10-FILLER PICTURE  999.                     CI0063
            11            AA10-FILLER PICTURE  999.                     CI0063
            11            AA10-FILLER PICTURE  999.                     CI0063
            11            AA10-CSTWH  PICTURE  9(8).                    CI0063
            11            AA10-FILLER PICTURE  9(8).                    CI0063
            11            AA10-FILLER PICTURE  99.                      CI0063
            11            AA10-FILLER PICTURE  9.                       CI0063
            11            AA10-FILLER PICTURE  9.                       CI0063
            11            AA10-FILLER PICTURE  99.                      CI0063
            11            AA10-FILLER PICTURE  999.                     CI0063
            11            AA10-FILLER PICTURE  999.                     CI0063
            11            AA10-CPRPM  PICTURE  9(3).                    CI0063
            11            AA10-FILLER PICTURE  99.                      CI0063
            11            AA10-FILLER PICTURE  9(6).                    CI0063
            11            AA10-FILLER PICTURE  99.                      CI0063
            11            AA10-FILLER PICTURE  999.                     CI0063
            11            AA10-DTRCM  PICTURE  9(8).                    CI0063
            11            AA10-DLATR  PICTURE  9(8).                    CI0063
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-ALPMOD PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-CRSBN  PICTURE  99.                      CI0063
            11            AA10-FILLER PICTURE  X.                       CI0063
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-FILLER PICTURE  9.                       CI0063
            11            AA10-FILLER PICTURE  X.                       CI0063
            11            AA10-FILLER PICTURE  XX.                      CI0063
            11            AA10-FILLER PICTURE  99.                      CI0063
            11            AA10-CTRHO  PICTURE  9(8).                    CI0063
            11            AA10-FILLER PICTURE  9.                       CI0063
            11            AA10-FILLER PICTURE  99.                      CI0063
            11            AA10-FILLER PICTURE  99.                      CI0063
            11            AA10-CTSGD  PICTURE  9(8).                    CI0063
            11            AA10-IANRD  PICTURE  9.                       CI0063
            11            AA10-ALINNO PICTURE  99.                      CI0063
            11            AA10-ALSANN PICTURE  9(5).                    CI0063
            11            AA10-FILLER PICTURE  S9(9)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-ALPAGR PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-ALRISK PICTURE  S9(9)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-ALAPIT PICTURE  S9(07)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-FILLER PICTURE  X.                       CI0063
            11            AA10-CADPR  PICTURE  9.                       CI0063
            11            AA10-AAPRT  PICTURE  S9(07)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-NREPN1 PICTURE  9(06).                   CI0063
            11            AA10-CCST1  PICTURE  9.                       CI0063
            11            AA10-CESRD  PICTURE  9(3).                    CI0063
            11            AA10-ALLRT  PICTURE  S9V99                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-ANTPAA PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-ALDDUE PICTURE  9(08).                   CI0063
            11            AA10-ALMODE PICTURE  99.                      CI0063
            11            AA10-FILLER PICTURE  X(08).                   CI0063
            11            AA10-CTCUS1 PICTURE  99.                      CI0063
            11            AA10-CNPPR  PICTURE  9(03).                   CI0063
            11            AA10-FILLER PICTURE  9.                       CI0063
            11            AA10-FILLER PICTURE  9(03).                   CI0063
            11            AA10-ITMEC  PICTURE  X(1).                    CI0063
            11            AA10-IMCDI  PICTURE  X.                       CI0063
            11            AA10-LSIDTE PICTURE  9(08).                   CI0063
            11            AA10-PLINE  PICTURE  S9V99                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-ATSA8  PICTURE  S9(07)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-ATSA9  PICTURE  S9(05)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-CRATS  PICTURE  X.                       CI0063
            11            AA10-PPTKN  PICTURE  S9(3)V9(6)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            AA10-FILLER PICTURE  X(24).                   CI0063
            02            AA20.                                         CI0063
            10            AA20-CLTRN  PICTURE  999.                     CI0063
            10            AA20-DLACC  PICTURE  9(8).                    CI0063
            10            AA20-ALDUED PICTURE  9(8).                    CI0063
            10            AA20-DLTRN  PICTURE  9(8).                    CI0063
            10            AA20-ALLNB  PICTURE  S9(07)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA20-ALLPA  PICTURE  S9(07)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA20-FILLER PICTURE  9(06).                   CI0063
            10            AA20-FILLER PICTURE  S9(05)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA20-FILLER PICTURE  S9(05)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA20-FILLER PICTURE  9(06).                   CI0063
            10            AA20-FILLER PICTURE  S9(07)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA20-FILLER PICTURE  S9(07)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            02            AA25.                                         CI0063
            10            AA25-ANPTT  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-APCLO  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-APTXL  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-AVARP  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-AVARN  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-APCLD  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-AVART  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-AVLSC  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-AFLSC  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ALUNIT PICTURE  S9(8)V999                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-CSURV  PICTURE  99.                      CI0063
            10            AA25-CFGLC  PICTURE  9(6).                    CI0063
            10            AA25-CMINP  PICTURE  9.                       CI0063
            10            AA25-APPYP  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-APPY2  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-APPY3  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-DSIDF  PICTURE  9(8).                    CI0063
            10            AA25-ALYTDF PICTURE  S9(07)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-DSIDV  PICTURE  9(8).                    CI0063
            10            AA25-ALYTDV PICTURE  S9(07)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-DTRNF  PICTURE  9(8).                    CI0063
            10            AA25-AREF   PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-DTRLA  PICTURE  9(8).                    CI0063
            10            AA25-DTRLT  PICTURE  9(8).                    CI0063
            10            AA25-CTRNL  PICTURE  9(4).                    CI0063
            10            AA25-ATRNL  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ATRCV  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ATRCI  PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-QTRAU  PICTURE  S9(8)V999                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ATRCT  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ATCTI  PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-QTRTA  PICTURE  S9(8)V999                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-IOMIT  PICTURE  9.                       CI0063
            10            AA25-DPAPR  PICTURE  9(8).                    CI0063
            10            AA25-DCAPR  PICTURE  9(8).                    CI0063
            10            AA25-PVAP   PICTURE  S9(3)V9(4)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ALPALC PICTURE  S9(04)V999               CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-PAACF  PICTURE  S9(2)V999                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-NBUID  PICTURE  S999                     CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ABUCV  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ANFMC  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ANFMP  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-DLACD  PICTURE  9(8).                    CI0063
            10            AA25-DPACD  PICTURE  9(8).                    CI0063
            10            AA25-CBASP  PICTURE  9.                       CI0063
            10            AA25-FILLER PICTURE  9.                       CI0063
            10            AA25-APYTI  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-FILLER PICTURE  X(2).                    CI0063
            10            AA25-GNPCA.                                   CI0063
            11            AA25-CPRDE  PICTURE  99.                      CI0063
            11            AA25-CPFGH  PICTURE  999.                     CI0063
            11            AA25-CPRI   PICTURE  9.                       CI0063
            11            AA25-CPRJ   PICTURE  9.                       CI0063
            11            AA25-CPKL   PICTURE  99.                      CI0063
            10            AA25-CLPRC  PICTURE  9(7).                    CI0063
            10            AA25-AQPAP  PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-AQPTC  PICTURE  S9(7)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ATPRC  PICTURE  S9(7)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ATASP  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ANPIP  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-DNPCA  PICTURE  9(8).                    CI0063
            10            AA25-DLAPC  PICTURE  9(8).                    CI0063
            10            AA25-APRCR  PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-AMATP  PICTURE  S9(7)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-AOGAP  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-APYCV  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-AACV   PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-AAICV  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ACVCY  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ACVPY  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-FILLER PICTURE  S9(6)V999                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ATACV  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ATCVI  PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ACCYV  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-APCYV  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-CES79  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ARAPI  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-CARDC  PICTURE  99.                      CI0063
            10            AA25-PGAIR  PICTURE  SV999                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ASPAP  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ATFCV  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ATFCI  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-QTFAU  PICTURE  S9(6)V999                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ATFPI  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-CWITH  PICTURE  99.                      CI0063
            10            AA25-ATCBC  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ATCEP  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ATLTC  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ATRCIT PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-QTRAUT PICTURE  S9(8)V999                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-DINNO  PICTURE  9(8).                    CI0063
            10            AA25-CSIRAC PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-CSIRAP PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-AIRALC PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-AIPYT  PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-DLDIC  PICTURE  9(4).                    CI0063
            10            AA25-AROLC  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-AROLP  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA25-ISEPP  PICTURE  X.                       CI0063
            10            AA25-IDEMR  PICTURE  X(1).                    CI0063
            10            AA25-FILLER PICTURE  X(139).                  CI0063
            02            AA66.                                         CI0063
            10            AA66-CLTRN  PICTURE  999.                     CI0063
            10            AA66-DLACC  PICTURE  9(8).                    CI0063
            10            AA66-DLPAD  PICTURE  9(08).                   CI0063
            10            AA66-DLTRNL PICTURE  9(8).                    CI0063
            10            AA66-DLTRN  PICTURE  9(8).                    CI0063
            10            AA66-ALDUED PICTURE  9(8).                    CI0063
            10            AA66-ALLNB  PICTURE  S9(07)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA66-ALLPA  PICTURE  S9(07)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA66-ALLIP  PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA66-ALADJL PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA66-QLLNT  PICTURE  99.                      CI0063
            10            AA66-ILNST  PICTURE  X.                       CI0063
            10            AA66-PVPAP  PICTURE  S9(04)V999               CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA66-DCAPR  PICTURE  9(8).                    CI0063
            10            AA66-PVPAPP PICTURE  S9(4)V9(3)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA66-DPAPR  PICTURE  9(8).                    CI0063
            10            AA66-ALADJF PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA66-DTRNFD PICTURE  9(8).                    CI0063
            10            AA66-ANIFN  PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA66-AIREF  PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA66-FILLER PICTURE  X(8).                    CI0063
            02            AA85.                                         CI0063
            10            AA85-DUWAC  PICTURE  9(8).                    CI0063
            10            AA85-CUWAC  PICTURE  99.                      CI0063
            10            AA85-CRFAC  PICTURE  9(6).                    CI0063
            10            AA85-IIAAF  PICTURE  9.                       CI0063
            10            AA85-DPOLI  PICTURE  9(8).                    CI0063
            10            AA85-DCONM  PICTURE  9(8).                    CI0063
            10            AA85-DBYR   PICTURE  99.                      CI0063
            10            AA85-DLMED  PICTURE  9(8).                    CI0063
            10            AA85-LSIDTE PICTURE  9(08).                   CI0063
            10            AA85-GESTD  PICTURE  9(8).                    CI0063
            10            AA85-CRBTR  PICTURE  9(6).                    CI0063
            10            AA85-CBTR   PICTURE  X.                       CI0063
            10            AA85-PSBTR  PICTURE  S9(3)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA85-ANTPF  PICTURE  S9(3)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA85-QDYT1  PICTURE  99.                      CI0063
            10            AA85-AEPT1  PICTURE  S9(3)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA85-QDYT2  PICTURE  99.                      CI0063
            10            AA85-AEPT2  PICTURE  S9(3)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA85-CRFER  PICTURE  9(6).                    CI0063
            10            AA85-PRADB  PICTURE  S99V9                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA85-PRWP   PICTURE  S99V9                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA85-PRNOM  PICTURE  S99V9                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA85-CDETH  PICTURE  X.                       CI0063
            10            AA85-CRFSP  PICTURE  X(6).                    CI0063
            10            AA85-CREDI  PICTURE  X(16).                   CI0063
            10            AA85-CRBD   PICTURE  X(6).                    CI0063
            10            AA85-CFRTR  PICTURE  X.                       CI0063
            10            AA85-ANTPG  PICTURE  S9(3)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA85-ATEPF  PICTURE  S9(3)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA85-CRFRT  PICTURE  X(6).                    CI0063
            10            AA85-QDYT3  PICTURE  99.                      CI0063
            10            AA85-CRFEX  PICTURE  9(6).                    CI0063
            10            AA85-AAPEB  PICTURE  S9(3)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA85-AAPER  PICTURE  S9(3)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA85-ARGAP  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            AA85-CWAE   PICTURE  XX.                      CI0063
            10            AA85-IMIBP  PICTURE  X.                       CI0063
            10            AA85-CNIR   PICTURE  XX.                      CI0063
            10            AA85-CMINM  PICTURE  XX.                      CI0063

      *PASS AREA TO/FROM CI0018 (GET CLIENTS FOR ACCT)
      *!WF DSP=AC DSL=DU SEL=14 FOR=I LEV=1 PLT=AC
       01                 AC00.                                         CI0063
          05              AC00-SUITE.                                   CI0063
            15       FILLER         PICTURE  X(00917).                  CI0063
       01                 AC14  REDEFINES      AC00.                    CI0063
            10            AC14-C299.                                    CI0063
            11            AC14-CTID.                                    CI0063
            12            AC14-CTIDA  PICTURE  9(3).                    CI0063
            12            AC14-CTIDN.                                   CI0063
            13            AC14-CTIDNP PICTURE  X(13).                   CI0063
            13            AC14-CTIDND PICTURE  9(11).                   CI0063
            10            AC14-DCACG  PICTURE  9(8).                    CI0063
            10            AC14-IPOCH  PICTURE  X.                       CI0063
            10            AC14-FILLER PICTURE  X(100).                  CI0063
            10            AC14-CLID01.                                  CI0063
            11            AC14-CLIDO1 PICTURE  X(3).                    CI0063
            11            AC14-NCLID1.                                  CI0063
            12            AC14-CLIDP1 PICTURE  X(12).                   CI0063
            12            AC14-CLIDNA PICTURE  9(8).                    CI0063
            10            AC14-CLCTR  PICTURE  9(3).                    CI0063
            10            AC14-DU21                                     CI0063
                          OCCURS       025     TIMES.                   CI0063
            11            AC14-C199.                                    CI0063
            12            AC14-CLID.                                    CI0063
            13            AC14-CLIDO  PICTURE  9(3).                    CI0063
            13            AC14-CLIDN.                                   CI0063
            14            AC14-CLIDNP PICTURE  X(12).                   CI0063
            14            AC14-CLIDND PICTURE  9(8).                    CI0063
            11            AC14-CLCTRC PICTURE  9(3).                    CI0063
            10            AC14-QITEM  PICTURE  9(3).                    CI0063
            10            AC14-XIMAX  PICTURE  S9(4)                    CI0063
                          BINARY.                                       CI0063
            10            AC14-CRROL  PICTURE  X.                       CI0063
            10            AC14-FILLER PICTURE  X(099).                  CI0063

      *!WF DSP=PE DSL=PE SEL=3031 FOR=I DES=1 LEV=1                     AAZ000
       01                 PE30.                                         CI0063
            10            PE30-XPROPT PICTURE  X(04).                   CI0063
            10            PE30-GVSGK  PICTURE  X(87).                   CI0063
            10            PE30-CVACO  PICTURE  X(3).                    CI0063
            10            PE30-GVAID  PICTURE  X(15).                   CI0063
            10            PE30-CTHCO  PICTURE  X(4).                    CI0063
            10            PE30-IDBCD  PICTURE  X(1).                    CI0063
            10            PE30-IDBMF  PICTURE  X(1).                    CI0063
            10            PE30-IDBNO  PICTURE  X(1).                    CI0063
            10            PE30-IDBNI  PICTURE  X(1).                    CI0063
            10            PE30-IDBTH  PICTURE  X(1).                    CI0063
            10            PE30-IDBSW  PICTURE  X(1).                    CI0063
            10            PE30-CPOLV  PICTURE  X.                       CI0063
            10            PE30-DEFFT  PICTURE  9(8).                    CI0063
            10            PE30-IIRSO  PICTURE  X(1).                    CI0063
            10            PE30-IMVAO  PICTURE  X(1).                    CI0063
            10            PE30-CMEMO  PICTURE  X(2).                    CI0063
            10            PE30-IVOGN  PICTURE  X.                       CI0063
            10            PE30-APRGT  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            PE30-CDSTV  PICTURE  X.                       CI0063
            10            PE30-IDBUV  PICTURE  X(1).                    CI0063
            10            PE30-IDBAI  PICTURE  X(1).                    CI0063
            10            PE30-IDBRI  PICTURE  X(1).                    CI0063
            10            PE30-IDBCO  PICTURE  X(1).                    CI0063
            10            PE30-IDBCA  PICTURE  X(1).                    CI0063
            10            PE30-IDBBI  PICTURE  X(1).                    CI0063
            10            PE30-IDBLO  PICTURE  X(1).                    CI0063
            10            PE30-IDBFD  PICTURE  X(1).                    CI0063
            10            PE30-IDBPY  PICTURE  X(1).                    CI0063
            10            PE30-IDBRP  PICTURE  X(1).                    CI0063
            10            PE30-IDBRE  PICTURE  X(1).                    CI0063
            10            PE30-CFNDC  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            PE30-CCLTP1 PICTURE  X(2).                    CI0063
            10            PE30-CDSTV3 PICTURE  X(3).                    CI0063
            10            PE30-IVIVS  PICTURE  X.                       CI0063
            10            PE30-IDBSR  PICTURE  X(2).                    CI0063
            10            PE30-CPNOP  PICTURE  X(2).                    CI0063
            10            PE30-IDDTH  PICTURE  X(1).                    CI0063
            10            PE30-CLDOD  PICTURE  9(8).                    CI0063
            10            PE30-IDBIF  PICTURE  X(1).                    CI0063
            10            PE30-IDBMA  PICTURE  X(1).                    CI0063
            10            PE30-CCOUL  PICTURE  XX.                      CI0063
            10            PE30-FILLER PICTURE  X(298).                  CI0063
       01                 PE31.                                         CI0063
            10            PE31-CVSTC  PICTURE  X(4).                    CI0063
            10            PE31-GVSGK  PICTURE  X(87).                   CI0063
            10            PE31-QFNDO  PICTURE  S9(4)                    CI0063
                          BINARY.                                       CI0063
            10            PE31-PE32.                                    CI0063
            11            PE31-PE65.                                    CI0063
            12            PE31-CVAPC  PICTURE  X(6).                    CI0063
            12            PE31-CVALB  PICTURE  X(3).                    CI0063
            12            PE31-CASTA  PICTURE  X.                       CI0063
            12            PE31-CTWST1 PICTURE  X(3).                    CI0063
            12            PE31-CPISC  PICTURE  X(3).                    CI0063
            12            PE31-ALPLDT PICTURE  9(8).                    CI0063
            12            PE31-DEFFT  PICTURE  9(8).                    CI0063
            12            PE31-DANNI  PICTURE  9(8).                    CI0063
            12            PE31-DTPMT  PICTURE  9(8).                    CI0063
            12            PE31-ITAMR  PICTURE  X(1).                    CI0063
            12            PE31-AGAPA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ATRPA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ATROP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ITMECC PICTURE  X(1).                    CI0063
            12            PE31-CBPLCA PICTURE  X.                       CI0063
            12            PE31-CPRCC  PICTURE  X.                       CI0063
            12            PE31-CLSEX  PICTURE  X.                       CI0063
            12            PE31-QPOIA  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-IREIA  PICTURE  X.                       CI0063
            12            PE31-APCUA  PICTURE  S9(6)V9(5)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AGLPA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AGSPA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ATGPA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ACBIN1 PICTURE  S9(09)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CABRC  PICTURE  X.                       CI0063
            12            PE31-CDSON  PICTURE  X(06).                   CI0063
            12            PE31-CADVN  PICTURE  X(10).                   CI0063
            12            PE31-CVOMC1 PICTURE  X(1).                    CI0063
            12            PE31-CSSUP2 PICTURE  X.                       CI0063
            12            PE31-ACECP  PICTURE  S9(09)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CCOFE  PICTURE  X.                       CI0063
            12            PE31-GRIDN7 PICTURE  S9(7)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-QNZDS  PICTURE  S9(04)                   CI0063
                          COMPUTATIONAL   SYNC RIGHT.                   CI0063
            12            PE31-QTNOL  PICTURE  S9(05)                   CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-QTNPW  PICTURE  S9(05)                   CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-IREIN  PICTURE  X.                       CI0063
            12            PE31-DNRIP  PICTURE  9(8).                    CI0063
            12            PE31-ATDPA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CREPC  PICTURE  XX.                      CI0063
            12            PE31-CMLTP  PICTURE  X.                       CI0063
            12            PE31-IREPL6 PICTURE  X.                       CI0063
            12            PE31-CCLAC  PICTURE  X.                       CI0063
            12            PE31-ALNTI  PICTURE  S9(09)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ATIPA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CVASC  PICTURE  XX.                      CI0063
            12            PE31-DRTHC  PICTURE  9(8).                    CI0063
            12            PE31-CENDO  PICTURE  X.                       CI0063
            12            PE31-ICOEX  PICTURE  X(1).                    CI0063
            12            PE31-INURS  PICTURE  X(1).                    CI0063
            12            PE31-ITRML  PICTURE  X(1).                    CI0063
            12            PE31-IBIRA  PICTURE  X(1).                    CI0063
            12            PE31-CCOUL  PICTURE  XX.                      CI0063
            12            PE31-ASGLP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-DMATUR PICTURE  9(8).                    CI0063
            12            PE31-CJOIP  PICTURE  X.                       CI0063
            12            PE31-CPNOP  PICTURE  X(2).                    CI0063
            12            PE31-ARBRP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ACCHV  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ASCHV  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CGMBR  PICTURE  X.                       CI0063
            12            PE31-AMCTV  PICTURE  S9(7)V99.                CI0063
            12            PE31-ACBIN2 PICTURE  S9(09)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PE1B                                     CI0063
                          REDEFINES            PE31-ACBIN2.             CI0063
            13            PE31-NAAMC  PICTURE  9(2).                    CI0063
            13            PE31-PNPCT  PICTURE  999.                     CI0063
            13            PE31-FILLER PICTURE  X(1).                    CI0063
            11            PE31-PE75                                     CI0063
                          REDEFINES            PE31-PE65.               CI0063
            12            PE31-CASTAR PICTURE  X.                       CI0063
            12            PE31-DLPLDA PICTURE  9(8).                    CI0063
            12            PE31-ATRPAR PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-DSTT1  PICTURE  9(8).                    CI0063
            12            PE31-CTLPD  PICTURE  9(8).                    CI0063
            12            PE31-DNPMT  PICTURE  9(8).                    CI0063
            12            PE31-ALFXP  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CFRQZ  PICTURE  X.                       CI0063
            12            PE31-ASTXW1 PICTURE  S9(13)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ATWHDD PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-APOCY  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-APOPY  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-APOTD  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-TPLNL  PICTURE  X(30).                   CI0063
            12            PE31-NPSTA  PICTURE  X(11).                   CI0063
            12            PE31-CPSTP  PICTURE  X(6).                    CI0063
            12            PE31-DPSTI  PICTURE  9(8).                    CI0063
            12            PE31-IFXVR  PICTURE  X.                       CI0063
            12            PE31-IDTHI  PICTURE  X.                       CI0063
            12            PE31-PFPAY  PICTURE  S9(3)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PVPAY  PICTURE  S9(3)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CPURF  PICTURE  X(5).                    CI0063
            12            PE31-ATAXY  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CVALB1 PICTURE  X(4).                    CI0063
            12            PE31-CRATC1 PICTURE  X.                       CI0063
            12            PE31-PSTIN  PICTURE  9(3)V99                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PFTXW  PICTURE  9(3)V99                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PSTXW  PICTURE  9(3)V99                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PFERT  PICTURE  9(3)V99                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AFEDV  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PSERT  PICTURE  9(3)V99                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ASEDV  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-DPRPA  PICTURE  9(8).                    CI0063
            12            PE31-ALVXP  PICTURE  S9(7)V9(4)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ISTWH1 PICTURE  X(1).                    CI0063
            12            PE31-FILLER PICTURE  X(44).                   CI0063
            11            PE31-PE67.                                    CI0063
            12            PE31-DMDOB1 PICTURE  X(8).                    CI0063
            12            PE31-CTNAM  PICTURE  X(01).                   CI0063
            12            PE31-MCOMP                                    CI0063
                          OCCURS       002     TIMES.                   CI0063
            13            PE31-MINDL  PICTURE  X(35).                   CI0063
            13            PE31-MINDF  PICTURE  X(25).                   CI0063
            13            PE31-MINDP  PICTURE  X(10).                   CI0063
            13            PE31-MINDS  PICTURE  X(10).                   CI0063
            12            PE31-MADR1V PICTURE  X(35)                    CI0063
                          OCCURS       004     TIMES.                   CI0063
            12            PE31-MCIT   PICTURE  X(30).                   CI0063
            12            PE31-CPISC  PICTURE  X(3).                    CI0063
            12            PE31-CZIPC  PICTURE  X(15).                   CI0063
            12            PE31-CLSEX1 PICTURE  X.                       CI0063
            11            PE31-PE88.                                    CI0063
            12            PE31-QNSUR  PICTURE  S9(3)                    CI0063
                          BINARY.                                       CI0063
            12            PE31-QTSUR  PICTURE  S9(3)                    CI0063
                          BINARY.                                       CI0063
            12            PE31-ACGPA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ATOTA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PE8C                                     CI0063
                          OCCURS       010     TIMES.                   CI0063
            13            PE31-ALHDTE PICTURE  9(08).                   CI0063
            13            PE31-ATWHDE PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ASTXW  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ACHKJ  PICTURE  S9(09)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-CVTXN1 PICTURE  X(4).                    CI0063
            13            PE31-ACACTV PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ASPAM1 PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-CSSVL  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-PE86.                                    CI0063
            12            PE31-CVOOD  PICTURE  S9(5)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-DEIRNB PICTURE  9(8).                    CI0063
            12            PE31-QCRPD  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CBRIT  PICTURE  SV9(5)                   CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-PE91.                                    CI0063
            12            PE31-CVOOD1 PICTURE  S9(5)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-DTRMSX PICTURE  X(8).                    CI0063
            12            PE31-DTRMEX PICTURE  X(8).                    CI0063
            12            PE31-ALPRUN PICTURE  S999V9(6)                CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PSPSX                                    CI0063
                          REDEFINES            PE31-ALPRUN              CI0063
               PICTURE    S9(7)V99                                      CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PIXPR  PICTURE  S9V9(4)                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-PE6B.                                    CI0063
            12            PE31-CEBMO  PICTURE  9(2).                    CI0063
            12            PE31-CBNBC1 PICTURE  X.                       CI0063
            12            PE31-AMDAR  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ALDDUE PICTURE  9(08).                   CI0063
            12            PE31-ASPAM2 PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-DRESE  PICTURE  9(8).                    CI0063
            12            PE31-DACUP  PICTURE  9(02).                   CI0063
            12            PE31-ACVAMG PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ISTWH  PICTURE  X(1).                    CI0063
            12            PE31-AVAIP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-FILLER PICTURE  X(63).                   CI0063
            11            PE31-PE6C.                                    CI0063
            12            PE31-ARGPA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AROGP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ACYTA  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-FILLER PICTURE  X(050).                  CI0063
            11            PE31-PE6R.                                    CI0063
            12            PE31-CRTYC  PICTURE  X(1).                    CI0063
            12            PE31-CRICO  PICTURE  X(3).                    CI0063
            11            PE31-PE3V.                                    CI0063
            12            PE31-PE90.                                    CI0063
            13            PE31-CSTCVE PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ASURR1 PICTURE  S9(09)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ALSURR PICTURE  S9(09)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ASINTC PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-AMVA1  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ASPAM  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ACVAM  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-AMSBT  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ACVALC PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ATWS   PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ISELO  PICTURE  X.                       CI0063
            13            PE31-FILLER PICTURE  X(1).                    CI0063
            13            PE31-FILLER PICTURE  X(2).                    CI0063
            13            PE31-ASTCV1 PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ATFCVC PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-FILLER PICTURE  X(1).                    CI0063
            13            PE31-FILLER PICTURE  X(2).                    CI0063
            13            PE31-AUINTA PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-FILLER PICTURE  X(1).                    CI0063
            13            PE31-IRCHG  PICTURE  X.                       CI0063
            13            PE31-ASTXW8 PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ATFRA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-APLIV  PICTURE  S9(09)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-PCIRB1 PICTURE  S9(2)V9(3)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ACVAMF PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-CSNCVE PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-PCIRB7 PICTURE  99V999.                  CI0063
            13            PE31-AMNSR  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-AMINL  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ASCHV  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-PRCHG  PICTURE  999V999                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ISTUR  PICTURE  X.                       CI0063
            13            PE31-CSTIM  PICTURE  X.                       CI0063
            13            PE31-AMCAV1 PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ARCHG  PICTURE  9(7)V99                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PE7C                                     CI0063
                          REDEFINES            PE31-PE90.               CI0063
            13            PE31-ACOGR  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ACONE  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ASURR3 PICTURE  S9(07)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-PSURR1 PICTURE  S9(3)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ACOTX  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-TCOMG  PICTURE  X(30).                   CI0063
            13            PE31-CVCST  PICTURE  X(4).                    CI0063
            13            PE31-FILLER PICTURE  X(100).                  CI0063
            12            PE31-ALCCV  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ATLPD  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ARTLP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-APRLP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ALPAY  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-DLSST  PICTURE  9(8).                    CI0063
            12            PE31-ACAUN  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-PE6W.                                    CI0063
            12            PE31-PCIRA  PICTURE  S99V999                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PCIRB  PICTURE  S99V999                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PE6G                                     CI0063
                          OCCURS       099     TIMES.                   CI0063
            13            PE31-NFUNDV PICTURE  S999                     CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-CFUNT  PICTURE  X.                       CI0063
            13            PE31-AFUNA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-QFUNT                                    CI0063
                          REDEFINES            PE31-AFUNA               CI0063
               PICTURE    S9(7)V9(4)                                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-AFUUV  PICTURE  S9(3)V9(6)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-AFUNV  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-AFUEI  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-PE1L.                                    CI0063
            12            PE31-PE6L.                                    CI0063
            13            PE31-DLTRN  PICTURE  9(8).                    CI0063
            13            PE31-IVOGN  PICTURE  X.                       CI0063
            13            PE31-DTLPA  PICTURE  9(8).                    CI0063
            13            PE31-ARTLP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ALBUL  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-CLOFC  PICTURE  S9(5)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ILNST  PICTURE  X.                       CI0063
            12            PE31-AIELN  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-FILLER PICTURE  X(23).                   CI0063
            11            PE31-PE64.                                    CI0063
            12            PE31-AXUNVP PICTURE  S9(3)V9(6)               CI0063
                          OCCURS       998     TIMES                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-PE6A.                                    CI0063
            12            PE31-FILLER PICTURE  X(310).                  CI0063
            12            PE31-PE63                                     CI0063
                          OCCURS       099     TIMES.                   CI0063
            13            PE31-NVAFN  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-CFUNT  PICTURE  X.                       CI0063
            13            PE31-PINFA6 PICTURE  S9(4)V9(1)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-FILLER PICTURE  X(18).                   CI0063
            13            PE31-ACTFA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-FILLER PICTURE  X(62).                   CI0063
            11            PE31-PE3D.                                    CI0063
            12            PE31-PE89.                                    CI0063
            13            PE31-CVOFC  PICTURE  S9(5)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-DEFFT  PICTURE  9(8).                    CI0063
            13            PE31-CVTXN  PICTURE  X(4).                    CI0063
            13            PE31-APRGT  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-CAORT  PICTURE  X(01).                   CI0063
            13            PE31-CVAST1 PICTURE  X.                       CI0063
            13            PE31-CVAST  PICTURE  X.                       CI0063
            13            PE31-CAMCN  PICTURE  X(8).                    CI0063
            13            PE31-CIRAR1 PICTURE  X.                       CI0063
            13            PE31-INQEX  PICTURE  X.                       CI0063
            13            PE31-CVAPC1 PICTURE  X(6).                    CI0063
            13            PE31-CMEMO  PICTURE  X(2).                    CI0063
            13            PE31-DEFFC  PICTURE  9(8).                    CI0063
            11            PE31-PE38.                                    CI0063
            12            PE31-ACPPA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-FILLER PICTURE  X(36).                   CI0063
            11            PE31-PE6P.                                    CI0063
            12            PE31-PCIRBA PICTURE  S99V999.                 CI0063
            12            PE31-PCIRBB PICTURE  S99V999.                 CI0063
            12            PE31-DCLWP  PICTURE  X(8).                    CI0063
            12            PE31-DGLWP  PICTURE  X(8).                    CI0063
            12            PE31-DCLNP  PICTURE  X(8).                    CI0063
            12            PE31-DGLNP  PICTURE  X(8).                    CI0063
            12            PE31-FILLER PICTURE  X(11).                   CI0063
            12            PE31-ACCHV  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ASCHV  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-DN1WP  PICTURE  X(8).                    CI0063
            12            PE31-DN1NP  PICTURE  X(8).                    CI0063
            12            PE31-DN2WP  PICTURE  X(8).                    CI0063
            12            PE31-DN2NP  PICTURE  X(8).                    CI0063
            12            PE31-CNLG1  PICTURE  X.                       CI0063
            12            PE31-CNLG2  PICTURE  X.                       CI0063
            12            PE31-CDMES  PICTURE  X(2)                     CI0063
                          OCCURS       015     TIMES.                   CI0063
            12            PE31-FILLER PICTURE  X(17).                   CI0063
            12            PE31-GDATA                                    CI0063
                          OCCURS       130     TIMES.                   CI0063
            13            PE31-ATPRP2 PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ACVWP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-AGVWP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ACVNP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-AGVNP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ACSWP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-AGSWP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-ACSNP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-AGSNP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-PE6X                                     CI0063
                          REDEFINES            PE31-PE6P.               CI0063
            12            PE31-CLFOI  PICTURE  X.                       CI0063
            12            PE31-AMECP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AGRPD  PICTURE  S9(9)V99                 CI0063
                          OCCURS       007     TIMES                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-DLMCH  PICTURE  9(8).                    CI0063
            12            PE31-APVTC  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ASMIP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ASMIPL PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AWDRTT PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CLFOI1 PICTURE  X.                       CI0063
            12            PE31-CLFOI2 PICTURE  X.                       CI0063
            12            PE31-APYMT1 PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-APYMT2 PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ASMIP1 PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ASMIP2 PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-IEXIC  PICTURE  X(01).                   CI0063
            12            PE31-DTODAF PICTURE  9(8).                    CI0063
            12            PE31-CLFOI3 PICTURE  X.                       CI0063
            12            PE31-CLFOI4 PICTURE  X.                       CI0063
            12            PE31-DEFFT  PICTURE  9(8).                    CI0063
            12            PE31-DLP20  PICTURE  9(8).                    CI0063
            12            PE31-DLP201 PICTURE  9(8).                    CI0063
            12            PE31-DLP202 PICTURE  9(8).                    CI0063
            12            PE31-AMRAC  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AMOPE  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CORAT  PICTURE  X(6).                    CI0063
            12            PE31-FILLER PICTURE  X(121).                  CI0063
            12            PE31-FILLER PICTURE  X(6873).                 CI0063
            11            PE31-PE7D.                                    CI0063
            12            PE31-DMAVS  PICTURE  9(8).                    CI0063
            12            PE31-DMAVE  PICTURE  9(8).                    CI0063
            12            PE31-CEBPR  PICTURE  X.                       CI0063
            12            PE31-DEBPS  PICTURE  9(8).                    CI0063
            12            PE31-DEBPE  PICTURE  9(8).                    CI0063
            12            PE31-ITOWN  PICTURE  X.                       CI0063
            12            PE31-AMAVR  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AEBPR  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AMDBG  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-IMAV5  PICTURE  X(1).                    CI0063
            12            PE31-DROPS  PICTURE  9(8).                    CI0063
            12            PE31-DROPE  PICTURE  9(8).                    CI0063
            12            PE31-APPCR  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AROPR  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PRCHGN PICTURE  999V999                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-FILLER PICTURE  X(15).                   CI0063
            11            PE31-PE7L.                                    CI0063
            12            PE31-APGBA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-APRBA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-APGBP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-APRBP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-DWSDT  PICTURE  9(8).                    CI0063
            12            PE31-DWAIT  PICTURE  9(8).                    CI0063
            12            PE31-CPNCG  PICTURE  X.                       CI0063
            12            PE31-DUVDT  PICTURE  9(8).                    CI0063
            11            PE31-PE66.                                    CI0063
            12            PE31-AENTI  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-DBFDT  PICTURE  9(8).                    CI0063
            12            PE31-DATFR  PICTURE  9(8).                    CI0063
            12            PE31-DATLT  PICTURE  9(8).                    CI0063
            12            PE31-ISTWH2 PICTURE  X(1).                    CI0063
            12            PE31-DRFID  PICTURE  9(8).                    CI0063
            12            PE31-DOPOD  PICTURE  9(8).                    CI0063
            12            PE31-COPIN  PICTURE  X(1).                    CI0063
            12            PE31-DPNPS  PICTURE  9(8).                    CI0063
            12            PE31-DRITR  PICTURE  9(8).                    CI0063
            12            PE31-CRIBT  PICTURE  X(1).                    CI0063
            12            PE31-INMVF  PICTURE  X(1).                    CI0063
            12            PE31-FILLER PICTURE  X(134).                  CI0063
            11            PE31-PE8L.                                    CI0063
            12            PE31-APALP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ARALP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-IRDPH  PICTURE  X.                       CI0063
            12            PE31-DEWCN  PICTURE  9(8).                    CI0063
            12            PE31-FILLER PICTURE  X(15).                   CI0063
            11            PE31-AAFEA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-ATPWO  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-SR02.                                    CI0063
            12            PE31-PTBDP  PICTURE  9(2)V99                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AMWAB  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PTALP  PICTURE  9(2)V99                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PTGBP  PICTURE  9(2)V99                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-IATAG  PICTURE  X.                       CI0063
            12            PE31-AGBAP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ARBAP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AGBPP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AALPP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AGBAS  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ARBAS  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ASGBP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AALPS  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-IAPGP  PICTURE  X.                       CI0063
            12            PE31-AALPR  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AGBPR  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ARLPR  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ARBPR  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-IASWA  PICTURE  X.                       CI0063
            12            PE31-DRFIL  PICTURE  9(8).                    CI0063
            11            PE31-PE6T.                                    CI0063
            12            PE31-ISDIS  PICTURE  X.                       CI0063
            12            PE31-IBPER  PICTURE  X.                       CI0063
            12            PE31-PINFL  PICTURE  X(02).                   CI0063
            12            PE31-CINFT  PICTURE  X.                       CI0063
            12            PE31-ARODE  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ARDBL  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ARPSL  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CLONT  PICTURE  X.                       CI0063
            12            PE31-CLONT1 PICTURE  X.                       CI0063
            12            PE31-CLONT2 PICTURE  X.                       CI0063
            12            PE31-DPRPA  PICTURE  9(8).                    CI0063
            12            PE31-ADBSU  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-FILLER PICTURE  X(2362).                 CI0063
            11            PE31-FILLER PICTURE  X(1).                    CI0063
            10            PE31-PE6Y                                     CI0063
                          REDEFINES            PE31-PE32.               CI0063
            11            PE31-PCTSW  PICTURE  S99V999                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-COLPI  PICTURE  X.                       CI0063
            11            PE31-FILLER PICTURE  X(135).                  CI0063
            10            PE31-PE69                                     CI0063
                          REDEFINES            PE31-PE32.               CI0063
            11            PE31-PE98.                                    CI0063
            12            PE31-NRCLN  PICTURE  S9(4)                    CI0063
                          BINARY.                                       CI0063
            12            PE31-FILLER PICTURE  X(2).                    CI0063
            12            PE31-CVACO1 PICTURE  X(3).                    CI0063
            12            PE31-XSW1   PICTURE  X.                       CI0063
            12            PE31-GVAID  PICTURE  X(15).                   CI0063
            12            PE31-DEFFT  PICTURE  9(8).                    CI0063
            12            PE31-FILLER PICTURE  X(2).                    CI0063
            12            PE31-CVTXN  PICTURE  X(4).                    CI0063
            12            PE31-DEFFU  PICTURE  9(8).                    CI0063
            12            PE31-FILLER PICTURE  X(15).                   CI0063
            12            PE31-CAMCN  PICTURE  X(8).                    CI0063
            12            PE31-FILLER PICTURE  X(6).                    CI0063
            12            PE31-CVOFC  PICTURE  S9(5)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CAORT  PICTURE  X(01).                   CI0063
            12            PE31-CVOFCA PICTURE  S9(5)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CVAST1 PICTURE  X.                       CI0063
            12            PE31-CVAST  PICTURE  X.                       CI0063
            12            PE31-FILLER PICTURE  X(2).                    CI0063
            12            PE31-CTRNTB PICTURE  XX.                      CI0063
            12            PE31-DEFFC  PICTURE  9(8).                    CI0063
            12            PE31-FILLER PICTURE  X(56).                   CI0063
            11            PE31-PE99.                                    CI0063
            12            PE31-FILLER PICTURE  X(19000).                CI0063
            10            PE31-PE7G                                     CI0063
                          REDEFINES            PE31-PE32.               CI0063
            11            PE31-PE79.                                    CI0063
            12            PE31-CVACO3 PICTURE  X(3).                    CI0063
            12            PE31-FILLER PICTURE  X(11).                   CI0063
            12            PE31-FILLER PICTURE  X(16).                   CI0063
            12            PE31-GVAID2 PICTURE  X(11).                   CI0063
            12            PE31-FILLER PICTURE  X(02).                   CI0063
            12            PE31-FILLER PICTURE  9(8).                    CI0063
            12            PE31-FILLER PICTURE  S9(7)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-FILLER PICTURE  X(44).                   CI0063
            12            PE31-FILLER PICTURE  X(8).                    CI0063
            12            PE31-DEFFT  PICTURE  9(8).                    CI0063
            12            PE31-DACTG  PICTURE  9(8).                    CI0063
            12            PE31-CRPAY  PICTURE  X(03).                   CI0063
            12            PE31-CHREV  PICTURE  X.                       CI0063
            12            PE31-CFEDD  PICTURE  X(02).                   CI0063
            12            PE31-ALWTAX PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-GESTNS PICTURE  X(2).                    CI0063
            12            PE31-ADSW   PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ASURR4 PICTURE  S9(07)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CSURR  PICTURE  X(01).                   CI0063
            12            PE31-ARPMT  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ALPMTR PICTURE  S9(07)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AFETX5 PICTURE  S9(07)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ANOTX  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-APAYR  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-FILLER PICTURE  X(200).                  CI0063
            11            PE31-PE84                                     CI0063
                          OCCURS       100     TIMES.                   CI0063
            12            PE31-MSACN  PICTURE  X(3).                    CI0063
            12            PE31-AFUNV3 PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-QFUUT  PICTURE  S9(9)V9(4)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AUPAM  PICTURE  S9(3)V9(7)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ITTFI  PICTURE  X.                       CI0063
            12            PE31-FILLER PICTURE  X(57).                   CI0063
            11            PE31-FILLER PICTURE  X(10778).                CI0063
            10            PE31-PE6D                                     CI0063
                          REDEFINES            PE31-PE32                CI0063
                          OCCURS       070     TIMES.                   CI0063
            11            PE31-CVSET  PICTURE  X(5).                    CI0063
            11            PE31-GVSCK.                                   CI0063
            12            PE31-MCLNM9 PICTURE  X(80).                   CI0063
            12            PE31-FILLER PICTURE  X(7).                    CI0063
            11            PE31-GDATA                                    CI0063
                          REDEFINES            PE31-GVSCK.              CI0063
            12            PE31-FILLER PICTURE  X(50).                   CI0063
            12            PE31-QPOIA2 PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-QPOIA3 PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CSPCL  PICTURE  XX.                      CI0063
            12            PE31-DSCCC  PICTURE  9(8).                    CI0063
            12            PE31-ABEPA                                    CI0063
                          REDEFINES            PE31-DSCCC               CI0063
               PICTURE    S9(9)V99                                      CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-QNORU  PICTURE  9(6)V9(5)                CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PCPUC  PICTURE  S9(3)V9(5)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-DLSTC  PICTURE  9(8).                    CI0063
            12            PE31-CTRSNV PICTURE  X(02).                   CI0063
            12            PE31-CCISR  PICTURE  X(02).                   CI0063
            11            PE31-DEFFT  PICTURE  9(8).                    CI0063
            11            PE31-CVAST  PICTURE  X.                       CI0063
            11            PE31-QPOIA  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-CLSEX  PICTURE  X.                       CI0063
            11            PE31-QNOUN  PICTURE  9(6)V9(5)                CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-CPRCC  PICTURE  X.                       CI0063
            11            PE31-DPOLI  PICTURE  9(8).                    CI0063
            11            PE31-AGLPA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-CVAPC3 PICTURE  X(6).                    CI0063
            11            PE31-PPRRT2 PICTURE  S9(2)V9(3)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-DRAEN  PICTURE  9(8).                    CI0063
            11            PE31-PE6F                                     CI0063
                          OCCURS       006     TIMES.                   CI0063
            12            PE31-AFLEX  PICTURE  S9(3)V9(2)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-DFEED  PICTURE  9(8).                    CI0063
            11            PE31-APYCT  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-APYMT  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-ARGPA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-AROGP  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-ACYTA  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-CRTYC  PICTURE  X(1).                    CI0063
            11            PE31-CRICO  PICTURE  X(3).                    CI0063
            11            PE31-CACOV  PICTURE  X(02).                   CI0063
            11            PE31-ACECP  PICTURE  S9(09)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-DMDOB2 PICTURE  X(8).                    CI0063
            11            PE31-DTERMG PICTURE  X(8).                    CI0063
            11            PE31-ASRBE  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-ATCDS                                    CI0063
                          REDEFINES            PE31-ASRBE               CI0063
               PICTURE    S9(9)V99                                      CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-ASRRM  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-AMMOB                                    CI0063
                          REDEFINES            PE31-ASRRM               CI0063
               PICTURE    S9(9)V99                                      CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-PASRM  PICTURE  999.                     CI0063
            11            PE31-ASRHL  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-AMMOI                                    CI0063
                          REDEFINES            PE31-ASRHL               CI0063
               PICTURE    S9(9)V99                                      CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-ALTCB  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-ALTCI  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-AAFAA  PICTURE  S9(3)V9(8)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-FILLER PICTURE  X(79).                   CI0063
            10            PE31-PE6S                                     CI0063
                          REDEFINES            PE31-PE32.               CI0063
            11            PE31-ISRRS  PICTURE  X(4).                    CI0063
            11            PE31-ISRFN  PICTURE  X.                       CI0063
            11            PE31-ISRRP  PICTURE  X.                       CI0063
            11            PE31-ISRGB  PICTURE  X.                       CI0063
            11            PE31-ISRRPN PICTURE  X.                       CI0063
            11            PE31-CSPPW  PICTURE  9(2).                    CI0063
            11            PE31-CSRPN  PICTURE  X(2).                    CI0063
            11            PE31-CSRPNB PICTURE  X(120).                  CI0063
            11            PE31-CSRPNA                                   CI0063
                          REDEFINES            PE31-CSRPNB              CI0063
                          OCCURS       020     TIMES.                   CI0063
            12            PE31-CSRPNR PICTURE  X(2).                    CI0063
            12            PE31-ARIPR  PICTURE  S9(2)V9(5)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-FILLER PICTURE  X(50).                   CI0063
            10            PE31-PE6E                                     CI0063
                          REDEFINES            PE31-PE32.               CI0063
            11            PE31-FILLER PICTURE  X(59).                   CI0063
            11            PE31-AGLPB  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-ASRCD  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-ASRCR  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-FILLER PICTURE  X(108).                  CI0063
            11            PE31-ASRHM  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            PE31-PE47                                     CI0063
                          REDEFINES            PE31-PE32.               CI0063
            11            PE31-CALRE  PICTURE  X.                       CI0063
            11            PE31-NFUIA.                                   CI0063
            12            PE31-NFUIO                                    CI0063
                          OCCURS       097     TIMES.                   CI0063
            13            PE31-NFUNR  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            13            PE31-PDEDU  PICTURE  S9(4)V9                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-FILLER PICTURE  X(28674).                CI0063
            10            PE31-PE6K                                     CI0063
                          REDEFINES            PE31-PE32.               CI0063
            11            PE31-COTTC  PICTURE  X(2).                    CI0063
            11            PE31-DABAL  PICTURE  9(8).                    CI0063
            11            PE31-DAEAL  PICTURE  9(8).                    CI0063
            11            PE31-CDTFR  PICTURE  99.                      CI0063
            11            PE31-QTERS  PICTURE  9(3).                    CI0063
            11            PE31-DATRF  PICTURE  99.                      CI0063
            11            PE31-IOVRF  PICTURE  X.                       CI0063
            11            PE31-AOVFE  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            PE31-CSEMR  PICTURE  X.                       CI0063
            11            PE31-ILTOV  PICTURE  X(1).                    CI0063
            11            PE31-DATDT  PICTURE  9(8).                    CI0063
            11            PE31-DATDN  PICTURE  9(8).                    CI0063
            11            PE31-CMEMO  PICTURE  X(2).                    CI0063
            11            PE31-CAREA  PICTURE  X(8).                    CI0063
            11            PE31-CBADT  PICTURE  X(04).                   CI0063
            11            PE31-FILLER PICTURE  X(296).                  CI0063
            11            PE31-PE26                                     CI0063
                          OCCURS       097     TIMES.                   CI0063
            12            PE31-ITFTI  PICTURE  X.                       CI0063
            12            PE31-NFUNX  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CAMTX  PICTURE  X.                       CI0063
            12            PE31-ACOTA  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-ACOTA1                                   CI0063
                          REDEFINES            PE31-ACOTA               CI0063
               PICTURE    S9(6)V9(5)                                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PALLO  PICTURE  S999V9                   CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PALLO1                                   CI0063
                          REDEFINES            PE31-PALLO               CI0063
               PICTURE    S9V999                                        CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-FILLER PICTURE  X(18).                   CI0063
            10            PE31-PE6M                                     CI0063
                          REDEFINES            PE31-PE32.               CI0063
            11            PE31-PE6J                                     CI0063
                          OCCURS       024     TIMES.                   CI0063
            12            PE31-CASTA6 PICTURE  X.                       CI0063
            12            PE31-CDPFC  PICTURE  S9(5)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CDPTC  PICTURE  X(4).                    CI0063
            12            PE31-CMEMO  PICTURE  X(2).                    CI0063
            12            PE31-IRENR  PICTURE  X(01).                   CI0063
            12            PE31-QDDAM  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-DDEPO  PICTURE  9(8).                    CI0063
            12            PE31-DTIRNN PICTURE  9(8).                    CI0063
            12            PE31-DTIRNL PICTURE  9(8).                    CI0063
            12            PE31-DTIRN  PICTURE  9(8).                    CI0063
            12            PE31-DTERT  PICTURE  9(8).                    CI0063
            12            PE31-ADVAL  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PINRA  PICTURE  S9(3)V9(6)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PCTIU  PICTURE  S9V9(4)                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PCTCP  PICTURE  S9V9(4)                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PCTCPR                                   CI0063
                          REDEFINES            PE31-PCTCP               CI0063
               PICTURE    S9(3)V99                                      CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AGAPJ  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PPARN  PICTURE  S9V9(4)                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-CGNTC  PICTURE  X(4).                    CI0063
            12            PE31-CGNFC  PICTURE  S9(5)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-DINDL  PICTURE  9(8).                    CI0063
            12            PE31-IRENRV PICTURE  X(01).                   CI0063
            12            PE31-AMTRLC PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AMTRL  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AMTRLW PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AMTRLO PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AMTAV  PICTURE  S9(9)V9(2)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AMTAVM PICTURE  S9(9)V9(2)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AMTAVF PICTURE  S9(9)V9(2)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-PENRA  PICTURE  S9(3)V9(6)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AICRI  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-AROFA  PICTURE  S9V9(4)                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            PE31-FILLER PICTURE  X(70).                   CI0063
            11            PE31-CFNDC  PICTURE  S9(3)                    CI0063
                          OCCURS       024     TIMES                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            PE31-PE7S                                     CI0063
                          REDEFINES            PE31-PE32.               CI0063
            11            PE31-CVSTC1 PICTURE  X(4).                    CI0063
            11            PE31-GXRTN1                                   CI0063
                          OCCURS       250     TIMES.                   CI0063
            12            PE31-NFUNDL PICTURE  999.                     CI0063
            12            PE31-CFIDC4 PICTURE  X(3).                    CI0063
            12            PE31-CTFIN  PICTURE  X.                       CI0063
            12            PE31-NFDNM  PICTURE  X(20).                   CI0063
            12            PE31-FILLER PICTURE  X(40).                   CI0063
            10            PE31-INDSP  PICTURE  X.                       CI0063
       01          VANTAGE-ACCESS-MODULE  PIC X(8)                      AAZ000
                           VALUE         'LPAZ000 '.                    AAZ000
      *
      *ASSIGN POLICY NUMBER TO VANTAGE *
       01  7-PE30-GVAID.
           05  7-PE30-FILLER                PIC X(4)   VALUE SPACES.
           05  7-PE30-CTIDND                PIC X(11).

      ******************************************************************ADUTAB
      **              TABLE BA98 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-BA98-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=BA DSL=TA SEL=98 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-BA98.                                                CI0063
           04    G-BA98-PARAM.                                          CI0063
             10  G-BA98-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0063
                        VALUE      +053.                                CI0063
             10  G-BA98-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0063
                        VALUE      +001.                                CI0063
             10  G-BA98-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0063
                        VALUE      +010.                                CI0063
             10  G-BA98-NUAPP  PICTURE 99                               CI0063
                        VALUE       0.                                  CI0063
             10  G-BA98-NUTAB  PICTURE X(6)                             CI0063
                        VALUE 'GCPRAR'.                                 CI0063
             10  G-BA98-TABFO  PICTURE XX                 VALUE SPACE.  CI0063
             10  G-BA98-TABCR  PICTURE XX                 VALUE SPACE.  CI0063
             10  G-BA98-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0063
             10  G-BA98-NUSSC  PICTURE X  VALUE   ' '.                  CI0063
             10  G-BA98-NUSSY  PICTURE X                  VALUE SPACE.  CI0063
             10  G-BA98-TRANID PICTURE X(4)               VALUE SPACE.  CI0063
             10  G-BA98-FILSYS.                                         CI0063
             15  G-BA98-USERC  PICTURE X(6)               VALUE SPACE.  CI0063
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0063
           04             BA98.                                         CI0063
            10            BA98-GCPRAR.                                  CI0063
            11            BA98-CTIDA  PICTURE  9(3)                     CI0063
                          VALUE                ZERO.                    CI0063
            11            BA98-PRCOD  PICTURE  9(5)                     CI0063
                          VALUE                ZERO.                    CI0063
            11            BA98-CARTY  PICTURE  99                       CI0063
                          VALUE                ZERO.                    CI0063
            10            BA98-IARTYA PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IARLNA PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQAN  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQSA  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQFM  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQQT  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQBM  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQMO  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQBF  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQSM  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQBW  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQWK  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQIR  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQIF  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQIS  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQIK  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQIW  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQOD  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IARPSA PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IARRGA PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IFQET  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IMLNA  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-IMPRA  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            BA98-ZDA20  PICTURE  X(20)                    CI0063
                          VALUE                SPACE.                   CI0063
      **                                                                ADUTAB
      *      WORK AREA FOR PMS AAOAG3                                   AAOAG3
       01  7-OAGE-PASSED-FIELDS.                                        AAOAG3
           05  7-OAGE-BIRTH-DATE      PIC 9(7).                         AAOAG3
           05  FILLER REDEFINES                                         AAOAG3
               7-OAGE-BIRTH-DATE.                                       AAOAG3
               10  7-OAGE-BD-CCYY     PIC 9(4).                         AAOAG3
               10  7-OAGE-BD-CCYY-RED REDEFINES                         AAOAG3
                   7-OAGE-BD-CCYY.                                      AAOAG3
                   15  7-OAGE-BD-CC   PIC 99.                           AAOAG3
                   15  7-OAGE-BD-YY   PIC 99.                           AAOAG3
               10  7-OAGE-BD-DDD      PIC 999.                          AAOAG3
           05  7-OAGE-CURRENT-DATE    PIC 9(7).                         AAOAG3
           05  FILLER REDEFINES                                         AAOAG3
               7-OAGE-CURRENT-DATE.                                     AAOAG3
               10  7-OAGE-CD-CCYY     PIC 9(4).                         AAOAG3
               10  7-OAGE-CD-CCYY-RED REDEFINES                         AAOAG3
                   7-OAGE-CD-CCYY.                                      AAOAG3
                   15  7-OAGE-CD-CC   PIC 99.                           AAOAG3
                   15  7-OAGE-CD-YY   PIC 99.                           AAOAG3
               10  7-OAGE-CD-DDD      PIC 999.                          AAOAG3
       01  7-OAGE-WORK-AREA.                                            AAOAG3
           05  7-OAGE-CLIENT-AGE      PIC 999V9.                        AAOAG3
           05  7-OAGE-AGE-DAYS        PIC 999.                          AAOAG3
           05  7-OAGE-AGE-YRS         PIC 999.                          AAOAG3
       01  7-OAGE-LITERALS.                                             AAOAG3
           05  7-OAGE-DAYS-IN-HALF-YR PIC 999  VALUE 182.               AAOAG3
           05  7-OAGE-DAYS-IN-A-YR    PIC 999  VALUE 365.               AAOAG3
           05  7-OAGE-HALF-PERCENT    PIC V9   VALUE .5.                AAOAG3
           05  7-OAGE-19TH-CENTURY    PIC 99   VALUE 19.                AAOAG3
           05  7-OAGE-20TH-CENTURY    PIC 99   VALUE 20.                AAOAG3
      *      END OF WORK AREA FOR PMS AAOAG3                            AAOAG3
      ******************************************************************AM0400
      ** WORKING STORAGE FOR CI0400                                    *AM0400
      ******************************************************************AM0400
      *                                                                 AM0400
      *!WF DSP=K9 DSL=K9 SEL=10 FOR=I DES=2 LEV=1                       AM0400
       01                 K910.                                         CI0063
            10            K910-DIRAYR PICTURE  9(4)                     CI0063
                          VALUE                ZERO.                    CI0063
            10            K910-CQACT  PICTURE  999                      CI0063
                          VALUE                ZERO.                    CI0063
            10            K910-CIRAT  PICTURE  999                      CI0063
                          VALUE                ZERO.                    CI0063
            10            K910-CIRAS  PICTURE  999                      CI0063
                          VALUE                ZERO.                    CI0063
            10            K910-CIRAP  PICTURE  XX                       CI0063
                          VALUE                SPACE.                   CI0063
            10            K910-CLDOB  PICTURE  9(8)                     CI0063
                          VALUE                ZERO.                    CI0063
            10            K910-ACLIM  PICTURE  S9(7)V9(2)               CI0063
                          VALUE                ZERO.                    CI0063
            10            K910-GERTC  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            K910-CSQLRC PICTURE  S9(3)                    CI0063
                          VALUE                ZERO.                    CI0063
            10            K910-ACASH  PICTURE  S9(9)V99                 CI0063
                          VALUE                ZERO                     CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            K910-ICBLM  PICTURE  X(1)                     CI0063
                          VALUE                SPACE.                   CI0063
      *                                                                 AM0400
      *                                                                 AM0400
      ******************************************************************ADUTAB
      **              TABLE CA5B ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-CA5B-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=CA DSL=TA SEL=5B FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-CA5B.                                                CI0063
           04    G-CA5B-PARAM.                                          CI0063
             10  G-CA5B-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0063
                        VALUE      +154.                                CI0063
             10  G-CA5B-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0063
                        VALUE      +001.                                CI0063
             10  G-CA5B-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0063
                        VALUE      +017.                                CI0063
             10  G-CA5B-NUAPP  PICTURE 99                               CI0063
                        VALUE       0.                                  CI0063
             10  G-CA5B-NUTAB  PICTURE X(6)                             CI0063
                        VALUE 'TA005B'.                                 CI0063
             10  G-CA5B-TABFO  PICTURE XX                 VALUE SPACE.  CI0063
             10  G-CA5B-TABCR  PICTURE XX                 VALUE SPACE.  CI0063
             10  G-CA5B-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0063
             10  G-CA5B-NUSSC  PICTURE X  VALUE   ' '.                  CI0063
             10  G-CA5B-NUSSY  PICTURE X                  VALUE SPACE.  CI0063
             10  G-CA5B-TRANID PICTURE X(4)               VALUE SPACE.  CI0063
             10  G-CA5B-FILSYS.                                         CI0063
             15  G-CA5B-USERC  PICTURE X(6)               VALUE SPACE.  CI0063
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0063
           04             CA5B.                                         CI0063
            10            CA5B-GAPSC.                                   CI0063
            11            CA5B-CTIDA  PICTURE  9(3)                     CI0063
                          VALUE                ZERO.                    CI0063
            11            CA5B-PRCOD  PICTURE  9(5)                     CI0063
                          VALUE                ZERO.                    CI0063
            11            CA5B-PRSCD  PICTURE  X(9)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-PRCODX PICTURE  9(5)                     CI0063
                          VALUE                ZERO.                    CI0063
            10            CA5B-PRCSUB PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-PRCAUT PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-PRCBAS PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-PRCSTK PICTURE  XX                       CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-PRCPRE PICTURE  X(4)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-IBDUP  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-IUSPR  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-CVSYS  PICTURE  X(2)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-IDTOD  PICTURE  X(1)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-GRSFC  PICTURE  99                       CI0063
                          VALUE                ZERO.                    CI0063
            10            CA5B-ZDA18  PICTURE  X(18)                    CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-CMPCTB PICTURE  X(4)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-ITERM  PICTURE  X(1)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-AMFAC  PICTURE  S9(7)                    CI0063
                          VALUE                ZERO.                    CI0063
            10            CA5B-ZDA20  PICTURE  X(20)                    CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-CPRBK  PICTURE  X(3)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-CFXDM  PICTURE  99                       CI0063
                          VALUE                ZERO.                    CI0063
            10            CA5B-NGLCS  PICTURE  X(5)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-NDFCS  PICTURE  X(5)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-ZDA20  PICTURE  X(20)                    CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-CTNLI  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-CBANK  PICTURE  X(03)                    CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-ISYPO  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-ISYPP  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-ICOPT  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-IANPY  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-IDSAR  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-ICIPT  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-IANDS  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-IKPMA  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-INMWT  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-IVANT  PICTURE  X(1)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-ISDAV  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-IUDAV  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            CA5B-ZDA15  PICTURE  X(15)                    CI0063
                          VALUE                SPACE.                   CI0063
      **                                                                ADUTAB
       01  CA5B-CF           PIC X(1) VALUE '0'.
       01                 CL01.                                         CI0063
            10            CL01-CL01K.                                   CI0063
            11            CL01-C199.                                    CI0063
            12            CL01-CLID.                                    CI0063
            13            CL01-CLIDO  PICTURE  9(3).                    CI0063
            13            CL01-CLIDN.                                   CI0063
            14            CL01-CLIDNP PICTURE  X(12).                   CI0063
            14            CL01-CLIDND PICTURE  9(8).                    CI0063
            10            CL01-GECKD  PICTURE  9.                       CI0063
            10            CL01-GEMDA  PICTURE  9(8).                    CI0063
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0063
                          BINARY.                                       CI0063
            10            CL01-GECUC  PICTURE  99.                      CI0063
            10            CL01-CLDOR  PICTURE  9(8).                    CI0063
            10            CL01-CLLNG  PICTURE  XX.                      CI0063
            10            CL01-GESLC  PICTURE  99.                      CI0063
            10            CL01-CLTYP  PICTURE  X.                       CI0063
            10            CL01-CLCLS  PICTURE  9(3).                    CI0063
            10            CL01-CLTWRC PICTURE  99.                      CI0063
            10            CL01-CLPVC  PICTURE  99.                      CI0063
            10            CL01-CLIND  PICTURE  9(3).                    CI0063
            10            CL01-CLTRC  PICTURE  99.                      CI0063
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CL01-AYSIDA PICTURE  9(3).                    CI0063
            10            CL01-AYSID  PICTURE  9(5).                    CI0063
            10            CL01-CLSTR  PICTURE  9(2).                    CI0063
            10            CL01-CLC11  PICTURE  X.                       CI0063
            10            CL01-CLTIN  PICTURE  9(12).                   CI0063
            10            CL01-CLTND  PICTURE  9(8).                    CI0063
            10            CL01-CLTINC PICTURE  9.                       CI0063
            10            CL01-CCDWA  PICTURE  9.                       CI0063
            10            CL01-CICES  PICTURE  X.                       CI0063
            10            CL01-CLTRA  PICTURE  9(2).                    CI0063
            10            CL01-DIRSY  PICTURE  9(4)                     CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CL01-CFEDS  PICTURE  X.                       CI0063
            10            CL01-FILLER PICTURE  X(06).                   CI0063
       01                 CL03.                                         CI0063
            10            CL03-GEDLA  PICTURE  9(8).                    CI0063
            10            CL03-DDREP  PICTURE  9(8).                    CI0063
            10            CL03-DPRFR  PICTURE  9(8).                    CI0063
            10            CL03-IACCI  PICTURE  X.                       CI0063
            10            CL03-CLDOB  PICTURE  9(8).                    CI0063
            10            CL03-CLDOD  PICTURE  9(8).                    CI0063
            10            CL03-CLDTH  PICTURE  X.                       CI0063
            10            CL03-CCINI  PICTURE  X.                       CI0063
            10            CL03-FILLER PICTURE  X(1).                    CI0063
            10            CL03-CLAIN  PICTURE  S9(11)                   CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CL03-CCAOD  PICTURE  999.                     CI0063
            10            CL03-CLMAR  PICTURE  X.                       CI0063
            10            CL03-C198.                                    CI0063
            11            CL03-CLNAM.                                   CI0063
            12            CL03-CLNAMH PICTURE  X(6).                    CI0063
            12            CL03-CLNAMF PICTURE  X(20).                   CI0063
            12            CL03-CLNAMM.                                  CI0063
            13            CL03-CLNAMI PICTURE  X.                       CI0063
            13            CL03-CLNAMR PICTURE  X(14).                   CI0063
            12            CL03-CLNAML PICTURE  X(25).                   CI0063
            12            CL03-CLNAMS PICTURE  X(4).                    CI0063
            10            CL03-FILLER PICTURE  X(10).                   CI0063
            10            CL03-MPRFS  PICTURE  X(4).                    CI0063
            10            CL03-CLOCC  PICTURE  9(3).                    CI0063
            10            CL03-CLRET  PICTURE  X.                       CI0063
            10            CL03-IOCOB  PICTURE  X.                       CI0063
            10            CL03-CLSEX  PICTURE  X.                       CI0063
            10            CL03-CLWIL  PICTURE  X.                       CI0063
            10            CL03-GECFC  PICTURE  99.                      CI0063
            10            CL03-GECFY  PICTURE  9(4).                    CI0063
            10            CL03-ICUSC  PICTURE  X.                       CI0063
            10            CL03-MCTYC  PICTURE  X(20).                   CI0063
            10            CL03-CLWIP  PICTURE  X.                       CI0063
            10            CL03-CLCTXF PICTURE  99.                      CI0063
            10            CL03-CLCUS  PICTURE  99.                      CI0063
            10            CL03-NPDLU  PICTURE  9(5).                    CI0063
            10            CL03-CLEMI  PICTURE  X.                       CI0063
            10            CL03-GEPHNH PICTURE  X(14).                   CI0063
            10            CL03-GEPHNB PICTURE  X(14).                   CI0063
            10            CL03-GEPHNX PICTURE  9(4).                    CI0063
            10            CL03-GEPHNA PICTURE  X(14).                   CI0063
            10            CL03-FILLER PICTURE  X(3).                    CI0063
            10            CL03-IAPRT  PICTURE  X.                       CI0063
            10            CL03-CEMSC  PICTURE  X.                       CI0063
            10            CL03-CSEPS  PICTURE  X.                       CI0063
            10            CL03-CRACE  PICTURE  X.                       CI0063
            10            CL03-CNIRA  PICTURE  X.                       CI0063
            10            CL03-FILLER PICTURE  X(11).                   CI0063
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0018           PIC X(8) VALUE 'CI0018P '.                  AM0018
       01  CI0020           PIC X(8) VALUE 'CI0020P '.                  AM0020
       01  CI0400           PIC X(8) VALUE 'CI0400P '.                  AM0400
       01  CI0975           PIC X(08) VALUE 'CI0975P'.                  AM0975
       01                 CM01.                                         CI0063
            10            CM01-CEKCNT.                                  CI0063
            11            CM01-CEGCN.                                   CI0063
            12            CM01-CEPRE  PICTURE  9(4).                    CI0063
            12            CM01-CEBAS  PICTURE  9(8).                    CI0063
            10            CM01-CEMAD  PICTURE  9(8).                    CI0063
            10            CM01-CEAUD  PICTURE  9(8).                    CI0063
            10            CM01-CEBKD  PICTURE  9(8).                    CI0063
            10            CM01-CELAD  PICTURE  9(8).                    CI0063
            10            CM01-CECSD  PICTURE  9(8).                    CI0063
            10            CM01-CECOM  PICTURE  9(2).                    CI0063
            10            CM01-CEPDT  PICTURE  9(4).                    CI0063
            10            CM01-CECHK  PICTURE  9(1).                    CI0063
            10            CM01-CECTC  PICTURE  X(1).                    CI0063
            10            CM01-CEST   PICTURE  9(1).                    CI0063
            10            CM01-CETYP  PICTURE  9(2).                    CI0063
            10            CM01-CECLO  PICTURE  9(1).                    CI0063
            10            CM01-CEAV   PICTURE  9(1).                    CI0063
            10            CM01-CE30M  PICTURE  X(1).                    CI0063
            10            CM01-CEBCD  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-CEUNT  PICTURE  S9(5)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-CEFAC  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-CERD   PICTURE  9(3).                    CI0063
            10            CM01-CEDST  PICTURE  9(2).                    CI0063
            10            CM01-CEIPN  PICTURE  9(1).                    CI0063
            10            CM01-CEMAL  PICTURE  X(1).                    CI0063
            10            CM01-CEMALC PICTURE  9(2).                    CI0063
            10            CM01-CEUNL  PICTURE  X(1).                    CI0063
            10            CM01-CEMLN  PICTURE  X(1).                    CI0063
            10            CM01-CEYEX  PICTURE  9(2).                    CI0063
            10            CM01-CELLO  PICTURE  9(1).                    CI0063
            10            CM01-CESLD  PICTURE  9(8).                    CI0063
            10            CM01-CELAT.                                   CI0063
            11            CM01-CETMJ  PICTURE  9(3).                    CI0063
            11            CM01-CETMN  PICTURE  9(3).                    CI0063
            10            CM01-CELATD PICTURE  9(8).                    CI0063
            10            CM01-CEYER  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-CEPYR  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-CELIN  PICTURE  X(1).                    CI0063
            10            CM01-CEANP  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-FILLER PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-FILLER PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-CELBD  PICTURE  9(8).                    CI0063
            10            CM01-CELMC  PICTURE  9(3).                    CI0063
            10            CM01-CEXNO  PICTURE  S9(9)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-CEORS  PICTURE  9(2).                    CI0063
            10            CM01-CESST  PICTURE  9(2).                    CI0063
            10            CM01-CESAG  PICTURE  9(2).                    CI0063
            10            CM01-CEOSD  PICTURE  9(3).                    CI0063
            10            CM01-CEORD  PICTURE  9(3).                    CI0063
            10            CM01-CECLS  PICTURE  9(2).                    CI0063
            10            CM01-CEIND  PICTURE  9(3).                    CI0063
            10            CM01-CEOCC  PICTURE  9(3).                    CI0063
            10            CM01-CEASI  PICTURE  X(1).                    CI0063
            10            CM01-CESUB  PICTURE  9(3)                     CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-CERDC  PICTURE  9(1).                    CI0063
            10            CM01-CECEA  PICTURE  S9(13)                   CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-CETEA  PICTURE  S9(13)                   CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-FILLER PICTURE  X(1).                    CI0063
            10            CM01-COPDC  PICTURE  9.                       CI0063
            10            CM01-FILLER PICTURE  9(1).                    CI0063
            10            CM01-CEOWN  PICTURE  9(2).                    CI0063
            10            CM01-CESSC  PICTURE  9(2).                    CI0063
            10            CM01-CESSI  PICTURE  X(1).                    CI0063
            10            CM01-CECID  PICTURE  9(8).                    CI0063
            10            CM01-CECO   PICTURE  9(2).                    CI0063
            10            CM01-DSLRT  PICTURE  9(8).                    CI0063
            10            CM01-CEYPR  PICTURE  9(2).                    CI0063
            10            CM01-CEIPCB PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-CETTR  PICTURE  X(1).                    CI0063
            10            CM01-CESCM  PICTURE  9(2).                    CI0063
            10            CM01-CEBSA  PICTURE  X(1).                    CI0063
            10            CM01-CESX   PICTURE  9(1).                    CI0063
            10            CM01-CERZC  PICTURE  S9(9)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-CERBD  PICTURE  9(8).                    CI0063
            10            CM01-CECUS  PICTURE  9(2).                    CI0063
            10            CM01-CEDUW  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-CEDCB  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-CTIDA1 PICTURE  9(3).                    CI0063
            10            CM01-IBPR1  PICTURE  9.                       CI0063
            10            CM01-IBPR2  PICTURE  9.                       CI0063
            10            CM01-IBPR3  PICTURE  9.                       CI0063
            10            CM01-IBPR4  PICTURE  9.                       CI0063
            10            CM01-IBPR0  PICTURE  9.                       CI0063
            10            CM01-CEXP   PICTURE  9(2).                    CI0063
            10            CM01-FILLER PICTURE  S9(7)V99                 CI0063
                          OCCURS       004     TIMES                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM01-GRPLT  PICTURE  99.                      CI0063
            10            CM01-FILLER PICTURE  X(16).                   CI0063
       01                 CM24.                                         CI0063
            10            CM24-CELLT.                                   CI0063
            11            CM24-CETMJ  PICTURE  9(3).                    CI0063
            11            CM24-CETMN  PICTURE  9(3).                    CI0063
            10            CM24-CELLD  PICTURE  9(8).                    CI0063
            10            CM24-CELFD  PICTURE  9(8).                    CI0063
            10            CM24-CELPD  PICTURE  9(8).                    CI0063
            10            CM24-CELDD  PICTURE  9(8).                    CI0063
            10            CM24-CELBL  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM24-FILLER PICTURE  S9(6)V9(7)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM24-CELBD  PICTURE  9(8).                    CI0063
            10            CM24-FILLER PICTURE  S9(6)V9(7)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM24-CELIR  PICTURE  SV9(5)                   CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM24-CELLBA PICTURE  S9(7)V9(2)               CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM24-CEL12  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM24-FILLER PICTURE  S9(7)V99                 CI0063
                          OCCURS       002     TIMES                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CM24-FILLER PICTURE  X(10).                   CI0063
       01                 CM54.                                         CI0063
            10            CM54-CEKLBL.                                  CI0063
            11            CM54-CESQ2  PICTURE  9(2).                    CI0063
            10            CM54-CEGLB                                    CI0063
                          OCCURS       010     TIMES.                   CI0063
            11            CM54-CELBDT PICTURE  9(8).                    CI0063
            11            CM54-CELBA  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CM54-CEPTA  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CM54-CEPTC1.                                  CI0063
            12            CM54-CEPTC.                                   CI0063
            13            CM54-CETMJ  PICTURE  9(3).                    CI0063
            13            CM54-CETMN  PICTURE  9(3).                    CI0063
            12            CM54-FILLER PICTURE  X(2).                    CI0063
            11            CM54-DTRME                                    CI0063
                          REDEFINES            CM54-CEPTC1              CI0063
               PICTURE    9(8).                                         CI0063
            11            CM54-FILLER                                   CI0063
                          REDEFINES            CM54-CEPTC1.             CI0063
            12            CM54-ACAPI  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            CM54-FILLER PICTURE  X(2).                    CI0063
            11            CM54-NBSEI  PICTURE  999V99                   CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CM54-FILLER PICTURE  X(2).                    CI0063
            11            CM54-NESEI  PICTURE  999V99                   CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CM54-ACMTH                                    CI0063
                          REDEFINES            CM54-NESEI               CI0063
               PICTURE    S9(5)                                         CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CM54-CETLB  PICTURE  9(3).                    CI0063
            11            CM54-FILLER PICTURE  X(3).                    CI0063
            10            CM54-FILLER PICTURE  X(1).                    CI0063
      *
       01   7-CTID-REFORMAT.
            05  7-CM01-CEPRE    PIC  9(4).
            05  7-CM01-CEBAS    PIC  9(7).

       01   7-CM01-FORMAT.
            05  7-CM01-ZERO     PIC  9    VALUE ZERO.
            05  7-CM01-CEBASL   PIC  9(7).
       01                 CX01.                                         CI0063
            10            CX01-CX01K.                                   CI0063
            11            CX01-C199.                                    CI0063
            12            CX01-CLID.                                    CI0063
            13            CX01-CLIDO  PICTURE  9(3).                    CI0063
            13            CX01-CLIDN.                                   CI0063
            14            CX01-CLIDNP PICTURE  X(12).                   CI0063
            14            CX01-CLIDND PICTURE  9(8).                    CI0063
            10            CX01-GEMDA  PICTURE  9(8).                    CI0063
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0063
                          BINARY.                                       CI0063
            10            CX01-FILLER PICTURE  X(5).                    CI0063
       01                 CX03.                                         CI0063
            10            CX03-GELL   PICTURE  9(4)                     CI0063
                          BINARY.                                       CI0063
            10            CX03-CY00.                                    CI0063
            11            CX03-CX03K.                                   CI0063
            12            CX03-CARTY  PICTURE  99.                      CI0063
            12            CX03-NARRS  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX03-CARST  PICTURE  99.                      CI0063
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX03-CPMTG  PICTURE  99.                      CI0063
            11            CX03-GRCRNG PICTURE  9(3).                    CI0063
            11            CX03-DEXDT  PICTURE  9(8).                    CI0063
            11            CX03-DASUP  PICTURE  9(8).                    CI0063
            11            CX03-CSTEC  PICTURE  X(3).                    CI0063
            11            CX03-FILLER PICTURE  X(17).                   CI0063
            11            CX03-CY50.                                    CI0063
            12            CX03-NARID  PICTURE  X(30).                   CI0063
            11            CX03-CY51                                     CI0063
                          REDEFINES            CX03-CY50.               CI0063
            12            CX03-NDIDN  PICTURE  9(12).                   CI0063
            12            CX03-FILLER PICTURE  X(18).                   CI0063
            11            CX03-CY52                                     CI0063
                          REDEFINES            CX03-CY50.               CI0063
            12            CX03-NAIDC  PICTURE  9(12).                   CI0063
            12            CX03-FILLER PICTURE  X(18).                   CI0063
            11            CX03-CY53                                     CI0063
                          REDEFINES            CX03-CY50.               CI0063
            12            CX03-NAMEXB PICTURE  9(15).                   CI0063
            12            CX03-FILLER PICTURE  X(15).                   CI0063
            10            CX03-CY99.                                    CI0063
            11            CX03-FILLER PICTURE  X(109).                  CI0063
            10            CX03-CY01                                     CI0063
                          REDEFINES            CX03-CY99.               CI0063
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX03-ICPCI  PICTURE  X.                       CI0063
            11            CX03-CLUPD  PICTURE  9(3).                    CI0063
            11            CX03-DLAUP  PICTURE  9(8).                    CI0063
            11            CX03-CWRC   PICTURE  99.                      CI0063
            11            CX03-CHCR   PICTURE  99.                      CI0063
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0063
            11            CX03-GEAUN  PICTURE  9(5).                    CI0063
            11            CX03-DPCHD  PICTURE  9(8).                    CI0063
            11            CX03-DLRCHK PICTURE  9(8).                    CI0063
            11            CX03-QTRCHK PICTURE  9(2).                    CI0063
            11            CX03-DNPMT  PICTURE  9(8).                    CI0063
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CX03-CY02                                     CI0063
                          REDEFINES            CX03-CY99.               CI0063
            11            CX03-QSIRQ  PICTURE  99.                      CI0063
            11            CX03-QDRMN  PICTURE  9(2)                     CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX03-DDPRE  PICTURE  9(8).                    CI0063
            11            CX03-DDSHP  PICTURE  9(8).                    CI0063
            11            CX03-NDRFTB PICTURE  9(5).                    CI0063
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0063
            11            CX03-DDSHPA PICTURE  9(8).                    CI0063
            11            CX03-NDRFTF PICTURE  9(5).                    CI0063
            11            CX03-QDIPBK PICTURE  9(3).                    CI0063
            11            CX03-CREOR  PICTURE  X(1).                    CI0063
            11            CX03-CREOR1 PICTURE  X(1).                    CI0063
            11            CX03-DDASC  PICTURE  9(8).                    CI0063
            11            CX03-FILLER PICTURE  X(7).                    CI0063
            10            CX03-CY03                                     CI0063
                          REDEFINES            CX03-CY99.               CI0063
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0063
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0063
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0063
            11            CX03-DOPDA  PICTURE  99.                      CI0063
            11            CX03-CPMTF  PICTURE  99.                      CI0063
            11            CX03-CIRMO  PICTURE  X(12).                   CI0063
            11            CX03-CPALL  PICTURE  X(1).                    CI0063
            11            CX03-CCOLM  PICTURE  9(2).                    CI0063
            11            CX03-CBLTP  PICTURE  X(1).                    CI0063
            11            CX03-CASUB  PICTURE  9(2).                    CI0063
            11            CX03-CBLFM  PICTURE  9(2).                    CI0063
            11            CX03-IBILS  PICTURE  X.                       CI0063
            11            CX03-IPAOS  PICTURE  X.                       CI0063
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0063
            11            CX03-DLBPD  PICTURE  9(8).                    CI0063
            11            CX03-DNBPD  PICTURE  9(8).                    CI0063
            11            CX03-DODBD  PICTURE  9(8).                    CI0063
            11            CX03-CPSRE  PICTURE  99.                      CI0063
            11            CX03-ISPHN  PICTURE  X.                       CI0063
            11            CX03-TCARR  PICTURE  X(6).                    CI0063
            11            CX03-CBKPT  PICTURE  9(2).                    CI0063
            11            CX03-IECNT  PICTURE  X.                       CI0063
            11            CX03-ICONV  PICTURE  X(1).                    CI0063
            11            CX03-FILLER PICTURE  X(4).                    CI0063
            10            CX03-CY04                                     CI0063
                          REDEFINES            CX03-CY99.               CI0063
            11            CX03-CCARD  PICTURE  X(02).                   CI0063
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0063
            11            CX03-IREMT  PICTURE  X(01).                   CI0063
            11            CX03-ISBILA PICTURE  X.                       CI0063
            11            CX03-DLBPDA PICTURE  9(8).                    CI0063
            11            CX03-DNBPDA.                                  CI0063
            12            CX03-DNCYM  PICTURE  9(6).                    CI0063
            12            CX03-CEDTD  PICTURE  9(2).                    CI0063
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX03-DREMT  PICTURE  9(8).                    CI0063
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0063
            11            CX03-CWRC2  PICTURE  99.                      CI0063
            11            CX03-CHCR2  PICTURE  99.                      CI0063
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0063
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0063
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0063
       01                 CX06.                                         CI0063
            10            CX06-CX06K.                                   CI0063
            11            CX06-C299.                                    CI0063
            12            CX06-CTID.                                    CI0063
            13            CX06-CTIDA  PICTURE  9(3).                    CI0063
            13            CX06-CTIDN.                                   CI0063
            14            CX06-CTIDNP PICTURE  X(13).                   CI0063
            14            CX06-CTIDND PICTURE  9(11).                   CI0063
            10            CX06-NPECK  PICTURE  9(02).                   CI0063
            10            CX06-FILLER PICTURE  X.                       CI0063
       01                 CX12.                                         CI0063
            10            CX12-CX12K.                                   CI0063
            11            CX12-CPMTC  PICTURE  99.                      CI0063
            11            CX12-NAPDS  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX12-GESTD  PICTURE  9(8).                    CI0063
            10            CX12-GEEND  PICTURE  9(8).                    CI0063
            10            CX12-CIRMO  PICTURE  X(12).                   CI0063
            10            CX12-CDEST  PICTURE  99.                      CI0063
            10            CX12-APMTL  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CX12-DNPMT  PICTURE  9(8).                    CI0063
            10            CX12-NIRACM PICTURE  9(2).                    CI0063
            10            CX12-CPMTF  PICTURE  99.                      CI0063
            10            CX12-IPODM  PICTURE  X.                       CI0063
            10            CX12-CLUPD  PICTURE  9(3).                    CI0063
            10            CX12-DLAUP  PICTURE  9(8).                    CI0063
            10            CX12-CWRC   PICTURE  99.                      CI0063
            10            CX12-CHCR   PICTURE  99.                      CI0063
            10            CX12-GEOPD2 PICTURE  X(8).                    CI0063
            10            CX12-GEAUN  PICTURE  9(5).                    CI0063
            10            CX12-DPCHD  PICTURE  9(8).                    CI0063
            10            CX12-DNEXE  PICTURE  9(8).                    CI0063
            10            CX12-CCSMQ  PICTURE  X.                       CI0063
            10            CX12-GCUSPZ PICTURE  X(12).                   CI0063
            10            CX12-CORTY  PICTURE  X.                       CI0063
            10            CX12-CNAVR  PICTURE  X(1).                    CI0063
            10            CX12-DELOI3 PICTURE  9(6).                    CI0063
            10            CX12-ALOIDD PICTURE  9(9)V99                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CX12-FILLER PICTURE  X(5).                    CI0063
       01                 CX13.                                         CI0063
            10            CX13-GELL   PICTURE  9(4)                     CI0063
                          BINARY.                                       CI0063
            10            CX13-CY20.                                    CI0063
            11            CX13-CX13K.                                   CI0063
            12            CX13-CARTZ  PICTURE  99.                      CI0063
            12            CX13-NAPDS  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX13-GESTD  PICTURE  9(8).                    CI0063
            11            CX13-GEEND  PICTURE  9(8).                    CI0063
            11            CX13-DASUQ  PICTURE  9(8).                    CI0063
            11            CX13-CDEST  PICTURE  99.                      CI0063
            11            CX13-IIARR  PICTURE  X.                       CI0063
            11            CX13-DLAUP  PICTURE  9(8).                    CI0063
            11            CX13-GEOPD2 PICTURE  X(8).                    CI0063
            11            CX13-GEAUN  PICTURE  9(5).                    CI0063
            11            CX13-DPCHD  PICTURE  9(8).                    CI0063
            11            CX13-PPOT1  PICTURE  S9(3)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX13-ACOT1  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX13-QPST1  PICTURE  S9(7)V999                CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX13-FILLER PICTURE  X(03).                   CI0063
            10            CX13-CY96.                                    CI0063
            11            CX13-FILLER PICTURE  X(50).                   CI0063
            10            CX13-CY21                                     CI0063
                          REDEFINES            CX13-CY96.               CI0063
            11            CX13-DNPMT  PICTURE  9(8).                    CI0063
            11            CX13-CPMTF  PICTURE  99.                      CI0063
            11            CX13-ADBRQ  PICTURE  S9(11)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX13-QSHOWQ PICTURE  S9(9)V999                CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX13-PACT1  PICTURE  S999V999                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX13-DOPDA  PICTURE  99.                      CI0063
            11            CX13-DNEXE  PICTURE  9(8).                    CI0063
            11            CX13-CIRMO  PICTURE  X(12).                   CI0063
            10            CX13-CY98.                                    CI0063
            11            CX13-FILLER PICTURE  X(120).                  CI0063
            10            CX13-CY25                                     CI0063
                          REDEFINES            CX13-CY98.               CI0063
            11            CX13-COPTC  PICTURE  9(1).                    CI0063
            11            CX13-ILPOI  PICTURE  X(1).                    CI0063
            11            CX13-CATOC  PICTURE  X(1).                    CI0063
            11            CX13-CEOIA  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX13-ACOAR  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX13-CEOTR  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX13-DSTMO  PICTURE  99.                      CI0063
            10            CX13-CY27                                     CI0063
                          REDEFINES            CX13-CY98.               CI0063
            11            CX13-QMTH1  PICTURE  9(3).                    CI0063
            11            CX13-IDRMD  PICTURE  X.                       CI0063
            10            CX13-CY28                                     CI0063
                          REDEFINES            CX13-CY98.               CI0063
            11            CX13-AALLBL PICTURE  S9(8)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX13-PSURR  PICTURE  S9(3)V999                CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX13-DFPMT  PICTURE  9(8).                    CI0063
            11            CX13-QMTHLA PICTURE  9(3).                    CI0063
            11            CX13-PWHLDS PICTURE  S999V9(5)                CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX13-ISWHO  PICTURE  X(1).                    CI0063
            10            CX13-CY29                                     CI0063
                          REDEFINES            CX13-CY98.               CI0063
            11            CX13-IINDI1 PICTURE  X(1).                    CI0063
            11            CX13-IINDI2 PICTURE  X(1).                    CI0063
            11            CX13-IINDI3 PICTURE  X(1).                    CI0063
            11            CX13-PWHLD5 PICTURE  S999V99                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX13-CCSMQ  PICTURE  X.                       CI0063
            11            CX13-CPLEC  PICTURE  XX.                      CI0063
            11            CX13-IPTRDA PICTURE  X(01).                   CI0063
            11            CX13-GCUSPY PICTURE  X(12).                   CI0063
            11            CX13-ALOIDA PICTURE  S9(11)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            CX13-DELOI  PICTURE  9(8).                    CI0063
            11            CX13-CLGND  PICTURE  X.                       CI0063
            11            CX13-CORTYA PICTURE  X(3).                    CI0063
            11            CX13-CPH3U  PICTURE  X.                       CI0063
            11            CX13-CNAVR  PICTURE  X(1).                    CI0063
            11            CX13-NEXEC  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
       01                 CX14.                                         CI0063
            10            CX14-GELL   PICTURE  9(4)                     CI0063
                          BINARY.                                       CI0063
            10            CX14-CX14K.                                   CI0063
            11            CX14-NPISQ  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CX14-ACOTD  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CX14-PPOTD  PICTURE  S9(3)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CX14-QPSTD  PICTURE  S9(7)V999                CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CX14-CPITC  PICTURE  99.                      CI0063
            10            CX14-FILLER PICTURE  X(04).                   CI0063
            10            CX14-CY97.                                    CI0063
            11            CX14-FILLER PICTURE  X(32).                   CI0063
            10            CX14-CY30                                     CI0063
                          REDEFINES            CX14-CY97.               CI0063
            11            CX14-IOWNC  PICTURE  X.                       CI0063
            11            CX14-CTYPE  PICTURE  X.                       CI0063
            11            CX14-C299.                                    CI0063
            12            CX14-CTID.                                    CI0063
            13            CX14-CTIDA  PICTURE  9(3).                    CI0063
            13            CX14-CTIDN.                                   CI0063
            14            CX14-CTIDNP PICTURE  X(13).                   CI0063
            14            CX14-CTIDND PICTURE  9(11).                   CI0063
            11            CX14-CPMTC  PICTURE  99.                      CI0063
            11            CX14-IACSD  PICTURE  X.                       CI0063
            10            CX14-CY31                                     CI0063
                          REDEFINES            CX14-CY97.               CI0063
            11            CX14-FILLER PICTURE  X(2).                    CI0063
            11            CX14-IDELI  PICTURE  X.                       CI0063
            11            CX14-CDEL1  PICTURE  9(3).                    CI0063
            11            CX14-NDELS  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CX14-CY32                                     CI0063
                          REDEFINES            CX14-CY97.               CI0063
            11            CX14-GCUSPZ PICTURE  X(12).                   CI0063
       01                 CX18.                                         CI0063
            10            CX18-CX18K.                                   CI0063
            11            CX18-NBASQ  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CX18-NPBN   PICTURE  X(20).                   CI0063
            10            CX18-CCBAT  PICTURE  99.                      CI0063
            10            CX18-DACHP  PICTURE  9(8).                    CI0063
            10            CX18-CSTPRE PICTURE  99.                      CI0063
            10            CX18-C199.                                    CI0063
            11            CX18-CLID.                                    CI0063
            12            CX18-CLIDO  PICTURE  9(3).                    CI0063
            12            CX18-CLIDN.                                   CI0063
            13            CX18-CLIDNP PICTURE  X(12).                   CI0063
            13            CX18-CLIDND PICTURE  9(8).                    CI0063
            10            CX18-MCSIG  PICTURE  X(30).                   CI0063
            10            CX18-CPBNU  PICTURE  X.                       CI0063
            10            CX18-CSPCR  PICTURE  99.                      CI0063
            10            CX18-DAPCR  PICTURE  9(8).                    CI0063
            10            CX18-FILLER PICTURE  XX.                      CI0063
       01                 CX2Y.                                         CI0063
            10            CX2Y-CX2YK.                                   CI0063
            11            CX2Y-C299.                                    CI0063
            12            CX2Y-CTID.                                    CI0063
            13            CX2Y-CTIDA  PICTURE  9(3).                    CI0063
            13            CX2Y-CTIDN.                                   CI0063
            14            CX2Y-CTIDNP PICTURE  X(13).                   CI0063
            14            CX2Y-CTIDND PICTURE  9(11).                   CI0063
            11            CX2Y-C199.                                    CI0063
            12            CX2Y-CLID.                                    CI0063
            13            CX2Y-CLIDO  PICTURE  9(3).                    CI0063
            13            CX2Y-CLIDN.                                   CI0063
            14            CX2Y-CLIDNP PICTURE  X(12).                   CI0063
            14            CX2Y-CLIDND PICTURE  9(8).                    CI0063
            11            CX2Y-CARTY  PICTURE  99.                      CI0063
            11            CX2Y-NARRS  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
      ******************************************************************AM0975
      *  LINKAGE SEGMENTS FOR CI0975                                    AM0975
      ******************************************************************AM0975
      *                                                                 AM0975
      *!WF DSP=DA DSL=QT SEL=8G FOR=I DES=2 LEV=1                       AM0975
       01                 DA8G.                                         CI0063
            10            DA8G-CFAUL1 PICTURE  X(4)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            DA8G-TFACT1 PICTURE  X(20)                    CI0063
                          VALUE                SPACE.                   CI0063
            10            DA8G-CFAUL2 PICTURE  X(4)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            DA8G-TFACT2 PICTURE  X(20)                    CI0063
                          VALUE                SPACE.                   CI0063
            10            DA8G-CFAUL3 PICTURE  X(4)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            DA8G-TFACT3 PICTURE  X(20)                    CI0063
                          VALUE                SPACE.                   CI0063
            10            DA8G-CARTY  PICTURE  99                       CI0063
                          VALUE                ZERO.                    CI0063
            10            DA8G-CSYS   PICTURE  X(4)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            DA8G-GERTC  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            DA8G-CERRE  PICTURE  X(4)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            DA8G-QITEM  PICTURE  9(3)                     CI0063
                          VALUE                ZERO.                    CI0063
            10            DA8G-FILLER PICTURE  X(32400)                 CI0063
                          VALUE                SPACE.                   CI0063
            10            DA8G-QT8M                                     CI0063
                          REDEFINES            DA8G-FILLER.             CI0063
            11            DA8G-QT8I                                     CI0063
                          OCCURS       090     TIMES.                   CI0063
            12            DA8G-CTID   PICTURE  X(27).                   CI0063
            12            DA8G-CARST  PICTURE  99.                      CI0063
            12            DA8G-CPMTF  PICTURE  99.                      CI0063
            12            DA8G-CPALL  PICTURE  X(1).                    CI0063
            12            DA8G-PPOTD  PICTURE  S9(3)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            DA8G-ACOTD  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            DA8G-GESTD  PICTURE  9(8).                    CI0063
            12            DA8G-GEEND  PICTURE  9(8).                    CI0063
            12            DA8G-GEMDA  PICTURE  9(8).                    CI0063
            12            DA8G-DNPMT1 PICTURE  9(8).                    CI0063
            12            DA8G-MVSYS  PICTURE  X(6).                    CI0063
            12            DA8G-NAIDC  PICTURE  9(12).                   CI0063
            12            DA8G-CIRMO  PICTURE  X(12).                   CI0063
            12            DA8G-CEBTP  PICTURE  9(2).                    CI0063
            12            DA8G-CLTIN  PICTURE  9(12).                   CI0063
            12            DA8G-CLORN  PICTURE  X(45).                   CI0063
            12            DA8G-GESAD1 PICTURE  X(30).                   CI0063
            12            DA8G-GESAD2 PICTURE  X(30).                   CI0063
            12            DA8G-GESAD3 PICTURE  X(30).                   CI0063
            12            DA8G-GECIT  PICTURE  X(25).                   CI0063
            12            DA8G-GEST   PICTURE  X(8).                    CI0063
            12            DA8G-GEPCD  PICTURE  X(12).                   CI0063
            12            DA8G-CLNMF  PICTURE  X(20).                   CI0063
            12            DA8G-CLNML  PICTURE  X(25).                   CI0063
            12            DA8G-CLTIN1 PICTURE  9(12).                   CI0063
            12            DA8G-FILLER PICTURE  X(05).                   CI0063
            10            DA8G-FILLER PICTURE  X(100)                   CI0063
                          VALUE                SPACE.                   CI0063
       01                 DA8H.                                         CI0063
            10            DA8H-QITEM  PICTURE  9(3)                     CI0063
                          VALUE                ZERO.                    CI0063
            10            DA8H-CTID   PICTURE  X(27)                    CI0063
                          OCCURS       030     TIMES                    CI0063
                          VALUE                SPACE.                   CI0063
            10            DA8H-CRQAS  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            DA8H-FILLER PICTURE  X(299)                   CI0063
                          VALUE                SPACE.                   CI0063
      *!WF DSP=DA DSL=QT SEL=8H FOR=I DES=2 LEV=1                       AM0975

      ******************************************************************
      **  DATE WORK AREA USED WITH MACRO AADA52                        *
      ******************************************************************
      *!WF DSP=DD DSL=DD SEL=01 FOR=I LEV=1 PLT=DD
       01                 DD00.                                         CI0063
          05              DD00-SUITE.                                   CI0063
            15       FILLER         PICTURE  X(00093).                  CI0063
       01                 DD01  REDEFINES      DD00.                    CI0063
            10            DD01-XDAT8.                                   CI0063
            11            DD01-XDATC  PICTURE  XX.                      CI0063
            11            DD01-XDATY  PICTURE  XX.                      CI0063
            11            DD01-XDATM  PICTURE  XX.                      CI0063
            11            DD01-XDATD  PICTURE  XX.                      CI0063
            10            DD01-XDAT8D                                   CI0063
                          REDEFINES            DD01-XDAT8               CI0063
               PICTURE    9(8).                                         CI0063
            10            DD01-XDAT81.                                  CI0063
            11            DD01-XDATM1 PICTURE  XX.                      CI0063
            11            DD01-XDATD1 PICTURE  XX.                      CI0063
            11            DD01-XDATC1 PICTURE  XX.                      CI0063
            11            DD01-XDATY1 PICTURE  XX.                      CI0063
            10            DD01-XDAT80                                   CI0063
                          REDEFINES            DD01-XDAT81              CI0063
               PICTURE    9(8).                                         CI0063
            10            DD01-XDAT62.                                  CI0063
            11            DD01-XDATM2 PICTURE  XX.                      CI0063
            11            DD01-XDATD2 PICTURE  XX.                      CI0063
            11            DD01-XDATY2 PICTURE  XX.                      CI0063
            10            DD01-XDAT69                                   CI0063
                          REDEFINES            DD01-XDAT62              CI0063
               PICTURE    9(6).                                         CI0063
            10            DD01-XDATCU.                                  CI0063
            11            DD01-XDATC9 PICTURE  99.                      CI0063
            11            DD01-XDAYMD.                                  CI0063
            12            DD01-XDATY9 PICTURE  99.                      CI0063
            12            DD01-XDAMD.                                   CI0063
            13            DD01-XDATM9 PICTURE  99.                      CI0063
            13            DD01-XDATD9 PICTURE  99.                      CI0063
            10            DD01-XDAT89 PICTURE  9(8).                    CI0063
            10            DD01-XDAJC  PICTURE  9(7).                    CI0063
            10            DD01-XDAJC1.                                  CI0063
            11            DD01-XDAJC9 PICTURE  99.                      CI0063
            11            DD01-XDAJY  PICTURE  99.                      CI0063
            11            DD01-XDAJN  PICTURE  999.                     CI0063
            10            DD01-XDAB   PICTURE  9(5).                    CI0063
            10            DD01-DD05.                                    CI0063
            11            DD01-XDACT  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            DD01-XDACV  PICTURE  S9                       CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            DD01-XDAGP  PICTURE  S9(9)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            DD01-XDAJP  PICTURE  S9(7)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            DD01-XDACV1 PICTURE  S9                       CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            DD01-XDAGP1 PICTURE  S9(9)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            DD01-XDAJP1 PICTURE  S9(7)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            DD01-XW03.                                    CI0063
            11            DD01-XDATG.                                   CI0063
            12            DD01-XDAT1.                                   CI0063
            13            DD01-XDAT19 PICTURE  99.                      CI0063
            12            DD01-XDAT2.                                   CI0063
            13            DD01-XDAT29 PICTURE  99.                      CI0063
            12            DD01-XDAT3.                                   CI0063
            13            DD01-XDAT39 PICTURE  99.                      CI0063
            12            DD01-XDAT4.                                   CI0063
            13            DD01-XDAT49 PICTURE  99.                      CI0063
            11            DD01-XLEAPY PICTURE  99.                      CI0063
            11            DD01-DTGCY  PICTURE  9(4).                    CI0063
            11            DD01-FILLER                                   CI0063
                          REDEFINES            DD01-DTGCY.              CI0063
            12            DD01-DTGCC  PICTURE  9(2).                    CI0063
            12            DD01-DTGYY  PICTURE  9(2).                    CI0063

      ** DATE WORK AREA
       01  DEL-ER                 PIC 9(1).

       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.

      ******************************************************************ACMCTI
      *WORKING STORAGE SEGMENT FOR STORING THE LINKAGE DATA FOR CI0361. ACMCTI
      ******************************************************************ACMCTI
      *!WF DSP=I9 DSL=K9 SEL=3B FOR=I DES=2 LEV=1                       ACMCTI
       01                 I93B.                                         CI0063
            10            I93B-CEADC  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            I93B-DACTT  PICTURE  X(10)                    CI0063
                          VALUE                SPACE.                   CI0063
            10            I93B-GEOPDC PICTURE  X(8)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            I93B-GEOPDB PICTURE  X(8)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            I93B-CAEMCE PICTURE  X(8)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            I93B-CAEMCD PICTURE  X(8)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            I93B-GETIMM PICTURE  X(8)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            I93B-CRTNC  PICTURE  S9(9)                    CI0063
                          VALUE                ZERO                     CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            I93B-GERTC  PICTURE  X                        CI0063
                          VALUE                SPACE.                   CI0063
            10            I93B-DXTMST PICTURE  X(26)                    CI0063
                          VALUE                SPACE.                   CI0063
            10            I93B-DXTMS2 PICTURE  X(26)                    CI0063
                          VALUE                SPACE.                   CI0063
                                                                        ACMCTI
      ******************************************************************ACMCTI
      *WORKING STORAGE FIELD TO STORE THE NAME OF THE CALLED MODULE.    ACMCTI
      ******************************************************************ACMCTI
       01          CI0361       PIC X(08)                               ACMCTI
                                VALUE 'CI0361P'.                        ACMCTI
                                                                        ACMCTI
      ******************************************************************ACMCTI
      *WORKING STORAGE FIELD TO STORE THE DATE FOR THE DUMMY DB2 CALL.  ACMCTI
      ******************************************************************ACMCTI
       01          WS00-DATE    PIC X(10)     VALUE SPACES.             ACMCTI
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0063
            10            XW05-XW06.                                    CI0063
            11            XW05-XDBPCB.                                  CI0063
            12            XW05-XDBDNM PICTURE  X(08)                    CI0063
                          VALUE                SPACE.                   CI0063
            12            XW05-XSEGLV PICTURE  X(02)                    CI0063
                          VALUE                SPACE.                   CI0063
            12            XW05-XRC    PICTURE  X(02)                    CI0063
                          VALUE                SPACE.                   CI0063
            12            XW05-XPROPT PICTURE  X(04)                    CI0063
                          VALUE                SPACE.                   CI0063
            12            XW05-FILLER PICTURE  S9(5)                    CI0063
                          VALUE                ZERO                     CI0063
                          BINARY.                                       CI0063
            12            XW05-XSEGNM PICTURE  X(08)                    CI0063
                          VALUE                SPACE.                   CI0063
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0063
                          VALUE                ZERO                     CI0063
                          BINARY.                                       CI0063
            12            XW05-XSEGNB PICTURE  9(05)                    CI0063
                          VALUE                ZERO                     CI0063
                          BINARY.                                       CI0063
            12            XW05-XCOKEY PICTURE  X(70)                    CI0063
                          VALUE                SPACE.                   CI0063
            10            XW05-XW07.                                    CI0063
            11            XW05-XIOPCB.                                  CI0063
            12            XW05-XTERMI PICTURE  X(08)                    CI0063
                          VALUE                SPACE.                   CI0063
            12            XW05-FILLER PICTURE  XX                       CI0063
                          VALUE                SPACE.                   CI0063
            12            XW05-XRC1   PICTURE  X(02)                    CI0063
                          VALUE                SPACE.                   CI0063
            12            XW05-FILLER PICTURE  X(12)                    CI0063
                          VALUE                SPACE.                   CI0063
            12            XW05-XMODNM PICTURE  X(8)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0063
                          VALUE                ZERO.                    CI0063
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0063
                          VALUE                ZERO.                    CI0063
            10            XW05-XGU    PICTURE  X(4)                     CI0063
                          VALUE                'GU  '.                  CI0063
            10            XW05-XGHU   PICTURE  X(4)                     CI0063
                          VALUE                'GHU '.                  CI0063
            10            XW05-XGN    PICTURE  X(4)                     CI0063
                          VALUE                'GN  '.                  CI0063
            10            XW05-XGHN   PICTURE  X(4)                     CI0063
                          VALUE                'GHN '.                  CI0063
            10            XW05-XGNP   PICTURE  X(4)                     CI0063
                          VALUE                'GNP '.                  CI0063
            10            XW05-XGHNP  PICTURE  X(4)                     CI0063
                          VALUE                'GHNP'.                  CI0063
            10            XW05-XREPL  PICTURE  XXXX                     CI0063
                          VALUE                'REPL'.                  CI0063
            10            XW05-XISRT  PICTURE  X(4)                     CI0063
                          VALUE                'ISRT'.                  CI0063
            10            XW05-XDLET  PICTURE  X(4)                     CI0063
                          VALUE                'DLET'.                  CI0063
            10            XW05-XOPEN  PICTURE  X(4)                     CI0063
                          VALUE                'OPEN'.                  CI0063
            10            XW05-XCLSE  PICTURE  X(4)                     CI0063
                          VALUE                'CLSE'.                  CI0063
            10            XW05-XCHKP  PICTURE  X(4)                     CI0063
                          VALUE                'CHKP'.                  CI0063
            10            XW05-XXRST  PICTURE  X(4)                     CI0063
                          VALUE                'XRST'.                  CI0063
            10            XW05-XTERM  PICTURE  X(4)                     CI0063
                          VALUE                'TERM'.                  CI0063
            10            XW05-XNFPAC PICTURE  X(13)                    CI0063
                          VALUE                SPACE.                   CI0063
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0063
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0063
       01                 FT01.                                         CI0063
            10            FT01-CX01K.                                   CI0063
            11            FT01-C199.                                    CI0063
            12            FT01-CLID.                                    CI0063
            13            FT01-CLIDO  PICTURE  9(3).                    CI0063
            13            FT01-CLIDN.                                   CI0063
            14            FT01-CLIDNP PICTURE  X(12).                   CI0063
            14            FT01-CLIDND PICTURE  9(8).                    CI0063
            10            FT01-GEMDA  PICTURE  9(8).                    CI0063
            10            FT01-NSEQ4B PICTURE  9(8)                     CI0063
                          BINARY.                                       CI0063
            10            FT01-FILLER PICTURE  X(5).                    CI0063
       01                 FT03.                                         CI0063
            10            FT03-GELL   PICTURE  9(4)                     CI0063
                          BINARY.                                       CI0063
            10            FT03-CY00.                                    CI0063
            11            FT03-CX03K.                                   CI0063
            12            FT03-CARTY  PICTURE  99.                      CI0063
            12            FT03-NARRS  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            FT03-CARST  PICTURE  99.                      CI0063
            11            FT03-GECSQ  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            FT03-CPMTG  PICTURE  99.                      CI0063
            11            FT03-GRCRNG PICTURE  9(3).                    CI0063
            11            FT03-DEXDT  PICTURE  9(8).                    CI0063
            11            FT03-DASUP  PICTURE  9(8).                    CI0063
            11            FT03-CSTEC  PICTURE  X(3).                    CI0063
            11            FT03-FILLER PICTURE  X(17).                   CI0063
            11            FT03-CY50.                                    CI0063
            12            FT03-NARID  PICTURE  X(30).                   CI0063
            11            FT03-CY51                                     CI0063
                          REDEFINES            FT03-CY50.               CI0063
            12            FT03-NDIDN  PICTURE  9(12).                   CI0063
            12            FT03-FILLER PICTURE  X(18).                   CI0063
            11            FT03-CY52                                     CI0063
                          REDEFINES            FT03-CY50.               CI0063
            12            FT03-NAIDC  PICTURE  9(12).                   CI0063
            12            FT03-FILLER PICTURE  X(18).                   CI0063
            11            FT03-CY53                                     CI0063
                          REDEFINES            FT03-CY50.               CI0063
            12            FT03-NAMEXB PICTURE  9(15).                   CI0063
            12            FT03-FILLER PICTURE  X(15).                   CI0063
            10            FT03-CY99.                                    CI0063
            11            FT03-FILLER PICTURE  X(109).                  CI0063
            10            FT03-CY01                                     CI0063
                          REDEFINES            FT03-CY99.               CI0063
            11            FT03-NBASQ  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            FT03-ICPCI  PICTURE  X.                       CI0063
            11            FT03-CLUPD  PICTURE  9(3).                    CI0063
            11            FT03-DLAUP  PICTURE  9(8).                    CI0063
            11            FT03-CWRC   PICTURE  99.                      CI0063
            11            FT03-CHCR   PICTURE  99.                      CI0063
            11            FT03-GEOPD2 PICTURE  X(8).                    CI0063
            11            FT03-GEAUN  PICTURE  9(5).                    CI0063
            11            FT03-DPCHD  PICTURE  9(8).                    CI0063
            11            FT03-DLRCHK PICTURE  9(8).                    CI0063
            11            FT03-QTRCHK PICTURE  9(2).                    CI0063
            11            FT03-DNPMT  PICTURE  9(8).                    CI0063
            11            FT03-APMTLA PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            FT03-CY02                                     CI0063
                          REDEFINES            FT03-CY99.               CI0063
            11            FT03-QSIRQ  PICTURE  99.                      CI0063
            11            FT03-QDRMN  PICTURE  9(2)                     CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            FT03-DDPRE  PICTURE  9(8).                    CI0063
            11            FT03-DDSHP  PICTURE  9(8).                    CI0063
            11            FT03-NDRFTB PICTURE  9(5).                    CI0063
            11            FT03-QDIPBJ PICTURE  9(3).                    CI0063
            11            FT03-DDSHPA PICTURE  9(8).                    CI0063
            11            FT03-NDRFTF PICTURE  9(5).                    CI0063
            11            FT03-QDIPBK PICTURE  9(3).                    CI0063
            11            FT03-CREOR  PICTURE  X(1).                    CI0063
            11            FT03-CREOR1 PICTURE  X(1).                    CI0063
            11            FT03-DDASC  PICTURE  9(8).                    CI0063
            11            FT03-FILLER PICTURE  X(7).                    CI0063
            10            FT03-CY03                                     CI0063
                          REDEFINES            FT03-CY99.               CI0063
            11            FT03-DLAUP1 PICTURE  9(8).                    CI0063
            11            FT03-GEOPD3 PICTURE  X(8).                    CI0063
            11            FT03-DNPMT1 PICTURE  9(8).                    CI0063
            11            FT03-DOPDA  PICTURE  99.                      CI0063
            11            FT03-CPMTF  PICTURE  99.                      CI0063
            11            FT03-CIRMO  PICTURE  X(12).                   CI0063
            11            FT03-CPALL  PICTURE  X(1).                    CI0063
            11            FT03-CCOLM  PICTURE  9(2).                    CI0063
            11            FT03-CBLTP  PICTURE  X(1).                    CI0063
            11            FT03-CASUB  PICTURE  9(2).                    CI0063
            11            FT03-CBLFM  PICTURE  9(2).                    CI0063
            11            FT03-IBILS  PICTURE  X.                       CI0063
            11            FT03-IPAOS  PICTURE  X.                       CI0063
            11            FT03-CBLSQ  PICTURE  X(4).                    CI0063
            11            FT03-DLBPD  PICTURE  9(8).                    CI0063
            11            FT03-DNBPD  PICTURE  9(8).                    CI0063
            11            FT03-DODBD  PICTURE  9(8).                    CI0063
            11            FT03-CPSRE  PICTURE  99.                      CI0063
            11            FT03-ISPHN  PICTURE  X.                       CI0063
            11            FT03-TCARR  PICTURE  X(6).                    CI0063
            11            FT03-CBKPT  PICTURE  9(2).                    CI0063
            11            FT03-IECNT  PICTURE  X.                       CI0063
            11            FT03-ICONV  PICTURE  X(1).                    CI0063
            11            FT03-FILLER PICTURE  X(4).                    CI0063
            10            FT03-CY04                                     CI0063
                          REDEFINES            FT03-CY99.               CI0063
            11            FT03-CCARD  PICTURE  X(02).                   CI0063
            11            FT03-MCSIG4 PICTURE  X(20).                   CI0063
            11            FT03-IREMT  PICTURE  X(01).                   CI0063
            11            FT03-ISBILA PICTURE  X.                       CI0063
            11            FT03-DLBPDA PICTURE  9(8).                    CI0063
            11            FT03-DNBPDA.                                  CI0063
            12            FT03-DNCYM  PICTURE  9(6).                    CI0063
            12            FT03-CEDTD  PICTURE  9(2).                    CI0063
            11            FT03-AREMT  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            FT03-DREMT  PICTURE  9(8).                    CI0063
            11            FT03-ADBRQ  PICTURE  S9(11)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            FT03-CLUPD1 PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            FT03-DLAUP3 PICTURE  9(8).                    CI0063
            11            FT03-CWRC2  PICTURE  99.                      CI0063
            11            FT03-CHCR2  PICTURE  99.                      CI0063
            11            FT03-GEOPD9 PICTURE  X(8).                    CI0063
            11            FT03-GEAUN1 PICTURE  9(5).                    CI0063
            11            FT03-DPCHD1 PICTURE  9(8).                    CI0063
       01                 FT06.                                         CI0063
            10            FT06-CX06K.                                   CI0063
            11            FT06-C299.                                    CI0063
            12            FT06-CTID.                                    CI0063
            13            FT06-CTIDA  PICTURE  9(3).                    CI0063
            13            FT06-CTIDN.                                   CI0063
            14            FT06-CTIDNP PICTURE  X(13).                   CI0063
            14            FT06-CTIDND PICTURE  9(11).                   CI0063
            10            FT06-NPECK  PICTURE  9(02).                   CI0063
            10            FT06-FILLER PICTURE  X.                       CI0063
       01                 FT12.                                         CI0063
            10            FT12-CX12K.                                   CI0063
            11            FT12-CPMTC  PICTURE  99.                      CI0063
            11            FT12-NAPDS  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            FT12-GESTD  PICTURE  9(8).                    CI0063
            10            FT12-GEEND  PICTURE  9(8).                    CI0063
            10            FT12-CIRMO  PICTURE  X(12).                   CI0063
            10            FT12-CDEST  PICTURE  99.                      CI0063
            10            FT12-APMTL  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            FT12-DNPMT  PICTURE  9(8).                    CI0063
            10            FT12-NIRACM PICTURE  9(2).                    CI0063
            10            FT12-CPMTF  PICTURE  99.                      CI0063
            10            FT12-IPODM  PICTURE  X.                       CI0063
            10            FT12-CLUPD  PICTURE  9(3).                    CI0063
            10            FT12-DLAUP  PICTURE  9(8).                    CI0063
            10            FT12-CWRC   PICTURE  99.                      CI0063
            10            FT12-CHCR   PICTURE  99.                      CI0063
            10            FT12-GEOPD2 PICTURE  X(8).                    CI0063
            10            FT12-GEAUN  PICTURE  9(5).                    CI0063
            10            FT12-DPCHD  PICTURE  9(8).                    CI0063
            10            FT12-DNEXE  PICTURE  9(8).                    CI0063
            10            FT12-CCSMQ  PICTURE  X.                       CI0063
            10            FT12-GCUSPZ PICTURE  X(12).                   CI0063
            10            FT12-CORTY  PICTURE  X.                       CI0063
            10            FT12-CNAVR  PICTURE  X(1).                    CI0063
            10            FT12-DELOI3 PICTURE  9(6).                    CI0063
            10            FT12-ALOIDD PICTURE  9(9)V99                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            FT12-FILLER PICTURE  X(5).                    CI0063
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
      *                                                                 AM0020
      ******************************************************************AM0020
      **     SEGMENT THAT CONTAINS THE CAMS ACCOUNTING DATES           *AM0020
      ******************************************************************AM0020
      *                                                                 AM0020
      *!WF DSP=NS DSL=NS SEL=20 FOR=I LEV=1                             AM0020
       01                 NS00.                                         CI0063
          05              NS00-00.                                      CI0063
            10            NS00-NS00K.                                   CI0063
            11            NS00-PRCSTK PICTURE  XX.                      CI0063
          05              NS00-SUITE.                                   CI0063
            15       FILLER         PICTURE  X(00078).                  CI0063
       01                 NS20  REDEFINES      NS00.                    CI0063
            10       FILLER         PICTURE  X(00002).                  CI0063
            10            NS20-DCACG  PICTURE  9(8).                    CI0063
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            NS20-CCDAT  PICTURE  X(8).                    CI0063
            10            NS20-DCALP  PICTURE  X(12).                   CI0063
            10            NS20-DNACG  PICTURE  9(8).                    CI0063
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            NS20-CNDAT  PICTURE  X(8).                    CI0063
            10            NS20-DNALP  PICTURE  X(12).                   CI0063
            10            NS20-DCACD  PICTURE  X(10).                   CI0063
            10            NS20-FILLER PICTURE  X(4).                    CI0063
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
                                                                        AM0018
      ******************************************************************AM0018
      **     PCB ADDRESS LIST FOR CI0018.  MODULE CI0018 WILL NEED     *AM0018
      **     PCB'S FOR:                                                *AM0018
      **                CONTRACT DATABASE(CT1P)                        *AM0018
      ******************************************************************AM0018
                                                                        AM0018
       01  CI0018C-PCB-ADDRESS-LIST.                                    AM0018
           05  CI0018C-PCB-CT1P-PTR1      POINTER.                      AM0018
      ******************************************************************ADUTAB
      **              TABLE SA72 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-SA72-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=SA DSL=TA SEL=72 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-SA72.                                                CI0063
           04    G-SA72-PARAM.                                          CI0063
             10  G-SA72-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0063
                        VALUE      +041.                                CI0063
             10  G-SA72-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0063
                        VALUE      +001.                                CI0063
             10  G-SA72-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0063
                        VALUE      +003.                                CI0063
             10  G-SA72-NUAPP  PICTURE 99                               CI0063
                        VALUE       0.                                  CI0063
             10  G-SA72-NUTAB  PICTURE X(6)                             CI0063
                        VALUE 'CLUPD '.                                 CI0063
             10  G-SA72-TABFO  PICTURE XX                 VALUE SPACE.  CI0063
             10  G-SA72-TABCR  PICTURE XX                 VALUE SPACE.  CI0063
             10  G-SA72-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0063
             10  G-SA72-NUSSC  PICTURE X  VALUE   ' '.                  CI0063
             10  G-SA72-NUSSY  PICTURE X                  VALUE SPACE.  CI0063
             10  G-SA72-TRANID PICTURE X(4)               VALUE SPACE.  CI0063
             10  G-SA72-FILSYS.                                         CI0063
             15  G-SA72-USERC  PICTURE X(6)               VALUE SPACE.  CI0063
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0063
           04             SA72.                                         CI0063
            10            SA72-CLUPD  PICTURE  9(3)                     CI0063
                          VALUE                ZERO.                    CI0063
            10            SA72-MLUPD  PICTURE  X(8)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            SA72-TLUPD  PICTURE  X(30)                    CI0063
                          VALUE                SPACE.                   CI0063
      **                                                                ADUTAB
      *
       01  W-SW00-AREAS.
      *
      *- LIFE SWITCHES TO IDENTIFY THE DIFFERENT TYPES
      *
      *!WI
           05  W-UL-XSW1
                        PICTURE X.                                      CI0063
      *!WI
           05  W-LIFE-TR-XSW1
                        PICTURE X.                                      CI0063
      *!WI
           05  W-TSA-XSW1
                        PICTURE X.                                      CI0063
      *
      *- INDICATES IF A CERT SURRENDER EXISTS
      *
      *!WI
           05  W-SURR-XSW1
                        PICTURE X.                                      CI0063
      *
      *- INDICATES IF A BA IS ALLOWED BASED ON SD EXISTING
      *
           05  W-ALLOW-BA              PIC X(01).
      *
      *- MISCELLANEOUS CF SWITCHES USED FOR SEGMENT ACCESS
      *
           05  AA10-CF                 PIC X(01).
           05  AA20-CF                 PIC X(01).
           05  AA85-CF                 PIC X(01).
      *
      *- INDICATES THAT AN ACTIVE GROUP BILL WAS FOUND
      *
           05  W-GROUP-BILL            PIC X(01).
      ******************************************************************ADUTAB
      **              TABLE TA71 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA71-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=71 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA71.                                                CI0063
           04    G-TA71-PARAM.                                          CI0063
             10  G-TA71-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0063
                        VALUE      +042.                                CI0063
             10  G-TA71-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0063
                        VALUE      +001.                                CI0063
             10  G-TA71-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0063
                        VALUE      +002.                                CI0063
             10  G-TA71-NUAPP  PICTURE 99                               CI0063
                        VALUE       0.                                  CI0063
             10  G-TA71-NUTAB  PICTURE X(6)                             CI0063
                        VALUE 'CPMTF '.                                 CI0063
             10  G-TA71-TABFO  PICTURE XX                 VALUE SPACE.  CI0063
             10  G-TA71-TABCR  PICTURE XX                 VALUE SPACE.  CI0063
             10  G-TA71-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0063
             10  G-TA71-NUSSC  PICTURE X  VALUE   ' '.                  CI0063
             10  G-TA71-NUSSY  PICTURE X                  VALUE SPACE.  CI0063
             10  G-TA71-TRANID PICTURE X(4)               VALUE SPACE.  CI0063
             10  G-TA71-FILSYS.                                         CI0063
             15  G-TA71-USERC  PICTURE X(6)               VALUE SPACE.  CI0063
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0063
           04             TA71.                                         CI0063
            10            TA71-CPMTF  PICTURE  99                       CI0063
                          VALUE                ZERO.                    CI0063
            10            TA71-MPMTF  PICTURE  X(14)                    CI0063
                          VALUE                SPACE.                   CI0063
            10            TA71-CPMTFA PICTURE  X(2)                     CI0063
                          VALUE                SPACE.                   CI0063
            10            TA71-MPMTFL PICTURE  X(24)                    CI0063
                          VALUE                SPACE.                   CI0063
      **                                                                ADUTAB
      *
       01  W-WK00-AREAS.
      **
      ** THIS FIELD IS USED TO BREAK DOWN THE DATE OF NEXT PAYMENT
      ** SO THAT THE MONTH AND YEAR CAN BE MANIPULATED.
      **
      *!WE
           05  W-FRMT-DNPMT
                        PICTURE 9(8).                                   CI0063
           05  FILLER REDEFINES W-FRMT-DNPMT.
               10  W-FRMT-CCYY      PIC 9(04).
               10  FILLER           PIC X(04).
      **
      ** THIS FIELD IS USED TO STORE THE DAY OF THE MONTH THE PAYMENT
      ** IS TO BE APPLIED
      **
      *!WI
           05  W-WK00-NDAY
                        PICTURE 99.                                     CI0063
      **
      ** THIS FIELD IS USED TO STORE THE NUMBER OF MONTH THAT ARE
      ** MISSING IN CIRMO FOR IRREGULAR SCHEDULES.
      **
           05  W-WK00-COUNT         PIC 9(02).
      **
      ** THIS AREA WILL BE USED TO ENSURE EACH POSITION OF CIRMO HAS
      ** THE PROPER VALUE IN IT.
      **
           05  W-WK00-CIRMO.
               10  FILLER           PIC X(01).
                   88  JAN                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  FEB                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  MAR                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  APR                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  MAY                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  JUN                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  JUL                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  AUG                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  SEP                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  OCT                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  NOV                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  DEC                     VALUE ' ', 'X'.
      **
      ** THIS AREA IS USED TO BREAK DOWN THE SUB PRODUCT TO IDENTIFY
      ** SPECIAL CODES USED BY LIFE.
      **
           05  W-WK00-PRSCD.
               10  FILLER            PIC X(02).
               10  W-FGHIJKL-CODE.
                   15  FILLER        PIC X(03).
                   15  W-WK00-I-CODE PIC X(01).
                   15  FILLER        PIC X(03).
      *
      ** - USED TO BREAK MONTH OFF OF MMDD IRA CODE ON CONTRACT
      *
      *!WI
           05  W-WK00-DIRAC
                        PICTURE 9(4).                                   CI0063
           05  FILLER REDEFINES W-WK00-DIRAC.
      *!WI
               10  W-WK00-NIRACM
                        PICTURE 9(2).                                   CI0063
               10  FILLER         PIC X(02).
      **
      ** THESE FIELDS WILL BE USED TO INDICATE IF THE ALLOWABLE
      ** ARRANGEMENTS ARE EXISTING OR NOT
      **
      *!WI
           05  W-WK99-NAPDS
                        PICTURE S9(3)                                   CI0063
                          COMPUTATIONAL-3.                              CI0063
      **
      ** AREA USED TO SUMMARIZE ARRANGEMENTS ALREADY STORED ON THE
      ** THE ARRANGEMENT DATABASE
      **
      *!WI
           05  W-SD-XCOUNT
                        PICTURE S9(5).                                  CI0063
      *!WI
           05  W-LOAN-XCOUNT
                        PICTURE S9(5).                                  CI0063
      *!WI
           05  W-REGL-XCOUNT
                        PICTURE S9(5).                                  CI0063
      **
      ** AREA USED TO STORE THE NEXT TO THE FARTHEST KEY.  THIS WILL
      ** BE OF USE FOR EDITTING WHEN A DELETE IS BEING PROCESSED.
      **
      *!WI
           05  W-PREV-CPMTCX
                        PICTURE XX.                                     CI0063
      *!WI
           05  W-PREV-NAPDSK
                        PICTURE S9(3)                                   CI0063
                          COMPUTATIONAL-3.                              CI0063
      *!WI
           05  W-PREV-GESTD1
                        PICTURE 9(8).                                   CI0063
      *!WI
           05  W-PREV-DNPMT1
                        PICTURE 9(8).                                   CI0063

       01               7-WS-ABEND       PIC 9(4)  VALUE ZERO.
       01               7-WS-ABENDX      REDEFINES 7-WS-ABEND.
           05           7-WS-FIRST       PIC X.
           05           FILLER           PIC X(3).
      *!WI
       01  WS-IAIND    VALUE SPACES
                        PICTURE X.                                      CI0063
      *WORKING STORAGE TABLE FOR STORING CX12-NAPDS.
       01                 WS00-TABLE.
          05              WS00    OCCURS 97 TIMES.
      *!WI
             10           WS00-NAPDS
                        PICTURE S9(3)                                   CI0063
                          COMPUTATIONAL-3.                              CI0063
       01               WS00-COUNT       PIC 9(4)  VALUE ZERO.
      *WORKING STORAGE FOR MARKET CLOSE TIME
      *!WI
       01  WS-GETIMM
                        PICTURE X(8).                                   CI0063
       01  WS-GETIMM-RED REDEFINES WS-GETIMM.
           05  WS-HH      PIC XX.
           05  WS-DOT1    PIC X.
           05  WS-MM      PIC XX.
           05  WS-DOT2    PIC X.
           05  WS-SS      PIC XX.
      *!WI
       01  WS-GETIM6
                        PICTURE 9(06).                                  CI0063
       01  WS-MKT-TIME REDEFINES WS-GETIM6.
           05  WS-MKT-HH  PIC 99.
           05  WS-MKT-MM  PIC 99.
           05  WS-MKT-SS  PIC 99.
      **** B-SHARES TO BE CLOSED******************************
      *!WI
       01  WS00-PRCOD
                        PICTURE 9(5).                                   CI0063
           88 CLOSE-B-SHARE VALUE 24, 42, 67, 102, 106, 107.
       01                 WX03.                                         CI0063
            10            WX03-GELL   PICTURE  9(4)                     CI0063
                          BINARY.                                       CI0063
            10            WX03-CY00.                                    CI0063
            11            WX03-CX03K.                                   CI0063
            12            WX03-CARTY  PICTURE  99.                      CI0063
            12            WX03-NARRS  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WX03-CARST  PICTURE  99.                      CI0063
            11            WX03-GECSQ  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WX03-CPMTG  PICTURE  99.                      CI0063
            11            WX03-GRCRNG PICTURE  9(3).                    CI0063
            11            WX03-DEXDT  PICTURE  9(8).                    CI0063
            11            WX03-DASUP  PICTURE  9(8).                    CI0063
            11            WX03-CSTEC  PICTURE  X(3).                    CI0063
            11            WX03-FILLER PICTURE  X(17).                   CI0063
            11            WX03-CY50.                                    CI0063
            12            WX03-NARID  PICTURE  X(30).                   CI0063
            11            WX03-CY51                                     CI0063
                          REDEFINES            WX03-CY50.               CI0063
            12            WX03-NDIDN  PICTURE  9(12).                   CI0063
            12            WX03-FILLER PICTURE  X(18).                   CI0063
            11            WX03-CY52                                     CI0063
                          REDEFINES            WX03-CY50.               CI0063
            12            WX03-NAIDC  PICTURE  9(12).                   CI0063
            12            WX03-FILLER PICTURE  X(18).                   CI0063
            11            WX03-CY53                                     CI0063
                          REDEFINES            WX03-CY50.               CI0063
            12            WX03-NAMEXB PICTURE  9(15).                   CI0063
            12            WX03-FILLER PICTURE  X(15).                   CI0063
            10            WX03-CY99.                                    CI0063
            11            WX03-FILLER PICTURE  X(109).                  CI0063
            10            WX03-CY01                                     CI0063
                          REDEFINES            WX03-CY99.               CI0063
            11            WX03-NBASQ  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WX03-ICPCI  PICTURE  X.                       CI0063
            11            WX03-CLUPD  PICTURE  9(3).                    CI0063
            11            WX03-DLAUP  PICTURE  9(8).                    CI0063
            11            WX03-CWRC   PICTURE  99.                      CI0063
            11            WX03-CHCR   PICTURE  99.                      CI0063
            11            WX03-GEOPD2 PICTURE  X(8).                    CI0063
            11            WX03-GEAUN  PICTURE  9(5).                    CI0063
            11            WX03-DPCHD  PICTURE  9(8).                    CI0063
            11            WX03-DLRCHK PICTURE  9(8).                    CI0063
            11            WX03-QTRCHK PICTURE  9(2).                    CI0063
            11            WX03-DNPMT  PICTURE  9(8).                    CI0063
            11            WX03-APMTLA PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            WX03-CY02                                     CI0063
                          REDEFINES            WX03-CY99.               CI0063
            11            WX03-QSIRQ  PICTURE  99.                      CI0063
            11            WX03-QDRMN  PICTURE  9(2)                     CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WX03-DDPRE  PICTURE  9(8).                    CI0063
            11            WX03-DDSHP  PICTURE  9(8).                    CI0063
            11            WX03-NDRFTB PICTURE  9(5).                    CI0063
            11            WX03-QDIPBJ PICTURE  9(3).                    CI0063
            11            WX03-DDSHPA PICTURE  9(8).                    CI0063
            11            WX03-NDRFTF PICTURE  9(5).                    CI0063
            11            WX03-QDIPBK PICTURE  9(3).                    CI0063
            11            WX03-CREOR  PICTURE  X(1).                    CI0063
            11            WX03-CREOR1 PICTURE  X(1).                    CI0063
            11            WX03-DDASC  PICTURE  9(8).                    CI0063
            11            WX03-FILLER PICTURE  X(7).                    CI0063
            10            WX03-CY03                                     CI0063
                          REDEFINES            WX03-CY99.               CI0063
            11            WX03-DLAUP1 PICTURE  9(8).                    CI0063
            11            WX03-GEOPD3 PICTURE  X(8).                    CI0063
            11            WX03-DNPMT1 PICTURE  9(8).                    CI0063
            11            WX03-DOPDA  PICTURE  99.                      CI0063
            11            WX03-CPMTF  PICTURE  99.                      CI0063
            11            WX03-CIRMO  PICTURE  X(12).                   CI0063
            11            WX03-CPALL  PICTURE  X(1).                    CI0063
            11            WX03-CCOLM  PICTURE  9(2).                    CI0063
            11            WX03-CBLTP  PICTURE  X(1).                    CI0063
            11            WX03-CASUB  PICTURE  9(2).                    CI0063
            11            WX03-CBLFM  PICTURE  9(2).                    CI0063
            11            WX03-IBILS  PICTURE  X.                       CI0063
            11            WX03-IPAOS  PICTURE  X.                       CI0063
            11            WX03-CBLSQ  PICTURE  X(4).                    CI0063
            11            WX03-DLBPD  PICTURE  9(8).                    CI0063
            11            WX03-DNBPD  PICTURE  9(8).                    CI0063
            11            WX03-DODBD  PICTURE  9(8).                    CI0063
            11            WX03-CPSRE  PICTURE  99.                      CI0063
            11            WX03-ISPHN  PICTURE  X.                       CI0063
            11            WX03-TCARR  PICTURE  X(6).                    CI0063
            11            WX03-CBKPT  PICTURE  9(2).                    CI0063
            11            WX03-IECNT  PICTURE  X.                       CI0063
            11            WX03-ICONV  PICTURE  X(1).                    CI0063
            11            WX03-FILLER PICTURE  X(4).                    CI0063
            10            WX03-CY04                                     CI0063
                          REDEFINES            WX03-CY99.               CI0063
            11            WX03-CCARD  PICTURE  X(02).                   CI0063
            11            WX03-MCSIG4 PICTURE  X(20).                   CI0063
            11            WX03-IREMT  PICTURE  X(01).                   CI0063
            11            WX03-ISBILA PICTURE  X.                       CI0063
            11            WX03-DLBPDA PICTURE  9(8).                    CI0063
            11            WX03-DNBPDA.                                  CI0063
            12            WX03-DNCYM  PICTURE  9(6).                    CI0063
            12            WX03-CEDTD  PICTURE  9(2).                    CI0063
            11            WX03-AREMT  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WX03-DREMT  PICTURE  9(8).                    CI0063
            11            WX03-ADBRQ  PICTURE  S9(11)V99                CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WX03-CLUPD1 PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WX03-DLAUP3 PICTURE  9(8).                    CI0063
            11            WX03-CWRC2  PICTURE  99.                      CI0063
            11            WX03-CHCR2  PICTURE  99.                      CI0063
            11            WX03-GEOPD9 PICTURE  X(8).                    CI0063
            11            WX03-GEAUN1 PICTURE  9(5).                    CI0063
            11            WX03-DPCHD1 PICTURE  9(8).                    CI0063
       01                 WX06.                                         CI0063
            10            WX06-CX06K.                                   CI0063
            11            WX06-C299.                                    CI0063
            12            WX06-CTID.                                    CI0063
            13            WX06-CTIDA  PICTURE  9(3).                    CI0063
            13            WX06-CTIDN.                                   CI0063
            14            WX06-CTIDNP PICTURE  X(13).                   CI0063
            14            WX06-CTIDND PICTURE  9(11).                   CI0063
            10            WX06-NPECK  PICTURE  9(02).                   CI0063
            10            WX06-FILLER PICTURE  X.                       CI0063
       01                 WX12.                                         CI0063
            10            WX12-CX12K.                                   CI0063
            11            WX12-CPMTC  PICTURE  99.                      CI0063
            11            WX12-NAPDS  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WX12-GESTD  PICTURE  9(8).                    CI0063
            10            WX12-GEEND  PICTURE  9(8).                    CI0063
            10            WX12-CIRMO  PICTURE  X(12).                   CI0063
            10            WX12-CDEST  PICTURE  99.                      CI0063
            10            WX12-APMTL  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            WX12-DNPMT  PICTURE  9(8).                    CI0063
            10            WX12-NIRACM PICTURE  9(2).                    CI0063
            10            WX12-CPMTF  PICTURE  99.                      CI0063
            10            WX12-IPODM  PICTURE  X.                       CI0063
            10            WX12-CLUPD  PICTURE  9(3).                    CI0063
            10            WX12-DLAUP  PICTURE  9(8).                    CI0063
            10            WX12-CWRC   PICTURE  99.                      CI0063
            10            WX12-CHCR   PICTURE  99.                      CI0063
            10            WX12-GEOPD2 PICTURE  X(8).                    CI0063
            10            WX12-GEAUN  PICTURE  9(5).                    CI0063
            10            WX12-DPCHD  PICTURE  9(8).                    CI0063
            10            WX12-DNEXE  PICTURE  9(8).                    CI0063
            10            WX12-CCSMQ  PICTURE  X.                       CI0063
            10            WX12-GCUSPZ PICTURE  X(12).                   CI0063
            10            WX12-CORTY  PICTURE  X.                       CI0063
            10            WX12-CNAVR  PICTURE  X(1).                    CI0063
            10            WX12-DELOI3 PICTURE  9(6).                    CI0063
            10            WX12-ALOIDD PICTURE  9(9)V99                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            WX12-FILLER PICTURE  X(5).                    CI0063
       01   DEBUT-WSS.                                                  CI0063
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0063
            05   IK     PICTURE X.                                      CI0063
       01  CONSTANTES-PAC.                                              CI0063
           05  FILLER  PICTURE X(87)   VALUE                            CI0063
                     '6015 CAT09/08/14CI0063ADMIN   14:34:25CI0063P AMERCI0063
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0063
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0063
           05  NUGNA   PICTURE X(5).                                    CI0063
           05  APPLI   PICTURE X(3).                                    CI0063
           05  DATGN   PICTURE X(8).                                    CI0063
           05  PROGR   PICTURE X(6).                                    CI0063
           05  CODUTI  PICTURE X(8).                                    CI0063
           05  TIMGN   PICTURE X(8).                                    CI0063
           05  PROGE   PICTURE X(8).                                    CI0063
           05  COBASE  PICTURE X(4).                                    CI0063
           05  DATGNC  PICTURE X(10).                                   CI0063
           05  RELEAS  PICTURE X(7).                                    CI0063
           05  DATGE   PICTURE X(10).                                   CI0063
           05  DATSQ   PICTURE X(10).                                   CI0063
       01  DATCE.                                                       CI0063
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0063
         05  DATOR.                                                     CI0063
           10  DATOA  PICTURE XX.                                       CI0063
           10  DATOM  PICTURE XX.                                       CI0063
           10  DATOJ  PICTURE XX.                                       CI0063
       01   VARIABLES-CONDITIONNELLES.                                  CI0063
            05                  FT      PICTURE X VALUE '0'.            CI0063
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0063
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0063
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           IWS00L PICTURE S9(4) VALUE  ZERO.
            05           IWS00R PICTURE S9(4) VALUE  ZERO.
            05           IWS00M PICTURE S9(4) VALUE +0097.
            05           J35HTR PICTURE S9(4) VALUE  ZERO.
            05           J40CMR PICTURE S9(4) VALUE  ZERO.
            05           J40COR PICTURE S9(4) VALUE  ZERO.
            05           J40CWR PICTURE S9(4) VALUE  ZERO.
            05           J40EHR PICTURE S9(4) VALUE  ZERO.
            05           J40FGR PICTURE S9(4) VALUE  ZERO.
            05           J45EMR PICTURE S9(4) VALUE  ZERO.
            05           J57BMR PICTURE S9(4) VALUE  ZERO.
            05           J72EAR PICTURE S9(4) VALUE  ZERO.
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0063
            05       5-AA00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0063
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0063
            05       5-CM00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0063
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0063
            05       5-FT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0063
            05       5-WX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0063
       01               S-AA10-SSA.                                     CI0063
            10         S1-AA10-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'LMSPCON '.                 CI0063
            10         S1-AA10-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-AA10-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01            S-AAU10-SSA.                                       CI0063
            11      S1-AAU10-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'LMSPCON '.                 CI0063
            11      S1-AAU10-CCOM   PICTURE X VALUE '*'.                CI0063
            11       S-AAU10-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            11      S1-AAU10-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(LMSPCONK'.                CI0063
            11       S-AAU10-OPER  PICTURE XX VALUE ' ='.               CI0063
            11       S-AAU10-ALCIDN   PICTURE  9(11).                   CI0063
            11  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01               S-AA20-SSA.                                     CI0063
            10         S1-AA20-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'LMSPLON '.                 CI0063
            10         S1-AA20-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-AA20-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01               S-AA25-SSA.                                     CI0063
            10         S1-AA25-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'LMSPANA '.                 CI0063
            10         S1-AA25-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-AA25-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01               S-AA66-SSA.                                     CI0063
            10         S1-AA66-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'LMSPTLN '.                 CI0063
            10         S1-AA66-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-AA66-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01               S-AA85-SSA.                                     CI0063
            10         S1-AA85-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'LMSPUWG '.                 CI0063
            10         S1-AA85-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-AA85-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01               S-CL01-SSA.                                     CI0063
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'CL01    '.                 CI0063
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-CL01-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01            S-CLU01-SSA.                                       CI0063
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CL01    '.                 CI0063
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0063
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CL01K'.                   CI0063
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0063
            10       S-CLU01-CL01K.                                     CI0063
            11       S-CLU01-C199.                                      CI0063
            12       S-CLU01-CLID.                                      CI0063
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0063
            13       S-CLU01-CLIDN.                                     CI0063
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0063
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0063
            10  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01               S-CL03-SSA.                                     CI0063
            10         S1-CL03-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'CL03    '.                 CI0063
            10         S1-CL03-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-CL03-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01            S-CLA03-SSA.                                       CI0063
            10      S1-CLA03-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CL03    '.                 CI0063
            10      S1-CLA03-CCOM   PICTURE X VALUE '*'.                CI0063
            10       S-CLA03-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            10      S1-CLA03-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CLDOD'.                   CI0063
            10       S-CLA03-OPER  PICTURE XX VALUE ' ='.               CI0063
            10       S-CLA03-CLDOD    PICTURE  9(8).                    CI0063
            10  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01               S-CM01-SSA.                                     CI0063
            10         S1-CM01-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'CACPCNT'.                  CI0063
            10         S1-CM01-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-CM01-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01            S-CMA01-SSA.                                       CI0063
            10      S1-CMA01-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CACPCNT'.                  CI0063
            10      S1-CMA01-CCOM   PICTURE X VALUE '*'.                CI0063
            10       S-CMA01-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            10      S1-CMA01-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CACPPRDK'.                CI0063
            10       S-CMA01-OPER  PICTURE XX VALUE ' ='.               CI0063
            10       S-CMA01-CEPDT    PICTURE  9(4).                    CI0063
            10  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CMU01-SSA.                                       CI0063
            10      S1-CMU01-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CACPCNT'.                  CI0063
            10      S1-CMU01-CCOM   PICTURE X VALUE '*'.                CI0063
            10       S-CMU01-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            10      S1-CMU01-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CACPCNTK'.                CI0063
            10       S-CMU01-OPER  PICTURE XX VALUE ' ='.               CI0063
            10       S-CMU01-CEKCNT.                                    CI0063
            11       S-CMU01-CEGCN.                                     CI0063
            12       S-CMU01-CEPRE    PICTURE  9(4).                    CI0063
            12       S-CMU01-CEBAS    PICTURE  9(8).                    CI0063
            10  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01               S-CM24-SSA.                                     CI0063
            10         S1-CM24-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'CACPLON'.                  CI0063
            10         S1-CM24-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-CM24-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01               S-CM54-SSA.                                     CI0063
            10         S1-CM54-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'CACPLBL'.                  CI0063
            10         S1-CM54-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-CM54-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01            S-CMU54-SSA.                                       CI0063
            10      S1-CMU54-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CACPLBL'.                  CI0063
            10      S1-CMU54-CCOM   PICTURE X VALUE '*'.                CI0063
            10       S-CMU54-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            10      S1-CMU54-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CACPLBLK'.                CI0063
            10       S-CMU54-OPER  PICTURE XX VALUE ' ='.               CI0063
            10       S-CMU54-CEKLBL.                                    CI0063
            11       S-CMU54-CESQ2    PICTURE  9(2).                    CI0063
            10  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01               S-CX01-SSA.                                     CI0063
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'CX01    '.                 CI0063
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-CX01-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01            S-CXU01-SSA.                                       CI0063
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX01    '.                 CI0063
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0063
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CX01K'.                   CI0063
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0063
            10       S-CXU01-CX01K.                                     CI0063
            11       S-CXU01-C199.                                      CI0063
            12       S-CXU01-CLID.                                      CI0063
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0063
            13       S-CXU01-CLIDN.                                     CI0063
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0063
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0063
            10  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01               S-CX03-SSA.                                     CI0063
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'CX03    '.                 CI0063
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-CX03-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01            S-CXA03-SSA.                                       CI0063
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX03    '.                 CI0063
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0063
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CARTY'.                   CI0063
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0063
            12       S-CXA03-CARTY    PICTURE  99.                      CI0063
            12  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXB03-SSA.                                       CI0063
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX03    '.                 CI0063
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0063
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(NARRS'.                   CI0063
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0063
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXC03-SSA.                                       CI0063
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX03    '.                 CI0063
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0063
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CPMTG'.                   CI0063
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0063
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0063
            11  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXD03-SSA.                                       CI0063
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX03    '.                 CI0063
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0063
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(GRCRNG'.                  CI0063
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0063
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0063
            11  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXE03-SSA.                                       CI0063
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX03    '.                 CI0063
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0063
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(DEXDT'.                   CI0063
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0063
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0063
            11  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXF03-SSA.                                       CI0063
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX03    '.                 CI0063
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0063
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CY50'.                    CI0063
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0063
            11       S-CXF03-CY50.                                      CI0063
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0063
            11  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXG03-SSA.                                       CI0063
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX03    '.                 CI0063
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0063
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(NBASQ'.                   CI0063
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0063
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXH03-SSA.                                       CI0063
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX03    '.                 CI0063
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0063
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(NARID'.                   CI0063
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0063
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0063
            12  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXU03-SSA.                                       CI0063
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX03    '.                 CI0063
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0063
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CX03K'.                   CI0063
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0063
            11       S-CXU03-CX03K.                                     CI0063
            12       S-CXU03-CARTY    PICTURE  99.                      CI0063
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01               S-CX06-SSA.                                     CI0063
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'CX06    '.                 CI0063
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-CX06-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01            S-CXU06-SSA.                                       CI0063
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX06    '.                 CI0063
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0063
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CX06K'.                   CI0063
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0063
            10       S-CXU06-CX06K.                                     CI0063
            11       S-CXU06-C299.                                      CI0063
            12       S-CXU06-CTID.                                      CI0063
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0063
            13       S-CXU06-CTIDN.                                     CI0063
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0063
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0063
            10  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01               S-CX12-SSA.                                     CI0063
            10         S1-CX12-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'CX12    '.                 CI0063
            10         S1-CX12-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-CX12-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01            S-CXA12-SSA.                                       CI0063
            10      S1-CXA12-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX12    '.                 CI0063
            10      S1-CXA12-CCOM   PICTURE X VALUE '*'.                CI0063
            10       S-CXA12-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            10      S1-CXA12-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CDEST'.                   CI0063
            10       S-CXA12-OPER  PICTURE XX VALUE ' ='.               CI0063
            10       S-CXA12-CDEST    PICTURE  99.                      CI0063
            10  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXB12-SSA.                                       CI0063
            10      S1-CXB12-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX12    '.                 CI0063
            10      S1-CXB12-CCOM   PICTURE X VALUE '*'.                CI0063
            10       S-CXB12-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            10      S1-CXB12-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(DNPMT'.                   CI0063
            10       S-CXB12-OPER  PICTURE XX VALUE ' ='.               CI0063
            10       S-CXB12-DNPMT    PICTURE  9(8).                    CI0063
            10  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXC12-SSA.                                       CI0063
            11      S1-CXC12-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX12    '.                 CI0063
            11      S1-CXC12-CCOM   PICTURE X VALUE '*'.                CI0063
            11       S-CXC12-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            11      S1-CXC12-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(NAPDS'.                   CI0063
            11       S-CXC12-OPER  PICTURE XX VALUE ' ='.               CI0063
            11       S-CXC12-NAPDS    PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXU12-SSA.                                       CI0063
            10      S1-CXU12-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX12    '.                 CI0063
            10      S1-CXU12-CCOM   PICTURE X VALUE '*'.                CI0063
            10       S-CXU12-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            10      S1-CXU12-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CX12K'.                   CI0063
            10       S-CXU12-OPER  PICTURE XX VALUE ' ='.               CI0063
            10       S-CXU12-CX12K.                                     CI0063
            11       S-CXU12-CPMTC    PICTURE  99.                      CI0063
            11       S-CXU12-NAPDS    PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11       S-CXU12-GESTD    PICTURE  9(8).                    CI0063
            10  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01               S-CX13-SSA.                                     CI0063
            10         S1-CX13-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'CX13    '.                 CI0063
            10         S1-CX13-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-CX13-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01            S-CXA13-SSA.                                       CI0063
            11      S1-CXA13-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX13    '.                 CI0063
            11      S1-CXA13-CCOM   PICTURE X VALUE '*'.                CI0063
            11       S-CXA13-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            11      S1-CXA13-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CDEST'.                   CI0063
            11       S-CXA13-OPER  PICTURE XX VALUE ' ='.               CI0063
            11       S-CXA13-CDEST    PICTURE  99.                      CI0063
            11  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXB13-SSA.                                       CI0063
            12      S1-CXB13-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX13    '.                 CI0063
            12      S1-CXB13-CCOM   PICTURE X VALUE '*'.                CI0063
            12       S-CXB13-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            12      S1-CXB13-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CARTZ'.                   CI0063
            12       S-CXB13-OPER  PICTURE XX VALUE ' ='.               CI0063
            12       S-CXB13-CARTZ    PICTURE  99.                      CI0063
            12  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXC13-SSA.                                       CI0063
            12      S1-CXC13-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX13    '.                 CI0063
            12      S1-CXC13-CCOM   PICTURE X VALUE '*'.                CI0063
            12       S-CXC13-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            12      S1-CXC13-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(NAPDS'.                   CI0063
            12       S-CXC13-OPER  PICTURE XX VALUE ' ='.               CI0063
            12       S-CXC13-NAPDS    PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXU13-SSA.                                       CI0063
            11      S1-CXU13-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX13    '.                 CI0063
            11      S1-CXU13-CCOM   PICTURE X VALUE '*'.                CI0063
            11       S-CXU13-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            11      S1-CXU13-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CX13K'.                   CI0063
            11       S-CXU13-OPER  PICTURE XX VALUE ' ='.               CI0063
            11       S-CXU13-CX13K.                                     CI0063
            12       S-CXU13-CARTZ    PICTURE  99.                      CI0063
            12       S-CXU13-NAPDS    PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CX113-SSA.                                       CI0063
            11      S1-CX113-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX13    '.                 CI0063
            11      S1-CX113-CCOM   PICTURE X VALUE '*'.                CI0063
            11       S-CX113-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            11      S1-CX113-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(XGCUSPY'.                 CI0063
            11       S-CX113-OPER  PICTURE XX VALUE ' ='.               CI0063
            11       S-CX113-GCUSPY   PICTURE  X(12).                   CI0063
            11  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01               S-CX14-SSA.                                     CI0063
            10         S1-CX14-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'CX14    '.                 CI0063
            10         S1-CX14-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-CX14-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01            S-CXU14-SSA.                                       CI0063
            10      S1-CXU14-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX14    '.                 CI0063
            10      S1-CXU14-CCOM   PICTURE X VALUE '*'.                CI0063
            10       S-CXU14-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            10      S1-CXU14-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CX14K'.                   CI0063
            10       S-CXU14-OPER  PICTURE XX VALUE ' ='.               CI0063
            10       S-CXU14-CX14K.                                     CI0063
            11       S-CXU14-NPISQ    PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CX114-SSA.                                       CI0063
            11      S1-CX114-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX14    '.                 CI0063
            11      S1-CX114-CCOM   PICTURE X VALUE '*'.                CI0063
            11       S-CX114-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            11      S1-CX114-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(XGCUSPZ'.                 CI0063
            11       S-CX114-OPER  PICTURE XX VALUE ' ='.               CI0063
            11       S-CX114-GCUSPZ   PICTURE  X(12).                   CI0063
            11  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01               S-CX18-SSA.                                     CI0063
            10         S1-CX18-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'CX18    '.                 CI0063
            10         S1-CX18-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-CX18-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01            S-CXA18-SSA.                                       CI0063
            10      S1-CXA18-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX18    '.                 CI0063
            10      S1-CXA18-CCOM   PICTURE X VALUE '*'.                CI0063
            10       S-CXA18-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            10      S1-CXA18-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CSTPRE'.                  CI0063
            10       S-CXA18-OPER  PICTURE XX VALUE ' ='.               CI0063
            10       S-CXA18-CSTPRE   PICTURE  99.                      CI0063
            10  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXB18-SSA.                                       CI0063
            10      S1-CXB18-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX18    '.                 CI0063
            10      S1-CXB18-CCOM   PICTURE X VALUE '*'.                CI0063
            10       S-CXB18-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            10      S1-CXB18-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CSPCR'.                   CI0063
            10       S-CXB18-OPER  PICTURE XX VALUE ' ='.               CI0063
            10       S-CXB18-CSPCR    PICTURE  99.                      CI0063
            10  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXU18-SSA.                                       CI0063
            10      S1-CXU18-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX18    '.                 CI0063
            10      S1-CXU18-CCOM   PICTURE X VALUE '*'.                CI0063
            10       S-CXU18-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            10      S1-CXU18-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CX18K'.                   CI0063
            10       S-CXU18-OPER  PICTURE XX VALUE ' ='.               CI0063
            10       S-CXU18-CX18K.                                     CI0063
            11       S-CXU18-NBASQ    PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01               S-CX2Y-SSA.                                     CI0063
            10         S1-CX2Y-SEGNAM PICTURE X(8)                      CI0063
                                      VALUE 'CX2Y    '.                 CI0063
            10         S1-CX2Y-CCOM   PICTURE X VALUE '*'.              CI0063
            10          S-CX2Y-CCOD   PICTURE X(5)                      CI0063
                                      VALUE '-----'.                    CI0063
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0063
       01            S-CXA2Y-SSA.                                       CI0063
            11      S1-CXA2Y-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX2Y    '.                 CI0063
            11      S1-CXA2Y-CCOM   PICTURE X VALUE '*'.                CI0063
            11       S-CXA2Y-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            11      S1-CXA2Y-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CARTY'.                   CI0063
            11       S-CXA2Y-OPER  PICTURE XX VALUE ' ='.               CI0063
            11       S-CXA2Y-CARTY    PICTURE  99.                      CI0063
            11  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXB2Y-SSA.                                       CI0063
            11      S1-CXB2Y-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX2Y    '.                 CI0063
            11      S1-CXB2Y-CCOM   PICTURE X VALUE '*'.                CI0063
            11       S-CXB2Y-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            11      S1-CXB2Y-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(C299'.                    CI0063
            11       S-CXB2Y-OPER  PICTURE XX VALUE ' ='.               CI0063
            11       S-CXB2Y-C299.                                      CI0063
            12       S-CXB2Y-CTID.                                      CI0063
            13       S-CXB2Y-CTIDA    PICTURE  9(3).                    CI0063
            13       S-CXB2Y-CTIDN.                                     CI0063
            14       S-CXB2Y-CTIDNP   PICTURE  X(13).                   CI0063
            14       S-CXB2Y-CTIDND   PICTURE  9(11).                   CI0063
            11  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01            S-CXU2Y-SSA.                                       CI0063
            10      S1-CXU2Y-SEGNAM PICTURE X(8)                        CI0063
                                      VALUE 'CX2Y    '.                 CI0063
            10      S1-CXU2Y-CCOM   PICTURE X VALUE '*'.                CI0063
            10       S-CXU2Y-CCOD   PICTURE X(5)                        CI0063
                                      VALUE '-----'.                    CI0063
            10      S1-CXU2Y-FLDNAM PICTURE X(9)                        CI0063
                                      VALUE '(CX2YK'.                   CI0063
            10       S-CXU2Y-OPER  PICTURE XX VALUE ' ='.               CI0063
            10       S-CXU2Y-CX2YK.                                     CI0063
            11       S-CXU2Y-C299.                                      CI0063
            12       S-CXU2Y-CTID.                                      CI0063
            13       S-CXU2Y-CTIDA    PICTURE  9(3).                    CI0063
            13       S-CXU2Y-CTIDN.                                     CI0063
            14       S-CXU2Y-CTIDNP   PICTURE  X(13).                   CI0063
            14       S-CXU2Y-CTIDND   PICTURE  9(11).                   CI0063
            11       S-CXU2Y-C199.                                      CI0063
            12       S-CXU2Y-CLID.                                      CI0063
            13       S-CXU2Y-CLIDO    PICTURE  9(3).                    CI0063
            13       S-CXU2Y-CLIDN.                                     CI0063
            14       S-CXU2Y-CLIDNP   PICTURE  X(12).                   CI0063
            14       S-CXU2Y-CLIDND   PICTURE  9(8).                    CI0063
            11       S-CXU2Y-CARTY    PICTURE  99.                      CI0063
            11       S-CXU2Y-NARRS    PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10  FILLER   PICTURE X    VALUE ')'.                        CI0063
       01   ZONES-UTILISATEUR PICTURE X.                                CI0063
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
      ** PCB POINTER FOR LM1P                                           ADU015
            05 PCB-LM1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CA1P                                           ADU015
            05 PCB-CA1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR ARAY                                           ADU015
            05 PCB-ARAY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=XA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XA00.                                         CI0063
          05              XA00-SUITE.                                   CI0063
            15       FILLER         PICTURE  X(00106).                  CI0063
       01                 XA06  REDEFINES      XA00.                    CI0063
            10            XA06-XDBPCB.                                  CI0063
            11            XA06-XDBDNM PICTURE  X(08).                   CI0063
            11            XA06-XSEGLV PICTURE  X(02).                   CI0063
            11            XA06-XRC    PICTURE  X(02).                   CI0063
            11            XA06-XPROPT PICTURE  X(04).                   CI0063
            11            XA06-FILLER PICTURE  S9(5)                    CI0063
                          BINARY.                                       CI0063
            11            XA06-XSEGNM PICTURE  X(08).                   CI0063
            11            XA06-XKEYLN PICTURE  S9(05)                   CI0063
                          BINARY.                                       CI0063
            11            XA06-XSEGNB PICTURE  9(05)                    CI0063
                          BINARY.                                       CI0063
            11            XA06-XCOKEY PICTURE  X(70).                   CI0063
      *** PCB MASK FOR LM1P                                             ADU015
      *!WF DSP=XB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XB00.                                         CI0063
          05              XB00-SUITE.                                   CI0063
            15       FILLER         PICTURE  X(00106).                  CI0063
       01                 XB06  REDEFINES      XB00.                    CI0063
            10            XB06-XDBPCB.                                  CI0063
            11            XB06-XDBDNM PICTURE  X(08).                   CI0063
            11            XB06-XSEGLV PICTURE  X(02).                   CI0063
            11            XB06-XRC    PICTURE  X(02).                   CI0063
            11            XB06-XPROPT PICTURE  X(04).                   CI0063
            11            XB06-FILLER PICTURE  S9(5)                    CI0063
                          BINARY.                                       CI0063
            11            XB06-XSEGNM PICTURE  X(08).                   CI0063
            11            XB06-XKEYLN PICTURE  S9(05)                   CI0063
                          BINARY.                                       CI0063
            11            XB06-XSEGNB PICTURE  9(05)                    CI0063
                          BINARY.                                       CI0063
            11            XB06-XCOKEY PICTURE  X(70).                   CI0063
      *** PCB MASK FOR CA1P                                             ADU015
      *!WF DSP=XC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XC00.                                         CI0063
          05              XC00-SUITE.                                   CI0063
            15       FILLER         PICTURE  X(00106).                  CI0063
       01                 XC06  REDEFINES      XC00.                    CI0063
            10            XC06-XDBPCB.                                  CI0063
            11            XC06-XDBDNM PICTURE  X(08).                   CI0063
            11            XC06-XSEGLV PICTURE  X(02).                   CI0063
            11            XC06-XRC    PICTURE  X(02).                   CI0063
            11            XC06-XPROPT PICTURE  X(04).                   CI0063
            11            XC06-FILLER PICTURE  S9(5)                    CI0063
                          BINARY.                                       CI0063
            11            XC06-XSEGNM PICTURE  X(08).                   CI0063
            11            XC06-XKEYLN PICTURE  S9(05)                   CI0063
                          BINARY.                                       CI0063
            11            XC06-XSEGNB PICTURE  9(05)                    CI0063
                          BINARY.                                       CI0063
            11            XC06-XCOKEY PICTURE  X(70).                   CI0063
      *** PCB MASK FOR ARAY                                             ADU015
      *!WF DSP=XD DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XD00.                                         CI0063
          05              XD00-SUITE.                                   CI0063
            15       FILLER         PICTURE  X(00106).                  CI0063
       01                 XD06  REDEFINES      XD00.                    CI0063
            10            XD06-XDBPCB.                                  CI0063
            11            XD06-XDBDNM PICTURE  X(08).                   CI0063
            11            XD06-XSEGLV PICTURE  X(02).                   CI0063
            11            XD06-XRC    PICTURE  X(02).                   CI0063
            11            XD06-XPROPT PICTURE  X(04).                   CI0063
            11            XD06-FILLER PICTURE  S9(5)                    CI0063
                          BINARY.                                       CI0063
            11            XD06-XSEGNM PICTURE  X(08).                   CI0063
            11            XD06-XKEYLN PICTURE  S9(05)                   CI0063
                          BINARY.                                       CI0063
            11            XD06-XSEGNB PICTURE  9(05)                    CI0063
                          BINARY.                                       CI0063
            11            XD06-XCOKEY PICTURE  X(70).                   CI0063
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=XE DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XE00.                                         CI0063
          05              XE00-SUITE.                                   CI0063
            15       FILLER         PICTURE  X(00106).                  CI0063
       01                 XE06  REDEFINES      XE00.                    CI0063
            10            XE06-XDBPCB.                                  CI0063
            11            XE06-XDBDNM PICTURE  X(08).                   CI0063
            11            XE06-XSEGLV PICTURE  X(02).                   CI0063
            11            XE06-XRC    PICTURE  X(02).                   CI0063
            11            XE06-XPROPT PICTURE  X(04).                   CI0063
            11            XE06-FILLER PICTURE  S9(5)                    CI0063
                          BINARY.                                       CI0063
            11            XE06-XSEGNM PICTURE  X(08).                   CI0063
            11            XE06-XKEYLN PICTURE  S9(05)                   CI0063
                          BINARY.                                       CI0063
            11            XE06-XSEGNB PICTURE  9(05)                    CI0063
                          BINARY.                                       CI0063
            11            XE06-XCOKEY PICTURE  X(70).                   CI0063
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=XF DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XF00.                                         CI0063
          05              XF00-SUITE.                                   CI0063
            15       FILLER         PICTURE  X(00106).                  CI0063
       01                 XF06  REDEFINES      XF00.                    CI0063
            10            XF06-XDBPCB.                                  CI0063
            11            XF06-XDBDNM PICTURE  X(08).                   CI0063
            11            XF06-XSEGLV PICTURE  X(02).                   CI0063
            11            XF06-XRC    PICTURE  X(02).                   CI0063
            11            XF06-XPROPT PICTURE  X(04).                   CI0063
            11            XF06-FILLER PICTURE  S9(5)                    CI0063
                          BINARY.                                       CI0063
            11            XF06-XSEGNM PICTURE  X(08).                   CI0063
            11            XF06-XKEYLN PICTURE  S9(05)                   CI0063
                          BINARY.                                       CI0063
            11            XF06-XSEGNB PICTURE  9(05)                    CI0063
                          BINARY.                                       CI0063
            11            XF06-XCOKEY PICTURE  X(70).                   CI0063

      *PASS AREA TO/FROM CI0063
      *!WF DSP=WZ DSL=DU SEL=63 FOR=I DES=1 LEV=1 PLT=10
       01                 WZ63.                                         CI0063
            10            WZ63-DU76.                                    CI0063
            11            WZ63-CUPIQ  PICTURE  X.                       CI0063
            11            WZ63-CACTS  PICTURE  X.                       CI0063
            11            WZ63-CACTA  PICTURE  X(1).                    CI0063
            11            WZ63-GEMDA  PICTURE  9(8).                    CI0063
            11            WZ63-DCACG  PICTURE  9(8).                    CI0063
            11            WZ63-DNACG  PICTURE  9(8).                    CI0063
            11            WZ63-IBDPR  PICTURE  X.                       CI0063
            11            WZ63-IUGMA  PICTURE  X.                       CI0063
            10            WZ63-C199.                                    CI0063
            11            WZ63-CLID.                                    CI0063
            12            WZ63-CLIDO  PICTURE  9(3).                    CI0063
            12            WZ63-CLIDN.                                   CI0063
            13            WZ63-CLIDNP PICTURE  X(12).                   CI0063
            13            WZ63-CLIDND PICTURE  9(8).                    CI0063
            10            WZ63-CARTY  PICTURE  99.                      CI0063
            10            WZ63-NARRS  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            WZ63-DU71.                                    CI0063
            11            WZ63-IARTYA PICTURE  X.                       CI0063
            11            WZ63-CERRBB PICTURE  X(02).                   CI0063
            11            WZ63-IARRGA PICTURE  X.                       CI0063
            11            WZ63-CERRBR PICTURE  X(02).                   CI0063
            11            WZ63-IARLNA PICTURE  X.                       CI0063
            11            WZ63-CERRBL PICTURE  X(02).                   CI0063
            11            WZ63-IFQOD  PICTURE  X.                       CI0063
            11            WZ63-CERRBO PICTURE  X(02).                   CI0063
            11            WZ63-IFQODL PICTURE  X(01).                   CI0063
            11            WZ63-CERRBP PICTURE  X(02).                   CI0063
            11            WZ63-IFQAN  PICTURE  X.                       CI0063
            11            WZ63-IFQSA  PICTURE  X.                       CI0063
            11            WZ63-IFQFM  PICTURE  X.                       CI0063
            11            WZ63-IFQQT  PICTURE  X.                       CI0063
            11            WZ63-IFQBM  PICTURE  X.                       CI0063
            11            WZ63-IFQMO  PICTURE  X.                       CI0063
            11            WZ63-IFQBF  PICTURE  X.                       CI0063
            11            WZ63-IFQSM  PICTURE  X.                       CI0063
            11            WZ63-IFQBW  PICTURE  X.                       CI0063
            11            WZ63-IFQWK  PICTURE  X.                       CI0063
            11            WZ63-IFQIR  PICTURE  X.                       CI0063
            11            WZ63-IFQIF  PICTURE  X.                       CI0063
            11            WZ63-IFQIS  PICTURE  X.                       CI0063
            11            WZ63-IFQIK  PICTURE  X.                       CI0063
            11            WZ63-IFQIW  PICTURE  X.                       CI0063
            11            WZ63-IARPSA PICTURE  X.                       CI0063
            10            WZ63-DU72.                                    CI0063
            11            WZ63-AMAXA  PICTURE  S9(7)V99.                CI0063
            11            WZ63-AMAXAO PICTURE  S9(7)V99.                CI0063
            11            WZ63-AMINA  PICTURE  S9(7)V99.                CI0063
            11            WZ63-AMNDL  PICTURE  S9(5)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WZ63-ALMIN  PICTURE  S9(5)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WZ63-ALPAGR PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WZ63-ALPAGS PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WZ63-ALPAGQ PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WZ63-ALPAGM PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            WZ63-DU75.                                    CI0063
            11            WZ63-ALDDUE PICTURE  9(08).                   CI0063
            11            WZ63-DENDD  PICTURE  9(08).                   CI0063
            11            WZ63-DAUTB  PICTURE  9(8).                    CI0063
            10            WZ63-DU73.                                    CI0063
            11            WZ63-IACHI  PICTURE  X.                       CI0063
            11            WZ63-CERRBC PICTURE  X(02).                   CI0063
            11            WZ63-ISTAT  PICTURE  X.                       CI0063
            11            WZ63-CERRBS PICTURE  X(02).                   CI0063
            11            WZ63-IDELT  PICTURE  X.                       CI0063
            11            WZ63-CERRBD PICTURE  X(02).                   CI0063
            10            WZ63-CIRAD  PICTURE  X.                       CI0063
            10            WZ63-CFLOW  PICTURE  X.                       CI0063
            10            WZ63-DU74.                                    CI0063
            11            WZ63-CPMTCX PICTURE  XX.                      CI0063
            11            WZ63-NAPDSK PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WZ63-GESTD1 PICTURE  9(8).                    CI0063
            11            WZ63-DNPMT1 PICTURE  9(8).                    CI0063
            10            WZ63-CX06.                                    CI0063
            11            WZ63-CX06K.                                   CI0063
            12            WZ63-C299.                                    CI0063
            13            WZ63-CTID.                                    CI0063
            14            WZ63-CTIDA  PICTURE  9(3).                    CI0063
            14            WZ63-CTIDN.                                   CI0063
            15            WZ63-CTIDNP PICTURE  X(13).                   CI0063
            15            WZ63-CTIDND PICTURE  9(11).                   CI0063
            11            WZ63-NPECK  PICTURE  9(02).                   CI0063
            11            WZ63-FILLER PICTURE  X.                       CI0063
            10            WZ63-CX12.                                    CI0063
            11            WZ63-CX12K.                                   CI0063
            12            WZ63-CPMTC  PICTURE  99.                      CI0063
            12            WZ63-NAPDS  PICTURE  S9(3)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            12            WZ63-GESTD  PICTURE  9(8).                    CI0063
            11            WZ63-GEEND  PICTURE  9(8).                    CI0063
            11            WZ63-CIRMO  PICTURE  X(12).                   CI0063
            11            WZ63-CDEST  PICTURE  99.                      CI0063
            11            WZ63-APMTL  PICTURE  S9(9)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WZ63-DNPMT  PICTURE  9(8).                    CI0063
            11            WZ63-NIRACM PICTURE  9(2).                    CI0063
            11            WZ63-CPMTF  PICTURE  99.                      CI0063
            11            WZ63-IPODM  PICTURE  X.                       CI0063
            11            WZ63-CLUPD  PICTURE  9(3).                    CI0063
            11            WZ63-DLAUP  PICTURE  9(8).                    CI0063
            11            WZ63-CWRC   PICTURE  99.                      CI0063
            11            WZ63-CHCR   PICTURE  99.                      CI0063
            11            WZ63-GEOPD2 PICTURE  X(8).                    CI0063
            11            WZ63-GEAUN  PICTURE  9(5).                    CI0063
            11            WZ63-DPCHD  PICTURE  9(8).                    CI0063
            11            WZ63-DNEXE  PICTURE  9(8).                    CI0063
            11            WZ63-CCSMQ  PICTURE  X.                       CI0063
            11            WZ63-GCUSPZ PICTURE  X(12).                   CI0063
            11            WZ63-CORTY  PICTURE  X.                       CI0063
            11            WZ63-CNAVR  PICTURE  X(1).                    CI0063
            11            WZ63-DELOI3 PICTURE  9(6).                    CI0063
            11            WZ63-ALOIDD PICTURE  9(9)V99                  CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            WZ63-FILLER PICTURE  X(5).                    CI0063
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE CT01 SEGMENT WHICH IS PASSED    *
      **     TO THIS MODULE FROM THE CALLING PROGRAM                   *
      ******************************************************************
      *
      *!WF DSP=CT DSL=CT SEL=01 FOR=I DES=1 LEV=1 PLT=20
       01                 CT01.                                         CI0063
            10            CT01-CT01K.                                   CI0063
            11            CT01-C299.                                    CI0063
            12            CT01-CTID.                                    CI0063
            13            CT01-CTIDA  PICTURE  9(3).                    CI0063
            13            CT01-CTIDN.                                   CI0063
            14            CT01-CTIDNP PICTURE  X(13).                   CI0063
            14            CT01-CTIDND PICTURE  9(11).                   CI0063
            10            CT01-GECKD  PICTURE  9.                       CI0063
            10            CT01-GEMDA  PICTURE  9(8).                    CI0063
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0063
                          BINARY.                                       CI0063
            10            CT01-GECUC  PICTURE  99.                      CI0063
            10            CT01-CTAUL  PICTURE  9(3).                    CI0063
            10            CT01-DIRAC  PICTURE  9(4).                    CI0063
            10            CT01-CTCCI  PICTURE  X.                       CI0063
            10            CT01-CTCUS  PICTURE  999.                     CI0063
            10            CT01-CTEFD  PICTURE  9(8).                    CI0063
            10            CT01-CTIAD  PICTURE  9(8).                    CI0063
            10            CT01-CLCUS  PICTURE  99.                      CI0063
            10            CT01-CAMMB  PICTURE  X(3).                    CI0063
            10            CT01-CKPMM  PICTURE  X.                       CI0063
            10            CT01-CTLAD  PICTURE  9(8).                    CI0063
            10            CT01-IPERS  PICTURE  X.                       CI0063
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            CT01-CTLAT  PICTURE  9(8).                    CI0063
            10            CT01-CTLATC PICTURE  9(6).                    CI0063
            10            CT01-IMEGA  PICTURE  X.                       CI0063
            10            CT01-DIRAB  PICTURE  9(8).                    CI0063
            10            CT01-COLRQ  PICTURE  X.                       CI0063
            10            CT01-ZDA04  PICTURE  X(4).                    CI0063
            10            CT01-CTLPD  PICTURE  9(8).                    CI0063
            10            CT01-CIRASP PICTURE  9.                       CI0063
            10            CT01-CIRATP PICTURE  99.                      CI0063
            10            CT01-DRTHC  PICTURE  9(8).                    CI0063
            10            CT01-CPPTC  PICTURE  X.                       CI0063
            10            CT01-ZDA06  PICTURE  X(6).                    CI0063
            10            CT01-CTACD  PICTURE  9(8).                    CI0063
            10            CT01-CTNLI  PICTURE  X.                       CI0063
            10            CT01-CTRHO  PICTURE  9(8).                    CI0063
            10            CT01-CTSGD  PICTURE  9(8).                    CI0063
            10            CT01-CPATP  PICTURE  X(1).                    CI0063
            10            CT01-IRSTA  PICTURE  X.                       CI0063
            10            CT01-CTSTA  PICTURE  99.                      CI0063
            10            CT01-CTSSC  PICTURE  99.                      CI0063
            10            CT01-PRLIN  PICTURE  9(3).                    CI0063
            10            CT01-PRCOD  PICTURE  9(5).                    CI0063
            10            CT01-PRSCD  PICTURE  X(9).                    CI0063
            10            CT01-CTLNI  PICTURE  X.                       CI0063
            10            CT01-AYSIDA PICTURE  9(3).                    CI0063
            10            CT01-AYSID  PICTURE  9(5).                    CI0063
            10            CT01-CTBMC  PICTURE  99.                      CI0063
            10            CT01-CINAR  PICTURE  99.                      CI0063
            10            CT01-CPHTR  PICTURE  X.                       CI0063
            10            CT01-CDSTR  PICTURE  XX.                      CI0063
            10            CT01-CQACT  PICTURE  999.                     CI0063
            10            CT01-CIRAS  PICTURE  999.                     CI0063
            10            CT01-CIRAT  PICTURE  999.                     CI0063
            10            CT01-CLRAY  PICTURE  9(5).                    CI0063
            10            CT01-CATTP  PICTURE  X.                       CI0063
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0063
          05              DE00-SUITE.                                   CI0063
            15       FILLER         PICTURE  X(00653).                  CI0063
       01                 DE10  REDEFINES      DE00.                    CI0063
            10            DE10-DU11.                                    CI0063
            11            DE10-XFONC  PICTURE  X(4).                    CI0063
            11            DE10-MPSBN  PICTURE  X(8).                    CI0063
            11            DE10-XDBDNM PICTURE  X(08).                   CI0063
            11            DE10-XSEGNM PICTURE  X(08).                   CI0063
            11            DE10-XRC    PICTURE  X(02).                   CI0063
            11            DE10-MSEG   PICTURE  X(08).                   CI0063
            11            DE10-XCOKEY PICTURE  X(70).                   CI0063
            11            DE10-CUIBR  PICTURE  X(01).                   CI0063
            11            DE10-CUIBA  PICTURE  X(01).                   CI0063
            11            DE10-IPBIK  PICTURE  X(1).                    CI0063
            10            DE10-DU03.                                    CI0063
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            DE10-CMSSF  PICTURE  XX.                      CI0063
            11            DE10-DU09.                                    CI0063
            12            DE10-CMESA  PICTURE  S9(9)                    CI0063
                          BINARY.                                       CI0063
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0063
                          BINARY.                                       CI0063
            12            DE10-CMESB  PICTURE  S9(9)                    CI0063
                          BINARY.                                       CI0063
            12            DE10-CMSST  PICTURE  S9(9)                    CI0063
                          BINARY.                                       CI0063
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0063
                          BINARY.                                       CI0063
            12            DE10-QELLAA PICTURE  S9(9)                    CI0063
                          BINARY.                                       CI0063
            12            DE10-TMESS4 PICTURE  X(512).                  CI0063
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
       01                 MS00.                                         CI0063
          05              MS00-SUITE.                                   CI0063
            15       FILLER         PICTURE  X(00542).                  CI0063
       01                 MS03  REDEFINES      MS00.                    CI0063
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            10            MS03-CMSSF  PICTURE  XX.                      CI0063
            10            MS03-DU09.                                    CI0063
            11            MS03-CMESA  PICTURE  S9(9)                    CI0063
                          BINARY.                                       CI0063
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0063
                          BINARY.                                       CI0063
            11            MS03-CMESB  PICTURE  S9(9)                    CI0063
                          BINARY.                                       CI0063
            11            MS03-CMSST  PICTURE  S9(9)                    CI0063
                          BINARY.                                       CI0063
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0063
                          BINARY.                                       CI0063
            11            MS03-QELLAA PICTURE  S9(9)                    CI0063
                          BINARY.                                       CI0063
            11            MS03-TMESS4 PICTURE  X(512).                  CI0063
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0063
            10            MX11-QMSGS  PICTURE  9(03).                   CI0063
            10            MX11-PJ09                                     CI0063
                          OCCURS       025     TIMES.                   CI0063
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0063
                          COMPUTATIONAL-3.                              CI0063
            11            MX11-CMESB  PICTURE  S9(9)                    CI0063
                          BINARY.                                       CI0063
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                WZ63
                                CT01
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0063
      *               *                                   *             CI0063
      *               *INITIALISATIONS                    *             CI0063
      *               *                                   *             CI0063
      *               *************************************.            CI0063
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
      *N02CA.    NOTE *SET ADDRESS FOR ARRANGEMENT DB     *.
       F02CA.                                                           lv10
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF XA06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR LM1P                                             DOT
           SET ADDRESS OF XB06 TO                                       ADU015
                PCB-LM1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CA1P                                             DOT
           SET ADDRESS OF XC06 TO                                       ADU015
                PCB-CA1P-PTR1.                                          ADU015
      *SET ADDRESS FOR ARAY                                             DOT
           SET ADDRESS OF XD06 TO                                       ADU015
                PCB-ARAY-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF XE06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF XF06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
       F02CA-FN. EXIT.
      *N02FA.    NOTE *INITIALIZE THE MESSAGE AREA        *.
       F02FA.                                                           lv10
      **
      *********************************
      ** INITIALIZE THE MS03 SEG      *
      ** WHICH WILL BE RETURNED TO THE*
      ** CALLING MODULE               *
      *********************************
      **
           INITIALIZE  MS03
           W-WK00-AREAS
           W-SW00-AREAS.
       F02FA-FN. EXIT.
      *N02FG.    NOTE *********************************   *.            ACMCTI
       F02FG.                                                           lv10
      ** SUB-FUNCTION TO PERFORM A    *                                 ACMCTI
      ** DUMMY DB2 CALL.              *                                 ACMCTI
      *********************************                                 ACMCTI
      *SET
      * :WS00-DATE = CURRENT_DATE
      *F93SQ
       F02FG-FN. EXIT.
      *N02HA.    NOTE *QUICK VALIDATION OF PSSD PARAMS    *.
       F02HA.    IF    WZ63-CLID NOT NUMERIC                            lv10
                 OR    WZ63-CARTY NOT NUMERIC
                 OR    WZ63-NARRS NOT NUMERIC
                 OR    WZ63-CTID NOT NUMERIC
                 OR    WZ63-CPMTC NOT NUMERIC
                 OR    WZ63-NAPDS NOT NUMERIC
                 OR    WZ63-GESTD NOT NUMERIC
                 NEXT SENTENCE ELSE GO TO     F02HA-FN.
      *********************************
      ** DO AN INITIAL CHECK TO ENSURE*
      ** THAT ALL THE PARAMETERS ARE  *
      ** OF THE RIGHT TYPE.           *
      *********************************
      **
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012596 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F02HA-FN. EXIT.
      *N02IA.    NOTE *ENSURE PROCESSING CODES ARE GOOD   *.
       F02IA.    IF    (WZ63-CUPIQ NOT = 'I'                            lv10
                 AND   WZ63-CUPIQ NOT = 'U')
                 OR    (WZ63-CACTS NOT = 'A'
                 AND   WZ63-CACTS NOT = 'C'
                 AND   WZ63-CACTS NOT = 'D'
                 AND   WZ63-CACTS NOT = 'S')
                 OR    (WZ63-CACTA NOT = 'A'
                 AND   WZ63-CACTA NOT = 'C'
                 AND   WZ63-CACTA NOT = 'D'
                 AND   WZ63-CACTA NOT = 'I'
                 AND   WZ63-CACTA NOT = 'F'
                 AND   WZ63-CACTA NOT = 'R')
                 NEXT SENTENCE ELSE GO TO     F02IA-FN.
      *- CUPIQ MUST BE (I)NQUIRY OR
      *  (U)PDATE   &
      *- CACTS MUST BE (A)DD SCREEN,
      *  (C)HANGE SCREEN, (D)ELETE
      *  FUNCTION OR (S)TATUS FUNCTION
      *- CACTA MUST BE (A)DD, (C)HANGE,
      *  (D)ELETE, (I)NACTIVATE,
      *  (F)UTURE OR (R)EINSTATE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012596 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F02IA-FN. EXIT.
      *N02IG.    NOTE *ENSURE CONTRACT PASSED             *.
       F02IG.    IF    CT01-CTID NOT =                                  lv10
                       WZ63-CTID
                 NEXT SENTENCE ELSE GO TO     F02IG-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012012 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F02IG-FN. EXIT.
      *N02IK.    NOTE *ENSURE PROCESSING DATE IS GOOD     *.
       F02IK.                                                           lv10
           MOVE        WZ63-GEMDA TO DD01-XDATG
           PERFORM     F93DD THRU F93DD-FN.
                 IF    DEL-ER NOT = 1                                   DOT
      *IF BAD DATE; ERROR OUT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012257 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F02IK-FN. EXIT.
      *N02IT.    NOTE *VALIDATE BACK DATE FIELD           *.
       F02IT.    IF    WZ63-IBDPR NOT = 'N'                             lv10
                 AND   WZ63-IBDPR NOT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F02IT-FN.
           MOVE        'N' TO WZ63-IBDPR.
       F02IT-FN. EXIT.
      *N02MA.    NOTE *INITIALIZE ALL OUTGOING LINKAGE    *.
       F02MA.                                                           lv10
           MOVE        SPACE TO WZ63-CERRBB
           WZ63-CERRBR
           WZ63-CERRBL
           WZ63-CERRBO
           WZ63-CERRBP
           WZ63-CERRBC
           WZ63-CERRBD
           WZ63-CERRBS
           WZ63-CFLOW
           MOVE        'N' TO WZ63-IARTYA
           WZ63-IARLNA
           WZ63-IARRGA
           WZ63-IFQAN
           WZ63-IFQSA
           WZ63-IFQFM
           WZ63-IFQQT
           WZ63-IFQBM
           WZ63-IFQMO
           WZ63-IFQBF
           WZ63-IFQSM
           WZ63-IFQBW
           WZ63-IFQWK
           WZ63-IFQIR
           WZ63-IFQIF
           WZ63-IFQIS
           WZ63-IFQIK
           WZ63-IFQIW
           WZ63-IFQOD
           WZ63-IFQODL
           WZ63-IARPSA
           MOVE        9999999.99 TO WZ63-AMAXA
           MOVE        25000.00 TO WZ63-AMAXAO
           MOVE        ZERO TO WZ63-AMINA
           WZ63-AMNDL
           WZ63-ALMIN
           WZ63-ALPAGR
           WZ63-ALPAGS
           WZ63-ALPAGQ
           WZ63-ALPAGM
           WZ63-ALDDUE
           WZ63-DENDD
           WZ63-DAUTB
           WZ63-CPMTCX
           WZ63-NAPDSK
           WZ63-GESTD1
           WZ63-DNPMT1
           MOVE        'N' TO WZ63-CIRAD
           WZ63-IACHI
           WZ63-ISTAT
           WZ63-IDELT.
       F02MA-FN. EXIT.
      *N02MD.    NOTE *INITIALIZE FARTHEST CX12 KEYS      *.
       F02MD.                                                           lv10
           MOVE        ZERO TO W-PREV-CPMTCX
           W-PREV-NAPDSK
           W-PREV-GESTD1
           W-PREV-DNPMT1.
       F02MD-FN. EXIT.
      *N02NA.    NOTE *IF DELETE AND CONTRACT NOT FOUND   *.
       F02NA.    IF    CT01-PRCOD = ZERO                                lv10
                 AND   WZ63-CACTA = 'D'
                 NEXT SENTENCE ELSE GO TO     F02NA-FN.
      *- IF CONTRACT NOT FOUND; DON"T
      *  EDIT; LET IT BE BLOWN AWAY
      *N02ND.    NOTE *IF ADMIN IS ALLOWED                *.
       F02ND.    IF    CT01-CTIDA = 001                                 lv15
                 OR    CT01-CTIDA = 002
                 OR    CT01-CTIDA = 004
                 OR    CT01-CTIDA = 005
                 OR    CT01-CTIDA = 013
                 NEXT SENTENCE ELSE GO TO     F02ND-FN.
           MOVE        'Y' TO WZ63-IDELT.
       F02ND-FN. EXIT.
      *N02NG.    NOTE *GET OUT IF ADMIN ALLOW"D OR NOT    *.
       F02NG.                                                           lv15
           MOVE                     ALL '1' TO FT GO TO F20.
       F02NG-FN. EXIT.
       F02NA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0063
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0063
      *               *                                   *             CI0063
      *               *FIN DE TRAITEMENT                  *             CI0063
      *               *                                   *             CI0063
      *               *************************************.            CI0063
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0063
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *FIELD EDITS                        *
      *               *                                   *
      *               *************************************.
       F30.      IF    WZ63-CUPIQ = 'U'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F30-FN.
      *
      *********************************
      ** IF AN UPDATE IS BEING DONE   *
      ** ALL FIELDS MUST BE EDITTED   *
      ** TO ENSURE ITS VALID VALUES   *
      ** FOR THE RESPECTIVE ELEMENT   *
      *********************************
      *
      *N30BA.    NOTE *CX06 EDITS                         *.
       F30BA.                                                           lv10
      *KEY WILL BE VALIDATED BY ACCESS
      *N30BM.    NOTE *ENSURE NUMERIC PECKING ORDER       *.
       F30BM.    IF    WZ63-NPECK NOT NUMERIC                           lv15
                 NEXT SENTENCE ELSE GO TO     F30BM-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012670 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30BM-FN. EXIT.
       F30BA-FN. EXIT.
      *N30DA.    NOTE *CX12 EDITS                         *.
       F30DA.                                                           lv10
      *
      *KEY VALIDATED BY ACCESS ALSO
      *N30DC.    NOTE *PAYMENT TYPE SHOULD BE VALID       *.
       F30DC.    IF    WZ63-CPMTC NUMERIC                               lv15
                 AND   (WZ63-CPMTC = 00
                 OR    WZ63-CPMTC = 01)
                 NEXT SENTENCE ELSE GO TO     F30DC-FN.
      *********************************
      ** REGULAR (00) AND LOAN (01)   *
      ** ARE THE ONLY ONES ALLOWED    *
      *********************************
       F30DC-900. GO TO F30DE-FN.
       F30DC-FN. EXIT.
      *N30DE.    NOTE *ELSE... PAYMENT TYPE INCORRECT     *.
       F30DE.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012057 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DE-FN. EXIT.
      *N30DG.    NOTE *CHECK PAYMENT SEQ #                *.
       F30DG.    IF    WZ63-NAPDS NUMERIC                               lv15
                 AND   WZ63-NAPDS > 0
                 NEXT SENTENCE ELSE GO TO     F30DG-FN.
       F30DG-900. GO TO F30DI-FN.
       F30DG-FN. EXIT.
      *N30DI.    NOTE *ELSE... PAYMENT TYPE INCORRECT     *.
       F30DI.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012036 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DI-FN. EXIT.
      *N30DK.    NOTE *VALIDATE START DATE                *.
       F30DK.    IF    WZ63-GESTD NUMERIC                               lv15
                 AND   WZ63-GESTD > ZERO
                 NEXT SENTENCE ELSE GO TO     F30DK-FN.
      *********************************
      ** GESTD IS SET BY THE FIRST    *
      ** NEXT PAYMENT DATE SET ON     *
      ** BA WHEN IT IS SETUP.  THE    *
      ** NEXT PAYMENT DATE (DNPMT)    *
      ** IS NOT A NORMAL DATE - SEE   *
      ** DNPMT EDIT BELOW             *
      *********************************
           MOVE        WZ63-GESTD TO DD01-XDATG.
      *N30DM.    NOTE *IF CENTURY IS VALID                *.
       F30DM.    IF    DD01-XDAT1 NOT = 19                              lv20
                 AND   DD01-XDAT1 NOT = 20
                 NEXT SENTENCE ELSE GO TO     F30DM-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012537 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DM-FN. EXIT.
      *N30DO.    NOTE *IF MONTH IS NOT VALID              *.
       F30DO.    IF    DD01-XDAT3 NOT > ZERO                            lv20
                 OR    DD01-XDAT3 NOT < 13
                 NEXT SENTENCE ELSE GO TO     F30DO-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012537 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DO-FN. EXIT.
      *N30DQ.    NOTE *IF DAY IS NOT VALID                *.
       F30DQ.    IF    DD01-XDAT4 NOT > ZERO                            lv20
                 OR    DD01-XDAT4 NOT < 32
                 NEXT SENTENCE ELSE GO TO     F30DQ-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012537 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DQ-FN. EXIT.
       F30DK-900. GO TO F30DT-FN.
       F30DK-FN. EXIT.
      *N30DT.    NOTE *ELSE. START DATE IS BAD            *.
       F30DT.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012537 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DT-FN. EXIT.
      *N30EA.    NOTE *ENSURE END DATE IS NUMERIC         *.
       F30EA.    IF    WZ63-GEEND NOT NUMERIC                           lv15
                 NEXT SENTENCE ELSE GO TO     F30EA-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012205 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30EA-900. GO TO F30ED-FN.
       F30EA-FN. EXIT.
      *N30ED.    NOTE *ELSE... ENSURE A GOOD DATE         *.
       F30ED.         EXIT.                                             lv15
      *N30EG.    NOTE *IF END DATE IS ENTERED             *.
       F30EG.    IF    WZ63-GEEND > ZERO                                lv20
                 NEXT SENTENCE ELSE GO TO     F30EG-FN.
           MOVE        WZ63-GEEND TO DD01-XDATG
           PERFORM     F93DD THRU F93DD-FN.
      *N30EL.    NOTE *IF DATE INVALID; ERROR OUT         *.
       F30EL.    IF    DEL-ER NOT = 1                                   lv25
                 NEXT SENTENCE ELSE GO TO     F30EL-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012205 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30EL-FN. EXIT.
       F30EG-FN. EXIT.
       F30ED-FN. EXIT.
      *N30FA.    NOTE *EDIT IRREGULAR SCHEDULE FIELD      *.
       F30FA.                                                           lv15
           MOVE        WZ63-CIRMO TO W-WK00-CIRMO.
      *N30FF.    NOTE *ENSURE EACH POSITION IS CORRECT    *.
       F30FF.    IF    JAN                                              lv20
                 AND   FEB
                 AND   MAR
                 AND   APR
                 AND   MAY
                 AND   JUN
                 AND   JUL
                 AND   AUG
                 AND   SEP
                 AND   OCT
                 AND   NOV
                 AND   DEC
                 NEXT SENTENCE ELSE GO TO     F30FF-FN.
      ********************************
      **                             *
      ** CIRMO IS A 12 BYTE FIELD    *
      ** THAT HAS "XXXXXXXXXXXX" IN  *
      ** IT WHEN A NORMAL FREQUENCY  *
      ** IS USED.  IF AN IRREGULAR   *
      ** FREQUENCY IS USED, AT LEAST *
      ** ONE OF THESE CHARACTERS     *
      ** WILL BE SPACED OUT.  EACH   *
      ** POSITION REPRESENTS EACH    *
      ** MONTH                       *
      ********************************
       F30FF-900. GO TO F30FH-FN.
       F30FF-FN. EXIT.
      *N30FH.    NOTE *ELSE. ONE OF THE MONTHS IS WRONG   *.
       F30FH.                                                           lv20
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012206 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30FH-FN. EXIT.
      *N30FJ.    NOTE *COUNT NUMBER OF MONTHS MISSING     *.
       F30FJ.                                                           lv20
      *   - THIS COUNT WILL BE USED
      *     LATER
           INSPECT     WZ63-CIRMO TALLYING
                W-WK00-COUNT FOR ALL ' '.
       F30FJ-FN. EXIT.
       F30FA-FN. EXIT.
      *N30GA.    NOTE *IF STATUS CODE VALID               *.
       F30GA.    IF    WZ63-CDEST NUMERIC                               lv15
                 AND   WZ63-CDEST > ZERO
                 AND   WZ63-CDEST < 05
                 NEXT SENTENCE ELSE GO TO     F30GA-FN.
      *********************************
      **                              *
      ** VALID VALUES ARE 01-ACTIVE,  *
      ** 02-PENDING, 03-INACTIVE AND  *
      ** 04-FUTURE.                   *
      *********************************
       F30GA-900. GO TO F30GH-FN.
       F30GA-FN. EXIT.
      *N30GH.    NOTE *ELSE. STATUS CODE IS INCORRECT     *.
       F30GH.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012611 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30GH-FN. EXIT.
      *N30HA.    NOTE *VALIDATE AMOUNT                    *.
       F30HA.    IF    WZ63-APMTL NUMERIC                               lv15
                 NEXT SENTENCE ELSE GO TO     F30HA-FN.
      *N30HC.    NOTE *IS AMOUNT > ZERO OR OK 2B ZERO     *.
       F30HC.    IF    WZ63-APMTL > ZERO                                lv20
                 OR    (WZ63-APMTL = ZERO
                 AND   (WZ63-CACTA = 'I'
                 OR    WZ63-CACTA = 'D'))
                 NEXT SENTENCE ELSE GO TO     F30HC-FN.
      * - MUST BE GREATER THAN ZERO
      *   UNLESS ITS AN EXHAUSTED OD
      *   WHICH CAN BE DELETED
       F30HC-900. GO TO F30HJ-FN.
       F30HC-FN. EXIT.
      *N30HJ.    NOTE *ELSE. AMOUNT IS INCORRECT          *.
       F30HJ.                                                           lv20
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012055 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30HJ-FN. EXIT.
       F30HA-900. GO TO F30HT-FN.
       F30HA-FN. EXIT.
      *N30HT.    NOTE *ELSE. AMOUNT IS INCORRECT          *.
       F30HT.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012055 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30HT-FN. EXIT.
      *N30IA.    NOTE *VALIDATE NEXT PAYMENT DATE         *.
       F30IA.    IF    WZ63-DNPMT NUMERIC                               lv15
                 AND   WZ63-DNPMT > ZERO
                 NEXT SENTENCE ELSE GO TO     F30IA-FN.
      *********************************
      **                              *
      ** DNPMT IS A STRANGE DATE      *
      ** FIELD, IN THAT IS WILL ALLOW *
      ** NON STANDARD DATES SUCH AS   *
      ** 19960231 TO BE CONSIDERED    *
      ** VALID.  SINCE THE DAY OF     *
      ** MONTH IS USED MONTH TO MONTH *
      ** TO CALC THE NEXT PAYMENT.    *
      ** IT MAY CREATE DATES THAT ARE *
      ** INVALID, SO THIS EDIT IS     *
      ** DONE JUST TO ENSURE THE      *
      ** MONTH, DAY & CENTURY ARE     *
      ** VALID                        *
      **                              *
      *********************************
           MOVE        WZ63-DNPMT TO DD01-XDATG
           MOVE        DD01-XDAT4 TO W-WK00-NDAY.
      *N30ID.    NOTE *IF CENTURY IS VALID                *.
       F30ID.    IF    DD01-XDAT1 NOT = 19                              lv20
                 AND   DD01-XDAT1 NOT = 20
                 NEXT SENTENCE ELSE GO TO     F30ID-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012204 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30ID-FN. EXIT.
      *N30IH.    NOTE *IF MONTH IS NOT VALID              *.
       F30IH.    IF    DD01-XDAT3 NOT > ZERO                            lv20
                 OR    DD01-XDAT3 NOT < 13
                 NEXT SENTENCE ELSE GO TO     F30IH-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012204 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30IH-FN. EXIT.
      *N30IN.    NOTE *IF DAY IS NOT VALID                *.
       F30IN.    IF    DD01-XDAT4 NOT > ZERO                            lv20
                 OR    DD01-XDAT4 NOT < 32
                 NEXT SENTENCE ELSE GO TO     F30IN-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012204 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30IN-FN. EXIT.
       F30IA-900. GO TO F30IT-FN.
       F30IA-FN. EXIT.
      *N30IT.    NOTE *ELSE. DATE OF NEXT PAYMENT BAD     *.
       F30IT.         EXIT.                                             lv15
      *N30IV.    NOTE *IF NOT A DELETE                    *.
       F30IV.    IF    WZ63-CACTS NOT = 'D'                             lv20
                 NEXT SENTENCE ELSE GO TO     F30IV-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012204 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30IV-FN. EXIT.
       F30IT-FN. EXIT.
      *N30KA.    NOTE *VALIDATE IRA MONTH                 *.
       F30KA.    IF    WZ63-NIRACM NUMERIC                              lv15
                 AND   WZ63-NIRACM < 05
                 NEXT SENTENCE ELSE GO TO     F30KA-FN.
      *********************************
      **                              *
      ** THIS FIELD IS USED WHEN AN   *
      ** PAYMENT IS TO BE APPLIED TO  *
      ** A PRIOR YEAR IRA.  IT IS USED*
      ** ALONG WITH THE DAY AND       *
      ** CANNOT EXCEED APRIL 15TH     *
      *********************************
       F30KA-900. GO TO F30KH-FN.
       F30KA-FN. EXIT.
      *N30KH.    NOTE *ELSE... IRA MONTH IS INCORRECT     *.
       F30KH.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012629 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30KH-FN. EXIT.
      *N30KM.    NOTE *VALIDATE PAYMENT FREQUENCY         *.
       F30KM.    IF    WZ63-CPMTF NUMERIC                               lv15
                 NEXT SENTENCE ELSE GO TO     F30KM-FN.
      *********************************
      **                              *
      ** TA71 IS USED TO VALIDATE THE *
      ** FREQUENCY CODE               *
      *********************************
           MOVE        WZ63-CPMTF TO TA71-CPMTF
           PERFORM     F92TA THRU F92TA-FN.
       F30KM-900. GO TO F30KT-FN.
       F30KM-FN. EXIT.
      *N30KT.    NOTE *ELSE... FREQUENCY IS INCORRECT     *.
       F30KT.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012118 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30KT-FN. EXIT.
      *N30LA.    NOTE *VALIDATE PAYMENT ON DEMAND IND     *.
       F30LA.    IF    WZ63-IPODM = 'N'                                 lv15
                 OR    = 'Y'
                 NEXT SENTENCE ELSE GO TO     F30LA-FN.
      *********************************
      **                              *
      ** NOTE THIS FIELD IS NOT USED  *
      ** BY THE WORKSTATION, ALTHOUGH *
      ** IT IS NEEDED ON THE DATABASE *
      ** SO IT MUST BE VALIDATED      *
      *********************************
       F30LA-900. GO TO F30LT-FN.
       F30LA-FN. EXIT.
      *N30LT.    NOTE *ELSE... ON DEMAND IND INCORRECT    *.
       F30LT.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012630 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30LT-FN. EXIT.
      *N30MA.    NOTE *VALIDATE LAST UPDATE CODE          *.
       F30MA.    IF    WZ63-CLUPD NUMERIC                               lv15
                 NEXT SENTENCE ELSE GO TO     F30MA-FN.
      *********************************
      **                              *
      ** TA72 IS USED TO ENSURE THAT  *
      ** THE LAST UPDATE CODE IS      *
      ** CORRECT                      *
      *********************************
           MOVE        WZ63-CLUPD TO SA72-CLUPD
           PERFORM     F92SA THRU F92SA-FN.
       F30MA-900. GO TO F30MG-FN.
       F30MA-FN. EXIT.
      *N30MG.    NOTE *ELSE... LAST UPDATE CODE INVALID   *.
       F30MG.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012631 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30MG-FN. EXIT.
      *N30MM.    NOTE *VALIDATE LAST UPDATE DATE          *.
       F30MM.    IF    WZ63-DLAUP NUMERIC                               lv15
                 AND   WZ63-DLAUP > ZERO
                 NEXT SENTENCE ELSE GO TO     F30MM-FN.
           MOVE        WZ63-DLAUP TO DD01-XDATG
           PERFORM     F93DD THRU F93DD-FN.
      *N30MQ.    NOTE *IF DATE INVALID; ERROR OUT         *.
       F30MQ.    IF    DEL-ER NOT = 1                                   lv20
                 NEXT SENTENCE ELSE GO TO     F30MQ-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012632 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30MQ-FN. EXIT.
       F30MM-900. GO TO F30MT-FN.
       F30MM-FN. EXIT.
      *N30MT.    NOTE *ELSE... INVALID LAST UPDATE DATE   *.
       F30MT.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012632 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30MT-FN. EXIT.
      *N30NA.    NOTE *IF ITS NOT A DELETE; ERROR         *.
       F30NA.    IF    WZ63-CACTS NOT = 'D'                             lv15
                 NEXT SENTENCE ELSE GO TO     F30NA-FN.
      *N30ND.    NOTE *VALIDATE WHO REQUESTED CODE        *.
       F30ND.    IF    WZ63-CWRC NUMERIC                                lv20
                 AND   (WZ63-CWRC = 01
                 OR    WZ63-CWRC = 02
                 OR    WZ63-CWRC = 03
                 OR    WZ63-CWRC = 07
                 OR    WZ63-CWRC = 09)
                 NEXT SENTENCE ELSE GO TO     F30ND-FN.
      *VALID VALUES ARE:
      *   01 = CUS, 02 = DSO, 03 = HOF
      *   07 = REP & 09 = BNK
       F30ND-900. GO TO F30NK-FN.
       F30ND-FN. EXIT.
      *N30NK.    NOTE *ELSE.. WHO REQUESTED CDE INVALID   *.
       F30NK.                                                           lv20
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012045 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30NK-FN. EXIT.
      *N30OA.    NOTE *VALIDATE HOW CHANGED CODE          *.
       F30OA.    IF    WZ63-CHCR NUMERIC                                lv20
                 AND   (WZ63-CHCR = 02
                 OR    WZ63-CHCR = 03)
                 NEXT SENTENCE ELSE GO TO     F30OA-FN.
      *VALID VALUES ARE:
      *   02 = WRITTEN & 03 = TELEPHONE
       F30OA-900. GO TO F30OK-FN.
       F30OA-FN. EXIT.
      *N30OK.    NOTE *ELSE... HOW CHANGED CODE INVALID   *.
       F30OK.                                                           lv20
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012053 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30OK-FN. EXIT.
      *N30PA.    NOTE *VALIDATE OPERATOR ID               *.
       F30PA.    IF    WZ63-GEOPD2 > SPACES                             lv20
                 NEXT SENTENCE ELSE GO TO     F30PA-FN.
       F30PA-900. GO TO F30PT-FN.
       F30PA-FN. EXIT.
      *N30PT.    NOTE *ELSE... INVALID OPERATOR ID        *.
       F30PT.                                                           lv20
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012633 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30PT-FN. EXIT.
      *N30QA.    NOTE *VALIDATE UNIT ID                   *.
       F30QA.    IF    WZ63-GEAUN NUMERIC                               lv20
                 NEXT SENTENCE ELSE GO TO     F30QA-FN.
       F30QA-900. GO TO F30QT-FN.
       F30QA-FN. EXIT.
      *N30QT.    NOTE *ELSE... INVALID UNIT ID            *.
       F30QT.                                                           lv20
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012634 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30QT-FN. EXIT.
      *N30RA.    NOTE *VALIDATE PREVIOUS CHANGE DATE      *.
       F30RA.    IF    WZ63-DPCHD NUMERIC                               lv20
                 NEXT SENTENCE ELSE GO TO     F30RA-FN.
      *N30RD.    NOTE *IF PREVIOUS CHANGE DATE ENTERED    *.
       F30RD.    IF    WZ63-DPCHD > ZERO                                lv25
                 NEXT SENTENCE ELSE GO TO     F30RD-FN.
           MOVE        WZ63-DPCHD TO DD01-XDATG
           PERFORM     F93DD THRU F93DD-FN.
      *N30RN.    NOTE *IF DATE INVALID; ERROR OUT         *.
       F30RN.    IF    DEL-ER NOT = 1                                   lv30
                 NEXT SENTENCE ELSE GO TO     F30RN-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012636 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30RN-FN. EXIT.
       F30RD-FN. EXIT.
       F30RA-900. GO TO F30RT-FN.
       F30RA-FN. EXIT.
      *N30RT.    NOTE *ELSE... INVALID PREV CHANGE DATE   *.
       F30RT.                                                           lv20
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012635 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30RT-FN. EXIT.
      *N30SA.    NOTE *VALIDATE NEXT EXECUTION DATE       *.
       F30SA.    IF    WZ63-DNEXE NUMERIC                               lv20
                 AND   WZ63-DNEXE > ZERO
                 NEXT SENTENCE ELSE GO TO     F30SA-FN.
           MOVE        WZ63-DNEXE TO DD01-XDATG
           PERFORM     F93DD THRU F93DD-FN.
      *N30SN.    NOTE *IF DATE INVALID; ERROR OUT         *.
       F30SN.    IF    DEL-ER NOT = 1                                   lv25
                 NEXT SENTENCE ELSE GO TO     F30SN-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012636 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30SN-FN. EXIT.
       F30SA-900. GO TO F30SP-FN.
       F30SA-FN. EXIT.
      *N30SP.    NOTE *ELSE....                           *.
       F30SP.         EXIT.                                             lv20
      *N30ST.    NOTE *ONLY ERROR IF ADD OR CHANGE        *.
       F30ST.    IF    WZ63-CACTA = 'A'                                 lv25
                 OR    WZ63-CACTA = 'C'
                 NEXT SENTENCE ELSE GO TO     F30ST-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012636 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30ST-FN. EXIT.
       F30SP-FN. EXIT.
       F30NA-FN. EXIT.
      *N30TA.    NOTE *BROKERAGE SUB ACCT S/B SPACES      *.
       F30TA.    IF    WZ63-CCSMQ = SPACE                               lv15
                 NEXT SENTENCE ELSE GO TO     F30TA-FN.
      *
      *********************************
      **                              *
      ** THIS IS A BROKERAGE FIELD    *
      ** AND SHOULD ALWAYS BE BLANK   *
      ** SINCE BROKERAGE CANNOT BE    *
      ** UPDATED BY THIS MODULE       *
      **                              *
      *********************************
       F30TA-900. GO TO F30TT-FN.
       F30TA-FN. EXIT.
      *N30TT.    NOTE *ELSE. ERROR; NON BROKERAGE ACCT    *.
       F30TT.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012637 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30TT-FN. EXIT.
      *N30UA.    NOTE *BROKERAGE CUSIP MUST BE SPACES     *.
       F30UA.    IF    WZ63-GCUSPZ = SPACE                              lv15
                 NEXT SENTENCE ELSE GO TO     F30UA-FN.
      *
      *********************************
      **                              *
      ** THIS IS A BROKERAGE FIELD    *
      ** AND SHOULD ALWAYS BE BLANK   *
      ** SINCE BROKERAGE CANNOT BE    *
      ** UPDATED BY THIS MODULE       *
      **                              *
      *********************************
       F30UA-900. GO TO F30UT-FN.
       F30UA-FN. EXIT.
      *N30UT.    NOTE *ELSE. CUSIP ONLY ON BROKERAGE      *.
       F30UT.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012638 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30UT-FN. EXIT.
       F30DA-FN. EXIT.
       F30-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *SEGMENT ACCESSES                   *
      *               *                                   *
      *               *************************************.
       F35.                                                             lv05
      *
      *********************************
      ** DO ALL THE ACCESSES NEEDED   *
      ** FOR PROCESSING               *
      *********************************
      *
      *N35BA.    NOTE *SET UP INDICATORS                  *.
       F35BA.                                                           lv10
           MOVE        'N' TO W-UL-XSW1
           W-LIFE-TR-XSW1
           W-TSA-XSW1
           MOVE        CT01-PRSCD TO W-WK00-PRSCD
           MOVE        ZERO TO W-LOAN-XCOUNT
           W-REGL-XCOUNT
           MOVE        CT01-DIRAC TO W-WK00-DIRAC.
       F35BA-FN. EXIT.
      *N35CA.    NOTE *IF LIFE PRODUCT                    *.
       F35CA.    IF    CT01-CTIDA = 004                                 lv10
                 OR    CT01-CTIDA = 005
                 NEXT SENTENCE ELSE GO TO     F35CA-FN.
      *N35CC.    NOTE *IF TRADITIONAL LIFE                *.
       F35CC.    IF    CT01-PRCOD < 00199                               lv15
                 OR    (CT01-PRCOD > 00299
                 AND   CT01-PRCOD < 00599)
                 NEXT SENTENCE ELSE GO TO     F35CC-FN.
           MOVE        'Y' TO W-LIFE-TR-XSW1
           MOVE        'T' TO WZ63-CFLOW.
       F35CC-FN. EXIT.
      *N35CE.    NOTE *IF UL PRODUCT                      *.
       F35CE.    IF    CT01-PRCOD > 00199                               lv15
                 AND   CT01-PRCOD < 00261
                 NEXT SENTENCE ELSE GO TO     F35CE-FN.
           MOVE        'Y' TO W-UL-XSW1
           MOVE        'U' TO WZ63-CFLOW.
       F35CE-FN. EXIT.
      *N35CH.    NOTE *IF I-CODE INDICATES TSA PRODUCT    *.
       F35CH.    IF    W-WK00-I-CODE = '2'                              lv15
                 OR    W-WK00-I-CODE = '3'
                 NEXT SENTENCE ELSE GO TO     F35CH-FN.
           MOVE        'Y' TO W-TSA-XSW1.
       F35CH-FN. EXIT.
       F35CA-FN. EXIT.
      *N35GA.    NOTE *ACCESS MISC PRODUCT RULES (TA5B)   *.
       F35GA.                                                           lv10
      *********************************
      **                              *
      ** TA5B IS USED BY THIS PROGRAM *
      ** TO DETERMINE IF A FUND IS    *
      ** TERMINATED OR NOT            *
      *********************************
           MOVE        '1' TO CA5B-CF
           MOVE        CT01-CTIDA TO CA5B-CTIDA
           MOVE        CT01-PRCOD TO CA5B-PRCOD
           MOVE        SPACES TO CA5B-PRSCD
           PERFORM     F92CA THRU F92CA-FN.
      *N35GG.    NOTE *IF TA5B INDICATES TSA PRODUCT      *.
       F35GG.    IF    CA5B-IVANT = 'Y'                                 lv15
                 AND   (CT01-CQACT = 002 OR 003)
                 NEXT SENTENCE ELSE GO TO     F35GG-FN.
           MOVE        'Y' TO W-TSA-XSW1.
       F35GG-FN. EXIT.
       F35GA-FN. EXIT.
      *N35HA.    NOTE *ACCESS THE BA RULES       (TA98)   *.
       F35HA.                                                           lv10
      *********************************
      **                              *
      ** TA98 IS THE MAIN PRODUCT     *
      ** LEVEL TABLE THAT CONTROLS    *
      ** WHETHER OR NOT A PRODUCT     *
      ** CAN HAVE A BA OR NOT AND     *
      ** WHAT FREQUENCIES ARE ALLOWED *
      *********************************
           MOVE        CT01-CTIDA TO BA98-CTIDA
           MOVE        CT01-PRCOD TO BA98-PRCOD
           MOVE        WZ63-CARTY TO BA98-CARTY
           PERFORM     F92BA THRU F92BA-FN.
      *N35HD.    NOTE *IF LIFE PRODUCT                    *.
       F35HD.    IF    (CT01-CTIDA = 004                                lv15
                 OR    CT01-CTIDA = 005)
                 AND   CT01-PRCOD = 00710
                 NEXT SENTENCE ELSE GO TO     F35HD-FN.
      *********************************
      ** THIS PRODUCT IS CURRENTLY    *
      ** NOT ALLOWED ON TA98          *
      *********************************
      *N35HF.    NOTE *IF SUBPRODUCT ALLOWS BA"S          *.
       F35HF.    IF    (CT01-CTIDA = 004                                lv20
                 AND   (W-FGHIJKL-CODE = '1100070'
                 OR    W-FGHIJKL-CODE = '2100070'
                 OR    W-FGHIJKL-CODE = '1100870'))
                 OR    (CT01-CTIDA = 005
                 AND   W-FGHIJKL-CODE = '2100070')
                 NEXT SENTENCE ELSE GO TO     F35HF-FN.
      ********************************
      **                             *
      ** EVEN THOUGH TA98 DOES NOT   *
      ** ALLOW, THERE ARE A FEW      *
      ** SUB-PRODUCTS THAT ARE STILL *
      ** ALLOWED BASED ON FGHI CODES *
      ** AS LONG AS THE FREQUENCY IS *
      ** NOT AN END OF TERM          *
      ********************************
      *N35HH.    NOTE *IF NOT ET UPDATE PROCESS           *.
       F35HH.    IF    WZ63-CUPIQ NOT = 'U'                             lv25
                 AND   WZ63-CPMTF NOT = 98
                 NEXT SENTENCE ELSE GO TO     F35HH-FN.
           MOVE        'Y' TO BA98-IARTYA
           BA98-IARLNA
           BA98-IFQAN
           BA98-IFQSA
           BA98-IFQFM
           BA98-IFQQT
           BA98-IFQBM
           BA98-IFQMO
           BA98-IFQBF
           BA98-IFQSM
           BA98-IFQBW
           BA98-IFQWK
           BA98-IFQIR
           BA98-IFQIF
           BA98-IFQIS
           BA98-IFQIK
           BA98-IFQIW
           BA98-IFQOD
           BA98-IARRGA.
       F35HH-FN. EXIT.
       F35HF-FN. EXIT.
       F35HD-FN. EXIT.
      *N35HJ.    NOTE *IF FINANCIAL PLANNING ACCOUNTS     *.
       F35HJ.    IF    CT01-CTIDA = 013                                 lv15
                 NEXT SENTENCE ELSE GO TO     F35HJ-FN.
      *FOR FP ACCOUNT REG ALLOW
      *INDICATOR OF TA98 IS INCORRECT,
      *ARR TYPE ALLOWED INDICATOR IS
      *USED TO POPULATE SWITCHES
           MOVE        BA98-IARTYA TO BA98-IARRGA
      *SET THE VALID FREQUENCIES AS
      *SAME AS CLIENT VIEWER FP BA
           MOVE        'Y' TO BA98-IFQAN
           BA98-IFQSA
           BA98-IFQQT
           BA98-IFQBM
           BA98-IFQMO
           BA98-IFQSM
           BA98-IFQBW
           BA98-IFQWK
           BA98-IFQOD
           MOVE        'N' TO BA98-IFQFM
           BA98-IFQBF
           BA98-IFQIR
           BA98-IFQIF
           BA98-IFQIS
           BA98-IFQIK
           BA98-IFQIW.
       F35HJ-FN. EXIT.
      *N35HM.    NOTE *MOVE WZ63 VALUES TO LINKAGE        *.
       F35HM.                                                           lv15
           MOVE        BA98-IARTYA TO WZ63-IARTYA
           MOVE        BA98-IARLNA TO WZ63-IARLNA
           MOVE        BA98-IFQAN TO WZ63-IFQAN
           MOVE        BA98-IFQSA TO WZ63-IFQSA
           MOVE        BA98-IFQFM TO WZ63-IFQFM
           MOVE        BA98-IFQQT TO WZ63-IFQQT
           MOVE        BA98-IFQBM TO WZ63-IFQBM
           MOVE        BA98-IFQMO TO WZ63-IFQMO
           MOVE        BA98-IFQBF TO WZ63-IFQBF
           MOVE        BA98-IFQSM TO WZ63-IFQSM
           MOVE        BA98-IFQBW TO WZ63-IFQBW
           MOVE        BA98-IFQWK TO WZ63-IFQWK
           MOVE        BA98-IFQIR TO WZ63-IFQIR
           MOVE        BA98-IFQIF TO WZ63-IFQIF
           MOVE        BA98-IFQIS TO WZ63-IFQIS
           MOVE        BA98-IFQIK TO WZ63-IFQIK
           MOVE        BA98-IFQIW TO WZ63-IFQIW
           MOVE        BA98-IFQOD TO WZ63-IFQOD
           WZ63-IFQODL
           MOVE        BA98-IARPSA TO WZ63-IARPSA
           MOVE        BA98-IARRGA TO WZ63-IARRGA.
       F35HM-FN. EXIT.
       F35HA-FN. EXIT.
      *N35HO.    NOTE *ACCESS GROUP BILL FROM EODS IS     *.
       F35HO.    IF    W-LIFE-TR-XSW1 = 'Y'                             lv10
                 AND   WZ63-IARRGA = 'Y'
                 NEXT SENTENCE ELSE GO TO     F35HO-FN.
      *REGULAR BA IS BE ALLOWED FOR
      *TRADITIONAL LIFE ACCOUNTS
      *
      *N35HP.    NOTE *CALL EODS VIA CALLING CI0975       *.
       F35HP.                                                           lv15
           INITIALIZE  DA8G
           DA8H
           MS03
           MOVE        'N' TO W-GROUP-BILL
           MOVE        14 TO DA8G-CARTY
           MOVE        CT01-CTID TO DA8H-CTID (1)
           MOVE        1 TO DA8H-QITEM
           MOVE        'RPC' TO DA8G-CSYS
           PERFORM     F91BA THRU F91BA-FN.
      *N35HQ.    NOTE *GET THE RESPONSE QUEUE FROM EODS   *.
       F35HQ.    IF    (DA8G-CERRE = SPACES                             lv20
                 OR    DA8G-CERRE = ZEROES)
                 AND   MS03-NMESS2 = ZEROES
                 AND   DA8G-GERTC = 'Y'
                 NEXT SENTENCE ELSE GO TO     F35HQ-FN.
      *SUCCESSFULLY
      *N35HR.    NOTE *IF CALL SERVICE SUCCESSFUL         *.
       F35HR.    IF    DA8G-CFAUL1 = SPACES                             lv25
                 AND   DA8G-TFACT1 = SPACES
                 NEXT SENTENCE ELSE GO TO     F35HR-FN.
      *
      *N35HS.    NOTE *IF GB ARRANGEMENTS ARE RETURNED    *.
       F35HS.    IF    CT01-CTID = DA8G-CTID (1)                        lv30
                 AND   DA8G-QITEM > 0
                 NEXT SENTENCE ELSE GO TO     F35HS-FN.
      *N35HT.    NOTE *LOOP UNTIL NO MORE ARRANGEMENT     *.
       F35HT.                                                           lv35
           MOVE        1                        TO J35HTR
                                    GO TO     F35HT-B.
       F35HT-A.
           ADD         1                        TO J35HTR.
       F35HT-B.
           IF          J35HTR                   >  DA8G-QITEM
                                    GO TO     F35HT-FN.
      *********************************
      *N35HV.    NOTE *SET THE REGULAR GROUP BILL ARR     *.
       F35HV.    IF    DA8G-CEBTP (J35HTR)                              lv40
                       NOT = '08'
                 NEXT SENTENCE ELSE GO TO     F35HV-FN.
      *INDICATOR AS 'Y'.
           MOVE        'Y' TO W-GROUP-BILL.
       F35HV-FN. EXIT.
       F35HT-900. GO TO F35HT-A.
       F35HT-FN. EXIT.
       F35HS-900. GO TO F35HW-FN.
       F35HS-FN. EXIT.
      *N35HW.    NOTE *NO GROUP BILL ARRANGEMENT RETURN   *.
       F35HW.                                                           lv30
      *DUE TO FOLLOW REASON:
                 IF    DA8G-CFAUL2 = '0102'                             DOT
                 OR    DA8G-CFAUL3 = '1000'
      *1. REQUEST ACCOUNT NOT FOUND OR
      *2. NO RECORDS FOUND
           MOVE        'N' TO W-GROUP-BILL
                 ELSE
      *EODS SERVICE ERROR
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015017 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35HW-FN. EXIT.
       F35HR-900. GO TO F35HX-FN.
       F35HR-FN. EXIT.
      *N35HX.    NOTE *CALL SERVICE FAILED                *.
       F35HX.                                                           lv25
      *POPULATED THE ERROR MESSAGE
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015005 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35HX-FN. EXIT.
       F35HQ-900. GO TO F35HZ-FN.
       F35HQ-FN. EXIT.
      *N35HZ.    NOTE *IF GET QUEUE FAILED                *.
       F35HZ.                                                           lv20
      *POPULATED THE ERROR MESSAGE
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015017 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35HZ-FN. EXIT.
       F35HP-FN. EXIT.
       F35HO-FN. EXIT.
      *N35IA.    NOTE *ACCESS ARRANGEMENT        (CX03)   *.
       F35IA.                                                           lv10
      *********************************
      **                              *
      ** THE FOLLOWING ACCESS KEYS    *
      ** ARE PASSED BY THE CALLING    *
      ** MODULE                       *
      *********************************
           INITIALIZE  CX06
           CX12
           WX06
           WX12
           MOVE        WZ63-CLID TO S-CXU01-CLID
           MOVE        WZ63-CARTY TO S-CXU03-CARTY
           MOVE        WZ63-NARRS TO S-CXU03-NARRS
           PERFORM     F94X3 THRU F94X3-FN.
       F35IA-FN. EXIT.
      *N35JA.    NOTE *CX03 FOUND; GET ACCOUNT (CX06)     *.
       F35JA.    IF    IK = '0'                                         lv10
                 NEXT SENTENCE ELSE GO TO     F35JA-FN.
      *N35JM.    NOTE *GN CX06                            *.
       F35JM.                                                           lv15
           MOVE        WZ63-CTID TO S-CXU06-CTID
           PERFORM     F94X6 THRU F94X6-FN.
       F35JM-FN. EXIT.
      *N35JP.    NOTE *CX06 FOUND                         *.
       F35JP.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F35JP-FN.
           MOVE        CX06 TO WX06.
      *N35JT.    NOTE *IF INQUIRY STORE THE ACCOUNT       *.
       F35JT.    IF    WZ63-CUPIQ = 'I'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F35JT-FN.
           MOVE        CX06 TO WZ63-CX06.
       F35JT-FN. EXIT.
       F35JP-FN. EXIT.
      *N35KA.    NOTE *GET THE BANK SEGMENT (GN CX18)     *.
       F35KA.                                                           lv15
           MOVE        CX03-NBASQ TO S-CXU18-NBASQ
           PERFORM     F94X8 THRU F94X8-FN.
      *N35KD.    NOTE *CX18 NOT FOUND                     *.
       F35KD.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F35KD-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012027 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35KD-FN. EXIT.
       F35KA-FN. EXIT.
      *N35LA.    NOTE *ACCESS THE ACTIVITY FOR ACCOUNT    *.
       F35LA.                                                           lv15
      *
      *********************************
      ** THIS ROUTINE WILL LOOP THRU  *
      ** THE CX12"S AND SUMMARIZE AND *
      ** DO STATISTICS ON THE CX12"S  *
      *********************************
      *
           PERFORM     F81 THRU F81-FN.
      *N35LD.    NOTE *IF DELETE... STORE NEXT TO LAST    *.
       F35LD.    IF    WZ63-CACTA = 'D'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F35LD-FN.
           MOVE        W-PREV-CPMTCX TO WZ63-CPMTCX
           MOVE        W-PREV-NAPDSK TO WZ63-NAPDSK
           MOVE        W-PREV-GESTD1 TO WZ63-GESTD1
           MOVE        W-PREV-DNPMT1 TO WZ63-DNPMT1.
       F35LD-FN. EXIT.
       F35LA-FN. EXIT.
      *N35MA.    NOTE *IF SEGMENT WAS FOUND; CONTINUE     *.
       F35MA.    IF    WX06-CTID = CT01-CTID                            lv15
                 AND   WX12-CPMTC = WZ63-CPMTC
                 AND   WX12-NAPDS = WZ63-NAPDS
                 AND   WX12-GESTD = WZ63-GESTD
                 AND   WZ63-GESTD > ZERO
                 NEXT SENTENCE ELSE GO TO     F35MA-FN.
      *  - IF CX06 KEYS AND CX12 KEYS
      *    ARE MATCHED WITH REQUESTED
      *    KEYS; THEN MATCH IS FOUND
      *N35MD.    NOTE *IF REQUESTING A CREATE; ERROR      *.
       F35MD.    IF    WZ63-CACTA = 'A'                                 lv20
                 OR    WZ63-CACTA = 'F'
                 NEXT SENTENCE ELSE GO TO     F35MD-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012639 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35MD-FN. EXIT.
      *N35MT.    NOTE *IF INQUIRY STORE THE ARRANGEMENT   *.
       F35MT.    IF    WZ63-CUPIQ = 'I'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F35MT-FN.
           MOVE        WX12 TO WZ63-CX12.
       F35MT-FN. EXIT.
      *N35NA.    NOTE *IF DELETE ALLOWED; SET IND         *.
       F35NA.    IF    WX12-CDEST = 03                                  lv20
                 OR    WX12-NAPDS = 99
                 OR    (WX12-CDEST = 04
                 AND   WX12-GESTD NOT <
                       WZ63-GESTD1)
                 NEXT SENTENCE ELSE GO TO     F35NA-FN.
      *********************************
      **                              *
      ** ONLY INACTIVE, FUTURE AND    *
      ** ON DEMANDS CAN BE DELETED    *
      *********************************
           MOVE        'Y' TO WZ63-IDELT.
       F35NA-900. GO TO F35NM-FN.
       F35NA-FN. EXIT.
      *N35NM.    NOTE *ELSE....                           *.
       F35NM.         EXIT.                                             lv20
      *N35NP.    NOTE *IF FUTURE; MUST NOT BE FARTHEST    *.
       F35NP.    IF    WX12-CDEST = 04                                  lv25
                 NEXT SENTENCE ELSE GO TO     F35NP-FN.
      *NOT ALLOWED: BA IS NOT FURTHEST
      *FUTURE
           MOVE        '7B' TO WZ63-CERRBD.
       F35NP-900. GO TO F35NR-FN.
       F35NP-FN. EXIT.
      *N35NR.    NOTE *ELSE... MUST BE INACTIVE FIRST     *.
       F35NR.                                                           lv25
      *NOT ALLOWED: BA IS NOT INACTIVE
           MOVE        '7A' TO WZ63-CERRBD.
       F35NR-FN. EXIT.
       F35NM-FN. EXIT.
      *N35OA.    NOTE *IF STATUS CHG ALLOWED; SET IND     *.
       F35OA.    IF    WX12-CDEST NOT = 04                              lv20
                 AND   WX12-NAPDS NOT = 99
                 NEXT SENTENCE ELSE GO TO     F35OA-FN.
      *********************************
      **                              *
      ** ONLY ACTIVE, PENDING AND     *
      ** INACTIVE ACCOUNTS CAN HAVE   *
      ** THE STATUS CHANGED           *
      *********************************
           MOVE        'Y' TO WZ63-ISTAT.
      *N35OD.    NOTE *IF LIFE PRODUCT                    *.
       F35OD.    IF    (CT01-CTIDA = 004                                lv25
                 OR    CT01-CTIDA = 005)
                 AND   W-UL-XSW1 = 'N'
                 AND   W-TSA-XSW1 = 'N'
                 NEXT SENTENCE ELSE GO TO     F35OD-FN.
      *N35OF.    NOTE *IF LOAN STILL EXISTS               *.
       F35OF.    IF    WX12-CPMTC NOT = 01                              lv30
                 AND   W-LOAN-XCOUNT > ZERO
                 NEXT SENTENCE ELSE GO TO     F35OF-FN.
           MOVE        '6A' TO WZ63-CERRBS
           MOVE        'N' TO WZ63-ISTAT.
       F35OF-FN. EXIT.
       F35OD-FN. EXIT.
       F35OA-900. GO TO F35OM-FN.
       F35OA-FN. EXIT.
      *N35OM.    NOTE *ELSE....                           *.
       F35OM.         EXIT.                                             lv20
      *N35OP.    NOTE *IF FUTURE CANNOT CHANGE STATUS     *.
       F35OP.    IF    WX12-CDEST = 04                                  lv25
                 NEXT SENTENCE ELSE GO TO     F35OP-FN.
           MOVE        '6B' TO WZ63-CERRBS.
       F35OP-FN. EXIT.
      *N35OR.    NOTE *IF ON DEMAND CANNOT CHANGE STAT    *.
       F35OR.    IF    WX12-NAPDS = 99                                  lv25
                 NEXT SENTENCE ELSE GO TO     F35OR-FN.
      *N35OT.    NOTE *IF ACTIVE; ALLOW TO INACTIVATE     *.
       F35OT.    IF    WX12-CDEST = 01                                  lv30
                 NEXT SENTENCE ELSE GO TO     F35OT-FN.
           MOVE        'Y' TO WZ63-ISTAT.
       F35OT-900. GO TO F35OV-FN.
       F35OT-FN. EXIT.
      *N35OV.    NOTE *ELSE; MUST DELETE                  *.
       F35OV.                                                           lv30
           MOVE        '6C' TO WZ63-CERRBS.
       F35OV-FN. EXIT.
       F35OR-FN. EXIT.
       F35OM-FN. EXIT.
      *N35PA.    NOTE *IF CHANGE ALLOWED; SET IND         *.
       F35PA.    IF    WX12-CDEST NOT = 03                              lv20
                 NEXT SENTENCE ELSE GO TO     F35PA-FN.
      *
      *********************************
      **                              *
      ** IF THE ACCOUNT IS INACTIVE   *
      ** DO NOT ALLOW IT TO BE        *
      ** CHANGED.  FUTURES CANNOT HAVE*
      ** THEIR DATA CHANGED...        *
      ** ALTHOUGH YOU CAN GO TO THE   *
      ** CHANGE SCREEN AND ADD ANOTHER*
      ** FUTURE                       *
      *********************************
           MOVE        'Y' TO WZ63-IACHI.
      *N35PD.    NOTE *IF NOT FARTHEST FUTURE             *.
       F35PD.    IF    WX12-CDEST = 04                                  lv25
                 AND   WX12-GESTD < WZ63-GESTD1
                 NEXT SENTENCE ELSE GO TO     F35PD-FN.
      *    - RESET BACK TO NO CHANGE
           MOVE        'N' TO WZ63-IACHI
           MOVE        '5B' TO WZ63-CERRBC.
       F35PD-FN. EXIT.
       F35PA-900. GO TO F35PM-FN.
       F35PA-FN. EXIT.
      *N35PM.    NOTE *ELSE.... CANNOT CHANGE INACTIVE    *.
       F35PM.                                                           lv20
           MOVE        '5A' TO WZ63-CERRBC.
       F35PM-FN. EXIT.
       F35MA-900. GO TO F35RA-FN.
       F35MA-FN. EXIT.
      *N35RA.    NOTE *ELSE.. SEGMENT NOT FOUND           *.
       F35RA.         EXIT.                                             lv15
      *N35RD.    NOTE *IF THIS IS MODIFY OR DELETE        *.
       F35RD.    IF    WZ63-CACTA = 'C'                                 lv20
                 OR    WZ63-CACTA = 'D'
                 OR    WZ63-CACTA = 'I'
                 OR    WZ63-CACTA = 'R'
                 NEXT SENTENCE ELSE GO TO     F35RD-FN.
      * - RECORD SHOULD HAVE BEEN FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012640 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35RD-FN. EXIT.
       F35RA-FN. EXIT.
       F35JA-900. GO TO F35UA-FN.
       F35JA-FN. EXIT.
      *N35UA.    NOTE *ELSE... CX03 WAS NOT FOUND         *.
       F35UA.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012007 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35UA-FN. EXIT.
      *N35VA.    NOTE *GET TAXPAYER DATE OF BIRTH         *.
       F35VA.    IF    CT01-CTIDA NOT = 013                             lv10
                 NEXT SENTENCE ELSE GO TO     F35VA-FN.
      *EXCEPT FOR FP ACCOUNTS AS THERE
      *IS NO TAXPAYER EXISTING FOR
      *FINANCIAL PLANNING ACCOUNTS
      *ALSO PERFORM TWO EDITS:
      *1)  ERROR IF ARRANGEMENT CLIENT
      *    ID EQUALS TAXPAYER CLIENT ID
      *2)  ERROR IF TAXPAYER AGE >= 18
      *    IF ED IRA
      *--------------------------------
      *TO PERFORM THESE EDITS:
      *1)  CALL MODULE CI0018 TO RETRV
      *    TAXPAYER CLIENT ID
      *2)  USING TAXPAYER CLIENT ID,
      *    READ CL01/CL03 TO GET B-DAY
      *3)  PERFORM MACRO AAOAG3 TO
      *    CALCULATE AGE IF ED IRA
      *N35WA.    NOTE *CALL CI0018 - ACCT CLIENTS         *.            AM0018
       F35WA.                                                           lv15
      *                                                                 AM0018
      *********************************                                 AM0018
      ** THIS MODULE WILL READ THE    *                                 AM0018
      ** CONTRACT DATABASE TO GET THE *                                 AM0018
      ** TAXPAYER CLIENT ID AND OWNER *                                 AM0018
      ** CLIENT ID'S ASSOCIATED WITH  *                                 AM0018
      ** THE ACCOUNT NUMBER.          *                                 AM0018
      *********************************                                 AM0018
      *                                                                 AM0018
           INITIALIZE      AC14                                         AM0018
           MOVE        CT01-CTID TO AC14-CTID                           AM0018
           MOVE        WZ63-DCACG TO AC14-DCACG                         AM0018
           MOVE        25 TO AC14-XIMAX                                 AM0018
           MOVE        'Y' TO AC14-IPOCH                                AM0018
           SET CI0018C-PCB-CT1P-PTR1 TO                                 AM0018
                       PCB-CT1P-PTR1                                    AM0018
           INITIALIZE      DE10-DU03                                    AM0018
           CALL        CI0018 USING                                     AM0018
           DFHEIBLK                                                     AM0018
           DFHCOMMAREA                                                  AM0018
           DLIUIBII                                                     AM0018
           CI0018C-PCB-ADDRESS-LIST                                     AM0018
           AC14                                                         AM0018
           DE10                                                         AM0018
           MS03.                                                        AM0018
       F35WA-FN. EXIT.
      *N35WD.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F35WD.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F35WD-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0018 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0018 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F35WD-900. GO TO F35WG-FN.
       F35WD-FN. EXIT.
      *N35WG.    NOTE *NO ERRORS                          *.            ADU071
       F35WG.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
      *N35WN.    NOTE *FIRST EDIT:                        *.
       F35WN.    IF    WZ63-CLID = AC14-CLID01                          lv20
                 AND   CT01-CIRAT = 007
                 NEXT SENTENCE ELSE GO TO     F35WN-FN.
      *1)  ERROR IF ARRANGEMENT CLIENT
      *    ID EQUALS TAXPAYER CLIENT ID
      *
      *---> Send ED IRA Message                                         ADU119
      *      and EXIT                                                   ADU119
           MOVE        013242 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35WN-FN. EXIT.
      *N35XC.    NOTE *GET THE TAXPAYER CLIENT            *.
       F35XC.                                                           lv20
           MOVE        AC14-CLID01 TO S-CLU01-CL01K
           PERFORM     F94CL THRU F94CL-FN.
       F35XC-FN. EXIT.
      *N35XE.    NOTE *SEND A MESSAGE AND END IF BAD      *.
       F35XE.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F35XE-FN.
      *---> Send NO CLIENT Message                                      ADU119
      *      and EXIT                                                   ADU119
           MOVE        012012 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35XE-FN. EXIT.
      *N35XG.    NOTE *IF PERSON SEGMENT; CONTINUE        *.
       F35XG.    IF    CL01-CLTYP = 'P'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F35XG-FN.
           PERFORM     F94CM THRU F94CM-FN.
      *N35XM.    NOTE *IF PERSON SEGMENT NOT FOUND        *.
       F35XM.    IF    IK = '1'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F35XM-FN.
      *---> Send NO CLIENT Message                                      ADU119
      *      and EXIT                                                   ADU119
           MOVE        012161 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35XM-900. GO TO F35XT-FN.
       F35XM-FN. EXIT.
      *N35XT.    NOTE *ELSE IT IS A PERSON                *.
       F35XT.         EXIT.                                             lv25
      *N35YD.    NOTE *DETR JULIAN BIRTH DATE IF ED IRA   *.
       F35YD.    IF    CT01-CIRAT = 007                                 lv30
                 NEXT SENTENCE ELSE GO TO     F35YD-FN.
           MOVE        CL03-CLDOB TO DD01-XDAGP
           MOVE        1 TO DD01-XDACT
           MOVE        1 TO DD01-XDACV
      *.
      *CALL MWS100EX - DYNAMIC                                          DOT
           CALL        MWS100EX USING DD01-DD05                         AADA58
           MOVE        DD01-XDAJP TO 7-OAGE-BIRTH-DATE.
       F35YD-FN. EXIT.
      *N35YF.    NOTE *DETR JULIAN CURR DATE IF ED IRA    *.
       F35YF.    IF    CT01-CIRAT = 007                                 lv30
                 NEXT SENTENCE ELSE GO TO     F35YF-FN.
           MOVE        WZ63-DCACG TO DD01-XDAGP
           MOVE        1 TO DD01-XDACT
           MOVE        1 TO DD01-XDACV
      *.
      *CALL MWS100EX - DYNAMIC                                          DOT
           CALL        MWS100EX USING DD01-DD05                         AADA58
           MOVE        DD01-XDAJP TO 7-OAGE-CURRENT-DATE.
       F35YF-FN. EXIT.
      *N35YL.    NOTE *PERFORM AGE CALC RUTN IF ED IRA    *.
       F35YL.    IF    CT01-CIRAT = 007                                 lv30
                 NEXT SENTENCE ELSE GO TO     F35YL-FN.
      *********************************
      *PASS MACRO AAOAG3 THE CLIENT'S
      *AGE AND THE CURRENT ACCOUNTING
      *DATE.  THE MACRO WILL RETURN THE
      *CLIENT'S AGE IN FIELD -->
      *      7-OAGE-AGE-YEARS
      *********************************
           PERFORM     F92BD THRU F92BD-FN.
       F35YL-FN. EXIT.
      *N35YN.    NOTE *SECOND EDIT:                       *.
       F35YN.    IF    CT01-CIRAT = 007                                 lv30
                 AND   7-OAGE-AGE-YRS NOT < 18
                 NEXT SENTENCE ELSE GO TO     F35YN-FN.
      *2)  ERROR IF TAXPAYER AGE >= 18
      *    ID EQUALS TAXPAYER CLIENT ID
      *    IF ED IRA
      *---> Send ED IRA Message                                         ADU119
      *      and EXIT                                                   ADU119
           MOVE        013243 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35YN-FN. EXIT.
       F35XT-FN. EXIT.
       F35XG-900. GO TO F35YR-FN.
       F35XG-FN. EXIT.
      *N35YR.    NOTE *IF ORGANIZATIONAL CLIENT           *.
       F35YR.                                                           lv20
           MOVE        ZEROES TO CL03-CLDOB.
       F35YR-FN. EXIT.
       F35WG-FN. EXIT.
       F35VA-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *GENERAL RULES                      *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *
      *********************************
      ** THIS ROUTINE WILL DO EDITS   *
      ** THAT ARE COMMON ACROSS       *
      ** PRODUCT LINES                *
      *********************************
      *
      *N40AB.    NOTE *CALL CI0020 - CAMS ACCTG DATE      *.            AM0020
       F40AB.                                                           lv10
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
       F40AB-FN. EXIT.
      *N40CA.    NOTE *IF TA98 STATES BA NOT ALLOWED      *.
       F40CA.    IF    WZ63-IARTYA NOT = 'Y'                            lv10
                 NEXT SENTENCE ELSE GO TO     F40CA-FN.
           MOVE        '1B' TO WZ63-CERRBB
           WZ63-CERRBC
           WZ63-CERRBS
           MOVE        'N' TO WZ63-IACHI
           WZ63-ISTAT.
       F40CA-900. GO TO F40CD-FN.
       F40CA-FN. EXIT.
      *N40CD.    NOTE *ELSE...                            *.
       F40CD.         EXIT.                                             lv10
      *N40CG.    NOTE *IF THIS ADMIN IS NOT ALLOWED       *.
       F40CG.    IF    CT01-CTIDA NOT = 001                             lv15
                 AND   CT01-CTIDA NOT = 002
                 AND   CT01-CTIDA NOT = 004
                 AND   CT01-CTIDA NOT = 005
                 AND   CT01-CTIDA NOT = 013
                 NEXT SENTENCE ELSE GO TO     F40CG-FN.
      *********************************
      **                              *
      **  THIS APPLICATION DOES NOT   *
      **  ALLOW PROCESSING PRODUCTS   *
      **  OTHER THAN WHAT IS LISTED   *
      **  HERE                        *
      *********************************
           MOVE        'N' TO WZ63-IARTYA
           MOVE        '1A' TO WZ63-CERRBB
           WZ63-CERRBC
           WZ63-CERRBD
           WZ63-CERRBS
           MOVE        'N' TO WZ63-IACHI
           WZ63-IDELT
           WZ63-ISTAT.
       F40CG-FN. EXIT.
      *N40CI.    NOTE *IF ITS AN ADD                      *.
       F40CI.    IF    WZ63-CACTA = 'A'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F40CI-FN.
      *N40CK.    NOTE *IF PRODUCT RESTRICTS TO ONE BA     *.
       F40CK.    IF    (CT01-CTIDA = 004                                lv20
                 OR    CT01-CTIDA = 005)
                 AND   (CT01-PRCOD = 00210
                 OR    CT01-PRCOD = 00220
                 OR    CT01-PRCOD = 00230
                 OR    CT01-PRCOD = 00240
                 OR    CT01-PRCOD = 00250)
                 NEXT SENTENCE ELSE GO TO     F40CK-FN.
      *N40CM.    NOTE *IF PRODUCT RESTRICTS TO ONE BA     *.
       F40CM.                                                           lv25
           MOVE        1                        TO J40CMR
                                    GO TO     F40CM-B.
       F40CM-A.
           ADD         1                        TO J40CMR.
       F40CM-B.
           IF          J40CMR                   >  IWS00M
                                    GO TO     F40CM-FN.
                 IF    WS00-NAPDS (J40CMR)                              DOT
                       NOT = J40CMR
           MOVE        J40CMR TO WS00-NAPDS (J40CMR)
           MOVE        IWS00M TO J40CMR.
       F40CM-900. GO TO F40CM-A.
       F40CM-FN. EXIT.
      *N40CN.    NOTE *IF PRODUCT ALLOWS CURRENTLY        *.
       F40CN.    IF    WZ63-IARTYA = 'Y'                                lv25
                 NEXT SENTENCE ELSE GO TO     F40CN-FN.
      *N40CO.    NOTE *SET THE ERROR CODE                 *.
       F40CO.                                                           lv30
           MOVE        1                        TO J40COR
                                    GO TO     F40CO-B.
       F40CO-A.
           ADD         1                        TO J40COR.
       F40CO-B.
           IF          J40COR                   >  IWS00L
                                    GO TO     F40CO-FN.
                 IF    J40COR = 1                                       DOT
           MOVE        ZERO TO WS00-COUNT.
                 IF    J40COR NOT = 99                                  DOT
                 AND   WS00-NAPDS (J40COR)
                       = J40COR
           ADD         1 TO WS00-COUNT.
       F40CO-900. GO TO F40CO-A.
       F40CO-FN. EXIT.
      *N40CP.    NOTE *SET THE ERROR CODE                 *.
       F40CP.    IF    WS00-COUNT = IWS00M                              lv27
                 NEXT SENTENCE ELSE GO TO     F40CP-FN.
           MOVE        'N' TO WZ63-IARTYA
           MOVE        '1G' TO WZ63-CERRBB.
       F40CP-FN. EXIT.
       F40CN-FN. EXIT.
       F40CK-FN. EXIT.
      *N40CV.    NOTE *IF ALL EXISTING BA + ONDEMAND      *.
       F40CV.    IF    WZ63-IARTYA = 'Y'                                lv20
                 NEXT SENTENCE ELSE GO TO     F40CV-FN.
      *N40CW.    NOTE *CHECK THE WS-TABLE                 *.
       F40CW.                                                           lv25
           MOVE        1                        TO J40CWR
                                    GO TO     F40CW-B.
       F40CW-A.
           ADD         1                        TO J40CWR.
       F40CW-B.
           IF          J40CWR                   >  IWS00L
                                    GO TO     F40CW-FN.
                 IF    J40CWR = 1                                       DOT
           MOVE        0 TO WS00-COUNT.
                 IF    WS00-NAPDS (J40CWR) > 0                          DOT
           ADD         1 TO WS00-COUNT.
       F40CW-900. GO TO F40CW-A.
       F40CW-FN. EXIT.
      *N40CX.    NOTE *CHECK THE COUNT                    *.
       F40CX.    IF    WS00-COUNT = IWS00M                              lv25
                 AND   W-WK99-NAPDS = 99
                 NEXT SENTENCE ELSE GO TO     F40CX-FN.
           MOVE        'N' TO WZ63-IARTYA
           MOVE        '1F' TO WZ63-CERRBB.
       F40CX-FN. EXIT.
       F40CV-FN. EXIT.
       F40CI-FN. EXIT.
      *N40DA.    NOTE *ACCT STATUS IS UNKNOWN,            *.
       F40DA.    IF    (CT01-CTSTA = 00                                 lv15
                 OR    CT01-CTSTA = 03
                 OR    CT01-CTSTA = 04)
                 AND   WZ63-IARTYA = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40DA-FN.
      *OR INACTIVE,
      *OR SPECIAL
      *(I.E. NOT ACTIVE/PENDING)
      *N40DC.    NOTE *IF INACTIVE FUND                   *.
       F40DC.    IF    (CT01-CTSTA = 03                                 lv20
                 AND   CT01-CTIDA = 002)
                 NEXT SENTENCE ELSE GO TO     F40DC-FN.
       F40DC-900. GO TO F40DE-FN.
       F40DC-FN. EXIT.
      *N40DE.    NOTE *ELSE... INACTIVE NOT ALLOWED       *.
       F40DE.                                                           lv20
           MOVE        'N' TO WZ63-IARTYA
           WZ63-ISTAT
           WZ63-IACHI
           MOVE        '1H' TO WZ63-CERRBB
           WZ63-CERRBC
           WZ63-CERRBS.
       F40DE-FN. EXIT.
       F40DA-FN. EXIT.
       F40CD-FN. EXIT.
      *N40EA.    NOTE *IF ADD SITUATION                   *.
       F40EA.    IF    WZ63-CACTA = 'A'                                 lv10
                 AND   WZ63-CUPIQ = 'I'
                 NEXT SENTENCE ELSE GO TO     F40EA-FN.
      *N40EH.    NOTE *SET NAPDS TO PROPER LEVEL          *.
       F40EH.                                                           lv15
           MOVE        1                        TO J40EHR
                                    GO TO     F40EH-B.
       F40EH-A.
           ADD         1                        TO J40EHR.
       F40EH-B.
           IF          J40EHR                   >  IWS00M
                                    GO TO     F40EH-FN.
                 IF    WS00-NAPDS (J40EHR) NOT =                        DOT
                       J40EHR
           MOVE        J40EHR TO WZ63-NAPDS
           MOVE        IWS00M TO J40EHR.
       F40EH-900. GO TO F40EH-A.
       F40EH-FN. EXIT.
       F40EA-FN. EXIT.
      *N40FA.    NOTE *IF TA98 STATES REGL NOT ALLOWED    *.
       F40FA.    IF    WZ63-IARRGA NOT = 'Y'                            lv10
                 NEXT SENTENCE ELSE GO TO     F40FA-FN.
      *
           MOVE        '2A' TO WZ63-CERRBR.
       F40FA-900. GO TO F40FD-FN.
       F40FA-FN. EXIT.
      *N40FD.    NOTE *ELSE...                            *.
       F40FD.         EXIT.                                             lv10
      *N40FG.    NOTE *IF ALL  EXISTING BA                *.
       F40FG.                                                           lv15
           MOVE        1                        TO J40FGR
                                    GO TO     F40FG-B.
       F40FG-A.
           ADD         1                        TO J40FGR.
       F40FG-B.
           IF          J40FGR                   >  IWS00M
                                    GO TO     F40FG-FN.
                 IF    J40FGR = 1                                       DOT
           MOVE        ZERO TO WS00-COUNT.
                 IF    WS00-NAPDS (J40FGR) =                            DOT
                       J40FGR
                 AND   WZ63-CACTA = 'A'
           ADD         1 TO WS00-COUNT.
       F40FG-900. GO TO F40FG-A.
       F40FG-FN. EXIT.
      *N40FH.    NOTE *IF ALL  EXISTING BA                *.
       F40FH.    IF    WS00-COUNT = IWS00M                              lv15
                 NEXT SENTENCE ELSE GO TO     F40FH-FN.
           MOVE        'N' TO WZ63-IARRGA
           MOVE        '2E' TO WZ63-CERRBR.
       F40FH-FN. EXIT.
       F40FD-FN. EXIT.
      *N40GA.    NOTE *IRA                                *.
       F40GA.    IF    CT01-CQACT = 001                                 lv10
                 NEXT SENTENCE ELSE GO TO     F40GA-FN.
      *SET INDICATOR TO ALLOW IRA MONTH
      *DROP DOWN LIST
           MOVE        'Y' TO WZ63-CIRAD.
      *N40GB.    NOTE *DETERMINE THE TAXPAYER AGE         *.
       F40GB.    IF    CT01-CIRAT NOT = 003                             lv15
                 AND   CT01-CIRAT NOT = 004
                 AND   CT01-CTIDA NOT = 013
                 NEXT SENTENCE ELSE GO TO     F40GB-FN.
      *AND THE CONTRIBUTION LIMIT
      *EXCEPT FOR FP ACCOUNTS AS THERE
      *IS NO TAXPAYER EXISTING FOR
      *FINANCIAL PLANNING ACCOUNTS
           PERFORM     F98BB THRU F98BB-FN
           MOVE        K910-ACLIM TO WZ63-AMAXA
           WZ63-AMAXAO.
       F40GB-FN. EXIT.
      *N40GL.    NOTE *SRA                                *.
       F40GL.    IF    CT01-CIRAT = 004                                 lv15
                 NEXT SENTENCE ELSE GO TO     F40GL-FN.
      *SRA CANNOT HAVE PRIOR YEAR CONTR
           MOVE        'N' TO WZ63-CIRAD.
       F40GL-FN. EXIT.
      *N40GN.    NOTE *SEP                                *.
       F40GN.    IF    CT01-CIRAT = 003                                 lv15
                 NEXT SENTENCE ELSE GO TO     F40GN-FN.
      *SEP CANNOT HAVE PRIOR YEAR CONTR
           MOVE        'N' TO WZ63-CIRAD.
       F40GN-FN. EXIT.
      *N40GO.    NOTE *COVERDELL ESA                      *.
       F40GO.    IF    CT01-CIRAT = 007                                 lv15
                 AND   WZ63-DCACG < 20020501
                 NEXT SENTENCE ELSE GO TO     F40GO-FN.
      *CANNOT HAVE PRIOR YEAR CONTR
           MOVE        'N' TO WZ63-CIRAD.
       F40GO-FN. EXIT.
       F40GA-900. GO TO F40GS-FN.
       F40GA-FN. EXIT.
      *N40GS.    NOTE *ELSE.... REGULAR ACCOUNT           *.
       F40GS.         EXIT.                                             lv10
      *N40GV.    NOTE *IF LIFE PRODUCT SET MAX HIGH       *.
       F40GV.    IF    CT01-CTIDA = 004                                 lv15
                 OR    CT01-CTIDA = 005
                 NEXT SENTENCE ELSE GO TO     F40GV-FN.
           MOVE        +9999.99 TO WZ63-AMAXA.
       F40GV-FN. EXIT.
       F40GS-FN. EXIT.
      *N40HA.    NOTE *IF TA98 STATES LOAN NOT ALLOWED    *.
       F40HA.    IF    WZ63-IARLNA NOT = 'Y'                            lv10
                 NEXT SENTENCE ELSE GO TO     F40HA-FN.
           MOVE        '3A' TO WZ63-CERRBL
           WZ63-CERRBP
           MOVE        'N' TO WZ63-IFQODL.
       F40HA-900. GO TO F40HD-FN.
       F40HA-FN. EXIT.
      *N40HD.    NOTE *ELSE...                            *.
       F40HD.         EXIT.                                             lv10
      *N40HG.    NOTE *IF IRA; DO NOT ALLOW LOAN          *.
       F40HG.    IF    WZ63-CIRAD = 'Y'                                 lv15
                 AND   (WZ63-CACTA = 'A'
                 OR    WZ63-CACTA = 'C'
                 OR    WZ63-CACTA = 'R')
                 NEXT SENTENCE ELSE GO TO     F40HG-FN.
           MOVE        '3B' TO WZ63-CERRBL
           WZ63-CERRBP
           MOVE        'N' TO WZ63-IARLNA
           MOVE        'N' TO WZ63-IFQODL.
      *N40HM.    NOTE *IF IRA; DO NOT ALLOW CHANGE        *.
       F40HM.    IF    WZ63-CUPIQ = 'I'                                 lv20
                 AND   WZ63-CACTA = 'C'
                 AND   WZ63-CPMTC = 01
                 NEXT SENTENCE ELSE GO TO     F40HM-FN.
           MOVE        '3B' TO WZ63-CERRBC
           MOVE        'N' TO WZ63-IACHI.
       F40HM-FN. EXIT.
       F40HG-FN. EXIT.
       F40HD-FN. EXIT.
      *N40IA.    NOTE *IF TA98 STATES OD NOT ALLOWED      *.
       F40IA.    IF    WZ63-IFQOD NOT = 'Y'                             lv10
                 NEXT SENTENCE ELSE GO TO     F40IA-FN.
           MOVE        '4E' TO WZ63-CERRBO
           WZ63-CERRBP
           MOVE        'N' TO WZ63-IFQODL.
       F40IA-900. GO TO F40ID-FN.
       F40IA-FN. EXIT.
      *N40ID.    NOTE *ELSE...                            *.
       F40ID.         EXIT.                                             lv10
      *N40IG.    NOTE *IF SAVINGS; OD NOT ALLOWED         *.
       F40IG.    IF    CX18-CCBAT = 02                                  lv15
                 AND   (WZ63-CACTA = 'C' OR
                       WZ63-CACTA = 'A' OR
                       WZ63-CACTA = 'R')
                 AND   (CT01-CTIDA = 001 OR
                       CT01-CTIDA = 002 OR
                       CT01-CTIDA = 013 OR
                       ((CT01-CTIDA = 004 OR 005)
                       AND
                       (CT01-AYSID = 202 OR 205)))
                 NEXT SENTENCE ELSE GO TO     F40IG-FN.
      *
      * - CHANGE,
      *   ADD,
      *   REACTIVATE
      *
      * - NOT ALLOWED FOR FUND,
      *                   CERT,
      *   FINANCIAL PLANNING ACCOUNTS,
      *   OR LIFE PRODUCTS IN AGENCY
      *      202,205
           MOVE        '4B' TO WZ63-CERRBO
           WZ63-CERRBP
           MOVE        'N' TO WZ63-IFQOD
           MOVE        'N' TO WZ63-IFQODL.
      *N40IH.    NOTE *IF CHANGING OD AND SAVINGS         *.
       F40IH.    IF    WZ63-CUPIQ = 'I'                                 lv20
                 AND   WZ63-CACTA = 'C'
                 AND   WZ63-CPMTF = 99
                 NEXT SENTENCE ELSE GO TO     F40IH-FN.
           MOVE        'N' TO WZ63-IACHI
           MOVE        '4B' TO WZ63-CERRBC.
       F40IH-FN. EXIT.
       F40IG-900. GO TO F40II-FN.
       F40IG-FN. EXIT.
      *N40II.    NOTE *ELSE....                           *.
       F40II.         EXIT.                                             lv15
      *N40IM.    NOTE *IF ON HOLD OR INACTIVE; NO GOOD    *.
       F40IM.    IF    (CX03-CARST = 04                                 lv20
                 OR    CX03-CARST = 03)
                 AND   (WZ63-CACTA = 'C'
                 OR    WZ63-CACTA = 'A'
                 OR    WZ63-CACTA = 'R')
                 NEXT SENTENCE ELSE GO TO     F40IM-FN.
           MOVE        '4C' TO WZ63-CERRBO
           WZ63-CERRBP
           MOVE        'N' TO WZ63-IFQOD
           MOVE        'N' TO WZ63-IFQODL.
      *N40IN.    NOTE *IF CHANGING OD AND ON HOLD/INACT   *.
       F40IN.    IF    WZ63-CUPIQ = 'I'                                 lv25
                 AND   WZ63-CACTA = 'C'
                 AND   WZ63-CPMTF = 99
                 NEXT SENTENCE ELSE GO TO     F40IN-FN.
           MOVE        'N' TO WZ63-IACHI
           MOVE        '4C' TO WZ63-CERRBC.
       F40IN-FN. EXIT.
       F40IM-900. GO TO F40IO-FN.
       F40IM-FN. EXIT.
      *N40IO.    NOTE *ELSE....                           *.
       F40IO.         EXIT.                                             lv20
      *N40IQ.    NOTE *IF OD ALREADY EXISTS               *.
       F40IQ.    IF    W-WK99-NAPDS = 99                                lv25
                 AND   (WZ63-CACTA = 'A'
                 OR    (WZ63-CACTA = 'C'
                 AND   WZ63-NAPDS NOT = 99
                 AND   WZ63-CUPIQ = 'I'))
                 NEXT SENTENCE ELSE GO TO     F40IQ-FN.
      *IF ADD REQUEST OR A CHANGE OF
      * A NON-ONDEMAND AND MAX OD EXIST
           MOVE        '4A' TO WZ63-CERRBO
           WZ63-CERRBP
           MOVE        'N' TO WZ63-IFQOD
           MOVE        'N' TO WZ63-IFQODL.
       F40IQ-FN. EXIT.
       F40IO-FN. EXIT.
       F40II-FN. EXIT.
       F40ID-FN. EXIT.
      *N40LA.    NOTE *PROCESS EARLIEST/LATEST DATES      *.
       F40LA.         EXIT.                                             lv10
      *N40LD.    NOTE *IF ADDING ALLOW ONE MONTH BEFORE   *.
       F40LD.    IF    WZ63-CACTA = 'A'                                 lv15
                 AND   WZ63-IBDPR = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40LD-FN.
           MOVE        WZ63-DCACG TO DD01-XDATG
           SUBTRACT    1 FROM DD01-XDAT39.
      *N40LF.    NOTE *IF PREV YEAR                       *.
       F40LF.    IF    DD01-XDAT39 = ZERO                               lv20
                 NEXT SENTENCE ELSE GO TO     F40LF-FN.
           MOVE        12 TO DD01-XDAT3
           MOVE        DD01-XDATG TO W-FRMT-DNPMT
           SUBTRACT    1 FROM W-FRMT-CCYY
           MOVE        W-FRMT-DNPMT TO WZ63-DAUTB.
       F40LF-900. GO TO F40LH-FN.
       F40LF-FN. EXIT.
      *N40LH.    NOTE *ELSE.. CURRENT YEAR                *.
       F40LH.                                                           lv20
           MOVE        DD01-XDATG TO WZ63-DAUTB.
       F40LH-FN. EXIT.
       F40LD-900. GO TO F40LJ-FN.
       F40LD-FN. EXIT.
      *N40LJ.    NOTE *ELSE... NOT IN FLOW - USE TODAY    *.
       F40LJ.                                                           lv15
           MOVE        WZ63-DCACG TO WZ63-DAUTB.
       F40LJ-FN. EXIT.
      *N40LN.    NOTE *CALCULATE 10 MONTHS AFTER          *.
       F40LN.                                                           lv15
           MOVE        WZ63-DCACG TO DD01-XDATG
           ADD         10 TO DD01-XDAT39.
      *N40LR.    NOTE *IF NEXT YEAR; ADJUST MONTHS/YR     *.
       F40LR.    IF    DD01-XDAT39 > 12                                 lv20
                 NEXT SENTENCE ELSE GO TO     F40LR-FN.
           SUBTRACT    12 FROM DD01-XDAT39
           MOVE        DD01-XDATG TO W-FRMT-DNPMT
           ADD         1 TO W-FRMT-CCYY
           MOVE        W-FRMT-DNPMT TO WZ63-DENDD.
       F40LR-900. GO TO F40LU-FN.
       F40LR-FN. EXIT.
      *N40LU.    NOTE *ELSE.. CURRENT YEAR                *.
       F40LU.                                                           lv20
           MOVE        DD01-XDATG TO WZ63-DENDD.
       F40LU-FN. EXIT.
       F40LN-FN. EXIT.
       F40LA-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *CERTS EDITS                        *
      *               *                                   *
      *               *************************************.
       F45.      IF    CT01-CTIDA = 001                                 lv05
                 NEXT SENTENCE ELSE GO TO     F45-FN.
      *
      *********************************
      ** THIS ROUTINE WILL DO ALL THE *
      ** EDITS SPECIFIC TO CERTS      *
      *********************************
      *
      *N45CA.    NOTE *CHECK LOAN/SURRENDER INDICATORS    *.
       F45CA.    IF    WZ63-IARLNA = 'Y'                                lv10
                 OR    (WZ63-IARPSA = 'Y'
                 AND   WZ63-IARRGA = 'N')
                 NEXT SENTENCE ELSE GO TO     F45CA-FN.
           MOVE        CT01-CTIDND TO 7-CTID-REFORMAT
           MOVE        7-CM01-CEBAS TO 7-CM01-CEBASL
           MOVE        7-CM01-CEPRE TO S-CMU01-CEPRE
           MOVE        7-CM01-FORMAT TO S-CMU01-CEBAS
      **
           PERFORM     F94C1 THRU F94C1-FN.
      *N45DA.    NOTE *IF LOAN IS ALLOWED                 *.
       F45DA.    IF    WZ63-IARLNA = 'Y'                                lv15
                 NEXT SENTENCE ELSE GO TO     F45DA-FN.
      *N45DD.    NOTE *CHECK IF CM01 SEGMENT NOT FOUND    *.
       F45DD.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F45DD-FN.
      *********************************
      ** IF THE CM01 SEGMENT WAS NOT  *
      ** FOUND, SET THE LOAN          *
      ** REPAYMENT INDICATOR TO 'N'   *
      ** AND GO TO REGULAR PAYMENT    *
      ** EDIT.                        *
      *********************************
      *
           MOVE        'N' TO WZ63-IARLNA
           WZ63-IFQODL
           MOVE        '3C' TO WZ63-CERRBL
           WZ63-CERRBP.
       F45DD-900. GO TO F45DJ-FN.
       F45DD-FN. EXIT.
      *N45DJ.    NOTE *READ CM24 SEGMENT                  *.
       F45DJ.                                                           lv20
           PERFORM     F94C2 THRU F94C2-FN.
      *N45DP.    NOTE *CHECK IF CM24 SEGMENT NOT FOUND    *.
       F45DP.    IF    IK = '1'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F45DP-FN.
      *********************************
      ** IF THE CM24 SEGMENT WAS NOT  *
      ** FOUND, SET THE LOAN          *
      ** REPAYMENT INDICATOR TO 'N'   *
      ** AND GO TO REGULAR PAYMENT    *
      ** EDIT.                        *
      *********************************
      *
           MOVE        'N' TO WZ63-IARLNA
           WZ63-IFQODL
           MOVE        '3C' TO WZ63-CERRBL
           WZ63-CERRBP.
       F45DP-900. GO TO F45DR-FN.
       F45DP-FN. EXIT.
      *N45DR.    NOTE *CM24 FOUND                         *.
       F45DR.         EXIT.                                             lv25
      *N45DU.    NOTE *LOAN BALANCE MUST BE POSITIVE      *.
       F45DU.    IF    CM24-CELBL = ZEROS                               lv30
                 NEXT SENTENCE ELSE GO TO     F45DU-FN.
      *
      *********************************
      ** IF THE LOAN BALANCE AMOUNT ON*
      ** THE CM24 SEGMENT IS ZEROS,   *
      ** SET LOAN REPAYMENT INDICATOR *
      ** TO 'N' AND GO TO THE REGULAR *
      ** PAYMENT EDIT.                *
      *********************************
      *
           MOVE        'N' TO WZ63-IARLNA
           WZ63-IFQODL
           MOVE        '3C' TO WZ63-CERRBL
           WZ63-CERRBP.
       F45DU-FN. EXIT.
       F45DR-FN. EXIT.
       F45DJ-FN. EXIT.
       F45DA-FN. EXIT.
      *N45EA.    NOTE *IF PARTIAL SURRENDERS ALLOWED      *.
       F45EA.    IF    WZ63-IARPSA = 'Y'                                lv15
                 AND   WZ63-IARRGA = 'N'
                 NEXT SENTENCE ELSE GO TO     F45EA-FN.
      *N45ED.    NOTE *READ LAST CM54 SEGMENT             *.
       F45ED.                                                           lv20
           MOVE        'L----' TO S-CM54-CCOD
           PERFORM     F94C3 THRU F94C3-FN.
       F45ED-FN. EXIT.
      *N45EG.    NOTE *CHECK IF CM54 SEGMENT NOT FOUND    *.
       F45EG.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F45EG-FN.
      *********************************
      ** IF THE CM54 SEGMENT WAS NOT  *
      ** FOUND, SET THE REGULAR       *
      ** PAYMENT INDICATOR TO 'N' AND *
      ** RETURN TO THE CALLING MODULE.*
      *********************************
      *
           MOVE        'N' TO WZ63-IARPSA
           MOVE        '2D' TO WZ63-CERRBR.
       F45EG-900. GO TO F45EJ-FN.
       F45EG-FN. EXIT.
      *N45EJ.    NOTE *ELSE... LAST ACTIVITY FOUND        *.
       F45EJ.                                                           lv20
           MOVE        'N' TO W-SURR-XSW1.
      *N45EM.    NOTE *LOOP THRU CM54 SEGMENT             *.
       F45EM.                                                           lv25
           MOVE        1                        TO J45EMR
                                    GO TO     F45EM-B.
       F45EM-A.
           ADD         1                        TO J45EMR.
       F45EM-B.
           IF          J45EMR                   >  10
                                    GO TO     F45EM-FN.
      *N45ER.    NOTE *CHECK IF PARTIAL SURRENDER BAL     *.
       F45ER.    IF    CM54-CEPTA (J45EMR) > ZEROS                      lv30
                 NEXT SENTENCE ELSE GO TO     F45ER-FN.
      *
      *********************************
      ** IF IT IS A NON-ZERO PARTIAL  *
      ** SURRENDER BALANCE THEN ALLOW *
      *********************************
           MOVE        'Y' TO W-SURR-XSW1
               GO TO     F45EM-FN.
       F45ER-FN. EXIT.
       F45EM-900. GO TO F45EM-A.
       F45EM-FN. EXIT.
      *N45EU.    NOTE *IF REG NOT ALLOWED; NO PARTIAL     *.
       F45EU.    IF    W-SURR-XSW1 = 'N'                                lv25
                 AND   WZ63-CERRBR = SPACES
                 NEXT SENTENCE ELSE GO TO     F45EU-FN.
           MOVE        'N' TO WZ63-IARPSA
           MOVE        '2D' TO WZ63-CERRBR.
       F45EU-FN. EXIT.
      *N45EW.    NOTE *IF REG NOT ALLOWED; AND PARTIAL    *.
       F45EW.    IF    W-SURR-XSW1 = 'Y'                                lv25
                 AND   WZ63-IARRGA = 'N'
                 AND   WZ63-CERRBR = '2A'
                 NEXT SENTENCE ELSE GO TO     F45EW-FN.
      *IF REGULAR NOT ALLOWED BY TA98
      *AND PARTIAL SURRENDER EXISTS
      *SET UP REGULAR PAYMENT ALLOWED
      *
           MOVE        'Y' TO WZ63-IARRGA
           MOVE        SPACE TO WZ63-CERRBR.
       F45EW-FN. EXIT.
       F45EJ-FN. EXIT.
       F45EA-FN. EXIT.
       F45CA-FN. EXIT.
      *N45FA.    NOTE *IF OD ALLOWED BUT REGULAR NOT      *.
       F45FA.    IF    WZ63-IFQOD = 'Y'                                 lv10
                 AND   (WZ63-CERRBR = '2A'
                 OR    WZ63-CERRBR = '2D')
                 NEXT SENTENCE ELSE GO TO     F45FA-FN.
      *IF OD DEMAND IS ALLOWED BY
      *REGULAR IS NOT ALLOWED ON TA98
      *AND NOT SURRENDER INFO COULD BE
      *FOUND, THEN THIS IS AN ERROR
      *
           MOVE        'N' TO WZ63-IFQOD
           MOVE        '4G' TO WZ63-CERRBO.
       F45FA-FN. EXIT.
      *N45GA.    NOTE *DEFAULT MINIMUM AMOUNTS            *.
       F45GA.    IF    WZ63-CUPIQ = 'I'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F45GA-FN.
           MOVE        +50 TO WZ63-AMINA
           WZ63-AMNDL
           WZ63-ALMIN.
       F45GA-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *FUNDS EDITS                        *
      *               *                                   *
      *               *************************************.
       F50.      IF    CT01-CTIDA = 002                                 lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *
      *********************************
      ** THIS ROUTINE WILL DO ALL     *
      ** FUND SPECIFIC EDITS          *
      *********************************
      *
           MOVE        CT01-PRCOD TO WS00-PRCOD.
      *N50CA.    NOTE *IF BA'S ARE ALLOWED                *.
       F50CA.    IF    WZ63-IARTYA = 'Y'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50CA-FN.
      *N50CB.    NOTE *DISALLOW MUTUAL FUND CLASS B       *.
       F50CB.    IF    CT01-PRSCD = 000000002                           lv12
                 NEXT SENTENCE ELSE GO TO     F50CB-FN.
           MOVE        '1B' TO WZ63-CERRBB
           WZ63-CERRBC
           WZ63-CERRBS
           MOVE        'N' TO WZ63-IARTYA
           WZ63-ISTAT
           WZ63-IACHI.
       F50CB-900. GO TO F50CC-FN.
       F50CB-FN. EXIT.
      *N50CC.    NOTE *ELSE- CEHCK FOR OTHER CLASS        *.
       F50CC.         EXIT.                                             lv12
      *N50CD.    NOTE *IF PRODUCT THAT ALLOWS A SD        *.
       F50CD.    IF    CT01-PRCOD = 00016                               lv15
                 OR    ((CT01-PRCOD = 00013
                 OR    CT01-PRCOD = 00167)
                 AND   (CT01-PRSCD = 000000001
                 OR    CT01-PRSCD = SPACES))
                 NEXT SENTENCE ELSE GO TO     F50CD-FN.
      *********************************
      **                              *
      **                              *
      ** PRODUCT 16 IS A TAX FREE     *
      ** FUND AND 13 IS A CASH MGMT   *
      ** FUND OR RVS GOVT MONEY MKT   *
      ** FUND AND ALLOW SD ALONG WITH *
      ** BA"S AT THE SAME TIME        *
      *********************************
       F50CD-900. GO TO F50CE-FN.
       F50CD-FN. EXIT.
      *N50CE.    NOTE *ELSE... CHECK FOR EXISTING SD"S    *.
       F50CE.         EXIT.                                             lv15
      *N50CI.    NOTE *IF PRODUCT PROHIBITS BA"S & SD"S   *.
       F50CI.    IF    W-SD-XCOUNT > ZERO                               lv20
                 AND   W-ALLOW-BA = 'N'
                 NEXT SENTENCE ELSE GO TO     F50CI-FN.
           MOVE        '1E' TO WZ63-CERRBC
           MOVE        'N' TO WZ63-IACHI.
      *N50CM.    NOTE *IF INQUIRY; INDICATE ERROR         *.
       F50CM.    IF    WZ63-CUPIQ = 'I'                                 lv25
                 OR    (WZ63-CACTA = 'A'
                 OR    WZ63-CACTA = 'C'
                 OR    WZ63-CACTA = 'F'
                 OR    WZ63-CACTA = 'R')
                 NEXT SENTENCE ELSE GO TO     F50CM-FN.
      *- IF INQUIRY; SHOW THE "1E" AND
      *  THEN THE WORKSTATION WILL
      *  STILL ALLOW AN INACTIVE OR
      *  DELETE; THIS LOGIC WILL ALLOW
      *  DELETES AND INACTIVATE TO FLOW
      *  THROUGH WITHOUT ERROR IF IN
      *  UPDATE MODE.
           MOVE        '1E' TO WZ63-CERRBB
           MOVE        'N' TO WZ63-IARTYA.
       F50CM-FN. EXIT.
      *N50CV.    NOTE *IF INACTIVE; DON"T ALLOW STATUS    *.
       F50CV.    IF    CX12-CDEST = 03                                  lv25
                 NEXT SENTENCE ELSE GO TO     F50CV-FN.
      *
           MOVE        '1E' TO WZ63-CERRBS
           MOVE        'N' TO WZ63-ISTAT.
       F50CV-FN. EXIT.
       F50CI-FN. EXIT.
       F50CE-FN. EXIT.
      *N50DB.    NOTE *CLOSE SPECIFIED B-FUNDS            *.
       F50DB.    IF    CT01-PRSCD = '000000002'                         lv15
                 AND   CLOSE-B-SHARE
                 NEXT SENTENCE ELSE GO TO     F50DB-FN.
           MOVE        '1K' TO WZ63-CERRBB
           WZ63-CERRBC
           WZ63-CERRBS
           MOVE        'N' TO WZ63-IARTYA
           WZ63-ISTAT
           WZ63-IACHI.
       F50DB-FN. EXIT.
       F50CC-FN. EXIT.
       F50CA-FN. EXIT.
      *N50EA.    NOTE *SET MINIMUMS                       *.
       F50EA.         EXIT.                                             lv10
      *N50ED.    NOTE *QUALIFIED ACCOUNT                  *.
       F50ED.    IF    CT01-CQACT > ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F50ED-FN.
           MOVE        +50 TO WZ63-AMINA
           WZ63-AMNDL.
       F50ED-900. GO TO F50EM-FN.
       F50ED-FN. EXIT.
      *N50EM.    NOTE *NOT A QUALIFIED ACCOUNT            *.
       F50EM.                                                           lv15
           MOVE        +100 TO WZ63-AMINA
           WZ63-AMNDL.
       F50EM-FN. EXIT.
      *N50EP.    NOTE *EXCEPTION FOR FUND 102             *.
       F50EP.    IF    (CT01-PRCOD = 102 OR 106                         lv15
                       OR 108)
                 AND   (CT01-CTSTA = 01 OR 03)
                 NEXT SENTENCE ELSE GO TO     F50EP-FN.
      *OR 106 OR 108
      *PENDING OR INACTIVE
      *SET ONE-TIME AMT TO 5000
           MOVE        +5000 TO WZ63-AMNDL.
      *ALSO SET REG MIN AMINA.                                          DOT
      *UI EDITS AGAINST MC86-AMINA
      *(MSG 500044).
           MOVE        +5000 TO WZ63-AMINA.
       F50EP-FN. EXIT.
      *N50ET.    NOTE * EXCEPTION FOR FUND 107            *.
       F50ET.    IF    (CT01-PRCOD = 107 OR 124                         lv15
                       OR 125 OR 126)
                 AND   (CT01-CTSTA = 01 OR 03)
                 NEXT SENTENCE ELSE GO TO     F50ET-FN.
      * OR 124 OR 125 OR 126
      * PENDING OR INACTIVE
      * SET ONE-TIME AMT TO 10000
           MOVE        +10000 TO WZ63-AMNDL.
      * ALSO SET REG MIN AMINA.                                         DOT
      * UI EDITS AGAINST MC86-AMINA
      * (MSG 500044).
           MOVE        +10000 TO WZ63-AMINA.
       F50ET-FN. EXIT.
       F50EA-FN. EXIT.
      *N50FA.    NOTE *IF BA'S ARE STILL ALLOWED          *.
       F50FA.    IF    WZ63-IARTYA = 'Y'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50FA-FN.
      *N50FD.    NOTE *IF TERMINATED MUTUAL FUND          *.
       F50FD.    IF    CA5B-PRCAUT = 'T'                                lv15
                 NEXT SENTENCE ELSE GO TO     F50FD-FN.
           MOVE        '1C' TO WZ63-CERRBB
           WZ63-CERRBC
           WZ63-CERRBS
           MOVE        'N' TO WZ63-IARTYA
           WZ63-ISTAT
           WZ63-IACHI.
       F50FD-900. GO TO F50GA-FN.
       F50FD-FN. EXIT.
      *N50GA.    NOTE *ELSE...BA'S STILL ALLOWED          *.
       F50GA.         EXIT.                                             lv15
      *N50GD.    NOTE *IF CASH MANAGEMENT B ACCOUNT       *.
       F50GD.    IF    (CT01-PRCOD = 00013                              lv20
                 OR    CT01-PRCOD = 00167)
                 AND   CT01-PRSCD = 000000002
                 NEXT SENTENCE ELSE GO TO     F50GD-FN.
      *OR RVS GOVT MONEY MKT FD B ACCT
           MOVE        '1D' TO WZ63-CERRBB
           WZ63-CERRBC
           WZ63-CERRBS
           MOVE        'N' TO WZ63-IARTYA
           WZ63-ISTAT
           WZ63-IACHI.
       F50GD-FN. EXIT.
      *N50GF.    NOTE *IF CASH MANAGEMENT C ACCOUNT       *.
       F50GF.    IF    (CT01-PRCOD = 00013                              lv20
                 OR    CT01-PRCOD = 00167)
                 AND   CT01-PRSCD = 000000006
                 NEXT SENTENCE ELSE GO TO     F50GF-FN.
      *OR RVS GOVT MONEY MKT FD C ACCT
           MOVE        '1J' TO WZ63-CERRBB
           WZ63-CERRBC
           WZ63-CERRBS
           MOVE        'N' TO WZ63-IARTYA
           WZ63-ISTAT
           WZ63-IACHI.
       F50GF-FN. EXIT.
       F50GA-FN. EXIT.
       F50FA-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *LIFE EDITS                         *
      *               *                                   *
      *               *************************************.
       F55.      IF    CT01-CTIDA = 004                                 lv05
                 OR    CT01-CTIDA = 005
                 NEXT SENTENCE ELSE GO TO     F55-FN.
      *********************************
      ** THIS ROUTINE WILL DO ALL     *
      ** EDITS UNIQUE TO LIFE PRODUCTS*
      *********************************
      *
      *N55CA.    NOTE *IF UL PRODUCT                      *.
       F55CA.    IF    CT01-PRCOD > 00199                               lv10
                 AND   CT01-PRCOD < 00261
                 NEXT SENTENCE ELSE GO TO     F55CA-FN.
           MOVE        +20 TO WZ63-ALMIN
           WZ63-AMINA
           WZ63-AMNDL.
      *N55CC.    NOTE *IF VUL PRODUCT                     *.
       F55CC.    IF    CT01-PRCOD = 00201                               lv15
                 OR    CT01-PRCOD = 00202
                 OR    CT01-PRCOD = 00203
                 OR    CT01-PRCOD = 00221
                 OR    CT01-PRCOD = 00223
                 OR    CT01-PRCOD = 00228
                 OR    CT01-PRCOD = 00229
                 NEXT SENTENCE ELSE GO TO     F55CC-FN.
           MOVE        +25 TO WZ63-ALMIN
           WZ63-AMINA
           WZ63-AMNDL.
       F55CC-FN. EXIT.
       F55CA-900. GO TO F55CE-FN.
       F55CA-FN. EXIT.
      *N55CE.    NOTE *NON-UL (TRAD) PRODUCTS             *.
       F55CE.         EXIT.                                             lv10
      *N55CM.    NOTE *IF UPDATE/ DON"T FORCE MINIMUM     *.
       F55CM.    IF    WZ63-CUPIQ = 'I'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F55CM-FN.
      *
           MOVE        +50 TO WZ63-ALMIN
           WZ63-AMINA
           WZ63-AMNDL.
       F55CM-FN. EXIT.
       F55CE-FN. EXIT.
      *N55DA.    NOTE *IF ADD OR RE-ACTIVATE              *.
       F55DA.    IF    WZ63-CACTA = 'A'                                 lv10
                 OR    WZ63-CACTA = 'R'
                 NEXT SENTENCE ELSE GO TO     F55DA-FN.
      *N55DG.    NOTE *IF REGULAR ALLOWED; BUT GRP BILL   *.
       F55DG.    IF    WZ63-IARRGA = 'Y'                                lv15
                 AND   W-GROUP-BILL = 'Y'
                 NEXT SENTENCE ELSE GO TO     F55DG-FN.
           MOVE        '2B' TO WZ63-CERRBR
           MOVE        'N' TO WZ63-IARRGA.
       F55DG-FN. EXIT.
      *N55DM.    NOTE *IF OD ALLOWED; BUT GROUP BILL      *.
       F55DM.    IF    WZ63-IFQOD = 'Y'                                 lv15
                 AND   W-GROUP-BILL = 'Y'
                 NEXT SENTENCE ELSE GO TO     F55DM-FN.
           MOVE        '4F' TO WZ63-CERRBO
           MOVE        'N' TO WZ63-IFQOD.
       F55DM-FN. EXIT.
      *N55FJ.    NOTE *IF MULTIPLE REGULARS NOT ALLOWED   *.
       F55FJ.    IF    W-REGL-XCOUNT > ZERO                             lv15
                 AND   BA98-IMPRA = 'N'
                 AND   W-LIFE-TR-XSW1 = 'Y'
                 AND   WZ63-IARRGA = 'Y'
                 NEXT SENTENCE ELSE GO TO     F55FJ-FN.
           MOVE        '2C' TO WZ63-CERRBR
           MOVE        'N' TO WZ63-IARRGA.
       F55FJ-FN. EXIT.
      *N55FM.    NOTE *IF PENDING ACCT & LOAN NOT ALLOW   *.
       F55FM.    IF    WZ63-IARLNA = 'Y'                                lv15
                 AND   CT01-CTSTA = 01
                 AND   W-UL-XSW1 = 'Y'
                 NEXT SENTENCE ELSE GO TO     F55FM-FN.
           MOVE        '3F' TO WZ63-CERRBL
           WZ63-CERRBP
           MOVE        'N' TO WZ63-IARLNA
           WZ63-IFQODL.
       F55FM-FN. EXIT.
      *N55FT.    NOTE *IF MULTIPLE LOANS NOT ALLOWED      *.
       F55FT.    IF    WZ63-IARLNA = 'Y'                                lv15
                 AND   BA98-IMLNA = 'N'
                 AND   W-LOAN-XCOUNT > ZERO
                 NEXT SENTENCE ELSE GO TO     F55FT-FN.
           MOVE        '3E' TO WZ63-CERRBL
           WZ63-CERRBP
           MOVE        'N' TO WZ63-IARLNA
           WZ63-IFQODL.
       F55FT-FN. EXIT.
       F55DA-FN. EXIT.
      *N55GA.    NOTE *IF LOAN REQUIRES A REGULAR SETUP   *.
       F55GA.    IF    W-REGL-XCOUNT = ZERO                             lv10
                 AND   W-UL-XSW1 = 'N'
                 AND   W-TSA-XSW1 = 'N'
                 AND   WZ63-IARLNA = 'Y'
                 NEXT SENTENCE ELSE GO TO     F55GA-FN.
           MOVE        '3D' TO WZ63-CERRBL
           WZ63-CERRBP
           MOVE        'N' TO WZ63-IARLNA
           WZ63-IFQODL.
       F55GA-FN. EXIT.
      *N55GM.    NOTE *IF PENDING LIFE ACCT; NO MASTER    *.
       F55GM.    IF    CT01-CTSTA = 01                                  lv10
                 NEXT SENTENCE ELSE GO TO     F55GM-FN.
           MOVE        ZERO TO WZ63-ALPAGR
           WZ63-ALPAGM
           WZ63-ALPAGQ
           WZ63-ALPAGS
           WZ63-ALDDUE.
      *N55GP.    NOTE *IF "MUST EQUAL" TYPE PRODUCTS      *.
       F55GP.    IF    CT01-PRCOD < 00199                               lv15
                 OR    (CT01-PRCOD > 00299
                 AND   CT01-PRCOD < 00599)
                 NEXT SENTENCE ELSE GO TO     F55GP-FN.
      *********************************
      * MINIMUMS MUST BE ELIMINATED
      * BECAUSE "MUST EQUAL" AMOUNTS
      * ARE USED BY THE UI TO DETERMINE
      * NOT ONLY THE "MUST EQUAL" AMT,
      * BUT ALSO IF MINIMUMS SHOULD BE
      * IGNORED IN FAVOR OF THESE "MUST
      * EQUAL" AMOUNTS.
      *
      * IF THE ACCOUNT IS PENDING IT IS
      * STILL A "MUST EQUAL" ACCOUNT SO
      * MINIMUMS WOULD NOT APPLY EXCEPT
      * FOR LOANS AND ON DEMANDS
      *********************************
      *
           MOVE        ZERO TO WZ63-AMINA.
       F55GP-FN. EXIT.
       F55GM-900. GO TO F55HA-FN.
       F55GM-FN. EXIT.
      *N55HA.    NOTE *ELSE... LIFE MASTER SHOULD EXIST   *.
       F55HA.         EXIT.                                             lv10
      *N55HE.    NOTE *ACCESS THE LIFE MASTER             *.
       F55HE.    IF    CA5B-IVANT = 'N'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F55HE-FN.
           MOVE        CT01-CTIDND TO S-AAU10-ALCIDN
      *GU AA10
           PERFORM     F94A1 THRU F94A1-FN.
      *N55HG.    NOTE *AA10 NOT FOUND; ERROR              *.
       F55HG.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F55HG-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012265 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F55HG-900. GO TO F55HH-FN.
       F55HG-FN. EXIT.
      *N55HH.    NOTE *ELSE.. CONTINUE                    *.
       F55HH.                                                           lv20
           MOVE        '1' TO AA10-CF
           MOVE        '0' TO AA20-CF.
      *N55HK.    NOTE *IF TRADITIONAL OR UNIVERSAL        *.
       F55HK.    IF    (CT01-PRCOD = 00100                              lv25
                 OR    CT01-PRCOD = 00300
                 OR    CT01-PRCOD = 00310
                 OR    CT01-PRCOD = 00400)
                 OR    W-UL-XSW1 = 'Y'
                 OR    ((CT01-PRCOD > 00610
                 AND   CT01-PRCOD < 00900)
                 OR    (CT01-PRCOD > 00999
                 AND   CT01-PRCOD < 01040))
                 NEXT SENTENCE ELSE GO TO     F55HK-FN.
      *GU AA20
           PERFORM     F94A2 THRU F94A2-FN.
      *N55HM.    NOTE *AA20 (LOAN) FOUND                  *.
       F55HM.    IF    IK = '0'                                         lv30
                 NEXT SENTENCE ELSE GO TO     F55HM-FN.
           MOVE        '1' TO AA20-CF.
      *N55HP.    NOTE *IF LOAN BALANCE IS ZERO            *.
       F55HP.    IF    AA20-ALLNB = ZERO                                lv35
                 AND   WZ63-IARLNA = 'Y'
                 NEXT SENTENCE ELSE GO TO     F55HP-FN.
           MOVE        '3C' TO WZ63-CERRBL
           WZ63-CERRBP
           MOVE        'N' TO WZ63-IARLNA
           WZ63-IFQODL.
       F55HP-FN. EXIT.
       F55HM-FN. EXIT.
       F55HK-FN. EXIT.
      *N55HT.    NOTE *IF AA20 WAS NOT FOUND;GET TSA      *.
       F55HT.    IF    AA20-CF = '0'                                    lv25
                 NEXT SENTENCE ELSE GO TO     F55HT-FN.
      *GU AA66
           PERFORM     F94A3 THRU F94A3-FN.
      *N55HV.    NOTE *IF LOAN WAS NOT FOUND OR ZERO      *.
       F55HV.    IF    (IK = '1'                                        lv30
                 OR    AA66-ALLNB = ZERO)
                 AND   WZ63-IARLNA = 'Y'
                 NEXT SENTENCE ELSE GO TO     F55HV-FN.
           MOVE        '3C' TO WZ63-CERRBL
           WZ63-CERRBP
           MOVE        'N' TO WZ63-IARLNA
           WZ63-IFQODL.
       F55HV-FN. EXIT.
       F55HT-FN. EXIT.
      *N55IA.    NOTE *DETERMINE GRACE DATE               *.
       F55IA.                                                           lv25
           PERFORM     F92IA THRU F92IA-FN.
       F55IA-FN. EXIT.
      *N55JA.    NOTE *IF TRAD OR UL CALC AMOUNTS         *.
       F55JA.    IF    CT01-PRCOD < 00199                               lv25
                 OR    (CT01-PRCOD > 00299
                 AND   CT01-PRCOD < 00599)
                 NEXT SENTENCE ELSE GO TO     F55JA-FN.
           MOVE        ZERO TO AA85-CF
           WZ63-ALPAGM
           WZ63-ALPAGQ
           WZ63-ALPAGS
           MOVE        AA10-ALPAGR TO WZ63-ALPAGR
      *
      *TA98 DOESN'T HAVE ANNUAL SET FOR
      *ALL PRODUCTS, SO IT'S SET HERE
      *SO THAT IT CAN BE DISPLAYED AS
      *A FREQUENCY; ULTIMATELY THOUGH
      *IF SEMI-ANNUAL OR ANNUAL ARE
      *SELECTED THEY WILL BE ERRORED
      *OUT (I.E. WHEN CUPIQ = 'U')
      *
           MOVE        'Y' TO WZ63-IFQAN
      *
           PERFORM     F82JL THRU F82JL-FN.
       F55JA-FN. EXIT.
       F55HH-FN. EXIT.
       F55HE-900. GO TO F55MA-FN.
       F55HE-FN. EXIT.
      *N55MA.    NOTE *ACCESS VANTAGE BROKER DATA         *.
       F55MA.         EXIT.                                             lv15
      *N55MD.    NOTE *IF VANTAGE; ACCESS BROKER          *.
       F55MD.                                                           lv20
           MOVE        '0' TO IK
           PERFORM     F82FA THRU F82FA-FN.
       F55MD-FN. EXIT.
      *N55MG.    NOTE *IF IT WASN"T FOUND; ERROR          *.
       F55MG.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F55MG-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012265 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F55MG-900. GO TO F55MM-FN.
       F55MG-FN. EXIT.
      *N55MM.    NOTE *ELSE.. DETERMINE GRACE DATE        *.
       F55MM.                                                           lv20
           PERFORM     F92IA THRU F92IA-FN.
       F55MM-FN. EXIT.
       F55MA-FN. EXIT.
       F55HA-FN. EXIT.
      *N55ZA.    NOTE *LIFE OVERRIDES OF "AN" AND "SA"    *.
       F55ZA.    IF    CT01-PRCOD < 00600                               lv10
                 AND   WZ63-CUPIQ = 'U'
                 NEXT SENTENCE ELSE GO TO     F55ZA-FN.
      *********************************
      ** DUE TO CUSTOMER SERVICE      *
      ** ISSUES IT WAS DETERMINED THAT*
      ** LIFE POLICIES SHOULD NOT     *
      ** RECEIVE AN ANNUAL OR SEMI-   *
      ** ANNUAL FREQUENCY TO REDUCE   *
      ** RETURNS                      *
      *********************************
      *
           MOVE        'N' TO WZ63-IFQAN
           MOVE        'N' TO WZ63-IFQSA.
       F55ZA-FN. EXIT.
      *N55ZD.    NOTE *FOR RAVA SET BA ARRANGEMENT TO     *.
       F55ZD.    IF    CT01-CTSTA = 01                                  lv10
                 NEXT SENTENCE ELSE GO TO     F55ZD-FN.
      *ACTIVE EVEN IF THE ACCOUNT IS
      *PENDING
      *N55ZG.    NOTE *ON AN ADD OR REACTIVATE            *.
       F55ZG.    IF    WZ63-CACTA = 'A'                                 lv15
                 OR    (WZ63-CACTA = 'C' AND
                       WZ63-CACTS = 'S')
                 NEXT SENTENCE ELSE GO TO     F55ZG-FN.
      *N55ZI.    NOTE *RAVA PRODUCTS                      *.
       F55ZI.    IF    (CT01-PRCOD > 855 AND                            lv20
                       CT01-PRCOD < 860)
                 OR    (CT01-PRCOD > 865 AND
                       CT01-PRCOD < 870)
                 OR    (CT01-PRCOD > 870 AND
                       CT01-PRCOD < 879)
                 OR    (CT01-PRCOD = 891 OR 892)
                 NEXT SENTENCE ELSE GO TO     F55ZI-FN.
           MOVE        01 TO WZ63-CDEST.
       F55ZI-FN. EXIT.
       F55ZG-FN. EXIT.
       F55ZD-FN. EXIT.
       F55-FN.   EXIT.
      *N56.      NOTE *************************************.
      *               *                                   *
      *               *FINANCIAL PLANNING ACCOUNT EDITS   *
      *               *                                   *
      *               *************************************.
       F56.      IF    CT01-CTIDA = 013                                 lv05
                 NEXT SENTENCE ELSE GO TO     F56-FN.
      *
      *********************************
      ** THIS ROUTINE WILL DO ALL THE *
      ** EDITS SPECIFIC TO FINANCIAL  *
      ** PLANNING ACCOUNTS            *
      *********************************
      *N56BB.    NOTE *SET THE MINIMUM AMOUNTS            *.
       F56BB.                                                           lv10
           MOVE        +1 TO WZ63-AMINA
           WZ63-AMNDL.
       F56BB-FN. EXIT.
      *N56BC.    NOTE *SET THE MAXIMUM AMOUNTS            *.
       F56BC.                                                           lv10
           MOVE        99999.99 TO WZ63-AMAXA
           MOVE        25000 TO WZ63-AMAXAO.
       F56BC-FN. EXIT.
       F56-FN.   EXIT.
      *N57.      NOTE *************************************.
      *               *                                   *
      *               *SET FREQUENCIES IN BA NOT ALLOWD   *
      *               *                                   *
      *               *************************************.
       F57.           EXIT.                                             lv05
      *N57BA.    NOTE *IF LOAN IS STILL ALLOWED           *.
       F57BA.    IF    WZ63-IARLNA = 'Y'                                lv10
                 NEXT SENTENCE ELSE GO TO     F57BA-FN.
      *N57BM.    NOTE *IF ALL  EXISTING BA                *.
       F57BM.                                                           lv15
           MOVE        1                        TO J57BMR
                                    GO TO     F57BM-B.
       F57BM-A.
           ADD         1                        TO J57BMR.
       F57BM-B.
           IF          J57BMR                   >  IWS00M
                                    GO TO     F57BM-FN.
                 IF    J57BMR = 1                                       DOT
           MOVE        ZERO TO WS00-COUNT.
                 IF    WS00-NAPDS (J57BMR) =                            DOT
                       J57BMR
                 AND   WZ63-CACTA = 'A'
           ADD         1 TO WS00-COUNT.
       F57BM-900. GO TO F57BM-A.
       F57BM-FN. EXIT.
      *N57BN.    NOTE *IF ALL  EXISTING BA                *.
       F57BN.    IF    WS00-COUNT = IWS00M                              lv15
                 NEXT SENTENCE ELSE GO TO     F57BN-FN.
           MOVE        'N' TO WZ63-IARLNA
           MOVE        '2E' TO WZ63-CERRBL.
       F57BN-FN. EXIT.
       F57BA-FN. EXIT.
      *N57CA.    NOTE *IF BA NOT ALLOWED                  *.
       F57CA.    IF    WZ63-IARTYA = 'N'                                lv10
                 NEXT SENTENCE ELSE GO TO     F57CA-FN.
           MOVE        'N' TO WZ63-IARRGA
           WZ63-IARLNA
           WZ63-IFQOD
           WZ63-IFQODL.
       F57CA-FN. EXIT.
      *N57DA.    NOTE *IF REG/LOAN OFF; TURN FREQ OFF     *.
       F57DA.    IF    WZ63-IARRGA = 'N'                                lv10
                 AND   WZ63-IARLNA = 'N'
                 NEXT SENTENCE ELSE GO TO     F57DA-FN.
           MOVE        'N' TO WZ63-IFQAN
           WZ63-IFQSA
           WZ63-IFQFM
           WZ63-IFQQT
           WZ63-IFQBM
           WZ63-IFQMO
           WZ63-IFQBF
           WZ63-IFQSM
           WZ63-IFQBW
           WZ63-IFQWK
           WZ63-IFQIR
           WZ63-IFQIF
           WZ63-IFQIS
           WZ63-IFQIK
           WZ63-IFQIW.
       F57DA-FN. EXIT.
      *N57FA.    NOTE *IF REG/LOAN/OD OFF; TURN BA OFF    *.
       F57FA.    IF    WZ63-IARRGA = 'N'                                lv10
                 AND   WZ63-IARLNA = 'N'
                 AND   WZ63-IFQODL = 'N'
                 AND   WZ63-IFQOD = 'N'
                 AND   WZ63-IARTYA NOT = 'N'
                 NEXT SENTENCE ELSE GO TO     F57FA-FN.
           MOVE        '1I' TO WZ63-CERRBB
           MOVE        'N' TO WZ63-IARTYA.
       F57FA-FN. EXIT.
       F57-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *UPDATE CROSS EDITS                 *
      *               *                                   *
      *               *************************************.
       F60.      IF    WZ63-CUPIQ = 'U'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F60-FN.
      *
      *********************************
      ** IF AN UPDATE IS BEING DONE   *
      ** ALL FIELDS MUST BE EDITED    *
      ** TO ENSURE ITS VALID VALUES   *
      ** FOR THE RESPECTIVE ELEMENT   *
      *********************************
      *
      *N60BA.    NOTE *IRREGULAR FREQUENCY                *.
       F60BA.    IF    WZ63-CPMTF = 91                                  lv10
                 OR    WZ63-CPMTF = 92
                 OR    WZ63-CPMTF = 93
                 OR    WZ63-CPMTF = 94
                 OR    WZ63-CPMTF = 95
                 NEXT SENTENCE ELSE GO TO     F60BA-FN.
      *N60BG.    NOTE *IF NUMBER OF MONTHS INCORRECT      *.
       F60BG.    IF    W-WK00-COUNT = ZERO                              lv15
                 OR    W-WK00-COUNT = 12
                 NEXT SENTENCE ELSE GO TO     F60BG-FN.
      *IF THE NBR OF MONTHS BYPASSED
      *WAS ZERO OR ALL THE MONTHS
      *WERE BYPASSED THEN THAT IS
      *AN ERROR
      *
           MOVE        012121 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60BG-FN. EXIT.
       F60BA-900. GO TO F60BM-FN.
       F60BA-FN. EXIT.
      *N60BM.    NOTE *ELSE.. NORMAL FREQUENCY            *.
       F60BM.         EXIT.                                             lv10
      *N60BO.    NOTE *IF SOME MONTHS WERE BLANKED OUT    *.
       F60BO.    IF    W-WK00-COUNT > ZERO                              lv15
                 NEXT SENTENCE ELSE GO TO     F60BO-FN.
           MOVE        012206 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60BO-FN. EXIT.
       F60BM-FN. EXIT.
      *N60CA.    NOTE *IF INVALID ON DEMAND INDICATOR     *.
       F60CA.    IF    (WZ63-CPMTF = 99                                 lv10
                 AND   WZ63-IPODM = 'N')
                 OR    (WZ63-CPMTF NOT = 99
                 AND   WZ63-IPODM = 'Y')
                 NEXT SENTENCE ELSE GO TO     F60CA-FN.
           MOVE        012630 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60CA-FN. EXIT.
      *N60DA.    NOTE *IF IRA MO SET & ACCT IS NOT IRA    *.
       F60DA.    IF    WZ63-NIRACM > ZERO                               lv10
                 AND   WZ63-CIRAD = 'N'
                 AND   WZ63-CACTA NOT = 'I'
                 AND   WZ63-CACTA NOT = 'D'
                 NEXT SENTENCE ELSE GO TO     F60DA-FN.
      *   THAT ALLOWS PRIOR PERIOD PAY
           MOVE        012642 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60DA-FN. EXIT.
      *N60DD.    NOTE *IF IRA MO IS BEYOND APRIL 15       *.
       F60DD.    IF    WZ63-NIRACM = 04                                 lv10
                 AND   W-WK00-NDAY > 15
                 NEXT SENTENCE ELSE GO TO     F60DD-FN.
           MOVE        012643 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60DD-FN. EXIT.
      *N60DJ.    NOTE *IF IRA MO SET & OD; ERROR          *.
       F60DJ.    IF    WZ63-NIRACM > ZERO                               lv10
                 AND   WZ63-CPMTF = 99
                 NEXT SENTENCE ELSE GO TO     F60DJ-FN.
           MOVE        012644 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60DJ-FN. EXIT.
      *N60EA.    NOTE *ENSURE PREVIOUS CHG BEFORE CURR    *.
       F60EA.    IF    WZ63-DPCHD NOT <                                 lv10
                       WZ63-DLAUP
                 NEXT SENTENCE ELSE GO TO     F60EA-FN.
           MOVE        012645 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60EA-FN. EXIT.
      *N60FA.    NOTE *ENSURE EXECUTION NOT AFTER PMT     *.
       F60FA.    IF    WZ63-DNEXE >                                     lv10
                       WZ63-DNPMT
                 NEXT SENTENCE ELSE GO TO     F60FA-FN.
           MOVE        012646 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60FA-FN. EXIT.
      *N60FF.    NOTE *ENSURE NEXT PYMT DATE VALID        *.
       F60FF.    IF    (WZ63-DNPMT < WZ63-DAUTB                         lv10
                 OR    WZ63-DNPMT > WZ63-DENDD)
                 AND   (WZ63-CACTA = 'A'
                 OR    WZ63-CACTA = 'C'
                 OR    WZ63-CACTA = 'F'
                 OR    WZ63-CACTA = 'R')
                 NEXT SENTENCE ELSE GO TO     F60FF-FN.
      * - IF INACTIVATING OR DELETING
      *   THIS EDIT IS NOT NECESSARY
           MOVE        012204 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60FF-FN. EXIT.
      *N60GA.    NOTE *IF END DATE ENTERED                *.
       F60GA.    IF    WZ63-GEEND > ZERO                                lv10
                 NEXT SENTENCE ELSE GO TO     F60GA-FN.
      *N60GC.    NOTE *IF A NEW GEEND DATE SITUATION      *.
       F60GC.    IF    WZ63-CACTA = 'A'                                 lv15
                 OR    WZ63-CACTA = 'F'
                 OR    WZ63-CACTA = 'R'
                 NEXT SENTENCE ELSE GO TO     F60GC-FN.
      *N60GF.    NOTE *ENSURE END IS BEYOND GREATEST DT   *.
       F60GF.    IF    WZ63-GEEND NOT >                                 lv20
                       WZ63-DNPMT1
                 NEXT SENTENCE ELSE GO TO     F60GF-FN.
           MOVE        012205 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60GF-FN. EXIT.
       F60GC-FN. EXIT.
      *N60GJ.    NOTE *IF A CHANGE TO END DATE            *.
       F60GJ.    IF    WZ63-CACTA = 'C'                                 lv15
                 AND   WZ63-GEEND NOT = WX12-GEEND
                 NEXT SENTENCE ELSE GO TO     F60GJ-FN.
      *N60GM.    NOTE *DATE NEXT PYMT MUST BE GREATEST    *.
       F60GM.    IF    WX12-DNPMT < WZ63-DNPMT1                         lv20
                 NEXT SENTENCE ELSE GO TO     F60GM-FN.
           MOVE        012205 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60GM-FN. EXIT.
       F60GJ-FN. EXIT.
       F60GA-FN. EXIT.
      *N60IA.    NOTE *EDIT FREQUENCIES                   *.
       F60IA.         EXIT.                                             lv10
      *N60ID.    NOTE *IF MONTHLY                         *.
       F60ID.    IF    WZ63-CPMTF =                                     lv15
                       12
                 NEXT SENTENCE ELSE GO TO     F60ID-FN.
      *- NOTE: THIS IS DONE FIRST
      *        SINCE IT IS THE MOST
      *        COMMON/LIKELY
      *N60IG.    NOTE *IF MONTHLY NOT ALLOWED             *.
       F60IG.    IF    WZ63-IFQMO = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60IG-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60IG-FN. EXIT.
       F60ID-900. GO TO F60IA-FN.
       F60ID-FN. EXIT.
      *N60IP.    NOTE *IF ON DEMAND; OK; PROCESS LATER    *.
       F60IP.    IF    WZ63-CPMTF =                                     lv15
                       99
                 NEXT SENTENCE ELSE GO TO     F60IP-FN.
       F60IP-900. GO TO F60IA-FN.
       F60IP-FN. EXIT.
      *N60JE.    NOTE *IF ANNUAL                          *.
       F60JE.    IF    WZ63-CPMTF =                                     lv15
                       01
                 NEXT SENTENCE ELSE GO TO     F60JE-FN.
      *N60JJ.    NOTE *IF ANNUAL NOT ALLOWED              *.
       F60JJ.    IF    WZ63-IFQAN = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60JJ-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60JJ-FN. EXIT.
       F60JE-900. GO TO F60IA-FN.
       F60JE-FN. EXIT.
      *N60JN.    NOTE *IF SEMI-ANNUAL                     *.
       F60JN.    IF    WZ63-CPMTF =                                     lv15
                       02
                 NEXT SENTENCE ELSE GO TO     F60JN-FN.
      *N60JR.    NOTE *IF SEMI-ANNUAL NOT ALLOWED         *.
       F60JR.    IF    WZ63-IFQSA = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60JR-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60JR-FN. EXIT.
       F60JN-900. GO TO F60IA-FN.
       F60JN-FN. EXIT.
      *N60KA.    NOTE *IF EVERY 4 MOS                     *.
       F60KA.    IF    WZ63-CPMTF =                                     lv15
                       03
                 NEXT SENTENCE ELSE GO TO     F60KA-FN.
      *N60KG.    NOTE *IF EVERY 4 MOS NOT ALLOWED         *.
       F60KG.    IF    WZ63-IFQFM = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60KG-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60KG-FN. EXIT.
       F60KA-900. GO TO F60IA-FN.
       F60KA-FN. EXIT.
      *N60KK.    NOTE *IF QUARTERLY                       *.
       F60KK.    IF    WZ63-CPMTF =                                     lv15
                       04
                 NEXT SENTENCE ELSE GO TO     F60KK-FN.
      *N60KO.    NOTE *IF QUARTERLY NOT ALLOWED           *.
       F60KO.    IF    WZ63-IFQQT = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60KO-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60KO-FN. EXIT.
       F60KK-900. GO TO F60IA-FN.
       F60KK-FN. EXIT.
      *N60KS.    NOTE *IF BI-MONTHLY                      *.
       F60KS.    IF    WZ63-CPMTF =                                     lv15
                       06
                 NEXT SENTENCE ELSE GO TO     F60KS-FN.
      *N60KV.    NOTE *IF BI-MONTHLY NOT ALLOWED          *.
       F60KV.    IF    WZ63-IFQBM = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60KV-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60KV-FN. EXIT.
       F60KS-900. GO TO F60IA-FN.
       F60KS-FN. EXIT.
      *N60LK.    NOTE *IF BI-FORTNIGHTLY                  *.
       F60LK.    IF    WZ63-CPMTF =                                     lv15
                       13
                 NEXT SENTENCE ELSE GO TO     F60LK-FN.
      *N60LO.    NOTE *IF BI-FORTNIGHTLY NOT ALLOWED      *.
       F60LO.    IF    WZ63-IFQBF = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60LO-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60LO-FN. EXIT.
       F60LK-900. GO TO F60IA-FN.
       F60LK-FN. EXIT.
      *N60LQ.    NOTE *IF SEMI-MONTHLY                    *.
       F60LQ.    IF    WZ63-CPMTF =                                     lv15
                       24
                 NEXT SENTENCE ELSE GO TO     F60LQ-FN.
      *N60LS.    NOTE *IF SEMI-MONTHLY NOT ALLOWED        *.
       F60LS.    IF    WZ63-IFQSM = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60LS-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60LS-FN. EXIT.
      *N60LV.    NOTE *IF THE DAY IS 31; DO NOT ALLOW     *.
       F60LV.    IF    W-WK00-NDAY = 31                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60LV-FN.
           MOVE        012641 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60LV-FN. EXIT.
       F60LQ-900. GO TO F60IA-FN.
       F60LQ-FN. EXIT.
      *N60MA.    NOTE *IF BI-WEEKLY                       *.
       F60MA.    IF    WZ63-CPMTF =                                     lv15
                       26
                 NEXT SENTENCE ELSE GO TO     F60MA-FN.
      *N60MG.    NOTE *IF BI-WEEKLY NOT ALLOWED           *.
       F60MG.    IF    WZ63-IFQBW = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60MG-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60MG-FN. EXIT.
       F60MA-900. GO TO F60IA-FN.
       F60MA-FN. EXIT.
      *N60MK.    NOTE *IF DAILY; NO GOOD; NOT ALLOWED     *.
       F60MK.    IF    WZ63-CPMTF =                                     lv15
                       31
                 NEXT SENTENCE ELSE GO TO     F60MK-FN.
           MOVE        012119 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60MK-900. GO TO F60IA-FN.
       F60MK-FN. EXIT.
      *N60MS.    NOTE *IF WEEKLY                          *.
       F60MS.    IF    WZ63-CPMTF =                                     lv15
                       52
                 NEXT SENTENCE ELSE GO TO     F60MS-FN.
      *N60MV.    NOTE *IF WEEKLY NOT ALLOWED              *.
       F60MV.    IF    WZ63-IFQWK = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60MV-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60MV-FN. EXIT.
       F60MS-900. GO TO F60IA-FN.
       F60MS-FN. EXIT.
      *N60NA.    NOTE *IF IRREGULAR MONTHLY               *.
       F60NA.    IF    WZ63-CPMTF =                                     lv15
                       91
                 NEXT SENTENCE ELSE GO TO     F60NA-FN.
      *N60NG.    NOTE *IF IRREGULAR MONTHLY NOT ALLOWED   *.
       F60NG.    IF    WZ63-IFQIR = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60NG-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60NG-900. GO TO F60NK-FN.
       F60NG-FN. EXIT.
      *N60NK.    NOTE *ELSE... IRREGULAR MONTHLY OK       *.
       F60NK.         EXIT.                                             lv20
      *N60NO.    NOTE *IF UL LOAN; DO NOT ALLOW           *.
       F60NO.    IF    (CT01-CTIDA = 004                                lv25
                 OR    CT01-CTIDA = 005)
                 AND   CT01-PRCOD > 00199
                 AND   CT01-PRCOD < 00261
                 AND   WZ63-CPMTC = 01
                 NEXT SENTENCE ELSE GO TO     F60NO-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60NO-FN. EXIT.
       F60NK-FN. EXIT.
       F60NA-900. GO TO F60IA-FN.
       F60NA-FN. EXIT.
      *N60NS.    NOTE *IF IRREGULAR BI-FORTNIGHTLY        *.
       F60NS.    IF    WZ63-CPMTF =                                     lv15
                       92
                 NEXT SENTENCE ELSE GO TO     F60NS-FN.
      *N60NV.    NOTE *IF IRR. BI-FORTNIGHTLY INVALID     *.
       F60NV.    IF    WZ63-IFQIF = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60NV-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60NV-FN. EXIT.
       F60NS-900. GO TO F60IA-FN.
       F60NS-FN. EXIT.
      *N60OA.    NOTE *IF IRREGULAR SEMI-MONTHLY          *.
       F60OA.    IF    WZ63-CPMTF =                                     lv15
                       93
                 NEXT SENTENCE ELSE GO TO     F60OA-FN.
      *N60OG.    NOTE *IF IRR. SEMI-MONTHLY INVALID       *.
       F60OG.    IF    WZ63-IFQIS = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60OG-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60OG-FN. EXIT.
       F60OA-900. GO TO F60IA-FN.
       F60OA-FN. EXIT.
      *N60OK.    NOTE *IF IRREGULAR BI-WEEKLY             *.
       F60OK.    IF    WZ63-CPMTF =                                     lv15
                       94
                 NEXT SENTENCE ELSE GO TO     F60OK-FN.
      *N60OO.    NOTE *IF IRR. BI-WEEKLY INVALID          *.
       F60OO.    IF    WZ63-IFQIK = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60OO-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60OO-FN. EXIT.
       F60OK-900. GO TO F60IA-FN.
       F60OK-FN. EXIT.
      *N60OS.    NOTE *IF IRREGULAR WEEKLY                *.
       F60OS.    IF    WZ63-CPMTF =                                     lv15
                       95
                 NEXT SENTENCE ELSE GO TO     F60OS-FN.
      *N60OV.    NOTE *IF IRR. WEEKLY INVALID             *.
       F60OV.    IF    WZ63-IFQIW = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60OV-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60OV-FN. EXIT.
       F60OS-900. GO TO F60IA-FN.
       F60OS-FN. EXIT.
      *N60PA.    NOTE *IF END OF TERM; NOT GOOD; ERROR    *.
       F60PA.    IF    WZ63-CPMTF =                                     lv15
                       98
                 NEXT SENTENCE ELSE GO TO     F60PA-FN.
           MOVE        012119 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60PA-900. GO TO F60IA-FN.
       F60PA-FN. EXIT.
      *N60PT.    NOTE *UNKNOWN; ABEND                     *.
       F60PT.                                                           lv15
           MOVE        012118 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60PT-FN. EXIT.
       F60IA-FN. EXIT.
       F60-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *GENERAL UPDATE PROCESSING          *
      *               *                                   *
      *               *************************************.
       F65.      IF    WZ63-CUPIQ = 'U'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F65-FN.
      *
      *********************************
      ** THIS FUNCTION WILL EXECUTE   *
      ** IF AN UPDATE IS BEING DONE   *
      ** TO SEE IF THAT TYPE OF       *
      ** UPDATE CAN BE DONE           *
      *********************************
      *
      *N65BA.    NOTE *IF ARRANGEMENT NOT ALLOWED         *.
       F65BA.    IF    WZ63-IARTYA = 'N'                                lv10
                 NEXT SENTENCE ELSE GO TO     F65BA-FN.
           MOVE        012647 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65BA-FN. EXIT.
      *N65CA.    NOTE *IF REGULAR NOT ALLOWED             *.
       F65CA.    IF    WZ63-IARRGA = 'N'                                lv10
                 AND   WZ63-CPMTC = 00
                 AND   WZ63-CPMTF NOT = 99
                 NEXT SENTENCE ELSE GO TO     F65CA-FN.
           MOVE        012648 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65CA-FN. EXIT.
      *N65DA.    NOTE *IF LOAN NOT ALLOWED                *.
       F65DA.    IF    WZ63-IARLNA = 'N'                                lv10
                 AND   WZ63-CPMTC = 01
                 AND   WZ63-CPMTF NOT = 99
                 NEXT SENTENCE ELSE GO TO     F65DA-FN.
           MOVE        012649 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65DA-FN. EXIT.
      *N65EA.    NOTE *IF REG OD NOT ALLOWED              *.
       F65EA.    IF    WZ63-IFQOD = 'N'                                 lv10
                 AND   WZ63-CPMTC = 00
                 AND   WZ63-CPMTF = 99
                 NEXT SENTENCE ELSE GO TO     F65EA-FN.
           MOVE        012650 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65EA-FN. EXIT.
      *N65EE.    NOTE *IF OD LOAN NOT ALLOWED             *.
       F65EE.    IF    WZ63-IFQODL = 'N'                                lv10
                 AND   WZ63-CPMTC = 01
                 AND   WZ63-CPMTF = 99
                 NEXT SENTENCE ELSE GO TO     F65EE-FN.
           MOVE        012650 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65EE-FN. EXIT.
      *N65HA.    NOTE *EDIT AMOUNT AGAINST RANGES         *.
       F65HA.    IF    (WZ63-CACTA = 'A'                                lv10
                 OR    WZ63-CACTA = 'C'
                 OR    WZ63-CACTA = 'F'
                 OR    WZ63-CACTA = 'R')
                 NEXT SENTENCE ELSE GO TO     F65HA-FN.
      *- DON"T DO EDITS FOR DELETES AND
      *  INACTIVATES
      *N65HC.    NOTE *IF ONE TIME (ON DEMAND)            *.
       F65HC.    IF    WZ63-CPMTF = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F65HC-FN.
      *N65HE.    NOTE *IF OUT OF RANGE                    *.
       F65HE.    IF    WZ63-APMTL < WZ63-AMNDL                          lv20
                 NEXT SENTENCE ELSE GO TO     F65HE-FN.
      *NOTE: CONDITION IS NEVER HIT FOR
      *CERT, FUND, & TRAD LIFE PRODUCTS
      *AS AMNDL IS ONLY SET WHEN CUPIQ=
      *I, AND F65 IS ONLY PERFORMED
      *WHEN CUPIQ=U.
           MOVE        012023 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65HE-FN. EXIT.
       F65HC-900. GO TO F65HM-FN.
       F65HC-FN. EXIT.
      *N65HM.    NOTE *ELSE... NORMAL FREQUENCY           *.
       F65HM.         EXIT.                                             lv15
      *N65HO.    NOTE *IF IRREGULAR; PMT = YEARLY         *.
       F65HO.    IF    WZ63-CPMTF = 91                                  lv20
                 OR    WZ63-CPMTF = 92
                 OR    WZ63-CPMTF = 93
                 OR    WZ63-CPMTF = 94
                 OR    WZ63-CPMTF = 95
                 NEXT SENTENCE ELSE GO TO     F65HO-FN.
      *N65HQ.    NOTE *IF OUT OF RANGE                    *.
       F65HQ.    IF    WZ63-APMTL < WZ63-AMINA                          lv25
                 NEXT SENTENCE ELSE GO TO     F65HQ-FN.
      *NOTE: CONDITION IS NEVER HIT FOR
      *CERT, FUND, & TRAD LIFE PRODUCTS
      *AS AMINA IS ONLY SET WHEN CUPIQ=
      *I, AND F65 IS ONLY PERFORMED
      *WHEN CUPIQ=U.
           MOVE        012023 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65HQ-FN. EXIT.
       F65HO-900. GO TO F65HV-FN.
       F65HO-FN. EXIT.
      *N65HV.    NOTE *ELSE.. MULTIPLY AMOUNT BY FREQ     *.
       F65HV.         EXIT.                                             lv20
      *N65IA.    NOTE *EDIT TRAD AND UL BALANCES          *.
       F65IA.    IF    (CT01-CTIDA = 004                                lv25
                 OR    CT01-CTIDA = 005)
                 AND   (CT01-PRCOD < 00199
                 OR    (CT01-PRCOD > 00299
                 AND   CT01-PRCOD < 00599))
                 AND   WZ63-ALPAGR > ZERO
                 AND   CT01-CTSTA NOT = 01
                 AND   WZ63-CPMTC = 00
                 NEXT SENTENCE ELSE GO TO     F65IA-FN.
      *N65ID.    NOTE *IF FREQUENCY IS VALID              *.
       F65ID.    IF    WZ63-CPMTF = 01                                  lv30
                 OR    WZ63-CPMTF = 02
                 OR    WZ63-CPMTF = 04
                 OR    WZ63-CPMTF = 12
                 NEXT SENTENCE ELSE GO TO     F65ID-FN.
      *N65IG.    NOTE *IF ANNUAL                          *.
       F65IG.    IF    WZ63-CPMTF = 01                                  lv35
                 AND   WZ63-APMTL NOT =
                       WZ63-ALPAGR
                 NEXT SENTENCE ELSE GO TO     F65IG-FN.
           MOVE        012652 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65IG-FN. EXIT.
      *N65IJ.    NOTE *IF SEMI ANNUAL                     *.
       F65IJ.    IF    WZ63-CPMTF = 02                                  lv35
                 AND   WZ63-APMTL NOT =
                       WZ63-ALPAGS
                 NEXT SENTENCE ELSE GO TO     F65IJ-FN.
           MOVE        012652 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65IJ-FN. EXIT.
      *N65IN.    NOTE *IF QUARTERLY                       *.
       F65IN.    IF    WZ63-CPMTF = 04                                  lv35
                 AND   WZ63-APMTL NOT =
                       WZ63-ALPAGQ
                 NEXT SENTENCE ELSE GO TO     F65IN-FN.
           MOVE        012652 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65IN-FN. EXIT.
      *N65IS.    NOTE *IF MONTHLY                         *.
       F65IS.    IF    WZ63-CPMTF = 12                                  lv35
                 AND   WZ63-APMTL NOT =
                       WZ63-ALPAGM
                 NEXT SENTENCE ELSE GO TO     F65IS-FN.
           MOVE        012652 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65IS-FN. EXIT.
       F65ID-900. GO TO F65IV-FN.
       F65ID-FN. EXIT.
      *N65IV.    NOTE *UNKNOWN MUST EQUAL AMOUNT          *.
       F65IV.                                                           lv30
           MOVE        012652 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65IV-FN. EXIT.
       F65IA-900. GO TO F65JA-FN.
       F65IA-FN. EXIT.
      *N65JA.    NOTE *ELSE.... REGULAR NON UL ACCTS      *.
       F65JA.         EXIT.                                             lv25
      *N65JD.    NOTE *IF OUT OF RANGE                    *.
       F65JD.    IF    WZ63-APMTL < WZ63-AMINA                          lv30
                 NEXT SENTENCE ELSE GO TO     F65JD-FN.
      *NOTE: CONDITION IS NEVER HIT FOR
      *CERT, FUND, & TRAD LIFE PRODUCTS
      *AS AMINA IS ONLY SET WHEN CUPIQ=
      *I, AND F65 IS ONLY PERFORMED
      *WHEN CUPIQ=U.
           MOVE        012023 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65JD-FN. EXIT.
       F65JA-FN. EXIT.
       F65HV-FN. EXIT.
       F65HM-FN. EXIT.
      *N65JT.    NOTE *IF LOAN; CHECK MINIMUM             *.
       F65JT.    IF    WZ63-CPMTC = 01                                  lv15
                 AND   WZ63-APMTL < WZ63-ALMIN
                 NEXT SENTENCE ELSE GO TO     F65JT-FN.
      *NOTE: CONDITION IS NEVER HIT FOR
      *CERT & TRAD LIFE PRODUCTS
      *AS ALMIN IS ONLY SET WHEN CUPIQ=
      *I, AND F65 IS ONLY PERFORMED
      *WHEN CUPIQ=U.
           MOVE        012558 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65JT-FN. EXIT.
       F65HA-FN. EXIT.
      *N65KK.    NOTE *CANNOT CHANGE IRA MONTH ON FUTR    *.
       F65KK.    IF    WZ63-CACTA = 'F'                                 lv10
                 AND   WZ63-NIRACM NOT =
                       W-WK00-NIRACM
                 NEXT SENTENCE ELSE GO TO     F65KK-FN.
           MOVE        012654 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65KK-FN. EXIT.
      *N65LA.    NOTE *IF ON DEMAND IS INVALID            *.
       F65LA.    IF    (WZ63-CPMTF = 99                                 lv10
                 OR    FT12-CPMTF = 99)
                 AND   (WZ63-CACTA = 'F'
                 OR    WZ63-CACTA = 'R')
                 NEXT SENTENCE ELSE GO TO     F65LA-FN.
      * - NO FUTURE OR REINSTATE
           MOVE        012653 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65LA-FN. EXIT.
      *N65MA.    NOTE *CANNOT BE INACT/SPECIAL/UNKNOWN    *.
       F65MA.    IF    (WZ63-CACTA = 'A'                                lv10
                 OR    WZ63-CACTA = 'R')
                 AND   (CT01-CTSTA = 00
                 OR    CT01-CTSTA = 03
                 OR    CT01-CTSTA = 04)
                 NEXT SENTENCE ELSE GO TO     F65MA-FN.
      * - 01 IS PENDING, 02 IS ACTIVE
      *N65MD.    NOTE *EXCEPTION IS INACTIVE FUNDS        *.
       F65MD.    IF    CT01-CTIDA = 002                                 lv15
                 AND   CT01-CTSTA = 03
                 NEXT SENTENCE ELSE GO TO     F65MD-FN.
       F65MD-900. GO TO F65MG-FN.
       F65MD-FN. EXIT.
      *N65MG.    NOTE *ELSE... ERROR INVALID STATUS       *.
       F65MG.                                                           lv15
           MOVE        012136 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65MG-FN. EXIT.
       F65MA-FN. EXIT.
      *N65NA.    NOTE *FOR PENDING OR INACTIVE FUND       *.
       F65NA.    IF    (WZ63-CACTA = 'A' OR 'R')                        lv10
                 AND   CT01-CTIDA = 002
                 AND   (CT01-PRCOD = 102 OR 106
                       OR 108 OR 107 OR 124
                       OR 125 OR 126)
                 AND   (CT01-CTSTA = 01 OR 03)
                 AND   WZ63-CPMTF NOT = 99
                 NEXT SENTENCE ELSE GO TO     F65NA-FN.
      *102 OR 106 OR 108 OR 107 OR
      *124 OR 125 OR 126
      *ON-DEMAND IS ONLY FREQUENCY
      *ALLOWED WHEN ADDING OR
      *REACTIVATING A BA.
           MOVE        015008 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65NA-FN. EXIT.
      *N65OA.    NOTE *IF ADD OR RE-ACTIVATE              *.
       F65OA.    IF    WZ63-CACTA = 'A' OR 'R'                          lv10
                 NEXT SENTENCE ELSE GO TO     F65OA-FN.
      *N65OD.    NOTE *IF LIFE PRODUCT SPECIAL FIG        *.
       F65OD.    IF    (CT01-CTIDA = 004 OR 005)                        lv15
                 AND   (CT01-AYSID = 202 OR 205)
                 AND   WZ63-CPMTF = 99
                 NEXT SENTENCE ELSE GO TO     F65OD-FN.
      *AND FREQ = ONE-TIME (ON DEMAND)
      *N65OG.    NOTE *IF CONTRACT PENDING OR ACTIVE      *.
       F65OG.    IF    (CT01-CTSTA = 01 OR 02)                          lv20
                 AND   WZ63-CDEST NOT = 01
                 NEXT SENTENCE ELSE GO TO     F65OG-FN.
      *ARRANGEMENT S/B ACTIVE
           MOVE        012655 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65OG-FN. EXIT.
       F65OD-FN. EXIT.
       F65OA-FN. EXIT.
      *N65PA.    NOTE *IF ON DEMAND ADDED/CHANGED         *.
       F65PA.    IF    WZ63-CPMTF = 99                                  lv10
                 AND   (WZ63-CACTA = 'A'
                 OR    WZ63-CACTA = 'C')
                 NEXT SENTENCE ELSE GO TO     F65PA-FN.
      *N65PC.    NOTE *CALL LATE TRADING MODULE           *.
       F65PC.                                                           lv15
           INITIALIZE  I93B
           MOVE        NS20-DCACD TO I93B-DACTT
           MOVE        'O' TO I93B-CEADC
           PERFORM     F97BB THRU F97BB-FN.
       F65PC-FN. EXIT.
      *N65PD.    NOTE *BEFORE 3PM                         *.
       F65PD.    IF    EIBTIME < WS-GETIM6                              lv15
                 NEXT SENTENCE ELSE GO TO     F65PD-FN.
      *N65PG.    NOTE *IF OD IS NOT TODAY; ERROR          *.
       F65PG.    IF    WZ63-DNPMT NOT =                                 lv20
                       WZ63-DCACG
                 OR    WZ63-DNEXE NOT =
                       WZ63-DCACG
                 NEXT SENTENCE ELSE GO TO     F65PG-FN.
           MOVE        012660 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65PG-FN. EXIT.
       F65PD-900. GO TO F65PL-FN.
       F65PD-FN. EXIT.
      *N65PL.    NOTE *3PM OR AFTER                       *.
       F65PL.         EXIT.                                             lv15
      *N65PO.    NOTE *IF TODAY OR NEXT ACCOUNTING DATE   *.
       F65PO.    IF    (WZ63-DNPMT = WZ63-DCACG                         lv20
                 OR    WZ63-DNPMT = WZ63-DNACG)
                 AND   WZ63-DNPMT = WZ63-DNEXE
                 NEXT SENTENCE ELSE GO TO     F65PO-FN.
       F65PO-900. GO TO F65PT-FN.
       F65PO-FN. EXIT.
      *N65PT.    NOTE *ELSE... BAD DATE                   *.
       F65PT.                                                           lv20
           MOVE        012660 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F65PT-FN. EXIT.
       F65PL-FN. EXIT.
       F65PA-FN. EXIT.
       F65-FN.   EXIT.
      *N70.      NOTE *************************************.
      *               *                                   *
      *               *UPDATE DELETES                     *
      *               *                                   *
      *               *************************************.
       F70.      IF    WZ63-CUPIQ = 'U'                                 lv05
                 AND   WZ63-CACTA = 'D'
                 NEXT SENTENCE ELSE GO TO     F70-FN.
      *********************************
      ** THIS FUNCTION WILL EXECUTE   *
      ** EDITS SPECIFICALLY NEEDED    *
      ** FOR DELETES                  *
      *********************************
      *
      *N70DA.    NOTE *IF STATUS IS OK TO DELETE OR OD    *.
       F70DA.    IF    WZ63-CDEST = 03                                  lv10
                 OR    WZ63-CDEST = 04
                 OR    WZ63-CPMTF = 99
                 NEXT SENTENCE ELSE GO TO     F70DA-FN.
       F70DA-900. GO TO F70DD-FN.
       F70DA-FN. EXIT.
      *N70DD.    NOTE *ELSE... ERROR                      *.
       F70DD.                                                           lv10
      *
           MOVE        012656 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F70DD-FN. EXIT.
      *N70GA.    NOTE *ENSURE DATE OK TO DELETE           *.
       F70GA.    IF    WZ63-DNPMT NOT >                                 lv10
                       WZ63-DNPMT1
                 AND   WZ63-CPMTF NOT = 99
                 NEXT SENTENCE ELSE GO TO     F70GA-FN.
           MOVE        012657 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F70GA-FN. EXIT.
       F70-FN.   EXIT.
      *N72.      NOTE *************************************.
      *               *                                   *
      *               *UPDATE ADD                         *
      *               *                                   *
      *               *************************************.
       F72.      IF    WZ63-CUPIQ = 'U'                                 lv05
                 AND   WZ63-CACTA = 'A'
                 NEXT SENTENCE ELSE GO TO     F72-FN.
      *********************************
      ** THIS FUNCTION WILL EXECUTE   *
      ** EDITS SPECIFICALLY NEEDED    *
      ** FOR ADDS                     *
      *********************************
      *
      *N72CA.    NOTE *STATUS MUST BE ACTIVE/PENDING      *.
       F72CA.    IF    WZ63-CDEST NOT = 01                              lv10
                 AND   WZ63-CDEST NOT = 02
                 NEXT SENTENCE ELSE GO TO     F72CA-FN.
           MOVE        012658 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F72CA-FN. EXIT.
      *N72EA.    NOTE *NAPDS MUST BE UNIQUE FOR ACCOUNT   *.
       F72EA.                                                           lv10
           MOVE        1                        TO J72EAR
                                    GO TO     F72EA-B.
       F72EA-A.
           ADD         1                        TO J72EAR.
       F72EA-B.
           IF          J72EAR                   >  IWS00L
                                    GO TO     F72EA-FN.
                 IF    WS00-NAPDS (J72EAR) =                            DOT
                       WZ63-NAPDS
           MOVE        12036 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F72EA-900. GO TO F72EA-A.
       F72EA-FN. EXIT.
      *N72EB.    NOTE *CHECK FOR THE ON DEMAND COUNT      *.
       F72EB.    IF    WZ63-NAPDS = W-WK99-NAPDS                        lv10
                 NEXT SENTENCE ELSE GO TO     F72EB-FN.
      *
           MOVE        12036 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F72EB-FN. EXIT.
       F72-FN.   EXIT.
      *N74.      NOTE *************************************.
      *               *                                   *
      *               *UPDATE MODIFY                      *
      *               *                                   *
      *               *************************************.
       F74.      IF    WZ63-CUPIQ = 'U'                                 lv05
                 AND   WZ63-CACTA NOT = 'A'
                 AND   WZ63-CACTA NOT = 'D'
                 NEXT SENTENCE ELSE GO TO     F74-FN.
      *********************************
      ** THIS FUNCTION WILL EXECUTE   *
      ** EDITS SPECIFICALLY NEEDED    *
      ** FOR MODIFICATIONS            *
      *********************************
      *
      *N74DA.    NOTE *CANNOT CHANGE FREQUENCY ON OD      *.
       F74DA.    IF    (WX12-CPMTF NOT =                                lv10
                       WZ63-CPMTF
                 AND   (WX12-CPMTF = 99
                 OR    WZ63-CPMTF = 99))
                 OR    (WX12-IPODM NOT =
                       WZ63-IPODM
                 AND   WZ63-IPODM = 'Y')
                 NEXT SENTENCE ELSE GO TO     F74DA-FN.
           MOVE        012661 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74DA-FN. EXIT.
      *N74EA.    NOTE *CANNOT UPDATE THE PAYMENT TYPE     *.
       F74EA.    IF    WX12-CPMTC NOT = WZ63-CPMTC                      lv10
                 NEXT SENTENCE ELSE GO TO     F74EA-FN.
           MOVE        012057 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74EA-FN. EXIT.
      *N74FA.    NOTE *IF CHANGE CHECK STATUS             *.
       F74FA.    IF    WZ63-CACTA = 'C'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F74FA-FN.
      *N74FD.    NOTE *IF ORIGINAL NOT ACTIVE OR PEND     *.
       F74FD.    IF    WX12-CDEST NOT = 01                              lv15
                 AND   WX12-CDEST NOT = 02
                 NEXT SENTENCE ELSE GO TO     F74FD-FN.
      *N74FG.    NOTE *IF ALLOWABLE CHANGE                *.
       F74FG.    IF    (WX12-CDEST = 03                                 lv20
                 AND   WZ63-CACTS = 'S')
                 OR    (WX12-CDEST = 04
                 AND   WX12-CIRMO = WZ63-CIRMO
                 AND   WX12-APMTL = WZ63-APMTL
                 AND   WX12-DNPMT = WZ63-DNPMT
                 AND   W-WK00-NIRACM = WZ63-NIRACM
                 AND   WX12-CPMTF = WZ63-CPMTF
                 AND   WX12-CDEST = WZ63-CDEST)
                 NEXT SENTENCE ELSE GO TO     F74FG-FN.
      *********************************
      ** WHEN CHANGING A STATUS IT IS *
      ** ALLOWABLE TO CHANGE DATA IN  *
      ** WHICH CASE THE CACTA = 'C'   *
      ** AND THE CACTS WOULD BE 'S'   *
      **  OR                          *
      ** A FUTURE IS BEING END DATED  *
      ** IS OK TOO AS LONG AS NO      *
      ** OTHER FIELDS CHANGED         *
      *********************************
       F74FG-900. GO TO F74FI-FN.
       F74FG-FN. EXIT.
      *N74FI.    NOTE *ELSE.. ERROR ON ORIGINAL STATUS    *.
       F74FI.                                                           lv20
           MOVE        012662 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74FI-FN. EXIT.
       F74FD-FN. EXIT.
       F74FA-FN. EXIT.
      *N74IA.    NOTE *IF FUTURE                          *.
       F74IA.    IF    WZ63-CACTA = 'F'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F74IA-FN.
      *N74IC.    NOTE *ORIGINAL SHOULD BE ACTIVE          *.
       F74IC.    IF    FT12-CDEST NOT = 01                              lv15
                 NEXT SENTENCE ELSE GO TO     F74IC-FN.
      *
           MOVE        012664 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74IC-FN. EXIT.
      *N74IE.    NOTE *ORIGINAL SHOULD BE SAME ARRANGE    *.
       F74IE.    IF    FT01-CLID NOT = WZ63-CLID                        lv15
                 OR    FT03-CARTY NOT = WZ63-CARTY
                 OR    FT03-NARRS NOT = WZ63-NARRS
                 OR    FT06-CTID NOT = WZ63-CTID
                 OR    FT12-CPMTC NOT = WZ63-CPMTC
                 OR    FT12-NAPDS NOT = WZ63-NAPDS
                 NEXT SENTENCE ELSE GO TO     F74IE-FN.
           MOVE        012723 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74IE-FN. EXIT.
      *N74IG.    NOTE *CONTRACT SHOULD BE ACTIVE          *.
       F74IG.    IF    CT01-CTSTA NOT = 02                              lv15
                 AND   (CT01-CTIDA = 004
                 OR    CT01-CTIDA = 005)
                 NEXT SENTENCE ELSE GO TO     F74IG-FN.
           MOVE        012664 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74IG-FN. EXIT.
      *N74IL.    NOTE *ENSURE DATE OK TO ADD              *.
       F74IL.    IF    WZ63-DNPMT NOT > WZ63-DNPMT1                     lv15
                 NEXT SENTENCE ELSE GO TO     F74IL-FN.
      *
      *- DNPMT1 IS THE GREATEST DATE
      *  FOUND ON THE EXISTING SET OF
      *  ARRANGEMENTS
      *
           MOVE        012665 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74IL-FN. EXIT.
       F74IA-FN. EXIT.
      *N74MA.    NOTE *EDIT STATUS                        *.
       F74MA.         EXIT.                                             lv10
      *N74ML.    NOTE *IS FUTURE STATUS OK                *.
       F74ML.    IF    (WZ63-CDEST NOT = 04                             lv15
                 AND   WZ63-CACTA = 'F')
                 OR    (WZ63-CDEST = 04
                 AND   WZ63-CACTA = 'C'
                 AND   WZ63-GEEND = ZERO
                 AND   WX12-GEEND = ZERO
                 AND   WX12-CHCR = WZ63-CHCR
                 AND   WX12-CWRC = WZ63-CWRC)
                 OR    (WZ63-CDEST = 04
                 AND   WZ63-CACTA NOT = 'F'
                 AND   WZ63-CACTA NOT = 'C')
                 NEXT SENTENCE ELSE GO TO     F74ML-FN.
      *- IF STATUS IS A 04 AND ACTION
      *  IS "F"UTURE    OR
      *- IF STATUS IS A 04 AND ACTION
      *  IS A "C"HANGE THEN THE END
      *  DATE OR THE WHO/HOW SHOULD
      *  CHANGE
           MOVE        012669 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74ML-FN. EXIT.
       F74MA-FN. EXIT.
      *N74PA.    NOTE *IF STATUS CHANGE                   *.
       F74PA.    IF    WZ63-CACTS = 'S'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F74PA-FN.
      *N74PD.    NOTE *IF CHANGE & REACTIVATE             *.
       F74PD.    IF    WZ63-CACTA = 'C'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F74PD-FN.
      *N74PG.    NOTE *END DATE SHOULD >= TODAY           *.
       F74PG.    IF    WZ63-GEEND < WZ63-DCACG                          lv20
                 AND   WZ63-GEEND > ZERO
                 NEXT SENTENCE ELSE GO TO     F74PG-FN.
           MOVE        012205 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74PG-FN. EXIT.
      *N74PJ.    NOTE *STATUS S/B ACTIVE/PEND AND ORIG    *.
       F74PJ.    IF    ((WZ63-CDEST NOT = 01                            lv20
                 AND   WZ63-CDEST NOT = 02)
                 OR    WX12-CDEST NOT = 03)
                 NEXT SENTENCE ELSE GO TO     F74PJ-FN.
      *    SHOULD BE INACTIVE
           MOVE        012663 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74PJ-FN. EXIT.
       F74PD-FN. EXIT.
      *N74QA.    NOTE *IF RE-ACTIVATE/ SB ACTV OR PEND    *.
       F74QA.    IF    WZ63-CACTA = 'R'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F74QA-FN.
      *N74QD.    NOTE *STATUS S/B ACTIVE/PEND AND ORIG    *.
       F74QD.    IF    ((WZ63-CDEST NOT = 01                            lv20
                 AND   WZ63-CDEST NOT = 02)
                 OR    WX12-CDEST NOT = 03)
                 NEXT SENTENCE ELSE GO TO     F74QD-FN.
      *    SHOULD BE INACTIVE
           MOVE        012663 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74QD-FN. EXIT.
      *N74QM.    NOTE *END DATE SHOULD >= TODAY           *.
       F74QM.    IF    WZ63-GEEND > ZERO                                lv20
                 NEXT SENTENCE ELSE GO TO     F74QM-FN.
           MOVE        012205 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74QM-FN. EXIT.
       F74QA-FN. EXIT.
      *N74RA.    NOTE *IF INACTIVATE REQUESTED            *.
       F74RA.    IF    WZ63-CACTA = 'I'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F74RA-FN.
      *N74RD.    NOTE *STATUS MUST BE ACTIVE/PENDING      *.
       F74RD.    IF    WX12-CDEST NOT = 01                              lv20
                 AND   WX12-CDEST NOT = 02
                 NEXT SENTENCE ELSE GO TO     F74RD-FN.
           MOVE        012666 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74RD-FN. EXIT.
      *N74RG.    NOTE *DETAIL STATUS S/B INACTIVE ALSO    *.
       F74RG.    IF    WZ63-CDEST NOT = 03                              lv20
                 NEXT SENTENCE ELSE GO TO     F74RG-FN.
           MOVE        012667 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74RG-FN. EXIT.
      *N74RL.    NOTE *ENSURE END DATE IS TODAY           *.
       F74RL.    IF    WZ63-GEEND NOT = WZ63-DCACG                      lv20
                 NEXT SENTENCE ELSE GO TO     F74RL-FN.
      *
      *- IF INACTIVATING THE ARRANGEMNT
      *  THE END DATE MUST BE SET TO
      *  TODAY
      *
           MOVE        012205 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74RL-FN. EXIT.
      *N74RN.    NOTE *IF LIFE PRODUCT                    *.
       F74RN.    IF    (CT01-CTIDA = 004                                lv20
                 OR    CT01-CTIDA = 005)
                 AND   W-UL-XSW1 = 'N'
                 AND   W-TSA-XSW1 = 'N'
                 NEXT SENTENCE ELSE GO TO     F74RN-FN.
      *N74RP.    NOTE *IF LOAN STILL EXISTS               *.
       F74RP.    IF    WZ63-CPMTC NOT = 01                              lv25
                 AND   W-LOAN-XCOUNT > ZERO
                 NEXT SENTENCE ELSE GO TO     F74RP-FN.
           MOVE        012668 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F74RP-FN. EXIT.
       F74RN-FN. EXIT.
       F74RA-FN. EXIT.
       F74PA-FN. EXIT.
       F74-FN.   EXIT.
      *N79.      NOTE *************************************.            ADU102
      *               *                                   *             ADU102
      *               *---> Normal Termination            *             ADU102
      *               *                                   *             ADU102
      *               *************************************.            ADU102
       F79.                                                             lv05
      *     Return to Calling Module                                    ADU102
           MOVE                     ALL '1' TO FT GO TO F20.            ADU102
       F79-FN.   EXIT.
      *N81.      NOTE *************************************.
      *               *                                   *
      *               *ACCESS ALL ARRANGEMENTS FOR ACCT   *
      *               *                                   *
      *               *************************************.
       F81.           EXIT.                                             lv05
      *N81CA.    NOTE *SET UP PROCESS                     *.
       F81CA.                                                           lv10
           MOVE        'N' TO W-ALLOW-BA
           MOVE        ZERO TO W-SD-XCOUNT
           W-WK99-NAPDS
           INITIALIZE  WS00-TABLE.
       F81CA-FN. EXIT.
      *N81EA.    NOTE *PROCESS THE ACCT 2ND INDEX         *.
       F81EA.                                                           lv10
           MOVE        CT01-CTID TO S-CXU2Y-CTID
           S-CXU06-CTID
           MOVE        ZERO TO S-CXU2Y-CLID
           S-CXU01-CLID
           MOVE        00 TO S-CXU2Y-CARTY
           S-CXU03-CARTY
           MOVE        ZERO TO S-CXU2Y-NARRS
           MOVE        '>=' TO S-CXU2Y-OPER
      *
      *DO A GU ON CX2Y
      *
           PERFORM     F94Y1 THRU F94Y1-FN.
       F81EA-FN. EXIT.
      *N81EG.    NOTE *LOOP UNTIL NO MORE CONTRACTS       *.
       F81EG.    IF    IK = '0'                                         lv10
                 AND   CX2Y-CTID = CT01-CTID
                 NEXT SENTENCE ELSE GO TO     F81EG-FN.
      **
      *N81EM.    NOTE *GET POSITION ON CX03               *.
       F81EM.                                                           lv15
           MOVE        CX2Y-CLID TO S-CXU01-CLID
           MOVE        CX2Y-CARTY TO S-CXU03-CARTY
           MOVE        CX2Y-NARRS TO S-CXU03-NARRS
      *
      *DO A GU ON CX03
      *
           PERFORM     F94X4 THRU F94X4-FN.
       F81EM-FN. EXIT.
      *N81ET.    NOTE *IF CX03 WAS FOUND & POSITION SET   *.
       F81ET.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F81ET-FN.
      *N81FA.    NOTE *IF THIS IS A BA                    *.
       F81FA.    IF    CX2Y-CARTY = 01                                  lv20
                 NEXT SENTENCE ELSE GO TO     F81FA-FN.
      *
      *DO A GN ON CX12
      *
           PERFORM     F94X2 THRU F94X2-FN.
      *N81FH.    NOTE *LOOP UNTIL NO MORE BA"S            *.
       F81FH.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F81FH-FN.
      *N81FR.    NOTE *IF CX12 IS REGULAR IS THE 01       *.
       F81FR.                                                           lv30
                 IF    CX12-NAPDS > 0                                   DOT
                 AND   CX12-NAPDS < 99
           MOVE        CX12-NAPDS TO IWS00L
           WS00-NAPDS (CX12-NAPDS).
       F81FR-FN. EXIT.
      *N81FV.    NOTE *IF CX12 IS ON DEMAND (99)          *.
       F81FV.    IF    CX12-NAPDS = 99                                  lv30
                 NEXT SENTENCE ELSE GO TO     F81FV-FN.
           MOVE        CX12-NAPDS TO W-WK99-NAPDS.
       F81FV-FN. EXIT.
      *N81GA.    NOTE *IF CX12 IS ONE BEING PROCESSED     *.
       F81GA.    IF    S-CXU01-CLID = WZ63-CLID                         lv30
                 AND   S-CXU03-NARRS = WZ63-NARRS
                 AND   CX12-CPMTC = WZ63-CPMTC
                 AND   CX12-NAPDS = WZ63-NAPDS
                 AND   CX12-GESTD = WZ63-GESTD
                 AND   WZ63-NAPDS > ZERO
                 AND   WZ63-GESTD > ZERO
                 NEXT SENTENCE ELSE GO TO     F81GA-FN.
      *    - STORE IT
           MOVE        CX12 TO WX12.
       F81GA-FN. EXIT.
      *N81GD.    NOTE *IF INQUIRY AND CX12 KEY MISSING    *.
       F81GD.    IF    WZ63-CUPIQ = 'I'                                 lv30
                 AND   S-CXU01-CLID = WZ63-CLID
                 AND   S-CXU03-NARRS = WZ63-NARRS
                 AND   WZ63-NAPDS = ZERO
                 AND   WZ63-GESTD = ZERO
                 AND   CX12-CPMTC = 00
                 AND   CX12-NAPDS NOT = 99
                 NEXT SENTENCE ELSE GO TO     F81GD-FN.
      *IF CX12 IS ONE BEING PROCESSED
      *    - STORE IT
      *N81GF.    NOTE *IF NO EXISTING SAVED CX12          *.
       F81GF.    IF    WX12-NAPDS = ZERO                                lv35
                 NEXT SENTENCE ELSE GO TO     F81GF-FN.
           MOVE        CX12 TO WX12.
       F81GF-900. GO TO F81GH-FN.
       F81GF-FN. EXIT.
      *N81GH.    NOTE *ELSE..                             *.
       F81GH.         EXIT.                                             lv35
      *N81GL.    NOTE *IF THIS CX12 IS BETTER THAN SAVD   *.
       F81GL.    IF    (WX12-CDEST NOT = 01                             lv40
                 AND   CX12-CDEST = 01)
                 OR    ((CX12-CDEST = WX12-CDEST
                 OR    CX12-CDEST = 01)
                 AND   CX12-NAPDS > WX12-NAPDS)
                 OR    (WX12-CDEST = 03
                 AND   CX12-CDEST NOT = 03)
                 NEXT SENTENCE ELSE GO TO     F81GL-FN.
           MOVE        CX12 TO WX12.
       F81GL-FN. EXIT.
       F81GH-FN. EXIT.
       F81GD-FN. EXIT.
      *N81HA.    NOTE *DETERMINE FARTHEST FUTURE DATE     *.
       F81HA.    IF    S-CXU01-CLID = WZ63-CLID                         lv30
                 AND   S-CXU03-NARRS = WZ63-NARRS
                 AND   CX12-NAPDS = WZ63-NAPDS
                 AND   CX12-CPMTC = WZ63-CPMTC
                 NEXT SENTENCE ELSE GO TO     F81HA-FN.
      *    - STORE IT
      *N81HC.    NOTE *IF THE DETAIL ACTIVE/FUTURE        *.
       F81HC.    IF    CX12-CDEST = 01                                  lv35
                 OR    CX12-CDEST = 02
                 OR    CX12-CDEST = 04
                 NEXT SENTENCE ELSE GO TO     F81HC-FN.
      *N81HF.    NOTE *IF DATE NEXT PAYMENT FARTHER       *.
       F81HF.    IF    CX12-DNPMT > WZ63-DNPMT1                         lv40
                 NEXT SENTENCE ELSE GO TO     F81HF-FN.
           MOVE        WZ63-CPMTCX TO W-PREV-CPMTCX
           MOVE        WZ63-NAPDSK TO W-PREV-NAPDSK
           MOVE        WZ63-GESTD1 TO W-PREV-GESTD1
           MOVE        WZ63-DNPMT1 TO W-PREV-DNPMT1
           MOVE        CX12-CPMTC TO WZ63-CPMTCX
           MOVE        CX12-NAPDS TO WZ63-NAPDSK
           MOVE        CX12-GESTD TO WZ63-GESTD1
           MOVE        CX12-DNPMT TO WZ63-DNPMT1.
       F81HF-FN. EXIT.
       F81HC-FN. EXIT.
      *N81IA.    NOTE *IF FUTURE...                       *.
       F81IA.    IF    WZ63-CACTA = 'F'                                 lv35
                 NEXT SENTENCE ELSE GO TO     F81IA-FN.
      *N81IG.    NOTE *IF ACTIVE THAT FUTURE IS TIED TO   *.
       F81IG.    IF    (CX12-CDEST = 01                                 lv40
                 OR    CX12-CDEST = 02)
                 NEXT SENTENCE ELSE GO TO     F81IG-FN.
      *
           MOVE        S-CXU01-CLID TO FT01-CLID
           MOVE        S-CXU03-CARTY TO FT03-CARTY
           MOVE        S-CXU03-NARRS TO FT03-NARRS
           MOVE        S-CXU06-CTID TO FT06-CTID
           MOVE        CX12 TO FT12.
       F81IG-FN. EXIT.
       F81IA-FN. EXIT.
       F81HA-FN. EXIT.
      *N81JA.    NOTE *IF THE DETAIL ACTIVE/PENDING       *.
       F81JA.    IF    CX12-CDEST = 01                                  lv30
                 OR    CX12-CDEST = 02
                 NEXT SENTENCE ELSE GO TO     F81JA-FN.
      *N81JC.    NOTE *IF REGULAR                         *.
       F81JC.    IF    CX12-CPMTC = 00                                  lv35
                 AND   CX12-CPMTF NOT = 99
                 NEXT SENTENCE ELSE GO TO     F81JC-FN.
           ADD         +1 TO W-REGL-XCOUNT.
       F81JC-FN. EXIT.
      *N81JF.    NOTE *IF LOAN                            *.
       F81JF.    IF    CX12-CPMTC = 01                                  lv35
                 NEXT SENTENCE ELSE GO TO     F81JF-FN.
           ADD         +1 TO W-LOAN-XCOUNT.
       F81JF-FN. EXIT.
       F81JA-FN. EXIT.
      *N81PA.    NOTE *GET NEXT CX12 SEGMENT              *.
       F81PA.                                                           lv30
      *
      *DO A GN ON CX12
      *
           PERFORM     F94X2 THRU F94X2-FN.
       F81PA-FN. EXIT.
       F81FH-900. GO TO F81FH.
       F81FH-FN. EXIT.
       F81FA-FN. EXIT.
      *N81QA.    NOTE *IF THIS IS A SD                    *.
       F81QA.    IF    CX2Y-CARTY = 10                                  lv20
                 AND   CT01-CTIDA = 002
                 NEXT SENTENCE ELSE GO TO     F81QA-FN.
      *N81QC.    NOTE *IF PRODUCT THAT ALLOWS A SD        *.
       F81QC.    IF    CT01-PRCOD = 00016                               lv25
                 OR    ((CT01-PRCOD = 00013
                 OR    CT01-PRCOD = 00167)
                 AND   (CT01-PRSCD = 000000001
                 OR    CT01-PRSCD = SPACES))
                 NEXT SENTENCE ELSE GO TO     F81QC-FN.
      *-BYPASS EDITING OF EXISTING
      *SD"S BECAUSE IT DOES NOT
      *MATTER IF SD"S EXIST OR NOT
       F81QC-900. GO TO F81QE-FN.
       F81QC-FN. EXIT.
      *N81QE.    NOTE *ELSE....                           *.
       F81QE.                                                           lv25
      *
      *DO A GN ON CX13
      *
           PERFORM     F94S1 THRU F94S1-FN.
      *N81QG.    NOTE *LOOP THRU DISBURSEMENTS   (CX13)   *.
       F81QG.    IF    IK = '0'                                         lv30
                 NEXT SENTENCE ELSE GO TO     F81QG-FN.
      *N81QR.    NOTE *IF SD IS ACTIVE/PENDING            *.
       F81QR.    IF    (CX13-CDEST = 01                                 lv35
                 OR    CX13-CDEST = 02)
                 AND   CX13-IIARR = 'Y'
                 NEXT SENTENCE ELSE GO TO     F81QR-FN.
      **
      **
           ADD         +1 TO W-SD-XCOUNT
           MOVE        CX13-CX13K TO S-CXU13-CX13K
      *
      *DO A GN ON CX14
      *
           PERFORM     F94S2 THRU F94S2-FN.
      *N81RA.    NOTE *LOOP THRU DESTINATIONS    (CX14)   *.
       F81RA.    IF    IK = '0'                                         lv40
                 NEXT SENTENCE ELSE GO TO     F81RA-FN.
      *N81RD.    NOTE *IF A VALID TRANSFER; BA IS VALID   *.
       F81RD.    IF    CX14-CPITC = 02                                  lv45
                 AND   CX14-CTIDA = 002
                 NEXT SENTENCE ELSE GO TO     F81RD-FN.
      **
           MOVE        'Y' TO W-ALLOW-BA.
       F81RD-FN. EXIT.
      *N81RG.    NOTE *GET NEXT DESTINATION      (CX14)   *.
       F81RG.                                                           lv45
           PERFORM     F94S2 THRU F94S2-FN.
       F81RG-FN. EXIT.
       F81RA-900. GO TO F81RA.
       F81RA-FN. EXIT.
       F81QR-FN. EXIT.
      *N81RL.    NOTE *GET THE NEXT CX13                  *.
       F81RL.                                                           lv35
           PERFORM     F94S1 THRU F94S1-FN.
       F81RL-FN. EXIT.
       F81QG-900. GO TO F81QG.
       F81QG-FN. EXIT.
       F81QE-FN. EXIT.
       F81QA-FN. EXIT.
       F81ET-FN. EXIT.
      *N81VA.    NOTE *GET THE NEXT CX2Y SEGMENT          *.
       F81VA.                                                           lv15
           PERFORM     F94Y2 THRU F94Y2-FN.
       F81VA-FN. EXIT.
       F81EG-900. GO TO F81EG.
       F81EG-FN. EXIT.
       F81-FN.   EXIT.
      *N82.      NOTE *************************************.
      *               *                                   *
      *               *LIFE SPECIAL EDITS                 *
      *               *                                   *
      *               *************************************.
       F82.           EXIT.                                             lv05
      *N82FA.    NOTE *ACCESS VANTAGE BROKER              *.            AAOLBB
       F82FA.    IF    CA5B-CF = '1'                                    lv10
                 AND   CA5B-IVANT = 'Y'                                 AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82FA-FN.                 AAOLBB
      *********************************                                 AAOLBB
      **  ACCESS VANTAGE MODULE HERE  *                                 AAOLBB
      *********************************                                 AAOLBB
      *POPULATE PE30 FIELDS                                             AAOLBB
           INITIALIZE  PE30                                             AAOLBB
           INITIALIZE  PE31                                             AAOLBB
      *LOAD COMPANY                                                     AAOLBB
                 IF    CT01-CTIDA = 005                                 DOT
           MOVE        'LNY' TO PE30-CVACO                              AAOLBB
                 ELSE                                                   AAOLBB
           MOVE        'LIF' TO PE30-CVACO.                             AAOLBB
      *LOAD ACCOUNT ID                                                  DOT
           MOVE        CT01-CTIDND TO 7-PE30-CTIDND                     AAOLBB
           MOVE        7-PE30-GVAID TO PE30-GVAID                       AAOLBB
      *SET THE CONTRACT INDICATOR                                       AAOLBB
           MOVE        'Y' TO PE30-IDBCD                                AAOLBB
      *SET THE BILLING INDICATOR                                        AAOLBB
           MOVE        'Y' TO PE30-IDBBI                                AAOLBB
           MOVE        '0' TO IK                                        AAOLBB
      *CALL PAZ000                                                      AAOLBB
           PERFORM     F98AZ THRU F98AZ-FN.                             AAOLBB
                 IF    IK = '1'                                         DOT
      *VANTAGE MODULE NOT FOUND, ERROR                                  AAOLBB
      *
               GO TO     F82FA-FN.
           MOVE        '0' TO AA10-CF.                                  AAOLBB
                 IF    WZ63-IARLNA = 'Y'                                DOT
                 AND   PE31-QTNOL = ZEROES
      *IF LOAN IS POSSIBLE IN EZ TRANS
      *AND TOTAL NUMBER OF LOANS ZERO                                   AAOLBB
      *DO NOT USE ERU
      *
           MOVE        '3C' TO WZ63-CERRBL
           WZ63-CERRBP
           MOVE        'N' TO WZ63-IARLNA
           WZ63-IFQODL.
           MOVE        '1' TO AA10-CF                                   DOT
           MOVE        PE31-ALDDUE TO AA10-ALDDUE.                      AAOLBB
      *N82JL.    NOTE *BLOCK LEVEL TO WRAP CODE           *.
       F82JL.         EXIT.                                             lv14
      *N82JM.    NOTE *CHECKS FOR DI AND TRAD LIFE        *.            AAOLBB
       F82JM.    IF    (CT01-PRCOD < 199)                               lv15
                 OR    (CT01-PRCOD > 299                                AAOLBB
                 AND   CT01-PRCOD < 599)                                AAOLBB
                 AND   AA10-CF = '1'                                    AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82JM-FN.                 AAOLBB
           MOVE        AA10-ALPAGR TO 7-82-PREMIUMS                     AAOLBB
           MOVE        7-82-ONE TO 7-82-ONES                            AAOLBB
           MOVE        'N' TO 7-82-FL-DI                                AAOLBB
           MOVE        'N' TO 7-82-TL-2012.                             AAOLBB
      *N82JN.    NOTE *IF FUTURE DETAIL AND ZERO AMT      *.            AAOLBB
       F82JN.                                                           lv17
      *IF THE ACCOUNT DETAIL ENTERED                                    AAOLBB
      *IS A FUTURE, AND THE AMOUNT IS                                   AAOLBB
      *ZERO, SKIP THE AMOUNT/MODE                                       AAOLBB
      *EDITS.  THE DETAIL ENTERED IS A                                  AAOLBB
      *FUTURE SKIP, WHICH IS USED TO                                    AAOLBB
      *SKIP PAYMENT MONTHS. IT HAS NO                                   AAOLBB
      *AFFECT ON THE MODE OR AMOUNT.                                    AAOLBB
      *IF THE ON-DEMAND IS SETUP AND                                    AAOLBB
      *DOLLAR AMOUNT NOT ENTERED YET                                    AAOLBB
      *SKIP AMOUNT AND MODE EDITS.                                      AAOLBB
      *NOT CONSIDERED THE NORMAL                                        AAOLBB
      *MODE TO BE CONCERNED WITH.                                       AAOLBB
      *                                                                 AAOLBB
      *
      *
      *
      *
      *N82JO.    NOTE *CHECK FOR FLORIDA DI POLICIES      *.            AAOLBB
       F82JO.                                                           lv20
      *SET SWITCH WHICH IS USED IN                                      AAOLBB
      *LATER CALCULATIONS TO NOT CHARGE                                 AAOLBB
      *MODAL PREMIUMS FOR FLORIDA DI                                    AAOLBB
           MOVE        AA10-ALCIDN TO 7-82-ALCIDN                       AAOLBB
           MOVE        CT01-PRSCD TO 7-82-PRSCD.                        AAOLBB
                 IF    AA10-ALAPST = 15                                 DOT
                 AND   7-82-FIRST-4 = 9100                              AAOLBB
                 AND   7-82-DE-CODE = 22                                AAOLBB
                 AND   ((7-82-FGH-CODE = 160)                           AAOLBB
                 OR    (7-82-FGH-CODE < 140                             AAOLBB
                 OR    7-82-FGH-CODE > 184))                            AAOLBB
      *SET FLORIDA DI SWITCH                                            AAOLBB
           MOVE        'Y' TO 7-82-FL-DI.                               AAOLBB
                 IF    CT01-PRCOD = 300                                 DOT
                 AND   (7-82-FIRST-4 = 9000                             AAOLBB
                 OR    7-82-FIRST-4 = 9700)                             AAOLBB
                 AND   7-82-DE-CODE = 19                                AAOLBB
                 AND   (7-82-FGH-CODE = 313                             AAOLBB
                       OR 316 OR 318)                                   AAOLBB
      *SET TERM LIFE 2012 SWITCH                                        AAOLBB
           MOVE        'Y' TO 7-82-TL-2012.                             AAOLBB
       F82JO-FN. EXIT.
      *N82JP.    NOTE *CHECK POLICY DATE FOR FORMULA      *.            AAOLBB
       F82JP.    IF    AA10-ALPLDT > 19661002                           lv20
                 NEXT SENTENCE ELSE GO TO     F82JP-FN.                 AAOLBB
           MOVE        ZERO TO AA85-CF.                                 AAOLBB
      *N82JQ.    NOTE *GET SEGMENT AA85 WITH A DATE       *.            AAOLBB
       F82JQ.    IF    AA10-CF = '1'                                    lv25
                 NEXT SENTENCE ELSE GO TO     F82JQ-FN.                 AAOLBB
           PERFORM     F94A4 THRU F94A4-FN.
                 IF    IK = ZERO                                        DOT
           MOVE        '1' TO AA85-CF.                                  AAOLBB
       F82JQ-FN. EXIT.
      *N82JR.    NOTE *NEW CALCULATION FORMULA            *.            AAOLBB
       F82JR.    IF    AA85-CF = '1'                                    lv25
                 AND   AA10-CPCAL = 1                                   AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82JR-FN.                 AAOLBB
      *N82JT.    NOTE *MODE PREMIUM CHECK MONTHLY         *.
       F82JT.         EXIT.                                             lv30
      *N82LA.    NOTE *CALCULATE PREMIUM BY MONTHLY       *.            AAOLBB
       F82LA.    IF    CT01-PRCOD = 310                                 lv35
                 OR    7-82-TL-2012 = 'Y'                               AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82LA-FN.                 AAOLBB
      *FOR IPE INSURED OR TL2012                                        AAOLBB
           COMPUTE     WZ63-ALPAGM ROUNDED =
           (AA10-ALPAGR * 0.0875).                                      AAOLBB
      *                                                                 DOT
      *
       F82LA-FN. EXIT.
      *N82LB.    NOTE *CALCULATE PREMIUM BY MONTHLY       *.            AAOLBB
       F82LB.    IF    CT01-PRCOD NOT = 310                             lv35
                 AND   (7-82-TL-2012 NOT = 'Y')                         AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82LB-FN.                 AAOLBB
      *DON'T CHARGE PREMIUM FOR FL DI                                   AAOLBB
                 IF    7-82-FL-DI = 'Y'                                 DOT
           COMPUTE     WZ63-ALPAGM ROUNDED =
           (AA10-ALPAGR * 0.0868)                                       AAOLBB
                 ELSE
           COMPUTE     WZ63-ALPAGM ROUNDED =
           ((AA10-ALPAGR * 0.0868) + 0.60).                             AAOLBB
      *DISCONTINUED: A 50? DISCOUNT                                     AAOLBB
      *HERE FOR MONTHLY LIFE OR DI.                                     AAOLBB
      *
      *
       F82LB-FN. EXIT.
       F82JT-FN. EXIT.
      *N82LF.    NOTE *MODE PREMIUM CHECK QUARTERLY       *.
       F82LF.         EXIT.                                             lv30
      *N82LL.    NOTE *CALCULATE PREMIUM BY QUARTERS      *.            AAOLBB
       F82LL.    IF    CT01-PRCOD = 310                                 lv35
                 OR    7-82-TL-2012 = 'Y'                               AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82LL-FN.                 AAOLBB
      *FOR IPE INSURED OR TL 2012                                       AAOLBB
           COMPUTE     WZ63-ALPAGQ ROUNDED =
           (AA10-ALPAGR * 0.2625).                                      AAOLBB
      *                                                                 DOT
      *
       F82LL-FN. EXIT.
      *N82LM.    NOTE *CALCULATE PREMIUM BY QUARTERS      *.            AAOLBB
       F82LM.    IF    CT01-PRCOD NOT = 310                             lv35
                 AND   (7-82-TL-2012 NOT = 'Y')                         AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82LM-FN.                 AAOLBB
      *DON'T CHARGE PREMIUM FOR FL DI                                   AAOLBB
                 IF    7-82-FL-DI = 'Y'                                 DOT
           COMPUTE     WZ63-ALPAGQ ROUNDED =
           (AA10-ALPAGR * 0.258)                                        AAOLBB
                 ELSE
           COMPUTE     WZ63-ALPAGQ ROUNDED =
           ((AA10-ALPAGR * 0.258) + 0.50).                              AAOLBB
      *                                                                 DOT
      *
       F82LM-FN. EXIT.
       F82LF-FN. EXIT.
      *N82LP.    NOTE *MODE PREMIUM CHECK SEMI-ANNUAL     *.
       F82LP.         EXIT.                                             lv30
      *N82LS.    NOTE *CALCULATE PREMIUM FOR SEMI-ANN     *.            AAOLBB
       F82LS.    IF    CT01-PRCOD = 310                                 lv35
                 OR    7-82-TL-2012 = 'Y'                               AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82LS-FN.                 AAOLBB
      *FOR IPE INSURED OR TL 2012                                       AAOLBB
           COMPUTE     WZ63-ALPAGS ROUNDED =
           (AA10-ALPAGR * 0.515).                                       AAOLBB
      *                                                                 DOT
      *
       F82LS-FN. EXIT.
      *N82LT.    NOTE *CALCULATE PREMIUM FOR SEMI-ANN     *.            AAOLBB
       F82LT.    IF    CT01-PRCOD NOT = 310                             lv35
                 AND   (7-82-TL-2012 NOT = 'Y')                         AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82LT-FN.                 AAOLBB
      *DON'T CHARGE PREMIUM FOR FL DI                                   AAOLBB
                 IF    7-82-FL-DI = 'Y'                                 DOT
           COMPUTE     WZ63-ALPAGS ROUNDED =
           (AA10-ALPAGR * 0.502)                                        AAOLBB
                 ELSE
           COMPUTE     WZ63-ALPAGS ROUNDED =
           ((AA10-ALPAGR * 0.502) + 0.40).                              AAOLBB
      *                                                                 DOT
      *
       F82LT-FN. EXIT.
       F82LP-FN. EXIT.
       F82JR-FN. EXIT.
      *N82LW.    NOTE *OLD CALCULATION FORMULA            *.            AAOLBB
       F82LW.    IF    AA85-CF = ZERO                                   lv25
                 OR    AA10-CPCAL NOT = 1                               AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82LW-FN.                 AAOLBB
      *N82LY.    NOTE *MODE PREMIUM CHECK MONTHLY         *.
       F82LY.         EXIT.                                             lv30
      *N82MA.    NOTE *CALCULATE PREMIUM BY MONTHLY       *.            AAOLBB
       F82MA.    IF    CT01-PRCOD = 310                                 lv35
                 OR    7-82-TL-2012 = 'Y'                               AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82MA-FN.                 AAOLBB
      *FOR IPE INSURED PRODUCTS                                         AAOLBB
           COMPUTE     WZ63-ALPAGM ROUNDED =
           (AA10-ALPAGR * 0.0875).                                      AAOLBB
      *                                                                 DOT
      *
       F82MA-FN. EXIT.
      *N82MC.    NOTE *CALCULATE PREMIUM BY MONTHLY       *.            AAOLBB
       F82MC.    IF    CT01-PRCOD NOT = 310                             lv35
                 AND   (7-82-TL-2012 NOT = 'Y')                         AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82MC-FN.                 AAOLBB
           COMPUTE     7-82-TENS =                                      AAOLBB
           (AA10-ALPAGR - 7-82-ONE)                                     AAOLBB
           COMPUTE     7-82-PREMIUM-T ROUNDED =                         AAOLBB
           (7-82-TENS * 0.0868)                                         AAOLBB
           COMPUTE     7-82-PREMIUM-O ROUNDED =                         AAOLBB
           (7-82-CONES * 0.0868)                                        AAOLBB
           COMPUTE     WZ63-ALPAGM =
           ((7-82-PREMIUM-T                                             AAOLBB
           + 7-82-PREMIUM-O) + 0.60).                                   AAOLBB
      *DISCONTINUED: A 50? DISCOUNT                                     DOT
      *HERE FOR MONTHLY LIFE OR DI.                                     AAOLBB
      *
      *
       F82MC-FN. EXIT.
       F82LY-FN. EXIT.
      *N82ME.    NOTE *MODE PREMIUM CHECK QUARTERLY       *.
       F82ME.         EXIT.                                             lv30
      *N82MF.    NOTE *CALCULATE PREMIUM BY QUARTERS      *.            AAOLBB
       F82MF.    IF    CT01-PRCOD = 310                                 lv35
                 OR    7-82-TL-2012 = 'Y'                               AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82MF-FN.                 AAOLBB
      *FOR IPE INSURED OR TL 2012                                       AAOLBB
           COMPUTE     WZ63-ALPAGQ ROUNDED =
           (AA10-ALPAGR * 0.2625).                                      AAOLBB
      *                                                                 DOT
      *
       F82MF-FN. EXIT.
      *N82MG.    NOTE *CALCULATE PREMIUM BY QUARTERS      *.            AAOLBB
       F82MG.    IF    CT01-PRCOD NOT = 310                             lv35
                 AND   (7-82-TL-2012 NOT = 'Y')                         AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82MG-FN.                 AAOLBB
           COMPUTE     7-82-TENS =                                      AAOLBB
           (AA10-ALPAGR - 7-82-ONE)                                     AAOLBB
           COMPUTE     7-82-PREMIUM-T ROUNDED =                         AAOLBB
           (7-82-TENS * 0.258)                                          AAOLBB
           COMPUTE     7-82-PREMIUM-O ROUNDED =                         AAOLBB
           (7-82-CONES * 0.258)                                         AAOLBB
           COMPUTE     WZ63-ALPAGQ =
           ((7-82-PREMIUM-T                                             AAOLBB
           + 7-82-PREMIUM-O) + 0.50).                                   AAOLBB
      *                                                                 DOT
      *
       F82MG-FN. EXIT.
       F82ME-FN. EXIT.
      *N82MI.    NOTE *MODE PREMIUM CHECK SEMI-ANNUAL     *.
       F82MI.         EXIT.                                             lv30
      *N82MJ.    NOTE *CALCULATE PREMIUM FOR SEMI-ANN     *.            AAOLBB
       F82MJ.    IF    CT01-PRCOD = 310                                 lv35
                 OR    7-82-TL-2012 = 'Y'                               AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82MJ-FN.                 AAOLBB
      *FOR IPE INSURED OR TL 2012                                       AAOLBB
           COMPUTE     WZ63-ALPAGS ROUNDED =
           (AA10-ALPAGR * 0.515).                                       AAOLBB
      *                                                                 DOT
      *
       F82MJ-FN. EXIT.
      *N82MK.    NOTE *CALCULATE PREMIUM FOR SEMI-ANN     *.            AAOLBB
       F82MK.    IF    CT01-PRCOD NOT = 310                             lv35
                 AND   (7-82-TL-2012 NOT = 'Y')                         AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82MK-FN.                 AAOLBB
           COMPUTE     7-82-TENS =                                      AAOLBB
           (AA10-ALPAGR - 7-82-ONE)                                     AAOLBB
           COMPUTE     7-82-PREMIUM-T ROUNDED =                         AAOLBB
           (7-82-TENS * 0.502)                                          AAOLBB
           COMPUTE     7-82-PREMIUM-O ROUNDED =                         AAOLBB
           (7-82-CONES * 0.502)                                         AAOLBB
           COMPUTE     WZ63-ALPAGS =
           ((7-82-PREMIUM-T                                             AAOLBB
           + 7-82-PREMIUM-O) + 0.40).                                   AAOLBB
      *                                                                 DOT
      *
       F82MK-FN. EXIT.
       F82MI-FN. EXIT.
       F82LW-FN. EXIT.
       F82JP-900. GO TO F82NB-FN.
       F82JP-FN. EXIT.
      *N82NB.    NOTE *USE OLD PREMIUM CALC FORMULA       *.            AAOLBB
       F82NB.         EXIT.                                             lv20
      *N82NF.    NOTE *COMPUTE PREMIUMS                   *.
       F82NF.                                                           lv25
           COMPUTE     WZ63-ALPAGS ROUNDED =
           (AA10-ALPAGR * 0.510).                                       AAOLBB
      *                                                                 DOT
      *
       F82NF-FN. EXIT.
      *N82NM.    NOTE *COMPUTE PREMIUMS                   *.
       F82NM.                                                           lv25
           COMPUTE     WZ63-ALPAGQ ROUNDED =
           (AA10-ALPAGR * 0.260).                                       AAOLBB
      *                                                                 DOT
      *
       F82NM-FN. EXIT.
      *N82NP.    NOTE *COMPUTE PREMIUMS                   *.
       F82NP.                                                           lv25
           COMPUTE     WZ63-ALPAGM ROUNDED =
           (AA10-ALPAGR * 0.0875).                                      AAOLBB
      *                                                                 DOT
      *
       F82NP-FN. EXIT.
       F82NB-FN. EXIT.
      *N82NT.    NOTE *ALWAYS DETERMIN IF QT/SA ALLOW'D   *.
       F82NT.                                                           lv20
      *WITH A MODE CHANGE, THE DUE DATE                                 AAOLBB
      *MUST FALL ON ANNIVERSARY DATE.                                   AAOLBB
           MOVE        AA10-ALDDUE TO 7-82-WORK-DATE                    AAOLBB
           MOVE        AA10-ALPLDT TO 7-82-ANN-DATE.                    AAOLBB
      *N82PB.    NOTE *QUARTERLY EDIT                     *.
       F82PB.         EXIT.                                             lv25
      *N82PF.    NOTE *CHECK IF MONTHS DON'T MATCH        *.            AAOLBB
       F82PF.    IF    7-82-WORK-MM NOT =                               lv30
                       7-82-ANN-MM                                      AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82PF-FN.                 AAOLBB
                 IF    7-82-WORK-MM > 7-82-ANN-MM                       DOT
           SUBTRACT    7-82-ANN-MM FROM 7-82-WORK-MM                    AAOLBB
           DIVIDE      3 INTO 7-82-WORK-MM GIVING                       AAOLBB
           7-82-RESULT REMAINDER 7-82-REM                               AAOLBB
                 ELSE                                                   AAOLBB
           SUBTRACT    7-82-WORK-MM FROM 7-82-ANN-MM                    AAOLBB
           DIVIDE      3 INTO 7-82-ANN-MM GIVING                        AAOLBB
           7-82-RESULT REMAINDER 7-82-REM.                              AAOLBB
                 IF    7-82-REM NOT = ZERO                              DOT
                 AND   WZ63-CUPIQ = 'U'
      *ONLY OVERRIDE FREQ IF UPDATE
           MOVE        'N' TO WZ63-IFQQT.
       F82PF-FN. EXIT.
       F82PB-FN. EXIT.
      *N82PP.    NOTE *DETERMIN SEMI-ANNUAL FREQUENCY     *.
       F82PP.         EXIT.                                             lv25
      *N82PT.    NOTE *CHECK IF MONTHS DON'T MATCH        *.            AAOLBB
       F82PT.    IF    7-82-WORK-MM NOT =                               lv30
                       7-82-ANN-MM                                      AAOLBB
                 NEXT SENTENCE ELSE GO TO     F82PT-FN.                 AAOLBB
                 IF    7-82-WORK-MM > 7-82-ANN-MM                       DOT
           SUBTRACT    7-82-ANN-MM FROM 7-82-WORK-MM                    AAOLBB
           DIVIDE      6 INTO 7-82-WORK-MM GIVING                       AAOLBB
           7-82-RESULT REMAINDER 7-82-REM                               AAOLBB
                 ELSE                                                   AAOLBB
           SUBTRACT    7-82-WORK-MM FROM 7-82-ANN-MM                    AAOLBB
           DIVIDE      6 INTO 7-82-ANN-MM GIVING                        AAOLBB
           7-82-RESULT REMAINDER 7-82-REM.                              AAOLBB
                 IF    7-82-REM NOT = ZERO                              DOT
                 AND   WZ63-CUPIQ = 'U'
      *ONLY OVERRIDE FREQ IF UPDATE
           MOVE        'N' TO WZ63-IFQSA.
       F82PT-FN. EXIT.
       F82PP-FN. EXIT.
       F82NT-FN. EXIT.
       F82JN-FN. EXIT.
       F82JM-FN. EXIT.
       F82JL-FN. EXIT.
       F82FA-FN. EXIT.
       F82-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N91.      NOTE *************************************.
      *               *                                   *
      *               *CALLED MODULES                     *
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
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *ACCESS TABLES                      *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92BA.    NOTE *RANDOM TABLE READ FOR BA98         *.            ADUTAB
       F92BA.                                                           lv10
           MOVE        'R1' TO G-BA98-TABFO                             ADUTAB
           COMPUTE     G-BA98-LTH = 60 + G-BA98-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-BA98-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-BA98)                                ADUTAB
                       LENGTH (G-BA98-LTH)                   END-EXEC.  ADUTAB
                 IF    G-BA98-TABCR NOT = '00'                          DOT
           PERFORM     F92BE THRU F92BE-FN.                             ADUTAB
       F92BA-FN. EXIT.
      *N92BD.    NOTE *MACRO AAOAG3  -  CALC CLIENT AGE   *.            AAOAG3
       F92BD.                                                           lv10
      *INITIALIZE WORK AREAS                                            AAOAG3
           MOVE        ZEROES TO 7-OAGE-CLIENT-AGE                      AAOAG3
           7-OAGE-AGE-DAYS                                              AAOAG3
           7-OAGE-AGE-YRS.                                              AAOAG3
                 IF    7-OAGE-CURRENT-DATE                              DOT
                       NOT NUMERIC                                      AAOAG3
                 OR    7-OAGE-CURRENT-DATE = ZEROES                     AAOAG3
                 OR    7-OAGE-BIRTH-DATE                                AAOAG3
                       NOT NUMERIC                                      AAOAG3
                 OR    7-OAGE-BIRTH-DATE = ZEROES                       AAOAG3
                 OR    7-OAGE-BD-CC = ZEROES                            AAOAG3
                 OR    7-OAGE-CD-CC = ZEROES                            AAOAG3
      *RETURN CLIENT AGE OF ZERO IF                                     AAOAG3
      *EITHER PASSED DATES ARE INVALID                                  AAOAG3
               GO TO     F92BD-FN.                                      AAOAG3
                 IF    7-OAGE-BIRTH-DATE >                              DOT
                       7-OAGE-CURRENT-DATE                              AAOAG3
      *RETURN CLIENT AGE OF ZERO IF                                     AAOAG3
      *BIRTH DATE > CURRENT DATE                                        AAOAG3
               GO TO     F92BD-FN.                                      AAOAG3
                 IF    7-OAGE-BD-DDD >                                  DOT
                       7-OAGE-CD-DDD                                    AAOAG3
      *SUBTRACT 1 FROM CURRENT DATE                                     AAOAG3
      *YEAR AND ADD 365 DAYS IF DAYS IN                                 AAOAG3
      *BIRTH DATE > CURRENT DATE DAYS                                   AAOAG3
           SUBTRACT    1 FROM 7-OAGE-CD-CCYY                            AAOAG3
           ADD         7-OAGE-DAYS-IN-A-YR TO                           AAOAG3
           7-OAGE-CD-DDD.                                               AAOAG3
      *CALCULATE AGE DAYS                                               DOT
           COMPUTE     7-OAGE-AGE-DAYS =                                AAOAG3
           7-OAGE-CD-DDD - 7-OAGE-BD-DDD.                               AAOAG3
                 IF    7-OAGE-AGE-DAYS NOT <                            DOT
                       7-OAGE-DAYS-IN-HALF-YR                           AAOAG3
      *ADD 1/2 YEAR TO CLIENT AGE                                       AAOAG3
           MOVE        7-OAGE-HALF-PERCENT TO                           AAOAG3
           7-OAGE-CLIENT-AGE.                                           AAOAG3
      *CALCULATE AGE YEARS                                              DOT
           COMPUTE     7-OAGE-AGE-YRS =                                 AAOAG3
           7-OAGE-CD-CCYY - 7-OAGE-BD-CCYY                              AAOAG3
      *ADD AGE YEARS TO CLIENT AGE                                      AAOAG3
           ADD         7-OAGE-AGE-YRS TO                                AAOAG3
           7-OAGE-CLIENT-AGE.                                           AAOAG3
       F92BD-FN. EXIT.
      *N92BE.    NOTE *TABLE READ ERROR PROCESS - TA98    *.
       F92BE.                                                           lv10
           MOVE        012092 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F92BE-FN. EXIT.
      *N92CA.    NOTE *RANDOM TABLE READ FOR CA5B         *.            ADUTAB
       F92CA.                                                           lv10
           MOVE        'R1' TO G-CA5B-TABFO                             ADUTAB
           COMPUTE     G-CA5B-LTH = 60 + G-CA5B-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-CA5B-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-CA5B)                                ADUTAB
                       LENGTH (G-CA5B-LTH)                   END-EXEC.  ADUTAB
                 IF    G-CA5B-TABCR NOT = '00'                          DOT
           PERFORM     F92CE THRU F92CE-FN.                             ADUTAB
       F92CA-FN. EXIT.
      *N92CE.    NOTE *TABLE READ ERROR PROCESS - TA5B    *.
       F92CE.                                                           lv10
           MOVE        012405 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F92CE-FN. EXIT.
      *N92IA.    NOTE *DETERMINE GRACE DATE               *.
       F92IA.         EXIT.                                             lv10
      *N92IC.    NOTE *SET GRACE DATE FOR TRADITIONAL     *.
       F92IC.    IF    CT01-PRCOD < 00199                               lv15
                 OR    (CT01-PRCOD > 00299
                 AND   CT01-PRCOD < 00600)
                 NEXT SENTENCE ELSE GO TO     F92IC-FN.
      *FORMAT CCYYMMDD; 100 = 1 MONTH
           COMPUTE     WZ63-ALDDUE = AA10-ALDDUE +
           100.
       F92IC-FN. EXIT.
      *N92ID.    NOTE *SET GRACE DATE FOR UL              *.
       F92ID.    IF    CT01-PRCOD > 00199                               lv15
                 AND   CT01-PRCOD < 00300
                 NEXT SENTENCE ELSE GO TO     F92ID-FN.
      *FORMAT CCYYMMDD; 200 = 2 MONTHS
           COMPUTE     WZ63-ALDDUE = AA10-ALDDUE +
           200.
       F92ID-FN. EXIT.
      *N92IG.    NOTE *IF GRACE DATE POPULATED            *.
       F92IG.    IF    WZ63-ALDDUE > ZERO                               lv15
                 NEXT SENTENCE ELSE GO TO     F92IG-FN.
      *N92II.    NOTE *IF WENT BEYOND YEAR END            *.
       F92II.    IF    WZ63-ALDDUE (5 : 2) = 13                         lv20
                 OR    WZ63-ALDDUE (5 : 2) = 14
                 NEXT SENTENCE ELSE GO TO     F92II-FN.
      ** FORMAT IS CCYYMMDD
      *- SUBTRACT 12 MONTHS
           COMPUTE     WZ63-ALDDUE = WZ63-ALDDUE -
           1200
      *- ADD 1 YEAR  (10000)
           COMPUTE     WZ63-ALDDUE = WZ63-ALDDUE +
           10000.
       F92II-FN. EXIT.
       F92IG-FN. EXIT.
       F92IA-FN. EXIT.
      *N92SA.    NOTE *RANDOM TABLE READ FOR SA72         *.            ADUTAB
       F92SA.                                                           lv10
           MOVE        'R1' TO G-SA72-TABFO                             ADUTAB
           COMPUTE     G-SA72-LTH = 60 + G-SA72-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-SA72-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-SA72)                                ADUTAB
                       LENGTH (G-SA72-LTH)                   END-EXEC.  ADUTAB
                 IF    G-SA72-TABCR NOT = '00'                          DOT
           PERFORM     F92SE THRU F92SE-FN.                             ADUTAB
       F92SA-FN. EXIT.
      *N92SE.    NOTE *TABLE READ ERROR PROCESS - TA72    *.
       F92SE.                                                           lv10
           MOVE        012631 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F92SE-FN. EXIT.
      *N92TA.    NOTE *RANDOM TABLE READ FOR TA71         *.            ADUTAB
       F92TA.                                                           lv10
           MOVE        'R1' TO G-TA71-TABFO                             ADUTAB
           COMPUTE     G-TA71-LTH = 60 + G-TA71-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA71-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA71)                                ADUTAB
                       LENGTH (G-TA71-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA71-TABCR NOT = '00'                          DOT
           PERFORM     F92TE THRU F92TE-FN.                             ADUTAB
       F92TA-FN. EXIT.
      *N92TE.    NOTE *TABLE READ ERROR PROCESS - TA71    *.
       F92TE.                                                           lv10
           MOVE        012118 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F92TE-FN. EXIT.
       F92-FN.   EXIT.
      *N93.      NOTE *************************************.            ADU129
      *               *                                   *             ADU129
      *               *---> Common DL/1 Error Checks      *             ADU129
      *               *                                   *             ADU129
      *               *************************************.            ADU129
       F93.           EXIT.                                             lv05
      *N93DD.    NOTE *DATE VALIDATION                    *.            AADA56
       F93DD.                                                           lv10
           MOVE        1 TO DEL-ER.                                     AADA56
                 IF    DD01-XDATG NOT NUMERIC                           DOT
           MOVE        4 TO DEL-ER                                      AADA56
               GO TO     F93DD-FN.                                      AADA56
                 IF    DD01-XDAT1 > '99'                                DOT
                 OR    DD01-XDAT1 < '18'                                AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F93DD-FN.                                      AADA56
                 IF    DD01-XDAT3 > '12'                                DOT
                 OR    DD01-XDAT3 = '00'                                AADA56
                 OR    DD01-XDAT4 > '31'                                AADA56
                 OR    DD01-XDAT4 = '00'                                AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F93DD-FN.                                      AADA56
                 IF    DD01-XDAT4 > '30'                                DOT
                 AND   (DD01-XDAT3 = '04'                               AADA56
                 OR    DD01-XDAT3 = '06'                                AADA56
                 OR    DD01-XDAT3 = '09'                                AADA56
                 OR    DD01-XDAT3 = '11')                               AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F93DD-FN.                                      AADA56
                 IF    DD01-XDAT3 NOT = '02'                            DOT
               GO TO     F93DD-FN.                                      AADA56
                 IF    DD01-XDAT4 > '29'                                DOT
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F93DD-FN.                                      AADA56
           MOVE        DD01-XDAT29 TO DD01-DTGYY                        DOT
           MOVE        DD01-XDAT19 TO DD01-DTGCC.                       AADA56
                 IF    DD01-DTGYY NOT = ZERO                            DOT
           COMPUTE     DD01-XLEAPY = DD01-DTGCY -                       AADA56
           ((DD01-DTGCY / 4) * 4)                                       AADA56
                 ELSE                                                   AADA56
           COMPUTE     DD01-XLEAPY = (DD01-DTGCY -                      AADA56
           ((DD01-DTGCY / 400) * 400))                                  AADA56
           / 100.                                                       AADA56
                 IF    DD01-XLEAPY NOT = ZERO                           DOT
                 AND   DD01-XDAT4 > '28'                                AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F93DD-FN.                                      AADA56
       F93DD-FN. EXIT.
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
      *N94A1.    NOTE *CALL GU ON AA10                    *.            ADU026
       F94A1.                                                           lv10
           MOVE        'LM1P' TO DE10-XDBDNM                            ADU026
           MOVE        'AA10' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XB06 AA10                                                    ADU026
           S-AAU10-SSA                                                  ADU026
           MOVE        XB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94A1-FN. EXIT.
      *N94A2.    NOTE *CALL GU ON AA20                    *.            ADU026
       F94A2.                                                           lv10
           MOVE        'LM1P' TO DE10-XDBDNM                            ADU026
           MOVE        'AA20' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XB06 AA20                                                    ADU026
           S-AAU10-SSA S-AA20-SSA                                       ADU026
           MOVE        XB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94A2-FN. EXIT.
      *N94A3.    NOTE *CALL GU ON AA66                    *.            ADU026
       F94A3.                                                           lv10
           MOVE        'LM1P' TO DE10-XDBDNM                            ADU026
           MOVE        'AA66' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XB06 AA66                                                    ADU026
           S-AAU10-SSA S-AA25-SSA S-AA66-SSA                            ADU026
           MOVE        XB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94A3-FN. EXIT.
      *N94A4.    NOTE *CALL GU ON AA85                    *.            ADU026
       F94A4.                                                           lv10
           MOVE        'LM1P' TO DE10-XDBDNM                            ADU026
           MOVE        'AA85' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XB06 AA85                                                    ADU026
           S-AAU10-SSA S-AA85-SSA                                       ADU026
           MOVE        XB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94A4-FN. EXIT.
      *N94CL.    NOTE *CALL GU ON CL01                    *.            ADU026
       F94CL.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XF06 CL01                                                    ADU026
           S-CLU01-SSA                                                  ADU026
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CL-FN. EXIT.
      *N94CM.    NOTE *CALL GN ON CL03                    *.            ADU026
       F94CM.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XF06 CL03                                                    ADU026
           S-CLU01-SSA S-CL03-SSA                                       ADU026
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CM-FN. EXIT.
      *N94C1.    NOTE *CALL GU ON CM01                    *.            ADU026
       F94C1.                                                           lv10
           MOVE        'CA1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CM01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XC06 CM01                                                    ADU026
           S-CMU01-SSA                                                  ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C1-FN. EXIT.
      *N94C2.    NOTE *CALL GN ON CM24                    *.            ADU026
       F94C2.                                                           lv10
           MOVE        'CA1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CM24' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XC06 CM24                                                    ADU026
           S-CMU01-SSA S-CM24-SSA                                       ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C2-FN. EXIT.
      *N94C3.    NOTE *CALL GN ON CM54                    *.            ADU026
       F94C3.                                                           lv10
           MOVE        'CA1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CM54' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XC06 CM54                                                    ADU026
           S-CMU01-SSA S-CM54-SSA                                       ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C3-FN. EXIT.
      *N94S1.    NOTE *CALL GN ON CX13                    *.            ADU026
       F94S1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XA06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CX13-SSA
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94S1-FN. EXIT.
      *N94S2.    NOTE *CALL GN ON CX14                    *.            ADU026
       F94S2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX14' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XA06 CX14                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           S-CX14-SSA
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94S2-FN. EXIT.
      *N94X2.    NOTE *CALL GN ON CX12                    *.            ADU026
       F94X2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XA06 CX12                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CX12-SSA
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94X2-FN. EXIT.
      *N94X3.    NOTE *CALL GU ON CX03                    *.            ADU026
       F94X3.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XA06 CX03                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94X3-FN. EXIT.
      *N94X4.    NOTE *CALL GU ON WX03                    *.            ADU026
       F94X4.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'WX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XA06 WX03                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94X4-FN. EXIT.
      *N94X6.    NOTE *CALL GN ON CX06                    *.            ADU026
       F94X6.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XA06 CX06                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94X6-FN. EXIT.
      *N94X8.    NOTE *CALL GN ON CX18                    *.            ADU026
       F94X8.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XA06 CX18                                                    ADU026
           S-CXU01-SSA S-CXU18-SSA                                      ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94X8-FN. EXIT.
      *N94Y1.    NOTE *CALL GU ON CX2Y                    *.            ADU026
       F94Y1.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XD06 CX2Y                                                    ADU026
           S-CXU2Y-SSA                                                  ADU026
           MOVE        XD06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94Y1-FN. EXIT.
      *N94Y2.    NOTE *CALL GN ON CX2Y                    *.            ADU026
       F94Y2.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XD06 CX2Y                                                    ADU026
           S-CX2Y-SSA                                                   ADU026
           MOVE        XD06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94Y2-FN. EXIT.
      *N97BB.    NOTE *ROUTINE TO CALL THE ONLINE         *.
       F97BB.                                                           lv10
      ** THIS SUBFUNCTION CALLS THE   *                                 ACMCTI
      ** ONLINE INQUIRY MODULE        *                                 ACMCTI
      ** (CI0361) TO RETRIEVE THE ROW *                                 ACMCTI
      ** INFORMATION FROM THE DB2     *                                 ACMCTI
      ** TABLE (TBS234) CORRESPONDING *                                 ACMCTI
      ** TO THE KEYS: 'DATE TYPE CODE'*                                 ACMCTI
      ** (ELEMENT: CEADC) AND         *                                 ACMCTI
      ** 'ACCOUNTING DATE'(ELEMENT:   *                                 ACMCTI
      ** DACTT).                      *                                 ACMCTI
      *********************************                                 ACMCTI
      *********************************                                 ACMCTI
      ** MOVE VALUES TO THE LINKAGE   *                                 ACMCTI
      ** AREA ELEMENTS TO BE PASSED   *                                 ACMCTI
      ** TO THE CALLED MODULE CI0361. *                                 ACMCTI
      *********************************                                 ACMCTI
      *********************************                                 ACMCTI
      ** CALL THE ONLINE INQUIRY      *                                 ACMCTI
      ** MODULE (CI0361).             *                                 ACMCTI
      *********************************                                 ACMCTI
           CALL        CI0361 USING                                     ACMCTI
           DFHEIBLK                                                     ACMCTI
           DFHCOMMAREA                                                  ACMCTI
           I93B.                                                        ACMCTI
      *N97CR.    NOTE *CHECK RETURN CODES                 *.
       F97CR.    IF    I93B-CRTNC = 11111                               lv15
                 OR    I93B-CRTNC = 22222
                 OR    I93B-CRTNC = 100
                 NEXT SENTENCE ELSE GO TO     F97CR-FN.
      *IF INVALID CODE, SEND ERROR MSG
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012786 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F97CR-FN. EXIT.
      *N97CU.    NOTE *REFORMAT DATE                      *.
       F97CU.                                                           lv15
           MOVE        I93B-GETIMM TO WS-GETIMM
           MOVE        WS-HH TO WS-MKT-HH
           MOVE        WS-MM TO WS-MKT-MM
           MOVE        WS-SS TO WS-MKT-SS.
       F97CU-FN. EXIT.
       F97BB-FN. EXIT.
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
           USING PE30 PE31.                                             AAZ000
                 IF    PE31-CVSTC NOT = SPACE                           DOT
      *---> CHECK FOR ERROR                                             AAZ000
           MOVE        '1' TO IK.
       F98AZ-FN. EXIT.
      *N98BB.    NOTE *CALL CI0400 FOR CNTB LIMIT         *.            AM0400
       F98BB.                                                           lv10
      *                                                                 AM0400
      *********************************                                 AM0400
      ** CALL THE DRIVER PROGRAM FOR  *                                 AM0400
      ** FINDING THE CONTRIBUTION     *                                 AM0400
      ** LIMIT                        *                                 AM0400
      *********************************                                 AM0400
      *INITIALIZE PASS AREA - K910
           INITIALIZE  K910
           MOVE        WZ63-DCACG (1:4) TO K910-DIRAYR
           MOVE        CT01-CQACT TO K910-CQACT
           MOVE        CT01-CIRAT TO K910-CIRAT
           MOVE        CT01-CIRAS TO K910-CIRAS
           MOVE        CL03-CLDOB TO K910-CLDOB
           MOVE        'CU' TO K910-CIRAP
           CALL        CI0400 USING                                     AM0400
           DFHEIBLK                                                     AM0400
           DFHCOMMAREA                                                  AM0400
           K910.                                                        AM0400
      *N98BD.    NOTE *ABEND FOR CRITICAL RETURN CODE     *.
       F98BD.    IF    K910-CSQLRC NOT = ZEROES                         lv15
                 AND   K910-CSQLRC NOT = +100
                 NEXT SENTENCE ELSE GO TO     F98BD-FN.
      *FORMAT THE ABEND CODE FROM
      *CI0400 AND ABEND.
           MOVE        K910-CSQLRC TO 7-WS-ABEND.
                 IF    K910-CSQLRC NEGATIVE                             DOT
           MOVE        '-' TO 7-WS-FIRST
                 ELSE
           MOVE        '+' TO 7-WS-FIRST.
           EXEC CICS   ABEND ABCODE (7-WS-ABEND)                        DOT
                       CANCEL                                END-EXEC.
       F98BD-FN. EXIT.
      *N98BG.    NOTE *ROW NOT IN TAX LAW QUAL TABLE      *.
       F98BG.    IF    K910-CSQLRC = +100                               lv15
                 NEXT SENTENCE ELSE GO TO     F98BG-FN.
      *ABEND FOR NO ROW FOUND IN THE
      *TAX LAW QUALIFIED AMOUNT TABLE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014311 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F98BG-FN. EXIT.
       F98BB-FN. EXIT.
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
