       IDENTIFICATION DIVISION.                                         CI0067
       PROGRAM-ID.  CI0067P.                                            CI0067
      *AUTHOR.         M\M - PAYOUT AMOUNT LIMITS.                      CI0067
      *DATE-COMPILED.   09/08/14.                                       CI0067
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
       ENVIRONMENT DIVISION.                                            CI0067
       CONFIGURATION SECTION.                                           CI0067
       SOURCE-COMPUTER. IBM-370.                                        CI0067
       OBJECT-COMPUTER. IBM-370.                                        CI0067
       DATA DIVISION.                                                   CI0067
       WORKING-STORAGE SECTION.                                         CI0067
       01                 AA10.                                         CI0067
            10            AA10-AE00.                                    CI0067
            11            AA10-ALCIDN PICTURE  9(11).                   CI0067
            10            AA10-AE01.                                    CI0067
            11            AA10-FILLER PICTURE  X(12).                   CI0067
            11            AA10-DLAUP  PICTURE  9(8).                    CI0067
            11            AA10-FILLER PICTURE  S9(07)                   CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-FILLER PICTURE  9(8).                    CI0067
            11            AA10-FILLER PICTURE  S9(07)                   CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-FILLER PICTURE  9(8).                    CI0067
            11            AA10-FILLER PICTURE  S9(07)                   CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-FILLER PICTURE  X(311).                  CI0067
            10            AA10-AE02                                     CI0067
                          REDEFINES            AA10-AE01.               CI0067
            11            AA10-FILLER PICTURE  9.                       CI0067
            11            AA10-ALCOMP PICTURE  99.                      CI0067
            11            AA10-CRTYP  PICTURE  9(4).                    CI0067
            11            AA10-FILLER PICTURE  9.                       CI0067
            11            AA10-CPOST  PICTURE  99.                      CI0067
            11            AA10-GEHCDI PICTURE  9(3).                    CI0067
            11            AA10-FILLER PICTURE  9(8).                    CI0067
            11            AA10-FILLER PICTURE  9(7).                    CI0067
            11            AA10-FILLER PICTURE  X.                       CI0067
            11            AA10-ALPLDT PICTURE  9(8).                    CI0067
            11            AA10-DENEX  PICTURE  9(8).                    CI0067
            11            AA10-CENXC1 PICTURE  9(3).                    CI0067
            11            AA10-FILLER PICTURE  99.                      CI0067
            11            AA10-CROOR  PICTURE  99.                      CI0067
            11            AA10-CREIN  PICTURE  99.                      CI0067
            11            AA10-FILLER PICTURE  X.                       CI0067
            11            AA10-ALAPST PICTURE  99.                      CI0067
            11            AA10-ALSTSA PICTURE  XX.                      CI0067
            11            AA10-FILLER PICTURE  99.                      CI0067
            11            AA10-CPCAL  PICTURE  9.                       CI0067
            11            AA10-CNAEX  PICTURE  9.                       CI0067
            11            AA10-CSUSI  PICTURE  99.                      CI0067
            11            AA10-FILLER PICTURE  99.                      CI0067
            11            AA10-FILLER PICTURE  99.                      CI0067
            11            AA10-FILLER PICTURE  9(8).                    CI0067
            11            AA10-FILLER PICTURE  9.                       CI0067
            11            AA10-FILLER PICTURE  X(10).                   CI0067
            11            AA10-FILLER PICTURE  99.                      CI0067
            11            AA10-FILLER PICTURE  X.                       CI0067
            11            AA10-FILLER PICTURE  99.                      CI0067
            11            AA10-CAPL   PICTURE  9.                       CI0067
            11            AA10-FILLER PICTURE  9.                       CI0067
            11            AA10-FILLER PICTURE  999.                     CI0067
            11            AA10-FILLER PICTURE  999.                     CI0067
            11            AA10-FILLER PICTURE  999.                     CI0067
            11            AA10-CSTWH  PICTURE  9(8).                    CI0067
            11            AA10-FILLER PICTURE  9(8).                    CI0067
            11            AA10-FILLER PICTURE  99.                      CI0067
            11            AA10-FILLER PICTURE  9.                       CI0067
            11            AA10-FILLER PICTURE  9.                       CI0067
            11            AA10-FILLER PICTURE  99.                      CI0067
            11            AA10-FILLER PICTURE  999.                     CI0067
            11            AA10-FILLER PICTURE  999.                     CI0067
            11            AA10-CPRPM  PICTURE  9(3).                    CI0067
            11            AA10-FILLER PICTURE  99.                      CI0067
            11            AA10-FILLER PICTURE  9(6).                    CI0067
            11            AA10-FILLER PICTURE  99.                      CI0067
            11            AA10-FILLER PICTURE  999.                     CI0067
            11            AA10-DTRCM  PICTURE  9(8).                    CI0067
            11            AA10-DLATR  PICTURE  9(8).                    CI0067
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-ALPMOD PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-CRSBN  PICTURE  99.                      CI0067
            11            AA10-FILLER PICTURE  X.                       CI0067
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-FILLER PICTURE  9.                       CI0067
            11            AA10-FILLER PICTURE  X.                       CI0067
            11            AA10-FILLER PICTURE  XX.                      CI0067
            11            AA10-FILLER PICTURE  99.                      CI0067
            11            AA10-CTRHO  PICTURE  9(8).                    CI0067
            11            AA10-FILLER PICTURE  9.                       CI0067
            11            AA10-FILLER PICTURE  99.                      CI0067
            11            AA10-FILLER PICTURE  99.                      CI0067
            11            AA10-CTSGD  PICTURE  9(8).                    CI0067
            11            AA10-IANRD  PICTURE  9.                       CI0067
            11            AA10-ALINNO PICTURE  99.                      CI0067
            11            AA10-ALSANN PICTURE  9(5).                    CI0067
            11            AA10-FILLER PICTURE  S9(9)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-ALPAGR PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-ALRISK PICTURE  S9(9)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-ALAPIT PICTURE  S9(07)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-FILLER PICTURE  X.                       CI0067
            11            AA10-CADPR  PICTURE  9.                       CI0067
            11            AA10-AAPRT  PICTURE  S9(07)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-NREPN1 PICTURE  9(06).                   CI0067
            11            AA10-CCST1  PICTURE  9.                       CI0067
            11            AA10-CESRD  PICTURE  9(3).                    CI0067
            11            AA10-ALLRT  PICTURE  S9V99                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-ANTPAA PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-ALDDUE PICTURE  9(08).                   CI0067
            11            AA10-ALMODE PICTURE  99.                      CI0067
            11            AA10-FILLER PICTURE  X(08).                   CI0067
            11            AA10-CTCUS1 PICTURE  99.                      CI0067
            11            AA10-CNPPR  PICTURE  9(03).                   CI0067
            11            AA10-FILLER PICTURE  9.                       CI0067
            11            AA10-FILLER PICTURE  9(03).                   CI0067
            11            AA10-ITMEC  PICTURE  X(1).                    CI0067
            11            AA10-IMCDI  PICTURE  X.                       CI0067
            11            AA10-LSIDTE PICTURE  9(08).                   CI0067
            11            AA10-PLINE  PICTURE  S9V99                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-ATSA8  PICTURE  S9(07)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-ATSA9  PICTURE  S9(05)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-CRATS  PICTURE  X.                       CI0067
            11            AA10-PPTKN  PICTURE  S9(3)V9(6)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            AA10-FILLER PICTURE  X(24).                   CI0067
       01                 AA20.                                         CI0067
            10            AA20-CLTRN  PICTURE  999.                     CI0067
            10            AA20-DLACC  PICTURE  9(8).                    CI0067
            10            AA20-ALDUED PICTURE  9(8).                    CI0067
            10            AA20-DLTRN  PICTURE  9(8).                    CI0067
            10            AA20-ALLNB  PICTURE  S9(07)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA20-ALLPA  PICTURE  S9(07)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA20-FILLER PICTURE  9(06).                   CI0067
            10            AA20-FILLER PICTURE  S9(05)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA20-FILLER PICTURE  S9(05)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA20-FILLER PICTURE  9(06).                   CI0067
            10            AA20-FILLER PICTURE  S9(07)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA20-FILLER PICTURE  S9(07)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
       01                 AA25.                                         CI0067
            10            AA25-ANPTT  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-APCLO  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-APTXL  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-AVARP  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-AVARN  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-APCLD  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-AVART  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-AVLSC  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-AFLSC  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ALUNIT PICTURE  S9(8)V999                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-CSURV  PICTURE  99.                      CI0067
            10            AA25-CFGLC  PICTURE  9(6).                    CI0067
            10            AA25-CMINP  PICTURE  9.                       CI0067
            10            AA25-APPYP  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-APPY2  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-APPY3  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-DSIDF  PICTURE  9(8).                    CI0067
            10            AA25-ALYTDF PICTURE  S9(07)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-DSIDV  PICTURE  9(8).                    CI0067
            10            AA25-ALYTDV PICTURE  S9(07)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-DTRNF  PICTURE  9(8).                    CI0067
            10            AA25-AREF   PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-DTRLA  PICTURE  9(8).                    CI0067
            10            AA25-DTRLT  PICTURE  9(8).                    CI0067
            10            AA25-CTRNL  PICTURE  9(4).                    CI0067
            10            AA25-ATRNL  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ATRCV  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ATRCI  PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-QTRAU  PICTURE  S9(8)V999                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ATRCT  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ATCTI  PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-QTRTA  PICTURE  S9(8)V999                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-IOMIT  PICTURE  9.                       CI0067
            10            AA25-DPAPR  PICTURE  9(8).                    CI0067
            10            AA25-DCAPR  PICTURE  9(8).                    CI0067
            10            AA25-PVAP   PICTURE  S9(3)V9(4)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ALPALC PICTURE  S9(04)V999               CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-PAACF  PICTURE  S9(2)V999                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-NBUID  PICTURE  S999                     CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ABUCV  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ANFMC  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ANFMP  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-DLACD  PICTURE  9(8).                    CI0067
            10            AA25-DPACD  PICTURE  9(8).                    CI0067
            10            AA25-CBASP  PICTURE  9.                       CI0067
            10            AA25-FILLER PICTURE  9.                       CI0067
            10            AA25-APYTI  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-FILLER PICTURE  X(2).                    CI0067
            10            AA25-GNPCA.                                   CI0067
            11            AA25-CPRDE  PICTURE  99.                      CI0067
            11            AA25-CPFGH  PICTURE  999.                     CI0067
            11            AA25-CPRI   PICTURE  9.                       CI0067
            11            AA25-CPRJ   PICTURE  9.                       CI0067
            11            AA25-CPKL   PICTURE  99.                      CI0067
            10            AA25-CLPRC  PICTURE  9(7).                    CI0067
            10            AA25-AQPAP  PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-AQPTC  PICTURE  S9(7)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ATPRC  PICTURE  S9(7)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ATASP  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ANPIP  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-DNPCA  PICTURE  9(8).                    CI0067
            10            AA25-DLAPC  PICTURE  9(8).                    CI0067
            10            AA25-APRCR  PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-AMATP  PICTURE  S9(7)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-AOGAP  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-APYCV  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-AACV   PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-AAICV  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ACVCY  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ACVPY  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-FILLER PICTURE  S9(6)V999                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ATACV  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ATCVI  PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ACCYV  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-APCYV  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-CES79  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ARAPI  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-CARDC  PICTURE  99.                      CI0067
            10            AA25-PGAIR  PICTURE  SV999                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ASPAP  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ATFCV  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ATFCI  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-QTFAU  PICTURE  S9(6)V999                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ATFPI  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-CWITH  PICTURE  99.                      CI0067
            10            AA25-ATCBC  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ATCEP  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ATLTC  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ATRCIT PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-QTRAUT PICTURE  S9(8)V999                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-DINNO  PICTURE  9(8).                    CI0067
            10            AA25-CSIRAC PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-CSIRAP PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-AIRALC PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-AIPYT  PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-DLDIC  PICTURE  9(4).                    CI0067
            10            AA25-AROLC  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-AROLP  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA25-ISEPP  PICTURE  X.                       CI0067
            10            AA25-IDEMR  PICTURE  X(1).                    CI0067
            10            AA25-FILLER PICTURE  X(139).                  CI0067
       01                 AA66.                                         CI0067
            10            AA66-CLTRN  PICTURE  999.                     CI0067
            10            AA66-DLACC  PICTURE  9(8).                    CI0067
            10            AA66-DLPAD  PICTURE  9(08).                   CI0067
            10            AA66-DLTRNL PICTURE  9(8).                    CI0067
            10            AA66-DLTRN  PICTURE  9(8).                    CI0067
            10            AA66-ALDUED PICTURE  9(8).                    CI0067
            10            AA66-ALLNB  PICTURE  S9(07)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA66-ALLPA  PICTURE  S9(07)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA66-ALLIP  PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA66-ALADJL PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA66-QLLNT  PICTURE  99.                      CI0067
            10            AA66-ILNST  PICTURE  X.                       CI0067
            10            AA66-PVPAP  PICTURE  S9(04)V999               CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA66-DCAPR  PICTURE  9(8).                    CI0067
            10            AA66-PVPAPP PICTURE  S9(4)V9(3)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA66-DPAPR  PICTURE  9(8).                    CI0067
            10            AA66-ALADJF PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA66-DTRNFD PICTURE  9(8).                    CI0067
            10            AA66-ANIFN  PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA66-AIREF  PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA66-FILLER PICTURE  X(8).                    CI0067
       01                 AA85.                                         CI0067
            10            AA85-DUWAC  PICTURE  9(8).                    CI0067
            10            AA85-CUWAC  PICTURE  99.                      CI0067
            10            AA85-CRFAC  PICTURE  9(6).                    CI0067
            10            AA85-IIAAF  PICTURE  9.                       CI0067
            10            AA85-DPOLI  PICTURE  9(8).                    CI0067
            10            AA85-DCONM  PICTURE  9(8).                    CI0067
            10            AA85-DBYR   PICTURE  99.                      CI0067
            10            AA85-DLMED  PICTURE  9(8).                    CI0067
            10            AA85-LSIDTE PICTURE  9(08).                   CI0067
            10            AA85-GESTD  PICTURE  9(8).                    CI0067
            10            AA85-CRBTR  PICTURE  9(6).                    CI0067
            10            AA85-CBTR   PICTURE  X.                       CI0067
            10            AA85-PSBTR  PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA85-ANTPF  PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA85-QDYT1  PICTURE  99.                      CI0067
            10            AA85-AEPT1  PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA85-QDYT2  PICTURE  99.                      CI0067
            10            AA85-AEPT2  PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA85-CRFER  PICTURE  9(6).                    CI0067
            10            AA85-PRADB  PICTURE  S99V9                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA85-PRWP   PICTURE  S99V9                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA85-PRNOM  PICTURE  S99V9                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA85-CDETH  PICTURE  X.                       CI0067
            10            AA85-CRFSP  PICTURE  X(6).                    CI0067
            10            AA85-CREDI  PICTURE  X(16).                   CI0067
            10            AA85-CRBD   PICTURE  X(6).                    CI0067
            10            AA85-CFRTR  PICTURE  X.                       CI0067
            10            AA85-ANTPG  PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA85-ATEPF  PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA85-CRFRT  PICTURE  X(6).                    CI0067
            10            AA85-QDYT3  PICTURE  99.                      CI0067
            10            AA85-CRFEX  PICTURE  9(6).                    CI0067
            10            AA85-AAPEB  PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA85-AAPER  PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA85-ARGAP  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            AA85-CWAE   PICTURE  XX.                      CI0067
            10            AA85-IMIBP  PICTURE  X.                       CI0067
            10            AA85-CNIR   PICTURE  XX.                      CI0067
            10            AA85-CMINM  PICTURE  XX.                      CI0067
      *!WF DSP=PE DSL=PE SEL=3031 FOR=I DES=1 LEV=1                     AAZ000
       01                 PE22.                                         CI0067
            10            PE22-C299.                                    CI0067
            11            PE22-CTID.                                    CI0067
            12            PE22-CTIDA  PICTURE  9(3).                    CI0067
            12            PE22-CTIDN.                                   CI0067
            13            PE22-CTIDNP PICTURE  X(13).                   CI0067
            13            PE22-CTIDND PICTURE  9(11).                   CI0067
            10            PE22-PRCOD  PICTURE  9(5).                    CI0067
            10            PE22-CTSTA  PICTURE  99.                      CI0067
            10            PE22-MAPPN  PICTURE  X(10).                   CI0067
            10            PE22-FILLER PICTURE  X(088).                  CI0067
            10            PE22-IARRGA PICTURE  X.                       CI0067
            10            PE22-IARLNA PICTURE  X.                       CI0067
            10            PE22-CELBL  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PE22-AMINAL PICTURE  S9(7)V99.                CI0067
            10            PE22-AMAXAL PICTURE  S9(7)V99.                CI0067
            10            PE22-AMIND  PICTURE  S9(7)V99.                CI0067
            10            PE22-AMAXAR PICTURE  S9(7)V99.                CI0067
            10            PE22-IARPSA PICTURE  X.                       CI0067
            10            PE22-CELBA  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PE22-INPAY  PICTURE  X(01).                   CI0067
            10            PE22-AMINAN PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PE22-AMAXAN PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PE22-FILLER PICTURE  X(042).                  CI0067
       01                 PE30.                                         CI0067
            10            PE30-XPROPT PICTURE  X(04).                   CI0067
            10            PE30-GVSGK  PICTURE  X(87).                   CI0067
            10            PE30-CVACO  PICTURE  X(3).                    CI0067
            10            PE30-GVAID  PICTURE  X(15).                   CI0067
            10            PE30-CTHCO  PICTURE  X(4).                    CI0067
            10            PE30-IDBCD  PICTURE  X(1).                    CI0067
            10            PE30-IDBMF  PICTURE  X(1).                    CI0067
            10            PE30-IDBNO  PICTURE  X(1).                    CI0067
            10            PE30-IDBNI  PICTURE  X(1).                    CI0067
            10            PE30-IDBTH  PICTURE  X(1).                    CI0067
            10            PE30-IDBSW  PICTURE  X(1).                    CI0067
            10            PE30-CPOLV  PICTURE  X.                       CI0067
            10            PE30-DEFFT  PICTURE  9(8).                    CI0067
            10            PE30-IIRSO  PICTURE  X(1).                    CI0067
            10            PE30-IMVAO  PICTURE  X(1).                    CI0067
            10            PE30-CMEMO  PICTURE  X(2).                    CI0067
            10            PE30-IVOGN  PICTURE  X.                       CI0067
            10            PE30-APRGT  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PE30-CDSTV  PICTURE  X.                       CI0067
            10            PE30-IDBUV  PICTURE  X(1).                    CI0067
            10            PE30-IDBAI  PICTURE  X(1).                    CI0067
            10            PE30-IDBRI  PICTURE  X(1).                    CI0067
            10            PE30-IDBCO  PICTURE  X(1).                    CI0067
            10            PE30-IDBCA  PICTURE  X(1).                    CI0067
            10            PE30-IDBBI  PICTURE  X(1).                    CI0067
            10            PE30-IDBLO  PICTURE  X(1).                    CI0067
            10            PE30-IDBFD  PICTURE  X(1).                    CI0067
            10            PE30-IDBPY  PICTURE  X(1).                    CI0067
            10            PE30-IDBRP  PICTURE  X(1).                    CI0067
            10            PE30-IDBRE  PICTURE  X(1).                    CI0067
            10            PE30-CFNDC  PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PE30-CCLTP1 PICTURE  X(2).                    CI0067
            10            PE30-CDSTV3 PICTURE  X(3).                    CI0067
            10            PE30-IVIVS  PICTURE  X.                       CI0067
            10            PE30-IDBSR  PICTURE  X(2).                    CI0067
            10            PE30-CPNOP  PICTURE  X(2).                    CI0067
            10            PE30-IDDTH  PICTURE  X(1).                    CI0067
            10            PE30-CLDOD  PICTURE  9(8).                    CI0067
            10            PE30-IDBIF  PICTURE  X(1).                    CI0067
            10            PE30-IDBMA  PICTURE  X(1).                    CI0067
            10            PE30-CCOUL  PICTURE  XX.                      CI0067
            10            PE30-FILLER PICTURE  X(298).                  CI0067
       01                 PE31.                                         CI0067
            10            PE31-CVSTC  PICTURE  X(4).                    CI0067
            10            PE31-GVSGK  PICTURE  X(87).                   CI0067
            10            PE31-QFNDO  PICTURE  S9(4)                    CI0067
                          BINARY.                                       CI0067
            10            PE31-PE32.                                    CI0067
            11            PE31-PE65.                                    CI0067
            12            PE31-CVAPC  PICTURE  X(6).                    CI0067
            12            PE31-CVALB  PICTURE  X(3).                    CI0067
            12            PE31-CASTA  PICTURE  X.                       CI0067
            12            PE31-CTWST1 PICTURE  X(3).                    CI0067
            12            PE31-CPISC  PICTURE  X(3).                    CI0067
            12            PE31-ALPLDT PICTURE  9(8).                    CI0067
            12            PE31-DEFFT  PICTURE  9(8).                    CI0067
            12            PE31-DANNI  PICTURE  9(8).                    CI0067
            12            PE31-DTPMT  PICTURE  9(8).                    CI0067
            12            PE31-ITAMR  PICTURE  X(1).                    CI0067
            12            PE31-AGAPA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ATRPA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ATROP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ITMECC PICTURE  X(1).                    CI0067
            12            PE31-CBPLCA PICTURE  X.                       CI0067
            12            PE31-CPRCC  PICTURE  X.                       CI0067
            12            PE31-CLSEX  PICTURE  X.                       CI0067
            12            PE31-QPOIA  PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-IREIA  PICTURE  X.                       CI0067
            12            PE31-APCUA  PICTURE  S9(6)V9(5)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AGLPA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AGSPA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ATGPA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ACBIN1 PICTURE  S9(09)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CABRC  PICTURE  X.                       CI0067
            12            PE31-CDSON  PICTURE  X(06).                   CI0067
            12            PE31-CADVN  PICTURE  X(10).                   CI0067
            12            PE31-CVOMC1 PICTURE  X(1).                    CI0067
            12            PE31-CSSUP2 PICTURE  X.                       CI0067
            12            PE31-ACECP  PICTURE  S9(09)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CCOFE  PICTURE  X.                       CI0067
            12            PE31-GRIDN7 PICTURE  S9(7)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-QNZDS  PICTURE  S9(04)                   CI0067
                          COMPUTATIONAL   SYNC RIGHT.                   CI0067
            12            PE31-QTNOL  PICTURE  S9(05)                   CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-QTNPW  PICTURE  S9(05)                   CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-IREIN  PICTURE  X.                       CI0067
            12            PE31-DNRIP  PICTURE  9(8).                    CI0067
            12            PE31-ATDPA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CREPC  PICTURE  XX.                      CI0067
            12            PE31-CMLTP  PICTURE  X.                       CI0067
            12            PE31-IREPL6 PICTURE  X.                       CI0067
            12            PE31-CCLAC  PICTURE  X.                       CI0067
            12            PE31-ALNTI  PICTURE  S9(09)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ATIPA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CVASC  PICTURE  XX.                      CI0067
            12            PE31-DRTHC  PICTURE  9(8).                    CI0067
            12            PE31-CENDO  PICTURE  X.                       CI0067
            12            PE31-ICOEX  PICTURE  X(1).                    CI0067
            12            PE31-INURS  PICTURE  X(1).                    CI0067
            12            PE31-ITRML  PICTURE  X(1).                    CI0067
            12            PE31-IBIRA  PICTURE  X(1).                    CI0067
            12            PE31-CCOUL  PICTURE  XX.                      CI0067
            12            PE31-ASGLP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-DMATUR PICTURE  9(8).                    CI0067
            12            PE31-CJOIP  PICTURE  X.                       CI0067
            12            PE31-CPNOP  PICTURE  X(2).                    CI0067
            12            PE31-ARBRP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ACCHV  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ASCHV  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CGMBR  PICTURE  X.                       CI0067
            12            PE31-AMCTV  PICTURE  S9(7)V99.                CI0067
            12            PE31-ACBIN2 PICTURE  S9(09)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PE1B                                     CI0067
                          REDEFINES            PE31-ACBIN2.             CI0067
            13            PE31-NAAMC  PICTURE  9(2).                    CI0067
            13            PE31-PNPCT  PICTURE  999.                     CI0067
            13            PE31-FILLER PICTURE  X(1).                    CI0067
            11            PE31-PE75                                     CI0067
                          REDEFINES            PE31-PE65.               CI0067
            12            PE31-CASTAR PICTURE  X.                       CI0067
            12            PE31-DLPLDA PICTURE  9(8).                    CI0067
            12            PE31-ATRPAR PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-DSTT1  PICTURE  9(8).                    CI0067
            12            PE31-CTLPD  PICTURE  9(8).                    CI0067
            12            PE31-DNPMT  PICTURE  9(8).                    CI0067
            12            PE31-ALFXP  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CFRQZ  PICTURE  X.                       CI0067
            12            PE31-ASTXW1 PICTURE  S9(13)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ATWHDD PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-APOCY  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-APOPY  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-APOTD  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-TPLNL  PICTURE  X(30).                   CI0067
            12            PE31-NPSTA  PICTURE  X(11).                   CI0067
            12            PE31-CPSTP  PICTURE  X(6).                    CI0067
            12            PE31-DPSTI  PICTURE  9(8).                    CI0067
            12            PE31-IFXVR  PICTURE  X.                       CI0067
            12            PE31-IDTHI  PICTURE  X.                       CI0067
            12            PE31-PFPAY  PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PVPAY  PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CPURF  PICTURE  X(5).                    CI0067
            12            PE31-ATAXY  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CVALB1 PICTURE  X(4).                    CI0067
            12            PE31-CRATC1 PICTURE  X.                       CI0067
            12            PE31-PSTIN  PICTURE  9(3)V99                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PFTXW  PICTURE  9(3)V99                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PSTXW  PICTURE  9(3)V99                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PFERT  PICTURE  9(3)V99                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AFEDV  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PSERT  PICTURE  9(3)V99                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ASEDV  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-DPRPA  PICTURE  9(8).                    CI0067
            12            PE31-ALVXP  PICTURE  S9(7)V9(4)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ISTWH1 PICTURE  X(1).                    CI0067
            12            PE31-FILLER PICTURE  X(44).                   CI0067
            11            PE31-PE67.                                    CI0067
            12            PE31-DMDOB1 PICTURE  X(8).                    CI0067
            12            PE31-CTNAM  PICTURE  X(01).                   CI0067
            12            PE31-MCOMP                                    CI0067
                          OCCURS       002     TIMES.                   CI0067
            13            PE31-MINDL  PICTURE  X(35).                   CI0067
            13            PE31-MINDF  PICTURE  X(25).                   CI0067
            13            PE31-MINDP  PICTURE  X(10).                   CI0067
            13            PE31-MINDS  PICTURE  X(10).                   CI0067
            12            PE31-MADR1V PICTURE  X(35)                    CI0067
                          OCCURS       004     TIMES.                   CI0067
            12            PE31-MCIT   PICTURE  X(30).                   CI0067
            12            PE31-CPISC  PICTURE  X(3).                    CI0067
            12            PE31-CZIPC  PICTURE  X(15).                   CI0067
            12            PE31-CLSEX1 PICTURE  X.                       CI0067
            11            PE31-PE88.                                    CI0067
            12            PE31-QNSUR  PICTURE  S9(3)                    CI0067
                          BINARY.                                       CI0067
            12            PE31-QTSUR  PICTURE  S9(3)                    CI0067
                          BINARY.                                       CI0067
            12            PE31-ACGPA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ATOTA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PE8C                                     CI0067
                          OCCURS       010     TIMES.                   CI0067
            13            PE31-ALHDTE PICTURE  9(08).                   CI0067
            13            PE31-ATWHDE PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ASTXW  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ACHKJ  PICTURE  S9(09)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-CVTXN1 PICTURE  X(4).                    CI0067
            13            PE31-ACACTV PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ASPAM1 PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-CSSVL  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-PE86.                                    CI0067
            12            PE31-CVOOD  PICTURE  S9(5)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-DEIRNB PICTURE  9(8).                    CI0067
            12            PE31-QCRPD  PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CBRIT  PICTURE  SV9(5)                   CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-PE91.                                    CI0067
            12            PE31-CVOOD1 PICTURE  S9(5)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-DTRMSX PICTURE  X(8).                    CI0067
            12            PE31-DTRMEX PICTURE  X(8).                    CI0067
            12            PE31-ALPRUN PICTURE  S999V9(6)                CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PSPSX                                    CI0067
                          REDEFINES            PE31-ALPRUN              CI0067
               PICTURE    S9(7)V99                                      CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PIXPR  PICTURE  S9V9(4)                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-PE6B.                                    CI0067
            12            PE31-CEBMO  PICTURE  9(2).                    CI0067
            12            PE31-CBNBC1 PICTURE  X.                       CI0067
            12            PE31-AMDAR  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ALDDUE PICTURE  9(08).                   CI0067
            12            PE31-ASPAM2 PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-DRESE  PICTURE  9(8).                    CI0067
            12            PE31-DACUP  PICTURE  9(02).                   CI0067
            12            PE31-ACVAMG PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ISTWH  PICTURE  X(1).                    CI0067
            12            PE31-AVAIP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-FILLER PICTURE  X(63).                   CI0067
            11            PE31-PE6C.                                    CI0067
            12            PE31-ARGPA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AROGP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ACYTA  PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-FILLER PICTURE  X(050).                  CI0067
            11            PE31-PE6R.                                    CI0067
            12            PE31-CRTYC  PICTURE  X(1).                    CI0067
            12            PE31-CRICO  PICTURE  X(3).                    CI0067
            11            PE31-PE3V.                                    CI0067
            12            PE31-PE90.                                    CI0067
            13            PE31-CSTCVE PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ASURR1 PICTURE  S9(09)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ALSURR PICTURE  S9(09)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ASINTC PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-AMVA1  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ASPAM  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ACVAM  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-AMSBT  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ACVALC PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ATWS   PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ISELO  PICTURE  X.                       CI0067
            13            PE31-FILLER PICTURE  X(1).                    CI0067
            13            PE31-FILLER PICTURE  X(2).                    CI0067
            13            PE31-ASTCV1 PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ATFCVC PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-FILLER PICTURE  X(1).                    CI0067
            13            PE31-FILLER PICTURE  X(2).                    CI0067
            13            PE31-AUINTA PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-FILLER PICTURE  X(1).                    CI0067
            13            PE31-IRCHG  PICTURE  X.                       CI0067
            13            PE31-ASTXW8 PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ATFRA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-APLIV  PICTURE  S9(09)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-PCIRB1 PICTURE  S9(2)V9(3)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ACVAMF PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-CSNCVE PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-PCIRB7 PICTURE  99V999.                  CI0067
            13            PE31-AMNSR  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-AMINL  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ASCHV  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-PRCHG  PICTURE  999V999                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ISTUR  PICTURE  X.                       CI0067
            13            PE31-CSTIM  PICTURE  X.                       CI0067
            13            PE31-AMCAV1 PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ARCHG  PICTURE  9(7)V99                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PE7C                                     CI0067
                          REDEFINES            PE31-PE90.               CI0067
            13            PE31-ACOGR  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ACONE  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ASURR3 PICTURE  S9(07)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-PSURR1 PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ACOTX  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-TCOMG  PICTURE  X(30).                   CI0067
            13            PE31-CVCST  PICTURE  X(4).                    CI0067
            13            PE31-FILLER PICTURE  X(100).                  CI0067
            12            PE31-ALCCV  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ATLPD  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ARTLP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-APRLP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ALPAY  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-DLSST  PICTURE  9(8).                    CI0067
            12            PE31-ACAUN  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-PE6W.                                    CI0067
            12            PE31-PCIRA  PICTURE  S99V999                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PCIRB  PICTURE  S99V999                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PE6G                                     CI0067
                          OCCURS       099     TIMES.                   CI0067
            13            PE31-NFUNDV PICTURE  S999                     CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-CFUNT  PICTURE  X.                       CI0067
            13            PE31-AFUNA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-QFUNT                                    CI0067
                          REDEFINES            PE31-AFUNA               CI0067
               PICTURE    S9(7)V9(4)                                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-AFUUV  PICTURE  S9(3)V9(6)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-AFUNV  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-AFUEI  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-PE1L.                                    CI0067
            12            PE31-PE6L.                                    CI0067
            13            PE31-DLTRN  PICTURE  9(8).                    CI0067
            13            PE31-IVOGN  PICTURE  X.                       CI0067
            13            PE31-DTLPA  PICTURE  9(8).                    CI0067
            13            PE31-ARTLP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ALBUL  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-CLOFC  PICTURE  S9(5)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ILNST  PICTURE  X.                       CI0067
            12            PE31-AIELN  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-FILLER PICTURE  X(23).                   CI0067
            11            PE31-PE64.                                    CI0067
            12            PE31-AXUNVP PICTURE  S9(3)V9(6)               CI0067
                          OCCURS       998     TIMES                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-PE6A.                                    CI0067
            12            PE31-FILLER PICTURE  X(310).                  CI0067
            12            PE31-PE63                                     CI0067
                          OCCURS       099     TIMES.                   CI0067
            13            PE31-NVAFN  PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-CFUNT  PICTURE  X.                       CI0067
            13            PE31-PINFA6 PICTURE  S9(4)V9(1)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-FILLER PICTURE  X(18).                   CI0067
            13            PE31-ACTFA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-FILLER PICTURE  X(62).                   CI0067
            11            PE31-PE3D.                                    CI0067
            12            PE31-PE89.                                    CI0067
            13            PE31-CVOFC  PICTURE  S9(5)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-DEFFT  PICTURE  9(8).                    CI0067
            13            PE31-CVTXN  PICTURE  X(4).                    CI0067
            13            PE31-APRGT  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-CAORT  PICTURE  X(01).                   CI0067
            13            PE31-CVAST1 PICTURE  X.                       CI0067
            13            PE31-CVAST  PICTURE  X.                       CI0067
            13            PE31-CAMCN  PICTURE  X(8).                    CI0067
            13            PE31-CIRAR1 PICTURE  X.                       CI0067
            13            PE31-INQEX  PICTURE  X.                       CI0067
            13            PE31-CVAPC1 PICTURE  X(6).                    CI0067
            13            PE31-CMEMO  PICTURE  X(2).                    CI0067
            13            PE31-DEFFC  PICTURE  9(8).                    CI0067
            11            PE31-PE38.                                    CI0067
            12            PE31-ACPPA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-FILLER PICTURE  X(36).                   CI0067
            11            PE31-PE6P.                                    CI0067
            12            PE31-PCIRBA PICTURE  S99V999.                 CI0067
            12            PE31-PCIRBB PICTURE  S99V999.                 CI0067
            12            PE31-DCLWP  PICTURE  X(8).                    CI0067
            12            PE31-DGLWP  PICTURE  X(8).                    CI0067
            12            PE31-DCLNP  PICTURE  X(8).                    CI0067
            12            PE31-DGLNP  PICTURE  X(8).                    CI0067
            12            PE31-FILLER PICTURE  X(11).                   CI0067
            12            PE31-ACCHV  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ASCHV  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-DN1WP  PICTURE  X(8).                    CI0067
            12            PE31-DN1NP  PICTURE  X(8).                    CI0067
            12            PE31-DN2WP  PICTURE  X(8).                    CI0067
            12            PE31-DN2NP  PICTURE  X(8).                    CI0067
            12            PE31-CNLG1  PICTURE  X.                       CI0067
            12            PE31-CNLG2  PICTURE  X.                       CI0067
            12            PE31-CDMES  PICTURE  X(2)                     CI0067
                          OCCURS       015     TIMES.                   CI0067
            12            PE31-FILLER PICTURE  X(17).                   CI0067
            12            PE31-GDATA                                    CI0067
                          OCCURS       130     TIMES.                   CI0067
            13            PE31-ATPRP2 PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ACVWP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-AGVWP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ACVNP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-AGVNP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ACSWP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-AGSWP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-ACSNP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-AGSNP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-PE6X                                     CI0067
                          REDEFINES            PE31-PE6P.               CI0067
            12            PE31-CLFOI  PICTURE  X.                       CI0067
            12            PE31-AMECP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AGRPD  PICTURE  S9(9)V99                 CI0067
                          OCCURS       007     TIMES                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-DLMCH  PICTURE  9(8).                    CI0067
            12            PE31-APVTC  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ASMIP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ASMIPL PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AWDRTT PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CLFOI1 PICTURE  X.                       CI0067
            12            PE31-CLFOI2 PICTURE  X.                       CI0067
            12            PE31-APYMT1 PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-APYMT2 PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ASMIP1 PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ASMIP2 PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-IEXIC  PICTURE  X(01).                   CI0067
            12            PE31-DTODAF PICTURE  9(8).                    CI0067
            12            PE31-CLFOI3 PICTURE  X.                       CI0067
            12            PE31-CLFOI4 PICTURE  X.                       CI0067
            12            PE31-DEFFT  PICTURE  9(8).                    CI0067
            12            PE31-DLP20  PICTURE  9(8).                    CI0067
            12            PE31-DLP201 PICTURE  9(8).                    CI0067
            12            PE31-DLP202 PICTURE  9(8).                    CI0067
            12            PE31-AMRAC  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AMOPE  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CORAT  PICTURE  X(6).                    CI0067
            12            PE31-FILLER PICTURE  X(121).                  CI0067
            12            PE31-FILLER PICTURE  X(6873).                 CI0067
            11            PE31-PE7D.                                    CI0067
            12            PE31-DMAVS  PICTURE  9(8).                    CI0067
            12            PE31-DMAVE  PICTURE  9(8).                    CI0067
            12            PE31-CEBPR  PICTURE  X.                       CI0067
            12            PE31-DEBPS  PICTURE  9(8).                    CI0067
            12            PE31-DEBPE  PICTURE  9(8).                    CI0067
            12            PE31-ITOWN  PICTURE  X.                       CI0067
            12            PE31-AMAVR  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AEBPR  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AMDBG  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-IMAV5  PICTURE  X(1).                    CI0067
            12            PE31-DROPS  PICTURE  9(8).                    CI0067
            12            PE31-DROPE  PICTURE  9(8).                    CI0067
            12            PE31-APPCR  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AROPR  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PRCHGN PICTURE  999V999                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-FILLER PICTURE  X(15).                   CI0067
            11            PE31-PE7L.                                    CI0067
            12            PE31-APGBA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-APRBA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-APGBP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-APRBP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-DWSDT  PICTURE  9(8).                    CI0067
            12            PE31-DWAIT  PICTURE  9(8).                    CI0067
            12            PE31-CPNCG  PICTURE  X.                       CI0067
            12            PE31-DUVDT  PICTURE  9(8).                    CI0067
            11            PE31-PE66.                                    CI0067
            12            PE31-AENTI  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-DBFDT  PICTURE  9(8).                    CI0067
            12            PE31-DATFR  PICTURE  9(8).                    CI0067
            12            PE31-DATLT  PICTURE  9(8).                    CI0067
            12            PE31-ISTWH2 PICTURE  X(1).                    CI0067
            12            PE31-DRFID  PICTURE  9(8).                    CI0067
            12            PE31-DOPOD  PICTURE  9(8).                    CI0067
            12            PE31-COPIN  PICTURE  X(1).                    CI0067
            12            PE31-DPNPS  PICTURE  9(8).                    CI0067
            12            PE31-DRITR  PICTURE  9(8).                    CI0067
            12            PE31-CRIBT  PICTURE  X(1).                    CI0067
            12            PE31-INMVF  PICTURE  X(1).                    CI0067
            12            PE31-FILLER PICTURE  X(134).                  CI0067
            11            PE31-PE8L.                                    CI0067
            12            PE31-APALP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ARALP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-IRDPH  PICTURE  X.                       CI0067
            12            PE31-DEWCN  PICTURE  9(8).                    CI0067
            12            PE31-FILLER PICTURE  X(15).                   CI0067
            11            PE31-AAFEA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-ATPWO  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-SR02.                                    CI0067
            12            PE31-PTBDP  PICTURE  9(2)V99                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AMWAB  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PTALP  PICTURE  9(2)V99                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PTGBP  PICTURE  9(2)V99                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-IATAG  PICTURE  X.                       CI0067
            12            PE31-AGBAP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ARBAP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AGBPP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AALPP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AGBAS  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ARBAS  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ASGBP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AALPS  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-IAPGP  PICTURE  X.                       CI0067
            12            PE31-AALPR  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AGBPR  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ARLPR  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ARBPR  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-IASWA  PICTURE  X.                       CI0067
            12            PE31-DRFIL  PICTURE  9(8).                    CI0067
            11            PE31-PE6T.                                    CI0067
            12            PE31-ISDIS  PICTURE  X.                       CI0067
            12            PE31-IBPER  PICTURE  X.                       CI0067
            12            PE31-PINFL  PICTURE  X(02).                   CI0067
            12            PE31-CINFT  PICTURE  X.                       CI0067
            12            PE31-ARODE  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ARDBL  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ARPSL  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CLONT  PICTURE  X.                       CI0067
            12            PE31-CLONT1 PICTURE  X.                       CI0067
            12            PE31-CLONT2 PICTURE  X.                       CI0067
            12            PE31-DPRPA  PICTURE  9(8).                    CI0067
            12            PE31-ADBSU  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-FILLER PICTURE  X(2362).                 CI0067
            11            PE31-FILLER PICTURE  X(1).                    CI0067
            10            PE31-PE6Y                                     CI0067
                          REDEFINES            PE31-PE32.               CI0067
            11            PE31-PCTSW  PICTURE  S99V999                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-COLPI  PICTURE  X.                       CI0067
            11            PE31-FILLER PICTURE  X(135).                  CI0067
            10            PE31-PE69                                     CI0067
                          REDEFINES            PE31-PE32.               CI0067
            11            PE31-PE98.                                    CI0067
            12            PE31-NRCLN  PICTURE  S9(4)                    CI0067
                          BINARY.                                       CI0067
            12            PE31-FILLER PICTURE  X(2).                    CI0067
            12            PE31-CVACO1 PICTURE  X(3).                    CI0067
            12            PE31-XSW1   PICTURE  X.                       CI0067
            12            PE31-GVAID  PICTURE  X(15).                   CI0067
            12            PE31-DEFFT  PICTURE  9(8).                    CI0067
            12            PE31-FILLER PICTURE  X(2).                    CI0067
            12            PE31-CVTXN  PICTURE  X(4).                    CI0067
            12            PE31-DEFFU  PICTURE  9(8).                    CI0067
            12            PE31-FILLER PICTURE  X(15).                   CI0067
            12            PE31-CAMCN  PICTURE  X(8).                    CI0067
            12            PE31-FILLER PICTURE  X(6).                    CI0067
            12            PE31-CVOFC  PICTURE  S9(5)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CAORT  PICTURE  X(01).                   CI0067
            12            PE31-CVOFCA PICTURE  S9(5)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CVAST1 PICTURE  X.                       CI0067
            12            PE31-CVAST  PICTURE  X.                       CI0067
            12            PE31-FILLER PICTURE  X(2).                    CI0067
            12            PE31-CTRNTB PICTURE  XX.                      CI0067
            12            PE31-DEFFC  PICTURE  9(8).                    CI0067
            12            PE31-FILLER PICTURE  X(56).                   CI0067
            11            PE31-PE99.                                    CI0067
            12            PE31-FILLER PICTURE  X(19000).                CI0067
            10            PE31-PE7G                                     CI0067
                          REDEFINES            PE31-PE32.               CI0067
            11            PE31-PE79.                                    CI0067
            12            PE31-CVACO3 PICTURE  X(3).                    CI0067
            12            PE31-FILLER PICTURE  X(11).                   CI0067
            12            PE31-FILLER PICTURE  X(16).                   CI0067
            12            PE31-GVAID2 PICTURE  X(11).                   CI0067
            12            PE31-FILLER PICTURE  X(02).                   CI0067
            12            PE31-FILLER PICTURE  9(8).                    CI0067
            12            PE31-FILLER PICTURE  S9(7)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-FILLER PICTURE  X(44).                   CI0067
            12            PE31-FILLER PICTURE  X(8).                    CI0067
            12            PE31-DEFFT  PICTURE  9(8).                    CI0067
            12            PE31-DACTG  PICTURE  9(8).                    CI0067
            12            PE31-CRPAY  PICTURE  X(03).                   CI0067
            12            PE31-CHREV  PICTURE  X.                       CI0067
            12            PE31-CFEDD  PICTURE  X(02).                   CI0067
            12            PE31-ALWTAX PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-GESTNS PICTURE  X(2).                    CI0067
            12            PE31-ADSW   PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ASURR4 PICTURE  S9(07)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CSURR  PICTURE  X(01).                   CI0067
            12            PE31-ARPMT  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ALPMTR PICTURE  S9(07)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AFETX5 PICTURE  S9(07)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ANOTX  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-APAYR  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-FILLER PICTURE  X(200).                  CI0067
            11            PE31-PE84                                     CI0067
                          OCCURS       100     TIMES.                   CI0067
            12            PE31-MSACN  PICTURE  X(3).                    CI0067
            12            PE31-AFUNV3 PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-QFUUT  PICTURE  S9(9)V9(4)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AUPAM  PICTURE  S9(3)V9(7)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ITTFI  PICTURE  X.                       CI0067
            12            PE31-FILLER PICTURE  X(57).                   CI0067
            11            PE31-FILLER PICTURE  X(10778).                CI0067
            10            PE31-PE6D                                     CI0067
                          REDEFINES            PE31-PE32                CI0067
                          OCCURS       070     TIMES.                   CI0067
            11            PE31-CVSET  PICTURE  X(5).                    CI0067
            11            PE31-GVSCK.                                   CI0067
            12            PE31-MCLNM9 PICTURE  X(80).                   CI0067
            12            PE31-FILLER PICTURE  X(7).                    CI0067
            11            PE31-GDATA                                    CI0067
                          REDEFINES            PE31-GVSCK.              CI0067
            12            PE31-FILLER PICTURE  X(50).                   CI0067
            12            PE31-QPOIA2 PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-QPOIA3 PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CSPCL  PICTURE  XX.                      CI0067
            12            PE31-DSCCC  PICTURE  9(8).                    CI0067
            12            PE31-ABEPA                                    CI0067
                          REDEFINES            PE31-DSCCC               CI0067
               PICTURE    S9(9)V99                                      CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-QNORU  PICTURE  9(6)V9(5)                CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PCPUC  PICTURE  S9(3)V9(5)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-DLSTC  PICTURE  9(8).                    CI0067
            12            PE31-CTRSNV PICTURE  X(02).                   CI0067
            12            PE31-CCISR  PICTURE  X(02).                   CI0067
            11            PE31-DEFFT  PICTURE  9(8).                    CI0067
            11            PE31-CVAST  PICTURE  X.                       CI0067
            11            PE31-QPOIA  PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-CLSEX  PICTURE  X.                       CI0067
            11            PE31-QNOUN  PICTURE  9(6)V9(5)                CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-CPRCC  PICTURE  X.                       CI0067
            11            PE31-DPOLI  PICTURE  9(8).                    CI0067
            11            PE31-AGLPA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-CVAPC3 PICTURE  X(6).                    CI0067
            11            PE31-PPRRT2 PICTURE  S9(2)V9(3)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-DRAEN  PICTURE  9(8).                    CI0067
            11            PE31-PE6F                                     CI0067
                          OCCURS       006     TIMES.                   CI0067
            12            PE31-AFLEX  PICTURE  S9(3)V9(2)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-DFEED  PICTURE  9(8).                    CI0067
            11            PE31-APYCT  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-APYMT  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-ARGPA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-AROGP  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-ACYTA  PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-CRTYC  PICTURE  X(1).                    CI0067
            11            PE31-CRICO  PICTURE  X(3).                    CI0067
            11            PE31-CACOV  PICTURE  X(02).                   CI0067
            11            PE31-ACECP  PICTURE  S9(09)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-DMDOB2 PICTURE  X(8).                    CI0067
            11            PE31-DTERMG PICTURE  X(8).                    CI0067
            11            PE31-ASRBE  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-ATCDS                                    CI0067
                          REDEFINES            PE31-ASRBE               CI0067
               PICTURE    S9(9)V99                                      CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-ASRRM  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-AMMOB                                    CI0067
                          REDEFINES            PE31-ASRRM               CI0067
               PICTURE    S9(9)V99                                      CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-PASRM  PICTURE  999.                     CI0067
            11            PE31-ASRHL  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-AMMOI                                    CI0067
                          REDEFINES            PE31-ASRHL               CI0067
               PICTURE    S9(9)V99                                      CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-ALTCB  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-ALTCI  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-AAFAA  PICTURE  S9(3)V9(8)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-FILLER PICTURE  X(79).                   CI0067
            10            PE31-PE6S                                     CI0067
                          REDEFINES            PE31-PE32.               CI0067
            11            PE31-ISRRS  PICTURE  X(4).                    CI0067
            11            PE31-ISRFN  PICTURE  X.                       CI0067
            11            PE31-ISRRP  PICTURE  X.                       CI0067
            11            PE31-ISRGB  PICTURE  X.                       CI0067
            11            PE31-ISRRPN PICTURE  X.                       CI0067
            11            PE31-CSPPW  PICTURE  9(2).                    CI0067
            11            PE31-CSRPN  PICTURE  X(2).                    CI0067
            11            PE31-CSRPNB PICTURE  X(120).                  CI0067
            11            PE31-CSRPNA                                   CI0067
                          REDEFINES            PE31-CSRPNB              CI0067
                          OCCURS       020     TIMES.                   CI0067
            12            PE31-CSRPNR PICTURE  X(2).                    CI0067
            12            PE31-ARIPR  PICTURE  S9(2)V9(5)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-FILLER PICTURE  X(50).                   CI0067
            10            PE31-PE6E                                     CI0067
                          REDEFINES            PE31-PE32.               CI0067
            11            PE31-FILLER PICTURE  X(59).                   CI0067
            11            PE31-AGLPB  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-ASRCD  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-ASRCR  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-FILLER PICTURE  X(108).                  CI0067
            11            PE31-ASRHM  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PE31-PE47                                     CI0067
                          REDEFINES            PE31-PE32.               CI0067
            11            PE31-CALRE  PICTURE  X.                       CI0067
            11            PE31-NFUIA.                                   CI0067
            12            PE31-NFUIO                                    CI0067
                          OCCURS       097     TIMES.                   CI0067
            13            PE31-NFUNR  PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            13            PE31-PDEDU  PICTURE  S9(4)V9                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-FILLER PICTURE  X(28674).                CI0067
            10            PE31-PE6K                                     CI0067
                          REDEFINES            PE31-PE32.               CI0067
            11            PE31-COTTC  PICTURE  X(2).                    CI0067
            11            PE31-DABAL  PICTURE  9(8).                    CI0067
            11            PE31-DAEAL  PICTURE  9(8).                    CI0067
            11            PE31-CDTFR  PICTURE  99.                      CI0067
            11            PE31-QTERS  PICTURE  9(3).                    CI0067
            11            PE31-DATRF  PICTURE  99.                      CI0067
            11            PE31-IOVRF  PICTURE  X.                       CI0067
            11            PE31-AOVFE  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            PE31-CSEMR  PICTURE  X.                       CI0067
            11            PE31-ILTOV  PICTURE  X(1).                    CI0067
            11            PE31-DATDT  PICTURE  9(8).                    CI0067
            11            PE31-DATDN  PICTURE  9(8).                    CI0067
            11            PE31-CMEMO  PICTURE  X(2).                    CI0067
            11            PE31-CAREA  PICTURE  X(8).                    CI0067
            11            PE31-CBADT  PICTURE  X(04).                   CI0067
            11            PE31-FILLER PICTURE  X(296).                  CI0067
            11            PE31-PE26                                     CI0067
                          OCCURS       097     TIMES.                   CI0067
            12            PE31-ITFTI  PICTURE  X.                       CI0067
            12            PE31-NFUNX  PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CAMTX  PICTURE  X.                       CI0067
            12            PE31-ACOTA  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-ACOTA1                                   CI0067
                          REDEFINES            PE31-ACOTA               CI0067
               PICTURE    S9(6)V9(5)                                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PALLO  PICTURE  S999V9                   CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PALLO1                                   CI0067
                          REDEFINES            PE31-PALLO               CI0067
               PICTURE    S9V999                                        CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-FILLER PICTURE  X(18).                   CI0067
            10            PE31-PE6M                                     CI0067
                          REDEFINES            PE31-PE32.               CI0067
            11            PE31-PE6J                                     CI0067
                          OCCURS       024     TIMES.                   CI0067
            12            PE31-CASTA6 PICTURE  X.                       CI0067
            12            PE31-CDPFC  PICTURE  S9(5)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CDPTC  PICTURE  X(4).                    CI0067
            12            PE31-CMEMO  PICTURE  X(2).                    CI0067
            12            PE31-IRENR  PICTURE  X(01).                   CI0067
            12            PE31-QDDAM  PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-DDEPO  PICTURE  9(8).                    CI0067
            12            PE31-DTIRNN PICTURE  9(8).                    CI0067
            12            PE31-DTIRNL PICTURE  9(8).                    CI0067
            12            PE31-DTIRN  PICTURE  9(8).                    CI0067
            12            PE31-DTERT  PICTURE  9(8).                    CI0067
            12            PE31-ADVAL  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PINRA  PICTURE  S9(3)V9(6)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PCTIU  PICTURE  S9V9(4)                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PCTCP  PICTURE  S9V9(4)                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PCTCPR                                   CI0067
                          REDEFINES            PE31-PCTCP               CI0067
               PICTURE    S9(3)V99                                      CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AGAPJ  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PPARN  PICTURE  S9V9(4)                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-CGNTC  PICTURE  X(4).                    CI0067
            12            PE31-CGNFC  PICTURE  S9(5)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-DINDL  PICTURE  9(8).                    CI0067
            12            PE31-IRENRV PICTURE  X(01).                   CI0067
            12            PE31-AMTRLC PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AMTRL  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AMTRLW PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AMTRLO PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AMTAV  PICTURE  S9(9)V9(2)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AMTAVM PICTURE  S9(9)V9(2)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AMTAVF PICTURE  S9(9)V9(2)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-PENRA  PICTURE  S9(3)V9(6)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AICRI  PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-AROFA  PICTURE  S9V9(4)                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            12            PE31-FILLER PICTURE  X(70).                   CI0067
            11            PE31-CFNDC  PICTURE  S9(3)                    CI0067
                          OCCURS       024     TIMES                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PE31-PE7S                                     CI0067
                          REDEFINES            PE31-PE32.               CI0067
            11            PE31-CVSTC1 PICTURE  X(4).                    CI0067
            11            PE31-GXRTN1                                   CI0067
                          OCCURS       250     TIMES.                   CI0067
            12            PE31-NFUNDL PICTURE  999.                     CI0067
            12            PE31-CFIDC4 PICTURE  X(3).                    CI0067
            12            PE31-CTFIN  PICTURE  X.                       CI0067
            12            PE31-NFDNM  PICTURE  X(20).                   CI0067
            12            PE31-FILLER PICTURE  X(40).                   CI0067
            10            PE31-INDSP  PICTURE  X.                       CI0067
       01          VANTAGE-ACCESS-MODULE  PIC X(8)                      AAZ000
                           VALUE         'LPAZ000 '.                    AAZ000

      *ASSIGN POLICY NUMBER TO VANTAGE *
       01  7-PE30-GVAID.
           05  7-PE30-FILLER                PIC X(4)   VALUE SPACES.
           05  7-PE30-CTIDND                PIC X(11).

      ******************************************************************AM0400
      ** WORKING STORAGE FOR CI0400                                    *AM0400
      ******************************************************************AM0400
      *                                                                 AM0400
      *!WF DSP=K9 DSL=K9 SEL=10 FOR=I DES=2 LEV=1                       AM0400
       01                 K910.                                         CI0067
            10            K910-DIRAYR PICTURE  9(4)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            K910-CQACT  PICTURE  999                      CI0067
                          VALUE                ZERO.                    CI0067
            10            K910-CIRAT  PICTURE  999                      CI0067
                          VALUE                ZERO.                    CI0067
            10            K910-CIRAS  PICTURE  999                      CI0067
                          VALUE                ZERO.                    CI0067
            10            K910-CIRAP  PICTURE  XX                       CI0067
                          VALUE                SPACE.                   CI0067
            10            K910-CLDOB  PICTURE  9(8)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            K910-ACLIM  PICTURE  S9(7)V9(2)               CI0067
                          VALUE                ZERO.                    CI0067
            10            K910-GERTC  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            K910-CSQLRC PICTURE  S9(3)                    CI0067
                          VALUE                ZERO.                    CI0067
            10            K910-ACASH  PICTURE  S9(9)V99                 CI0067
                          VALUE                ZERO                     CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            K910-ICBLM  PICTURE  X(1)                     CI0067
                          VALUE                SPACE.                   CI0067
      *                                                                 AM0400
      *                                                                 AM0400
      *BOOLEAN SSA TO PERFORM GET NEXT ON CT07/CT09
       01               7-CTA09-1-SSA.                                  AAADBL
         05             FILLER          PIC X(08)   VALUE 'CT09'.       AAADBL
         05             FILLER          PIC X(01)   VALUE '*'.          AAADBL
         05             7-CTA09-1-CCOD PIC X(05)  VALUE '-----'.        AAADBL
         05             FILLER          PIC X(01)   VALUE '('.          AAADBL
         05             FILLER          PIC X(08)   VALUE 'CT09K'.
         05             FILLER          PIC X(02)   VALUE ' ='.
         05             FILLER          PIC 9(03)   VALUE 004.
         05             FILLER          PIC X(01)   VALUE '&'.
         05             FILLER          PIC X(08)   VALUE 'GERED'.
         05             FILLER          PIC X(02)   VALUE ' ='.
         05             FILLER          PIC 9(08)   VALUE ZEROS.
         05             FILLER          PIC X(01)   VALUE ')'.          AAADBL
       01                 CL00.                                         CI0067
            02            CL01.                                         CI0067
            10            CL01-CL01K.                                   CI0067
            11            CL01-C199.                                    CI0067
            12            CL01-CLID.                                    CI0067
            13            CL01-CLIDO  PICTURE  9(3).                    CI0067
            13            CL01-CLIDN.                                   CI0067
            14            CL01-CLIDNP PICTURE  X(12).                   CI0067
            14            CL01-CLIDND PICTURE  9(8).                    CI0067
            10            CL01-GECKD  PICTURE  9.                       CI0067
            10            CL01-GEMDA  PICTURE  9(8).                    CI0067
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0067
                          BINARY.                                       CI0067
            10            CL01-GECUC  PICTURE  99.                      CI0067
            10            CL01-CLDOR  PICTURE  9(8).                    CI0067
            10            CL01-CLLNG  PICTURE  XX.                      CI0067
            10            CL01-GESLC  PICTURE  99.                      CI0067
            10            CL01-CLTYP  PICTURE  X.                       CI0067
            10            CL01-CLCLS  PICTURE  9(3).                    CI0067
            10            CL01-CLTWRC PICTURE  99.                      CI0067
            10            CL01-CLPVC  PICTURE  99.                      CI0067
            10            CL01-CLIND  PICTURE  9(3).                    CI0067
            10            CL01-CLTRC  PICTURE  99.                      CI0067
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            CL01-AYSIDA PICTURE  9(3).                    CI0067
            10            CL01-AYSID  PICTURE  9(5).                    CI0067
            10            CL01-CLSTR  PICTURE  9(2).                    CI0067
            10            CL01-CLC11  PICTURE  X.                       CI0067
            10            CL01-CLTIN  PICTURE  9(12).                   CI0067
            10            CL01-CLTND  PICTURE  9(8).                    CI0067
            10            CL01-CLTINC PICTURE  9.                       CI0067
            10            CL01-CCDWA  PICTURE  9.                       CI0067
            10            CL01-CICES  PICTURE  X.                       CI0067
            10            CL01-CLTRA  PICTURE  9(2).                    CI0067
            10            CL01-DIRSY  PICTURE  9(4)                     CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            CL01-CFEDS  PICTURE  X.                       CI0067
            10            CL01-FILLER PICTURE  X(06).                   CI0067
            02            CL03.                                         CI0067
            10            CL03-GEDLA  PICTURE  9(8).                    CI0067
            10            CL03-DDREP  PICTURE  9(8).                    CI0067
            10            CL03-DPRFR  PICTURE  9(8).                    CI0067
            10            CL03-IACCI  PICTURE  X.                       CI0067
            10            CL03-CLDOB  PICTURE  9(8).                    CI0067
            10            CL03-CLDOD  PICTURE  9(8).                    CI0067
            10            CL03-CLDTH  PICTURE  X.                       CI0067
            10            CL03-CCINI  PICTURE  X.                       CI0067
            10            CL03-FILLER PICTURE  X(1).                    CI0067
            10            CL03-CLAIN  PICTURE  S9(11)                   CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            CL03-CCAOD  PICTURE  999.                     CI0067
            10            CL03-CLMAR  PICTURE  X.                       CI0067
            10            CL03-C198.                                    CI0067
            11            CL03-CLNAM.                                   CI0067
            12            CL03-CLNAMH PICTURE  X(6).                    CI0067
            12            CL03-CLNAMF PICTURE  X(20).                   CI0067
            12            CL03-CLNAMM.                                  CI0067
            13            CL03-CLNAMI PICTURE  X.                       CI0067
            13            CL03-CLNAMR PICTURE  X(14).                   CI0067
            12            CL03-CLNAML PICTURE  X(25).                   CI0067
            12            CL03-CLNAMS PICTURE  X(4).                    CI0067
            10            CL03-FILLER PICTURE  X(10).                   CI0067
            10            CL03-MPRFS  PICTURE  X(4).                    CI0067
            10            CL03-CLOCC  PICTURE  9(3).                    CI0067
            10            CL03-CLRET  PICTURE  X.                       CI0067
            10            CL03-IOCOB  PICTURE  X.                       CI0067
            10            CL03-CLSEX  PICTURE  X.                       CI0067
            10            CL03-CLWIL  PICTURE  X.                       CI0067
            10            CL03-GECFC  PICTURE  99.                      CI0067
            10            CL03-GECFY  PICTURE  9(4).                    CI0067
            10            CL03-ICUSC  PICTURE  X.                       CI0067
            10            CL03-MCTYC  PICTURE  X(20).                   CI0067
            10            CL03-CLWIP  PICTURE  X.                       CI0067
            10            CL03-CLCTXF PICTURE  99.                      CI0067
            10            CL03-CLCUS  PICTURE  99.                      CI0067
            10            CL03-NPDLU  PICTURE  9(5).                    CI0067
            10            CL03-CLEMI  PICTURE  X.                       CI0067
            10            CL03-GEPHNH PICTURE  X(14).                   CI0067
            10            CL03-GEPHNB PICTURE  X(14).                   CI0067
            10            CL03-GEPHNX PICTURE  9(4).                    CI0067
            10            CL03-GEPHNA PICTURE  X(14).                   CI0067
            10            CL03-FILLER PICTURE  X(3).                    CI0067
            10            CL03-IAPRT  PICTURE  X.                       CI0067
            10            CL03-CEMSC  PICTURE  X.                       CI0067
            10            CL03-CSEPS  PICTURE  X.                       CI0067
            10            CL03-CRACE  PICTURE  X.                       CI0067
            10            CL03-CNIRA  PICTURE  X.                       CI0067
            10            CL03-FILLER PICTURE  X(11).                   CI0067
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0021           PIC X(8) VALUE 'CI0021P '.                  AM0021
       01  CI0022           PIC X(8) VALUE 'CI0022P '.                  AM0022
       01  CI0135           PIC X(8) VALUE 'CI0135P '.                  AM0135
       01  CI0400           PIC X(8) VALUE 'CI0400P '.                  AM0400
       01                 CT01.                                         CI0067
            10            CT01-CT01K.                                   CI0067
            11            CT01-C299.                                    CI0067
            12            CT01-CTID.                                    CI0067
            13            CT01-CTIDA  PICTURE  9(3).                    CI0067
            13            CT01-CTIDN.                                   CI0067
            14            CT01-CTIDNP PICTURE  X(13).                   CI0067
            14            CT01-CTIDND PICTURE  9(11).                   CI0067
            10            CT01-GECKD  PICTURE  9.                       CI0067
            10            CT01-GEMDA  PICTURE  9(8).                    CI0067
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0067
                          BINARY.                                       CI0067
            10            CT01-GECUC  PICTURE  99.                      CI0067
            10            CT01-CTAUL  PICTURE  9(3).                    CI0067
            10            CT01-DIRAC  PICTURE  9(4).                    CI0067
            10            CT01-CTCCI  PICTURE  X.                       CI0067
            10            CT01-CTCUS  PICTURE  999.                     CI0067
            10            CT01-CTEFD  PICTURE  9(8).                    CI0067
            10            CT01-CTIAD  PICTURE  9(8).                    CI0067
            10            CT01-CLCUS  PICTURE  99.                      CI0067
            10            CT01-CAMMB  PICTURE  X(3).                    CI0067
            10            CT01-CKPMM  PICTURE  X.                       CI0067
            10            CT01-CTLAD  PICTURE  9(8).                    CI0067
            10            CT01-IPERS  PICTURE  X.                       CI0067
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            CT01-CTLAT  PICTURE  9(8).                    CI0067
            10            CT01-CTLATC PICTURE  9(6).                    CI0067
            10            CT01-IMEGA  PICTURE  X.                       CI0067
            10            CT01-DIRAB  PICTURE  9(8).                    CI0067
            10            CT01-COLRQ  PICTURE  X.                       CI0067
            10            CT01-ZDA04  PICTURE  X(4).                    CI0067
            10            CT01-CTLPD  PICTURE  9(8).                    CI0067
            10            CT01-CIRASP PICTURE  9.                       CI0067
            10            CT01-CIRATP PICTURE  99.                      CI0067
            10            CT01-DRTHC  PICTURE  9(8).                    CI0067
            10            CT01-CPPTC  PICTURE  X.                       CI0067
            10            CT01-ZDA06  PICTURE  X(6).                    CI0067
            10            CT01-CTACD  PICTURE  9(8).                    CI0067
            10            CT01-CTNLI  PICTURE  X.                       CI0067
            10            CT01-CTRHO  PICTURE  9(8).                    CI0067
            10            CT01-CTSGD  PICTURE  9(8).                    CI0067
            10            CT01-CPATP  PICTURE  X(1).                    CI0067
            10            CT01-IRSTA  PICTURE  X.                       CI0067
            10            CT01-CTSTA  PICTURE  99.                      CI0067
            10            CT01-CTSSC  PICTURE  99.                      CI0067
            10            CT01-PRLIN  PICTURE  9(3).                    CI0067
            10            CT01-PRCOD  PICTURE  9(5).                    CI0067
            10            CT01-PRSCD  PICTURE  X(9).                    CI0067
            10            CT01-CTLNI  PICTURE  X.                       CI0067
            10            CT01-AYSIDA PICTURE  9(3).                    CI0067
            10            CT01-AYSID  PICTURE  9(5).                    CI0067
            10            CT01-CTBMC  PICTURE  99.                      CI0067
            10            CT01-CINAR  PICTURE  99.                      CI0067
            10            CT01-CPHTR  PICTURE  X.                       CI0067
            10            CT01-CDSTR  PICTURE  XX.                      CI0067
            10            CT01-CQACT  PICTURE  999.                     CI0067
            10            CT01-CIRAS  PICTURE  999.                     CI0067
            10            CT01-CIRAT  PICTURE  999.                     CI0067
            10            CT01-CLRAY  PICTURE  9(5).                    CI0067
            10            CT01-CATTP  PICTURE  X.                       CI0067
       01                 CT07.                                         CI0067
            10            CT07-CT07K.                                   CI0067
            11            CT07-C199.                                    CI0067
            12            CT07-CLID.                                    CI0067
            13            CT07-CLIDO  PICTURE  9(3).                    CI0067
            13            CT07-CLIDN.                                   CI0067
            14            CT07-CLIDNP PICTURE  X(12).                   CI0067
            14            CT07-CLIDND PICTURE  9(8).                    CI0067
       01                 CT09.                                         CI0067
            10            CT09-A100.                                    CI0067
            11            CT09-GELL   PICTURE  9(4)                     CI0067
                          BINARY.                                       CI0067
            11            CT09-CT09K.                                   CI0067
            12            CT09-CLCTRC PICTURE  9(3).                    CI0067
            11            CT09-GERSD  PICTURE  9(8).                    CI0067
            11            CT09-GERED  PICTURE  9(8).                    CI0067
            10            CT09-A199.                                    CI0067
            11            CT09-FILLER PICTURE  X(20).                   CI0067
            10            CT09-A101                                     CI0067
                          REDEFINES            CT09-A199.               CI0067
            11            CT09-GECSQ  PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            CT09-CTAXR  PICTURE  X.                       CI0067
            11            CT09-GETAI  PICTURE  X.                       CI0067
            11            CT09-CTLACD PICTURE  9(8).                    CI0067
            11            CT09-GEPCS  PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            CT09-A102                                     CI0067
                          REDEFINES            CT09-A199.               CI0067
            11            CT09-CLPID  PICTURE  9(9).                    CI0067

      ******************************************************************
      **  DATE WORK AREA USED WITH MACRO AADA56                        *
      ******************************************************************
      *!WF DSP=DD DSL=DD SEL=01 FOR=I LEV=1 PLT=DD
       01                 DD00.                                         CI0067
          05              DD00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00093).                  CI0067
       01                 DD01  REDEFINES      DD00.                    CI0067
            10            DD01-XDAT8.                                   CI0067
            11            DD01-XDATC  PICTURE  XX.                      CI0067
            11            DD01-XDATY  PICTURE  XX.                      CI0067
            11            DD01-XDATM  PICTURE  XX.                      CI0067
            11            DD01-XDATD  PICTURE  XX.                      CI0067
            10            DD01-XDAT8D                                   CI0067
                          REDEFINES            DD01-XDAT8               CI0067
               PICTURE    9(8).                                         CI0067
            10            DD01-XDAT81.                                  CI0067
            11            DD01-XDATM1 PICTURE  XX.                      CI0067
            11            DD01-XDATD1 PICTURE  XX.                      CI0067
            11            DD01-XDATC1 PICTURE  XX.                      CI0067
            11            DD01-XDATY1 PICTURE  XX.                      CI0067
            10            DD01-XDAT80                                   CI0067
                          REDEFINES            DD01-XDAT81              CI0067
               PICTURE    9(8).                                         CI0067
            10            DD01-XDAT62.                                  CI0067
            11            DD01-XDATM2 PICTURE  XX.                      CI0067
            11            DD01-XDATD2 PICTURE  XX.                      CI0067
            11            DD01-XDATY2 PICTURE  XX.                      CI0067
            10            DD01-XDAT69                                   CI0067
                          REDEFINES            DD01-XDAT62              CI0067
               PICTURE    9(6).                                         CI0067
            10            DD01-XDATCU.                                  CI0067
            11            DD01-XDATC9 PICTURE  99.                      CI0067
            11            DD01-XDAYMD.                                  CI0067
            12            DD01-XDATY9 PICTURE  99.                      CI0067
            12            DD01-XDAMD.                                   CI0067
            13            DD01-XDATM9 PICTURE  99.                      CI0067
            13            DD01-XDATD9 PICTURE  99.                      CI0067
            10            DD01-XDAT89 PICTURE  9(8).                    CI0067
            10            DD01-XDAJC  PICTURE  9(7).                    CI0067
            10            DD01-XDAJC1.                                  CI0067
            11            DD01-XDAJC9 PICTURE  99.                      CI0067
            11            DD01-XDAJY  PICTURE  99.                      CI0067
            11            DD01-XDAJN  PICTURE  999.                     CI0067
            10            DD01-XDAB   PICTURE  9(5).                    CI0067
            10            DD01-DD05.                                    CI0067
            11            DD01-XDACT  PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            DD01-XDACV  PICTURE  S9                       CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            DD01-XDAGP  PICTURE  S9(9)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            DD01-XDAJP  PICTURE  S9(7)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            DD01-XDACV1 PICTURE  S9                       CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            DD01-XDAGP1 PICTURE  S9(9)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            DD01-XDAJP1 PICTURE  S9(7)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            DD01-XW03.                                    CI0067
            11            DD01-XDATG.                                   CI0067
            12            DD01-XDAT1.                                   CI0067
            13            DD01-XDAT19 PICTURE  99.                      CI0067
            12            DD01-XDAT2.                                   CI0067
            13            DD01-XDAT29 PICTURE  99.                      CI0067
            12            DD01-XDAT3.                                   CI0067
            13            DD01-XDAT39 PICTURE  99.                      CI0067
            12            DD01-XDAT4.                                   CI0067
            13            DD01-XDAT49 PICTURE  99.                      CI0067
            11            DD01-XLEAPY PICTURE  99.                      CI0067
            11            DD01-DTGCY  PICTURE  9(4).                    CI0067
            11            DD01-FILLER                                   CI0067
                          REDEFINES            DD01-DTGCY.              CI0067
            12            DD01-DTGCC  PICTURE  9(2).                    CI0067
            12            DD01-DTGYY  PICTURE  9(2).                    CI0067

      *RETURN CODE FROM DATE VALIDATION ROUTINE
       01  DEL-ER                 PIC 9(1).

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
      ******************************************************            AADA81
      ****      WORK AREAS FOR COMMON DATE UTILITY       ***            AADA81
      ******************************************************            AADA81
      **                                                                AADA81
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA81
      **                                                                AADA81
      **   SEGMENT DD30 - FUNCTION LAYOUT                               AADA81
      **                                                                AADA81
      *!WF DSP=DF DSL=DD SEL=30 FOR=I DES=2 LEV=1                       AADA81
       01                 DF30.                                         CI0067
            10            DF30-CDTFN  PICTURE  9(4)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            DF30-CDTSF  PICTURE  9(4)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            DF30-CDTSC  PICTURE  9(4)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            DF30-FILLER PICTURE  X(40)                    CI0067
                          VALUE                SPACE.                   CI0067
       01                 DF33.                                         CI0067
            10            DF33-DTGRG.                                   CI0067
            11            DF33-DTGCY.                                   CI0067
            12            DF33-DTGCC  PICTURE  9(2)                     CI0067
                          VALUE                ZERO.                    CI0067
            12            DF33-DTGYY  PICTURE  9(2)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            DF33-DTGMM  PICTURE  9(2)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            DF33-DTGDD  PICTURE  9(2)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            DF33-DTJULC.                                  CI0067
            11            DF33-DTJCY.                                   CI0067
            12            DF33-DTJCC  PICTURE  9(2)                     CI0067
                          VALUE                ZERO.                    CI0067
            12            DF33-DTJYY  PICTURE  9(2)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            DF33-DTJDDC PICTURE  S9(3)                    CI0067
                          VALUE                ZERO.                    CI0067
            11            DF33-DTJDD                                    CI0067
                          REDEFINES            DF33-DTJDDC              CI0067
               PICTURE    9(3).                                         CI0067
            10            DF33-DTJUL                                    CI0067
                          REDEFINES            DF33-DTJULC              CI0067
               PICTURE    9(7).                                         CI0067
            10            DF33-DTDYR  PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            DF33-DTDMO  PICTURE  9(2)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            DF33-FILLER PICTURE  X(18)                    CI0067
                          VALUE                SPACE.                   CI0067
      **                                                                AADA81
      **   SEGMENT DD33 - CONVERT DATE LAYOUT                           AADA81
      **                                                                AADA81
      *!WF DSP=DF DSL=DD SEL=33 FOR=I DES=2 LEV=1                       AADA81
      **                                                                AADA81
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0067
            10            XW05-XW06.                                    CI0067
            11            XW05-XDBPCB.                                  CI0067
            12            XW05-XDBDNM PICTURE  X(08)                    CI0067
                          VALUE                SPACE.                   CI0067
            12            XW05-XSEGLV PICTURE  X(02)                    CI0067
                          VALUE                SPACE.                   CI0067
            12            XW05-XRC    PICTURE  X(02)                    CI0067
                          VALUE                SPACE.                   CI0067
            12            XW05-XPROPT PICTURE  X(04)                    CI0067
                          VALUE                SPACE.                   CI0067
            12            XW05-FILLER PICTURE  S9(5)                    CI0067
                          VALUE                ZERO                     CI0067
                          BINARY.                                       CI0067
            12            XW05-XSEGNM PICTURE  X(08)                    CI0067
                          VALUE                SPACE.                   CI0067
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0067
                          VALUE                ZERO                     CI0067
                          BINARY.                                       CI0067
            12            XW05-XSEGNB PICTURE  9(05)                    CI0067
                          VALUE                ZERO                     CI0067
                          BINARY.                                       CI0067
            12            XW05-XCOKEY PICTURE  X(70)                    CI0067
                          VALUE                SPACE.                   CI0067
            10            XW05-XW07.                                    CI0067
            11            XW05-XIOPCB.                                  CI0067
            12            XW05-XTERMI PICTURE  X(08)                    CI0067
                          VALUE                SPACE.                   CI0067
            12            XW05-FILLER PICTURE  XX                       CI0067
                          VALUE                SPACE.                   CI0067
            12            XW05-XRC1   PICTURE  X(02)                    CI0067
                          VALUE                SPACE.                   CI0067
            12            XW05-FILLER PICTURE  X(12)                    CI0067
                          VALUE                SPACE.                   CI0067
            12            XW05-XMODNM PICTURE  X(8)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0067
                          VALUE                ZERO.                    CI0067
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0067
                          VALUE                ZERO.                    CI0067
            10            XW05-XGU    PICTURE  X(4)                     CI0067
                          VALUE                'GU  '.                  CI0067
            10            XW05-XGHU   PICTURE  X(4)                     CI0067
                          VALUE                'GHU '.                  CI0067
            10            XW05-XGN    PICTURE  X(4)                     CI0067
                          VALUE                'GN  '.                  CI0067
            10            XW05-XGHN   PICTURE  X(4)                     CI0067
                          VALUE                'GHN '.                  CI0067
            10            XW05-XGNP   PICTURE  X(4)                     CI0067
                          VALUE                'GNP '.                  CI0067
            10            XW05-XGHNP  PICTURE  X(4)                     CI0067
                          VALUE                'GHNP'.                  CI0067
            10            XW05-XREPL  PICTURE  XXXX                     CI0067
                          VALUE                'REPL'.                  CI0067
            10            XW05-XISRT  PICTURE  X(4)                     CI0067
                          VALUE                'ISRT'.                  CI0067
            10            XW05-XDLET  PICTURE  X(4)                     CI0067
                          VALUE                'DLET'.                  CI0067
            10            XW05-XOPEN  PICTURE  X(4)                     CI0067
                          VALUE                'OPEN'.                  CI0067
            10            XW05-XCLSE  PICTURE  X(4)                     CI0067
                          VALUE                'CLSE'.                  CI0067
            10            XW05-XCHKP  PICTURE  X(4)                     CI0067
                          VALUE                'CHKP'.                  CI0067
            10            XW05-XXRST  PICTURE  X(4)                     CI0067
                          VALUE                'XRST'.                  CI0067
            10            XW05-XTERM  PICTURE  X(4)                     CI0067
                          VALUE                'TERM'.                  CI0067
            10            XW05-XNFPAC PICTURE  X(13)                    CI0067
                          VALUE                SPACE.                   CI0067
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0067
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0067
      *
      ******************************************************************
      ** SEGMENT THAT CONTAINS SAVED CT01 SEGMENT (FOR FROM ACCOUNT)   *
      ******************************************************************
      *!WF DSP=FR DSL=CT SEL=01 FOR=I DES=1 LEV=1 PLT=FR
       01                 FR01.                                         CI0067
            10            FR01-CT01K.                                   CI0067
            11            FR01-C299.                                    CI0067
            12            FR01-CTID.                                    CI0067
            13            FR01-CTIDA  PICTURE  9(3).                    CI0067
            13            FR01-CTIDN.                                   CI0067
            14            FR01-CTIDNP PICTURE  X(13).                   CI0067
            14            FR01-CTIDND PICTURE  9(11).                   CI0067
            10            FR01-GECKD  PICTURE  9.                       CI0067
            10            FR01-GEMDA  PICTURE  9(8).                    CI0067
            10            FR01-NSEQ4B PICTURE  9(8)                     CI0067
                          BINARY.                                       CI0067
            10            FR01-GECUC  PICTURE  99.                      CI0067
            10            FR01-CTAUL  PICTURE  9(3).                    CI0067
            10            FR01-DIRAC  PICTURE  9(4).                    CI0067
            10            FR01-CTCCI  PICTURE  X.                       CI0067
            10            FR01-CTCUS  PICTURE  999.                     CI0067
            10            FR01-CTEFD  PICTURE  9(8).                    CI0067
            10            FR01-CTIAD  PICTURE  9(8).                    CI0067
            10            FR01-CLCUS  PICTURE  99.                      CI0067
            10            FR01-CAMMB  PICTURE  X(3).                    CI0067
            10            FR01-CKPMM  PICTURE  X.                       CI0067
            10            FR01-CTLAD  PICTURE  9(8).                    CI0067
            10            FR01-IPERS  PICTURE  X.                       CI0067
            10            FR01-AUNCB  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            FR01-CTLAT  PICTURE  9(8).                    CI0067
            10            FR01-CTLATC PICTURE  9(6).                    CI0067
            10            FR01-IMEGA  PICTURE  X.                       CI0067
            10            FR01-DIRAB  PICTURE  9(8).                    CI0067
            10            FR01-COLRQ  PICTURE  X.                       CI0067
            10            FR01-ZDA04  PICTURE  X(4).                    CI0067
            10            FR01-CTLPD  PICTURE  9(8).                    CI0067
            10            FR01-CIRASP PICTURE  9.                       CI0067
            10            FR01-CIRATP PICTURE  99.                      CI0067
            10            FR01-DRTHC  PICTURE  9(8).                    CI0067
            10            FR01-CPPTC  PICTURE  X.                       CI0067
            10            FR01-ZDA06  PICTURE  X(6).                    CI0067
            10            FR01-CTACD  PICTURE  9(8).                    CI0067
            10            FR01-CTNLI  PICTURE  X.                       CI0067
            10            FR01-CTRHO  PICTURE  9(8).                    CI0067
            10            FR01-CTSGD  PICTURE  9(8).                    CI0067
            10            FR01-CPATP  PICTURE  X(1).                    CI0067
            10            FR01-IRSTA  PICTURE  X.                       CI0067
            10            FR01-CTSTA  PICTURE  99.                      CI0067
            10            FR01-CTSSC  PICTURE  99.                      CI0067
            10            FR01-PRLIN  PICTURE  9(3).                    CI0067
            10            FR01-PRCOD  PICTURE  9(5).                    CI0067
            10            FR01-PRSCD  PICTURE  X(9).                    CI0067
            10            FR01-CTLNI  PICTURE  X.                       CI0067
            10            FR01-AYSIDA PICTURE  9(3).                    CI0067
            10            FR01-AYSID  PICTURE  9(5).                    CI0067
            10            FR01-CTBMC  PICTURE  99.                      CI0067
            10            FR01-CINAR  PICTURE  99.                      CI0067
            10            FR01-CPHTR  PICTURE  X.                       CI0067
            10            FR01-CDSTR  PICTURE  XX.                      CI0067
            10            FR01-CQACT  PICTURE  999.                     CI0067
            10            FR01-CIRAS  PICTURE  999.                     CI0067
            10            FR01-CIRAT  PICTURE  999.                     CI0067
            10            FR01-CLRAY  PICTURE  9(5).                    CI0067
            10            FR01-CATTP  PICTURE  X.                       CI0067
      *
      *
      *
      *
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

      *AREA TO HOLD CTID FOR 'FROM' OR 'TO' ACCT BEFORE BEING PASSED
      *TO CI0135
      *!WI
       01  PC01-CTID
                        PICTURE X(27).                                  CI0067
                                                                        AM0135
      *-----> PCB address list for calling CI0135...                    AM0135
      *                                                                 AM0135
       01                 CI0135-PCB-ADDRESS-LIST.                      AM0135
           05             CI0135-PCB-CH1P-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CCRP-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CPRP-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CBTP-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CA1P-PTR1      POINTER.            AM0135
                                                                        AM0022
      ******************************************************************AM0022
      **     PCB ADDRESS LIST FOR CI0022.  MODULE CI0022 WILL NEED     *AM0022
      **     PCB'S FOR:                                                *AM0022
      **            ARRANGEMENT ACCOUNT SECONDARY INDEX(ARAY)          *AM0022
      **            ARRANGEMENT DATABASE(AR1P)                         *AM0022
      **            ARRANGEMENT DEST ACCOUNT SECONDARY INDEX (AREY)    *AM0022
      ******************************************************************AM0022
                                                                        AM0022
       01  CI0022F-PCB-ADDRESS-LIST.                                    AM0022
           05  CI0022F-PCB-ARAY-PTR1      POINTER.                      AM0022
           05  CI0022F-PCB-AR1P-PTR1      POINTER.                      AM0022
           05  CI0022F-PCB-AREY-PTR1      POINTER.                      AM0022

      *PASS AREA TO/FROM CI0022 (BA/BILLING INTO ACCT; SD IN/OUT OF ACCT
      *!WF DSP=BE DSL=DU SEL=26 FOR=I LEV=1 PLT=PF
       01                 BE00.                                         CI0067
          05              BE00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00238).                  CI0067
       01                 BE26  REDEFINES      BE00.                    CI0067
            10            BE26-C299.                                    CI0067
            11            BE26-CTID.                                    CI0067
            12            BE26-CTIDA  PICTURE  9(3).                    CI0067
            12            BE26-CTIDN.                                   CI0067
            13            BE26-CTIDNP PICTURE  X(13).                   CI0067
            13            BE26-CTIDND PICTURE  9(11).                   CI0067
            10            BE26-DCACG  PICTURE  9(8).                    CI0067
            10            BE26-MAPPN  PICTURE  X(10).                   CI0067
            10            BE26-FILLER PICTURE  X(90).                   CI0067
            10            BE26-IFDAB  PICTURE  X.                       CI0067
            10            BE26-IFDAG  PICTURE  X.                       CI0067
            10            BE26-IFDAP  PICTURE  X.                       CI0067
            10            BE26-IFDAS  PICTURE  X(01).                   CI0067
            10            BE26-ISINF  PICTURE  X(01).                   CI0067
            10            BE26-ISOUT  PICTURE  X(01).                   CI0067
            10            BE26-ISINO  PICTURE  X.                       CI0067
            10            BE26-IGBAR  PICTURE  X.                       CI0067
            10            BE26-FILLER PICTURE  X(95).                   CI0067

                                                                        AM0021
      ******************************************************************AM0021
      **     PCB ADDRESS LIST FOR CI0021.  MODULE CI0021 WILL NEED     *AM0021
      **     PCB'S FOR:                                                *AM0021
      **                CERTS ACCOUNT DATABASE(CA1P)                   *AM0021
      **                LIFE MASTER DATABASE(LM1P)                     *AM0021
      ******************************************************************AM0021
                                                                        AM0021
       01  CI0021G-PCB-ADDRESS-LIST.                                    AM0021
           05  CI0021G-PCB-CA1P-PTR1      POINTER.                      AM0021
           05  CI0021G-PCB-LM1P-PTR1      POINTER.                      AM0021

      *PASS AREA TO/FROM CI0021
      *!WF DSP=PE DSL=DU SEL=22 FOR=I DES=1 LEV=1 PLT=PG

      *
      ******************************************************************
      ** THIS SEGMENT IS THE INPUT / OUTPUT SEGMENT FOR MODULE CI0135  *
      ******************************************************************
      *!WF DSP=PJ DSL=PJ SEL=02 FOR=I DES=1 LEV=1 PLT=PJ
       01                 PJ02.                                         CI0067
            10            PJ02-CTID   PICTURE  X(27).                   CI0067
            10            PJ02-DCACG  PICTURE  9(8).                    CI0067
            10            PJ02-ACCTV8 PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-AIDOL1 PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-AUINT1 PICTURE  S9(9)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-CXCSV  PICTURE  S9(7)V9(2)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-PCIRB5 PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-PANYDD PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-PCIRA5 PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-PANYDF PICTURE  9(3)V99                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-PCIRCB PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-PANYDG PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-PPART  PICTURE  9(3)V99                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-PMRTN  PICTURE  9(3)V99                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-PMRTEB PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-PBRITD PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-CEIAPI PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-CEIRND PICTURE  9(8).                    CI0067
            10            PJ02-CEIT   PICTURE  9(3).                    CI0067
            10            PJ02-DMATUR PICTURE  9(8).                    CI0067
            10            PJ02-AMTUR  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-CELBDT PICTURE  9(8).                    CI0067
            10            PJ02-DTRME  PICTURE  9(8).                    CI0067
            10            PJ02-NBSEI  PICTURE  999V99                   CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-NBSEIC PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-TRPTH  PICTURE  X(30).                   CI0067
            10            PJ02-CELBL  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-ALINT  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-PELIRB PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-ASANP  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-AAPAA  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-IQLIF  PICTURE  X.                       CI0067
            10            PJ02-QMTHAA PICTURE  9(2).                    CI0067
            10            PJ02-QMTHCC PICTURE  9(2).                    CI0067
            10            PJ02-QYEARA PICTURE  9(2).                    CI0067
            10            PJ02-DANNIA PICTURE  9(8).                    CI0067
            10            PJ02-PBONS  PICTURE  9(2).                    CI0067
            10            PJ02-AARQDA PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-AACFA  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-AIEPAA PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-CVSUR  PICTURE  X(30).                   CI0067
            10            PJ02-CPRDA1 PICTURE  9(3).                    CI0067
            10            PJ02-DFYR   PICTURE  9(4).                    CI0067
            10            PJ02-DFYRB  PICTURE  9(4).                    CI0067
            10            PJ02-DVALU  PICTURE  9(8).                    CI0067
            10            PJ02-DNIPM  PICTURE  9(8).                    CI0067
            10            PJ02-CIPFM  PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-CESLD  PICTURE  9(8).                    CI0067
            10            PJ02-CEHCD  PICTURE  9(3)                     CI0067
                          OCCURS       006     TIMES.                   CI0067
            10            PJ02-CETYPC PICTURE  9(2).                    CI0067
            10            PJ02-CEOTP  PICTURE  9(1).                    CI0067
            10            PJ02-CEIIS  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-DTRME1 PICTURE  9(8).                    CI0067
            10            PJ02-CEFOIM PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-CEIPDA PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-GECTR  PICTURE  99.                      CI0067
            10            PJ02-GMKTS.                                   CI0067
            11            PJ02-DTRME2 PICTURE  9(8)                     CI0067
                          OCCURS       005     TIMES.                   CI0067
            11            PJ02-DTRME3 PICTURE  9(8)                     CI0067
                          OCCURS       005     TIMES.                   CI0067
            10            PJ02-PRCOD  PICTURE  9(5).                    CI0067
            10            PJ02-CEFOTR PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            PJ02-DGPED  PICTURE  9(8).                    CI0067
            10            PJ02-DIPED  PICTURE  9(8).                    CI0067
            10            PJ02-FILLER PICTURE  X(27).                   CI0067
      *
      *
      *
      *
       01                 SS01.                                         CI0067
            10            SS01-SS01K.                                   CI0067
            11            SS01-CTIDA  PICTURE  9(3).                    CI0067
            11            SS01-PRCOD  PICTURE  9(5).                    CI0067
            11            SS01-CPRSCN PICTURE  9(9).                    CI0067
            10            SS01-PRCODI PICTURE  9(5).                    CI0067
            10            SS01-PRCODS PICTURE  9(5).                    CI0067
            10            SS01-CLORN1 PICTURE  X(45).                   CI0067
            10            SS01-CLORN2 PICTURE  X(45).                   CI0067
            10            SS01-MFND1  PICTURE  X(22).                   CI0067
            10            SS01-MFND2  PICTURE  X(14).                   CI0067
            10            SS01-MFND3  PICTURE  XXX.                     CI0067
            10            SS01-DS01.                                    CI0067
            11            SS01-NCUSP  PICTURE  X(06).                   CI0067
            11            SS01-CCUSP  PICTURE  XX.                      CI0067
            11            SS01-GECKD  PICTURE  9.                       CI0067
            11            SS01-UCUSP  PICTURE  XX.                      CI0067
            10            SS01-DS22                                     CI0067
                          REDEFINES            SS01-DS01.               CI0067
            11            SS01-CCSIP  PICTURE  X(09).                   CI0067
            11            SS01-CCUSP1 PICTURE  XX.                      CI0067
            10            SS01-DPRDA  PICTURE  9(8).                    CI0067
            10            SS01-DPRDI  PICTURE  9(8).                    CI0067
            10            SS01-DPRDT  PICTURE  9(8).                    CI0067
            10            SS01-CTYFI  PICTURE  9(02).                   CI0067
            10            SS01-GEFYE  PICTURE  9(4).                    CI0067
            10            SS01-APARS  PICTURE  99V9(7)                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            SS01-IASSC  PICTURE  X.                       CI0067
            10            SS01-IASSS  PICTURE  X.                       CI0067
            10            SS01-IRPUR  PICTURE  X.                       CI0067
            10            SS01-IRACC  PICTURE  X.                       CI0067
            10            SS01-IDDIV  PICTURE  X.                       CI0067
            10            SS01-IASSD  PICTURE  X.                       CI0067
            10            SS01-IDIVL  PICTURE  X.                       CI0067
            10            SS01-IDPAS  PICTURE  X.                       CI0067
            10            SS01-IDTEX  PICTURE  X.                       CI0067
            10            SS01-IFEDF  PICTURE  X.                       CI0067
            10            SS01-QFOSPD PICTURE  9(2).                    CI0067
            10            SS01-CLDTY  PICTURE  XX.                      CI0067
            10            SS01-QPRCF  PICTURE  9.                       CI0067
            10            SS01-QDMBR  PICTURE  99.                      CI0067
            10            SS01-CMBEM  PICTURE  9.                       CI0067
            10            SS01-AMWIP  PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            SS01-AMBIC  PICTURE  9(5)V99                  CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            SS01-ARMSV  PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            SS01-QDRVP  PICTURE  99.                      CI0067
            10            SS01-NIDTB  PICTURE  9(5).                    CI0067
            10            SS01-CRTBL  PICTURE  99.                      CI0067
            10            SS01-CSTLD  PICTURE  X.                       CI0067
            10            SS01-IFOFD  PICTURE  X.                       CI0067
            10            SS01-IREFD  PICTURE  X.                       CI0067
            10            SS01-FILLER PICTURE  X(2).                    CI0067
            10            SS01-AEFEE  PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            SS01-GEMIN                                    CI0067
                          OCCURS       005     TIMES.                   CI0067
            11            SS01-COOBF  PICTURE  9(2).                    CI0067
            11            SS01-AMTRO  PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            SS01-AMDER  PICTURE  S9(5)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            SS01-IFTDY  PICTURE  X.                       CI0067

       01  W-SW00-AREAS.

      *  MISCELLANEOUS CF SWITCHES USED FOR SEGMENT ACCESS
      *  '1' = FOUND
      *  '0' = NOT FOUND
           05  AA10-CF                 PIC X(01).
           05  AA20-CF                 PIC X(01).
           05  AA85-CF                 PIC X(01).

      ******************************************************************ADUTAB
      **              TABLE TA5B ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA5B-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=5B FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA5B.                                                CI0067
           04    G-TA5B-PARAM.                                          CI0067
             10  G-TA5B-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0067
                        VALUE      +154.                                CI0067
             10  G-TA5B-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0067
                        VALUE      +001.                                CI0067
             10  G-TA5B-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0067
                        VALUE      +017.                                CI0067
             10  G-TA5B-NUAPP  PICTURE 99                               CI0067
                        VALUE       0.                                  CI0067
             10  G-TA5B-NUTAB  PICTURE X(6)                             CI0067
                        VALUE 'TA005B'.                                 CI0067
             10  G-TA5B-TABFO  PICTURE XX                 VALUE SPACE.  CI0067
             10  G-TA5B-TABCR  PICTURE XX                 VALUE SPACE.  CI0067
             10  G-TA5B-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0067
             10  G-TA5B-NUSSC  PICTURE X  VALUE   ' '.                  CI0067
             10  G-TA5B-NUSSY  PICTURE X                  VALUE SPACE.  CI0067
             10  G-TA5B-TRANID PICTURE X(4)               VALUE SPACE.  CI0067
             10  G-TA5B-FILSYS.                                         CI0067
             15  G-TA5B-USERC  PICTURE X(6)               VALUE SPACE.  CI0067
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0067
           04             TA5B.                                         CI0067
            10            TA5B-GAPSC.                                   CI0067
            11            TA5B-CTIDA  PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            TA5B-PRCOD  PICTURE  9(5)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            TA5B-PRSCD  PICTURE  X(9)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-PRCODX PICTURE  9(5)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            TA5B-PRCSUB PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-PRCAUT PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-PRCBAS PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-PRCSTK PICTURE  XX                       CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-PRCPRE PICTURE  X(4)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-IBDUP  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-IUSPR  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-CVSYS  PICTURE  X(2)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-IDTOD  PICTURE  X(1)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-GRSFC  PICTURE  99                       CI0067
                          VALUE                ZERO.                    CI0067
            10            TA5B-ZDA18  PICTURE  X(18)                    CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-CMPCTB PICTURE  X(4)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-ITERM  PICTURE  X(1)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-AMFAC  PICTURE  S9(7)                    CI0067
                          VALUE                ZERO.                    CI0067
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-CPRBK  PICTURE  X(3)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-CFXDM  PICTURE  99                       CI0067
                          VALUE                ZERO.                    CI0067
            10            TA5B-NGLCS  PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-NDFCS  PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-CTNLI  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-CBANK  PICTURE  X(03)                    CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-ISYPO  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-ISYPP  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-ICOPT  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-IANPY  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-IDSAR  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-ICIPT  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-IANDS  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-IKPMA  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-INMWT  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-IVANT  PICTURE  X(1)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-ISDAV  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-IUDAV  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TA5B-ZDA15  PICTURE  X(15)                    CI0067
                          VALUE                SPACE.                   CI0067
      **                                                                ADUTAB

      *INDICATES WHETHER TA5B ENTRY FOUND
      *'1' = FOUND
      *'0' = NOT FOUND
        01 TA5B-CF           PIC X(1) VALUE '0'.

      ******************************************************************ADUTAB
      **              TABLE TF09 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TF09-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TF DSL=TF SEL=09 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TF09.                                                CI0067
           04    G-TF09-PARAM.                                          CI0067
             10  G-TF09-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0067
                        VALUE      +142.                                CI0067
             10  G-TF09-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0067
                        VALUE      +001.                                CI0067
             10  G-TF09-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0067
                        VALUE      +005.                                CI0067
             10  G-TF09-NUAPP  PICTURE 99                               CI0067
                        VALUE       0.                                  CI0067
             10  G-TF09-NUTAB  PICTURE X(6)                             CI0067
                        VALUE 'TF0009'.                                 CI0067
             10  G-TF09-TABFO  PICTURE XX                 VALUE SPACE.  CI0067
             10  G-TF09-TABCR  PICTURE XX                 VALUE SPACE.  CI0067
             10  G-TF09-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0067
             10  G-TF09-NUSSC  PICTURE X  VALUE   ' '.                  CI0067
             10  G-TF09-NUSSY  PICTURE X                  VALUE SPACE.  CI0067
             10  G-TF09-TRANID PICTURE X(4)               VALUE SPACE.  CI0067
             10  G-TF09-FILSYS.                                         CI0067
             15  G-TF09-USERC  PICTURE X(6)               VALUE SPACE.  CI0067
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0067
           04             TF09.                                         CI0067
            10            TF09-CERRE2 PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TF09-CERRE3 PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TF09-TERMT  PICTURE  X(66)                    CI0067
                          VALUE                SPACE.                   CI0067
            10            TF09-TERMT3 PICTURE  X(66)                    CI0067
                          VALUE                SPACE.                   CI0067
      **                                                                ADUTAB
      *
      ******************************************************************
      ** SEGMENT THAT CONTAINS SAVED CT01 SEGMENT (FOR TO ACCOUNT)     *
      ******************************************************************
      *
      *!WF DSP=TO DSL=CT SEL=01 FOR=I DES=1 LEV=1 PLT=TO
       01                 TO01.                                         CI0067
            10            TO01-CT01K.                                   CI0067
            11            TO01-C299.                                    CI0067
            12            TO01-CTID.                                    CI0067
            13            TO01-CTIDA  PICTURE  9(3).                    CI0067
            13            TO01-CTIDN.                                   CI0067
            14            TO01-CTIDNP PICTURE  X(13).                   CI0067
            14            TO01-CTIDND PICTURE  9(11).                   CI0067
            10            TO01-GECKD  PICTURE  9.                       CI0067
            10            TO01-GEMDA  PICTURE  9(8).                    CI0067
            10            TO01-NSEQ4B PICTURE  9(8)                     CI0067
                          BINARY.                                       CI0067
            10            TO01-GECUC  PICTURE  99.                      CI0067
            10            TO01-CTAUL  PICTURE  9(3).                    CI0067
            10            TO01-DIRAC  PICTURE  9(4).                    CI0067
            10            TO01-CTCCI  PICTURE  X.                       CI0067
            10            TO01-CTCUS  PICTURE  999.                     CI0067
            10            TO01-CTEFD  PICTURE  9(8).                    CI0067
            10            TO01-CTIAD  PICTURE  9(8).                    CI0067
            10            TO01-CLCUS  PICTURE  99.                      CI0067
            10            TO01-CAMMB  PICTURE  X(3).                    CI0067
            10            TO01-CKPMM  PICTURE  X.                       CI0067
            10            TO01-CTLAD  PICTURE  9(8).                    CI0067
            10            TO01-IPERS  PICTURE  X.                       CI0067
            10            TO01-AUNCB  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            TO01-CTLAT  PICTURE  9(8).                    CI0067
            10            TO01-CTLATC PICTURE  9(6).                    CI0067
            10            TO01-IMEGA  PICTURE  X.                       CI0067
            10            TO01-DIRAB  PICTURE  9(8).                    CI0067
            10            TO01-COLRQ  PICTURE  X.                       CI0067
            10            TO01-ZDA04  PICTURE  X(4).                    CI0067
            10            TO01-CTLPD  PICTURE  9(8).                    CI0067
            10            TO01-CIRASP PICTURE  9.                       CI0067
            10            TO01-CIRATP PICTURE  99.                      CI0067
            10            TO01-DRTHC  PICTURE  9(8).                    CI0067
            10            TO01-CPPTC  PICTURE  X.                       CI0067
            10            TO01-ZDA06  PICTURE  X(6).                    CI0067
            10            TO01-CTACD  PICTURE  9(8).                    CI0067
            10            TO01-CTNLI  PICTURE  X.                       CI0067
            10            TO01-CTRHO  PICTURE  9(8).                    CI0067
            10            TO01-CTSGD  PICTURE  9(8).                    CI0067
            10            TO01-CPATP  PICTURE  X(1).                    CI0067
            10            TO01-IRSTA  PICTURE  X.                       CI0067
            10            TO01-CTSTA  PICTURE  99.                      CI0067
            10            TO01-CTSSC  PICTURE  99.                      CI0067
            10            TO01-PRLIN  PICTURE  9(3).                    CI0067
            10            TO01-PRCOD  PICTURE  9(5).                    CI0067
            10            TO01-PRSCD  PICTURE  X(9).                    CI0067
            10            TO01-CTLNI  PICTURE  X.                       CI0067
            10            TO01-AYSIDA PICTURE  9(3).                    CI0067
            10            TO01-AYSID  PICTURE  9(5).                    CI0067
            10            TO01-CTBMC  PICTURE  99.                      CI0067
            10            TO01-CINAR  PICTURE  99.                      CI0067
            10            TO01-CPHTR  PICTURE  X.                       CI0067
            10            TO01-CDSTR  PICTURE  XX.                      CI0067
            10            TO01-CQACT  PICTURE  999.                     CI0067
            10            TO01-CIRAS  PICTURE  999.                     CI0067
            10            TO01-CIRAT  PICTURE  999.                     CI0067
            10            TO01-CLRAY  PICTURE  9(5).                    CI0067
            10            TO01-CATTP  PICTURE  X.                       CI0067
      *
      *
      *
      *
      ******************************************************************
      *** WORKING STORAGE SEGMENTS TO STORE THE ACCOUNT SPECIFIC      **
      *** DETAILS FROM THE TSQ CREATED BY CI0299.                     **
      ******************************************************************
      *
      *!WF DSP=TS DSL=G6 SEL=989Z FOR=I DES=2 LEV=1
      * PLT=TS
       01                 TS9Z.                                         CI0067
            10            TS9Z-CERRE1 PICTURE  X(5)                     CI0067
                          OCCURS       004     TIMES                    CI0067
                          VALUE                SPACE.                   CI0067
            10            TS9Z-CERRE3 PICTURE  X(5)                     CI0067
                          OCCURS       004     TIMES                    CI0067
                          VALUE                SPACE.                   CI0067
            10            TS9Z-IERRC  PICTURE  X                        CI0067
                          OCCURS       004     TIMES                    CI0067
                          VALUE                SPACE.                   CI0067
            10            TS9Z-TERMT  PICTURE  X(66)                    CI0067
                          OCCURS       004     TIMES                    CI0067
                          VALUE                SPACE.                   CI0067
       01                 TS98.                                         CI0067
            10            TS98-CERRE1 PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TS98-CERRE2 PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TS98-CERRE3 PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TS98-CAPIR1 PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-CERREF PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TS98-CERRED PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TS98-CTID   PICTURE  X(27)                    CI0067
                          VALUE                SPACE.                   CI0067
            10            TS98-PRCOD  PICTURE  9(5)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-CPRSCN PICTURE  9(9)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-ACINEV PICTURE  S9(9)V99                 CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-AADDCD PICTURE  S9(7)V99                 CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-CELAT1 PICTURE  9(6)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-CELATD PICTURE  9(8)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-CMGAC  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TS98-DDVAC  PICTURE  9(8)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-QSHES4 PICTURE  S9(10)V999               CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-QSHUI  PICTURE  S9(10)V9(3)              CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-GRID   PICTURE  X(13)                    CI0067
                          VALUE                SPACE.                   CI0067
            10            TS98-QSHOM2 PICTURE  S9(10)V999               CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-NDCUS  PICTURE  X(9)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TS98-CSTKR5 PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            TS98-AYTDD  PICTURE  S9(13)V99                CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-DASOF  PICTURE  9(8)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-QSHON  PICTURE  S9(10)V999               CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-APRAP  PICTURE  S9(11)V99                CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-AACCD2 PICTURE  S9(11)V99                CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-AFAV10 PICTURE  S9(4)V9(3)               CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-ANGOF1 PICTURE  S9(9)V99                 CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-AGOFD2 PICTURE  S9(9)V99                 CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-IBIFU  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TS98-ICOVE  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TS98-QCSAC  PICTURE  S9(11)V9(4)              CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-ACOSA  PICTURE  S9(13)V99                CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-ACACB  PICTURE  S9(13)V99                CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-ICOVN  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TS98-QNCOS  PICTURE  S9(11)V9(4)              CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-ANCSA  PICTURE  S9(13)V99                CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-ANCBA  PICTURE  S9(13)V99                CI0067
                          VALUE                ZERO.                    CI0067
            10            TS98-CRCDL  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            TS98-FILLER PICTURE  X(30)                    CI0067
                          VALUE                SPACE.                   CI0067
      *
      *!WI
       01                 WS00-CPRSCN     VALUE ZEROS
                        PICTURE 9(9).                                   CI0067
      *-----------------------------------------------------------------AAER85
      *WORKING STORAGE VARIABLES REQUIRED FOR THE BROKER CALL ERROR     AAER85
      *HANDLING MACRO                                                   AAER85
      *-----------------------------------------------------------------AAER85
      *                                                                 AAER85
      *LENGTH OF THE FIXED AREA OF THE RESPONSE FROM DBI5000N           AAER85
      *!WI pl=WA040                                                     AAER85
       01               WS-FIX-GELL    VALUE ZEROS                      AAER85
                        PICTURE 9(4)                                    CI0067
                          BINARY.                                       CI0067
      *                                                                 AAER85
      *LENGTH OF THE VARIABLE AREA OF THE RESPONSE FROM DBI5000N        AAER85
      *!WI pl=WA055                                                     AAER85
       01               WS-VAR-GELL    VALUE ZEROS                      AAER85
                        PICTURE 9(4)                                    CI0067
                          BINARY.                                       CI0067
      *                                                                 AAER85
      *CALCULATED POSITION OF THE ERROR CODE IN THE RESPONSE SECTION    AAER85
      *!WI pl=WA070                                                     AAER85
       01               WS-ERR-GELL    VALUE ZEROS                      AAER85
                        PICTURE 9(4)                                    CI0067
                          BINARY.                                       CI0067
      *                                                                 AAER85
      *TOTAL NUMBER OF ROWS RETURNED IN THE VIEW RESPONSE.              AAER85
      *!WI pl=WA085                                                     AAER85
       01               WS-NRURO       VALUE ZEROS                      AAER85
                        PICTURE 9(3).                                   CI0067
      *                                                                 AAER85
      *ERROR CODE SET BY THE MACRO.                                     AAER85
      *!WI pl=WA100                                                     AAER85
       01               WS00-IERRC       VALUE SPACES                   AAER85
                        PICTURE X.                                      CI0067
      *                                                                 AAER85
      *SEVERITY CODE SET BY THE MACRO.                                  AAER85
      *!WI pl=WA115                                                     AAER85
       01               WS00-CSEVR       VALUE SPACES                   AAER85
                        PICTURE X.                                      CI0067
      *                                                                 AAER85
      *ERROR CODE FOR THE NON CRITICAL ERRORS NEED TO IGNORE.           AAER85
      *!WI pl=WA130                                                     AAER85
       01               WS01-IERRC       VALUE SPACES                   AAER85
                        PICTURE X.                                      CI0067
      *                                                                 AAER85
      *ARRAY FOR STORING THE LIST OF THE ERROR CODES NEED TO IGNORE     AAER85
       01                 WE00-ERROR-TABLE.                             AAER85
         05               WE00            OCCURS 25 TIMES.              AAER85
      *!WI pl=WA155                                                     AAER85
           10             WE00-CERRE1     VALUE SPACES                  AAER85
                        PICTURE X(5).                                   CI0067
      *                                                                 AAER85
      *VARIABLE FOR STORING THE TYPE OF ERROR HANDLING REQUIRED         AAER85
       01               WS00-ERROR-TYPE  PIC X(1)  VALUE SPACES.        AAER85
      *                                                                 AAER85
      *VARIABLE FOR STORING THE ERROR CODE                              AAER85
      *!WI pl=WA185                                                     AAER85
       01               WS00-CERRE1      VALUE SPACES                   AAER85
                        PICTURE X(5).                                   CI0067
      *VARIABLE FOR STORING IGNORE ERROR INDICATOR                      AAER85
       01               WS00-IGNORE-ERROR PIC X VALUE 'N'.              AAER85
      *VARIABLE FOR STORING THE AREA OF THE ERROR.                      AAER85
       01               WS00-ERROR-AREA   PIC X VALUE SPACES.           AAER85
      ******************************************************************AAER85
      *INPUT AND OUTPUT SEGMENTS TO THE DST VIEWS FOR CALLING DBI5000N.*AAER85
      ******************************************************************AAER85
      *                  ACCOUNT VIEW - 2933                            AAER85
      ******************************************************************AAER85
      *          SQ1A    -    ACCOUNT INFO VIEW REQUEST                *AAER85
      *          SQ5A    -    ACCOUNT INFO VIEW RESPONSE               *AAER85
      ******************************************************************AAER85
      **                   GROUP VIEW - 2939                           *AAER85
      ******************************************************************AAER85
      *          SQ1G    -    GROUP INFO VIEW REQUEST                  *AAER85
      *          SQ5G    -    GROUP INFO VIEW RESPONSE                 *AAER85
      ******************************************************************AAER85
      *                 TRANSACTION HISTORY VIEWS                      *AAER85
      *       SINGLE TRANSACTION VIEW - 2934                           *AAER85
      *         LIST TRANSACTION VIEW - 2935                           *AAER85
      *   ASSOCIATED TRANSACTION VIEW - 2936                           *AAER85
      ******************************************************************AAER85
      *          SQ1S    -    SINGLE TRAN INFO VIEW REQUEST            *AAER85
      *          SQ5S    -    SINGLE TRAN INFO VIEW RESPONSE           *AAER85
      ******************************************************************AAER85
      *          SQ1C    -    LIST TRAN INFO VIEW REQUEST              *AAER85
      *          SQ5C    -    LIST TRAN INFO VIEW RESPONSE             *AAER85
      ******************************************************************AAER85
      *          SQ1R    -    ASSOCIATED TRAN INFO VIEW REQUEST       * AAER85
      *          SQ5R    -    ASSOCIATED TRAN INFO VIEW RESPONSE      * AAER85
      ***************************************************************** AAER85
      *                  CALCULATIONS  VIEW                           * AAER85
      *          ACCOUNT VALUE VIEW - 2949                            * AAER85
      *             GOOD FUNDS VIEW - 2940                            * AAER85
      ***************************************************************** AAER85
      *          SQ1E    -    ACCOUNT VALUE VIEW REQUEST              * AAER85
      *          SQ5E    -    ACCOUNT VALUE VIEW RESPONSE             * AAER85
      ***************************************************************** AAER85
      *          SQ1F    -    GOOF FUNDS VIEW REQUEST                 * AAER85
      *          SQ5F    -    GOOD FUNDS VIEW RESPONSE                * AAER85
      ***************************************************************** AAER85
      *                      CDSC   VIEWS                             * AAER85
      *                    FJX8 VIEW - 2903                           * AAER85
      *                    FJXC VIEW - 2907                           * AAER85
      ***************************************************************** AAER85
      *          SQ1Q    -    FJX8 HYPO CALC VIEW REQUEST             * AAER85
      *          SQ5Q    -    FJX8 HYPO CALC VIEW RESPONSE            * AAER85
      ***************************************************************** AAER85
      *          SQ1X    -    FJXC PAF HYPO CALC VIEW REQUEST         * AAER85
      *          SQ5X    -    FJXC PAF HYPO CALC VIEW RESPONSE        * AAER85
      ***************************************************************** AAER85
      *          SQ1T    -    TAX TRAN INFO VIEW REQUEST              * AAER85
      *          SQ5T    -    TAX TRAN INFO VIEW RESPONSE             * AAER85
      ***************************************************************** AAER85
      *          MCB ACCOUNT INFORMATION VIEW - 4838                  * AAER85
      ***************************************************************** AAER85
      *          SQ1Z    -    MCB ACCOUNT INFORMATION VIEW REQUEST    * AAER85
      *          SQ5Z    -    MCB ACCOUNT INFORMATION VIEW RESPONSE   * AAER85
      ***************************************************************** AAER85
      *          MCB GAIN LOSS INFORMATION VIEW 4865                  * AAER85
      ***************************************************************** AAER85
      *          SQ2A    -    MCB GAIN/LOSS INFORMATION VIEW REQUEST  * AAER85
      *          SQ6Z    -    MCB GAIN/LOSS INFORMATION VIEW RESPONSE * AAER85
      ***************************************************************** AAER85
      *!WF DSP=SQ DSL=SQ SEL=1A5A1E5E FOR=I DES=2 LEV=1                 AAER85
       01                 SQ1A.                                         CI0067
            10            SQ1A-CSYSI  PICTURE  X(4)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ1A-CFSPN  PICTURE  X(3)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ1A-NRERO  PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            SQ1A-FILLER PICTURE  X(30)                    CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ1A-GTD71                                    CI0067
                          OCCURS 0 TO  125     TIMES                    CI0067
                          DEPENDING  ON        SQ1A-NRERO.              CI0067
            11            SQ1A-CTID.                                    CI0067
            12            SQ1A-CTIDA  PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            12            SQ1A-CTIDNP PICTURE  X(13)                    CI0067
                          VALUE                SPACE.                   CI0067
            12            SQ1A-CTIDND PICTURE  9(11)                    CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ1A-PRCOD  PICTURE  9(5)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ1A-CPRSCN PICTURE  9(9)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ1A-FILLER PICTURE  X(30)                    CI0067
                          VALUE                SPACE.                   CI0067
       01                 SQ1E.                                         CI0067
            10            SQ1E-CSYSI  PICTURE  X(4)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ1E-CFSPN  PICTURE  X(3)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ1E-NRERO  PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            SQ1E-FILLER PICTURE  X(33)                    CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ1E-GTD79                                    CI0067
                          OCCURS 0 TO  150     TIMES                    CI0067
                          DEPENDING  ON        SQ1E-NRERO.              CI0067
            11            SQ1E-CTID.                                    CI0067
            12            SQ1E-CTIDA  PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            12            SQ1E-CTIDNP PICTURE  X(13)                    CI0067
                          VALUE                SPACE.                   CI0067
            12            SQ1E-CTIDND PICTURE  9(11)                    CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ1E-PRCOD  PICTURE  9(5)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ1E-CPRSCN PICTURE  9(9)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ1E-DCACG  PICTURE  9(8)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ1E-FILLER PICTURE  X(30)                    CI0067
                          VALUE                SPACE.                   CI0067
       01                 SQ1L.                                         CI0067
            10            SQ1L-NPVERH PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            SQ1L-NPVERC PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            SQ1L-NPVERD PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            SQ1L-NVIEW  PICTURE  X(4)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ1L-NCUSR2 PICTURE  X(12)                    CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ1L-CPRT2  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ1L-MRPIB1 PICTURE  X(08)                    CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ1L-CLOGY  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ1L-QTOUT  PICTURE  9(5)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            SQ1L-GELLP1 PICTURE  S9(5)                    CI0067
                          VALUE                ZERO                     CI0067
                          BINARY.                                       CI0067
            10            SQ1L-GELLP2 PICTURE  S9(5)                    CI0067
                          VALUE                ZERO                     CI0067
                          BINARY.                                       CI0067
            10            SQ1L-CVSIZ  PICTURE  X(1)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ1L-FILLER PICTURE  X(49)                    CI0067
                          VALUE                SPACE.                   CI0067
       01                 SQ2L.                                         CI0067
            10            SQ2L-NPID4  PICTURE  X(4)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ2L-NVIEW  PICTURE  X(4)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ2L-CTRID  PICTURE  X(4)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ2L-CPRGSX PICTURE  X(8)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ2L-COMND  PICTURE  X(8)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ2L-CERRE1 PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ2L-CSEVR1 PICTURE  X(2)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ2L-GERD1.                                   CI0067
            11            SQ2L-FILLER PICTURE  X(3)                     CI0067
                          VALUE                SPACE.                   CI0067
            11            SQ2L-CERRE2 PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ2L-TERMT  PICTURE  X(66)                    CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ2L-GERD2.                                   CI0067
            11            SQ2L-FILLER PICTURE  X(3)                     CI0067
                          VALUE                SPACE.                   CI0067
            11            SQ2L-CERRE3 PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ2L-TER255 PICTURE  X(255)                   CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ2L-FILLER PICTURE  X(50)                    CI0067
                          VALUE                SPACE.                   CI0067
       01                 SQ3L.                                         CI0067
            10            SQ3L-GELLP3 PICTURE  S9(5)                    CI0067
                          VALUE                ZERO                     CI0067
                          BINARY.                                       CI0067
            10            SQ3L-GELLP4 PICTURE  S9(5)                    CI0067
                          VALUE                ZERO                     CI0067
                          BINARY.                                       CI0067
            10            SQ3L-NAREA  PICTURE  9(03)                    CI0067
                          VALUE                ZERO.                    CI0067
            10            SQ3L-GEOPDM PICTURE  X(8)                     CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ3L-CAPIR1 PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            10            SQ3L-FILLER PICTURE  X(50)                    CI0067
                          VALUE                SPACE.                   CI0067
       01                 SQ5A.                                         CI0067
            10            SQ5A-GRFIX.                                   CI0067
            11            SQ5A-CERRE1 PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            11            SQ5A-NRURO  PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5A-FILLER PICTURE  X(32)                    CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ5A-GRVAR                                    CI0067
                          OCCURS 0 TO  125     TIMES                    CI0067
                          DEPENDING  ON        SQ5A-NRURO.              CI0067
            11            SQ5A-CERRE2 PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            11            SQ5A-CTID.                                    CI0067
            12            SQ5A-CTIDA  PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            12            SQ5A-CTIDNP PICTURE  X(13)                    CI0067
                          VALUE                SPACE.                   CI0067
            12            SQ5A-CTIDND PICTURE  9(11)                    CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5A-PRCOD  PICTURE  9(5)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5A-CPRSCN PICTURE  9(9)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5A-ACINEV PICTURE  S9(9)V99                 CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5A-AADDCD PICTURE  S9(7)V99                 CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5A-CELAT1 PICTURE  9(6)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5A-CELATD PICTURE  9(8)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5A-CMGAC  PICTURE  X                        CI0067
                          VALUE                SPACE.                   CI0067
            11            SQ5A-DDVAC  PICTURE  9(8)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5A-QSHES4 PICTURE  S9(10)V999               CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5A-QSHUI  PICTURE  S9(10)V9(3)              CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5A-GRID.                                    CI0067
            12            SQ5A-GRIDC  PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            12            SQ5A-GRIDNP PICTURE  99                       CI0067
                          VALUE                ZERO.                    CI0067
            12            SQ5A-GRIDND PICTURE  9(8)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5A-QSHOM2 PICTURE  S9(10)V999               CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5A-NDCUS  PICTURE  X(9)                     CI0067
                          VALUE                SPACE.                   CI0067
            11            SQ5A-CSTKR5 PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            11            SQ5A-AYTDD  PICTURE  S9(13)V99                CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5A-FILLER PICTURE  X(42)                    CI0067
                          VALUE                SPACE.                   CI0067
       01                 SQ5E.                                         CI0067
            10            SQ5E-GRFIX.                                   CI0067
            11            SQ5E-CERRE1 PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            11            SQ5E-NRURO  PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5E-FILLER PICTURE  X(22)                    CI0067
                          VALUE                SPACE.                   CI0067
            10            SQ5E-GRVAR                                    CI0067
                          OCCURS 0 TO  150     TIMES                    CI0067
                          DEPENDING  ON        SQ5E-NRURO.              CI0067
            11            SQ5E-CERRE2 PICTURE  X(5)                     CI0067
                          VALUE                SPACE.                   CI0067
            11            SQ5E-CTID.                                    CI0067
            12            SQ5E-CTIDA  PICTURE  9(3)                     CI0067
                          VALUE                ZERO.                    CI0067
            12            SQ5E-CTIDNP PICTURE  X(13)                    CI0067
                          VALUE                SPACE.                   CI0067
            12            SQ5E-CTIDND PICTURE  9(11)                    CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5E-PRCOD  PICTURE  9(5)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5E-CPRSCN PICTURE  9(9)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5E-DASOF  PICTURE  9(8)                     CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5E-QSHON  PICTURE  S9(10)V999               CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5E-AADDCD PICTURE  S9(7)V99                 CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5E-APRAP  PICTURE  S9(11)V99                CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5E-AACCD2 PICTURE  S9(11)V99                CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5E-AFAV10 PICTURE  S9(4)V9(3)               CI0067
                          VALUE                ZERO.                    CI0067
            11            SQ5E-FILLER PICTURE  X(30)                    CI0067
                          VALUE                SPACE.                   CI0067
      ***************************************************************** AAER85
      *THIS MACRO INCLUDES THE I/O SEGMENTS FOR 2 VIEWS ONLY. IF THE    AAER85
      *PROGRAM CALLS MORE THAN 2 VIEWS, ADD THEM MANUALLY AFTER LINE 940AAER85
      ***************************************************************** AAER85

      ******************************************************************
      **     WORKING STORAGE MISC FIELDS                               *
      ******************************************************************

       01  W-WORK-MISC.

      *    9'S COMP OF W-WORK-DLAUP2
      *!WI
           05  W-NINE-DCACG
                        PICTURE 9(8).                                   CI0067

      *    IF DEST ACCT FIELD WZ67-CTID > 0, 02 IS MOVED HERE
      *    ELSE                              01 IS MOVED HERE
      *!WI
           05  W-WORK-CPITC
                        PICTURE 99                                      CI0067
                                                  VALUE ZERO.
               88   PAYOUT-TRANSACTION            VALUE 01.
               88   TRANSFER                      VALUE 02.

      *    WORK AREA FOR FUND ACCOUNT VALUE MINUS VALUE OF CERT SHARES
      *    (SOURCE ACCOUNT)
      *!WI
           05  W-WORK-AACTVD
                        PICTURE S9(11)V99.                              CI0067

      *    THIS AREA IS USED FOR CALCULATING THE NEW MONEY FOR A FUND
      *    (SOURCE ACCOUNT)
      *!WI
           05  WK00-ACNMO
                        PICTURE S9(9)V99                                CI0067
                          COMPUTATIONAL-3.                              CI0067
      *    THIS AREA IS USED FOR CALCULATING THE OLD MONEY FOR A FUND
      *    (SOURCE ACCOUNT)
      *!WI
           05  WK00-ACOMO
                        PICTURE S9(9)V99                                CI0067
                          COMPUTATIONAL-3.                              CI0067

      *SEGMENTS AND WORKING STORAGE VARIABLES FOR CALLING DBI5000N.     AWSDBI
      *!WF DSP=SQ DSL=SQ SEL=1L2L3L FOR=I DES=1 LEV=1                   AWSDBI
      *                                                                 AWSDBI
       01     WL00-REQUEST.                                             AWSDBI
              05   FILLER    PIC X OCCURS 1 TO 32554                    AWSDBI
                             DEPENDING ON SQ1L-GELLP1.                  AWSDBI
      *                                                                 AWSDBI
       01     WL00-RESPONSE.                                            AWSDBI
              05   FILLER    PIC X OCCURS 1 TO 32594                    AWSDBI
                             DEPENDING ON SQ1L-GELLP2.                  AWSDBI
      *                                                                 AWSDBI
      *WORKING STORAGE VARIABLE FOR NAME OF BROKER MODULE DBI5000N.     AWSDBI
       01     DBI5000N  PIC X(8) VALUE 'DBI5000N'.                      AWSDBI
      *                                                                 AWSDBI

       01               7-WS-ABEND       PIC 9(4)  VALUE ZERO.
       01               7-WS-ABENDX      REDEFINES 7-WS-ABEND.
           05           7-WS-FIRST       PIC X.
           05           FILLER           PIC X(3).

      *DEST ACCT TAXPAYER CLID (FROM CT07 KEY RETURNED IN CT09 CALL)
      *!WI
       01               7-TAXPAYER-CLID
                        PICTURE X(23).                                  CI0067
      *-----------------------------------------------------------
      *WORKING STORAGE FOR USER ID.                             *
      *----------------------------------------------------------
      *!WI
       01               WS00-NCUSR2     VALUE SPACES
                        PICTURE X(12).                                  CI0067
      *------------------------------------------------------------

      ******************************************************************
      *WORKING STORAGE VARIABLES FOR SKIPPING THE PROCESSING FOR THE
      *FROM ACCOUNT EVRYTIME THE MODULE IS CALLED. (F35)
      ******************************************************************
      **** FIRST TIME IN INDICATOR *************************************
       01                 WG00-FIRST-TIME     PIC X.
           88             FIRST-TIME-IN       VALUE 'Y'.
      *!WI
       01                 SV00-CTID    VALUE SPACES
                        PICTURE X(27).                                  CI0067
      *!WI
       01                 WS00-NMESS2  VALUE ZEROS
                        PICTURE S9(6)                                   CI0067
                          COMPUTATIONAL-3.                              CI0067
      *!WI
       01                 WS00-AACTVD  VALUE ZEROS
                        PICTURE S9(11)V99.                              CI0067
      *!WI
       01                 WS00-CEIT    VALUE ZEROS
                        PICTURE 9(3).                                   CI0067
      *!WI
       01                 WS00-QFOSPD  VALUE ZEROS
                        PICTURE 9(2).                                   CI0067
      *!WI
       01                 WS00-AACTV   VALUE ZEROS
                        PICTURE S9(11)V99                               CI0067
                          COMPUTATIONAL-3.                              CI0067
      *!WI
       01                 WS00-INMRC   VALUE SPACES
                        PICTURE X(01).                                  CI0067
      *!WI
       01                 WS00-ACNMO   VALUE ZEROS
                        PICTURE S9(9)V99                                CI0067
                          COMPUTATIONAL-3.                              CI0067
      *!WF DSP=WS DSL=DU SEL=03 FOR=I DES=2 LEV=1 PLT=WT
       01                 WS03.                                         CI0067
            10            WS03-NMESS2 PICTURE  S9(6)                    CI0067
                          VALUE                ZERO                     CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WS03-CMSSF  PICTURE  XX                       CI0067
                          VALUE                SPACE.                   CI0067
            10            WS03-DU09.                                    CI0067
            11            WS03-CMESA  PICTURE  S9(9)                    CI0067
                          VALUE                ZERO                     CI0067
                          BINARY.                                       CI0067
            11            WS03-NMESS3 PICTURE  S9(9)                    CI0067
                          VALUE                ZERO                     CI0067
                          BINARY.                                       CI0067
            11            WS03-CMESB  PICTURE  S9(9)                    CI0067
                          VALUE                ZERO                     CI0067
                          BINARY.                                       CI0067
            11            WS03-CMSST  PICTURE  S9(9)                    CI0067
                          VALUE                ZERO                     CI0067
                          BINARY.                                       CI0067
            11            WS03-CSLNN  PICTURE  S9(9)                    CI0067
                          VALUE                ZERO                     CI0067
                          BINARY.                                       CI0067
            11            WS03-QELLAA PICTURE  S9(9)                    CI0067
                          VALUE                ZERO                     CI0067
                          BINARY.                                       CI0067
            11            WS03-TMESS4 PICTURE  X(512)                   CI0067
                          VALUE                SPACE.                   CI0067
       01   DEBUT-WSS.                                                  CI0067
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0067
            05   IK     PICTURE X.                                      CI0067
       01  CONSTANTES-PAC.                                              CI0067
           05  FILLER  PICTURE X(87)   VALUE                            CI0067
                     '6015 CAT09/08/14CI0067ADMIN   14:34:32CI0067P AMERCI0067
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0067
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0067
           05  NUGNA   PICTURE X(5).                                    CI0067
           05  APPLI   PICTURE X(3).                                    CI0067
           05  DATGN   PICTURE X(8).                                    CI0067
           05  PROGR   PICTURE X(6).                                    CI0067
           05  CODUTI  PICTURE X(8).                                    CI0067
           05  TIMGN   PICTURE X(8).                                    CI0067
           05  PROGE   PICTURE X(8).                                    CI0067
           05  COBASE  PICTURE X(4).                                    CI0067
           05  DATGNC  PICTURE X(10).                                   CI0067
           05  RELEAS  PICTURE X(7).                                    CI0067
           05  DATGE   PICTURE X(10).                                   CI0067
           05  DATSQ   PICTURE X(10).                                   CI0067
       01  DATCE.                                                       CI0067
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0067
         05  DATOR.                                                     CI0067
           10  DATOA  PICTURE XX.                                       CI0067
           10  DATOM  PICTURE XX.                                       CI0067
           10  DATOJ  PICTURE XX.                                       CI0067
       01   VARIABLES-CONDITIONNELLES.                                  CI0067
            05                  FT      PICTURE X VALUE '0'.            CI0067
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0067
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0067
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           IWE00L PICTURE S9(4) VALUE  ZERO.              AAER85
            05           IWE00R PICTURE S9(4) VALUE  ZERO.              AAER85
            05           IWE00M PICTURE S9(4) VALUE +0025.              AAER85
            05           J96FLR PICTURE S9(4) VALUE  ZERO.              AAER85
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0067
            05       5-AA00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0067
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0067
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0067
            05       5-SS00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0067
       01               S-AA10-SSA.                                     CI0067
            10         S1-AA10-SEGNAM PICTURE X(8)                      CI0067
                                      VALUE 'LMSPCON '.                 CI0067
            10         S1-AA10-CCOM   PICTURE X VALUE '*'.              CI0067
            10          S-AA10-CCOD   PICTURE X(5)                      CI0067
                                      VALUE '-----'.                    CI0067
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0067
       01            S-AAU10-SSA.                                       CI0067
            11      S1-AAU10-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'LMSPCON '.                 CI0067
            11      S1-AAU10-CCOM   PICTURE X VALUE '*'.                CI0067
            11       S-AAU10-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            11      S1-AAU10-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(LMSPCONK'.                CI0067
            11       S-AAU10-OPER  PICTURE XX VALUE ' ='.               CI0067
            11       S-AAU10-ALCIDN   PICTURE  9(11).                   CI0067
            11  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01               S-AA20-SSA.                                     CI0067
            10         S1-AA20-SEGNAM PICTURE X(8)                      CI0067
                                      VALUE 'LMSPLON '.                 CI0067
            10         S1-AA20-CCOM   PICTURE X VALUE '*'.              CI0067
            10          S-AA20-CCOD   PICTURE X(5)                      CI0067
                                      VALUE '-----'.                    CI0067
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0067
       01               S-AA25-SSA.                                     CI0067
            10         S1-AA25-SEGNAM PICTURE X(8)                      CI0067
                                      VALUE 'LMSPANA '.                 CI0067
            10         S1-AA25-CCOM   PICTURE X VALUE '*'.              CI0067
            10          S-AA25-CCOD   PICTURE X(5)                      CI0067
                                      VALUE '-----'.                    CI0067
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0067
       01               S-AA66-SSA.                                     CI0067
            10         S1-AA66-SEGNAM PICTURE X(8)                      CI0067
                                      VALUE 'LMSPTLN '.                 CI0067
            10         S1-AA66-CCOM   PICTURE X VALUE '*'.              CI0067
            10          S-AA66-CCOD   PICTURE X(5)                      CI0067
                                      VALUE '-----'.                    CI0067
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0067
       01               S-AA85-SSA.                                     CI0067
            10         S1-AA85-SEGNAM PICTURE X(8)                      CI0067
                                      VALUE 'LMSPUWG '.                 CI0067
            10         S1-AA85-CCOM   PICTURE X VALUE '*'.              CI0067
            10          S-AA85-CCOD   PICTURE X(5)                      CI0067
                                      VALUE '-----'.                    CI0067
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0067
       01               S-CL01-SSA.                                     CI0067
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0067
                                      VALUE 'CL01    '.                 CI0067
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0067
            10          S-CL01-CCOD   PICTURE X(5)                      CI0067
                                      VALUE '-----'.                    CI0067
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0067
       01            S-CLU01-SSA.                                       CI0067
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'CL01    '.                 CI0067
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0067
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(CL01K'.                   CI0067
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0067
            10       S-CLU01-CL01K.                                     CI0067
            11       S-CLU01-C199.                                      CI0067
            12       S-CLU01-CLID.                                      CI0067
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0067
            13       S-CLU01-CLIDN.                                     CI0067
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0067
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0067
            10  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01               S-CL03-SSA.                                     CI0067
            10         S1-CL03-SEGNAM PICTURE X(8)                      CI0067
                                      VALUE 'CL03    '.                 CI0067
            10         S1-CL03-CCOM   PICTURE X VALUE '*'.              CI0067
            10          S-CL03-CCOD   PICTURE X(5)                      CI0067
                                      VALUE '-----'.                    CI0067
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0067
       01            S-CLA03-SSA.                                       CI0067
            10      S1-CLA03-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'CL03    '.                 CI0067
            10      S1-CLA03-CCOM   PICTURE X VALUE '*'.                CI0067
            10       S-CLA03-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            10      S1-CLA03-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(CLDOD'.                   CI0067
            10       S-CLA03-OPER  PICTURE XX VALUE ' ='.               CI0067
            10       S-CLA03-CLDOD    PICTURE  9(8).                    CI0067
            10  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01               S-CT01-SSA.                                     CI0067
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0067
                                      VALUE 'CT01    '.                 CI0067
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0067
            10          S-CT01-CCOD   PICTURE X(5)                      CI0067
                                      VALUE '-----'.                    CI0067
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0067
       01            S-CTU01-SSA.                                       CI0067
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'CT01    '.                 CI0067
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0067
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(CT01K'.                   CI0067
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0067
            10       S-CTU01-CT01K.                                     CI0067
            11       S-CTU01-C299.                                      CI0067
            12       S-CTU01-CTID.                                      CI0067
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0067
            13       S-CTU01-CTIDN.                                     CI0067
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0067
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0067
            10  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01               S-CT07-SSA.                                     CI0067
            10         S1-CT07-SEGNAM PICTURE X(8)                      CI0067
                                      VALUE 'CT07    '.                 CI0067
            10         S1-CT07-CCOM   PICTURE X VALUE '*'.              CI0067
            10          S-CT07-CCOD   PICTURE X(5)                      CI0067
                                      VALUE '-----'.                    CI0067
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0067
       01            S-CTU07-SSA.                                       CI0067
            10      S1-CTU07-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'CT07    '.                 CI0067
            10      S1-CTU07-CCOM   PICTURE X VALUE '*'.                CI0067
            10       S-CTU07-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            10      S1-CTU07-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(CT07K'.                   CI0067
            10       S-CTU07-OPER  PICTURE XX VALUE ' ='.               CI0067
            10       S-CTU07-CT07K.                                     CI0067
            11       S-CTU07-C199.                                      CI0067
            12       S-CTU07-CLID.                                      CI0067
            13       S-CTU07-CLIDO    PICTURE  9(3).                    CI0067
            13       S-CTU07-CLIDN.                                     CI0067
            14       S-CTU07-CLIDNP   PICTURE  X(12).                   CI0067
            14       S-CTU07-CLIDND   PICTURE  9(8).                    CI0067
            10  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01               S-CT09-SSA.                                     CI0067
            10         S1-CT09-SEGNAM PICTURE X(8)                      CI0067
                                      VALUE 'CT09    '.                 CI0067
            10         S1-CT09-CCOM   PICTURE X VALUE '*'.              CI0067
            10          S-CT09-CCOD   PICTURE X(5)                      CI0067
                                      VALUE '-----'.                    CI0067
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0067
       01            S-CTA09-SSA.                                       CI0067
            11      S1-CTA09-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'CT09    '.                 CI0067
            11      S1-CTA09-CCOM   PICTURE X VALUE '*'.                CI0067
            11       S-CTA09-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            11      S1-CTA09-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(GERED'.                   CI0067
            11       S-CTA09-OPER  PICTURE XX VALUE ' ='.               CI0067
            11       S-CTA09-GERED    PICTURE  9(8).                    CI0067
            11  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01            S-CTB09-SSA.                                       CI0067
            11      S1-CTB09-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'CT09    '.                 CI0067
            11      S1-CTB09-CCOM   PICTURE X VALUE '*'.                CI0067
            11       S-CTB09-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            11      S1-CTB09-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(GECSQ'.                   CI0067
            11       S-CTB09-OPER  PICTURE XX VALUE ' ='.               CI0067
            11       S-CTB09-GECSQ    PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            11  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01            S-CTU09-SSA.                                       CI0067
            11      S1-CTU09-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'CT09    '.                 CI0067
            11      S1-CTU09-CCOM   PICTURE X VALUE '*'.                CI0067
            11       S-CTU09-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            11      S1-CTU09-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(CT09K'.                   CI0067
            11       S-CTU09-OPER  PICTURE XX VALUE ' ='.               CI0067
            11       S-CTU09-CT09K.                                     CI0067
            12       S-CTU09-CLCTRC   PICTURE  9(3).                    CI0067
            11  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01               S-SS01-SSA.                                     CI0067
            10         S1-SS01-SEGNAM PICTURE X(8)                      CI0067
                                      VALUE 'SS01    '.                 CI0067
            10         S1-SS01-CCOM   PICTURE X VALUE '*'.              CI0067
            10          S-SS01-CCOD   PICTURE X(5)                      CI0067
                                      VALUE '-----'.                    CI0067
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0067
       01            S-SSA01-SSA.                                       CI0067
            11      S1-SSA01-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'SS01    '.                 CI0067
            11      S1-SSA01-CCOM   PICTURE X VALUE '*'.                CI0067
            11       S-SSA01-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            11      S1-SSA01-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(NCUSP'.                   CI0067
            11       S-SSA01-OPER  PICTURE XX VALUE ' ='.               CI0067
            11       S-SSA01-NCUSP    PICTURE  X(06).                   CI0067
            11  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01            S-SSB01-SSA.                                       CI0067
            11      S1-SSB01-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'SS01    '.                 CI0067
            11      S1-SSB01-CCOM   PICTURE X VALUE '*'.                CI0067
            11       S-SSB01-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            11      S1-SSB01-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(CCUSP'.                   CI0067
            11       S-SSB01-OPER  PICTURE XX VALUE ' ='.               CI0067
            11       S-SSB01-CCUSP    PICTURE  XX.                      CI0067
            11  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01            S-SSC01-SSA.                                       CI0067
            11      S1-SSC01-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'SS01    '.                 CI0067
            11      S1-SSC01-CCOM   PICTURE X VALUE '*'.                CI0067
            11       S-SSC01-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            11      S1-SSC01-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(GECKD'.                   CI0067
            11       S-SSC01-OPER  PICTURE XX VALUE ' ='.               CI0067
            11       S-SSC01-GECKD    PICTURE  9.                       CI0067
            11  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01            S-SSD01-SSA.                                       CI0067
            11      S1-SSD01-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'SS01    '.                 CI0067
            11      S1-SSD01-CCOM   PICTURE X VALUE '*'.                CI0067
            11       S-SSD01-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            11      S1-SSD01-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(UCUSP'.                   CI0067
            11       S-SSD01-OPER  PICTURE XX VALUE ' ='.               CI0067
            11       S-SSD01-UCUSP    PICTURE  XX.                      CI0067
            11  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01            S-SSE01-SSA.                                       CI0067
            11      S1-SSE01-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'SS01    '.                 CI0067
            11      S1-SSE01-CCOM   PICTURE X VALUE '*'.                CI0067
            11       S-SSE01-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            11      S1-SSE01-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(PRCOD'.                   CI0067
            11       S-SSE01-OPER  PICTURE XX VALUE ' ='.               CI0067
            11       S-SSE01-PRCOD    PICTURE  9(5).                    CI0067
            11  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01            S-SSF01-SSA.                                       CI0067
            11      S1-SSF01-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'SS01    '.                 CI0067
            11      S1-SSF01-CCOM   PICTURE X VALUE '*'.                CI0067
            11       S-SSF01-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            11      S1-SSF01-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(CPRSCN'.                  CI0067
            11       S-SSF01-OPER  PICTURE XX VALUE ' ='.               CI0067
            11       S-SSF01-CPRSCN   PICTURE  9(9).                    CI0067
            11  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01            S-SSG01-SSA.                                       CI0067
            11      S1-SSG01-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'SS01    '.                 CI0067
            11      S1-SSG01-CCOM   PICTURE X VALUE '*'.                CI0067
            11       S-SSG01-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            11      S1-SSG01-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(CTIDA'.                   CI0067
            11       S-SSG01-OPER  PICTURE XX VALUE ' ='.               CI0067
            11       S-SSG01-CTIDA    PICTURE  9(3).                    CI0067
            11  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01            S-SSH01-SSA.                                       CI0067
            11      S1-SSH01-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'SS01    '.                 CI0067
            11      S1-SSH01-CCOM   PICTURE X VALUE '*'.                CI0067
            11       S-SSH01-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            11      S1-SSH01-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(CCSIP'.                   CI0067
            11       S-SSH01-OPER  PICTURE XX VALUE ' ='.               CI0067
            11       S-SSH01-CCSIP    PICTURE  X(09).                   CI0067
            11  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01            S-SSU01-SSA.                                       CI0067
            10      S1-SSU01-SEGNAM PICTURE X(8)                        CI0067
                                      VALUE 'SS01    '.                 CI0067
            10      S1-SSU01-CCOM   PICTURE X VALUE '*'.                CI0067
            10       S-SSU01-CCOD   PICTURE X(5)                        CI0067
                                      VALUE '-----'.                    CI0067
            10      S1-SSU01-FLDNAM PICTURE X(9)                        CI0067
                                      VALUE '(SS01K'.                   CI0067
            10       S-SSU01-OPER  PICTURE XX VALUE ' ='.               CI0067
            10       S-SSU01-SS01K.                                     CI0067
            11       S-SSU01-CTIDA    PICTURE  9(3).                    CI0067
            11       S-SSU01-PRCOD    PICTURE  9(5).                    CI0067
            11       S-SSU01-CPRSCN   PICTURE  9(9).                    CI0067
            10  FILLER   PICTURE X    VALUE ')'.                        CI0067
       01   ZONES-UTILISATEUR PICTURE X.                                CI0067
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
      ** PCB POINTER FOR SSPP                                           ADU015
            05 PCB-SSPP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LM1P                                           ADU015
            05 PCB-LM1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CA1P                                           ADU015
            05 PCB-CA1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CH1P                                           ADU015
            05 PCB-CH1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CCRP                                           ADU015
            05 PCB-CCRP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CPRP                                           ADU015
            05 PCB-CPRP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CBTP                                           ADU015
            05 PCB-CBTP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR ARAY                                           ADU015
            05 PCB-ARAY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AREY                                           ADU015
            05 PCB-AREY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=XA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XA00.                                         CI0067
          05              XA00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00106).                  CI0067
       01                 XA06  REDEFINES      XA00.                    CI0067
            10            XA06-XDBPCB.                                  CI0067
            11            XA06-XDBDNM PICTURE  X(08).                   CI0067
            11            XA06-XSEGLV PICTURE  X(02).                   CI0067
            11            XA06-XRC    PICTURE  X(02).                   CI0067
            11            XA06-XPROPT PICTURE  X(04).                   CI0067
            11            XA06-FILLER PICTURE  S9(5)                    CI0067
                          BINARY.                                       CI0067
            11            XA06-XSEGNM PICTURE  X(08).                   CI0067
            11            XA06-XKEYLN PICTURE  S9(05)                   CI0067
                          BINARY.                                       CI0067
            11            XA06-XSEGNB PICTURE  9(05)                    CI0067
                          BINARY.                                       CI0067
            11            XA06-XCOKEY PICTURE  X(70).                   CI0067
      *** PCB MASK FOR SSPP                                             ADU015
      *!WF DSP=XE DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XE00.                                         CI0067
          05              XE00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00106).                  CI0067
       01                 XE06  REDEFINES      XE00.                    CI0067
            10            XE06-XDBPCB.                                  CI0067
            11            XE06-XDBDNM PICTURE  X(08).                   CI0067
            11            XE06-XSEGLV PICTURE  X(02).                   CI0067
            11            XE06-XRC    PICTURE  X(02).                   CI0067
            11            XE06-XPROPT PICTURE  X(04).                   CI0067
            11            XE06-FILLER PICTURE  S9(5)                    CI0067
                          BINARY.                                       CI0067
            11            XE06-XSEGNM PICTURE  X(08).                   CI0067
            11            XE06-XKEYLN PICTURE  S9(05)                   CI0067
                          BINARY.                                       CI0067
            11            XE06-XSEGNB PICTURE  9(05)                    CI0067
                          BINARY.                                       CI0067
            11            XE06-XCOKEY PICTURE  X(70).                   CI0067
      *** PCB MASK FOR LM1P                                             ADU015
      *!WF DSP=XF DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XF00.                                         CI0067
          05              XF00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00106).                  CI0067
       01                 XF06  REDEFINES      XF00.                    CI0067
            10            XF06-XDBPCB.                                  CI0067
            11            XF06-XDBDNM PICTURE  X(08).                   CI0067
            11            XF06-XSEGLV PICTURE  X(02).                   CI0067
            11            XF06-XRC    PICTURE  X(02).                   CI0067
            11            XF06-XPROPT PICTURE  X(04).                   CI0067
            11            XF06-FILLER PICTURE  S9(5)                    CI0067
                          BINARY.                                       CI0067
            11            XF06-XSEGNM PICTURE  X(08).                   CI0067
            11            XF06-XKEYLN PICTURE  S9(05)                   CI0067
                          BINARY.                                       CI0067
            11            XF06-XSEGNB PICTURE  9(05)                    CI0067
                          BINARY.                                       CI0067
            11            XF06-XCOKEY PICTURE  X(70).                   CI0067
      *** PCB MASK FOR CA1P                                             ADU015
      *!WF DSP=XG DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XG00.                                         CI0067
          05              XG00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00106).                  CI0067
       01                 XG06  REDEFINES      XG00.                    CI0067
            10            XG06-XDBPCB.                                  CI0067
            11            XG06-XDBDNM PICTURE  X(08).                   CI0067
            11            XG06-XSEGLV PICTURE  X(02).                   CI0067
            11            XG06-XRC    PICTURE  X(02).                   CI0067
            11            XG06-XPROPT PICTURE  X(04).                   CI0067
            11            XG06-FILLER PICTURE  S9(5)                    CI0067
                          BINARY.                                       CI0067
            11            XG06-XSEGNM PICTURE  X(08).                   CI0067
            11            XG06-XKEYLN PICTURE  S9(05)                   CI0067
                          BINARY.                                       CI0067
            11            XG06-XSEGNB PICTURE  9(05)                    CI0067
                          BINARY.                                       CI0067
            11            XG06-XCOKEY PICTURE  X(70).                   CI0067
      *** PCB MASK FOR CH1P                                             ADU015
      *!WF DSP=XH DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XH00.                                         CI0067
          05              XH00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00106).                  CI0067
       01                 XH06  REDEFINES      XH00.                    CI0067
            10            XH06-XDBPCB.                                  CI0067
            11            XH06-XDBDNM PICTURE  X(08).                   CI0067
            11            XH06-XSEGLV PICTURE  X(02).                   CI0067
            11            XH06-XRC    PICTURE  X(02).                   CI0067
            11            XH06-XPROPT PICTURE  X(04).                   CI0067
            11            XH06-FILLER PICTURE  S9(5)                    CI0067
                          BINARY.                                       CI0067
            11            XH06-XSEGNM PICTURE  X(08).                   CI0067
            11            XH06-XKEYLN PICTURE  S9(05)                   CI0067
                          BINARY.                                       CI0067
            11            XH06-XSEGNB PICTURE  9(05)                    CI0067
                          BINARY.                                       CI0067
            11            XH06-XCOKEY PICTURE  X(70).                   CI0067
      *** PCB MASK FOR CCRP                                             ADU015
      *!WF DSP=XI DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XI00.                                         CI0067
          05              XI00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00106).                  CI0067
       01                 XI06  REDEFINES      XI00.                    CI0067
            10            XI06-XDBPCB.                                  CI0067
            11            XI06-XDBDNM PICTURE  X(08).                   CI0067
            11            XI06-XSEGLV PICTURE  X(02).                   CI0067
            11            XI06-XRC    PICTURE  X(02).                   CI0067
            11            XI06-XPROPT PICTURE  X(04).                   CI0067
            11            XI06-FILLER PICTURE  S9(5)                    CI0067
                          BINARY.                                       CI0067
            11            XI06-XSEGNM PICTURE  X(08).                   CI0067
            11            XI06-XKEYLN PICTURE  S9(05)                   CI0067
                          BINARY.                                       CI0067
            11            XI06-XSEGNB PICTURE  9(05)                    CI0067
                          BINARY.                                       CI0067
            11            XI06-XCOKEY PICTURE  X(70).                   CI0067
      *** PCB MASK FOR CPRP                                             ADU015
      *!WF DSP=XJ DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XJ00.                                         CI0067
          05              XJ00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00106).                  CI0067
       01                 XJ06  REDEFINES      XJ00.                    CI0067
            10            XJ06-XDBPCB.                                  CI0067
            11            XJ06-XDBDNM PICTURE  X(08).                   CI0067
            11            XJ06-XSEGLV PICTURE  X(02).                   CI0067
            11            XJ06-XRC    PICTURE  X(02).                   CI0067
            11            XJ06-XPROPT PICTURE  X(04).                   CI0067
            11            XJ06-FILLER PICTURE  S9(5)                    CI0067
                          BINARY.                                       CI0067
            11            XJ06-XSEGNM PICTURE  X(08).                   CI0067
            11            XJ06-XKEYLN PICTURE  S9(05)                   CI0067
                          BINARY.                                       CI0067
            11            XJ06-XSEGNB PICTURE  9(05)                    CI0067
                          BINARY.                                       CI0067
            11            XJ06-XCOKEY PICTURE  X(70).                   CI0067
      *** PCB MASK FOR CBTP                                             ADU015
      *!WF DSP=XK DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XK00.                                         CI0067
          05              XK00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00106).                  CI0067
       01                 XK06  REDEFINES      XK00.                    CI0067
            10            XK06-XDBPCB.                                  CI0067
            11            XK06-XDBDNM PICTURE  X(08).                   CI0067
            11            XK06-XSEGLV PICTURE  X(02).                   CI0067
            11            XK06-XRC    PICTURE  X(02).                   CI0067
            11            XK06-XPROPT PICTURE  X(04).                   CI0067
            11            XK06-FILLER PICTURE  S9(5)                    CI0067
                          BINARY.                                       CI0067
            11            XK06-XSEGNM PICTURE  X(08).                   CI0067
            11            XK06-XKEYLN PICTURE  S9(05)                   CI0067
                          BINARY.                                       CI0067
            11            XK06-XSEGNB PICTURE  9(05)                    CI0067
                          BINARY.                                       CI0067
            11            XK06-XCOKEY PICTURE  X(70).                   CI0067
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=XM DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XM00.                                         CI0067
          05              XM00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00106).                  CI0067
       01                 XM06  REDEFINES      XM00.                    CI0067
            10            XM06-XDBPCB.                                  CI0067
            11            XM06-XDBDNM PICTURE  X(08).                   CI0067
            11            XM06-XSEGLV PICTURE  X(02).                   CI0067
            11            XM06-XRC    PICTURE  X(02).                   CI0067
            11            XM06-XPROPT PICTURE  X(04).                   CI0067
            11            XM06-FILLER PICTURE  S9(5)                    CI0067
                          BINARY.                                       CI0067
            11            XM06-XSEGNM PICTURE  X(08).                   CI0067
            11            XM06-XKEYLN PICTURE  S9(05)                   CI0067
                          BINARY.                                       CI0067
            11            XM06-XSEGNB PICTURE  9(05)                    CI0067
                          BINARY.                                       CI0067
            11            XM06-XCOKEY PICTURE  X(70).                   CI0067
      *** PCB MASK FOR ARAY                                             ADU015
      *!WF DSP=XN DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XN00.                                         CI0067
          05              XN00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00106).                  CI0067
       01                 XN06  REDEFINES      XN00.                    CI0067
            10            XN06-XDBPCB.                                  CI0067
            11            XN06-XDBDNM PICTURE  X(08).                   CI0067
            11            XN06-XSEGLV PICTURE  X(02).                   CI0067
            11            XN06-XRC    PICTURE  X(02).                   CI0067
            11            XN06-XPROPT PICTURE  X(04).                   CI0067
            11            XN06-FILLER PICTURE  S9(5)                    CI0067
                          BINARY.                                       CI0067
            11            XN06-XSEGNM PICTURE  X(08).                   CI0067
            11            XN06-XKEYLN PICTURE  S9(05)                   CI0067
                          BINARY.                                       CI0067
            11            XN06-XSEGNB PICTURE  9(05)                    CI0067
                          BINARY.                                       CI0067
            11            XN06-XCOKEY PICTURE  X(70).                   CI0067
      *** PCB MASK FOR AREY                                             ADU015
      *!WF DSP=XO DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XO00.                                         CI0067
          05              XO00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00106).                  CI0067
       01                 XO06  REDEFINES      XO00.                    CI0067
            10            XO06-XDBPCB.                                  CI0067
            11            XO06-XDBDNM PICTURE  X(08).                   CI0067
            11            XO06-XSEGLV PICTURE  X(02).                   CI0067
            11            XO06-XRC    PICTURE  X(02).                   CI0067
            11            XO06-XPROPT PICTURE  X(04).                   CI0067
            11            XO06-FILLER PICTURE  S9(5)                    CI0067
                          BINARY.                                       CI0067
            11            XO06-XSEGNM PICTURE  X(08).                   CI0067
            11            XO06-XKEYLN PICTURE  S9(05)                   CI0067
                          BINARY.                                       CI0067
            11            XO06-XSEGNB PICTURE  9(05)                    CI0067
                          BINARY.                                       CI0067
            11            XO06-XCOKEY PICTURE  X(70).                   CI0067
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=XP DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XP00.                                         CI0067
          05              XP00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00106).                  CI0067
       01                 XP06  REDEFINES      XP00.                    CI0067
            10            XP06-XDBPCB.                                  CI0067
            11            XP06-XDBDNM PICTURE  X(08).                   CI0067
            11            XP06-XSEGLV PICTURE  X(02).                   CI0067
            11            XP06-XRC    PICTURE  X(02).                   CI0067
            11            XP06-XPROPT PICTURE  X(04).                   CI0067
            11            XP06-FILLER PICTURE  S9(5)                    CI0067
                          BINARY.                                       CI0067
            11            XP06-XSEGNM PICTURE  X(08).                   CI0067
            11            XP06-XKEYLN PICTURE  S9(05)                   CI0067
                          BINARY.                                       CI0067
            11            XP06-XSEGNB PICTURE  9(05)                    CI0067
                          BINARY.                                       CI0067
            11            XP06-XCOKEY PICTURE  X(70).                   CI0067

      *PASS AREA TO/FROM CL0067
      *!WF DSP=WZ DSL=DU SEL=67 FOR=I DES=1 LEV=1 PLT=10
       01                 WZ67.                                         CI0067
            10            WZ67-CTID01 PICTURE  X(27).                   CI0067
            10            WZ67-C299.                                    CI0067
            11            WZ67-CTID.                                    CI0067
            12            WZ67-CTIDA  PICTURE  9(3).                    CI0067
            12            WZ67-CTIDN.                                   CI0067
            13            WZ67-CTIDNP PICTURE  X(13).                   CI0067
            13            WZ67-CTIDND PICTURE  9(11).                   CI0067
            10            WZ67-IDELI  PICTURE  X.                       CI0067
            10            WZ67-CDEL1  PICTURE  9(3).                    CI0067
            10            WZ67-NDELS  PICTURE  S9(3)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-IUGMA  PICTURE  X.                       CI0067
            10            WZ67-DCACG  PICTURE  9(8).                    CI0067
            10            WZ67-AACTVD PICTURE  S9(11)V99.               CI0067
            10            WZ67-AACTV  PICTURE  S9(11)V99                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-AFAVP  PICTURE  S9(4)V9(3)               CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-INMRC  PICTURE  X(01).                   CI0067
            10            WZ67-IARLNA PICTURE  X.                       CI0067
            10            WZ67-CELBL  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-AMINAL PICTURE  S9(7)V99.                CI0067
            10            WZ67-AMAXAL PICTURE  S9(7)V99.                CI0067
            10            WZ67-IARRGA PICTURE  X.                       CI0067
            10            WZ67-AMIND  PICTURE  S9(7)V99.                CI0067
            10            WZ67-AMAXAR PICTURE  S9(7)V99.                CI0067
            10            WZ67-IARPSA PICTURE  X.                       CI0067
            10            WZ67-CELBA  PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-AMAXA  PICTURE  S9(7)V99.                CI0067
            10            WZ67-QSHOWQ PICTURE  S9(9)V999                CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-QSHOW  PICTURE  S9(10)V999               CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-PACT1  PICTURE  S999V999                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-PPOT1  PICTURE  S9(3)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-QMTH   PICTURE  9(3).                    CI0067
            10            WZ67-QMTH1  PICTURE  9(3).                    CI0067
            10            WZ67-ALDDUE PICTURE  9(08).                   CI0067
            10            WZ67-ALPAGM PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-ALPAGQ PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-ALPAGS PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-ALPAGR PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-IFQQT  PICTURE  X.                       CI0067
            10            WZ67-IFQSA  PICTURE  X.                       CI0067
            10            WZ67-AINPU  PICTURE  S9(9)V99.                CI0067
            10            WZ67-QFOSPD PICTURE  9(2).                    CI0067
            10            WZ67-MAPPN  PICTURE  X(10).                   CI0067
            10            WZ67-CHCR   PICTURE  99.                      CI0067
            10            WZ67-IFDAB  PICTURE  X.                       CI0067
            10            WZ67-IFDAG  PICTURE  X.                       CI0067
            10            WZ67-IFDAP  PICTURE  X.                       CI0067
            10            WZ67-IFDAS  PICTURE  X(01).                   CI0067
            10            WZ67-ALPMOD PICTURE  S9(7)V99                 CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-CEIT   PICTURE  9(3).                    CI0067
            10            WZ67-QSHIS  PICTURE  S9(10)V999               CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-QSHES  PICTURE  S9(10)V999               CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            WZ67-CIRAP  PICTURE  XX.                      CI0067
            10            WZ67-CLDOB  PICTURE  9(8).                    CI0067
            10            WZ67-IARCDA PICTURE  X.                       CI0067
            10            WZ67-FILLER PICTURE  X(38).                   CI0067

      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0067
          05              DE00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00653).                  CI0067
       01                 DE10  REDEFINES      DE00.                    CI0067
            10            DE10-DU11.                                    CI0067
            11            DE10-XFONC  PICTURE  X(4).                    CI0067
            11            DE10-MPSBN  PICTURE  X(8).                    CI0067
            11            DE10-XDBDNM PICTURE  X(08).                   CI0067
            11            DE10-XSEGNM PICTURE  X(08).                   CI0067
            11            DE10-XRC    PICTURE  X(02).                   CI0067
            11            DE10-MSEG   PICTURE  X(08).                   CI0067
            11            DE10-XCOKEY PICTURE  X(70).                   CI0067
            11            DE10-CUIBR  PICTURE  X(01).                   CI0067
            11            DE10-CUIBA  PICTURE  X(01).                   CI0067
            11            DE10-IPBIK  PICTURE  X(1).                    CI0067
            10            DE10-DU03.                                    CI0067
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            DE10-CMSSF  PICTURE  XX.                      CI0067
            11            DE10-DU09.                                    CI0067
            12            DE10-CMESA  PICTURE  S9(9)                    CI0067
                          BINARY.                                       CI0067
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0067
                          BINARY.                                       CI0067
            12            DE10-CMESB  PICTURE  S9(9)                    CI0067
                          BINARY.                                       CI0067
            12            DE10-CMSST  PICTURE  S9(9)                    CI0067
                          BINARY.                                       CI0067
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0067
                          BINARY.                                       CI0067
            12            DE10-QELLAA PICTURE  S9(9)                    CI0067
                          BINARY.                                       CI0067
            12            DE10-TMESS4 PICTURE  X(512).                  CI0067
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
       01                 MS00.                                         CI0067
          05              MS00-SUITE.                                   CI0067
            15       FILLER         PICTURE  X(00542).                  CI0067
       01                 MS03  REDEFINES      MS00.                    CI0067
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            10            MS03-CMSSF  PICTURE  XX.                      CI0067
            10            MS03-DU09.                                    CI0067
            11            MS03-CMESA  PICTURE  S9(9)                    CI0067
                          BINARY.                                       CI0067
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0067
                          BINARY.                                       CI0067
            11            MS03-CMESB  PICTURE  S9(9)                    CI0067
                          BINARY.                                       CI0067
            11            MS03-CMSST  PICTURE  S9(9)                    CI0067
                          BINARY.                                       CI0067
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0067
                          BINARY.                                       CI0067
            11            MS03-QELLAA PICTURE  S9(9)                    CI0067
                          BINARY.                                       CI0067
            11            MS03-TMESS4 PICTURE  X(512).                  CI0067
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0067
            10            MX11-QMSGS  PICTURE  9(03).                   CI0067
            10            MX11-PJ09                                     CI0067
                          OCCURS       025     TIMES.                   CI0067
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0067
                          COMPUTATIONAL-3.                              CI0067
            11            MX11-CMESB  PICTURE  S9(9)                    CI0067
                          BINARY.                                       CI0067
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                WZ67
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0067
      *               *                                   *             CI0067
      *               *INITIALISATIONS                    *             CI0067
      *               *                                   *             CI0067
      *               *************************************.            CI0067
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
      *N02CA.    NOTE *ALIGN WITH PCBs PASSED             *.
       F02CA.                                                           lv10
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF XA06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR SSPP                                             DOT
           SET ADDRESS OF XE06 TO                                       ADU015
                PCB-SSPP-PTR1.                                          ADU015
      *SET ADDRESS FOR LM1P                                             DOT
           SET ADDRESS OF XF06 TO                                       ADU015
                PCB-LM1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CA1P                                             DOT
           SET ADDRESS OF XG06 TO                                       ADU015
                PCB-CA1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CH1P                                             DOT
           SET ADDRESS OF XH06 TO                                       ADU015
                PCB-CH1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CCRP                                             DOT
           SET ADDRESS OF XI06 TO                                       ADU015
                PCB-CCRP-PTR1.                                          ADU015
      *SET ADDRESS FOR CPRP                                             DOT
           SET ADDRESS OF XJ06 TO                                       ADU015
                PCB-CPRP-PTR1.                                          ADU015
      *SET ADDRESS FOR CBTP                                             DOT
           SET ADDRESS OF XK06 TO                                       ADU015
                PCB-CBTP-PTR1.                                          ADU015
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF XM06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR ARAY                                             DOT
           SET ADDRESS OF XN06 TO                                       ADU015
                PCB-ARAY-PTR1.                                          ADU015
      *SET ADDRESS FOR AREY                                             DOT
           SET ADDRESS OF XO06 TO                                       ADU015
                PCB-AREY-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF XP06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
       F02CA-FN. EXIT.
      *N02MA.    NOTE *INIT RETURNED FIELDS               *.
       F02MA.                                                           lv10
           MOVE        9999999.99 TO WZ67-AMINAL
           MOVE        9999999.99 TO WZ67-AMIND
           MOVE        999999999.99 TO WZ67-AINPU
           MOVE        ZEROS TO WZ67-AMAXA
           MOVE        ZEROS TO WZ67-AMAXAL
           MOVE        ZEROS TO WZ67-AMAXAR
           MOVE        ZEROS TO WZ67-AACTVD
           MOVE        ZEROS TO WZ67-AACTV
           MOVE        ZEROS TO WZ67-AFAVP
           MOVE        ZEROS TO WZ67-QSHOW
           MOVE        ZEROS TO WZ67-QSHOWQ
           MOVE        ZEROS TO WZ67-PACT1
           MOVE        ZEROS TO WZ67-PPOT1
           MOVE        ZEROS TO WZ67-QMTH
           MOVE        ZEROS TO WZ67-QMTH1
           MOVE        ZEROS TO WZ67-ALPAGM
           MOVE        ZEROS TO WZ67-ALPAGQ
           MOVE        ZEROS TO WZ67-ALPAGS
           MOVE        ZEROS TO WZ67-ALPAGR
           MOVE        ZEROS TO WZ67-ALDDUE
           MOVE        ZEROS TO WZ67-CELBA
           MOVE        ZEROS TO WZ67-CELBL
           MOVE        ZEROS TO WZ67-QFOSPD
           MOVE        ZEROS TO WZ67-ALPMOD
           MOVE        'N' TO WZ67-INMRC
           MOVE        'N' TO WZ67-IARLNA
           MOVE        'N' TO WZ67-IARRGA
           MOVE        'N' TO WZ67-IARPSA
           MOVE        'N' TO WZ67-IARCDA
           MOVE        'Y' TO WZ67-IFQQT
           MOVE        'Y' TO WZ67-IFQSA
           MOVE        'N' TO WZ67-IFDAB
           WZ67-IFDAG
           WZ67-IFDAP
           WZ67-IFDAS
           MOVE        ZERO TO WZ67-CEIT
           MOVE        ZERO TO WZ67-QSHIS
           MOVE        ZERO TO WZ67-QSHES
           MOVE        ZERO TO WZ67-CLDOB.
       F02MA-FN. EXIT.
      *N02PA.    NOTE ***** SEGMENT INITIALIZATION ****   *.
       F02PA.                                                           lv10
      *SHARK PROD/SUBPROD DB ROOT
           INITIALIZE  SS01.
       F02PA-FN. EXIT.
      *N02PB.    NOTE *PERFORM CICS ASSIGN TO GET         *.
       F02PB.                                                           lv10
      *LOGIN ID
      *
           EXEC CICS   ASSIGN USERID (WS00-NCUSR2)           END-EXEC.
       F02PB-FN. EXIT.
      *N02RA.    NOTE *FIRST TIME IN FOR "FROM" ACCOUNT   *.
       F02RA.    IF    WZ67-CTID01 NOT = SV00-CTID                      lv10
                 NEXT SENTENCE ELSE GO TO     F02RA-FN.
      *OR SPECIFIC "TO" ACCT REQUESTED
      *********************************
           MOVE        'Y' TO WG00-FIRST-TIME
           MOVE        WZ67-CTID01 TO SV00-CTID.
       F02RA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0067
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0067
      *               *                                   *             CI0067
      *               *FIN DE TRAITEMENT                  *             CI0067
      *               *                                   *             CI0067
      *               *************************************.            CI0067
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0067
      *N20BB.    NOTE *RESET FIRST TIME IN FLAG           *.
       F20BB.                                                           lv10
           MOVE        'N' TO WG00-FIRST-TIME.
       F20BB-FN. EXIT.
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               **** VALIDATE THE INPUT PARMS ***   *
      *               *                                   *
      *               *************************************.
       F30.           EXIT.                                             lv05
      *N30CA.    NOTE *IF APPLICATION RECOGNIZED          *.
       F30CA.    IF    WZ67-MAPPN = 'SD'                                lv10
                 OR    WZ67-MAPPN = 'UD'
                 OR    WZ67-MAPPN = 'FDC'
                 NEXT SENTENCE ELSE GO TO     F30CA-FN.
       F30CA-900. GO TO F30CG-FN.
       F30CA-FN. EXIT.
      *N30CG.    NOTE *ELSE.. DEFAULT TO ORIGINAL (SD)    *.
       F30CG.                                                           lv10
           MOVE        'SD        ' TO WZ67-MAPPN.
       F30CG-FN. EXIT.
      *N30CK.    NOTE *HOW RECEIVED MUST BE NUMERIC AND   *.
       F30CK.    IF    WZ67-CHCR NUMERIC                                lv10
                 AND   (WZ67-CHCR = 02 OR 03)
                 NEXT SENTENCE ELSE GO TO     F30CK-FN.
      *WRITTEN OR PHONE
       F30CK-900. GO TO F30CL-FN.
       F30CK-FN. EXIT.
      *N30CL.    NOTE *INVALID HOW RECEIVED               *.
       F30CL.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012053 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CL-FN. EXIT.
      *N30DA.    NOTE *SOURCE ACCOUNT                     *.
       F30DA.    IF    WZ67-CTID01 NOT NUMERIC                          lv10
                 OR    WZ67-CTID01 NOT > ZERO
                 NEXT SENTENCE ELSE GO TO     F30DA-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012114 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DA-FN. EXIT.
      *N30EA.    NOTE *DESTINATION ACCOUNT                *.
       F30EA.    IF    WZ67-CTID NUMERIC                                lv10
                 NEXT SENTENCE ELSE GO TO     F30EA-FN.
      *N30ED.    NOTE *TRANSFER: DEST IS AN AEFA ACCT     *.
       F30ED.    IF    WZ67-CTID > ZERO                                 lv15
                 NEXT SENTENCE ELSE GO TO     F30ED-FN.
           MOVE        02 TO W-WORK-CPITC.
       F30ED-900. GO TO F30EG-FN.
       F30ED-FN. EXIT.
      *N30EG.    NOTE *NOT A TRANSFER: DEST IS A PAYEE    *.
       F30EG.                                                           lv15
           MOVE        01 TO W-WORK-CPITC.
       F30EG-FN. EXIT.
       F30EA-900. GO TO F30EM-FN.
       F30EA-FN. EXIT.
      *N30EM.    NOTE *INVALID DEST ACCT                  *.
       F30EM.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012115 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30EM-FN. EXIT.
      *N30HA.    NOTE *NOT A TRANSFER                     *.
       F30HA.    IF    PAYOUT-TRANSACTION                               lv10
                 NEXT SENTENCE ELSE GO TO     F30HA-FN.
      *N30IA.    NOTE *DELIVERY INSTR DEFAULT INDICATOR   *.
       F30IA.    IF    WZ67-IDELI NOT = 'Y'                             lv15
                 AND   WZ67-IDELI NOT = 'N'
                 NEXT SENTENCE ELSE GO TO     F30IA-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012795 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30IA-FN. EXIT.
      *N30JA.    NOTE *DELIV INSTR MEDIA TYPE             *.
       F30JA.    IF    WZ67-CDEL1 = 001                                 lv15
                 OR    WZ67-CDEL1 = 002
                 OR    WZ67-CDEL1 = 003
                 OR    (WZ67-CDEL1 = 000
                 AND   WZ67-IDELI = 'Y')
                 NEXT SENTENCE ELSE GO TO     F30JA-FN.
       F30JA-900. GO TO F30JB-FN.
       F30JA-FN. EXIT.
      *N30JB.    NOTE *INVALID DELIV INSTR MEDIA TYPE     *.
       F30JB.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012796 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30JB-FN. EXIT.
       F30HA-900. GO TO F30MA-FN.
       F30HA-FN. EXIT.
      *N30MA.    NOTE *TRANSFER                           *.
       F30MA.         EXIT.                                             lv10
      *N30MD.    NOTE *UGMA INDICATOR                     *.
       F30MD.    IF    WZ67-IUGMA NOT = 'Y'                             lv15
                 AND   WZ67-IUGMA NOT = 'N'
                 NEXT SENTENCE ELSE GO TO     F30MD-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012798 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30MD-FN. EXIT.
       F30MA-FN. EXIT.
      *N30TA.    NOTE *** ENSURE ACCTG DATE IS VALID **   *.
       F30TA.    IF    WZ67-DCACG > ZERO                                lv10
                 NEXT SENTENCE ELSE GO TO     F30TA-FN.
           MOVE        WZ67-DCACG TO DD01-XDATG
           PERFORM     F93DD THRU F93DD-FN.
      *N30TD.    NOTE *** IF DATE INVALID; ERROR OUT **   *.
       F30TD.    IF    DEL-ER NOT = 1                                   lv15
                 NEXT SENTENCE ELSE GO TO     F30TD-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012786 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30TD-FN. EXIT.
       F30TA-FN. EXIT.
       F30-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               ***** GET 'FROM' ACCOUNT INFO ***   *
      *               *                                   *
      *               *************************************.
       F35.      IF    FIRST-TIME-IN                                    lv05
                 NEXT SENTENCE ELSE GO TO     F35-FN.
      *N35BA.    NOTE *GU ON CT01                         *.
       F35BA.                                                           lv10
           MOVE        WZ67-CTID01 TO S-CTU01-CTID
           PERFORM     F94CT THRU F94CT-FN.
      *N35BD.    NOTE *CT01 FOUND                         *.
       F35BD.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F35BD-FN.
           MOVE        CT01 TO FR01.
                 IF    FR01-CTIDA = 002                                 DOT
                 AND   FR01-PRSCD = SPACES
      *FUND ACCT W/ NO SUB-PROD
           MOVE        '000000001' TO FR01-PRSCD.
       F35BD-900. GO TO F35BM-FN.
       F35BD-FN. EXIT.
      *N35BM.    NOTE *CT01 NOT FOUND                     *.
       F35BM.                                                           lv15
      *********************************
           MOVE        012077 TO WS00-NMESS2
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012077 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35BM-FN. EXIT.
      *N35BP.    NOTE *GET CLDOB FOR TAXPAYER             *.
       F35BP.                                                           lv15
      *GN CT09 FOR TAXPAYER ROLE
           PERFORM     F94CV THRU F94CV-FN.
                 IF    IK = '1'                                         DOT
      *CT09 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012792 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N35CA.    NOTE *READ CL01 FOR TAXPAYER             *.
       F35CA.                                                           lv20
      *PULL TAXPAYER CLID FROM CTO7 KEY
           MOVE        XW05-XCOKEY (28:23) TO
           7-TAXPAYER-CLID
      *GU CL01
           MOVE        7-TAXPAYER-CLID TO S-CLU01-CL01K
           PERFORM     F94BB THRU F94BB-FN.
                 IF    IK = '1'                                         DOT
      *CL01 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012788 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N35CH.    NOTE *PERSON CLIENT                      *.
       F35CH.    IF    CL01-CLTYP = 'P'                                 lv25
                 NEXT SENTENCE ELSE GO TO     F35CH-FN.
      *GN CL03
           PERFORM     F94BD THRU F94BD-FN.
                 IF    IK = '0'                                         DOT
      *CL03-FOUND
           MOVE        CL03-CLDOB TO WZ67-CLDOB.
       F35CH-FN. EXIT.
       F35CA-FN. EXIT.
       F35BP-FN. EXIT.
       F35BA-FN. EXIT.
      *N35GA.    NOTE **** IF SOURCE = CERTIFICATE ***    *.
       F35GA.    IF    FR01-CTIDA = 001                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35GA-FN.
      *CALL CI0135
           MOVE        FR01-CTID TO PC01-CTID
           PERFORM     F92DA THRU F92DA-FN.
      *N35GG.    NOTE *MOVE CERT VALUES TO DU67           *.
       F35GG.                                                           lv15
           MOVE        PJ02-ACCTV8 TO WZ67-AACTVD
           WS00-AACTVD
           MOVE        PJ02-CEIT TO WZ67-CEIT
           WS00-CEIT.
       F35GG-FN. EXIT.
       F35GA-FN. EXIT.
      *N35HA.    NOTE ***** IF SOURCE = MUTUAL FUND ***   *.
       F35HA.    IF    FR01-CTIDA = 002                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35HA-FN.
      *N35HJ.    NOTE *GU ON SS01                         *.
       F35HJ.                                                           lv15
      *(SHARK PROD/SUBPROD DATABASE)
           MOVE        FR01-CTIDA TO S-SSU01-CTIDA
           MOVE        FR01-PRCOD TO S-SSU01-PRCOD
           MOVE        FR01-PRSCD TO S-SSU01-CPRSCN
           PERFORM     F94SS THRU F94SS-FN.
      *N35HL.    NOTE *SS01 FOUND                         *.
       F35HL.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F35HL-FN.
      *MOVE SOURCE ACCOUNT DIVIDEND
      *DISTRIBUTION FREQ TO PASS SEGM
           MOVE        SS01-QFOSPD TO WZ67-QFOSPD
           WS00-QFOSPD.
       F35HL-900. GO TO F35HM-FN.
       F35HL-FN. EXIT.
      *N35HM.    NOTE *SS01 NOT FOUND                     *.
       F35HM.                                                           lv20
      *********************************
           MOVE        012018 TO WS00-NMESS2
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012018 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35HM-FN. EXIT.
       F35HJ-FN. EXIT.
      *N35IA.    NOTE *CALL THE ACCOUNT VIEW              *.
       F35IA.                                                           lv15
           MOVE        SPACES TO SQ1A
           SQ5A
      *POPULATE THE REQUEST AREA OF THE
      *ACCOUNT VIEW - SQ1A
           MOVE        001 TO SQ1A-NRERO
           MOVE        001 TO SQ5A-NRURO
           MOVE        FR01-CTID TO SQ1A-CTID (1)
           MOVE        FR01-PRCOD TO SQ1A-PRCOD (1)
           MOVE        FR01-PRSCD TO SQ1A-CPRSCN (1).
      *N35IC.    NOTE *POPULATE THE VALUES TO THE INPUT   *.
       F35IC.                                                           lv20
      *LINKAGE SEGMENT OF THE BROKER
      ********************************
           MOVE        SPACES TO WL00-RESPONSE                          AADBI5
           WL00-REQUEST                                                 AADBI5
           INITIALIZE  SQ1L SQ2L SQ3L                                   AADBI5
           MOVE        001 TO SQ1L-NPVERH                               AADBI5
           MOVE        001 TO SQ1L-NPVERC                               AADBI5
           MOVE        001 TO SQ1L-NPVERD                               AADBI5
           MOVE        LENGTH OF SQ1A TO                                AADBI5
           SQ1L-GELLP1                                                  AADBI5
           MOVE        LENGTH OF SQ5A TO                                AADBI5
           SQ1L-GELLP2                                                  AADBI5
           MOVE        '2933' TO SQ1L-NVIEW                             AADBI5
           MOVE        WS00-NCUSR2 TO SQ1L-NCUSR2                       AADBI5
           MOVE        'U' TO SQ1L-CPRT2                                AADBI5
           MOVE        PROGE TO SQ1L-MRPIB1                             AADBI5
           MOVE        'N' TO SQ1L-CLOGY                                AADBI5
           MOVE        ZERO TO SQ1L-QTOUT                               AADBI5
           MOVE        'S' TO SQ1L-CVSIZ                                AADBI5
           MOVE        SQ1A TO WL00-REQUEST                             AADBI5
      *CALL TO IP SOCKET BROKER
           PERFORM     F95BA THRU F95BA-FN.
       F35IC-FN. EXIT.
       F35IA-FN. EXIT.
      *N35IH.    NOTE *SOCKET BROKER CALL ERROR           *.
       F35IH.                                                           lv15
      *PROCESSING
           MOVE        LENGTH OF SQ5A-GRFIX TO
           WS-FIX-GELL
           MOVE        LENGTH OF SQ5A-GRVAR (1) TO
           WS-VAR-GELL
           MOVE        'L' TO WS00-ERROR-TYPE
           MOVE        '20059' TO WE00-CERRE1 (1)
           MOVE        '20009' TO WE00-CERRE1 (2)
           MOVE        2 TO IWE00L
           PERFORM     F96BC THRU F96BC-FN.
      *N35IM.    NOTE *ACCOUNT VIEW CALL - SUCCESSFUL     *.
       F35IM.    IF    WS00-IERRC = SPACES                              lv20
                 NEXT SENTENCE ELSE GO TO     F35IM-FN.
           MOVE        WL00-RESPONSE TO SQ5A.
       F35IM-900. GO TO F35IR-FN.
       F35IM-FN. EXIT.
      *N35IR.    NOTE *IN CASE OF ANY ERROR FROM DST      *.
       F35IR.                                                           lv20
      *********************************
                 IF    (CT01-CTSTA = 01                                 DOT
                 OR    CT01-CTSTA = 03)
                 AND   (WL00-RESPONSE (41:5) =
                       '10004'
                 OR    WL00-RESPONSE (41:5) =
                       '10005'
                 OR    WL00-RESPONSE (41:5) =
                       '10006')
      *IF MASTER RECORD NOT FOUND
      *FOR PENDING OR INACTIVE ACCOUNT
      *********************************
           INITIALIZE  MS03
           SQ5A-GRVAR (1)
           SQ5A-GRFIX
                 ELSE
      *********************************
      *ANY DST ERROR OTHER THAN '10005'
      *'10004','10006'POPULATE MS03 AND
      *GOBACK TO CALLING PROGRAM
      *********************************
           MOVE        MS03 TO WS03
           MOVE        ZEROES TO WZ67-QSHES
           MOVE                     ALL '1' TO FT GO TO F20.
       F35IR-FN. EXIT.
      *N35JC.    NOTE *MOVE SQ5A FIELDS                   *.
       F35JC.                                                           lv20
           MOVE        SQ5A-QSHES4 (1) TO WZ67-QSHES.
       F35JC-FN. EXIT.
       F35IH-FN. EXIT.
      *N35QA.    NOTE *INITIALISE THE REQUEST & RESPONS   *.
       F35QA.                                                           lv15
      *AREA FOR ACCOUNT VALUE VIEW.
      *********************************
           MOVE        SPACES TO SQ1E
           SQ5E
           MOVE        001 TO SQ1E-NRERO
           MOVE        001 TO SQ5E-NRURO
           INITIALIZE  WE00-ERROR-TABLE.
      *N35QC.    NOTE *LOAD THE SEGMENT TO CALL           *.
       F35QC.                                                           lv20
      *DST'S CALCULATION VIEW
      *************************
      *LOAD:
      *   ACCOUNT NUMBER
      *   PRODUCT CODE
      *   SUBPRODUCT CODE
      *   AS OF DATE
      *************************
           MOVE        FR01-CTID TO SQ1E-CTID (1)
           MOVE        FR01-PRCOD TO SQ1E-PRCOD (1)
           MOVE        FR01-PRSCD TO SQ1E-CPRSCN (1).
       F35QC-FN. EXIT.
      *N35QE.    NOTE *LOAD THE VALUES TO THE INPUT       *.
       F35QE.                                                           lv20
      *LINKAGE SEGMENT OF THE BROKER
      ********************************
           MOVE        SPACES TO WL00-RESPONSE                          AADBI5
           WL00-REQUEST                                                 AADBI5
           INITIALIZE  SQ1L SQ2L SQ3L                                   AADBI5
           MOVE        001 TO SQ1L-NPVERH                               AADBI5
           MOVE        001 TO SQ1L-NPVERC                               AADBI5
           MOVE        001 TO SQ1L-NPVERD                               AADBI5
           MOVE        LENGTH OF SQ1E TO                                AADBI5
           SQ1L-GELLP1                                                  AADBI5
           MOVE        LENGTH OF SQ5E TO                                AADBI5
           SQ1L-GELLP2                                                  AADBI5
           MOVE        '2949' TO SQ1L-NVIEW                             AADBI5
           MOVE        WS00-NCUSR2 TO SQ1L-NCUSR2                       AADBI5
           MOVE        'U' TO SQ1L-CPRT2                                AADBI5
           MOVE        PROGE TO SQ1L-MRPIB1                             AADBI5
           MOVE        'N' TO SQ1L-CLOGY                                AADBI5
           MOVE        ZERO TO SQ1L-QTOUT                               AADBI5
           MOVE        'M' TO SQ1L-CVSIZ                                AADBI5
           MOVE        SQ1E TO WL00-REQUEST                             AADBI5
      *CALL TO IP SOCKET BROKER
           PERFORM     F95BA THRU F95BA-FN
      *CHECK FOR ERRORS
           MOVE        LENGTH OF SQ5E-GRFIX TO
           WS-FIX-GELL
           MOVE        LENGTH OF SQ5E-GRVAR (1) TO
           WS-VAR-GELL
           PERFORM     F96BC THRU F96BC-FN.
      *N35QR.    NOTE *GET THE RESPONSE FROM THE VIEW     *.
       F35QR.    IF    WS00-IERRC = SPACES                              lv25
                 NEXT SENTENCE ELSE GO TO     F35QR-FN.
      *SUCCESSFUL CALL
           MOVE        WL00-RESPONSE TO SQ5E.
       F35QR-900. GO TO F35QY-FN.
       F35QR-FN. EXIT.
      *N35QY.    NOTE *IF ACCOUNT NOT FOUND AND           *.
       F35QY.                                                           lv25
                 IF    (CT01-CTSTA = 01                                 DOT
                 OR    CT01-CTSTA = 03)
                 AND   (WL00-RESPONSE (31:5) =
                       '10005'
                 OR    WL00-RESPONSE (31:5) =
                       '10004')
      *THE ACCOUNT IS EITHER PENDING
      *OR INACTIVE THEN CONTINUE
      *********************************
           INITIALIZE  MS03
           SQ5E-GRFIX
           SQ5E-GRVAR (1)
                 ELSE
      *ANY OTHER ERROR GOBACK
           MOVE        MS03 TO WS03
           MOVE                     ALL '1' TO FT GO TO F20.
       F35QY-FN. EXIT.
      *N35RT.    NOTE **** IF ACCOUNT VALUE EXISTS  ***   *.
       F35RT.    IF    SQ5E-AACCD2 (1) > ZERO                           lv25
                 NEXT SENTENCE ELSE GO TO     F35RT-FN.
      *VALUE OF OUTSTANDING CERT SHRS
      *AND LOI ESCROW SHRS
           COMPUTE     WZ67-AACTV ROUNDED =
           (WZ67-QSHIS + WZ67-QSHES) *
           SQ5E-AFAV10 (1)
           MOVE        WZ67-AACTV TO WS00-AACTV
      *ACCT VALUE
           MOVE        SQ5E-AACCD2 (1) TO WZ67-AACTVD
           WS00-AACTVD.
       F35RT-900. GO TO F35SH-FN.
       F35RT-FN. EXIT.
      *N35SH.    NOTE **** ELSE... NO ACCOUNT VALUE ***   *.
       F35SH.                                                           lv25
           MOVE        9999999.99 TO WZ67-AACTVD
           WS00-AACTVD.
       F35SH-FN. EXIT.
      *N35SR.    NOTE *SET NEW MONEY CASH INDICATOR       *.
       F35SR.                                                           lv25
           COMPUTE     WK00-ACOMO = SQ5A-QSHOM2 (1) *
           SQ5E-AFAV10 (1).
                 IF    WK00-ACOMO = SQ5A-ACINEV (1)                     DOT
      *IF OLD MONEY CASH AMOUNT =
      *       CASH INVESTED AMOUNT
           MOVE        'N' TO WZ67-INMRC
           WS00-INMRC
                 ELSE
      * ELSE NEW MONEY DOES EXIST
           MOVE        'Y' TO WZ67-INMRC
           WS00-INMRC
           COMPUTE     WK00-ACNMO = SQ5A-ACINEV (1) -
           WK00-ACOMO
           MOVE        WK00-ACNMO TO WS00-ACNMO.
       F35SR-FN. EXIT.
       F35QE-FN. EXIT.
       F35QA-FN. EXIT.
      *N35UG.    NOTE *FUND ASSET VALUE PRICE             *.
       F35UG.                                                           lv15
                 IF    SQ5E-AFAV10 (1) = ZERO                           DOT
      *IF PRICE IS 0, SET TO 1
           MOVE        1.000 TO SQ5E-AFAV10 (1).
           MOVE        SQ5E-AFAV10 (1) TO WZ67-AFAVP.                   DOT
       F35UG-FN. EXIT.
       F35HA-FN. EXIT.
      *N35VA.    NOTE ***** IF SOURCE = BROKERAGE ***     *.
       F35VA.    IF    FR01-CTIDA = 021                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35VA-FN.
      *N35VB.    NOTE *DEFAULT BROKERAGE VALUE            *.
       F35VB.                                                           lv15
           MOVE        9999999.99 TO WZ67-AACTVD.
       F35VB-FN. EXIT.
       F35VA-FN. EXIT.
       F35-FN.   EXIT.
      *N37.      NOTE *************************************.
      *               *                                   *
      *               ***** GET 'FROM' ACCOUNT INFO ***   *
      *               *                                   *
      *               *************************************.
       F37.      IF    NOT FIRST-TIME-IN                                lv05
                 NEXT SENTENCE ELSE GO TO     F37-FN.
      *USE THE INFORMATION SAVED DURING
      *THE FIRST CALL.
      *N37CB.    NOTE *CHECK IF ANY ERROR OCCURED FOR     *.
       F37CB.    IF    WS00-NMESS2 NOT = ZERO                           lv10
                 NEXT SENTENCE ELSE GO TO     F37CB-FN.
      *THE FROM ACCOUNT DETAILS IN THE
      *FIRST CALL TO THIS MODULE
      *********************************
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        WS00-NMESS2 TO MS03-NMESS2                       ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F37CB-FN. EXIT.
      *N37GL.    NOTE **** IF SOURCE = CERTIFICATE ***    *.
       F37GL.    IF    FR01-CTIDA = 001                                 lv10
                 NEXT SENTENCE ELSE GO TO     F37GL-FN.
      *POPULATE THE VALUES FOR THE
      *CERTIFICATE ACCOUNT
      *********************************
           MOVE        WS00-AACTVD TO WZ67-AACTVD
           PJ02-ACCTV8
           MOVE        WS00-CEIT TO PJ02-CEIT
           WZ67-CEIT.
       F37GL-FN. EXIT.
      *N37TB.    NOTE ***** IF SOURCE = MUTUAL FUND ***   *.
       F37TB.    IF    FR01-CTIDA = 002                                 lv10
                 NEXT SENTENCE ELSE GO TO     F37TB-FN.
      *POPULATE THE VALUES FROM THE
      *FIRST CALL TO THIS MODULE
      *********************************
           MOVE        WS00-QFOSPD TO WZ67-QFOSPD.
                 IF    WS03-NMESS2 = ZEROS                              DOT
      *********************************
      *IF ACCOUNT VALUE VIEW CALL WAS
      *SUCCESSFUL POPULATE THE DETAILS
      *********************************
           MOVE        WS00-AACTV TO WZ67-AACTV
           MOVE        WS00-AACTVD TO WZ67-AACTVD
           MOVE        WS00-INMRC TO WZ67-INMRC
           MOVE        WS00-ACNMO TO WK00-ACOMO
      *********************************
                 ELSE
      *********************************
      *GO BACK TO CALLING PROGRAM WITH
      *ERROR MESSAGE
           MOVE        ZEROES TO WZ67-QSHES
           MOVE        WS03 TO MS03
           MOVE                     ALL '1' TO FT GO TO F20.
      *N37UG.    NOTE *FUND ASSET VALUE PRICE             *.
       F37UG.                                                           lv15
                 IF    SQ5E-AFAV10 (1) = ZERO                           DOT
      *IF PRICE IS 0, SET TO 1
           MOVE        1.000 TO SQ5E-AFAV10 (1).
           MOVE        SQ5E-AFAV10 (1) TO WZ67-AFAVP.                   DOT
       F37UG-FN. EXIT.
       F37TB-FN. EXIT.
      *N37VA.    NOTE ***** IF SOURCE = BROKERAGE ***     *.
       F37VA.    IF    FR01-CTIDA = 021                                 lv10
                 NEXT SENTENCE ELSE GO TO     F37VA-FN.
      *N37VB.    NOTE *DEFAULT BROKERAGE VALUE            *.
       F37VB.                                                           lv15
           MOVE        9999999.99 TO WZ67-AACTVD.
       F37VB-FN. EXIT.
       F37VA-FN. EXIT.
       F37-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *DEST ACCT ID (TRANSFER)            *
      *               *                                   *
      *               *************************************.
       F40.      IF    WZ67-CTID > ZERO                                 lv05
                 NEXT SENTENCE ELSE GO TO     F40-FN.
      *N40BB.    NOTE *GU ON CT01                         *.
       F40BB.                                                           lv10
           MOVE        WZ67-CTID TO S-CTU01-CTID
           PERFORM     F94CT THRU F94CT-FN.
      *N40BG.    NOTE *CT01 FOUND                         *.
       F40BG.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F40BG-FN.
           MOVE        CT01 TO TO01.
                 IF    TO01-CTIDA = 002                                 DOT
                 AND   TO01-PRSCD = SPACES
      *FUND ACCT W/ NO SUB-PROD
           MOVE        '000000001' TO TO01-PRSCD.
       F40BG-900. GO TO F40BJ-FN.
       F40BG-FN. EXIT.
      *N40BJ.    NOTE *CT01 NOT FOUND                     *.
       F40BJ.         EXIT.                                             lv15
       F40BJ-FN. EXIT.
       F40BB-FN. EXIT.
      *N40CA.    NOTE *IRA; SET CONTRIBUTION LIMITS       *.
       F40CA.    IF    TO01-CQACT = 001                                 lv10
                 NEXT SENTENCE ELSE GO TO     F40CA-FN.
           INITIALIZE  K910.
      *N40CC.    NOTE *EZ TRANS UD OR SD                  *.
       F40CC.    IF    WZ67-MAPPN = 'UD' OR 'SD'                        lv15
                 NEXT SENTENCE ELSE GO TO     F40CC-FN.
      *N40CD.    NOTE *IF IRA TYPE IS SEP OR SRA,         *.
       F40CD.    IF    TO01-CIRAT = 003 OR 004                          lv20
                 NEXT SENTENCE ELSE GO TO     F40CD-FN.
               GO TO     F40CA-FN.
       F40CD-FN. EXIT.
       F40CC-FN. EXIT.
      *N40CF.    NOTE *GET CLDOB FOR TAXPAYER             *.
       F40CF.                                                           lv15
      *GN CT09 FOR TAXPAYER ROLE
           PERFORM     F94CV THRU F94CV-FN.
                 IF    IK = '1'                                         DOT
      *CT09 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012792 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N40CG.    NOTE *READ CL01 FOR TAXPAYER             *.
       F40CG.                                                           lv20
      *PULL TAXPAYER CLID FROM CT07 KEY
           MOVE        XW05-XCOKEY (28:23) TO
           7-TAXPAYER-CLID
      *GU CL01
           MOVE        7-TAXPAYER-CLID TO S-CLU01-CL01K
           PERFORM     F94BB THRU F94BB-FN.
                 IF    IK = '1'                                         DOT
      *CL01 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012788 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N40CH.    NOTE *PERSON CLIENT                      *.
       F40CH.    IF    CL01-CLTYP = 'P'                                 lv25
                 NEXT SENTENCE ELSE GO TO     F40CH-FN.
      *GN CL03
           PERFORM     F94BD THRU F94BD-FN.
                 IF    IK = '0'                                         DOT
      *CL03 FOUND
           MOVE        CL03-CLDOB TO WZ67-CLDOB.
       F40CH-FN. EXIT.
       F40CG-FN. EXIT.
       F40CF-FN. EXIT.
      *N40CK.    NOTE *CALL CI0400                        *.
       F40CK.    IF    FR01-CQACT = 000                                 lv15
                 NEXT SENTENCE ELSE GO TO     F40CK-FN.
           MOVE        WZ67-DCACG (1:4) TO K910-DIRAYR
           MOVE        TO01-CQACT TO K910-CQACT
           MOVE        TO01-CIRAT TO K910-CIRAT
           MOVE        TO01-CIRAS TO K910-CIRAS
           MOVE        WZ67-CIRAP TO K910-CIRAP
           MOVE        WZ67-CLDOB TO K910-CLDOB.
      *CALL CI0400                                                      DOT
           PERFORM     F98BB THRU F98BB-FN
           MOVE        K910-ACLIM TO WZ67-AMAXA.
       F40CK-FN. EXIT.
       F40CA-FN. EXIT.
      *N40FA.    NOTE *CALL CI0022 - BA/BILLING EDITS     *.            AM0022
       F40FA.                                                           lv10
      *********************************                                 AM0022
      ** THIS MODULE WILL ACCESS THE  *                                 AM0022
      ** ARRANGEMENT DATABASE AND ITS *                                 AM0022
      ** SECONDARY INDEXES, AS WELL AS*                                 AM0022
      ** CALL A FUNDS BILLING MODULE. *                                 AM0022
      **                              *                                 AM0022
      ** IT SETS VARIOUS INDICATORS   *                                 AM0022
      ** TELLING WHETHER AN ACCOUNT   *                                 AM0022
      ** HAS SCHEDULED MONEY GOING    *                                 AM0022
      ** INTO OR OUT OF IT.           *                                 AM0022
      *********************************                                 AM0022
           INITIALIZE  BE26                                             AM0022
           MOVE        WZ67-CTID TO BE26-CTID                           AM0022
           MOVE        WZ67-DCACG TO BE26-DCACG                         AM0022
           MOVE        WZ67-MAPPN TO BE26-MAPPN                         AM0022
           SET CI0022F-PCB-ARAY-PTR1 TO                                 AM0022
                       PCB-ARAY-PTR1                                    AM0022
           SET CI0022F-PCB-AR1P-PTR1 TO                                 AM0022
                       PCB-AR1P-PTR1                                    AM0022
           SET CI0022F-PCB-AREY-PTR1 TO                                 AM0022
                       PCB-AREY-PTR1                                    AM0022
           INITIALIZE  DE10-DU03                                        AM0022
           CALL        CI0022 USING                                     AM0022
           DFHEIBLK                                                     AM0022
           DFHCOMMAREA                                                  AM0022
           DLIUIBII                                                     AM0022
           CI0022F-PCB-ADDRESS-LIST                                     AM0022
           BE26                                                         AM0022
           DE10                                                         AM0022
           MS03                                                         AM0022
           MX11.                                                        AM0022
      *N40FD.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F40FD.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F40FD-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0022 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0022 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40FD-900. GO TO F40FK-FN.
       F40FD-FN. EXIT.
      *N40FK.    NOTE *NO ERRORS                          *.            ADU071
       F40FK.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
      *N40FN.    NOTE *STORE DEST FUNDING SWITCHES        *.
       F40FN.                                                           lv20
           MOVE        BE26-IFDAB TO WZ67-IFDAB
           MOVE        BE26-IFDAG TO WZ67-IFDAG
           MOVE        BE26-IFDAP TO WZ67-IFDAP
           MOVE        BE26-IFDAS TO WZ67-IFDAS.
       F40FN-FN. EXIT.
       F40FK-FN. EXIT.
       F40FA-FN. EXIT.
      *N40GA.    NOTE *CALL CI0021 - PAYMENT EDITS        *.            AM0021
       F40GA.                                                           lv10
      *********************************                                 AM0021
      *FOR DEST ACCT PASSED IN,                                         AM0021
      *SETS INDICATORS FOR TYPES OF                                     AM0021
      *PAYMENTS ALLOWED.                                                AM0021
      *PE22-IARRGA - REG PMTS ALLOWED                                   AM0021
      *PE22-IARLNA - LOAN REPMTS ALLWD                                  AM0021
      *PE22-INPAY  - NEW PMTS ALLOWED                                   AM0021
      *                                                                 AM0021
      *ALSO SETS MIN & MAX PMT AMTS FOR                                 AM0021
      *EACH OF THE 3 TYPES OF PMTS.                                     AM0021
      *                                                                 AM0021
      *ALSO RETURNS 2 LOAN BALANCE AMTS                                 AM0021
      *********************************                                 AM0021
           MOVE        TO01-CTID TO PE22-CTID                           AM0021
           MOVE        TO01-PRCOD TO PE22-PRCOD                         AM0021
           MOVE        TO01-CTSTA TO PE22-CTSTA                         AM0021
           MOVE        WZ67-MAPPN TO PE22-MAPPN                         AM0021
           SET CI0021G-PCB-CA1P-PTR1 TO                                 AM0021
                       PCB-CA1P-PTR1                                    AM0021
           SET CI0021G-PCB-LM1P-PTR1 TO                                 AM0021
                       PCB-LM1P-PTR1                                    AM0021
           CALL        CI0021 USING                                     AM0021
           DFHEIBLK                                                     AM0021
           DFHCOMMAREA                                                  AM0021
           DLIUIBII                                                     AM0021
           CI0021G-PCB-ADDRESS-LIST                                     AM0021
           PE22                                                         AM0021
           DE10                                                         AM0021
           MS03                                                         AM0021
           MX11.                                                        AM0021
      *N40GC.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F40GC.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F40GC-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0021 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0021 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40GC-900. GO TO F40GE-FN.
       F40GC-FN. EXIT.
      *N40GE.    NOTE *NO ERRORS                          *.            ADU071
       F40GE.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
      *N40GH.    NOTE *MOVE MIN & MAX FIELDS FOR SD       *.
       F40GH.    IF    WZ67-MAPPN = 'SD'                                lv20
                 NEXT SENTENCE ELSE GO TO     F40GH-FN.
      *LOAN FIELDS
           MOVE        PE22-IARLNA TO WZ67-IARLNA
           MOVE        PE22-CELBL TO WZ67-CELBL
           MOVE        PE22-AMINAL TO WZ67-AMINAL
           MOVE        PE22-AMAXAL TO WZ67-AMAXAL.
                 IF    PE22-INPAY = 'Y'                                 DOT
      *INITIAL PAYMENT
           MOVE        PE22-INPAY TO WZ67-IARRGA
           MOVE        PE22-AMINAN TO WZ67-AMIND
           WZ67-AINPU
           MOVE        PE22-AMAXAN TO WZ67-AMAXAR
                 ELSE
      *REG PAYMENT FIELDS
           MOVE        PE22-IARRGA TO WZ67-IARRGA
           MOVE        PE22-AMIND TO WZ67-AMIND
           WZ67-AINPU
           MOVE        PE22-AMAXAR TO WZ67-AMAXAR.
      *PARTIAL SURRENDER FIELDS(?)                                      DOT
           MOVE        PE22-IARPSA TO WZ67-IARPSA
           MOVE        PE22-CELBA TO WZ67-CELBA.
       F40GH-FN. EXIT.
      *N40GL.    NOTE *MOVE MIN & MAX FIELDS FOR UD,FDC   *.
       F40GL.    IF    WZ67-MAPPN = 'UD' OR 'FDC'                       lv20
                 NEXT SENTENCE ELSE GO TO     F40GL-FN.
      *LOAN FIELDS
           MOVE        PE22-IARLNA TO WZ67-IARLNA
           MOVE        PE22-CELBL TO WZ67-CELBL
           MOVE        PE22-AMINAL TO WZ67-AMINAL
           MOVE        PE22-AMAXAL TO WZ67-AMAXAL.
      *N40GM.    NOTE *INITIAL PAYMENT                    *.
       F40GM.    IF    PE22-INPAY = 'Y'                                 lv25
                 NEXT SENTENCE ELSE GO TO     F40GM-FN.
           MOVE        PE22-INPAY TO WZ67-IARRGA
           MOVE        PE22-AMINAN TO WZ67-AMIND
           WZ67-AINPU
           MOVE        PE22-AMAXAN TO WZ67-AMAXAR.
       F40GM-900. GO TO F40GT-FN.
       F40GM-FN. EXIT.
      *N40GT.    NOTE *ELSE.. USE ADD-ON DEFAULTS         *.
       F40GT.                                                           lv25
           MOVE        PE22-IARRGA TO WZ67-IARRGA
           MOVE        PE22-AMIND TO WZ67-AMIND
           WZ67-AINPU
           MOVE        PE22-AMAXAR TO WZ67-AMAXAR.
       F40GT-FN. EXIT.
       F40GL-FN. EXIT.
      *N40KB.    NOTE *CASH DEPOSIT PAYMENT FIELD ONLY    *.
       F40KB.    IF    WZ67-MAPPN = ('UD' OR 'SD')                      lv20
                 AND   WZ67-CTID (1:3) = '133'
                 NEXT SENTENCE ELSE GO TO     F40KB-FN.
      *APPLY TO EZTRANS UD AND SD TRANS
      *IF DESTINATION IS A BETA BROK
      *ACCOUNT
           MOVE        'Y' TO WZ67-IARCDA.
       F40KB-FN. EXIT.
       F40GE-FN. EXIT.
       F40GA-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *DEST IS CERTIFICATE ACCOUNT        *
      *               *                                   *
      *               *************************************.
       F45.      IF    TO01-CTIDA = 001                                 lv05
                 NEXT SENTENCE ELSE GO TO     F45-FN.
      *OVERRIDE MINIMUMS WHEN NEEDED
      *N45BA.    NOTE *SD                                 *.
       F45BA.    IF    WZ67-MAPPN = 'SD'                                lv10
                 NEXT SENTENCE ELSE GO TO     F45BA-FN.
      *N45BC.    NOTE *MARKET STRATEGY CERT               *.
       F45BC.    IF    FR01-CTIDA = 001                                 lv15
                 AND   (FR01-PRCOD = 00181
                 OR    FR01-PRCOD = 00961)
                 NEXT SENTENCE ELSE GO TO     F45BC-FN.
      *(ONLY INTRA-ACCOUNT TRANSFERS
      *ALLOWED, SO IF SOURCE IS A MKT
      *STRATEGY, SO IS DEST)
      *SET ADD-ON MIN TO 50 DOLLARS
           MOVE        50.00 TO WZ67-AMIND
      *SET INITIAL MIN TO 1000 DOLLARS
           MOVE        1000.00 TO WZ67-AINPU.
       F45BC-900. GO TO F45BN-FN.
       F45BC-FN. EXIT.
      *N45BN.    NOTE *ALL OTHER CERTS                    *.
       F45BN.                                                           lv15
           MOVE        50.00 TO WZ67-AMIND
           WZ67-AINPU.
       F45BN-FN. EXIT.
       F45BA-FN. EXIT.
      *N45DA.    NOTE *UD OR FDC                          *.
       F45DA.    IF    WZ67-MAPPN = 'UD' OR 'FDC'                       lv10
                 NEXT SENTENCE ELSE GO TO     F45DA-FN.
      *N45DB.    NOTE *INITIAL PAYMENT                    *.
       F45DB.    IF    PE22-INPAY = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F45DB-FN.
      *N45DF.    NOTE *OVERRIDE FOR PREF INV (RET) CERT   *.
       F45DF.    IF    TO01-PRCOD = 00250                               lv20
                 OR    TO01-PRCOD = 00950
                 NEXT SENTENCE ELSE GO TO     F45DF-FN.
      *PRODUCT HAS MIN AMT NEW =
      *$100,000 ON TABLE TA0096,
      *SO HERE ITS CHANGED
      *TO A 1/4 MILLION
           MOVE        +250000.00 TO WZ67-AMIND
           WZ67-AINPU.
       F45DF-FN. EXIT.
      *N45DH.    NOTE *CASH RESERVE VARIABLE PAYMENT      *.
       F45DH.    IF    (TO01-PRCOD = 00662 OR                           lv20
                       TO01-PRCOD = 00972)
                 AND   (BE26-IFDAB = 'Y' OR
                       BE26-IFDAG = 'Y' OR
                       BE26-IFDAP = 'Y' OR
                       BE26-IFDAS = 'Y')
                 NEXT SENTENCE ELSE GO TO     F45DH-FN.
      *3 MONTH TERM CERT
      *SCHEDULED MONEY COMING IN
      *OVERRIDE $1000 MIN FROM TA96
           MOVE        50.00 TO WZ67-AMIND
           WZ67-AINPU.
       F45DH-FN. EXIT.
      *N45EA.    NOTE *IF FDC; SPECIAL EDITS              *.
       F45EA.    IF    WZ67-MAPPN = 'FDC'                               lv20
                 NEXT SENTENCE ELSE GO TO     F45EA-FN.
      *N45EC.    NOTE *IF FUND FEEDING A CERT;            *.
       F45EC.    IF    FR01-CTIDA = 002                                 lv25
                 AND   (TO01-PRCOD = 00165
                 OR    TO01-PRCOD = 00971)
                 NEXT SENTENCE ELSE GO TO     F45EC-FN.
      * - CALL CI0135 TO GET TERM
           MOVE        TO01-CTID TO PC01-CTID
           PERFORM     F92DA THRU F92DA-FN.
      *N45EH.    NOTE *IF 7 MONTH TERM; SET FDC LIMITS    *.
       F45EH.    IF    PJ02-CEIT = 007                                  lv30
                 NEXT SENTENCE ELSE GO TO     F45EH-FN.
      *N45EJ.    NOTE *IF NEW MONEY EXCEEDS 250000        *.
       F45EJ.    IF    WK00-ACNMO > 250000                              lv35
                 NEXT SENTENCE ELSE GO TO     F45EJ-FN.
           MOVE        250000 TO WZ67-AMAXAR.
       F45EJ-900. GO TO F45EL-FN.
       F45EJ-FN. EXIT.
      *N45EL.    NOTE *ELSE... USE NEW MONEY FOR MAX      *.
       F45EL.                                                           lv35
                 IF    WK00-ACNMO > 999.99                              DOT
      *IF ENOUGH NEW MONEY FOR PURCHASE
           MOVE        WK00-ACNMO TO WZ67-AMAXAR
                 ELSE
      *ELSE.. NOT ENOUGH $ FOR MINIMUM
           MOVE        'N' TO WZ67-IARRGA
           MOVE        9999999 TO WZ67-AMIND
           MOVE        9999999 TO WZ67-AINPU
           MOVE        ZERO TO WZ67-AMAXAR.
       F45EL-FN. EXIT.
       F45EH-FN. EXIT.
       F45EC-FN. EXIT.
       F45EA-FN. EXIT.
       F45DB-FN. EXIT.
       F45DA-FN. EXIT.
      *N45ZA.    NOTE *FOR FUND SOURCE                    *.
       F45ZA.                                                           lv10
      *COMPUTE: MIN/MAX SHARES
      *         MIN/MAX %
      *         MIN/MAX NBR OF MONTHS
           PERFORM     F91BA THRU F91BA-FN.
       F45ZA-FN. EXIT.
      *N45ZZ.    NOTE *END OF PROCESSING FOR CERT DEST    *.
       F45ZZ.                                                           lv10
           MOVE                     ALL '1' TO FT GO TO F20.
       F45ZZ-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *DEST IS MUTUAL FUND ACCOUNT        *
      *               *                                   *
      *               *************************************.
       F50.      IF    TO01-CTIDA = 002                                 lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *OVERRIDE MINIMUMS WHEN NEEDED
      *N50DC.    NOTE *SD                                 *.
       F50DC.    IF    WZ67-MAPPN = 'SD'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50DC-FN.
      *IN CI0021, THE HARD-CODED MIN
      *AMT FOR ADD-ON PMTS (PE22-AMIND)
      *IS 100.00
      *N50DE.    NOTE *OVERRIDE MIN FOR QUAL ACCTS        *.
       F50DE.    IF    TO01-CQACT > ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F50DE-FN.
           MOVE        50.00 TO WZ67-AMIND
           WZ67-AINPU
      *
                 IF    (TO01-PRCOD = 00107                              DOT
                 OR    00124 OR 00125 OR 00126)
                 AND   (TO01-CTSTA = 01 OR 03)
      *INFLATION PROTECTED SEC FUND
      *FOR CONTRARIAN EQUITY FUND
      *US EQUITY FUND
      *AND THREADNEEDLE GLOBAL EXTENDED
      *ALPHA FUND
           MOVE        10000.00 TO WZ67-AMIND.
       F50DE-FN. EXIT.
       F50DC-FN. EXIT.
      *N50HA.    NOTE *UD OR FDC                          *.
       F50HA.    IF    WZ67-MAPPN = 'UD' OR 'FDC'                       lv10
                 NEXT SENTENCE ELSE GO TO     F50HA-FN.
      *N50HC.    NOTE *QUALIFIED ACCOUNT                  *.
       F50HC.    IF    TO01-CQACT > ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F50HC-FN.
      *"TRULY ON ANY QUALIFIED ACCOUNT
      *THERE IS NO MINIMUM. ARRANGE-
      *MENTS HAVE A MINIMUM OF $50/PMT.
      *BUT UNSCHEDULED PURCHASES CAN BE
      *ANY AMT." (T.L., 1/12/99)
           MOVE        ZERO TO WZ67-AMIND
           WZ67-AINPU.
                 IF    (TO01-PRCOD = 00102                              DOT
                       OR 00106 OR
                       00108)
                 AND   (TO01-CTSTA = 01 OR 03)
      *INFLATION PROTECTED SEC FUND
           MOVE        5000.00 TO WZ67-AMIND.
                 IF    (TO01-PRCOD = 00107                              DOT
                 OR    00124 OR 00125 OR 00126)
                 AND   (TO01-CTSTA = 01 OR 03)
      *INFLATION PROTECTED SEC FUND
      *FOR CONTRARIAN EQUITY FUND
      *US EQUITY FUND
      *AND THREADNEEDLE GLOBAL EXTEN
      *ALPHA FUND
           MOVE        10000.00 TO WZ67-AMIND.
               GO TO     F50HA-FN.                                      DOT
       F50HC-FN. EXIT.
      *N50HF.    NOTE *INITIAL PAYMENT                    *.
       F50HF.    IF    PE22-INPAY = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50HF-FN.
      *IN CI0021, THE HARD-CODED MIN
      *AMT FOR INIT PMT (PE22-AMINAN)
      *IS 2000.00
      *N50HJ.    NOTE *CASH MGMT OR TFM OR RVS GMM        *.
       F50HJ.    IF    (TO01-PRCOD = 13 OR 16                           lv20
                 OR    167)
                 AND   TO01-CTNLI = 'N'
                 NEXT SENTENCE ELSE GO TO     F50HJ-FN.
      * - TAX FREE AND CASH MANAGEMENT
      *   SHOULD ALWAYS DEFAULT TO
      *   NEW MINIMUM EVEN IF INCOMING
      *   MONEY
               GO TO     F50HF-FN.
       F50HJ-FN. EXIT.
      *N50HM.    NOTE *SCHEDULED MONEY COMING IN          *.
       F50HM.    IF    BE26-IFDAB = 'Y'                                 lv20
                 OR    BE26-IFDAG = 'Y'
                 OR    BE26-IFDAP = 'Y'
                 OR    BE26-IFDAS = 'Y'
                 OR    TO01-CTNLI = 'Y'
                 NEXT SENTENCE ELSE GO TO     F50HM-FN.
      *OR ASSET VALUE ACCOUNT
      *USE ADD-ON MINIMUMS
           MOVE        PE22-AMIND TO WZ67-AMIND
           WZ67-AINPU.
       F50HM-FN. EXIT.
       F50HF-FN. EXIT.
       F50HA-FN. EXIT.
      *N50ZA.    NOTE *FOR FUND SOURCE                    *.
       F50ZA.                                                           lv10
      *COMPUTE: MIN/MAX SHARES
      *         MIN/MAX %
      *         MIN/MAX NBR OF MONTHS
           PERFORM     F91BA THRU F91BA-FN.
       F50ZA-FN. EXIT.
      *N50ZB.    NOTE *IF SOURCE IS CASH MGMNT AND        *.
       F50ZB.    IF    ((((FR01-PRCOD = 00013                           lv10
                 OR    FR01-PRCOD = 00167)
                 AND   FR01-PRSCD = '000000001')
                 OR    FR01-PRCOD = 00016
                 OR    FR01-CTIDA = 001)
                 AND   TO01-PRSCD = '000000002')
                 NEXT SENTENCE ELSE GO TO     F50ZB-FN.
      *CLASS A OR RVS GOVT MONEY MKT FD
      *CLASS A OR IF SOURCE IS
      *TAX FREE MONEY OR
      *CERTIFICATE ACCOUNT
      *AND TO ACCOUNT IS CLASS B
           MOVE        49999.99 TO WZ67-AMAXAR.
       F50ZB-FN. EXIT.
      *N50ZZ.    NOTE *END OF PROCESSING FOR FUND DEST    *.
       F50ZZ.                                                           lv10
           MOVE                     ALL '1' TO FT GO TO F20.
       F50ZZ-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *DEST IS IDSL OR IDSL-NY ACCT       *
      *               *                                   *
      *               *************************************.
       F55.      IF    TO01-CTIDA = 004                                 lv05
                 OR    TO01-CTIDA = 005
                 NEXT SENTENCE ELSE GO TO     F55-FN.
      *N55BA.    NOTE *REGULAR ALLOWED                    *.
       F55BA.    IF    WZ67-IARRGA = 'Y'                                lv10
                 NEXT SENTENCE ELSE GO TO     F55BA-FN.
      *SET MAX
           MOVE        9999999.99 TO WZ67-AMAXAR.
      *N55DA.    NOTE ******* IF MINIMUM TOO LOW ******   *.
       F55DA.    IF    WZ67-AMIND = ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F55DA-FN.
      *(POSSIBLY NO ENTRY IN TA96 TBL)
      *N55DD.    NOTE *SD                                 *.
       F55DD.    IF    WZ67-MAPPN = 'SD'                                lv20
                 NEXT SENTENCE ELSE GO TO     F55DD-FN.
           MOVE        50.00 TO WZ67-AMIND
           WZ67-AINPU.
       F55DD-FN. EXIT.
      *N55DH.    NOTE *UD, FDC                            *.
       F55DH.    IF    WZ67-MAPPN = 'UD' OR 'FDC'                       lv20
                 NEXT SENTENCE ELSE GO TO     F55DH-FN.
      *N55DK.    NOTE *ANNUITIES                          *.
       F55DK.    IF    TO01-PRCOD > 599                                 lv25
                 NEXT SENTENCE ELSE GO TO     F55DK-FN.
           MOVE        50.00 TO WZ67-AMIND
           WZ67-AINPU.
       F55DK-FN. EXIT.
      *N55DN.    NOTE *UL                                 *.
       F55DN.    IF    TO01-PRCOD > 199                                 lv25
                 AND   TO01-PRCOD < 261
                 NEXT SENTENCE ELSE GO TO     F55DN-FN.
           MOVE        25.00 TO WZ67-AMIND
           WZ67-AINPU.
       F55DN-FN. EXIT.
       F55DH-FN. EXIT.
       F55DA-FN. EXIT.
       F55BA-900. GO TO F55EA-FN.
       F55BA-FN. EXIT.
      *N55EA.    NOTE *REG NOT ALW; MINIMUM IS HIGHEST    *.
       F55EA.                                                           lv10
           MOVE        9999999.99 TO WZ67-AMIND
           WZ67-AINPU
           MOVE        ZERO TO WZ67-AMAXAR.
       F55EA-FN. EXIT.
      *N55HA.    NOTE *LOAN NOT ALW; MINIMUM IS HIGHEST   *.
       F55HA.    IF    WZ67-IARLNA = 'N'                                lv10
                 NEXT SENTENCE ELSE GO TO     F55HA-FN.
           MOVE        9999999.99 TO WZ67-AMINAL
           MOVE        ZERO TO WZ67-AMAXAL.
       F55HA-FN. EXIT.
      *N55JA.    NOTE *STATUS NOT PENDING                 *.
       F55JA.    IF    TO01-CTSTA NOT = 01                              lv10
                 NEXT SENTENCE ELSE GO TO     F55JA-FN.
      *N55JC.    NOTE *READ TABLE TA5B                    *.
       F55JC.                                                           lv15
           MOVE        TO01-CTIDA TO TA5B-CTIDA
           MOVE        TO01-PRCOD TO TA5B-PRCOD
           MOVE        SPACES TO TA5B-PRSCD
           MOVE        '1' TO TA5B-CF
           PERFORM     F92TA THRU F92TA-FN.
       F55JC-FN. EXIT.
      *N55JJ.    NOTE *TA5B ENTRY FOUND &                 *.
       F55JJ.    IF    TA5B-CF = '1'                                    lv15
                 AND   TA5B-IVANT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F55JJ-FN.
      *ACCT IS ON VANTAGE
           MOVE        '0' TO IK
      *ACCESSS VANTAGE BROKER
           PERFORM     F82FA THRU F82FA-FN.
       F55JJ-900. GO TO F55JL-FN.
       F55JJ-FN. EXIT.
      *N55JL.    NOTE *ACCESS NON VANTAGE ACCOUNTS        *.
       F55JL.                                                           lv15
      *(ROOT OF LIFE ONLINE MASTER DB)
           MOVE        TO01-CTIDND TO S-AAU10-ALCIDN
           PERFORM     F94A1 THRU F94A1-FN.
       F55JL-FN. EXIT.
      *N55KA.    NOTE *AA10 NOT FOUND  *OR*               *.
       F55KA.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F55KA-FN.
      *** VANTAGE BROKER FAILED **
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012265 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F55KA-900. GO TO F55KD-FN.
       F55KA-FN. EXIT.
      *N55KD.    NOTE *AA10 FOUND OR SUCCESSFUL VANTAGE   *.
       F55KD.                                                           lv15
      *ACCESS
           MOVE        '1' TO AA10-CF.
      *N55KF.    NOTE *SET GRACE DATE                     *.
       F55KF.                                                           lv20
      *NOTE THAT FOR ANY VANTAGE
      *PRODUCTS THAT FALL INTO THE
      *CODE BELOW, AA10-ALDDUE WAS
      *SET IN F82FA
      *N55LA.    NOTE *SET GRACE DATE FOR TRADITIONAL     *.
       F55LA.    IF    TO01-PRCOD < 00199                               lv25
                 OR    (TO01-PRCOD > 00299
                 AND   TO01-PRCOD < 00599)
                 NEXT SENTENCE ELSE GO TO     F55LA-FN.
      *FORMAT CCYYMMDD; 100 = 1 MONTH
           COMPUTE     WZ67-ALDDUE = AA10-ALDDUE +
           100.
       F55LA-FN. EXIT.
      *N55LF.    NOTE ****** SET GRACE DATE FOR UL ****   *.
       F55LF.    IF    TO01-PRCOD > 00199                               lv25
                 AND   TO01-PRCOD < 00300
                 NEXT SENTENCE ELSE GO TO     F55LF-FN.
      *FORMAT CCYYMMDD; 200 = 2 MONTHS
           COMPUTE     WZ67-ALDDUE = AA10-ALDDUE +
           200.
       F55LF-FN. EXIT.
      *N55NA.    NOTE ***** IF GRACE DATE POPULATED ***   *.
       F55NA.    IF    WZ67-ALDDUE > ZERO                               lv25
                 NEXT SENTENCE ELSE GO TO     F55NA-FN.
      *N55ND.    NOTE ***** IF WENT BEYOND YEAR END ***   *.
       F55ND.    IF    WZ67-ALDDUE (5 : 2) = 13                         lv30
                 OR    WZ67-ALDDUE (5 : 2) = 14
                 NEXT SENTENCE ELSE GO TO     F55ND-FN.
      *********************************
      ****** FORMAT IS CCYYMMDD   *****
      ****** - SUBTRACT 12 MONTHS *****
      *********************************
           COMPUTE     WZ67-ALDDUE = WZ67-ALDDUE -
           1200
      *********************************
      ******  ADD 1 YEAR (10000)  *****
      *********************************
           COMPUTE     WZ67-ALDDUE = WZ67-ALDDUE +
           10000.
       F55ND-FN. EXIT.
       F55NA-FN. EXIT.
       F55KF-FN. EXIT.
      *N55PA.    NOTE *** IF TRAD OR UL CALC AMOUNTS **   *.
       F55PA.    IF    TO01-PRCOD < 00199                               lv20
                 OR    (TO01-PRCOD > 00299
                 AND   TO01-PRCOD < 00599)
                 NEXT SENTENCE ELSE GO TO     F55PA-FN.
           MOVE        ZERO TO AA85-CF
           WZ67-ALPAGM
           WZ67-ALPAGQ
           WZ67-ALPAGS
           MOVE        AA10-ALPAGR TO WZ67-ALPAGR
           MOVE        AA10-ALPMOD TO WZ67-ALPMOD
           PERFORM     F82JL THRU F82JL-FN.
       F55PA-FN. EXIT.
       F55KD-FN. EXIT.
       F55JA-FN. EXIT.
      *N55ZA.    NOTE *FOR FUND SOURCE                    *.
       F55ZA.                                                           lv10
      *COMPUTE: MIN/MAX SHARES
      *         MIN/MAX %
      *         MIN/MAX NBR OF MONTHS
           PERFORM     F91BA THRU F91BA-FN.
       F55ZA-FN. EXIT.
      *N55ZZ.    NOTE *END OF PROCESSING FOR LIFE DEST    *.
       F55ZZ.                                                           lv10
           MOVE                     ALL '1' TO FT GO TO F20.
       F55ZZ-FN. EXIT.
       F55-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *NOT A TRANSFER (DEST IS A PAYEE)   *
      *               *                                   *
      *               *************************************.
       F60.      IF    PAYOUT-TRANSACTION                               lv05
                 NEXT SENTENCE ELSE GO TO     F60-FN.
      *N60CA.    NOTE *SD                                 *.
       F60CA.    IF    WZ67-MAPPN = 'SD'                                lv10
                 NEXT SENTENCE ELSE GO TO     F60CA-FN.
      *SET REG PMT FIELDS:
      *- INDICATOR
      *- MIN
      *- MAX
           MOVE        'Y' TO WZ67-IARRGA
           MOVE        +50.00 TO WZ67-AMIND
           WZ67-AINPU
           MOVE        +9999999.99 TO WZ67-AMAXAR.
       F60CA-FN. EXIT.
      *N60DA.    NOTE *UD OR FDC                          *.
       F60DA.    IF    WZ67-MAPPN = 'UD' OR 'FDC'                       lv10
                 NEXT SENTENCE ELSE GO TO     F60DA-FN.
      *SET REG PMT FIELDS:
      *- INDICATOR
      *- MIN
      *- MAX
           MOVE        'Y' TO WZ67-IARRGA
           MOVE        ZERO TO WZ67-AMIND
           WZ67-AINPU
           MOVE        +9999999.99 TO WZ67-AMAXAR
           INITIALIZE  DF30 DF33 7-OAGE-PASSED-FIELDS
           MOVE        WZ67-DCACG TO DF33-DTGRG
           MOVE        4 TO DF30-CDTSF
           PERFORM     F91AB THRU F91AB-FN.
                 IF    DF30-CDTSC = 0                                   DOT
           MOVE        DF33-DTJUL TO 7-OAGE-CURRENT-DATE.
           INITIALIZE  DF30 DF33                                        DOT
           MOVE        WZ67-CLDOB TO DF33-DTGRG
           MOVE        4 TO DF30-CDTSF
           PERFORM     F91AB THRU F91AB-FN.
                 IF    DF30-CDTSC = 0                                   DOT
           MOVE        DF33-DTJUL TO 7-OAGE-BIRTH-DATE.
                 IF    7-OAGE-CURRENT-DATE > ZERO                       DOT
                 AND   7-OAGE-BIRTH-DATE > ZERO
           PERFORM     F91FA THRU F91FA-FN.
      *N60DD.    NOTE *IF PHONE... INCREASE MINIMUMS      *.
       F60DD.    IF    WZ67-CHCR = 03                                   lv15
                 NEXT SENTENCE ELSE GO TO     F60DD-FN.
      *N60DG.    NOTE *IF CHECK TO OWNER                  *.
       F60DG.    IF    WZ67-IDELI = 'Y'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60DG-FN.
      *N60DH.    NOTE *IF QUALIFIED MUTUAL FUND ACCOUNT   *.
       F60DH.    IF    FR01-CTIDA = 002                                 lv25
                 AND   FR01-CQACT > ZERO
                 NEXT SENTENCE ELSE GO TO     F60DH-FN.
      *AND CLIENT AGE < 70.5
                 IF    7-OAGE-CLIENT-AGE < 70.5                         DOT
           MOVE        +50.00 TO WZ67-AMIND
           WZ67-AINPU
           MOVE        +50000.00 TO WZ67-AMAXAR
                 ELSE
      *NO RESTRICTION - AGE > 70.5
           MOVE        +0.00 TO WZ67-AMIND
           WZ67-AINPU
           MOVE        +50000.00 TO WZ67-AMAXAR.
                 IF    FR01-CIRAS = 003                                 DOT
      *IF BENEFICIARY IRA
           MOVE        +0.00 TO WZ67-AMIND
           WZ67-AINPU
           MOVE        +50000.00 TO WZ67-AMAXAR.
       F60DH-900. GO TO F60DI-FN.
       F60DH-FN. EXIT.
      *N60DI.    NOTE *ALL OTHER ACCOUNTS DEFAULT EDIT    *.
       F60DI.                                                           lv25
           MOVE        +100.00 TO WZ67-AMIND
           WZ67-AINPU
           MOVE        +50000.00 TO WZ67-AMAXAR.
       F60DI-FN. EXIT.
       F60DG-FN. EXIT.
      *N60DK.    NOTE *IF CHECK OR ACH                    *.
       F60DK.    IF    WZ67-CDEL1 = 002                                 lv20
                 OR    WZ67-CDEL1 = 003
                 NEXT SENTENCE ELSE GO TO     F60DK-FN.
      *N60DN.    NOTE *IF QUALIFIED MUTUAL FUND ACCOUNT   *.
       F60DN.    IF    FR01-CTIDA = 002                                 lv25
                 AND   FR01-CQACT > ZERO
                 NEXT SENTENCE ELSE GO TO     F60DN-FN.
      *AND CLIENT AGE < 70.5 - EXCLUDE
                 IF    7-OAGE-CLIENT-AGE < 70.5                         DOT
           MOVE        +50.00 TO WZ67-AMIND
           WZ67-AINPU
           MOVE        +50000.00 TO WZ67-AMAXAR
                 ELSE
      *NO RESTRICTION - AGE > 70.5
           MOVE        +0.00 TO WZ67-AMIND
           WZ67-AINPU
           MOVE        +50000.00 TO WZ67-AMAXAR.
                 IF    FR01-CIRAS = 003                                 DOT
      *IF BENEFICIARY IRA
           MOVE        +0.00 TO WZ67-AMIND
           WZ67-AINPU
           MOVE        +50000.00 TO WZ67-AMAXAR.
       F60DN-900. GO TO F60DO-FN.
       F60DN-FN. EXIT.
      *N60DO.    NOTE *ALL OTHER ACCOUNTS DEFAULT EDIT    *.
       F60DO.                                                           lv25
           MOVE        +100.00 TO WZ67-AMIND
           WZ67-AINPU
           MOVE        +50000.00 TO WZ67-AMAXAR.
       F60DO-FN. EXIT.
       F60DK-FN. EXIT.
       F60DD-FN. EXIT.
       F60DA-FN. EXIT.
      *N60ZA.    NOTE *FOR FUND SOURCE                    *.
       F60ZA.                                                           lv10
      *COMPUTE: MIN/MAX SHARES
      *         MIN/MAX %
      *         MIN/MAX NBR OF MONTHS
           PERFORM     F91BA THRU F91BA-FN.
       F60ZA-FN. EXIT.
      *N60ZZ.    NOTE *END OF PROCESSING FOR PAYEE DEST   *.
       F60ZZ.                                                           lv10
           MOVE                     ALL '1' TO FT GO TO F20.
       F60ZZ-FN. EXIT.
       F60-FN.   EXIT.
      *N62.      NOTE *************************************.
      *               *                                   *
      *               *DEST IS BROKERAGE ACCT             *
      *               *                                   *
      *               *************************************.
       F62.      IF    TO01-CTIDA = 021 OR 133                          lv05
                 NEXT SENTENCE ELSE GO TO     F62-FN.
      *N62CA.    NOTE *BROKERAGE - REG PMTS ALLOWED       *.
       F62CA.    IF    WZ67-IARRGA = 'Y'                                lv10
                 OR    (WZ67-IARRGA = 'N'
                 AND   TO01-CTIDA = 133)
                 NEXT SENTENCE ELSE GO TO     F62CA-FN.
      *SET REG PMT FIELDS
      *BETA - SET REG PMT FIELDS
      *- MIN
      *- MAX
      *FOR BETA BROKERAGE ACCOUNTS, THE
      *'REGULAR PAYMENT TYPE' FIELDS
      *WILL BE USED AS 'CASH DEPOSIT
      *PAYMENT TYPE' FIELDS
           MOVE        ZERO TO WZ67-AMIND
           WZ67-AINPU
           MOVE        +9999999.99 TO WZ67-AMAXAR.
       F62CA-FN. EXIT.
      *N62ZA.    NOTE *FOR FUND SOURCE                    *.
       F62ZA.                                                           lv10
      *COMPUTE: MIN/MAX SHARES
      *         MIN/MAX %
      *         MIN/MAX NBR OF MONTHS
           PERFORM     F91BA THRU F91BA-FN.
       F62ZA-FN. EXIT.
      *N62ZZ.    NOTE *END OF PROCESSING FOR BROK DEST    *.
       F62ZZ.                                                           lv10
           MOVE                     ALL '1' TO FT GO TO F20.
       F62ZZ-FN. EXIT.
       F62-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *FINANCIAL PLAN AS DESTINATION      *
      *               *                                   *
      *               *************************************.
       F65.      IF    TO01-CTIDA = 013                                 lv05
                 NEXT SENTENCE ELSE GO TO     F65-FN.
      *N65DA.    NOTE *SD                                 *.
       F65DA.    IF    WZ67-MAPPN = 'SD'                                lv10
                 NEXT SENTENCE ELSE GO TO     F65DA-FN.
      *SET MIN DISBURSEMENT AMT
           MOVE        50.00 TO WZ67-AMIND
           WZ67-AINPU.
       F65DA-FN. EXIT.
      *N65ZA.    NOTE *FUND SOURCE                        *.
       F65ZA.    IF    FR01-CTIDA = 002                                 lv10
                 NEXT SENTENCE ELSE GO TO     F65ZA-FN.
      *DISBURSEMENTS REQUESTED IN
      *SHARES, PERCENTAGES, AND NUMBER
      *OF MONTHS ARE NOT VALID FOR
      *FINANCIAL PLANS DESTINATIONS.
      *SET MINIMUMS TO 9'S
           MOVE        999999999.999 TO WZ67-QSHOWQ
           MOVE        999.999 TO WZ67-PACT1
           MOVE        999 TO WZ67-QMTH.
       F65ZA-FN. EXIT.
      *N65ZZ.    NOTE *END OF PROCESSING FOR FP           *.
       F65ZZ.                                                           lv10
           MOVE                     ALL '1' TO FT GO TO F20.
       F65ZZ-FN. EXIT.
       F65-FN.   EXIT.
      *N75.      NOTE *************************************.
      *               *                                   *
      *               *** INVALID ADMINISTRATOR CODE **   *
      *               *                                   *
      *               *************************************.
       F75.           EXIT.                                             lv05
      *N75BA.    NOTE ******* SEND ERROR MESSAGE ******   *.
       F75BA.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012104 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F75BA-FN. EXIT.
       F75-FN.   EXIT.
      *N79.      NOTE *************************************.            ADU102
      *               *                                   *             ADU102
      *               *---> Normal Termination            *             ADU102
      *               *                                   *             ADU102
      *               *************************************.            ADU102
       F79.                                                             lv05
      *     Return to Calling Module                                    ADU102
           MOVE                     ALL '1' TO FT GO TO F20.            ADU102
       F79-FN.   EXIT.
      *N82.      NOTE *************************************.
      *               *                                   *
      *               ******* LIFE SPECIAL EDITS ******   *
      *               *                                   *
      *               *************************************.
       F82.           EXIT.                                             lv05
      *N82FA.    NOTE *ACCESS VANTAGE BROKER              *.            AAOLBB
       F82FA.    IF    TA5B-CF = '1'                                    lv10
                 AND   TA5B-IVANT = 'Y'                                 AAOLBB
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
                 IF    WZ67-IARLNA = 'Y'                                DOT
                 AND   PE31-QTNOL = ZEROES
      *IF LOAN IS POSSIBLE IN EZ TRANS
      *AND TOTAL NUMBER OF LOANS ZERO                                   AAOLBB
      *DO NOT USE ERU
           MOVE        'N' TO WZ67-IARLNA
           MOVE        9999999.99 TO WZ67-AMINAL
           MOVE        ZERO TO WZ67-AMAXAL.
           MOVE        '1' TO AA10-CF                                   DOT
           MOVE        PE31-ALDDUE TO AA10-ALDDUE.                      AAOLBB
      *N82JL.    NOTE *BLOCK LEVEL TO WRAP CODE           *.
       F82JL.         EXIT.                                             lv14
      *N82JM.    NOTE *CHECKS FOR DI AND TRAD LIFE        *.
       F82JM.    IF    (TO01-PRCOD < 00199)                             lv15
                 OR    (TO01-PRCOD > 00299
                 AND   TO01-PRCOD < 00599)
                 AND   AA10-CF = '1'
                 NEXT SENTENCE ELSE GO TO     F82JM-FN.
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
      *N82LA.    NOTE *CALCULATE PREMIUM BY MONTHLY       *.
       F82LA.    IF    TO01-PRCOD = 310                                 lv35
                 OR    7-82-TL-2012 = 'Y'
                 NEXT SENTENCE ELSE GO TO     F82LA-FN.
      *FOR IPE INSURED OR TL2012                                        AAOLBB
           COMPUTE     WZ67-ALPAGM ROUNDED =
           (AA10-ALPAGR * 0.0875).                                      AAOLBB
      *                                                                 DOT
      *
       F82LA-FN. EXIT.
      *N82LB.    NOTE *CALCULATE PREMIUM BY MONTHLY       *.
       F82LB.    IF    TO01-PRCOD NOT = 310                             lv35
                 AND   (7-82-TL-2012 NOT = 'Y')
                 NEXT SENTENCE ELSE GO TO     F82LB-FN.
      *DON'T CHARGE PREMIUM FOR FL DI                                   AAOLBB
                 IF    7-82-FL-DI = 'Y'                                 DOT
           COMPUTE     WZ67-ALPAGM ROUNDED =
           (AA10-ALPAGR * 0.0868)                                       AAOLBB
                 ELSE
           COMPUTE     WZ67-ALPAGM ROUNDED =
           ((AA10-ALPAGR * 0.0868) + 0.60).                             AAOLBB
      *DISCONTINUED: A 50? DISCOUNT                                     AAOLBB
      *HERE FOR MONTHLY LIFE OR DI.                                     AAOLBB
      *
      *
       F82LB-FN. EXIT.
       F82JT-FN. EXIT.
      *N82LF.    NOTE *MODE PREMIUM CHECK QUARTERLY       *.
       F82LF.         EXIT.                                             lv30
      *N82LL.    NOTE *CALCULATE PREMIUM BY QUARTERS      *.
       F82LL.    IF    TO01-PRCOD = 310                                 lv35
                 OR    7-82-TL-2012 = 'Y'
                 NEXT SENTENCE ELSE GO TO     F82LL-FN.
      *FOR IPE INSURED OR TL 2012                                       AAOLBB
           COMPUTE     WZ67-ALPAGQ ROUNDED =
           (AA10-ALPAGR * 0.2625).                                      AAOLBB
      *                                                                 DOT
      *
       F82LL-FN. EXIT.
      *N82LM.    NOTE *CALCULATE PREMIUM BY QUARTERS      *.
       F82LM.    IF    TO01-PRCOD NOT = 310                             lv35
                 AND   (7-82-TL-2012 NOT = 'Y')
                 NEXT SENTENCE ELSE GO TO     F82LM-FN.
      *DON'T CHARGE PREMIUM FOR FL DI                                   AAOLBB
                 IF    7-82-FL-DI = 'Y'                                 DOT
           COMPUTE     WZ67-ALPAGQ ROUNDED =
           (AA10-ALPAGR * 0.258)                                        AAOLBB
                 ELSE
           COMPUTE     WZ67-ALPAGQ ROUNDED =
           ((AA10-ALPAGR * 0.258) + 0.50).                              AAOLBB
      *                                                                 DOT
      *
       F82LM-FN. EXIT.
       F82LF-FN. EXIT.
      *N82LP.    NOTE *MODE PREMIUM CHECK SEMI-ANNUAL     *.
       F82LP.         EXIT.                                             lv30
      *N82LS.    NOTE *CALCULATE PREMIUM FOR SEMI-ANN     *.
       F82LS.    IF    TO01-PRCOD = 310                                 lv35
                 OR    7-82-TL-2012 = 'Y'
                 NEXT SENTENCE ELSE GO TO     F82LS-FN.
      *FOR IPE INSURED OR TL 2012                                       AAOLBB
           COMPUTE     WZ67-ALPAGS ROUNDED =
           (AA10-ALPAGR * 0.515).                                       AAOLBB
      *                                                                 DOT
      *
       F82LS-FN. EXIT.
      *N82LT.    NOTE *CALCULATE PREMIUM FOR SEMI-ANN     *.
       F82LT.    IF    TO01-PRCOD NOT = 310                             lv35
                 AND   (7-82-TL-2012 NOT = 'Y')
                 NEXT SENTENCE ELSE GO TO     F82LT-FN.
      *DON'T CHARGE PREMIUM FOR FL DI                                   AAOLBB
                 IF    7-82-FL-DI = 'Y'                                 DOT
           COMPUTE     WZ67-ALPAGS ROUNDED =
           (AA10-ALPAGR * 0.502)                                        AAOLBB
                 ELSE
           COMPUTE     WZ67-ALPAGS ROUNDED =
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
      *N82MA.    NOTE *CALCULATE PREMIUM BY MONTHLY       *.
       F82MA.    IF    TO01-PRCOD = 310                                 lv35
                 OR    7-82-TL-2012 = 'Y'
                 NEXT SENTENCE ELSE GO TO     F82MA-FN.
      *FOR IPE INSURED PRODUCTS                                         AAOLBB
           COMPUTE     WZ67-ALPAGM ROUNDED =
           (AA10-ALPAGR * 0.0875).                                      AAOLBB
      *                                                                 DOT
      *
       F82MA-FN. EXIT.
      *N82MC.    NOTE *CALCULATE PREMIUM BY MONTHLY       *.
       F82MC.    IF    TO01-PRCOD NOT = 310                             lv35
                 AND   (7-82-TL-2012 NOT = 'Y')
                 NEXT SENTENCE ELSE GO TO     F82MC-FN.
           COMPUTE     7-82-TENS =                                      AAOLBB
           (AA10-ALPAGR - 7-82-ONE)                                     AAOLBB
           COMPUTE     7-82-PREMIUM-T ROUNDED =                         AAOLBB
           (7-82-TENS * 0.0868)                                         AAOLBB
           COMPUTE     7-82-PREMIUM-O ROUNDED =                         AAOLBB
           (7-82-CONES * 0.0868)                                        AAOLBB
           COMPUTE     WZ67-ALPAGM =
           ((7-82-PREMIUM-T                                             AAOLBB
           + 7-82-PREMIUM-O) + 0.60).                                   AAOLBB
      *DISCONTINUED: A 50? DISCOUNT                                     AAOLBB
      *HERE FOR MONTHLY LIFE OR DI.                                     AAOLBB
      *
      *
       F82MC-FN. EXIT.
       F82LY-FN. EXIT.
      *N82ME.    NOTE *MODE PREMIUM CHECK QUARTERLY       *.
       F82ME.         EXIT.                                             lv30
      *N82MF.    NOTE *CALCULATE PREMIUM BY QUARTERS      *.
       F82MF.    IF    TO01-PRCOD = 310                                 lv35
                 OR    7-82-TL-2012 = 'Y'
                 NEXT SENTENCE ELSE GO TO     F82MF-FN.
      *FOR IPE INSURED OR TL 2012                                       AAOLBB
           COMPUTE     WZ67-ALPAGQ ROUNDED =
           (AA10-ALPAGR * 0.2625).                                      AAOLBB
      *                                                                 DOT
      *
       F82MF-FN. EXIT.
      *N82MG.    NOTE *CALCULATE PREMIUM BY QUARTERS      *.
       F82MG.    IF    TO01-PRCOD NOT = 310                             lv35
                 AND   (7-82-TL-2012 NOT = 'Y')
                 NEXT SENTENCE ELSE GO TO     F82MG-FN.
           COMPUTE     7-82-TENS =                                      AAOLBB
           (AA10-ALPAGR - 7-82-ONE)                                     AAOLBB
           COMPUTE     7-82-PREMIUM-T ROUNDED =                         AAOLBB
           (7-82-TENS * 0.258)                                          AAOLBB
           COMPUTE     7-82-PREMIUM-O ROUNDED =                         AAOLBB
           (7-82-CONES * 0.258)                                         AAOLBB
           COMPUTE     WZ67-ALPAGQ =
           ((7-82-PREMIUM-T                                             AAOLBB
           + 7-82-PREMIUM-O) + 0.50).                                   AAOLBB
      *                                                                 DOT
      *
       F82MG-FN. EXIT.
       F82ME-FN. EXIT.
      *N82MI.    NOTE *MODE PREMIUM CHECK SEMI-ANNUAL     *.
       F82MI.         EXIT.                                             lv30
      *N82MJ.    NOTE *CALCULATE PREMIUM FOR SEMI-ANN     *.
       F82MJ.    IF    TO01-PRCOD = 310                                 lv35
                 OR    7-82-TL-2012 = 'Y'
                 NEXT SENTENCE ELSE GO TO     F82MJ-FN.
      *FOR IPE INSURED OR TL 2012                                       AAOLBB
           COMPUTE     WZ67-ALPAGS ROUNDED =
           (AA10-ALPAGR * 0.515).                                       AAOLBB
      *                                                                 DOT
      *
       F82MJ-FN. EXIT.
      *N82MK.    NOTE *CALCULATE PREMIUM FOR SEMI-ANN     *.
       F82MK.    IF    TO01-PRCOD NOT = 310                             lv35
                 AND   (7-82-TL-2012 NOT = 'Y')
                 NEXT SENTENCE ELSE GO TO     F82MK-FN.
           COMPUTE     7-82-TENS =                                      AAOLBB
           (AA10-ALPAGR - 7-82-ONE)                                     AAOLBB
           COMPUTE     7-82-PREMIUM-T ROUNDED =                         AAOLBB
           (7-82-TENS * 0.502)                                          AAOLBB
           COMPUTE     7-82-PREMIUM-O ROUNDED =                         AAOLBB
           (7-82-CONES * 0.502)                                         AAOLBB
           COMPUTE     WZ67-ALPAGS =
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
           COMPUTE     WZ67-ALPAGS ROUNDED =
           (AA10-ALPAGR * 0.510).                                       AAOLBB
      *                                                                 DOT
      *
       F82NF-FN. EXIT.
      *N82NM.    NOTE *COMPUTE PREMIUMS                   *.
       F82NM.                                                           lv25
           COMPUTE     WZ67-ALPAGQ ROUNDED =
           (AA10-ALPAGR * 0.260).                                       AAOLBB
      *                                                                 DOT
      *
       F82NM-FN. EXIT.
      *N82NP.    NOTE *COMPUTE PREMIUMS                   *.
       F82NP.                                                           lv25
           COMPUTE     WZ67-ALPAGM ROUNDED =
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
           MOVE        'N' TO WZ67-IFQQT.
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
           MOVE        'N' TO WZ67-IFQSA.
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
      *               ******     COMPUTATIONS     *****   *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91AB.    NOTE *CDU - DATE VALIDATE/CONVERT        *.            AADA81
       F91AB.                                                           lv10
      ** * * * * * * * * * * * * * * *                                  AADA81
      *This code calls the common date                                  AADA81
      *utility MWS100EX to validate a                                   AADA81
      *gregorian date or convert a date                                 AADA81
      *with a dynamic call.                                             AADA81
      ** * * * * * * * * * * * * * * *                                  AADA81
      *Before the call set the subfunc                                  AADA81
      *request code DF30-CDTSF:                                         AADA81
      *  1 = greg to julian conversion                                  AADA81
      *      (without greg validation)                                  AADA81
      *  2 = julian to greg conversion                                  AADA81
      *  3 = gregorian validation                                       AADA81
      *  4 = greg to julian conversion                                  AADA81
      *      (with greg validation)                                     AADA81
      ** * * * * * * * * * * * * * * *                                  AADA81
      *Check return code DF30-CDTSC                                     AADA81
      *after the call.                                                  AADA81
      *    0 = Error Free                                               AADA81
      *    3 = Invalid Date                                             AADA81
      *    5 = Invalid Day                                              AADA81
      *    6 = Invalid Month                                            AADA81
      ** * * * * * * * * * * * * * * *                                  AADA81
           MOVE        3 TO DF30-CDTFN                                  AADA81
           CALL        MWS100EX USING DF30                              AADA81
           DF33.                                                        AADA81
       F91AB-FN. EXIT.
      *N91BA.    NOTE *COMPUTE FIELDS FOR FUND SOURCE     *.
       F91BA.    IF    FR01-CTIDA = 002                                 lv10
                 NEXT SENTENCE ELSE GO TO     F91BA-FN.
      *N91BH.    NOTE *WORKING FIELD: ACCT VALUE MINUS    *.
       F91BH.                                                           lv15
      *VALUE OF CERT, LOI ESCROW SHARES
                 IF    WZ67-AACTVD > WZ67-AACTV                         DOT
      *ACCT VALUE > CERT,ESCROW SHR VAL
           COMPUTE     W-WORK-AACTVD = WZ67-AACTVD
           - WZ67-AACTV
                 ELSE
      *ELSE SET TO ZERO
           MOVE        ZERO TO W-WORK-AACTVD.
       F91BH-FN. EXIT.
      *N91CA.    NOTE *MINIMUM SHARES                     *.
       F91CA.                                                           lv15
           COMPUTE     WZ67-QSHOWQ ROUNDED =
           WZ67-AMIND / SQ5E-AFAV10 (1).
       F91CA-FN. EXIT.
      *N91CD.    NOTE *MAXIMUM SHARES                     *.
       F91CD.                                                           lv15
                 IF    FR01-CTSTA = 02                                  DOT
      *ACCOUNT ACTIVE
      *EXCLUDE CERT, LOI ESCROW SHRS
           COMPUTE     WZ67-QSHOW = SQ5E-QSHON (1) -
           WZ67-QSHIS -
           WZ67-QSHES
                 ELSE
           MOVE        9999999999.999 TO WZ67-QSHOW.
       F91CD-FN. EXIT.
      *N91CJ.    NOTE *MAXIMUM PERCENT                    *.
       F91CJ.                                                           lv15
           MOVE        +100 TO WZ67-PPOT1.
       F91CJ-FN. EXIT.
      *N91CL.    NOTE *MINIMUM PERCENT                    *.
       F91CL.         EXIT.                                             lv15
      *N91CM.    NOTE *ACCT VALUE MINUS CERT, ESCROW      *.
       F91CM.    IF    W-WORK-AACTVD > ZERO                             lv20
                 NEXT SENTENCE ELSE GO TO     F91CM-FN.
      *SHR VALUE IS POSITIVE
      *MINIMUM DESTINATION AMOUNT
      *DIVIDED BY SOURCE ACCOUNT VALUE
      *LESS CERT, ESCROW SHR VALUE
           COMPUTE     WZ67-PACT1 ROUNDED =
           WZ67-AMIND / W-WORK-AACTVD.
      *N91CO.    NOTE *% ROUNDED TO ZERO                  *.
       F91CO.    IF    WZ67-PACT1 = ZERO                                lv25
                 NEXT SENTENCE ELSE GO TO     F91CO-FN.
      *PERCENT WILL FALL TO ZERO
      *IF ACCOUNT VALUE IS A LOT
      *BIGGER THAN MINIMUM AMOUNT
           MOVE        001.000 TO WZ67-PACT1.
       F91CO-FN. EXIT.
       F91CM-900. GO TO F91CT-FN.
       F91CM-FN. EXIT.
      *N91CT.    NOTE *ELSE... SET MINIMUM TO ZERO        *.
       F91CT.                                                           lv20
           MOVE        ZERO TO WZ67-PACT1.
       F91CT-FN. EXIT.
       F91CL-FN. EXIT.
      *N91DA.    NOTE *MINIMUM NUMBER OF MONTHS           *.
       F91DA.                                                           lv15
           MOVE        001 TO WZ67-QMTH.
       F91DA-FN. EXIT.
      *N91DC.    NOTE *MAXIMUM NUMBER OF MONTHS           *.
       F91DC.                                                           lv15
      *(TO LIQUIDATE ACCOUNT)
                 IF    WZ67-AMIND > ZERO                                DOT
      *IF POSTIVE DEST MIN PMT:
      *ACCOUNT VALUE LESS VALUE OF
      *CERT, ESCROW SHARES DIVIDED BY
      *THE DESTINATIONS MINIMUM PAYMENT
      *AMOUNT
           COMPUTE     WZ67-QMTH1 = (W-WORK-AACTVD /
           WZ67-AMIND) + 1
           ON SIZE ERROR
           MOVE        999 TO WZ67-QMTH1
                 ELSE
      *ELSE... DEFAULT
           MOVE        999 TO WZ67-QMTH1.
       F91DC-FN. EXIT.
       F91BA-FN. EXIT.
      *N91FA.    NOTE *MACRO AAOAG3  -  CALC CLIENT AGE   *.            AAOAG3
       F91FA.                                                           lv10
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
               GO TO     F91FA-FN.                                      AAOAG3
                 IF    7-OAGE-BIRTH-DATE >                              DOT
                       7-OAGE-CURRENT-DATE                              AAOAG3
      *RETURN CLIENT AGE OF ZERO IF                                     AAOAG3
      *BIRTH DATE > CURRENT DATE                                        AAOAG3
               GO TO     F91FA-FN.                                      AAOAG3
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
       F91FA-FN. EXIT.
       F91-FN.   EXIT.
      *N92DA.    NOTE *---> Call CI0135                   *.            AM0135
       F92DA.                                                           lv10
      *     Get Cert Account Info                                       AM0135
      *                                                                 AM0135
           INITIALIZE  PJ02                                             AM0135
           DE10-DU03                                                    AM0135
           MOVE        PC01-CTID TO PJ02-CTID                           AM0135
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
           PJ02                                                         AM0135
           DE10                                                         AM0135
           MS03                                                         AM0135
           MX11.                                                        AM0135
      *N92DB.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F92DB.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F92DB-FN.                 ADU071
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
       F92DB-900. GO TO F92DC-FN.
       F92DB-FN. EXIT.
      *N92DC.    NOTE *NO ERRORS                          *.            ADU071
       F92DC.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F92DC-FN. EXIT.
       F92DA-FN. EXIT.
      *N92EB.    NOTE *ERROR ON TABLE READ FOR TA5B       *.
       F92EB.                                                           lv10
           MOVE        '0' TO TA5B-CF.
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
      *N92TF.    NOTE *RANDOM TABLE READ FOR TF09         *.            ADUTAB
       F92TF.                                                           lv10
           MOVE        'R1' TO G-TF09-TABFO                             ADUTAB
           COMPUTE     G-TF09-LTH = 60 + G-TF09-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TF09-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TF09)                                ADUTAB
                       LENGTH (G-TF09-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TF09-TABCR NOT = '00'                          DOT
           MOVE        '1' TO IK
                 ELSE
           MOVE        '0' TO IK.
       F92TF-FN. EXIT.
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
           XF06 AA10                                                    ADU026
           S-AAU10-SSA                                                  ADU026
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94A1-FN. EXIT.
      *N94A2.    NOTE *CALL GU ON AA20                    *.            ADU026
       F94A2.                                                           lv10
           MOVE        'LM1P' TO DE10-XDBDNM                            ADU026
           MOVE        'AA20' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XF06 AA20                                                    ADU026
           S-AAU10-SSA S-AA20-SSA                                       ADU026
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94A2-FN. EXIT.
      *N94A3.    NOTE *CALL GU ON AA66                    *.            ADU026
       F94A3.                                                           lv10
           MOVE        'LM1P' TO DE10-XDBDNM                            ADU026
           MOVE        'AA66' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XF06 AA66                                                    ADU026
           S-AAU10-SSA S-AA25-SSA S-AA66-SSA                            ADU026
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94A3-FN. EXIT.
      *N94A4.    NOTE *CALL GU ON AA85                    *.            ADU026
       F94A4.                                                           lv10
           MOVE        'LM1P' TO DE10-XDBDNM                            ADU026
           MOVE        'AA85' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XF06 AA85                                                    ADU026
           S-AAU10-SSA S-AA85-SSA                                       ADU026
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94A4-FN. EXIT.
      *N94BB.    NOTE *CALL GU ON CL01                    *.            ADU026
       F94BB.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XP06 CL01                                                    ADU026
           S-CLU01-SSA                                                  ADU026
           MOVE        XP06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94BB-FN. EXIT.
      *N94BD.    NOTE *CALL GN ON CL03                    *.            ADU026
       F94BD.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XP06 CL03                                                    ADU026
           S-CLU01-SSA S-CL03-SSA                                       ADU026
           MOVE        XP06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94BD-FN. EXIT.
      *N94CT.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94CT.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XA06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CT-FN. EXIT.
      *N94CV.    NOTE *CALL GN ON CT09                    *.            ADU026
       F94CV.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT09' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XA06 CT09                                                    ADU026
           S-CTU01-SSA S-CT07-SSA                                       ADU026
           7-CTA09-1-SSA
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CV-FN. EXIT.
      *N94SS.    NOTE *CALL GU ON SS01                    *.            ADU026
       F94SS.                                                           lv10
           MOVE        'SSPP' TO DE10-XDBDNM                            ADU026
           MOVE        'SS01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XE06 SS01                                                    ADU026
           S-SSU01-SSA                                                  ADU026
           MOVE        XE06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94SS-FN. EXIT.
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *CALL TO IP SOCKET BROKER           *
      *               *                                   *
      *               *************************************.
       F95.           EXIT.                                             lv05
      *N95BA.    NOTE *CALL DBI5000N                      *.
       F95BA.                                                           lv10
           CALL        DBI5000N USING                                   ACLDBI
           DFHEIBLK DFHCOMMAREA                                         ACLDBI
           WL00-REQUEST SQ1L                                            ACLDBI
           WL00-RESPONSE                                                ACLDBI
           SQ2L SQ3L.                                                   ACLDBI
       F95BA-FN. EXIT.
       F95-FN.   EXIT.
      *N96.      NOTE *************************************.
      *               *                                   *
      *               *ERROR HANDLING FOR BROKER CALL     *
      *               *                                   *
      *               *************************************.
       F96.           EXIT.                                             lv05
      *N96BC.    NOTE *DO THE ERROR HANDLING FOR THE      *.            AAER85
       F96BC.                                                           lv10
      *BROKER CALL DONE AS A PART OF                                    AAER85
      *MFTA PROJECT.                                                    AAER85
      *--------------------------------                                 AAER85
      *INITIALIZE THE VARIABLES USED                                    AAER85
      *TO INDICATE ERROR.                                               AAER85
      *--------------------------------                                 AAER85
           INITIALIZE  WS00-IERRC                                       AAER85
           WS00-CSEVR                                                   AAER85
           WS01-IERRC.                                                  AAER85
      *N96BH.    NOTE *CHECK THE BROKER ERROR.            *.            AAER85
       F96BH.    IF    SQ2L-CSEVR1 NOT = '00'                           lv15
                 AND   SQ2L-CSEVR1 NOT = '04'                           AAER85
                 NEXT SENTENCE ELSE GO TO     F96BH-FN.                 AAER85
      *A RETURN CODE OF '00' IS GOOD.A                                  AAER85
      *RETURN CODE OF '04' IS A WARNING                                 AAER85
      *ON A CLOSE CALL, BUT DATA GETS                                   AAER85
      *RETURNED. IN CASE OF ANY OTHER                                   AAER85
      *RETURN CODE PREPARE ERROR MESG.                                  AAER85
      *--------------------------------                                 AAER85
      *CHECK IF IGNORABLE ERROR                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        SQ2L-CERRE1 TO WS00-CERRE1                       AAER85
           MOVE        'B' TO WS00-ERROR-AREA                           AAER85
           PERFORM     F96TB THRU F96TB-FN.                             AAER85
      *N96BM.    NOTE *NOT IGNORABLE ERROR, PREPARE THE   *.            AAER85
       F96BM.    IF    WS00-IGNORE-ERROR = 'N'                          lv20
                 NEXT SENTENCE ELSE GO TO     F96BM-FN.                 AAER85
      *CORRESPONDING DST VIEW.                                          AAER85
      *--------------------------------                                 AAER85
           MOVE        'B' TO WS00-IERRC                                AAER85
           MOVE        'S' TO WS00-CSEVR                                AAER85
           PERFORM     F96KB THRU F96KB-FN.                             AAER85
      *N96BR.    NOTE *APPEND THE BROKER ERROR AT THE     *.            AAER85
       F96BR.    IF    SQ2L-CERRE1 = '01093'                            lv25
                 NEXT SENTENCE ELSE GO TO     F96BR-FN.                 AAER85
      *END OF THE ERROR MESSAGE. FOR                                    AAER85
      *THE DST ERROR, GET THE TEXT                                      AAER85
      *FROM THE PACTABLE TF0009                                         AAER85
      *--------------------------------                                 AAER85
      *GET THE ERROR TEXT FOR THE                                       AAER85
      *ERROR RETURNED BY DST.                                           AAER85
      *--------------------------------                                 AAER85
           MOVE        SQ2L-CERRE3 TO TF09-CERRE2                       AAER85
           PERFORM     F92TF THRU F92TF-FN                              AAER85
      *--------------------------------                                 AAER85
                 IF    IK NOT = '0'                                     DOT
      *IF PACTABLE READ FAILS, POPULATE                                 AAER85
      *DEFAULT MESSAGE                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE                         'Message information could not bAAER85
      -                'e found.' TO TF09-TERMT3.                       AAER85
      *--------------------------------                                 DOT
      *APPEND THE PB0251 ERROR AT THE                                   AAER85
      *END OF THE ERROR MESSAGE                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        TF09-CERRE2 TO MS03-TMESS4 (55:5)                AAER85
           MOVE        ' - ' TO MS03-TMESS4 (60:3)                      AAER85
           MOVE        TF09-TERMT3 TO MS03-TMESS4 (63:66).              AAER85
       F96BR-900. GO TO F96BV-FN.
       F96BR-FN. EXIT.
      *N96BV.    NOTE *APPEND THE BROKER ERROR AT THE     *.            AAER85
       F96BV.                                                           lv25
      *END OF THE ERROR MESSAGE                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        SQ2L-CERRE1 TO MS03-TMESS4 (55:5)                AAER85
           MOVE        ' - ' TO MS03-TMESS4 (60:3)                      AAER85
           MOVE        SQ2L-TERMT TO MS03-TMESS4 (63:66).               AAER85
       F96BV-FN. EXIT.
       F96BM-900. GO TO F96BY-FN.
       F96BM-FN. EXIT.
      *N96BY.    NOTE *IGNORABLE ERROR. SPACE OUT THE     *.            AAER85
       F96BY.                                                           lv20
      *ERROR IN BROKER AREA.                                            AAER85
      *--------------------------------                                 AAER85
           MOVE ALL    ZERO TO SQ2L-CSEVR1                              AAER85
           INITIALIZE  SQ2L-CERRE1                                      AAER85
      *--------------------------------                                 AAER85
      *POPULATE THE VARIABLE FOR                                        AAER85
      *IGNORABLE BROKER ERROR                                           AAER85
      *--------------------------------                                 AAER85
           MOVE        'B' TO WS01-IERRC                                AAER85
           MOVE        'N' TO WS00-CSEVR.                               AAER85
       F96BY-FN. EXIT.
       F96BH-FN. EXIT.
      *N96CB.    NOTE *CHECK IF ERROR IN THE PB0251       *.            AAER85
       F96CB.                                                           lv15
      *AREA.                                                            AAER85
      *N96CH.    NOTE *IF ERROR FOUND IN PB0251 PREPARE   *.            AAER85
       F96CH.    IF    SQ3L-CAPIR1 NOT = ZERO                           lv20
                 AND   SQ3L-CAPIR1 NOT = 11                             AAER85
                 NEXT SENTENCE ELSE GO TO     F96CH-FN.                 AAER85
      *THE CORRESPONDING ERROR MESSAGE                                  AAER85
      *--------------------------------                                 AAER85
      *CHECK OF IGNORABLE ERROR                                         AAER85
      *--------------------------------                                 AAER85
           MOVE ALL    ZEROS TO WS00-CERRE1                             AAER85
           MOVE        SQ3L-CAPIR1 TO WS00-CERRE1 (3:3)                 AAER85
           MOVE        'P' TO WS00-ERROR-AREA                           AAER85
           PERFORM     F96TB THRU F96TB-FN.                             AAER85
      *N96CM.    NOTE *IF FIRST NON-IGNORABLE ERROR GET   *.            AAER85
       F96CM.    IF    WS00-IGNORE-ERROR = 'N'                          lv25
                 AND   WS00-IERRC = SPACES                              AAER85
                 NEXT SENTENCE ELSE GO TO     F96CM-FN.                 AAER85
      *THE ERROR MESSAGE                                                AAER85
      *--------------------------------                                 AAER85
           MOVE        'P' TO WS00-IERRC                                AAER85
           MOVE        'S' TO WS00-CSEVR                                AAER85
           PERFORM     F96KB THRU F96KB-FN.                             AAER85
      *N96CT.    NOTE *PREPARE THE ERROR MESSAGE          *.            AAER85
       F96CT.                                                           lv30
      *--------------------------------                                 AAER85
      *GET THE ERROR TEXT FOR THE                                       AAER85
      *PB0251 ERROR.                                                    AAER85
      *--------------------------------                                 AAER85
           MOVE        WS00-CERRE1 TO TF09-CERRE2                       AAER85
           PERFORM     F92TF THRU F92TF-FN                              AAER85
      *--------------------------------                                 AAER85
                 IF    IK NOT = '0'                                     DOT
      *IF PACTABLE READ FAILS, POPULATE                                 AAER85
      *DEFAULT MESSAGE                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE                         'Message information could not bAAER85
      -                'e found.' TO TF09-TERMT3.                       AAER85
      *--------------------------------                                 DOT
      *APPEND THE PB0251 ERROR AT THE                                   AAER85
      *END OF THE ERROR MESSAGE                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        TF09-CERRE2 TO MS03-TMESS4 (55:5)                AAER85
           MOVE        ' - ' TO MS03-TMESS4 (60:3)                      AAER85
           MOVE        TF09-TERMT3 TO MS03-TMESS4 (63:66).              AAER85
       F96CT-FN. EXIT.
       F96CM-FN. EXIT.
      *N96CX.    NOTE *IGNORABLE ERROR. SPACE OUT THE     *.            AAER85
       F96CX.    IF    WS00-IGNORE-ERROR NOT = 'N'                      lv25
                 NEXT SENTENCE ELSE GO TO     F96CX-FN.                 AAER85
      *ERROR IN PB251 AREA.                                             AAER85
      *--------------------------------                                 AAER85
           INITIALIZE  SQ3L-CAPIR1.                                     AAER85
                 IF    WS01-IERRC = SPACES                              DOT
      *--------------------------------                                 AAER85
      *IF THIS IS THE FIRST IGNORABLE                                   AAER85
      *ERROR, POPULATE THE INDICATOR                                    AAER85
      *WITH ERROR CODE                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE        'P' TO WS01-IERRC                                AAER85
           MOVE        'N' TO WS00-CSEVR.                               AAER85
       F96CX-FN. EXIT.
       F96CH-FN. EXIT.
       F96CB-FN. EXIT.
      *N96DB.    NOTE *CHECK FOR THE ERROR IN THE         *.            AAER85
       F96DB.                                                           lv15
      *RESPONSE SECTION.                                                AAER85
      *N96DM.    NOTE *IF ERROR IN THE FIXED AREA OF      *.            AAER85
       F96DM.    IF    WL00-RESPONSE (1:5)                              lv20
                       NOT = SPACES                                     AAER85
                 NEXT SENTENCE ELSE GO TO     F96DM-FN.                 AAER85
      *THE RESPONSE SECTION.                                            AAER85
      *--------------------------------                                 AAER85
      *CHECK IF IGNORABLE ERROR.                                        AAER85
      *--------------------------------                                 AAER85
           MOVE        WL00-RESPONSE (1:5) TO WS00-CERRE1               AAER85
           MOVE        'F' TO WS00-ERROR-AREA                           AAER85
           PERFORM     F96TB THRU F96TB-FN.                             AAER85
      *N96DR.    NOTE *IF FIRST NON-IGNORABLE ERROR       *.            AAER85
       F96DR.    IF    WS00-IGNORE-ERROR = 'N'                          lv25
                 AND   WS00-IERRC = SPACES                              AAER85
                 NEXT SENTENCE ELSE GO TO     F96DR-FN.                 AAER85
      *PREPARE THE ERROR MESSAGE                                        AAER85
      *--------------------------------                                 AAER85
           MOVE        'F' TO WS00-IERRC                                AAER85
           MOVE        'S' TO WS00-CSEVR                                AAER85
           PERFORM     F96KB THRU F96KB-FN.                             AAER85
      *N96EB.    NOTE *PREPARE THE ERROR MESSAGE          *.            AAER85
       F96EB.                                                           lv30
      *--------------------------------                                 AAER85
      *GET THE ERROR TEXT FOR THE FIXED                                 AAER85
      *AREA ERROR                                                       AAER85
      *--------------------------------                                 AAER85
           MOVE        WL00-RESPONSE (1:5) TO TF09-CERRE2               AAER85
           PERFORM     F92TF THRU F92TF-FN.                             AAER85
                 IF    IK NOT = '0'                                     DOT
      *--------------------------------                                 AAER85
      *IF PACTABLE READ FAILS, POPULATE                                 AAER85
      *DEFAULT MESSAGE                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE                         'Message information could not bAAER85
      -                'e found.' TO TF09-TERMT3.                       AAER85
      *--------------------------------                                 DOT
      *APPEND THE FIXED AREA ERROR AT                                   AAER85
      *THE END OF THE ERROR MESSAGE                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        TF09-CERRE2 TO MS03-TMESS4 (55:5)                AAER85
           MOVE        ' - ' TO MS03-TMESS4 (60:3)                      AAER85
           MOVE        TF09-TERMT3 TO MS03-TMESS4 (63:66).              AAER85
       F96EB-FN. EXIT.
       F96DR-FN. EXIT.
      *N96EF.    NOTE *IGNORABLE ERROR. SPACE OUT THE     *.            AAER85
       F96EF.    IF    WS00-IGNORE-ERROR NOT = 'N'                      lv25
                 NEXT SENTENCE ELSE GO TO     F96EF-FN.                 AAER85
      *ERROR IN FIXED RESPONSE AREA.                                    AAER85
      *--------------------------------                                 AAER85
           MOVE        SPACES TO WL00-RESPONSE (1:5).                   AAER85
                 IF    WS01-IERRC = SPACES                              DOT
      *--------------------------------                                 AAER85
      *IF THIS IS THE FIRST IGNORABLE                                   AAER85
      *ERROR, POPULATE THE INDICATOR                                    AAER85
      *WITH ERROR CODE                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE        'F' TO WS01-IERRC                                AAER85
           MOVE        'N' TO WS00-CSEVR.                               AAER85
       F96EF-FN. EXIT.
       F96DM-FN. EXIT.
       F96DB-FN. EXIT.
      *N96FB.    NOTE *IF VARIABLE REPEATABLE AREA IS     *.            AAER85
       F96FB.    IF    WS-VAR-GELL > 0                                  lv15
                 NEXT SENTENCE ELSE GO TO     F96FB-FN.                 AAER85
      *PRESENT IN THE RESPONSE SECTION                                  AAER85
      *CHECK FOR THE ERRORS                                             AAER85
      *N96FG.    NOTE *SET THE VARIABLES TO IDENTIFY      *.            AAER85
       F96FG.                                                           lv20
      *THE POSITION OF THE ERROR CODES                                  AAER85
      *IN THE REPEATABLE AREA OF                                        AAER85
      *RESPONSE                                                         AAER85
      *--------------------------------                                 AAER85
           COMPUTE     WS-ERR-GELL = WS-FIX-GELL + 1                    AAER85
      *--------------------------------                                 AAER85
      *GET THE NUMBER OF ROWS RETURNED                                  AAER85
      *--------------------------------                                 AAER85
           MOVE        WL00-RESPONSE (6:3) TO WS-NRURO.                 AAER85
      *N96FL.    NOTE *CHECK FOR ALL THE ROWS IN THE      *.            AAER85
       F96FL.                                                           lv25
           MOVE        1                        TO J96FLR               AAER85
                                    GO TO     F96FL-B.                  AAER85
       F96FL-A.
           ADD         1                        TO J96FLR.              AAER85
       F96FL-B.
           IF          J96FLR                   >  WS-NRURO             AAER85
                                    GO TO     F96FL-FN.                 AAER85
      *VARIABLE REPEATABLE AREA.                                        AAER85
      *--------------------------------                                 AAER85
           MOVE        'V' TO WS00-ERROR-AREA.                          AAER85
      *N96FQ.    NOTE *ERROR IN IN THE RESPONSE SECTION   *.            AAER85
       F96FQ.    IF    WL00-RESPONSE (WS-ERR-GELL:5)                    lv30
                       NOT = SPACES                                     AAER85
                 NEXT SENTENCE ELSE GO TO     F96FQ-FN.                 AAER85
      *--------------------------------                                 AAER85
      *CHECK IF IGNORABLE ERROR                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        WL00-RESPONSE (WS-ERR-GELL:5) TO                 AAER85
           WS00-CERRE1                                                  AAER85
           PERFORM     F96TB THRU F96TB-FN.                             AAER85
      *N96FV.    NOTE *GET THE ERROR MESSAGE FOR NON      *.            AAER85
       F96FV.    IF    WS00-IGNORE-ERROR = 'N'                          lv35
                 AND   WS00-IERRC = SPACES                              AAER85
                 NEXT SENTENCE ELSE GO TO     F96FV-FN.                 AAER85
      *IGNORABLE ERROR RETURNED BY THE                                  AAER85
      *DST VIEW.                                                        AAER85
      *--------------------------------                                 AAER85
           MOVE        'V' TO WS00-IERRC                                AAER85
           MOVE        'S' TO WS00-CSEVR                                AAER85
           PERFORM     F96KB THRU F96KB-FN.                             AAER85
      *N96GB.    NOTE *PREPARE THE ERROR MESSAGE          *.            AAER85
       F96GB.                                                           lv40
      *--------------------------------                                 AAER85
      *GET THE ERROR TEXT FOR THE                                       AAER85
      *VARIABLE AREA ERROR                                              AAER85
      *--------------------------------                                 AAER85
           MOVE        WL00-RESPONSE (WS-ERR-GELL:5) TO                 AAER85
           TF09-CERRE2                                                  AAER85
           PERFORM     F92TF THRU F92TF-FN.                             AAER85
                 IF    IK NOT = '0'                                     DOT
      *--------------------------------                                 AAER85
      *IF PACTABLE READ FAILS, POPULATE                                 AAER85
      *DEFAULT MESSAGE                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE                         'Message information could not bAAER85
      -                'e found.' TO TF09-TERMT3.                       AAER85
      *--------------------------------                                 DOT
      *APPEND THE REPEATABLE AREA ERROR                                 AAER85
      *AT THE END OF THE ERROR MESSAGE                                  AAER85
      *--------------------------------                                 AAER85
           MOVE        TF09-CERRE2 TO MS03-TMESS4 (55:5)                AAER85
           MOVE        ' - ' TO MS03-TMESS4 (60:3)                      AAER85
           MOVE        TF09-TERMT3 TO MS03-TMESS4 (63:66).              AAER85
       F96GB-FN. EXIT.
       F96FV-FN. EXIT.
      *N96GH.    NOTE *IGNORABLE ERROR. SPACE OUT THE     *.            AAER85
       F96GH.    IF    WS00-IGNORE-ERROR NOT = 'N'                      lv35
                 NEXT SENTENCE ELSE GO TO     F96GH-FN.                 AAER85
      *ERROR IN FIXED RESPONSE AREA.                                    AAER85
      *--------------------------------                                 AAER85
           MOVE        SPACES TO                                        AAER85
           WL00-RESPONSE (WS-ERR-GELL:5).                               AAER85
                 IF    WS01-IERRC = SPACES                              DOT
      *--------------------------------                                 AAER85
      *IF THIS IS THE FIRST IGNORABLE                                   AAER85
      *ERROR, POPULATE THE INDICATOR                                    AAER85
      *WITH ERROR CODE                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE        'V' TO WS01-IERRC                                AAER85
           MOVE        'N' TO WS00-CSEVR.                               AAER85
       F96GH-FN. EXIT.
       F96FQ-FN. EXIT.
      *N96GM.    NOTE *ADD THE LENGTH OF THE VARIABLE     *.            AAER85
       F96GM.                                                           lv30
      *AREA ROW TO GET THE POSITION OF                                  AAER85
      *NEXT ERROR FIELD.                                                AAER85
      *--------------------------------                                 AAER85
           ADD         WS-VAR-GELL TO WS-ERR-GELL.                      AAER85
       F96GM-FN. EXIT.
       F96FL-900. GO TO F96FL-A.
       F96FL-FN. EXIT.
       F96FG-FN. EXIT.
       F96FB-FN. EXIT.
       F96BC-FN. EXIT.
      *N96KB.    NOTE *GET THE GENERIC ERROR MESSAGE      *.            AAER85
       F96KB.                                                           lv10
      *FOR THE DST VIEW.                                                AAER85
      *N96KH.    NOTE *GET THE ERROR NUMBER FOR THE       *.            AAER85
       F96KH.                                                           lv15
      *GENERIC ERROR MESSAGE FOR THE                                    AAER85
      *DST VIEW.                                                        AAER85
      *N96KM.    NOTE *ERROR IN THE ACCOUNT INFO. VIEW    *.            AAER85
       F96KM.    IF    SQ1L-NVIEW =                                     lv20
                       '2933'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F96KM-FN.                 AAER85
      *CALL.                                                            AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *ACCOUNT INFO. VIEW ERROR.                                        AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14516 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14551 TO MS03-NMESS2.                            AAER85
       F96KM-900. GO TO F96KH-FN.
       F96KM-FN. EXIT.
      *N96KO.    NOTE *ERROR IN THE MCB ACCOUNT INFO      *.            AAER85
       F96KO.    IF    SQ1L-NVIEW =                                     lv20
                       '4838'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F96KO-FN.                 AAER85
      *VIEW - 4838 CALL.                                                AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *MCB ACCOUNT INFO VIEW ERROR.                                     AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        15611 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        15612 TO MS03-NMESS2.                            AAER85
       F96KO-900. GO TO F96KH-FN.
       F96KO-FN. EXIT.
      *N96KQ.    NOTE *ERROR IN THE MCB GAIN/LOSS         *.            AAER85
       F96KQ.    IF    SQ1L-NVIEW =                                     lv20
                       '4865'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F96KQ-FN.                 AAER85
      *INFORMATION VIEW - 4865 CALL.                                    AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *MCB GAIN/LOSS INFORMATION VIEW                                   AAER85
      *ERROR.                                                           AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14536 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14571 TO MS03-NMESS2.                            AAER85
       F96KQ-900. GO TO F96KH-FN.
       F96KQ-FN. EXIT.
      *N96KR.    NOTE *ERROR IN THE TRAN. INFO (SINGLE)   *.            AAER85
       F96KR.    IF    SQ1L-NVIEW =                                     lv20
                       '2934'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F96KR-FN.                 AAER85
      *VIEW.                                                            AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *TRAN. INFO. (SINGLE) VIEW.                                       AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14517 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14552 TO MS03-NMESS2.                            AAER85
       F96KR-900. GO TO F96KH-FN.
       F96KR-FN. EXIT.
      *N96KW.    NOTE *ERROR IN THE TRAN. INFO (LIST)     *.            AAER85
       F96KW.    IF    SQ1L-NVIEW =                                     lv20
                       '2935'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F96KW-FN.                 AAER85
      *VIEW.                                                            AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *TRAN. INFO. (LIST) VIEW.                                         AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14518 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14553 TO MS03-NMESS2.                            AAER85
       F96KW-900. GO TO F96KH-FN.
       F96KW-FN. EXIT.
      *N96LC.    NOTE *ERROR IN THE GROUP INFO. VIEW      *.            AAER85
       F96LC.    IF    SQ1L-NVIEW =                                     lv20
                       '2939'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F96LC-FN.                 AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *GROUP INFO. VIEW.                                                AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14520 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14555 TO MS03-NMESS2.                            AAER85
       F96LC-900. GO TO F96KH-FN.
       F96LC-FN. EXIT.
      *N96LH.    NOTE *ERROR IN THE ACCOUNT VALUE VIEW    *.            AAER85
       F96LH.    IF    SQ1L-NVIEW =                                     lv20
                       '2949'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F96LH-FN.                 AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *ACCOUNT VALUE VIEW.                                              AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14533 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14568 TO MS03-NMESS2.                            AAER85
       F96LH-900. GO TO F96KH-FN.
       F96LH-FN. EXIT.
      *N96LM.    NOTE *ERROR IN THE GOOD FUNDS VIEW       *.            AAER85
       F96LM.    IF    SQ1L-NVIEW =                                     lv20
                       '2940'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F96LM-FN.                 AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *GOOD FUNDS VIEW.                                                 AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14534 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14569 TO MS03-NMESS2.                            AAER85
       F96LM-900. GO TO F96KH-FN.
       F96LM-FN. EXIT.
      *N96LR.    NOTE *ERROR IN THE CDSC FJ8X VIEW        *.            AAER85
       F96LR.    IF    SQ1L-NVIEW =                                     lv20
                       '2903'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F96LR-FN.                 AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *CDSC FJ8X VIEW.                                                  AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14528 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14563 TO MS03-NMESS2.                            AAER85
       F96LR-900. GO TO F96KH-FN.
       F96LR-FN. EXIT.
      *N96LW.    NOTE *ERROR IN THE CDSC FJXC VIEW        *.            AAER85
       F96LW.    IF    SQ1L-NVIEW =                                     lv20
                       '2907'                                           AAER85
                 NEXT SENTENCE ELSE GO TO     F96LW-FN.                 AAER85
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *CDSC FJXC VIEW.                                                  AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14532 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14567 TO MS03-NMESS2.                            AAER85
       F96LW-900. GO TO F96KH-FN.
       F96LW-FN. EXIT.
      *N96PB.    NOTE *OTHER ERROR                        *.            AAER85
       F96PB.                                                           lv20
      *--------------------------------                                 AAER85
      *MOVE THE MESSAGE NUMBER FOR THE                                  AAER85
      *INVALID VIEW.                                                    AAER85
      *--------------------------------                                 AAER85
                 IF    WS00-IERRC = 'B'                                 DOT
      *BROKER ERROR                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        14550 TO MS03-NMESS2                             AAER85
      *--------------------------------                                 AAER85
                 ELSE                                                   AAER85
      *NON-BROKER ERROR                                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        14585 TO MS03-NMESS2.                            AAER85
       F96PB-FN. EXIT.
       F96KH-FN. EXIT.
      *N96PH.    NOTE *PREPARE THE ERROR MESSAGE          *.            AAER85
       F96PH.                                                           lv15
      *--------------------------------                                 AAER85
      *    SET MESSAGE SEVERITY                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        11 TO MS03-CMESB                                 AAER85
      *--------------------------------                                 AAER85
      *GET THE ERROR MESSAGE TEXT                                       AAER85
      *--------------------------------                                 AAER85
           PERFORM     F98GM THRU F98GM-FN.                             AAER85
      *N96PM.    NOTE *IF ERROR MESSAGE NOT FOUND IN      *.            AAER85
       F96PM.    IF    MS03-CMSSF = 'UN'                                lv20
                 NEXT SENTENCE ELSE GO TO     F96PM-FN.                 AAER85
      *TBDU93, POPULATE DEFAULT ERROR                                   AAER85
      *MESSAGE'                                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        'Message info. not found' TO                     AAER85
           MS03-TMESS4.                                                 AAER85
       F96PM-FN. EXIT.
       F96PH-FN. EXIT.
      *N96PR.    NOTE *SET THE MESSAGE LENGTH AND         *.            AAER85
       F96PR.                                                           lv15
      *APPEND THE PROGRAM NAME AT THE                                   AAER85
      *END OF THE MESSAGE.                                              AAER85
      *--------------------------------                                 AAER85
           MOVE        128 TO MS03-QELLAA                               AAER85
           MOVE        ' Program : ' TO                                 AAER85
           MS03-TMESS4 (28:11)                                          AAER85
           MOVE        PROGE TO MS03-TMESS4 (39:8)                      AAER85
           MOVE        ' Reason ' TO MS03-TMESS4 (47:8).                AAER85
       F96PR-FN. EXIT.
       F96KB-FN. EXIT.
      *N96TB.    NOTE *DEPENDING ON THE TYPE OF ERROR     *.            AAER85
       F96TB.                                                           lv10
      *HANDLING REQUIRED, POPULATE THE                                  AAER85
      *ERROR CODE IN APPROPRIATE FIELD.                                 AAER85
      *--------------------------------                                 AAER85
           MOVE        'N' TO WS00-IGNORE-ERROR.                        AAER85
      *N96TG.    NOTE *FOR REQUEST TO IGNORE THE LIST     *.            AAER85
       F96TG.    IF    WS00-ERROR-TYPE =                                lv15
                       'L'                                              AAER85
                 NEXT SENTENCE ELSE GO TO     F96TG-FN.                 AAER85
      *OF THE ERRORS, SEARCH THE ERROR                                  AAER85
      *IN THE LIST.                                                     AAER85
      *--------------------------------                                 AAER85
           MOVE 1 TO     IWE00R.                                        AAER85
       F96TG-080. IF     IWE00R NOT >    IWE00L
           AND           WE00-CERRE1    (IWE00R)                        AAER85
           NOT =           WS00-CERRE1                                  AAER85
           ADD 1 TO      IWE00R    GO TO F96TG-080.                     AAER85
      *--------------------------------                                 AAER85
                 IF    IWE00R <= IWE00L                                 DOT
      *IF THE ERROR NEEDS TO IGNORE                                     AAER85
      *POPULATE THE WORKING STORAGE                                     AAER85
      *VARIABLE TO INDICATE IT.                                         AAER85
      *--------------------------------                                 AAER85
           MOVE        'Y' TO WS00-IGNORE-ERROR.                        AAER85
      *--------------------------------                                 DOT
       F96TG-900. GO TO F96TB-FN.
       F96TG-FN. EXIT.
      *N96TL.    NOTE *FOR IGNORE ALL THE ERROR           *.            AAER85
       F96TL.    IF    WS00-ERROR-TYPE =                                lv15
                       'A'                                              AAER85
                 NEXT SENTENCE ELSE GO TO     F96TL-FN.                 AAER85
      *REQUEST POPULATE THE WORKING                                     AAER85
      *STORAGE VARIABLE TO INDICATE                                     AAER85
      *IGNORABLE ERROR                                                  AAER85
      *--------------------------------                                 AAER85
           MOVE        'Y' TO WS00-IGNORE-ERROR.                        AAER85
       F96TL-900. GO TO F96TB-FN.
       F96TL-FN. EXIT.
      *N96TS.    NOTE *FOR IGNORE ALL APPLICATION AREA    *.            AAER85
       F96TS.    IF    WS00-ERROR-TYPE =                                lv15
                       'R'                                              AAER85
                 NEXT SENTENCE ELSE GO TO     F96TS-FN.                 AAER85
      *(RESPONSE) ERRORS.                                               AAER85
                 IF    WS00-ERROR-AREA NOT = 'B'                        DOT
                 AND   WS00-ERROR-AREA NOT = 'P'                        AAER85
      *--------------------------------                                 AAER85
      *FOR THE RESPONSE AREA ERRORS                                     AAER85
      *POPULATE THE WORKING STORAGE                                     AAER85
      *VARIABLE FOR IGNORABLE ERROR                                     AAER85
      *--------------------------------                                 AAER85
           MOVE        'Y' TO WS00-IGNORE-ERROR.                        AAER85
       F96TS-900. GO TO F96TB-FN.
       F96TS-FN. EXIT.
      *N96VB.    NOTE *FOR ANY OTHER TYPE OF ERRORS       *.            AAER85
       F96VB.                                                           lv15
      *HANDLING REQUEST DON'T IGNORE                                    AAER85
      *ERROR                                                            AAER85
       F96VB-FN. EXIT.
       F96TB-FN. EXIT.
       F96-FN.   EXIT.
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
      *N98BG.    NOTE *ROW NOT IN TAX LAW QUALIFIED       *.
       F98BG.    IF    K910-CSQLRC = +100                               lv15
                 NEXT SENTENCE ELSE GO TO     F98BG-FN.
      *AMOUNT REFERENCE TABLE
      *********************************
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
