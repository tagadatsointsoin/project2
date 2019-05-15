       IDENTIFICATION DIVISION.                                         CI0272
       PROGRAM-ID.  CI0272P.                                            CI0272
      *AUTHOR.         ANNUITIES UD BUILD ACTIVITY.                     CI0272
      *DATE-COMPILED.   09/08/14.                                       CI0272
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
       ENVIRONMENT DIVISION.                                            CI0272
       CONFIGURATION SECTION.                                           CI0272
       SOURCE-COMPUTER. IBM-370.                                        CI0272
       OBJECT-COMPUTER. IBM-370.                                        CI0272
       DATA DIVISION.                                                   CI0272
       WORKING-STORAGE SECTION.                                         CI0272
       01  AA10-CF           PIC X VALUE SPACE.
       01                 AA10.                                         CI0272
            10            AA10-AE00.                                    CI0272
            11            AA10-ALCIDN PICTURE  9(11).                   CI0272
            10            AA10-AE01.                                    CI0272
            11            AA10-FILLER PICTURE  X(12).                   CI0272
            11            AA10-DLAUP  PICTURE  9(8).                    CI0272
            11            AA10-FILLER PICTURE  S9(07)                   CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-FILLER PICTURE  9(8).                    CI0272
            11            AA10-FILLER PICTURE  S9(07)                   CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-FILLER PICTURE  9(8).                    CI0272
            11            AA10-FILLER PICTURE  S9(07)                   CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-FILLER PICTURE  X(311).                  CI0272
            10            AA10-AE02                                     CI0272
                          REDEFINES            AA10-AE01.               CI0272
            11            AA10-FILLER PICTURE  9.                       CI0272
            11            AA10-ALCOMP PICTURE  99.                      CI0272
            11            AA10-CRTYP  PICTURE  9(4).                    CI0272
            11            AA10-FILLER PICTURE  9.                       CI0272
            11            AA10-CPOST  PICTURE  99.                      CI0272
            11            AA10-GEHCDI PICTURE  9(3).                    CI0272
            11            AA10-FILLER PICTURE  9(8).                    CI0272
            11            AA10-FILLER PICTURE  9(7).                    CI0272
            11            AA10-FILLER PICTURE  X.                       CI0272
            11            AA10-ALPLDT PICTURE  9(8).                    CI0272
            11            AA10-DENEX  PICTURE  9(8).                    CI0272
            11            AA10-CENXC1 PICTURE  9(3).                    CI0272
            11            AA10-FILLER PICTURE  99.                      CI0272
            11            AA10-CROOR  PICTURE  99.                      CI0272
            11            AA10-CREIN  PICTURE  99.                      CI0272
            11            AA10-FILLER PICTURE  X.                       CI0272
            11            AA10-ALAPST PICTURE  99.                      CI0272
            11            AA10-ALSTSA PICTURE  XX.                      CI0272
            11            AA10-FILLER PICTURE  99.                      CI0272
            11            AA10-CPCAL  PICTURE  9.                       CI0272
            11            AA10-CNAEX  PICTURE  9.                       CI0272
            11            AA10-CSUSI  PICTURE  99.                      CI0272
            11            AA10-FILLER PICTURE  99.                      CI0272
            11            AA10-FILLER PICTURE  99.                      CI0272
            11            AA10-FILLER PICTURE  9(8).                    CI0272
            11            AA10-FILLER PICTURE  9.                       CI0272
            11            AA10-FILLER PICTURE  X(10).                   CI0272
            11            AA10-FILLER PICTURE  99.                      CI0272
            11            AA10-FILLER PICTURE  X.                       CI0272
            11            AA10-FILLER PICTURE  99.                      CI0272
            11            AA10-CAPL   PICTURE  9.                       CI0272
            11            AA10-FILLER PICTURE  9.                       CI0272
            11            AA10-FILLER PICTURE  999.                     CI0272
            11            AA10-FILLER PICTURE  999.                     CI0272
            11            AA10-FILLER PICTURE  999.                     CI0272
            11            AA10-CSTWH  PICTURE  9(8).                    CI0272
            11            AA10-FILLER PICTURE  9(8).                    CI0272
            11            AA10-FILLER PICTURE  99.                      CI0272
            11            AA10-FILLER PICTURE  9.                       CI0272
            11            AA10-FILLER PICTURE  9.                       CI0272
            11            AA10-FILLER PICTURE  99.                      CI0272
            11            AA10-FILLER PICTURE  999.                     CI0272
            11            AA10-FILLER PICTURE  999.                     CI0272
            11            AA10-CPRPM  PICTURE  9(3).                    CI0272
            11            AA10-FILLER PICTURE  99.                      CI0272
            11            AA10-FILLER PICTURE  9(6).                    CI0272
            11            AA10-FILLER PICTURE  99.                      CI0272
            11            AA10-FILLER PICTURE  999.                     CI0272
            11            AA10-DTRCM  PICTURE  9(8).                    CI0272
            11            AA10-DLATR  PICTURE  9(8).                    CI0272
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-ALPMOD PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-CRSBN  PICTURE  99.                      CI0272
            11            AA10-FILLER PICTURE  X.                       CI0272
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-FILLER PICTURE  9.                       CI0272
            11            AA10-FILLER PICTURE  X.                       CI0272
            11            AA10-FILLER PICTURE  XX.                      CI0272
            11            AA10-FILLER PICTURE  99.                      CI0272
            11            AA10-CTRHO  PICTURE  9(8).                    CI0272
            11            AA10-FILLER PICTURE  9.                       CI0272
            11            AA10-FILLER PICTURE  99.                      CI0272
            11            AA10-FILLER PICTURE  99.                      CI0272
            11            AA10-CTSGD  PICTURE  9(8).                    CI0272
            11            AA10-IANRD  PICTURE  9.                       CI0272
            11            AA10-ALINNO PICTURE  99.                      CI0272
            11            AA10-ALSANN PICTURE  9(5).                    CI0272
            11            AA10-FILLER PICTURE  S9(9)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-ALPAGR PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-ALRISK PICTURE  S9(9)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-ALAPIT PICTURE  S9(07)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-FILLER PICTURE  X.                       CI0272
            11            AA10-CADPR  PICTURE  9.                       CI0272
            11            AA10-AAPRT  PICTURE  S9(07)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-NREPN1 PICTURE  9(06).                   CI0272
            11            AA10-CCST1  PICTURE  9.                       CI0272
            11            AA10-CESRD  PICTURE  9(3).                    CI0272
            11            AA10-ALLRT  PICTURE  S9V99                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-ANTPAA PICTURE  S9(5)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-ALDDUE PICTURE  9(08).                   CI0272
            11            AA10-ALMODE PICTURE  99.                      CI0272
            11            AA10-FILLER PICTURE  X(08).                   CI0272
            11            AA10-CTCUS1 PICTURE  99.                      CI0272
            11            AA10-CNPPR  PICTURE  9(03).                   CI0272
            11            AA10-FILLER PICTURE  9.                       CI0272
            11            AA10-FILLER PICTURE  9(03).                   CI0272
            11            AA10-ITMEC  PICTURE  X(1).                    CI0272
            11            AA10-IMCDI  PICTURE  X.                       CI0272
            11            AA10-LSIDTE PICTURE  9(08).                   CI0272
            11            AA10-PLINE  PICTURE  S9V99                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-ATSA8  PICTURE  S9(07)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-ATSA9  PICTURE  S9(05)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-CRATS  PICTURE  X.                       CI0272
            11            AA10-PPTKN  PICTURE  S9(3)V9(6)               CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            AA10-FILLER PICTURE  X(24).                   CI0272
      *****************************************************************
      ** FIELD USED TO INDICATE IF TA5B ACCESS WAS SUCCESSFUL
      *****************************************************************
       01  TA5B-IK          PIC X(01).
      *
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0272
            10            XW05-XW06.                                    CI0272
            11            XW05-XDBPCB.                                  CI0272
            12            XW05-XDBDNM PICTURE  X(08)                    CI0272
                          VALUE                SPACE.                   CI0272
            12            XW05-XSEGLV PICTURE  X(02)                    CI0272
                          VALUE                SPACE.                   CI0272
            12            XW05-XRC    PICTURE  X(02)                    CI0272
                          VALUE                SPACE.                   CI0272
            12            XW05-XPROPT PICTURE  X(04)                    CI0272
                          VALUE                SPACE.                   CI0272
            12            XW05-FILLER PICTURE  S9(5)                    CI0272
                          VALUE                ZERO                     CI0272
                          BINARY.                                       CI0272
            12            XW05-XSEGNM PICTURE  X(08)                    CI0272
                          VALUE                SPACE.                   CI0272
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0272
                          VALUE                ZERO                     CI0272
                          BINARY.                                       CI0272
            12            XW05-XSEGNB PICTURE  9(05)                    CI0272
                          VALUE                ZERO                     CI0272
                          BINARY.                                       CI0272
            12            XW05-XCOKEY PICTURE  X(70)                    CI0272
                          VALUE                SPACE.                   CI0272
            10            XW05-XW07.                                    CI0272
            11            XW05-XIOPCB.                                  CI0272
            12            XW05-XTERMI PICTURE  X(08)                    CI0272
                          VALUE                SPACE.                   CI0272
            12            XW05-FILLER PICTURE  XX                       CI0272
                          VALUE                SPACE.                   CI0272
            12            XW05-XRC1   PICTURE  X(02)                    CI0272
                          VALUE                SPACE.                   CI0272
            12            XW05-FILLER PICTURE  X(12)                    CI0272
                          VALUE                SPACE.                   CI0272
            12            XW05-XMODNM PICTURE  X(8)                     CI0272
                          VALUE                SPACE.                   CI0272
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0272
                          VALUE                ZERO.                    CI0272
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0272
                          VALUE                ZERO.                    CI0272
            10            XW05-XGU    PICTURE  X(4)                     CI0272
                          VALUE                'GU  '.                  CI0272
            10            XW05-XGHU   PICTURE  X(4)                     CI0272
                          VALUE                'GHU '.                  CI0272
            10            XW05-XGN    PICTURE  X(4)                     CI0272
                          VALUE                'GN  '.                  CI0272
            10            XW05-XGHN   PICTURE  X(4)                     CI0272
                          VALUE                'GHN '.                  CI0272
            10            XW05-XGNP   PICTURE  X(4)                     CI0272
                          VALUE                'GNP '.                  CI0272
            10            XW05-XGHNP  PICTURE  X(4)                     CI0272
                          VALUE                'GHNP'.                  CI0272
            10            XW05-XREPL  PICTURE  XXXX                     CI0272
                          VALUE                'REPL'.                  CI0272
            10            XW05-XISRT  PICTURE  X(4)                     CI0272
                          VALUE                'ISRT'.                  CI0272
            10            XW05-XDLET  PICTURE  X(4)                     CI0272
                          VALUE                'DLET'.                  CI0272
            10            XW05-XOPEN  PICTURE  X(4)                     CI0272
                          VALUE                'OPEN'.                  CI0272
            10            XW05-XCLSE  PICTURE  X(4)                     CI0272
                          VALUE                'CLSE'.                  CI0272
            10            XW05-XCHKP  PICTURE  X(4)                     CI0272
                          VALUE                'CHKP'.                  CI0272
            10            XW05-XXRST  PICTURE  X(4)                     CI0272
                          VALUE                'XRST'.                  CI0272
            10            XW05-XTERM  PICTURE  X(4)                     CI0272
                          VALUE                'TERM'.                  CI0272
            10            XW05-XNFPAC PICTURE  X(13)                    CI0272
                          VALUE                SPACE.                   CI0272
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0272
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0272
       01                 GC01.                                         CI0272
            10            GC01-GC01K.                                   CI0272
            11            GC01-C299.                                    CI0272
            12            GC01-CTID.                                    CI0272
            13            GC01-CTIDA  PICTURE  9(3).                    CI0272
            13            GC01-CTIDN.                                   CI0272
            14            GC01-CTIDNP PICTURE  X(13).                   CI0272
            14            GC01-CTIDND PICTURE  9(11).                   CI0272
            10            GC01-DCAG9L PICTURE  9(8).                    CI0272
            10            GC01-NAASQL PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            GC01-ICUST  PICTURE  X.                       CI0272
            10            GC01-NSEQ4B PICTURE  9(8)                     CI0272
                          BINARY.                                       CI0272
            10            GC01-PRCOD  PICTURE  9(5).                    CI0272
            10            GC01-PRSCD  PICTURE  X(9).                    CI0272
            10            GC01-FILLER PICTURE  X(8).                    CI0272
       01                 GC03.                                         CI0272
            10            GC03-GELL   PICTURE  9(4)                     CI0272
                          BINARY.                                       CI0272
            10            GC03-GD00.                                    CI0272
            11            GC03-GC03K.                                   CI0272
            12            GC03-DCACG9 PICTURE  9(8).                    CI0272
            12            GC03-NAASQ  PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-CAATY  PICTURE  9(3).                    CI0272
            11            GC03-CVSYS  PICTURE  X(2).                    CI0272
            11            GC03-CACTO  PICTURE  9(3).                    CI0272
            11            GC03-CATRN.                                   CI0272
            12            GC03-CATRF  PICTURE  9(3).                    CI0272
            12            GC03-CATRS  PICTURE  9(3).                    CI0272
            11            GC03-CASTC  PICTURE  99.                      CI0272
            11            GC03-IPULL  PICTURE  X.                       CI0272
            11            GC03-GEAUN  PICTURE  9(5).                    CI0272
            11            GC03-GEOPD2 PICTURE  X(8).                    CI0272
            11            GC03-NBTCH  PICTURE  9(4).                    CI0272
            11            GC03-DEFFT  PICTURE  9(8).                    CI0272
            11            GC03-NSUNT  PICTURE  9(4).                    CI0272
            11            GC03-ITRAN  PICTURE  X.                       CI0272
            11            GC03-DLAUP1 PICTURE  9(8).                    CI0272
            11            GC03-ADRET  PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-TTRMS  PICTURE  X(12).                   CI0272
            11            GC03-IDELT  PICTURE  X.                       CI0272
            11            GC03-GEOPDM PICTURE  X(8).                    CI0272
            11            GC03-FILLER PICTURE  X(07).                   CI0272
            10            GC03-GD09.                                    CI0272
            11            GC03-FILLER PICTURE  X(70).                   CI0272
            10            GC03-GD01                                     CI0272
                          REDEFINES            GC03-GD09.               CI0272
            11            GC03-ADBRQ  PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-CTRTP  PICTURE  X(2).                    CI0272
            11            GC03-CPORT  PICTURE  X.                       CI0272
            11            GC03-CSCRNU PICTURE  X(4).                    CI0272
            11            GC03-DLAUP  PICTURE  9(8).                    CI0272
            11            GC03-CTWHAT PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-PWHLD  PICTURE  S999V9(5)                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-IWTHH  PICTURE  X.                       CI0272
            11            GC03-NDRFT  PICTURE  9(5).                    CI0272
            11            GC03-IDPAP  PICTURE  X.                       CI0272
            11            GC03-GETIM  PICTURE  S9(7)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-QNACT  PICTURE  9(3).                    CI0272
            11            GC03-AEDRQ  PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-IPLIN  PICTURE  X.                       CI0272
            11            GC03-CLIDNB PICTURE  9(8).                    CI0272
            11            GC03-CSLCT  PICTURE  X.                       CI0272
            11            GC03-ITELE  PICTURE  X.                       CI0272
            11            GC03-FILLER PICTURE  X(06).                   CI0272
            10            GC03-GD02                                     CI0272
                          REDEFINES            GC03-GD09.               CI0272
            11            GC03-CSYST  PICTURE  99.                      CI0272
            11            GC03-FILLER PICTURE  X.                       CI0272
            11            GC03-ACASH  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-DTRAC  PICTURE  9(8).                    CI0272
            11            GC03-CTRSO  PICTURE  9(02).                   CI0272
            11            GC03-NTRCE  PICTURE  9(06).                   CI0272
            11            GC03-GECKD1 PICTURE  9.                       CI0272
            11            GC03-CCOLL  PICTURE  X(3).                    CI0272
            11            GC03-CLTDP  PICTURE  X(3).                    CI0272
            11            GC03-PSLLD  PICTURE  S99V999                  CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-ISLOR  PICTURE  X.                       CI0272
            11            GC03-ITPAC  PICTURE  X.                       CI0272
            11            GC03-CPMTCA PICTURE  XXX.                     CI0272
            11            GC03-CSERV  PICTURE  X(3).                    CI0272
            11            GC03-ACOMO  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-IPLIN1 PICTURE  X.                       CI0272
            11            GC03-INQEX  PICTURE  X.                       CI0272
            11            GC03-CTKRAA PICTURE  X(12).                   CI0272
            11            GC03-CCSMQ  PICTURE  X.                       CI0272
            11            GC03-IVAEX1 PICTURE  X.                       CI0272
            11            GC03-IHPMT  PICTURE  X(1).                    CI0272
            11            GC03-GETIM3 PICTURE  S9(7)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            GC03-GD03                                     CI0272
                          REDEFINES            GC03-GD09.               CI0272
            11            GC03-CATRNC PICTURE  9(6).                    CI0272
            11            GC03-APRNT1 PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-QSHOWT PICTURE  S9(10)V999               CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-ACINVT PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-ACOMO7 PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-QSHOMW PICTURE  S9(10)V999               CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-ATAXT3 PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-CTSTR  PICTURE  9(2).                    CI0272
            11            GC03-ICIRA  PICTURE  X.                       CI0272
            11            GC03-GETIM2 PICTURE  S9(7)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-CPMTCX PICTURE  XX.                      CI0272
            11            GC03-FILLER PICTURE  X(16).                   CI0272
            10            GC03-GD99.                                    CI0272
            11            GC03-FILLER PICTURE  X(248).                  CI0272
            10            GC03-GD10                                     CI0272
                          REDEFINES            GC03-GD99.               CI0272
            11            GC03-MROTC  PICTURE  X(7).                    CI0272
            11            GC03-CEDSC  PICTURE  9(1).                    CI0272
            11            GC03-ILPOI  PICTURE  X(1).                    CI0272
            11            GC03-AWRCH  PICTURE  S9(3)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-CHCOC1 PICTURE  9(2).                    CI0272
            11            GC03-CHCOC2 PICTURE  9(2).                    CI0272
            11            GC03-CHCOC3 PICTURE  9(2).                    CI0272
            11            GC03-CHCOC4 PICTURE  9(2).                    CI0272
            11            GC03-CMCOC1 PICTURE  9(3).                    CI0272
            11            GC03-CMCOC2 PICTURE  9(3).                    CI0272
            11            GC03-CMCOC3 PICTURE  9(3).                    CI0272
            11            GC03-GD11.                                    CI0272
            12            GC03-FILLER PICTURE  X(219).                  CI0272
            11            GC03-GD12                                     CI0272
                          REDEFINES            GC03-GD11.               CI0272
            12            GC03-CELLO  PICTURE  9(1).                    CI0272
            12            GC03-CECLO  PICTURE  9(1).                    CI0272
            12            GC03-AEXML  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-CEPI   PICTURE  X(1).                    CI0272
            12            GC03-CEXTY  PICTURE  X.                       CI0272
            12            GC03-CROPC  PICTURE  9(1).                    CI0272
            12            GC03-CPUTY  PICTURE  9(1).                    CI0272
            12            GC03-IMCII  PICTURE  X(1).                    CI0272
            12            GC03-GEMISC                                   CI0272
                          OCCURS       010     TIMES.                   CI0272
            13            GC03-AMGLA  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            13            GC03-CMGLC  PICTURE  9(1).                    CI0272
            13            GC03-NMGLN  PICTURE  9(4).                    CI0272
            12            GC03-ACTRN  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-IWRBK  PICTURE  X.                       CI0272
            12            GC03-IFEDX  PICTURE  X.                       CI0272
            12            GC03-ICNTR  PICTURE  X.                       CI0272
            12            GC03-IOCKH  PICTURE  X.                       CI0272
            12            GC03-ICRCK  PICTURE  X.                       CI0272
            12            GC03-NHMPN  PICTURE  S9(10)                   CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-ITELR1 PICTURE  X.                       CI0272
            11            GC03-GD13                                     CI0272
                          REDEFINES            GC03-GD11.               CI0272
            12            GC03-DREDO  PICTURE  9(8).                    CI0272
            12            GC03-CATRNR PICTURE  9(6).                    CI0272
            12            GC03-CEVN   PICTURE  9(9).                    CI0272
            12            GC03-ISUSP  PICTURE  X(1).                    CI0272
            11            GC03-GD15                                     CI0272
                          REDEFINES            GC03-GD11.               CI0272
            12            GC03-CPUTZ  PICTURE  9(1).                    CI0272
            12            GC03-CETLB  PICTURE  9(3).                    CI0272
            12            GC03-QTRMC  PICTURE  9(3).                    CI0272
            12            GC03-DEFFTE PICTURE  9(8).                    CI0272
            12            GC03-DEFFTF PICTURE  9(8).                    CI0272
            12            GC03-DEFFTG PICTURE  9(8).                    CI0272
            12            GC03-XZ1A   PICTURE  X.                       CI0272
            12            GC03-XZ1B   PICTURE  X.                       CI0272
            12            GC03-XZ1C   PICTURE  X.                       CI0272
            12            GC03-XZ1D   PICTURE  X.                       CI0272
            12            GC03-XZ1E   PICTURE  X.                       CI0272
            12            GC03-XZ1F   PICTURE  X.                       CI0272
            12            GC03-XZ1G   PICTURE  X.                       CI0272
            12            GC03-XZ1H   PICTURE  X.                       CI0272
            12            GC03-XZ1I   PICTURE  X.                       CI0272
            12            GC03-DEFFTH PICTURE  9(8).                    CI0272
            11            GC03-GD19                                     CI0272
                          REDEFINES            GC03-GD11.               CI0272
            12            GC03-GD11.                                    CI0272
            13            GC03-FILLER PICTURE  X(219).                  CI0272
            10            GC03-GD20                                     CI0272
                          REDEFINES            GC03-GD99.               CI0272
            11            GC03-ADDACT PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-ISIGV  PICTURE  X.                       CI0272
            11            GC03-IALLF  PICTURE  X.                       CI0272
            11            GC03-QSHOWQ PICTURE  S9(9)V999                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-CCDSCW PICTURE  9(2).                    CI0272
            11            GC03-IDWRL  PICTURE  X.                       CI0272
            11            GC03-ITELR  PICTURE  X.                       CI0272
            11            GC03-IABIN  PICTURE  X.                       CI0272
            11            GC03-PACT1  PICTURE  S999V999                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-IBFAF  PICTURE  X.                       CI0272
            11            GC03-IFRSA  PICTURE  X.                       CI0272
            11            GC03-ICRCAN PICTURE  X.                       CI0272
            11            GC03-ACACTV PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-AGFND  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-QCSHOW PICTURE  S9(9)V999                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-QCSHIS PICTURE  S9(9)V999                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-NDTRC  PICTURE  9(8).                    CI0272
            11            GC03-CAERU  PICTURE  X(4).                    CI0272
            11            GC03-IFDGO  PICTURE  X.                       CI0272
            11            GC03-PSLLD2 PICTURE  S99V999                  CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-ISLOR2 PICTURE  X.                       CI0272
            11            GC03-QSFIO  PICTURE  S9(10)V999               CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-QSFID  PICTURE  S9(10)V999               CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-CGDIN  PICTURE  X.                       CI0272
            11            GC03-DGDIN  PICTURE  9(8).                    CI0272
            10            GC03-GD30                                     CI0272
                          REDEFINES            GC03-GD99.               CI0272
            11            GC03-ISKED  PICTURE  X.                       CI0272
            11            GC03-CENXC  PICTURE  9(2).                    CI0272
            11            GC03-GD31.                                    CI0272
            12            GC03-FILLER PICTURE  X(245).                  CI0272
            11            GC03-GD32                                     CI0272
                          REDEFINES            GC03-GD31.               CI0272
            12            GC03-IABIN1 PICTURE  X.                       CI0272
            12            GC03-CLDOD  PICTURE  9(8).                    CI0272
            12            GC03-NCLAM  PICTURE  9(5)                     CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-ISURR  PICTURE  X.                       CI0272
            12            GC03-GEHCD  PICTURE  9(3).                    CI0272
            12            GC03-CRATC  PICTURE  9(4).                    CI0272
            12            GC03-AMAXD  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-ASCHGA PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-APYOM  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-IWTHH1 PICTURE  X.                       CI0272
            12            GC03-CPAYCL PICTURE  X(2).                    CI0272
            12            GC03-CTSAO  PICTURE  X.                       CI0272
            12            GC03-NCONF  PICTURE  9(08).                   CI0272
            12            GC03-CLID   PICTURE  X(23).                   CI0272
            12            GC03-CARTY  PICTURE  99.                      CI0272
            12            GC03-NARRS  PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-CARTZ  PICTURE  99.                      CI0272
            12            GC03-NAPDS  PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-CPMTO  PICTURE  X.                       CI0272
            12            GC03-DNPMT  PICTURE  9(8).                    CI0272
            12            GC03-IPCTV  PICTURE  X.                       CI0272
            12            GC03-IMECH  PICTURE  X(01).                   CI0272
            12            GC03-IMVAO  PICTURE  X(1).                    CI0272
            12            GC03-AMVA1  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-CACTS  PICTURE  X.                       CI0272
            12            GC03-CTSPP  PICTURE  X(1).                    CI0272
            12            GC03-CACT4  PICTURE  X(2).                    CI0272
            12            GC03-IVAEX  PICTURE  X.                       CI0272
            12            GC03-DFPMT  PICTURE  9(8).                    CI0272
            12            GC03-IDEMD  PICTURE  X.                       CI0272
            12            GC03-IOFST  PICTURE  X.                       CI0272
            12            GC03-AMXLB  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-ACULB  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-DEIRNB PICTURE  9(8).                    CI0272
            12            GC03-DEFFE  PICTURE  9(8).                    CI0272
            12            GC03-DEFFR  PICTURE  9(8).                    CI0272
            12            GC03-ISPUP  PICTURE  X.                       CI0272
            12            GC03-CPNCG  PICTURE  X.                       CI0272
            12            GC03-IEXPU  PICTURE  X.                       CI0272
            12            GC03-IPPCF  PICTURE  X.                       CI0272
            12            GC03-NAAPT  PICTURE  9(2).                    CI0272
            12            GC03-PWHLDS PICTURE  S999V9(5)                CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-ISWHO  PICTURE  X(1).                    CI0272
            11            GC03-GD33                                     CI0272
                          REDEFINES            GC03-GD31.               CI0272
            12            GC03-CPAYC  PICTURE  X(2).                    CI0272
            12            GC03-ADBRQX PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-ADBRQV PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-APTXR  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-CTRTPE PICTURE  X(2).                    CI0272
            12            GC03-NCLAMI PICTURE  S9(9)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-CLIDO8 PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-CLIDN  PICTURE  X(20).                   CI0272
            12            GC03-DSET01 PICTURE  S9(8)                    CI0272
                          BINARY.                                       CI0272
            12            GC03-CTSET1 PICTURE  S9(6)                    CI0272
                          BINARY.                                       CI0272
            12            GC03-DSET02 PICTURE  S9(8)                    CI0272
                          BINARY.                                       CI0272
            12            GC03-CTSET2 PICTURE  S9(6)                    CI0272
                          BINARY.                                       CI0272
            11            GC03-GD34                                     CI0272
                          REDEFINES            GC03-GD31.               CI0272
            12            GC03-QNOFM  PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-CLTRM  PICTURE  99.                      CI0272
            12            GC03-AMXLN  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-ALADJ  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-ACHK   PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-APRMO  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-IMECH1 PICTURE  X(01).                   CI0272
            12            GC03-CACT41 PICTURE  X(2).                    CI0272
            12            GC03-ACDSCC PICTURE  S9(05)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-ACDSCD PICTURE  S9(05)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-GD39                                     CI0272
                          REDEFINES            GC03-GD31.               CI0272
            12            GC03-GD31.                                    CI0272
            13            GC03-FILLER PICTURE  X(245).                  CI0272
            10            GC03-GD40                                     CI0272
                          REDEFINES            GC03-GD99.               CI0272
            11            GC03-NTR    PICTURE  9(8).                    CI0272
            11            GC03-NPBNC  PICTURE  X(24).                   CI0272
            11            GC03-CRREV  PICTURE  X(3).                    CI0272
            11            GC03-CSUSL  PICTURE  S9.                      CI0272
            11            GC03-NMGLN1 PICTURE  9(4).                    CI0272
            11            GC03-DCAC92 PICTURE  9(8).                    CI0272
            11            GC03-NAASQ3 PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-GD49.                                    CI0272
            12            GC03-FILLER PICTURE  X(198).                  CI0272
            11            GC03-GD41                                     CI0272
                          REDEFINES            GC03-GD49.               CI0272
            12            GC03-CRREF  PICTURE  9(2).                    CI0272
            12            GC03-CORIR  PICTURE  X(02).                   CI0272
            12            GC03-CIPDB  PICTURE  X(03).                   CI0272
            12            GC03-CPAYH  PICTURE  X(02).                   CI0272
            12            GC03-NAMEX  PICTURE  9(15)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC03-DCHAE  PICTURE  9(4).                    CI0272
            12            GC03-DRQST  PICTURE  S9(8)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-GD42                                     CI0272
                          REDEFINES            GC03-GD49.               CI0272
            12            GC03-CPMTCB PICTURE  X(3).                    CI0272
            10            GC03-GD50                                     CI0272
                          REDEFINES            GC03-GD99.               CI0272
            11            GC03-ALOAD  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-PSLLD4 PICTURE  S99V999                  CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-CSUSL1 PICTURE  S9.                      CI0272
            11            GC03-CRREV1 PICTURE  X(3).                    CI0272
            11            GC03-ADDAC  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-DL13.                                    CI0272
            12            GC03-GEYR   PICTURE  9(4).                    CI0272
            12            GC03-GEMTH  PICTURE  99.                      CI0272
            12            GC03-NDAY   PICTURE  99.                      CI0272
            11            GC03-NSEQ3P PICTURE  S9(5)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-XZ6A   PICTURE  X(6).                    CI0272
            11            GC03-XZ7    PICTURE  X(7).                    CI0272
            11            GC03-XZ6B   PICTURE  X(6).                    CI0272
            11            GC03-XZ6    PICTURE  X(6).                    CI0272
            11            GC03-XZ6C   PICTURE  X(6).                    CI0272
            11            GC03-XZ20   PICTURE  X(20).                   CI0272
            11            GC03-CATRN1 PICTURE  9(6).                    CI0272
            11            GC03-ADDAC2 PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-ATAXT2 PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-ACOMOT PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-XZ5    PICTURE  X(5).                    CI0272
            11            GC03-IREVD  PICTURE  X(1).                    CI0272
            11            GC03-ISUSP1 PICTURE  X(1).                    CI0272
            11            GC03-XZ6D   PICTURE  X(6).                    CI0272
            11            GC03-XZ13   PICTURE  X(13).                   CI0272
            11            GC03-CWHTP2 PICTURE  X(3).                    CI0272
            11            GC03-CWHTP3 PICTURE  X(3).                    CI0272
            11            GC03-DTREN  PICTURE  9(8).                    CI0272
            11            GC03-NAASQ1 PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            GC03-GD51                                     CI0272
                          REDEFINES            GC03-GD99.               CI0272
            11            GC03-ADOMOT PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-ACGLT  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-ACGST  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-CTXMT  PICTURE  9(2).                    CI0272
            11            GC03-ALOAD3 PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-FILLER PICTURE  X(31).                   CI0272
            10            GC03-GD52                                     CI0272
                          REDEFINES            GC03-GD99.               CI0272
            11            GC03-DEFFT5 PICTURE  9(8).                    CI0272
            11            GC03-PSLLD5 PICTURE  S99V999                  CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-CSUSL2 PICTURE  S9.                      CI0272
            11            GC03-ALOAD2 PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-DL22.                                    CI0272
            12            GC03-NYEAR1 PICTURE  9(4).                    CI0272
            12            GC03-GEMTHA PICTURE  99.                      CI0272
            12            GC03-NDAY01 PICTURE  99.                      CI0272
            11            GC03-NSEQ3R PICTURE  S9(5)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-CWHTP  PICTURE  X(3).                    CI0272
            11            GC03-CWHFR  PICTURE  X(3).                    CI0272
            11            GC03-CATRN7 PICTURE  9(6).                    CI0272
            11            GC03-ATAXT5 PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-QSHOT  PICTURE  S9(10)V999               CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-ACINT3 PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-CWHTP1 PICTURE  X(3).                    CI0272
            11            GC03-CWHFR1 PICTURE  X(3).                    CI0272
            11            GC03-ACOMO5 PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-QSHOMU PICTURE  S9(10)V999               CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-ACASH1 PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-FILLER PICTURE  X(04).                   CI0272
            11            GC03-CATRN8 PICTURE  9(6).                    CI0272
            11            GC03-ALOAD1 PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-PSLLD1 PICTURE  S99V999                  CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-QSHOT1 PICTURE  S9(10)V999               CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-ACINT4 PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-CSUSL4 PICTURE  S9.                      CI0272
            11            GC03-ACOMO4 PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            GC03-GD60                                     CI0272
                          REDEFINES            GC03-GD99.               CI0272
            11            GC03-GEOPDD PICTURE  X(8)                     CI0272
                          OCCURS       005     TIMES.                   CI0272
            11            GC03-DLAUP3 PICTURE  9(8)                     CI0272
                          OCCURS       005     TIMES.                   CI0272
            11            GC03-GEOPDB PICTURE  X(8).                    CI0272
            11            GC03-DLAUP4 PICTURE  9(8).                    CI0272
            11            GC03-ITELR2 PICTURE  X.                       CI0272
            11            GC03-IPMTA  PICTURE  X.                       CI0272
            11            GC03-CCSMG  PICTURE  X.                       CI0272
            11            GC03-CPLEC  PICTURE  XX.                      CI0272
            11            GC03-CORTYA PICTURE  X(3).                    CI0272
            11            GC03-CACTBC PICTURE  X(1).                    CI0272
            11            GC03-CGSPIA PICTURE  X.                       CI0272
            11            GC03-IPTRDA PICTURE  X(01).                   CI0272
            11            GC03-GCUSPY PICTURE  X(12).                   CI0272
            11            GC03-CPALLA PICTURE  X(1).                    CI0272
            11            GC03-QSHO5A PICTURE  S9(9)V999                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-IFRSAB PICTURE  X.                       CI0272
            11            GC03-DELOI  PICTURE  9(8).                    CI0272
            11            GC03-IAROAA PICTURE  X.                       CI0272
            11            GC03-ACINVR PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-ILTINA PICTURE  X.                       CI0272
            11            GC03-ALOIDA PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC03-CFUNTA PICTURE  X(2).                    CI0272
            11            GC03-CLGND  PICTURE  X.                       CI0272
            11            GC03-CPH3U  PICTURE  X.                       CI0272
            11            GC03-GESTD  PICTURE  9(8).                    CI0272
            11            GC03-GEEND  PICTURE  9(8).                    CI0272
            11            GC03-CPMTF  PICTURE  99.                      CI0272
            11            GC03-CNAVR  PICTURE  X(1).                    CI0272
            10            GC03-GD70                                     CI0272
                          REDEFINES            GC03-GD99.               CI0272
            11            GC03-CMEMO  PICTURE  X(2).                    CI0272
            11            GC03-ALPLDT PICTURE  9(8).                    CI0272
            11            GC03-CTLPD  PICTURE  9(8).                    CI0272
            11            GC03-CPAYCM PICTURE  X(2).                    CI0272
       01                 GC06.                                         CI0272
            10            GC06-GELL   PICTURE  9(4)                     CI0272
                          BINARY.                                       CI0272
            10            GC06-GE00.                                    CI0272
            11            GC06-GC06K.                                   CI0272
            12            GC06-NPISQ  PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC06-ACOTD  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC06-PPOTD  PICTURE  S9(3)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC06-QPSTD  PICTURE  S9(7)V999                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC06-CPITC  PICTURE  99.                      CI0272
            11            GC06-ITRNB  PICTURE  X.                       CI0272
            11            GC06-FILLER PICTURE  X(14).                   CI0272
            10            GC06-GE98.                                    CI0272
            11            GC06-FILLER PICTURE  X(240).                  CI0272
            10            GC06-GE10                                     CI0272
                          REDEFINES            GC06-GE98.               CI0272
            11            GC06-CDELI  PICTURE  9(3).                    CI0272
            11            GC06-CPAYC  PICTURE  X(2).                    CI0272
            11            GC06-ICHKP  PICTURE  X.                       CI0272
            11            GC06-CLTIN  PICTURE  9(12).                   CI0272
            11            GC06-IFHAI  PICTURE  X.                       CI0272
            11            GC06-CDQUA  PICTURE  X(2).                    CI0272
            11            GC06-FILLER PICTURE  X(07).                   CI0272
            11            GC06-GE99.                                    CI0272
            12            GC06-FILLER PICTURE  X(212).                  CI0272
            11            GC06-GE01                                     CI0272
                          REDEFINES            GC06-GE99.               CI0272
            12            GC06-NTR    PICTURE  9(8).                    CI0272
            12            GC06-GECKD  PICTURE  9.                       CI0272
            12            GC06-NPBN   PICTURE  X(20).                   CI0272
            12            GC06-CCBAT  PICTURE  99.                      CI0272
            12            GC06-CLID4  PICTURE  X(23).                   CI0272
            12            GC06-GENAL1 PICTURE  X(30)                    CI0272
                          OCCURS       002     TIMES.                   CI0272
            12            GC06-GESAD1 PICTURE  X(30)                    CI0272
                          OCCURS       003     TIMES.                   CI0272
            11            GC06-GE02                                     CI0272
                          REDEFINES            GC06-GE99.               CI0272
            12            GC06-GENAL  PICTURE  X(30)                    CI0272
                          OCCURS       002     TIMES.                   CI0272
            12            GC06-GESAD  PICTURE  X(30)                    CI0272
                          OCCURS       003     TIMES.                   CI0272
            11            GC06-GE03                                     CI0272
                          REDEFINES            GC06-GE99.               CI0272
            12            GC06-NCHKN  PICTURE  9(11).                   CI0272
            11            GC06-GE04                                     CI0272
                          REDEFINES            GC06-GE99.               CI0272
            12            GC06-CTIDAP PICTURE  9(3).                    CI0272
            12            GC06-PRCOD  PICTURE  9(5).                    CI0272
            12            GC06-TDELI  PICTURE  X(30).                   CI0272
            12            GC06-CINCD  PICTURE  9(02).                   CI0272
            10            GC06-GE20                                     CI0272
                          REDEFINES            GC06-GE98.               CI0272
            11            GC06-C299.                                    CI0272
            12            GC06-CTID.                                    CI0272
            13            GC06-CTIDA  PICTURE  9(3).                    CI0272
            13            GC06-CTIDN.                                   CI0272
            14            GC06-CTIDNP PICTURE  X(13).                   CI0272
            14            GC06-CTIDND PICTURE  9(11).                   CI0272
            11            GC06-DCACG9 PICTURE  9(8).                    CI0272
            11            GC06-NAASQ  PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            GC06-CIRAP  PICTURE  XX.                      CI0272
            11            GC06-CTYPE  PICTURE  X.                       CI0272
            11            GC06-INACT  PICTURE  X.                       CI0272
            11            GC06-FILLER PICTURE  X(01).                   CI0272
            11            GC06-ITPAC  PICTURE  X.                       CI0272
            11            GC06-ITAXI  PICTURE  X.                       CI0272
            11            GC06-IOWNC  PICTURE  X.                       CI0272
            11            GC06-CDVCD  PICTURE  X(2).                    CI0272
            11            GC06-CTCUS  PICTURE  999.                     CI0272
            11            GC06-CPMTCB PICTURE  X(3).                    CI0272
            11            GC06-CASTC1 PICTURE  99.                      CI0272
            11            GC06-PRCOD1 PICTURE  9(5).                    CI0272
            11            GC06-CPRSC1 PICTURE  X(9).                    CI0272
            11            GC06-CPRTB  PICTURE  X.                       CI0272
            11            GC06-CBRKD  PICTURE  9(4).                    CI0272
            11            GC06-FILLER PICTURE  X(12).                   CI0272
            10            GC06-GE30                                     CI0272
                          REDEFINES            GC06-GE98.               CI0272
            11            GC06-CFIDC  PICTURE  X(5).                    CI0272
            11            GC06-CPHSE  PICTURE  9(2).                    CI0272
            11            GC06-FILLER PICTURE  X(05).                   CI0272
            11            GC06-IABIN  PICTURE  X.                       CI0272
            11            GC06-PDFND  PICTURE  S999V9(3)                CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            GC06-GE40                                     CI0272
                          REDEFINES            GC06-GE98.               CI0272
            11            GC06-CACCT  PICTURE  X.                       CI0272
            11            GC06-CPAYR  PICTURE  X(2).                    CI0272
            11            GC06-CDELI1 PICTURE  9(3).                    CI0272
            11            GC06-CATRN.                                   CI0272
            12            GC06-CATRF  PICTURE  9(3).                    CI0272
            12            GC06-CATRS  PICTURE  9(3).                    CI0272
            11            GC06-DEFFT  PICTURE  9(8).                    CI0272
            11            GC06-CTYPC  PICTURE  X.                       CI0272
            11            GC06-CIRAPA PICTURE  XX.                      CI0272
            11            GC06-FILLER PICTURE  X(09).                   CI0272
            11            GC06-GE49.                                    CI0272
            12            GC06-FILLER PICTURE  X(208).                  CI0272
            11            GC06-GE41                                     CI0272
                          REDEFINES            GC06-GE49.               CI0272
            12            GC06-NCHKN1 PICTURE  9(6).                    CI0272
            11            GC06-GE42                                     CI0272
                          REDEFINES            GC06-GE49.               CI0272
            12            GC06-CTID1.                                   CI0272
            13            GC06-CTIDA1 PICTURE  9(3).                    CI0272
            13            GC06-CTIDP1 PICTURE  X(13).                   CI0272
            13            GC06-CTIDN1 PICTURE  9(11).                   CI0272
            11            GC06-GE43                                     CI0272
                          REDEFINES            GC06-GE49.               CI0272
            12            GC06-GENAL2 PICTURE  X(30)                    CI0272
                          OCCURS       002     TIMES.                   CI0272
            12            GC06-GESAD2 PICTURE  X(30)                    CI0272
                          OCCURS       003     TIMES.                   CI0272
            11            GC06-GE44                                     CI0272
                          REDEFINES            GC06-GE49.               CI0272
            12            GC06-CTID01.                                  CI0272
            13            GC06-CTIDA6 PICTURE  9(3).                    CI0272
            13            GC06-NTIDP2 PICTURE  X(13).                   CI0272
            13            GC06-CTIDN2 PICTURE  9(11).                   CI0272
            12            GC06-GECKD2 PICTURE  9.                       CI0272
            12            GC06-PACCT  PICTURE  S999V99                  CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC06-PLOAN  PICTURE  S999V99                  CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC06-PADPT  PICTURE  S999V99                  CI0272
                          COMPUTATIONAL-3.                              CI0272
            12            GC06-IPCTL  PICTURE  X.                       CI0272
            12            GC06-IPCTP  PICTURE  X.                       CI0272
            12            GC06-CEUNT  PICTURE  S9(5)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            GC06-GE31                                     CI0272
                          REDEFINES            GC06-GE98.               CI0272
            11            GC06-GCUSPZ PICTURE  X(12).                   CI0272
       01                 GC18.                                         CI0272
            10            GC18-GC18K.                                   CI0272
            11            GC18-GESQ2  PICTURE  99.                      CI0272
            10            GC18-FILLER PICTURE  X(01).                   CI0272
            10            GC18-CFIDC  PICTURE  X(5).                    CI0272
            10            GC18-CPHSE  PICTURE  9(2).                    CI0272
            10            GC18-AACTVF PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            GC18-IBALF  PICTURE  X.                       CI0272
            10            GC18-IABIN  PICTURE  X.                       CI0272
            10            GC18-ADBRQF PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            GC18-QSHOWP PICTURE  S9(7)V999                CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            GC18-PDFND  PICTURE  S999V9(3)                CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            GC18-PFNDV  PICTURE  S999V9(3)                CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            GC18-FILLER PICTURE  X(05).                   CI0272
       01                 GC29.                                         CI0272
            10            GC29-CSLCT  PICTURE  X.                       CI0272
            10            GC29-NGEOR  PICTURE  9(08).                   CI0272
            10            GC29-CACLS2 PICTURE  X(20).                   CI0272
            10            GC29-CAPID  PICTURE  9(2).                    CI0272
            10            GC29-NGEOPA PICTURE  X(08).                   CI0272
            10            GC29-CACLS1 PICTURE  X(20).                   CI0272
            10            GC29-CTRHO  PICTURE  9(8).                    CI0272
            10            GC29-GETIM3 PICTURE  S9(7)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            GC29-GEOPD9 PICTURE  X(8).                    CI0272
            10            GC29-DCACG1 PICTURE  9(8).                    CI0272
            10            GC29-IWEBBT PICTURE  X.                       CI0272
            10            GC29-CAVER  PICTURE  X.                       CI0272
            10            GC29-FILLER PICTURE  X(30).                   CI0272
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
      *     PMS AAOLMD WORK AREA                                        AAOLMD
       01   7-CTID.                                                     AAOLMD
            05  FILLER        PIC  X(3).                                AAOLMD
            05  FILLER        PIC  X(13).                               AAOLMD
            05  7-CTIDND.                                               AAOLMD
              10 7-FIRST4     PIC  X(4).                                AAOLMD
              10 FILLER       PIC  X(6).                                AAOLMD
              10 7-LAST1      PIC  X.                                   AAOLMD
      ******************************************************************ADUTAB
      **              TABLE TA5B ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA5B-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=5B FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA5B.                                                CI0272
           04    G-TA5B-PARAM.                                          CI0272
             10  G-TA5B-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0272
                        VALUE      +154.                                CI0272
             10  G-TA5B-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0272
                        VALUE      +001.                                CI0272
             10  G-TA5B-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0272
                        VALUE      +017.                                CI0272
             10  G-TA5B-NUAPP  PICTURE 99                               CI0272
                        VALUE       0.                                  CI0272
             10  G-TA5B-NUTAB  PICTURE X(6)                             CI0272
                        VALUE 'TA005B'.                                 CI0272
             10  G-TA5B-TABFO  PICTURE XX                 VALUE SPACE.  CI0272
             10  G-TA5B-TABCR  PICTURE XX                 VALUE SPACE.  CI0272
             10  G-TA5B-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0272
             10  G-TA5B-NUSSC  PICTURE X  VALUE   ' '.                  CI0272
             10  G-TA5B-NUSSY  PICTURE X                  VALUE SPACE.  CI0272
             10  G-TA5B-TRANID PICTURE X(4)               VALUE SPACE.  CI0272
             10  G-TA5B-FILSYS.                                         CI0272
             15  G-TA5B-USERC  PICTURE X(6)               VALUE SPACE.  CI0272
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0272
           04             TA5B.                                         CI0272
            10            TA5B-GAPSC.                                   CI0272
            11            TA5B-CTIDA  PICTURE  9(3)                     CI0272
                          VALUE                ZERO.                    CI0272
            11            TA5B-PRCOD  PICTURE  9(5)                     CI0272
                          VALUE                ZERO.                    CI0272
            11            TA5B-PRSCD  PICTURE  X(9)                     CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-PRCODX PICTURE  9(5)                     CI0272
                          VALUE                ZERO.                    CI0272
            10            TA5B-PRCSUB PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-PRCAUT PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-PRCBAS PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-PRCSTK PICTURE  XX                       CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-PRCPRE PICTURE  X(4)                     CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-IBDUP  PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-IUSPR  PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-CVSYS  PICTURE  X(2)                     CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-IDTOD  PICTURE  X(1)                     CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-GRSFC  PICTURE  99                       CI0272
                          VALUE                ZERO.                    CI0272
            10            TA5B-ZDA18  PICTURE  X(18)                    CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-CMPCTB PICTURE  X(4)                     CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-ITERM  PICTURE  X(1)                     CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-AMFAC  PICTURE  S9(7)                    CI0272
                          VALUE                ZERO.                    CI0272
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-CPRBK  PICTURE  X(3)                     CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-CFXDM  PICTURE  99                       CI0272
                          VALUE                ZERO.                    CI0272
            10            TA5B-NGLCS  PICTURE  X(5)                     CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-NDFCS  PICTURE  X(5)                     CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-CTNLI  PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-CBANK  PICTURE  X(03)                    CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-ISYPO  PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-ISYPP  PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-ICOPT  PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-IANPY  PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-IDSAR  PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-ICIPT  PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-IANDS  PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-IKPMA  PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-INMWT  PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-IVANT  PICTURE  X(1)                     CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-ISDAV  PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-IUDAV  PICTURE  X                        CI0272
                          VALUE                SPACE.                   CI0272
            10            TA5B-ZDA15  PICTURE  X(15)                    CI0272
                          VALUE                SPACE.                   CI0272
      **                                                                ADUTAB
      *WORK FIELDS
      *
       01 WS01-QSACT        PIC 999.
       01 WS01-SUB          PIC 999.
       01 WS01-SUB1         PIC 99.
      *!WI
       01 WS01-DCAG9L
                        PICTURE 9(8).                                   CI0272
       01 WS00-GETIM        PIC X(08).
       01   DEBUT-WSS.                                                  CI0272
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0272
            05   IK     PICTURE X.                                      CI0272
       01  CONSTANTES-PAC.                                              CI0272
           05  FILLER  PICTURE X(87)   VALUE                            CI0272
                     '6015 CAT09/08/14CI0272ADMIN   14:35:09CI0272P AMERCI0272
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0272
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0272
           05  NUGNA   PICTURE X(5).                                    CI0272
           05  APPLI   PICTURE X(3).                                    CI0272
           05  DATGN   PICTURE X(8).                                    CI0272
           05  PROGR   PICTURE X(6).                                    CI0272
           05  CODUTI  PICTURE X(8).                                    CI0272
           05  TIMGN   PICTURE X(8).                                    CI0272
           05  PROGE   PICTURE X(8).                                    CI0272
           05  COBASE  PICTURE X(4).                                    CI0272
           05  DATGNC  PICTURE X(10).                                   CI0272
           05  RELEAS  PICTURE X(7).                                    CI0272
           05  DATGE   PICTURE X(10).                                   CI0272
           05  DATSQ   PICTURE X(10).                                   CI0272
       01  DATCE.                                                       CI0272
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0272
         05  DATOR.                                                     CI0272
           10  DATOA  PICTURE XX.                                       CI0272
           10  DATOM  PICTURE XX.                                       CI0272
           10  DATOJ  PICTURE XX.                                       CI0272
       01   VARIABLES-CONDITIONNELLES.                                  CI0272
            05                  FT      PICTURE X VALUE '0'.            CI0272
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0272
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0272
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0272
            05       5-AA00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0272
            05       5-GC00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0272
       01               S-AA10-SSA.                                     CI0272
            10         S1-AA10-SEGNAM PICTURE X(8)                      CI0272
                                      VALUE 'LMSPCON '.                 CI0272
            10         S1-AA10-CCOM   PICTURE X VALUE '*'.              CI0272
            10          S-AA10-CCOD   PICTURE X(5)                      CI0272
                                      VALUE '-----'.                    CI0272
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0272
       01            S-AAU10-SSA.                                       CI0272
            11      S1-AAU10-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'LMSPCON '.                 CI0272
            11      S1-AAU10-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-AAU10-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-AAU10-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(LMSPCONK'.                CI0272
            11       S-AAU10-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-AAU10-ALCIDN   PICTURE  9(11).                   CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01               S-GC01-SSA.                                     CI0272
            10         S1-GC01-SEGNAM PICTURE X(8)                      CI0272
                                      VALUE 'GC01    '.                 CI0272
            10         S1-GC01-CCOM   PICTURE X VALUE '*'.              CI0272
            10          S-GC01-CCOD   PICTURE X(5)                      CI0272
                                      VALUE '-----'.                    CI0272
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0272
       01            S-GCU01-SSA.                                       CI0272
            10      S1-GCU01-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC01    '.                 CI0272
            10      S1-GCU01-CCOM   PICTURE X VALUE '*'.                CI0272
            10       S-GCU01-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            10      S1-GCU01-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(GC01K'.                   CI0272
            10       S-GCU01-OPER  PICTURE XX VALUE ' ='.               CI0272
            10       S-GCU01-GC01K.                                     CI0272
            11       S-GCU01-C299.                                      CI0272
            12       S-GCU01-CTID.                                      CI0272
            13       S-GCU01-CTIDA    PICTURE  9(3).                    CI0272
            13       S-GCU01-CTIDN.                                     CI0272
            14       S-GCU01-CTIDNP   PICTURE  X(13).                   CI0272
            14       S-GCU01-CTIDND   PICTURE  9(11).                   CI0272
            10  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01               S-GC03-SSA.                                     CI0272
            10         S1-GC03-SEGNAM PICTURE X(8)                      CI0272
                                      VALUE 'GC03    '.                 CI0272
            10         S1-GC03-CCOM   PICTURE X VALUE '*'.              CI0272
            10          S-GC03-CCOD   PICTURE X(5)                      CI0272
                                      VALUE '-----'.                    CI0272
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0272
       01            S-GCA03-SSA.                                       CI0272
            11      S1-GCA03-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GCA03-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCA03-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCA03-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(CAATY'.                   CI0272
            11       S-GCA03-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCA03-CAATY    PICTURE  9(3).                    CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GCB03-SSA.                                       CI0272
            11      S1-GCB03-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GCB03-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCB03-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCB03-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(CVSYS'.                   CI0272
            11       S-GCB03-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCB03-CVSYS    PICTURE  X(2).                    CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GCC03-SSA.                                       CI0272
            11      S1-GCC03-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GCC03-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCC03-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCC03-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(CASTC'.                   CI0272
            11       S-GCC03-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCC03-CASTC    PICTURE  99.                      CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GCD03-SSA.                                       CI0272
            11      S1-GCD03-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GCD03-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCD03-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCD03-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(CACTO'.                   CI0272
            11       S-GCD03-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCD03-CACTO    PICTURE  9(3).                    CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GCE03-SSA.                                       CI0272
            11      S1-GCE03-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GCE03-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCE03-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCE03-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(IPULL'.                   CI0272
            11       S-GCE03-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCE03-IPULL    PICTURE  X.                       CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GCF03-SSA.                                       CI0272
            11      S1-GCF03-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GCF03-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCF03-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCF03-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(DTRAC'.                   CI0272
            11       S-GCF03-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCF03-DTRAC    PICTURE  9(8).                    CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GCG03-SSA.                                       CI0272
            11      S1-GCG03-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GCG03-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCG03-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCG03-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(CTRSO'.                   CI0272
            11       S-GCG03-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCG03-CTRSO    PICTURE  9(02).                   CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GCH03-SSA.                                       CI0272
            11      S1-GCH03-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GCH03-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCH03-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCH03-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(NTRCE'.                   CI0272
            11       S-GCH03-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCH03-NTRCE    PICTURE  9(06).                   CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GCI03-SSA.                                       CI0272
            11      S1-GCI03-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GCI03-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCI03-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCI03-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(ITRAN'.                   CI0272
            11       S-GCI03-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCI03-ITRAN    PICTURE  X.                       CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GCJ03-SSA.                                       CI0272
            11      S1-GCJ03-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GCJ03-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCJ03-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCJ03-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(DEFFT'.                   CI0272
            11       S-GCJ03-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCJ03-DEFFT    PICTURE  9(8).                    CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GCK03-SSA.                                       CI0272
            11      S1-GCK03-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GCK03-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCK03-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCK03-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(CPMTCA'.                  CI0272
            11       S-GCK03-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCK03-CPMTCA   PICTURE  XXX.                     CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GCL03-SSA.                                       CI0272
            11      S1-GCL03-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GCL03-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCL03-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCL03-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(ACASH'.                   CI0272
            11       S-GCL03-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCL03-ACASH    PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GCN03-SSA.                                       CI0272
            11      S1-GCN03-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GCN03-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCN03-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCN03-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(CRREV'.                   CI0272
            11       S-GCN03-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCN03-CRREV    PICTURE  X(3).                    CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GCO03-SSA.                                       CI0272
            11      S1-GCO03-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GCO03-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCO03-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCO03-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(CSYST'.                   CI0272
            11       S-GCO03-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCO03-CSYST    PICTURE  99.                      CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GCU03-SSA.                                       CI0272
            11      S1-GCU03-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GCU03-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCU03-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCU03-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(GC03K'.                   CI0272
            11       S-GCU03-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCU03-GC03K.                                     CI0272
            12       S-GCU03-DCACG9   PICTURE  9(8).                    CI0272
            12       S-GCU03-NAASQ    PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GC103-SSA.                                       CI0272
            12      S1-GC103-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            12      S1-GC103-CCOM   PICTURE X VALUE '*'.                CI0272
            12       S-GC103-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            12      S1-GC103-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(XDCACG9'.                 CI0272
            12       S-GC103-OPER  PICTURE XX VALUE ' ='.               CI0272
            12       S-GC103-DCACG9   PICTURE  9(8).                    CI0272
            12  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GC203-SSA.                                       CI0272
            11      S1-GC203-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GC203-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GC203-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GC203-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(XGEAUN'.                  CI0272
            11       S-GC203-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GC203-GEAUN    PICTURE  9(5).                    CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GC303-SSA.                                       CI0272
            11      S1-GC303-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GC303-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GC303-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GC303-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(XGEOPD2'.                 CI0272
            11       S-GC303-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GC303-GEOPD2   PICTURE  X(8).                    CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GC403-SSA.                                       CI0272
            11      S1-GC403-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            11      S1-GC403-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GC403-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GC403-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(XNBTCH'.                  CI0272
            11       S-GC403-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GC403-NBTCH    PICTURE  9(4).                    CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GC803-SSA.                                       CI0272
            12      S1-GC803-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC03    '.                 CI0272
            12      S1-GC803-CCOM   PICTURE X VALUE '*'.                CI0272
            12       S-GC803-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            12      S1-GC803-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(XNAASQ'.                  CI0272
            12       S-GC803-OPER  PICTURE XX VALUE ' ='.               CI0272
            12       S-GC803-NAASQ    PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            12  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01               S-GC06-SSA.                                     CI0272
            10         S1-GC06-SEGNAM PICTURE X(8)                      CI0272
                                      VALUE 'GC06    '.                 CI0272
            10         S1-GC06-CCOM   PICTURE X VALUE '*'.              CI0272
            10          S-GC06-CCOD   PICTURE X(5)                      CI0272
                                      VALUE '-----'.                    CI0272
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0272
       01            S-GCF06-SSA.                                       CI0272
            11      S1-GCF06-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC06    '.                 CI0272
            11      S1-GCF06-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCF06-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCF06-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(PRCOD1'.                  CI0272
            11       S-GCF06-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCF06-PRCOD1   PICTURE  9(5).                    CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01            S-GCU06-SSA.                                       CI0272
            11      S1-GCU06-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC06    '.                 CI0272
            11      S1-GCU06-CCOM   PICTURE X VALUE '*'.                CI0272
            11       S-GCU06-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            11      S1-GCU06-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(GC06K'.                   CI0272
            11       S-GCU06-OPER  PICTURE XX VALUE ' ='.               CI0272
            11       S-GCU06-GC06K.                                     CI0272
            12       S-GCU06-NPISQ    PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01               S-GC18-SSA.                                     CI0272
            10         S1-GC18-SEGNAM PICTURE X(8)                      CI0272
                                      VALUE 'GC18    '.                 CI0272
            10         S1-GC18-CCOM   PICTURE X VALUE '*'.              CI0272
            10          S-GC18-CCOD   PICTURE X(5)                      CI0272
                                      VALUE '-----'.                    CI0272
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0272
       01            S-GCU18-SSA.                                       CI0272
            10      S1-GCU18-SEGNAM PICTURE X(8)                        CI0272
                                      VALUE 'GC18    '.                 CI0272
            10      S1-GCU18-CCOM   PICTURE X VALUE '*'.                CI0272
            10       S-GCU18-CCOD   PICTURE X(5)                        CI0272
                                      VALUE '-----'.                    CI0272
            10      S1-GCU18-FLDNAM PICTURE X(9)                        CI0272
                                      VALUE '(GC18K'.                   CI0272
            10       S-GCU18-OPER  PICTURE XX VALUE ' ='.               CI0272
            10       S-GCU18-GC18K.                                     CI0272
            11       S-GCU18-GESQ2    PICTURE  99.                      CI0272
            10  FILLER   PICTURE X    VALUE ')'.                        CI0272
       01               S-GC29-SSA.                                     CI0272
            10         S1-GC29-SEGNAM PICTURE X(8)                      CI0272
                                      VALUE 'GC29    '.                 CI0272
            10         S1-GC29-CCOM   PICTURE X VALUE '*'.              CI0272
            10          S-GC29-CCOD   PICTURE X(5)                      CI0272
                                      VALUE '-----'.                    CI0272
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0272
       01   ZONES-UTILISATEUR PICTURE X.                                CI0272
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
      ** PCB POINTER FOR LM1P                                           ADU015
            05 PCB-LM1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR ACAP                                             ADU015
      *!WF DSP=XA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XA00.                                         CI0272
          05              XA00-SUITE.                                   CI0272
            15       FILLER         PICTURE  X(00106).                  CI0272
       01                 XA06  REDEFINES      XA00.                    CI0272
            10            XA06-XDBPCB.                                  CI0272
            11            XA06-XDBDNM PICTURE  X(08).                   CI0272
            11            XA06-XSEGLV PICTURE  X(02).                   CI0272
            11            XA06-XRC    PICTURE  X(02).                   CI0272
            11            XA06-XPROPT PICTURE  X(04).                   CI0272
            11            XA06-FILLER PICTURE  S9(5)                    CI0272
                          BINARY.                                       CI0272
            11            XA06-XSEGNM PICTURE  X(08).                   CI0272
            11            XA06-XKEYLN PICTURE  S9(05)                   CI0272
                          BINARY.                                       CI0272
            11            XA06-XSEGNB PICTURE  9(05)                    CI0272
                          BINARY.                                       CI0272
            11            XA06-XCOKEY PICTURE  X(70).                   CI0272
      *** PCB MASK FOR LM1P                                             ADU015
      *!WF DSP=XE DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XE00.                                         CI0272
          05              XE00-SUITE.                                   CI0272
            15       FILLER         PICTURE  X(00106).                  CI0272
       01                 XE06  REDEFINES      XE00.                    CI0272
            10            XE06-XDBPCB.                                  CI0272
            11            XE06-XDBDNM PICTURE  X(08).                   CI0272
            11            XE06-XSEGLV PICTURE  X(02).                   CI0272
            11            XE06-XRC    PICTURE  X(02).                   CI0272
            11            XE06-XPROPT PICTURE  X(04).                   CI0272
            11            XE06-FILLER PICTURE  S9(5)                    CI0272
                          BINARY.                                       CI0272
            11            XE06-XSEGNM PICTURE  X(08).                   CI0272
            11            XE06-XKEYLN PICTURE  S9(05)                   CI0272
                          BINARY.                                       CI0272
            11            XE06-XSEGNB PICTURE  9(05)                    CI0272
                          BINARY.                                       CI0272
            11            XE06-XCOKEY PICTURE  X(70).                   CI0272
      *K974 BROWSER INPUT
      *!WF DSP=K9 DSL=K9 SEL=74 FOR=E DES=1 LEV=1 PLT=10
       01                 K974.                                         CI0272
            10            K974-MAPPN  PICTURE  X(10).                   CI0272
            10            K974-CAFLOW PICTURE  X(4).                    CI0272
            10            K974-CACTA  PICTURE  X(1).                    CI0272
            10            K974-IDBUP  PICTURE  X.                       CI0272
            10            K974-NSSSI  PICTURE  X(24).                   CI0272
            10            K974-CTID   PICTURE  X(27).                   CI0272
            10            K974-QSACTF PICTURE  9(3).                    CI0272
            10            K974-QSACTT PICTURE  9(3).                    CI0272
            10            K974-FILLER PICTURE  X(100).                  CI0272
            10            K974-K981.                                    CI0272
            11            K974-GRUST                                    CI0272
                          OCCURS       194     TIMES.                   CI0272
            12            K974-CFIDC  PICTURE  X(5).                    CI0272
            12            K974-CACCT  PICTURE  X.                       CI0272
            12            K974-AACTV  PICTURE  S9(11)V99.               CI0272
            12            K974-PFNDV  PICTURE  999V999.                 CI0272
            12            K974-ITRNB  PICTURE  X.                       CI0272
            12            K974-IALLV  PICTURE  X.                       CI0272
            12            K974-ADBRQ  PICTURE  S9(11)V99.               CI0272
            12            K974-PACT1  PICTURE  S999V999.                CI0272
            12            K974-AEDRQ  PICTURE  S9(11)V99.               CI0272
            12            K974-AMAXD  PICTURE  S9(7)V99.                CI0272
            10            K974-NSEQ4  PICTURE  9(4)                     CI0272
                          OCCURS       194     TIMES.                   CI0272
      *V218 INPUT FOR ORDER TICKET INFO
      *!WF DSP=W2 DSL=V2 SEL=18 FOR=I DES=1 LEV=1 PLT=10
       01                 W218.                                         CI0272
            10            W218-CSLCT  PICTURE  X.                       CI0272
            10            W218-NGEOR  PICTURE  9(08).                   CI0272
            10            W218-CACLS2 PICTURE  X(20).                   CI0272
            10            W218-CAPID  PICTURE  9(2).                    CI0272
            10            W218-NGEOPA PICTURE  X(08).                   CI0272
            10            W218-CACLS1 PICTURE  X(20).                   CI0272
            10            W218-CTRHO  PICTURE  9(8).                    CI0272
            10            W218-GETIM3 PICTURE  S9(7)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            W218-GEOPD9 PICTURE  X(8).                    CI0272
            10            W218-DCACG1 PICTURE  9(8).                    CI0272
            10            W218-IWEBBT PICTURE  X.                       CI0272
            10            W218-CAVER  PICTURE  X.                       CI0272
            10            W218-FILLER PICTURE  X(30).                   CI0272
      *
      *QT58 MC58 INPUT
      *!WF DSP=QT DSL=QT SEL=58 FOR=I LEV=1 PLT=10
       01                 QT00.                                         CI0272
          05              QT00-SUITE.                                   CI0272
            15       FILLER         PICTURE  X(02300).                  CI0272
       01                 QT58  REDEFINES      QT00.                    CI0272
            10            QT58-QT5K.                                    CI0272
            11            QT58-C299.                                    CI0272
            12            QT58-CTID.                                    CI0272
            13            QT58-CTIDA  PICTURE  9(3).                    CI0272
            13            QT58-CTIDN.                                   CI0272
            14            QT58-CTIDNP PICTURE  X(13).                   CI0272
            14            QT58-CTIDND PICTURE  9(11).                   CI0272
            11            QT58-GECKD2 PICTURE  9.                       CI0272
            11            QT58-NSEQ5  PICTURE  9(5).                    CI0272
            11            QT58-CTSTA  PICTURE  99.                      CI0272
            11            QT58-CTSTAL PICTURE  X(10).                   CI0272
            11            QT58-CTOWN  PICTURE  9(3).                    CI0272
            11            QT58-CTTLN1 PICTURE  X(30).                   CI0272
            11            QT58-CTTLN2 PICTURE  X(30).                   CI0272
            11            QT58-CTTLN3 PICTURE  X(30).                   CI0272
            11            QT58-CTTBO1 PICTURE  X(45).                   CI0272
            11            QT58-CTTBO2 PICTURE  X(45).                   CI0272
            11            QT58-CTEFD  PICTURE  9(8).                    CI0272
            11            QT58-CTIAD  PICTURE  9(8).                    CI0272
            11            QT58-CTCUS  PICTURE  999.                     CI0272
            11            QT58-GR98.                                    CI0272
            12            QT58-GRID.                                    CI0272
            13            QT58-GRIDC  PICTURE  9(3).                    CI0272
            13            QT58-GRIDN.                                   CI0272
            14            QT58-GRIDNP PICTURE  99.                      CI0272
            14            QT58-GRIDND PICTURE  9(8).                    CI0272
            11            QT58-CQACT  PICTURE  999.                     CI0272
            11            QT58-CTCCI  PICTURE  X.                       CI0272
            11            QT58-CIRAS  PICTURE  999.                     CI0272
            11            QT58-CIRAT  PICTURE  999.                     CI0272
            11            QT58-IACVD  PICTURE  X.                       CI0272
            11            QT58-FILLER PICTURE  X(4).                    CI0272
            11            QT58-PRCODA PICTURE  X(5).                    CI0272
            11            QT58-PRCMN  PICTURE  X(20).                   CI0272
            11            QT58-MRPLN  PICTURE  X(30).                   CI0272
            11            QT58-CPRDG  PICTURE  9(2).                    CI0272
            11            QT58-CPRDA1 PICTURE  9(3).                    CI0272
            11            QT58-PRSCD  PICTURE  X(9).                    CI0272
            11            QT58-MSP03  PICTURE  X(3).                    CI0272
            11            QT58-CGRLI  PICTURE  X.                       CI0272
            11            QT58-ITERM  PICTURE  X(1).                    CI0272
            11            QT58-IVARP  PICTURE  X.                       CI0272
            11            QT58-DVALU  PICTURE  9(8).                    CI0272
            11            QT58-AACTV  PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ACCTVC PICTURE  X(20).                   CI0272
            11            QT58-ITXTI  PICTURE  X.                       CI0272
            11            QT58-ASANP  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ACINV  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-CELBL  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-NMESS2 PICTURE  S9(6)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-FILLER PICTURE  X(1).                    CI0272
            11            QT58-PRCLN  PICTURE  X(60).                   CI0272
            11            QT58-GECKD  PICTURE  9.                       CI0272
            11            QT58-MPLNA  PICTURE  X(19).                   CI0272
            11            QT58-CQACTL PICTURE  X(45).                   CI0272
            11            QT58-CRQPA  PICTURE  9(3).                    CI0272
            11            QT58-IVANT  PICTURE  X(1).                    CI0272
            11            QT58-IDBRP  PICTURE  X(1).                    CI0272
            11            QT58-IANPY  PICTURE  X.                       CI0272
            11            QT58-IVARP1 PICTURE  X.                       CI0272
            11            QT58-FILLER PICTURE  X(27).                   CI0272
            11            QT58-NSEQ2A PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-NSEQ2P PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-MRPSN  PICTURE  X(12).                   CI0272
            11            QT58-GEHCD  PICTURE  9(3)                     CI0272
                          OCCURS       002     TIMES.                   CI0272
            11            QT58-GEHCSU PICTURE  9(5)                     CI0272
                          OCCURS       002     TIMES.                   CI0272
            11            QT58-PRCSN  PICTURE  X(9).                    CI0272
            11            QT58-CGRMF  PICTURE  X.                       CI0272
            11            QT58-IGFEX  PICTURE  X.                       CI0272
            11            QT58-CLIDP  PICTURE  X(23).                   CI0272
            11            QT58-CLCTRC PICTURE  9(3).                    CI0272
            11            QT58-ADINP  PICTURE  X(20).                   CI0272
            11            QT58-CLCTRA PICTURE  9(3).                    CI0272
            11            QT58-GRPLC  PICTURE  99.                      CI0272
            11            QT58-CIDRP  PICTURE  99.                      CI0272
            11            QT58-FILLER PICTURE  X(01).                   CI0272
            11            QT58-AVMTOT PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-AVCSH  PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-AMARC  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-AVLMX  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-AVLMN  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-INDRS  PICTURE  X.                       CI0272
            11            QT58-MPRN4  PICTURE  X(35).                   CI0272
            11            QT58-FILLER PICTURE  X(1).                    CI0272
            11            QT58-ACVALM PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-INDRSA PICTURE  X(2).                    CI0272
            11            QT58-DXTMSA PICTURE  X(26).                   CI0272
            11            QT58-NMESS6 PICTURE  S9(6)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-NMESS7 PICTURE  S9(6)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-IBIDSA PICTURE  X.                       CI0272
            11            QT58-IBIDSB PICTURE  X.                       CI0272
            11            QT58-INSPOS PICTURE  X.                       CI0272
            11            QT58-INSPOD PICTURE  X.                       CI0272
            11            QT58-ACBALX PICTURE  X(20).                   CI0272
            11            QT58-AINVMX PICTURE  X(20).                   CI0272
            11            QT58-AMARCX PICTURE  X(20).                   CI0272
            11            QT58-AVMTOX PICTURE  X(20).                   CI0272
            11            QT58-IMNPR  PICTURE  X.                       CI0272
            11            QT58-ISSPL  PICTURE  X.                       CI0272
            11            QT58-AVMTOI PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-AVCSHI PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-APOSC  PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-AVLMXI PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-AVLMN1 PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-AVLMN2 PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-FILLER PICTURE  X(05).                   CI0272
            10            QT58-QT5A.                                    CI0272
            11            QT58-CLID   PICTURE  X(23).                   CI0272
            11            QT58-GECKD1 PICTURE  9.                       CI0272
            11            QT58-MCLNM  PICTURE  X(40).                   CI0272
            11            QT58-MCLNM2 PICTURE  X(40).                   CI0272
            11            QT58-CLTYP  PICTURE  X.                       CI0272
            11            QT58-CLDOB  PICTURE  9(8).                    CI0272
            11            QT58-CLDTH  PICTURE  X.                       CI0272
            11            QT58-CLTIN  PICTURE  9(12).                   CI0272
            11            QT58-CLTINC PICTURE  9.                       CI0272
            11            QT58-GESAD1 PICTURE  X(30).                   CI0272
            11            QT58-GESAD2 PICTURE  X(30).                   CI0272
            11            QT58-GESAD3 PICTURE  X(30).                   CI0272
            11            QT58-GECIT  PICTURE  X(25).                   CI0272
            11            QT58-GECTRY PICTURE  X(20).                   CI0272
            11            QT58-GEPCD  PICTURE  X(12).                   CI0272
            11            QT58-GEST   PICTURE  X(8).                    CI0272
            11            QT58-GEADS  PICTURE  9.                       CI0272
            11            QT58-GECSD  PICTURE  9(8).                    CI0272
            11            QT58-QCLAGE PICTURE  9(3)V9                   CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-FILLER PICTURE  X(06).                   CI0272
            10            QT58-QT5T.                                    CI0272
            11            QT58-ATFRA  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-AGOFD  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-APRMX  PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-APRMN  PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-IOWNC  PICTURE  X.                       CI0272
            11            QT58-COWNF  PICTURE  X(30).                   CI0272
            11            QT58-CTYPE  PICTURE  X.                       CI0272
            11            QT58-CIRAC  PICTURE  X(5).                    CI0272
            11            QT58-CTXMT  PICTURE  9(2).                    CI0272
            11            QT58-AMIND  PICTURE  S9(7)V99.                CI0272
            11            QT58-AMAXAR PICTURE  S9(7)V99.                CI0272
            11            QT58-QSHOWQ PICTURE  S9(9)V999                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-QSHOW0 PICTURE  S9(10)V999               CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-PPOT1  PICTURE  S9(3)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-PACT1  PICTURE  S999V999                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-IPRTA  PICTURE  X.                       CI0272
            11            QT58-FILLER PICTURE  X.                       CI0272
            11            QT58-CLCUS  PICTURE  99.                      CI0272
            11            QT58-CCDSCW PICTURE  9(2).                    CI0272
            11            QT58-CCACT  PICTURE  99.                      CI0272
            11            QT58-CIRAG.                                   CI0272
            12            QT58-CIRAP  PICTURE  XX                       CI0272
                          OCCURS       010     TIMES.                   CI0272
            11            QT58-ITERF  PICTURE  X.                       CI0272
            11            QT58-IACFPD PICTURE  X(1).                    CI0272
            11            QT58-AFEET  PICTURE  S9(5)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ATERF  PICTURE  S9(5)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-CLIDNB PICTURE  9(8).                    CI0272
            11            QT58-ALOAD  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ASURR  PICTURE  S9(07)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ASHIS  PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-AMNBL  PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-APNAC  PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ANGOF  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-CPLTYP PICTURE  X(14).                   CI0272
            10            QT58-QT5N.                                    CI0272
            11            QT58-IARRAN PICTURE  X.                       CI0272
            11            QT58-GESTD1 PICTURE  9(8).                    CI0272
            11            QT58-GEEND1 PICTURE  S9(8)                    CI0272
                          BINARY.                                       CI0272
            11            QT58-GESTD  PICTURE  9(8).                    CI0272
            11            QT58-GEEND  PICTURE  9(8).                    CI0272
            11            QT58-NSQ4B2 PICTURE  9(8)                     CI0272
                          BINARY.                                       CI0272
            11            QT58-CDEST  PICTURE  99.                      CI0272
            11            QT58-DEFFT  PICTURE  9(8).                    CI0272
            11            QT58-CPMTF  PICTURE  99.                      CI0272
            11            QT58-CPMTG  PICTURE  99.                      CI0272
            11            QT58-MPMTFL PICTURE  X(24).                   CI0272
            11            QT58-MPMTFE PICTURE  X(24).                   CI0272
            11            QT58-DLAUP  PICTURE  9(8).                    CI0272
            11            QT58-NSEQ4B PICTURE  9(8)                     CI0272
                          BINARY.                                       CI0272
            11            QT58-QSACTF PICTURE  9(3).                    CI0272
            11            QT58-QSACTT PICTURE  9(3).                    CI0272
            11            QT58-CCONF  PICTURE  X(25).                   CI0272
            11            QT58-DCONF  PICTURE  9(8).                    CI0272
            11            QT58-DTIMT  PICTURE  X(8).                    CI0272
            11            QT58-CACTS  PICTURE  X.                       CI0272
            11            QT58-ADBRQ  PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-DNPMT  PICTURE  9(8).                    CI0272
            11            QT58-NAPDS  PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-CDEST1 PICTURE  99.                      CI0272
            11            QT58-CLANR1 PICTURE  X(23).                   CI0272
            11            QT58-FILLER PICTURE  X(01).                   CI0272
            10            QT58-FILLER PICTURE  X(600).                  CI0272
            10            QT58-QT5C                                     CI0272
                          REDEFINES            QT58-FILLER.             CI0272
            11            QT58-CESLD  PICTURE  9(8).                    CI0272
            11            QT58-PCIRB5 PICTURE  S9(3)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-PANYDD PICTURE  S9(3)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-CEIT   PICTURE  9(3).                    CI0272
            11            QT58-PPART  PICTURE  9(3)V99                  CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-DTRME  PICTURE  9(8).                    CI0272
            11            QT58-CEIRND PICTURE  9(8).                    CI0272
            11            QT58-DANNIA PICTURE  9(8).                    CI0272
            11            QT58-AAPAA  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-CELBDT PICTURE  9(8).                    CI0272
            11            QT58-CEIIS  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-DTRME1 PICTURE  9(8).                    CI0272
            11            QT58-GMKTS.                                   CI0272
            12            QT58-DTRME2 PICTURE  9(8)                     CI0272
                          OCCURS       005     TIMES.                   CI0272
            12            QT58-DTRME3 PICTURE  9(8)                     CI0272
                          OCCURS       005     TIMES.                   CI0272
            11            QT58-ALINT  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-CEHCD  PICTURE  9(3)                     CI0272
                          OCCURS       006     TIMES.                   CI0272
            11            QT58-CEFOTR PICTURE  S9(3)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-DGPED  PICTURE  9(8).                    CI0272
            11            QT58-DIPED  PICTURE  9(8).                    CI0272
            11            QT58-FILLER PICTURE  X(409).                  CI0272
            10            QT58-QT5F                                     CI0272
                          REDEFINES            QT58-FILLER.             CI0272
            11            QT58-DLAUP2 PICTURE  9(8).                    CI0272
            11            QT58-QSHOW  PICTURE  S9(10)V999               CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-AFAVP  PICTURE  S9(4)V9(3)               CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-QSHIS  PICTURE  S9(10)V999               CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-QSHNM  PICTURE  S9(10)V999.              CI0272
            11            QT58-QSHOM  PICTURE  S9(10)V999               CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ADDAC  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-QSHES  PICTURE  S9(10)V999               CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-NDCUS  PICTURE  X(9).                    CI0272
            11            QT58-CSTKR5 PICTURE  X(5).                    CI0272
            11            QT58-NACID  PICTURE  S9(11)                   CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-AGOFD2 PICTURE  S9(9)V99.                CI0272
            11            QT58-TCBAT  PICTURE  X(21).                   CI0272
            11            QT58-FILLER PICTURE  X(490).                  CI0272
            10            QT58-QT5L                                     CI0272
                          REDEFINES            QT58-FILLER.             CI0272
            11            QT58-ALDBEN PICTURE  S9(09)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-APREL  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ALMODE PICTURE  99.                      CI0272
            11            QT58-ITMEC  PICTURE  X(1).                    CI0272
            11            QT58-ITAMR  PICTURE  X(1).                    CI0272
            11            QT58-MPMTF  PICTURE  X(14).                   CI0272
            11            QT58-TPLNL  PICTURE  X(30).                   CI0272
            11            QT58-ASBENA PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ASBENB PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ASBENC PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ASBENE PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ASBENF PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-GESTNS PICTURE  X(2).                    CI0272
            11            QT58-CTWHPB PICTURE  9(3)V999.                CI0272
            11            QT58-CTWHCB PICTURE  X.                       CI0272
            11            QT58-AMVA1  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ASPAM  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ACTCH  PICTURE  S9(07)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-AMXLN  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ALFGH  PICTURE  999.                     CI0272
            11            QT58-ALPLNI PICTURE  9.                       CI0272
            11            QT58-ATSA8  PICTURE  S9(07)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-CVALB  PICTURE  X(3).                    CI0272
            11            QT58-ASURRN PICTURE  S9(07)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ASURRW PICTURE  S9(07)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ATLTB  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-AEARN0 PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ATFPI  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-AEARN1 PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ISELO  PICTURE  X.                       CI0272
            11            QT58-CCLAC  PICTURE  X.                       CI0272
            11            QT58-ALINNO PICTURE  99.                      CI0272
            11            QT58-ALPLNJ PICTURE  9.                       CI0272
            11            QT58-COLPL  PICTURE  9(05).                   CI0272
            11            QT58-ALPLDT PICTURE  9(8).                    CI0272
            11            QT58-ANFMC  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-CPNOP  PICTURE  X(2).                    CI0272
            11            QT58-CVSTC  PICTURE  X(4).                    CI0272
            11            QT58-CGMBR  PICTURE  X.                       CI0272
            11            QT58-DWSDT  PICTURE  9(8).                    CI0272
            11            QT58-IRDPH  PICTURE  X.                       CI0272
            11            QT58-DWAIT  PICTURE  9(8).                    CI0272
            11            QT58-IAPGP  PICTURE  X.                       CI0272
            11            QT58-CASTA  PICTURE  X.                       CI0272
            11            QT58-CSSUP2 PICTURE  X.                       CI0272
            11            QT58-CVOMC1 PICTURE  X(1).                    CI0272
            11            QT58-APGBP  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ALDDUE PICTURE  9(08).                   CI0272
            11            QT58-APYMT  PICTURE  S9(9)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ALSURR PICTURE  S9(09)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-CESTP  PICTURE  X(03).                   CI0272
            11            QT58-FILLER PICTURE  X(356).                  CI0272
            10            QT58-QT5O                                     CI0272
                          REDEFINES            QT58-FILLER.             CI0272
            11            QT58-NBACT  PICTURE  S9(11)                   CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-CTIAC  PICTURE  S9(3)                    CI0272
                          BINARY.                                       CI0272
            11            QT58-CASTT  PICTURE  S99                      CI0272
                          BINARY.                                       CI0272
            11            QT58-CATMI  PICTURE  S9                       CI0272
                          BINARY.                                       CI0272
            11            QT58-IATMR  PICTURE  X(3).                    CI0272
            11            QT58-IBIPI  PICTURE  X.                       CI0272
            11            QT58-CBPST  PICTURE  S99                      CI0272
                          BINARY.                                       CI0272
            11            QT58-TBPST  PICTURE  X(16).                   CI0272
            11            QT58-CODPI  PICTURE  X.                       CI0272
            11            QT58-TODPS  PICTURE  X(9).                    CI0272
            11            QT58-FILLER PICTURE  X(448).                  CI0272
            11            QT58-IBPSD  PICTURE  X.                       CI0272
            11            QT58-FILLER PICTURE  X(107).                  CI0272
            11            QT58-QT5E                                     CI0272
                          REDEFINES            QT58-FILLER.             CI0272
            12            QT58-MPRN4X PICTURE  X(100).                  CI0272
            12            QT58-CCMSH  PICTURE  X(2).                    CI0272
            12            QT58-CPRCS  PICTURE  X(04).                   CI0272
            12            QT58-CURST  PICTURE  X.                       CI0272
            10            QT58-QT5M                                     CI0272
                          REDEFINES            QT58-FILLER.             CI0272
            11            QT58-NAPCN1 PICTURE  X(24).                   CI0272
            11            QT58-FILLER PICTURE  X(576).                  CI0272
            10            QT58-QT5B                                     CI0272
                          REDEFINES            QT58-FILLER.             CI0272
            11            QT58-NAPCN2 PICTURE  X(24).                   CI0272
            11            QT58-CTIDAL PICTURE  X(40).                   CI0272
            11            QT58-NPHNS  PICTURE  X(14).                   CI0272
            11            QT58-FILLER PICTURE  X(522).                  CI0272
            10            QT58-QT5P                                     CI0272
                          REDEFINES            QT58-FILLER.             CI0272
            11            QT58-CFPPT  PICTURE  9(3).                    CI0272
            11            QT58-TTYPP  PICTURE  X(40).                   CI0272
            11            QT58-CPPST  PICTURE  9(3).                    CI0272
            11            QT58-TPPST  PICTURE  X(15).                   CI0272
            11            QT58-APFEEQ PICTURE  S9(7)V99.                CI0272
            11            QT58-APFEEC PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-APFEEP PICTURE  S9(7)V99.                CI0272
            11            QT58-ISVCA  PICTURE  X.                       CI0272
            11            QT58-NSBVS  PICTURE  X(5).                    CI0272
            11            QT58-ICKRV  PICTURE  X.                       CI0272
            11            QT58-PDAMT  PICTURE  S9(03).                  CI0272
            11            QT58-PSTAX  PICTURE  S9(03)V999.              CI0272
            11            QT58-DPCAL  PICTURE  9(8).                    CI0272
            11            QT58-NADVF  PICTURE  X(08).                   CI0272
            11            QT58-DAGUP  PICTURE  9(8).                    CI0272
            11            QT58-AANFEA PICTURE  9(5)V99                  CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-CLIDN7 PICTURE  9(8)                     CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-ARANV  PICTURE  S9(7)V99                 CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            QT58-DRANV  PICTURE  9(8).                    CI0272
            11            QT58-FILLER PICTURE  X(454).                  CI0272
            10            QT58-QT50                                     CI0272
                          REDEFINES            QT58-FILLER.             CI0272
            11            QT58-NANCA  PICTURE  X(30).                   CI0272
            11            QT58-MANCN  PICTURE  X(100).                  CI0272
            11            QT58-AINPTX PICTURE  X(20).                   CI0272
            11            QT58-CTID01 PICTURE  X(27).                   CI0272
            11            QT58-NANCA1 PICTURE  X(04).                   CI0272
            11            QT58-IIVAR  PICTURE  X(1).                    CI0272
            11            QT58-FILLER PICTURE  X(418).                  CI0272
            10            QT58-QT5R                                     CI0272
                          REDEFINES            QT58-FILLER.             CI0272
            11            QT58-NACTJ  PICTURE  X(04).                   CI0272
            11            QT58-NACNO6 PICTURE  X(11).                   CI0272
            11            QT58-FILLER PICTURE  X(585).                  CI0272
            10            QT58-AMAXA  PICTURE  S9(7)V99.                CI0272
            10            QT58-ISAOR  PICTURE  X.                       CI0272
            10            QT58-ISACH  PICTURE  X.                       CI0272
            10            QT58-CERRBA PICTURE  X(02).                   CI0272
            10            QT58-CERRBH PICTURE  X(02).                   CI0272
            10            QT58-IWITHH PICTURE  X.                       CI0272
            10            QT58-CTID20 PICTURE  X(27).                   CI0272
            10            QT58-GECKD3 PICTURE  9.                       CI0272
            10            QT58-DANFC  PICTURE  X(10).                   CI0272
            10            QT58-DAFCN  PICTURE  X(10).                   CI0272
            10            QT58-ISMTA  PICTURE  X.                       CI0272
            10            QT58-CERRBT PICTURE  X(02).                   CI0272
            10            QT58-NPLNI  PICTURE  X(10).                   CI0272
            10            QT58-FILLER PICTURE  X(023).                  CI0272
      *
      *MISCELLANEOUS INPUT FROM CI9035 AND CI0268
      *!WF DSP=M9 DSL=K9 SEL=77 FOR=I DES=1 LEV=1 PLT=10
       01                 M977.                                         CI0272
            10            M977-ICUST  PICTURE  X.                       CI0272
            10            M977-GEAUN  PICTURE  9(5).                    CI0272
            10            M977-GEOPD2 PICTURE  X(8).                    CI0272
            10            M977-ADBRQ  PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            M977-AEDRQ  PICTURE  S9(11)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            M977-ALINNO PICTURE  99.                      CI0272
            10            M977-IANTF  PICTURE  XX.                      CI0272
            10            M977-GETIM  PICTURE  S9(7)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            M977-DEFFT  PICTURE  9(8).                    CI0272
            10            M977-ISUBA1 PICTURE  X.                       CI0272
            10            M977-ISUBA2 PICTURE  X.                       CI0272
            10            M977-ALLNB  PICTURE  S9(07)V99                CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            M977-DCACG  PICTURE  9(8).                    CI0272
            10            M977-CRSNG1 PICTURE  X(02).                   CI0272
            10            M977-CRSNG2 PICTURE  X(02).                   CI0272
            10            M977-IFTDY1 PICTURE  X.                       CI0272
            10            M977-IFXFD  PICTURE  X.                       CI0272
            10            M977-FILLER PICTURE  X(99).                   CI0272
      *
      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0272
          05              DE00-SUITE.                                   CI0272
            15       FILLER         PICTURE  X(00653).                  CI0272
       01                 DE10  REDEFINES      DE00.                    CI0272
            10            DE10-DU11.                                    CI0272
            11            DE10-XFONC  PICTURE  X(4).                    CI0272
            11            DE10-MPSBN  PICTURE  X(8).                    CI0272
            11            DE10-XDBDNM PICTURE  X(08).                   CI0272
            11            DE10-XSEGNM PICTURE  X(08).                   CI0272
            11            DE10-XRC    PICTURE  X(02).                   CI0272
            11            DE10-MSEG   PICTURE  X(08).                   CI0272
            11            DE10-XCOKEY PICTURE  X(70).                   CI0272
            11            DE10-CUIBR  PICTURE  X(01).                   CI0272
            11            DE10-CUIBA  PICTURE  X(01).                   CI0272
            11            DE10-IPBIK  PICTURE  X(1).                    CI0272
            10            DE10-DU03.                                    CI0272
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            DE10-CMSSF  PICTURE  XX.                      CI0272
            11            DE10-DU09.                                    CI0272
            12            DE10-CMESA  PICTURE  S9(9)                    CI0272
                          BINARY.                                       CI0272
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0272
                          BINARY.                                       CI0272
            12            DE10-CMESB  PICTURE  S9(9)                    CI0272
                          BINARY.                                       CI0272
            12            DE10-CMSST  PICTURE  S9(9)                    CI0272
                          BINARY.                                       CI0272
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0272
                          BINARY.                                       CI0272
            12            DE10-QELLAA PICTURE  S9(9)                    CI0272
                          BINARY.                                       CI0272
            12            DE10-TMESS4 PICTURE  X(512).                  CI0272
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0272
          05              MS00-SUITE.                                   CI0272
            15       FILLER         PICTURE  X(00542).                  CI0272
       01                 MS03  REDEFINES      MS00.                    CI0272
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            10            MS03-CMSSF  PICTURE  XX.                      CI0272
            10            MS03-DU09.                                    CI0272
            11            MS03-CMESA  PICTURE  S9(9)                    CI0272
                          BINARY.                                       CI0272
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0272
                          BINARY.                                       CI0272
            11            MS03-CMESB  PICTURE  S9(9)                    CI0272
                          BINARY.                                       CI0272
            11            MS03-CMSST  PICTURE  S9(9)                    CI0272
                          BINARY.                                       CI0272
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0272
                          BINARY.                                       CI0272
            11            MS03-QELLAA PICTURE  S9(9)                    CI0272
                          BINARY.                                       CI0272
            11            MS03-TMESS4 PICTURE  X(512).                  CI0272
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0272
            10            MX11-QMSGS  PICTURE  9(03).                   CI0272
            10            MX11-PJ09                                     CI0272
                          OCCURS       025     TIMES.                   CI0272
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0272
                          COMPUTATIONAL-3.                              CI0272
            11            MX11-CMESB  PICTURE  S9(9)                    CI0272
                          BINARY.                                       CI0272
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                K974
                                W218
                                M977
                                QT58
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0272
      *               *                                   *             CI0272
      *               *INITIALISATIONS                    *             CI0272
      *               *                                   *             CI0272
      *               *************************************.            CI0272
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
      *N02CA.    NOTE *INITIALIZE LINKAGE SEGMENTS        *.
       F02CA.                                                           lv10
           INITIALIZE  GC01
           GC03
           GC06
           GC18
           GC29.
       F02CA-FN. EXIT.
      *N02SC.    NOTE *SETTING OF POINTERS                *.
       F02SC.                                                           lv10
      *SET ADDRESS FOR ACAP                                             DOT
           SET ADDRESS OF XA06 TO                                       ADU015
                PCB-ACAP-PTR1.                                          ADU015
      *SET ADDRESS FOR LM1P                                             DOT
           SET ADDRESS OF XE06 TO                                       ADU015
                PCB-LM1P-PTR1.                                          ADU015
       F02SC-FN. EXIT.
       F02-FN.   EXIT.
      *N03.      NOTE *************************************.
      *               *                                   *
      *               *LOAD MISCELLANEOUS VARIABLES       *
      *               *                                   *
      *               *************************************.
       F03.           EXIT.                                             lv05
      *N03DD.    NOTE *CALC TOTAL NUMBER OF SUB ACCTS     *.
       F03DD.                                                           lv10
           COMPUTE     WS01-QSACT = K974-QSACTF +
           K974-QSACTT.
       F03DD-FN. EXIT.
       F03-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0272
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0272
      *               *                                   *             CI0272
      *               *FIN DE TRAITEMENT                  *             CI0272
      *               *                                   *             CI0272
      *               *************************************.            CI0272
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0272
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *MAINLINE PROCESSING                *
      *               *                                   *
      *               *************************************.
       F35.           EXIT.                                             lv05
      *N35FG.    NOTE *MOVE ZERO TO SUB-UNIT              *.
       F35FG.                                                           lv10
           MOVE        ZERO TO GC03-NSUNT.
       F35FG-FN. EXIT.
      *N35LF.    NOTE *DETERMINE CVSYS                    *.
       F35LF.                                                           lv10
           MOVE        QT58-CTIDA TO TA5B-CTIDA
           MOVE        QT58-PRCODA TO TA5B-PRCOD
           MOVE        SPACES TO TA5B-PRSCD
           MOVE        '0' TO TA5B-IK
           PERFORM     F92TA THRU F92TA-FN.
      *N35LJ.    NOTE *IF CVSYS WAS FOUND; MOVE           *.
       F35LJ.    IF    TA5B-IK = '0'                                    lv15
                 NEXT SENTENCE ELSE GO TO     F35LJ-FN.
           MOVE        TA5B-CVSYS TO GC03-CVSYS.
       F35LJ-900. GO TO F35LL-FN.
       F35LJ-FN. EXIT.
      *N35LL.    NOTE *ELSE... SET TO ZERO                *.
       F35LL.                                                           lv15
           MOVE        ZERO TO GC03-CVSYS.
       F35LL-FN. EXIT.
       F35LF-FN. EXIT.
      *N35PE.    NOTE *GET MOST RECENT LIFE UPDATE DATE   *.
       F35PE.                                                           lv10
      ********** PMS AAOLMD *********                                   AAOLMD
           MOVE        '0' TO AA10-CF                                   AAOLMD
           MOVE        QT58-CTID TO 7-CTID.                             AAOLMD
                 IF    7-FIRST4 = '9000' OR                             DOT
                       '9700'
           MOVE        '90000000000' TO S-AAU10-ALCIDN.
                 IF    7-FIRST4 = '9090' OR                             DOT
                       '9790'
           MOVE        '90900000000' TO S-AAU10-ALCIDN.
                 IF    7-FIRST4 NOT = '9000'                            DOT
                       AND '9700' AND '9090'                            AAOLMD
                       AND '9790'
           MOVE        '93000000000' TO S-AAU10-ALCIDN.
                 IF    (7-LAST1 = '5' OR '6' OR                         DOT
                       '7' OR '8' OR '9')                               AAOLMD
                 AND   (7-FIRST4 NOT = '9090' AND                       AAOLMD
                       '9790')
           ADD         5 TO S-AAU10-ALCIDN.
      *K-AAA10-ALCIDN S-AAU10-ALCIDN.                                   DOT
           MOVE        S-AAU10-ALCIDN TO AA10-ALCIDN                    DOT
           PERFORM     F94RA THRU F94RA-FN.
                 IF    IK = '0'                                         DOT
           MOVE        '1' TO AA10-CF.                                  AAOLMD
      ******** END OF PMS AAOLMD ******                                 DOT
      *N35PG.    NOTE * NITIALISE DATE IF NOT FOUND       *.
       F35PG.    IF    AA10-CF NOT = '1'                                lv15
                 NEXT SENTENCE ELSE GO TO     F35PG-FN.
           MOVE        0 TO AA10-DLAUP.
       F35PG-FN. EXIT.
       F35PE-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *FORMAT THE GC01                    *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      **
      *********************************
      **                              *
      **   THIS ROUTINE IS USED TO    *
      **   FORMAT THE DISBURSEMENT    *
      **   ROOT SEGMENT               *
      **                              *
      *********************************
      **
      *N40CO.    NOTE *MOVE FIELDS FROM INPUT             *.
       F40CO.                                                           lv10
           MOVE        K974-CTID TO GC01-CTID
           MOVE        'N' TO GC01-ICUST
           COMPUTE     GC01-DCAG9L = 99999999 -
           M977-DCACG
           MOVE        GC01-DCAG9L TO WS01-DCAG9L
           MOVE        QT58-PRCODA TO GC01-PRCOD
           MOVE        QT58-PRSCD TO GC01-PRSCD
           MOVE        +1 TO GC01-NAASQL
           GC01-NSEQ4B.
       F40CO-FN. EXIT.
       F40-FN.   EXIT.
      *N41.      NOTE *************************************.
      *               *                                   *
      *               *INSERT GC01                        *
      *               *                                   *
      *               *************************************.
       F41.           EXIT.                                             lv05
      *N41DA.    NOTE *INSERT GC01                        *.
       F41DA.                                                           lv15
           PERFORM     F94I1 THRU F94I1-FN.
       F41DA-FN. EXIT.
      *N41EA.    NOTE *IF GC01 ALREADY EXISTS             *.
       F41EA.    IF    XW05-XRC = 'II'                                  lv15
                 NEXT SENTENCE ELSE GO TO     F41EA-FN.
      *N41EF.    NOTE *READ THE ORIGINAL GC01             *.
       F41EF.                                                           lv20
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           PERFORM     F94G1 THRU F94G1-FN.
       F41EF-FN. EXIT.
      *N41EM.    NOTE *IF ORIGINAL SEGMENT FOUND          *.
       F41EM.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F41EM-FN.
      *
      *ENSURE WE HAVE THE CORRECT
      *CUSTODIAL CODE
      *
           MOVE        'N' TO GC01-ICUST.
      *N41FA.    NOTE *IF TRAN DATE MATCHES ORIGINAL      *.
       F41FA.    IF    GC01-NAASQL < +999                               lv25
                 AND   GC01-DCAG9L = WS01-DCAG9L
                 NEXT SENTENCE ELSE GO TO     F41FA-FN.
           ADD         +1 TO GC01-NAASQL.
       F41FA-900. GO TO F41FG-FN.
       F41FA-FN. EXIT.
      *N41FG.    NOTE *ELSE... CHANGE DATE; RESET SEQ     *.
       F41FG.         EXIT.                                             lv25
      *N41FJ.    NOTE *IF GC01 IS AT THE LIMIT            *.
       F41FJ.    IF    GC01-NAASQL = +999                               lv30
                 NEXT SENTENCE ELSE GO TO     F41FJ-FN.
       F41FJ-900. GO TO F41FM-FN.
       F41FJ-FN. EXIT.
      *N41FM.    NOTE *ELSE... FIELD IS OK                *.
       F41FM.                                                           lv30
           MOVE        +1 TO GC01-NAASQL
           MOVE        WS01-DCAG9L TO GC01-DCAG9L.
       F41FM-FN. EXIT.
       F41FG-FN. EXIT.
      *N41FT.    NOTE *SET UPDATE SEQ NUMBER BY 1         *.
       F41FT.                                                           lv25
                 IF    GC01-NSEQ4B > ZERO                               DOT
                 AND   GC01-NSEQ4B < +99999999
      *ADD ONE IF VALID SEQENCE NUMBER
           ADD         +1 TO GC01-NSEQ4B
                 ELSE
           MOVE        +1 TO GC01-NSEQ4B.
       F41FT-FN. EXIT.
      *N41GA.    NOTE *REPLACE THE GC01                   *.
       F41GA.                                                           lv25
           PERFORM     F94I9 THRU F94I9-FN.
       F41GA-FN. EXIT.
      *N41GG.    NOTE *IF REPLACE FAILS - ERROR & EXIT    *.
       F41GG.    IF    IK = '1'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F41GG-FN.
      *---> Send BAD GC01 REPL Message                                  ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013313 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F41GG-FN. EXIT.
       F41EM-900. GO TO F41GT-FN.
       F41EM-FN. EXIT.
      *N41GT.    NOTE *ELSE... NO ORIGINAL; ERROR         *.
       F41GT.                                                           lv20
      *---> Send BAD GC01 READ Message                                  ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013312 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F41GT-FN. EXIT.
       F41EA-FN. EXIT.
       F41-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *FORMAT THE GC03                    *
      *               *                                   *
      *               *************************************.
       F45.                                                             lv05
      **
      *********************************
      **                              *
      **   THIS ROUTINE IS USED TO    *
      **   FORMAT THE DISBURSEMENT    *
      **   ACTIVITY SEGMENT           *
      **                              *
      *********************************
      **
      *N45CA.    NOTE *MOVE CONTROL FIELDS FROM GC01      *.
       F45CA.                                                           lv10
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           MOVE        276 TO GC03-GELL
           MOVE        GC01-DCAG9L TO GC03-DCACG9
           MOVE        GC01-NAASQL TO GC03-NAASQ.
       F45CA-FN. EXIT.
      *N45CG.    NOTE *MOVE CONSTANT FIELDS FOR DISB      *.
       F45CG.                                                           lv10
           MOVE        001 TO GC03-CAATY
           MOVE        014 TO GC03-CACTO
           MOVE        '000000' TO GC03-CATRN
           MOVE        01 TO GC03-CASTC
           MOVE        'Y' TO GC03-IPULL
           MOVE        0001 TO GC03-NBTCH
           MOVE        ZERO TO GC03-DLAUP1
           GC03-ADRET
           GC03-NDRFT
           GC03-CTWHAT
           GC03-PWHLD
           GC03-QNACT
           GC03-ADBRQ
           MOVE        SPACES TO GC03-TTRMS
           GC03-IWTHH
           GC03-IDPAP
           MOVE        ' ' TO GC03-IDELT
           MOVE        'Y' TO GC03-ITRAN
           MOVE        'I' TO GC03-CTRTP
           MOVE        'KD21' TO GC03-CSCRNU
           MOVE        'Y' TO GC03-IPLIN
           MOVE        SPACES TO GC03-GEOPDM
           MOVE        'Y' TO GC03-ITELE
           MOVE        M977-GETIM TO WS00-GETIM
           MOVE        M977-DCACG (7:2) TO GC03-NCONF (1:2)
           MOVE        WS00-GETIM (2:4) TO GC03-NCONF (3:4)
           MOVE        72 TO GC03-NCONF (7:2).
       F45CG-FN. EXIT.
      *N45DA.    NOTE *MOVE FIELDS FROM INPUT             *.
       F45DA.                                                           lv10
           MOVE        M977-GEAUN TO GC03-GEAUN
           MOVE        M977-GEOPD2 TO GC03-GEOPD2
           MOVE        M977-DEFFT TO GC03-DEFFT
           MOVE        M977-GETIM TO GC03-GETIM
      *M977-ADBRQ      GC03-ADBRQ
           MOVE        M977-AEDRQ TO GC03-AEDRQ
           MOVE        QT58-CLIDNB TO GC03-CLIDNB
           MOVE        AA10-DLAUP TO GC03-DLAUP.
       F45DA-FN. EXIT.
      *N45IE.    NOTE *SET TRANSACTION FUNCTION CODE      *.
       F45IE.                                                           lv10
           MOVE        712 TO GC03-CATRF.
       F45IE-FN. EXIT.
      *N45IG.    NOTE *IF NON INNOVEST AND NOT ASSET      *.
       F45IG.    IF    (M977-ALINNO = 0 OR 2)                           lv10
                 AND   K974-CAFLOW = 'IC'
                 AND   TA5B-IVANT = 'N'
                 NEXT SENTENCE ELSE GO TO     F45IG-FN.
      *REALLOCATION AND NOT VANTAGE
      *N45IJ.    NOTE *LOAD CATRF                         *.
       F45IJ.                                                           lv15
      *MOVE 710 IF FIXED TO VARIABLE
                 IF    M977-IANTF = 'FV'                                DOT
           MOVE        710 TO GC03-CATRF
      *MOVE 711 IF VARIABLE TO VARIABLE
                 ELSE
           MOVE        711 TO GC03-CATRF.
       F45IJ-FN. EXIT.
       F45IG-FN. EXIT.
      *N45IM.    NOTE *DEFAULT ISKED FIELD TO 'N'         *.
       F45IM.                                                           lv10
           MOVE        'N' TO GC03-ISKED.
       F45IM-FN. EXIT.
      *N45IN.    NOTE *LOAD ISKED FOR ASSET ALLOCATION    *.
       F45IN.    IF    K974-CAFLOW = 'AA'                               lv10
                 NEXT SENTENCE ELSE GO TO     F45IN-FN.
                 IF    M977-IANTF = 'FV'                                DOT
                 OR    M977-IANTF = 'VF'
           MOVE        'F' TO GC03-ISKED
                 ELSE
           MOVE        'V' TO GC03-ISKED.
       F45IN-FN. EXIT.
       F45-FN.   EXIT.
      *N46.      NOTE *************************************.
      *               *                                   *
      *               *WRITE THE GC03                     *
      *               *                                   *
      *               *************************************.
       F46.           EXIT.                                             lv05
      *N46ID.    NOTE *INSERT GC03                        *.
       F46ID.                       GO TO     F46ID-B.                  lv15
       F46ID-A.
                 IF    IK = '0'
                 OR    GC03-NAASQ = +999
                                    GO TO     F46ID-FN.
       F46ID-B.
           PERFORM     F94I3 THRU F94I3-FN.
      *N46IG.    NOTE *IF ALREADY EXISTS; SET UP SEQ      *.
       F46IG.    IF    XW05-XRC = 'II'                                  lv20
                 NEXT SENTENCE ELSE GO TO     F46IG-FN.
           ADD         +1 TO GC03-NAASQ.
       F46IG-FN. EXIT.
       F46ID-900. GO TO F46ID-A.
       F46ID-FN. EXIT.
       F46-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *PROCESS SUB ACCOUNT TANSFERS       *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50DD.    NOTE *PROCESS FOR INVESTMENT CHANGE      *.
       F50DD.    IF    K974-CAFLOW = 'IC'                               lv10
                 NEXT SENTENCE ELSE GO TO     F50DD-FN.
      *N50DE.    NOTE *INITIALIZE SUBSCRIPTS              *.
       F50DE.                                                           lv15
           MOVE        01 TO WS01-SUB
           MOVE        01 TO WS01-SUB1.
       F50DE-FN. EXIT.
      *N50EE.    NOTE *PROCESS TABLE                      *.
       F50EE.                       GO TO     F50EE-B.                  lv15
       F50EE-A.
                 IF    WS01-SUB > WS01-QSACT
                                    GO TO     F50EE-FN.
       F50EE-B.       EXIT.
      *N50GE.    NOTE *PROCESS FROM ACCOUNTS              *.
       F50GE.    IF    K974-CACCT (WS01-SUB) = 'F'                      lv20
                 NEXT SENTENCE ELSE GO TO     F50GE-FN.
           MOVE        WS01-SUB1 TO GC18-GESQ2
           ADD         1 TO WS01-SUB1
           MOVE        K974-CFIDC (WS01-SUB) TO
           GC18-CFIDC
           MOVE        0 TO GC18-CPHSE
           MOVE        K974-AACTV (WS01-SUB) TO
           GC18-AACTVF
           MOVE        0 TO GC18-QSHOWP
           GC18-PDFND
           MOVE        SPACE TO GC18-IABIN
           MOVE        K974-ADBRQ (WS01-SUB) TO
           GC18-ADBRQF
           MOVE        K974-PACT1 (WS01-SUB) TO
           GC18-PFNDV.
                 IF    K974-IALLV (WS01-SUB) = 'Y'                      DOT
           MOVE        0 TO GC18-ADBRQF
           MOVE        100 TO GC18-PFNDV.
           MOVE        'N' TO GC18-IBALF                                DOT
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           MOVE        GC03-GC03K TO S-GCU03-GC03K.
           PERFORM     F94I8 THRU F94I8-FN.                             DOT
       F50GE-FN. EXIT.
      *N50NI.    NOTE *PROCESS TO ACCOUNTS                *.
       F50NI.    IF    K974-CACCT (WS01-SUB) = 'T'                      lv20
                 NEXT SENTENCE ELSE GO TO     F50NI-FN.
           MOVE        53 TO GC06-GELL
           MOVE        K974-NSEQ4 (WS01-SUB) TO
           GC06-NPISQ
           MOVE        K974-ADBRQ (WS01-SUB) TO
           GC06-ACOTD
           MOVE        K974-PACT1 (WS01-SUB) TO
           GC06-PPOTD
           GC06-PDFND
           MOVE        0 TO GC06-QPSTD
           MOVE        03 TO GC06-CPITC
           MOVE        K974-CFIDC (WS01-SUB) TO
           GC06-CFIDC
           MOVE        0 TO GC06-CPHSE
           MOVE        SPACE TO GC06-IABIN
           MOVE        K974-ITRNB (WS01-SUB) TO
           GC06-ITRNB.
                 IF    GC06-ITRNB = 'Y'                                 DOT
           MOVE        0 TO GC06-PPOTD
           GC06-PDFND
           GC06-ACOTD.
           MOVE        GC01-GC01K TO S-GCU01-GC01K                      DOT
           MOVE        GC03-GC03K TO S-GCU03-GC03K
           PERFORM     F94I6 THRU F94I6-FN.
       F50NI-FN. EXIT.
      *N50NO.    NOTE *UP SUBSCRIPT                       *.
       F50NO.                                                           lv20
           ADD         1 TO WS01-SUB.
       F50NO-FN. EXIT.
       F50EE-900. GO TO F50EE-A.
       F50EE-FN. EXIT.
       F50DD-FN. EXIT.
      *N50PA.    NOTE *ORDER TICKET FIELDS                *.
       F50PA.                                                           lv10
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           MOVE        GC03-GC03K TO S-GCU03-GC03K
           MOVE        W218 TO GC29
           PERFORM     F94SA THRU F94SA-FN.
       F50PA-FN. EXIT.
       F50-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *PROCESS SUB ACCOUNT TANSFERS       *
      *               *                                   *
      *               *************************************.
       F60.           EXIT.                                             lv05
      *N60DD.    NOTE *PROCESS FOR ASSET ALLOCATION       *.
       F60DD.    IF    K974-CAFLOW = 'AA'                               lv10
                 NEXT SENTENCE ELSE GO TO     F60DD-FN.
      *N60DE.    NOTE *INITIALIZE SUBSCRIPT               *.
       F60DE.                                                           lv15
           MOVE        01 TO WS01-SUB.
       F60DE-FN. EXIT.
      *N60EE.    NOTE *PROCESS TABLE                      *.
       F60EE.                       GO TO     F60EE-B.                  lv15
       F60EE-A.
                 IF    WS01-SUB > WS01-QSACT
                                    GO TO     F60EE-FN.
       F60EE-B.       EXIT.
      *N60NI.    NOTE *PROCESS TO ACCOUNTS                *.
       F60NI.    IF    K974-PACT1 (WS01-SUB) > 0                        lv20
                 NEXT SENTENCE ELSE GO TO     F60NI-FN.
           MOVE        53 TO GC06-GELL
           MOVE        K974-NSEQ4 (WS01-SUB) TO
           GC06-NPISQ
           MOVE        K974-ADBRQ (WS01-SUB) TO
           GC06-ACOTD
           MOVE        K974-PACT1 (WS01-SUB) TO
           GC06-PPOTD
           GC06-PDFND
           MOVE        0 TO GC06-QPSTD
           MOVE        03 TO GC06-CPITC
           MOVE        K974-ITRNB (WS01-SUB) TO
           GC06-ITRNB
           MOVE        K974-CFIDC (WS01-SUB) TO
           GC06-CFIDC
           MOVE        0 TO GC06-CPHSE
           MOVE        SPACE TO GC06-IABIN
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           MOVE        GC03-GC03K TO S-GCU03-GC03K
           PERFORM     F94I6 THRU F94I6-FN.
       F60NI-FN. EXIT.
      *N60NO.    NOTE *UP SUBSCRIPT                       *.
       F60NO.                                                           lv20
           ADD         1 TO WS01-SUB.
       F60NO-FN. EXIT.
       F60EE-900. GO TO F60EE-A.
       F60EE-FN. EXIT.
       F60DD-FN. EXIT.
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
      *N92EB.    NOTE *ERROR ON TABLE READ FOR TA5B       *.
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
      *N94G1.    NOTE *CALL GHU ON GC01                   *.            ADU026
       F94G1.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           XA06 GC01                                                    ADU026
           S-GCU01-SSA                                                  ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G1-FN. EXIT.
      *N94I1.    NOTE *CALL ISRT ON GC01                  *.            ADU026
       F94I1.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XA06 GC01                                                    ADU026
           S-GC01-SSA                                                   ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94I1-FN. EXIT.
      *N94I3.    NOTE *CALL ISRT ON GC03                  *.            ADU026
       F94I3.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XA06 GC03                                                    ADU026
           S-GCU01-SSA S-GC03-SSA                                       ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94I3-FN. EXIT.
      *N94I6.    NOTE *CALL ISRT ON GC06                  *.            ADU026
       F94I6.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XA06 GC06                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           S-GC06-SSA
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94I6-FN. EXIT.
      *N94I8.    NOTE *CALL ISRT ON GC18                  *.            ADU026
       F94I8.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XA06 GC18                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           S-GC18-SSA
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94I8-FN. EXIT.
      *N94I9.    NOTE *CALL REPL ON GC01                  *.            ADU026
       F94I9.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           XA06 GC01                                                    ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94I9-FN. EXIT.
      *N94RA.    NOTE *CALL GU ON AA10                    *.            ADU026
       F94RA.                                                           lv10
           MOVE        'LM1P' TO DE10-XDBDNM                            ADU026
           MOVE        'AA10' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XE06 AA10                                                    ADU026
           S-AAU10-SSA                                                  ADU026
           MOVE        XE06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94RA-FN. EXIT.
      *N94SA.    NOTE *CALL ISRT ON GC29                  *.            ADU026
       F94SA.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC29' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XA06 GC29                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           S-GC29-SSA
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94SA-FN. EXIT.
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
