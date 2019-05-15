       IDENTIFICATION DIVISION.                                         CI0314
       PROGRAM-ID.  CI0314P.                                            CI0314
      *AUTHOR.         CHECK FOR DUPLICATE TRANS.                       CI0314
      *DATE-COMPILED.   09/08/14.                                       CI0314
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2007                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE OST   SYSTEM AND ALL INFORMATION RELATING THERETO,    * ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE OST   SYSTEM AND ALL            * ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE OST         * ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 2007                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0314
       CONFIGURATION SECTION.                                           CI0314
       SOURCE-COMPUTER. IBM-370.                                        CI0314
       OBJECT-COMPUTER. IBM-370.                                        CI0314
       DATA DIVISION.                                                   CI0314
       WORKING-STORAGE SECTION.                                         CI0314
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0314
            10            XW05-XW06.                                    CI0314
            11            XW05-XDBPCB.                                  CI0314
            12            XW05-XDBDNM PICTURE  X(08)                    CI0314
                          VALUE                SPACE.                   CI0314
            12            XW05-XSEGLV PICTURE  X(02)                    CI0314
                          VALUE                SPACE.                   CI0314
            12            XW05-XRC    PICTURE  X(02)                    CI0314
                          VALUE                SPACE.                   CI0314
            12            XW05-XPROPT PICTURE  X(04)                    CI0314
                          VALUE                SPACE.                   CI0314
            12            XW05-FILLER PICTURE  S9(5)                    CI0314
                          VALUE                ZERO                     CI0314
                          BINARY.                                       CI0314
            12            XW05-XSEGNM PICTURE  X(08)                    CI0314
                          VALUE                SPACE.                   CI0314
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0314
                          VALUE                ZERO                     CI0314
                          BINARY.                                       CI0314
            12            XW05-XSEGNB PICTURE  9(05)                    CI0314
                          VALUE                ZERO                     CI0314
                          BINARY.                                       CI0314
            12            XW05-XCOKEY PICTURE  X(70)                    CI0314
                          VALUE                SPACE.                   CI0314
            10            XW05-XW07.                                    CI0314
            11            XW05-XIOPCB.                                  CI0314
            12            XW05-XTERMI PICTURE  X(08)                    CI0314
                          VALUE                SPACE.                   CI0314
            12            XW05-FILLER PICTURE  XX                       CI0314
                          VALUE                SPACE.                   CI0314
            12            XW05-XRC1   PICTURE  X(02)                    CI0314
                          VALUE                SPACE.                   CI0314
            12            XW05-FILLER PICTURE  X(12)                    CI0314
                          VALUE                SPACE.                   CI0314
            12            XW05-XMODNM PICTURE  X(8)                     CI0314
                          VALUE                SPACE.                   CI0314
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0314
                          VALUE                ZERO.                    CI0314
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0314
                          VALUE                ZERO.                    CI0314
            10            XW05-XGU    PICTURE  X(4)                     CI0314
                          VALUE                'GU  '.                  CI0314
            10            XW05-XGHU   PICTURE  X(4)                     CI0314
                          VALUE                'GHU '.                  CI0314
            10            XW05-XGN    PICTURE  X(4)                     CI0314
                          VALUE                'GN  '.                  CI0314
            10            XW05-XGHN   PICTURE  X(4)                     CI0314
                          VALUE                'GHN '.                  CI0314
            10            XW05-XGNP   PICTURE  X(4)                     CI0314
                          VALUE                'GNP '.                  CI0314
            10            XW05-XGHNP  PICTURE  X(4)                     CI0314
                          VALUE                'GHNP'.                  CI0314
            10            XW05-XREPL  PICTURE  XXXX                     CI0314
                          VALUE                'REPL'.                  CI0314
            10            XW05-XISRT  PICTURE  X(4)                     CI0314
                          VALUE                'ISRT'.                  CI0314
            10            XW05-XDLET  PICTURE  X(4)                     CI0314
                          VALUE                'DLET'.                  CI0314
            10            XW05-XOPEN  PICTURE  X(4)                     CI0314
                          VALUE                'OPEN'.                  CI0314
            10            XW05-XCLSE  PICTURE  X(4)                     CI0314
                          VALUE                'CLSE'.                  CI0314
            10            XW05-XCHKP  PICTURE  X(4)                     CI0314
                          VALUE                'CHKP'.                  CI0314
            10            XW05-XXRST  PICTURE  X(4)                     CI0314
                          VALUE                'XRST'.                  CI0314
            10            XW05-XTERM  PICTURE  X(4)                     CI0314
                          VALUE                'TERM'.                  CI0314
            10            XW05-XNFPAC PICTURE  X(13)                    CI0314
                          VALUE                SPACE.                   CI0314
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0314
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0314
       01                 GC01.                                         CI0314
            10            GC01-GC01K.                                   CI0314
            11            GC01-C299.                                    CI0314
            12            GC01-CTID.                                    CI0314
            13            GC01-CTIDA  PICTURE  9(3).                    CI0314
            13            GC01-CTIDN.                                   CI0314
            14            GC01-CTIDNP PICTURE  X(13).                   CI0314
            14            GC01-CTIDND PICTURE  9(11).                   CI0314
            10            GC01-DCAG9L PICTURE  9(8).                    CI0314
            10            GC01-NAASQL PICTURE  S9(3)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            10            GC01-ICUST  PICTURE  X.                       CI0314
            10            GC01-NSEQ4B PICTURE  9(8)                     CI0314
                          BINARY.                                       CI0314
            10            GC01-PRCOD  PICTURE  9(5).                    CI0314
            10            GC01-PRSCD  PICTURE  X(9).                    CI0314
            10            GC01-FILLER PICTURE  X(8).                    CI0314
       01                 GC03.                                         CI0314
            10            GC03-GELL   PICTURE  9(4)                     CI0314
                          BINARY.                                       CI0314
            10            GC03-GD00.                                    CI0314
            11            GC03-GC03K.                                   CI0314
            12            GC03-DCACG9 PICTURE  9(8).                    CI0314
            12            GC03-NAASQ  PICTURE  S9(3)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-CAATY  PICTURE  9(3).                    CI0314
            11            GC03-CVSYS  PICTURE  X(2).                    CI0314
            11            GC03-CACTO  PICTURE  9(3).                    CI0314
            11            GC03-CATRN.                                   CI0314
            12            GC03-CATRF  PICTURE  9(3).                    CI0314
            12            GC03-CATRS  PICTURE  9(3).                    CI0314
            11            GC03-CASTC  PICTURE  99.                      CI0314
            11            GC03-IPULL  PICTURE  X.                       CI0314
            11            GC03-GEAUN  PICTURE  9(5).                    CI0314
            11            GC03-GEOPD2 PICTURE  X(8).                    CI0314
            11            GC03-NBTCH  PICTURE  9(4).                    CI0314
            11            GC03-DEFFT  PICTURE  9(8).                    CI0314
            11            GC03-NSUNT  PICTURE  9(4).                    CI0314
            11            GC03-ITRAN  PICTURE  X.                       CI0314
            11            GC03-DLAUP1 PICTURE  9(8).                    CI0314
            11            GC03-ADRET  PICTURE  S9(11)V99                CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-TTRMS  PICTURE  X(12).                   CI0314
            11            GC03-IDELT  PICTURE  X.                       CI0314
            11            GC03-GEOPDM PICTURE  X(8).                    CI0314
            11            GC03-FILLER PICTURE  X(07).                   CI0314
            10            GC03-GD09.                                    CI0314
            11            GC03-FILLER PICTURE  X(70).                   CI0314
            10            GC03-GD01                                     CI0314
                          REDEFINES            GC03-GD09.               CI0314
            11            GC03-ADBRQ  PICTURE  S9(11)V99                CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-CTRTP  PICTURE  X(2).                    CI0314
            11            GC03-CPORT  PICTURE  X.                       CI0314
            11            GC03-CSCRNU PICTURE  X(4).                    CI0314
            11            GC03-DLAUP  PICTURE  9(8).                    CI0314
            11            GC03-CTWHAT PICTURE  S9(7)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-PWHLD  PICTURE  S999V9(5)                CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-IWTHH  PICTURE  X.                       CI0314
            11            GC03-NDRFT  PICTURE  9(5).                    CI0314
            11            GC03-IDPAP  PICTURE  X.                       CI0314
            11            GC03-GETIM  PICTURE  S9(7)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-QNACT  PICTURE  9(3).                    CI0314
            11            GC03-AEDRQ  PICTURE  S9(11)V99                CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-IPLIN  PICTURE  X.                       CI0314
            11            GC03-CLIDNB PICTURE  9(8).                    CI0314
            11            GC03-CSLCT  PICTURE  X.                       CI0314
            11            GC03-ITELE  PICTURE  X.                       CI0314
            11            GC03-FILLER PICTURE  X(06).                   CI0314
            10            GC03-GD02                                     CI0314
                          REDEFINES            GC03-GD09.               CI0314
            11            GC03-CSYST  PICTURE  99.                      CI0314
            11            GC03-FILLER PICTURE  X.                       CI0314
            11            GC03-ACASH  PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-DTRAC  PICTURE  9(8).                    CI0314
            11            GC03-CTRSO  PICTURE  9(02).                   CI0314
            11            GC03-NTRCE  PICTURE  9(06).                   CI0314
            11            GC03-GECKD1 PICTURE  9.                       CI0314
            11            GC03-CCOLL  PICTURE  X(3).                    CI0314
            11            GC03-CLTDP  PICTURE  X(3).                    CI0314
            11            GC03-PSLLD  PICTURE  S99V999                  CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-ISLOR  PICTURE  X.                       CI0314
            11            GC03-ITPAC  PICTURE  X.                       CI0314
            11            GC03-CPMTCA PICTURE  XXX.                     CI0314
            11            GC03-CSERV  PICTURE  X(3).                    CI0314
            11            GC03-ACOMO  PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-IPLIN1 PICTURE  X.                       CI0314
            11            GC03-INQEX  PICTURE  X.                       CI0314
            11            GC03-CTKRAA PICTURE  X(12).                   CI0314
            11            GC03-CCSMQ  PICTURE  X.                       CI0314
            11            GC03-IVAEX1 PICTURE  X.                       CI0314
            11            GC03-IHPMT  PICTURE  X(1).                    CI0314
            11            GC03-GETIM3 PICTURE  S9(7)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            10            GC03-GD03                                     CI0314
                          REDEFINES            GC03-GD09.               CI0314
            11            GC03-CATRNC PICTURE  9(6).                    CI0314
            11            GC03-APRNT1 PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-QSHOWT PICTURE  S9(10)V999               CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-ACINVT PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-ACOMO7 PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-QSHOMW PICTURE  S9(10)V999               CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-ATAXT3 PICTURE  S9(11)V99                CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-CTSTR  PICTURE  9(2).                    CI0314
            11            GC03-ICIRA  PICTURE  X.                       CI0314
            11            GC03-GETIM2 PICTURE  S9(7)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-CPMTCX PICTURE  XX.                      CI0314
            11            GC03-FILLER PICTURE  X(16).                   CI0314
            10            GC03-GD99.                                    CI0314
            11            GC03-FILLER PICTURE  X(248).                  CI0314
            10            GC03-GD10                                     CI0314
                          REDEFINES            GC03-GD99.               CI0314
            11            GC03-MROTC  PICTURE  X(7).                    CI0314
            11            GC03-CEDSC  PICTURE  9(1).                    CI0314
            11            GC03-ILPOI  PICTURE  X(1).                    CI0314
            11            GC03-AWRCH  PICTURE  S9(3)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-CHCOC1 PICTURE  9(2).                    CI0314
            11            GC03-CHCOC2 PICTURE  9(2).                    CI0314
            11            GC03-CHCOC3 PICTURE  9(2).                    CI0314
            11            GC03-CHCOC4 PICTURE  9(2).                    CI0314
            11            GC03-CMCOC1 PICTURE  9(3).                    CI0314
            11            GC03-CMCOC2 PICTURE  9(3).                    CI0314
            11            GC03-CMCOC3 PICTURE  9(3).                    CI0314
            11            GC03-GD11.                                    CI0314
            12            GC03-FILLER PICTURE  X(219).                  CI0314
            11            GC03-GD12                                     CI0314
                          REDEFINES            GC03-GD11.               CI0314
            12            GC03-CELLO  PICTURE  9(1).                    CI0314
            12            GC03-CECLO  PICTURE  9(1).                    CI0314
            12            GC03-AEXML  PICTURE  S9(7)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-CEPI   PICTURE  X(1).                    CI0314
            12            GC03-CEXTY  PICTURE  X.                       CI0314
            12            GC03-CROPC  PICTURE  9(1).                    CI0314
            12            GC03-CPUTY  PICTURE  9(1).                    CI0314
            12            GC03-IMCII  PICTURE  X(1).                    CI0314
            12            GC03-GEMISC                                   CI0314
                          OCCURS       010     TIMES.                   CI0314
            13            GC03-AMGLA  PICTURE  S9(7)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            13            GC03-CMGLC  PICTURE  9(1).                    CI0314
            13            GC03-NMGLN  PICTURE  9(4).                    CI0314
            12            GC03-ACTRN  PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-IWRBK  PICTURE  X.                       CI0314
            12            GC03-IFEDX  PICTURE  X.                       CI0314
            12            GC03-ICNTR  PICTURE  X.                       CI0314
            12            GC03-IOCKH  PICTURE  X.                       CI0314
            12            GC03-ICRCK  PICTURE  X.                       CI0314
            12            GC03-NHMPN  PICTURE  S9(10)                   CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-ITELR1 PICTURE  X.                       CI0314
            11            GC03-GD13                                     CI0314
                          REDEFINES            GC03-GD11.               CI0314
            12            GC03-DREDO  PICTURE  9(8).                    CI0314
            12            GC03-CATRNR PICTURE  9(6).                    CI0314
            12            GC03-CEVN   PICTURE  9(9).                    CI0314
            12            GC03-ISUSP  PICTURE  X(1).                    CI0314
            11            GC03-GD15                                     CI0314
                          REDEFINES            GC03-GD11.               CI0314
            12            GC03-CPUTZ  PICTURE  9(1).                    CI0314
            12            GC03-CETLB  PICTURE  9(3).                    CI0314
            12            GC03-QTRMC  PICTURE  9(3).                    CI0314
            12            GC03-DEFFTE PICTURE  9(8).                    CI0314
            12            GC03-DEFFTF PICTURE  9(8).                    CI0314
            12            GC03-DEFFTG PICTURE  9(8).                    CI0314
            12            GC03-XZ1A   PICTURE  X.                       CI0314
            12            GC03-XZ1B   PICTURE  X.                       CI0314
            12            GC03-XZ1C   PICTURE  X.                       CI0314
            12            GC03-XZ1D   PICTURE  X.                       CI0314
            12            GC03-XZ1E   PICTURE  X.                       CI0314
            12            GC03-XZ1F   PICTURE  X.                       CI0314
            12            GC03-XZ1G   PICTURE  X.                       CI0314
            12            GC03-XZ1H   PICTURE  X.                       CI0314
            12            GC03-XZ1I   PICTURE  X.                       CI0314
            12            GC03-DEFFTH PICTURE  9(8).                    CI0314
            11            GC03-GD19                                     CI0314
                          REDEFINES            GC03-GD11.               CI0314
            12            GC03-GD11.                                    CI0314
            13            GC03-FILLER PICTURE  X(219).                  CI0314
            10            GC03-GD20                                     CI0314
                          REDEFINES            GC03-GD99.               CI0314
            11            GC03-ADDACT PICTURE  S9(7)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-ISIGV  PICTURE  X.                       CI0314
            11            GC03-IALLF  PICTURE  X.                       CI0314
            11            GC03-QSHOWQ PICTURE  S9(9)V999                CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-CCDSCW PICTURE  9(2).                    CI0314
            11            GC03-IDWRL  PICTURE  X.                       CI0314
            11            GC03-ITELR  PICTURE  X.                       CI0314
            11            GC03-IABIN  PICTURE  X.                       CI0314
            11            GC03-PACT1  PICTURE  S999V999                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-IBFAF  PICTURE  X.                       CI0314
            11            GC03-IFRSA  PICTURE  X.                       CI0314
            11            GC03-ICRCAN PICTURE  X.                       CI0314
            11            GC03-ACACTV PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-AGFND  PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-QCSHOW PICTURE  S9(9)V999                CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-QCSHIS PICTURE  S9(9)V999                CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-NDTRC  PICTURE  9(8).                    CI0314
            11            GC03-CAERU  PICTURE  X(4).                    CI0314
            11            GC03-IFDGO  PICTURE  X.                       CI0314
            11            GC03-PSLLD2 PICTURE  S99V999                  CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-ISLOR2 PICTURE  X.                       CI0314
            11            GC03-QSFIO  PICTURE  S9(10)V999               CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-QSFID  PICTURE  S9(10)V999               CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-CGDIN  PICTURE  X.                       CI0314
            11            GC03-DGDIN  PICTURE  9(8).                    CI0314
            10            GC03-GD30                                     CI0314
                          REDEFINES            GC03-GD99.               CI0314
            11            GC03-ISKED  PICTURE  X.                       CI0314
            11            GC03-CENXC  PICTURE  9(2).                    CI0314
            11            GC03-GD31.                                    CI0314
            12            GC03-FILLER PICTURE  X(245).                  CI0314
            11            GC03-GD32                                     CI0314
                          REDEFINES            GC03-GD31.               CI0314
            12            GC03-IABIN1 PICTURE  X.                       CI0314
            12            GC03-CLDOD  PICTURE  9(8).                    CI0314
            12            GC03-NCLAM  PICTURE  9(5)                     CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-ISURR  PICTURE  X.                       CI0314
            12            GC03-GEHCD  PICTURE  9(3).                    CI0314
            12            GC03-CRATC  PICTURE  9(4).                    CI0314
            12            GC03-AMAXD  PICTURE  S9(7)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-ASCHGA PICTURE  S9(7)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-APYOM  PICTURE  S9(7)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-IWTHH1 PICTURE  X.                       CI0314
            12            GC03-CPAYCL PICTURE  X(2).                    CI0314
            12            GC03-CTSAO  PICTURE  X.                       CI0314
            12            GC03-NCONF  PICTURE  9(08).                   CI0314
            12            GC03-CLID   PICTURE  X(23).                   CI0314
            12            GC03-CARTY  PICTURE  99.                      CI0314
            12            GC03-NARRS  PICTURE  S9(3)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-CARTZ  PICTURE  99.                      CI0314
            12            GC03-NAPDS  PICTURE  S9(3)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-CPMTO  PICTURE  X.                       CI0314
            12            GC03-DNPMT  PICTURE  9(8).                    CI0314
            12            GC03-IPCTV  PICTURE  X.                       CI0314
            12            GC03-IMECH  PICTURE  X(01).                   CI0314
            12            GC03-IMVAO  PICTURE  X(1).                    CI0314
            12            GC03-AMVA1  PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-CACTS  PICTURE  X.                       CI0314
            12            GC03-CTSPP  PICTURE  X(1).                    CI0314
            12            GC03-CACT4  PICTURE  X(2).                    CI0314
            12            GC03-IVAEX  PICTURE  X.                       CI0314
            12            GC03-DFPMT  PICTURE  9(8).                    CI0314
            12            GC03-IDEMD  PICTURE  X.                       CI0314
            12            GC03-IOFST  PICTURE  X.                       CI0314
            12            GC03-AMXLB  PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-ACULB  PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-DEIRNB PICTURE  9(8).                    CI0314
            12            GC03-DEFFE  PICTURE  9(8).                    CI0314
            12            GC03-DEFFR  PICTURE  9(8).                    CI0314
            12            GC03-ISPUP  PICTURE  X.                       CI0314
            12            GC03-CPNCG  PICTURE  X.                       CI0314
            12            GC03-IEXPU  PICTURE  X.                       CI0314
            12            GC03-IPPCF  PICTURE  X.                       CI0314
            12            GC03-NAAPT  PICTURE  9(2).                    CI0314
            12            GC03-PWHLDS PICTURE  S999V9(5)                CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-ISWHO  PICTURE  X(1).                    CI0314
            11            GC03-GD33                                     CI0314
                          REDEFINES            GC03-GD31.               CI0314
            12            GC03-CPAYC  PICTURE  X(2).                    CI0314
            12            GC03-ADBRQX PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-ADBRQV PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-APTXR  PICTURE  S9(7)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-CTRTPE PICTURE  X(2).                    CI0314
            12            GC03-NCLAMI PICTURE  S9(9)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-CLIDO8 PICTURE  S9(3)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-CLIDN  PICTURE  X(20).                   CI0314
            12            GC03-DSET01 PICTURE  S9(8)                    CI0314
                          BINARY.                                       CI0314
            12            GC03-CTSET1 PICTURE  S9(6)                    CI0314
                          BINARY.                                       CI0314
            12            GC03-DSET02 PICTURE  S9(8)                    CI0314
                          BINARY.                                       CI0314
            12            GC03-CTSET2 PICTURE  S9(6)                    CI0314
                          BINARY.                                       CI0314
            11            GC03-GD34                                     CI0314
                          REDEFINES            GC03-GD31.               CI0314
            12            GC03-QNOFM  PICTURE  S9(3)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-CLTRM  PICTURE  99.                      CI0314
            12            GC03-AMXLN  PICTURE  S9(7)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-ALADJ  PICTURE  S9(7)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-ACHK   PICTURE  S9(7)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-APRMO  PICTURE  S9(7)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-IMECH1 PICTURE  X(01).                   CI0314
            12            GC03-CACT41 PICTURE  X(2).                    CI0314
            12            GC03-ACDSCC PICTURE  S9(05)V99                CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-ACDSCD PICTURE  S9(05)V99                CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-GD39                                     CI0314
                          REDEFINES            GC03-GD31.               CI0314
            12            GC03-GD31.                                    CI0314
            13            GC03-FILLER PICTURE  X(245).                  CI0314
            10            GC03-GD40                                     CI0314
                          REDEFINES            GC03-GD99.               CI0314
            11            GC03-NTR    PICTURE  9(8).                    CI0314
            11            GC03-NPBNC  PICTURE  X(24).                   CI0314
            11            GC03-CRREV  PICTURE  X(3).                    CI0314
            11            GC03-CSUSL  PICTURE  S9.                      CI0314
            11            GC03-NMGLN1 PICTURE  9(4).                    CI0314
            11            GC03-DCAC92 PICTURE  9(8).                    CI0314
            11            GC03-NAASQ3 PICTURE  S9(3)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-GD49.                                    CI0314
            12            GC03-FILLER PICTURE  X(198).                  CI0314
            11            GC03-GD41                                     CI0314
                          REDEFINES            GC03-GD49.               CI0314
            12            GC03-CRREF  PICTURE  9(2).                    CI0314
            12            GC03-CORIR  PICTURE  X(02).                   CI0314
            12            GC03-CIPDB  PICTURE  X(03).                   CI0314
            12            GC03-CPAYH  PICTURE  X(02).                   CI0314
            12            GC03-NAMEX  PICTURE  9(15)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC03-DCHAE  PICTURE  9(4).                    CI0314
            12            GC03-DRQST  PICTURE  S9(8)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-GD42                                     CI0314
                          REDEFINES            GC03-GD49.               CI0314
            12            GC03-CPMTCB PICTURE  X(3).                    CI0314
            10            GC03-GD50                                     CI0314
                          REDEFINES            GC03-GD99.               CI0314
            11            GC03-ALOAD  PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-PSLLD4 PICTURE  S99V999                  CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-CSUSL1 PICTURE  S9.                      CI0314
            11            GC03-CRREV1 PICTURE  X(3).                    CI0314
            11            GC03-ADDAC  PICTURE  S9(7)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-DL13.                                    CI0314
            12            GC03-GEYR   PICTURE  9(4).                    CI0314
            12            GC03-GEMTH  PICTURE  99.                      CI0314
            12            GC03-NDAY   PICTURE  99.                      CI0314
            11            GC03-NSEQ3P PICTURE  S9(5)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-XZ6A   PICTURE  X(6).                    CI0314
            11            GC03-XZ7    PICTURE  X(7).                    CI0314
            11            GC03-XZ6B   PICTURE  X(6).                    CI0314
            11            GC03-XZ6    PICTURE  X(6).                    CI0314
            11            GC03-XZ6C   PICTURE  X(6).                    CI0314
            11            GC03-XZ20   PICTURE  X(20).                   CI0314
            11            GC03-CATRN1 PICTURE  9(6).                    CI0314
            11            GC03-ADDAC2 PICTURE  S9(7)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-ATAXT2 PICTURE  S9(11)V99                CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-ACOMOT PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-XZ5    PICTURE  X(5).                    CI0314
            11            GC03-IREVD  PICTURE  X(1).                    CI0314
            11            GC03-ISUSP1 PICTURE  X(1).                    CI0314
            11            GC03-XZ6D   PICTURE  X(6).                    CI0314
            11            GC03-XZ13   PICTURE  X(13).                   CI0314
            11            GC03-CWHTP2 PICTURE  X(3).                    CI0314
            11            GC03-CWHTP3 PICTURE  X(3).                    CI0314
            11            GC03-DTREN  PICTURE  9(8).                    CI0314
            11            GC03-NAASQ1 PICTURE  S9(3)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            10            GC03-GD51                                     CI0314
                          REDEFINES            GC03-GD99.               CI0314
            11            GC03-ADOMOT PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-ACGLT  PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-ACGST  PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-CTXMT  PICTURE  9(2).                    CI0314
            11            GC03-ALOAD3 PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-FILLER PICTURE  X(31).                   CI0314
            10            GC03-GD52                                     CI0314
                          REDEFINES            GC03-GD99.               CI0314
            11            GC03-DEFFT5 PICTURE  9(8).                    CI0314
            11            GC03-PSLLD5 PICTURE  S99V999                  CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-CSUSL2 PICTURE  S9.                      CI0314
            11            GC03-ALOAD2 PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-DL22.                                    CI0314
            12            GC03-NYEAR1 PICTURE  9(4).                    CI0314
            12            GC03-GEMTHA PICTURE  99.                      CI0314
            12            GC03-NDAY01 PICTURE  99.                      CI0314
            11            GC03-NSEQ3R PICTURE  S9(5)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-CWHTP  PICTURE  X(3).                    CI0314
            11            GC03-CWHFR  PICTURE  X(3).                    CI0314
            11            GC03-CATRN7 PICTURE  9(6).                    CI0314
            11            GC03-ATAXT5 PICTURE  S9(11)V99                CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-QSHOT  PICTURE  S9(10)V999               CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-ACINT3 PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-CWHTP1 PICTURE  X(3).                    CI0314
            11            GC03-CWHFR1 PICTURE  X(3).                    CI0314
            11            GC03-ACOMO5 PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-QSHOMU PICTURE  S9(10)V999               CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-ACASH1 PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-FILLER PICTURE  X(04).                   CI0314
            11            GC03-CATRN8 PICTURE  9(6).                    CI0314
            11            GC03-ALOAD1 PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-PSLLD1 PICTURE  S99V999                  CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-QSHOT1 PICTURE  S9(10)V999               CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-ACINT4 PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-CSUSL4 PICTURE  S9.                      CI0314
            11            GC03-ACOMO4 PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            10            GC03-GD60                                     CI0314
                          REDEFINES            GC03-GD99.               CI0314
            11            GC03-GEOPDD PICTURE  X(8)                     CI0314
                          OCCURS       005     TIMES.                   CI0314
            11            GC03-DLAUP3 PICTURE  9(8)                     CI0314
                          OCCURS       005     TIMES.                   CI0314
            11            GC03-GEOPDB PICTURE  X(8).                    CI0314
            11            GC03-DLAUP4 PICTURE  9(8).                    CI0314
            11            GC03-ITELR2 PICTURE  X.                       CI0314
            11            GC03-IPMTA  PICTURE  X.                       CI0314
            11            GC03-CCSMG  PICTURE  X.                       CI0314
            11            GC03-CPLEC  PICTURE  XX.                      CI0314
            11            GC03-CORTYA PICTURE  X(3).                    CI0314
            11            GC03-CACTBC PICTURE  X(1).                    CI0314
            11            GC03-CGSPIA PICTURE  X.                       CI0314
            11            GC03-IPTRDA PICTURE  X(01).                   CI0314
            11            GC03-GCUSPY PICTURE  X(12).                   CI0314
            11            GC03-CPALLA PICTURE  X(1).                    CI0314
            11            GC03-QSHO5A PICTURE  S9(9)V999                CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-IFRSAB PICTURE  X.                       CI0314
            11            GC03-DELOI  PICTURE  9(8).                    CI0314
            11            GC03-IAROAA PICTURE  X.                       CI0314
            11            GC03-ACINVR PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-ILTINA PICTURE  X.                       CI0314
            11            GC03-ALOIDA PICTURE  S9(11)V99                CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC03-CFUNTA PICTURE  X(2).                    CI0314
            11            GC03-CLGND  PICTURE  X.                       CI0314
            11            GC03-CPH3U  PICTURE  X.                       CI0314
            11            GC03-GESTD  PICTURE  9(8).                    CI0314
            11            GC03-GEEND  PICTURE  9(8).                    CI0314
            11            GC03-CPMTF  PICTURE  99.                      CI0314
            11            GC03-CNAVR  PICTURE  X(1).                    CI0314
            10            GC03-GD70                                     CI0314
                          REDEFINES            GC03-GD99.               CI0314
            11            GC03-CMEMO  PICTURE  X(2).                    CI0314
            11            GC03-ALPLDT PICTURE  9(8).                    CI0314
            11            GC03-CTLPD  PICTURE  9(8).                    CI0314
            11            GC03-CPAYCM PICTURE  X(2).                    CI0314
       01                 GC06.                                         CI0314
            10            GC06-GELL   PICTURE  9(4)                     CI0314
                          BINARY.                                       CI0314
            10            GC06-GE00.                                    CI0314
            11            GC06-GC06K.                                   CI0314
            12            GC06-NPISQ  PICTURE  S9(3)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC06-ACOTD  PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC06-PPOTD  PICTURE  S9(3)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC06-QPSTD  PICTURE  S9(7)V999                CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC06-CPITC  PICTURE  99.                      CI0314
            11            GC06-ITRNB  PICTURE  X.                       CI0314
            11            GC06-FILLER PICTURE  X(14).                   CI0314
            10            GC06-GE98.                                    CI0314
            11            GC06-FILLER PICTURE  X(240).                  CI0314
            10            GC06-GE10                                     CI0314
                          REDEFINES            GC06-GE98.               CI0314
            11            GC06-CDELI  PICTURE  9(3).                    CI0314
            11            GC06-CPAYC  PICTURE  X(2).                    CI0314
            11            GC06-ICHKP  PICTURE  X.                       CI0314
            11            GC06-CLTIN  PICTURE  9(12).                   CI0314
            11            GC06-IFHAI  PICTURE  X.                       CI0314
            11            GC06-CDQUA  PICTURE  X(2).                    CI0314
            11            GC06-FILLER PICTURE  X(07).                   CI0314
            11            GC06-GE99.                                    CI0314
            12            GC06-FILLER PICTURE  X(212).                  CI0314
            11            GC06-GE01                                     CI0314
                          REDEFINES            GC06-GE99.               CI0314
            12            GC06-NTR    PICTURE  9(8).                    CI0314
            12            GC06-GECKD  PICTURE  9.                       CI0314
            12            GC06-NPBN   PICTURE  X(20).                   CI0314
            12            GC06-CCBAT  PICTURE  99.                      CI0314
            12            GC06-CLID4  PICTURE  X(23).                   CI0314
            12            GC06-GENAL1 PICTURE  X(30)                    CI0314
                          OCCURS       002     TIMES.                   CI0314
            12            GC06-GESAD1 PICTURE  X(30)                    CI0314
                          OCCURS       003     TIMES.                   CI0314
            11            GC06-GE02                                     CI0314
                          REDEFINES            GC06-GE99.               CI0314
            12            GC06-GENAL  PICTURE  X(30)                    CI0314
                          OCCURS       002     TIMES.                   CI0314
            12            GC06-GESAD  PICTURE  X(30)                    CI0314
                          OCCURS       003     TIMES.                   CI0314
            11            GC06-GE03                                     CI0314
                          REDEFINES            GC06-GE99.               CI0314
            12            GC06-NCHKN  PICTURE  9(11).                   CI0314
            11            GC06-GE04                                     CI0314
                          REDEFINES            GC06-GE99.               CI0314
            12            GC06-CTIDAP PICTURE  9(3).                    CI0314
            12            GC06-PRCOD  PICTURE  9(5).                    CI0314
            12            GC06-TDELI  PICTURE  X(30).                   CI0314
            12            GC06-CINCD  PICTURE  9(02).                   CI0314
            10            GC06-GE20                                     CI0314
                          REDEFINES            GC06-GE98.               CI0314
            11            GC06-C299.                                    CI0314
            12            GC06-CTID.                                    CI0314
            13            GC06-CTIDA  PICTURE  9(3).                    CI0314
            13            GC06-CTIDN.                                   CI0314
            14            GC06-CTIDNP PICTURE  X(13).                   CI0314
            14            GC06-CTIDND PICTURE  9(11).                   CI0314
            11            GC06-DCACG9 PICTURE  9(8).                    CI0314
            11            GC06-NAASQ  PICTURE  S9(3)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            GC06-CIRAP  PICTURE  XX.                      CI0314
            11            GC06-CTYPE  PICTURE  X.                       CI0314
            11            GC06-INACT  PICTURE  X.                       CI0314
            11            GC06-FILLER PICTURE  X(01).                   CI0314
            11            GC06-ITPAC  PICTURE  X.                       CI0314
            11            GC06-ITAXI  PICTURE  X.                       CI0314
            11            GC06-IOWNC  PICTURE  X.                       CI0314
            11            GC06-CDVCD  PICTURE  X(2).                    CI0314
            11            GC06-CTCUS  PICTURE  999.                     CI0314
            11            GC06-CPMTCB PICTURE  X(3).                    CI0314
            11            GC06-CASTC1 PICTURE  99.                      CI0314
            11            GC06-PRCOD1 PICTURE  9(5).                    CI0314
            11            GC06-CPRSC1 PICTURE  X(9).                    CI0314
            11            GC06-CPRTB  PICTURE  X.                       CI0314
            11            GC06-CBRKD  PICTURE  9(4).                    CI0314
            11            GC06-FILLER PICTURE  X(12).                   CI0314
            10            GC06-GE30                                     CI0314
                          REDEFINES            GC06-GE98.               CI0314
            11            GC06-CFIDC  PICTURE  X(5).                    CI0314
            11            GC06-CPHSE  PICTURE  9(2).                    CI0314
            11            GC06-FILLER PICTURE  X(05).                   CI0314
            11            GC06-IABIN  PICTURE  X.                       CI0314
            11            GC06-PDFND  PICTURE  S999V9(3)                CI0314
                          COMPUTATIONAL-3.                              CI0314
            10            GC06-GE40                                     CI0314
                          REDEFINES            GC06-GE98.               CI0314
            11            GC06-CACCT  PICTURE  X.                       CI0314
            11            GC06-CPAYR  PICTURE  X(2).                    CI0314
            11            GC06-CDELI1 PICTURE  9(3).                    CI0314
            11            GC06-CATRN.                                   CI0314
            12            GC06-CATRF  PICTURE  9(3).                    CI0314
            12            GC06-CATRS  PICTURE  9(3).                    CI0314
            11            GC06-DEFFT  PICTURE  9(8).                    CI0314
            11            GC06-CTYPC  PICTURE  X.                       CI0314
            11            GC06-CIRAPA PICTURE  XX.                      CI0314
            11            GC06-FILLER PICTURE  X(09).                   CI0314
            11            GC06-GE49.                                    CI0314
            12            GC06-FILLER PICTURE  X(208).                  CI0314
            11            GC06-GE41                                     CI0314
                          REDEFINES            GC06-GE49.               CI0314
            12            GC06-NCHKN1 PICTURE  9(6).                    CI0314
            11            GC06-GE42                                     CI0314
                          REDEFINES            GC06-GE49.               CI0314
            12            GC06-CTID1.                                   CI0314
            13            GC06-CTIDA1 PICTURE  9(3).                    CI0314
            13            GC06-CTIDP1 PICTURE  X(13).                   CI0314
            13            GC06-CTIDN1 PICTURE  9(11).                   CI0314
            11            GC06-GE43                                     CI0314
                          REDEFINES            GC06-GE49.               CI0314
            12            GC06-GENAL2 PICTURE  X(30)                    CI0314
                          OCCURS       002     TIMES.                   CI0314
            12            GC06-GESAD2 PICTURE  X(30)                    CI0314
                          OCCURS       003     TIMES.                   CI0314
            11            GC06-GE44                                     CI0314
                          REDEFINES            GC06-GE49.               CI0314
            12            GC06-CTID01.                                  CI0314
            13            GC06-CTIDA6 PICTURE  9(3).                    CI0314
            13            GC06-NTIDP2 PICTURE  X(13).                   CI0314
            13            GC06-CTIDN2 PICTURE  9(11).                   CI0314
            12            GC06-GECKD2 PICTURE  9.                       CI0314
            12            GC06-PACCT  PICTURE  S999V99                  CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC06-PLOAN  PICTURE  S999V99                  CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC06-PADPT  PICTURE  S999V99                  CI0314
                          COMPUTATIONAL-3.                              CI0314
            12            GC06-IPCTL  PICTURE  X.                       CI0314
            12            GC06-IPCTP  PICTURE  X.                       CI0314
            12            GC06-CEUNT  PICTURE  S9(5)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            10            GC06-GE31                                     CI0314
                          REDEFINES            GC06-GE98.               CI0314
            11            GC06-GCUSPZ PICTURE  X(12).                   CI0314
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
      ******************************************************************
      *               FLAGS USED
      ******************************************************************
       01   GC03-CF          PIC X(1) VALUE '1'.
       01   GC06-CF          PIC X(1) VALUE '1'.
      ******************************************************************
      *               GENERAL WORKING STORAGE AREA
      ******************************************************************
       01   WS00-TO-ACCT     PIC X(1)  VALUE 'N'.
       01   DEBUT-WSS.                                                  CI0314
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0314
            05   IK     PICTURE X.                                      CI0314
       01  CONSTANTES-PAC.                                              CI0314
           05  FILLER  PICTURE X(87)   VALUE                            CI0314
                     '6015 CAT09/08/14CI0314ADMIN   14:35:19CI0314P AMERCI0314
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0314
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0314
           05  NUGNA   PICTURE X(5).                                    CI0314
           05  APPLI   PICTURE X(3).                                    CI0314
           05  DATGN   PICTURE X(8).                                    CI0314
           05  PROGR   PICTURE X(6).                                    CI0314
           05  CODUTI  PICTURE X(8).                                    CI0314
           05  TIMGN   PICTURE X(8).                                    CI0314
           05  PROGE   PICTURE X(8).                                    CI0314
           05  COBASE  PICTURE X(4).                                    CI0314
           05  DATGNC  PICTURE X(10).                                   CI0314
           05  RELEAS  PICTURE X(7).                                    CI0314
           05  DATGE   PICTURE X(10).                                   CI0314
           05  DATSQ   PICTURE X(10).                                   CI0314
       01  DATCE.                                                       CI0314
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0314
         05  DATOR.                                                     CI0314
           10  DATOA  PICTURE XX.                                       CI0314
           10  DATOM  PICTURE XX.                                       CI0314
           10  DATOJ  PICTURE XX.                                       CI0314
       01   VARIABLES-CONDITIONNELLES.                                  CI0314
            05                  FT      PICTURE X VALUE '0'.            CI0314
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0314
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0314
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0314
            05       5-GC00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0314
       01               S-GC01-SSA.                                     CI0314
            10         S1-GC01-SEGNAM PICTURE X(8)                      CI0314
                                      VALUE 'GC01    '.                 CI0314
            10         S1-GC01-CCOM   PICTURE X VALUE '*'.              CI0314
            10          S-GC01-CCOD   PICTURE X(5)                      CI0314
                                      VALUE '-----'.                    CI0314
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0314
       01            S-GCU01-SSA.                                       CI0314
            10      S1-GCU01-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC01    '.                 CI0314
            10      S1-GCU01-CCOM   PICTURE X VALUE '*'.                CI0314
            10       S-GCU01-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            10      S1-GCU01-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(GC01K'.                   CI0314
            10       S-GCU01-OPER  PICTURE XX VALUE ' ='.               CI0314
            10       S-GCU01-GC01K.                                     CI0314
            11       S-GCU01-C299.                                      CI0314
            12       S-GCU01-CTID.                                      CI0314
            13       S-GCU01-CTIDA    PICTURE  9(3).                    CI0314
            13       S-GCU01-CTIDN.                                     CI0314
            14       S-GCU01-CTIDNP   PICTURE  X(13).                   CI0314
            14       S-GCU01-CTIDND   PICTURE  9(11).                   CI0314
            10  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01               S-GC03-SSA.                                     CI0314
            10         S1-GC03-SEGNAM PICTURE X(8)                      CI0314
                                      VALUE 'GC03    '.                 CI0314
            10         S1-GC03-CCOM   PICTURE X VALUE '*'.              CI0314
            10          S-GC03-CCOD   PICTURE X(5)                      CI0314
                                      VALUE '-----'.                    CI0314
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0314
       01            S-GCA03-SSA.                                       CI0314
            11      S1-GCA03-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GCA03-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCA03-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCA03-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(CAATY'.                   CI0314
            11       S-GCA03-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCA03-CAATY    PICTURE  9(3).                    CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GCB03-SSA.                                       CI0314
            11      S1-GCB03-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GCB03-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCB03-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCB03-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(CVSYS'.                   CI0314
            11       S-GCB03-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCB03-CVSYS    PICTURE  X(2).                    CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GCC03-SSA.                                       CI0314
            11      S1-GCC03-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GCC03-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCC03-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCC03-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(CASTC'.                   CI0314
            11       S-GCC03-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCC03-CASTC    PICTURE  99.                      CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GCD03-SSA.                                       CI0314
            11      S1-GCD03-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GCD03-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCD03-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCD03-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(CACTO'.                   CI0314
            11       S-GCD03-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCD03-CACTO    PICTURE  9(3).                    CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GCE03-SSA.                                       CI0314
            11      S1-GCE03-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GCE03-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCE03-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCE03-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(IPULL'.                   CI0314
            11       S-GCE03-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCE03-IPULL    PICTURE  X.                       CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GCF03-SSA.                                       CI0314
            11      S1-GCF03-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GCF03-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCF03-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCF03-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(DTRAC'.                   CI0314
            11       S-GCF03-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCF03-DTRAC    PICTURE  9(8).                    CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GCG03-SSA.                                       CI0314
            11      S1-GCG03-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GCG03-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCG03-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCG03-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(CTRSO'.                   CI0314
            11       S-GCG03-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCG03-CTRSO    PICTURE  9(02).                   CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GCH03-SSA.                                       CI0314
            11      S1-GCH03-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GCH03-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCH03-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCH03-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(NTRCE'.                   CI0314
            11       S-GCH03-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCH03-NTRCE    PICTURE  9(06).                   CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GCI03-SSA.                                       CI0314
            11      S1-GCI03-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GCI03-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCI03-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCI03-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(ITRAN'.                   CI0314
            11       S-GCI03-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCI03-ITRAN    PICTURE  X.                       CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GCJ03-SSA.                                       CI0314
            11      S1-GCJ03-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GCJ03-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCJ03-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCJ03-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(DEFFT'.                   CI0314
            11       S-GCJ03-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCJ03-DEFFT    PICTURE  9(8).                    CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GCK03-SSA.                                       CI0314
            11      S1-GCK03-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GCK03-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCK03-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCK03-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(CPMTCA'.                  CI0314
            11       S-GCK03-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCK03-CPMTCA   PICTURE  XXX.                     CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GCL03-SSA.                                       CI0314
            11      S1-GCL03-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GCL03-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCL03-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCL03-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(ACASH'.                   CI0314
            11       S-GCL03-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCL03-ACASH    PICTURE  S9(9)V99                 CI0314
                          COMPUTATIONAL-3.                              CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GCN03-SSA.                                       CI0314
            11      S1-GCN03-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GCN03-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCN03-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCN03-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(CRREV'.                   CI0314
            11       S-GCN03-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCN03-CRREV    PICTURE  X(3).                    CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GCO03-SSA.                                       CI0314
            11      S1-GCO03-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GCO03-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCO03-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCO03-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(CSYST'.                   CI0314
            11       S-GCO03-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCO03-CSYST    PICTURE  99.                      CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GCU03-SSA.                                       CI0314
            11      S1-GCU03-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GCU03-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCU03-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCU03-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(GC03K'.                   CI0314
            11       S-GCU03-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCU03-GC03K.                                     CI0314
            12       S-GCU03-DCACG9   PICTURE  9(8).                    CI0314
            12       S-GCU03-NAASQ    PICTURE  S9(3)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GC103-SSA.                                       CI0314
            12      S1-GC103-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            12      S1-GC103-CCOM   PICTURE X VALUE '*'.                CI0314
            12       S-GC103-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            12      S1-GC103-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(XDCACG9'.                 CI0314
            12       S-GC103-OPER  PICTURE XX VALUE ' ='.               CI0314
            12       S-GC103-DCACG9   PICTURE  9(8).                    CI0314
            12  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GC203-SSA.                                       CI0314
            11      S1-GC203-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GC203-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GC203-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GC203-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(XGEAUN'.                  CI0314
            11       S-GC203-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GC203-GEAUN    PICTURE  9(5).                    CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GC303-SSA.                                       CI0314
            11      S1-GC303-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GC303-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GC303-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GC303-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(XGEOPD2'.                 CI0314
            11       S-GC303-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GC303-GEOPD2   PICTURE  X(8).                    CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GC403-SSA.                                       CI0314
            11      S1-GC403-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            11      S1-GC403-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GC403-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GC403-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(XNBTCH'.                  CI0314
            11       S-GC403-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GC403-NBTCH    PICTURE  9(4).                    CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GC803-SSA.                                       CI0314
            12      S1-GC803-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC03    '.                 CI0314
            12      S1-GC803-CCOM   PICTURE X VALUE '*'.                CI0314
            12       S-GC803-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            12      S1-GC803-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(XNAASQ'.                  CI0314
            12       S-GC803-OPER  PICTURE XX VALUE ' ='.               CI0314
            12       S-GC803-NAASQ    PICTURE  S9(3)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            12  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01               S-GC06-SSA.                                     CI0314
            10         S1-GC06-SEGNAM PICTURE X(8)                      CI0314
                                      VALUE 'GC06    '.                 CI0314
            10         S1-GC06-CCOM   PICTURE X VALUE '*'.              CI0314
            10          S-GC06-CCOD   PICTURE X(5)                      CI0314
                                      VALUE '-----'.                    CI0314
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0314
       01            S-GCF06-SSA.                                       CI0314
            11      S1-GCF06-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC06    '.                 CI0314
            11      S1-GCF06-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCF06-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCF06-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(PRCOD1'.                  CI0314
            11       S-GCF06-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCF06-PRCOD1   PICTURE  9(5).                    CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01            S-GCU06-SSA.                                       CI0314
            11      S1-GCU06-SEGNAM PICTURE X(8)                        CI0314
                                      VALUE 'GC06    '.                 CI0314
            11      S1-GCU06-CCOM   PICTURE X VALUE '*'.                CI0314
            11       S-GCU06-CCOD   PICTURE X(5)                        CI0314
                                      VALUE '-----'.                    CI0314
            11      S1-GCU06-FLDNAM PICTURE X(9)                        CI0314
                                      VALUE '(GC06K'.                   CI0314
            11       S-GCU06-OPER  PICTURE XX VALUE ' ='.               CI0314
            11       S-GCU06-GC06K.                                     CI0314
            12       S-GCU06-NPISQ    PICTURE  S9(3)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            11  FILLER   PICTURE X    VALUE ')'.                        CI0314
       01   ZONES-UTILISATEUR PICTURE X.                                CI0314
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
      *** PCB MASK FOR ACAP                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0314
          05              PA00-SUITE.                                   CI0314
            15       FILLER         PICTURE  X(00106).                  CI0314
       01                 PA06  REDEFINES      PA00.                    CI0314
            10            PA06-XDBPCB.                                  CI0314
            11            PA06-XDBDNM PICTURE  X(08).                   CI0314
            11            PA06-XSEGLV PICTURE  X(02).                   CI0314
            11            PA06-XRC    PICTURE  X(02).                   CI0314
            11            PA06-XPROPT PICTURE  X(04).                   CI0314
            11            PA06-FILLER PICTURE  S9(5)                    CI0314
                          BINARY.                                       CI0314
            11            PA06-XSEGNM PICTURE  X(08).                   CI0314
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0314
                          BINARY.                                       CI0314
            11            PA06-XSEGNB PICTURE  9(05)                    CI0314
                          BINARY.                                       CI0314
            11            PA06-XCOKEY PICTURE  X(70).                   CI0314
      *
      ******************************************************************
      *PASS AREA TO/FROM CI0314
      ******************************************************************
      *!WF DSP=PL DSL=PL SEL=05 FOR=I DES=1 LEV=1 PLT=10
       01                 PL05.                                         CI0314
            10            PL05-NPBN   PICTURE  X(20).                   CI0314
            10            PL05-IAIND  PICTURE  X.                       CI0314
            10            PL05-CTID   PICTURE  X(27).                   CI0314
            10            PL05-DCACG9 PICTURE  9(8).                    CI0314
            10            PL05-DEFFT  PICTURE  9(8).                    CI0314
            10            PL05-CTID01 PICTURE  X(27).                   CI0314
            10            PL05-ADBRQ  PICTURE  S9(11)V99                CI0314
                          COMPUTATIONAL-3.                              CI0314
            10            PL05-FILLER PICTURE  X(100).                  CI0314
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0314
          05              DE00-SUITE.                                   CI0314
            15       FILLER         PICTURE  X(00653).                  CI0314
       01                 DE10  REDEFINES      DE00.                    CI0314
            10            DE10-DU11.                                    CI0314
            11            DE10-XFONC  PICTURE  X(4).                    CI0314
            11            DE10-MPSBN  PICTURE  X(8).                    CI0314
            11            DE10-XDBDNM PICTURE  X(08).                   CI0314
            11            DE10-XSEGNM PICTURE  X(08).                   CI0314
            11            DE10-XRC    PICTURE  X(02).                   CI0314
            11            DE10-MSEG   PICTURE  X(08).                   CI0314
            11            DE10-XCOKEY PICTURE  X(70).                   CI0314
            11            DE10-CUIBR  PICTURE  X(01).                   CI0314
            11            DE10-CUIBA  PICTURE  X(01).                   CI0314
            11            DE10-IPBIK  PICTURE  X(1).                    CI0314
            10            DE10-DU03.                                    CI0314
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            DE10-CMSSF  PICTURE  XX.                      CI0314
            11            DE10-DU09.                                    CI0314
            12            DE10-CMESA  PICTURE  S9(9)                    CI0314
                          BINARY.                                       CI0314
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0314
                          BINARY.                                       CI0314
            12            DE10-CMESB  PICTURE  S9(9)                    CI0314
                          BINARY.                                       CI0314
            12            DE10-CMSST  PICTURE  S9(9)                    CI0314
                          BINARY.                                       CI0314
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0314
                          BINARY.                                       CI0314
            12            DE10-QELLAA PICTURE  S9(9)                    CI0314
                          BINARY.                                       CI0314
            12            DE10-TMESS4 PICTURE  X(512).                  CI0314
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
       01                 MS00.                                         CI0314
          05              MS00-SUITE.                                   CI0314
            15       FILLER         PICTURE  X(00542).                  CI0314
       01                 MS03  REDEFINES      MS00.                    CI0314
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            10            MS03-CMSSF  PICTURE  XX.                      CI0314
            10            MS03-DU09.                                    CI0314
            11            MS03-CMESA  PICTURE  S9(9)                    CI0314
                          BINARY.                                       CI0314
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0314
                          BINARY.                                       CI0314
            11            MS03-CMESB  PICTURE  S9(9)                    CI0314
                          BINARY.                                       CI0314
            11            MS03-CMSST  PICTURE  S9(9)                    CI0314
                          BINARY.                                       CI0314
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0314
                          BINARY.                                       CI0314
            11            MS03-QELLAA PICTURE  S9(9)                    CI0314
                          BINARY.                                       CI0314
            11            MS03-TMESS4 PICTURE  X(512).                  CI0314
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0314
            10            MX11-QMSGS  PICTURE  9(03).                   CI0314
            10            MX11-PJ09                                     CI0314
                          OCCURS       025     TIMES.                   CI0314
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0314
                          COMPUTATIONAL-3.                              CI0314
            11            MX11-CMESB  PICTURE  S9(9)                    CI0314
                          BINARY.                                       CI0314
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PL05
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0314
      *               *                                   *             CI0314
      *               *INITIALISATIONS                    *             CI0314
      *               *                                   *             CI0314
      *               *************************************.            CI0314
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
       F02PA.                                                           lv10
      *SET ADDRESS FOR ACAP                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-ACAP-PTR1.                                          ADU015
       F02PA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0314
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0314
      *               *                                   *             CI0314
      *               *FIN DE TRAITEMENT                  *             CI0314
      *               *                                   *             CI0314
      *               *************************************.            CI0314
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0314
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *MAIN PROCESSING                    *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40AB.    NOTE *CHECK THE SOURCE ACCT IN GC01      *.
       F40AB.                                                           lv10
      *********************************
           MOVE        'Y' TO PL05-IAIND
           MOVE        PL05-CTID TO S-GCU01-GC01K
           PERFORM     F94G1 THRU F94G1-FN.
      *N40AE.    NOTE *IF GC01 FOUND THEN READ GC03       *.
       F40AE.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F40AE-FN.
      *********************************
      *N40AH.    NOTE *LOOP THROUGH ACTIVITY - GC03       *.
       F40AH.                       GO TO     F40AH-B.                  lv20
       F40AH-A.
                 IF    GC03-CF = '0'
                 OR    WS00-TO-ACCT = 'Y'
                                    GO TO     F40AH-FN.
       F40AH-B.
      *******************************
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           PERFORM     F94G2 THRU F94G2-FN.
      *N40AM.    NOTE *CHECK IF TRAN IS DISTRIBUTION      *.
       F40AM.    IF    GC03-CAATY = '001'                               lv25
                 NEXT SENTENCE ELSE GO TO     F40AM-FN.
      *TRANSACTION
      *N40AQ.    NOTE *CHECK IF SOURCE ACCOUNT IS A       *.
       F40AQ.    IF    (GC01-CTIDA = 001                                lv30
                 AND   PL05-DCACG9 = GC03-DCACG9)
                 OR    GC01-CTIDA = 133
                 NEXT SENTENCE ELSE GO TO     F40AQ-FN.
      *CERTS ACCT AND INPUT TRAN DATE
      *MATCHES THE DATE IN GC03 OR
      *INPUT ACCOUNT IS A BETA ACCOUNT
      *N40BD.    NOTE *IF THE ABOVE CONDITION SATISFIED   *.
       F40BD.                       GO TO     F40BD-B.                  lv35
       F40BD-A.
                 IF    GC06-CF = '0'
                 OR    WS00-TO-ACCT = 'Y'
                                    GO TO     F40BD-FN.
       F40BD-B.
      *THEN LOOP THRU GC06
           MOVE        GC03-GC03K TO S-GCU03-GC03K
           PERFORM     F94G4 THRU F94G4-FN.
      *N40BH.    NOTE *CHECK FOR DUPLICATE TRANSACTION    *.
       F40BH.    IF    GC06-NPBN = PL05-NPBN                            lv40
                 AND   GC03-DEFFT = PL05-DEFFT
                 AND   GC06-CDELI = '006'
                 NEXT SENTENCE ELSE GO TO     F40BH-FN.
      *FOR CERTS ACH-OUT TRANS
           MOVE        'Y' TO WS00-TO-ACCT
           MOVE        'N' TO PL05-IAIND.
       F40BH-FN. EXIT.
      *N40CB.    NOTE *CHECK FOR DUPLICATE TRANSACTION    *.
       F40CB.    IF    GC06-CTID = PL05-CTID01                          lv40
                 AND   GC03-ADBRQ = PL05-ADBRQ
                 AND   GC03-DEFFT = PL05-DEFFT
                 AND   GC06-CPITC = 02
                 NEXT SENTENCE ELSE GO TO     F40CB-FN.
      *FOR BETA BROKERAGE TO CERTS TRAN
           MOVE        'Y' TO WS00-TO-ACCT
           MOVE        'N' TO PL05-IAIND.
       F40CB-FN. EXIT.
       F40BD-900. GO TO F40BD-A.
       F40BD-FN. EXIT.
       F40AQ-FN. EXIT.
       F40AM-FN. EXIT.
       F40AH-900. GO TO F40AH-A.
       F40AH-FN. EXIT.
       F40AE-FN. EXIT.
       F40AB-FN. EXIT.
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
       F9099-ITER-FN.  GO TO F05.
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
      *               *DATABASE ACCESS CALLS              *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94G1.    NOTE *CALL GU ON GC01                    *.            ADU026
       F94G1.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 GC01                                                    ADU026
           S-GCU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G1-FN. EXIT.
      *N94G2.    NOTE *CALL GN ON GC03                    *.            ADU026
       F94G2.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 GC03                                                    ADU026
           S-GCU01-SSA S-GC03-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
                 IF    IK = '0'                                         DOT
           MOVE        '1' TO GC03-CF
                 ELSE
           MOVE        '0' TO GC03-CF.
       F94G2-FN. EXIT.
      *N94G4.    NOTE *CALL GN ON GC06                    *.            ADU026
       F94G4.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 GC06                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           S-GC06-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
                 IF    IK = '0'                                         DOT
           MOVE        '1' TO GC06-CF
                 ELSE
           MOVE        '0' TO GC06-CF.
       F94G4-FN. EXIT.
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
