       IDENTIFICATION DIVISION.                                         CI0100
       PROGRAM-ID.  CI0100P.                                            CI0100
      *AUTHOR.         CATS ACCESS ACTIVITY.                            CI0100
      *DATE-COMPILED.   09/08/14.                                       CI0100
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
       ENVIRONMENT DIVISION.                                            CI0100
       CONFIGURATION SECTION.                                           CI0100
       SOURCE-COMPUTER. IBM-370.                                        CI0100
       OBJECT-COMPUTER. IBM-370.                                        CI0100
       DATA DIVISION.                                                   CI0100
       WORKING-STORAGE SECTION.                                         CI0100
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0100
            10            XW05-XW06.                                    CI0100
            11            XW05-XDBPCB.                                  CI0100
            12            XW05-XDBDNM PICTURE  X(08)                    CI0100
                          VALUE                SPACE.                   CI0100
            12            XW05-XSEGLV PICTURE  X(02)                    CI0100
                          VALUE                SPACE.                   CI0100
            12            XW05-XRC    PICTURE  X(02)                    CI0100
                          VALUE                SPACE.                   CI0100
            12            XW05-XPROPT PICTURE  X(04)                    CI0100
                          VALUE                SPACE.                   CI0100
            12            XW05-FILLER PICTURE  S9(5)                    CI0100
                          VALUE                ZERO                     CI0100
                          BINARY.                                       CI0100
            12            XW05-XSEGNM PICTURE  X(08)                    CI0100
                          VALUE                SPACE.                   CI0100
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0100
                          VALUE                ZERO                     CI0100
                          BINARY.                                       CI0100
            12            XW05-XSEGNB PICTURE  9(05)                    CI0100
                          VALUE                ZERO                     CI0100
                          BINARY.                                       CI0100
            12            XW05-XCOKEY PICTURE  X(70)                    CI0100
                          VALUE                SPACE.                   CI0100
            10            XW05-XW07.                                    CI0100
            11            XW05-XIOPCB.                                  CI0100
            12            XW05-XTERMI PICTURE  X(08)                    CI0100
                          VALUE                SPACE.                   CI0100
            12            XW05-FILLER PICTURE  XX                       CI0100
                          VALUE                SPACE.                   CI0100
            12            XW05-XRC1   PICTURE  X(02)                    CI0100
                          VALUE                SPACE.                   CI0100
            12            XW05-FILLER PICTURE  X(12)                    CI0100
                          VALUE                SPACE.                   CI0100
            12            XW05-XMODNM PICTURE  X(8)                     CI0100
                          VALUE                SPACE.                   CI0100
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0100
                          VALUE                ZERO.                    CI0100
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0100
                          VALUE                ZERO.                    CI0100
            10            XW05-XGU    PICTURE  X(4)                     CI0100
                          VALUE                'GU  '.                  CI0100
            10            XW05-XGHU   PICTURE  X(4)                     CI0100
                          VALUE                'GHU '.                  CI0100
            10            XW05-XGN    PICTURE  X(4)                     CI0100
                          VALUE                'GN  '.                  CI0100
            10            XW05-XGHN   PICTURE  X(4)                     CI0100
                          VALUE                'GHN '.                  CI0100
            10            XW05-XGNP   PICTURE  X(4)                     CI0100
                          VALUE                'GNP '.                  CI0100
            10            XW05-XGHNP  PICTURE  X(4)                     CI0100
                          VALUE                'GHNP'.                  CI0100
            10            XW05-XREPL  PICTURE  XXXX                     CI0100
                          VALUE                'REPL'.                  CI0100
            10            XW05-XISRT  PICTURE  X(4)                     CI0100
                          VALUE                'ISRT'.                  CI0100
            10            XW05-XDLET  PICTURE  X(4)                     CI0100
                          VALUE                'DLET'.                  CI0100
            10            XW05-XOPEN  PICTURE  X(4)                     CI0100
                          VALUE                'OPEN'.                  CI0100
            10            XW05-XCLSE  PICTURE  X(4)                     CI0100
                          VALUE                'CLSE'.                  CI0100
            10            XW05-XCHKP  PICTURE  X(4)                     CI0100
                          VALUE                'CHKP'.                  CI0100
            10            XW05-XXRST  PICTURE  X(4)                     CI0100
                          VALUE                'XRST'.                  CI0100
            10            XW05-XTERM  PICTURE  X(4)                     CI0100
                          VALUE                'TERM'.                  CI0100
            10            XW05-XNFPAC PICTURE  X(13)                    CI0100
                          VALUE                SPACE.                   CI0100
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0100
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0100
       01                 GC00.                                         CI0100
          05              GC00-SUITE.                                   CI0100
            15       FILLER         PICTURE  X(00420).                  CI0100
       01                 GC01  REDEFINES      GC00.                    CI0100
            10            GC01-GC01K.                                   CI0100
            11            GC01-C299.                                    CI0100
            12            GC01-CTID.                                    CI0100
            13            GC01-CTIDA  PICTURE  9(3).                    CI0100
            13            GC01-CTIDN.                                   CI0100
            14            GC01-CTIDNP PICTURE  X(13).                   CI0100
            14            GC01-CTIDND PICTURE  9(11).                   CI0100
            10            GC01-DCAG9L PICTURE  9(8).                    CI0100
            10            GC01-NAASQL PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            GC01-ICUST  PICTURE  X.                       CI0100
            10            GC01-NSEQ4B PICTURE  9(8)                     CI0100
                          BINARY.                                       CI0100
            10            GC01-PRCOD  PICTURE  9(5).                    CI0100
            10            GC01-PRSCD  PICTURE  X(9).                    CI0100
            10            GC01-FILLER PICTURE  X(8).                    CI0100
            10       FILLER         PICTURE  X(00356).                  CI0100
       01                 GC03  REDEFINES      GC00.                    CI0100
            10            GC03-GELL   PICTURE  9(4)                     CI0100
                          BINARY.                                       CI0100
            10            GC03-GD00.                                    CI0100
            11            GC03-GC03K.                                   CI0100
            12            GC03-DCACG9 PICTURE  9(8).                    CI0100
            12            GC03-NAASQ  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-CAATY  PICTURE  9(3).                    CI0100
            11            GC03-CVSYS  PICTURE  X(2).                    CI0100
            11            GC03-CACTO  PICTURE  9(3).                    CI0100
            11            GC03-CATRN.                                   CI0100
            12            GC03-CATRF  PICTURE  9(3).                    CI0100
            12            GC03-CATRS  PICTURE  9(3).                    CI0100
            11            GC03-CASTC  PICTURE  99.                      CI0100
            11            GC03-IPULL  PICTURE  X.                       CI0100
            11            GC03-GEAUN  PICTURE  9(5).                    CI0100
            11            GC03-GEOPD2 PICTURE  X(8).                    CI0100
            11            GC03-NBTCH  PICTURE  9(4).                    CI0100
            11            GC03-DEFFT  PICTURE  9(8).                    CI0100
            11            GC03-NSUNT  PICTURE  9(4).                    CI0100
            11            GC03-ITRAN  PICTURE  X.                       CI0100
            11            GC03-DLAUP1 PICTURE  9(8).                    CI0100
            11            GC03-ADRET  PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-TTRMS  PICTURE  X(12).                   CI0100
            11            GC03-IDELT  PICTURE  X.                       CI0100
            11            GC03-GEOPDM PICTURE  X(8).                    CI0100
            11            GC03-FILLER PICTURE  X(07).                   CI0100
            10            GC03-GD09.                                    CI0100
            11            GC03-FILLER PICTURE  X(70).                   CI0100
            10            GC03-GD01                                     CI0100
                          REDEFINES            GC03-GD09.               CI0100
            11            GC03-ADBRQ  PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-CTRTP  PICTURE  X(2).                    CI0100
            11            GC03-CPORT  PICTURE  X.                       CI0100
            11            GC03-CSCRNU PICTURE  X(4).                    CI0100
            11            GC03-DLAUP  PICTURE  9(8).                    CI0100
            11            GC03-CTWHAT PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-PWHLD  PICTURE  S999V9(5)                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-IWTHH  PICTURE  X.                       CI0100
            11            GC03-NDRFT  PICTURE  9(5).                    CI0100
            11            GC03-IDPAP  PICTURE  X.                       CI0100
            11            GC03-GETIM  PICTURE  S9(7)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-QNACT  PICTURE  9(3).                    CI0100
            11            GC03-AEDRQ  PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-IPLIN  PICTURE  X.                       CI0100
            11            GC03-CLIDNB PICTURE  9(8).                    CI0100
            11            GC03-CSLCT  PICTURE  X.                       CI0100
            11            GC03-ITELE  PICTURE  X.                       CI0100
            11            GC03-FILLER PICTURE  X(06).                   CI0100
            10            GC03-GD02                                     CI0100
                          REDEFINES            GC03-GD09.               CI0100
            11            GC03-CSYST  PICTURE  99.                      CI0100
            11            GC03-FILLER PICTURE  X.                       CI0100
            11            GC03-ACASH  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-DTRAC  PICTURE  9(8).                    CI0100
            11            GC03-CTRSO  PICTURE  9(02).                   CI0100
            11            GC03-NTRCE  PICTURE  9(06).                   CI0100
            11            GC03-GECKD1 PICTURE  9.                       CI0100
            11            GC03-CCOLL  PICTURE  X(3).                    CI0100
            11            GC03-CLTDP  PICTURE  X(3).                    CI0100
            11            GC03-PSLLD  PICTURE  S99V999                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-ISLOR  PICTURE  X.                       CI0100
            11            GC03-ITPAC  PICTURE  X.                       CI0100
            11            GC03-CPMTCA PICTURE  XXX.                     CI0100
            11            GC03-CSERV  PICTURE  X(3).                    CI0100
            11            GC03-ACOMO  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-IPLIN1 PICTURE  X.                       CI0100
            11            GC03-INQEX  PICTURE  X.                       CI0100
            11            GC03-CTKRAA PICTURE  X(12).                   CI0100
            11            GC03-CCSMQ  PICTURE  X.                       CI0100
            11            GC03-IVAEX1 PICTURE  X.                       CI0100
            11            GC03-IHPMT  PICTURE  X(1).                    CI0100
            11            GC03-GETIM3 PICTURE  S9(7)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            GC03-GD03                                     CI0100
                          REDEFINES            GC03-GD09.               CI0100
            11            GC03-CATRNC PICTURE  9(6).                    CI0100
            11            GC03-APRNT1 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-QSHOWT PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-ACINVT PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-ACOMO7 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-QSHOMW PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-ATAXT3 PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-CTSTR  PICTURE  9(2).                    CI0100
            11            GC03-ICIRA  PICTURE  X.                       CI0100
            11            GC03-GETIM2 PICTURE  S9(7)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-CPMTCX PICTURE  XX.                      CI0100
            11            GC03-FILLER PICTURE  X(16).                   CI0100
            10            GC03-GD99.                                    CI0100
            11            GC03-FILLER PICTURE  X(248).                  CI0100
            10            GC03-GD10                                     CI0100
                          REDEFINES            GC03-GD99.               CI0100
            11            GC03-MROTC  PICTURE  X(7).                    CI0100
            11            GC03-CEDSC  PICTURE  9(1).                    CI0100
            11            GC03-ILPOI  PICTURE  X(1).                    CI0100
            11            GC03-AWRCH  PICTURE  S9(3)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-CHCOC1 PICTURE  9(2).                    CI0100
            11            GC03-CHCOC2 PICTURE  9(2).                    CI0100
            11            GC03-CHCOC3 PICTURE  9(2).                    CI0100
            11            GC03-CHCOC4 PICTURE  9(2).                    CI0100
            11            GC03-CMCOC1 PICTURE  9(3).                    CI0100
            11            GC03-CMCOC2 PICTURE  9(3).                    CI0100
            11            GC03-CMCOC3 PICTURE  9(3).                    CI0100
            11            GC03-GD11.                                    CI0100
            12            GC03-FILLER PICTURE  X(219).                  CI0100
            11            GC03-GD12                                     CI0100
                          REDEFINES            GC03-GD11.               CI0100
            12            GC03-CELLO  PICTURE  9(1).                    CI0100
            12            GC03-CECLO  PICTURE  9(1).                    CI0100
            12            GC03-AEXML  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-CEPI   PICTURE  X(1).                    CI0100
            12            GC03-CEXTY  PICTURE  X.                       CI0100
            12            GC03-CROPC  PICTURE  9(1).                    CI0100
            12            GC03-CPUTY  PICTURE  9(1).                    CI0100
            12            GC03-IMCII  PICTURE  X(1).                    CI0100
            12            GC03-GEMISC                                   CI0100
                          OCCURS       010     TIMES.                   CI0100
            13            GC03-AMGLA  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            GC03-CMGLC  PICTURE  9(1).                    CI0100
            13            GC03-NMGLN  PICTURE  9(4).                    CI0100
            12            GC03-ACTRN  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-IWRBK  PICTURE  X.                       CI0100
            12            GC03-IFEDX  PICTURE  X.                       CI0100
            12            GC03-ICNTR  PICTURE  X.                       CI0100
            12            GC03-IOCKH  PICTURE  X.                       CI0100
            12            GC03-ICRCK  PICTURE  X.                       CI0100
            12            GC03-NHMPN  PICTURE  S9(10)                   CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-ITELR1 PICTURE  X.                       CI0100
            11            GC03-GD13                                     CI0100
                          REDEFINES            GC03-GD11.               CI0100
            12            GC03-DREDO  PICTURE  9(8).                    CI0100
            12            GC03-CATRNR PICTURE  9(6).                    CI0100
            12            GC03-CEVN   PICTURE  9(9).                    CI0100
            12            GC03-ISUSP  PICTURE  X(1).                    CI0100
            11            GC03-GD15                                     CI0100
                          REDEFINES            GC03-GD11.               CI0100
            12            GC03-CPUTZ  PICTURE  9(1).                    CI0100
            12            GC03-CETLB  PICTURE  9(3).                    CI0100
            12            GC03-QTRMC  PICTURE  9(3).                    CI0100
            12            GC03-DEFFTE PICTURE  9(8).                    CI0100
            12            GC03-DEFFTF PICTURE  9(8).                    CI0100
            12            GC03-DEFFTG PICTURE  9(8).                    CI0100
            12            GC03-XZ1A   PICTURE  X.                       CI0100
            12            GC03-XZ1B   PICTURE  X.                       CI0100
            12            GC03-XZ1C   PICTURE  X.                       CI0100
            12            GC03-XZ1D   PICTURE  X.                       CI0100
            12            GC03-XZ1E   PICTURE  X.                       CI0100
            12            GC03-XZ1F   PICTURE  X.                       CI0100
            12            GC03-XZ1G   PICTURE  X.                       CI0100
            12            GC03-XZ1H   PICTURE  X.                       CI0100
            12            GC03-XZ1I   PICTURE  X.                       CI0100
            12            GC03-DEFFTH PICTURE  9(8).                    CI0100
            11            GC03-GD19                                     CI0100
                          REDEFINES            GC03-GD11.               CI0100
            12            GC03-GD11.                                    CI0100
            13            GC03-FILLER PICTURE  X(219).                  CI0100
            10            GC03-GD20                                     CI0100
                          REDEFINES            GC03-GD99.               CI0100
            11            GC03-ADDACT PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-ISIGV  PICTURE  X.                       CI0100
            11            GC03-IALLF  PICTURE  X.                       CI0100
            11            GC03-QSHOWQ PICTURE  S9(9)V999                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-CCDSCW PICTURE  9(2).                    CI0100
            11            GC03-IDWRL  PICTURE  X.                       CI0100
            11            GC03-ITELR  PICTURE  X.                       CI0100
            11            GC03-IABIN  PICTURE  X.                       CI0100
            11            GC03-PACT1  PICTURE  S999V999                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-IBFAF  PICTURE  X.                       CI0100
            11            GC03-IFRSA  PICTURE  X.                       CI0100
            11            GC03-ICRCAN PICTURE  X.                       CI0100
            11            GC03-ACACTV PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-AGFND  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-QCSHOW PICTURE  S9(9)V999                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-QCSHIS PICTURE  S9(9)V999                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-NDTRC  PICTURE  9(8).                    CI0100
            11            GC03-CAERU  PICTURE  X(4).                    CI0100
            11            GC03-IFDGO  PICTURE  X.                       CI0100
            11            GC03-PSLLD2 PICTURE  S99V999                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-ISLOR2 PICTURE  X.                       CI0100
            11            GC03-QSFIO  PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-QSFID  PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-CGDIN  PICTURE  X.                       CI0100
            11            GC03-DGDIN  PICTURE  9(8).                    CI0100
            10            GC03-GD30                                     CI0100
                          REDEFINES            GC03-GD99.               CI0100
            11            GC03-ISKED  PICTURE  X.                       CI0100
            11            GC03-CENXC  PICTURE  9(2).                    CI0100
            11            GC03-GD31.                                    CI0100
            12            GC03-FILLER PICTURE  X(245).                  CI0100
            11            GC03-GD32                                     CI0100
                          REDEFINES            GC03-GD31.               CI0100
            12            GC03-IABIN1 PICTURE  X.                       CI0100
            12            GC03-CLDOD  PICTURE  9(8).                    CI0100
            12            GC03-NCLAM  PICTURE  9(5)                     CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-ISURR  PICTURE  X.                       CI0100
            12            GC03-GEHCD  PICTURE  9(3).                    CI0100
            12            GC03-CRATC  PICTURE  9(4).                    CI0100
            12            GC03-AMAXD  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-ASCHGA PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-APYOM  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-IWTHH1 PICTURE  X.                       CI0100
            12            GC03-CPAYCL PICTURE  X(2).                    CI0100
            12            GC03-CTSAO  PICTURE  X.                       CI0100
            12            GC03-NCONF  PICTURE  9(08).                   CI0100
            12            GC03-CLID   PICTURE  X(23).                   CI0100
            12            GC03-CARTY  PICTURE  99.                      CI0100
            12            GC03-NARRS  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-CARTZ  PICTURE  99.                      CI0100
            12            GC03-NAPDS  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-CPMTO  PICTURE  X.                       CI0100
            12            GC03-DNPMT  PICTURE  9(8).                    CI0100
            12            GC03-IPCTV  PICTURE  X.                       CI0100
            12            GC03-IMECH  PICTURE  X(01).                   CI0100
            12            GC03-IMVAO  PICTURE  X(1).                    CI0100
            12            GC03-AMVA1  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-CACTS  PICTURE  X.                       CI0100
            12            GC03-CTSPP  PICTURE  X(1).                    CI0100
            12            GC03-CACT4  PICTURE  X(2).                    CI0100
            12            GC03-IVAEX  PICTURE  X.                       CI0100
            12            GC03-DFPMT  PICTURE  9(8).                    CI0100
            12            GC03-IDEMD  PICTURE  X.                       CI0100
            12            GC03-IOFST  PICTURE  X.                       CI0100
            12            GC03-AMXLB  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-ACULB  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-DEIRNB PICTURE  9(8).                    CI0100
            12            GC03-DEFFE  PICTURE  9(8).                    CI0100
            12            GC03-DEFFR  PICTURE  9(8).                    CI0100
            12            GC03-ISPUP  PICTURE  X.                       CI0100
            12            GC03-CPNCG  PICTURE  X.                       CI0100
            12            GC03-IEXPU  PICTURE  X.                       CI0100
            12            GC03-IPPCF  PICTURE  X.                       CI0100
            12            GC03-NAAPT  PICTURE  9(2).                    CI0100
            12            GC03-PWHLDS PICTURE  S999V9(5)                CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-ISWHO  PICTURE  X(1).                    CI0100
            11            GC03-GD33                                     CI0100
                          REDEFINES            GC03-GD31.               CI0100
            12            GC03-CPAYC  PICTURE  X(2).                    CI0100
            12            GC03-ADBRQX PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-ADBRQV PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-APTXR  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-CTRTPE PICTURE  X(2).                    CI0100
            12            GC03-NCLAMI PICTURE  S9(9)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-CLIDO8 PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-CLIDN  PICTURE  X(20).                   CI0100
            12            GC03-DSET01 PICTURE  S9(8)                    CI0100
                          BINARY.                                       CI0100
            12            GC03-CTSET1 PICTURE  S9(6)                    CI0100
                          BINARY.                                       CI0100
            12            GC03-DSET02 PICTURE  S9(8)                    CI0100
                          BINARY.                                       CI0100
            12            GC03-CTSET2 PICTURE  S9(6)                    CI0100
                          BINARY.                                       CI0100
            11            GC03-GD34                                     CI0100
                          REDEFINES            GC03-GD31.               CI0100
            12            GC03-QNOFM  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-CLTRM  PICTURE  99.                      CI0100
            12            GC03-AMXLN  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-ALADJ  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-ACHK   PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-APRMO  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-IMECH1 PICTURE  X(01).                   CI0100
            12            GC03-CACT41 PICTURE  X(2).                    CI0100
            12            GC03-ACDSCC PICTURE  S9(05)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-ACDSCD PICTURE  S9(05)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-GD39                                     CI0100
                          REDEFINES            GC03-GD31.               CI0100
            12            GC03-GD31.                                    CI0100
            13            GC03-FILLER PICTURE  X(245).                  CI0100
            10            GC03-GD40                                     CI0100
                          REDEFINES            GC03-GD99.               CI0100
            11            GC03-NTR    PICTURE  9(8).                    CI0100
            11            GC03-NPBNC  PICTURE  X(24).                   CI0100
            11            GC03-CRREV  PICTURE  X(3).                    CI0100
            11            GC03-CSUSL  PICTURE  S9.                      CI0100
            11            GC03-NMGLN1 PICTURE  9(4).                    CI0100
            11            GC03-DCAC92 PICTURE  9(8).                    CI0100
            11            GC03-NAASQ3 PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-GD49.                                    CI0100
            12            GC03-FILLER PICTURE  X(198).                  CI0100
            11            GC03-GD41                                     CI0100
                          REDEFINES            GC03-GD49.               CI0100
            12            GC03-CRREF  PICTURE  9(2).                    CI0100
            12            GC03-CORIR  PICTURE  X(02).                   CI0100
            12            GC03-CIPDB  PICTURE  X(03).                   CI0100
            12            GC03-CPAYH  PICTURE  X(02).                   CI0100
            12            GC03-NAMEX  PICTURE  9(15)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC03-DCHAE  PICTURE  9(4).                    CI0100
            12            GC03-DRQST  PICTURE  S9(8)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-GD42                                     CI0100
                          REDEFINES            GC03-GD49.               CI0100
            12            GC03-CPMTCB PICTURE  X(3).                    CI0100
            10            GC03-GD50                                     CI0100
                          REDEFINES            GC03-GD99.               CI0100
            11            GC03-ALOAD  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-PSLLD4 PICTURE  S99V999                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-CSUSL1 PICTURE  S9.                      CI0100
            11            GC03-CRREV1 PICTURE  X(3).                    CI0100
            11            GC03-ADDAC  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-DL13.                                    CI0100
            12            GC03-GEYR   PICTURE  9(4).                    CI0100
            12            GC03-GEMTH  PICTURE  99.                      CI0100
            12            GC03-NDAY   PICTURE  99.                      CI0100
            11            GC03-NSEQ3P PICTURE  S9(5)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-XZ6A   PICTURE  X(6).                    CI0100
            11            GC03-XZ7    PICTURE  X(7).                    CI0100
            11            GC03-XZ6B   PICTURE  X(6).                    CI0100
            11            GC03-XZ6    PICTURE  X(6).                    CI0100
            11            GC03-XZ6C   PICTURE  X(6).                    CI0100
            11            GC03-XZ20   PICTURE  X(20).                   CI0100
            11            GC03-CATRN1 PICTURE  9(6).                    CI0100
            11            GC03-ADDAC2 PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-ATAXT2 PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-ACOMOT PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-XZ5    PICTURE  X(5).                    CI0100
            11            GC03-IREVD  PICTURE  X(1).                    CI0100
            11            GC03-ISUSP1 PICTURE  X(1).                    CI0100
            11            GC03-XZ6D   PICTURE  X(6).                    CI0100
            11            GC03-XZ13   PICTURE  X(13).                   CI0100
            11            GC03-CWHTP2 PICTURE  X(3).                    CI0100
            11            GC03-CWHTP3 PICTURE  X(3).                    CI0100
            11            GC03-DTREN  PICTURE  9(8).                    CI0100
            11            GC03-NAASQ1 PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            GC03-GD51                                     CI0100
                          REDEFINES            GC03-GD99.               CI0100
            11            GC03-ADOMOT PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-ACGLT  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-ACGST  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-CTXMT  PICTURE  9(2).                    CI0100
            11            GC03-ALOAD3 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-FILLER PICTURE  X(31).                   CI0100
            10            GC03-GD52                                     CI0100
                          REDEFINES            GC03-GD99.               CI0100
            11            GC03-DEFFT5 PICTURE  9(8).                    CI0100
            11            GC03-PSLLD5 PICTURE  S99V999                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-CSUSL2 PICTURE  S9.                      CI0100
            11            GC03-ALOAD2 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-DL22.                                    CI0100
            12            GC03-NYEAR1 PICTURE  9(4).                    CI0100
            12            GC03-GEMTHA PICTURE  99.                      CI0100
            12            GC03-NDAY01 PICTURE  99.                      CI0100
            11            GC03-NSEQ3R PICTURE  S9(5)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-CWHTP  PICTURE  X(3).                    CI0100
            11            GC03-CWHFR  PICTURE  X(3).                    CI0100
            11            GC03-CATRN7 PICTURE  9(6).                    CI0100
            11            GC03-ATAXT5 PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-QSHOT  PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-ACINT3 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-CWHTP1 PICTURE  X(3).                    CI0100
            11            GC03-CWHFR1 PICTURE  X(3).                    CI0100
            11            GC03-ACOMO5 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-QSHOMU PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-ACASH1 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-FILLER PICTURE  X(04).                   CI0100
            11            GC03-CATRN8 PICTURE  9(6).                    CI0100
            11            GC03-ALOAD1 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-PSLLD1 PICTURE  S99V999                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-QSHOT1 PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-ACINT4 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-CSUSL4 PICTURE  S9.                      CI0100
            11            GC03-ACOMO4 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            GC03-GD60                                     CI0100
                          REDEFINES            GC03-GD99.               CI0100
            11            GC03-GEOPDD PICTURE  X(8)                     CI0100
                          OCCURS       005     TIMES.                   CI0100
            11            GC03-DLAUP3 PICTURE  9(8)                     CI0100
                          OCCURS       005     TIMES.                   CI0100
            11            GC03-GEOPDB PICTURE  X(8).                    CI0100
            11            GC03-DLAUP4 PICTURE  9(8).                    CI0100
            11            GC03-ITELR2 PICTURE  X.                       CI0100
            11            GC03-IPMTA  PICTURE  X.                       CI0100
            11            GC03-CCSMG  PICTURE  X.                       CI0100
            11            GC03-CPLEC  PICTURE  XX.                      CI0100
            11            GC03-CORTYA PICTURE  X(3).                    CI0100
            11            GC03-CACTBC PICTURE  X(1).                    CI0100
            11            GC03-CGSPIA PICTURE  X.                       CI0100
            11            GC03-IPTRDA PICTURE  X(01).                   CI0100
            11            GC03-GCUSPY PICTURE  X(12).                   CI0100
            11            GC03-CPALLA PICTURE  X(1).                    CI0100
            11            GC03-QSHO5A PICTURE  S9(9)V999                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-IFRSAB PICTURE  X.                       CI0100
            11            GC03-DELOI  PICTURE  9(8).                    CI0100
            11            GC03-IAROAA PICTURE  X.                       CI0100
            11            GC03-ACINVR PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-ILTINA PICTURE  X.                       CI0100
            11            GC03-ALOIDA PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC03-CFUNTA PICTURE  X(2).                    CI0100
            11            GC03-CLGND  PICTURE  X.                       CI0100
            11            GC03-CPH3U  PICTURE  X.                       CI0100
            11            GC03-GESTD  PICTURE  9(8).                    CI0100
            11            GC03-GEEND  PICTURE  9(8).                    CI0100
            11            GC03-CPMTF  PICTURE  99.                      CI0100
            11            GC03-CNAVR  PICTURE  X(1).                    CI0100
            10            GC03-GD70                                     CI0100
                          REDEFINES            GC03-GD99.               CI0100
            11            GC03-CMEMO  PICTURE  X(2).                    CI0100
            11            GC03-ALPLDT PICTURE  9(8).                    CI0100
            11            GC03-CTLPD  PICTURE  9(8).                    CI0100
            11            GC03-CPAYCM PICTURE  X(2).                    CI0100
       01                 GC04  REDEFINES      GC00.                    CI0100
            10            GC04-CLCUS  PICTURE  99.                      CI0100
            10            GC04-CCACT  PICTURE  99.                      CI0100
            10            GC04-AFEET  PICTURE  S9(5)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            GC04-ITERF  PICTURE  X.                       CI0100
            10            GC04-ATERF  PICTURE  S9(5)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            GC04-CLDOB  PICTURE  9(8).                    CI0100
            10            GC04-CPLTYP PICTURE  X(14).                   CI0100
            10            GC04-IACFPD PICTURE  X(1).                    CI0100
            10            GC04-FILLER PICTURE  X(14).                   CI0100
            10       FILLER         PICTURE  X(00370).                  CI0100
       01                 GC06  REDEFINES      GC00.                    CI0100
            10            GC06-GELL   PICTURE  9(4)                     CI0100
                          BINARY.                                       CI0100
            10            GC06-GE00.                                    CI0100
            11            GC06-GC06K.                                   CI0100
            12            GC06-NPISQ  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC06-ACOTD  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC06-PPOTD  PICTURE  S9(3)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC06-QPSTD  PICTURE  S9(7)V999                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC06-CPITC  PICTURE  99.                      CI0100
            11            GC06-ITRNB  PICTURE  X.                       CI0100
            11            GC06-FILLER PICTURE  X(14).                   CI0100
            10            GC06-GE98.                                    CI0100
            11            GC06-FILLER PICTURE  X(240).                  CI0100
            10            GC06-GE10                                     CI0100
                          REDEFINES            GC06-GE98.               CI0100
            11            GC06-CDELI  PICTURE  9(3).                    CI0100
            11            GC06-CPAYC  PICTURE  X(2).                    CI0100
            11            GC06-ICHKP  PICTURE  X.                       CI0100
            11            GC06-CLTIN  PICTURE  9(12).                   CI0100
            11            GC06-IFHAI  PICTURE  X.                       CI0100
            11            GC06-CDQUA  PICTURE  X(2).                    CI0100
            11            GC06-FILLER PICTURE  X(07).                   CI0100
            11            GC06-GE99.                                    CI0100
            12            GC06-FILLER PICTURE  X(212).                  CI0100
            11            GC06-GE01                                     CI0100
                          REDEFINES            GC06-GE99.               CI0100
            12            GC06-NTR    PICTURE  9(8).                    CI0100
            12            GC06-GECKD  PICTURE  9.                       CI0100
            12            GC06-NPBN   PICTURE  X(20).                   CI0100
            12            GC06-CCBAT  PICTURE  99.                      CI0100
            12            GC06-CLID4  PICTURE  X(23).                   CI0100
            12            GC06-GENAL1 PICTURE  X(30)                    CI0100
                          OCCURS       002     TIMES.                   CI0100
            12            GC06-GESAD1 PICTURE  X(30)                    CI0100
                          OCCURS       003     TIMES.                   CI0100
            11            GC06-GE02                                     CI0100
                          REDEFINES            GC06-GE99.               CI0100
            12            GC06-GENAL  PICTURE  X(30)                    CI0100
                          OCCURS       002     TIMES.                   CI0100
            12            GC06-GESAD  PICTURE  X(30)                    CI0100
                          OCCURS       003     TIMES.                   CI0100
            11            GC06-GE03                                     CI0100
                          REDEFINES            GC06-GE99.               CI0100
            12            GC06-NCHKN  PICTURE  9(11).                   CI0100
            11            GC06-GE04                                     CI0100
                          REDEFINES            GC06-GE99.               CI0100
            12            GC06-CTIDAP PICTURE  9(3).                    CI0100
            12            GC06-PRCOD  PICTURE  9(5).                    CI0100
            12            GC06-TDELI  PICTURE  X(30).                   CI0100
            12            GC06-CINCD  PICTURE  9(02).                   CI0100
            10            GC06-GE20                                     CI0100
                          REDEFINES            GC06-GE98.               CI0100
            11            GC06-C299.                                    CI0100
            12            GC06-CTID.                                    CI0100
            13            GC06-CTIDA  PICTURE  9(3).                    CI0100
            13            GC06-CTIDN.                                   CI0100
            14            GC06-CTIDNP PICTURE  X(13).                   CI0100
            14            GC06-CTIDND PICTURE  9(11).                   CI0100
            11            GC06-DCACG9 PICTURE  9(8).                    CI0100
            11            GC06-NAASQ  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            GC06-CIRAP  PICTURE  XX.                      CI0100
            11            GC06-CTYPE  PICTURE  X.                       CI0100
            11            GC06-INACT  PICTURE  X.                       CI0100
            11            GC06-FILLER PICTURE  X(01).                   CI0100
            11            GC06-ITPAC  PICTURE  X.                       CI0100
            11            GC06-ITAXI  PICTURE  X.                       CI0100
            11            GC06-IOWNC  PICTURE  X.                       CI0100
            11            GC06-CDVCD  PICTURE  X(2).                    CI0100
            11            GC06-CTCUS  PICTURE  999.                     CI0100
            11            GC06-CPMTCB PICTURE  X(3).                    CI0100
            11            GC06-CASTC1 PICTURE  99.                      CI0100
            11            GC06-PRCOD1 PICTURE  9(5).                    CI0100
            11            GC06-CPRSC1 PICTURE  X(9).                    CI0100
            11            GC06-CPRTB  PICTURE  X.                       CI0100
            11            GC06-CBRKD  PICTURE  9(4).                    CI0100
            11            GC06-FILLER PICTURE  X(12).                   CI0100
            10            GC06-GE30                                     CI0100
                          REDEFINES            GC06-GE98.               CI0100
            11            GC06-CFIDC  PICTURE  X(5).                    CI0100
            11            GC06-CPHSE  PICTURE  9(2).                    CI0100
            11            GC06-FILLER PICTURE  X(05).                   CI0100
            11            GC06-IABIN  PICTURE  X.                       CI0100
            11            GC06-PDFND  PICTURE  S999V9(3)                CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            GC06-GE40                                     CI0100
                          REDEFINES            GC06-GE98.               CI0100
            11            GC06-CACCT  PICTURE  X.                       CI0100
            11            GC06-CPAYR  PICTURE  X(2).                    CI0100
            11            GC06-CDELI1 PICTURE  9(3).                    CI0100
            11            GC06-CATRN.                                   CI0100
            12            GC06-CATRF  PICTURE  9(3).                    CI0100
            12            GC06-CATRS  PICTURE  9(3).                    CI0100
            11            GC06-DEFFT  PICTURE  9(8).                    CI0100
            11            GC06-CTYPC  PICTURE  X.                       CI0100
            11            GC06-CIRAPA PICTURE  XX.                      CI0100
            11            GC06-FILLER PICTURE  X(09).                   CI0100
            11            GC06-GE49.                                    CI0100
            12            GC06-FILLER PICTURE  X(208).                  CI0100
            11            GC06-GE41                                     CI0100
                          REDEFINES            GC06-GE49.               CI0100
            12            GC06-NCHKN1 PICTURE  9(6).                    CI0100
            11            GC06-GE42                                     CI0100
                          REDEFINES            GC06-GE49.               CI0100
            12            GC06-CTID1.                                   CI0100
            13            GC06-CTIDA1 PICTURE  9(3).                    CI0100
            13            GC06-CTIDP1 PICTURE  X(13).                   CI0100
            13            GC06-CTIDN1 PICTURE  9(11).                   CI0100
            11            GC06-GE43                                     CI0100
                          REDEFINES            GC06-GE49.               CI0100
            12            GC06-GENAL2 PICTURE  X(30)                    CI0100
                          OCCURS       002     TIMES.                   CI0100
            12            GC06-GESAD2 PICTURE  X(30)                    CI0100
                          OCCURS       003     TIMES.                   CI0100
            11            GC06-GE44                                     CI0100
                          REDEFINES            GC06-GE49.               CI0100
            12            GC06-CTID01.                                  CI0100
            13            GC06-CTIDA6 PICTURE  9(3).                    CI0100
            13            GC06-NTIDP2 PICTURE  X(13).                   CI0100
            13            GC06-CTIDN2 PICTURE  9(11).                   CI0100
            12            GC06-GECKD2 PICTURE  9.                       CI0100
            12            GC06-PACCT  PICTURE  S999V99                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC06-PLOAN  PICTURE  S999V99                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC06-PADPT  PICTURE  S999V99                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            GC06-IPCTL  PICTURE  X.                       CI0100
            12            GC06-IPCTP  PICTURE  X.                       CI0100
            12            GC06-CEUNT  PICTURE  S9(5)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            GC06-GE31                                     CI0100
                          REDEFINES            GC06-GE98.               CI0100
            11            GC06-GCUSPZ PICTURE  X(12).                   CI0100
            10       FILLER         PICTURE  X(00144).                  CI0100
       01                 GC12  REDEFINES      GC00.                    CI0100
            10            GC12-GC12K.                                   CI0100
            11            GC12-CIRAP  PICTURE  XX.                      CI0100
            10            GC12-AIRCT  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            GC12-FILLER PICTURE  X.                       CI0100
            10       FILLER         PICTURE  X(00412).                  CI0100
       01                 GC21  REDEFINES      GC00.                    CI0100
            10            GC21-C299.                                    CI0100
            11            GC21-CTID.                                    CI0100
            12            GC21-CTIDA  PICTURE  9(3).                    CI0100
            12            GC21-CTIDN.                                   CI0100
            13            GC21-CTIDNP PICTURE  X(13).                   CI0100
            13            GC21-CTIDND PICTURE  9(11).                   CI0100
            10            GC21-DCACG9 PICTURE  9(8).                    CI0100
            10            GC21-NAASQ  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            GC21-FILLER PICTURE  X.                       CI0100
            10       FILLER         PICTURE  X(00382).                  CI0100
      ******************************************************************ADU035
      ***      CORPORATE INTERFACE APPLICATION CONTROL BLOCK - UPDATE **ADU035
      ******************************************************************ADU035
         COPY DBI3006F.                                                 ADU035
      ******************************************************************ADU035
      *** COPYBOOKS FOR THE TBL-HDR AND PASS-AREA FOR PERTINENT FIELDS  ADU035
      ******************************************************************ADU035
         COPY CIMD07PE.                                                 ADU035
         COPY CIMD07PI.                                                 ADU035
      *                                                                 ADU035
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
       01                 OC01.                                         CI0100
            10            OC01-GC01K.                                   CI0100
            11            OC01-C299.                                    CI0100
            12            OC01-CTID.                                    CI0100
            13            OC01-CTIDA  PICTURE  9(3).                    CI0100
            13            OC01-CTIDN.                                   CI0100
            14            OC01-CTIDNP PICTURE  X(13).                   CI0100
            14            OC01-CTIDND PICTURE  9(11).                   CI0100
            10            OC01-DCAG9L PICTURE  9(8).                    CI0100
            10            OC01-NAASQL PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            OC01-ICUST  PICTURE  X.                       CI0100
            10            OC01-NSEQ4B PICTURE  9(8)                     CI0100
                          BINARY.                                       CI0100
            10            OC01-PRCOD  PICTURE  9(5).                    CI0100
            10            OC01-PRSCD  PICTURE  X(9).                    CI0100
            10            OC01-FILLER PICTURE  X(8).                    CI0100
       01                 OC03.                                         CI0100
            10            OC03-GELL   PICTURE  9(4)                     CI0100
                          BINARY.                                       CI0100
            10            OC03-GD00.                                    CI0100
            11            OC03-GC03K.                                   CI0100
            12            OC03-DCACG9 PICTURE  9(8).                    CI0100
            12            OC03-NAASQ  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-CAATY  PICTURE  9(3).                    CI0100
            11            OC03-CVSYS  PICTURE  X(2).                    CI0100
            11            OC03-CACTO  PICTURE  9(3).                    CI0100
            11            OC03-CATRN.                                   CI0100
            12            OC03-CATRF  PICTURE  9(3).                    CI0100
            12            OC03-CATRS  PICTURE  9(3).                    CI0100
            11            OC03-CASTC  PICTURE  99.                      CI0100
            11            OC03-IPULL  PICTURE  X.                       CI0100
            11            OC03-GEAUN  PICTURE  9(5).                    CI0100
            11            OC03-GEOPD2 PICTURE  X(8).                    CI0100
            11            OC03-NBTCH  PICTURE  9(4).                    CI0100
            11            OC03-DEFFT  PICTURE  9(8).                    CI0100
            11            OC03-NSUNT  PICTURE  9(4).                    CI0100
            11            OC03-ITRAN  PICTURE  X.                       CI0100
            11            OC03-DLAUP1 PICTURE  9(8).                    CI0100
            11            OC03-ADRET  PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-TTRMS  PICTURE  X(12).                   CI0100
            11            OC03-IDELT  PICTURE  X.                       CI0100
            11            OC03-GEOPDM PICTURE  X(8).                    CI0100
            11            OC03-FILLER PICTURE  X(07).                   CI0100
            10            OC03-GD09.                                    CI0100
            11            OC03-FILLER PICTURE  X(70).                   CI0100
            10            OC03-GD01                                     CI0100
                          REDEFINES            OC03-GD09.               CI0100
            11            OC03-ADBRQ  PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-CTRTP  PICTURE  X(2).                    CI0100
            11            OC03-CPORT  PICTURE  X.                       CI0100
            11            OC03-CSCRNU PICTURE  X(4).                    CI0100
            11            OC03-DLAUP  PICTURE  9(8).                    CI0100
            11            OC03-CTWHAT PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-PWHLD  PICTURE  S999V9(5)                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-IWTHH  PICTURE  X.                       CI0100
            11            OC03-NDRFT  PICTURE  9(5).                    CI0100
            11            OC03-IDPAP  PICTURE  X.                       CI0100
            11            OC03-GETIM  PICTURE  S9(7)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-QNACT  PICTURE  9(3).                    CI0100
            11            OC03-AEDRQ  PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-IPLIN  PICTURE  X.                       CI0100
            11            OC03-CLIDNB PICTURE  9(8).                    CI0100
            11            OC03-CSLCT  PICTURE  X.                       CI0100
            11            OC03-ITELE  PICTURE  X.                       CI0100
            11            OC03-FILLER PICTURE  X(06).                   CI0100
            10            OC03-GD02                                     CI0100
                          REDEFINES            OC03-GD09.               CI0100
            11            OC03-CSYST  PICTURE  99.                      CI0100
            11            OC03-FILLER PICTURE  X.                       CI0100
            11            OC03-ACASH  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-DTRAC  PICTURE  9(8).                    CI0100
            11            OC03-CTRSO  PICTURE  9(02).                   CI0100
            11            OC03-NTRCE  PICTURE  9(06).                   CI0100
            11            OC03-GECKD1 PICTURE  9.                       CI0100
            11            OC03-CCOLL  PICTURE  X(3).                    CI0100
            11            OC03-CLTDP  PICTURE  X(3).                    CI0100
            11            OC03-PSLLD  PICTURE  S99V999                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-ISLOR  PICTURE  X.                       CI0100
            11            OC03-ITPAC  PICTURE  X.                       CI0100
            11            OC03-CPMTCA PICTURE  XXX.                     CI0100
            11            OC03-CSERV  PICTURE  X(3).                    CI0100
            11            OC03-ACOMO  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-IPLIN1 PICTURE  X.                       CI0100
            11            OC03-INQEX  PICTURE  X.                       CI0100
            11            OC03-CTKRAA PICTURE  X(12).                   CI0100
            11            OC03-CCSMQ  PICTURE  X.                       CI0100
            11            OC03-IVAEX1 PICTURE  X.                       CI0100
            11            OC03-IHPMT  PICTURE  X(1).                    CI0100
            11            OC03-GETIM3 PICTURE  S9(7)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            OC03-GD03                                     CI0100
                          REDEFINES            OC03-GD09.               CI0100
            11            OC03-CATRNC PICTURE  9(6).                    CI0100
            11            OC03-APRNT1 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-QSHOWT PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-ACINVT PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-ACOMO7 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-QSHOMW PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-ATAXT3 PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-CTSTR  PICTURE  9(2).                    CI0100
            11            OC03-ICIRA  PICTURE  X.                       CI0100
            11            OC03-GETIM2 PICTURE  S9(7)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-CPMTCX PICTURE  XX.                      CI0100
            11            OC03-FILLER PICTURE  X(16).                   CI0100
            10            OC03-GD99.                                    CI0100
            11            OC03-FILLER PICTURE  X(248).                  CI0100
            10            OC03-GD10                                     CI0100
                          REDEFINES            OC03-GD99.               CI0100
            11            OC03-MROTC  PICTURE  X(7).                    CI0100
            11            OC03-CEDSC  PICTURE  9(1).                    CI0100
            11            OC03-ILPOI  PICTURE  X(1).                    CI0100
            11            OC03-AWRCH  PICTURE  S9(3)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-CHCOC1 PICTURE  9(2).                    CI0100
            11            OC03-CHCOC2 PICTURE  9(2).                    CI0100
            11            OC03-CHCOC3 PICTURE  9(2).                    CI0100
            11            OC03-CHCOC4 PICTURE  9(2).                    CI0100
            11            OC03-CMCOC1 PICTURE  9(3).                    CI0100
            11            OC03-CMCOC2 PICTURE  9(3).                    CI0100
            11            OC03-CMCOC3 PICTURE  9(3).                    CI0100
            11            OC03-GD11.                                    CI0100
            12            OC03-FILLER PICTURE  X(219).                  CI0100
            11            OC03-GD12                                     CI0100
                          REDEFINES            OC03-GD11.               CI0100
            12            OC03-CELLO  PICTURE  9(1).                    CI0100
            12            OC03-CECLO  PICTURE  9(1).                    CI0100
            12            OC03-AEXML  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-CEPI   PICTURE  X(1).                    CI0100
            12            OC03-CEXTY  PICTURE  X.                       CI0100
            12            OC03-CROPC  PICTURE  9(1).                    CI0100
            12            OC03-CPUTY  PICTURE  9(1).                    CI0100
            12            OC03-IMCII  PICTURE  X(1).                    CI0100
            12            OC03-GEMISC                                   CI0100
                          OCCURS       010     TIMES.                   CI0100
            13            OC03-AMGLA  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            OC03-CMGLC  PICTURE  9(1).                    CI0100
            13            OC03-NMGLN  PICTURE  9(4).                    CI0100
            12            OC03-ACTRN  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-IWRBK  PICTURE  X.                       CI0100
            12            OC03-IFEDX  PICTURE  X.                       CI0100
            12            OC03-ICNTR  PICTURE  X.                       CI0100
            12            OC03-IOCKH  PICTURE  X.                       CI0100
            12            OC03-ICRCK  PICTURE  X.                       CI0100
            12            OC03-NHMPN  PICTURE  S9(10)                   CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-ITELR1 PICTURE  X.                       CI0100
            11            OC03-GD13                                     CI0100
                          REDEFINES            OC03-GD11.               CI0100
            12            OC03-DREDO  PICTURE  9(8).                    CI0100
            12            OC03-CATRNR PICTURE  9(6).                    CI0100
            12            OC03-CEVN   PICTURE  9(9).                    CI0100
            12            OC03-ISUSP  PICTURE  X(1).                    CI0100
            11            OC03-GD15                                     CI0100
                          REDEFINES            OC03-GD11.               CI0100
            12            OC03-CPUTZ  PICTURE  9(1).                    CI0100
            12            OC03-CETLB  PICTURE  9(3).                    CI0100
            12            OC03-QTRMC  PICTURE  9(3).                    CI0100
            12            OC03-DEFFTE PICTURE  9(8).                    CI0100
            12            OC03-DEFFTF PICTURE  9(8).                    CI0100
            12            OC03-DEFFTG PICTURE  9(8).                    CI0100
            12            OC03-XZ1A   PICTURE  X.                       CI0100
            12            OC03-XZ1B   PICTURE  X.                       CI0100
            12            OC03-XZ1C   PICTURE  X.                       CI0100
            12            OC03-XZ1D   PICTURE  X.                       CI0100
            12            OC03-XZ1E   PICTURE  X.                       CI0100
            12            OC03-XZ1F   PICTURE  X.                       CI0100
            12            OC03-XZ1G   PICTURE  X.                       CI0100
            12            OC03-XZ1H   PICTURE  X.                       CI0100
            12            OC03-XZ1I   PICTURE  X.                       CI0100
            12            OC03-DEFFTH PICTURE  9(8).                    CI0100
            11            OC03-GD19                                     CI0100
                          REDEFINES            OC03-GD11.               CI0100
            12            OC03-GD11.                                    CI0100
            13            OC03-FILLER PICTURE  X(219).                  CI0100
            10            OC03-GD20                                     CI0100
                          REDEFINES            OC03-GD99.               CI0100
            11            OC03-ADDACT PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-ISIGV  PICTURE  X.                       CI0100
            11            OC03-IALLF  PICTURE  X.                       CI0100
            11            OC03-QSHOWQ PICTURE  S9(9)V999                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-CCDSCW PICTURE  9(2).                    CI0100
            11            OC03-IDWRL  PICTURE  X.                       CI0100
            11            OC03-ITELR  PICTURE  X.                       CI0100
            11            OC03-IABIN  PICTURE  X.                       CI0100
            11            OC03-PACT1  PICTURE  S999V999                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-IBFAF  PICTURE  X.                       CI0100
            11            OC03-IFRSA  PICTURE  X.                       CI0100
            11            OC03-ICRCAN PICTURE  X.                       CI0100
            11            OC03-ACACTV PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-AGFND  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-QCSHOW PICTURE  S9(9)V999                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-QCSHIS PICTURE  S9(9)V999                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-NDTRC  PICTURE  9(8).                    CI0100
            11            OC03-CAERU  PICTURE  X(4).                    CI0100
            11            OC03-IFDGO  PICTURE  X.                       CI0100
            11            OC03-PSLLD2 PICTURE  S99V999                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-ISLOR2 PICTURE  X.                       CI0100
            11            OC03-QSFIO  PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-QSFID  PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-CGDIN  PICTURE  X.                       CI0100
            11            OC03-DGDIN  PICTURE  9(8).                    CI0100
            10            OC03-GD30                                     CI0100
                          REDEFINES            OC03-GD99.               CI0100
            11            OC03-ISKED  PICTURE  X.                       CI0100
            11            OC03-CENXC  PICTURE  9(2).                    CI0100
            11            OC03-GD31.                                    CI0100
            12            OC03-FILLER PICTURE  X(245).                  CI0100
            11            OC03-GD32                                     CI0100
                          REDEFINES            OC03-GD31.               CI0100
            12            OC03-IABIN1 PICTURE  X.                       CI0100
            12            OC03-CLDOD  PICTURE  9(8).                    CI0100
            12            OC03-NCLAM  PICTURE  9(5)                     CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-ISURR  PICTURE  X.                       CI0100
            12            OC03-GEHCD  PICTURE  9(3).                    CI0100
            12            OC03-CRATC  PICTURE  9(4).                    CI0100
            12            OC03-AMAXD  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-ASCHGA PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-APYOM  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-IWTHH1 PICTURE  X.                       CI0100
            12            OC03-CPAYCL PICTURE  X(2).                    CI0100
            12            OC03-CTSAO  PICTURE  X.                       CI0100
            12            OC03-NCONF  PICTURE  9(08).                   CI0100
            12            OC03-CLID   PICTURE  X(23).                   CI0100
            12            OC03-CARTY  PICTURE  99.                      CI0100
            12            OC03-NARRS  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-CARTZ  PICTURE  99.                      CI0100
            12            OC03-NAPDS  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-CPMTO  PICTURE  X.                       CI0100
            12            OC03-DNPMT  PICTURE  9(8).                    CI0100
            12            OC03-IPCTV  PICTURE  X.                       CI0100
            12            OC03-IMECH  PICTURE  X(01).                   CI0100
            12            OC03-IMVAO  PICTURE  X(1).                    CI0100
            12            OC03-AMVA1  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-CACTS  PICTURE  X.                       CI0100
            12            OC03-CTSPP  PICTURE  X(1).                    CI0100
            12            OC03-CACT4  PICTURE  X(2).                    CI0100
            12            OC03-IVAEX  PICTURE  X.                       CI0100
            12            OC03-DFPMT  PICTURE  9(8).                    CI0100
            12            OC03-IDEMD  PICTURE  X.                       CI0100
            12            OC03-IOFST  PICTURE  X.                       CI0100
            12            OC03-AMXLB  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-ACULB  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-DEIRNB PICTURE  9(8).                    CI0100
            12            OC03-DEFFE  PICTURE  9(8).                    CI0100
            12            OC03-DEFFR  PICTURE  9(8).                    CI0100
            12            OC03-ISPUP  PICTURE  X.                       CI0100
            12            OC03-CPNCG  PICTURE  X.                       CI0100
            12            OC03-IEXPU  PICTURE  X.                       CI0100
            12            OC03-IPPCF  PICTURE  X.                       CI0100
            12            OC03-NAAPT  PICTURE  9(2).                    CI0100
            12            OC03-PWHLDS PICTURE  S999V9(5)                CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-ISWHO  PICTURE  X(1).                    CI0100
            11            OC03-GD33                                     CI0100
                          REDEFINES            OC03-GD31.               CI0100
            12            OC03-CPAYC  PICTURE  X(2).                    CI0100
            12            OC03-ADBRQX PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-ADBRQV PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-APTXR  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-CTRTPE PICTURE  X(2).                    CI0100
            12            OC03-NCLAMI PICTURE  S9(9)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-CLIDO8 PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-CLIDN  PICTURE  X(20).                   CI0100
            12            OC03-DSET01 PICTURE  S9(8)                    CI0100
                          BINARY.                                       CI0100
            12            OC03-CTSET1 PICTURE  S9(6)                    CI0100
                          BINARY.                                       CI0100
            12            OC03-DSET02 PICTURE  S9(8)                    CI0100
                          BINARY.                                       CI0100
            12            OC03-CTSET2 PICTURE  S9(6)                    CI0100
                          BINARY.                                       CI0100
            11            OC03-GD34                                     CI0100
                          REDEFINES            OC03-GD31.               CI0100
            12            OC03-QNOFM  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-CLTRM  PICTURE  99.                      CI0100
            12            OC03-AMXLN  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-ALADJ  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-ACHK   PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-APRMO  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-IMECH1 PICTURE  X(01).                   CI0100
            12            OC03-CACT41 PICTURE  X(2).                    CI0100
            12            OC03-ACDSCC PICTURE  S9(05)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-ACDSCD PICTURE  S9(05)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-GD39                                     CI0100
                          REDEFINES            OC03-GD31.               CI0100
            12            OC03-GD31.                                    CI0100
            13            OC03-FILLER PICTURE  X(245).                  CI0100
            10            OC03-GD40                                     CI0100
                          REDEFINES            OC03-GD99.               CI0100
            11            OC03-NTR    PICTURE  9(8).                    CI0100
            11            OC03-NPBNC  PICTURE  X(24).                   CI0100
            11            OC03-CRREV  PICTURE  X(3).                    CI0100
            11            OC03-CSUSL  PICTURE  S9.                      CI0100
            11            OC03-NMGLN1 PICTURE  9(4).                    CI0100
            11            OC03-DCAC92 PICTURE  9(8).                    CI0100
            11            OC03-NAASQ3 PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-GD49.                                    CI0100
            12            OC03-FILLER PICTURE  X(198).                  CI0100
            11            OC03-GD41                                     CI0100
                          REDEFINES            OC03-GD49.               CI0100
            12            OC03-CRREF  PICTURE  9(2).                    CI0100
            12            OC03-CORIR  PICTURE  X(02).                   CI0100
            12            OC03-CIPDB  PICTURE  X(03).                   CI0100
            12            OC03-CPAYH  PICTURE  X(02).                   CI0100
            12            OC03-NAMEX  PICTURE  9(15)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC03-DCHAE  PICTURE  9(4).                    CI0100
            12            OC03-DRQST  PICTURE  S9(8)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-GD42                                     CI0100
                          REDEFINES            OC03-GD49.               CI0100
            12            OC03-CPMTCB PICTURE  X(3).                    CI0100
            10            OC03-GD50                                     CI0100
                          REDEFINES            OC03-GD99.               CI0100
            11            OC03-ALOAD  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-PSLLD4 PICTURE  S99V999                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-CSUSL1 PICTURE  S9.                      CI0100
            11            OC03-CRREV1 PICTURE  X(3).                    CI0100
            11            OC03-ADDAC  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-DL13.                                    CI0100
            12            OC03-GEYR   PICTURE  9(4).                    CI0100
            12            OC03-GEMTH  PICTURE  99.                      CI0100
            12            OC03-NDAY   PICTURE  99.                      CI0100
            11            OC03-NSEQ3P PICTURE  S9(5)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-XZ6A   PICTURE  X(6).                    CI0100
            11            OC03-XZ7    PICTURE  X(7).                    CI0100
            11            OC03-XZ6B   PICTURE  X(6).                    CI0100
            11            OC03-XZ6    PICTURE  X(6).                    CI0100
            11            OC03-XZ6C   PICTURE  X(6).                    CI0100
            11            OC03-XZ20   PICTURE  X(20).                   CI0100
            11            OC03-CATRN1 PICTURE  9(6).                    CI0100
            11            OC03-ADDAC2 PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-ATAXT2 PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-ACOMOT PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-XZ5    PICTURE  X(5).                    CI0100
            11            OC03-IREVD  PICTURE  X(1).                    CI0100
            11            OC03-ISUSP1 PICTURE  X(1).                    CI0100
            11            OC03-XZ6D   PICTURE  X(6).                    CI0100
            11            OC03-XZ13   PICTURE  X(13).                   CI0100
            11            OC03-CWHTP2 PICTURE  X(3).                    CI0100
            11            OC03-CWHTP3 PICTURE  X(3).                    CI0100
            11            OC03-DTREN  PICTURE  9(8).                    CI0100
            11            OC03-NAASQ1 PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            OC03-GD51                                     CI0100
                          REDEFINES            OC03-GD99.               CI0100
            11            OC03-ADOMOT PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-ACGLT  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-ACGST  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-CTXMT  PICTURE  9(2).                    CI0100
            11            OC03-ALOAD3 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-FILLER PICTURE  X(31).                   CI0100
            10            OC03-GD52                                     CI0100
                          REDEFINES            OC03-GD99.               CI0100
            11            OC03-DEFFT5 PICTURE  9(8).                    CI0100
            11            OC03-PSLLD5 PICTURE  S99V999                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-CSUSL2 PICTURE  S9.                      CI0100
            11            OC03-ALOAD2 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-DL22.                                    CI0100
            12            OC03-NYEAR1 PICTURE  9(4).                    CI0100
            12            OC03-GEMTHA PICTURE  99.                      CI0100
            12            OC03-NDAY01 PICTURE  99.                      CI0100
            11            OC03-NSEQ3R PICTURE  S9(5)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-CWHTP  PICTURE  X(3).                    CI0100
            11            OC03-CWHFR  PICTURE  X(3).                    CI0100
            11            OC03-CATRN7 PICTURE  9(6).                    CI0100
            11            OC03-ATAXT5 PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-QSHOT  PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-ACINT3 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-CWHTP1 PICTURE  X(3).                    CI0100
            11            OC03-CWHFR1 PICTURE  X(3).                    CI0100
            11            OC03-ACOMO5 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-QSHOMU PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-ACASH1 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-FILLER PICTURE  X(04).                   CI0100
            11            OC03-CATRN8 PICTURE  9(6).                    CI0100
            11            OC03-ALOAD1 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-PSLLD1 PICTURE  S99V999                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-QSHOT1 PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-ACINT4 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-CSUSL4 PICTURE  S9.                      CI0100
            11            OC03-ACOMO4 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            OC03-GD60                                     CI0100
                          REDEFINES            OC03-GD99.               CI0100
            11            OC03-GEOPDD PICTURE  X(8)                     CI0100
                          OCCURS       005     TIMES.                   CI0100
            11            OC03-DLAUP3 PICTURE  9(8)                     CI0100
                          OCCURS       005     TIMES.                   CI0100
            11            OC03-GEOPDB PICTURE  X(8).                    CI0100
            11            OC03-DLAUP4 PICTURE  9(8).                    CI0100
            11            OC03-ITELR2 PICTURE  X.                       CI0100
            11            OC03-IPMTA  PICTURE  X.                       CI0100
            11            OC03-CCSMG  PICTURE  X.                       CI0100
            11            OC03-CPLEC  PICTURE  XX.                      CI0100
            11            OC03-CORTYA PICTURE  X(3).                    CI0100
            11            OC03-CACTBC PICTURE  X(1).                    CI0100
            11            OC03-CGSPIA PICTURE  X.                       CI0100
            11            OC03-IPTRDA PICTURE  X(01).                   CI0100
            11            OC03-GCUSPY PICTURE  X(12).                   CI0100
            11            OC03-CPALLA PICTURE  X(1).                    CI0100
            11            OC03-QSHO5A PICTURE  S9(9)V999                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-IFRSAB PICTURE  X.                       CI0100
            11            OC03-DELOI  PICTURE  9(8).                    CI0100
            11            OC03-IAROAA PICTURE  X.                       CI0100
            11            OC03-ACINVR PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-ILTINA PICTURE  X.                       CI0100
            11            OC03-ALOIDA PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC03-CFUNTA PICTURE  X(2).                    CI0100
            11            OC03-CLGND  PICTURE  X.                       CI0100
            11            OC03-CPH3U  PICTURE  X.                       CI0100
            11            OC03-GESTD  PICTURE  9(8).                    CI0100
            11            OC03-GEEND  PICTURE  9(8).                    CI0100
            11            OC03-CPMTF  PICTURE  99.                      CI0100
            11            OC03-CNAVR  PICTURE  X(1).                    CI0100
            10            OC03-GD70                                     CI0100
                          REDEFINES            OC03-GD99.               CI0100
            11            OC03-CMEMO  PICTURE  X(2).                    CI0100
            11            OC03-ALPLDT PICTURE  9(8).                    CI0100
            11            OC03-CTLPD  PICTURE  9(8).                    CI0100
            11            OC03-CPAYCM PICTURE  X(2).                    CI0100
       01                 OC04.                                         CI0100
            10            OC04-CLCUS  PICTURE  99.                      CI0100
            10            OC04-CCACT  PICTURE  99.                      CI0100
            10            OC04-AFEET  PICTURE  S9(5)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            OC04-ITERF  PICTURE  X.                       CI0100
            10            OC04-ATERF  PICTURE  S9(5)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            OC04-CLDOB  PICTURE  9(8).                    CI0100
            10            OC04-CPLTYP PICTURE  X(14).                   CI0100
            10            OC04-IACFPD PICTURE  X(1).                    CI0100
            10            OC04-FILLER PICTURE  X(14).                   CI0100
       01                 OC06.                                         CI0100
            10            OC06-GELL   PICTURE  9(4)                     CI0100
                          BINARY.                                       CI0100
            10            OC06-GE00.                                    CI0100
            11            OC06-GC06K.                                   CI0100
            12            OC06-NPISQ  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC06-ACOTD  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC06-PPOTD  PICTURE  S9(3)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC06-QPSTD  PICTURE  S9(7)V999                CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC06-CPITC  PICTURE  99.                      CI0100
            11            OC06-ITRNB  PICTURE  X.                       CI0100
            11            OC06-FILLER PICTURE  X(14).                   CI0100
            10            OC06-GE98.                                    CI0100
            11            OC06-FILLER PICTURE  X(240).                  CI0100
            10            OC06-GE10                                     CI0100
                          REDEFINES            OC06-GE98.               CI0100
            11            OC06-CDELI  PICTURE  9(3).                    CI0100
            11            OC06-CPAYC  PICTURE  X(2).                    CI0100
            11            OC06-ICHKP  PICTURE  X.                       CI0100
            11            OC06-CLTIN  PICTURE  9(12).                   CI0100
            11            OC06-IFHAI  PICTURE  X.                       CI0100
            11            OC06-CDQUA  PICTURE  X(2).                    CI0100
            11            OC06-FILLER PICTURE  X(07).                   CI0100
            11            OC06-GE99.                                    CI0100
            12            OC06-FILLER PICTURE  X(212).                  CI0100
            11            OC06-GE01                                     CI0100
                          REDEFINES            OC06-GE99.               CI0100
            12            OC06-NTR    PICTURE  9(8).                    CI0100
            12            OC06-GECKD  PICTURE  9.                       CI0100
            12            OC06-NPBN   PICTURE  X(20).                   CI0100
            12            OC06-CCBAT  PICTURE  99.                      CI0100
            12            OC06-CLID4  PICTURE  X(23).                   CI0100
            12            OC06-GENAL1 PICTURE  X(30)                    CI0100
                          OCCURS       002     TIMES.                   CI0100
            12            OC06-GESAD1 PICTURE  X(30)                    CI0100
                          OCCURS       003     TIMES.                   CI0100
            11            OC06-GE02                                     CI0100
                          REDEFINES            OC06-GE99.               CI0100
            12            OC06-GENAL  PICTURE  X(30)                    CI0100
                          OCCURS       002     TIMES.                   CI0100
            12            OC06-GESAD  PICTURE  X(30)                    CI0100
                          OCCURS       003     TIMES.                   CI0100
            11            OC06-GE03                                     CI0100
                          REDEFINES            OC06-GE99.               CI0100
            12            OC06-NCHKN  PICTURE  9(11).                   CI0100
            11            OC06-GE04                                     CI0100
                          REDEFINES            OC06-GE99.               CI0100
            12            OC06-CTIDAP PICTURE  9(3).                    CI0100
            12            OC06-PRCOD  PICTURE  9(5).                    CI0100
            12            OC06-TDELI  PICTURE  X(30).                   CI0100
            12            OC06-CINCD  PICTURE  9(02).                   CI0100
            10            OC06-GE20                                     CI0100
                          REDEFINES            OC06-GE98.               CI0100
            11            OC06-C299.                                    CI0100
            12            OC06-CTID.                                    CI0100
            13            OC06-CTIDA  PICTURE  9(3).                    CI0100
            13            OC06-CTIDN.                                   CI0100
            14            OC06-CTIDNP PICTURE  X(13).                   CI0100
            14            OC06-CTIDND PICTURE  9(11).                   CI0100
            11            OC06-DCACG9 PICTURE  9(8).                    CI0100
            11            OC06-NAASQ  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            OC06-CIRAP  PICTURE  XX.                      CI0100
            11            OC06-CTYPE  PICTURE  X.                       CI0100
            11            OC06-INACT  PICTURE  X.                       CI0100
            11            OC06-FILLER PICTURE  X(01).                   CI0100
            11            OC06-ITPAC  PICTURE  X.                       CI0100
            11            OC06-ITAXI  PICTURE  X.                       CI0100
            11            OC06-IOWNC  PICTURE  X.                       CI0100
            11            OC06-CDVCD  PICTURE  X(2).                    CI0100
            11            OC06-CTCUS  PICTURE  999.                     CI0100
            11            OC06-CPMTCB PICTURE  X(3).                    CI0100
            11            OC06-CASTC1 PICTURE  99.                      CI0100
            11            OC06-PRCOD1 PICTURE  9(5).                    CI0100
            11            OC06-CPRSC1 PICTURE  X(9).                    CI0100
            11            OC06-CPRTB  PICTURE  X.                       CI0100
            11            OC06-CBRKD  PICTURE  9(4).                    CI0100
            11            OC06-FILLER PICTURE  X(12).                   CI0100
            10            OC06-GE30                                     CI0100
                          REDEFINES            OC06-GE98.               CI0100
            11            OC06-CFIDC  PICTURE  X(5).                    CI0100
            11            OC06-CPHSE  PICTURE  9(2).                    CI0100
            11            OC06-FILLER PICTURE  X(05).                   CI0100
            11            OC06-IABIN  PICTURE  X.                       CI0100
            11            OC06-PDFND  PICTURE  S999V9(3)                CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            OC06-GE40                                     CI0100
                          REDEFINES            OC06-GE98.               CI0100
            11            OC06-CACCT  PICTURE  X.                       CI0100
            11            OC06-CPAYR  PICTURE  X(2).                    CI0100
            11            OC06-CDELI1 PICTURE  9(3).                    CI0100
            11            OC06-CATRN.                                   CI0100
            12            OC06-CATRF  PICTURE  9(3).                    CI0100
            12            OC06-CATRS  PICTURE  9(3).                    CI0100
            11            OC06-DEFFT  PICTURE  9(8).                    CI0100
            11            OC06-CTYPC  PICTURE  X.                       CI0100
            11            OC06-CIRAPA PICTURE  XX.                      CI0100
            11            OC06-FILLER PICTURE  X(09).                   CI0100
            11            OC06-GE49.                                    CI0100
            12            OC06-FILLER PICTURE  X(208).                  CI0100
            11            OC06-GE41                                     CI0100
                          REDEFINES            OC06-GE49.               CI0100
            12            OC06-NCHKN1 PICTURE  9(6).                    CI0100
            11            OC06-GE42                                     CI0100
                          REDEFINES            OC06-GE49.               CI0100
            12            OC06-CTID1.                                   CI0100
            13            OC06-CTIDA1 PICTURE  9(3).                    CI0100
            13            OC06-CTIDP1 PICTURE  X(13).                   CI0100
            13            OC06-CTIDN1 PICTURE  9(11).                   CI0100
            11            OC06-GE43                                     CI0100
                          REDEFINES            OC06-GE49.               CI0100
            12            OC06-GENAL2 PICTURE  X(30)                    CI0100
                          OCCURS       002     TIMES.                   CI0100
            12            OC06-GESAD2 PICTURE  X(30)                    CI0100
                          OCCURS       003     TIMES.                   CI0100
            11            OC06-GE44                                     CI0100
                          REDEFINES            OC06-GE49.               CI0100
            12            OC06-CTID01.                                  CI0100
            13            OC06-CTIDA6 PICTURE  9(3).                    CI0100
            13            OC06-NTIDP2 PICTURE  X(13).                   CI0100
            13            OC06-CTIDN2 PICTURE  9(11).                   CI0100
            12            OC06-GECKD2 PICTURE  9.                       CI0100
            12            OC06-PACCT  PICTURE  S999V99                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC06-PLOAN  PICTURE  S999V99                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC06-PADPT  PICTURE  S999V99                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            OC06-IPCTL  PICTURE  X.                       CI0100
            12            OC06-IPCTP  PICTURE  X.                       CI0100
            12            OC06-CEUNT  PICTURE  S9(5)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            OC06-GE31                                     CI0100
                          REDEFINES            OC06-GE98.               CI0100
            11            OC06-GCUSPZ PICTURE  X(12).                   CI0100
      *CREATE INDEX --->  PJ41 - USED FOR ACCESSING ARRAY
      ******************************************************************
      **                                                               *
      ** GENERAL WORK AREA FOR TEMPORARY DATA MANIPULATION             *
      **                                                               *
      ******************************************************************
        01  WS01-AREA.
      **
      ** AREA USED TO CONVERT PACKED FIELD TO ZONED
      **
      *!WE
            05  WS01-GESQ2C
                        PICTURE 99.                                     CI0100
            05  WS01-XESQ2C  REDEFINES WS01-GESQ2C PIC X(02).
      *
      **
      ** AREA USED TO IDENTIFY WHEN RESTART WAS SUCCESSFUL
      **
            05  W-RESTART-GOOD        PIC X(01) VALUE 'N'.
       01   DEBUT-WSS.                                                  CI0100
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0100
            05   IK     PICTURE X.                                      CI0100
       01  CONSTANTES-PAC.                                              CI0100
           05  FILLER  PICTURE X(87)   VALUE                            CI0100
                     '6015 CAT09/08/14CI0100ADMIN   14:34:44CI0100P AMERCI0100
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0100
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0100
           05  NUGNA   PICTURE X(5).                                    CI0100
           05  APPLI   PICTURE X(3).                                    CI0100
           05  DATGN   PICTURE X(8).                                    CI0100
           05  PROGR   PICTURE X(6).                                    CI0100
           05  CODUTI  PICTURE X(8).                                    CI0100
           05  TIMGN   PICTURE X(8).                                    CI0100
           05  PROGE   PICTURE X(8).                                    CI0100
           05  COBASE  PICTURE X(4).                                    CI0100
           05  DATGNC  PICTURE X(10).                                   CI0100
           05  RELEAS  PICTURE X(7).                                    CI0100
           05  DATGE   PICTURE X(10).                                   CI0100
           05  DATSQ   PICTURE X(10).                                   CI0100
       01  DATCE.                                                       CI0100
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0100
         05  DATOR.                                                     CI0100
           10  DATOA  PICTURE XX.                                       CI0100
           10  DATOM  PICTURE XX.                                       CI0100
           10  DATOJ  PICTURE XX.                                       CI0100
       01   VARIABLES-CONDITIONNELLES.                                  CI0100
            05                  FT      PICTURE X VALUE '0'.            CI0100
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0100
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0100
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           IPJ41L PICTURE S9(4) VALUE  ZERO.
            05           IPJ41R PICTURE S9(4) VALUE  ZERO.
            05           IPJ41M PICTURE S9(4) VALUE +0010.
            05           J02DDR PICTURE S9(4) VALUE  ZERO.
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0100
            05       5-GC00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0100
            05       5-OC00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0100
       01               S-GC01-SSA.                                     CI0100
            10         S1-GC01-SEGNAM PICTURE X(8)                      CI0100
                                      VALUE 'GC01    '.                 CI0100
            10         S1-GC01-CCOM   PICTURE X VALUE '*'.              CI0100
            10          S-GC01-CCOD   PICTURE X(5)                      CI0100
                                      VALUE '-----'.                    CI0100
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0100
       01            S-GCU01-SSA.                                       CI0100
            10      S1-GCU01-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC01    '.                 CI0100
            10      S1-GCU01-CCOM   PICTURE X VALUE '*'.                CI0100
            10       S-GCU01-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            10      S1-GCU01-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(GC01K'.                   CI0100
            10       S-GCU01-OPER  PICTURE XX VALUE ' ='.               CI0100
            10       S-GCU01-GC01K.                                     CI0100
            11       S-GCU01-C299.                                      CI0100
            12       S-GCU01-CTID.                                      CI0100
            13       S-GCU01-CTIDA    PICTURE  9(3).                    CI0100
            13       S-GCU01-CTIDN.                                     CI0100
            14       S-GCU01-CTIDNP   PICTURE  X(13).                   CI0100
            14       S-GCU01-CTIDND   PICTURE  9(11).                   CI0100
            10  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01               S-GC03-SSA.                                     CI0100
            10         S1-GC03-SEGNAM PICTURE X(8)                      CI0100
                                      VALUE 'GC03    '.                 CI0100
            10         S1-GC03-CCOM   PICTURE X VALUE '*'.              CI0100
            10          S-GC03-CCOD   PICTURE X(5)                      CI0100
                                      VALUE '-----'.                    CI0100
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0100
       01            S-GCA03-SSA.                                       CI0100
            11      S1-GCA03-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GCA03-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCA03-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCA03-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(CAATY'.                   CI0100
            11       S-GCA03-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCA03-CAATY    PICTURE  9(3).                    CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GCB03-SSA.                                       CI0100
            11      S1-GCB03-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GCB03-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCB03-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCB03-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(CVSYS'.                   CI0100
            11       S-GCB03-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCB03-CVSYS    PICTURE  X(2).                    CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GCC03-SSA.                                       CI0100
            11      S1-GCC03-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GCC03-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCC03-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCC03-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(CASTC'.                   CI0100
            11       S-GCC03-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCC03-CASTC    PICTURE  99.                      CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GCD03-SSA.                                       CI0100
            11      S1-GCD03-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GCD03-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCD03-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCD03-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(CACTO'.                   CI0100
            11       S-GCD03-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCD03-CACTO    PICTURE  9(3).                    CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GCE03-SSA.                                       CI0100
            11      S1-GCE03-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GCE03-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCE03-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCE03-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(IPULL'.                   CI0100
            11       S-GCE03-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCE03-IPULL    PICTURE  X.                       CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GCF03-SSA.                                       CI0100
            11      S1-GCF03-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GCF03-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCF03-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCF03-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(DTRAC'.                   CI0100
            11       S-GCF03-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCF03-DTRAC    PICTURE  9(8).                    CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GCG03-SSA.                                       CI0100
            11      S1-GCG03-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GCG03-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCG03-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCG03-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(CTRSO'.                   CI0100
            11       S-GCG03-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCG03-CTRSO    PICTURE  9(02).                   CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GCH03-SSA.                                       CI0100
            11      S1-GCH03-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GCH03-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCH03-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCH03-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(NTRCE'.                   CI0100
            11       S-GCH03-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCH03-NTRCE    PICTURE  9(06).                   CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GCI03-SSA.                                       CI0100
            11      S1-GCI03-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GCI03-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCI03-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCI03-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(ITRAN'.                   CI0100
            11       S-GCI03-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCI03-ITRAN    PICTURE  X.                       CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GCJ03-SSA.                                       CI0100
            11      S1-GCJ03-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GCJ03-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCJ03-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCJ03-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(DEFFT'.                   CI0100
            11       S-GCJ03-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCJ03-DEFFT    PICTURE  9(8).                    CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GCK03-SSA.                                       CI0100
            11      S1-GCK03-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GCK03-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCK03-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCK03-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(CPMTCA'.                  CI0100
            11       S-GCK03-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCK03-CPMTCA   PICTURE  XXX.                     CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GCL03-SSA.                                       CI0100
            11      S1-GCL03-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GCL03-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCL03-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCL03-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(ACASH'.                   CI0100
            11       S-GCL03-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCL03-ACASH    PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GCN03-SSA.                                       CI0100
            11      S1-GCN03-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GCN03-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCN03-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCN03-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(CRREV'.                   CI0100
            11       S-GCN03-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCN03-CRREV    PICTURE  X(3).                    CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GCO03-SSA.                                       CI0100
            11      S1-GCO03-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GCO03-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCO03-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCO03-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(CSYST'.                   CI0100
            11       S-GCO03-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCO03-CSYST    PICTURE  99.                      CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GCU03-SSA.                                       CI0100
            11      S1-GCU03-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GCU03-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCU03-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCU03-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(GC03K'.                   CI0100
            11       S-GCU03-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCU03-GC03K.                                     CI0100
            12       S-GCU03-DCACG9   PICTURE  9(8).                    CI0100
            12       S-GCU03-NAASQ    PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GC103-SSA.                                       CI0100
            12      S1-GC103-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            12      S1-GC103-CCOM   PICTURE X VALUE '*'.                CI0100
            12       S-GC103-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            12      S1-GC103-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(XDCACG9'.                 CI0100
            12       S-GC103-OPER  PICTURE XX VALUE ' ='.               CI0100
            12       S-GC103-DCACG9   PICTURE  9(8).                    CI0100
            12  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GC203-SSA.                                       CI0100
            11      S1-GC203-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GC203-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GC203-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GC203-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(XGEAUN'.                  CI0100
            11       S-GC203-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GC203-GEAUN    PICTURE  9(5).                    CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GC303-SSA.                                       CI0100
            11      S1-GC303-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GC303-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GC303-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GC303-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(XGEOPD2'.                 CI0100
            11       S-GC303-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GC303-GEOPD2   PICTURE  X(8).                    CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GC403-SSA.                                       CI0100
            11      S1-GC403-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            11      S1-GC403-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GC403-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GC403-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(XNBTCH'.                  CI0100
            11       S-GC403-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GC403-NBTCH    PICTURE  9(4).                    CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GC803-SSA.                                       CI0100
            12      S1-GC803-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC03    '.                 CI0100
            12      S1-GC803-CCOM   PICTURE X VALUE '*'.                CI0100
            12       S-GC803-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            12      S1-GC803-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(XNAASQ'.                  CI0100
            12       S-GC803-OPER  PICTURE XX VALUE ' ='.               CI0100
            12       S-GC803-NAASQ    PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01               S-GC04-SSA.                                     CI0100
            10         S1-GC04-SEGNAM PICTURE X(8)                      CI0100
                                      VALUE 'GC04    '.                 CI0100
            10         S1-GC04-CCOM   PICTURE X VALUE '*'.              CI0100
            10          S-GC04-CCOD   PICTURE X(5)                      CI0100
                                      VALUE '-----'.                    CI0100
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0100
       01               S-GC06-SSA.                                     CI0100
            10         S1-GC06-SEGNAM PICTURE X(8)                      CI0100
                                      VALUE 'GC06    '.                 CI0100
            10         S1-GC06-CCOM   PICTURE X VALUE '*'.              CI0100
            10          S-GC06-CCOD   PICTURE X(5)                      CI0100
                                      VALUE '-----'.                    CI0100
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0100
       01            S-GCF06-SSA.                                       CI0100
            11      S1-GCF06-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC06    '.                 CI0100
            11      S1-GCF06-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCF06-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCF06-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(PRCOD1'.                  CI0100
            11       S-GCF06-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCF06-PRCOD1   PICTURE  9(5).                    CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01            S-GCU06-SSA.                                       CI0100
            11      S1-GCU06-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC06    '.                 CI0100
            11      S1-GCU06-CCOM   PICTURE X VALUE '*'.                CI0100
            11       S-GCU06-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            11      S1-GCU06-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(GC06K'.                   CI0100
            11       S-GCU06-OPER  PICTURE XX VALUE ' ='.               CI0100
            11       S-GCU06-GC06K.                                     CI0100
            12       S-GCU06-NPISQ    PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01               S-GC12-SSA.                                     CI0100
            10         S1-GC12-SEGNAM PICTURE X(8)                      CI0100
                                      VALUE 'GC12    '.                 CI0100
            10         S1-GC12-CCOM   PICTURE X VALUE '*'.              CI0100
            10          S-GC12-CCOD   PICTURE X(5)                      CI0100
                                      VALUE '-----'.                    CI0100
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0100
       01            S-GCU12-SSA.                                       CI0100
            10      S1-GCU12-SEGNAM PICTURE X(8)                        CI0100
                                      VALUE 'GC12    '.                 CI0100
            10      S1-GCU12-CCOM   PICTURE X VALUE '*'.                CI0100
            10       S-GCU12-CCOD   PICTURE X(5)                        CI0100
                                      VALUE '-----'.                    CI0100
            10      S1-GCU12-FLDNAM PICTURE X(9)                        CI0100
                                      VALUE '(GC12K'.                   CI0100
            10       S-GCU12-OPER  PICTURE XX VALUE ' ='.               CI0100
            10       S-GCU12-GC12K.                                     CI0100
            11       S-GCU12-CIRAP    PICTURE  XX.                      CI0100
            10  FILLER   PICTURE X    VALUE ')'.                        CI0100
       01               S-GC21-SSA.                                     CI0100
            10         S1-GC21-SEGNAM PICTURE X(8)                      CI0100
                                      VALUE 'GC21    '.                 CI0100
            10         S1-GC21-CCOM   PICTURE X VALUE '*'.              CI0100
            10          S-GC21-CCOD   PICTURE X(5)                      CI0100
                                      VALUE '-----'.                    CI0100
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0100
       01   ZONES-UTILISATEUR PICTURE X.                                CI0100
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
       01                 PA00.                                         CI0100
          05              PA00-SUITE.                                   CI0100
            15       FILLER         PICTURE  X(00106).                  CI0100
       01                 PA06  REDEFINES      PA00.                    CI0100
            10            PA06-XDBPCB.                                  CI0100
            11            PA06-XDBDNM PICTURE  X(08).                   CI0100
            11            PA06-XSEGLV PICTURE  X(02).                   CI0100
            11            PA06-XRC    PICTURE  X(02).                   CI0100
            11            PA06-XPROPT PICTURE  X(04).                   CI0100
            11            PA06-FILLER PICTURE  S9(5)                    CI0100
                          BINARY.                                       CI0100
            11            PA06-XSEGNM PICTURE  X(08).                   CI0100
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0100
                          BINARY.                                       CI0100
            11            PA06-XSEGNB PICTURE  9(05)                    CI0100
                          BINARY.                                       CI0100
            11            PA06-XCOKEY PICTURE  X(70).                   CI0100
      *>>>>>> UNIQUE SEGMENTS TO THIS ROUTINE
      *!WF DSP=PJ DSL=PJ SEL=40 FOR=I DES=1 LEV=1 PLT=75
       01                 PJ40.                                         CI0100
            10            PJ40-MAPPN  PICTURE  X(10).                   CI0100
            10            PJ40-CFUNC  PICTURE  X(3).                    CI0100
            10            PJ40-CASTC  PICTURE  99                       CI0100
                          OCCURS       006     TIMES.                   CI0100
            10            PJ40-CAATY  PICTURE  9(3)                     CI0100
                          OCCURS       003     TIMES.                   CI0100
            10            PJ40-C299.                                    CI0100
            11            PJ40-CTID.                                    CI0100
            12            PJ40-CTIDA  PICTURE  9(3).                    CI0100
            12            PJ40-CTIDN.                                   CI0100
            13            PJ40-CTIDNP PICTURE  X(13).                   CI0100
            13            PJ40-CTIDND PICTURE  9(11).                   CI0100
            10            PJ40-DCACG9 PICTURE  9(8).                    CI0100
            10            PJ40-NAASQ  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            PJ40-NPISQ  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            PJ40-CIRAP  PICTURE  XX.                      CI0100
            10            PJ40-IPERT  PICTURE  X.                       CI0100
            10            PJ40-NEIBT  PICTURE  X(7).                    CI0100
            10            PJ40-GESQ2C PICTURE  S99                      CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            PJ40-MIPPS  PICTURE  X(4).                    CI0100
            10            PJ40-IENDP  PICTURE  X.                       CI0100
            10            PJ40-FILLER PICTURE  X(20).                   CI0100
       01                 PJ41.                                         CI0100
            10            PJ41-IENDP  PICTURE  X.                       CI0100
            10            PJ41-MIPPS  PICTURE  X(4).                    CI0100
            10            PJ41-GC01.                                    CI0100
            11            PJ41-GC01K.                                   CI0100
            12            PJ41-C299.                                    CI0100
            13            PJ41-CTID.                                    CI0100
            14            PJ41-CTIDA  PICTURE  9(3).                    CI0100
            14            PJ41-CTIDN.                                   CI0100
            15            PJ41-CTIDNP PICTURE  X(13).                   CI0100
            15            PJ41-CTIDND PICTURE  9(11).                   CI0100
            11            PJ41-DCAG9L PICTURE  9(8).                    CI0100
            11            PJ41-NAASQL PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            PJ41-ICUST  PICTURE  X.                       CI0100
            11            PJ41-NSEQ4B PICTURE  9(8)                     CI0100
                          BINARY.                                       CI0100
            11            PJ41-PRCOD  PICTURE  9(5).                    CI0100
            11            PJ41-PRSCD  PICTURE  X(9).                    CI0100
            11            PJ41-FILLER PICTURE  X(8).                    CI0100
            10            PJ41-IGC01  PICTURE  X(01).                   CI0100
            10            PJ41-QDECT9 PICTURE  99.                      CI0100
            10            PJ41-FILLER PICTURE  X(20).                   CI0100
            10            PJ41-GAKEY                                    CI0100
                          OCCURS       010     TIMES.                   CI0100
            11            PJ41-IGC03  PICTURE  X(01).                   CI0100
            11            PJ41-IGC04  PICTURE  X(01).                   CI0100
            11            PJ41-IGC06  PICTURE  X(01).                   CI0100
            11            PJ41-IGC12  PICTURE  X(01).                   CI0100
            11            PJ41-IGC21  PICTURE  X(01).                   CI0100
            11            PJ41-GC03.                                    CI0100
            12            PJ41-GELL   PICTURE  9(4)                     CI0100
                          BINARY.                                       CI0100
            12            PJ41-GD00.                                    CI0100
            13            PJ41-GC03K.                                   CI0100
            14            PJ41-DCACG9 PICTURE  9(8).                    CI0100
            14            PJ41-NAASQ  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CAATY  PICTURE  9(3).                    CI0100
            13            PJ41-CVSYS  PICTURE  X(2).                    CI0100
            13            PJ41-CACTO  PICTURE  9(3).                    CI0100
            13            PJ41-CATRN.                                   CI0100
            14            PJ41-CATRF  PICTURE  9(3).                    CI0100
            14            PJ41-CATRS  PICTURE  9(3).                    CI0100
            13            PJ41-CASTC  PICTURE  99.                      CI0100
            13            PJ41-IPULL  PICTURE  X.                       CI0100
            13            PJ41-GEAUN  PICTURE  9(5).                    CI0100
            13            PJ41-GEOPD2 PICTURE  X(8).                    CI0100
            13            PJ41-NBTCH  PICTURE  9(4).                    CI0100
            13            PJ41-DEFFT  PICTURE  9(8).                    CI0100
            13            PJ41-NSUNT  PICTURE  9(4).                    CI0100
            13            PJ41-ITRAN  PICTURE  X.                       CI0100
            13            PJ41-DLAUP1 PICTURE  9(8).                    CI0100
            13            PJ41-ADRET  PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-TTRMS  PICTURE  X(12).                   CI0100
            13            PJ41-IDELT  PICTURE  X.                       CI0100
            13            PJ41-GEOPDM PICTURE  X(8).                    CI0100
            13            PJ41-FILLER PICTURE  X(07).                   CI0100
            12            PJ41-GD09.                                    CI0100
            13            PJ41-FILLER PICTURE  X(70).                   CI0100
            12            PJ41-GD01                                     CI0100
                          REDEFINES            PJ41-GD09.               CI0100
            13            PJ41-ADBRQ  PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CTRTP  PICTURE  X(2).                    CI0100
            13            PJ41-CPORT  PICTURE  X.                       CI0100
            13            PJ41-CSCRNU PICTURE  X(4).                    CI0100
            13            PJ41-DLAUP  PICTURE  9(8).                    CI0100
            13            PJ41-CTWHAT PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-PWHLD  PICTURE  S999V9(5)                CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-IWTHH  PICTURE  X.                       CI0100
            13            PJ41-NDRFT  PICTURE  9(5).                    CI0100
            13            PJ41-IDPAP  PICTURE  X.                       CI0100
            13            PJ41-GETIM  PICTURE  S9(7)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-QNACT  PICTURE  9(3).                    CI0100
            13            PJ41-AEDRQ  PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-IPLIN  PICTURE  X.                       CI0100
            13            PJ41-CLIDNB PICTURE  9(8).                    CI0100
            13            PJ41-CSLCT  PICTURE  X.                       CI0100
            13            PJ41-ITELE  PICTURE  X.                       CI0100
            13            PJ41-FILLER PICTURE  X(06).                   CI0100
            12            PJ41-GD02                                     CI0100
                          REDEFINES            PJ41-GD09.               CI0100
            13            PJ41-CSYST  PICTURE  99.                      CI0100
            13            PJ41-FILLER PICTURE  X.                       CI0100
            13            PJ41-ACASH  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-DTRAC  PICTURE  9(8).                    CI0100
            13            PJ41-CTRSO  PICTURE  9(02).                   CI0100
            13            PJ41-NTRCE  PICTURE  9(06).                   CI0100
            13            PJ41-GECKD1 PICTURE  9.                       CI0100
            13            PJ41-CCOLL  PICTURE  X(3).                    CI0100
            13            PJ41-CLTDP  PICTURE  X(3).                    CI0100
            13            PJ41-PSLLD  PICTURE  S99V999                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-ISLOR  PICTURE  X.                       CI0100
            13            PJ41-ITPAC  PICTURE  X.                       CI0100
            13            PJ41-CPMTCA PICTURE  XXX.                     CI0100
            13            PJ41-CSERV  PICTURE  X(3).                    CI0100
            13            PJ41-ACOMO  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-IPLIN1 PICTURE  X.                       CI0100
            13            PJ41-INQEX  PICTURE  X.                       CI0100
            13            PJ41-CTKRAA PICTURE  X(12).                   CI0100
            13            PJ41-CCSMQ  PICTURE  X.                       CI0100
            13            PJ41-IVAEX1 PICTURE  X.                       CI0100
            13            PJ41-IHPMT  PICTURE  X(1).                    CI0100
            13            PJ41-GETIM3 PICTURE  S9(7)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            PJ41-GD03                                     CI0100
                          REDEFINES            PJ41-GD09.               CI0100
            13            PJ41-CATRNC PICTURE  9(6).                    CI0100
            13            PJ41-APRNT1 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-QSHOWT PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-ACINVT PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-ACOMO7 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-QSHOMW PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-ATAXT3 PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CTSTR  PICTURE  9(2).                    CI0100
            13            PJ41-ICIRA  PICTURE  X.                       CI0100
            13            PJ41-GETIM2 PICTURE  S9(7)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CPMTCX PICTURE  XX.                      CI0100
            13            PJ41-FILLER PICTURE  X(16).                   CI0100
            12            PJ41-GD99.                                    CI0100
            13            PJ41-FILLER PICTURE  X(248).                  CI0100
            12            PJ41-GD10                                     CI0100
                          REDEFINES            PJ41-GD99.               CI0100
            13            PJ41-MROTC  PICTURE  X(7).                    CI0100
            13            PJ41-CEDSC  PICTURE  9(1).                    CI0100
            13            PJ41-ILPOI  PICTURE  X(1).                    CI0100
            13            PJ41-AWRCH  PICTURE  S9(3)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CHCOC1 PICTURE  9(2).                    CI0100
            13            PJ41-CHCOC2 PICTURE  9(2).                    CI0100
            13            PJ41-CHCOC3 PICTURE  9(2).                    CI0100
            13            PJ41-CHCOC4 PICTURE  9(2).                    CI0100
            13            PJ41-CMCOC1 PICTURE  9(3).                    CI0100
            13            PJ41-CMCOC2 PICTURE  9(3).                    CI0100
            13            PJ41-CMCOC3 PICTURE  9(3).                    CI0100
            13            PJ41-GD11.                                    CI0100
            14            PJ41-FILLER PICTURE  X(219).                  CI0100
            13            PJ41-GD12                                     CI0100
                          REDEFINES            PJ41-GD11.               CI0100
            14            PJ41-CELLO  PICTURE  9(1).                    CI0100
            14            PJ41-CECLO  PICTURE  9(1).                    CI0100
            14            PJ41-AEXML  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-CEPI   PICTURE  X(1).                    CI0100
            14            PJ41-CEXTY  PICTURE  X.                       CI0100
            14            PJ41-CROPC  PICTURE  9(1).                    CI0100
            14            PJ41-CPUTY  PICTURE  9(1).                    CI0100
            14            PJ41-IMCII  PICTURE  X(1).                    CI0100
            14            PJ41-GEMISC                                   CI0100
                          OCCURS       010     TIMES.                   CI0100
            15            PJ41-AMGLA  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            15            PJ41-CMGLC  PICTURE  9(1).                    CI0100
            15            PJ41-NMGLN  PICTURE  9(4).                    CI0100
            14            PJ41-ACTRN  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-IWRBK  PICTURE  X.                       CI0100
            14            PJ41-IFEDX  PICTURE  X.                       CI0100
            14            PJ41-ICNTR  PICTURE  X.                       CI0100
            14            PJ41-IOCKH  PICTURE  X.                       CI0100
            14            PJ41-ICRCK  PICTURE  X.                       CI0100
            14            PJ41-NHMPN  PICTURE  S9(10)                   CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-ITELR1 PICTURE  X.                       CI0100
            13            PJ41-GD13                                     CI0100
                          REDEFINES            PJ41-GD11.               CI0100
            14            PJ41-DREDO  PICTURE  9(8).                    CI0100
            14            PJ41-CATRNR PICTURE  9(6).                    CI0100
            14            PJ41-CEVN   PICTURE  9(9).                    CI0100
            14            PJ41-ISUSP  PICTURE  X(1).                    CI0100
            13            PJ41-GD15                                     CI0100
                          REDEFINES            PJ41-GD11.               CI0100
            14            PJ41-CPUTZ  PICTURE  9(1).                    CI0100
            14            PJ41-CETLB  PICTURE  9(3).                    CI0100
            14            PJ41-QTRMC  PICTURE  9(3).                    CI0100
            14            PJ41-DEFFTE PICTURE  9(8).                    CI0100
            14            PJ41-DEFFTF PICTURE  9(8).                    CI0100
            14            PJ41-DEFFTG PICTURE  9(8).                    CI0100
            14            PJ41-XZ1A   PICTURE  X.                       CI0100
            14            PJ41-XZ1B   PICTURE  X.                       CI0100
            14            PJ41-XZ1C   PICTURE  X.                       CI0100
            14            PJ41-XZ1D   PICTURE  X.                       CI0100
            14            PJ41-XZ1E   PICTURE  X.                       CI0100
            14            PJ41-XZ1F   PICTURE  X.                       CI0100
            14            PJ41-XZ1G   PICTURE  X.                       CI0100
            14            PJ41-XZ1H   PICTURE  X.                       CI0100
            14            PJ41-XZ1I   PICTURE  X.                       CI0100
            14            PJ41-DEFFTH PICTURE  9(8).                    CI0100
            13            PJ41-GD19                                     CI0100
                          REDEFINES            PJ41-GD11.               CI0100
            14            PJ41-GD11.                                    CI0100
            15            PJ41-FILLER PICTURE  X(219).                  CI0100
            12            PJ41-GD20                                     CI0100
                          REDEFINES            PJ41-GD99.               CI0100
            13            PJ41-ADDACT PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-ISIGV  PICTURE  X.                       CI0100
            13            PJ41-IALLF  PICTURE  X.                       CI0100
            13            PJ41-QSHOWQ PICTURE  S9(9)V999                CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CCDSCW PICTURE  9(2).                    CI0100
            13            PJ41-IDWRL  PICTURE  X.                       CI0100
            13            PJ41-ITELR  PICTURE  X.                       CI0100
            13            PJ41-IABIN  PICTURE  X.                       CI0100
            13            PJ41-PACT1  PICTURE  S999V999                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-IBFAF  PICTURE  X.                       CI0100
            13            PJ41-IFRSA  PICTURE  X.                       CI0100
            13            PJ41-ICRCAN PICTURE  X.                       CI0100
            13            PJ41-ACACTV PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-AGFND  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-QCSHOW PICTURE  S9(9)V999                CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-QCSHIS PICTURE  S9(9)V999                CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-NDTRC  PICTURE  9(8).                    CI0100
            13            PJ41-CAERU  PICTURE  X(4).                    CI0100
            13            PJ41-IFDGO  PICTURE  X.                       CI0100
            13            PJ41-PSLLD2 PICTURE  S99V999                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-ISLOR2 PICTURE  X.                       CI0100
            13            PJ41-QSFIO  PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-QSFID  PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CGDIN  PICTURE  X.                       CI0100
            13            PJ41-DGDIN  PICTURE  9(8).                    CI0100
            12            PJ41-GD30                                     CI0100
                          REDEFINES            PJ41-GD99.               CI0100
            13            PJ41-ISKED  PICTURE  X.                       CI0100
            13            PJ41-CENXC  PICTURE  9(2).                    CI0100
            13            PJ41-GD31.                                    CI0100
            14            PJ41-FILLER PICTURE  X(245).                  CI0100
            13            PJ41-GD32                                     CI0100
                          REDEFINES            PJ41-GD31.               CI0100
            14            PJ41-IABIN1 PICTURE  X.                       CI0100
            14            PJ41-CLDOD  PICTURE  9(8).                    CI0100
            14            PJ41-NCLAM  PICTURE  9(5)                     CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-ISURR  PICTURE  X.                       CI0100
            14            PJ41-GEHCD  PICTURE  9(3).                    CI0100
            14            PJ41-CRATC  PICTURE  9(4).                    CI0100
            14            PJ41-AMAXD  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-ASCHGA PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-APYOM  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-IWTHH1 PICTURE  X.                       CI0100
            14            PJ41-CPAYCL PICTURE  X(2).                    CI0100
            14            PJ41-CTSAO  PICTURE  X.                       CI0100
            14            PJ41-NCONF  PICTURE  9(08).                   CI0100
            14            PJ41-CLID   PICTURE  X(23).                   CI0100
            14            PJ41-CARTY  PICTURE  99.                      CI0100
            14            PJ41-NARRS  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-CARTZ  PICTURE  99.                      CI0100
            14            PJ41-NAPDS  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-CPMTO  PICTURE  X.                       CI0100
            14            PJ41-DNPMT  PICTURE  9(8).                    CI0100
            14            PJ41-IPCTV  PICTURE  X.                       CI0100
            14            PJ41-IMECH  PICTURE  X(01).                   CI0100
            14            PJ41-IMVAO  PICTURE  X(1).                    CI0100
            14            PJ41-AMVA1  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-CACTS  PICTURE  X.                       CI0100
            14            PJ41-CTSPP  PICTURE  X(1).                    CI0100
            14            PJ41-CACT4  PICTURE  X(2).                    CI0100
            14            PJ41-IVAEX  PICTURE  X.                       CI0100
            14            PJ41-DFPMT  PICTURE  9(8).                    CI0100
            14            PJ41-IDEMD  PICTURE  X.                       CI0100
            14            PJ41-IOFST  PICTURE  X.                       CI0100
            14            PJ41-AMXLB  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-ACULB  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-DEIRNB PICTURE  9(8).                    CI0100
            14            PJ41-DEFFE  PICTURE  9(8).                    CI0100
            14            PJ41-DEFFR  PICTURE  9(8).                    CI0100
            14            PJ41-ISPUP  PICTURE  X.                       CI0100
            14            PJ41-CPNCG  PICTURE  X.                       CI0100
            14            PJ41-IEXPU  PICTURE  X.                       CI0100
            14            PJ41-IPPCF  PICTURE  X.                       CI0100
            14            PJ41-NAAPT  PICTURE  9(2).                    CI0100
            14            PJ41-PWHLDS PICTURE  S999V9(5)                CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-ISWHO  PICTURE  X(1).                    CI0100
            13            PJ41-GD33                                     CI0100
                          REDEFINES            PJ41-GD31.               CI0100
            14            PJ41-CPAYC  PICTURE  X(2).                    CI0100
            14            PJ41-ADBRQX PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-ADBRQV PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-APTXR  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-CTRTPE PICTURE  X(2).                    CI0100
            14            PJ41-NCLAMI PICTURE  S9(9)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-CLIDO8 PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-CLIDN  PICTURE  X(20).                   CI0100
            14            PJ41-DSET01 PICTURE  S9(8)                    CI0100
                          BINARY.                                       CI0100
            14            PJ41-CTSET1 PICTURE  S9(6)                    CI0100
                          BINARY.                                       CI0100
            14            PJ41-DSET02 PICTURE  S9(8)                    CI0100
                          BINARY.                                       CI0100
            14            PJ41-CTSET2 PICTURE  S9(6)                    CI0100
                          BINARY.                                       CI0100
            13            PJ41-GD34                                     CI0100
                          REDEFINES            PJ41-GD31.               CI0100
            14            PJ41-QNOFM  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-CLTRM  PICTURE  99.                      CI0100
            14            PJ41-AMXLN  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-ALADJ  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-ACHK   PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-APRMO  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-IMECH1 PICTURE  X(01).                   CI0100
            14            PJ41-CACT41 PICTURE  X(2).                    CI0100
            14            PJ41-ACDSCC PICTURE  S9(05)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-ACDSCD PICTURE  S9(05)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-GD39                                     CI0100
                          REDEFINES            PJ41-GD31.               CI0100
            14            PJ41-GD31.                                    CI0100
            15            PJ41-FILLER PICTURE  X(245).                  CI0100
            12            PJ41-GD40                                     CI0100
                          REDEFINES            PJ41-GD99.               CI0100
            13            PJ41-NTR    PICTURE  9(8).                    CI0100
            13            PJ41-NPBNC  PICTURE  X(24).                   CI0100
            13            PJ41-CRREV  PICTURE  X(3).                    CI0100
            13            PJ41-CSUSL  PICTURE  S9.                      CI0100
            13            PJ41-NMGLN1 PICTURE  9(4).                    CI0100
            13            PJ41-DCAC92 PICTURE  9(8).                    CI0100
            13            PJ41-NAASQ3 PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-GD49.                                    CI0100
            14            PJ41-FILLER PICTURE  X(198).                  CI0100
            13            PJ41-GD41                                     CI0100
                          REDEFINES            PJ41-GD49.               CI0100
            14            PJ41-CRREF  PICTURE  9(2).                    CI0100
            14            PJ41-CORIR  PICTURE  X(02).                   CI0100
            14            PJ41-CIPDB  PICTURE  X(03).                   CI0100
            14            PJ41-CPAYH  PICTURE  X(02).                   CI0100
            14            PJ41-NAMEX  PICTURE  9(15)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-DCHAE  PICTURE  9(4).                    CI0100
            14            PJ41-DRQST  PICTURE  S9(8)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-GD42                                     CI0100
                          REDEFINES            PJ41-GD49.               CI0100
            14            PJ41-CPMTCB PICTURE  X(3).                    CI0100
            12            PJ41-GD50                                     CI0100
                          REDEFINES            PJ41-GD99.               CI0100
            13            PJ41-ALOAD  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-PSLLD4 PICTURE  S99V999                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CSUSL1 PICTURE  S9.                      CI0100
            13            PJ41-CRREV1 PICTURE  X(3).                    CI0100
            13            PJ41-ADDAC  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-DL13.                                    CI0100
            14            PJ41-GEYR   PICTURE  9(4).                    CI0100
            14            PJ41-GEMTH  PICTURE  99.                      CI0100
            14            PJ41-NDAY   PICTURE  99.                      CI0100
            13            PJ41-NSEQ3P PICTURE  S9(5)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-XZ6A   PICTURE  X(6).                    CI0100
            13            PJ41-XZ7    PICTURE  X(7).                    CI0100
            13            PJ41-XZ6B   PICTURE  X(6).                    CI0100
            13            PJ41-XZ6    PICTURE  X(6).                    CI0100
            13            PJ41-XZ6C   PICTURE  X(6).                    CI0100
            13            PJ41-XZ20   PICTURE  X(20).                   CI0100
            13            PJ41-CATRN1 PICTURE  9(6).                    CI0100
            13            PJ41-ADDAC2 PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-ATAXT2 PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-ACOMOT PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-XZ5    PICTURE  X(5).                    CI0100
            13            PJ41-IREVD  PICTURE  X(1).                    CI0100
            13            PJ41-ISUSP1 PICTURE  X(1).                    CI0100
            13            PJ41-XZ6D   PICTURE  X(6).                    CI0100
            13            PJ41-XZ13   PICTURE  X(13).                   CI0100
            13            PJ41-CWHTP2 PICTURE  X(3).                    CI0100
            13            PJ41-CWHTP3 PICTURE  X(3).                    CI0100
            13            PJ41-DTREN  PICTURE  9(8).                    CI0100
            13            PJ41-NAASQ1 PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            PJ41-GD51                                     CI0100
                          REDEFINES            PJ41-GD99.               CI0100
            13            PJ41-ADOMOT PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-ACGLT  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-ACGST  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CTXMT  PICTURE  9(2).                    CI0100
            13            PJ41-ALOAD3 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-FILLER PICTURE  X(31).                   CI0100
            12            PJ41-GD52                                     CI0100
                          REDEFINES            PJ41-GD99.               CI0100
            13            PJ41-DEFFT5 PICTURE  9(8).                    CI0100
            13            PJ41-PSLLD5 PICTURE  S99V999                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CSUSL2 PICTURE  S9.                      CI0100
            13            PJ41-ALOAD2 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-DL22.                                    CI0100
            14            PJ41-NYEAR1 PICTURE  9(4).                    CI0100
            14            PJ41-GEMTHA PICTURE  99.                      CI0100
            14            PJ41-NDAY01 PICTURE  99.                      CI0100
            13            PJ41-NSEQ3R PICTURE  S9(5)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CWHTP  PICTURE  X(3).                    CI0100
            13            PJ41-CWHFR  PICTURE  X(3).                    CI0100
            13            PJ41-CATRN7 PICTURE  9(6).                    CI0100
            13            PJ41-ATAXT5 PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-QSHOT  PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-ACINT3 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CWHTP1 PICTURE  X(3).                    CI0100
            13            PJ41-CWHFR1 PICTURE  X(3).                    CI0100
            13            PJ41-ACOMO5 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-QSHOMU PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-ACASH1 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-FILLER PICTURE  X(04).                   CI0100
            13            PJ41-CATRN8 PICTURE  9(6).                    CI0100
            13            PJ41-ALOAD1 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-PSLLD1 PICTURE  S99V999                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-QSHOT1 PICTURE  S9(10)V999               CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-ACINT4 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CSUSL4 PICTURE  S9.                      CI0100
            13            PJ41-ACOMO4 PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            PJ41-GD60                                     CI0100
                          REDEFINES            PJ41-GD99.               CI0100
            13            PJ41-GEOPDD PICTURE  X(8)                     CI0100
                          OCCURS       005     TIMES.                   CI0100
            13            PJ41-DLAUP3 PICTURE  9(8)                     CI0100
                          OCCURS       005     TIMES.                   CI0100
            13            PJ41-GEOPDB PICTURE  X(8).                    CI0100
            13            PJ41-DLAUP4 PICTURE  9(8).                    CI0100
            13            PJ41-ITELR2 PICTURE  X.                       CI0100
            13            PJ41-IPMTA  PICTURE  X.                       CI0100
            13            PJ41-CCSMG  PICTURE  X.                       CI0100
            13            PJ41-CPLEC  PICTURE  XX.                      CI0100
            13            PJ41-CORTYA PICTURE  X(3).                    CI0100
            13            PJ41-CACTBC PICTURE  X(1).                    CI0100
            13            PJ41-CGSPIA PICTURE  X.                       CI0100
            13            PJ41-IPTRDA PICTURE  X(01).                   CI0100
            13            PJ41-GCUSPY PICTURE  X(12).                   CI0100
            13            PJ41-CPALLA PICTURE  X(1).                    CI0100
            13            PJ41-QSHO5A PICTURE  S9(9)V999                CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-IFRSAB PICTURE  X.                       CI0100
            13            PJ41-DELOI  PICTURE  9(8).                    CI0100
            13            PJ41-IAROAA PICTURE  X.                       CI0100
            13            PJ41-ACINVR PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-ILTINA PICTURE  X.                       CI0100
            13            PJ41-ALOIDA PICTURE  S9(11)V99                CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CFUNTA PICTURE  X(2).                    CI0100
            13            PJ41-CLGND  PICTURE  X.                       CI0100
            13            PJ41-CPH3U  PICTURE  X.                       CI0100
            13            PJ41-GESTD  PICTURE  9(8).                    CI0100
            13            PJ41-GEEND  PICTURE  9(8).                    CI0100
            13            PJ41-CPMTF  PICTURE  99.                      CI0100
            13            PJ41-CNAVR  PICTURE  X(1).                    CI0100
            12            PJ41-GD70                                     CI0100
                          REDEFINES            PJ41-GD99.               CI0100
            13            PJ41-CMEMO  PICTURE  X(2).                    CI0100
            13            PJ41-ALPLDT PICTURE  9(8).                    CI0100
            13            PJ41-CTLPD  PICTURE  9(8).                    CI0100
            13            PJ41-CPAYCM PICTURE  X(2).                    CI0100
            11            PJ41-GC06.                                    CI0100
            12            PJ41-GELL   PICTURE  9(4)                     CI0100
                          BINARY.                                       CI0100
            12            PJ41-GE00.                                    CI0100
            13            PJ41-GC06K.                                   CI0100
            14            PJ41-NPISQ  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-ACOTD  PICTURE  S9(9)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-PPOTD  PICTURE  S9(3)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-QPSTD  PICTURE  S9(7)V999                CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CPITC  PICTURE  99.                      CI0100
            13            PJ41-ITRNB  PICTURE  X.                       CI0100
            13            PJ41-FILLER PICTURE  X(14).                   CI0100
            12            PJ41-GE98.                                    CI0100
            13            PJ41-FILLER PICTURE  X(240).                  CI0100
            12            PJ41-GE10                                     CI0100
                          REDEFINES            PJ41-GE98.               CI0100
            13            PJ41-CDELI  PICTURE  9(3).                    CI0100
            13            PJ41-CPAYC  PICTURE  X(2).                    CI0100
            13            PJ41-ICHKP  PICTURE  X.                       CI0100
            13            PJ41-CLTIN  PICTURE  9(12).                   CI0100
            13            PJ41-IFHAI  PICTURE  X.                       CI0100
            13            PJ41-CDQUA  PICTURE  X(2).                    CI0100
            13            PJ41-FILLER PICTURE  X(07).                   CI0100
            13            PJ41-GE99.                                    CI0100
            14            PJ41-FILLER PICTURE  X(212).                  CI0100
            13            PJ41-GE01                                     CI0100
                          REDEFINES            PJ41-GE99.               CI0100
            14            PJ41-NTR    PICTURE  9(8).                    CI0100
            14            PJ41-GECKD  PICTURE  9.                       CI0100
            14            PJ41-NPBN   PICTURE  X(20).                   CI0100
            14            PJ41-CCBAT  PICTURE  99.                      CI0100
            14            PJ41-CLID4  PICTURE  X(23).                   CI0100
            14            PJ41-GENAL1 PICTURE  X(30)                    CI0100
                          OCCURS       002     TIMES.                   CI0100
            14            PJ41-GESAD1 PICTURE  X(30)                    CI0100
                          OCCURS       003     TIMES.                   CI0100
            13            PJ41-GE02                                     CI0100
                          REDEFINES            PJ41-GE99.               CI0100
            14            PJ41-GENAL  PICTURE  X(30)                    CI0100
                          OCCURS       002     TIMES.                   CI0100
            14            PJ41-GESAD  PICTURE  X(30)                    CI0100
                          OCCURS       003     TIMES.                   CI0100
            13            PJ41-GE03                                     CI0100
                          REDEFINES            PJ41-GE99.               CI0100
            14            PJ41-NCHKN  PICTURE  9(11).                   CI0100
            13            PJ41-GE04                                     CI0100
                          REDEFINES            PJ41-GE99.               CI0100
            14            PJ41-CTIDAP PICTURE  9(3).                    CI0100
            14            PJ41-PRCOD  PICTURE  9(5).                    CI0100
            14            PJ41-TDELI  PICTURE  X(30).                   CI0100
            14            PJ41-CINCD  PICTURE  9(02).                   CI0100
            12            PJ41-GE20                                     CI0100
                          REDEFINES            PJ41-GE98.               CI0100
            13            PJ41-C299.                                    CI0100
            14            PJ41-CTID.                                    CI0100
            15            PJ41-CTIDA  PICTURE  9(3).                    CI0100
            15            PJ41-CTIDN.                                   CI0100
            16            PJ41-CTIDNP PICTURE  X(13).                   CI0100
            16            PJ41-CTIDND PICTURE  9(11).                   CI0100
            13            PJ41-DCACG9 PICTURE  9(8).                    CI0100
            13            PJ41-NAASQ  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            13            PJ41-CIRAP  PICTURE  XX.                      CI0100
            13            PJ41-CTYPE  PICTURE  X.                       CI0100
            13            PJ41-INACT  PICTURE  X.                       CI0100
            13            PJ41-FILLER PICTURE  X(01).                   CI0100
            13            PJ41-ITPAC  PICTURE  X.                       CI0100
            13            PJ41-ITAXI  PICTURE  X.                       CI0100
            13            PJ41-IOWNC  PICTURE  X.                       CI0100
            13            PJ41-CDVCD  PICTURE  X(2).                    CI0100
            13            PJ41-CTCUS  PICTURE  999.                     CI0100
            13            PJ41-CPMTCB PICTURE  X(3).                    CI0100
            13            PJ41-CASTC1 PICTURE  99.                      CI0100
            13            PJ41-PRCOD1 PICTURE  9(5).                    CI0100
            13            PJ41-CPRSC1 PICTURE  X(9).                    CI0100
            13            PJ41-CPRTB  PICTURE  X.                       CI0100
            13            PJ41-CBRKD  PICTURE  9(4).                    CI0100
            13            PJ41-FILLER PICTURE  X(12).                   CI0100
            12            PJ41-GE30                                     CI0100
                          REDEFINES            PJ41-GE98.               CI0100
            13            PJ41-CFIDC  PICTURE  X(5).                    CI0100
            13            PJ41-CPHSE  PICTURE  9(2).                    CI0100
            13            PJ41-FILLER PICTURE  X(05).                   CI0100
            13            PJ41-IABIN  PICTURE  X.                       CI0100
            13            PJ41-PDFND  PICTURE  S999V9(3)                CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            PJ41-GE40                                     CI0100
                          REDEFINES            PJ41-GE98.               CI0100
            13            PJ41-CACCT  PICTURE  X.                       CI0100
            13            PJ41-CPAYR  PICTURE  X(2).                    CI0100
            13            PJ41-CDELI1 PICTURE  9(3).                    CI0100
            13            PJ41-CATRN.                                   CI0100
            14            PJ41-CATRF  PICTURE  9(3).                    CI0100
            14            PJ41-CATRS  PICTURE  9(3).                    CI0100
            13            PJ41-DEFFT  PICTURE  9(8).                    CI0100
            13            PJ41-CTYPC  PICTURE  X.                       CI0100
            13            PJ41-CIRAPA PICTURE  XX.                      CI0100
            13            PJ41-FILLER PICTURE  X(09).                   CI0100
            13            PJ41-GE49.                                    CI0100
            14            PJ41-FILLER PICTURE  X(208).                  CI0100
            13            PJ41-GE41                                     CI0100
                          REDEFINES            PJ41-GE49.               CI0100
            14            PJ41-NCHKN1 PICTURE  9(6).                    CI0100
            13            PJ41-GE42                                     CI0100
                          REDEFINES            PJ41-GE49.               CI0100
            14            PJ41-CTID1.                                   CI0100
            15            PJ41-CTIDA1 PICTURE  9(3).                    CI0100
            15            PJ41-CTIDP1 PICTURE  X(13).                   CI0100
            15            PJ41-CTIDN1 PICTURE  9(11).                   CI0100
            13            PJ41-GE43                                     CI0100
                          REDEFINES            PJ41-GE49.               CI0100
            14            PJ41-GENAL2 PICTURE  X(30)                    CI0100
                          OCCURS       002     TIMES.                   CI0100
            14            PJ41-GESAD2 PICTURE  X(30)                    CI0100
                          OCCURS       003     TIMES.                   CI0100
            13            PJ41-GE44                                     CI0100
                          REDEFINES            PJ41-GE49.               CI0100
            14            PJ41-CTID01.                                  CI0100
            15            PJ41-CTIDA6 PICTURE  9(3).                    CI0100
            15            PJ41-NTIDP2 PICTURE  X(13).                   CI0100
            15            PJ41-CTIDN2 PICTURE  9(11).                   CI0100
            14            PJ41-GECKD2 PICTURE  9.                       CI0100
            14            PJ41-PACCT  PICTURE  S999V99                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-PLOAN  PICTURE  S999V99                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-PADPT  PICTURE  S999V99                  CI0100
                          COMPUTATIONAL-3.                              CI0100
            14            PJ41-IPCTL  PICTURE  X.                       CI0100
            14            PJ41-IPCTP  PICTURE  X.                       CI0100
            14            PJ41-CEUNT  PICTURE  S9(5)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            PJ41-GE31                                     CI0100
                          REDEFINES            PJ41-GE98.               CI0100
            13            PJ41-GCUSPZ PICTURE  X(12).                   CI0100
            11            PJ41-GC12                                     CI0100
                          REDEFINES            PJ41-GC06.               CI0100
            12            PJ41-GC12K.                                   CI0100
            13            PJ41-CIRAP  PICTURE  XX.                      CI0100
            12            PJ41-AIRCT  PICTURE  S9(7)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            PJ41-FILLER PICTURE  X.                       CI0100
            11            PJ41-GC04.                                    CI0100
            12            PJ41-CLCUS  PICTURE  99.                      CI0100
            12            PJ41-CCACT  PICTURE  99.                      CI0100
            12            PJ41-AFEET  PICTURE  S9(5)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            PJ41-ITERF  PICTURE  X.                       CI0100
            12            PJ41-ATERF  PICTURE  S9(5)V99                 CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            PJ41-CLDOB  PICTURE  9(8).                    CI0100
            12            PJ41-CPLTYP PICTURE  X(14).                   CI0100
            12            PJ41-IACFPD PICTURE  X(1).                    CI0100
            12            PJ41-FILLER PICTURE  X(14).                   CI0100
            11            PJ41-GC21                                     CI0100
                          REDEFINES            PJ41-GC04.               CI0100
            12            PJ41-C299.                                    CI0100
            13            PJ41-CTID.                                    CI0100
            14            PJ41-CTIDA  PICTURE  9(3).                    CI0100
            14            PJ41-CTIDN.                                   CI0100
            15            PJ41-CTIDNP PICTURE  X(13).                   CI0100
            15            PJ41-CTIDND PICTURE  9(11).                   CI0100
            12            PJ41-DCACG9 PICTURE  9(8).                    CI0100
            12            PJ41-NAASQ  PICTURE  S9(3)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            12            PJ41-FILLER PICTURE  X.                       CI0100
      *!WF DSP=PJ DSL=PJ SEL=41 FOR=I DES=1 LEV=1 PLT=75
      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0100
          05              DE00-SUITE.                                   CI0100
            15       FILLER         PICTURE  X(00653).                  CI0100
       01                 DE10  REDEFINES      DE00.                    CI0100
            10            DE10-DU11.                                    CI0100
            11            DE10-XFONC  PICTURE  X(4).                    CI0100
            11            DE10-MPSBN  PICTURE  X(8).                    CI0100
            11            DE10-XDBDNM PICTURE  X(08).                   CI0100
            11            DE10-XSEGNM PICTURE  X(08).                   CI0100
            11            DE10-XRC    PICTURE  X(02).                   CI0100
            11            DE10-MSEG   PICTURE  X(08).                   CI0100
            11            DE10-XCOKEY PICTURE  X(70).                   CI0100
            11            DE10-CUIBR  PICTURE  X(01).                   CI0100
            11            DE10-CUIBA  PICTURE  X(01).                   CI0100
            11            DE10-IPBIK  PICTURE  X(1).                    CI0100
            10            DE10-DU03.                                    CI0100
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            DE10-CMSSF  PICTURE  XX.                      CI0100
            11            DE10-DU09.                                    CI0100
            12            DE10-CMESA  PICTURE  S9(9)                    CI0100
                          BINARY.                                       CI0100
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0100
                          BINARY.                                       CI0100
            12            DE10-CMESB  PICTURE  S9(9)                    CI0100
                          BINARY.                                       CI0100
            12            DE10-CMSST  PICTURE  S9(9)                    CI0100
                          BINARY.                                       CI0100
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0100
                          BINARY.                                       CI0100
            12            DE10-QELLAA PICTURE  S9(9)                    CI0100
                          BINARY.                                       CI0100
            12            DE10-TMESS4 PICTURE  X(512).                  CI0100
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0100
          05              MS00-SUITE.                                   CI0100
            15       FILLER         PICTURE  X(00542).                  CI0100
       01                 MS03  REDEFINES      MS00.                    CI0100
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            10            MS03-CMSSF  PICTURE  XX.                      CI0100
            10            MS03-DU09.                                    CI0100
            11            MS03-CMESA  PICTURE  S9(9)                    CI0100
                          BINARY.                                       CI0100
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0100
                          BINARY.                                       CI0100
            11            MS03-CMESB  PICTURE  S9(9)                    CI0100
                          BINARY.                                       CI0100
            11            MS03-CMSST  PICTURE  S9(9)                    CI0100
                          BINARY.                                       CI0100
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0100
                          BINARY.                                       CI0100
            11            MS03-QELLAA PICTURE  S9(9)                    CI0100
                          BINARY.                                       CI0100
            11            MS03-TMESS4 PICTURE  X(512).                  CI0100
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0100
            10            MX11-QMSGS  PICTURE  9(03).                   CI0100
            10            MX11-PJ09                                     CI0100
                          OCCURS       025     TIMES.                   CI0100
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0100
                          COMPUTATIONAL-3.                              CI0100
            11            MX11-CMESB  PICTURE  S9(9)                    CI0100
                          BINARY.                                       CI0100
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PJ40
                                PJ41
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0100
      *               *                                   *             CI0100
      *               *INITIALISATIONS                    *             CI0100
      *               *                                   *             CI0100
      *               *************************************.            CI0100
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
      *N02DA.    NOTE *INITIALIZE OUTPUT LINKAGE          *.
       F02DA.                                                           lv10
           INITIALIZE  PJ41
           MOVE        'N' TO PJ41-IGC01
           MOVE        'Y' TO PJ41-IENDP
           MOVE        ZERO TO IPJ41L.
       F02DA-FN. EXIT.
      *N02DD.    NOTE *SET THE REST TO NO ALSO            *.
       F02DD.                                                           lv10
           MOVE        1                        TO J02DDR
                                    GO TO     F02DD-B.
       F02DD-A.
           ADD         1                        TO J02DDR.
       F02DD-B.
           IF          J02DDR                   >  10
                                    GO TO     F02DD-FN.
           MOVE        'N' TO PJ41-IGC03 (J02DDR)
           MOVE        'N' TO PJ41-IGC04 (J02DDR)
           MOVE        'N' TO PJ41-IGC06 (J02DDR)
           MOVE        'N' TO PJ41-IGC12 (J02DDR)
           MOVE        'N' TO PJ41-IGC21 (J02DDR).
       F02DD-900. GO TO F02DD-A.
       F02DD-FN. EXIT.
      *N02XA.    NOTE *SET POINTERS FOR DB ACCESS         *.
       F02XA.                                                           lv10
      *SET ADDRESS FOR ACAP                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-ACAP-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0100
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0100
      *               *                                   *             CI0100
      *               *FIN DE TRAITEMENT                  *             CI0100
      *               *                                   *             CI0100
      *               *************************************.            CI0100
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0100
      *N20BA.    NOTE *SET THE ARRAY ENTRIES COUNT        *.
       F20BA.                                                           lv10
           MOVE        IPJ41L TO PJ41-QDECT9.
       F20BA-FN. EXIT.
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATE INPUT PARMS               *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *
      *********************************
      **  ENSURE PARMS HAVE THE       *
      **  CORRECT CONTENTS BASED ON   *
      **  FIELD CLASS AND CONTENTS    *
      *********************************
      *N40CA.    NOTE *VALIDATE SYSTEM RECOGNIZED         *.
       F40CA.    IF    PJ40-MAPPN NOT = 'UD'                            lv10
                 AND   PJ40-IPERT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40CA-FN.
      *---> Send BAD SOURCE Message                                     ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012734 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40CA-FN. EXIT.
      *N40DA.    NOTE *IS FUNCTION VALID?                 *.
       F40DA.    IF    PJ40-CFUNC NOT = 'GU'                            lv10
                 AND   PJ40-CFUNC NOT = 'GNP'
                 NEXT SENTENCE ELSE GO TO     F40DA-FN.
      *---> Send BAD FUNCTION Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012595 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40DA-FN. EXIT.
      *N40EA.    NOTE *IF GNP FUNC; WHAT STATUS/TYPE      *.
       F40EA.    IF    PJ40-CFUNC = 'GNP'                               lv10
                 NEXT SENTENCE ELSE GO TO     F40EA-FN.
      *N40ED.    NOTE *IS TYPE OF ACTIVITY VALID          *.
       F40ED.    IF    (PJ40-CAATY (001)                                lv15
                       NOT NUMERIC
                 OR    PJ40-CAATY (002)
                       NOT NUMERIC
                 OR    PJ40-CAATY (003)
                       NOT NUMERIC)
                 OR    PJ40-CAATY (001) > 003
                 OR    PJ40-CAATY (002) > 003
                 OR    PJ40-CAATY (003) > 003
                 NEXT SENTENCE ELSE GO TO     F40ED-FN.
      *---> Send BAD TYPE Message                                       ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012367 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40ED-FN. EXIT.
      *N40EI.    NOTE *IS STATUS OF ACTIVITY VALID        *.
       F40EI.    IF    (PJ40-CASTC (001)                                lv15
                       NOT NUMERIC
                 OR    PJ40-CASTC (002)
                       NOT NUMERIC
                 OR    PJ40-CASTC (003)
                       NOT NUMERIC
                 OR    PJ40-CASTC (004)
                       NOT NUMERIC
                 OR    PJ40-CASTC (005)
                       NOT NUMERIC
                 OR    PJ40-CASTC (006)
                       NOT NUMERIC)
                 OR    PJ40-CASTC (001) > 06
                 OR    PJ40-CASTC (002) > 06
                 OR    PJ40-CASTC (003) > 06
                 OR    PJ40-CASTC (004) > 06
                 OR    PJ40-CASTC (005) > 06
                 OR    PJ40-CASTC (006) > 06
                 NEXT SENTENCE ELSE GO TO     F40EI-FN.
      *---> Send BAD STATUS Message                                     ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012070 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40EI-FN. EXIT.
       F40EA-900. GO TO F40ET-FN.
       F40EA-FN. EXIT.
      *N40ET.    NOTE *ELSE.. TYPE DOES NOT APPLY         *.
       F40ET.                                                           lv10
           MOVE        ZERO TO PJ40-CAATY (1)
           PJ40-CAATY (2)
           PJ40-CAATY (3)
           MOVE        ZERO TO PJ40-CASTC (1)
           PJ40-CASTC (2)
           PJ40-CASTC (3)
           PJ40-CASTC (4)
           PJ40-CASTC (5)
           PJ40-CASTC (6).
       F40ET-FN. EXIT.
      *N40FA.    NOTE *ENSURE NUMERIC KEYS ARE VALID      *.
       F40FA.         EXIT.                                             lv10
      *N40FC.    NOTE *IS DATE VALID                      *.
       F40FC.    IF    PJ40-DCACG9 NUMERIC                              lv15
                 NEXT SENTENCE ELSE GO TO     F40FC-FN.
       F40FC-900. GO TO F40FG-FN.
       F40FC-FN. EXIT.
      *N40FG.    NOTE *ELSE... INVALID DATE               *.
       F40FG.                                                           lv15
      *---> Send BAD DATE Message                                       ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012786 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40FG-FN. EXIT.
      *N40FL.    NOTE *IS SEQUENCE KEY FOR GC03 VALID     *.
       F40FL.    IF    PJ40-NAASQ NUMERIC                               lv15
                 NEXT SENTENCE ELSE GO TO     F40FL-FN.
       F40FL-900. GO TO F40FN-FN.
       F40FL-FN. EXIT.
      *N40FN.    NOTE *ELSE... INVALID GC03 SEQUENCE      *.
       F40FN.                                                           lv15
      *---> Send BAD GC03 SEQ Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013216 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40FN-FN. EXIT.
      *N40FQ.    NOTE *IS SEQUENCE KEY FOR GC06 VALID     *.
       F40FQ.    IF    PJ40-NPISQ NUMERIC                               lv15
                 NEXT SENTENCE ELSE GO TO     F40FQ-FN.
       F40FQ-900. GO TO F40FT-FN.
       F40FQ-FN. EXIT.
      *N40FT.    NOTE *ELSE... INVALID GC06 SEQUENCE      *.
       F40FT.                                                           lv15
      *---> Send BAD GC06 SEQ Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013350 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40FT-FN. EXIT.
       F40FA-FN. EXIT.
      *N40LA.    NOTE *EDIT CONTINUATION KEYS             *.
       F40LA.    IF    PJ40-CFUNC = 'GNP'                               lv10
                 NEXT SENTENCE ELSE GO TO     F40LA-FN.
      *N40LC.    NOTE *IF LAST SEGMENT IS VALID           *.
       F40LC.    IF    PJ40-MIPPS = 'GC03'                              lv15
                 OR    PJ40-MIPPS = 'GC06'
                 NEXT SENTENCE ELSE GO TO     F40LC-FN.
       F40LC-900. GO TO F40LG-FN.
       F40LC-FN. EXIT.
      *N40LG.    NOTE *ELSE... INVALID SEGMENT TYPE       *.
       F40LG.                                                           lv15
           MOVE        SPACES TO PJ40-MIPPS.
       F40LG-FN. EXIT.
      *N40LL.    NOTE *IF END OF PROCESS IS VALID         *.
       F40LL.    IF    PJ40-IENDP = 'Y'                                 lv15
                 OR    PJ40-IENDP = 'N'
                 OR    PJ40-IENDP = ' '
                 NEXT SENTENCE ELSE GO TO     F40LL-FN.
       F40LL-900. GO TO F40LO-FN.
       F40LL-FN. EXIT.
      *N40LO.    NOTE *ELSE... SET END OF PROCSS IND      *.
       F40LO.                                                           lv15
      *---> Send BAD CONTINUATE Message                                 ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013361 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40LO-FN. EXIT.
       F40LA-900. GO TO F40LT-FN.
       F40LA-FN. EXIT.
      *N40LT.    NOTE *ELSE... SET DEFAULT TO FIELDS      *.
       F40LT.                                                           lv10
           MOVE        SPACES TO PJ40-MIPPS
           MOVE        'Y' TO PJ40-IENDP.
       F40LT-FN. EXIT.
      *N40MA.    NOTE *IF MODIFY; VALIDATE TASK/SEQ #     *.
       F40MA.    IF    PJ40-IPERT = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F40MA-FN.
      *N40MF.    NOTE *VALIDATE TASK/SEQ #                *.
       F40MF.    IF    PJ40-NEIBT NUMERIC                               lv15
                 AND   PJ40-NEIBT > ZERO
                 AND   PJ40-GESQ2C NUMERIC
                 NEXT SENTENCE ELSE GO TO     F40MF-FN.
       F40MF-900. GO TO F40MH-FN.
       F40MF-FN. EXIT.
      *N40MH.    NOTE *ELSE... ERROR                      *.
       F40MH.                                                           lv15
      *---> Send BAD PERT Message                                       ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012856 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40MH-FN. EXIT.
       F40MA-900. GO TO F40MK-FN.
       F40MA-FN. EXIT.
      *N40MK.    NOTE *ELSE... DEFAULT TASK & SEQ NBR     *.
       F40MK.                                                           lv10
           MOVE        ZERO TO PJ40-NEIBT
           PJ40-GESQ2C.
       F40MK-FN. EXIT.
       F40-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *ACCESS ROOT AND GU PROCESSING      *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *
      *********************************
      **                              *
      ** THIS PROCESSING WILL DO TWO  *
      ** FUNCTIONS:                   *
      **  - ACCESS THE GC01 AND STORE *
      **    IT FOR GU AND GNP         *
      **  - FOR GU PROCESSING IT WILL *
      **    ACCESS ALL THE RELATED    *
      **    ACTIVITIES AND PLACE THEM *
      **    IN THE 1ST OCCURRANCE OF  *
      **    THE ARRAY.                *
      **                              *
      *********************************
      *N50CA.    NOTE *ACCESS THE GC01 SEGMENT            *.
       F50CA.                                                           lv10
           MOVE        PJ40-CTID TO S-GCU01-GC01K
           PERFORM     F94G1 THRU F94G1-FN.
       F50CA-FN. EXIT.
      *N50DA.    NOTE *IF GC01 IS FOUND; STORE IT         *.
       F50DA.    IF    IK = '0'                                         lv10
                 NEXT SENTENCE ELSE GO TO     F50DA-FN.
           PERFORM     F95CA THRU F95CA-FN.
       F50DA-900. GO TO F50DD-FN.
       F50DA-FN. EXIT.
      *N50DD.    NOTE *ELSE... CONTRACT NOT FOUND         *.
       F50DD.                                                           lv10
           MOVE                     ALL '1' TO FT GO TO F20.
       F50DD-FN. EXIT.
      *N50GA.    NOTE *IF GU PROCESSING                   *.
       F50GA.    IF    PJ40-CFUNC = 'GU'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50GA-FN.
      *N50HA.    NOTE *READ GC03                          *.
       F50HA.                                                           lv15
           MOVE        PJ40-DCACG9 TO S-GCU03-DCACG9
           MOVE        PJ40-NAASQ TO S-GCU03-NAASQ
           PERFORM     F94G2 THRU F94G2-FN.
       F50HA-FN. EXIT.
      *N50HM.    NOTE *IF GC03 WAS FOUND                  *.
       F50HM.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50HM-FN.
           PERFORM     F95CA THRU F95CA-FN.
      *N50IA.    NOTE *IF ACTIVITY IS A DISB OR ADJ       *.
       F50IA.    IF    GC03-CAATY = 001                                 lv20
                 OR    GC03-CAATY = 003
                 NEXT SENTENCE ELSE GO TO     F50IA-FN.
      *N50ID.    NOTE *READ GC04                          *.
       F50ID.                                                           lv25
           PERFORM     F94G3 THRU F94G3-FN.
       F50ID-FN. EXIT.
      *N50IM.    NOTE *IF GC04 WAS FOUND; STORE IT        *.
       F50IM.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F50IM-FN.
           PERFORM     F95CA THRU F95CA-FN.
       F50IM-FN. EXIT.
      *N50JA.    NOTE *READ GC06                          *.
       F50JA.                                                           lv25
           MOVE        PJ40-NPISQ TO S-GCU06-NPISQ
           MOVE        '= ' TO S-GCU06-OPER
           PERFORM     F94G4 THRU F94G4-FN.
       F50JA-FN. EXIT.
      *N50JM.    NOTE *IF GC06 WAS FOUND                  *.
       F50JM.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F50JM-FN.
           PERFORM     F95CA THRU F95CA-FN.
       F50JM-FN. EXIT.
       F50IA-FN. EXIT.
      *N50KA.    NOTE *IF ACTIVITY IS A COLL OR ADJ       *.
       F50KA.    IF    GC03-CAATY = 002                                 lv20
                 OR    GC03-CAATY = 003
                 NEXT SENTENCE ELSE GO TO     F50KA-FN.
      *N50KD.    NOTE *READ GC12                          *.
       F50KD.                                                           lv25
           MOVE        PJ40-CIRAP TO S-GCU12-CIRAP
           PERFORM     F94G5 THRU F94G5-FN.
       F50KD-FN. EXIT.
      *N50KM.    NOTE *IF GC12 WAS FOUND; STORE IT        *.
       F50KM.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F50KM-FN.
           PERFORM     F95CA THRU F95CA-FN.
       F50KM-FN. EXIT.
      *N50LA.    NOTE *READ GC21                          *.
       F50LA.                                                           lv25
           PERFORM     F94G6 THRU F94G6-FN.
       F50LA-FN. EXIT.
      *N50LM.    NOTE *IF GC21 WAS FOUND; STORE IT        *.
       F50LM.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F50LM-FN.
           PERFORM     F95CA THRU F95CA-FN.
       F50LM-FN. EXIT.
       F50KA-FN. EXIT.
       F50HM-FN. EXIT.
       F50GA-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *GNP PROCESSING                     *
      *               *                                   *
      *               *************************************.
       F55.      IF    PJ40-CFUNC = 'GNP'                               lv05
                 NEXT SENTENCE ELSE GO TO     F55-FN.
      *
      *********************************
      **                              *
      ** THE GC01 WAS ACCESSED IN F50 *
      ** ABOVE; THIS PROCESS WILL 1ST *
      ** DETERMINE IF IT NEEDS TO     *
      ** RESTART; IF SO IT WILL START *
      ** AT THE PROPER PLACE OTHERWISE*
      ** IT WILL START WITH THE FIRST *
      ** SEGMENT UNDER THE GC01       *
      ** THEN.....                    *
      ** THIS PROCESSING WILL LOOP    *
      ** THROUGH THE ACTIVITY DATABASE*
      ** AND DETERMINE IF THE         *
      ** ACTIVITY SHOULD BE LOADED TO *
      ** THE ARRAY OR NOT UNTIL IT    *
      ** FILLS THE ARRAY OR RUNS OUT  *
      ** OF SEGMENTS UNDER THE GC01   *
      **                              *
      ** THIS PROCESSING WILL DO      *
      ** GNP PROCESSING WITH NO SSAS  *
      ** TO ACCESS ALL SEGMENTS UNDER *
      ** THE GC01                     *
      *********************************
      *N55BA.    NOTE *INIT END OF PROCESS INDICATOR      *.
       F55BA.                                                           lv10
      *
      *"Y" - INDICATES NO MORE RECORDS
      *
           MOVE        'Y' TO PJ41-IENDP.
       F55BA-FN. EXIT.
      *N55DA.    NOTE *IF CONTINUATION PROCESSING         *.
       F55DA.    IF    PJ40-IENDP = 'N'                                 lv10
                 AND   PJ40-MIPPS > SPACES
                 NEXT SENTENCE ELSE GO TO     F55DA-FN.
           MOVE        'N' TO W-RESTART-GOOD.
      *N55FA.    NOTE *IF LAST SEGMENT WAS A GC06         *.
       F55FA.    IF    PJ40-MIPPS = 'GC06'                              lv15
                 NEXT SENTENCE ELSE GO TO     F55FA-FN.
      *
      *GET NEXT GC03; POSITION ON GC01
      *
           MOVE        'P----' TO S-GCU01-CCOD
           MOVE        PJ40-DCACG9 TO S-GCU03-DCACG9
           MOVE        PJ40-NAASQ TO S-GCU03-NAASQ
           MOVE        '= ' TO S-GCU03-OPER
           PERFORM     F94G2 THRU F94G2-FN.
      *N55FC.    NOTE *IF GC03 FND; STORE IT; GET GC04    *.
       F55FC.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F55FC-FN.
      *
           PERFORM     F95CA THRU F95CA-FN
      *
      *GET CURRENT GC04
      *
           PERFORM     F94G3 THRU F94G3-FN.
      *N55FF.    NOTE *IF GC04 FOUND; STORE IT            *.
       F55FF.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F55FF-FN.
           PERFORM     F95CA THRU F95CA-FN.
       F55FF-FN. EXIT.
      *N55FJ.    NOTE *GET NEXT GC06; POSITION ON GC01    *.
       F55FJ.                                                           lv25
           MOVE        PJ40-NPISQ TO S-GCU06-NPISQ
           MOVE        '> ' TO S-GCU06-OPER
           PERFORM     F94G4 THRU F94G4-FN.
       F55FJ-FN. EXIT.
      *N55FM.    NOTE *IF FOUND; CONTINUE; STORE BELOW    *.
       F55FM.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F55FM-FN.
      *
      *RECORD WILL BE STORED AS PART
      *OF DO WHILE LOOP BELOW
      *
           MOVE        'Y' TO W-RESTART-GOOD
           MOVE        '= ' TO S-GCU06-OPER.
       F55FM-FN. EXIT.
       F55FC-FN. EXIT.
      *N55GA.    NOTE *IF RESTART FAILED; RESET 1ST ROW   *.
       F55GA.    IF    W-RESTART-GOOD = 'N'                             lv20
                 NEXT SENTENCE ELSE GO TO     F55GA-FN.
           INITIALIZE  PJ41-GC03 (001)
           PJ41-GC04 (001)
           PJ41-GC06 (001)
           MOVE        'N' TO PJ41-IGC03 (001)
           PJ41-IGC04 (001)
           PJ41-IGC06 (001)
           MOVE        ZERO TO IPJ41L.
       F55GA-FN. EXIT.
       F55FA-FN. EXIT.
      *N55HA.    NOTE *IF NEXT GC03 SHOULD BE ACCESSED    *.
       F55HA.    IF    PJ40-MIPPS = 'GC03'                              lv15
                 OR    W-RESTART-GOOD = 'N'
                 NEXT SENTENCE ELSE GO TO     F55HA-FN.
      *--> OR GC06 WAS NOT FOUND ABOVE
      *
      *GET NEXT GC03; POSITION ON GC01
      *
           MOVE        'P----' TO S-GCU01-CCOD
           MOVE        PJ40-DCACG9 TO S-GCU03-DCACG9
           MOVE        PJ40-NAASQ TO S-GCU03-NAASQ
           MOVE        '> ' TO S-GCU03-OPER
           PERFORM     F94G2 THRU F94G2-FN.
      *N55HD.    NOTE *LOOP UNTIL MATCHING ACTIVITY       *.
       F55HD.    IF    IK = '0'                                         lv20
                 AND   W-RESTART-GOOD = 'N'
                 NEXT SENTENCE ELSE GO TO     F55HD-FN.
      *N55HF.    NOTE *IF ACTIVITY MATCHES; STOP LOOP     *.
       F55HF.    IF    (GC03-CAATY =                                    lv25
                       PJ40-CAATY (001)
                 OR    GC03-CAATY =
                       PJ40-CAATY (002)
                 OR    GC03-CAATY =
                       PJ40-CAATY (003))
                 AND   (GC03-CASTC =
                       PJ40-CASTC (001)
                 OR    GC03-CASTC =
                       PJ40-CASTC (002)
                 OR    GC03-CASTC =
                       PJ40-CASTC (003)
                 OR    GC03-CASTC =
                       PJ40-CASTC (004)
                 OR    GC03-CASTC =
                       PJ40-CASTC (005)
                 OR    GC03-CASTC =
                       PJ40-CASTC (006))
                 NEXT SENTENCE ELSE GO TO     F55HF-FN.
      *IF THE ACTIVITY TYPE MATCHES
      *ONE OF THE REQUESTED TYPES
      *AND THE STATUS IS ONE OF
      *THE REQUESTED STATUS VALUES,
      *THEN SELECT THE ACTIVITY
           MOVE        'Y' TO W-RESTART-GOOD.
       F55HF-900. GO TO F55HH-FN.
       F55HF-FN. EXIT.
      *N55HH.    NOTE *ELSE... GET NEXT ACTIVITY          *.
       F55HH.                                                           lv25
           PERFORM     F94N2 THRU F94N2-FN.
       F55HH-FN. EXIT.
       F55HD-900. GO TO F55HD.
       F55HD-FN. EXIT.
      *N55IA.    NOTE *IF GC03 FND; GO ON; STORE BELOW    *.
       F55IA.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F55IA-FN.
      *
      *RECORD WILL BE STORED AS PART OF
      *DO WHILE LOOP BELOW
      *
      *RESET SSA VALUES
      *
           MOVE        '-----' TO S-GCU01-CCOD
           MOVE        '= ' TO S-GCU03-OPER.
       F55IA-900. GO TO F55IG-FN.
       F55IA-FN. EXIT.
      *N55IG.    NOTE *ELSE... RESTART FAILED; EXIT       *.
       F55IG.                                                           lv20
           MOVE                     ALL '1' TO FT GO TO F20.
       F55IG-FN. EXIT.
       F55HA-FN. EXIT.
       F55DA-900. GO TO F55JA-FN.
       F55DA-FN. EXIT.
      *N55JA.    NOTE *ELSE; GET NEXT REC; STORE BELOW    *.
       F55JA.                                                           lv10
      *
      *RECORD WILL BE STORED AS PART OF
      *DO WHILE LOOP BELOW
      *
           PERFORM     F94N1 THRU F94N1-FN.
       F55JA-FN. EXIT.
      *N55LA.    NOTE *LOOP THRU SEGMENTS UNTIL END       *.
       F55LA.    IF    IK = '0'                                         lv10
                 AND   PJ41-IENDP = 'Y'
                 NEXT SENTENCE ELSE GO TO     F55LA-FN.
      *N55MA.    NOTE *IF GC03; DETERMINE IF OK TO USE    *.
       F55MA.    IF    XW05-XSEGNM = 'GC03    '                         lv15
                 NEXT SENTENCE ELSE GO TO     F55MA-FN.
      *N55MF.    NOTE *IF ACTIVITY MATCHES; STORE IT      *.
       F55MF.    IF    (GC03-CAATY =                                    lv20
                       PJ40-CAATY (001)
                 OR    GC03-CAATY =
                       PJ40-CAATY (002)
                 OR    GC03-CAATY =
                       PJ40-CAATY (003))
                 AND   (GC03-CASTC =
                       PJ40-CASTC (001)
                 OR    GC03-CASTC =
                       PJ40-CASTC (002)
                 OR    GC03-CASTC =
                       PJ40-CASTC (003)
                 OR    GC03-CASTC =
                       PJ40-CASTC (004)
                 OR    GC03-CASTC =
                       PJ40-CASTC (005)
                 OR    GC03-CASTC =
                       PJ40-CASTC (006))
                 NEXT SENTENCE ELSE GO TO     F55MF-FN.
      *IF THE ACTIVITY TYPE MATCHES
      *ONE OF THE REQUESTED TYPES
      *AND THE STATUS IS ONE OF
      *THE REQUESTED STATUS VALUES,
      *THEN SELECT THE ACTIVITY
           PERFORM     F95CA THRU F95CA-FN.
      *N55MH.    NOTE *GNP UNDER GC01                     *.
       F55MH.                                                           lv25
           PERFORM     F94N1 THRU F94N1-FN.
       F55MH-FN. EXIT.
       F55MF-900. GO TO F55MM-FN.
       F55MF-FN. EXIT.
      *N55MM.    NOTE *ELSE... GET NEXT GC03;             *.
       F55MM.                                                           lv20
      *   - POSITION ON GC01 THOUGH
           MOVE        'P----' TO S-GCU01-CCOD
           PERFORM     F94N2 THRU F94N2-FN
           MOVE        '-----' TO S-GCU01-CCOD.
       F55MM-FN. EXIT.
       F55MA-900. GO TO F55NA-FN.
       F55MA-FN. EXIT.
      *N55NA.    NOTE *ALL OTHERS; THEY ARE OK TO STORE   *.
       F55NA.                                                           lv15
           PERFORM     F95CA THRU F95CA-FN.
      *N55NH.    NOTE *GNP UNDER GC01                     *.
       F55NH.                                                           lv20
           PERFORM     F94N1 THRU F94N1-FN.
       F55NH-FN. EXIT.
       F55NA-FN. EXIT.
       F55LA-900. GO TO F55LA.
       F55LA-FN. EXIT.
       F55-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *PERTINENCE PROCESSING...           *
      *               *                                   *
      *               *************************************.
       F60.      IF    PJ40-IPERT = 'Y'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F60-FN.
      *
      *********************************
      ** THIS CODE WILL BE USED FOR   *
      ** DETERMINING IF DATA HAS BEEN *
      ** CHANGED SINCE IT WAS         *
      ** PREVIOUSLY RETRIEVED.        *
      *********************************
      *N60BA.    NOTE *TR UD PERTINENCE PROCESSING..      *.
       F60BA.    IF    PJ40-MAPPN = 'UD        '                        lv10
                 NEXT SENTENCE ELSE GO TO     F60BA-FN.
      *N60CB.    NOTE *BUILD MD07 COPY BOOK               *.
       F60CB.                                                           lv15
           INITIALIZE  MD07-PASS-AREA
           OC01
           OC03
           OC04
           OC06.
       F60CB-FN. EXIT.
      *N60DA.    NOTE *MOVE THE ROOTS VALUES              *.
       F60DA.    IF    PJ41-IGC01 = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F60DA-FN.
           MOVE        PJ41-GC01 TO OC01
           MOVE        OC01-NSEQ4B TO MD07-NSEQ4B.
       F60DA-FN. EXIT.
      *N60DT.    NOTE *LOAD KEYS FOR UPDATE INTERFACE     *.
       F60DT.                                                           lv15
           MOVE        MD07-TBL-SIZE TO UPD-COL-PASS
           MOVE        PJ40-NEIBT TO UPD-EIBTASKN
           MOVE        PJ40-GESQ2C TO UPD-GESQ2C
           MOVE        'MD07' TO UPD-EIBTRNID.
                 IF    EIBTRNID (1:1) = 'X'                             DOT
           MOVE        'XD07' TO UPD-EIBTRNID.
       F60DT-FN. EXIT.
       F60BA-FN. EXIT.
      *N60YA.    NOTE *PERFORM THE PERTINENCE CHECK       *.
       F60YA.                                                           lv10
           PERFORM     F98UC THRU F98UC-FN.
       F60YA-FN. EXIT.
      *N60YD.    NOTE *PERTINENCE CHECK FAILED            *.
       F60YD.    IF    UPD-RETURN-LEVEL = 'E'                           lv10
                 NEXT SENTENCE ELSE GO TO     F60YD-FN.
      *
      *---> Send PERT FAILED Message                                    ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012758 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F60YD-FN. EXIT.
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
      *               *DB CALLS                           *
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
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G2-FN. EXIT.
      *N94G3.    NOTE *CALL GN ON GC04                    *.            ADU026
       F94G3.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC04' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 GC04                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           S-GC04-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G3-FN. EXIT.
      *N94G4.    NOTE *CALL GN ON GC06                    *.            ADU026
       F94G4.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 GC06                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           S-GCU06-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G4-FN. EXIT.
      *N94G5.    NOTE *CALL GN ON GC12                    *.            ADU026
       F94G5.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 GC12                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           S-GCU12-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G5-FN. EXIT.
      *N94G6.    NOTE *CALL GN ON GC21                    *.            ADU026
       F94G6.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC21' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 GC21                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           S-GC21-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G6-FN. EXIT.
      *N94N1.    NOTE *CALL GNP ON GC03                   *.            ADU026
       F94N1.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGNP                        ADU026
           PA06 GC03                                                    ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGNP TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
                 IF    XW05-XRC = 'GA'                                  DOT
                 OR    XW05-XRC = 'GK'
      *ALLOWABLE "GOOD" RET CDE FOR GNP
           MOVE        '0' TO IK.
       F94N1-FN. EXIT.
      *N94N2.    NOTE *CALL GN ON GC03                    *.            ADU026
       F94N2.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 GC03                                                    ADU026
           S-GCU01-SSA S-GC03-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94N2-FN. EXIT.
       F94-FN.   EXIT.
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *UTILITY FUNCTIONS                  *
      *               *                                   *
      *               *************************************.
       F95.                                                             lv05
      *
      *********************************
      **  ENSURE PARMS HAVE THE       *
      **  CORRECT CONTENTS BASED ON   *
      **  FIELD CLASS AND CONTENTS    *
      *********************************
      *N95CA.    NOTE *UPDATE ARRAY ACCORDINGLY           *.
       F95CA.                                                           lv10
           MOVE        XW05-XSEGNM TO PJ41-MIPPS.
      *N95DA.    NOTE *DETERMINE SEGMENT                  *.
       F95DA.         EXIT.                                             lv15
      *N95EA.    NOTE *IF GC01 UPDATE OUTPUT AREA         *.
       F95EA.    IF    XW05-XSEGNM =                                    lv20
                       'GC01    '
                 NEXT SENTENCE ELSE GO TO     F95EA-FN.
           MOVE        GC01 TO PJ41-GC01
           MOVE        'Y' TO PJ41-IGC01.
       F95EA-900. GO TO F95DA-FN.
       F95EA-FN. EXIT.
      *N95FA.    NOTE *IF GC03; UPDATE INDEX              *.
       F95FA.    IF    XW05-XSEGNM =                                    lv20
                       'GC03    '
                 NEXT SENTENCE ELSE GO TO     F95FA-FN.
      *N95FC.    NOTE *IF ROOM EXIST IN ARRAY             *.
       F95FC.    IF    IPJ41L < 10                                      lv25
                 NEXT SENTENCE ELSE GO TO     F95FC-FN.
           ADD         +1 TO IPJ41L
           MOVE        GC03 TO PJ41-GC03 (IPJ41L)
           MOVE        'Y' TO PJ41-IGC03 (IPJ41L).
       F95FC-900. GO TO F95FM-FN.
       F95FC-FN. EXIT.
      *N95FM.    NOTE *ELSE... INDICATE ARRAY IS FULL     *.
       F95FM.                                                           lv25
           MOVE        'N' TO PJ41-IENDP.
       F95FM-FN. EXIT.
       F95FA-900. GO TO F95DA-FN.
       F95FA-FN. EXIT.
      *N95GA.    NOTE *IF GC04; UPDATE OUTPUT AREA        *.
       F95GA.    IF    XW05-XSEGNM =                                    lv20
                       'GC04    '
                 NEXT SENTENCE ELSE GO TO     F95GA-FN.
           MOVE        GC04 TO PJ41-GC04 (IPJ41L)
           MOVE        'Y' TO PJ41-IGC04 (IPJ41L).
       F95GA-900. GO TO F95DA-FN.
       F95GA-FN. EXIT.
      *N95HA.    NOTE *IF GC06; MOVE TO ARRAY             *.
       F95HA.    IF    XW05-XSEGNM =                                    lv20
                       'GC06    '
                 NEXT SENTENCE ELSE GO TO     F95HA-FN.
      *N95HC.    NOTE *IF GC06 ALREADY EXISTS             *.
       F95HC.    IF    PJ41-IGC06 (IPJ41L) = 'Y'                        lv25
                 NEXT SENTENCE ELSE GO TO     F95HC-FN.
      *  - NEED TO BUMP UP ARRAY
      *N95HG.    NOTE *IF ROOM EXISTS IN ARRAY            *.
       F95HG.    IF    IPJ41L < 10                                      lv30
                 NEXT SENTENCE ELSE GO TO     F95HG-FN.
      *- STORE THE PREVIOUS LOAD INDEX
           MOVE        IPJ41L TO IPJ41R
           ADD         +1 TO IPJ41L
      *
      *- MOVE RELATED ACTIVITY AND
      *  CUSTODIAL INFORMATION (IF ANY)
      *
           MOVE        PJ41-GC03 (IPJ41R) TO
           PJ41-GC03 (IPJ41L)
           MOVE        PJ41-IGC03 (IPJ41R) TO
           PJ41-IGC03 (IPJ41L)
           MOVE        PJ41-GC04 (IPJ41R) TO
           PJ41-GC04 (IPJ41L)
           MOVE        PJ41-IGC04 (IPJ41R) TO
           PJ41-IGC04 (IPJ41L)
      *
      *- MOVE CURRENT SEGMENT
      *
           MOVE        GC06 TO PJ41-GC06 (IPJ41L)
           MOVE        'Y' TO PJ41-IGC06 (IPJ41L).
       F95HG-900. GO TO F95HI-FN.
       F95HG-FN. EXIT.
      *N95HI.    NOTE *ELSE... ARRAY IS FULL              *.
       F95HI.                                                           lv30
           MOVE        'N' TO PJ41-IENDP.
       F95HI-FN. EXIT.
       F95HC-900. GO TO F95HM-FN.
       F95HC-FN. EXIT.
      *N95HM.    NOTE *ELSE... JUST UPDATE EXISTING       *.
       F95HM.                                                           lv25
           MOVE        GC06 TO PJ41-GC06 (IPJ41L)
           MOVE        'Y' TO PJ41-IGC06 (IPJ41L).
       F95HM-FN. EXIT.
       F95HA-900. GO TO F95DA-FN.
       F95HA-FN. EXIT.
      *N95IA.    NOTE *IF GC12; MOVE TO OUTPUT AREA       *.
       F95IA.    IF    XW05-XSEGNM =                                    lv20
                       'GC12    '
                 NEXT SENTENCE ELSE GO TO     F95IA-FN.
           MOVE        GC12 TO PJ41-GC12 (IPJ41L)
           MOVE        'Y' TO PJ41-IGC12 (IPJ41L).
       F95IA-900. GO TO F95DA-FN.
       F95IA-FN. EXIT.
      *N95JA.    NOTE *IF GC21; MOVE TO OUTPUT AREA       *.
       F95JA.    IF    XW05-XSEGNM =                                    lv20
                       'GC21    '
                 NEXT SENTENCE ELSE GO TO     F95JA-FN.
           MOVE        GC21 TO PJ41-GC21 (IPJ41L)
           MOVE        'Y' TO PJ41-IGC21 (IPJ41L).
       F95JA-900. GO TO F95DA-FN.
       F95JA-FN. EXIT.
      *N95LA.    NOTE *UNKNOWN SEGMENT; SKIP RECORD       *.
       F95LA.         EXIT.                                             lv20
       F95LA-FN. EXIT.
       F95DA-FN. EXIT.
       F95CA-FN. EXIT.
       F95-FN.   EXIT.
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
           MD07-TBL-HDR                                                 ADU035
           MD07-PASS-AREA.                                              ADU035
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
