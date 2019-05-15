       IDENTIFICATION DIVISION.                                         CI0271
       PROGRAM-ID.  CI0271P.                                            CI0271
      *AUTHOR.         FA DELETE CATS ACTIVITY.                         CI0271
      *DATE-COMPILED.   09/08/14.                                       CI0271
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2013                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE FA    SYSTEM AND ALL INFORMATION RELATING THERETO,    * ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE FA    SYSTEM AND ALL            * ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE FA          * ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 2013                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0271
       CONFIGURATION SECTION.                                           CI0271
       SOURCE-COMPUTER. IBM-370.                                        CI0271
       OBJECT-COMPUTER. IBM-370.                                        CI0271
       DATA DIVISION.                                                   CI0271
       WORKING-STORAGE SECTION.                                         CI0271

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
                        PICTURE S9(3)                                   CI0271
                          COMPUTATIONAL-3.                              CI0271
                                                                        ADU165
      *>>>>>>> Linkage Area for Logger Program DBI110                   ADU165
      *!WF DSP=DH DSL=DH SEL=10 FOR=I DES=2 LEV=1                       ADU165
       01                 DH10.                                         CI0271
            10            DH10-GERTC  PICTURE  X                        CI0271
                          VALUE                SPACE.                   CI0271
            10            DH10-XUIBP  PICTURE  S9(8)                    CI0271
                          VALUE                ZERO                     CI0271
                          BINARY.                                       CI0271
            10            DH10-NSEQ2P PICTURE  S9(3)                    CI0271
                          VALUE                ZERO                     CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            DH10-CAUL   PICTURE  X                        CI0271
                          VALUE                SPACE.                   CI0271
            10            DH10-MAUSB  PICTURE  X(8)                     CI0271
                          VALUE                SPACE.                   CI0271
            10            DH10-NAUSK  PICTURE  X(50)                    CI0271
                          VALUE                SPACE.                   CI0271
            10            DH10-CSYS   PICTURE  X(4)                     CI0271
                          VALUE                SPACE.                   CI0271
            10            DH10-CAPPL  PICTURE  X(8)                     CI0271
                          VALUE                SPACE.                   CI0271
            10            DH10-CAUSR  PICTURE  X                        CI0271
                          VALUE                SPACE.                   CI0271
            10            DH10-CAUFR  PICTURE  S9(5)                    CI0271
                          VALUE                ZERO                     CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            DH10-CAUAC  PICTURE  S9(5)                    CI0271
                          VALUE                ZERO                     CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            DH10-GEOPID PICTURE  X(6)                     CI0271
                          VALUE                SPACE.                   CI0271
            10            DH10-CAUNIT PICTURE  X(4)                     CI0271
                          VALUE                SPACE.                   CI0271
            10            DH10-GAUVR  PICTURE  X(400)                   CI0271
                          VALUE                SPACE.                   CI0271
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01                 CT01.                                         CI0271
            10            CT01-GC01K.                                   CI0271
            11            CT01-C299.                                    CI0271
            12            CT01-CTID.                                    CI0271
            13            CT01-CTIDA  PICTURE  9(3).                    CI0271
            13            CT01-CTIDN.                                   CI0271
            14            CT01-CTIDNP PICTURE  X(13).                   CI0271
            14            CT01-CTIDND PICTURE  9(11).                   CI0271
            10            CT01-DCAG9L PICTURE  9(8).                    CI0271
            10            CT01-NAASQL PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            CT01-ICUST  PICTURE  X.                       CI0271
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0271
                          BINARY.                                       CI0271
            10            CT01-PRCOD  PICTURE  9(5).                    CI0271
            10            CT01-PRSCD  PICTURE  X(9).                    CI0271
            10            CT01-FILLER PICTURE  X(8).                    CI0271
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0271
            10            XW05-XW06.                                    CI0271
            11            XW05-XDBPCB.                                  CI0271
            12            XW05-XDBDNM PICTURE  X(08)                    CI0271
                          VALUE                SPACE.                   CI0271
            12            XW05-XSEGLV PICTURE  X(02)                    CI0271
                          VALUE                SPACE.                   CI0271
            12            XW05-XRC    PICTURE  X(02)                    CI0271
                          VALUE                SPACE.                   CI0271
            12            XW05-XPROPT PICTURE  X(04)                    CI0271
                          VALUE                SPACE.                   CI0271
            12            XW05-FILLER PICTURE  S9(5)                    CI0271
                          VALUE                ZERO                     CI0271
                          BINARY.                                       CI0271
            12            XW05-XSEGNM PICTURE  X(08)                    CI0271
                          VALUE                SPACE.                   CI0271
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0271
                          VALUE                ZERO                     CI0271
                          BINARY.                                       CI0271
            12            XW05-XSEGNB PICTURE  9(05)                    CI0271
                          VALUE                ZERO                     CI0271
                          BINARY.                                       CI0271
            12            XW05-XCOKEY PICTURE  X(70)                    CI0271
                          VALUE                SPACE.                   CI0271
            10            XW05-XW07.                                    CI0271
            11            XW05-XIOPCB.                                  CI0271
            12            XW05-XTERMI PICTURE  X(08)                    CI0271
                          VALUE                SPACE.                   CI0271
            12            XW05-FILLER PICTURE  XX                       CI0271
                          VALUE                SPACE.                   CI0271
            12            XW05-XRC1   PICTURE  X(02)                    CI0271
                          VALUE                SPACE.                   CI0271
            12            XW05-FILLER PICTURE  X(12)                    CI0271
                          VALUE                SPACE.                   CI0271
            12            XW05-XMODNM PICTURE  X(8)                     CI0271
                          VALUE                SPACE.                   CI0271
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0271
                          VALUE                ZERO.                    CI0271
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0271
                          VALUE                ZERO.                    CI0271
            10            XW05-XGU    PICTURE  X(4)                     CI0271
                          VALUE                'GU  '.                  CI0271
            10            XW05-XGHU   PICTURE  X(4)                     CI0271
                          VALUE                'GHU '.                  CI0271
            10            XW05-XGN    PICTURE  X(4)                     CI0271
                          VALUE                'GN  '.                  CI0271
            10            XW05-XGHN   PICTURE  X(4)                     CI0271
                          VALUE                'GHN '.                  CI0271
            10            XW05-XGNP   PICTURE  X(4)                     CI0271
                          VALUE                'GNP '.                  CI0271
            10            XW05-XGHNP  PICTURE  X(4)                     CI0271
                          VALUE                'GHNP'.                  CI0271
            10            XW05-XREPL  PICTURE  XXXX                     CI0271
                          VALUE                'REPL'.                  CI0271
            10            XW05-XISRT  PICTURE  X(4)                     CI0271
                          VALUE                'ISRT'.                  CI0271
            10            XW05-XDLET  PICTURE  X(4)                     CI0271
                          VALUE                'DLET'.                  CI0271
            10            XW05-XOPEN  PICTURE  X(4)                     CI0271
                          VALUE                'OPEN'.                  CI0271
            10            XW05-XCLSE  PICTURE  X(4)                     CI0271
                          VALUE                'CLSE'.                  CI0271
            10            XW05-XCHKP  PICTURE  X(4)                     CI0271
                          VALUE                'CHKP'.                  CI0271
            10            XW05-XXRST  PICTURE  X(4)                     CI0271
                          VALUE                'XRST'.                  CI0271
            10            XW05-XTERM  PICTURE  X(4)                     CI0271
                          VALUE                'TERM'.                  CI0271
            10            XW05-XNFPAC PICTURE  X(13)                    CI0271
                          VALUE                SPACE.                   CI0271
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0271
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0271
       01                 GC01.                                         CI0271
            10            GC01-GC01K.                                   CI0271
            11            GC01-C299.                                    CI0271
            12            GC01-CTID.                                    CI0271
            13            GC01-CTIDA  PICTURE  9(3).                    CI0271
            13            GC01-CTIDN.                                   CI0271
            14            GC01-CTIDNP PICTURE  X(13).                   CI0271
            14            GC01-CTIDND PICTURE  9(11).                   CI0271
            10            GC01-DCAG9L PICTURE  9(8).                    CI0271
            10            GC01-NAASQL PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            GC01-ICUST  PICTURE  X.                       CI0271
            10            GC01-NSEQ4B PICTURE  9(8)                     CI0271
                          BINARY.                                       CI0271
            10            GC01-PRCOD  PICTURE  9(5).                    CI0271
            10            GC01-PRSCD  PICTURE  X(9).                    CI0271
            10            GC01-FILLER PICTURE  X(8).                    CI0271
       01                 GC03.                                         CI0271
            10            GC03-GELL   PICTURE  9(4)                     CI0271
                          BINARY.                                       CI0271
            10            GC03-GD00.                                    CI0271
            11            GC03-GC03K.                                   CI0271
            12            GC03-DCACG9 PICTURE  9(8).                    CI0271
            12            GC03-NAASQ  PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-CAATY  PICTURE  9(3).                    CI0271
            11            GC03-CVSYS  PICTURE  X(2).                    CI0271
            11            GC03-CACTO  PICTURE  9(3).                    CI0271
            11            GC03-CATRN.                                   CI0271
            12            GC03-CATRF  PICTURE  9(3).                    CI0271
            12            GC03-CATRS  PICTURE  9(3).                    CI0271
            11            GC03-CASTC  PICTURE  99.                      CI0271
            11            GC03-IPULL  PICTURE  X.                       CI0271
            11            GC03-GEAUN  PICTURE  9(5).                    CI0271
            11            GC03-GEOPD2 PICTURE  X(8).                    CI0271
            11            GC03-NBTCH  PICTURE  9(4).                    CI0271
            11            GC03-DEFFT  PICTURE  9(8).                    CI0271
            11            GC03-NSUNT  PICTURE  9(4).                    CI0271
            11            GC03-ITRAN  PICTURE  X.                       CI0271
            11            GC03-DLAUP1 PICTURE  9(8).                    CI0271
            11            GC03-ADRET  PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-TTRMS  PICTURE  X(12).                   CI0271
            11            GC03-IDELT  PICTURE  X.                       CI0271
            11            GC03-GEOPDM PICTURE  X(8).                    CI0271
            11            GC03-FILLER PICTURE  X(07).                   CI0271
            10            GC03-GD09.                                    CI0271
            11            GC03-FILLER PICTURE  X(70).                   CI0271
            10            GC03-GD01                                     CI0271
                          REDEFINES            GC03-GD09.               CI0271
            11            GC03-ADBRQ  PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-CTRTP  PICTURE  X(2).                    CI0271
            11            GC03-CPORT  PICTURE  X.                       CI0271
            11            GC03-CSCRNU PICTURE  X(4).                    CI0271
            11            GC03-DLAUP  PICTURE  9(8).                    CI0271
            11            GC03-CTWHAT PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-PWHLD  PICTURE  S999V9(5)                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-IWTHH  PICTURE  X.                       CI0271
            11            GC03-NDRFT  PICTURE  9(5).                    CI0271
            11            GC03-IDPAP  PICTURE  X.                       CI0271
            11            GC03-GETIM  PICTURE  S9(7)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-QNACT  PICTURE  9(3).                    CI0271
            11            GC03-AEDRQ  PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-IPLIN  PICTURE  X.                       CI0271
            11            GC03-CLIDNB PICTURE  9(8).                    CI0271
            11            GC03-CSLCT  PICTURE  X.                       CI0271
            11            GC03-ITELE  PICTURE  X.                       CI0271
            11            GC03-FILLER PICTURE  X(06).                   CI0271
            10            GC03-GD02                                     CI0271
                          REDEFINES            GC03-GD09.               CI0271
            11            GC03-CSYST  PICTURE  99.                      CI0271
            11            GC03-FILLER PICTURE  X.                       CI0271
            11            GC03-ACASH  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-DTRAC  PICTURE  9(8).                    CI0271
            11            GC03-CTRSO  PICTURE  9(02).                   CI0271
            11            GC03-NTRCE  PICTURE  9(06).                   CI0271
            11            GC03-GECKD1 PICTURE  9.                       CI0271
            11            GC03-CCOLL  PICTURE  X(3).                    CI0271
            11            GC03-CLTDP  PICTURE  X(3).                    CI0271
            11            GC03-PSLLD  PICTURE  S99V999                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-ISLOR  PICTURE  X.                       CI0271
            11            GC03-ITPAC  PICTURE  X.                       CI0271
            11            GC03-CPMTCA PICTURE  XXX.                     CI0271
            11            GC03-CSERV  PICTURE  X(3).                    CI0271
            11            GC03-ACOMO  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-IPLIN1 PICTURE  X.                       CI0271
            11            GC03-INQEX  PICTURE  X.                       CI0271
            11            GC03-CTKRAA PICTURE  X(12).                   CI0271
            11            GC03-CCSMQ  PICTURE  X.                       CI0271
            11            GC03-IVAEX1 PICTURE  X.                       CI0271
            11            GC03-IHPMT  PICTURE  X(1).                    CI0271
            11            GC03-GETIM3 PICTURE  S9(7)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            GC03-GD03                                     CI0271
                          REDEFINES            GC03-GD09.               CI0271
            11            GC03-CATRNC PICTURE  9(6).                    CI0271
            11            GC03-APRNT1 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-QSHOWT PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-ACINVT PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-ACOMO7 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-QSHOMW PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-ATAXT3 PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-CTSTR  PICTURE  9(2).                    CI0271
            11            GC03-ICIRA  PICTURE  X.                       CI0271
            11            GC03-GETIM2 PICTURE  S9(7)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-CPMTCX PICTURE  XX.                      CI0271
            11            GC03-FILLER PICTURE  X(16).                   CI0271
            10            GC03-GD99.                                    CI0271
            11            GC03-FILLER PICTURE  X(248).                  CI0271
            10            GC03-GD10                                     CI0271
                          REDEFINES            GC03-GD99.               CI0271
            11            GC03-MROTC  PICTURE  X(7).                    CI0271
            11            GC03-CEDSC  PICTURE  9(1).                    CI0271
            11            GC03-ILPOI  PICTURE  X(1).                    CI0271
            11            GC03-AWRCH  PICTURE  S9(3)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-CHCOC1 PICTURE  9(2).                    CI0271
            11            GC03-CHCOC2 PICTURE  9(2).                    CI0271
            11            GC03-CHCOC3 PICTURE  9(2).                    CI0271
            11            GC03-CHCOC4 PICTURE  9(2).                    CI0271
            11            GC03-CMCOC1 PICTURE  9(3).                    CI0271
            11            GC03-CMCOC2 PICTURE  9(3).                    CI0271
            11            GC03-CMCOC3 PICTURE  9(3).                    CI0271
            11            GC03-GD11.                                    CI0271
            12            GC03-FILLER PICTURE  X(219).                  CI0271
            11            GC03-GD12                                     CI0271
                          REDEFINES            GC03-GD11.               CI0271
            12            GC03-CELLO  PICTURE  9(1).                    CI0271
            12            GC03-CECLO  PICTURE  9(1).                    CI0271
            12            GC03-AEXML  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-CEPI   PICTURE  X(1).                    CI0271
            12            GC03-CEXTY  PICTURE  X.                       CI0271
            12            GC03-CROPC  PICTURE  9(1).                    CI0271
            12            GC03-CPUTY  PICTURE  9(1).                    CI0271
            12            GC03-IMCII  PICTURE  X(1).                    CI0271
            12            GC03-GEMISC                                   CI0271
                          OCCURS       010     TIMES.                   CI0271
            13            GC03-AMGLA  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            13            GC03-CMGLC  PICTURE  9(1).                    CI0271
            13            GC03-NMGLN  PICTURE  9(4).                    CI0271
            12            GC03-ACTRN  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-IWRBK  PICTURE  X.                       CI0271
            12            GC03-IFEDX  PICTURE  X.                       CI0271
            12            GC03-ICNTR  PICTURE  X.                       CI0271
            12            GC03-IOCKH  PICTURE  X.                       CI0271
            12            GC03-ICRCK  PICTURE  X.                       CI0271
            12            GC03-NHMPN  PICTURE  S9(10)                   CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-ITELR1 PICTURE  X.                       CI0271
            11            GC03-GD13                                     CI0271
                          REDEFINES            GC03-GD11.               CI0271
            12            GC03-DREDO  PICTURE  9(8).                    CI0271
            12            GC03-CATRNR PICTURE  9(6).                    CI0271
            12            GC03-CEVN   PICTURE  9(9).                    CI0271
            12            GC03-ISUSP  PICTURE  X(1).                    CI0271
            11            GC03-GD15                                     CI0271
                          REDEFINES            GC03-GD11.               CI0271
            12            GC03-CPUTZ  PICTURE  9(1).                    CI0271
            12            GC03-CETLB  PICTURE  9(3).                    CI0271
            12            GC03-QTRMC  PICTURE  9(3).                    CI0271
            12            GC03-DEFFTE PICTURE  9(8).                    CI0271
            12            GC03-DEFFTF PICTURE  9(8).                    CI0271
            12            GC03-DEFFTG PICTURE  9(8).                    CI0271
            12            GC03-XZ1A   PICTURE  X.                       CI0271
            12            GC03-XZ1B   PICTURE  X.                       CI0271
            12            GC03-XZ1C   PICTURE  X.                       CI0271
            12            GC03-XZ1D   PICTURE  X.                       CI0271
            12            GC03-XZ1E   PICTURE  X.                       CI0271
            12            GC03-XZ1F   PICTURE  X.                       CI0271
            12            GC03-XZ1G   PICTURE  X.                       CI0271
            12            GC03-XZ1H   PICTURE  X.                       CI0271
            12            GC03-XZ1I   PICTURE  X.                       CI0271
            12            GC03-DEFFTH PICTURE  9(8).                    CI0271
            11            GC03-GD19                                     CI0271
                          REDEFINES            GC03-GD11.               CI0271
            12            GC03-GD11.                                    CI0271
            13            GC03-FILLER PICTURE  X(219).                  CI0271
            10            GC03-GD20                                     CI0271
                          REDEFINES            GC03-GD99.               CI0271
            11            GC03-ADDACT PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-ISIGV  PICTURE  X.                       CI0271
            11            GC03-IALLF  PICTURE  X.                       CI0271
            11            GC03-QSHOWQ PICTURE  S9(9)V999                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-CCDSCW PICTURE  9(2).                    CI0271
            11            GC03-IDWRL  PICTURE  X.                       CI0271
            11            GC03-ITELR  PICTURE  X.                       CI0271
            11            GC03-IABIN  PICTURE  X.                       CI0271
            11            GC03-PACT1  PICTURE  S999V999                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-IBFAF  PICTURE  X.                       CI0271
            11            GC03-IFRSA  PICTURE  X.                       CI0271
            11            GC03-ICRCAN PICTURE  X.                       CI0271
            11            GC03-ACACTV PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-AGFND  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-QCSHOW PICTURE  S9(9)V999                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-QCSHIS PICTURE  S9(9)V999                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-NDTRC  PICTURE  9(8).                    CI0271
            11            GC03-CAERU  PICTURE  X(4).                    CI0271
            11            GC03-IFDGO  PICTURE  X.                       CI0271
            11            GC03-PSLLD2 PICTURE  S99V999                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-ISLOR2 PICTURE  X.                       CI0271
            11            GC03-QSFIO  PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-QSFID  PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-CGDIN  PICTURE  X.                       CI0271
            11            GC03-DGDIN  PICTURE  9(8).                    CI0271
            10            GC03-GD30                                     CI0271
                          REDEFINES            GC03-GD99.               CI0271
            11            GC03-ISKED  PICTURE  X.                       CI0271
            11            GC03-CENXC  PICTURE  9(2).                    CI0271
            11            GC03-GD31.                                    CI0271
            12            GC03-FILLER PICTURE  X(245).                  CI0271
            11            GC03-GD32                                     CI0271
                          REDEFINES            GC03-GD31.               CI0271
            12            GC03-IABIN1 PICTURE  X.                       CI0271
            12            GC03-CLDOD  PICTURE  9(8).                    CI0271
            12            GC03-NCLAM  PICTURE  9(5)                     CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-ISURR  PICTURE  X.                       CI0271
            12            GC03-GEHCD  PICTURE  9(3).                    CI0271
            12            GC03-CRATC  PICTURE  9(4).                    CI0271
            12            GC03-AMAXD  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-ASCHGA PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-APYOM  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-IWTHH1 PICTURE  X.                       CI0271
            12            GC03-CPAYCL PICTURE  X(2).                    CI0271
            12            GC03-CTSAO  PICTURE  X.                       CI0271
            12            GC03-NCONF  PICTURE  9(08).                   CI0271
            12            GC03-CLID   PICTURE  X(23).                   CI0271
            12            GC03-CARTY  PICTURE  99.                      CI0271
            12            GC03-NARRS  PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-CARTZ  PICTURE  99.                      CI0271
            12            GC03-NAPDS  PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-CPMTO  PICTURE  X.                       CI0271
            12            GC03-DNPMT  PICTURE  9(8).                    CI0271
            12            GC03-IPCTV  PICTURE  X.                       CI0271
            12            GC03-IMECH  PICTURE  X(01).                   CI0271
            12            GC03-IMVAO  PICTURE  X(1).                    CI0271
            12            GC03-AMVA1  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-CACTS  PICTURE  X.                       CI0271
            12            GC03-CTSPP  PICTURE  X(1).                    CI0271
            12            GC03-CACT4  PICTURE  X(2).                    CI0271
            12            GC03-IVAEX  PICTURE  X.                       CI0271
            12            GC03-DFPMT  PICTURE  9(8).                    CI0271
            12            GC03-IDEMD  PICTURE  X.                       CI0271
            12            GC03-IOFST  PICTURE  X.                       CI0271
            12            GC03-AMXLB  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-ACULB  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-DEIRNB PICTURE  9(8).                    CI0271
            12            GC03-DEFFE  PICTURE  9(8).                    CI0271
            12            GC03-DEFFR  PICTURE  9(8).                    CI0271
            12            GC03-ISPUP  PICTURE  X.                       CI0271
            12            GC03-CPNCG  PICTURE  X.                       CI0271
            12            GC03-IEXPU  PICTURE  X.                       CI0271
            12            GC03-IPPCF  PICTURE  X.                       CI0271
            12            GC03-NAAPT  PICTURE  9(2).                    CI0271
            12            GC03-PWHLDS PICTURE  S999V9(5)                CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-ISWHO  PICTURE  X(1).                    CI0271
            11            GC03-GD33                                     CI0271
                          REDEFINES            GC03-GD31.               CI0271
            12            GC03-CPAYC  PICTURE  X(2).                    CI0271
            12            GC03-ADBRQX PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-ADBRQV PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-APTXR  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-CTRTPE PICTURE  X(2).                    CI0271
            12            GC03-NCLAMI PICTURE  S9(9)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-CLIDO8 PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-CLIDN  PICTURE  X(20).                   CI0271
            12            GC03-DSET01 PICTURE  S9(8)                    CI0271
                          BINARY.                                       CI0271
            12            GC03-CTSET1 PICTURE  S9(6)                    CI0271
                          BINARY.                                       CI0271
            12            GC03-DSET02 PICTURE  S9(8)                    CI0271
                          BINARY.                                       CI0271
            12            GC03-CTSET2 PICTURE  S9(6)                    CI0271
                          BINARY.                                       CI0271
            11            GC03-GD34                                     CI0271
                          REDEFINES            GC03-GD31.               CI0271
            12            GC03-QNOFM  PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-CLTRM  PICTURE  99.                      CI0271
            12            GC03-AMXLN  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-ALADJ  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-ACHK   PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-APRMO  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-IMECH1 PICTURE  X(01).                   CI0271
            12            GC03-CACT41 PICTURE  X(2).                    CI0271
            12            GC03-ACDSCC PICTURE  S9(05)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-ACDSCD PICTURE  S9(05)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-GD39                                     CI0271
                          REDEFINES            GC03-GD31.               CI0271
            12            GC03-GD31.                                    CI0271
            13            GC03-FILLER PICTURE  X(245).                  CI0271
            10            GC03-GD40                                     CI0271
                          REDEFINES            GC03-GD99.               CI0271
            11            GC03-NTR    PICTURE  9(8).                    CI0271
            11            GC03-NPBNC  PICTURE  X(24).                   CI0271
            11            GC03-CRREV  PICTURE  X(3).                    CI0271
            11            GC03-CSUSL  PICTURE  S9.                      CI0271
            11            GC03-NMGLN1 PICTURE  9(4).                    CI0271
            11            GC03-DCAC92 PICTURE  9(8).                    CI0271
            11            GC03-NAASQ3 PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-GD49.                                    CI0271
            12            GC03-FILLER PICTURE  X(198).                  CI0271
            11            GC03-GD41                                     CI0271
                          REDEFINES            GC03-GD49.               CI0271
            12            GC03-CRREF  PICTURE  9(2).                    CI0271
            12            GC03-CORIR  PICTURE  X(02).                   CI0271
            12            GC03-CIPDB  PICTURE  X(03).                   CI0271
            12            GC03-CPAYH  PICTURE  X(02).                   CI0271
            12            GC03-NAMEX  PICTURE  9(15)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC03-DCHAE  PICTURE  9(4).                    CI0271
            12            GC03-DRQST  PICTURE  S9(8)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-GD42                                     CI0271
                          REDEFINES            GC03-GD49.               CI0271
            12            GC03-CPMTCB PICTURE  X(3).                    CI0271
            10            GC03-GD50                                     CI0271
                          REDEFINES            GC03-GD99.               CI0271
            11            GC03-ALOAD  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-PSLLD4 PICTURE  S99V999                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-CSUSL1 PICTURE  S9.                      CI0271
            11            GC03-CRREV1 PICTURE  X(3).                    CI0271
            11            GC03-ADDAC  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-DL13.                                    CI0271
            12            GC03-GEYR   PICTURE  9(4).                    CI0271
            12            GC03-GEMTH  PICTURE  99.                      CI0271
            12            GC03-NDAY   PICTURE  99.                      CI0271
            11            GC03-NSEQ3P PICTURE  S9(5)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-XZ6A   PICTURE  X(6).                    CI0271
            11            GC03-XZ7    PICTURE  X(7).                    CI0271
            11            GC03-XZ6B   PICTURE  X(6).                    CI0271
            11            GC03-XZ6    PICTURE  X(6).                    CI0271
            11            GC03-XZ6C   PICTURE  X(6).                    CI0271
            11            GC03-XZ20   PICTURE  X(20).                   CI0271
            11            GC03-CATRN1 PICTURE  9(6).                    CI0271
            11            GC03-ADDAC2 PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-ATAXT2 PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-ACOMOT PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-XZ5    PICTURE  X(5).                    CI0271
            11            GC03-IREVD  PICTURE  X(1).                    CI0271
            11            GC03-ISUSP1 PICTURE  X(1).                    CI0271
            11            GC03-XZ6D   PICTURE  X(6).                    CI0271
            11            GC03-XZ13   PICTURE  X(13).                   CI0271
            11            GC03-CWHTP2 PICTURE  X(3).                    CI0271
            11            GC03-CWHTP3 PICTURE  X(3).                    CI0271
            11            GC03-DTREN  PICTURE  9(8).                    CI0271
            11            GC03-NAASQ1 PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            GC03-GD51                                     CI0271
                          REDEFINES            GC03-GD99.               CI0271
            11            GC03-ADOMOT PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-ACGLT  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-ACGST  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-CTXMT  PICTURE  9(2).                    CI0271
            11            GC03-ALOAD3 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-FILLER PICTURE  X(31).                   CI0271
            10            GC03-GD52                                     CI0271
                          REDEFINES            GC03-GD99.               CI0271
            11            GC03-DEFFT5 PICTURE  9(8).                    CI0271
            11            GC03-PSLLD5 PICTURE  S99V999                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-CSUSL2 PICTURE  S9.                      CI0271
            11            GC03-ALOAD2 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-DL22.                                    CI0271
            12            GC03-NYEAR1 PICTURE  9(4).                    CI0271
            12            GC03-GEMTHA PICTURE  99.                      CI0271
            12            GC03-NDAY01 PICTURE  99.                      CI0271
            11            GC03-NSEQ3R PICTURE  S9(5)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-CWHTP  PICTURE  X(3).                    CI0271
            11            GC03-CWHFR  PICTURE  X(3).                    CI0271
            11            GC03-CATRN7 PICTURE  9(6).                    CI0271
            11            GC03-ATAXT5 PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-QSHOT  PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-ACINT3 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-CWHTP1 PICTURE  X(3).                    CI0271
            11            GC03-CWHFR1 PICTURE  X(3).                    CI0271
            11            GC03-ACOMO5 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-QSHOMU PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-ACASH1 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-FILLER PICTURE  X(04).                   CI0271
            11            GC03-CATRN8 PICTURE  9(6).                    CI0271
            11            GC03-ALOAD1 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-PSLLD1 PICTURE  S99V999                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-QSHOT1 PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-ACINT4 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-CSUSL4 PICTURE  S9.                      CI0271
            11            GC03-ACOMO4 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            GC03-GD60                                     CI0271
                          REDEFINES            GC03-GD99.               CI0271
            11            GC03-GEOPDD PICTURE  X(8)                     CI0271
                          OCCURS       005     TIMES.                   CI0271
            11            GC03-DLAUP3 PICTURE  9(8)                     CI0271
                          OCCURS       005     TIMES.                   CI0271
            11            GC03-GEOPDB PICTURE  X(8).                    CI0271
            11            GC03-DLAUP4 PICTURE  9(8).                    CI0271
            11            GC03-ITELR2 PICTURE  X.                       CI0271
            11            GC03-IPMTA  PICTURE  X.                       CI0271
            11            GC03-CCSMG  PICTURE  X.                       CI0271
            11            GC03-CPLEC  PICTURE  XX.                      CI0271
            11            GC03-CORTYA PICTURE  X(3).                    CI0271
            11            GC03-CACTBC PICTURE  X(1).                    CI0271
            11            GC03-CGSPIA PICTURE  X.                       CI0271
            11            GC03-IPTRDA PICTURE  X(01).                   CI0271
            11            GC03-GCUSPY PICTURE  X(12).                   CI0271
            11            GC03-CPALLA PICTURE  X(1).                    CI0271
            11            GC03-QSHO5A PICTURE  S9(9)V999                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-IFRSAB PICTURE  X.                       CI0271
            11            GC03-DELOI  PICTURE  9(8).                    CI0271
            11            GC03-IAROAA PICTURE  X.                       CI0271
            11            GC03-ACINVR PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-ILTINA PICTURE  X.                       CI0271
            11            GC03-ALOIDA PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC03-CFUNTA PICTURE  X(2).                    CI0271
            11            GC03-CLGND  PICTURE  X.                       CI0271
            11            GC03-CPH3U  PICTURE  X.                       CI0271
            11            GC03-GESTD  PICTURE  9(8).                    CI0271
            11            GC03-GEEND  PICTURE  9(8).                    CI0271
            11            GC03-CPMTF  PICTURE  99.                      CI0271
            11            GC03-CNAVR  PICTURE  X(1).                    CI0271
            10            GC03-GD70                                     CI0271
                          REDEFINES            GC03-GD99.               CI0271
            11            GC03-CMEMO  PICTURE  X(2).                    CI0271
            11            GC03-ALPLDT PICTURE  9(8).                    CI0271
            11            GC03-CTLPD  PICTURE  9(8).                    CI0271
            11            GC03-CPAYCM PICTURE  X(2).                    CI0271
       01                 GC04.                                         CI0271
            10            GC04-CLCUS  PICTURE  99.                      CI0271
            10            GC04-CCACT  PICTURE  99.                      CI0271
            10            GC04-AFEET  PICTURE  S9(5)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            GC04-ITERF  PICTURE  X.                       CI0271
            10            GC04-ATERF  PICTURE  S9(5)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            GC04-CLDOB  PICTURE  9(8).                    CI0271
            10            GC04-CPLTYP PICTURE  X(14).                   CI0271
            10            GC04-IACFPD PICTURE  X(1).                    CI0271
            10            GC04-FILLER PICTURE  X(14).                   CI0271
       01                 GC06.                                         CI0271
            10            GC06-GELL   PICTURE  9(4)                     CI0271
                          BINARY.                                       CI0271
            10            GC06-GE00.                                    CI0271
            11            GC06-GC06K.                                   CI0271
            12            GC06-NPISQ  PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC06-ACOTD  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC06-PPOTD  PICTURE  S9(3)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC06-QPSTD  PICTURE  S9(7)V999                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC06-CPITC  PICTURE  99.                      CI0271
            11            GC06-ITRNB  PICTURE  X.                       CI0271
            11            GC06-FILLER PICTURE  X(14).                   CI0271
            10            GC06-GE98.                                    CI0271
            11            GC06-FILLER PICTURE  X(240).                  CI0271
            10            GC06-GE10                                     CI0271
                          REDEFINES            GC06-GE98.               CI0271
            11            GC06-CDELI  PICTURE  9(3).                    CI0271
            11            GC06-CPAYC  PICTURE  X(2).                    CI0271
            11            GC06-ICHKP  PICTURE  X.                       CI0271
            11            GC06-CLTIN  PICTURE  9(12).                   CI0271
            11            GC06-IFHAI  PICTURE  X.                       CI0271
            11            GC06-CDQUA  PICTURE  X(2).                    CI0271
            11            GC06-FILLER PICTURE  X(07).                   CI0271
            11            GC06-GE99.                                    CI0271
            12            GC06-FILLER PICTURE  X(212).                  CI0271
            11            GC06-GE01                                     CI0271
                          REDEFINES            GC06-GE99.               CI0271
            12            GC06-NTR    PICTURE  9(8).                    CI0271
            12            GC06-GECKD  PICTURE  9.                       CI0271
            12            GC06-NPBN   PICTURE  X(20).                   CI0271
            12            GC06-CCBAT  PICTURE  99.                      CI0271
            12            GC06-CLID4  PICTURE  X(23).                   CI0271
            12            GC06-GENAL1 PICTURE  X(30)                    CI0271
                          OCCURS       002     TIMES.                   CI0271
            12            GC06-GESAD1 PICTURE  X(30)                    CI0271
                          OCCURS       003     TIMES.                   CI0271
            11            GC06-GE02                                     CI0271
                          REDEFINES            GC06-GE99.               CI0271
            12            GC06-GENAL  PICTURE  X(30)                    CI0271
                          OCCURS       002     TIMES.                   CI0271
            12            GC06-GESAD  PICTURE  X(30)                    CI0271
                          OCCURS       003     TIMES.                   CI0271
            11            GC06-GE03                                     CI0271
                          REDEFINES            GC06-GE99.               CI0271
            12            GC06-NCHKN  PICTURE  9(11).                   CI0271
            11            GC06-GE04                                     CI0271
                          REDEFINES            GC06-GE99.               CI0271
            12            GC06-CTIDAP PICTURE  9(3).                    CI0271
            12            GC06-PRCOD  PICTURE  9(5).                    CI0271
            12            GC06-TDELI  PICTURE  X(30).                   CI0271
            12            GC06-CINCD  PICTURE  9(02).                   CI0271
            10            GC06-GE20                                     CI0271
                          REDEFINES            GC06-GE98.               CI0271
            11            GC06-C299.                                    CI0271
            12            GC06-CTID.                                    CI0271
            13            GC06-CTIDA  PICTURE  9(3).                    CI0271
            13            GC06-CTIDN.                                   CI0271
            14            GC06-CTIDNP PICTURE  X(13).                   CI0271
            14            GC06-CTIDND PICTURE  9(11).                   CI0271
            11            GC06-DCACG9 PICTURE  9(8).                    CI0271
            11            GC06-NAASQ  PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GC06-CIRAP  PICTURE  XX.                      CI0271
            11            GC06-CTYPE  PICTURE  X.                       CI0271
            11            GC06-INACT  PICTURE  X.                       CI0271
            11            GC06-FILLER PICTURE  X(01).                   CI0271
            11            GC06-ITPAC  PICTURE  X.                       CI0271
            11            GC06-ITAXI  PICTURE  X.                       CI0271
            11            GC06-IOWNC  PICTURE  X.                       CI0271
            11            GC06-CDVCD  PICTURE  X(2).                    CI0271
            11            GC06-CTCUS  PICTURE  999.                     CI0271
            11            GC06-CPMTCB PICTURE  X(3).                    CI0271
            11            GC06-CASTC1 PICTURE  99.                      CI0271
            11            GC06-PRCOD1 PICTURE  9(5).                    CI0271
            11            GC06-CPRSC1 PICTURE  X(9).                    CI0271
            11            GC06-CPRTB  PICTURE  X.                       CI0271
            11            GC06-CBRKD  PICTURE  9(4).                    CI0271
            11            GC06-FILLER PICTURE  X(12).                   CI0271
            10            GC06-GE30                                     CI0271
                          REDEFINES            GC06-GE98.               CI0271
            11            GC06-CFIDC  PICTURE  X(5).                    CI0271
            11            GC06-CPHSE  PICTURE  9(2).                    CI0271
            11            GC06-FILLER PICTURE  X(05).                   CI0271
            11            GC06-IABIN  PICTURE  X.                       CI0271
            11            GC06-PDFND  PICTURE  S999V9(3)                CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            GC06-GE40                                     CI0271
                          REDEFINES            GC06-GE98.               CI0271
            11            GC06-CACCT  PICTURE  X.                       CI0271
            11            GC06-CPAYR  PICTURE  X(2).                    CI0271
            11            GC06-CDELI1 PICTURE  9(3).                    CI0271
            11            GC06-CATRN.                                   CI0271
            12            GC06-CATRF  PICTURE  9(3).                    CI0271
            12            GC06-CATRS  PICTURE  9(3).                    CI0271
            11            GC06-DEFFT  PICTURE  9(8).                    CI0271
            11            GC06-CTYPC  PICTURE  X.                       CI0271
            11            GC06-CIRAPA PICTURE  XX.                      CI0271
            11            GC06-FILLER PICTURE  X(09).                   CI0271
            11            GC06-GE49.                                    CI0271
            12            GC06-FILLER PICTURE  X(208).                  CI0271
            11            GC06-GE41                                     CI0271
                          REDEFINES            GC06-GE49.               CI0271
            12            GC06-NCHKN1 PICTURE  9(6).                    CI0271
            11            GC06-GE42                                     CI0271
                          REDEFINES            GC06-GE49.               CI0271
            12            GC06-CTID1.                                   CI0271
            13            GC06-CTIDA1 PICTURE  9(3).                    CI0271
            13            GC06-CTIDP1 PICTURE  X(13).                   CI0271
            13            GC06-CTIDN1 PICTURE  9(11).                   CI0271
            11            GC06-GE43                                     CI0271
                          REDEFINES            GC06-GE49.               CI0271
            12            GC06-GENAL2 PICTURE  X(30)                    CI0271
                          OCCURS       002     TIMES.                   CI0271
            12            GC06-GESAD2 PICTURE  X(30)                    CI0271
                          OCCURS       003     TIMES.                   CI0271
            11            GC06-GE44                                     CI0271
                          REDEFINES            GC06-GE49.               CI0271
            12            GC06-CTID01.                                  CI0271
            13            GC06-CTIDA6 PICTURE  9(3).                    CI0271
            13            GC06-NTIDP2 PICTURE  X(13).                   CI0271
            13            GC06-CTIDN2 PICTURE  9(11).                   CI0271
            12            GC06-GECKD2 PICTURE  9.                       CI0271
            12            GC06-PACCT  PICTURE  S999V99                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC06-PLOAN  PICTURE  S999V99                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC06-PADPT  PICTURE  S999V99                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GC06-IPCTL  PICTURE  X.                       CI0271
            12            GC06-IPCTP  PICTURE  X.                       CI0271
            12            GC06-CEUNT  PICTURE  S9(5)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            GC06-GE31                                     CI0271
                          REDEFINES            GC06-GE98.               CI0271
            11            GC06-GCUSPZ PICTURE  X(12).                   CI0271
       01                 GX01.                                         CI0271
            10            GX01-GC01K.                                   CI0271
            11            GX01-C299.                                    CI0271
            12            GX01-CTID.                                    CI0271
            13            GX01-CTIDA  PICTURE  9(3).                    CI0271
            13            GX01-CTIDN.                                   CI0271
            14            GX01-CTIDNP PICTURE  X(13).                   CI0271
            14            GX01-CTIDND PICTURE  9(11).                   CI0271
            10            GX01-DCAG9L PICTURE  9(8).                    CI0271
            10            GX01-NAASQL PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            GX01-ICUST  PICTURE  X.                       CI0271
            10            GX01-NSEQ4B PICTURE  9(8)                     CI0271
                          BINARY.                                       CI0271
            10            GX01-PRCOD  PICTURE  9(5).                    CI0271
            10            GX01-PRSCD  PICTURE  X(9).                    CI0271
            10            GX01-FILLER PICTURE  X(8).                    CI0271
       01                 GX03.                                         CI0271
            10            GX03-GELL   PICTURE  9(4)                     CI0271
                          BINARY.                                       CI0271
            10            GX03-GD00.                                    CI0271
            11            GX03-GC03K.                                   CI0271
            12            GX03-DCACG9 PICTURE  9(8).                    CI0271
            12            GX03-NAASQ  PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-CAATY  PICTURE  9(3).                    CI0271
            11            GX03-CVSYS  PICTURE  X(2).                    CI0271
            11            GX03-CACTO  PICTURE  9(3).                    CI0271
            11            GX03-CATRN.                                   CI0271
            12            GX03-CATRF  PICTURE  9(3).                    CI0271
            12            GX03-CATRS  PICTURE  9(3).                    CI0271
            11            GX03-CASTC  PICTURE  99.                      CI0271
            11            GX03-IPULL  PICTURE  X.                       CI0271
            11            GX03-GEAUN  PICTURE  9(5).                    CI0271
            11            GX03-GEOPD2 PICTURE  X(8).                    CI0271
            11            GX03-NBTCH  PICTURE  9(4).                    CI0271
            11            GX03-DEFFT  PICTURE  9(8).                    CI0271
            11            GX03-NSUNT  PICTURE  9(4).                    CI0271
            11            GX03-ITRAN  PICTURE  X.                       CI0271
            11            GX03-DLAUP1 PICTURE  9(8).                    CI0271
            11            GX03-ADRET  PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-TTRMS  PICTURE  X(12).                   CI0271
            11            GX03-IDELT  PICTURE  X.                       CI0271
            11            GX03-GEOPDM PICTURE  X(8).                    CI0271
            11            GX03-FILLER PICTURE  X(07).                   CI0271
            10            GX03-GD09.                                    CI0271
            11            GX03-FILLER PICTURE  X(70).                   CI0271
            10            GX03-GD01                                     CI0271
                          REDEFINES            GX03-GD09.               CI0271
            11            GX03-ADBRQ  PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-CTRTP  PICTURE  X(2).                    CI0271
            11            GX03-CPORT  PICTURE  X.                       CI0271
            11            GX03-CSCRNU PICTURE  X(4).                    CI0271
            11            GX03-DLAUP  PICTURE  9(8).                    CI0271
            11            GX03-CTWHAT PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-PWHLD  PICTURE  S999V9(5)                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-IWTHH  PICTURE  X.                       CI0271
            11            GX03-NDRFT  PICTURE  9(5).                    CI0271
            11            GX03-IDPAP  PICTURE  X.                       CI0271
            11            GX03-GETIM  PICTURE  S9(7)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-QNACT  PICTURE  9(3).                    CI0271
            11            GX03-AEDRQ  PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-IPLIN  PICTURE  X.                       CI0271
            11            GX03-CLIDNB PICTURE  9(8).                    CI0271
            11            GX03-CSLCT  PICTURE  X.                       CI0271
            11            GX03-ITELE  PICTURE  X.                       CI0271
            11            GX03-FILLER PICTURE  X(06).                   CI0271
            10            GX03-GD02                                     CI0271
                          REDEFINES            GX03-GD09.               CI0271
            11            GX03-CSYST  PICTURE  99.                      CI0271
            11            GX03-FILLER PICTURE  X.                       CI0271
            11            GX03-ACASH  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-DTRAC  PICTURE  9(8).                    CI0271
            11            GX03-CTRSO  PICTURE  9(02).                   CI0271
            11            GX03-NTRCE  PICTURE  9(06).                   CI0271
            11            GX03-GECKD1 PICTURE  9.                       CI0271
            11            GX03-CCOLL  PICTURE  X(3).                    CI0271
            11            GX03-CLTDP  PICTURE  X(3).                    CI0271
            11            GX03-PSLLD  PICTURE  S99V999                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-ISLOR  PICTURE  X.                       CI0271
            11            GX03-ITPAC  PICTURE  X.                       CI0271
            11            GX03-CPMTCA PICTURE  XXX.                     CI0271
            11            GX03-CSERV  PICTURE  X(3).                    CI0271
            11            GX03-ACOMO  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-IPLIN1 PICTURE  X.                       CI0271
            11            GX03-INQEX  PICTURE  X.                       CI0271
            11            GX03-CTKRAA PICTURE  X(12).                   CI0271
            11            GX03-CCSMQ  PICTURE  X.                       CI0271
            11            GX03-IVAEX1 PICTURE  X.                       CI0271
            11            GX03-IHPMT  PICTURE  X(1).                    CI0271
            11            GX03-GETIM3 PICTURE  S9(7)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            GX03-GD03                                     CI0271
                          REDEFINES            GX03-GD09.               CI0271
            11            GX03-CATRNC PICTURE  9(6).                    CI0271
            11            GX03-APRNT1 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-QSHOWT PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-ACINVT PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-ACOMO7 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-QSHOMW PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-ATAXT3 PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-CTSTR  PICTURE  9(2).                    CI0271
            11            GX03-ICIRA  PICTURE  X.                       CI0271
            11            GX03-GETIM2 PICTURE  S9(7)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-CPMTCX PICTURE  XX.                      CI0271
            11            GX03-FILLER PICTURE  X(16).                   CI0271
            10            GX03-GD99.                                    CI0271
            11            GX03-FILLER PICTURE  X(248).                  CI0271
            10            GX03-GD10                                     CI0271
                          REDEFINES            GX03-GD99.               CI0271
            11            GX03-MROTC  PICTURE  X(7).                    CI0271
            11            GX03-CEDSC  PICTURE  9(1).                    CI0271
            11            GX03-ILPOI  PICTURE  X(1).                    CI0271
            11            GX03-AWRCH  PICTURE  S9(3)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-CHCOC1 PICTURE  9(2).                    CI0271
            11            GX03-CHCOC2 PICTURE  9(2).                    CI0271
            11            GX03-CHCOC3 PICTURE  9(2).                    CI0271
            11            GX03-CHCOC4 PICTURE  9(2).                    CI0271
            11            GX03-CMCOC1 PICTURE  9(3).                    CI0271
            11            GX03-CMCOC2 PICTURE  9(3).                    CI0271
            11            GX03-CMCOC3 PICTURE  9(3).                    CI0271
            11            GX03-GD11.                                    CI0271
            12            GX03-FILLER PICTURE  X(219).                  CI0271
            11            GX03-GD12                                     CI0271
                          REDEFINES            GX03-GD11.               CI0271
            12            GX03-CELLO  PICTURE  9(1).                    CI0271
            12            GX03-CECLO  PICTURE  9(1).                    CI0271
            12            GX03-AEXML  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-CEPI   PICTURE  X(1).                    CI0271
            12            GX03-CEXTY  PICTURE  X.                       CI0271
            12            GX03-CROPC  PICTURE  9(1).                    CI0271
            12            GX03-CPUTY  PICTURE  9(1).                    CI0271
            12            GX03-IMCII  PICTURE  X(1).                    CI0271
            12            GX03-GEMISC                                   CI0271
                          OCCURS       010     TIMES.                   CI0271
            13            GX03-AMGLA  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            13            GX03-CMGLC  PICTURE  9(1).                    CI0271
            13            GX03-NMGLN  PICTURE  9(4).                    CI0271
            12            GX03-ACTRN  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-IWRBK  PICTURE  X.                       CI0271
            12            GX03-IFEDX  PICTURE  X.                       CI0271
            12            GX03-ICNTR  PICTURE  X.                       CI0271
            12            GX03-IOCKH  PICTURE  X.                       CI0271
            12            GX03-ICRCK  PICTURE  X.                       CI0271
            12            GX03-NHMPN  PICTURE  S9(10)                   CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-ITELR1 PICTURE  X.                       CI0271
            11            GX03-GD13                                     CI0271
                          REDEFINES            GX03-GD11.               CI0271
            12            GX03-DREDO  PICTURE  9(8).                    CI0271
            12            GX03-CATRNR PICTURE  9(6).                    CI0271
            12            GX03-CEVN   PICTURE  9(9).                    CI0271
            12            GX03-ISUSP  PICTURE  X(1).                    CI0271
            11            GX03-GD15                                     CI0271
                          REDEFINES            GX03-GD11.               CI0271
            12            GX03-CPUTZ  PICTURE  9(1).                    CI0271
            12            GX03-CETLB  PICTURE  9(3).                    CI0271
            12            GX03-QTRMC  PICTURE  9(3).                    CI0271
            12            GX03-DEFFTE PICTURE  9(8).                    CI0271
            12            GX03-DEFFTF PICTURE  9(8).                    CI0271
            12            GX03-DEFFTG PICTURE  9(8).                    CI0271
            12            GX03-XZ1A   PICTURE  X.                       CI0271
            12            GX03-XZ1B   PICTURE  X.                       CI0271
            12            GX03-XZ1C   PICTURE  X.                       CI0271
            12            GX03-XZ1D   PICTURE  X.                       CI0271
            12            GX03-XZ1E   PICTURE  X.                       CI0271
            12            GX03-XZ1F   PICTURE  X.                       CI0271
            12            GX03-XZ1G   PICTURE  X.                       CI0271
            12            GX03-XZ1H   PICTURE  X.                       CI0271
            12            GX03-XZ1I   PICTURE  X.                       CI0271
            12            GX03-DEFFTH PICTURE  9(8).                    CI0271
            11            GX03-GD19                                     CI0271
                          REDEFINES            GX03-GD11.               CI0271
            12            GX03-GD11.                                    CI0271
            13            GX03-FILLER PICTURE  X(219).                  CI0271
            10            GX03-GD20                                     CI0271
                          REDEFINES            GX03-GD99.               CI0271
            11            GX03-ADDACT PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-ISIGV  PICTURE  X.                       CI0271
            11            GX03-IALLF  PICTURE  X.                       CI0271
            11            GX03-QSHOWQ PICTURE  S9(9)V999                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-CCDSCW PICTURE  9(2).                    CI0271
            11            GX03-IDWRL  PICTURE  X.                       CI0271
            11            GX03-ITELR  PICTURE  X.                       CI0271
            11            GX03-IABIN  PICTURE  X.                       CI0271
            11            GX03-PACT1  PICTURE  S999V999                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-IBFAF  PICTURE  X.                       CI0271
            11            GX03-IFRSA  PICTURE  X.                       CI0271
            11            GX03-ICRCAN PICTURE  X.                       CI0271
            11            GX03-ACACTV PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-AGFND  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-QCSHOW PICTURE  S9(9)V999                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-QCSHIS PICTURE  S9(9)V999                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-NDTRC  PICTURE  9(8).                    CI0271
            11            GX03-CAERU  PICTURE  X(4).                    CI0271
            11            GX03-IFDGO  PICTURE  X.                       CI0271
            11            GX03-PSLLD2 PICTURE  S99V999                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-ISLOR2 PICTURE  X.                       CI0271
            11            GX03-QSFIO  PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-QSFID  PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-CGDIN  PICTURE  X.                       CI0271
            11            GX03-DGDIN  PICTURE  9(8).                    CI0271
            10            GX03-GD30                                     CI0271
                          REDEFINES            GX03-GD99.               CI0271
            11            GX03-ISKED  PICTURE  X.                       CI0271
            11            GX03-CENXC  PICTURE  9(2).                    CI0271
            11            GX03-GD31.                                    CI0271
            12            GX03-FILLER PICTURE  X(245).                  CI0271
            11            GX03-GD32                                     CI0271
                          REDEFINES            GX03-GD31.               CI0271
            12            GX03-IABIN1 PICTURE  X.                       CI0271
            12            GX03-CLDOD  PICTURE  9(8).                    CI0271
            12            GX03-NCLAM  PICTURE  9(5)                     CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-ISURR  PICTURE  X.                       CI0271
            12            GX03-GEHCD  PICTURE  9(3).                    CI0271
            12            GX03-CRATC  PICTURE  9(4).                    CI0271
            12            GX03-AMAXD  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-ASCHGA PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-APYOM  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-IWTHH1 PICTURE  X.                       CI0271
            12            GX03-CPAYCL PICTURE  X(2).                    CI0271
            12            GX03-CTSAO  PICTURE  X.                       CI0271
            12            GX03-NCONF  PICTURE  9(08).                   CI0271
            12            GX03-CLID   PICTURE  X(23).                   CI0271
            12            GX03-CARTY  PICTURE  99.                      CI0271
            12            GX03-NARRS  PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-CARTZ  PICTURE  99.                      CI0271
            12            GX03-NAPDS  PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-CPMTO  PICTURE  X.                       CI0271
            12            GX03-DNPMT  PICTURE  9(8).                    CI0271
            12            GX03-IPCTV  PICTURE  X.                       CI0271
            12            GX03-IMECH  PICTURE  X(01).                   CI0271
            12            GX03-IMVAO  PICTURE  X(1).                    CI0271
            12            GX03-AMVA1  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-CACTS  PICTURE  X.                       CI0271
            12            GX03-CTSPP  PICTURE  X(1).                    CI0271
            12            GX03-CACT4  PICTURE  X(2).                    CI0271
            12            GX03-IVAEX  PICTURE  X.                       CI0271
            12            GX03-DFPMT  PICTURE  9(8).                    CI0271
            12            GX03-IDEMD  PICTURE  X.                       CI0271
            12            GX03-IOFST  PICTURE  X.                       CI0271
            12            GX03-AMXLB  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-ACULB  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-DEIRNB PICTURE  9(8).                    CI0271
            12            GX03-DEFFE  PICTURE  9(8).                    CI0271
            12            GX03-DEFFR  PICTURE  9(8).                    CI0271
            12            GX03-ISPUP  PICTURE  X.                       CI0271
            12            GX03-CPNCG  PICTURE  X.                       CI0271
            12            GX03-IEXPU  PICTURE  X.                       CI0271
            12            GX03-IPPCF  PICTURE  X.                       CI0271
            12            GX03-NAAPT  PICTURE  9(2).                    CI0271
            12            GX03-PWHLDS PICTURE  S999V9(5)                CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-ISWHO  PICTURE  X(1).                    CI0271
            11            GX03-GD33                                     CI0271
                          REDEFINES            GX03-GD31.               CI0271
            12            GX03-CPAYC  PICTURE  X(2).                    CI0271
            12            GX03-ADBRQX PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-ADBRQV PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-APTXR  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-CTRTPE PICTURE  X(2).                    CI0271
            12            GX03-NCLAMI PICTURE  S9(9)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-CLIDO8 PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-CLIDN  PICTURE  X(20).                   CI0271
            12            GX03-DSET01 PICTURE  S9(8)                    CI0271
                          BINARY.                                       CI0271
            12            GX03-CTSET1 PICTURE  S9(6)                    CI0271
                          BINARY.                                       CI0271
            12            GX03-DSET02 PICTURE  S9(8)                    CI0271
                          BINARY.                                       CI0271
            12            GX03-CTSET2 PICTURE  S9(6)                    CI0271
                          BINARY.                                       CI0271
            11            GX03-GD34                                     CI0271
                          REDEFINES            GX03-GD31.               CI0271
            12            GX03-QNOFM  PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-CLTRM  PICTURE  99.                      CI0271
            12            GX03-AMXLN  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-ALADJ  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-ACHK   PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-APRMO  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-IMECH1 PICTURE  X(01).                   CI0271
            12            GX03-CACT41 PICTURE  X(2).                    CI0271
            12            GX03-ACDSCC PICTURE  S9(05)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-ACDSCD PICTURE  S9(05)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-GD39                                     CI0271
                          REDEFINES            GX03-GD31.               CI0271
            12            GX03-GD31.                                    CI0271
            13            GX03-FILLER PICTURE  X(245).                  CI0271
            10            GX03-GD40                                     CI0271
                          REDEFINES            GX03-GD99.               CI0271
            11            GX03-NTR    PICTURE  9(8).                    CI0271
            11            GX03-NPBNC  PICTURE  X(24).                   CI0271
            11            GX03-CRREV  PICTURE  X(3).                    CI0271
            11            GX03-CSUSL  PICTURE  S9.                      CI0271
            11            GX03-NMGLN1 PICTURE  9(4).                    CI0271
            11            GX03-DCAC92 PICTURE  9(8).                    CI0271
            11            GX03-NAASQ3 PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-GD49.                                    CI0271
            12            GX03-FILLER PICTURE  X(198).                  CI0271
            11            GX03-GD41                                     CI0271
                          REDEFINES            GX03-GD49.               CI0271
            12            GX03-CRREF  PICTURE  9(2).                    CI0271
            12            GX03-CORIR  PICTURE  X(02).                   CI0271
            12            GX03-CIPDB  PICTURE  X(03).                   CI0271
            12            GX03-CPAYH  PICTURE  X(02).                   CI0271
            12            GX03-NAMEX  PICTURE  9(15)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            GX03-DCHAE  PICTURE  9(4).                    CI0271
            12            GX03-DRQST  PICTURE  S9(8)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-GD42                                     CI0271
                          REDEFINES            GX03-GD49.               CI0271
            12            GX03-CPMTCB PICTURE  X(3).                    CI0271
            10            GX03-GD50                                     CI0271
                          REDEFINES            GX03-GD99.               CI0271
            11            GX03-ALOAD  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-PSLLD4 PICTURE  S99V999                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-CSUSL1 PICTURE  S9.                      CI0271
            11            GX03-CRREV1 PICTURE  X(3).                    CI0271
            11            GX03-ADDAC  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-DL13.                                    CI0271
            12            GX03-GEYR   PICTURE  9(4).                    CI0271
            12            GX03-GEMTH  PICTURE  99.                      CI0271
            12            GX03-NDAY   PICTURE  99.                      CI0271
            11            GX03-NSEQ3P PICTURE  S9(5)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-XZ6A   PICTURE  X(6).                    CI0271
            11            GX03-XZ7    PICTURE  X(7).                    CI0271
            11            GX03-XZ6B   PICTURE  X(6).                    CI0271
            11            GX03-XZ6    PICTURE  X(6).                    CI0271
            11            GX03-XZ6C   PICTURE  X(6).                    CI0271
            11            GX03-XZ20   PICTURE  X(20).                   CI0271
            11            GX03-CATRN1 PICTURE  9(6).                    CI0271
            11            GX03-ADDAC2 PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-ATAXT2 PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-ACOMOT PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-XZ5    PICTURE  X(5).                    CI0271
            11            GX03-IREVD  PICTURE  X(1).                    CI0271
            11            GX03-ISUSP1 PICTURE  X(1).                    CI0271
            11            GX03-XZ6D   PICTURE  X(6).                    CI0271
            11            GX03-XZ13   PICTURE  X(13).                   CI0271
            11            GX03-CWHTP2 PICTURE  X(3).                    CI0271
            11            GX03-CWHTP3 PICTURE  X(3).                    CI0271
            11            GX03-DTREN  PICTURE  9(8).                    CI0271
            11            GX03-NAASQ1 PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            GX03-GD51                                     CI0271
                          REDEFINES            GX03-GD99.               CI0271
            11            GX03-ADOMOT PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-ACGLT  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-ACGST  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-CTXMT  PICTURE  9(2).                    CI0271
            11            GX03-ALOAD3 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-FILLER PICTURE  X(31).                   CI0271
            10            GX03-GD52                                     CI0271
                          REDEFINES            GX03-GD99.               CI0271
            11            GX03-DEFFT5 PICTURE  9(8).                    CI0271
            11            GX03-PSLLD5 PICTURE  S99V999                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-CSUSL2 PICTURE  S9.                      CI0271
            11            GX03-ALOAD2 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-DL22.                                    CI0271
            12            GX03-NYEAR1 PICTURE  9(4).                    CI0271
            12            GX03-GEMTHA PICTURE  99.                      CI0271
            12            GX03-NDAY01 PICTURE  99.                      CI0271
            11            GX03-NSEQ3R PICTURE  S9(5)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-CWHTP  PICTURE  X(3).                    CI0271
            11            GX03-CWHFR  PICTURE  X(3).                    CI0271
            11            GX03-CATRN7 PICTURE  9(6).                    CI0271
            11            GX03-ATAXT5 PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-QSHOT  PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-ACINT3 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-CWHTP1 PICTURE  X(3).                    CI0271
            11            GX03-CWHFR1 PICTURE  X(3).                    CI0271
            11            GX03-ACOMO5 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-QSHOMU PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-ACASH1 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-FILLER PICTURE  X(04).                   CI0271
            11            GX03-CATRN8 PICTURE  9(6).                    CI0271
            11            GX03-ALOAD1 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-PSLLD1 PICTURE  S99V999                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-QSHOT1 PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-ACINT4 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-CSUSL4 PICTURE  S9.                      CI0271
            11            GX03-ACOMO4 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            GX03-GD60                                     CI0271
                          REDEFINES            GX03-GD99.               CI0271
            11            GX03-GEOPDD PICTURE  X(8)                     CI0271
                          OCCURS       005     TIMES.                   CI0271
            11            GX03-DLAUP3 PICTURE  9(8)                     CI0271
                          OCCURS       005     TIMES.                   CI0271
            11            GX03-GEOPDB PICTURE  X(8).                    CI0271
            11            GX03-DLAUP4 PICTURE  9(8).                    CI0271
            11            GX03-ITELR2 PICTURE  X.                       CI0271
            11            GX03-IPMTA  PICTURE  X.                       CI0271
            11            GX03-CCSMG  PICTURE  X.                       CI0271
            11            GX03-CPLEC  PICTURE  XX.                      CI0271
            11            GX03-CORTYA PICTURE  X(3).                    CI0271
            11            GX03-CACTBC PICTURE  X(1).                    CI0271
            11            GX03-CGSPIA PICTURE  X.                       CI0271
            11            GX03-IPTRDA PICTURE  X(01).                   CI0271
            11            GX03-GCUSPY PICTURE  X(12).                   CI0271
            11            GX03-CPALLA PICTURE  X(1).                    CI0271
            11            GX03-QSHO5A PICTURE  S9(9)V999                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-IFRSAB PICTURE  X.                       CI0271
            11            GX03-DELOI  PICTURE  9(8).                    CI0271
            11            GX03-IAROAA PICTURE  X.                       CI0271
            11            GX03-ACINVR PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-ILTINA PICTURE  X.                       CI0271
            11            GX03-ALOIDA PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            GX03-CFUNTA PICTURE  X(2).                    CI0271
            11            GX03-CLGND  PICTURE  X.                       CI0271
            11            GX03-CPH3U  PICTURE  X.                       CI0271
            11            GX03-GESTD  PICTURE  9(8).                    CI0271
            11            GX03-GEEND  PICTURE  9(8).                    CI0271
            11            GX03-CPMTF  PICTURE  99.                      CI0271
            11            GX03-CNAVR  PICTURE  X(1).                    CI0271
            10            GX03-GD70                                     CI0271
                          REDEFINES            GX03-GD99.               CI0271
            11            GX03-CMEMO  PICTURE  X(2).                    CI0271
            11            GX03-ALPLDT PICTURE  9(8).                    CI0271
            11            GX03-CTLPD  PICTURE  9(8).                    CI0271
            11            GX03-CPAYCM PICTURE  X(2).                    CI0271
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
      ****************************************************************
      ****             AUDIT LOG SEGMENT                           ***
      ****************************************************************
      *!WF DSP=VA DSL=VA SEL=8386 FOR=I DES=2 LEV=1
      * PLT=VA
       01                 VA83.                                         CI0271
            10            VA83-K11J.                                    CI0271
            11            VA83-GC01K.                                   CI0271
            12            VA83-C299.                                    CI0271
            13            VA83-CTID.                                    CI0271
            14            VA83-CTIDA  PICTURE  9(3)                     CI0271
                          VALUE                ZERO.                    CI0271
            14            VA83-CTIDN.                                   CI0271
            15            VA83-CTIDNP PICTURE  X(13)                    CI0271
                          VALUE                SPACE.                   CI0271
            15            VA83-CTIDND PICTURE  9(11)                    CI0271
                          VALUE                ZERO.                    CI0271
            10            VA83-GD00.                                    CI0271
            11            VA83-GC03K.                                   CI0271
            12            VA83-DCACG9 PICTURE  9(8)                     CI0271
                          VALUE                ZERO.                    CI0271
            12            VA83-NAASQ  PICTURE  S9(3)                    CI0271
                          VALUE                ZERO                     CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-CAATY  PICTURE  9(3)                     CI0271
                          VALUE                ZERO.                    CI0271
            11            VA83-CVSYS  PICTURE  X(2)                     CI0271
                          VALUE                SPACE.                   CI0271
            11            VA83-CACTO  PICTURE  9(3)                     CI0271
                          VALUE                ZERO.                    CI0271
            11            VA83-CATRN.                                   CI0271
            12            VA83-CATRF  PICTURE  9(3)                     CI0271
                          VALUE                ZERO.                    CI0271
            12            VA83-CATRS  PICTURE  9(3)                     CI0271
                          VALUE                ZERO.                    CI0271
            11            VA83-CASTC  PICTURE  99                       CI0271
                          VALUE                ZERO.                    CI0271
            11            VA83-IPULL  PICTURE  X                        CI0271
                          VALUE                SPACE.                   CI0271
            11            VA83-GEAUN  PICTURE  9(5)                     CI0271
                          VALUE                ZERO.                    CI0271
            11            VA83-GEOPD2 PICTURE  X(8)                     CI0271
                          VALUE                SPACE.                   CI0271
            11            VA83-NBTCH  PICTURE  9(4)                     CI0271
                          VALUE                ZERO.                    CI0271
            11            VA83-DEFFT  PICTURE  9(8)                     CI0271
                          VALUE                ZERO.                    CI0271
            11            VA83-NSUNT  PICTURE  9(4)                     CI0271
                          VALUE                ZERO.                    CI0271
            11            VA83-ITRAN  PICTURE  X                        CI0271
                          VALUE                SPACE.                   CI0271
            11            VA83-DLAUP1 PICTURE  9(8)                     CI0271
                          VALUE                ZERO.                    CI0271
            11            VA83-ADRET  PICTURE  S9(11)V99                CI0271
                          VALUE                ZERO                     CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-TTRMS  PICTURE  X(12)                    CI0271
                          VALUE                SPACE.                   CI0271
            11            VA83-IDELT  PICTURE  X                        CI0271
                          VALUE                SPACE.                   CI0271
            11            VA83-GEOPDM PICTURE  X(8)                     CI0271
                          VALUE                SPACE.                   CI0271
            11            VA83-FILLER PICTURE  X(07)                    CI0271
                          VALUE                SPACE.                   CI0271
            10            VA83-GD09.                                    CI0271
            11            VA83-FILLER PICTURE  X(70)                    CI0271
                          VALUE                SPACE.                   CI0271
            10            VA83-GD01                                     CI0271
                          REDEFINES            VA83-GD09.               CI0271
            11            VA83-ADBRQ  PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-CTRTP  PICTURE  X(2).                    CI0271
            11            VA83-CPORT  PICTURE  X.                       CI0271
            11            VA83-CSCRNU PICTURE  X(4).                    CI0271
            11            VA83-DLAUP  PICTURE  9(8).                    CI0271
            11            VA83-CTWHAT PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-PWHLD  PICTURE  S999V9(5)                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-IWTHH  PICTURE  X.                       CI0271
            11            VA83-NDRFT  PICTURE  9(5).                    CI0271
            11            VA83-IDPAP  PICTURE  X.                       CI0271
            11            VA83-GETIM  PICTURE  S9(7)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-QNACT  PICTURE  9(3).                    CI0271
            11            VA83-AEDRQ  PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-IPLIN  PICTURE  X.                       CI0271
            11            VA83-CLIDNB PICTURE  9(8).                    CI0271
            11            VA83-CSLCT  PICTURE  X.                       CI0271
            11            VA83-ITELE  PICTURE  X.                       CI0271
            11            VA83-FILLER PICTURE  X(06).                   CI0271
            10            VA83-GD02                                     CI0271
                          REDEFINES            VA83-GD09.               CI0271
            11            VA83-CSYST  PICTURE  99.                      CI0271
            11            VA83-FILLER PICTURE  X.                       CI0271
            11            VA83-ACASH  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-DTRAC  PICTURE  9(8).                    CI0271
            11            VA83-CTRSO  PICTURE  9(02).                   CI0271
            11            VA83-NTRCE  PICTURE  9(06).                   CI0271
            11            VA83-GECKD1 PICTURE  9.                       CI0271
            11            VA83-CCOLL  PICTURE  X(3).                    CI0271
            11            VA83-CLTDP  PICTURE  X(3).                    CI0271
            11            VA83-PSLLD  PICTURE  S99V999                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-ISLOR  PICTURE  X.                       CI0271
            11            VA83-ITPAC  PICTURE  X.                       CI0271
            11            VA83-CPMTCA PICTURE  XXX.                     CI0271
            11            VA83-CSERV  PICTURE  X(3).                    CI0271
            11            VA83-ACOMO  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-IPLIN1 PICTURE  X.                       CI0271
            11            VA83-INQEX  PICTURE  X.                       CI0271
            11            VA83-CTKRAA PICTURE  X(12).                   CI0271
            11            VA83-CCSMQ  PICTURE  X.                       CI0271
            11            VA83-IVAEX1 PICTURE  X.                       CI0271
            11            VA83-IHPMT  PICTURE  X(1).                    CI0271
            11            VA83-GETIM3 PICTURE  S9(7)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            VA83-GD03                                     CI0271
                          REDEFINES            VA83-GD09.               CI0271
            11            VA83-CATRNC PICTURE  9(6).                    CI0271
            11            VA83-APRNT1 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-QSHOWT PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-ACINVT PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-ACOMO7 PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-QSHOMW PICTURE  S9(10)V999               CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-ATAXT3 PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-CTSTR  PICTURE  9(2).                    CI0271
            11            VA83-ICIRA  PICTURE  X.                       CI0271
            11            VA83-GETIM2 PICTURE  S9(7)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA83-CPMTCX PICTURE  XX.                      CI0271
            11            VA83-FILLER PICTURE  X(16).                   CI0271
            10            VA83-ITELR  PICTURE  X                        CI0271
                          VALUE                SPACE.                   CI0271
       01                 VA86.                                         CI0271
            10            VA86-K11J.                                    CI0271
            11            VA86-GC01K.                                   CI0271
            12            VA86-C299.                                    CI0271
            13            VA86-CTID.                                    CI0271
            14            VA86-CTIDA  PICTURE  9(3)                     CI0271
                          VALUE                ZERO.                    CI0271
            14            VA86-CTIDN.                                   CI0271
            15            VA86-CTIDNP PICTURE  X(13)                    CI0271
                          VALUE                SPACE.                   CI0271
            15            VA86-CTIDND PICTURE  9(11)                    CI0271
                          VALUE                ZERO.                    CI0271
            10            VA86-GC03K.                                   CI0271
            11            VA86-DCACG9 PICTURE  9(8)                     CI0271
                          VALUE                ZERO.                    CI0271
            11            VA86-NAASQ  PICTURE  S9(3)                    CI0271
                          VALUE                ZERO                     CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            VA86-GE00.                                    CI0271
            11            VA86-GC06K.                                   CI0271
            12            VA86-NPISQ  PICTURE  S9(3)                    CI0271
                          VALUE                ZERO                     CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA86-ACOTD  PICTURE  S9(9)V99                 CI0271
                          VALUE                ZERO                     CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA86-PPOTD  PICTURE  S9(3)V99                 CI0271
                          VALUE                ZERO                     CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA86-QPSTD  PICTURE  S9(7)V999                CI0271
                          VALUE                ZERO                     CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA86-CPITC  PICTURE  99                       CI0271
                          VALUE                ZERO.                    CI0271
            11            VA86-ITRNB  PICTURE  X                        CI0271
                          VALUE                SPACE.                   CI0271
            11            VA86-FILLER PICTURE  X(14)                    CI0271
                          VALUE                SPACE.                   CI0271
            10            VA86-GE98.                                    CI0271
            11            VA86-FILLER PICTURE  X(240)                   CI0271
                          VALUE                SPACE.                   CI0271
            10            VA86-GE10                                     CI0271
                          REDEFINES            VA86-GE98.               CI0271
            11            VA86-CDELI  PICTURE  9(3).                    CI0271
            11            VA86-CPAYC  PICTURE  X(2).                    CI0271
            11            VA86-ICHKP  PICTURE  X.                       CI0271
            11            VA86-CLTIN  PICTURE  9(12).                   CI0271
            11            VA86-IFHAI  PICTURE  X.                       CI0271
            11            VA86-CDQUA  PICTURE  X(2).                    CI0271
            11            VA86-FILLER PICTURE  X(07).                   CI0271
            11            VA86-GE99.                                    CI0271
            12            VA86-FILLER PICTURE  X(212).                  CI0271
            11            VA86-GE01                                     CI0271
                          REDEFINES            VA86-GE99.               CI0271
            12            VA86-NTR    PICTURE  9(8).                    CI0271
            12            VA86-GECKD  PICTURE  9.                       CI0271
            12            VA86-NPBN   PICTURE  X(20).                   CI0271
            12            VA86-CCBAT  PICTURE  99.                      CI0271
            12            VA86-CLID4  PICTURE  X(23).                   CI0271
            12            VA86-GENAL1 PICTURE  X(30)                    CI0271
                          OCCURS       002     TIMES.                   CI0271
            12            VA86-GESAD1 PICTURE  X(30)                    CI0271
                          OCCURS       003     TIMES.                   CI0271
            11            VA86-GE02                                     CI0271
                          REDEFINES            VA86-GE99.               CI0271
            12            VA86-GENAL  PICTURE  X(30)                    CI0271
                          OCCURS       002     TIMES.                   CI0271
            12            VA86-GESAD  PICTURE  X(30)                    CI0271
                          OCCURS       003     TIMES.                   CI0271
            11            VA86-GE03                                     CI0271
                          REDEFINES            VA86-GE99.               CI0271
            12            VA86-NCHKN  PICTURE  9(11).                   CI0271
            11            VA86-GE04                                     CI0271
                          REDEFINES            VA86-GE99.               CI0271
            12            VA86-CTIDAP PICTURE  9(3).                    CI0271
            12            VA86-PRCOD  PICTURE  9(5).                    CI0271
            12            VA86-TDELI  PICTURE  X(30).                   CI0271
            12            VA86-CINCD  PICTURE  9(02).                   CI0271
            10            VA86-GE20                                     CI0271
                          REDEFINES            VA86-GE98.               CI0271
            11            VA86-C299.                                    CI0271
            12            VA86-CTID.                                    CI0271
            13            VA86-CTIDA  PICTURE  9(3).                    CI0271
            13            VA86-CTIDN.                                   CI0271
            14            VA86-CTIDNP PICTURE  X(13).                   CI0271
            14            VA86-CTIDND PICTURE  9(11).                   CI0271
            11            VA86-DCACG9 PICTURE  9(8).                    CI0271
            11            VA86-NAASQ  PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            VA86-CIRAP  PICTURE  XX.                      CI0271
            11            VA86-CTYPE  PICTURE  X.                       CI0271
            11            VA86-INACT  PICTURE  X.                       CI0271
            11            VA86-FILLER PICTURE  X(01).                   CI0271
            11            VA86-ITPAC  PICTURE  X.                       CI0271
            11            VA86-ITAXI  PICTURE  X.                       CI0271
            11            VA86-IOWNC  PICTURE  X.                       CI0271
            11            VA86-CDVCD  PICTURE  X(2).                    CI0271
            11            VA86-CTCUS  PICTURE  999.                     CI0271
            11            VA86-CPMTCB PICTURE  X(3).                    CI0271
            11            VA86-CASTC1 PICTURE  99.                      CI0271
            11            VA86-PRCOD1 PICTURE  9(5).                    CI0271
            11            VA86-CPRSC1 PICTURE  X(9).                    CI0271
            11            VA86-CPRTB  PICTURE  X.                       CI0271
            11            VA86-CBRKD  PICTURE  9(4).                    CI0271
            11            VA86-FILLER PICTURE  X(12).                   CI0271
            10            VA86-GE30                                     CI0271
                          REDEFINES            VA86-GE98.               CI0271
            11            VA86-CFIDC  PICTURE  X(5).                    CI0271
            11            VA86-CPHSE  PICTURE  9(2).                    CI0271
            11            VA86-FILLER PICTURE  X(05).                   CI0271
            11            VA86-IABIN  PICTURE  X.                       CI0271
            11            VA86-PDFND  PICTURE  S999V9(3)                CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            VA86-GE40                                     CI0271
                          REDEFINES            VA86-GE98.               CI0271
            11            VA86-CACCT  PICTURE  X.                       CI0271
            11            VA86-CPAYR  PICTURE  X(2).                    CI0271
            11            VA86-CDELI1 PICTURE  9(3).                    CI0271
            11            VA86-CATRN.                                   CI0271
            12            VA86-CATRF  PICTURE  9(3).                    CI0271
            12            VA86-CATRS  PICTURE  9(3).                    CI0271
            11            VA86-DEFFT  PICTURE  9(8).                    CI0271
            11            VA86-CTYPC  PICTURE  X.                       CI0271
            11            VA86-CIRAPA PICTURE  XX.                      CI0271
            11            VA86-FILLER PICTURE  X(09).                   CI0271
            11            VA86-GE49.                                    CI0271
            12            VA86-FILLER PICTURE  X(208).                  CI0271
            11            VA86-GE41                                     CI0271
                          REDEFINES            VA86-GE49.               CI0271
            12            VA86-NCHKN1 PICTURE  9(6).                    CI0271
            11            VA86-GE42                                     CI0271
                          REDEFINES            VA86-GE49.               CI0271
            12            VA86-CTID1.                                   CI0271
            13            VA86-CTIDA1 PICTURE  9(3).                    CI0271
            13            VA86-CTIDP1 PICTURE  X(13).                   CI0271
            13            VA86-CTIDN1 PICTURE  9(11).                   CI0271
            11            VA86-GE43                                     CI0271
                          REDEFINES            VA86-GE49.               CI0271
            12            VA86-GENAL2 PICTURE  X(30)                    CI0271
                          OCCURS       002     TIMES.                   CI0271
            12            VA86-GESAD2 PICTURE  X(30)                    CI0271
                          OCCURS       003     TIMES.                   CI0271
            11            VA86-GE44                                     CI0271
                          REDEFINES            VA86-GE49.               CI0271
            12            VA86-CTID01.                                  CI0271
            13            VA86-CTIDA6 PICTURE  9(3).                    CI0271
            13            VA86-NTIDP2 PICTURE  X(13).                   CI0271
            13            VA86-CTIDN2 PICTURE  9(11).                   CI0271
            12            VA86-GECKD2 PICTURE  9.                       CI0271
            12            VA86-PACCT  PICTURE  S999V99                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            VA86-PLOAN  PICTURE  S999V99                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            VA86-PADPT  PICTURE  S999V99                  CI0271
                          COMPUTATIONAL-3.                              CI0271
            12            VA86-IPCTL  PICTURE  X.                       CI0271
            12            VA86-IPCTP  PICTURE  X.                       CI0271
            12            VA86-CEUNT  PICTURE  S9(5)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            VA86-GE31                                     CI0271
                          REDEFINES            VA86-GE98.               CI0271
            11            VA86-GCUSPZ PICTURE  X(12).                   CI0271
      *INPUT DATE "NOT" IN 9'S COMP FORMAT
      *!WI
       01  7-INPUT-DCACG
                        PICTURE 9(8).                                   CI0271
      *
      *   CONFIGURATION SWITCHES
      *
       01  GC03-CF                        PIC X  VALUE '0'.
       01  GC04-CF                        PIC X  VALUE '0'.
       01  GC06-CF                        PIC X  VALUE '0'.
      *VALID ACTIVITY ORIGINATOR CODE.
      *CATS ONLINE - 001, CV - 014, MYFA - 021.
      *!WI
       01  WS00-CACTO
                        PICTURE 9(3).                                   CI0271
           88  VALID-CACTO       VALUES 001 014 021 .
       01   DEBUT-WSS.                                                  CI0271
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0271
            05   IK     PICTURE X.                                      CI0271
       01  CONSTANTES-PAC.                                              CI0271
           05  FILLER  PICTURE X(87)   VALUE                            CI0271
                     '6015 CAT09/08/14CI0271ADMIN   14:35:08CI0271P AMERCI0271
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0271
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0271
           05  NUGNA   PICTURE X(5).                                    CI0271
           05  APPLI   PICTURE X(3).                                    CI0271
           05  DATGN   PICTURE X(8).                                    CI0271
           05  PROGR   PICTURE X(6).                                    CI0271
           05  CODUTI  PICTURE X(8).                                    CI0271
           05  TIMGN   PICTURE X(8).                                    CI0271
           05  PROGE   PICTURE X(8).                                    CI0271
           05  COBASE  PICTURE X(4).                                    CI0271
           05  DATGNC  PICTURE X(10).                                   CI0271
           05  RELEAS  PICTURE X(7).                                    CI0271
           05  DATGE   PICTURE X(10).                                   CI0271
           05  DATSQ   PICTURE X(10).                                   CI0271
       01  DATCE.                                                       CI0271
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0271
         05  DATOR.                                                     CI0271
           10  DATOA  PICTURE XX.                                       CI0271
           10  DATOM  PICTURE XX.                                       CI0271
           10  DATOJ  PICTURE XX.                                       CI0271
       01   VARIABLES-CONDITIONNELLES.                                  CI0271
            05                  FT      PICTURE X VALUE '0'.            CI0271
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0271
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0271
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0271
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0271
            05       5-GC00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0271
            05       5-GX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0271
       01               S-CT01-SSA.                                     CI0271
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0271
                                      VALUE 'GC01    '.                 CI0271
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0271
            10          S-CT01-CCOD   PICTURE X(5)                      CI0271
                                      VALUE '-----'.                    CI0271
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0271
       01            S-CTU01-SSA.                                       CI0271
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC01    '.                 CI0271
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0271
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(GC01K'.                   CI0271
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0271
            10       S-CTU01-GC01K.                                     CI0271
            11       S-CTU01-C299.                                      CI0271
            12       S-CTU01-CTID.                                      CI0271
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0271
            13       S-CTU01-CTIDN.                                     CI0271
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0271
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0271
            10  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01               S-GC01-SSA.                                     CI0271
            10         S1-GC01-SEGNAM PICTURE X(8)                      CI0271
                                      VALUE 'GC01    '.                 CI0271
            10         S1-GC01-CCOM   PICTURE X VALUE '*'.              CI0271
            10          S-GC01-CCOD   PICTURE X(5)                      CI0271
                                      VALUE '-----'.                    CI0271
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0271
       01            S-GCU01-SSA.                                       CI0271
            10      S1-GCU01-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC01    '.                 CI0271
            10      S1-GCU01-CCOM   PICTURE X VALUE '*'.                CI0271
            10       S-GCU01-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            10      S1-GCU01-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(GC01K'.                   CI0271
            10       S-GCU01-OPER  PICTURE XX VALUE ' ='.               CI0271
            10       S-GCU01-GC01K.                                     CI0271
            11       S-GCU01-C299.                                      CI0271
            12       S-GCU01-CTID.                                      CI0271
            13       S-GCU01-CTIDA    PICTURE  9(3).                    CI0271
            13       S-GCU01-CTIDN.                                     CI0271
            14       S-GCU01-CTIDNP   PICTURE  X(13).                   CI0271
            14       S-GCU01-CTIDND   PICTURE  9(11).                   CI0271
            10  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01               S-GC03-SSA.                                     CI0271
            10         S1-GC03-SEGNAM PICTURE X(8)                      CI0271
                                      VALUE 'GC03    '.                 CI0271
            10         S1-GC03-CCOM   PICTURE X VALUE '*'.              CI0271
            10          S-GC03-CCOD   PICTURE X(5)                      CI0271
                                      VALUE '-----'.                    CI0271
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0271
       01            S-GCA03-SSA.                                       CI0271
            11      S1-GCA03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GCA03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCA03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCA03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CAATY'.                   CI0271
            11       S-GCA03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCA03-CAATY    PICTURE  9(3).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GCB03-SSA.                                       CI0271
            11      S1-GCB03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GCB03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCB03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCB03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CVSYS'.                   CI0271
            11       S-GCB03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCB03-CVSYS    PICTURE  X(2).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GCC03-SSA.                                       CI0271
            11      S1-GCC03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GCC03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCC03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCC03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CASTC'.                   CI0271
            11       S-GCC03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCC03-CASTC    PICTURE  99.                      CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GCD03-SSA.                                       CI0271
            11      S1-GCD03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GCD03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCD03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCD03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CACTO'.                   CI0271
            11       S-GCD03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCD03-CACTO    PICTURE  9(3).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GCE03-SSA.                                       CI0271
            11      S1-GCE03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GCE03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCE03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCE03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(IPULL'.                   CI0271
            11       S-GCE03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCE03-IPULL    PICTURE  X.                       CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GCF03-SSA.                                       CI0271
            11      S1-GCF03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GCF03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCF03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCF03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(DTRAC'.                   CI0271
            11       S-GCF03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCF03-DTRAC    PICTURE  9(8).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GCG03-SSA.                                       CI0271
            11      S1-GCG03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GCG03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCG03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCG03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CTRSO'.                   CI0271
            11       S-GCG03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCG03-CTRSO    PICTURE  9(02).                   CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GCH03-SSA.                                       CI0271
            11      S1-GCH03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GCH03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCH03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCH03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(NTRCE'.                   CI0271
            11       S-GCH03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCH03-NTRCE    PICTURE  9(06).                   CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GCI03-SSA.                                       CI0271
            11      S1-GCI03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GCI03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCI03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCI03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(ITRAN'.                   CI0271
            11       S-GCI03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCI03-ITRAN    PICTURE  X.                       CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GCJ03-SSA.                                       CI0271
            11      S1-GCJ03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GCJ03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCJ03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCJ03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(DEFFT'.                   CI0271
            11       S-GCJ03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCJ03-DEFFT    PICTURE  9(8).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GCK03-SSA.                                       CI0271
            11      S1-GCK03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GCK03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCK03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCK03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CPMTCA'.                  CI0271
            11       S-GCK03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCK03-CPMTCA   PICTURE  XXX.                     CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GCL03-SSA.                                       CI0271
            11      S1-GCL03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GCL03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCL03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCL03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(ACASH'.                   CI0271
            11       S-GCL03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCL03-ACASH    PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GCN03-SSA.                                       CI0271
            11      S1-GCN03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GCN03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCN03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCN03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CRREV'.                   CI0271
            11       S-GCN03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCN03-CRREV    PICTURE  X(3).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GCO03-SSA.                                       CI0271
            11      S1-GCO03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GCO03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCO03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCO03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CSYST'.                   CI0271
            11       S-GCO03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCO03-CSYST    PICTURE  99.                      CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GCU03-SSA.                                       CI0271
            11      S1-GCU03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GCU03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCU03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCU03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(GC03K'.                   CI0271
            11       S-GCU03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCU03-GC03K.                                     CI0271
            12       S-GCU03-DCACG9   PICTURE  9(8).                    CI0271
            12       S-GCU03-NAASQ    PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GC103-SSA.                                       CI0271
            12      S1-GC103-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            12      S1-GC103-CCOM   PICTURE X VALUE '*'.                CI0271
            12       S-GC103-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            12      S1-GC103-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(XDCACG9'.                 CI0271
            12       S-GC103-OPER  PICTURE XX VALUE ' ='.               CI0271
            12       S-GC103-DCACG9   PICTURE  9(8).                    CI0271
            12  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GC203-SSA.                                       CI0271
            11      S1-GC203-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GC203-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GC203-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GC203-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(XGEAUN'.                  CI0271
            11       S-GC203-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GC203-GEAUN    PICTURE  9(5).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GC303-SSA.                                       CI0271
            11      S1-GC303-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GC303-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GC303-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GC303-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(XGEOPD2'.                 CI0271
            11       S-GC303-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GC303-GEOPD2   PICTURE  X(8).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GC403-SSA.                                       CI0271
            11      S1-GC403-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GC403-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GC403-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GC403-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(XNBTCH'.                  CI0271
            11       S-GC403-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GC403-NBTCH    PICTURE  9(4).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GC803-SSA.                                       CI0271
            12      S1-GC803-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            12      S1-GC803-CCOM   PICTURE X VALUE '*'.                CI0271
            12       S-GC803-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            12      S1-GC803-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(XNAASQ'.                  CI0271
            12       S-GC803-OPER  PICTURE XX VALUE ' ='.               CI0271
            12       S-GC803-NAASQ    PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            12  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01               S-GC04-SSA.                                     CI0271
            10         S1-GC04-SEGNAM PICTURE X(8)                      CI0271
                                      VALUE 'GC04    '.                 CI0271
            10         S1-GC04-CCOM   PICTURE X VALUE '*'.              CI0271
            10          S-GC04-CCOD   PICTURE X(5)                      CI0271
                                      VALUE '-----'.                    CI0271
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0271
       01               S-GC06-SSA.                                     CI0271
            10         S1-GC06-SEGNAM PICTURE X(8)                      CI0271
                                      VALUE 'GC06    '.                 CI0271
            10         S1-GC06-CCOM   PICTURE X VALUE '*'.              CI0271
            10          S-GC06-CCOD   PICTURE X(5)                      CI0271
                                      VALUE '-----'.                    CI0271
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0271
       01            S-GCF06-SSA.                                       CI0271
            11      S1-GCF06-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC06    '.                 CI0271
            11      S1-GCF06-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCF06-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCF06-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(PRCOD1'.                  CI0271
            11       S-GCF06-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCF06-PRCOD1   PICTURE  9(5).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GCU06-SSA.                                       CI0271
            11      S1-GCU06-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC06    '.                 CI0271
            11      S1-GCU06-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GCU06-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GCU06-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(GC06K'.                   CI0271
            11       S-GCU06-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GCU06-GC06K.                                     CI0271
            12       S-GCU06-NPISQ    PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01               S-GX01-SSA.                                     CI0271
            10         S1-GX01-SEGNAM PICTURE X(8)                      CI0271
                                      VALUE 'GC01    '.                 CI0271
            10         S1-GX01-CCOM   PICTURE X VALUE '*'.              CI0271
            10          S-GX01-CCOD   PICTURE X(5)                      CI0271
                                      VALUE '-----'.                    CI0271
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0271
       01            S-GXU01-SSA.                                       CI0271
            10      S1-GXU01-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC01    '.                 CI0271
            10      S1-GXU01-CCOM   PICTURE X VALUE '*'.                CI0271
            10       S-GXU01-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            10      S1-GXU01-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(GC01K'.                   CI0271
            10       S-GXU01-OPER  PICTURE XX VALUE ' ='.               CI0271
            10       S-GXU01-GC01K.                                     CI0271
            11       S-GXU01-C299.                                      CI0271
            12       S-GXU01-CTID.                                      CI0271
            13       S-GXU01-CTIDA    PICTURE  9(3).                    CI0271
            13       S-GXU01-CTIDN.                                     CI0271
            14       S-GXU01-CTIDNP   PICTURE  X(13).                   CI0271
            14       S-GXU01-CTIDND   PICTURE  9(11).                   CI0271
            10  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01               S-GX03-SSA.                                     CI0271
            10         S1-GX03-SEGNAM PICTURE X(8)                      CI0271
                                      VALUE 'GC03    '.                 CI0271
            10         S1-GX03-CCOM   PICTURE X VALUE '*'.              CI0271
            10          S-GX03-CCOD   PICTURE X(5)                      CI0271
                                      VALUE '-----'.                    CI0271
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0271
       01            S-GXA03-SSA.                                       CI0271
            11      S1-GXA03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GXA03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GXA03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GXA03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CAATY'.                   CI0271
            11       S-GXA03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GXA03-CAATY    PICTURE  9(3).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GXB03-SSA.                                       CI0271
            11      S1-GXB03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GXB03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GXB03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GXB03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CVSYS'.                   CI0271
            11       S-GXB03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GXB03-CVSYS    PICTURE  X(2).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GXC03-SSA.                                       CI0271
            11      S1-GXC03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GXC03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GXC03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GXC03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CASTC'.                   CI0271
            11       S-GXC03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GXC03-CASTC    PICTURE  99.                      CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GXD03-SSA.                                       CI0271
            11      S1-GXD03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GXD03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GXD03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GXD03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CACTO'.                   CI0271
            11       S-GXD03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GXD03-CACTO    PICTURE  9(3).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GXE03-SSA.                                       CI0271
            11      S1-GXE03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GXE03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GXE03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GXE03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(IPULL'.                   CI0271
            11       S-GXE03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GXE03-IPULL    PICTURE  X.                       CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GXF03-SSA.                                       CI0271
            11      S1-GXF03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GXF03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GXF03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GXF03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(DTRAC'.                   CI0271
            11       S-GXF03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GXF03-DTRAC    PICTURE  9(8).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GXG03-SSA.                                       CI0271
            11      S1-GXG03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GXG03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GXG03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GXG03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CTRSO'.                   CI0271
            11       S-GXG03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GXG03-CTRSO    PICTURE  9(02).                   CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GXH03-SSA.                                       CI0271
            11      S1-GXH03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GXH03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GXH03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GXH03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(NTRCE'.                   CI0271
            11       S-GXH03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GXH03-NTRCE    PICTURE  9(06).                   CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GXI03-SSA.                                       CI0271
            11      S1-GXI03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GXI03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GXI03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GXI03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(ITRAN'.                   CI0271
            11       S-GXI03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GXI03-ITRAN    PICTURE  X.                       CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GXJ03-SSA.                                       CI0271
            11      S1-GXJ03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GXJ03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GXJ03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GXJ03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(DEFFT'.                   CI0271
            11       S-GXJ03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GXJ03-DEFFT    PICTURE  9(8).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GXK03-SSA.                                       CI0271
            11      S1-GXK03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GXK03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GXK03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GXK03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CPMTCA'.                  CI0271
            11       S-GXK03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GXK03-CPMTCA   PICTURE  XXX.                     CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GXL03-SSA.                                       CI0271
            11      S1-GXL03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GXL03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GXL03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GXL03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(ACASH'.                   CI0271
            11       S-GXL03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GXL03-ACASH    PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GXN03-SSA.                                       CI0271
            11      S1-GXN03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GXN03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GXN03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GXN03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CRREV'.                   CI0271
            11       S-GXN03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GXN03-CRREV    PICTURE  X(3).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GXO03-SSA.                                       CI0271
            11      S1-GXO03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GXO03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GXO03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GXO03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(CSYST'.                   CI0271
            11       S-GXO03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GXO03-CSYST    PICTURE  99.                      CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GXU03-SSA.                                       CI0271
            11      S1-GXU03-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GXU03-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GXU03-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GXU03-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(GC03K'.                   CI0271
            11       S-GXU03-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GXU03-GC03K.                                     CI0271
            12       S-GXU03-DCACG9   PICTURE  9(8).                    CI0271
            12       S-GXU03-NAASQ    PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GX103-SSA.                                       CI0271
            12      S1-GX103-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            12      S1-GX103-CCOM   PICTURE X VALUE '*'.                CI0271
            12       S-GX103-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            12      S1-GX103-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(XDCACG9'.                 CI0271
            12       S-GX103-OPER  PICTURE XX VALUE ' ='.               CI0271
            12       S-GX103-DCACG9   PICTURE  9(8).                    CI0271
            12  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GX203-SSA.                                       CI0271
            11      S1-GX203-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GX203-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GX203-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GX203-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(XGEAUN'.                  CI0271
            11       S-GX203-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GX203-GEAUN    PICTURE  9(5).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GX303-SSA.                                       CI0271
            11      S1-GX303-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GX303-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GX303-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GX303-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(XGEOPD2'.                 CI0271
            11       S-GX303-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GX303-GEOPD2   PICTURE  X(8).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GX403-SSA.                                       CI0271
            11      S1-GX403-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            11      S1-GX403-CCOM   PICTURE X VALUE '*'.                CI0271
            11       S-GX403-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            11      S1-GX403-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(XNBTCH'.                  CI0271
            11       S-GX403-OPER  PICTURE XX VALUE ' ='.               CI0271
            11       S-GX403-NBTCH    PICTURE  9(4).                    CI0271
            11  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01            S-GX803-SSA.                                       CI0271
            12      S1-GX803-SEGNAM PICTURE X(8)                        CI0271
                                      VALUE 'GC03    '.                 CI0271
            12      S1-GX803-CCOM   PICTURE X VALUE '*'.                CI0271
            12       S-GX803-CCOD   PICTURE X(5)                        CI0271
                                      VALUE '-----'.                    CI0271
            12      S1-GX803-FLDNAM PICTURE X(9)                        CI0271
                                      VALUE '(XNAASQ'.                  CI0271
            12       S-GX803-OPER  PICTURE XX VALUE ' ='.               CI0271
            12       S-GX803-NAASQ    PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            12  FILLER   PICTURE X    VALUE ')'.                        CI0271
       01   ZONES-UTILISATEUR PICTURE X.                                CI0271
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
      *PCB POINTER FOR ACAP 2ND PCB
            05 PCB-ACAP-PTR2        POINTER.
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR ACAP                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0271
          05              PB00-SUITE.                                   CI0271
            15       FILLER         PICTURE  X(00106).                  CI0271
       01                 PB06  REDEFINES      PB00.                    CI0271
            10            PB06-XDBPCB.                                  CI0271
            11            PB06-XDBDNM PICTURE  X(08).                   CI0271
            11            PB06-XSEGLV PICTURE  X(02).                   CI0271
            11            PB06-XRC    PICTURE  X(02).                   CI0271
            11            PB06-XPROPT PICTURE  X(04).                   CI0271
            11            PB06-FILLER PICTURE  S9(5)                    CI0271
                          BINARY.                                       CI0271
            11            PB06-XSEGNM PICTURE  X(08).                   CI0271
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0271
                          BINARY.                                       CI0271
            11            PB06-XSEGNB PICTURE  9(05)                    CI0271
                          BINARY.                                       CI0271
            11            PB06-XCOKEY PICTURE  X(70).                   CI0271
      *** PCB MASK FOR ACAP                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0271
          05              PC00-SUITE.                                   CI0271
            15       FILLER         PICTURE  X(00106).                  CI0271
       01                 PC06  REDEFINES      PC00.                    CI0271
            10            PC06-XDBPCB.                                  CI0271
            11            PC06-XDBDNM PICTURE  X(08).                   CI0271
            11            PC06-XSEGLV PICTURE  X(02).                   CI0271
            11            PC06-XRC    PICTURE  X(02).                   CI0271
            11            PC06-XPROPT PICTURE  X(04).                   CI0271
            11            PC06-FILLER PICTURE  S9(5)                    CI0271
                          BINARY.                                       CI0271
            11            PC06-XSEGNM PICTURE  X(08).                   CI0271
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0271
                          BINARY.                                       CI0271
            11            PC06-XSEGNB PICTURE  9(05)                    CI0271
                          BINARY.                                       CI0271
            11            PC06-XCOKEY PICTURE  X(70).                   CI0271
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PF DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PF00.                                         CI0271
          05              PF00-SUITE.                                   CI0271
            15       FILLER         PICTURE  X(00106).                  CI0271
       01                 PF06  REDEFINES      PF00.                    CI0271
            10            PF06-XDBPCB.                                  CI0271
            11            PF06-XDBDNM PICTURE  X(08).                   CI0271
            11            PF06-XSEGLV PICTURE  X(02).                   CI0271
            11            PF06-XRC    PICTURE  X(02).                   CI0271
            11            PF06-XPROPT PICTURE  X(04).                   CI0271
            11            PF06-FILLER PICTURE  S9(5)                    CI0271
                          BINARY.                                       CI0271
            11            PF06-XSEGNM PICTURE  X(08).                   CI0271
            11            PF06-XKEYLN PICTURE  S9(05)                   CI0271
                          BINARY.                                       CI0271
            11            PF06-XSEGNB PICTURE  9(05)                    CI0271
                          BINARY.                                       CI0271
            11            PF06-XCOKEY PICTURE  X(70).                   CI0271
      ******************************************************************
      **              PASS AREA TO CIMF25
      ******************************************************************
      *!WF DSP=LK DSL=V1 SEL=70 FOR=I DES=1 LEV=1 PLT=75
       01                 LK70.                                         CI0271
            10            LK70-C299.                                    CI0271
            11            LK70-CTID.                                    CI0271
            12            LK70-CTIDA  PICTURE  9(3).                    CI0271
            12            LK70-CTIDN.                                   CI0271
            13            LK70-CTIDNP PICTURE  X(13).                   CI0271
            13            LK70-CTIDND PICTURE  9(11).                   CI0271
            10            LK70-GECKD  PICTURE  9.                       CI0271
            10            LK70-ICUST  PICTURE  X.                       CI0271
            10            LK70-PRCOD  PICTURE  9(5).                    CI0271
            10            LK70-PRSCD  PICTURE  X(9).                    CI0271
            10            LK70-DCACG9 PICTURE  9(8).                    CI0271
            10            LK70-NAASQ  PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-CAATY  PICTURE  9(3).                    CI0271
            10            LK70-CACTO  PICTURE  9(3).                    CI0271
            10            LK70-CASTC  PICTURE  99.                      CI0271
            10            LK70-ITRAN  PICTURE  X.                       CI0271
            10            LK70-GEAUN  PICTURE  9(5).                    CI0271
            10            LK70-GEOPD2 PICTURE  X(8).                    CI0271
            10            LK70-DEFFT  PICTURE  9(8).                    CI0271
            10            LK70-CTRTP  PICTURE  X(2).                    CI0271
            10            LK70-CTWHAT PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-PWHLD  PICTURE  S999V9(5)                CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-GETIM  PICTURE  S9(7)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-ADBRQA PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-IWTHH  PICTURE  X.                       CI0271
            10            LK70-CLCUS  PICTURE  99.                      CI0271
            10            LK70-CCACT  PICTURE  99.                      CI0271
            10            LK70-AFEET  PICTURE  S9(5)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-ITERF  PICTURE  X.                       CI0271
            10            LK70-ATERF  PICTURE  S9(5)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-CLDOB  PICTURE  9(8).                    CI0271
            10            LK70-CPLTYP PICTURE  X(14).                   CI0271
            10            LK70-IACFPD PICTURE  X(1).                    CI0271
            10            LK70-CDELI  PICTURE  9(3).                    CI0271
            10            LK70-CPAYC  PICTURE  X(2).                    CI0271
            10            LK70-ACOTD  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-NPBN   PICTURE  X(20).                   CI0271
            10            LK70-CCBAT  PICTURE  99.                      CI0271
            10            LK70-CLID4.                                   CI0271
            11            LK70-CLIDA  PICTURE  9(3).                    CI0271
            11            LK70-CLIDNP PICTURE  X(12).                   CI0271
            11            LK70-CLIDNA PICTURE  9(8).                    CI0271
            10            LK70-GENAL1 PICTURE  X(30).                   CI0271
            10            LK70-GENAL2 PICTURE  X(30).                   CI0271
            10            LK70-GESAD1 PICTURE  X(30).                   CI0271
            10            LK70-GESAD2 PICTURE  X(30).                   CI0271
            10            LK70-GESAD3 PICTURE  X(30).                   CI0271
            10            LK70-NTR    PICTURE  9(8).                    CI0271
            10            LK70-GECKD1 PICTURE  9.                       CI0271
            10            LK70-IMQMG  PICTURE  X.                       CI0271
            10            LK70-NIPAD  PICTURE  X(15).                   CI0271
            10            LK70-CLNAM.                                   CI0271
            11            LK70-CLNAMH PICTURE  X(6).                    CI0271
            11            LK70-CLNAMF PICTURE  X(20).                   CI0271
            11            LK70-CLNAMI PICTURE  X.                       CI0271
            11            LK70-CLNAMR PICTURE  X(14).                   CI0271
            11            LK70-CLNAML PICTURE  X(25).                   CI0271
            11            LK70-CLNAMS PICTURE  X(4).                    CI0271
            10            LK70-CSLCT  PICTURE  X.                       CI0271
            10            LK70-C199.                                    CI0271
            11            LK70-CLID.                                    CI0271
            12            LK70-CLIDO  PICTURE  9(3).                    CI0271
            12            LK70-CLIDN.                                   CI0271
            13            LK70-CLIDNP PICTURE  X(12).                   CI0271
            13            LK70-CLIDND PICTURE  9(8).                    CI0271
            10            LK70-GECKD2 PICTURE  9.                       CI0271
            10            LK70-CPROCM PICTURE  X.                       CI0271
            10            LK70-NAASQL PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-NSEQ4B PICTURE  9(8)                     CI0271
                          BINARY.                                       CI0271
            10            LK70-CLTIN  PICTURE  9(12).                   CI0271
            10            LK70-IPULL  PICTURE  X.                       CI0271
            10            LK70-NBTCH  PICTURE  9(4).                    CI0271
            10            LK70-CVSYS  PICTURE  X(2).                    CI0271
            10            LK70-NPISQ  PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-CPITC  PICTURE  99.                      CI0271
            10            LK70-PPOTD  PICTURE  S9(3)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-ITRNB  PICTURE  X.                       CI0271
            10            LK70-CTTLN1 PICTURE  X(30).                   CI0271
            10            LK70-CTTLN2 PICTURE  X(30).                   CI0271
            10            LK70-CTTLN3 PICTURE  X(30).                   CI0271
            10            LK70-CTTBO1 PICTURE  X(45).                   CI0271
            10            LK70-CTTBO2 PICTURE  X(45).                   CI0271
            10            LK70-PRCMN  PICTURE  X(20).                   CI0271
            10            LK70-TTRTP  PICTURE  X(30).                   CI0271
            10            LK70-DXTMSA PICTURE  X(26).                   CI0271
            10            LK70-DXTMS2 PICTURE  X(26).                   CI0271
            10            LK70-CUPIQ  PICTURE  X.                       CI0271
            10            LK70-IQACT  PICTURE  X.                       CI0271
            10            LK70-MAPPN  PICTURE  X(10).                   CI0271
            10            LK70-CTTYPG PICTURE  X(04).                   CI0271
            10            LK70-CLORN  PICTURE  X(45).                   CI0271
            10            LK70-APMTL  PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-MPMTFL PICTURE  X(24).                   CI0271
            10            LK70-ANETTQ PICTURE  S9(9)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-TTBAL  PICTURE  X(15).                   CI0271
            10            LK70-MPRN4  PICTURE  X(35).                   CI0271
            10            LK70-CCONF  PICTURE  X(25).                   CI0271
            10            LK70-DCACG  PICTURE  9(8).                    CI0271
            10            LK70-NMESA  PICTURE  9(6).                    CI0271
            10            LK70-MCSIG  PICTURE  X(30).                   CI0271
            10            LK70-IERRC  PICTURE  X.                       CI0271
            10            LK70-AVLMN  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-AVLMX  PICTURE  S9(7)V99                 CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-AVCSH  PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-ACVALM PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-INDRS  PICTURE  X.                       CI0271
            10            LK70-GRID   PICTURE  X(13).                   CI0271
            10            LK70-AACTV  PICTURE  S9(11)V99                CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-CTCUS  PICTURE  999.                     CI0271
            10            LK70-CCDSCW PICTURE  9(2).                    CI0271
            10            LK70-CHCR   PICTURE  99.                      CI0271
            10            LK70-IOWNG  PICTURE  X.                       CI0271
            10            LK70-GECSQ  PICTURE  S9(3)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            LK70-CQACT  PICTURE  999.                     CI0271
            10            LK70-CTOWN  PICTURE  9(3).                    CI0271
            10            LK70-NGEOR  PICTURE  9(08).                   CI0271
            10            LK70-FILLER PICTURE  X(85).                   CI0271
      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I DES=1 LEV=1 PLT=85
       01                 DE10.                                         CI0271
            10            DE10-DU11.                                    CI0271
            11            DE10-XFONC  PICTURE  X(4).                    CI0271
            11            DE10-MPSBN  PICTURE  X(8).                    CI0271
            11            DE10-XDBDNM PICTURE  X(08).                   CI0271
            11            DE10-XSEGNM PICTURE  X(08).                   CI0271
            11            DE10-XRC    PICTURE  X(02).                   CI0271
            11            DE10-MSEG   PICTURE  X(08).                   CI0271
            11            DE10-XCOKEY PICTURE  X(70).                   CI0271
            11            DE10-CUIBR  PICTURE  X(01).                   CI0271
            11            DE10-CUIBA  PICTURE  X(01).                   CI0271
            11            DE10-IPBIK  PICTURE  X(1).                    CI0271
            10            DE10-DU03.                                    CI0271
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            DE10-CMSSF  PICTURE  XX.                      CI0271
            11            DE10-DU09.                                    CI0271
            12            DE10-CMESA  PICTURE  S9(9)                    CI0271
                          BINARY.                                       CI0271
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0271
                          BINARY.                                       CI0271
            12            DE10-CMESB  PICTURE  S9(9)                    CI0271
                          BINARY.                                       CI0271
            12            DE10-CMSST  PICTURE  S9(9)                    CI0271
                          BINARY.                                       CI0271
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0271
                          BINARY.                                       CI0271
            12            DE10-QELLAA PICTURE  S9(9)                    CI0271
                          BINARY.                                       CI0271
            12            DE10-TMESS4 PICTURE  X(512).                  CI0271
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0271
          05              MS00-SUITE.                                   CI0271
            15       FILLER         PICTURE  X(00542).                  CI0271
       01                 MS03  REDEFINES      MS00.                    CI0271
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            10            MS03-CMSSF  PICTURE  XX.                      CI0271
            10            MS03-DU09.                                    CI0271
            11            MS03-CMESA  PICTURE  S9(9)                    CI0271
                          BINARY.                                       CI0271
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0271
                          BINARY.                                       CI0271
            11            MS03-CMESB  PICTURE  S9(9)                    CI0271
                          BINARY.                                       CI0271
            11            MS03-CMSST  PICTURE  S9(9)                    CI0271
                          BINARY.                                       CI0271
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0271
                          BINARY.                                       CI0271
            11            MS03-QELLAA PICTURE  S9(9)                    CI0271
                          BINARY.                                       CI0271
            11            MS03-TMESS4 PICTURE  X(512).                  CI0271
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0271
            10            MX11-QMSGS  PICTURE  9(03).                   CI0271
            10            MX11-PJ09                                     CI0271
                          OCCURS       025     TIMES.                   CI0271
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0271
                          COMPUTATIONAL-3.                              CI0271
            11            MX11-CMESB  PICTURE  S9(9)                    CI0271
                          BINARY.                                       CI0271
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                LK70
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0271
      *               *                                   *             CI0271
      *               *INITIALISATIONS                    *             CI0271
      *               *                                   *             CI0271
      *               *************************************.            CI0271
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
      *N02DA.    NOTE *INITIALIZE LINKAGE SEGMENST        *.
       F02DA.                                                           lv10
      ****************************
           INITIALIZE  DE10
           MS03.
       F02DA-FN. EXIT.
       F02XA.                                                           lv10
      *SET ADDRESS FOR ACAP                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-ACAP-PTR1.                                          ADU015
      *SET ADDRESS FOR ACAP 2ND PCB                                     DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-ACAP-PTR2.
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PF06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0271
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0271
      *               *                                   *             CI0271
      *               *FIN DE TRAITEMENT                  *             CI0271
      *               *                                   *             CI0271
      *               *************************************.            CI0271
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0271
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATE INPUT PARMS               *
      *               *                                   *
      *               *************************************.
       F35.                                                             lv05
      *
      *********************************
      **  ENSURE PARMS HAVE THE       *
      **  CORRECT CONTENTS BASED ON   *
      **  FIELD CLASS AND CONTENTS    *
      *********************************
      *N35BB.    NOTE *VALIDATE SYSTEM RECOGNIZED         *.
       F35BB.    IF    LK70-MAPPN NOT = 'EFU'                           lv10
                 NEXT SENTENCE ELSE GO TO     F35BB-FN.
      *---> Send BAD SOURCE Message                                     ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012734 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35BB-FN. EXIT.
      *N35CB.    NOTE *VALIDATE NUMERIC CTID              *.
       F35CB.    IF    LK70-CTID NOT NUMERIC                            lv10
                 AND   LK70-CTID NOT > ZEROES
                 NEXT SENTENCE ELSE GO TO     F35CB-FN.
      *---> Send INVALID DATA Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012232 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35CB-FN. EXIT.
      *N35DB.    NOTE *VALIDATE NUMERIC CURRENT DATE      *.
       F35DB.    IF    LK70-DCACG9 NOT NUMERIC                          lv10
                 AND   LK70-DCACG9 NOT > ZEROES
                 NEXT SENTENCE ELSE GO TO     F35DB-FN.
      *---> Send INVALID DATA Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012531 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35DB-FN. EXIT.
      *N35EB.    NOTE *VALIDATE ACCT ACTIVITY SEQ # IS    *.
       F35EB.    IF    LK70-NAASQ NOT NUMERIC                           lv10
                 AND   LK70-NAASQ NOT > ZEROES
                 NEXT SENTENCE ELSE GO TO     F35EB-FN.
      *NUMERIC
      *---> Send INVALID DATA Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012449 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35EB-FN. EXIT.
      *N35FB.    NOTE *VALIDATE SEQUENCE NUMBER NUMERIC   *.
       F35FB.    IF    LK70-NPISQ NOT NUMERIC                           lv10
                 AND   LK70-NPISQ NOT > ZEROES
                 NEXT SENTENCE ELSE GO TO     F35FB-FN.
      *---> Send BAD DEST. SEQ Message                                  ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012821 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35FB-FN. EXIT.
       F35-FN.   EXIT.
      *N38.      NOTE *************************************.
      *               *                                   *
      *               *ACF2 CALL                          *
      *               *                                   *
      *               *************************************.
       F38.           EXIT.                                             lv05
      *N38BA.    NOTE *CALL ACF EXIT MODULE               *.            ADU031
       F38BA.                                                           lv10
           EXEC CICS   LINK PROGRAM (ACF-PROG)                          ADU031
                       COMMAREA (ACF-USER-AREA)                         ADU031
                       LENGTH (ACF-AREA-LEN)                 END-EXEC.  ADU031
       F38BA-FN. EXIT.
       F38-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *********************************   *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      ******MAIN PROCESSING************
      *THIS IS THE MAIN PROCESSING
      *FUNCTION FOR DELETING THE FA
      *CLIENT CERTS ACH-OUT & INTERNAL
      *TRANSFER TRANSACTIONS. ALL THE
      *KEY PARAMETERS HAVE BEEN
      *VALIDATED EARLIER.
      *********************************
      *N40BB.    NOTE *READ GC03                          *.
       F40BB.                                                           lv10
      *********************************
           INITIALIZE  GC03-CF
           MOVE        LK70-CTID TO S-GCU01-GC01K
           MOVE        LK70-DCACG9 TO S-GCU03-DCACG9
           SUBTRACT    LK70-DCACG9 FROM 99999999
           GIVING 7-INPUT-DCACG
           MOVE        LK70-NAASQ TO S-GCU03-NAASQ.
      *THE FOLLOWING READ PERFORMS A                                    DOT
      *SIMPLE GU (NO HOLD) SO THE
      *DATABASE SEGMENTS ARE LOCKED FOR
      *THE SHORTEST TIME POSSIBLE.
           PERFORM     F94GC THRU F94GC-FN.
                 IF    IK = '1'                                         DOT
      *GC03 NOT FOUND
      *---> Send MISSING ACTVITY Message                                ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013411 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *MOVE ORIGINATOR CODE                                             DOT
           MOVE        GC03-CACTO TO WS00-CACTO.
      *N40BD.    NOTE *ACTIVITY STATUS NOT UNPROCESSED    *.
       F40BD.    IF    GC03-CASTC NOT = 01                              lv15
                 NEXT SENTENCE ELSE GO TO     F40BD-FN.
      *(I.E. NOT PENDING)
      *---> Send BAD STATUS Message                                     ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013352 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40BD-FN. EXIT.
      *N40BE.    NOTE *IF THIS ACTIVITY ORIGINATED IN     *.
       F40BE.    IF    VALID-CACTO                                      lv15
                 AND   GC03-CTRTP = 'S'
                 AND   GC03-CPORT = 'P'
                 NEXT SENTENCE ELSE GO TO     F40BE-FN.
      *CATS ONLINE (001), CLIENT VIEWER
      *(014) AND MYFA(021) AND IT IS
      *A PARTIAL SURRENDER TRANS
       F40BE-900. GO TO F40BG-FN.
       F40BE-FN. EXIT.
      *N40BG.    NOTE *THIS ACTIVITY ORIGINATED           *.
       F40BG.                                                           lv15
      *IN A SYSTEM FOR WHICH FA
      *CANNOT PROCESS THE DELETE.
      *---> Send INVALID SYSTEM Message                                 ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015224 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40BG-FN. EXIT.
      *N40BM.    NOTE *GC03 FOUND - LOG GC03 'BEFORE'     *.
       F40BM.                                                           lv15
      *ON DELETE
           INITIALIZE  VA83
           MOVE        70080 TO DH10-CAUFR
           MOVE        00004 TO DH10-CAUAC
      *
      *FILL IN VA83 (GC01K)
           MOVE        LK70-CTID TO VA83-GC01K
      *FILL IN GC03 DATA
           MOVE        GC03-GD00 TO VA83-GD00
           MOVE        GC03-GD01 TO VA83-GD01
           MOVE        GC03-ITELR2 TO VA83-ITELR
           MOVE        VA83 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F40BM-FN. EXIT.
      *N40CF.    NOTE *READ THRU ALL THE GC06'S UNDER     *.
       F40CF.                                                           lv15
      *THIS GC03 SEGMENT
      *DELETE ALL RELATED COLLECTION
      *ACTIVITY RECORDS IF IT IS A
      *TRANSFER TRAN.
      *******************************
           PERFORM     F90 THRU F90-FN.
       F40CF-FN. EXIT.
      *N40CK.    NOTE *RETRIEVE (GHU) GC03 FOR DELETION   *.
       F40CK.                                                           lv15
      *********************************
           MOVE        LK70-CTID TO S-GCU01-CTID
           MOVE        LK70-DCACG9 TO S-GCU03-DCACG9
           MOVE        LK70-NAASQ TO S-GCU03-NAASQ
      *GHU GC03
           PERFORM     F94GK THRU F94GK-FN.
                 IF    IK = '1'                                         DOT
      *GC03 NOT FOUND
      *---> Send BAD GHU ON GC03 Message                                ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013331 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40CK-FN. EXIT.
      *N40DB.    NOTE *DELETE GC03 SEGMENT AND CHECK IF   *.
       F40DB.                                                           lv15
      *OTHER GC03 EXIST.IF NOT, DELETE
      *GC01.
      *********************************
      *DLET GC03
           PERFORM     F94GO THRU F94GO-FN.
                 IF    IK = '1'                                         DOT
      *ERROR ON DLET OF GC03
      *---> Send BAD DLET ON GC03 Message                               ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013333 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N40EB.    NOTE *CHECK FOR OTHER GC03 SEGMENTS      *.
       F40EB.                                                           lv20
      *********************************
      *READ FOR FIRST GC03
           MOVE        LK70-CTID TO S-GCU01-GC01K
      *SET THE COMMAND CODE FOR FIRST
           MOVE        'F----' TO S-GC03-CCOD
      *NOW, PERFORM GN ON GC03
           PERFORM     F94GP THRU F94GP-FN
      *RESET THE COMMAND CODE
           MOVE        '-----' TO S-GC03-CCOD.
      *N40EG.    NOTE *IF NO OTHER GC03 FOUND, DELETE     *.
       F40EG.    IF    IK = '1'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F40EG-FN.
      *GC01 SEGMENT
           MOVE        LK70-CTID TO S-GCU01-GC01K
      *GHU GC01
           PERFORM     F94GM THRU F94GM-FN.
                 IF    IK = '1'                                         DOT
      *GC01 NOT FOUND
      *---> Send BAD GHU ON GC01 Message                                ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013334 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N40EL.    NOTE *IF FOUND, DELETE GC01 SEG          *.
       F40EL.                                                           lv30
      *********************************
      *PERFORM DELETE OF GC01
           PERFORM     F94GQ THRU F94GQ-FN
      *
                 IF    IK = '1'                                         DOT
      *ERROR ON DLET OF GC01
      *---> Send BAD DLET ON GC01 Message                               ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013335 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40EL-FN. EXIT.
       F40EG-900. GO TO F40ET-FN.
       F40EG-FN. EXIT.
      *N40ET.    NOTE *ANOTHER GC03 FOUND.                *.
       F40ET.                                                           lv25
      *UPDATE SEQUENCE NUMBER ON GC01
      *
           MOVE        LK70-CTID TO S-GCU01-GC01K
      *GHU GC01
           PERFORM     F94GM THRU F94GM-FN.
                 IF    IK = '1'                                         DOT
      *GC01 NOT FOUND
      *---> Send BAD GHU ON GC01 Message                                ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013334 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N40EV.    NOTE *IF GC01 FOUND, UPDATE GC01         *.
       F40EV.                                                           lv30
      *SEQUENCE NUMBER.
      *ADD 1 TO THE SEQUENCE NUMBER
           ADD         1 TO GC01-NSEQ4B
      *PERFORM REPL ON GC01
           PERFORM     F94GN THRU F94GN-FN.
                 IF    IK = '1'                                         DOT
      *ERROR ON REPL OF GC01
      *---> Send BAD REPL ON GC01 Message                               ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013217 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40EV-FN. EXIT.
       F40ET-FN. EXIT.
       F40EB-FN. EXIT.
       F40DB-FN. EXIT.
       F40BB-FN. EXIT.
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
      *N90.      NOTE *************************************.
      *               *                                   *
      *               *IF GC06 SEGMENT FOUND WRITE TO     *
      *               *                                   *
      *               *************************************.
       F90.                                                             lv05
      *AUDIT LOG
      *N90BA.    NOTE *PRIME FOR LOOPING                  *.
       F90BA.                                                           lv10
      *********************************
           MOVE        '1' TO GC06-CF
      *THIS WILL INSURE THE FIRST GC06
      *IS RETRIEVED.
           MOVE        LK70-CTID TO S-GCU01-CTID
           MOVE        LK70-DCACG9 TO S-GCU03-DCACG9
           MOVE        LK70-NAASQ TO S-GCU03-NAASQ
           MOVE        'F----' TO S-GC06-CCOD.
       F90BA-FN. EXIT.
      *N90BB.    NOTE *READ THRU GC06'S                   *.
       F90BB.                       GO TO     F90BB-B.                  lv10
       F90BB-A.
                 IF    GC06-CF = '0'
                                    GO TO     F90BB-FN.
       F90BB-B.
      *********************************
           PERFORM     F94GD THRU F94GD-FN
      *RESET THE COMMAND CODE
           MOVE        '-----' TO S-GC06-CCOD.
                 IF    IK = '0'                                         DOT
      *GC06 FOUND
           MOVE        '1' TO GC06-CF
                 ELSE
      *GC06 NOT FOUND
           MOVE        '0' TO GC06-CF.
      *N90BD.    NOTE *GC06 FOUND - UPDATE AUDIT LOG      *.
       F90BD.    IF    GC06-CF = '1'                                    lv15
                 NEXT SENTENCE ELSE GO TO     F90BD-FN.
      *N90BF.    NOTE *LOG GC06 'BEFORE' ON DEL OR CHG    *.
       F90BF.    IF    GC06-NPISQ = LK70-NPISQ                          lv20
                 NEXT SENTENCE ELSE GO TO     F90BF-FN.
      *NOTE: GC06 IS DELETED BY VIRTUE
      *OF GC03 BEING DELETED
           MOVE        70081 TO DH10-CAUFR
           MOVE        00004 TO DH10-CAUAC
      *.
      *FILL IN VA86-K11J (GC01K)                                        DOT
           MOVE        LK70-CTID TO VA86-GC01K
      *
      *FILL IN GC03K
           MOVE        GC03-GC03K TO VA86-GC03K
      *
      *FILL IN GC06 DATA
           MOVE        GC06-GE00 TO VA86-GE00
           MOVE        GC06-GE01 TO VA86-GE01
      *
           MOVE        VA86 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F90BF-FN. EXIT.
      *N90BL.    NOTE *DEST IS AN AEFA ACCOUNT            *.
       F90BL.    IF    GC06-CPITC = 02                                  lv20
                 NEXT SENTENCE ELSE GO TO     F90BL-FN.
      *N90BP.    NOTE *IF THE GC06-NPISQ IS NOT EQUAL     *.
       F90BP.    IF    GC06-NPISQ NOT = LK70-NPISQ                      lv25
                 NEXT SENTENCE ELSE GO TO     F90BP-FN.
      *THE INPUT NPISQ, RETURN ERROR
      *AND TERMINATE
      *THIS WAS ADDED BECAUSE FUNDS UD
      *DISBURSEMENTS ARE NOT SUPPOSED
      *TO BE SPLIT.  SO IF MORE THAN
      *ONE EXISTS, TR CANNOT BE USED
      *TO DELETE THIS DISBURSEMENT.
      *---> Send WRONG SEQUENCE # Message                               ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013350 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F90BP-FN. EXIT.
      *N90BT.    NOTE *LOAD KEY FOR COLLECTION ACCOUNT    *.
       F90BT.                                                           lv25
      *USE INFO FROM EACH GC06 SEGMENT
      *UNDER THE DISBURSEMENT TO UPDATE
      *ANY RELATED COLLECTION ACCOUNTS.
           MOVE        GC06-CTID TO S-GXU01-CTID
           MOVE        GC06-DCACG9 TO S-GXU03-DCACG9
           MOVE        GC06-NAASQ TO S-GXU03-NAASQ
      *GHU GX03
           PERFORM     F94GE THRU F94GE-FN.
      *N90CB.    NOTE *GX03 NOT FOUND                     *.
       F90CB.    IF    IK = '1'                                         lv30
                 NEXT SENTENCE ELSE GO TO     F90CB-FN.
      *---> Send MISSING COLL REC Message                               ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013342 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F90CB-900. GO TO F90CN-FN.
       F90CB-FN. EXIT.
      *N90CN.    NOTE *GX03 FOUND (GC03 FOR COLLECTION)   *.
       F90CN.                                                           lv30
      *PERFORM THE DELETE OF THE GX03
           PERFORM     F94GF THRU F94GF-FN.
      *N90CP.    NOTE *ERR ON DLET OF GX03                *.
       F90CP.    IF    IK = '1'                                         lv35
                 NEXT SENTENCE ELSE GO TO     F90CP-FN.
      *---> Send BAD DLET ON GX03 Message                               ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013344 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F90CP-900. GO TO F90DB-FN.
       F90CP-FN. EXIT.
      *N90DB.    NOTE *IF GX03 SUCCESSFULLY DELETED,      *.
       F90DB.                                                           lv35
      *READ FOR FIRST OF REMAINING GX03
      *SEGMENTS TO SEE IF OTHERS EXIST
      *FOR THIS COLLECTION ACCOUNT.
      *SET UP FOR READ FIRST ----
           MOVE        GC06-CTID TO S-GXU01-CTID
           MOVE        'F----' TO S-GX03-CCOD
      *READ FOR THE FIRST GX03
           PERFORM     F94GG THRU F94GG-FN
      *RESET THE COMMAND CODE
           MOVE        '-----' TO S-GX03-CCOD.
      *N90DN.    NOTE *IF BAD RETURN, DELETE THE GX01     *.
       F90DN.    IF    IK = '1'                                         lv40
                 NEXT SENTENCE ELSE GO TO     F90DN-FN.
      *BECAUSE NO MORE GX03 SEGMENTS
      *REMAIN UNDER THIS COLLECTION
      *ACCOUNT.
      *PERFORM GET HOLD ON GX01
           MOVE        GC06-CTID TO S-GXU01-CTID
           PERFORM     F94GL THRU F94GL-FN.
                 IF    IK = '1'                                         DOT
      *GX01 NOT FOUND
      *---> Send BAD GHU ON GX01 Message                                ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013341 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN                              ADU119
                 ELSE
      *GX01 FOUND; DELETE IT
           PERFORM     F94GJ THRU F94GJ-FN.
                 IF    IK = '1'                                         DOT
      *DLET OF GX01 FAILED
      *---> Send BAD DLET ON GX01 Message                               ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013343 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F90DN-FN. EXIT.
       F90DB-FN. EXIT.
       F90CN-FN. EXIT.
       F90BT-FN. EXIT.
       F90BL-FN. EXIT.
       F90BD-FN. EXIT.
       F90BB-900. GO TO F90BB-A.
       F90BB-FN. EXIT.
       F90-FN.   EXIT.
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
      *N94CT.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94CT.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PF06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CT-FN. EXIT.
      *N94GC.    NOTE *CALL GU ON GC03                    *.            ADU026
       F94GC.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 GC03                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GC-FN. EXIT.
      *N94GD.    NOTE *CALL GN ON GC06                    *.            ADU026
       F94GD.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PB06 GC06                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA S-GC06-SSA                           ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GD-FN. EXIT.
      *N94GE.    NOTE *CALL GHU ON GX03                   *.            ADU026
       F94GE.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PC06 GX03                                                    ADU026
           S-GXU01-SSA S-GXU03-SSA                                      ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GE-FN. EXIT.
      *N94GF.    NOTE *CALL DLET ON GX03                  *.            ADU026
       F94GF.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XDLET                       ADU026
           PC06 GX03                                                    ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XDLET TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GF-FN. EXIT.
      *N94GG.    NOTE *CALL GN ON GX03                    *.            ADU026
       F94GG.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PC06 GX03                                                    ADU026
           S-GXU01-SSA S-GX03-SSA                                       ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GG-FN. EXIT.
      *N94GH.    NOTE *CALL GN ON GC03                    *.            ADU026
       F94GH.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PB06 GC03                                                    ADU026
           S-GCU01-SSA S-GC03-SSA                                       ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GH-FN. EXIT.
      *N94GI.    NOTE *CALL GHU ON GC01                   *.            ADU026
       F94GI.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PB06 GC01                                                    ADU026
           S-GCU01-SSA                                                  ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GI-FN. EXIT.
      *N94GJ.    NOTE *CALL DLET ON GX01                  *.            ADU026
       F94GJ.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XDLET                       ADU026
           PC06 GX01                                                    ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XDLET TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GJ-FN. EXIT.
      *N94GK.    NOTE *CALL GHU ON GC03                   *.            ADU026
       F94GK.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PB06 GC03                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GK-FN. EXIT.
      *N94GL.    NOTE *CALL GHU ON GX01                   *.            ADU026
       F94GL.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PC06 GX01                                                    ADU026
           S-GXU01-SSA                                                  ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GL-FN. EXIT.
      *N94GM.    NOTE *CALL GHU ON GC01                   *.            ADU026
       F94GM.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PB06 GC01                                                    ADU026
           S-GCU01-SSA                                                  ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GM-FN. EXIT.
      *N94GN.    NOTE *CALL REPL ON GC01                  *.            ADU026
       F94GN.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PB06 GC01                                                    ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GN-FN. EXIT.
      *N94GO.    NOTE *CALL DLET ON GC03                  *.            ADU026
       F94GO.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XDLET                       ADU026
           PB06 GC03                                                    ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XDLET TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GO-FN. EXIT.
      *N94GP.    NOTE *CALL GN ON GC03                    *.            ADU026
       F94GP.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PB06 GC03                                                    ADU026
           S-GCU01-SSA S-GC03-SSA                                       ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GP-FN. EXIT.
      *N94GQ.    NOTE *CALL DLET ON GC01                  *.            ADU026
       F94GQ.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XDLET                       ADU026
           PB06 GC01                                                    ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XDLET TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GQ-FN. EXIT.
       F94-FN.   EXIT.
      *N96.      NOTE *************************************.
      *               *                                   *
      *               *AUDIT LOG PROCESSING               *
      *               *                                   *
      *               *************************************.
       F96.           EXIT.                                             lv05
      *N96AL.    NOTE *---> Audit Log Process             *.            ADU165
       F96AL.         EXIT.                                             lv10
      *N96AN.    NOTE *---> Format Audit Log Data         *.            ADU165
       F96AN.                                                           lv15
           SET AL00-NPNTR                                               ADU165
           TO ADDRESS OF DLIUIBII                                       ADU165
           MOVE        AL00-ADDR TO DH10-XUIBP                          ADU165
           MOVE        AL00-NSEQ2P TO DH10-NSEQ2P                       ADU165
           MOVE        'E' TO DH10-CAUL                                 ADU165
           MOVE        'CONTRACT' TO DH10-MAUSB                         ADU165
           MOVE        LK70-CTID TO DH10-NAUSK                          ADU165
           MOVE        'FA' TO DH10-CSYS                                ADU165
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
