       IDENTIFICATION DIVISION.                                         CI0112
       PROGRAM-ID.  CI0112P.                                            CI0112
      *AUTHOR.         UD ACTIVITY DATABASE UPDATE.                     CI0112
      *DATE-COMPILED.   09/08/14.                                       CI0112
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
       ENVIRONMENT DIVISION.                                            CI0112
       CONFIGURATION SECTION.                                           CI0112
       SOURCE-COMPUTER. IBM-370.                                        CI0112
       OBJECT-COMPUTER. IBM-370.                                        CI0112
       DATA DIVISION.                                                   CI0112
       WORKING-STORAGE SECTION.                                         CI0112
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
                        PICTURE S9(3)                                   CI0112
                          COMPUTATIONAL-3.                              CI0112
                                                                        ADU165
      *>>>>>>> Linkage Area for Logger Program DBI110                   ADU165
      *!WF DSP=DH DSL=DH SEL=10 FOR=I DES=2 LEV=1                       ADU165
       01                 DH10.                                         CI0112
            10            DH10-GERTC  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            DH10-XUIBP  PICTURE  S9(8)                    CI0112
                          VALUE                ZERO                     CI0112
                          BINARY.                                       CI0112
            10            DH10-NSEQ2P PICTURE  S9(3)                    CI0112
                          VALUE                ZERO                     CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            DH10-CAUL   PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            DH10-MAUSB  PICTURE  X(8)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            DH10-NAUSK  PICTURE  X(50)                    CI0112
                          VALUE                SPACE.                   CI0112
            10            DH10-CSYS   PICTURE  X(4)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            DH10-CAPPL  PICTURE  X(8)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            DH10-CAUSR  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            DH10-CAUFR  PICTURE  S9(5)                    CI0112
                          VALUE                ZERO                     CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            DH10-CAUAC  PICTURE  S9(5)                    CI0112
                          VALUE                ZERO                     CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            DH10-GEOPID PICTURE  X(6)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            DH10-CAUNIT PICTURE  X(4)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            DH10-GAUVR  PICTURE  X(400)                   CI0112
                          VALUE                SPACE.                   CI0112

      *AUDIT LOG - ACTIVITY DETAIL (GC03) & DEST INSTRUCTIONS (GC06)
      *GET MOVED TO GAUVR
      *!WF DSP=VA DSL=VA SEL=8386 FOR=I DES=1 LEV=1
      * PLT=AM
       01                 VA83.                                         CI0112
            10            VA83-K11J.                                    CI0112
            11            VA83-GC01K.                                   CI0112
            12            VA83-C299.                                    CI0112
            13            VA83-CTID.                                    CI0112
            14            VA83-CTIDA  PICTURE  9(3).                    CI0112
            14            VA83-CTIDN.                                   CI0112
            15            VA83-CTIDNP PICTURE  X(13).                   CI0112
            15            VA83-CTIDND PICTURE  9(11).                   CI0112
            10            VA83-GD00.                                    CI0112
            11            VA83-GC03K.                                   CI0112
            12            VA83-DCACG9 PICTURE  9(8).                    CI0112
            12            VA83-NAASQ  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-CAATY  PICTURE  9(3).                    CI0112
            11            VA83-CVSYS  PICTURE  X(2).                    CI0112
            11            VA83-CACTO  PICTURE  9(3).                    CI0112
            11            VA83-CATRN.                                   CI0112
            12            VA83-CATRF  PICTURE  9(3).                    CI0112
            12            VA83-CATRS  PICTURE  9(3).                    CI0112
            11            VA83-CASTC  PICTURE  99.                      CI0112
            11            VA83-IPULL  PICTURE  X.                       CI0112
            11            VA83-GEAUN  PICTURE  9(5).                    CI0112
            11            VA83-GEOPD2 PICTURE  X(8).                    CI0112
            11            VA83-NBTCH  PICTURE  9(4).                    CI0112
            11            VA83-DEFFT  PICTURE  9(8).                    CI0112
            11            VA83-NSUNT  PICTURE  9(4).                    CI0112
            11            VA83-ITRAN  PICTURE  X.                       CI0112
            11            VA83-DLAUP1 PICTURE  9(8).                    CI0112
            11            VA83-ADRET  PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-TTRMS  PICTURE  X(12).                   CI0112
            11            VA83-IDELT  PICTURE  X.                       CI0112
            11            VA83-GEOPDM PICTURE  X(8).                    CI0112
            11            VA83-FILLER PICTURE  X(07).                   CI0112
            10            VA83-GD09.                                    CI0112
            11            VA83-FILLER PICTURE  X(70).                   CI0112
            10            VA83-GD01                                     CI0112
                          REDEFINES            VA83-GD09.               CI0112
            11            VA83-ADBRQ  PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-CTRTP  PICTURE  X(2).                    CI0112
            11            VA83-CPORT  PICTURE  X.                       CI0112
            11            VA83-CSCRNU PICTURE  X(4).                    CI0112
            11            VA83-DLAUP  PICTURE  9(8).                    CI0112
            11            VA83-CTWHAT PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-PWHLD  PICTURE  S999V9(5)                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-IWTHH  PICTURE  X.                       CI0112
            11            VA83-NDRFT  PICTURE  9(5).                    CI0112
            11            VA83-IDPAP  PICTURE  X.                       CI0112
            11            VA83-GETIM  PICTURE  S9(7)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-QNACT  PICTURE  9(3).                    CI0112
            11            VA83-AEDRQ  PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-IPLIN  PICTURE  X.                       CI0112
            11            VA83-CLIDNB PICTURE  9(8).                    CI0112
            11            VA83-CSLCT  PICTURE  X.                       CI0112
            11            VA83-ITELE  PICTURE  X.                       CI0112
            11            VA83-FILLER PICTURE  X(06).                   CI0112
            10            VA83-GD02                                     CI0112
                          REDEFINES            VA83-GD09.               CI0112
            11            VA83-CSYST  PICTURE  99.                      CI0112
            11            VA83-FILLER PICTURE  X.                       CI0112
            11            VA83-ACASH  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-DTRAC  PICTURE  9(8).                    CI0112
            11            VA83-CTRSO  PICTURE  9(02).                   CI0112
            11            VA83-NTRCE  PICTURE  9(06).                   CI0112
            11            VA83-GECKD1 PICTURE  9.                       CI0112
            11            VA83-CCOLL  PICTURE  X(3).                    CI0112
            11            VA83-CLTDP  PICTURE  X(3).                    CI0112
            11            VA83-PSLLD  PICTURE  S99V999                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-ISLOR  PICTURE  X.                       CI0112
            11            VA83-ITPAC  PICTURE  X.                       CI0112
            11            VA83-CPMTCA PICTURE  XXX.                     CI0112
            11            VA83-CSERV  PICTURE  X(3).                    CI0112
            11            VA83-ACOMO  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-IPLIN1 PICTURE  X.                       CI0112
            11            VA83-INQEX  PICTURE  X.                       CI0112
            11            VA83-CTKRAA PICTURE  X(12).                   CI0112
            11            VA83-CCSMQ  PICTURE  X.                       CI0112
            11            VA83-IVAEX1 PICTURE  X.                       CI0112
            11            VA83-IHPMT  PICTURE  X(1).                    CI0112
            11            VA83-GETIM3 PICTURE  S9(7)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            VA83-GD03                                     CI0112
                          REDEFINES            VA83-GD09.               CI0112
            11            VA83-CATRNC PICTURE  9(6).                    CI0112
            11            VA83-APRNT1 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-QSHOWT PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-ACINVT PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-ACOMO7 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-QSHOMW PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-ATAXT3 PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-CTSTR  PICTURE  9(2).                    CI0112
            11            VA83-ICIRA  PICTURE  X.                       CI0112
            11            VA83-GETIM2 PICTURE  S9(7)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA83-CPMTCX PICTURE  XX.                      CI0112
            11            VA83-FILLER PICTURE  X(16).                   CI0112
            10            VA83-ITELR  PICTURE  X.                       CI0112
       01                 VA86.                                         CI0112
            10            VA86-K11J.                                    CI0112
            11            VA86-GC01K.                                   CI0112
            12            VA86-C299.                                    CI0112
            13            VA86-CTID.                                    CI0112
            14            VA86-CTIDA  PICTURE  9(3).                    CI0112
            14            VA86-CTIDN.                                   CI0112
            15            VA86-CTIDNP PICTURE  X(13).                   CI0112
            15            VA86-CTIDND PICTURE  9(11).                   CI0112
            10            VA86-GC03K.                                   CI0112
            11            VA86-DCACG9 PICTURE  9(8).                    CI0112
            11            VA86-NAASQ  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            VA86-GE00.                                    CI0112
            11            VA86-GC06K.                                   CI0112
            12            VA86-NPISQ  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA86-ACOTD  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA86-PPOTD  PICTURE  S9(3)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA86-QPSTD  PICTURE  S9(7)V999                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA86-CPITC  PICTURE  99.                      CI0112
            11            VA86-ITRNB  PICTURE  X.                       CI0112
            11            VA86-FILLER PICTURE  X(14).                   CI0112
            10            VA86-GE98.                                    CI0112
            11            VA86-FILLER PICTURE  X(240).                  CI0112
            10            VA86-GE10                                     CI0112
                          REDEFINES            VA86-GE98.               CI0112
            11            VA86-CDELI  PICTURE  9(3).                    CI0112
            11            VA86-CPAYC  PICTURE  X(2).                    CI0112
            11            VA86-ICHKP  PICTURE  X.                       CI0112
            11            VA86-CLTIN  PICTURE  9(12).                   CI0112
            11            VA86-IFHAI  PICTURE  X.                       CI0112
            11            VA86-CDQUA  PICTURE  X(2).                    CI0112
            11            VA86-FILLER PICTURE  X(07).                   CI0112
            11            VA86-GE99.                                    CI0112
            12            VA86-FILLER PICTURE  X(212).                  CI0112
            11            VA86-GE01                                     CI0112
                          REDEFINES            VA86-GE99.               CI0112
            12            VA86-NTR    PICTURE  9(8).                    CI0112
            12            VA86-GECKD  PICTURE  9.                       CI0112
            12            VA86-NPBN   PICTURE  X(20).                   CI0112
            12            VA86-CCBAT  PICTURE  99.                      CI0112
            12            VA86-CLID4  PICTURE  X(23).                   CI0112
            12            VA86-GENAL1 PICTURE  X(30)                    CI0112
                          OCCURS       002     TIMES.                   CI0112
            12            VA86-GESAD1 PICTURE  X(30)                    CI0112
                          OCCURS       003     TIMES.                   CI0112
            11            VA86-GE02                                     CI0112
                          REDEFINES            VA86-GE99.               CI0112
            12            VA86-GENAL  PICTURE  X(30)                    CI0112
                          OCCURS       002     TIMES.                   CI0112
            12            VA86-GESAD  PICTURE  X(30)                    CI0112
                          OCCURS       003     TIMES.                   CI0112
            11            VA86-GE03                                     CI0112
                          REDEFINES            VA86-GE99.               CI0112
            12            VA86-NCHKN  PICTURE  9(11).                   CI0112
            11            VA86-GE04                                     CI0112
                          REDEFINES            VA86-GE99.               CI0112
            12            VA86-CTIDAP PICTURE  9(3).                    CI0112
            12            VA86-PRCOD  PICTURE  9(5).                    CI0112
            12            VA86-TDELI  PICTURE  X(30).                   CI0112
            12            VA86-CINCD  PICTURE  9(02).                   CI0112
            10            VA86-GE20                                     CI0112
                          REDEFINES            VA86-GE98.               CI0112
            11            VA86-C299.                                    CI0112
            12            VA86-CTID.                                    CI0112
            13            VA86-CTIDA  PICTURE  9(3).                    CI0112
            13            VA86-CTIDN.                                   CI0112
            14            VA86-CTIDNP PICTURE  X(13).                   CI0112
            14            VA86-CTIDND PICTURE  9(11).                   CI0112
            11            VA86-DCACG9 PICTURE  9(8).                    CI0112
            11            VA86-NAASQ  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            VA86-CIRAP  PICTURE  XX.                      CI0112
            11            VA86-CTYPE  PICTURE  X.                       CI0112
            11            VA86-INACT  PICTURE  X.                       CI0112
            11            VA86-FILLER PICTURE  X(01).                   CI0112
            11            VA86-ITPAC  PICTURE  X.                       CI0112
            11            VA86-ITAXI  PICTURE  X.                       CI0112
            11            VA86-IOWNC  PICTURE  X.                       CI0112
            11            VA86-CDVCD  PICTURE  X(2).                    CI0112
            11            VA86-CTCUS  PICTURE  999.                     CI0112
            11            VA86-CPMTCB PICTURE  X(3).                    CI0112
            11            VA86-CASTC1 PICTURE  99.                      CI0112
            11            VA86-PRCOD1 PICTURE  9(5).                    CI0112
            11            VA86-CPRSC1 PICTURE  X(9).                    CI0112
            11            VA86-CPRTB  PICTURE  X.                       CI0112
            11            VA86-CBRKD  PICTURE  9(4).                    CI0112
            11            VA86-FILLER PICTURE  X(12).                   CI0112
            10            VA86-GE30                                     CI0112
                          REDEFINES            VA86-GE98.               CI0112
            11            VA86-CFIDC  PICTURE  X(5).                    CI0112
            11            VA86-CPHSE  PICTURE  9(2).                    CI0112
            11            VA86-FILLER PICTURE  X(05).                   CI0112
            11            VA86-IABIN  PICTURE  X.                       CI0112
            11            VA86-PDFND  PICTURE  S999V9(3)                CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            VA86-GE40                                     CI0112
                          REDEFINES            VA86-GE98.               CI0112
            11            VA86-CACCT  PICTURE  X.                       CI0112
            11            VA86-CPAYR  PICTURE  X(2).                    CI0112
            11            VA86-CDELI1 PICTURE  9(3).                    CI0112
            11            VA86-CATRN.                                   CI0112
            12            VA86-CATRF  PICTURE  9(3).                    CI0112
            12            VA86-CATRS  PICTURE  9(3).                    CI0112
            11            VA86-DEFFT  PICTURE  9(8).                    CI0112
            11            VA86-CTYPC  PICTURE  X.                       CI0112
            11            VA86-CIRAPA PICTURE  XX.                      CI0112
            11            VA86-FILLER PICTURE  X(09).                   CI0112
            11            VA86-GE49.                                    CI0112
            12            VA86-FILLER PICTURE  X(208).                  CI0112
            11            VA86-GE41                                     CI0112
                          REDEFINES            VA86-GE49.               CI0112
            12            VA86-NCHKN1 PICTURE  9(6).                    CI0112
            11            VA86-GE42                                     CI0112
                          REDEFINES            VA86-GE49.               CI0112
            12            VA86-CTID1.                                   CI0112
            13            VA86-CTIDA1 PICTURE  9(3).                    CI0112
            13            VA86-CTIDP1 PICTURE  X(13).                   CI0112
            13            VA86-CTIDN1 PICTURE  9(11).                   CI0112
            11            VA86-GE43                                     CI0112
                          REDEFINES            VA86-GE49.               CI0112
            12            VA86-GENAL2 PICTURE  X(30)                    CI0112
                          OCCURS       002     TIMES.                   CI0112
            12            VA86-GESAD2 PICTURE  X(30)                    CI0112
                          OCCURS       003     TIMES.                   CI0112
            11            VA86-GE44                                     CI0112
                          REDEFINES            VA86-GE49.               CI0112
            12            VA86-CTID01.                                  CI0112
            13            VA86-CTIDA6 PICTURE  9(3).                    CI0112
            13            VA86-NTIDP2 PICTURE  X(13).                   CI0112
            13            VA86-CTIDN2 PICTURE  9(11).                   CI0112
            12            VA86-GECKD2 PICTURE  9.                       CI0112
            12            VA86-PACCT  PICTURE  S999V99                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            VA86-PLOAN  PICTURE  S999V99                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            VA86-PADPT  PICTURE  S999V99                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            VA86-IPCTL  PICTURE  X.                       CI0112
            12            VA86-IPCTP  PICTURE  X.                       CI0112
            12            VA86-CEUNT  PICTURE  S9(5)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            VA86-GE31                                     CI0112
                          REDEFINES            VA86-GE98.               CI0112
            11            VA86-GCUSPZ PICTURE  X(12).                   CI0112

       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0112
            10            XW05-XW06.                                    CI0112
            11            XW05-XDBPCB.                                  CI0112
            12            XW05-XDBDNM PICTURE  X(08)                    CI0112
                          VALUE                SPACE.                   CI0112
            12            XW05-XSEGLV PICTURE  X(02)                    CI0112
                          VALUE                SPACE.                   CI0112
            12            XW05-XRC    PICTURE  X(02)                    CI0112
                          VALUE                SPACE.                   CI0112
            12            XW05-XPROPT PICTURE  X(04)                    CI0112
                          VALUE                SPACE.                   CI0112
            12            XW05-FILLER PICTURE  S9(5)                    CI0112
                          VALUE                ZERO                     CI0112
                          BINARY.                                       CI0112
            12            XW05-XSEGNM PICTURE  X(08)                    CI0112
                          VALUE                SPACE.                   CI0112
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0112
                          VALUE                ZERO                     CI0112
                          BINARY.                                       CI0112
            12            XW05-XSEGNB PICTURE  9(05)                    CI0112
                          VALUE                ZERO                     CI0112
                          BINARY.                                       CI0112
            12            XW05-XCOKEY PICTURE  X(70)                    CI0112
                          VALUE                SPACE.                   CI0112
            10            XW05-XW07.                                    CI0112
            11            XW05-XIOPCB.                                  CI0112
            12            XW05-XTERMI PICTURE  X(08)                    CI0112
                          VALUE                SPACE.                   CI0112
            12            XW05-FILLER PICTURE  XX                       CI0112
                          VALUE                SPACE.                   CI0112
            12            XW05-XRC1   PICTURE  X(02)                    CI0112
                          VALUE                SPACE.                   CI0112
            12            XW05-FILLER PICTURE  X(12)                    CI0112
                          VALUE                SPACE.                   CI0112
            12            XW05-XMODNM PICTURE  X(8)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0112
                          VALUE                ZERO.                    CI0112
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0112
                          VALUE                ZERO.                    CI0112
            10            XW05-XGU    PICTURE  X(4)                     CI0112
                          VALUE                'GU  '.                  CI0112
            10            XW05-XGHU   PICTURE  X(4)                     CI0112
                          VALUE                'GHU '.                  CI0112
            10            XW05-XGN    PICTURE  X(4)                     CI0112
                          VALUE                'GN  '.                  CI0112
            10            XW05-XGHN   PICTURE  X(4)                     CI0112
                          VALUE                'GHN '.                  CI0112
            10            XW05-XGNP   PICTURE  X(4)                     CI0112
                          VALUE                'GNP '.                  CI0112
            10            XW05-XGHNP  PICTURE  X(4)                     CI0112
                          VALUE                'GHNP'.                  CI0112
            10            XW05-XREPL  PICTURE  XXXX                     CI0112
                          VALUE                'REPL'.                  CI0112
            10            XW05-XISRT  PICTURE  X(4)                     CI0112
                          VALUE                'ISRT'.                  CI0112
            10            XW05-XDLET  PICTURE  X(4)                     CI0112
                          VALUE                'DLET'.                  CI0112
            10            XW05-XOPEN  PICTURE  X(4)                     CI0112
                          VALUE                'OPEN'.                  CI0112
            10            XW05-XCLSE  PICTURE  X(4)                     CI0112
                          VALUE                'CLSE'.                  CI0112
            10            XW05-XCHKP  PICTURE  X(4)                     CI0112
                          VALUE                'CHKP'.                  CI0112
            10            XW05-XXRST  PICTURE  X(4)                     CI0112
                          VALUE                'XRST'.                  CI0112
            10            XW05-XTERM  PICTURE  X(4)                     CI0112
                          VALUE                'TERM'.                  CI0112
            10            XW05-XNFPAC PICTURE  X(13)                    CI0112
                          VALUE                SPACE.                   CI0112
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0112
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0112
       01                 GC01.                                         CI0112
            10            GC01-GC01K.                                   CI0112
            11            GC01-C299.                                    CI0112
            12            GC01-CTID.                                    CI0112
            13            GC01-CTIDA  PICTURE  9(3).                    CI0112
            13            GC01-CTIDN.                                   CI0112
            14            GC01-CTIDNP PICTURE  X(13).                   CI0112
            14            GC01-CTIDND PICTURE  9(11).                   CI0112
            10            GC01-DCAG9L PICTURE  9(8).                    CI0112
            10            GC01-NAASQL PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            GC01-ICUST  PICTURE  X.                       CI0112
            10            GC01-NSEQ4B PICTURE  9(8)                     CI0112
                          BINARY.                                       CI0112
            10            GC01-PRCOD  PICTURE  9(5).                    CI0112
            10            GC01-PRSCD  PICTURE  X(9).                    CI0112
            10            GC01-FILLER PICTURE  X(8).                    CI0112
       01                 GC03.                                         CI0112
            10            GC03-GELL   PICTURE  9(4)                     CI0112
                          BINARY.                                       CI0112
            10            GC03-GD00.                                    CI0112
            11            GC03-GC03K.                                   CI0112
            12            GC03-DCACG9 PICTURE  9(8).                    CI0112
            12            GC03-NAASQ  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-CAATY  PICTURE  9(3).                    CI0112
            11            GC03-CVSYS  PICTURE  X(2).                    CI0112
            11            GC03-CACTO  PICTURE  9(3).                    CI0112
            11            GC03-CATRN.                                   CI0112
            12            GC03-CATRF  PICTURE  9(3).                    CI0112
            12            GC03-CATRS  PICTURE  9(3).                    CI0112
            11            GC03-CASTC  PICTURE  99.                      CI0112
            11            GC03-IPULL  PICTURE  X.                       CI0112
            11            GC03-GEAUN  PICTURE  9(5).                    CI0112
            11            GC03-GEOPD2 PICTURE  X(8).                    CI0112
            11            GC03-NBTCH  PICTURE  9(4).                    CI0112
            11            GC03-DEFFT  PICTURE  9(8).                    CI0112
            11            GC03-NSUNT  PICTURE  9(4).                    CI0112
            11            GC03-ITRAN  PICTURE  X.                       CI0112
            11            GC03-DLAUP1 PICTURE  9(8).                    CI0112
            11            GC03-ADRET  PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-TTRMS  PICTURE  X(12).                   CI0112
            11            GC03-IDELT  PICTURE  X.                       CI0112
            11            GC03-GEOPDM PICTURE  X(8).                    CI0112
            11            GC03-FILLER PICTURE  X(07).                   CI0112
            10            GC03-GD09.                                    CI0112
            11            GC03-FILLER PICTURE  X(70).                   CI0112
            10            GC03-GD01                                     CI0112
                          REDEFINES            GC03-GD09.               CI0112
            11            GC03-ADBRQ  PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-CTRTP  PICTURE  X(2).                    CI0112
            11            GC03-CPORT  PICTURE  X.                       CI0112
            11            GC03-CSCRNU PICTURE  X(4).                    CI0112
            11            GC03-DLAUP  PICTURE  9(8).                    CI0112
            11            GC03-CTWHAT PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-PWHLD  PICTURE  S999V9(5)                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-IWTHH  PICTURE  X.                       CI0112
            11            GC03-NDRFT  PICTURE  9(5).                    CI0112
            11            GC03-IDPAP  PICTURE  X.                       CI0112
            11            GC03-GETIM  PICTURE  S9(7)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-QNACT  PICTURE  9(3).                    CI0112
            11            GC03-AEDRQ  PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-IPLIN  PICTURE  X.                       CI0112
            11            GC03-CLIDNB PICTURE  9(8).                    CI0112
            11            GC03-CSLCT  PICTURE  X.                       CI0112
            11            GC03-ITELE  PICTURE  X.                       CI0112
            11            GC03-FILLER PICTURE  X(06).                   CI0112
            10            GC03-GD02                                     CI0112
                          REDEFINES            GC03-GD09.               CI0112
            11            GC03-CSYST  PICTURE  99.                      CI0112
            11            GC03-FILLER PICTURE  X.                       CI0112
            11            GC03-ACASH  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-DTRAC  PICTURE  9(8).                    CI0112
            11            GC03-CTRSO  PICTURE  9(02).                   CI0112
            11            GC03-NTRCE  PICTURE  9(06).                   CI0112
            11            GC03-GECKD1 PICTURE  9.                       CI0112
            11            GC03-CCOLL  PICTURE  X(3).                    CI0112
            11            GC03-CLTDP  PICTURE  X(3).                    CI0112
            11            GC03-PSLLD  PICTURE  S99V999                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-ISLOR  PICTURE  X.                       CI0112
            11            GC03-ITPAC  PICTURE  X.                       CI0112
            11            GC03-CPMTCA PICTURE  XXX.                     CI0112
            11            GC03-CSERV  PICTURE  X(3).                    CI0112
            11            GC03-ACOMO  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-IPLIN1 PICTURE  X.                       CI0112
            11            GC03-INQEX  PICTURE  X.                       CI0112
            11            GC03-CTKRAA PICTURE  X(12).                   CI0112
            11            GC03-CCSMQ  PICTURE  X.                       CI0112
            11            GC03-IVAEX1 PICTURE  X.                       CI0112
            11            GC03-IHPMT  PICTURE  X(1).                    CI0112
            11            GC03-GETIM3 PICTURE  S9(7)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            GC03-GD03                                     CI0112
                          REDEFINES            GC03-GD09.               CI0112
            11            GC03-CATRNC PICTURE  9(6).                    CI0112
            11            GC03-APRNT1 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-QSHOWT PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-ACINVT PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-ACOMO7 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-QSHOMW PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-ATAXT3 PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-CTSTR  PICTURE  9(2).                    CI0112
            11            GC03-ICIRA  PICTURE  X.                       CI0112
            11            GC03-GETIM2 PICTURE  S9(7)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-CPMTCX PICTURE  XX.                      CI0112
            11            GC03-FILLER PICTURE  X(16).                   CI0112
            10            GC03-GD99.                                    CI0112
            11            GC03-FILLER PICTURE  X(248).                  CI0112
            10            GC03-GD10                                     CI0112
                          REDEFINES            GC03-GD99.               CI0112
            11            GC03-MROTC  PICTURE  X(7).                    CI0112
            11            GC03-CEDSC  PICTURE  9(1).                    CI0112
            11            GC03-ILPOI  PICTURE  X(1).                    CI0112
            11            GC03-AWRCH  PICTURE  S9(3)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-CHCOC1 PICTURE  9(2).                    CI0112
            11            GC03-CHCOC2 PICTURE  9(2).                    CI0112
            11            GC03-CHCOC3 PICTURE  9(2).                    CI0112
            11            GC03-CHCOC4 PICTURE  9(2).                    CI0112
            11            GC03-CMCOC1 PICTURE  9(3).                    CI0112
            11            GC03-CMCOC2 PICTURE  9(3).                    CI0112
            11            GC03-CMCOC3 PICTURE  9(3).                    CI0112
            11            GC03-GD11.                                    CI0112
            12            GC03-FILLER PICTURE  X(219).                  CI0112
            11            GC03-GD12                                     CI0112
                          REDEFINES            GC03-GD11.               CI0112
            12            GC03-CELLO  PICTURE  9(1).                    CI0112
            12            GC03-CECLO  PICTURE  9(1).                    CI0112
            12            GC03-AEXML  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-CEPI   PICTURE  X(1).                    CI0112
            12            GC03-CEXTY  PICTURE  X.                       CI0112
            12            GC03-CROPC  PICTURE  9(1).                    CI0112
            12            GC03-CPUTY  PICTURE  9(1).                    CI0112
            12            GC03-IMCII  PICTURE  X(1).                    CI0112
            12            GC03-GEMISC                                   CI0112
                          OCCURS       010     TIMES.                   CI0112
            13            GC03-AMGLA  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            13            GC03-CMGLC  PICTURE  9(1).                    CI0112
            13            GC03-NMGLN  PICTURE  9(4).                    CI0112
            12            GC03-ACTRN  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-IWRBK  PICTURE  X.                       CI0112
            12            GC03-IFEDX  PICTURE  X.                       CI0112
            12            GC03-ICNTR  PICTURE  X.                       CI0112
            12            GC03-IOCKH  PICTURE  X.                       CI0112
            12            GC03-ICRCK  PICTURE  X.                       CI0112
            12            GC03-NHMPN  PICTURE  S9(10)                   CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-ITELR1 PICTURE  X.                       CI0112
            11            GC03-GD13                                     CI0112
                          REDEFINES            GC03-GD11.               CI0112
            12            GC03-DREDO  PICTURE  9(8).                    CI0112
            12            GC03-CATRNR PICTURE  9(6).                    CI0112
            12            GC03-CEVN   PICTURE  9(9).                    CI0112
            12            GC03-ISUSP  PICTURE  X(1).                    CI0112
            11            GC03-GD15                                     CI0112
                          REDEFINES            GC03-GD11.               CI0112
            12            GC03-CPUTZ  PICTURE  9(1).                    CI0112
            12            GC03-CETLB  PICTURE  9(3).                    CI0112
            12            GC03-QTRMC  PICTURE  9(3).                    CI0112
            12            GC03-DEFFTE PICTURE  9(8).                    CI0112
            12            GC03-DEFFTF PICTURE  9(8).                    CI0112
            12            GC03-DEFFTG PICTURE  9(8).                    CI0112
            12            GC03-XZ1A   PICTURE  X.                       CI0112
            12            GC03-XZ1B   PICTURE  X.                       CI0112
            12            GC03-XZ1C   PICTURE  X.                       CI0112
            12            GC03-XZ1D   PICTURE  X.                       CI0112
            12            GC03-XZ1E   PICTURE  X.                       CI0112
            12            GC03-XZ1F   PICTURE  X.                       CI0112
            12            GC03-XZ1G   PICTURE  X.                       CI0112
            12            GC03-XZ1H   PICTURE  X.                       CI0112
            12            GC03-XZ1I   PICTURE  X.                       CI0112
            12            GC03-DEFFTH PICTURE  9(8).                    CI0112
            11            GC03-GD19                                     CI0112
                          REDEFINES            GC03-GD11.               CI0112
            12            GC03-GD11.                                    CI0112
            13            GC03-FILLER PICTURE  X(219).                  CI0112
            10            GC03-GD20                                     CI0112
                          REDEFINES            GC03-GD99.               CI0112
            11            GC03-ADDACT PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-ISIGV  PICTURE  X.                       CI0112
            11            GC03-IALLF  PICTURE  X.                       CI0112
            11            GC03-QSHOWQ PICTURE  S9(9)V999                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-CCDSCW PICTURE  9(2).                    CI0112
            11            GC03-IDWRL  PICTURE  X.                       CI0112
            11            GC03-ITELR  PICTURE  X.                       CI0112
            11            GC03-IABIN  PICTURE  X.                       CI0112
            11            GC03-PACT1  PICTURE  S999V999                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-IBFAF  PICTURE  X.                       CI0112
            11            GC03-IFRSA  PICTURE  X.                       CI0112
            11            GC03-ICRCAN PICTURE  X.                       CI0112
            11            GC03-ACACTV PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-AGFND  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-QCSHOW PICTURE  S9(9)V999                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-QCSHIS PICTURE  S9(9)V999                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-NDTRC  PICTURE  9(8).                    CI0112
            11            GC03-CAERU  PICTURE  X(4).                    CI0112
            11            GC03-IFDGO  PICTURE  X.                       CI0112
            11            GC03-PSLLD2 PICTURE  S99V999                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-ISLOR2 PICTURE  X.                       CI0112
            11            GC03-QSFIO  PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-QSFID  PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-CGDIN  PICTURE  X.                       CI0112
            11            GC03-DGDIN  PICTURE  9(8).                    CI0112
            10            GC03-GD30                                     CI0112
                          REDEFINES            GC03-GD99.               CI0112
            11            GC03-ISKED  PICTURE  X.                       CI0112
            11            GC03-CENXC  PICTURE  9(2).                    CI0112
            11            GC03-GD31.                                    CI0112
            12            GC03-FILLER PICTURE  X(245).                  CI0112
            11            GC03-GD32                                     CI0112
                          REDEFINES            GC03-GD31.               CI0112
            12            GC03-IABIN1 PICTURE  X.                       CI0112
            12            GC03-CLDOD  PICTURE  9(8).                    CI0112
            12            GC03-NCLAM  PICTURE  9(5)                     CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-ISURR  PICTURE  X.                       CI0112
            12            GC03-GEHCD  PICTURE  9(3).                    CI0112
            12            GC03-CRATC  PICTURE  9(4).                    CI0112
            12            GC03-AMAXD  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-ASCHGA PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-APYOM  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-IWTHH1 PICTURE  X.                       CI0112
            12            GC03-CPAYCL PICTURE  X(2).                    CI0112
            12            GC03-CTSAO  PICTURE  X.                       CI0112
            12            GC03-NCONF  PICTURE  9(08).                   CI0112
            12            GC03-CLID   PICTURE  X(23).                   CI0112
            12            GC03-CARTY  PICTURE  99.                      CI0112
            12            GC03-NARRS  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-CARTZ  PICTURE  99.                      CI0112
            12            GC03-NAPDS  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-CPMTO  PICTURE  X.                       CI0112
            12            GC03-DNPMT  PICTURE  9(8).                    CI0112
            12            GC03-IPCTV  PICTURE  X.                       CI0112
            12            GC03-IMECH  PICTURE  X(01).                   CI0112
            12            GC03-IMVAO  PICTURE  X(1).                    CI0112
            12            GC03-AMVA1  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-CACTS  PICTURE  X.                       CI0112
            12            GC03-CTSPP  PICTURE  X(1).                    CI0112
            12            GC03-CACT4  PICTURE  X(2).                    CI0112
            12            GC03-IVAEX  PICTURE  X.                       CI0112
            12            GC03-DFPMT  PICTURE  9(8).                    CI0112
            12            GC03-IDEMD  PICTURE  X.                       CI0112
            12            GC03-IOFST  PICTURE  X.                       CI0112
            12            GC03-AMXLB  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-ACULB  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-DEIRNB PICTURE  9(8).                    CI0112
            12            GC03-DEFFE  PICTURE  9(8).                    CI0112
            12            GC03-DEFFR  PICTURE  9(8).                    CI0112
            12            GC03-ISPUP  PICTURE  X.                       CI0112
            12            GC03-CPNCG  PICTURE  X.                       CI0112
            12            GC03-IEXPU  PICTURE  X.                       CI0112
            12            GC03-IPPCF  PICTURE  X.                       CI0112
            12            GC03-NAAPT  PICTURE  9(2).                    CI0112
            12            GC03-PWHLDS PICTURE  S999V9(5)                CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-ISWHO  PICTURE  X(1).                    CI0112
            11            GC03-GD33                                     CI0112
                          REDEFINES            GC03-GD31.               CI0112
            12            GC03-CPAYC  PICTURE  X(2).                    CI0112
            12            GC03-ADBRQX PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-ADBRQV PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-APTXR  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-CTRTPE PICTURE  X(2).                    CI0112
            12            GC03-NCLAMI PICTURE  S9(9)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-CLIDO8 PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-CLIDN  PICTURE  X(20).                   CI0112
            12            GC03-DSET01 PICTURE  S9(8)                    CI0112
                          BINARY.                                       CI0112
            12            GC03-CTSET1 PICTURE  S9(6)                    CI0112
                          BINARY.                                       CI0112
            12            GC03-DSET02 PICTURE  S9(8)                    CI0112
                          BINARY.                                       CI0112
            12            GC03-CTSET2 PICTURE  S9(6)                    CI0112
                          BINARY.                                       CI0112
            11            GC03-GD34                                     CI0112
                          REDEFINES            GC03-GD31.               CI0112
            12            GC03-QNOFM  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-CLTRM  PICTURE  99.                      CI0112
            12            GC03-AMXLN  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-ALADJ  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-ACHK   PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-APRMO  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-IMECH1 PICTURE  X(01).                   CI0112
            12            GC03-CACT41 PICTURE  X(2).                    CI0112
            12            GC03-ACDSCC PICTURE  S9(05)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-ACDSCD PICTURE  S9(05)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-GD39                                     CI0112
                          REDEFINES            GC03-GD31.               CI0112
            12            GC03-GD31.                                    CI0112
            13            GC03-FILLER PICTURE  X(245).                  CI0112
            10            GC03-GD40                                     CI0112
                          REDEFINES            GC03-GD99.               CI0112
            11            GC03-NTR    PICTURE  9(8).                    CI0112
            11            GC03-NPBNC  PICTURE  X(24).                   CI0112
            11            GC03-CRREV  PICTURE  X(3).                    CI0112
            11            GC03-CSUSL  PICTURE  S9.                      CI0112
            11            GC03-NMGLN1 PICTURE  9(4).                    CI0112
            11            GC03-DCAC92 PICTURE  9(8).                    CI0112
            11            GC03-NAASQ3 PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-GD49.                                    CI0112
            12            GC03-FILLER PICTURE  X(198).                  CI0112
            11            GC03-GD41                                     CI0112
                          REDEFINES            GC03-GD49.               CI0112
            12            GC03-CRREF  PICTURE  9(2).                    CI0112
            12            GC03-CORIR  PICTURE  X(02).                   CI0112
            12            GC03-CIPDB  PICTURE  X(03).                   CI0112
            12            GC03-CPAYH  PICTURE  X(02).                   CI0112
            12            GC03-NAMEX  PICTURE  9(15)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC03-DCHAE  PICTURE  9(4).                    CI0112
            12            GC03-DRQST  PICTURE  S9(8)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-GD42                                     CI0112
                          REDEFINES            GC03-GD49.               CI0112
            12            GC03-CPMTCB PICTURE  X(3).                    CI0112
            10            GC03-GD50                                     CI0112
                          REDEFINES            GC03-GD99.               CI0112
            11            GC03-ALOAD  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-PSLLD4 PICTURE  S99V999                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-CSUSL1 PICTURE  S9.                      CI0112
            11            GC03-CRREV1 PICTURE  X(3).                    CI0112
            11            GC03-ADDAC  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-DL13.                                    CI0112
            12            GC03-GEYR   PICTURE  9(4).                    CI0112
            12            GC03-GEMTH  PICTURE  99.                      CI0112
            12            GC03-NDAY   PICTURE  99.                      CI0112
            11            GC03-NSEQ3P PICTURE  S9(5)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-XZ6A   PICTURE  X(6).                    CI0112
            11            GC03-XZ7    PICTURE  X(7).                    CI0112
            11            GC03-XZ6B   PICTURE  X(6).                    CI0112
            11            GC03-XZ6    PICTURE  X(6).                    CI0112
            11            GC03-XZ6C   PICTURE  X(6).                    CI0112
            11            GC03-XZ20   PICTURE  X(20).                   CI0112
            11            GC03-CATRN1 PICTURE  9(6).                    CI0112
            11            GC03-ADDAC2 PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-ATAXT2 PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-ACOMOT PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-XZ5    PICTURE  X(5).                    CI0112
            11            GC03-IREVD  PICTURE  X(1).                    CI0112
            11            GC03-ISUSP1 PICTURE  X(1).                    CI0112
            11            GC03-XZ6D   PICTURE  X(6).                    CI0112
            11            GC03-XZ13   PICTURE  X(13).                   CI0112
            11            GC03-CWHTP2 PICTURE  X(3).                    CI0112
            11            GC03-CWHTP3 PICTURE  X(3).                    CI0112
            11            GC03-DTREN  PICTURE  9(8).                    CI0112
            11            GC03-NAASQ1 PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            GC03-GD51                                     CI0112
                          REDEFINES            GC03-GD99.               CI0112
            11            GC03-ADOMOT PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-ACGLT  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-ACGST  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-CTXMT  PICTURE  9(2).                    CI0112
            11            GC03-ALOAD3 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-FILLER PICTURE  X(31).                   CI0112
            10            GC03-GD52                                     CI0112
                          REDEFINES            GC03-GD99.               CI0112
            11            GC03-DEFFT5 PICTURE  9(8).                    CI0112
            11            GC03-PSLLD5 PICTURE  S99V999                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-CSUSL2 PICTURE  S9.                      CI0112
            11            GC03-ALOAD2 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-DL22.                                    CI0112
            12            GC03-NYEAR1 PICTURE  9(4).                    CI0112
            12            GC03-GEMTHA PICTURE  99.                      CI0112
            12            GC03-NDAY01 PICTURE  99.                      CI0112
            11            GC03-NSEQ3R PICTURE  S9(5)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-CWHTP  PICTURE  X(3).                    CI0112
            11            GC03-CWHFR  PICTURE  X(3).                    CI0112
            11            GC03-CATRN7 PICTURE  9(6).                    CI0112
            11            GC03-ATAXT5 PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-QSHOT  PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-ACINT3 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-CWHTP1 PICTURE  X(3).                    CI0112
            11            GC03-CWHFR1 PICTURE  X(3).                    CI0112
            11            GC03-ACOMO5 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-QSHOMU PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-ACASH1 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-FILLER PICTURE  X(04).                   CI0112
            11            GC03-CATRN8 PICTURE  9(6).                    CI0112
            11            GC03-ALOAD1 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-PSLLD1 PICTURE  S99V999                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-QSHOT1 PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-ACINT4 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-CSUSL4 PICTURE  S9.                      CI0112
            11            GC03-ACOMO4 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            GC03-GD60                                     CI0112
                          REDEFINES            GC03-GD99.               CI0112
            11            GC03-GEOPDD PICTURE  X(8)                     CI0112
                          OCCURS       005     TIMES.                   CI0112
            11            GC03-DLAUP3 PICTURE  9(8)                     CI0112
                          OCCURS       005     TIMES.                   CI0112
            11            GC03-GEOPDB PICTURE  X(8).                    CI0112
            11            GC03-DLAUP4 PICTURE  9(8).                    CI0112
            11            GC03-ITELR2 PICTURE  X.                       CI0112
            11            GC03-IPMTA  PICTURE  X.                       CI0112
            11            GC03-CCSMG  PICTURE  X.                       CI0112
            11            GC03-CPLEC  PICTURE  XX.                      CI0112
            11            GC03-CORTYA PICTURE  X(3).                    CI0112
            11            GC03-CACTBC PICTURE  X(1).                    CI0112
            11            GC03-CGSPIA PICTURE  X.                       CI0112
            11            GC03-IPTRDA PICTURE  X(01).                   CI0112
            11            GC03-GCUSPY PICTURE  X(12).                   CI0112
            11            GC03-CPALLA PICTURE  X(1).                    CI0112
            11            GC03-QSHO5A PICTURE  S9(9)V999                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-IFRSAB PICTURE  X.                       CI0112
            11            GC03-DELOI  PICTURE  9(8).                    CI0112
            11            GC03-IAROAA PICTURE  X.                       CI0112
            11            GC03-ACINVR PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-ILTINA PICTURE  X.                       CI0112
            11            GC03-ALOIDA PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC03-CFUNTA PICTURE  X(2).                    CI0112
            11            GC03-CLGND  PICTURE  X.                       CI0112
            11            GC03-CPH3U  PICTURE  X.                       CI0112
            11            GC03-GESTD  PICTURE  9(8).                    CI0112
            11            GC03-GEEND  PICTURE  9(8).                    CI0112
            11            GC03-CPMTF  PICTURE  99.                      CI0112
            11            GC03-CNAVR  PICTURE  X(1).                    CI0112
            10            GC03-GD70                                     CI0112
                          REDEFINES            GC03-GD99.               CI0112
            11            GC03-CMEMO  PICTURE  X(2).                    CI0112
            11            GC03-ALPLDT PICTURE  9(8).                    CI0112
            11            GC03-CTLPD  PICTURE  9(8).                    CI0112
            11            GC03-CPAYCM PICTURE  X(2).                    CI0112
       01                 GC04.                                         CI0112
            10            GC04-CLCUS  PICTURE  99.                      CI0112
            10            GC04-CCACT  PICTURE  99.                      CI0112
            10            GC04-AFEET  PICTURE  S9(5)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            GC04-ITERF  PICTURE  X.                       CI0112
            10            GC04-ATERF  PICTURE  S9(5)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            GC04-CLDOB  PICTURE  9(8).                    CI0112
            10            GC04-CPLTYP PICTURE  X(14).                   CI0112
            10            GC04-IACFPD PICTURE  X(1).                    CI0112
            10            GC04-FILLER PICTURE  X(14).                   CI0112
       01                 GC06.                                         CI0112
            10            GC06-GELL   PICTURE  9(4)                     CI0112
                          BINARY.                                       CI0112
            10            GC06-GE00.                                    CI0112
            11            GC06-GC06K.                                   CI0112
            12            GC06-NPISQ  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC06-ACOTD  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC06-PPOTD  PICTURE  S9(3)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC06-QPSTD  PICTURE  S9(7)V999                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC06-CPITC  PICTURE  99.                      CI0112
            11            GC06-ITRNB  PICTURE  X.                       CI0112
            11            GC06-FILLER PICTURE  X(14).                   CI0112
            10            GC06-GE98.                                    CI0112
            11            GC06-FILLER PICTURE  X(240).                  CI0112
            10            GC06-GE10                                     CI0112
                          REDEFINES            GC06-GE98.               CI0112
            11            GC06-CDELI  PICTURE  9(3).                    CI0112
            11            GC06-CPAYC  PICTURE  X(2).                    CI0112
            11            GC06-ICHKP  PICTURE  X.                       CI0112
            11            GC06-CLTIN  PICTURE  9(12).                   CI0112
            11            GC06-IFHAI  PICTURE  X.                       CI0112
            11            GC06-CDQUA  PICTURE  X(2).                    CI0112
            11            GC06-FILLER PICTURE  X(07).                   CI0112
            11            GC06-GE99.                                    CI0112
            12            GC06-FILLER PICTURE  X(212).                  CI0112
            11            GC06-GE01                                     CI0112
                          REDEFINES            GC06-GE99.               CI0112
            12            GC06-NTR    PICTURE  9(8).                    CI0112
            12            GC06-GECKD  PICTURE  9.                       CI0112
            12            GC06-NPBN   PICTURE  X(20).                   CI0112
            12            GC06-CCBAT  PICTURE  99.                      CI0112
            12            GC06-CLID4  PICTURE  X(23).                   CI0112
            12            GC06-GENAL1 PICTURE  X(30)                    CI0112
                          OCCURS       002     TIMES.                   CI0112
            12            GC06-GESAD1 PICTURE  X(30)                    CI0112
                          OCCURS       003     TIMES.                   CI0112
            11            GC06-GE02                                     CI0112
                          REDEFINES            GC06-GE99.               CI0112
            12            GC06-GENAL  PICTURE  X(30)                    CI0112
                          OCCURS       002     TIMES.                   CI0112
            12            GC06-GESAD  PICTURE  X(30)                    CI0112
                          OCCURS       003     TIMES.                   CI0112
            11            GC06-GE03                                     CI0112
                          REDEFINES            GC06-GE99.               CI0112
            12            GC06-NCHKN  PICTURE  9(11).                   CI0112
            11            GC06-GE04                                     CI0112
                          REDEFINES            GC06-GE99.               CI0112
            12            GC06-CTIDAP PICTURE  9(3).                    CI0112
            12            GC06-PRCOD  PICTURE  9(5).                    CI0112
            12            GC06-TDELI  PICTURE  X(30).                   CI0112
            12            GC06-CINCD  PICTURE  9(02).                   CI0112
            10            GC06-GE20                                     CI0112
                          REDEFINES            GC06-GE98.               CI0112
            11            GC06-C299.                                    CI0112
            12            GC06-CTID.                                    CI0112
            13            GC06-CTIDA  PICTURE  9(3).                    CI0112
            13            GC06-CTIDN.                                   CI0112
            14            GC06-CTIDNP PICTURE  X(13).                   CI0112
            14            GC06-CTIDND PICTURE  9(11).                   CI0112
            11            GC06-DCACG9 PICTURE  9(8).                    CI0112
            11            GC06-NAASQ  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            GC06-CIRAP  PICTURE  XX.                      CI0112
            11            GC06-CTYPE  PICTURE  X.                       CI0112
            11            GC06-INACT  PICTURE  X.                       CI0112
            11            GC06-FILLER PICTURE  X(01).                   CI0112
            11            GC06-ITPAC  PICTURE  X.                       CI0112
            11            GC06-ITAXI  PICTURE  X.                       CI0112
            11            GC06-IOWNC  PICTURE  X.                       CI0112
            11            GC06-CDVCD  PICTURE  X(2).                    CI0112
            11            GC06-CTCUS  PICTURE  999.                     CI0112
            11            GC06-CPMTCB PICTURE  X(3).                    CI0112
            11            GC06-CASTC1 PICTURE  99.                      CI0112
            11            GC06-PRCOD1 PICTURE  9(5).                    CI0112
            11            GC06-CPRSC1 PICTURE  X(9).                    CI0112
            11            GC06-CPRTB  PICTURE  X.                       CI0112
            11            GC06-CBRKD  PICTURE  9(4).                    CI0112
            11            GC06-FILLER PICTURE  X(12).                   CI0112
            10            GC06-GE30                                     CI0112
                          REDEFINES            GC06-GE98.               CI0112
            11            GC06-CFIDC  PICTURE  X(5).                    CI0112
            11            GC06-CPHSE  PICTURE  9(2).                    CI0112
            11            GC06-FILLER PICTURE  X(05).                   CI0112
            11            GC06-IABIN  PICTURE  X.                       CI0112
            11            GC06-PDFND  PICTURE  S999V9(3)                CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            GC06-GE40                                     CI0112
                          REDEFINES            GC06-GE98.               CI0112
            11            GC06-CACCT  PICTURE  X.                       CI0112
            11            GC06-CPAYR  PICTURE  X(2).                    CI0112
            11            GC06-CDELI1 PICTURE  9(3).                    CI0112
            11            GC06-CATRN.                                   CI0112
            12            GC06-CATRF  PICTURE  9(3).                    CI0112
            12            GC06-CATRS  PICTURE  9(3).                    CI0112
            11            GC06-DEFFT  PICTURE  9(8).                    CI0112
            11            GC06-CTYPC  PICTURE  X.                       CI0112
            11            GC06-CIRAPA PICTURE  XX.                      CI0112
            11            GC06-FILLER PICTURE  X(09).                   CI0112
            11            GC06-GE49.                                    CI0112
            12            GC06-FILLER PICTURE  X(208).                  CI0112
            11            GC06-GE41                                     CI0112
                          REDEFINES            GC06-GE49.               CI0112
            12            GC06-NCHKN1 PICTURE  9(6).                    CI0112
            11            GC06-GE42                                     CI0112
                          REDEFINES            GC06-GE49.               CI0112
            12            GC06-CTID1.                                   CI0112
            13            GC06-CTIDA1 PICTURE  9(3).                    CI0112
            13            GC06-CTIDP1 PICTURE  X(13).                   CI0112
            13            GC06-CTIDN1 PICTURE  9(11).                   CI0112
            11            GC06-GE43                                     CI0112
                          REDEFINES            GC06-GE49.               CI0112
            12            GC06-GENAL2 PICTURE  X(30)                    CI0112
                          OCCURS       002     TIMES.                   CI0112
            12            GC06-GESAD2 PICTURE  X(30)                    CI0112
                          OCCURS       003     TIMES.                   CI0112
            11            GC06-GE44                                     CI0112
                          REDEFINES            GC06-GE49.               CI0112
            12            GC06-CTID01.                                  CI0112
            13            GC06-CTIDA6 PICTURE  9(3).                    CI0112
            13            GC06-NTIDP2 PICTURE  X(13).                   CI0112
            13            GC06-CTIDN2 PICTURE  9(11).                   CI0112
            12            GC06-GECKD2 PICTURE  9.                       CI0112
            12            GC06-PACCT  PICTURE  S999V99                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC06-PLOAN  PICTURE  S999V99                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC06-PADPT  PICTURE  S999V99                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            GC06-IPCTL  PICTURE  X.                       CI0112
            12            GC06-IPCTP  PICTURE  X.                       CI0112
            12            GC06-CEUNT  PICTURE  S9(5)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            GC06-GE31                                     CI0112
                          REDEFINES            GC06-GE98.               CI0112
            11            GC06-GCUSPZ PICTURE  X(12).                   CI0112
       01                 GC12.                                         CI0112
            10            GC12-GC12K.                                   CI0112
            11            GC12-CIRAP  PICTURE  XX.                      CI0112
            10            GC12-AIRCT  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            GC12-FILLER PICTURE  X.                       CI0112
       01                 GC21.                                         CI0112
            10            GC21-C299.                                    CI0112
            11            GC21-CTID.                                    CI0112
            12            GC21-CTIDA  PICTURE  9(3).                    CI0112
            12            GC21-CTIDN.                                   CI0112
            13            GC21-CTIDNP PICTURE  X(13).                   CI0112
            13            GC21-CTIDND PICTURE  9(11).                   CI0112
            10            GC21-DCACG9 PICTURE  9(8).                    CI0112
            10            GC21-NAASQ  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            GC21-FILLER PICTURE  X.                       CI0112
       01                 GC29.                                         CI0112
            10            GC29-CSLCT  PICTURE  X.                       CI0112
            10            GC29-NGEOR  PICTURE  9(08).                   CI0112
            10            GC29-CACLS2 PICTURE  X(20).                   CI0112
            10            GC29-CAPID  PICTURE  9(2).                    CI0112
            10            GC29-NGEOPA PICTURE  X(08).                   CI0112
            10            GC29-CACLS1 PICTURE  X(20).                   CI0112
            10            GC29-CTRHO  PICTURE  9(8).                    CI0112
            10            GC29-GETIM3 PICTURE  S9(7)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            GC29-GEOPD9 PICTURE  X(8).                    CI0112
            10            GC29-DCACG1 PICTURE  9(8).                    CI0112
            10            GC29-IWEBBT PICTURE  X.                       CI0112
            10            GC29-CAVER  PICTURE  X.                       CI0112
            10            GC29-FILLER PICTURE  X(30).                   CI0112
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
      ******************************************************************ADUTAB
      **              TABLE TA5B ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA5B-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=5B FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA5B.                                                CI0112
           04    G-TA5B-PARAM.                                          CI0112
             10  G-TA5B-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0112
                        VALUE      +154.                                CI0112
             10  G-TA5B-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0112
                        VALUE      +001.                                CI0112
             10  G-TA5B-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0112
                        VALUE      +017.                                CI0112
             10  G-TA5B-NUAPP  PICTURE 99                               CI0112
                        VALUE       0.                                  CI0112
             10  G-TA5B-NUTAB  PICTURE X(6)                             CI0112
                        VALUE 'TA005B'.                                 CI0112
             10  G-TA5B-TABFO  PICTURE XX                 VALUE SPACE.  CI0112
             10  G-TA5B-TABCR  PICTURE XX                 VALUE SPACE.  CI0112
             10  G-TA5B-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0112
             10  G-TA5B-NUSSC  PICTURE X  VALUE   ' '.                  CI0112
             10  G-TA5B-NUSSY  PICTURE X                  VALUE SPACE.  CI0112
             10  G-TA5B-TRANID PICTURE X(4)               VALUE SPACE.  CI0112
             10  G-TA5B-FILSYS.                                         CI0112
             15  G-TA5B-USERC  PICTURE X(6)               VALUE SPACE.  CI0112
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0112
           04             TA5B.                                         CI0112
            10            TA5B-GAPSC.                                   CI0112
            11            TA5B-CTIDA  PICTURE  9(3)                     CI0112
                          VALUE                ZERO.                    CI0112
            11            TA5B-PRCOD  PICTURE  9(5)                     CI0112
                          VALUE                ZERO.                    CI0112
            11            TA5B-PRSCD  PICTURE  X(9)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-PRCODX PICTURE  9(5)                     CI0112
                          VALUE                ZERO.                    CI0112
            10            TA5B-PRCSUB PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-PRCAUT PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-PRCBAS PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-PRCSTK PICTURE  XX                       CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-PRCPRE PICTURE  X(4)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-IBDUP  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-IUSPR  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-CVSYS  PICTURE  X(2)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-IDTOD  PICTURE  X(1)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-GRSFC  PICTURE  99                       CI0112
                          VALUE                ZERO.                    CI0112
            10            TA5B-ZDA18  PICTURE  X(18)                    CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-CMPCTB PICTURE  X(4)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-ITERM  PICTURE  X(1)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-AMFAC  PICTURE  S9(7)                    CI0112
                          VALUE                ZERO.                    CI0112
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-CPRBK  PICTURE  X(3)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-CFXDM  PICTURE  99                       CI0112
                          VALUE                ZERO.                    CI0112
            10            TA5B-NGLCS  PICTURE  X(5)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-NDFCS  PICTURE  X(5)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-CTNLI  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-CBANK  PICTURE  X(03)                    CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-ISYPO  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-ISYPP  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-ICOPT  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-IANPY  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-IDSAR  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-ICIPT  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-IANDS  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-IKPMA  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-INMWT  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-IVANT  PICTURE  X(1)                     CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-ISDAV  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-IUDAV  PICTURE  X                        CI0112
                          VALUE                SPACE.                   CI0112
            10            TA5B-ZDA15  PICTURE  X(15)                    CI0112
                          VALUE                SPACE.                   CI0112
      **                                                                ADUTAB

      ******************************************************************
      ** MISCELLANEOUS WORK FIELDS                                     *
      ******************************************************************
      *01  7-WS00-AREA.

      *    ACCUMULATOR OF AMOUNTS TO BE TAKEN FROM ACCOUNT PRIOR TO
      *    DISBURSEMENT
      *!WI
           05  7-TOTL-AAMWTB
                        PICTURE S9(9)V9(2)                              CI0112
                          COMPUTATIONAL-3                               CI0112
                                         VALUE ZERO.

      *    FIELD USED TO INDICATE IF TA5B ACCESS WAS SUCCESSFUL
           05  TA5B-IK          PIC X(01)   VALUE '0'.

      *    VALUES TO STORE THE KEYS TO A COLLECTION SHOULD ONE BE
      *    CREATED
      *!WI
           05  W-WS00-DCACG9
                        PICTURE 9(8)                                    CI0112
                                         VALUE ZERO.
      *!WI
           05  W-WS00-NAASQ
                        PICTURE S9(3)                                   CI0112
                          COMPUTATIONAL-3                               CI0112
                                         VALUE ZERO.

       01   DEBUT-WSS.                                                  CI0112
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0112
            05   IK     PICTURE X.                                      CI0112
       01  CONSTANTES-PAC.                                              CI0112
           05  FILLER  PICTURE X(87)   VALUE                            CI0112
                     '6015 CAT09/08/14CI0112ADMIN   14:34:52CI0112P AMERCI0112
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0112
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0112
           05  NUGNA   PICTURE X(5).                                    CI0112
           05  APPLI   PICTURE X(3).                                    CI0112
           05  DATGN   PICTURE X(8).                                    CI0112
           05  PROGR   PICTURE X(6).                                    CI0112
           05  CODUTI  PICTURE X(8).                                    CI0112
           05  TIMGN   PICTURE X(8).                                    CI0112
           05  PROGE   PICTURE X(8).                                    CI0112
           05  COBASE  PICTURE X(4).                                    CI0112
           05  DATGNC  PICTURE X(10).                                   CI0112
           05  RELEAS  PICTURE X(7).                                    CI0112
           05  DATGE   PICTURE X(10).                                   CI0112
           05  DATSQ   PICTURE X(10).                                   CI0112
       01  DATCE.                                                       CI0112
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0112
         05  DATOR.                                                     CI0112
           10  DATOA  PICTURE XX.                                       CI0112
           10  DATOM  PICTURE XX.                                       CI0112
           10  DATOJ  PICTURE XX.                                       CI0112
       01   VARIABLES-CONDITIONNELLES.                                  CI0112
            05                  FT      PICTURE X VALUE '0'.            CI0112
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0112
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0112
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0112
            05       5-GC00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0112
            05       5-LC00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0112
       01               S-GC01-SSA.                                     CI0112
            10         S1-GC01-SEGNAM PICTURE X(8)                      CI0112
                                      VALUE 'GC01    '.                 CI0112
            10         S1-GC01-CCOM   PICTURE X VALUE '*'.              CI0112
            10          S-GC01-CCOD   PICTURE X(5)                      CI0112
                                      VALUE '-----'.                    CI0112
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0112
       01            S-GCU01-SSA.                                       CI0112
            10      S1-GCU01-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC01    '.                 CI0112
            10      S1-GCU01-CCOM   PICTURE X VALUE '*'.                CI0112
            10       S-GCU01-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            10      S1-GCU01-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(GC01K'.                   CI0112
            10       S-GCU01-OPER  PICTURE XX VALUE ' ='.               CI0112
            10       S-GCU01-GC01K.                                     CI0112
            11       S-GCU01-C299.                                      CI0112
            12       S-GCU01-CTID.                                      CI0112
            13       S-GCU01-CTIDA    PICTURE  9(3).                    CI0112
            13       S-GCU01-CTIDN.                                     CI0112
            14       S-GCU01-CTIDNP   PICTURE  X(13).                   CI0112
            14       S-GCU01-CTIDND   PICTURE  9(11).                   CI0112
            10  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01               S-GC03-SSA.                                     CI0112
            10         S1-GC03-SEGNAM PICTURE X(8)                      CI0112
                                      VALUE 'GC03    '.                 CI0112
            10         S1-GC03-CCOM   PICTURE X VALUE '*'.              CI0112
            10          S-GC03-CCOD   PICTURE X(5)                      CI0112
                                      VALUE '-----'.                    CI0112
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0112
       01            S-GCA03-SSA.                                       CI0112
            11      S1-GCA03-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GCA03-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCA03-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCA03-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(CAATY'.                   CI0112
            11       S-GCA03-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCA03-CAATY    PICTURE  9(3).                    CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GCB03-SSA.                                       CI0112
            11      S1-GCB03-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GCB03-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCB03-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCB03-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(CVSYS'.                   CI0112
            11       S-GCB03-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCB03-CVSYS    PICTURE  X(2).                    CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GCC03-SSA.                                       CI0112
            11      S1-GCC03-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GCC03-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCC03-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCC03-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(CASTC'.                   CI0112
            11       S-GCC03-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCC03-CASTC    PICTURE  99.                      CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GCD03-SSA.                                       CI0112
            11      S1-GCD03-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GCD03-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCD03-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCD03-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(CACTO'.                   CI0112
            11       S-GCD03-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCD03-CACTO    PICTURE  9(3).                    CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GCE03-SSA.                                       CI0112
            11      S1-GCE03-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GCE03-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCE03-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCE03-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(IPULL'.                   CI0112
            11       S-GCE03-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCE03-IPULL    PICTURE  X.                       CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GCF03-SSA.                                       CI0112
            11      S1-GCF03-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GCF03-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCF03-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCF03-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(DTRAC'.                   CI0112
            11       S-GCF03-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCF03-DTRAC    PICTURE  9(8).                    CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GCG03-SSA.                                       CI0112
            11      S1-GCG03-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GCG03-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCG03-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCG03-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(CTRSO'.                   CI0112
            11       S-GCG03-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCG03-CTRSO    PICTURE  9(02).                   CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GCH03-SSA.                                       CI0112
            11      S1-GCH03-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GCH03-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCH03-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCH03-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(NTRCE'.                   CI0112
            11       S-GCH03-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCH03-NTRCE    PICTURE  9(06).                   CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GCI03-SSA.                                       CI0112
            11      S1-GCI03-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GCI03-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCI03-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCI03-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(ITRAN'.                   CI0112
            11       S-GCI03-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCI03-ITRAN    PICTURE  X.                       CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GCJ03-SSA.                                       CI0112
            11      S1-GCJ03-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GCJ03-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCJ03-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCJ03-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(DEFFT'.                   CI0112
            11       S-GCJ03-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCJ03-DEFFT    PICTURE  9(8).                    CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GCK03-SSA.                                       CI0112
            11      S1-GCK03-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GCK03-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCK03-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCK03-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(CPMTCA'.                  CI0112
            11       S-GCK03-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCK03-CPMTCA   PICTURE  XXX.                     CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GCL03-SSA.                                       CI0112
            11      S1-GCL03-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GCL03-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCL03-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCL03-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(ACASH'.                   CI0112
            11       S-GCL03-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCL03-ACASH    PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GCN03-SSA.                                       CI0112
            11      S1-GCN03-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GCN03-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCN03-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCN03-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(CRREV'.                   CI0112
            11       S-GCN03-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCN03-CRREV    PICTURE  X(3).                    CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GCO03-SSA.                                       CI0112
            11      S1-GCO03-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GCO03-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCO03-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCO03-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(CSYST'.                   CI0112
            11       S-GCO03-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCO03-CSYST    PICTURE  99.                      CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GCU03-SSA.                                       CI0112
            11      S1-GCU03-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GCU03-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCU03-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCU03-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(GC03K'.                   CI0112
            11       S-GCU03-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCU03-GC03K.                                     CI0112
            12       S-GCU03-DCACG9   PICTURE  9(8).                    CI0112
            12       S-GCU03-NAASQ    PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GC103-SSA.                                       CI0112
            12      S1-GC103-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            12      S1-GC103-CCOM   PICTURE X VALUE '*'.                CI0112
            12       S-GC103-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            12      S1-GC103-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(XDCACG9'.                 CI0112
            12       S-GC103-OPER  PICTURE XX VALUE ' ='.               CI0112
            12       S-GC103-DCACG9   PICTURE  9(8).                    CI0112
            12  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GC203-SSA.                                       CI0112
            11      S1-GC203-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GC203-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GC203-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GC203-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(XGEAUN'.                  CI0112
            11       S-GC203-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GC203-GEAUN    PICTURE  9(5).                    CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GC303-SSA.                                       CI0112
            11      S1-GC303-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GC303-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GC303-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GC303-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(XGEOPD2'.                 CI0112
            11       S-GC303-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GC303-GEOPD2   PICTURE  X(8).                    CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GC403-SSA.                                       CI0112
            11      S1-GC403-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            11      S1-GC403-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GC403-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GC403-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(XNBTCH'.                  CI0112
            11       S-GC403-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GC403-NBTCH    PICTURE  9(4).                    CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GC803-SSA.                                       CI0112
            12      S1-GC803-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC03    '.                 CI0112
            12      S1-GC803-CCOM   PICTURE X VALUE '*'.                CI0112
            12       S-GC803-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            12      S1-GC803-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(XNAASQ'.                  CI0112
            12       S-GC803-OPER  PICTURE XX VALUE ' ='.               CI0112
            12       S-GC803-NAASQ    PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            12  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01               S-GC04-SSA.                                     CI0112
            10         S1-GC04-SEGNAM PICTURE X(8)                      CI0112
                                      VALUE 'GC04    '.                 CI0112
            10         S1-GC04-CCOM   PICTURE X VALUE '*'.              CI0112
            10          S-GC04-CCOD   PICTURE X(5)                      CI0112
                                      VALUE '-----'.                    CI0112
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0112
       01               S-GC06-SSA.                                     CI0112
            10         S1-GC06-SEGNAM PICTURE X(8)                      CI0112
                                      VALUE 'GC06    '.                 CI0112
            10         S1-GC06-CCOM   PICTURE X VALUE '*'.              CI0112
            10          S-GC06-CCOD   PICTURE X(5)                      CI0112
                                      VALUE '-----'.                    CI0112
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0112
       01            S-GCF06-SSA.                                       CI0112
            11      S1-GCF06-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC06    '.                 CI0112
            11      S1-GCF06-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCF06-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCF06-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(PRCOD1'.                  CI0112
            11       S-GCF06-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCF06-PRCOD1   PICTURE  9(5).                    CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01            S-GCU06-SSA.                                       CI0112
            11      S1-GCU06-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC06    '.                 CI0112
            11      S1-GCU06-CCOM   PICTURE X VALUE '*'.                CI0112
            11       S-GCU06-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            11      S1-GCU06-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(GC06K'.                   CI0112
            11       S-GCU06-OPER  PICTURE XX VALUE ' ='.               CI0112
            11       S-GCU06-GC06K.                                     CI0112
            12       S-GCU06-NPISQ    PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01               S-GC12-SSA.                                     CI0112
            10         S1-GC12-SEGNAM PICTURE X(8)                      CI0112
                                      VALUE 'GC12    '.                 CI0112
            10         S1-GC12-CCOM   PICTURE X VALUE '*'.              CI0112
            10          S-GC12-CCOD   PICTURE X(5)                      CI0112
                                      VALUE '-----'.                    CI0112
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0112
       01            S-GCU12-SSA.                                       CI0112
            10      S1-GCU12-SEGNAM PICTURE X(8)                        CI0112
                                      VALUE 'GC12    '.                 CI0112
            10      S1-GCU12-CCOM   PICTURE X VALUE '*'.                CI0112
            10       S-GCU12-CCOD   PICTURE X(5)                        CI0112
                                      VALUE '-----'.                    CI0112
            10      S1-GCU12-FLDNAM PICTURE X(9)                        CI0112
                                      VALUE '(GC12K'.                   CI0112
            10       S-GCU12-OPER  PICTURE XX VALUE ' ='.               CI0112
            10       S-GCU12-GC12K.                                     CI0112
            11       S-GCU12-CIRAP    PICTURE  XX.                      CI0112
            10  FILLER   PICTURE X    VALUE ')'.                        CI0112
       01               S-GC21-SSA.                                     CI0112
            10         S1-GC21-SEGNAM PICTURE X(8)                      CI0112
                                      VALUE 'GC21    '.                 CI0112
            10         S1-GC21-CCOM   PICTURE X VALUE '*'.              CI0112
            10          S-GC21-CCOD   PICTURE X(5)                      CI0112
                                      VALUE '-----'.                    CI0112
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0112
       01               S-GC29-SSA.                                     CI0112
            10         S1-GC29-SEGNAM PICTURE X(8)                      CI0112
                                      VALUE 'GC29    '.                 CI0112
            10         S1-GC29-CCOM   PICTURE X VALUE '*'.              CI0112
            10          S-GC29-CCOD   PICTURE X(5)                      CI0112
                                      VALUE '-----'.                    CI0112
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0112
       01   ZONES-UTILISATEUR PICTURE X.                                CI0112

      *NEEDED FOR AUDIT LOG PROCESSING
      *!WF DSP=K1 DSL=K1 SEL=1O FOR=I LEV=1 PLT=K1
       01                 K100.                                         CI0112
          05              K100-SUITE.                                   CI0112
            15       FILLER         PICTURE  X(00001).                  CI0112
       01                 K11O  REDEFINES      K100.                    CI0112
            10            K11O-IPLIN  PICTURE  X.                       CI0112

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
       01                 PA00.                                         CI0112
          05              PA00-SUITE.                                   CI0112
            15       FILLER         PICTURE  X(00106).                  CI0112
       01                 PA06  REDEFINES      PA00.                    CI0112
            10            PA06-XDBPCB.                                  CI0112
            11            PA06-XDBDNM PICTURE  X(08).                   CI0112
            11            PA06-XSEGLV PICTURE  X(02).                   CI0112
            11            PA06-XRC    PICTURE  X(02).                   CI0112
            11            PA06-XPROPT PICTURE  X(04).                   CI0112
            11            PA06-FILLER PICTURE  S9(5)                    CI0112
                          BINARY.                                       CI0112
            11            PA06-XSEGNM PICTURE  X(08).                   CI0112
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0112
                          BINARY.                                       CI0112
            11            PA06-XSEGNB PICTURE  9(05)                    CI0112
                          BINARY.                                       CI0112
            11            PA06-XCOKEY PICTURE  X(70).                   CI0112

      *PASS AREA TO CI0112
      *!WF DSP=PJ DSL=WM SEL=48 FOR=I DES=1 LEV=1 PLT=10
       01                 PJ18.                                         CI0112
            10            PJ18-CSLCT  PICTURE  X.                       CI0112
            10            PJ18-NGEOR  PICTURE  9(08).                   CI0112
            10            PJ18-CACLS2 PICTURE  X(20).                   CI0112
            10            PJ18-CAPID  PICTURE  9(2).                    CI0112
            10            PJ18-NGEOPA PICTURE  X(08).                   CI0112
            10            PJ18-CACLS1 PICTURE  X(20).                   CI0112
            10            PJ18-CTRHO  PICTURE  9(8).                    CI0112
            10            PJ18-GETIM3 PICTURE  S9(7)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            PJ18-GEOPD9 PICTURE  X(8).                    CI0112
            10            PJ18-DCACG1 PICTURE  9(8).                    CI0112
            10            PJ18-IWEBBT PICTURE  X.                       CI0112
            10            PJ18-CAVER  PICTURE  X.                       CI0112
            10            PJ18-FILLER PICTURE  X(30).                   CI0112
       01                 PJ48.                                         CI0112
            10            PJ48-MAPPN  PICTURE  X(10).                   CI0112
            10            PJ48-ICUST  PICTURE  X.                       CI0112
            10            PJ48-ITELR  PICTURE  X.                       CI0112
            10            PJ48-IMUDT  PICTURE  X.                       CI0112
            10            PJ48-ACASH  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            PJ48-CACTS  PICTURE  X.                       CI0112
            10            PJ48-FILLER PICTURE  X(39).                   CI0112
      *!WF DSP=PJ DSL=V2 SEL=18 FOR=I LEV=1 PLT=10
       01                 LC01.                                         CI0112
            10            LC01-GC01K.                                   CI0112
            11            LC01-C299.                                    CI0112
            12            LC01-CTID.                                    CI0112
            13            LC01-CTIDA  PICTURE  9(3).                    CI0112
            13            LC01-CTIDN.                                   CI0112
            14            LC01-CTIDNP PICTURE  X(13).                   CI0112
            14            LC01-CTIDND PICTURE  9(11).                   CI0112
            10            LC01-DCAG9L PICTURE  9(8).                    CI0112
            10            LC01-NAASQL PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            LC01-ICUST  PICTURE  X.                       CI0112
            10            LC01-NSEQ4B PICTURE  9(8)                     CI0112
                          BINARY.                                       CI0112
            10            LC01-PRCOD  PICTURE  9(5).                    CI0112
            10            LC01-PRSCD  PICTURE  X(9).                    CI0112
            10            LC01-FILLER PICTURE  X(8).                    CI0112
       01                 LC03.                                         CI0112
            10            LC03-GELL   PICTURE  9(4)                     CI0112
                          BINARY.                                       CI0112
            10            LC03-GD00.                                    CI0112
            11            LC03-GC03K.                                   CI0112
            12            LC03-DCACG9 PICTURE  9(8).                    CI0112
            12            LC03-NAASQ  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-CAATY  PICTURE  9(3).                    CI0112
            11            LC03-CVSYS  PICTURE  X(2).                    CI0112
            11            LC03-CACTO  PICTURE  9(3).                    CI0112
            11            LC03-CATRN.                                   CI0112
            12            LC03-CATRF  PICTURE  9(3).                    CI0112
            12            LC03-CATRS  PICTURE  9(3).                    CI0112
            11            LC03-CASTC  PICTURE  99.                      CI0112
            11            LC03-IPULL  PICTURE  X.                       CI0112
            11            LC03-GEAUN  PICTURE  9(5).                    CI0112
            11            LC03-GEOPD2 PICTURE  X(8).                    CI0112
            11            LC03-NBTCH  PICTURE  9(4).                    CI0112
            11            LC03-DEFFT  PICTURE  9(8).                    CI0112
            11            LC03-NSUNT  PICTURE  9(4).                    CI0112
            11            LC03-ITRAN  PICTURE  X.                       CI0112
            11            LC03-DLAUP1 PICTURE  9(8).                    CI0112
            11            LC03-ADRET  PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-TTRMS  PICTURE  X(12).                   CI0112
            11            LC03-IDELT  PICTURE  X.                       CI0112
            11            LC03-GEOPDM PICTURE  X(8).                    CI0112
            11            LC03-FILLER PICTURE  X(07).                   CI0112
            10            LC03-GD09.                                    CI0112
            11            LC03-FILLER PICTURE  X(70).                   CI0112
            10            LC03-GD01                                     CI0112
                          REDEFINES            LC03-GD09.               CI0112
            11            LC03-ADBRQ  PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-CTRTP  PICTURE  X(2).                    CI0112
            11            LC03-CPORT  PICTURE  X.                       CI0112
            11            LC03-CSCRNU PICTURE  X(4).                    CI0112
            11            LC03-DLAUP  PICTURE  9(8).                    CI0112
            11            LC03-CTWHAT PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-PWHLD  PICTURE  S999V9(5)                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-IWTHH  PICTURE  X.                       CI0112
            11            LC03-NDRFT  PICTURE  9(5).                    CI0112
            11            LC03-IDPAP  PICTURE  X.                       CI0112
            11            LC03-GETIM  PICTURE  S9(7)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-QNACT  PICTURE  9(3).                    CI0112
            11            LC03-AEDRQ  PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-IPLIN  PICTURE  X.                       CI0112
            11            LC03-CLIDNB PICTURE  9(8).                    CI0112
            11            LC03-CSLCT  PICTURE  X.                       CI0112
            11            LC03-ITELE  PICTURE  X.                       CI0112
            11            LC03-FILLER PICTURE  X(06).                   CI0112
            10            LC03-GD02                                     CI0112
                          REDEFINES            LC03-GD09.               CI0112
            11            LC03-CSYST  PICTURE  99.                      CI0112
            11            LC03-FILLER PICTURE  X.                       CI0112
            11            LC03-ACASH  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-DTRAC  PICTURE  9(8).                    CI0112
            11            LC03-CTRSO  PICTURE  9(02).                   CI0112
            11            LC03-NTRCE  PICTURE  9(06).                   CI0112
            11            LC03-GECKD1 PICTURE  9.                       CI0112
            11            LC03-CCOLL  PICTURE  X(3).                    CI0112
            11            LC03-CLTDP  PICTURE  X(3).                    CI0112
            11            LC03-PSLLD  PICTURE  S99V999                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-ISLOR  PICTURE  X.                       CI0112
            11            LC03-ITPAC  PICTURE  X.                       CI0112
            11            LC03-CPMTCA PICTURE  XXX.                     CI0112
            11            LC03-CSERV  PICTURE  X(3).                    CI0112
            11            LC03-ACOMO  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-IPLIN1 PICTURE  X.                       CI0112
            11            LC03-INQEX  PICTURE  X.                       CI0112
            11            LC03-CTKRAA PICTURE  X(12).                   CI0112
            11            LC03-CCSMQ  PICTURE  X.                       CI0112
            11            LC03-IVAEX1 PICTURE  X.                       CI0112
            11            LC03-IHPMT  PICTURE  X(1).                    CI0112
            11            LC03-GETIM3 PICTURE  S9(7)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            LC03-GD03                                     CI0112
                          REDEFINES            LC03-GD09.               CI0112
            11            LC03-CATRNC PICTURE  9(6).                    CI0112
            11            LC03-APRNT1 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-QSHOWT PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-ACINVT PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-ACOMO7 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-QSHOMW PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-ATAXT3 PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-CTSTR  PICTURE  9(2).                    CI0112
            11            LC03-ICIRA  PICTURE  X.                       CI0112
            11            LC03-GETIM2 PICTURE  S9(7)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-CPMTCX PICTURE  XX.                      CI0112
            11            LC03-FILLER PICTURE  X(16).                   CI0112
            10            LC03-GD99.                                    CI0112
            11            LC03-FILLER PICTURE  X(248).                  CI0112
            10            LC03-GD10                                     CI0112
                          REDEFINES            LC03-GD99.               CI0112
            11            LC03-MROTC  PICTURE  X(7).                    CI0112
            11            LC03-CEDSC  PICTURE  9(1).                    CI0112
            11            LC03-ILPOI  PICTURE  X(1).                    CI0112
            11            LC03-AWRCH  PICTURE  S9(3)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-CHCOC1 PICTURE  9(2).                    CI0112
            11            LC03-CHCOC2 PICTURE  9(2).                    CI0112
            11            LC03-CHCOC3 PICTURE  9(2).                    CI0112
            11            LC03-CHCOC4 PICTURE  9(2).                    CI0112
            11            LC03-CMCOC1 PICTURE  9(3).                    CI0112
            11            LC03-CMCOC2 PICTURE  9(3).                    CI0112
            11            LC03-CMCOC3 PICTURE  9(3).                    CI0112
            11            LC03-GD11.                                    CI0112
            12            LC03-FILLER PICTURE  X(219).                  CI0112
            11            LC03-GD12                                     CI0112
                          REDEFINES            LC03-GD11.               CI0112
            12            LC03-CELLO  PICTURE  9(1).                    CI0112
            12            LC03-CECLO  PICTURE  9(1).                    CI0112
            12            LC03-AEXML  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-CEPI   PICTURE  X(1).                    CI0112
            12            LC03-CEXTY  PICTURE  X.                       CI0112
            12            LC03-CROPC  PICTURE  9(1).                    CI0112
            12            LC03-CPUTY  PICTURE  9(1).                    CI0112
            12            LC03-IMCII  PICTURE  X(1).                    CI0112
            12            LC03-GEMISC                                   CI0112
                          OCCURS       010     TIMES.                   CI0112
            13            LC03-AMGLA  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            13            LC03-CMGLC  PICTURE  9(1).                    CI0112
            13            LC03-NMGLN  PICTURE  9(4).                    CI0112
            12            LC03-ACTRN  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-IWRBK  PICTURE  X.                       CI0112
            12            LC03-IFEDX  PICTURE  X.                       CI0112
            12            LC03-ICNTR  PICTURE  X.                       CI0112
            12            LC03-IOCKH  PICTURE  X.                       CI0112
            12            LC03-ICRCK  PICTURE  X.                       CI0112
            12            LC03-NHMPN  PICTURE  S9(10)                   CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-ITELR1 PICTURE  X.                       CI0112
            11            LC03-GD13                                     CI0112
                          REDEFINES            LC03-GD11.               CI0112
            12            LC03-DREDO  PICTURE  9(8).                    CI0112
            12            LC03-CATRNR PICTURE  9(6).                    CI0112
            12            LC03-CEVN   PICTURE  9(9).                    CI0112
            12            LC03-ISUSP  PICTURE  X(1).                    CI0112
            11            LC03-GD15                                     CI0112
                          REDEFINES            LC03-GD11.               CI0112
            12            LC03-CPUTZ  PICTURE  9(1).                    CI0112
            12            LC03-CETLB  PICTURE  9(3).                    CI0112
            12            LC03-QTRMC  PICTURE  9(3).                    CI0112
            12            LC03-DEFFTE PICTURE  9(8).                    CI0112
            12            LC03-DEFFTF PICTURE  9(8).                    CI0112
            12            LC03-DEFFTG PICTURE  9(8).                    CI0112
            12            LC03-XZ1A   PICTURE  X.                       CI0112
            12            LC03-XZ1B   PICTURE  X.                       CI0112
            12            LC03-XZ1C   PICTURE  X.                       CI0112
            12            LC03-XZ1D   PICTURE  X.                       CI0112
            12            LC03-XZ1E   PICTURE  X.                       CI0112
            12            LC03-XZ1F   PICTURE  X.                       CI0112
            12            LC03-XZ1G   PICTURE  X.                       CI0112
            12            LC03-XZ1H   PICTURE  X.                       CI0112
            12            LC03-XZ1I   PICTURE  X.                       CI0112
            12            LC03-DEFFTH PICTURE  9(8).                    CI0112
            11            LC03-GD19                                     CI0112
                          REDEFINES            LC03-GD11.               CI0112
            12            LC03-GD11.                                    CI0112
            13            LC03-FILLER PICTURE  X(219).                  CI0112
            10            LC03-GD20                                     CI0112
                          REDEFINES            LC03-GD99.               CI0112
            11            LC03-ADDACT PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-ISIGV  PICTURE  X.                       CI0112
            11            LC03-IALLF  PICTURE  X.                       CI0112
            11            LC03-QSHOWQ PICTURE  S9(9)V999                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-CCDSCW PICTURE  9(2).                    CI0112
            11            LC03-IDWRL  PICTURE  X.                       CI0112
            11            LC03-ITELR  PICTURE  X.                       CI0112
            11            LC03-IABIN  PICTURE  X.                       CI0112
            11            LC03-PACT1  PICTURE  S999V999                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-IBFAF  PICTURE  X.                       CI0112
            11            LC03-IFRSA  PICTURE  X.                       CI0112
            11            LC03-ICRCAN PICTURE  X.                       CI0112
            11            LC03-ACACTV PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-AGFND  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-QCSHOW PICTURE  S9(9)V999                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-QCSHIS PICTURE  S9(9)V999                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-NDTRC  PICTURE  9(8).                    CI0112
            11            LC03-CAERU  PICTURE  X(4).                    CI0112
            11            LC03-IFDGO  PICTURE  X.                       CI0112
            11            LC03-PSLLD2 PICTURE  S99V999                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-ISLOR2 PICTURE  X.                       CI0112
            11            LC03-QSFIO  PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-QSFID  PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-CGDIN  PICTURE  X.                       CI0112
            11            LC03-DGDIN  PICTURE  9(8).                    CI0112
            10            LC03-GD30                                     CI0112
                          REDEFINES            LC03-GD99.               CI0112
            11            LC03-ISKED  PICTURE  X.                       CI0112
            11            LC03-CENXC  PICTURE  9(2).                    CI0112
            11            LC03-GD31.                                    CI0112
            12            LC03-FILLER PICTURE  X(245).                  CI0112
            11            LC03-GD32                                     CI0112
                          REDEFINES            LC03-GD31.               CI0112
            12            LC03-IABIN1 PICTURE  X.                       CI0112
            12            LC03-CLDOD  PICTURE  9(8).                    CI0112
            12            LC03-NCLAM  PICTURE  9(5)                     CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-ISURR  PICTURE  X.                       CI0112
            12            LC03-GEHCD  PICTURE  9(3).                    CI0112
            12            LC03-CRATC  PICTURE  9(4).                    CI0112
            12            LC03-AMAXD  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-ASCHGA PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-APYOM  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-IWTHH1 PICTURE  X.                       CI0112
            12            LC03-CPAYCL PICTURE  X(2).                    CI0112
            12            LC03-CTSAO  PICTURE  X.                       CI0112
            12            LC03-NCONF  PICTURE  9(08).                   CI0112
            12            LC03-CLID   PICTURE  X(23).                   CI0112
            12            LC03-CARTY  PICTURE  99.                      CI0112
            12            LC03-NARRS  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-CARTZ  PICTURE  99.                      CI0112
            12            LC03-NAPDS  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-CPMTO  PICTURE  X.                       CI0112
            12            LC03-DNPMT  PICTURE  9(8).                    CI0112
            12            LC03-IPCTV  PICTURE  X.                       CI0112
            12            LC03-IMECH  PICTURE  X(01).                   CI0112
            12            LC03-IMVAO  PICTURE  X(1).                    CI0112
            12            LC03-AMVA1  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-CACTS  PICTURE  X.                       CI0112
            12            LC03-CTSPP  PICTURE  X(1).                    CI0112
            12            LC03-CACT4  PICTURE  X(2).                    CI0112
            12            LC03-IVAEX  PICTURE  X.                       CI0112
            12            LC03-DFPMT  PICTURE  9(8).                    CI0112
            12            LC03-IDEMD  PICTURE  X.                       CI0112
            12            LC03-IOFST  PICTURE  X.                       CI0112
            12            LC03-AMXLB  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-ACULB  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-DEIRNB PICTURE  9(8).                    CI0112
            12            LC03-DEFFE  PICTURE  9(8).                    CI0112
            12            LC03-DEFFR  PICTURE  9(8).                    CI0112
            12            LC03-ISPUP  PICTURE  X.                       CI0112
            12            LC03-CPNCG  PICTURE  X.                       CI0112
            12            LC03-IEXPU  PICTURE  X.                       CI0112
            12            LC03-IPPCF  PICTURE  X.                       CI0112
            12            LC03-NAAPT  PICTURE  9(2).                    CI0112
            12            LC03-PWHLDS PICTURE  S999V9(5)                CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-ISWHO  PICTURE  X(1).                    CI0112
            11            LC03-GD33                                     CI0112
                          REDEFINES            LC03-GD31.               CI0112
            12            LC03-CPAYC  PICTURE  X(2).                    CI0112
            12            LC03-ADBRQX PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-ADBRQV PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-APTXR  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-CTRTPE PICTURE  X(2).                    CI0112
            12            LC03-NCLAMI PICTURE  S9(9)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-CLIDO8 PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-CLIDN  PICTURE  X(20).                   CI0112
            12            LC03-DSET01 PICTURE  S9(8)                    CI0112
                          BINARY.                                       CI0112
            12            LC03-CTSET1 PICTURE  S9(6)                    CI0112
                          BINARY.                                       CI0112
            12            LC03-DSET02 PICTURE  S9(8)                    CI0112
                          BINARY.                                       CI0112
            12            LC03-CTSET2 PICTURE  S9(6)                    CI0112
                          BINARY.                                       CI0112
            11            LC03-GD34                                     CI0112
                          REDEFINES            LC03-GD31.               CI0112
            12            LC03-QNOFM  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-CLTRM  PICTURE  99.                      CI0112
            12            LC03-AMXLN  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-ALADJ  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-ACHK   PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-APRMO  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-IMECH1 PICTURE  X(01).                   CI0112
            12            LC03-CACT41 PICTURE  X(2).                    CI0112
            12            LC03-ACDSCC PICTURE  S9(05)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-ACDSCD PICTURE  S9(05)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-GD39                                     CI0112
                          REDEFINES            LC03-GD31.               CI0112
            12            LC03-GD31.                                    CI0112
            13            LC03-FILLER PICTURE  X(245).                  CI0112
            10            LC03-GD40                                     CI0112
                          REDEFINES            LC03-GD99.               CI0112
            11            LC03-NTR    PICTURE  9(8).                    CI0112
            11            LC03-NPBNC  PICTURE  X(24).                   CI0112
            11            LC03-CRREV  PICTURE  X(3).                    CI0112
            11            LC03-CSUSL  PICTURE  S9.                      CI0112
            11            LC03-NMGLN1 PICTURE  9(4).                    CI0112
            11            LC03-DCAC92 PICTURE  9(8).                    CI0112
            11            LC03-NAASQ3 PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-GD49.                                    CI0112
            12            LC03-FILLER PICTURE  X(198).                  CI0112
            11            LC03-GD41                                     CI0112
                          REDEFINES            LC03-GD49.               CI0112
            12            LC03-CRREF  PICTURE  9(2).                    CI0112
            12            LC03-CORIR  PICTURE  X(02).                   CI0112
            12            LC03-CIPDB  PICTURE  X(03).                   CI0112
            12            LC03-CPAYH  PICTURE  X(02).                   CI0112
            12            LC03-NAMEX  PICTURE  9(15)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC03-DCHAE  PICTURE  9(4).                    CI0112
            12            LC03-DRQST  PICTURE  S9(8)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-GD42                                     CI0112
                          REDEFINES            LC03-GD49.               CI0112
            12            LC03-CPMTCB PICTURE  X(3).                    CI0112
            10            LC03-GD50                                     CI0112
                          REDEFINES            LC03-GD99.               CI0112
            11            LC03-ALOAD  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-PSLLD4 PICTURE  S99V999                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-CSUSL1 PICTURE  S9.                      CI0112
            11            LC03-CRREV1 PICTURE  X(3).                    CI0112
            11            LC03-ADDAC  PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-DL13.                                    CI0112
            12            LC03-GEYR   PICTURE  9(4).                    CI0112
            12            LC03-GEMTH  PICTURE  99.                      CI0112
            12            LC03-NDAY   PICTURE  99.                      CI0112
            11            LC03-NSEQ3P PICTURE  S9(5)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-XZ6A   PICTURE  X(6).                    CI0112
            11            LC03-XZ7    PICTURE  X(7).                    CI0112
            11            LC03-XZ6B   PICTURE  X(6).                    CI0112
            11            LC03-XZ6    PICTURE  X(6).                    CI0112
            11            LC03-XZ6C   PICTURE  X(6).                    CI0112
            11            LC03-XZ20   PICTURE  X(20).                   CI0112
            11            LC03-CATRN1 PICTURE  9(6).                    CI0112
            11            LC03-ADDAC2 PICTURE  S9(7)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-ATAXT2 PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-ACOMOT PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-XZ5    PICTURE  X(5).                    CI0112
            11            LC03-IREVD  PICTURE  X(1).                    CI0112
            11            LC03-ISUSP1 PICTURE  X(1).                    CI0112
            11            LC03-XZ6D   PICTURE  X(6).                    CI0112
            11            LC03-XZ13   PICTURE  X(13).                   CI0112
            11            LC03-CWHTP2 PICTURE  X(3).                    CI0112
            11            LC03-CWHTP3 PICTURE  X(3).                    CI0112
            11            LC03-DTREN  PICTURE  9(8).                    CI0112
            11            LC03-NAASQ1 PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            LC03-GD51                                     CI0112
                          REDEFINES            LC03-GD99.               CI0112
            11            LC03-ADOMOT PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-ACGLT  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-ACGST  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-CTXMT  PICTURE  9(2).                    CI0112
            11            LC03-ALOAD3 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-FILLER PICTURE  X(31).                   CI0112
            10            LC03-GD52                                     CI0112
                          REDEFINES            LC03-GD99.               CI0112
            11            LC03-DEFFT5 PICTURE  9(8).                    CI0112
            11            LC03-PSLLD5 PICTURE  S99V999                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-CSUSL2 PICTURE  S9.                      CI0112
            11            LC03-ALOAD2 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-DL22.                                    CI0112
            12            LC03-NYEAR1 PICTURE  9(4).                    CI0112
            12            LC03-GEMTHA PICTURE  99.                      CI0112
            12            LC03-NDAY01 PICTURE  99.                      CI0112
            11            LC03-NSEQ3R PICTURE  S9(5)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-CWHTP  PICTURE  X(3).                    CI0112
            11            LC03-CWHFR  PICTURE  X(3).                    CI0112
            11            LC03-CATRN7 PICTURE  9(6).                    CI0112
            11            LC03-ATAXT5 PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-QSHOT  PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-ACINT3 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-CWHTP1 PICTURE  X(3).                    CI0112
            11            LC03-CWHFR1 PICTURE  X(3).                    CI0112
            11            LC03-ACOMO5 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-QSHOMU PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-ACASH1 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-FILLER PICTURE  X(04).                   CI0112
            11            LC03-CATRN8 PICTURE  9(6).                    CI0112
            11            LC03-ALOAD1 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-PSLLD1 PICTURE  S99V999                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-QSHOT1 PICTURE  S9(10)V999               CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-ACINT4 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-CSUSL4 PICTURE  S9.                      CI0112
            11            LC03-ACOMO4 PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            LC03-GD60                                     CI0112
                          REDEFINES            LC03-GD99.               CI0112
            11            LC03-GEOPDD PICTURE  X(8)                     CI0112
                          OCCURS       005     TIMES.                   CI0112
            11            LC03-DLAUP3 PICTURE  9(8)                     CI0112
                          OCCURS       005     TIMES.                   CI0112
            11            LC03-GEOPDB PICTURE  X(8).                    CI0112
            11            LC03-DLAUP4 PICTURE  9(8).                    CI0112
            11            LC03-ITELR2 PICTURE  X.                       CI0112
            11            LC03-IPMTA  PICTURE  X.                       CI0112
            11            LC03-CCSMG  PICTURE  X.                       CI0112
            11            LC03-CPLEC  PICTURE  XX.                      CI0112
            11            LC03-CORTYA PICTURE  X(3).                    CI0112
            11            LC03-CACTBC PICTURE  X(1).                    CI0112
            11            LC03-CGSPIA PICTURE  X.                       CI0112
            11            LC03-IPTRDA PICTURE  X(01).                   CI0112
            11            LC03-GCUSPY PICTURE  X(12).                   CI0112
            11            LC03-CPALLA PICTURE  X(1).                    CI0112
            11            LC03-QSHO5A PICTURE  S9(9)V999                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-IFRSAB PICTURE  X.                       CI0112
            11            LC03-DELOI  PICTURE  9(8).                    CI0112
            11            LC03-IAROAA PICTURE  X.                       CI0112
            11            LC03-ACINVR PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-ILTINA PICTURE  X.                       CI0112
            11            LC03-ALOIDA PICTURE  S9(11)V99                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC03-CFUNTA PICTURE  X(2).                    CI0112
            11            LC03-CLGND  PICTURE  X.                       CI0112
            11            LC03-CPH3U  PICTURE  X.                       CI0112
            11            LC03-GESTD  PICTURE  9(8).                    CI0112
            11            LC03-GEEND  PICTURE  9(8).                    CI0112
            11            LC03-CPMTF  PICTURE  99.                      CI0112
            11            LC03-CNAVR  PICTURE  X(1).                    CI0112
            10            LC03-GD70                                     CI0112
                          REDEFINES            LC03-GD99.               CI0112
            11            LC03-CMEMO  PICTURE  X(2).                    CI0112
            11            LC03-ALPLDT PICTURE  9(8).                    CI0112
            11            LC03-CTLPD  PICTURE  9(8).                    CI0112
            11            LC03-CPAYCM PICTURE  X(2).                    CI0112
       01                 LC04.                                         CI0112
            10            LC04-CLCUS  PICTURE  99.                      CI0112
            10            LC04-CCACT  PICTURE  99.                      CI0112
            10            LC04-AFEET  PICTURE  S9(5)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            LC04-ITERF  PICTURE  X.                       CI0112
            10            LC04-ATERF  PICTURE  S9(5)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            LC04-CLDOB  PICTURE  9(8).                    CI0112
            10            LC04-CPLTYP PICTURE  X(14).                   CI0112
            10            LC04-IACFPD PICTURE  X(1).                    CI0112
            10            LC04-FILLER PICTURE  X(14).                   CI0112
       01                 LC06.                                         CI0112
            10            LC06-GELL   PICTURE  9(4)                     CI0112
                          BINARY.                                       CI0112
            10            LC06-GE00.                                    CI0112
            11            LC06-GC06K.                                   CI0112
            12            LC06-NPISQ  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC06-ACOTD  PICTURE  S9(9)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC06-PPOTD  PICTURE  S9(3)V99                 CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC06-QPSTD  PICTURE  S9(7)V999                CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC06-CPITC  PICTURE  99.                      CI0112
            11            LC06-ITRNB  PICTURE  X.                       CI0112
            11            LC06-FILLER PICTURE  X(14).                   CI0112
            10            LC06-GE98.                                    CI0112
            11            LC06-FILLER PICTURE  X(240).                  CI0112
            10            LC06-GE10                                     CI0112
                          REDEFINES            LC06-GE98.               CI0112
            11            LC06-CDELI  PICTURE  9(3).                    CI0112
            11            LC06-CPAYC  PICTURE  X(2).                    CI0112
            11            LC06-ICHKP  PICTURE  X.                       CI0112
            11            LC06-CLTIN  PICTURE  9(12).                   CI0112
            11            LC06-IFHAI  PICTURE  X.                       CI0112
            11            LC06-CDQUA  PICTURE  X(2).                    CI0112
            11            LC06-FILLER PICTURE  X(07).                   CI0112
            11            LC06-GE99.                                    CI0112
            12            LC06-FILLER PICTURE  X(212).                  CI0112
            11            LC06-GE01                                     CI0112
                          REDEFINES            LC06-GE99.               CI0112
            12            LC06-NTR    PICTURE  9(8).                    CI0112
            12            LC06-GECKD  PICTURE  9.                       CI0112
            12            LC06-NPBN   PICTURE  X(20).                   CI0112
            12            LC06-CCBAT  PICTURE  99.                      CI0112
            12            LC06-CLID4  PICTURE  X(23).                   CI0112
            12            LC06-GENAL1 PICTURE  X(30)                    CI0112
                          OCCURS       002     TIMES.                   CI0112
            12            LC06-GESAD1 PICTURE  X(30)                    CI0112
                          OCCURS       003     TIMES.                   CI0112
            11            LC06-GE02                                     CI0112
                          REDEFINES            LC06-GE99.               CI0112
            12            LC06-GENAL  PICTURE  X(30)                    CI0112
                          OCCURS       002     TIMES.                   CI0112
            12            LC06-GESAD  PICTURE  X(30)                    CI0112
                          OCCURS       003     TIMES.                   CI0112
            11            LC06-GE03                                     CI0112
                          REDEFINES            LC06-GE99.               CI0112
            12            LC06-NCHKN  PICTURE  9(11).                   CI0112
            11            LC06-GE04                                     CI0112
                          REDEFINES            LC06-GE99.               CI0112
            12            LC06-CTIDAP PICTURE  9(3).                    CI0112
            12            LC06-PRCOD  PICTURE  9(5).                    CI0112
            12            LC06-TDELI  PICTURE  X(30).                   CI0112
            12            LC06-CINCD  PICTURE  9(02).                   CI0112
            10            LC06-GE20                                     CI0112
                          REDEFINES            LC06-GE98.               CI0112
            11            LC06-C299.                                    CI0112
            12            LC06-CTID.                                    CI0112
            13            LC06-CTIDA  PICTURE  9(3).                    CI0112
            13            LC06-CTIDN.                                   CI0112
            14            LC06-CTIDNP PICTURE  X(13).                   CI0112
            14            LC06-CTIDND PICTURE  9(11).                   CI0112
            11            LC06-DCACG9 PICTURE  9(8).                    CI0112
            11            LC06-NAASQ  PICTURE  S9(3)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            LC06-CIRAP  PICTURE  XX.                      CI0112
            11            LC06-CTYPE  PICTURE  X.                       CI0112
            11            LC06-INACT  PICTURE  X.                       CI0112
            11            LC06-FILLER PICTURE  X(01).                   CI0112
            11            LC06-ITPAC  PICTURE  X.                       CI0112
            11            LC06-ITAXI  PICTURE  X.                       CI0112
            11            LC06-IOWNC  PICTURE  X.                       CI0112
            11            LC06-CDVCD  PICTURE  X(2).                    CI0112
            11            LC06-CTCUS  PICTURE  999.                     CI0112
            11            LC06-CPMTCB PICTURE  X(3).                    CI0112
            11            LC06-CASTC1 PICTURE  99.                      CI0112
            11            LC06-PRCOD1 PICTURE  9(5).                    CI0112
            11            LC06-CPRSC1 PICTURE  X(9).                    CI0112
            11            LC06-CPRTB  PICTURE  X.                       CI0112
            11            LC06-CBRKD  PICTURE  9(4).                    CI0112
            11            LC06-FILLER PICTURE  X(12).                   CI0112
            10            LC06-GE30                                     CI0112
                          REDEFINES            LC06-GE98.               CI0112
            11            LC06-CFIDC  PICTURE  X(5).                    CI0112
            11            LC06-CPHSE  PICTURE  9(2).                    CI0112
            11            LC06-FILLER PICTURE  X(05).                   CI0112
            11            LC06-IABIN  PICTURE  X.                       CI0112
            11            LC06-PDFND  PICTURE  S999V9(3)                CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            LC06-GE40                                     CI0112
                          REDEFINES            LC06-GE98.               CI0112
            11            LC06-CACCT  PICTURE  X.                       CI0112
            11            LC06-CPAYR  PICTURE  X(2).                    CI0112
            11            LC06-CDELI1 PICTURE  9(3).                    CI0112
            11            LC06-CATRN.                                   CI0112
            12            LC06-CATRF  PICTURE  9(3).                    CI0112
            12            LC06-CATRS  PICTURE  9(3).                    CI0112
            11            LC06-DEFFT  PICTURE  9(8).                    CI0112
            11            LC06-CTYPC  PICTURE  X.                       CI0112
            11            LC06-CIRAPA PICTURE  XX.                      CI0112
            11            LC06-FILLER PICTURE  X(09).                   CI0112
            11            LC06-GE49.                                    CI0112
            12            LC06-FILLER PICTURE  X(208).                  CI0112
            11            LC06-GE41                                     CI0112
                          REDEFINES            LC06-GE49.               CI0112
            12            LC06-NCHKN1 PICTURE  9(6).                    CI0112
            11            LC06-GE42                                     CI0112
                          REDEFINES            LC06-GE49.               CI0112
            12            LC06-CTID1.                                   CI0112
            13            LC06-CTIDA1 PICTURE  9(3).                    CI0112
            13            LC06-CTIDP1 PICTURE  X(13).                   CI0112
            13            LC06-CTIDN1 PICTURE  9(11).                   CI0112
            11            LC06-GE43                                     CI0112
                          REDEFINES            LC06-GE49.               CI0112
            12            LC06-GENAL2 PICTURE  X(30)                    CI0112
                          OCCURS       002     TIMES.                   CI0112
            12            LC06-GESAD2 PICTURE  X(30)                    CI0112
                          OCCURS       003     TIMES.                   CI0112
            11            LC06-GE44                                     CI0112
                          REDEFINES            LC06-GE49.               CI0112
            12            LC06-CTID01.                                  CI0112
            13            LC06-CTIDA6 PICTURE  9(3).                    CI0112
            13            LC06-NTIDP2 PICTURE  X(13).                   CI0112
            13            LC06-CTIDN2 PICTURE  9(11).                   CI0112
            12            LC06-GECKD2 PICTURE  9.                       CI0112
            12            LC06-PACCT  PICTURE  S999V99                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC06-PLOAN  PICTURE  S999V99                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC06-PADPT  PICTURE  S999V99                  CI0112
                          COMPUTATIONAL-3.                              CI0112
            12            LC06-IPCTL  PICTURE  X.                       CI0112
            12            LC06-IPCTP  PICTURE  X.                       CI0112
            12            LC06-CEUNT  PICTURE  S9(5)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            LC06-GE31                                     CI0112
                          REDEFINES            LC06-GE98.               CI0112
            11            LC06-GCUSPZ PICTURE  X(12).                   CI0112

      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0112
          05              DE00-SUITE.                                   CI0112
            15       FILLER         PICTURE  X(00653).                  CI0112
       01                 DE10  REDEFINES      DE00.                    CI0112
            10            DE10-DU11.                                    CI0112
            11            DE10-XFONC  PICTURE  X(4).                    CI0112
            11            DE10-MPSBN  PICTURE  X(8).                    CI0112
            11            DE10-XDBDNM PICTURE  X(08).                   CI0112
            11            DE10-XSEGNM PICTURE  X(08).                   CI0112
            11            DE10-XRC    PICTURE  X(02).                   CI0112
            11            DE10-MSEG   PICTURE  X(08).                   CI0112
            11            DE10-XCOKEY PICTURE  X(70).                   CI0112
            11            DE10-CUIBR  PICTURE  X(01).                   CI0112
            11            DE10-CUIBA  PICTURE  X(01).                   CI0112
            11            DE10-IPBIK  PICTURE  X(1).                    CI0112
            10            DE10-DU03.                                    CI0112
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            DE10-CMSSF  PICTURE  XX.                      CI0112
            11            DE10-DU09.                                    CI0112
            12            DE10-CMESA  PICTURE  S9(9)                    CI0112
                          BINARY.                                       CI0112
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0112
                          BINARY.                                       CI0112
            12            DE10-CMESB  PICTURE  S9(9)                    CI0112
                          BINARY.                                       CI0112
            12            DE10-CMSST  PICTURE  S9(9)                    CI0112
                          BINARY.                                       CI0112
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0112
                          BINARY.                                       CI0112
            12            DE10-QELLAA PICTURE  S9(9)                    CI0112
                          BINARY.                                       CI0112
            12            DE10-TMESS4 PICTURE  X(512).                  CI0112
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0112
          05              MS00-SUITE.                                   CI0112
            15       FILLER         PICTURE  X(00542).                  CI0112
       01                 MS03  REDEFINES      MS00.                    CI0112
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            10            MS03-CMSSF  PICTURE  XX.                      CI0112
            10            MS03-DU09.                                    CI0112
            11            MS03-CMESA  PICTURE  S9(9)                    CI0112
                          BINARY.                                       CI0112
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0112
                          BINARY.                                       CI0112
            11            MS03-CMESB  PICTURE  S9(9)                    CI0112
                          BINARY.                                       CI0112
            11            MS03-CMSST  PICTURE  S9(9)                    CI0112
                          BINARY.                                       CI0112
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0112
                          BINARY.                                       CI0112
            11            MS03-QELLAA PICTURE  S9(9)                    CI0112
                          BINARY.                                       CI0112
            11            MS03-TMESS4 PICTURE  X(512).                  CI0112
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0112
            10            MX11-QMSGS  PICTURE  9(03).                   CI0112
            10            MX11-PJ09                                     CI0112
                          OCCURS       025     TIMES.                   CI0112
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0112
                          COMPUTATIONAL-3.                              CI0112
            11            MX11-CMESB  PICTURE  S9(9)                    CI0112
                          BINARY.                                       CI0112
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PJ48
                                PJ18
                                LC01
                                LC03
                                LC04
                                LC06
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0112
      *               *                                   *             CI0112
      *               *INITIALISATIONS                    *             CI0112
      *               *                                   *             CI0112
      *               *************************************.            CI0112
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
      *N02CA.    NOTE *INITIALIZE WORKING STORAGE         *.
       F02CA.                                                           lv10
           MOVE        ZERO TO 7-TOTL-AAMWTB
           TA5B-IK
           W-WS00-DCACG9
           W-WS00-NAASQ.
       F02CA-FN. EXIT.
      *N02XA.    NOTE *GET ADDRESSABILITY                 *.
       F02XA.                                                           lv10
      *SET ADDRESS FOR ACAP                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-ACAP-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0112
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0112
      *               *                                   *             CI0112
      *               *FIN DE TRAITEMENT                  *             CI0112
      *               *                                   *             CI0112
      *               *************************************.            CI0112
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0112
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATE THE INPUT PARMS           *
      *               *                                   *
      *               *************************************.
       F30.           EXIT.                                             lv05
      *N30CA.    NOTE *IF SOURCE IS KNOWN                 *.
       F30CA.    IF    PJ48-MAPPN = 'UD        '                        lv10
                 NEXT SENTENCE ELSE GO TO     F30CA-FN.
      *
      *********************************
      ** IT IS A GIVEN THAT IF THIS   *
      ** PROGRAM RECOGNIZES THE MAPPN *
      ** ALL EDITS AND "PROPER"       *
      ** FORMATTING HAVE BEEN DONE    *
      ** PRIOR TO CALLING THIS        *
      ** PROGRAM SO, NO EDITING FOR   *
      ** NUMERICS WILL BE DONE ON     *
      ** ANY LC** FIELDS.             *
      **                              *
      ** FOR UD - CI0113 WILL DO      *
      ** THE "PROPER" FORMATTING.     *
      **                              *
      *********************************
       F30CA-900. GO TO F30CG-FN.
       F30CA-FN. EXIT.
      *N30CG.    NOTE *ELSE.. ABEND                       *.
       F30CG.                                                           lv10
      *---> Send UNKNOWN SOURCE Message                                 ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012734 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CG-FN. EXIT.
      *N30DA.    NOTE *IF CUSTODIAL CODE IS KNOWN         *.
       F30DA.    IF    PJ48-ICUST = 'Y'                                 lv10
                 OR    PJ48-ICUST = 'N'
                 NEXT SENTENCE ELSE GO TO     F30DA-FN.
       F30DA-900. GO TO F30DG-FN.
       F30DA-FN. EXIT.
      *N30DG.    NOTE *ELSE.. ITS NOT CUSTODIAL           *.
       F30DG.                                                           lv10
           MOVE        'N' TO PJ48-ICUST.
       F30DG-FN. EXIT.
      *N30EA.    NOTE *IF MULTIPLE                        *.
       F30EA.    IF    PJ48-IMUDT = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F30EA-FN.
      *N30EI.    NOTE *IF FUND... CANNOT HAVE MULTIPLE    *.
       F30EI.    IF    GC01-CTIDA = 002                                 lv15
                 NEXT SENTENCE ELSE GO TO     F30EI-FN.
           MOVE        'N' TO PJ48-IMUDT.
       F30EI-FN. EXIT.
       F30EA-900. GO TO F30EM-FN.
       F30EA-FN. EXIT.
      *N30EM.    NOTE *ELSE.. FORCE TO NO                 *.
       F30EM.                                                           lv10
           MOVE        'N' TO PJ48-IMUDT.
       F30EM-FN. EXIT.
       F30-FN.   EXIT.
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
      *               *CREATE THE DISBURSEMENT            *
      *               *                                   *
      *               *************************************.
       F40.      IF    PJ48-IMUDT = 'N'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F40-FN.
      *
      *********************************
      ** THIS LOGIC WILL CREATE THE   *
      ** DISBURSEMENT BUT NOT THE     *
      ** DESTINATION.  THAT LOGIC IS  *
      ** IN F60. SOME DISBURSEMENTS   *
      ** CAN HAVE MANY DESTINATIONS,  *
      ** SO THAT LOGIC IS ISOLATED TO *
      ** FUNCTION 60 AND CONTROLLED   *
      ** BY FIELD PJ48-IMUDT          *
      *********************************
      *N40CA.    NOTE *FORMAT THE GC01                    *.
       F40CA.                                                           lv10
           MOVE        LC01 TO GC01.
      *N40DA.    NOTE *INSERT GC01                        *.
       F40DA.                                                           lv15
           PERFORM     F94I1 THRU F94I1-FN.
       F40DA-FN. EXIT.
      *N40EA.    NOTE *IF GC01 ALREADY EXISTS             *.
       F40EA.    IF    XW05-XRC = 'II'                                  lv15
                 NEXT SENTENCE ELSE GO TO     F40EA-FN.
      *N40EF.    NOTE *READ THE ORIGINAL GC01             *.
       F40EF.                                                           lv20
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           PERFORM     F94G1 THRU F94G1-FN.
       F40EF-FN. EXIT.
      *N40EM.    NOTE *IF ORIGINAL SEGMENT FOUND          *.
       F40EM.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F40EM-FN.
      *
      *ENSURE WE HAVE THE CORRECT
      *CUSTODIAL CODE
      *
           MOVE        LC01-ICUST TO GC01-ICUST.
      *N40FA.    NOTE *IF TRAN DATE MATCHES ORIGINAL      *.
       F40FA.    IF    GC01-DCAG9L = LC01-DCAG9L                        lv25
                 AND   GC01-NAASQL < +999
                 NEXT SENTENCE ELSE GO TO     F40FA-FN.
           ADD         +1 TO GC01-NAASQL.
       F40FA-900. GO TO F40FG-FN.
       F40FA-FN. EXIT.
      *N40FG.    NOTE *ELSE... CHANGE DATE; RESET SEQ     *.
       F40FG.         EXIT.                                             lv25
      *N40FJ.    NOTE *IF GC01 IS AT THE LIMIT            *.
       F40FJ.    IF    GC01-NAASQL = +999                               lv30
                 NEXT SENTENCE ELSE GO TO     F40FJ-FN.
       F40FJ-900. GO TO F40FM-FN.
       F40FJ-FN. EXIT.
      *N40FM.    NOTE *ELSE... FIELD IS OK                *.
       F40FM.                                                           lv30
           MOVE        +1 TO GC01-NAASQL
           MOVE        LC01-DCAG9L TO GC01-DCAG9L.
       F40FM-FN. EXIT.
       F40FG-FN. EXIT.
      *N40FT.    NOTE *SET UPDATE SEQ NUMBER BY 1         *.
       F40FT.                                                           lv25
                 IF    GC01-NSEQ4B > ZERO                               DOT
                 AND   GC01-NSEQ4B < +99999999
      *ADD ONE IF VALID SEQENCE NUMBER
           ADD         +1 TO GC01-NSEQ4B
                 ELSE
           MOVE        +1 TO GC01-NSEQ4B.
       F40FT-FN. EXIT.
      *N40GA.    NOTE *REPLACE THE GC01                   *.
       F40GA.                                                           lv25
           PERFORM     F94R1 THRU F94R1-FN.
       F40GA-FN. EXIT.
      *N40GG.    NOTE *IF REPLACE FAILS; ERROR & EXIT     *.
       F40GG.    IF    IK = '1'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F40GG-FN.
      *---> Send BAD GC01 REPL Message                                  ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013313 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40GG-900. GO TO F40GM-FN.
       F40GG-FN. EXIT.
      *N40GM.    NOTE *ELSE... REPLACE WAS SUCCESSFUL     *.
       F40GM.                                                           lv25
           MOVE        GC01 TO LC01.
       F40GM-FN. EXIT.
       F40EM-900. GO TO F40GT-FN.
       F40EM-FN. EXIT.
      *N40GT.    NOTE *ELSE... NO ORIGINAL; ERROR         *.
       F40GT.                                                           lv20
      *---> Send BAD GC01 READ Message                                  ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013312 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40GT-FN. EXIT.
       F40EA-FN. EXIT.
       F40CA-FN. EXIT.
      *N40IA.    NOTE *FORMAT THE GC03                    *.
       F40IA.                                                           lv10
           MOVE        LC01-GC01K TO S-GCU01-GC01K
           MOVE        LC03 TO GC03
           MOVE        LC01-DCAG9L TO GC03-DCACG9
           MOVE        LC01-NAASQL TO GC03-NAASQ.
      *N40ID.    NOTE *INSERT GC03                        *.
       F40ID.                       GO TO     F40ID-B.                  lv15
       F40ID-A.
                 IF    IK = '0'
                                    GO TO     F40ID-FN.
       F40ID-B.
           PERFORM     F94I3 THRU F94I3-FN.
      *N40IG.    NOTE *IF ALREADY EXISTS; SET UP SEQ      *.
       F40IG.    IF    XW05-XRC = 'II'                                  lv20
                 NEXT SENTENCE ELSE GO TO     F40IG-FN.
           ADD         +1 TO GC03-NAASQ.
       F40IG-900. GO TO F40IJ-FN.
       F40IG-FN. EXIT.
      *N40IJ.    NOTE *STORE GC03 BACK TO LINKAGE         *.
       F40IJ.                                                           lv20
           MOVE        GC03 TO LC03.
       F40IJ-FN. EXIT.
       F40ID-900. GO TO F40ID-A.
       F40ID-FN. EXIT.
      *N40IP.    NOTE *IF GC01 DOESN'T MATCH NEW GC03     *.
       F40IP.    IF    GC03-NAASQ > LC01-NAASQL                         lv15
                 NEXT SENTENCE ELSE GO TO     F40IP-FN.
      *N40IR.    NOTE *READ THE ORIGINAL GC01             *.
       F40IR.                                                           lv20
           MOVE        LC01-GC01K TO S-GCU01-GC01K
           PERFORM     F94G1 THRU F94G1-FN.
       F40IR-FN. EXIT.
      *N40IT.    NOTE *IF ORIGINAL SEGMENT FOUND          *.
       F40IT.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F40IT-FN.
           MOVE        GC03-NAASQ TO GC01-NAASQL.
      *N40IV.    NOTE *REPLACE THE GC01                   *.
       F40IV.                                                           lv25
           PERFORM     F94R1 THRU F94R1-FN.
       F40IV-FN. EXIT.
      *N40JA.    NOTE *IF REPLACE WAS SUCCESSFUL          *.
       F40JA.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F40JA-FN.
           MOVE        GC01 TO LC01.
       F40JA-900. GO TO F40JG-FN.
       F40JA-FN. EXIT.
      *N40JG.    NOTE *ELSE... GC01 REPL FAILED; ERROR    *.
       F40JG.                                                           lv25
      *---> Send BAD GC01 REPL Message                                  ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013313 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40JG-FN. EXIT.
       F40IT-900. GO TO F40JT-FN.
       F40IT-FN. EXIT.
      *N40JT.    NOTE *ELSE... GC01 GHU FAILED;ERROR      *.
       F40JT.                                                           lv20
      *---> Send BAD GC01 READ Message                                  ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013312 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40JT-FN. EXIT.
       F40IP-FN. EXIT.
       F40IA-FN. EXIT.
      *N40JW.    NOTE *ON CHANGE, WRITE 'AFTER' IMAGE     *.
       F40JW.    IF    PJ48-CACTS = 'C'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F40JW-FN.
      *OF GC03 TO AUDIT LOG
           MOVE        70080 TO DH10-CAUFR
           MOVE        00003 TO DH10-CAUAC
      *FILL IN VA83-K11J (GC01K)
           MOVE        LC01-GC01K TO VA83-GC01K
      *FILL IN GC03K
           MOVE        LC03-GC03K TO VA83-GC03K
      *FILL IN GC03 DATA
           MOVE        GC03-GD00 TO VA83-GD00
           MOVE        GC03-GD01 TO VA83-GD01
           MOVE        GC03-ITELR2 TO VA83-ITELR
           MOVE        VA83 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F40JW-FN. EXIT.
      *N40KA.    NOTE *FORMAT THE GC04                    *.
       F40KA.    IF    LC04-CLCUS > 0                                   lv10
                 NEXT SENTENCE ELSE GO TO     F40KA-FN.
           MOVE        LC04 TO GC04.
      *N40KD.    NOTE *INSERT GC04                        *.
       F40KD.                                                           lv15
           MOVE        LC01-GC01K TO S-GCU01-GC01K
           MOVE        LC03-GC03K TO S-GCU03-GC03K
           PERFORM     F94I4 THRU F94I4-FN.
      *N40KM.    NOTE *IF INSERT FAILED; ERROR            *.
       F40KM.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F40KM-FN.
      *---> Send BAD GC04 CREATE Message                                ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013200 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40KM-FN. EXIT.
       F40KD-FN. EXIT.
       F40KA-900. GO TO F40KT-FN.
       F40KA-FN. EXIT.
      *N40KT.    NOTE *ELSE.... INIT GC04                 *.
       F40KT.                                                           lv10
           INITIALIZE  GC04.
       F40KT-FN. EXIT.
      *N40VA.    NOTE *CREATE GC29 SEGMENT                *.
       F40VA.                                                           lv10
           INITIALIZE  GC29
           MOVE        PJ18 TO GC29
           MOVE        LC01-GC01K TO S-GCU01-GC01K
           MOVE        LC03-GC03K TO S-GCU03-GC03K
           PERFORM     F94IA THRU F94IA-FN.
      *N40VE.    NOTE *IF UNSUCCESSFUL; ERROR             *.
       F40VE.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F40VE-FN.
      *---> Send BAD GC29 ISRT Message                                  ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015427 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40VE-FN. EXIT.
       F40VA-FN. EXIT.
       F40-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *IF COLLECTION NEEDED               *
      *               *                                   *
      *               *************************************.
       F50.      IF    LC06-CPITC = 02                                  lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *
      *********************************
      ** THIS ROUTINE WILL CREATE THE *
      ** COLLECTION IF NECESSARY.     *
      *********************************
      *N50CA.    NOTE *IF CERT TO CERT                    *.
       F50CA.    IF    LC01-CTIDA = 001                                 lv10
                 AND   LC06-CTIDA = 001
                 NEXT SENTENCE ELSE GO TO     F50CA-FN.
      *N50CD.    NOTE *IF MARKET STRATEGY; EXCHANGE       *.
       F50CD.    IF    (LC01-PRCOD = 00181                              lv15
                 OR    LC01-PRCOD = 00961)
                 AND   LC01-CTID = LC06-CTID
                 NEXT SENTENCE ELSE GO TO     F50CD-FN.
      *--> DO NOT CREATE COLLECTION!!!
      *--> SEE KD34 FOR RELATED CODE!!
      *
      *COULD NOT TEST THIS... TEST ONCE
      *CERTS PHASE IS DONE; NO MKT
      *STRATEGY ACCOUNTS EXISTED IN
      *TEST OR MODEL
      *
               GO TO     F50-FN.
       F50CD-FN. EXIT.
       F50CA-FN. EXIT.
      *N50IA.    NOTE *CREATE GC01 FOR THE COLLECTION     *.
       F50IA.                                                           lv10
           MOVE        LC06-CTID TO GC01-CTID
           MOVE        PJ48-ICUST TO GC01-ICUST
           MOVE        LC01-DCAG9L TO GC01-DCAG9L
           MOVE        +1 TO GC01-NAASQL
           MOVE        1 TO GC01-NSEQ4B
           MOVE        LC06-PRCOD1 TO GC01-PRCOD
           MOVE        LC06-CPRSC1 TO GC01-PRSCD.
      *N50IF.    NOTE *IF DESTINATION SUBPRODUCT BAD      *.
       F50IF.    IF    GC01-CTIDA = 002                                 lv15
                 AND   GC01-PRSCD = SPACES
                 NEXT SENTENCE ELSE GO TO     F50IF-FN.
           MOVE        '000000001' TO GC01-PRSCD.
       F50IF-FN. EXIT.
      *N50IM.    NOTE *INSERT GC01                        *.
       F50IM.                                                           lv15
           PERFORM     F94I1 THRU F94I1-FN.
       F50IM-FN. EXIT.
      *N50JA.    NOTE *UPDATE DATE AND SEQ NUMBER         *.
       F50JA.    IF    XW05-XRC = 'II'                                  lv15
                 NEXT SENTENCE ELSE GO TO     F50JA-FN.
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           PERFORM     F94G1 THRU F94G1-FN.
      *N50JG.    NOTE *IF FOUND                           *.
       F50JG.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F50JG-FN.
           MOVE        PJ48-ICUST TO GC01-ICUST
           MOVE        LC06-PRCOD1 TO GC01-PRCOD
           MOVE        LC06-CPRSC1 TO GC01-PRSCD.
      *N50JJ.    NOTE *IF SUB-PRODUCT NOT ESTABLISHED     *.
       F50JJ.    IF    GC01-CTIDA = 002                                 lv25
                 AND   GC01-PRSCD = SPACES
                 NEXT SENTENCE ELSE GO TO     F50JJ-FN.
           MOVE        '000000001' TO GC01-PRSCD.
       F50JJ-FN. EXIT.
      *N50JP.    NOTE *IF THE DATE MATCHES TODAY          *.
       F50JP.    IF    LC03-DCACG9 = GC01-DCAG9L                        lv25
                 AND   GC01-NAASQL < +999
                 NEXT SENTENCE ELSE GO TO     F50JP-FN.
           ADD         +1 TO GC01-NAASQL.
       F50JP-900. GO TO F50JR-FN.
       F50JP-FN. EXIT.
      *N50JR.    NOTE *ELSE... CHANGE DATE; RESET SEQ     *.
       F50JR.         EXIT.                                             lv25
      *N50JT.    NOTE *IF GC01 IS AT THE LIMIT            *.
       F50JT.    IF    GC01-NAASQL = +999                               lv30
                 NEXT SENTENCE ELSE GO TO     F50JT-FN.
       F50JT-900. GO TO F50JW-FN.
       F50JT-FN. EXIT.
      *N50JW.    NOTE *ELSE... FIELD IS OK                *.
       F50JW.                                                           lv30
           MOVE        +1 TO GC01-NAASQL
           MOVE        LC01-DCAG9L TO GC01-DCAG9L.
       F50JW-FN. EXIT.
       F50JR-FN. EXIT.
      *N50KA.    NOTE *REPLACE GC01                       *.
       F50KA.                                                           lv25
           PERFORM     F94R1 THRU F94R1-FN.
       F50KA-FN. EXIT.
      *N50KD.    NOTE *IF REPLACE FAILS; ERROR & EXIT     *.
       F50KD.    IF    IK = '1'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F50KD-FN.
      *---> Send BAD GC01 REPL Message                                  ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013217 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50KD-FN. EXIT.
       F50JG-900. GO TO F50KG-FN.
       F50JG-FN. EXIT.
      *N50KG.    NOTE *ELSE... NO ORIGINAL; ERROR         *.
       F50KG.                                                           lv20
      *---> Send BAD GC01 READ Message                                  ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013213 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50KG-FN. EXIT.
       F50JA-FN. EXIT.
       F50IA-FN. EXIT.
      *N50LA.    NOTE *LOAD GC03 FOR THE COLLECTION       *.
       F50LA.                                                           lv10
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           INITIALIZE  GC03-GD00
           GC03-GD02
           MOVE        +172 TO GC03-GELL.
      *N50NA.    NOTE *LOAD GC03-GD00                     *.
       F50NA.                                                           lv15
           MOVE        GC01-DCAG9L TO GC03-DCACG9
           MOVE        GC01-NAASQL TO GC03-NAASQ
           MOVE        002 TO GC03-CAATY
           MOVE        LC03-CACTO TO GC03-CACTO
           MOVE        ZEROES TO GC03-CATRN
           MOVE        01 TO GC03-CASTC
           MOVE        LC03-GEAUN TO GC03-GEAUN
           MOVE        LC03-GEOPD2 TO GC03-GEOPD2
           MOVE        ZERO TO GC03-NBTCH
           MOVE        LC03-NSUNT TO GC03-NSUNT
           MOVE        'Y' TO GC03-ITRAN
           COMPUTE     GC03-DLAUP1 = 99999999 -
           LC03-DCACG9
           MOVE        'N' TO GC03-IDELT
           MOVE        SPACES TO GC03-GEOPDM.
      *N50NC.    NOTE *READ TABLE TA5B TO GET CVSYS       *.
       F50NC.                                                           lv20
           MOVE        GC01-CTIDA TO TA5B-CTIDA
           MOVE        GC01-PRCOD TO TA5B-PRCOD
           MOVE        SPACES TO TA5B-PRSCD
           MOVE        '0' TO TA5B-IK
           PERFORM     F92TA THRU F92TA-FN.
      *N50ND.    NOTE *TA5B RECORD FOUND: MOVE CVSYS      *.
       F50ND.    IF    TA5B-IK = '0'                                    lv25
                 NEXT SENTENCE ELSE GO TO     F50ND-FN.
           MOVE        TA5B-CVSYS TO GC03-CVSYS.
       F50ND-900. GO TO F50NG-FN.
       F50ND-FN. EXIT.
      *N50NG.    NOTE *ELSE.. DEFAULT TO ZERO             *.
       F50NG.                                                           lv25
           MOVE        ZEROES TO GC03-CVSYS.
       F50NG-FN. EXIT.
       F50NC-FN. EXIT.
      *N50OA.    NOTE *IF FUND TO FUND TRANSFER           *.
       F50OA.    IF    LC01-CTIDA = 002                                 lv20
                 AND   LC06-CTIDA = 002
                 NEXT SENTENCE ELSE GO TO     F50OA-FN.
           MOVE        'Y' TO GC03-IPULL
           MOVE        LC03-DEFFT TO GC03-DEFFT.
       F50OA-900. GO TO F50OG-FN.
       F50OA-FN. EXIT.
      *N50OG.    NOTE *ELSE.. WAIT FOR BACKFEED           *.
       F50OG.                                                           lv20
           MOVE        'N' TO GC03-IPULL
      *
      *DEFAULT SAME EFFECTIVE DATE
           MOVE        LC03-DEFFT TO GC03-DEFFT.
      *N50OI.    NOTE *IF DEST NOT A STOCK MARKET CERT    *.
       F50OI.    IF    LC01-CTIDA = 001                                 lv25
                 AND   LC06-CTIDA = 001
                 AND   (LC06-PRCOD1 = 00180
                 OR    LC06-PRCOD1 = 00960)
                 NEXT SENTENCE ELSE GO TO     F50OI-FN.
      *  - USE NEXT EFFECTIVE DATE
       F50OI-900. GO TO F50OL-FN.
       F50OI-FN. EXIT.
      *N50OL.    NOTE *ELSE.. CALC NEXT EFFECTIVE DATE    *.
       F50OL.                                                           lv25
           MOVE        LC03-DEFFT TO 7-XX01-PCKDAT                      $AACTG
           COMPUTE     7-XX01-PUDAT =                                   $AACTG
           (7-XX01-PCKDAT * 10)                                         $AACTG
           MOVE        7-XX01-UNSDAT TO 7-XX01-ICURR                    $AACTG
           CALL        7-XX01-DATMOD USING                              $AACTG
           7-XX01-IDTFLD                                                $AACTG
           7-XX01-RDTFLD                                                $AACTG
           MOVE        7-XX01-RCDATE TO 7-XX01-CHKDAT.                  $AACTG
                 IF    7-XX01-CHKPDT = +177607040                       DOT
           MOVE        ZEROES TO GC03-DEFFT                             $AACTG
                 ELSE                                                   $AACTG
           MOVE        7-XX01-RNDATE TO 7-XX01-CHKDAT                   $AACTG
           COMPUTE     7-XX01-NEXTDT =                                  $AACTG
           (7-XX01-CHKPDT / 10)                                         $AACTG
           MOVE        7-XX01-NEXTDT TO GC03-DEFFT.                     $AACTG
       F50OL-FN. EXIT.
       F50OG-FN. EXIT.
       F50NA-FN. EXIT.
      *N50PA.    NOTE *LOAD GC03-GD02                     *.
       F50PA.                                                           lv15
           MOVE        15 TO GC03-CSYST
           MOVE        PJ48-ACASH TO GC03-ACASH
           MOVE        LC03-DCACG9 TO GC03-DTRAC
           MOVE        'IDS' TO GC03-CCOLL
           MOVE        'N' TO GC03-ISLOR
           MOVE        LC06-CPMTCB TO GC03-CPMTCA
           MOVE        'GEN' TO GC03-CSERV
           MOVE        LC03-IPLIN TO GC03-IPLIN1
           MOVE        LC03-GETIM TO GC03-GETIM3.
                 IF    LC06-CTIDA = 021 OR 133                          DOT
      *BROK OR BETA BROK DESTINATION
           MOVE        '1' TO GC03-CCSMQ.
       F50PA-FN. EXIT.
      *N50QA.    NOTE *WRITE UNTIL SUCCESSFUL             *.
       F50QA.                       GO TO     F50QA-B.                  lv15
       F50QA-A.
                 IF    IK = '0'
                                    GO TO     F50QA-FN.
       F50QA-B.
           PERFORM     F94I3 THRU F94I3-FN.
      *N50QK.    NOTE *IF DUPLICATE INSERT; UPDATE SEQ    *.
       F50QK.    IF    IK = '1'                                         lv20
                 AND   XW05-XRC = 'II'
                 NEXT SENTENCE ELSE GO TO     F50QK-FN.
           ADD         1 TO GC03-NAASQ.
       F50QK-FN. EXIT.
       F50QA-900. GO TO F50QA-A.
       F50QA-FN. EXIT.
      *N50QM.    NOTE *STORE COLLECTION KEYS              *.
       F50QM.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50QM-FN.
           MOVE        GC03-DCACG9 TO W-WS00-DCACG9
           MOVE        GC03-NAASQ TO W-WS00-NAASQ.
       F50QM-FN. EXIT.
       F50LA-FN. EXIT.
      *N50TA.    NOTE *CREATE GC12 FOR THE COLLECTION     *.
       F50TA.    IF    LC06-CIRAP > SPACES                              lv10
                 NEXT SENTENCE ELSE GO TO     F50TA-FN.
           INITIALIZE  GC12
           MOVE        LC06-CIRAP TO GC12-CIRAP
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           MOVE        GC03-GC03K TO S-GCU03-GC03K
           PERFORM     F94I8 THRU F94I8-FN.
      *N50TD.    NOTE *IF UNSUCCESSFUL; ERROR             *.
       F50TD.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50TD-FN.
      *---> Send BAD GC12 ISRT Message                                  ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013202 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50TD-FN. EXIT.
       F50TA-FN. EXIT.
      *N50UA.    NOTE *CREATE GC21 SEGMENT                *.
       F50UA.                                                           lv10
           INITIALIZE  GC21
           MOVE        LC01-CTID TO GC21-CTID
           MOVE        LC03-DCACG9 TO GC21-DCACG9
           MOVE        LC03-NAASQ TO GC21-NAASQ
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           MOVE        GC03-GC03K TO S-GCU03-GC03K
           PERFORM     F94I9 THRU F94I9-FN.
      *N50UD.    NOTE *IF UNSUCCESSFUL; ERROR             *.
       F50UD.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50UD-FN.
      *---> Send BAD GC21 ISRT Message                                  ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013203 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50UD-FN. EXIT.
       F50UA-FN. EXIT.
      *N50VA.    NOTE *CREATE GC29 SEGMENT                *.
       F50VA.                                                           lv10
           INITIALIZE  GC29
           MOVE        PJ18 TO GC29
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           MOVE        GC03-GC03K TO S-GCU03-GC03K
           PERFORM     F94IA THRU F94IA-FN.
      *N50VE.    NOTE *IF UNSUCCESSFUL; ERROR             *.
       F50VE.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50VE-FN.
      *---> Send BAD GC29 ISRT Message                                  ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015427 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50VE-FN. EXIT.
       F50VA-FN. EXIT.
       F50-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *CREATE THE DESTINATION             *
      *               *                                   *
      *               *************************************.
       F60.                                                             lv05
      *
      *********************************
      ** THIS ROUTINE WILL CREATE THE *
      ** DISBURSEMENTS DESTINAION     *
      ** SEGMENT.  IT IS DONE LAST    *
      ** BECAUSE IF THE DESTINATION   *
      ** IS ANOTHER ACCOUNT THE GC06  *
      ** CANNOT BE CREATED UNTIL THE  *
      ** COLLECTIONS GC03 IS CREATED. *
      *********************************
      *N60CA.    NOTE *FORMAT THE GC06                    *.
       F60CA.                                                           lv10
           MOVE        LC01-GC01K TO S-GCU01-GC01K
           MOVE        LC03-GC03K TO S-GCU03-GC03K.
      *N60CD.    NOTE *IF TRANSFER; LOAD TRANSFER KEY     *.
       F60CD.    IF    LC06-CPITC = 02                                  lv15
                 NEXT SENTENCE ELSE GO TO     F60CD-FN.
           MOVE        W-WS00-DCACG9 TO LC06-DCACG9
           MOVE        W-WS00-NAASQ TO LC06-NAASQ.
       F60CD-FN. EXIT.
      *N60CZ.    NOTE *STORE LINKAGE TO WORKING           *.
       F60CZ.                                                           lv15
           MOVE        LC06 TO GC06.
       F60CZ-FN. EXIT.
       F60CA-FN. EXIT.
      *N60DA.    NOTE *INSERT GC06                        *.
       F60DA.                       GO TO     F60DA-B.                  lv10
       F60DA-A.
                 IF    IK = '0'
                                    GO TO     F60DA-FN.
       F60DA-B.
           PERFORM     F94I6 THRU F94I6-FN.
      *N60EA.    NOTE *IF INSERT FAILED; UP SEQ NUMBER    *.
       F60EA.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F60EA-FN.
                 IF    LC06-NPISQ = +999                                DOT
      *IF LAST SEG ALREADY EXISTS
           SUBTRACT    +1 FROM GC06-NPISQ
                 ELSE
           ADD         +1 TO GC06-NPISQ.
       F60EA-900. GO TO F60ED-FN.
       F60EA-FN. EXIT.
      *N60ED.    NOTE *ELSE; SUCCESSFUL; UPDATE LINKAGE   *.
       F60ED.                                                           lv15
           MOVE        GC06 TO LC06.
       F60ED-FN. EXIT.
       F60DA-900. GO TO F60DA-A.
       F60DA-FN. EXIT.
      *N60JW.    NOTE *ON CHANGE, WRITE 'AFTER' IMAGE     *.
       F60JW.    IF    PJ48-CACTS = 'C'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F60JW-FN.
      *OF GC06 TO AUDIT LOG
           MOVE        70081 TO DH10-CAUFR
           MOVE        00003 TO DH10-CAUAC
      *FILL IN VA83-K11J (GC01K)
           MOVE        LC01-GC01K TO VA86-GC01K
      *FILL IN GC03K
           MOVE        LC03-GC03K TO VA86-GC03K
      *FILL IN GC06 DATA
           MOVE        GC06-GE00 TO VA86-GE00
           MOVE        GC06-GE98 TO VA86-GE98
           MOVE        VA86 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F60JW-FN. EXIT.
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
      *               *MISCELLANEOUS ROUTINES             *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92EB.    NOTE *ERROR ON TABLE READ FOR TB5B       *.
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
      *               *DATABASE ACCESS/UPDATES            *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94G1.    NOTE *CALL GHU ON GC01                   *.            ADU026
       F94G1.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PA06 GC01                                                    ADU026
           S-GCU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G1-FN. EXIT.
      *N94IA.    NOTE *CALL ISRT ON GC29                  *.            ADU026
       F94IA.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC29' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 GC29                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           S-GC29-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94IA-FN. EXIT.
      *N94I1.    NOTE *CALL ISRT ON GC01                  *.            ADU026
       F94I1.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 GC01                                                    ADU026
           S-GC01-SSA                                                   ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94I1-FN. EXIT.
      *N94I3.    NOTE *CALL ISRT ON GC03                  *.            ADU026
       F94I3.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 GC03                                                    ADU026
           S-GCU01-SSA S-GC03-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94I3-FN. EXIT.
      *N94I4.    NOTE *CALL ISRT ON GC04                  *.            ADU026
       F94I4.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC04' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 GC04                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           S-GC04-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94I4-FN. EXIT.
      *N94I6.    NOTE *CALL ISRT ON GC06                  *.            ADU026
       F94I6.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 GC06                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           S-GC06-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94I6-FN. EXIT.
      *N94I8.    NOTE *CALL ISRT ON GC12                  *.            ADU026
       F94I8.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 GC12                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           S-GC12-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94I8-FN. EXIT.
      *N94I9.    NOTE *CALL ISRT ON GC21                  *.            ADU026
       F94I9.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC21' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 GC21                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           S-GC21-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94I9-FN. EXIT.
      *N94R1.    NOTE *CALL REPL ON GC01                  *.            ADU026
       F94R1.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PA06 GC01                                                    ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94R1-FN. EXIT.
       F94-FN.   EXIT.
      *N96AL.    NOTE *---> Audit Log Process             *.            ADU165
       F96AL.         EXIT.                                             lv10
      *N96AN.    NOTE *---> Format Audit Log Data         *.            ADU165
       F96AN.                                                           lv15
           SET AL00-NPNTR                                               ADU165
           TO ADDRESS OF DLIUIBII                                       ADU165
           MOVE        AL00-ADDR TO DH10-XUIBP                          ADU165
           MOVE        AL00-NSEQ2P TO DH10-NSEQ2P                       ADU165
           MOVE        'E' TO DH10-CAUL                                 ADU165
           MOVE        'CONTRAC1' TO DH10-MAUSB                         ADU165
           MOVE        LC01-CTID TO DH10-NAUSK                          ADU165
           MOVE        'CATS' TO DH10-CSYS                              ADU165
           MOVE        EIBTRNID TO DH10-CAPPL                           ADU165
           MOVE        'C' TO DH10-CAUSR.                               ADU165
                 IF    DH10-CAUFR = 70040                               DOT
           MOVE        LC03-GEOPD2 TO DH10-GEOPID
                 ELSE
           MOVE        ACF-USER-ID TO DH10-GEOPID.                      ADU165
                 IF    DH10-CAUFR = 70040                               DOT
           MOVE        LC03-GEAUN (2 : 4) TO DH10-CAUNIT
                 ELSE
           MOVE        ACF-USER-UNIT TO DH10-CAUNIT.                    ADU165
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
