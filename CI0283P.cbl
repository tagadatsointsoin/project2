       IDENTIFICATION DIVISION.                                         CI0283
       PROGRAM-ID.  CI0283P.                                            CI0283
      *AUTHOR.         ANNUITIES UD WRITE ACTIVITY.                     CI0283
      *DATE-COMPILED.   09/08/14.                                       CI0283
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
       ENVIRONMENT DIVISION.                                            CI0283
       CONFIGURATION SECTION.                                           CI0283
       SOURCE-COMPUTER. IBM-370.                                        CI0283
       OBJECT-COMPUTER. IBM-370.                                        CI0283
       DATA DIVISION.                                                   CI0283
       WORKING-STORAGE SECTION.                                         CI0283
       01  AA10-CF           PIC X VALUE SPACE.
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *ADMIN CODE NEEDED FOR AAOEID ACCESS
      *!WI
       01  CT01-CTIDA
                        PICTURE 9(3).                                   CI0283
      ******************************************************            AADA81
      ****      WORK AREAS FOR COMMON DATE UTILITY       ***            AADA81
      ******************************************************            AADA81
      **                                                                AADA81
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA81
      **                                                                AADA81
      **   SEGMENT DD30 - FUNCTION LAYOUT                               AADA81
      **                                                                AADA81
      *!WF DSP=DD DSL=DD SEL=30 FOR=I DES=2 LEV=1                       AADA81
       01                 DD30.                                         CI0283
            10            DD30-CDTFN  PICTURE  9(4)                     CI0283
                          VALUE                ZERO.                    CI0283
            10            DD30-CDTSF  PICTURE  9(4)                     CI0283
                          VALUE                ZERO.                    CI0283
            10            DD30-CDTSC  PICTURE  9(4)                     CI0283
                          VALUE                ZERO.                    CI0283
            10            DD30-FILLER PICTURE  X(40)                    CI0283
                          VALUE                SPACE.                   CI0283
       01                 DD33.                                         CI0283
            10            DD33-DTGRG.                                   CI0283
            11            DD33-DTGCY.                                   CI0283
            12            DD33-DTGCC  PICTURE  9(2)                     CI0283
                          VALUE                ZERO.                    CI0283
            12            DD33-DTGYY  PICTURE  9(2)                     CI0283
                          VALUE                ZERO.                    CI0283
            11            DD33-DTGMM  PICTURE  9(2)                     CI0283
                          VALUE                ZERO.                    CI0283
            11            DD33-DTGDD  PICTURE  9(2)                     CI0283
                          VALUE                ZERO.                    CI0283
            10            DD33-DTJULC.                                  CI0283
            11            DD33-DTJCY.                                   CI0283
            12            DD33-DTJCC  PICTURE  9(2)                     CI0283
                          VALUE                ZERO.                    CI0283
            12            DD33-DTJYY  PICTURE  9(2)                     CI0283
                          VALUE                ZERO.                    CI0283
            11            DD33-DTJDDC PICTURE  S9(3)                    CI0283
                          VALUE                ZERO.                    CI0283
            11            DD33-DTJDD                                    CI0283
                          REDEFINES            DD33-DTJDDC              CI0283
               PICTURE    9(3).                                         CI0283
            10            DD33-DTJUL                                    CI0283
                          REDEFINES            DD33-DTJULC              CI0283
               PICTURE    9(7).                                         CI0283
            10            DD33-DTDYR  PICTURE  9(3)                     CI0283
                          VALUE                ZERO.                    CI0283
            10            DD33-DTDMO  PICTURE  9(2)                     CI0283
                          VALUE                ZERO.                    CI0283
            10            DD33-FILLER PICTURE  X(18)                    CI0283
                          VALUE                SPACE.                   CI0283
      **                                                                AADA81
      **   SEGMENT DD33 - CONVERT DATE LAYOUT                           AADA81
      **                                                                AADA81
      *!WF DSP=DD DSL=DD SEL=33 FOR=I DES=2 LEV=1                       AADA81
      **                                                                AADA81
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
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0283
            10            XW05-XW06.                                    CI0283
            11            XW05-XDBPCB.                                  CI0283
            12            XW05-XDBDNM PICTURE  X(08)                    CI0283
                          VALUE                SPACE.                   CI0283
            12            XW05-XSEGLV PICTURE  X(02)                    CI0283
                          VALUE                SPACE.                   CI0283
            12            XW05-XRC    PICTURE  X(02)                    CI0283
                          VALUE                SPACE.                   CI0283
            12            XW05-XPROPT PICTURE  X(04)                    CI0283
                          VALUE                SPACE.                   CI0283
            12            XW05-FILLER PICTURE  S9(5)                    CI0283
                          VALUE                ZERO                     CI0283
                          BINARY.                                       CI0283
            12            XW05-XSEGNM PICTURE  X(08)                    CI0283
                          VALUE                SPACE.                   CI0283
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0283
                          VALUE                ZERO                     CI0283
                          BINARY.                                       CI0283
            12            XW05-XSEGNB PICTURE  9(05)                    CI0283
                          VALUE                ZERO                     CI0283
                          BINARY.                                       CI0283
            12            XW05-XCOKEY PICTURE  X(70)                    CI0283
                          VALUE                SPACE.                   CI0283
            10            XW05-XW07.                                    CI0283
            11            XW05-XIOPCB.                                  CI0283
            12            XW05-XTERMI PICTURE  X(08)                    CI0283
                          VALUE                SPACE.                   CI0283
            12            XW05-FILLER PICTURE  XX                       CI0283
                          VALUE                SPACE.                   CI0283
            12            XW05-XRC1   PICTURE  X(02)                    CI0283
                          VALUE                SPACE.                   CI0283
            12            XW05-FILLER PICTURE  X(12)                    CI0283
                          VALUE                SPACE.                   CI0283
            12            XW05-XMODNM PICTURE  X(8)                     CI0283
                          VALUE                SPACE.                   CI0283
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0283
                          VALUE                ZERO.                    CI0283
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0283
                          VALUE                ZERO.                    CI0283
            10            XW05-XGU    PICTURE  X(4)                     CI0283
                          VALUE                'GU  '.                  CI0283
            10            XW05-XGHU   PICTURE  X(4)                     CI0283
                          VALUE                'GHU '.                  CI0283
            10            XW05-XGN    PICTURE  X(4)                     CI0283
                          VALUE                'GN  '.                  CI0283
            10            XW05-XGHN   PICTURE  X(4)                     CI0283
                          VALUE                'GHN '.                  CI0283
            10            XW05-XGNP   PICTURE  X(4)                     CI0283
                          VALUE                'GNP '.                  CI0283
            10            XW05-XGHNP  PICTURE  X(4)                     CI0283
                          VALUE                'GHNP'.                  CI0283
            10            XW05-XREPL  PICTURE  XXXX                     CI0283
                          VALUE                'REPL'.                  CI0283
            10            XW05-XISRT  PICTURE  X(4)                     CI0283
                          VALUE                'ISRT'.                  CI0283
            10            XW05-XDLET  PICTURE  X(4)                     CI0283
                          VALUE                'DLET'.                  CI0283
            10            XW05-XOPEN  PICTURE  X(4)                     CI0283
                          VALUE                'OPEN'.                  CI0283
            10            XW05-XCLSE  PICTURE  X(4)                     CI0283
                          VALUE                'CLSE'.                  CI0283
            10            XW05-XCHKP  PICTURE  X(4)                     CI0283
                          VALUE                'CHKP'.                  CI0283
            10            XW05-XXRST  PICTURE  X(4)                     CI0283
                          VALUE                'XRST'.                  CI0283
            10            XW05-XTERM  PICTURE  X(4)                     CI0283
                          VALUE                'TERM'.                  CI0283
            10            XW05-XNFPAC PICTURE  X(13)                    CI0283
                          VALUE                SPACE.                   CI0283
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0283
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0283
       01                 GC01.                                         CI0283
            10            GC01-GC01K.                                   CI0283
            11            GC01-C299.                                    CI0283
            12            GC01-CTID.                                    CI0283
            13            GC01-CTIDA  PICTURE  9(3).                    CI0283
            13            GC01-CTIDN.                                   CI0283
            14            GC01-CTIDNP PICTURE  X(13).                   CI0283
            14            GC01-CTIDND PICTURE  9(11).                   CI0283
            10            GC01-DCAG9L PICTURE  9(8).                    CI0283
            10            GC01-NAASQL PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            GC01-ICUST  PICTURE  X.                       CI0283
            10            GC01-NSEQ4B PICTURE  9(8)                     CI0283
                          BINARY.                                       CI0283
            10            GC01-PRCOD  PICTURE  9(5).                    CI0283
            10            GC01-PRSCD  PICTURE  X(9).                    CI0283
            10            GC01-FILLER PICTURE  X(8).                    CI0283
       01                 GC03.                                         CI0283
            10            GC03-GELL   PICTURE  9(4)                     CI0283
                          BINARY.                                       CI0283
            10            GC03-GD00.                                    CI0283
            11            GC03-GC03K.                                   CI0283
            12            GC03-DCACG9 PICTURE  9(8).                    CI0283
            12            GC03-NAASQ  PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-CAATY  PICTURE  9(3).                    CI0283
            11            GC03-CVSYS  PICTURE  X(2).                    CI0283
            11            GC03-CACTO  PICTURE  9(3).                    CI0283
            11            GC03-CATRN.                                   CI0283
            12            GC03-CATRF  PICTURE  9(3).                    CI0283
            12            GC03-CATRS  PICTURE  9(3).                    CI0283
            11            GC03-CASTC  PICTURE  99.                      CI0283
            11            GC03-IPULL  PICTURE  X.                       CI0283
            11            GC03-GEAUN  PICTURE  9(5).                    CI0283
            11            GC03-GEOPD2 PICTURE  X(8).                    CI0283
            11            GC03-NBTCH  PICTURE  9(4).                    CI0283
            11            GC03-DEFFT  PICTURE  9(8).                    CI0283
            11            GC03-NSUNT  PICTURE  9(4).                    CI0283
            11            GC03-ITRAN  PICTURE  X.                       CI0283
            11            GC03-DLAUP1 PICTURE  9(8).                    CI0283
            11            GC03-ADRET  PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-TTRMS  PICTURE  X(12).                   CI0283
            11            GC03-IDELT  PICTURE  X.                       CI0283
            11            GC03-GEOPDM PICTURE  X(8).                    CI0283
            11            GC03-FILLER PICTURE  X(07).                   CI0283
            10            GC03-GD09.                                    CI0283
            11            GC03-FILLER PICTURE  X(70).                   CI0283
            10            GC03-GD01                                     CI0283
                          REDEFINES            GC03-GD09.               CI0283
            11            GC03-ADBRQ  PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-CTRTP  PICTURE  X(2).                    CI0283
            11            GC03-CPORT  PICTURE  X.                       CI0283
            11            GC03-CSCRNU PICTURE  X(4).                    CI0283
            11            GC03-DLAUP  PICTURE  9(8).                    CI0283
            11            GC03-CTWHAT PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-PWHLD  PICTURE  S999V9(5)                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-IWTHH  PICTURE  X.                       CI0283
            11            GC03-NDRFT  PICTURE  9(5).                    CI0283
            11            GC03-IDPAP  PICTURE  X.                       CI0283
            11            GC03-GETIM  PICTURE  S9(7)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-QNACT  PICTURE  9(3).                    CI0283
            11            GC03-AEDRQ  PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-IPLIN  PICTURE  X.                       CI0283
            11            GC03-CLIDNB PICTURE  9(8).                    CI0283
            11            GC03-CSLCT  PICTURE  X.                       CI0283
            11            GC03-ITELE  PICTURE  X.                       CI0283
            11            GC03-FILLER PICTURE  X(06).                   CI0283
            10            GC03-GD02                                     CI0283
                          REDEFINES            GC03-GD09.               CI0283
            11            GC03-CSYST  PICTURE  99.                      CI0283
            11            GC03-FILLER PICTURE  X.                       CI0283
            11            GC03-ACASH  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-DTRAC  PICTURE  9(8).                    CI0283
            11            GC03-CTRSO  PICTURE  9(02).                   CI0283
            11            GC03-NTRCE  PICTURE  9(06).                   CI0283
            11            GC03-GECKD1 PICTURE  9.                       CI0283
            11            GC03-CCOLL  PICTURE  X(3).                    CI0283
            11            GC03-CLTDP  PICTURE  X(3).                    CI0283
            11            GC03-PSLLD  PICTURE  S99V999                  CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-ISLOR  PICTURE  X.                       CI0283
            11            GC03-ITPAC  PICTURE  X.                       CI0283
            11            GC03-CPMTCA PICTURE  XXX.                     CI0283
            11            GC03-CSERV  PICTURE  X(3).                    CI0283
            11            GC03-ACOMO  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-IPLIN1 PICTURE  X.                       CI0283
            11            GC03-INQEX  PICTURE  X.                       CI0283
            11            GC03-CTKRAA PICTURE  X(12).                   CI0283
            11            GC03-CCSMQ  PICTURE  X.                       CI0283
            11            GC03-IVAEX1 PICTURE  X.                       CI0283
            11            GC03-IHPMT  PICTURE  X(1).                    CI0283
            11            GC03-GETIM3 PICTURE  S9(7)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            GC03-GD03                                     CI0283
                          REDEFINES            GC03-GD09.               CI0283
            11            GC03-CATRNC PICTURE  9(6).                    CI0283
            11            GC03-APRNT1 PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-QSHOWT PICTURE  S9(10)V999               CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-ACINVT PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-ACOMO7 PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-QSHOMW PICTURE  S9(10)V999               CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-ATAXT3 PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-CTSTR  PICTURE  9(2).                    CI0283
            11            GC03-ICIRA  PICTURE  X.                       CI0283
            11            GC03-GETIM2 PICTURE  S9(7)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-CPMTCX PICTURE  XX.                      CI0283
            11            GC03-FILLER PICTURE  X(16).                   CI0283
            10            GC03-GD99.                                    CI0283
            11            GC03-FILLER PICTURE  X(248).                  CI0283
            10            GC03-GD10                                     CI0283
                          REDEFINES            GC03-GD99.               CI0283
            11            GC03-MROTC  PICTURE  X(7).                    CI0283
            11            GC03-CEDSC  PICTURE  9(1).                    CI0283
            11            GC03-ILPOI  PICTURE  X(1).                    CI0283
            11            GC03-AWRCH  PICTURE  S9(3)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-CHCOC1 PICTURE  9(2).                    CI0283
            11            GC03-CHCOC2 PICTURE  9(2).                    CI0283
            11            GC03-CHCOC3 PICTURE  9(2).                    CI0283
            11            GC03-CHCOC4 PICTURE  9(2).                    CI0283
            11            GC03-CMCOC1 PICTURE  9(3).                    CI0283
            11            GC03-CMCOC2 PICTURE  9(3).                    CI0283
            11            GC03-CMCOC3 PICTURE  9(3).                    CI0283
            11            GC03-GD11.                                    CI0283
            12            GC03-FILLER PICTURE  X(219).                  CI0283
            11            GC03-GD12                                     CI0283
                          REDEFINES            GC03-GD11.               CI0283
            12            GC03-CELLO  PICTURE  9(1).                    CI0283
            12            GC03-CECLO  PICTURE  9(1).                    CI0283
            12            GC03-AEXML  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-CEPI   PICTURE  X(1).                    CI0283
            12            GC03-CEXTY  PICTURE  X.                       CI0283
            12            GC03-CROPC  PICTURE  9(1).                    CI0283
            12            GC03-CPUTY  PICTURE  9(1).                    CI0283
            12            GC03-IMCII  PICTURE  X(1).                    CI0283
            12            GC03-GEMISC                                   CI0283
                          OCCURS       010     TIMES.                   CI0283
            13            GC03-AMGLA  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            13            GC03-CMGLC  PICTURE  9(1).                    CI0283
            13            GC03-NMGLN  PICTURE  9(4).                    CI0283
            12            GC03-ACTRN  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-IWRBK  PICTURE  X.                       CI0283
            12            GC03-IFEDX  PICTURE  X.                       CI0283
            12            GC03-ICNTR  PICTURE  X.                       CI0283
            12            GC03-IOCKH  PICTURE  X.                       CI0283
            12            GC03-ICRCK  PICTURE  X.                       CI0283
            12            GC03-NHMPN  PICTURE  S9(10)                   CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-ITELR1 PICTURE  X.                       CI0283
            11            GC03-GD13                                     CI0283
                          REDEFINES            GC03-GD11.               CI0283
            12            GC03-DREDO  PICTURE  9(8).                    CI0283
            12            GC03-CATRNR PICTURE  9(6).                    CI0283
            12            GC03-CEVN   PICTURE  9(9).                    CI0283
            12            GC03-ISUSP  PICTURE  X(1).                    CI0283
            11            GC03-GD15                                     CI0283
                          REDEFINES            GC03-GD11.               CI0283
            12            GC03-CPUTZ  PICTURE  9(1).                    CI0283
            12            GC03-CETLB  PICTURE  9(3).                    CI0283
            12            GC03-QTRMC  PICTURE  9(3).                    CI0283
            12            GC03-DEFFTE PICTURE  9(8).                    CI0283
            12            GC03-DEFFTF PICTURE  9(8).                    CI0283
            12            GC03-DEFFTG PICTURE  9(8).                    CI0283
            12            GC03-XZ1A   PICTURE  X.                       CI0283
            12            GC03-XZ1B   PICTURE  X.                       CI0283
            12            GC03-XZ1C   PICTURE  X.                       CI0283
            12            GC03-XZ1D   PICTURE  X.                       CI0283
            12            GC03-XZ1E   PICTURE  X.                       CI0283
            12            GC03-XZ1F   PICTURE  X.                       CI0283
            12            GC03-XZ1G   PICTURE  X.                       CI0283
            12            GC03-XZ1H   PICTURE  X.                       CI0283
            12            GC03-XZ1I   PICTURE  X.                       CI0283
            12            GC03-DEFFTH PICTURE  9(8).                    CI0283
            11            GC03-GD19                                     CI0283
                          REDEFINES            GC03-GD11.               CI0283
            12            GC03-GD11.                                    CI0283
            13            GC03-FILLER PICTURE  X(219).                  CI0283
            10            GC03-GD20                                     CI0283
                          REDEFINES            GC03-GD99.               CI0283
            11            GC03-ADDACT PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-ISIGV  PICTURE  X.                       CI0283
            11            GC03-IALLF  PICTURE  X.                       CI0283
            11            GC03-QSHOWQ PICTURE  S9(9)V999                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-CCDSCW PICTURE  9(2).                    CI0283
            11            GC03-IDWRL  PICTURE  X.                       CI0283
            11            GC03-ITELR  PICTURE  X.                       CI0283
            11            GC03-IABIN  PICTURE  X.                       CI0283
            11            GC03-PACT1  PICTURE  S999V999                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-IBFAF  PICTURE  X.                       CI0283
            11            GC03-IFRSA  PICTURE  X.                       CI0283
            11            GC03-ICRCAN PICTURE  X.                       CI0283
            11            GC03-ACACTV PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-AGFND  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-QCSHOW PICTURE  S9(9)V999                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-QCSHIS PICTURE  S9(9)V999                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-NDTRC  PICTURE  9(8).                    CI0283
            11            GC03-CAERU  PICTURE  X(4).                    CI0283
            11            GC03-IFDGO  PICTURE  X.                       CI0283
            11            GC03-PSLLD2 PICTURE  S99V999                  CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-ISLOR2 PICTURE  X.                       CI0283
            11            GC03-QSFIO  PICTURE  S9(10)V999               CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-QSFID  PICTURE  S9(10)V999               CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-CGDIN  PICTURE  X.                       CI0283
            11            GC03-DGDIN  PICTURE  9(8).                    CI0283
            10            GC03-GD30                                     CI0283
                          REDEFINES            GC03-GD99.               CI0283
            11            GC03-ISKED  PICTURE  X.                       CI0283
            11            GC03-CENXC  PICTURE  9(2).                    CI0283
            11            GC03-GD31.                                    CI0283
            12            GC03-FILLER PICTURE  X(245).                  CI0283
            11            GC03-GD32                                     CI0283
                          REDEFINES            GC03-GD31.               CI0283
            12            GC03-IABIN1 PICTURE  X.                       CI0283
            12            GC03-CLDOD  PICTURE  9(8).                    CI0283
            12            GC03-NCLAM  PICTURE  9(5)                     CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-ISURR  PICTURE  X.                       CI0283
            12            GC03-GEHCD  PICTURE  9(3).                    CI0283
            12            GC03-CRATC  PICTURE  9(4).                    CI0283
            12            GC03-AMAXD  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-ASCHGA PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-APYOM  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-IWTHH1 PICTURE  X.                       CI0283
            12            GC03-CPAYCL PICTURE  X(2).                    CI0283
            12            GC03-CTSAO  PICTURE  X.                       CI0283
            12            GC03-NCONF  PICTURE  9(08).                   CI0283
            12            GC03-CLID   PICTURE  X(23).                   CI0283
            12            GC03-CARTY  PICTURE  99.                      CI0283
            12            GC03-NARRS  PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-CARTZ  PICTURE  99.                      CI0283
            12            GC03-NAPDS  PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-CPMTO  PICTURE  X.                       CI0283
            12            GC03-DNPMT  PICTURE  9(8).                    CI0283
            12            GC03-IPCTV  PICTURE  X.                       CI0283
            12            GC03-IMECH  PICTURE  X(01).                   CI0283
            12            GC03-IMVAO  PICTURE  X(1).                    CI0283
            12            GC03-AMVA1  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-CACTS  PICTURE  X.                       CI0283
            12            GC03-CTSPP  PICTURE  X(1).                    CI0283
            12            GC03-CACT4  PICTURE  X(2).                    CI0283
            12            GC03-IVAEX  PICTURE  X.                       CI0283
            12            GC03-DFPMT  PICTURE  9(8).                    CI0283
            12            GC03-IDEMD  PICTURE  X.                       CI0283
            12            GC03-IOFST  PICTURE  X.                       CI0283
            12            GC03-AMXLB  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-ACULB  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-DEIRNB PICTURE  9(8).                    CI0283
            12            GC03-DEFFE  PICTURE  9(8).                    CI0283
            12            GC03-DEFFR  PICTURE  9(8).                    CI0283
            12            GC03-ISPUP  PICTURE  X.                       CI0283
            12            GC03-CPNCG  PICTURE  X.                       CI0283
            12            GC03-IEXPU  PICTURE  X.                       CI0283
            12            GC03-IPPCF  PICTURE  X.                       CI0283
            12            GC03-NAAPT  PICTURE  9(2).                    CI0283
            12            GC03-PWHLDS PICTURE  S999V9(5)                CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-ISWHO  PICTURE  X(1).                    CI0283
            11            GC03-GD33                                     CI0283
                          REDEFINES            GC03-GD31.               CI0283
            12            GC03-CPAYC  PICTURE  X(2).                    CI0283
            12            GC03-ADBRQX PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-ADBRQV PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-APTXR  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-CTRTPE PICTURE  X(2).                    CI0283
            12            GC03-NCLAMI PICTURE  S9(9)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-CLIDO8 PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-CLIDN  PICTURE  X(20).                   CI0283
            12            GC03-DSET01 PICTURE  S9(8)                    CI0283
                          BINARY.                                       CI0283
            12            GC03-CTSET1 PICTURE  S9(6)                    CI0283
                          BINARY.                                       CI0283
            12            GC03-DSET02 PICTURE  S9(8)                    CI0283
                          BINARY.                                       CI0283
            12            GC03-CTSET2 PICTURE  S9(6)                    CI0283
                          BINARY.                                       CI0283
            11            GC03-GD34                                     CI0283
                          REDEFINES            GC03-GD31.               CI0283
            12            GC03-QNOFM  PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-CLTRM  PICTURE  99.                      CI0283
            12            GC03-AMXLN  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-ALADJ  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-ACHK   PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-APRMO  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-IMECH1 PICTURE  X(01).                   CI0283
            12            GC03-CACT41 PICTURE  X(2).                    CI0283
            12            GC03-ACDSCC PICTURE  S9(05)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-ACDSCD PICTURE  S9(05)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-GD39                                     CI0283
                          REDEFINES            GC03-GD31.               CI0283
            12            GC03-GD31.                                    CI0283
            13            GC03-FILLER PICTURE  X(245).                  CI0283
            10            GC03-GD40                                     CI0283
                          REDEFINES            GC03-GD99.               CI0283
            11            GC03-NTR    PICTURE  9(8).                    CI0283
            11            GC03-NPBNC  PICTURE  X(24).                   CI0283
            11            GC03-CRREV  PICTURE  X(3).                    CI0283
            11            GC03-CSUSL  PICTURE  S9.                      CI0283
            11            GC03-NMGLN1 PICTURE  9(4).                    CI0283
            11            GC03-DCAC92 PICTURE  9(8).                    CI0283
            11            GC03-NAASQ3 PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-GD49.                                    CI0283
            12            GC03-FILLER PICTURE  X(198).                  CI0283
            11            GC03-GD41                                     CI0283
                          REDEFINES            GC03-GD49.               CI0283
            12            GC03-CRREF  PICTURE  9(2).                    CI0283
            12            GC03-CORIR  PICTURE  X(02).                   CI0283
            12            GC03-CIPDB  PICTURE  X(03).                   CI0283
            12            GC03-CPAYH  PICTURE  X(02).                   CI0283
            12            GC03-NAMEX  PICTURE  9(15)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC03-DCHAE  PICTURE  9(4).                    CI0283
            12            GC03-DRQST  PICTURE  S9(8)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-GD42                                     CI0283
                          REDEFINES            GC03-GD49.               CI0283
            12            GC03-CPMTCB PICTURE  X(3).                    CI0283
            10            GC03-GD50                                     CI0283
                          REDEFINES            GC03-GD99.               CI0283
            11            GC03-ALOAD  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-PSLLD4 PICTURE  S99V999                  CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-CSUSL1 PICTURE  S9.                      CI0283
            11            GC03-CRREV1 PICTURE  X(3).                    CI0283
            11            GC03-ADDAC  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-DL13.                                    CI0283
            12            GC03-GEYR   PICTURE  9(4).                    CI0283
            12            GC03-GEMTH  PICTURE  99.                      CI0283
            12            GC03-NDAY   PICTURE  99.                      CI0283
            11            GC03-NSEQ3P PICTURE  S9(5)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-XZ6A   PICTURE  X(6).                    CI0283
            11            GC03-XZ7    PICTURE  X(7).                    CI0283
            11            GC03-XZ6B   PICTURE  X(6).                    CI0283
            11            GC03-XZ6    PICTURE  X(6).                    CI0283
            11            GC03-XZ6C   PICTURE  X(6).                    CI0283
            11            GC03-XZ20   PICTURE  X(20).                   CI0283
            11            GC03-CATRN1 PICTURE  9(6).                    CI0283
            11            GC03-ADDAC2 PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-ATAXT2 PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-ACOMOT PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-XZ5    PICTURE  X(5).                    CI0283
            11            GC03-IREVD  PICTURE  X(1).                    CI0283
            11            GC03-ISUSP1 PICTURE  X(1).                    CI0283
            11            GC03-XZ6D   PICTURE  X(6).                    CI0283
            11            GC03-XZ13   PICTURE  X(13).                   CI0283
            11            GC03-CWHTP2 PICTURE  X(3).                    CI0283
            11            GC03-CWHTP3 PICTURE  X(3).                    CI0283
            11            GC03-DTREN  PICTURE  9(8).                    CI0283
            11            GC03-NAASQ1 PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            GC03-GD51                                     CI0283
                          REDEFINES            GC03-GD99.               CI0283
            11            GC03-ADOMOT PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-ACGLT  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-ACGST  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-CTXMT  PICTURE  9(2).                    CI0283
            11            GC03-ALOAD3 PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-FILLER PICTURE  X(31).                   CI0283
            10            GC03-GD52                                     CI0283
                          REDEFINES            GC03-GD99.               CI0283
            11            GC03-DEFFT5 PICTURE  9(8).                    CI0283
            11            GC03-PSLLD5 PICTURE  S99V999                  CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-CSUSL2 PICTURE  S9.                      CI0283
            11            GC03-ALOAD2 PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-DL22.                                    CI0283
            12            GC03-NYEAR1 PICTURE  9(4).                    CI0283
            12            GC03-GEMTHA PICTURE  99.                      CI0283
            12            GC03-NDAY01 PICTURE  99.                      CI0283
            11            GC03-NSEQ3R PICTURE  S9(5)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-CWHTP  PICTURE  X(3).                    CI0283
            11            GC03-CWHFR  PICTURE  X(3).                    CI0283
            11            GC03-CATRN7 PICTURE  9(6).                    CI0283
            11            GC03-ATAXT5 PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-QSHOT  PICTURE  S9(10)V999               CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-ACINT3 PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-CWHTP1 PICTURE  X(3).                    CI0283
            11            GC03-CWHFR1 PICTURE  X(3).                    CI0283
            11            GC03-ACOMO5 PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-QSHOMU PICTURE  S9(10)V999               CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-ACASH1 PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-FILLER PICTURE  X(04).                   CI0283
            11            GC03-CATRN8 PICTURE  9(6).                    CI0283
            11            GC03-ALOAD1 PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-PSLLD1 PICTURE  S99V999                  CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-QSHOT1 PICTURE  S9(10)V999               CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-ACINT4 PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-CSUSL4 PICTURE  S9.                      CI0283
            11            GC03-ACOMO4 PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            GC03-GD60                                     CI0283
                          REDEFINES            GC03-GD99.               CI0283
            11            GC03-GEOPDD PICTURE  X(8)                     CI0283
                          OCCURS       005     TIMES.                   CI0283
            11            GC03-DLAUP3 PICTURE  9(8)                     CI0283
                          OCCURS       005     TIMES.                   CI0283
            11            GC03-GEOPDB PICTURE  X(8).                    CI0283
            11            GC03-DLAUP4 PICTURE  9(8).                    CI0283
            11            GC03-ITELR2 PICTURE  X.                       CI0283
            11            GC03-IPMTA  PICTURE  X.                       CI0283
            11            GC03-CCSMG  PICTURE  X.                       CI0283
            11            GC03-CPLEC  PICTURE  XX.                      CI0283
            11            GC03-CORTYA PICTURE  X(3).                    CI0283
            11            GC03-CACTBC PICTURE  X(1).                    CI0283
            11            GC03-CGSPIA PICTURE  X.                       CI0283
            11            GC03-IPTRDA PICTURE  X(01).                   CI0283
            11            GC03-GCUSPY PICTURE  X(12).                   CI0283
            11            GC03-CPALLA PICTURE  X(1).                    CI0283
            11            GC03-QSHO5A PICTURE  S9(9)V999                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-IFRSAB PICTURE  X.                       CI0283
            11            GC03-DELOI  PICTURE  9(8).                    CI0283
            11            GC03-IAROAA PICTURE  X.                       CI0283
            11            GC03-ACINVR PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-ILTINA PICTURE  X.                       CI0283
            11            GC03-ALOIDA PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC03-CFUNTA PICTURE  X(2).                    CI0283
            11            GC03-CLGND  PICTURE  X.                       CI0283
            11            GC03-CPH3U  PICTURE  X.                       CI0283
            11            GC03-GESTD  PICTURE  9(8).                    CI0283
            11            GC03-GEEND  PICTURE  9(8).                    CI0283
            11            GC03-CPMTF  PICTURE  99.                      CI0283
            11            GC03-CNAVR  PICTURE  X(1).                    CI0283
            10            GC03-GD70                                     CI0283
                          REDEFINES            GC03-GD99.               CI0283
            11            GC03-CMEMO  PICTURE  X(2).                    CI0283
            11            GC03-ALPLDT PICTURE  9(8).                    CI0283
            11            GC03-CTLPD  PICTURE  9(8).                    CI0283
            11            GC03-CPAYCM PICTURE  X(2).                    CI0283
       01                 GC04.                                         CI0283
            10            GC04-CLCUS  PICTURE  99.                      CI0283
            10            GC04-CCACT  PICTURE  99.                      CI0283
            10            GC04-AFEET  PICTURE  S9(5)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            GC04-ITERF  PICTURE  X.                       CI0283
            10            GC04-ATERF  PICTURE  S9(5)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            GC04-CLDOB  PICTURE  9(8).                    CI0283
            10            GC04-CPLTYP PICTURE  X(14).                   CI0283
            10            GC04-IACFPD PICTURE  X(1).                    CI0283
            10            GC04-FILLER PICTURE  X(14).                   CI0283
       01                 GC06.                                         CI0283
            10            GC06-GELL   PICTURE  9(4)                     CI0283
                          BINARY.                                       CI0283
            10            GC06-GE00.                                    CI0283
            11            GC06-GC06K.                                   CI0283
            12            GC06-NPISQ  PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC06-ACOTD  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC06-PPOTD  PICTURE  S9(3)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC06-QPSTD  PICTURE  S9(7)V999                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC06-CPITC  PICTURE  99.                      CI0283
            11            GC06-ITRNB  PICTURE  X.                       CI0283
            11            GC06-FILLER PICTURE  X(14).                   CI0283
            10            GC06-GE98.                                    CI0283
            11            GC06-FILLER PICTURE  X(240).                  CI0283
            10            GC06-GE10                                     CI0283
                          REDEFINES            GC06-GE98.               CI0283
            11            GC06-CDELI  PICTURE  9(3).                    CI0283
            11            GC06-CPAYC  PICTURE  X(2).                    CI0283
            11            GC06-ICHKP  PICTURE  X.                       CI0283
            11            GC06-CLTIN  PICTURE  9(12).                   CI0283
            11            GC06-IFHAI  PICTURE  X.                       CI0283
            11            GC06-CDQUA  PICTURE  X(2).                    CI0283
            11            GC06-FILLER PICTURE  X(07).                   CI0283
            11            GC06-GE99.                                    CI0283
            12            GC06-FILLER PICTURE  X(212).                  CI0283
            11            GC06-GE01                                     CI0283
                          REDEFINES            GC06-GE99.               CI0283
            12            GC06-NTR    PICTURE  9(8).                    CI0283
            12            GC06-GECKD  PICTURE  9.                       CI0283
            12            GC06-NPBN   PICTURE  X(20).                   CI0283
            12            GC06-CCBAT  PICTURE  99.                      CI0283
            12            GC06-CLID4  PICTURE  X(23).                   CI0283
            12            GC06-GENAL1 PICTURE  X(30)                    CI0283
                          OCCURS       002     TIMES.                   CI0283
            12            GC06-GESAD1 PICTURE  X(30)                    CI0283
                          OCCURS       003     TIMES.                   CI0283
            11            GC06-GE02                                     CI0283
                          REDEFINES            GC06-GE99.               CI0283
            12            GC06-GENAL  PICTURE  X(30)                    CI0283
                          OCCURS       002     TIMES.                   CI0283
            12            GC06-GESAD  PICTURE  X(30)                    CI0283
                          OCCURS       003     TIMES.                   CI0283
            11            GC06-GE03                                     CI0283
                          REDEFINES            GC06-GE99.               CI0283
            12            GC06-NCHKN  PICTURE  9(11).                   CI0283
            11            GC06-GE04                                     CI0283
                          REDEFINES            GC06-GE99.               CI0283
            12            GC06-CTIDAP PICTURE  9(3).                    CI0283
            12            GC06-PRCOD  PICTURE  9(5).                    CI0283
            12            GC06-TDELI  PICTURE  X(30).                   CI0283
            12            GC06-CINCD  PICTURE  9(02).                   CI0283
            10            GC06-GE20                                     CI0283
                          REDEFINES            GC06-GE98.               CI0283
            11            GC06-C299.                                    CI0283
            12            GC06-CTID.                                    CI0283
            13            GC06-CTIDA  PICTURE  9(3).                    CI0283
            13            GC06-CTIDN.                                   CI0283
            14            GC06-CTIDNP PICTURE  X(13).                   CI0283
            14            GC06-CTIDND PICTURE  9(11).                   CI0283
            11            GC06-DCACG9 PICTURE  9(8).                    CI0283
            11            GC06-NAASQ  PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            GC06-CIRAP  PICTURE  XX.                      CI0283
            11            GC06-CTYPE  PICTURE  X.                       CI0283
            11            GC06-INACT  PICTURE  X.                       CI0283
            11            GC06-FILLER PICTURE  X(01).                   CI0283
            11            GC06-ITPAC  PICTURE  X.                       CI0283
            11            GC06-ITAXI  PICTURE  X.                       CI0283
            11            GC06-IOWNC  PICTURE  X.                       CI0283
            11            GC06-CDVCD  PICTURE  X(2).                    CI0283
            11            GC06-CTCUS  PICTURE  999.                     CI0283
            11            GC06-CPMTCB PICTURE  X(3).                    CI0283
            11            GC06-CASTC1 PICTURE  99.                      CI0283
            11            GC06-PRCOD1 PICTURE  9(5).                    CI0283
            11            GC06-CPRSC1 PICTURE  X(9).                    CI0283
            11            GC06-CPRTB  PICTURE  X.                       CI0283
            11            GC06-CBRKD  PICTURE  9(4).                    CI0283
            11            GC06-FILLER PICTURE  X(12).                   CI0283
            10            GC06-GE30                                     CI0283
                          REDEFINES            GC06-GE98.               CI0283
            11            GC06-CFIDC  PICTURE  X(5).                    CI0283
            11            GC06-CPHSE  PICTURE  9(2).                    CI0283
            11            GC06-FILLER PICTURE  X(05).                   CI0283
            11            GC06-IABIN  PICTURE  X.                       CI0283
            11            GC06-PDFND  PICTURE  S999V9(3)                CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            GC06-GE40                                     CI0283
                          REDEFINES            GC06-GE98.               CI0283
            11            GC06-CACCT  PICTURE  X.                       CI0283
            11            GC06-CPAYR  PICTURE  X(2).                    CI0283
            11            GC06-CDELI1 PICTURE  9(3).                    CI0283
            11            GC06-CATRN.                                   CI0283
            12            GC06-CATRF  PICTURE  9(3).                    CI0283
            12            GC06-CATRS  PICTURE  9(3).                    CI0283
            11            GC06-DEFFT  PICTURE  9(8).                    CI0283
            11            GC06-CTYPC  PICTURE  X.                       CI0283
            11            GC06-CIRAPA PICTURE  XX.                      CI0283
            11            GC06-FILLER PICTURE  X(09).                   CI0283
            11            GC06-GE49.                                    CI0283
            12            GC06-FILLER PICTURE  X(208).                  CI0283
            11            GC06-GE41                                     CI0283
                          REDEFINES            GC06-GE49.               CI0283
            12            GC06-NCHKN1 PICTURE  9(6).                    CI0283
            11            GC06-GE42                                     CI0283
                          REDEFINES            GC06-GE49.               CI0283
            12            GC06-CTID1.                                   CI0283
            13            GC06-CTIDA1 PICTURE  9(3).                    CI0283
            13            GC06-CTIDP1 PICTURE  X(13).                   CI0283
            13            GC06-CTIDN1 PICTURE  9(11).                   CI0283
            11            GC06-GE43                                     CI0283
                          REDEFINES            GC06-GE49.               CI0283
            12            GC06-GENAL2 PICTURE  X(30)                    CI0283
                          OCCURS       002     TIMES.                   CI0283
            12            GC06-GESAD2 PICTURE  X(30)                    CI0283
                          OCCURS       003     TIMES.                   CI0283
            11            GC06-GE44                                     CI0283
                          REDEFINES            GC06-GE49.               CI0283
            12            GC06-CTID01.                                  CI0283
            13            GC06-CTIDA6 PICTURE  9(3).                    CI0283
            13            GC06-NTIDP2 PICTURE  X(13).                   CI0283
            13            GC06-CTIDN2 PICTURE  9(11).                   CI0283
            12            GC06-GECKD2 PICTURE  9.                       CI0283
            12            GC06-PACCT  PICTURE  S999V99                  CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC06-PLOAN  PICTURE  S999V99                  CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC06-PADPT  PICTURE  S999V99                  CI0283
                          COMPUTATIONAL-3.                              CI0283
            12            GC06-IPCTL  PICTURE  X.                       CI0283
            12            GC06-IPCTP  PICTURE  X.                       CI0283
            12            GC06-CEUNT  PICTURE  S9(5)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            GC06-GE31                                     CI0283
                          REDEFINES            GC06-GE98.               CI0283
            11            GC06-GCUSPZ PICTURE  X(12).                   CI0283
       01                 GC29.                                         CI0283
            10            GC29-CSLCT  PICTURE  X.                       CI0283
            10            GC29-NGEOR  PICTURE  9(08).                   CI0283
            10            GC29-CACLS2 PICTURE  X(20).                   CI0283
            10            GC29-CAPID  PICTURE  9(2).                    CI0283
            10            GC29-NGEOPA PICTURE  X(08).                   CI0283
            10            GC29-CACLS1 PICTURE  X(20).                   CI0283
            10            GC29-CTRHO  PICTURE  9(8).                    CI0283
            10            GC29-GETIM3 PICTURE  S9(7)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            GC29-GEOPD9 PICTURE  X(8).                    CI0283
            10            GC29-DCACG1 PICTURE  9(8).                    CI0283
            10            GC29-IWEBBT PICTURE  X.                       CI0283
            10            GC29-CAVER  PICTURE  X.                       CI0283
            10            GC29-FILLER PICTURE  X(30).                   CI0283
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
      **              TABLE TA5B ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA5B-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=5B FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA5B.                                                CI0283
           04    G-TA5B-PARAM.                                          CI0283
             10  G-TA5B-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0283
                        VALUE      +154.                                CI0283
             10  G-TA5B-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0283
                        VALUE      +001.                                CI0283
             10  G-TA5B-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0283
                        VALUE      +017.                                CI0283
             10  G-TA5B-NUAPP  PICTURE 99                               CI0283
                        VALUE       0.                                  CI0283
             10  G-TA5B-NUTAB  PICTURE X(6)                             CI0283
                        VALUE 'TA005B'.                                 CI0283
             10  G-TA5B-TABFO  PICTURE XX                 VALUE SPACE.  CI0283
             10  G-TA5B-TABCR  PICTURE XX                 VALUE SPACE.  CI0283
             10  G-TA5B-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0283
             10  G-TA5B-NUSSC  PICTURE X  VALUE   ' '.                  CI0283
             10  G-TA5B-NUSSY  PICTURE X                  VALUE SPACE.  CI0283
             10  G-TA5B-TRANID PICTURE X(4)               VALUE SPACE.  CI0283
             10  G-TA5B-FILSYS.                                         CI0283
             15  G-TA5B-USERC  PICTURE X(6)               VALUE SPACE.  CI0283
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0283
           04             TA5B.                                         CI0283
            10            TA5B-GAPSC.                                   CI0283
            11            TA5B-CTIDA  PICTURE  9(3)                     CI0283
                          VALUE                ZERO.                    CI0283
            11            TA5B-PRCOD  PICTURE  9(5)                     CI0283
                          VALUE                ZERO.                    CI0283
            11            TA5B-PRSCD  PICTURE  X(9)                     CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-PRCODX PICTURE  9(5)                     CI0283
                          VALUE                ZERO.                    CI0283
            10            TA5B-PRCSUB PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-PRCAUT PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-PRCBAS PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-PRCSTK PICTURE  XX                       CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-PRCPRE PICTURE  X(4)                     CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-IBDUP  PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-IUSPR  PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-CVSYS  PICTURE  X(2)                     CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-IDTOD  PICTURE  X(1)                     CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-GRSFC  PICTURE  99                       CI0283
                          VALUE                ZERO.                    CI0283
            10            TA5B-ZDA18  PICTURE  X(18)                    CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-CMPCTB PICTURE  X(4)                     CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-ITERM  PICTURE  X(1)                     CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-AMFAC  PICTURE  S9(7)                    CI0283
                          VALUE                ZERO.                    CI0283
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-CPRBK  PICTURE  X(3)                     CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-CFXDM  PICTURE  99                       CI0283
                          VALUE                ZERO.                    CI0283
            10            TA5B-NGLCS  PICTURE  X(5)                     CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-NDFCS  PICTURE  X(5)                     CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-CTNLI  PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-CBANK  PICTURE  X(03)                    CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-ISYPO  PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-ISYPP  PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-ICOPT  PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-IANPY  PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-IDSAR  PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-ICIPT  PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-IANDS  PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-IKPMA  PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-INMWT  PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-IVANT  PICTURE  X(1)                     CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-ISDAV  PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-IUDAV  PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TA5B-ZDA15  PICTURE  X(15)                    CI0283
                          VALUE                SPACE.                   CI0283
      **                                                                ADUTAB
      ******************************************************************ADUTAB
      **              TABLE TG04 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TG04-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TG DSL=TG SEL=04 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TG04.                                                CI0283
           04    G-TG04-PARAM.                                          CI0283
             10  G-TG04-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0283
                        VALUE      +018.                                CI0283
             10  G-TG04-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0283
                        VALUE      +001.                                CI0283
             10  G-TG04-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0283
                        VALUE      +011.                                CI0283
             10  G-TG04-NUAPP  PICTURE 99                               CI0283
                        VALUE       0.                                  CI0283
             10  G-TG04-NUTAB  PICTURE X(6)                             CI0283
                        VALUE 'TG0004'.                                 CI0283
             10  G-TG04-TABFO  PICTURE XX                 VALUE SPACE.  CI0283
             10  G-TG04-TABCR  PICTURE XX                 VALUE SPACE.  CI0283
             10  G-TG04-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0283
             10  G-TG04-NUSSC  PICTURE X  VALUE   ' '.                  CI0283
             10  G-TG04-NUSSY  PICTURE X                  VALUE SPACE.  CI0283
             10  G-TG04-TRANID PICTURE X(4)               VALUE SPACE.  CI0283
             10  G-TG04-FILSYS.                                         CI0283
             15  G-TG04-USERC  PICTURE X(6)               VALUE SPACE.  CI0283
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0283
           04             TG04.                                         CI0283
            10            TG04-GTG01K.                                  CI0283
            11            TG04-CTIDA  PICTURE  9(3)                     CI0283
                          VALUE                ZERO.                    CI0283
            11            TG04-PRCOD  PICTURE  9(5)                     CI0283
                          VALUE                ZERO.                    CI0283
            11            TG04-CTRTP  PICTURE  X(2)                     CI0283
                          VALUE                SPACE.                   CI0283
            11            TG04-CPORT  PICTURE  X                        CI0283
                          VALUE                SPACE.                   CI0283
            10            TG04-CSCRNU PICTURE  X(4)                     CI0283
                          VALUE                SPACE.                   CI0283
            10            TG04-CATRF  PICTURE  9(3)                     CI0283
                          VALUE                ZERO.                    CI0283
      **                                                                ADUTAB
      *WORK FIELDS
      *
      *!WI
       01 WS01-DCAG9L
                        PICTURE 9(8).                                   CI0283
      ** FIELD USED TO INDICATE IF TA5B ACCESS WAS SUCCESSFUL
      *
       01  TA5B-IK                 PIC X(01)   VALUE '0'.
      *
      ** FIELD USED TO INDICATE IF TG04 ACCESS WAS SUCCESSFUL
      *
       01  TG04-IK                 PIC X(01)   VALUE '0'.
      *
       01  WS01-PRSCD.
           05  FILLER              PIC X(5).
           05  WS01-ICODE          PIC 9(1).
           05  FILLER              PIC X(3).
      *
      *!WI
       01  WS01-ASPAM              VALUE 0
                        PICTURE S9(9)V99                                CI0283
                          COMPUTATIONAL-3.                              CI0283
       01  WS00-GETIM    PIC X(8).
       01   DEBUT-WSS.                                                  CI0283
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0283
            05   IK     PICTURE X.                                      CI0283
       01  CONSTANTES-PAC.                                              CI0283
           05  FILLER  PICTURE X(87)   VALUE                            CI0283
                     '6015 CAT09/08/14CI0283ADMIN   14:35:14CI0283P AMERCI0283
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0283
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0283
           05  NUGNA   PICTURE X(5).                                    CI0283
           05  APPLI   PICTURE X(3).                                    CI0283
           05  DATGN   PICTURE X(8).                                    CI0283
           05  PROGR   PICTURE X(6).                                    CI0283
           05  CODUTI  PICTURE X(8).                                    CI0283
           05  TIMGN   PICTURE X(8).                                    CI0283
           05  PROGE   PICTURE X(8).                                    CI0283
           05  COBASE  PICTURE X(4).                                    CI0283
           05  DATGNC  PICTURE X(10).                                   CI0283
           05  RELEAS  PICTURE X(7).                                    CI0283
           05  DATGE   PICTURE X(10).                                   CI0283
           05  DATSQ   PICTURE X(10).                                   CI0283
       01  DATCE.                                                       CI0283
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0283
         05  DATOR.                                                     CI0283
           10  DATOA  PICTURE XX.                                       CI0283
           10  DATOM  PICTURE XX.                                       CI0283
           10  DATOJ  PICTURE XX.                                       CI0283
       01   VARIABLES-CONDITIONNELLES.                                  CI0283
            05                  FT      PICTURE X VALUE '0'.            CI0283
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0283
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0283
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0283
            05       5-GC00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0283
       01               S-GC01-SSA.                                     CI0283
            10         S1-GC01-SEGNAM PICTURE X(8)                      CI0283
                                      VALUE 'GC01    '.                 CI0283
            10         S1-GC01-CCOM   PICTURE X VALUE '*'.              CI0283
            10          S-GC01-CCOD   PICTURE X(5)                      CI0283
                                      VALUE '-----'.                    CI0283
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0283
       01            S-GCU01-SSA.                                       CI0283
            10      S1-GCU01-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC01    '.                 CI0283
            10      S1-GCU01-CCOM   PICTURE X VALUE '*'.                CI0283
            10       S-GCU01-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            10      S1-GCU01-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(GC01K'.                   CI0283
            10       S-GCU01-OPER  PICTURE XX VALUE ' ='.               CI0283
            10       S-GCU01-GC01K.                                     CI0283
            11       S-GCU01-C299.                                      CI0283
            12       S-GCU01-CTID.                                      CI0283
            13       S-GCU01-CTIDA    PICTURE  9(3).                    CI0283
            13       S-GCU01-CTIDN.                                     CI0283
            14       S-GCU01-CTIDNP   PICTURE  X(13).                   CI0283
            14       S-GCU01-CTIDND   PICTURE  9(11).                   CI0283
            10  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01               S-GC03-SSA.                                     CI0283
            10         S1-GC03-SEGNAM PICTURE X(8)                      CI0283
                                      VALUE 'GC03    '.                 CI0283
            10         S1-GC03-CCOM   PICTURE X VALUE '*'.              CI0283
            10          S-GC03-CCOD   PICTURE X(5)                      CI0283
                                      VALUE '-----'.                    CI0283
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0283
       01            S-GCA03-SSA.                                       CI0283
            11      S1-GCA03-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GCA03-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCA03-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCA03-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(CAATY'.                   CI0283
            11       S-GCA03-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCA03-CAATY    PICTURE  9(3).                    CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GCB03-SSA.                                       CI0283
            11      S1-GCB03-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GCB03-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCB03-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCB03-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(CVSYS'.                   CI0283
            11       S-GCB03-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCB03-CVSYS    PICTURE  X(2).                    CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GCC03-SSA.                                       CI0283
            11      S1-GCC03-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GCC03-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCC03-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCC03-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(CASTC'.                   CI0283
            11       S-GCC03-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCC03-CASTC    PICTURE  99.                      CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GCD03-SSA.                                       CI0283
            11      S1-GCD03-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GCD03-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCD03-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCD03-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(CACTO'.                   CI0283
            11       S-GCD03-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCD03-CACTO    PICTURE  9(3).                    CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GCE03-SSA.                                       CI0283
            11      S1-GCE03-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GCE03-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCE03-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCE03-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(IPULL'.                   CI0283
            11       S-GCE03-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCE03-IPULL    PICTURE  X.                       CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GCF03-SSA.                                       CI0283
            11      S1-GCF03-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GCF03-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCF03-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCF03-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(DTRAC'.                   CI0283
            11       S-GCF03-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCF03-DTRAC    PICTURE  9(8).                    CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GCG03-SSA.                                       CI0283
            11      S1-GCG03-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GCG03-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCG03-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCG03-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(CTRSO'.                   CI0283
            11       S-GCG03-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCG03-CTRSO    PICTURE  9(02).                   CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GCH03-SSA.                                       CI0283
            11      S1-GCH03-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GCH03-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCH03-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCH03-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(NTRCE'.                   CI0283
            11       S-GCH03-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCH03-NTRCE    PICTURE  9(06).                   CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GCI03-SSA.                                       CI0283
            11      S1-GCI03-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GCI03-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCI03-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCI03-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(ITRAN'.                   CI0283
            11       S-GCI03-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCI03-ITRAN    PICTURE  X.                       CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GCJ03-SSA.                                       CI0283
            11      S1-GCJ03-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GCJ03-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCJ03-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCJ03-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(DEFFT'.                   CI0283
            11       S-GCJ03-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCJ03-DEFFT    PICTURE  9(8).                    CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GCK03-SSA.                                       CI0283
            11      S1-GCK03-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GCK03-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCK03-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCK03-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(CPMTCA'.                  CI0283
            11       S-GCK03-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCK03-CPMTCA   PICTURE  XXX.                     CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GCL03-SSA.                                       CI0283
            11      S1-GCL03-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GCL03-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCL03-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCL03-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(ACASH'.                   CI0283
            11       S-GCL03-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCL03-ACASH    PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GCN03-SSA.                                       CI0283
            11      S1-GCN03-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GCN03-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCN03-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCN03-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(CRREV'.                   CI0283
            11       S-GCN03-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCN03-CRREV    PICTURE  X(3).                    CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GCO03-SSA.                                       CI0283
            11      S1-GCO03-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GCO03-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCO03-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCO03-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(CSYST'.                   CI0283
            11       S-GCO03-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCO03-CSYST    PICTURE  99.                      CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GCU03-SSA.                                       CI0283
            11      S1-GCU03-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GCU03-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCU03-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCU03-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(GC03K'.                   CI0283
            11       S-GCU03-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCU03-GC03K.                                     CI0283
            12       S-GCU03-DCACG9   PICTURE  9(8).                    CI0283
            12       S-GCU03-NAASQ    PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GC103-SSA.                                       CI0283
            12      S1-GC103-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            12      S1-GC103-CCOM   PICTURE X VALUE '*'.                CI0283
            12       S-GC103-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            12      S1-GC103-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(XDCACG9'.                 CI0283
            12       S-GC103-OPER  PICTURE XX VALUE ' ='.               CI0283
            12       S-GC103-DCACG9   PICTURE  9(8).                    CI0283
            12  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GC203-SSA.                                       CI0283
            11      S1-GC203-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GC203-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GC203-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GC203-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(XGEAUN'.                  CI0283
            11       S-GC203-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GC203-GEAUN    PICTURE  9(5).                    CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GC303-SSA.                                       CI0283
            11      S1-GC303-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GC303-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GC303-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GC303-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(XGEOPD2'.                 CI0283
            11       S-GC303-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GC303-GEOPD2   PICTURE  X(8).                    CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GC403-SSA.                                       CI0283
            11      S1-GC403-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            11      S1-GC403-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GC403-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GC403-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(XNBTCH'.                  CI0283
            11       S-GC403-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GC403-NBTCH    PICTURE  9(4).                    CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GC803-SSA.                                       CI0283
            12      S1-GC803-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC03    '.                 CI0283
            12      S1-GC803-CCOM   PICTURE X VALUE '*'.                CI0283
            12       S-GC803-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            12      S1-GC803-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(XNAASQ'.                  CI0283
            12       S-GC803-OPER  PICTURE XX VALUE ' ='.               CI0283
            12       S-GC803-NAASQ    PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            12  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01               S-GC04-SSA.                                     CI0283
            10         S1-GC04-SEGNAM PICTURE X(8)                      CI0283
                                      VALUE 'GC04    '.                 CI0283
            10         S1-GC04-CCOM   PICTURE X VALUE '*'.              CI0283
            10          S-GC04-CCOD   PICTURE X(5)                      CI0283
                                      VALUE '-----'.                    CI0283
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0283
       01               S-GC06-SSA.                                     CI0283
            10         S1-GC06-SEGNAM PICTURE X(8)                      CI0283
                                      VALUE 'GC06    '.                 CI0283
            10         S1-GC06-CCOM   PICTURE X VALUE '*'.              CI0283
            10          S-GC06-CCOD   PICTURE X(5)                      CI0283
                                      VALUE '-----'.                    CI0283
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0283
       01            S-GCF06-SSA.                                       CI0283
            11      S1-GCF06-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC06    '.                 CI0283
            11      S1-GCF06-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCF06-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCF06-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(PRCOD1'.                  CI0283
            11       S-GCF06-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCF06-PRCOD1   PICTURE  9(5).                    CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01            S-GCU06-SSA.                                       CI0283
            11      S1-GCU06-SEGNAM PICTURE X(8)                        CI0283
                                      VALUE 'GC06    '.                 CI0283
            11      S1-GCU06-CCOM   PICTURE X VALUE '*'.                CI0283
            11       S-GCU06-CCOD   PICTURE X(5)                        CI0283
                                      VALUE '-----'.                    CI0283
            11      S1-GCU06-FLDNAM PICTURE X(9)                        CI0283
                                      VALUE '(GC06K'.                   CI0283
            11       S-GCU06-OPER  PICTURE XX VALUE ' ='.               CI0283
            11       S-GCU06-GC06K.                                     CI0283
            12       S-GCU06-NPISQ    PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11  FILLER   PICTURE X    VALUE ')'.                        CI0283
       01               S-GC29-SSA.                                     CI0283
            10         S1-GC29-SEGNAM PICTURE X(8)                      CI0283
                                      VALUE 'GC29    '.                 CI0283
            10         S1-GC29-CCOM   PICTURE X VALUE '*'.              CI0283
            10          S-GC29-CCOD   PICTURE X(5)                      CI0283
                                      VALUE '-----'.                    CI0283
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0283
       01   ZONES-UTILISATEUR PICTURE X.                                CI0283
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
      *!WF DSP=XA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XA00.                                         CI0283
          05              XA00-SUITE.                                   CI0283
            15       FILLER         PICTURE  X(00106).                  CI0283
       01                 XA06  REDEFINES      XA00.                    CI0283
            10            XA06-XDBPCB.                                  CI0283
            11            XA06-XDBDNM PICTURE  X(08).                   CI0283
            11            XA06-XSEGLV PICTURE  X(02).                   CI0283
            11            XA06-XRC    PICTURE  X(02).                   CI0283
            11            XA06-XPROPT PICTURE  X(04).                   CI0283
            11            XA06-FILLER PICTURE  S9(5)                    CI0283
                          BINARY.                                       CI0283
            11            XA06-XSEGNM PICTURE  X(08).                   CI0283
            11            XA06-XKEYLN PICTURE  S9(05)                   CI0283
                          BINARY.                                       CI0283
            11            XA06-XSEGNB PICTURE  9(05)                    CI0283
                          BINARY.                                       CI0283
            11            XA06-XCOKEY PICTURE  X(70).                   CI0283
      *V218 ORDER TICKET INFORMATION
      *!WF DSP=K8 DSL=V2 SEL=18 FOR=I LEV=1 PLT=10
       01                 K800.                                         CI0283
          05              K800-SUITE.                                   CI0283
            15       FILLER         PICTURE  X(00119).                  CI0283
       01                 K818  REDEFINES      K800.                    CI0283
            10            K818-CSLCT  PICTURE  X.                       CI0283
            10            K818-NGEOR  PICTURE  9(08).                   CI0283
            10            K818-CACLS2 PICTURE  X(20).                   CI0283
            10            K818-CAPID  PICTURE  9(2).                    CI0283
            10            K818-NGEOPA PICTURE  X(08).                   CI0283
            10            K818-CACLS1 PICTURE  X(20).                   CI0283
            10            K818-CTRHO  PICTURE  9(8).                    CI0283
            10            K818-GETIM3 PICTURE  S9(7)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            K818-GEOPD9 PICTURE  X(8).                    CI0283
            10            K818-DCACG1 PICTURE  9(8).                    CI0283
            10            K818-IWEBBT PICTURE  X.                       CI0283
            10            K818-CAVER  PICTURE  X.                       CI0283
            10            K818-FILLER PICTURE  X(30).                   CI0283
      *K943 BROWSER INPUT
      *!WF DSP=K9 DSL=K9 SEL=43 FOR=I DES=1 LEV=1 PLT=10
       01                 K943.                                         CI0283
            10            K943-CTID   PICTURE  X(27).                   CI0283
            10            K943-CTTYPG PICTURE  X(04).                   CI0283
            10            K943-CPORTA PICTURE  X.                       CI0283
            10            K943-CDELI  PICTURE  9(3).                    CI0283
            10            K943-ADBRQ  PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            K943-PWHLDF PICTURE  999V999                  CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            K943-FILLER PICTURE  X(08).                   CI0283
            10            K943-GEAUN  PICTURE  9(5).                    CI0283
            10            K943-GEOPD2 PICTURE  X(8).                    CI0283
            10            K943-GETIM  PICTURE  S9(7)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            K943-DEFFT  PICTURE  9(8).                    CI0283
            10            K943-DCACG  PICTURE  9(8).                    CI0283
            10            K943-ATSA8  PICTURE  S9(07)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            K943-CPAYC  PICTURE  X(2).                    CI0283
            10            K943-CLID   PICTURE  X(23).                   CI0283
            10            K943-GECSQ  PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            K943-NTR    PICTURE  9(8).                    CI0283
            10            K943-GECKD  PICTURE  9.                       CI0283
            10            K943-NPBN   PICTURE  X(20).                   CI0283
            10            K943-CCBAT  PICTURE  99.                      CI0283
            10            K943-GENAL1 PICTURE  X(30).                   CI0283
            10            K943-GENAL2 PICTURE  X(30).                   CI0283
            10            K943-GESAD1 PICTURE  X(30).                   CI0283
            10            K943-GESAD2 PICTURE  X(30).                   CI0283
            10            K943-GESAD3 PICTURE  X(30).                   CI0283
            10            K943-CLID4  PICTURE  X(23).                   CI0283
            10            K943-FILLER PICTURE  X(165).                  CI0283
      *
      *QT58 MC58 INPUT
      *!WF DSP=QT DSL=QT SEL=58 FOR=I LEV=1 PLT=10
       01                 QT00.                                         CI0283
          05              QT00-SUITE.                                   CI0283
            15       FILLER         PICTURE  X(02300).                  CI0283
       01                 QT58  REDEFINES      QT00.                    CI0283
            10            QT58-QT5K.                                    CI0283
            11            QT58-C299.                                    CI0283
            12            QT58-CTID.                                    CI0283
            13            QT58-CTIDA  PICTURE  9(3).                    CI0283
            13            QT58-CTIDN.                                   CI0283
            14            QT58-CTIDNP PICTURE  X(13).                   CI0283
            14            QT58-CTIDND PICTURE  9(11).                   CI0283
            11            QT58-GECKD2 PICTURE  9.                       CI0283
            11            QT58-NSEQ5  PICTURE  9(5).                    CI0283
            11            QT58-CTSTA  PICTURE  99.                      CI0283
            11            QT58-CTSTAL PICTURE  X(10).                   CI0283
            11            QT58-CTOWN  PICTURE  9(3).                    CI0283
            11            QT58-CTTLN1 PICTURE  X(30).                   CI0283
            11            QT58-CTTLN2 PICTURE  X(30).                   CI0283
            11            QT58-CTTLN3 PICTURE  X(30).                   CI0283
            11            QT58-CTTBO1 PICTURE  X(45).                   CI0283
            11            QT58-CTTBO2 PICTURE  X(45).                   CI0283
            11            QT58-CTEFD  PICTURE  9(8).                    CI0283
            11            QT58-CTIAD  PICTURE  9(8).                    CI0283
            11            QT58-CTCUS  PICTURE  999.                     CI0283
            11            QT58-GR98.                                    CI0283
            12            QT58-GRID.                                    CI0283
            13            QT58-GRIDC  PICTURE  9(3).                    CI0283
            13            QT58-GRIDN.                                   CI0283
            14            QT58-GRIDNP PICTURE  99.                      CI0283
            14            QT58-GRIDND PICTURE  9(8).                    CI0283
            11            QT58-CQACT  PICTURE  999.                     CI0283
            11            QT58-CTCCI  PICTURE  X.                       CI0283
            11            QT58-CIRAS  PICTURE  999.                     CI0283
            11            QT58-CIRAT  PICTURE  999.                     CI0283
            11            QT58-IACVD  PICTURE  X.                       CI0283
            11            QT58-FILLER PICTURE  X(4).                    CI0283
            11            QT58-PRCODA PICTURE  X(5).                    CI0283
            11            QT58-PRCMN  PICTURE  X(20).                   CI0283
            11            QT58-MRPLN  PICTURE  X(30).                   CI0283
            11            QT58-CPRDG  PICTURE  9(2).                    CI0283
            11            QT58-CPRDA1 PICTURE  9(3).                    CI0283
            11            QT58-PRSCD  PICTURE  X(9).                    CI0283
            11            QT58-MSP03  PICTURE  X(3).                    CI0283
            11            QT58-CGRLI  PICTURE  X.                       CI0283
            11            QT58-ITERM  PICTURE  X(1).                    CI0283
            11            QT58-IVARP  PICTURE  X.                       CI0283
            11            QT58-DVALU  PICTURE  9(8).                    CI0283
            11            QT58-AACTV  PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ACCTVC PICTURE  X(20).                   CI0283
            11            QT58-ITXTI  PICTURE  X.                       CI0283
            11            QT58-ASANP  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ACINV  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-CELBL  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-NMESS2 PICTURE  S9(6)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-FILLER PICTURE  X(1).                    CI0283
            11            QT58-PRCLN  PICTURE  X(60).                   CI0283
            11            QT58-GECKD  PICTURE  9.                       CI0283
            11            QT58-MPLNA  PICTURE  X(19).                   CI0283
            11            QT58-CQACTL PICTURE  X(45).                   CI0283
            11            QT58-CRQPA  PICTURE  9(3).                    CI0283
            11            QT58-IVANT  PICTURE  X(1).                    CI0283
            11            QT58-IDBRP  PICTURE  X(1).                    CI0283
            11            QT58-IANPY  PICTURE  X.                       CI0283
            11            QT58-IVARP1 PICTURE  X.                       CI0283
            11            QT58-FILLER PICTURE  X(27).                   CI0283
            11            QT58-NSEQ2A PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-NSEQ2P PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-MRPSN  PICTURE  X(12).                   CI0283
            11            QT58-GEHCD  PICTURE  9(3)                     CI0283
                          OCCURS       002     TIMES.                   CI0283
            11            QT58-GEHCSU PICTURE  9(5)                     CI0283
                          OCCURS       002     TIMES.                   CI0283
            11            QT58-PRCSN  PICTURE  X(9).                    CI0283
            11            QT58-CGRMF  PICTURE  X.                       CI0283
            11            QT58-IGFEX  PICTURE  X.                       CI0283
            11            QT58-CLIDP  PICTURE  X(23).                   CI0283
            11            QT58-CLCTRC PICTURE  9(3).                    CI0283
            11            QT58-ADINP  PICTURE  X(20).                   CI0283
            11            QT58-CLCTRA PICTURE  9(3).                    CI0283
            11            QT58-GRPLC  PICTURE  99.                      CI0283
            11            QT58-CIDRP  PICTURE  99.                      CI0283
            11            QT58-FILLER PICTURE  X(01).                   CI0283
            11            QT58-AVMTOT PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-AVCSH  PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-AMARC  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-AVLMX  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-AVLMN  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-INDRS  PICTURE  X.                       CI0283
            11            QT58-MPRN4  PICTURE  X(35).                   CI0283
            11            QT58-FILLER PICTURE  X(1).                    CI0283
            11            QT58-ACVALM PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-INDRSA PICTURE  X(2).                    CI0283
            11            QT58-DXTMSA PICTURE  X(26).                   CI0283
            11            QT58-NMESS6 PICTURE  S9(6)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-NMESS7 PICTURE  S9(6)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-IBIDSA PICTURE  X.                       CI0283
            11            QT58-IBIDSB PICTURE  X.                       CI0283
            11            QT58-INSPOS PICTURE  X.                       CI0283
            11            QT58-INSPOD PICTURE  X.                       CI0283
            11            QT58-ACBALX PICTURE  X(20).                   CI0283
            11            QT58-AINVMX PICTURE  X(20).                   CI0283
            11            QT58-AMARCX PICTURE  X(20).                   CI0283
            11            QT58-AVMTOX PICTURE  X(20).                   CI0283
            11            QT58-IMNPR  PICTURE  X.                       CI0283
            11            QT58-ISSPL  PICTURE  X.                       CI0283
            11            QT58-AVMTOI PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-AVCSHI PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-APOSC  PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-AVLMXI PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-AVLMN1 PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-AVLMN2 PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-FILLER PICTURE  X(05).                   CI0283
            10            QT58-QT5A.                                    CI0283
            11            QT58-CLID   PICTURE  X(23).                   CI0283
            11            QT58-GECKD1 PICTURE  9.                       CI0283
            11            QT58-MCLNM  PICTURE  X(40).                   CI0283
            11            QT58-MCLNM2 PICTURE  X(40).                   CI0283
            11            QT58-CLTYP  PICTURE  X.                       CI0283
            11            QT58-CLDOB  PICTURE  9(8).                    CI0283
            11            QT58-CLDTH  PICTURE  X.                       CI0283
            11            QT58-CLTIN  PICTURE  9(12).                   CI0283
            11            QT58-CLTINC PICTURE  9.                       CI0283
            11            QT58-GESAD1 PICTURE  X(30).                   CI0283
            11            QT58-GESAD2 PICTURE  X(30).                   CI0283
            11            QT58-GESAD3 PICTURE  X(30).                   CI0283
            11            QT58-GECIT  PICTURE  X(25).                   CI0283
            11            QT58-GECTRY PICTURE  X(20).                   CI0283
            11            QT58-GEPCD  PICTURE  X(12).                   CI0283
            11            QT58-GEST   PICTURE  X(8).                    CI0283
            11            QT58-GEADS  PICTURE  9.                       CI0283
            11            QT58-GECSD  PICTURE  9(8).                    CI0283
            11            QT58-QCLAGE PICTURE  9(3)V9                   CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-FILLER PICTURE  X(06).                   CI0283
            10            QT58-QT5T.                                    CI0283
            11            QT58-ATFRA  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-AGOFD  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-APRMX  PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-APRMN  PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-IOWNC  PICTURE  X.                       CI0283
            11            QT58-COWNF  PICTURE  X(30).                   CI0283
            11            QT58-CTYPE  PICTURE  X.                       CI0283
            11            QT58-CIRAC  PICTURE  X(5).                    CI0283
            11            QT58-CTXMT  PICTURE  9(2).                    CI0283
            11            QT58-AMIND  PICTURE  S9(7)V99.                CI0283
            11            QT58-AMAXAR PICTURE  S9(7)V99.                CI0283
            11            QT58-QSHOWQ PICTURE  S9(9)V999                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-QSHOW0 PICTURE  S9(10)V999               CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-PPOT1  PICTURE  S9(3)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-PACT1  PICTURE  S999V999                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-IPRTA  PICTURE  X.                       CI0283
            11            QT58-FILLER PICTURE  X.                       CI0283
            11            QT58-CLCUS  PICTURE  99.                      CI0283
            11            QT58-CCDSCW PICTURE  9(2).                    CI0283
            11            QT58-CCACT  PICTURE  99.                      CI0283
            11            QT58-CIRAG.                                   CI0283
            12            QT58-CIRAP  PICTURE  XX                       CI0283
                          OCCURS       010     TIMES.                   CI0283
            11            QT58-ITERF  PICTURE  X.                       CI0283
            11            QT58-IACFPD PICTURE  X(1).                    CI0283
            11            QT58-AFEET  PICTURE  S9(5)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ATERF  PICTURE  S9(5)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-CLIDNB PICTURE  9(8).                    CI0283
            11            QT58-ALOAD  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ASURR  PICTURE  S9(07)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ASHIS  PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-AMNBL  PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-APNAC  PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ANGOF  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-CPLTYP PICTURE  X(14).                   CI0283
            10            QT58-QT5N.                                    CI0283
            11            QT58-IARRAN PICTURE  X.                       CI0283
            11            QT58-GESTD1 PICTURE  9(8).                    CI0283
            11            QT58-GEEND1 PICTURE  S9(8)                    CI0283
                          BINARY.                                       CI0283
            11            QT58-GESTD  PICTURE  9(8).                    CI0283
            11            QT58-GEEND  PICTURE  9(8).                    CI0283
            11            QT58-NSQ4B2 PICTURE  9(8)                     CI0283
                          BINARY.                                       CI0283
            11            QT58-CDEST  PICTURE  99.                      CI0283
            11            QT58-DEFFT  PICTURE  9(8).                    CI0283
            11            QT58-CPMTF  PICTURE  99.                      CI0283
            11            QT58-CPMTG  PICTURE  99.                      CI0283
            11            QT58-MPMTFL PICTURE  X(24).                   CI0283
            11            QT58-MPMTFE PICTURE  X(24).                   CI0283
            11            QT58-DLAUP  PICTURE  9(8).                    CI0283
            11            QT58-NSEQ4B PICTURE  9(8)                     CI0283
                          BINARY.                                       CI0283
            11            QT58-QSACTF PICTURE  9(3).                    CI0283
            11            QT58-QSACTT PICTURE  9(3).                    CI0283
            11            QT58-CCONF  PICTURE  X(25).                   CI0283
            11            QT58-DCONF  PICTURE  9(8).                    CI0283
            11            QT58-DTIMT  PICTURE  X(8).                    CI0283
            11            QT58-CACTS  PICTURE  X.                       CI0283
            11            QT58-ADBRQ  PICTURE  S9(11)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-DNPMT  PICTURE  9(8).                    CI0283
            11            QT58-NAPDS  PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-CDEST1 PICTURE  99.                      CI0283
            11            QT58-CLANR1 PICTURE  X(23).                   CI0283
            11            QT58-FILLER PICTURE  X(01).                   CI0283
            10            QT58-FILLER PICTURE  X(600).                  CI0283
            10            QT58-QT5C                                     CI0283
                          REDEFINES            QT58-FILLER.             CI0283
            11            QT58-CESLD  PICTURE  9(8).                    CI0283
            11            QT58-PCIRB5 PICTURE  S9(3)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-PANYDD PICTURE  S9(3)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-CEIT   PICTURE  9(3).                    CI0283
            11            QT58-PPART  PICTURE  9(3)V99                  CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-DTRME  PICTURE  9(8).                    CI0283
            11            QT58-CEIRND PICTURE  9(8).                    CI0283
            11            QT58-DANNIA PICTURE  9(8).                    CI0283
            11            QT58-AAPAA  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-CELBDT PICTURE  9(8).                    CI0283
            11            QT58-CEIIS  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-DTRME1 PICTURE  9(8).                    CI0283
            11            QT58-GMKTS.                                   CI0283
            12            QT58-DTRME2 PICTURE  9(8)                     CI0283
                          OCCURS       005     TIMES.                   CI0283
            12            QT58-DTRME3 PICTURE  9(8)                     CI0283
                          OCCURS       005     TIMES.                   CI0283
            11            QT58-ALINT  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-CEHCD  PICTURE  9(3)                     CI0283
                          OCCURS       006     TIMES.                   CI0283
            11            QT58-CEFOTR PICTURE  S9(3)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-DGPED  PICTURE  9(8).                    CI0283
            11            QT58-DIPED  PICTURE  9(8).                    CI0283
            11            QT58-FILLER PICTURE  X(409).                  CI0283
            10            QT58-QT5F                                     CI0283
                          REDEFINES            QT58-FILLER.             CI0283
            11            QT58-DLAUP2 PICTURE  9(8).                    CI0283
            11            QT58-QSHOW  PICTURE  S9(10)V999               CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-AFAVP  PICTURE  S9(4)V9(3)               CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-QSHIS  PICTURE  S9(10)V999               CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-QSHNM  PICTURE  S9(10)V999.              CI0283
            11            QT58-QSHOM  PICTURE  S9(10)V999               CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ADDAC  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-QSHES  PICTURE  S9(10)V999               CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-NDCUS  PICTURE  X(9).                    CI0283
            11            QT58-CSTKR5 PICTURE  X(5).                    CI0283
            11            QT58-NACID  PICTURE  S9(11)                   CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-AGOFD2 PICTURE  S9(9)V99.                CI0283
            11            QT58-TCBAT  PICTURE  X(21).                   CI0283
            11            QT58-FILLER PICTURE  X(490).                  CI0283
            10            QT58-QT5L                                     CI0283
                          REDEFINES            QT58-FILLER.             CI0283
            11            QT58-ALDBEN PICTURE  S9(09)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-APREL  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ALMODE PICTURE  99.                      CI0283
            11            QT58-ITMEC  PICTURE  X(1).                    CI0283
            11            QT58-ITAMR  PICTURE  X(1).                    CI0283
            11            QT58-MPMTF  PICTURE  X(14).                   CI0283
            11            QT58-TPLNL  PICTURE  X(30).                   CI0283
            11            QT58-ASBENA PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ASBENB PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ASBENC PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ASBENE PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ASBENF PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-GESTNS PICTURE  X(2).                    CI0283
            11            QT58-CTWHPB PICTURE  9(3)V999.                CI0283
            11            QT58-CTWHCB PICTURE  X.                       CI0283
            11            QT58-AMVA1  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ASPAM  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ACTCH  PICTURE  S9(07)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-AMXLN  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ALFGH  PICTURE  999.                     CI0283
            11            QT58-ALPLNI PICTURE  9.                       CI0283
            11            QT58-ATSA8  PICTURE  S9(07)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-CVALB  PICTURE  X(3).                    CI0283
            11            QT58-ASURRN PICTURE  S9(07)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ASURRW PICTURE  S9(07)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ATLTB  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-AEARN0 PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ATFPI  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-AEARN1 PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ISELO  PICTURE  X.                       CI0283
            11            QT58-CCLAC  PICTURE  X.                       CI0283
            11            QT58-ALINNO PICTURE  99.                      CI0283
            11            QT58-ALPLNJ PICTURE  9.                       CI0283
            11            QT58-COLPL  PICTURE  9(05).                   CI0283
            11            QT58-ALPLDT PICTURE  9(8).                    CI0283
            11            QT58-ANFMC  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-CPNOP  PICTURE  X(2).                    CI0283
            11            QT58-CVSTC  PICTURE  X(4).                    CI0283
            11            QT58-CGMBR  PICTURE  X.                       CI0283
            11            QT58-DWSDT  PICTURE  9(8).                    CI0283
            11            QT58-IRDPH  PICTURE  X.                       CI0283
            11            QT58-DWAIT  PICTURE  9(8).                    CI0283
            11            QT58-IAPGP  PICTURE  X.                       CI0283
            11            QT58-CASTA  PICTURE  X.                       CI0283
            11            QT58-CSSUP2 PICTURE  X.                       CI0283
            11            QT58-CVOMC1 PICTURE  X(1).                    CI0283
            11            QT58-APGBP  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ALDDUE PICTURE  9(08).                   CI0283
            11            QT58-APYMT  PICTURE  S9(9)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ALSURR PICTURE  S9(09)V99                CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-CESTP  PICTURE  X(03).                   CI0283
            11            QT58-FILLER PICTURE  X(356).                  CI0283
            10            QT58-QT5O                                     CI0283
                          REDEFINES            QT58-FILLER.             CI0283
            11            QT58-NBACT  PICTURE  S9(11)                   CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-CTIAC  PICTURE  S9(3)                    CI0283
                          BINARY.                                       CI0283
            11            QT58-CASTT  PICTURE  S99                      CI0283
                          BINARY.                                       CI0283
            11            QT58-CATMI  PICTURE  S9                       CI0283
                          BINARY.                                       CI0283
            11            QT58-IATMR  PICTURE  X(3).                    CI0283
            11            QT58-IBIPI  PICTURE  X.                       CI0283
            11            QT58-CBPST  PICTURE  S99                      CI0283
                          BINARY.                                       CI0283
            11            QT58-TBPST  PICTURE  X(16).                   CI0283
            11            QT58-CODPI  PICTURE  X.                       CI0283
            11            QT58-TODPS  PICTURE  X(9).                    CI0283
            11            QT58-FILLER PICTURE  X(448).                  CI0283
            11            QT58-IBPSD  PICTURE  X.                       CI0283
            11            QT58-FILLER PICTURE  X(107).                  CI0283
            11            QT58-QT5E                                     CI0283
                          REDEFINES            QT58-FILLER.             CI0283
            12            QT58-MPRN4X PICTURE  X(100).                  CI0283
            12            QT58-CCMSH  PICTURE  X(2).                    CI0283
            12            QT58-CPRCS  PICTURE  X(04).                   CI0283
            12            QT58-CURST  PICTURE  X.                       CI0283
            10            QT58-QT5M                                     CI0283
                          REDEFINES            QT58-FILLER.             CI0283
            11            QT58-NAPCN1 PICTURE  X(24).                   CI0283
            11            QT58-FILLER PICTURE  X(576).                  CI0283
            10            QT58-QT5B                                     CI0283
                          REDEFINES            QT58-FILLER.             CI0283
            11            QT58-NAPCN2 PICTURE  X(24).                   CI0283
            11            QT58-CTIDAL PICTURE  X(40).                   CI0283
            11            QT58-NPHNS  PICTURE  X(14).                   CI0283
            11            QT58-FILLER PICTURE  X(522).                  CI0283
            10            QT58-QT5P                                     CI0283
                          REDEFINES            QT58-FILLER.             CI0283
            11            QT58-CFPPT  PICTURE  9(3).                    CI0283
            11            QT58-TTYPP  PICTURE  X(40).                   CI0283
            11            QT58-CPPST  PICTURE  9(3).                    CI0283
            11            QT58-TPPST  PICTURE  X(15).                   CI0283
            11            QT58-APFEEQ PICTURE  S9(7)V99.                CI0283
            11            QT58-APFEEC PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-APFEEP PICTURE  S9(7)V99.                CI0283
            11            QT58-ISVCA  PICTURE  X.                       CI0283
            11            QT58-NSBVS  PICTURE  X(5).                    CI0283
            11            QT58-ICKRV  PICTURE  X.                       CI0283
            11            QT58-PDAMT  PICTURE  S9(03).                  CI0283
            11            QT58-PSTAX  PICTURE  S9(03)V999.              CI0283
            11            QT58-DPCAL  PICTURE  9(8).                    CI0283
            11            QT58-NADVF  PICTURE  X(08).                   CI0283
            11            QT58-DAGUP  PICTURE  9(8).                    CI0283
            11            QT58-AANFEA PICTURE  9(5)V99                  CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-CLIDN7 PICTURE  9(8)                     CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-ARANV  PICTURE  S9(7)V99                 CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            QT58-DRANV  PICTURE  9(8).                    CI0283
            11            QT58-FILLER PICTURE  X(454).                  CI0283
            10            QT58-QT50                                     CI0283
                          REDEFINES            QT58-FILLER.             CI0283
            11            QT58-NANCA  PICTURE  X(30).                   CI0283
            11            QT58-MANCN  PICTURE  X(100).                  CI0283
            11            QT58-AINPTX PICTURE  X(20).                   CI0283
            11            QT58-CTID01 PICTURE  X(27).                   CI0283
            11            QT58-NANCA1 PICTURE  X(04).                   CI0283
            11            QT58-IIVAR  PICTURE  X(1).                    CI0283
            11            QT58-FILLER PICTURE  X(418).                  CI0283
            10            QT58-QT5R                                     CI0283
                          REDEFINES            QT58-FILLER.             CI0283
            11            QT58-NACTJ  PICTURE  X(04).                   CI0283
            11            QT58-NACNO6 PICTURE  X(11).                   CI0283
            11            QT58-FILLER PICTURE  X(585).                  CI0283
            10            QT58-AMAXA  PICTURE  S9(7)V99.                CI0283
            10            QT58-ISAOR  PICTURE  X.                       CI0283
            10            QT58-ISACH  PICTURE  X.                       CI0283
            10            QT58-CERRBA PICTURE  X(02).                   CI0283
            10            QT58-CERRBH PICTURE  X(02).                   CI0283
            10            QT58-IWITHH PICTURE  X.                       CI0283
            10            QT58-CTID20 PICTURE  X(27).                   CI0283
            10            QT58-GECKD3 PICTURE  9.                       CI0283
            10            QT58-DANFC  PICTURE  X(10).                   CI0283
            10            QT58-DAFCN  PICTURE  X(10).                   CI0283
            10            QT58-ISMTA  PICTURE  X.                       CI0283
            10            QT58-CERRBT PICTURE  X(02).                   CI0283
            10            QT58-NPLNI  PICTURE  X(10).                   CI0283
            10            QT58-FILLER PICTURE  X(023).                  CI0283
      *
      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0283
          05              DE00-SUITE.                                   CI0283
            15       FILLER         PICTURE  X(00653).                  CI0283
       01                 DE10  REDEFINES      DE00.                    CI0283
            10            DE10-DU11.                                    CI0283
            11            DE10-XFONC  PICTURE  X(4).                    CI0283
            11            DE10-MPSBN  PICTURE  X(8).                    CI0283
            11            DE10-XDBDNM PICTURE  X(08).                   CI0283
            11            DE10-XSEGNM PICTURE  X(08).                   CI0283
            11            DE10-XRC    PICTURE  X(02).                   CI0283
            11            DE10-MSEG   PICTURE  X(08).                   CI0283
            11            DE10-XCOKEY PICTURE  X(70).                   CI0283
            11            DE10-CUIBR  PICTURE  X(01).                   CI0283
            11            DE10-CUIBA  PICTURE  X(01).                   CI0283
            11            DE10-IPBIK  PICTURE  X(1).                    CI0283
            10            DE10-DU03.                                    CI0283
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            DE10-CMSSF  PICTURE  XX.                      CI0283
            11            DE10-DU09.                                    CI0283
            12            DE10-CMESA  PICTURE  S9(9)                    CI0283
                          BINARY.                                       CI0283
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0283
                          BINARY.                                       CI0283
            12            DE10-CMESB  PICTURE  S9(9)                    CI0283
                          BINARY.                                       CI0283
            12            DE10-CMSST  PICTURE  S9(9)                    CI0283
                          BINARY.                                       CI0283
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0283
                          BINARY.                                       CI0283
            12            DE10-QELLAA PICTURE  S9(9)                    CI0283
                          BINARY.                                       CI0283
            12            DE10-TMESS4 PICTURE  X(512).                  CI0283
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0283
          05              MS00-SUITE.                                   CI0283
            15       FILLER         PICTURE  X(00542).                  CI0283
       01                 MS03  REDEFINES      MS00.                    CI0283
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            10            MS03-CMSSF  PICTURE  XX.                      CI0283
            10            MS03-DU09.                                    CI0283
            11            MS03-CMESA  PICTURE  S9(9)                    CI0283
                          BINARY.                                       CI0283
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0283
                          BINARY.                                       CI0283
            11            MS03-CMESB  PICTURE  S9(9)                    CI0283
                          BINARY.                                       CI0283
            11            MS03-CMSST  PICTURE  S9(9)                    CI0283
                          BINARY.                                       CI0283
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0283
                          BINARY.                                       CI0283
            11            MS03-QELLAA PICTURE  S9(9)                    CI0283
                          BINARY.                                       CI0283
            11            MS03-TMESS4 PICTURE  X(512).                  CI0283
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0283
            10            MX11-QMSGS  PICTURE  9(03).                   CI0283
            10            MX11-PJ09                                     CI0283
                          OCCURS       025     TIMES.                   CI0283
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0283
                          COMPUTATIONAL-3.                              CI0283
            11            MX11-CMESB  PICTURE  S9(9)                    CI0283
                          BINARY.                                       CI0283
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                K818
                                K943
                                QT58
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0283
      *               *                                   *             CI0283
      *               *INITIALISATIONS                    *             CI0283
      *               *                                   *             CI0283
      *               *************************************.            CI0283
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
           GC04
           GC06.
       F02CA-FN. EXIT.
      *N02SC.    NOTE *SETTING THE POINTERS               *.
       F02SC.                                                           lv10
      *SET ADDRESS FOR ACAP                                             DOT
           SET ADDRESS OF XA06 TO                                       ADU015
                PCB-ACAP-PTR1.                                          ADU015
       F02SC-FN. EXIT.
       F02-FN.   EXIT.
      *N03.      NOTE *************************************.
      *               *                                   *
      *               *INITIALISE MISCELLANEOUS FIELDS    *
      *               *                                   *
      *               *************************************.
       F03.           EXIT.                                             lv05
      *N03DD.    NOTE *DETERMINE ICODE                    *.
       F03DD.                                                           lv10
           MOVE        QT58-PRSCD TO WS01-PRSCD.
       F03DD-FN. EXIT.
       F03-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0283
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0283
      *               *                                   *             CI0283
      *               *FIN DE TRAITEMENT                  *             CI0283
      *               *                                   *             CI0283
      *               *************************************.            CI0283
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0283
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *ACCESS MISC TABLES AND DBASES      *
      *               *                                   *
      *               *************************************.
       F35.           EXIT.                                             lv05
      *N35LF.    NOTE *DETERMINE CVSYS                    *.
       F35LF.                                                           lv10
           MOVE        QT58-CTIDA TO TA5B-CTIDA
           CT01-CTIDA
           MOVE        QT58-PRCODA TO TA5B-PRCOD
           MOVE        SPACES TO TA5B-PRSCD
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
      *N35SD.    NOTE *ACCESS TG04 TABLE TO RETRIEVE      *.
       F35SD.                                                           lv10
      *TRANSACTION FUNCTION CODE
           MOVE        004 TO TG04-CTIDA
           MOVE        QT58-PRCODA TO TG04-PRCOD
           MOVE        'S' TO TG04-CTRTP
           MOVE        K943-CPORTA TO TG04-CPORT
           PERFORM     F92TG THRU F92TG-FN.
      *N35SF.    NOTE *IF CATRF WAS FOUND; MOVE           *.
       F35SF.    IF    TG04-IK = '0'                                    lv15
                 NEXT SENTENCE ELSE GO TO     F35SF-FN.
           MOVE        TG04-CATRF TO GC03-CATRF.
       F35SF-900. GO TO F35SH-FN.
       F35SF-FN. EXIT.
      *N35SH.    NOTE *ERROR TRANSACTION CODE NOT FOUND   *.
       F35SH.                                                           lv15
      *---> Send BAD TG04 READ Message                                  ADU119
      *      and TERMINATE                                              ADU119
           MOVE        000000 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35SH-FN. EXIT.
       F35SD-FN. EXIT.
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
           MOVE        K943-CTID TO GC01-CTID
           MOVE        'N' TO GC01-ICUST
           COMPUTE     GC01-DCAG9L = 99999999 -
           K943-DCACG
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
           MOVE        334 TO GC03-GELL
           MOVE        GC01-DCAG9L TO GC03-DCACG9
           MOVE        GC01-NAASQL TO GC03-NAASQ.
       F45CA-FN. EXIT.
      *N45CG.    NOTE *MOVE CONSTANT FIELDS FOR DISB      *.
       F45CG.                                                           lv10
           MOVE        001 TO GC03-CAATY
           MOVE        014 TO GC03-CACTO
           MOVE        01 TO GC03-CASTC
           MOVE        'Y' TO GC03-IPULL
           MOVE        0001 TO GC03-NBTCH
           MOVE        ZERO TO GC03-DLAUP1
           GC03-ADRET
           GC03-NDRFT
           GC03-QNACT
           GC03-ADBRQ
           GC03-CTWHAT
           GC03-CATRS
           GC03-NSUNT
           MOVE        SPACES TO GC03-TTRMS
           GC03-IDELT
           MOVE        'N' TO GC03-ITRAN
           GC03-IDPAP
           MOVE        'S' TO GC03-CTRTP
           MOVE        'KD20' TO GC03-CSCRNU
           MOVE        'Y' TO GC03-IPLIN
           MOVE        K943-CPORTA TO GC03-CPORT
           MOVE        'N' TO GC03-ISKED
           MOVE        0 TO GC03-CENXC
           MOVE        SPACES TO GC03-GEOPDM
           MOVE        'Y' TO GC03-ITELE.
       F45CG-FN. EXIT.
      *N45DA.    NOTE *MOVE FIELDS FROM INPUT             *.
       F45DA.                                                           lv10
           MOVE        K943-GEAUN TO GC03-GEAUN
           MOVE        K943-GEOPD2 TO GC03-GEOPD2
           MOVE        K943-DEFFT TO GC03-DEFFT
           MOVE        K943-GETIM TO GC03-GETIM
           COMPUTE     GC03-AEDRQ = K943-ADBRQ
           - QT58-ASURRW
           MOVE        QT58-CLIDNB TO GC03-CLIDNB
           MOVE        QT58-DVALU TO GC03-DLAUP
           MOVE        K943-CPORTA TO GC03-CPORT.
                 IF    K943-PWHLDF > 0                                  DOT
           MOVE        K943-PWHLDF TO GC03-PWHLD
           MOVE        'Y' TO GC03-IWTHH
                 ELSE
           MOVE        0 TO GC03-PWHLD
           MOVE        'N' TO GC03-IWTHH.
       F45DA-FN. EXIT.
      *N45FD.    NOTE *LOAD GC03-GD32 LIFE INFORMATION    *.
       F45FD.                                                           lv10
           INITIALIZE  GC03-GD32
           MOVE        'N' TO GC03-IABIN1
           MOVE        0 TO GC03-CLDOD
           MOVE        K943-GETIM TO WS00-GETIM
           MOVE        K943-DCACG (7:2) TO GC03-NCONF (1:2)
           MOVE        WS00-GETIM (2:4) TO GC03-NCONF (3:4)
           MOVE        83 TO GC03-NCONF (7:2)
           MOVE        QT58-ASURRW TO GC03-ASCHGA
           MOVE        0 TO GC03-GEHCD
           GC03-CRATC
           GC03-CARTY
           GC03-NARRS
           GC03-CARTZ
           GC03-NAPDS
           GC03-DNPMT
           GC03-AMVA1
           GC03-NCLAM
           GC03-PWHLDS
           MOVE        SPACES TO GC03-CTSAO
           GC03-CLID
           GC03-CPMTO
           GC03-IPCTV
           GC03-IWTHH1
           GC03-CACTS
           GC03-CTSPP
           GC03-CACT4.
                 IF    TA5B-IVANT = 'Y'                                 DOT
                 AND   K943-CTTYPG = 'SMDD'
           MOVE        'N' TO GC03-IMECH
                 ELSE
           MOVE        'Y' TO GC03-IMECH.
           MOVE        'N' TO GC03-IMVAO                                DOT
           GC03-ISURR
           GC03-ISWHO
      *
      *CACULATE AMAXD = TOTAL ACCT
      *VALUE MINUS THE TOTAL CHARGES
           COMPUTE     GC03-AMAXD = QT58-AACTV -
           QT58-ASURRW
           MOVE        QT58-ANGOF TO GC03-APYOM
           MOVE        'O' TO GC03-CPAYCL.
       F45FD-FN. EXIT.
      *N45FG.    NOTE *LOAD ADBRQ FOR VANTAGE ACCOUNT     *.
       F45FG.    IF    TA5B-IVANT = 'Y'                                 lv10
                 AND   GC03-CTRTP = 'S'
                 AND   (K943-CPORTA = 'M' OR 'F')
                 NEXT SENTENCE ELSE GO TO     F45FG-FN.
           MOVE        GC03-AMAXD TO GC03-ADBRQ.
       F45FG-FN. EXIT.
      *N45GB.    NOTE *EDIT TSA SURRENDERS                *.
       F45GB.    IF    WS01-ICODE = 2 OR 3                              lv10
                 NEXT SENTENCE ELSE GO TO     F45GB-FN.
      *N45GD.    NOTE *DETERMINE CLIENTS AGE              *.
       F45GD.                                                           lv15
           INITIALIZE  DD30 DD33 7-OAGE-PASSED-FIELDS
           MOVE        K943-DEFFT TO DD33-DTGRG
           MOVE        4 TO DD30-CDTSF
           PERFORM     F91AB THRU F91AB-FN.
                 IF    DD30-CDTSC = 0                                   DOT
           MOVE        DD33-DTJUL TO 7-OAGE-CURRENT-DATE.
           INITIALIZE  DD30 DD33                                        DOT
           MOVE        QT58-CLDOB TO DD33-DTGRG
           MOVE        4 TO DD30-CDTSF
           PERFORM     F91AB THRU F91AB-FN.
                 IF    DD30-CDTSC = 0                                   DOT
           MOVE        DD33-DTJUL TO 7-OAGE-BIRTH-DATE.
                 IF    7-OAGE-CURRENT-DATE > ZERO                       DOT
                 AND   7-OAGE-BIRTH-DATE > ZERO
           PERFORM     F91BB THRU F91BB-FN.
       F45GD-FN. EXIT.
      *N45GG.    NOTE *SET TSA SURRENDER OVERRIDE CODE    *.
       F45GG.    IF    (K943-ADBRQ > K943-ATSA8                         lv15
                 AND   K943-CPORTA = 'F')
                 OR    (GC03-AMAXD > K943-ATSA8
                 AND   K943-CPORTA = 'F')
                 NEXT SENTENCE ELSE GO TO     F45GG-FN.
      *SET TO T IF CLIENT OLDER > 59.5
                 IF    7-OAGE-CLIENT-AGE > 59.5                         DOT
           MOVE        'T' TO GC03-CTSAO
                 ELSE
           MOVE        SPACE TO GC03-CTSAO.
       F45GG-FN. EXIT.
       F45GB-900. GO TO F45GJ-FN.
       F45GB-FN. EXIT.
      *N45GJ.    NOTE *INITIALISE CTSAO WITH SPACE        *.
       F45GJ.                                                           lv10
           MOVE        SPACE TO GC03-CTSAO.
       F45GJ-FN. EXIT.
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
      *               *BUILD AND WRITE GC04               *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50DD.    NOTE *ONLY DO GC04 BUILD AND WRITE       *.
       F50DD.    IF    WS01-ICODE = 2 OR 3 OR 5                         lv10
                 OR    QT58-CQACT = 1 OR 2 OR 3
                 NEXT SENTENCE ELSE GO TO     F50DD-FN.
      *IF ICODE OR CQACT CORRRECT VALUE
      *IE ANNUITY ACCOUNTS THAT ARE
      *PUBLIC SHOOLS 403B,
      *501(C)(3) EMPLOYEES(NON PROFITS)
      *IRA OR SEP
      *N50EE.    NOTE *BUILD GC04                         *.
       F50EE.                                                           lv15
           MOVE        QT58-CLCUS TO GC04-CLCUS
           MOVE        QT58-CCACT TO GC04-CCACT
           MOVE        0 TO GC04-AFEET
           MOVE        'N' TO GC04-ITERF
           MOVE        0 TO GC04-ATERF
           MOVE        QT58-CLDOB TO GC04-CLDOB
           MOVE        QT58-CPLTYP TO GC04-CPLTYP
           MOVE        'N' TO GC04-IACFPD.
       F50EE-FN. EXIT.
      *N50ID.    NOTE *INSERT GC04                        *.
       F50ID.                                                           lv15
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           MOVE        GC03-GC03K TO S-GCU03-GC03K
           PERFORM     F94I4 THRU F94I4-FN.
      *N50IG.    NOTE *IF ALREADY EXISTS; SET UP SEQ      *.
       F50IG.    IF    XW05-XRC = 'II'                                  lv20
                 NEXT SENTENCE ELSE GO TO     F50IG-FN.
      *---> Send BAD GC04 WRITE Message                                 ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013200 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50IG-FN. EXIT.
       F50ID-FN. EXIT.
       F50DD-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *BUILD GC06                         *
      *               *                                   *
      *               *************************************.
       F55.           EXIT.                                             lv05
      *N55DA.    NOTE *SEND MONEY                         *.
       F55DA.                                                           lv10
           INITIALIZE  GC06
           INITIALIZE  GC06-GE10
           MOVE        64 TO GC06-GELL
           ADD         1 TO GC06-NPISQ.
                 IF    K943-CPORTA = 'F'                                DOT
           MOVE        0 TO GC06-ACOTD
           MOVE        100 TO GC06-PPOTD.
           MOVE        0 TO GC06-QPSTD                                  DOT
           MOVE        K943-CDELI TO GC06-CDELI
           MOVE        01 TO GC06-CPITC
           MOVE        'Y' TO GC06-ICHKP
           MOVE        SPACE TO GC06-ITRNB
           MOVE        QT58-CLTIN TO GC06-CLTIN
           MOVE        K943-CPAYC TO GC06-CPAYC.
       F55DA-FN. EXIT.
      *N55DE.    NOTE *BANK DIRECT DEPOSIT                *.
       F55DE.    IF    K943-CDELI = 006                                 lv10
                 NEXT SENTENCE ELSE GO TO     F55DE-FN.
           INITIALIZE  GC06-GE01
           MOVE        276 TO GC06-GELL
           MOVE        K943-GENAL1 TO GC06-GENAL1 (1)
           MOVE        K943-GENAL2 TO GC06-GENAL1 (2)
           MOVE        K943-GESAD1 TO GC06-GESAD1 (1)
           MOVE        K943-GESAD2 TO GC06-GESAD1 (2)
           MOVE        K943-GESAD3 TO GC06-GESAD1 (3)
           MOVE        K943-NTR TO GC06-NTR
           MOVE        K943-GECKD TO GC06-GECKD
           MOVE        K943-NPBN TO GC06-NPBN
           MOVE        K943-CCBAT TO GC06-CCBAT
           MOVE        K943-CLID4 TO GC06-CLID4
           MOVE        'N' TO GC06-ICHKP.
       F55DE-FN. EXIT.
      *N55DG.    NOTE *LOAD AMOUNT FIELDS FOR VANTAGE     *.
       F55DG.    IF    TA5B-IVANT = 'Y'                                 lv10
                 AND   GC03-CTRTP = 'S'
                 AND   (K943-CPORTA = 'M' OR 'F')
                 NEXT SENTENCE ELSE GO TO     F55DG-FN.
           MOVE        GC03-AMAXD TO GC06-ACOTD
           MOVE        0 TO GC06-PPOTD.
       F55DG-FN. EXIT.
      *N55GD.    NOTE *WRITE GC06                         *.
       F55GD.                                                           lv10
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           MOVE        GC03-GC03K TO S-GCU03-GC03K
           PERFORM     F94I6 THRU F94I6-FN.
       F55GD-FN. EXIT.
       F55-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               * BUILD GC29                        *
      *               *                                   *
      *               *************************************.
       F60.           EXIT.                                             lv05
      *N60CD.    NOTE *GC29 - ORDER TICKET KEYS           *.
       F60CD.                                                           lv10
           INITIALIZE  GC29
           MOVE        GC01-GC01K TO S-GCU01-GC01K
           MOVE        GC03-GC03K TO S-GCU03-GC03K
           MOVE        K818 TO GC29
           PERFORM     F94I8 THRU F94I8-FN.
       F60CD-FN. EXIT.
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
      *               *MISCELLANEOUS MACROS               *
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
      *request code DD30-CDTSF:                                         AADA81
      *  1 = greg to julian conversion                                  AADA81
      *      (without greg validation)                                  AADA81
      *  2 = julian to greg conversion                                  AADA81
      *  3 = gregorian validation                                       AADA81
      *  4 = greg to julian conversion                                  AADA81
      *      (with greg validation)                                     AADA81
      ** * * * * * * * * * * * * * * *                                  AADA81
      *Check return code DD30-CDTSC                                     AADA81
      *after the call.                                                  AADA81
      *    0 = Error Free                                               AADA81
      *    3 = Invalid Date                                             AADA81
      *    5 = Invalid Day                                              AADA81
      *    6 = Invalid Month                                            AADA81
      ** * * * * * * * * * * * * * * *                                  AADA81
           MOVE        3 TO DD30-CDTFN                                  AADA81
           CALL        MWS100EX USING DD30                              AADA81
           DD33.                                                        AADA81
       F91AB-FN. EXIT.
      *N91BB.    NOTE *MACRO AAOAG3  -  CALC CLIENT AGE   *.            AAOAG3
       F91BB.                                                           lv10
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
               GO TO     F91BB-FN.                                      AAOAG3
                 IF    7-OAGE-BIRTH-DATE >                              DOT
                       7-OAGE-CURRENT-DATE                              AAOAG3
      *RETURN CLIENT AGE OF ZERO IF                                     AAOAG3
      *BIRTH DATE > CURRENT DATE                                        AAOAG3
               GO TO     F91BB-FN.                                      AAOAG3
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
       F91BB-FN. EXIT.
       F91-FN.   EXIT.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *PACBASE TABLE ACCESS ROUTINES      *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92EB.    NOTE *ERROR ON TABLE READ FOR TB5B       *.
       F92EB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012617 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F92EB-FN. EXIT.
      *N92ED.    NOTE *ERROR ON TABLE READ FOR TB5B       *.
       F92ED.                                                           lv10
           MOVE        '1' TO TG04-IK.
       F92ED-FN. EXIT.
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
      *N92TG.    NOTE *RANDOM TABLE READ FOR TG04         *.            ADUTAB
       F92TG.                                                           lv10
           MOVE        'R1' TO G-TG04-TABFO                             ADUTAB
           COMPUTE     G-TG04-LTH = 60 + G-TG04-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TG04-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TG04)                                ADUTAB
                       LENGTH (G-TG04-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TG04-TABCR NOT = '00'                          DOT
           PERFORM     F92ED THRU F92ED-FN.                             ADUTAB
       F92TG-FN. EXIT.
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
      *               *DATABASE ACCESSES                  *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
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
      *N94I4.    NOTE *CALL ISRT ON GC04                  *.            ADU026
       F94I4.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC04' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XA06 GC04                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           S-GC04-SSA
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94I4-FN. EXIT.
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
      *N94I8.    NOTE *CALL ISRT ON GC29                  *.            ADU026
       F94I8.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC29' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XA06 GC29                                                    ADU026
           S-GCU01-SSA S-GCU03-SSA                                      ADU026
           S-GC29-SSA
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
