       IDENTIFICATION DIVISION.                                         CI0328
       PROGRAM-ID.  CI0328P.                                            CI0328
      *AUTHOR.         SPO TRANSACTION MESSAGES.                        CI0328
      *DATE-COMPILED.   09/08/14.                                       CI0328
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
       ENVIRONMENT DIVISION.                                            CI0328
       CONFIGURATION SECTION.                                           CI0328
       SOURCE-COMPUTER. IBM-370.                                        CI0328
       OBJECT-COMPUTER. IBM-370.                                        CI0328
       DATA DIVISION.                                                   CI0328
       WORKING-STORAGE SECTION.                                         CI0328
      *                                                                 AM0205
      ******************************************************************AM0205
      ** WORKING STORAGE FOR CI0205                                    *AM0205
      ******************************************************************AM0205
      *                                                                 AM0205
      ******************************************************************AM0205
      ** PCB ADDRESS LIST FOR CI0205.  MODULE CI0205 WILL NEED         *AM0205
      ** PCB'S FOR:                                                     AM0205
      **             CLIENT DATABASE         (CL1P)                     AM0205
      **             CONTRACT DATABASE       (CT1P)                     AM0205
      **             ARRANGEMENT DB          (AR1P)                     AM0205
      **             CX6Y   DATABASE         (AREY)                     AM0205
      **             CX2Y   DATABASE         (ARAY)                     AM0205
      **             GROUP  DATABASE         (GR1P)                     AM0205
      **             SHARK PROD/SUBPROD      (SSPP)                     AM0205
      **             CERTIFICATE DB          (CA1P)                     AM0205
      **             MISC TRAN DB            (TR1P)                     AM0205
      **             ARRANGEMENT DB          (AR2P)                     AM0205
      **             LIFE DB                 (LM1P)                     AM0205
      **             LIFE COST DB            (LUVP)                     AM0205
      **             ACTUAL RATES DB         (LARP)                     AM0205
      **             LIFE PLAN DESC SEG DB   (LPDP)                     AM0205
       01   CI0205-CB-PCB-ADDR-LIST.                                    AM0205
            05  CI0205-CB-PCB-CL1P-PTR1       POINTER.                  AM0205
            05  CI0205-CB-PCB-CT1P-PTR1       POINTER.                  AM0205
            05  CI0205-CB-PCB-AR1P-PTR1       POINTER.                  AM0205
            05  CI0205-CB-PCB-AREY-PTR1       POINTER.                  AM0205
            05  CI0205-CB-PCB-ARAY-PTR1       POINTER.                  AM0205
            05  CI0205-CB-PCB-GR1P-PTR1       POINTER.                  AM0205
            05  CI0205-CB-PCB-SSPP-PTR1       POINTER.                  AM0205
            05  CI0205-CB-PCB-CA1P-PTR1       POINTER.                  AM0205
            05  CI0205-CB-PCB-TR1P-PTR1       POINTER.                  AM0205
            05  CI0205-CB-PCB-AR2P-PTR1       POINTER.                  AM0205
            05  CI0205-CB-PCB-LH1P-PTR1       POINTER.                  AM0205
            05  CI0205-CB-PCB-LM1P-PTR1       POINTER.                  AM0205
            05  CI0205-CB-PCB-LUVP-PTR1       POINTER.                  AM0205
            05  CI0205-CB-PCB-LARP-PTR1       POINTER.                  AM0205
            05  CI0205-CB-PCB-LPDP-PTR1       POINTER.                  AM0205
      ******************************************************************
      **  WORK AREA FOR CI0205 CALL                                    *
      ******************************************************************
      *
      *!WF DSP=CB DSL=V1 SEL=01=00 FOR=I LEV=1 PLT=CB
       01                 CB00.                                         CI0328
            10            CB00-CTID   PICTURE  X(27).                   CI0328
            10            CB00-CTSTA  PICTURE  99.                      CI0328
            10            CB00-QIMAX  PICTURE  9(3).                    CI0328
            10            CB00-QITEM  PICTURE  9(3).                    CI0328
            10            CB00-QT8L                                     CI0328
                          OCCURS       099     TIMES.                   CI0328
            11            CB00-CARTZA PICTURE  XX.                      CI0328
            11            CB00-TARTY  PICTURE  X(25).                   CI0328
            11            CB00-DBKEYS.                                  CI0328
            12            CB00-CLID   PICTURE  X(23).                   CI0328
            12            CB00-GEMDA  PICTURE  9(8).                    CI0328
            12            CB00-NSEQ4B PICTURE  9(8)                     CI0328
                          BINARY.                                       CI0328
            12            CB00-CARTYA PICTURE  XX.                      CI0328
            12            CB00-NARRS  PICTURE  S9(3)                    CI0328
                          COMPUTATIONAL-3.                              CI0328
            12            CB00-CTID01 PICTURE  X(27).                   CI0328
            12            CB00-CPMTC  PICTURE  99.                      CI0328
            12            CB00-MPMTT  PICTURE  X(20).                   CI0328
            12            CB00-NAPDS  PICTURE  S9(3)                    CI0328
                          COMPUTATIONAL-3.                              CI0328
            12            CB00-GESTD  PICTURE  9(8).                    CI0328
            12            CB00-CARTZ  PICTURE  99.                      CI0328
            12            CB00-CARST  PICTURE  99.                      CI0328
            12            CB00-NPISQ  PICTURE  S9(3)                    CI0328
                          COMPUTATIONAL-3.                              CI0328
            12            CB00-NPAIS  PICTURE  S9(3)                    CI0328
                          COMPUTATIONAL-3.                              CI0328
            12            CB00-GESQ2  PICTURE  99.                      CI0328
            11            CB00-FRFLDS.                                  CI0328
            12            CB00-CTID02 PICTURE  X(27).                   CI0328
            12            CB00-GECKD  PICTURE  9.                       CI0328
            12            CB00-CTIDA  PICTURE  9(3).                    CI0328
            12            CB00-MRPSN  PICTURE  X(12).                   CI0328
            12            CB00-CLTYP  PICTURE  X.                       CI0328
            12            CB00-MFDNM4 PICTURE  X(40).                   CI0328
            12            CB00-CTTLN1 PICTURE  X(30).                   CI0328
            12            CB00-CTTLN2 PICTURE  X(30).                   CI0328
            12            CB00-CTTLN3 PICTURE  X(30).                   CI0328
            12            CB00-CTTBO1 PICTURE  X(45).                   CI0328
            12            CB00-CTTBO2 PICTURE  X(45).                   CI0328
            12            CB00-CLORN  PICTURE  X(45).                   CI0328
            12            CB00-CLNAMF PICTURE  X(20).                   CI0328
            12            CB00-CLNAMI PICTURE  X.                       CI0328
            12            CB00-CLNAML PICTURE  X(25).                   CI0328
            12            CB00-CLNAMS PICTURE  X(4).                    CI0328
            11            CB00-CDETY  PICTURE  XX.                      CI0328
            11            CB00-TOFLDS.                                  CI0328
            12            CB00-CTID03 PICTURE  X(27).                   CI0328
            12            CB00-CACKD  PICTURE  9.                       CI0328
            12            CB00-CACTID PICTURE  9(3).                    CI0328
            12            CB00-MRPSN3 PICTURE  X(12).                   CI0328
            12            CB00-CLTYP1 PICTURE  X.                       CI0328
            12            CB00-MFDNM5 PICTURE  X(40).                   CI0328
            12            CB00-CATLN1 PICTURE  X(30).                   CI0328
            12            CB00-CATLN2 PICTURE  X(30).                   CI0328
            12            CB00-CATLN3 PICTURE  X(30).                   CI0328
            12            CB00-CATBO1 PICTURE  X(45).                   CI0328
            12            CB00-CATBO2 PICTURE  X(45).                   CI0328
            12            CB00-CLORN1 PICTURE  X(45).                   CI0328
            12            CB00-CLNMF  PICTURE  X(20).                   CI0328
            12            CB00-CLNAM5 PICTURE  X.                       CI0328
            12            CB00-CLNML  PICTURE  X(25).                   CI0328
            12            CB00-CLNMS  PICTURE  X(4).                    CI0328
            11            CB00-CPMTF  PICTURE  99.                      CI0328
            11            CB00-MPMTFL PICTURE  X(24).                   CI0328
            11            CB00-ACOTD  PICTURE  S9(9)V99                 CI0328
                          COMPUTATIONAL-3.                              CI0328
            11            CB00-PPOTD  PICTURE  S9(3)V99                 CI0328
                          COMPUTATIONAL-3.                              CI0328
            11            CB00-QPSTD  PICTURE  S9(7)V999                CI0328
                          COMPUTATIONAL-3.                              CI0328
            11            CB00-QMTH1  PICTURE  9(3).                    CI0328
            11            CB00-MFDNMS PICTURE  X(30).                   CI0328
            11            CB00-CFIDC  PICTURE  X(5).                    CI0328
            11            CB00-CACCT  PICTURE  X.                       CI0328
            11            CB00-GESAD1 PICTURE  X(30).                   CI0328
            11            CB00-GESAD2 PICTURE  X(30).                   CI0328
            11            CB00-GESAD3 PICTURE  X(30).                   CI0328
            11            CB00-GECIT  PICTURE  X(25).                   CI0328
            11            CB00-GEST   PICTURE  X(8).                    CI0328
            11            CB00-GEPCD  PICTURE  X(12).                   CI0328
            11            CB00-NPBN   PICTURE  X(20).                   CI0328
            11            CB00-MCSIG  PICTURE  X(30).                   CI0328
            11            CB00-NTR    PICTURE  9(8).                    CI0328
            11            CB00-CCBAT  PICTURE  99.                      CI0328
            11            CB00-TTBAL  PICTURE  X(15).                   CI0328
            11            CB00-CPCCDE PICTURE  99.                      CI0328
            11            CB00-DNPMT  PICTURE  9(8).                    CI0328
            11            CB00-GEEND  PICTURE  9(8).                    CI0328
            11            CB00-CIRMO  PICTURE  X(12).                   CI0328
            11            CB00-CIRM1  PICTURE  X(12).                   CI0328
            11            CB00-NIRACM PICTURE  9(2).                    CI0328
            11            CB00-CDEST  PICTURE  99.                      CI0328
            11            CB00-TARST  PICTURE  X(10).                   CI0328
            11            CB00-TDESA  PICTURE  X(10).                   CI0328
            11            CB00-NAIDC  PICTURE  9(12).                   CI0328
            11            CB00-DNBPD  PICTURE  9(8).                    CI0328
            11            CB00-DLBPD  PICTURE  9(8).                    CI0328
            11            CB00-TWITH  PICTURE  X(12).                   CI0328
            11            CB00-IACSD1 PICTURE  XXX.                     CI0328
            11            CB00-TDSTR  PICTURE  X(8).                    CI0328
            11            CB00-IINDI1 PICTURE  X(1).                    CI0328
            11            CB00-IINDI2 PICTURE  X(1).                    CI0328
            11            CB00-IINDI3 PICTURE  X(1).                    CI0328
            11            CB00-TDELI  PICTURE  X(30).                   CI0328
            11            CB00-DDSHPA PICTURE  9(8).                    CI0328
            11            CB00-NDRFTF PICTURE  9(5).                    CI0328
            11            CB00-QDIPBK PICTURE  9(3).                    CI0328
            11            CB00-NDRFTL PICTURE  9(5).                    CI0328
            11            CB00-CLID4  PICTURE  X(23).                   CI0328
            11            CB00-TINDI1 PICTURE  X(09).                   CI0328
            11            CB00-TINDI2 PICTURE  X(09).                   CI0328
            11            CB00-TINDI3 PICTURE  X(09).                   CI0328
            11            CB00-TSECD  PICTURE  X(30).                   CI0328
            11            CB00-CTKRAA PICTURE  X(12).                   CI0328
            11            CB00-GCUSPZ PICTURE  X(12).                   CI0328
            11            CB00-CORTY  PICTURE  X.                       CI0328
            11            CB00-ALOIDD PICTURE  9(9)V99                  CI0328
                          COMPUTATIONAL-3.                              CI0328
            11            CB00-DELOI3 PICTURE  9(6).                    CI0328
            11            CB00-INROA  PICTURE  X(1).                    CI0328
            11            CB00-IDRMD  PICTURE  X.                       CI0328
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0205           PIC X(08)  VALUE 'CI0205P '.                AM0205
       01  CI0975           PIC X(08) VALUE 'CI0975P'.                  AM0975
      ******************************************************************AM0975
      *  LINKAGE SEGMENTS FOR CI0975                                    AM0975
      ******************************************************************AM0975
      *                                                                 AM0975
      *!WF DSP=DA DSL=QT SEL=8G FOR=I DES=2 LEV=1                       AM0975
       01                 DA8G.                                         CI0328
            10            DA8G-CFAUL1 PICTURE  X(4)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            DA8G-TFACT1 PICTURE  X(20)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            DA8G-CFAUL2 PICTURE  X(4)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            DA8G-TFACT2 PICTURE  X(20)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            DA8G-CFAUL3 PICTURE  X(4)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            DA8G-TFACT3 PICTURE  X(20)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            DA8G-CARTY  PICTURE  99                       CI0328
                          VALUE                ZERO.                    CI0328
            10            DA8G-CSYS   PICTURE  X(4)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            DA8G-GERTC  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            10            DA8G-CERRE  PICTURE  X(4)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            DA8G-QITEM  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            DA8G-FILLER PICTURE  X(32400)                 CI0328
                          VALUE                SPACE.                   CI0328
            10            DA8G-QT8M                                     CI0328
                          REDEFINES            DA8G-FILLER.             CI0328
            11            DA8G-QT8I                                     CI0328
                          OCCURS       090     TIMES.                   CI0328
            12            DA8G-CTID   PICTURE  X(27).                   CI0328
            12            DA8G-CARST  PICTURE  99.                      CI0328
            12            DA8G-CPMTF  PICTURE  99.                      CI0328
            12            DA8G-CPALL  PICTURE  X(1).                    CI0328
            12            DA8G-PPOTD  PICTURE  S9(3)V99                 CI0328
                          COMPUTATIONAL-3.                              CI0328
            12            DA8G-ACOTD  PICTURE  S9(9)V99                 CI0328
                          COMPUTATIONAL-3.                              CI0328
            12            DA8G-GESTD  PICTURE  9(8).                    CI0328
            12            DA8G-GEEND  PICTURE  9(8).                    CI0328
            12            DA8G-GEMDA  PICTURE  9(8).                    CI0328
            12            DA8G-DNPMT1 PICTURE  9(8).                    CI0328
            12            DA8G-MVSYS  PICTURE  X(6).                    CI0328
            12            DA8G-NAIDC  PICTURE  9(12).                   CI0328
            12            DA8G-CIRMO  PICTURE  X(12).                   CI0328
            12            DA8G-CEBTP  PICTURE  9(2).                    CI0328
            12            DA8G-CLTIN  PICTURE  9(12).                   CI0328
            12            DA8G-CLORN  PICTURE  X(45).                   CI0328
            12            DA8G-GESAD1 PICTURE  X(30).                   CI0328
            12            DA8G-GESAD2 PICTURE  X(30).                   CI0328
            12            DA8G-GESAD3 PICTURE  X(30).                   CI0328
            12            DA8G-GECIT  PICTURE  X(25).                   CI0328
            12            DA8G-GEST   PICTURE  X(8).                    CI0328
            12            DA8G-GEPCD  PICTURE  X(12).                   CI0328
            12            DA8G-CLNMF  PICTURE  X(20).                   CI0328
            12            DA8G-CLNML  PICTURE  X(25).                   CI0328
            12            DA8G-CLTIN1 PICTURE  9(12).                   CI0328
            12            DA8G-FILLER PICTURE  X(05).                   CI0328
            10            DA8G-FILLER PICTURE  X(100)                   CI0328
                          VALUE                SPACE.                   CI0328
       01                 DA8H.                                         CI0328
            10            DA8H-QITEM  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            DA8H-CTID   PICTURE  X(27)                    CI0328
                          OCCURS       030     TIMES                    CI0328
                          VALUE                SPACE.                   CI0328
            10            DA8H-CRQAS  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            10            DA8H-FILLER PICTURE  X(299)                   CI0328
                          VALUE                SPACE.                   CI0328
      *!WF DSP=DA DSL=QT SEL=8H FOR=I DES=2 LEV=1                       AM0975
      ******************************************************            AADA81
      ****      WORK AREAS FOR COMMON DATE UTILITY       ***            AADA81
      ******************************************************            AADA81
      **                                                                AADA81
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA81
      **                                                                AADA81
      **   SEGMENT DD30 - FUNCTION LAYOUT                               AADA81
      **                                                                AADA81
      *!WF DSP=DD DSL=DD SEL=30 FOR=I DES=2 LEV=1                       AADA81
       01                 DD30.                                         CI0328
            10            DD30-CDTFN  PICTURE  9(4)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            DD30-CDTSF  PICTURE  9(4)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            DD30-CDTSC  PICTURE  9(4)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            DD30-FILLER PICTURE  X(40)                    CI0328
                          VALUE                SPACE.                   CI0328
       01                 DD33.                                         CI0328
            10            DD33-DTGRG.                                   CI0328
            11            DD33-DTGCY.                                   CI0328
            12            DD33-DTGCC  PICTURE  9(2)                     CI0328
                          VALUE                ZERO.                    CI0328
            12            DD33-DTGYY  PICTURE  9(2)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            DD33-DTGMM  PICTURE  9(2)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            DD33-DTGDD  PICTURE  9(2)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            DD33-DTJULC.                                  CI0328
            11            DD33-DTJCY.                                   CI0328
            12            DD33-DTJCC  PICTURE  9(2)                     CI0328
                          VALUE                ZERO.                    CI0328
            12            DD33-DTJYY  PICTURE  9(2)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            DD33-DTJDDC PICTURE  S9(3)                    CI0328
                          VALUE                ZERO.                    CI0328
            11            DD33-DTJDD                                    CI0328
                          REDEFINES            DD33-DTJDDC              CI0328
               PICTURE    9(3).                                         CI0328
            10            DD33-DTJUL                                    CI0328
                          REDEFINES            DD33-DTJULC              CI0328
               PICTURE    9(7).                                         CI0328
            10            DD33-DTDYR  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            DD33-DTDMO  PICTURE  9(2)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            DD33-FILLER PICTURE  X(18)                    CI0328
                          VALUE                SPACE.                   CI0328
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
       01                 XW05.                                         CI0328
            10            XW05-XW06.                                    CI0328
            11            XW05-XDBPCB.                                  CI0328
            12            XW05-XDBDNM PICTURE  X(08)                    CI0328
                          VALUE                SPACE.                   CI0328
            12            XW05-XSEGLV PICTURE  X(02)                    CI0328
                          VALUE                SPACE.                   CI0328
            12            XW05-XRC    PICTURE  X(02)                    CI0328
                          VALUE                SPACE.                   CI0328
            12            XW05-XPROPT PICTURE  X(04)                    CI0328
                          VALUE                SPACE.                   CI0328
            12            XW05-FILLER PICTURE  S9(5)                    CI0328
                          VALUE                ZERO                     CI0328
                          BINARY.                                       CI0328
            12            XW05-XSEGNM PICTURE  X(08)                    CI0328
                          VALUE                SPACE.                   CI0328
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0328
                          VALUE                ZERO                     CI0328
                          BINARY.                                       CI0328
            12            XW05-XSEGNB PICTURE  9(05)                    CI0328
                          VALUE                ZERO                     CI0328
                          BINARY.                                       CI0328
            12            XW05-XCOKEY PICTURE  X(70)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            XW05-XW07.                                    CI0328
            11            XW05-XIOPCB.                                  CI0328
            12            XW05-XTERMI PICTURE  X(08)                    CI0328
                          VALUE                SPACE.                   CI0328
            12            XW05-FILLER PICTURE  XX                       CI0328
                          VALUE                SPACE.                   CI0328
            12            XW05-XRC1   PICTURE  X(02)                    CI0328
                          VALUE                SPACE.                   CI0328
            12            XW05-FILLER PICTURE  X(12)                    CI0328
                          VALUE                SPACE.                   CI0328
            12            XW05-XMODNM PICTURE  X(8)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0328
                          VALUE                ZERO.                    CI0328
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0328
                          VALUE                ZERO.                    CI0328
            10            XW05-XGU    PICTURE  X(4)                     CI0328
                          VALUE                'GU  '.                  CI0328
            10            XW05-XGHU   PICTURE  X(4)                     CI0328
                          VALUE                'GHU '.                  CI0328
            10            XW05-XGN    PICTURE  X(4)                     CI0328
                          VALUE                'GN  '.                  CI0328
            10            XW05-XGHN   PICTURE  X(4)                     CI0328
                          VALUE                'GHN '.                  CI0328
            10            XW05-XGNP   PICTURE  X(4)                     CI0328
                          VALUE                'GNP '.                  CI0328
            10            XW05-XGHNP  PICTURE  X(4)                     CI0328
                          VALUE                'GHNP'.                  CI0328
            10            XW05-XREPL  PICTURE  XXXX                     CI0328
                          VALUE                'REPL'.                  CI0328
            10            XW05-XISRT  PICTURE  X(4)                     CI0328
                          VALUE                'ISRT'.                  CI0328
            10            XW05-XDLET  PICTURE  X(4)                     CI0328
                          VALUE                'DLET'.                  CI0328
            10            XW05-XOPEN  PICTURE  X(4)                     CI0328
                          VALUE                'OPEN'.                  CI0328
            10            XW05-XCLSE  PICTURE  X(4)                     CI0328
                          VALUE                'CLSE'.                  CI0328
            10            XW05-XCHKP  PICTURE  X(4)                     CI0328
                          VALUE                'CHKP'.                  CI0328
            10            XW05-XXRST  PICTURE  X(4)                     CI0328
                          VALUE                'XRST'.                  CI0328
            10            XW05-XTERM  PICTURE  X(4)                     CI0328
                          VALUE                'TERM'.                  CI0328
            10            XW05-XNFPAC PICTURE  X(13)                    CI0328
                          VALUE                SPACE.                   CI0328
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0328
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0328
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
      ******************************************************************ADUTAB
      **              TABLE TA5A ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA5A-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=5A FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA5A.                                                CI0328
           04    G-TA5A-PARAM.                                          CI0328
             10  G-TA5A-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0328
                        VALUE      +772.                                CI0328
             10  G-TA5A-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0328
                        VALUE      +001.                                CI0328
             10  G-TA5A-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0328
                        VALUE      +017.                                CI0328
             10  G-TA5A-NUAPP  PICTURE 99                               CI0328
                        VALUE       0.                                  CI0328
             10  G-TA5A-NUTAB  PICTURE X(6)                             CI0328
                        VALUE 'TA005A'.                                 CI0328
             10  G-TA5A-TABFO  PICTURE XX                 VALUE SPACE.  CI0328
             10  G-TA5A-TABCR  PICTURE XX                 VALUE SPACE.  CI0328
             10  G-TA5A-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0328
             10  G-TA5A-NUSSC  PICTURE X  VALUE   ' '.                  CI0328
             10  G-TA5A-NUSSY  PICTURE X                  VALUE SPACE.  CI0328
             10  G-TA5A-TRANID PICTURE X(4)               VALUE SPACE.  CI0328
             10  G-TA5A-FILSYS.                                         CI0328
             15  G-TA5A-USERC  PICTURE X(6)               VALUE SPACE.  CI0328
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0328
           04             TA5A.                                         CI0328
            10            TA5A-GAPSC.                                   CI0328
            11            TA5A-CTIDA  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            TA5A-PRCOD  PICTURE  9(5)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            TA5A-PRSCD  PICTURE  X(9)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-PRCLN  PICTURE  X(60)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-PRCMN  PICTURE  X(20)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-PRCSN  PICTURE  X(9)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-MRCLN1 PICTURE  X(51)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-MRCLN2 PICTURE  X(51)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-MRCLN3 PICTURE  X(51)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-MRCLN4 PICTURE  X(51)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-MRCMN2 PICTURE  X(20)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-MRCMN3 PICTURE  X(20)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-PRCCS1 PICTURE  X(15)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-PRCCS2 PICTURE  X(15)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-PRCCS3 PICTURE  X(15)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-MPCLN  PICTURE  X(45)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-MPCL1  PICTURE  X(45)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-MSP1   PICTURE  X(60)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-MSP5   PICTURE  X(30)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-MSP03  PICTURE  X(3)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-MPRDG  PICTURE  X(20)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-CPRDG  PICTURE  9(2)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            TA5A-MPRDA1 PICTURE  X(50)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-CPRDA1 PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            TA5A-MSP06  PICTURE  X(20)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-CPOIN  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-CPITY  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            TA5A-CLITY  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            TA5A-IVARP  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            10            TA5A-CASCL  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            TA5A-ZDA88  PICTURE  X(88)                    CI0328
                          VALUE                SPACE.                   CI0328
      **                                                                ADUTAB
      ******************************************************************ADUTAB
      **              TABLE TD8A ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TD8A-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TD DSL=TA SEL=8A FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TD8A.                                                CI0328
           04    G-TD8A-PARAM.                                          CI0328
             10  G-TD8A-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0328
                        VALUE      +106.                                CI0328
             10  G-TD8A-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0328
                        VALUE      +001.                                CI0328
             10  G-TD8A-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0328
                        VALUE      +008.                                CI0328
             10  G-TD8A-NUAPP  PICTURE 99                               CI0328
                        VALUE       0.                                  CI0328
             10  G-TD8A-NUTAB  PICTURE X(6)                             CI0328
                        VALUE 'TA008A'.                                 CI0328
             10  G-TD8A-TABFO  PICTURE XX                 VALUE SPACE.  CI0328
             10  G-TD8A-TABCR  PICTURE XX                 VALUE SPACE.  CI0328
             10  G-TD8A-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0328
             10  G-TD8A-NUSSC  PICTURE X  VALUE   ' '.                  CI0328
             10  G-TD8A-NUSSY  PICTURE X                  VALUE SPACE.  CI0328
             10  G-TD8A-TRANID PICTURE X(4)               VALUE SPACE.  CI0328
             10  G-TD8A-FILSYS.                                         CI0328
             15  G-TD8A-USERC  PICTURE X(6)               VALUE SPACE.  CI0328
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0328
           04             TD8A.                                         CI0328
            10            TD8A-GADPR.                                   CI0328
            11            TD8A-CTIDA  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            TD8A-PRCOD  PICTURE  9(5)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            TD8A-CLIAN  PICTURE  9(02)                    CI0328
                          VALUE                ZERO.                    CI0328
            10            TD8A-CLAST  PICTURE  9(03)                    CI0328
                          VALUE                ZERO.                    CI0328
            10            TD8A-ISMTD  PICTURE  X(1)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            TD8A-ISUBA  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            10            TD8A-IVINS  PICTURE  X(1)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            TD8A-IEOIR  PICTURE  X(1)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            TD8A-IDBNL  PICTURE  X(1)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            TD8A-ICHRC  PICTURE  X(1)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            TD8A-ICHPN  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            10            TD8A-IVAPR  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            10            TD8A-ICLSF  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            10            TD8A-IIULA  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            10            TD8A-IINPS  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            10            TD8A-IINLN  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            10            TD8A-CINPS  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            10            TD8A-CINLN  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            10            TD8A-ZDA79  PICTURE  X(79)                    CI0328
                          VALUE                SPACE.                   CI0328
      **                                                                ADUTAB
      ******************************************************************ADUTAB
      **              TABLE TF09 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TF09-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TF DSL=TF SEL=09 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TF09.                                                CI0328
           04    G-TF09-PARAM.                                          CI0328
             10  G-TF09-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0328
                        VALUE      +142.                                CI0328
             10  G-TF09-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0328
                        VALUE      +001.                                CI0328
             10  G-TF09-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0328
                        VALUE      +005.                                CI0328
             10  G-TF09-NUAPP  PICTURE 99                               CI0328
                        VALUE       0.                                  CI0328
             10  G-TF09-NUTAB  PICTURE X(6)                             CI0328
                        VALUE 'TF0009'.                                 CI0328
             10  G-TF09-TABFO  PICTURE XX                 VALUE SPACE.  CI0328
             10  G-TF09-TABCR  PICTURE XX                 VALUE SPACE.  CI0328
             10  G-TF09-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0328
             10  G-TF09-NUSSC  PICTURE X  VALUE   ' '.                  CI0328
             10  G-TF09-NUSSY  PICTURE X                  VALUE SPACE.  CI0328
             10  G-TF09-TRANID PICTURE X(4)               VALUE SPACE.  CI0328
             10  G-TF09-FILSYS.                                         CI0328
             15  G-TF09-USERC  PICTURE X(6)               VALUE SPACE.  CI0328
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0328
           04             TF09.                                         CI0328
            10            TF09-CERRE2 PICTURE  X(5)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            TF09-CERRE3 PICTURE  X(5)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            TF09-TERMT  PICTURE  X(66)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            TF09-TERMT3 PICTURE  X(66)                    CI0328
                          VALUE                SPACE.                   CI0328
      **                                                                ADUTAB
      *-----------------------------------------------------------------AAER85
      *WORKING STORAGE VARIABLES REQUIRED FOR THE BROKER CALL ERROR     AAER85
      *HANDLING MACRO                                                   AAER85
      *-----------------------------------------------------------------AAER85
      *                                                                 AAER85
      *LENGTH OF THE FIXED AREA OF THE RESPONSE FROM DBI5000N           AAER85
      *!WI pl=WA040                                                     AAER85
       01               WS-FIX-GELL    VALUE ZEROS                      AAER85
                        PICTURE 9(4)                                    CI0328
                          BINARY.                                       CI0328
      *                                                                 AAER85
      *LENGTH OF THE VARIABLE AREA OF THE RESPONSE FROM DBI5000N        AAER85
      *!WI pl=WA055                                                     AAER85
       01               WS-VAR-GELL    VALUE ZEROS                      AAER85
                        PICTURE 9(4)                                    CI0328
                          BINARY.                                       CI0328
      *                                                                 AAER85
      *CALCULATED POSITION OF THE ERROR CODE IN THE RESPONSE SECTION    AAER85
      *!WI pl=WA070                                                     AAER85
       01               WS-ERR-GELL    VALUE ZEROS                      AAER85
                        PICTURE 9(4)                                    CI0328
                          BINARY.                                       CI0328
      *                                                                 AAER85
      *TOTAL NUMBER OF ROWS RETURNED IN THE VIEW RESPONSE.              AAER85
      *!WI pl=WA085                                                     AAER85
       01               WS-NRURO       VALUE ZEROS                      AAER85
                        PICTURE 9(3).                                   CI0328
      *                                                                 AAER85
      *ERROR CODE SET BY THE MACRO.                                     AAER85
      *!WI pl=WA100                                                     AAER85
       01               WS00-IERRC       VALUE SPACES                   AAER85
                        PICTURE X.                                      CI0328
      *                                                                 AAER85
      *SEVERITY CODE SET BY THE MACRO.                                  AAER85
      *!WI pl=WA115                                                     AAER85
       01               WS00-CSEVR       VALUE SPACES                   AAER85
                        PICTURE X.                                      CI0328
      *                                                                 AAER85
      *ERROR CODE FOR THE NON CRITICAL ERRORS NEED TO IGNORE.           AAER85
      *!WI pl=WA130                                                     AAER85
       01               WS01-IERRC       VALUE SPACES                   AAER85
                        PICTURE X.                                      CI0328
      *                                                                 AAER85
      *ARRAY FOR STORING THE LIST OF THE ERROR CODES NEED TO IGNORE     AAER85
       01                 WE00-ERROR-TABLE.                             AAER85
         05               WE00            OCCURS 25 TIMES.              AAER85
      *!WI pl=WA155                                                     AAER85
           10             WE00-CERRE1     VALUE SPACES                  AAER85
                        PICTURE X(5).                                   CI0328
      *                                                                 AAER85
      *VARIABLE FOR STORING THE TYPE OF ERROR HANDLING REQUIRED         AAER85
       01               WS00-ERROR-TYPE  PIC X(1)  VALUE SPACES.        AAER85
      *                                                                 AAER85
      *VARIABLE FOR STORING THE ERROR CODE                              AAER85
      *!WI pl=WA185                                                     AAER85
       01               WS00-CERRE1      VALUE SPACES                   AAER85
                        PICTURE X(5).                                   CI0328
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
       01                 SQ1A.                                         CI0328
            10            SQ1A-CSYSI  PICTURE  X(4)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ1A-CFSPN  PICTURE  X(3)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ1A-NRERO  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            SQ1A-FILLER PICTURE  X(30)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ1A-GTD71                                    CI0328
                          OCCURS 0 TO  125     TIMES                    CI0328
                          DEPENDING  ON        SQ1A-NRERO.              CI0328
            11            SQ1A-CTID.                                    CI0328
            12            SQ1A-CTIDA  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            12            SQ1A-CTIDNP PICTURE  X(13)                    CI0328
                          VALUE                SPACE.                   CI0328
            12            SQ1A-CTIDND PICTURE  9(11)                    CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ1A-PRCOD  PICTURE  9(5)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ1A-CPRSCN PICTURE  9(9)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ1A-FILLER PICTURE  X(30)                    CI0328
                          VALUE                SPACE.                   CI0328
       01                 SQ1E.                                         CI0328
            10            SQ1E-CSYSI  PICTURE  X(4)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ1E-CFSPN  PICTURE  X(3)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ1E-NRERO  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            SQ1E-FILLER PICTURE  X(33)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ1E-GTD79                                    CI0328
                          OCCURS 0 TO  150     TIMES                    CI0328
                          DEPENDING  ON        SQ1E-NRERO.              CI0328
            11            SQ1E-CTID.                                    CI0328
            12            SQ1E-CTIDA  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            12            SQ1E-CTIDNP PICTURE  X(13)                    CI0328
                          VALUE                SPACE.                   CI0328
            12            SQ1E-CTIDND PICTURE  9(11)                    CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ1E-PRCOD  PICTURE  9(5)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ1E-CPRSCN PICTURE  9(9)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ1E-DCACG  PICTURE  9(8)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ1E-FILLER PICTURE  X(30)                    CI0328
                          VALUE                SPACE.                   CI0328
       01                 SQ1L.                                         CI0328
            10            SQ1L-NPVERH PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            SQ1L-NPVERC PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            SQ1L-NPVERD PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            SQ1L-NVIEW  PICTURE  X(4)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ1L-NCUSR2 PICTURE  X(12)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ1L-CPRT2  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ1L-MRPIB1 PICTURE  X(08)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ1L-CLOGY  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ1L-QTOUT  PICTURE  9(5)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            SQ1L-GELLP1 PICTURE  S9(5)                    CI0328
                          VALUE                ZERO                     CI0328
                          BINARY.                                       CI0328
            10            SQ1L-GELLP2 PICTURE  S9(5)                    CI0328
                          VALUE                ZERO                     CI0328
                          BINARY.                                       CI0328
            10            SQ1L-CVSIZ  PICTURE  X(1)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ1L-FILLER PICTURE  X(49)                    CI0328
                          VALUE                SPACE.                   CI0328
       01                 SQ2L.                                         CI0328
            10            SQ2L-NPID4  PICTURE  X(4)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ2L-NVIEW  PICTURE  X(4)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ2L-CTRID  PICTURE  X(4)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ2L-CPRGSX PICTURE  X(8)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ2L-COMND  PICTURE  X(8)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ2L-CERRE1 PICTURE  X(5)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ2L-CSEVR1 PICTURE  X(2)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ2L-GERD1.                                   CI0328
            11            SQ2L-FILLER PICTURE  X(3)                     CI0328
                          VALUE                SPACE.                   CI0328
            11            SQ2L-CERRE2 PICTURE  X(5)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ2L-TERMT  PICTURE  X(66)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ2L-GERD2.                                   CI0328
            11            SQ2L-FILLER PICTURE  X(3)                     CI0328
                          VALUE                SPACE.                   CI0328
            11            SQ2L-CERRE3 PICTURE  X(5)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ2L-TER255 PICTURE  X(255)                   CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ2L-FILLER PICTURE  X(50)                    CI0328
                          VALUE                SPACE.                   CI0328
       01                 SQ3L.                                         CI0328
            10            SQ3L-GELLP3 PICTURE  S9(5)                    CI0328
                          VALUE                ZERO                     CI0328
                          BINARY.                                       CI0328
            10            SQ3L-GELLP4 PICTURE  S9(5)                    CI0328
                          VALUE                ZERO                     CI0328
                          BINARY.                                       CI0328
            10            SQ3L-NAREA  PICTURE  9(03)                    CI0328
                          VALUE                ZERO.                    CI0328
            10            SQ3L-GEOPDM PICTURE  X(8)                     CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ3L-CAPIR1 PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            10            SQ3L-FILLER PICTURE  X(50)                    CI0328
                          VALUE                SPACE.                   CI0328
       01                 SQ5A.                                         CI0328
            10            SQ5A-GRFIX.                                   CI0328
            11            SQ5A-CERRE1 PICTURE  X(5)                     CI0328
                          VALUE                SPACE.                   CI0328
            11            SQ5A-NRURO  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5A-FILLER PICTURE  X(32)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ5A-GRVAR                                    CI0328
                          OCCURS 0 TO  125     TIMES                    CI0328
                          DEPENDING  ON        SQ5A-NRURO.              CI0328
            11            SQ5A-CERRE2 PICTURE  X(5)                     CI0328
                          VALUE                SPACE.                   CI0328
            11            SQ5A-CTID.                                    CI0328
            12            SQ5A-CTIDA  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            12            SQ5A-CTIDNP PICTURE  X(13)                    CI0328
                          VALUE                SPACE.                   CI0328
            12            SQ5A-CTIDND PICTURE  9(11)                    CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5A-PRCOD  PICTURE  9(5)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5A-CPRSCN PICTURE  9(9)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5A-ACINEV PICTURE  S9(9)V99                 CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5A-AADDCD PICTURE  S9(7)V99                 CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5A-CELAT1 PICTURE  9(6)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5A-CELATD PICTURE  9(8)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5A-CMGAC  PICTURE  X                        CI0328
                          VALUE                SPACE.                   CI0328
            11            SQ5A-DDVAC  PICTURE  9(8)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5A-QSHES4 PICTURE  S9(10)V999               CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5A-QSHUI  PICTURE  S9(10)V9(3)              CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5A-GRID.                                    CI0328
            12            SQ5A-GRIDC  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            12            SQ5A-GRIDNP PICTURE  99                       CI0328
                          VALUE                ZERO.                    CI0328
            12            SQ5A-GRIDND PICTURE  9(8)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5A-QSHOM2 PICTURE  S9(10)V999               CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5A-NDCUS  PICTURE  X(9)                     CI0328
                          VALUE                SPACE.                   CI0328
            11            SQ5A-CSTKR5 PICTURE  X(5)                     CI0328
                          VALUE                SPACE.                   CI0328
            11            SQ5A-AYTDD  PICTURE  S9(13)V99                CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5A-FILLER PICTURE  X(42)                    CI0328
                          VALUE                SPACE.                   CI0328
       01                 SQ5E.                                         CI0328
            10            SQ5E-GRFIX.                                   CI0328
            11            SQ5E-CERRE1 PICTURE  X(5)                     CI0328
                          VALUE                SPACE.                   CI0328
            11            SQ5E-NRURO  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5E-FILLER PICTURE  X(22)                    CI0328
                          VALUE                SPACE.                   CI0328
            10            SQ5E-GRVAR                                    CI0328
                          OCCURS 0 TO  150     TIMES                    CI0328
                          DEPENDING  ON        SQ5E-NRURO.              CI0328
            11            SQ5E-CERRE2 PICTURE  X(5)                     CI0328
                          VALUE                SPACE.                   CI0328
            11            SQ5E-CTID.                                    CI0328
            12            SQ5E-CTIDA  PICTURE  9(3)                     CI0328
                          VALUE                ZERO.                    CI0328
            12            SQ5E-CTIDNP PICTURE  X(13)                    CI0328
                          VALUE                SPACE.                   CI0328
            12            SQ5E-CTIDND PICTURE  9(11)                    CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5E-PRCOD  PICTURE  9(5)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5E-CPRSCN PICTURE  9(9)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5E-DASOF  PICTURE  9(8)                     CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5E-QSHON  PICTURE  S9(10)V999               CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5E-AADDCD PICTURE  S9(7)V99                 CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5E-APRAP  PICTURE  S9(11)V99                CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5E-AACCD2 PICTURE  S9(11)V99                CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5E-AFAV10 PICTURE  S9(4)V9(3)               CI0328
                          VALUE                ZERO.                    CI0328
            11            SQ5E-FILLER PICTURE  X(30)                    CI0328
                          VALUE                SPACE.                   CI0328
      ***************************************************************** AAER85
      *THIS MACRO INCLUDES THE I/O SEGMENTS FOR 2 VIEWS ONLY. IF THE    AAER85
      *PROGRAM CALLS MORE THAN 2 VIEWS, ADD THEM MANUALLY AFTER LINE 940AAER85
      ***************************************************************** AAER85
      ******************************************************************
      **     WORKING STORAGE MISC FIELDS                               *
      ******************************************************************

       01  W-WORK-MISC.

      *THIS AREA IS USED FOR CALCULATING THE NEW MONEY FOR A FUND
      *(SOURCE ACCOUNT)
      *!WI
           05  WK00-ACNMO
                        PICTURE S9(9)V99                                CI0328
                          COMPUTATIONAL-3.                              CI0328
      *THIS AREA IS USED FOR CALCULATING THE OLD MONEY FOR A FUND
      *(SOURCE ACCOUNT)
      *!WI
           05  WK00-ACOMO
                        PICTURE S9(9)V99                                CI0328
                          COMPUTATIONAL-3.                              CI0328
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

       01  WS00-WORK-FIELDS.

      *!WI
           05  WS00-CTTYPG              VALUE SPACE
                        PICTURE X(04).                                  CI0328
               88  MMTA                 VALUE 'MMTA'.
               88  SMTC                 VALUE 'SMTC'.
               88  SMDD                 VALUE 'SMDD'.


      *!WI
          05  WS00-CTCUS                VALUE ZERO
                        PICTURE 999.                                    CI0328
              88  FROM-NON-QUALIFIED    VALUE ZERO.
              88  FROM-SRA              VALUE 010.
              88  FROM-QUALIFIED        VALUE 002, 006, 008, 010,
                                              011, 012, 013, 014.

      *!WI
          05  WS00-CPRDA1               VALUE ZERO
                        PICTURE 9(3).                                   CI0328
              88  FROM-TAXDEF-CERT      VALUE 107.

      *!WI
          05  WS00-CLCUS                VALUE ZERO
                        PICTURE 99.                                     CI0328
              88  FROM-PRX              VALUE 02.
              88  FROM-DIS              VALUE 03.
              88  FROM-DTH              VALUE 04, 25, 45.
              88  FROM-RET              VALUE 07.
              88  FROM-RTH              VALUE 21.
              88  FROM-RPX              VALUE 23.
              88  FROM-CON              VALUE 32.
              88  FROM-CPX              VALUE 34.
              88  FROM-NON              VALUE 99.

      *!WI
          05  WS00-MSP03                VALUE SPACE
                        PICTURE X(3).                                   CI0328
              88  FROM-CLASSB-FUND      VALUE 'B  ', ' B ', '  B'.
              88  FROM-CLASSC-FUND      VALUE 'C  ', ' C ', '  C'.

      *!WI
          05  WS00-CTIDA1               VALUE ZERO
                        PICTURE 9(3).                                   CI0328
              88  TO-CERTIFICATE        VALUE 001.
              88  TO-FUND               VALUE 002.


      *!WI
          05  WS00-CTCUS2               VALUE ZERO
                        PICTURE 999.                                    CI0328
              88  TO-NON-QUALIFIED      VALUE ZERO.
              88  TO-QUALIFIED          VALUE 002, 006, 008, 010,
                                              011, 012, 013, 014.

          05  WS00-CLIENT-AGE  PIC 999V9 VALUE ZERO.

      *!WI
          05  WS00-APRMN                VALUE ZERO
                        PICTURE S9(11)V99                               CI0328
                          COMPUTATIONAL-3.                              CI0328

      *!WI
          05  WS00-APRMX                VALUE ZERO
                        PICTURE S9(11)V99                               CI0328
                          COMPUTATIONAL-3.                              CI0328

      *!WI
          05  WS00-PRCOD                VALUE ZERO
                        PICTURE 9(5).                                   CI0328

      *!WI
          05  WS00-AACTV                VALUE ZERO
                        PICTURE S9(11)V99                               CI0328
                          COMPUTATIONAL-3.                              CI0328

      *!WI
          05  WS00-CDEST                VALUE ZERO
                        PICTURE 99.                                     CI0328
              88  ACTIVE-ARRANGEMENT    VALUE 01.

      *!WI
          05  WS00-CARTZA               VALUE SPACE
                        PICTURE XX.                                     CI0328

          05  WS00-FP-SW      PIC X     VALUE SPACE.
               88  FUND-PAYOUT           VALUE 'Y'.
           05  WS00-CP-SW      PIC X     VALUE SPACE.
               88  CERTIFICATE-PAYOUT    VALUE 'Y'.
           05  WS00-MC-SW      PIC X     VALUE SPACE.
               88  MONEY-COMING-IN       VALUE 'Y'.
           05  WS00-GB-SW      PIC X     VALUE SPACE.
               88  GROUP-BILLING         VALUE 'Y'.
           05  WS00-MA-SW      PIC X     VALUE SPACE.
               88  MILITARY-ALLOTMENT    VALUE 'Y'.
           05  WS00-AD-SW      PIC X     VALUE SPACE.
               88  DOLLAR-COST-AVERAGING VALUE 'Y'.
           05  WS00-AP-SW      PIC X     VALUE SPACE.
               88  ANN-PARTIAL-SURR-ARR  VALUE 'Y'.

      *!WI
           05  WS00-QITEM                VALUE ZERO
                        PICTURE 9(3).                                   CI0328

      *!WI
           05  WS00-NMESS2               VALUE ZERO
                        PICTURE S9(6)                                   CI0328
                          COMPUTATIONAL-3.                              CI0328

      *!WI
           05  WS00-CACTS                VALUE SPACES
                        PICTURE X.                                      CI0328
               88  VALID-ACTIVITY-TYPE   VALUE 'V' 'S'.
               88  FROM-UPDATE           VALUE 'S'.

      *!WI
           05  WS00-NCUSR2  VALUE SPACES
                        PICTURE X(12).                                  CI0328

      *!WI
           05  WS00-INMRC   VALUE SPACES
                        PICTURE X(01).                                  CI0328

      *!WI
           05  WS00-ACNMO   VALUE ZEROS
                        PICTURE S9(9)V99                                CI0328
                          COMPUTATIONAL-3.                              CI0328


       01                 WS00-INDEX     PIC X(01).


      *WORKING STORAGE FOR CDSC SKIP PROCESSING FLAG
       01      WS00-SKIP       PIC X VALUE SPACES.
      *DATE MANIPULATION
       01      WS00-CURRENT.
         05    WS00-CURR-CCYY PIC 9(4) VALUE ZEROES.
         05    WS00-CURR-MMDD PIC 9(4) VALUE ZEROES.
       01      WS00-BIRTH.
         05    WS00-BIRTH-CCYY PIC 9(4) VALUE ZEROES.
         05    WS00-BIRTH-MMDD PIC 9(4) VALUE ZEROES.

       01   DEBUT-WSS.                                                  CI0328
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0328
            05   IK     PICTURE X.                                      CI0328
       01  CONSTANTES-PAC.                                              CI0328
           05  FILLER  PICTURE X(87)   VALUE                            CI0328
                     '6015 CAT09/08/14CI0328ADMIN   14:35:20CI0328P AMERCI0328
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0328
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0328
           05  NUGNA   PICTURE X(5).                                    CI0328
           05  APPLI   PICTURE X(3).                                    CI0328
           05  DATGN   PICTURE X(8).                                    CI0328
           05  PROGR   PICTURE X(6).                                    CI0328
           05  CODUTI  PICTURE X(8).                                    CI0328
           05  TIMGN   PICTURE X(8).                                    CI0328
           05  PROGE   PICTURE X(8).                                    CI0328
           05  COBASE  PICTURE X(4).                                    CI0328
           05  DATGNC  PICTURE X(10).                                   CI0328
           05  RELEAS  PICTURE X(7).                                    CI0328
           05  DATGE   PICTURE X(10).                                   CI0328
           05  DATSQ   PICTURE X(10).                                   CI0328
       01  DATCE.                                                       CI0328
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0328
         05  DATOR.                                                     CI0328
           10  DATOA  PICTURE XX.                                       CI0328
           10  DATOM  PICTURE XX.                                       CI0328
           10  DATOJ  PICTURE XX.                                       CI0328
       01   VARIABLES-CONDITIONNELLES.                                  CI0328
            05                  FT      PICTURE X VALUE '0'.            CI0328
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0328
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0328
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           IWE00L PICTURE S9(4) VALUE  ZERO.              AAER85
            05           IWE00R PICTURE S9(4) VALUE  ZERO.              AAER85
            05           IWE00M PICTURE S9(4) VALUE +0025.              AAER85
            05           IWS00L PICTURE S9(4) VALUE  ZERO.
            05           IWS00R PICTURE S9(4) VALUE  ZERO.
            05           IWS00M PICTURE S9(4) VALUE +0020.
            05           J96FLR PICTURE S9(4) VALUE  ZERO.              AAER85
       01   ZONES-UTILISATEUR PICTURE X.                                CI0328
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
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AREY                                           ADU015
            05 PCB-AREY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR ARAY                                           ADU015
            05 PCB-ARAY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR GR1P                                           ADU015
            05 PCB-GR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR SSPP                                           ADU015
            05 PCB-SSPP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CA1P                                           ADU015
            05 PCB-CA1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR TR1P                                           ADU015
            05 PCB-TR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LH1P                                           ADU015
            05 PCB-LH1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LM1P                                           ADU015
            05 PCB-LM1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LUVP                                           ADU015
            05 PCB-LUVP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LARP                                           ADU015
            05 PCB-LARP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LPDP                                           ADU015
            05 PCB-LPDP-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0328
          05              PA00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00106).                  CI0328
       01                 PA06  REDEFINES      PA00.                    CI0328
            10            PA06-XDBPCB.                                  CI0328
            11            PA06-XDBDNM PICTURE  X(08).                   CI0328
            11            PA06-XSEGLV PICTURE  X(02).                   CI0328
            11            PA06-XRC    PICTURE  X(02).                   CI0328
            11            PA06-XPROPT PICTURE  X(04).                   CI0328
            11            PA06-FILLER PICTURE  S9(5)                    CI0328
                          BINARY.                                       CI0328
            11            PA06-XSEGNM PICTURE  X(08).                   CI0328
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0328
                          BINARY.                                       CI0328
            11            PA06-XSEGNB PICTURE  9(05)                    CI0328
                          BINARY.                                       CI0328
            11            PA06-XCOKEY PICTURE  X(70).                   CI0328
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0328
          05              PB00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00106).                  CI0328
       01                 PB06  REDEFINES      PB00.                    CI0328
            10            PB06-XDBPCB.                                  CI0328
            11            PB06-XDBDNM PICTURE  X(08).                   CI0328
            11            PB06-XSEGLV PICTURE  X(02).                   CI0328
            11            PB06-XRC    PICTURE  X(02).                   CI0328
            11            PB06-XPROPT PICTURE  X(04).                   CI0328
            11            PB06-FILLER PICTURE  S9(5)                    CI0328
                          BINARY.                                       CI0328
            11            PB06-XSEGNM PICTURE  X(08).                   CI0328
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0328
                          BINARY.                                       CI0328
            11            PB06-XSEGNB PICTURE  9(05)                    CI0328
                          BINARY.                                       CI0328
            11            PB06-XCOKEY PICTURE  X(70).                   CI0328
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0328
          05              PC00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00106).                  CI0328
       01                 PC06  REDEFINES      PC00.                    CI0328
            10            PC06-XDBPCB.                                  CI0328
            11            PC06-XDBDNM PICTURE  X(08).                   CI0328
            11            PC06-XSEGLV PICTURE  X(02).                   CI0328
            11            PC06-XRC    PICTURE  X(02).                   CI0328
            11            PC06-XPROPT PICTURE  X(04).                   CI0328
            11            PC06-FILLER PICTURE  S9(5)                    CI0328
                          BINARY.                                       CI0328
            11            PC06-XSEGNM PICTURE  X(08).                   CI0328
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0328
                          BINARY.                                       CI0328
            11            PC06-XSEGNB PICTURE  9(05)                    CI0328
                          BINARY.                                       CI0328
            11            PC06-XCOKEY PICTURE  X(70).                   CI0328
      *** PCB MASK FOR AREY                                             ADU015
      *!WF DSP=PD DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PD00.                                         CI0328
          05              PD00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00106).                  CI0328
       01                 PD06  REDEFINES      PD00.                    CI0328
            10            PD06-XDBPCB.                                  CI0328
            11            PD06-XDBDNM PICTURE  X(08).                   CI0328
            11            PD06-XSEGLV PICTURE  X(02).                   CI0328
            11            PD06-XRC    PICTURE  X(02).                   CI0328
            11            PD06-XPROPT PICTURE  X(04).                   CI0328
            11            PD06-FILLER PICTURE  S9(5)                    CI0328
                          BINARY.                                       CI0328
            11            PD06-XSEGNM PICTURE  X(08).                   CI0328
            11            PD06-XKEYLN PICTURE  S9(05)                   CI0328
                          BINARY.                                       CI0328
            11            PD06-XSEGNB PICTURE  9(05)                    CI0328
                          BINARY.                                       CI0328
            11            PD06-XCOKEY PICTURE  X(70).                   CI0328
      *** PCB MASK FOR ARAY                                             ADU015
      *!WF DSP=PE DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PE00.                                         CI0328
          05              PE00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00106).                  CI0328
       01                 PE06  REDEFINES      PE00.                    CI0328
            10            PE06-XDBPCB.                                  CI0328
            11            PE06-XDBDNM PICTURE  X(08).                   CI0328
            11            PE06-XSEGLV PICTURE  X(02).                   CI0328
            11            PE06-XRC    PICTURE  X(02).                   CI0328
            11            PE06-XPROPT PICTURE  X(04).                   CI0328
            11            PE06-FILLER PICTURE  S9(5)                    CI0328
                          BINARY.                                       CI0328
            11            PE06-XSEGNM PICTURE  X(08).                   CI0328
            11            PE06-XKEYLN PICTURE  S9(05)                   CI0328
                          BINARY.                                       CI0328
            11            PE06-XSEGNB PICTURE  9(05)                    CI0328
                          BINARY.                                       CI0328
            11            PE06-XCOKEY PICTURE  X(70).                   CI0328
      *** PCB MASK FOR GR1P                                             ADU015
      *!WF DSP=PF DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PF00.                                         CI0328
          05              PF00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00106).                  CI0328
       01                 PF06  REDEFINES      PF00.                    CI0328
            10            PF06-XDBPCB.                                  CI0328
            11            PF06-XDBDNM PICTURE  X(08).                   CI0328
            11            PF06-XSEGLV PICTURE  X(02).                   CI0328
            11            PF06-XRC    PICTURE  X(02).                   CI0328
            11            PF06-XPROPT PICTURE  X(04).                   CI0328
            11            PF06-FILLER PICTURE  S9(5)                    CI0328
                          BINARY.                                       CI0328
            11            PF06-XSEGNM PICTURE  X(08).                   CI0328
            11            PF06-XKEYLN PICTURE  S9(05)                   CI0328
                          BINARY.                                       CI0328
            11            PF06-XSEGNB PICTURE  9(05)                    CI0328
                          BINARY.                                       CI0328
            11            PF06-XCOKEY PICTURE  X(70).                   CI0328
      *** PCB MASK FOR SSPP                                             ADU015
      *!WF DSP=PG DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PG00.                                         CI0328
          05              PG00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00106).                  CI0328
       01                 PG06  REDEFINES      PG00.                    CI0328
            10            PG06-XDBPCB.                                  CI0328
            11            PG06-XDBDNM PICTURE  X(08).                   CI0328
            11            PG06-XSEGLV PICTURE  X(02).                   CI0328
            11            PG06-XRC    PICTURE  X(02).                   CI0328
            11            PG06-XPROPT PICTURE  X(04).                   CI0328
            11            PG06-FILLER PICTURE  S9(5)                    CI0328
                          BINARY.                                       CI0328
            11            PG06-XSEGNM PICTURE  X(08).                   CI0328
            11            PG06-XKEYLN PICTURE  S9(05)                   CI0328
                          BINARY.                                       CI0328
            11            PG06-XSEGNB PICTURE  9(05)                    CI0328
                          BINARY.                                       CI0328
            11            PG06-XCOKEY PICTURE  X(70).                   CI0328
      *** PCB MASK FOR CA1P                                             ADU015
      *!WF DSP=PH DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PH00.                                         CI0328
          05              PH00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00106).                  CI0328
       01                 PH06  REDEFINES      PH00.                    CI0328
            10            PH06-XDBPCB.                                  CI0328
            11            PH06-XDBDNM PICTURE  X(08).                   CI0328
            11            PH06-XSEGLV PICTURE  X(02).                   CI0328
            11            PH06-XRC    PICTURE  X(02).                   CI0328
            11            PH06-XPROPT PICTURE  X(04).                   CI0328
            11            PH06-FILLER PICTURE  S9(5)                    CI0328
                          BINARY.                                       CI0328
            11            PH06-XSEGNM PICTURE  X(08).                   CI0328
            11            PH06-XKEYLN PICTURE  S9(05)                   CI0328
                          BINARY.                                       CI0328
            11            PH06-XSEGNB PICTURE  9(05)                    CI0328
                          BINARY.                                       CI0328
            11            PH06-XCOKEY PICTURE  X(70).                   CI0328
      *** PCB MASK FOR TR1P                                             ADU015
      *!WF DSP=PI DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PI00.                                         CI0328
          05              PI00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00106).                  CI0328
       01                 PI06  REDEFINES      PI00.                    CI0328
            10            PI06-XDBPCB.                                  CI0328
            11            PI06-XDBDNM PICTURE  X(08).                   CI0328
            11            PI06-XSEGLV PICTURE  X(02).                   CI0328
            11            PI06-XRC    PICTURE  X(02).                   CI0328
            11            PI06-XPROPT PICTURE  X(04).                   CI0328
            11            PI06-FILLER PICTURE  S9(5)                    CI0328
                          BINARY.                                       CI0328
            11            PI06-XSEGNM PICTURE  X(08).                   CI0328
            11            PI06-XKEYLN PICTURE  S9(05)                   CI0328
                          BINARY.                                       CI0328
            11            PI06-XSEGNB PICTURE  9(05)                    CI0328
                          BINARY.                                       CI0328
            11            PI06-XCOKEY PICTURE  X(70).                   CI0328
      *** PCB MASK FOR LH1P                                             ADU015
      *!WF DSP=PK DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PK00.                                         CI0328
          05              PK00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00106).                  CI0328
       01                 PK06  REDEFINES      PK00.                    CI0328
            10            PK06-XDBPCB.                                  CI0328
            11            PK06-XDBDNM PICTURE  X(08).                   CI0328
            11            PK06-XSEGLV PICTURE  X(02).                   CI0328
            11            PK06-XRC    PICTURE  X(02).                   CI0328
            11            PK06-XPROPT PICTURE  X(04).                   CI0328
            11            PK06-FILLER PICTURE  S9(5)                    CI0328
                          BINARY.                                       CI0328
            11            PK06-XSEGNM PICTURE  X(08).                   CI0328
            11            PK06-XKEYLN PICTURE  S9(05)                   CI0328
                          BINARY.                                       CI0328
            11            PK06-XSEGNB PICTURE  9(05)                    CI0328
                          BINARY.                                       CI0328
            11            PK06-XCOKEY PICTURE  X(70).                   CI0328
      *** PCB MASK FOR LM1P                                             ADU015
      *!WF DSP=PL DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PL00.                                         CI0328
          05              PL00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00106).                  CI0328
       01                 PL06  REDEFINES      PL00.                    CI0328
            10            PL06-XDBPCB.                                  CI0328
            11            PL06-XDBDNM PICTURE  X(08).                   CI0328
            11            PL06-XSEGLV PICTURE  X(02).                   CI0328
            11            PL06-XRC    PICTURE  X(02).                   CI0328
            11            PL06-XPROPT PICTURE  X(04).                   CI0328
            11            PL06-FILLER PICTURE  S9(5)                    CI0328
                          BINARY.                                       CI0328
            11            PL06-XSEGNM PICTURE  X(08).                   CI0328
            11            PL06-XKEYLN PICTURE  S9(05)                   CI0328
                          BINARY.                                       CI0328
            11            PL06-XSEGNB PICTURE  9(05)                    CI0328
                          BINARY.                                       CI0328
            11            PL06-XCOKEY PICTURE  X(70).                   CI0328
      *** PCB MASK FOR LUVP                                             ADU015
      *!WF DSP=PM DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PM00.                                         CI0328
          05              PM00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00106).                  CI0328
       01                 PM06  REDEFINES      PM00.                    CI0328
            10            PM06-XDBPCB.                                  CI0328
            11            PM06-XDBDNM PICTURE  X(08).                   CI0328
            11            PM06-XSEGLV PICTURE  X(02).                   CI0328
            11            PM06-XRC    PICTURE  X(02).                   CI0328
            11            PM06-XPROPT PICTURE  X(04).                   CI0328
            11            PM06-FILLER PICTURE  S9(5)                    CI0328
                          BINARY.                                       CI0328
            11            PM06-XSEGNM PICTURE  X(08).                   CI0328
            11            PM06-XKEYLN PICTURE  S9(05)                   CI0328
                          BINARY.                                       CI0328
            11            PM06-XSEGNB PICTURE  9(05)                    CI0328
                          BINARY.                                       CI0328
            11            PM06-XCOKEY PICTURE  X(70).                   CI0328
      *** PCB MASK FOR LARP                                             ADU015
      *!WF DSP=PN DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PN00.                                         CI0328
          05              PN00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00106).                  CI0328
       01                 PN06  REDEFINES      PN00.                    CI0328
            10            PN06-XDBPCB.                                  CI0328
            11            PN06-XDBDNM PICTURE  X(08).                   CI0328
            11            PN06-XSEGLV PICTURE  X(02).                   CI0328
            11            PN06-XRC    PICTURE  X(02).                   CI0328
            11            PN06-XPROPT PICTURE  X(04).                   CI0328
            11            PN06-FILLER PICTURE  S9(5)                    CI0328
                          BINARY.                                       CI0328
            11            PN06-XSEGNM PICTURE  X(08).                   CI0328
            11            PN06-XKEYLN PICTURE  S9(05)                   CI0328
                          BINARY.                                       CI0328
            11            PN06-XSEGNB PICTURE  9(05)                    CI0328
                          BINARY.                                       CI0328
            11            PN06-XCOKEY PICTURE  X(70).                   CI0328
      *** PCB MASK FOR LPDP                                             ADU015
      *!WF DSP=PO DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PO00.                                         CI0328
          05              PO00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00106).                  CI0328
       01                 PO06  REDEFINES      PO00.                    CI0328
            10            PO06-XDBPCB.                                  CI0328
            11            PO06-XDBDNM PICTURE  X(08).                   CI0328
            11            PO06-XSEGLV PICTURE  X(02).                   CI0328
            11            PO06-XRC    PICTURE  X(02).                   CI0328
            11            PO06-XPROPT PICTURE  X(04).                   CI0328
            11            PO06-FILLER PICTURE  S9(5)                    CI0328
                          BINARY.                                       CI0328
            11            PO06-XSEGNM PICTURE  X(08).                   CI0328
            11            PO06-XKEYLN PICTURE  S9(05)                   CI0328
                          BINARY.                                       CI0328
            11            PO06-XSEGNB PICTURE  9(05)                    CI0328
                          BINARY.                                       CI0328
            11            PO06-XCOKEY PICTURE  X(70).                   CI0328

      *PASS AREA TO/FROM CI0328
      *!WF DSP=V2 DSL=V2 SEL=28 FOR=I DES=1 LEV=1 PLT=04
       01                 V228.                                         CI0328
            10            V228-CTID.                                    CI0328
            11            V228-CTIDA  PICTURE  9(3).                    CI0328
            11            V228-CTIDN.                                   CI0328
            12            V228-CTIDNP PICTURE  X(13).                   CI0328
            12            V228-CTIDND PICTURE  9(11).                   CI0328
            10            V228-CTTYPG PICTURE  X(04).                   CI0328
            10            V228-CPORTA PICTURE  X.                       CI0328
            10            V228-CACTS  PICTURE  X.                       CI0328
            10            V228-CPROCM PICTURE  X.                       CI0328
            10            V228-APMTD  PICTURE  S9(11)V99                CI0328
                          COMPUTATIONAL-3.                              CI0328
            10            V228-GESTD  PICTURE  9(8).                    CI0328
            10            V228-CIRAP  PICTURE  XX.                      CI0328
            10            V228-CTCUS  PICTURE  999.                     CI0328
            10            V228-CTSTA  PICTURE  99.                      CI0328
            10            V228-CPRDA1 PICTURE  9(3).                    CI0328
            10            V228-CLCUS  PICTURE  99.                      CI0328
            10            V228-PRCOD  PICTURE  9(5).                    CI0328
            10            V228-PRSCD  PICTURE  X(9).                    CI0328
            10            V228-CPRSC2 PICTURE  X(9).                    CI0328
            10            V228-ASURR  PICTURE  S9(07)V99                CI0328
                          COMPUTATIONAL-3.                              CI0328
            10            V228-ATERF  PICTURE  S9(5)V99                 CI0328
                          COMPUTATIONAL-3.                              CI0328
            10            V228-ALOAD  PICTURE  S9(9)V99                 CI0328
                          COMPUTATIONAL-3.                              CI0328
            10            V228-ATFRA  PICTURE  S9(9)V99                 CI0328
                          COMPUTATIONAL-3.                              CI0328
            10            V228-CLDOB  PICTURE  9(8).                    CI0328
            10            V228-CTID01.                                  CI0328
            11            V228-CACTID PICTURE  9(3).                    CI0328
            11            V228-CTIDNB.                                  CI0328
            12            V228-CTIDP1 PICTURE  X(13).                   CI0328
            12            V228-CTIDNA PICTURE  9(11).                   CI0328
            10            V228-CACTA  PICTURE  X(1).                    CI0328
            10            V228-CTCUS2 PICTURE  999.                     CI0328
            10            V228-PRCOD1 PICTURE  9(5).                    CI0328
            10            V228-QITEM  PICTURE  9(3).                    CI0328
            10            V228-NMESS2 PICTURE  S9(6)                    CI0328
                          OCCURS       020     TIMES                    CI0328
                          COMPUTATIONAL-3.                              CI0328
            10            V228-GEEND  PICTURE  9(8).                    CI0328
            10            V228-GEEND2 PICTURE  9(8).                    CI0328
            10            V228-ALDDUE PICTURE  9(08).                   CI0328
            10            V228-ITRAL  PICTURE  X(1).                    CI0328
            10            V228-IGRACV PICTURE  X.                       CI0328
            10            V228-MPMTT  PICTURE  X(20).                   CI0328
            10            V228-MPMTF  PICTURE  X(14).                   CI0328
            10            V228-AACTV  PICTURE  S9(11)V99                CI0328
                          COMPUTATIONAL-3.                              CI0328
            10            V228-IDRMD  PICTURE  X.                       CI0328
            10            V228-CGMBR  PICTURE  X.                       CI0328
            10            V228-FILLER PICTURE  X(091).                  CI0328

      ******************************************************************
      **CAMS ACCOUNTING DATE                                           *
      ******************************************************************
      *
      *!WF DSP=NS DSL=NS SEL=20 FOR=I LEV=1 PLT=04
       01                 NS00.                                         CI0328
          05              NS00-00.                                      CI0328
            10            NS00-NS00K.                                   CI0328
            11            NS00-PRCSTK PICTURE  XX.                      CI0328
          05              NS00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00078).                  CI0328
       01                 NS20  REDEFINES      NS00.                    CI0328
            10       FILLER         PICTURE  X(00002).                  CI0328
            10            NS20-DCACG  PICTURE  9(8).                    CI0328
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0328
                          COMPUTATIONAL-3.                              CI0328
            10            NS20-CCDAT  PICTURE  X(8).                    CI0328
            10            NS20-DCALP  PICTURE  X(12).                   CI0328
            10            NS20-DNACG  PICTURE  9(8).                    CI0328
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0328
                          COMPUTATIONAL-3.                              CI0328
            10            NS20-CNDAT  PICTURE  X(8).                    CI0328
            10            NS20-DNALP  PICTURE  X(12).                   CI0328
            10            NS20-DCACD  PICTURE  X(10).                   CI0328
            10            NS20-FILLER PICTURE  X(4).                    CI0328
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0328
          05              DE00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00653).                  CI0328
       01                 DE10  REDEFINES      DE00.                    CI0328
            10            DE10-DU11.                                    CI0328
            11            DE10-XFONC  PICTURE  X(4).                    CI0328
            11            DE10-MPSBN  PICTURE  X(8).                    CI0328
            11            DE10-XDBDNM PICTURE  X(08).                   CI0328
            11            DE10-XSEGNM PICTURE  X(08).                   CI0328
            11            DE10-XRC    PICTURE  X(02).                   CI0328
            11            DE10-MSEG   PICTURE  X(08).                   CI0328
            11            DE10-XCOKEY PICTURE  X(70).                   CI0328
            11            DE10-CUIBR  PICTURE  X(01).                   CI0328
            11            DE10-CUIBA  PICTURE  X(01).                   CI0328
            11            DE10-IPBIK  PICTURE  X(1).                    CI0328
            10            DE10-DU03.                                    CI0328
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0328
                          COMPUTATIONAL-3.                              CI0328
            11            DE10-CMSSF  PICTURE  XX.                      CI0328
            11            DE10-DU09.                                    CI0328
            12            DE10-CMESA  PICTURE  S9(9)                    CI0328
                          BINARY.                                       CI0328
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0328
                          BINARY.                                       CI0328
            12            DE10-CMESB  PICTURE  S9(9)                    CI0328
                          BINARY.                                       CI0328
            12            DE10-CMSST  PICTURE  S9(9)                    CI0328
                          BINARY.                                       CI0328
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0328
                          BINARY.                                       CI0328
            12            DE10-QELLAA PICTURE  S9(9)                    CI0328
                          BINARY.                                       CI0328
            12            DE10-TMESS4 PICTURE  X(512).                  CI0328
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
       01                 MS00.                                         CI0328
          05              MS00-SUITE.                                   CI0328
            15       FILLER         PICTURE  X(00542).                  CI0328
       01                 MS03  REDEFINES      MS00.                    CI0328
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0328
                          COMPUTATIONAL-3.                              CI0328
            10            MS03-CMSSF  PICTURE  XX.                      CI0328
            10            MS03-DU09.                                    CI0328
            11            MS03-CMESA  PICTURE  S9(9)                    CI0328
                          BINARY.                                       CI0328
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0328
                          BINARY.                                       CI0328
            11            MS03-CMESB  PICTURE  S9(9)                    CI0328
                          BINARY.                                       CI0328
            11            MS03-CMSST  PICTURE  S9(9)                    CI0328
                          BINARY.                                       CI0328
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0328
                          BINARY.                                       CI0328
            11            MS03-QELLAA PICTURE  S9(9)                    CI0328
                          BINARY.                                       CI0328
            11            MS03-TMESS4 PICTURE  X(512).                  CI0328
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0328
            10            MX11-QMSGS  PICTURE  9(03).                   CI0328
            10            MX11-PJ09                                     CI0328
                          OCCURS       025     TIMES.                   CI0328
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0328
                          COMPUTATIONAL-3.                              CI0328
            11            MX11-CMESB  PICTURE  S9(9)                    CI0328
                          BINARY.                                       CI0328
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                V228
                                NS20
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0328
      *               *                                   *             CI0328
      *               *INITIALISATIONS                    *             CI0328
      *               *                                   *             CI0328
      *               *************************************.            CI0328
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
      *N02BB.    NOTE *INITIALIZE WORKING STORAGE         *.
       F02BB.                                                           lv10
           MOVE        1 TO IWS00L
           MOVE        ZERO TO V228-QITEM
           MOVE        V228-CTTYPG TO WS00-CTTYPG
           MOVE        V228-CACTS TO WS00-CACTS
           MOVE        V228-CTCUS TO WS00-CTCUS
           MOVE        V228-CPRDA1 TO WS00-CPRDA1
           MOVE        V228-CLCUS TO WS00-CLCUS
           MOVE        V228-CACTID TO WS00-CTIDA1
           MOVE        V228-CTCUS2 TO WS00-CTCUS2
           MOVE        V228-PRCOD TO WS00-PRCOD
                 IF    V228-CTIDA = 002                                 DOT
      *FUND SOURCE ACCT
      *READ TA5A TO GET SHARE CLASS
           MOVE        V228-CTIDA TO TA5A-CTIDA
           MOVE        V228-PRCOD TO TA5A-PRCOD
           MOVE        V228-PRSCD TO TA5A-PRSCD
           PERFORM     F92TA THRU F92TA-FN
           MOVE        TA5A-MSP03 TO WS00-MSP03.
       F02BB-FN. EXIT.
      *N02BI.    NOTE *MUST BE VERIFY OR UPDATE           *.
       F02BI.    IF    NOT VALID-ACTIVITY-TYPE                          lv10
                 NEXT SENTENCE ELSE GO TO     F02BI-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012367 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F02BI-FN. EXIT.
      *N02CB.    NOTE *SET DATABASE POINTERS              *.
       F02CB.                                                           lv10
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR AREY                                             DOT
           SET ADDRESS OF PD06 TO                                       ADU015
                PCB-AREY-PTR1.                                          ADU015
      *SET ADDRESS FOR ARAY                                             DOT
           SET ADDRESS OF PE06 TO                                       ADU015
                PCB-ARAY-PTR1.                                          ADU015
      *SET ADDRESS FOR GR1P                                             DOT
           SET ADDRESS OF PF06 TO                                       ADU015
                PCB-GR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR SSPP                                             DOT
           SET ADDRESS OF PG06 TO                                       ADU015
                PCB-SSPP-PTR1.                                          ADU015
      *SET ADDRESS FOR CA1P                                             DOT
           SET ADDRESS OF PH06 TO                                       ADU015
                PCB-CA1P-PTR1.                                          ADU015
      *SET ADDRESS FOR TR1P                                             DOT
           SET ADDRESS OF PI06 TO                                       ADU015
                PCB-TR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR LH1P                                             DOT
           SET ADDRESS OF PK06 TO                                       ADU015
                PCB-LH1P-PTR1.                                          ADU015
      *SET ADDRESS FOR LM1P                                             DOT
           SET ADDRESS OF PL06 TO                                       ADU015
                PCB-LM1P-PTR1.                                          ADU015
      *SET ADDRESS FOR LUVP                                             DOT
           SET ADDRESS OF PM06 TO                                       ADU015
                PCB-LUVP-PTR1.                                          ADU015
      *SET ADDRESS FOR LARP                                             DOT
           SET ADDRESS OF PN06 TO                                       ADU015
                PCB-LARP-PTR1.                                          ADU015
      *SET ADDRESS FOR LPDP                                             DOT
           SET ADDRESS OF PO06 TO                                       ADU015
                PCB-LPDP-PTR1.                                          ADU015
       F02CB-FN. EXIT.
      *N02DA.    NOTE *CALCULATE CLIENT AGE               *.
       F02DA.                                                           lv10
      * COMPARE DAYS AND MONTHS
           MOVE        NS20-DCACG TO WS00-CURRENT
           MOVE        V228-CLDOB TO WS00-BIRTH.
       F02DA-FN. EXIT.
      *N02DB.    NOTE *CALCULATE CLIENT AGE               *.
       F02DB.    IF    WS00-CURR-MMDD NOT =                             lv10
                       WS00-BIRTH-MMDD
                 NEXT SENTENCE ELSE GO TO     F02DB-FN.
      *CONVERT CURRENT DATE TO JULIAN
           INITIALIZE  DD30 DD33 7-OAGE-PASSED-FIELDS
           MOVE        NS20-DCACG TO DD33-DTGRG
           MOVE        4 TO DD30-CDTSF
           PERFORM     F91AB THRU F91AB-FN.
                 IF    DD30-CDTSC = 0                                   DOT
           MOVE        DD33-DTJUL TO 7-OAGE-CURRENT-DATE.
      *CONVERT DATE OF BIRTH TO JULIAN                                  DOT
           INITIALIZE  DD30 DD33
           MOVE        V228-CLDOB TO DD33-DTGRG
           MOVE        4 TO DD30-CDTSF
           PERFORM     F91AB THRU F91AB-FN.
                 IF    DD30-CDTSC = 0                                   DOT
           MOVE        DD33-DTJUL TO 7-OAGE-BIRTH-DATE.
      *N02DD.    NOTE *ONLY IF JULIAN DATES ARE > ZERO    *.
       F02DD.    IF    7-OAGE-CURRENT-DATE > ZERO                       lv15
                 AND   7-OAGE-BIRTH-DATE > ZERO
                 NEXT SENTENCE ELSE GO TO     F02DD-FN.
           PERFORM     F91BB THRU F91BB-FN
           MOVE        7-OAGE-CLIENT-AGE TO
           WS00-CLIENT-AGE.
       F02DD-FN. EXIT.
       F02DB-900. GO TO F02DF-FN.
       F02DB-FN. EXIT.
      *N02DF.    NOTE *DIRECTLY SUBTRACT YEARS            *.
       F02DF.                                                           lv10
           COMPUTE     WS00-CLIENT-AGE =
           WS00-CURR-CCYY -
           WS00-BIRTH-CCYY.
       F02DF-FN. EXIT.
      *N02GB.    NOTE *PERFORM CICS ASSIGN TO GET         *.
       F02GB.                                                           lv10
      *LOGIN ID
      *
           EXEC CICS   ASSIGN USERID (WS00-NCUSR2)           END-EXEC.
       F02GB-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0328
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0328
      *               *                                   *             CI0328
      *               *FIN DE TRAITEMENT                  *             CI0328
      *               *                                   *             CI0328
      *               *************************************.            CI0328
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0328
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N42.      NOTE *************************************.
      *               *                                   *
      *               *COMMON TRANSACTION MESSAGES THAT   *
      *               *                                   *
      *               *************************************.
       F42.                                                             lv05
      *WILL BE DISPLAYED EACH TIME
      *N42BB.    NOTE *PROCESSING MESSAGE                 *.
       F42BB.         EXIT.                                             lv10
      *N42BC.    NOTE *DEFAULT MESSAGE FOR IMPACT OF      *.
       F42BC.                                                           lv15
      *CHANGING ARRANGMENTS
           MOVE        15069 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F42BC-FN. EXIT.
      *N42BD.    NOTE *MOVE MONEY TO ACCOUNT              *.
       F42BD.    IF    MMTA                                             lv15
                 OR    SMTC OR SMDD
                 NEXT SENTENCE ELSE GO TO     F42BD-FN.
      *SEND MONEY TO CLIENT
      *N42BF.    NOTE *FOR VERIFY REQUEST                 *.
       F42BF.    IF    V228-CACTS = 'V'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F42BF-FN.
      *
                 IF    V228-CPROCM = 'A'                                DOT
      *ADD REQUEST
           MOVE        15065 TO WS00-NMESS2.
                 IF    V228-CPROCM = 'M'                                DOT
      *MODIFY REQUEST
           MOVE        15067 TO WS00-NMESS2.
           PERFORM     F91OB THRU F91OB-FN.                             DOT
       F42BF-FN. EXIT.
      *N42BI.    NOTE *FOR CONFIRM REQUEST                *.
       F42BI.    IF    V228-CACTS = 'S'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F42BI-FN.
      *
                 IF    V228-CPROCM = 'A'                                DOT
      *ADD REQUEST
           MOVE        15066 TO WS00-NMESS2.
                 IF    V228-CPROCM = 'M'                                DOT
      *MODIFY REQUEST
           MOVE        15068 TO WS00-NMESS2.
           PERFORM     F91OB THRU F91OB-FN.                             DOT
       F42BI-FN. EXIT.
       F42BD-FN. EXIT.
       F42BB-FN. EXIT.
       F42-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *CERTIFICATE SOURCE ACCOUNT         *
      *               *                                   *
      *               *************************************.
       F45.      IF    V228-CTIDA = 001                                 lv05
                 NEXT SENTENCE ELSE GO TO     F45-FN.
      *N45CB.    NOTE *SURR CHARGES MESSAGE LOGIC         *.
       F45CB.         EXIT.                                             lv10
      *N45CH.    NOTE *SURRENDER CHARGE MESSAGE           *.
       F45CH.    IF    V228-ASURR > ZERO                                lv20
                 NEXT SENTENCE ELSE GO TO     F45CH-FN.
           MOVE        15054 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F45CH-FN. EXIT.
       F45CB-FN. EXIT.
      *N45EB.    NOTE *TAXATION                           *.
       F45EB.         EXIT.                                             lv10
      *N45ED.    NOTE *(MOVE MONEY TO ACCOUNT             *.
       F45ED.    IF    ((MMTA                                           lv15
                 AND   TO-NON-QUALIFIED)
                 OR    (SMTC)
                 OR    (SMDD))
                 AND   V228-CACTA NOT = 'I'
                 NEXT SENTENCE ELSE GO TO     F45ED-FN.
      *AND TO NON-QUALIFIED)
      *OR (SEND MONEY TO CLIENT)
      *OR (SEND MONEY DIRECT DEPOSIT)
      *AND NOT INACTIVATION
      *N45EF.    NOTE *FROM NON-QULAIFIED                 *.
       F45EF.    IF    FROM-NON-QUALIFIED                               lv20
                 NEXT SENTENCE ELSE GO TO     F45EF-FN.
                 IF    FROM-TAXDEF-CERT                                 DOT
      *TAX DEFERRED CERTIFICATE
           MOVE        15057 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F45EF-900. GO TO F45EH-FN.
       F45EF-FN. EXIT.
      *N45EH.    NOTE *FROM IRA                           *.
       F45EH.         EXIT.                                             lv20
      *N45EJ.    NOTE *CLIENT AGE IS 59.5 OR OLDER        *.
       F45EJ.    IF    WS00-CLIENT-AGE >= 59.5                          lv25
                 NEXT SENTENCE ELSE GO TO     F45EJ-FN.
           MOVE        13521 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
      *N45EK.    NOTE *NOT FROM PREMATURE DISTRIBUTION    *.
       F45EK.    IF    NOT FROM-PRX                                     lv30
                 AND   NOT FROM-DIS
                 AND   NOT FROM-NON
                 AND   FROM-SRA
                 NEXT SENTENCE ELSE GO TO     F45EK-FN.
      *NOT FROM DISABILITY
      *NOT FROM NON-TAX REPORTABLE
           MOVE        13546 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F45EK-FN. EXIT.
       F45EJ-FN. EXIT.
       F45EH-FN. EXIT.
       F45ED-FN. EXIT.
       F45EB-FN. EXIT.
      *N45GB.    NOTE *PREMATURE WITHDRAWAL PENALTY       *.
       F45GB.         EXIT.                                             lv10
      *N45GD.    NOTE *(MOVE MONEY TO ACCOUNT             *.
       F45GD.    IF    (MMTA                                            lv15
                 AND   TO-NON-QUALIFIED)
                 OR    (SMTC)
                 OR    (SMDD)
                 NEXT SENTENCE ELSE GO TO     F45GD-FN.
      *AND TO NON-QUALIFIED)
      *OR (SEND MONEY TO CLIENT)
      *OR (SEND MONEY DIRECT DEPOSIT)
      *N45GF.    NOTE *FROM IRA                           *.
       F45GF.    IF    FROM-QUALIFIED                                   lv20
                 AND   V228-CACTA NOT = 'I'
                 NEXT SENTENCE ELSE GO TO     F45GF-FN.
      *AND NOT INACTIVATE
      *N45GH.    NOTE *CLIENT AGE IS UNDER 59.5           *.
       F45GH.    IF    WS00-CLIENT-AGE < 59.5                           lv25
                 NEXT SENTENCE ELSE GO TO     F45GH-FN.
      *N45GJ.    NOTE *NOT FROM PREMATURE DISTRIBUTION    *.
       F45GJ.    IF    NOT FROM-PRX                                     lv30
                 AND   NOT FROM-DIS
                 AND   NOT FROM-NON
                 NEXT SENTENCE ELSE GO TO     F45GJ-FN.
      *NOT FROM DISABILITY
      *NOT FROM NON-TAX REPORTABLE
                 IF    FROM-SRA                                         DOT
      *FROM SRA
           MOVE        13540 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN
           MOVE        13546 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN
                 ELSE
      *ANY OTHER IRA
           MOVE        13541 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN
           MOVE        13547 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F45GJ-FN. EXIT.
       F45GH-FN. EXIT.
       F45GF-FN. EXIT.
       F45GD-FN. EXIT.
       F45GB-FN. EXIT.
      *N45JB.    NOTE *ARRANGEMENTS                       *.
       F45JB.         EXIT.                                             lv10
      *N45JF.    NOTE *CHECK FOR OTHER ARRANGEMENTS       *.
       F45JF.                                                           lv15
      *MOVE DATA FOR CI0205 CALL
           INITIALIZE  CB00
           MOVE        V228-CTID TO CB00-CTID
           MOVE        99 TO CB00-QIMAX
      *CALL CI0205 TO GET ARRANGEMENTS
           PERFORM     F95CB THRU F95CB-FN
      *CALL CI0975 TO GET GROUP BILL
           INITIALIZE  DA8G
           DA8H
           MOVE        14 TO DA8G-CARTY
           MOVE        V228-CTID TO DA8H-CTID (1)
           MOVE        1 TO DA8H-QITEM
           MOVE        'RPC' TO DA8G-CSYS
           PERFORM     F95EA THRU F95EA-FN.
                 IF    MONEY-COMING-IN                                  DOT
      *MONEY COMING IN
           MOVE        15060 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
                 IF    GROUP-BILLING                                    DOT
      *GROUP BILLING
           MOVE        15061 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
                 IF    MILITARY-ALLOTMENT                               DOT
      *MILITARY ALLOTMENT
           MOVE        15062 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F45JF-FN. EXIT.
       F45JB-FN. EXIT.
      *N45MB.    NOTE *IRA CONTRIBUTION MESSAGES          *.
       F45MB.         EXIT.                                             lv10
      *N45MD.    NOTE *MOVE MONEY TO ACCOUNT              *.
       F45MD.    IF    MMTA                                             lv15
                 NEXT SENTENCE ELSE GO TO     F45MD-FN.
      *N45MF.    NOTE *FROM NON-QUALIFIED                 *.
       F45MF.    IF    FROM-NON-QUALIFIED                               lv20
                 NEXT SENTENCE ELSE GO TO     F45MF-FN.
      *N45MH.    NOTE *TO IRA                             *.
       F45MH.    IF    TO-QUALIFIED                                     lv25
                 NEXT SENTENCE ELSE GO TO     F45MH-FN.
      *N45MJ.    NOTE *CASE OF CUSTODIAL CODE (IRA)       *.
       F45MJ.         EXIT.                                             lv30
      *N45ML.    NOTE *TO SEP                             *.
       F45ML.    IF    WS00-CTCUS2 =                                    lv35
                       008
                 NEXT SENTENCE ELSE GO TO     F45ML-FN.
                 IF    WS00-CLIENT-AGE < 50                             DOT
           MOVE        13610 TO WS00-NMESS2
                 ELSE
           MOVE        13516 TO WS00-NMESS2.
           PERFORM     F91OB THRU F91OB-FN.                             DOT
       F45ML-900. GO TO F45MJ-FN.
       F45ML-FN. EXIT.
      *N45MN.    NOTE *TO SRA                             *.
       F45MN.    IF    WS00-CTCUS2 =                                    lv35
                       010
                 NEXT SENTENCE ELSE GO TO     F45MN-FN.
                 IF    WS00-CLIENT-AGE < 50                             DOT
           MOVE        13611 TO WS00-NMESS2
                 ELSE
           MOVE        13515 TO WS00-NMESS2.
           PERFORM     F91OB THRU F91OB-FN.                             DOT
       F45MN-900. GO TO F45MJ-FN.
       F45MN-FN. EXIT.
      *N45QP.    NOTE *TO IRA ROLLOVER                    *.
       F45QP.    IF    WS00-CTCUS2 =                                    lv35
                       002
                 NEXT SENTENCE ELSE GO TO     F45QP-FN.
           MOVE        13612 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F45QP-900. GO TO F45MJ-FN.
       F45QP-FN. EXIT.
       F45MJ-FN. EXIT.
       F45MH-FN. EXIT.
       F45MF-FN. EXIT.
       F45MD-FN. EXIT.
       F45MB-FN. EXIT.
      *N45RB.    NOTE *CROSS ADMIN TRANSACTION            *.
       F45RB.    IF    MMTA                                             lv10
                 AND   V228-CTIDA NOT =
                       V228-CACTID
                 NEXT SENTENCE ELSE GO TO     F45RB-FN.
      *INDICATE A TWO DAY TURNAROUND
           MOVE        015236 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F45RB-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *FUND SOURCE ACCOUNT                *
      *               *                                   *
      *               *************************************.
       F50.      IF    V228-CTIDA = 002                                 lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *N50BA.    NOTE *MOVE MONEY TO ACCOUNT              *.
       F50BA.    IF    MMTA                                             lv10
                 OR    SMTC OR SMDD
                 NEXT SENTENCE ELSE GO TO     F50BA-FN.
      *SEND MONEY TO CLIENT
      *N50BC.    NOTE *PROCESSING DATE                    *.
       F50BC.    IF    FROM-NON-QUALIFIED                               lv15
                 AND   WS00-PRCOD NOT = 00107
                 AND   WS00-PRCOD NOT = 00124
                 AND   WS00-PRCOD NOT = 00125
                 AND   WS00-PRCOD NOT = 00126
                 NEXT SENTENCE ELSE GO TO     F50BC-FN.
      *ARC FUND
      *CONTRARIAN EQUITY FUND
      *U.S. EQUITY FUND
      *AND THREADNEEDLE GLOBAL EXTENDED
           COMPUTE     WS00-AACTV = V228-AACTV
           - V228-APMTD.
      *N50BF.    NOTE *ACCOUNT BALANCE MUST > $50         *.
       F50BF.    IF    WS00-AACTV < 50                                  lv20
                 NEXT SENTENCE ELSE GO TO     F50BF-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015456 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50BF-FN. EXIT.
      *N50BK.    NOTE *LOAD MESSAGE CODE                  *.
       F50BK.    IF    WS00-AACTV >= 50                                 lv20
                 AND   WS00-AACTV <= 1000
                 NEXT SENTENCE ELSE GO TO     F50BK-FN.
           MOVE        015455 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F50BK-FN. EXIT.
      *N50BN.    NOTE *RESET VALUE                        *.
       F50BN.                                                           lv20
           INITIALIZE  WS00-AACTV.
       F50BN-FN. EXIT.
       F50BC-FN. EXIT.
       F50BA-FN. EXIT.
      *N50CB.    NOTE *CDSC                               *.
       F50CB.    IF    V228-ALOAD NOT = 0                               lv10
                 NEXT SENTENCE ELSE GO TO     F50CB-FN.
      *N50CD.    NOTE *(MOVE MONEY TO ACCOUNT             *.
       F50CD.    IF    (MMTA                                            lv15
                 AND   TO-CERTIFICATE)
                 OR    (SMTC)
                 OR    (SMDD)
                 NEXT SENTENCE ELSE GO TO     F50CD-FN.
      *AND TO CERTIFICATE)
      *OR (SEND MONEY TO CLIENT)
      *OR (SEND MONEY DIRECT DEPOSIT)
      *N50CF.    NOTE *FROM CLASS B OR C MUTUAL FUND      *.
       F50CF.    IF    FROM-CLASSB-FUND                                 lv20
                 OR    FROM-CLASSC-FUND
                 NEXT SENTENCE ELSE GO TO     F50CF-FN.
      *INITIALISE SKIP FLAG TO SPACES
           MOVE        SPACES TO WS00-SKIP.
      *N50CG.    NOTE *CLIENT AGE IS 70.5 OR OLDER        *.
       F50CG.    IF    WS00-CLIENT-AGE >= 70.5                          lv25
                 AND   FROM-RET
                 AND   V228-IDRMD = 'Y'
                 NEXT SENTENCE ELSE GO TO     F50CG-FN.
      *AND FROM RET
      *SKIP CDSC LOGIC
           MOVE        'Y' TO WS00-SKIP.
       F50CG-FN. EXIT.
      *N50CH.    NOTE *FROM IRA                           *.
       F50CH.    IF    FROM-QUALIFIED                                   lv25
                 AND   ((TO-NON-QUALIFIED
                 AND   WS00-CLIENT-AGE >= 59.5)
                 OR    (WS00-CLIENT-AGE >= 59.5
                 AND   (FROM-PRX
                 OR    FROM-RPX
                 OR    FROM-CPX)))
                 NEXT SENTENCE ELSE GO TO     F50CH-FN.
      *DESTINATION
      *AGE
      *CLIENT AGE IS 59.5 OR OLDER
      *AND FROM SEPP - PRX
      * OR FROM SEPP - RPX
      * OR FROM SEPP - CPX
      *SKIP CDSC PROCESSING
           MOVE        'Y' TO WS00-SKIP.
       F50CH-FN. EXIT.
      *N50CI.    NOTE *CHECK IF CDSC IS SKIPPED           *.
       F50CI.    IF    WS00-SKIP NOT = 'Y'                              lv25
                 NEXT SENTENCE ELSE GO TO     F50CI-FN.
      *N50CJ.    NOTE *NOT FROM DEATH                     *.
       F50CJ.    IF    NOT FROM-DTH                                     lv30
                 NEXT SENTENCE ELSE GO TO     F50CJ-FN.
      *N50CP.    NOTE *PARTIAL REDEMPTION                 *.
       F50CP.                                                           lv35
      *+/- 5% AMOUNT FREE OF CDSC RANGE
           COMPUTE     WS00-APRMN = V228-ATFRA -
           (V228-ATFRA * 0.05)
           COMPUTE     WS00-APRMX = V228-ATFRA +
           (V228-ATFRA * 0.05).
      *N50CV.    NOTE *REDEMPTION AMOUNT IS >= MIN AMT    *.
       F50CV.    IF    V228-APMTD >= WS00-APRMN                         lv40
                 NEXT SENTENCE ELSE GO TO     F50CV-FN.
                 IF    MMTA                                             DOT
                 AND   NOT TO-FUND
      *IN CASE OF MMTA NOT TO FUND
           MOVE        15055 TO WS00-NMESS2.
                 IF    SMTC OR SMDD                                     DOT
           MOVE        15056 TO WS00-NMESS2.
           PERFORM     F91OB THRU F91OB-FN.                             DOT
       F50CV-FN. EXIT.
       F50CP-FN. EXIT.
       F50CJ-FN. EXIT.
       F50CI-FN. EXIT.
       F50CF-FN. EXIT.
       F50CD-FN. EXIT.
       F50CB-FN. EXIT.
      *N50EB.    NOTE *TAXATION                           *.
       F50EB.    IF    V228-CACTA NOT = 'I'                             lv10
                 NEXT SENTENCE ELSE GO TO     F50EB-FN.
      *N50EF.    NOTE *FROM NON-QULAIFIED                 *.
       F50EF.    IF    FROM-NON-QUALIFIED                               lv20
                 NEXT SENTENCE ELSE GO TO     F50EF-FN.
      *N50EH.    NOTE *MOVE MONEY TO ACCOUNT              *.
       F50EH.    IF    MMTA                                             lv25
                 NEXT SENTENCE ELSE GO TO     F50EH-FN.
      *N50EJ.    NOTE *SAME PRODUCT CODE                  *.
       F50EJ.    IF    V228-PRCOD = V228-PRCOD1                         lv30
                 NEXT SENTENCE ELSE GO TO     F50EJ-FN.
                 IF    TO-QUALIFIED                                     DOT
      *TO IRA
           MOVE        15059 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F50EJ-900. GO TO F50EL-FN.
       F50EJ-FN. EXIT.
      *N50EL.    NOTE *DIFFERENT PRODUCT CODE             *.
       F50EL.                                                           lv30
           MOVE        15058 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F50EL-FN. EXIT.
       F50EH-FN. EXIT.
       F50EF-900. GO TO F50EP-FN.
       F50EF-FN. EXIT.
      *N50EP.    NOTE *FROM IRA                           *.
       F50EP.         EXIT.                                             lv20
      *N50ER.    NOTE *CLIENT AGE IS 59.5 OR OLDER        *.
       F50ER.    IF    WS00-CLIENT-AGE >= 59.5                          lv25
                 NEXT SENTENCE ELSE GO TO     F50ER-FN.
                 IF    MMTA                                             DOT
                 AND   TO-NON-QUALIFIED
      *MOVE MONEY TO ACCOUNT
      *TO NON-QUALIFIED
           MOVE        13524 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
                 IF    SMTC OR SMDD                                     DOT
      *SEND MONEY TO CLIENT
           MOVE        13526 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
                 IF    NOT FROM-PRX                                     DOT
                 AND   NOT FROM-DIS
                 AND   NOT FROM-NON
                 AND   FROM-SRA
      *SRA IMPLICATION MESSAGE
           MOVE        13546 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F50ER-FN. EXIT.
       F50EP-FN. EXIT.
       F50EB-FN. EXIT.
      *N50GB.    NOTE *PREMATURE WITHDRAWAL PENALTY       *.
       F50GB.         EXIT.                                             lv10
      *N50GD.    NOTE *(MOVE MONEY TO ACCOUNT             *.
       F50GD.    IF    (MMTA                                            lv15
                 AND   TO-NON-QUALIFIED)
                 OR    (SMTC)
                 OR    (SMDD)
                 NEXT SENTENCE ELSE GO TO     F50GD-FN.
      *AND TO NON-QUALIFIED)
      *OR (SEND MONEY TO CLIENT)
      *OR (SEND MONEY DIRECT DEPOSIT)
      *N50GF.    NOTE *FROM IRA                           *.
       F50GF.    IF    NOT FROM-NON-QUALIFIED                           lv20
                 AND   V228-CACTA NOT = 'I'
                 NEXT SENTENCE ELSE GO TO     F50GF-FN.
      *AND NOT INACTIVATE
      *N50GH.    NOTE *CLIENT AGE IS UNDER 59.5           *.
       F50GH.    IF    WS00-CLIENT-AGE < 59.5                           lv25
                 NEXT SENTENCE ELSE GO TO     F50GH-FN.
      *N50GJ.    NOTE *NOT FROM PREMATURE DISTRIBUTION    *.
       F50GJ.    IF    NOT FROM-PRX                                     lv30
                 AND   NOT FROM-DIS
                 AND   NOT FROM-NON
                 NEXT SENTENCE ELSE GO TO     F50GJ-FN.
      *NOT FROM DISABILITY
      *NOT FROM NON-TAX REPORTABLE
      *N50GL.    NOTE *FROM SRA                           *.
       F50GL.    IF    FROM-SRA                                         lv35
                 NEXT SENTENCE ELSE GO TO     F50GL-FN.
                 IF    MMTA                                             DOT
           MOVE        13542 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
                 IF    SMTC OR SMDD                                     DOT
           MOVE        13544 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
           MOVE        13546 TO WS00-NMESS2                             DOT
           PERFORM     F91OB THRU F91OB-FN.
       F50GL-900. GO TO F50GN-FN.
       F50GL-FN. EXIT.
      *N50GN.    NOTE *ANY OTHER IRA                      *.
       F50GN.                                                           lv35
                 IF    MMTA                                             DOT
           MOVE        13543 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
                 IF    SMTC OR SMDD                                     DOT
           MOVE        13545 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
           MOVE        13547 TO WS00-NMESS2                             DOT
           PERFORM     F91OB THRU F91OB-FN.
       F50GN-FN. EXIT.
       F50GJ-FN. EXIT.
       F50GH-FN. EXIT.
       F50GF-FN. EXIT.
       F50GD-FN. EXIT.
       F50GB-FN. EXIT.
      *N50MB.    NOTE *ARRANGEMENTS                       *.
       F50MB.         EXIT.                                             lv10
      *N50MF.    NOTE *CHECK FOR OTHER ARRANGEMENTS       *.
       F50MF.                                                           lv20
      *MOVE DATA FOR CI0205 CALL
           INITIALIZE  CB00
           MOVE        V228-CTID TO CB00-CTID
           MOVE        99 TO CB00-QIMAX
      *CALL CI0205 TO GET ARRANGEMENTS
           PERFORM     F95CB THRU F95CB-FN
      *CALL CI0975 TO GET GROUP BILL
           INITIALIZE  DA8G
           DA8H
           MOVE        14 TO DA8G-CARTY
           MOVE        V228-CTID TO DA8H-CTID (1)
           MOVE        1 TO DA8H-QITEM
           MOVE        'RPC' TO DA8G-CSYS
           PERFORM     F95EA THRU F95EA-FN.
                 IF    MILITARY-ALLOTMENT                               DOT
      *MILITARY ALLOTMENT
           MOVE        15062 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F50MF-FN. EXIT.
       F50MB-FN. EXIT.
      *N50QB.    NOTE *IRA CONTRIBUTIONS                  *.
       F50QB.         EXIT.                                             lv10
      *N50QD.    NOTE *MOVE MONEY TO ACCOUNT              *.
       F50QD.    IF    MMTA                                             lv15
                 NEXT SENTENCE ELSE GO TO     F50QD-FN.
      *N50QF.    NOTE *FROM NON-QUALIFIED                 *.
       F50QF.    IF    FROM-NON-QUALIFIED                               lv20
                 NEXT SENTENCE ELSE GO TO     F50QF-FN.
      *N50QH.    NOTE *TO IRA                             *.
       F50QH.    IF    NOT TO-NON-QUALIFIED                             lv25
                 NEXT SENTENCE ELSE GO TO     F50QH-FN.
      *N50QJ.    NOTE *CASE OF CUSTODIAL CODE (IRA)       *.
       F50QJ.         EXIT.                                             lv30
      *N50QL.    NOTE *TO SEP                             *.
       F50QL.    IF    WS00-CTCUS2 =                                    lv35
                       008
                 NEXT SENTENCE ELSE GO TO     F50QL-FN.
                 IF    WS00-CLIENT-AGE < 50                             DOT
           MOVE        13610 TO WS00-NMESS2
                 ELSE
           MOVE        13516 TO WS00-NMESS2.
           PERFORM     F91OB THRU F91OB-FN.                             DOT
       F50QL-900. GO TO F50QJ-FN.
       F50QL-FN. EXIT.
      *N50QN.    NOTE *TO SRA                             *.
       F50QN.    IF    WS00-CTCUS2 =                                    lv35
                       010
                 NEXT SENTENCE ELSE GO TO     F50QN-FN.
                 IF    WS00-CLIENT-AGE < 50                             DOT
           MOVE        13611 TO WS00-NMESS2
                 ELSE
           MOVE        13515 TO WS00-NMESS2.
           PERFORM     F91OB THRU F91OB-FN.                             DOT
       F50QN-900. GO TO F50QJ-FN.
       F50QN-FN. EXIT.
      *N50QP.    NOTE *TO IRA ROLLOVER                    *.
       F50QP.    IF    WS00-CTCUS2 =                                    lv35
                       002
                 NEXT SENTENCE ELSE GO TO     F50QP-FN.
           MOVE        13612 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F50QP-900. GO TO F50QJ-FN.
       F50QP-FN. EXIT.
       F50QJ-FN. EXIT.
       F50QH-FN. EXIT.
       F50QF-FN. EXIT.
       F50QD-FN. EXIT.
       F50QB-FN. EXIT.
      *N50SB.    NOTE *IF SOURCE IS CASH MGMNT AND        *.
       F50SB.    IF    (((V228-PRCOD = 00013                            lv10
                 OR    V228-PRCOD = 00167)
                 AND   V228-PRSCD = '000000001')
                 OR    V228-PRCOD = 00016)
                 AND   (V228-CACTID = 002
                 AND   (V228-CPRSC2 = '000000002'
                 OR    V228-CPRSC2 = '000000006'))
                 NEXT SENTENCE ELSE GO TO     F50SB-FN.
      *CLASS A OR RVS GOVT MONEY MKT FN
      *CLASS A OR IF SOURCE IS TAX
      *TAX FREE MONEY AND DESTINATION
      *IS CLASS B OR CLASS MUTUAL FUND
      *N50SE.    NOTE *CALL THE ACCOUNT VIEW              *.
       F50SE.                                                           lv15
           MOVE        SPACES TO SQ1A
           SQ5A
      *POPULATE THE REQUEST AREA OF THE
      *ACCOUNT VIEW - SQ1A
           MOVE        001 TO SQ1A-NRERO
           MOVE        001 TO SQ5A-NRURO
           MOVE        V228-CTID TO SQ1A-CTID (1)
           MOVE        V228-PRCOD TO SQ1A-PRCOD (1)
           MOVE        V228-PRSCD TO SQ1A-CPRSCN (1).
      *N50SH.    NOTE *POPULATE THE VALUES TO THE INPUT   *.
       F50SH.                                                           lv20
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
       F50SH-FN. EXIT.
       F50SE-FN. EXIT.
      *N50SK.    NOTE *SOCKET BROKER CALL ERROR           *.
       F50SK.                                                           lv15
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
      *N50SN.    NOTE *ACCOUNT VIEW CALL - SUCCESSFUL     *.
       F50SN.    IF    WS00-IERRC = SPACES                              lv20
                 NEXT SENTENCE ELSE GO TO     F50SN-FN.
           MOVE        WL00-RESPONSE TO SQ5A.
       F50SN-900. GO TO F50SQ-FN.
       F50SN-FN. EXIT.
      *N50SQ.    NOTE *IN CASE OF ANY ERROR FROM DST      *.
       F50SQ.                                                           lv20
      *********************************
                 IF    (V228-CTSTA = 01                                 DOT
                 OR    V228-CTSTA = 03)
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
           MOVE                     ALL '1' TO FT GO TO F20.
       F50SQ-FN. EXIT.
       F50SK-FN. EXIT.
      *N50ST.    NOTE *INITIALISE THE REQUEST & RESPONS   *.
       F50ST.                                                           lv15
      *AREA FOR ACCOUNT VALUE VIEW.
      *********************************
           MOVE        SPACES TO SQ1E
           SQ5E
           MOVE        001 TO SQ1E-NRERO
           MOVE        001 TO SQ5E-NRURO
           INITIALIZE  WE00-ERROR-TABLE.
      *N50SW.    NOTE *LOAD THE SEGMENT TO CALL           *.
       F50SW.                                                           lv20
      *DST'S CALCULATION VIEW
      *************************
      *LOAD:
      *   ACCOUNT NUMBER
      *   PRODUCT CODE
      *   SUBPRODUCT CODE
      *   AS OF DATE
      *************************
           MOVE        V228-CTID TO SQ1E-CTID (1)
           MOVE        V228-PRCOD TO SQ1E-PRCOD (1)
           MOVE        V228-PRSCD TO SQ1E-CPRSCN (1).
       F50SW-FN. EXIT.
      *N50TB.    NOTE *LOAD THE VALUES TO THE INPUT       *.
       F50TB.                                                           lv20
      *LINKAGE SEGMENT OF THE BROKER
      *************************
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
      *N50TE.    NOTE *GET THE RESPONSE FROM THE VIEW     *.
       F50TE.    IF    WS00-IERRC = SPACES                              lv25
                 NEXT SENTENCE ELSE GO TO     F50TE-FN.
      *SUCCESSFUL CALL
           MOVE        WL00-RESPONSE TO SQ5E.
       F50TE-900. GO TO F50TH-FN.
       F50TE-FN. EXIT.
      *N50TH.    NOTE *IF ACCOUNT NOT FOUND AND           *.
       F50TH.                                                           lv25
                 IF    (V228-CTSTA = 01                                 DOT
                 OR    V228-CTSTA = 03)
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
           MOVE                     ALL '1' TO FT GO TO F20.
       F50TH-FN. EXIT.
      *N50TK.    NOTE *SET NEW MONEY CASH INDICATOR       *.
       F50TK.                                                           lv25
           COMPUTE     WK00-ACOMO = SQ5A-QSHOM2 (1) *
           SQ5E-AFAV10 (1).
                 IF    WK00-ACOMO = SQ5A-ACINEV (1)                     DOT
      *IF OLD MONEY CASH AMOUNT =
      *   CASH INVESTED AMOUNT
           MOVE        'N' TO WS00-INMRC
                 ELSE
      *ELSE NEW MONEY DOES EXIST
           MOVE        'Y' TO WS00-INMRC
           COMPUTE     WK00-ACNMO = SQ5A-ACINEV (1) -
           WK00-ACOMO
           MOVE        WK00-ACNMO TO WS00-ACNMO.
      *N50TN.    NOTE *IF NEW MONEY EXISTS                *.
       F50TN.    IF    WS00-INMRC = 'Y'                                 lv30
                 NEXT SENTENCE ELSE GO TO     F50TN-FN.
      *********************************
                 IF    V228-APMTD > WS00-ACNMO                          DOT
      *IF AMOUNT ENTERED IS GREATER
      *THAN NEW MONEY AMOUNT
      *********************************
           MOVE        15115 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F50TN-FN. EXIT.
       F50TK-FN. EXIT.
       F50TB-FN. EXIT.
       F50ST-FN. EXIT.
       F50SB-FN. EXIT.
      *N50UB.    NOTE *CROSS ADMIN TRANSACTION            *.
       F50UB.    IF    MMTA                                             lv10
                 AND   V228-CTIDA NOT =
                       V228-CACTID
                 NEXT SENTENCE ELSE GO TO     F50UB-FN.
      *INDICATE A TWO DAY TURNAROUND
           MOVE        015236 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F50UB-FN. EXIT.
      *N50UD.    NOTE *IF SOURCE ACCOUNT IS MUTUAL FUND   *.
       F50UD.                                                           lv10
      *SEND OUT DISCLOSURE MESSAGE
           MOVE        015608 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F50UD-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *BROK OR BETA BROK SOURCE ACCOUNT   *
      *               *                                   *
      *               *************************************.
       F55.      IF    V228-CTIDA = 021 OR 133                          lv05
                 NEXT SENTENCE ELSE GO TO     F55-FN.
      *N55BB.    NOTE *SOURCE ACCOUNT                     *.
       F55BB.         EXIT.                                             lv10
      *N55BE.    NOTE *ACCESS FREE CASH BALANCE           *.
       F55BE.                                                           lv15
           MOVE        015227 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F55BE-FN. EXIT.
       F55BB-FN. EXIT.
      *N55EB.    NOTE *TAXATION                           *.
       F55EB.    IF    V228-CACTA NOT = 'I'                             lv10
                 NEXT SENTENCE ELSE GO TO     F55EB-FN.
      *N55EP.    NOTE *FROM IRA                           *.
       F55EP.    IF    FROM-QUALIFIED                                   lv20
                 NEXT SENTENCE ELSE GO TO     F55EP-FN.
      *N55ER.    NOTE *CLIENT AGE IS 59.5 OR OLDER        *.
       F55ER.    IF    WS00-CLIENT-AGE >= 59.5                          lv25
                 NEXT SENTENCE ELSE GO TO     F55ER-FN.
                 IF    MMTA                                             DOT
                 AND   TO-NON-QUALIFIED
      *MOVE MONEY TO ACCOUNT
      *TO NON-QUALIFIED
           MOVE        13524 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
                 IF    SMTC OR SMDD                                     DOT
      *SEND MONEY TO CLIENT
           MOVE        13526 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F55ER-FN. EXIT.
       F55EP-FN. EXIT.
       F55EB-FN. EXIT.
      *N55GB.    NOTE *PREMATURE WITHDRAWAL PENALTY       *.
       F55GB.         EXIT.                                             lv10
      *N55GD.    NOTE *(MOVE MONEY TO ACCOUNT             *.
       F55GD.    IF    (MMTA                                            lv15
                 AND   TO-NON-QUALIFIED)
                 OR    (SMTC)
                 OR    (SMDD)
                 NEXT SENTENCE ELSE GO TO     F55GD-FN.
      *AND TO NON-QUALIFIED)
      *OR (SEND MONEY TO CLIENT)
      *OR (SEND MONEY DIRECT DEPOSIT)
      *N55GF.    NOTE *FROM IRA                           *.
       F55GF.    IF    NOT FROM-NON-QUALIFIED                           lv20
                 AND   V228-CACTA NOT = 'I'
                 NEXT SENTENCE ELSE GO TO     F55GF-FN.
      *AND NOT INACTIVATE
      *N55GH.    NOTE *CLIENT AGE IS UNDER 59.5           *.
       F55GH.    IF    WS00-CLIENT-AGE < 59.5                           lv25
                 NEXT SENTENCE ELSE GO TO     F55GH-FN.
      *N55GJ.    NOTE *NOT FROM PREMATURE DISTRIBUTION    *.
       F55GJ.    IF    NOT FROM-PRX                                     lv30
                 AND   NOT FROM-DIS
                 AND   NOT FROM-NON
                 NEXT SENTENCE ELSE GO TO     F55GJ-FN.
      *NOT FROM DISABILITY
      *NOT FROM NON-TAX REPORTABLE
      *N55GL.    NOTE *FROM SRA                           *.
       F55GL.    IF    FROM-SRA                                         lv35
                 NEXT SENTENCE ELSE GO TO     F55GL-FN.
                 IF    SMTC OR SMDD                                     DOT
           MOVE        13544 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F55GL-900. GO TO F55GN-FN.
       F55GL-FN. EXIT.
      *N55GN.    NOTE *ANY OTHER IRA                      *.
       F55GN.                                                           lv35
                 IF    SMTC OR SMDD                                     DOT
           MOVE        13545 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
           MOVE        13547 TO WS00-NMESS2                             DOT
           PERFORM     F91OB THRU F91OB-FN.
       F55GN-FN. EXIT.
       F55GJ-FN. EXIT.
       F55GH-FN. EXIT.
       F55GF-FN. EXIT.
       F55GD-FN. EXIT.
       F55GB-FN. EXIT.
      *N55JB.    NOTE *IRA CONTRIBUTIONS                  *.
       F55JB.         EXIT.                                             lv10
      *N55JD.    NOTE *MOVE MONEY TO ACCOUNT              *.
       F55JD.    IF    MMTA                                             lv15
                 NEXT SENTENCE ELSE GO TO     F55JD-FN.
      *N55JF.    NOTE *FROM NON-QUALIFIED                 *.
       F55JF.    IF    FROM-NON-QUALIFIED                               lv20
                 NEXT SENTENCE ELSE GO TO     F55JF-FN.
      *N55JH.    NOTE *TO IRA                             *.
       F55JH.    IF    NOT TO-NON-QUALIFIED                             lv25
                 NEXT SENTENCE ELSE GO TO     F55JH-FN.
      *N55JJ.    NOTE *CASE OF CUSTODIAL CODE (IRA)       *.
       F55JJ.         EXIT.                                             lv30
      *N55JL.    NOTE *TO SEP                             *.
       F55JL.    IF    WS00-CTCUS2 =                                    lv35
                       008
                 NEXT SENTENCE ELSE GO TO     F55JL-FN.
                 IF    WS00-CLIENT-AGE < 50                             DOT
           MOVE        13610 TO WS00-NMESS2
                 ELSE
           MOVE        13516 TO WS00-NMESS2.
           PERFORM     F91OB THRU F91OB-FN.                             DOT
       F55JL-900. GO TO F55JJ-FN.
       F55JL-FN. EXIT.
      *N55JN.    NOTE *TO SRA                             *.
       F55JN.    IF    WS00-CTCUS2 =                                    lv35
                       010
                 NEXT SENTENCE ELSE GO TO     F55JN-FN.
                 IF    WS00-CLIENT-AGE < 50                             DOT
           MOVE        13611 TO WS00-NMESS2
                 ELSE
           MOVE        13515 TO WS00-NMESS2.
           PERFORM     F91OB THRU F91OB-FN.                             DOT
       F55JN-900. GO TO F55JJ-FN.
       F55JN-FN. EXIT.
      *N55JP.    NOTE *TO IRA ROLLOVER                    *.
       F55JP.    IF    WS00-CTCUS2 =                                    lv35
                       002
                 NEXT SENTENCE ELSE GO TO     F55JP-FN.
           MOVE        13612 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F55JP-900. GO TO F55JJ-FN.
       F55JP-FN. EXIT.
       F55JJ-FN. EXIT.
       F55JH-FN. EXIT.
       F55JF-FN. EXIT.
       F55JD-FN. EXIT.
       F55JB-FN. EXIT.
      *N55MB.    NOTE *CROSS ADMIN TRANSACTION            *.
       F55MB.    IF    MMTA                                             lv10
                 AND   V228-CTIDA NOT =
                       V228-CACTID
                 NEXT SENTENCE ELSE GO TO     F55MB-FN.
      *INDICATE A TWO DAY TURNAROUND
           MOVE        015236 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F55MB-FN. EXIT.
       F55-FN.   EXIT.
      *N57.      NOTE *************************************.
      *               *                                   *
      *               *ANNUITY SOURCE ACCOUNT             *
      *               *                                   *
      *               *************************************.
       F57.      IF    V228-CTIDA = 004 OR 005                          lv05
                 NEXT SENTENCE ELSE GO TO     F57-FN.
      *N57BB.    NOTE *MESSAGE FOR RBP AND RALP           *.
       F57BB.                                                           lv10
           MOVE        015480 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F57BB-FN. EXIT.
      *N57CB.    NOTE *SURRENDER CHARGE MESSAGE           *.
       F57CB.                                                           lv10
           MOVE        015481 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F57CB-FN. EXIT.
      *N57DB.    NOTE *MESSAGE FOR FLEX RIDER             *.
       F57DB.    IF    V228-CGMBR = 'X' OR 'Y'                          lv10
                 NEXT SENTENCE ELSE GO TO     F57DB-FN.
      *FOR ANNUITY SPO TRAN
           MOVE        015487 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F57DB-FN. EXIT.
       F57-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *INSURANCE DESTINATION ACCOUNTS,    *
      *               *                                   *
      *               *************************************.
       F60.                                                             lv05
      *LOAN PAYMENT FOR ALL ACCOUNTS
      *N60BA.    NOTE *INSURANCE DESTINATION ACCOUNTS     *.
       F60BA.    IF    V228-CACTID = (004 OR 005)                       lv10
                 AND   V228-PRCOD1 < 600
                 NEXT SENTENCE ELSE GO TO     F60BA-FN.
      *N60BB.    NOTE *IF THE ARRANGEMENT HAS BEEN        *.
       F60BB.    IF    V228-CPROCM = 'M'                                lv15
                 AND   ((V228-GEEND = ZEROES
                 AND   V228-GEEND2 > ZEROES)
                 OR    V228-CACTA = 'I')
                 AND   V228-MPMTT = 'REGULAR'
                 NEXT SENTENCE ELSE GO TO     F60BB-FN.
      *MODIFIED AND THE USER ENTERS THE
      *DATE OR INACTIVATES REGULAR
      *PAYMENT
      *
                 IF    V228-MPMTF = 'Monthly'                           DOT
      *FOR MONTHLY ARRANGEMENT
           MOVE        14151 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
                 IF    V228-MPMTF = 'Quarterly'                         DOT
      *FOR QUARTERLY ARRANGEMENT
           MOVE        14152 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
                 IF    V228-MPMTF = 'Semi-annual'                       DOT
      *FOR SEMI-ANNUAL ARRANGEMENT
           MOVE        14153 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
                 IF    V228-MPMTF = 'Annual'                            DOT
      *FOR ANNUAL ARRANGEMENT
           MOVE        14154 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F60BB-FN. EXIT.
      *N60BH.    NOTE *FOR AN ACTIVE DESTINATION          *.
       F60BH.    IF    V228-CACTA NOT = 'I'                             lv15
                 NEXT SENTENCE ELSE GO TO     F60BH-FN.
      *INSURANCE ACCOUNT ARRANGEMENT
      *CHECK THE GRACE PERIOD LIMITS
      *
                 IF    V228-ITRAL = 'Y'                                 DOT
                 AND   V228-GESTD > V228-ALDDUE
      *FOR A TRAD ACCOUNT IF THE START
      *DATE IS GREATER THAN PREMIUM DUE
           MOVE        14157 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
                 IF    V228-IGRACV = 'Y'                                DOT
      *FOR A VANTAGE ACCOUNT IF THE
           MOVE        14157 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F60BH-FN. EXIT.
      *N60BI.    NOTE *SET MESSAGE FOR IUL ACCOUNT        *.
       F60BI.                                                           lv15
      *
           INITIALIZE  TD8A
           MOVE        V228-CACTID TO TD8A-CTIDA
           MOVE        V228-PRCOD1 TO TD8A-PRCOD
           PERFORM     F92TD THRU F92TD-FN.
                 IF    TD8A-IIULA = 'Y'                                 DOT
      *IF THE DESTINATION IS IUL ACCT
           MOVE        15120 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F60BI-FN. EXIT.
       F60BA-FN. EXIT.
      *N60BK.    NOTE *FOR AN ACTIVE LOAN ARRANGEMENT     *.
       F60BK.    IF    V228-GEEND2 = ZEROES                             lv10
                 AND   V228-CACTA NOT = 'I'
                 AND   V228-MPMTT = 'LOAN'
                 NEXT SENTENCE ELSE GO TO     F60BK-FN.
      *IF THE END DATE IS ZEROES
           MOVE        14156 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F60BK-FN. EXIT.
       F60-FN.   EXIT.
      *N62.      NOTE *************************************.
      *               *                                   *
      *               *ANNUITY DESTINATION ACCOUNT        *
      *               *                                   *
      *               *************************************.
       F62.           EXIT.                                             lv05
      *N62BP.    NOTE *DESTINATION ACCOUNT IS ANNUITY     *.
       F62BP.    IF    V228-CACTID = (004 OR 005)                       lv10
                 AND   V228-PRCOD1 >= 600
                 NEXT SENTENCE ELSE GO TO     F62BP-FN.
           MOVE        15653 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F62BP-FN. EXIT.
       F62-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *PRINTING PURPOSE MESSAGE           *
      *               *                                   *
      *               *************************************.
       F65.                                                             lv05
      *ONLY IN SUBMT MODE
      *N65UF.    NOTE *MESSAGE ONLY ON UPDATE             *.
       F65UF.    IF    FROM-UPDATE                                      lv10
                 NEXT SENTENCE ELSE GO TO     F65UF-FN.
      *PRINT/COPY RECORD
      *PRINT/COPY RECORD
           MOVE        14114 TO WS00-NMESS2
           PERFORM     F91OB THRU F91OB-FN.
       F65UF-FN. EXIT.
       F65-FN.   EXIT.
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
      *               *MISCELLANEOUS ROUTINES             *
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
      *N91OB.    NOTE *ADD TRX MESSAGE CODE TO ARRAY      *.
       F91OB.    IF    IWS00L NOT > IWS00M                              lv10
                 NEXT SENTENCE ELSE GO TO     F91OB-FN.
           MOVE        WS00-NMESS2 TO V228-NMESS2 (IWS00L)
           MOVE        IWS00L TO V228-QITEM
           ADD         1 TO IWS00L.
       F91OB-FN. EXIT.
       F91SS.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012615 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F91SS-FN. EXIT.
       F91-FN.   EXIT.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *TABE ACCESS                        *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92TA.    NOTE *RANDOM TABLE READ FOR TA5A         *.            ADUTAB
       F92TA.                                                           lv10
           MOVE        'R1' TO G-TA5A-TABFO                             ADUTAB
           COMPUTE     G-TA5A-LTH = 60 + G-TA5A-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA5A-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA5A)                                ADUTAB
                       LENGTH (G-TA5A-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA5A-TABCR NOT = '00'                          DOT
      *ON ERROR
           INITIALIZE  TA5A.
       F92TA-FN. EXIT.
      *N92TD.    NOTE *RANDOM TABLE READ FOR TD8A         *.            ADUTAB
       F92TD.                                                           lv10
           MOVE        'R1' TO G-TD8A-TABFO                             ADUTAB
           COMPUTE     G-TD8A-LTH = 60 + G-TD8A-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TD8A-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TD8A)                                ADUTAB
                       LENGTH (G-TD8A-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TD8A-TABCR NOT = '00'                          DOT
      *ON ERROR
           INITIALIZE  TD8A.
       F92TD-FN. EXIT.
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
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *CALLED MODULES                     *
      *               *                                   *
      *               *************************************.
       F95.           EXIT.                                             lv05
      *N95BA.    NOTE *CALL MODULE DBI5000N TO READ       *.
       F95BA.                                                           lv10
      *ACCOUNT INFORMATION VIEW - 2933
           CALL        DBI5000N USING                                   ACLDBI
           DFHEIBLK DFHCOMMAREA                                         ACLDBI
           WL00-REQUEST SQ1L                                            ACLDBI
           WL00-RESPONSE                                                ACLDBI
           SQ2L SQ3L.                                                   ACLDBI
       F95BA-FN. EXIT.
      *N95CB.    NOTE *ARRANGMENT LIST FOR AN ACCOUNT     *.            AM0205
       F95CB.                                                           lv10
      *                                                                 AM0205
      *********************************                                 AM0205
      ** CALL THE DRIVER PROGRAM FOR  *                                 AM0205
      ** GET ARRANGEMENTS.            *                                 AM0205
      *********************************                                 AM0205
      *                                                                 AM0205
      *                                                                 AM0205
           SET CI0205-CB-PCB-CL1P-PTR1 TO                               AM0205
                         PCB-CL1P-PTR1                                  AM0205
           SET CI0205-CB-PCB-CT1P-PTR1 TO                               AM0205
                         PCB-CT1P-PTR1                                  AM0205
           SET CI0205-CB-PCB-AR1P-PTR1 TO                               AM0205
                         PCB-AR1P-PTR1                                  AM0205
           SET CI0205-CB-PCB-AREY-PTR1 TO                               AM0205
                         PCB-AREY-PTR1                                  AM0205
           SET CI0205-CB-PCB-ARAY-PTR1 TO                               AM0205
                         PCB-ARAY-PTR1                                  AM0205
           SET CI0205-CB-PCB-GR1P-PTR1 TO                               AM0205
                         PCB-GR1P-PTR1                                  AM0205
           SET CI0205-CB-PCB-SSPP-PTR1 TO                               AM0205
                         PCB-SSPP-PTR1                                  AM0205
           SET CI0205-CB-PCB-CA1P-PTR1 TO                               AM0205
                         PCB-CA1P-PTR1                                  AM0205
           SET CI0205-CB-PCB-TR1P-PTR1 TO                               AM0205
                         PCB-TR1P-PTR1                                  AM0205
           SET CI0205-CB-PCB-AR2P-PTR1 TO                               AM0205
                         PCB-AR1P-PTR1
           SET CI0205-CB-PCB-LH1P-PTR1 TO                               AM0205
                         PCB-LH1P-PTR1                                  AM0205
           SET CI0205-CB-PCB-LM1P-PTR1 TO                               AM0205
                         PCB-LM1P-PTR1                                  AM0205
           SET CI0205-CB-PCB-LUVP-PTR1 TO                               AM0205
                         PCB-LUVP-PTR1                                  AM0205
           SET CI0205-CB-PCB-LARP-PTR1 TO                               AM0205
                         PCB-LARP-PTR1                                  AM0205
           SET CI0205-CB-PCB-LPDP-PTR1 TO                               AM0205
                         PCB-LPDP-PTR1                                  AM0205
           CALL        CI0205 USING                                     AM0205
           DFHEIBLK                                                     AM0205
           DFHCOMMAREA                                                  AM0205
           DLIUIBII                                                     AM0205
           CI0205-CB-PCB-ADDR-LIST                                      AM0205
           CB00                                                         AM0205
           DE10                                                         AM0205
           MS03                                                         AM0205
           MX11.                                                        AM0205
      *N95CC.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F95CC.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F95CC-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0205 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0205 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F95CC-900. GO TO F95CD-FN.
       F95CC-FN. EXIT.
      *N95CD.    NOTE *NO ERRORS                          *.            ADU071
       F95CD.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F95CD-FN. EXIT.
      *N95CF.    NOTE *CHECK ARRANGEMENTS RESULT          *.
       F95CF.                                                           lv15
           MOVE        ZERO TO MS03-NMESS2
           MOVE        +1 TO WS00-QITEM.
      *N95CH.    NOTE *LOOP THRU ARRAY OF ARRANGEMENTS    *.
       F95CH.    IF    WS00-QITEM NOT > CB00-QITEM                      lv20
                 NEXT SENTENCE ELSE GO TO     F95CH-FN.
      *N95CJ.    NOTE *MOVE ARRAY TO WORKING STORAGE      *.
       F95CJ.                                                           lv25
           MOVE        CB00-CDEST (WS00-QITEM) TO
           WS00-CDEST
           MOVE        CB00-CARTZA (WS00-QITEM) TO
           WS00-CARTZA
      *INCREMENT INDEX
           ADD         +1 TO WS00-QITEM.
      *N95CL.    NOTE *ONLY ACTIVE ARRANGEMENT            *.
       F95CL.    IF    ACTIVE-ARRANGEMENT                               lv30
                 NEXT SENTENCE ELSE GO TO     F95CL-FN.
      *N95CN.    NOTE *CHECK ARRANGEMENT TYPE             *.
       F95CN.         EXIT.                                             lv35
      *N95CR.    NOTE *SET FUND PAYOUT SWITCH             *.
       F95CR.    IF    WS00-CARTZA =                                    lv40
                       'FP'
                 NEXT SENTENCE ELSE GO TO     F95CR-FN.
           MOVE        'Y' TO WS00-FP-SW.
       F95CR-900. GO TO F95CN-FN.
       F95CR-FN. EXIT.
      *N95CS.    NOTE *SET CERTIFICATE PAYOUT SWITCH      *.
       F95CS.    IF    WS00-CARTZA =                                    lv40
                       'CP'
                 NEXT SENTENCE ELSE GO TO     F95CS-FN.
           MOVE        'Y' TO WS00-CP-SW.
       F95CS-900. GO TO F95CN-FN.
       F95CS-FN. EXIT.
      *N95CT.    NOTE *SET MONEY COMING IN SWITCH         *.
       F95CT.    IF    WS00-CARTZA =                                    lv40
                       'MC'
                 NEXT SENTENCE ELSE GO TO     F95CT-FN.
           MOVE        'Y' TO WS00-MC-SW.
       F95CT-900. GO TO F95CN-FN.
       F95CT-FN. EXIT.
      *N95CY.    NOTE *SET DOLLAR COST AVERAGE SWITCH     *.
       F95CY.    IF    WS00-CARTZA =                                    lv40
                       'AD'
                 NEXT SENTENCE ELSE GO TO     F95CY-FN.
           MOVE        'Y' TO WS00-AD-SW.
       F95CY-900. GO TO F95CN-FN.
       F95CY-FN. EXIT.
      *N95CZ.    NOTE *SET ANN PARTIAL SURR ARR SWITCH    *.
       F95CZ.    IF    WS00-CARTZA =                                    lv40
                       'AP' OR 'AI'
                 NEXT SENTENCE ELSE GO TO     F95CZ-FN.
           MOVE        'Y' TO WS00-AP-SW.
       F95CZ-900. GO TO F95CN-FN.
       F95CZ-FN. EXIT.
       F95CN-FN. EXIT.
       F95CL-FN. EXIT.
       F95CJ-FN. EXIT.
       F95CH-900. GO TO F95CH.
       F95CH-FN. EXIT.
       F95CF-FN. EXIT.
       F95CB-FN. EXIT.
      *N95EA.    NOTE *GROUP BILL ARR LIST FOR AN ACCT    *.
       F95EA.         EXIT.                                             lv10
      *N95EB.    NOTE *CALL CI0975 TO GET ARRANGEMENT     *.            AM0975
       F95EB.                                                           lv15
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
      *N95EE.    NOTE *GET THE RESPONSE QUEUE FROM EODS   *.
       F95EE.    IF    (DA8G-CERRE = SPACES                             lv20
                 OR    DA8G-CERRE = ZEROES)
                 AND   MS03-NMESS2 = ZEROES
                 AND   DA8G-GERTC = 'Y'
                 NEXT SENTENCE ELSE GO TO     F95EE-FN.
      *SUCCESSFULLY
      *N95EF.    NOTE *IF CALL SERVICE SUCCESSFUL         *.
       F95EF.    IF    DA8G-CFAUL1 = SPACES                             lv25
                 AND   DA8G-TFACT1 = SPACES
                 NEXT SENTENCE ELSE GO TO     F95EF-FN.
      *
      *N95EG.    NOTE *GROUP BILL ARRANGEMENT RETURNED    *.
       F95EG.    IF    V228-CTID = DA8G-CTID (1)                        lv30
                 AND   DA8G-QITEM > 0
                 NEXT SENTENCE ELSE GO TO     F95EG-FN.
           MOVE        ZERO TO MS03-NMESS2
           MOVE        +1 TO WS00-QITEM.
      *N95FB.    NOTE *LOOP THRU ARRAY OF ARRANGEMENTS    *.
       F95FB.    IF    WS00-QITEM NOT > DA8G-QITEM                      lv35
                 NEXT SENTENCE ELSE GO TO     F95FB-FN.
      *N95FD.    NOTE *MOVE ARRAY TO WORKING STORAGE      *.
       F95FD.                                                           lv40
      *
                 IF    DA8G-CEBTP (WS00-QITEM) = '05'                   DOT
           MOVE        'MA' TO WS00-CARTZA.
                 IF    DA8G-CEBTP (WS00-QITEM)                          DOT
                       NOT = '06'
                 AND   DA8G-CEBTP (WS00-QITEM)
                       NOT = '05'
           MOVE        'GB' TO WS00-CARTZA.
           ADD         +1 TO WS00-QITEM.                                DOT
      *N95FF.    NOTE *CHECK ARRANGEMENT TYPE             *.
       F95FF.         EXIT.                                             lv45
      *N95FG.    NOTE *SET GROUP BILL SWITCH              *.
       F95FG.    IF    WS00-CARTZA =                                    lv50
                       'GB'
                 NEXT SENTENCE ELSE GO TO     F95FG-FN.
           MOVE        'Y' TO WS00-GB-SW.
       F95FG-900. GO TO F95FF-FN.
       F95FG-FN. EXIT.
      *N95FH.    NOTE *SET MILITARY ALLOTMENT SWITCH      *.
       F95FH.    IF    WS00-CARTZA =                                    lv50
                       'MA'
                 NEXT SENTENCE ELSE GO TO     F95FH-FN.
           MOVE        'Y' TO WS00-MA-SW.
       F95FH-900. GO TO F95FF-FN.
       F95FH-FN. EXIT.
       F95FF-FN. EXIT.
       F95FD-FN. EXIT.
       F95FB-900. GO TO F95FB.
       F95FB-FN. EXIT.
       F95EG-900. GO TO F95FJ-FN.
       F95EG-FN. EXIT.
      *N95FJ.    NOTE *NO GROUP BILL GET                  *.
       F95FJ.                                                           lv30
                 IF    DA8G-CFAUL2 = '0102'                             DOT
                 OR    DA8G-CFAUL3 = '1000'
      *REQUEST ACCOUNT NOT FOUND
      *NO RECORDS FOUND
           MOVE        ZERO TO MS03-NMESS2
                 ELSE
           MOVE        015017 TO MS03-NMESS2.
       F95FJ-FN. EXIT.
       F95EF-900. GO TO F95FK-FN.
       F95EF-FN. EXIT.
      *N95FK.    NOTE *CALL SERVICE FAILED                *.
       F95FK.                                                           lv25
      *MOVE THE ERROR MESSAGE
           MOVE        '015017' TO MS03-NMESS2.
       F95FK-FN. EXIT.
       F95EE-FN. EXIT.
      *N95FM.    NOTE *NON-DL1 ERROR OF A CERTAIN         *.
       F95FM.    IF    (MS03-NMESS2 > ZERO                              lv20
                 AND   MS03-CMESB > 10)
                 NEXT SENTENCE ELSE GO TO     F95FM-FN.
      *SEVERITY
      *
           COMPUTE     IMS03R = MS03-QELLAA + 2
           MOVE        CI0975 TO MS03-TMESS4 (IMS03R : 6)
           ADD         +7 TO MS03-QELLAA.
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F95FM-FN. EXIT.
       F95EB-FN. EXIT.
       F95EA-FN. EXIT.
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
