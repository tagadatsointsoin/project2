       IDENTIFICATION DIVISION.                                         CI0276
       PROGRAM-ID.  CI0276P.                                            CI0276
      *AUTHOR.         SST ARR UPDATES.                                 CI0276
      *DATE-COMPILED.   09/08/14.                                       CI0276
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 1999                          *ACOPYP
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
      *     COPR. 1999                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0276
       CONFIGURATION SECTION.                                           CI0276
       SOURCE-COMPUTER. IBM-370.                                        CI0276
       OBJECT-COMPUTER. IBM-370.                                        CI0276
       DATA DIVISION.                                                   CI0276
       WORKING-STORAGE SECTION.                                         CI0276
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
                        PICTURE S9(3)                                   CI0276
                          COMPUTATIONAL-3.                              CI0276
                                                                        ADU165
      *>>>>>>> Linkage Area for Logger Program DBI110                   ADU165
      *!WF DSP=DH DSL=DH SEL=10 FOR=I DES=2 LEV=1                       ADU165
       01                 DH10.                                         CI0276
            10            DH10-GERTC  PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            DH10-XUIBP  PICTURE  S9(8)                    CI0276
                          VALUE                ZERO                     CI0276
                          BINARY.                                       CI0276
            10            DH10-NSEQ2P PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            DH10-CAUL   PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            DH10-MAUSB  PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            DH10-NAUSK  PICTURE  X(50)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            DH10-CSYS   PICTURE  X(4)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            DH10-CAPPL  PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            DH10-CAUSR  PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            DH10-CAUFR  PICTURE  S9(5)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            DH10-CAUAC  PICTURE  S9(5)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            DH10-GEOPID PICTURE  X(6)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            DH10-CAUNIT PICTURE  X(4)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            DH10-GAUVR  PICTURE  X(400)                   CI0276
                          VALUE                SPACE.                   CI0276

       01  7-AOACFM-WORKAREA.
      *!WI
           05  7-AOACFM-CACKD
                        PICTURE 9.                                      CI0276
      *!WI
           05  7-AOACFM-CRETP
                        PICTURE X.                                      CI0276

       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0020           PIC X(8) VALUE 'CI0020P '.                  AM0020

      ***************************************************************
      **           WORK ARRANGEMENT SEGMENTS                        *
      ***************************************************************
      *BUILD AREA FOR CX13
      *!WF DSP=SA DSL=CX SEL=13 FOR=I DES=2 LEV=1 PLT=CX
       01                 SA13.                                         CI0276
            10            SA13-GELL   PICTURE  9(4)                     CI0276
                          VALUE                ZERO                     CI0276
                          BINARY.                                       CI0276
            10            SA13-CY20.                                    CI0276
            11            SA13-CX13K.                                   CI0276
            12            SA13-CARTZ  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            12            SA13-NAPDS  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            SA13-GESTD  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            11            SA13-GEEND  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            11            SA13-DASUQ  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            11            SA13-CDEST  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            11            SA13-IIARR  PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            11            SA13-DLAUP  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            11            SA13-GEOPD2 PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            11            SA13-GEAUN  PICTURE  9(5)                     CI0276
                          VALUE                ZERO.                    CI0276
            11            SA13-DPCHD  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            11            SA13-PPOT1  PICTURE  S9(3)V99                 CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            SA13-ACOT1  PICTURE  S9(9)V99                 CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            SA13-QPST1  PICTURE  S9(7)V999                CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            SA13-FILLER PICTURE  X(03)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            SA13-CY96.                                    CI0276
            11            SA13-FILLER PICTURE  X(50)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            SA13-CY21                                     CI0276
                          REDEFINES            SA13-CY96.               CI0276
            11            SA13-DNPMT  PICTURE  9(8).                    CI0276
            11            SA13-CPMTF  PICTURE  99.                      CI0276
            11            SA13-ADBRQ  PICTURE  S9(11)V99                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            SA13-QSHOWQ PICTURE  S9(9)V999                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            SA13-PACT1  PICTURE  S999V999                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            SA13-DOPDA  PICTURE  99.                      CI0276
            11            SA13-DNEXE  PICTURE  9(8).                    CI0276
            11            SA13-CIRMO  PICTURE  X(12).                   CI0276
            10            SA13-CY98.                                    CI0276
            11            SA13-FILLER PICTURE  X(120)                   CI0276
                          VALUE                SPACE.                   CI0276
            10            SA13-CY25                                     CI0276
                          REDEFINES            SA13-CY98.               CI0276
            11            SA13-COPTC  PICTURE  9(1).                    CI0276
            11            SA13-ILPOI  PICTURE  X(1).                    CI0276
            11            SA13-CATOC  PICTURE  X(1).                    CI0276
            11            SA13-CEOIA  PICTURE  S9(7)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            SA13-ACOAR  PICTURE  S9(9)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            SA13-CEOTR  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            SA13-DSTMO  PICTURE  99.                      CI0276
            10            SA13-CY27                                     CI0276
                          REDEFINES            SA13-CY98.               CI0276
            11            SA13-QMTH1  PICTURE  9(3).                    CI0276
            11            SA13-IDRMD  PICTURE  X.                       CI0276
            10            SA13-CY28                                     CI0276
                          REDEFINES            SA13-CY98.               CI0276
            11            SA13-AALLBL PICTURE  S9(8)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            SA13-PSURR  PICTURE  S9(3)V999                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            SA13-DFPMT  PICTURE  9(8).                    CI0276
            11            SA13-QMTHLA PICTURE  9(3).                    CI0276
            11            SA13-PWHLDS PICTURE  S999V9(5)                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            SA13-ISWHO  PICTURE  X(1).                    CI0276
            10            SA13-CY29                                     CI0276
                          REDEFINES            SA13-CY98.               CI0276
            11            SA13-IINDI1 PICTURE  X(1).                    CI0276
            11            SA13-IINDI2 PICTURE  X(1).                    CI0276
            11            SA13-IINDI3 PICTURE  X(1).                    CI0276
            11            SA13-PWHLD5 PICTURE  S999V99                  CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            SA13-CCSMQ  PICTURE  X.                       CI0276
            11            SA13-CPLEC  PICTURE  XX.                      CI0276
            11            SA13-IPTRDA PICTURE  X(01).                   CI0276
            11            SA13-GCUSPY PICTURE  X(12).                   CI0276
            11            SA13-ALOIDA PICTURE  S9(11)V99                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            SA13-DELOI  PICTURE  9(8).                    CI0276
            11            SA13-CLGND  PICTURE  X.                       CI0276
            11            SA13-CORTYA PICTURE  X(3).                    CI0276
            11            SA13-CPH3U  PICTURE  X.                       CI0276
            11            SA13-CNAVR  PICTURE  X(1).                    CI0276
            11            SA13-NEXEC  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
       01                 SA15.                                         CI0276
            10            SA15-CX15K.                                   CI0276
            11            SA15-CFIDC  PICTURE  X(5)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            SA15-ACOTD  PICTURE  S9(9)V99                 CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            SA15-PPOTD  PICTURE  S9(3)V99                 CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            SA15-QPSTD  PICTURE  S9(7)V999                CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            SA15-FILLER PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
      *BUILD AREA FOR CX15
      *!WF DSP=SA DSL=CX SEL=15 FOR=I DES=2 LEV=1 PLT=CX

      *BUILD AREA FOR CX13 AUDIT LOG SEGMENT
      *!WF DSP=VA DSL=VA SEL=13 FOR=I DES=2 LEV=1 PLT=CX
       01                 VA13.                                         CI0276
            10            VA13-K11A.                                    CI0276
            11            VA13-CX03K.                                   CI0276
            12            VA13-CARTY  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            12            VA13-NARRS  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-CX06K.                                   CI0276
            12            VA13-CTIDA1 PICTURE  9(3)                     CI0276
                          VALUE                ZERO.                    CI0276
            12            VA13-NACID1 PICTURE  X(24)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            VA13-CY20.                                    CI0276
            11            VA13-CX13K.                                   CI0276
            12            VA13-CARTZ  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            12            VA13-NAPDS  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-GESTD  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            11            VA13-GEEND  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            11            VA13-DASUQ  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            11            VA13-CDEST  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            11            VA13-IIARR  PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            11            VA13-DLAUP  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            11            VA13-GEOPD2 PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            11            VA13-GEAUN  PICTURE  9(5)                     CI0276
                          VALUE                ZERO.                    CI0276
            11            VA13-DPCHD  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            11            VA13-PPOT1  PICTURE  S9(3)V99                 CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-ACOT1  PICTURE  S9(9)V99                 CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-QPST1  PICTURE  S9(7)V999                CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-FILLER PICTURE  X(03)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            VA13-CY96.                                    CI0276
            11            VA13-FILLER PICTURE  X(50)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            VA13-CY21                                     CI0276
                          REDEFINES            VA13-CY96.               CI0276
            11            VA13-DNPMT  PICTURE  9(8).                    CI0276
            11            VA13-CPMTF  PICTURE  99.                      CI0276
            11            VA13-ADBRQ  PICTURE  S9(11)V99                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-QSHOWQ PICTURE  S9(9)V999                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-PACT1  PICTURE  S999V999                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-DOPDA  PICTURE  99.                      CI0276
            11            VA13-DNEXE  PICTURE  9(8).                    CI0276
            11            VA13-CIRMO  PICTURE  X(12).                   CI0276
            10            VA13-CY98.                                    CI0276
            11            VA13-FILLER PICTURE  X(120)                   CI0276
                          VALUE                SPACE.                   CI0276
            10            VA13-CY25                                     CI0276
                          REDEFINES            VA13-CY98.               CI0276
            11            VA13-COPTC  PICTURE  9(1).                    CI0276
            11            VA13-ILPOI  PICTURE  X(1).                    CI0276
            11            VA13-CATOC  PICTURE  X(1).                    CI0276
            11            VA13-CEOIA  PICTURE  S9(7)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-ACOAR  PICTURE  S9(9)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-CEOTR  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-DSTMO  PICTURE  99.                      CI0276
            10            VA13-CY27                                     CI0276
                          REDEFINES            VA13-CY98.               CI0276
            11            VA13-QMTH1  PICTURE  9(3).                    CI0276
            11            VA13-IDRMD  PICTURE  X.                       CI0276
            10            VA13-CY28                                     CI0276
                          REDEFINES            VA13-CY98.               CI0276
            11            VA13-AALLBL PICTURE  S9(8)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-PSURR  PICTURE  S9(3)V999                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-DFPMT  PICTURE  9(8).                    CI0276
            11            VA13-QMTHLA PICTURE  9(3).                    CI0276
            11            VA13-PWHLDS PICTURE  S999V9(5)                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-ISWHO  PICTURE  X(1).                    CI0276
            10            VA13-CY29                                     CI0276
                          REDEFINES            VA13-CY98.               CI0276
            11            VA13-IINDI1 PICTURE  X(1).                    CI0276
            11            VA13-IINDI2 PICTURE  X(1).                    CI0276
            11            VA13-IINDI3 PICTURE  X(1).                    CI0276
            11            VA13-PWHLD5 PICTURE  S999V99                  CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-CCSMQ  PICTURE  X.                       CI0276
            11            VA13-CPLEC  PICTURE  XX.                      CI0276
            11            VA13-IPTRDA PICTURE  X(01).                   CI0276
            11            VA13-GCUSPY PICTURE  X(12).                   CI0276
            11            VA13-ALOIDA PICTURE  S9(11)V99                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA13-DELOI  PICTURE  9(8).                    CI0276
            11            VA13-CLGND  PICTURE  X.                       CI0276
            11            VA13-CORTYA PICTURE  X(3).                    CI0276
            11            VA13-CPH3U  PICTURE  X.                       CI0276
            11            VA13-CNAVR  PICTURE  X(1).                    CI0276
            11            VA13-NEXEC  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
       01                 VA14.                                         CI0276
            10            VA14-K11A.                                    CI0276
            11            VA14-CX03K.                                   CI0276
            12            VA14-CARTY  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            12            VA14-NARRS  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA14-CX06K.                                   CI0276
            12            VA14-CTIDA1 PICTURE  9(3)                     CI0276
                          VALUE                ZERO.                    CI0276
            12            VA14-NACID1 PICTURE  X(24)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            VA14-CX13K.                                   CI0276
            11            VA14-CARTZ  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            11            VA14-NAPDS  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            VA14-CX14K.                                   CI0276
            11            VA14-NPISQ  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            VA14-ACOTD  PICTURE  S9(9)V99                 CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            VA14-PPOTD  PICTURE  S9(3)V99                 CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            VA14-QPSTD  PICTURE  S9(7)V999                CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            VA14-CPITC  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            10            VA14-FILLER PICTURE  X(32)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            VA14-CY30                                     CI0276
                          REDEFINES            VA14-FILLER.             CI0276
            11            VA14-IOWNC  PICTURE  X.                       CI0276
            11            VA14-CTYPE  PICTURE  X.                       CI0276
            11            VA14-C299.                                    CI0276
            12            VA14-CTID.                                    CI0276
            13            VA14-CTIDA  PICTURE  9(3).                    CI0276
            13            VA14-CTIDN.                                   CI0276
            14            VA14-CTIDNP PICTURE  X(13).                   CI0276
            14            VA14-CTIDND PICTURE  9(11).                   CI0276
            11            VA14-CPMTC  PICTURE  99.                      CI0276
            11            VA14-IACSD  PICTURE  X.                       CI0276
            10            VA14-CY31                                     CI0276
                          REDEFINES            VA14-FILLER.             CI0276
            11            VA14-FILLER PICTURE  X(2).                    CI0276
            11            VA14-IDELI  PICTURE  X.                       CI0276
            11            VA14-CDEL1  PICTURE  9(3).                    CI0276
            11            VA14-NDELS  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            VA14-CY32                                     CI0276
                          REDEFINES            VA14-FILLER.             CI0276
            11            VA14-GCUSPZ PICTURE  X(12).                   CI0276
       01                 VA15.                                         CI0276
            10            VA15-K11A.                                    CI0276
            11            VA15-CX03K.                                   CI0276
            12            VA15-CARTY  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            12            VA15-NARRS  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA15-CX06K.                                   CI0276
            12            VA15-CTIDA1 PICTURE  9(3)                     CI0276
                          VALUE                ZERO.                    CI0276
            12            VA15-NACID1 PICTURE  X(24)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            VA15-CX13K.                                   CI0276
            11            VA15-CARTZ  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            11            VA15-NAPDS  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            VA15-CX14K.                                   CI0276
            11            VA15-NPISQ  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            VA15-CX15K.                                   CI0276
            11            VA15-CFIDC  PICTURE  X(5)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            VA15-ACOTD  PICTURE  S9(9)V99                 CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            VA15-PPOD1  PICTURE  S9(3)V999                CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            VA15-QPSTD  PICTURE  S9(7)V999                CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
       01                 VA17.                                         CI0276
            10            VA17-K11A.                                    CI0276
            11            VA17-CX03K.                                   CI0276
            12            VA17-CARTY  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            12            VA17-NARRS  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            VA17-CX06K.                                   CI0276
            12            VA17-CTIDA1 PICTURE  9(3)                     CI0276
                          VALUE                ZERO.                    CI0276
            12            VA17-NACID1 PICTURE  X(24)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            VA17-CX13K.                                   CI0276
            11            VA17-CARTZ  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            11            VA17-NAPDS  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            VA17-CX17K.                                   CI0276
            11            VA17-CFIDC  PICTURE  X(5)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            VA17-ADBRQF PICTURE  S9(9)V99                 CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            VA17-PACT1  PICTURE  S999V999                 CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            VA17-QSHOWQ PICTURE  S9(9)V999                CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
      *BUILD AREA FOR CX14 AUDIT LOG SEGMENT
      *!WF DSP=VA DSL=VA SEL=14 FOR=I DES=2 LEV=1 PLT=CX
      *BUILD AREA FOR CX15 AUDIT LOG SEGMENT
      *!WF DSP=VA DSL=VA SEL=15 FOR=I DES=2 LEV=1 PLT=CX
      *BUILD AREA FOR CX17 AUDIT LOG SEGMENT
      *!WF DSP=VA DSL=VA SEL=17 FOR=I DES=2 LEV=1 PLT=CX
      *
      *BUILD AREA FOR FROM ACCOUNTS CX17'S
       01                 7-WX17-TABLE.
           02             WX17-ITEM
                          OCCURS 99.
      *!WF DSP=WX DSL=CX SEL=17 FOR=I LEV=3 PLT=CX
            03            WX00.                                         CI0276
          05              WX00-SUITE.                                   CI0276
            15       FILLER         PICTURE  X(00030).                  CI0276
            03            WX17  REDEFINES      WX00.                    CI0276
            10            WX17-CX17K.                                   CI0276
            11            WX17-CFIDC  PICTURE  X(5).                    CI0276
            10            WX17-ADBRQF PICTURE  S9(9)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            WX17-PACT1  PICTURE  S999V999                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            WX17-QSHOWQ PICTURE  S9(9)V999                CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            WX17-FILLER PICTURE  X(8).                    CI0276
      *
       01                 CX01.                                         CI0276
            10            CX01-CX01K.                                   CI0276
            11            CX01-C199.                                    CI0276
            12            CX01-CLID.                                    CI0276
            13            CX01-CLIDO  PICTURE  9(3).                    CI0276
            13            CX01-CLIDN.                                   CI0276
            14            CX01-CLIDNP PICTURE  X(12).                   CI0276
            14            CX01-CLIDND PICTURE  9(8).                    CI0276
            10            CX01-GEMDA  PICTURE  9(8).                    CI0276
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0276
                          BINARY.                                       CI0276
            10            CX01-FILLER PICTURE  X(5).                    CI0276
       01                 CX03.                                         CI0276
            10            CX03-GELL   PICTURE  9(4)                     CI0276
                          BINARY.                                       CI0276
            10            CX03-CY00.                                    CI0276
            11            CX03-CX03K.                                   CI0276
            12            CX03-CARTY  PICTURE  99.                      CI0276
            12            CX03-NARRS  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX03-CARST  PICTURE  99.                      CI0276
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX03-CPMTG  PICTURE  99.                      CI0276
            11            CX03-GRCRNG PICTURE  9(3).                    CI0276
            11            CX03-DEXDT  PICTURE  9(8).                    CI0276
            11            CX03-DASUP  PICTURE  9(8).                    CI0276
            11            CX03-CSTEC  PICTURE  X(3).                    CI0276
            11            CX03-FILLER PICTURE  X(17).                   CI0276
            11            CX03-CY50.                                    CI0276
            12            CX03-NARID  PICTURE  X(30).                   CI0276
            11            CX03-CY51                                     CI0276
                          REDEFINES            CX03-CY50.               CI0276
            12            CX03-NDIDN  PICTURE  9(12).                   CI0276
            12            CX03-FILLER PICTURE  X(18).                   CI0276
            11            CX03-CY52                                     CI0276
                          REDEFINES            CX03-CY50.               CI0276
            12            CX03-NAIDC  PICTURE  9(12).                   CI0276
            12            CX03-FILLER PICTURE  X(18).                   CI0276
            11            CX03-CY53                                     CI0276
                          REDEFINES            CX03-CY50.               CI0276
            12            CX03-NAMEXB PICTURE  9(15).                   CI0276
            12            CX03-FILLER PICTURE  X(15).                   CI0276
            10            CX03-CY99.                                    CI0276
            11            CX03-FILLER PICTURE  X(109).                  CI0276
            10            CX03-CY01                                     CI0276
                          REDEFINES            CX03-CY99.               CI0276
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX03-ICPCI  PICTURE  X.                       CI0276
            11            CX03-CLUPD  PICTURE  9(3).                    CI0276
            11            CX03-DLAUP  PICTURE  9(8).                    CI0276
            11            CX03-CWRC   PICTURE  99.                      CI0276
            11            CX03-CHCR   PICTURE  99.                      CI0276
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0276
            11            CX03-GEAUN  PICTURE  9(5).                    CI0276
            11            CX03-DPCHD  PICTURE  9(8).                    CI0276
            11            CX03-DLRCHK PICTURE  9(8).                    CI0276
            11            CX03-QTRCHK PICTURE  9(2).                    CI0276
            11            CX03-DNPMT  PICTURE  9(8).                    CI0276
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            CX03-CY02                                     CI0276
                          REDEFINES            CX03-CY99.               CI0276
            11            CX03-QSIRQ  PICTURE  99.                      CI0276
            11            CX03-QDRMN  PICTURE  9(2)                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX03-DDPRE  PICTURE  9(8).                    CI0276
            11            CX03-DDSHP  PICTURE  9(8).                    CI0276
            11            CX03-NDRFTB PICTURE  9(5).                    CI0276
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0276
            11            CX03-DDSHPA PICTURE  9(8).                    CI0276
            11            CX03-NDRFTF PICTURE  9(5).                    CI0276
            11            CX03-QDIPBK PICTURE  9(3).                    CI0276
            11            CX03-CREOR  PICTURE  X(1).                    CI0276
            11            CX03-CREOR1 PICTURE  X(1).                    CI0276
            11            CX03-DDASC  PICTURE  9(8).                    CI0276
            11            CX03-FILLER PICTURE  X(7).                    CI0276
            10            CX03-CY03                                     CI0276
                          REDEFINES            CX03-CY99.               CI0276
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0276
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0276
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0276
            11            CX03-DOPDA  PICTURE  99.                      CI0276
            11            CX03-CPMTF  PICTURE  99.                      CI0276
            11            CX03-CIRMO  PICTURE  X(12).                   CI0276
            11            CX03-CPALL  PICTURE  X(1).                    CI0276
            11            CX03-CCOLM  PICTURE  9(2).                    CI0276
            11            CX03-CBLTP  PICTURE  X(1).                    CI0276
            11            CX03-CASUB  PICTURE  9(2).                    CI0276
            11            CX03-CBLFM  PICTURE  9(2).                    CI0276
            11            CX03-IBILS  PICTURE  X.                       CI0276
            11            CX03-IPAOS  PICTURE  X.                       CI0276
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0276
            11            CX03-DLBPD  PICTURE  9(8).                    CI0276
            11            CX03-DNBPD  PICTURE  9(8).                    CI0276
            11            CX03-DODBD  PICTURE  9(8).                    CI0276
            11            CX03-CPSRE  PICTURE  99.                      CI0276
            11            CX03-ISPHN  PICTURE  X.                       CI0276
            11            CX03-TCARR  PICTURE  X(6).                    CI0276
            11            CX03-CBKPT  PICTURE  9(2).                    CI0276
            11            CX03-IECNT  PICTURE  X.                       CI0276
            11            CX03-ICONV  PICTURE  X(1).                    CI0276
            11            CX03-FILLER PICTURE  X(4).                    CI0276
            10            CX03-CY04                                     CI0276
                          REDEFINES            CX03-CY99.               CI0276
            11            CX03-CCARD  PICTURE  X(02).                   CI0276
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0276
            11            CX03-IREMT  PICTURE  X(01).                   CI0276
            11            CX03-ISBILA PICTURE  X.                       CI0276
            11            CX03-DLBPDA PICTURE  9(8).                    CI0276
            11            CX03-DNBPDA.                                  CI0276
            12            CX03-DNCYM  PICTURE  9(6).                    CI0276
            12            CX03-CEDTD  PICTURE  9(2).                    CI0276
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX03-DREMT  PICTURE  9(8).                    CI0276
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0276
            11            CX03-CWRC2  PICTURE  99.                      CI0276
            11            CX03-CHCR2  PICTURE  99.                      CI0276
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0276
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0276
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0276
       01                 CX06.                                         CI0276
            10            CX06-CX06K.                                   CI0276
            11            CX06-C299.                                    CI0276
            12            CX06-CTID.                                    CI0276
            13            CX06-CTIDA  PICTURE  9(3).                    CI0276
            13            CX06-CTIDN.                                   CI0276
            14            CX06-CTIDNP PICTURE  X(13).                   CI0276
            14            CX06-CTIDND PICTURE  9(11).                   CI0276
            10            CX06-NPECK  PICTURE  9(02).                   CI0276
            10            CX06-FILLER PICTURE  X.                       CI0276
       01                 CX13.                                         CI0276
            10            CX13-GELL   PICTURE  9(4)                     CI0276
                          BINARY.                                       CI0276
            10            CX13-CY20.                                    CI0276
            11            CX13-CX13K.                                   CI0276
            12            CX13-CARTZ  PICTURE  99.                      CI0276
            12            CX13-NAPDS  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX13-GESTD  PICTURE  9(8).                    CI0276
            11            CX13-GEEND  PICTURE  9(8).                    CI0276
            11            CX13-DASUQ  PICTURE  9(8).                    CI0276
            11            CX13-CDEST  PICTURE  99.                      CI0276
            11            CX13-IIARR  PICTURE  X.                       CI0276
            11            CX13-DLAUP  PICTURE  9(8).                    CI0276
            11            CX13-GEOPD2 PICTURE  X(8).                    CI0276
            11            CX13-GEAUN  PICTURE  9(5).                    CI0276
            11            CX13-DPCHD  PICTURE  9(8).                    CI0276
            11            CX13-PPOT1  PICTURE  S9(3)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX13-ACOT1  PICTURE  S9(9)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX13-QPST1  PICTURE  S9(7)V999                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX13-FILLER PICTURE  X(03).                   CI0276
            10            CX13-CY96.                                    CI0276
            11            CX13-FILLER PICTURE  X(50).                   CI0276
            10            CX13-CY21                                     CI0276
                          REDEFINES            CX13-CY96.               CI0276
            11            CX13-DNPMT  PICTURE  9(8).                    CI0276
            11            CX13-CPMTF  PICTURE  99.                      CI0276
            11            CX13-ADBRQ  PICTURE  S9(11)V99                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX13-QSHOWQ PICTURE  S9(9)V999                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX13-PACT1  PICTURE  S999V999                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX13-DOPDA  PICTURE  99.                      CI0276
            11            CX13-DNEXE  PICTURE  9(8).                    CI0276
            11            CX13-CIRMO  PICTURE  X(12).                   CI0276
            10            CX13-CY98.                                    CI0276
            11            CX13-FILLER PICTURE  X(120).                  CI0276
            10            CX13-CY25                                     CI0276
                          REDEFINES            CX13-CY98.               CI0276
            11            CX13-COPTC  PICTURE  9(1).                    CI0276
            11            CX13-ILPOI  PICTURE  X(1).                    CI0276
            11            CX13-CATOC  PICTURE  X(1).                    CI0276
            11            CX13-CEOIA  PICTURE  S9(7)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX13-ACOAR  PICTURE  S9(9)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX13-CEOTR  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX13-DSTMO  PICTURE  99.                      CI0276
            10            CX13-CY27                                     CI0276
                          REDEFINES            CX13-CY98.               CI0276
            11            CX13-QMTH1  PICTURE  9(3).                    CI0276
            11            CX13-IDRMD  PICTURE  X.                       CI0276
            10            CX13-CY28                                     CI0276
                          REDEFINES            CX13-CY98.               CI0276
            11            CX13-AALLBL PICTURE  S9(8)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX13-PSURR  PICTURE  S9(3)V999                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX13-DFPMT  PICTURE  9(8).                    CI0276
            11            CX13-QMTHLA PICTURE  9(3).                    CI0276
            11            CX13-PWHLDS PICTURE  S999V9(5)                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX13-ISWHO  PICTURE  X(1).                    CI0276
            10            CX13-CY29                                     CI0276
                          REDEFINES            CX13-CY98.               CI0276
            11            CX13-IINDI1 PICTURE  X(1).                    CI0276
            11            CX13-IINDI2 PICTURE  X(1).                    CI0276
            11            CX13-IINDI3 PICTURE  X(1).                    CI0276
            11            CX13-PWHLD5 PICTURE  S999V99                  CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX13-CCSMQ  PICTURE  X.                       CI0276
            11            CX13-CPLEC  PICTURE  XX.                      CI0276
            11            CX13-IPTRDA PICTURE  X(01).                   CI0276
            11            CX13-GCUSPY PICTURE  X(12).                   CI0276
            11            CX13-ALOIDA PICTURE  S9(11)V99                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            CX13-DELOI  PICTURE  9(8).                    CI0276
            11            CX13-CLGND  PICTURE  X.                       CI0276
            11            CX13-CORTYA PICTURE  X(3).                    CI0276
            11            CX13-CPH3U  PICTURE  X.                       CI0276
            11            CX13-CNAVR  PICTURE  X(1).                    CI0276
            11            CX13-NEXEC  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
       01                 CX14.                                         CI0276
            10            CX14-GELL   PICTURE  9(4)                     CI0276
                          BINARY.                                       CI0276
            10            CX14-CX14K.                                   CI0276
            11            CX14-NPISQ  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            CX14-ACOTD  PICTURE  S9(9)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            CX14-PPOTD  PICTURE  S9(3)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            CX14-QPSTD  PICTURE  S9(7)V999                CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            CX14-CPITC  PICTURE  99.                      CI0276
            10            CX14-FILLER PICTURE  X(04).                   CI0276
            10            CX14-CY97.                                    CI0276
            11            CX14-FILLER PICTURE  X(32).                   CI0276
            10            CX14-CY30                                     CI0276
                          REDEFINES            CX14-CY97.               CI0276
            11            CX14-IOWNC  PICTURE  X.                       CI0276
            11            CX14-CTYPE  PICTURE  X.                       CI0276
            11            CX14-C299.                                    CI0276
            12            CX14-CTID.                                    CI0276
            13            CX14-CTIDA  PICTURE  9(3).                    CI0276
            13            CX14-CTIDN.                                   CI0276
            14            CX14-CTIDNP PICTURE  X(13).                   CI0276
            14            CX14-CTIDND PICTURE  9(11).                   CI0276
            11            CX14-CPMTC  PICTURE  99.                      CI0276
            11            CX14-IACSD  PICTURE  X.                       CI0276
            10            CX14-CY31                                     CI0276
                          REDEFINES            CX14-CY97.               CI0276
            11            CX14-FILLER PICTURE  X(2).                    CI0276
            11            CX14-IDELI  PICTURE  X.                       CI0276
            11            CX14-CDEL1  PICTURE  9(3).                    CI0276
            11            CX14-NDELS  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            CX14-CY32                                     CI0276
                          REDEFINES            CX14-CY97.               CI0276
            11            CX14-GCUSPZ PICTURE  X(12).                   CI0276
       01                 CX15.                                         CI0276
            10            CX15-CX15K.                                   CI0276
            11            CX15-CFIDC  PICTURE  X(5).                    CI0276
            10            CX15-ACOTD  PICTURE  S9(9)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            CX15-PPOTD  PICTURE  S9(3)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            CX15-QPSTD  PICTURE  S9(7)V999                CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            CX15-FILLER PICTURE  X(8).                    CI0276
       01                 CX17.                                         CI0276
            10            CX17-CX17K.                                   CI0276
            11            CX17-CFIDC  PICTURE  X(5).                    CI0276
            10            CX17-ADBRQF PICTURE  S9(9)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            CX17-PACT1  PICTURE  S999V999                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            CX17-QSHOWQ PICTURE  S9(9)V999                CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            CX17-FILLER PICTURE  X(8).                    CI0276
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0276
            10            XW05-XW06.                                    CI0276
            11            XW05-XDBPCB.                                  CI0276
            12            XW05-XDBDNM PICTURE  X(08)                    CI0276
                          VALUE                SPACE.                   CI0276
            12            XW05-XSEGLV PICTURE  X(02)                    CI0276
                          VALUE                SPACE.                   CI0276
            12            XW05-XRC    PICTURE  X(02)                    CI0276
                          VALUE                SPACE.                   CI0276
            12            XW05-XPROPT PICTURE  X(04)                    CI0276
                          VALUE                SPACE.                   CI0276
            12            XW05-FILLER PICTURE  S9(5)                    CI0276
                          VALUE                ZERO                     CI0276
                          BINARY.                                       CI0276
            12            XW05-XSEGNM PICTURE  X(08)                    CI0276
                          VALUE                SPACE.                   CI0276
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0276
                          VALUE                ZERO                     CI0276
                          BINARY.                                       CI0276
            12            XW05-XSEGNB PICTURE  9(05)                    CI0276
                          VALUE                ZERO                     CI0276
                          BINARY.                                       CI0276
            12            XW05-XCOKEY PICTURE  X(70)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            XW05-XW07.                                    CI0276
            11            XW05-XIOPCB.                                  CI0276
            12            XW05-XTERMI PICTURE  X(08)                    CI0276
                          VALUE                SPACE.                   CI0276
            12            XW05-FILLER PICTURE  XX                       CI0276
                          VALUE                SPACE.                   CI0276
            12            XW05-XRC1   PICTURE  X(02)                    CI0276
                          VALUE                SPACE.                   CI0276
            12            XW05-FILLER PICTURE  X(12)                    CI0276
                          VALUE                SPACE.                   CI0276
            12            XW05-XMODNM PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0276
                          VALUE                ZERO.                    CI0276
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0276
                          VALUE                ZERO.                    CI0276
            10            XW05-XGU    PICTURE  X(4)                     CI0276
                          VALUE                'GU  '.                  CI0276
            10            XW05-XGHU   PICTURE  X(4)                     CI0276
                          VALUE                'GHU '.                  CI0276
            10            XW05-XGN    PICTURE  X(4)                     CI0276
                          VALUE                'GN  '.                  CI0276
            10            XW05-XGHN   PICTURE  X(4)                     CI0276
                          VALUE                'GHN '.                  CI0276
            10            XW05-XGNP   PICTURE  X(4)                     CI0276
                          VALUE                'GNP '.                  CI0276
            10            XW05-XGHNP  PICTURE  X(4)                     CI0276
                          VALUE                'GHNP'.                  CI0276
            10            XW05-XREPL  PICTURE  XXXX                     CI0276
                          VALUE                'REPL'.                  CI0276
            10            XW05-XISRT  PICTURE  X(4)                     CI0276
                          VALUE                'ISRT'.                  CI0276
            10            XW05-XDLET  PICTURE  X(4)                     CI0276
                          VALUE                'DLET'.                  CI0276
            10            XW05-XOPEN  PICTURE  X(4)                     CI0276
                          VALUE                'OPEN'.                  CI0276
            10            XW05-XCLSE  PICTURE  X(4)                     CI0276
                          VALUE                'CLSE'.                  CI0276
            10            XW05-XCHKP  PICTURE  X(4)                     CI0276
                          VALUE                'CHKP'.                  CI0276
            10            XW05-XXRST  PICTURE  X(4)                     CI0276
                          VALUE                'XRST'.                  CI0276
            10            XW05-XTERM  PICTURE  X(4)                     CI0276
                          VALUE                'TERM'.                  CI0276
            10            XW05-XNFPAC PICTURE  X(13)                    CI0276
                          VALUE                SPACE.                   CI0276
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0276
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0276
       01                 GQ01.                                         CI0276
            10            GQ01-GELL   PICTURE  9(4)                     CI0276
                          BINARY.                                       CI0276
            10            GQ01-GMISC.                                   CI0276
            11            GQ01-GS00.                                    CI0276
            12            GQ01-GT01.                                    CI0276
            13            GQ01-GQ01K.                                   CI0276
            14            GQ01-CANUMB PICTURE  X(27).                   CI0276
            14            GQ01-CAMCTR PICTURE  9(5).                    CI0276
            14            GQ01-GESQ2  PICTURE  99.                      CI0276
            12            GQ01-GT02                                     CI0276
                          REDEFINES            GQ01-GT01.               CI0276
            13            GQ01-C199.                                    CI0276
            14            GQ01-CLID.                                    CI0276
            15            GQ01-CLIDO  PICTURE  9(3).                    CI0276
            15            GQ01-CLIDN.                                   CI0276
            16            GQ01-CLIDNP PICTURE  X(12).                   CI0276
            16            GQ01-CLIDND PICTURE  9(8).                    CI0276
            12            GQ01-GT03                                     CI0276
                          REDEFINES            GQ01-GT01.               CI0276
            13            GQ01-C299.                                    CI0276
            14            GQ01-CTID.                                    CI0276
            15            GQ01-CTIDA  PICTURE  9(3).                    CI0276
            15            GQ01-CTIDN.                                   CI0276
            16            GQ01-CTIDNP PICTURE  X(13).                   CI0276
            16            GQ01-CTIDND PICTURE  9(11).                   CI0276
            12            GQ01-GT04                                     CI0276
                          REDEFINES            GQ01-GT01.               CI0276
            13            GQ01-NPBN   PICTURE  X(20).                   CI0276
            12            GQ01-GT05                                     CI0276
                          REDEFINES            GQ01-GT01.               CI0276
            13            GQ01-GR98.                                    CI0276
            14            GQ01-GRID.                                    CI0276
            15            GQ01-GRIDC  PICTURE  9(3).                    CI0276
            15            GQ01-GRIDN.                                   CI0276
            16            GQ01-GRIDNP PICTURE  99.                      CI0276
            16            GQ01-GRIDND PICTURE  9(8).                    CI0276
            12            GQ01-GT06                                     CI0276
                          REDEFINES            GQ01-GT01.               CI0276
            13            GQ01-NTR    PICTURE  9(8).                    CI0276
            12            GQ01-GT07                                     CI0276
                          REDEFINES            GQ01-GT01.               CI0276
            13            GQ01-NTRAC  PICTURE  9(14).                   CI0276
            12            GQ01-GT08                                     CI0276
                          REDEFINES            GQ01-GT01.               CI0276
            13            GQ01-NSRAB  PICTURE  9(7).                    CI0276
            13            GQ01-GECKD  PICTURE  9.                       CI0276
            13            GQ01-NBLCK  PICTURE  9(5).                    CI0276
            13            GQ01-CTRID  PICTURE  X(4).                    CI0276
            12            GQ01-GT19                                     CI0276
                          REDEFINES            GQ01-GT01.               CI0276
            13            GQ01-GEOPD2 PICTURE  X(8).                    CI0276
            12            GQ01-CACKD  PICTURE  9.                       CI0276
            12            GQ01-CENTT  PICTURE  X.                       CI0276
            12            GQ01-CADATE PICTURE  X(8).                    CI0276
            12            GQ01-GETIM  PICTURE  S9(7)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            12            GQ01-GEOPID PICTURE  X(6).                    CI0276
            12            GQ01-CAUNIT PICTURE  X(4).                    CI0276
            12            GQ01-XTERMI PICTURE  X(08).                   CI0276
            12            GQ01-CAPPL  PICTURE  X(8).                    CI0276
            12            GQ01-CSYS   PICTURE  X(4).                    CI0276
            12            GQ01-NTRSU  PICTURE  999.                     CI0276
            12            GQ01-FILLER PICTURE  X(20).                   CI0276
            11            GQ01-XMISL  PICTURE  X(599).                  CI0276
      ******************************************************************
      **           MISCELLANEOUS TRAN WORK AREAS                       *
      ******************************************************************
      *!WF DSP=GS DSL=GS SEL=51 FOR=I DES=2 LEV=1                       ADU034
       01                 GS00.                                         CI0276
            10            GS00-GT01.                                    CI0276
            11            GS00-GQ01K.                                   CI0276
            12            GS00-CANUMB PICTURE  X(27)                    CI0276
                          VALUE                SPACE.                   CI0276
            12            GS00-CAMCTR PICTURE  9(5)                     CI0276
                          VALUE                ZERO.                    CI0276
            12            GS00-GESQ2  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            10            GS00-GT02                                     CI0276
                          REDEFINES            GS00-GT01.               CI0276
            11            GS00-C199.                                    CI0276
            12            GS00-CLID.                                    CI0276
            13            GS00-CLIDO  PICTURE  9(3).                    CI0276
            13            GS00-CLIDN.                                   CI0276
            14            GS00-CLIDNP PICTURE  X(12).                   CI0276
            14            GS00-CLIDND PICTURE  9(8).                    CI0276
            10            GS00-GT03                                     CI0276
                          REDEFINES            GS00-GT01.               CI0276
            11            GS00-C299.                                    CI0276
            12            GS00-CTID.                                    CI0276
            13            GS00-CTIDA  PICTURE  9(3).                    CI0276
            13            GS00-CTIDN.                                   CI0276
            14            GS00-CTIDNP PICTURE  X(13).                   CI0276
            14            GS00-CTIDND PICTURE  9(11).                   CI0276
            10            GS00-GT04                                     CI0276
                          REDEFINES            GS00-GT01.               CI0276
            11            GS00-NPBN   PICTURE  X(20).                   CI0276
            10            GS00-GT05                                     CI0276
                          REDEFINES            GS00-GT01.               CI0276
            11            GS00-GR98.                                    CI0276
            12            GS00-GRID.                                    CI0276
            13            GS00-GRIDC  PICTURE  9(3).                    CI0276
            13            GS00-GRIDN.                                   CI0276
            14            GS00-GRIDNP PICTURE  99.                      CI0276
            14            GS00-GRIDND PICTURE  9(8).                    CI0276
            10            GS00-GT06                                     CI0276
                          REDEFINES            GS00-GT01.               CI0276
            11            GS00-NTR    PICTURE  9(8).                    CI0276
            10            GS00-GT07                                     CI0276
                          REDEFINES            GS00-GT01.               CI0276
            11            GS00-NTRAC  PICTURE  9(14).                   CI0276
            10            GS00-GT08                                     CI0276
                          REDEFINES            GS00-GT01.               CI0276
            11            GS00-NSRAB  PICTURE  9(7).                    CI0276
            11            GS00-GECKD  PICTURE  9.                       CI0276
            11            GS00-NBLCK  PICTURE  9(5).                    CI0276
            11            GS00-CTRID  PICTURE  X(4).                    CI0276
            10            GS00-GT19                                     CI0276
                          REDEFINES            GS00-GT01.               CI0276
            11            GS00-GEOPD2 PICTURE  X(8).                    CI0276
            10            GS00-CACKD  PICTURE  9                        CI0276
                          VALUE                ZERO.                    CI0276
            10            GS00-CENTT  PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            GS00-CADATE PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            GS00-GETIM  PICTURE  S9(7)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            GS00-GEOPID PICTURE  X(6)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            GS00-CAUNIT PICTURE  X(4)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            GS00-XTERMI PICTURE  X(08)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            GS00-CAPPL  PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            GS00-CSYS   PICTURE  X(4)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            GS00-NTRSU  PICTURE  999                      CI0276
                          VALUE                ZERO.                    CI0276
            10            GS00-FILLER PICTURE  X(20)                    CI0276
                          VALUE                SPACE.                   CI0276
       01                 GS1A.                                         CI0276
            10            GS1A-NGEOPA PICTURE  X(08)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            GS1A-CACLS1 PICTURE  X(20)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            GS1A-CTRHO  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            10            GS1A-GETIM3 PICTURE  S9(7)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            GS1A-CSLCT  PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            GS1A-GEOPD9 PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            GS1A-GETIM  PICTURE  S9(7)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            GS1A-DCACG1 PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            10            GS1A-GEOPD2 PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            GS1A-DCACG  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            10            GS1A-GETIM2 PICTURE  S9(7)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            GS1A-CAVER  PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            GS1A-IWEBBT PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            GS1A-FILLER PICTURE  X(17)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            GS1A-NGEOR  PICTURE  9(08)                    CI0276
                          VALUE                ZERO.                    CI0276
            10            GS1A-CACLS2 PICTURE  X(20)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            GS1A-CAPID  PICTURE  9(2)                     CI0276
                          VALUE                ZERO.                    CI0276
            10            GS1A-CX01K.                                   CI0276
            11            GS1A-C199.                                    CI0276
            12            GS1A-CLID.                                    CI0276
            13            GS1A-CLIDO  PICTURE  9(3)                     CI0276
                          VALUE                ZERO.                    CI0276
            13            GS1A-CLIDN.                                   CI0276
            14            GS1A-CLIDNP PICTURE  X(12)                    CI0276
                          VALUE                SPACE.                   CI0276
            14            GS1A-CLIDND PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            10            GS1A-CX03K.                                   CI0276
            11            GS1A-CARTY  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            11            GS1A-NARRS  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            GS1A-CX06K.                                   CI0276
            11            GS1A-C299.                                    CI0276
            12            GS1A-CTID.                                    CI0276
            13            GS1A-CTIDA  PICTURE  9(3)                     CI0276
                          VALUE                ZERO.                    CI0276
            13            GS1A-CTIDN.                                   CI0276
            14            GS1A-CTIDNP PICTURE  X(13)                    CI0276
                          VALUE                SPACE.                   CI0276
            14            GS1A-CTIDND PICTURE  9(11)                    CI0276
                          VALUE                ZERO.                    CI0276
            10            GS1A-CX12K.                                   CI0276
            11            GS1A-CPMTC  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            11            GS1A-NAPDS  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            GS1A-GESTD  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            10            GS1A-CX13K.                                   CI0276
            11            GS1A-CARTZ  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            11            GS1A-NAPDS  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
       01                 GS51.                                         CI0276
            10            GS51-CRETP  PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            GS51-ICSWD  PICTURE  X(1)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            GS51-FILLER PICTURE  X(09)                    CI0276
                          VALUE                SPACE.                   CI0276
      *!WF DSP=GS DSL=GS SEL=1A FOR=I DES=2 LEV=1 PLT=GS
      *!WF DSP=WS DSL=GS SEL=1A FOR=I DES=2 LEV=1 PLT=GS
       01                 WS00.                                         CI0276
            10            WS00-GT01.                                    CI0276
            11            WS00-GQ01K.                                   CI0276
            12            WS00-CANUMB PICTURE  X(27)                    CI0276
                          VALUE                SPACE.                   CI0276
            12            WS00-CAMCTR PICTURE  9(5)                     CI0276
                          VALUE                ZERO.                    CI0276
            12            WS00-GESQ2  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            10            WS00-GT02                                     CI0276
                          REDEFINES            WS00-GT01.               CI0276
            11            WS00-C199.                                    CI0276
            12            WS00-CLID.                                    CI0276
            13            WS00-CLIDO  PICTURE  9(3).                    CI0276
            13            WS00-CLIDN.                                   CI0276
            14            WS00-CLIDNP PICTURE  X(12).                   CI0276
            14            WS00-CLIDND PICTURE  9(8).                    CI0276
            10            WS00-GT03                                     CI0276
                          REDEFINES            WS00-GT01.               CI0276
            11            WS00-C299.                                    CI0276
            12            WS00-CTID.                                    CI0276
            13            WS00-CTIDA  PICTURE  9(3).                    CI0276
            13            WS00-CTIDN.                                   CI0276
            14            WS00-CTIDNP PICTURE  X(13).                   CI0276
            14            WS00-CTIDND PICTURE  9(11).                   CI0276
            10            WS00-GT04                                     CI0276
                          REDEFINES            WS00-GT01.               CI0276
            11            WS00-NPBN   PICTURE  X(20).                   CI0276
            10            WS00-GT05                                     CI0276
                          REDEFINES            WS00-GT01.               CI0276
            11            WS00-GR98.                                    CI0276
            12            WS00-GRID.                                    CI0276
            13            WS00-GRIDC  PICTURE  9(3).                    CI0276
            13            WS00-GRIDN.                                   CI0276
            14            WS00-GRIDNP PICTURE  99.                      CI0276
            14            WS00-GRIDND PICTURE  9(8).                    CI0276
            10            WS00-GT06                                     CI0276
                          REDEFINES            WS00-GT01.               CI0276
            11            WS00-NTR    PICTURE  9(8).                    CI0276
            10            WS00-GT07                                     CI0276
                          REDEFINES            WS00-GT01.               CI0276
            11            WS00-NTRAC  PICTURE  9(14).                   CI0276
            10            WS00-GT08                                     CI0276
                          REDEFINES            WS00-GT01.               CI0276
            11            WS00-NSRAB  PICTURE  9(7).                    CI0276
            11            WS00-GECKD  PICTURE  9.                       CI0276
            11            WS00-NBLCK  PICTURE  9(5).                    CI0276
            11            WS00-CTRID  PICTURE  X(4).                    CI0276
            10            WS00-GT19                                     CI0276
                          REDEFINES            WS00-GT01.               CI0276
            11            WS00-GEOPD2 PICTURE  X(8).                    CI0276
            10            WS00-CACKD  PICTURE  9                        CI0276
                          VALUE                ZERO.                    CI0276
            10            WS00-CENTT  PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            WS00-CADATE PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            WS00-GETIM  PICTURE  S9(7)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            WS00-GEOPID PICTURE  X(6)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            WS00-CAUNIT PICTURE  X(4)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            WS00-XTERMI PICTURE  X(08)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            WS00-CAPPL  PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            WS00-CSYS   PICTURE  X(4)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            WS00-NTRSU  PICTURE  999                      CI0276
                          VALUE                ZERO.                    CI0276
            10            WS00-FILLER PICTURE  X(20)                    CI0276
                          VALUE                SPACE.                   CI0276
       01                 WS1A.                                         CI0276
            10            WS1A-NGEOPA PICTURE  X(08)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            WS1A-CACLS1 PICTURE  X(20)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            WS1A-CTRHO  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            10            WS1A-GETIM3 PICTURE  S9(7)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            WS1A-CSLCT  PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            WS1A-GEOPD9 PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            WS1A-GETIM  PICTURE  S9(7)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            WS1A-DCACG1 PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            10            WS1A-GEOPD2 PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            WS1A-DCACG  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            10            WS1A-GETIM2 PICTURE  S9(7)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            WS1A-CAVER  PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            WS1A-IWEBBT PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            WS1A-FILLER PICTURE  X(17)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            WS1A-NGEOR  PICTURE  9(08)                    CI0276
                          VALUE                ZERO.                    CI0276
            10            WS1A-CACLS2 PICTURE  X(20)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            WS1A-CAPID  PICTURE  9(2)                     CI0276
                          VALUE                ZERO.                    CI0276
            10            WS1A-CX01K.                                   CI0276
            11            WS1A-C199.                                    CI0276
            12            WS1A-CLID.                                    CI0276
            13            WS1A-CLIDO  PICTURE  9(3)                     CI0276
                          VALUE                ZERO.                    CI0276
            13            WS1A-CLIDN.                                   CI0276
            14            WS1A-CLIDNP PICTURE  X(12)                    CI0276
                          VALUE                SPACE.                   CI0276
            14            WS1A-CLIDND PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            10            WS1A-CX03K.                                   CI0276
            11            WS1A-CARTY  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            11            WS1A-NARRS  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            WS1A-CX06K.                                   CI0276
            11            WS1A-C299.                                    CI0276
            12            WS1A-CTID.                                    CI0276
            13            WS1A-CTIDA  PICTURE  9(3)                     CI0276
                          VALUE                ZERO.                    CI0276
            13            WS1A-CTIDN.                                   CI0276
            14            WS1A-CTIDNP PICTURE  X(13)                    CI0276
                          VALUE                SPACE.                   CI0276
            14            WS1A-CTIDND PICTURE  9(11)                    CI0276
                          VALUE                ZERO.                    CI0276
            10            WS1A-CX12K.                                   CI0276
            11            WS1A-CPMTC  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            11            WS1A-NAPDS  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            WS1A-GESTD  PICTURE  9(8)                     CI0276
                          VALUE                ZERO.                    CI0276
            10            WS1A-CX13K.                                   CI0276
            11            WS1A-CARTZ  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            11            WS1A-NAPDS  PICTURE  S9(3)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276

      ******************************************************************
      **  MISCELLANEOUS TRANSACTION BUILD AREA                       ***
      ******************************************************************
      *!WF DSP=KY DSL=KY FOR=I LEV=1 PLT=KY
       01                 KY00.                                         CI0276
          05              KY00-00.                                      CI0276
            10            KY00-GS00.                                    CI0276
            11            KY00-GT01.                                    CI0276
            12            KY00-GQ01K.                                   CI0276
            13            KY00-CANUMB PICTURE  X(27).                   CI0276
            13            KY00-CAMCTR PICTURE  9(5).                    CI0276
            13            KY00-GESQ2  PICTURE  99.                      CI0276
            11            KY00-GT02                                     CI0276
                          REDEFINES            KY00-GT01.               CI0276
            12            KY00-C199.                                    CI0276
            13            KY00-CLID.                                    CI0276
            14            KY00-CLIDO  PICTURE  9(3).                    CI0276
            14            KY00-CLIDN.                                   CI0276
            15            KY00-CLIDNP PICTURE  X(12).                   CI0276
            15            KY00-CLIDND PICTURE  9(8).                    CI0276
            11            KY00-GT03                                     CI0276
                          REDEFINES            KY00-GT01.               CI0276
            12            KY00-C299.                                    CI0276
            13            KY00-CTID.                                    CI0276
            14            KY00-CTIDA  PICTURE  9(3).                    CI0276
            14            KY00-CTIDN.                                   CI0276
            15            KY00-CTIDNP PICTURE  X(13).                   CI0276
            15            KY00-CTIDND PICTURE  9(11).                   CI0276
            11            KY00-GT04                                     CI0276
                          REDEFINES            KY00-GT01.               CI0276
            12            KY00-NPBN   PICTURE  X(20).                   CI0276
            11            KY00-GT05                                     CI0276
                          REDEFINES            KY00-GT01.               CI0276
            12            KY00-GR98.                                    CI0276
            13            KY00-GRID.                                    CI0276
            14            KY00-GRIDC  PICTURE  9(3).                    CI0276
            14            KY00-GRIDN.                                   CI0276
            15            KY00-GRIDNP PICTURE  99.                      CI0276
            15            KY00-GRIDND PICTURE  9(8).                    CI0276
            11            KY00-GT06                                     CI0276
                          REDEFINES            KY00-GT01.               CI0276
            12            KY00-NTR    PICTURE  9(8).                    CI0276
            11            KY00-GT07                                     CI0276
                          REDEFINES            KY00-GT01.               CI0276
            12            KY00-NTRAC  PICTURE  9(14).                   CI0276
            11            KY00-GT08                                     CI0276
                          REDEFINES            KY00-GT01.               CI0276
            12            KY00-NSRAB  PICTURE  9(7).                    CI0276
            12            KY00-GECKD  PICTURE  9.                       CI0276
            12            KY00-NBLCK  PICTURE  9(5).                    CI0276
            12            KY00-CTRID  PICTURE  X(4).                    CI0276
            11            KY00-GT19                                     CI0276
                          REDEFINES            KY00-GT01.               CI0276
            12            KY00-GEOPD2 PICTURE  X(8).                    CI0276
            11            KY00-CACKD  PICTURE  9.                       CI0276
            11            KY00-CENTT  PICTURE  X.                       CI0276
            11            KY00-CADATE PICTURE  X(8).                    CI0276
            11            KY00-GETIM  PICTURE  S9(7)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            KY00-GEOPID PICTURE  X(6).                    CI0276
            11            KY00-CAUNIT PICTURE  X(4).                    CI0276
            11            KY00-XTERMI PICTURE  X(08).                   CI0276
            11            KY00-CAPPL  PICTURE  X(8).                    CI0276
            11            KY00-CSYS   PICTURE  X(4).                    CI0276
            11            KY00-NTRSU  PICTURE  999.                     CI0276
            11            KY00-FILLER PICTURE  X(20).                   CI0276
          05              KY00-SUITE.                                   CI0276
            15       FILLER         PICTURE  X(00599).                  CI0276
       01                 KY11  REDEFINES      KY00.                    CI0276
            10       FILLER         PICTURE  X(00101).                  CI0276
            10            KY11-GS11.                                    CI0276
            11            KY11-IBASU  PICTURE  X.                       CI0276
            11            KY11-CTRLR  PICTURE  9.                       CI0276
            11            KY11-CDEST  PICTURE  99.                      CI0276
            11            KY11-APMT   PICTURE  S9(5)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            KY11-CPMTC  PICTURE  99.                      CI0276
            11            KY11-CPMTF  PICTURE  99.                      CI0276
            11            KY11-DNPMT  PICTURE  9(8).                    CI0276
            10       FILLER         PICTURE  X(00579).                  CI0276
       01                 KY12  REDEFINES      KY00.                    CI0276
            10       FILLER         PICTURE  X(00101).                  CI0276
            10            KY12-GS12.                                    CI0276
            11            KY12-CINQD  PICTURE  X.                       CI0276
            10       FILLER         PICTURE  X(00598).                  CI0276
       01                 KY13  REDEFINES      KY00.                    CI0276
            10       FILLER         PICTURE  X(00101).                  CI0276
            10            KY13-GS13.                                    CI0276
            11            KY13-CDIRE  PICTURE  99.                      CI0276
            11            KY13-APBC2  PICTURE  9(8)V99                  CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            KY13-NBTCH8 PICTURE  9(8).                    CI0276
            11            KY13-IPACR  PICTURE  X.                       CI0276
            10            KY13-FILLER PICTURE  X(582).                  CI0276
       01                 KY52  REDEFINES      KY00.                    CI0276
            10       FILLER         PICTURE  X(00101).                  CI0276
            10            KY52-NAIDC  PICTURE  9(12).                   CI0276
            10            KY52-FILLER PICTURE  X(18).                   CI0276
            10       FILLER         PICTURE  X(00569).                  CI0276
      *
      *
      *GENERATE INDEX FOR MESSAGE TEXT (ONLY IF 'OCCURS' SPECIFIED)     ADU070
      *                   MS03                                          ADU070
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
       01                 NS00.                                         CI0276
          05              NS00-00.                                      CI0276
            10            NS00-NS00K.                                   CI0276
            11            NS00-PRCSTK PICTURE  XX.                      CI0276
          05              NS00-SUITE.                                   CI0276
            15       FILLER         PICTURE  X(00078).                  CI0276
       01                 NS20  REDEFINES      NS00.                    CI0276
            10       FILLER         PICTURE  X(00002).                  CI0276
            10            NS20-DCACG  PICTURE  9(8).                    CI0276
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            NS20-CCDAT  PICTURE  X(8).                    CI0276
            10            NS20-DCALP  PICTURE  X(12).                   CI0276
            10            NS20-DNACG  PICTURE  9(8).                    CI0276
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            NS20-CNDAT  PICTURE  X(8).                    CI0276
            10            NS20-DNALP  PICTURE  X(12).                   CI0276
            10            NS20-DCACD  PICTURE  X(10).                   CI0276
            10            NS20-FILLER PICTURE  X(4).                    CI0276
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      ******************************************************************ADUTAB
      **              TABLE TA75 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA75-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=75 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA75.                                                CI0276
           04    G-TA75-PARAM.                                          CI0276
             10  G-TA75-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0276
                        VALUE      +060.                                CI0276
             10  G-TA75-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0276
                        VALUE      +001.                                CI0276
             10  G-TA75-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0276
                        VALUE      +005.                                CI0276
             10  G-TA75-NUAPP  PICTURE 99                               CI0276
                        VALUE       0.                                  CI0276
             10  G-TA75-NUTAB  PICTURE X(6)                             CI0276
                        VALUE 'CAMCTR'.                                 CI0276
             10  G-TA75-TABFO  PICTURE XX                 VALUE SPACE.  CI0276
             10  G-TA75-TABCR  PICTURE XX                 VALUE SPACE.  CI0276
             10  G-TA75-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0276
             10  G-TA75-NUSSC  PICTURE X  VALUE   ' '.                  CI0276
             10  G-TA75-NUSSY  PICTURE X                  VALUE SPACE.  CI0276
             10  G-TA75-TRANID PICTURE X(4)               VALUE SPACE.  CI0276
             10  G-TA75-FILSYS.                                         CI0276
             15  G-TA75-USERC  PICTURE X(6)               VALUE SPACE.  CI0276
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0276
           04             TA75.                                         CI0276
            10            TA75-CAMCTR PICTURE  9(5)                     CI0276
                          VALUE                ZERO.                    CI0276
            10            TA75-TTDES  PICTURE  X(36)                    CI0276
                          VALUE                SPACE.                   CI0276
            10            TA75-MSYSID PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            10            TA75-NDLEN  PICTURE  S9(4)                    CI0276
                          VALUE                ZERO.                    CI0276
            10            TA75-IMIND1 PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            TA75-IMIND2 PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            TA75-IMIND3 PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            TA75-IMIND5 PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            TA75-IMIND7 PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            TA75-IMIND8 PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            10            TA75-IMINE  PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
      **                                                                ADUTAB
       01                 WQ01.                                         CI0276
            10            WQ01-GELL   PICTURE  9(4)                     CI0276
                          VALUE                ZERO                     CI0276
                          BINARY.                                       CI0276
            10            WQ01-GMISC.                                   CI0276
            11            WQ01-GS00.                                    CI0276
            12            WQ01-GT01.                                    CI0276
            13            WQ01-GQ01K.                                   CI0276
            14            WQ01-CANUMB PICTURE  X(27)                    CI0276
                          VALUE                SPACE.                   CI0276
            14            WQ01-CAMCTR PICTURE  9(5)                     CI0276
                          VALUE                ZERO.                    CI0276
            14            WQ01-GESQ2  PICTURE  99                       CI0276
                          VALUE                ZERO.                    CI0276
            12            WQ01-GT02                                     CI0276
                          REDEFINES            WQ01-GT01.               CI0276
            13            WQ01-C199.                                    CI0276
            14            WQ01-CLID.                                    CI0276
            15            WQ01-CLIDO  PICTURE  9(3).                    CI0276
            15            WQ01-CLIDN.                                   CI0276
            16            WQ01-CLIDNP PICTURE  X(12).                   CI0276
            16            WQ01-CLIDND PICTURE  9(8).                    CI0276
            12            WQ01-GT03                                     CI0276
                          REDEFINES            WQ01-GT01.               CI0276
            13            WQ01-C299.                                    CI0276
            14            WQ01-CTID.                                    CI0276
            15            WQ01-CTIDA  PICTURE  9(3).                    CI0276
            15            WQ01-CTIDN.                                   CI0276
            16            WQ01-CTIDNP PICTURE  X(13).                   CI0276
            16            WQ01-CTIDND PICTURE  9(11).                   CI0276
            12            WQ01-GT04                                     CI0276
                          REDEFINES            WQ01-GT01.               CI0276
            13            WQ01-NPBN   PICTURE  X(20).                   CI0276
            12            WQ01-GT05                                     CI0276
                          REDEFINES            WQ01-GT01.               CI0276
            13            WQ01-GR98.                                    CI0276
            14            WQ01-GRID.                                    CI0276
            15            WQ01-GRIDC  PICTURE  9(3).                    CI0276
            15            WQ01-GRIDN.                                   CI0276
            16            WQ01-GRIDNP PICTURE  99.                      CI0276
            16            WQ01-GRIDND PICTURE  9(8).                    CI0276
            12            WQ01-GT06                                     CI0276
                          REDEFINES            WQ01-GT01.               CI0276
            13            WQ01-NTR    PICTURE  9(8).                    CI0276
            12            WQ01-GT07                                     CI0276
                          REDEFINES            WQ01-GT01.               CI0276
            13            WQ01-NTRAC  PICTURE  9(14).                   CI0276
            12            WQ01-GT08                                     CI0276
                          REDEFINES            WQ01-GT01.               CI0276
            13            WQ01-NSRAB  PICTURE  9(7).                    CI0276
            13            WQ01-GECKD  PICTURE  9.                       CI0276
            13            WQ01-NBLCK  PICTURE  9(5).                    CI0276
            13            WQ01-CTRID  PICTURE  X(4).                    CI0276
            12            WQ01-GT19                                     CI0276
                          REDEFINES            WQ01-GT01.               CI0276
            13            WQ01-GEOPD2 PICTURE  X(8).                    CI0276
            12            WQ01-CACKD  PICTURE  9                        CI0276
                          VALUE                ZERO.                    CI0276
            12            WQ01-CENTT  PICTURE  X                        CI0276
                          VALUE                SPACE.                   CI0276
            12            WQ01-CADATE PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            12            WQ01-GETIM  PICTURE  S9(7)                    CI0276
                          VALUE                ZERO                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            12            WQ01-GEOPID PICTURE  X(6)                     CI0276
                          VALUE                SPACE.                   CI0276
            12            WQ01-CAUNIT PICTURE  X(4)                     CI0276
                          VALUE                SPACE.                   CI0276
            12            WQ01-XTERMI PICTURE  X(08)                    CI0276
                          VALUE                SPACE.                   CI0276
            12            WQ01-CAPPL  PICTURE  X(8)                     CI0276
                          VALUE                SPACE.                   CI0276
            12            WQ01-CSYS   PICTURE  X(4)                     CI0276
                          VALUE                SPACE.                   CI0276
            12            WQ01-NTRSU  PICTURE  999                      CI0276
                          VALUE                ZERO.                    CI0276
            12            WQ01-FILLER PICTURE  X(20)                    CI0276
                          VALUE                SPACE.                   CI0276
            11            WQ01-XMISL  PICTURE  X(599)                   CI0276
                          VALUE                SPACE.                   CI0276
      *** WORK FIELDS **
       01  7-WK-DATE-WORKAREA.
          05  7-WK-DNPMT.
              10  FILLER             PIC 99      VALUE ZERO.
              10  7-WK-DYY           PIC 99      VALUE ZERO.
              10  7-WK-DMM           PIC 99      VALUE ZERO.
              10  FILLER             PIC 99      VALUE ZERO.
          05  7-WK-ALPLDT.
              10  FILLER             PIC 99      VALUE ZERO.
              10  7-WK-AYY           PIC 99      VALUE ZERO.
              10  7-WK-AMM           PIC 99      VALUE ZERO.
              10  FILLER             PIC 99      VALUE ZERO.

      *!WI
       01  7-CX15-ACOTD VALUE ZERO
                        PICTURE S9(9)V99                                CI0276
                          COMPUTATIONAL-3.                              CI0276
      *!WI
       01  7-HOLD-ADBCRQ VALUE ZERO
                        PICTURE S9(11)V99                               CI0276
                          COMPUTATIONAL-3.                              CI0276

      ** TOTAL OF ALL SUB ACCOUNTS
      *!WI
       01  WS-QSACT  VALUE 0
                        PICTURE 9(3).                                   CI0276

       01  SUB1    PIC 9(03) VALUE 0.
       01  IDX     PIC S9(05) VALUE 0.
       01  IDX1    PIC S9(05) VALUE 0.

       01  CX14-FOUND  PIC X(1) VALUE 'N'.
       01  CX15-FOUND  PIC X(1) VALUE 'N'.
       01  CX17-FOUND  PIC X(1) VALUE 'N'.
       01  CHECK-TO-OWNER  PIC X(1) VALUE 'N'.
      ***
       01 7-ALLOWABLE-FREQ.
      *!WI
          05  7-WS-CPMTF
                        PICTURE 99.                                     CI0276
              88  ANNUITY-FREQ    VALUE 04, 06, 12, 24, 26, 52.
      **
       01  7-WS-DATE.
           05 7-WS-DTCCYY   PIC 9(04).
           05 7-WS-DTMM     PIC 9(02).
           05 7-WS-DTDD     PIC 9(02).
       01 WS-CF       PIC X(1) VALUE '0'.
      *!WI
       01 WS-CX01K
                        PICTURE X(23).                                  CI0276
      *!WI
       01 WS-CX03K
                        PICTURE X(4).                                   CI0276
      *!WI
       01 WS-CX06K
                        PICTURE X(27).                                  CI0276
      *!WI
       01 WS-CX13K
                        PICTURE X(04).                                  CI0276
      *!WI
       01 WS-GELL
                        PICTURE 9(4)                                    CI0276
                          BINARY.                                       CI0276
       01   DEBUT-WSS.                                                  CI0276
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0276
            05   IK     PICTURE X.                                      CI0276
       01  CONSTANTES-PAC.                                              CI0276
           05  FILLER  PICTURE X(87)   VALUE                            CI0276
                     '6015 CAT09/08/14CI0276ADMIN   14:35:11CI0276P AMERCI0276
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0276
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0276
           05  NUGNA   PICTURE X(5).                                    CI0276
           05  APPLI   PICTURE X(3).                                    CI0276
           05  DATGN   PICTURE X(8).                                    CI0276
           05  PROGR   PICTURE X(6).                                    CI0276
           05  CODUTI  PICTURE X(8).                                    CI0276
           05  TIMGN   PICTURE X(8).                                    CI0276
           05  PROGE   PICTURE X(8).                                    CI0276
           05  COBASE  PICTURE X(4).                                    CI0276
           05  DATGNC  PICTURE X(10).                                   CI0276
           05  RELEAS  PICTURE X(7).                                    CI0276
           05  DATGE   PICTURE X(10).                                   CI0276
           05  DATSQ   PICTURE X(10).                                   CI0276
       01  DATCE.                                                       CI0276
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0276
         05  DATOR.                                                     CI0276
           10  DATOA  PICTURE XX.                                       CI0276
           10  DATOM  PICTURE XX.                                       CI0276
           10  DATOJ  PICTURE XX.                                       CI0276
       01   VARIABLES-CONDITIONNELLES.                                  CI0276
            05                  FT      PICTURE X VALUE '0'.            CI0276
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0276
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0276
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU070
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           IWX17L PICTURE S9(4) VALUE  ZERO.
            05           IWX17R PICTURE S9(4) VALUE  ZERO.
            05           IWX17M PICTURE S9(4) VALUE +0099.
            05           J35LGR PICTURE S9(4) VALUE  ZERO.
            05           J45FDR PICTURE S9(4) VALUE  ZERO.
            05           J50LBR PICTURE S9(4) VALUE  ZERO.
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0276
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0276
            05       5-GQ00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0276
            05       5-WQ00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0276
       01               S-CX01-SSA.                                     CI0276
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0276
                                      VALUE 'CX01    '.                 CI0276
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0276
            10          S-CX01-CCOD   PICTURE X(5)                      CI0276
                                      VALUE '-----'.                    CI0276
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0276
       01            S-CXU01-SSA.                                       CI0276
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX01    '.                 CI0276
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0276
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(CX01K'.                   CI0276
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0276
            10       S-CXU01-CX01K.                                     CI0276
            11       S-CXU01-C199.                                      CI0276
            12       S-CXU01-CLID.                                      CI0276
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0276
            13       S-CXU01-CLIDN.                                     CI0276
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0276
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0276
            10  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01               S-CX03-SSA.                                     CI0276
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0276
                                      VALUE 'CX03    '.                 CI0276
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0276
            10          S-CX03-CCOD   PICTURE X(5)                      CI0276
                                      VALUE '-----'.                    CI0276
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0276
       01            S-CXA03-SSA.                                       CI0276
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX03    '.                 CI0276
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0276
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(CARTY'.                   CI0276
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0276
            12       S-CXA03-CARTY    PICTURE  99.                      CI0276
            12  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01            S-CXB03-SSA.                                       CI0276
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX03    '.                 CI0276
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0276
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(NARRS'.                   CI0276
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0276
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            12  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01            S-CXC03-SSA.                                       CI0276
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX03    '.                 CI0276
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0276
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(CPMTG'.                   CI0276
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0276
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0276
            11  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01            S-CXD03-SSA.                                       CI0276
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX03    '.                 CI0276
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0276
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(GRCRNG'.                  CI0276
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0276
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0276
            11  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01            S-CXE03-SSA.                                       CI0276
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX03    '.                 CI0276
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0276
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(DEXDT'.                   CI0276
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0276
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0276
            11  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01            S-CXF03-SSA.                                       CI0276
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX03    '.                 CI0276
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0276
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(CY50'.                    CI0276
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0276
            11       S-CXF03-CY50.                                      CI0276
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0276
            11  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01            S-CXG03-SSA.                                       CI0276
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX03    '.                 CI0276
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0276
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(NBASQ'.                   CI0276
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0276
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01            S-CXH03-SSA.                                       CI0276
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX03    '.                 CI0276
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0276
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(NARID'.                   CI0276
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0276
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0276
            12  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01            S-CXU03-SSA.                                       CI0276
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX03    '.                 CI0276
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0276
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(CX03K'.                   CI0276
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0276
            11       S-CXU03-CX03K.                                     CI0276
            12       S-CXU03-CARTY    PICTURE  99.                      CI0276
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01               S-CX06-SSA.                                     CI0276
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0276
                                      VALUE 'CX06    '.                 CI0276
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0276
            10          S-CX06-CCOD   PICTURE X(5)                      CI0276
                                      VALUE '-----'.                    CI0276
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0276
       01            S-CXU06-SSA.                                       CI0276
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX06    '.                 CI0276
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0276
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(CX06K'.                   CI0276
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0276
            10       S-CXU06-CX06K.                                     CI0276
            11       S-CXU06-C299.                                      CI0276
            12       S-CXU06-CTID.                                      CI0276
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0276
            13       S-CXU06-CTIDN.                                     CI0276
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0276
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0276
            10  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01               S-CX13-SSA.                                     CI0276
            10         S1-CX13-SEGNAM PICTURE X(8)                      CI0276
                                      VALUE 'CX13    '.                 CI0276
            10         S1-CX13-CCOM   PICTURE X VALUE '*'.              CI0276
            10          S-CX13-CCOD   PICTURE X(5)                      CI0276
                                      VALUE '-----'.                    CI0276
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0276
       01            S-CXA13-SSA.                                       CI0276
            11      S1-CXA13-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX13    '.                 CI0276
            11      S1-CXA13-CCOM   PICTURE X VALUE '*'.                CI0276
            11       S-CXA13-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            11      S1-CXA13-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(CDEST'.                   CI0276
            11       S-CXA13-OPER  PICTURE XX VALUE ' ='.               CI0276
            11       S-CXA13-CDEST    PICTURE  99.                      CI0276
            11  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01            S-CXB13-SSA.                                       CI0276
            12      S1-CXB13-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX13    '.                 CI0276
            12      S1-CXB13-CCOM   PICTURE X VALUE '*'.                CI0276
            12       S-CXB13-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            12      S1-CXB13-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(CARTZ'.                   CI0276
            12       S-CXB13-OPER  PICTURE XX VALUE ' ='.               CI0276
            12       S-CXB13-CARTZ    PICTURE  99.                      CI0276
            12  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01            S-CXC13-SSA.                                       CI0276
            12      S1-CXC13-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX13    '.                 CI0276
            12      S1-CXC13-CCOM   PICTURE X VALUE '*'.                CI0276
            12       S-CXC13-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            12      S1-CXC13-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(NAPDS'.                   CI0276
            12       S-CXC13-OPER  PICTURE XX VALUE ' ='.               CI0276
            12       S-CXC13-NAPDS    PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            12  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01            S-CXU13-SSA.                                       CI0276
            11      S1-CXU13-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX13    '.                 CI0276
            11      S1-CXU13-CCOM   PICTURE X VALUE '*'.                CI0276
            11       S-CXU13-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            11      S1-CXU13-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(CX13K'.                   CI0276
            11       S-CXU13-OPER  PICTURE XX VALUE ' ='.               CI0276
            11       S-CXU13-CX13K.                                     CI0276
            12       S-CXU13-CARTZ    PICTURE  99.                      CI0276
            12       S-CXU13-NAPDS    PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01            S-CX113-SSA.                                       CI0276
            11      S1-CX113-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX13    '.                 CI0276
            11      S1-CX113-CCOM   PICTURE X VALUE '*'.                CI0276
            11       S-CX113-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            11      S1-CX113-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(XGCUSPY'.                 CI0276
            11       S-CX113-OPER  PICTURE XX VALUE ' ='.               CI0276
            11       S-CX113-GCUSPY   PICTURE  X(12).                   CI0276
            11  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01               S-CX14-SSA.                                     CI0276
            10         S1-CX14-SEGNAM PICTURE X(8)                      CI0276
                                      VALUE 'CX14    '.                 CI0276
            10         S1-CX14-CCOM   PICTURE X VALUE '*'.              CI0276
            10          S-CX14-CCOD   PICTURE X(5)                      CI0276
                                      VALUE '-----'.                    CI0276
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0276
       01            S-CXU14-SSA.                                       CI0276
            10      S1-CXU14-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX14    '.                 CI0276
            10      S1-CXU14-CCOM   PICTURE X VALUE '*'.                CI0276
            10       S-CXU14-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            10      S1-CXU14-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(CX14K'.                   CI0276
            10       S-CXU14-OPER  PICTURE XX VALUE ' ='.               CI0276
            10       S-CXU14-CX14K.                                     CI0276
            11       S-CXU14-NPISQ    PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            10  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01            S-CX114-SSA.                                       CI0276
            11      S1-CX114-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX14    '.                 CI0276
            11      S1-CX114-CCOM   PICTURE X VALUE '*'.                CI0276
            11       S-CX114-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            11      S1-CX114-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(XGCUSPZ'.                 CI0276
            11       S-CX114-OPER  PICTURE XX VALUE ' ='.               CI0276
            11       S-CX114-GCUSPZ   PICTURE  X(12).                   CI0276
            11  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01               S-CX15-SSA.                                     CI0276
            10         S1-CX15-SEGNAM PICTURE X(8)                      CI0276
                                      VALUE 'CX15    '.                 CI0276
            10         S1-CX15-CCOM   PICTURE X VALUE '*'.              CI0276
            10          S-CX15-CCOD   PICTURE X(5)                      CI0276
                                      VALUE '-----'.                    CI0276
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0276
       01            S-CXU15-SSA.                                       CI0276
            10      S1-CXU15-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX15    '.                 CI0276
            10      S1-CXU15-CCOM   PICTURE X VALUE '*'.                CI0276
            10       S-CXU15-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            10      S1-CXU15-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(CX15K'.                   CI0276
            10       S-CXU15-OPER  PICTURE XX VALUE ' ='.               CI0276
            10       S-CXU15-CX15K.                                     CI0276
            11       S-CXU15-CFIDC    PICTURE  X(5).                    CI0276
            10  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01               S-CX17-SSA.                                     CI0276
            10         S1-CX17-SEGNAM PICTURE X(8)                      CI0276
                                      VALUE 'CX17    '.                 CI0276
            10         S1-CX17-CCOM   PICTURE X VALUE '*'.              CI0276
            10          S-CX17-CCOD   PICTURE X(5)                      CI0276
                                      VALUE '-----'.                    CI0276
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0276
       01            S-CXU17-SSA.                                       CI0276
            10      S1-CXU17-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'CX17    '.                 CI0276
            10      S1-CXU17-CCOM   PICTURE X VALUE '*'.                CI0276
            10       S-CXU17-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            10      S1-CXU17-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(CX17K'.                   CI0276
            10       S-CXU17-OPER  PICTURE XX VALUE ' ='.               CI0276
            10       S-CXU17-CX17K.                                     CI0276
            11       S-CXU17-CFIDC    PICTURE  X(5).                    CI0276
            10  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01               S-GQ01-SSA.                                     CI0276
            10         S1-GQ01-SEGNAM PICTURE X(8)                      CI0276
                                      VALUE 'GQ01    '.                 CI0276
            10         S1-GQ01-CCOM   PICTURE X VALUE '*'.              CI0276
            10          S-GQ01-CCOD   PICTURE X(5)                      CI0276
                                      VALUE '-----'.                    CI0276
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0276
       01            S-GQU01-SSA.                                       CI0276
            13      S1-GQU01-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'GQ01    '.                 CI0276
            13      S1-GQU01-CCOM   PICTURE X VALUE '*'.                CI0276
            13       S-GQU01-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            13      S1-GQU01-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(GQ01K'.                   CI0276
            13       S-GQU01-OPER  PICTURE XX VALUE ' ='.               CI0276
            13       S-GQU01-GQ01K.                                     CI0276
            14       S-GQU01-CANUMB   PICTURE  X(27).                   CI0276
            14       S-GQU01-CAMCTR   PICTURE  9(5).                    CI0276
            14       S-GQU01-GESQ2    PICTURE  99.                      CI0276
            13  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01            S-GQ701-SSA.                                       CI0276
            14      S1-GQ701-SEGNAM PICTURE X(8)                        CI0276
                                      VALUE 'GQ01    '.                 CI0276
            14      S1-GQ701-CCOM   PICTURE X VALUE '*'.                CI0276
            14       S-GQ701-CCOD   PICTURE X(5)                        CI0276
                                      VALUE '-----'.                    CI0276
            14      S1-GQ701-FLDNAM PICTURE X(9)                        CI0276
                                      VALUE '(XCANUMB'.                 CI0276
            14       S-GQ701-OPER  PICTURE XX VALUE ' ='.               CI0276
            14       S-GQ701-CANUMB   PICTURE  X(27).                   CI0276
            14  FILLER   PICTURE X    VALUE ')'.                        CI0276
       01   ZONES-UTILISATEUR PICTURE X.                                CI0276
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
      ** PCB POINTER FOR ARAY                                           ADU015
            05 PCB-ARAY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR TR1P                                           ADU015
            05 PCB-TR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR DATP                                           ADU015
            05 PCB-DATP-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=XC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XC00.                                         CI0276
          05              XC00-SUITE.                                   CI0276
            15       FILLER         PICTURE  X(00106).                  CI0276
       01                 XC06  REDEFINES      XC00.                    CI0276
            10            XC06-XDBPCB.                                  CI0276
            11            XC06-XDBDNM PICTURE  X(08).                   CI0276
            11            XC06-XSEGLV PICTURE  X(02).                   CI0276
            11            XC06-XRC    PICTURE  X(02).                   CI0276
            11            XC06-XPROPT PICTURE  X(04).                   CI0276
            11            XC06-FILLER PICTURE  S9(5)                    CI0276
                          BINARY.                                       CI0276
            11            XC06-XSEGNM PICTURE  X(08).                   CI0276
            11            XC06-XKEYLN PICTURE  S9(05)                   CI0276
                          BINARY.                                       CI0276
            11            XC06-XSEGNB PICTURE  9(05)                    CI0276
                          BINARY.                                       CI0276
            11            XC06-XCOKEY PICTURE  X(70).                   CI0276
      *** PCB MASK FOR ARAY                                             ADU015
      *!WF DSP=XD DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XD00.                                         CI0276
          05              XD00-SUITE.                                   CI0276
            15       FILLER         PICTURE  X(00106).                  CI0276
       01                 XD06  REDEFINES      XD00.                    CI0276
            10            XD06-XDBPCB.                                  CI0276
            11            XD06-XDBDNM PICTURE  X(08).                   CI0276
            11            XD06-XSEGLV PICTURE  X(02).                   CI0276
            11            XD06-XRC    PICTURE  X(02).                   CI0276
            11            XD06-XPROPT PICTURE  X(04).                   CI0276
            11            XD06-FILLER PICTURE  S9(5)                    CI0276
                          BINARY.                                       CI0276
            11            XD06-XSEGNM PICTURE  X(08).                   CI0276
            11            XD06-XKEYLN PICTURE  S9(05)                   CI0276
                          BINARY.                                       CI0276
            11            XD06-XSEGNB PICTURE  9(05)                    CI0276
                          BINARY.                                       CI0276
            11            XD06-XCOKEY PICTURE  X(70).                   CI0276
      *** PCB MASK FOR TR1P                                             ADU015
      *!WF DSP=XF DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XF00.                                         CI0276
          05              XF00-SUITE.                                   CI0276
            15       FILLER         PICTURE  X(00106).                  CI0276
       01                 XF06  REDEFINES      XF00.                    CI0276
            10            XF06-XDBPCB.                                  CI0276
            11            XF06-XDBDNM PICTURE  X(08).                   CI0276
            11            XF06-XSEGLV PICTURE  X(02).                   CI0276
            11            XF06-XRC    PICTURE  X(02).                   CI0276
            11            XF06-XPROPT PICTURE  X(04).                   CI0276
            11            XF06-FILLER PICTURE  S9(5)                    CI0276
                          BINARY.                                       CI0276
            11            XF06-XSEGNM PICTURE  X(08).                   CI0276
            11            XF06-XKEYLN PICTURE  S9(05)                   CI0276
                          BINARY.                                       CI0276
            11            XF06-XSEGNB PICTURE  9(05)                    CI0276
                          BINARY.                                       CI0276
            11            XF06-XCOKEY PICTURE  X(70).                   CI0276
      *** PCB MASK FOR DATP                                             ADU015
      *!WF DSP=XG DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XG00.                                         CI0276
          05              XG00-SUITE.                                   CI0276
            15       FILLER         PICTURE  X(00106).                  CI0276
       01                 XG06  REDEFINES      XG00.                    CI0276
            10            XG06-XDBPCB.                                  CI0276
            11            XG06-XDBDNM PICTURE  X(08).                   CI0276
            11            XG06-XSEGLV PICTURE  X(02).                   CI0276
            11            XG06-XRC    PICTURE  X(02).                   CI0276
            11            XG06-XPROPT PICTURE  X(04).                   CI0276
            11            XG06-FILLER PICTURE  S9(5)                    CI0276
                          BINARY.                                       CI0276
            11            XG06-XSEGNM PICTURE  X(08).                   CI0276
            11            XG06-XKEYLN PICTURE  S9(05)                   CI0276
                          BINARY.                                       CI0276
            11            XG06-XSEGNB PICTURE  9(05)                    CI0276
                          BINARY.                                       CI0276
            11            XG06-XCOKEY PICTURE  X(70).                   CI0276

      *PASS AREA TO CI0276
      *!WF DSP=K9 DSL=K9 SEL=85 FOR=I DES=1 LEV=1 PLT=75
       01                 K985.                                         CI0276
            10            K985-MAPPN  PICTURE  X(10).                   CI0276
            10            K985-C299.                                    CI0276
            11            K985-CTID.                                    CI0276
            12            K985-CTIDA  PICTURE  9(3).                    CI0276
            12            K985-CTIDN.                                   CI0276
            13            K985-CTIDNP PICTURE  X(13).                   CI0276
            13            K985-CTIDND PICTURE  9(11).                   CI0276
            10            K985-CAPPL  PICTURE  X(8).                    CI0276
            10            K985-CACTS  PICTURE  X.                       CI0276
            10            K985-CPMTF  PICTURE  99.                      CI0276
            10            K985-GESTD  PICTURE  9(8).                    CI0276
            10            K985-DNPMT  PICTURE  9(8).                    CI0276
            10            K985-GEEND  PICTURE  9(8).                    CI0276
            10            K985-C199.                                    CI0276
            11            K985-CLID.                                    CI0276
            12            K985-CLIDO  PICTURE  9(3).                    CI0276
            12            K985-CLIDN.                                   CI0276
            13            K985-CLIDNP PICTURE  X(12).                   CI0276
            13            K985-CLIDND PICTURE  9(8).                    CI0276
            10            K985-PRCOD  PICTURE  9(5).                    CI0276
            10            K985-IVANT  PICTURE  X(1).                    CI0276
            10            K985-ALPLDT PICTURE  9(8).                    CI0276
            10            K985-ALINNO PICTURE  99.                      CI0276
            10            K985-ADBRQ  PICTURE  S9(11)V99                CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            K985-DEFFT  PICTURE  9(8).                    CI0276
            10            K985-DCACGE PICTURE  S9(8)                    CI0276
                          BINARY.                                       CI0276
            10            K985-CDEST  PICTURE  99.                      CI0276
            10            K985-NAPDS  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            K985-ISUBAA PICTURE  X.                       CI0276
            10            K985-ISUBAB PICTURE  X.                       CI0276
            10            K985-QSACTF PICTURE  9(3).                    CI0276
            10            K985-QSACTT PICTURE  9(3).                    CI0276
            10            K985-QSACTG PICTURE  9(3).                    CI0276
            10            K985-QSACTU PICTURE  9(3).                    CI0276
            10            K985-GEOPD2 PICTURE  X(8).                    CI0276
            10            K985-GETOD  PICTURE  9(6).                    CI0276
            10            K985-CTRHO  PICTURE  9(8).                    CI0276
            10            K985-FILLER PICTURE  X(28).                   CI0276
            10            K985-K980.                                    CI0276
            11            K985-GRSST                                    CI0276
                          OCCURS       200     TIMES.                   CI0276
            12            K985-CFIDC  PICTURE  X(5).                    CI0276
            12            K985-CACCT  PICTURE  X.                       CI0276
            12            K985-ADBRQM PICTURE  S9(11)V99.               CI0276
      *
      *ARRANGEMENT DB SEGMENTS
      *!WF DSP=LX DSL=CX SEL=010306 FOR=I DES=1 LEV=1
      * PLT=75
       01                 LX01.                                         CI0276
            10            LX01-CX01K.                                   CI0276
            11            LX01-C199.                                    CI0276
            12            LX01-CLID.                                    CI0276
            13            LX01-CLIDO  PICTURE  9(3).                    CI0276
            13            LX01-CLIDN.                                   CI0276
            14            LX01-CLIDNP PICTURE  X(12).                   CI0276
            14            LX01-CLIDND PICTURE  9(8).                    CI0276
            10            LX01-GEMDA  PICTURE  9(8).                    CI0276
            10            LX01-NSEQ4B PICTURE  9(8)                     CI0276
                          BINARY.                                       CI0276
            10            LX01-FILLER PICTURE  X(5).                    CI0276
       01                 LX03.                                         CI0276
            10            LX03-GELL   PICTURE  9(4)                     CI0276
                          BINARY.                                       CI0276
            10            LX03-CY00.                                    CI0276
            11            LX03-CX03K.                                   CI0276
            12            LX03-CARTY  PICTURE  99.                      CI0276
            12            LX03-NARRS  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            LX03-CARST  PICTURE  99.                      CI0276
            11            LX03-GECSQ  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            LX03-CPMTG  PICTURE  99.                      CI0276
            11            LX03-GRCRNG PICTURE  9(3).                    CI0276
            11            LX03-DEXDT  PICTURE  9(8).                    CI0276
            11            LX03-DASUP  PICTURE  9(8).                    CI0276
            11            LX03-CSTEC  PICTURE  X(3).                    CI0276
            11            LX03-FILLER PICTURE  X(17).                   CI0276
            11            LX03-CY50.                                    CI0276
            12            LX03-NARID  PICTURE  X(30).                   CI0276
            11            LX03-CY51                                     CI0276
                          REDEFINES            LX03-CY50.               CI0276
            12            LX03-NDIDN  PICTURE  9(12).                   CI0276
            12            LX03-FILLER PICTURE  X(18).                   CI0276
            11            LX03-CY52                                     CI0276
                          REDEFINES            LX03-CY50.               CI0276
            12            LX03-NAIDC  PICTURE  9(12).                   CI0276
            12            LX03-FILLER PICTURE  X(18).                   CI0276
            11            LX03-CY53                                     CI0276
                          REDEFINES            LX03-CY50.               CI0276
            12            LX03-NAMEXB PICTURE  9(15).                   CI0276
            12            LX03-FILLER PICTURE  X(15).                   CI0276
            10            LX03-CY99.                                    CI0276
            11            LX03-FILLER PICTURE  X(109).                  CI0276
            10            LX03-CY01                                     CI0276
                          REDEFINES            LX03-CY99.               CI0276
            11            LX03-NBASQ  PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            LX03-ICPCI  PICTURE  X.                       CI0276
            11            LX03-CLUPD  PICTURE  9(3).                    CI0276
            11            LX03-DLAUP  PICTURE  9(8).                    CI0276
            11            LX03-CWRC   PICTURE  99.                      CI0276
            11            LX03-CHCR   PICTURE  99.                      CI0276
            11            LX03-GEOPD2 PICTURE  X(8).                    CI0276
            11            LX03-GEAUN  PICTURE  9(5).                    CI0276
            11            LX03-DPCHD  PICTURE  9(8).                    CI0276
            11            LX03-DLRCHK PICTURE  9(8).                    CI0276
            11            LX03-QTRCHK PICTURE  9(2).                    CI0276
            11            LX03-DNPMT  PICTURE  9(8).                    CI0276
            11            LX03-APMTLA PICTURE  S9(9)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            LX03-CY02                                     CI0276
                          REDEFINES            LX03-CY99.               CI0276
            11            LX03-QSIRQ  PICTURE  99.                      CI0276
            11            LX03-QDRMN  PICTURE  9(2)                     CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            LX03-DDPRE  PICTURE  9(8).                    CI0276
            11            LX03-DDSHP  PICTURE  9(8).                    CI0276
            11            LX03-NDRFTB PICTURE  9(5).                    CI0276
            11            LX03-QDIPBJ PICTURE  9(3).                    CI0276
            11            LX03-DDSHPA PICTURE  9(8).                    CI0276
            11            LX03-NDRFTF PICTURE  9(5).                    CI0276
            11            LX03-QDIPBK PICTURE  9(3).                    CI0276
            11            LX03-CREOR  PICTURE  X(1).                    CI0276
            11            LX03-CREOR1 PICTURE  X(1).                    CI0276
            11            LX03-DDASC  PICTURE  9(8).                    CI0276
            11            LX03-FILLER PICTURE  X(7).                    CI0276
            10            LX03-CY03                                     CI0276
                          REDEFINES            LX03-CY99.               CI0276
            11            LX03-DLAUP1 PICTURE  9(8).                    CI0276
            11            LX03-GEOPD3 PICTURE  X(8).                    CI0276
            11            LX03-DNPMT1 PICTURE  9(8).                    CI0276
            11            LX03-DOPDA  PICTURE  99.                      CI0276
            11            LX03-CPMTF  PICTURE  99.                      CI0276
            11            LX03-CIRMO  PICTURE  X(12).                   CI0276
            11            LX03-CPALL  PICTURE  X(1).                    CI0276
            11            LX03-CCOLM  PICTURE  9(2).                    CI0276
            11            LX03-CBLTP  PICTURE  X(1).                    CI0276
            11            LX03-CASUB  PICTURE  9(2).                    CI0276
            11            LX03-CBLFM  PICTURE  9(2).                    CI0276
            11            LX03-IBILS  PICTURE  X.                       CI0276
            11            LX03-IPAOS  PICTURE  X.                       CI0276
            11            LX03-CBLSQ  PICTURE  X(4).                    CI0276
            11            LX03-DLBPD  PICTURE  9(8).                    CI0276
            11            LX03-DNBPD  PICTURE  9(8).                    CI0276
            11            LX03-DODBD  PICTURE  9(8).                    CI0276
            11            LX03-CPSRE  PICTURE  99.                      CI0276
            11            LX03-ISPHN  PICTURE  X.                       CI0276
            11            LX03-TCARR  PICTURE  X(6).                    CI0276
            11            LX03-CBKPT  PICTURE  9(2).                    CI0276
            11            LX03-IECNT  PICTURE  X.                       CI0276
            11            LX03-ICONV  PICTURE  X(1).                    CI0276
            11            LX03-FILLER PICTURE  X(4).                    CI0276
            10            LX03-CY04                                     CI0276
                          REDEFINES            LX03-CY99.               CI0276
            11            LX03-CCARD  PICTURE  X(02).                   CI0276
            11            LX03-MCSIG4 PICTURE  X(20).                   CI0276
            11            LX03-IREMT  PICTURE  X(01).                   CI0276
            11            LX03-ISBILA PICTURE  X.                       CI0276
            11            LX03-DLBPDA PICTURE  9(8).                    CI0276
            11            LX03-DNBPDA.                                  CI0276
            12            LX03-DNCYM  PICTURE  9(6).                    CI0276
            12            LX03-CEDTD  PICTURE  9(2).                    CI0276
            11            LX03-AREMT  PICTURE  S9(7)V99                 CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            LX03-DREMT  PICTURE  9(8).                    CI0276
            11            LX03-ADBRQ  PICTURE  S9(11)V99                CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            LX03-CLUPD1 PICTURE  S9(3)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            LX03-DLAUP3 PICTURE  9(8).                    CI0276
            11            LX03-CWRC2  PICTURE  99.                      CI0276
            11            LX03-CHCR2  PICTURE  99.                      CI0276
            11            LX03-GEOPD9 PICTURE  X(8).                    CI0276
            11            LX03-GEAUN1 PICTURE  9(5).                    CI0276
            11            LX03-DPCHD1 PICTURE  9(8).                    CI0276
       01                 LX06.                                         CI0276
            10            LX06-CX06K.                                   CI0276
            11            LX06-C299.                                    CI0276
            12            LX06-CTID.                                    CI0276
            13            LX06-CTIDA  PICTURE  9(3).                    CI0276
            13            LX06-CTIDN.                                   CI0276
            14            LX06-CTIDNP PICTURE  X(13).                   CI0276
            14            LX06-CTIDND PICTURE  9(11).                   CI0276
            10            LX06-NPECK  PICTURE  9(02).                   CI0276
            10            LX06-FILLER PICTURE  X.                       CI0276

      ******************************************************************
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *
      ******************************************************************
      *
      *!WF DSP=DE DSL=DU SEL=10 FOR=I DES=1 LEV=1 PLT=85
       01                 DE10.                                         CI0276
            10            DE10-DU11.                                    CI0276
            11            DE10-XFONC  PICTURE  X(4).                    CI0276
            11            DE10-MPSBN  PICTURE  X(8).                    CI0276
            11            DE10-XDBDNM PICTURE  X(08).                   CI0276
            11            DE10-XSEGNM PICTURE  X(08).                   CI0276
            11            DE10-XRC    PICTURE  X(02).                   CI0276
            11            DE10-MSEG   PICTURE  X(08).                   CI0276
            11            DE10-XCOKEY PICTURE  X(70).                   CI0276
            11            DE10-CUIBR  PICTURE  X(01).                   CI0276
            11            DE10-CUIBA  PICTURE  X(01).                   CI0276
            11            DE10-IPBIK  PICTURE  X(1).                    CI0276
            10            DE10-DU03.                                    CI0276
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            DE10-CMSSF  PICTURE  XX.                      CI0276
            11            DE10-DU09.                                    CI0276
            12            DE10-CMESA  PICTURE  S9(9)                    CI0276
                          BINARY.                                       CI0276
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0276
                          BINARY.                                       CI0276
            12            DE10-CMESB  PICTURE  S9(9)                    CI0276
                          BINARY.                                       CI0276
            12            DE10-CMSST  PICTURE  S9(9)                    CI0276
                          BINARY.                                       CI0276
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0276
                          BINARY.                                       CI0276
            12            DE10-QELLAA PICTURE  S9(9)                    CI0276
                          BINARY.                                       CI0276
            12            DE10-TMESS4 PICTURE  X(512).                  CI0276

      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0276
          05              MS00-SUITE.                                   CI0276
            15       FILLER         PICTURE  X(00542).                  CI0276
       01                 MS03  REDEFINES      MS00.                    CI0276
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            10            MS03-CMSSF  PICTURE  XX.                      CI0276
            10            MS03-DU09.                                    CI0276
            11            MS03-CMESA  PICTURE  S9(9)                    CI0276
                          BINARY.                                       CI0276
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0276
                          BINARY.                                       CI0276
            11            MS03-CMESB  PICTURE  S9(9)                    CI0276
                          BINARY.                                       CI0276
            11            MS03-CMSST  PICTURE  S9(9)                    CI0276
                          BINARY.                                       CI0276
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0276
                          BINARY.                                       CI0276
            11            MS03-QELLAA PICTURE  S9(9)                    CI0276
                          BINARY.                                       CI0276
            11            MS03-TMESS4 PICTURE  X(512).                  CI0276
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0276
            10            MX11-QMSGS  PICTURE  9(03).                   CI0276
            10            MX11-PJ09                                     CI0276
                          OCCURS       025     TIMES.                   CI0276
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0276
                          COMPUTATIONAL-3.                              CI0276
            11            MX11-CMESB  PICTURE  S9(9)                    CI0276
                          BINARY.                                       CI0276
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                K985
                                LX01
                                LX03
                                LX06
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N0T.      NOTE *************************************.
      *               *                                   *
      *               *SET POINTERS FOR DB ACCESS         *
      *               *                                   *
      *               *************************************.
       F0T.           EXIT.                                             lv05
      *N0TSC.    NOTE *----> SCHEDULE PSB                 *.
       F0TSC.                                                           lv10
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF XC06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR ARAY                                             DOT
           SET ADDRESS OF XD06 TO                                       ADU015
                PCB-ARAY-PTR1.                                          ADU015
      *SET ADDRESS FOR TR1P                                             DOT
           SET ADDRESS OF XF06 TO                                       ADU015
                PCB-TR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR DATP                                             DOT
           SET ADDRESS OF XG06 TO                                       ADU015
                PCB-DATP-PTR1.                                          ADU015
       F0TSC-FN. EXIT.
       F0T-FN.   EXIT.
      *N01.      NOTE *************************************.            CI0276
      *               *                                   *             CI0276
      *               *INITIALISATIONS                    *             CI0276
      *               *                                   *             CI0276
      *               *************************************.            CI0276
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
      *N02DB.    NOTE *INITIALIZE WORK FIELDS             *.
       F02DB.                                                           lv10
           MOVE        'N' TO CX14-FOUND
           CX15-FOUND
           CX17-FOUND
           CHECK-TO-OWNER.
       F02DB-FN. EXIT.
       F02-FN.   EXIT.
      *N03.      NOTE *************************************.
      *               *                                   *
      *               *COMMON DATA GATHERING              *
      *               *                                   *
      *               *************************************.
       F03.           EXIT.                                             lv05
      *N03BB.    NOTE *CALL ACF EXIT MODULE               *.            ADU031
       F03BB.                                                           lv10
           EXEC CICS   LINK PROGRAM (ACF-PROG)                          ADU031
                       COMMAREA (ACF-USER-AREA)                         ADU031
                       LENGTH (ACF-AREA-LEN)                 END-EXEC.  ADU031
       F03BB-FN. EXIT.
      *N03DT.    NOTE *CALL CI0020 - CAMS ACCTG DATE      *.            AM0020
       F03DT.                                                           lv10
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
      *N03DU.    NOTE *NON-DL1 ERROR                      *.            ADU070
       F03DU.    IF    MS03-NMESS2 > ZERO                               lv15
                 AND   MS03-CMESB > 10                                  ADU070
                 NEXT SENTENCE ELSE GO TO     F03DU-FN.                 ADU070
      *OF A CERTAIN SEVERITY                                            ADU070
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU070
           MOVE        CI0020 TO MS03-TMESS4 (IMS03R : 6)               ADU070
           ADD         +7 TO MS03-QELLAA                                ADU070
           MOVE                     ALL '1' TO FT GO TO F20.            ADU070
       F03DU-900. GO TO F03DV-FN.
       F03DU-FN. EXIT.
      *N03DV.    NOTE *NO ERRORS                          *.            ADU070
       F03DV.                                                           lv15
           INITIALIZE  MS03.                                            ADU070
       F03DV-FN. EXIT.
       F03DT-FN. EXIT.
       F03-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0276
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0276
      *               *                                   *             CI0276
      *               *FIN DE TRAITEMENT                  *             CI0276
      *               *                                   *             CI0276
      *               *************************************.            CI0276
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0276
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATE INPUT PARMS               *
      *               *                                   *
      *               *************************************.
       F30.                                                             lv05
      *
      *********************************
      **  ENSURE PARMS HAVE THE       *
      **  CORRECT CONTENTS BASED ON   *
      **  FIELD CLASS AND CONTENTS    *
      *********************************
      *N30BB.    NOTE *VALIDATE CONTRACT ID               *.
       F30BB.    IF    K985-CTID NUMERIC                                lv10
                 AND   K985-CTID > 0
                 NEXT SENTENCE ELSE GO TO     F30BB-FN.
      *MUST BE GREATER THAN ZERO
      **  INACTIVATE, REACTIVATE      *
       F30BB-900. GO TO F30BD-FN.
       F30BB-FN. EXIT.
      *N30BD.    NOTE *CONTRACT ID INVALID                *.
       F30BD.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012004 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30BD-FN. EXIT.
      *N30BF.    NOTE *VALID ACTION TYPE                  *.
       F30BF.    IF    K985-CACTS = 'A'                                 lv10
                 OR    K985-CACTS = 'C'
                 OR    K985-CACTS = 'I'
                 OR    K985-CACTS = 'R'
                 NEXT SENTENCE ELSE GO TO     F30BF-FN.
      **  CREATE OR MODIFY OR      *
      **  INACTIVATE OR            *
      **  REACTIVATE               *
       F30BF-900. GO TO F30BG-FN.
       F30BF-FN. EXIT.
      *N30BG.    NOTE *INVALID ACTION TYPE                *.
       F30BG.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012382 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30BG-FN. EXIT.
      *N30BJ.    NOTE *VALID ARRANGEMENT STATUS           *.
       F30BJ.    IF    K985-CDEST NUMERIC                               lv10
                 AND   (K985-CDEST = 01
                 OR    K985-CDEST = 02
                 OR    K985-CDEST = 03
                 OR    K985-CDEST = 04)
                 NEXT SENTENCE ELSE GO TO     F30BJ-FN.
      ***************************
      ***************************
      ***************************
      *************************
       F30BJ-900. GO TO F30BK-FN.
       F30BJ-FN. EXIT.
      *N30BK.    NOTE *INVALID ARRANGEMENT STATUS         *.
       F30BK.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012611 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30BK-FN. EXIT.
      *N30DB.    NOTE *VALIDATE APPLICATION CODE          *.
       F30DB.    IF    K985-CAPPL NOT =                                 lv10
                       'ANNUITY '
                 NEXT SENTENCE ELSE GO TO     F30DB-FN.
      **  VERIFY OR UPDATE            *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012775 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DB-FN. EXIT.
      *N30DG.    NOTE *VALIDATE APPLICATION/FREQUENCY     *.
       F30DG.    IF    K985-CAPPL = 'ANNUITY '                          lv10
                 NEXT SENTENCE ELSE GO TO     F30DG-FN.
           MOVE        K985-CPMTF TO 7-WS-CPMTF.
                 IF    NOT ANNUITY-FREQ                                 DOT
      *FREQUENCY NOT VALID FOR ANNUITY
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012119 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DG-FN. EXIT.
      *N30FB.    NOTE *VALIDATE START DATE                *.
       F30FB.    IF    K985-GESTD NUMERIC                               lv10
                 NEXT SENTENCE ELSE GO TO     F30FB-FN.
       F30FB-900. GO TO F30FD-FN.
       F30FB-FN. EXIT.
      *N30FD.    NOTE *START DATE INVALID                 *.
       F30FD.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012537 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30FD-FN. EXIT.
      *N30FL.    NOTE *VALIDATE END DATE                  *.
       F30FL.    IF    K985-GEEND NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F30FL-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012205 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30FL-FN. EXIT.
      *N30HB.    NOTE *VALIDATE POLICY DATE NUMERIC       *.
       F30HB.    IF    K985-ALPLDT NOT NUMERIC                          lv10
                 NEXT SENTENCE ELSE GO TO     F30HB-FN.
      **
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013814 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30HB-FN. EXIT.
      *N30JB.    NOTE *VALID ARR SEQUENCE                 *.
       F30JB.    IF    K985-NAPDS NUMERIC                               lv10
                 AND   K985-NAPDS > 0
                 NEXT SENTENCE ELSE GO TO     F30JB-FN.
      ** *
       F30JB-900. GO TO F30JD-FN.
       F30JB-FN. EXIT.
      *N30JD.    NOTE *INVALID ARR SEQUENCE               *.
       F30JD.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012034 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30JD-FN. EXIT.
      *N30KB.    NOTE *VALID CLIENT ID                    *.
       F30KB.    IF    K985-CLID NUMERIC                                lv10
                 AND   K985-CLID > 0
                 NEXT SENTENCE ELSE GO TO     F30KB-FN.
      *MUST BE GREATER THAN ZERO
       F30KB-900. GO TO F30KD-FN.
       F30KB-FN. EXIT.
      *N30KD.    NOTE *CLIENT ID INVALID                  *.
       F30KD.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012002 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30KD-FN. EXIT.
      *N30KG.    NOTE *INVALID PRODUCT CODE               *.
       F30KG.    IF    K985-PRCOD NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F30KG-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012780 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30KG-FN. EXIT.
      *N30KL.    NOTE *INVALID INNOVEST CODE              *.
       F30KL.    IF    K985-ALINNO NOT NUMERIC                          lv10
                 NEXT SENTENCE ELSE GO TO     F30KL-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013838 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30KL-FN. EXIT.
      *N30LB.    NOTE *INVALID EFFECTIVE DATE             *.
       F30LB.    IF    K985-DEFFT NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F30LB-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012185 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30LB-FN. EXIT.
      *N30LL.    NOTE *INVALID NEXT PAYMENT DATE          *.
       F30LL.    IF    K985-DNPMT NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F30LL-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012204 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30LL-FN. EXIT.
       F30-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *MORE VALIDATION                    *
      *               *                                   *
      *               *************************************.
       F35.           EXIT.                                             lv05
      *N35BB.    NOTE *VALIDATE NUM OF FROM SUB-ACCTS     *.
       F35BB.    IF    (K985-QSACTF NUMERIC                             lv10
                 AND   K985-QSACTF > 0)
                 AND   (K985-QSACTG NUMERIC
                 AND   K985-QSACTG > 0)
                 NEXT SENTENCE ELSE GO TO     F35BB-FN.
      *MUST BE GREATER THAN ZERO.
      *ALSO MUST BE TRUE FOR COUNTS
      *OF VALUED SUB-ACCOUNTS.
       F35BB-900. GO TO F35BG-FN.
       F35BB-FN. EXIT.
      *N35BG.    NOTE *INVALID NUM OF FROM SUB-ACCTS      *.
       F35BG.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013712 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35BG-FN. EXIT.
      *N35DB.    NOTE *VALIDATE NUM OF TO SUB-ACCTS       *.
       F35DB.    IF    K985-QSACTT NUMERIC                              lv10
                 AND   K985-QSACTT > 0
                 AND   (K985-QSACTU NUMERIC
                 AND   K985-QSACTU > 0)
                 NEXT SENTENCE ELSE GO TO     F35DB-FN.
      *MUST BE GREATER THAN ZERO
      *ALSO MUST BE TRUE FOR COUNTS
      *OF VALUED SUB-ACCOUNTS.
       F35DB-900. GO TO F35DG-FN.
       F35DB-FN. EXIT.
      *N35DG.    NOTE *INVALID NUM OF TO SUB-ACCTS        *.
       F35DG.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013712 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35DG-FN. EXIT.
      *N35FB.    NOTE *CALC TOTAL NUMBER OF SUB ACCTS     *.
       F35FB.                                                           lv10
           COMPUTE     WS-QSACT = K985-QSACTG +
           K985-QSACTU.
       F35FB-FN. EXIT.
      *N35FG.    NOTE *TOTAL DISBS AMT MUST BE NUMERIC    *.
       F35FG.    IF    K985-ADBRQ NUMERIC                               lv10
                 NEXT SENTENCE ELSE GO TO     F35FG-FN.
      *
       F35FG-900. GO TO F35FL-FN.
       F35FG-FN. EXIT.
      *N35FL.    NOTE *INVALID TOTAL DISBS AMOUNT         *.
       F35FL.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013815 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35FL-FN. EXIT.
      *N35LB.    NOTE *SUB-ACCOUNT VALIDATION             *.
       F35LB.         EXIT.                                             lv10
      *N35LG.    NOTE *TOP OF SUB-ACCT LOOP               *.
       F35LG.                                                           lv15
           MOVE        1                        TO J35LGR
                                    GO TO     F35LG-B.
       F35LG-A.
           ADD         1                        TO J35LGR.
       F35LG-B.
           IF          J35LGR                   >  WS-QSACT
                                    GO TO     F35LG-FN.
      ********************************
      *N35LL.    NOTE *INVALID SUB-ACCOUNT AMOUNT         *.
       F35LL.    IF    K985-ADBRQM (J35LGR)                             lv20
                       NOT NUMERIC
                 NEXT SENTENCE ELSE GO TO     F35LL-FN.
      ********************************
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013815 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35LL-FN. EXIT.
      *N35PG.    NOTE *INVALID SUB-ACCOUNT                *.
       F35PG.    IF    K985-CFIDC (J35LGR)                              lv20
                       NOT > SPACES
                 NEXT SENTENCE ELSE GO TO     F35PG-FN.
      ********************************
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013806 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35PG-FN. EXIT.
       F35LG-900. GO TO F35LG-A.
       F35LG-FN. EXIT.
       F35LB-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *COMMON PROCESSING                  *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40BB.    NOTE *=====COMMON AR1P KEYS ==========   *.
       F40BB.                                                           lv10
      *
      *BUILD KEYS DOWN TO CX13
           MOVE        LX01-CX01K TO S-CXU01-CX01K
           MOVE        LX03-CX03K TO S-CXU03-CX03K
           MOVE        K985-CTID TO S-CXU06-CX06K.
       F40BB-FN. EXIT.
      *N40BG.    NOTE *GU CX06                            *.
       F40BG.                                                           lv10
           PERFORM     F94C3 THRU F94C3-FN.
      *N40BL.    NOTE *CX06 NOT FOUND                     *.
       F40BL.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F40BL-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012006 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40BL-FN. EXIT.
       F40BG-FN. EXIT.
      *N40DL.    NOTE *AUDIT LOG                          *.
       F40DL.                                                           lv10
      *CLEAR OUT AUDIT PARAMETER LIST
           MOVE        SPACES TO DH10
           MOVE        ZERO TO DH10-XUIBP
           DH10-NSEQ2P
           DH10-CAUFR
           DH10-CAUAC.
       F40DL-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *ACTION = ADD                       *
      *               *                                   *
      *               *************************************.
       F45.      IF    K985-CACTS = 'A'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F45-FN.
      *********************************
      **                              *
      **          A D D               *
      **                              *
      *********************************
      *N45BB.    NOTE *========= AR1P UPDATE ==========   *.
       F45BB.         EXIT.                                             lv10
      *N45BG.    NOTE ************** CX13 *************   *.
       F45BG.                                                           lv15
           INITIALIZE  CX13
           MOVE        +157 TO CX13-GELL
           MOVE        07 TO CX13-CARTZ
           MOVE        +1 TO CX13-NAPDS
           MOVE        K985-DNPMT TO CX13-DNPMT
           CX13-GESTD
           CX13-DNEXE
           7-WS-DATE
      ** GET THE PAYMENT DAY
           MOVE        7-WS-DTDD TO CX13-DOPDA
           MOVE        NS20-DCACG TO CX13-DASUQ
           CX13-DLAUP
           MOVE        K985-GEEND TO CX13-GEEND
           MOVE        'Y' TO CX13-IIARR
           MOVE        ACF-USER-ID TO CX13-GEOPD2
           MOVE        ACF-USER-UNIT TO CX13-GEAUN
           MOVE        ZERO TO CX13-DPCHD
           CX13-PPOT1
           CX13-ACOT1
           CX13-QPST1
           MOVE        K985-ADBRQ TO CX13-ACOT1
           MOVE        K985-CPMTF TO CX13-CPMTF
           MOVE        K985-ADBRQ TO CX13-ADBRQ
           MOVE ALL    'X' TO CX13-CIRMO
           MOVE        ZERO TO CX13-QSHOWQ
           CX13-PACT1
           MOVE        01 TO CX13-CDEST
           MOVE        ZEROES TO CX13-DFPMT
           MOVE        ZEROES TO CX13-QMTHLA
           MOVE        ZEROES TO CX13-PWHLDS
           MOVE        'N' TO CX13-ISWHO.
       F45BG-FN. EXIT.
      *N45BJ.    NOTE **** CX13 AUDIT LOG RECORD   ****   *.
       F45BJ.                                                           lv15
      *********************************
      **BUILD THE DATA PORTION OF THE *
      **LOG RECORD FOR CX13 THAT WILL *
      **BE INSERTED.                  *
      *********************************
           MOVE        SPACES TO VA13
           MOVE        LX03-CX03K TO VA13-CX03K
           MOVE        K985-CTID TO VA13-CX06K
           MOVE        CX13-CY20 TO VA13-CY20
           MOVE        CX13-CY21 TO VA13-CY21
           MOVE        CX13-CY28 TO VA13-CY28.
       F45BJ-FN. EXIT.
      *N45BL.    NOTE *INSERT CX13 UNTIL IT GOES IN       *.
       F45BL.                       GO TO     F45BL-B.                  lv15
       F45BL-A.
                 IF    IK = '0'
                                    GO TO     F45BL-FN.
       F45BL-B.
           PERFORM     F94C5 THRU F94C5-FN.
                 IF    IK = '1'                                         DOT
           ADD         +1 TO CX13-NAPDS.
           PERFORM     F91AA THRU F91AA-FN.                             DOT
       F45BL-900. GO TO F45BL-A.
       F45BL-FN. EXIT.
      *N45DB.    NOTE *ONLY DO THIS IF 100% TO OWNER      *.
       F45DB.    IF    CHECK-TO-OWNER = 'Y'                             lv15
                 NEXT SENTENCE ELSE GO TO     F45DB-FN.
      *********************************
      ** THIS PROGRAM DOES NOT PROCESS*
      ** CHECK TO OWNER AT THIS TIME  *
      ** SO THE CHECK-TO-OWNER FIELD  *
      ** WAS INITIALIZED TO 'N' IN F02*
      *********************************
      ** WRITE CX14 SEGMENT
           MOVE        CX13-CX13K TO S-CXU13-CX13K
           PERFORM     F94E2 THRU F94E2-FN
           PERFORM     F91AA THRU F91AA-FN.
      *N45DG.    NOTE *SUCCESSFUL CX14 SEGMENT WRITE      *.
       F45DG.    IF    IK = '0'                                         lv20
                 AND   K985-CTIDA NOT = 005
                 NEXT SENTENCE ELSE GO TO     F45DG-FN.
      *********************************
      *REQUEST CONFIRMATION, MISC TRAN
      *********************************
           MOVE        'S' TO 7-AOACFM-CRETP
           MOVE        ZERO TO 7-AOACFM-CACKD
           PERFORM     F91BB THRU F91BB-FN.
       F45DG-FN. EXIT.
       F45DB-FN. EXIT.
      *N45FB.    NOTE ****  BUILD CX17 WORK TABLE  ***    *.
       F45FB.                                                           lv15
           INITIALIZE  7-WX17-TABLE
           MOVE        +1 TO SUB1.
       F45FB-FN. EXIT.
      *N45FD.    NOTE *LOOP THRU ALL INPUT FUNDS          *.
       F45FD.                                                           lv15
           MOVE        1                        TO J45FDR
                                    GO TO     F45FD-B.
       F45FD-A.
           ADD         1                        TO J45FDR.
       F45FD-B.
           IF          J45FDR                   >  WS-QSACT
                                    GO TO     F45FD-FN.
      *N45FG.    NOTE *USE SOURCE FUNDS TO BUILD TABLE    *.
       F45FG.    IF    K985-CACCT (J45FDR) = 'F'                        lv20
                 AND   K985-ADBRQM (J45FDR) > 0
                 NEXT SENTENCE ELSE GO TO     F45FG-FN.
      *********************************
      ** THE INPUT TABLE WILL HAVE    *
      ** ALL SOURCE SUB-ACCTS, ONLY   *
      ** TAKE THOSE WITH $ VALUES.    *
      ** FOR ADD SHOULD NOT FIND ANY  *
      ** WITH NO $ AMOUNTS.           *
      *********************************
           MOVE        K985-CFIDC (J45FDR) TO
           WX17-CFIDC (SUB1)
           MOVE        K985-ADBRQM (J45FDR) TO
           WX17-ADBRQF (SUB1)
           MOVE        ZERO TO WX17-PACT1 (SUB1)
           WX17-QSHOWQ (SUB1)
           ADD         +1 TO SUB1.
       F45FG-FN. EXIT.
       F45FD-900. GO TO F45FD-A.
       F45FD-FN. EXIT.
      *N45JB.    NOTE *BUILD CX17 KEY FOR INSERT          *.
       F45JB.                                                           lv15
      *MOVE CX13 KEY TO SSA
           MOVE        CX13-CX13K TO S-CXU13-CX13K
      *INIT INDEX FOR CX17 TABLE
           COMPUTE     IWX17L = SUB1 - +1
           MOVE        +1 TO IWX17R.
       F45JB-FN. EXIT.
      *N45JG.    NOTE *INSERT EACH TABLE ENTRY ONCE       *.
       F45JG.    IF    IWX17R NOT > IWX17L                              lv15
                 NEXT SENTENCE ELSE GO TO     F45JG-FN.
      *MOVE CX17 INTO POSITION
           MOVE        WX17 (IWX17R) TO CX17
      *INSERT, IT SHOULD GO EVERY TIME
           PERFORM     F94D3 THRU F94D3-FN.
                 IF    IK = '0'                                         DOT
      *GO TO NEXT TABLE ENTRY
           ADD         +1 TO IWX17R
                 ELSE
      *CX17 INSERT FAILED - CRITICAL
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013816 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45JG-900. GO TO F45JG.
       F45JG-FN. EXIT.
      *N45LG.    NOTE *INSERT CX13 AUDIT LOG RECORD       *.
       F45LG.                                                           lv15
           MOVE        'CLIENT' TO DH10-MAUSB
           MOVE        +70022 TO DH10-CAUFR
           MOVE        +00001 TO DH10-CAUAC
      *SET THE AUDIT LOG WORKING
      *SEQUENCE BECAUSE CAN NOT BE
      *SURE THAT THE TIME WILL BE
      *UPDATED IN DBI110.  ALREADY
      *USED SEQ 00 AND 01 FOR CX03
      *CX06 IN CALL TO CI0091.
           MOVE        +4 TO AL00-NSEQ2P
      *UPDATE CX13 SEQ IN CASE THE
      *CX13 KEY WAS CHANGED AT INSERT.
           MOVE        CX13-NAPDS TO VA13-NAPDS
      *SET UP CALL TO LOG ROUTINE
           MOVE        VA13 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F45LG-FN. EXIT.
      *N45NB.    NOTE *INSERT CX17 AUDIT LOG RECORDS      *.
       F45NB.                                                           lv15
      *WILL LOOP THROUGH CX17'S TO INIT
      *INDEX
           MOVE        +1 TO IWX17R.
      *N45NG.    NOTE *TOP OF LOOP                        *.
       F45NG.    IF    IWX17R NOT > IWX17L                              lv20
                 NEXT SENTENCE ELSE GO TO     F45NG-FN.
      *ESTABLISH COMMON FIELDS AGAIN
           MOVE        'CLIENT' TO DH10-MAUSB
           MOVE        +70026 TO DH10-CAUFR
           MOVE        +00001 TO DH10-CAUAC
      *BUILD CX17 PORTION OF RECORD
           MOVE        SPACES TO VA17
           MOVE        LX03-CX03K TO VA17-CX03K
           MOVE        K985-CTID TO VA17-CX06K
           MOVE        CX13-CX13K TO VA17-CX13K
           MOVE        WX17-CX17K (IWX17R) TO VA17-CX17K
           MOVE        WX17-ADBRQF (IWX17R) TO VA17-ADBRQF
           MOVE        WX17-PACT1 (IWX17R) TO VA17-PACT1
           MOVE        WX17-QSHOWQ (IWX17R) TO VA17-QSHOWQ
      *SET UP CALL TO LOG ROUTINE
           MOVE        VA17 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN
      *INCREMENT INDEX TO PROCESS NEXT
      *CX17 THAT WAS INSERTED
           ADD         +1 TO IWX17R.
       F45NG-900. GO TO F45NG.
       F45NG-FN. EXIT.
       F45NB-FN. EXIT.
       F45BB-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *ACTION = MODIFY                    *
      *               *                                   *
      *               *************************************.
       F50.      IF    K985-CACTS = 'C'                                 lv05
                 OR    K985-CACTS = 'I'
                 OR    K985-CACTS = 'R'
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *********************************
      **                              *
      **      M O D I F Y             *
      **                              *
      *********************************
      *N50DB.    NOTE *========= AR1P UPDATE ==========   *.
       F50DB.         EXIT.                                             lv10
      *N50DG.    NOTE *READ CX13 FOR UPDATE               *.
       F50DG.                                                           lv15
           MOVE        07 TO CX13-CARTZ.
                 IF    K985-NAPDS = 0                                   DOT
           MOVE        +1 TO CX13-NAPDS
                 ELSE
           MOVE        K985-NAPDS TO CX13-NAPDS.
           MOVE        CX13-CX13K TO S-CXU13-CX13K                      DOT
           PERFORM     F94C7 THRU F94C7-FN.
      *N50DH.    NOTE *CX13 FOUND                         *.
       F50DH.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F50DH-FN.
           MOVE        CX13 TO SA13.
       F50DH-FN. EXIT.
      *N50DL.    NOTE *CX13 NOT FOUND                     *.
       F50DL.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F50DL-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012009 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50DL-FN. EXIT.
       F50DG-FN. EXIT.
      *N50FB.    NOTE *BUILD CX13 BEFORE MODIFY LOG       *.
       F50FB.                                                           lv15
      ********************************
      ** FOLLOWING STATEMENTS SET UP *
      ** THE AUDIT LOG CONTROL FOR   *
      ** AUDIT LOG FUNCTIONS         *
      ********************************
      *
           MOVE        'CLIENT' TO DH10-MAUSB
           MOVE        +70022 TO DH10-CAUFR
           MOVE        +00002 TO DH10-CAUAC
      *******************************
      ** SET THE AUDIT LOG WORKING
      ** SEQUENCE BECAUSE CAN NOT BE
      ** SURE THAT THE TIME WILL BE
      ** UPDATED IN DBI110.  ALREADY
      ** USED SEQ 00 THRU 03 FOR
      ** CX03,CX06 IN CALL TO CI0091.
      *******************************
           MOVE        +4 TO AL00-NSEQ2P
      *******************************
      ** FOLLOWING STATEMENTS SET UP*
      ** THE AUDIT LOG IMAGE BEFORE *
      ** ANY MODIFICATIONS          *
      *******************************
           MOVE        SPACES TO VA13
           MOVE        S-CXU03-CARTY TO VA13-CARTY
           MOVE        S-CXU03-NARRS TO VA13-NARRS
           MOVE        S-CXU06-CTIDA TO VA13-CTIDA1
           MOVE        S-CXU06-CTIDN TO VA13-NACID1
           MOVE        CX13-CY20 TO VA13-CY20
           MOVE        CX13-CY21 TO VA13-CY21
           MOVE        CX13-CY28 TO VA13-CY28
      *******************************
      ** FOLLOWING ROUTINE CALLS    *
      ** FUNCTION TO PERFORM UPDATE *
      ** OF THE AUDIT LOG DATABASE  *
      *******************************
           MOVE        VA13 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F50FB-FN. EXIT.
      *N50FG.    NOTE ************** CX13 *************   *.
       F50FG.                                                           lv15
      ********************************
      ** SUBFUNCTION UPDATES (CY20)  *
      ** COMMON PART FOR THE CATS    *
      ** ARRANGEMENT SEGMENT CX13    *
      ********************************
      *
      *KA57 DOES NOT PUT IN ADBRQ
      *I DO BECAUSE CHANGE OF TOTAL
      *VALUE IS ALLOWED IN OST
           MOVE        K985-ADBRQ TO CX13-ADBRQ
           CX13-ACOT1
           MOVE        K985-DNPMT TO CX13-DNPMT
           CX13-DNEXE
           7-WS-DATE
      ** KEEP OLD CX13-GESTD
      ** GET PAYMENT DAY
           MOVE        7-WS-DTDD TO CX13-DOPDA.
                 IF    CX13-DLAUP NOT = NS20-DCACG                      DOT
           MOVE        CX13-DLAUP TO CX13-DPCHD
           MOVE        NS20-DCACG TO CX13-DLAUP.
           MOVE        K985-GEEND TO CX13-GEEND                         DOT
           MOVE        ACF-USER-ID TO CX13-GEOPD2
           MOVE        ACF-USER-UNIT TO CX13-GEAUN
           MOVE        K985-CPMTF TO CX13-CPMTF.
                 IF    K985-CACTS = 'C'                                 DOT
      *USE QT58 STATUS FOR MODIFY
           MOVE        K985-CDEST TO CX13-CDEST.
                 IF    K985-CACTS = 'I'                                 DOT
      *SET TO INACTIVE STATUS
           MOVE        03 TO CX13-CDEST.
                 IF    K985-CACTS = 'R'                                 DOT
      *SET TO ACTIVE STATUS
           MOVE        01 TO CX13-CDEST.
       F50FG-FN. EXIT.
      *N50FL.    NOTE *  ADD LOGIC HERE FOR IIARR         *.
       F50FL.                                                           lv15
           MOVE        'Y' TO CX13-IIARR.
       F50FL-FN. EXIT.
      *N50HB.    NOTE *REWRITE CATS ARRGMT DETAIL(CX13)   *.
       F50HB.                                                           lv15
      *
           PERFORM     F94C6 THRU F94C6-FN
           PERFORM     F91AA THRU F91AA-FN.
                 IF    IK = '1'                                         DOT
      *REWRITE FAILED - CRITICAL
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012075 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50HB-FN. EXIT.
      *N50HG.    NOTE *REQUEST CONFIRMATION MISC TRAN     *.
       F50HG.    IF    (CX13-CPMTF NOT = SA13-CPMTF                     lv15
                 OR    CX13-DNPMT NOT = SA13-DNPMT)
                 AND   K985-CTIDA NOT = 005
                 NEXT SENTENCE ELSE GO TO     F50HG-FN.
      *CHANGED FREQ OR NEXT PYMT DATE
           MOVE        'C' TO 7-AOACFM-CRETP
           MOVE        ZERO TO 7-AOACFM-CACKD
           PERFORM     F91BB THRU F91BB-FN.
       F50HG-FN. EXIT.
      *N50HL.    NOTE ***  BUILD CX17 WORK TABLE    **    *.
       F50HL.    IF    K985-CACTS = 'C'                                 lv15
                 OR    K985-CACTS = 'R'
                 NEXT SENTENCE ELSE GO TO     F50HL-FN.
           MOVE        +1 TO IWX17R
           IDX
           INITIALIZE  7-WX17-TABLE.
      *N50JB.    NOTE *TOP OF LOOP                        *.
       F50JB.                       GO TO     F50JB-B.                  lv20
       F50JB-A.
                 IF    IK = '1'
                 OR    IWX17R > IWX17M
                                    GO TO     F50JB-FN.
       F50JB-B.
      **************
           MOVE        '> ' TO S-CXU17-OPER
      *READ CX17 GHN
           PERFORM     F94D5 THRU F94D5-FN
           MOVE        '= ' TO S-CXU17-OPER.
                 IF    IK = '0'                                         DOT
      *FOUND CX17 RECORD
           MOVE        CX17 TO WX17 (IWX17R)
           ADD         +1 TO IWX17R.
       F50JB-900. GO TO F50JB-A.
       F50JB-FN. EXIT.
      *N50JG.    NOTE *SAVE NUMBER OF CX17'S FOUND        *.
       F50JG.                                                           lv20
           COMPUTE     IWX17L = IWX17R - +1.
       F50JG-FN. EXIT.
      *N50JL.    NOTE *PERFORM AUDIT LOG PROCESSING       *.
       F50JL.                                                           lv20
      *******************************
      ** THIS ROUTINE WILL LOG ALL  *
      ** CHANGES TO CX17 FOR EACH   *
      ** FUND                       *
      *******************************
           MOVE        +0 TO IWX17R.
       F50JL-FN. EXIT.
      *N50LB.    NOTE *LOOP & COMPARE OLD TO NEW          *.
       F50LB.                                                           lv20
           MOVE        1                        TO J50LBR
                                    GO TO     F50LB-B.
       F50LB-A.
           ADD         1                        TO J50LBR.
       F50LB-B.
           IF          J50LBR                   >  WS-QSACT
                                    GO TO     F50LB-FN.
      *N50LG.    NOTE *PROCESS INPUT SOURCE FUND          *.
       F50LG.    IF    K985-CACCT (J50LBR) = 'F'                        lv25
                 AND   K985-ADBRQM (J50LBR)
                       = ZEROES
                 NEXT SENTENCE ELSE GO TO     F50LG-FN.
      *NOT PART OF INPUT ARRANGEMENT
      *IT WILL BE DELETED FROM CX17
           PERFORM     F97FB THRU F97FB-FN.
       F50LG-FN. EXIT.
      *N50LL.    NOTE *PROCESS INPUT SOURCE FUND          *.
       F50LL.    IF    K985-CACCT (J50LBR) = 'F'                        lv25
                 AND   K985-ADBRQM (J50LBR)
                       > ZEROES
                 NEXT SENTENCE ELSE GO TO     F50LL-FN.
      *PART OF NEW INPUT ARRANGEMENT
      *IT WILL BE UPDATED
           MOVE        K985-ADBRQM (J50LBR) TO
           7-HOLD-ADBCRQ
           PERFORM     F97HB THRU F97HB-FN.
       F50LL-FN. EXIT.
       F50LB-900. GO TO F50LB-A.
       F50LB-FN. EXIT.
      *N50NB.    NOTE *SAVE THE NUMBER OF CX17'S BUILT    *.
       F50NB.                                                           lv20
           MOVE        IWX17R TO IWX17L.
       F50NB-FN. EXIT.
       F50HL-FN. EXIT.
      *N50VB.    NOTE *FORMAT CX13 AFTER IMAGE            *.
       F50VB.                                                           lv15
      ********************************
      ** FOLLOWING STATEMENTS SET UP *
      ** THE AUDIT LOG IMAGE AFTER   *
      ** ANY MODIFICATIONS           *
      ********************************
           MOVE        SPACES TO VA13
           MOVE        'CLIENT' TO DH10-MAUSB
           MOVE        +70022 TO DH10-CAUFR
           MOVE        +00003 TO DH10-CAUAC
           MOVE        S-CXU03-CARTY TO VA13-CARTY
           MOVE        S-CXU03-NARRS TO VA13-NARRS
           MOVE        S-CXU06-CTIDA TO VA13-CTIDA1
           MOVE        S-CXU06-CTIDN TO VA13-NACID1
           MOVE        CX13-CY20 TO VA13-CY20
           MOVE        CX13-CY21 TO VA13-CY21
           MOVE        CX13-CY28 TO VA13-CY28.
       F50VB-FN. EXIT.
      *N50VG.    NOTE *AUDIT TRAIL ROUTINE                *.
       F50VG.                                                           lv15
      ********************************
      ** FOLLOWING ROUTINE CALLS     *
      ** FUNCTION TO PERFORM UPDATE  *
      ** OF THE AUDIT LOG DATABASE   *
      ********************************
      *
           MOVE        VA13 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F50VG-FN. EXIT.
       F50DB-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *DISBURSEMENT PROCESSING            *
      *               *                                   *
      *               *************************************.
       F55.      IF    K985-CACTS = 'A'                                 lv05
                 OR    K985-CACTS = 'C'
                 OR    K985-CACTS = 'R'
                 NEXT SENTENCE ELSE GO TO     F55-FN.
      *********************************
      **                              *
      **        U P D A T E           *
      **        DESTINATION           *
      **       SUB-ACCOUNTS           *
      **    ADD & MODIFY WILL DIFFER  *
      **                              *
      *********************************
      *N55BB.    NOTE *SET UP                             *.
       F55BB.                                                           lv10
           MOVE        'N' TO CX14-FOUND.
       F55BB-FN. EXIT.
      *N55BG.    NOTE ********* READ CX14 **********      *.
       F55BG.                                                           lv10
      ********************************
      ** FOLLOWING STATEMENTS SET UP *
      *
           MOVE        K985-CTID TO S-CXU06-CX06K
           MOVE        CX13-CX13K TO S-CXU13-CX13K
           MOVE        +001 TO S-CXU14-NPISQ
      *READ CX14
           PERFORM     F94E1 THRU F94E1-FN.
                 IF    IK = '0'                                         DOT
           MOVE        'Y' TO CX14-FOUND.
       F55BG-FN. EXIT.
      *N55BL.    NOTE *ERROR IF MODIFY AND NO CX14        *.
       F55BL.    IF    (K985-CACTS = 'C'                                lv10
                 OR    K985-CACTS = 'R')
                 AND   (CX14-FOUND = 'N')
                 NEXT SENTENCE ELSE GO TO     F55BL-FN.
      **
      **
      **
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012009 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F55BL-FN. EXIT.
      *N55DB.    NOTE *SKIP, IF VANTAGE VUL               *.
       F55DB.    IF    K985-IVANT = 'Y'                                 lv10
                 OR    (K985-PRCOD = 00221
                 OR    K985-PRCOD = 00222
                 OR    K985-PRCOD = 00223)
                 NEXT SENTENCE ELSE GO TO     F55DB-FN.
      **
      **
      **
       F55DB-900. GO TO F55DG-FN.
       F55DB-FN. EXIT.
      *N55DG.    NOTE *ELSE, PROCESS LIFE FIELDS          *.
       F55DG.         EXIT.                                             lv10
      *N55DL.    NOTE *INNOVEST                           *.
       F55DL.    IF    K985-ALINNO NOT = 0 AND 2                        lv15
                 NEXT SENTENCE ELSE GO TO     F55DL-FN.
      *SET POLICY DATE TO CURRENT ANNV
           MOVE        K985-ALPLDT TO 7-WK-ALPLDT
           MOVE        K985-DNPMT TO 7-WK-DNPMT
           MOVE        7-WK-DYY TO 7-WK-AYY.
                 IF    7-WK-ALPLDT > 7-WK-DNPMT                         DOT
           SUBTRACT    1 FROM 7-WK-AYY.
       F55DL-FN. EXIT.
       F55DG-FN. EXIT.
      *N55FB.    NOTE *WRITE AUDIT LOG BEFORE - CX14      *.
       F55FB.    IF    K985-CACTS = 'C'                                 lv10
                 OR    K985-CACTS = 'R'
                 NEXT SENTENCE ELSE GO TO     F55FB-FN.
      ***
           PERFORM     F96JG THRU F96JG-FN.
       F55FB-FN. EXIT.
      *N55HB.    NOTE ******** LOAD  CX14 *************   *.
       F55HB.                                                           lv10
           MOVE        SPACES TO CX14
           MOVE        +1 TO CX14-NPISQ
           MOVE        ZEROES TO CX14-PPOTD
           MOVE        K985-ADBRQ TO CX14-ACOTD
           MOVE        ZEROES TO CX14-QPSTD
           MOVE        +57 TO CX14-GELL
           MOVE        02 TO CX14-CPITC
           MOVE        'Y' TO CX14-IOWNC
           MOVE        'T' TO CX14-CTYPE
           MOVE        K985-CTID TO CX14-CTID
           MOVE        ZERO TO CX14-CPMTC
           MOVE        'N' TO CX14-IACSD.
       F55HB-FN. EXIT.
      *N55HG.    NOTE *FORMAT COMMON AREAS - AUDIT LOG    *.
       F55HG.                                                           lv10
      *********************************
      ** SET UP LOG CX14 LOG RECORD   *
      ** FOR BOTH CREATE A NEW CX14   *
      ** AND THE AFTER MODIFY CX14.   *
      *********************************
      ************ DH10 ***************
           MOVE        'CLIENT' TO DH10-MAUSB
      *DH10 FIELDS FOR VA14 SEGMENT
           MOVE        +70023 TO DH10-CAUFR
           MOVE        +001 TO VA14-NPISQ
      ************** VA14 *************
           MOVE        LX03-CX03K TO VA14-CX03K
           MOVE        K985-CTID TO VA14-CX06K
           MOVE        CX13-CX13K TO VA14-CX13K
      *CX14 KEY IS MOVED IN AFTER A
      *SUCCESFUL INSERT (NPISQ)
           MOVE        CX14-ACOTD TO VA14-ACOTD
           MOVE        CX14-PPOTD TO VA14-PPOTD
           MOVE        CX14-QPSTD TO VA14-QPSTD
           MOVE        CX14-CPITC TO VA14-CPITC
           MOVE        CX14-CY30 TO VA14-CY30.
       F55HG-FN. EXIT.
      *N55JB.    NOTE *REPOSITION FOR CX14 INSERT         *.
       F55JB.    IF    K985-CACTS = 'A'                                 lv10
                 AND   CX14-FOUND = 'N'
                 NEXT SENTENCE ELSE GO TO     F55JB-FN.
      *******************************
           MOVE        CX13-CX13K TO S-CXU13-CX13K
           PERFORM     F94C4 THRU F94C4-FN.
      *N55JD.    NOTE *CRITICAL ERROR IF NOT FOUND        *.
       F55JD.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F55JD-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012009 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F55JD-FN. EXIT.
      *N55JG.    NOTE *INSERT CX14 FOR CREATE             *.
       F55JG.                                                           lv15
           PERFORM     F94E2 THRU F94E2-FN
           PERFORM     F91AA THRU F91AA-FN.
                 IF    IK = '0'                                         DOT
           MOVE        'Y' TO CX14-FOUND.
       F55JG-FN. EXIT.
      *N55JL.    NOTE *WRITE AUD LOG CREATE - VA14        *.
       F55JL.    IF    CX14-FOUND = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F55JL-FN.
      *MOVE VARIABLE DATA TO AUDIT LOG
           MOVE        +00001 TO DH10-CAUAC
           MOVE        CX14-NPISQ TO VA14-NPISQ
           MOVE        VA14 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F55JL-FN. EXIT.
       F55JB-FN. EXIT.
      *N55LG.    NOTE *REWRITE MODIFY RECORD              *.
       F55LG.    IF    K985-CACTS = 'C'                                 lv10
                 OR    K985-CACTS = 'R'
                 NEXT SENTENCE ELSE GO TO     F55LG-FN.
      ********************************
      ********************************
           PERFORM     F94E3 THRU F94E3-FN.
      *N55LQ.    NOTE *WRITE AUDIT LOG AFTER - VA14       *.
       F55LQ.                                                           lv15
      *MOVE VARIABLE DATA TO AUDIT REC
           MOVE        +00003 TO DH10-CAUAC
           MOVE        CX14-NPISQ TO VA14-NPISQ
           MOVE        VA14 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F55LQ-FN. EXIT.
       F55LG-FN. EXIT.
      *N55NB.    NOTE *SET UP CX15 LOOP                   *.
       F55NB.                                                           lv10
      *
           MOVE        CX14-CX14K TO S-CXU14-CX14K
           INITIALIZE  SA15
           MOVE        +1 TO SUB1.
      *N55NG.    NOTE *CURRENT REPT. NOT > TOTAL REPT.    *.
       F55NG.                       GO TO     F55NG-B.                  lv15
       F55NG-A.
                 IF    IK = '1'
                 OR    SUB1 > WS-QSACT
                                    GO TO     F55NG-FN.
       F55NG-B.       EXIT.
      *N55NL.    NOTE *** READ CX15--FIRST 'TO' FUND **   *.
       F55NL.    IF    K985-CACCT (SUB1) = 'T'                          lv20
                 AND   K985-CFIDC (SUB1)
                       NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F55NL-FN.
      *GHU
           MOVE        K985-CFIDC (SUB1) TO S-CXU15-CFIDC
           PERFORM     F94F1 THRU F94F1-FN.
                 IF    IK = '0'                                         DOT
           MOVE        'Y' TO CX15-FOUND
                 ELSE
           MOVE        'N' TO CX15-FOUND.
      *N55PB.    NOTE ***NO CX15 - INSERT NEW SEGMENT**   *.
       F55PB.    IF    CX15-FOUND = 'N'                                 lv25
                 AND   K985-ADBRQM (SUB1)
                       > ZEROES
                 NEXT SENTENCE ELSE GO TO     F55PB-FN.
           INITIALIZE  CX15.
           MOVE        K985-CFIDC (SUB1) TO CX15-CFIDC                  DOT
           MOVE        K985-ADBRQM (SUB1) TO CX15-ACOTD
           MOVE        ZEROES TO CX15-PPOTD
           CX15-QPSTD
           PERFORM     F94F3 THRU F94F3-FN.
                 IF    IK = '0'                                         DOT
           PERFORM     F96LG THRU F96LG-FN
           MOVE        VA15 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F55PB-FN. EXIT.
      *N55PG.    NOTE ****** UPDATE CX15 SEGMENT  ****    *.
       F55PG.    IF    CX15-FOUND = 'Y'                                 lv25
                 NEXT SENTENCE ELSE GO TO     F55PG-FN.
      ** FOUND A CX15
      *N55PL.    NOTE ****ENTERED AMOUNT IS ZERO *****    *.
       F55PL.    IF    K985-ADBRQM (SUB1) = ZERO                        lv30
                 NEXT SENTENCE ELSE GO TO     F55PL-FN.
      *DELETE CX15
      *
           MOVE        K985-CFIDC (SUB1) TO CX15-CFIDC
           MOVE        CX15 TO SA15
           PERFORM     F94F2 THRU F94F2-FN.
                 IF    IK = '0'                                         DOT
      *AUDIT LOG
           PERFORM     F96NG THRU F96NG-FN
           MOVE        VA15 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F55PL-FN. EXIT.
      *N55RB.    NOTE ****ENTERED AMOUNT IS NOT ZERO**    *.
       F55RB.    IF    K985-ADBRQM (SUB1) NOT = ZERO                    lv30
                 NEXT SENTENCE ELSE GO TO     F55RB-FN.
      *SUBTRACT PREVIOUS AMOUNT AND
      *UPDATE WITH NEW DISBURSEMENT AMT
      *SAVE AMOUNT FOR AUDIT LOG UPDATE
      *
           SUBTRACT    CX15-ACOTD FROM 7-CX15-ACOTD
           MOVE        ZERO TO CX15-PPOTD
           CX15-QPSTD
           MOVE        K985-CFIDC (SUB1) TO CX15-CFIDC
           MOVE        K985-ADBRQM (SUB1) TO CX15-ACOTD
           PERFORM     F94F4 THRU F94F4-FN.
                 IF    IK = '0'                                         DOT
      *****************************
      *BEFORE AUDIT LOG FOR CX15
      *****************************
           PERFORM     F96LL THRU F96LL-FN
           MOVE        VA15 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN
      ******************************
      *AFTER AUDIT LOG FOR CX15
      ******************************
           PERFORM     F96NB THRU F96NB-FN
           MOVE        VA15 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F55RB-FN. EXIT.
       F55PG-FN. EXIT.
       F55NL-FN. EXIT.
      *N55RL.    NOTE *** INCREMENT SUB1                  *.
       F55RL.                                                           lv20
           ADD         +1 TO SUB1.
       F55RL-FN. EXIT.
       F55NG-900. GO TO F55NG-A.
       F55NG-FN. EXIT.
       F55NB-FN. EXIT.
       F55-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *UPDATE GQ01 WITH ARRANGEMENT KEY   *
      *               *                                   *
      *               *************************************.
       F65.           EXIT.                                             lv05
      *N65AB.    NOTE *GET GQ01-GELL VALUE FROM           *.
       F65AB.                                                           lv10
      *PACTABLE TA75
           PERFORM     F97EB THRU F97EB-FN
           MOVE        ZEROS TO S-GQU01-GESQ2
           IK.
       F65AB-FN. EXIT.
      *N65DA.    NOTE *READ ORDER TICKET RECORDS          *.
       F65DA.                       GO TO     F65DA-B.                  lv10
       F65DA-A.
                 IF    IK = '1'
                 OR    WS-CF = '1'
                                    GO TO     F65DA-FN.
       F65DA-B.
           MOVE        00100 TO S-GQU01-CAMCTR
           MOVE        K985-CTID TO S-GQU01-CANUMB
           ADD         01 TO S-GQU01-GESQ2
           INITIALIZE  WS1A
           PERFORM     F94T6 THRU F94T6-FN
           MOVE        GQ01-XMISL TO WS1A.
      *N65DD.    NOTE *REPLACE GQ01 WITH ARRANGEMENT      *.
       F65DD.    IF    GQ01-GEOPID = K985-GEOPD2                        lv15
                 AND   WS1A-GETIM3 = K985-GETOD
                 AND   WS1A-CTRHO = K985-CTRHO
                 NEXT SENTENCE ELSE GO TO     F65DD-FN.
      *KEYS ADDED IN XMISL
           MOVE        '1' TO WS-CF
           MOVE        WS-GELL TO GQ01-GELL
           MOVE        GQ01-XMISL TO GS1A
           MOVE        WS-CX01K TO GS1A-CX01K
           MOVE        WS-CX03K TO GS1A-CX03K
           MOVE        WS-CX06K TO GS1A-CX06K
           MOVE        WS-CX13K TO GS1A-CX13K
           MOVE        GS1A TO GQ01-XMISL
           PERFORM     F94T7 THRU F94T7-FN.
                 IF    IK = '0'                                         DOT
           PERFORM     F94T5 THRU F94T5-FN.
       F65DD-FN. EXIT.
       F65DA-900. GO TO F65DA-A.
       F65DA-FN. EXIT.
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
      *               *CALLED MODULES                     *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91AA.    NOTE *MISCELLANEOUS TRAN PROCESSING      *.
       F91AA.                                                           lv10
      *
           MOVE        XW05-XCOKEY (1:23) TO WS-CX01K
           MOVE        XW05-XCOKEY (24:4) TO WS-CX03K
           MOVE        XW05-XCOKEY (28:27) TO WS-CX06K
           MOVE        XW05-XCOKEY (55:4) TO WS-CX13K.
       F91AA-FN. EXIT.
      *N91BB.    NOTE *MISCELLANEOUS TRAN PROCESSING      *.
       F91BB.         EXIT.                                             lv10
      *N91BG.    NOTE *LOAD MISCELLANEOUS TRAN FIELDS     *.
       F91BG.                                                           lv15
      *INITIALIZE MISC TRAN FIELDS
           INITIALIZE  GQ01
           INITIALIZE  GS51
      *LOAD COMMON MISC TRAN FIELD
           PERFORM     F91QB THRU F91QB-FN
      *LOAD COMMON MISC TRAN FIELD
           PERFORM     F91QG THRU F91QG-FN
      *ATTEMPT WRITE TO MISC TRAN DB
      *OK, IF CAN NOT INSERT
      *IN FUTURE MAY CHECK CRETP FOR
      *TRIGGER TYPES, SEE SDKA57 F35LQ
           PERFORM     F94T2 THRU F94T2-FN.
       F91BG-FN. EXIT.
       F91BB-FN. EXIT.
      *N91QB.    NOTE *FORMAT "COMMON" MISC TRAN          *.
       F91QB.                                                           lv10
      *FORMAT "COMMON" MISC TRAN                                        DOT
           INITIALIZE GQ01                                              ADU033
           MOVE        00051 TO TA75-CAMCTR                             ADU033
           PERFORM     F92TA THRU F92TA-FN                              ADU033
           MOVE        TA75-NDLEN TO GQ01-GELL                          ADU033
           MOVE        K985-CLID TO GQ01-CANUMB                         ADU033
           MOVE        00051 TO GQ01-CAMCTR                             ADU033
           MOVE        1 TO GQ01-GESQ2                                  ADU033
           MOVE        7-AOACFM-CACKD TO GQ01-CACKD                     ADU033
           MOVE        'C' TO GQ01-CENTT                                ADU033
           MOVE        NS20-DCACG TO GQ01-CADATE                        ADU033
           MOVE        EIBTIME TO GQ01-GETIM                            ADU033
           MOVE        ACF-USER-ID TO GQ01-GEOPID                       ADU033
           MOVE        ACF-USER-UNIT TO GQ01-CAUNIT                     ADU033
           MOVE        EIBTRMID TO GQ01-XTERMI                          ADU033
           MOVE        PROGR TO GQ01-CAPPL                              ADU033
           MOVE        'COLA' TO GQ01-CSYS.                             ADU033
       F91QB-FN. EXIT.
      *N91QG.    NOTE *FORMAT GS51                        *.
       F91QG.                                                           lv10
           INITIALIZE GS51                                              DOT
      *FORMAT GS51 FLDS (LINES 203-458)                                 ADU034
           MOVE        7-AOACFM-CRETP TO GS51-CRETP
           MOVE        GS51 TO GQ01-XMISL.                              ADU034
       F91QG-FN. EXIT.
       F91-FN.   EXIT.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *MISC CALLS                         *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92TA.    NOTE *RANDOM TABLE READ FOR TA75         *.            ADUTAB
       F92TA.                                                           lv10
           MOVE        'R1' TO G-TA75-TABFO                             ADUTAB
           COMPUTE     G-TA75-LTH = 60 + G-TA75-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA75-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA75)                                ADUTAB
                       LENGTH (G-TA75-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA75-TABCR NOT = '00'                          DOT
           PERFORM     F92TE THRU F92TE-FN.                             ADUTAB
       F92TA-FN. EXIT.
      *N92TE.    NOTE *ERROR - READ OF TA75               *.
       F92TE.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012207 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F92TE-FN. EXIT.
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
      *N94C3.    NOTE *CALL GU ON CX06                    *.            ADU026
       F94C3.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XC06 CX06                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C3-FN. EXIT.
      *N94C4.    NOTE *CALL GHU ON CX13                   *.            ADU026
       F94C4.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           XC06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C4-FN. EXIT.
      *N94C5.    NOTE *CALL ISRT ON CX13                  *.            ADU026
       F94C5.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XC06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CX13-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C5-FN. EXIT.
      *N94C6.    NOTE *CALL REPL ON CX13                  *.            ADU026
       F94C6.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           XC06 CX13                                                    ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C6-FN. EXIT.
      *N94C7.    NOTE *CALL GHN ON CX13                   *.            ADU026
       F94C7.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHN                        ADU026
           XC06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHN TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C7-FN. EXIT.
      *N94D1.    NOTE *CALL GHU ON CX17                   *.            ADU026
       F94D1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX17' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           XC06 CX17                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           S-CXU17-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94D1-FN. EXIT.
      *N94D2.    NOTE *CALL REPL ON CX17                  *.            ADU026
       F94D2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX17' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           XC06 CX17                                                    ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94D2-FN. EXIT.
      *N94D3.    NOTE *CALL ISRT ON CX17                  *.            ADU026
       F94D3.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX17' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XC06 CX17                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           S-CX17-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94D3-FN. EXIT.
      *N94D4.    NOTE *CALL DLET ON CX17                  *.            ADU026
       F94D4.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX17' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XDLET                       ADU026
           XC06 CX17                                                    ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XDLET TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94D4-FN. EXIT.
      *N94D5.    NOTE *CALL GHN ON CX17                   *.            ADU026
       F94D5.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX17' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHN                        ADU026
           XC06 CX17                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           S-CXU17-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHN TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94D5-FN. EXIT.
      *N94E1.    NOTE *CALL GHU ON CX14                   *.            ADU026
       F94E1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX14' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           XC06 CX14                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           S-CXU14-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94E1-FN. EXIT.
      *N94E2.    NOTE *CALL ISRT ON CX14                  *.            ADU026
       F94E2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX14' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XC06 CX14                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           S-CX14-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94E2-FN. EXIT.
      *N94E3.    NOTE *CALL REPL ON CX14                  *.            ADU026
       F94E3.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX14' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           XC06 CX14                                                    ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94E3-FN. EXIT.
      *N94F1.    NOTE *CALL GHU ON CX15                   *.            ADU026
       F94F1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX15' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           XC06 CX15                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           S-CXU14-SSA S-CXU15-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94F1-FN. EXIT.
      *N94F2.    NOTE *CALL DLET ON CX15                  *.            ADU026
       F94F2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX15' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XDLET                       ADU026
           XC06 CX15                                                    ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XDLET TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94F2-FN. EXIT.
      *N94F3.    NOTE *CALL ISRT ON CX15                  *.            ADU026
       F94F3.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX15' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XC06 CX15                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           S-CXU14-SSA S-CX15-SSA
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94F3-FN. EXIT.
      *N94F4.    NOTE *CALL REPL ON CX15                  *.            ADU026
       F94F4.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX15' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           XC06 CX15                                                    ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94F4-FN. EXIT.
      *N94T2.    NOTE *CALL ISRT ON GQ01                  *.            ADU026
       F94T2.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XF06 GQ01                                                    ADU026
           S-GQ01-SSA                                                   ADU026
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94T2-FN. EXIT.
      *N94T5.    NOTE *CALL REPL ON GQ01                  *.            ADU026
       F94T5.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           XF06 GQ01                                                    ADU026
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94T5-FN. EXIT.
      *N94T6.    NOTE *CALL GN ON GQ01                    *.            ADU026
       F94T6.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XF06 GQ01                                                    ADU026
           S-GQU01-SSA                                                  ADU026
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94T6-FN. EXIT.
      *N94T7.    NOTE *CALL GHU ON WQ01                   *.            ADU026
       F94T7.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'WQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           XF06 WQ01                                                    ADU026
           S-GQU01-SSA                                                  ADU026
           MOVE        XF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94T7-FN. EXIT.
      *N96.      NOTE *************************************.
      *               *                                   *
      *               *---> AUDIT LOG ROUTINES            *
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
           MOVE        'CLIENT' TO DH10-MAUSB                           ADU165
           MOVE        K985-CLID TO DH10-NAUSK                          ADU165
           MOVE        'CATS' TO DH10-CSYS                              ADU165
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
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012038 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
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
      *N96DB.    NOTE *LOG CX17 AFTER IMAGE               *.
       F96DB.                                                           lv10
      *ESTABLISH COMMON FIELDS
           MOVE        'CLIENT' TO DH10-MAUSB
           MOVE        +70026 TO DH10-CAUFR
           MOVE        +00003 TO DH10-CAUAC
      *BUILD CX17 PORTION OF RECORD
           MOVE        LX03-CX03K TO VA17-CX03K
           MOVE        K985-CTID TO VA17-CX06K
           MOVE        SA13-CX13K TO VA17-CX13K
      *FUND CODE LOADED IN F35FL
      *FUND AMOUNT LOADED IN F35FL
           MOVE        ZERO TO VA17-PACT1
           VA17-QSHOWQ
      *SETUP CALL TO AUDIT LOG ROUTINE
           MOVE        VA17 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F96DB-FN. EXIT.
      *N96DG.    NOTE *LOG CX17 BEFORE IMAGE              *.
       F96DG.                                                           lv10
      *ESTABLISH COMMON FIELDS
           MOVE        'CLIENT' TO DH10-MAUSB
           MOVE        +70026 TO DH10-CAUFR
           MOVE        +00002 TO DH10-CAUAC
      *BUILD CX17 PORTION OF RECORD
           MOVE        SPACES TO VA17
           MOVE        LX03-CX03K TO VA17-CX03K
           MOVE        K985-CTID TO VA17-CX06K
           MOVE        SA13-CX13K TO VA17-CX13K
           MOVE        WX17-CX17K (IWX17R) TO VA17-CX17K
           MOVE        WX17-ADBRQF (IWX17R) TO VA17-ADBRQF
           MOVE        WX17-PACT1 (IWX17R) TO VA17-PACT1
           MOVE        WX17-QSHOWQ (IWX17R) TO VA17-QSHOWQ
      *SETUP CALL TO AUDIT LOG ROUTINE
           MOVE        VA17 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F96DG-FN. EXIT.
      *N96DL.    NOTE *LOG CX17 CREATE                    *.
       F96DL.                                                           lv10
      *ESTABLISH COMMON FIELDS
           MOVE        'CLIENT' TO DH10-MAUSB
           MOVE        +70026 TO DH10-CAUFR
           MOVE        +00001 TO DH10-CAUAC
      *BUILD CX17 PORTION OF RECORD
           MOVE        LX03-CX03K TO VA17-CX03K
           MOVE        K985-CTID TO VA17-CX06K
           MOVE        SA13-CX13K TO VA17-CX13K
      *FUND CODE LOADED
      *FUND AMOUNT LOADED
           MOVE        ZERO TO VA17-PACT1
           VA17-QSHOWQ
      *SETUP CALL TO AUDIT LOG ROUTINE
           MOVE        VA17 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F96DL-FN. EXIT.
      *N96FB.    NOTE *LOG CX17 DELETE                    *.
       F96FB.                                                           lv10
      *ESTABLISH COMMON FIELDS
           MOVE        'CLIENT' TO DH10-MAUSB
           MOVE        +70026 TO DH10-CAUFR
           MOVE        +00004 TO DH10-CAUAC
      *BUILD CX17 PORTION OF RECORD
           MOVE        SPACES TO VA17
           MOVE        LX03-CX03K TO VA17-CX03K
           MOVE        K985-CTID TO VA17-CX06K
           MOVE        SA13-CX13K TO VA17-CX13K
           MOVE        WX17-CX17K (IWX17R) TO VA17-CX17K
           MOVE        WX17-ADBRQF (IWX17R) TO VA17-ADBRQF
           MOVE        WX17-PACT1 (IWX17R) TO VA17-PACT1
           MOVE        WX17-QSHOWQ (IWX17R) TO VA17-QSHOWQ
      *SETUP CALL TO AUDIT LOG ROUTINE
           MOVE        VA17 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F96FB-FN. EXIT.
      *N96FG.    NOTE *DELETE CX17                        *.
       F96FG.                                                           lv10
           MOVE        WX17-CFIDC (IWX17R) TO
           S-CXU17-CFIDC
           PERFORM     F94D4 THRU F94D4-FN.
       F96FG-FN. EXIT.
      *N96FL.    NOTE *GET HOLD CX17                      *.
       F96FL.                                                           lv10
           MOVE        WX17-CFIDC (IWX17R) TO
           S-CXU17-CFIDC
           PERFORM     F94D1 THRU F94D1-FN.
       F96FL-FN. EXIT.
      *N96HB.    NOTE *INSERT CX17 SEGMENT                *.
       F96HB.                                                           lv10
           MOVE        VA17-CX17K TO CX17-CX17K
           MOVE        VA17-ADBRQF TO CX17-ADBRQF
           MOVE        ZEROES TO CX17-PACT1
           CX17-QSHOWQ
           PERFORM     F94D3 THRU F94D3-FN.
       F96HB-FN. EXIT.
      *N96HG.    NOTE *REWRITE CX17                       *.
       F96HG.                                                           lv10
           MOVE        VA17-ADBRQF TO CX17-ADBRQF
           MOVE        VA17-ADBRQF TO CX17-ADBRQF
           PERFORM     F94D2 THRU F94D2-FN.
       F96HG-FN. EXIT.
      *N96JG.    NOTE *FORMAT AUDIT LOG BEFORE - CX14     *.
       F96JG.                                                           lv10
      ************ DH10 ***************
           MOVE        'CLIENT' TO DH10-MAUSB
      *DH10 FIELDS FOR VA14 SEGMENT
           MOVE        +70023 TO DH10-CAUFR
           MOVE        +00002 TO DH10-CAUAC
           MOVE        LX03-CX03K TO VA14-CX03K
           MOVE        K985-CTID TO VA14-CX06K
           MOVE        CX13-CX13K TO VA14-CX13K
           MOVE        CX14-NPISQ TO VA14-NPISQ
           MOVE        CX14-ACOTD TO VA14-ACOTD
           MOVE        CX14-PPOTD TO VA14-PPOTD
           MOVE        CX14-QPSTD TO VA14-QPSTD
           MOVE        CX14-CPITC TO VA14-CPITC
           MOVE        CX14-CY30 TO VA14-CY30
           MOVE        VA14 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F96JG-FN. EXIT.
      *N96LG.    NOTE *FORMAT VA15 AUDIT LOG RECORD       *.
       F96LG.                                                           lv10
      *INSERT CX15 SEGMENT
           MOVE        'CLIENT' TO DH10-MAUSB
           MOVE        LX01-CX01K TO DH10-NAUSK
           MOVE        +70027 TO DH10-CAUFR
           MOVE        +00001 TO DH10-CAUAC
           INITIALIZE  VA15
           MOVE        LX03-CX03K TO VA15-CX03K
           MOVE        K985-CTID TO VA15-CX06K
           MOVE        CX13-CX13K TO VA15-CX13K
           MOVE        CX14-CX14K TO VA15-CX14K
           MOVE        CX15-CFIDC TO VA15-CFIDC
           MOVE        CX15-ACOTD TO VA15-ACOTD.
       F96LG-FN. EXIT.
      *N96LL.    NOTE *FORMAT VA15 AUDIT LOG RECORD       *.
       F96LL.                                                           lv10
      *BEFORE CX15 UPDATE IMAGE
           MOVE        'CLIENT' TO DH10-MAUSB
           MOVE        LX01-CX01K TO DH10-NAUSK
           MOVE        +70027 TO DH10-CAUFR
           MOVE        +00002 TO DH10-CAUAC
           INITIALIZE  VA15
           MOVE        LX03-CX03K TO VA15-CX03K
           MOVE        K985-CTID TO VA15-CX06K
           MOVE        CX13-CX13K TO VA15-CX13K
           MOVE        CX14-CX14K TO VA15-CX14K
           MOVE        CX15-CFIDC TO VA15-CFIDC
           MOVE        7-CX15-ACOTD TO VA15-ACOTD.
       F96LL-FN. EXIT.
      *N96NB.    NOTE *FORMAT VA15 AUDIT LOG RECORD       *.
       F96NB.                                                           lv10
      *AFTER CX15 UPDATE IMAGE
           MOVE        'CLIENT' TO DH10-MAUSB
           MOVE        CX01-CX01K TO DH10-NAUSK
           MOVE        +70027 TO DH10-CAUFR
           MOVE        +00003 TO DH10-CAUAC
           INITIALIZE  VA15
           MOVE        LX03-CX03K TO VA15-CX03K
           MOVE        K985-CTID TO VA15-CX06K
           MOVE        CX13-CX13K TO VA15-CX13K
           MOVE        CX14-CX14K TO VA15-CX14K
           MOVE        CX15-CFIDC TO VA15-CFIDC
           MOVE        CX15-ACOTD TO VA15-ACOTD.
       F96NB-FN. EXIT.
      *N96NG.    NOTE *FORMAT VA15 AUDIT LOG RECORD       *.
       F96NG.                                                           lv10
      *DELETE CX15 SEGMENT
           MOVE        'CLIENT' TO DH10-MAUSB
           MOVE        LX01-CX01K TO DH10-NAUSK
           MOVE        +70027 TO DH10-CAUFR
           MOVE        +00004 TO DH10-CAUAC
           INITIALIZE  VA15
           MOVE        LX03-CX03K TO VA15-CX03K
           MOVE        K985-CTID TO VA15-CX06K
           MOVE        CX13-CX13K TO VA15-CX13K
           MOVE        CX14-CX14K TO VA15-CX14K
           MOVE        CX15-CFIDC TO VA15-CFIDC
           MOVE        CX15-ACOTD TO VA15-ACOTD.
       F96NG-FN. EXIT.
       F96-FN.   EXIT.
      *N97.      NOTE *************************************.
      *               *                                   *
      *               *CALLED ROUTINES                    *
      *               *                                   *
      *               *************************************.
       F97.           EXIT.                                             lv05
      *N97EB.    NOTE *SEE IF THEY DELETED AN AMOUNT      *.
       F97EB.                                                           lv10
           INITIALIZE  GQ01
           KY00
           MOVE        00100 TO TA75-CAMCTR
      *SET VARIABLE SEGMENT LENGTH
           PERFORM     F92TA THRU F92TA-FN
           MOVE        TA75-NDLEN TO WS-GELL.
       F97EB-FN. EXIT.
      *N97FB.    NOTE *SEE IF THEY DELETED AN AMOUNT      *.
       F97FB.                                                           lv10
           MOVE        ZERO TO IWX17R
           MOVE        'N' TO CX17-FOUND.
      *N97FG.    NOTE *SEARCH CX17 FOR EXISTING FUND      *.
       F97FG.                       GO TO     F97FG-B.                  lv15
       F97FG-A.
                 IF    IWX17R > IWX17L
                 OR    CX17-FOUND = 'Y'
                                    GO TO     F97FG-FN.
       F97FG-B.
           ADD         +1 TO IWX17R.
                 IF    K985-CFIDC (J50LBR) =                            DOT
                       WX17-CFIDC (IWX17R)
                 AND   K985-CFIDC (J50LBR)
                       NOT = SPACES
           MOVE        'Y' TO CX17-FOUND.
       F97FG-900. GO TO F97FG-A.
       F97FG-FN. EXIT.
      *N97FL.    NOTE *LOG CX17 DELETE, THEN DELETE       *.
       F97FL.    IF    CX17-FOUND = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F97FL-FN.
           PERFORM     F96FB THRU F96FB-FN
      *READ CX17 FOR UPDATE
           PERFORM     F96FL THRU F96FL-FN
      *THEN, DELETE CX17
           PERFORM     F96FG THRU F96FG-FN
      *THEN, WIPE IT OUT OF WX17
           MOVE        SPACES TO WX17-CFIDC (IWX17R).
       F97FL-FN. EXIT.
       F97FB-FN. EXIT.
      *N97HB.    NOTE *CHECK IF ADD OR CHANGE             *.
       F97HB.                                                           lv10
           MOVE        ZERO TO IWX17R
           MOVE        'N' TO CX17-FOUND.
      *N97HG.    NOTE *SEARCH CX17 TABLE FOR FUND ID      *.
       F97HG.                       GO TO     F97HG-B.                  lv15
       F97HG-A.
                 IF    IWX17R > IWX17L
                 OR    CX17-FOUND = 'Y'
                                    GO TO     F97HG-FN.
       F97HG-B.
           ADD         +1 TO IWX17R.
                 IF    K985-CFIDC (J50LBR) =                            DOT
                       WX17-CFIDC (IWX17R)
           MOVE        'Y' TO CX17-FOUND.
       F97HG-900. GO TO F97HG-A.
       F97HG-FN. EXIT.
      *N97HL.    NOTE *CX17 FOUND,  PROCESS A CHANGE      *.
       F97HL.    IF    CX17-FOUND = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F97HL-FN.
      *********************************
      **                              *
      **       C H A N G E            *
      **                              *
      *********************************
      *N97JB.    NOTE *AMOUNT WAS CHANGED                 *.
       F97JB.    IF    WX17-ADBRQF (IWX17R) NOT =                       lv20
                       7-HOLD-ADBCRQ
                 NEXT SENTENCE ELSE GO TO     F97JB-FN.
      *N97JG.    NOTE *LOG CX17 BEFORE IMAGE              *.
       F97JG.                                                           lv25
           PERFORM     F96DG THRU F96DG-FN.
       F97JG-FN. EXIT.
      *N97JL.    NOTE *LOG CX17 AFTER IMAGE               *.
       F97JL.                                                           lv25
           MOVE        SPACES TO VA17
           MOVE        K985-CFIDC (J50LBR) TO
           VA17-CX17K
           MOVE        7-HOLD-ADBCRQ TO VA17-ADBRQF
           PERFORM     F96DB THRU F96DB-FN.
       F97JL-FN. EXIT.
      *N97LB.    NOTE *UPDATE CX17 SEGMENT                *.
       F97LB.                                                           lv25
      *READ FOR UPDATE
           PERFORM     F96FL THRU F96FL-FN
      *REWRITE
           PERFORM     F96HG THRU F96HG-FN.
       F97LB-FN. EXIT.
       F97JB-FN. EXIT.
       F97HL-900. GO TO F97LG-FN.
       F97HL-FN. EXIT.
      *N97LG.    NOTE *LOG CX17 CREATE, THEN CREATE IT    *.
       F97LG.                                                           lv15
      *********************************
      **                              *
      **   I N S E R T                *
      **                              *
      *********************************
           MOVE        SPACES TO VA17
           MOVE        K985-CFIDC (J50LBR) TO
           VA17-CX17K
           MOVE        7-HOLD-ADBCRQ TO VA17-ADBRQF
           PERFORM     F96DL THRU F96DL-FN
      *INSERT CX17 SEGMENT
           PERFORM     F96HB THRU F96HB-FN.
       F97LG-FN. EXIT.
       F97HB-FN. EXIT.
       F97-FN.   EXIT.
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
