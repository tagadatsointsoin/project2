       IDENTIFICATION DIVISION.                                         CI0115
       PROGRAM-ID.  CI0115P.                                            CI0115
      *AUTHOR.         BA - OWNERSHIP COMPARE MOD.                      CI0115
      *DATE-COMPILED.   09/08/14.                                       CI0115
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2014                          *ACOPYP
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
      *     COPR. 2014                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0115
       CONFIGURATION SECTION.                                           CI0115
       SOURCE-COMPUTER. IBM-370.                                        CI0115
       OBJECT-COMPUTER. IBM-370.                                        CI0115
       DATA DIVISION.                                                   CI0115
       WORKING-STORAGE SECTION.                                         CI0115
      *WORKING STORAGE TABLE FOR STORING THE OWNRSHIP INFORMATION
       01                 WTBA-TABLE.
      *!WI
           05             WTBA-QITEM
                        PICTURE 9(3).                                   CI0115
           05             WTBA        OCCURS 300.
              10          WTBA-SRCH.
      *!WI
                 15       WTBA-CTTLN1
                        PICTURE X(30).                                  CI0115
      *!WI
                 15       WTBA-CTTLN2
                        PICTURE X(30).                                  CI0115
      *!WI
                 15       WTBA-CTTLN3
                        PICTURE X(30).                                  CI0115
      *!WI
                 15       WTBA-CTTBO1
                        PICTURE X(45).                                  CI0115
      *!WI
                 15       WTBA-CTTBO2
                        PICTURE X(45).                                  CI0115
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0218           PIC X(08)  VALUE 'CI0218P '.
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
      **** LAYOUTS FOR THE ACCOUNT AND ARRANGEMENT TSQ

      *!WF DSP=QT DSL=QT SEL=58 FOR=I DES=1 LEV=1 PLT=QT
       01                 QT58.                                         CI0115
            10            QT58-QT5K.                                    CI0115
            11            QT58-C299.                                    CI0115
            12            QT58-CTID.                                    CI0115
            13            QT58-CTIDA  PICTURE  9(3).                    CI0115
            13            QT58-CTIDN.                                   CI0115
            14            QT58-CTIDNP PICTURE  X(13).                   CI0115
            14            QT58-CTIDND PICTURE  9(11).                   CI0115
            11            QT58-GECKD2 PICTURE  9.                       CI0115
            11            QT58-NSEQ5  PICTURE  9(5).                    CI0115
            11            QT58-CTSTA  PICTURE  99.                      CI0115
            11            QT58-CTSTAL PICTURE  X(10).                   CI0115
            11            QT58-CTOWN  PICTURE  9(3).                    CI0115
            11            QT58-CTTLN1 PICTURE  X(30).                   CI0115
            11            QT58-CTTLN2 PICTURE  X(30).                   CI0115
            11            QT58-CTTLN3 PICTURE  X(30).                   CI0115
            11            QT58-CTTBO1 PICTURE  X(45).                   CI0115
            11            QT58-CTTBO2 PICTURE  X(45).                   CI0115
            11            QT58-CTEFD  PICTURE  9(8).                    CI0115
            11            QT58-CTIAD  PICTURE  9(8).                    CI0115
            11            QT58-CTCUS  PICTURE  999.                     CI0115
            11            QT58-GR98.                                    CI0115
            12            QT58-GRID.                                    CI0115
            13            QT58-GRIDC  PICTURE  9(3).                    CI0115
            13            QT58-GRIDN.                                   CI0115
            14            QT58-GRIDNP PICTURE  99.                      CI0115
            14            QT58-GRIDND PICTURE  9(8).                    CI0115
            11            QT58-CQACT  PICTURE  999.                     CI0115
            11            QT58-CTCCI  PICTURE  X.                       CI0115
            11            QT58-CIRAS  PICTURE  999.                     CI0115
            11            QT58-CIRAT  PICTURE  999.                     CI0115
            11            QT58-IACVD  PICTURE  X.                       CI0115
            11            QT58-FILLER PICTURE  X(4).                    CI0115
            11            QT58-PRCODA PICTURE  X(5).                    CI0115
            11            QT58-PRCMN  PICTURE  X(20).                   CI0115
            11            QT58-MRPLN  PICTURE  X(30).                   CI0115
            11            QT58-CPRDG  PICTURE  9(2).                    CI0115
            11            QT58-CPRDA1 PICTURE  9(3).                    CI0115
            11            QT58-PRSCD  PICTURE  X(9).                    CI0115
            11            QT58-MSP03  PICTURE  X(3).                    CI0115
            11            QT58-CGRLI  PICTURE  X.                       CI0115
            11            QT58-ITERM  PICTURE  X(1).                    CI0115
            11            QT58-IVARP  PICTURE  X.                       CI0115
            11            QT58-DVALU  PICTURE  9(8).                    CI0115
            11            QT58-AACTV  PICTURE  S9(11)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ACCTVC PICTURE  X(20).                   CI0115
            11            QT58-ITXTI  PICTURE  X.                       CI0115
            11            QT58-ASANP  PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ACINV  PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-CELBL  PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-NMESS2 PICTURE  S9(6)                    CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-FILLER PICTURE  X(1).                    CI0115
            11            QT58-PRCLN  PICTURE  X(60).                   CI0115
            11            QT58-GECKD  PICTURE  9.                       CI0115
            11            QT58-MPLNA  PICTURE  X(19).                   CI0115
            11            QT58-CQACTL PICTURE  X(45).                   CI0115
            11            QT58-CRQPA  PICTURE  9(3).                    CI0115
            11            QT58-IVANT  PICTURE  X(1).                    CI0115
            11            QT58-IDBRP  PICTURE  X(1).                    CI0115
            11            QT58-IANPY  PICTURE  X.                       CI0115
            11            QT58-IVARP1 PICTURE  X.                       CI0115
            11            QT58-FILLER PICTURE  X(27).                   CI0115
            11            QT58-NSEQ2A PICTURE  S9(3)                    CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-NSEQ2P PICTURE  S9(3)                    CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-MRPSN  PICTURE  X(12).                   CI0115
            11            QT58-GEHCD  PICTURE  9(3)                     CI0115
                          OCCURS       002     TIMES.                   CI0115
            11            QT58-GEHCSU PICTURE  9(5)                     CI0115
                          OCCURS       002     TIMES.                   CI0115
            11            QT58-PRCSN  PICTURE  X(9).                    CI0115
            11            QT58-CGRMF  PICTURE  X.                       CI0115
            11            QT58-IGFEX  PICTURE  X.                       CI0115
            11            QT58-CLIDP  PICTURE  X(23).                   CI0115
            11            QT58-CLCTRC PICTURE  9(3).                    CI0115
            11            QT58-ADINP  PICTURE  X(20).                   CI0115
            11            QT58-CLCTRA PICTURE  9(3).                    CI0115
            11            QT58-GRPLC  PICTURE  99.                      CI0115
            11            QT58-CIDRP  PICTURE  99.                      CI0115
            11            QT58-FILLER PICTURE  X(01).                   CI0115
            11            QT58-AVMTOT PICTURE  S9(11)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-AVCSH  PICTURE  S9(11)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-AMARC  PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-AVLMX  PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-AVLMN  PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-INDRS  PICTURE  X.                       CI0115
            11            QT58-MPRN4  PICTURE  X(35).                   CI0115
            11            QT58-FILLER PICTURE  X(1).                    CI0115
            11            QT58-ACVALM PICTURE  S9(11)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-INDRSA PICTURE  X(2).                    CI0115
            11            QT58-DXTMSA PICTURE  X(26).                   CI0115
            11            QT58-NMESS6 PICTURE  S9(6)                    CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-NMESS7 PICTURE  S9(6)                    CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-IBIDSA PICTURE  X.                       CI0115
            11            QT58-IBIDSB PICTURE  X.                       CI0115
            11            QT58-INSPOS PICTURE  X.                       CI0115
            11            QT58-INSPOD PICTURE  X.                       CI0115
            11            QT58-ACBALX PICTURE  X(20).                   CI0115
            11            QT58-AINVMX PICTURE  X(20).                   CI0115
            11            QT58-AMARCX PICTURE  X(20).                   CI0115
            11            QT58-AVMTOX PICTURE  X(20).                   CI0115
            11            QT58-IMNPR  PICTURE  X.                       CI0115
            11            QT58-ISSPL  PICTURE  X.                       CI0115
            11            QT58-AVMTOI PICTURE  S9(11)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-AVCSHI PICTURE  S9(11)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-APOSC  PICTURE  S9(11)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-AVLMXI PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-AVLMN1 PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-AVLMN2 PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-FILLER PICTURE  X(05).                   CI0115
            10            QT58-QT5A.                                    CI0115
            11            QT58-CLID   PICTURE  X(23).                   CI0115
            11            QT58-GECKD1 PICTURE  9.                       CI0115
            11            QT58-MCLNM  PICTURE  X(40).                   CI0115
            11            QT58-MCLNM2 PICTURE  X(40).                   CI0115
            11            QT58-CLTYP  PICTURE  X.                       CI0115
            11            QT58-CLDOB  PICTURE  9(8).                    CI0115
            11            QT58-CLDTH  PICTURE  X.                       CI0115
            11            QT58-CLTIN  PICTURE  9(12).                   CI0115
            11            QT58-CLTINC PICTURE  9.                       CI0115
            11            QT58-GESAD1 PICTURE  X(30).                   CI0115
            11            QT58-GESAD2 PICTURE  X(30).                   CI0115
            11            QT58-GESAD3 PICTURE  X(30).                   CI0115
            11            QT58-GECIT  PICTURE  X(25).                   CI0115
            11            QT58-GECTRY PICTURE  X(20).                   CI0115
            11            QT58-GEPCD  PICTURE  X(12).                   CI0115
            11            QT58-GEST   PICTURE  X(8).                    CI0115
            11            QT58-GEADS  PICTURE  9.                       CI0115
            11            QT58-GECSD  PICTURE  9(8).                    CI0115
            11            QT58-QCLAGE PICTURE  9(3)V9                   CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-FILLER PICTURE  X(06).                   CI0115
            10            QT58-QT5T.                                    CI0115
            11            QT58-ATFRA  PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-AGOFD  PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-APRMX  PICTURE  S9(11)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-APRMN  PICTURE  S9(11)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-IOWNC  PICTURE  X.                       CI0115
            11            QT58-COWNF  PICTURE  X(30).                   CI0115
            11            QT58-CTYPE  PICTURE  X.                       CI0115
            11            QT58-CIRAC  PICTURE  X(5).                    CI0115
            11            QT58-CTXMT  PICTURE  9(2).                    CI0115
            11            QT58-AMIND  PICTURE  S9(7)V99.                CI0115
            11            QT58-AMAXAR PICTURE  S9(7)V99.                CI0115
            11            QT58-QSHOWQ PICTURE  S9(9)V999                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-QSHOW0 PICTURE  S9(10)V999               CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-PPOT1  PICTURE  S9(3)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-PACT1  PICTURE  S999V999                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-IPRTA  PICTURE  X.                       CI0115
            11            QT58-FILLER PICTURE  X.                       CI0115
            11            QT58-CLCUS  PICTURE  99.                      CI0115
            11            QT58-CCDSCW PICTURE  9(2).                    CI0115
            11            QT58-CCACT  PICTURE  99.                      CI0115
            11            QT58-CIRAG.                                   CI0115
            12            QT58-CIRAP  PICTURE  XX                       CI0115
                          OCCURS       010     TIMES.                   CI0115
            11            QT58-ITERF  PICTURE  X.                       CI0115
            11            QT58-IACFPD PICTURE  X(1).                    CI0115
            11            QT58-AFEET  PICTURE  S9(5)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ATERF  PICTURE  S9(5)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-CLIDNB PICTURE  9(8).                    CI0115
            11            QT58-ALOAD  PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ASURR  PICTURE  S9(07)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ASHIS  PICTURE  S9(11)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-AMNBL  PICTURE  S9(11)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-APNAC  PICTURE  S9(11)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ANGOF  PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-CPLTYP PICTURE  X(14).                   CI0115
            10            QT58-QT5N.                                    CI0115
            11            QT58-IARRAN PICTURE  X.                       CI0115
            11            QT58-GESTD1 PICTURE  9(8).                    CI0115
            11            QT58-GEEND1 PICTURE  S9(8)                    CI0115
                          BINARY.                                       CI0115
            11            QT58-GESTD  PICTURE  9(8).                    CI0115
            11            QT58-GEEND  PICTURE  9(8).                    CI0115
            11            QT58-NSQ4B2 PICTURE  9(8)                     CI0115
                          BINARY.                                       CI0115
            11            QT58-CDEST  PICTURE  99.                      CI0115
            11            QT58-DEFFT  PICTURE  9(8).                    CI0115
            11            QT58-CPMTF  PICTURE  99.                      CI0115
            11            QT58-CPMTG  PICTURE  99.                      CI0115
            11            QT58-MPMTFL PICTURE  X(24).                   CI0115
            11            QT58-MPMTFE PICTURE  X(24).                   CI0115
            11            QT58-DLAUP  PICTURE  9(8).                    CI0115
            11            QT58-NSEQ4B PICTURE  9(8)                     CI0115
                          BINARY.                                       CI0115
            11            QT58-QSACTF PICTURE  9(3).                    CI0115
            11            QT58-QSACTT PICTURE  9(3).                    CI0115
            11            QT58-CCONF  PICTURE  X(25).                   CI0115
            11            QT58-DCONF  PICTURE  9(8).                    CI0115
            11            QT58-DTIMT  PICTURE  X(8).                    CI0115
            11            QT58-CACTS  PICTURE  X.                       CI0115
            11            QT58-ADBRQ  PICTURE  S9(11)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-DNPMT  PICTURE  9(8).                    CI0115
            11            QT58-NAPDS  PICTURE  S9(3)                    CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-CDEST1 PICTURE  99.                      CI0115
            11            QT58-CLANR1 PICTURE  X(23).                   CI0115
            11            QT58-FILLER PICTURE  X(01).                   CI0115
            10            QT58-FILLER PICTURE  X(600).                  CI0115
            10            QT58-QT5C                                     CI0115
                          REDEFINES            QT58-FILLER.             CI0115
            11            QT58-CESLD  PICTURE  9(8).                    CI0115
            11            QT58-PCIRB5 PICTURE  S9(3)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-PANYDD PICTURE  S9(3)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-CEIT   PICTURE  9(3).                    CI0115
            11            QT58-PPART  PICTURE  9(3)V99                  CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-DTRME  PICTURE  9(8).                    CI0115
            11            QT58-CEIRND PICTURE  9(8).                    CI0115
            11            QT58-DANNIA PICTURE  9(8).                    CI0115
            11            QT58-AAPAA  PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-CELBDT PICTURE  9(8).                    CI0115
            11            QT58-CEIIS  PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-DTRME1 PICTURE  9(8).                    CI0115
            11            QT58-GMKTS.                                   CI0115
            12            QT58-DTRME2 PICTURE  9(8)                     CI0115
                          OCCURS       005     TIMES.                   CI0115
            12            QT58-DTRME3 PICTURE  9(8)                     CI0115
                          OCCURS       005     TIMES.                   CI0115
            11            QT58-ALINT  PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-CEHCD  PICTURE  9(3)                     CI0115
                          OCCURS       006     TIMES.                   CI0115
            11            QT58-CEFOTR PICTURE  S9(3)                    CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-DGPED  PICTURE  9(8).                    CI0115
            11            QT58-DIPED  PICTURE  9(8).                    CI0115
            11            QT58-FILLER PICTURE  X(409).                  CI0115
            10            QT58-QT5F                                     CI0115
                          REDEFINES            QT58-FILLER.             CI0115
            11            QT58-DLAUP2 PICTURE  9(8).                    CI0115
            11            QT58-QSHOW  PICTURE  S9(10)V999               CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-AFAVP  PICTURE  S9(4)V9(3)               CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-QSHIS  PICTURE  S9(10)V999               CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-QSHNM  PICTURE  S9(10)V999.              CI0115
            11            QT58-QSHOM  PICTURE  S9(10)V999               CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ADDAC  PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-QSHES  PICTURE  S9(10)V999               CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-NDCUS  PICTURE  X(9).                    CI0115
            11            QT58-CSTKR5 PICTURE  X(5).                    CI0115
            11            QT58-NACID  PICTURE  S9(11)                   CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-AGOFD2 PICTURE  S9(9)V99.                CI0115
            11            QT58-TCBAT  PICTURE  X(21).                   CI0115
            11            QT58-FILLER PICTURE  X(490).                  CI0115
            10            QT58-QT5L                                     CI0115
                          REDEFINES            QT58-FILLER.             CI0115
            11            QT58-ALDBEN PICTURE  S9(09)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-APREL  PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ALMODE PICTURE  99.                      CI0115
            11            QT58-ITMEC  PICTURE  X(1).                    CI0115
            11            QT58-ITAMR  PICTURE  X(1).                    CI0115
            11            QT58-MPMTF  PICTURE  X(14).                   CI0115
            11            QT58-TPLNL  PICTURE  X(30).                   CI0115
            11            QT58-ASBENA PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ASBENB PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ASBENC PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ASBENE PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ASBENF PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-GESTNS PICTURE  X(2).                    CI0115
            11            QT58-CTWHPB PICTURE  9(3)V999.                CI0115
            11            QT58-CTWHCB PICTURE  X.                       CI0115
            11            QT58-AMVA1  PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ASPAM  PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ACTCH  PICTURE  S9(07)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-AMXLN  PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ALFGH  PICTURE  999.                     CI0115
            11            QT58-ALPLNI PICTURE  9.                       CI0115
            11            QT58-ATSA8  PICTURE  S9(07)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-CVALB  PICTURE  X(3).                    CI0115
            11            QT58-ASURRN PICTURE  S9(07)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ASURRW PICTURE  S9(07)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ATLTB  PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-AEARN0 PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ATFPI  PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-AEARN1 PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ISELO  PICTURE  X.                       CI0115
            11            QT58-CCLAC  PICTURE  X.                       CI0115
            11            QT58-ALINNO PICTURE  99.                      CI0115
            11            QT58-ALPLNJ PICTURE  9.                       CI0115
            11            QT58-COLPL  PICTURE  9(05).                   CI0115
            11            QT58-ALPLDT PICTURE  9(8).                    CI0115
            11            QT58-ANFMC  PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-CPNOP  PICTURE  X(2).                    CI0115
            11            QT58-CVSTC  PICTURE  X(4).                    CI0115
            11            QT58-CGMBR  PICTURE  X.                       CI0115
            11            QT58-DWSDT  PICTURE  9(8).                    CI0115
            11            QT58-IRDPH  PICTURE  X.                       CI0115
            11            QT58-DWAIT  PICTURE  9(8).                    CI0115
            11            QT58-IAPGP  PICTURE  X.                       CI0115
            11            QT58-CASTA  PICTURE  X.                       CI0115
            11            QT58-CSSUP2 PICTURE  X.                       CI0115
            11            QT58-CVOMC1 PICTURE  X(1).                    CI0115
            11            QT58-APGBP  PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ALDDUE PICTURE  9(08).                   CI0115
            11            QT58-APYMT  PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ALSURR PICTURE  S9(09)V99                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-CESTP  PICTURE  X(03).                   CI0115
            11            QT58-FILLER PICTURE  X(356).                  CI0115
            10            QT58-QT5O                                     CI0115
                          REDEFINES            QT58-FILLER.             CI0115
            11            QT58-NBACT  PICTURE  S9(11)                   CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-CTIAC  PICTURE  S9(3)                    CI0115
                          BINARY.                                       CI0115
            11            QT58-CASTT  PICTURE  S99                      CI0115
                          BINARY.                                       CI0115
            11            QT58-CATMI  PICTURE  S9                       CI0115
                          BINARY.                                       CI0115
            11            QT58-IATMR  PICTURE  X(3).                    CI0115
            11            QT58-IBIPI  PICTURE  X.                       CI0115
            11            QT58-CBPST  PICTURE  S99                      CI0115
                          BINARY.                                       CI0115
            11            QT58-TBPST  PICTURE  X(16).                   CI0115
            11            QT58-CODPI  PICTURE  X.                       CI0115
            11            QT58-TODPS  PICTURE  X(9).                    CI0115
            11            QT58-FILLER PICTURE  X(448).                  CI0115
            11            QT58-IBPSD  PICTURE  X.                       CI0115
            11            QT58-FILLER PICTURE  X(107).                  CI0115
            11            QT58-QT5E                                     CI0115
                          REDEFINES            QT58-FILLER.             CI0115
            12            QT58-MPRN4X PICTURE  X(100).                  CI0115
            12            QT58-CCMSH  PICTURE  X(2).                    CI0115
            12            QT58-CPRCS  PICTURE  X(04).                   CI0115
            12            QT58-CURST  PICTURE  X.                       CI0115
            10            QT58-QT5M                                     CI0115
                          REDEFINES            QT58-FILLER.             CI0115
            11            QT58-NAPCN1 PICTURE  X(24).                   CI0115
            11            QT58-FILLER PICTURE  X(576).                  CI0115
            10            QT58-QT5B                                     CI0115
                          REDEFINES            QT58-FILLER.             CI0115
            11            QT58-NAPCN2 PICTURE  X(24).                   CI0115
            11            QT58-CTIDAL PICTURE  X(40).                   CI0115
            11            QT58-NPHNS  PICTURE  X(14).                   CI0115
            11            QT58-FILLER PICTURE  X(522).                  CI0115
            10            QT58-QT5P                                     CI0115
                          REDEFINES            QT58-FILLER.             CI0115
            11            QT58-CFPPT  PICTURE  9(3).                    CI0115
            11            QT58-TTYPP  PICTURE  X(40).                   CI0115
            11            QT58-CPPST  PICTURE  9(3).                    CI0115
            11            QT58-TPPST  PICTURE  X(15).                   CI0115
            11            QT58-APFEEQ PICTURE  S9(7)V99.                CI0115
            11            QT58-APFEEC PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-APFEEP PICTURE  S9(7)V99.                CI0115
            11            QT58-ISVCA  PICTURE  X.                       CI0115
            11            QT58-NSBVS  PICTURE  X(5).                    CI0115
            11            QT58-ICKRV  PICTURE  X.                       CI0115
            11            QT58-PDAMT  PICTURE  S9(03).                  CI0115
            11            QT58-PSTAX  PICTURE  S9(03)V999.              CI0115
            11            QT58-DPCAL  PICTURE  9(8).                    CI0115
            11            QT58-NADVF  PICTURE  X(08).                   CI0115
            11            QT58-DAGUP  PICTURE  9(8).                    CI0115
            11            QT58-AANFEA PICTURE  9(5)V99                  CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-CLIDN7 PICTURE  9(8)                     CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-ARANV  PICTURE  S9(7)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT58-DRANV  PICTURE  9(8).                    CI0115
            11            QT58-FILLER PICTURE  X(454).                  CI0115
            10            QT58-QT50                                     CI0115
                          REDEFINES            QT58-FILLER.             CI0115
            11            QT58-NANCA  PICTURE  X(30).                   CI0115
            11            QT58-MANCN  PICTURE  X(100).                  CI0115
            11            QT58-AINPTX PICTURE  X(20).                   CI0115
            11            QT58-CTID01 PICTURE  X(27).                   CI0115
            11            QT58-NANCA1 PICTURE  X(04).                   CI0115
            11            QT58-IIVAR  PICTURE  X(1).                    CI0115
            11            QT58-FILLER PICTURE  X(418).                  CI0115
            10            QT58-QT5R                                     CI0115
                          REDEFINES            QT58-FILLER.             CI0115
            11            QT58-NACTJ  PICTURE  X(04).                   CI0115
            11            QT58-NACNO6 PICTURE  X(11).                   CI0115
            11            QT58-FILLER PICTURE  X(585).                  CI0115
            10            QT58-AMAXA  PICTURE  S9(7)V99.                CI0115
            10            QT58-ISAOR  PICTURE  X.                       CI0115
            10            QT58-ISACH  PICTURE  X.                       CI0115
            10            QT58-CERRBA PICTURE  X(02).                   CI0115
            10            QT58-CERRBH PICTURE  X(02).                   CI0115
            10            QT58-IWITHH PICTURE  X.                       CI0115
            10            QT58-CTID20 PICTURE  X(27).                   CI0115
            10            QT58-GECKD3 PICTURE  9.                       CI0115
            10            QT58-DANFC  PICTURE  X(10).                   CI0115
            10            QT58-DAFCN  PICTURE  X(10).                   CI0115
            10            QT58-ISMTA  PICTURE  X.                       CI0115
            10            QT58-CERRBT PICTURE  X(02).                   CI0115
            10            QT58-NPLNI  PICTURE  X(10).                   CI0115
            10            QT58-FILLER PICTURE  X(023).                  CI0115
       01                 QT82.                                         CI0115
            10            QT82-QT8L.                                    CI0115
            11            QT82-CARTZA PICTURE  XX.                      CI0115
            11            QT82-TARTY  PICTURE  X(25).                   CI0115
            11            QT82-DBKEYS.                                  CI0115
            12            QT82-CLID   PICTURE  X(23).                   CI0115
            12            QT82-GEMDA  PICTURE  9(8).                    CI0115
            12            QT82-NSEQ4B PICTURE  9(8)                     CI0115
                          BINARY.                                       CI0115
            12            QT82-CARTYA PICTURE  XX.                      CI0115
            12            QT82-NARRS  PICTURE  S9(3)                    CI0115
                          COMPUTATIONAL-3.                              CI0115
            12            QT82-CTID01 PICTURE  X(27).                   CI0115
            12            QT82-CPMTC  PICTURE  99.                      CI0115
            12            QT82-MPMTT  PICTURE  X(20).                   CI0115
            12            QT82-NAPDS  PICTURE  S9(3)                    CI0115
                          COMPUTATIONAL-3.                              CI0115
            12            QT82-GESTD  PICTURE  9(8).                    CI0115
            12            QT82-CARTZ  PICTURE  99.                      CI0115
            12            QT82-CARST  PICTURE  99.                      CI0115
            12            QT82-NPISQ  PICTURE  S9(3)                    CI0115
                          COMPUTATIONAL-3.                              CI0115
            12            QT82-NPAIS  PICTURE  S9(3)                    CI0115
                          COMPUTATIONAL-3.                              CI0115
            12            QT82-GESQ2  PICTURE  99.                      CI0115
            11            QT82-FRFLDS.                                  CI0115
            12            QT82-CTID02 PICTURE  X(27).                   CI0115
            12            QT82-GECKD  PICTURE  9.                       CI0115
            12            QT82-CTIDA  PICTURE  9(3).                    CI0115
            12            QT82-MRPSN  PICTURE  X(12).                   CI0115
            12            QT82-CLTYP  PICTURE  X.                       CI0115
            12            QT82-MFDNM4 PICTURE  X(40).                   CI0115
            12            QT82-CTTLN1 PICTURE  X(30).                   CI0115
            12            QT82-CTTLN2 PICTURE  X(30).                   CI0115
            12            QT82-CTTLN3 PICTURE  X(30).                   CI0115
            12            QT82-CTTBO1 PICTURE  X(45).                   CI0115
            12            QT82-CTTBO2 PICTURE  X(45).                   CI0115
            12            QT82-CLORN  PICTURE  X(45).                   CI0115
            12            QT82-CLNAMF PICTURE  X(20).                   CI0115
            12            QT82-CLNAMI PICTURE  X.                       CI0115
            12            QT82-CLNAML PICTURE  X(25).                   CI0115
            12            QT82-CLNAMS PICTURE  X(4).                    CI0115
            11            QT82-CDETY  PICTURE  XX.                      CI0115
            11            QT82-TOFLDS.                                  CI0115
            12            QT82-CTID03 PICTURE  X(27).                   CI0115
            12            QT82-CACKD  PICTURE  9.                       CI0115
            12            QT82-CACTID PICTURE  9(3).                    CI0115
            12            QT82-MRPSN3 PICTURE  X(12).                   CI0115
            12            QT82-CLTYP1 PICTURE  X.                       CI0115
            12            QT82-MFDNM5 PICTURE  X(40).                   CI0115
            12            QT82-CATLN1 PICTURE  X(30).                   CI0115
            12            QT82-CATLN2 PICTURE  X(30).                   CI0115
            12            QT82-CATLN3 PICTURE  X(30).                   CI0115
            12            QT82-CATBO1 PICTURE  X(45).                   CI0115
            12            QT82-CATBO2 PICTURE  X(45).                   CI0115
            12            QT82-CLORN1 PICTURE  X(45).                   CI0115
            12            QT82-CLNMF  PICTURE  X(20).                   CI0115
            12            QT82-CLNAM5 PICTURE  X.                       CI0115
            12            QT82-CLNML  PICTURE  X(25).                   CI0115
            12            QT82-CLNMS  PICTURE  X(4).                    CI0115
            11            QT82-CPMTF  PICTURE  99.                      CI0115
            11            QT82-MPMTFL PICTURE  X(24).                   CI0115
            11            QT82-ACOTD  PICTURE  S9(9)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT82-PPOTD  PICTURE  S9(3)V99                 CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT82-QPSTD  PICTURE  S9(7)V999                CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT82-QMTH1  PICTURE  9(3).                    CI0115
            11            QT82-MFDNMS PICTURE  X(30).                   CI0115
            11            QT82-CFIDC  PICTURE  X(5).                    CI0115
            11            QT82-CACCT  PICTURE  X.                       CI0115
            11            QT82-GESAD1 PICTURE  X(30).                   CI0115
            11            QT82-GESAD2 PICTURE  X(30).                   CI0115
            11            QT82-GESAD3 PICTURE  X(30).                   CI0115
            11            QT82-GECIT  PICTURE  X(25).                   CI0115
            11            QT82-GEST   PICTURE  X(8).                    CI0115
            11            QT82-GEPCD  PICTURE  X(12).                   CI0115
            11            QT82-NPBN   PICTURE  X(20).                   CI0115
            11            QT82-MCSIG  PICTURE  X(30).                   CI0115
            11            QT82-NTR    PICTURE  9(8).                    CI0115
            11            QT82-CCBAT  PICTURE  99.                      CI0115
            11            QT82-TTBAL  PICTURE  X(15).                   CI0115
            11            QT82-CPCCDE PICTURE  99.                      CI0115
            11            QT82-DNPMT  PICTURE  9(8).                    CI0115
            11            QT82-GEEND  PICTURE  9(8).                    CI0115
            11            QT82-CIRMO  PICTURE  X(12).                   CI0115
            11            QT82-CIRM1  PICTURE  X(12).                   CI0115
            11            QT82-NIRACM PICTURE  9(2).                    CI0115
            11            QT82-CDEST  PICTURE  99.                      CI0115
            11            QT82-TARST  PICTURE  X(10).                   CI0115
            11            QT82-TDESA  PICTURE  X(10).                   CI0115
            11            QT82-NAIDC  PICTURE  9(12).                   CI0115
            11            QT82-DNBPD  PICTURE  9(8).                    CI0115
            11            QT82-DLBPD  PICTURE  9(8).                    CI0115
            11            QT82-TWITH  PICTURE  X(12).                   CI0115
            11            QT82-IACSD1 PICTURE  XXX.                     CI0115
            11            QT82-TDSTR  PICTURE  X(8).                    CI0115
            11            QT82-IINDI1 PICTURE  X(1).                    CI0115
            11            QT82-IINDI2 PICTURE  X(1).                    CI0115
            11            QT82-IINDI3 PICTURE  X(1).                    CI0115
            11            QT82-TDELI  PICTURE  X(30).                   CI0115
            11            QT82-DDSHPA PICTURE  9(8).                    CI0115
            11            QT82-NDRFTF PICTURE  9(5).                    CI0115
            11            QT82-QDIPBK PICTURE  9(3).                    CI0115
            11            QT82-NDRFTL PICTURE  9(5).                    CI0115
            11            QT82-CLID4  PICTURE  X(23).                   CI0115
            11            QT82-TINDI1 PICTURE  X(09).                   CI0115
            11            QT82-TINDI2 PICTURE  X(09).                   CI0115
            11            QT82-TINDI3 PICTURE  X(09).                   CI0115
            11            QT82-TSECD  PICTURE  X(30).                   CI0115
            11            QT82-CTKRAA PICTURE  X(12).                   CI0115
            11            QT82-GCUSPZ PICTURE  X(12).                   CI0115
            11            QT82-CORTY  PICTURE  X.                       CI0115
            11            QT82-ALOIDD PICTURE  9(9)V99                  CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT82-DELOI3 PICTURE  9(6).                    CI0115
            11            QT82-INROA  PICTURE  X(1).                    CI0115
            11            QT82-IDRMD  PICTURE  X.                       CI0115
            10            QT82-RPCFLD.                                  CI0115
            11            QT82-NSEQ5  PICTURE  9(5).                    CI0115
            11            QT82-NSEQ5A PICTURE  9(5).                    CI0115
            11            QT82-IAIND1 PICTURE  X.                       CI0115
            11            QT82-IAIND2 PICTURE  X.                       CI0115
            11            QT82-IAIND9 PICTURE  X.                       CI0115
            11            QT82-NMESS2 PICTURE  S9(6)                    CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT82-NMESS5 PICTURE  S9(6)                    CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            QT82-CDETY1 PICTURE  XX.                      CI0115
            11            QT82-FILLER PICTURE  X(95).                   CI0115

      *!WF DSP=QT DSL=QT SEL=82 FOR=I DES=1 LEV=1 PLT=QT
      *MISCELLANEOUS WORKING STORAGE AREA ***
       01  WS00.
           05  WS-TSQ-TYPE-FIELDS.
               10  WS00-ACCTLIST       PIC X(10)   VALUE 'ACCTLIST  '.
               10  WS00-ARRANGE        PIC X(10)   VALUE 'ARRANGE   '.
               10  WS00-CTIDLIST       PIC X(10)   VALUE 'CTIDLIST  '.
               10  WS00-SUBACCT        PIC X(10)   VALUE 'SUBACCT   '.
               10  WS00-ACCTKEY        PIC X(10)   VALUE 'ACCTKEY   '.
           05  WS-TSQ-CALL-TYPES.
               10  WS00-STRT           PIC X(4)    VALUE 'STRT'.
               10  WS00-INIT           PIC X(4)    VALUE 'INIT'.
               10  WS00-READ           PIC X(4)    VALUE 'READ'.
               10  WS00-WRIT           PIC X(4)    VALUE 'WRIT'.
               10  WS00-REWR           PIC X(4)    VALUE 'REWR'.
               10  WS00-DONE           PIC X(4)    VALUE 'DONE'.
               10  WS00-PURG           PIC X(4)    VALUE 'PURG'.
               10  WS00-END            PIC X(4)    VALUE 'END '.
      *!WI
           05  ARRA-XIMAX
                        PICTURE S9(4)                                   CI0115
                          BINARY.                                       CI0115
      **** TEMP VARIABLES FOR BANK ACCOUNT *****************************
      *!WI
           05  WS00-CLID
                        PICTURE X(23).                                  CI0115
      *!WI
           05  WS00-NPBN
                        PICTURE X(20).                                  CI0115
      *!WI
           05  WS00-NTR
                        PICTURE 9(8).                                   CI0115
      *!WI
           05  WS00-GECKD
                        PICTURE 9.                                      CI0115
      *!WI
           05  WS00-TTBAL
                        PICTURE X(15).                                  CI0115
      *!WI
           05  WS00-MCSIG
                        PICTURE X(30).                                  CI0115
      **** FIRST TIME IN INDICATOR *************************************
           05  WS00-FIRST-TIME     PIC X(01).
               88  FIRST-TIME-IN       VALUE 'Y'.
      **** KEY FOR READING ARRANGEMENT TSQ *****************************
           05  WS00-CANUMB.
               10  FILLER          PIC X(22) VALUE ZEROES.
               10  WS00-ITEM       PIC 9(5)  VALUE ZEROES.
      **** SEARCH KEY FOR WTBA-TABLE ***********************************
           05  WS00-SRCH-KEY.
      *!WI
               10  WS00-CTTLN1
                        PICTURE X(30).                                  CI0115
      *!WI
               10  WS00-CTTLN2
                        PICTURE X(30).                                  CI0115
      *!WI
               10  WS00-CTTLN3
                        PICTURE X(30).                                  CI0115
      *!WI
               10  WS00-CTTBO1
                        PICTURE X(45).                                  CI0115
      *!WI
               10  WS00-CTTBO2
                        PICTURE X(45).                                  CI0115
       01   DEBUT-WSS.                                                  CI0115
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0115
            05   IK     PICTURE X.                                      CI0115
       01  CONSTANTES-PAC.                                              CI0115
           05  FILLER  PICTURE X(87)   VALUE                            CI0115
                     '6015 CAT09/08/14CI0115ADMIN   14:34:55CI0115P AMERCI0115
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0115
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0115
           05  NUGNA   PICTURE X(5).                                    CI0115
           05  APPLI   PICTURE X(3).                                    CI0115
           05  DATGN   PICTURE X(8).                                    CI0115
           05  PROGR   PICTURE X(6).                                    CI0115
           05  CODUTI  PICTURE X(8).                                    CI0115
           05  TIMGN   PICTURE X(8).                                    CI0115
           05  PROGE   PICTURE X(8).                                    CI0115
           05  COBASE  PICTURE X(4).                                    CI0115
           05  DATGNC  PICTURE X(10).                                   CI0115
           05  RELEAS  PICTURE X(7).                                    CI0115
           05  DATGE   PICTURE X(10).                                   CI0115
           05  DATSQ   PICTURE X(10).                                   CI0115
       01  DATCE.                                                       CI0115
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0115
         05  DATOR.                                                     CI0115
           10  DATOA  PICTURE XX.                                       CI0115
           10  DATOM  PICTURE XX.                                       CI0115
           10  DATOJ  PICTURE XX.                                       CI0115
       01   VARIABLES-CONDITIONNELLES.                                  CI0115
            05                  FT      PICTURE X VALUE '0'.            CI0115
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0115
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0115
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           IWTBAL PICTURE S9(4) VALUE  ZERO.
            05           IWTBAR PICTURE S9(4) VALUE  ZERO.
            05           IWTBAM PICTURE S9(4) VALUE +0300.
            05           J50BAR PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0115
      ******************************************************************AM0218
      ** WORKING STORAGE SEGMENT FOR CI0218                            *AM0218
      ******************************************************************AM0218
      *                                                                 AM0218
       01   7-X200-STAGING.                                             AM0218
      *!WI pl=X2210                                                     AM0218
            05  7-X200-XFUNC                                            AM0218
                        PICTURE X(04).                                  CI0115
      *!WI pl=X2220                                                     AM0218
            05  7-X200-NPNTRA                                           AM0218
                          POINTER.                                      CI0115
            05  7-X200-AREATYPE PIC X(10).

      **** AREAS USED TO COMMUNICATE WITH TSQ ***
      *!WF DSP=X2 DSL=VW SEL=01 FOR=I DES=1 LEV=1 PLT=X2
       01                 X201.                                         CI0115
            10            X201-FILLER.                                  CI0115
            11            X201-XFUNC  PICTURE  X(04).                   CI0115
            11            X201-CANUMB PICTURE  X(27).                   CI0115
            10            X201-CENTT  PICTURE  X.                       CI0115
            10            X201-FILLER PICTURE  X(68).                   CI0115
            10            X201-MTQUE  PICTURE  X(08).                   CI0115
            10            X201-FILLER PICTURE  X(04).                   CI0115
            10            X201-FILLER.                                  CI0115
            11            X201-NPNTRB                                   CI0115
                          POINTER.                                      CI0115
            11            X201-NPNTRC                                   CI0115
                          POINTER.                                      CI0115
            11            X201-NPNTRD                                   CI0115
                          POINTER.                                      CI0115
            11            X201-NPNTRE                                   CI0115
                          POINTER.                                      CI0115
            11            X201-NPNTRF                                   CI0115
                          POINTER.                                      CI0115
            11            X201-NPNTRG                                   CI0115
                          POINTER.                                      CI0115
            11            X201-NPNTRH                                   CI0115
                          POINTER.                                      CI0115
            10            X201-DCACG  PICTURE  9(8).                    CI0115
            10            X201-FILLER PICTURE  X(492).                  CI0115
      ******************************************************************AM0218
      ** WORKING STORAGE SEGMENT FOR CI0218                            *AM0218
      ******************************************************************AM0218
      *                                                                 AM0218
       01   7-X300-STAGING.                                             AM0218
      *!WI pl=X3210                                                     AM0218
            05  7-X300-XFUNC                                            AM0218
                        PICTURE X(04).                                  CI0115
      *!WI pl=X3220                                                     AM0218
            05  7-X300-NPNTRA                                           AM0218
                          POINTER.                                      CI0115
            05  7-X300-AREATYPE PIC X(10).
       LINKAGE SECTION.                                                 ADU102
      *>>>>>>LINKAGE SEGMENT V258
      *-----------------------------------------------------------------
      *!WF DSP=V1 DSL=QT SEL=15 FOR=I LEV=1 PLT=05
       01                 V100.                                         CI0115
          05              V100-SUITE.                                   CI0115
            15       FILLER         PICTURE  X(00429).                  CI0115
       01                 V115  REDEFINES      V100.                    CI0115
            10            V115-NSSSI  PICTURE  X(24).                   CI0115
            10            V115-C299.                                    CI0115
            11            V115-CTID.                                    CI0115
            12            V115-CTIDA  PICTURE  9(3).                    CI0115
            12            V115-CTIDN.                                   CI0115
            13            V115-CTIDNP PICTURE  X(13).                   CI0115
            13            V115-CTIDND PICTURE  9(11).                   CI0115
            10            V115-CTTLN1 PICTURE  X(30).                   CI0115
            10            V115-CTTLN2 PICTURE  X(30).                   CI0115
            10            V115-CTTLN3 PICTURE  X(30).                   CI0115
            10            V115-CTTBO1 PICTURE  X(45).                   CI0115
            10            V115-CTTBO2 PICTURE  X(45).                   CI0115
            10            V115-CLID   PICTURE  X(23).                   CI0115
            10            V115-NPBN   PICTURE  X(20).                   CI0115
            10            V115-NTR    PICTURE  9(8).                    CI0115
            10            V115-GECKD  PICTURE  9.                       CI0115
            10            V115-TTBAL  PICTURE  X(15).                   CI0115
            10            V115-MCSIG  PICTURE  X(30).                   CI0115
            10            V115-IOWNC  PICTURE  X.                       CI0115
            10            V115-FILLER PICTURE  X(100).                  CI0115
      *
      ******************************************************************
      ** LINKAGE AREA FOR USER-ID TSQ-RECORD                           *
      ******************************************************************
      *!WF DSP=L1 DSL=VW SEL=02 FOR=I DES=1 LEV=1 PLT=20
       01                 L102.                                         CI0115
            10            L102-MTQUE  PICTURE  X(08).                   CI0115
            10            L102-GEOPD2 PICTURE  X(8).                    CI0115
            10            L102-GEAUN  PICTURE  9(5).                    CI0115
            10            L102-XIMAX  PICTURE  S9(4)                    CI0115
                          OCCURS       004     TIMES                    CI0115
                          BINARY.                                       CI0115
            10            L102-MPLNR2 PICTURE  X(40).                   CI0115
            10            L102-ATROLL PICTURE  X(25).                   CI0115
            10            L102-GESTNS PICTURE  X(2).                    CI0115
            10            L102-NFLID  PICTURE  X(8).                    CI0115
            10            L102-CAOSTA PICTURE  X.                       CI0115
            10            L102-CAOSTB PICTURE  X.                       CI0115
            10            L102-CAOSTC PICTURE  X.                       CI0115
            10            L102-CAOSTD PICTURE  X.                       CI0115
            10            L102-CAOSTE PICTURE  X.                       CI0115
            10            L102-CAOSTF PICTURE  X.                       CI0115
            10            L102-CAOSTG PICTURE  X.                       CI0115
            10            L102-CAOSTH PICTURE  X.                       CI0115
            10            L102-CAOSTI PICTURE  X.                       CI0115
            10            L102-XDATE  PICTURE  X(8).                    CI0115
            10            L102-GTMST  PICTURE  X(6).                    CI0115
            10            L102-FILLER PICTURE  X(135).                  CI0115
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0115
          05              MS00-SUITE.                                   CI0115
            15       FILLER         PICTURE  X(00542).                  CI0115
       01                 MS03  REDEFINES      MS00.                    CI0115
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0115
                          COMPUTATIONAL-3.                              CI0115
            10            MS03-CMSSF  PICTURE  XX.                      CI0115
            10            MS03-DU09.                                    CI0115
            11            MS03-CMESA  PICTURE  S9(9)                    CI0115
                          BINARY.                                       CI0115
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0115
                          BINARY.                                       CI0115
            11            MS03-CMESB  PICTURE  S9(9)                    CI0115
                          BINARY.                                       CI0115
            11            MS03-CMSST  PICTURE  S9(9)                    CI0115
                          BINARY.                                       CI0115
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0115
                          BINARY.                                       CI0115
            11            MS03-QELLAA PICTURE  S9(9)                    CI0115
                          BINARY.                                       CI0115
            11            MS03-TMESS4 PICTURE  X(512).                  CI0115
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0115
            10            MX11-QMSGS  PICTURE  9(03).                   CI0115
            10            MX11-PJ09                                     CI0115
                          OCCURS       025     TIMES.                   CI0115
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0115
                          COMPUTATIONAL-3.                              CI0115
            11            MX11-CMESB  PICTURE  S9(9)                    CI0115
                          BINARY.                                       CI0115
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                V115
                                L102
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0115
      *               *                                   *             CI0115
      *               *INITIALISATIONS                    *             CI0115
      *               *                                   *             CI0115
      *               *************************************.            CI0115
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
      *N02BB.    NOTE *SAVE THE VALUE FOR SELECTED BANK   *.
       F02BB.    IF    V115-NPBN NOT = WS00-NPBN                        lv10
                 NEXT SENTENCE ELSE GO TO     F02BB-FN.
      *ACCOUNT ONLY IN FIRST TIME
           MOVE        V115-CLID TO WS00-CLID
           MOVE        V115-NPBN TO WS00-NPBN
           MOVE        V115-NTR TO WS00-NTR
           MOVE        V115-GECKD TO WS00-GECKD
           MOVE        V115-TTBAL TO WS00-TTBAL
           MOVE        V115-MCSIG TO WS00-MCSIG
           MOVE        'Y' TO WS00-FIRST-TIME.
       F02BB-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0115
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0115
      *               *                                   *             CI0115
      *               *FIN DE TRAITEMENT                  *             CI0115
      *               *                                   *             CI0115
      *               *************************************.            CI0115
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0115
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *INITIAL PROCESSING                 *
      *               *                                   *
      *               *************************************.
       F45.      IF    FIRST-TIME-IN                                    lv05
                 NEXT SENTENCE ELSE GO TO     F45-FN.
      *N45AB.    NOTE *INITIALIZE THE OWNERSHIP TEXT      *.
       F45AB.                                                           lv10
      *LINES TABLE
           INITIALIZE  WTBA-TABLE IWTBAL.
       F45AB-FN. EXIT.
      *N45BB.    NOTE *POPULATE THE ARRANGEMENTS TSQ      *.
       F45BB.                                                           lv10
      *COUNTER FROM LINKAGE L102
           MOVE        L102-XIMAX (4) TO ARRA-XIMAX.
       F45BB-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *MAIN PROCESS(ONLY FIRST TIME IN)   *
      *               *                                   *
      *               *************************************.
       F50.      IF    FIRST-TIME-IN                                    lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *N50BA.    NOTE *READ THE ARRANGEMENT TSQ           *.
       F50BA.                                                           lv10
           MOVE        1                        TO J50BAR
                                    GO TO     F50BA-B.
       F50BA-A.
           ADD         1                        TO J50BAR.
       F50BA-B.
           IF          J50BAR                   >  ARRA-XIMAX
                                    GO TO     F50BA-FN.
      *SEQUENTIALLY
           MOVE        WS00-READ TO 7-X200-XFUNC
           MOVE        'S' TO X201-CENTT
           MOVE        WS00-ARRANGE TO 7-X200-AREATYPE
           MOVE        J50BAR TO WS00-ITEM
           MOVE        WS00-CANUMB TO X201-CANUMB
           PERFORM     F91EC THRU F91EC-FN.
      *N50BD.    NOTE *READ NEXT ARRANGEMENT IF NOT       *.
       F50BD.    IF    QT82-CARTZA NOT = 'BA'                           lv15
                 NEXT SENTENCE ELSE GO TO     F50BD-FN.
      *BANK AUTHORIZATION
               GO TO     F50BA-900.
       F50BD-900. GO TO F50BG-FN.
       F50BD-FN. EXIT.
      *N50BG.    NOTE *PROCEED FOR BA ARRANGEMENTS ONLY   *.
       F50BG.         EXIT.                                             lv15
      *N50CB.    NOTE *THE BANK INFO READ FROM ARR TSQ    *.
       F50CB.    IF    QT82-CLID NOT = WS00-CLID                        lv20
                 OR    QT82-NPBN NOT = WS00-NPBN
                 OR    QT82-NTR NOT = WS00-NTR
                 OR    QT82-GECKD NOT = WS00-GECKD
                 OR    QT82-TTBAL NOT = WS00-TTBAL
                 OR    QT82-MCSIG NOT = WS00-MCSIG
                 NEXT SENTENCE ELSE GO TO     F50CB-FN.
      *SHOULD EXACTLY MATCH ALL THE
      *SAME FIELDS PASSED TO CI0115. IF
      *NOT MATCH, READ NEXT ARRANGEMENT
      *FROM TSQ.
               GO TO     F50BA-900.
       F50CB-FN. EXIT.
      *N50CG.    NOTE *ONLY PROCEED FOR THE ACTIVE AND    *.
       F50CG.    IF    QT82-CDEST NOT = 01                              lv20
                 AND   QT82-CDEST NOT = 03
                 AND   QT82-CDEST NOT = 00
                 NEXT SENTENCE ELSE GO TO     F50CG-FN.
      *INACTIVE BA
      *IF THE ARRANGEMENT STATUS
      *DOESN'T EXIST, THE ARR WILL BE
      *IMPLIED AS ACTIVE. THIS LOGIC
      *WILL MATCH THE XE82 CHANGES
      *FOR PTR10475
               GO TO     F50BA-900.
       F50CG-FN. EXIT.
      *N50DB.    NOTE *READ ACCOUNTS TSQ TO FIND THE BA   *.
       F50DB.                                                           lv20
      *ARRANGEMENT ACCOUNT (FROM
      *ARRANGEMENT TSQ).
           MOVE        WS00-READ TO 7-X300-XFUNC
           MOVE        'A' TO X201-CENTT
           MOVE        WS00-ACCTLIST TO 7-X300-AREATYPE
           MOVE        QT82-CTID01 TO X201-CANUMB
           PERFORM     F91FC THRU F91FC-FN.
       F50DB-FN. EXIT.
      *N50DG.    NOTE *BUILD THE OWNERSHIP LINES TABLE    *.
       F50DG.    IF    QT58-CTSTA = 02                                  lv20
                 NEXT SENTENCE ELSE GO TO     F50DG-FN.
      *ONLY FOR ACTIVE ACCOUNT.
      *POPULATE THE OWNERSHIP LINES                                     DOT
      *INTO SEARCH KEY WS00-SRCH-KEY
           MOVE        QT58-CTTLN1 TO WS00-CTTLN1
           MOVE        QT58-CTTLN2 TO WS00-CTTLN2
           MOVE        QT58-CTTLN3 TO WS00-CTTLN3
           MOVE        QT58-CTTBO1 TO WS00-CTTBO1
           MOVE        QT58-CTTBO2 TO WS00-CTTBO2.
           MOVE 1 TO     IWTBAR.                                        DOT
       F50DG-100. IF     IWTBAR NOT >    IWTBAL
           AND           WTBA-SRCH      (IWTBAR)
           NOT =           WS00-SRCH-KEY
           ADD 1 TO      IWTBAR    GO TO F50DG-100.
      *N50DI.    NOTE *SAVE IT INTO OWNERSHIP TABLE       *.
       F50DI.    IF    IWTBAR > IWTBAL                                  lv25
                 AND   WTBA-QITEM < 300
                 NEXT SENTENCE ELSE GO TO     F50DI-FN.
      *WTBA-TABLE IF NOT ALREADY THERE
           ADD         1 TO WTBA-QITEM
           IWTBAL
           MOVE        QT58-CTTLN1 TO WTBA-CTTLN1 (IWTBAL)
           MOVE        QT58-CTTLN2 TO WTBA-CTTLN2 (IWTBAL)
           MOVE        QT58-CTTLN3 TO WTBA-CTTLN3 (IWTBAL)
           MOVE        QT58-CTTBO1 TO WTBA-CTTBO1 (IWTBAL)
           MOVE        QT58-CTTBO2 TO WTBA-CTTBO2 (IWTBAL).
       F50DI-FN. EXIT.
       F50DG-FN. EXIT.
       F50BG-FN. EXIT.
       F50BA-900. GO TO F50BA-A.
       F50BA-FN. EXIT.
      *N50XB.    NOTE *RESET FIRST TIME IN FLAG           *.
       F50XB.                                                           lv10
           MOVE        'N' TO WS00-FIRST-TIME.
       F50XB-FN. EXIT.
       F50-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *PROCEED EVERY TIME                 *
      *               *                                   *
      *               *************************************.
       F60.           EXIT.                                             lv05
      *N60BA.    NOTE *SEARCH THE OWNERSHIP TABLE TO      *.
       F60BA.                                                           lv10
      *CHECK WHETHER THE OWNERSHIP TEXT
      *LINES OF THE REQUESTED ACCOUNT
      *ARE EXACTLY MATCHED THE ONE IN
      *OWNERSHIP TABLE
           MOVE        V115-CTTLN1 TO WS00-CTTLN1
           MOVE        V115-CTTLN2 TO WS00-CTTLN2
           MOVE        V115-CTTLN3 TO WS00-CTTLN3
           MOVE        V115-CTTBO1 TO WS00-CTTBO1
           MOVE        V115-CTTBO2 TO WS00-CTTBO2.
           MOVE 1 TO     IWTBAR.                                        DOT
       F60BA-200. IF     IWTBAR NOT >    IWTBAL
           AND           WTBA-SRCH      (IWTBAR)
           NOT =           WS00-SRCH-KEY
           ADD 1 TO      IWTBAR    GO TO F60BA-200.
      *N60BD.    NOTE *SET INDICATOR AS 'N' IF NO         *.
       F60BD.    IF    IWTBAR > IWTBAL                                  lv15
                 NEXT SENTENCE ELSE GO TO     F60BD-FN.
      *MATCHED OWNERSHIP TEXT LINES
      *WAS FOUND
           MOVE        'N' TO V115-IOWNC.
       F60BD-FN. EXIT.
       F60BA-FN. EXIT.
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
      *               *CALLED MODULES                     *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91EC.    NOTE *CALL CI0218 TEMP STORAGE UTIL      *.            AM0218
       F91EC.                                                           lv10
      *                                                                 AM0218
           MOVE        0 TO MS03-NMESS2
           MOVE        7-X200-XFUNC TO X201-XFUNC                       AM0218
           SET 7-X200-NPNTRA TO NULL                                    AM0218
           STRING      'FDCX'                                           AM0218
           V115-NSSSI                                                   AM0218
           DELIMITED BY SIZE                                            AM0218
           INTO X201-MTQUE                                              AM0218
           CALL        CI0218 USING                                     AM0218
           DFHEIBLK                                                     AM0218
           X201                                                         AM0218
           MS03                                                         AM0218
           MX11                                                         AM0218
      *AREAS TO PASS GO BEFORE LINE 940                                 AM0218
                  7-X200-AREATYPE
                  QT82
                  7-X200-NPNTRA                                         AM0218
      *7-X200-NPNTRA MUST BE THE LAST                                   AM0218
      *PARM PASSED TO CI0218                                            AM0218
                 IF    MS03-NMESS2 NOT = ZERO                           DOT
      *CHECK FOR ERROR IN CI0218                                        AM0218
           PERFORM     F98ET THRU F98ET-FN.                             AM0218
       F91EC-FN. EXIT.
      *N91FC.    NOTE *CALL CI0218 TEMP STORAGE UTIL      *.            AM0218
       F91FC.                                                           lv10
      *                                                                 AM0218
           MOVE        0 TO MS03-NMESS2
           MOVE        7-X300-XFUNC TO X201-XFUNC                       AM0218
           SET 7-X300-NPNTRA TO NULL                                    AM0218
           STRING      'FDCX'                                           AM0218
           V115-NSSSI                                                   AM0218
           DELIMITED BY SIZE                                            AM0218
           INTO X201-MTQUE                                              AM0218
           CALL        CI0218 USING                                     AM0218
           DFHEIBLK                                                     AM0218
           X201                                                         AM0218
           MS03                                                         AM0218
           MX11                                                         AM0218
      *AREAS TO PASS GO BEFORE LINE 940                                 AM0218
                  7-X300-AREATYPE
                  QT58
                  7-X300-NPNTRA                                         AM0218
      *7-X300-NPNTRA MUST BE THE LAST                                   AM0218
      *PARM PASSED TO CI0218                                            AM0218
                 IF    MS03-NMESS2 NOT = ZERO                           DOT
      *CHECK FOR ERROR IN CI0218                                        AM0218
           PERFORM     F98ET THRU F98ET-FN.                             AM0218
       F91FC-FN. EXIT.
       F91-FN.   EXIT.
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
