       IDENTIFICATION DIVISION.                                         CI0273
       PROGRAM-ID.  CI0273P.                                            CI0273
      *AUTHOR.         VALIDATE ONE-TIME TRANS AMT.                     CI0273
      *DATE-COMPILED.   09/08/14.                                       CI0273
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2013                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE FA     SYSTEM AND ALL INFORMATION RELATING THERETO,    *ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE FA     SYSTEM AND ALL            *ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE FA           *ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 2013                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0273
       CONFIGURATION SECTION.                                           CI0273
       SOURCE-COMPUTER. IBM-370.                                        CI0273
       OBJECT-COMPUTER. IBM-370.                                        CI0273
       DATA DIVISION.                                                   CI0273
       WORKING-STORAGE SECTION.                                         CI0273
      ******************************************************************
      *SEGMENT FOR DB2 TABLE TBCL2B
      ******************************************************************
      *!WF DSP=CL DSL=CL SEL=2B FOR=I DES=2 LEV=2 PLT=CB
       01                 CL00.                                         CI0273
            02            CL2B.                                         CI0273
            10            CL2B-CLIDO5 PICTURE  S9(3)                    CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CL2B-CLIDN  PICTURE  X(20)                    CI0273
                          VALUE                SPACE.                   CI0273
            10            CL2B-CPRCP  PICTURE  S9(3)                    CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CL2B-CTTNC  PICTURE  S9(3)                    CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CL2B-DLTND  PICTURE  X(10)                    CI0273
                          VALUE                SPACE.                   CI0273
            10            CL2B-DCTIN  PICTURE  X(10)                    CI0273
                          VALUE                SPACE.                   CI0273
            10            CL2B-MCRBY  PICTURE  X(16)                    CI0273
                          VALUE                SPACE.                   CI0273
            10            CL2B-GEOPDC PICTURE  X(8)                     CI0273
                          VALUE                SPACE.                   CI0273
            10            CL2B-DXTMST PICTURE  X(26)                    CI0273
                          VALUE                SPACE.                   CI0273
            10            CL2B-MUPBY  PICTURE  X(16)                    CI0273
                          VALUE                SPACE.                   CI0273
            10            CL2B-GEOPDB PICTURE  X(8)                     CI0273
                          VALUE                SPACE.                   CI0273
            10            CL2B-DXTMS2 PICTURE  X(26)                    CI0273
                          VALUE                SPACE.                   CI0273

      *PASS AREA TO/FROM CI0135 (CERT ACCT INFO)
      *!WF DSP=CE DSL=PJ SEL=02 FOR=I DES=2 LEV=1 PLT=CE
       01                 CE02.                                         CI0273
            10            CE02-CTID   PICTURE  X(27)                    CI0273
                          VALUE                SPACE.                   CI0273
            10            CE02-DCACG  PICTURE  9(8)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-ACCTV8 PICTURE  S9(9)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-AIDOL1 PICTURE  S9(9)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-AUINT1 PICTURE  S9(9)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-CXCSV  PICTURE  S9(7)V9(2)               CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-PCIRB5 PICTURE  S9(3)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-PANYDD PICTURE  S9(3)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-PCIRA5 PICTURE  S9(3)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-PANYDF PICTURE  9(3)V99                  CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-PCIRCB PICTURE  S9(3)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-PANYDG PICTURE  S9(3)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-PPART  PICTURE  9(3)V99                  CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-PMRTN  PICTURE  9(3)V99                  CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-PMRTEB PICTURE  S9(3)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-PBRITD PICTURE  S9(3)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-CEIAPI PICTURE  S9(7)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-CEIRND PICTURE  9(8)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-CEIT   PICTURE  9(3)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-DMATUR PICTURE  9(8)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-AMTUR  PICTURE  S9(7)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-CELBDT PICTURE  9(8)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-DTRME  PICTURE  9(8)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-NBSEI  PICTURE  999V99                   CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-NBSEIC PICTURE  S9(3)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-TRPTH  PICTURE  X(30)                    CI0273
                          VALUE                SPACE.                   CI0273
            10            CE02-CELBL  PICTURE  S9(7)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-ALINT  PICTURE  S9(7)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-PELIRB PICTURE  S9(3)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-ASANP  PICTURE  S9(7)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-AAPAA  PICTURE  S9(7)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-IQLIF  PICTURE  X                        CI0273
                          VALUE                SPACE.                   CI0273
            10            CE02-QMTHAA PICTURE  9(2)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-QMTHCC PICTURE  9(2)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-QYEARA PICTURE  9(2)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-DANNIA PICTURE  9(8)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-PBONS  PICTURE  9(2)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-AARQDA PICTURE  S9(5)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-AACFA  PICTURE  S9(7)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-AIEPAA PICTURE  S9(7)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-CVSUR  PICTURE  X(30)                    CI0273
                          VALUE                SPACE.                   CI0273
            10            CE02-CPRDA1 PICTURE  9(3)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-DFYR   PICTURE  9(4)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-DFYRB  PICTURE  9(4)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-DVALU  PICTURE  9(8)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-DNIPM  PICTURE  9(8)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-CIPFM  PICTURE  S9(3)                    CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-CESLD  PICTURE  9(8)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-CEHCD  PICTURE  9(3)                     CI0273
                          OCCURS       006     TIMES                    CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-CETYPC PICTURE  9(2)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-CEOTP  PICTURE  9(1)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-CEIIS  PICTURE  S9(7)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-DTRME1 PICTURE  9(8)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-CEFOIM PICTURE  S9(7)V99                 CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-CEIPDA PICTURE  S9(3)                    CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-GECTR  PICTURE  99                       CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-GMKTS.                                   CI0273
            11            CE02-DTRME2 PICTURE  9(8)                     CI0273
                          OCCURS       005     TIMES                    CI0273
                          VALUE                ZERO.                    CI0273
            11            CE02-DTRME3 PICTURE  9(8)                     CI0273
                          OCCURS       005     TIMES                    CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-PRCOD  PICTURE  9(5)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-CEFOTR PICTURE  S9(3)                    CI0273
                          VALUE                ZERO                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CE02-DGPED  PICTURE  9(8)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-DIPED  PICTURE  9(8)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            CE02-FILLER PICTURE  X(27)                    CI0273
                          VALUE                SPACE.                   CI0273
                                                                        AM0135
      *-----> PCB address list for calling CI0135...                    AM0135
      *                                                                 AM0135
       01                 CI0135-PCB-ADDRESS-LIST.                      AM0135
           05             CI0135-PCB-CH1P-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CCRP-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CPRP-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CBTP-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CA1P-PTR1      POINTER.            AM0135
       01                 CL01.                                         CI0273
            10            CL01-CL01K.                                   CI0273
            11            CL01-C199.                                    CI0273
            12            CL01-CLID.                                    CI0273
            13            CL01-CLIDO  PICTURE  9(3).                    CI0273
            13            CL01-CLIDN.                                   CI0273
            14            CL01-CLIDNP PICTURE  X(12).                   CI0273
            14            CL01-CLIDND PICTURE  9(8).                    CI0273
            10            CL01-GECKD  PICTURE  9.                       CI0273
            10            CL01-GEMDA  PICTURE  9(8).                    CI0273
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0273
                          BINARY.                                       CI0273
            10            CL01-GECUC  PICTURE  99.                      CI0273
            10            CL01-CLDOR  PICTURE  9(8).                    CI0273
            10            CL01-CLLNG  PICTURE  XX.                      CI0273
            10            CL01-GESLC  PICTURE  99.                      CI0273
            10            CL01-CLTYP  PICTURE  X.                       CI0273
            10            CL01-CLCLS  PICTURE  9(3).                    CI0273
            10            CL01-CLTWRC PICTURE  99.                      CI0273
            10            CL01-CLPVC  PICTURE  99.                      CI0273
            10            CL01-CLIND  PICTURE  9(3).                    CI0273
            10            CL01-CLTRC  PICTURE  99.                      CI0273
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CL01-AYSIDA PICTURE  9(3).                    CI0273
            10            CL01-AYSID  PICTURE  9(5).                    CI0273
            10            CL01-CLSTR  PICTURE  9(2).                    CI0273
            10            CL01-CLC11  PICTURE  X.                       CI0273
            10            CL01-CLTIN  PICTURE  9(12).                   CI0273
            10            CL01-CLTND  PICTURE  9(8).                    CI0273
            10            CL01-CLTINC PICTURE  9.                       CI0273
            10            CL01-CCDWA  PICTURE  9.                       CI0273
            10            CL01-CICES  PICTURE  X.                       CI0273
            10            CL01-CLTRA  PICTURE  9(2).                    CI0273
            10            CL01-DIRSY  PICTURE  9(4)                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CL01-CFEDS  PICTURE  X.                       CI0273
            10            CL01-FILLER PICTURE  X(06).                   CI0273
       01                 CL03.                                         CI0273
            10            CL03-GEDLA  PICTURE  9(8).                    CI0273
            10            CL03-DDREP  PICTURE  9(8).                    CI0273
            10            CL03-DPRFR  PICTURE  9(8).                    CI0273
            10            CL03-IACCI  PICTURE  X.                       CI0273
            10            CL03-CLDOB  PICTURE  9(8).                    CI0273
            10            CL03-CLDOD  PICTURE  9(8).                    CI0273
            10            CL03-CLDTH  PICTURE  X.                       CI0273
            10            CL03-CCINI  PICTURE  X.                       CI0273
            10            CL03-FILLER PICTURE  X(1).                    CI0273
            10            CL03-CLAIN  PICTURE  S9(11)                   CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CL03-CCAOD  PICTURE  999.                     CI0273
            10            CL03-CLMAR  PICTURE  X.                       CI0273
            10            CL03-C198.                                    CI0273
            11            CL03-CLNAM.                                   CI0273
            12            CL03-CLNAMH PICTURE  X(6).                    CI0273
            12            CL03-CLNAMF PICTURE  X(20).                   CI0273
            12            CL03-CLNAMM.                                  CI0273
            13            CL03-CLNAMI PICTURE  X.                       CI0273
            13            CL03-CLNAMR PICTURE  X(14).                   CI0273
            12            CL03-CLNAML PICTURE  X(25).                   CI0273
            12            CL03-CLNAMS PICTURE  X(4).                    CI0273
            10            CL03-FILLER PICTURE  X(10).                   CI0273
            10            CL03-MPRFS  PICTURE  X(4).                    CI0273
            10            CL03-CLOCC  PICTURE  9(3).                    CI0273
            10            CL03-CLRET  PICTURE  X.                       CI0273
            10            CL03-IOCOB  PICTURE  X.                       CI0273
            10            CL03-CLSEX  PICTURE  X.                       CI0273
            10            CL03-CLWIL  PICTURE  X.                       CI0273
            10            CL03-GECFC  PICTURE  99.                      CI0273
            10            CL03-GECFY  PICTURE  9(4).                    CI0273
            10            CL03-ICUSC  PICTURE  X.                       CI0273
            10            CL03-MCTYC  PICTURE  X(20).                   CI0273
            10            CL03-CLWIP  PICTURE  X.                       CI0273
            10            CL03-CLCTXF PICTURE  99.                      CI0273
            10            CL03-CLCUS  PICTURE  99.                      CI0273
            10            CL03-NPDLU  PICTURE  9(5).                    CI0273
            10            CL03-CLEMI  PICTURE  X.                       CI0273
            10            CL03-GEPHNH PICTURE  X(14).                   CI0273
            10            CL03-GEPHNB PICTURE  X(14).                   CI0273
            10            CL03-GEPHNX PICTURE  9(4).                    CI0273
            10            CL03-GEPHNA PICTURE  X(14).                   CI0273
            10            CL03-FILLER PICTURE  X(3).                    CI0273
            10            CL03-IAPRT  PICTURE  X.                       CI0273
            10            CL03-CEMSC  PICTURE  X.                       CI0273
            10            CL03-CSEPS  PICTURE  X.                       CI0273
            10            CL03-CRACE  PICTURE  X.                       CI0273
            10            CL03-CNIRA  PICTURE  X.                       CI0273
            10            CL03-FILLER PICTURE  X(11).                   CI0273
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0020           PIC X(8) VALUE 'CI0020P '.                  AM0020
       01  CI0085           PIC X(8) VALUE 'CI0085P '.                  AM0085
       01  CI0135           PIC X(8) VALUE 'CI0135P '.                  AM0135
       01  CI0222           PIC X(8) VALUE 'CI0222P '.                  AM0222
       01  CI0265           PIC X(8)  VALUE 'CI0265P '.                 AM0265
       01                 CT01.                                         CI0273
            10            CT01-CT01K.                                   CI0273
            11            CT01-C299.                                    CI0273
            12            CT01-CTID.                                    CI0273
            13            CT01-CTIDA  PICTURE  9(3).                    CI0273
            13            CT01-CTIDN.                                   CI0273
            14            CT01-CTIDNP PICTURE  X(13).                   CI0273
            14            CT01-CTIDND PICTURE  9(11).                   CI0273
            10            CT01-GECKD  PICTURE  9.                       CI0273
            10            CT01-GEMDA  PICTURE  9(8).                    CI0273
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0273
                          BINARY.                                       CI0273
            10            CT01-GECUC  PICTURE  99.                      CI0273
            10            CT01-CTAUL  PICTURE  9(3).                    CI0273
            10            CT01-DIRAC  PICTURE  9(4).                    CI0273
            10            CT01-CTCCI  PICTURE  X.                       CI0273
            10            CT01-CTCUS  PICTURE  999.                     CI0273
            10            CT01-CTEFD  PICTURE  9(8).                    CI0273
            10            CT01-CTIAD  PICTURE  9(8).                    CI0273
            10            CT01-CLCUS  PICTURE  99.                      CI0273
            10            CT01-CAMMB  PICTURE  X(3).                    CI0273
            10            CT01-CKPMM  PICTURE  X.                       CI0273
            10            CT01-CTLAD  PICTURE  9(8).                    CI0273
            10            CT01-IPERS  PICTURE  X.                       CI0273
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CT01-CTLAT  PICTURE  9(8).                    CI0273
            10            CT01-CTLATC PICTURE  9(6).                    CI0273
            10            CT01-IMEGA  PICTURE  X.                       CI0273
            10            CT01-DIRAB  PICTURE  9(8).                    CI0273
            10            CT01-COLRQ  PICTURE  X.                       CI0273
            10            CT01-ZDA04  PICTURE  X(4).                    CI0273
            10            CT01-CTLPD  PICTURE  9(8).                    CI0273
            10            CT01-CIRASP PICTURE  9.                       CI0273
            10            CT01-CIRATP PICTURE  99.                      CI0273
            10            CT01-DRTHC  PICTURE  9(8).                    CI0273
            10            CT01-CPPTC  PICTURE  X.                       CI0273
            10            CT01-ZDA06  PICTURE  X(6).                    CI0273
            10            CT01-CTACD  PICTURE  9(8).                    CI0273
            10            CT01-CTNLI  PICTURE  X.                       CI0273
            10            CT01-CTRHO  PICTURE  9(8).                    CI0273
            10            CT01-CTSGD  PICTURE  9(8).                    CI0273
            10            CT01-CPATP  PICTURE  X(1).                    CI0273
            10            CT01-IRSTA  PICTURE  X.                       CI0273
            10            CT01-CTSTA  PICTURE  99.                      CI0273
            10            CT01-CTSSC  PICTURE  99.                      CI0273
            10            CT01-PRLIN  PICTURE  9(3).                    CI0273
            10            CT01-PRCOD  PICTURE  9(5).                    CI0273
            10            CT01-PRSCD  PICTURE  X(9).                    CI0273
            10            CT01-CTLNI  PICTURE  X.                       CI0273
            10            CT01-AYSIDA PICTURE  9(3).                    CI0273
            10            CT01-AYSID  PICTURE  9(5).                    CI0273
            10            CT01-CTBMC  PICTURE  99.                      CI0273
            10            CT01-CINAR  PICTURE  99.                      CI0273
            10            CT01-CPHTR  PICTURE  X.                       CI0273
            10            CT01-CDSTR  PICTURE  XX.                      CI0273
            10            CT01-CQACT  PICTURE  999.                     CI0273
            10            CT01-CIRAS  PICTURE  999.                     CI0273
            10            CT01-CIRAT  PICTURE  999.                     CI0273
            10            CT01-CLRAY  PICTURE  9(5).                    CI0273
            10            CT01-CATTP  PICTURE  X.                       CI0273
       01                 CT07.                                         CI0273
            10            CT07-CT07K.                                   CI0273
            11            CT07-C199.                                    CI0273
            12            CT07-CLID.                                    CI0273
            13            CT07-CLIDO  PICTURE  9(3).                    CI0273
            13            CT07-CLIDN.                                   CI0273
            14            CT07-CLIDNP PICTURE  X(12).                   CI0273
            14            CT07-CLIDND PICTURE  9(8).                    CI0273
       01                 CT09.                                         CI0273
            10            CT09-A100.                                    CI0273
            11            CT09-GELL   PICTURE  9(4)                     CI0273
                          BINARY.                                       CI0273
            11            CT09-CT09K.                                   CI0273
            12            CT09-CLCTRC PICTURE  9(3).                    CI0273
            11            CT09-GERSD  PICTURE  9(8).                    CI0273
            11            CT09-GERED  PICTURE  9(8).                    CI0273
            10            CT09-A199.                                    CI0273
            11            CT09-FILLER PICTURE  X(20).                   CI0273
            10            CT09-A101                                     CI0273
                          REDEFINES            CT09-A199.               CI0273
            11            CT09-GECSQ  PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            CT09-CTAXR  PICTURE  X.                       CI0273
            11            CT09-GETAI  PICTURE  X.                       CI0273
            11            CT09-CTLACD PICTURE  9(8).                    CI0273
            11            CT09-GEPCS  PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            CT09-A102                                     CI0273
                          REDEFINES            CT09-A199.               CI0273
            11            CT09-CLPID  PICTURE  9(9).                    CI0273
      ******************************************************            AADA81
      ****      WORK AREAS FOR COMMON DATE UTILITY       ***            AADA81
      ******************************************************            AADA81
      **                                                                AADA81
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA81
      **                                                                AADA81
      **   SEGMENT DD30 - FUNCTION LAYOUT                               AADA81
      **                                                                AADA81
      *!WF DSP=DD DSL=DD SEL=30 FOR=I DES=2 LEV=1                       AADA81
       01                 DD30.                                         CI0273
            10            DD30-CDTFN  PICTURE  9(4)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            DD30-CDTSF  PICTURE  9(4)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            DD30-CDTSC  PICTURE  9(4)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            DD30-FILLER PICTURE  X(40)                    CI0273
                          VALUE                SPACE.                   CI0273
       01                 DD33.                                         CI0273
            10            DD33-DTGRG.                                   CI0273
            11            DD33-DTGCY.                                   CI0273
            12            DD33-DTGCC  PICTURE  9(2)                     CI0273
                          VALUE                ZERO.                    CI0273
            12            DD33-DTGYY  PICTURE  9(2)                     CI0273
                          VALUE                ZERO.                    CI0273
            11            DD33-DTGMM  PICTURE  9(2)                     CI0273
                          VALUE                ZERO.                    CI0273
            11            DD33-DTGDD  PICTURE  9(2)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            DD33-DTJULC.                                  CI0273
            11            DD33-DTJCY.                                   CI0273
            12            DD33-DTJCC  PICTURE  9(2)                     CI0273
                          VALUE                ZERO.                    CI0273
            12            DD33-DTJYY  PICTURE  9(2)                     CI0273
                          VALUE                ZERO.                    CI0273
            11            DD33-DTJDDC PICTURE  S9(3)                    CI0273
                          VALUE                ZERO.                    CI0273
            11            DD33-DTJDD                                    CI0273
                          REDEFINES            DD33-DTJDDC              CI0273
               PICTURE    9(3).                                         CI0273
            10            DD33-DTJUL                                    CI0273
                          REDEFINES            DD33-DTJULC              CI0273
               PICTURE    9(7).                                         CI0273
            10            DD33-DTDYR  PICTURE  9(3)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            DD33-DTDMO  PICTURE  9(2)                     CI0273
                          VALUE                ZERO.                    CI0273
            10            DD33-FILLER PICTURE  X(18)                    CI0273
                          VALUE                SPACE.                   CI0273
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
       01                 XW05.                                         CI0273
            10            XW05-XW06.                                    CI0273
            11            XW05-XDBPCB.                                  CI0273
            12            XW05-XDBDNM PICTURE  X(08)                    CI0273
                          VALUE                SPACE.                   CI0273
            12            XW05-XSEGLV PICTURE  X(02)                    CI0273
                          VALUE                SPACE.                   CI0273
            12            XW05-XRC    PICTURE  X(02)                    CI0273
                          VALUE                SPACE.                   CI0273
            12            XW05-XPROPT PICTURE  X(04)                    CI0273
                          VALUE                SPACE.                   CI0273
            12            XW05-FILLER PICTURE  S9(5)                    CI0273
                          VALUE                ZERO                     CI0273
                          BINARY.                                       CI0273
            12            XW05-XSEGNM PICTURE  X(08)                    CI0273
                          VALUE                SPACE.                   CI0273
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0273
                          VALUE                ZERO                     CI0273
                          BINARY.                                       CI0273
            12            XW05-XSEGNB PICTURE  9(05)                    CI0273
                          VALUE                ZERO                     CI0273
                          BINARY.                                       CI0273
            12            XW05-XCOKEY PICTURE  X(70)                    CI0273
                          VALUE                SPACE.                   CI0273
            10            XW05-XW07.                                    CI0273
            11            XW05-XIOPCB.                                  CI0273
            12            XW05-XTERMI PICTURE  X(08)                    CI0273
                          VALUE                SPACE.                   CI0273
            12            XW05-FILLER PICTURE  XX                       CI0273
                          VALUE                SPACE.                   CI0273
            12            XW05-XRC1   PICTURE  X(02)                    CI0273
                          VALUE                SPACE.                   CI0273
            12            XW05-FILLER PICTURE  X(12)                    CI0273
                          VALUE                SPACE.                   CI0273
            12            XW05-XMODNM PICTURE  X(8)                     CI0273
                          VALUE                SPACE.                   CI0273
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0273
                          VALUE                ZERO.                    CI0273
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0273
                          VALUE                ZERO.                    CI0273
            10            XW05-XGU    PICTURE  X(4)                     CI0273
                          VALUE                'GU  '.                  CI0273
            10            XW05-XGHU   PICTURE  X(4)                     CI0273
                          VALUE                'GHU '.                  CI0273
            10            XW05-XGN    PICTURE  X(4)                     CI0273
                          VALUE                'GN  '.                  CI0273
            10            XW05-XGHN   PICTURE  X(4)                     CI0273
                          VALUE                'GHN '.                  CI0273
            10            XW05-XGNP   PICTURE  X(4)                     CI0273
                          VALUE                'GNP '.                  CI0273
            10            XW05-XGHNP  PICTURE  X(4)                     CI0273
                          VALUE                'GHNP'.                  CI0273
            10            XW05-XREPL  PICTURE  XXXX                     CI0273
                          VALUE                'REPL'.                  CI0273
            10            XW05-XISRT  PICTURE  X(4)                     CI0273
                          VALUE                'ISRT'.                  CI0273
            10            XW05-XDLET  PICTURE  X(4)                     CI0273
                          VALUE                'DLET'.                  CI0273
            10            XW05-XOPEN  PICTURE  X(4)                     CI0273
                          VALUE                'OPEN'.                  CI0273
            10            XW05-XCLSE  PICTURE  X(4)                     CI0273
                          VALUE                'CLSE'.                  CI0273
            10            XW05-XCHKP  PICTURE  X(4)                     CI0273
                          VALUE                'CHKP'.                  CI0273
            10            XW05-XXRST  PICTURE  X(4)                     CI0273
                          VALUE                'XRST'.                  CI0273
            10            XW05-XTERM  PICTURE  X(4)                     CI0273
                          VALUE                'TERM'.                  CI0273
            10            XW05-XNFPAC PICTURE  X(13)                    CI0273
                          VALUE                SPACE.                   CI0273
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0273
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0273
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
      *****************************************************************
      *              FLAGS USED IN THE PROGRAM
      *****************************************************************
      **
       01  CT07-CF                 PIC X(1).
       01  CT09-CF                 PIC X(1).
       01  CL03-CF                 PIC X(1).
       01  GC03-CF                 PIC X(1).
       01                 GC01.                                         CI0273
            10            GC01-GC01K.                                   CI0273
            11            GC01-C299.                                    CI0273
            12            GC01-CTID.                                    CI0273
            13            GC01-CTIDA  PICTURE  9(3).                    CI0273
            13            GC01-CTIDN.                                   CI0273
            14            GC01-CTIDNP PICTURE  X(13).                   CI0273
            14            GC01-CTIDND PICTURE  9(11).                   CI0273
            10            GC01-DCAG9L PICTURE  9(8).                    CI0273
            10            GC01-NAASQL PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            GC01-ICUST  PICTURE  X.                       CI0273
            10            GC01-NSEQ4B PICTURE  9(8)                     CI0273
                          BINARY.                                       CI0273
            10            GC01-PRCOD  PICTURE  9(5).                    CI0273
            10            GC01-PRSCD  PICTURE  X(9).                    CI0273
            10            GC01-FILLER PICTURE  X(8).                    CI0273
       01                 GC03.                                         CI0273
            10            GC03-GELL   PICTURE  9(4)                     CI0273
                          BINARY.                                       CI0273
            10            GC03-GD00.                                    CI0273
            11            GC03-GC03K.                                   CI0273
            12            GC03-DCACG9 PICTURE  9(8).                    CI0273
            12            GC03-NAASQ  PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-CAATY  PICTURE  9(3).                    CI0273
            11            GC03-CVSYS  PICTURE  X(2).                    CI0273
            11            GC03-CACTO  PICTURE  9(3).                    CI0273
            11            GC03-CATRN.                                   CI0273
            12            GC03-CATRF  PICTURE  9(3).                    CI0273
            12            GC03-CATRS  PICTURE  9(3).                    CI0273
            11            GC03-CASTC  PICTURE  99.                      CI0273
            11            GC03-IPULL  PICTURE  X.                       CI0273
            11            GC03-GEAUN  PICTURE  9(5).                    CI0273
            11            GC03-GEOPD2 PICTURE  X(8).                    CI0273
            11            GC03-NBTCH  PICTURE  9(4).                    CI0273
            11            GC03-DEFFT  PICTURE  9(8).                    CI0273
            11            GC03-NSUNT  PICTURE  9(4).                    CI0273
            11            GC03-ITRAN  PICTURE  X.                       CI0273
            11            GC03-DLAUP1 PICTURE  9(8).                    CI0273
            11            GC03-ADRET  PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-TTRMS  PICTURE  X(12).                   CI0273
            11            GC03-IDELT  PICTURE  X.                       CI0273
            11            GC03-GEOPDM PICTURE  X(8).                    CI0273
            11            GC03-FILLER PICTURE  X(07).                   CI0273
            10            GC03-GD09.                                    CI0273
            11            GC03-FILLER PICTURE  X(70).                   CI0273
            10            GC03-GD01                                     CI0273
                          REDEFINES            GC03-GD09.               CI0273
            11            GC03-ADBRQ  PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-CTRTP  PICTURE  X(2).                    CI0273
            11            GC03-CPORT  PICTURE  X.                       CI0273
            11            GC03-CSCRNU PICTURE  X(4).                    CI0273
            11            GC03-DLAUP  PICTURE  9(8).                    CI0273
            11            GC03-CTWHAT PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-PWHLD  PICTURE  S999V9(5)                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-IWTHH  PICTURE  X.                       CI0273
            11            GC03-NDRFT  PICTURE  9(5).                    CI0273
            11            GC03-IDPAP  PICTURE  X.                       CI0273
            11            GC03-GETIM  PICTURE  S9(7)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-QNACT  PICTURE  9(3).                    CI0273
            11            GC03-AEDRQ  PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-IPLIN  PICTURE  X.                       CI0273
            11            GC03-CLIDNB PICTURE  9(8).                    CI0273
            11            GC03-CSLCT  PICTURE  X.                       CI0273
            11            GC03-ITELE  PICTURE  X.                       CI0273
            11            GC03-FILLER PICTURE  X(06).                   CI0273
            10            GC03-GD02                                     CI0273
                          REDEFINES            GC03-GD09.               CI0273
            11            GC03-CSYST  PICTURE  99.                      CI0273
            11            GC03-FILLER PICTURE  X.                       CI0273
            11            GC03-ACASH  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-DTRAC  PICTURE  9(8).                    CI0273
            11            GC03-CTRSO  PICTURE  9(02).                   CI0273
            11            GC03-NTRCE  PICTURE  9(06).                   CI0273
            11            GC03-GECKD1 PICTURE  9.                       CI0273
            11            GC03-CCOLL  PICTURE  X(3).                    CI0273
            11            GC03-CLTDP  PICTURE  X(3).                    CI0273
            11            GC03-PSLLD  PICTURE  S99V999                  CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-ISLOR  PICTURE  X.                       CI0273
            11            GC03-ITPAC  PICTURE  X.                       CI0273
            11            GC03-CPMTCA PICTURE  XXX.                     CI0273
            11            GC03-CSERV  PICTURE  X(3).                    CI0273
            11            GC03-ACOMO  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-IPLIN1 PICTURE  X.                       CI0273
            11            GC03-INQEX  PICTURE  X.                       CI0273
            11            GC03-CTKRAA PICTURE  X(12).                   CI0273
            11            GC03-CCSMQ  PICTURE  X.                       CI0273
            11            GC03-IVAEX1 PICTURE  X.                       CI0273
            11            GC03-IHPMT  PICTURE  X(1).                    CI0273
            11            GC03-GETIM3 PICTURE  S9(7)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            GC03-GD03                                     CI0273
                          REDEFINES            GC03-GD09.               CI0273
            11            GC03-CATRNC PICTURE  9(6).                    CI0273
            11            GC03-APRNT1 PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-QSHOWT PICTURE  S9(10)V999               CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-ACINVT PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-ACOMO7 PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-QSHOMW PICTURE  S9(10)V999               CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-ATAXT3 PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-CTSTR  PICTURE  9(2).                    CI0273
            11            GC03-ICIRA  PICTURE  X.                       CI0273
            11            GC03-GETIM2 PICTURE  S9(7)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-CPMTCX PICTURE  XX.                      CI0273
            11            GC03-FILLER PICTURE  X(16).                   CI0273
            10            GC03-GD99.                                    CI0273
            11            GC03-FILLER PICTURE  X(248).                  CI0273
            10            GC03-GD10                                     CI0273
                          REDEFINES            GC03-GD99.               CI0273
            11            GC03-MROTC  PICTURE  X(7).                    CI0273
            11            GC03-CEDSC  PICTURE  9(1).                    CI0273
            11            GC03-ILPOI  PICTURE  X(1).                    CI0273
            11            GC03-AWRCH  PICTURE  S9(3)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-CHCOC1 PICTURE  9(2).                    CI0273
            11            GC03-CHCOC2 PICTURE  9(2).                    CI0273
            11            GC03-CHCOC3 PICTURE  9(2).                    CI0273
            11            GC03-CHCOC4 PICTURE  9(2).                    CI0273
            11            GC03-CMCOC1 PICTURE  9(3).                    CI0273
            11            GC03-CMCOC2 PICTURE  9(3).                    CI0273
            11            GC03-CMCOC3 PICTURE  9(3).                    CI0273
            11            GC03-GD11.                                    CI0273
            12            GC03-FILLER PICTURE  X(219).                  CI0273
            11            GC03-GD12                                     CI0273
                          REDEFINES            GC03-GD11.               CI0273
            12            GC03-CELLO  PICTURE  9(1).                    CI0273
            12            GC03-CECLO  PICTURE  9(1).                    CI0273
            12            GC03-AEXML  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-CEPI   PICTURE  X(1).                    CI0273
            12            GC03-CEXTY  PICTURE  X.                       CI0273
            12            GC03-CROPC  PICTURE  9(1).                    CI0273
            12            GC03-CPUTY  PICTURE  9(1).                    CI0273
            12            GC03-IMCII  PICTURE  X(1).                    CI0273
            12            GC03-GEMISC                                   CI0273
                          OCCURS       010     TIMES.                   CI0273
            13            GC03-AMGLA  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            13            GC03-CMGLC  PICTURE  9(1).                    CI0273
            13            GC03-NMGLN  PICTURE  9(4).                    CI0273
            12            GC03-ACTRN  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-IWRBK  PICTURE  X.                       CI0273
            12            GC03-IFEDX  PICTURE  X.                       CI0273
            12            GC03-ICNTR  PICTURE  X.                       CI0273
            12            GC03-IOCKH  PICTURE  X.                       CI0273
            12            GC03-ICRCK  PICTURE  X.                       CI0273
            12            GC03-NHMPN  PICTURE  S9(10)                   CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-ITELR1 PICTURE  X.                       CI0273
            11            GC03-GD13                                     CI0273
                          REDEFINES            GC03-GD11.               CI0273
            12            GC03-DREDO  PICTURE  9(8).                    CI0273
            12            GC03-CATRNR PICTURE  9(6).                    CI0273
            12            GC03-CEVN   PICTURE  9(9).                    CI0273
            12            GC03-ISUSP  PICTURE  X(1).                    CI0273
            11            GC03-GD15                                     CI0273
                          REDEFINES            GC03-GD11.               CI0273
            12            GC03-CPUTZ  PICTURE  9(1).                    CI0273
            12            GC03-CETLB  PICTURE  9(3).                    CI0273
            12            GC03-QTRMC  PICTURE  9(3).                    CI0273
            12            GC03-DEFFTE PICTURE  9(8).                    CI0273
            12            GC03-DEFFTF PICTURE  9(8).                    CI0273
            12            GC03-DEFFTG PICTURE  9(8).                    CI0273
            12            GC03-XZ1A   PICTURE  X.                       CI0273
            12            GC03-XZ1B   PICTURE  X.                       CI0273
            12            GC03-XZ1C   PICTURE  X.                       CI0273
            12            GC03-XZ1D   PICTURE  X.                       CI0273
            12            GC03-XZ1E   PICTURE  X.                       CI0273
            12            GC03-XZ1F   PICTURE  X.                       CI0273
            12            GC03-XZ1G   PICTURE  X.                       CI0273
            12            GC03-XZ1H   PICTURE  X.                       CI0273
            12            GC03-XZ1I   PICTURE  X.                       CI0273
            12            GC03-DEFFTH PICTURE  9(8).                    CI0273
            11            GC03-GD19                                     CI0273
                          REDEFINES            GC03-GD11.               CI0273
            12            GC03-GD11.                                    CI0273
            13            GC03-FILLER PICTURE  X(219).                  CI0273
            10            GC03-GD20                                     CI0273
                          REDEFINES            GC03-GD99.               CI0273
            11            GC03-ADDACT PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-ISIGV  PICTURE  X.                       CI0273
            11            GC03-IALLF  PICTURE  X.                       CI0273
            11            GC03-QSHOWQ PICTURE  S9(9)V999                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-CCDSCW PICTURE  9(2).                    CI0273
            11            GC03-IDWRL  PICTURE  X.                       CI0273
            11            GC03-ITELR  PICTURE  X.                       CI0273
            11            GC03-IABIN  PICTURE  X.                       CI0273
            11            GC03-PACT1  PICTURE  S999V999                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-IBFAF  PICTURE  X.                       CI0273
            11            GC03-IFRSA  PICTURE  X.                       CI0273
            11            GC03-ICRCAN PICTURE  X.                       CI0273
            11            GC03-ACACTV PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-AGFND  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-QCSHOW PICTURE  S9(9)V999                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-QCSHIS PICTURE  S9(9)V999                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-NDTRC  PICTURE  9(8).                    CI0273
            11            GC03-CAERU  PICTURE  X(4).                    CI0273
            11            GC03-IFDGO  PICTURE  X.                       CI0273
            11            GC03-PSLLD2 PICTURE  S99V999                  CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-ISLOR2 PICTURE  X.                       CI0273
            11            GC03-QSFIO  PICTURE  S9(10)V999               CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-QSFID  PICTURE  S9(10)V999               CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-CGDIN  PICTURE  X.                       CI0273
            11            GC03-DGDIN  PICTURE  9(8).                    CI0273
            10            GC03-GD30                                     CI0273
                          REDEFINES            GC03-GD99.               CI0273
            11            GC03-ISKED  PICTURE  X.                       CI0273
            11            GC03-CENXC  PICTURE  9(2).                    CI0273
            11            GC03-GD31.                                    CI0273
            12            GC03-FILLER PICTURE  X(245).                  CI0273
            11            GC03-GD32                                     CI0273
                          REDEFINES            GC03-GD31.               CI0273
            12            GC03-IABIN1 PICTURE  X.                       CI0273
            12            GC03-CLDOD  PICTURE  9(8).                    CI0273
            12            GC03-NCLAM  PICTURE  9(5)                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-ISURR  PICTURE  X.                       CI0273
            12            GC03-GEHCD  PICTURE  9(3).                    CI0273
            12            GC03-CRATC  PICTURE  9(4).                    CI0273
            12            GC03-AMAXD  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-ASCHGA PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-APYOM  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-IWTHH1 PICTURE  X.                       CI0273
            12            GC03-CPAYCL PICTURE  X(2).                    CI0273
            12            GC03-CTSAO  PICTURE  X.                       CI0273
            12            GC03-NCONF  PICTURE  9(08).                   CI0273
            12            GC03-CLID   PICTURE  X(23).                   CI0273
            12            GC03-CARTY  PICTURE  99.                      CI0273
            12            GC03-NARRS  PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-CARTZ  PICTURE  99.                      CI0273
            12            GC03-NAPDS  PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-CPMTO  PICTURE  X.                       CI0273
            12            GC03-DNPMT  PICTURE  9(8).                    CI0273
            12            GC03-IPCTV  PICTURE  X.                       CI0273
            12            GC03-IMECH  PICTURE  X(01).                   CI0273
            12            GC03-IMVAO  PICTURE  X(1).                    CI0273
            12            GC03-AMVA1  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-CACTS  PICTURE  X.                       CI0273
            12            GC03-CTSPP  PICTURE  X(1).                    CI0273
            12            GC03-CACT4  PICTURE  X(2).                    CI0273
            12            GC03-IVAEX  PICTURE  X.                       CI0273
            12            GC03-DFPMT  PICTURE  9(8).                    CI0273
            12            GC03-IDEMD  PICTURE  X.                       CI0273
            12            GC03-IOFST  PICTURE  X.                       CI0273
            12            GC03-AMXLB  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-ACULB  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-DEIRNB PICTURE  9(8).                    CI0273
            12            GC03-DEFFE  PICTURE  9(8).                    CI0273
            12            GC03-DEFFR  PICTURE  9(8).                    CI0273
            12            GC03-ISPUP  PICTURE  X.                       CI0273
            12            GC03-CPNCG  PICTURE  X.                       CI0273
            12            GC03-IEXPU  PICTURE  X.                       CI0273
            12            GC03-IPPCF  PICTURE  X.                       CI0273
            12            GC03-NAAPT  PICTURE  9(2).                    CI0273
            12            GC03-PWHLDS PICTURE  S999V9(5)                CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-ISWHO  PICTURE  X(1).                    CI0273
            11            GC03-GD33                                     CI0273
                          REDEFINES            GC03-GD31.               CI0273
            12            GC03-CPAYC  PICTURE  X(2).                    CI0273
            12            GC03-ADBRQX PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-ADBRQV PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-APTXR  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-CTRTPE PICTURE  X(2).                    CI0273
            12            GC03-NCLAMI PICTURE  S9(9)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-CLIDO8 PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-CLIDN  PICTURE  X(20).                   CI0273
            12            GC03-DSET01 PICTURE  S9(8)                    CI0273
                          BINARY.                                       CI0273
            12            GC03-CTSET1 PICTURE  S9(6)                    CI0273
                          BINARY.                                       CI0273
            12            GC03-DSET02 PICTURE  S9(8)                    CI0273
                          BINARY.                                       CI0273
            12            GC03-CTSET2 PICTURE  S9(6)                    CI0273
                          BINARY.                                       CI0273
            11            GC03-GD34                                     CI0273
                          REDEFINES            GC03-GD31.               CI0273
            12            GC03-QNOFM  PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-CLTRM  PICTURE  99.                      CI0273
            12            GC03-AMXLN  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-ALADJ  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-ACHK   PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-APRMO  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-IMECH1 PICTURE  X(01).                   CI0273
            12            GC03-CACT41 PICTURE  X(2).                    CI0273
            12            GC03-ACDSCC PICTURE  S9(05)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-ACDSCD PICTURE  S9(05)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-GD39                                     CI0273
                          REDEFINES            GC03-GD31.               CI0273
            12            GC03-GD31.                                    CI0273
            13            GC03-FILLER PICTURE  X(245).                  CI0273
            10            GC03-GD40                                     CI0273
                          REDEFINES            GC03-GD99.               CI0273
            11            GC03-NTR    PICTURE  9(8).                    CI0273
            11            GC03-NPBNC  PICTURE  X(24).                   CI0273
            11            GC03-CRREV  PICTURE  X(3).                    CI0273
            11            GC03-CSUSL  PICTURE  S9.                      CI0273
            11            GC03-NMGLN1 PICTURE  9(4).                    CI0273
            11            GC03-DCAC92 PICTURE  9(8).                    CI0273
            11            GC03-NAASQ3 PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-GD49.                                    CI0273
            12            GC03-FILLER PICTURE  X(198).                  CI0273
            11            GC03-GD41                                     CI0273
                          REDEFINES            GC03-GD49.               CI0273
            12            GC03-CRREF  PICTURE  9(2).                    CI0273
            12            GC03-CORIR  PICTURE  X(02).                   CI0273
            12            GC03-CIPDB  PICTURE  X(03).                   CI0273
            12            GC03-CPAYH  PICTURE  X(02).                   CI0273
            12            GC03-NAMEX  PICTURE  9(15)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC03-DCHAE  PICTURE  9(4).                    CI0273
            12            GC03-DRQST  PICTURE  S9(8)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-GD42                                     CI0273
                          REDEFINES            GC03-GD49.               CI0273
            12            GC03-CPMTCB PICTURE  X(3).                    CI0273
            10            GC03-GD50                                     CI0273
                          REDEFINES            GC03-GD99.               CI0273
            11            GC03-ALOAD  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-PSLLD4 PICTURE  S99V999                  CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-CSUSL1 PICTURE  S9.                      CI0273
            11            GC03-CRREV1 PICTURE  X(3).                    CI0273
            11            GC03-ADDAC  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-DL13.                                    CI0273
            12            GC03-GEYR   PICTURE  9(4).                    CI0273
            12            GC03-GEMTH  PICTURE  99.                      CI0273
            12            GC03-NDAY   PICTURE  99.                      CI0273
            11            GC03-NSEQ3P PICTURE  S9(5)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-XZ6A   PICTURE  X(6).                    CI0273
            11            GC03-XZ7    PICTURE  X(7).                    CI0273
            11            GC03-XZ6B   PICTURE  X(6).                    CI0273
            11            GC03-XZ6    PICTURE  X(6).                    CI0273
            11            GC03-XZ6C   PICTURE  X(6).                    CI0273
            11            GC03-XZ20   PICTURE  X(20).                   CI0273
            11            GC03-CATRN1 PICTURE  9(6).                    CI0273
            11            GC03-ADDAC2 PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-ATAXT2 PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-ACOMOT PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-XZ5    PICTURE  X(5).                    CI0273
            11            GC03-IREVD  PICTURE  X(1).                    CI0273
            11            GC03-ISUSP1 PICTURE  X(1).                    CI0273
            11            GC03-XZ6D   PICTURE  X(6).                    CI0273
            11            GC03-XZ13   PICTURE  X(13).                   CI0273
            11            GC03-CWHTP2 PICTURE  X(3).                    CI0273
            11            GC03-CWHTP3 PICTURE  X(3).                    CI0273
            11            GC03-DTREN  PICTURE  9(8).                    CI0273
            11            GC03-NAASQ1 PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            GC03-GD51                                     CI0273
                          REDEFINES            GC03-GD99.               CI0273
            11            GC03-ADOMOT PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-ACGLT  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-ACGST  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-CTXMT  PICTURE  9(2).                    CI0273
            11            GC03-ALOAD3 PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-FILLER PICTURE  X(31).                   CI0273
            10            GC03-GD52                                     CI0273
                          REDEFINES            GC03-GD99.               CI0273
            11            GC03-DEFFT5 PICTURE  9(8).                    CI0273
            11            GC03-PSLLD5 PICTURE  S99V999                  CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-CSUSL2 PICTURE  S9.                      CI0273
            11            GC03-ALOAD2 PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-DL22.                                    CI0273
            12            GC03-NYEAR1 PICTURE  9(4).                    CI0273
            12            GC03-GEMTHA PICTURE  99.                      CI0273
            12            GC03-NDAY01 PICTURE  99.                      CI0273
            11            GC03-NSEQ3R PICTURE  S9(5)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-CWHTP  PICTURE  X(3).                    CI0273
            11            GC03-CWHFR  PICTURE  X(3).                    CI0273
            11            GC03-CATRN7 PICTURE  9(6).                    CI0273
            11            GC03-ATAXT5 PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-QSHOT  PICTURE  S9(10)V999               CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-ACINT3 PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-CWHTP1 PICTURE  X(3).                    CI0273
            11            GC03-CWHFR1 PICTURE  X(3).                    CI0273
            11            GC03-ACOMO5 PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-QSHOMU PICTURE  S9(10)V999               CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-ACASH1 PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-FILLER PICTURE  X(04).                   CI0273
            11            GC03-CATRN8 PICTURE  9(6).                    CI0273
            11            GC03-ALOAD1 PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-PSLLD1 PICTURE  S99V999                  CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-QSHOT1 PICTURE  S9(10)V999               CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-ACINT4 PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-CSUSL4 PICTURE  S9.                      CI0273
            11            GC03-ACOMO4 PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            GC03-GD60                                     CI0273
                          REDEFINES            GC03-GD99.               CI0273
            11            GC03-GEOPDD PICTURE  X(8)                     CI0273
                          OCCURS       005     TIMES.                   CI0273
            11            GC03-DLAUP3 PICTURE  9(8)                     CI0273
                          OCCURS       005     TIMES.                   CI0273
            11            GC03-GEOPDB PICTURE  X(8).                    CI0273
            11            GC03-DLAUP4 PICTURE  9(8).                    CI0273
            11            GC03-ITELR2 PICTURE  X.                       CI0273
            11            GC03-IPMTA  PICTURE  X.                       CI0273
            11            GC03-CCSMG  PICTURE  X.                       CI0273
            11            GC03-CPLEC  PICTURE  XX.                      CI0273
            11            GC03-CORTYA PICTURE  X(3).                    CI0273
            11            GC03-CACTBC PICTURE  X(1).                    CI0273
            11            GC03-CGSPIA PICTURE  X.                       CI0273
            11            GC03-IPTRDA PICTURE  X(01).                   CI0273
            11            GC03-GCUSPY PICTURE  X(12).                   CI0273
            11            GC03-CPALLA PICTURE  X(1).                    CI0273
            11            GC03-QSHO5A PICTURE  S9(9)V999                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-IFRSAB PICTURE  X.                       CI0273
            11            GC03-DELOI  PICTURE  9(8).                    CI0273
            11            GC03-IAROAA PICTURE  X.                       CI0273
            11            GC03-ACINVR PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-ILTINA PICTURE  X.                       CI0273
            11            GC03-ALOIDA PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC03-CFUNTA PICTURE  X(2).                    CI0273
            11            GC03-CLGND  PICTURE  X.                       CI0273
            11            GC03-CPH3U  PICTURE  X.                       CI0273
            11            GC03-GESTD  PICTURE  9(8).                    CI0273
            11            GC03-GEEND  PICTURE  9(8).                    CI0273
            11            GC03-CPMTF  PICTURE  99.                      CI0273
            11            GC03-CNAVR  PICTURE  X(1).                    CI0273
            10            GC03-GD70                                     CI0273
                          REDEFINES            GC03-GD99.               CI0273
            11            GC03-CMEMO  PICTURE  X(2).                    CI0273
            11            GC03-ALPLDT PICTURE  9(8).                    CI0273
            11            GC03-CTLPD  PICTURE  9(8).                    CI0273
            11            GC03-CPAYCM PICTURE  X(2).                    CI0273
       01                 GC06.                                         CI0273
            10            GC06-GELL   PICTURE  9(4)                     CI0273
                          BINARY.                                       CI0273
            10            GC06-GE00.                                    CI0273
            11            GC06-GC06K.                                   CI0273
            12            GC06-NPISQ  PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC06-ACOTD  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC06-PPOTD  PICTURE  S9(3)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC06-QPSTD  PICTURE  S9(7)V999                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC06-CPITC  PICTURE  99.                      CI0273
            11            GC06-ITRNB  PICTURE  X.                       CI0273
            11            GC06-FILLER PICTURE  X(14).                   CI0273
            10            GC06-GE98.                                    CI0273
            11            GC06-FILLER PICTURE  X(240).                  CI0273
            10            GC06-GE10                                     CI0273
                          REDEFINES            GC06-GE98.               CI0273
            11            GC06-CDELI  PICTURE  9(3).                    CI0273
            11            GC06-CPAYC  PICTURE  X(2).                    CI0273
            11            GC06-ICHKP  PICTURE  X.                       CI0273
            11            GC06-CLTIN  PICTURE  9(12).                   CI0273
            11            GC06-IFHAI  PICTURE  X.                       CI0273
            11            GC06-CDQUA  PICTURE  X(2).                    CI0273
            11            GC06-FILLER PICTURE  X(07).                   CI0273
            11            GC06-GE99.                                    CI0273
            12            GC06-FILLER PICTURE  X(212).                  CI0273
            11            GC06-GE01                                     CI0273
                          REDEFINES            GC06-GE99.               CI0273
            12            GC06-NTR    PICTURE  9(8).                    CI0273
            12            GC06-GECKD  PICTURE  9.                       CI0273
            12            GC06-NPBN   PICTURE  X(20).                   CI0273
            12            GC06-CCBAT  PICTURE  99.                      CI0273
            12            GC06-CLID4  PICTURE  X(23).                   CI0273
            12            GC06-GENAL1 PICTURE  X(30)                    CI0273
                          OCCURS       002     TIMES.                   CI0273
            12            GC06-GESAD1 PICTURE  X(30)                    CI0273
                          OCCURS       003     TIMES.                   CI0273
            11            GC06-GE02                                     CI0273
                          REDEFINES            GC06-GE99.               CI0273
            12            GC06-GENAL  PICTURE  X(30)                    CI0273
                          OCCURS       002     TIMES.                   CI0273
            12            GC06-GESAD  PICTURE  X(30)                    CI0273
                          OCCURS       003     TIMES.                   CI0273
            11            GC06-GE03                                     CI0273
                          REDEFINES            GC06-GE99.               CI0273
            12            GC06-NCHKN  PICTURE  9(11).                   CI0273
            11            GC06-GE04                                     CI0273
                          REDEFINES            GC06-GE99.               CI0273
            12            GC06-CTIDAP PICTURE  9(3).                    CI0273
            12            GC06-PRCOD  PICTURE  9(5).                    CI0273
            12            GC06-TDELI  PICTURE  X(30).                   CI0273
            12            GC06-CINCD  PICTURE  9(02).                   CI0273
            10            GC06-GE20                                     CI0273
                          REDEFINES            GC06-GE98.               CI0273
            11            GC06-C299.                                    CI0273
            12            GC06-CTID.                                    CI0273
            13            GC06-CTIDA  PICTURE  9(3).                    CI0273
            13            GC06-CTIDN.                                   CI0273
            14            GC06-CTIDNP PICTURE  X(13).                   CI0273
            14            GC06-CTIDND PICTURE  9(11).                   CI0273
            11            GC06-DCACG9 PICTURE  9(8).                    CI0273
            11            GC06-NAASQ  PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            GC06-CIRAP  PICTURE  XX.                      CI0273
            11            GC06-CTYPE  PICTURE  X.                       CI0273
            11            GC06-INACT  PICTURE  X.                       CI0273
            11            GC06-FILLER PICTURE  X(01).                   CI0273
            11            GC06-ITPAC  PICTURE  X.                       CI0273
            11            GC06-ITAXI  PICTURE  X.                       CI0273
            11            GC06-IOWNC  PICTURE  X.                       CI0273
            11            GC06-CDVCD  PICTURE  X(2).                    CI0273
            11            GC06-CTCUS  PICTURE  999.                     CI0273
            11            GC06-CPMTCB PICTURE  X(3).                    CI0273
            11            GC06-CASTC1 PICTURE  99.                      CI0273
            11            GC06-PRCOD1 PICTURE  9(5).                    CI0273
            11            GC06-CPRSC1 PICTURE  X(9).                    CI0273
            11            GC06-CPRTB  PICTURE  X.                       CI0273
            11            GC06-CBRKD  PICTURE  9(4).                    CI0273
            11            GC06-FILLER PICTURE  X(12).                   CI0273
            10            GC06-GE30                                     CI0273
                          REDEFINES            GC06-GE98.               CI0273
            11            GC06-CFIDC  PICTURE  X(5).                    CI0273
            11            GC06-CPHSE  PICTURE  9(2).                    CI0273
            11            GC06-FILLER PICTURE  X(05).                   CI0273
            11            GC06-IABIN  PICTURE  X.                       CI0273
            11            GC06-PDFND  PICTURE  S999V9(3)                CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            GC06-GE40                                     CI0273
                          REDEFINES            GC06-GE98.               CI0273
            11            GC06-CACCT  PICTURE  X.                       CI0273
            11            GC06-CPAYR  PICTURE  X(2).                    CI0273
            11            GC06-CDELI1 PICTURE  9(3).                    CI0273
            11            GC06-CATRN.                                   CI0273
            12            GC06-CATRF  PICTURE  9(3).                    CI0273
            12            GC06-CATRS  PICTURE  9(3).                    CI0273
            11            GC06-DEFFT  PICTURE  9(8).                    CI0273
            11            GC06-CTYPC  PICTURE  X.                       CI0273
            11            GC06-CIRAPA PICTURE  XX.                      CI0273
            11            GC06-FILLER PICTURE  X(09).                   CI0273
            11            GC06-GE49.                                    CI0273
            12            GC06-FILLER PICTURE  X(208).                  CI0273
            11            GC06-GE41                                     CI0273
                          REDEFINES            GC06-GE49.               CI0273
            12            GC06-NCHKN1 PICTURE  9(6).                    CI0273
            11            GC06-GE42                                     CI0273
                          REDEFINES            GC06-GE49.               CI0273
            12            GC06-CTID1.                                   CI0273
            13            GC06-CTIDA1 PICTURE  9(3).                    CI0273
            13            GC06-CTIDP1 PICTURE  X(13).                   CI0273
            13            GC06-CTIDN1 PICTURE  9(11).                   CI0273
            11            GC06-GE43                                     CI0273
                          REDEFINES            GC06-GE49.               CI0273
            12            GC06-GENAL2 PICTURE  X(30)                    CI0273
                          OCCURS       002     TIMES.                   CI0273
            12            GC06-GESAD2 PICTURE  X(30)                    CI0273
                          OCCURS       003     TIMES.                   CI0273
            11            GC06-GE44                                     CI0273
                          REDEFINES            GC06-GE49.               CI0273
            12            GC06-CTID01.                                  CI0273
            13            GC06-CTIDA6 PICTURE  9(3).                    CI0273
            13            GC06-NTIDP2 PICTURE  X(13).                   CI0273
            13            GC06-CTIDN2 PICTURE  9(11).                   CI0273
            12            GC06-GECKD2 PICTURE  9.                       CI0273
            12            GC06-PACCT  PICTURE  S999V99                  CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC06-PLOAN  PICTURE  S999V99                  CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC06-PADPT  PICTURE  S999V99                  CI0273
                          COMPUTATIONAL-3.                              CI0273
            12            GC06-IPCTL  PICTURE  X.                       CI0273
            12            GC06-IPCTP  PICTURE  X.                       CI0273
            12            GC06-CEUNT  PICTURE  S9(5)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            GC06-GE31                                     CI0273
                          REDEFINES            GC06-GE98.               CI0273
            11            GC06-GCUSPZ PICTURE  X(12).                   CI0273
      *                   MS03
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
       01                 NS00.                                         CI0273
          05              NS00-00.                                      CI0273
            10            NS00-NS00K.                                   CI0273
            11            NS00-PRCSTK PICTURE  XX.                      CI0273
          05              NS00-SUITE.                                   CI0273
            15       FILLER         PICTURE  X(00078).                  CI0273
       01                 NS20  REDEFINES      NS00.                    CI0273
            10       FILLER         PICTURE  X(00002).                  CI0273
            10            NS20-DCACG  PICTURE  9(8).                    CI0273
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            NS20-CCDAT  PICTURE  X(8).                    CI0273
            10            NS20-DCALP  PICTURE  X(12).                   CI0273
            10            NS20-DNACG  PICTURE  9(8).                    CI0273
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            NS20-CNDAT  PICTURE  X(8).                    CI0273
            10            NS20-DNALP  PICTURE  X(12).                   CI0273
            10            NS20-DCACD  PICTURE  X(10).                   CI0273
            10            NS20-FILLER PICTURE  X(4).                    CI0273
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *****************************************************************
      ****     AREA USED TO CALL CI0085
      *****************************************************************
      *!WF DSP=QT DSL=QT SEL=58 FOR=I LEV=1 PLT=QT
       01                 QT00.                                         CI0273
          05              QT00-SUITE.                                   CI0273
            15       FILLER         PICTURE  X(02300).                  CI0273
       01                 QT58  REDEFINES      QT00.                    CI0273
            10            QT58-QT5K.                                    CI0273
            11            QT58-C299.                                    CI0273
            12            QT58-CTID.                                    CI0273
            13            QT58-CTIDA  PICTURE  9(3).                    CI0273
            13            QT58-CTIDN.                                   CI0273
            14            QT58-CTIDNP PICTURE  X(13).                   CI0273
            14            QT58-CTIDND PICTURE  9(11).                   CI0273
            11            QT58-GECKD2 PICTURE  9.                       CI0273
            11            QT58-NSEQ5  PICTURE  9(5).                    CI0273
            11            QT58-CTSTA  PICTURE  99.                      CI0273
            11            QT58-CTSTAL PICTURE  X(10).                   CI0273
            11            QT58-CTOWN  PICTURE  9(3).                    CI0273
            11            QT58-CTTLN1 PICTURE  X(30).                   CI0273
            11            QT58-CTTLN2 PICTURE  X(30).                   CI0273
            11            QT58-CTTLN3 PICTURE  X(30).                   CI0273
            11            QT58-CTTBO1 PICTURE  X(45).                   CI0273
            11            QT58-CTTBO2 PICTURE  X(45).                   CI0273
            11            QT58-CTEFD  PICTURE  9(8).                    CI0273
            11            QT58-CTIAD  PICTURE  9(8).                    CI0273
            11            QT58-CTCUS  PICTURE  999.                     CI0273
            11            QT58-GR98.                                    CI0273
            12            QT58-GRID.                                    CI0273
            13            QT58-GRIDC  PICTURE  9(3).                    CI0273
            13            QT58-GRIDN.                                   CI0273
            14            QT58-GRIDNP PICTURE  99.                      CI0273
            14            QT58-GRIDND PICTURE  9(8).                    CI0273
            11            QT58-CQACT  PICTURE  999.                     CI0273
            11            QT58-CTCCI  PICTURE  X.                       CI0273
            11            QT58-CIRAS  PICTURE  999.                     CI0273
            11            QT58-CIRAT  PICTURE  999.                     CI0273
            11            QT58-IACVD  PICTURE  X.                       CI0273
            11            QT58-FILLER PICTURE  X(4).                    CI0273
            11            QT58-PRCODA PICTURE  X(5).                    CI0273
            11            QT58-PRCMN  PICTURE  X(20).                   CI0273
            11            QT58-MRPLN  PICTURE  X(30).                   CI0273
            11            QT58-CPRDG  PICTURE  9(2).                    CI0273
            11            QT58-CPRDA1 PICTURE  9(3).                    CI0273
            11            QT58-PRSCD  PICTURE  X(9).                    CI0273
            11            QT58-MSP03  PICTURE  X(3).                    CI0273
            11            QT58-CGRLI  PICTURE  X.                       CI0273
            11            QT58-ITERM  PICTURE  X(1).                    CI0273
            11            QT58-IVARP  PICTURE  X.                       CI0273
            11            QT58-DVALU  PICTURE  9(8).                    CI0273
            11            QT58-AACTV  PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ACCTVC PICTURE  X(20).                   CI0273
            11            QT58-ITXTI  PICTURE  X.                       CI0273
            11            QT58-ASANP  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ACINV  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-CELBL  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-NMESS2 PICTURE  S9(6)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-FILLER PICTURE  X(1).                    CI0273
            11            QT58-PRCLN  PICTURE  X(60).                   CI0273
            11            QT58-GECKD  PICTURE  9.                       CI0273
            11            QT58-MPLNA  PICTURE  X(19).                   CI0273
            11            QT58-CQACTL PICTURE  X(45).                   CI0273
            11            QT58-CRQPA  PICTURE  9(3).                    CI0273
            11            QT58-IVANT  PICTURE  X(1).                    CI0273
            11            QT58-IDBRP  PICTURE  X(1).                    CI0273
            11            QT58-IANPY  PICTURE  X.                       CI0273
            11            QT58-IVARP1 PICTURE  X.                       CI0273
            11            QT58-FILLER PICTURE  X(27).                   CI0273
            11            QT58-NSEQ2A PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-NSEQ2P PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-MRPSN  PICTURE  X(12).                   CI0273
            11            QT58-GEHCD  PICTURE  9(3)                     CI0273
                          OCCURS       002     TIMES.                   CI0273
            11            QT58-GEHCSU PICTURE  9(5)                     CI0273
                          OCCURS       002     TIMES.                   CI0273
            11            QT58-PRCSN  PICTURE  X(9).                    CI0273
            11            QT58-CGRMF  PICTURE  X.                       CI0273
            11            QT58-IGFEX  PICTURE  X.                       CI0273
            11            QT58-CLIDP  PICTURE  X(23).                   CI0273
            11            QT58-CLCTRC PICTURE  9(3).                    CI0273
            11            QT58-ADINP  PICTURE  X(20).                   CI0273
            11            QT58-CLCTRA PICTURE  9(3).                    CI0273
            11            QT58-GRPLC  PICTURE  99.                      CI0273
            11            QT58-CIDRP  PICTURE  99.                      CI0273
            11            QT58-FILLER PICTURE  X(01).                   CI0273
            11            QT58-AVMTOT PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-AVCSH  PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-AMARC  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-AVLMX  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-AVLMN  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-INDRS  PICTURE  X.                       CI0273
            11            QT58-MPRN4  PICTURE  X(35).                   CI0273
            11            QT58-FILLER PICTURE  X(1).                    CI0273
            11            QT58-ACVALM PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-INDRSA PICTURE  X(2).                    CI0273
            11            QT58-DXTMSA PICTURE  X(26).                   CI0273
            11            QT58-NMESS6 PICTURE  S9(6)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-NMESS7 PICTURE  S9(6)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-IBIDSA PICTURE  X.                       CI0273
            11            QT58-IBIDSB PICTURE  X.                       CI0273
            11            QT58-INSPOS PICTURE  X.                       CI0273
            11            QT58-INSPOD PICTURE  X.                       CI0273
            11            QT58-ACBALX PICTURE  X(20).                   CI0273
            11            QT58-AINVMX PICTURE  X(20).                   CI0273
            11            QT58-AMARCX PICTURE  X(20).                   CI0273
            11            QT58-AVMTOX PICTURE  X(20).                   CI0273
            11            QT58-IMNPR  PICTURE  X.                       CI0273
            11            QT58-ISSPL  PICTURE  X.                       CI0273
            11            QT58-AVMTOI PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-AVCSHI PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-APOSC  PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-AVLMXI PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-AVLMN1 PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-AVLMN2 PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-FILLER PICTURE  X(05).                   CI0273
            10            QT58-QT5A.                                    CI0273
            11            QT58-CLID   PICTURE  X(23).                   CI0273
            11            QT58-GECKD1 PICTURE  9.                       CI0273
            11            QT58-MCLNM  PICTURE  X(40).                   CI0273
            11            QT58-MCLNM2 PICTURE  X(40).                   CI0273
            11            QT58-CLTYP  PICTURE  X.                       CI0273
            11            QT58-CLDOB  PICTURE  9(8).                    CI0273
            11            QT58-CLDTH  PICTURE  X.                       CI0273
            11            QT58-CLTIN  PICTURE  9(12).                   CI0273
            11            QT58-CLTINC PICTURE  9.                       CI0273
            11            QT58-GESAD1 PICTURE  X(30).                   CI0273
            11            QT58-GESAD2 PICTURE  X(30).                   CI0273
            11            QT58-GESAD3 PICTURE  X(30).                   CI0273
            11            QT58-GECIT  PICTURE  X(25).                   CI0273
            11            QT58-GECTRY PICTURE  X(20).                   CI0273
            11            QT58-GEPCD  PICTURE  X(12).                   CI0273
            11            QT58-GEST   PICTURE  X(8).                    CI0273
            11            QT58-GEADS  PICTURE  9.                       CI0273
            11            QT58-GECSD  PICTURE  9(8).                    CI0273
            11            QT58-QCLAGE PICTURE  9(3)V9                   CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-FILLER PICTURE  X(06).                   CI0273
            10            QT58-QT5T.                                    CI0273
            11            QT58-ATFRA  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-AGOFD  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-APRMX  PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-APRMN  PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-IOWNC  PICTURE  X.                       CI0273
            11            QT58-COWNF  PICTURE  X(30).                   CI0273
            11            QT58-CTYPE  PICTURE  X.                       CI0273
            11            QT58-CIRAC  PICTURE  X(5).                    CI0273
            11            QT58-CTXMT  PICTURE  9(2).                    CI0273
            11            QT58-AMIND  PICTURE  S9(7)V99.                CI0273
            11            QT58-AMAXAR PICTURE  S9(7)V99.                CI0273
            11            QT58-QSHOWQ PICTURE  S9(9)V999                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-QSHOW0 PICTURE  S9(10)V999               CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-PPOT1  PICTURE  S9(3)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-PACT1  PICTURE  S999V999                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-IPRTA  PICTURE  X.                       CI0273
            11            QT58-FILLER PICTURE  X.                       CI0273
            11            QT58-CLCUS  PICTURE  99.                      CI0273
            11            QT58-CCDSCW PICTURE  9(2).                    CI0273
            11            QT58-CCACT  PICTURE  99.                      CI0273
            11            QT58-CIRAG.                                   CI0273
            12            QT58-CIRAP  PICTURE  XX                       CI0273
                          OCCURS       010     TIMES.                   CI0273
            11            QT58-ITERF  PICTURE  X.                       CI0273
            11            QT58-IACFPD PICTURE  X(1).                    CI0273
            11            QT58-AFEET  PICTURE  S9(5)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ATERF  PICTURE  S9(5)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-CLIDNB PICTURE  9(8).                    CI0273
            11            QT58-ALOAD  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ASURR  PICTURE  S9(07)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ASHIS  PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-AMNBL  PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-APNAC  PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ANGOF  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-CPLTYP PICTURE  X(14).                   CI0273
            10            QT58-QT5N.                                    CI0273
            11            QT58-IARRAN PICTURE  X.                       CI0273
            11            QT58-GESTD1 PICTURE  9(8).                    CI0273
            11            QT58-GEEND1 PICTURE  S9(8)                    CI0273
                          BINARY.                                       CI0273
            11            QT58-GESTD  PICTURE  9(8).                    CI0273
            11            QT58-GEEND  PICTURE  9(8).                    CI0273
            11            QT58-NSQ4B2 PICTURE  9(8)                     CI0273
                          BINARY.                                       CI0273
            11            QT58-CDEST  PICTURE  99.                      CI0273
            11            QT58-DEFFT  PICTURE  9(8).                    CI0273
            11            QT58-CPMTF  PICTURE  99.                      CI0273
            11            QT58-CPMTG  PICTURE  99.                      CI0273
            11            QT58-MPMTFL PICTURE  X(24).                   CI0273
            11            QT58-MPMTFE PICTURE  X(24).                   CI0273
            11            QT58-DLAUP  PICTURE  9(8).                    CI0273
            11            QT58-NSEQ4B PICTURE  9(8)                     CI0273
                          BINARY.                                       CI0273
            11            QT58-QSACTF PICTURE  9(3).                    CI0273
            11            QT58-QSACTT PICTURE  9(3).                    CI0273
            11            QT58-CCONF  PICTURE  X(25).                   CI0273
            11            QT58-DCONF  PICTURE  9(8).                    CI0273
            11            QT58-DTIMT  PICTURE  X(8).                    CI0273
            11            QT58-CACTS  PICTURE  X.                       CI0273
            11            QT58-ADBRQ  PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-DNPMT  PICTURE  9(8).                    CI0273
            11            QT58-NAPDS  PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-CDEST1 PICTURE  99.                      CI0273
            11            QT58-CLANR1 PICTURE  X(23).                   CI0273
            11            QT58-FILLER PICTURE  X(01).                   CI0273
            10            QT58-FILLER PICTURE  X(600).                  CI0273
            10            QT58-QT5C                                     CI0273
                          REDEFINES            QT58-FILLER.             CI0273
            11            QT58-CESLD  PICTURE  9(8).                    CI0273
            11            QT58-PCIRB5 PICTURE  S9(3)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-PANYDD PICTURE  S9(3)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-CEIT   PICTURE  9(3).                    CI0273
            11            QT58-PPART  PICTURE  9(3)V99                  CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-DTRME  PICTURE  9(8).                    CI0273
            11            QT58-CEIRND PICTURE  9(8).                    CI0273
            11            QT58-DANNIA PICTURE  9(8).                    CI0273
            11            QT58-AAPAA  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-CELBDT PICTURE  9(8).                    CI0273
            11            QT58-CEIIS  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-DTRME1 PICTURE  9(8).                    CI0273
            11            QT58-GMKTS.                                   CI0273
            12            QT58-DTRME2 PICTURE  9(8)                     CI0273
                          OCCURS       005     TIMES.                   CI0273
            12            QT58-DTRME3 PICTURE  9(8)                     CI0273
                          OCCURS       005     TIMES.                   CI0273
            11            QT58-ALINT  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-CEHCD  PICTURE  9(3)                     CI0273
                          OCCURS       006     TIMES.                   CI0273
            11            QT58-CEFOTR PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-DGPED  PICTURE  9(8).                    CI0273
            11            QT58-DIPED  PICTURE  9(8).                    CI0273
            11            QT58-FILLER PICTURE  X(409).                  CI0273
            10            QT58-QT5F                                     CI0273
                          REDEFINES            QT58-FILLER.             CI0273
            11            QT58-DLAUP2 PICTURE  9(8).                    CI0273
            11            QT58-QSHOW  PICTURE  S9(10)V999               CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-AFAVP  PICTURE  S9(4)V9(3)               CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-QSHIS  PICTURE  S9(10)V999               CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-QSHNM  PICTURE  S9(10)V999.              CI0273
            11            QT58-QSHOM  PICTURE  S9(10)V999               CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ADDAC  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-QSHES  PICTURE  S9(10)V999               CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-NDCUS  PICTURE  X(9).                    CI0273
            11            QT58-CSTKR5 PICTURE  X(5).                    CI0273
            11            QT58-NACID  PICTURE  S9(11)                   CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-AGOFD2 PICTURE  S9(9)V99.                CI0273
            11            QT58-TCBAT  PICTURE  X(21).                   CI0273
            11            QT58-FILLER PICTURE  X(490).                  CI0273
            10            QT58-QT5L                                     CI0273
                          REDEFINES            QT58-FILLER.             CI0273
            11            QT58-ALDBEN PICTURE  S9(09)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-APREL  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ALMODE PICTURE  99.                      CI0273
            11            QT58-ITMEC  PICTURE  X(1).                    CI0273
            11            QT58-ITAMR  PICTURE  X(1).                    CI0273
            11            QT58-MPMTF  PICTURE  X(14).                   CI0273
            11            QT58-TPLNL  PICTURE  X(30).                   CI0273
            11            QT58-ASBENA PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ASBENB PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ASBENC PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ASBENE PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ASBENF PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-GESTNS PICTURE  X(2).                    CI0273
            11            QT58-CTWHPB PICTURE  9(3)V999.                CI0273
            11            QT58-CTWHCB PICTURE  X.                       CI0273
            11            QT58-AMVA1  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ASPAM  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ACTCH  PICTURE  S9(07)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-AMXLN  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ALFGH  PICTURE  999.                     CI0273
            11            QT58-ALPLNI PICTURE  9.                       CI0273
            11            QT58-ATSA8  PICTURE  S9(07)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-CVALB  PICTURE  X(3).                    CI0273
            11            QT58-ASURRN PICTURE  S9(07)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ASURRW PICTURE  S9(07)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ATLTB  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-AEARN0 PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ATFPI  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-AEARN1 PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ISELO  PICTURE  X.                       CI0273
            11            QT58-CCLAC  PICTURE  X.                       CI0273
            11            QT58-ALINNO PICTURE  99.                      CI0273
            11            QT58-ALPLNJ PICTURE  9.                       CI0273
            11            QT58-COLPL  PICTURE  9(05).                   CI0273
            11            QT58-ALPLDT PICTURE  9(8).                    CI0273
            11            QT58-ANFMC  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-CPNOP  PICTURE  X(2).                    CI0273
            11            QT58-CVSTC  PICTURE  X(4).                    CI0273
            11            QT58-CGMBR  PICTURE  X.                       CI0273
            11            QT58-DWSDT  PICTURE  9(8).                    CI0273
            11            QT58-IRDPH  PICTURE  X.                       CI0273
            11            QT58-DWAIT  PICTURE  9(8).                    CI0273
            11            QT58-IAPGP  PICTURE  X.                       CI0273
            11            QT58-CASTA  PICTURE  X.                       CI0273
            11            QT58-CSSUP2 PICTURE  X.                       CI0273
            11            QT58-CVOMC1 PICTURE  X(1).                    CI0273
            11            QT58-APGBP  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ALDDUE PICTURE  9(08).                   CI0273
            11            QT58-APYMT  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ALSURR PICTURE  S9(09)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-CESTP  PICTURE  X(03).                   CI0273
            11            QT58-FILLER PICTURE  X(356).                  CI0273
            10            QT58-QT5O                                     CI0273
                          REDEFINES            QT58-FILLER.             CI0273
            11            QT58-NBACT  PICTURE  S9(11)                   CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-CTIAC  PICTURE  S9(3)                    CI0273
                          BINARY.                                       CI0273
            11            QT58-CASTT  PICTURE  S99                      CI0273
                          BINARY.                                       CI0273
            11            QT58-CATMI  PICTURE  S9                       CI0273
                          BINARY.                                       CI0273
            11            QT58-IATMR  PICTURE  X(3).                    CI0273
            11            QT58-IBIPI  PICTURE  X.                       CI0273
            11            QT58-CBPST  PICTURE  S99                      CI0273
                          BINARY.                                       CI0273
            11            QT58-TBPST  PICTURE  X(16).                   CI0273
            11            QT58-CODPI  PICTURE  X.                       CI0273
            11            QT58-TODPS  PICTURE  X(9).                    CI0273
            11            QT58-FILLER PICTURE  X(448).                  CI0273
            11            QT58-IBPSD  PICTURE  X.                       CI0273
            11            QT58-FILLER PICTURE  X(107).                  CI0273
            11            QT58-QT5E                                     CI0273
                          REDEFINES            QT58-FILLER.             CI0273
            12            QT58-MPRN4X PICTURE  X(100).                  CI0273
            12            QT58-CCMSH  PICTURE  X(2).                    CI0273
            12            QT58-CPRCS  PICTURE  X(04).                   CI0273
            12            QT58-CURST  PICTURE  X.                       CI0273
            10            QT58-QT5M                                     CI0273
                          REDEFINES            QT58-FILLER.             CI0273
            11            QT58-NAPCN1 PICTURE  X(24).                   CI0273
            11            QT58-FILLER PICTURE  X(576).                  CI0273
            10            QT58-QT5B                                     CI0273
                          REDEFINES            QT58-FILLER.             CI0273
            11            QT58-NAPCN2 PICTURE  X(24).                   CI0273
            11            QT58-CTIDAL PICTURE  X(40).                   CI0273
            11            QT58-NPHNS  PICTURE  X(14).                   CI0273
            11            QT58-FILLER PICTURE  X(522).                  CI0273
            10            QT58-QT5P                                     CI0273
                          REDEFINES            QT58-FILLER.             CI0273
            11            QT58-CFPPT  PICTURE  9(3).                    CI0273
            11            QT58-TTYPP  PICTURE  X(40).                   CI0273
            11            QT58-CPPST  PICTURE  9(3).                    CI0273
            11            QT58-TPPST  PICTURE  X(15).                   CI0273
            11            QT58-APFEEQ PICTURE  S9(7)V99.                CI0273
            11            QT58-APFEEC PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-APFEEP PICTURE  S9(7)V99.                CI0273
            11            QT58-ISVCA  PICTURE  X.                       CI0273
            11            QT58-NSBVS  PICTURE  X(5).                    CI0273
            11            QT58-ICKRV  PICTURE  X.                       CI0273
            11            QT58-PDAMT  PICTURE  S9(03).                  CI0273
            11            QT58-PSTAX  PICTURE  S9(03)V999.              CI0273
            11            QT58-DPCAL  PICTURE  9(8).                    CI0273
            11            QT58-NADVF  PICTURE  X(08).                   CI0273
            11            QT58-DAGUP  PICTURE  9(8).                    CI0273
            11            QT58-AANFEA PICTURE  9(5)V99                  CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-CLIDN7 PICTURE  9(8)                     CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-ARANV  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            QT58-DRANV  PICTURE  9(8).                    CI0273
            11            QT58-FILLER PICTURE  X(454).                  CI0273
            10            QT58-QT50                                     CI0273
                          REDEFINES            QT58-FILLER.             CI0273
            11            QT58-NANCA  PICTURE  X(30).                   CI0273
            11            QT58-MANCN  PICTURE  X(100).                  CI0273
            11            QT58-AINPTX PICTURE  X(20).                   CI0273
            11            QT58-CTID01 PICTURE  X(27).                   CI0273
            11            QT58-NANCA1 PICTURE  X(04).                   CI0273
            11            QT58-IIVAR  PICTURE  X(1).                    CI0273
            11            QT58-FILLER PICTURE  X(418).                  CI0273
            10            QT58-QT5R                                     CI0273
                          REDEFINES            QT58-FILLER.             CI0273
            11            QT58-NACTJ  PICTURE  X(04).                   CI0273
            11            QT58-NACNO6 PICTURE  X(11).                   CI0273
            11            QT58-FILLER PICTURE  X(585).                  CI0273
            10            QT58-AMAXA  PICTURE  S9(7)V99.                CI0273
            10            QT58-ISAOR  PICTURE  X.                       CI0273
            10            QT58-ISACH  PICTURE  X.                       CI0273
            10            QT58-CERRBA PICTURE  X(02).                   CI0273
            10            QT58-CERRBH PICTURE  X(02).                   CI0273
            10            QT58-IWITHH PICTURE  X.                       CI0273
            10            QT58-CTID20 PICTURE  X(27).                   CI0273
            10            QT58-GECKD3 PICTURE  9.                       CI0273
            10            QT58-DANFC  PICTURE  X(10).                   CI0273
            10            QT58-DAFCN  PICTURE  X(10).                   CI0273
            10            QT58-ISMTA  PICTURE  X.                       CI0273
            10            QT58-CERRBT PICTURE  X(02).                   CI0273
            10            QT58-NPLNI  PICTURE  X(10).                   CI0273
            10            QT58-FILLER PICTURE  X(023).                  CI0273
      *---------------  SQL INCLUDE STATEMENTS ---------------------    ADB221
           EXEC SQL     INCLUDE SQLCA             END-EXEC.             ADB221
      *                                                                 ADB221
      *--------------  ERROR HANDLING VARIABLES --------------------    ADB221
       01               7-DB2-FUNCT      PIC X(35) VALUE SPACES.        ADB221
       01               7-SQLR.                                         ADB221
         05             7-SQLR-TEXT-LEN  PIC S9(9) COMP VALUE +80.      ADB221
         05             7-SQLR-MESSAGE.                                 ADB221
           10           7-SQLR-LEN       PIC S9(4) COMP VALUE +960.     ADB221
           10           7-SQLR-TEXT      PIC X(80) OCCURS 12 TIMES.     ADB221
       01               7-DB2-ABEND      PIC 9(4)  VALUE ZERO.          ADB221
       01               7-DB2-ABENDX     REDEFINES 7-DB2-ABEND.         ADB221
           05           7-DB2-FIRST      PIC X.                         ADB221
           05           FILLER           PIC X(3).                      ADB221
       01               7-TEST-SQLCODE   PIC S9(9) COMP.                ADB221
           88           ROW-NOT-FOUND              VALUE +100.          ADB221
           88           DUPLICATE-KEY              VALUE -803.          ADB221
           88           MULTIPLE-ROWS-FOUND        VALUE -811.          ADB221
           88           RESOURCE-NOT-AVAILABLE     VALUE -904.          ADB221
           88           RESOURCE-IN-USE            VALUE -913.          ADB221
       01               7-Q913-COUNT     PIC S9(3) COMP-3               ADB221
                                                   VALUE ZERO.          ADB221
       01               7-Q913-LNGTH     PIC S9(4) COMP                 ADB221
                                                   VALUE +66.           ADB221
      *!WI pl=SQ430                                                     ADB221
       01               7-Q913-TMSGV5                                   ADB221
                        PICTURE X(66)                                   CI0273
                           VALUE 'DB2 RESOURCE IN USE. TRY AGAIN '.     ADB221
       01               7-MAXM-RETRY     PIC S9(3) COMP-3               ADB221
                                                   VALUE +001.          ADB221
      *                                                                 AM0265
      ******************************************************************AM0265
      ** WORKING STORAGE FOR CI0265                                    *AM0265
      ******************************************************************AM0265
      *                                                                 AM0265
      *!WF DSP=WA DSL=V1 SEL=65 FOR=I LEV=1                             AM0265
       01                 WA00.                                         CI0273
          05              WA00-SUITE.                                   CI0273
            15       FILLER         PICTURE  X(00100).                  CI0273
       01                 WA65  REDEFINES      WA00.                    CI0273
            10            WA65-MAPPN  PICTURE  X(10).                   CI0273
            10            WA65-CTID   PICTURE  X(27).                   CI0273
            10            WA65-DCACG  PICTURE  9(8).                    CI0273
            10            WA65-AACTV  PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            WA65-AGOFD  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            WA65-ANGOF  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            WA65-NACID  PICTURE  S9(11)                   CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            WA65-FILLER PICTURE  X(30).                   CI0273
      *                                                                 AM0265
      ******************************************************************AM0265
      ** PCB ADDRESS LIST FOR CI0265.  MODULE CI0265 WILL NEED         *AM0265
      ** PCB'S FOR:                                                     AM0265
      **             CONTRACT   DATABASE       (CT1P)                   AM0265
      **             CERT HIST  DATABASE       (CH1P)                   AM0265
      **             SHARK PROD DATABASE       (SCOP)                   AM0265
      ******************************************************************AM0265
       01   CI0265-WA-PCB-ADDR-LIST.                                    AM0265
            05  CI0265-WA-PCB-CT1P-PTR1       POINTER.                  AM0265
            05  CI0265-WA-PCB-CH1P-PTR1       POINTER.                  AM0265
            05  CI0265-WA-PCB-SCOP-PTR1       POINTER.                  AM0265
      *
      ******************************************************************
      *            MISCELLANEOUS WORKING STORAGE
      ******************************************************************
      **
      *!WI
       01  WS-DCACG9
                        PICTURE 9(8).                                   CI0273
      **
      ****************************************************************
      **            FLAGS USED IN THE PROGRAM.
      ****************************************************************
       01  WS-CLIENT-VALID-AGE       PIC X(1)       VALUE 'Y'.
           88 CLIENT-VALID-AGE                      VALUE 'Y'.
           88 CLIENT-INVALID-AGE                    VALUE 'N'.
       01  WS00-LIVING               PIC X(1)       VALUE 'N'.
      *!WI
       01  WS00-IAIND1
                        PICTURE X.                                      CI0273
      *!WI
       01  WS00-PRCOD
                        PICTURE 9(5).                                   CI0273
           88  VALID-BETA-PRCOD  VALUES 00001 00006 00010
                                        00011 00014 00015
                                        00016 00017.
      *
      *!WI
       01  WS00-DNACG
                        PICTURE 9(8).                                   CI0273
      *!WI
       01  WS00-DCACG
                        PICTURE 9(8).                                   CI0273
      *!WI
       01  WS00-CLTINC
                        PICTURE 9.                                      CI0273
      *
       01  WS00-CLTIN           PIC X(1)  VALUE 'N'.
      *ELEMENT FOR CALCULATING BUSINESS DAYS
       01  WS00-CESLD      PIC 9(8)   VALUE ZEROS.
      *TRANSFER AMOUNT FOR CERTS
       01  WS00-TRAN       PIC S9(11)V99   VALUE ZEROS.
      *MINIMUM TRANSFER AMOUNT FOR CERTS
       01  WS00-AMNBL.
           05  ONE-TIME-MIN   PIC S9(11)V99   VALUE 100.00.
      *CUMULATIVE AMOUNT IN ACTIVITY DATABASE
       01  WS00-CASH       PIC S9(11)V99 VALUE ZEROS.
                                                                        AM0222
      ******************************************************************AM0222
      *      PCB ADDRESS LIST FOR AM0222.  MODULE CI0222 WILL NEED     *AM0222
      *      PCB'S FOR:                                                *AM0222
      *                 CONTRACT DATABASE (CT1P)                       *AM0222
      *                 CLIENT            (CL1P)                       *AM0222
      ******************************************************************AM0222
                                                                        AM0222
       01  CI0222-PCB-ADDRESS-LIST.                                     AM0222
           05  CI0222-PCB-CT1P-PTR1        POINTER.                     AM0222
           05  CI0222-PCB-CL1P-PTR1        POINTER.                     AM0222
                                                                        AM0222
                                                                        AM0222
                                                                        AM0222
                                                                        AM0222
                                                                        AM0222
                                                                        AM0222
                                                                        AM0222
       01   DEBUT-WSS.                                                  CI0273
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0273
            05   IK     PICTURE X.                                      CI0273
       01  CONSTANTES-PAC.                                              CI0273
           05  FILLER  PICTURE X(87)   VALUE                            CI0273
                     '6015 CAT09/08/14CI0273ADMIN   14:35:10CI0273P AMERCI0273
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0273
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0273
           05  NUGNA   PICTURE X(5).                                    CI0273
           05  APPLI   PICTURE X(3).                                    CI0273
           05  DATGN   PICTURE X(8).                                    CI0273
           05  PROGR   PICTURE X(6).                                    CI0273
           05  CODUTI  PICTURE X(8).                                    CI0273
           05  TIMGN   PICTURE X(8).                                    CI0273
           05  PROGE   PICTURE X(8).                                    CI0273
           05  COBASE  PICTURE X(4).                                    CI0273
           05  DATGNC  PICTURE X(10).                                   CI0273
           05  RELEAS  PICTURE X(7).                                    CI0273
           05  DATGE   PICTURE X(10).                                   CI0273
           05  DATSQ   PICTURE X(10).                                   CI0273
       01  DATCE.                                                       CI0273
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0273
         05  DATOR.                                                     CI0273
           10  DATOA  PICTURE XX.                                       CI0273
           10  DATOM  PICTURE XX.                                       CI0273
           10  DATOJ  PICTURE XX.                                       CI0273
       01   VARIABLES-CONDITIONNELLES.                                  CI0273
            05                  FT      PICTURE X VALUE '0'.            CI0273
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0273
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0273
            05           IMS03L PICTURE S9(4) VALUE  ZERO.
            05           IMS03R PICTURE S9(4) VALUE  ZERO.
            05           IMS03M PICTURE S9(4) VALUE +0512.
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J40BDR PICTURE S9(4) VALUE  ZERO.
            05           J40JWR PICTURE S9(4) VALUE  ZERO.
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0273
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0273
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0273
            05       5-GC00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0273
       01               S-CL01-SSA.                                     CI0273
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0273
                                      VALUE 'CL01    '.                 CI0273
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0273
            10          S-CL01-CCOD   PICTURE X(5)                      CI0273
                                      VALUE '-----'.                    CI0273
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0273
       01            S-CLU01-SSA.                                       CI0273
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'CL01    '.                 CI0273
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0273
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(CL01K'.                   CI0273
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0273
            10       S-CLU01-CL01K.                                     CI0273
            11       S-CLU01-C199.                                      CI0273
            12       S-CLU01-CLID.                                      CI0273
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0273
            13       S-CLU01-CLIDN.                                     CI0273
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0273
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0273
            10  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01               S-CL03-SSA.                                     CI0273
            10         S1-CL03-SEGNAM PICTURE X(8)                      CI0273
                                      VALUE 'CL03    '.                 CI0273
            10         S1-CL03-CCOM   PICTURE X VALUE '*'.              CI0273
            10          S-CL03-CCOD   PICTURE X(5)                      CI0273
                                      VALUE '-----'.                    CI0273
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0273
       01            S-CLA03-SSA.                                       CI0273
            10      S1-CLA03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'CL03    '.                 CI0273
            10      S1-CLA03-CCOM   PICTURE X VALUE '*'.                CI0273
            10       S-CLA03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            10      S1-CLA03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(CLDOD'.                   CI0273
            10       S-CLA03-OPER  PICTURE XX VALUE ' ='.               CI0273
            10       S-CLA03-CLDOD    PICTURE  9(8).                    CI0273
            10  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01               S-CT01-SSA.                                     CI0273
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0273
                                      VALUE 'CT01    '.                 CI0273
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0273
            10          S-CT01-CCOD   PICTURE X(5)                      CI0273
                                      VALUE '-----'.                    CI0273
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0273
       01            S-CTU01-SSA.                                       CI0273
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'CT01    '.                 CI0273
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0273
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(CT01K'.                   CI0273
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0273
            10       S-CTU01-CT01K.                                     CI0273
            11       S-CTU01-C299.                                      CI0273
            12       S-CTU01-CTID.                                      CI0273
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0273
            13       S-CTU01-CTIDN.                                     CI0273
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0273
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0273
            10  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01               S-CT07-SSA.                                     CI0273
            10         S1-CT07-SEGNAM PICTURE X(8)                      CI0273
                                      VALUE 'CT07    '.                 CI0273
            10         S1-CT07-CCOM   PICTURE X VALUE '*'.              CI0273
            10          S-CT07-CCOD   PICTURE X(5)                      CI0273
                                      VALUE '-----'.                    CI0273
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0273
       01            S-CTU07-SSA.                                       CI0273
            10      S1-CTU07-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'CT07    '.                 CI0273
            10      S1-CTU07-CCOM   PICTURE X VALUE '*'.                CI0273
            10       S-CTU07-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            10      S1-CTU07-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(CT07K'.                   CI0273
            10       S-CTU07-OPER  PICTURE XX VALUE ' ='.               CI0273
            10       S-CTU07-CT07K.                                     CI0273
            11       S-CTU07-C199.                                      CI0273
            12       S-CTU07-CLID.                                      CI0273
            13       S-CTU07-CLIDO    PICTURE  9(3).                    CI0273
            13       S-CTU07-CLIDN.                                     CI0273
            14       S-CTU07-CLIDNP   PICTURE  X(12).                   CI0273
            14       S-CTU07-CLIDND   PICTURE  9(8).                    CI0273
            10  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01               S-CT09-SSA.                                     CI0273
            10         S1-CT09-SEGNAM PICTURE X(8)                      CI0273
                                      VALUE 'CT09    '.                 CI0273
            10         S1-CT09-CCOM   PICTURE X VALUE '*'.              CI0273
            10          S-CT09-CCOD   PICTURE X(5)                      CI0273
                                      VALUE '-----'.                    CI0273
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0273
       01            S-CTA09-SSA.                                       CI0273
            11      S1-CTA09-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'CT09    '.                 CI0273
            11      S1-CTA09-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-CTA09-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-CTA09-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(GERED'.                   CI0273
            11       S-CTA09-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-CTA09-GERED    PICTURE  9(8).                    CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-CTB09-SSA.                                       CI0273
            11      S1-CTB09-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'CT09    '.                 CI0273
            11      S1-CTB09-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-CTB09-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-CTB09-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(GECSQ'.                   CI0273
            11       S-CTB09-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-CTB09-GECSQ    PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-CTU09-SSA.                                       CI0273
            11      S1-CTU09-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'CT09    '.                 CI0273
            11      S1-CTU09-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-CTU09-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-CTU09-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(CT09K'.                   CI0273
            11       S-CTU09-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-CTU09-CT09K.                                     CI0273
            12       S-CTU09-CLCTRC   PICTURE  9(3).                    CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01               S-GC01-SSA.                                     CI0273
            10         S1-GC01-SEGNAM PICTURE X(8)                      CI0273
                                      VALUE 'GC01    '.                 CI0273
            10         S1-GC01-CCOM   PICTURE X VALUE '*'.              CI0273
            10          S-GC01-CCOD   PICTURE X(5)                      CI0273
                                      VALUE '-----'.                    CI0273
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0273
       01            S-GCU01-SSA.                                       CI0273
            10      S1-GCU01-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC01    '.                 CI0273
            10      S1-GCU01-CCOM   PICTURE X VALUE '*'.                CI0273
            10       S-GCU01-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            10      S1-GCU01-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(GC01K'.                   CI0273
            10       S-GCU01-OPER  PICTURE XX VALUE ' ='.               CI0273
            10       S-GCU01-GC01K.                                     CI0273
            11       S-GCU01-C299.                                      CI0273
            12       S-GCU01-CTID.                                      CI0273
            13       S-GCU01-CTIDA    PICTURE  9(3).                    CI0273
            13       S-GCU01-CTIDN.                                     CI0273
            14       S-GCU01-CTIDNP   PICTURE  X(13).                   CI0273
            14       S-GCU01-CTIDND   PICTURE  9(11).                   CI0273
            10  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01               S-GC03-SSA.                                     CI0273
            10         S1-GC03-SEGNAM PICTURE X(8)                      CI0273
                                      VALUE 'GC03    '.                 CI0273
            10         S1-GC03-CCOM   PICTURE X VALUE '*'.              CI0273
            10          S-GC03-CCOD   PICTURE X(5)                      CI0273
                                      VALUE '-----'.                    CI0273
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0273
       01            S-GCA03-SSA.                                       CI0273
            11      S1-GCA03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GCA03-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCA03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCA03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(CAATY'.                   CI0273
            11       S-GCA03-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCA03-CAATY    PICTURE  9(3).                    CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GCB03-SSA.                                       CI0273
            11      S1-GCB03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GCB03-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCB03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCB03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(CVSYS'.                   CI0273
            11       S-GCB03-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCB03-CVSYS    PICTURE  X(2).                    CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GCC03-SSA.                                       CI0273
            11      S1-GCC03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GCC03-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCC03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCC03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(CASTC'.                   CI0273
            11       S-GCC03-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCC03-CASTC    PICTURE  99.                      CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GCD03-SSA.                                       CI0273
            11      S1-GCD03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GCD03-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCD03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCD03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(CACTO'.                   CI0273
            11       S-GCD03-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCD03-CACTO    PICTURE  9(3).                    CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GCE03-SSA.                                       CI0273
            11      S1-GCE03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GCE03-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCE03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCE03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(IPULL'.                   CI0273
            11       S-GCE03-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCE03-IPULL    PICTURE  X.                       CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GCF03-SSA.                                       CI0273
            11      S1-GCF03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GCF03-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCF03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCF03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(DTRAC'.                   CI0273
            11       S-GCF03-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCF03-DTRAC    PICTURE  9(8).                    CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GCG03-SSA.                                       CI0273
            11      S1-GCG03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GCG03-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCG03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCG03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(CTRSO'.                   CI0273
            11       S-GCG03-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCG03-CTRSO    PICTURE  9(02).                   CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GCH03-SSA.                                       CI0273
            11      S1-GCH03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GCH03-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCH03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCH03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(NTRCE'.                   CI0273
            11       S-GCH03-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCH03-NTRCE    PICTURE  9(06).                   CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GCI03-SSA.                                       CI0273
            11      S1-GCI03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GCI03-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCI03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCI03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(ITRAN'.                   CI0273
            11       S-GCI03-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCI03-ITRAN    PICTURE  X.                       CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GCJ03-SSA.                                       CI0273
            11      S1-GCJ03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GCJ03-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCJ03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCJ03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(DEFFT'.                   CI0273
            11       S-GCJ03-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCJ03-DEFFT    PICTURE  9(8).                    CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GCK03-SSA.                                       CI0273
            11      S1-GCK03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GCK03-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCK03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCK03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(CPMTCA'.                  CI0273
            11       S-GCK03-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCK03-CPMTCA   PICTURE  XXX.                     CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GCL03-SSA.                                       CI0273
            11      S1-GCL03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GCL03-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCL03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCL03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(ACASH'.                   CI0273
            11       S-GCL03-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCL03-ACASH    PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GCN03-SSA.                                       CI0273
            11      S1-GCN03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GCN03-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCN03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCN03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(CRREV'.                   CI0273
            11       S-GCN03-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCN03-CRREV    PICTURE  X(3).                    CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GCO03-SSA.                                       CI0273
            11      S1-GCO03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GCO03-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCO03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCO03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(CSYST'.                   CI0273
            11       S-GCO03-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCO03-CSYST    PICTURE  99.                      CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GCU03-SSA.                                       CI0273
            11      S1-GCU03-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GCU03-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCU03-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCU03-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(GC03K'.                   CI0273
            11       S-GCU03-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCU03-GC03K.                                     CI0273
            12       S-GCU03-DCACG9   PICTURE  9(8).                    CI0273
            12       S-GCU03-NAASQ    PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GC103-SSA.                                       CI0273
            12      S1-GC103-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            12      S1-GC103-CCOM   PICTURE X VALUE '*'.                CI0273
            12       S-GC103-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            12      S1-GC103-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(XDCACG9'.                 CI0273
            12       S-GC103-OPER  PICTURE XX VALUE ' ='.               CI0273
            12       S-GC103-DCACG9   PICTURE  9(8).                    CI0273
            12  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GC203-SSA.                                       CI0273
            11      S1-GC203-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GC203-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GC203-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GC203-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(XGEAUN'.                  CI0273
            11       S-GC203-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GC203-GEAUN    PICTURE  9(5).                    CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GC303-SSA.                                       CI0273
            11      S1-GC303-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GC303-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GC303-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GC303-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(XGEOPD2'.                 CI0273
            11       S-GC303-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GC303-GEOPD2   PICTURE  X(8).                    CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GC403-SSA.                                       CI0273
            11      S1-GC403-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            11      S1-GC403-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GC403-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GC403-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(XNBTCH'.                  CI0273
            11       S-GC403-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GC403-NBTCH    PICTURE  9(4).                    CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GC803-SSA.                                       CI0273
            12      S1-GC803-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC03    '.                 CI0273
            12      S1-GC803-CCOM   PICTURE X VALUE '*'.                CI0273
            12       S-GC803-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            12      S1-GC803-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(XNAASQ'.                  CI0273
            12       S-GC803-OPER  PICTURE XX VALUE ' ='.               CI0273
            12       S-GC803-NAASQ    PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            12  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01               S-GC06-SSA.                                     CI0273
            10         S1-GC06-SEGNAM PICTURE X(8)                      CI0273
                                      VALUE 'GC06    '.                 CI0273
            10         S1-GC06-CCOM   PICTURE X VALUE '*'.              CI0273
            10          S-GC06-CCOD   PICTURE X(5)                      CI0273
                                      VALUE '-----'.                    CI0273
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0273
       01            S-GCF06-SSA.                                       CI0273
            11      S1-GCF06-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC06    '.                 CI0273
            11      S1-GCF06-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCF06-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCF06-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(PRCOD1'.                  CI0273
            11       S-GCF06-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCF06-PRCOD1   PICTURE  9(5).                    CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01            S-GCU06-SSA.                                       CI0273
            11      S1-GCU06-SEGNAM PICTURE X(8)                        CI0273
                                      VALUE 'GC06    '.                 CI0273
            11      S1-GCU06-CCOM   PICTURE X VALUE '*'.                CI0273
            11       S-GCU06-CCOD   PICTURE X(5)                        CI0273
                                      VALUE '-----'.                    CI0273
            11      S1-GCU06-FLDNAM PICTURE X(9)                        CI0273
                                      VALUE '(GC06K'.                   CI0273
            11       S-GCU06-OPER  PICTURE XX VALUE ' ='.               CI0273
            11       S-GCU06-GC06K.                                     CI0273
            12       S-GCU06-NPISQ    PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11  FILLER   PICTURE X    VALUE ')'.                        CI0273
       01   ZONES-UTILISATEUR PICTURE X.                                CI0273
      ******************************************************************AM0085
      ** LINKAGE SEGMENT FOR CI0085(OWNERSHIP INFORMATION)             *AM0085
      ******************************************************************AM0085
      **                                                                AM0085
      *!WF DSP=K2 DSL=K1 SEL=1F FOR=I LEV=1                             AM0085
       01                 K200.                                         CI0273
          05              K200-SUITE.                                   CI0273
            15       FILLER         PICTURE  X(00266).                  CI0273
       01                 K21F  REDEFINES      K200.                    CI0273
            10            K21F-INPUT.                                   CI0273
            11            K21F-MAPPN  PICTURE  X(10).                   CI0273
            11            K21F-PROGR  PICTURE  X(06).                   CI0273
            11            K21F-ADDRLN.                                  CI0273
            12            K21F-GESAD1 PICTURE  X(30).                   CI0273
            12            K21F-GESAD2 PICTURE  X(30).                   CI0273
            12            K21F-GESAD3 PICTURE  X(30).                   CI0273
            11            K21F-FILLER PICTURE  X(100).                  CI0273
            10            K21F-OUTPUT.                                  CI0273
            11            K21F-IOWNC  PICTURE  X                        CI0273
                          OCCURS       060     TIMES.                   CI0273
      **                                                                AM0085
      *!WF DSP=AB DSL=DU SEL=04 FOR=I DES=1 LEV=1                       AM0085
       01                 AB04.                                         CI0273
            10            AB04-C299.                                    CI0273
            11            AB04-CTID.                                    CI0273
            12            AB04-CTIDA  PICTURE  9(3).                    CI0273
            12            AB04-CTIDN.                                   CI0273
            13            AB04-CTIDNP PICTURE  X(13).                   CI0273
            13            AB04-CTIDND PICTURE  9(11).                   CI0273
            10            AB04-IPOCH  PICTURE  X.                       CI0273
            10            AB04-FILLER PICTURE  X(099).                  CI0273
            10            AB04-CTTLN1 PICTURE  X(30).                   CI0273
            10            AB04-CTTLN2 PICTURE  X(30).                   CI0273
            10            AB04-CTTLN3 PICTURE  X(30).                   CI0273
            10            AB04-CTTBO1 PICTURE  X(45).                   CI0273
            10            AB04-CTTBO2 PICTURE  X(45).                   CI0273
            10            AB04-CTOWN  PICTURE  9(3).                    CI0273
            10            AB04-IUGMA  PICTURE  X.                       CI0273
            10            AB04-FILLER PICTURE  X(096).                  CI0273
      **                                                                AM0085
      ******************************************************************
      *            LINKAGE FOR CALLED MODULE CI0222
      ******************************************************************
      *!WF DSP=X1 DSL=VW SEL=22 FOR=I DES=1 LEV=1 PLT=X1
       01                 X122.                                         CI0273
            10            X122-CTID   PICTURE  X(27).                   CI0273
            10            X122-IPOCH  PICTURE  X.                       CI0273
            10            X122-FILLER                                   CI0273
                          OCCURS       020     TIMES.                   CI0273
            11            X122-CLID   PICTURE  X(23).                   CI0273
            11            X122-GECKD  PICTURE  9.                       CI0273
            11            X122-CLTYP  PICTURE  X.                       CI0273
            11            X122-CLCTRC PICTURE  9(3).                    CI0273
            11            X122-MCLNM  PICTURE  X(40).                   CI0273
            11            X122-CLTIN  PICTURE  9(12).                   CI0273
            11            X122-CLTINC PICTURE  9.                       CI0273
            11            X122-CLDOB  PICTURE  9(8).                    CI0273
            11            X122-CLDTH  PICTURE  X.                       CI0273
            11            X122-QCLAGE PICTURE  9(3)V9                   CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            X122-FILLER PICTURE  X(47).                   CI0273
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
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR ACAP                                           ADU015
            05 PCB-ACAP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CH1P                                           ADU015
            05 PCB-CH1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CCRP                                           ADU015
            05 PCB-CCRP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CPRP                                           ADU015
            05 PCB-CPRP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CBTP                                           ADU015
            05 PCB-CBTP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CA1P                                           ADU015
            05 PCB-CA1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR SCOP                                           ADU015
            05 PCB-SCOP-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0273
          05              PA00-SUITE.                                   CI0273
            15       FILLER         PICTURE  X(00106).                  CI0273
       01                 PA06  REDEFINES      PA00.                    CI0273
            10            PA06-XDBPCB.                                  CI0273
            11            PA06-XDBDNM PICTURE  X(08).                   CI0273
            11            PA06-XSEGLV PICTURE  X(02).                   CI0273
            11            PA06-XRC    PICTURE  X(02).                   CI0273
            11            PA06-XPROPT PICTURE  X(04).                   CI0273
            11            PA06-FILLER PICTURE  S9(5)                    CI0273
                          BINARY.                                       CI0273
            11            PA06-XSEGNM PICTURE  X(08).                   CI0273
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0273
                          BINARY.                                       CI0273
            11            PA06-XSEGNB PICTURE  9(05)                    CI0273
                          BINARY.                                       CI0273
            11            PA06-XCOKEY PICTURE  X(70).                   CI0273
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0273
          05              PC00-SUITE.                                   CI0273
            15       FILLER         PICTURE  X(00106).                  CI0273
       01                 PC06  REDEFINES      PC00.                    CI0273
            10            PC06-XDBPCB.                                  CI0273
            11            PC06-XDBDNM PICTURE  X(08).                   CI0273
            11            PC06-XSEGLV PICTURE  X(02).                   CI0273
            11            PC06-XRC    PICTURE  X(02).                   CI0273
            11            PC06-XPROPT PICTURE  X(04).                   CI0273
            11            PC06-FILLER PICTURE  S9(5)                    CI0273
                          BINARY.                                       CI0273
            11            PC06-XSEGNM PICTURE  X(08).                   CI0273
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0273
                          BINARY.                                       CI0273
            11            PC06-XSEGNB PICTURE  9(05)                    CI0273
                          BINARY.                                       CI0273
            11            PC06-XCOKEY PICTURE  X(70).                   CI0273
      *** PCB MASK FOR ACAP                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0273
          05              PB00-SUITE.                                   CI0273
            15       FILLER         PICTURE  X(00106).                  CI0273
       01                 PB06  REDEFINES      PB00.                    CI0273
            10            PB06-XDBPCB.                                  CI0273
            11            PB06-XDBDNM PICTURE  X(08).                   CI0273
            11            PB06-XSEGLV PICTURE  X(02).                   CI0273
            11            PB06-XRC    PICTURE  X(02).                   CI0273
            11            PB06-XPROPT PICTURE  X(04).                   CI0273
            11            PB06-FILLER PICTURE  S9(5)                    CI0273
                          BINARY.                                       CI0273
            11            PB06-XSEGNM PICTURE  X(08).                   CI0273
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0273
                          BINARY.                                       CI0273
            11            PB06-XSEGNB PICTURE  9(05)                    CI0273
                          BINARY.                                       CI0273
            11            PB06-XCOKEY PICTURE  X(70).                   CI0273
      *** PCB MASK FOR CH1P                                             ADU015
      *!WF DSP=PF DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PF00.                                         CI0273
          05              PF00-SUITE.                                   CI0273
            15       FILLER         PICTURE  X(00106).                  CI0273
       01                 PF06  REDEFINES      PF00.                    CI0273
            10            PF06-XDBPCB.                                  CI0273
            11            PF06-XDBDNM PICTURE  X(08).                   CI0273
            11            PF06-XSEGLV PICTURE  X(02).                   CI0273
            11            PF06-XRC    PICTURE  X(02).                   CI0273
            11            PF06-XPROPT PICTURE  X(04).                   CI0273
            11            PF06-FILLER PICTURE  S9(5)                    CI0273
                          BINARY.                                       CI0273
            11            PF06-XSEGNM PICTURE  X(08).                   CI0273
            11            PF06-XKEYLN PICTURE  S9(05)                   CI0273
                          BINARY.                                       CI0273
            11            PF06-XSEGNB PICTURE  9(05)                    CI0273
                          BINARY.                                       CI0273
            11            PF06-XCOKEY PICTURE  X(70).                   CI0273
      *** PCB MASK FOR CCRP                                             ADU015
      *!WF DSP=PG DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PG00.                                         CI0273
          05              PG00-SUITE.                                   CI0273
            15       FILLER         PICTURE  X(00106).                  CI0273
       01                 PG06  REDEFINES      PG00.                    CI0273
            10            PG06-XDBPCB.                                  CI0273
            11            PG06-XDBDNM PICTURE  X(08).                   CI0273
            11            PG06-XSEGLV PICTURE  X(02).                   CI0273
            11            PG06-XRC    PICTURE  X(02).                   CI0273
            11            PG06-XPROPT PICTURE  X(04).                   CI0273
            11            PG06-FILLER PICTURE  S9(5)                    CI0273
                          BINARY.                                       CI0273
            11            PG06-XSEGNM PICTURE  X(08).                   CI0273
            11            PG06-XKEYLN PICTURE  S9(05)                   CI0273
                          BINARY.                                       CI0273
            11            PG06-XSEGNB PICTURE  9(05)                    CI0273
                          BINARY.                                       CI0273
            11            PG06-XCOKEY PICTURE  X(70).                   CI0273
      *** PCB MASK FOR CPRP                                             ADU015
      *!WF DSP=PH DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PH00.                                         CI0273
          05              PH00-SUITE.                                   CI0273
            15       FILLER         PICTURE  X(00106).                  CI0273
       01                 PH06  REDEFINES      PH00.                    CI0273
            10            PH06-XDBPCB.                                  CI0273
            11            PH06-XDBDNM PICTURE  X(08).                   CI0273
            11            PH06-XSEGLV PICTURE  X(02).                   CI0273
            11            PH06-XRC    PICTURE  X(02).                   CI0273
            11            PH06-XPROPT PICTURE  X(04).                   CI0273
            11            PH06-FILLER PICTURE  S9(5)                    CI0273
                          BINARY.                                       CI0273
            11            PH06-XSEGNM PICTURE  X(08).                   CI0273
            11            PH06-XKEYLN PICTURE  S9(05)                   CI0273
                          BINARY.                                       CI0273
            11            PH06-XSEGNB PICTURE  9(05)                    CI0273
                          BINARY.                                       CI0273
            11            PH06-XCOKEY PICTURE  X(70).                   CI0273
      *** PCB MASK FOR CBTP                                             ADU015
      *!WF DSP=PI DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PI00.                                         CI0273
          05              PI00-SUITE.                                   CI0273
            15       FILLER         PICTURE  X(00106).                  CI0273
       01                 PI06  REDEFINES      PI00.                    CI0273
            10            PI06-XDBPCB.                                  CI0273
            11            PI06-XDBDNM PICTURE  X(08).                   CI0273
            11            PI06-XSEGLV PICTURE  X(02).                   CI0273
            11            PI06-XRC    PICTURE  X(02).                   CI0273
            11            PI06-XPROPT PICTURE  X(04).                   CI0273
            11            PI06-FILLER PICTURE  S9(5)                    CI0273
                          BINARY.                                       CI0273
            11            PI06-XSEGNM PICTURE  X(08).                   CI0273
            11            PI06-XKEYLN PICTURE  S9(05)                   CI0273
                          BINARY.                                       CI0273
            11            PI06-XSEGNB PICTURE  9(05)                    CI0273
                          BINARY.                                       CI0273
            11            PI06-XCOKEY PICTURE  X(70).                   CI0273
      *** PCB MASK FOR CA1P                                             ADU015
      *!WF DSP=PJ DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PJ00.                                         CI0273
          05              PJ00-SUITE.                                   CI0273
            15       FILLER         PICTURE  X(00106).                  CI0273
       01                 PJ06  REDEFINES      PJ00.                    CI0273
            10            PJ06-XDBPCB.                                  CI0273
            11            PJ06-XDBDNM PICTURE  X(08).                   CI0273
            11            PJ06-XSEGLV PICTURE  X(02).                   CI0273
            11            PJ06-XRC    PICTURE  X(02).                   CI0273
            11            PJ06-XPROPT PICTURE  X(04).                   CI0273
            11            PJ06-FILLER PICTURE  S9(5)                    CI0273
                          BINARY.                                       CI0273
            11            PJ06-XSEGNM PICTURE  X(08).                   CI0273
            11            PJ06-XKEYLN PICTURE  S9(05)                   CI0273
                          BINARY.                                       CI0273
            11            PJ06-XSEGNB PICTURE  9(05)                    CI0273
                          BINARY.                                       CI0273
            11            PJ06-XCOKEY PICTURE  X(70).                   CI0273
      *** PCB MASK FOR SCOP                                             ADU015
      *!WF DSP=PK DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PK00.                                         CI0273
          05              PK00-SUITE.                                   CI0273
            15       FILLER         PICTURE  X(00106).                  CI0273
       01                 PK06  REDEFINES      PK00.                    CI0273
            10            PK06-XDBPCB.                                  CI0273
            11            PK06-XDBDNM PICTURE  X(08).                   CI0273
            11            PK06-XSEGLV PICTURE  X(02).                   CI0273
            11            PK06-XRC    PICTURE  X(02).                   CI0273
            11            PK06-XPROPT PICTURE  X(04).                   CI0273
            11            PK06-FILLER PICTURE  S9(5)                    CI0273
                          BINARY.                                       CI0273
            11            PK06-XSEGNM PICTURE  X(08).                   CI0273
            11            PK06-XKEYLN PICTURE  S9(05)                   CI0273
                          BINARY.                                       CI0273
            11            PK06-XSEGNB PICTURE  9(05)                    CI0273
                          BINARY.                                       CI0273
            11            PK06-XCOKEY PICTURE  X(70).                   CI0273
      *****************************************************************
      *PASS AREA FROM CALLING MODULE
      *****************************************************************
      *!WF DSP=LK DSL=V1 SEL=70 FOR=I DES=1 LEV=1 PLT=10
       01                 LK70.                                         CI0273
            10            LK70-C299.                                    CI0273
            11            LK70-CTID.                                    CI0273
            12            LK70-CTIDA  PICTURE  9(3).                    CI0273
            12            LK70-CTIDN.                                   CI0273
            13            LK70-CTIDNP PICTURE  X(13).                   CI0273
            13            LK70-CTIDND PICTURE  9(11).                   CI0273
            10            LK70-GECKD  PICTURE  9.                       CI0273
            10            LK70-ICUST  PICTURE  X.                       CI0273
            10            LK70-PRCOD  PICTURE  9(5).                    CI0273
            10            LK70-PRSCD  PICTURE  X(9).                    CI0273
            10            LK70-DCACG9 PICTURE  9(8).                    CI0273
            10            LK70-NAASQ  PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-CAATY  PICTURE  9(3).                    CI0273
            10            LK70-CACTO  PICTURE  9(3).                    CI0273
            10            LK70-CASTC  PICTURE  99.                      CI0273
            10            LK70-ITRAN  PICTURE  X.                       CI0273
            10            LK70-GEAUN  PICTURE  9(5).                    CI0273
            10            LK70-GEOPD2 PICTURE  X(8).                    CI0273
            10            LK70-DEFFT  PICTURE  9(8).                    CI0273
            10            LK70-CTRTP  PICTURE  X(2).                    CI0273
            10            LK70-CTWHAT PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-PWHLD  PICTURE  S999V9(5)                CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-GETIM  PICTURE  S9(7)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-ADBRQA PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-IWTHH  PICTURE  X.                       CI0273
            10            LK70-CLCUS  PICTURE  99.                      CI0273
            10            LK70-CCACT  PICTURE  99.                      CI0273
            10            LK70-AFEET  PICTURE  S9(5)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-ITERF  PICTURE  X.                       CI0273
            10            LK70-ATERF  PICTURE  S9(5)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-CLDOB  PICTURE  9(8).                    CI0273
            10            LK70-CPLTYP PICTURE  X(14).                   CI0273
            10            LK70-IACFPD PICTURE  X(1).                    CI0273
            10            LK70-CDELI  PICTURE  9(3).                    CI0273
            10            LK70-CPAYC  PICTURE  X(2).                    CI0273
            10            LK70-ACOTD  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-NPBN   PICTURE  X(20).                   CI0273
            10            LK70-CCBAT  PICTURE  99.                      CI0273
            10            LK70-CLID4.                                   CI0273
            11            LK70-CLIDA  PICTURE  9(3).                    CI0273
            11            LK70-CLIDNP PICTURE  X(12).                   CI0273
            11            LK70-CLIDNA PICTURE  9(8).                    CI0273
            10            LK70-GENAL1 PICTURE  X(30).                   CI0273
            10            LK70-GENAL2 PICTURE  X(30).                   CI0273
            10            LK70-GESAD1 PICTURE  X(30).                   CI0273
            10            LK70-GESAD2 PICTURE  X(30).                   CI0273
            10            LK70-GESAD3 PICTURE  X(30).                   CI0273
            10            LK70-NTR    PICTURE  9(8).                    CI0273
            10            LK70-GECKD1 PICTURE  9.                       CI0273
            10            LK70-IMQMG  PICTURE  X.                       CI0273
            10            LK70-NIPAD  PICTURE  X(15).                   CI0273
            10            LK70-CLNAM.                                   CI0273
            11            LK70-CLNAMH PICTURE  X(6).                    CI0273
            11            LK70-CLNAMF PICTURE  X(20).                   CI0273
            11            LK70-CLNAMI PICTURE  X.                       CI0273
            11            LK70-CLNAMR PICTURE  X(14).                   CI0273
            11            LK70-CLNAML PICTURE  X(25).                   CI0273
            11            LK70-CLNAMS PICTURE  X(4).                    CI0273
            10            LK70-CSLCT  PICTURE  X.                       CI0273
            10            LK70-C199.                                    CI0273
            11            LK70-CLID.                                    CI0273
            12            LK70-CLIDO  PICTURE  9(3).                    CI0273
            12            LK70-CLIDN.                                   CI0273
            13            LK70-CLIDNP PICTURE  X(12).                   CI0273
            13            LK70-CLIDND PICTURE  9(8).                    CI0273
            10            LK70-GECKD2 PICTURE  9.                       CI0273
            10            LK70-CPROCM PICTURE  X.                       CI0273
            10            LK70-NAASQL PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-NSEQ4B PICTURE  9(8)                     CI0273
                          BINARY.                                       CI0273
            10            LK70-CLTIN  PICTURE  9(12).                   CI0273
            10            LK70-IPULL  PICTURE  X.                       CI0273
            10            LK70-NBTCH  PICTURE  9(4).                    CI0273
            10            LK70-CVSYS  PICTURE  X(2).                    CI0273
            10            LK70-NPISQ  PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-CPITC  PICTURE  99.                      CI0273
            10            LK70-PPOTD  PICTURE  S9(3)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-ITRNB  PICTURE  X.                       CI0273
            10            LK70-CTTLN1 PICTURE  X(30).                   CI0273
            10            LK70-CTTLN2 PICTURE  X(30).                   CI0273
            10            LK70-CTTLN3 PICTURE  X(30).                   CI0273
            10            LK70-CTTBO1 PICTURE  X(45).                   CI0273
            10            LK70-CTTBO2 PICTURE  X(45).                   CI0273
            10            LK70-PRCMN  PICTURE  X(20).                   CI0273
            10            LK70-TTRTP  PICTURE  X(30).                   CI0273
            10            LK70-DXTMSA PICTURE  X(26).                   CI0273
            10            LK70-DXTMS2 PICTURE  X(26).                   CI0273
            10            LK70-CUPIQ  PICTURE  X.                       CI0273
            10            LK70-IQACT  PICTURE  X.                       CI0273
            10            LK70-MAPPN  PICTURE  X(10).                   CI0273
            10            LK70-CTTYPG PICTURE  X(04).                   CI0273
            10            LK70-CLORN  PICTURE  X(45).                   CI0273
            10            LK70-APMTL  PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-MPMTFL PICTURE  X(24).                   CI0273
            10            LK70-ANETTQ PICTURE  S9(9)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-TTBAL  PICTURE  X(15).                   CI0273
            10            LK70-MPRN4  PICTURE  X(35).                   CI0273
            10            LK70-CCONF  PICTURE  X(25).                   CI0273
            10            LK70-DCACG  PICTURE  9(8).                    CI0273
            10            LK70-NMESA  PICTURE  9(6).                    CI0273
            10            LK70-MCSIG  PICTURE  X(30).                   CI0273
            10            LK70-IERRC  PICTURE  X.                       CI0273
            10            LK70-AVLMN  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-AVLMX  PICTURE  S9(7)V99                 CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-AVCSH  PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-ACVALM PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-INDRS  PICTURE  X.                       CI0273
            10            LK70-GRID   PICTURE  X(13).                   CI0273
            10            LK70-AACTV  PICTURE  S9(11)V99                CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-CTCUS  PICTURE  999.                     CI0273
            10            LK70-CCDSCW PICTURE  9(2).                    CI0273
            10            LK70-CHCR   PICTURE  99.                      CI0273
            10            LK70-IOWNG  PICTURE  X.                       CI0273
            10            LK70-GECSQ  PICTURE  S9(3)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            LK70-CQACT  PICTURE  999.                     CI0273
            10            LK70-CTOWN  PICTURE  9(3).                    CI0273
            10            LK70-NGEOR  PICTURE  9(08).                   CI0273
            10            LK70-FILLER PICTURE  X(85).                   CI0273
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0273
          05              DE00-SUITE.                                   CI0273
            15       FILLER         PICTURE  X(00653).                  CI0273
       01                 DE10  REDEFINES      DE00.                    CI0273
            10            DE10-DU11.                                    CI0273
            11            DE10-XFONC  PICTURE  X(4).                    CI0273
            11            DE10-MPSBN  PICTURE  X(8).                    CI0273
            11            DE10-XDBDNM PICTURE  X(08).                   CI0273
            11            DE10-XSEGNM PICTURE  X(08).                   CI0273
            11            DE10-XRC    PICTURE  X(02).                   CI0273
            11            DE10-MSEG   PICTURE  X(08).                   CI0273
            11            DE10-XCOKEY PICTURE  X(70).                   CI0273
            11            DE10-CUIBR  PICTURE  X(01).                   CI0273
            11            DE10-CUIBA  PICTURE  X(01).                   CI0273
            11            DE10-IPBIK  PICTURE  X(1).                    CI0273
            10            DE10-DU03.                                    CI0273
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            DE10-CMSSF  PICTURE  XX.                      CI0273
            11            DE10-DU09.                                    CI0273
            12            DE10-CMESA  PICTURE  S9(9)                    CI0273
                          BINARY.                                       CI0273
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0273
                          BINARY.                                       CI0273
            12            DE10-CMESB  PICTURE  S9(9)                    CI0273
                          BINARY.                                       CI0273
            12            DE10-CMSST  PICTURE  S9(9)                    CI0273
                          BINARY.                                       CI0273
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0273
                          BINARY.                                       CI0273
            12            DE10-QELLAA PICTURE  S9(9)                    CI0273
                          BINARY.                                       CI0273
            12            DE10-TMESS4 PICTURE  X(512).                  CI0273
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
       01                 MS00.                                         CI0273
          05              MS00-SUITE.                                   CI0273
            15       FILLER         PICTURE  X(00542).                  CI0273
       01                 MS03  REDEFINES      MS00.                    CI0273
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            10            MS03-CMSSF  PICTURE  XX.                      CI0273
            10            MS03-DU09.                                    CI0273
            11            MS03-CMESA  PICTURE  S9(9)                    CI0273
                          BINARY.                                       CI0273
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0273
                          BINARY.                                       CI0273
            11            MS03-CMESB  PICTURE  S9(9)                    CI0273
                          BINARY.                                       CI0273
            11            MS03-CMSST  PICTURE  S9(9)                    CI0273
                          BINARY.                                       CI0273
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0273
                          BINARY.                                       CI0273
            11            MS03-QELLAA PICTURE  S9(9)                    CI0273
                          BINARY.                                       CI0273
            11            MS03-TMESS4 PICTURE  X(512).                  CI0273
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0273
            10            MX11-QMSGS  PICTURE  9(03).                   CI0273
            10            MX11-PJ09                                     CI0273
                          OCCURS       025     TIMES.                   CI0273
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0273
                          COMPUTATIONAL-3.                              CI0273
            11            MX11-CMESB  PICTURE  S9(9)                    CI0273
                          BINARY.                                       CI0273
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                LK70
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
       F0TSC.                                                           lv10
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR ACAP                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-ACAP-PTR1.                                          ADU015
      *SET ADDRESS FOR CH1P                                             DOT
           SET ADDRESS OF PF06 TO                                       ADU015
                PCB-CH1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CCRP                                             DOT
           SET ADDRESS OF PG06 TO                                       ADU015
                PCB-CCRP-PTR1.                                          ADU015
      *SET ADDRESS FOR CPRP                                             DOT
           SET ADDRESS OF PH06 TO                                       ADU015
                PCB-CPRP-PTR1.                                          ADU015
      *SET ADDRESS FOR CBTP                                             DOT
           SET ADDRESS OF PI06 TO                                       ADU015
                PCB-CBTP-PTR1.                                          ADU015
      *SET ADDRESS FOR CA1P                                             DOT
           SET ADDRESS OF PJ06 TO                                       ADU015
                PCB-CA1P-PTR1.                                          ADU015
      *SET ADDRESS FOR SCOP                                             DOT
           SET ADDRESS OF PK06 TO                                       ADU015
                PCB-SCOP-PTR1.                                          ADU015
       F0TSC-FN. EXIT.
      *N01.      NOTE *************************************.            CI0273
      *               *                                   *             CI0273
      *               *INITIALISATIONS                    *             CI0273
      *               *                                   *             CI0273
      *               *************************************.            CI0273
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
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0273
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0273
      *               *                                   *             CI0273
      *               *FIN DE TRAITEMENT                  *             CI0273
      *               *                                   *             CI0273
      *               *************************************.            CI0273
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0273
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               ********************************    *
      *               *                                   *
      *               *************************************.
       F30.                                                             lv05
      *       MAIN PROCESSING
      ********************************
      *N30BB.    NOTE *GET CAMS ACCOUNTING DATE           *.
       F30BB.                                                           lv10
      *
           PERFORM     F91BC THRU F91BC-FN.
       F30BB-FN. EXIT.
      *N30BC.    NOTE *VALIDATE CLIENT ID                 *.
       F30BC.                                                           lv10
           MOVE        'Y' TO WS00-LIVING
           MOVE        LK70-CLID TO S-CLU01-CL01K
      *GU CALL ON CL01
           PERFORM     F94CC THRU F94CC-FN.
       F30BC-FN. EXIT.
      *N30BD.    NOTE *READ CL03                          *.
       F30BD.    IF    CL01-CLTYP = 'P'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F30BD-FN.
      *
      *GN CALL FOR CL03
           INITIALIZE  CL03-CF
           PERFORM     F94CP THRU F94CP-FN.
                 IF    CL03-CF = '0'                                    DOT
           MOVE        012238 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN
      *SET FLAG IF CLIENT IS ALIVE OR
      *DEAD.
                 IF    CL03-CLDOD > 0                                   DOT
                 OR    CL03-CLDTH = 'Y'
           MOVE        'N' TO WS00-LIVING.
      *N30BE.    NOTE *CHECK IF CLIENT TIN IS ZEROS       *.
       F30BE.                                                           lv15
      *********************************
                 IF    CL01-CLTIN = ZEROES                              DOT
           MOVE        'N' TO WS00-CLTIN
                 ELSE
           MOVE        'Y' TO WS00-CLTIN
           MOVE        CL01-CLTINC TO WS00-CLTINC.
       F30BE-FN. EXIT.
      *N30BF.    NOTE *DETERMINE CLIENT AGE.              *.
       F30BF.    IF    WS00-LIVING = 'Y'                                lv15
                 AND   WS00-CLTIN = 'Y'
                 NEXT SENTENCE ELSE GO TO     F30BF-FN.
           INITIALIZE  DD30 DD33 7-OAGE-PASSED-FIELDS
           MOVE        NS20-DCACG TO DD33-DTGRG
           MOVE        4 TO DD30-CDTSF
      *CALL AADA81 TO CALCULATE CURRENT
      *DATE IN JULIAN FORMAT
           PERFORM     F92AB THRU F92AB-FN.
                 IF    DD30-CDTSC = 0                                   DOT
           MOVE        DD33-DTJUL TO 7-OAGE-CURRENT-DATE
      *.
           INITIALIZE  DD30 DD33 7-OAGE-BIRTH-DATE                      DOT
           MOVE        CL03-CLDOB TO DD33-DTGRG
           MOVE        4 TO DD30-CDTSF
      *CALL AADA81 TO CALCULATE BIRTH
      *DATE IN JULIAN FORMAT
           PERFORM     F92AB THRU F92AB-FN.
                 IF    DD30-CDTSC = 0                                   DOT
           MOVE        DD33-DTJUL TO 7-OAGE-BIRTH-DATE
      *CALL AAOAG3 TO CALCULATE CLIENT
      *AGE.
                 IF    7-OAGE-CURRENT-DATE > ZERO                       DOT
                 AND   7-OAGE-BIRTH-DATE > ZERO
           PERFORM     F92BB THRU F92BB-FN.
       F30BF-FN. EXIT.
       F30BD-FN. EXIT.
      *N30CE.    NOTE *VALIDATE CLIENT ID                 *.
       F30CE.    IF    CL01-CLTYP = 'O'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F30CE-FN.
           MOVE        CL01-CLTINC TO WS00-CLTINC.
       F30CE-FN. EXIT.
       F30-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *VERIFY ALL THE BUSINESS RULES      *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40AK.    NOTE *CALL CI0085 TO DECIPHER            *.
       F40AK.                                                           lv10
      *OWNERSHIP/ADDRESS LINES
           MOVE        LK70-CTTLN1 TO QT58-CTTLN1
           MOVE        LK70-CTTLN2 TO QT58-CTTLN2
           MOVE        LK70-CTTLN3 TO QT58-CTTLN3
           MOVE        LK70-CTTBO1 TO QT58-CTTBO1
           MOVE        LK70-CTTBO2 TO QT58-CTTBO2
           MOVE        LK70-GESAD1 TO QT58-GESAD1
           MOVE        LK70-GESAD2 TO QT58-GESAD2
           MOVE        LK70-GESAD3 TO QT58-GESAD3
           PERFORM     F91IB THRU F91IB-FN.
      *N40AL.    NOTE *ONLY CHECK THE PERSONAL CLIENT     *.
       F40AL.    IF    WS00-LIVING = 'Y'                                lv15
                 AND   WS00-CLTIN = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40AL-FN.
      *N40AM.    NOTE *FOR UGMA/UTMA ACCOUNT              *.
       F40AM.    IF    K21F-IOWNC (34) = 'Y'                            lv20
                 OR    K21F-IOWNC (35) = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40AM-FN.
      *N40AP.    NOTE *CLIENT AGE IS LESS THAN 18         *.
       F40AP.    IF    7-OAGE-CLIENT-AGE < 18                           lv25
                 NEXT SENTENCE ELSE GO TO     F40AP-FN.
           MOVE        015673 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40AP-FN. EXIT.
      *N40AT.    NOTE *CLIENT AGE >= 18                   *.
       F40AT.    IF    7-OAGE-CLIENT-AGE >= 18                          lv25
                 AND   LK70-CTIDA = (001 OR 133)
                 NEXT SENTENCE ELSE GO TO     F40AT-FN.
      *CERTS/BETA BROKERAGE ACCOUNT
      *CALL CI0222 TO GET CLIENT ROLE
           PERFORM     F91IP THRU F91IP-FN.
      *N40BD.    NOTE *CHECK ALL THE ROLES ON THE         *.
       F40BD.                                                           lv30
           MOVE        1                        TO J40BDR
                                    GO TO     F40BD-B.
       F40BD-A.
           ADD         1                        TO J40BDR.
       F40BD-B.
           IF          J40BDR                   >  20
                                    GO TO     F40BD-FN.
      *ACCOUNT
      *N40BG.    NOTE *PRIMARY OWNER                      *.
       F40BG.    IF    X122-CLCTRC (J40BDR) = 001                       lv35
                 NEXT SENTENCE ELSE GO TO     F40BG-FN.
      *********************************
                 IF    X122-CLID (J40BDR) =                             DOT
                       LK70-CLID
      *IF THE LOGIN CLIENT IS PRIMARY
      *OWNER OF ACCOUNT
           MOVE        015673 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
      *NEXT LOOP                                                        DOT
               GO TO     F40BD-900.
       F40BG-FN. EXIT.
       F40BD-900. GO TO F40BD-A.
       F40BD-FN. EXIT.
      *N40BI.    NOTE *IF THE LOGIN CLIENT IS NOT         *.
       F40BI.                                                           lv30
      *PRIMARY OWNER OF ACCOUNT
           MOVE        015639 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC.
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40BI-FN. EXIT.
       F40AT-FN. EXIT.
       F40AM-900. GO TO F40BK-FN.
       F40AM-FN. EXIT.
      *N40BK.    NOTE *NOT UGMA/UTMA ACCOUNT, ERROR OUT   *.
       F40BK.                                                           lv20
                 IF    LK70-CTIDA = (001 OR 133)                        DOT
                 AND   7-OAGE-CLIENT-AGE < 18
      *IF CERTS/BETA BROKERAGE AND
      *CLIENT AGE < 18
           MOVE        015433 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40BK-FN. EXIT.
       F40AL-FN. EXIT.
       F40AK-FN. EXIT.
      *N40BP.    NOTE *CHECK IF CLIENT IS OWNER ON ACCT   *.
       F40BP.    IF    LK70-CTIDA = (001 OR 133)                        lv10
                 NEXT SENTENCE ELSE GO TO     F40BP-FN.
           MOVE        LK70-CTID TO S-CTU01-CT01K
           PERFORM     F94DA THRU F94DA-FN.
      *N40DB.    NOTE *CT01 READ - SUCCESS                *.
       F40DB.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F40DB-FN.
      *N40DC.    NOTE *ERROR OUT IF THE ACCOUNT IS        *.
       F40DC.    IF    CT01-CTSTA NOT = 02                              lv20
                 NEXT SENTENCE ELSE GO TO     F40DC-FN.
      *NOT ACTIVE
           MOVE        015659 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40DC-FN. EXIT.
      *N40DH.    NOTE *READ THROUGH CT07                  *.
       F40DH.                                                           lv20
           MOVE        LK70-CLID TO S-CTU07-CT07K
           PERFORM     F94JA THRU F94JA-FN.
      *N40DK.    NOTE *CT07 READ - SUCCESS                *.
       F40DK.    IF    CT07-CF = '1'                                    lv25
                 NEXT SENTENCE ELSE GO TO     F40DK-FN.
           MOVE        '1' TO CT09-CF.
      *N40DN.    NOTE *LOOP THROUGH CT09                  *.
       F40DN.                       GO TO     F40DN-B.                  lv30
       F40DN-A.
                 IF    CT09-CF = '0'
                                    GO TO     F40DN-FN.
       F40DN-B.
           MOVE        CT07-CLID TO S-CTU07-CT07K
           PERFORM     F94HA THRU F94HA-FN.
      *N40DQ.    NOTE *CHECK IF THE CLIENT IS OWNER       *.
       F40DQ.    IF    (CT09-CLCTRC = 001                               lv35
                 OR    CT09-CLCTRC = 007)
                 AND   CT09-CF = '1'
                 AND   (CT09-GERED = ZEROS
                 OR    CT09-GERED NOT < NS20-DCACG)
                 NEXT SENTENCE ELSE GO TO     F40DQ-FN.
           MOVE        'Y' TO WS00-IAIND1.
      *N40DS.    NOTE *ERROR OUT IF CLIENT IS NOT THE     *.
       F40DS.    IF    CT07-CF = '0'                                    lv40
                 OR    WS00-IAIND1 = 'N'
                 NEXT SENTENCE ELSE GO TO     F40DS-FN.
      *OWNER OF THE ACCOUNT
           MOVE        015434 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40DS-FN. EXIT.
       F40DQ-FN. EXIT.
       F40DN-900. GO TO F40DN-A.
       F40DN-FN. EXIT.
       F40DK-FN. EXIT.
       F40DH-FN. EXIT.
       F40DB-900. GO TO F40DV-FN.
       F40DB-FN. EXIT.
      *N40DV.    NOTE *READ CT01 FAILED                   *.
       F40DV.                                                           lv15
           MOVE        015435 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F40DV-FN. EXIT.
       F40BP-FN. EXIT.
      *N40DX.    NOTE *TIN CERTIFICATION REQUIRED FOR     *.
       F40DX.    IF    LK70-CTIDA = 133                                 lv10
                 NEXT SENTENCE ELSE GO TO     F40DX-FN.
      *BETA BROKERAGE
      *********************************
                 IF    WS00-CLTINC NOT = '2'                            DOT
      *IF NOT CERTIFIED
           MOVE        13256 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40DX-FN. EXIT.
      *N40EB.    NOTE *CHECK CERTS PRODUCT CODE           *.
       F40EB.    IF    LK70-CTIDA = 001                                 lv10
                 NEXT SENTENCE ELSE GO TO     F40EB-FN.
      *********************************
                 IF    (LK70-CQACT > 0                                  DOT
                 AND   LK70-PRCOD NOT = '00972')
                 OR    (LK70-CQACT = 0
                 AND   LK70-PRCOD NOT = '00662')
      *ONLY QUALIFIED 972 OR
      *NON-QUALIFIED 662 IS VALID
           MOVE        015670 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40EB-FN. EXIT.
      *N40ED.    NOTE *CHECK BETA BROK PRODUCT CODE       *.
       F40ED.    IF    LK70-CTIDA = 133                                 lv10
                 NEXT SENTENCE ELSE GO TO     F40ED-FN.
      *********************************
           MOVE        LK70-PRCOD TO WS00-PRCOD.
                 IF    NOT VALID-BETA-PRCOD                             DOT
      *NOT VALID PRODUCTS
           MOVE        015670 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40ED-FN. EXIT.
      *N40EE.    NOTE *ACCOUNT IS 403B ACCOUNT            *.
       F40EE.    IF    LK70-CQACT = 002                                 lv10
                 OR    LK70-CQACT = 003
                 OR    LK70-CQACT = 004
                 NEXT SENTENCE ELSE GO TO     F40EE-FN.
           MOVE        015586 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40EE-FN. EXIT.
      *N40EG.    NOTE *ACCOUNT OWNER IS NON-RESIDENT      *.
       F40EG.    IF    CL01-CLTYP = 'P'                                 lv10
                 AND   CL03-ICUSC = 'F'
                 NEXT SENTENCE ELSE GO TO     F40EG-FN.
      *ALIEN (NRA)
      *********************************
           MOVE        015640 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40EG-FN. EXIT.
      *N40EQ.    NOTE *ERROR OUT IF THE ACCOUNT HAS       *.
       F40EQ.    IF    K21F-IOWNC (40) = 'Y'                            lv10
                 OR    K21F-IOWNC (37) = 'Y'
                 OR    K21F-IOWNC (18) = 'Y'
                 OR    K21F-IOWNC (10) = 'Y'
                 OR    K21F-IOWNC (39) = 'Y'
                 OR    K21F-IOWNC (38) = 'Y'
                 OR    K21F-IOWNC (36) = 'Y'
                 OR    K21F-IOWNC (02) = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40EQ-FN.
      *INVALID OWNERSHIP
      *COOGAN LAW, TUTOR/TUTRIX,
      *USUFRUCTORY, GUARDIANSHIP
      *NEXT FRIEND,
      *CONSERVATORSHIP,
      *COMMITTEE/CURATOR,
      *CONDITIONAL, A MINOR
      *********************************
           MOVE        014106 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40EQ-FN. EXIT.
      *N40ES.    NOTE *ERROR OUT IF THE ACCOUNT WITH      *.
       F40ES.    IF    LK70-CLCUS = 02 OR 23                            lv10
                 NEXT SENTENCE ELSE GO TO     F40ES-FN.
      *IRA PLAN WITH SEPP ARRANGEMENT
           MOVE        015636 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40ES-FN. EXIT.
      *N40JA.    NOTE *SPECIFIC CHECK FOR CERTS ACCOUNT   *.
       F40JA.    IF    LK70-CTIDA = 001                                 lv10
                 NEXT SENTENCE ELSE GO TO     F40JA-FN.
      *N40JC.    NOTE *FOR CERTS ACCOUNT                  *.
       F40JC.                                                           lv15
      *READ CL2B TO CHECK ACC/ATC TIN
           INITIALIZE  CL2B
           MOVE        LK70-CLID (1:3) TO CL2B-CLIDO5
           MOVE        LK70-CLID (4:20) TO CL2B-CLIDN.
                 IF    LK70-CQACT > 0                                   DOT
           MOVE        002 TO CL2B-CPRCP.
                 IF    LK70-CQACT = 0                                   DOT
           MOVE        001 TO CL2B-CPRCP.
           PERFORM     F94MD THRU F94MD-FN.                             DOT
      *N40JG.    NOTE *ERROR OUT IF THE ACC OR ATC TIN    *.
       F40JG.    IF    CL2B-CTTNC NOT = 002                             lv20
                 NEXT SENTENCE ELSE GO TO     F40JG-FN.
      *NOT CERTIFIED FOR THE ACCOUNT
           MOVE        013256 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JG-FN. EXIT.
       F40JC-FN. EXIT.
      *N40JL.    NOTE *CALL CI0135 FOR CERTS VALUES       *.
       F40JL.                                                           lv15
           PERFORM     F91CE THRU F91CE-FN.
      *N40JO.    NOTE *ERROR OUT IF THERE IS AN HOLD      *.
       F40JO.    IF    CE02-CEHCD (1) > 0                               lv20
                 OR    CE02-CEHCD (2) > 0
                 OR    CE02-CEHCD (3) > 0
                 OR    CE02-CEHCD (4) > 0
                 OR    CE02-CEHCD (5) > 0
                 OR    CE02-CEHCD (6) > 0
                 NEXT SENTENCE ELSE GO TO     F40JO-FN.
      *CODE ON THE CERTS ACCOUNT
           MOVE        013174 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JO-FN. EXIT.
      *N40JR.    NOTE *ERROR OUT IF THE TRANSFER AMT      *.
       F40JR.                                                           lv20
      *IS LESS THAN 100
      *CALCULATE TRANSFER AMOUNT
           PERFORM     F90 THRU F90-FN.
                 IF    WS00-TRAN < ONE-TIME-MIN                         DOT
      *TRANSFER AMOUNT IS LESS THAN THE
      *MINIMUM AMOUNT 100
           MOVE        015671 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JR-FN. EXIT.
      *N40JV.    NOTE *ERROR OUT IF INITIAL FUNDING       *.
       F40JV.                                                           lv20
      *DATE IS WITHIN 10 BUSINESS DAYS
      *FROM CURRENT ACCOUNTING DATE
           INITIALIZE  WS00-DCACG
           WS00-DNACG
           MOVE        CE02-CESLD TO WS00-DNACG.
      *N40JW.    NOTE *GET NEXT 10 BUSINESS DAYS          *.
       F40JW.                                                           lv25
           MOVE        1                        TO J40JWR
                                    GO TO     F40JW-B.
       F40JW-A.
           ADD         1                        TO J40JWR.
       F40JW-B.
           IF          J40JWR                   >  10
                                    GO TO     F40JW-FN.
      *
           MOVE        WS00-DNACG TO WS00-DCACG
           PERFORM     F91DT THRU F91DT-FN.
       F40JW-900. GO TO F40JW-A.
       F40JW-FN. EXIT.
      *N40JX.    NOTE *ERROR OUT IF THE REQUEST DATE IS   *.
       F40JX.    IF    WS00-DNACG > NS20-DCACG                          lv25
                 NEXT SENTENCE ELSE GO TO     F40JX-FN.
      *IN THE INITIAL FUNDING PERIOD
           MOVE        015672 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JX-FN. EXIT.
       F40JV-FN. EXIT.
       F40JL-FN. EXIT.
       F40JA-FN. EXIT.
      *N40KB.    NOTE *IF THE CLIENT AGE IS CORRECT       *.
       F40KB.    IF    CLIENT-VALID-AGE                                 lv10
                 OR    CL01-CLTYP = 'O'
                 NEXT SENTENCE ELSE GO TO     F40KB-FN.
      *OR ORGANIZATIONAL CLIENT
      *N40KD.    NOTE *CHECK FOR VALID WITHHOLDING        *.
       F40KD.    IF    LK70-IQACT = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F40KD-FN.
      *PERCENT FOR QUALIFIED ACCOUNT
      *
                 IF    (LK70-PWHLD < 10                                 DOT
                 AND   LK70-PWHLD NOT = ZEROES)
                 OR    LK70-PWHLD > 99
      *IF INVALID WITHHOLDING PERCENT
      *SEND ERROR MESSAGE OUT
           MOVE        013287 TO LK70-NMESA
           MOVE        'Y' TO LK70-IERRC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40KD-FN. EXIT.
       F40KB-FN. EXIT.
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
      *               *CALCULATE TRANSFER AMOUNT          *
      *               *                                   *
      *               *************************************.
       F90.           EXIT.                                             lv05
      *N90AG.    NOTE *INITIALIZE AMOUNT                  *.
       F90AG.                                                           lv10
           INITIALIZE  WS00-TRAN
           WS00-CASH.
       F90AG-FN. EXIT.
      *N90AP.    NOTE *ADJUST ACCT VALUE IF LOAN OPTION   *.
       F90AP.    IF    CE02-CELBL > 0                                   lv10
                 NEXT SENTENCE ELSE GO TO     F90AP-FN.
      *********************************
           COMPUTE     CE02-ACCTV8 = CE02-ACCTV8 +
           CE02-CELBL.
       F90AP-FN. EXIT.
      *N90AT.    NOTE *CALL CI0265 TO GET RISK FUND       *.
       F90AT.                                                           lv10
           INITIALIZE  MS03 WA65
           MOVE        LK70-MAPPN TO WA65-MAPPN
           MOVE        LK70-CTID TO WA65-CTID
           PERFORM     F91MB THRU F91MB-FN.
       F90AT-FN. EXIT.
      *N90BH.    NOTE *READ ACTIVITY DATABASE TO          *.
       F90BH.                                                           lv10
      *CALCULATE THE CUMULATIVE AMOUNT
           MOVE        LK70-CTID TO S-GCU01-GC01K
           PERFORM     F94GA THRU F94GA-FN.
      *N90BI.    NOTE *READ GC03                          *.
       F90BI.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F90BI-FN.
           MOVE        '1' TO GC03-CF.
      *N90BK.    NOTE *LOOP THROUGH GC03                  *.
       F90BK.                       GO TO     F90BK-B.                  lv20
       F90BK-A.
                 IF    GC03-CF = '0'
                                    GO TO     F90BK-FN.
       F90BK-B.
      *********************************
           PERFORM     F94GB THRU F94GB-FN.
                 IF    GC03-CF = '1'                                    DOT
                 AND   ((GC03-IPULL = 'Y'
                 AND   GC03-CASTC = 1)
                 OR    (GC03-IPULL = 'N'
                 AND   GC03-CASTC = 4))
                 AND   GC03-ADBRQ IS NUMERIC
      *GET PENDING ACTIVITY AMOUNT
      *THE ACTIVITY IS TO BE PULLED,
      *AND NOT YET PROCESSED, OR THE
      *ACTIVITY IS PULLED BUT SUSPENSED
      *********************************
           COMPUTE     WS00-CASH = WS00-CASH +
           GC03-ADBRQ.
       F90BK-900. GO TO F90BK-A.
       F90BK-FN. EXIT.
       F90BI-FN. EXIT.
       F90BH-FN. EXIT.
      *N90CA.    NOTE *TRANSFER AMOUNT IS GOOD FUNDS      *.
       F90CA.                                                           lv10
      *AMOUNT MINUS MINIMUM BALANCE
      *AMOUNT 1000 AND CUMULATIVE
      *AMOUNT
           COMPUTE     WS00-TRAN = WA65-AGOFD -
           1000 - WS00-CASH.
       F90CA-FN. EXIT.
       F90-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N91.      NOTE *************************************.
      *               *                                   *
      *               *CALLED MODULES                     *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91BC.    NOTE *CALL CI0020 - CAMS ACCTG DATE      *.            AM0020
       F91BC.                                                           lv10
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
       F91BC-FN. EXIT.
      *N91CE.    NOTE *---> Call CI0135                   *.            AM0135
       F91CE.                                                           lv10
      *     Get Cert Account Info                                       AM0135
      *                                                                 AM0135
           INITIALIZE  CE02                                             AM0135
           MS03
           DE10-DU03                                                    AM0135
           MOVE        LK70-CTID TO CE02-CTID                           AM0135
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
           CE02                                                         AM0135
           DE10                                                         AM0135
           MS03                                                         AM0135
           MX11.                                                        AM0135
      *N91CF.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F91CF.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F91CF-FN.                 ADU071
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
       F91CF-900. GO TO F91CG-FN.
       F91CF-FN. EXIT.
      *N91CG.    NOTE *NO ERRORS                          *.            ADU071
       F91CG.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F91CG-FN. EXIT.
       F91CE-FN. EXIT.
      *N91DT.    NOTE *VALIDATE ACCOUNTING DATE           *.
       F91DT.                                                           lv10
      *********************************
           MOVE        WS00-DCACG TO 7-XX01-PCKDAT                      $AACTG
           COMPUTE     7-XX01-PUDAT =                                   $AACTG
           (7-XX01-PCKDAT * 10)                                         $AACTG
           MOVE        7-XX01-UNSDAT TO 7-XX01-ICURR                    $AACTG
           CALL        7-XX01-DATMOD USING                              $AACTG
           7-XX01-IDTFLD                                                $AACTG
           7-XX01-RDTFLD                                                $AACTG
           MOVE        7-XX01-RCDATE TO 7-XX01-CHKDAT.                  $AACTG
                 IF    7-XX01-CHKPDT = +177607040                       DOT
           MOVE        ZEROES TO WS00-DNACG                             $AACTG
                 ELSE                                                   $AACTG
           MOVE        7-XX01-RNDATE TO 7-XX01-CHKDAT                   $AACTG
           COMPUTE     7-XX01-NEXTDT =                                  $AACTG
           (7-XX01-CHKPDT / 10)                                         $AACTG
           MOVE        7-XX01-NEXTDT TO WS00-DNACG.                     $AACTG
       F91DT-FN. EXIT.
      *N91IB.    NOTE *CALL CI0085 - DECIPHER OWNERSHIP   *.            AM0085
       F91IB.                                                           lv10
      *********************************                                 AM0085
      ** THIS MODULE WILL SEARCH      *                                 AM0085
      ** OWNERSHIP LINES THROUGH      *                                 AM0085
      ** THE TEXT LOOKING FOR ACCT    *                                 AM0085
      ** CLASSIFICATION               *                                 AM0085
      *********************************                                 AM0085
           INITIALIZE AB04 K21F MS03                                    AM0085
           MOVE        QT58-CTTLN1 TO AB04-CTTLN1                       AM0085
           MOVE        QT58-CTTLN2 TO AB04-CTTLN2                       AM0085
           MOVE        QT58-CTTLN3 TO AB04-CTTLN3                       AM0085
           MOVE        QT58-CTTBO1 TO AB04-CTTBO1                       AM0085
           MOVE        QT58-CTTBO2 TO AB04-CTTBO2                       AM0085
           MOVE        PROGR TO K21F-PROGR                              AM0085
           MOVE        QT58-GESAD1 TO K21F-GESAD1                       AM0085
           MOVE        QT58-GESAD2 TO K21F-GESAD2                       AM0085
           MOVE        QT58-GESAD3 TO K21F-GESAD3                       AM0085
           CALL        CI0085 USING                                     AM0085
           DFHEIBLK                                                     AM0085
           DFHCOMMAREA                                                  AM0085
           AB04                                                         AM0085
           K21F                                                         AM0085
           MS03                                                         AM0085
           MX11.                                                        AM0085
      *N91ID.    NOTE *IF NO ERRORS - BREAK DOWN K21F     *.            AM0085
       F91ID.    IF    (MS03-NMESS2 = ZEROS                             lv15
                 OR    (MS03-NMESS2 NOT = ZEROS                         AM0085
                 AND   MS03-CMESB < 11))                                AM0085
                 NEXT SENTENCE ELSE GO TO     F91ID-FN.                 AM0085
      *TALLY NUMBER OF 'Y' (TRUE) FLAGS                                 AM0085
           INITIALIZE  TALLI                                            AM0085
           INSPECT     K21F-OUTPUT TALLYING TALLI                       AM0085
           FOR ALL 'Y'.                                                 AM0085
       F91ID-FN. EXIT.
       F91IB-FN. EXIT.
      *N91IP.    NOTE *CALL CI0222 - ACCT OWNER/ROLE      *.            AM0222
       F91IP.                                                           lv10
      *                                                                 AM0222
      *********************************                                 AM0222
      **                                                                AM0222
      ** THIS MODULE WILL READ THE    *                                 AM0222
      ** CONTRACT & CLIENT DATABASES  *                                 AM0222
      ** TO GET ACCT ROLES.           *                                 AM0222
      **                              *                                 AM0222
      *********************************                                 AM0222
      *                                                                 AM0222
           INITIALIZE   X122                                            AM0222
           MOVE        LK70-CTID TO X122-CTID                           AM0222
           MOVE        'Y' TO X122-IPOCH                                AM0222
           SET CI0222-PCB-CT1P-PTR1 TO                                  AM0222
                       PCB-CT1P-PTR1                                    AM0222
           SET CI0222-PCB-CL1P-PTR1 TO                                  AM0222
                       PCB-CL1P-PTR1                                    AM0222
           INITIALIZE      DE10-DU03                                    AM0222
           CALL        CI0222 USING                                     AM0222
           DFHEIBLK                                                     AM0222
           DFHCOMMAREA                                                  AM0222
           DLIUIBII                                                     AM0222
           CI0222-PCB-ADDRESS-LIST                                      AM0222
           X122                                                         AM0222
           DE10                                                         AM0222
           MS03                                                         AM0222
           MX11.                                                        AM0222
       F91IP-FN. EXIT.
      *N91MB.    NOTE *GOOD FUNDS AMOUNT                  *.            AM0265
       F91MB.                                                           lv10
      *                                                                 AM0265
      *********************************                                 AM0265
      ** CALL CI0265 PROGRAM          *                                 AM0265
      *********************************                                 AM0265
      *                                                                 AM0265
           MOVE        NS20-DCACG TO                                    AM0265
           WA65-DCACG                                                   AM0265
           MOVE        CE02-ACCTV8 TO                                   AM0265
           WA65-AACTV                                                   AM0265
           SET CI0265-WA-PCB-CT1P-PTR1 TO                               AM0265
                         PCB-CT1P-PTR1                                  AM0265
           SET CI0265-WA-PCB-CH1P-PTR1 TO                               AM0265
                         PCB-CH1P-PTR1                                  AM0265
           SET CI0265-WA-PCB-SCOP-PTR1 TO                               AM0265
                         PCB-SCOP-PTR1                                  AM0265
      *                                                                 AM0265
           CALL        CI0265 USING                                     AM0265
           DFHEIBLK                                                     AM0265
           DFHCOMMAREA                                                  AM0265
           DLIUIBII                                                     AM0265
           CI0265-WA-PCB-ADDR-LIST                                      AM0265
           WA65                                                         AM0265
           DE10                                                         AM0265
           MS03                                                         AM0265
           MX11.                                                        AM0265
      *N91MC.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F91MC.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F91MC-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0265 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0265 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F91MC-900. GO TO F91MD-FN.
       F91MC-FN. EXIT.
      *N91MD.    NOTE *NO ERRORS                          *.            ADU071
       F91MD.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F91MD-FN. EXIT.
       F91MB-FN. EXIT.
       F91-FN.   EXIT.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *DATE/CLIENT AGE PROCESSING         *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92AB.    NOTE *CDU - DATE VALIDATE/CONVERT        *.            AADA81
       F92AB.                                                           lv10
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
       F92AB-FN. EXIT.
      *N92BB.    NOTE *MACRO AAOAG3  -  CALC CLIENT AGE   *.            AAOAG3
       F92BB.                                                           lv10
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
               GO TO     F92BB-FN.                                      AAOAG3
                 IF    7-OAGE-BIRTH-DATE >                              DOT
                       7-OAGE-CURRENT-DATE                              AAOAG3
      *RETURN CLIENT AGE OF ZERO IF                                     AAOAG3
      *BIRTH DATE > CURRENT DATE                                        AAOAG3
               GO TO     F92BB-FN.                                      AAOAG3
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
       F92BB-FN. EXIT.
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
      *N93SQ.    NOTE *SQL ERROR HANDLING                 *.            ADB221
       F93SQ.                                                           lv10
           MOVE        SQLCODE TO 7-TEST-SQLCODE.                       ADB221
      *N93SR.    NOTE *TEST FOR NORMAL PROCESSING CODE    *.            ADB221
       F93SR.    IF    SQLCODE = +0                                     lv15
                 NEXT SENTENCE ELSE GO TO     F93SR-FN.                 ADB221
           MOVE        '0' TO IK                                        ADB221
           MOVE        ZERO TO 7-Q913-COUNT                             ADB221
               GO TO     F93SQ-FN.                                      ADB221
       F93SR-FN. EXIT.
      *N93SS.    NOTE *CHECK FOR NON-CRITICAL SQLCODE     *.            ADB221
       F93SS.    IF    SQLCODE = +100                                   lv15
                 OR    SQLCODE = -803                                   ADB221
                 OR    SQLCODE = -811                                   ADB221
                 OR    SQLCODE = -904                                   ADB221
                 NEXT SENTENCE ELSE GO TO     F93SS-FN.                 ADB221
           MOVE        ZERO TO 7-Q913-COUNT                             ADB221
           MOVE        '1' TO IK                                        ADB221
               GO TO     F93SQ-FN.                                      ADB221
       F93SS-FN. EXIT.
      *N93ST.    NOTE *CHECK FOR RESOURCE-IN-USE          *.            ADB221
       F93ST.    IF    SQLCODE = -913                                   lv15
                 NEXT SENTENCE ELSE GO TO     F93ST-FN.                 ADB221
      *N93SU.    NOTE *CHECK TO SEE IF ATTEMPT RETRY      *.            ADB221
       F93SU.    IF    7-Q913-COUNT < +0                                lv20
                 AND   7-Q913-COUNT < 7-MAXM-RETRY                      ADB221
                 NEXT SENTENCE ELSE GO TO     F93SU-FN.                 ADB221
           ADD         +1 TO 7-Q913-COUNT                               ADB221
           MOVE        '1' TO IK                                        ADB221
               GO TO     F93SQ-FN.                                      ADB221
       F93SU-FN. EXIT.
       F93ST-FN. EXIT.
      *N93SX.    NOTE **** CRITICAL SQLCODE ** ABEND **   *.            ADB221
       F93SX.                                                           lv15
      *COMMENTED OUT UNTIL PROBLEMS                                     ADB221
      *WITH THIS ARE RESOLVED!!!!!!                                     ADB221
      *CAL DSNTIAR FOR TEXT EXPLANATION                                 ADB221
      *CAL 'DSNTIAR' USING SQLCA                                        ADB221
      *                7-SQLR-MESSAGE                                   ADB221
      *                7-SQLR-TEXT-LEN.                                 ADB221
      *FORMAT CICS ABEND CODE AND ABEND                                 ADB221
           MOVE        SQLCODE TO 7-DB2-ABEND.                          ADB221
                 IF    SQLCODE NEGATIVE                                 DOT
           MOVE        '-' TO 7-DB2-FIRST                               ADB221
                 ELSE                                                   ADB221
           MOVE        '+' TO 7-DB2-FIRST.                              ADB221
           EXEC CICS   ABEND ABCODE (7-DB2-ABEND)                       DOT
                       CANCEL                                END-EXEC.  ADB221
       F93SX-FN. EXIT.
       F93SQ-FN. EXIT.
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
      *N94CC.    NOTE *CALL GU ON CL01                    *.            ADU026
       F94CC.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PC06 CL01                                                    ADU026
           S-CLU01-SSA                                                  ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CC-FN. EXIT.
      *N94CP.    NOTE *CALL GNP ON CL03                   *.            ADU026
       F94CP.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGNP                        ADU026
           PC06 CL03                                                    ADU026
           S-CLU01-SSA S-CL03-SSA                                       ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGNP TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CP-FN. EXIT.
      *N94DA.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94DA.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DA-FN. EXIT.
      *N94GA.    NOTE *CALL GU ON GC01                    *.            ADU026
       F94GA.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 GC01                                                    ADU026
           S-GCU01-SSA                                                  ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94GA-FN. EXIT.
      *N94GB.    NOTE *CALL GN ON GC03                    *.            ADU026
       F94GB.                                                           lv10
           MOVE        'ACAP' TO DE10-XDBDNM                            ADU026
           MOVE        'GC03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PB06 GC03                                                    ADU026
           S-GCU01-SSA S-GC03-SSA                                       ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
                 IF    IK = '0'                                         DOT
           MOVE        '1' TO GC03-CF
                 ELSE
           MOVE        '0' TO GC03-CF.
       F94GB-FN. EXIT.
      *N94HA.    NOTE *CALL GN ON CT09                    *.            ADU026
       F94HA.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT09' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CT09                                                    ADU026
           S-CTU01-SSA S-CTU07-SSA                                      ADU026
           S-CT09-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
                 IF    IK = '0'                                         DOT
           MOVE        '1' TO CT09-CF
                 ELSE
           MOVE        '0' TO CT09-CF.
       F94HA-FN. EXIT.
      *N94JA.    NOTE *CALL GNP ON CT07                   *.            ADU026
       F94JA.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT07' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGNP                        ADU026
           PA06 CT07                                                    ADU026
           S-CTU01-SSA S-CTU07-SSA                                      ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGNP TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
                 IF    IK = '0'                                         DOT
           MOVE        '1' TO CT07-CF
                 ELSE
           MOVE        '0' TO CT07-CF.
       F94JA-FN. EXIT.
       F94MD.                                                           lv10
           MOVE        'F94MD - SELECT' TO 7-DB2-FUNCT                  ADB226
           EXEC SQL    SELECT                                           ADB226
                             CTTNC
                       INTO
                             :CL2B-CTTNC
                       FROM CORP.TBCL2B
                       WHERE CLIDO5 =:CL2B-CLIDO5
                       AND CLIDN =:CL2B-CLIDN
                       AND CPRCP =:CL2B-CPRCP                END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB226
       F94MD-FN. EXIT.
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
