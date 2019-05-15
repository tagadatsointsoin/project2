       IDENTIFICATION DIVISION.                                         CI0435
       PROGRAM-ID.  CI0435P.                                            CI0435
      *AUTHOR.         FA BA ARR DETAIL RULES.                          CI0435
      *DATE-COMPILED.   09/08/14.                                       CI0435
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2011                          *ACOPYP
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
      *     COPR. 2011                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0435
       CONFIGURATION SECTION.                                           CI0435
       SOURCE-COMPUTER. IBM-370.                                        CI0435
       OBJECT-COMPUTER. IBM-370.                                        CI0435
       DATA DIVISION.                                                   CI0435
       WORKING-STORAGE SECTION.                                         CI0435
       01                 CL01.                                         CI0435
            10            CL01-CL01K.                                   CI0435
            11            CL01-C199.                                    CI0435
            12            CL01-CLID.                                    CI0435
            13            CL01-CLIDO  PICTURE  9(3).                    CI0435
            13            CL01-CLIDN.                                   CI0435
            14            CL01-CLIDNP PICTURE  X(12).                   CI0435
            14            CL01-CLIDND PICTURE  9(8).                    CI0435
            10            CL01-GECKD  PICTURE  9.                       CI0435
            10            CL01-GEMDA  PICTURE  9(8).                    CI0435
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0435
                          BINARY.                                       CI0435
            10            CL01-GECUC  PICTURE  99.                      CI0435
            10            CL01-CLDOR  PICTURE  9(8).                    CI0435
            10            CL01-CLLNG  PICTURE  XX.                      CI0435
            10            CL01-GESLC  PICTURE  99.                      CI0435
            10            CL01-CLTYP  PICTURE  X.                       CI0435
            10            CL01-CLCLS  PICTURE  9(3).                    CI0435
            10            CL01-CLTWRC PICTURE  99.                      CI0435
            10            CL01-CLPVC  PICTURE  99.                      CI0435
            10            CL01-CLIND  PICTURE  9(3).                    CI0435
            10            CL01-CLTRC  PICTURE  99.                      CI0435
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0435
                          COMPUTATIONAL-3.                              CI0435
            10            CL01-AYSIDA PICTURE  9(3).                    CI0435
            10            CL01-AYSID  PICTURE  9(5).                    CI0435
            10            CL01-CLSTR  PICTURE  9(2).                    CI0435
            10            CL01-CLC11  PICTURE  X.                       CI0435
            10            CL01-CLTIN  PICTURE  9(12).                   CI0435
            10            CL01-CLTND  PICTURE  9(8).                    CI0435
            10            CL01-CLTINC PICTURE  9.                       CI0435
            10            CL01-CCDWA  PICTURE  9.                       CI0435
            10            CL01-CICES  PICTURE  X.                       CI0435
            10            CL01-CLTRA  PICTURE  9(2).                    CI0435
            10            CL01-DIRSY  PICTURE  9(4)                     CI0435
                          COMPUTATIONAL-3.                              CI0435
            10            CL01-CFEDS  PICTURE  X.                       CI0435
            10            CL01-FILLER PICTURE  X(06).                   CI0435
       01                 CL03.                                         CI0435
            10            CL03-GEDLA  PICTURE  9(8).                    CI0435
            10            CL03-DDREP  PICTURE  9(8).                    CI0435
            10            CL03-DPRFR  PICTURE  9(8).                    CI0435
            10            CL03-IACCI  PICTURE  X.                       CI0435
            10            CL03-CLDOB  PICTURE  9(8).                    CI0435
            10            CL03-CLDOD  PICTURE  9(8).                    CI0435
            10            CL03-CLDTH  PICTURE  X.                       CI0435
            10            CL03-CCINI  PICTURE  X.                       CI0435
            10            CL03-FILLER PICTURE  X(1).                    CI0435
            10            CL03-CLAIN  PICTURE  S9(11)                   CI0435
                          COMPUTATIONAL-3.                              CI0435
            10            CL03-CCAOD  PICTURE  999.                     CI0435
            10            CL03-CLMAR  PICTURE  X.                       CI0435
            10            CL03-C198.                                    CI0435
            11            CL03-CLNAM.                                   CI0435
            12            CL03-CLNAMH PICTURE  X(6).                    CI0435
            12            CL03-CLNAMF PICTURE  X(20).                   CI0435
            12            CL03-CLNAMM.                                  CI0435
            13            CL03-CLNAMI PICTURE  X.                       CI0435
            13            CL03-CLNAMR PICTURE  X(14).                   CI0435
            12            CL03-CLNAML PICTURE  X(25).                   CI0435
            12            CL03-CLNAMS PICTURE  X(4).                    CI0435
            10            CL03-FILLER PICTURE  X(10).                   CI0435
            10            CL03-MPRFS  PICTURE  X(4).                    CI0435
            10            CL03-CLOCC  PICTURE  9(3).                    CI0435
            10            CL03-CLRET  PICTURE  X.                       CI0435
            10            CL03-IOCOB  PICTURE  X.                       CI0435
            10            CL03-CLSEX  PICTURE  X.                       CI0435
            10            CL03-CLWIL  PICTURE  X.                       CI0435
            10            CL03-GECFC  PICTURE  99.                      CI0435
            10            CL03-GECFY  PICTURE  9(4).                    CI0435
            10            CL03-ICUSC  PICTURE  X.                       CI0435
            10            CL03-MCTYC  PICTURE  X(20).                   CI0435
            10            CL03-CLWIP  PICTURE  X.                       CI0435
            10            CL03-CLCTXF PICTURE  99.                      CI0435
            10            CL03-CLCUS  PICTURE  99.                      CI0435
            10            CL03-NPDLU  PICTURE  9(5).                    CI0435
            10            CL03-CLEMI  PICTURE  X.                       CI0435
            10            CL03-GEPHNH PICTURE  X(14).                   CI0435
            10            CL03-GEPHNB PICTURE  X(14).                   CI0435
            10            CL03-GEPHNX PICTURE  9(4).                    CI0435
            10            CL03-GEPHNA PICTURE  X(14).                   CI0435
            10            CL03-FILLER PICTURE  X(3).                    CI0435
            10            CL03-IAPRT  PICTURE  X.                       CI0435
            10            CL03-CEMSC  PICTURE  X.                       CI0435
            10            CL03-CSEPS  PICTURE  X.                       CI0435
            10            CL03-CRACE  PICTURE  X.                       CI0435
            10            CL03-CNIRA  PICTURE  X.                       CI0435
            10            CL03-FILLER PICTURE  X(11).                   CI0435
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0003           PIC X(8) VALUE 'CI0003P '.                  AM0003
       01  CI0004           PIC X(8) VALUE 'CI0004P '.                  AM0004
       01  CI0018           PIC X(8) VALUE 'CI0018P '.                  AM0018
       01  CI0085           PIC X(8) VALUE 'CI0085P '.                  AM0085
       01                 CT01.                                         CI0435
            10            CT01-CT01K.                                   CI0435
            11            CT01-C299.                                    CI0435
            12            CT01-CTID.                                    CI0435
            13            CT01-CTIDA  PICTURE  9(3).                    CI0435
            13            CT01-CTIDN.                                   CI0435
            14            CT01-CTIDNP PICTURE  X(13).                   CI0435
            14            CT01-CTIDND PICTURE  9(11).                   CI0435
            10            CT01-GECKD  PICTURE  9.                       CI0435
            10            CT01-GEMDA  PICTURE  9(8).                    CI0435
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0435
                          BINARY.                                       CI0435
            10            CT01-GECUC  PICTURE  99.                      CI0435
            10            CT01-CTAUL  PICTURE  9(3).                    CI0435
            10            CT01-DIRAC  PICTURE  9(4).                    CI0435
            10            CT01-CTCCI  PICTURE  X.                       CI0435
            10            CT01-CTCUS  PICTURE  999.                     CI0435
            10            CT01-CTEFD  PICTURE  9(8).                    CI0435
            10            CT01-CTIAD  PICTURE  9(8).                    CI0435
            10            CT01-CLCUS  PICTURE  99.                      CI0435
            10            CT01-CAMMB  PICTURE  X(3).                    CI0435
            10            CT01-CKPMM  PICTURE  X.                       CI0435
            10            CT01-CTLAD  PICTURE  9(8).                    CI0435
            10            CT01-IPERS  PICTURE  X.                       CI0435
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0435
                          COMPUTATIONAL-3.                              CI0435
            10            CT01-CTLAT  PICTURE  9(8).                    CI0435
            10            CT01-CTLATC PICTURE  9(6).                    CI0435
            10            CT01-IMEGA  PICTURE  X.                       CI0435
            10            CT01-DIRAB  PICTURE  9(8).                    CI0435
            10            CT01-COLRQ  PICTURE  X.                       CI0435
            10            CT01-ZDA04  PICTURE  X(4).                    CI0435
            10            CT01-CTLPD  PICTURE  9(8).                    CI0435
            10            CT01-CIRASP PICTURE  9.                       CI0435
            10            CT01-CIRATP PICTURE  99.                      CI0435
            10            CT01-DRTHC  PICTURE  9(8).                    CI0435
            10            CT01-CPPTC  PICTURE  X.                       CI0435
            10            CT01-ZDA06  PICTURE  X(6).                    CI0435
            10            CT01-CTACD  PICTURE  9(8).                    CI0435
            10            CT01-CTNLI  PICTURE  X.                       CI0435
            10            CT01-CTRHO  PICTURE  9(8).                    CI0435
            10            CT01-CTSGD  PICTURE  9(8).                    CI0435
            10            CT01-CPATP  PICTURE  X(1).                    CI0435
            10            CT01-IRSTA  PICTURE  X.                       CI0435
            10            CT01-CTSTA  PICTURE  99.                      CI0435
            10            CT01-CTSSC  PICTURE  99.                      CI0435
            10            CT01-PRLIN  PICTURE  9(3).                    CI0435
            10            CT01-PRCOD  PICTURE  9(5).                    CI0435
            10            CT01-PRSCD  PICTURE  X(9).                    CI0435
            10            CT01-CTLNI  PICTURE  X.                       CI0435
            10            CT01-AYSIDA PICTURE  9(3).                    CI0435
            10            CT01-AYSID  PICTURE  9(5).                    CI0435
            10            CT01-CTBMC  PICTURE  99.                      CI0435
            10            CT01-CINAR  PICTURE  99.                      CI0435
            10            CT01-CPHTR  PICTURE  X.                       CI0435
            10            CT01-CDSTR  PICTURE  XX.                      CI0435
            10            CT01-CQACT  PICTURE  999.                     CI0435
            10            CT01-CIRAS  PICTURE  999.                     CI0435
            10            CT01-CIRAT  PICTURE  999.                     CI0435
            10            CT01-CLRAY  PICTURE  9(5).                    CI0435
            10            CT01-CATTP  PICTURE  X.                       CI0435
       01                 CT49.                                         CI0435
            10            CT49-CT49K.                                   CI0435
            11            CT49-GESTD  PICTURE  9(8).                    CI0435
            10            CT49-CTOWN  PICTURE  9(3).                    CI0435
            10            CT49-FILLER PICTURE  X.                       CI0435
      ******************************************************            AADA82
      ****      WORK AREAS FOR COMMON DATE UTILITY       ***            AADA82
      ******************************************************            AADA82
      **                                                                AADA82
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA82
      **                                                                AADA82
      **   SEGMENT DD30 - FUNCTION LAYOUT                               AADA82
      **                                                                AADA82
      *!WF DSP=DF DSL=DD SEL=30 FOR=I DES=2 LEV=1                       AADA82
       01                 DF30.                                         CI0435
            10            DF30-CDTFN  PICTURE  9(4)                     CI0435
                          VALUE                ZERO.                    CI0435
            10            DF30-CDTSF  PICTURE  9(4)                     CI0435
                          VALUE                ZERO.                    CI0435
            10            DF30-CDTSC  PICTURE  9(4)                     CI0435
                          VALUE                ZERO.                    CI0435
            10            DF30-FILLER PICTURE  X(40)                    CI0435
                          VALUE                SPACE.                   CI0435
       01                 DF34.                                         CI0435
            10            DF34-CAINS  PICTURE  X(03)                    CI0435
                          VALUE                SPACE.                   CI0435
            10            DF34-CDTUC  PICTURE  9                        CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-NDTUN  PICTURE  S9(05)                   CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-FILLER PICTURE  X(162)                   CI0435
                          VALUE                SPACE.                   CI0435
            10            DF34-DTGRG.                                   CI0435
            11            DF34-DTGCY.                                   CI0435
            12            DF34-DTGCC  PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            12            DF34-DTGYY  PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            11            DF34-DTGMM  PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            11            DF34-DTGDD  PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-DTJUL.                                   CI0435
            11            DF34-DTJCY.                                   CI0435
            12            DF34-DTJCC  PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            12            DF34-DTJYY  PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            11            DF34-DTJDD  PICTURE  9(3)                     CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTFM  PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTLM  PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTFF  PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTLF  PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTFW  PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTLW  PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CCDOWA PICTURE  9                        CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CCDRW  PICTURE  9                        CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-FILLER PICTURE  X(58)                    CI0435
                          VALUE                SPACE.                   CI0435
            10            DF34-DTGRGA.                                  CI0435
            11            DF34-DTGCYA.                                  CI0435
            12            DF34-DTGCCA PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            12            DF34-DTGYYA PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            11            DF34-DTGMMA PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            11            DF34-DTGDDA PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-DTJULA.                                  CI0435
            11            DF34-DTJCYA.                                  CI0435
            12            DF34-DTJCCA PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            12            DF34-DTJYYA PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            11            DF34-DTJDDA PICTURE  9(3)                     CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTFMA PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTLMA PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTFFA PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTLFA PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTFWA PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTLWA PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CCDOWB PICTURE  9                        CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CCDRWA PICTURE  9                        CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-FILLER PICTURE  X(58)                    CI0435
                          VALUE                SPACE.                   CI0435
            10            DF34-DTGRGB.                                  CI0435
            11            DF34-DTGCYB.                                  CI0435
            12            DF34-DTGCCB PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            12            DF34-DTGYYB PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            11            DF34-DTGMMB PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            11            DF34-DTGDDB PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-DTJULB.                                  CI0435
            11            DF34-DTJCYB.                                  CI0435
            12            DF34-DTJCCB PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            12            DF34-DTJYYB PICTURE  9(2)                     CI0435
                          VALUE                ZERO.                    CI0435
            11            DF34-DTJDDB PICTURE  9(3)                     CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTFMB PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTLMB PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTFFB PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTLFB PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTFWB PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CDTLWB PICTURE  9(01)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CCDOWC PICTURE  9                        CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-CCDRWB PICTURE  9                        CI0435
                          VALUE                ZERO.                    CI0435
            10            DF34-FILLER PICTURE  X(58)                    CI0435
                          VALUE                SPACE.                   CI0435
            10            DF34-FILLER PICTURE  X(40)                    CI0435
                          VALUE                SPACE.                   CI0435
      **                                                                AADA82
      **   SEGMENT DD34 - CONVERT DATE LAYOUT                           AADA82
      **                                                                AADA82
      *!WF DSP=DF DSL=DD SEL=34 FOR=I DES=2 LEV=1                       AADA82
      **                                                                AADA82
      ******************************************************************ACMCTI
      *WORKING STORAGE SEGMENT FOR STORING THE LINKAGE DATA FOR CI0361. ACMCTI
      ******************************************************************ACMCTI
      *!WF DSP=I9 DSL=K9 SEL=3B FOR=I DES=2 LEV=1                       ACMCTI
       01                 I93B.                                         CI0435
            10            I93B-CEADC  PICTURE  X                        CI0435
                          VALUE                SPACE.                   CI0435
            10            I93B-DACTT  PICTURE  X(10)                    CI0435
                          VALUE                SPACE.                   CI0435
            10            I93B-GEOPDC PICTURE  X(8)                     CI0435
                          VALUE                SPACE.                   CI0435
            10            I93B-GEOPDB PICTURE  X(8)                     CI0435
                          VALUE                SPACE.                   CI0435
            10            I93B-CAEMCE PICTURE  X(8)                     CI0435
                          VALUE                SPACE.                   CI0435
            10            I93B-CAEMCD PICTURE  X(8)                     CI0435
                          VALUE                SPACE.                   CI0435
            10            I93B-GETIMM PICTURE  X(8)                     CI0435
                          VALUE                SPACE.                   CI0435
            10            I93B-CRTNC  PICTURE  S9(9)                    CI0435
                          VALUE                ZERO                     CI0435
                          COMPUTATIONAL-3.                              CI0435
            10            I93B-GERTC  PICTURE  X                        CI0435
                          VALUE                SPACE.                   CI0435
            10            I93B-DXTMST PICTURE  X(26)                    CI0435
                          VALUE                SPACE.                   CI0435
            10            I93B-DXTMS2 PICTURE  X(26)                    CI0435
                          VALUE                SPACE.                   CI0435
                                                                        ACMCTI
      ******************************************************************ACMCTI
      *WORKING STORAGE FIELD TO STORE THE NAME OF THE CALLED MODULE.    ACMCTI
      ******************************************************************ACMCTI
       01          CI0361       PIC X(08)                               ACMCTI
                                VALUE 'CI0361P'.                        ACMCTI
                                                                        ACMCTI
      ******************************************************************ACMCTI
      *WORKING STORAGE FIELD TO STORE THE DATE FOR THE DUMMY DB2 CALL.  ACMCTI
      ******************************************************************ACMCTI
       01          WS00-DATE    PIC X(10)     VALUE SPACES.             ACMCTI
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0435
            10            XW05-XW06.                                    CI0435
            11            XW05-XDBPCB.                                  CI0435
            12            XW05-XDBDNM PICTURE  X(08)                    CI0435
                          VALUE                SPACE.                   CI0435
            12            XW05-XSEGLV PICTURE  X(02)                    CI0435
                          VALUE                SPACE.                   CI0435
            12            XW05-XRC    PICTURE  X(02)                    CI0435
                          VALUE                SPACE.                   CI0435
            12            XW05-XPROPT PICTURE  X(04)                    CI0435
                          VALUE                SPACE.                   CI0435
            12            XW05-FILLER PICTURE  S9(5)                    CI0435
                          VALUE                ZERO                     CI0435
                          BINARY.                                       CI0435
            12            XW05-XSEGNM PICTURE  X(08)                    CI0435
                          VALUE                SPACE.                   CI0435
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0435
                          VALUE                ZERO                     CI0435
                          BINARY.                                       CI0435
            12            XW05-XSEGNB PICTURE  9(05)                    CI0435
                          VALUE                ZERO                     CI0435
                          BINARY.                                       CI0435
            12            XW05-XCOKEY PICTURE  X(70)                    CI0435
                          VALUE                SPACE.                   CI0435
            10            XW05-XW07.                                    CI0435
            11            XW05-XIOPCB.                                  CI0435
            12            XW05-XTERMI PICTURE  X(08)                    CI0435
                          VALUE                SPACE.                   CI0435
            12            XW05-FILLER PICTURE  XX                       CI0435
                          VALUE                SPACE.                   CI0435
            12            XW05-XRC1   PICTURE  X(02)                    CI0435
                          VALUE                SPACE.                   CI0435
            12            XW05-FILLER PICTURE  X(12)                    CI0435
                          VALUE                SPACE.                   CI0435
            12            XW05-XMODNM PICTURE  X(8)                     CI0435
                          VALUE                SPACE.                   CI0435
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0435
                          VALUE                ZERO.                    CI0435
            10            XW05-XGU    PICTURE  X(4)                     CI0435
                          VALUE                'GU  '.                  CI0435
            10            XW05-XGHU   PICTURE  X(4)                     CI0435
                          VALUE                'GHU '.                  CI0435
            10            XW05-XGN    PICTURE  X(4)                     CI0435
                          VALUE                'GN  '.                  CI0435
            10            XW05-XGHN   PICTURE  X(4)                     CI0435
                          VALUE                'GHN '.                  CI0435
            10            XW05-XGNP   PICTURE  X(4)                     CI0435
                          VALUE                'GNP '.                  CI0435
            10            XW05-XGHNP  PICTURE  X(4)                     CI0435
                          VALUE                'GHNP'.                  CI0435
            10            XW05-XREPL  PICTURE  XXXX                     CI0435
                          VALUE                'REPL'.                  CI0435
            10            XW05-XISRT  PICTURE  X(4)                     CI0435
                          VALUE                'ISRT'.                  CI0435
            10            XW05-XDLET  PICTURE  X(4)                     CI0435
                          VALUE                'DLET'.                  CI0435
            10            XW05-XOPEN  PICTURE  X(4)                     CI0435
                          VALUE                'OPEN'.                  CI0435
            10            XW05-XCLSE  PICTURE  X(4)                     CI0435
                          VALUE                'CLSE'.                  CI0435
            10            XW05-XCHKP  PICTURE  X(4)                     CI0435
                          VALUE                'CHKP'.                  CI0435
            10            XW05-XXRST  PICTURE  X(4)                     CI0435
                          VALUE                'XRST'.                  CI0435
            10            XW05-XTERM  PICTURE  X(4)                     CI0435
                          VALUE                'TERM'.                  CI0435
            10            XW05-XNFPAC PICTURE  X(13)                    CI0435
                          VALUE                SPACE.                   CI0435
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0435
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0435
      *                                                                 ADU155
      ******************************************************************ADU155
      ** WORK AREA NEEDED FOR MACRO ADU155                             *ADU155
      **        DATE COMMON AREA FOR EXECUTING CICS ASKTIME/FORMATTIME *ADU155
      ******************************************************************ADU155
      *                                                                 ADU155
      *!WI pl=DT100                                                     ADU155
       01  DT01-XMSTS                                                   ADU155
                        PICTURE S9(15)                                  CI0435
                          COMPUTATIONAL-3.                              CI0435
       01  DT01-F2CCYY             PIC S9(08) COMP.                     ADU155
      *!WI pl=DT200                                                     ADU155
       01  DT01-XDAT69                                                  ADU155
                        PICTURE 9(6).                                   CI0435
       01  DT01-UDATE.                                                  ADU155
           05  DT01-YEAR           PIC  9(04).                          ADU155
           05  DT01-MMDD           PIC  9(04).                          ADU155
      *!WI pl=DT280                                                     ADU155
       01  DT01-XDATCU REDEFINES DT01-UDATE                             ADU155
                        PICTURE X(8).                                   CI0435
      *                                                                 ADU155
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
                                                                        AM0003
      ******************************************************************AM0003
      **     PCB ADDRESS LIST FOR CI0003.  MODULE CI0003 WILL NEED     *AM0003
      **     PCB'S FOR:                                                *AM0003
      **                CONTRACT DATABASE(CT1P)                        *AM0003
      ******************************************************************AM0003
                                                                        AM0003
       01  CI0003-PCB-ADDRESS-LIST.                                     AM0003
           05  CI0003-PCB-CT1P-PTR1      POINTER.                       AM0003
                                                                        AM0004
      ******************************************************************AM0004
      **     PCB ADDRESS LIST FOR CI0004.  MODULE CI0003 WILL NEED     *AM0004
      **     PCB'S FOR:                                                *AM0004
      **                CLIENT DATABASE(CL1P)                          *AM0004
      **                CONTRACT DATABASE(CT1P)                        *AM0004
      ******************************************************************AM0004
                                                                        AM0004
       01  CI0004-PCB-ADDRESS-LIST.                                     AM0004
           05  CI0004-PCB-CL1P-PTR1      POINTER.                       AM0004
           05  CI0004-PCB-CT1P-PTR1      POINTER.                       AM0004
                                                                        AM0018
      ******************************************************************AM0018
      **     PCB ADDRESS LIST FOR CI0018.  MODULE CI0018 WILL NEED     *AM0018
      **     PCB'S FOR:                                                *AM0018
      **                CONTRACT DATABASE(CT1P)                        *AM0018
      ******************************************************************AM0018
                                                                        AM0018
       01  CI0018-PCB-ADDRESS-LIST.                                     AM0018
           05  CI0018-PCB-CT1P-PTR1      POINTER.                       AM0018
      ******************************************************************AM0085
      ** LINKAGE SEGMENT FOR CI0085(OWNERSHIP INFORMATION)             *AM0085
      ******************************************************************AM0085
      **                                                                AM0085
      *!WF DSP=PF DSL=K1 SEL=1F FOR=I LEV=1                             AM0085
       01                 PF00.                                         CI0435
          05              PF00-SUITE.                                   CI0435
            15       FILLER         PICTURE  X(00266).                  CI0435
       01                 PF1F  REDEFINES      PF00.                    CI0435
            10            PF1F-INPUT.                                   CI0435
            11            PF1F-MAPPN  PICTURE  X(10).                   CI0435
            11            PF1F-PROGR  PICTURE  X(06).                   CI0435
            11            PF1F-ADDRLN.                                  CI0435
            12            PF1F-GESAD1 PICTURE  X(30).                   CI0435
            12            PF1F-GESAD2 PICTURE  X(30).                   CI0435
            12            PF1F-GESAD3 PICTURE  X(30).                   CI0435
            11            PF1F-FILLER PICTURE  X(100).                  CI0435
            10            PF1F-OUTPUT.                                  CI0435
            11            PF1F-IOWNC  PICTURE  X                        CI0435
                          OCCURS       060     TIMES.                   CI0435
      **                                                                AM0085
      *!WF DSP=PA DSL=DU SEL=04 FOR=I DES=1 LEV=1                       AM0085
       01                 PA04.                                         CI0435
            10            PA04-C299.                                    CI0435
            11            PA04-CTID.                                    CI0435
            12            PA04-CTIDA  PICTURE  9(3).                    CI0435
            12            PA04-CTIDN.                                   CI0435
            13            PA04-CTIDNP PICTURE  X(13).                   CI0435
            13            PA04-CTIDND PICTURE  9(11).                   CI0435
            10            PA04-IPOCH  PICTURE  X.                       CI0435
            10            PA04-FILLER PICTURE  X(099).                  CI0435
            10            PA04-CTTLN1 PICTURE  X(30).                   CI0435
            10            PA04-CTTLN2 PICTURE  X(30).                   CI0435
            10            PA04-CTTLN3 PICTURE  X(30).                   CI0435
            10            PA04-CTTBO1 PICTURE  X(45).                   CI0435
            10            PA04-CTTBO2 PICTURE  X(45).                   CI0435
            10            PA04-CTOWN  PICTURE  9(3).                    CI0435
            10            PA04-IUGMA  PICTURE  X.                       CI0435
            10            PA04-FILLER PICTURE  X(096).                  CI0435
      **                                                                AM0085
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
                        PICTURE X(66)                                   CI0435
                           VALUE 'DB2 RESOURCE IN USE. TRY AGAIN '.     ADB221
       01               7-MAXM-RETRY     PIC S9(3) COMP-3               ADB221
                                                   VALUE +001.          ADB221
      ******************************************************************
      **                PROCESSING CONTROL SWITCHES                    *
      ******************************************************************
       01    WS00-FIELDS.
          05    WS00-FIRST            PIC X    VALUE 'Y'.
          05    I1                    PIC 999  VALUE ZERO.
          05  WS00-EIBTIME            PIC 9(07).
          05  FILLER REDEFINES   WS00-EIBTIME.
              10  FILLER              PIC X(01).
              10  WS00-HH             PIC X(02).
              10  FILLER              PIC X(04).
          05  WS00-CURRDATE           PIC 9(08).
      *!WI
          05  WS00-CLID
                        PICTURE X(23).                                  CI0435
      *!WI
          05  WS00-TAXPAYER-CLID
                        PICTURE X(23).                                  CI0435
      *!WI
          05  WS00-CLCTRC
                        PICTURE 9(3).                                   CI0435
          05  WS00-AGE                PIC 999.
          05  WS00-TP-AGE             PIC 999.
      *!WI
          05  WS00-PROGR
                        PICTURE X(06).                                  CI0435
          05  ACCT-DEAD               PIC X    VALUE SPACE.
      *!WI
          05  WS00-CLTIN
                        PICTURE 9(12).                                  CI0435
      *!WI
          05  WS00-CLTINC
                        PICTURE 9.                                      CI0435
      *!WI
       01 WS00-GECTRY
                        PICTURE X(20).                                  CI0435
           88 COUNTRY-US     VALUES ' ' 'US' 'USA'.
      *CLIENT CUSTODIAN INDICATOR.
       01 WS00-CUSTODIAN  PIC X VALUE SPACE.
      *TEMP DATE FOR TAX REPORTING YEAR CALCULATION.
      *!WI
       01 WS00-GESTD
                        PICTURE 9(8).                                   CI0435
      *
      *WORKING STORAGE FOR MARKET CLOSE TIME
      *!WI
       01  WS-GETIMM
                        PICTURE X(8).                                   CI0435
       01  WS-GETIMM-RED REDEFINES WS-GETIMM.
           05  WS-HH      PIC XX.
           05  WS-DOT1    PIC X.
           05  WS-MM      PIC XX.
           05  WS-DOT2    PIC X.
           05  WS-SS      PIC XX.
      *!WI
       01  WS-GETIM6
                        PICTURE 9(06).                                  CI0435
       01  WS-MKT-TIME  REDEFINES WS-GETIM6.
           05  WS-MKT-HH   PIC 99.
           05  WS-MKT-MM   PIC 99.
           05  WS-MKT-SS   PIC 99.
      *!WI
       01  WS01-CLID
                        PICTURE X(23).                                  CI0435
      *WORKING STORAGE FOR THE NUMBER OF CLIENT ID
       01  I2             PIC 99  VALUE ZERO.
      *                                                                 AMDU07
      ******************************************************************AMDU07
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU07
      **     ACCOUNT'S ADDRESS.  IT ALSO CONTAINS THE CL24 SEGMENT.    *AMDU07
      ******************************************************************AMDU07
      *                                                                 AMDU07
      *!WF DSP=PB DSL=DU SEL=07 FOR=I LEV=1                             AMDU07
       01                 PB00.                                         CI0435
          05              PB00-SUITE.                                   CI0435
            15       FILLER         PICTURE  X(00437).                  CI0435
       01                 PB07  REDEFINES      PB00.                    CI0435
            10            PB07-C299.                                    CI0435
            11            PB07-CTID.                                    CI0435
            12            PB07-CTIDA  PICTURE  9(3).                    CI0435
            12            PB07-CTIDN.                                   CI0435
            13            PB07-CTIDNP PICTURE  X(13).                   CI0435
            13            PB07-CTIDND PICTURE  9(11).                   CI0435
            10            PB07-DCACG  PICTURE  9(8).                    CI0435
            10            PB07-FILLER PICTURE  X(100).                  CI0435
            10            PB07-CL24.                                    CI0435
            11            PB07-GELL   PICTURE  9(4)                     CI0435
                          BINARY.                                       CI0435
            11            PB07-CL24K.                                   CI0435
            12            PB07-GECSQ  PICTURE  S9(3)                    CI0435
                          COMPUTATIONAL-3.                              CI0435
            11            PB07-GECSD  PICTURE  9(8).                    CI0435
            11            PB07-GECED  PICTURE  9(8).                    CI0435
            11            PB07-CREQ2  PICTURE  X.                       CI0435
            11            PB07-FILLER PICTURE  X(4).                    CI0435
            11            PB07-GECTA  PICTURE  X.                       CI0435
            11            PB07-GELCD  PICTURE  9(8).                    CI0435
            11            PB07-GEADS  PICTURE  9.                       CI0435
            11            PB07-GECIT  PICTURE  X(25).                   CI0435
            11            PB07-GECTRY PICTURE  X(20).                   CI0435
            11            PB07-GECTY  PICTURE  9(3).                    CI0435
            11            PB07-GEPCD  PICTURE  X(12).                   CI0435
            11            PB07-GEST   PICTURE  X(8).                    CI0435
            11            PB07-IRESA  PICTURE  X.                       CI0435
            11            PB07-FILLER PICTURE  X(8).                    CI0435
            11            PB07-GESAD  PICTURE  X(30)                    CI0435
                          OCCURS       003     TIMES.                   CI0435
            10            PB07-FILLER PICTURE  X(100).                  CI0435
      *                                                                 AMDU07
      *                                                                 AMDU07
      *                                                                 AMDU07
      *                                                                 AMDU07
      *                                                                 AMDU14
      ******************************************************************AMDU14
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU14
      **     REQUESTED TYPE OF CLIENTS FOR THE ACCOUNT NUMBER PASSED.  *AMDU14
      ******************************************************************AMDU14
      *                                                                 AMDU14
      *!WF DSP=PC DSL=DU SEL=14 FOR=I LEV=1                             AMDU14
       01                 PC00.                                         CI0435
          05              PC00-SUITE.                                   CI0435
            15       FILLER         PICTURE  X(00917).                  CI0435
       01                 PC14  REDEFINES      PC00.                    CI0435
            10            PC14-C299.                                    CI0435
            11            PC14-CTID.                                    CI0435
            12            PC14-CTIDA  PICTURE  9(3).                    CI0435
            12            PC14-CTIDN.                                   CI0435
            13            PC14-CTIDNP PICTURE  X(13).                   CI0435
            13            PC14-CTIDND PICTURE  9(11).                   CI0435
            10            PC14-DCACG  PICTURE  9(8).                    CI0435
            10            PC14-IPOCH  PICTURE  X.                       CI0435
            10            PC14-FILLER PICTURE  X(100).                  CI0435
            10            PC14-CLID01.                                  CI0435
            11            PC14-CLIDO1 PICTURE  X(3).                    CI0435
            11            PC14-NCLID1.                                  CI0435
            12            PC14-CLIDP1 PICTURE  X(12).                   CI0435
            12            PC14-CLIDNA PICTURE  9(8).                    CI0435
            10            PC14-CLCTR  PICTURE  9(3).                    CI0435
            10            PC14-DU21                                     CI0435
                          OCCURS       025     TIMES.                   CI0435
            11            PC14-C199.                                    CI0435
            12            PC14-CLID.                                    CI0435
            13            PC14-CLIDO  PICTURE  9(3).                    CI0435
            13            PC14-CLIDN.                                   CI0435
            14            PC14-CLIDNP PICTURE  X(12).                   CI0435
            14            PC14-CLIDND PICTURE  9(8).                    CI0435
            11            PC14-CLCTRC PICTURE  9(3).                    CI0435
            10            PC14-QITEM  PICTURE  9(3).                    CI0435
            10            PC14-XIMAX  PICTURE  S9(4)                    CI0435
                          BINARY.                                       CI0435
            10            PC14-CRROL  PICTURE  X.                       CI0435
            10            PC14-FILLER PICTURE  X(099).                  CI0435
      *                                                                 AMDU14
      *                                                                 AMDU14
      *                                                                 AMDU14
      *                                                                 AMDU14
       01   DEBUT-WSS.                                                  CI0435
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0435
            05   IK     PICTURE X.                                      CI0435
       01  CONSTANTES-PAC.                                              CI0435
           05  FILLER  PICTURE X(87)   VALUE                            CI0435
                     '6015 CAT09/08/14CI0435ADMIN   14:35:27CI0435P AMERCI0435
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0435
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0435
           05  NUGNA   PICTURE X(5).                                    CI0435
           05  APPLI   PICTURE X(3).                                    CI0435
           05  DATGN   PICTURE X(8).                                    CI0435
           05  PROGR   PICTURE X(6).                                    CI0435
           05  CODUTI  PICTURE X(8).                                    CI0435
           05  TIMGN   PICTURE X(8).                                    CI0435
           05  PROGE   PICTURE X(8).                                    CI0435
           05  COBASE  PICTURE X(4).                                    CI0435
           05  DATGNC  PICTURE X(10).                                   CI0435
           05  RELEAS  PICTURE X(7).                                    CI0435
           05  DATGE   PICTURE X(10).                                   CI0435
           05  DATSQ   PICTURE X(10).                                   CI0435
       01  DATCE.                                                       CI0435
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0435
         05  DATOR.                                                     CI0435
           10  DATOA  PICTURE XX.                                       CI0435
           10  DATOM  PICTURE XX.                                       CI0435
           10  DATOJ  PICTURE XX.                                       CI0435
       01   VARIABLES-CONDITIONNELLES.                                  CI0435
            05                  FT      PICTURE X VALUE '0'.            CI0435
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0435
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0435
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0435
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0435
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0435
       01               S-CL01-SSA.                                     CI0435
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0435
                                      VALUE 'CL01    '.                 CI0435
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0435
            10          S-CL01-CCOD   PICTURE X(5)                      CI0435
                                      VALUE '-----'.                    CI0435
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0435
       01            S-CLU01-SSA.                                       CI0435
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0435
                                      VALUE 'CL01    '.                 CI0435
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0435
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0435
                                      VALUE '-----'.                    CI0435
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0435
                                      VALUE '(CL01K'.                   CI0435
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0435
            10       S-CLU01-CL01K.                                     CI0435
            11       S-CLU01-C199.                                      CI0435
            12       S-CLU01-CLID.                                      CI0435
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0435
            13       S-CLU01-CLIDN.                                     CI0435
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0435
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0435
            10  FILLER   PICTURE X    VALUE ')'.                        CI0435
       01               S-CL03-SSA.                                     CI0435
            10         S1-CL03-SEGNAM PICTURE X(8)                      CI0435
                                      VALUE 'CL03    '.                 CI0435
            10         S1-CL03-CCOM   PICTURE X VALUE '*'.              CI0435
            10          S-CL03-CCOD   PICTURE X(5)                      CI0435
                                      VALUE '-----'.                    CI0435
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0435
       01            S-CLA03-SSA.                                       CI0435
            10      S1-CLA03-SEGNAM PICTURE X(8)                        CI0435
                                      VALUE 'CL03    '.                 CI0435
            10      S1-CLA03-CCOM   PICTURE X VALUE '*'.                CI0435
            10       S-CLA03-CCOD   PICTURE X(5)                        CI0435
                                      VALUE '-----'.                    CI0435
            10      S1-CLA03-FLDNAM PICTURE X(9)                        CI0435
                                      VALUE '(CLDOD'.                   CI0435
            10       S-CLA03-OPER  PICTURE XX VALUE ' ='.               CI0435
            10       S-CLA03-CLDOD    PICTURE  9(8).                    CI0435
            10  FILLER   PICTURE X    VALUE ')'.                        CI0435
       01               S-CT01-SSA.                                     CI0435
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0435
                                      VALUE 'CT01    '.                 CI0435
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0435
            10          S-CT01-CCOD   PICTURE X(5)                      CI0435
                                      VALUE '-----'.                    CI0435
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0435
       01            S-CTU01-SSA.                                       CI0435
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0435
                                      VALUE 'CT01    '.                 CI0435
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0435
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0435
                                      VALUE '-----'.                    CI0435
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0435
                                      VALUE '(CT01K'.                   CI0435
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0435
            10       S-CTU01-CT01K.                                     CI0435
            11       S-CTU01-C299.                                      CI0435
            12       S-CTU01-CTID.                                      CI0435
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0435
            13       S-CTU01-CTIDN.                                     CI0435
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0435
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0435
            10  FILLER   PICTURE X    VALUE ')'.                        CI0435
       01               S-CT49-SSA.                                     CI0435
            10         S1-CT49-SEGNAM PICTURE X(8)                      CI0435
                                      VALUE 'CT49    '.                 CI0435
            10         S1-CT49-CCOM   PICTURE X VALUE '*'.              CI0435
            10          S-CT49-CCOD   PICTURE X(5)                      CI0435
                                      VALUE '-----'.                    CI0435
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0435
       01            S-CTU49-SSA.                                       CI0435
            10      S1-CTU49-SEGNAM PICTURE X(8)                        CI0435
                                      VALUE 'CT49    '.                 CI0435
            10      S1-CTU49-CCOM   PICTURE X VALUE '*'.                CI0435
            10       S-CTU49-CCOD   PICTURE X(5)                        CI0435
                                      VALUE '-----'.                    CI0435
            10      S1-CTU49-FLDNAM PICTURE X(9)                        CI0435
                                      VALUE '(CT49K'.                   CI0435
            10       S-CTU49-OPER  PICTURE XX VALUE ' ='.               CI0435
            10       S-CTU49-CT49K.                                     CI0435
            11       S-CTU49-GESTD    PICTURE  9(8).                    CI0435
            10  FILLER   PICTURE X    VALUE ')'.                        CI0435
       01   ZONES-UTILISATEUR PICTURE X.                                CI0435
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
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=XL DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XL00.                                         CI0435
          05              XL00-SUITE.                                   CI0435
            15       FILLER         PICTURE  X(00106).                  CI0435
       01                 XL06  REDEFINES      XL00.                    CI0435
            10            XL06-XDBPCB.                                  CI0435
            11            XL06-XDBDNM PICTURE  X(08).                   CI0435
            11            XL06-XSEGLV PICTURE  X(02).                   CI0435
            11            XL06-XRC    PICTURE  X(02).                   CI0435
            11            XL06-XPROPT PICTURE  X(04).                   CI0435
            11            XL06-FILLER PICTURE  S9(5)                    CI0435
                          BINARY.                                       CI0435
            11            XL06-XSEGNM PICTURE  X(08).                   CI0435
            11            XL06-XKEYLN PICTURE  S9(05)                   CI0435
                          BINARY.                                       CI0435
            11            XL06-XSEGNB PICTURE  9(05)                    CI0435
                          BINARY.                                       CI0435
            11            XL06-XCOKEY PICTURE  X(70).                   CI0435
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=XM DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XM00.                                         CI0435
          05              XM00-SUITE.                                   CI0435
            15       FILLER         PICTURE  X(00106).                  CI0435
       01                 XM06  REDEFINES      XM00.                    CI0435
            10            XM06-XDBPCB.                                  CI0435
            11            XM06-XDBDNM PICTURE  X(08).                   CI0435
            11            XM06-XSEGLV PICTURE  X(02).                   CI0435
            11            XM06-XRC    PICTURE  X(02).                   CI0435
            11            XM06-XPROPT PICTURE  X(04).                   CI0435
            11            XM06-FILLER PICTURE  S9(5)                    CI0435
                          BINARY.                                       CI0435
            11            XM06-XSEGNM PICTURE  X(08).                   CI0435
            11            XM06-XKEYLN PICTURE  S9(05)                   CI0435
                          BINARY.                                       CI0435
            11            XM06-XSEGNB PICTURE  9(05)                    CI0435
                          BINARY.                                       CI0435
            11            XM06-XCOKEY PICTURE  X(70).                   CI0435
      *PASS AREA TO/FROM CI0435
      *!WF DSP=QT DSL=QT SEL=64 FOR=I DES=1 LEV=1 PLT=10
       01                 QT64.                                         CI0435
            10            QT64-CLID   PICTURE  X(23).                   CI0435
            10            QT64-CTID   PICTURE  X(27).                   CI0435
            10            QT64-CPMTFA PICTURE  X(2).                    CI0435
            10            QT64-DCACG  PICTURE  9(8).                    CI0435
            10            QT64-DNACG  PICTURE  9(8).                    CI0435
            10            QT64-DCACD  PICTURE  X(10).                   CI0435
            10            QT64-DIRAYR PICTURE  9(4).                    CI0435
            10            QT64-CTCUS  PICTURE  999.                     CI0435
            10            QT64-IARTYA PICTURE  X.                       CI0435
            10            QT64-NMESS2 PICTURE  S9(6)                    CI0435
                          COMPUTATIONAL-3.                              CI0435
            10            QT64-IORGC  PICTURE  X.                       CI0435
            10            QT64-QITEM  PICTURE  9(3).                    CI0435
            10            QT64-FILLER PICTURE  X(196).                  CI0435
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0435
          05              DE00-SUITE.                                   CI0435
            15       FILLER         PICTURE  X(00653).                  CI0435
       01                 DE10  REDEFINES      DE00.                    CI0435
            10            DE10-DU11.                                    CI0435
            11            DE10-XFONC  PICTURE  X(4).                    CI0435
            11            DE10-MPSBN  PICTURE  X(8).                    CI0435
            11            DE10-XDBDNM PICTURE  X(08).                   CI0435
            11            DE10-XSEGNM PICTURE  X(08).                   CI0435
            11            DE10-XRC    PICTURE  X(02).                   CI0435
            11            DE10-MSEG   PICTURE  X(08).                   CI0435
            11            DE10-XCOKEY PICTURE  X(70).                   CI0435
            11            DE10-CUIBR  PICTURE  X(01).                   CI0435
            11            DE10-CUIBA  PICTURE  X(01).                   CI0435
            11            DE10-IPBIK  PICTURE  X(1).                    CI0435
            10            DE10-DU03.                                    CI0435
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0435
                          COMPUTATIONAL-3.                              CI0435
            11            DE10-CMSSF  PICTURE  XX.                      CI0435
            11            DE10-DU09.                                    CI0435
            12            DE10-CMESA  PICTURE  S9(9)                    CI0435
                          BINARY.                                       CI0435
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0435
                          BINARY.                                       CI0435
            12            DE10-CMESB  PICTURE  S9(9)                    CI0435
                          BINARY.                                       CI0435
            12            DE10-CMSST  PICTURE  S9(9)                    CI0435
                          BINARY.                                       CI0435
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0435
                          BINARY.                                       CI0435
            12            DE10-QELLAA PICTURE  S9(9)                    CI0435
                          BINARY.                                       CI0435
            12            DE10-TMESS4 PICTURE  X(512).                  CI0435
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
       01                 MS00.                                         CI0435
          05              MS00-SUITE.                                   CI0435
            15       FILLER         PICTURE  X(00542).                  CI0435
       01                 MS03  REDEFINES      MS00.                    CI0435
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0435
                          COMPUTATIONAL-3.                              CI0435
            10            MS03-CMSSF  PICTURE  XX.                      CI0435
            10            MS03-DU09.                                    CI0435
            11            MS03-CMESA  PICTURE  S9(9)                    CI0435
                          BINARY.                                       CI0435
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0435
                          BINARY.                                       CI0435
            11            MS03-CMESB  PICTURE  S9(9)                    CI0435
                          BINARY.                                       CI0435
            11            MS03-CMSST  PICTURE  S9(9)                    CI0435
                          BINARY.                                       CI0435
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0435
                          BINARY.                                       CI0435
            11            MS03-QELLAA PICTURE  S9(9)                    CI0435
                          BINARY.                                       CI0435
            11            MS03-TMESS4 PICTURE  X(512).                  CI0435
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0435
            10            MX11-QMSGS  PICTURE  9(03).                   CI0435
            10            MX11-PJ09                                     CI0435
                          OCCURS       025     TIMES.                   CI0435
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0435
                          COMPUTATIONAL-3.                              CI0435
            11            MX11-CMESB  PICTURE  S9(9)                    CI0435
                          BINARY.                                       CI0435
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                QT64
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N0T.      NOTE *************************************.
      *               *                                   *
      *               *SET ADDRESSES FOR PCB LINKAGE      *
      *               *                                   *
      *               *************************************.
       F0T.           EXIT.                                             lv05
      *N0TSC.    NOTE *SET ADDRESSES FOR PCB LINKAGE      *.
       F0TSC.                                                           lv10
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF XL06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF XM06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
       F0TSC-FN. EXIT.
       F0T-FN.   EXIT.
      *N01.      NOTE *************************************.            CI0435
      *               *                                   *             CI0435
      *               *INITIALISATIONS                    *             CI0435
      *               *                                   *             CI0435
      *               *************************************.            CI0435
       F01.      EXIT.
       F01-FN.   EXIT.
      *N02.      NOTE *************************************.            ADU102
      *               *                                   *             ADU102
      *               *---> Module Initializations        *             ADU102
      *               *                                   *             ADU102
      *               *************************************.            ADU102
       F02.                                                             lv05
      *                                                                 ADU102
      *N02AG.    NOTE *********************************   *.            ACMCTI
       F02AG.                                                           lv10
      ** SUB-FUNCTION TO PERFORM A    *                                 ACMCTI
      ** DUMMY DB2 CALL.              *                                 ACMCTI
      *********************************                                 ACMCTI
           EXEC SQL    SET                                              ACMCTI
                        :WS00-DATE = CURRENT_DATE            END-EXEC.  ACMCTI
           PERFORM     F93SQ THRU F93SQ-FN.                             ACMCTI
       F02AG-FN. EXIT.
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
      *N02CB.    NOTE *NEEDS TO BE DONE ONLY ONCE         *.
       F02CB.    IF    WS00-FIRST = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F02CB-FN.
           MOVE        EIBTIME TO WS00-EIBTIME
           PERFORM     F92CD THRU F92CD-FN
           MOVE        DT01-UDATE TO WS00-CURRDATE
           MOVE        'N' TO WS00-FIRST.
       F02CB-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0435
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0435
      *               *                                   *             CI0435
      *               *FIN DE TRAITEMENT                  *             CI0435
      *               *                                   *             CI0435
      *               *************************************.            CI0435
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0435
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *COMMON FUNCTION                    *
      *               *                                   *
      *               *************************************.
       F45.           EXIT.                                             lv05
      *N45BH.    NOTE *VALIDATE ACCOUNT ID                *.
       F45BH.                                                           lv10
           MOVE        QT64-CTID TO S-CTU01-CTID
           PERFORM     F94BB THRU F94BB-FN.
                 IF    IK = '1'                                         DOT
      *CT01 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012234 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45BH-FN. EXIT.
      *N45GG.    NOTE *CALL CI0018 ACCOUNT CLIENTS        *.
       F45GG.                                                           lv10
           PERFORM     F97DB THRU F97DB-FN
      *CHECK RETURN FROM CALLED PROGRAM
           MOVE        CI0018 TO WS00-PROGR
           PERFORM     F97ZB THRU F97ZB-FN
      *GET ALL RELATED CLIENTS
      *TO SEE IF ANY CLIENTS ARE DEAD
           MOVE        1 TO I1
           MOVE        ZERO TO I2
           QT64-QITEM
           MOVE        ZERO TO WS00-CLID
           WS00-CLCTRC
           WS00-TAXPAYER-CLID
           MOVE        'N' TO WS00-CUSTODIAN
           MOVE        'N' TO QT64-IORGC
           INITIALIZE  WS00-AGE WS00-TP-AGE WS01-CLID
           WS00-CLTIN WS00-CLTINC
      *SET DEAD INDICATOR AS DEFAULT
           MOVE        'N' TO ACCT-DEAD.
      *N45HA.    NOTE *GO THROUGH ALL CLIENTS             *.
       F45HA.    IF    I1 NOT > PC14-QITEM                              lv15
                 NEXT SENTENCE ELSE GO TO     F45HA-FN.
      *GET THE FIRST OWNER CLIENT ID                                    DOT
                 IF    PC14-CLCTRC (I1) = 001                           DOT
                 AND   WS00-CLCTRC = ZEROES
           MOVE        PC14-CLID (I1) TO WS00-CLID
           MOVE        PC14-CLCTRC (I1) TO WS00-CLCTRC.
                 IF    PC14-CLCTRC (I1) = 001                           DOT
                 AND   PC14-CLID (I1) = QT64-CLID
      *SET THE INDICATOR FOR OWNER
           MOVE        'Y' TO WS00-CUSTODIAN.
      *SUPERSEDE WITH TAX PAYER CLID                                    DOT
                 IF    PC14-CLCTRC (I1) = 004                           DOT
           MOVE        PC14-CLID (I1) TO WS00-CLID
           MOVE        PC14-CLCTRC (I1) TO WS00-CLCTRC.
           MOVE        PC14-CLID (I1) TO S-CLU01-CL01K                  DOT
           ADD         1 TO I1
           ADD         1 TO I2.
      *N45HH.    NOTE *READ CL01                          *.
       F45HH.                                                           lv20
      *GU CL01
           PERFORM     F94CB THRU F94CB-FN.
                 IF    IK = '1'                                         DOT
      *CL01 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012012 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
                 IF    PC14-CLCTRC (I2)                                 DOT
                       = (001 OR 004 OR 007)
                 AND   CL01-CLTYP = 'O'
                 AND   QT64-IORGC = 'N'
      *FOR OWNER OR TAXPAYER
      *CHECK IF ORGANIZATION CLIENT
           MOVE        'Y' TO QT64-IORGC.
                 IF    PC14-CLCTRC (I2)                                 DOT
                       = (001 OR 007)
                 AND   CL01-CLTYP = 'P'
      *COUNT THE NUMBER OF OWNERS
           ADD         1 TO QT64-QITEM.
      *N45HJ.    NOTE *ONLY FOR ICS MONEY MOVEMENT        *.
       F45HJ.    IF    CT01-CTIDA = 133                                 lv25
                 NEXT SENTENCE ELSE GO TO     F45HJ-FN.
      *CALCULATE THE AGE AND SET DEATH
      *INDICATOR
      *N45HK.    NOTE *READ CL03 IF PERSON                *.
       F45HK.    IF    S-CLU01-CLID NOT = WS01-CLID                     lv30
                 NEXT SENTENCE ELSE GO TO     F45HK-FN.
      *********************************
                 IF    S-CLU01-CLID = QT64-CLID                         DOT
      *SAVE THE TAXPAYER ID AND
      *TAXPAYER ID CERT CODE
           MOVE        CL01-CLTIN TO WS00-CLTIN
           MOVE        CL01-CLTINC TO WS00-CLTINC.
                 IF    CL01-CLTYP NOT = 'P'                             DOT
      *CLIENT IS NOT A PERSON
               GO TO     F45HA-900.
      *READ CL03                                                        DOT
           PERFORM     F94CC THRU F94CC-FN.
                 IF    IK = '1'                                         DOT
      *CL03 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012161 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *SAVE THE CLIENT ID                                               DOT
           MOVE        S-CLU01-CLID TO WS01-CLID.
      *N45HZ.    NOTE *SOME ONE DEAD IS RELATED TO ACCT   *.
       F45HZ.    IF    CL03-CLDOD > ZERO                                lv35
                 OR    CL03-CLDTH = 'Y'
                 NEXT SENTENCE ELSE GO TO     F45HZ-FN.
      *SET THE INDICATOR
           MOVE        'Y' TO ACCT-DEAD.
       F45HZ-FN. EXIT.
       F45HK-FN. EXIT.
      *N45ID.    NOTE *GET THE PERSON'S AGE IN YEARS      *.
       F45ID.    IF    WS00-CLID = S-CLU01-CLID                         lv30
                 NEXT SENTENCE ELSE GO TO     F45ID-FN.
           MOVE        CL03-CLDOB TO DF34-DTGRGA
           MOVE        WS00-CURRDATE TO DF34-DTGRGB
           MOVE        8 TO DF30-CDTSF
           MOVE        3 TO DF34-CDTUC
           PERFORM     F92DF THRU F92DF-FN
      *SAVE THE AGE IN YEARS
           MOVE        DF34-NDTUN TO WS00-AGE.
                 IF    WS00-CLCTRC = 004                                DOT
      *SAVE THE AGE FOR TAXPAYER ID
           MOVE        WS00-AGE TO WS00-TP-AGE.
       F45ID-FN. EXIT.
       F45HJ-FN. EXIT.
       F45HH-FN. EXIT.
       F45HA-900. GO TO F45HA.
       F45HA-FN. EXIT.
      *N45IG.    NOTE *HAS ORGANIZATION CLIENT            *.
       F45IG.    IF    QT64-IORGC = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F45IG-FN.
           MOVE        ZERO TO QT64-QITEM.
       F45IG-FN. EXIT.
       F45GG-FN. EXIT.
      *N45MB.    NOTE *INITIALIZE BA ARR INDICATOR        *.
       F45MB.                                                           lv10
           MOVE        'Y' TO QT64-IARTYA.
       F45MB-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATIONS FOR BETA BROKERAGE     *
      *               *                                   *
      *               *************************************.
       F50.      IF    CT01-CTIDA = 133                                 lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *N50BA.    NOTE *IF CLIENT TIN IS MISSING           *.
       F50BA.    IF    WS00-CLTIN = ZEROES                              lv10
                 NEXT SENTENCE ELSE GO TO     F50BA-FN.
      *********************************
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012285 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50BA-FN. EXIT.
      *N50BF.    NOTE *FOR QUALIFIED BETA BROKERAGE       *.
       F50BF.    IF    CT01-CQACT > ZEROS                               lv10
                 NEXT SENTENCE ELSE GO TO     F50BF-FN.
      *********************************
                 IF    WS00-CLTINC NOT = 2                              DOT
      *IF TIN IS NOT CERTIFIED
           MOVE        'N' TO QT64-IARTYA
           MOVE        013256 TO QT64-NMESS2
           MOVE                     ALL '1' TO FT GO TO F20.
       F50BF-FN. EXIT.
      *N50BM.    NOTE *403B ACCOUNT IS NOT ALLOWED        *.
       F50BM.    IF    CT01-CQACT = 002 OR 003                          lv10
                 OR    004
                 NEXT SENTENCE ELSE GO TO     F50BM-FN.
           MOVE        'N' TO QT64-IARTYA
           MOVE        015586 TO QT64-NMESS2
           MOVE                     ALL '1' TO FT GO TO F20.
       F50BM-FN. EXIT.
      *N50CA.    NOTE *REJECT CESA ACCOUNT IF THE OWNER   *.
       F50CA.    IF    WS00-AGE >= 18                                   lv10
                 AND   CT01-CIRAT = 007
                 NEXT SENTENCE ELSE GO TO     F50CA-FN.
      *IS EQUAL TO OR OLDER THAN 18
      *YEARS OLD
           MOVE        'N' TO QT64-IARTYA
           MOVE        014219 TO QT64-NMESS2
           MOVE                     ALL '1' TO FT GO TO F20.
       F50CA-FN. EXIT.
      *N50CG.    NOTE *NO OWNER FOR ACCT                  *.
       F50CG.    IF    WS00-CLCTRC = ZERO                               lv10
                 NEXT SENTENCE ELSE GO TO     F50CG-FN.
           MOVE        'N' TO QT64-IARTYA
           MOVE        015429 TO QT64-NMESS2
           MOVE                     ALL '1' TO FT GO TO F20.
       F50CG-FN. EXIT.
      *N50CO.    NOTE *SOME ONE DEAD IS RELATED TO ACCT   *.
       F50CO.    IF    ACCT-DEAD = 'Y'                                  lv10
                 NEXT SENTENCE ELSE GO TO     F50CO-FN.
           MOVE        'N' TO QT64-IARTYA
           MOVE        014218 TO QT64-NMESS2
           MOVE                     ALL '1' TO FT GO TO F20.
       F50CO-FN. EXIT.
      *N50DB.    NOTE *VALIDATE THE ADDRESS               *.
       F50DB.                                                           lv10
      *CALL CI0004 - ACCOUNT ADDRESS
           PERFORM     F97CB THRU F97CB-FN
      *CHECK RETURN FROM CALLED PROGRAM
           MOVE        CI0004 TO WS00-PROGR
           PERFORM     F97ZB THRU F97ZB-FN
           MOVE        PB07-GECTRY TO WS00-GECTRY.
                 IF    NOT COUNTRY-US                                   DOT
      *FOREIGN ADDRESS NOT VALID
           MOVE        'N' TO QT64-IARTYA
           MOVE        014167 TO QT64-NMESS2
           MOVE                     ALL '1' TO FT GO TO F20.
       F50DB-FN. EXIT.
      *N50EA.    NOTE *VALIDATE OWNERSHIP                 *.
       F50EA.                                                           lv10
      *********************************
      *N50ED.    NOTE *ACH-IN TRAN IS NOT ALLOWED IF      *.
       F50ED.    IF    QT64-CTCUS = 009                                 lv15
                 OR    QT64-CTCUS = 012
                 OR    QT64-CTCUS = 014
                 NEXT SENTENCE ELSE GO TO     F50ED-FN.
      *VALUES FOR CTCUS ARE AS FOLLOWS
      *IRA BENEFICIAL               009
      *ROTH CONTRIBUTORY BENEFICIAL 012
      *ROTH CONVERSION BENEFICIAL   014
           MOVE        'N' TO QT64-IARTYA
           MOVE        015635 TO QT64-NMESS2
           MOVE                     ALL '1' TO FT GO TO F20.
       F50ED-FN. EXIT.
      *N50EG.    NOTE *ACHIN TRAN IS NOT ALLOWED FOR      *.
       F50EG.    IF    QT64-CTCUS = 008                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50EG-FN.
      *SEP IRA 133 ACCOUNT
           MOVE        'N' TO QT64-IARTYA
           MOVE        013570 TO QT64-NMESS2
           MOVE                     ALL '1' TO FT GO TO F20.
       F50EG-FN. EXIT.
      *N50EH.    NOTE *CALL CI0003 - ACCOUNT OWNERSHIP    *.
       F50EH.                                                           lv15
           PERFORM     F97BB THRU F97BB-FN
      *CHECK RETURN FROM CALLED PROGRAM
           MOVE        CI0003 TO WS00-PROGR
           PERFORM     F97ZB THRU F97ZB-FN.
       F50EH-FN. EXIT.
      *N50EN.    NOTE *CALL CI0085 - OWNERSHIP SCRUBBER   *.
       F50EN.                                                           lv15
           PERFORM     F97GB THRU F97GB-FN
      *CHECK RETURN FROM CALLED PROGRAM
           MOVE        CI0085 TO WS00-PROGR
           PERFORM     F97ZB THRU F97ZB-FN.
      *N50EQ.    NOTE *REJECT THE ACCOUNT WHICH HAS       *.
       F50EQ.    IF    PF1F-IOWNC (40) = 'Y'                            lv20
                 OR    PF1F-IOWNC (37) = 'Y'
                 OR    PF1F-IOWNC (18) = 'Y'
                 OR    PF1F-IOWNC (10) = 'Y'
                 OR    PF1F-IOWNC (39) = 'Y'
                 OR    PF1F-IOWNC (38) = 'Y'
                 OR    PF1F-IOWNC (36) = 'Y'
                 OR    PF1F-IOWNC (02) = 'Y'
                 NEXT SENTENCE ELSE GO TO     F50EQ-FN.
      *INVALID OWNERSHIP
      *COOGAN LAW, TUTOR/TUTRIX,
      *USUFRUCTORY, GUARDIANSHIP
      *NEXT FRIEND,
      *CONSERVATORSHIP,
      *COMMITTEE/CURATOR,
      *CONDITIONAL, A MINOR
      *********************************
           MOVE        'N' TO QT64-IARTYA
           MOVE        014217 TO QT64-NMESS2
           MOVE                     ALL '1' TO FT GO TO F20.
       F50EQ-FN. EXIT.
      *N50IH.    NOTE *FOR UGMA/UTMA BETA BROK ACCOUNT    *.
       F50IH.    IF    PF1F-IOWNC (34) = 'Y'                            lv20
                 OR    PF1F-IOWNC (35) = 'Y'
                 NEXT SENTENCE ELSE GO TO     F50IH-FN.
      *N50IM.    NOTE *REJECT THE ACCOUNT WHEN THE AGE    *.
       F50IM.    IF    WS00-TP-AGE >= 21                                lv25
                 NEXT SENTENCE ELSE GO TO     F50IM-FN.
      *OF TAXPAYER CLIENT IS EQUAL TO
      *OR OLDER THAN 21 YEARS OLD
           MOVE        'N' TO QT64-IARTYA
           MOVE        015637 TO QT64-NMESS2
           MOVE                     ALL '1' TO FT GO TO F20.
       F50IM-FN. EXIT.
      *N50IQ.    NOTE *REJECT THE ACCOUNT IF THE          *.
       F50IQ.    IF    WS00-CUSTODIAN NOT = 'Y'                         lv25
                 NEXT SENTENCE ELSE GO TO     F50IQ-FN.
      *CLIENT IS NOT THE OWNER ON THE
      *BETA BROKERAGE ACCOUNT
           MOVE        'N' TO QT64-IARTYA
           MOVE        015638 TO QT64-NMESS2
           MOVE                     ALL '1' TO FT GO TO F20.
       F50IQ-FN. EXIT.
       F50IH-FN. EXIT.
       F50EN-FN. EXIT.
       F50EA-FN. EXIT.
       F50-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *SET TAX REPORTING YEAR FOR         *
      *               *                                   *
      *               *************************************.
       F60.      IF    QT64-CPMTFA = 'OD'                               lv05
                 AND   CT01-CQACT > ZEROS
                 NEXT SENTENCE ELSE GO TO     F60-FN.
      *FOR QUALIFIED BETA BROKERAGE
      *ACCOUNT ON-DEMAND REQUEST
      *N60CA.    NOTE *CALL LATE TRADING MODULE           *.
       F60CA.                                                           lv10
           INITIALIZE  I93B
           MOVE        QT64-DCACD TO I93B-DACTT
           MOVE        'O' TO I93B-CEADC
           PERFORM     F97HB THRU F97HB-FN.
       F60CA-FN. EXIT.
      *N60CG.    NOTE *SET THE DATE FOR DIRAYR CALC       *.
       F60CG.                                                           lv10
      *********************************
                 IF    WS00-EIBTIME < WS-GETIM6                         DOT
                 OR    WS00-EIBTIME = WS-GETIM6
                 OR    QT64-DCACG NOT =
                       WS00-CURRDATE
           MOVE        QT64-DCACG TO WS00-GESTD
                 ELSE
           MOVE        QT64-DNACG TO WS00-GESTD.
      *N60DA.    NOTE *DEFAULT FOR QUALIFIED              *.
       F60DA.                                                           lv15
           MOVE        WS00-GESTD (1:4) TO QT64-DIRAYR.
      *N60DD.    NOTE *OVERRIDE FOR IRA PRIOR YEAR        *.
       F60DD.    IF    CT01-DIRAC NOT = ZERO                            lv20
                 NEXT SENTENCE ELSE GO TO     F60DD-FN.
                 IF    WS00-GESTD (5:4) <=                              DOT
                       CT01-DIRAC
           SUBTRACT    1 FROM QT64-DIRAYR.
       F60DD-FN. EXIT.
       F60DA-FN. EXIT.
       F60CG-FN. EXIT.
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
      *               *GET CURRENT DATE AND DATE          *
      *               *                                   *
      *               *************************************.
       F92.                                                             lv05
      *CALCULATION
      *N92CD.    NOTE *GET CURRENT DATE                   *.
       F92CD.                                                           lv10
           EXEC CICS   ASKTIME ABSTIME (DT01-XMSTS)          END-EXEC.  ADU155
           EXEC CICS   FORMATTIME ABSTIME (DT01-XMSTS)                  ADU155
                       YYMMDD (DT01-XDAT69)                             ADU155
                       YEAR (DT01-F2CCYY)                    END-EXEC.  ADU155
           COMPUTE     DT01-YEAR = DT01-F2CCYY                          ADU155
      ** MOVE DT01-UDATE TO YOUR FIELD                                  ADU155
           MOVE        DT01-XDAT69 (3:4) TO DT01-MMDD.                  ADU155
       F92CD-FN. EXIT.
      *N92DF.    NOTE *CDU - DATE DIFF/CALCULATION        *.            AADA82
       F92DF.                                                           lv10
      ** * * * * * * * * * * * * * * *                                  AADA82
      *This code calls the common date                                  AADA82
      *utility MWS100EX to calculate                                    AADA82
      *the difference between 2 dates                                   AADA82
      *or calculate a new date (add/                                    AADA82
      *subtract days). It uses a                                        AADA82
      *dynamic call.                                                    AADA82
      ** * * * * * * * * * * * * * * *                                  AADA82
      *Before the call set the subfunc                                  AADA82
      *request code DF30-CDTSF:                                         AADA82
      *  8 = date difference                                            AADA82
      *  9 = date add/subtract days                                     AADA82
      ** * * * * * * * * * * * * * * *                                  AADA82
      *Check return code DF30-CDTSC                                     AADA82
      *after the call.                                                  AADA82
      *    0 = Error Free                                               AADA82
      *    3 = Invalid Date                                             AADA82
      *    5 = Invalid Day                                              AADA82
      *    6 = Invalid Month                                            AADA82
      ** * * * * * * * * * * * * * * *                                  AADA82
           MOVE        4 TO DF30-CDTFN                                  AADA82
           CALL        MWS100EX USING DF30                              AADA82
           DF34.                                                        AADA82
       F92DF-FN. EXIT.
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
      *               *ACCESS IMS DATABASE                *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94BB.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94BB.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XL06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        XL06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94BB-FN. EXIT.
      *N94CB.    NOTE *CALL GU ON CL01                    *.            ADU026
       F94CB.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XM06 CL01                                                    ADU026
           S-CLU01-SSA                                                  ADU026
           MOVE        XM06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CB-FN. EXIT.
      *N94CC.    NOTE *CALL GU ON CL03                    *.            ADU026
       F94CC.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XM06 CL03                                                    ADU026
           S-CLU01-SSA S-CL03-SSA                                       ADU026
           MOVE        XM06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CC-FN. EXIT.
       F94-FN.   EXIT.
      *N97.      NOTE *************************************.
      *               *                                   *
      *               *CALLS TO MODULES                   *
      *               *                                   *
      *               *************************************.
       F97.           EXIT.                                             lv05
      *N97BB.    NOTE *CALL CI0003 - ACCT OWNER/BENE      *.            AM0003
       F97BB.                                                           lv10
      *                                                                 AM0003
      *********************************                                 AM0003
      ** THIS MODULE WILL READ THE    *                                 AM0003
      ** CONTRACT DATABASE TO GET THE *                                 AM0003
      ** ACCOUNT OWNERSHIP AND        *                                 AM0003
      ** BENEFICIARY LINES FOR THE    *                                 AM0003
      ** REQUESTED ACCOUNT NUMBER.    *                                 AM0003
      *********************************                                 AM0003
      *                                                                 AM0003
           INITIALIZE      PA04                                         AM0003
           MOVE        CT01-CTID TO PA04-CTID                           AM0003
           MOVE        'Y' TO PA04-IPOCH                                AM0003
           SET CI0003-PCB-CT1P-PTR1 TO                                  AM0003
                       PCB-CT1P-PTR1                                    AM0003
           INITIALIZE      DE10-DU03                                    AM0003
           CALL        CI0003 USING                                     AM0003
           DFHEIBLK                                                     AM0003
           DFHCOMMAREA                                                  AM0003
           DLIUIBII                                                     AM0003
           CI0003-PCB-ADDRESS-LIST                                      AM0003
           PA04                                                         AM0003
           DE10                                                         AM0003
           MS03.                                                        AM0003
       F97BB-FN. EXIT.
      *N97CB.    NOTE *CALL CI0004 - ACCOUNT ADDRESS      *.            AM0004
       F97CB.                                                           lv10
      *                                                                 AM0004
      *********************************                                 AM0004
      ** THIS MODULE WILL READ THE    *                                 AM0004
      ** CONTRACT DATABASE TO THE     *                                 AM0004
      ** OWNER CLIENT THAT THE ADDRESS*                                 AM0004
      ** ASSOCIATED WITH IT.  IT WILL *                                 AM0004
      ** THEN CALL MODULE CI0006 TO   *                                 AM0004
      ** GET THE CLIENT'S ADDRESS     *                                 AM0004
      ** SEGMENT(CL24).               *                                 AM0004
      *********************************                                 AM0004
      *                                                                 AM0004
           INITIALIZE      PB07                                         AM0004
           MOVE        CT01-CTID TO PB07-CTID                           AM0004
           MOVE        QT64-DCACG TO PB07-DCACG                         AM0004
           SET CI0004-PCB-CT1P-PTR1 TO                                  AM0004
                       PCB-CT1P-PTR1                                    AM0004
           SET CI0004-PCB-CL1P-PTR1 TO                                  AM0004
                       PCB-CL1P-PTR1                                    AM0004
           INITIALIZE DE10-DU03                                         AM0004
           CALL        CI0004 USING                                     AM0004
           DFHEIBLK                                                     AM0004
           DFHCOMMAREA                                                  AM0004
           DLIUIBII                                                     AM0004
           CI0004-PCB-ADDRESS-LIST                                      AM0004
           PB07                                                         AM0004
           DE10                                                         AM0004
           MS03.                                                        AM0004
       F97CB-FN. EXIT.
      *N97DB.    NOTE *CALL CI0018 - ACCT CLIENTS         *.            AM0018
       F97DB.                                                           lv10
      *                                                                 AM0018
      *********************************                                 AM0018
      ** THIS MODULE WILL READ THE    *                                 AM0018
      ** CONTRACT DATABASE TO GET THE *                                 AM0018
      ** TAXPAYER CLIENT ID AND OWNER *                                 AM0018
      ** CLIENT ID'S ASSOCIATED WITH  *                                 AM0018
      ** THE ACCOUNT NUMBER.          *                                 AM0018
      *********************************                                 AM0018
      *                                                                 AM0018
           INITIALIZE      PC14                                         AM0018
           MOVE        CT01-CTID TO PC14-CTID                           AM0018
           MOVE        QT64-DCACG TO PC14-DCACG                         AM0018
           MOVE        25 TO PC14-XIMAX                                 AM0018
           MOVE        'Y' TO PC14-IPOCH                                AM0018
           MOVE        'A' TO PC14-CRROL
           SET CI0018-PCB-CT1P-PTR1 TO                                  AM0018
                       PCB-CT1P-PTR1                                    AM0018
           INITIALIZE      DE10-DU03                                    AM0018
           CALL        CI0018 USING                                     AM0018
           DFHEIBLK                                                     AM0018
           DFHCOMMAREA                                                  AM0018
           DLIUIBII                                                     AM0018
           CI0018-PCB-ADDRESS-LIST                                      AM0018
           PC14                                                         AM0018
           DE10                                                         AM0018
           MS03.                                                        AM0018
       F97DB-FN. EXIT.
      *N97GB.    NOTE *CALL CI0085 - DECIPHER OWNERSHIP   *.            AM0085
       F97GB.                                                           lv10
      *********************************                                 AM0085
      ** THIS MODULE WILL SEARCH      *                                 AM0085
      ** OWNERSHIP LINES THROUGH      *                                 AM0085
      ** THE TEXT LOOKING FOR ACCT    *                                 AM0085
      ** CLASSIFICATION               *                                 AM0085
      *********************************                                 AM0085
           INITIALIZE PF1F MS03
      *DIRECTLY USE PA04 FROM CI0003
      *
      *
      *
      *
           MOVE        PROGR TO PF1F-PROGR                              AM0085
           MOVE        PB07-GESAD (1) TO PF1F-GESAD1
           MOVE        PB07-GESAD (2) TO PF1F-GESAD2
           MOVE        PB07-GESAD (3) TO PF1F-GESAD3
           CALL        CI0085 USING                                     AM0085
           DFHEIBLK                                                     AM0085
           DFHCOMMAREA                                                  AM0085
           PA04                                                         AM0085
           PF1F                                                         AM0085
           MS03                                                         AM0085
           MX11.                                                        AM0085
      *N97GE.    NOTE *IF NO ERRORS - BREAK DOWN PF1F     *.            AM0085
       F97GE.    IF    (MS03-NMESS2 = ZEROS                             lv15
                 OR    (MS03-NMESS2 NOT = ZEROS                         AM0085
                 AND   MS03-CMESB < 11))                                AM0085
                 NEXT SENTENCE ELSE GO TO     F97GE-FN.                 AM0085
      *TALLY NUMBER OF 'Y' (TRUE) FLAGS                                 AM0085
           INITIALIZE  TALLI                                            AM0085
           INSPECT     PF1F-OUTPUT TALLYING TALLI                       AM0085
           FOR ALL 'Y'.                                                 AM0085
       F97GE-FN. EXIT.
       F97GB-FN. EXIT.
      *N97HB.    NOTE *********************************   *.            ACMCTI
       F97HB.                                                           lv10
      ** THIS SUBFUNCTION CALLS THE   *                                 ACMCTI
      ** ONLINE INQUIRY MODULE        *                                 ACMCTI
      ** (CI0361) TO RETRIEVE THE ROW *                                 ACMCTI
      ** INFORMATION FROM THE DB2     *                                 ACMCTI
      ** TABLE (TBS234) CORRESPONDING *                                 ACMCTI
      ** TO THE KEYS: 'DATE TYPE CODE'*                                 ACMCTI
      ** (ELEMENT: CEADC) AND         *                                 ACMCTI
      ** 'ACCOUNTING DATE'(ELEMENT:   *                                 ACMCTI
      ** DACTT).                      *                                 ACMCTI
      *********************************                                 ACMCTI
      *********************************                                 ACMCTI
      ** MOVE VALUES TO THE LINKAGE   *                                 ACMCTI
      ** AREA ELEMENTS TO BE PASSED   *                                 ACMCTI
      ** TO THE CALLED MODULE CI0361. *                                 ACMCTI
      *********************************                                 ACMCTI
      *********************************                                 ACMCTI
      ** CALL THE ONLINE INQUIRY      *                                 ACMCTI
      ** MODULE (CI0361).             *                                 ACMCTI
      *********************************                                 ACMCTI
           CALL        CI0361 USING                                     ACMCTI
           DFHEIBLK                                                     ACMCTI
           DFHCOMMAREA                                                  ACMCTI
           I93B.                                                        ACMCTI
      *N97HE.    NOTE *CHECK RETURN CODES                 *.
       F97HE.    IF    I93B-CRTNC = 11111                               lv15
                 OR    I93B-CRTNC = 22222
                 OR    I93B-CRTNC = 100
                 NEXT SENTENCE ELSE GO TO     F97HE-FN.
      *IF INVALID CODE, SEND ERROR MSG
      *AND TERMINATE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012786 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F97HE-FN. EXIT.
      *N97HH.    NOTE *REFORMAT DATE                      *.
       F97HH.                                                           lv15
           MOVE        I93B-GETIMM TO WS-GETIMM
           MOVE        WS-HH TO WS-MKT-HH
           MOVE        WS-MM TO WS-MKT-MM
           MOVE        WS-SS TO WS-MKT-SS.
       F97HH-FN. EXIT.
       F97HB-FN. EXIT.
      *N97ZB.    NOTE *CHECK ERRORS FROM CALL             *.
       F97ZB.                                                           lv10
      *EXCEPTIONS TO KNOWN PROD PROBS
      *N97ZC.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F97ZC.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F97ZC-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        WS00-PROGR TO MS03-TMESS4 (IMS03R : 6)           ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        WS00-PROGR TO DE10-TMESS4 (IMS03R : 6)           ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F97ZC-900. GO TO F97ZD-FN.
       F97ZC-FN. EXIT.
      *N97ZD.    NOTE *NO ERRORS                          *.            ADU071
       F97ZD.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F97ZD-FN. EXIT.
       F97ZB-FN. EXIT.
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
