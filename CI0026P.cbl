       IDENTIFICATION DIVISION.                                         CI0026
       PROGRAM-ID.  CI0026P.                                            CI0026
      *AUTHOR.         M\M - ACCOUNT WITHHOLDING.                       CI0026
      *DATE-COMPILED.   09/08/14.                                       CI0026
      ******************************************************************$2000
      ** YEAR 2000 COMPLIANT - YES                                      $2000
      ** (THIS IS NOT CERTIFICATION FOR YEAR 2000)                      $2000
      ******************************************************************$2000
       ENVIRONMENT DIVISION.                                            CI0026
       CONFIGURATION SECTION.                                           CI0026
       SOURCE-COMPUTER. IBM-370.                                        CI0026
       OBJECT-COMPUTER. IBM-370.                                        CI0026
       DATA DIVISION.                                                   CI0026
       WORKING-STORAGE SECTION.                                         CI0026
      ******************************************************            AADA82
      ****      WORK AREAS FOR COMMON DATE UTILITY       ***            AADA82
      ******************************************************            AADA82
      **                                                                AADA82
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA82
      **                                                                AADA82
      **   SEGMENT DD30 - FUNCTION LAYOUT                               AADA82
      **                                                                AADA82
      *!WF DSP=DD DSL=DD SEL=30 FOR=I DES=2 LEV=1                       AADA82
       01                 DD01.                                         CI0026
            10            DD01-XDAT8.                                   CI0026
            11            DD01-XDATC  PICTURE  XX                       CI0026
                          VALUE                SPACE.                   CI0026
            11            DD01-XDATY  PICTURE  XX                       CI0026
                          VALUE                SPACE.                   CI0026
            11            DD01-XDATM  PICTURE  XX                       CI0026
                          VALUE                SPACE.                   CI0026
            11            DD01-XDATD  PICTURE  XX                       CI0026
                          VALUE                SPACE.                   CI0026
            10            DD01-XDAT8D                                   CI0026
                          REDEFINES            DD01-XDAT8               CI0026
               PICTURE    9(8).                                         CI0026
            10            DD01-XDAT81.                                  CI0026
            11            DD01-XDATM1 PICTURE  XX                       CI0026
                          VALUE                SPACE.                   CI0026
            11            DD01-XDATD1 PICTURE  XX                       CI0026
                          VALUE                SPACE.                   CI0026
            11            DD01-XDATC1 PICTURE  XX                       CI0026
                          VALUE                SPACE.                   CI0026
            11            DD01-XDATY1 PICTURE  XX                       CI0026
                          VALUE                SPACE.                   CI0026
            10            DD01-XDAT80                                   CI0026
                          REDEFINES            DD01-XDAT81              CI0026
               PICTURE    9(8).                                         CI0026
            10            DD01-XDAT62.                                  CI0026
            11            DD01-XDATM2 PICTURE  XX                       CI0026
                          VALUE                SPACE.                   CI0026
            11            DD01-XDATD2 PICTURE  XX                       CI0026
                          VALUE                SPACE.                   CI0026
            11            DD01-XDATY2 PICTURE  XX                       CI0026
                          VALUE                SPACE.                   CI0026
            10            DD01-XDAT69                                   CI0026
                          REDEFINES            DD01-XDAT62              CI0026
               PICTURE    9(6).                                         CI0026
            10            DD01-XDATCU.                                  CI0026
            11            DD01-XDATC9 PICTURE  99                       CI0026
                          VALUE                ZERO.                    CI0026
            11            DD01-XDAYMD.                                  CI0026
            12            DD01-XDATY9 PICTURE  99                       CI0026
                          VALUE                ZERO.                    CI0026
            12            DD01-XDAMD.                                   CI0026
            13            DD01-XDATM9 PICTURE  99                       CI0026
                          VALUE                ZERO.                    CI0026
            13            DD01-XDATD9 PICTURE  99                       CI0026
                          VALUE                ZERO.                    CI0026
            10            DD01-XDAT89 PICTURE  9(8)                     CI0026
                          VALUE                ZERO.                    CI0026
            10            DD01-XDAJC  PICTURE  9(7)                     CI0026
                          VALUE                ZERO.                    CI0026
            10            DD01-XDAJC1.                                  CI0026
            11            DD01-XDAJC9 PICTURE  99                       CI0026
                          VALUE                ZERO.                    CI0026
            11            DD01-XDAJY  PICTURE  99                       CI0026
                          VALUE                ZERO.                    CI0026
            11            DD01-XDAJN  PICTURE  999                      CI0026
                          VALUE                ZERO.                    CI0026
            10            DD01-XDAB   PICTURE  9(5)                     CI0026
                          VALUE                ZERO.                    CI0026
            10            DD01-DD05.                                    CI0026
            11            DD01-XDACT  PICTURE  S9(3)                    CI0026
                          VALUE                ZERO                     CI0026
                          COMPUTATIONAL-3.                              CI0026
            11            DD01-XDACV  PICTURE  S9                       CI0026
                          VALUE                ZERO                     CI0026
                          COMPUTATIONAL-3.                              CI0026
            11            DD01-XDAGP  PICTURE  S9(9)                    CI0026
                          VALUE                ZERO                     CI0026
                          COMPUTATIONAL-3.                              CI0026
            11            DD01-XDAJP  PICTURE  S9(7)                    CI0026
                          VALUE                ZERO                     CI0026
                          COMPUTATIONAL-3.                              CI0026
            11            DD01-XDACV1 PICTURE  S9                       CI0026
                          VALUE                ZERO                     CI0026
                          COMPUTATIONAL-3.                              CI0026
            11            DD01-XDAGP1 PICTURE  S9(9)                    CI0026
                          VALUE                ZERO                     CI0026
                          COMPUTATIONAL-3.                              CI0026
            11            DD01-XDAJP1 PICTURE  S9(7)                    CI0026
                          VALUE                ZERO                     CI0026
                          COMPUTATIONAL-3.                              CI0026
            10            DD01-XW03.                                    CI0026
            11            DD01-XDATG.                                   CI0026
            12            DD01-XDAT1.                                   CI0026
            13            DD01-XDAT19 PICTURE  99                       CI0026
                          VALUE                ZERO.                    CI0026
            12            DD01-XDAT2.                                   CI0026
            13            DD01-XDAT29 PICTURE  99                       CI0026
                          VALUE                ZERO.                    CI0026
            12            DD01-XDAT3.                                   CI0026
            13            DD01-XDAT39 PICTURE  99                       CI0026
                          VALUE                ZERO.                    CI0026
            12            DD01-XDAT4.                                   CI0026
            13            DD01-XDAT49 PICTURE  99                       CI0026
                          VALUE                ZERO.                    CI0026
            11            DD01-XLEAPY PICTURE  99                       CI0026
                          VALUE                ZERO.                    CI0026
            11            DD01-DTGCY  PICTURE  9(4)                     CI0026
                          VALUE                ZERO.                    CI0026
            11            DD01-FILLER                                   CI0026
                          REDEFINES            DD01-DTGCY.              CI0026
            12            DD01-DTGCC  PICTURE  9(2).                    CI0026
            12            DD01-DTGYY  PICTURE  9(2).                    CI0026
       01                 DD30.                                         CI0026
            10            DD30-CDTFN  PICTURE  9(4)                     CI0026
                          VALUE                ZERO.                    CI0026
            10            DD30-CDTSF  PICTURE  9(4)                     CI0026
                          VALUE                ZERO.                    CI0026
            10            DD30-CDTSC  PICTURE  9(4)                     CI0026
                          VALUE                ZERO.                    CI0026
            10            DD30-FILLER PICTURE  X(40)                    CI0026
                          VALUE                SPACE.                   CI0026
       01                 DD34.                                         CI0026
            10            DD34-CAINS  PICTURE  X(03)                    CI0026
                          VALUE                SPACE.                   CI0026
            10            DD34-CDTUC  PICTURE  9                        CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-NDTUN  PICTURE  S9(05)                   CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-FILLER PICTURE  X(162)                   CI0026
                          VALUE                SPACE.                   CI0026
            10            DD34-DTGRG.                                   CI0026
            11            DD34-DTGCY.                                   CI0026
            12            DD34-DTGCC  PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            12            DD34-DTGYY  PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            11            DD34-DTGMM  PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            11            DD34-DTGDD  PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-DTJUL.                                   CI0026
            11            DD34-DTJCY.                                   CI0026
            12            DD34-DTJCC  PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            12            DD34-DTJYY  PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            11            DD34-DTJDD  PICTURE  9(3)                     CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTFM  PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTLM  PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTFF  PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTLF  PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTFW  PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTLW  PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CCDOWA PICTURE  9                        CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CCDRW  PICTURE  9                        CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-FILLER PICTURE  X(58)                    CI0026
                          VALUE                SPACE.                   CI0026
            10            DD34-DTGRGA.                                  CI0026
            11            DD34-DTGCYA.                                  CI0026
            12            DD34-DTGCCA PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            12            DD34-DTGYYA PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            11            DD34-DTGMMA PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            11            DD34-DTGDDA PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-DTJULA.                                  CI0026
            11            DD34-DTJCYA.                                  CI0026
            12            DD34-DTJCCA PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            12            DD34-DTJYYA PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            11            DD34-DTJDDA PICTURE  9(3)                     CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTFMA PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTLMA PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTFFA PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTLFA PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTFWA PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTLWA PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CCDOWB PICTURE  9                        CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CCDRWA PICTURE  9                        CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-FILLER PICTURE  X(58)                    CI0026
                          VALUE                SPACE.                   CI0026
            10            DD34-DTGRGB.                                  CI0026
            11            DD34-DTGCYB.                                  CI0026
            12            DD34-DTGCCB PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            12            DD34-DTGYYB PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            11            DD34-DTGMMB PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            11            DD34-DTGDDB PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-DTJULB.                                  CI0026
            11            DD34-DTJCYB.                                  CI0026
            12            DD34-DTJCCB PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            12            DD34-DTJYYB PICTURE  9(2)                     CI0026
                          VALUE                ZERO.                    CI0026
            11            DD34-DTJDDB PICTURE  9(3)                     CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTFMB PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTLMB PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTFFB PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTLFB PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTFWB PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CDTLWB PICTURE  9(01)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CCDOWC PICTURE  9                        CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-CCDRWB PICTURE  9                        CI0026
                          VALUE                ZERO.                    CI0026
            10            DD34-FILLER PICTURE  X(58)                    CI0026
                          VALUE                SPACE.                   CI0026
            10            DD34-FILLER PICTURE  X(40)                    CI0026
                          VALUE                SPACE.                   CI0026
      **                                                                AADA82
      **   SEGMENT DD34 - CONVERT DATE LAYOUT                           AADA82
      **                                                                AADA82
      *!WF DSP=DD DSL=DD SEL=34 FOR=I DES=2 LEV=1                       AADA82
      **                                                                AADA82
       01                 CL00.                                         CI0026
            02            CL01.                                         CI0026
            10            CL01-CL01K.                                   CI0026
            11            CL01-C199.                                    CI0026
            12            CL01-CLID.                                    CI0026
            13            CL01-CLIDO  PICTURE  9(3).                    CI0026
            13            CL01-CLIDN.                                   CI0026
            14            CL01-CLIDNP PICTURE  X(12).                   CI0026
            14            CL01-CLIDND PICTURE  9(8).                    CI0026
            10            CL01-GECKD  PICTURE  9.                       CI0026
            10            CL01-GEMDA  PICTURE  9(8).                    CI0026
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0026
                          BINARY.                                       CI0026
            10            CL01-GECUC  PICTURE  99.                      CI0026
            10            CL01-CLDOR  PICTURE  9(8).                    CI0026
            10            CL01-CLLNG  PICTURE  XX.                      CI0026
            10            CL01-GESLC  PICTURE  99.                      CI0026
            10            CL01-CLTYP  PICTURE  X.                       CI0026
            10            CL01-CLCLS  PICTURE  9(3).                    CI0026
            10            CL01-CLTWRC PICTURE  99.                      CI0026
            10            CL01-CLPVC  PICTURE  99.                      CI0026
            10            CL01-CLIND  PICTURE  9(3).                    CI0026
            10            CL01-CLTRC  PICTURE  99.                      CI0026
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0026
                          COMPUTATIONAL-3.                              CI0026
            10            CL01-AYSIDA PICTURE  9(3).                    CI0026
            10            CL01-AYSID  PICTURE  9(5).                    CI0026
            10            CL01-CLSTR  PICTURE  9(2).                    CI0026
            10            CL01-CLC11  PICTURE  X.                       CI0026
            10            CL01-CLTIN  PICTURE  9(12).                   CI0026
            10            CL01-CLTND  PICTURE  9(8).                    CI0026
            10            CL01-CLTINC PICTURE  9.                       CI0026
            10            CL01-CCDWA  PICTURE  9.                       CI0026
            10            CL01-CICES  PICTURE  X.                       CI0026
            10            CL01-CLTRA  PICTURE  9(2).                    CI0026
            10            CL01-DIRSY  PICTURE  9(4)                     CI0026
                          COMPUTATIONAL-3.                              CI0026
            10            CL01-CFEDS  PICTURE  X.                       CI0026
            10            CL01-FILLER PICTURE  X(06).                   CI0026
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0027           PIC X(8) VALUE 'CI0027P '.                  AM0027
       01  CI0028           PIC X(8) VALUE 'CI0028P '.                  AM0028
       01                 CT00.                                         CI0026
            02            CT01.                                         CI0026
            10            CT01-CT01K.                                   CI0026
            11            CT01-C299.                                    CI0026
            12            CT01-CTID.                                    CI0026
            13            CT01-CTIDA  PICTURE  9(3).                    CI0026
            13            CT01-CTIDN.                                   CI0026
            14            CT01-CTIDNP PICTURE  X(13).                   CI0026
            14            CT01-CTIDND PICTURE  9(11).                   CI0026
            10            CT01-GECKD  PICTURE  9.                       CI0026
            10            CT01-GEMDA  PICTURE  9(8).                    CI0026
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0026
                          BINARY.                                       CI0026
            10            CT01-GECUC  PICTURE  99.                      CI0026
            10            CT01-CTAUL  PICTURE  9(3).                    CI0026
            10            CT01-DIRAC  PICTURE  9(4).                    CI0026
            10            CT01-CTCCI  PICTURE  X.                       CI0026
            10            CT01-CTCUS  PICTURE  999.                     CI0026
            10            CT01-CTEFD  PICTURE  9(8).                    CI0026
            10            CT01-CTIAD  PICTURE  9(8).                    CI0026
            10            CT01-CLCUS  PICTURE  99.                      CI0026
            10            CT01-CAMMB  PICTURE  X(3).                    CI0026
            10            CT01-CKPMM  PICTURE  X.                       CI0026
            10            CT01-CTLAD  PICTURE  9(8).                    CI0026
            10            CT01-IPERS  PICTURE  X.                       CI0026
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0026
                          COMPUTATIONAL-3.                              CI0026
            10            CT01-CTLAT  PICTURE  9(8).                    CI0026
            10            CT01-CTLATC PICTURE  9(6).                    CI0026
            10            CT01-IMEGA  PICTURE  X.                       CI0026
            10            CT01-DIRAB  PICTURE  9(8).                    CI0026
            10            CT01-COLRQ  PICTURE  X.                       CI0026
            10            CT01-ZDA04  PICTURE  X(4).                    CI0026
            10            CT01-CTLPD  PICTURE  9(8).                    CI0026
            10            CT01-CIRASP PICTURE  9.                       CI0026
            10            CT01-CIRATP PICTURE  99.                      CI0026
            10            CT01-DRTHC  PICTURE  9(8).                    CI0026
            10            CT01-CPPTC  PICTURE  X.                       CI0026
            10            CT01-ZDA06  PICTURE  X(6).                    CI0026
            10            CT01-CTACD  PICTURE  9(8).                    CI0026
            10            CT01-CTNLI  PICTURE  X.                       CI0026
            10            CT01-CTRHO  PICTURE  9(8).                    CI0026
            10            CT01-CTSGD  PICTURE  9(8).                    CI0026
            10            CT01-CPATP  PICTURE  X(1).                    CI0026
            10            CT01-IRSTA  PICTURE  X.                       CI0026
            10            CT01-CTSTA  PICTURE  99.                      CI0026
            10            CT01-CTSSC  PICTURE  99.                      CI0026
            10            CT01-PRLIN  PICTURE  9(3).                    CI0026
            10            CT01-PRCOD  PICTURE  9(5).                    CI0026
            10            CT01-PRSCD  PICTURE  X(9).                    CI0026
            10            CT01-CTLNI  PICTURE  X.                       CI0026
            10            CT01-AYSIDA PICTURE  9(3).                    CI0026
            10            CT01-AYSID  PICTURE  9(5).                    CI0026
            10            CT01-CTBMC  PICTURE  99.                      CI0026
            10            CT01-CINAR  PICTURE  99.                      CI0026
            10            CT01-CPHTR  PICTURE  X.                       CI0026
            10            CT01-CDSTR  PICTURE  XX.                      CI0026
            10            CT01-CQACT  PICTURE  999.                     CI0026
            10            CT01-CIRAS  PICTURE  999.                     CI0026
            10            CT01-CIRAT  PICTURE  999.                     CI0026
            10            CT01-CLRAY  PICTURE  9(5).                    CI0026
            10            CT01-CATTP  PICTURE  X.                       CI0026
      ******************************************************************
      **  DATE WORK AREA USED WITH MACRO AADA53                        *
      ******************************************************************
      *!WF DSP=DD DSL=DD SEL=01 FOR=I LEV=1 PLT=DD
      ******************************************************************ADU029
      ***  STORAGE AREAS FOR DL1                                        ADU029
      ******************************************************************ADU029
      ***  DL1 FUNCTIONS                                                ADU029
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU029
       01                 XW05.                                         CI0026
            10            XW05-XW06.                                    CI0026
            11            XW05-XDBPCB.                                  CI0026
            12            XW05-XDBDNM PICTURE  X(08)                    CI0026
                          VALUE                SPACE.                   CI0026
            12            XW05-XSEGLV PICTURE  X(02)                    CI0026
                          VALUE                SPACE.                   CI0026
            12            XW05-XRC    PICTURE  X(02)                    CI0026
                          VALUE                SPACE.                   CI0026
            12            XW05-XPROPT PICTURE  X(04)                    CI0026
                          VALUE                SPACE.                   CI0026
            12            XW05-FILLER PICTURE  S9(5)                    CI0026
                          VALUE                ZERO                     CI0026
                          BINARY.                                       CI0026
            12            XW05-XSEGNM PICTURE  X(08)                    CI0026
                          VALUE                SPACE.                   CI0026
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0026
                          VALUE                ZERO                     CI0026
                          BINARY.                                       CI0026
            12            XW05-XSEGNB PICTURE  9(05)                    CI0026
                          VALUE                ZERO                     CI0026
                          BINARY.                                       CI0026
            12            XW05-XCOKEY PICTURE  X(70)                    CI0026
                          VALUE                SPACE.                   CI0026
            10            XW05-XW07.                                    CI0026
            11            XW05-XIOPCB.                                  CI0026
            12            XW05-XTERMI PICTURE  X(08)                    CI0026
                          VALUE                SPACE.                   CI0026
            12            XW05-FILLER PICTURE  XX                       CI0026
                          VALUE                SPACE.                   CI0026
            12            XW05-XRC1   PICTURE  X(02)                    CI0026
                          VALUE                SPACE.                   CI0026
            12            XW05-FILLER PICTURE  X(12)                    CI0026
                          VALUE                SPACE.                   CI0026
            12            XW05-XMODNM PICTURE  X(8)                     CI0026
                          VALUE                SPACE.                   CI0026
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0026
                          VALUE                ZERO.                    CI0026
            10            XW05-XGU    PICTURE  X(4)                     CI0026
                          VALUE                'GU  '.                  CI0026
            10            XW05-XGHU   PICTURE  X(4)                     CI0026
                          VALUE                'GHU '.                  CI0026
            10            XW05-XGN    PICTURE  X(4)                     CI0026
                          VALUE                'GN  '.                  CI0026
            10            XW05-XGHN   PICTURE  X(4)                     CI0026
                          VALUE                'GHN '.                  CI0026
            10            XW05-XGNP   PICTURE  X(4)                     CI0026
                          VALUE                'GNP '.                  CI0026
            10            XW05-XGHNP  PICTURE  X(4)                     CI0026
                          VALUE                'GHNP'.                  CI0026
            10            XW05-XREPL  PICTURE  XXXX                     CI0026
                          VALUE                'REPL'.                  CI0026
            10            XW05-XISRT  PICTURE  X(4)                     CI0026
                          VALUE                'ISRT'.                  CI0026
            10            XW05-XDLET  PICTURE  X(4)                     CI0026
                          VALUE                'DLET'.                  CI0026
            10            XW05-XOPEN  PICTURE  X(4)                     CI0026
                          VALUE                'OPEN'.                  CI0026
            10            XW05-XCLSE  PICTURE  X(4)                     CI0026
                          VALUE                'CLSE'.                  CI0026
            10            XW05-XCHKP  PICTURE  X(4)                     CI0026
                          VALUE                'CHKP'.                  CI0026
            10            XW05-XXRST  PICTURE  X(4)                     CI0026
                          VALUE                'XRST'.                  CI0026
            10            XW05-XTERM  PICTURE  X(4)                     CI0026
                          VALUE                'TERM'.                  CI0026
            10            XW05-XNFPAC PICTURE  X(13)                    CI0026
                          VALUE                SPACE.                   CI0026
      *!WI pl=DL200                                                     ADU029
       01  DL01-KFPCB                                                   ADU029
                        PICTURE X(04)                                   CI0026
              VALUE 'PCB '.                                             ADU029
      ***  SAVE AREA FOR DL1 FUNCTION VALUE - USED FOR ERROR PROCESSING ADU029
       01  SV01-FUNC     PIC X(4).                                      ADU029
      ******************************************************************ADU029
      *** USED WHEN DYNAMICALLY CALLING PCB & UIB ERROR CHECKING MODULESADU029
      ***    (CI0008P - UIB ERROR CHECK    CI0009P - PCB ERROR CHECK)   ADU029
      ******************************************************************ADU029
      *!WI pl=DN100                                                     ADU029
       01  W-PASS-XPROGR                                                ADU029
                        PICTURE X(8).                                   CI0026
      *GENERATE INDEX FOR MESSAGE TEXT (ONLY IF 'OCCURS' SPECIFIED)     ADU071
      *                   MS03                                          ADU071
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
                                                                        AM0028
      ******************************************************************AM0028
      **     PCB ADDRESS LIST FOR CI0028.  MODULE CI0028 WILL NEED     *AM0028
      **     PCB'S FOR:                                                *AM0028
      **                CONTRACT DATABASE(CT1P)                        *AM0028
      **                CLIENT DATABASE(CL1P)                          *AM0028
      **                GROUP DATABASE(GR1P)                           *AM0028
      ******************************************************************AM0028
                                                                        AM0028
       01  CI0028A-PCB-ADDRESS-LIST.                                    AM0028
           05  CI0028A-PCB-CT1P-PTR1      POINTER.                      AM0028
           05  CI0028A-PCB-CL1P-PTR1      POINTER.                      AM0028
           05  CI0028A-PCB-GR1P-PTR1      POINTER.                      AM0028
                                                                        AM0027
      ******************************************************************AM0027
      **     PCB ADDRESS LIST FOR CI0027.  MODULE CI0027 WILL NEED     *AM0027
      **     PCB'S FOR:                                                *AM0027
      **                CONTRACT DATABASE(CT1P)                        *AM0027
      ******************************************************************AM0027
                                                                        AM0027
       01  CI0027B-PCB-ADDRESS-LIST.                                    AM0027
           05  CI0027B-PCB-CT1P-PTR1      POINTER.                      AM0027
      *                                                                 AMDU35
      ******************************************************************AMDU35
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU35
      **     TAX MARKET SEGMENT FOR AN ACCOUNT NUMBER PASSED.          *AMDU35
      ******************************************************************AMDU35
      *                                                                 AMDU35
      *!WF DSP=TM DSL=DU SEL=35 FOR=I LEV=1                             AMDU35
       01                 TM00.                                         CI0026
          05              TM00-SUITE.                                   CI0026
            15       FILLER         PICTURE  X(00237).                  CI0026
       01                 TM35  REDEFINES      TM00.                    CI0026
            10            TM35-C299.                                    CI0026
            11            TM35-CTID.                                    CI0026
            12            TM35-CTIDA  PICTURE  9(3).                    CI0026
            12            TM35-CTIDN.                                   CI0026
            13            TM35-CTIDNP PICTURE  X(13).                   CI0026
            13            TM35-CTIDND PICTURE  9(11).                   CI0026
            10            TM35-DCACG  PICTURE  9(8).                    CI0026
            10            TM35-FILLER PICTURE  X(100).                  CI0026
            10            TM35-CTXMT  PICTURE  9(2).                    CI0026
            10            TM35-CTCUS  PICTURE  999.                     CI0026
            10            TM35-CGRMF  PICTURE  X.                       CI0026
            10            TM35-GRPLC  PICTURE  99.                      CI0026
            10            TM35-GRPLT  PICTURE  99.                      CI0026
            10            TM35-FILLER PICTURE  X(092).                  CI0026
      *                                                                 AMDU35
      *                                                                 AMDU35
      *                                                                 AMDU35
      *                                                                 AMDU35
      *                                                                 AMDU34
      ******************************************************************AMDU34
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU34
      **     CT22 SEGMENTS FOR AN ACCOUNT NUMBER PASSED.               *AMDU34
      ******************************************************************AMDU34
      *                                                                 AMDU34
      *!WF DSP=WA DSL=DU SEL=34 FOR=I LEV=1                             AMDU34
       01                 WA00.                                         CI0026
          05              WA00-SUITE.                                   CI0026
            15       FILLER         PICTURE  X(00541).                  CI0026
       01                 WA34  REDEFINES      WA00.                    CI0026
            10            WA34-C299.                                    CI0026
            11            WA34-CTID.                                    CI0026
            12            WA34-CTIDA  PICTURE  9(3).                    CI0026
            12            WA34-CTIDN.                                   CI0026
            13            WA34-CTIDNP PICTURE  X(13).                   CI0026
            13            WA34-CTIDND PICTURE  9(11).                   CI0026
            10            WA34-DCACG  PICTURE  9(8).                    CI0026
            10            WA34-IPOCH  PICTURE  X.                       CI0026
            10            WA34-FILLER PICTURE  X(100).                  CI0026
            10            WA34-CT22                                     CI0026
                          OCCURS       010     TIMES.                   CI0026
            11            WA34-CT22K.                                   CI0026
            12            WA34-CGVEN  PICTURE  X(2).                    CI0026
            12            WA34-CTWHC  PICTURE  9(2).                    CI0026
            11            WA34-CFCNTY PICTURE  X(2).                    CI0026
            11            WA34-DLAUP  PICTURE  9(8).                    CI0026
            11            WA34-CTWTC  PICTURE  9(2).                    CI0026
            11            WA34-CTWHAT PICTURE  S9(7)V99                 CI0026
                          COMPUTATIONAL-3.                              CI0026
            11            WA34-CTWHP  PICTURE  9(3)V99                  CI0026
                          COMPUTATIONAL-3.                              CI0026
            11            WA34-FILLER PICTURE  X(06).                   CI0026
            10            WA34-QITEM  PICTURE  9(3).                    CI0026
            10            WA34-XIMAX  PICTURE  S9(4)                    CI0026
                          BINARY.                                       CI0026
            10            WA34-CTXMT  PICTURE  9(2).                    CI0026
            10            WA34-CTCUS  PICTURE  999.                     CI0026
            10            WA34-FILLER PICTURE  X(95).                   CI0026
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
      *
      ******************************************************************
      **     WORKING STORAGE MISC FIELDS                               *
      ******************************************************************
      *
       01  W-WORK-MISC.
      *!WI
           05  W-BACK-DCACG
                        PICTURE 9(8).                                   CI0026
      *
      *
      *
      *
       01   DEBUT-WSS.                                                  CI0026
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0026
            05   IK     PICTURE X.                                      CI0026
       01  CONSTANTES-PAC.                                              CI0026
           05  FILLER  PICTURE X(87)   VALUE                            CI0026
                     '6015 CAT09/08/14CI0026ADMIN   14:34:19CI0026P AMERCI0026
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0026
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0026
           05  NUGNA   PICTURE X(5).                                    CI0026
           05  APPLI   PICTURE X(3).                                    CI0026
           05  DATGN   PICTURE X(8).                                    CI0026
           05  PROGR   PICTURE X(6).                                    CI0026
           05  CODUTI  PICTURE X(8).                                    CI0026
           05  TIMGN   PICTURE X(8).                                    CI0026
           05  PROGE   PICTURE X(8).                                    CI0026
           05  COBASE  PICTURE X(4).                                    CI0026
           05  DATGNC  PICTURE X(10).                                   CI0026
           05  RELEAS  PICTURE X(7).                                    CI0026
           05  DATGE   PICTURE X(10).                                   CI0026
           05  DATSQ   PICTURE X(10).                                   CI0026
       01  DATCE.                                                       CI0026
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0026
         05  DATOR.                                                     CI0026
           10  DATOA  PICTURE XX.                                       CI0026
           10  DATOM  PICTURE XX.                                       CI0026
           10  DATOJ  PICTURE XX.                                       CI0026
       01   VARIABLES-CONDITIONNELLES.                                  CI0026
            05                  FT      PICTURE X VALUE '0'.            CI0026
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0026
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0026
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0026
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0026
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0026
       01               S-CL01-SSA.                                     CI0026
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0026
                                      VALUE 'CL01    '.                 CI0026
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0026
            10          S-CL01-CCOD   PICTURE X(5)                      CI0026
                                      VALUE '-----'.                    CI0026
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0026
       01            S-CLU01-SSA.                                       CI0026
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0026
                                      VALUE 'CL01    '.                 CI0026
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0026
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0026
                                      VALUE '-----'.                    CI0026
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0026
                                      VALUE '(CL01K'.                   CI0026
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0026
            10       S-CLU01-CL01K.                                     CI0026
            11       S-CLU01-C199.                                      CI0026
            12       S-CLU01-CLID.                                      CI0026
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0026
            13       S-CLU01-CLIDN.                                     CI0026
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0026
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0026
            10  FILLER   PICTURE X    VALUE ')'.                        CI0026
       01               S-CT01-SSA.                                     CI0026
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0026
                                      VALUE 'CT01    '.                 CI0026
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0026
            10          S-CT01-CCOD   PICTURE X(5)                      CI0026
                                      VALUE '-----'.                    CI0026
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0026
       01            S-CTU01-SSA.                                       CI0026
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0026
                                      VALUE 'CT01    '.                 CI0026
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0026
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0026
                                      VALUE '-----'.                    CI0026
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0026
                                      VALUE '(CT01K'.                   CI0026
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0026
            10       S-CTU01-CT01K.                                     CI0026
            11       S-CTU01-C299.                                      CI0026
            12       S-CTU01-CTID.                                      CI0026
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0026
            13       S-CTU01-CTIDN.                                     CI0026
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0026
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0026
            10  FILLER   PICTURE X    VALUE ')'.                        CI0026
       01   ZONES-UTILISATEUR PICTURE X.                                CI0026
       LINKAGE SECTION.                                                 ADU102
      ***  DLIUIBII COPYBOOK                                            ADU029
            COPY  DLIUIBII.                                             ADU029
      *                                                                 ADU029
      *** ADDRESS LIST OF PCB'S                                         ADU029
       01   PCB-ADDRESS-LIST.                                           ADU029
      ** ALL PCB POINTERS MUST BE ADDED HERE USING MACRO ADU015         ADU029
      **  ONCE FOR EACH DATABASE USED                                   ADU029
      *                                                                 ADU029
      ** FOLLOWING ALL PCB POINTERS, INCLUDE PCB MASKS (USE MACRO       ADU029
      **  ADU015) FOR EACH DATABASE USED                                ADU029
      ******************************************************************ADU029
      ******************************************************************ADU029
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR GR1P                                           ADU015
            05 PCB-GR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0026
          05              PA00-SUITE.                                   CI0026
            15       FILLER         PICTURE  X(00106).                  CI0026
       01                 PA06  REDEFINES      PA00.                    CI0026
            10            PA06-XDBPCB.                                  CI0026
            11            PA06-XDBDNM PICTURE  X(08).                   CI0026
            11            PA06-XSEGLV PICTURE  X(02).                   CI0026
            11            PA06-XRC    PICTURE  X(02).                   CI0026
            11            PA06-XPROPT PICTURE  X(04).                   CI0026
            11            PA06-FILLER PICTURE  S9(5)                    CI0026
                          BINARY.                                       CI0026
            11            PA06-XSEGNM PICTURE  X(08).                   CI0026
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0026
                          BINARY.                                       CI0026
            11            PA06-XSEGNB PICTURE  9(05)                    CI0026
                          BINARY.                                       CI0026
            11            PA06-XCOKEY PICTURE  X(70).                   CI0026
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0026
          05              PB00-SUITE.                                   CI0026
            15       FILLER         PICTURE  X(00106).                  CI0026
       01                 PB06  REDEFINES      PB00.                    CI0026
            10            PB06-XDBPCB.                                  CI0026
            11            PB06-XDBDNM PICTURE  X(08).                   CI0026
            11            PB06-XSEGLV PICTURE  X(02).                   CI0026
            11            PB06-XRC    PICTURE  X(02).                   CI0026
            11            PB06-XPROPT PICTURE  X(04).                   CI0026
            11            PB06-FILLER PICTURE  S9(5)                    CI0026
                          BINARY.                                       CI0026
            11            PB06-XSEGNM PICTURE  X(08).                   CI0026
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0026
                          BINARY.                                       CI0026
            11            PB06-XSEGNB PICTURE  9(05)                    CI0026
                          BINARY.                                       CI0026
            11            PB06-XCOKEY PICTURE  X(70).                   CI0026
      *** PCB MASK FOR GR1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0026
          05              PC00-SUITE.                                   CI0026
            15       FILLER         PICTURE  X(00106).                  CI0026
       01                 PC06  REDEFINES      PC00.                    CI0026
            10            PC06-XDBPCB.                                  CI0026
            11            PC06-XDBDNM PICTURE  X(08).                   CI0026
            11            PC06-XSEGLV PICTURE  X(02).                   CI0026
            11            PC06-XRC    PICTURE  X(02).                   CI0026
            11            PC06-XPROPT PICTURE  X(04).                   CI0026
            11            PC06-FILLER PICTURE  S9(5)                    CI0026
                          BINARY.                                       CI0026
            11            PC06-XSEGNM PICTURE  X(08).                   CI0026
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0026
                          BINARY.                                       CI0026
            11            PC06-XSEGNB PICTURE  9(05)                    CI0026
                          BINARY.                                       CI0026
            11            PC06-XCOKEY PICTURE  X(70).                   CI0026
      *                                                                 AMDU34
      ******************************************************************AMDU34
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU34
      **     CT22 SEGMENTS FOR AN ACCOUNT NUMBER PASSED.               *AMDU34
      ******************************************************************AMDU34
      *                                                                 AMDU34
      *!WF DSP=AW DSL=DU SEL=34 FOR=I LEV=1                             AMDU34
       01                 AW00.                                         CI0026
          05              AW00-SUITE.                                   CI0026
            15       FILLER         PICTURE  X(00541).                  CI0026
       01                 AW34  REDEFINES      AW00.                    CI0026
            10            AW34-C299.                                    CI0026
            11            AW34-CTID.                                    CI0026
            12            AW34-CTIDA  PICTURE  9(3).                    CI0026
            12            AW34-CTIDN.                                   CI0026
            13            AW34-CTIDNP PICTURE  X(13).                   CI0026
            13            AW34-CTIDND PICTURE  9(11).                   CI0026
            10            AW34-DCACG  PICTURE  9(8).                    CI0026
            10            AW34-IPOCH  PICTURE  X.                       CI0026
            10            AW34-FILLER PICTURE  X(100).                  CI0026
            10            AW34-CT22                                     CI0026
                          OCCURS       010     TIMES.                   CI0026
            11            AW34-CT22K.                                   CI0026
            12            AW34-CGVEN  PICTURE  X(2).                    CI0026
            12            AW34-CTWHC  PICTURE  9(2).                    CI0026
            11            AW34-CFCNTY PICTURE  X(2).                    CI0026
            11            AW34-DLAUP  PICTURE  9(8).                    CI0026
            11            AW34-CTWTC  PICTURE  9(2).                    CI0026
            11            AW34-CTWHAT PICTURE  S9(7)V99                 CI0026
                          COMPUTATIONAL-3.                              CI0026
            11            AW34-CTWHP  PICTURE  9(3)V99                  CI0026
                          COMPUTATIONAL-3.                              CI0026
            11            AW34-FILLER PICTURE  X(06).                   CI0026
            10            AW34-QITEM  PICTURE  9(3).                    CI0026
            10            AW34-XIMAX  PICTURE  S9(4)                    CI0026
                          BINARY.                                       CI0026
            10            AW34-CTXMT  PICTURE  9(2).                    CI0026
            10            AW34-CTCUS  PICTURE  999.                     CI0026
            10            AW34-FILLER PICTURE  X(95).                   CI0026
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU14
      ******************************************************************AMDU14
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU14
      **     REQUESTED TYPE OF CLIENTS FOR THE ACCOUNT NUMBER PASSED.  *AMDU14
      ******************************************************************AMDU14
      *                                                                 AMDU14
      *!WF DSP=AC DSL=DU SEL=14 FOR=I LEV=1                             AMDU14
       01                 AC00.                                         CI0026
          05              AC00-SUITE.                                   CI0026
            15       FILLER         PICTURE  X(00917).                  CI0026
       01                 AC14  REDEFINES      AC00.                    CI0026
            10            AC14-C299.                                    CI0026
            11            AC14-CTID.                                    CI0026
            12            AC14-CTIDA  PICTURE  9(3).                    CI0026
            12            AC14-CTIDN.                                   CI0026
            13            AC14-CTIDNP PICTURE  X(13).                   CI0026
            13            AC14-CTIDND PICTURE  9(11).                   CI0026
            10            AC14-DCACG  PICTURE  9(8).                    CI0026
            10            AC14-IPOCH  PICTURE  X.                       CI0026
            10            AC14-FILLER PICTURE  X(100).                  CI0026
            10            AC14-CLID01.                                  CI0026
            11            AC14-CLIDO1 PICTURE  X(3).                    CI0026
            11            AC14-NCLID1.                                  CI0026
            12            AC14-CLIDP1 PICTURE  X(12).                   CI0026
            12            AC14-CLIDNA PICTURE  9(8).                    CI0026
            10            AC14-CLCTR  PICTURE  9(3).                    CI0026
            10            AC14-DU21                                     CI0026
                          OCCURS       025     TIMES.                   CI0026
            11            AC14-C199.                                    CI0026
            12            AC14-CLID.                                    CI0026
            13            AC14-CLIDO  PICTURE  9(3).                    CI0026
            13            AC14-CLIDN.                                   CI0026
            14            AC14-CLIDNP PICTURE  X(12).                   CI0026
            14            AC14-CLIDND PICTURE  9(8).                    CI0026
            11            AC14-CLCTRC PICTURE  9(3).                    CI0026
            10            AC14-QITEM  PICTURE  9(3).                    CI0026
            10            AC14-XIMAX  PICTURE  S9(4)                    CI0026
                          BINARY.                                       CI0026
            10            AC14-CRROL  PICTURE  X.                       CI0026
            10            AC14-FILLER PICTURE  X(099).                  CI0026
      *                                                                 AMDU14
      *                                                                 AMDU14
      *                                                                 AMDU14
      *                                                                 AMDU14
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0026
          05              DE00-SUITE.                                   CI0026
            15       FILLER         PICTURE  X(00653).                  CI0026
       01                 DE10  REDEFINES      DE00.                    CI0026
            10            DE10-DU11.                                    CI0026
            11            DE10-XFONC  PICTURE  X(4).                    CI0026
            11            DE10-MPSBN  PICTURE  X(8).                    CI0026
            11            DE10-XDBDNM PICTURE  X(08).                   CI0026
            11            DE10-XSEGNM PICTURE  X(08).                   CI0026
            11            DE10-XRC    PICTURE  X(02).                   CI0026
            11            DE10-MSEG   PICTURE  X(08).                   CI0026
            11            DE10-XCOKEY PICTURE  X(70).                   CI0026
            11            DE10-CUIBR  PICTURE  X(01).                   CI0026
            11            DE10-CUIBA  PICTURE  X(01).                   CI0026
            11            DE10-IPBIK  PICTURE  X(1).                    CI0026
            10            DE10-DU03.                                    CI0026
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0026
                          COMPUTATIONAL-3.                              CI0026
            11            DE10-CMSSF  PICTURE  XX.                      CI0026
            11            DE10-DU09.                                    CI0026
            12            DE10-CMESA  PICTURE  S9(9)                    CI0026
                          BINARY.                                       CI0026
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0026
                          BINARY.                                       CI0026
            12            DE10-CMESB  PICTURE  S9(9)                    CI0026
                          BINARY.                                       CI0026
            12            DE10-CMSST  PICTURE  S9(9)                    CI0026
                          BINARY.                                       CI0026
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0026
                          BINARY.                                       CI0026
            12            DE10-QELLAA PICTURE  S9(9)                    CI0026
                          BINARY.                                       CI0026
            12            DE10-TMESS4 PICTURE  X(512).                  CI0026
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
       01                 MS00.                                         CI0026
          05              MS00-SUITE.                                   CI0026
            15       FILLER         PICTURE  X(00542).                  CI0026
       01                 MS03  REDEFINES      MS00.                    CI0026
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0026
                          COMPUTATIONAL-3.                              CI0026
            10            MS03-CMSSF  PICTURE  XX.                      CI0026
            10            MS03-DU09.                                    CI0026
            11            MS03-CMESA  PICTURE  S9(9)                    CI0026
                          BINARY.                                       CI0026
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0026
                          BINARY.                                       CI0026
            11            MS03-CMESB  PICTURE  S9(9)                    CI0026
                          BINARY.                                       CI0026
            11            MS03-CMSST  PICTURE  S9(9)                    CI0026
                          BINARY.                                       CI0026
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0026
                          BINARY.                                       CI0026
            11            MS03-QELLAA PICTURE  S9(9)                    CI0026
                          BINARY.                                       CI0026
            11            MS03-TMESS4 PICTURE  X(512).                  CI0026
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0026
            10            MX11-QMSGS  PICTURE  9(03).                   CI0026
            10            MX11-PJ09                                     CI0026
                          OCCURS       025     TIMES.                   CI0026
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0026
                          COMPUTATIONAL-3.                              CI0026
            11            MX11-CMESB  PICTURE  S9(9)                    CI0026
                          BINARY.                                       CI0026
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                AW34
                                AC14
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0026
      *               *                                   *             CI0026
      *               *INITIALISATIONS                    *             CI0026
      *               *                                   *             CI0026
      *               *************************************.            CI0026
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
      *N02CA.    NOTE *INIT NUMBER OF CT22 SEGS PASSED    *.
       F02CA.                                                           lv10
      *
      *********************************
      ** INITIALIZE THE NUMBER OF CT22*
      ** SEGMENTS THAT WILL BE PASSED *
      ** BACK TO THE CALLING MODULE   *
      *********************************
      *
           MOVE        ZEROS TO AW34-QITEM.
       F02CA-FN. EXIT.
      *N02XA.    NOTE *SET ADDRESSES FOR DATABASES        *.
       F02XA.                                                           lv10
      *
      *********************************
      ** SET ADDRESSES FOR DATABASES  *
      *********************************
      *.
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR GR1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-GR1P-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0026
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0026
      *               *                                   *             CI0026
      *               *FIN DE TRAITEMENT                  *             CI0026
      *               *                                   *             CI0026
      *               *************************************.            CI0026
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0026
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *GET DATA FOR ACCOUNT               *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *
      *********************************
      ** CALL MODULE CI0028 TO GET    *
      ** THE TAX MARKET FOR THE       *
      ** ACCOUNT(USED TO SEE IF       *
      ** BACKUP WITHHOLDING CAN APPLY *
      ** TO THIS PERSONAL ACCOUNT)    *
      ** CALL MODULE CI0027 TO GET    *
      ** THE ACCOUNT CT22 SEGMENTS.   *
      *********************************
      *
      *N50BA.    NOTE *CALL CI0028 - DETERMINE TAX MKT    *.            AM0028
       F50BA.                                                           lv10
      *                                                                 AM0028
      *********************************                                 AM0028
      ** THIS MODULE WILL DETERMINE   *                                 AM0028
      ** THE TAX MARKET FOR THE       *                                 AM0028
      ** ACCOUNT NUMBER.              *                                 AM0028
      *********************************                                 AM0028
      *                                                                 AM0028
           INITIALIZE      TM35                                         AM0028
           MOVE        AW34-CTID TO TM35-CTID                           AM0028
           MOVE        AW34-DCACG TO TM35-DCACG                         AM0028
           SET CI0028A-PCB-CT1P-PTR1 TO                                 AM0028
                       PCB-CT1P-PTR1                                    AM0028
           SET CI0028A-PCB-CL1P-PTR1 TO                                 AM0028
                       PCB-CL1P-PTR1                                    AM0028
           SET CI0028A-PCB-GR1P-PTR1 TO                                 AM0028
                       PCB-GR1P-PTR1                                    AM0028
           INITIALIZE      DE10-DU03                                    AM0028
           CALL        CI0028 USING                                     AM0028
           DFHEIBLK                                                     AM0028
           DFHCOMMAREA                                                  AM0028
           DLIUIBII                                                     AM0028
           CI0028A-PCB-ADDRESS-LIST                                     AM0028
           TM35                                                         AM0028
           DE10                                                         AM0028
           MS03.                                                        AM0028
      *N50CA.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F50CA.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F50CA-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0028 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0028 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F50CA-900. GO TO F50CC-FN.
       F50CA-FN. EXIT.
      *N50CC.    NOTE *NO ERRORS                          *.            ADU071
       F50CC.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F50CC-FN. EXIT.
       F50BA-FN. EXIT.
      *N50DA.    NOTE *CALL CI0027 - ACCT CT22 SEGMENTS   *.            AM0027
       F50DA.                                                           lv10
      *                                                                 AM0027
      *********************************                                 AM0027
      ** THIS MODULE WILL READ THE    *                                 AM0027
      ** CONTRACT DATABASE TO GET THE *                                 AM0027
      ** CT22 SEGMENTS FOR THE ACCOUNT*                                 AM0027
      ** NUMBER.                      *                                 AM0027
      *********************************                                 AM0027
      *                                                                 AM0027
           INITIALIZE      WA34                                         AM0027
           MOVE        AW34-CTID TO WA34-CTID                           AM0027
           MOVE        AW34-DCACG TO WA34-DCACG                         AM0027
           MOVE        10 TO WA34-XIMAX                                 AM0027
           MOVE        'Y' TO WA34-IPOCH                                AM0027
           SET CI0027B-PCB-CT1P-PTR1 TO                                 AM0027
                       PCB-CT1P-PTR1                                    AM0027
           INITIALIZE      DE10-DU03                                    AM0027
           CALL        CI0027 USING                                     AM0027
           DFHEIBLK                                                     AM0027
           DFHCOMMAREA                                                  AM0027
           DLIUIBII                                                     AM0027
           CI0027B-PCB-ADDRESS-LIST                                     AM0027
           WA34                                                         AM0027
           DE10                                                         AM0027
           MS03.                                                        AM0027
      *N50EA.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F50EA.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F50EA-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0027 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0027 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F50EA-900. GO TO F50EC-FN.
       F50EA-FN. EXIT.
      *N50EC.    NOTE *NO ERRORS                          *.            ADU071
       F50EC.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F50EC-FN. EXIT.
       F50DA-FN. EXIT.
      *N50FA.    NOTE *MOVE WA34 TO AW34                  *.
       F50FA.                                                           lv10
      *
      *********************************
      ** MOVE CT22 SEGMENTS           *
      *********************************
      *
           MOVE        WA34 TO AW34.
       F50FA-FN. EXIT.
      *N50FC.    NOTE *MOVE 2 FIELDS FROM CI0028 CALL     *.
       F50FC.                                                           lv10
           MOVE        TM35-CTXMT TO AW34-CTXMT
           MOVE        TM35-CTCUS TO AW34-CTCUS.
       F50FC-FN. EXIT.
       F50-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *CHECK IF PERSONAL ACCOUNT          *
      *               *                                   *
      *               *************************************.
       F60.      IF    TM35-CTXMT = 01                                  lv05
                 NEXT SENTENCE ELSE GO TO     F60-FN.
      *
      *********************************
      ** IF IT IS A PERSONAL ACCOUNT, *
      ** CHECK THE TIN CERTIFICATION  *
      ** CODE TO SEE IF BACKUP        *
      ** WITHHOLDING SHOULD BE APPLIED*
      *********************************
      *
      *N60DA.    NOTE *SET UP SSA FOR CL01 READ           *.
       F60DA.                                                           lv10
      *
      *********************************
      ** SET UP SSA FOR CL01 READ     *
      ** USING TAXPAYER CLIENT ID     *
      ** FROM MODULE CI0018.          *
      *********************************
      *
           MOVE        AC14-CLID01 TO S-CLU01-CLID.
       F60DA-FN. EXIT.
      *N60EA.    NOTE *READ CL01 SEGMENT                  *.
       F60EA.                                                           lv10
      *
      *********************************
      ** READ CL01 SEGMENT            *
      *********************************
      *
           PERFORM     F94CL THRU F94CL-FN.
       F60EA-FN. EXIT.
      *N60FA.    NOTE *CHECK IF CLIENT ID NOT FOUND       *.
       F60FA.    IF    IK = '1'                                         lv10
                 OR    DE10-NMESS2 NOT = ZEROS
                 NEXT SENTENCE ELSE GO TO     F60FA-FN.
      *********************************
      ** IF CLIENT NUMBER  NOT FOUND  *
      ** ON THE CLIENT DATABASE,      *
      ** GET APPROPRIATE MESSAGE INFO *
      ** BY CALLING CI0002 AND RETURN *
      ** TO CALLING MODULE.           *
      *********************************
      *
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012012 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98IC THRU F98IC-FN.                             ADU019
      *N60FZ.    NOTE *RETURN TO CALLING MODULE           *.
       F60FZ.                                                           lv15
      *
      *********************************
      ** RETURN TO CALLING MODULE     *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F60FZ-FN. EXIT.
       F60FA-FN. EXIT.
      *N60GA.    NOTE *NO W/H ON CERTIFIED TIN NUMBER     *.
       F60GA.    IF    CL01-CLTINC = 2                                  lv10
                 NEXT SENTENCE ELSE GO TO     F60GA-FN.
      *
      *********************************
      ** NO BACKUP W/H ON CERTIFIED   *
      ** TIN NUMBER.                  *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F60GA-FN. EXIT.
      *N60HA.    NOTE *NO W/H ON WHEN IRS REQUIRES W/H    *.
       F60HA.    IF    CL01-CLTINC = 3                                  lv10
                 NEXT SENTENCE ELSE GO TO     F60HA-FN.
      *
      *********************************
      ** NO BACKUP W/H ON WHEN IRS    *
      ** REQUIRES W/H.                *
      ** STRANGE BUT THAT IS HOW      *
      ** SHARK IS DOING IT.           *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F60HA-FN. EXIT.
      *N60IA.    NOTE *TIN NOT CERTIFIED OR REQUESTED     *.
       F60IA.    IF    CL01-CLTINC = 1 OR 5                             lv10
                 NEXT SENTENCE ELSE GO TO     F60IA-FN.
      *
      *********************************
      ** READ CT01 SEGMENT WHEN TIN   *
      ** NOT CERTIFIED OR TIN HAS     *
      ** BEEN REQUESTED.              *
      *********************************
      *
      *N60JA.    NOTE *SET UP SSA FOR CT01 READ           *.
       F60JA.                                                           lv15
      *
      *********************************
      ** SET UP SSA FOR CT01 READ     *
      ** USING ACCOUNT ID NUMBER      *
      ** PASSED FROM CALLING MODULE   *
      *********************************
      *
           MOVE        AW34-CTID TO S-CTU01-CTID.
       F60JA-FN. EXIT.
      *N60KA.    NOTE *READ CT01 SEGMENT                  *.
       F60KA.                                                           lv15
      *
      *********************************
      ** READ CT01 SEGMENT            *
      *********************************
      *
           PERFORM     F94CT THRU F94CT-FN.
       F60KA-FN. EXIT.
      *N60LA.    NOTE *CHECK IF ACCT NUMBER NOT FOUND     *.
       F60LA.    IF    IK = '1'                                         lv15
                 OR    DE10-NMESS2 NOT = ZEROS
                 NEXT SENTENCE ELSE GO TO     F60LA-FN.
      *********************************
      ** IF ACCOUNT NUMBER NOT FOUND  *
      ** ON THE CONTRACT DATABASE     *
      ** GET APPROPRIATE MESSAGE INFO *
      ** BY CALLING CI0002 AND RETURN *
      ** TO CALLING MODULE.           *
      *********************************
      *
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012011 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98IC THRU F98IC-FN.                             ADU019
      *N60LZ.    NOTE *RETURN TO CALLING MODULE           *.
       F60LZ.                                                           lv20
      *
      *********************************
      ** RETURN TO CALLING MODULE     *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F60LZ-FN. EXIT.
       F60LA-FN. EXIT.
       F60IA-FN. EXIT.
      *N60MA.    NOTE *NO W/H BEFORE 19840101             *.
       F60MA.    IF    CL01-CLTINC = 1                                  lv10
                 AND   CL01-CLTIN NOT = ZEROS
                 AND   CT01-CTEFD < 19840101
                 NEXT SENTENCE ELSE GO TO     F60MA-FN.
      *********************************
      ** NO BACKUP WITHHOLDING IF THE *
      ** TIN IS NOT CERTIFIED BUT THE *
      ** ACCOUNT WAS OPENED BEFORE    *
      ** 19840101.                    *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F60MA-FN. EXIT.
      *N60NA.    NOTE *CHECK IF TIN HAS BEEN REQUESTED    *.
       F60NA.    IF    CL01-CLTINC = 5                                  lv10
                 NEXT SENTENCE ELSE GO TO     F60NA-FN.
      *
      *********************************
      ** IF TIN HAS BEEN REQUESTED,   *
      ** CALCULATE 60 DAYS BEFORE THE *
      ** CURRENT ACCOUNTING DATE      *
      *********************************
      *
           MOVE        AW34-DCACG TO DD34-DTGRGA
           MOVE        0 TO DD34-CDTUC
           MOVE        -060 TO DD34-NDTUN
           MOVE        9 TO DD30-CDTSF
           PERFORM     F95 THRU F95-FN
           MOVE        DD34-DTGRGB TO W-BACK-DCACG.
      *N60OA.    NOTE *NO W/H IF BEFORE ACCT OPEN DATE    *.
       F60OA.    IF    W-BACK-DCACG NOT >                               lv15
                       CT01-CTEFD
                 NEXT SENTENCE ELSE GO TO     F60OA-FN.
      *********************************
      ** NO BACKUP W/H IF THE ACCOUNT *
      ** HAS BEEN OPENED WITHIN THE   *
      ** LAST 60 DAYS.                *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F60OA-FN. EXIT.
       F60NA-FN. EXIT.
      *N60PA.    NOTE *BACKUP W/H IS REQUIRED             *.
       F60PA.                                                           lv10
      *
      *********************************
      ** BACKUP W/H IS REQUIRED.      *
      ** BUILD AN ENTRY IN THE CT22   *
      ** SEGMENT TABLE FOR THE BACKUP *
      ** W/H.                         *
      *********************************
      *
      *N60QA.    NOTE *ADD 1 TO TABLE LOAD INDEX          *.
       F60QA.                                                           lv15
      *
      *********************************
      ** ADD 1 TO THE CT22 TABLE      *
      ** LOAD INDEX.                  *
      *********************************
      *
           ADD         +1 TO AW34-QITEM.
       F60QA-FN. EXIT.
      *N60RA.    NOTE *CHECK IF TABLE OVERFLOW            *.
       F60RA.    IF    AW34-QITEM > AW34-XIMAX                          lv15
                 NEXT SENTENCE ELSE GO TO     F60RA-FN.
      *
      *********************************
      ** IF THERE IS A TABLE OVERFLOW *
      ** JUST GET OUT.                *
      *********************************
      *
           MOVE        AW34-XIMAX TO AW34-QITEM
           MOVE                     ALL '1' TO FT GO TO F20.
       F60RA-FN. EXIT.
      *N60SA.    NOTE *BUILD CT22 TABLE ENTRY             *.
       F60SA.                                                           lv15
      *
      *********************************
      ** BUILD THE CT22 TABLE ENTRY   *
      ** FOR BACKUP WITHHOLDING.      *
      *********************************
      *
           INITIALIZE
                     AW34-CT22(AW34-QITEM)
           MOVE        'US05' TO AW34-CT22K (AW34-QITEM)
           MOVE        02 TO AW34-CTWTC (AW34-QITEM)
           MOVE        31.00 TO AW34-CTWHP (AW34-QITEM).
       F60SA-FN. EXIT.
       F60PA-FN. EXIT.
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
      *N93.      NOTE *************************************.            ADU029
      *               *                                   *             ADU029
      *               *COMMON DL1 ERROR ROUTINES          *             ADU029
      *               *                                   *             ADU029
      *               *************************************.            ADU029
       F93.           EXIT.                                             lv05
      *N93EA.    NOTE *DATABASE I/O ERROR PROCESSING      *.            ADU029
       F93EA.                                                           lv10
                 IF    XW05-XRC = '  '                                  DOT
           MOVE        ZERO TO IK                                       ADU029
                 ELSE                                                   ADU029
           MOVE        '1' TO IK                                        ADU029
           DE10-IPBIK                                                   ADU029
           INITIALIZE        DE10-DU03                                  ADU029
           PERFORM     F93UI THRU F93UI-FN                              ADU029
           PERFORM     F93PC THRU F93PC-FN.                             ADU029
       F93EA-FN. EXIT.
      *N93PC.    NOTE *CHECK DL1 PCB - CALL CI0009P       *.            ADU029
       F93PC.                                                           lv10
           MOVE        SV01-FUNC TO DE10-XFONC                          ADU029
           MOVE        XW05-XRC TO DE10-XRC                             ADU029
           MOVE        XW05-XSEGNM TO DE10-MSEG                         ADU029
           MOVE        XW05-XCOKEY TO DE10-XCOKEY                       ADU029
           MOVE        XW05-XDBDNM TO DE10-XDBDNM                       ADU029
      *                                                                 ADU029
           MOVE        'CI0009P ' TO W-PASS-XPROGR                      ADU029
           CALL        W-PASS-XPROGR                                    ADU029
           USING DFHEIBLK                                               ADU029
           DFHCOMMAREA                                                  ADU029
           DE10                                                         ADU029
           MS03.                                                        ADU029
       F93PC-FN. EXIT.
      *N93UI.    NOTE *CHECK DL1 USER INTRFACE BLK(UIB)   *.            ADU029
       F93UI.                                                           lv10
      *>>> CALL CI0008P                                                 ADU029
           MOVE        SV01-FUNC TO DE10-XFONC                          ADU029
           MOVE        UIBFCTR TO DE10-CUIBR                            ADU029
           MOVE        UIBDLTR TO DE10-CUIBA                            ADU029
      *                                                                 ADU029
           MOVE        'CI0008P ' TO W-PASS-XPROGR                      ADU029
           CALL        W-PASS-XPROGR                                    ADU029
           USING DFHEIBLK                                               ADU029
           DFHCOMMAREA                                                  ADU029
           DE10                                                         ADU029
           MS03.                                                        ADU029
       F93UI-FN. EXIT.
       F93-FN.   EXIT.
      *N94.      NOTE *************************************.
      *               *                                   *
      *               *DL/I CALLS                         *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94CL.    NOTE *CALL GU ON CL01                    *.            ADU026
       F94CL.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 CL01                                                    ADU026
           S-CLU01-SSA                                                  ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CL-FN. EXIT.
      *N94CT.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94CT.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CT-FN. EXIT.
       F94-FN.   EXIT.
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *CALC STD DATE FROM CCYYMMDD        *
      *               *                                   *
      *               *************************************.
       F95.           EXIT.                                             lv05
      *N95BA.    NOTE *CDU - DATE DIFF/CALCULATION        *.            AADA82
       F95BA.                                                           lv10
      ** * * * * * * * * * * * * * * *                                  AADA82
      *This code calls the common date                                  AADA82
      *utility MWS100EX to calculate                                    AADA82
      *the difference between 2 dates                                   AADA82
      *or calculate a new date (add/                                    AADA82
      *subtract days). It uses a                                        AADA82
      *dynamic call.                                                    AADA82
      ** * * * * * * * * * * * * * * *                                  AADA82
      *Before the call set the subfunc                                  AADA82
      *request code DD30-CDTSF:                                         AADA82
      *  8 = date difference                                            AADA82
      *  9 = date add/subtract days                                     AADA82
      ** * * * * * * * * * * * * * * *                                  AADA82
      *Check return code DD30-CDTSC                                     AADA82
      *after the call.                                                  AADA82
      *    0 = Error Free                                               AADA82
      *    3 = Invalid Date                                             AADA82
      *    5 = Invalid Day                                              AADA82
      *    6 = Invalid Month                                            AADA82
      ** * * * * * * * * * * * * * * *                                  AADA82
           MOVE        4 TO DD30-CDTFN                                  AADA82
           CALL        MWS100EX USING DD30                              AADA82
           DD34.                                                        AADA82
       F95BA-FN. EXIT.
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
       F98-FN.   EXIT.
