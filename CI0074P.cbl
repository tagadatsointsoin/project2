       IDENTIFICATION DIVISION.                                         CI0074
       PROGRAM-ID.  CI0074P.                                            CI0074
      *AUTHOR.         TAX SETTLEMENT CODE MODULE.                      CI0074
      *DATE-COMPILED.   09/08/14.                                       CI0074
       ENVIRONMENT DIVISION.                                            CI0074
       CONFIGURATION SECTION.                                           CI0074
       SOURCE-COMPUTER. IBM-370.                                        CI0074
       OBJECT-COMPUTER. IBM-370.                                        CI0074
       DATA DIVISION.                                                   CI0074
       WORKING-STORAGE SECTION.                                         CI0074
      *>>>>>>> Audit Log Work Area                                      ADU165
                                                                        ADU165
       01               AL00-ADDR.                                      ADU165
              05        AL00-NPNTR     USAGE IS POINTER.                ADU165
                                                                        ADU165
      *!WI pl=AL005                                                     ADU165
       01               AL00-NSEQ2P    VALUE ZERO                       ADU165
                        PICTURE S9(3)                                   CI0074
                          COMPUTATIONAL-3.                              CI0074
                                                                        ADU165
      *>>>>>>> Linkage Area for Logger Program DBI110                   ADU165
      *!WF DSP=DH DSL=DH SEL=10 FOR=I DES=2 LEV=1                       ADU165
       01                 DH10.                                         CI0074
            10            DH10-GERTC  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            DH10-XUIBP  PICTURE  S9(8)                    CI0074
                          VALUE                ZERO                     CI0074
                          BINARY.                                       CI0074
            10            DH10-NSEQ2P PICTURE  S9(3)                    CI0074
                          VALUE                ZERO                     CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            DH10-CAUL   PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            DH10-MAUSB  PICTURE  X(8)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            DH10-NAUSK  PICTURE  X(50)                    CI0074
                          VALUE                SPACE.                   CI0074
            10            DH10-CSYS   PICTURE  X(4)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            DH10-CAPPL  PICTURE  X(8)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            DH10-CAUSR  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            DH10-CAUFR  PICTURE  S9(5)                    CI0074
                          VALUE                ZERO                     CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            DH10-CAUAC  PICTURE  S9(5)                    CI0074
                          VALUE                ZERO                     CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            DH10-GEOPID PICTURE  X(6)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            DH10-CAUNIT PICTURE  X(4)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            DH10-GAUVR  PICTURE  X(400)                   CI0074
                          VALUE                SPACE.                   CI0074

      ******************************************************************
      ** **** VARIABLE NEEDED BY AUDIT LOG PROCESS                   ***
      ******************************************************************
         COPY ACFUAREA.

      ******************************************************************
      ** ****     AUDIT LOG VARIABLE AREA SEGMENTS                   ***
      ******************************************************************
      *!WF DSP=AL DSL=DK SEL=26 FOR=I DES=2 LEV=1 PLT=AL
       01                 AL26.                                         CI0074
            10            AL26-CTCCI  PICTURE  X                        CI0074
                          OCCURS       002     TIMES                    CI0074
                          VALUE                SPACE.                   CI0074
            10            AL26-CQACT  PICTURE  999                      CI0074
                          OCCURS       002     TIMES                    CI0074
                          VALUE                ZERO.                    CI0074
            10            AL26-CIRAS  PICTURE  999                      CI0074
                          OCCURS       002     TIMES                    CI0074
                          VALUE                ZERO.                    CI0074
            10            AL26-CIRAT  PICTURE  999                      CI0074
                          OCCURS       002     TIMES                    CI0074
                          VALUE                ZERO.                    CI0074
            10            AL26-CLCUS  PICTURE  99                       CI0074
                          OCCURS       002     TIMES                    CI0074
                          VALUE                ZERO.                    CI0074
            10            AL26-DIRAC  PICTURE  9(4)                     CI0074
                          OCCURS       002     TIMES                    CI0074
                          VALUE                ZERO.                    CI0074

      ******************************************************************
      **        AUDIT LOG SAVE AREAS                                   *
      ******************************************************************
      *!WF DSP=AS DSL=CT SEL=01 FOR=I DES=1 LEV=1 PLT=AL
       01                 AS01.                                         CI0074
            10            AS01-CT01K.                                   CI0074
            11            AS01-C299.                                    CI0074
            12            AS01-CTID.                                    CI0074
            13            AS01-CTIDA  PICTURE  9(3).                    CI0074
            13            AS01-CTIDN.                                   CI0074
            14            AS01-CTIDNP PICTURE  X(13).                   CI0074
            14            AS01-CTIDND PICTURE  9(11).                   CI0074
            10            AS01-GECKD  PICTURE  9.                       CI0074
            10            AS01-GEMDA  PICTURE  9(8).                    CI0074
            10            AS01-NSEQ4B PICTURE  9(8)                     CI0074
                          BINARY.                                       CI0074
            10            AS01-GECUC  PICTURE  99.                      CI0074
            10            AS01-CTAUL  PICTURE  9(3).                    CI0074
            10            AS01-DIRAC  PICTURE  9(4).                    CI0074
            10            AS01-CTCCI  PICTURE  X.                       CI0074
            10            AS01-CTCUS  PICTURE  999.                     CI0074
            10            AS01-CTEFD  PICTURE  9(8).                    CI0074
            10            AS01-CTIAD  PICTURE  9(8).                    CI0074
            10            AS01-CLCUS  PICTURE  99.                      CI0074
            10            AS01-CAMMB  PICTURE  X(3).                    CI0074
            10            AS01-CKPMM  PICTURE  X.                       CI0074
            10            AS01-CTLAD  PICTURE  9(8).                    CI0074
            10            AS01-IPERS  PICTURE  X.                       CI0074
            10            AS01-AUNCB  PICTURE  S9(7)V99                 CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            AS01-CTLAT  PICTURE  9(8).                    CI0074
            10            AS01-CTLATC PICTURE  9(6).                    CI0074
            10            AS01-IMEGA  PICTURE  X.                       CI0074
            10            AS01-DIRAB  PICTURE  9(8).                    CI0074
            10            AS01-COLRQ  PICTURE  X.                       CI0074
            10            AS01-ZDA04  PICTURE  X(4).                    CI0074
            10            AS01-CTLPD  PICTURE  9(8).                    CI0074
            10            AS01-CIRASP PICTURE  9.                       CI0074
            10            AS01-CIRATP PICTURE  99.                      CI0074
            10            AS01-DRTHC  PICTURE  9(8).                    CI0074
            10            AS01-CPPTC  PICTURE  X.                       CI0074
            10            AS01-ZDA06  PICTURE  X(6).                    CI0074
            10            AS01-CTACD  PICTURE  9(8).                    CI0074
            10            AS01-CTNLI  PICTURE  X.                       CI0074
            10            AS01-CTRHO  PICTURE  9(8).                    CI0074
            10            AS01-CTSGD  PICTURE  9(8).                    CI0074
            10            AS01-CPATP  PICTURE  X(1).                    CI0074
            10            AS01-IRSTA  PICTURE  X.                       CI0074
            10            AS01-CTSTA  PICTURE  99.                      CI0074
            10            AS01-CTSSC  PICTURE  99.                      CI0074
            10            AS01-PRLIN  PICTURE  9(3).                    CI0074
            10            AS01-PRCOD  PICTURE  9(5).                    CI0074
            10            AS01-PRSCD  PICTURE  X(9).                    CI0074
            10            AS01-CTLNI  PICTURE  X.                       CI0074
            10            AS01-AYSIDA PICTURE  9(3).                    CI0074
            10            AS01-AYSID  PICTURE  9(5).                    CI0074
            10            AS01-CTBMC  PICTURE  99.                      CI0074
            10            AS01-CINAR  PICTURE  99.                      CI0074
            10            AS01-CPHTR  PICTURE  X.                       CI0074
            10            AS01-CDSTR  PICTURE  XX.                      CI0074
            10            AS01-CQACT  PICTURE  999.                     CI0074
            10            AS01-CIRAS  PICTURE  999.                     CI0074
            10            AS01-CIRAT  PICTURE  999.                     CI0074
            10            AS01-CLRAY  PICTURE  9(5).                    CI0074
            10            AS01-CATTP  PICTURE  X.                       CI0074

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
       01                 CL00.                                         CI0074
            02            CL01.                                         CI0074
            10            CL01-CL01K.                                   CI0074
            11            CL01-C199.                                    CI0074
            12            CL01-CLID.                                    CI0074
            13            CL01-CLIDO  PICTURE  9(3).                    CI0074
            13            CL01-CLIDN.                                   CI0074
            14            CL01-CLIDNP PICTURE  X(12).                   CI0074
            14            CL01-CLIDND PICTURE  9(8).                    CI0074
            10            CL01-GECKD  PICTURE  9.                       CI0074
            10            CL01-GEMDA  PICTURE  9(8).                    CI0074
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0074
                          BINARY.                                       CI0074
            10            CL01-GECUC  PICTURE  99.                      CI0074
            10            CL01-CLDOR  PICTURE  9(8).                    CI0074
            10            CL01-CLLNG  PICTURE  XX.                      CI0074
            10            CL01-GESLC  PICTURE  99.                      CI0074
            10            CL01-CLTYP  PICTURE  X.                       CI0074
            10            CL01-CLCLS  PICTURE  9(3).                    CI0074
            10            CL01-CLTWRC PICTURE  99.                      CI0074
            10            CL01-CLPVC  PICTURE  99.                      CI0074
            10            CL01-CLIND  PICTURE  9(3).                    CI0074
            10            CL01-CLTRC  PICTURE  99.                      CI0074
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            CL01-AYSIDA PICTURE  9(3).                    CI0074
            10            CL01-AYSID  PICTURE  9(5).                    CI0074
            10            CL01-CLSTR  PICTURE  9(2).                    CI0074
            10            CL01-CLC11  PICTURE  X.                       CI0074
            10            CL01-CLTIN  PICTURE  9(12).                   CI0074
            10            CL01-CLTND  PICTURE  9(8).                    CI0074
            10            CL01-CLTINC PICTURE  9.                       CI0074
            10            CL01-CCDWA  PICTURE  9.                       CI0074
            10            CL01-CICES  PICTURE  X.                       CI0074
            10            CL01-CLTRA  PICTURE  9(2).                    CI0074
            10            CL01-DIRSY  PICTURE  9(4)                     CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            CL01-CFEDS  PICTURE  X.                       CI0074
            10            CL01-FILLER PICTURE  X(06).                   CI0074
            02            CL03.                                         CI0074
            10            CL03-GEDLA  PICTURE  9(8).                    CI0074
            10            CL03-DDREP  PICTURE  9(8).                    CI0074
            10            CL03-DPRFR  PICTURE  9(8).                    CI0074
            10            CL03-IACCI  PICTURE  X.                       CI0074
            10            CL03-CLDOB  PICTURE  9(8).                    CI0074
            10            CL03-CLDOD  PICTURE  9(8).                    CI0074
            10            CL03-CLDTH  PICTURE  X.                       CI0074
            10            CL03-CCINI  PICTURE  X.                       CI0074
            10            CL03-FILLER PICTURE  X(1).                    CI0074
            10            CL03-CLAIN  PICTURE  S9(11)                   CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            CL03-CCAOD  PICTURE  999.                     CI0074
            10            CL03-CLMAR  PICTURE  X.                       CI0074
            10            CL03-C198.                                    CI0074
            11            CL03-CLNAM.                                   CI0074
            12            CL03-CLNAMH PICTURE  X(6).                    CI0074
            12            CL03-CLNAMF PICTURE  X(20).                   CI0074
            12            CL03-CLNAMM.                                  CI0074
            13            CL03-CLNAMI PICTURE  X.                       CI0074
            13            CL03-CLNAMR PICTURE  X(14).                   CI0074
            12            CL03-CLNAML PICTURE  X(25).                   CI0074
            12            CL03-CLNAMS PICTURE  X(4).                    CI0074
            10            CL03-FILLER PICTURE  X(10).                   CI0074
            10            CL03-MPRFS  PICTURE  X(4).                    CI0074
            10            CL03-CLOCC  PICTURE  9(3).                    CI0074
            10            CL03-CLRET  PICTURE  X.                       CI0074
            10            CL03-IOCOB  PICTURE  X.                       CI0074
            10            CL03-CLSEX  PICTURE  X.                       CI0074
            10            CL03-CLWIL  PICTURE  X.                       CI0074
            10            CL03-GECFC  PICTURE  99.                      CI0074
            10            CL03-GECFY  PICTURE  9(4).                    CI0074
            10            CL03-ICUSC  PICTURE  X.                       CI0074
            10            CL03-MCTYC  PICTURE  X(20).                   CI0074
            10            CL03-CLWIP  PICTURE  X.                       CI0074
            10            CL03-CLCTXF PICTURE  99.                      CI0074
            10            CL03-CLCUS  PICTURE  99.                      CI0074
            10            CL03-NPDLU  PICTURE  9(5).                    CI0074
            10            CL03-CLEMI  PICTURE  X.                       CI0074
            10            CL03-GEPHNH PICTURE  X(14).                   CI0074
            10            CL03-GEPHNB PICTURE  X(14).                   CI0074
            10            CL03-GEPHNX PICTURE  9(4).                    CI0074
            10            CL03-GEPHNA PICTURE  X(14).                   CI0074
            10            CL03-FILLER PICTURE  X(3).                    CI0074
            10            CL03-IAPRT  PICTURE  X.                       CI0074
            10            CL03-CEMSC  PICTURE  X.                       CI0074
            10            CL03-CSEPS  PICTURE  X.                       CI0074
            10            CL03-CRACE  PICTURE  X.                       CI0074
            10            CL03-CNIRA  PICTURE  X.                       CI0074
            10            CL03-FILLER PICTURE  X(11).                   CI0074
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0020           PIC X(8) VALUE 'CI0020P '.                  AM0020
       01                 CT00.                                         CI0074
            02            CT01.                                         CI0074
            10            CT01-CT01K.                                   CI0074
            11            CT01-C299.                                    CI0074
            12            CT01-CTID.                                    CI0074
            13            CT01-CTIDA  PICTURE  9(3).                    CI0074
            13            CT01-CTIDN.                                   CI0074
            14            CT01-CTIDNP PICTURE  X(13).                   CI0074
            14            CT01-CTIDND PICTURE  9(11).                   CI0074
            10            CT01-GECKD  PICTURE  9.                       CI0074
            10            CT01-GEMDA  PICTURE  9(8).                    CI0074
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0074
                          BINARY.                                       CI0074
            10            CT01-GECUC  PICTURE  99.                      CI0074
            10            CT01-CTAUL  PICTURE  9(3).                    CI0074
            10            CT01-DIRAC  PICTURE  9(4).                    CI0074
            10            CT01-CTCCI  PICTURE  X.                       CI0074
            10            CT01-CTCUS  PICTURE  999.                     CI0074
            10            CT01-CTEFD  PICTURE  9(8).                    CI0074
            10            CT01-CTIAD  PICTURE  9(8).                    CI0074
            10            CT01-CLCUS  PICTURE  99.                      CI0074
            10            CT01-CAMMB  PICTURE  X(3).                    CI0074
            10            CT01-CKPMM  PICTURE  X.                       CI0074
            10            CT01-CTLAD  PICTURE  9(8).                    CI0074
            10            CT01-IPERS  PICTURE  X.                       CI0074
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            CT01-CTLAT  PICTURE  9(8).                    CI0074
            10            CT01-CTLATC PICTURE  9(6).                    CI0074
            10            CT01-IMEGA  PICTURE  X.                       CI0074
            10            CT01-DIRAB  PICTURE  9(8).                    CI0074
            10            CT01-COLRQ  PICTURE  X.                       CI0074
            10            CT01-ZDA04  PICTURE  X(4).                    CI0074
            10            CT01-CTLPD  PICTURE  9(8).                    CI0074
            10            CT01-CIRASP PICTURE  9.                       CI0074
            10            CT01-CIRATP PICTURE  99.                      CI0074
            10            CT01-DRTHC  PICTURE  9(8).                    CI0074
            10            CT01-CPPTC  PICTURE  X.                       CI0074
            10            CT01-ZDA06  PICTURE  X(6).                    CI0074
            10            CT01-CTACD  PICTURE  9(8).                    CI0074
            10            CT01-CTNLI  PICTURE  X.                       CI0074
            10            CT01-CTRHO  PICTURE  9(8).                    CI0074
            10            CT01-CTSGD  PICTURE  9(8).                    CI0074
            10            CT01-CPATP  PICTURE  X(1).                    CI0074
            10            CT01-IRSTA  PICTURE  X.                       CI0074
            10            CT01-CTSTA  PICTURE  99.                      CI0074
            10            CT01-CTSSC  PICTURE  99.                      CI0074
            10            CT01-PRLIN  PICTURE  9(3).                    CI0074
            10            CT01-PRCOD  PICTURE  9(5).                    CI0074
            10            CT01-PRSCD  PICTURE  X(9).                    CI0074
            10            CT01-CTLNI  PICTURE  X.                       CI0074
            10            CT01-AYSIDA PICTURE  9(3).                    CI0074
            10            CT01-AYSID  PICTURE  9(5).                    CI0074
            10            CT01-CTBMC  PICTURE  99.                      CI0074
            10            CT01-CINAR  PICTURE  99.                      CI0074
            10            CT01-CPHTR  PICTURE  X.                       CI0074
            10            CT01-CDSTR  PICTURE  XX.                      CI0074
            10            CT01-CQACT  PICTURE  999.                     CI0074
            10            CT01-CIRAS  PICTURE  999.                     CI0074
            10            CT01-CIRAT  PICTURE  999.                     CI0074
            10            CT01-CLRAY  PICTURE  9(5).                    CI0074
            10            CT01-CATTP  PICTURE  X.                       CI0074
            02            CT10.                                         CI0074
            10            CT10-CT10K.                                   CI0074
            11            CT10-GR98.                                    CI0074
            12            CT10-GRID.                                    CI0074
            13            CT10-GRIDC  PICTURE  9(3).                    CI0074
            13            CT10-GRIDN.                                   CI0074
            14            CT10-GRIDNP PICTURE  99.                      CI0074
            14            CT10-GRIDND PICTURE  9(8).                    CI0074
            10            CT10-GR97                                     CI0074
                          REDEFINES            CT10-CT10K.              CI0074
            11            CT10-GRIDCB PICTURE  9(3).                    CI0074
            11            CT10-FILLER PICTURE  X(10).                   CI0074
            10            CT10-GERSD  PICTURE  9(8).                    CI0074
            10            CT10-GERED  PICTURE  9(8).                    CI0074
            10            CT10-GRCSI  PICTURE  X.                       CI0074

      ******************************************************************
      *STANDARD DATE WORK AREA - USED IN MACRO AADA58
      ******************************************************************
      *!WF DSP=DD DSL=DD SEL=01 FOR=I LEV=1 PLT=DD
       01                 DD00.                                         CI0074
          05              DD00-SUITE.                                   CI0074
            15       FILLER         PICTURE  X(00093).                  CI0074
       01                 DD01  REDEFINES      DD00.                    CI0074
            10            DD01-XDAT8.                                   CI0074
            11            DD01-XDATC  PICTURE  XX.                      CI0074
            11            DD01-XDATY  PICTURE  XX.                      CI0074
            11            DD01-XDATM  PICTURE  XX.                      CI0074
            11            DD01-XDATD  PICTURE  XX.                      CI0074
            10            DD01-XDAT8D                                   CI0074
                          REDEFINES            DD01-XDAT8               CI0074
               PICTURE    9(8).                                         CI0074
            10            DD01-XDAT81.                                  CI0074
            11            DD01-XDATM1 PICTURE  XX.                      CI0074
            11            DD01-XDATD1 PICTURE  XX.                      CI0074
            11            DD01-XDATC1 PICTURE  XX.                      CI0074
            11            DD01-XDATY1 PICTURE  XX.                      CI0074
            10            DD01-XDAT80                                   CI0074
                          REDEFINES            DD01-XDAT81              CI0074
               PICTURE    9(8).                                         CI0074
            10            DD01-XDAT62.                                  CI0074
            11            DD01-XDATM2 PICTURE  XX.                      CI0074
            11            DD01-XDATD2 PICTURE  XX.                      CI0074
            11            DD01-XDATY2 PICTURE  XX.                      CI0074
            10            DD01-XDAT69                                   CI0074
                          REDEFINES            DD01-XDAT62              CI0074
               PICTURE    9(6).                                         CI0074
            10            DD01-XDATCU.                                  CI0074
            11            DD01-XDATC9 PICTURE  99.                      CI0074
            11            DD01-XDAYMD.                                  CI0074
            12            DD01-XDATY9 PICTURE  99.                      CI0074
            12            DD01-XDAMD.                                   CI0074
            13            DD01-XDATM9 PICTURE  99.                      CI0074
            13            DD01-XDATD9 PICTURE  99.                      CI0074
            10            DD01-XDAT89 PICTURE  9(8).                    CI0074
            10            DD01-XDAJC  PICTURE  9(7).                    CI0074
            10            DD01-XDAJC1.                                  CI0074
            11            DD01-XDAJC9 PICTURE  99.                      CI0074
            11            DD01-XDAJY  PICTURE  99.                      CI0074
            11            DD01-XDAJN  PICTURE  999.                     CI0074
            10            DD01-XDAB   PICTURE  9(5).                    CI0074
            10            DD01-DD05.                                    CI0074
            11            DD01-XDACT  PICTURE  S9(3)                    CI0074
                          COMPUTATIONAL-3.                              CI0074
            11            DD01-XDACV  PICTURE  S9                       CI0074
                          COMPUTATIONAL-3.                              CI0074
            11            DD01-XDAGP  PICTURE  S9(9)                    CI0074
                          COMPUTATIONAL-3.                              CI0074
            11            DD01-XDAJP  PICTURE  S9(7)                    CI0074
                          COMPUTATIONAL-3.                              CI0074
            11            DD01-XDACV1 PICTURE  S9                       CI0074
                          COMPUTATIONAL-3.                              CI0074
            11            DD01-XDAGP1 PICTURE  S9(9)                    CI0074
                          COMPUTATIONAL-3.                              CI0074
            11            DD01-XDAJP1 PICTURE  S9(7)                    CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            DD01-XW03.                                    CI0074
            11            DD01-XDATG.                                   CI0074
            12            DD01-XDAT1.                                   CI0074
            13            DD01-XDAT19 PICTURE  99.                      CI0074
            12            DD01-XDAT2.                                   CI0074
            13            DD01-XDAT29 PICTURE  99.                      CI0074
            12            DD01-XDAT3.                                   CI0074
            13            DD01-XDAT39 PICTURE  99.                      CI0074
            12            DD01-XDAT4.                                   CI0074
            13            DD01-XDAT49 PICTURE  99.                      CI0074
            11            DD01-XLEAPY PICTURE  99.                      CI0074
            11            DD01-DTGCY  PICTURE  9(4).                    CI0074
            11            DD01-FILLER                                   CI0074
                          REDEFINES            DD01-DTGCY.              CI0074
            12            DD01-DTGCC  PICTURE  9(2).                    CI0074
            12            DD01-DTGYY  PICTURE  9(2).                    CI0074

       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA58

      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0074
            10            XW05-XW06.                                    CI0074
            11            XW05-XDBPCB.                                  CI0074
            12            XW05-XDBDNM PICTURE  X(08)                    CI0074
                          VALUE                SPACE.                   CI0074
            12            XW05-XSEGLV PICTURE  X(02)                    CI0074
                          VALUE                SPACE.                   CI0074
            12            XW05-XRC    PICTURE  X(02)                    CI0074
                          VALUE                SPACE.                   CI0074
            12            XW05-XPROPT PICTURE  X(04)                    CI0074
                          VALUE                SPACE.                   CI0074
            12            XW05-FILLER PICTURE  S9(5)                    CI0074
                          VALUE                ZERO                     CI0074
                          BINARY.                                       CI0074
            12            XW05-XSEGNM PICTURE  X(08)                    CI0074
                          VALUE                SPACE.                   CI0074
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0074
                          VALUE                ZERO                     CI0074
                          BINARY.                                       CI0074
            12            XW05-XSEGNB PICTURE  9(05)                    CI0074
                          VALUE                ZERO                     CI0074
                          BINARY.                                       CI0074
            12            XW05-XCOKEY PICTURE  X(70)                    CI0074
                          VALUE                SPACE.                   CI0074
            10            XW05-XW07.                                    CI0074
            11            XW05-XIOPCB.                                  CI0074
            12            XW05-XTERMI PICTURE  X(08)                    CI0074
                          VALUE                SPACE.                   CI0074
            12            XW05-FILLER PICTURE  XX                       CI0074
                          VALUE                SPACE.                   CI0074
            12            XW05-XRC1   PICTURE  X(02)                    CI0074
                          VALUE                SPACE.                   CI0074
            12            XW05-FILLER PICTURE  X(12)                    CI0074
                          VALUE                SPACE.                   CI0074
            12            XW05-XMODNM PICTURE  X(8)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0074
                          VALUE                ZERO.                    CI0074
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0074
                          VALUE                ZERO.                    CI0074
            10            XW05-XGU    PICTURE  X(4)                     CI0074
                          VALUE                'GU  '.                  CI0074
            10            XW05-XGHU   PICTURE  X(4)                     CI0074
                          VALUE                'GHU '.                  CI0074
            10            XW05-XGN    PICTURE  X(4)                     CI0074
                          VALUE                'GN  '.                  CI0074
            10            XW05-XGHN   PICTURE  X(4)                     CI0074
                          VALUE                'GHN '.                  CI0074
            10            XW05-XGNP   PICTURE  X(4)                     CI0074
                          VALUE                'GNP '.                  CI0074
            10            XW05-XGHNP  PICTURE  X(4)                     CI0074
                          VALUE                'GHNP'.                  CI0074
            10            XW05-XREPL  PICTURE  XXXX                     CI0074
                          VALUE                'REPL'.                  CI0074
            10            XW05-XISRT  PICTURE  X(4)                     CI0074
                          VALUE                'ISRT'.                  CI0074
            10            XW05-XDLET  PICTURE  X(4)                     CI0074
                          VALUE                'DLET'.                  CI0074
            10            XW05-XOPEN  PICTURE  X(4)                     CI0074
                          VALUE                'OPEN'.                  CI0074
            10            XW05-XCLSE  PICTURE  X(4)                     CI0074
                          VALUE                'CLSE'.                  CI0074
            10            XW05-XCHKP  PICTURE  X(4)                     CI0074
                          VALUE                'CHKP'.                  CI0074
            10            XW05-XXRST  PICTURE  X(4)                     CI0074
                          VALUE                'XRST'.                  CI0074
            10            XW05-XTERM  PICTURE  X(4)                     CI0074
                          VALUE                'TERM'.                  CI0074
            10            XW05-XNFPAC PICTURE  X(13)                    CI0074
                          VALUE                SPACE.                   CI0074
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0074
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0074
       01                 GQ00.                                         CI0074
            02            GQ01.                                         CI0074
            10            GQ01-GELL   PICTURE  9(4)                     CI0074
                          BINARY.                                       CI0074
            10            GQ01-GMISC.                                   CI0074
            11            GQ01-GS00.                                    CI0074
            12            GQ01-GT01.                                    CI0074
            13            GQ01-GQ01K.                                   CI0074
            14            GQ01-CANUMB PICTURE  X(27).                   CI0074
            14            GQ01-CAMCTR PICTURE  9(5).                    CI0074
            14            GQ01-GESQ2  PICTURE  99.                      CI0074
            12            GQ01-GT02                                     CI0074
                          REDEFINES            GQ01-GT01.               CI0074
            13            GQ01-C199.                                    CI0074
            14            GQ01-CLID.                                    CI0074
            15            GQ01-CLIDO  PICTURE  9(3).                    CI0074
            15            GQ01-CLIDN.                                   CI0074
            16            GQ01-CLIDNP PICTURE  X(12).                   CI0074
            16            GQ01-CLIDND PICTURE  9(8).                    CI0074
            12            GQ01-GT03                                     CI0074
                          REDEFINES            GQ01-GT01.               CI0074
            13            GQ01-C299.                                    CI0074
            14            GQ01-CTID.                                    CI0074
            15            GQ01-CTIDA  PICTURE  9(3).                    CI0074
            15            GQ01-CTIDN.                                   CI0074
            16            GQ01-CTIDNP PICTURE  X(13).                   CI0074
            16            GQ01-CTIDND PICTURE  9(11).                   CI0074
            12            GQ01-GT04                                     CI0074
                          REDEFINES            GQ01-GT01.               CI0074
            13            GQ01-NPBN   PICTURE  X(20).                   CI0074
            12            GQ01-GT05                                     CI0074
                          REDEFINES            GQ01-GT01.               CI0074
            13            GQ01-GR98.                                    CI0074
            14            GQ01-GRID.                                    CI0074
            15            GQ01-GRIDC  PICTURE  9(3).                    CI0074
            15            GQ01-GRIDN.                                   CI0074
            16            GQ01-GRIDNP PICTURE  99.                      CI0074
            16            GQ01-GRIDND PICTURE  9(8).                    CI0074
            12            GQ01-GT06                                     CI0074
                          REDEFINES            GQ01-GT01.               CI0074
            13            GQ01-NTR    PICTURE  9(8).                    CI0074
            12            GQ01-GT07                                     CI0074
                          REDEFINES            GQ01-GT01.               CI0074
            13            GQ01-NTRAC  PICTURE  9(14).                   CI0074
            12            GQ01-GT08                                     CI0074
                          REDEFINES            GQ01-GT01.               CI0074
            13            GQ01-NSRAB  PICTURE  9(7).                    CI0074
            13            GQ01-GECKD  PICTURE  9.                       CI0074
            13            GQ01-NBLCK  PICTURE  9(5).                    CI0074
            13            GQ01-CTRID  PICTURE  X(4).                    CI0074
            12            GQ01-GT19                                     CI0074
                          REDEFINES            GQ01-GT01.               CI0074
            13            GQ01-GEOPD2 PICTURE  X(8).                    CI0074
            12            GQ01-CACKD  PICTURE  9.                       CI0074
            12            GQ01-CENTT  PICTURE  X.                       CI0074
            12            GQ01-CADATE PICTURE  X(8).                    CI0074
            12            GQ01-GETIM  PICTURE  S9(7)                    CI0074
                          COMPUTATIONAL-3.                              CI0074
            12            GQ01-GEOPID PICTURE  X(6).                    CI0074
            12            GQ01-CAUNIT PICTURE  X(4).                    CI0074
            12            GQ01-XTERMI PICTURE  X(08).                   CI0074
            12            GQ01-CAPPL  PICTURE  X(8).                    CI0074
            12            GQ01-CSYS   PICTURE  X(4).                    CI0074
            12            GQ01-NTRSU  PICTURE  999.                     CI0074
            12            GQ01-FILLER PICTURE  X(20).                   CI0074
            11            GQ01-XMISL  PICTURE  X(599).                  CI0074
       01                 GR00.                                         CI0074
            02            GR01.                                         CI0074
            10            GR01-GR01K.                                   CI0074
            11            GR01-GR98.                                    CI0074
            12            GR01-GRID.                                    CI0074
            13            GR01-GRIDC  PICTURE  9(3).                    CI0074
            13            GR01-GRIDN.                                   CI0074
            14            GR01-GRIDNP PICTURE  99.                      CI0074
            14            GR01-GRIDND PICTURE  9(8).                    CI0074
            10            GR01-GECKD  PICTURE  9.                       CI0074
            10            GR01-GEMDA  PICTURE  9(8).                    CI0074
            10            GR01-NSEQ4B PICTURE  9(8)                     CI0074
                          BINARY.                                       CI0074
            10            GR01-GRDOR  PICTURE  9(8).                    CI0074
            10            GR01-GRIAD  PICTURE  9(8).                    CI0074
            10            GR01-GECUC  PICTURE  99.                      CI0074
            10            GR01-GRLNG  PICTURE  99.                      CI0074
            10            GR01-GESLC  PICTURE  99.                      CI0074
            10            GR01-AYSIDA PICTURE  9(3).                    CI0074
            10            GR01-AYSID  PICTURE  9(5).                    CI0074
            10            GR01-GRCSD  PICTURE  9(8).                    CI0074
            10            GR01-GRCFD  PICTURE  9(8).                    CI0074
            10            GR01-GRNCL  PICTURE  S9(5)                    CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            GR01-GRNCT  PICTURE  S9(5)                    CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            GR01-GRSFC  PICTURE  99.                      CI0074
            10            GR01-GRCRN  PICTURE  9(3).                    CI0074
            10            GR01-GRCSS  PICTURE  X.                       CI0074
            10            GR01-MKSRC  PICTURE  99                       CI0074
                          OCCURS       010     TIMES.                   CI0074
            10            GR01-NEFPS  PICTURE  X(5).                    CI0074
            10            GR01-DEFPS  PICTURE  9(8).                    CI0074
            10            GR01-DLSRV  PICTURE  9(8).                    CI0074
            10            GR01-CTLNI  PICTURE  X.                       CI0074
            10            GR01-CGRLI  PICTURE  X.                       CI0074
            10            GR01-CAMGR  PICTURE  9(5)                     CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            GR01-CAMGS  PICTURE  9(5)                     CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            GR01-CAMGN  PICTURE  9(3)                     CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            GR01-CGRMF  PICTURE  X.                       CI0074
            10            GR01-FILLER PICTURE  X(08).                   CI0074
            02            GR07.                                         CI0074
            10            GR07-GEDLA  PICTURE  9(8).                    CI0074
            10            GR07-GRAID  PICTURE  X(12).                   CI0074
            10            GR07-GRPAP  PICTURE  X(14).                   CI0074
            10            GR07-GEPHNX PICTURE  9(4).                    CI0074
            10            GR07-DPLEF  PICTURE  9(8).                    CI0074
            10            GR07-DPLAM  PICTURE  9(8).                    CI0074
            10            GR07-NCPFN  PICTURE  9(6).                    CI0074
            10            GR07-GEFYE  PICTURE  9(4).                    CI0074
            10            GR07-FILLER PICTURE  X(06).                   CI0074
            10            GR07-GRPAN  PICTURE  X(45).                   CI0074
            10            GR07-CGRPA  PICTURE  99.                      CI0074
            10            GR07-IPRTT7 PICTURE  X.                       CI0074
            10            GR07-GRPED  PICTURE  9(8).                    CI0074
            10            GR07-FILLER PICTURE  X(05).                   CI0074
            10            GR07-GRPLC  PICTURE  99.                      CI0074
            10            GR07-GRPLT  PICTURE  99.                      CI0074
            10            GR07-FILLER PICTURE  X(04).                   CI0074
            10            GR07-GEADI  PICTURE  X.                       CI0074
            10            GR07-GRCFA  PICTURE  S9(11)V99                CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            GR07-GECFY  PICTURE  9(4).                    CI0074
            10            GR07-GECFC  PICTURE  99.                      CI0074
            10            GR07-MEMPL  PICTURE  X(20).                   CI0074
            10            GR07-CAUNIT PICTURE  X(4).                    CI0074
            10            GR07-FILLER PICTURE  X(21).                   CI0074
            10            GR07-GRPPP  PICTURE  999.                     CI0074
            10            GR07-CCORT  PICTURE  9(3).                    CI0074
            10            GR07-CIDRP  PICTURE  99.                      CI0074
            10            GR07-CCDWA  PICTURE  9.                       CI0074
            10            GR07-IERSA  PICTURE  X.                       CI0074
            10            GR07-DERSA  PICTURE  9(8).                    CI0074
            10            GR07-FILLER PICTURE  X(04).                   CI0074

      *COMMAND CODES FOR ACCESSING THE FIRST OR NEXT SEGMENT UNDER THE
      *CONTRACT ROOT RECORD.  THIS TECHNIQUE PROVIDES THE ABILITY
      *TO 'BACKUP' WITHIN THE CHILD SEGMENTS UNDER THE ROOT AFTER
      *ESTABLISHING PARENTAGE ON THE APPLICABLE PARENT SEGMENT OR ROOT.
       01  FILLER.
           05  7-CCOD-FIRST                PIC  X(05) VALUE 'F----'.
           05  7-CCOD-NEXT                 PIC  X(05) VALUE '-----'.

       01  SEGMENT-FLAGS.
      *    '0' = SEGMENT NOT FOUND
      *    '1' = SEGMENT FOUND
           05  CT10-CF                     PIC  X     VALUE ZERO.

      *
      ******************************************************************
      **   TEMPORARY HOLDING SEGMENT FOR SOURCE ACCOUNT CT10
      ******************************************************************
      *
      *!WF DSP=HO DSL=CT SEL=10 FOR=I DES=2 LEV=1 PLT=HO
       01                 HO10.                                         CI0074
            10            HO10-CT10K.                                   CI0074
            11            HO10-GR98.                                    CI0074
            12            HO10-GRID.                                    CI0074
            13            HO10-GRIDC  PICTURE  9(3)                     CI0074
                          VALUE                ZERO.                    CI0074
            13            HO10-GRIDN.                                   CI0074
            14            HO10-GRIDNP PICTURE  99                       CI0074
                          VALUE                ZERO.                    CI0074
            14            HO10-GRIDND PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            HO10-GR97                                     CI0074
                          REDEFINES            HO10-CT10K.              CI0074
            11            HO10-GRIDCB PICTURE  9(3).                    CI0074
            11            HO10-FILLER PICTURE  X(10).                   CI0074
            10            HO10-GERSD  PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            HO10-GERED  PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            HO10-GRCSI  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
      *
      *
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
       01                 NS00.                                         CI0074
          05              NS00-00.                                      CI0074
            10            NS00-NS00K.                                   CI0074
            11            NS00-PRCSTK PICTURE  XX.                      CI0074
          05              NS00-SUITE.                                   CI0074
            15       FILLER         PICTURE  X(00078).                  CI0074
       01                 NS20  REDEFINES      NS00.                    CI0074
            10       FILLER         PICTURE  X(00002).                  CI0074
            10            NS20-DCACG  PICTURE  9(8).                    CI0074
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            NS20-CCDAT  PICTURE  X(8).                    CI0074
            10            NS20-DCALP  PICTURE  X(12).                   CI0074
            10            NS20-DNACG  PICTURE  9(8).                    CI0074
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            NS20-CNDAT  PICTURE  X(8).                    CI0074
            10            NS20-DNALP  PICTURE  X(12).                   CI0074
            10            NS20-DCACD  PICTURE  X(10).                   CI0074
            10            NS20-FILLER PICTURE  X(4).                    CI0074
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
       01  7-SACV-CODES.                                                ASETCV
      *!WI pl=SA040                                                     ASETCV
           05  7-SACV-CLCUS            VALUE ZEROES                     ASETCV
                        PICTURE 99.                                     CI0074
      *!WI pl=SA060                                                     ASETCV
           05  7-SACV-CLCUSA           VALUE SPACES                     ASETCV
                        PICTURE XXX.                                    CI0074
           05  7-SACV-YEAR    PIC X(4) VALUE SPACES.                    ASETCV
       01  7-PASSED-FIELDS.                                             ASETCD
      *!WI pl=SC100                                                     ASETCD
         05  7-PASS-CLCUS                                               ASETCD
                        PICTURE 99                                      CI0074
                                        VALUE ZERO.                     ASETCD
                                                                        ASETCD
           88  VALID-SET-CODE           VALUES ARE 01 02 03 04 05 07    ASETCD
                                                08 10 11 12 14 15 16    ASETCD
                                                17 18 19 21 22 23 24    ASETCD
                                                25 26 27 28 32 33 34    ASETCD
                                                35 36 37 38 39 43 44    ASETCD
                                                45 46 47 48 50 88       ASETCD
                                                99 00 09 13 51 52 53    ASETCD
                                                54 55 56 57 58 59 60    ASETCD
                                                61.                     ASETCD
           88  COPS-VALID-SET-CODE      VALUES ARE 01 02 03 04 05 07    ASETCD
                                                08 10 11 12 14 15 16    ASETCD
                                                17 18 19 21 22 23 24    ASETCD
                                                25 26 27 28 32 33 34    ASETCD
                                                35 36 37 38 39 43 44    ASETCD
                                                45 46 47 48 50 88       ASETCD
                                                99 00 09 13 51 52 53    ASETCD
                                                54 55 56 57 58 59 60    ASETCD
                                                61.                     ASETCD
      *!WI pl=SC160                                                     ASETCD
         05  7-PASS-CLCUSA                                              ASETCD
                        PICTURE XXX                                     CI0074
                                        VALUE SPACES.                   ASETCD
      *!WI pl=SC200                                                     ASETCD
         05  7-PASS-CLDTH                                               ASETCD
                        PICTURE X                                       CI0074
                                        VALUE SPACES.                   ASETCD
      *!WI pl=SC220                                                     ASETCD
         05  7-PASS-CTIDA                                               ASETCD
                        PICTURE 9(3)                                    CI0074
                                        VALUE ZERO.                     ASETCD
      *!WI pl=SC300                                                     ASETCD
         05  7-PASS-CLDOD                                               ASETCD
                        PICTURE 9(8)                                    CI0074
                                        VALUE ZERO.                     ASETCD
      *!WI pl=SC400                                                     ASETCD
         05  7-PASS-QCAGE1                                              ASETCD
                        PICTURE 999V9                                   CI0074
                                        VALUE ZERO.                     ASETCD
      *!WI pl=SC500                                                     ASETCD
         05  7-PASS-DEFFT                                               ASETCD
                        PICTURE 9(8)                                    CI0074
                                        VALUE ZERO.                     ASETCD
      *!WI pl=SC512                                                     ASETCD
         05  7-ROTH-DRTHC               VALUE ZERO                      ASETCD
                        PICTURE 9(8).                                   CI0074
         05  FILLER REDEFINES 7-ROTH-DRTHC.                             ASETCD
             10  7-ROTH-YEAR    PIC X(04).                              ASETCD
             10  FILLER         PIC X(04).                              ASETCD
         05  7-PASS-EDTYP       PIC XX                                  ASETCD
                                        VALUE SPACES.                   ASETCD
         05  7-PASS-FWIND       PIC X                                   ASETCD
                                        VALUE SPACES.                   ASETCD
         05  7-PASS-SWIND       PIC X                                   ASETCD
                                        VALUE SPACES.                   ASETCD
      *!WI pl=SC640                                                     ASETCD
         05  7-PASS-CIRAS                                               ASETCD
                        PICTURE 999                                     CI0074
                                        VALUE ZERO.                     ASETCD
      *!WI pl=SC650                                                     ASETCD
         05  7-PASS-CIRAT                                               ASETCD
                        PICTURE 999                                     CI0074
                                        VALUE ZERO.                     ASETCD
      *      *****   RETURN-FIELDS    **                                ASETCD
      *!WI pl=SC710                                                     ASETCD
         05  7-RETN-CLCUS                                               ASETCD
                        PICTURE 99                                      CI0074
                                        VALUE ZERO.                     ASETCD
      *!WI pl=SC730                                                     ASETCD
         05  7-RETN-CLCUSA                                              ASETCD
                        PICTURE XXX                                     CI0074
                                        VALUE SPACES.                   ASETCD
      *!WI pl=SC750                                                     ASETCD
         05  7-RETN-CSMSG                                               ASETCD
                        PICTURE 99                                      CI0074
                                        VALUE ZERO.                     ASETCD
         05  7-PASS-ACTYP     PIC X(05)                                 ASETCD
                                        VALUE SPACES.                   ASETCD
             88  VALID-ACCT-TYPE   VALUES ARE 'TSA1 ' 'TSA2 '           ASETCD
                                      'IRA  '   'TSCA ' 'KEOGH'.        ASETCD
      *                                                                 ASETCD
      * INITIALIZED FIELDS FOR 7-PASSED-FIELDS.                         ASETCD
       01  7-INIT-FIELDS.                                               ASETCD
      *!WI pl=SC902                                                     ASETCD
         05  7-INIT-CLCUS                                               ASETCD
                        PICTURE 99                                      CI0074
                                        VALUE ZERO.                     ASETCD
      *!WI pl=SC907                                                     ASETCD
         05  7-INIT-CLCUSA                                              ASETCD
                        PICTURE XXX                                     CI0074
                                        VALUE SPACES.                   ASETCD
      *!WI pl=SC909                                                     ASETCD
         05  7-INIT-CLDTH                                               ASETCD
                        PICTURE X                                       CI0074
                                        VALUE SPACES.                   ASETCD
      *!WI pl=SC930                                                     ASETCD
         05  7-INIT-CTIDA                                               ASETCD
                        PICTURE 9(3)                                    CI0074
                                        VALUE ZERO.                     ASETCD
      *!WI pl=SC932                                                     ASETCD
         05  7-INIT-CLDOD                                               ASETCD
                        PICTURE 9(8)                                    CI0074
                                        VALUE ZERO.                     ASETCD
      *!WI pl=SC936                                                     ASETCD
         05  7-INIT-QCAGE1                                              ASETCD
                        PICTURE 999V9                                   CI0074
                                        VALUE ZERO.                     ASETCD
      *!WI pl=SC941                                                     ASETCD
         05  7-INIT-DEFFT                                               ASETCD
                        PICTURE 9(8)                                    CI0074
                                        VALUE ZERO.                     ASETCD
         05  7-INIT-EDTYP       PIC XX                                  ASETCD
                                        VALUE SPACES.                   ASETCD
         05  7-INIT-FWIND       PIC X                                   ASETCD
                                        VALUE 'N'.                      ASETCD
         05  7-INIT-SWIND       PIC X                                   ASETCD
                                        VALUE 'N'.                      ASETCD
      *!WI pl=SC955                                                     ASETCD
         05  7-INIT-CIRAS                                               ASETCD
                        PICTURE 999                                     CI0074
                                        VALUE ZERO.                     ASETCD
      *!WI pl=SC960                                                     ASETCD
         05  7-INIT-CIRAT                                               ASETCD
                        PICTURE 999                                     CI0074
                                        VALUE ZERO.                     ASETCD
      *!WI pl=SC965                                                     ASETCD
         05  7-INRT-CLCUS                                               ASETCD
                        PICTURE 99                                      CI0074
                                        VALUE ZERO.                     ASETCD
      *!WI pl=SC970                                                     ASETCD
         05  7-INRT-CLCUSA                                              ASETCD
                        PICTURE XXX                                     CI0074
                                        VALUE SPACES.                   ASETCD
      *!WI pl=SC975                                                     ASETCD
         05  7-INRT-CSMSG                                               ASETCD
                        PICTURE 99                                      CI0074
                                        VALUE ZERO.                     ASETCD
      ******************************************************************ADUTAB
      **              TABLE TA75 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA75-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=75 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA75.                                                CI0074
           04    G-TA75-PARAM.                                          CI0074
             10  G-TA75-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0074
                        VALUE      +060.                                CI0074
             10  G-TA75-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0074
                        VALUE      +001.                                CI0074
             10  G-TA75-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0074
                        VALUE      +005.                                CI0074
             10  G-TA75-NUAPP  PICTURE 99                               CI0074
                        VALUE       0.                                  CI0074
             10  G-TA75-NUTAB  PICTURE X(6)                             CI0074
                        VALUE 'CAMCTR'.                                 CI0074
             10  G-TA75-TABFO  PICTURE XX                 VALUE SPACE.  CI0074
             10  G-TA75-TABCR  PICTURE XX                 VALUE SPACE.  CI0074
             10  G-TA75-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0074
             10  G-TA75-NUSSC  PICTURE X  VALUE   ' '.                  CI0074
             10  G-TA75-NUSSY  PICTURE X                  VALUE SPACE.  CI0074
             10  G-TA75-TRANID PICTURE X(4)               VALUE SPACE.  CI0074
             10  G-TA75-FILSYS.                                         CI0074
             15  G-TA75-USERC  PICTURE X(6)               VALUE SPACE.  CI0074
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0074
           04             TA75.                                         CI0074
            10            TA75-CAMCTR PICTURE  9(5)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TA75-TTDES  PICTURE  X(36)                    CI0074
                          VALUE                SPACE.                   CI0074
            10            TA75-MSYSID PICTURE  X(8)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            TA75-NDLEN  PICTURE  S9(4)                    CI0074
                          VALUE                ZERO.                    CI0074
            10            TA75-IMIND1 PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TA75-IMIND2 PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TA75-IMIND3 PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TA75-IMIND5 PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TA75-IMIND7 PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TA75-IMIND8 PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TA75-IMINE  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
      **                                                                ADUTAB
      *
      ******************************************************************
      **     SEGMENT THAT CONTAINS THE DESTINATION ACCOUNT CT01        *
      ******************************************************************
      *
      *!WF DSP=TC DSL=CT SEL=01 FOR=I DES=2 LEV=1 PLT=TC
       01                 TC01.                                         CI0074
            10            TC01-CT01K.                                   CI0074
            11            TC01-C299.                                    CI0074
            12            TC01-CTID.                                    CI0074
            13            TC01-CTIDA  PICTURE  9(3)                     CI0074
                          VALUE                ZERO.                    CI0074
            13            TC01-CTIDN.                                   CI0074
            14            TC01-CTIDNP PICTURE  X(13)                    CI0074
                          VALUE                SPACE.                   CI0074
            14            TC01-CTIDND PICTURE  9(11)                    CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-GECKD  PICTURE  9                        CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-GEMDA  PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-NSEQ4B PICTURE  9(8)                     CI0074
                          VALUE                ZERO                     CI0074
                          BINARY.                                       CI0074
            10            TC01-GECUC  PICTURE  99                       CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CTAUL  PICTURE  9(3)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-DIRAC  PICTURE  9(4)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CTCCI  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-CTCUS  PICTURE  999                      CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CTEFD  PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CTIAD  PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CLCUS  PICTURE  99                       CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CAMMB  PICTURE  X(3)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-CKPMM  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-CTLAD  PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-IPERS  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-AUNCB  PICTURE  S9(7)V99                 CI0074
                          VALUE                ZERO                     CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            TC01-CTLAT  PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CTLATC PICTURE  9(6)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-IMEGA  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-DIRAB  PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-COLRQ  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-ZDA04  PICTURE  X(4)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-CTLPD  PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CIRASP PICTURE  9                        CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CIRATP PICTURE  99                       CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-DRTHC  PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CPPTC  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-ZDA06  PICTURE  X(6)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-CTACD  PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CTNLI  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-CTRHO  PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CTSGD  PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CPATP  PICTURE  X(1)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-IRSTA  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-CTSTA  PICTURE  99                       CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CTSSC  PICTURE  99                       CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-PRLIN  PICTURE  9(3)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-PRCOD  PICTURE  9(5)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-PRSCD  PICTURE  X(9)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-CTLNI  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-AYSIDA PICTURE  9(3)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-AYSID  PICTURE  9(5)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CTBMC  PICTURE  99                       CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CINAR  PICTURE  99                       CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CPHTR  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-CDSTR  PICTURE  XX                       CI0074
                          VALUE                SPACE.                   CI0074
            10            TC01-CQACT  PICTURE  999                      CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CIRAS  PICTURE  999                      CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CIRAT  PICTURE  999                      CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CLRAY  PICTURE  9(5)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC01-CATTP  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
       01                 TC10.                                         CI0074
            10            TC10-CT10K.                                   CI0074
            11            TC10-GR98.                                    CI0074
            12            TC10-GRID.                                    CI0074
            13            TC10-GRIDC  PICTURE  9(3)                     CI0074
                          VALUE                ZERO.                    CI0074
            13            TC10-GRIDN.                                   CI0074
            14            TC10-GRIDNP PICTURE  99                       CI0074
                          VALUE                ZERO.                    CI0074
            14            TC10-GRIDND PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC10-GR97                                     CI0074
                          REDEFINES            TC10-CT10K.              CI0074
            11            TC10-GRIDCB PICTURE  9(3).                    CI0074
            11            TC10-FILLER PICTURE  X(10).                   CI0074
            10            TC10-GERSD  PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC10-GERED  PICTURE  9(8)                     CI0074
                          VALUE                ZERO.                    CI0074
            10            TC10-GRCSI  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
      *
      ******************************************************************
      **     SEGMENT THAT CONTAINS THE DESTINATION ACCOUNT CT10        *
      ******************************************************************
      *
      *!WF DSP=TC DSL=CT SEL=10 FOR=I DES=2 LEV=1 PLT=TC
      *
      *
      *
      *

      *****************************************************************
      ** THE FOLLOWING SEGMENTS GET MOVED TO THE VARIABLE PORTION OF  *
      ** THE MISC TRAN DB SEGMENT: GQ01-XMISL                         *
      *****************************************************************

      *GS49 - CUSTODIAL SETTLEMENT CODE UPDATE
      *!WF DSP=GS DSL=GS SEL=49 FOR=I DES=2 LEV=1 PLT=WA
       01                 GS00.                                         CI0074
            10            GS00-GT01.                                    CI0074
            11            GS00-GQ01K.                                   CI0074
            12            GS00-CANUMB PICTURE  X(27)                    CI0074
                          VALUE                SPACE.                   CI0074
            12            GS00-CAMCTR PICTURE  9(5)                     CI0074
                          VALUE                ZERO.                    CI0074
            12            GS00-GESQ2  PICTURE  99                       CI0074
                          VALUE                ZERO.                    CI0074
            10            GS00-GT02                                     CI0074
                          REDEFINES            GS00-GT01.               CI0074
            11            GS00-C199.                                    CI0074
            12            GS00-CLID.                                    CI0074
            13            GS00-CLIDO  PICTURE  9(3).                    CI0074
            13            GS00-CLIDN.                                   CI0074
            14            GS00-CLIDNP PICTURE  X(12).                   CI0074
            14            GS00-CLIDND PICTURE  9(8).                    CI0074
            10            GS00-GT03                                     CI0074
                          REDEFINES            GS00-GT01.               CI0074
            11            GS00-C299.                                    CI0074
            12            GS00-CTID.                                    CI0074
            13            GS00-CTIDA  PICTURE  9(3).                    CI0074
            13            GS00-CTIDN.                                   CI0074
            14            GS00-CTIDNP PICTURE  X(13).                   CI0074
            14            GS00-CTIDND PICTURE  9(11).                   CI0074
            10            GS00-GT04                                     CI0074
                          REDEFINES            GS00-GT01.               CI0074
            11            GS00-NPBN   PICTURE  X(20).                   CI0074
            10            GS00-GT05                                     CI0074
                          REDEFINES            GS00-GT01.               CI0074
            11            GS00-GR98.                                    CI0074
            12            GS00-GRID.                                    CI0074
            13            GS00-GRIDC  PICTURE  9(3).                    CI0074
            13            GS00-GRIDN.                                   CI0074
            14            GS00-GRIDNP PICTURE  99.                      CI0074
            14            GS00-GRIDND PICTURE  9(8).                    CI0074
            10            GS00-GT06                                     CI0074
                          REDEFINES            GS00-GT01.               CI0074
            11            GS00-NTR    PICTURE  9(8).                    CI0074
            10            GS00-GT07                                     CI0074
                          REDEFINES            GS00-GT01.               CI0074
            11            GS00-NTRAC  PICTURE  9(14).                   CI0074
            10            GS00-GT08                                     CI0074
                          REDEFINES            GS00-GT01.               CI0074
            11            GS00-NSRAB  PICTURE  9(7).                    CI0074
            11            GS00-GECKD  PICTURE  9.                       CI0074
            11            GS00-NBLCK  PICTURE  9(5).                    CI0074
            11            GS00-CTRID  PICTURE  X(4).                    CI0074
            10            GS00-GT19                                     CI0074
                          REDEFINES            GS00-GT01.               CI0074
            11            GS00-GEOPD2 PICTURE  X(8).                    CI0074
            10            GS00-CACKD  PICTURE  9                        CI0074
                          VALUE                ZERO.                    CI0074
            10            GS00-CENTT  PICTURE  X                        CI0074
                          VALUE                SPACE.                   CI0074
            10            GS00-CADATE PICTURE  X(8)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            GS00-GETIM  PICTURE  S9(7)                    CI0074
                          VALUE                ZERO                     CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            GS00-GEOPID PICTURE  X(6)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            GS00-CAUNIT PICTURE  X(4)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            GS00-XTERMI PICTURE  X(08)                    CI0074
                          VALUE                SPACE.                   CI0074
            10            GS00-CAPPL  PICTURE  X(8)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            GS00-CSYS   PICTURE  X(4)                     CI0074
                          VALUE                SPACE.                   CI0074
            10            GS00-NTRSU  PICTURE  999                      CI0074
                          VALUE                ZERO.                    CI0074
            10            GS00-FILLER PICTURE  X(20)                    CI0074
                          VALUE                SPACE.                   CI0074
       01                 GS49.                                         CI0074
            10            GS49-CLCUS  PICTURE  99                       CI0074
                          VALUE                ZERO.                    CI0074

      *DEFAULT TAX SETTLEMENT CODE VALUE DETERMINED BY THIS MODULE
      *!WI
       01  DEFAULT-CLCUS
                        PICTURE 99.                                     CI0074

      *NEED TO INTERROGATE THE ICODE TO DETERMINE IF THE ANNUITY ACCT
      *SHOULD BE PROCESSED WITH CUSTODIAL ACCOUNTS.
       01  WS01-PRSCD.
           05  FILLER              PIC X(5).
           05  WS01-ICODE          PIC 9(1).
           05  FILLER              PIC X(3).

       01   DEBUT-WSS.                                                  CI0074
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0074
            05   IK     PICTURE X.                                      CI0074
       01  CONSTANTES-PAC.                                              CI0074
           05  FILLER  PICTURE X(87)   VALUE                            CI0074
                     '6015 CAT09/08/14CI0074ADMIN   14:34:38CI0074P AMERCI0074
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0074
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0074
           05  NUGNA   PICTURE X(5).                                    CI0074
           05  APPLI   PICTURE X(3).                                    CI0074
           05  DATGN   PICTURE X(8).                                    CI0074
           05  PROGR   PICTURE X(6).                                    CI0074
           05  CODUTI  PICTURE X(8).                                    CI0074
           05  TIMGN   PICTURE X(8).                                    CI0074
           05  PROGE   PICTURE X(8).                                    CI0074
           05  COBASE  PICTURE X(4).                                    CI0074
           05  DATGNC  PICTURE X(10).                                   CI0074
           05  RELEAS  PICTURE X(7).                                    CI0074
           05  DATGE   PICTURE X(10).                                   CI0074
           05  DATSQ   PICTURE X(10).                                   CI0074
       01  DATCE.                                                       CI0074
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0074
         05  DATOR.                                                     CI0074
           10  DATOA  PICTURE XX.                                       CI0074
           10  DATOM  PICTURE XX.                                       CI0074
           10  DATOJ  PICTURE XX.                                       CI0074
       01   VARIABLES-CONDITIONNELLES.                                  CI0074
            05                  FT      PICTURE X VALUE '0'.            CI0074
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0074
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0074
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU070
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0074
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0074
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0074
            05       5-GQ00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0074
            05       5-GR00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0074
       01               S-CL01-SSA.                                     CI0074
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0074
                                      VALUE 'CL01    '.                 CI0074
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0074
            10          S-CL01-CCOD   PICTURE X(5)                      CI0074
                                      VALUE '-----'.                    CI0074
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0074
       01            S-CLU01-SSA.                                       CI0074
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0074
                                      VALUE 'CL01    '.                 CI0074
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0074
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0074
                                      VALUE '-----'.                    CI0074
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0074
                                      VALUE '(CL01K'.                   CI0074
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0074
            10       S-CLU01-CL01K.                                     CI0074
            11       S-CLU01-C199.                                      CI0074
            12       S-CLU01-CLID.                                      CI0074
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0074
            13       S-CLU01-CLIDN.                                     CI0074
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0074
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0074
            10  FILLER   PICTURE X    VALUE ')'.                        CI0074
       01               S-CL03-SSA.                                     CI0074
            10         S1-CL03-SEGNAM PICTURE X(8)                      CI0074
                                      VALUE 'CL03    '.                 CI0074
            10         S1-CL03-CCOM   PICTURE X VALUE '*'.              CI0074
            10          S-CL03-CCOD   PICTURE X(5)                      CI0074
                                      VALUE '-----'.                    CI0074
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0074
       01            S-CLA03-SSA.                                       CI0074
            10      S1-CLA03-SEGNAM PICTURE X(8)                        CI0074
                                      VALUE 'CL03    '.                 CI0074
            10      S1-CLA03-CCOM   PICTURE X VALUE '*'.                CI0074
            10       S-CLA03-CCOD   PICTURE X(5)                        CI0074
                                      VALUE '-----'.                    CI0074
            10      S1-CLA03-FLDNAM PICTURE X(9)                        CI0074
                                      VALUE '(CLDOD'.                   CI0074
            10       S-CLA03-OPER  PICTURE XX VALUE ' ='.               CI0074
            10       S-CLA03-CLDOD    PICTURE  9(8).                    CI0074
            10  FILLER   PICTURE X    VALUE ')'.                        CI0074
       01               S-CT01-SSA.                                     CI0074
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0074
                                      VALUE 'CT01    '.                 CI0074
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0074
            10          S-CT01-CCOD   PICTURE X(5)                      CI0074
                                      VALUE '-----'.                    CI0074
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0074
       01            S-CTU01-SSA.                                       CI0074
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0074
                                      VALUE 'CT01    '.                 CI0074
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0074
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0074
                                      VALUE '-----'.                    CI0074
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0074
                                      VALUE '(CT01K'.                   CI0074
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0074
            10       S-CTU01-CT01K.                                     CI0074
            11       S-CTU01-C299.                                      CI0074
            12       S-CTU01-CTID.                                      CI0074
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0074
            13       S-CTU01-CTIDN.                                     CI0074
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0074
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0074
            10  FILLER   PICTURE X    VALUE ')'.                        CI0074
       01               S-CT10-SSA.                                     CI0074
            10         S1-CT10-SEGNAM PICTURE X(8)                      CI0074
                                      VALUE 'CT10    '.                 CI0074
            10         S1-CT10-CCOM   PICTURE X VALUE '*'.              CI0074
            10          S-CT10-CCOD   PICTURE X(5)                      CI0074
                                      VALUE '-----'.                    CI0074
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0074
       01            S-CTA10-SSA.                                       CI0074
            10      S1-CTA10-SEGNAM PICTURE X(8)                        CI0074
                                      VALUE 'CT10    '.                 CI0074
            10      S1-CTA10-CCOM   PICTURE X VALUE '*'.                CI0074
            10       S-CTA10-CCOD   PICTURE X(5)                        CI0074
                                      VALUE '-----'.                    CI0074
            10      S1-CTA10-FLDNAM PICTURE X(9)                        CI0074
                                      VALUE '(GERED'.                   CI0074
            10       S-CTA10-OPER  PICTURE XX VALUE ' ='.               CI0074
            10       S-CTA10-GERED    PICTURE  9(8).                    CI0074
            10  FILLER   PICTURE X    VALUE ')'.                        CI0074
       01            S-CTB10-SSA.                                       CI0074
            11      S1-CTB10-SEGNAM PICTURE X(8)                        CI0074
                                      VALUE 'CT10    '.                 CI0074
            11      S1-CTB10-CCOM   PICTURE X VALUE '*'.                CI0074
            11       S-CTB10-CCOD   PICTURE X(5)                        CI0074
                                      VALUE '-----'.                    CI0074
            11      S1-CTB10-FLDNAM PICTURE X(9)                        CI0074
                                      VALUE '(GRIDCB'.                  CI0074
            11       S-CTB10-OPER  PICTURE XX VALUE ' ='.               CI0074
            11       S-CTB10-GRIDCB   PICTURE  9(3).                    CI0074
            11  FILLER   PICTURE X    VALUE ')'.                        CI0074
       01            S-CTC10-SSA.                                       CI0074
            10      S1-CTC10-SEGNAM PICTURE X(8)                        CI0074
                                      VALUE 'CT10    '.                 CI0074
            10      S1-CTC10-CCOM   PICTURE X VALUE '*'.                CI0074
            10       S-CTC10-CCOD   PICTURE X(5)                        CI0074
                                      VALUE '-----'.                    CI0074
            10      S1-CTC10-FLDNAM PICTURE X(9)                        CI0074
                                      VALUE '(GRCSI'.                   CI0074
            10       S-CTC10-OPER  PICTURE XX VALUE ' ='.               CI0074
            10       S-CTC10-GRCSI    PICTURE  X.                       CI0074
            10  FILLER   PICTURE X    VALUE ')'.                        CI0074
       01            S-CTU10-SSA.                                       CI0074
            10      S1-CTU10-SEGNAM PICTURE X(8)                        CI0074
                                      VALUE 'CT10    '.                 CI0074
            10      S1-CTU10-CCOM   PICTURE X VALUE '*'.                CI0074
            10       S-CTU10-CCOD   PICTURE X(5)                        CI0074
                                      VALUE '-----'.                    CI0074
            10      S1-CTU10-FLDNAM PICTURE X(9)                        CI0074
                                      VALUE '(CT10K'.                   CI0074
            10       S-CTU10-OPER  PICTURE XX VALUE ' ='.               CI0074
            10       S-CTU10-CT10K.                                     CI0074
            11       S-CTU10-GR98.                                      CI0074
            12       S-CTU10-GRID.                                      CI0074
            13       S-CTU10-GRIDC    PICTURE  9(3).                    CI0074
            13       S-CTU10-GRIDN.                                     CI0074
            14       S-CTU10-GRIDNP   PICTURE  99.                      CI0074
            14       S-CTU10-GRIDND   PICTURE  9(8).                    CI0074
            10  FILLER   PICTURE X    VALUE ')'.                        CI0074
       01               S-GQ01-SSA.                                     CI0074
            10         S1-GQ01-SEGNAM PICTURE X(8)                      CI0074
                                      VALUE 'GQ01    '.                 CI0074
            10         S1-GQ01-CCOM   PICTURE X VALUE '*'.              CI0074
            10          S-GQ01-CCOD   PICTURE X(5)                      CI0074
                                      VALUE '-----'.                    CI0074
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0074
       01            S-GQU01-SSA.                                       CI0074
            13      S1-GQU01-SEGNAM PICTURE X(8)                        CI0074
                                      VALUE 'GQ01    '.                 CI0074
            13      S1-GQU01-CCOM   PICTURE X VALUE '*'.                CI0074
            13       S-GQU01-CCOD   PICTURE X(5)                        CI0074
                                      VALUE '-----'.                    CI0074
            13      S1-GQU01-FLDNAM PICTURE X(9)                        CI0074
                                      VALUE '(GQ01K'.                   CI0074
            13       S-GQU01-OPER  PICTURE XX VALUE ' ='.               CI0074
            13       S-GQU01-GQ01K.                                     CI0074
            14       S-GQU01-CANUMB   PICTURE  X(27).                   CI0074
            14       S-GQU01-CAMCTR   PICTURE  9(5).                    CI0074
            14       S-GQU01-GESQ2    PICTURE  99.                      CI0074
            13  FILLER   PICTURE X    VALUE ')'.                        CI0074
       01            S-GQ701-SSA.                                       CI0074
            14      S1-GQ701-SEGNAM PICTURE X(8)                        CI0074
                                      VALUE 'GQ01    '.                 CI0074
            14      S1-GQ701-CCOM   PICTURE X VALUE '*'.                CI0074
            14       S-GQ701-CCOD   PICTURE X(5)                        CI0074
                                      VALUE '-----'.                    CI0074
            14      S1-GQ701-FLDNAM PICTURE X(9)                        CI0074
                                      VALUE '(XCANUMB'.                 CI0074
            14       S-GQ701-OPER  PICTURE XX VALUE ' ='.               CI0074
            14       S-GQ701-CANUMB   PICTURE  X(27).                   CI0074
            14  FILLER   PICTURE X    VALUE ')'.                        CI0074
       01               S-GR01-SSA.                                     CI0074
            10         S1-GR01-SEGNAM PICTURE X(8)                      CI0074
                                      VALUE 'GR01    '.                 CI0074
            10         S1-GR01-CCOM   PICTURE X VALUE '*'.              CI0074
            10          S-GR01-CCOD   PICTURE X(5)                      CI0074
                                      VALUE '-----'.                    CI0074
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0074
       01            S-GRU01-SSA.                                       CI0074
            10      S1-GRU01-SEGNAM PICTURE X(8)                        CI0074
                                      VALUE 'GR01    '.                 CI0074
            10      S1-GRU01-CCOM   PICTURE X VALUE '*'.                CI0074
            10       S-GRU01-CCOD   PICTURE X(5)                        CI0074
                                      VALUE '-----'.                    CI0074
            10      S1-GRU01-FLDNAM PICTURE X(9)                        CI0074
                                      VALUE '(GR01K'.                   CI0074
            10       S-GRU01-OPER  PICTURE XX VALUE ' ='.               CI0074
            10       S-GRU01-GR01K.                                     CI0074
            11       S-GRU01-GR98.                                      CI0074
            12       S-GRU01-GRID.                                      CI0074
            13       S-GRU01-GRIDC    PICTURE  9(3).                    CI0074
            13       S-GRU01-GRIDN.                                     CI0074
            14       S-GRU01-GRIDNP   PICTURE  99.                      CI0074
            14       S-GRU01-GRIDND   PICTURE  9(8).                    CI0074
            10  FILLER   PICTURE X    VALUE ')'.                        CI0074
       01               S-GR07-SSA.                                     CI0074
            10         S1-GR07-SEGNAM PICTURE X(8)                      CI0074
                                      VALUE 'GR07    '.                 CI0074
            10         S1-GR07-CCOM   PICTURE X VALUE '*'.              CI0074
            10          S-GR07-CCOD   PICTURE X(5)                      CI0074
                                      VALUE '-----'.                    CI0074
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0074
       01            S-GRA07-SSA.                                       CI0074
            10      S1-GRA07-SEGNAM PICTURE X(8)                        CI0074
                                      VALUE 'GR07    '.                 CI0074
            10      S1-GRA07-CCOM   PICTURE X VALUE '*'.                CI0074
            10       S-GRA07-CCOD   PICTURE X(5)                        CI0074
                                      VALUE '-----'.                    CI0074
            10      S1-GRA07-FLDNAM PICTURE X(9)                        CI0074
                                      VALUE '(NCPFN'.                   CI0074
            10       S-GRA07-OPER  PICTURE XX VALUE ' ='.               CI0074
            10       S-GRA07-NCPFN    PICTURE  9(6).                    CI0074
            10  FILLER   PICTURE X    VALUE ')'.                        CI0074
       01   ZONES-UTILISATEUR PICTURE X.                                CI0074
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
      ** PCB POINTER FOR GR1P                                           ADU015
            05 PCB-GR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR TR1P                                           ADU015
            05 PCB-TR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0074
          05              PA00-SUITE.                                   CI0074
            15       FILLER         PICTURE  X(00106).                  CI0074
       01                 PA06  REDEFINES      PA00.                    CI0074
            10            PA06-XDBPCB.                                  CI0074
            11            PA06-XDBDNM PICTURE  X(08).                   CI0074
            11            PA06-XSEGLV PICTURE  X(02).                   CI0074
            11            PA06-XRC    PICTURE  X(02).                   CI0074
            11            PA06-XPROPT PICTURE  X(04).                   CI0074
            11            PA06-FILLER PICTURE  S9(5)                    CI0074
                          BINARY.                                       CI0074
            11            PA06-XSEGNM PICTURE  X(08).                   CI0074
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0074
                          BINARY.                                       CI0074
            11            PA06-XSEGNB PICTURE  9(05)                    CI0074
                          BINARY.                                       CI0074
            11            PA06-XCOKEY PICTURE  X(70).                   CI0074
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0074
          05              PC00-SUITE.                                   CI0074
            15       FILLER         PICTURE  X(00106).                  CI0074
       01                 PC06  REDEFINES      PC00.                    CI0074
            10            PC06-XDBPCB.                                  CI0074
            11            PC06-XDBDNM PICTURE  X(08).                   CI0074
            11            PC06-XSEGLV PICTURE  X(02).                   CI0074
            11            PC06-XRC    PICTURE  X(02).                   CI0074
            11            PC06-XPROPT PICTURE  X(04).                   CI0074
            11            PC06-FILLER PICTURE  S9(5)                    CI0074
                          BINARY.                                       CI0074
            11            PC06-XSEGNM PICTURE  X(08).                   CI0074
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0074
                          BINARY.                                       CI0074
            11            PC06-XSEGNB PICTURE  9(05)                    CI0074
                          BINARY.                                       CI0074
            11            PC06-XCOKEY PICTURE  X(70).                   CI0074
      *** PCB MASK FOR GR1P                                             ADU015
      *!WF DSP=PD DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PD00.                                         CI0074
          05              PD00-SUITE.                                   CI0074
            15       FILLER         PICTURE  X(00106).                  CI0074
       01                 PD06  REDEFINES      PD00.                    CI0074
            10            PD06-XDBPCB.                                  CI0074
            11            PD06-XDBDNM PICTURE  X(08).                   CI0074
            11            PD06-XSEGLV PICTURE  X(02).                   CI0074
            11            PD06-XRC    PICTURE  X(02).                   CI0074
            11            PD06-XPROPT PICTURE  X(04).                   CI0074
            11            PD06-FILLER PICTURE  S9(5)                    CI0074
                          BINARY.                                       CI0074
            11            PD06-XSEGNM PICTURE  X(08).                   CI0074
            11            PD06-XKEYLN PICTURE  S9(05)                   CI0074
                          BINARY.                                       CI0074
            11            PD06-XSEGNB PICTURE  9(05)                    CI0074
                          BINARY.                                       CI0074
            11            PD06-XCOKEY PICTURE  X(70).                   CI0074
      *** PCB MASK FOR TR1P                                             ADU015
      *!WF DSP=PE DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PE00.                                         CI0074
          05              PE00-SUITE.                                   CI0074
            15       FILLER         PICTURE  X(00106).                  CI0074
       01                 PE06  REDEFINES      PE00.                    CI0074
            10            PE06-XDBPCB.                                  CI0074
            11            PE06-XDBDNM PICTURE  X(08).                   CI0074
            11            PE06-XSEGLV PICTURE  X(02).                   CI0074
            11            PE06-XRC    PICTURE  X(02).                   CI0074
            11            PE06-XPROPT PICTURE  X(04).                   CI0074
            11            PE06-FILLER PICTURE  S9(5)                    CI0074
                          BINARY.                                       CI0074
            11            PE06-XSEGNM PICTURE  X(08).                   CI0074
            11            PE06-XKEYLN PICTURE  S9(05)                   CI0074
                          BINARY.                                       CI0074
            11            PE06-XSEGNB PICTURE  9(05)                    CI0074
                          BINARY.                                       CI0074
            11            PE06-XCOKEY PICTURE  X(70).                   CI0074

      *PASS AREA TO/FROM CI0074
      *!WF DSP=PJ DSL=PJ SEL=15 FOR=I DES=1 LEV=1 PLT=60
       01                 PJ15.                                         CI0074
            10            PJ15-CT99.                                    CI0074
            11            PJ15-CT99K.                                   CI0074
            12            PJ15-C299.                                    CI0074
            13            PJ15-CTID.                                    CI0074
            14            PJ15-CTIDA  PICTURE  9(3).                    CI0074
            14            PJ15-CTIDN.                                   CI0074
            15            PJ15-CTIDNP PICTURE  X(13).                   CI0074
            15            PJ15-CTIDND PICTURE  9(11).                   CI0074
            10            PJ15-CLID   PICTURE  X(23).                   CI0074
            10            PJ15-CLCUS  PICTURE  99.                      CI0074
            10            PJ15-DCACG  PICTURE  9(8).                    CI0074
            10            PJ15-CAUNIT PICTURE  X(4).                    CI0074
            10            PJ15-GEOPID PICTURE  X(6).                    CI0074
            10            PJ15-CLCUSA PICTURE  XXX.                     CI0074
            10            PJ15-MAPPN  PICTURE  X(10).                   CI0074
            10            PJ15-CUPIQ  PICTURE  X.                       CI0074
            10            PJ15-CTID01 PICTURE  X(27).                   CI0074
            10            PJ15-CTYPE  PICTURE  X.                       CI0074
            10            PJ15-CPAYF  PICTURE  X(2).                    CI0074
            10            PJ15-FILLER PICTURE  X(84).                   CI0074

      *
      ******************************************************************
      **     THIS SEG CONTAINS INFO FROM A CALL TO CI0018  ACCT CLIENTS*
      **     FOR THE "FROM" ACCOUNT.                                   *
      ******************************************************************
      *
      *!WF DSP=FR DSL=DU SEL=14 FOR=I DES=1 LEV=1 PLT=70
       01                 FR14.                                         CI0074
            10            FR14-C299.                                    CI0074
            11            FR14-CTID.                                    CI0074
            12            FR14-CTIDA  PICTURE  9(3).                    CI0074
            12            FR14-CTIDN.                                   CI0074
            13            FR14-CTIDNP PICTURE  X(13).                   CI0074
            13            FR14-CTIDND PICTURE  9(11).                   CI0074
            10            FR14-DCACG  PICTURE  9(8).                    CI0074
            10            FR14-IPOCH  PICTURE  X.                       CI0074
            10            FR14-FILLER PICTURE  X(100).                  CI0074
            10            FR14-CLID01.                                  CI0074
            11            FR14-CLIDO1 PICTURE  X(3).                    CI0074
            11            FR14-NCLID1.                                  CI0074
            12            FR14-CLIDP1 PICTURE  X(12).                   CI0074
            12            FR14-CLIDNA PICTURE  9(8).                    CI0074
            10            FR14-CLCTR  PICTURE  9(3).                    CI0074
            10            FR14-DU21                                     CI0074
                          OCCURS       025     TIMES.                   CI0074
            11            FR14-C199.                                    CI0074
            12            FR14-CLID.                                    CI0074
            13            FR14-CLIDO  PICTURE  9(3).                    CI0074
            13            FR14-CLIDN.                                   CI0074
            14            FR14-CLIDNP PICTURE  X(12).                   CI0074
            14            FR14-CLIDND PICTURE  9(8).                    CI0074
            11            FR14-CLCTRC PICTURE  9(3).                    CI0074
            10            FR14-QITEM  PICTURE  9(3).                    CI0074
            10            FR14-XIMAX  PICTURE  S9(4)                    CI0074
                          BINARY.                                       CI0074
            10            FR14-CRROL  PICTURE  X.                       CI0074
            10            FR14-FILLER PICTURE  X(099).                  CI0074
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEG CONTAINS INFO FROM A CALL TO CI0018  ACCT CLIENTS*
      **     FOR THE  "TO"  ACCOUNT.                                   *
      ******************************************************************
      *
      *!WF DSP=TO DSL=DU SEL=14 FOR=I DES=1 LEV=1 PLT=75
       01                 TO14.                                         CI0074
            10            TO14-C299.                                    CI0074
            11            TO14-CTID.                                    CI0074
            12            TO14-CTIDA  PICTURE  9(3).                    CI0074
            12            TO14-CTIDN.                                   CI0074
            13            TO14-CTIDNP PICTURE  X(13).                   CI0074
            13            TO14-CTIDND PICTURE  9(11).                   CI0074
            10            TO14-DCACG  PICTURE  9(8).                    CI0074
            10            TO14-IPOCH  PICTURE  X.                       CI0074
            10            TO14-FILLER PICTURE  X(100).                  CI0074
            10            TO14-CLID01.                                  CI0074
            11            TO14-CLIDO1 PICTURE  X(3).                    CI0074
            11            TO14-NCLID1.                                  CI0074
            12            TO14-CLIDP1 PICTURE  X(12).                   CI0074
            12            TO14-CLIDNA PICTURE  9(8).                    CI0074
            10            TO14-CLCTR  PICTURE  9(3).                    CI0074
            10            TO14-DU21                                     CI0074
                          OCCURS       025     TIMES.                   CI0074
            11            TO14-C199.                                    CI0074
            12            TO14-CLID.                                    CI0074
            13            TO14-CLIDO  PICTURE  9(3).                    CI0074
            13            TO14-CLIDN.                                   CI0074
            14            TO14-CLIDNP PICTURE  X(12).                   CI0074
            14            TO14-CLIDND PICTURE  9(8).                    CI0074
            11            TO14-CLCTRC PICTURE  9(3).                    CI0074
            10            TO14-QITEM  PICTURE  9(3).                    CI0074
            10            TO14-XIMAX  PICTURE  S9(4)                    CI0074
                          BINARY.                                       CI0074
            10            TO14-CRROL  PICTURE  X.                       CI0074
            10            TO14-FILLER PICTURE  X(099).                  CI0074
      *
      *
      *
      *
      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0074
          05              DE00-SUITE.                                   CI0074
            15       FILLER         PICTURE  X(00653).                  CI0074
       01                 DE10  REDEFINES      DE00.                    CI0074
            10            DE10-DU11.                                    CI0074
            11            DE10-XFONC  PICTURE  X(4).                    CI0074
            11            DE10-MPSBN  PICTURE  X(8).                    CI0074
            11            DE10-XDBDNM PICTURE  X(08).                   CI0074
            11            DE10-XSEGNM PICTURE  X(08).                   CI0074
            11            DE10-XRC    PICTURE  X(02).                   CI0074
            11            DE10-MSEG   PICTURE  X(08).                   CI0074
            11            DE10-XCOKEY PICTURE  X(70).                   CI0074
            11            DE10-CUIBR  PICTURE  X(01).                   CI0074
            11            DE10-CUIBA  PICTURE  X(01).                   CI0074
            11            DE10-IPBIK  PICTURE  X(1).                    CI0074
            10            DE10-DU03.                                    CI0074
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0074
                          COMPUTATIONAL-3.                              CI0074
            11            DE10-CMSSF  PICTURE  XX.                      CI0074
            11            DE10-DU09.                                    CI0074
            12            DE10-CMESA  PICTURE  S9(9)                    CI0074
                          BINARY.                                       CI0074
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0074
                          BINARY.                                       CI0074
            12            DE10-CMESB  PICTURE  S9(9)                    CI0074
                          BINARY.                                       CI0074
            12            DE10-CMSST  PICTURE  S9(9)                    CI0074
                          BINARY.                                       CI0074
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0074
                          BINARY.                                       CI0074
            12            DE10-QELLAA PICTURE  S9(9)                    CI0074
                          BINARY.                                       CI0074
            12            DE10-TMESS4 PICTURE  X(512).                  CI0074
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0074
          05              MS00-SUITE.                                   CI0074
            15       FILLER         PICTURE  X(00542).                  CI0074
       01                 MS03  REDEFINES      MS00.                    CI0074
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0074
                          COMPUTATIONAL-3.                              CI0074
            10            MS03-CMSSF  PICTURE  XX.                      CI0074
            10            MS03-DU09.                                    CI0074
            11            MS03-CMESA  PICTURE  S9(9)                    CI0074
                          BINARY.                                       CI0074
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0074
                          BINARY.                                       CI0074
            11            MS03-CMESB  PICTURE  S9(9)                    CI0074
                          BINARY.                                       CI0074
            11            MS03-CMSST  PICTURE  S9(9)                    CI0074
                          BINARY.                                       CI0074
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0074
                          BINARY.                                       CI0074
            11            MS03-QELLAA PICTURE  S9(9)                    CI0074
                          BINARY.                                       CI0074
            11            MS03-TMESS4 PICTURE  X(512).                  CI0074
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0074
            10            MX11-QMSGS  PICTURE  9(03).                   CI0074
            10            MX11-PJ09                                     CI0074
                          OCCURS       025     TIMES.                   CI0074
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0074
                          COMPUTATIONAL-3.                              CI0074
            11            MX11-CMESB  PICTURE  S9(9)                    CI0074
                          BINARY.                                       CI0074
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PJ15
                                FR14
                                TO14
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N0TSC.    NOTE *---> DATABASE POINTERS...          *.
       F0TSC.                                                           lv10
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR GR1P                                             DOT
           SET ADDRESS OF PD06 TO                                       ADU015
                PCB-GR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR TR1P                                             DOT
           SET ADDRESS OF PE06 TO                                       ADU015
                PCB-TR1P-PTR1.                                          ADU015
       F0TSC-FN. EXIT.
      *N01.      NOTE *************************************.            CI0074
      *               *                                   *             CI0074
      *               *INITIALISATIONS                    *             CI0074
      *               *                                   *             CI0074
      *               *************************************.            CI0074
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
      *N02CA.    NOTE *STORE THE USER AND UNIT            *.
       F02CA.                                                           lv10
           MOVE        PJ15-GEOPID TO ACF-USER-ID
           MOVE        PJ15-CAUNIT TO ACF-USER-UNIT.
       F02CA-FN. EXIT.
      *N02DA.    NOTE *INITIALIZATIONS                    *.
       F02DA.                                                           lv10
      *WORK FIELDS
           MOVE        LOW-VALUES TO 7-SACV-CODES
      *DATABASE SEGMENTS
           INITIALIZE  TC01
           TC10.
       F02DA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0074
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0074
      *               *                                   *             CI0074
      *               *FIN DE TRAITEMENT                  *             CI0074
      *               *                                   *             CI0074
      *               *************************************.            CI0074
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0074
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATE PARMS                     *
      *               *                                   *
      *               *************************************.
       F30.           EXIT.                                             lv05
      *N30DA.    NOTE *IF INQ/UPD CODE VALID              *.
       F30DA.    IF    PJ15-CUPIQ = 'I'                                 lv10
                 OR    PJ15-CUPIQ = 'U'
                 NEXT SENTENCE ELSE GO TO     F30DA-FN.
      *N30DD.    NOTE *ONLY ALLOW SD TO UPDATE            *.
       F30DD.    IF    PJ15-CUPIQ = 'U'                                 lv15
                 AND   PJ15-MAPPN NOT = 'SD'
                 NEXT SENTENCE ELSE GO TO     F30DD-FN.
           MOVE        'I' TO PJ15-CUPIQ.
       F30DD-FN. EXIT.
       F30DA-900. GO TO F30DT-FN.
       F30DA-FN. EXIT.
      *N30DT.    NOTE *ELSE... DEFAULT IT TO INQUIRY      *.
       F30DT.                                                           lv10
           MOVE        'I' TO PJ15-CUPIQ.
       F30DT-FN. EXIT.
      *N30EA.    NOTE *IF IN UPDATE MODE A SETTLEMENT     *.
       F30EA.    IF    PJ15-CUPIQ = 'U'                                 lv10
                 AND   PJ15-CLCUS = ZERO
                 NEXT SENTENCE ELSE GO TO     F30EA-FN.
      *CODE MUST BE PASSED
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012219 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30EA-FN. EXIT.
      *N30FA.    NOTE *IF DOING AN EXCHANGE EXIT          *.
       F30FA.    IF    PJ15-CTYPE = 'E'                                 lv10
                 AND   PJ15-MAPPN NOT = 'SD'
                 NEXT SENTENCE ELSE GO TO     F30FA-FN.
      *BECAUSE EXCHANGE IS NOT A
      *REPORTABLE EVENT
      *(ON SD'S, LET ALL CTYPES THRU)
           MOVE        ZERO TO PJ15-CLCUS
           7-SACV-CLCUS
      *GET ALPHA EQUIVALENT FOR CLCUS
           PERFORM     F95SA THRU F95SA-FN
           MOVE        7-SACV-CLCUSA TO PJ15-CLCUSA
           MOVE                     ALL '1' TO FT GO TO F20.
       F30FA-FN. EXIT.
       F30-FN.   EXIT.
      *N34.      NOTE *************************************.
      *               *                                   *
      *               *ACCESS SEGMENTS                    *
      *               *                                   *
      *               *************************************.
       F34.           EXIT.                                             lv05
      *N34DA.    NOTE *GU CT01 - SOURCE ACCOUNT           *.
       F34DA.                                                           lv10
           MOVE        PJ15-CTID TO S-CTU01-CT01K
           PERFORM     F94CA THRU F94CA-FN
           MOVE        CT01-PRSCD TO WS01-PRSCD.
       F34DA-FN. EXIT.
      *N34DG.    NOTE *SEND MESSAGE IF NOT FOUND          *.
       F34DG.    IF    IK = '1'                                         lv10
                 NEXT SENTENCE ELSE GO TO     F34DG-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012011 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F34DG-FN. EXIT.
      *N34GA.    NOTE *GET THE ACCOUNT GROUP SEGM CT10    *.
       F34GA.                                                           lv10
           PERFORM     F92KK THRU F92KK-FN.
      *N34GM.    NOTE *CT10 NOT FOUND                     *.
       F34GM.    IF    CT10-CF = ZERO                                   lv15
                 NEXT SENTENCE ELSE GO TO     F34GM-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012227 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F34GM-FN. EXIT.
       F34GA-FN. EXIT.
      *N34HA.    NOTE *PENSION GROUP                      *.
       F34HA.    IF    CT10-GRIDC = 002                                 lv10
                 NEXT SENTENCE ELSE GO TO     F34HA-FN.
      *GU GR01 (GROUP ROOT SEG)
           MOVE        CT10-GR98 TO S-GRU01-GR01K
           PERFORM     F94CG THRU F94CG-FN.
                 IF    IK = '1'                                         DOT
      *GR01 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012227 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *GN GR07 (GROUP PENSION SEG)                                      DOT
           PERFORM     F94CH THRU F94CH-FN.
                 IF    IK = '1'                                         DOT
      *GR07 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012228 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F34HA-FN. EXIT.
      *N34IA.    NOTE *---> ONLY DETERMINE TAX            *.
       F34IA.    IF    CT01-CTCCI = 1                                   lv10
                 OR    GR07-CIDRP = 1
                 OR    (CT01-CTIDA = 004 OR 005
                 AND   (WS01-ICODE = 2 OR 3 OR 5
                 OR    CT01-CQACT = 1 OR 2 OR 3))
                 NEXT SENTENCE ELSE GO TO     F34IA-FN.
      *     SETTLEMENT CODE FOR
      *     CUSTODIAL ACCOUNTS
      *AND ANNUITY ACCOUNTS THAT ARE
      *PUBLIC SCHOOLS 403B OR
      *501(C)(3) EMPLOYEES(NON PROFITS)
      *OR IRA
       F34IA-900. GO TO F34IE-FN.
       F34IA-FN. EXIT.
      *N34IE.    NOTE *ELSE... NOT A CUSTODIAL ACCOUNT    *.
       F34IE.                                                           lv10
      *  SETTLEMENT CODE IS N/A
           MOVE        ZERO TO PJ15-CLCUS
           7-SACV-CLCUS
      *GET ALPHA EQUIVALENT FOR CLCUS
           PERFORM     F95SA THRU F95SA-FN
           MOVE        7-SACV-CLCUSA TO PJ15-CLCUSA
           MOVE                     ALL '1' TO FT GO TO F20.
       F34IE-FN. EXIT.
      *N34PA.    NOTE *GU CL01 - SOURCE ACCT TAXPAYER     *.
       F34PA.                                                           lv10
           MOVE        PJ15-CLID TO S-CLU01-CL01K
           PERFORM     F94CD THRU F94CD-FN.
       F34PA-FN. EXIT.
      *N34QA.    NOTE *ACCESS THE CL03 IF PERSON          *.
       F34QA.    IF    IK = '0'                                         lv10
                 AND   CL01-CLTYP = 'P'
                 NEXT SENTENCE ELSE GO TO     F34QA-FN.
           PERFORM     F94CE THRU F94CE-FN.
      *N34QG.    NOTE *CL03 ACCESS FAILED                 *.
       F34QG.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F34QG-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012161 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F34QG-FN. EXIT.
       F34QA-900. GO TO F34RA-FN.
       F34QA-FN. EXIT.
      *N34RA.    NOTE *ELSE... DON'T HAVE PERSON CLIENT   *.
       F34RA.         EXIT.                                             lv10
      *N34RG.    NOTE *IF NO CL01                         *.
       F34RG.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F34RG-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012012 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F34RG-900. GO TO F34RM-FN.
       F34RG-FN. EXIT.
      *N34RM.    NOTE *ELSE.... A NON PERSON GET OUT      *.
       F34RM.                                                           lv15
                 IF    CT01-CIRAT = 001                                 DOT
                 AND   CT01-CIRAS = 003
      *ORG CLIENTS/BENE IRAS
           MOVE        4 TO PJ15-CLCUS
                 ELSE
           MOVE        ZERO TO PJ15-CLCUS
           7-SACV-CLCUS.
      *GET ALPHA EQUIVALENT FOR CLCUS                                   DOT
           PERFORM     F95SA THRU F95SA-FN
           MOVE        7-SACV-CLCUSA TO PJ15-CLCUSA
           MOVE                     ALL '1' TO FT GO TO F20.
       F34RM-FN. EXIT.
       F34RA-FN. EXIT.
      *N34SA.    NOTE *IF A DEATH TAX SETTLEMENT CODE     *.
       F34SA.    IF    (CT01-CLCUS = 04 OR 25 OR 36                     lv10
                       OR 45)
                 AND   CT01-CIRAS NOT = 003
                 AND   CL03-CLDTH = 'N'
                 AND   EIBTRNID NOT = ('MD28' OR
                       'XD28')
                 NEXT SENTENCE ELSE GO TO     F34SA-FN.
      *IS SET ON THE ACCOUNT AND THE
      *CLIENT IS NOT SET TO DEATH AND
      *THE ACCOUNT IS NOT BENEFICIAL
      *PRODUCE A CRITICAL ERROR--THIS
      *IS MOST LIKELY AN INCORRECTLY
      *CODED ACCOUNT.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013464 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F34SA-FN. EXIT.
      *N34SF.    NOTE *IF ACCOUNT IS NOT BENEFICIAL       *.
       F34SF.    IF    CT01-CIRAS NOT = 003                             lv10
                 AND   CL03-CLDTH = 'Y'
                 AND   EIBTRNID NOT = ('MD28' OR
                       'XD28')
                 NEXT SENTENCE ELSE GO TO     F34SF-FN.
      *AND CLIENT IS SET TO DEATH
      *PRODUCE A CRITICAL ERROR--THE
      *CLIENT BY DEFINITION MUST BE
      *DEAD TO HAVE A BENEFICIAL ACCT.
      *- FOR MONEY TO BE MOVED FOR DEAD
      *CLIENTS IT CAN'T BE DONE IN
      *EZTRANS AND MUST BE DONE ON KD95
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013464 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F34SF-FN. EXIT.
      *N34TA.    NOTE *FIND DESTINATION ACCOUNT INFO      *.
       F34TA.    IF    PJ15-CTID01 > ZERO                               lv10
                 NEXT SENTENCE ELSE GO TO     F34TA-FN.
      *IF THE DESTINATION IS ACCOUNT
      *N34TE.    NOTE *GU CT01 - DESTINATION ACCT         *.
       F34TE.                                                           lv15
           MOVE        PJ15-CTID01 TO S-CTU01-CT01K
           PERFORM     F94CJ THRU F94CJ-FN.
       F34TE-FN. EXIT.
      *N34TH.    NOTE *IF CT01 IS NOT FOUND               *.
       F34TH.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F34TH-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012037 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F34TH-FN. EXIT.
      *N34TM.    NOTE *GET CT10                           *.
       F34TM.                                                           lv15
      *FIRST PUT SOURCE ACCT CT10 IN
      *TEMP HOLDING AREA
           MOVE        CT10 TO HO10
           PERFORM     F92KK THRU F92KK-FN.
       F34TM-FN. EXIT.
      *N34TP.    NOTE *IF CT10 IS NOT FOUND               *.
       F34TP.    IF    CT10-CF = ZERO                                   lv15
                 NEXT SENTENCE ELSE GO TO     F34TP-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012037 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F34TP-FN. EXIT.
      *N34TQ.    NOTE *MOVE SOURCE/DESTINATION CT10'S     *.
       F34TQ.                                                           lv15
           MOVE        CT10 TO TC10
           MOVE        HO10 TO CT10.
       F34TQ-FN. EXIT.
       F34TA-FN. EXIT.
       F34-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *CALCULATE CLIENT AGE               *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *********************************
      ** IN THIS SECTION GET THE      *
      ** CLIENTS AGE BASED ON CURRENT *
      ** DATE.                        *
      *********************************
      *N40DA.    NOTE *GET THE CURRENT ACCOUNTING DATE    *.
       F40DA.                                                           lv10
           PERFORM     F95AB THRU F95AB-FN.
       F40DA-FN. EXIT.
      *N40EA.    NOTE *CALCULATE THE CLIENT'S AGE         *.
       F40EA.                                                           lv10
           PERFORM     F95AF THRU F95AF-FN.
       F40EA-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *SET CUSTODIAL CODE                 *
      *               *                                   *
      *               *************************************.
       F45.           EXIT.                                             lv05
      *N45EA.    NOTE *DETERMINE DEFAULT CODE             *.
       F45EA.                                                           lv10
      *********************************
      *RULES ARE:
      *1.SET BASED ON AGE
      *2.OVERRIDE IF APPROPRIATE CODE
      *  ON SOURCE ACCOUNT (CT01)
      *3.OVERRIDE IF CLIENT IS DEAD
      *  OR IT'S A BENEFICIAL IRA
      *4.OVERRIDE IF MONEY IS MOVING
      *  BETWEEN TWO OF A TAXPAYER'S
      *  ACCOUNTS AND IS NON-REPORTABLE
      *5.OVERRIDE IF DESTINATION IS A
      *  QUALIFIED ANNUITY/LIFE ACCOUNT
      *  AEFA IS THE CUSTODIAN AND THE
      *  TRAN IS NON-REPORTABLE
      *6.OVERRIDE IF MONEY IS MOVING
      *  BETWEEN A IRA ROLLOVER AND A
      *  PENSION, TSA, OR TSCA ACCOUNT.
      *7.OVERRIDE IF CUSTODIAL TRANSFER
      *********************************
      *N45EK.    NOTE *IF OVER 59.5 YEARS OLD; RETIRE     *.
       F45EK.    IF    7-OAGE-CLIENT-AGE NOT < 59.5                     lv15
                 NEXT SENTENCE ELSE GO TO     F45EK-FN.
           MOVE        07 TO DEFAULT-CLCUS.
                 IF    CT01-CIRAT = 005 OR 006                          DOT
      *ROTH (CONTRIB OR CONVERSION)
           MOVE        21 TO DEFAULT-CLCUS.
       F45EK-900. GO TO F45EM-FN.
       F45EK-FN. EXIT.
      *N45EM.    NOTE *ELSE.... PREMATURE OR UNKNOWN      *.
       F45EM.         EXIT.                                             lv15
      *N45EP.    NOTE *IF AGE WAS CALCULATED; PREMATURE   *.
       F45EP.    IF    7-OAGE-CLIENT-AGE > ZERO                         lv20
                 NEXT SENTENCE ELSE GO TO     F45EP-FN.
           MOVE        01 TO DEFAULT-CLCUS.
                 IF    CT01-CIRAT = 005 OR 006                          DOT
      *ROTH (CONTRIB OR CONVERSION)
           MOVE        22 TO DEFAULT-CLCUS.
       F45EP-900. GO TO F45ET-FN.
       F45EP-FN. EXIT.
      *N45ET.    NOTE *IF NO AGE; SET TO UNKNOWN          *.
       F45ET.                                                           lv20
           MOVE        00 TO DEFAULT-CLCUS
      *SESSION 5032, REMOVED THE
      *SEPERATE IF CODE FOR 'SD'.
      *NOW SEND THE CRITICAL MESSAGE
      *FOR BOTH 'SD' AND 'UD' FROM
      *THIS PROGRAM.  REMOVED THE
      *CODE FROM CI9007 THAT USED TO
      *SEND THIS MESSAGE FOR 'SD'.
                 IF    PJ15-MAPPN = 'UD'                                DOT
                 OR    PJ15-MAPPN = 'SD'
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012283 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45ET-FN. EXIT.
       F45EM-FN. EXIT.
      *N45EX.    NOTE *IF ED IRA CODE IS 'EIR'            *.
       F45EX.    IF    CT01-CIRAT = 007                                 lv15
                 NEXT SENTENCE ELSE GO TO     F45EX-FN.
           MOVE        43 TO DEFAULT-CLCUS.
       F45EX-FN. EXIT.
       F45EA-FN. EXIT.
      *N45MA.    NOTE *OVERRIDE DEFAULT IF APPROPRIATE    *.
       F45MA.    IF    CT01-CLCUS NOT = ZERO                            lv10
                       AND 01 AND 07
                       AND 21 AND 22
                       AND 32 AND 33
                       AND 04 AND 25
                       AND 36 AND 45
                       AND 99
                 NEXT SENTENCE ELSE GO TO     F45MA-FN.
      *SETTLEMENT CODE IS ON CT01
      *DON'T USE:
      *'PRE' (01) OR 'RET' (07) OR
      *'RTH' (21) OR 'RPR' (22) OR
      *'CON' (32) OR 'CPR' (33) OR
      *'DTH' (04) OR 'RDH' (25) OR
      *'CDH' (36) OR 'EDH' (45) OR
      *'NON' (99) FROM
      *THE CT01 BUT INSTEAD USE
      *THE CODE DETERMINED BY ITS OWN
      *LOGIC.  THIS WILL CORRECT
      *ANY ROTH ACCOUNTS THAT
      *INCORRECTLY ARE SET TO 'PRE' OR
      *'RET' AS WELL AS CHECK THE
      *CLIENT'S AGE AT THE TIME OF THE
      *THE DISBURSEMENT.  IT ALSO MAKES
      *THE CL03-CLDTH THE CONTROL
      *FOR SETTING A DEATH SETTLEMENT
      *CODE.
           MOVE        CT01-CLCUS TO DEFAULT-CLCUS.
       F45MA-FN. EXIT.
      *N45PA.    NOTE *OVERRIDE DEFAULT IF CLIENT HAS     *.
       F45PA.    IF    CL03-CLDTH = 'Y'                                 lv10
                 OR    CT01-CIRAS = 003
                 NEXT SENTENCE ELSE GO TO     F45PA-FN.
      *DIED OR IT'S A BENEFICIAL IRA
      *N45PE.    NOTE *SET DEFAULT DEATH SETTLEMENT       *.
       F45PE.                                                           lv15
      *CODE TO 'DTH'
           MOVE        04 TO DEFAULT-CLCUS.
       F45PE-FN. EXIT.
      *N45PM.    NOTE *OVERRIDE FOR ROTH AND ROTH BENE    *.
       F45PM.    IF    CT01-CQACT = 001                                 lv15
                 AND   (CT01-CIRAS = 001 OR 003)
                 AND   (CT01-CIRAT = 005 OR 006)
                 NEXT SENTENCE ELSE GO TO     F45PM-FN.
      *- 'RDH'
           MOVE        25 TO DEFAULT-CLCUS.
       F45PM-FN. EXIT.
      *N45PP.    NOTE *OVERRIDE FOR ED IRA - 'EDH'        *.
       F45PP.    IF    CT01-CQACT = 001                                 lv15
                 AND   CT01-CIRAS = 001
                 AND   CT01-CIRAT = 007
                 NEXT SENTENCE ELSE GO TO     F45PP-FN.
           MOVE        45 TO DEFAULT-CLCUS.
       F45PP-FN. EXIT.
       F45PA-FN. EXIT.
      *N45SA.    NOTE *OVERRIDE DEFAULT IF MONEY IS       *.
       F45SA.    IF    FR14-CLID01 = TO14-CLID01                        lv10
                 NEXT SENTENCE ELSE GO TO     F45SA-FN.
      *MOVED BETWEEN TWO OF A TAX-
      *PAYER'S ACCOUNTS AND MEETS ONE
      *OF CONDITIONS BELOW
      *THESE TRANSACTIONS ARE NON TAX
      *REPORTABLE (CODE OF 'NON').
      *N45SB.    NOTE *SOURCE IS TRAD IRA -ANNUITY        *.
       F45SB.    IF    CT01-CQACT = 001                                 lv15
                 AND   CT01-CIRAS = 001
                 AND   CT01-CIRAT = 001
                 AND   CT01-CTIDA = (004 OR 005)
                 NEXT SENTENCE ELSE GO TO     F45SB-FN.
      *N45SC.    NOTE *DESTINATION IS TRAD IRA            *.
       F45SC.    IF    TC01-CQACT = 001                                 lv20
                 AND   TC01-CIRAS = 001
                 AND   TC01-CIRAT = 001
                 NEXT SENTENCE ELSE GO TO     F45SC-FN.
           MOVE        99 TO DEFAULT-CLCUS.
       F45SC-FN. EXIT.
       F45SB-FN. EXIT.
      *N45SE.    NOTE *SOURCE IS NOT AN SEP               *.
       F45SE.    IF    CT01-CQACT = 001                                 lv15
                 AND   CT01-CIRAS = 001
                 AND   CT01-CIRAT NOT = 003
                 NEXT SENTENCE ELSE GO TO     F45SE-FN.
      *N45SF.    NOTE *DESTINATION IS SEP                 *.
       F45SF.    IF    TC01-CIRAT = 003                                 lv20
                 NEXT SENTENCE ELSE GO TO     F45SF-FN.
           MOVE        99 TO DEFAULT-CLCUS.
       F45SF-FN. EXIT.
       F45SE-FN. EXIT.
      *N45SI.    NOTE *SOURCE IS INDIVID IRA OR SEP OR    *.
       F45SI.    IF    CT01-CQACT = 001                                 lv15
                 AND   CT01-CIRAS = 001
                 AND   (CT01-CIRAT = 001 OR
                       003 OR
                       004)
                 NEXT SENTENCE ELSE GO TO     F45SI-FN.
      *SRA
      *N45SJ.    NOTE *DESTINATION IS ROLLOVER            *.
       F45SJ.    IF    TC01-CQACT = 001                                 lv20
                 AND   TC01-CIRAS = 002
                 NEXT SENTENCE ELSE GO TO     F45SJ-FN.
           MOVE        99 TO DEFAULT-CLCUS.
       F45SJ-FN. EXIT.
       F45SI-FN. EXIT.
      *N45SM.    NOTE *SOURCE IS ROLLOVER                 *.
       F45SM.    IF    CT01-CQACT = 001                                 lv15
                 AND   CT01-CIRAS = 002
                 NEXT SENTENCE ELSE GO TO     F45SM-FN.
      *N45SN.    NOTE *DESTINATION IS INDIVID IRA         *.
       F45SN.    IF    TC01-CQACT = 001                                 lv20
                 AND   TC01-CIRAS = 001
                 AND   TC01-CIRAT = 001
                 NEXT SENTENCE ELSE GO TO     F45SN-FN.
           MOVE        99 TO DEFAULT-CLCUS.
       F45SN-FN. EXIT.
       F45SM-FN. EXIT.
      *N45SS.    NOTE *SOURCE IS ROLLOVER                 *.
       F45SS.    IF    CT01-CQACT = 001                                 lv15
                 AND   CT01-CIRAS = 002
                 NEXT SENTENCE ELSE GO TO     F45SS-FN.
      *N45ST.    NOTE *DESTINATION IS SEP                 *.
       F45ST.    IF    TC01-CIRAT = 003                                 lv20
                 NEXT SENTENCE ELSE GO TO     F45ST-FN.
           MOVE        99 TO DEFAULT-CLCUS.
       F45ST-FN. EXIT.
       F45SS-FN. EXIT.
      *N45TA.    NOTE *SOURCE IS SEP                      *.
       F45TA.    IF    CT01-CIRAT = 003                                 lv15
                 NEXT SENTENCE ELSE GO TO     F45TA-FN.
      *N45TB.    NOTE *DESTINATION IS INDIVID IRA         *.
       F45TB.    IF    TC01-CQACT = 001                                 lv20
                 AND   TC01-CIRAS = 001
                 AND   TC01-CIRAT = 001
                 NEXT SENTENCE ELSE GO TO     F45TB-FN.
           MOVE        99 TO DEFAULT-CLCUS.
       F45TB-FN. EXIT.
       F45TA-FN. EXIT.
      *N45TF.    NOTE *SOURCE IS ROTH CONTRIBUTORY        *.
       F45TF.    IF    CT01-CQACT = 001                                 lv15
                 AND   CT01-CIRAS = 001
                 AND   CT01-CIRAT = 005
                 NEXT SENTENCE ELSE GO TO     F45TF-FN.
      *N45TG.    NOTE *DESTINATION IS ROTH CONVERSION     *.
       F45TG.    IF    TC01-CQACT = 001                                 lv20
                 AND   TC01-CIRAS = 001
                 AND   TC01-CIRAT = 006
                 NEXT SENTENCE ELSE GO TO     F45TG-FN.
           MOVE        99 TO DEFAULT-CLCUS.
       F45TG-FN. EXIT.
       F45TF-FN. EXIT.
      *N45TH.    NOTE *SOURCE IS ROTH COVERSION           *.
       F45TH.    IF    CT01-CQACT = 001                                 lv15
                 AND   CT01-CIRAS = 001
                 AND   CT01-CIRAT = 006
                 NEXT SENTENCE ELSE GO TO     F45TH-FN.
      *N45TI.    NOTE *DESTINATION IS ROTH CONTRIBUTORY   *.
       F45TI.    IF    TC01-CQACT = 001                                 lv20
                 AND   TC01-CIRAS = 001
                 AND   TC01-CIRAT = 005
                 NEXT SENTENCE ELSE GO TO     F45TI-FN.
           MOVE        99 TO DEFAULT-CLCUS.
       F45TI-FN. EXIT.
       F45TH-FN. EXIT.
      *N45TJ.    NOTE *SOURCE IS TSCA                     *.
       F45TJ.    IF    CT01-CQACT = 004                                 lv15
                 NEXT SENTENCE ELSE GO TO     F45TJ-FN.
      *N45TK.    NOTE *DESTINATION IS TSA                 *.
       F45TK.    IF    TC01-CQACT = 002 OR 003                          lv20
                 NEXT SENTENCE ELSE GO TO     F45TK-FN.
           MOVE        99 TO DEFAULT-CLCUS.
       F45TK-FN. EXIT.
       F45TJ-FN. EXIT.
      *N45TT.    NOTE *SAME GROUP CATEGORY                *.
       F45TT.    IF    CT10-GRIDC = TC10-GRIDC                          lv15
                 NEXT SENTENCE ELSE GO TO     F45TT-FN.
      *N45TU.    NOTE *DON'T CHECK CTYPE ON SD            *.
       F45TU.    IF    PJ15-MAPPN = 'SD'                                lv18
                 OR    (PJ15-MAPPN NOT = 'SD'
                 AND   ((PJ15-CTYPE = 'T'
                 AND   CT01-CTIDA NOT = 004
                 AND   CT01-CTIDA NOT = 005)
                 OR    (PJ15-CTYPE = ('T'
                 OR    'E' OR 'D')
                 AND   CT01-CTIDA = (004 OR 005))))
                 NEXT SENTENCE ELSE GO TO     F45TU-FN.
      *IF NOT SD, CHECK FOR CTYPE = T
      *FOR NON-ANNUITY,
      *CHECK CTYPE = T,E,D
      *FOR ANNUIYT
      *N45TV.    NOTE *HOUSEHOLD GROUP                    *.
       F45TV.    IF    CT10-GRIDC = 001                                 lv20
                 AND   CT01-CQACT = TC01-CQACT
                 AND   CT01-CIRAS = TC01-CIRAS
                 AND   CT01-CIRAT = TC01-CIRAT
                 NEXT SENTENCE ELSE GO TO     F45TV-FN.
      *SAME QUALIFIED ACCOUNT TYPE
      *SAME IRA STATUS
      *SAME IRA TYPE
           MOVE        99 TO DEFAULT-CLCUS.
       F45TV-FN. EXIT.
      *N45TW.    NOTE *PENSION GROUP                      *.
       F45TW.    IF    CT10-GRIDC = 002                                 lv20
                 NEXT SENTENCE ELSE GO TO     F45TW-FN.
           MOVE        99 TO DEFAULT-CLCUS.
       F45TW-FN. EXIT.
       F45TU-FN. EXIT.
       F45TT-FN. EXIT.
      *N45TX.    NOTE *ANYTHING TO A QUALIFIED ANNUITY    *.
       F45TX.    IF    TC01-CQACT = 001                                 lv15
                 AND   TC10-GRIDC NOT = 002
                 AND   (TC01-CTIDA = 004 OR 005)
                 NEXT SENTENCE ELSE GO TO     F45TX-FN.
      *OR LIFE PRODUCT
      *THAT IS NOT A PENSION, TSA, OR
      *TSCA IS NON-TAXABLE (BECAUSE
      *AEFA IS THE CUSTODIAN OF THE
      *ANNUITY).  NOTE: THE SOURCE WILL
      *ALWAYS BE A QUALIFIED ACCOUNT
      *BECAUSE OF THE EDIT ON
      *NON-QUALIFIED ACCOUNTS IN THE
      *ACCESS SEGMENTS FUNCTION (F34)
           MOVE        99 TO DEFAULT-CLCUS.
       F45TX-FN. EXIT.
       F45SA-FN. EXIT.
      *N45XA.    NOTE *OVERRIDE DEFAULT IF MONEY IS       *.
       F45XA.    IF    CT01-CQACT = 001                                 lv10
                 AND   CT01-CIRAS = 002
                 NEXT SENTENCE ELSE GO TO     F45XA-FN.
      *BEING MOVED BETWEEN AN IRA
      *ROLLOVER ACCOUNT AND A PENSION
      *TSA, OR TSCA ACCOUNT. SETTLEMENT
      *CODE IS DIRECT ROLLOVER TO A
      *QUALIFIED PLAN - DRQ
      *N45XC.    NOTE *DESTINATION IS PENSION, TSA        *.
       F45XC.    IF    TC10-GRIDC = 002                                 lv15
                 OR    TC01-CQACT = 002 OR 003
                       OR 004
                 NEXT SENTENCE ELSE GO TO     F45XC-FN.
      *OR TSCA
           MOVE        11 TO DEFAULT-CLCUS.
       F45XC-FN. EXIT.
       F45XA-FN. EXIT.
      *N45YA.    NOTE *OVERRIDE TO 'NON' IF THIS IS A     *.
       F45YA.    IF    PJ15-CPAYF = 'CT'                                lv10
                 NEXT SENTENCE ELSE GO TO     F45YA-FN.
      *CUSTODIAL TRANSFER
           MOVE        99 TO DEFAULT-CLCUS.
       F45YA-FN. EXIT.
       F45-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *UPDATE CONTRACT DB IF NECESSARY    *
      *               *                                   *
      *               *************************************.
       F60.           EXIT.                                             lv05
      *N60CA.    NOTE *UPDATE CONTRACT IF NECESSARY       *.
       F60CA.    IF    PJ15-CUPIQ = 'U'                                 lv10
                 AND   CT01-CLCUS NOT =
                       DEFAULT-CLCUS
                 AND   DEFAULT-CLCUS NOT = 11
                 NEXT SENTENCE ELSE GO TO     F60CA-FN.
      *- IF CT01 IS NOT SET
      * AND DETERMINED TAX SETTLEMENT
      * CODE IS DIFFERENT THAN CT01
      * & IT'S NOT 11 (DIRECT ROLLOVER
      * TO QUALIFIED PLAN OR TSA)
      *- THIS MEANS THAT FOR SD THE
      * SETTLEMENT CODE COULD BE SET TO
      * ANYTHING (INCLUDING DEATH) THAT
      * IS SET BY THE "SET CUSTODIAL
      * CODE" LOGIC.
                 IF    CT01-CTIDA = 002                                 DOT
                 AND   DEFAULT-CLCUS = 99
      *IF SOURCE ACCT IS MUTUAL FUND &
      *SD IS NON-REPORTABLE, GET OUT.
      *THIS WILL ALLOW CERTS WITH A 99
      *TO CONTINUE, SIMILAR TO KA60,
      *F35FM.
               GO TO     F60CA-FN.
      *N60EA.    NOTE *IF MODULE WAS PASSED A             *.
       F60EA.    IF    PJ15-CLCUS NOT =                                 lv15
                       DEFAULT-CLCUS
                 NEXT SENTENCE ELSE GO TO     F60EA-FN.
      *SETTLEMENT CODE DIFFERENT THAN
      *WHAT THIS MODULE DETERMINED WAS
      *THE APPROPRIATE CODE...ERROR
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013468 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F60EA-FN. EXIT.
      *N60FA.    NOTE *READ SOURCE ACCT CT01 FOR UPDATE   *.
       F60FA.                                                           lv15
           MOVE        PJ15-CTID TO S-CTU01-CT01K
           PERFORM     F94CB THRU F94CB-FN.
       F60FA-FN. EXIT.
      *N60GA.    NOTE *CT01 FOUND, CONTINUE               *.
       F60GA.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F60GA-FN.
      *
      *********************************
      *SAVE CT01 FOR AUDIT LOGGING
      *********************************
      *
           MOVE        CT01 TO AS01.
      *N60HA.    NOTE *REPLACE THE CT01                   *.
       F60HA.                                                           lv20
           MOVE        DEFAULT-CLCUS TO CT01-CLCUS
           PERFORM     F94CC THRU F94CC-FN.
                 IF    IK = '1'                                         DOT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013469 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F60HA-FN. EXIT.
      *N60IA.    NOTE *COMPLETE PROCESS--UPDATE GOOD      *.
       F60IA.         EXIT.                                             lv20
      *N60IH.    NOTE *FORMAT AUDIT LOG - CT01 UPDATE     *.
       F60IH.                                                           lv25
      *FORMAT CODE
           MOVE        +00226 TO DH10-CAUFR
      *ACTIVITY CODE
           MOVE        +00531 TO DH10-CAUAC
           MOVE        AS01-CTCCI TO AL26-CTCCI (1)
           MOVE        CT01-CTCCI TO AL26-CTCCI (2)
           MOVE        AS01-CQACT TO AL26-CQACT (1)
           MOVE        CT01-CQACT TO AL26-CQACT (2)
           MOVE        AS01-CIRAS TO AL26-CIRAS (1)
           MOVE        CT01-CIRAS TO AL26-CIRAS (2)
           MOVE        AS01-CIRAT TO AL26-CIRAT (1)
           MOVE        CT01-CIRAT TO AL26-CIRAT (2)
           MOVE        AS01-CLCUS TO AL26-CLCUS (1)
           MOVE        CT01-CLCUS TO AL26-CLCUS (2)
           MOVE        AS01-DIRAC TO AL26-DIRAC (1)
           MOVE        CT01-DIRAC TO AL26-DIRAC (2)
           MOVE        AL26 TO DH10-GAUVR.
       F60IH-FN. EXIT.
      *N60JA.    NOTE *INSERT AUDIT LOG - CT01 UPDATE     *.
       F60JA.                                                           lv25
           PERFORM     F96AL THRU F96AL-FN.
       F60JA-FN. EXIT.
      *N60KA.    NOTE *CREATE MISC TRAN IF NOT A FUND     *.
       F60KA.    IF    PJ15-CTIDA NOT = 002                             lv25
                 NEXT SENTENCE ELSE GO TO     F60KA-FN.
           PERFORM     F92BA THRU F92BA-FN.
       F60KA-FN. EXIT.
       F60IA-FN. EXIT.
       F60GA-900. GO TO F60MA-FN.
       F60GA-FN. EXIT.
      *N60MA.    NOTE *ELSE... UNABLE TO READ CT01        *.
       F60MA.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013469 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F60MA-FN. EXIT.
       F60CA-FN. EXIT.
       F60-FN.   EXIT.
      *N70.      NOTE *************************************.
      *               *                                   *
      *               *MOVE SETTLEMENT CODE TO LINKAGE    *
      *               *                                   *
      *               *************************************.
       F70.           EXIT.                                             lv05
      *N70BA.    NOTE *MOVE SETTLEMENT CODE TO LINKAGE    *.
       F70BA.                                                           lv10
           MOVE        DEFAULT-CLCUS TO PJ15-CLCUS
           7-SACV-CLCUS
      *GET ALPHA EQUIVALENT FOR CLCUS
           PERFORM     F95SA THRU F95SA-FN
           MOVE        7-SACV-CLCUSA TO PJ15-CLCUSA.
       F70BA-FN. EXIT.
       F70-FN.   EXIT.
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
      *               *MISCELLANEOUS PERFORMED ROUTINES   *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92BA.    NOTE *CUST. SETTLEMENT CODE MISC TRAN    *.
       F92BA.                                                           lv10
      *- CREATE A 00049 MISC TRAN
           INITIALIZE  GQ01 GS49
           MOVE        PJ15-CTID TO GQ01-CANUMB
           MOVE        00049 TO GQ01-CAMCTR
           MOVE        1 TO GQ01-GESQ2
      *FILL IN COMMON MISC TRAN FIELDS
           MOVE        00049 TO TA75-CAMCTR
           PERFORM     F92DA THRU F92DA-FN
      *FILL IN VARIABLE MISC TRN FIELD
           MOVE        CT01-CLCUS TO GS49-CLCUS
           MOVE        GS49 TO GQ01-XMISL
      *TRY TO INSERT MISC TRAN RECORD
           PERFORM     F94C7 THRU F94C7-FN.
      *N92BG.    NOTE *SEGM ALREADY EXISTS; REPLACE IT    *.
       F92BG.    IF    XW05-XRC = 'II'                                  lv15
                 NEXT SENTENCE ELSE GO TO     F92BG-FN.
           MOVE        GQ01-CANUMB TO S-GQU01-CANUMB
           MOVE        GQ01-CAMCTR TO S-GQU01-CAMCTR
           MOVE        GQ01-GESQ2 TO S-GQU01-GESQ2
      *GHU GQ01
           PERFORM     F94C5 THRU F94C5-FN.
      *N92BR.    NOTE *GHU ON GQ01 SUCCESSFUL             *.
       F92BR.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F92BR-FN.
      *FILL IN COMMON MISC TRAN FIELDS
           MOVE        00049 TO TA75-CAMCTR
           PERFORM     F92DA THRU F92DA-FN
      *FILL IN VARIABLE MISC TRN FIELDS
      *(LEAVE CRETP AS IT WAS)
           MOVE        GQ01-XMISL TO GS49
           MOVE        CT01-CLCUS TO GS49-CLCUS
           MOVE        GS49 TO GQ01-XMISL
      *REPLACE GQ01
           PERFORM     F94C6 THRU F94C6-FN.
       F92BR-FN. EXIT.
       F92BG-FN. EXIT.
       F92BA-FN. EXIT.
      *N92DA.    NOTE *FILL IN COMMON MISC TRAN FIELDS    *.
       F92DA.                                                           lv10
      *READ TA75
           PERFORM     F94TA THRU F94TA-FN
           MOVE        TA75-NDLEN TO GQ01-GELL
           MOVE        'A' TO GQ01-CENTT
           MOVE        ZERO TO GQ01-CACKD
           MOVE        NS20-DCACG TO GQ01-CADATE
           MOVE        EIBTIME TO GQ01-GETIM
           MOVE        PJ15-GEOPID TO GQ01-GEOPID
           MOVE        PJ15-CAUNIT TO GQ01-CAUNIT
           MOVE        EIBTRMID TO GQ01-XTERMI
           MOVE        PROGR TO GQ01-CAPPL
           MOVE        'CATS' TO GQ01-CSYS.
       F92DA-FN. EXIT.
      *N92KK.    NOTE *GET ATTACHED GROUP AFTER SETTING   *.
       F92KK.                                                           lv10
      *THE COMMAND CODE TO 'GET FIRST'
      *PROCESSING
           MOVE        7-CCOD-FIRST TO S-CT10-CCOD
           MOVE        ZERO TO CT10-CF.
      *N92KM.    NOTE *LOOK FOR ACTIVE HOUSEHOLD OR       *.
       F92KM.                       GO TO     F92KM-B.                  lv15
       F92KM-A.
                 IF    CT10-CF = '1'
                                    GO TO     F92KM-FN.
       F92KM-B.
      *PENSION GROUP
      *GET NEXT CT10 AND RESET THE
      *COMMAND CODE BACK TO 'GET NEXT'
      *PROCESSING
           PERFORM     F94CI THRU F94CI-FN
           MOVE        7-CCOD-NEXT TO S-CT10-CCOD.
                 IF    IK = '1'                                         DOT
      *CT10 NOT FOUND - EXIT LOOP
               GO TO     F92KM-FN.
                 IF    CT10-GERED NOT = ZEROES                          DOT
      *WANT ACTIVE GROUP
               GO TO     F92KM-900.
                 IF    CT10-GRIDC NOT = 001 AND 002                     DOT
      *WANT HH OR PENSION GROUP
               GO TO     F92KM-900.
      *CT10 FOUND
           MOVE        '1' TO CT10-CF.
       F92KM-900. GO TO F92KM-A.
       F92KM-FN. EXIT.
       F92KK-FN. EXIT.
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
      *               *DATA ACCESS FUNCTION               *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94CA.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94CA.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CA-FN. EXIT.
      *N94CB.    NOTE *CALL GHU ON CT01                   *.            ADU026
       F94CB.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PA06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CB-FN. EXIT.
      *N94CC.    NOTE *CALL REPL ON CT01                  *.            ADU026
       F94CC.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PA06 CT01                                                    ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CC-FN. EXIT.
      *N94CD.    NOTE *CALL GU ON CL01                    *.            ADU026
       F94CD.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PC06 CL01                                                    ADU026
           S-CLU01-SSA                                                  ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CD-FN. EXIT.
      *N94CE.    NOTE *CALL GN ON CL03                    *.            ADU026
       F94CE.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PC06 CL03                                                    ADU026
           S-CLU01-SSA S-CL03-SSA                                       ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CE-FN. EXIT.
      *N94CG.    NOTE *CALL GU ON GR01                    *.            ADU026
       F94CG.                                                           lv10
           MOVE        'GR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GR01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PD06 GR01                                                    ADU026
           S-GRU01-SSA                                                  ADU026
           MOVE        PD06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CG-FN. EXIT.
      *N94CH.    NOTE *CALL GN ON GR07                    *.            ADU026
       F94CH.                                                           lv10
           MOVE        'GR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GR07' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PD06 GR07                                                    ADU026
           S-GRU01-SSA S-GR07-SSA                                       ADU026
           MOVE        PD06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CH-FN. EXIT.
      *N94CI.    NOTE *CALL GN ON CT10                    *.            ADU026
       F94CI.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT10' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CT10                                                    ADU026
           S-CTU01-SSA S-CT10-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CI-FN. EXIT.
      *N94CJ.    NOTE *CALL GU ON TC01                    *.            ADU026
       F94CJ.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'TC01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 TC01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CJ-FN. EXIT.
      *N94C5.    NOTE *CALL GHU ON GQ01                   *.            ADU026
       F94C5.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PE06 GQ01                                                    ADU026
           S-GQU01-SSA                                                  ADU026
           MOVE        PE06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C5-FN. EXIT.
      *N94C6.    NOTE *CALL REPL ON GQ01                  *.            ADU026
       F94C6.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PE06 GQ01                                                    ADU026
           MOVE        PE06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C6-FN. EXIT.
      *N94C7.    NOTE *CALL ISRT ON GQ01                  *.            ADU026
       F94C7.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PE06 GQ01                                                    ADU026
           S-GQ01-SSA                                                   ADU026
           MOVE        PE06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C7-FN. EXIT.
      *N94TA.    NOTE *RANDOM TABLE READ FOR TA75         *.            ADUTAB
       F94TA.                                                           lv10
           MOVE        'R1' TO G-TA75-TABFO                             ADUTAB
           COMPUTE     G-TA75-LTH = 60 + G-TA75-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA75-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA75)                                ADUTAB
                       LENGTH (G-TA75-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA75-TABCR NOT = '00'                          DOT
           PERFORM     F94TB THRU F94TB-FN.                             ADUTAB
       F94TA-FN. EXIT.
      *N94TB.    NOTE *ERR: MISC TRAN NOT ON TABLE TA75   *.
       F94TB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012207 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F94TB-FN. EXIT.
       F94-FN.   EXIT.
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *PROG SPECIFIC PERFORMED ROUTINES   *
      *               *                                   *
      *               *************************************.
       F95.           EXIT.                                             lv05
      *N95AB.    NOTE *CALL CI0020 - CAMS ACCTG DATE      *.            AM0020
       F95AB.                                                           lv10
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
      *N95AC.    NOTE *NON-DL1 ERROR                      *.            ADU070
       F95AC.    IF    MS03-NMESS2 > ZERO                               lv15
                 AND   MS03-CMESB > 10                                  ADU070
                 NEXT SENTENCE ELSE GO TO     F95AC-FN.                 ADU070
      *OF A CERTAIN SEVERITY                                            ADU070
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU070
           MOVE        CI0020 TO MS03-TMESS4 (IMS03R : 6)               ADU070
           ADD         +7 TO MS03-QELLAA                                ADU070
           MOVE                     ALL '1' TO FT GO TO F20.            ADU070
       F95AC-900. GO TO F95AD-FN.
       F95AC-FN. EXIT.
      *N95AD.    NOTE *NO ERRORS                          *.            ADU070
       F95AD.                                                           lv15
           INITIALIZE  MS03.                                            ADU070
       F95AD-FN. EXIT.
       F95AB-FN. EXIT.
      *N95AF.    NOTE *CALCULATE THE CLIENT'S AGE         *.
       F95AF.         EXIT.                                             lv10
      *N95AH.    NOTE *CONVERT DATE OF BIRTH TO JULIAN    *.
       F95AH.                                                           lv15
      *********************************
      *THIS ROUTINE WILL RETURN THE
      *CLIENTS DATE OF BIRTH-JULIAN IN
      *FIELD -->> DD01-XDAJP
      *********************************
           MOVE        1 TO DD01-XDACT
           MOVE        1 TO DD01-XDACV
           MOVE        CL03-CLDOB TO DD01-XDAGP.
      *CALL MWS100EX - DYNAMIC                                          DOT
           CALL        MWS100EX USING DD01-DD05.                        AADA58
       F95AH-FN. EXIT.
      *N95BA.    NOTE *PERFORM AGE CALCULATION ROUTINE    *.
       F95BA.                                                           lv15
      *********************************
      *PASS MACRO AAOAGE THE CLIENT'S
      *AGE AND THE CURRENT ACCOUNTING
      *DATE.  THE MACRO WILL RETURN THE
      *CLIENT'S AGE IN FIELD -->
      *      7-OAGE-CURRENT-AGE
      *********************************
           MOVE        NS20-DCACJ TO 7-OAGE-CURRENT-DATE
           MOVE        DD01-XDAJP TO 7-OAGE-BIRTH-DATE
           PERFORM     F96OA THRU F96OA-FN.
       F95BA-FN. EXIT.
       F95AF-FN. EXIT.
      *N95SA.    NOTE *CONVERT SETTLEMENT CODES           *.            ASETCV
       F95SA.         EXIT.                                             lv10
      *N95SB.    NOTE *CONVERT ALPHA TO NUMERIC           *.            ASETCV
       F95SB.    IF    7-SACV-CLCUS = LOW-VALUES                        lv15
                 AND   7-SACV-CLCUSA > LOW-VALUES                       ASETCV
                 NEXT SENTENCE ELSE GO TO     F95SB-FN.                 ASETCV
                 IF    7-SACV-CLCUSA = 'PRE'                            DOT
           MOVE        01 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'ROL'                            DOT
                 OR    7-SACV-CLCUSA = 'PRX'                            ASETCV
           MOVE        02 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'DIS'                            DOT
           MOVE        03 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'DTH'                            DOT
           MOVE        04 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'PRO'                            DOT
           MOVE        05 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'OTH'                            DOT
                 OR    7-SACV-CLCUSA = 'XCH'                            ASETCV
           MOVE        06 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'RET'                            DOT
           MOVE        07 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'EXC'                            DOT
           MOVE        08 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'PSC'                            DOT
           MOVE        09 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'EXD'                            DOT
           MOVE        87 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'EXP'                            DOT
           MOVE        88 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'NON'                            DOT
           MOVE        99 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'DRI'                            DOT
           MOVE        10 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'DRQ'                            DOT
           MOVE        11 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'EAA'                            DOT
           MOVE        12 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'DDR'                            DOT
           MOVE        13 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'XPC'                            DOT
           MOVE        14 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'XPX'                            DOT
           MOVE        15 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'XPP'                            DOT
           MOVE        16 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'LND'                            DOT
           MOVE        17 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'SMP'                            DOT
           MOVE        18 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'LDP'                            DOT
           MOVE        19 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'RTH'                            DOT
           MOVE        21 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'RPR'                            DOT
           MOVE        22 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'RPX'                            DOT
           MOVE        23 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'RDI'                            DOT
           MOVE        24 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'RDH'                            DOT
           MOVE        25 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'RPO'                            DOT
           MOVE        26 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'RXC'                            DOT
           MOVE        27 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'RXP'                            DOT
           MOVE        28 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'CON'                            DOT
           MOVE        32 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'CPR'                            DOT
           MOVE        33 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'CPX'                            DOT
           MOVE        34 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'CDI'                            DOT
           MOVE        35 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'CDH'                            DOT
           MOVE        36 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'CPO'                            DOT
           MOVE        37 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'CXC'                            DOT
           MOVE        38 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'CXP'                            DOT
           MOVE        39 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'EIR'                            DOT
           MOVE        43 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'EDI'                            DOT
           MOVE        44 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'EDH'                            DOT
           MOVE        45 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'EPO'                            DOT
           MOVE        46 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'EEX'                            DOT
           MOVE        47 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'EEP'                            DOT
           MOVE        48 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'RCH'                            DOT
           MOVE        50 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'PCV'                            DOT
           MOVE        51 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'RCV'                            DOT
           MOVE        52 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'CRC'                            DOT
           MOVE        53 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'QXP'                            DOT
           MOVE        54 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'QXC'                            DOT
           MOVE        55 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'QRD'                            DOT
           MOVE        56 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'QDI'                            DOT
           MOVE        57 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'QDH'                            DOT
           MOVE        58 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'EFM'                            DOT
           MOVE        59 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'EFC'                            DOT
           MOVE        60 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = 'EQT'                            DOT
           MOVE        61 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUSA = SPACES                           DOT
           MOVE        00 TO 7-SACV-CLCUS                               ASETCV
               GO TO     F95SA-FN.                                      ASETCV
       F95SB-FN. EXIT.
      *N95SC.    NOTE *CONVERT NUMERIC TO ALPHA,          *.            ASETCV
       F95SC.    IF    7-SACV-CLCUS > LOW-VALUES                        lv15
                 AND   7-SACV-CLCUS NUMERIC                             ASETCV
                 AND   7-SACV-CLCUSA = LOW-VALUES                       ASETCV
                 NEXT SENTENCE ELSE GO TO     F95SC-FN.                 ASETCV
      *CHECKING YEAR WHEN NECESSARY                                     ASETCV
                 IF    7-SACV-CLCUS = 01                                DOT
           MOVE        'PRE' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 02                                DOT
                 AND   7-SACV-YEAR NUMERIC                              ASETCV
                 AND   (7-SACV-YEAR > SPACES AND                        ASETCV
                       7-SACV-YEAR < 1990)                              ASETCV
           MOVE        'ROL' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 02                                DOT
           MOVE        'PRX' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 03                                DOT
           MOVE        'DIS' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 04                                DOT
           MOVE        'DTH' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 05                                DOT
           MOVE        'PRO' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 06                                DOT
                 AND   7-SACV-YEAR NUMERIC                              ASETCV
                 AND   (7-SACV-YEAR > SPACES AND                        ASETCV
                       7-SACV-YEAR < 1990)                              ASETCV
           MOVE        'OTH' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 06                                DOT
           MOVE        'XCH' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 07                                DOT
           MOVE        'RET' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 08                                DOT
           MOVE        'EXC' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 09                                DOT
           MOVE        'PSC' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 87                                DOT
           MOVE        'EXD' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 88                                DOT
           MOVE        'EXP' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 99                                DOT
           MOVE        'NON' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 10                                DOT
           MOVE        'DRI' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 11                                DOT
           MOVE        'DRQ' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 12                                DOT
           MOVE        'EAA' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 13                                DOT
           MOVE        'DDR' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 14                                DOT
           MOVE        'XPC' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 15                                DOT
           MOVE        'XPX' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 16                                DOT
           MOVE        'XPP' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 17                                DOT
           MOVE        'LND' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 18                                DOT
           MOVE        'SMP' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 19                                DOT
           MOVE        'LDP' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 21                                DOT
           MOVE        'RTH' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 22                                DOT
           MOVE        'RPR' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 23                                DOT
           MOVE        'RPX' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 24                                DOT
           MOVE        'RDI' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 25                                DOT
           MOVE        'RDH' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 26                                DOT
           MOVE        'RPO' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 27                                DOT
           MOVE        'RXC' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 28                                DOT
           MOVE        'RXP' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 32                                DOT
           MOVE        'CON' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 33                                DOT
           MOVE        'CPR' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 34                                DOT
           MOVE        'CPX' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 35                                DOT
           MOVE        'CDI' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 36                                DOT
           MOVE        'CDH' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 37                                DOT
           MOVE        'CPO' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 38                                DOT
           MOVE        'CXC' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 39                                DOT
           MOVE        'CXP' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 43                                DOT
           MOVE        'EIR' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 44                                DOT
           MOVE        'EDI' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 45                                DOT
           MOVE        'EDH' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 46                                DOT
           MOVE        'EPO' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 47                                DOT
           MOVE        'EEX' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 48                                DOT
           MOVE        'EEP' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 50                                DOT
           MOVE        'RCH' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 51                                DOT
           MOVE        'PCV' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 52                                DOT
           MOVE        'RCV' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 53                                DOT
           MOVE        'CRC' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 54                                DOT
           MOVE        'QXP' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 55                                DOT
           MOVE        'QXC' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 56                                DOT
           MOVE        'QRD' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 57                                DOT
           MOVE        'QDI' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 58                                DOT
           MOVE        'QDH' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 59                                DOT
           MOVE        'EFM' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 60                                DOT
           MOVE        'EFC' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 61                                DOT
           MOVE        'EQT' TO 7-SACV-CLCUSA                           ASETCV
               GO TO     F95SA-FN.                                      ASETCV
                 IF    7-SACV-CLCUS = 00                                DOT
           MOVE        SPACES TO 7-SACV-CLCUSA.                         ASETCV
       F95SC-FN. EXIT.
       F95SA-FN. EXIT.
       F95-FN.   EXIT.
      *N96.      NOTE *************************************.
      *               *                                   *
      *               *COMMON PERFORMED ROUTINES          *
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
           MOVE        'CT01' TO DH10-MAUSB                             ADU165
           MOVE        PJ15-CTID TO DH10-NAUSK                          ADU165
           MOVE        'COLA' TO DH10-CSYS                              ADU165
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
      *N96OA.    NOTE *MACRO AAOAG3  -  CALC CLIENT AGE   *.            AAOAG3
       F96OA.                                                           lv10
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
               GO TO     F96OA-FN.                                      AAOAG3
                 IF    7-OAGE-BIRTH-DATE >                              DOT
                       7-OAGE-CURRENT-DATE                              AAOAG3
      *RETURN CLIENT AGE OF ZERO IF                                     AAOAG3
      *BIRTH DATE > CURRENT DATE                                        AAOAG3
               GO TO     F96OA-FN.                                      AAOAG3
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
       F96OA-FN. EXIT.
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
