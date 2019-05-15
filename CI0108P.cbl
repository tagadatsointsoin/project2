       IDENTIFICATION DIVISION.                                         CI0108
       PROGRAM-ID.  CI0108P.                                            CI0108
      *AUTHOR.         CSTD/TERM FEE DETERMINATION.                     CI0108
      *DATE-COMPILED.   09/08/14.                                       CI0108
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
       ENVIRONMENT DIVISION.                                            CI0108
       CONFIGURATION SECTION.                                           CI0108
       SOURCE-COMPUTER. IBM-370.                                        CI0108
       OBJECT-COMPUTER. IBM-370.                                        CI0108
       DATA DIVISION.                                                   CI0108
       WORKING-STORAGE SECTION.                                         CI0108
       01               7-CTA09-1-SSA.                                  AAADBL
         05             FILLER          PIC X(08)   VALUE 'CT09'.       AAADBL
         05             FILLER          PIC X(01)   VALUE '*'.          AAADBL
         05             7-CTA09-1-CCOD PIC X(05)  VALUE '-----'.        AAADBL
         05             FILLER          PIC X(01)   VALUE '('.          AAADBL
         05             FILLER          PIC X(08)   VALUE 'CT09K'.
         05             FILLER          PIC X(02)   VALUE ' ='.
         05             FILLER          PIC 9(03)   VALUE 004.
         05             FILLER          PIC X(01)   VALUE '&'.
         05             FILLER          PIC X(08)   VALUE 'GERED'.
         05             FILLER          PIC X(02)   VALUE ' ='.
         05             FILLER          PIC 9(08)   VALUE ZEROS.
         05             FILLER          PIC X(01)   VALUE ')'.          AAADBL
       01               7-CTA10-1-SSA.                                  AAADBL
         05             FILLER          PIC X(08)   VALUE 'CT10'.       AAADBL
         05             FILLER          PIC X(01)   VALUE '*'.          AAADBL
         05             7-CTA10-1-CCOD PIC X(05)  VALUE '-----'.        AAADBL
         05             FILLER          PIC X(01)   VALUE '('.          AAADBL
         05             FILLER          PIC X(08)   VALUE 'GERED'.
         05             FILLER          PIC X(02)   VALUE ' ='.
         05             FILLER          PIC 9(08)   VALUE ZEROS.
         05             FILLER          PIC X(01)   VALUE ')'.          AAADBL
       01               7-GRA19-1-SSA.                                  AAADBL
         05             FILLER          PIC X(08)   VALUE 'GR19'.       AAADBL
         05             FILLER          PIC X(01)   VALUE '*'.          AAADBL
         05             7-GRA19-1-CCOD PIC X(05)  VALUE '-----'.        AAADBL
         05             FILLER          PIC X(01)   VALUE '('.          AAADBL
         05             FILLER          PIC X(08)   VALUE 'GERED'.
         05             FILLER          PIC X(02)   VALUE ' ='.
         05             FILLER          PIC 9(08)   VALUE ZEROS.
         05             FILLER          PIC X(01)   VALUE ')'.          AAADBL
      *    WORK AREAS FOR AADA71                                        AADA71
       01  7-DA70-XDAT8            PIC  X(08).                          AADA71
       01  7-DA70-XDAT8R  REDEFINES 7-DA70-XDAT8 PIC 9(08).             AADA71
       01  7-DA70-BASE             PIC S9(03)    COMP-3.                AADA71
       01  7-DA70-XDAJC            PIC S9(07).                          AADA71
       01  FILLER   REDEFINES   7-DA70-XDAJC.                           AADA71
           05  7-DA70-XDAJY        PIC  9(04).                          AADA71
           05  7-DA70-XDAJN        PIC  9(03).                          AADA71
       01  7-DA70-X93V2            PIC  9(03)V99.                       AADA71
       01  FILLER   REDEFINES   7-DA70-X93V2.                           AADA71
           05  7-DA70-X93          PIC  9(03).                          AADA71
           05  7-DA70-X9V2         PIC  9(02).                          AADA71
       01  7-DA70-XDAJN-SIGNED     PIC  S9(03).                         AADA71
       01  7-DA70-XDAY3            PIC  X(03).                          AADA71
       01  7-DA70-XDAY3R REDEFINES 7-DA70-XDAY3 PIC 9(03).              AADA71
       01  7-DA70-XDAJY-WORK       PIC  9(04).                          AADA71
       01  FILLER REDEFINES 7-DA70-XDAJY-WORK.                          AADA71
           05  7-DA70-XDAJC9       PIC  9(02).                          AADA71
           05  7-DA70-XDAJY9       PIC  9(02).                          AADA71

      *88 LEVELS
       01               7-LEVEL-88S.

      *    GR19-CTIDA (OTHER ACCTS IN SAME GROUP AS SOURCE ACCT)
      *!WI
           05  7-GR19-CTIDA
                        PICTURE 9(3).                                   CI0108
               88  CERT                     VALUE 001.
               88  FUND                     VALUE 002.
               88  SECURITIES               VALUE 021, 133.

       01                 CL01.                                         CI0108
            10            CL01-CL01K.                                   CI0108
            11            CL01-C199.                                    CI0108
            12            CL01-CLID.                                    CI0108
            13            CL01-CLIDO  PICTURE  9(3).                    CI0108
            13            CL01-CLIDN.                                   CI0108
            14            CL01-CLIDNP PICTURE  X(12).                   CI0108
            14            CL01-CLIDND PICTURE  9(8).                    CI0108
            10            CL01-GECKD  PICTURE  9.                       CI0108
            10            CL01-GEMDA  PICTURE  9(8).                    CI0108
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0108
                          BINARY.                                       CI0108
            10            CL01-GECUC  PICTURE  99.                      CI0108
            10            CL01-CLDOR  PICTURE  9(8).                    CI0108
            10            CL01-CLLNG  PICTURE  XX.                      CI0108
            10            CL01-GESLC  PICTURE  99.                      CI0108
            10            CL01-CLTYP  PICTURE  X.                       CI0108
            10            CL01-CLCLS  PICTURE  9(3).                    CI0108
            10            CL01-CLTWRC PICTURE  99.                      CI0108
            10            CL01-CLPVC  PICTURE  99.                      CI0108
            10            CL01-CLIND  PICTURE  9(3).                    CI0108
            10            CL01-CLTRC  PICTURE  99.                      CI0108
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            CL01-AYSIDA PICTURE  9(3).                    CI0108
            10            CL01-AYSID  PICTURE  9(5).                    CI0108
            10            CL01-CLSTR  PICTURE  9(2).                    CI0108
            10            CL01-CLC11  PICTURE  X.                       CI0108
            10            CL01-CLTIN  PICTURE  9(12).                   CI0108
            10            CL01-CLTND  PICTURE  9(8).                    CI0108
            10            CL01-CLTINC PICTURE  9.                       CI0108
            10            CL01-CCDWA  PICTURE  9.                       CI0108
            10            CL01-CICES  PICTURE  X.                       CI0108
            10            CL01-CLTRA  PICTURE  9(2).                    CI0108
            10            CL01-DIRSY  PICTURE  9(4)                     CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            CL01-CFEDS  PICTURE  X.                       CI0108
            10            CL01-FILLER PICTURE  X(06).                   CI0108
       01                 CL03.                                         CI0108
            10            CL03-GEDLA  PICTURE  9(8).                    CI0108
            10            CL03-DDREP  PICTURE  9(8).                    CI0108
            10            CL03-DPRFR  PICTURE  9(8).                    CI0108
            10            CL03-IACCI  PICTURE  X.                       CI0108
            10            CL03-CLDOB  PICTURE  9(8).                    CI0108
            10            CL03-CLDOD  PICTURE  9(8).                    CI0108
            10            CL03-CLDTH  PICTURE  X.                       CI0108
            10            CL03-CCINI  PICTURE  X.                       CI0108
            10            CL03-FILLER PICTURE  X(1).                    CI0108
            10            CL03-CLAIN  PICTURE  S9(11)                   CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            CL03-CCAOD  PICTURE  999.                     CI0108
            10            CL03-CLMAR  PICTURE  X.                       CI0108
            10            CL03-C198.                                    CI0108
            11            CL03-CLNAM.                                   CI0108
            12            CL03-CLNAMH PICTURE  X(6).                    CI0108
            12            CL03-CLNAMF PICTURE  X(20).                   CI0108
            12            CL03-CLNAMM.                                  CI0108
            13            CL03-CLNAMI PICTURE  X.                       CI0108
            13            CL03-CLNAMR PICTURE  X(14).                   CI0108
            12            CL03-CLNAML PICTURE  X(25).                   CI0108
            12            CL03-CLNAMS PICTURE  X(4).                    CI0108
            10            CL03-FILLER PICTURE  X(10).                   CI0108
            10            CL03-MPRFS  PICTURE  X(4).                    CI0108
            10            CL03-CLOCC  PICTURE  9(3).                    CI0108
            10            CL03-CLRET  PICTURE  X.                       CI0108
            10            CL03-IOCOB  PICTURE  X.                       CI0108
            10            CL03-CLSEX  PICTURE  X.                       CI0108
            10            CL03-CLWIL  PICTURE  X.                       CI0108
            10            CL03-GECFC  PICTURE  99.                      CI0108
            10            CL03-GECFY  PICTURE  9(4).                    CI0108
            10            CL03-ICUSC  PICTURE  X.                       CI0108
            10            CL03-MCTYC  PICTURE  X(20).                   CI0108
            10            CL03-CLWIP  PICTURE  X.                       CI0108
            10            CL03-CLCTXF PICTURE  99.                      CI0108
            10            CL03-CLCUS  PICTURE  99.                      CI0108
            10            CL03-NPDLU  PICTURE  9(5).                    CI0108
            10            CL03-CLEMI  PICTURE  X.                       CI0108
            10            CL03-GEPHNH PICTURE  X(14).                   CI0108
            10            CL03-GEPHNB PICTURE  X(14).                   CI0108
            10            CL03-GEPHNX PICTURE  9(4).                    CI0108
            10            CL03-GEPHNA PICTURE  X(14).                   CI0108
            10            CL03-FILLER PICTURE  X(3).                    CI0108
            10            CL03-IAPRT  PICTURE  X.                       CI0108
            10            CL03-CEMSC  PICTURE  X.                       CI0108
            10            CL03-CSEPS  PICTURE  X.                       CI0108
            10            CL03-CRACE  PICTURE  X.                       CI0108
            10            CL03-CNIRA  PICTURE  X.                       CI0108
            10            CL03-FILLER PICTURE  X(11).                   CI0108
      * THE FOLLOWING MODULE IS THE DU MESSAGES MODULE (DAR LIBRARY)
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0100           PIC X(8) VALUE 'CI0100P '.                  AM0100
       01                 CT01.                                         CI0108
            10            CT01-CT01K.                                   CI0108
            11            CT01-C299.                                    CI0108
            12            CT01-CTID.                                    CI0108
            13            CT01-CTIDA  PICTURE  9(3).                    CI0108
            13            CT01-CTIDN.                                   CI0108
            14            CT01-CTIDNP PICTURE  X(13).                   CI0108
            14            CT01-CTIDND PICTURE  9(11).                   CI0108
            10            CT01-GECKD  PICTURE  9.                       CI0108
            10            CT01-GEMDA  PICTURE  9(8).                    CI0108
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0108
                          BINARY.                                       CI0108
            10            CT01-GECUC  PICTURE  99.                      CI0108
            10            CT01-CTAUL  PICTURE  9(3).                    CI0108
            10            CT01-DIRAC  PICTURE  9(4).                    CI0108
            10            CT01-CTCCI  PICTURE  X.                       CI0108
            10            CT01-CTCUS  PICTURE  999.                     CI0108
            10            CT01-CTEFD  PICTURE  9(8).                    CI0108
            10            CT01-CTIAD  PICTURE  9(8).                    CI0108
            10            CT01-CLCUS  PICTURE  99.                      CI0108
            10            CT01-CAMMB  PICTURE  X(3).                    CI0108
            10            CT01-CKPMM  PICTURE  X.                       CI0108
            10            CT01-CTLAD  PICTURE  9(8).                    CI0108
            10            CT01-IPERS  PICTURE  X.                       CI0108
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            CT01-CTLAT  PICTURE  9(8).                    CI0108
            10            CT01-CTLATC PICTURE  9(6).                    CI0108
            10            CT01-IMEGA  PICTURE  X.                       CI0108
            10            CT01-DIRAB  PICTURE  9(8).                    CI0108
            10            CT01-COLRQ  PICTURE  X.                       CI0108
            10            CT01-ZDA04  PICTURE  X(4).                    CI0108
            10            CT01-CTLPD  PICTURE  9(8).                    CI0108
            10            CT01-CIRASP PICTURE  9.                       CI0108
            10            CT01-CIRATP PICTURE  99.                      CI0108
            10            CT01-DRTHC  PICTURE  9(8).                    CI0108
            10            CT01-CPPTC  PICTURE  X.                       CI0108
            10            CT01-ZDA06  PICTURE  X(6).                    CI0108
            10            CT01-CTACD  PICTURE  9(8).                    CI0108
            10            CT01-CTNLI  PICTURE  X.                       CI0108
            10            CT01-CTRHO  PICTURE  9(8).                    CI0108
            10            CT01-CTSGD  PICTURE  9(8).                    CI0108
            10            CT01-CPATP  PICTURE  X(1).                    CI0108
            10            CT01-IRSTA  PICTURE  X.                       CI0108
            10            CT01-CTSTA  PICTURE  99.                      CI0108
            10            CT01-CTSSC  PICTURE  99.                      CI0108
            10            CT01-PRLIN  PICTURE  9(3).                    CI0108
            10            CT01-PRCOD  PICTURE  9(5).                    CI0108
            10            CT01-PRSCD  PICTURE  X(9).                    CI0108
            10            CT01-CTLNI  PICTURE  X.                       CI0108
            10            CT01-AYSIDA PICTURE  9(3).                    CI0108
            10            CT01-AYSID  PICTURE  9(5).                    CI0108
            10            CT01-CTBMC  PICTURE  99.                      CI0108
            10            CT01-CINAR  PICTURE  99.                      CI0108
            10            CT01-CPHTR  PICTURE  X.                       CI0108
            10            CT01-CDSTR  PICTURE  XX.                      CI0108
            10            CT01-CQACT  PICTURE  999.                     CI0108
            10            CT01-CIRAS  PICTURE  999.                     CI0108
            10            CT01-CIRAT  PICTURE  999.                     CI0108
            10            CT01-CLRAY  PICTURE  9(5).                    CI0108
            10            CT01-CATTP  PICTURE  X.                       CI0108
       01                 CT07.                                         CI0108
            10            CT07-CT07K.                                   CI0108
            11            CT07-C199.                                    CI0108
            12            CT07-CLID.                                    CI0108
            13            CT07-CLIDO  PICTURE  9(3).                    CI0108
            13            CT07-CLIDN.                                   CI0108
            14            CT07-CLIDNP PICTURE  X(12).                   CI0108
            14            CT07-CLIDND PICTURE  9(8).                    CI0108
       01                 CT09.                                         CI0108
            10            CT09-A100.                                    CI0108
            11            CT09-GELL   PICTURE  9(4)                     CI0108
                          BINARY.                                       CI0108
            11            CT09-CT09K.                                   CI0108
            12            CT09-CLCTRC PICTURE  9(3).                    CI0108
            11            CT09-GERSD  PICTURE  9(8).                    CI0108
            11            CT09-GERED  PICTURE  9(8).                    CI0108
            10            CT09-A199.                                    CI0108
            11            CT09-FILLER PICTURE  X(20).                   CI0108
            10            CT09-A101                                     CI0108
                          REDEFINES            CT09-A199.               CI0108
            11            CT09-GECSQ  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            CT09-CTAXR  PICTURE  X.                       CI0108
            11            CT09-GETAI  PICTURE  X.                       CI0108
            11            CT09-CTLACD PICTURE  9(8).                    CI0108
            11            CT09-GEPCS  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            CT09-A102                                     CI0108
                          REDEFINES            CT09-A199.               CI0108
            11            CT09-CLPID  PICTURE  9(9).                    CI0108
       01                 CT10.                                         CI0108
            10            CT10-CT10K.                                   CI0108
            11            CT10-GR98.                                    CI0108
            12            CT10-GRID.                                    CI0108
            13            CT10-GRIDC  PICTURE  9(3).                    CI0108
            13            CT10-GRIDN.                                   CI0108
            14            CT10-GRIDNP PICTURE  99.                      CI0108
            14            CT10-GRIDND PICTURE  9(8).                    CI0108
            10            CT10-GR97                                     CI0108
                          REDEFINES            CT10-CT10K.              CI0108
            11            CT10-GRIDCB PICTURE  9(3).                    CI0108
            11            CT10-FILLER PICTURE  X(10).                   CI0108
            10            CT10-GERSD  PICTURE  9(8).                    CI0108
            10            CT10-GERED  PICTURE  9(8).                    CI0108
            10            CT10-GRCSI  PICTURE  X.                       CI0108

      ******************************************************************
      *STANDARD DATE WORK SEGMENT
      ******************************************************************
      *!WF DSP=DD DSL=DD SEL=01 FOR=I LEV=1 PLT=DD
       01                 DD00.                                         CI0108
          05              DD00-SUITE.                                   CI0108
            15       FILLER         PICTURE  X(00093).                  CI0108
       01                 DD01  REDEFINES      DD00.                    CI0108
            10            DD01-XDAT8.                                   CI0108
            11            DD01-XDATC  PICTURE  XX.                      CI0108
            11            DD01-XDATY  PICTURE  XX.                      CI0108
            11            DD01-XDATM  PICTURE  XX.                      CI0108
            11            DD01-XDATD  PICTURE  XX.                      CI0108
            10            DD01-XDAT8D                                   CI0108
                          REDEFINES            DD01-XDAT8               CI0108
               PICTURE    9(8).                                         CI0108
            10            DD01-XDAT81.                                  CI0108
            11            DD01-XDATM1 PICTURE  XX.                      CI0108
            11            DD01-XDATD1 PICTURE  XX.                      CI0108
            11            DD01-XDATC1 PICTURE  XX.                      CI0108
            11            DD01-XDATY1 PICTURE  XX.                      CI0108
            10            DD01-XDAT80                                   CI0108
                          REDEFINES            DD01-XDAT81              CI0108
               PICTURE    9(8).                                         CI0108
            10            DD01-XDAT62.                                  CI0108
            11            DD01-XDATM2 PICTURE  XX.                      CI0108
            11            DD01-XDATD2 PICTURE  XX.                      CI0108
            11            DD01-XDATY2 PICTURE  XX.                      CI0108
            10            DD01-XDAT69                                   CI0108
                          REDEFINES            DD01-XDAT62              CI0108
               PICTURE    9(6).                                         CI0108
            10            DD01-XDATCU.                                  CI0108
            11            DD01-XDATC9 PICTURE  99.                      CI0108
            11            DD01-XDAYMD.                                  CI0108
            12            DD01-XDATY9 PICTURE  99.                      CI0108
            12            DD01-XDAMD.                                   CI0108
            13            DD01-XDATM9 PICTURE  99.                      CI0108
            13            DD01-XDATD9 PICTURE  99.                      CI0108
            10            DD01-XDAT89 PICTURE  9(8).                    CI0108
            10            DD01-XDAJC  PICTURE  9(7).                    CI0108
            10            DD01-XDAJC1.                                  CI0108
            11            DD01-XDAJC9 PICTURE  99.                      CI0108
            11            DD01-XDAJY  PICTURE  99.                      CI0108
            11            DD01-XDAJN  PICTURE  999.                     CI0108
            10            DD01-XDAB   PICTURE  9(5).                    CI0108
            10            DD01-DD05.                                    CI0108
            11            DD01-XDACT  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            DD01-XDACV  PICTURE  S9                       CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            DD01-XDAGP  PICTURE  S9(9)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            DD01-XDAJP  PICTURE  S9(7)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            DD01-XDACV1 PICTURE  S9                       CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            DD01-XDAGP1 PICTURE  S9(9)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            DD01-XDAJP1 PICTURE  S9(7)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            DD01-XW03.                                    CI0108
            11            DD01-XDATG.                                   CI0108
            12            DD01-XDAT1.                                   CI0108
            13            DD01-XDAT19 PICTURE  99.                      CI0108
            12            DD01-XDAT2.                                   CI0108
            13            DD01-XDAT29 PICTURE  99.                      CI0108
            12            DD01-XDAT3.                                   CI0108
            13            DD01-XDAT39 PICTURE  99.                      CI0108
            12            DD01-XDAT4.                                   CI0108
            13            DD01-XDAT49 PICTURE  99.                      CI0108
            11            DD01-XLEAPY PICTURE  99.                      CI0108
            11            DD01-DTGCY  PICTURE  9(4).                    CI0108
            11            DD01-FILLER                                   CI0108
                          REDEFINES            DD01-DTGCY.              CI0108
            12            DD01-DTGCC  PICTURE  9(2).                    CI0108
            12            DD01-DTGYY  PICTURE  9(2).                    CI0108

      *RETURN CODE FROM DATE VALIDATION ROUTINE
       01  DEL-ER          PIC 9(01).

      ******************************************************************
      *  WORK AREA FOR MACRO AADA54
      *
      *!WF DSP=DG DSL=DD SEL=01 FOR=I DES=2 LEV=1 PLT=DG
       01                 DG01.                                         CI0108
            10            DG01-XDAT8.                                   CI0108
            11            DG01-XDATC  PICTURE  XX                       CI0108
                          VALUE                SPACE.                   CI0108
            11            DG01-XDATY  PICTURE  XX                       CI0108
                          VALUE                SPACE.                   CI0108
            11            DG01-XDATM  PICTURE  XX                       CI0108
                          VALUE                SPACE.                   CI0108
            11            DG01-XDATD  PICTURE  XX                       CI0108
                          VALUE                SPACE.                   CI0108
            10            DG01-XDAT8D                                   CI0108
                          REDEFINES            DG01-XDAT8               CI0108
               PICTURE    9(8).                                         CI0108
            10            DG01-XDAT81.                                  CI0108
            11            DG01-XDATM1 PICTURE  XX                       CI0108
                          VALUE                SPACE.                   CI0108
            11            DG01-XDATD1 PICTURE  XX                       CI0108
                          VALUE                SPACE.                   CI0108
            11            DG01-XDATC1 PICTURE  XX                       CI0108
                          VALUE                SPACE.                   CI0108
            11            DG01-XDATY1 PICTURE  XX                       CI0108
                          VALUE                SPACE.                   CI0108
            10            DG01-XDAT80                                   CI0108
                          REDEFINES            DG01-XDAT81              CI0108
               PICTURE    9(8).                                         CI0108
            10            DG01-XDAT62.                                  CI0108
            11            DG01-XDATM2 PICTURE  XX                       CI0108
                          VALUE                SPACE.                   CI0108
            11            DG01-XDATD2 PICTURE  XX                       CI0108
                          VALUE                SPACE.                   CI0108
            11            DG01-XDATY2 PICTURE  XX                       CI0108
                          VALUE                SPACE.                   CI0108
            10            DG01-XDAT69                                   CI0108
                          REDEFINES            DG01-XDAT62              CI0108
               PICTURE    9(6).                                         CI0108
            10            DG01-XDATCU.                                  CI0108
            11            DG01-XDATC9 PICTURE  99                       CI0108
                          VALUE                ZERO.                    CI0108
            11            DG01-XDAYMD.                                  CI0108
            12            DG01-XDATY9 PICTURE  99                       CI0108
                          VALUE                ZERO.                    CI0108
            12            DG01-XDAMD.                                   CI0108
            13            DG01-XDATM9 PICTURE  99                       CI0108
                          VALUE                ZERO.                    CI0108
            13            DG01-XDATD9 PICTURE  99                       CI0108
                          VALUE                ZERO.                    CI0108
            10            DG01-XDAT89 PICTURE  9(8)                     CI0108
                          VALUE                ZERO.                    CI0108
            10            DG01-XDAJC  PICTURE  9(7)                     CI0108
                          VALUE                ZERO.                    CI0108
            10            DG01-XDAJC1.                                  CI0108
            11            DG01-XDAJC9 PICTURE  99                       CI0108
                          VALUE                ZERO.                    CI0108
            11            DG01-XDAJY  PICTURE  99                       CI0108
                          VALUE                ZERO.                    CI0108
            11            DG01-XDAJN  PICTURE  999                      CI0108
                          VALUE                ZERO.                    CI0108
            10            DG01-XDAB   PICTURE  9(5)                     CI0108
                          VALUE                ZERO.                    CI0108
            10            DG01-DD05.                                    CI0108
            11            DG01-XDACT  PICTURE  S9(3)                    CI0108
                          VALUE                ZERO                     CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            DG01-XDACV  PICTURE  S9                       CI0108
                          VALUE                ZERO                     CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            DG01-XDAGP  PICTURE  S9(9)                    CI0108
                          VALUE                ZERO                     CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            DG01-XDAJP  PICTURE  S9(7)                    CI0108
                          VALUE                ZERO                     CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            DG01-XDACV1 PICTURE  S9                       CI0108
                          VALUE                ZERO                     CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            DG01-XDAGP1 PICTURE  S9(9)                    CI0108
                          VALUE                ZERO                     CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            DG01-XDAJP1 PICTURE  S9(7)                    CI0108
                          VALUE                ZERO                     CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            DG01-XW03.                                    CI0108
            11            DG01-XDATG.                                   CI0108
            12            DG01-XDAT1.                                   CI0108
            13            DG01-XDAT19 PICTURE  99                       CI0108
                          VALUE                ZERO.                    CI0108
            12            DG01-XDAT2.                                   CI0108
            13            DG01-XDAT29 PICTURE  99                       CI0108
                          VALUE                ZERO.                    CI0108
            12            DG01-XDAT3.                                   CI0108
            13            DG01-XDAT39 PICTURE  99                       CI0108
                          VALUE                ZERO.                    CI0108
            12            DG01-XDAT4.                                   CI0108
            13            DG01-XDAT49 PICTURE  99                       CI0108
                          VALUE                ZERO.                    CI0108
            11            DG01-XLEAPY PICTURE  99                       CI0108
                          VALUE                ZERO.                    CI0108
            11            DG01-DTGCY  PICTURE  9(4)                     CI0108
                          VALUE                ZERO.                    CI0108
            11            DG01-FILLER                                   CI0108
                          REDEFINES            DG01-DTGCY.              CI0108
            12            DG01-DTGCC  PICTURE  9(2).                    CI0108
            12            DG01-DTGYY  PICTURE  9(2).                    CI0108
      **
      ******************************************************************
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA58
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0108
            10            XW05-XW06.                                    CI0108
            11            XW05-XDBPCB.                                  CI0108
            12            XW05-XDBDNM PICTURE  X(08)                    CI0108
                          VALUE                SPACE.                   CI0108
            12            XW05-XSEGLV PICTURE  X(02)                    CI0108
                          VALUE                SPACE.                   CI0108
            12            XW05-XRC    PICTURE  X(02)                    CI0108
                          VALUE                SPACE.                   CI0108
            12            XW05-XPROPT PICTURE  X(04)                    CI0108
                          VALUE                SPACE.                   CI0108
            12            XW05-FILLER PICTURE  S9(5)                    CI0108
                          VALUE                ZERO                     CI0108
                          BINARY.                                       CI0108
            12            XW05-XSEGNM PICTURE  X(08)                    CI0108
                          VALUE                SPACE.                   CI0108
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0108
                          VALUE                ZERO                     CI0108
                          BINARY.                                       CI0108
            12            XW05-XSEGNB PICTURE  9(05)                    CI0108
                          VALUE                ZERO                     CI0108
                          BINARY.                                       CI0108
            12            XW05-XCOKEY PICTURE  X(70)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            XW05-XW07.                                    CI0108
            11            XW05-XIOPCB.                                  CI0108
            12            XW05-XTERMI PICTURE  X(08)                    CI0108
                          VALUE                SPACE.                   CI0108
            12            XW05-FILLER PICTURE  XX                       CI0108
                          VALUE                SPACE.                   CI0108
            12            XW05-XRC1   PICTURE  X(02)                    CI0108
                          VALUE                SPACE.                   CI0108
            12            XW05-FILLER PICTURE  X(12)                    CI0108
                          VALUE                SPACE.                   CI0108
            12            XW05-XMODNM PICTURE  X(8)                     CI0108
                          VALUE                SPACE.                   CI0108
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0108
                          VALUE                ZERO.                    CI0108
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0108
                          VALUE                ZERO.                    CI0108
            10            XW05-XGU    PICTURE  X(4)                     CI0108
                          VALUE                'GU  '.                  CI0108
            10            XW05-XGHU   PICTURE  X(4)                     CI0108
                          VALUE                'GHU '.                  CI0108
            10            XW05-XGN    PICTURE  X(4)                     CI0108
                          VALUE                'GN  '.                  CI0108
            10            XW05-XGHN   PICTURE  X(4)                     CI0108
                          VALUE                'GHN '.                  CI0108
            10            XW05-XGNP   PICTURE  X(4)                     CI0108
                          VALUE                'GNP '.                  CI0108
            10            XW05-XGHNP  PICTURE  X(4)                     CI0108
                          VALUE                'GHNP'.                  CI0108
            10            XW05-XREPL  PICTURE  XXXX                     CI0108
                          VALUE                'REPL'.                  CI0108
            10            XW05-XISRT  PICTURE  X(4)                     CI0108
                          VALUE                'ISRT'.                  CI0108
            10            XW05-XDLET  PICTURE  X(4)                     CI0108
                          VALUE                'DLET'.                  CI0108
            10            XW05-XOPEN  PICTURE  X(4)                     CI0108
                          VALUE                'OPEN'.                  CI0108
            10            XW05-XCLSE  PICTURE  X(4)                     CI0108
                          VALUE                'CLSE'.                  CI0108
            10            XW05-XCHKP  PICTURE  X(4)                     CI0108
                          VALUE                'CHKP'.                  CI0108
            10            XW05-XXRST  PICTURE  X(4)                     CI0108
                          VALUE                'XRST'.                  CI0108
            10            XW05-XTERM  PICTURE  X(4)                     CI0108
                          VALUE                'TERM'.                  CI0108
            10            XW05-XNFPAC PICTURE  X(13)                    CI0108
                          VALUE                SPACE.                   CI0108
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0108
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0108
                                                                        AM0100
      ******************************************************************AM0100
      **     FIELDS USED IN THE PARAMETER LIST OF CI0100.  THESE WILL  *AM0100
      **     BE VALUED AND PASSED IN THE CALLING PROGRAM.              *AM0100
      ******************************************************************AM0100
                                                                        AM0100
       01  7-GC00-AREA.                                                 AM0100
      *!WI pl=GC140                                                     AM0100
           05  7-GC00-MAPPN                                             AM0100
                        PICTURE X(10)                                   CI0108
                                   VALUE SPACES.                        AM0100
      *!WI pl=GC180                                                     AM0100
           05  7-GC00-CFUNC                                             AM0100
                        PICTURE X(3)                                    CI0108
                                   VALUE SPACES.                        AM0100
      *!WI pl=GC220                                                     AM0100
           05  7-GC00-CASTC OCCURS 6                                    AM0100
                        PICTURE 99.                                     CI0108
      *!WI pl=GC240                                                     AM0100
           05  7-GC00-CAATY OCCURS 3                                    AM0100
                        PICTURE 9(3).                                   CI0108
           05  7-GC00-C299.                                             AM0100
      *!WI pl=GC280                                                     AM0100
               10  7-GC00-CTID                                          AM0100
                        PICTURE X(27)                                   CI0108
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC320                                                     AM0100
           05  7-GC00-DCACG9                                            AM0100
                        PICTURE 9(8)                                    CI0108
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC360                                                     AM0100
           05  7-GC00-NAASQ                                             AM0100
                        PICTURE S9(3)                                   CI0108
                          COMPUTATIONAL-3                               CI0108
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC400                                                     AM0100
           05  7-GC00-NPISQ                                             AM0100
                        PICTURE S9(3)                                   CI0108
                          COMPUTATIONAL-3                               CI0108
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC440                                                     AM0100
           05  7-GC00-CIRAP                                             AM0100
                        PICTURE XX                                      CI0108
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC480                                                     AM0100
           05  7-GC00-IPERT                                             AM0100
                        PICTURE X                                       CI0108
                                   VALUE SPACES.                        AM0100
      *!WI pl=GC520                                                     AM0100
           05  7-GC00-NEIBT                                             AM0100
                        PICTURE X(7)                                    CI0108
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC560                                                     AM0100
           05  7-GC00-GESQ2C                                            AM0100
                        PICTURE S99                                     CI0108
                          COMPUTATIONAL-3                               CI0108
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC600                                                     AM0100
           05  7-GC00-MIPPS                                             AM0100
                        PICTURE X(4)                                    CI0108
                                   VALUE SPACES.                        AM0100
      *!WI pl=GC640                                                     AM0100
           05  7-GC00-IENDP                                             AM0100
                        PICTURE X                                       CI0108
                                   VALUE SPACES.                        AM0100
                                                                        AM0100
      ******************************************************************AM0100
      **     PCB ADDRESS LIST FOR CI0100.  MODULE CI0100 WILL NEED     *AM0100
      **     PCB'S FOR:                                                *AM0100
      **             ARRANGEMENT DATABASE(ACAP)                        *AM0100
      ******************************************************************AM0100
                                                                        AM0100
       01  CI0100GC-PCB-ADDRESS-LIST.                                   AM0100
           05  CI0100GC-PCB-ACAP-PTR1      POINTER.                     AM0100

      *INPUT SEGMENT TO CI0100
      *!WF DSP=PK DSL=PJ SEL=40 FOR=I DES=1 LEV=1 PLT=GC
       01                 PK40.                                         CI0108
            10            PK40-MAPPN  PICTURE  X(10).                   CI0108
            10            PK40-CFUNC  PICTURE  X(3).                    CI0108
            10            PK40-CASTC  PICTURE  99                       CI0108
                          OCCURS       006     TIMES.                   CI0108
            10            PK40-CAATY  PICTURE  9(3)                     CI0108
                          OCCURS       003     TIMES.                   CI0108
            10            PK40-C299.                                    CI0108
            11            PK40-CTID.                                    CI0108
            12            PK40-CTIDA  PICTURE  9(3).                    CI0108
            12            PK40-CTIDN.                                   CI0108
            13            PK40-CTIDNP PICTURE  X(13).                   CI0108
            13            PK40-CTIDND PICTURE  9(11).                   CI0108
            10            PK40-DCACG9 PICTURE  9(8).                    CI0108
            10            PK40-NAASQ  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            PK40-NPISQ  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            PK40-CIRAP  PICTURE  XX.                      CI0108
            10            PK40-IPERT  PICTURE  X.                       CI0108
            10            PK40-NEIBT  PICTURE  X(7).                    CI0108
            10            PK40-GESQ2C PICTURE  S99                      CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            PK40-MIPPS  PICTURE  X(4).                    CI0108
            10            PK40-IENDP  PICTURE  X.                       CI0108
            10            PK40-FILLER PICTURE  X(20).                   CI0108
       01                 PK41.                                         CI0108
            10            PK41-IENDP  PICTURE  X.                       CI0108
            10            PK41-MIPPS  PICTURE  X(4).                    CI0108
            10            PK41-GC01.                                    CI0108
            11            PK41-GC01K.                                   CI0108
            12            PK41-C299.                                    CI0108
            13            PK41-CTID.                                    CI0108
            14            PK41-CTIDA  PICTURE  9(3).                    CI0108
            14            PK41-CTIDN.                                   CI0108
            15            PK41-CTIDNP PICTURE  X(13).                   CI0108
            15            PK41-CTIDND PICTURE  9(11).                   CI0108
            11            PK41-DCAG9L PICTURE  9(8).                    CI0108
            11            PK41-NAASQL PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            PK41-ICUST  PICTURE  X.                       CI0108
            11            PK41-NSEQ4B PICTURE  9(8)                     CI0108
                          BINARY.                                       CI0108
            11            PK41-PRCOD  PICTURE  9(5).                    CI0108
            11            PK41-PRSCD  PICTURE  X(9).                    CI0108
            11            PK41-FILLER PICTURE  X(8).                    CI0108
            10            PK41-IGC01  PICTURE  X(01).                   CI0108
            10            PK41-QDECT9 PICTURE  99.                      CI0108
            10            PK41-FILLER PICTURE  X(20).                   CI0108
            10            PK41-GAKEY                                    CI0108
                          OCCURS       010     TIMES.                   CI0108
            11            PK41-IGC03  PICTURE  X(01).                   CI0108
            11            PK41-IGC04  PICTURE  X(01).                   CI0108
            11            PK41-IGC06  PICTURE  X(01).                   CI0108
            11            PK41-IGC12  PICTURE  X(01).                   CI0108
            11            PK41-IGC21  PICTURE  X(01).                   CI0108
            11            PK41-GC03.                                    CI0108
            12            PK41-GELL   PICTURE  9(4)                     CI0108
                          BINARY.                                       CI0108
            12            PK41-GD00.                                    CI0108
            13            PK41-GC03K.                                   CI0108
            14            PK41-DCACG9 PICTURE  9(8).                    CI0108
            14            PK41-NAASQ  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CAATY  PICTURE  9(3).                    CI0108
            13            PK41-CVSYS  PICTURE  X(2).                    CI0108
            13            PK41-CACTO  PICTURE  9(3).                    CI0108
            13            PK41-CATRN.                                   CI0108
            14            PK41-CATRF  PICTURE  9(3).                    CI0108
            14            PK41-CATRS  PICTURE  9(3).                    CI0108
            13            PK41-CASTC  PICTURE  99.                      CI0108
            13            PK41-IPULL  PICTURE  X.                       CI0108
            13            PK41-GEAUN  PICTURE  9(5).                    CI0108
            13            PK41-GEOPD2 PICTURE  X(8).                    CI0108
            13            PK41-NBTCH  PICTURE  9(4).                    CI0108
            13            PK41-DEFFT  PICTURE  9(8).                    CI0108
            13            PK41-NSUNT  PICTURE  9(4).                    CI0108
            13            PK41-ITRAN  PICTURE  X.                       CI0108
            13            PK41-DLAUP1 PICTURE  9(8).                    CI0108
            13            PK41-ADRET  PICTURE  S9(11)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-TTRMS  PICTURE  X(12).                   CI0108
            13            PK41-IDELT  PICTURE  X.                       CI0108
            13            PK41-GEOPDM PICTURE  X(8).                    CI0108
            13            PK41-FILLER PICTURE  X(07).                   CI0108
            12            PK41-GD09.                                    CI0108
            13            PK41-FILLER PICTURE  X(70).                   CI0108
            12            PK41-GD01                                     CI0108
                          REDEFINES            PK41-GD09.               CI0108
            13            PK41-ADBRQ  PICTURE  S9(11)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CTRTP  PICTURE  X(2).                    CI0108
            13            PK41-CPORT  PICTURE  X.                       CI0108
            13            PK41-CSCRNU PICTURE  X(4).                    CI0108
            13            PK41-DLAUP  PICTURE  9(8).                    CI0108
            13            PK41-CTWHAT PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-PWHLD  PICTURE  S999V9(5)                CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-IWTHH  PICTURE  X.                       CI0108
            13            PK41-NDRFT  PICTURE  9(5).                    CI0108
            13            PK41-IDPAP  PICTURE  X.                       CI0108
            13            PK41-GETIM  PICTURE  S9(7)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-QNACT  PICTURE  9(3).                    CI0108
            13            PK41-AEDRQ  PICTURE  S9(11)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-IPLIN  PICTURE  X.                       CI0108
            13            PK41-CLIDNB PICTURE  9(8).                    CI0108
            13            PK41-CSLCT  PICTURE  X.                       CI0108
            13            PK41-ITELE  PICTURE  X.                       CI0108
            13            PK41-FILLER PICTURE  X(06).                   CI0108
            12            PK41-GD02                                     CI0108
                          REDEFINES            PK41-GD09.               CI0108
            13            PK41-CSYST  PICTURE  99.                      CI0108
            13            PK41-FILLER PICTURE  X.                       CI0108
            13            PK41-ACASH  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-DTRAC  PICTURE  9(8).                    CI0108
            13            PK41-CTRSO  PICTURE  9(02).                   CI0108
            13            PK41-NTRCE  PICTURE  9(06).                   CI0108
            13            PK41-GECKD1 PICTURE  9.                       CI0108
            13            PK41-CCOLL  PICTURE  X(3).                    CI0108
            13            PK41-CLTDP  PICTURE  X(3).                    CI0108
            13            PK41-PSLLD  PICTURE  S99V999                  CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-ISLOR  PICTURE  X.                       CI0108
            13            PK41-ITPAC  PICTURE  X.                       CI0108
            13            PK41-CPMTCA PICTURE  XXX.                     CI0108
            13            PK41-CSERV  PICTURE  X(3).                    CI0108
            13            PK41-ACOMO  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-IPLIN1 PICTURE  X.                       CI0108
            13            PK41-INQEX  PICTURE  X.                       CI0108
            13            PK41-CTKRAA PICTURE  X(12).                   CI0108
            13            PK41-CCSMQ  PICTURE  X.                       CI0108
            13            PK41-IVAEX1 PICTURE  X.                       CI0108
            13            PK41-IHPMT  PICTURE  X(1).                    CI0108
            13            PK41-GETIM3 PICTURE  S9(7)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            PK41-GD03                                     CI0108
                          REDEFINES            PK41-GD09.               CI0108
            13            PK41-CATRNC PICTURE  9(6).                    CI0108
            13            PK41-APRNT1 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-QSHOWT PICTURE  S9(10)V999               CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-ACINVT PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-ACOMO7 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-QSHOMW PICTURE  S9(10)V999               CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-ATAXT3 PICTURE  S9(11)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CTSTR  PICTURE  9(2).                    CI0108
            13            PK41-ICIRA  PICTURE  X.                       CI0108
            13            PK41-GETIM2 PICTURE  S9(7)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CPMTCX PICTURE  XX.                      CI0108
            13            PK41-FILLER PICTURE  X(16).                   CI0108
            12            PK41-GD99.                                    CI0108
            13            PK41-FILLER PICTURE  X(248).                  CI0108
            12            PK41-GD10                                     CI0108
                          REDEFINES            PK41-GD99.               CI0108
            13            PK41-MROTC  PICTURE  X(7).                    CI0108
            13            PK41-CEDSC  PICTURE  9(1).                    CI0108
            13            PK41-ILPOI  PICTURE  X(1).                    CI0108
            13            PK41-AWRCH  PICTURE  S9(3)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CHCOC1 PICTURE  9(2).                    CI0108
            13            PK41-CHCOC2 PICTURE  9(2).                    CI0108
            13            PK41-CHCOC3 PICTURE  9(2).                    CI0108
            13            PK41-CHCOC4 PICTURE  9(2).                    CI0108
            13            PK41-CMCOC1 PICTURE  9(3).                    CI0108
            13            PK41-CMCOC2 PICTURE  9(3).                    CI0108
            13            PK41-CMCOC3 PICTURE  9(3).                    CI0108
            13            PK41-GD11.                                    CI0108
            14            PK41-FILLER PICTURE  X(219).                  CI0108
            13            PK41-GD12                                     CI0108
                          REDEFINES            PK41-GD11.               CI0108
            14            PK41-CELLO  PICTURE  9(1).                    CI0108
            14            PK41-CECLO  PICTURE  9(1).                    CI0108
            14            PK41-AEXML  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-CEPI   PICTURE  X(1).                    CI0108
            14            PK41-CEXTY  PICTURE  X.                       CI0108
            14            PK41-CROPC  PICTURE  9(1).                    CI0108
            14            PK41-CPUTY  PICTURE  9(1).                    CI0108
            14            PK41-IMCII  PICTURE  X(1).                    CI0108
            14            PK41-GEMISC                                   CI0108
                          OCCURS       010     TIMES.                   CI0108
            15            PK41-AMGLA  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            15            PK41-CMGLC  PICTURE  9(1).                    CI0108
            15            PK41-NMGLN  PICTURE  9(4).                    CI0108
            14            PK41-ACTRN  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-IWRBK  PICTURE  X.                       CI0108
            14            PK41-IFEDX  PICTURE  X.                       CI0108
            14            PK41-ICNTR  PICTURE  X.                       CI0108
            14            PK41-IOCKH  PICTURE  X.                       CI0108
            14            PK41-ICRCK  PICTURE  X.                       CI0108
            14            PK41-NHMPN  PICTURE  S9(10)                   CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-ITELR1 PICTURE  X.                       CI0108
            13            PK41-GD13                                     CI0108
                          REDEFINES            PK41-GD11.               CI0108
            14            PK41-DREDO  PICTURE  9(8).                    CI0108
            14            PK41-CATRNR PICTURE  9(6).                    CI0108
            14            PK41-CEVN   PICTURE  9(9).                    CI0108
            14            PK41-ISUSP  PICTURE  X(1).                    CI0108
            13            PK41-GD15                                     CI0108
                          REDEFINES            PK41-GD11.               CI0108
            14            PK41-CPUTZ  PICTURE  9(1).                    CI0108
            14            PK41-CETLB  PICTURE  9(3).                    CI0108
            14            PK41-QTRMC  PICTURE  9(3).                    CI0108
            14            PK41-DEFFTE PICTURE  9(8).                    CI0108
            14            PK41-DEFFTF PICTURE  9(8).                    CI0108
            14            PK41-DEFFTG PICTURE  9(8).                    CI0108
            14            PK41-XZ1A   PICTURE  X.                       CI0108
            14            PK41-XZ1B   PICTURE  X.                       CI0108
            14            PK41-XZ1C   PICTURE  X.                       CI0108
            14            PK41-XZ1D   PICTURE  X.                       CI0108
            14            PK41-XZ1E   PICTURE  X.                       CI0108
            14            PK41-XZ1F   PICTURE  X.                       CI0108
            14            PK41-XZ1G   PICTURE  X.                       CI0108
            14            PK41-XZ1H   PICTURE  X.                       CI0108
            14            PK41-XZ1I   PICTURE  X.                       CI0108
            14            PK41-DEFFTH PICTURE  9(8).                    CI0108
            13            PK41-GD19                                     CI0108
                          REDEFINES            PK41-GD11.               CI0108
            14            PK41-GD11.                                    CI0108
            15            PK41-FILLER PICTURE  X(219).                  CI0108
            12            PK41-GD20                                     CI0108
                          REDEFINES            PK41-GD99.               CI0108
            13            PK41-ADDACT PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-ISIGV  PICTURE  X.                       CI0108
            13            PK41-IALLF  PICTURE  X.                       CI0108
            13            PK41-QSHOWQ PICTURE  S9(9)V999                CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CCDSCW PICTURE  9(2).                    CI0108
            13            PK41-IDWRL  PICTURE  X.                       CI0108
            13            PK41-ITELR  PICTURE  X.                       CI0108
            13            PK41-IABIN  PICTURE  X.                       CI0108
            13            PK41-PACT1  PICTURE  S999V999                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-IBFAF  PICTURE  X.                       CI0108
            13            PK41-IFRSA  PICTURE  X.                       CI0108
            13            PK41-ICRCAN PICTURE  X.                       CI0108
            13            PK41-ACACTV PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-AGFND  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-QCSHOW PICTURE  S9(9)V999                CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-QCSHIS PICTURE  S9(9)V999                CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-NDTRC  PICTURE  9(8).                    CI0108
            13            PK41-CAERU  PICTURE  X(4).                    CI0108
            13            PK41-IFDGO  PICTURE  X.                       CI0108
            13            PK41-PSLLD2 PICTURE  S99V999                  CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-ISLOR2 PICTURE  X.                       CI0108
            13            PK41-QSFIO  PICTURE  S9(10)V999               CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-QSFID  PICTURE  S9(10)V999               CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CGDIN  PICTURE  X.                       CI0108
            13            PK41-DGDIN  PICTURE  9(8).                    CI0108
            12            PK41-GD30                                     CI0108
                          REDEFINES            PK41-GD99.               CI0108
            13            PK41-ISKED  PICTURE  X.                       CI0108
            13            PK41-CENXC  PICTURE  9(2).                    CI0108
            13            PK41-GD31.                                    CI0108
            14            PK41-FILLER PICTURE  X(245).                  CI0108
            13            PK41-GD32                                     CI0108
                          REDEFINES            PK41-GD31.               CI0108
            14            PK41-IABIN1 PICTURE  X.                       CI0108
            14            PK41-CLDOD  PICTURE  9(8).                    CI0108
            14            PK41-NCLAM  PICTURE  9(5)                     CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-ISURR  PICTURE  X.                       CI0108
            14            PK41-GEHCD  PICTURE  9(3).                    CI0108
            14            PK41-CRATC  PICTURE  9(4).                    CI0108
            14            PK41-AMAXD  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-ASCHGA PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-APYOM  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-IWTHH1 PICTURE  X.                       CI0108
            14            PK41-CPAYCL PICTURE  X(2).                    CI0108
            14            PK41-CTSAO  PICTURE  X.                       CI0108
            14            PK41-NCONF  PICTURE  9(08).                   CI0108
            14            PK41-CLID   PICTURE  X(23).                   CI0108
            14            PK41-CARTY  PICTURE  99.                      CI0108
            14            PK41-NARRS  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-CARTZ  PICTURE  99.                      CI0108
            14            PK41-NAPDS  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-CPMTO  PICTURE  X.                       CI0108
            14            PK41-DNPMT  PICTURE  9(8).                    CI0108
            14            PK41-IPCTV  PICTURE  X.                       CI0108
            14            PK41-IMECH  PICTURE  X(01).                   CI0108
            14            PK41-IMVAO  PICTURE  X(1).                    CI0108
            14            PK41-AMVA1  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-CACTS  PICTURE  X.                       CI0108
            14            PK41-CTSPP  PICTURE  X(1).                    CI0108
            14            PK41-CACT4  PICTURE  X(2).                    CI0108
            14            PK41-IVAEX  PICTURE  X.                       CI0108
            14            PK41-DFPMT  PICTURE  9(8).                    CI0108
            14            PK41-IDEMD  PICTURE  X.                       CI0108
            14            PK41-IOFST  PICTURE  X.                       CI0108
            14            PK41-AMXLB  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-ACULB  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-DEIRNB PICTURE  9(8).                    CI0108
            14            PK41-DEFFE  PICTURE  9(8).                    CI0108
            14            PK41-DEFFR  PICTURE  9(8).                    CI0108
            14            PK41-ISPUP  PICTURE  X.                       CI0108
            14            PK41-CPNCG  PICTURE  X.                       CI0108
            14            PK41-IEXPU  PICTURE  X.                       CI0108
            14            PK41-IPPCF  PICTURE  X.                       CI0108
            14            PK41-NAAPT  PICTURE  9(2).                    CI0108
            14            PK41-PWHLDS PICTURE  S999V9(5)                CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-ISWHO  PICTURE  X(1).                    CI0108
            13            PK41-GD33                                     CI0108
                          REDEFINES            PK41-GD31.               CI0108
            14            PK41-CPAYC  PICTURE  X(2).                    CI0108
            14            PK41-ADBRQX PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-ADBRQV PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-APTXR  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-CTRTPE PICTURE  X(2).                    CI0108
            14            PK41-NCLAMI PICTURE  S9(9)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-CLIDO8 PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-CLIDN  PICTURE  X(20).                   CI0108
            14            PK41-DSET01 PICTURE  S9(8)                    CI0108
                          BINARY.                                       CI0108
            14            PK41-CTSET1 PICTURE  S9(6)                    CI0108
                          BINARY.                                       CI0108
            14            PK41-DSET02 PICTURE  S9(8)                    CI0108
                          BINARY.                                       CI0108
            14            PK41-CTSET2 PICTURE  S9(6)                    CI0108
                          BINARY.                                       CI0108
            13            PK41-GD34                                     CI0108
                          REDEFINES            PK41-GD31.               CI0108
            14            PK41-QNOFM  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-CLTRM  PICTURE  99.                      CI0108
            14            PK41-AMXLN  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-ALADJ  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-ACHK   PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-APRMO  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-IMECH1 PICTURE  X(01).                   CI0108
            14            PK41-CACT41 PICTURE  X(2).                    CI0108
            14            PK41-ACDSCC PICTURE  S9(05)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-ACDSCD PICTURE  S9(05)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-GD39                                     CI0108
                          REDEFINES            PK41-GD31.               CI0108
            14            PK41-GD31.                                    CI0108
            15            PK41-FILLER PICTURE  X(245).                  CI0108
            12            PK41-GD40                                     CI0108
                          REDEFINES            PK41-GD99.               CI0108
            13            PK41-NTR    PICTURE  9(8).                    CI0108
            13            PK41-NPBNC  PICTURE  X(24).                   CI0108
            13            PK41-CRREV  PICTURE  X(3).                    CI0108
            13            PK41-CSUSL  PICTURE  S9.                      CI0108
            13            PK41-NMGLN1 PICTURE  9(4).                    CI0108
            13            PK41-DCAC92 PICTURE  9(8).                    CI0108
            13            PK41-NAASQ3 PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-GD49.                                    CI0108
            14            PK41-FILLER PICTURE  X(198).                  CI0108
            13            PK41-GD41                                     CI0108
                          REDEFINES            PK41-GD49.               CI0108
            14            PK41-CRREF  PICTURE  9(2).                    CI0108
            14            PK41-CORIR  PICTURE  X(02).                   CI0108
            14            PK41-CIPDB  PICTURE  X(03).                   CI0108
            14            PK41-CPAYH  PICTURE  X(02).                   CI0108
            14            PK41-NAMEX  PICTURE  9(15)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-DCHAE  PICTURE  9(4).                    CI0108
            14            PK41-DRQST  PICTURE  S9(8)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-GD42                                     CI0108
                          REDEFINES            PK41-GD49.               CI0108
            14            PK41-CPMTCB PICTURE  X(3).                    CI0108
            12            PK41-GD50                                     CI0108
                          REDEFINES            PK41-GD99.               CI0108
            13            PK41-ALOAD  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-PSLLD4 PICTURE  S99V999                  CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CSUSL1 PICTURE  S9.                      CI0108
            13            PK41-CRREV1 PICTURE  X(3).                    CI0108
            13            PK41-ADDAC  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-DL13.                                    CI0108
            14            PK41-GEYR   PICTURE  9(4).                    CI0108
            14            PK41-GEMTH  PICTURE  99.                      CI0108
            14            PK41-NDAY   PICTURE  99.                      CI0108
            13            PK41-NSEQ3P PICTURE  S9(5)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-XZ6A   PICTURE  X(6).                    CI0108
            13            PK41-XZ7    PICTURE  X(7).                    CI0108
            13            PK41-XZ6B   PICTURE  X(6).                    CI0108
            13            PK41-XZ6    PICTURE  X(6).                    CI0108
            13            PK41-XZ6C   PICTURE  X(6).                    CI0108
            13            PK41-XZ20   PICTURE  X(20).                   CI0108
            13            PK41-CATRN1 PICTURE  9(6).                    CI0108
            13            PK41-ADDAC2 PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-ATAXT2 PICTURE  S9(11)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-ACOMOT PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-XZ5    PICTURE  X(5).                    CI0108
            13            PK41-IREVD  PICTURE  X(1).                    CI0108
            13            PK41-ISUSP1 PICTURE  X(1).                    CI0108
            13            PK41-XZ6D   PICTURE  X(6).                    CI0108
            13            PK41-XZ13   PICTURE  X(13).                   CI0108
            13            PK41-CWHTP2 PICTURE  X(3).                    CI0108
            13            PK41-CWHTP3 PICTURE  X(3).                    CI0108
            13            PK41-DTREN  PICTURE  9(8).                    CI0108
            13            PK41-NAASQ1 PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            PK41-GD51                                     CI0108
                          REDEFINES            PK41-GD99.               CI0108
            13            PK41-ADOMOT PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-ACGLT  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-ACGST  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CTXMT  PICTURE  9(2).                    CI0108
            13            PK41-ALOAD3 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-FILLER PICTURE  X(31).                   CI0108
            12            PK41-GD52                                     CI0108
                          REDEFINES            PK41-GD99.               CI0108
            13            PK41-DEFFT5 PICTURE  9(8).                    CI0108
            13            PK41-PSLLD5 PICTURE  S99V999                  CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CSUSL2 PICTURE  S9.                      CI0108
            13            PK41-ALOAD2 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-DL22.                                    CI0108
            14            PK41-NYEAR1 PICTURE  9(4).                    CI0108
            14            PK41-GEMTHA PICTURE  99.                      CI0108
            14            PK41-NDAY01 PICTURE  99.                      CI0108
            13            PK41-NSEQ3R PICTURE  S9(5)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CWHTP  PICTURE  X(3).                    CI0108
            13            PK41-CWHFR  PICTURE  X(3).                    CI0108
            13            PK41-CATRN7 PICTURE  9(6).                    CI0108
            13            PK41-ATAXT5 PICTURE  S9(11)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-QSHOT  PICTURE  S9(10)V999               CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-ACINT3 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CWHTP1 PICTURE  X(3).                    CI0108
            13            PK41-CWHFR1 PICTURE  X(3).                    CI0108
            13            PK41-ACOMO5 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-QSHOMU PICTURE  S9(10)V999               CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-ACASH1 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-FILLER PICTURE  X(04).                   CI0108
            13            PK41-CATRN8 PICTURE  9(6).                    CI0108
            13            PK41-ALOAD1 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-PSLLD1 PICTURE  S99V999                  CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-QSHOT1 PICTURE  S9(10)V999               CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-ACINT4 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CSUSL4 PICTURE  S9.                      CI0108
            13            PK41-ACOMO4 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            PK41-GD60                                     CI0108
                          REDEFINES            PK41-GD99.               CI0108
            13            PK41-GEOPDD PICTURE  X(8)                     CI0108
                          OCCURS       005     TIMES.                   CI0108
            13            PK41-DLAUP3 PICTURE  9(8)                     CI0108
                          OCCURS       005     TIMES.                   CI0108
            13            PK41-GEOPDB PICTURE  X(8).                    CI0108
            13            PK41-DLAUP4 PICTURE  9(8).                    CI0108
            13            PK41-ITELR2 PICTURE  X.                       CI0108
            13            PK41-IPMTA  PICTURE  X.                       CI0108
            13            PK41-CCSMG  PICTURE  X.                       CI0108
            13            PK41-CPLEC  PICTURE  XX.                      CI0108
            13            PK41-CORTYA PICTURE  X(3).                    CI0108
            13            PK41-CACTBC PICTURE  X(1).                    CI0108
            13            PK41-CGSPIA PICTURE  X.                       CI0108
            13            PK41-IPTRDA PICTURE  X(01).                   CI0108
            13            PK41-GCUSPY PICTURE  X(12).                   CI0108
            13            PK41-CPALLA PICTURE  X(1).                    CI0108
            13            PK41-QSHO5A PICTURE  S9(9)V999                CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-IFRSAB PICTURE  X.                       CI0108
            13            PK41-DELOI  PICTURE  9(8).                    CI0108
            13            PK41-IAROAA PICTURE  X.                       CI0108
            13            PK41-ACINVR PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-ILTINA PICTURE  X.                       CI0108
            13            PK41-ALOIDA PICTURE  S9(11)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CFUNTA PICTURE  X(2).                    CI0108
            13            PK41-CLGND  PICTURE  X.                       CI0108
            13            PK41-CPH3U  PICTURE  X.                       CI0108
            13            PK41-GESTD  PICTURE  9(8).                    CI0108
            13            PK41-GEEND  PICTURE  9(8).                    CI0108
            13            PK41-CPMTF  PICTURE  99.                      CI0108
            13            PK41-CNAVR  PICTURE  X(1).                    CI0108
            12            PK41-GD70                                     CI0108
                          REDEFINES            PK41-GD99.               CI0108
            13            PK41-CMEMO  PICTURE  X(2).                    CI0108
            13            PK41-ALPLDT PICTURE  9(8).                    CI0108
            13            PK41-CTLPD  PICTURE  9(8).                    CI0108
            13            PK41-CPAYCM PICTURE  X(2).                    CI0108
            11            PK41-GC06.                                    CI0108
            12            PK41-GELL   PICTURE  9(4)                     CI0108
                          BINARY.                                       CI0108
            12            PK41-GE00.                                    CI0108
            13            PK41-GC06K.                                   CI0108
            14            PK41-NPISQ  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-ACOTD  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-PPOTD  PICTURE  S9(3)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-QPSTD  PICTURE  S9(7)V999                CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CPITC  PICTURE  99.                      CI0108
            13            PK41-ITRNB  PICTURE  X.                       CI0108
            13            PK41-FILLER PICTURE  X(14).                   CI0108
            12            PK41-GE98.                                    CI0108
            13            PK41-FILLER PICTURE  X(240).                  CI0108
            12            PK41-GE10                                     CI0108
                          REDEFINES            PK41-GE98.               CI0108
            13            PK41-CDELI  PICTURE  9(3).                    CI0108
            13            PK41-CPAYC  PICTURE  X(2).                    CI0108
            13            PK41-ICHKP  PICTURE  X.                       CI0108
            13            PK41-CLTIN  PICTURE  9(12).                   CI0108
            13            PK41-IFHAI  PICTURE  X.                       CI0108
            13            PK41-CDQUA  PICTURE  X(2).                    CI0108
            13            PK41-FILLER PICTURE  X(07).                   CI0108
            13            PK41-GE99.                                    CI0108
            14            PK41-FILLER PICTURE  X(212).                  CI0108
            13            PK41-GE01                                     CI0108
                          REDEFINES            PK41-GE99.               CI0108
            14            PK41-NTR    PICTURE  9(8).                    CI0108
            14            PK41-GECKD  PICTURE  9.                       CI0108
            14            PK41-NPBN   PICTURE  X(20).                   CI0108
            14            PK41-CCBAT  PICTURE  99.                      CI0108
            14            PK41-CLID4  PICTURE  X(23).                   CI0108
            14            PK41-GENAL1 PICTURE  X(30)                    CI0108
                          OCCURS       002     TIMES.                   CI0108
            14            PK41-GESAD1 PICTURE  X(30)                    CI0108
                          OCCURS       003     TIMES.                   CI0108
            13            PK41-GE02                                     CI0108
                          REDEFINES            PK41-GE99.               CI0108
            14            PK41-GENAL  PICTURE  X(30)                    CI0108
                          OCCURS       002     TIMES.                   CI0108
            14            PK41-GESAD  PICTURE  X(30)                    CI0108
                          OCCURS       003     TIMES.                   CI0108
            13            PK41-GE03                                     CI0108
                          REDEFINES            PK41-GE99.               CI0108
            14            PK41-NCHKN  PICTURE  9(11).                   CI0108
            13            PK41-GE04                                     CI0108
                          REDEFINES            PK41-GE99.               CI0108
            14            PK41-CTIDAP PICTURE  9(3).                    CI0108
            14            PK41-PRCOD  PICTURE  9(5).                    CI0108
            14            PK41-TDELI  PICTURE  X(30).                   CI0108
            14            PK41-CINCD  PICTURE  9(02).                   CI0108
            12            PK41-GE20                                     CI0108
                          REDEFINES            PK41-GE98.               CI0108
            13            PK41-C299.                                    CI0108
            14            PK41-CTID.                                    CI0108
            15            PK41-CTIDA  PICTURE  9(3).                    CI0108
            15            PK41-CTIDN.                                   CI0108
            16            PK41-CTIDNP PICTURE  X(13).                   CI0108
            16            PK41-CTIDND PICTURE  9(11).                   CI0108
            13            PK41-DCACG9 PICTURE  9(8).                    CI0108
            13            PK41-NAASQ  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            PK41-CIRAP  PICTURE  XX.                      CI0108
            13            PK41-CTYPE  PICTURE  X.                       CI0108
            13            PK41-INACT  PICTURE  X.                       CI0108
            13            PK41-FILLER PICTURE  X(01).                   CI0108
            13            PK41-ITPAC  PICTURE  X.                       CI0108
            13            PK41-ITAXI  PICTURE  X.                       CI0108
            13            PK41-IOWNC  PICTURE  X.                       CI0108
            13            PK41-CDVCD  PICTURE  X(2).                    CI0108
            13            PK41-CTCUS  PICTURE  999.                     CI0108
            13            PK41-CPMTCB PICTURE  X(3).                    CI0108
            13            PK41-CASTC1 PICTURE  99.                      CI0108
            13            PK41-PRCOD1 PICTURE  9(5).                    CI0108
            13            PK41-CPRSC1 PICTURE  X(9).                    CI0108
            13            PK41-CPRTB  PICTURE  X.                       CI0108
            13            PK41-CBRKD  PICTURE  9(4).                    CI0108
            13            PK41-FILLER PICTURE  X(12).                   CI0108
            12            PK41-GE30                                     CI0108
                          REDEFINES            PK41-GE98.               CI0108
            13            PK41-CFIDC  PICTURE  X(5).                    CI0108
            13            PK41-CPHSE  PICTURE  9(2).                    CI0108
            13            PK41-FILLER PICTURE  X(05).                   CI0108
            13            PK41-IABIN  PICTURE  X.                       CI0108
            13            PK41-PDFND  PICTURE  S999V9(3)                CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            PK41-GE40                                     CI0108
                          REDEFINES            PK41-GE98.               CI0108
            13            PK41-CACCT  PICTURE  X.                       CI0108
            13            PK41-CPAYR  PICTURE  X(2).                    CI0108
            13            PK41-CDELI1 PICTURE  9(3).                    CI0108
            13            PK41-CATRN.                                   CI0108
            14            PK41-CATRF  PICTURE  9(3).                    CI0108
            14            PK41-CATRS  PICTURE  9(3).                    CI0108
            13            PK41-DEFFT  PICTURE  9(8).                    CI0108
            13            PK41-CTYPC  PICTURE  X.                       CI0108
            13            PK41-CIRAPA PICTURE  XX.                      CI0108
            13            PK41-FILLER PICTURE  X(09).                   CI0108
            13            PK41-GE49.                                    CI0108
            14            PK41-FILLER PICTURE  X(208).                  CI0108
            13            PK41-GE41                                     CI0108
                          REDEFINES            PK41-GE49.               CI0108
            14            PK41-NCHKN1 PICTURE  9(6).                    CI0108
            13            PK41-GE42                                     CI0108
                          REDEFINES            PK41-GE49.               CI0108
            14            PK41-CTID1.                                   CI0108
            15            PK41-CTIDA1 PICTURE  9(3).                    CI0108
            15            PK41-CTIDP1 PICTURE  X(13).                   CI0108
            15            PK41-CTIDN1 PICTURE  9(11).                   CI0108
            13            PK41-GE43                                     CI0108
                          REDEFINES            PK41-GE49.               CI0108
            14            PK41-GENAL2 PICTURE  X(30)                    CI0108
                          OCCURS       002     TIMES.                   CI0108
            14            PK41-GESAD2 PICTURE  X(30)                    CI0108
                          OCCURS       003     TIMES.                   CI0108
            13            PK41-GE44                                     CI0108
                          REDEFINES            PK41-GE49.               CI0108
            14            PK41-CTID01.                                  CI0108
            15            PK41-CTIDA6 PICTURE  9(3).                    CI0108
            15            PK41-NTIDP2 PICTURE  X(13).                   CI0108
            15            PK41-CTIDN2 PICTURE  9(11).                   CI0108
            14            PK41-GECKD2 PICTURE  9.                       CI0108
            14            PK41-PACCT  PICTURE  S999V99                  CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-PLOAN  PICTURE  S999V99                  CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-PADPT  PICTURE  S999V99                  CI0108
                          COMPUTATIONAL-3.                              CI0108
            14            PK41-IPCTL  PICTURE  X.                       CI0108
            14            PK41-IPCTP  PICTURE  X.                       CI0108
            14            PK41-CEUNT  PICTURE  S9(5)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            PK41-GE31                                     CI0108
                          REDEFINES            PK41-GE98.               CI0108
            13            PK41-GCUSPZ PICTURE  X(12).                   CI0108
            11            PK41-GC12                                     CI0108
                          REDEFINES            PK41-GC06.               CI0108
            12            PK41-GC12K.                                   CI0108
            13            PK41-CIRAP  PICTURE  XX.                      CI0108
            12            PK41-AIRCT  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            PK41-FILLER PICTURE  X.                       CI0108
            11            PK41-GC04.                                    CI0108
            12            PK41-CLCUS  PICTURE  99.                      CI0108
            12            PK41-CCACT  PICTURE  99.                      CI0108
            12            PK41-AFEET  PICTURE  S9(5)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            PK41-ITERF  PICTURE  X.                       CI0108
            12            PK41-ATERF  PICTURE  S9(5)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            PK41-CLDOB  PICTURE  9(8).                    CI0108
            12            PK41-CPLTYP PICTURE  X(14).                   CI0108
            12            PK41-IACFPD PICTURE  X(1).                    CI0108
            12            PK41-FILLER PICTURE  X(14).                   CI0108
            11            PK41-GC21                                     CI0108
                          REDEFINES            PK41-GC04.               CI0108
            12            PK41-C299.                                    CI0108
            13            PK41-CTID.                                    CI0108
            14            PK41-CTIDA  PICTURE  9(3).                    CI0108
            14            PK41-CTIDN.                                   CI0108
            15            PK41-CTIDNP PICTURE  X(13).                   CI0108
            15            PK41-CTIDND PICTURE  9(11).                   CI0108
            12            PK41-DCACG9 PICTURE  9(8).                    CI0108
            12            PK41-NAASQ  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            PK41-FILLER PICTURE  X.                       CI0108

      *OUTPUT SEGMENT FROM CI0100
      *!WF DSP=PK DSL=PJ SEL=41 FOR=I DES=1 LEV=1 PLT=GC

       01                 GC01.                                         CI0108
            10            GC01-GC01K.                                   CI0108
            11            GC01-C299.                                    CI0108
            12            GC01-CTID.                                    CI0108
            13            GC01-CTIDA  PICTURE  9(3).                    CI0108
            13            GC01-CTIDN.                                   CI0108
            14            GC01-CTIDNP PICTURE  X(13).                   CI0108
            14            GC01-CTIDND PICTURE  9(11).                   CI0108
            10            GC01-DCAG9L PICTURE  9(8).                    CI0108
            10            GC01-NAASQL PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            GC01-ICUST  PICTURE  X.                       CI0108
            10            GC01-NSEQ4B PICTURE  9(8)                     CI0108
                          BINARY.                                       CI0108
            10            GC01-PRCOD  PICTURE  9(5).                    CI0108
            10            GC01-PRSCD  PICTURE  X(9).                    CI0108
            10            GC01-FILLER PICTURE  X(8).                    CI0108
       01                 GC03.                                         CI0108
            10            GC03-GELL   PICTURE  9(4)                     CI0108
                          BINARY.                                       CI0108
            10            GC03-GD00.                                    CI0108
            11            GC03-GC03K.                                   CI0108
            12            GC03-DCACG9 PICTURE  9(8).                    CI0108
            12            GC03-NAASQ  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-CAATY  PICTURE  9(3).                    CI0108
            11            GC03-CVSYS  PICTURE  X(2).                    CI0108
            11            GC03-CACTO  PICTURE  9(3).                    CI0108
            11            GC03-CATRN.                                   CI0108
            12            GC03-CATRF  PICTURE  9(3).                    CI0108
            12            GC03-CATRS  PICTURE  9(3).                    CI0108
            11            GC03-CASTC  PICTURE  99.                      CI0108
            11            GC03-IPULL  PICTURE  X.                       CI0108
            11            GC03-GEAUN  PICTURE  9(5).                    CI0108
            11            GC03-GEOPD2 PICTURE  X(8).                    CI0108
            11            GC03-NBTCH  PICTURE  9(4).                    CI0108
            11            GC03-DEFFT  PICTURE  9(8).                    CI0108
            11            GC03-NSUNT  PICTURE  9(4).                    CI0108
            11            GC03-ITRAN  PICTURE  X.                       CI0108
            11            GC03-DLAUP1 PICTURE  9(8).                    CI0108
            11            GC03-ADRET  PICTURE  S9(11)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-TTRMS  PICTURE  X(12).                   CI0108
            11            GC03-IDELT  PICTURE  X.                       CI0108
            11            GC03-GEOPDM PICTURE  X(8).                    CI0108
            11            GC03-FILLER PICTURE  X(07).                   CI0108
            10            GC03-GD09.                                    CI0108
            11            GC03-FILLER PICTURE  X(70).                   CI0108
            10            GC03-GD01                                     CI0108
                          REDEFINES            GC03-GD09.               CI0108
            11            GC03-ADBRQ  PICTURE  S9(11)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-CTRTP  PICTURE  X(2).                    CI0108
            11            GC03-CPORT  PICTURE  X.                       CI0108
            11            GC03-CSCRNU PICTURE  X(4).                    CI0108
            11            GC03-DLAUP  PICTURE  9(8).                    CI0108
            11            GC03-CTWHAT PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-PWHLD  PICTURE  S999V9(5)                CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-IWTHH  PICTURE  X.                       CI0108
            11            GC03-NDRFT  PICTURE  9(5).                    CI0108
            11            GC03-IDPAP  PICTURE  X.                       CI0108
            11            GC03-GETIM  PICTURE  S9(7)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-QNACT  PICTURE  9(3).                    CI0108
            11            GC03-AEDRQ  PICTURE  S9(11)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-IPLIN  PICTURE  X.                       CI0108
            11            GC03-CLIDNB PICTURE  9(8).                    CI0108
            11            GC03-CSLCT  PICTURE  X.                       CI0108
            11            GC03-ITELE  PICTURE  X.                       CI0108
            11            GC03-FILLER PICTURE  X(06).                   CI0108
            10            GC03-GD02                                     CI0108
                          REDEFINES            GC03-GD09.               CI0108
            11            GC03-CSYST  PICTURE  99.                      CI0108
            11            GC03-FILLER PICTURE  X.                       CI0108
            11            GC03-ACASH  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-DTRAC  PICTURE  9(8).                    CI0108
            11            GC03-CTRSO  PICTURE  9(02).                   CI0108
            11            GC03-NTRCE  PICTURE  9(06).                   CI0108
            11            GC03-GECKD1 PICTURE  9.                       CI0108
            11            GC03-CCOLL  PICTURE  X(3).                    CI0108
            11            GC03-CLTDP  PICTURE  X(3).                    CI0108
            11            GC03-PSLLD  PICTURE  S99V999                  CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-ISLOR  PICTURE  X.                       CI0108
            11            GC03-ITPAC  PICTURE  X.                       CI0108
            11            GC03-CPMTCA PICTURE  XXX.                     CI0108
            11            GC03-CSERV  PICTURE  X(3).                    CI0108
            11            GC03-ACOMO  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-IPLIN1 PICTURE  X.                       CI0108
            11            GC03-INQEX  PICTURE  X.                       CI0108
            11            GC03-CTKRAA PICTURE  X(12).                   CI0108
            11            GC03-CCSMQ  PICTURE  X.                       CI0108
            11            GC03-IVAEX1 PICTURE  X.                       CI0108
            11            GC03-IHPMT  PICTURE  X(1).                    CI0108
            11            GC03-GETIM3 PICTURE  S9(7)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            GC03-GD03                                     CI0108
                          REDEFINES            GC03-GD09.               CI0108
            11            GC03-CATRNC PICTURE  9(6).                    CI0108
            11            GC03-APRNT1 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-QSHOWT PICTURE  S9(10)V999               CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-ACINVT PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-ACOMO7 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-QSHOMW PICTURE  S9(10)V999               CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-ATAXT3 PICTURE  S9(11)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-CTSTR  PICTURE  9(2).                    CI0108
            11            GC03-ICIRA  PICTURE  X.                       CI0108
            11            GC03-GETIM2 PICTURE  S9(7)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-CPMTCX PICTURE  XX.                      CI0108
            11            GC03-FILLER PICTURE  X(16).                   CI0108
            10            GC03-GD99.                                    CI0108
            11            GC03-FILLER PICTURE  X(248).                  CI0108
            10            GC03-GD10                                     CI0108
                          REDEFINES            GC03-GD99.               CI0108
            11            GC03-MROTC  PICTURE  X(7).                    CI0108
            11            GC03-CEDSC  PICTURE  9(1).                    CI0108
            11            GC03-ILPOI  PICTURE  X(1).                    CI0108
            11            GC03-AWRCH  PICTURE  S9(3)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-CHCOC1 PICTURE  9(2).                    CI0108
            11            GC03-CHCOC2 PICTURE  9(2).                    CI0108
            11            GC03-CHCOC3 PICTURE  9(2).                    CI0108
            11            GC03-CHCOC4 PICTURE  9(2).                    CI0108
            11            GC03-CMCOC1 PICTURE  9(3).                    CI0108
            11            GC03-CMCOC2 PICTURE  9(3).                    CI0108
            11            GC03-CMCOC3 PICTURE  9(3).                    CI0108
            11            GC03-GD11.                                    CI0108
            12            GC03-FILLER PICTURE  X(219).                  CI0108
            11            GC03-GD12                                     CI0108
                          REDEFINES            GC03-GD11.               CI0108
            12            GC03-CELLO  PICTURE  9(1).                    CI0108
            12            GC03-CECLO  PICTURE  9(1).                    CI0108
            12            GC03-AEXML  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-CEPI   PICTURE  X(1).                    CI0108
            12            GC03-CEXTY  PICTURE  X.                       CI0108
            12            GC03-CROPC  PICTURE  9(1).                    CI0108
            12            GC03-CPUTY  PICTURE  9(1).                    CI0108
            12            GC03-IMCII  PICTURE  X(1).                    CI0108
            12            GC03-GEMISC                                   CI0108
                          OCCURS       010     TIMES.                   CI0108
            13            GC03-AMGLA  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            13            GC03-CMGLC  PICTURE  9(1).                    CI0108
            13            GC03-NMGLN  PICTURE  9(4).                    CI0108
            12            GC03-ACTRN  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-IWRBK  PICTURE  X.                       CI0108
            12            GC03-IFEDX  PICTURE  X.                       CI0108
            12            GC03-ICNTR  PICTURE  X.                       CI0108
            12            GC03-IOCKH  PICTURE  X.                       CI0108
            12            GC03-ICRCK  PICTURE  X.                       CI0108
            12            GC03-NHMPN  PICTURE  S9(10)                   CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-ITELR1 PICTURE  X.                       CI0108
            11            GC03-GD13                                     CI0108
                          REDEFINES            GC03-GD11.               CI0108
            12            GC03-DREDO  PICTURE  9(8).                    CI0108
            12            GC03-CATRNR PICTURE  9(6).                    CI0108
            12            GC03-CEVN   PICTURE  9(9).                    CI0108
            12            GC03-ISUSP  PICTURE  X(1).                    CI0108
            11            GC03-GD15                                     CI0108
                          REDEFINES            GC03-GD11.               CI0108
            12            GC03-CPUTZ  PICTURE  9(1).                    CI0108
            12            GC03-CETLB  PICTURE  9(3).                    CI0108
            12            GC03-QTRMC  PICTURE  9(3).                    CI0108
            12            GC03-DEFFTE PICTURE  9(8).                    CI0108
            12            GC03-DEFFTF PICTURE  9(8).                    CI0108
            12            GC03-DEFFTG PICTURE  9(8).                    CI0108
            12            GC03-XZ1A   PICTURE  X.                       CI0108
            12            GC03-XZ1B   PICTURE  X.                       CI0108
            12            GC03-XZ1C   PICTURE  X.                       CI0108
            12            GC03-XZ1D   PICTURE  X.                       CI0108
            12            GC03-XZ1E   PICTURE  X.                       CI0108
            12            GC03-XZ1F   PICTURE  X.                       CI0108
            12            GC03-XZ1G   PICTURE  X.                       CI0108
            12            GC03-XZ1H   PICTURE  X.                       CI0108
            12            GC03-XZ1I   PICTURE  X.                       CI0108
            12            GC03-DEFFTH PICTURE  9(8).                    CI0108
            11            GC03-GD19                                     CI0108
                          REDEFINES            GC03-GD11.               CI0108
            12            GC03-GD11.                                    CI0108
            13            GC03-FILLER PICTURE  X(219).                  CI0108
            10            GC03-GD20                                     CI0108
                          REDEFINES            GC03-GD99.               CI0108
            11            GC03-ADDACT PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-ISIGV  PICTURE  X.                       CI0108
            11            GC03-IALLF  PICTURE  X.                       CI0108
            11            GC03-QSHOWQ PICTURE  S9(9)V999                CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-CCDSCW PICTURE  9(2).                    CI0108
            11            GC03-IDWRL  PICTURE  X.                       CI0108
            11            GC03-ITELR  PICTURE  X.                       CI0108
            11            GC03-IABIN  PICTURE  X.                       CI0108
            11            GC03-PACT1  PICTURE  S999V999                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-IBFAF  PICTURE  X.                       CI0108
            11            GC03-IFRSA  PICTURE  X.                       CI0108
            11            GC03-ICRCAN PICTURE  X.                       CI0108
            11            GC03-ACACTV PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-AGFND  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-QCSHOW PICTURE  S9(9)V999                CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-QCSHIS PICTURE  S9(9)V999                CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-NDTRC  PICTURE  9(8).                    CI0108
            11            GC03-CAERU  PICTURE  X(4).                    CI0108
            11            GC03-IFDGO  PICTURE  X.                       CI0108
            11            GC03-PSLLD2 PICTURE  S99V999                  CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-ISLOR2 PICTURE  X.                       CI0108
            11            GC03-QSFIO  PICTURE  S9(10)V999               CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-QSFID  PICTURE  S9(10)V999               CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-CGDIN  PICTURE  X.                       CI0108
            11            GC03-DGDIN  PICTURE  9(8).                    CI0108
            10            GC03-GD30                                     CI0108
                          REDEFINES            GC03-GD99.               CI0108
            11            GC03-ISKED  PICTURE  X.                       CI0108
            11            GC03-CENXC  PICTURE  9(2).                    CI0108
            11            GC03-GD31.                                    CI0108
            12            GC03-FILLER PICTURE  X(245).                  CI0108
            11            GC03-GD32                                     CI0108
                          REDEFINES            GC03-GD31.               CI0108
            12            GC03-IABIN1 PICTURE  X.                       CI0108
            12            GC03-CLDOD  PICTURE  9(8).                    CI0108
            12            GC03-NCLAM  PICTURE  9(5)                     CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-ISURR  PICTURE  X.                       CI0108
            12            GC03-GEHCD  PICTURE  9(3).                    CI0108
            12            GC03-CRATC  PICTURE  9(4).                    CI0108
            12            GC03-AMAXD  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-ASCHGA PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-APYOM  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-IWTHH1 PICTURE  X.                       CI0108
            12            GC03-CPAYCL PICTURE  X(2).                    CI0108
            12            GC03-CTSAO  PICTURE  X.                       CI0108
            12            GC03-NCONF  PICTURE  9(08).                   CI0108
            12            GC03-CLID   PICTURE  X(23).                   CI0108
            12            GC03-CARTY  PICTURE  99.                      CI0108
            12            GC03-NARRS  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-CARTZ  PICTURE  99.                      CI0108
            12            GC03-NAPDS  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-CPMTO  PICTURE  X.                       CI0108
            12            GC03-DNPMT  PICTURE  9(8).                    CI0108
            12            GC03-IPCTV  PICTURE  X.                       CI0108
            12            GC03-IMECH  PICTURE  X(01).                   CI0108
            12            GC03-IMVAO  PICTURE  X(1).                    CI0108
            12            GC03-AMVA1  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-CACTS  PICTURE  X.                       CI0108
            12            GC03-CTSPP  PICTURE  X(1).                    CI0108
            12            GC03-CACT4  PICTURE  X(2).                    CI0108
            12            GC03-IVAEX  PICTURE  X.                       CI0108
            12            GC03-DFPMT  PICTURE  9(8).                    CI0108
            12            GC03-IDEMD  PICTURE  X.                       CI0108
            12            GC03-IOFST  PICTURE  X.                       CI0108
            12            GC03-AMXLB  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-ACULB  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-DEIRNB PICTURE  9(8).                    CI0108
            12            GC03-DEFFE  PICTURE  9(8).                    CI0108
            12            GC03-DEFFR  PICTURE  9(8).                    CI0108
            12            GC03-ISPUP  PICTURE  X.                       CI0108
            12            GC03-CPNCG  PICTURE  X.                       CI0108
            12            GC03-IEXPU  PICTURE  X.                       CI0108
            12            GC03-IPPCF  PICTURE  X.                       CI0108
            12            GC03-NAAPT  PICTURE  9(2).                    CI0108
            12            GC03-PWHLDS PICTURE  S999V9(5)                CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-ISWHO  PICTURE  X(1).                    CI0108
            11            GC03-GD33                                     CI0108
                          REDEFINES            GC03-GD31.               CI0108
            12            GC03-CPAYC  PICTURE  X(2).                    CI0108
            12            GC03-ADBRQX PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-ADBRQV PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-APTXR  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-CTRTPE PICTURE  X(2).                    CI0108
            12            GC03-NCLAMI PICTURE  S9(9)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-CLIDO8 PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-CLIDN  PICTURE  X(20).                   CI0108
            12            GC03-DSET01 PICTURE  S9(8)                    CI0108
                          BINARY.                                       CI0108
            12            GC03-CTSET1 PICTURE  S9(6)                    CI0108
                          BINARY.                                       CI0108
            12            GC03-DSET02 PICTURE  S9(8)                    CI0108
                          BINARY.                                       CI0108
            12            GC03-CTSET2 PICTURE  S9(6)                    CI0108
                          BINARY.                                       CI0108
            11            GC03-GD34                                     CI0108
                          REDEFINES            GC03-GD31.               CI0108
            12            GC03-QNOFM  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-CLTRM  PICTURE  99.                      CI0108
            12            GC03-AMXLN  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-ALADJ  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-ACHK   PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-APRMO  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-IMECH1 PICTURE  X(01).                   CI0108
            12            GC03-CACT41 PICTURE  X(2).                    CI0108
            12            GC03-ACDSCC PICTURE  S9(05)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-ACDSCD PICTURE  S9(05)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-GD39                                     CI0108
                          REDEFINES            GC03-GD31.               CI0108
            12            GC03-GD31.                                    CI0108
            13            GC03-FILLER PICTURE  X(245).                  CI0108
            10            GC03-GD40                                     CI0108
                          REDEFINES            GC03-GD99.               CI0108
            11            GC03-NTR    PICTURE  9(8).                    CI0108
            11            GC03-NPBNC  PICTURE  X(24).                   CI0108
            11            GC03-CRREV  PICTURE  X(3).                    CI0108
            11            GC03-CSUSL  PICTURE  S9.                      CI0108
            11            GC03-NMGLN1 PICTURE  9(4).                    CI0108
            11            GC03-DCAC92 PICTURE  9(8).                    CI0108
            11            GC03-NAASQ3 PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-GD49.                                    CI0108
            12            GC03-FILLER PICTURE  X(198).                  CI0108
            11            GC03-GD41                                     CI0108
                          REDEFINES            GC03-GD49.               CI0108
            12            GC03-CRREF  PICTURE  9(2).                    CI0108
            12            GC03-CORIR  PICTURE  X(02).                   CI0108
            12            GC03-CIPDB  PICTURE  X(03).                   CI0108
            12            GC03-CPAYH  PICTURE  X(02).                   CI0108
            12            GC03-NAMEX  PICTURE  9(15)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            12            GC03-DCHAE  PICTURE  9(4).                    CI0108
            12            GC03-DRQST  PICTURE  S9(8)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-GD42                                     CI0108
                          REDEFINES            GC03-GD49.               CI0108
            12            GC03-CPMTCB PICTURE  X(3).                    CI0108
            10            GC03-GD50                                     CI0108
                          REDEFINES            GC03-GD99.               CI0108
            11            GC03-ALOAD  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-PSLLD4 PICTURE  S99V999                  CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-CSUSL1 PICTURE  S9.                      CI0108
            11            GC03-CRREV1 PICTURE  X(3).                    CI0108
            11            GC03-ADDAC  PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-DL13.                                    CI0108
            12            GC03-GEYR   PICTURE  9(4).                    CI0108
            12            GC03-GEMTH  PICTURE  99.                      CI0108
            12            GC03-NDAY   PICTURE  99.                      CI0108
            11            GC03-NSEQ3P PICTURE  S9(5)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-XZ6A   PICTURE  X(6).                    CI0108
            11            GC03-XZ7    PICTURE  X(7).                    CI0108
            11            GC03-XZ6B   PICTURE  X(6).                    CI0108
            11            GC03-XZ6    PICTURE  X(6).                    CI0108
            11            GC03-XZ6C   PICTURE  X(6).                    CI0108
            11            GC03-XZ20   PICTURE  X(20).                   CI0108
            11            GC03-CATRN1 PICTURE  9(6).                    CI0108
            11            GC03-ADDAC2 PICTURE  S9(7)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-ATAXT2 PICTURE  S9(11)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-ACOMOT PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-XZ5    PICTURE  X(5).                    CI0108
            11            GC03-IREVD  PICTURE  X(1).                    CI0108
            11            GC03-ISUSP1 PICTURE  X(1).                    CI0108
            11            GC03-XZ6D   PICTURE  X(6).                    CI0108
            11            GC03-XZ13   PICTURE  X(13).                   CI0108
            11            GC03-CWHTP2 PICTURE  X(3).                    CI0108
            11            GC03-CWHTP3 PICTURE  X(3).                    CI0108
            11            GC03-DTREN  PICTURE  9(8).                    CI0108
            11            GC03-NAASQ1 PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            GC03-GD51                                     CI0108
                          REDEFINES            GC03-GD99.               CI0108
            11            GC03-ADOMOT PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-ACGLT  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-ACGST  PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-CTXMT  PICTURE  9(2).                    CI0108
            11            GC03-ALOAD3 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-FILLER PICTURE  X(31).                   CI0108
            10            GC03-GD52                                     CI0108
                          REDEFINES            GC03-GD99.               CI0108
            11            GC03-DEFFT5 PICTURE  9(8).                    CI0108
            11            GC03-PSLLD5 PICTURE  S99V999                  CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-CSUSL2 PICTURE  S9.                      CI0108
            11            GC03-ALOAD2 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-DL22.                                    CI0108
            12            GC03-NYEAR1 PICTURE  9(4).                    CI0108
            12            GC03-GEMTHA PICTURE  99.                      CI0108
            12            GC03-NDAY01 PICTURE  99.                      CI0108
            11            GC03-NSEQ3R PICTURE  S9(5)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-CWHTP  PICTURE  X(3).                    CI0108
            11            GC03-CWHFR  PICTURE  X(3).                    CI0108
            11            GC03-CATRN7 PICTURE  9(6).                    CI0108
            11            GC03-ATAXT5 PICTURE  S9(11)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-QSHOT  PICTURE  S9(10)V999               CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-ACINT3 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-CWHTP1 PICTURE  X(3).                    CI0108
            11            GC03-CWHFR1 PICTURE  X(3).                    CI0108
            11            GC03-ACOMO5 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-QSHOMU PICTURE  S9(10)V999               CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-ACASH1 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-FILLER PICTURE  X(04).                   CI0108
            11            GC03-CATRN8 PICTURE  9(6).                    CI0108
            11            GC03-ALOAD1 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-PSLLD1 PICTURE  S99V999                  CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-QSHOT1 PICTURE  S9(10)V999               CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-ACINT4 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-CSUSL4 PICTURE  S9.                      CI0108
            11            GC03-ACOMO4 PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            GC03-GD60                                     CI0108
                          REDEFINES            GC03-GD99.               CI0108
            11            GC03-GEOPDD PICTURE  X(8)                     CI0108
                          OCCURS       005     TIMES.                   CI0108
            11            GC03-DLAUP3 PICTURE  9(8)                     CI0108
                          OCCURS       005     TIMES.                   CI0108
            11            GC03-GEOPDB PICTURE  X(8).                    CI0108
            11            GC03-DLAUP4 PICTURE  9(8).                    CI0108
            11            GC03-ITELR2 PICTURE  X.                       CI0108
            11            GC03-IPMTA  PICTURE  X.                       CI0108
            11            GC03-CCSMG  PICTURE  X.                       CI0108
            11            GC03-CPLEC  PICTURE  XX.                      CI0108
            11            GC03-CORTYA PICTURE  X(3).                    CI0108
            11            GC03-CACTBC PICTURE  X(1).                    CI0108
            11            GC03-CGSPIA PICTURE  X.                       CI0108
            11            GC03-IPTRDA PICTURE  X(01).                   CI0108
            11            GC03-GCUSPY PICTURE  X(12).                   CI0108
            11            GC03-CPALLA PICTURE  X(1).                    CI0108
            11            GC03-QSHO5A PICTURE  S9(9)V999                CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-IFRSAB PICTURE  X.                       CI0108
            11            GC03-DELOI  PICTURE  9(8).                    CI0108
            11            GC03-IAROAA PICTURE  X.                       CI0108
            11            GC03-ACINVR PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-ILTINA PICTURE  X.                       CI0108
            11            GC03-ALOIDA PICTURE  S9(11)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            GC03-CFUNTA PICTURE  X(2).                    CI0108
            11            GC03-CLGND  PICTURE  X.                       CI0108
            11            GC03-CPH3U  PICTURE  X.                       CI0108
            11            GC03-GESTD  PICTURE  9(8).                    CI0108
            11            GC03-GEEND  PICTURE  9(8).                    CI0108
            11            GC03-CPMTF  PICTURE  99.                      CI0108
            11            GC03-CNAVR  PICTURE  X(1).                    CI0108
            10            GC03-GD70                                     CI0108
                          REDEFINES            GC03-GD99.               CI0108
            11            GC03-CMEMO  PICTURE  X(2).                    CI0108
            11            GC03-ALPLDT PICTURE  9(8).                    CI0108
            11            GC03-CTLPD  PICTURE  9(8).                    CI0108
            11            GC03-CPAYCM PICTURE  X(2).                    CI0108
       01                 GC04.                                         CI0108
            10            GC04-CLCUS  PICTURE  99.                      CI0108
            10            GC04-CCACT  PICTURE  99.                      CI0108
            10            GC04-AFEET  PICTURE  S9(5)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            GC04-ITERF  PICTURE  X.                       CI0108
            10            GC04-ATERF  PICTURE  S9(5)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            GC04-CLDOB  PICTURE  9(8).                    CI0108
            10            GC04-CPLTYP PICTURE  X(14).                   CI0108
            10            GC04-IACFPD PICTURE  X(1).                    CI0108
            10            GC04-FILLER PICTURE  X(14).                   CI0108
       01                 GR01.                                         CI0108
            10            GR01-GR01K.                                   CI0108
            11            GR01-GR98.                                    CI0108
            12            GR01-GRID.                                    CI0108
            13            GR01-GRIDC  PICTURE  9(3).                    CI0108
            13            GR01-GRIDN.                                   CI0108
            14            GR01-GRIDNP PICTURE  99.                      CI0108
            14            GR01-GRIDND PICTURE  9(8).                    CI0108
            10            GR01-GECKD  PICTURE  9.                       CI0108
            10            GR01-GEMDA  PICTURE  9(8).                    CI0108
            10            GR01-NSEQ4B PICTURE  9(8)                     CI0108
                          BINARY.                                       CI0108
            10            GR01-GRDOR  PICTURE  9(8).                    CI0108
            10            GR01-GRIAD  PICTURE  9(8).                    CI0108
            10            GR01-GECUC  PICTURE  99.                      CI0108
            10            GR01-GRLNG  PICTURE  99.                      CI0108
            10            GR01-GESLC  PICTURE  99.                      CI0108
            10            GR01-AYSIDA PICTURE  9(3).                    CI0108
            10            GR01-AYSID  PICTURE  9(5).                    CI0108
            10            GR01-GRCSD  PICTURE  9(8).                    CI0108
            10            GR01-GRCFD  PICTURE  9(8).                    CI0108
            10            GR01-GRNCL  PICTURE  S9(5)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            GR01-GRNCT  PICTURE  S9(5)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            GR01-GRSFC  PICTURE  99.                      CI0108
            10            GR01-GRCRN  PICTURE  9(3).                    CI0108
            10            GR01-GRCSS  PICTURE  X.                       CI0108
            10            GR01-MKSRC  PICTURE  99                       CI0108
                          OCCURS       010     TIMES.                   CI0108
            10            GR01-NEFPS  PICTURE  X(5).                    CI0108
            10            GR01-DEFPS  PICTURE  9(8).                    CI0108
            10            GR01-DLSRV  PICTURE  9(8).                    CI0108
            10            GR01-CTLNI  PICTURE  X.                       CI0108
            10            GR01-CGRLI  PICTURE  X.                       CI0108
            10            GR01-CAMGR  PICTURE  9(5)                     CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            GR01-CAMGS  PICTURE  9(5)                     CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            GR01-CAMGN  PICTURE  9(3)                     CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            GR01-CGRMF  PICTURE  X.                       CI0108
            10            GR01-FILLER PICTURE  X(08).                   CI0108
       01                 GR07.                                         CI0108
            10            GR07-GEDLA  PICTURE  9(8).                    CI0108
            10            GR07-GRAID  PICTURE  X(12).                   CI0108
            10            GR07-GRPAP  PICTURE  X(14).                   CI0108
            10            GR07-GEPHNX PICTURE  9(4).                    CI0108
            10            GR07-DPLEF  PICTURE  9(8).                    CI0108
            10            GR07-DPLAM  PICTURE  9(8).                    CI0108
            10            GR07-NCPFN  PICTURE  9(6).                    CI0108
            10            GR07-GEFYE  PICTURE  9(4).                    CI0108
            10            GR07-FILLER PICTURE  X(06).                   CI0108
            10            GR07-GRPAN  PICTURE  X(45).                   CI0108
            10            GR07-CGRPA  PICTURE  99.                      CI0108
            10            GR07-IPRTT7 PICTURE  X.                       CI0108
            10            GR07-GRPED  PICTURE  9(8).                    CI0108
            10            GR07-FILLER PICTURE  X(05).                   CI0108
            10            GR07-GRPLC  PICTURE  99.                      CI0108
            10            GR07-GRPLT  PICTURE  99.                      CI0108
            10            GR07-FILLER PICTURE  X(04).                   CI0108
            10            GR07-GEADI  PICTURE  X.                       CI0108
            10            GR07-GRCFA  PICTURE  S9(11)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            GR07-GECFY  PICTURE  9(4).                    CI0108
            10            GR07-GECFC  PICTURE  99.                      CI0108
            10            GR07-MEMPL  PICTURE  X(20).                   CI0108
            10            GR07-CAUNIT PICTURE  X(4).                    CI0108
            10            GR07-FILLER PICTURE  X(21).                   CI0108
            10            GR07-GRPPP  PICTURE  999.                     CI0108
            10            GR07-CCORT  PICTURE  9(3).                    CI0108
            10            GR07-CIDRP  PICTURE  99.                      CI0108
            10            GR07-CCDWA  PICTURE  9.                       CI0108
            10            GR07-IERSA  PICTURE  X.                       CI0108
            10            GR07-DERSA  PICTURE  9(8).                    CI0108
            10            GR07-FILLER PICTURE  X(04).                   CI0108
       01                 GR19.                                         CI0108
            10            GR19-GR19K.                                   CI0108
            11            GR19-C299.                                    CI0108
            12            GR19-CTID.                                    CI0108
            13            GR19-CTIDA  PICTURE  9(3).                    CI0108
            13            GR19-CTIDN.                                   CI0108
            14            GR19-CTIDNP PICTURE  X(13).                   CI0108
            14            GR19-CTIDND PICTURE  9(11).                   CI0108
            10            GR19-GERSD  PICTURE  9(8).                    CI0108
            10            GR19-GERED  PICTURE  9(8).                    CI0108
            10            GR19-GRCSI  PICTURE  X.                       CI0108
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
      *---------------  SQL INCLUDE STATEMENTS ---------------------    ADB220
           EXEC SQL     INCLUDE SQLCA             END-EXEC.             ADB220
      *                                                                 ADB220
      *--------------  ERROR HANDLING VARIABLES --------------------    ADB220
       01               7-DB2-FUNCT      PIC X(35) VALUE SPACES.        ADB220
       01               7-SQLR.                                         ADB220
         05             7-SQLR-TEXT-LEN  PIC S9(9) COMP VALUE +80.      ADB220
         05             7-SQLR-MESSAGE.                                 ADB220
           10           7-SQLR-LEN       PIC S9(4) COMP VALUE +960.     ADB220
           10           7-SQLR-TEXT      PIC X(80) OCCURS 12 TIMES.     ADB220
       01               7-DB2-ABEND      PIC S9(4) COMP VALUE +3570.    ADB220
       01               7-SQL-ERROR      PIC S999  COMP.                ADB220
       01               7-ABEND-MOD      PIC X(8)  VALUE 'W910808'.     ADB220
       01               7-TEST-SQLCODE   PIC S9(9) COMP.                ADB220
           88           ROW-NOT-FOUND              VALUE +100.          ADB220
           88           DUPLICATE-KEY              VALUE -803.          ADB220
           88           MULTIPLE-ROWS-FOUND        VALUE -811.          ADB220
           88           RESOURCE-NOT-AVAILABLE     VALUE -904.          ADB220
           88           RESOURCE-IN-USE            VALUE -913.          ADB220
      ******************************************************************ADUTAB
      **              TABLE TA5A ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA5A-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=5A FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA5A.                                                CI0108
           04    G-TA5A-PARAM.                                          CI0108
             10  G-TA5A-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0108
                        VALUE      +772.                                CI0108
             10  G-TA5A-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0108
                        VALUE      +001.                                CI0108
             10  G-TA5A-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0108
                        VALUE      +017.                                CI0108
             10  G-TA5A-NUAPP  PICTURE 99                               CI0108
                        VALUE       0.                                  CI0108
             10  G-TA5A-NUTAB  PICTURE X(6)                             CI0108
                        VALUE 'TA005A'.                                 CI0108
             10  G-TA5A-TABFO  PICTURE XX                 VALUE SPACE.  CI0108
             10  G-TA5A-TABCR  PICTURE XX                 VALUE SPACE.  CI0108
             10  G-TA5A-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0108
             10  G-TA5A-NUSSC  PICTURE X  VALUE   ' '.                  CI0108
             10  G-TA5A-NUSSY  PICTURE X                  VALUE SPACE.  CI0108
             10  G-TA5A-TRANID PICTURE X(4)               VALUE SPACE.  CI0108
             10  G-TA5A-FILSYS.                                         CI0108
             15  G-TA5A-USERC  PICTURE X(6)               VALUE SPACE.  CI0108
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0108
           04             TA5A.                                         CI0108
            10            TA5A-GAPSC.                                   CI0108
            11            TA5A-CTIDA  PICTURE  9(3)                     CI0108
                          VALUE                ZERO.                    CI0108
            11            TA5A-PRCOD  PICTURE  9(5)                     CI0108
                          VALUE                ZERO.                    CI0108
            11            TA5A-PRSCD  PICTURE  X(9)                     CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-PRCLN  PICTURE  X(60)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-PRCMN  PICTURE  X(20)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-PRCSN  PICTURE  X(9)                     CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-MRCLN1 PICTURE  X(51)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-MRCLN2 PICTURE  X(51)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-MRCLN3 PICTURE  X(51)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-MRCLN4 PICTURE  X(51)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-MRCMN2 PICTURE  X(20)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-MRCMN3 PICTURE  X(20)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-PRCCS1 PICTURE  X(15)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-PRCCS2 PICTURE  X(15)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-PRCCS3 PICTURE  X(15)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-MPCLN  PICTURE  X(45)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-MPCL1  PICTURE  X(45)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-MSP1   PICTURE  X(60)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-MSP5   PICTURE  X(30)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-MSP03  PICTURE  X(3)                     CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-MPRDG  PICTURE  X(20)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-CPRDG  PICTURE  9(2)                     CI0108
                          VALUE                ZERO.                    CI0108
            10            TA5A-MPRDA1 PICTURE  X(50)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-CPRDA1 PICTURE  9(3)                     CI0108
                          VALUE                ZERO.                    CI0108
            10            TA5A-MSP06  PICTURE  X(20)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-CPOIN  PICTURE  X                        CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-CPITY  PICTURE  9(3)                     CI0108
                          VALUE                ZERO.                    CI0108
            10            TA5A-CLITY  PICTURE  9(3)                     CI0108
                          VALUE                ZERO.                    CI0108
            10            TA5A-IVARP  PICTURE  X                        CI0108
                          VALUE                SPACE.                   CI0108
            10            TA5A-CASCL  PICTURE  9(3)                     CI0108
                          VALUE                ZERO.                    CI0108
            10            TA5A-ZDA88  PICTURE  X(88)                    CI0108
                          VALUE                SPACE.                   CI0108
      **                                                                ADUTAB
      ******************************************************************ADUTAB
      **              TABLE TJ08 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TJ08-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TJ DSL=TJ SEL=08 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TJ08.                                                CI0108
           04    G-TJ08-PARAM.                                          CI0108
             10  G-TJ08-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0108
                        VALUE      +154.                                CI0108
             10  G-TJ08-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0108
                        VALUE      +001.                                CI0108
             10  G-TJ08-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0108
                        VALUE      +010.                                CI0108
             10  G-TJ08-NUAPP  PICTURE 99                               CI0108
                        VALUE       0.                                  CI0108
             10  G-TJ08-NUTAB  PICTURE X(6)                             CI0108
                        VALUE 'TJ0008'.                                 CI0108
             10  G-TJ08-TABFO  PICTURE XX                 VALUE SPACE.  CI0108
             10  G-TJ08-TABCR  PICTURE XX                 VALUE SPACE.  CI0108
             10  G-TJ08-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0108
             10  G-TJ08-NUSSC  PICTURE X  VALUE   ' '.                  CI0108
             10  G-TJ08-NUSSY  PICTURE X                  VALUE SPACE.  CI0108
             10  G-TJ08-TRANID PICTURE X(4)               VALUE SPACE.  CI0108
             10  G-TJ08-FILSYS.                                         CI0108
             15  G-TJ08-USERC  PICTURE X(6)               VALUE SPACE.  CI0108
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0108
           04             TJ08.                                         CI0108
            10            TJ08-CNTTCS PICTURE  X(10)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TJ08-TSHDS  PICTURE  X(80)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            TJ08-AFEETQ PICTURE  S9(5)V99                 CI0108
                          VALUE                ZERO.                    CI0108
            10            TJ08-AFEETK PICTURE  S9(5)V99                 CI0108
                          VALUE                ZERO.                    CI0108
            10            TJ08-ZDA50  PICTURE  X(50)                    CI0108
                          VALUE                SPACE.                   CI0108
      **                                                                ADUTAB
      *BEGIN DB2          UL03                                          CI0108
       01                 UL03.                                         CI0108
            10            UL03-CLID   PICTURE  X(23).                   CI0108
            10            UL03-NSHID  PICTURE  X(20).                   CI0108
            10            UL03-DFEEW  PICTURE  X(4).                    CI0108
            10            UL03-CFEWT  PICTURE  X(10).                   CI0108
            10            UL03-AFEWV  PICTURE  S9(10)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            UL03-PFEWV  PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            UL03-CFEWD  PICTURE  X(10).                   CI0108
            10            UL03-DRSTR  PICTURE  X(10).                   CI0108
            10            UL03-DREND  PICTURE  X(10).                   CI0108
            10            UL03-XCTMP  PICTURE  X(26).                   CI0108
            10            UL03-XUTMP  PICTURE  X(26).                   CI0108
            10            UL03-MCRBY  PICTURE  X(16).                   CI0108
            10            UL03-MUPBY  PICTURE  X(16).                   CI0108
            10            UL03-MCRPG  PICTURE  X(08).                   CI0108
            10            UL03-MUPPG  PICTURE  X(08).                   CI0108
      *END DB2                                                          CI0108
      *!WF DSP=UQ DSL=U1 SEL=40 FOR=I DES=2 LEV=1 PLT=UQ
       01                 UQ40.                                         CI0108
            10            UQ40-NSHID  PICTURE  X(20)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-TSHDS  PICTURE  X(80)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-CNTTCS PICTURE  X(10)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-CTPAA  PICTURE  X(10)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-CNIRA1 PICTURE  X                        CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-CNIRA2 PICTURE  X                        CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-IIRASB PICTURE  X(01)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-IIRASA PICTURE  X                        CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-DFSED  PICTURE  X(10)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-XCTMP  PICTURE  X(26)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-XUTMP  PICTURE  X(26)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-MCRBY  PICTURE  X(16)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-MUPBY  PICTURE  X(16)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-MCRPG  PICTURE  X(08)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-MUPPG  PICTURE  X(08)                    CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-ICUSP  PICTURE  X(1)                     CI0108
                          VALUE                SPACE.                   CI0108
            10            UQ40-CCSTO  PICTURE  X(10)                    CI0108
                          VALUE                SPACE.                   CI0108
      *BEGIN DB2          UQ04                                          CI0108
       01                 UQ04.                                         CI0108
            10            UQ04-CLID   PICTURE  X(23).                   CI0108
            10            UQ04-NSHID  PICTURE  X(20).                   CI0108
            10            UQ04-CRFEE  PICTURE  X(10).                   CI0108
            10            UQ04-DFEEC  PICTURE  X(4).                    CI0108
            10            UQ04-DFTAK  PICTURE  X(10).                   CI0108
            10            UQ04-NSEQ2  PICTURE  S9(4)                    CI0108
                          BINARY.                                       CI0108
            10            UQ04-AFECH  PICTURE  S9(10)V99                CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            UQ04-CTID   PICTURE  X(27).                   CI0108
            10            UQ04-XCTMP  PICTURE  X(26).                   CI0108
            10            UQ04-XUTMP  PICTURE  X(26).                   CI0108
            10            UQ04-MCRBY  PICTURE  X(16).                   CI0108
            10            UQ04-MUPBY  PICTURE  X(16).                   CI0108
            10            UQ04-MCRPG  PICTURE  X(08).                   CI0108
            10            UQ04-MUPPG  PICTURE  X(08).                   CI0108
            10            UQ04-CFPSC  PICTURE  X(10).                   CI0108
            10            UQ04-CFPMC  PICTURE  X(10).                   CI0108
      *END DB2                                                          CI0108
      *BEGIN DB2          US02                                          CI0108
       01                 US02.                                         CI0108
            10            US02-NSHID  PICTURE  X(20).                   CI0108
            10            US02-CTID   PICTURE  X(27).                   CI0108
            10            US02-DRSTR  PICTURE  X(10).                   CI0108
            10            US02-DREND  PICTURE  X(10).                   CI0108
            10            US02-XCTMP  PICTURE  X(26).                   CI0108
            10            US02-XUTMP  PICTURE  X(26).                   CI0108
            10            US02-MCRBY  PICTURE  X(16).                   CI0108
            10            US02-MUPBY  PICTURE  X(16).                   CI0108
            10            US02-MCRPG  PICTURE  X(08).                   CI0108
            10            US02-MUPPG  PICTURE  X(08).                   CI0108
      *END DB2                                                          CI0108
      ******************************************************************APRDTP
      *USED TO DETERMINE MAJOR PRODUCT TYPE FOR VALUATION               APRDTP
       01  7-PRODUCT.                                                   APRDTP
      *!WI pl=WK040                                                     APRDTP
           05  7-CPRDG                                                  APRDTP
                        PICTURE 9(2).                                   CI0108
               88 7-CERTIFICATE-PRODUCT    VALUE 01.                    APRDTP
               88 7-MUTUAL-FUND-PRODUCT    VALUE 02.                    APRDTP
               88 7-SPECIAL-PRODUCT        VALUE 03.                    APRDTP
               88 7-LIFE-PRODUCT           VALUE 04.                    APRDTP
               88 7-OTHER-PRODUCT          VALUE 10.                    APRDTP
      *!WI pl=WK100                                                     APRDTP
           05  7-CPRDA1                                                 APRDTP
                        PICTURE 9(3).                                   CI0108
       01  FILLER REDEFINES 7-PRODUCT  PIC 9(05).                       APRDTP
           88 7-FLEX-SAVINGS-GROUP     VALUE  01101.                    APRDTP
           88 7-CASH-RESERVE-GROUP     VALUE  01102.                    APRDTP
           88 7-FUTURE-VALUE-GROUP     VALUE  01104.                    APRDTP
           88 7-STOCK-MARKET-GROUP     VALUE  01105.                    APRDTP
           88 7-QUARTERLY-GROUP        VALUE  01106.                    APRDTP
           88 7-TAX-DEFERRED-GROUP     VALUE  01107.                    APRDTP
           88 7-SINGLE-PAY-GROUP       VALUE  01108.                    APRDTP
           88 7-SERIES-D--GROUP        VALUE  01109.                    APRDTP
           88 7-MARKET-STRAT-GROUP     VALUE  01110.                    APRDTP
           88 7-LTD-PARTNER-GROUP      VALUE  03301.                    APRDTP
           88 7-BROKERED-INS-GROUP     VALUE  03302.                    APRDTP
           88 7-SECURITIES-GROUP       VALUE  03303.                    APRDTP
           88 7-BETAANNUITY-GROUP      VALUE  03306.                    APRDTP
           88 7-LIFE-INSURANCE-GROUP   VALUES 04401 04402.              APRDTP
           88 7-TRAD-INSURANCE-GROUP   VALUE  04401.                    APRDTP
           88 7-UL-INSURANCE-GROUP     VALUES 04402.                    APRDTP
           88 7-LONG-TERM-CARE-GROUP   VALUE  04403.                    APRDTP
           88 7-DISABILITY-GROUP       VALUE  04404.                    APRDTP
           88 7-ANNUITY-GROUP          VALUE  04405.                    APRDTP
           88 7-FINANCIAL-PLAN-GROUP   VALUE  10002.                    APRDTP
           88 7-BANK-GROUP             VALUES 10004 10005 10098.        APRDTP
           88 7-BANK-CARD-GROUP        VALUE  10004.                    APRDTP
           88 7-SPS-GROUP              VALUE  10096.                    APRDTP
           88 7-OTHER-OTHER-GROUP      VALUES 10001 10003 10006 10007   APRDTP
                                              10094 10095 10097.        APRDTP
      *FUTURE USE *                                                     APRDTP
      *FUTURE USE *                                                     APRDTP
      *FUTURE USE *                                                     APRDTP
      *FUTURE USE *                                                     APRDTP
      *FUTURE USE *                                                     APRDTP
      *FUTURE USE *                                                     APRDTP
      *FUTURE USE *                                                     APRDTP
      *FUTURE USE *                                                     APRDTP

      *SWITCHES TO INDICATE WHETHER OR NOT A DATABASE SEGMENT WAS FOUND
      *'1' = FOUND
      *'0' = NOT FOUND
       01  CL47-CF             PIC X  VALUE '0'.
       01  GR19-CF             PIC X  VALUE '0'.

      *PROGRAM 88 LEVELS
       01  7-LEVEL-88S.

           05  7-IS-ACCT-TO-PROCESS    PIC X  VALUE 'N'.
               88  ACCT-TO-BE-PROCESSED       VALUE 'Y'.
               88  ACCT-TO-BE-SKIPPED         VALUE 'N'.

       01  7-WORK-INIT-FIELDS.

      *    TAXPAYER ON SOURCE ACCT
      *!WI
           05  7-TAXPAYER-CLID
                        PICTURE X(23).                                  CI0108

      *    TAXPAYER ON DEST ACCT
      *!WI
           05  7-DESTINATION-CLID
                        PICTURE X(23).                                  CI0108

      *    TAXPAYER ON OTHER ACCTS IN SOURCE ACCT GROUP
      *!WI
           05  7-OTHER-CLID
                        PICTURE X(23).                                  CI0108

      *    CT10-GRID FOR SOURCE ACCT
           05  7-SRCE-GRID.
      *!WI
               10  7-SRCE-GRIDC
                        PICTURE 9(3).                                   CI0108
               10  FILLER          PIC 9(10).

      *    CT10-GRID FOR DEST ACCT
           05  7-DEST-GRID.
      *!WI
               10  7-DEST-GRIDC
                        PICTURE 9(3).                                   CI0108
               10  FILLER          PIC 9(10).

      *    CT01-CTIDA FOR SOURCE ACCT
      *!WI
           05  7-SRCE-CTIDA
                        PICTURE 9(3).                                   CI0108

      *    CT01-CTIDA FOR DEST ACCT
      *!WI
           05  7-DEST-CTIDA
                        PICTURE 9(3).                                   CI0108

      *    CUSTODIAL ACCT FIELDS FOR SOURCE ACCT IN HH GROUP
           05  7-SRCE-HH-ATTR.
      *!WI
               10  7-SRCE-CTCCI
                        PICTURE X.                                      CI0108
      *!WI
               10  7-SRCE-CQACT
                        PICTURE 999.                                    CI0108
      *!WI
               10  7-SRCE-CIRAT
                        PICTURE 999.                                    CI0108
      *!WI
               10  7-SRCE-CIRAS
                        PICTURE 999.                                    CI0108

      *    CUSTODIAL ACCT FIELDS FOR DEST ACCT IN HH GROUP
           05  7-DEST-HH-ATTR.
      *!WI
               10  7-DEST-CTCCI
                        PICTURE X.                                      CI0108
      *!WI
               10  7-DEST-CQACT
                        PICTURE 999.                                    CI0108
      *!WI
               10  7-DEST-CIRAT
                        PICTURE 999.                                    CI0108
      *!WI
               10  7-DEST-CIRAS
                        PICTURE 999.                                    CI0108

      *    CUSTODIAL ACCT FIELDS FOR SOURCE ACCT IN PENSION GROUP
           05  7-SRCE-PEN-ATTR.
      *!WI
               10  7-SRCE-CIDRP
                        PICTURE 99.                                     CI0108
      *!WI
               10  7-SRCE-GRPLT
                        PICTURE 99.                                     CI0108
      *!WI
               10  7-SRCE-GRPLC
                        PICTURE 99.                                     CI0108

      *    CUSTODIAL ACCT FIELDS FOR DEST ACCT IN PENSION GROUP
           05  7-DEST-PEN-ATTR.
      *!WI
               10  7-DEST-CIDRP
                        PICTURE 99.                                     CI0108
      *!WI
               10  7-DEST-GRPLT
                        PICTURE 99.                                     CI0108
      *!WI
               10  7-DEST-GRPLC
                        PICTURE 99.                                     CI0108
      ******************************************************************
      **         WS ELEMENT TO ACCUMULATE FEE PAID                     *
      ******************************************************************
      *!WI
       01  WS00-AFECH              VALUE ZEROES
                        PICTURE S9(10)V99                               CI0108
                          COMPUTATIONAL-3.                              CI0108
       01  WS-MORE-RECORDS    PIC X      VALUE 'Y'.
      *DEFULT SHELTER ID
      *!WI
       01  WS-NSHID              VALUE SPACES
                        PICTURE X(20).                                  CI0108
      *DEFULT END DATE
      *!WI
       01  WS-DREND              VALUE '9999-12-31'
                        PICTURE X(10).                                  CI0108
      *FEE TYPE CODE FOR CUSTODIAL FEES
      *!WI
       01  7-CUST-CRFEE            VALUE '0500080001'
                        PICTURE X(10).                                  CI0108

      *FEE TYPE CODE FOR TERMINATION FEES
      *!WI
       01  7-TERM-CRFEE            VALUE '0500080002'
                        PICTURE X(10).                                  CI0108


      *USED FOR BOTH CUST & TERM FEES. SET TO 'Y' IF IT'S DETERMINED
      *FROM READING TBU304 THAT THE FEE HAS ALREADY BEEN PAID.
       01  7-FEE-PAID              PIC X(01) VALUE 'N'.

      *USED ONLY FOR CUST FEE. SET TO 'Y' IF IT'S DETERMINED FROM
      *READING TBU303 THAT THERE IS A CUST FEE WAIVER CODE.
       01  7-FEE-WAIVED            PIC X(01) VALUE 'N'.

      *INDICATES IF THERE'S A PENDING FULL DISB ON THE ACTIVITY
      *DATABASE ON AN ACCOUNT IN THE SAME GROUP AS THE SOURCE ACCT
       01  7-FULL-REDEMP           PIC X.

      *INDICATES IF CUST/TERM FEES HAVE ALREADY BEEN TAKEN TODAY
      *FROM ANOTHER ACCOUNT IN SAME GROUP AS SOURCE ACCOUNT
       01  7-FEES-TAKEN            PIC X.


      *NO. OF WHOLE YEARS TO BE SUBTRACTED FROM CAMS CURR ACCTG DATE
      *(700000 FOR 70 YEARS; 590000 FOR 59 YEARS)
       01  7-NO-OF-YEARS           PIC 9(06).

      *CALCULATED FIELD. WILL CONTAIN A DATE 70.5 OR 59.5 YEARS BACK
      *FROM CAMS CURRENT ACCOUNTING DATE.
       01  7-WS-YRS-BACK           PIC  9(8).

       01   DEBUT-WSS.                                                  CI0108
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0108
            05   IK     PICTURE X.                                      CI0108
       01  CONSTANTES-PAC.                                              CI0108
           05  FILLER  PICTURE X(87)   VALUE                            CI0108
                     '6015 CAT09/08/14CI0108ADMIN   14:34:50CI0108P AMERCI0108
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0108
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0108
           05  NUGNA   PICTURE X(5).                                    CI0108
           05  APPLI   PICTURE X(3).                                    CI0108
           05  DATGN   PICTURE X(8).                                    CI0108
           05  PROGR   PICTURE X(6).                                    CI0108
           05  CODUTI  PICTURE X(8).                                    CI0108
           05  TIMGN   PICTURE X(8).                                    CI0108
           05  PROGE   PICTURE X(8).                                    CI0108
           05  COBASE  PICTURE X(4).                                    CI0108
           05  DATGNC  PICTURE X(10).                                   CI0108
           05  RELEAS  PICTURE X(7).                                    CI0108
           05  DATGE   PICTURE X(10).                                   CI0108
           05  DATSQ   PICTURE X(10).                                   CI0108
       01  DATCE.                                                       CI0108
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0108
         05  DATOR.                                                     CI0108
           10  DATOA  PICTURE XX.                                       CI0108
           10  DATOM  PICTURE XX.                                       CI0108
           10  DATOJ  PICTURE XX.                                       CI0108
       01   VARIABLES-CONDITIONNELLES.                                  CI0108
            05                  FT      PICTURE X VALUE '0'.            CI0108
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0108
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0108
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J90GHR PICTURE S9(4) VALUE  ZERO.
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0108
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0108
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0108
            05       5-GC00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0108
            05       5-GR00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0108
            05       5-UL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0108
            05       5-UQ00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0108
            05       5-US00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0108
       01               S-CL01-SSA.                                     CI0108
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0108
                                      VALUE 'CL01    '.                 CI0108
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0108
            10          S-CL01-CCOD   PICTURE X(5)                      CI0108
                                      VALUE '-----'.                    CI0108
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0108
       01            S-CLU01-SSA.                                       CI0108
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'CL01    '.                 CI0108
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0108
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(CL01K'.                   CI0108
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0108
            10       S-CLU01-CL01K.                                     CI0108
            11       S-CLU01-C199.                                      CI0108
            12       S-CLU01-CLID.                                      CI0108
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0108
            13       S-CLU01-CLIDN.                                     CI0108
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0108
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0108
            10  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01               S-CL03-SSA.                                     CI0108
            10         S1-CL03-SEGNAM PICTURE X(8)                      CI0108
                                      VALUE 'CL03    '.                 CI0108
            10         S1-CL03-CCOM   PICTURE X VALUE '*'.              CI0108
            10          S-CL03-CCOD   PICTURE X(5)                      CI0108
                                      VALUE '-----'.                    CI0108
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0108
       01            S-CLA03-SSA.                                       CI0108
            10      S1-CLA03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'CL03    '.                 CI0108
            10      S1-CLA03-CCOM   PICTURE X VALUE '*'.                CI0108
            10       S-CLA03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            10      S1-CLA03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(CLDOD'.                   CI0108
            10       S-CLA03-OPER  PICTURE XX VALUE ' ='.               CI0108
            10       S-CLA03-CLDOD    PICTURE  9(8).                    CI0108
            10  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01               S-CT01-SSA.                                     CI0108
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0108
                                      VALUE 'CT01    '.                 CI0108
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0108
            10          S-CT01-CCOD   PICTURE X(5)                      CI0108
                                      VALUE '-----'.                    CI0108
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0108
       01            S-CTU01-SSA.                                       CI0108
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'CT01    '.                 CI0108
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0108
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(CT01K'.                   CI0108
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0108
            10       S-CTU01-CT01K.                                     CI0108
            11       S-CTU01-C299.                                      CI0108
            12       S-CTU01-CTID.                                      CI0108
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0108
            13       S-CTU01-CTIDN.                                     CI0108
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0108
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0108
            10  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01               S-CT07-SSA.                                     CI0108
            10         S1-CT07-SEGNAM PICTURE X(8)                      CI0108
                                      VALUE 'CT07    '.                 CI0108
            10         S1-CT07-CCOM   PICTURE X VALUE '*'.              CI0108
            10          S-CT07-CCOD   PICTURE X(5)                      CI0108
                                      VALUE '-----'.                    CI0108
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0108
       01            S-CTU07-SSA.                                       CI0108
            10      S1-CTU07-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'CT07    '.                 CI0108
            10      S1-CTU07-CCOM   PICTURE X VALUE '*'.                CI0108
            10       S-CTU07-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            10      S1-CTU07-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(CT07K'.                   CI0108
            10       S-CTU07-OPER  PICTURE XX VALUE ' ='.               CI0108
            10       S-CTU07-CT07K.                                     CI0108
            11       S-CTU07-C199.                                      CI0108
            12       S-CTU07-CLID.                                      CI0108
            13       S-CTU07-CLIDO    PICTURE  9(3).                    CI0108
            13       S-CTU07-CLIDN.                                     CI0108
            14       S-CTU07-CLIDNP   PICTURE  X(12).                   CI0108
            14       S-CTU07-CLIDND   PICTURE  9(8).                    CI0108
            10  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01               S-CT09-SSA.                                     CI0108
            10         S1-CT09-SEGNAM PICTURE X(8)                      CI0108
                                      VALUE 'CT09    '.                 CI0108
            10         S1-CT09-CCOM   PICTURE X VALUE '*'.              CI0108
            10          S-CT09-CCOD   PICTURE X(5)                      CI0108
                                      VALUE '-----'.                    CI0108
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0108
       01            S-CTA09-SSA.                                       CI0108
            11      S1-CTA09-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'CT09    '.                 CI0108
            11      S1-CTA09-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-CTA09-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-CTA09-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(GERED'.                   CI0108
            11       S-CTA09-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-CTA09-GERED    PICTURE  9(8).                    CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-CTB09-SSA.                                       CI0108
            11      S1-CTB09-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'CT09    '.                 CI0108
            11      S1-CTB09-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-CTB09-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-CTB09-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(GECSQ'.                   CI0108
            11       S-CTB09-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-CTB09-GECSQ    PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-CTU09-SSA.                                       CI0108
            11      S1-CTU09-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'CT09    '.                 CI0108
            11      S1-CTU09-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-CTU09-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-CTU09-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(CT09K'.                   CI0108
            11       S-CTU09-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-CTU09-CT09K.                                     CI0108
            12       S-CTU09-CLCTRC   PICTURE  9(3).                    CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01               S-CT10-SSA.                                     CI0108
            10         S1-CT10-SEGNAM PICTURE X(8)                      CI0108
                                      VALUE 'CT10    '.                 CI0108
            10         S1-CT10-CCOM   PICTURE X VALUE '*'.              CI0108
            10          S-CT10-CCOD   PICTURE X(5)                      CI0108
                                      VALUE '-----'.                    CI0108
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0108
       01            S-CTA10-SSA.                                       CI0108
            10      S1-CTA10-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'CT10    '.                 CI0108
            10      S1-CTA10-CCOM   PICTURE X VALUE '*'.                CI0108
            10       S-CTA10-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            10      S1-CTA10-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(GERED'.                   CI0108
            10       S-CTA10-OPER  PICTURE XX VALUE ' ='.               CI0108
            10       S-CTA10-GERED    PICTURE  9(8).                    CI0108
            10  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-CTB10-SSA.                                       CI0108
            11      S1-CTB10-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'CT10    '.                 CI0108
            11      S1-CTB10-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-CTB10-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-CTB10-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(GRIDCB'.                  CI0108
            11       S-CTB10-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-CTB10-GRIDCB   PICTURE  9(3).                    CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-CTC10-SSA.                                       CI0108
            10      S1-CTC10-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'CT10    '.                 CI0108
            10      S1-CTC10-CCOM   PICTURE X VALUE '*'.                CI0108
            10       S-CTC10-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            10      S1-CTC10-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(GRCSI'.                   CI0108
            10       S-CTC10-OPER  PICTURE XX VALUE ' ='.               CI0108
            10       S-CTC10-GRCSI    PICTURE  X.                       CI0108
            10  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-CTU10-SSA.                                       CI0108
            10      S1-CTU10-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'CT10    '.                 CI0108
            10      S1-CTU10-CCOM   PICTURE X VALUE '*'.                CI0108
            10       S-CTU10-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            10      S1-CTU10-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(CT10K'.                   CI0108
            10       S-CTU10-OPER  PICTURE XX VALUE ' ='.               CI0108
            10       S-CTU10-CT10K.                                     CI0108
            11       S-CTU10-GR98.                                      CI0108
            12       S-CTU10-GRID.                                      CI0108
            13       S-CTU10-GRIDC    PICTURE  9(3).                    CI0108
            13       S-CTU10-GRIDN.                                     CI0108
            14       S-CTU10-GRIDNP   PICTURE  99.                      CI0108
            14       S-CTU10-GRIDND   PICTURE  9(8).                    CI0108
            10  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01               S-GC01-SSA.                                     CI0108
            10         S1-GC01-SEGNAM PICTURE X(8)                      CI0108
                                      VALUE 'GC01    '.                 CI0108
            10         S1-GC01-CCOM   PICTURE X VALUE '*'.              CI0108
            10          S-GC01-CCOD   PICTURE X(5)                      CI0108
                                      VALUE '-----'.                    CI0108
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0108
       01            S-GCU01-SSA.                                       CI0108
            10      S1-GCU01-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC01    '.                 CI0108
            10      S1-GCU01-CCOM   PICTURE X VALUE '*'.                CI0108
            10       S-GCU01-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            10      S1-GCU01-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(GC01K'.                   CI0108
            10       S-GCU01-OPER  PICTURE XX VALUE ' ='.               CI0108
            10       S-GCU01-GC01K.                                     CI0108
            11       S-GCU01-C299.                                      CI0108
            12       S-GCU01-CTID.                                      CI0108
            13       S-GCU01-CTIDA    PICTURE  9(3).                    CI0108
            13       S-GCU01-CTIDN.                                     CI0108
            14       S-GCU01-CTIDNP   PICTURE  X(13).                   CI0108
            14       S-GCU01-CTIDND   PICTURE  9(11).                   CI0108
            10  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01               S-GC03-SSA.                                     CI0108
            10         S1-GC03-SEGNAM PICTURE X(8)                      CI0108
                                      VALUE 'GC03    '.                 CI0108
            10         S1-GC03-CCOM   PICTURE X VALUE '*'.              CI0108
            10          S-GC03-CCOD   PICTURE X(5)                      CI0108
                                      VALUE '-----'.                    CI0108
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0108
       01            S-GCA03-SSA.                                       CI0108
            11      S1-GCA03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GCA03-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GCA03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GCA03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(CAATY'.                   CI0108
            11       S-GCA03-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GCA03-CAATY    PICTURE  9(3).                    CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GCB03-SSA.                                       CI0108
            11      S1-GCB03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GCB03-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GCB03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GCB03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(CVSYS'.                   CI0108
            11       S-GCB03-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GCB03-CVSYS    PICTURE  X(2).                    CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GCC03-SSA.                                       CI0108
            11      S1-GCC03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GCC03-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GCC03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GCC03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(CASTC'.                   CI0108
            11       S-GCC03-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GCC03-CASTC    PICTURE  99.                      CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GCD03-SSA.                                       CI0108
            11      S1-GCD03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GCD03-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GCD03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GCD03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(CACTO'.                   CI0108
            11       S-GCD03-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GCD03-CACTO    PICTURE  9(3).                    CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GCE03-SSA.                                       CI0108
            11      S1-GCE03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GCE03-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GCE03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GCE03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(IPULL'.                   CI0108
            11       S-GCE03-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GCE03-IPULL    PICTURE  X.                       CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GCF03-SSA.                                       CI0108
            11      S1-GCF03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GCF03-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GCF03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GCF03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(DTRAC'.                   CI0108
            11       S-GCF03-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GCF03-DTRAC    PICTURE  9(8).                    CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GCG03-SSA.                                       CI0108
            11      S1-GCG03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GCG03-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GCG03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GCG03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(CTRSO'.                   CI0108
            11       S-GCG03-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GCG03-CTRSO    PICTURE  9(02).                   CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GCH03-SSA.                                       CI0108
            11      S1-GCH03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GCH03-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GCH03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GCH03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(NTRCE'.                   CI0108
            11       S-GCH03-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GCH03-NTRCE    PICTURE  9(06).                   CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GCI03-SSA.                                       CI0108
            11      S1-GCI03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GCI03-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GCI03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GCI03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(ITRAN'.                   CI0108
            11       S-GCI03-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GCI03-ITRAN    PICTURE  X.                       CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GCJ03-SSA.                                       CI0108
            11      S1-GCJ03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GCJ03-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GCJ03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GCJ03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(DEFFT'.                   CI0108
            11       S-GCJ03-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GCJ03-DEFFT    PICTURE  9(8).                    CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GCK03-SSA.                                       CI0108
            11      S1-GCK03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GCK03-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GCK03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GCK03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(CPMTCA'.                  CI0108
            11       S-GCK03-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GCK03-CPMTCA   PICTURE  XXX.                     CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GCL03-SSA.                                       CI0108
            11      S1-GCL03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GCL03-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GCL03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GCL03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(ACASH'.                   CI0108
            11       S-GCL03-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GCL03-ACASH    PICTURE  S9(9)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GCN03-SSA.                                       CI0108
            11      S1-GCN03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GCN03-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GCN03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GCN03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(CRREV'.                   CI0108
            11       S-GCN03-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GCN03-CRREV    PICTURE  X(3).                    CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GCO03-SSA.                                       CI0108
            11      S1-GCO03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GCO03-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GCO03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GCO03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(CSYST'.                   CI0108
            11       S-GCO03-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GCO03-CSYST    PICTURE  99.                      CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GCU03-SSA.                                       CI0108
            11      S1-GCU03-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GCU03-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GCU03-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GCU03-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(GC03K'.                   CI0108
            11       S-GCU03-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GCU03-GC03K.                                     CI0108
            12       S-GCU03-DCACG9   PICTURE  9(8).                    CI0108
            12       S-GCU03-NAASQ    PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GC103-SSA.                                       CI0108
            12      S1-GC103-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            12      S1-GC103-CCOM   PICTURE X VALUE '*'.                CI0108
            12       S-GC103-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            12      S1-GC103-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(XDCACG9'.                 CI0108
            12       S-GC103-OPER  PICTURE XX VALUE ' ='.               CI0108
            12       S-GC103-DCACG9   PICTURE  9(8).                    CI0108
            12  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GC203-SSA.                                       CI0108
            11      S1-GC203-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GC203-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GC203-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GC203-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(XGEAUN'.                  CI0108
            11       S-GC203-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GC203-GEAUN    PICTURE  9(5).                    CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GC303-SSA.                                       CI0108
            11      S1-GC303-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GC303-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GC303-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GC303-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(XGEOPD2'.                 CI0108
            11       S-GC303-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GC303-GEOPD2   PICTURE  X(8).                    CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GC403-SSA.                                       CI0108
            11      S1-GC403-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            11      S1-GC403-CCOM   PICTURE X VALUE '*'.                CI0108
            11       S-GC403-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            11      S1-GC403-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(XNBTCH'.                  CI0108
            11       S-GC403-OPER  PICTURE XX VALUE ' ='.               CI0108
            11       S-GC403-NBTCH    PICTURE  9(4).                    CI0108
            11  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GC803-SSA.                                       CI0108
            12      S1-GC803-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GC03    '.                 CI0108
            12      S1-GC803-CCOM   PICTURE X VALUE '*'.                CI0108
            12       S-GC803-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            12      S1-GC803-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(XNAASQ'.                  CI0108
            12       S-GC803-OPER  PICTURE XX VALUE ' ='.               CI0108
            12       S-GC803-NAASQ    PICTURE  S9(3)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            12  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01               S-GC04-SSA.                                     CI0108
            10         S1-GC04-SEGNAM PICTURE X(8)                      CI0108
                                      VALUE 'GC04    '.                 CI0108
            10         S1-GC04-CCOM   PICTURE X VALUE '*'.              CI0108
            10          S-GC04-CCOD   PICTURE X(5)                      CI0108
                                      VALUE '-----'.                    CI0108
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0108
       01               S-GR01-SSA.                                     CI0108
            10         S1-GR01-SEGNAM PICTURE X(8)                      CI0108
                                      VALUE 'GR01    '.                 CI0108
            10         S1-GR01-CCOM   PICTURE X VALUE '*'.              CI0108
            10          S-GR01-CCOD   PICTURE X(5)                      CI0108
                                      VALUE '-----'.                    CI0108
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0108
       01            S-GRU01-SSA.                                       CI0108
            10      S1-GRU01-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GR01    '.                 CI0108
            10      S1-GRU01-CCOM   PICTURE X VALUE '*'.                CI0108
            10       S-GRU01-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            10      S1-GRU01-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(GR01K'.                   CI0108
            10       S-GRU01-OPER  PICTURE XX VALUE ' ='.               CI0108
            10       S-GRU01-GR01K.                                     CI0108
            11       S-GRU01-GR98.                                      CI0108
            12       S-GRU01-GRID.                                      CI0108
            13       S-GRU01-GRIDC    PICTURE  9(3).                    CI0108
            13       S-GRU01-GRIDN.                                     CI0108
            14       S-GRU01-GRIDNP   PICTURE  99.                      CI0108
            14       S-GRU01-GRIDND   PICTURE  9(8).                    CI0108
            10  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01               S-GR07-SSA.                                     CI0108
            10         S1-GR07-SEGNAM PICTURE X(8)                      CI0108
                                      VALUE 'GR07    '.                 CI0108
            10         S1-GR07-CCOM   PICTURE X VALUE '*'.              CI0108
            10          S-GR07-CCOD   PICTURE X(5)                      CI0108
                                      VALUE '-----'.                    CI0108
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0108
       01            S-GRA07-SSA.                                       CI0108
            10      S1-GRA07-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GR07    '.                 CI0108
            10      S1-GRA07-CCOM   PICTURE X VALUE '*'.                CI0108
            10       S-GRA07-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            10      S1-GRA07-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(NCPFN'.                   CI0108
            10       S-GRA07-OPER  PICTURE XX VALUE ' ='.               CI0108
            10       S-GRA07-NCPFN    PICTURE  9(6).                    CI0108
            10  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01               S-GR19-SSA.                                     CI0108
            10         S1-GR19-SEGNAM PICTURE X(8)                      CI0108
                                      VALUE 'GR19    '.                 CI0108
            10         S1-GR19-CCOM   PICTURE X VALUE '*'.              CI0108
            10          S-GR19-CCOD   PICTURE X(5)                      CI0108
                                      VALUE '-----'.                    CI0108
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0108
       01            S-GRA19-SSA.                                       CI0108
            10      S1-GRA19-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GR19    '.                 CI0108
            10      S1-GRA19-CCOM   PICTURE X VALUE '*'.                CI0108
            10       S-GRA19-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            10      S1-GRA19-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(GERED'.                   CI0108
            10       S-GRA19-OPER  PICTURE XX VALUE ' ='.               CI0108
            10       S-GRA19-GERED    PICTURE  9(8).                    CI0108
            10  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01            S-GRU19-SSA.                                       CI0108
            10      S1-GRU19-SEGNAM PICTURE X(8)                        CI0108
                                      VALUE 'GR19    '.                 CI0108
            10      S1-GRU19-CCOM   PICTURE X VALUE '*'.                CI0108
            10       S-GRU19-CCOD   PICTURE X(5)                        CI0108
                                      VALUE '-----'.                    CI0108
            10      S1-GRU19-FLDNAM PICTURE X(9)                        CI0108
                                      VALUE '(GR19K'.                   CI0108
            10       S-GRU19-OPER  PICTURE XX VALUE ' ='.               CI0108
            10       S-GRU19-GR19K.                                     CI0108
            11       S-GRU19-C299.                                      CI0108
            12       S-GRU19-CTID.                                      CI0108
            13       S-GRU19-CTIDA    PICTURE  9(3).                    CI0108
            13       S-GRU19-CTIDN.                                     CI0108
            14       S-GRU19-CTIDNP   PICTURE  X(13).                   CI0108
            14       S-GRU19-CTIDND   PICTURE  9(11).                   CI0108
            10  FILLER   PICTURE X    VALUE ')'.                        CI0108
       01                VUL03.                                         CI0108
          05             VUL03CLID     PICTURE S9(4) COMP.              CI0108
          05             VUL03NSHID    PICTURE S9(4) COMP.              CI0108
          05             VUL03DFEEW    PICTURE S9(4) COMP.              CI0108
          05             VUL03CFEWT    PICTURE S9(4) COMP.              CI0108
          05             VUL03AFEWV    PICTURE S9(4) COMP.              CI0108
          05             VUL03PFEWV    PICTURE S9(4) COMP.              CI0108
          05             VUL03CFEWD    PICTURE S9(4) COMP.              CI0108
          05             VUL03DRSTR    PICTURE S9(4) COMP.              CI0108
          05             VUL03DREND    PICTURE S9(4) COMP.              CI0108
          05             VUL03XCTMP    PICTURE S9(4) COMP.              CI0108
          05             VUL03XUTMP    PICTURE S9(4) COMP.              CI0108
          05             VUL03MCRBY    PICTURE S9(4) COMP.              CI0108
          05             VUL03MUPBY    PICTURE S9(4) COMP.              CI0108
          05             VUL03MCRPG    PICTURE S9(4) COMP.              CI0108
          05             VUL03MUPPG    PICTURE S9(4) COMP.              CI0108
       01                VUL03R    REDEFINES  VUL03.                    CI0108
          05             VUL03A        PICTURE S9(4) COMP               CI0108
                         OCCURS                      0015.              CI0108
       01                VUQ04.                                         CI0108
          05             VUQ04CLID     PICTURE S9(4) COMP.              CI0108
          05             VUQ04NSHID    PICTURE S9(4) COMP.              CI0108
          05             VUQ04CRFEE    PICTURE S9(4) COMP.              CI0108
          05             VUQ04DFEEC    PICTURE S9(4) COMP.              CI0108
          05             VUQ04DFTAK    PICTURE S9(4) COMP.              CI0108
          05             VUQ04NSEQ2    PICTURE S9(4) COMP.              CI0108
          05             VUQ04AFECH    PICTURE S9(4) COMP.              CI0108
          05             VUQ04CTID     PICTURE S9(4) COMP.              CI0108
          05             VUQ04XCTMP    PICTURE S9(4) COMP.              CI0108
          05             VUQ04XUTMP    PICTURE S9(4) COMP.              CI0108
          05             VUQ04MCRBY    PICTURE S9(4) COMP.              CI0108
          05             VUQ04MUPBY    PICTURE S9(4) COMP.              CI0108
          05             VUQ04MCRPG    PICTURE S9(4) COMP.              CI0108
          05             VUQ04MUPPG    PICTURE S9(4) COMP.              CI0108
          05             VUQ04CFPSC    PICTURE S9(4) COMP.              CI0108
          05             VUQ04CFPMC    PICTURE S9(4) COMP.              CI0108
       01                VUQ04R    REDEFINES  VUQ04.                    CI0108
          05             VUQ04A        PICTURE S9(4) COMP               CI0108
                         OCCURS                      0016.              CI0108
       01                VUS02.                                         CI0108
          05             VUS02NSHID    PICTURE S9(4) COMP.              CI0108
          05             VUS02CTID     PICTURE S9(4) COMP.              CI0108
          05             VUS02DRSTR    PICTURE S9(4) COMP.              CI0108
          05             VUS02DREND    PICTURE S9(4) COMP.              CI0108
          05             VUS02XCTMP    PICTURE S9(4) COMP.              CI0108
          05             VUS02XUTMP    PICTURE S9(4) COMP.              CI0108
          05             VUS02MCRBY    PICTURE S9(4) COMP.              CI0108
          05             VUS02MUPBY    PICTURE S9(4) COMP.              CI0108
          05             VUS02MCRPG    PICTURE S9(4) COMP.              CI0108
          05             VUS02MUPPG    PICTURE S9(4) COMP.              CI0108
       01                VUS02R    REDEFINES  VUS02.                    CI0108
          05             VUS02A        PICTURE S9(4) COMP               CI0108
                         OCCURS                      0010.              CI0108
       01   ZONES-UTILISATEUR PICTURE X.                                CI0108
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
      ** PCB POINTER FOR GR1P                                           ADU015
            05 PCB-GR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR ACAP                                           ADU015
            05 PCB-ACAP-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0108
          05              PA00-SUITE.                                   CI0108
            15       FILLER         PICTURE  X(00106).                  CI0108
       01                 PA06  REDEFINES      PA00.                    CI0108
            10            PA06-XDBPCB.                                  CI0108
            11            PA06-XDBDNM PICTURE  X(08).                   CI0108
            11            PA06-XSEGLV PICTURE  X(02).                   CI0108
            11            PA06-XRC    PICTURE  X(02).                   CI0108
            11            PA06-XPROPT PICTURE  X(04).                   CI0108
            11            PA06-FILLER PICTURE  S9(5)                    CI0108
                          BINARY.                                       CI0108
            11            PA06-XSEGNM PICTURE  X(08).                   CI0108
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0108
                          BINARY.                                       CI0108
            11            PA06-XSEGNB PICTURE  9(05)                    CI0108
                          BINARY.                                       CI0108
            11            PA06-XCOKEY PICTURE  X(70).                   CI0108
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0108
          05              PB00-SUITE.                                   CI0108
            15       FILLER         PICTURE  X(00106).                  CI0108
       01                 PB06  REDEFINES      PB00.                    CI0108
            10            PB06-XDBPCB.                                  CI0108
            11            PB06-XDBDNM PICTURE  X(08).                   CI0108
            11            PB06-XSEGLV PICTURE  X(02).                   CI0108
            11            PB06-XRC    PICTURE  X(02).                   CI0108
            11            PB06-XPROPT PICTURE  X(04).                   CI0108
            11            PB06-FILLER PICTURE  S9(5)                    CI0108
                          BINARY.                                       CI0108
            11            PB06-XSEGNM PICTURE  X(08).                   CI0108
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0108
                          BINARY.                                       CI0108
            11            PB06-XSEGNB PICTURE  9(05)                    CI0108
                          BINARY.                                       CI0108
            11            PB06-XCOKEY PICTURE  X(70).                   CI0108
      *** PCB MASK FOR GR1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0108
          05              PC00-SUITE.                                   CI0108
            15       FILLER         PICTURE  X(00106).                  CI0108
       01                 PC06  REDEFINES      PC00.                    CI0108
            10            PC06-XDBPCB.                                  CI0108
            11            PC06-XDBDNM PICTURE  X(08).                   CI0108
            11            PC06-XSEGLV PICTURE  X(02).                   CI0108
            11            PC06-XRC    PICTURE  X(02).                   CI0108
            11            PC06-XPROPT PICTURE  X(04).                   CI0108
            11            PC06-FILLER PICTURE  S9(5)                    CI0108
                          BINARY.                                       CI0108
            11            PC06-XSEGNM PICTURE  X(08).                   CI0108
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0108
                          BINARY.                                       CI0108
            11            PC06-XSEGNB PICTURE  9(05)                    CI0108
                          BINARY.                                       CI0108
            11            PC06-XCOKEY PICTURE  X(70).                   CI0108
      *** PCB MASK FOR ACAP                                             ADU015
      *!WF DSP=PD DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PD00.                                         CI0108
          05              PD00-SUITE.                                   CI0108
            15       FILLER         PICTURE  X(00106).                  CI0108
       01                 PD06  REDEFINES      PD00.                    CI0108
            10            PD06-XDBPCB.                                  CI0108
            11            PD06-XDBDNM PICTURE  X(08).                   CI0108
            11            PD06-XSEGLV PICTURE  X(02).                   CI0108
            11            PD06-XRC    PICTURE  X(02).                   CI0108
            11            PD06-XPROPT PICTURE  X(04).                   CI0108
            11            PD06-FILLER PICTURE  S9(5)                    CI0108
                          BINARY.                                       CI0108
            11            PD06-XSEGNM PICTURE  X(08).                   CI0108
            11            PD06-XKEYLN PICTURE  S9(05)                   CI0108
                          BINARY.                                       CI0108
            11            PD06-XSEGNB PICTURE  9(05)                    CI0108
                          BINARY.                                       CI0108
            11            PD06-XCOKEY PICTURE  X(70).                   CI0108

      *CAMS ACCOUNTING DATES RECORD PASSED TO CI0108
      *!WF DSP=NS DSL=NS SEL=20 FOR=I LEV=1 PLT=75
       01                 NS00.                                         CI0108
          05              NS00-00.                                      CI0108
            10            NS00-NS00K.                                   CI0108
            11            NS00-PRCSTK PICTURE  XX.                      CI0108
          05              NS00-SUITE.                                   CI0108
            15       FILLER         PICTURE  X(00078).                  CI0108
       01                 NS20  REDEFINES      NS00.                    CI0108
            10       FILLER         PICTURE  X(00002).                  CI0108
            10            NS20-DCACG  PICTURE  9(8).                    CI0108
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            NS20-CCDAT  PICTURE  X(8).                    CI0108
            10            NS20-DCALP  PICTURE  X(12).                   CI0108
            10            NS20-DNACG  PICTURE  9(8).                    CI0108
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            NS20-CNDAT  PICTURE  X(8).                    CI0108
            10            NS20-DNALP  PICTURE  X(12).                   CI0108
            10            NS20-DCACD  PICTURE  X(10).                   CI0108
            10            NS20-FILLER PICTURE  X(4).                    CI0108

      *PASS AREA TO/FROM CI0108
      *!WF DSP=PJ DSL=PJ SEL=47 FOR=I DES=1 LEV=1 PLT=75
       01                 PJ47.                                         CI0108
            10            PJ47-MAPPN  PICTURE  X(10).                   CI0108
            10            PJ47-CTID   PICTURE  X(27)                    CI0108
                          OCCURS       002     TIMES.                   CI0108
            10            PJ47-ICUST  PICTURE  X                        CI0108
                          OCCURS       002     TIMES.                   CI0108
            10            PJ47-IACFPD PICTURE  X(1).                    CI0108
            10            PJ47-AFEED  PICTURE  S9(5)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            PJ47-IWAIV  PICTURE  X.                       CI0108
            10            PJ47-AFEET  PICTURE  S9(5)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            PJ47-ITERF  PICTURE  X.                       CI0108
            10            PJ47-ATERF  PICTURE  S9(5)V99                 CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            PJ47-CCACT  PICTURE  99.                      CI0108
            10            PJ47-CPLTYP PICTURE  X(14).                   CI0108
            10            PJ47-CSPDT  PICTURE  X.                       CI0108
            10            PJ47-CPAYF  PICTURE  X(2).                    CI0108
            10            PJ47-FILLER PICTURE  X(47).                   CI0108

      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0108
          05              DE00-SUITE.                                   CI0108
            15       FILLER         PICTURE  X(00653).                  CI0108
       01                 DE10  REDEFINES      DE00.                    CI0108
            10            DE10-DU11.                                    CI0108
            11            DE10-XFONC  PICTURE  X(4).                    CI0108
            11            DE10-MPSBN  PICTURE  X(8).                    CI0108
            11            DE10-XDBDNM PICTURE  X(08).                   CI0108
            11            DE10-XSEGNM PICTURE  X(08).                   CI0108
            11            DE10-XRC    PICTURE  X(02).                   CI0108
            11            DE10-MSEG   PICTURE  X(08).                   CI0108
            11            DE10-XCOKEY PICTURE  X(70).                   CI0108
            11            DE10-CUIBR  PICTURE  X(01).                   CI0108
            11            DE10-CUIBA  PICTURE  X(01).                   CI0108
            11            DE10-IPBIK  PICTURE  X(1).                    CI0108
            10            DE10-DU03.                                    CI0108
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            DE10-CMSSF  PICTURE  XX.                      CI0108
            11            DE10-DU09.                                    CI0108
            12            DE10-CMESA  PICTURE  S9(9)                    CI0108
                          BINARY.                                       CI0108
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0108
                          BINARY.                                       CI0108
            12            DE10-CMESB  PICTURE  S9(9)                    CI0108
                          BINARY.                                       CI0108
            12            DE10-CMSST  PICTURE  S9(9)                    CI0108
                          BINARY.                                       CI0108
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0108
                          BINARY.                                       CI0108
            12            DE10-QELLAA PICTURE  S9(9)                    CI0108
                          BINARY.                                       CI0108
            12            DE10-TMESS4 PICTURE  X(512).                  CI0108
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0108
          05              MS00-SUITE.                                   CI0108
            15       FILLER         PICTURE  X(00542).                  CI0108
       01                 MS03  REDEFINES      MS00.                    CI0108
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            10            MS03-CMSSF  PICTURE  XX.                      CI0108
            10            MS03-DU09.                                    CI0108
            11            MS03-CMESA  PICTURE  S9(9)                    CI0108
                          BINARY.                                       CI0108
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0108
                          BINARY.                                       CI0108
            11            MS03-CMESB  PICTURE  S9(9)                    CI0108
                          BINARY.                                       CI0108
            11            MS03-CMSST  PICTURE  S9(9)                    CI0108
                          BINARY.                                       CI0108
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0108
                          BINARY.                                       CI0108
            11            MS03-QELLAA PICTURE  S9(9)                    CI0108
                          BINARY.                                       CI0108
            11            MS03-TMESS4 PICTURE  X(512).                  CI0108
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0108
            10            MX11-QMSGS  PICTURE  9(03).                   CI0108
            10            MX11-PJ09                                     CI0108
                          OCCURS       025     TIMES.                   CI0108
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0108
                          COMPUTATIONAL-3.                              CI0108
            11            MX11-CMESB  PICTURE  S9(9)                    CI0108
                          BINARY.                                       CI0108
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                NS20
                                PJ47
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0108
      *               *                                   *             CI0108
      *               *INITIALISATIONS                    *             CI0108
      *               *                                   *             CI0108
      *               *************************************.            CI0108
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
      *N02DA.    NOTE *---> INIT WORK AREA                *.
       F02DA.                                                           lv10
           INITIALIZE  7-WORK-INIT-FIELDS
           MOVE        'N' TO 7-FEE-PAID
           7-FEE-WAIVED
           7-IS-ACCT-TO-PROCESS
           INITIALIZE  TJ08.
       F02DA-FN. EXIT.
      *N02XA.    NOTE *SET POINTERS FOR DB ACCESS         *.
       F02XA.                                                           lv10
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR GR1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-GR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR ACAP                                             DOT
           SET ADDRESS OF PD06 TO                                       ADU015
                PCB-ACAP-PTR1.                                          ADU015
       F02XA-FN. EXIT.
      *N02YA.    NOTE *INITIALIZE RETURN FIELDS           *.
       F02YA.                                                           lv10
           MOVE        SPACE TO PJ47-ICUST (1)
           PJ47-ICUST (2)
           MOVE        'OTHER' TO PJ47-CPLTYP
           MOVE        'N' TO PJ47-IACFPD
           PJ47-IWAIV
           PJ47-ITERF
           MOVE        ZERO TO PJ47-AFEED
           PJ47-AFEET
           PJ47-ATERF
           PJ47-CCACT.
       F02YA-FN. EXIT.
       F02-FN.   EXIT.
      *N03.      NOTE *************************************.
      *               *                                   *
      *               *CURSOR DECLARATION                 *
      *               *                                   *
      *               *************************************.
       F03.           EXIT.                                             lv05
      *N03SU.    NOTE *DECLARE UQ04 CURSOR                *.
       F03SU.                                                           lv10
           EXEC SQL    DECLARE UQ04-CURSOR CURSOR FOR                   ADB201
                       SELECT                                           ADB201
                           FEE_CHARGED_AMT
                       FROM                                             ADB201
                           CORP.TBU304
                       WHERE                                            ADB201
                               N_CL_ID = :UQ04-CLID
                        AND SHELTER_ID = :UQ04-NSHID
                        AND FEE_CHARGED_YEAR =
                                         :UQ04-DFEEC
                        AND RTR_PLN_FEE_TYP_CD =
                                         :UQ04-CRFEE         END-EXEC.
       F03SU-FN. EXIT.
       F03-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0108
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0108
      *               *                                   *             CI0108
      *               *FIN DE TRAITEMENT                  *             CI0108
      *               *                                   *             CI0108
      *               *************************************.            CI0108
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0108
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
       F35.           EXIT.                                             lv05
      *N35CB.    NOTE *VALIDATE SOURCE CTID IS NUMERIC    *.
       F35CB.    IF    PJ47-CTID (1) NOT NUMERIC                        lv10
                 NEXT SENTENCE ELSE GO TO     F35CB-FN.
      *---> Send INVALID DATA Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012004 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35CB-FN. EXIT.
      *N35DB.    NOTE *VALIDATE DEST CTID IS NUMERIC      *.
       F35DB.    IF    PJ47-CTID (2) NOT NUMERIC                        lv10
                 NEXT SENTENCE ELSE GO TO     F35DB-FN.
      *IF SECOND ACCOUNT IS NOT NUMERIC
      *MOVE IN A ZERO
           MOVE        ZERO TO PJ47-CTID (2).
       F35DB-FN. EXIT.
      *N35DT.    NOTE *VALIDATE INPUT ACCTG DT FIELD      *.
       F35DT.                                                           lv10
           MOVE        NS20-DCACG TO DD01-XDATG
           PERFORM     F90DT THRU F90DT-FN.
                 IF    DEL-ER NOT = 1                                   DOT
      *INVALID CURRENT ACCOUNTING DATE
      *      and TERMINATE                                              ADU119
           MOVE        012786 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35DT-FN. EXIT.
       F35-FN.   EXIT.
      *N37.      NOTE *************************************.
      *               *                                   *
      *               ***** PROCESS FOR TERM FEES ****    *
      *               *                                   *
      *               *************************************.
       F37.           EXIT.                                             lv05
      *N37BD.    NOTE *GET ACCT AND GROUP INFO            *.
       F37BD.                                                           lv10
      *********************************
      *GU ON CT01 FOR SOURCE ACCT
           MOVE        PJ47-CTID (1) TO S-CTU01-CT01K
           PERFORM     F94DA THRU F94DA-FN.
                 IF    IK = '1'                                         DOT
      *CT01 NOT FOUND
      *---> Send ACCT NOT FND Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012077 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *SAVE SOURCE CTIDA                                                DOT
           MOVE        CT01-CTIDA TO 7-SRCE-CTIDA.
       F37BD-FN. EXIT.
      *N37BO.    NOTE *RETRIEVE SOURCE TAXPAYER           *.
       F37BO.                                                           lv10
      *GU ON CT09 USING BOOLEAN SSA
           PERFORM     F94DB THRU F94DB-FN
      *PULL TAXPAYER FROM KEY FEEDBACK
           MOVE        XW05-XCOKEY (28:23) TO
           7-TAXPAYER-CLID.
                 IF    IK = '1'                                         DOT
      *CT09 NOT FOUND
      *---> Send IMS ERROR Message                                      ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013403 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N37BS.    NOTE *READ FOR THE FIRST UNENDED CT10    *.
       F37BS.                                                           lv15
      *TO DETERMINE THE GROUP TYPE
      *GN ON CT10 USING BOOLEAN SSA TO
      *GET FIRST UNENDED CT10
           PERFORM     F94DC THRU F94DC-FN.
                 IF    IK = '1'                                         DOT
      *CT10 NOT FOUND
      *---> Send IMS ERROR Message                                      ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012391 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
           MOVE        CT10-GRID TO 7-SRCE-GRID.                        DOT
       F37BS-FN. EXIT.
      *N37BU.    NOTE *SOURCE ACCT IS A HOUSEHOLD ACCT    *.
       F37BU.    IF    7-SRCE-GRIDC = 001                               lv15
                 NEXT SENTENCE ELSE GO TO     F37BU-FN.
                 IF    CT01-CTCCI = '1'                                 DOT
                 OR    ((CT01-CTIDA = 004 OR 005)
                 AND   (CT01-CQACT = 1 OR 2 OR 3))
      *IT'S A CUSTODIAL ACCT:
      *SAVE THE ATTRIBUTES
           MOVE        CT01-CIRAT TO 7-SRCE-CIRAT
           MOVE        CT01-CIRAS TO 7-SRCE-CIRAS
           MOVE        CT01-CTCCI TO 7-SRCE-CTCCI
           MOVE        CT01-CQACT TO 7-SRCE-CQACT
           MOVE        'Y' TO 7-IS-ACCT-TO-PROCESS.
       F37BU-FN. EXIT.
      *N37BV.    NOTE *CUST OR TER FEE IS  NOT            *.
       F37BV.    IF    7-SRCE-CTIDA = 021                               lv15
                 AND   CT01-PRCOD = 00016 OR 00015
                 NEXT SENTENCE ELSE GO TO     F37BV-FN.
      *APPLICABLE FOR BRK SPS OR SPS
      *WITH ONE ACCOUNTS
           MOVE                     ALL '1' TO FT GO TO F20.
       F37BV-FN. EXIT.
      *N37BW.    NOTE *SOURCE ACCT IS A PENSION ACCT      *.
       F37BW.    IF    7-SRCE-GRIDC = 002                               lv15
                 NEXT SENTENCE ELSE GO TO     F37BW-FN.
      *GU ON GR07
           MOVE        7-SRCE-GRID TO S-GRU01-GR01K
           PERFORM     F94EB THRU F94EB-FN.
                 IF    IK = '1'                                         DOT
      *GR07 NOT FOUND
      *---> Send IMS ERROR Message                                      ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012228 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N37BX.    NOTE *IT'S A CUSTODIAL ACCT:             *.
       F37BX.    IF    GR07-CIDRP = 01                                  lv20
                 NEXT SENTENCE ELSE GO TO     F37BX-FN.
      *SAVE THE ATTRIBUTES
           MOVE        GR07-CIDRP TO 7-SRCE-CIDRP
           MOVE        GR07-GRPLT TO 7-SRCE-GRPLT
           MOVE        GR07-GRPLC TO 7-SRCE-GRPLC
           MOVE        'Y' TO 7-IS-ACCT-TO-PROCESS.
       F37BX-FN. EXIT.
       F37BW-FN. EXIT.
      *N37BY.    NOTE *SOURCE ACCOUNT NOT A CUST ACCT     *.
       F37BY.    IF    ACCT-TO-BE-SKIPPED                               lv15
                 NEXT SENTENCE ELSE GO TO     F37BY-FN.
           MOVE        'N' TO PJ47-ICUST (1)
           MOVE                     ALL '1' TO FT GO TO F20.
       F37BY-900. GO TO F37BZ-FN.
       F37BY-FN. EXIT.
      *N37BZ.    NOTE *FIRST ACCOUNT IS CUSTODIAL         *.
       F37BZ.                                                           lv15
           MOVE        'Y' TO PJ47-ICUST (1)
      *SET CCACT AND CPLTYP
           PERFORM     F92PA THRU F92PA-FN.
       F37BZ-FN. EXIT.
      *N37CA.    NOTE *UD                                 *.
       F37CA.    IF    PJ47-MAPPN = 'UD        '                        lv15
                 AND   PJ47-CPAYF = 'P '
                 AND   PJ47-CSPDT = 'Q'
                 NEXT SENTENCE ELSE GO TO     F37CA-FN.
      *ON 'CHOOSE DESTINATION', SELECT:
      *'EXTERNAL DEST: SPECIAL PAYEE'
      *
      *NO FEES
           MOVE                     ALL '1' TO FT GO TO F20.
       F37CA-FN. EXIT.
      *N37CB.    NOTE *CUST OR TER FEE IS NOT             *.
       F37CB.    IF    7-SRCE-CTIDA = 001                               lv15
                 OR    PJ47-MAPPN = 'EFU'
                 NEXT SENTENCE ELSE GO TO     F37CB-FN.
      *APPLICABLE FOR CERTS ACCOUNT AND
      *CERTS ACH-OUT, INTERNAL TRANSFER
      *BETWEEN CERTS AND BETA BROKERAGE
      *ACCOUNTS FROM MYFA
           MOVE                     ALL '1' TO FT GO TO F20.
       F37CB-FN. EXIT.
       F37BO-FN. EXIT.
      *N37CC.    NOTE *GET CUSTODIAL FEE AND TERM FEE     *.
       F37CC.                                                           lv10
      *********************************
      *GET SHELTER ID
           PERFORM     F94SH THRU F94SH-FN.
      *N37CD.    NOTE *READ TBU140 USING SHELTER ID       *.
       F37CD.                                                           lv15
      *TO GET SHELTER TYPE CODE
           PERFORM     F92KB THRU F92KB-FN.
                 IF    IK = '1'                                         DOT
      *ERROR HANDLING
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015701 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N37CF.    NOTE *READ TJ08 USING SHELTER TYPE       *.
       F37CF.                                                           lv20
      *CODE TO GET CUSTODIAL FEE AND
      *TERMINATION FEE
           PERFORM     F92QZ THRU F92QZ-FN.
       F37CF-FN. EXIT.
       F37CD-FN. EXIT.
       F37CC-FN. EXIT.
      *N37DA.    NOTE *CHARGE TERM FEE FOR SMTC/SMDD      *.
       F37DA.    IF    7-SRCE-GRIDC = 001                               lv10
                 AND   CT01-CTIDA = 002
                 AND   CT01-CQACT > 000
                 AND   PJ47-CTID (2) = ZERO
                 NEXT SENTENCE ELSE GO TO     F37DA-FN.
      *TRAN IF SOURCE IS A QUALIFIED
      *FUND ACCT IN HOUSEHOLD GROUP
      *N37DC.    NOTE *CHECK IF THE ACCT IS THE LAST      *.
       F37DC.                                                           lv15
      *ACTIVE ACCT IN HOUSEHOLD GROUP
      *N37DF.    NOTE *GET THE 1ST ACCOUNT IN GROUP       *.
       F37DF.                                                           lv20
      *********************************
           MOVE        7-SRCE-GRID TO S-GRU01-GR01K
           MOVE        'F----' TO 7-GRA19-1-CCOD
      *READ FIRST GR19 WITH GN READ
           PERFORM     F94EA THRU F94EA-FN
           MOVE        '-----' TO 7-GRA19-1-CCOD.
       F37DF-FN. EXIT.
      *N37DK.    NOTE **** CHECK EACH GR19 FOUND          *.
       F37DK.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F37DK-FN.
                 IF    PJ47-CTID (1) = GR19-GR19K                       DOT
      *IF SAME ACCT WAS FOUND, SKIP AND
      *READ THE NEXT ACCT (GR19)
           PERFORM     F94EA THRU F94EA-FN
               GO TO     F37DK-900.
      *N37DL.    NOTE *GET ACCOUNT INFORMATION            *.
       F37DL.                                                           lv25
      *GU CT01
           MOVE        GR19-GR19K TO S-CTU01-CT01K
           PERFORM     F94DA THRU F94DA-FN.
                 IF    IK = '1'                                         DOT
      *CT01 NOT FOUND THEN READ THE
      *NEXT ACCT (GR19)
           PERFORM     F94EA THRU F94EA-FN
               GO TO     F37DK-900.
       F37DL-FN. EXIT.
      *N37DO.    NOTE *DO NOT DETERMINE TERM FEE IF       *.
       F37DO.    IF    CT01-CTSTA = 02                                  lv25
                 NEXT SENTENCE ELSE GO TO     F37DO-FN.
      *ANOTHER ACTIVE ACCT WAS FOUND
               GO TO     F37DA-FN.
       F37DO-FN. EXIT.
      *N37DR.    NOTE *GET NEXT GR19                      *.
       F37DR.                                                           lv25
           PERFORM     F94EA THRU F94EA-FN.
       F37DR-FN. EXIT.
       F37DK-900. GO TO F37DK.
       F37DK-FN. EXIT.
       F37DC-FN. EXIT.
      *N37DT.    NOTE *READ TBU304 TO SEE IF TERM         *.
       F37DT.                                                           lv15
      *FEE ALREADY PAID
           PERFORM     F95EB THRU F95EB-FN.
                 IF    7-FEE-PAID = 'Y'                                 DOT
      *TERM FEE HAS BEEN PAID
               GO TO     F37DA-FN.
       F37DT-FN. EXIT.
      *N37GB.    NOTE *POPULATE TERM FEES                 *.
       F37GB.                                                           lv15
           MOVE        TJ08-AFEETK TO PJ47-ATERF
      *SET TERM FEE INDICATOR
                 IF    PJ47-ATERF > ZERO                                DOT
           MOVE        'Y' TO PJ47-ITERF.
       F37GB-FN. EXIT.
       F37DA-FN. EXIT.
       F37-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *** PROCESS FOR CUSTODIAL FEES **   *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *IF ONE ACCOUNT HAS BEEN SENT,
      *THIS PROGRAM WILL CHECK IF IT IS
      *A CUSTODIAL ACCOUNT AND IF
      *UNPAID FEES REMAIN.
      *
      *IF TWO ACCOUNTS HAVE BEEN SENT,
      *THE TYPE OF BOTH WILL BE FOUND
      *AND CUSTODIAL FEES WILL BE
      *DETERMINED ASSUMING A TRANSFER
      *OF ALL FUNDS FROM THE 1ST ACCT
      *TO THE 2ND ACCT.
      *N40BD.    NOTE *DETERMINE ATTRIBUTES OF 1ST ACCT   *.
       F40BD.                                                           lv10
      *IF THE SOURCE ACCOUNT IS NOT A
      *CUSTODIAL ACCOUNT (EITHER HOUSE-
      *HOLD OR PENSION) THEN NO FURTHER
      *PROCESSING IS NECESSARY - RETURN
      *TO CALLING PROGRAM.
      *GU ON CT01 FOR SOURCE ACCT
           MOVE        PJ47-CTID (1) TO S-CTU01-CT01K
           PERFORM     F94DA THRU F94DA-FN.
                 IF    IK = '1'                                         DOT
      *CT01 NOT FOUND
      *---> Send ACCT NOT FND Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012077 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40BD-FN. EXIT.
      *N40FB.    NOTE *SEE IF CUST FEE ALREADY PAID OR    *.
       F40FB.                                                           lv10
      *WAIVED
      *THE ASSUMPTION HERE IS THAT THE
      *DISTRIBUTION BEING MADE WILL BE
      *A FULL DISTRIBUTION.
      *READ TBU303/4 TO SEE IF CUST
      *FEE ALREADY PAID OR WAIVED
           PERFORM     F95DA THRU F95DA-FN.
                 IF    7-FEE-PAID = 'Y'                                 DOT
                 OR    7-FEE-WAIVED = 'Y'
      *CUST FEE PAID OR WAIVED
           MOVE        'Y' TO PJ47-IACFPD.
       F40FB-FN. EXIT.
      *N40GB.    NOTE *HAS DESTINATION ACCOUNT            *.
       F40GB.    IF    PJ47-CTID (2) > ZERO                             lv10
                 NEXT SENTENCE ELSE GO TO     F40GB-FN.
      *GU ON CT01 FOR DEST ACCT
           MOVE        PJ47-CTID (2) TO S-CTU01-CT01K
           PERFORM     F94DA THRU F94DA-FN.
                 IF    IK = '1'                                         DOT
      *CT01 NOT FOUND
      *---> Send ACCT NOT FND Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012078 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *SAVE DEST CTIDA                                                  DOT
           MOVE        CT01-CTIDA TO 7-DEST-CTIDA.
      *N40GF.    NOTE *READ FOR THE FIRST UNENDED CT10    *.
       F40GF.                                                           lv15
      *TO DETERMINE THE GROUP TYPE
      *GN ON CT10 USING BOOLEAN SSA TO
      *GET FIRST UNENDED CT10
           PERFORM     F94DC THRU F94DC-FN.
                 IF    IK = '1'                                         DOT
      *CT10 NOT FOUND
      *---> Send IMS ERROR Message                                      ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012391 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
           MOVE        CT10-GRID TO 7-DEST-GRID.                        DOT
       F40GF-FN. EXIT.
      *N40GH.    NOTE *DEST ACCT IS A HOUSEHOLD ACCT      *.
       F40GH.    IF    7-DEST-GRIDC = 001                               lv15
                 NEXT SENTENCE ELSE GO TO     F40GH-FN.
                 IF    CT01-CTCCI = '1'                                 DOT
                 OR    ((CT01-CTIDA = 004 OR 005)
                 AND   (CT01-CQACT = 1 OR 2 OR 3))
      *IT'S A CUSTODIAL ACCT:
      *SAVE THE ATTRIBUTES
           MOVE        CT01-CIRAT TO 7-DEST-CIRAT
           MOVE        CT01-CIRAS TO 7-DEST-CIRAS
           MOVE        CT01-CTCCI TO 7-DEST-CTCCI
           MOVE        CT01-CQACT TO 7-DEST-CQACT
           MOVE        'Y' TO PJ47-ICUST (2).
       F40GH-FN. EXIT.
      *N40GM.    NOTE *DEST ACCT IS A PENSION ACCT        *.
       F40GM.    IF    7-DEST-GRIDC = 002                               lv15
                 NEXT SENTENCE ELSE GO TO     F40GM-FN.
      *GU ON GR07
           MOVE        7-DEST-GRID TO S-GRU01-GR01K
           PERFORM     F94EB THRU F94EB-FN.
                 IF    IK = '1'                                         DOT
      *GR07 NOT FOUND
      *---> Send IMS ERROR Message                                      ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012228 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N40GO.    NOTE *IT'S A CUSTODIAL ACCT:             *.
       F40GO.    IF    GR07-CIDRP = 01                                  lv20
                 NEXT SENTENCE ELSE GO TO     F40GO-FN.
      *SAVE ATTRIBUTES
           MOVE        GR07-CIDRP TO 7-DEST-CIDRP
           MOVE        GR07-GRPLT TO 7-DEST-GRPLT
           MOVE        GR07-GRPLC TO 7-DEST-GRPLC
           MOVE        'Y' TO PJ47-ICUST (2).
       F40GO-FN. EXIT.
       F40GM-FN. EXIT.
      *N40GT.    NOTE *RETRIEVE DESTINATION TAXPAYER      *.
       F40GT.                                                           lv15
      *GU ON CT09 USING BOOLEAN SSA
           MOVE        PJ47-CTID (2) TO S-CTU01-CT01K
           PERFORM     F94DB THRU F94DB-FN
      *PULL TAXPAYER FROM KEY FEEDBACK
           MOVE        XW05-XCOKEY (28:23) TO
           7-DESTINATION-CLID.
                 IF    IK = '1'                                         DOT
                 AND   CT01-CTIDA NOT = 013
      *CT09 NOT FOUND
      *---> Send IMS ERROR Message                                      ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013403 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40GT-FN. EXIT.
      *N40HA.    NOTE *SOURCE & DEST ACCTS BOTH ED IRA.   *.
       F40HA.    IF    7-SRCE-HH-ATTR =                                 lv15
                       7-DEST-HH-ATTR
                 AND   7-SRCE-CIRAT = 007
                 NEXT SENTENCE ELSE GO TO     F40HA-FN.
      *NO FEES.
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HA-FN. EXIT.
      *N40HB.    NOTE *SAME TAXPAYER                      *.
       F40HB.    IF    7-TAXPAYER-CLID =                                lv15
                       7-DESTINATION-CLID
                 AND   CT01-CTIDA NOT = 013
                 NEXT SENTENCE ELSE GO TO     F40HB-FN.
      *NOT A FINANCIAL PLAN (DON'T HAVE
      *A TAXPAYER)
      *N40HD.    NOTE *SOURCE & DEST IN SAME HH GROUP     *.
       F40HD.    IF    7-SRCE-GRIDC = 001                               lv20
                 AND   7-SRCE-GRID = 7-DEST-GRID
                 NEXT SENTENCE ELSE GO TO     F40HD-FN.
      *N40HF.    NOTE *IF SOURCE ACCOUNT ATTRIBUTES &     *.
       F40HF.    IF    7-SRCE-HH-ATTR =                                 lv25
                       7-DEST-HH-ATTR
                 NEXT SENTENCE ELSE GO TO     F40HF-FN.
      *DESTINATION ACCOUNT ATTRIBUTES
      *ARE THE SAME, NO FEES - GET OUT.
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HF-FN. EXIT.
       F40HD-FN. EXIT.
      *N40HM.    NOTE *SOURCE & DEST IN SAME PENS GROUP   *.
       F40HM.    IF    7-SRCE-GRIDC = 002                               lv20
                 AND   7-SRCE-GRID = 7-DEST-GRID
                 NEXT SENTENCE ELSE GO TO     F40HM-FN.
      *N40HO.    NOTE *IF SOURCE ACCOUNT ATTRIBUTES &     *.
       F40HO.    IF    7-SRCE-PEN-ATTR =                                lv25
                       7-DEST-PEN-ATTR
                 NEXT SENTENCE ELSE GO TO     F40HO-FN.
      *DESTINATION ACCOUNT ATTRIBUTES
      *ARE THE SAME, NO FEES - GET OUT.
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HO-FN. EXIT.
       F40HM-FN. EXIT.
      *N40HQ.    NOTE *SOURCE IS ACTIVE INDIVIDUAL IRA    *.
       F40HQ.    IF    7-SRCE-CIRAS = 001                               lv20
                 AND   7-SRCE-CIRAT = 001
                 NEXT SENTENCE ELSE GO TO     F40HQ-FN.
                 IF    7-DEST-CIRAT = 003                               DOT
                 AND   7-DEST-CTIDA NOT = 004 AND
                       005
      *DEST IS SEP
      *(EXCLUDE ANNUITIES)
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 006
                 AND   7-DEST-CTIDA NOT = 004 AND
                       005
      *DEST IS ACTIVE ROTH CONV
      *(EXCLUDE ANNUITIES)
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAS = 003                               DOT
                 AND   7-DEST-CIRAT = 001
      *DEST IS BENE INDIVIDUAL IRA
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HQ-FN. EXIT.
      *N40HR.    NOTE *SOURCE IS ROLLOVER                 *.
       F40HR.    IF    7-SRCE-CIRAS = 002                               lv20
                 NEXT SENTENCE ELSE GO TO     F40HR-FN.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 001
                 AND   7-DEST-CTIDA NOT = 004 AND
                       005
      *DEST IS ACTIVE INDIVIDUAL IRA
      *(EXCLUDE ANNUITIES)
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 006
                 AND   CT01-CTIDA NOT = 004 AND
                       005
      *DEST IS ACTIVE ROTH CONV
      *(EXCLUDE ANNUITIES)
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAS = 003                               DOT
                 AND   7-DEST-CIRAT = 001
      *DEST IS BENE INDIVIDUAL IRA
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HR-FN. EXIT.
      *N40HS.    NOTE *SOURCE IS SEP                      *.
       F40HS.    IF    7-SRCE-CIRAT = 003                               lv20
                 NEXT SENTENCE ELSE GO TO     F40HS-FN.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 001
                 AND   7-DEST-CTIDA NOT = 004 AND
                       005
      *DEST IS ACTIVE INDIVIDUAL IRA
      *(EXCLUDE ANNUITIES)
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 006
                 AND   7-DEST-CTIDA NOT = 004 AND
                       005
      *DEST IS ACTIVE ROTH CONV
      *(EXCLUDE ANNUITIES)
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAS = 003                               DOT
                 AND   7-DEST-CIRAT = 001
      *DEST IS BENE INDIVIDUAL IRA
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HS-FN. EXIT.
      *N40HT.    NOTE *SOURCE IS SRA                      *.
       F40HT.    IF    7-SRCE-CIRAT = 004                               lv20
                 NEXT SENTENCE ELSE GO TO     F40HT-FN.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 001
                 AND   7-DEST-CTIDA NOT = 004 AND
                       005
      *DEST IS ACTIVE INDIVIDUAL IRA
      *(EXCLUDE ANNUITIES)
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 006
                 AND   7-DEST-CTIDA NOT = 004 AND
                       005
      *DEST IS ACTIVE ROTH CONV
      *(EXCLUDE ANNUITIES)
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAS = 003                               DOT
                 AND   7-DEST-CIRAT = 001
      *DEST IS BENE INDIVIDUAL IRA
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HT-FN. EXIT.
      *N40HU.    NOTE *SOURCE IS ACTIVE ROTH CONTRIB      *.
       F40HU.    IF    7-SRCE-CIRAS = 001                               lv20
                 AND   7-SRCE-CIRAT = 005
                 NEXT SENTENCE ELSE GO TO     F40HU-FN.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 006
                 AND   7-DEST-CTIDA NOT = 004 AND
                       005
      *DEST IS ACTIVE ROTH CONV
      *(EXCLUDE ANNUITIES)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HU-FN. EXIT.
      *N40HV.    NOTE *SOURCE IS BENE ROTH CONTRIB        *.
       F40HV.    IF    7-SRCE-CIRAS = 003                               lv20
                 AND   7-SRCE-CIRAT = 005
                 NEXT SENTENCE ELSE GO TO     F40HV-FN.
                 IF    7-DEST-CIRAS = 003                               DOT
                 AND   7-DEST-CIRAT = 006
      *DEST IS BENEFICIAL ROTH CONV
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HV-FN. EXIT.
       F40HB-FN. EXIT.
       F40GB-FN. EXIT.
      *N40JB.    NOTE *EXAMINE OTHER ACCOUNTS IN SAME     *.
       F40JB.                                                           lv10
      *GROUP AS SOURCE ACCOUNT
      *REPOSITION TO FIRST GR19 IN THE
      *SOURCE GROUP
           MOVE        7-SRCE-GRID TO S-GRU01-GR01K
           MOVE        'F----' TO 7-GRA19-1-CCOD
      *READ FIRST GR19 WITH GN READ
           PERFORM     F94EA THRU F94EA-FN
           MOVE        '-----' TO 7-GRA19-1-CCOD.
      *N40JD.    NOTE **** CHECK EACH GR19 FOUND          *.
       F40JD.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F40JD-FN.
                 IF    PJ47-CTID (1) = GR19-GR19K                       DOT
      *IF SAME ACCT AS SOURCE, SKIP
           PERFORM     F94EA THRU F94EA-FN
               GO TO     F40JD-900.
           MOVE        GR19-CTIDA TO 7-GR19-CTIDA.
      *N40JE.    NOTE *ACCTS WE WANT TO LOOK AT           *.
       F40JE.    IF    CERT                                             lv20
                 OR    FUND
                 OR    SECURITIES
                 NEXT SENTENCE ELSE GO TO     F40JE-FN.
      *GU CT01
           MOVE        GR19-GR19K TO S-CTU01-CT01K
           PERFORM     F94DA THRU F94DA-FN.
                 IF    IK = '1'                                         DOT
      *CT01 NOT FOUND
           PERFORM     F94EA THRU F94EA-FN
               GO TO     F40JD-900.
      *N40JI.    NOTE *ACCOUNT ACTIVE AS OF LAST NIGHT    *.
       F40JI.    IF    CT01-CTSTA = 02                                  lv25
                 NEXT SENTENCE ELSE GO TO     F40JI-FN.
           MOVE        ZERO TO 7-OTHER-CLID
      *BOOLEAN READ OF CT09 (GN)
           PERFORM     F94DD THRU F94DD-FN.
                 IF    IK = '1'                                         DOT
      *CT09 NOT FOUND
      *---> Send IMS ERROR Message                                      ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013403 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *PULL TAXPAYER FROM KEY FEEDBACK                                  DOT
           MOVE        XW05-XCOKEY (28:23) TO
           7-OTHER-CLID.
      *N40JJ.    NOTE *SAME TAXPAYER                      *.
       F40JJ.    IF    7-TAXPAYER-CLID =                                lv30
                       7-OTHER-CLID
                 NEXT SENTENCE ELSE GO TO     F40JJ-FN.
      *N40JL.    NOTE *SOURCE IS A HOUSEHOLD ACCT.        *.
       F40JL.    IF    7-SRCE-GRIDC = 001                               lv35
                 NEXT SENTENCE ELSE GO TO     F40JL-FN.
      *CK IF OTHER ACCT IS IN SAME PLAN
                 IF    (CT01-CTCCI NOT =                                DOT
                       7-SRCE-CTCCI)
                 OR    (CT01-CQACT NOT =
                       7-SRCE-CQACT)
                 OR    (CT01-CIRAT NOT =
                       7-SRCE-CIRAT)
                 OR    (CT01-CIRAS NOT =
                       7-SRCE-CIRAS)
      *OTHER ACCT NOT IN SAME PLAN
               GO TO     F40JE-FN.
       F40JL-FN. EXIT.
      *N40JO.    NOTE *IF ACTIVE (AS OF LAST NIGHT) 021   *.
       F40JO.    IF    PJ47-MAPPN NOT = 'FDC'                           lv35
                 AND   SECURITIES
                 NEXT SENTENCE ELSE GO TO     F40JO-FN.
      *AND 133 ACCOUNTS IN GROUP, NO
      *FEES RETURN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JO-FN. EXIT.
      *N40JP.    NOTE *FDC EDIT FOR 001/002/021/133       *.
       F40JP.    IF    PJ47-MAPPN = 'FDC'                               lv35
                 NEXT SENTENCE ELSE GO TO     F40JP-FN.
      *ACCOUNTS IN THE GROUP.
      *PERFORM ADDITIONAL CHECKS.                                       DOT
      *--------------------------------.                                DOT
      *ACCESS TA5A TO GET PROD INFO                                     DOT
           INITIALIZE  TA5A
           MOVE        CT01-CTIDA TO TA5A-CTIDA
           MOVE        CT01-PRCOD TO TA5A-PRCOD.
      *FOR FUNDS ONLY - MOVE SUB-ACCT                                   DOT
                 IF    CT01-CTIDA = 002                                 DOT
           MOVE        CT01-PRSCD TO TA5A-PRSCD.
           PERFORM     F92TA THRU F92TA-FN.                             DOT
      *SET MAJOR PRODUCT TYPE FLAGS                                     DOT
           MOVE        TA5A-CPRDG TO 7-CPRDG
           MOVE        TA5A-CPRDA1 TO 7-CPRDA1.
                 IF    7-SECURITIES-GROUP                               DOT
      *CHECK IF THE ACCT IS 021 OR 133,
      *NO FEES RETURN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JP-FN. EXIT.
      *N40JX.    NOTE *READ THRU ACTIVITY DB              *.
       F40JX.                                                           lv35
           MOVE        'N' TO 7-FULL-REDEMP
           7-FEES-TAKEN
      *CALL TO CI0100
           PERFORM     F90GA THRU F90GA-FN.
                 IF    7-FEES-TAKEN = 'Y'                               DOT
      *FEES ALREADY DETERMINED & TAKEN
      *FROM ANOTHER ACCT TODAY.
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-FULL-REDEMP = 'Y'                              DOT
      *THERE'S A PENDING FULL REDEMP.
      *CONSIDER ACCT TO BE NOT ACTIVE.
               GO TO     F40JE-FN.
      *IF WE GET HERE, ACCT IN SAME                                     DOT
      *GROUP IS STILL ACTIVE.
      *NO FEES DUE.
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JX-FN. EXIT.
       F40JJ-FN. EXIT.
       F40JI-FN. EXIT.
       F40JE-FN. EXIT.
      *N40JZ.    NOTE *READ NEXT GR19 SEGMENT             *.
       F40JZ.                                                           lv20
           PERFORM     F94EA THRU F94EA-FN.
       F40JZ-FN. EXIT.
       F40JD-900. GO TO F40JD.
       F40JD-FN. EXIT.
       F40JB-FN. EXIT.
      *N40MA.    NOTE *CUST FEE NOT PAID OR WAIVED        *.
       F40MA.    IF    PJ47-IACFPD = 'N'                                lv10
                 NEXT SENTENCE ELSE GO TO     F40MA-FN.
      *N40MB.    NOTE *CUST FEE FOR HOUSEHOLD GROUP       *.
       F40MB.    IF    7-SRCE-GRIDC = 001                               lv15
                 NEXT SENTENCE ELSE GO TO     F40MB-FN.
           MOVE        TJ08-AFEETQ TO PJ47-AFEET.
       F40MB-FN. EXIT.
      *N40MD.    NOTE *CUST FEE FOR PENSION GROUP         *.
       F40MD.    IF    7-SRCE-GRIDC = 002                               lv15
                 NEXT SENTENCE ELSE GO TO     F40MD-FN.
           MOVE        30 TO PJ47-AFEET.
       F40MD-FN. EXIT.
       F40MA-FN. EXIT.
       F40-FN.   EXIT.
      *N41.      NOTE *************************************.
      *               *                                   *
      *               *PROCESS TERM FEES FOR PENSION      *
      *               *                                   *
      *               *************************************.
       F41.      IF    7-SRCE-GRIDC = 002                               lv05
                 NEXT SENTENCE ELSE GO TO     F41-FN.
      *GROUP
      *AS WITH THE CUSTODIAL FEES,
      *TERMINATION FEES WILL ALSO BE
      *DETERMINED WITH THE ASSUMPTION
      *THAT ALL FUNDS WILL BE REMOVED
      *FROM THE 1ST ACCT.
      *N41CC.    NOTE *SOURCE IS ACTIVE INDIVIDUAL IRA    *.
       F41CC.    IF    7-SRCE-CIRAS = 001                               lv10
                 AND   7-SRCE-CIRAT = 001
                 NEXT SENTENCE ELSE GO TO     F41CC-FN.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 001
                 AND   (7-DEST-CTIDA = 004 OR 005)
      *DEST IS ACTIVE INDIVIDUAL IRA
      *ANNUITY
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAT = 003                               DOT
                 AND   (7-DEST-CTIDA = 004 OR 005)
      *DEST IS SEP ANNUITY
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 006
                 AND   (7-DEST-CTIDA = 004 OR 005)
      *DEST IS ACTIVE ROTH CONV ANNUITY
           MOVE                     ALL '1' TO FT GO TO F20.
       F41CC-FN. EXIT.
      *N41CG.    NOTE *SOURCE IS ROLLOVER                 *.
       F41CG.    IF    7-SRCE-CIRAS = 002                               lv10
                 NEXT SENTENCE ELSE GO TO     F41CG-FN.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 001
                 AND   (7-DEST-CTIDA = 004 OR 005)
      *DEST IS ACTIVE INDIVIDUAL IRA
      *ANNUITY
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAS = 002                               DOT
                 AND   (7-DEST-CTIDA = 004 OR 005)
      *DEST IS ROLLOVER ANNUITY
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 006
                 AND   (7-DEST-CTIDA = 004 OR 005)
      *DEST IS ACTIVE ROTH CONV ANNUITY
           MOVE                     ALL '1' TO FT GO TO F20.
       F41CG-FN. EXIT.
      *N41CK.    NOTE *SOURCE IS SEP                      *.
       F41CK.    IF    7-SRCE-CIRAT = 003                               lv10
                 NEXT SENTENCE ELSE GO TO     F41CK-FN.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 001
                 AND   (7-DEST-CTIDA = 004 OR 005)
      *DEST IS ACTIVE INDIVIDUAL IRA
      *ANNUITY
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAT = 003                               DOT
                 AND   (7-DEST-CTIDA = 004 OR 005)
      *DEST IS SEP ANNUITY
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 006
                 AND   (7-DEST-CTIDA = 004 OR 005)
      *DEST IS ACTIVE ROTH CONV ANNUITY
           MOVE                     ALL '1' TO FT GO TO F20.
       F41CK-FN. EXIT.
      *N41CO.    NOTE *SOURCE IS SRA                      *.
       F41CO.    IF    7-SRCE-CIRAT = 004                               lv10
                 NEXT SENTENCE ELSE GO TO     F41CO-FN.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 001
                 AND   (7-DEST-CTIDA = 004 OR 005)
      *DEST IS ACTIVE INDIVIDUAL IRA
      *ANNUITY
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAT = 004                               DOT
                 AND   (7-DEST-CTIDA = 004 OR 005)
      *DEST IS SRA ANNUITY
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 006
                 AND   (7-DEST-CTIDA = 004 OR 005)
      *DEST IS ACTIVE ROTH CONV ANNUITY
           MOVE                     ALL '1' TO FT GO TO F20.
       F41CO-FN. EXIT.
      *N41CS.    NOTE *SOURCE IS ACTIVE ROTH CONTRIB      *.
       F41CS.    IF    7-SRCE-CIRAS = 001                               lv10
                 AND   7-SRCE-CIRAT = 005
                 NEXT SENTENCE ELSE GO TO     F41CS-FN.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 005
                 AND   (7-DEST-CTIDA = 004 OR 005)
      *DEST IS ACTIVE ROTH CONTRIB
      *ANNUITY
           MOVE                     ALL '1' TO FT GO TO F20.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 006
                 AND   (7-DEST-CTIDA = 004 OR 005)
      *DEST IS ACTIVE ROTH CONV ANNUITY
           MOVE                     ALL '1' TO FT GO TO F20.
       F41CS-FN. EXIT.
      *N41CU.    NOTE *SOURCE IS ACTIVE ROTH CONVERSION   *.
       F41CU.    IF    7-SRCE-CIRAS = 001                               lv10
                 AND   7-SRCE-CIRAT = 006
                 NEXT SENTENCE ELSE GO TO     F41CU-FN.
                 IF    7-DEST-CIRAS = 001                               DOT
                 AND   7-DEST-CIRAT = 006
                 AND   (7-DEST-CTIDA = 004 OR 005)
      *DEST IS ACTIVE ROTH CONV ANNUITY
           MOVE                     ALL '1' TO FT GO TO F20.
       F41CU-FN. EXIT.
      *N41DA.    NOTE *CHECK AGE & CONDITION OF TAXPAYR   *.
       F41DA.                                                           lv10
      *GU ON CL01
           MOVE        7-TAXPAYER-CLID TO S-CLU01-CL01K
           PERFORM     F94CA THRU F94CA-FN.
                 IF    IK = '1'                                         DOT
      *CL01 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012012 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N41DF.    NOTE *TAXPAYER IS A PERSON               *.
       F41DF.    IF    CL01-CLTYP = 'P'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F41DF-FN.
      *GN ON CL03
           PERFORM     F94CB THRU F94CB-FN.
                 IF    IK = '1'                                         DOT
      *CL03 NOT FOUND
      *---> Send IMS ERROR Message                                      ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012373 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N41DH.    NOTE *CLIENT IS DEAD; NO TERM FEE        *.
       F41DH.    IF    CL03-CLDOD NOT = ZERO                            lv20
                 NEXT SENTENCE ELSE GO TO     F41DH-FN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F41DH-FN. EXIT.
       F41DF-FN. EXIT.
       F41DA-FN. EXIT.
      *N41FB.    NOTE *SEE IF TERM FEE ALREADY PAID       *.
       F41FB.                                                           lv10
      *THE ASSUMPTION HERE IS THAT THE
      *DISTRIBUTION BEING MADE WILL BE
      *A FULL DISTRIBUTION.
      *READ TBU304 TO SEE IF TERM
      *FEE ALREADY PAID
           PERFORM     F95EB THRU F95EB-FN.
                 IF    7-FEE-PAID = 'Y'                                 DOT
      *TERM FEE HAS BEEN PAID
           MOVE                     ALL '1' TO FT GO TO F20.
       F41FB-FN. EXIT.
      *N41FJ.    NOTE *SOURCE ACCT IN PENS GROUP          *.
       F41FJ.                                                           lv10
      *CALCULATE A DATE 59.5 YRS BACK
      *FROM CURRENT BUSINESS DATE
           MOVE        590000 TO 7-NO-OF-YEARS
           PERFORM     F90CB THRU F90CB-FN.
                 IF    CL01-CLTYP = 'P'                                 DOT
                 AND   CL03-CLDOB NOT >
                       7-WS-YRS-BACK
      *NO TERM FEE IF TAXPAYER IS 59.5
      *OR OLDER
           MOVE                     ALL '1' TO FT GO TO F20.
      *SET TERM FEE TO $25                                              DOT
           MOVE        'Y' TO PJ47-ITERF
           MOVE        25 TO PJ47-ATERF
           MOVE                     ALL '1' TO FT GO TO F20.
       F41FJ-FN. EXIT.
       F41-FN.   EXIT.
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
      *               *PERFORMED ROUTINES                 *
      *               *                                   *
      *               *************************************.
       F90.           EXIT.                                             lv05
      *N90CB.    NOTE *CALC DATE 59.5 OR 70.5 YRS BACK    *.
       F90CB.                                                           lv10
           MOVE        NS20-DCACG TO 7-WS-YRS-BACK
      *SUBTRACT WHOLE YEARS FROM BUS DT
           SUBTRACT    7-NO-OF-YEARS FROM 7-WS-YRS-BACK
      *NOW USE MACRO AADA71 TO SUBTRACT
      *ANOTHER 183 DAYS
           PERFORM     F96DG THRU F96DG-FN.
       F90CB-FN. EXIT.
      *N90DT.    NOTE *DATE VALIDATION                    *.            AADA56
       F90DT.                                                           lv10
           MOVE        1 TO DEL-ER.                                     AADA56
                 IF    DD01-XDATG NOT NUMERIC                           DOT
           MOVE        4 TO DEL-ER                                      AADA56
               GO TO     F90DT-FN.                                      AADA56
                 IF    DD01-XDAT1 > '99'                                DOT
                 OR    DD01-XDAT1 < '18'                                AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F90DT-FN.                                      AADA56
                 IF    DD01-XDAT3 > '12'                                DOT
                 OR    DD01-XDAT3 = '00'                                AADA56
                 OR    DD01-XDAT4 > '31'                                AADA56
                 OR    DD01-XDAT4 = '00'                                AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F90DT-FN.                                      AADA56
                 IF    DD01-XDAT4 > '30'                                DOT
                 AND   (DD01-XDAT3 = '04'                               AADA56
                 OR    DD01-XDAT3 = '06'                                AADA56
                 OR    DD01-XDAT3 = '09'                                AADA56
                 OR    DD01-XDAT3 = '11')                               AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F90DT-FN.                                      AADA56
                 IF    DD01-XDAT3 NOT = '02'                            DOT
               GO TO     F90DT-FN.                                      AADA56
                 IF    DD01-XDAT4 > '29'                                DOT
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F90DT-FN.                                      AADA56
           MOVE        DD01-XDAT29 TO DD01-DTGYY                        DOT
           MOVE        DD01-XDAT19 TO DD01-DTGCC.                       AADA56
                 IF    DD01-DTGYY NOT = ZERO                            DOT
           COMPUTE     DD01-XLEAPY = DD01-DTGCY -                       AADA56
           ((DD01-DTGCY / 4) * 4)                                       AADA56
                 ELSE                                                   AADA56
           COMPUTE     DD01-XLEAPY = (DD01-DTGCY -                      AADA56
           ((DD01-DTGCY / 400) * 400))                                  AADA56
           / 100.                                                       AADA56
                 IF    DD01-XLEAPY NOT = ZERO                           DOT
                 AND   DD01-XDAT4 > '28'                                AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F90DT-FN.                                      AADA56
       F90DT-FN. EXIT.
      *N90GA.    NOTE *PREP FOR & CALL CI0100             *.
       F90GA.                                                           lv10
           INITIALIZE  7-GC00-AREA
           MOVE        PJ47-MAPPN TO 7-GC00-MAPPN
           MOVE        'GNP' TO 7-GC00-CFUNC
      *ACTIVITY STATUS OF UNPROCESSED,
      *PULLED FOR PROD SYSTEM,
      *SUSPENSED
           MOVE        01 TO 7-GC00-CASTC (1)
           MOVE        02 TO 7-GC00-CASTC (2)
           MOVE        04 TO 7-GC00-CASTC (3)
      *JUST WANT DISBURSEMENTS
           MOVE        001 TO 7-GC00-CAATY (1)
           MOVE        CT01-CTID TO 7-GC00-CTID.
      *N90GB.    NOTE *LOOP THRU ACTIVITIES               *.
       F90GB.                       GO TO     F90GB-B.                  lv15
       F90GB-A.
                 IF    PK41-IENDP = 'Y'
                                    GO TO     F90GB-FN.
       F90GB-B.
      *- DO UNTIL CI0100 HAS RETRIEVED
      *  ALL ROWS.
      *- MAY CALL CI0100 MORE THAN ONCE
      *  TO GET ALL ROWS.
      *- AT BOTTOM OF LOOP, PK41-IENDP
      *  IS CHECKED TO DETERMINE IF
      *  ANOTHER CALL IS NEEDED, & IF
      *  SO, SET UP RECALL KEY.
      *N90GC.    NOTE *CALL CI0100 - ACCESS ACTIVITY      *.            AM0100
       F90GC.                                                           lv20
      *********************************                                 AM0100
      ** THIS MODULE WILL ACCESS THE  *                                 AM0100
      ** ACTIVITY DATABASE AND        *                                 AM0100
      ** RETRIEVE 1 TO 10 ACTIVITIES  *                                 AM0100
      *********************************                                 AM0100
           INITIALIZE  PK40                                             AM0100
           MOVE        7-GC00-MAPPN TO PK40-MAPPN                       AM0100
           MOVE        7-GC00-CFUNC TO PK40-CFUNC                       AM0100
           MOVE        7-GC00-CASTC (1) TO PK40-CASTC (1)               AM0100
           MOVE        7-GC00-CASTC (2) TO PK40-CASTC (2)               AM0100
           MOVE        7-GC00-CASTC (3) TO PK40-CASTC (3)               AM0100
           MOVE        7-GC00-CASTC (4) TO PK40-CASTC (4)               AM0100
           MOVE        7-GC00-CASTC (5) TO PK40-CASTC (5)               AM0100
           MOVE        7-GC00-CASTC (6) TO PK40-CASTC (6)               AM0100
           MOVE        7-GC00-CAATY (1) TO PK40-CAATY (1)               AM0100
           MOVE        7-GC00-CAATY (2) TO PK40-CAATY (2)               AM0100
           MOVE        7-GC00-CAATY (3) TO PK40-CAATY (3)               AM0100
           MOVE        7-GC00-C299 TO PK40-C299                         AM0100
           MOVE        7-GC00-DCACG9 TO PK40-DCACG9                     AM0100
           MOVE        7-GC00-NAASQ TO PK40-NAASQ                       AM0100
           MOVE        7-GC00-NPISQ TO PK40-NPISQ                       AM0100
           MOVE        7-GC00-CIRAP TO PK40-CIRAP                       AM0100
           MOVE        7-GC00-IPERT TO PK40-IPERT                       AM0100
           MOVE        7-GC00-NEIBT TO PK40-NEIBT                       AM0100
           MOVE        7-GC00-GESQ2C TO PK40-GESQ2C                     AM0100
           MOVE        7-GC00-MIPPS TO PK40-MIPPS                       AM0100
           MOVE        7-GC00-IENDP TO PK40-IENDP                       AM0100
           SET CI0100GC-PCB-ACAP-PTR1 TO                                AM0100
                        PCB-ACAP-PTR1                                   AM0100
           INITIALIZE  DE10-DU03                                        AM0100
           CALL        CI0100 USING                                     AM0100
           DFHEIBLK                                                     AM0100
           DFHCOMMAREA                                                  AM0100
           DLIUIBII                                                     AM0100
           CI0100GC-PCB-ADDRESS-LIST                                    AM0100
           PK40                                                         AM0100
           PK41                                                         AM0100
           DE10                                                         AM0100
           MS03                                                         AM0100
           MX11.                                                        AM0100
      *N90GD.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F90GD.    IF    (MS03-NMESS2 > ZERO                              lv25
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F90GD-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0100 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0100 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F90GD-900. GO TO F90GE-FN.
       F90GD-FN. EXIT.
      *N90GE.    NOTE *NO ERRORS                          *.            ADU071
       F90GE.                                                           lv25
           INITIALIZE  MS03.                                            ADU071
                 IF    PK41-IGC01 = 'Y'                                 DOT
      *GC01 RETURNED
           MOVE        PK41-GC01 TO GC01
                 ELSE
      *NO ACTIVITY; EXIT FUNCTION
               GO TO     F90GA-FN.
       F90GE-FN. EXIT.
       F90GC-FN. EXIT.
      *N90GH.    NOTE *LOOP THRU ACTIVITIES RETURNED      *.
       F90GH.                                                           lv20
           MOVE        1                        TO J90GHR
                                    GO TO     F90GH-B.
       F90GH-A.
           ADD         1                        TO J90GHR.
       F90GH-B.
           IF          J90GHR                   >  PK41-QDECT9
                                    GO TO     F90GH-FN.
                 IF    PK41-IGC03 (J90GHR) = 'Y'                        DOT
      *GC03 RETURNED
           MOVE        PK41-GC03 (J90GHR) TO GC03
                 ELSE
      *GC03 SEGMENT WAS NOT FOUND
      *---> Send IMS ERROR Message                                      ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013421 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N90GI.    NOTE *FULL REDEMP REQUESTED              *.
       F90GI.    IF    GC03-CPORT = 'F'                                 lv25
                 AND   GC03-IPULL = 'Y'
                 NEXT SENTENCE ELSE GO TO     F90GI-FN.
      *& ACTIVITY TO BE PULLED
           MOVE        'Y' TO 7-FULL-REDEMP.
      *N90GJ.    NOTE *GC04 RETURNED; CK IF FEES          *.
       F90GJ.    IF    PK41-IGC04 (J90GHR) = 'Y'                        lv30
                 NEXT SENTENCE ELSE GO TO     F90GJ-FN.
      *ALREADY TAKEN
           MOVE        PK41-GC04 (J90GHR) TO GC04.
                 IF    GC04-ITERF = 'Y'                                 DOT
                 OR    GC04-AFEET > 0
      *TERM &/OR CUST FEES ALREADY
      *TAKEN TODAY
           MOVE        'Y' TO 7-FEES-TAKEN.
       F90GJ-FN. EXIT.
      *N90GK.    NOTE *QUIT READING ACTIVITIES            *.
       F90GK.                                                           lv30
               GO TO     F90GA-FN.
       F90GK-FN. EXIT.
       F90GI-FN. EXIT.
       F90GH-900. GO TO F90GH-A.
       F90GH-FN. EXIT.
      *N90GT.    NOTE *NOT AT END (MORE THAN 10 ROWS)     *.
       F90GT.    IF    PK41-IENDP = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F90GT-FN.
      *TAKE KEYS FROM 10TH ROW OF LAST
      *CALL AND PASS THEM AS INPUT TO
      *THE RESTART CALL.
           MOVE        PK41-IENDP TO 7-GC00-IENDP
           MOVE        PK41-MIPPS TO 7-GC00-MIPPS
           MOVE        GC03-DCACG9 TO 7-GC00-DCACG9
           MOVE        GC03-NAASQ TO 7-GC00-NAASQ.
                 IF    PK41-NPISQ (10) NOT NUMERIC                      DOT
      *CHECK FOR NON-NUMERIC GC06 KEY
           MOVE        ZEROS TO 7-GC00-NPISQ
                 ELSE
           MOVE        PK41-NPISQ (10) TO 7-GC00-NPISQ.
       F90GT-FN. EXIT.
       F90GB-900. GO TO F90GB-A.
       F90GB-FN. EXIT.
       F90GA-FN. EXIT.
       F90-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *PERFORMED ROUTINES                 *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92KB.    NOTE *READ TBU140 TO GET SHELTER         *.
       F92KB.                                                           lv10
      *TYPE CODE
           INITIALIZE  UQ40
           MOVE        US02-NSHID TO UQ40-NSHID
           MOVE        'F92KB - SELECT' TO 7-DB2-FUNCT                  ADB226
           EXEC SQL    SELECT                                           ADB226
                           SHELTER_TYPE_CDE
                       INTO
                           :UQ40-CNTTCS
                       FROM CORP.TBU140
                       WHERE
                           SHELTER_ID = :UQ40-NSHID          END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB226
       F92KB-FN. EXIT.
      *N92PA.    NOTE *SET CUST ACCT & PLAN TYPE CODES    *.
       F92PA.         EXIT.                                             lv10
      *N92PZ.    NOTE *SOURCE ACCT IS IN PENSION GROUP    *.
       F92PZ.    IF    7-SRCE-GRIDC = 002                               lv15
                 NEXT SENTENCE ELSE GO TO     F92PZ-FN.
      *N92QA.    NOTE *MOVE CODES FOR ACCT IN PENS GRP    *.
       F92QA.                                                           lv20
           MOVE        3 TO PJ47-CCACT.
                 IF    7-SRCE-GRPLT = 04                                DOT
           MOVE        'TARGET BENEFIT' TO PJ47-CPLTYP.
                 IF    7-SRCE-GRPLT = 05                                DOT
           MOVE        'PROFIT SHARING' TO PJ47-CPLTYP.
                 IF    7-SRCE-GRPLT = 06                                DOT
           MOVE        'MONEY PURCHASE' TO PJ47-CPLTYP.
       F92QA-FN. EXIT.
       F92PZ-900. GO TO F92QB-FN.
       F92PZ-FN. EXIT.
      *N92QB.    NOTE *SOURCE ACCT NOT IN PENSION GROUP   *.
       F92QB.         EXIT.                                             lv15
      *N92QC.    NOTE *SOURCE ACCT IS IRA                 *.
       F92QC.    IF    7-SRCE-CQACT = 001                               lv20
                 NEXT SENTENCE ELSE GO TO     F92QC-FN.
      *N92QD.    NOTE *INDIVIDUAL                         *.
       F92QD.    IF    7-SRCE-CIRAT = 001                               lv25
                 AND   7-SRCE-CIRAS = 001
                 NEXT SENTENCE ELSE GO TO     F92QD-FN.
      *ACTIVE
           MOVE        1 TO PJ47-CCACT
           MOVE        'IRA' TO PJ47-CPLTYP.
       F92QD-FN. EXIT.
      *N92QE.    NOTE *INDIVIDUAL                         *.
       F92QE.    IF    7-SRCE-CIRAT = 001                               lv25
                 AND   7-SRCE-CIRAS = 002
                 NEXT SENTENCE ELSE GO TO     F92QE-FN.
      *ROLLOVER
           MOVE        2 TO PJ47-CCACT
           MOVE        'IRA' TO PJ47-CPLTYP.
       F92QE-FN. EXIT.
      *N92QF.    NOTE *INDIVIDUAL                         *.
       F92QF.    IF    7-SRCE-CIRAT = 001                               lv25
                 AND   7-SRCE-CIRAS = 003
                 NEXT SENTENCE ELSE GO TO     F92QF-FN.
      *BENEFICIAL
           MOVE        4 TO PJ47-CCACT
           MOVE        'IRA' TO PJ47-CPLTYP.
       F92QF-FN. EXIT.
      *N92QG.    NOTE *SEP                                *.
       F92QG.    IF    7-SRCE-CIRAT = 003                               lv25
                 AND   7-SRCE-CIRAS = 001
                 NEXT SENTENCE ELSE GO TO     F92QG-FN.
      *ACTIVE
           MOVE        6 TO PJ47-CCACT
           MOVE        'SEP' TO PJ47-CPLTYP.
       F92QG-FN. EXIT.
      *N92QH.    NOTE *SRA                                *.
       F92QH.    IF    7-SRCE-CIRAT = 004                               lv25
                 AND   7-SRCE-CIRAS = 001
                 NEXT SENTENCE ELSE GO TO     F92QH-FN.
      *ACTIVE
           MOVE        5 TO PJ47-CCACT
           MOVE        'SRA' TO PJ47-CPLTYP.
       F92QH-FN. EXIT.
      *N92QI.    NOTE *ROTH CONTRIBUTORY                  *.
       F92QI.    IF    7-SRCE-CIRAT = 005                               lv25
                 AND   7-SRCE-CIRAS = 001
                 NEXT SENTENCE ELSE GO TO     F92QI-FN.
      *ACTIVE
           MOVE        7 TO PJ47-CCACT
           MOVE        'ROTH' TO PJ47-CPLTYP.
       F92QI-FN. EXIT.
      *N92QJ.    NOTE *ROTH CONTRIBUTORY                  *.
       F92QJ.    IF    7-SRCE-CIRAT = 005                               lv25
                 AND   7-SRCE-CIRAS = 003
                 NEXT SENTENCE ELSE GO TO     F92QJ-FN.
      *BENEFICIAL
           MOVE        9 TO PJ47-CCACT
           MOVE        'ROTH BENE' TO PJ47-CPLTYP.
       F92QJ-FN. EXIT.
      *N92QK.    NOTE *ROTH CONVERSION                    *.
       F92QK.    IF    7-SRCE-CIRAT = 006                               lv25
                 AND   7-SRCE-CIRAS = 001
                 NEXT SENTENCE ELSE GO TO     F92QK-FN.
      *ACTIVE
           MOVE        8 TO PJ47-CCACT
           MOVE        'ROTH CONV' TO PJ47-CPLTYP.
       F92QK-FN. EXIT.
      *N92QL.    NOTE *ROTH CONVERSION                    *.
       F92QL.    IF    7-SRCE-CIRAT = 006                               lv25
                 AND   7-SRCE-CIRAS = 003
                 NEXT SENTENCE ELSE GO TO     F92QL-FN.
      *BENEFICIAL
           MOVE        10 TO PJ47-CCACT
           MOVE        'ROTH CONVBEN' TO PJ47-CPLTYP.
       F92QL-FN. EXIT.
      *N92QM.    NOTE *COVERDELL (ED)                     *.
       F92QM.    IF    7-SRCE-CIRAT = 007                               lv25
                 AND   7-SRCE-CIRAS = 001
                 NEXT SENTENCE ELSE GO TO     F92QM-FN.
      *ACTIVE
           MOVE        11 TO PJ47-CCACT
           MOVE        'EDUCATION IRA' TO PJ47-CPLTYP.
       F92QM-FN. EXIT.
       F92QC-FN. EXIT.
      *N92QP.    NOTE *TSA PUBLIC SCHOOL EMPLOYEES        *.
       F92QP.    IF    7-SRCE-CQACT = 002                               lv20
                 NEXT SENTENCE ELSE GO TO     F92QP-FN.
           MOVE        'TSAPS' TO PJ47-CPLTYP.
       F92QP-FN. EXIT.
      *N92QR.    NOTE *TSA 501(C)(3) ORG EMPLOYEES        *.
       F92QR.    IF    7-SRCE-CQACT = 003                               lv20
                 NEXT SENTENCE ELSE GO TO     F92QR-FN.
           MOVE        'TSA501' TO PJ47-CPLTYP.
       F92QR-FN. EXIT.
      *N92QS.    NOTE *TSCA                               *.
       F92QS.    IF    7-SRCE-CQACT = 004                               lv20
                 NEXT SENTENCE ELSE GO TO     F92QS-FN.
           MOVE        3 TO PJ47-CCACT
           MOVE        'TSCA' TO PJ47-CPLTYP.
       F92QS-FN. EXIT.
       F92QB-FN. EXIT.
       F92PA-FN. EXIT.
      *N92QZ.    NOTE *READ TJ08 FOR CUST/TERM FEE        *.
       F92QZ.                                                           lv10
           MOVE        UQ40-CNTTCS TO TJ08-CNTTCS
           PERFORM     F92TB THRU F92TB-FN.
       F92QZ-FN. EXIT.
      *N92TA.    NOTE *RANDOM TABLE READ FOR TA5A         *.            ADUTAB
       F92TA.                                                           lv10
           MOVE        'R1' TO G-TA5A-TABFO                             ADUTAB
           COMPUTE     G-TA5A-LTH = 60 + G-TA5A-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA5A-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA5A)                                ADUTAB
                       LENGTH (G-TA5A-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA5A-TABCR NOT = '00'                          DOT
      *---> ON ERROR OVERRIDE
           INITIALIZE  TA5A.
       F92TA-FN. EXIT.
      *N92TB.    NOTE *RANDOM TABLE READ FOR TJ08         *.            ADUTAB
       F92TB.                                                           lv10
           MOVE        'R1' TO G-TJ08-TABFO                             ADUTAB
           COMPUTE     G-TJ08-LTH = 60 + G-TJ08-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TJ08-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TJ08)                                ADUTAB
                       LENGTH (G-TJ08-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TJ08-TABCR NOT = '00'                          DOT
           INITIALIZE  TJ08.
       F92TB-FN. EXIT.
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
      *N93SQ.    NOTE *SQL ERROR HANDLING                 *.            ADB220
       F93SQ.                                                           lv10
           MOVE        SQLCODE TO 7-TEST-SQLCODE.                       ADB220
                 IF    SQLCODE = +0                                     DOT
      *CHECK FOR GOOD SQLCODE                                           ADB220
           MOVE        '0' TO IK                                        ADB220
               GO TO     F93SQ-FN.                                      ADB220
                 IF    SQLCODE = +100                                   DOT
                 OR    SQLCODE = -803                                   ADB220
                 OR    SQLCODE = -811                                   ADB220
                 OR    SQLCODE = -913                                   ADB220
      *CHECK FOR NON-CRITICAL SQLCODE                                   ADB220
           MOVE        '1' TO IK                                        ADB220
               GO TO     F93SQ-FN.                                      ADB220
           PERFORM     F93SX THRU F93SX-FN.                             DOT
       F93SQ-FN. EXIT.
      *N93SX.    NOTE **** CRITICAL SQLCODE ** ABEND **   *.            ADB220
       F93SX.                                                           lv10
           MOVE        SQLCODE TO 7-SQL-ERROR                           ADB220
      *CAL DSNTIAR FOR TEXT EXPLANATION                                 ADB220
           CALL        'DSNTIAR' USING SQLCA                            ADB220
           7-SQLR-MESSAGE                                               ADB220
           7-SQLR-TEXT-LEN.                                             ADB220
      *FORMAT BATCH ABEND CODE                                          ADB220
           DISPLAY     '* * * * * * * * * * * * * * * '                 ADB220
           DISPLAY     'BAD SQLCODE RETURNED IN '                       ADB220
           DISPLAY     'PARAGRAPH ' 7-DB2-FUNCT                         ADB220
           DISPLAY     '* * * * * * * * * * * * * * * '                 ADB220
           DISPLAY     7-SQLR-TEXT (1)                                  ADB220
           DISPLAY     7-SQLR-TEXT (2)                                  ADB220
           DISPLAY     7-SQLR-TEXT (3)                                  ADB220
           DISPLAY     7-SQLR-TEXT (4)                                  ADB220
           DISPLAY     7-SQLR-TEXT (5)                                  ADB220
           DISPLAY     7-SQLR-TEXT (6)                                  ADB220
           DISPLAY     7-SQLR-TEXT (7)                                  ADB220
           DISPLAY     7-SQLR-TEXT (8)                                  ADB220
           DISPLAY     7-SQLR-TEXT (9)                                  ADB220
           DISPLAY     7-SQLR-TEXT (10)                                 ADB220
           DISPLAY     7-SQLR-TEXT (11)                                 ADB220
           DISPLAY     7-SQLR-TEXT (12)                                 ADB220
           CALL        7-ABEND-MOD USING 7-DB2-ABEND.                   ADB220
       F93SX-FN. EXIT.
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
      *               *DATABASE CALLS                     *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94CA.    NOTE *CALL GU ON CL01                    *.            ADU026
       F94CA.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CL01                                                    ADU026
           S-CLU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CA-FN. EXIT.
      *N94CB.    NOTE *CALL GN ON CL03                    *.            ADU026
       F94CB.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CL03                                                    ADU026
           S-CLU01-SSA S-CL03-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CB-FN. EXIT.
      *N94DA.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94DA.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DA-FN. EXIT.
      *N94DB.    NOTE *CALL GU ON CT09                    *.            ADU026
       F94DB.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT09' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 CT09                                                    ADU026
           S-CTU01-SSA S-CT07-SSA                                       ADU026
           7-CTA09-1-SSA
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DB-FN. EXIT.
      *N94DC.    NOTE *CALL GN ON CT10                    *.            ADU026
       F94DC.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT10' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PB06 CT10                                                    ADU026
           S-CTU01-SSA 7-CTA10-1-SSA                                    ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DC-FN. EXIT.
      *N94DD.    NOTE *CALL GN ON CT09                    *.            ADU026
       F94DD.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT09' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PB06 CT09                                                    ADU026
           S-CTU01-SSA S-CT07-SSA                                       ADU026
           7-CTA09-1-SSA
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DD-FN. EXIT.
      *N94EA.    NOTE *CALL GN ON GR19                    *.            ADU026
       F94EA.                                                           lv10
           MOVE        'GR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GR19' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PC06 GR19                                                    ADU026
           S-GRU01-SSA 7-GRA19-1-SSA                                    ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94EA-FN. EXIT.
      *N94EB.    NOTE *CALL GU ON GR07                    *.            ADU026
       F94EB.                                                           lv10
           MOVE        'GR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GR07' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PC06 GR07                                                    ADU026
           S-GRU01-SSA S-GR07-SSA                                       ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94EB-FN. EXIT.
      *N94FA.    NOTE *SEE IF CUST FEE ALREADY PAID       *.
       F94FA.                                                           lv10
      *BY CHECKING U304 DB2 TABLE
           MOVE        'N' TO 7-FEE-PAID.
      *N94FD.    NOTE *INITIALIZE DB SEGMENT AND IND      *.
       F94FD.                                                           lv15
           MOVE        'Y' TO WS-MORE-RECORDS.
       F94FD-FN. EXIT.
      *N94FF.    NOTE *SET DB KEYS FOR TBU304             *.
       F94FF.                                                           lv15
           MOVE        7-TAXPAYER-CLID TO UQ04-CLID
           MOVE        NS20-DCACG (1:4) TO
           UQ04-DFEEC
           MOVE        US02-NSHID TO UQ04-NSHID.
       F94FF-FN. EXIT.
      *N94FH.    NOTE *OPEN CURSOR                        *.
       F94FH.                                                           lv15
           PERFORM     F94RO THRU F94RO-FN.
      *N94FK.    NOTE *PROCESS CURSOR                     *.
       F94FK.                       GO TO     F94FK-B.                  lv20
       F94FK-A.
                 IF    WS-MORE-RECORDS = 'N'
                                    GO TO     F94FK-FN.
       F94FK-B.
           PERFORM     F94RF THRU F94RF-FN.
      *N94FM.    NOTE *RECORD FOUND SUM THE FEE PAID      *.
       F94FM.    IF    IK = '0'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F94FM-FN.
           ADD         UQ04-AFECH TO WS00-AFECH.
       F94FM-900. GO TO F94FQ-FN.
       F94FM-FN. EXIT.
      *N94FQ.    NOTE *ELSE... NO MORE RECORDS EXIST      *.
       F94FQ.                                                           lv25
           MOVE        'N' TO WS-MORE-RECORDS.
       F94FQ-FN. EXIT.
       F94FK-900. GO TO F94FK-A.
       F94FK-FN. EXIT.
       F94FH-FN. EXIT.
      *N94FS.    NOTE *CLOSE CURSOR                       *.
       F94FS.                                                           lv15
           PERFORM     F94RX THRU F94RX-FN.
       F94FS-FN. EXIT.
      *N94FZ.    NOTE *IF FOUND AND NON-ZERO FEE PAID     *.
       F94FZ.    IF    WS00-AFECH > ZEROS                               lv15
                 NEXT SENTENCE ELSE GO TO     F94FZ-FN.
           MOVE        'Y' TO 7-FEE-PAID.
       F94FZ-FN. EXIT.
       F94FA-FN. EXIT.
      *N94HA.    NOTE *GET FEE WAIVER TYPE CODE           *.
       F94HA.         EXIT.                                             lv10
      *N94HD.    NOTE *SET DB KEYS FOR TBU303             *.
       F94HD.                                                           lv15
           INITIALIZE  UL03
           MOVE        7-TAXPAYER-CLID TO UL03-CLID
           MOVE        NS20-DCACG (1:4) TO
           UL03-DFEEW
           MOVE        US02-NSHID TO UL03-NSHID.
       F94HD-FN. EXIT.
      *N94HF.    NOTE *READ TBU303 TABLE                  *.
       F94HF.                                                           lv15
           MOVE        'F94HF - SELECT' TO 7-DB2-FUNCT                  ADB226
           EXEC SQL    SELECT                                           ADB226
                           FEE_WAIVER_TYP_CD
                       INTO
                           :UL03-CFEWT
                       FROM CORP.TBU303
                       WHERE
                                 N_CL_ID  = :UL03-CLID
                       AND     SHELTER_ID = :UL03-NSHID
                       AND
                          FEE_WAIVED_YEAR = :UL03-DFEEW      END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB226
       F94HF-FN. EXIT.
      *N94HH.    NOTE *IF FEE WAIVED SET INDICATOR        *.
       F94HH.    IF    UL03-CFEWT NOT = SPACES                          lv15
                 NEXT SENTENCE ELSE GO TO     F94HH-FN.
           MOVE        'Y' TO 7-FEE-WAIVED
           PJ47-IWAIV.
       F94HH-FN. EXIT.
       F94HA-FN. EXIT.
      *N94RF.    NOTE *FETCH UQ04 CURSOR                  *.
       F94RF.                                                           lv10
           MOVE        'F94RF - FETCH' TO 7-DB2-FUNCT                   ADB224
           EXEC SQL    FETCH UQ04-CURSOR INTO                           ADB224
                                :UQ04-AFECH                  END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB224
       F94RF-FN. EXIT.
      *N94RO.    NOTE *FETCH UQ04 CURSOR                  *.
       F94RO.                                                           lv10
           MOVE        'F94RO - OPEN' TO 7-DB2-FUNCT                    ADB223
           EXEC SQL    OPEN UQ04-CURSOR                      END-EXEC.  ADB223
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB223
       F94RO-FN. EXIT.
      *N94RX.    NOTE *FETCH UQ04 CURSOR                  *.
       F94RX.                                                           lv10
           MOVE        'F94RX - CLOSE' TO 7-DB2-FUNCT                   ADB225
           EXEC SQL    CLOSE UQ04-CURSOR                     END-EXEC.  ADB225
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB225
       F94RX-FN. EXIT.
      *N94SH.    NOTE *GET SHELTER ID BY SRC ACCOUNT      *.
       F94SH.                                                           lv10
      *SET KEYS FOR READING TBU302
           INITIALIZE  US02
           MOVE        PJ47-CTID (1) TO US02-CTID
           MOVE        WS-DREND TO US02-DREND.
      *N94XQ.    NOTE *GET SHELTER ID FOR CTID            *.
       F94XQ.                                                           lv15
           MOVE        'F94XQ - SELECT' TO 7-DB2-FUNCT                  ADB226
           EXEC SQL    SELECT                                           ADB226
                           SHELTER_ID
                       INTO
                           :US02-NSHID
                       FROM CORP.TBU302
                       WHERE
                          N_CNTR_ID_GRP = :US02-CTID
                       AND     END_DATE = :US02-DREND        END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB226
       F94XQ-FN. EXIT.
       F94SH-FN. EXIT.
       F94-FN.   EXIT.
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *PERFORMED ROUTINES                 *
      *               *                                   *
      *               *************************************.
       F95.           EXIT.                                             lv05
      *N95DA.    NOTE *SEE IF CUST FEE ALREADY PAID       *.
       F95DA.                                                           lv10
      *OR WAIVED
           INITIALIZE  UQ04 WS00-AFECH
           MOVE        7-CUST-CRFEE TO UQ04-CRFEE
           MOVE        'N' TO 7-FEE-PAID
           7-FEE-WAIVED
      *CHECK IF FEE PAID
           PERFORM     F94FA THRU F94FA-FN.
                 IF    7-FEE-PAID = 'N'                                 DOT
      *CHECK IF WAIVED WHEN NOT PAID
           PERFORM     F94HA THRU F94HA-FN.
       F95DA-FN. EXIT.
      *N95EB.    NOTE *SEE IF TERM FEE ALREADY PAID       *.
       F95EB.                                                           lv10
           INITIALIZE  UQ04 WS00-AFECH
           MOVE        7-TERM-CRFEE TO UQ04-CRFEE
           MOVE        'N' TO 7-FEE-PAID
           7-FEE-WAIVED
      *CHECK IF FEE PAID
           PERFORM     F94FA THRU F94FA-FN.
       F95EB-FN. EXIT.
       F95-FN.   EXIT.
      *N96.      NOTE *************************************.
      *               *                                   *
      *               *PERFORMED ROUTINES                 *
      *               *                                   *
      *               *************************************.
       F96.           EXIT.                                             lv05
      *N96BB.    NOTE *CALL MWS100EX - DYNAMIC            *.
       F96BB.                                                           lv10
      *CALL MWS100EX - DYNAMIC                                          DOT
           CALL        MWS100EX USING DG01-DD05.                        AADA58
       F96BB-FN. EXIT.
      *N96DG.    NOTE *CALC STD DATE FROM CCYYMMDD        *.            AADA71
       F96DG.                                                           lv10
      *CONVERT CCYYMMDD TO CCYYDDD                                      AADA71
           MOVE        7-WS-YRS-BACK TO                                 AADA71
           7-DA70-XDAT8                                                 AADA71
           MOVE        7-DA70-XDAT8R TO DG01-XDAGP                      AADA71
           MOVE        1 TO DG01-XDACT                                  AADA71
           MOVE        1 TO DG01-XDACV                                  AADA71
           PERFORM     F96BB THRU F96BB-FN                              AADA71
           MOVE        DG01-XDAJP TO 7-DA70-XDAJC                       AADA71
      *WAS LAST YEAR LEAP YEAR ?                                        AADA71
           MOVE        7-DA70-XDAJY TO 7-DA70-XDAJY-WORK                AADA71
           SUBTRACT    1 FROM 7-DA70-XDAJY-WORK                         AADA71
           MOVE        365 TO 7-DA70-BASE.                              AADA71
                 IF    7-DA70-XDAJY9 NOT = ZERO                         DOT
           COMPUTE     7-DA70-X93V2 =                                   AADA71
           7-DA70-XDAJY-WORK / 4                                        AADA71
                 ELSE                                                   AADA71
           COMPUTE     7-DA70-X93V2 =                                   AADA71
           7-DA70-XDAJY-WORK / 400.                                     AADA71
                 IF    7-DA70-X9V2 = ZERO                               DOT
           MOVE        366 TO 7-DA70-BASE.                              AADA71
      *SUBTRACT NUMBER OF DAYS                                          DOT
           MOVE        '183' TO                                         AADA71
           7-DA70-XDAY3.                                                AADA71
                 IF    7-DA70-XDAY3R NOT <                              DOT
                       7-DA70-XDAJN                                     AADA71
      *CHECK FOR LAST YEAR DATE                                         AADA71
           MOVE        7-DA70-XDAJN TO                                  AADA71
           7-DA70-XDAJN-SIGNED                                          AADA71
           SUBTRACT    7-DA70-XDAY3R FROM                               AADA71
           7-DA70-XDAJN-SIGNED                                          AADA71
           SUBTRACT    1 FROM 7-DA70-XDAJY                              AADA71
           ADD         7-DA70-BASE TO                                   AADA71
           7-DA70-XDAJN-SIGNED                                          AADA71
           MOVE        7-DA70-XDAJN-SIGNED TO                           AADA71
           7-DA70-XDAJN                                                 AADA71
                 ELSE                                                   AADA71
      *DATE IS THE SAME YEAR                                            AADA71
           SUBTRACT    7-DA70-XDAY3R FROM 7-DA70-XDAJN.                 AADA71
      *CONVERT CCYYDDD TO CCYYMMDD                                      DOT
           MOVE        7-DA70-XDAJC TO DG01-XDAJP                       AADA71
           MOVE        1 TO DG01-XDACT                                  AADA71
           MOVE        2 TO DG01-XDACV                                  AADA71
           PERFORM     F96BB THRU F96BB-FN                              AADA71
           MOVE        DG01-XDAGP TO 7-DA70-XDAT8R                      AADA71
           MOVE        7-DA70-XDAT8 TO                                  AADA71
           7-WS-YRS-BACK.                                               AADA71
       F96DG-FN. EXIT.
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
