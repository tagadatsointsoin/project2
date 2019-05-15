       IDENTIFICATION DIVISION.                                         CI0045
       PROGRAM-ID.  CI0045.                                             CI0045
      *AUTHOR.         CALC YTD CONTRIBUTION AMT.                       CI0045
      *DATE-COMPILED.   09/08/14.                                       CI0045
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2008                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE CAT    SYSTEM AND ALL INFORMATION RELATING THERETO,    *ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE CAT    SYSTEM AND ALL            *ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE CAT          *ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 2008                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0045
       CONFIGURATION SECTION.                                           CI0045
       SOURCE-COMPUTER. IBM-370.                                        CI0045
       OBJECT-COMPUTER. IBM-370.                                        CI0045
       DATA DIVISION.                                                   CI0045
       WORKING-STORAGE SECTION.                                         CI0045
      *************SSA FOR CT09 ACCESS*****************
       01               7-CTA09-1-SSA.                                  AAADBL
         05             FILLER          PIC X(08)   VALUE 'CT09'.       AAADBL
         05             FILLER          PIC X(01)   VALUE '*'.          AAADBL
         05             7-CTA09-1-CCOD PIC X(05)  VALUE '-----'.        AAADBL
         05             FILLER          PIC X(01)   VALUE '('.          AAADBL
         05             FILLER          PIC X(08)   VALUE 'CT09K   '.
         05             FILLER          PIC X(02)   VALUE ' ='.
         05             FILLER          PIC 9(03)   VALUE 004.
         05             FILLER          PIC X(01)   VALUE '&'.
         05             FILLER          PIC X(08)   VALUE 'GERED   '.
         05             FILLER          PIC X(02)   VALUE ' ='.
         05             FILLER          PIC 9(08)   VALUE 00000000.
         05             FILLER          PIC X(01)   VALUE ')'.          AAADBL
      **********************************************************
      **       THESE ARE THE WORKING STORAGE VARIABLES         *
      **     WHICH ARE USED TO STORE THE DATE VARIABLES        *
      **********************************************************
       01  7-DATE-FIELDS.
           05  7-SEARCH-NDAT9            PIC  9(08).
           05  FILLER REDEFINES 7-SEARCH-NDAT9.
               10  7-S-NDAT9-CCYY        PIC  9(04).
               10  7-S-NDAT9-MMDD        PIC  9(04).
           05  7-HN10-NDAT9              PIC  9(08).
           05  FILLER REDEFINES 7-HN10-NDAT9.
               10  7-HN10-NDAT9-CCYY     PIC  9(04).
               10  7-HN10-NDAT9-MMDD     PIC  9(04).
      **********************************************************
      **       THESE ARE THE WORKING STORAGE VARIABLES         *
      **            THAT ARE USED SET THE CF FLAGS             *
      **********************************************************
       01 7-SET-FLAG.
          05  CL01-CF     PIC 9 VALUE ZEROES.
          05  CL36-CF     PIC 9 VALUE ZEROES.
          05  CL37-CF     PIC 9 VALUE ZEROES.
          05  CT01-CF     PIC 9 VALUE ZEROES.
          05  CT07-CF     PIC 9 VALUE ZEROES.
          05  CT09-CF     PIC 9 VALUE ZEROES.
          05  HN01-CF     PIC 9 VALUE ZEROES.
      **********************************************************
      **       THESE ARE THE WORKING STORAGE VARIABLES         *
      **          THAT ARE USED TO STORE CIRAP VALUE           *
      **********************************************************
       01  7-MISC-88-CIRAP.
      *!WI id=1
           05  7-CIRAP
                        PICTURE XX                                      CI0045
                                      VALUE SPACES.
               88 7-88-VALID-CIRAP    VALUES ARE 'CU' 'PR' 'RO' 'IT'
                                                 'ET' 'SP' 'SC' 'SD'
                                                 'CV' 'RE'.
               88  7-88-CURR-CONTRIB  VALUE 'CU'.
               88  7-88-PRIOR-CONTRIB VALUE 'PR'.
      *!WI id=2
           05  7-HN10-CIRAP
                        PICTURE XX                                      CI0045
                                      VALUE SPACES.
               88 7-88-HN10-VALID-CIRAP    VALUES ARE 'CU' 'PR' 'RO'
                                                 'IT' 'ET' 'SP' 'SC'
                                                 'SD' 'CV' 'RE'.
               88  7-88-HN10-CURR-CONTRIB  VALUE 'CU'.
               88  7-88-HN10-PRIOR-CONTRIB VALUE 'PR'.
      **********************************************************
      **       THESE ARE THE WORKING STORAGE VARIABLES         *
      **          THAT ARE USED TO STORE CATRF1 VALUE          *
      **********************************************************
      *!WI id=3
       01  7-HN10-CATRF1
                        PICTURE 9(3)                                    CI0045
                                       VALUE ZERO.

           88  7-88-HN10-CONTRIBUTION       VALUE 001.
      *        JUST WHAT IT SAYS
           88  7-88-HN10-CONT-ADJ           VALUE 002.
           88  7-88-HN10-CONT-REV           VALUE 003.
      *            CONTRIBUTION REVERSAL
           88  7-88-HN10-CONT-BF            VALUE 004.
      *            CONTRIBUTION BALANCE FORWARD
           88  7-88-HN10-CONT-BC            VALUE 005.
      *            CONTRIBUTION BUCKET CONVERSION
           88  7-88-HN10-GEN-CONT-REV       VALUE 016.
      *            ALLOWS REVERSAL OF ALL CONTRIBUTIONS
           88  7-88-HN10-ALL-CONT           VALUES ARE 001 002 003 004
                                                  005 016.
      *            ALL CONTRIBUTION TRANSACTIONS
       01                 CL01.                                         CI0045
            10            CL01-CL01K.                                   CI0045
            11            CL01-C199.                                    CI0045
            12            CL01-CLID.                                    CI0045
            13            CL01-CLIDO  PICTURE  9(3).                    CI0045
            13            CL01-CLIDN.                                   CI0045
            14            CL01-CLIDNP PICTURE  X(12).                   CI0045
            14            CL01-CLIDND PICTURE  9(8).                    CI0045
            10            CL01-GECKD  PICTURE  9.                       CI0045
            10            CL01-GEMDA  PICTURE  9(8).                    CI0045
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0045
                          BINARY.                                       CI0045
            10            CL01-GECUC  PICTURE  99.                      CI0045
            10            CL01-CLDOR  PICTURE  9(8).                    CI0045
            10            CL01-CLLNG  PICTURE  XX.                      CI0045
            10            CL01-GESLC  PICTURE  99.                      CI0045
            10            CL01-CLTYP  PICTURE  X.                       CI0045
            10            CL01-CLCLS  PICTURE  9(3).                    CI0045
            10            CL01-CLTWRC PICTURE  99.                      CI0045
            10            CL01-CLPVC  PICTURE  99.                      CI0045
            10            CL01-CLIND  PICTURE  9(3).                    CI0045
            10            CL01-CLTRC  PICTURE  99.                      CI0045
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            10            CL01-AYSIDA PICTURE  9(3).                    CI0045
            10            CL01-AYSID  PICTURE  9(5).                    CI0045
            10            CL01-CLSTR  PICTURE  9(2).                    CI0045
            10            CL01-CLC11  PICTURE  X.                       CI0045
            10            CL01-CLTIN  PICTURE  9(12).                   CI0045
            10            CL01-CLTND  PICTURE  9(8).                    CI0045
            10            CL01-CLTINC PICTURE  9.                       CI0045
            10            CL01-CCDWA  PICTURE  9.                       CI0045
            10            CL01-CICES  PICTURE  X.                       CI0045
            10            CL01-CLTRA  PICTURE  9(2).                    CI0045
            10            CL01-DIRSY  PICTURE  9(4)                     CI0045
                          COMPUTATIONAL-3.                              CI0045
            10            CL01-CFEDS  PICTURE  X.                       CI0045
            10            CL01-FILLER PICTURE  X(06).                   CI0045
       01                 CL36.                                         CI0045
            10            CL36-CL36K.                                   CI0045
            11            CL36-C299.                                    CI0045
            12            CL36-CTID.                                    CI0045
            13            CL36-CTIDA  PICTURE  9(3).                    CI0045
            13            CL36-CTIDN.                                   CI0045
            14            CL36-CTIDNP PICTURE  X(13).                   CI0045
            14            CL36-CTIDND PICTURE  9(11).                   CI0045
       01                 CL37.                                         CI0045
            10            CL37-C400.                                    CI0045
            11            CL37-GELL   PICTURE  9(4)                     CI0045
                          BINARY.                                       CI0045
            11            CL37-CL37K.                                   CI0045
            12            CL37-CLCTRC PICTURE  9(3).                    CI0045
            11            CL37-GERSD  PICTURE  9(8).                    CI0045
            11            CL37-GERED  PICTURE  9(8).                    CI0045
            10            CL37-C499.                                    CI0045
            11            CL37-FILLER PICTURE  X(20).                   CI0045
            10            CL37-C401                                     CI0045
                          REDEFINES            CL37-C499.               CI0045
            11            CL37-GECSQ  PICTURE  S9(3)                    CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            CL37-CTAXR  PICTURE  X.                       CI0045
            11            CL37-GETAI  PICTURE  X.                       CI0045
            11            CL37-CTLACD PICTURE  9(8).                    CI0045
            11            CL37-GEPCS  PICTURE  S9(3)                    CI0045
                          COMPUTATIONAL-3.                              CI0045
       01                 CT01.                                         CI0045
            10            CT01-CT01K.                                   CI0045
            11            CT01-C299.                                    CI0045
            12            CT01-CTID.                                    CI0045
            13            CT01-CTIDA  PICTURE  9(3).                    CI0045
            13            CT01-CTIDN.                                   CI0045
            14            CT01-CTIDNP PICTURE  X(13).                   CI0045
            14            CT01-CTIDND PICTURE  9(11).                   CI0045
            10            CT01-GECKD  PICTURE  9.                       CI0045
            10            CT01-GEMDA  PICTURE  9(8).                    CI0045
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0045
                          BINARY.                                       CI0045
            10            CT01-GECUC  PICTURE  99.                      CI0045
            10            CT01-CTAUL  PICTURE  9(3).                    CI0045
            10            CT01-DIRAC  PICTURE  9(4).                    CI0045
            10            CT01-CTCCI  PICTURE  X.                       CI0045
            10            CT01-CTCUS  PICTURE  999.                     CI0045
            10            CT01-CTEFD  PICTURE  9(8).                    CI0045
            10            CT01-CTIAD  PICTURE  9(8).                    CI0045
            10            CT01-CLCUS  PICTURE  99.                      CI0045
            10            CT01-CAMMB  PICTURE  X(3).                    CI0045
            10            CT01-CKPMM  PICTURE  X.                       CI0045
            10            CT01-CTLAD  PICTURE  9(8).                    CI0045
            10            CT01-IPERS  PICTURE  X.                       CI0045
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            10            CT01-CTLAT  PICTURE  9(8).                    CI0045
            10            CT01-CTLATC PICTURE  9(6).                    CI0045
            10            CT01-IMEGA  PICTURE  X.                       CI0045
            10            CT01-DIRAB  PICTURE  9(8).                    CI0045
            10            CT01-COLRQ  PICTURE  X.                       CI0045
            10            CT01-ZDA04  PICTURE  X(4).                    CI0045
            10            CT01-CTLPD  PICTURE  9(8).                    CI0045
            10            CT01-CIRASP PICTURE  9.                       CI0045
            10            CT01-CIRATP PICTURE  99.                      CI0045
            10            CT01-DRTHC  PICTURE  9(8).                    CI0045
            10            CT01-CPPTC  PICTURE  X.                       CI0045
            10            CT01-ZDA06  PICTURE  X(6).                    CI0045
            10            CT01-CTACD  PICTURE  9(8).                    CI0045
            10            CT01-CTNLI  PICTURE  X.                       CI0045
            10            CT01-CTRHO  PICTURE  9(8).                    CI0045
            10            CT01-CTSGD  PICTURE  9(8).                    CI0045
            10            CT01-CPATP  PICTURE  X(1).                    CI0045
            10            CT01-IRSTA  PICTURE  X.                       CI0045
            10            CT01-CTSTA  PICTURE  99.                      CI0045
            10            CT01-CTSSC  PICTURE  99.                      CI0045
            10            CT01-PRLIN  PICTURE  9(3).                    CI0045
            10            CT01-PRCOD  PICTURE  9(5).                    CI0045
            10            CT01-PRSCD  PICTURE  X(9).                    CI0045
            10            CT01-CTLNI  PICTURE  X.                       CI0045
            10            CT01-AYSIDA PICTURE  9(3).                    CI0045
            10            CT01-AYSID  PICTURE  9(5).                    CI0045
            10            CT01-CTBMC  PICTURE  99.                      CI0045
            10            CT01-CINAR  PICTURE  99.                      CI0045
            10            CT01-CPHTR  PICTURE  X.                       CI0045
            10            CT01-CDSTR  PICTURE  XX.                      CI0045
            10            CT01-CQACT  PICTURE  999.                     CI0045
            10            CT01-CIRAS  PICTURE  999.                     CI0045
            10            CT01-CIRAT  PICTURE  999.                     CI0045
            10            CT01-CLRAY  PICTURE  9(5).                    CI0045
            10            CT01-CATTP  PICTURE  X.                       CI0045
       01                 CT07.                                         CI0045
            10            CT07-CT07K.                                   CI0045
            11            CT07-C199.                                    CI0045
            12            CT07-CLID.                                    CI0045
            13            CT07-CLIDO  PICTURE  9(3).                    CI0045
            13            CT07-CLIDN.                                   CI0045
            14            CT07-CLIDNP PICTURE  X(12).                   CI0045
            14            CT07-CLIDND PICTURE  9(8).                    CI0045
       01                 CT09.                                         CI0045
            10            CT09-A100.                                    CI0045
            11            CT09-GELL   PICTURE  9(4)                     CI0045
                          BINARY.                                       CI0045
            11            CT09-CT09K.                                   CI0045
            12            CT09-CLCTRC PICTURE  9(3).                    CI0045
            11            CT09-GERSD  PICTURE  9(8).                    CI0045
            11            CT09-GERED  PICTURE  9(8).                    CI0045
            10            CT09-A199.                                    CI0045
            11            CT09-FILLER PICTURE  X(20).                   CI0045
            10            CT09-A101                                     CI0045
                          REDEFINES            CT09-A199.               CI0045
            11            CT09-GECSQ  PICTURE  S9(3)                    CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            CT09-CTAXR  PICTURE  X.                       CI0045
            11            CT09-GETAI  PICTURE  X.                       CI0045
            11            CT09-CTLACD PICTURE  9(8).                    CI0045
            11            CT09-GEPCS  PICTURE  S9(3)                    CI0045
                          COMPUTATIONAL-3.                              CI0045
            10            CT09-A102                                     CI0045
                          REDEFINES            CT09-A199.               CI0045
            11            CT09-CLPID  PICTURE  9(9).                    CI0045
      **********************************************************
      **       THESE ARE THE WORKING STORAGE VARIABLES         *
      **       THAT ARE USED TO STORE CONTRIBUTION AMOUNT      *
      **********************************************************
       01  7-SAVE-FIELDS.
      *!WI id=4
           05  7-SAVE-AIRCT
                        PICTURE S9(7)V99                                CI0045
                            COMPUTATIONAL-3                             CI0045
                                                           VALUE ZERO.
      *!WI id=5
           05  7-SAVE-AIRCU
                        PICTURE S9(7)V99                                CI0045
                            COMPUTATIONAL-3                             CI0045
                                                           VALUE ZERO.
      *!WI id=6
           05  7-SAVE-AIRPR
                        PICTURE S9(7)V99                                CI0045
                            COMPUTATIONAL-3                             CI0045
                                                           VALUE ZERO.
      **********************************************************
      **       THESE ARE THE WORKING STORAGE VARIABLES         *
      **       THAT ARE USED TO STORE ACCUMULATED AMOUNT       *
      **********************************************************
       01  7-ACCT-FIELDS.
      *!WI id=7
           05  7-ACCT-AIRCT
                        PICTURE S9(7)V99                                CI0045
                            COMPUTATIONAL-3                             CI0045
                                                           VALUE ZERO.
      *!WI id=8
           05  7-ACCT-AIRCU
                        PICTURE S9(7)V99                                CI0045
                            COMPUTATIONAL-3                             CI0045
                                                           VALUE ZERO.
      *!WI id=9
           05  7-ACCT-AIRPR
                        PICTURE S9(7)V99                                CI0045
                            COMPUTATIONAL-3                             CI0045
                                                           VALUE ZERO.
       01  7-INITIAL-FLDS.
      *!WI id=10
           05  7-INIT-AIRCT
                        PICTURE S9(7)V99                                CI0045
                            COMPUTATIONAL-3                             CI0045
                                                           VALUE ZERO.
      *!WI id=11
           05  7-INIT-AIRCU
                        PICTURE S9(7)V99                                CI0045
                            COMPUTATIONAL-3                             CI0045
                                                           VALUE ZERO.
      *!WI id=12
           05  7-INIT-AIRPR
                        PICTURE S9(7)V99                                CI0045
                            COMPUTATIONAL-3                             CI0045
                                                           VALUE ZERO.
       01                 HM01.                                         CI0045
            10            HM01-HN01K.                                   CI0045
            11            HM01-CTID.                                    CI0045
            12            HM01-CTIDA  PICTURE  9(3).                    CI0045
            12            HM01-CTIDN.                                   CI0045
            13            HM01-CTIDNP PICTURE  X(13).                   CI0045
            13            HM01-CTIDND PICTURE  9(11).                   CI0045
       01                 HN10.                                         CI0045
            10            HN10-GELL   PICTURE  9(4)                     CI0045
                          BINARY.                                       CI0045
            10            HN10-HO01.                                    CI0045
            11            HN10-HN10K.                                   CI0045
            12            HN10-NDAT9  PICTURE  9(8).                    CI0045
            12            HN10-GESQ3  PICTURE  999.                     CI0045
            12            HN10-GESQ3A PICTURE  X(3).                    CI0045
            11            HN10-CATRF1 PICTURE  9(3).                    CI0045
            11            HN10-CATRS1 PICTURE  9(3).                    CI0045
            11            HN10-DPROC  PICTURE  9(8).                    CI0045
            11            HN10-IORSY  PICTURE  X.                       CI0045
            11            HN10-GCTPRT.                                  CI0045
            12            HN10-GCTPR.                                   CI0045
            13            HN10-CTIDA1 PICTURE  9(3).                    CI0045
            13            HN10-PRCOD  PICTURE  9(5).                    CI0045
            12            HN10-CATRN.                                   CI0045
            13            HN10-CATRF  PICTURE  9(3).                    CI0045
            13            HN10-CATRS  PICTURE  9(3).                    CI0045
            11            HN10-NRRSQ  PICTURE  9(02).                   CI0045
            10            HN10-HO99.                                    CI0045
            11            HN10-FILLER PICTURE  X(300).                  CI0045
            10            HN10-HO20                                     CI0045
                          REDEFINES            HN10-HO99.               CI0045
            11            HN10-CIRAP  PICTURE  XX.                      CI0045
            11            HN10-AIRCT  PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            10            HN10-HO21                                     CI0045
                          REDEFINES            HN10-HO99.               CI0045
            11            HN10-CIRAP1 PICTURE  XX.                      CI0045
            11            HN10-AIRCT1 PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-AIRCU  PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-AIRPR  PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-AIRRO  PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-AIRIT  PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-AIRET  PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-AIRSC  PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-AIRSP  PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-AIRSD  PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-AIRCV  PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-AIRRE  PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            10            HN10-HO22                                     CI0045
                          REDEFINES            HN10-HO99.               CI0045
            11            HN10-CLCUS  PICTURE  99.                      CI0045
            11            HN10-AIRDS  PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-AIRFW  PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-AIRSW  PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-GESTNS PICTURE  X(2).                    CI0045
            10            HN10-HO23                                     CI0045
                          REDEFINES            HN10-HO99.               CI0045
            11            HN10-CLCUS1 PICTURE  99.                      CI0045
            11            HN10-AIRDS1 PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-AIRFW1 PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-AIRSW1 PICTURE  S9(7)V99                 CI0045
                          COMPUTATIONAL-3.                              CI0045
            11            HN10-GESTN1 PICTURE  X(2).                    CI0045
            11            HN10-CLTYP  PICTURE  X.                       CI0045
            11            HN10-XZ60A.                                   CI0045
            12            HN10-CLNAMF PICTURE  X(20).                   CI0045
            12            HN10-CLNAMM PICTURE  X(15).                   CI0045
            12            HN10-CLNAML PICTURE  X(25).                   CI0045
            11            HN10-XZ60B                                    CI0045
                          REDEFINES            HN10-XZ60A.              CI0045
            12            HN10-CLORN  PICTURE  X(45).                   CI0045
            12            HN10-XZ15   PICTURE  X(15).                   CI0045
            11            HN10-GECIT  PICTURE  X(25).                   CI0045
            11            HN10-GECTRY PICTURE  X(20).                   CI0045
            11            HN10-GEST   PICTURE  X(8).                    CI0045
            11            HN10-GEPCD  PICTURE  X(12).                   CI0045
            11            HN10-CLTIN  PICTURE  9(12).                   CI0045
            11            HN10-GESAD  PICTURE  X(30)                    CI0045
                          OCCURS       003     TIMES.                   CI0045
      **********************************************************
      **       THESE ARE THE WORKING STORAGE VARIABLES         *
      **         THAT ARE USED TO STORE PRIOR YEAR AMOUNT      *
      **********************************************************
       01  7-SAVE-PR-VALUE.
      *CONTRIBUTION FOR CURRENT YEAR IN PRIOR YEAR.
           05  7-SAVE-AMT-PR-CU     PIC S9(7)V99
                                                           VALUE ZERO.
      *CONTRIBUTION FOR PRIOR YEAR IN CURRENT YEAR.
           05  7-SAVE-AMT-PR        PIC S9(7)V99
                                                           VALUE ZERO.
      *TOTAL CONTRIBUTION FOR PRIOR YEAR.
           05  7-SAVE-TOTAL-PR      PIC S9(7)V99
                                                           VALUE ZERO.
      **********************************************************
      **       THESE ARE THE WORKING STORAGE VARIABLES         *
      **    WHICH ARE USED TO SAVE ACCT ID AND CLIENT ID       *
      **********************************************************
       01  7-SAVE-VARIABLES.
           05  7-SAVE-CTID  PIC X(27) VALUE SPACES.
           05  7-TAXPAYER-FOUND        PIC  X(03) VALUE 'NO'.
               88  7-88-TAXPAYER-FOUND VALUE 'YES'.
               88  7-88-TAXPAYER-NFND  VALUE 'NO'.
           05  7-TAXPAYER              PIC  X(01) VALUE 'N'.
               88 7-88-TAXPAYER                   VALUE 'Y'.
               88 7-88-NOT-TAXPAYER               VALUE 'N'.
           05  7-OWNER                 PIC  X(01) VALUE 'N'.
               88 7-88-OWNER                      VALUE 'Y'.
               88 7-88-NOT-OWNER                  VALUE 'N'.
       01  WS01-CLID.
      *!WI id=13
               05  WS01-CLIDO     VALUE ZEROES
                        PICTURE 9(3).                                   CI0045
      *!WI id=14
               05  WS01-CLIDN     VALUE SPACES
                        PICTURE X(20).                                  CI0045
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       AAAD30
       01                 XW05.                                         CI0045
            10            XW05-XW06.                                    CI0045
            11            XW05-XDBPCB.                                  CI0045
            12            XW05-XDBDNM PICTURE  X(08)                    CI0045
                          VALUE                SPACE.                   CI0045
            12            XW05-XSEGLV PICTURE  X(02)                    CI0045
                          VALUE                SPACE.                   CI0045
            12            XW05-XRC    PICTURE  X(02)                    CI0045
                          VALUE                SPACE.                   CI0045
            12            XW05-XPROPT PICTURE  X(04)                    CI0045
                          VALUE                SPACE.                   CI0045
            12            XW05-FILLER PICTURE  S9(5)                    CI0045
                          VALUE                ZERO                     CI0045
                          BINARY.                                       CI0045
            12            XW05-XSEGNM PICTURE  X(08)                    CI0045
                          VALUE                SPACE.                   CI0045
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0045
                          VALUE                ZERO                     CI0045
                          BINARY.                                       CI0045
            12            XW05-XSEGNB PICTURE  9(05)                    CI0045
                          VALUE                ZERO                     CI0045
                          BINARY.                                       CI0045
            12            XW05-XCOKEY PICTURE  X(70)                    CI0045
                          VALUE                SPACE.                   CI0045
            10            XW05-XW07.                                    CI0045
            11            XW05-XIOPCB.                                  CI0045
            12            XW05-XTERMI PICTURE  X(08)                    CI0045
                          VALUE                SPACE.                   CI0045
            12            XW05-FILLER PICTURE  XX                       CI0045
                          VALUE                SPACE.                   CI0045
            12            XW05-XRC1   PICTURE  X(02)                    CI0045
                          VALUE                SPACE.                   CI0045
            12            XW05-FILLER PICTURE  X(12)                    CI0045
                          VALUE                SPACE.                   CI0045
            12            XW05-XMODNM PICTURE  X(8)                     CI0045
                          VALUE                SPACE.                   CI0045
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0045
                          VALUE                ZERO.                    CI0045
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0045
                          VALUE                ZERO.                    CI0045
            10            XW05-XGU    PICTURE  X(4)                     CI0045
                          VALUE                'GU  '.                  CI0045
            10            XW05-XGHU   PICTURE  X(4)                     CI0045
                          VALUE                'GHU '.                  CI0045
            10            XW05-XGN    PICTURE  X(4)                     CI0045
                          VALUE                'GN  '.                  CI0045
            10            XW05-XGHN   PICTURE  X(4)                     CI0045
                          VALUE                'GHN '.                  CI0045
            10            XW05-XGNP   PICTURE  X(4)                     CI0045
                          VALUE                'GNP '.                  CI0045
            10            XW05-XGHNP  PICTURE  X(4)                     CI0045
                          VALUE                'GHNP'.                  CI0045
            10            XW05-XREPL  PICTURE  XXXX                     CI0045
                          VALUE                'REPL'.                  CI0045
            10            XW05-XISRT  PICTURE  X(4)                     CI0045
                          VALUE                'ISRT'.                  CI0045
            10            XW05-XDLET  PICTURE  X(4)                     CI0045
                          VALUE                'DLET'.                  CI0045
            10            XW05-XOPEN  PICTURE  X(4)                     CI0045
                          VALUE                'OPEN'.                  CI0045
            10            XW05-XCLSE  PICTURE  X(4)                     CI0045
                          VALUE                'CLSE'.                  CI0045
            10            XW05-XCHKP  PICTURE  X(4)                     CI0045
                          VALUE                'CHKP'.                  CI0045
            10            XW05-XXRST  PICTURE  X(4)                     CI0045
                          VALUE                'XRST'.                  CI0045
            10            XW05-XTERM  PICTURE  X(4)                     CI0045
                          VALUE                'TERM'.                  CI0045
            10            XW05-XNFPAC PICTURE  X(13)                    CI0045
                          VALUE                SPACE.                   CI0045
       01   DLIERROR  PIC X(8) VALUE 'DLIERROR'.                        AAAD30
       01   DEBUT-WSS.                                                  CI0045
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0045
            05   IK     PICTURE X.                                      CI0045
       01  CONSTANTES-PAC.                                              CI0045
           05  FILLER  PICTURE X(87)   VALUE                            CI0045
                     '9999 CAT09/08/14CI0045ADMIN   19:31:44CI0045  BVAPCI0045
      -    '09/08/20143.5 V0419/02/201425/02/2014'.                     CI0045
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0045
           05  NUGNA   PICTURE X(5).                                    CI0045
           05  APPLI   PICTURE X(3).                                    CI0045
           05  DATGN   PICTURE X(8).                                    CI0045
           05  PROGR   PICTURE X(6).                                    CI0045
           05  CODUTI  PICTURE X(8).                                    CI0045
           05  TIMGN   PICTURE X(8).                                    CI0045
           05  PROGE   PICTURE X(8).                                    CI0045
           05  COBASE  PICTURE X(4).                                    CI0045
           05  DATGNC  PICTURE X(10).                                   CI0045
           05  RELEAS  PICTURE X(7).                                    CI0045
           05  DATGE   PICTURE X(10).                                   CI0045
           05  DATSQ   PICTURE X(10).                                   CI0045
       01  DATCE.                                                       CI0045
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0045
         05  DATOR.                                                     CI0045
           10  DATOA  PICTURE XX.                                       CI0045
           10  DATOM  PICTURE XX.                                       CI0045
           10  DATOJ  PICTURE XX.                                       CI0045
       01   VARIABLES-CONDITIONNELLES.                                  CI0045
            05                  FT      PICTURE X VALUE '0'.            CI0045
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0045
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0045
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0045
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0045
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0045
            05       5-HM00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0045
            05       5-HN00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0045
            05       5-PB00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0045
            05       5-PC00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0045
            05       5-PD00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0045
       01               S-CL01-SSA.                                     CI0045
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0045
                                      VALUE 'CL01    '.                 CI0045
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0045
            10          S-CL01-CCOD   PICTURE X(5)                      CI0045
                                      VALUE '-----'.                    CI0045
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0045
       01            S-CLU01-SSA.                                       CI0045
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0045
                                      VALUE 'CL01    '.                 CI0045
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0045
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0045
                                      VALUE '-----'.                    CI0045
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0045
                                      VALUE '(CL01K'.                   CI0045
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0045
            10       S-CLU01-CL01K.                                     CI0045
            11       S-CLU01-C199.                                      CI0045
            12       S-CLU01-CLID.                                      CI0045
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0045
            13       S-CLU01-CLIDN.                                     CI0045
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0045
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0045
            10  FILLER   PICTURE X    VALUE ')'.                        CI0045
       01               S-CL36-SSA.                                     CI0045
            10         S1-CL36-SEGNAM PICTURE X(8)                      CI0045
                                      VALUE 'CL36    '.                 CI0045
            10         S1-CL36-CCOM   PICTURE X VALUE '*'.              CI0045
            10          S-CL36-CCOD   PICTURE X(5)                      CI0045
                                      VALUE '-----'.                    CI0045
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0045
       01            S-CLU36-SSA.                                       CI0045
            10      S1-CLU36-SEGNAM PICTURE X(8)                        CI0045
                                      VALUE 'CL36    '.                 CI0045
            10      S1-CLU36-CCOM   PICTURE X VALUE '*'.                CI0045
            10       S-CLU36-CCOD   PICTURE X(5)                        CI0045
                                      VALUE '-----'.                    CI0045
            10      S1-CLU36-FLDNAM PICTURE X(9)                        CI0045
                                      VALUE '(CL36K'.                   CI0045
            10       S-CLU36-OPER  PICTURE XX VALUE ' ='.               CI0045
            10       S-CLU36-CL36K.                                     CI0045
            11       S-CLU36-C299.                                      CI0045
            12       S-CLU36-CTID.                                      CI0045
            13       S-CLU36-CTIDA    PICTURE  9(3).                    CI0045
            13       S-CLU36-CTIDN.                                     CI0045
            14       S-CLU36-CTIDNP   PICTURE  X(13).                   CI0045
            14       S-CLU36-CTIDND   PICTURE  9(11).                   CI0045
            10  FILLER   PICTURE X    VALUE ')'.                        CI0045
       01               S-CL37-SSA.                                     CI0045
            10         S1-CL37-SEGNAM PICTURE X(8)                      CI0045
                                      VALUE 'CL37    '.                 CI0045
            10         S1-CL37-CCOM   PICTURE X VALUE '*'.              CI0045
            10          S-CL37-CCOD   PICTURE X(5)                      CI0045
                                      VALUE '-----'.                    CI0045
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0045
       01            S-CLA37-SSA.                                       CI0045
            11      S1-CLA37-SEGNAM PICTURE X(8)                        CI0045
                                      VALUE 'CL37    '.                 CI0045
            11      S1-CLA37-CCOM   PICTURE X VALUE '*'.                CI0045
            11       S-CLA37-CCOD   PICTURE X(5)                        CI0045
                                      VALUE '-----'.                    CI0045
            11      S1-CLA37-FLDNAM PICTURE X(9)                        CI0045
                                      VALUE '(GERED'.                   CI0045
            11       S-CLA37-OPER  PICTURE XX VALUE ' ='.               CI0045
            11       S-CLA37-GERED    PICTURE  9(8).                    CI0045
            11  FILLER   PICTURE X    VALUE ')'.                        CI0045
       01            S-CLB37-SSA.                                       CI0045
            11      S1-CLB37-SEGNAM PICTURE X(8)                        CI0045
                                      VALUE 'CL37    '.                 CI0045
            11      S1-CLB37-CCOM   PICTURE X VALUE '*'.                CI0045
            11       S-CLB37-CCOD   PICTURE X(5)                        CI0045
                                      VALUE '-----'.                    CI0045
            11      S1-CLB37-FLDNAM PICTURE X(9)                        CI0045
                                      VALUE '(GECSQ'.                   CI0045
            11       S-CLB37-OPER  PICTURE XX VALUE ' ='.               CI0045
            11       S-CLB37-GECSQ    PICTURE  S9(3)                    CI0045
                          COMPUTATIONAL-3.                              CI0045
            11  FILLER   PICTURE X    VALUE ')'.                        CI0045
       01            S-CLU37-SSA.                                       CI0045
            11      S1-CLU37-SEGNAM PICTURE X(8)                        CI0045
                                      VALUE 'CL37    '.                 CI0045
            11      S1-CLU37-CCOM   PICTURE X VALUE '*'.                CI0045
            11       S-CLU37-CCOD   PICTURE X(5)                        CI0045
                                      VALUE '-----'.                    CI0045
            11      S1-CLU37-FLDNAM PICTURE X(9)                        CI0045
                                      VALUE '(CL37K'.                   CI0045
            11       S-CLU37-OPER  PICTURE XX VALUE ' ='.               CI0045
            11       S-CLU37-CL37K.                                     CI0045
            12       S-CLU37-CLCTRC   PICTURE  9(3).                    CI0045
            11  FILLER   PICTURE X    VALUE ')'.                        CI0045
       01               S-CT01-SSA.                                     CI0045
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0045
                                      VALUE 'CT01    '.                 CI0045
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0045
            10          S-CT01-CCOD   PICTURE X(5)                      CI0045
                                      VALUE '-----'.                    CI0045
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0045
       01            S-CTU01-SSA.                                       CI0045
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0045
                                      VALUE 'CT01    '.                 CI0045
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0045
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0045
                                      VALUE '-----'.                    CI0045
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0045
                                      VALUE '(CT01K'.                   CI0045
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0045
            10       S-CTU01-CT01K.                                     CI0045
            11       S-CTU01-C299.                                      CI0045
            12       S-CTU01-CTID.                                      CI0045
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0045
            13       S-CTU01-CTIDN.                                     CI0045
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0045
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0045
            10  FILLER   PICTURE X    VALUE ')'.                        CI0045
       01               S-CT07-SSA.                                     CI0045
            10         S1-CT07-SEGNAM PICTURE X(8)                      CI0045
                                      VALUE 'CT07    '.                 CI0045
            10         S1-CT07-CCOM   PICTURE X VALUE '*'.              CI0045
            10          S-CT07-CCOD   PICTURE X(5)                      CI0045
                                      VALUE '-----'.                    CI0045
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0045
       01            S-CTU07-SSA.                                       CI0045
            10      S1-CTU07-SEGNAM PICTURE X(8)                        CI0045
                                      VALUE 'CT07    '.                 CI0045
            10      S1-CTU07-CCOM   PICTURE X VALUE '*'.                CI0045
            10       S-CTU07-CCOD   PICTURE X(5)                        CI0045
                                      VALUE '-----'.                    CI0045
            10      S1-CTU07-FLDNAM PICTURE X(9)                        CI0045
                                      VALUE '(CT07K'.                   CI0045
            10       S-CTU07-OPER  PICTURE XX VALUE ' ='.               CI0045
            10       S-CTU07-CT07K.                                     CI0045
            11       S-CTU07-C199.                                      CI0045
            12       S-CTU07-CLID.                                      CI0045
            13       S-CTU07-CLIDO    PICTURE  9(3).                    CI0045
            13       S-CTU07-CLIDN.                                     CI0045
            14       S-CTU07-CLIDNP   PICTURE  X(12).                   CI0045
            14       S-CTU07-CLIDND   PICTURE  9(8).                    CI0045
            10  FILLER   PICTURE X    VALUE ')'.                        CI0045
       01               S-CT09-SSA.                                     CI0045
            10         S1-CT09-SEGNAM PICTURE X(8)                      CI0045
                                      VALUE 'CT09    '.                 CI0045
            10         S1-CT09-CCOM   PICTURE X VALUE '*'.              CI0045
            10          S-CT09-CCOD   PICTURE X(5)                      CI0045
                                      VALUE '-----'.                    CI0045
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0045
       01            S-CTA09-SSA.                                       CI0045
            11      S1-CTA09-SEGNAM PICTURE X(8)                        CI0045
                                      VALUE 'CT09    '.                 CI0045
            11      S1-CTA09-CCOM   PICTURE X VALUE '*'.                CI0045
            11       S-CTA09-CCOD   PICTURE X(5)                        CI0045
                                      VALUE '-----'.                    CI0045
            11      S1-CTA09-FLDNAM PICTURE X(9)                        CI0045
                                      VALUE '(GERED'.                   CI0045
            11       S-CTA09-OPER  PICTURE XX VALUE ' ='.               CI0045
            11       S-CTA09-GERED    PICTURE  9(8).                    CI0045
            11  FILLER   PICTURE X    VALUE ')'.                        CI0045
       01            S-CTB09-SSA.                                       CI0045
            11      S1-CTB09-SEGNAM PICTURE X(8)                        CI0045
                                      VALUE 'CT09    '.                 CI0045
            11      S1-CTB09-CCOM   PICTURE X VALUE '*'.                CI0045
            11       S-CTB09-CCOD   PICTURE X(5)                        CI0045
                                      VALUE '-----'.                    CI0045
            11      S1-CTB09-FLDNAM PICTURE X(9)                        CI0045
                                      VALUE '(GECSQ'.                   CI0045
            11       S-CTB09-OPER  PICTURE XX VALUE ' ='.               CI0045
            11       S-CTB09-GECSQ    PICTURE  S9(3)                    CI0045
                          COMPUTATIONAL-3.                              CI0045
            11  FILLER   PICTURE X    VALUE ')'.                        CI0045
       01            S-CTU09-SSA.                                       CI0045
            11      S1-CTU09-SEGNAM PICTURE X(8)                        CI0045
                                      VALUE 'CT09    '.                 CI0045
            11      S1-CTU09-CCOM   PICTURE X VALUE '*'.                CI0045
            11       S-CTU09-CCOD   PICTURE X(5)                        CI0045
                                      VALUE '-----'.                    CI0045
            11      S1-CTU09-FLDNAM PICTURE X(9)                        CI0045
                                      VALUE '(CT09K'.                   CI0045
            11       S-CTU09-OPER  PICTURE XX VALUE ' ='.               CI0045
            11       S-CTU09-CT09K.                                     CI0045
            12       S-CTU09-CLCTRC   PICTURE  9(3).                    CI0045
            11  FILLER   PICTURE X    VALUE ')'.                        CI0045
       01               S-HM01-SSA.                                     CI0045
            10         S1-HM01-SEGNAM PICTURE X(8)                      CI0045
                                      VALUE 'HN01    '.                 CI0045
            10         S1-HM01-CCOM   PICTURE X VALUE '*'.              CI0045
            10          S-HM01-CCOD   PICTURE X(5)                      CI0045
                                      VALUE '-----'.                    CI0045
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0045
       01            S-HMC01-SSA.                                       CI0045
            12      S1-HMC01-SEGNAM PICTURE X(8)                        CI0045
                                      VALUE 'HN01    '.                 CI0045
            12      S1-HMC01-CCOM   PICTURE X VALUE '*'.                CI0045
            12       S-HMC01-CCOD   PICTURE X(5)                        CI0045
                                      VALUE '-----'.                    CI0045
            12      S1-HMC01-FLDNAM PICTURE X(9)                        CI0045
                                      VALUE '(CTIDA'.                   CI0045
            12       S-HMC01-OPER  PICTURE XX VALUE ' ='.               CI0045
            12       S-HMC01-CTIDA    PICTURE  9(3).                    CI0045
            12  FILLER   PICTURE X    VALUE ')'.                        CI0045
       01            S-HMU01-SSA.                                       CI0045
            10      S1-HMU01-SEGNAM PICTURE X(8)                        CI0045
                                      VALUE 'HN01    '.                 CI0045
            10      S1-HMU01-CCOM   PICTURE X VALUE '*'.                CI0045
            10       S-HMU01-CCOD   PICTURE X(5)                        CI0045
                                      VALUE '-----'.                    CI0045
            10      S1-HMU01-FLDNAM PICTURE X(9)                        CI0045
                                      VALUE '(HN01K'.                   CI0045
            10       S-HMU01-OPER  PICTURE XX VALUE ' ='.               CI0045
            10       S-HMU01-HN01K.                                     CI0045
            11       S-HMU01-CTID.                                      CI0045
            12       S-HMU01-CTIDA    PICTURE  9(3).                    CI0045
            12       S-HMU01-CTIDN.                                     CI0045
            13       S-HMU01-CTIDNP   PICTURE  X(13).                   CI0045
            13       S-HMU01-CTIDND   PICTURE  9(11).                   CI0045
            10  FILLER   PICTURE X    VALUE ')'.                        CI0045
       01               S-HN10-SSA.                                     CI0045
            10         S1-HN10-SEGNAM PICTURE X(8)                      CI0045
                                      VALUE 'HN10    '.                 CI0045
            10         S1-HN10-CCOM   PICTURE X VALUE '*'.              CI0045
            10          S-HN10-CCOD   PICTURE X(5)                      CI0045
                                      VALUE '-----'.                    CI0045
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0045
       01            S-HNA10-SSA.                                       CI0045
            11      S1-HNA10-SEGNAM PICTURE X(8)                        CI0045
                                      VALUE 'HN10    '.                 CI0045
            11      S1-HNA10-CCOM   PICTURE X VALUE '*'.                CI0045
            11       S-HNA10-CCOD   PICTURE X(5)                        CI0045
                                      VALUE '-----'.                    CI0045
            11      S1-HNA10-FLDNAM PICTURE X(9)                        CI0045
                                      VALUE '(CATRF1'.                  CI0045
            11       S-HNA10-OPER  PICTURE XX VALUE ' ='.               CI0045
            11       S-HNA10-CATRF1   PICTURE  9(3).                    CI0045
            11  FILLER   PICTURE X    VALUE ')'.                        CI0045
       01            S-HNB10-SSA.                                       CI0045
            11      S1-HNB10-SEGNAM PICTURE X(8)                        CI0045
                                      VALUE 'HN10    '.                 CI0045
            11      S1-HNB10-CCOM   PICTURE X VALUE '*'.                CI0045
            11       S-HNB10-CCOD   PICTURE X(5)                        CI0045
                                      VALUE '-----'.                    CI0045
            11      S1-HNB10-FLDNAM PICTURE X(9)                        CI0045
                                      VALUE '(CATRS1'.                  CI0045
            11       S-HNB10-OPER  PICTURE XX VALUE ' ='.               CI0045
            11       S-HNB10-CATRS1   PICTURE  9(3).                    CI0045
            11  FILLER   PICTURE X    VALUE ')'.                        CI0045
       01            S-HNU10-SSA.                                       CI0045
            11      S1-HNU10-SEGNAM PICTURE X(8)                        CI0045
                                      VALUE 'HN10    '.                 CI0045
            11      S1-HNU10-CCOM   PICTURE X VALUE '*'.                CI0045
            11       S-HNU10-CCOD   PICTURE X(5)                        CI0045
                                      VALUE '-----'.                    CI0045
            11      S1-HNU10-FLDNAM PICTURE X(9)                        CI0045
                                      VALUE '(HN10K'.                   CI0045
            11       S-HNU10-OPER  PICTURE XX VALUE ' ='.               CI0045
            11       S-HNU10-HN10K.                                     CI0045
            12       S-HNU10-NDAT9    PICTURE  9(8).                    CI0045
            12       S-HNU10-GESQ3    PICTURE  999.                     CI0045
            12       S-HNU10-GESQ3A   PICTURE  X(3).                    CI0045
            11  FILLER   PICTURE X    VALUE ')'.                        CI0045
       01   ZONES-UTILISATEUR PICTURE X.                                CI0045
       LINKAGE SECTION.                                                 AAAD30
       01                 PB06.                                         CI0045
            10            PB06-XDBPCB.                                  CI0045
            11            PB06-XDBDNM PICTURE  X(08).                   CI0045
            11            PB06-XSEGLV PICTURE  X(02).                   CI0045
            11            PB06-XRC    PICTURE  X(02).                   CI0045
            11            PB06-XPROPT PICTURE  X(04).                   CI0045
            11            PB06-FILLER PICTURE  S9(5)                    CI0045
                          BINARY.                                       CI0045
            11            PB06-XSEGNM PICTURE  X(08).                   CI0045
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0045
                          BINARY.                                       CI0045
            11            PB06-XSEGNB PICTURE  9(05)                    CI0045
                          BINARY.                                       CI0045
            11            PB06-XCOKEY PICTURE  X(70).                   CI0045
       01                 PC06.                                         CI0045
            10            PC06-XDBPCB.                                  CI0045
            11            PC06-XDBDNM PICTURE  X(08).                   CI0045
            11            PC06-XSEGLV PICTURE  X(02).                   CI0045
            11            PC06-XRC    PICTURE  X(02).                   CI0045
            11            PC06-XPROPT PICTURE  X(04).                   CI0045
            11            PC06-FILLER PICTURE  S9(5)                    CI0045
                          BINARY.                                       CI0045
            11            PC06-XSEGNM PICTURE  X(08).                   CI0045
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0045
                          BINARY.                                       CI0045
            11            PC06-XSEGNB PICTURE  9(05)                    CI0045
                          BINARY.                                       CI0045
            11            PC06-XCOKEY PICTURE  X(70).                   CI0045
       01                 PD06.                                         CI0045
            10            PD06-XDBPCB.                                  CI0045
            11            PD06-XDBDNM PICTURE  X(08).                   CI0045
            11            PD06-XSEGLV PICTURE  X(02).                   CI0045
            11            PD06-XRC    PICTURE  X(02).                   CI0045
            11            PD06-XPROPT PICTURE  X(04).                   CI0045
            11            PD06-FILLER PICTURE  S9(5)                    CI0045
                          BINARY.                                       CI0045
            11            PD06-XSEGNM PICTURE  X(08).                   CI0045
            11            PD06-XKEYLN PICTURE  S9(05)                   CI0045
                          BINARY.                                       CI0045
            11            PD06-XSEGNB PICTURE  9(05)                    CI0045
                          BINARY.                                       CI0045
            11            PD06-XCOKEY PICTURE  X(70).                   CI0045
      ********************************************************
      ******** INPUT AND OUTPUT SEGMENTS**********************
      ********************************************************
      *!WF DSP=K9 DSL=K9 SEL=5X FOR=I DES=2 LEV=1 PLT=05 id=15
       01                 K95X.                                         CI0045
            10            K95X-CTID   PICTURE  X(27)                    CI0045
                          VALUE                SPACE.                   CI0045
            10            K95X-DIRAY3 PICTURE  S9(4)                    CI0045
                          VALUE                ZERO                     CI0045
                          COMPUTATIONAL-3.                              CI0045
            10            K95X-CIRAP  PICTURE  XX                       CI0045
                          VALUE                SPACE.                   CI0045
            10            K95X-CQACT  PICTURE  999                      CI0045
                          VALUE                ZERO.                    CI0045
            10            K95X-CIRAT  PICTURE  999                      CI0045
                          VALUE                ZERO.                    CI0045
            10            K95X-CIRAS  PICTURE  999                      CI0045
                          VALUE                ZERO.                    CI0045
            10            K95X-AIRAC  PICTURE  S9(9)V99                 CI0045
                          VALUE                ZERO                     CI0045
                          COMPUTATIONAL-3.                              CI0045
            10            K95X-AIRAP  PICTURE  S9(9)V99                 CI0045
                          VALUE                ZERO                     CI0045
                          COMPUTATIONAL-3.                              CI0045
            10            K95X-GERTC  PICTURE  X                        CI0045
                          VALUE                SPACE.                   CI0045
       PROCEDURE DIVISION USING PB06
                                PC06
                                PD06
                                K95X.
                                                                        DOT
      *N01.      NOTE *************************************.            CI0045
      *               *                                   *             CI0045
      *               *INITIALISATIONS                    *             CI0045
      *               *                                   *             CI0045
      *               *************************************.            CI0045
       F01.      EXIT.
       F01-FN.   EXIT.
      *N02.      NOTE *************************************.
      *               *                                   *
      *               *---> MODULE INITIALIZATIONS        *
      *               *                                   *
      *               *************************************.
       F02.                                                             lv05
      *
      *N02BC.    NOTE *---> INITIALIZE FT TO ENSURE       *.
       F02BC.                                                           lv10
      *     F20 IS EXECUTED AT END OF
      *     PROCESSING
      *
           MOVE ALL    ZEROS TO FT.
       F02BC-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0045
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0045
      *               *                                   *             CI0045
      *               *FIN DE TRAITEMENT                  *             CI0045
      *               *                                   *             CI0045
      *               *************************************.            CI0045
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0045
      *N2099.    NOTE *END OF THE PROGRAM                 *.            AAAD30
       F2099.                                                           lv10
           GOBACK.                                                      AAAD30
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *MAIN PROCESSING LOOP               *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *********************************
      ** THIS PROGRAM IS A CALLED     *
      ** MODULE TO GET YEAR-TO-DATE   *
      ** CONTRIBUTION AMOUNT AT PLAN  *
      ** LEVEL. THE CALLING PROGRAM   *
      ** NEEDS TO PASS IN K95X.       *
      *********************************
      *N40AC.    NOTE *INITIALIZATION OF VARIABLES        *.
       F40AC.                                                           lv10
      *********************************
           INITIALIZE  7-ACCT-FIELDS
           7-SAVE-FIELDS
           7-SET-FLAG
           7-SAVE-PR-VALUE
           7-MISC-88-CIRAP
           7-SAVE-VARIABLES
           7-HN10-CATRF1
           7-DATE-FIELDS
           WS01-CLID
           MOVE        ZEROES TO 7-SEARCH-NDAT9
           7-HN10-NDAT9.
       F40AC-FN. EXIT.
      *N40AE.    NOTE *CHECKING WHETHER TAX REPORTING     *.
       F40AE.    IF    K95X-CTID NOT NUMERIC                            lv10
                 OR    K95X-DIRAY3 NOT NUMERIC
                 OR    K95X-CIRAP = SPACES
                 OR    K95X-CIRAT NOT NUMERIC
                 OR    K95X-CIRAS NOT NUMERIC
                 OR    K95X-CQACT NOT NUMERIC
                 NEXT SENTENCE ELSE GO TO     F40AE-FN.
      *YEAR, IRA-TYPE CODE, IRA-STATUS
      *CODE, ACCOUNT ID IS NOT NUMERIC
      *AND IRA TYPE OF CONTRIBUTION
      *CODE IS EQUAL TO SPACES
      *********************************
           MOVE        1 TO K95X-GERTC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40AE-FN. EXIT.
      *N40AG.    NOTE *CHECK WHETHER THE IRA ACCOUNT IS   *.
       F40AG.    IF    K95X-CIRAT = 003 OR 004                          lv10
                 OR    007
                 NEXT SENTENCE ELSE GO TO     F40AG-FN.
      *SEP, SRA & COVERDELL ESA IRA
      *ACCOUNT TYPES.
      *********************************
           MOVE        2 TO K95X-GERTC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40AG-FN. EXIT.
      *N40AJ.    NOTE *CHECK WHETHER THE CONTRIBUTION     *.
       F40AJ.    IF    K95X-CIRAP NOT = 'CU'                            lv10
                 AND   K95X-CIRAP NOT = 'PR'
                 NEXT SENTENCE ELSE GO TO     F40AJ-FN.
      *TYPE IS OTHER THAN 'CU' OR 'PR'
      *********************************
           MOVE        3 TO K95X-GERTC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40AJ-FN. EXIT.
      *N40AM.    NOTE *FIRST CALC THE 9'S COMP DATE       *.
       F40AM.                                                           lv10
      *********************************
           MOVE        99990000 TO 7-SEARCH-NDAT9
           SUBTRACT    K95X-DIRAY3 FROM 7-S-NDAT9-CCYY.
       F40AM-FN. EXIT.
      *N40BB.    NOTE *READ THE CONTRACT DATABASE, IF     *.
       F40BB.                                                           lv10
      *THE ACCOUNT IS FOUND THEN GET
      *THE CLIENT ID
      ********************************
           MOVE        K95X-CIRAP TO 7-CIRAP
           MOVE        K95X-CTID TO S-CTU01-CT01K
           PERFORM     F95CA THRU F95CA-FN
           MOVE        'NO' TO 7-TAXPAYER-FOUND.
                 IF    CT01-CF = 1                                      DOT
      *READ THE CT09 SEGMENT
           PERFORM     F95CC THRU F95CC-FN.
      *N40BJ.    NOTE *IF CT09 ACCESS SUCCESSFUL,         *.
       F40BJ.    IF    CT09-CF = 1                                      lv15
                 NEXT SENTENCE ELSE GO TO     F40BJ-FN.
      *MOVE CLIENT ID TO WRK STORAGE
      *********************************
           MOVE        'YES' TO 7-TAXPAYER-FOUND
           MOVE        XW05-XCOKEY (28:3) TO WS01-CLIDO
           MOVE        XW05-XCOKEY (31:20) TO WS01-CLIDN.
       F40BJ-900. GO TO F40BM-FN.
       F40BJ-FN. EXIT.
      *N40BM.    NOTE *ERROR IF NO TAXPAYER CLIENT IS     *.
       F40BM.                                                           lv15
      *FOUND
      *********************************
           DISPLAY     'ACCT NEEDS TAXPAYER'
           MOVE        4 TO K95X-GERTC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40BM-FN. EXIT.
       F40BB-FN. EXIT.
      *N40CD.    NOTE *GET ALL IRA'S FOR THAT TAXPAYER    *.
       F40CD.                                                           lv10
      *CLIENT
      *********************************
           MOVE        WS01-CLID TO S-CLU01-CLID
           PERFORM     F95CL THRU F95CL-FN.
                 IF    CL01-CF = 0                                      DOT
           DISPLAY     'TAXPAYER NOT ON CAM'
           MOVE        5 TO K95X-GERTC
           MOVE                     ALL '1' TO FT GO TO F20.
      *N40CE.    NOTE *ACCUMULATE CURRENT YEAR            *.
       F40CE.    IF    K95X-CIRAP = 'CU'                                lv15
                 NEXT SENTENCE ELSE GO TO     F40CE-FN.
      *YEAR-TO-DATE CONTRIBUTION AT
      *PLAN LEVEL
      *********************************
           PERFORM     F91 THRU F91-FN
           MOVE        7-ACCT-AIRCU TO K95X-AIRAC
           MOVE        ZEROES TO K95X-AIRAP
           MOVE                     ALL '1' TO FT GO TO F20.
       F40CE-900. GO TO F40CF-FN.
       F40CE-FN. EXIT.
      *N40CF.    NOTE *ACCUMULATE PRIOR YEAR              *.
       F40CF.                                                           lv15
      *YEAR-TO-DATE CONTRIBUTION AT
      *PLAN LEVEL. ADD THE CONTRIBUTION
      *FOR ALL THE ACCOUNTS PRIOR YEAR
      *CONTRIBUTION FOR CURRENT YEAR
      *AND CURRENT YEARS CONTRIBUTION
      *FOR PRIOR YEAR.
      *********************************
           MOVE        7-INITIAL-FLDS TO 7-SAVE-FIELDS
           7-ACCT-FIELDS
           PERFORM     F91 THRU F91-FN
           MOVE        7-ACCT-AIRCU TO 7-SAVE-AMT-PR-CU
           MOVE        7-ACCT-AIRPR TO 7-SAVE-AMT-PR
           COMPUTE     7-SAVE-TOTAL-PR =
           7-SAVE-AMT-PR-CU
           + 7-SAVE-AMT-PR
           MOVE        7-SAVE-TOTAL-PR TO K95X-AIRAP
           MOVE        ZEROES TO K95X-AIRAC
           MOVE                     ALL '1' TO FT GO TO F20.
       F40CF-FN. EXIT.
       F40CD-FN. EXIT.
       F40-FN.   EXIT.
      *N79.      NOTE *************************************.
      *               *                                   *
      *               *---> NORMAL TERMINATION            *
      *               *                                   *
      *               *************************************.
       F79.                                                             lv05
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F79-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N91.      NOTE *************************************.
      *               *                                   *
      *               *PLAN LEVEL CHECK                   *
      *               *                                   *
      *               *************************************.
       F91.                                                             lv05
      *********************************
      *N91CA.    NOTE *READ THE CL36 DATABASE TO GET      *.
       F91CA.                                                           lv10
      *ACCOUNTS
      *********************************
           PERFORM     F95CN THRU F95CN-FN.
      *N91CH.    NOTE *READ THE CL37 DATABASE TO GET      *.
       F91CH.    IF    CL36-CF = 1                                      lv15
                 NEXT SENTENCE ELSE GO TO     F91CH-FN.
      *THE ACCOUNTS ROLES
      *********************************.
           MOVE        CL36-CTID TO S-CLU36-CTID                        DOT
           MOVE        'N' TO 7-TAXPAYER
           7-OWNER
      *READ THE CL37 TO CHECK THE ROLES
           PERFORM     F95CP THRU F95CP-FN.
      *N91CJ.    NOTE *CHECK THE ROLES OF THE ACCOUNTS    *.
       F91CJ.    IF    CL37-CF = 1                                      lv20
                 AND   (7-88-NOT-TAXPAYER OR
                       7-88-NOT-OWNER)
                 AND   CL37-GERED = ZERO
                 NEXT SENTENCE ELSE GO TO     F91CJ-FN.
      *********************************
                 IF    CL37-CLCTRC = 001                                DOT
           MOVE        'Y' TO 7-OWNER.
                 IF    CL37-CLCTRC = 004                                DOT
           MOVE        'Y' TO 7-TAXPAYER.
      *N91CK.    NOTE *READ THE NEXT CL37 SEGMENT         *.
       F91CK.                                                           lv25
      *********************************
           PERFORM     F95CP THRU F95CP-FN.
       F91CK-FN. EXIT.
       F91CJ-900. GO TO F91CJ.
       F91CJ-FN. EXIT.
      *N91CM.    NOTE *IF THE ACCOUNT ROLE HAS NO OWNER   *.
       F91CM.    IF    (7-88-NOT-TAXPAYER                               lv20
                 OR    7-88-NOT-OWNER)
                 NEXT SENTENCE ELSE GO TO     F91CM-FN.
      *AND TAXPAYER, READ THE NEXT ACCT
      *********************************
      *READ THE NEXT CL36 SEGEMNT
           PERFORM     F95CN THRU F95CN-FN.
       F91CM-900. GO TO F91CN-FN.
       F91CM-FN. EXIT.
      *N91CN.    NOTE *CHECK THE ACCOUNT WITH OWNER AND   *.
       F91CN.                                                           lv20
      *TAXPAYER
      *********************************
      *N91CO.    NOTE *READ THE CONTRACT DATABASE         *.
       F91CO.                                                           lv25
      *********************************
           MOVE        S-CLU36-CTID TO S-CTU01-CT01K
           PERFORM     F95CA THRU F95CA-FN.
                 IF    CT01-CF = 0                                      DOT
           DISPLAY     'PLAN EDIT NOT CMPLT'
           MOVE        6 TO K95X-GERTC
           MOVE                     ALL '1' TO FT GO TO F20.
       F91CO-FN. EXIT.
      *N91FH.    NOTE *ACCUMULATE CONTRIBUTION FOR ACCT   *.
       F91FH.    IF    CT01-CQACT = 001                                 lv25
                 AND   CT01-CIRAT NOT = 004
                 AND   CT01-CIRAT NOT = 007
                 NEXT SENTENCE ELSE GO TO     F91FH-FN.
      *UNDER THAT PLAN EXCEPT FOR SRA,
      *AND COVERDELL ESA
      *********************************
      *
      *N91FM.    NOTE *ACCUMULATE CONTRIBUTION FOR ACCT   *.
       F91FM.                                                           lv30
      *********************************
           MOVE        S-CTU01-CTID TO 7-SAVE-CTID
           PERFORM     F96 THRU F96-FN
           MOVE        '0' TO IK.
       F91FM-FN. EXIT.
       F91FH-FN. EXIT.
      *N91KL.    NOTE *READ THE NEXT CL36 SEGMENT         *.
       F91KL.                                                           lv25
      *********************************
           PERFORM     F95CN THRU F95CN-FN.
       F91KL-FN. EXIT.
       F91CN-FN. EXIT.
       F91CH-900. GO TO F91CH.
       F91CH-FN. EXIT.
       F91CA-FN. EXIT.
       F91-FN.   EXIT.
      *N93EA.    NOTE *DATA BASE I/O ERROR PROCESSING     *.            AAAD30
       F93EA.                                                           lv10
           MOVE        '1' TO IK.                                       AAAD30
                 IF    XW05-XRC NOT = '  '                              DOT
                 AND   XW05-XRC NOT = 'GA'                              AAAD30
                 AND   XW05-XRC NOT = 'GB'                              AAAD30
                 AND   XW05-XRC NOT = 'GE'                              AAAD30
                 AND   XW05-XRC NOT = 'GK'                              AAAD30
                 AND   XW05-XRC NOT = 'II'                              AAAD30
      *NON STANDARD I/O ERROR                                           AAAD30
           MOVE        XW05-XSEGNB TO XW05-XSEGN9                       AAAD30
           MOVE        XW05-XKEYLN TO XW05-XKEYL9                       AAAD30
           DISPLAY     '******************************'                 AAAD30
           DISPLAY     '*     DATA BASE ERROR        *'                 AAAD30
           DISPLAY     '******************************'                 AAAD30
           DISPLAY     'STOP OF           ' PROGR                       AAAD30
           DISPLAY     'FUNCTION          ' XW05-XNFPAC                 AAAD30
           DISPLAY     'DATA BASE         ' XW05-XDBDNM
           DISPLAY     'SEGMENT LEVEL     ' XW05-XSEGLV
           DISPLAY     'RETURN CODE       ' XW05-XRC
           DISPLAY     'PROCESS OPTION    ' XW05-XPROPT
           DISPLAY     'SEGMENT NAME      ' XW05-XSEGNM
           DISPLAY     'KEY LENGTH        ' XW05-XKEYL9
           DISPLAY     'SEGMENT NUMBER    ' XW05-XSEGN9
           DISPLAY     'CONCATENATED KEY  ' XW05-XCOKEY                 AAAD30
           CALL        DLIERROR USING XW05-XDBPCB                       AAAD30
           MOVE                    ALL '1' TO FT GO TO F20.             AAAD30
                 IF    XW05-XRC = '  '                                  DOT
      *STANDARD I/O ERROR                                               AAAD30
           MOVE        ZERO TO IK.                                      AAAD30
       F93EA-FN. EXIT.
      *N93EC.    NOTE *DATA BASE LOGICAL ERROR            *.            AAAD30
       F93EC.                                                           lv10
      *STOP OF PGM DUE TO LOGICAL ERROR                                 AAAD30
      *DOES NOT FORCE AN ABEND                                          AAAD30
           MOVE        XW05-XSEGNB TO XW05-XSEGN9                       AAAD30
           MOVE        XW05-XKEYLN TO XW05-XKEYL9                       AAAD30
           DISPLAY     '******************************'                 AAAD30
           DISPLAY     '*       LOGICAL ERROR        *'                 AAAD30
           DISPLAY     '******************************'                 AAAD30
           DISPLAY     'STOP OF           ' PROGR                       AAAD30
           DISPLAY     'FUNCTION          ' XW05-XNFPAC                 AAAD30
           DISPLAY     'DATA BASE         ' XW05-XDBDNM                 AAAD30
           DISPLAY     'SEGMENT LEVEL     ' XW05-XSEGLV                 AAAD30
           DISPLAY     'RETURN CODE       ' XW05-XRC                    AAAD30
           DISPLAY     'PROCESS OPTION    ' XW05-XPROPT                 AAAD30
           DISPLAY     'SEGMENT NAME      ' XW05-XSEGNM                 AAAD30
           DISPLAY     'KEY LENGTH        ' XW05-XKEYL9                 AAAD30
           DISPLAY     'SEGMENT NUMBER    ' XW05-XSEGN9                 AAAD30
           DISPLAY     'CONCATENED KEY    ' XW05-XCOKEY                 AAAD30
           CALL        DLIERROR USING XW05-XDBPCB
           MOVE                    ALL '1' TO FT GO TO F20.             AAAD30
       F93EC-FN. EXIT.
      *N95AB.    NOTE *CALL GU ON CL36                    *.
       F95AB.                                                           lv10
           MOVE        '95AB-100 GU' TO XW05-XNFPAC                     AAAD36
           CALL        'CBLTDLI' USING XW05-XGU                         AAAD36
           PB06 CL36                                                    AAAD36
           S-CLU01-SSA                                                  AAAD36
           MOVE        PB06 TO XW05-XDBPCB                              AAAD36
           PERFORM     F93EA THRU F93EA-FN.                             AAAD36
       F95AB-FN. EXIT.
      *N95CA.    NOTE *CALL GU ON CT01                    *.
       F95CA.                                                           lv10
           MOVE        '95CA-100 GU' TO XW05-XNFPAC                     AAAD36
           CALL        'CBLTDLI' USING XW05-XGU                         AAAD36
           PC06 CT01                                                    AAAD36
           S-CTU01-SSA                                                  AAAD36
           MOVE        PC06 TO XW05-XDBPCB                              AAAD36
           PERFORM     F93EA THRU F93EA-FN.                             AAAD36
                 IF    IK = '0'                                         DOT
           MOVE        1 TO CT01-CF.
                 IF    IK = '1'                                         DOT
           MOVE        0 TO CT01-CF.
       F95CA-FN. EXIT.
      *N95CB.    NOTE *CALL GN ON CT07                    *.
       F95CB.                                                           lv10
           MOVE        '95CB-100 GN' TO XW05-XNFPAC                     AAAD36
           CALL        'CBLTDLI' USING XW05-XGN                         AAAD36
           PC06 CT07                                                    AAAD36
           S-CTU01-SSA                                                  AAAD36
           S-CT07-SSA
           MOVE        PC06 TO XW05-XDBPCB                              AAAD36
           PERFORM     F93EA THRU F93EA-FN.                             AAAD36
                 IF    IK = '0'                                         DOT
           MOVE        1 TO CT07-CF.
                 IF    IK = '1'                                         DOT
           MOVE        0 TO CT07-CF.
       F95CB-FN. EXIT.
      *N95CC.    NOTE *CALL GN ON CT09                    *.
       F95CC.                                                           lv10
           MOVE        '95CC-100 GN' TO XW05-XNFPAC                     AAAD36
           CALL        'CBLTDLI' USING XW05-XGN                         AAAD36
           PC06 CT09                                                    AAAD36
           S-CTU01-SSA                                                  AAAD36
           S-CT07-SSA
           7-CTA09-1-SSA
           PERFORM     F93EA THRU F93EA-FN.                             AAAD36
                 IF    IK = '0'                                         DOT
           MOVE        1 TO CT09-CF.
                 IF    IK = '1'                                         DOT
           MOVE        0 TO CT09-CF.
       F95CC-FN. EXIT.
      *N95CL.    NOTE *CALL GU ON CL01                    *.
       F95CL.                                                           lv10
           MOVE        '95CL-100 GU' TO XW05-XNFPAC                     AAAD36
           CALL        'CBLTDLI' USING XW05-XGU                         AAAD36
           PB06 CL01                                                    AAAD36
           S-CLU01-SSA                                                  AAAD36
           MOVE        PB06 TO XW05-XDBPCB                              AAAD36
           PERFORM     F93EA THRU F93EA-FN.                             AAAD36
                 IF    IK = '0'                                         DOT
           MOVE        1 TO CL01-CF.
                 IF    IK = '1'                                         DOT
           MOVE        0 TO CL01-CF.
       F95CL-FN. EXIT.
      *N95CN.    NOTE *CALL GN ON CL36                    *.
       F95CN.                                                           lv10
           MOVE        '95CN-100 GN' TO XW05-XNFPAC                     AAAD36
           CALL        'CBLTDLI' USING XW05-XGN                         AAAD36
           PB06 CL36                                                    AAAD36
           S-CLU01-SSA                                                  AAAD36
           S-CL36-SSA
           MOVE        PB06 TO XW05-XDBPCB                              AAAD36
           PERFORM     F93EA THRU F93EA-FN.                             AAAD36
                 IF    IK = '0'                                         DOT
           MOVE        1 TO CL36-CF.
                 IF    IK = '1'                                         DOT
           MOVE        0 TO CL36-CF.
       F95CN-FN. EXIT.
      *N95CP.    NOTE *CALL GN ON CL37                    *.
       F95CP.                                                           lv10
           MOVE        '95CP-100 GN' TO XW05-XNFPAC                     AAAD36
           CALL        'CBLTDLI' USING XW05-XGN                         AAAD36
           PB06 CL37                                                    AAAD36
           S-CLU01-SSA                                                  AAAD36
           S-CLU36-SSA
           S-CL37-SSA
           MOVE        PB06 TO XW05-XDBPCB                              AAAD36
           PERFORM     F93EA THRU F93EA-FN.                             AAAD36
                 IF    IK = '0'                                         DOT
           MOVE        1 TO CL37-CF.
                 IF    IK = '1'                                         DOT
           MOVE        0 TO CL37-CF.
       F95CP-FN. EXIT.
      *N95HN.    NOTE *CALL GN ON HN01                    *.
       F95HN.                                                           lv10
           MOVE        '95HN-100 GU' TO XW05-XNFPAC                     AAAD36
           CALL        'CBLTDLI' USING XW05-XGU                         AAAD36
           PD06 HM01                                                    AAAD36
           S-HMU01-SSA                                                  AAAD36
           MOVE        PD06 TO XW05-XDBPCB                              AAAD36
           PERFORM     F93EA THRU F93EA-FN.                             AAAD36
                 IF    IK = '0'                                         DOT
           MOVE        1 TO HN01-CF.
                 IF    IK = '1'                                         DOT
           MOVE        0 TO HN01-CF.
       F95HN-FN. EXIT.
      *N95HP.    NOTE *CALL GU ON HN10                    *.
       F95HP.                                                           lv10
           MOVE        '95HP-100 GU' TO XW05-XNFPAC                     AAAD36
           CALL        'CBLTDLI' USING XW05-XGU                         AAAD36
           PD06 HN10                                                    AAAD36
           S-HMU01-SSA                                                  AAAD36
           S-HNU10-SSA
           MOVE        PD06 TO XW05-XDBPCB                              AAAD36
           PERFORM     F93EA THRU F93EA-FN.                             AAAD36
       F95HP-FN. EXIT.
      *N95HT.    NOTE *CALL GN ON HN10                    *.
       F95HT.                                                           lv10
           MOVE        '95HT-100 GN' TO XW05-XNFPAC                     AAAD36
           CALL        'CBLTDLI' USING XW05-XGN                         AAAD36
           PD06 HN10                                                    AAAD36
           S-HMU01-SSA                                                  AAAD36
           S-HN10-SSA
           MOVE        PD06 TO XW05-XDBPCB                              AAAD36
           PERFORM     F93EA THRU F93EA-FN.                             AAAD36
       F95HT-FN. EXIT.
      *N96.      NOTE *************************************.
      *               *                                   *
      *               *ACCUMULATE CURRENT AMOUNTS         *
      *               *                                   *
      *               *************************************.
       F96.                                                             lv05
      *==> SEARCH THROUGH THE HISTORY
      *==> DATABASE TO ACCUMULATE THE
      *==> AMOUNT PREVIOUSLY CONTRIB-
      *==> UTED TO AN ACCOUNT.
      *N96BD.    NOTE *INITIALIZE VARIABLES               *.
       F96BD.                                                           lv10
      *******************************
           MOVE        7-SAVE-CTID TO S-HMU01-HN01K
           MOVE        ZERO TO S-HNU10-HN10K
           MOVE        7-SEARCH-NDAT9 TO S-HNU10-NDAT9
           MOVE        '>=' TO S-HNU10-OPER
           MOVE        '0' TO IK
           MOVE        7-INITIAL-FLDS TO 7-SAVE-FIELDS.
       F96BD-FN. EXIT.
      *N96DD.    NOTE *PROCESS FIRST HISTORY SEGMENT      *.
       F96DD.                                                           lv10
      *********************************
           PERFORM     F95HP THRU F95HP-FN
           MOVE        ' =' TO S-HNU10-OPER.
                 IF    IK = '1'                                         DOT
      *HIST REC NOT FOUND
               GO TO     F96-FN.
           MOVE        HN10-CATRF1 TO 7-HN10-CATRF1                     DOT
           MOVE        HN10-NDAT9 TO 7-HN10-NDAT9.
                 IF    (NOT 7-88-PRIOR-CONTRIB) AND                     DOT
                       (7-HN10-NDAT9-CCYY >
                       7-S-NDAT9-CCYY)
           MOVE        '1' TO IK
               GO TO     F96-FN.
                 IF    (7-88-PRIOR-CONTRIB) AND                         DOT
                       (7-HN10-NDAT9-CCYY >
                       7-S-NDAT9-CCYY + 1)
           MOVE        '1' TO IK
               GO TO     F96-FN.
                 IF    7-HN10-NDAT9-CCYY <                              DOT
                       7-S-NDAT9-CCYY
               GO TO     F96DD-FN.
                 IF    7-88-HN10-ALL-CONT                               DOT
           MOVE        7-INITIAL-FLDS TO 7-SAVE-FIELDS
           MOVE        HN10-CIRAP TO 7-HN10-CIRAP
           PERFORM     F97 THRU F97-FN.
       F96DD-FN. EXIT.
      *N96DG.    NOTE *CHECK IK                           *.
       F96DG.    IF    IK = '1'                                         lv10
                 NEXT SENTENCE ELSE GO TO     F96DG-FN.
               GO TO     F96-FN.
       F96DG-FN. EXIT.
      *N96ED.    NOTE *ACCUMULATE ALL CONTRIBUTIONS       *.
       F96ED.                       GO TO     F96ED-B.                  lv10
       F96ED-A.
                 IF    IK = '1'
                                    GO TO     F96ED-FN.
       F96ED-B.
           MOVE        HN10-NDAT9 TO 7-HN10-NDAT9.
                 IF    (NOT 7-88-PRIOR-CONTRIB) AND                     DOT
                       (7-HN10-NDAT9-CCYY >
                       7-S-NDAT9-CCYY)
           MOVE        '1' TO IK
               GO TO     F96ED-900.
                 IF    (7-88-PRIOR-CONTRIB) AND                         DOT
                       (7-HN10-NDAT9-CCYY >
                       7-S-NDAT9-CCYY + 1)
           MOVE        '1' TO IK
               GO TO     F96ED-900.
                 IF    7-HN10-NDAT9-CCYY <                              DOT
                       7-S-NDAT9-CCYY
           PERFORM     F95HT THRU F95HT-FN
               GO TO     F96ED-900.
           MOVE        HN10-CATRF1 TO 7-HN10-CATRF1.                    DOT
                 IF    7-88-HN10-ALL-CONT                               DOT
           MOVE        7-INITIAL-FLDS TO 7-SAVE-FIELDS
           MOVE        HN10-CIRAP TO 7-HN10-CIRAP
           PERFORM     F97 THRU F97-FN
                 ELSE
           PERFORM     F95HT THRU F95HT-FN.
       F96ED-900. GO TO F96ED-A.
       F96ED-FN. EXIT.
       F96-FN.   EXIT.
      *N97.      NOTE *************************************.
      *               *                                   *
      *               *CALCULATE EFFECTIVE AMT OF CONT.   *
      *               *                                   *
      *               *************************************.
       F97.                                                             lv05
      *==> CALCULATE THE PLACEMENT OF
      *==> A CONTRIBUTION'S FUNDS IN-
      *==> CLUDING ALL APPLIED ADJUST-
      *==> MENTS AND REVERSALS.
      *N97BD.    NOTE *INITIALIZE THE VARIABLES           *.
       F97BD.                                                           lv10
      *********************************
           MOVE        7-INITIAL-FLDS TO 7-SAVE-FIELDS.
      *N97BG.    NOTE *STORE CONTRIBUTION FIELDS          *.
       F97BG.    IF    7-88-HN10-CONTRIBUTION                           lv15
                 OR    7-88-HN10-CONT-BC
                 NEXT SENTENCE ELSE GO TO     F97BG-FN.
                 IF    7-88-HN10-CURR-CONTRIB                           DOT
           MOVE        HN10-AIRCT TO 7-SAVE-AIRCU.
                 IF    7-88-HN10-PRIOR-CONTRIB                          DOT
           MOVE        HN10-AIRCT TO 7-SAVE-AIRPR.
       F97BG-FN. EXIT.
      *N97BK.    NOTE *STORE THE ADJUSTMENT/BBF FIELDS    *.
       F97BK.    IF    7-88-HN10-CONT-ADJ                               lv15
                 OR    7-88-HN10-CONT-BF
                 NEXT SENTENCE ELSE GO TO     F97BK-FN.
           MOVE        HN10-AIRCT TO 7-SAVE-AIRCT
           MOVE        HN10-AIRCU TO 7-SAVE-AIRCU
           MOVE        HN10-AIRPR TO 7-SAVE-AIRPR.
       F97BK-FN. EXIT.
       F97BD-FN. EXIT.
      *N97ED.    NOTE *READ THROUGH SUB-SEQ SEGS          *.
       F97ED.                       GO TO     F97ED-B.                  lv10
       F97ED-A.
                 IF    IK = '1' OR
                       HN10-GESQ3A = '000'
                                    GO TO     F97ED-FN.
       F97ED-B.
      *IN ONE SEQUENCE GROUP
      *********************************
           PERFORM     F95HT THRU F95HT-FN.
                 IF    IK = '1' OR                                      DOT
                       HN10-GESQ3A = '000'
               GO TO     F97ED-900.
           MOVE        HN10-CATRF1 TO 7-HN10-CATRF1                     DOT
      *==>
      *==> SKIP REVERSALS
      *==>
                 IF    7-88-HN10-CONT-REV                               DOT
                 OR    7-88-HN10-GEN-CONT-REV
               GO TO     F97ED-900.
           MOVE        7-INITIAL-FLDS TO 7-SAVE-FIELDS                  DOT
           PERFORM     F97BD THRU F97BD-FN.
       F97ED-900. GO TO F97ED-A.
       F97ED-FN. EXIT.
      *N97HD.    NOTE *ADD TRAN AMOUNTS TO ACCT ACCUM.    *.
       F97HD.    IF    7-88-PRIOR-CONTRIB                               lv10
                 NEXT SENTENCE ELSE GO TO     F97HD-FN.
      *********************************
                 IF    7-HN10-NDAT9-CCYY =                              DOT
                       7-S-NDAT9-CCYY
           ADD         7-SAVE-AIRPR TO 7-ACCT-AIRPR.
                 IF    7-HN10-NDAT9-CCYY =                              DOT
                       7-S-NDAT9-CCYY + 1
           ADD         7-SAVE-AIRCU TO 7-ACCT-AIRCU.
       F97HD-FN. EXIT.
      *N97LD.    NOTE *ADD TRAN AMOUNTS TO ACCT ACCUM.    *.
       F97LD.    IF    7-88-CURR-CONTRIB                                lv10
                 NEXT SENTENCE ELSE GO TO     F97LD-FN.
      *********************************
           ADD         7-SAVE-AIRCU TO 7-ACCT-AIRCU.
       F97LD-FN. EXIT.
       F97-FN.   EXIT.
