       IDENTIFICATION DIVISION.                                         CI0425
       PROGRAM-ID.  CI0425P.                                            CI0425
      *AUTHOR.         FA BA ARR DETAIL RULES.                          CI0425
      *DATE-COMPILED.   09/08/14.                                       CI0425
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2000                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE OST    SYSTEM AND ALL INFORMATION RELATING THERETO,    *ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE OST    SYSTEM AND ALL            *ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE OST          *ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 2000                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0425
       CONFIGURATION SECTION.                                           CI0425
       SOURCE-COMPUTER. IBM-370.                                        CI0425
       OBJECT-COMPUTER. IBM-370.                                        CI0425
       DATA DIVISION.                                                   CI0425
       WORKING-STORAGE SECTION.                                         CI0425
       01                 AA10.                                         CI0425
            10            AA10-AE00.                                    CI0425
            11            AA10-ALCIDN PICTURE  9(11).                   CI0425
            10            AA10-AE01.                                    CI0425
            11            AA10-FILLER PICTURE  X(12).                   CI0425
            11            AA10-DLAUP  PICTURE  9(8).                    CI0425
            11            AA10-FILLER PICTURE  S9(07)                   CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-FILLER PICTURE  9(8).                    CI0425
            11            AA10-FILLER PICTURE  S9(07)                   CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-FILLER PICTURE  9(8).                    CI0425
            11            AA10-FILLER PICTURE  S9(07)                   CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-FILLER PICTURE  X(311).                  CI0425
            10            AA10-AE02                                     CI0425
                          REDEFINES            AA10-AE01.               CI0425
            11            AA10-FILLER PICTURE  9.                       CI0425
            11            AA10-ALCOMP PICTURE  99.                      CI0425
            11            AA10-CRTYP  PICTURE  9(4).                    CI0425
            11            AA10-FILLER PICTURE  9.                       CI0425
            11            AA10-CPOST  PICTURE  99.                      CI0425
            11            AA10-GEHCDI PICTURE  9(3).                    CI0425
            11            AA10-FILLER PICTURE  9(8).                    CI0425
            11            AA10-FILLER PICTURE  9(7).                    CI0425
            11            AA10-FILLER PICTURE  X.                       CI0425
            11            AA10-ALPLDT PICTURE  9(8).                    CI0425
            11            AA10-DENEX  PICTURE  9(8).                    CI0425
            11            AA10-CENXC1 PICTURE  9(3).                    CI0425
            11            AA10-FILLER PICTURE  99.                      CI0425
            11            AA10-CROOR  PICTURE  99.                      CI0425
            11            AA10-CREIN  PICTURE  99.                      CI0425
            11            AA10-FILLER PICTURE  X.                       CI0425
            11            AA10-ALAPST PICTURE  99.                      CI0425
            11            AA10-ALSTSA PICTURE  XX.                      CI0425
            11            AA10-FILLER PICTURE  99.                      CI0425
            11            AA10-CPCAL  PICTURE  9.                       CI0425
            11            AA10-CNAEX  PICTURE  9.                       CI0425
            11            AA10-CSUSI  PICTURE  99.                      CI0425
            11            AA10-FILLER PICTURE  99.                      CI0425
            11            AA10-FILLER PICTURE  99.                      CI0425
            11            AA10-FILLER PICTURE  9(8).                    CI0425
            11            AA10-FILLER PICTURE  9.                       CI0425
            11            AA10-FILLER PICTURE  X(10).                   CI0425
            11            AA10-FILLER PICTURE  99.                      CI0425
            11            AA10-FILLER PICTURE  X.                       CI0425
            11            AA10-FILLER PICTURE  99.                      CI0425
            11            AA10-CAPL   PICTURE  9.                       CI0425
            11            AA10-FILLER PICTURE  9.                       CI0425
            11            AA10-FILLER PICTURE  999.                     CI0425
            11            AA10-FILLER PICTURE  999.                     CI0425
            11            AA10-FILLER PICTURE  999.                     CI0425
            11            AA10-CSTWH  PICTURE  9(8).                    CI0425
            11            AA10-FILLER PICTURE  9(8).                    CI0425
            11            AA10-FILLER PICTURE  99.                      CI0425
            11            AA10-FILLER PICTURE  9.                       CI0425
            11            AA10-FILLER PICTURE  9.                       CI0425
            11            AA10-FILLER PICTURE  99.                      CI0425
            11            AA10-FILLER PICTURE  999.                     CI0425
            11            AA10-FILLER PICTURE  999.                     CI0425
            11            AA10-CPRPM  PICTURE  9(3).                    CI0425
            11            AA10-FILLER PICTURE  99.                      CI0425
            11            AA10-FILLER PICTURE  9(6).                    CI0425
            11            AA10-FILLER PICTURE  99.                      CI0425
            11            AA10-FILLER PICTURE  999.                     CI0425
            11            AA10-DTRCM  PICTURE  9(8).                    CI0425
            11            AA10-DLATR  PICTURE  9(8).                    CI0425
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-ALPMOD PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-CRSBN  PICTURE  99.                      CI0425
            11            AA10-FILLER PICTURE  X.                       CI0425
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-FILLER PICTURE  9.                       CI0425
            11            AA10-FILLER PICTURE  X.                       CI0425
            11            AA10-FILLER PICTURE  XX.                      CI0425
            11            AA10-FILLER PICTURE  99.                      CI0425
            11            AA10-CTRHO  PICTURE  9(8).                    CI0425
            11            AA10-FILLER PICTURE  9.                       CI0425
            11            AA10-FILLER PICTURE  99.                      CI0425
            11            AA10-FILLER PICTURE  99.                      CI0425
            11            AA10-CTSGD  PICTURE  9(8).                    CI0425
            11            AA10-IANRD  PICTURE  9.                       CI0425
            11            AA10-ALINNO PICTURE  99.                      CI0425
            11            AA10-ALSANN PICTURE  9(5).                    CI0425
            11            AA10-FILLER PICTURE  S9(9)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-ALPAGR PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-ALRISK PICTURE  S9(9)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-ALAPIT PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-FILLER PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-FILLER PICTURE  X.                       CI0425
            11            AA10-CADPR  PICTURE  9.                       CI0425
            11            AA10-AAPRT  PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-NREPN1 PICTURE  9(06).                   CI0425
            11            AA10-CCST1  PICTURE  9.                       CI0425
            11            AA10-CESRD  PICTURE  9(3).                    CI0425
            11            AA10-ALLRT  PICTURE  S9V99                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-ANTPAA PICTURE  S9(5)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-FILLER PICTURE  S9(5)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-ALDDUE PICTURE  9(08).                   CI0425
            11            AA10-ALMODE PICTURE  99.                      CI0425
            11            AA10-FILLER PICTURE  X(08).                   CI0425
            11            AA10-CTCUS1 PICTURE  99.                      CI0425
            11            AA10-CNPPR  PICTURE  9(03).                   CI0425
            11            AA10-FILLER PICTURE  9.                       CI0425
            11            AA10-FILLER PICTURE  9(03).                   CI0425
            11            AA10-ITMEC  PICTURE  X(1).                    CI0425
            11            AA10-IMCDI  PICTURE  X.                       CI0425
            11            AA10-LSIDTE PICTURE  9(08).                   CI0425
            11            AA10-PLINE  PICTURE  S9V99                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-ATSA8  PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-ATSA9  PICTURE  S9(05)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-CRATS  PICTURE  X.                       CI0425
            11            AA10-PPTKN  PICTURE  S9(3)V9(6)               CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            AA10-FILLER PICTURE  X(24).                   CI0425
       01                 AA85.                                         CI0425
            10            AA85-DUWAC  PICTURE  9(8).                    CI0425
            10            AA85-CUWAC  PICTURE  99.                      CI0425
            10            AA85-CRFAC  PICTURE  9(6).                    CI0425
            10            AA85-IIAAF  PICTURE  9.                       CI0425
            10            AA85-DPOLI  PICTURE  9(8).                    CI0425
            10            AA85-DCONM  PICTURE  9(8).                    CI0425
            10            AA85-DBYR   PICTURE  99.                      CI0425
            10            AA85-DLMED  PICTURE  9(8).                    CI0425
            10            AA85-LSIDTE PICTURE  9(08).                   CI0425
            10            AA85-GESTD  PICTURE  9(8).                    CI0425
            10            AA85-CRBTR  PICTURE  9(6).                    CI0425
            10            AA85-CBTR   PICTURE  X.                       CI0425
            10            AA85-PSBTR  PICTURE  S9(3)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            AA85-ANTPF  PICTURE  S9(3)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            AA85-QDYT1  PICTURE  99.                      CI0425
            10            AA85-AEPT1  PICTURE  S9(3)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            AA85-QDYT2  PICTURE  99.                      CI0425
            10            AA85-AEPT2  PICTURE  S9(3)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            AA85-CRFER  PICTURE  9(6).                    CI0425
            10            AA85-PRADB  PICTURE  S99V9                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            AA85-PRWP   PICTURE  S99V9                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            AA85-PRNOM  PICTURE  S99V9                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            AA85-CDETH  PICTURE  X.                       CI0425
            10            AA85-CRFSP  PICTURE  X(6).                    CI0425
            10            AA85-CREDI  PICTURE  X(16).                   CI0425
            10            AA85-CRBD   PICTURE  X(6).                    CI0425
            10            AA85-CFRTR  PICTURE  X.                       CI0425
            10            AA85-ANTPG  PICTURE  S9(3)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            AA85-ATEPF  PICTURE  S9(3)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            AA85-CRFRT  PICTURE  X(6).                    CI0425
            10            AA85-QDYT3  PICTURE  99.                      CI0425
            10            AA85-CRFEX  PICTURE  9(6).                    CI0425
            10            AA85-AAPEB  PICTURE  S9(3)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            AA85-AAPER  PICTURE  S9(3)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            AA85-ARGAP  PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            AA85-CWAE   PICTURE  XX.                      CI0425
            10            AA85-IMIBP  PICTURE  X.                       CI0425
            10            AA85-CNIR   PICTURE  XX.                      CI0425
            10            AA85-CMINM  PICTURE  XX.                      CI0425
      ******************************************************************
      ** ACCOUNT AND ARRANGEMENT SWITCHES AND COUNTERS                 *
      ******************************************************************
       01    CLIENT-SUITABILITY            PIC X.
       01    ACCOUNT-SUITABILITY           PIC X.
       01    FUND-VALERR                   PIC X.
       01    CLIENT-MINOR                  PIC X.
       01    BANK-FIELDS.
           05  BANK-COMMON.
               10  BANK-INACTIVE           PIC X.
               10  BANK-HOLD               PIC X.
       01    ACCT-FIELDS.
           05  ACCT-COMMON.
               10  ACCT-VALERR             PIC X.
               10  ACCT-INVALID            PIC X.
               10  ACCT-DEAD               PIC X.
               10  ACCT-NOT-MINOR          PIC X.
               10  ACCT-PENSION            PIC X.
               10  ACCT-IRA                PIC X.
               10  ACCT-INACTIVE           PIC X.
               10  ACCT-PENDING            PIC X.
               10  ACCT-SUITABILITY        PIC X.
               10  ACCT-NO-TAXPAYER        PIC X.
               10  ACCT-SHELL              PIC X.
           05  ANNT-COMMON.
               10  ANNT-TSA                PIC X.
               10  ANNT-SHEARSON           PIC X.
               10  ANNT-FIREFUND           PIC X.
               10  ANNT-PAYOUT             PIC X.
               10  ANNT-SINGLEPAY          PIC X.
               10  ANNT-EMPBENEFIT         PIC X.
               10  ANNT-REVA               PIC X.
               10  ANNT-RPS                PIC X.
               10  ANNT-MONEY-SUSP         PIC X.
               10  ANNT-HOLD               PIC X.
               10  ANNT-FIXEDSUBS          PIC X.
               10  ANNT-RESTRICT           PIC X.
               10  ANNT-DEATH              PIC X.
               10  ANNT-INNOVESTA          PIC X.
               10  ANNT-INNOVESTB          PIC X.
               10  ANNT-VARSUB             PIC X.
           05  CERT-COMMON.
               10  CERT-FLEXSAV            PIC X.
               10  CERT-NOLOANBAL          PIC X.
               10  CERT-MKTSTRATEGY        PIC X.
           05  FUND-COMMON.
               10  FUND-TERMINATED         PIC X.
               10  FUND-CASHMGMTC          PIC X.
               10  FUND-NOMINBAL           PIC X.
               10  FUND-FUNDSPO-OK         PIC X.
               10  FUND-NEWDIM             PIC X.
               10  FUND-CLASSB             PIC X.
           05  LIFE-COMMON.
               10  LIFE-TRAD-ACCT          PIC X.
               10  LIFE-UL-ACCT            PIC X.
               10  LIFE-TERM               PIC X.
               10  LIFE-DI                 PIC X.
               10  LIFE-LPPLUS             PIC X.
               10  LIFE-V2D                PIC X.
               10  LIFE-V2D2               PIC X.
               10  LIFE-F2D                PIC X.
               10  LIFE-VULIII             PIC X.
               10  LIFE-VUL4               PIC X.
               10  LIFE-SGLPAY             PIC X.
               10  LIFE-SGLPAYWHL          PIC X.
               10  LIFE-SHEARSON           PIC X.
               10  LIFE-NOLOAN             PIC X.
      *COUNTS ACROSS ALL BANKS.  USED FOR EDITS THAT APPLY ACROSS ALL
      *BANKS.
           05  ACCT-BA-COUNT           PIC S9(4) COMP.
           05  ACCT-BA-RCOUNT          PIC S9(4) COMP.
           05  ACCT-BA-LCOUNT          PIC S9(4) COMP.
           05  ACCT-BA-CCOUNT          PIC S9(4) COMP.
           05  ACCT-BA-TCOUNT          PIC S9(4) COMP.
           05  ACCT-BA-ACOUNT          PIC S9(4) COMP.
           05  ACCT-BA-PCOUNT          PIC S9(4) COMP.
           05  ACCT-BA-ICOUNT          PIC S9(4) COMP.
           05  ACCT-BA-FCOUNT          PIC S9(4) COMP.
           05  ACCT-99-COUNT           PIC S9(4) COMP.
           05  ACCT-99-RCOUNT          PIC S9(4) COMP.
           05  ACCT-99-LCOUNT          PIC S9(4) COMP.
           05  ACCT-99-CCOUNT          PIC S9(4) COMP.
           05  ACCT-99-TCOUNT          PIC S9(4) COMP.
           05  ACCT-99-ACOUNT          PIC S9(4) COMP.
           05  ACCT-99-PCOUNT          PIC S9(4) COMP.
           05  ACCT-99-ICOUNT          PIC S9(4) COMP.
           05  ACCT-99-FCOUNT          PIC S9(4) COMP.
           05  ACCT-SPO                PIC S9(4) COMP.
           05  ACCT-SPO-ACTPEN         PIC S9(4) COMP.
           05  ACCT-SPO-FP             PIC S9(4) COMP.
           05  ACCT-SPO-CP             PIC S9(4) COMP.
           05  ACCT-SPO-AP             PIC S9(4) COMP.
           05  ACCT-SPO-DI             PIC S9(4) COMP.
           05  ACCT-SPO-LP             PIC S9(4) COMP.
           05  ACCT-SPO-IP             PIC S9(4) COMP.
           05  ACCT-SPO-AD             PIC S9(4) COMP.
           05  ACCT-SPO-ID             PIC S9(4) COMP.
           05  ACCT-SPO-AI             PIC S9(4) COMP.
           05  ACCT-SPO-FP-FF          PIC S9(4) COMP.
           05  ACCT-SPO-DI-FF          PIC S9(4) COMP.
           05  ACCT-DCA                PIC S9(4) COMP.
           05  ACCT-DCA-MC             PIC S9(4) COMP.
           05  ACCT-GB                 PIC S9(4) COMP.
           05  ACCT-GB-ACTIVE          PIC S9(4) COMP.
      *   THESE ARE USED ONLY FOR FUND AND LIFE ACCOUNTS. OTHERS USE
      *   AMINA.
           05  ACCT-MONTHLY            PIC S9(7)V99 COMP-3.
           05  ACCT-QUARTERLY          PIC S9(7)V99 COMP-3.
           05  ACCT-SEMIANNUAL         PIC S9(7)V99 COMP-3.
           05  ACCT-ANNUAL             PIC S9(7)V99 COMP-3.
           05  ACCT-BIMONTHLY          PIC S9(7)V99 COMP-3.
           05  ACCT-SEMIMONTHLY        PIC S9(7)V99 COMP-3.
           05  ACCT-BIWEEKLY           PIC S9(7)V99 COMP-3.
           05  ACCT-WEEKLY             PIC S9(7)V99 COMP-3.
           05  ACCT-OD                 PIC S9(7)V99 COMP-3.
      *!WI
           05  LIFE-ALPLDT
                        PICTURE 9(8).                                   CI0425
      *!WI
           05  LIFE-ALDDUE
                        PICTURE 9(08).                                  CI0425
      *!WI
           05  LIFE-DGRAC
                        PICTURE 9(08).                                  CI0425
           05  ACCT-SPO-ACTIVE         PIC S9(4) COMP.
           05  ACCT-SPO-FP-A           PIC S9(4) COMP.
           05  ACCT-SPO-CP-A           PIC S9(4) COMP.
           05  ACCT-SPO-AP-A           PIC S9(4) COMP.
           05  ACCT-SPO-DI-A           PIC S9(4) COMP.
           05  ACCT-SPO-LP-A           PIC S9(4) COMP.
           05  ACCT-SPO-IP-A           PIC S9(4) COMP.
           05  ACCT-SPO-AD-A           PIC S9(4) COMP.
           05  ACCT-SPO-ID-A           PIC S9(4) COMP.
           05  ACCT-SPO-AI-A           PIC S9(4) COMP.
           05  ACCT-SPO-FP-FF-A        PIC S9(4) COMP.
           05  ACCT-SPO-DI-FF-A        PIC S9(4) COMP.
           05  ACCT-SPO-PEND           PIC S9(4) COMP.
           05  ACCT-SPO-FP-P           PIC S9(4) COMP.
           05  ACCT-SPO-CP-P           PIC S9(4) COMP.
           05  ACCT-SPO-AP-P           PIC S9(4) COMP.
           05  ACCT-SPO-DI-P           PIC S9(4) COMP.
           05  ACCT-SPO-LP-P           PIC S9(4) COMP.
           05  ACCT-SPO-IP-P           PIC S9(4) COMP.
           05  ACCT-SPO-AD-P           PIC S9(4) COMP.
           05  ACCT-SPO-ID-P           PIC S9(4) COMP.
           05  ACCT-SPO-AI-P           PIC S9(4) COMP.
           05  ACCT-SPO-FP-FF-P        PIC S9(4) COMP.
           05  ACCT-SPO-DI-FF-P        PIC S9(4) COMP.
      *SWITCHE FOR EODS SERVICE CALLING AND EODS QUEUE
           05  EODS-SERVICE-DOWN     PIC X.
           05  EODS-QUEUE-FAILED     PIC X.

      *WORKING STORAGE TABLE TO SAVE COUNTS FOR THE BAS FROM CX12.
      *ACROSS ALL BANKS.  USED FOR EDITS THAT APPLY ACROSS ALL BANKS.
       01               ACCT-TABLE.
         05               ACCT     OCCURS 97 TIMES.
           10             ACCT-COUNT          PIC S9(4) COMP.
           10             ACCT-RCOUNT         PIC S9(4) COMP.
           10             ACCT-LCOUNT         PIC S9(4) COMP.
           10             ACCT-CCOUNT         PIC S9(4) COMP.
           10             ACCT-TCOUNT         PIC S9(4) COMP.
           10             ACCT-ACOUNT         PIC S9(4) COMP.
           10             ACCT-PCOUNT         PIC S9(4) COMP.
           10             ACCT-ICOUNT         PIC S9(4) COMP.
           10             ACCT-FCOUNT         PIC S9(4) COMP.

      *  COUNTS RECURRING FREQUENCY DETAILS FOR 'THIS BANK'
      *  USED FOR EDITS THAT APPLY AT THE BANK LEVEL,
      *  AND FOR DETERMINING NEXT SEQ# (NAPDS) FOR ADD.
       01  BAPD-BA-TABLE.
           05             BAPD     OCCURS 97 TIMES.
               10  BAPD-BA-COUNT           PIC S9(4) COMP.
               10  BAPD-BA-RCOUNT          PIC S9(4) COMP.
               10  BAPD-BA-LCOUNT          PIC S9(4) COMP.
               10  BAPD-BA-CCOUNT          PIC S9(4) COMP.
               10  BAPD-BA-TCOUNT          PIC S9(4) COMP.
               10  BAPD-BA-ACOUNT          PIC S9(4) COMP.
               10  BAPD-BA-PCOUNT          PIC S9(4) COMP.
               10  BAPD-BA-ICOUNT          PIC S9(4) COMP.
               10  BAPD-BA-FCOUNT          PIC S9(4) COMP.
      *  COUNTS OD FREQUENCY DETAILS FOR 'THIS BANK'
      *  USED FOR EDITS THAT APPLY AT THE BANK LEVEL.
       01  BAOD-99-TABLE.
           05             BAOD     OCCURS 97 TIMES.
               10  BAOD-99-COUNT           PIC S9(4) COMP.
               10  BAOD-99-RCOUNT          PIC S9(4) COMP.
               10  BAOD-99-LCOUNT          PIC S9(4) COMP.
               10  BAOD-99-CCOUNT          PIC S9(4) COMP.
               10  BAOD-99-TCOUNT          PIC S9(4) COMP.
               10  BAOD-99-ACOUNT          PIC S9(4) COMP.
               10  BAOD-99-SCOUNT          PIC S9(4) COMP.
               10  BAOD-99-PCOUNT          PIC S9(4) COMP.
               10  BAOD-99-ICOUNT          PIC S9(4) COMP.
               10  BAOD-99-FCOUNT          PIC S9(4) COMP.
                                                                        AM0135
      *-----> PCB address list for calling CI0135...                    AM0135
      *                                                                 AM0135
       01                 CI0135-PCB-ADDRESS-LIST.                      AM0135
           05             CI0135-PCB-CH1P-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CCRP-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CPRP-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CBTP-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CA1P-PTR1      POINTER.            AM0135
       01                 CL01.                                         CI0425
            10            CL01-CL01K.                                   CI0425
            11            CL01-C199.                                    CI0425
            12            CL01-CLID.                                    CI0425
            13            CL01-CLIDO  PICTURE  9(3).                    CI0425
            13            CL01-CLIDN.                                   CI0425
            14            CL01-CLIDNP PICTURE  X(12).                   CI0425
            14            CL01-CLIDND PICTURE  9(8).                    CI0425
            10            CL01-GECKD  PICTURE  9.                       CI0425
            10            CL01-GEMDA  PICTURE  9(8).                    CI0425
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0425
                          BINARY.                                       CI0425
            10            CL01-GECUC  PICTURE  99.                      CI0425
            10            CL01-CLDOR  PICTURE  9(8).                    CI0425
            10            CL01-CLLNG  PICTURE  XX.                      CI0425
            10            CL01-GESLC  PICTURE  99.                      CI0425
            10            CL01-CLTYP  PICTURE  X.                       CI0425
            10            CL01-CLCLS  PICTURE  9(3).                    CI0425
            10            CL01-CLTWRC PICTURE  99.                      CI0425
            10            CL01-CLPVC  PICTURE  99.                      CI0425
            10            CL01-CLIND  PICTURE  9(3).                    CI0425
            10            CL01-CLTRC  PICTURE  99.                      CI0425
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            CL01-AYSIDA PICTURE  9(3).                    CI0425
            10            CL01-AYSID  PICTURE  9(5).                    CI0425
            10            CL01-CLSTR  PICTURE  9(2).                    CI0425
            10            CL01-CLC11  PICTURE  X.                       CI0425
            10            CL01-CLTIN  PICTURE  9(12).                   CI0425
            10            CL01-CLTND  PICTURE  9(8).                    CI0425
            10            CL01-CLTINC PICTURE  9.                       CI0425
            10            CL01-CCDWA  PICTURE  9.                       CI0425
            10            CL01-CICES  PICTURE  X.                       CI0425
            10            CL01-CLTRA  PICTURE  9(2).                    CI0425
            10            CL01-DIRSY  PICTURE  9(4)                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            CL01-CFEDS  PICTURE  X.                       CI0425
            10            CL01-FILLER PICTURE  X(06).                   CI0425
       01                 CL03.                                         CI0425
            10            CL03-GEDLA  PICTURE  9(8).                    CI0425
            10            CL03-DDREP  PICTURE  9(8).                    CI0425
            10            CL03-DPRFR  PICTURE  9(8).                    CI0425
            10            CL03-IACCI  PICTURE  X.                       CI0425
            10            CL03-CLDOB  PICTURE  9(8).                    CI0425
            10            CL03-CLDOD  PICTURE  9(8).                    CI0425
            10            CL03-CLDTH  PICTURE  X.                       CI0425
            10            CL03-CCINI  PICTURE  X.                       CI0425
            10            CL03-FILLER PICTURE  X(1).                    CI0425
            10            CL03-CLAIN  PICTURE  S9(11)                   CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            CL03-CCAOD  PICTURE  999.                     CI0425
            10            CL03-CLMAR  PICTURE  X.                       CI0425
            10            CL03-C198.                                    CI0425
            11            CL03-CLNAM.                                   CI0425
            12            CL03-CLNAMH PICTURE  X(6).                    CI0425
            12            CL03-CLNAMF PICTURE  X(20).                   CI0425
            12            CL03-CLNAMM.                                  CI0425
            13            CL03-CLNAMI PICTURE  X.                       CI0425
            13            CL03-CLNAMR PICTURE  X(14).                   CI0425
            12            CL03-CLNAML PICTURE  X(25).                   CI0425
            12            CL03-CLNAMS PICTURE  X(4).                    CI0425
            10            CL03-FILLER PICTURE  X(10).                   CI0425
            10            CL03-MPRFS  PICTURE  X(4).                    CI0425
            10            CL03-CLOCC  PICTURE  9(3).                    CI0425
            10            CL03-CLRET  PICTURE  X.                       CI0425
            10            CL03-IOCOB  PICTURE  X.                       CI0425
            10            CL03-CLSEX  PICTURE  X.                       CI0425
            10            CL03-CLWIL  PICTURE  X.                       CI0425
            10            CL03-GECFC  PICTURE  99.                      CI0425
            10            CL03-GECFY  PICTURE  9(4).                    CI0425
            10            CL03-ICUSC  PICTURE  X.                       CI0425
            10            CL03-MCTYC  PICTURE  X(20).                   CI0425
            10            CL03-CLWIP  PICTURE  X.                       CI0425
            10            CL03-CLCTXF PICTURE  99.                      CI0425
            10            CL03-CLCUS  PICTURE  99.                      CI0425
            10            CL03-NPDLU  PICTURE  9(5).                    CI0425
            10            CL03-CLEMI  PICTURE  X.                       CI0425
            10            CL03-GEPHNH PICTURE  X(14).                   CI0425
            10            CL03-GEPHNB PICTURE  X(14).                   CI0425
            10            CL03-GEPHNX PICTURE  9(4).                    CI0425
            10            CL03-GEPHNA PICTURE  X(14).                   CI0425
            10            CL03-FILLER PICTURE  X(3).                    CI0425
            10            CL03-IAPRT  PICTURE  X.                       CI0425
            10            CL03-CEMSC  PICTURE  X.                       CI0425
            10            CL03-CSEPS  PICTURE  X.                       CI0425
            10            CL03-CRACE  PICTURE  X.                       CI0425
            10            CL03-CNIRA  PICTURE  X.                       CI0425
            10            CL03-FILLER PICTURE  X(11).                   CI0425
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0003           PIC X(8) VALUE 'CI0003P '.                  AM0003
       01  CI0004           PIC X(8) VALUE 'CI0004P '.                  AM0004
       01  CI0018           PIC X(8) VALUE 'CI0018P '.                  AM0018
       01  CI0019           PIC X(8) VALUE 'CI0019P '.                  AM0019
       01  CI0020           PIC X(8) VALUE 'CI0020P '.                  AM0020
       01  CI0083           PIC X(8) VALUE 'CI0083P '.                  AM0083
       01  CI0124           PIC X(8) VALUE 'CI0124P '.                  AM0124
       01  CI0135           PIC X(8) VALUE 'CI0135P '.                  AM0135
       01  CI0140           PIC X(8) VALUE 'CI0140P '.                  AM0140
       01  CI0141           PIC X(8) VALUE 'CI0141P '.                  AM0141
       01  CI0223         PIC X(8) VALUE 'CI0223P '.                    AM0223
       01  CI0297           PIC X(08)  VALUE 'CI0297P '.                AM0297
       01  CI0320           PIC X(8) VALUE 'CI0320P '.                  AMC320
       01  CI0323           PIC X(8) VALUE 'CI0323P '.                  AMC323
       01  CI0975           PIC X(08) VALUE 'CI0975P'.                  AM0975
       01                 CT01.                                         CI0425
            10            CT01-CT01K.                                   CI0425
            11            CT01-C299.                                    CI0425
            12            CT01-CTID.                                    CI0425
            13            CT01-CTIDA  PICTURE  9(3).                    CI0425
            13            CT01-CTIDN.                                   CI0425
            14            CT01-CTIDNP PICTURE  X(13).                   CI0425
            14            CT01-CTIDND PICTURE  9(11).                   CI0425
            10            CT01-GECKD  PICTURE  9.                       CI0425
            10            CT01-GEMDA  PICTURE  9(8).                    CI0425
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0425
                          BINARY.                                       CI0425
            10            CT01-GECUC  PICTURE  99.                      CI0425
            10            CT01-CTAUL  PICTURE  9(3).                    CI0425
            10            CT01-DIRAC  PICTURE  9(4).                    CI0425
            10            CT01-CTCCI  PICTURE  X.                       CI0425
            10            CT01-CTCUS  PICTURE  999.                     CI0425
            10            CT01-CTEFD  PICTURE  9(8).                    CI0425
            10            CT01-CTIAD  PICTURE  9(8).                    CI0425
            10            CT01-CLCUS  PICTURE  99.                      CI0425
            10            CT01-CAMMB  PICTURE  X(3).                    CI0425
            10            CT01-CKPMM  PICTURE  X.                       CI0425
            10            CT01-CTLAD  PICTURE  9(8).                    CI0425
            10            CT01-IPERS  PICTURE  X.                       CI0425
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            CT01-CTLAT  PICTURE  9(8).                    CI0425
            10            CT01-CTLATC PICTURE  9(6).                    CI0425
            10            CT01-IMEGA  PICTURE  X.                       CI0425
            10            CT01-DIRAB  PICTURE  9(8).                    CI0425
            10            CT01-COLRQ  PICTURE  X.                       CI0425
            10            CT01-ZDA04  PICTURE  X(4).                    CI0425
            10            CT01-CTLPD  PICTURE  9(8).                    CI0425
            10            CT01-CIRASP PICTURE  9.                       CI0425
            10            CT01-CIRATP PICTURE  99.                      CI0425
            10            CT01-DRTHC  PICTURE  9(8).                    CI0425
            10            CT01-CPPTC  PICTURE  X.                       CI0425
            10            CT01-ZDA06  PICTURE  X(6).                    CI0425
            10            CT01-CTACD  PICTURE  9(8).                    CI0425
            10            CT01-CTNLI  PICTURE  X.                       CI0425
            10            CT01-CTRHO  PICTURE  9(8).                    CI0425
            10            CT01-CTSGD  PICTURE  9(8).                    CI0425
            10            CT01-CPATP  PICTURE  X(1).                    CI0425
            10            CT01-IRSTA  PICTURE  X.                       CI0425
            10            CT01-CTSTA  PICTURE  99.                      CI0425
            10            CT01-CTSSC  PICTURE  99.                      CI0425
            10            CT01-PRLIN  PICTURE  9(3).                    CI0425
            10            CT01-PRCOD  PICTURE  9(5).                    CI0425
            10            CT01-PRSCD  PICTURE  X(9).                    CI0425
            10            CT01-CTLNI  PICTURE  X.                       CI0425
            10            CT01-AYSIDA PICTURE  9(3).                    CI0425
            10            CT01-AYSID  PICTURE  9(5).                    CI0425
            10            CT01-CTBMC  PICTURE  99.                      CI0425
            10            CT01-CINAR  PICTURE  99.                      CI0425
            10            CT01-CPHTR  PICTURE  X.                       CI0425
            10            CT01-CDSTR  PICTURE  XX.                      CI0425
            10            CT01-CQACT  PICTURE  999.                     CI0425
            10            CT01-CIRAS  PICTURE  999.                     CI0425
            10            CT01-CIRAT  PICTURE  999.                     CI0425
            10            CT01-CLRAY  PICTURE  9(5).                    CI0425
            10            CT01-CATTP  PICTURE  X.                       CI0425
       01                 CT49.                                         CI0425
            10            CT49-CT49K.                                   CI0425
            11            CT49-GESTD  PICTURE  9(8).                    CI0425
            10            CT49-CTOWN  PICTURE  9(3).                    CI0425
            10            CT49-FILLER PICTURE  X.                       CI0425
       01                 CX01.                                         CI0425
            10            CX01-CX01K.                                   CI0425
            11            CX01-C199.                                    CI0425
            12            CX01-CLID.                                    CI0425
            13            CX01-CLIDO  PICTURE  9(3).                    CI0425
            13            CX01-CLIDN.                                   CI0425
            14            CX01-CLIDNP PICTURE  X(12).                   CI0425
            14            CX01-CLIDND PICTURE  9(8).                    CI0425
            10            CX01-GEMDA  PICTURE  9(8).                    CI0425
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0425
                          BINARY.                                       CI0425
            10            CX01-FILLER PICTURE  X(5).                    CI0425
       01                 CX03.                                         CI0425
            10            CX03-GELL   PICTURE  9(4)                     CI0425
                          BINARY.                                       CI0425
            10            CX03-CY00.                                    CI0425
            11            CX03-CX03K.                                   CI0425
            12            CX03-CARTY  PICTURE  99.                      CI0425
            12            CX03-NARRS  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX03-CARST  PICTURE  99.                      CI0425
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX03-CPMTG  PICTURE  99.                      CI0425
            11            CX03-GRCRNG PICTURE  9(3).                    CI0425
            11            CX03-DEXDT  PICTURE  9(8).                    CI0425
            11            CX03-DASUP  PICTURE  9(8).                    CI0425
            11            CX03-CSTEC  PICTURE  X(3).                    CI0425
            11            CX03-FILLER PICTURE  X(17).                   CI0425
            11            CX03-CY50.                                    CI0425
            12            CX03-NARID  PICTURE  X(30).                   CI0425
            11            CX03-CY51                                     CI0425
                          REDEFINES            CX03-CY50.               CI0425
            12            CX03-NDIDN  PICTURE  9(12).                   CI0425
            12            CX03-FILLER PICTURE  X(18).                   CI0425
            11            CX03-CY52                                     CI0425
                          REDEFINES            CX03-CY50.               CI0425
            12            CX03-NAIDC  PICTURE  9(12).                   CI0425
            12            CX03-FILLER PICTURE  X(18).                   CI0425
            11            CX03-CY53                                     CI0425
                          REDEFINES            CX03-CY50.               CI0425
            12            CX03-NAMEXB PICTURE  9(15).                   CI0425
            12            CX03-FILLER PICTURE  X(15).                   CI0425
            10            CX03-CY99.                                    CI0425
            11            CX03-FILLER PICTURE  X(109).                  CI0425
            10            CX03-CY01                                     CI0425
                          REDEFINES            CX03-CY99.               CI0425
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX03-ICPCI  PICTURE  X.                       CI0425
            11            CX03-CLUPD  PICTURE  9(3).                    CI0425
            11            CX03-DLAUP  PICTURE  9(8).                    CI0425
            11            CX03-CWRC   PICTURE  99.                      CI0425
            11            CX03-CHCR   PICTURE  99.                      CI0425
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0425
            11            CX03-GEAUN  PICTURE  9(5).                    CI0425
            11            CX03-DPCHD  PICTURE  9(8).                    CI0425
            11            CX03-DLRCHK PICTURE  9(8).                    CI0425
            11            CX03-QTRCHK PICTURE  9(2).                    CI0425
            11            CX03-DNPMT  PICTURE  9(8).                    CI0425
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            CX03-CY02                                     CI0425
                          REDEFINES            CX03-CY99.               CI0425
            11            CX03-QSIRQ  PICTURE  99.                      CI0425
            11            CX03-QDRMN  PICTURE  9(2)                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX03-DDPRE  PICTURE  9(8).                    CI0425
            11            CX03-DDSHP  PICTURE  9(8).                    CI0425
            11            CX03-NDRFTB PICTURE  9(5).                    CI0425
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0425
            11            CX03-DDSHPA PICTURE  9(8).                    CI0425
            11            CX03-NDRFTF PICTURE  9(5).                    CI0425
            11            CX03-QDIPBK PICTURE  9(3).                    CI0425
            11            CX03-CREOR  PICTURE  X(1).                    CI0425
            11            CX03-CREOR1 PICTURE  X(1).                    CI0425
            11            CX03-DDASC  PICTURE  9(8).                    CI0425
            11            CX03-FILLER PICTURE  X(7).                    CI0425
            10            CX03-CY03                                     CI0425
                          REDEFINES            CX03-CY99.               CI0425
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0425
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0425
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0425
            11            CX03-DOPDA  PICTURE  99.                      CI0425
            11            CX03-CPMTF  PICTURE  99.                      CI0425
            11            CX03-CIRMO  PICTURE  X(12).                   CI0425
            11            CX03-CPALL  PICTURE  X(1).                    CI0425
            11            CX03-CCOLM  PICTURE  9(2).                    CI0425
            11            CX03-CBLTP  PICTURE  X(1).                    CI0425
            11            CX03-CASUB  PICTURE  9(2).                    CI0425
            11            CX03-CBLFM  PICTURE  9(2).                    CI0425
            11            CX03-IBILS  PICTURE  X.                       CI0425
            11            CX03-IPAOS  PICTURE  X.                       CI0425
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0425
            11            CX03-DLBPD  PICTURE  9(8).                    CI0425
            11            CX03-DNBPD  PICTURE  9(8).                    CI0425
            11            CX03-DODBD  PICTURE  9(8).                    CI0425
            11            CX03-CPSRE  PICTURE  99.                      CI0425
            11            CX03-ISPHN  PICTURE  X.                       CI0425
            11            CX03-TCARR  PICTURE  X(6).                    CI0425
            11            CX03-CBKPT  PICTURE  9(2).                    CI0425
            11            CX03-IECNT  PICTURE  X.                       CI0425
            11            CX03-ICONV  PICTURE  X(1).                    CI0425
            11            CX03-FILLER PICTURE  X(4).                    CI0425
            10            CX03-CY04                                     CI0425
                          REDEFINES            CX03-CY99.               CI0425
            11            CX03-CCARD  PICTURE  X(02).                   CI0425
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0425
            11            CX03-IREMT  PICTURE  X(01).                   CI0425
            11            CX03-ISBILA PICTURE  X.                       CI0425
            11            CX03-DLBPDA PICTURE  9(8).                    CI0425
            11            CX03-DNBPDA.                                  CI0425
            12            CX03-DNCYM  PICTURE  9(6).                    CI0425
            12            CX03-CEDTD  PICTURE  9(2).                    CI0425
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX03-DREMT  PICTURE  9(8).                    CI0425
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0425
            11            CX03-CWRC2  PICTURE  99.                      CI0425
            11            CX03-CHCR2  PICTURE  99.                      CI0425
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0425
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0425
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0425
       01                 CX06.                                         CI0425
            10            CX06-CX06K.                                   CI0425
            11            CX06-C299.                                    CI0425
            12            CX06-CTID.                                    CI0425
            13            CX06-CTIDA  PICTURE  9(3).                    CI0425
            13            CX06-CTIDN.                                   CI0425
            14            CX06-CTIDNP PICTURE  X(13).                   CI0425
            14            CX06-CTIDND PICTURE  9(11).                   CI0425
            10            CX06-NPECK  PICTURE  9(02).                   CI0425
            10            CX06-FILLER PICTURE  X.                       CI0425
       01                 CX12.                                         CI0425
            10            CX12-CX12K.                                   CI0425
            11            CX12-CPMTC  PICTURE  99.                      CI0425
            11            CX12-NAPDS  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX12-GESTD  PICTURE  9(8).                    CI0425
            10            CX12-GEEND  PICTURE  9(8).                    CI0425
            10            CX12-CIRMO  PICTURE  X(12).                   CI0425
            10            CX12-CDEST  PICTURE  99.                      CI0425
            10            CX12-APMTL  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            CX12-DNPMT  PICTURE  9(8).                    CI0425
            10            CX12-NIRACM PICTURE  9(2).                    CI0425
            10            CX12-CPMTF  PICTURE  99.                      CI0425
            10            CX12-IPODM  PICTURE  X.                       CI0425
            10            CX12-CLUPD  PICTURE  9(3).                    CI0425
            10            CX12-DLAUP  PICTURE  9(8).                    CI0425
            10            CX12-CWRC   PICTURE  99.                      CI0425
            10            CX12-CHCR   PICTURE  99.                      CI0425
            10            CX12-GEOPD2 PICTURE  X(8).                    CI0425
            10            CX12-GEAUN  PICTURE  9(5).                    CI0425
            10            CX12-DPCHD  PICTURE  9(8).                    CI0425
            10            CX12-DNEXE  PICTURE  9(8).                    CI0425
            10            CX12-CCSMQ  PICTURE  X.                       CI0425
            10            CX12-GCUSPZ PICTURE  X(12).                   CI0425
            10            CX12-CORTY  PICTURE  X.                       CI0425
            10            CX12-CNAVR  PICTURE  X(1).                    CI0425
            10            CX12-DELOI3 PICTURE  9(6).                    CI0425
            10            CX12-ALOIDD PICTURE  9(9)V99                  CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            CX12-FILLER PICTURE  X(5).                    CI0425
       01                 CX13.                                         CI0425
            10            CX13-GELL   PICTURE  9(4)                     CI0425
                          BINARY.                                       CI0425
            10            CX13-CY20.                                    CI0425
            11            CX13-CX13K.                                   CI0425
            12            CX13-CARTZ  PICTURE  99.                      CI0425
            12            CX13-NAPDS  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX13-GESTD  PICTURE  9(8).                    CI0425
            11            CX13-GEEND  PICTURE  9(8).                    CI0425
            11            CX13-DASUQ  PICTURE  9(8).                    CI0425
            11            CX13-CDEST  PICTURE  99.                      CI0425
            11            CX13-IIARR  PICTURE  X.                       CI0425
            11            CX13-DLAUP  PICTURE  9(8).                    CI0425
            11            CX13-GEOPD2 PICTURE  X(8).                    CI0425
            11            CX13-GEAUN  PICTURE  9(5).                    CI0425
            11            CX13-DPCHD  PICTURE  9(8).                    CI0425
            11            CX13-PPOT1  PICTURE  S9(3)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX13-ACOT1  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX13-QPST1  PICTURE  S9(7)V999                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX13-FILLER PICTURE  X(03).                   CI0425
            10            CX13-CY96.                                    CI0425
            11            CX13-FILLER PICTURE  X(50).                   CI0425
            10            CX13-CY21                                     CI0425
                          REDEFINES            CX13-CY96.               CI0425
            11            CX13-DNPMT  PICTURE  9(8).                    CI0425
            11            CX13-CPMTF  PICTURE  99.                      CI0425
            11            CX13-ADBRQ  PICTURE  S9(11)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX13-QSHOWQ PICTURE  S9(9)V999                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX13-PACT1  PICTURE  S999V999                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX13-DOPDA  PICTURE  99.                      CI0425
            11            CX13-DNEXE  PICTURE  9(8).                    CI0425
            11            CX13-CIRMO  PICTURE  X(12).                   CI0425
            10            CX13-CY98.                                    CI0425
            11            CX13-FILLER PICTURE  X(120).                  CI0425
            10            CX13-CY25                                     CI0425
                          REDEFINES            CX13-CY98.               CI0425
            11            CX13-COPTC  PICTURE  9(1).                    CI0425
            11            CX13-ILPOI  PICTURE  X(1).                    CI0425
            11            CX13-CATOC  PICTURE  X(1).                    CI0425
            11            CX13-CEOIA  PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX13-ACOAR  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX13-CEOTR  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX13-DSTMO  PICTURE  99.                      CI0425
            10            CX13-CY27                                     CI0425
                          REDEFINES            CX13-CY98.               CI0425
            11            CX13-QMTH1  PICTURE  9(3).                    CI0425
            11            CX13-IDRMD  PICTURE  X.                       CI0425
            10            CX13-CY28                                     CI0425
                          REDEFINES            CX13-CY98.               CI0425
            11            CX13-AALLBL PICTURE  S9(8)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX13-PSURR  PICTURE  S9(3)V999                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX13-DFPMT  PICTURE  9(8).                    CI0425
            11            CX13-QMTHLA PICTURE  9(3).                    CI0425
            11            CX13-PWHLDS PICTURE  S999V9(5)                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX13-ISWHO  PICTURE  X(1).                    CI0425
            10            CX13-CY29                                     CI0425
                          REDEFINES            CX13-CY98.               CI0425
            11            CX13-IINDI1 PICTURE  X(1).                    CI0425
            11            CX13-IINDI2 PICTURE  X(1).                    CI0425
            11            CX13-IINDI3 PICTURE  X(1).                    CI0425
            11            CX13-PWHLD5 PICTURE  S999V99                  CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX13-CCSMQ  PICTURE  X.                       CI0425
            11            CX13-CPLEC  PICTURE  XX.                      CI0425
            11            CX13-IPTRDA PICTURE  X(01).                   CI0425
            11            CX13-GCUSPY PICTURE  X(12).                   CI0425
            11            CX13-ALOIDA PICTURE  S9(11)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX13-DELOI  PICTURE  9(8).                    CI0425
            11            CX13-CLGND  PICTURE  X.                       CI0425
            11            CX13-CORTYA PICTURE  X(3).                    CI0425
            11            CX13-CPH3U  PICTURE  X.                       CI0425
            11            CX13-CNAVR  PICTURE  X(1).                    CI0425
            11            CX13-NEXEC  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
       01                 CX14.                                         CI0425
            10            CX14-GELL   PICTURE  9(4)                     CI0425
                          BINARY.                                       CI0425
            10            CX14-CX14K.                                   CI0425
            11            CX14-NPISQ  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            CX14-ACOTD  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            CX14-PPOTD  PICTURE  S9(3)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            CX14-QPSTD  PICTURE  S9(7)V999                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            CX14-CPITC  PICTURE  99.                      CI0425
            10            CX14-FILLER PICTURE  X(04).                   CI0425
            10            CX14-CY97.                                    CI0425
            11            CX14-FILLER PICTURE  X(32).                   CI0425
            10            CX14-CY30                                     CI0425
                          REDEFINES            CX14-CY97.               CI0425
            11            CX14-IOWNC  PICTURE  X.                       CI0425
            11            CX14-CTYPE  PICTURE  X.                       CI0425
            11            CX14-C299.                                    CI0425
            12            CX14-CTID.                                    CI0425
            13            CX14-CTIDA  PICTURE  9(3).                    CI0425
            13            CX14-CTIDN.                                   CI0425
            14            CX14-CTIDNP PICTURE  X(13).                   CI0425
            14            CX14-CTIDND PICTURE  9(11).                   CI0425
            11            CX14-CPMTC  PICTURE  99.                      CI0425
            11            CX14-IACSD  PICTURE  X.                       CI0425
            10            CX14-CY31                                     CI0425
                          REDEFINES            CX14-CY97.               CI0425
            11            CX14-FILLER PICTURE  X(2).                    CI0425
            11            CX14-IDELI  PICTURE  X.                       CI0425
            11            CX14-CDEL1  PICTURE  9(3).                    CI0425
            11            CX14-NDELS  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            CX14-CY32                                     CI0425
                          REDEFINES            CX14-CY97.               CI0425
            11            CX14-GCUSPZ PICTURE  X(12).                   CI0425
       01                 CX18.                                         CI0425
            10            CX18-CX18K.                                   CI0425
            11            CX18-NBASQ  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            CX18-NPBN   PICTURE  X(20).                   CI0425
            10            CX18-CCBAT  PICTURE  99.                      CI0425
            10            CX18-DACHP  PICTURE  9(8).                    CI0425
            10            CX18-CSTPRE PICTURE  99.                      CI0425
            10            CX18-C199.                                    CI0425
            11            CX18-CLID.                                    CI0425
            12            CX18-CLIDO  PICTURE  9(3).                    CI0425
            12            CX18-CLIDN.                                   CI0425
            13            CX18-CLIDNP PICTURE  X(12).                   CI0425
            13            CX18-CLIDND PICTURE  9(8).                    CI0425
            10            CX18-MCSIG  PICTURE  X(30).                   CI0425
            10            CX18-CPBNU  PICTURE  X.                       CI0425
            10            CX18-CSPCR  PICTURE  99.                      CI0425
            10            CX18-DAPCR  PICTURE  9(8).                    CI0425
            10            CX18-FILLER PICTURE  XX.                      CI0425
       01                 CX2Y.                                         CI0425
            10            CX2Y-CX2YK.                                   CI0425
            11            CX2Y-C299.                                    CI0425
            12            CX2Y-CTID.                                    CI0425
            13            CX2Y-CTIDA  PICTURE  9(3).                    CI0425
            13            CX2Y-CTIDN.                                   CI0425
            14            CX2Y-CTIDNP PICTURE  X(13).                   CI0425
            14            CX2Y-CTIDND PICTURE  9(11).                   CI0425
            11            CX2Y-C199.                                    CI0425
            12            CX2Y-CLID.                                    CI0425
            13            CX2Y-CLIDO  PICTURE  9(3).                    CI0425
            13            CX2Y-CLIDN.                                   CI0425
            14            CX2Y-CLIDNP PICTURE  X(12).                   CI0425
            14            CX2Y-CLIDND PICTURE  9(8).                    CI0425
            11            CX2Y-CARTY  PICTURE  99.                      CI0425
            11            CX2Y-NARRS  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
       01                 CX6Y.                                         CI0425
            10            CX6Y-CX6YK.                                   CI0425
            11            CX6Y-C299.                                    CI0425
            12            CX6Y-CTID.                                    CI0425
            13            CX6Y-CTIDA  PICTURE  9(3).                    CI0425
            13            CX6Y-CTIDN.                                   CI0425
            14            CX6Y-CTIDNP PICTURE  X(13).                   CI0425
            14            CX6Y-CTIDND PICTURE  9(11).                   CI0425
            11            CX6Y-C199.                                    CI0425
            12            CX6Y-CLID.                                    CI0425
            13            CX6Y-CLIDO  PICTURE  9(3).                    CI0425
            13            CX6Y-CLIDN.                                   CI0425
            14            CX6Y-CLIDNP PICTURE  X(12).                   CI0425
            14            CX6Y-CLIDND PICTURE  9(8).                    CI0425
            11            CX6Y-CARTY  PICTURE  99.                      CI0425
            11            CX6Y-NARRS  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX6Y-CTID1  PICTURE  X(27).                   CI0425
            11            CX6Y-CARTZ  PICTURE  99.                      CI0425
            11            CX6Y-NAPDS  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            CX6Y-NPISQ  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
      ******************************************************************AM0975
      *  LINKAGE SEGMENTS FOR CI0975                                    AM0975
      ******************************************************************AM0975
      *                                                                 AM0975
      *!WF DSP=DA DSL=QT SEL=8G FOR=I DES=2 LEV=1                       AM0975
       01                 DA8G.                                         CI0425
            10            DA8G-CFAUL1 PICTURE  X(4)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            DA8G-TFACT1 PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            DA8G-CFAUL2 PICTURE  X(4)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            DA8G-TFACT2 PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            DA8G-CFAUL3 PICTURE  X(4)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            DA8G-TFACT3 PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            DA8G-CARTY  PICTURE  99                       CI0425
                          VALUE                ZERO.                    CI0425
            10            DA8G-CSYS   PICTURE  X(4)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            DA8G-GERTC  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            DA8G-CERRE  PICTURE  X(4)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            DA8G-QITEM  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            DA8G-FILLER PICTURE  X(32400)                 CI0425
                          VALUE                SPACE.                   CI0425
            10            DA8G-QT8M                                     CI0425
                          REDEFINES            DA8G-FILLER.             CI0425
            11            DA8G-QT8I                                     CI0425
                          OCCURS       090     TIMES.                   CI0425
            12            DA8G-CTID   PICTURE  X(27).                   CI0425
            12            DA8G-CARST  PICTURE  99.                      CI0425
            12            DA8G-CPMTF  PICTURE  99.                      CI0425
            12            DA8G-CPALL  PICTURE  X(1).                    CI0425
            12            DA8G-PPOTD  PICTURE  S9(3)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            DA8G-ACOTD  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            DA8G-GESTD  PICTURE  9(8).                    CI0425
            12            DA8G-GEEND  PICTURE  9(8).                    CI0425
            12            DA8G-GEMDA  PICTURE  9(8).                    CI0425
            12            DA8G-DNPMT1 PICTURE  9(8).                    CI0425
            12            DA8G-MVSYS  PICTURE  X(6).                    CI0425
            12            DA8G-NAIDC  PICTURE  9(12).                   CI0425
            12            DA8G-CIRMO  PICTURE  X(12).                   CI0425
            12            DA8G-CEBTP  PICTURE  9(2).                    CI0425
            12            DA8G-CLTIN  PICTURE  9(12).                   CI0425
            12            DA8G-CLORN  PICTURE  X(45).                   CI0425
            12            DA8G-GESAD1 PICTURE  X(30).                   CI0425
            12            DA8G-GESAD2 PICTURE  X(30).                   CI0425
            12            DA8G-GESAD3 PICTURE  X(30).                   CI0425
            12            DA8G-GECIT  PICTURE  X(25).                   CI0425
            12            DA8G-GEST   PICTURE  X(8).                    CI0425
            12            DA8G-GEPCD  PICTURE  X(12).                   CI0425
            12            DA8G-CLNMF  PICTURE  X(20).                   CI0425
            12            DA8G-CLNML  PICTURE  X(25).                   CI0425
            12            DA8G-CLTIN1 PICTURE  9(12).                   CI0425
            12            DA8G-FILLER PICTURE  X(05).                   CI0425
            10            DA8G-FILLER PICTURE  X(100)                   CI0425
                          VALUE                SPACE.                   CI0425
       01                 DA8H.                                         CI0425
            10            DA8H-QITEM  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            DA8H-CTID   PICTURE  X(27)                    CI0425
                          OCCURS       030     TIMES                    CI0425
                          VALUE                SPACE.                   CI0425
            10            DA8H-CRQAS  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            DA8H-FILLER PICTURE  X(299)                   CI0425
                          VALUE                SPACE.                   CI0425
      *!WF DSP=DA DSL=QT SEL=8H FOR=I DES=2 LEV=1                       AM0975
      ******************************************************************
      **  DATE WORK AREA USED WITH MACRO AADA52                        *
      ******************************************************************
      *!WF DSP=DD DSL=DD SEL=01 FOR=I LEV=1 PLT=DD
       01                 DD00.                                         CI0425
          05              DD00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00093).                  CI0425
       01                 DD01  REDEFINES      DD00.                    CI0425
            10            DD01-XDAT8.                                   CI0425
            11            DD01-XDATC  PICTURE  XX.                      CI0425
            11            DD01-XDATY  PICTURE  XX.                      CI0425
            11            DD01-XDATM  PICTURE  XX.                      CI0425
            11            DD01-XDATD  PICTURE  XX.                      CI0425
            10            DD01-XDAT8D                                   CI0425
                          REDEFINES            DD01-XDAT8               CI0425
               PICTURE    9(8).                                         CI0425
            10            DD01-XDAT81.                                  CI0425
            11            DD01-XDATM1 PICTURE  XX.                      CI0425
            11            DD01-XDATD1 PICTURE  XX.                      CI0425
            11            DD01-XDATC1 PICTURE  XX.                      CI0425
            11            DD01-XDATY1 PICTURE  XX.                      CI0425
            10            DD01-XDAT80                                   CI0425
                          REDEFINES            DD01-XDAT81              CI0425
               PICTURE    9(8).                                         CI0425
            10            DD01-XDAT62.                                  CI0425
            11            DD01-XDATM2 PICTURE  XX.                      CI0425
            11            DD01-XDATD2 PICTURE  XX.                      CI0425
            11            DD01-XDATY2 PICTURE  XX.                      CI0425
            10            DD01-XDAT69                                   CI0425
                          REDEFINES            DD01-XDAT62              CI0425
               PICTURE    9(6).                                         CI0425
            10            DD01-XDATCU.                                  CI0425
            11            DD01-XDATC9 PICTURE  99.                      CI0425
            11            DD01-XDAYMD.                                  CI0425
            12            DD01-XDATY9 PICTURE  99.                      CI0425
            12            DD01-XDAMD.                                   CI0425
            13            DD01-XDATM9 PICTURE  99.                      CI0425
            13            DD01-XDATD9 PICTURE  99.                      CI0425
            10            DD01-XDAT89 PICTURE  9(8).                    CI0425
            10            DD01-XDAJC  PICTURE  9(7).                    CI0425
            10            DD01-XDAJC1.                                  CI0425
            11            DD01-XDAJC9 PICTURE  99.                      CI0425
            11            DD01-XDAJY  PICTURE  99.                      CI0425
            11            DD01-XDAJN  PICTURE  999.                     CI0425
            10            DD01-XDAB   PICTURE  9(5).                    CI0425
            10            DD01-DD05.                                    CI0425
            11            DD01-XDACT  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            DD01-XDACV  PICTURE  S9                       CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            DD01-XDAGP  PICTURE  S9(9)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            DD01-XDAJP  PICTURE  S9(7)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            DD01-XDACV1 PICTURE  S9                       CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            DD01-XDAGP1 PICTURE  S9(9)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            DD01-XDAJP1 PICTURE  S9(7)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            DD01-XW03.                                    CI0425
            11            DD01-XDATG.                                   CI0425
            12            DD01-XDAT1.                                   CI0425
            13            DD01-XDAT19 PICTURE  99.                      CI0425
            12            DD01-XDAT2.                                   CI0425
            13            DD01-XDAT29 PICTURE  99.                      CI0425
            12            DD01-XDAT3.                                   CI0425
            13            DD01-XDAT39 PICTURE  99.                      CI0425
            12            DD01-XDAT4.                                   CI0425
            13            DD01-XDAT49 PICTURE  99.                      CI0425
            11            DD01-XLEAPY PICTURE  99.                      CI0425
            11            DD01-DTGCY  PICTURE  9(4).                    CI0425
            11            DD01-FILLER                                   CI0425
                          REDEFINES            DD01-DTGCY.              CI0425
            12            DD01-DTGCC  PICTURE  9(2).                    CI0425
            12            DD01-DTGYY  PICTURE  9(2).                    CI0425

      ** DATE WORK AREA
       01  DEL-ER                 PIC 9(1).

      ******************************************************            AADA82
      ****      WORK AREAS FOR COMMON DATE UTILITY       ***            AADA82
      ******************************************************            AADA82
      **                                                                AADA82
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA82
      **                                                                AADA82
      **   SEGMENT DD30 - FUNCTION LAYOUT                               AADA82
      **                                                                AADA82
      *!WF DSP=DF DSL=DD SEL=30 FOR=I DES=2 LEV=1                       AADA82
       01                 DF30.                                         CI0425
            10            DF30-CDTFN  PICTURE  9(4)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            DF30-CDTSF  PICTURE  9(4)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            DF30-CDTSC  PICTURE  9(4)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            DF30-FILLER PICTURE  X(40)                    CI0425
                          VALUE                SPACE.                   CI0425
       01                 DF34.                                         CI0425
            10            DF34-CAINS  PICTURE  X(03)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            DF34-CDTUC  PICTURE  9                        CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-NDTUN  PICTURE  S9(05)                   CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-FILLER PICTURE  X(162)                   CI0425
                          VALUE                SPACE.                   CI0425
            10            DF34-DTGRG.                                   CI0425
            11            DF34-DTGCY.                                   CI0425
            12            DF34-DTGCC  PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            12            DF34-DTGYY  PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            DF34-DTGMM  PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            DF34-DTGDD  PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-DTJUL.                                   CI0425
            11            DF34-DTJCY.                                   CI0425
            12            DF34-DTJCC  PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            12            DF34-DTJYY  PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            DF34-DTJDD  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTFM  PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTLM  PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTFF  PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTLF  PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTFW  PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTLW  PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CCDOWA PICTURE  9                        CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CCDRW  PICTURE  9                        CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-FILLER PICTURE  X(58)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            DF34-DTGRGA.                                  CI0425
            11            DF34-DTGCYA.                                  CI0425
            12            DF34-DTGCCA PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            12            DF34-DTGYYA PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            DF34-DTGMMA PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            DF34-DTGDDA PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-DTJULA.                                  CI0425
            11            DF34-DTJCYA.                                  CI0425
            12            DF34-DTJCCA PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            12            DF34-DTJYYA PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            DF34-DTJDDA PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTFMA PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTLMA PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTFFA PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTLFA PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTFWA PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTLWA PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CCDOWB PICTURE  9                        CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CCDRWA PICTURE  9                        CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-FILLER PICTURE  X(58)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            DF34-DTGRGB.                                  CI0425
            11            DF34-DTGCYB.                                  CI0425
            12            DF34-DTGCCB PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            12            DF34-DTGYYB PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            DF34-DTGMMB PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            DF34-DTGDDB PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-DTJULB.                                  CI0425
            11            DF34-DTJCYB.                                  CI0425
            12            DF34-DTJCCB PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            12            DF34-DTJYYB PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            DF34-DTJDDB PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTFMB PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTLMB PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTFFB PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTLFB PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTFWB PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CDTLWB PICTURE  9(01)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CCDOWC PICTURE  9                        CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-CCDRWB PICTURE  9                        CI0425
                          VALUE                ZERO.                    CI0425
            10            DF34-FILLER PICTURE  X(58)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            DF34-FILLER PICTURE  X(40)                    CI0425
                          VALUE                SPACE.                   CI0425
      **                                                                AADA82
      **   SEGMENT DD34 - CONVERT DATE LAYOUT                           AADA82
      **                                                                AADA82
      *!WF DSP=DF DSL=DD SEL=34 FOR=I DES=2 LEV=1                       AADA82
      **                                                                AADA82
      ******************************************************************ACMCTI
      *WORKING STORAGE SEGMENT FOR STORING THE LINKAGE DATA FOR CI0361. ACMCTI
      ******************************************************************ACMCTI
      *!WF DSP=I9 DSL=K9 SEL=3B FOR=I DES=2 LEV=1                       ACMCTI
       01                 I93B.                                         CI0425
            10            I93B-CEADC  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            I93B-DACTT  PICTURE  X(10)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            I93B-GEOPDC PICTURE  X(8)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            I93B-GEOPDB PICTURE  X(8)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            I93B-CAEMCE PICTURE  X(8)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            I93B-CAEMCD PICTURE  X(8)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            I93B-GETIMM PICTURE  X(8)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            I93B-CRTNC  PICTURE  S9(9)                    CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            I93B-GERTC  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            I93B-DXTMST PICTURE  X(26)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            I93B-DXTMS2 PICTURE  X(26)                    CI0425
                          VALUE                SPACE.                   CI0425
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
       01                 XW05.                                         CI0425
            10            XW05-XW06.                                    CI0425
            11            XW05-XDBPCB.                                  CI0425
            12            XW05-XDBDNM PICTURE  X(08)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            XW05-XSEGLV PICTURE  X(02)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            XW05-XRC    PICTURE  X(02)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            XW05-XPROPT PICTURE  X(04)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            XW05-FILLER PICTURE  S9(5)                    CI0425
                          VALUE                ZERO                     CI0425
                          BINARY.                                       CI0425
            12            XW05-XSEGNM PICTURE  X(08)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0425
                          VALUE                ZERO                     CI0425
                          BINARY.                                       CI0425
            12            XW05-XSEGNB PICTURE  9(05)                    CI0425
                          VALUE                ZERO                     CI0425
                          BINARY.                                       CI0425
            12            XW05-XCOKEY PICTURE  X(70)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            XW05-XW07.                                    CI0425
            11            XW05-XIOPCB.                                  CI0425
            12            XW05-XTERMI PICTURE  X(08)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            XW05-FILLER PICTURE  XX                       CI0425
                          VALUE                SPACE.                   CI0425
            12            XW05-XRC1   PICTURE  X(02)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            XW05-FILLER PICTURE  X(12)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            XW05-XMODNM PICTURE  X(8)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            XW05-XGU    PICTURE  X(4)                     CI0425
                          VALUE                'GU  '.                  CI0425
            10            XW05-XGHU   PICTURE  X(4)                     CI0425
                          VALUE                'GHU '.                  CI0425
            10            XW05-XGN    PICTURE  X(4)                     CI0425
                          VALUE                'GN  '.                  CI0425
            10            XW05-XGHN   PICTURE  X(4)                     CI0425
                          VALUE                'GHN '.                  CI0425
            10            XW05-XGNP   PICTURE  X(4)                     CI0425
                          VALUE                'GNP '.                  CI0425
            10            XW05-XGHNP  PICTURE  X(4)                     CI0425
                          VALUE                'GHNP'.                  CI0425
            10            XW05-XREPL  PICTURE  XXXX                     CI0425
                          VALUE                'REPL'.                  CI0425
            10            XW05-XISRT  PICTURE  X(4)                     CI0425
                          VALUE                'ISRT'.                  CI0425
            10            XW05-XDLET  PICTURE  X(4)                     CI0425
                          VALUE                'DLET'.                  CI0425
            10            XW05-XOPEN  PICTURE  X(4)                     CI0425
                          VALUE                'OPEN'.                  CI0425
            10            XW05-XCLSE  PICTURE  X(4)                     CI0425
                          VALUE                'CLSE'.                  CI0425
            10            XW05-XCHKP  PICTURE  X(4)                     CI0425
                          VALUE                'CHKP'.                  CI0425
            10            XW05-XXRST  PICTURE  X(4)                     CI0425
                          VALUE                'XRST'.                  CI0425
            10            XW05-XTERM  PICTURE  X(4)                     CI0425
                          VALUE                'TERM'.                  CI0425
            10            XW05-XNFPAC PICTURE  X(13)                    CI0425
                          VALUE                SPACE.                   CI0425
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0425
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0425
      *                                                                 ADU155
      ******************************************************************ADU155
      ** WORK AREA NEEDED FOR MACRO ADU155                             *ADU155
      **        DATE COMMON AREA FOR EXECUTING CICS ASKTIME/FORMATTIME *ADU155
      ******************************************************************ADU155
      *                                                                 ADU155
      *!WI pl=DT100                                                     ADU155
       01  DT01-XMSTS                                                   ADU155
                        PICTURE S9(15)                                  CI0425
                          COMPUTATIONAL-3.                              CI0425
       01  DT01-F2CCYY             PIC S9(08) COMP.                     ADU155
      *!WI pl=DT200                                                     ADU155
       01  DT01-XDAT69                                                  ADU155
                        PICTURE 9(6).                                   CI0425
       01  DT01-UDATE.                                                  ADU155
           05  DT01-YEAR           PIC  9(04).                          ADU155
           05  DT01-MMDD           PIC  9(04).                          ADU155
      *!WI pl=DT280                                                     ADU155
       01  DT01-XDATCU REDEFINES DT01-UDATE                             ADU155
                        PICTURE X(8).                                   CI0425
      *                                                                 ADU155
      *GROUP SEGMENT HOLD AREAS FOR RETIREMENT PLAN INFO
      *!WF DSP=GR DSL=GR SEL=0107 FOR=I DES=2 LEV=1
      * PLT=GR
       01                 GR01.                                         CI0425
            10            GR01-GR01K.                                   CI0425
            11            GR01-GR98.                                    CI0425
            12            GR01-GRID.                                    CI0425
            13            GR01-GRIDC  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            13            GR01-GRIDN.                                   CI0425
            14            GR01-GRIDNP PICTURE  99                       CI0425
                          VALUE                ZERO.                    CI0425
            14            GR01-GRIDND PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-GECKD  PICTURE  9                        CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-GEMDA  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-NSEQ4B PICTURE  9(8)                     CI0425
                          VALUE                ZERO                     CI0425
                          BINARY.                                       CI0425
            10            GR01-GRDOR  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-GRIAD  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-GECUC  PICTURE  99                       CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-GRLNG  PICTURE  99                       CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-GESLC  PICTURE  99                       CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-AYSIDA PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-AYSID  PICTURE  9(5)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-GRCSD  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-GRCFD  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-GRNCL  PICTURE  S9(5)                    CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            GR01-GRNCT  PICTURE  S9(5)                    CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            GR01-GRSFC  PICTURE  99                       CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-GRCRN  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-GRCSS  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            GR01-MKSRC  PICTURE  99                       CI0425
                          OCCURS       010     TIMES                    CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-NEFPS  PICTURE  X(5)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            GR01-DEFPS  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-DLSRV  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR01-CTLNI  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            GR01-CGRLI  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            GR01-CAMGR  PICTURE  9(5)                     CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            GR01-CAMGS  PICTURE  9(5)                     CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            GR01-CAMGN  PICTURE  9(3)                     CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            GR01-CGRMF  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            GR01-FILLER PICTURE  X(08)                    CI0425
                          VALUE                SPACE.                   CI0425
       01                 GR07.                                         CI0425
            10            GR07-GEDLA  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-GRAID  PICTURE  X(12)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            GR07-GRPAP  PICTURE  X(14)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            GR07-GEPHNX PICTURE  9(4)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-DPLEF  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-DPLAM  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-NCPFN  PICTURE  9(6)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-GEFYE  PICTURE  9(4)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-FILLER PICTURE  X(06)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            GR07-GRPAN  PICTURE  X(45)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            GR07-CGRPA  PICTURE  99                       CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-IPRTT7 PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            GR07-GRPED  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-FILLER PICTURE  X(05)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            GR07-GRPLC  PICTURE  99                       CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-GRPLT  PICTURE  99                       CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-FILLER PICTURE  X(04)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            GR07-GEADI  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            GR07-GRCFA  PICTURE  S9(11)V99                CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            GR07-GECFY  PICTURE  9(4)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-GECFC  PICTURE  99                       CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-MEMPL  PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            GR07-CAUNIT PICTURE  X(4)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            GR07-FILLER PICTURE  X(21)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            GR07-GRPPP  PICTURE  999                      CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-CCORT  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-CIDRP  PICTURE  99                       CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-CCDWA  PICTURE  9                        CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-IERSA  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            GR07-DERSA  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            GR07-FILLER PICTURE  X(04)                    CI0425
                          VALUE                SPACE.                   CI0425

       01  7-82-WORK-DATE.                                              AAOLBB
           05  7-82-WORK-CC            PIC 9(02).                       AAOLBB
           05  7-82-WORK-YY            PIC 9(02).                       AAOLBB
           05  7-82-WORK-MM            PIC 9(02).                       AAOLBB
           05  7-82-WORK-DD            PIC 9(02).                       AAOLBB
       01  7-82-ANN-DATE.                                               AAOLBB
           05  7-82-ANN-CC            PIC 9(02).                        AAOLBB
           05  7-82-ANN-YY            PIC 9(02).                        AAOLBB
           05  7-82-ANN-MM            PIC 9(02).                        AAOLBB
           05  7-82-ANN-DD            PIC 9(02).                        AAOLBB
       01  7-82-RESULT            PIC 9(02).                            AAOLBB
       01  7-82-REM               PIC 9(02).                            AAOLBB
       01  7-82-PREMIUM           PIC S9(7)V99   VALUE ZEROES.          AAOLBB
       01  7-82-PREMIUM-T         PIC S9(7)V99   VALUE ZEROES.          AAOLBB
       01  7-82-PREMIUM-O         PIC S9(7)V99   VALUE ZEROES.          AAOLBB
       01  7-82-PREMIUMS          PIC 9(7)V99    VALUE ZEROES.          AAOLBB
       01  7-82-CALC  REDEFINES 7-82-PREMIUMS.                          AAOLBB
           05  FILLER             PIC 999999.                           AAOLBB
           05  7-82-ONE           PIC 9V99.                             AAOLBB
       01  7-82-TENS              PIC 9(7)V99.                          AAOLBB
       01  7-82-ONES.                                                   AAOLBB
           05  7-82-CONES         PIC 9V9.                              AAOLBB
           05  FILLER             PIC 9.                                AAOLBB
       01  7-82-FL-DI             PIC X      VALUE 'N'.                 AAOLBB
       01  7-82-ALCIDN.                                                 AAOLBB
           05  7-82-FIRST-4       PIC 9(4).                             AAOLBB
           05  7-82-REST          PIC 9(7).                             AAOLBB
       01  7-82-PRSCD.                                                  AAOLBB
           05  7-82-DE-CODE       PIC 9(2).                             AAOLBB
           05  7-82-FGH-CODE      PIC 9(3).                             AAOLBB
           05  7-82-REST          PIC 9(4).                             AAOLBB
       01  7-82-TL-2012           PIC X      VALUE 'N'.                 AAOLBB
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
      *                                                                 AM0020
      ******************************************************************AM0020
      **     SEGMENT THAT CONTAINS THE CAMS ACCOUNTING DATES           *AM0020
      ******************************************************************AM0020
      *                                                                 AM0020
      *!WF DSP=NS DSL=NS SEL=20 FOR=I LEV=1                             AM0020
       01                 NS00.                                         CI0425
          05              NS00-00.                                      CI0425
            10            NS00-NS00K.                                   CI0425
            11            NS00-PRCSTK PICTURE  XX.                      CI0425
          05              NS00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00078).                  CI0425
       01                 NS20  REDEFINES      NS00.                    CI0425
            10       FILLER         PICTURE  X(00002).                  CI0425
            10            NS20-DCACG  PICTURE  9(8).                    CI0425
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            NS20-CCDAT  PICTURE  X(8).                    CI0425
            10            NS20-DCALP  PICTURE  X(12).                   CI0425
            10            NS20-DNACG  PICTURE  9(8).                    CI0425
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            NS20-CNDAT  PICTURE  X(8).                    CI0425
            10            NS20-DNALP  PICTURE  X(12).                   CI0425
            10            NS20-DCACD  PICTURE  X(10).                   CI0425
            10            NS20-FILLER PICTURE  X(4).                    CI0425
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
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
                                                                        AM0019
      ******************************************************************AM0019
      **     PCB ADDRESS LIST FOR CI0019.  MODULE CI0019 WILL NEED     *AM0019
      **     PCB'S FOR:                                                *AM0019
      **                CONTRACT DATABASE(CT1P)                        *AM0019
      **                GROUP DATABASE(GR1P)                           *AM0019
      ******************************************************************AM0019
                                                                        AM0019
       01  CI0019-PCB-ADDRESS-LIST.                                     AM0019
           05  CI0019-PCB-CT1P-PTR1      POINTER.                       AM0019
           05  CI0019-PCB-GR1P-PTR1      POINTER.                       AM0019
      ******************************************************************AM0083
      ** USE THE 88 FIELDS TO BREAK DOWN ARRAY FROM CI0083             *AM0083
      ******************************************************************AM0083
      **                                                                AM0083
      **                                                                AM0083
      *!WF DSP=PF DSL=K1 SEL=1F FOR=I LEV=1                             AM0083
       01                 PF00.                                         CI0425
          05              PF00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00266).                  CI0425
       01                 PF1F  REDEFINES      PF00.                    CI0425
            10            PF1F-INPUT.                                   CI0425
            11            PF1F-MAPPN  PICTURE  X(10).                   CI0425
            11            PF1F-PROGR  PICTURE  X(06).                   CI0425
            11            PF1F-ADDRLN.                                  CI0425
            12            PF1F-GESAD1 PICTURE  X(30).                   CI0425
            12            PF1F-GESAD2 PICTURE  X(30).                   CI0425
            12            PF1F-GESAD3 PICTURE  X(30).                   CI0425
            11            PF1F-FILLER PICTURE  X(100).                  CI0425
            10            PF1F-OUTPUT.                                  CI0425
            11            PF1F-IOWNC  PICTURE  X                        CI0425
                          OCCURS       060     TIMES.                   CI0425
      **                                                                AM0083
      *!WI pl=PF070                                                     AM0083
       01                 W-PF00-MAPPN                                  AM0083
                        PICTURE X(10).                                  CI0425
       01                 W-PF00-DECIPHER.                              AM0083
      *!WI pl=PF090                                                     AM0083
         05               W-PF01-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  CHARITABLE-REMAINDER      VALUE 'Y'.                     AM0083
      *!WI pl=PF110                                                     AM0083
         05               W-PF02-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  CONDITIONAL-MINOR         VALUE 'Y'.                     AM0083
      *!WI pl=PF130                                                     AM0083
         05               W-PF03-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  CORPORATION               VALUE 'Y'.                     AM0083
      *!WI pl=PF150                                                     AM0083
         05               W-PF04-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  CORPORATE-TRUSTEE         VALUE 'Y'.                     AM0083
      *!WI pl=PF170                                                     AM0083
         05               W-PF05-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  CUSTODIAL-TSCA            VALUE 'Y'.                     AM0083
      *!WI pl=PF190                                                     AM0083
         05               W-PF06-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  CUSTODIAL-KEOGH           VALUE 'Y'.                     AM0083
      *!WI pl=PF210                                                     AM0083
         05               W-PF07-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  DEFERRED-COMP-CORP        VALUE 'Y'.                     AM0083
      *!WI pl=PF230                                                     AM0083
         05               W-PF08-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  DEFERRED-COMP-GOVT        VALUE 'Y'.                     AM0083
      *!WI pl=PF250                                                     AM0083
         05               W-PF09-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  EXECUTOR                  VALUE 'Y'.                     AM0083
      *!WI pl=PF270                                                     AM0083
         05               W-PF10-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  GUARDIAN                  VALUE 'Y'.                     AM0083
      *!WI pl=PF290                                                     AM0083
         05               W-PF11-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  IRA-NON-AEFA              VALUE 'Y'.                     AM0083
      *!WI pl=PF310                                                     AM0083
         05               W-PF12-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  LIFE-TENANT               VALUE 'Y'.                     AM0083
      *!WI pl=PF330                                                     AM0083
         05               W-PF13-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  PARTNERSHIP               VALUE 'Y'.                     AM0083
      *!WI pl=PF350                                                     AM0083
         05               W-PF14-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  SOLE-PROPRIETOR           VALUE 'Y'.                     AM0083
      *!WI pl=PF370                                                     AM0083
         05               W-PF15-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  TENANTS-IN-COMMON         VALUE 'Y'.                     AM0083
      *!WI pl=PF390                                                     AM0083
         05               W-PF16-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  TRUST                     VALUE 'Y'.                     AM0083
      *!WI pl=PF410                                                     AM0083
         05               W-PF17-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  TRUSTEED-QUALIFIED-PLAN   VALUE 'Y'.                     AM0083
      *!WI pl=PF430                                                     AM0083
         05               W-PF18-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  USUFRUCTORY               VALUE 'Y'.                     AM0083
      *!WI pl=PF450                                                     AM0083
         05               W-PF19-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  A-401K                    VALUE 'Y'.                     AM0083
      *!WI pl=PF470                                                     AM0083
         05               W-PF20-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  TRANSFER-ON-DEATH         VALUE 'Y'.                     AM0083
      *!WI pl=PF490                                                     AM0083
         05               W-PF21-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  TENANTS-BY-ENTIRETY       VALUE 'Y'.                     AM0083
      *!WI pl=PF510                                                     AM0083
         05               W-PF22-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  CHURH                     VALUE 'Y'.                     AM0083
      *!WI pl=PF530                                                     AM0083
         05               W-PF23-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  COMMUNITY-PROPERTY        VALUE 'Y'.                     AM0083
      *!WI pl=PF550                                                     AM0083
         05               W-PF24-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  IRREVOCABLE               VALUE 'Y'.                     AM0083
      *!WI pl=PF570                                                     AM0083
         05               W-PF25-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  LIVING                    VALUE 'Y'.                     AM0083
      *!WI pl=PF590                                                     AM0083
         05               W-PF26-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  LOVING                    VALUE 'Y'.                     AM0083
      *!WI pl=PF610                                                     AM0083
         05               W-PF27-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  MARITAL                   VALUE 'Y'.                     AM0083
      *!WI pl=PF630                                                     AM0083
         05               W-PF28-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  REVOCABLE                 VALUE 'Y'.                     AM0083
      *!WI pl=PF650                                                     AM0083
         05               W-PF29-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  TESTAMENTARY              VALUE 'Y'.                     AM0083
      *!WI pl=PF670                                                     AM0083
         05               W-PF30-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  SSI-REPRESENTATIVE        VALUE 'Y'.                     AM0083
      *!WI pl=PF690                                                     AM0083
         05               W-PF31-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  LIMITED-LIABILITY         VALUE 'Y'.                     AM0083
      *!WI pl=PF710                                                     AM0083
         05               W-PF32-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  SEPARATE-PROPERTY         VALUE 'Y'.                     AM0083
      *!WI pl=PF730                                                     AM0083
         05               W-PF33-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  POA                       VALUE 'Y'.                     AM0083
      *!WI pl=PF750                                                     AM0083
         05               W-PF34-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  UTMA                      VALUE 'Y'.                     AM0083
      *!WI pl=PF770                                                     AM0083
         05               W-PF35-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  UGMA                      VALUE 'Y'.                     AM0083
      *!WI pl=PF790                                                     AM0083
         05               W-PF36-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  CURATOR                   VALUE 'Y'.                     AM0083
      *!WI pl=PF810                                                     AM0083
         05               W-PF37-IOWNC                                  AM0083
                        PICTURE X.                                      CI0425
           88  TUTOR                     VALUE 'Y'.                     AM0083
         05               FILLER                    PIC X(23).          AM0083
      *-----> PCB Address list for calling CI0124                       AM0124
      *       Access required to Contract Database (CT1P)               AM0124
      *       (Routine will access CT01 and CT13 records)               AM0124
      *                                                                 AM0124
                                                                        AM0124
       01                 CI0124-PCB-ADDRESS-LIST.                      AM0124
           05             CI0124-PCB-CT1P-PTR1      POINTER.            AM0124

      *PASS AREA TO/FROM CI0124 (GET ACCT HOLD CODES)
      *!WF DSP=PG DSL=PJ SEL=01 FOR=I DES=1 LEV=1 PLT=PG
       01                 PG01.                                         CI0425
            10            PG01-CTID   PICTURE  X(27).                   CI0425
            10            PG01-DASOF  PICTURE  9(8).                    CI0425
            10            PG01-CT13                                     CI0425
                          OCCURS       003     TIMES.                   CI0425
            11            PG01-CT13K.                                   CI0425
            12            PG01-GEHSD  PICTURE  9(8).                    CI0425
            12            PG01-GEHCD  PICTURE  9(3).                    CI0425
            12            PG01-GEHCSE PICTURE  X(12).                   CI0425
            12            PG01-GEHCSU PICTURE  9(5).                    CI0425
            11            PG01-GEHRD  PICTURE  9(8).                    CI0425
            11            PG01-GEHV   PICTURE  S9(7)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            PG01-GEDC   PICTURE  9(2).                    CI0425
            10            PG01-GEHCDD PICTURE  X(40)                    CI0425
                          OCCURS       003     TIMES.                   CI0425
            10            PG01-THCDM  PICTURE  X(20)                    CI0425
                          OCCURS       003     TIMES.                   CI0425

      ******************************************************************AM0140
      **** PCB ADDRESS LIST FOR CI0140                                 *AM0140
      ******************************************************************AM0140
       01                 CI0140-PH-PCB-ADDR-LIST.                      AM0140
           05             CI0140-PH-PCB-LM1P-PTR1      POINTER.         AM0140
           05             CI0140-PH-PCB-LUVP-PTR1      POINTER.         AM0140
           05             CI0140-PH-PCB-LH1P-PTR1      POINTER.         AM0140
           05             CI0140-PH-PCB-LARP-PTR1      POINTER.         AM0140
           05             CI0140-PH-PCB-LPDP-PTR1      POINTER.         AM0140
           05             CI0140-PH-PCB-ARAY-PTR1      POINTER.         AM0140
           05             CI0140-PH-PCB-AR1P-PTR1      POINTER.         AM0140

      *PASS AREA TO/FROM CI0135 (CERT ACCT INFO)
      *!WF DSP=PJ DSL=PJ SEL=02 FOR=I DES=2 LEV=1 PLT=PJ
       01                 PJ02.                                         CI0425
            10            PJ02-CTID   PICTURE  X(27)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            PJ02-DCACG  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-ACCTV8 PICTURE  S9(9)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-AIDOL1 PICTURE  S9(9)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-AUINT1 PICTURE  S9(9)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-CXCSV  PICTURE  S9(7)V9(2)               CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-PCIRB5 PICTURE  S9(3)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-PANYDD PICTURE  S9(3)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-PCIRA5 PICTURE  S9(3)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-PANYDF PICTURE  9(3)V99                  CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-PCIRCB PICTURE  S9(3)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-PANYDG PICTURE  S9(3)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-PPART  PICTURE  9(3)V99                  CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-PMRTN  PICTURE  9(3)V99                  CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-PMRTEB PICTURE  S9(3)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-PBRITD PICTURE  S9(3)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-CEIAPI PICTURE  S9(7)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-CEIRND PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-CEIT   PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-DMATUR PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-AMTUR  PICTURE  S9(7)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-CELBDT PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-DTRME  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-NBSEI  PICTURE  999V99                   CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-NBSEIC PICTURE  S9(3)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-TRPTH  PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            PJ02-CELBL  PICTURE  S9(7)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-ALINT  PICTURE  S9(7)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-PELIRB PICTURE  S9(3)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-ASANP  PICTURE  S9(7)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-AAPAA  PICTURE  S9(7)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-IQLIF  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            PJ02-QMTHAA PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-QMTHCC PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-QYEARA PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-DANNIA PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-PBONS  PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-AARQDA PICTURE  S9(5)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-AACFA  PICTURE  S9(7)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-AIEPAA PICTURE  S9(7)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-CVSUR  PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            PJ02-CPRDA1 PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-DFYR   PICTURE  9(4)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-DFYRB  PICTURE  9(4)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-DVALU  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-DNIPM  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-CIPFM  PICTURE  S9(3)                    CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-CESLD  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-CEHCD  PICTURE  9(3)                     CI0425
                          OCCURS       006     TIMES                    CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-CETYPC PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-CEOTP  PICTURE  9(1)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-CEIIS  PICTURE  S9(7)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-DTRME1 PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-CEFOIM PICTURE  S9(7)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-CEIPDA PICTURE  S9(3)                    CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-GECTR  PICTURE  99                       CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-GMKTS.                                   CI0425
            11            PJ02-DTRME2 PICTURE  9(8)                     CI0425
                          OCCURS       005     TIMES                    CI0425
                          VALUE                ZERO.                    CI0425
            11            PJ02-DTRME3 PICTURE  9(8)                     CI0425
                          OCCURS       005     TIMES                    CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-PRCOD  PICTURE  9(5)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-CEFOTR PICTURE  S9(3)                    CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PJ02-DGPED  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-DIPED  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            PJ02-FILLER PICTURE  X(27)                    CI0425
                          VALUE                SPACE.                   CI0425

       01  7-RA-DNPMT-AREA.                                             $DNPMT
           05  7-RA-DATE-TEMP.                                          $DNPMT
               10 7-RA-YR            PIC 9(4).                          $DNPMT
               10 7-RA-YR-X REDEFINES 7-RA-YR.                          $DNPMT
                  15 7-RA-YR-X-CC    PIC 9(2).                          $DNPMT
                  15 7-RA-YR-X-YY    PIC 9(2).                          $DNPMT
               10 7-RA-MO            PIC 99.                            $DNPMT
               10 7-RA-DA            PIC 99.                            $DNPMT
           05  7-RA-NUMB-OF-MOS      PIC 99.                            $DNPMT
           05  7-RA-NUMB-OF-DAYS     PIC 99.                            $DNPMT
           05  7-RA-LOOP-END         PIC X VALUE 'Y'.                   $DNPMT
           05  7-RA-WT12-CIRMO-IN            PIC X(12).                 $DNPMT
           05  7-RA-CIRMO-YR REDEFINES 7-RA-WT12-CIRMO-IN.              $DNPMT
               10  7-RA-CIRMO        PIC X                              $DNPMT
                             OCCURS 12.                                 $DNPMT
           05  7-RA-MONTH-LOAD       PIC X(24).                         $DNPMT
           05  7-RA-MONTH-TABLE REDEFINES 7-RA-MONTH-LOAD.              $DNPMT
               10 7-RA-MONTH  OCCURS 12 PIC 99.                         $DNPMT
           05 7-RA-T0                PIC 99V9(9).                       $DNPMT
           05 7-RA-T1 REDEFINES 7-RA-T0.                                $DNPMT
              10 7-RA-T1-INT       PIC 99.                              $DNPMT
              10 7-RA-T1-DEC       PIC V9(9).                           $DNPMT
           05 7-RA-ERR-FLAG        PIC X VALUE 'Y'.                     $DNPMT
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
                        PICTURE X(66)                                   CI0425
                           VALUE 'DB2 RESOURCE IN USE. TRY AGAIN '.     ADB221
       01               7-MAXM-RETRY     PIC S9(3) COMP-3               ADB221
                                                   VALUE +001.          ADB221
      ******************************************************************ADUTAB
      **              TABLE TA5A ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA5A-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=5A FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA5A.                                                CI0425
           04    G-TA5A-PARAM.                                          CI0425
             10  G-TA5A-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0425
                        VALUE      +772.                                CI0425
             10  G-TA5A-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0425
                        VALUE      +001.                                CI0425
             10  G-TA5A-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0425
                        VALUE      +017.                                CI0425
             10  G-TA5A-NUAPP  PICTURE 99                               CI0425
                        VALUE       0.                                  CI0425
             10  G-TA5A-NUTAB  PICTURE X(6)                             CI0425
                        VALUE 'TA005A'.                                 CI0425
             10  G-TA5A-TABFO  PICTURE XX                 VALUE SPACE.  CI0425
             10  G-TA5A-TABCR  PICTURE XX                 VALUE SPACE.  CI0425
             10  G-TA5A-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0425
             10  G-TA5A-NUSSC  PICTURE X  VALUE   ' '.                  CI0425
             10  G-TA5A-NUSSY  PICTURE X                  VALUE SPACE.  CI0425
             10  G-TA5A-TRANID PICTURE X(4)               VALUE SPACE.  CI0425
             10  G-TA5A-FILSYS.                                         CI0425
             15  G-TA5A-USERC  PICTURE X(6)               VALUE SPACE.  CI0425
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0425
           04             TA5A.                                         CI0425
            10            TA5A-GAPSC.                                   CI0425
            11            TA5A-CTIDA  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            TA5A-PRCOD  PICTURE  9(5)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            TA5A-PRSCD  PICTURE  X(9)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-PRCLN  PICTURE  X(60)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-PRCMN  PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-PRCSN  PICTURE  X(9)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-MRCLN1 PICTURE  X(51)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-MRCLN2 PICTURE  X(51)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-MRCLN3 PICTURE  X(51)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-MRCLN4 PICTURE  X(51)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-MRCMN2 PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-MRCMN3 PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-PRCCS1 PICTURE  X(15)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-PRCCS2 PICTURE  X(15)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-PRCCS3 PICTURE  X(15)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-MPCLN  PICTURE  X(45)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-MPCL1  PICTURE  X(45)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-MSP1   PICTURE  X(60)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-MSP5   PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-MSP03  PICTURE  X(3)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-MPRDG  PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-CPRDG  PICTURE  9(2)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            TA5A-MPRDA1 PICTURE  X(50)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-CPRDA1 PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            TA5A-MSP06  PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-CPOIN  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-CPITY  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            TA5A-CLITY  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            TA5A-IVARP  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TA5A-CASCL  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            TA5A-ZDA88  PICTURE  X(88)                    CI0425
                          VALUE                SPACE.                   CI0425
      **                                                                ADUTAB
      ******************************************************************ADUTAB
      **              TABLE TB5B ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TB5B-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TB DSL=TA SEL=5B FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TB5B.                                                CI0425
           04    G-TB5B-PARAM.                                          CI0425
             10  G-TB5B-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0425
                        VALUE      +154.                                CI0425
             10  G-TB5B-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0425
                        VALUE      +001.                                CI0425
             10  G-TB5B-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0425
                        VALUE      +017.                                CI0425
             10  G-TB5B-NUAPP  PICTURE 99                               CI0425
                        VALUE       0.                                  CI0425
             10  G-TB5B-NUTAB  PICTURE X(6)                             CI0425
                        VALUE 'TA005B'.                                 CI0425
             10  G-TB5B-TABFO  PICTURE XX                 VALUE SPACE.  CI0425
             10  G-TB5B-TABCR  PICTURE XX                 VALUE SPACE.  CI0425
             10  G-TB5B-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0425
             10  G-TB5B-NUSSC  PICTURE X  VALUE   ' '.                  CI0425
             10  G-TB5B-NUSSY  PICTURE X                  VALUE SPACE.  CI0425
             10  G-TB5B-TRANID PICTURE X(4)               VALUE SPACE.  CI0425
             10  G-TB5B-FILSYS.                                         CI0425
             15  G-TB5B-USERC  PICTURE X(6)               VALUE SPACE.  CI0425
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0425
           04             TB5B.                                         CI0425
            10            TB5B-GAPSC.                                   CI0425
            11            TB5B-CTIDA  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            TB5B-PRCOD  PICTURE  9(5)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            TB5B-PRSCD  PICTURE  X(9)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-PRCODX PICTURE  9(5)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            TB5B-PRCSUB PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-PRCAUT PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-PRCBAS PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-PRCSTK PICTURE  XX                       CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-PRCPRE PICTURE  X(4)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-IBDUP  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-IUSPR  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-CVSYS  PICTURE  X(2)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-IDTOD  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-GRSFC  PICTURE  99                       CI0425
                          VALUE                ZERO.                    CI0425
            10            TB5B-ZDA18  PICTURE  X(18)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-CMPCTB PICTURE  X(4)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-ITERM  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-AMFAC  PICTURE  S9(7)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            TB5B-ZDA20  PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-CPRBK  PICTURE  X(3)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-CFXDM  PICTURE  99                       CI0425
                          VALUE                ZERO.                    CI0425
            10            TB5B-NGLCS  PICTURE  X(5)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-NDFCS  PICTURE  X(5)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-ZDA20  PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-CTNLI  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-CBANK  PICTURE  X(03)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-ISYPO  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-ISYPP  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-ICOPT  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-IANPY  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-IDSAR  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-ICIPT  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-IANDS  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-IKPMA  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-INMWT  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-IVANT  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-ISDAV  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-IUDAV  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TB5B-ZDA15  PICTURE  X(15)                    CI0425
                          VALUE                SPACE.                   CI0425
      **                                                                ADUTAB
      ******************************************************************ADUTAB
      **              TABLE TC8A ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TC8A-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TC DSL=TA SEL=8A FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TC8A.                                                CI0425
           04    G-TC8A-PARAM.                                          CI0425
             10  G-TC8A-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0425
                        VALUE      +106.                                CI0425
             10  G-TC8A-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0425
                        VALUE      +001.                                CI0425
             10  G-TC8A-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0425
                        VALUE      +008.                                CI0425
             10  G-TC8A-NUAPP  PICTURE 99                               CI0425
                        VALUE       0.                                  CI0425
             10  G-TC8A-NUTAB  PICTURE X(6)                             CI0425
                        VALUE 'TA008A'.                                 CI0425
             10  G-TC8A-TABFO  PICTURE XX                 VALUE SPACE.  CI0425
             10  G-TC8A-TABCR  PICTURE XX                 VALUE SPACE.  CI0425
             10  G-TC8A-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0425
             10  G-TC8A-NUSSC  PICTURE X  VALUE   ' '.                  CI0425
             10  G-TC8A-NUSSY  PICTURE X                  VALUE SPACE.  CI0425
             10  G-TC8A-TRANID PICTURE X(4)               VALUE SPACE.  CI0425
             10  G-TC8A-FILSYS.                                         CI0425
             15  G-TC8A-USERC  PICTURE X(6)               VALUE SPACE.  CI0425
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0425
           04             TC8A.                                         CI0425
            10            TC8A-GADPR.                                   CI0425
            11            TC8A-CTIDA  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            TC8A-PRCOD  PICTURE  9(5)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            TC8A-CLIAN  PICTURE  9(02)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            TC8A-CLAST  PICTURE  9(03)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            TC8A-ISMTD  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TC8A-ISUBA  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TC8A-IVINS  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TC8A-IEOIR  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TC8A-IDBNL  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TC8A-ICHRC  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            TC8A-ICHPN  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TC8A-IVAPR  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TC8A-ICLSF  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TC8A-IIULA  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TC8A-IINPS  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TC8A-IINLN  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TC8A-CINPS  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TC8A-CINLN  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TC8A-ZDA79  PICTURE  X(79)                    CI0425
                          VALUE                SPACE.                   CI0425
      **                                                                ADUTAB
      ******************************************************************ADUTAB
      **              TABLE TD98 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TD98-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TD DSL=TA SEL=98 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TD98.                                                CI0425
           04    G-TD98-PARAM.                                          CI0425
             10  G-TD98-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0425
                        VALUE      +053.                                CI0425
             10  G-TD98-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0425
                        VALUE      +001.                                CI0425
             10  G-TD98-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0425
                        VALUE      +010.                                CI0425
             10  G-TD98-NUAPP  PICTURE 99                               CI0425
                        VALUE       0.                                  CI0425
             10  G-TD98-NUTAB  PICTURE X(6)                             CI0425
                        VALUE 'GCPRAR'.                                 CI0425
             10  G-TD98-TABFO  PICTURE XX                 VALUE SPACE.  CI0425
             10  G-TD98-TABCR  PICTURE XX                 VALUE SPACE.  CI0425
             10  G-TD98-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0425
             10  G-TD98-NUSSC  PICTURE X  VALUE   ' '.                  CI0425
             10  G-TD98-NUSSY  PICTURE X                  VALUE SPACE.  CI0425
             10  G-TD98-TRANID PICTURE X(4)               VALUE SPACE.  CI0425
             10  G-TD98-FILSYS.                                         CI0425
             15  G-TD98-USERC  PICTURE X(6)               VALUE SPACE.  CI0425
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0425
           04             TD98.                                         CI0425
            10            TD98-GCPRAR.                                  CI0425
            11            TD98-CTIDA  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            TD98-PRCOD  PICTURE  9(5)                     CI0425
                          VALUE                ZERO.                    CI0425
            11            TD98-CARTY  PICTURE  99                       CI0425
                          VALUE                ZERO.                    CI0425
            10            TD98-IARTYA PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IARLNA PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQAN  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQSA  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQFM  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQQT  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQBM  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQMO  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQBF  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQSM  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQBW  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQWK  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQIR  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQIF  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQIS  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQIK  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQIW  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQOD  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IARPSA PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IARRGA PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IFQET  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IMLNA  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-IMPRA  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            TD98-ZDA20  PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
      **                                                                ADUTAB
      **************************************************************
      ** WORKING STORAGE TO STORE PRODUCT CODE                     *
      **************************************************************
      *
      *!WI
       01  7-PRCOD
                        PICTURE 9(5).                                   CI0425
      ****************************************************
      *SAVE AREA FOR GMB INDICATOR AND STAND ALONE PN    *
      ****************************************************
      *
       01  ANNT-RAVAPLUS-GMAB  PIC X   VALUE 'N'.
      *-----------------------------------------------------------------AMLIFE
      *   SAVE AREA THAT SEPARATES THE ACCOUNT ID AND IDENTIFIES WHAT   AMLIFE
      *   TYPE OF LIFE PRODUCT IS PROCESSING                            AMLIFE
      *-----------------------------------------------------------------AMLIFE
      *                                                                 AMLIFE
       01  W-WORK-CTID.                                                 AMLIFE
      *!WI pl=WA020                                                     AMLIFE
           05  W-WORK-CTIDA                                             AMLIFE
                        PICTURE 9(3).                                   CI0425
           05  W-WORK-CTIDN.                                            AMLIFE
      *!WI pl=WA040                                                     AMLIFE
               10  W-WORK-CTIDNP                                        AMLIFE
                        PICTURE X(13).                                  CI0425
               10  W-WORK-CTIDND.                                       AMLIFE
                   15  W-WORK-PREFIX    PIC 9(4).                       AMLIFE
                       88 LIFE-DI-LTC   VALUE 9000, 9100, 9700, 9800.   AMLIFE
                       88 LIFE-INS      VALUE 9000, 9700.               AMLIFE
                       88 DI-LTC        VALUE 9100, 9800.               AMLIFE
                       88 ANNUITY       VALUE 9200, 9300, 9310, 9400,   AMLIFE
                                        9410, 9900, 9910.               AMLIFE
                       88 NEW-YORK      VALUE 9700, 9800, 9900, 9910.   AMLIFE
                       88 UL            VALUE 9090, 9790.               AMLIFE
                   15  W-WORK-BASE      PIC 9(7).                       AMLIFE
      *                                                                 AMLIFE
      *                                                                 AMLIFE
      ******************************************************************AM0140
      **** LIFE DETAIL INFO        PASS AREA (LINKAGE) *****************AM0140
      ******************************************************************AM0140
      *                                                                 AM0140
      *!WF DSP=WE DSL=K9 SEL=40 FOR=I DES=1 LEV=1                       AM0140
       01                 WE40.                                         CI0425
            10            WE40-C299.                                    CI0425
            11            WE40-CTID.                                    CI0425
            12            WE40-CTIDA  PICTURE  9(3).                    CI0425
            12            WE40-CTIDN.                                   CI0425
            13            WE40-CTIDNP PICTURE  X(13).                   CI0425
            13            WE40-CTIDND PICTURE  9(11).                   CI0425
            10            WE40-PRCOD  PICTURE  9(5).                    CI0425
            10            WE40-IANPY  PICTURE  X.                       CI0425
            10            WE40-DEFFT  PICTURE  9(8).                    CI0425
            10            WE40-ALPLDT PICTURE  9(8).                    CI0425
            10            WE40-TPLNL  PICTURE  X(30).                   CI0425
            10            WE40-MPLNA  PICTURE  X(19).                   CI0425
            10            WE40-ITMEC  PICTURE  X(1).                    CI0425
            10            WE40-ALPMOD PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ALMODE PICTURE  99.                      CI0425
            10            WE40-MPMTF  PICTURE  X(14).                   CI0425
            10            WE40-ALAPIT PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-AGSP   PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ALDBEN PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-CDBENE PICTURE  X.                       CI0425
            10            WE40-IDEBE  PICTURE  X.                       CI0425
            10            WE40-ALRISK PICTURE  S9(9)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-CFRQZ  PICTURE  X.                       CI0425
            10            WE40-ASBENA PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ASBENB PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ASBENC PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ASBENE PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ASBENF PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-AACTV  PICTURE  S9(11)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ANGOF  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ACCTVC PICTURE  X(20).                   CI0425
            10            WE40-ALLNB  PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-CBRIT  PICTURE  SV9(5)                   CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ASANP  PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ALSURR PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ASURR  PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ASURRN PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ASURRW PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-PSURR  PICTURE  S9(3)V999                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-PCIRB5 PICTURE  S9(3)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ALVLFA PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ALVLVA PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ISUBA  PICTURE  X.                       CI0425
            10            WE40-ACVALB PICTURE  S9(11)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ITAXI  PICTURE  X.                       CI0425
            10            WE40-ATLTB  PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-AEARN0 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ITAXD  PICTURE  X(1).                    CI0425
            10            WE40-ATFPI  PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-AEARN1 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-AFETY  PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-CEYAW  PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-APTXR  PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ITAXN  PICTURE  X.                       CI0425
            10            WE40-IUTAX  PICTURE  X.                       CI0425
            10            WE40-CETRL  PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-CSSVL  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ALTOT  PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ALFGH  PICTURE  999.                     CI0425
            10            WE40-DSTT1  PICTURE  9(8).                    CI0425
            10            WE40-DNPMT  PICTURE  9(8).                    CI0425
            10            WE40-CTLPD  PICTURE  9(8).                    CI0425
            10            WE40-CTLPD2 PICTURE  S9(8)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ALFXPO PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ALINPU PICTURE  S9(8)V999                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-PFPAY  PICTURE  S9(3)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-PVPAY  PICTURE  S9(3)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ATWHDD PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ATWHDE PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ACVIU  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-MPMTT  PICTURE  X(20).                   CI0425
            10            WE40-GESTNS PICTURE  X(2).                    CI0425
            10            WE40-CTWHPB PICTURE  9(3)V999.                CI0425
            10            WE40-CTWHCB PICTURE  X.                       CI0425
            10            WE40-ACTCH  PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-CACTL1 PICTURE  X(04).                   CI0425
            10            WE40-ALPLNI PICTURE  9.                       CI0425
            10            WE40-ATSA8  PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-QEDAY  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ATIPA  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-AGENN  PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-AGENO  PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-APOCY  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-APOTD  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-CSUSI  PICTURE  99.                      CI0425
            10            WE40-ALINNO PICTURE  99.                      CI0425
            10            WE40-ALPLNJ PICTURE  9.                       CI0425
            10            WE40-COLPL  PICTURE  9(05).                   CI0425
            10            WE40-ALPAGR PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ALDDUE PICTURE  9(08).                   CI0425
            10            WE40-ALAPST PICTURE  99.                      CI0425
            10            WE40-CPCAL  PICTURE  9.                       CI0425
            10            WE40-CRTBK  PICTURE  99.                      CI0425
            10            WE40-ACGPO  PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-AMMP   PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-AAMFY  PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-TBENPA PICTURE  X(30).                   CI0425
            10            WE40-TDE30  PICTURE  X(30).                   CI0425
            10            WE40-CROOR  PICTURE  99.                      CI0425
            10            WE40-AMXLN  PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-ILNST  PICTURE  X.                       CI0425
            10            WE40-AENTI  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-APRYT  PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WE40-FILLER PICTURE  X(409).                  CI0425
      ******************************************************************AM0141
      **** LIFE DETAIL INFO        PASS AREA (LINKAGE) *****************AM0141
      ******************************************************************AM0141
      *                                                                 AM0141
      *!WF DSP=WF DSL=K9 SEL=41 FOR=I DES=1 LEV=1                       AM0141
       01                 WF41.                                         CI0425
            10            WF41-FILLER PICTURE  X(2000).                 CI0425
            10            WF41-K94R                                     CI0425
                          REDEFINES            WF41-FILLER.             CI0425
            11            WF41-CTID   PICTURE  X(27).                   CI0425
            11            WF41-PRCOD  PICTURE  9(5).                    CI0425
            11            WF41-CVSTC  PICTURE  X(4).                    CI0425
            11            WF41-DVALU  PICTURE  9(8).                    CI0425
            11            WF41-AACTV  PICTURE  S9(11)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-PE65.                                    CI0425
            12            WF41-CVAPC  PICTURE  X(6).                    CI0425
            12            WF41-CVALB  PICTURE  X(3).                    CI0425
            12            WF41-CASTA  PICTURE  X.                       CI0425
            12            WF41-CTWST1 PICTURE  X(3).                    CI0425
            12            WF41-CPISC  PICTURE  X(3).                    CI0425
            12            WF41-ALPLDT PICTURE  9(8).                    CI0425
            12            WF41-DEFFT  PICTURE  9(8).                    CI0425
            12            WF41-DANNI  PICTURE  9(8).                    CI0425
            12            WF41-DTPMT  PICTURE  9(8).                    CI0425
            12            WF41-ITAMR  PICTURE  X(1).                    CI0425
            12            WF41-AGAPA  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-ATRPA  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-ATROP  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-ITMECC PICTURE  X(1).                    CI0425
            12            WF41-CBPLCA PICTURE  X.                       CI0425
            12            WF41-CPRCC  PICTURE  X.                       CI0425
            12            WF41-CLSEX  PICTURE  X.                       CI0425
            12            WF41-QPOIA  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-IREIA  PICTURE  X.                       CI0425
            12            WF41-APCUA  PICTURE  S9(6)V9(5)               CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-AGLPA  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-AGSPA  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-ATGPA  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-ACBIN1 PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-CABRC  PICTURE  X.                       CI0425
            12            WF41-CDSON  PICTURE  X(06).                   CI0425
            12            WF41-CADVN  PICTURE  X(10).                   CI0425
            12            WF41-CVOMC1 PICTURE  X(1).                    CI0425
            12            WF41-CSSUP2 PICTURE  X.                       CI0425
            12            WF41-ACECP  PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-CCOFE  PICTURE  X.                       CI0425
            12            WF41-GRIDN7 PICTURE  S9(7)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-QNZDS  PICTURE  S9(04)                   CI0425
                          COMPUTATIONAL   SYNC RIGHT.                   CI0425
            12            WF41-QTNOL  PICTURE  S9(05)                   CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-QTNPW  PICTURE  S9(05)                   CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-IREIN  PICTURE  X.                       CI0425
            12            WF41-DNRIP  PICTURE  9(8).                    CI0425
            12            WF41-ATDPA  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-CREPC  PICTURE  XX.                      CI0425
            12            WF41-CMLTP  PICTURE  X.                       CI0425
            12            WF41-IREPL6 PICTURE  X.                       CI0425
            12            WF41-CCLAC  PICTURE  X.                       CI0425
            12            WF41-ALNTI  PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-ATIPA  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-CVASC  PICTURE  XX.                      CI0425
            12            WF41-DRTHC  PICTURE  9(8).                    CI0425
            12            WF41-CENDO  PICTURE  X.                       CI0425
            12            WF41-ICOEX  PICTURE  X(1).                    CI0425
            12            WF41-INURS  PICTURE  X(1).                    CI0425
            12            WF41-ITRML  PICTURE  X(1).                    CI0425
            12            WF41-IBIRA  PICTURE  X(1).                    CI0425
            12            WF41-CCOUL  PICTURE  XX.                      CI0425
            12            WF41-ASGLP  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-DMATUR PICTURE  9(8).                    CI0425
            12            WF41-CJOIP  PICTURE  X.                       CI0425
            12            WF41-CPNOP  PICTURE  X(2).                    CI0425
            12            WF41-ARBRP  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-ACCHV  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-ASCHV  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-CGMBR  PICTURE  X.                       CI0425
            12            WF41-AMCTV  PICTURE  S9(7)V99.                CI0425
            12            WF41-ACBIN2 PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-PE1B                                     CI0425
                          REDEFINES            WF41-ACBIN2.             CI0425
            13            WF41-NAAMC  PICTURE  9(2).                    CI0425
            13            WF41-PNPCT  PICTURE  999.                     CI0425
            13            WF41-FILLER PICTURE  X(1).                    CI0425
            11            WF41-FILLER PICTURE  X.                       CI0425
            11            WF41-PE6B.                                    CI0425
            12            WF41-CEBMO  PICTURE  9(2).                    CI0425
            12            WF41-CBNBC1 PICTURE  X.                       CI0425
            12            WF41-AMDAR  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-ALDDUE PICTURE  9(08).                   CI0425
            12            WF41-ASPAM2 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-DRESE  PICTURE  9(8).                    CI0425
            12            WF41-DACUP  PICTURE  9(02).                   CI0425
            12            WF41-ACVAMG PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-ISTWH  PICTURE  X(1).                    CI0425
            12            WF41-AVAIP  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-FILLER PICTURE  X(63).                   CI0425
            11            WF41-MPMTF  PICTURE  X(14).                   CI0425
            11            WF41-PE86.                                    CI0425
            12            WF41-CVOOD  PICTURE  S9(5)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-DEIRNB PICTURE  9(8).                    CI0425
            12            WF41-QCRPD  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-CBRIT  PICTURE  SV9(5)                   CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-DEIRNA PICTURE  9(8).                    CI0425
            11            WF41-PE91.                                    CI0425
            12            WF41-CVOOD1 PICTURE  S9(5)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-DTRMSX PICTURE  X(8).                    CI0425
            12            WF41-DTRMEX PICTURE  X(8).                    CI0425
            12            WF41-ALPRUN PICTURE  S999V9(6)                CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-PSPSX                                    CI0425
                          REDEFINES            WF41-ALPRUN              CI0425
               PICTURE    S9(7)V99                                      CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-PIXPR  PICTURE  S9V9(4)                  CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-PE3V.                                    CI0425
            12            WF41-PE90.                                    CI0425
            13            WF41-CSTCVE PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ASURR1 PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ALSURR PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ASINTC PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-AMVA1  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ASPAM  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ACVAM  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-AMSBT  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ACVALC PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ATWS   PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ISELO  PICTURE  X.                       CI0425
            13            WF41-FILLER PICTURE  X(1).                    CI0425
            13            WF41-FILLER PICTURE  X(2).                    CI0425
            13            WF41-ASTCV1 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ATFCVC PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-FILLER PICTURE  X(1).                    CI0425
            13            WF41-FILLER PICTURE  X(2).                    CI0425
            13            WF41-AUINTA PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-FILLER PICTURE  X(1).                    CI0425
            13            WF41-IRCHG  PICTURE  X.                       CI0425
            13            WF41-ASTXW8 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ATFRA  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-APLIV  PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-PCIRB1 PICTURE  S9(2)V9(3)               CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ACVAMF PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-CSNCVE PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-PCIRB7 PICTURE  99V999.                  CI0425
            13            WF41-AMNSR  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-AMINL  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ASCHV  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-PRCHG  PICTURE  999V999                  CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ISTUR  PICTURE  X.                       CI0425
            13            WF41-CSTIM  PICTURE  X.                       CI0425
            13            WF41-AMCAV1 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ARCHG  PICTURE  9(7)V99                  CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-PE7C                                     CI0425
                          REDEFINES            WF41-PE90.               CI0425
            13            WF41-ACOGR  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ACONE  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ASURR3 PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-PSURR1 PICTURE  S9(3)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-ACOTX  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-TCOMG  PICTURE  X(30).                   CI0425
            13            WF41-CVCST  PICTURE  X(4).                    CI0425
            13            WF41-FILLER PICTURE  X(100).                  CI0425
            12            WF41-ALCCV  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-ATLPD  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-ARTLP  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-APRLP  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-ALPAY  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-DLSST  PICTURE  9(8).                    CI0425
            12            WF41-ACAUN  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-ITAXN  PICTURE  X.                       CI0425
            11            WF41-ITAXD  PICTURE  X(1).                    CI0425
            11            WF41-ITAXI  PICTURE  X.                       CI0425
            11            WF41-PCIRB  PICTURE  S99V999                  CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-ALBUL  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-ACGPA  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-ACACTV PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-ASURRN PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-ASURRW PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-PSURR  PICTURE  S9(3)V999                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-ANGOF  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-ACVIU  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-MPMTT  PICTURE  X(20).                   CI0425
            11            WF41-CETRL  PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-AFETY  PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-GESTNS PICTURE  X(2).                    CI0425
            11            WF41-CTWHPB PICTURE  9(3)V999.                CI0425
            11            WF41-CTWHCB PICTURE  X.                       CI0425
            11            WF41-ACTCH  PICTURE  S9(07)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-AMXLN  PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-CACTL1 PICTURE  X(04).                   CI0425
            11            WF41-AEARN0 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-AEARN1 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-APYMT  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-PCIRA  PICTURE  S99V999                  CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-ASTCVC PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-CDBNLG PICTURE  X(3).                    CI0425
            11            WF41-ILNST  PICTURE  X.                       CI0425
            11            WF41-AENTI  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-AAFEA  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-ATPWO  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-ACVAMD PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-TBENPA PICTURE  X(30).                   CI0425
            11            WF41-PE82.                                    CI0425
            12            WF41-CPRCC  PICTURE  X.                       CI0425
            12            WF41-CSPCL  PICTURE  XX.                      CI0425
            12            WF41-PE6F                                     CI0425
                          OCCURS       006     TIMES.                   CI0425
            13            WF41-AFLEX  PICTURE  S9(3)V9(2)               CI0425
                          COMPUTATIONAL-3.                              CI0425
            13            WF41-DFEED  PICTURE  9(8).                    CI0425
            11            WF41-DWSDT  PICTURE  9(8).                    CI0425
            11            WF41-IRDPH  PICTURE  X.                       CI0425
            11            WF41-DWAIT  PICTURE  9(8).                    CI0425
            11            WF41-IAPGP  PICTURE  X.                       CI0425
            11            WF41-APGBP  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-PE6T.                                    CI0425
            12            WF41-ISDIS  PICTURE  X.                       CI0425
            12            WF41-IBPER  PICTURE  X.                       CI0425
            12            WF41-PINFL  PICTURE  X(02).                   CI0425
            12            WF41-CINFT  PICTURE  X.                       CI0425
            12            WF41-ARODE  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-ARDBL  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-ARPSL  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            WF41-CLONT  PICTURE  X.                       CI0425
            12            WF41-CLONT1 PICTURE  X.                       CI0425
            12            WF41-CLONT2 PICTURE  X.                       CI0425
            12            WF41-DPRPA  PICTURE  9(8).                    CI0425
            12            WF41-ADBSU  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-ALDBEN PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-APRYT  PICTURE  S9(09)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-AMECP  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WF41-FILLER PICTURE  X(989).                  CI0425
      ******************************************************************APRDTP
      *USED TO DETERMINE MAJOR PRODUCT TYPE FOR VALUATION               APRDTP
       01  7-PRODUCT.                                                   APRDTP
      *!WI pl=WK040                                                     APRDTP
           05  7-CPRDG                                                  APRDTP
                        PICTURE 9(2).                                   CI0425
               88 7-CERTIFICATE-PRODUCT    VALUE 01.                    APRDTP
               88 7-MUTUAL-FUND-PRODUCT    VALUE 02.                    APRDTP
               88 7-SPECIAL-PRODUCT        VALUE 03.                    APRDTP
               88 7-LIFE-PRODUCT           VALUE 04.                    APRDTP
               88 7-OTHER-PRODUCT          VALUE 10.                    APRDTP
      *!WI pl=WK100                                                     APRDTP
           05  7-CPRDA1                                                 APRDTP
                        PICTURE 9(3).                                   CI0425
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
      ******************************************************************AM0223
      ** WORKING STORAGE FOR CI0223                                    *AM0223
      ******************************************************************AM0223
      *                                                                 AM0223
      *!WF DSP=V2 DSL=V2 SEL=23 FOR=I DES=2 LEV=1                       AM0223
       01                 V223.                                         CI0425
            10            V223-GERTC  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            V223-CTID   PICTURE  X(27)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            V223-CLIDOA PICTURE  S9(3)                    CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            V223-CLID1A PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            V223-CCTXT  PICTURE  X(2)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            V223-NDCIDA PICTURE  S9(05)                   CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            V223-CMPRD  PICTURE  S9(4)                    CI0425
                          VALUE                ZERO                     CI0425
                          BINARY.                                       CI0425
            10            V223-CAACCT PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            V223-ICLID  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
      *                                                                 AM0223
      *                                                                 AM0223
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
          05  WS00-DCACG
                        PICTURE 9(8).                                   CI0425
      *!WI
          05  WS00-DNACG
                        PICTURE 9(8).                                   CI0425
          05  WS00-CCYYMMDD.
              10  WS00-CCYY           PIC 9(04).
              10  WS00-MM             PIC 9(02).
              10  WS00-DD             PIC 9(02).
      *!WI
          05  WS00-SAVE-CTID
                        PICTURE X(27).                                  CI0425
      *!WI
          05  WS00-CLID
                        PICTURE X(23).                                  CI0425
      *!WI
          05  WS00-CLCTRC
                        PICTURE 9(3).                                   CI0425
          05  WS00-AGE                PIC 999.
          05  WS00-AGE1               PIC 999.
      *!WI
          05  WS00-GRIDC
                        PICTURE 9(3).                                   CI0425
      *!WI
          05  WS00-CARTZA
                        PICTURE XX.                                     CI0425
          05  WS00-PARTKEY            PIC X.
          05  WS00-NOCX12             PIC X.
          05  WS00-DUPCX12            PIC X.
          05  WS00-NOBANK             PIC X.
      *!WI
          05  WS00-GESTE
                        PICTURE 9(8).                                   CI0425
      *!WI
          05  WS00-GEENL
                        PICTURE 9(8).                                   CI0425
      *!WI
          05  WS00-GESTD1
                        PICTURE 9(8).                                   CI0425
      *!WI
          05  WS00-DCACG1
                        PICTURE 9(8).                                   CI0425
      *!WI
          05  WS00-PROGR
                        PICTURE X(06).                                  CI0425
      *!WI
          05  WS00-DSKIP
                        PICTURE 9(8).                                   CI0425
      *!WI
          05  WS00-DSKIP1
                        PICTURE 9(8).                                   CI0425
      *!WI
          05  WS00-DSKIP2
                        PICTURE 9(8).                                   CI0425
          05  WS00-POLAGE             PIC 999.
      *!WI
          05  WS00-CPCCDE
                        PICTURE 99.                                     CI0425
      *!WI
          05  WS00-CARST
                        PICTURE 99.                                     CI0425
          05  AA85-CF                 PIC X.
          05  WS00-DIV                PIC 99.
          05  WS00-OK                 PIC X.
          05  WS00-QT-OK              PIC X.
          05  WS00-SA-OK              PIC X.
          05  WS00-AN-OK              PIC X.
      *!WI
          05  WS00-CPMTG1
                        PICTURE 99.                                     CI0425
      *!WI
          05  WS00-MPMTF1
                        PICTURE X(24).                                  CI0425
      *!WI
          05  WS00-ACOTL1
                        PICTURE S9(9)V99                                CI0425
                          COMPUTATIONAL-3.                              CI0425
      *!WI
          05  WS00-ACOTU1
                        PICTURE S9(9)V99                                CI0425
                          COMPUTATIONAL-3.                              CI0425
      *!WI
          05  WS00-MPMTT1
                        PICTURE X(20).                                  CI0425
      ** - USED TO BREAK MONTH OFF OF MMDD IRA CODE ON CONTRACT
          05  WS00-DIRAC.
      *!WI
              10  WS00-NIRACM
                        PICTURE 9(2).                                   CI0425
              10  FILLER              PIC X(02).
      *
      *   USED TO STORE ACCOUNT'S TAXPAYER CLIENT ID, IF THE ACCOUNT
      *   HAS A TAXPAYER.
      *!WI
          05  WS00-TAXPAYER-CLID
                        PICTURE X(23).                                  CI0425
      *!WI
          05  WS00-TAXPAYER-CLTYP
                        PICTURE X.                                      CI0425
      *
      *   USED TO STORE TAXPAYER'S DEATH DATE AND DEATH INDICATOR
      *!WI
          05  WS00-CLDOD
                        PICTURE 9(8).                                   CI0425
      *!WI
          05  WS00-CLDTH
                        PICTURE X.                                      CI0425
      *   WORKING STORAGE VARIABLE COUNTER
          05  WS00-COUNTER PIC 9(3) VALUE ZEROES.
          05  WS00-RCOUNT  PIC 9(3) VALUE ZEROES.
          05  WS00-LCOUNT  PIC 9(3) VALUE ZEROES.
          05  WS00-ACOUNT  PIC 9(3) VALUE ZEROES.
          05  WS00-ICOUNT  PIC 9(3) VALUE ZEROES.
          05  WS00-LREACT  PIC 9(3) VALUE ZEROES.
      *
      *NEW FIELDS BEING ADDED FOR ROA (PTR 4114)
      *!WI
          05  WS00-CORTY
                        PICTURE X.                                      CI0425
      *!WI
          05  WS00-ALOIDD
                        PICTURE 9(9)V99                                 CI0425
                          COMPUTATIONAL-3.                              CI0425
      *!WI
          05  WS00-DELOI3
                        PICTURE 9(6).                                   CI0425
      *!WI
          05  WS00-INROA
                        PICTURE X(1).                                   CI0425
      *!WI
          05  WS00-NMESA
                        PICTURE 9(6).                                   CI0425
      *!WI
          05  WS00-NMESA1
                        PICTURE 9(6).                                   CI0425
      *WORKING STORAGE FOR THE NUMBER OF CLIENT ID
       01  I2             PIC 99  VALUE ZERO.
      *WORKING STORAGE FOR MARKET CLOSE TIME
      *!WI
       01  WS-GETIMM
                        PICTURE X(8).                                   CI0425
       01  WS-GETIMM-RED REDEFINES WS-GETIMM.
           05  WS-HH      PIC XX.
           05  WS-DOT1    PIC X.
           05  WS-MM      PIC XX.
           05  WS-DOT2    PIC X.
           05  WS-SS      PIC XX.
      *!WI
       01  WS-GETIM6
                        PICTURE 9(06).                                  CI0425
       01  WS-MKT-TIME  REDEFINES WS-GETIM6.
           05  WS-MKT-HH   PIC 99.
           05  WS-MKT-MM   PIC 99.
           05  WS-MKT-SS   PIC 99.
      **** B-SHARES TO BE CLOSED******************************
      *!WI
       01  WS00-PRCOD
                        PICTURE 9(5).                                   CI0425
           88 CLOSE-B-SHARE VALUE 24, 42, 67, 102, 106, 107.
      *!WF DSP=WT DSL=CX SEL=12 FOR=I LEV=1 PLT=WT
       01                 WT00.                                         CI0425
          05              WT00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00129).                  CI0425
       01                 WT12  REDEFINES      WT00.                    CI0425
            10            WT12-CX12K.                                   CI0425
            11            WT12-CPMTC  PICTURE  99.                      CI0425
            11            WT12-NAPDS  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            WT12-GESTD  PICTURE  9(8).                    CI0425
            10            WT12-GEEND  PICTURE  9(8).                    CI0425
            10            WT12-CIRMO  PICTURE  X(12).                   CI0425
            10            WT12-CDEST  PICTURE  99.                      CI0425
            10            WT12-APMTL  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WT12-DNPMT  PICTURE  9(8).                    CI0425
            10            WT12-NIRACM PICTURE  9(2).                    CI0425
            10            WT12-CPMTF  PICTURE  99.                      CI0425
            10            WT12-IPODM  PICTURE  X.                       CI0425
            10            WT12-CLUPD  PICTURE  9(3).                    CI0425
            10            WT12-DLAUP  PICTURE  9(8).                    CI0425
            10            WT12-CWRC   PICTURE  99.                      CI0425
            10            WT12-CHCR   PICTURE  99.                      CI0425
            10            WT12-GEOPD2 PICTURE  X(8).                    CI0425
            10            WT12-GEAUN  PICTURE  9(5).                    CI0425
            10            WT12-DPCHD  PICTURE  9(8).                    CI0425
            10            WT12-DNEXE  PICTURE  9(8).                    CI0425
            10            WT12-CCSMQ  PICTURE  X.                       CI0425
            10            WT12-GCUSPZ PICTURE  X(12).                   CI0425
            10            WT12-CORTY  PICTURE  X.                       CI0425
            10            WT12-CNAVR  PICTURE  X(1).                    CI0425
            10            WT12-DELOI3 PICTURE  9(6).                    CI0425
            10            WT12-ALOIDD PICTURE  9(9)V99                  CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            WT12-FILLER PICTURE  X(5).                    CI0425
      *                                                                 AMDU04
      ******************************************************************AMDU04
      **     SEGMENT THAT CONTAINS THE ACCOUNT OWNERSHIP AND           *AMDU04
      **     BENEFICIARY FOR REQUESTED ACCOUNT ID NUMBER               *AMDU04
      ******************************************************************AMDU04
      *                                                                 AMDU04
      *!WF DSP=PA DSL=DU SEL=04 FOR=I LEV=1                             AMDU04
       01                 PA00.                                         CI0425
          05              PA00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00407).                  CI0425
       01                 PA04  REDEFINES      PA00.                    CI0425
            10            PA04-C299.                                    CI0425
            11            PA04-CTID.                                    CI0425
            12            PA04-CTIDA  PICTURE  9(3).                    CI0425
            12            PA04-CTIDN.                                   CI0425
            13            PA04-CTIDNP PICTURE  X(13).                   CI0425
            13            PA04-CTIDND PICTURE  9(11).                   CI0425
            10            PA04-IPOCH  PICTURE  X.                       CI0425
            10            PA04-FILLER PICTURE  X(099).                  CI0425
            10            PA04-CTTLN1 PICTURE  X(30).                   CI0425
            10            PA04-CTTLN2 PICTURE  X(30).                   CI0425
            10            PA04-CTTLN3 PICTURE  X(30).                   CI0425
            10            PA04-CTTBO1 PICTURE  X(45).                   CI0425
            10            PA04-CTTBO2 PICTURE  X(45).                   CI0425
            10            PA04-CTOWN  PICTURE  9(3).                    CI0425
            10            PA04-IUGMA  PICTURE  X.                       CI0425
            10            PA04-FILLER PICTURE  X(096).                  CI0425
      *                                                                 AMDU04
      *                                                                 AMDU04
      *                                                                 AMDU04
      *                                                                 AMDU04
      *                                                                 AMDU07
      ******************************************************************AMDU07
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU07
      **     ACCOUNT'S ADDRESS.  IT ALSO CONTAINS THE CL24 SEGMENT.    *AMDU07
      ******************************************************************AMDU07
      *                                                                 AMDU07
      *!WF DSP=PB DSL=DU SEL=07 FOR=I LEV=1                             AMDU07
       01                 PB00.                                         CI0425
          05              PB00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00437).                  CI0425
       01                 PB07  REDEFINES      PB00.                    CI0425
            10            PB07-C299.                                    CI0425
            11            PB07-CTID.                                    CI0425
            12            PB07-CTIDA  PICTURE  9(3).                    CI0425
            12            PB07-CTIDN.                                   CI0425
            13            PB07-CTIDNP PICTURE  X(13).                   CI0425
            13            PB07-CTIDND PICTURE  9(11).                   CI0425
            10            PB07-DCACG  PICTURE  9(8).                    CI0425
            10            PB07-FILLER PICTURE  X(100).                  CI0425
            10            PB07-CL24.                                    CI0425
            11            PB07-GELL   PICTURE  9(4)                     CI0425
                          BINARY.                                       CI0425
            11            PB07-CL24K.                                   CI0425
            12            PB07-GECSQ  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            PB07-GECSD  PICTURE  9(8).                    CI0425
            11            PB07-GECED  PICTURE  9(8).                    CI0425
            11            PB07-CREQ2  PICTURE  X.                       CI0425
            11            PB07-FILLER PICTURE  X(4).                    CI0425
            11            PB07-GECTA  PICTURE  X.                       CI0425
            11            PB07-GELCD  PICTURE  9(8).                    CI0425
            11            PB07-GEADS  PICTURE  9.                       CI0425
            11            PB07-GECIT  PICTURE  X(25).                   CI0425
            11            PB07-GECTRY PICTURE  X(20).                   CI0425
            11            PB07-GECTY  PICTURE  9(3).                    CI0425
            11            PB07-GEPCD  PICTURE  X(12).                   CI0425
            11            PB07-GEST   PICTURE  X(8).                    CI0425
            11            PB07-IRESA  PICTURE  X.                       CI0425
            11            PB07-FILLER PICTURE  X(8).                    CI0425
            11            PB07-GESAD  PICTURE  X(30)                    CI0425
                          OCCURS       003     TIMES.                   CI0425
            10            PB07-FILLER PICTURE  X(100).                  CI0425
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
       01                 PC00.                                         CI0425
          05              PC00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00917).                  CI0425
       01                 PC14  REDEFINES      PC00.                    CI0425
            10            PC14-C299.                                    CI0425
            11            PC14-CTID.                                    CI0425
            12            PC14-CTIDA  PICTURE  9(3).                    CI0425
            12            PC14-CTIDN.                                   CI0425
            13            PC14-CTIDNP PICTURE  X(13).                   CI0425
            13            PC14-CTIDND PICTURE  9(11).                   CI0425
            10            PC14-DCACG  PICTURE  9(8).                    CI0425
            10            PC14-IPOCH  PICTURE  X.                       CI0425
            10            PC14-FILLER PICTURE  X(100).                  CI0425
            10            PC14-CLID01.                                  CI0425
            11            PC14-CLIDO1 PICTURE  X(3).                    CI0425
            11            PC14-NCLID1.                                  CI0425
            12            PC14-CLIDP1 PICTURE  X(12).                   CI0425
            12            PC14-CLIDNA PICTURE  9(8).                    CI0425
            10            PC14-CLCTR  PICTURE  9(3).                    CI0425
            10            PC14-DU21                                     CI0425
                          OCCURS       025     TIMES.                   CI0425
            11            PC14-C199.                                    CI0425
            12            PC14-CLID.                                    CI0425
            13            PC14-CLIDO  PICTURE  9(3).                    CI0425
            13            PC14-CLIDN.                                   CI0425
            14            PC14-CLIDNP PICTURE  X(12).                   CI0425
            14            PC14-CLIDND PICTURE  9(8).                    CI0425
            11            PC14-CLCTRC PICTURE  9(3).                    CI0425
            10            PC14-QITEM  PICTURE  9(3).                    CI0425
            10            PC14-XIMAX  PICTURE  S9(4)                    CI0425
                          BINARY.                                       CI0425
            10            PC14-CRROL  PICTURE  X.                       CI0425
            10            PC14-FILLER PICTURE  X(099).                  CI0425
      *                                                                 AMDU14
      *                                                                 AMDU14
      *                                                                 AMDU14
      *                                                                 AMDU14
      *                                                                 AMDU15
      ******************************************************************AMDU15
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU15
      **     GROUPS FOR THE ACCOUNT NUMBER PASSED.                     *AMDU15
      ******************************************************************AMDU15
      *                                                                 AMDU15
      *!WF DSP=PD DSL=DU SEL=15 FOR=I LEV=1                             AMDU15
       01                 PD00.                                         CI0425
          05              PD00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(04181).                  CI0425
       01                 PD15  REDEFINES      PD00.                    CI0425
            10            PD15-C299.                                    CI0425
            11            PD15-CTID.                                    CI0425
            12            PD15-CTIDA  PICTURE  9(3).                    CI0425
            12            PD15-CTIDN.                                   CI0425
            13            PD15-CTIDNP PICTURE  X(13).                   CI0425
            13            PD15-CTIDND PICTURE  9(11).                   CI0425
            10            PD15-DCACG  PICTURE  9(8).                    CI0425
            10            PD15-IPOCH  PICTURE  X.                       CI0425
            10            PD15-FILLER PICTURE  X(100).                  CI0425
            10            PD15-DU18                                     CI0425
                          OCCURS       010     TIMES.                   CI0425
            11            PD15-CT10.                                    CI0425
            12            PD15-CT10K.                                   CI0425
            13            PD15-GR98.                                    CI0425
            14            PD15-GRID.                                    CI0425
            15            PD15-GRIDC  PICTURE  9(3).                    CI0425
            15            PD15-GRIDN.                                   CI0425
            16            PD15-GRIDNP PICTURE  99.                      CI0425
            16            PD15-GRIDND PICTURE  9(8).                    CI0425
            12            PD15-GR97                                     CI0425
                          REDEFINES            PD15-CT10K.              CI0425
            13            PD15-GRIDCB PICTURE  9(3).                    CI0425
            13            PD15-FILLER PICTURE  X(10).                   CI0425
            12            PD15-GERSD  PICTURE  9(8).                    CI0425
            12            PD15-GERED  PICTURE  9(8).                    CI0425
            12            PD15-GRCSI  PICTURE  X.                       CI0425
            11            PD15-GR01.                                    CI0425
            12            PD15-GR01K.                                   CI0425
            13            PD15-GR98.                                    CI0425
            14            PD15-GRID.                                    CI0425
            15            PD15-GRIDC  PICTURE  9(3).                    CI0425
            15            PD15-GRIDN.                                   CI0425
            16            PD15-GRIDNP PICTURE  99.                      CI0425
            16            PD15-GRIDND PICTURE  9(8).                    CI0425
            12            PD15-GECKD  PICTURE  9.                       CI0425
            12            PD15-GEMDA  PICTURE  9(8).                    CI0425
            12            PD15-NSEQ4B PICTURE  9(8)                     CI0425
                          BINARY.                                       CI0425
            12            PD15-GRDOR  PICTURE  9(8).                    CI0425
            12            PD15-GRIAD  PICTURE  9(8).                    CI0425
            12            PD15-GECUC  PICTURE  99.                      CI0425
            12            PD15-GRLNG  PICTURE  99.                      CI0425
            12            PD15-GESLC  PICTURE  99.                      CI0425
            12            PD15-AYSIDA PICTURE  9(3).                    CI0425
            12            PD15-AYSID  PICTURE  9(5).                    CI0425
            12            PD15-GRCSD  PICTURE  9(8).                    CI0425
            12            PD15-GRCFD  PICTURE  9(8).                    CI0425
            12            PD15-GRNCL  PICTURE  S9(5)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            PD15-GRNCT  PICTURE  S9(5)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            PD15-GRSFC  PICTURE  99.                      CI0425
            12            PD15-GRCRN  PICTURE  9(3).                    CI0425
            12            PD15-GRCSS  PICTURE  X.                       CI0425
            12            PD15-MKSRC  PICTURE  99                       CI0425
                          OCCURS       010     TIMES.                   CI0425
            12            PD15-NEFPS  PICTURE  X(5).                    CI0425
            12            PD15-DEFPS  PICTURE  9(8).                    CI0425
            12            PD15-DLSRV  PICTURE  9(8).                    CI0425
            12            PD15-CTLNI  PICTURE  X.                       CI0425
            12            PD15-CGRLI  PICTURE  X.                       CI0425
            12            PD15-CAMGR  PICTURE  9(5)                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            PD15-CAMGS  PICTURE  9(5)                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            PD15-CAMGN  PICTURE  9(3)                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            PD15-CGRMF  PICTURE  X.                       CI0425
            12            PD15-FILLER PICTURE  X(08).                   CI0425
            11            PD15-GR07.                                    CI0425
            12            PD15-GEDLA  PICTURE  9(8).                    CI0425
            12            PD15-GRAID  PICTURE  X(12).                   CI0425
            12            PD15-GRPAP  PICTURE  X(14).                   CI0425
            12            PD15-GEPHNX PICTURE  9(4).                    CI0425
            12            PD15-DPLEF  PICTURE  9(8).                    CI0425
            12            PD15-DPLAM  PICTURE  9(8).                    CI0425
            12            PD15-NCPFN  PICTURE  9(6).                    CI0425
            12            PD15-GEFYE  PICTURE  9(4).                    CI0425
            12            PD15-FILLER PICTURE  X(06).                   CI0425
            12            PD15-GRPAN  PICTURE  X(45).                   CI0425
            12            PD15-CGRPA  PICTURE  99.                      CI0425
            12            PD15-IPRTT7 PICTURE  X.                       CI0425
            12            PD15-GRPED  PICTURE  9(8).                    CI0425
            12            PD15-FILLER PICTURE  X(05).                   CI0425
            12            PD15-GRPLC  PICTURE  99.                      CI0425
            12            PD15-GRPLT  PICTURE  99.                      CI0425
            12            PD15-FILLER PICTURE  X(04).                   CI0425
            12            PD15-GEADI  PICTURE  X.                       CI0425
            12            PD15-GRCFA  PICTURE  S9(11)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            PD15-GECFY  PICTURE  9(4).                    CI0425
            12            PD15-GECFC  PICTURE  99.                      CI0425
            12            PD15-MEMPL  PICTURE  X(20).                   CI0425
            12            PD15-CAUNIT PICTURE  X(4).                    CI0425
            12            PD15-FILLER PICTURE  X(21).                   CI0425
            12            PD15-GRPPP  PICTURE  999.                     CI0425
            12            PD15-CCORT  PICTURE  9(3).                    CI0425
            12            PD15-CIDRP  PICTURE  99.                      CI0425
            12            PD15-CCDWA  PICTURE  9.                       CI0425
            12            PD15-IERSA  PICTURE  X.                       CI0425
            12            PD15-DERSA  PICTURE  9(8).                    CI0425
            12            PD15-FILLER PICTURE  X(04).                   CI0425
            10            PD15-QITEM  PICTURE  9(3).                    CI0425
            10            PD15-XIMAX  PICTURE  S9(4)                    CI0425
                          BINARY.                                       CI0425
            10            PD15-FILLER PICTURE  X(100).                  CI0425
      *                                                                 AMDU15
      *                                                                 AMDU15
      *                                                                 AMDU15
      *                                                                 AMDU15
       01   DEBUT-WSS.                                                  CI0425
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0425
            05   IK     PICTURE X.                                      CI0425
       01  CONSTANTES-PAC.                                              CI0425
           05  FILLER  PICTURE X(87)   VALUE                            CI0425
                     '6015 CAT09/08/14CI0425ADMIN   14:35:24CI0425P AMERCI0425
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0425
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0425
           05  NUGNA   PICTURE X(5).                                    CI0425
           05  APPLI   PICTURE X(3).                                    CI0425
           05  DATGN   PICTURE X(8).                                    CI0425
           05  PROGR   PICTURE X(6).                                    CI0425
           05  CODUTI  PICTURE X(8).                                    CI0425
           05  TIMGN   PICTURE X(8).                                    CI0425
           05  PROGE   PICTURE X(8).                                    CI0425
           05  COBASE  PICTURE X(4).                                    CI0425
           05  DATGNC  PICTURE X(10).                                   CI0425
           05  RELEAS  PICTURE X(7).                                    CI0425
           05  DATGE   PICTURE X(10).                                   CI0425
           05  DATSQ   PICTURE X(10).                                   CI0425
       01  DATCE.                                                       CI0425
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0425
         05  DATOR.                                                     CI0425
           10  DATOA  PICTURE XX.                                       CI0425
           10  DATOM  PICTURE XX.                                       CI0425
           10  DATOJ  PICTURE XX.                                       CI0425
       01   VARIABLES-CONDITIONNELLES.                                  CI0425
            05                  FT      PICTURE X VALUE '0'.            CI0425
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0425
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0425
            05           IACCTL PICTURE S9(4) VALUE  ZERO.
            05           IACCTR PICTURE S9(4) VALUE  ZERO.
            05           IACCTM PICTURE S9(4) VALUE +0097.
            05           IBAODL PICTURE S9(4) VALUE  ZERO.
            05           IBAODR PICTURE S9(4) VALUE  ZERO.
            05           IBAODM PICTURE S9(4) VALUE +0097.
            05           IBAPDL PICTURE S9(4) VALUE  ZERO.
            05           IBAPDR PICTURE S9(4) VALUE  ZERO.
            05           IBAPDM PICTURE S9(4) VALUE +0097.
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J26CER PICTURE S9(4) VALUE  ZERO.
            05           J50DCR PICTURE S9(4) VALUE  ZERO.
            05           J50FCR PICTURE S9(4) VALUE  ZERO.
            05           J50FPR PICTURE S9(4) VALUE  ZERO.
            05           J50JCR PICTURE S9(4) VALUE  ZERO.
            05           J50JOR PICTURE S9(4) VALUE  ZERO.
            05           J50LCR PICTURE S9(4) VALUE  ZERO.
            05           J50WFR PICTURE S9(4) VALUE  ZERO.
            05           J91HCR PICTURE S9(4) VALUE  ZERO.
            05           J91HFR PICTURE S9(4) VALUE  ZERO.
            05           J91HHR PICTURE S9(4) VALUE  ZERO.
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0425
            05       5-AA00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0425
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0425
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0425
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0425
       01               S-AA10-SSA.                                     CI0425
            10         S1-AA10-SEGNAM PICTURE X(8)                      CI0425
                                      VALUE 'LMSPCON '.                 CI0425
            10         S1-AA10-CCOM   PICTURE X VALUE '*'.              CI0425
            10          S-AA10-CCOD   PICTURE X(5)                      CI0425
                                      VALUE '-----'.                    CI0425
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0425
       01            S-AAU10-SSA.                                       CI0425
            11      S1-AAU10-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'LMSPCON '.                 CI0425
            11      S1-AAU10-CCOM   PICTURE X VALUE '*'.                CI0425
            11       S-AAU10-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            11      S1-AAU10-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(LMSPCONK'.                CI0425
            11       S-AAU10-OPER  PICTURE XX VALUE ' ='.               CI0425
            11       S-AAU10-ALCIDN   PICTURE  9(11).                   CI0425
            11  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01               S-AA85-SSA.                                     CI0425
            10         S1-AA85-SEGNAM PICTURE X(8)                      CI0425
                                      VALUE 'LMSPUWG '.                 CI0425
            10         S1-AA85-CCOM   PICTURE X VALUE '*'.              CI0425
            10          S-AA85-CCOD   PICTURE X(5)                      CI0425
                                      VALUE '-----'.                    CI0425
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0425
       01               S-CL01-SSA.                                     CI0425
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0425
                                      VALUE 'CL01    '.                 CI0425
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0425
            10          S-CL01-CCOD   PICTURE X(5)                      CI0425
                                      VALUE '-----'.                    CI0425
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0425
       01            S-CLU01-SSA.                                       CI0425
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CL01    '.                 CI0425
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0425
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CL01K'.                   CI0425
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0425
            10       S-CLU01-CL01K.                                     CI0425
            11       S-CLU01-C199.                                      CI0425
            12       S-CLU01-CLID.                                      CI0425
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0425
            13       S-CLU01-CLIDN.                                     CI0425
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0425
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0425
            10  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01               S-CL03-SSA.                                     CI0425
            10         S1-CL03-SEGNAM PICTURE X(8)                      CI0425
                                      VALUE 'CL03    '.                 CI0425
            10         S1-CL03-CCOM   PICTURE X VALUE '*'.              CI0425
            10          S-CL03-CCOD   PICTURE X(5)                      CI0425
                                      VALUE '-----'.                    CI0425
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0425
       01            S-CLA03-SSA.                                       CI0425
            10      S1-CLA03-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CL03    '.                 CI0425
            10      S1-CLA03-CCOM   PICTURE X VALUE '*'.                CI0425
            10       S-CLA03-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            10      S1-CLA03-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CLDOD'.                   CI0425
            10       S-CLA03-OPER  PICTURE XX VALUE ' ='.               CI0425
            10       S-CLA03-CLDOD    PICTURE  9(8).                    CI0425
            10  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01               S-CT01-SSA.                                     CI0425
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0425
                                      VALUE 'CT01    '.                 CI0425
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0425
            10          S-CT01-CCOD   PICTURE X(5)                      CI0425
                                      VALUE '-----'.                    CI0425
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0425
       01            S-CTU01-SSA.                                       CI0425
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CT01    '.                 CI0425
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0425
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CT01K'.                   CI0425
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0425
            10       S-CTU01-CT01K.                                     CI0425
            11       S-CTU01-C299.                                      CI0425
            12       S-CTU01-CTID.                                      CI0425
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0425
            13       S-CTU01-CTIDN.                                     CI0425
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0425
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0425
            10  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01               S-CT49-SSA.                                     CI0425
            10         S1-CT49-SEGNAM PICTURE X(8)                      CI0425
                                      VALUE 'CT49    '.                 CI0425
            10         S1-CT49-CCOM   PICTURE X VALUE '*'.              CI0425
            10          S-CT49-CCOD   PICTURE X(5)                      CI0425
                                      VALUE '-----'.                    CI0425
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0425
       01            S-CTU49-SSA.                                       CI0425
            10      S1-CTU49-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CT49    '.                 CI0425
            10      S1-CTU49-CCOM   PICTURE X VALUE '*'.                CI0425
            10       S-CTU49-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            10      S1-CTU49-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CT49K'.                   CI0425
            10       S-CTU49-OPER  PICTURE XX VALUE ' ='.               CI0425
            10       S-CTU49-CT49K.                                     CI0425
            11       S-CTU49-GESTD    PICTURE  9(8).                    CI0425
            10  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01               S-CX01-SSA.                                     CI0425
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0425
                                      VALUE 'CX01    '.                 CI0425
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0425
            10          S-CX01-CCOD   PICTURE X(5)                      CI0425
                                      VALUE '-----'.                    CI0425
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0425
       01            S-CXU01-SSA.                                       CI0425
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX01    '.                 CI0425
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0425
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CX01K'.                   CI0425
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0425
            10       S-CXU01-CX01K.                                     CI0425
            11       S-CXU01-C199.                                      CI0425
            12       S-CXU01-CLID.                                      CI0425
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0425
            13       S-CXU01-CLIDN.                                     CI0425
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0425
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0425
            10  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01               S-CX03-SSA.                                     CI0425
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0425
                                      VALUE 'CX03    '.                 CI0425
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0425
            10          S-CX03-CCOD   PICTURE X(5)                      CI0425
                                      VALUE '-----'.                    CI0425
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0425
       01            S-CXA03-SSA.                                       CI0425
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX03    '.                 CI0425
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0425
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CARTY'.                   CI0425
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0425
            12       S-CXA03-CARTY    PICTURE  99.                      CI0425
            12  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXB03-SSA.                                       CI0425
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX03    '.                 CI0425
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0425
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(NARRS'.                   CI0425
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0425
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            12  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXC03-SSA.                                       CI0425
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX03    '.                 CI0425
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0425
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CPMTG'.                   CI0425
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0425
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0425
            11  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXD03-SSA.                                       CI0425
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX03    '.                 CI0425
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0425
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(GRCRNG'.                  CI0425
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0425
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0425
            11  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXE03-SSA.                                       CI0425
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX03    '.                 CI0425
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0425
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(DEXDT'.                   CI0425
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0425
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0425
            11  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXF03-SSA.                                       CI0425
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX03    '.                 CI0425
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0425
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CY50'.                    CI0425
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0425
            11       S-CXF03-CY50.                                      CI0425
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0425
            11  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXG03-SSA.                                       CI0425
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX03    '.                 CI0425
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0425
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(NBASQ'.                   CI0425
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0425
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXH03-SSA.                                       CI0425
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX03    '.                 CI0425
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0425
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(NARID'.                   CI0425
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0425
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0425
            12  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXU03-SSA.                                       CI0425
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX03    '.                 CI0425
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0425
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CX03K'.                   CI0425
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0425
            11       S-CXU03-CX03K.                                     CI0425
            12       S-CXU03-CARTY    PICTURE  99.                      CI0425
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01               S-CX06-SSA.                                     CI0425
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0425
                                      VALUE 'CX06    '.                 CI0425
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0425
            10          S-CX06-CCOD   PICTURE X(5)                      CI0425
                                      VALUE '-----'.                    CI0425
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0425
       01            S-CXU06-SSA.                                       CI0425
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX06    '.                 CI0425
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0425
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CX06K'.                   CI0425
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0425
            10       S-CXU06-CX06K.                                     CI0425
            11       S-CXU06-C299.                                      CI0425
            12       S-CXU06-CTID.                                      CI0425
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0425
            13       S-CXU06-CTIDN.                                     CI0425
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0425
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0425
            10  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01               S-CX12-SSA.                                     CI0425
            10         S1-CX12-SEGNAM PICTURE X(8)                      CI0425
                                      VALUE 'CX12    '.                 CI0425
            10         S1-CX12-CCOM   PICTURE X VALUE '*'.              CI0425
            10          S-CX12-CCOD   PICTURE X(5)                      CI0425
                                      VALUE '-----'.                    CI0425
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0425
       01            S-CXA12-SSA.                                       CI0425
            10      S1-CXA12-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX12    '.                 CI0425
            10      S1-CXA12-CCOM   PICTURE X VALUE '*'.                CI0425
            10       S-CXA12-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            10      S1-CXA12-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CDEST'.                   CI0425
            10       S-CXA12-OPER  PICTURE XX VALUE ' ='.               CI0425
            10       S-CXA12-CDEST    PICTURE  99.                      CI0425
            10  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXB12-SSA.                                       CI0425
            10      S1-CXB12-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX12    '.                 CI0425
            10      S1-CXB12-CCOM   PICTURE X VALUE '*'.                CI0425
            10       S-CXB12-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            10      S1-CXB12-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(DNPMT'.                   CI0425
            10       S-CXB12-OPER  PICTURE XX VALUE ' ='.               CI0425
            10       S-CXB12-DNPMT    PICTURE  9(8).                    CI0425
            10  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXC12-SSA.                                       CI0425
            11      S1-CXC12-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX12    '.                 CI0425
            11      S1-CXC12-CCOM   PICTURE X VALUE '*'.                CI0425
            11       S-CXC12-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            11      S1-CXC12-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(NAPDS'.                   CI0425
            11       S-CXC12-OPER  PICTURE XX VALUE ' ='.               CI0425
            11       S-CXC12-NAPDS    PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXU12-SSA.                                       CI0425
            10      S1-CXU12-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX12    '.                 CI0425
            10      S1-CXU12-CCOM   PICTURE X VALUE '*'.                CI0425
            10       S-CXU12-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            10      S1-CXU12-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CX12K'.                   CI0425
            10       S-CXU12-OPER  PICTURE XX VALUE ' ='.               CI0425
            10       S-CXU12-CX12K.                                     CI0425
            11       S-CXU12-CPMTC    PICTURE  99.                      CI0425
            11       S-CXU12-NAPDS    PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11       S-CXU12-GESTD    PICTURE  9(8).                    CI0425
            10  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01               S-CX13-SSA.                                     CI0425
            10         S1-CX13-SEGNAM PICTURE X(8)                      CI0425
                                      VALUE 'CX13    '.                 CI0425
            10         S1-CX13-CCOM   PICTURE X VALUE '*'.              CI0425
            10          S-CX13-CCOD   PICTURE X(5)                      CI0425
                                      VALUE '-----'.                    CI0425
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0425
       01            S-CXA13-SSA.                                       CI0425
            11      S1-CXA13-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX13    '.                 CI0425
            11      S1-CXA13-CCOM   PICTURE X VALUE '*'.                CI0425
            11       S-CXA13-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            11      S1-CXA13-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CDEST'.                   CI0425
            11       S-CXA13-OPER  PICTURE XX VALUE ' ='.               CI0425
            11       S-CXA13-CDEST    PICTURE  99.                      CI0425
            11  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXB13-SSA.                                       CI0425
            12      S1-CXB13-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX13    '.                 CI0425
            12      S1-CXB13-CCOM   PICTURE X VALUE '*'.                CI0425
            12       S-CXB13-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            12      S1-CXB13-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CARTZ'.                   CI0425
            12       S-CXB13-OPER  PICTURE XX VALUE ' ='.               CI0425
            12       S-CXB13-CARTZ    PICTURE  99.                      CI0425
            12  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXC13-SSA.                                       CI0425
            12      S1-CXC13-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX13    '.                 CI0425
            12      S1-CXC13-CCOM   PICTURE X VALUE '*'.                CI0425
            12       S-CXC13-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            12      S1-CXC13-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(NAPDS'.                   CI0425
            12       S-CXC13-OPER  PICTURE XX VALUE ' ='.               CI0425
            12       S-CXC13-NAPDS    PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            12  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXU13-SSA.                                       CI0425
            11      S1-CXU13-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX13    '.                 CI0425
            11      S1-CXU13-CCOM   PICTURE X VALUE '*'.                CI0425
            11       S-CXU13-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            11      S1-CXU13-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CX13K'.                   CI0425
            11       S-CXU13-OPER  PICTURE XX VALUE ' ='.               CI0425
            11       S-CXU13-CX13K.                                     CI0425
            12       S-CXU13-CARTZ    PICTURE  99.                      CI0425
            12       S-CXU13-NAPDS    PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CX113-SSA.                                       CI0425
            11      S1-CX113-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX13    '.                 CI0425
            11      S1-CX113-CCOM   PICTURE X VALUE '*'.                CI0425
            11       S-CX113-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            11      S1-CX113-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(XGCUSPY'.                 CI0425
            11       S-CX113-OPER  PICTURE XX VALUE ' ='.               CI0425
            11       S-CX113-GCUSPY   PICTURE  X(12).                   CI0425
            11  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01               S-CX14-SSA.                                     CI0425
            10         S1-CX14-SEGNAM PICTURE X(8)                      CI0425
                                      VALUE 'CX14    '.                 CI0425
            10         S1-CX14-CCOM   PICTURE X VALUE '*'.              CI0425
            10          S-CX14-CCOD   PICTURE X(5)                      CI0425
                                      VALUE '-----'.                    CI0425
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0425
       01            S-CXU14-SSA.                                       CI0425
            10      S1-CXU14-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX14    '.                 CI0425
            10      S1-CXU14-CCOM   PICTURE X VALUE '*'.                CI0425
            10       S-CXU14-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            10      S1-CXU14-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CX14K'.                   CI0425
            10       S-CXU14-OPER  PICTURE XX VALUE ' ='.               CI0425
            10       S-CXU14-CX14K.                                     CI0425
            11       S-CXU14-NPISQ    PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CX114-SSA.                                       CI0425
            11      S1-CX114-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX14    '.                 CI0425
            11      S1-CX114-CCOM   PICTURE X VALUE '*'.                CI0425
            11       S-CX114-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            11      S1-CX114-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(XGCUSPZ'.                 CI0425
            11       S-CX114-OPER  PICTURE XX VALUE ' ='.               CI0425
            11       S-CX114-GCUSPZ   PICTURE  X(12).                   CI0425
            11  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01               S-CX18-SSA.                                     CI0425
            10         S1-CX18-SEGNAM PICTURE X(8)                      CI0425
                                      VALUE 'CX18    '.                 CI0425
            10         S1-CX18-CCOM   PICTURE X VALUE '*'.              CI0425
            10          S-CX18-CCOD   PICTURE X(5)                      CI0425
                                      VALUE '-----'.                    CI0425
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0425
       01            S-CXA18-SSA.                                       CI0425
            10      S1-CXA18-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX18    '.                 CI0425
            10      S1-CXA18-CCOM   PICTURE X VALUE '*'.                CI0425
            10       S-CXA18-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            10      S1-CXA18-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CSTPRE'.                  CI0425
            10       S-CXA18-OPER  PICTURE XX VALUE ' ='.               CI0425
            10       S-CXA18-CSTPRE   PICTURE  99.                      CI0425
            10  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXB18-SSA.                                       CI0425
            10      S1-CXB18-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX18    '.                 CI0425
            10      S1-CXB18-CCOM   PICTURE X VALUE '*'.                CI0425
            10       S-CXB18-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            10      S1-CXB18-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CSPCR'.                   CI0425
            10       S-CXB18-OPER  PICTURE XX VALUE ' ='.               CI0425
            10       S-CXB18-CSPCR    PICTURE  99.                      CI0425
            10  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXU18-SSA.                                       CI0425
            10      S1-CXU18-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX18    '.                 CI0425
            10      S1-CXU18-CCOM   PICTURE X VALUE '*'.                CI0425
            10       S-CXU18-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            10      S1-CXU18-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CX18K'.                   CI0425
            10       S-CXU18-OPER  PICTURE XX VALUE ' ='.               CI0425
            10       S-CXU18-CX18K.                                     CI0425
            11       S-CXU18-NBASQ    PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01               S-CX2Y-SSA.                                     CI0425
            10         S1-CX2Y-SEGNAM PICTURE X(8)                      CI0425
                                      VALUE 'CX2Y    '.                 CI0425
            10         S1-CX2Y-CCOM   PICTURE X VALUE '*'.              CI0425
            10          S-CX2Y-CCOD   PICTURE X(5)                      CI0425
                                      VALUE '-----'.                    CI0425
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0425
       01            S-CXA2Y-SSA.                                       CI0425
            11      S1-CXA2Y-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX2Y    '.                 CI0425
            11      S1-CXA2Y-CCOM   PICTURE X VALUE '*'.                CI0425
            11       S-CXA2Y-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            11      S1-CXA2Y-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CARTY'.                   CI0425
            11       S-CXA2Y-OPER  PICTURE XX VALUE ' ='.               CI0425
            11       S-CXA2Y-CARTY    PICTURE  99.                      CI0425
            11  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXB2Y-SSA.                                       CI0425
            11      S1-CXB2Y-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX2Y    '.                 CI0425
            11      S1-CXB2Y-CCOM   PICTURE X VALUE '*'.                CI0425
            11       S-CXB2Y-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            11      S1-CXB2Y-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(C299'.                    CI0425
            11       S-CXB2Y-OPER  PICTURE XX VALUE ' ='.               CI0425
            11       S-CXB2Y-C299.                                      CI0425
            12       S-CXB2Y-CTID.                                      CI0425
            13       S-CXB2Y-CTIDA    PICTURE  9(3).                    CI0425
            13       S-CXB2Y-CTIDN.                                     CI0425
            14       S-CXB2Y-CTIDNP   PICTURE  X(13).                   CI0425
            14       S-CXB2Y-CTIDND   PICTURE  9(11).                   CI0425
            11  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01            S-CXU2Y-SSA.                                       CI0425
            10      S1-CXU2Y-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX2Y    '.                 CI0425
            10      S1-CXU2Y-CCOM   PICTURE X VALUE '*'.                CI0425
            10       S-CXU2Y-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            10      S1-CXU2Y-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CX2YK'.                   CI0425
            10       S-CXU2Y-OPER  PICTURE XX VALUE ' ='.               CI0425
            10       S-CXU2Y-CX2YK.                                     CI0425
            11       S-CXU2Y-C299.                                      CI0425
            12       S-CXU2Y-CTID.                                      CI0425
            13       S-CXU2Y-CTIDA    PICTURE  9(3).                    CI0425
            13       S-CXU2Y-CTIDN.                                     CI0425
            14       S-CXU2Y-CTIDNP   PICTURE  X(13).                   CI0425
            14       S-CXU2Y-CTIDND   PICTURE  9(11).                   CI0425
            11       S-CXU2Y-C199.                                      CI0425
            12       S-CXU2Y-CLID.                                      CI0425
            13       S-CXU2Y-CLIDO    PICTURE  9(3).                    CI0425
            13       S-CXU2Y-CLIDN.                                     CI0425
            14       S-CXU2Y-CLIDNP   PICTURE  X(12).                   CI0425
            14       S-CXU2Y-CLIDND   PICTURE  9(8).                    CI0425
            11       S-CXU2Y-CARTY    PICTURE  99.                      CI0425
            11       S-CXU2Y-NARRS    PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01               S-CX6Y-SSA.                                     CI0425
            10         S1-CX6Y-SEGNAM PICTURE X(8)                      CI0425
                                      VALUE 'CX6Y    '.                 CI0425
            10         S1-CX6Y-CCOM   PICTURE X VALUE '*'.              CI0425
            10          S-CX6Y-CCOD   PICTURE X(5)                      CI0425
                                      VALUE '-----'.                    CI0425
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0425
       01            S-CXU6Y-SSA.                                       CI0425
            10      S1-CXU6Y-SEGNAM PICTURE X(8)                        CI0425
                                      VALUE 'CX6Y    '.                 CI0425
            10      S1-CXU6Y-CCOM   PICTURE X VALUE '*'.                CI0425
            10       S-CXU6Y-CCOD   PICTURE X(5)                        CI0425
                                      VALUE '-----'.                    CI0425
            10      S1-CXU6Y-FLDNAM PICTURE X(9)                        CI0425
                                      VALUE '(CX6YK'.                   CI0425
            10       S-CXU6Y-OPER  PICTURE XX VALUE ' ='.               CI0425
            10       S-CXU6Y-CX6YK.                                     CI0425
            11       S-CXU6Y-C299.                                      CI0425
            12       S-CXU6Y-CTID.                                      CI0425
            13       S-CXU6Y-CTIDA    PICTURE  9(3).                    CI0425
            13       S-CXU6Y-CTIDN.                                     CI0425
            14       S-CXU6Y-CTIDNP   PICTURE  X(13).                   CI0425
            14       S-CXU6Y-CTIDND   PICTURE  9(11).                   CI0425
            11       S-CXU6Y-C199.                                      CI0425
            12       S-CXU6Y-CLID.                                      CI0425
            13       S-CXU6Y-CLIDO    PICTURE  9(3).                    CI0425
            13       S-CXU6Y-CLIDN.                                     CI0425
            14       S-CXU6Y-CLIDNP   PICTURE  X(12).                   CI0425
            14       S-CXU6Y-CLIDND   PICTURE  9(8).                    CI0425
            11       S-CXU6Y-CARTY    PICTURE  99.                      CI0425
            11       S-CXU6Y-NARRS    PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11       S-CXU6Y-CTID1    PICTURE  X(27).                   CI0425
            11       S-CXU6Y-CARTZ    PICTURE  99.                      CI0425
            11       S-CXU6Y-NAPDS    PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11       S-CXU6Y-NPISQ    PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10  FILLER   PICTURE X    VALUE ')'.                        CI0425
       01   ZONES-UTILISATEUR PICTURE X.                                CI0425
      ******************************************************************AM0297
      **** CALL AREA FOR SUITABILITY MESSAGE   (LINKAGE) ***************AM0297
      ******************************************************************AM0297
      *                                                                 AM0297
      *!WF DSP=K9 DSL=K9 SEL=96 FOR=I DES=2 LEV=1                       AM0297
       01                 K996.                                         CI0425
            10            K996-GERTC  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            10            K996-ISURQ  PICTURE  X(01)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            K996-AYSID  PICTURE  9(5)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            K996-PRCOD  PICTURE  9(5)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            K996-PRSCD  PICTURE  X(9)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            K996-CCTXT  PICTURE  X(2)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            K996-NDCID  PICTURE  S9(5)                    CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            K996-CMPRD  PICTURE  S9(4)                    CI0425
                          VALUE                ZERO                     CI0425
                          BINARY.                                       CI0425
            10            K996-DRELEP PICTURE  X(10)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            K996-ACASH  PICTURE  S9(9)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            K996-CTIDA  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            K996-CINRT  PICTURE  X(40)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            K996-CQACT  PICTURE  999                      CI0425
                          VALUE                ZERO.                    CI0425
            10            K996-GRIDC  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            K996-CTOWN  PICTURE  9(3)                     CI0425
                          VALUE                ZERO.                    CI0425
      ******************************************************************AMC320
      ** WORKING STORAGE FOR CI0320                                    *AMC320
      ******************************************************************AMC320
      *                                                                 AMC320
      *!WF DSP=W1 DSL=V1 SEL=67 FOR=I DES=2 LEV=1                       AMC320
       01                 W167.                                         CI0425
            10            W167-NVERN  PICTURE  99V99                    CI0425
                          VALUE                ZERO.                    CI0425
            10            W167-CERRE  PICTURE  X(4)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            W167-TERRAB PICTURE  X(70)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            W167-MEPID  PICTURE  X(06)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            W167-DCACG  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            W167-XDATCU PICTURE  X(8)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            W167-FILLER PICTURE  9(15)                    CI0425
                          VALUE                ZERO.                    CI0425
            10            W167-GEOPDE PICTURE  X(8)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            W167-CAUNIT PICTURE  X(4)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            W167-FILLER PICTURE  X(10)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            W167-FILLER PICTURE  X(96)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            W167-CRETU  PICTURE  X(02)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            W167-MOPTN3 PICTURE  X(50)                    CI0425
                          OCCURS       030     TIMES                    CI0425
                          VALUE                SPACE.                   CI0425
            10            W167-FILLER                                   CI0425
                          OCCURS       030     TIMES.                   CI0425
            11            W167-MOPTN4 PICTURE  X(50)                    CI0425
                          VALUE                SPACE.                   CI0425
            11            W167-MOPTN5 PICTURE  X(50)                    CI0425
                          VALUE                SPACE.                   CI0425
            11            W167-MOPTN6 PICTURE  X(10)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            W167-CLID   PICTURE  X(23)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            W167-V166                                     CI0425
                          OCCURS       013     TIMES.                   CI0425
            11            W167-MOPTN2 PICTURE  X(50)                    CI0425
                          VALUE                SPACE.                   CI0425
            11            W167-V153.                                    CI0425
            12            W167-CPEXP  PICTURE  X(1)                     CI0425
                          OCCURS       008     TIMES                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-TCPEX  PICTURE  X(30)                    CI0425
                          OCCURS       008     TIMES                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CCPEX  PICTURE  X                        CI0425
                          OCCURS       008     TIMES                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-MAPPN  PICTURE  X(10)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CLIDO8 PICTURE  S9(3)                    CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            W167-CLIDN  PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-ICFAE  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-TICFE  PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CICFE  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-ICFAR  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-TICFR  PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CICFR  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-PTAXBS PICTURE  S9(2)V99                 CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            W167-INTAX  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-TPTAX  PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CTAXB  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-TOCCU  PICTURE  X(40)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CTOCU  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-MEMNM  PICTURE  X(45)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CMEMN  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-MEAD1A PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CMEAD  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-MEAD2A PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-MECIT  PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CENME  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CESCD  PICTURE  X(2)                     CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CCESC  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CEPOS  PICTURE  X(10)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CCEPO  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-GECTRY PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CDLSC  PICTURE  X(2)                     CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-NDLNB  PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-NPASP  PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CINEX  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-TCINE  PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CCINE  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CPSIC  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-TCPSI  PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CCPSI  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CSEOC  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-TCSEO  PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CCSEO  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CIAIC  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-TCIAI  PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CCIAI  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CNWCD  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-TCNWC  PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CCNWC  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CLNWC  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-TCLNW  PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CCLNW  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-NCLDP  PICTURE  S9(3)                    CI0425
                          VALUE                ZERO                     CI0425
                          BINARY.                                       CI0425
            12            W167-INNDP  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CNCLD  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-DCONFS PICTURE  X(10)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-INDCO  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CSUSR  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-DXTMST PICTURE  X(26)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-DXTMS2 PICTURE  X(26)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-GEOPDC PICTURE  X(8)                     CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-GEOPDB PICTURE  X(8)                     CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-ICMPF  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CLAIN  PICTURE  S9(11)                   CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            W167-INAIN  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-ACLNW  PICTURE  S9(15)                   CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            W167-INNWA  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-ACLLN  PICTURE  S9(15)                   CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            W167-INLWA  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-AGLNT  PICTURE  S9(9)                    CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            W167-AGRNT  PICTURE  9(9)                     CI0425
                          VALUE                ZERO.                    CI0425
            12            W167-GEPHN  PICTURE  X(14)                    CI0425
                          VALUE                SPACE.                   CI0425
            12            W167-CGEPH  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
      *!WF DSP=PY DSL=PY SEL=15 FOR=I LEV=1                             AMC320
       01                 PY00.                                         CI0425
          05              PY00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00142).                  CI0425
       01                 PY15  REDEFINES      PY00.                    CI0425
            10            PY15-NVERN  PICTURE  99V99.                   CI0425
            10            PY15-CERRE  PICTURE  X(4).                    CI0425
            10            PY15-TERRAB PICTURE  X(70).                   CI0425
            10            PY15-MEPID  PICTURE  X(06).                   CI0425
            10            PY15-DCACG  PICTURE  9(8).                    CI0425
            10            PY15-XDATCU PICTURE  X(8).                    CI0425
            10            PY15-XMSTS  PICTURE  S9(15)                   CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            PY15-GEOPDB PICTURE  X(8).                    CI0425
            10            PY15-CAUNIT PICTURE  X(4).                    CI0425
            10            PY15-FILLER PICTURE  X(20).                   CI0425
            10            PY15-CRETU  PICTURE  X(02).                   CI0425
      *PCB POINTER                                                      AMC320
      *
      ******************************************************************AMC323
      ** WORKING STORAGE FOR CI0323                                    *AMC323
      ******************************************************************AMC323
      *                                                                 AMC323
      *!WF DSP=W2 DSL=V1 SEL=60 FOR=I DES=2 LEV=1                       AMC323
       01                 W260.                                         CI0425
            10            W260-NVERN  PICTURE  99V99                    CI0425
                          VALUE                ZERO.                    CI0425
            10            W260-CERRE  PICTURE  X(4)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            W260-TERRAB PICTURE  X(70)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            W260-MEPID  PICTURE  X(06)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            W260-DCACG  PICTURE  9(8)                     CI0425
                          VALUE                ZERO.                    CI0425
            10            W260-XDATCU PICTURE  X(8)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            W260-XMSTS  PICTURE  S9(15)                   CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            W260-GEOPID PICTURE  X(6)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            W260-CAUNIT PICTURE  X(4)                     CI0425
                          VALUE                SPACE.                   CI0425
            10            W260-FILLER PICTURE  X(10)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            W260-FILLER PICTURE  X(96)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            W260-CRETU  PICTURE  X(02)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            W260-CTID   PICTURE  X(27)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            W260-MOPTN2 PICTURE  X(50)                    CI0425
                          VALUE                SPACE.                   CI0425
            10            W260-V159.                                    CI0425
            11            W260-GERTC  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-CTNOB  PICTURE  X                        CI0425
                          OCCURS       003     TIMES                    CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-TCINO  PICTURE  X(30)                    CI0425
                          OCCURS       003     TIMES                    CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-CCINO  PICTURE  X                        CI0425
                          OCCURS       003     TIMES                    CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-CTOBR  PICTURE  X                        CI0425
                          OCCURS       003     TIMES                    CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-MAPPN  PICTURE  X(10)                    CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-CTIDA3 PICTURE  S9(3)                    CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            W260-CTIDN  PICTURE  X(24)                    CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-CLIDOA PICTURE  S9(3)                    CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            W260-NCLDN1 PICTURE  X(20)                    CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-CRITO  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-TCRIT  PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-CCRIT  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-CTIFR  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-TCTIF  PICTURE  X(30)                    CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-CCTIF  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-AINVT  PICTURE  S9(11)                   CI0425
                          VALUE                ZERO                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            W260-IAINV  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-CSASR  PICTURE  X                        CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-DXTMS2 PICTURE  X(26)                    CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-DXTMST PICTURE  X(26)                    CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-GEOPDB PICTURE  X(8)                     CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-GEOPDC PICTURE  X(8)                     CI0425
                          VALUE                SPACE.                   CI0425
            11            W260-ITSCI  PICTURE  X(1)                     CI0425
                          VALUE                SPACE.                   CI0425
      *PCB FOR CT1P                                                     AMC323
      *
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
      ** PCB POINTER FOR CBTP                                           ADU015
            05 PCB-CBTP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CH1P                                           ADU015
            05 PCB-CH1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CA1P                                           ADU015
            05 PCB-CA1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CPRP                                           ADU015
            05 PCB-CPRP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CCRP                                           ADU015
            05 PCB-CCRP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LM1P                                           ADU015
            05 PCB-LM1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LH1P                                           ADU015
            05 PCB-LH1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LPDP                                           ADU015
            05 PCB-LPDP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR ARAY                                           ADU015
            05 PCB-ARAY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AREY                                           ADU015
            05 PCB-AREY-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR GR1P                                           ADU015
            05 PCB-GR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LUVP                                           ADU015
            05 PCB-LUVP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LARP                                           ADU015
            05 PCB-LARP-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CBTP                                             ADU015
      *!WF DSP=XA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XA00.                                         CI0425
          05              XA00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XA06  REDEFINES      XA00.                    CI0425
            10            XA06-XDBPCB.                                  CI0425
            11            XA06-XDBDNM PICTURE  X(08).                   CI0425
            11            XA06-XSEGLV PICTURE  X(02).                   CI0425
            11            XA06-XRC    PICTURE  X(02).                   CI0425
            11            XA06-XPROPT PICTURE  X(04).                   CI0425
            11            XA06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XA06-XSEGNM PICTURE  X(08).                   CI0425
            11            XA06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XA06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XA06-XCOKEY PICTURE  X(70).                   CI0425
      *** PCB MASK FOR CH1P                                             ADU015
      *!WF DSP=XB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XB00.                                         CI0425
          05              XB00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XB06  REDEFINES      XB00.                    CI0425
            10            XB06-XDBPCB.                                  CI0425
            11            XB06-XDBDNM PICTURE  X(08).                   CI0425
            11            XB06-XSEGLV PICTURE  X(02).                   CI0425
            11            XB06-XRC    PICTURE  X(02).                   CI0425
            11            XB06-XPROPT PICTURE  X(04).                   CI0425
            11            XB06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XB06-XSEGNM PICTURE  X(08).                   CI0425
            11            XB06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XB06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XB06-XCOKEY PICTURE  X(70).                   CI0425
      *** PCB MASK FOR CA1P                                             ADU015
      *!WF DSP=XC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XC00.                                         CI0425
          05              XC00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XC06  REDEFINES      XC00.                    CI0425
            10            XC06-XDBPCB.                                  CI0425
            11            XC06-XDBDNM PICTURE  X(08).                   CI0425
            11            XC06-XSEGLV PICTURE  X(02).                   CI0425
            11            XC06-XRC    PICTURE  X(02).                   CI0425
            11            XC06-XPROPT PICTURE  X(04).                   CI0425
            11            XC06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XC06-XSEGNM PICTURE  X(08).                   CI0425
            11            XC06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XC06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XC06-XCOKEY PICTURE  X(70).                   CI0425
      *** PCB MASK FOR CPRP                                             ADU015
      *!WF DSP=XD DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XD00.                                         CI0425
          05              XD00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XD06  REDEFINES      XD00.                    CI0425
            10            XD06-XDBPCB.                                  CI0425
            11            XD06-XDBDNM PICTURE  X(08).                   CI0425
            11            XD06-XSEGLV PICTURE  X(02).                   CI0425
            11            XD06-XRC    PICTURE  X(02).                   CI0425
            11            XD06-XPROPT PICTURE  X(04).                   CI0425
            11            XD06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XD06-XSEGNM PICTURE  X(08).                   CI0425
            11            XD06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XD06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XD06-XCOKEY PICTURE  X(70).                   CI0425
      *** PCB MASK FOR CCRP                                             ADU015
      *!WF DSP=XE DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XE00.                                         CI0425
          05              XE00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XE06  REDEFINES      XE00.                    CI0425
            10            XE06-XDBPCB.                                  CI0425
            11            XE06-XDBDNM PICTURE  X(08).                   CI0425
            11            XE06-XSEGLV PICTURE  X(02).                   CI0425
            11            XE06-XRC    PICTURE  X(02).                   CI0425
            11            XE06-XPROPT PICTURE  X(04).                   CI0425
            11            XE06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XE06-XSEGNM PICTURE  X(08).                   CI0425
            11            XE06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XE06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XE06-XCOKEY PICTURE  X(70).                   CI0425
      *** PCB MASK FOR LM1P                                             ADU015
      *!WF DSP=XG DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XG00.                                         CI0425
          05              XG00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XG06  REDEFINES      XG00.                    CI0425
            10            XG06-XDBPCB.                                  CI0425
            11            XG06-XDBDNM PICTURE  X(08).                   CI0425
            11            XG06-XSEGLV PICTURE  X(02).                   CI0425
            11            XG06-XRC    PICTURE  X(02).                   CI0425
            11            XG06-XPROPT PICTURE  X(04).                   CI0425
            11            XG06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XG06-XSEGNM PICTURE  X(08).                   CI0425
            11            XG06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XG06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XG06-XCOKEY PICTURE  X(70).                   CI0425
      *** PCB MASK FOR LH1P                                             ADU015
      *!WF DSP=XH DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XH00.                                         CI0425
          05              XH00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XH06  REDEFINES      XH00.                    CI0425
            10            XH06-XDBPCB.                                  CI0425
            11            XH06-XDBDNM PICTURE  X(08).                   CI0425
            11            XH06-XSEGLV PICTURE  X(02).                   CI0425
            11            XH06-XRC    PICTURE  X(02).                   CI0425
            11            XH06-XPROPT PICTURE  X(04).                   CI0425
            11            XH06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XH06-XSEGNM PICTURE  X(08).                   CI0425
            11            XH06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XH06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XH06-XCOKEY PICTURE  X(70).                   CI0425
      *** PCB MASK FOR LPDP                                             ADU015
      *!WF DSP=XI DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XI00.                                         CI0425
          05              XI00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XI06  REDEFINES      XI00.                    CI0425
            10            XI06-XDBPCB.                                  CI0425
            11            XI06-XDBDNM PICTURE  X(08).                   CI0425
            11            XI06-XSEGLV PICTURE  X(02).                   CI0425
            11            XI06-XRC    PICTURE  X(02).                   CI0425
            11            XI06-XPROPT PICTURE  X(04).                   CI0425
            11            XI06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XI06-XSEGNM PICTURE  X(08).                   CI0425
            11            XI06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XI06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XI06-XCOKEY PICTURE  X(70).                   CI0425
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=XJ DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XJ00.                                         CI0425
          05              XJ00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XJ06  REDEFINES      XJ00.                    CI0425
            10            XJ06-XDBPCB.                                  CI0425
            11            XJ06-XDBDNM PICTURE  X(08).                   CI0425
            11            XJ06-XSEGLV PICTURE  X(02).                   CI0425
            11            XJ06-XRC    PICTURE  X(02).                   CI0425
            11            XJ06-XPROPT PICTURE  X(04).                   CI0425
            11            XJ06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XJ06-XSEGNM PICTURE  X(08).                   CI0425
            11            XJ06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XJ06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XJ06-XCOKEY PICTURE  X(70).                   CI0425
      *** PCB MASK FOR ARAY                                             ADU015
      *!WF DSP=XK DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XK00.                                         CI0425
          05              XK00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XK06  REDEFINES      XK00.                    CI0425
            10            XK06-XDBPCB.                                  CI0425
            11            XK06-XDBDNM PICTURE  X(08).                   CI0425
            11            XK06-XSEGLV PICTURE  X(02).                   CI0425
            11            XK06-XRC    PICTURE  X(02).                   CI0425
            11            XK06-XPROPT PICTURE  X(04).                   CI0425
            11            XK06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XK06-XSEGNM PICTURE  X(08).                   CI0425
            11            XK06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XK06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XK06-XCOKEY PICTURE  X(70).                   CI0425
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=XL DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XL00.                                         CI0425
          05              XL00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XL06  REDEFINES      XL00.                    CI0425
            10            XL06-XDBPCB.                                  CI0425
            11            XL06-XDBDNM PICTURE  X(08).                   CI0425
            11            XL06-XSEGLV PICTURE  X(02).                   CI0425
            11            XL06-XRC    PICTURE  X(02).                   CI0425
            11            XL06-XPROPT PICTURE  X(04).                   CI0425
            11            XL06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XL06-XSEGNM PICTURE  X(08).                   CI0425
            11            XL06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XL06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XL06-XCOKEY PICTURE  X(70).                   CI0425
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=XM DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XM00.                                         CI0425
          05              XM00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XM06  REDEFINES      XM00.                    CI0425
            10            XM06-XDBPCB.                                  CI0425
            11            XM06-XDBDNM PICTURE  X(08).                   CI0425
            11            XM06-XSEGLV PICTURE  X(02).                   CI0425
            11            XM06-XRC    PICTURE  X(02).                   CI0425
            11            XM06-XPROPT PICTURE  X(04).                   CI0425
            11            XM06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XM06-XSEGNM PICTURE  X(08).                   CI0425
            11            XM06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XM06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XM06-XCOKEY PICTURE  X(70).                   CI0425
      *** PCB MASK FOR AREY                                             ADU015
      *!WF DSP=XN DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XN00.                                         CI0425
          05              XN00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XN06  REDEFINES      XN00.                    CI0425
            10            XN06-XDBPCB.                                  CI0425
            11            XN06-XDBDNM PICTURE  X(08).                   CI0425
            11            XN06-XSEGLV PICTURE  X(02).                   CI0425
            11            XN06-XRC    PICTURE  X(02).                   CI0425
            11            XN06-XPROPT PICTURE  X(04).                   CI0425
            11            XN06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XN06-XSEGNM PICTURE  X(08).                   CI0425
            11            XN06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XN06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XN06-XCOKEY PICTURE  X(70).                   CI0425
      *** PCB MASK FOR GR1P                                             ADU015
      *!WF DSP=XO DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XO00.                                         CI0425
          05              XO00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XO06  REDEFINES      XO00.                    CI0425
            10            XO06-XDBPCB.                                  CI0425
            11            XO06-XDBDNM PICTURE  X(08).                   CI0425
            11            XO06-XSEGLV PICTURE  X(02).                   CI0425
            11            XO06-XRC    PICTURE  X(02).                   CI0425
            11            XO06-XPROPT PICTURE  X(04).                   CI0425
            11            XO06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XO06-XSEGNM PICTURE  X(08).                   CI0425
            11            XO06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XO06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XO06-XCOKEY PICTURE  X(70).                   CI0425
      *** PCB MASK FOR LUVP                                             ADU015
      *!WF DSP=XP DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XP00.                                         CI0425
          05              XP00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XP06  REDEFINES      XP00.                    CI0425
            10            XP06-XDBPCB.                                  CI0425
            11            XP06-XDBDNM PICTURE  X(08).                   CI0425
            11            XP06-XSEGLV PICTURE  X(02).                   CI0425
            11            XP06-XRC    PICTURE  X(02).                   CI0425
            11            XP06-XPROPT PICTURE  X(04).                   CI0425
            11            XP06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XP06-XSEGNM PICTURE  X(08).                   CI0425
            11            XP06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XP06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XP06-XCOKEY PICTURE  X(70).                   CI0425
      *** PCB MASK FOR LARP                                             ADU015
      *!WF DSP=XQ DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XQ00.                                         CI0425
          05              XQ00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00106).                  CI0425
       01                 XQ06  REDEFINES      XQ00.                    CI0425
            10            XQ06-XDBPCB.                                  CI0425
            11            XQ06-XDBDNM PICTURE  X(08).                   CI0425
            11            XQ06-XSEGLV PICTURE  X(02).                   CI0425
            11            XQ06-XRC    PICTURE  X(02).                   CI0425
            11            XQ06-XPROPT PICTURE  X(04).                   CI0425
            11            XQ06-FILLER PICTURE  S9(5)                    CI0425
                          BINARY.                                       CI0425
            11            XQ06-XSEGNM PICTURE  X(08).                   CI0425
            11            XQ06-XKEYLN PICTURE  S9(05)                   CI0425
                          BINARY.                                       CI0425
            11            XQ06-XSEGNB PICTURE  9(05)                    CI0425
                          BINARY.                                       CI0425
            11            XQ06-XCOKEY PICTURE  X(70).                   CI0425

      *PASS AREA INCLUDE USER DATA
      *!WF DSP=QT DSL=QT SEL=65 FOR=I DES=1 LEV=1 PLT=10
       01                 QT63.                                         CI0425
            10            QT63-INPUT.                                   CI0425
            11            QT63-GEMDA  PICTURE  9(8).                    CI0425
            11            QT63-NSEQ4B PICTURE  9(8)                     CI0425
                          BINARY.                                       CI0425
            11            QT63-DCACG  PICTURE  9(8).                    CI0425
            11            QT63-C199.                                    CI0425
            12            QT63-CLID.                                    CI0425
            13            QT63-CLIDO  PICTURE  9(3).                    CI0425
            13            QT63-CLIDN.                                   CI0425
            14            QT63-CLIDNP PICTURE  X(12).                   CI0425
            14            QT63-CLIDND PICTURE  9(8).                    CI0425
            11            QT63-NARRS  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-AACTV  PICTURE  S9(11)V99                CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-CTCUS  PICTURE  999.                     CI0425
            11            QT63-CTOWN  PICTURE  9(3).                    CI0425
            11            QT63-CTKRAA PICTURE  X(12).                   CI0425
            11            QT63-AMAXA2 PICTURE  S9(7)V99.                CI0425
            11            QT63-AMAXAL PICTURE  S9(7)V99.                CI0425
            10            QT63-OUTPT1.                                  CI0425
            11            QT63-IARTYA PICTURE  X.                       CI0425
            11            QT63-NMESAA PICTURE  9(6).                    CI0425
            11            QT63-IACHI  PICTURE  X.                       CI0425
            11            QT63-NMESAC PICTURE  9(6).                    CI0425
            11            QT63-IAIND2 PICTURE  X.                       CI0425
            11            QT63-NMESAR PICTURE  9(6).                    CI0425
            11            QT63-IARRGA PICTURE  X.                       CI0425
            11            QT63-NMESA0 PICTURE  9(6).                    CI0425
            11            QT63-IARLNA PICTURE  X.                       CI0425
            11            QT63-NMESA1 PICTURE  9(6).                    CI0425
            11            QT63-IARCDA PICTURE  X.                       CI0425
            11            QT63-NMESA2 PICTURE  9(6).                    CI0425
            11            QT63-IARCPA PICTURE  X.                       CI0425
            11            QT63-NMESA3 PICTURE  9(6).                    CI0425
            11            QT63-IARRG1 PICTURE  X.                       CI0425
            11            QT63-NMESA4 PICTURE  9(6).                    CI0425
            11            QT63-IARLN1 PICTURE  X.                       CI0425
            11            QT63-NMESA5 PICTURE  9(6).                    CI0425
            11            QT63-IARCD1 PICTURE  X.                       CI0425
            11            QT63-NMESA6 PICTURE  9(6).                    CI0425
            11            QT63-IARCP1 PICTURE  X.                       CI0425
            11            QT63-NMESA7 PICTURE  9(6).                    CI0425
            11            QT63-IAINDA PICTURE  X.                       CI0425
            11            QT63-NAPDSK PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-CDEST1 PICTURE  99.                      CI0425
            11            QT63-CPCCDE PICTURE  99.                      CI0425
            11            QT63-IFQAN  PICTURE  X.                       CI0425
            11            QT63-IFQSA  PICTURE  X.                       CI0425
            11            QT63-IFQQT  PICTURE  X.                       CI0425
            11            QT63-IFQBM  PICTURE  X.                       CI0425
            11            QT63-IFQMO  PICTURE  X.                       CI0425
            11            QT63-IFQSM  PICTURE  X.                       CI0425
            11            QT63-IFQBW  PICTURE  X.                       CI0425
            11            QT63-IFQWK  PICTURE  X.                       CI0425
            11            QT63-IFQOD  PICTURE  X.                       CI0425
            11            QT63-ALMIN  PICTURE  S9(5)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-ALMIN3 PICTURE  S9(5)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-ALPAGQ PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-ALPAGM PICTURE  S9(7)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-ALPLDT PICTURE  9(8).                    CI0425
            11            QT63-ALDDUE PICTURE  9(08).                   CI0425
            11            QT63-DGRAC  PICTURE  9(08).                   CI0425
            11            QT63-GESTE  PICTURE  9(8).                    CI0425
            11            QT63-GESTL  PICTURE  9(8).                    CI0425
            11            QT63-GEENE  PICTURE  9(8).                    CI0425
            11            QT63-GEENL  PICTURE  9(8).                    CI0425
            11            QT63-GESTD1 PICTURE  9(8).                    CI0425
            11            QT63-DSKIP  PICTURE  9(8).                    CI0425
            11            QT63-DSKIP1 PICTURE  9(8).                    CI0425
            11            QT63-DSKIP2 PICTURE  9(8).                    CI0425
            11            QT63-DIRAC1 PICTURE  XX.                      CI0425
            11            QT63-CIRAT  PICTURE  999.                     CI0425
            11            QT63-CIRAS  PICTURE  999.                     CI0425
            11            QT63-CQACT  PICTURE  999.                     CI0425
            11            QT63-IERRC  PICTURE  X.                       CI0425
            11            QT63-NMESA  PICTURE  9(6).                    CI0425
            10            QT63-OUTPT2.                                  CI0425
            11            QT63-CPMTG1 PICTURE  99.                      CI0425
            11            QT63-MPMTF1 PICTURE  X(24).                   CI0425
            11            QT63-ACOTL1 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-ACOTU1 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-CPMTG2 PICTURE  99.                      CI0425
            11            QT63-MPMTF2 PICTURE  X(24).                   CI0425
            11            QT63-ACOTL2 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-ACOTU2 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-CPMTG3 PICTURE  99.                      CI0425
            11            QT63-MPMTF3 PICTURE  X(24).                   CI0425
            11            QT63-ACOTL3 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-ACOTU3 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-CPMTG4 PICTURE  99.                      CI0425
            11            QT63-MPMTF4 PICTURE  X(24).                   CI0425
            11            QT63-ACOTL4 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-ACOTU4 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-CPMTG5 PICTURE  99.                      CI0425
            11            QT63-MPMTF5 PICTURE  X(24).                   CI0425
            11            QT63-ACOTL5 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-ACOTU5 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-CPMTG6 PICTURE  99.                      CI0425
            11            QT63-MPMTF6 PICTURE  X(24).                   CI0425
            11            QT63-ACOTL6 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-ACOTU6 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-CPMTG7 PICTURE  99.                      CI0425
            11            QT63-MPMTF7 PICTURE  X(24).                   CI0425
            11            QT63-ACOTL7 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-ACOTU7 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-CPMTG8 PICTURE  99.                      CI0425
            11            QT63-MPMTF8 PICTURE  X(24).                   CI0425
            11            QT63-ACOTL8 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-ACOTU8 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-CPMTG9 PICTURE  99.                      CI0425
            11            QT63-MPMTF9 PICTURE  X(24).                   CI0425
            11            QT63-ACOTL9 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-ACOTU9 PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-MPMTT1 PICTURE  X(20).                   CI0425
            11            QT63-MPMTT2 PICTURE  X(20).                   CI0425
            11            QT63-MPMTT3 PICTURE  X(20).                   CI0425
            11            QT63-MPMTT4 PICTURE  X(20).                   CI0425
            11            QT63-MPMTT5 PICTURE  X(20).                   CI0425
            11            QT63-IABAA  PICTURE  X(01).                   CI0425
            11            QT63-IIBAA  PICTURE  X(01).                   CI0425
            11            QT63-IDBMO  PICTURE  X.                       CI0425
            11            QT63-IDBQT  PICTURE  X.                       CI0425
            11            QT63-IDBSA  PICTURE  X.                       CI0425
            11            QT63-IDBAN  PICTURE  X.                       CI0425
            11            QT63-CPCCD1 PICTURE  9(5).                    CI0425
            11            QT63-PRCOD  PICTURE  9(5).                    CI0425
            11            QT63-OWNOUT PICTURE  X(60).                   CI0425
            11            QT63-TSECD  PICTURE  X(30).                   CI0425
            11            QT63-CSPRP  PICTURE  X(04).                   CI0425
            11            QT63-QSTSO  PICTURE  S999                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-QSTSM  PICTURE  S999                     CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-INROA  PICTURE  X(1).                    CI0425
            11            QT63-FILLER PICTURE  X(19).                   CI0425
            10            QT63-LIMITS                                   CI0425
                          REDEFINES            QT63-OUTPT2.             CI0425
            11            QT63-QT6R                                     CI0425
                          OCCURS       009     TIMES.                   CI0425
            12            QT63-CPMTFA PICTURE  X(2).                    CI0425
            12            QT63-MPMTFL PICTURE  X(24).                   CI0425
            12            QT63-ACOTL  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            QT63-ACOTU  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-MPMTT  PICTURE  X(20)                    CI0425
                          OCCURS       005     TIMES.                   CI0425
            10            QT63-CX06.                                    CI0425
            11            QT63-CX06K.                                   CI0425
            12            QT63-C299.                                    CI0425
            13            QT63-CTID.                                    CI0425
            14            QT63-CTIDA  PICTURE  9(3).                    CI0425
            14            QT63-CTIDN.                                   CI0425
            15            QT63-CTIDNP PICTURE  X(13).                   CI0425
            15            QT63-CTIDND PICTURE  9(11).                   CI0425
            11            QT63-NPECK  PICTURE  9(02).                   CI0425
            11            QT63-FILLER PICTURE  X.                       CI0425
            10            QT63-CX12.                                    CI0425
            11            QT63-CX12K.                                   CI0425
            12            QT63-CPMTC  PICTURE  99.                      CI0425
            12            QT63-NAPDS  PICTURE  S9(3)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            12            QT63-GESTD  PICTURE  9(8).                    CI0425
            11            QT63-GEEND  PICTURE  9(8).                    CI0425
            11            QT63-CIRMO  PICTURE  X(12).                   CI0425
            11            QT63-CDEST  PICTURE  99.                      CI0425
            11            QT63-APMTL  PICTURE  S9(9)V99                 CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-DNPMT  PICTURE  9(8).                    CI0425
            11            QT63-NIRACM PICTURE  9(2).                    CI0425
            11            QT63-CPMTF  PICTURE  99.                      CI0425
            11            QT63-IPODM  PICTURE  X.                       CI0425
            11            QT63-CLUPD  PICTURE  9(3).                    CI0425
            11            QT63-DLAUP  PICTURE  9(8).                    CI0425
            11            QT63-CWRC   PICTURE  99.                      CI0425
            11            QT63-CHCR   PICTURE  99.                      CI0425
            11            QT63-GEOPD2 PICTURE  X(8).                    CI0425
            11            QT63-GEAUN  PICTURE  9(5).                    CI0425
            11            QT63-DPCHD  PICTURE  9(8).                    CI0425
            11            QT63-DNEXE  PICTURE  9(8).                    CI0425
            11            QT63-CCSMQ  PICTURE  X.                       CI0425
            11            QT63-GCUSPZ PICTURE  X(12).                   CI0425
            11            QT63-CORTY  PICTURE  X.                       CI0425
            11            QT63-CNAVR  PICTURE  X(1).                    CI0425
            11            QT63-DELOI3 PICTURE  9(6).                    CI0425
            11            QT63-ALOIDD PICTURE  9(9)V99                  CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            QT63-FILLER PICTURE  X(5).                    CI0425
       01                 QT65.                                         CI0425
            10            QT65-CLID   PICTURE  X(23).                   CI0425
            10            QT65-IORGC  PICTURE  X.                       CI0425
            10            QT65-QITEM  PICTURE  9(3).                    CI0425
            10            QT65-FILLER PICTURE  X(296).                  CI0425
      *PASS AREA TO/FROM CI0225 (& CI0278 & CI9040)
      *!WF DSP=QT DSL=QT SEL=63 FOR=I DES=1 LEV=1 PLT=10
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0425
          05              DE00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00653).                  CI0425
       01                 DE10  REDEFINES      DE00.                    CI0425
            10            DE10-DU11.                                    CI0425
            11            DE10-XFONC  PICTURE  X(4).                    CI0425
            11            DE10-MPSBN  PICTURE  X(8).                    CI0425
            11            DE10-XDBDNM PICTURE  X(08).                   CI0425
            11            DE10-XSEGNM PICTURE  X(08).                   CI0425
            11            DE10-XRC    PICTURE  X(02).                   CI0425
            11            DE10-MSEG   PICTURE  X(08).                   CI0425
            11            DE10-XCOKEY PICTURE  X(70).                   CI0425
            11            DE10-CUIBR  PICTURE  X(01).                   CI0425
            11            DE10-CUIBA  PICTURE  X(01).                   CI0425
            11            DE10-IPBIK  PICTURE  X(1).                    CI0425
            10            DE10-DU03.                                    CI0425
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            DE10-CMSSF  PICTURE  XX.                      CI0425
            11            DE10-DU09.                                    CI0425
            12            DE10-CMESA  PICTURE  S9(9)                    CI0425
                          BINARY.                                       CI0425
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0425
                          BINARY.                                       CI0425
            12            DE10-CMESB  PICTURE  S9(9)                    CI0425
                          BINARY.                                       CI0425
            12            DE10-CMSST  PICTURE  S9(9)                    CI0425
                          BINARY.                                       CI0425
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0425
                          BINARY.                                       CI0425
            12            DE10-QELLAA PICTURE  S9(9)                    CI0425
                          BINARY.                                       CI0425
            12            DE10-TMESS4 PICTURE  X(512).                  CI0425
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
       01                 MS00.                                         CI0425
          05              MS00-SUITE.                                   CI0425
            15       FILLER         PICTURE  X(00542).                  CI0425
       01                 MS03  REDEFINES      MS00.                    CI0425
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            10            MS03-CMSSF  PICTURE  XX.                      CI0425
            10            MS03-DU09.                                    CI0425
            11            MS03-CMESA  PICTURE  S9(9)                    CI0425
                          BINARY.                                       CI0425
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0425
                          BINARY.                                       CI0425
            11            MS03-CMESB  PICTURE  S9(9)                    CI0425
                          BINARY.                                       CI0425
            11            MS03-CMSST  PICTURE  S9(9)                    CI0425
                          BINARY.                                       CI0425
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0425
                          BINARY.                                       CI0425
            11            MS03-QELLAA PICTURE  S9(9)                    CI0425
                          BINARY.                                       CI0425
            11            MS03-TMESS4 PICTURE  X(512).                  CI0425
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0425
            10            MX11-QMSGS  PICTURE  9(03).                   CI0425
            10            MX11-PJ09                                     CI0425
                          OCCURS       025     TIMES.                   CI0425
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0425
                          COMPUTATIONAL-3.                              CI0425
            11            MX11-CMESB  PICTURE  S9(9)                    CI0425
                          BINARY.                                       CI0425
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                QT65
                                QT63
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
      *SET ADDRESS FOR CBTP                                             DOT
           SET ADDRESS OF XA06 TO                                       ADU015
                PCB-CBTP-PTR1.                                          ADU015
      *SET ADDRESS FOR CH1P                                             DOT
           SET ADDRESS OF XB06 TO                                       ADU015
                PCB-CH1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CA1P                                             DOT
           SET ADDRESS OF XC06 TO                                       ADU015
                PCB-CA1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CPRP                                             DOT
           SET ADDRESS OF XD06 TO                                       ADU015
                PCB-CPRP-PTR1.                                          ADU015
      *SET ADDRESS FOR CCRP                                             DOT
           SET ADDRESS OF XE06 TO                                       ADU015
                PCB-CCRP-PTR1.                                          ADU015
      *SET ADDRESS FOR LM1P                                             DOT
           SET ADDRESS OF XG06 TO                                       ADU015
                PCB-LM1P-PTR1.                                          ADU015
      *SET ADDRESS FOR LH1P                                             DOT
           SET ADDRESS OF XH06 TO                                       ADU015
                PCB-LH1P-PTR1.                                          ADU015
      *SET ADDRESS FOR LPDP                                             DOT
           SET ADDRESS OF XI06 TO                                       ADU015
                PCB-LPDP-PTR1.                                          ADU015
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF XJ06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR ARAY                                             DOT
           SET ADDRESS OF XK06 TO                                       ADU015
                PCB-ARAY-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF XL06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF XM06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR AREY                                             DOT
           SET ADDRESS OF XN06 TO                                       ADU015
                PCB-AREY-PTR1.                                          ADU015
      *SET ADDRESS FOR GR1P                                             DOT
           SET ADDRESS OF XO06 TO                                       ADU015
                PCB-GR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR LUVP                                             DOT
           SET ADDRESS OF XP06 TO                                       ADU015
                PCB-LUVP-PTR1.                                          ADU015
      *SET ADDRESS FOR LARP                                             DOT
           SET ADDRESS OF XQ06 TO                                       ADU015
                PCB-LARP-PTR1.                                          ADU015
       F0TSC-FN. EXIT.
       F0T-FN.   EXIT.
      *N01.      NOTE *************************************.            CI0425
      *               *                                   *             CI0425
      *               *INITIALISATIONS                    *             CI0425
      *               *                                   *             CI0425
      *               *************************************.            CI0425
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
      *COMMENT OUT THIS CALL BECAUSE
      *RPCS THAT CALL THIS MODULE ARE
      *ALREADY DB2
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
           MOVE        DT01-UDATE TO WS00-CURRDATE.
       F02CB-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0425
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0425
      *               *                                   *             CI0425
      *               *FIN DE TRAITEMENT                  *             CI0425
      *               *                                   *             CI0425
      *               *************************************.            CI0425
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0425
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N25.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATE INPUT                     *
      *               *                                   *
      *               *************************************.
       F25.           EXIT.                                             lv05
      *N25AC.    NOTE *GET THE PERSON'S AGE IN YEARS      *.
       F25AC.                                                           lv07
           INITIALIZE  WS00-AGE1
      *GU CL01
           MOVE        QT65-CLID TO S-CLU01-CLID
           PERFORM     F94CB THRU F94CB-FN.
                 IF    IK = '1'                                         DOT
      *CL01 NOT FOUND
           MOVE        012012 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F25AC-FN.
                 IF    CL01-CLTYP = 'P'                                 DOT
      *CLIENT IS A PERSON
      *READ CL03
           PERFORM     F94CC THRU F94CC-FN.
                 IF    IK = '1'                                         DOT
      *CL03 NOT FOUND
           MOVE        012161 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F25AC-FN.
                 IF    CL03-CLDOB NOT = ZEROES                          DOT
           MOVE        CL03-CLDOB TO DF34-DTGRGA
           MOVE        WS00-CURRDATE TO DF34-DTGRGB
           MOVE        8 TO DF30-CDTSF
           MOVE        3 TO DF34-CDTUC
           PERFORM     F92DF THRU F92DF-FN
      *SAVE THE AGE IN YEARS
           MOVE        DF34-NDTUN TO WS00-AGE1.
       F25AC-FN. EXIT.
      *N25BB.    NOTE *GROUP THE VALIDATIONS              *.
       F25BB.    IF    QT63-IERRC NOT = 'Y'                             lv07
                 NEXT SENTENCE ELSE GO TO     F25BB-FN.
           MOVE        'N' TO WS00-PARTKEY
           WS00-NOCX12
           WS00-NOBANK
           FUND-VALERR.
      *N25BE.    NOTE *DETERMINE IF CALLER HAS            *.
       F25BE.                                                           lv10
      *SPECIFIED A BANK OR NOT.  ALL
      *THREE FIELDS MUST BE ZERO TO DO
      *A 'NO BANK' CALL TO CI0225. THIS
      *IS IN SUPPORT OF ADD NEW BANK.
                 IF    QT63-GEMDA = 0                                   DOT
                 AND   QT63-NARRS = 0
                 AND   QT63-CLID = ALL ZEROS
           MOVE        'Y' TO WS00-NOBANK.
       F25BE-FN. EXIT.
      *N25CB.    NOTE *VALIDATE MACHINE DATE OF LAST      *.
       F25CB.    IF    WS00-NOBANK = 'N'                                lv10
                 NEXT SENTENCE ELSE GO TO     F25CB-FN.
      *ACTIVITY
           MOVE        QT63-GEMDA TO DD01-XDATG
           PERFORM     F92VA THRU F92VA-FN.
      *N25CE.    NOTE *INVALID MACHINE DATE OF LAST       *.
       F25CE.    IF    DEL-ER NOT = 1                                   lv15
                 NEXT SENTENCE ELSE GO TO     F25CE-FN.
      *ACTIVITY
           MOVE        'Y' TO QT63-IERRC
           MOVE        012632 TO QT63-NMESA
               GO TO     F25BB-FN.
       F25CE-FN. EXIT.
       F25CB-FN. EXIT.
      *N25DB.    NOTE *INVALID SEQUENCE NUMBER            *.
       F25DB.    IF    QT63-NSEQ4B = ZERO                               lv10
                 AND   WS00-NOBANK = 'N'
                 NEXT SENTENCE ELSE GO TO     F25DB-FN.
           MOVE        'Y' TO QT63-IERRC
           MOVE        014173 TO QT63-NMESA
               GO TO     F25BB-FN.
       F25DB-FN. EXIT.
      *N25EB.    NOTE *VALIDATE CURRENT ACCTG DATE        *.
       F25EB.                                                           lv10
      *
           MOVE        QT63-DCACG TO DD01-XDATG
           PERFORM     F92VA THRU F92VA-FN.
      *N25EE.    NOTE *INVALID CURRENT ACCTG DATE         *.
       F25EE.    IF    DEL-ER NOT = 1                                   lv15
                 NEXT SENTENCE ELSE GO TO     F25EE-FN.
      *
           MOVE        'Y' TO QT63-IERRC
           MOVE        012786 TO QT63-NMESA
               GO TO     F25BB-FN.
       F25EE-FN. EXIT.
      *N25EH.    NOTE *VALIDATE ACCOUNTING DATE           *.
       F25EH.                                                           lv15
      *
           MOVE        QT63-DCACG TO WS00-DCACG
           PERFORM     F92NA THRU F92NA-FN.
                 IF    WS00-DNACG = ZEROES                              DOT
           MOVE        'Y' TO QT63-IERRC
           MOVE        012786 TO QT63-NMESA
               GO TO     F25BB-FN.
       F25EH-FN. EXIT.
       F25EB-FN. EXIT.
      *N25FB.    NOTE *INVALID CLIENT ID PASSED           *.
       F25FB.    IF    (QT63-CLID NOT > SPACES                          lv10
                 OR    QT63-CLID NOT NUMERIC)
                 NEXT SENTENCE ELSE GO TO     F25FB-FN.
           MOVE        'Y' TO QT63-IERRC
           MOVE        012462 TO QT63-NMESA
               GO TO     F25BB-FN.
       F25FB-FN. EXIT.
      *N25GB.    NOTE *INVALID ARR SEQUENCE NUMBER        *.
       F25GB.    IF    QT63-NARRS NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F25GB-FN.
           MOVE        'Y' TO QT63-IERRC
           MOVE        012034 TO QT63-NMESA
               GO TO     F25BB-FN.
       F25GB-FN. EXIT.
      *N25HB.    NOTE *INVALID ACCOUNT ID PASSED          *.
       F25HB.    IF    QT63-CTID NOT > SPACES                           lv10
                 OR    QT63-CTID NOT NUMERIC
                 NEXT SENTENCE ELSE GO TO     F25HB-FN.
           MOVE        'Y' TO QT63-IERRC
           MOVE        012004 TO QT63-NMESA
               GO TO     F25BB-FN.
       F25HB-FN. EXIT.
      *N25IB.    NOTE *CHECK ACCOUNT VALUE FIELD          *.
       F25IB.                                                           lv10
      *EXPECT NUMERIC INPUT
                 IF    QT63-AACTV NOT NUMERIC                           DOT
           MOVE        'Y' TO QT63-IERRC
           MOVE        012253 TO QT63-NMESA
               GO TO     F25BB-FN.
                 IF    QT63-AACTV = ZERO                                DOT
                 AND   QT63-CTIDA = 002
           MOVE        'Y' TO FUND-VALERR.
       F25IB-FN. EXIT.
      *N25JB.    NOTE *CHECK CUSTODIAL CODE FIELD         *.
       F25JB.                                                           lv10
      *EXPECT NUMERIC INPUT
                 IF    QT63-CTCUS NOT NUMERIC                           DOT
           MOVE        'Y' TO QT63-IERRC
           MOVE        014242 TO QT63-NMESA
               GO TO     F25BB-FN.
       F25JB-FN. EXIT.
      *N25KB.    NOTE *CHECK OWNERSHIP CODE FIELD         *.
       F25KB.                                                           lv10
      *EXPECT NUMERIC INPUT
                 IF    QT63-CTOWN NOT NUMERIC                           DOT
           MOVE        'Y' TO QT63-IERRC
           MOVE        014243 TO QT63-NMESA
               GO TO     F25BB-FN.
       F25KB-FN. EXIT.
      *N25LB.    NOTE *CX12 KEYS PASSED ?                 *.
       F25LB.    IF    (QT63-NAPDS NUMERIC                              lv10
                 AND   QT63-NAPDS > ZERO)
                 OR    (QT63-CPMTC NUMERIC
                 AND   QT63-CPMTC > ZERO)
                 OR    (QT63-GESTD NUMERIC
                 AND   QT63-GESTD > ZERO)
                 NEXT SENTENCE ELSE GO TO     F25LB-FN.
      *THEN EDIT FOR VALID COMBO
           MOVE        'Y' TO WS00-PARTKEY
           MOVE        'N' TO WS00-NOCX12.
      *N25LE.    NOTE *THIS FIELD MUST BE PASSED          *.
       F25LE.    IF    QT63-GESTD NOT > ZERO                            lv15
                 NEXT SENTENCE ELSE GO TO     F25LE-FN.
      *ALLOW SHELLS TO PASS
                 IF    QT63-NAPDS = 99                                  DOT
                 AND   QT63-DNPMT = 0
                 AND   QT63-APMTL = 0
               GO TO     F25LE-FN.
           MOVE        'Y' TO QT63-IERRC                                DOT
           MOVE        012537 TO QT63-NMESA
               GO TO     F25BB-FN.
       F25LE-FN. EXIT.
      *N25LH.    NOTE *NOT AN EXISTING CX12               *.
       F25LH.    IF    QT63-NAPDS NOT > ZERO                            lv15
                 NEXT SENTENCE ELSE GO TO     F25LH-FN.
           MOVE        'Y' TO WS00-NOCX12.
       F25LH-FN. EXIT.
      *N25LK.    NOTE *VALIDATE TYPE                      *.
       F25LK.    IF    QT63-CPMTC > 01                                  lv15
                 NEXT SENTENCE ELSE GO TO     F25LK-FN.
           MOVE        'Y' TO QT63-IERRC
           MOVE        012057 TO QT63-NMESA
               GO TO     F25BB-FN.
       F25LK-FN. EXIT.
      *N25LM.    NOTE *INVALID VALUE FOR PAYMENT SEQ #    *.
       F25LM.    IF    QT63-NAPDS NOT NUMERIC                           lv15
                 NEXT SENTENCE ELSE GO TO     F25LM-FN.
           MOVE        'Y' TO QT63-IERRC
           MOVE        012036 TO QT63-NMESA
               GO TO     F25BB-FN.
       F25LM-FN. EXIT.
      *N25LO.    NOTE *VALIDATE START DATE                *.
       F25LO.    IF    QT63-GESTD NOT = '19000000'                      lv15
                 NEXT SENTENCE ELSE GO TO     F25LO-FN.
      *ALLOW FOR JUNK IN PRODUCTION
      *THIS IS A SPECIAL DATE
      *CHECK ONLY RANGES
           MOVE        QT63-GESTD TO DD01-XDATG
      *ALLOW SHELLS TO PASS
                 IF    QT63-NAPDS = 99                                  DOT
                 AND   QT63-DNPMT = 0
                 AND   QT63-APMTL = 0
               GO TO     F25LO-FN.
      *N25LQ.    NOTE *INVALID START DATE CENTURY         *.
       F25LQ.    IF    DD01-XDAT1 NOT > 18                              lv20
                 NEXT SENTENCE ELSE GO TO     F25LQ-FN.
      *
           MOVE        'Y' TO QT63-IERRC
           MOVE        012537 TO QT63-NMESA
               GO TO     F25BB-FN.
       F25LQ-FN. EXIT.
      *N25LT.    NOTE *INVALID START DATE MONTH           *.
       F25LT.    IF    DD01-XDAT3 NOT > 00                              lv20
                 OR    DD01-XDAT3 > 12
                 NEXT SENTENCE ELSE GO TO     F25LT-FN.
           MOVE        'Y' TO QT63-IERRC
           MOVE        012537 TO QT63-NMESA
               GO TO     F25BB-FN.
       F25LT-FN. EXIT.
      *N25LW.    NOTE *INVALID START DATE DAY             *.
       F25LW.    IF    DD01-XDAT4 NOT > 00                              lv20
                 OR    DD01-XDAT4 > 31
                 NEXT SENTENCE ELSE GO TO     F25LW-FN.
           MOVE        'Y' TO QT63-IERRC
           MOVE        012537 TO QT63-NMESA
               GO TO     F25BB-FN.
       F25LW-FN. EXIT.
       F25LO-FN. EXIT.
       F25LB-900. GO TO F25LZ-FN.
       F25LB-FN. EXIT.
      *N25LZ.    NOTE *NO CX12 WAS PASSED                 *.
       F25LZ.                                                           lv10
           MOVE        'Y' TO WS00-NOCX12
           MOVE        'N' TO WS00-PARTKEY.
       F25LZ-FN. EXIT.
       F25BB-FN. EXIT.
      *N25ZB.    NOTE *VALIDATION ERROR ? CATCH IT HERE   *.
       F25ZB.    IF    QT63-IERRC = 'Y'                                 lv07
                 NEXT SENTENCE ELSE GO TO     F25ZB-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        QT63-NMESA TO MS03-NMESS2                        ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F25ZB-FN. EXIT.
       F25-FN.   EXIT.
      *N26.      NOTE *************************************.
      *               *                                   *
      *               *POST VALIDATION PROCESSING         *
      *               *                                   *
      *               *************************************.
       F26.           EXIT.                                             lv05
      *N26BF.    NOTE *CALL CI0020 - CAMS ACCTG DATE      *.            AM0020
       F26BF.                                                           lv10
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
       F26BF-FN. EXIT.
      *N26CA.    NOTE *CALL LATE TRADING MODULE           *.
       F26CA.                                                           lv10
           INITIALIZE  I93B
           MOVE        NS20-DCACD TO I93B-DACTT
           MOVE        'O' TO I93B-CEADC
           PERFORM     F99BB THRU F99BB-FN.
       F26CA-FN. EXIT.
      *N26CB.    NOTE *GET EARLIEST AND LATEST DATES      *.
       F26CB.    IF    WS00-FIRST = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F26CB-FN.
           MOVE        QT63-DCACG TO WS00-DNACG
           MOVE        'N' TO WS00-FIRST.
      *N26CE.    NOTE *CALLS TO GET NEXT ACCTG DATE       *.
       F26CE.                                                           lv15
           MOVE        1                        TO J26CER
                                    GO TO     F26CE-B.
       F26CE-A.
           ADD         1                        TO J26CER.
       F26CE-B.
           IF          J26CER                   >  4
                                    GO TO     F26CE-FN.
      *FOUR DAYS OUT
           MOVE        WS00-DNACG TO WS00-DCACG
           PERFORM     F92NA THRU F92NA-FN.
      *N26CG.    NOTE *SAVE CURRENT ACCOUNTING DATE       *.
       F26CG.    IF    J26CER = 1                                       lv20
                 NEXT SENTENCE ELSE GO TO     F26CG-FN.
      *AND FIGURE OD START DATE
      *BASED ON TIME OF DAY AND
      *WHETHER TODAY IS AN ACCOUNTING
      *DAY
           MOVE        WS00-DCACG TO WS00-DCACG1.
                 IF    WS00-EIBTIME > WS-GETIM6                         DOT
                 OR    WS00-CURRDATE > WS00-DCACG
           MOVE        WS00-DNACG TO WS00-GESTD1
                 ELSE
           MOVE        WS00-DCACG TO WS00-GESTD1.
       F26CG-FN. EXIT.
       F26CE-900. GO TO F26CE-A.
       F26CE-FN. EXIT.
      *N26CH.    NOTE *FIGURE RECURRING START DATE        *.
       F26CH.                                                           lv15
                 IF    WS00-HH < 16                                     DOT
           MOVE        WS00-DCACG TO WS00-GESTE
                 ELSE
           MOVE        WS00-DNACG TO WS00-GESTE.
       F26CH-FN. EXIT.
      *N26CK.    NOTE *FIGURE END DATE                    *.
       F26CK.                                                           lv15
           MOVE        QT63-DCACG TO WS00-CCYYMMDD
           ADD         10 TO WS00-MM.
                 IF    WS00-MM > 12                                     DOT
           SUBTRACT    12 FROM WS00-MM
           ADD         1 TO WS00-CCYY.
           MOVE        WS00-CCYYMMDD TO WS00-GEENL.                     DOT
       F26CK-FN. EXIT.
       F26CB-FN. EXIT.
       F26-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATE EXISTENCE OF SEGS         *
      *               *                                   *
      *               *************************************.
       F30.           EXIT.                                             lv05
      *N30BB.    NOTE *GROUP THE EDITS                    *.
       F30BB.         EXIT.                                             lv07
      *N30CB.    NOTE *CLIENT DB CHECKS CX01 KEY CHECK    *.
       F30CB.    IF    WS00-NOBANK = 'N'                                lv10
                 NEXT SENTENCE ELSE GO TO     F30CB-FN.
           MOVE        QT63-CLID TO S-CLU01-CLID
           PERFORM     F94CB THRU F94CB-FN.
      *N30CE.    NOTE *CLIENT NOT FOUND                   *.
       F30CE.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F30CE-FN.
           MOVE        012740 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F30BB-FN.
       F30CE-FN. EXIT.
       F30CB-FN. EXIT.
      *N30DB.    NOTE *ACCOUNT DB CHECKS (GU CT01)        *.
       F30DB.                                                           lv10
           MOVE        QT63-CTID TO S-CTU01-CTID
           PERFORM     F94BB THRU F94BB-FN.
      *N30DE.    NOTE *CT01 NOT FOUND                     *.
       F30DE.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F30DE-FN.
           MOVE        012234 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F30BB-FN.
       F30DE-FN. EXIT.
      *N30DH.    NOTE *MOVE TO 88-LEVEL WORK AREAS        *.
       F30DH.                                                           lv15
           MOVE        QT63-CTID TO W-WORK-CTID
           MOVE        CT01-PRCOD TO 7-PRCOD
           WS00-PRCOD
      *GET LIFE PRODUCT TYPE
           INITIALIZE  TC8A.
                 IF    CT01-CTIDA = 004                                 DOT
                 OR    CT01-CTIDA = 005
           MOVE        CT01-CTIDA TO TC8A-CTIDA
           MOVE        CT01-PRCOD TO TC8A-PRCOD
           PERFORM     F92TC THRU F92TC-FN.
       F30DH-FN. EXIT.
       F30DB-FN. EXIT.
      *N30EB.    NOTE *>>> ACCOUNT OWNERSHIP CHECKS <<<   *.
       F30EB.                                                           lv10
      *>>>       (GN CT49)          <<<
           PERFORM     F94FC THRU F94FC-FN.
      *N30EE.    NOTE *CT49 NOT FOUND                     *.
       F30EE.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F30EE-FN.
           MOVE        012030 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F30BB-FN.
       F30EE-FN. EXIT.
       F30EB-FN. EXIT.
      *N30FB.    NOTE *ARRANGEMENT DB CHECKS (GU CX01)    *.
       F30FB.    IF    WS00-NOBANK = 'N'                                lv10
                 NEXT SENTENCE ELSE GO TO     F30FB-FN.
           MOVE        QT63-CLID TO S-CXU01-CLID
           PERFORM     F94DB THRU F94DB-FN.
      *N30FE.    NOTE *CX01 NOT FOUND                     *.
       F30FE.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F30FE-FN.
           MOVE        012006 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F30BB-FN.
       F30FE-FN. EXIT.
      *N30FH.    NOTE *PERTINENT DATA CHANGED ?           *.
       F30FH.    IF    CX01-GEMDA NOT = QT63-GEMDA                      lv15
                 OR    CX01-NSEQ4B NOT =
                       QT63-NSEQ4B
                 NEXT SENTENCE ELSE GO TO     F30FH-FN.
           MOVE        012758 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F30BB-FN.
       F30FH-FN. EXIT.
       F30FB-FN. EXIT.
      *N30GB.    NOTE *CX03 EXISTENCE CHECK (GU CX03)     *.
       F30GB.    IF    WS00-NOBANK = 'N'                                lv10
                 NEXT SENTENCE ELSE GO TO     F30GB-FN.
           MOVE        01 TO S-CXU03-CARTY
           MOVE        QT63-NARRS TO S-CXU03-NARRS
           PERFORM     F94DC THRU F94DC-FN.
      *N30GE.    NOTE *CX03 NOT FOUND                     *.
       F30GE.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F30GE-FN.
           MOVE        012007 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F30BB-FN.
       F30GE-900. GO TO F30GH-FN.
       F30GE-FN. EXIT.
      *N30GH.    NOTE *SAVE CX03 INFO FOR LATER           *.
       F30GH.                                                           lv15
           MOVE        CX03-CARST TO WS00-CARST
           INITIALIZE  BANK-FIELDS.
       F30GH-FN. EXIT.
       F30GB-FN. EXIT.
      *N30HB.    NOTE *CX18 EXISTENCE CHECK (GU CX18)     *.
       F30HB.    IF    WS00-NOBANK = 'N'                                lv10
                 NEXT SENTENCE ELSE GO TO     F30HB-FN.
           MOVE        CX03-NBASQ TO S-CXU18-NBASQ
           PERFORM     F94DL THRU F94DL-FN.
      *N30HE.    NOTE *BANK ACCOUNT INFO NOT FOUND        *.
       F30HE.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F30HE-FN.
           MOVE        012027 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F30BB-FN.
       F30HE-FN. EXIT.
       F30HB-FN. EXIT.
      *N30IB.    NOTE *CHECK BANK EXISTS ON CLIENT DB     *.
       F30IB.    IF    WS00-NOBANK = 'N'                                lv10
                 NEXT SENTENCE ELSE GO TO     F30IB-FN.
           MOVE        CX18-CLID TO S-CLU01-CLID
           PERFORM     F94CB THRU F94CB-FN.
      *N30IE.    NOTE *BANK NOT FOUND                     *.
       F30IE.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F30IE-FN.
           MOVE        012612 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F30BB-FN.
       F30IE-FN. EXIT.
      *N30IH.    NOTE *CLIENT FOUND IS NOT AN ORG (BNK)   *.
       F30IH.    IF    CL01-CLTYP NOT = 'O'                             lv15
                 NEXT SENTENCE ELSE GO TO     F30IH-FN.
           MOVE        012016 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F30BB-FN.
       F30IH-FN. EXIT.
       F30IB-FN. EXIT.
       F30BB-FN. EXIT.
      *N30ZB.    NOTE *ERROR IN THIS FUNC ? TERMINATE     *.
       F30ZB.    IF    QT63-IERRC = 'Y'                                 lv07
                 NEXT SENTENCE ELSE GO TO     F30ZB-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        QT63-NMESA TO MS03-NMESS2                        ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30ZB-FN. EXIT.
       F30-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *GET ACCOUNT SPECIFIC INFO          *
      *               *                                   *
      *               *************************************.
       F35.      IF    QT63-CTID NOT =                                  lv05
                       WS00-SAVE-CTID
                 NEXT SENTENCE ELSE GO TO     F35-FN.
      *ONCE PER ACCOUNT
           MOVE        QT63-CTID TO WS00-SAVE-CTID
           INITIALIZE  CLIENT-SUITABILITY
           INITIALIZE  ACCOUNT-SUITABILITY
           INITIALIZE  ACCT-FIELDS
           INITIALIZE  ACCT-TABLE
           INITIALIZE  BAPD-BA-TABLE
           INITIALIZE  BAOD-99-TABLE
           INITIALIZE  WS00-LREACT
           INITIALIZE  CLIENT-MINOR.
      *N35BB.    NOTE *GROUP THE ACCESSES                 *.
       F35BB.         EXIT.                                             lv07
      *N35CB.    NOTE *PACTABLE READS                     *.
       F35CB.         EXIT.                                             lv10
      *N35CE.    NOTE *GET THE PRODUCT CODE NAME          *.
       F35CE.                                                           lv15
           MOVE        CT01-CTIDA TO TA5A-CTIDA
           MOVE        CT01-PRCOD TO TA5A-PRCOD
           MOVE        SPACES TO TA5A-PRSCD
      *--- FOR FUNDS ONLY
                 IF    CT01-CTIDA = 002                                 DOT
           MOVE        CT01-PRSCD TO TA5A-PRSCD.
      *--- ENDIF                                                        DOT
           PERFORM     F92TA THRU F92TA-FN.
       F35CE-FN. EXIT.
      *N35CH.    NOTE *LOAD THE TA5A FIELDS FOR 88 LVLS   *.
       F35CH.                                                           lv15
           MOVE        TA5A-CPRDG TO 7-CPRDG
           MOVE        TA5A-CPRDA1 TO 7-CPRDA1.
                 IF    7-CERTIFICATE-PRODUCT                            DOT
           MOVE        01 TO WS00-CPCCDE
               GO TO     F35CH-FN.
                 IF    7-MUTUAL-FUND-PRODUCT                            DOT
           MOVE        02 TO WS00-CPCCDE
               GO TO     F35CH-FN.
                 IF    7-ANNUITY-GROUP                                  DOT
           MOVE        03 TO WS00-CPCCDE
               GO TO     F35CH-FN.
                 IF    7-LIFE-INSURANCE-GROUP                           DOT
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
           MOVE        04 TO WS00-CPCCDE
               GO TO     F35CH-FN.
      *ALL OTHERS GET THIS                                              DOT
           MOVE        99 TO WS00-CPCCDE.
       F35CH-FN. EXIT.
      *N35CI.    NOTE *INVALID VALUE FOR PAYMENT SEQ #    *.
       F35CI.    IF    QT63-NAPDS NOT NUMERIC                           lv15
                 OR    QT63-NAPDS < 0
                 NEXT SENTENCE ELSE GO TO     F35CI-FN.
      *WHEN NOT BROKERAGE ACCOUNT
      *ZERO IS PASSED WHEN THE KEY IS
      *NOT KNOWN EG) WHEN ADDING
           MOVE        'Y' TO QT63-IERRC
           MOVE        012036 TO QT63-NMESA
               GO TO     F35BB-FN.
       F35CI-FN. EXIT.
      *N35CK.    NOTE *GET THE PRODUCT CODE MISC ATTR     *.
       F35CK.                                                           lv15
           MOVE        CT01-CTIDA TO TB5B-CTIDA
           MOVE        CT01-PRCOD TO TB5B-PRCOD
           MOVE        SPACES TO TB5B-PRSCD
      *--- FOR FUNDS ONLY
                 IF    CT01-CTIDA = 002                                 DOT
           MOVE        CT01-PRSCD TO TB5B-PRSCD.
      *--- ENDIF                                                        DOT
           PERFORM     F92TB THRU F92TB-FN.
       F35CK-FN. EXIT.
      *N35CN.    NOTE *GET THE TA98 SEG. PROD BA RULES    *.
       F35CN.                                                           lv15
           MOVE        CT01-CTIDA TO TD98-CTIDA
           MOVE        CT01-PRCOD TO TD98-PRCOD
           MOVE        01 TO TD98-CARTY
           PERFORM     F92TD THRU F92TD-FN.
       F35CN-FN. EXIT.
       F35CB-FN. EXIT.
      *N35DB.    NOTE *CALLED PROGRAMS                    *.
       F35DB.                                                           lv10
                 IF    W-WORK-PREFIX = 9990                             DOT
           MOVE        'Y' TO ACCT-INVALID.
      *N35EB.    NOTE *CALL CI0003 ACCOUNT OWNERSHIP      *.
       F35EB.                                                           lv15
           PERFORM     F97BB THRU F97BB-FN
      *CHECK RETURN FROM CALLED PROGRAM
           MOVE        CI0003 TO WS00-PROGR
           PERFORM     F97ZB THRU F97ZB-FN.
       F35EB-FN. EXIT.
      *N35FB.    NOTE *CALL CI0004 ACCOUNT ADDRESS        *.
       F35FB.                                                           lv15
           PERFORM     F97CB THRU F97CB-FN
      *CHECK RETURN FROM CALLED PROGRAM
           MOVE        CI0004 TO WS00-PROGR
           PERFORM     F97ZB THRU F97ZB-FN.
       F35FB-FN. EXIT.
      *N35GB.    NOTE *CALL CI0083 OWNERSHIP SCRUBBER     *.
       F35GB.                                                           lv15
           PERFORM     F97GB THRU F97GB-FN
      *CHECK RETURN FROM CALLED PROGRAM
           MOVE        CI0083 TO WS00-PROGR
           PERFORM     F97ZB THRU F97ZB-FN.
       F35GB-FN. EXIT.
      *N35HB.    NOTE *CALL CI0018 ACCOUNT CLIENTS        *.
       F35HB.                                                           lv15
           PERFORM     F97DB THRU F97DB-FN
      *CHECK RETURN FROM CALLED PROGRAM
           MOVE        CI0018 TO WS00-PROGR
           PERFORM     F97ZB THRU F97ZB-FN
      *GET ALL RELATED CLIENTS
      *TO SEE IF ANY CLIENTS ARE DEAD
      *OR IF CLIENT OWNER OR TAXPAYER
      *IS AN ORGANIZATION
           MOVE        1 TO I1
           MOVE        ZERO TO I2
           QT65-QITEM
           MOVE        ZERO TO WS00-CLID
           WS00-CLCTRC
           WS00-TAXPAYER-CLID
           MOVE        'N' TO QT65-IORGC.
      *N35HE.    NOTE *GO THROUGH ALL CLIENTS             *.
       F35HE.    IF    I1 NOT > PC14-QITEM                              lv20
                 NEXT SENTENCE ELSE GO TO     F35HE-FN.
      *GET THE FIRST OWNER CLIENT ID                                    DOT
                 IF    PC14-CLCTRC (I1) = 001                           DOT
                 AND   WS00-CLCTRC = ZEROES
           MOVE        PC14-CLID (I1) TO WS00-CLID
           MOVE        PC14-CLCTRC (I1) TO WS00-CLCTRC.
      *SUPERCEDE WITH TAX PAYER CLID                                    DOT
                 IF    PC14-CLCTRC (I1) = 004                           DOT
           MOVE        PC14-CLID (I1) TO WS00-CLID
           WS00-TAXPAYER-CLID
           MOVE        PC14-CLCTRC (I1) TO WS00-CLCTRC.
           MOVE        PC14-CLID (I1) TO S-CLU01-CL01K                  DOT
           ADD         1 TO I1
           ADD         1 TO I2.
      *N35HH.    NOTE *READ CL01 (& CL03 IF PERSON)       *.
       F35HH.                                                           lv25
      *INITIALIZE DEATH WORK FIELDS
      *(USED IN F42HB)
           MOVE        ZEROES TO WS00-CLDOD
           MOVE        SPACE TO WS00-CLDTH
      *GU CL01
           PERFORM     F94CB THRU F94CB-FN.
                 IF    IK = '1'                                         DOT
      *CL01 NOT FOUND
           MOVE        012012 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F35BB-FN.
                 IF    PC14-CLCTRC (I2)                                 DOT
                       = (001 OR 004 OR 007)
                 AND   CL01-CLTYP = 'O'
                 AND   QT65-IORGC = 'N'
      *FOR OWNER OR TAXPAYER
      *ORGANIZATION CLIENT
           MOVE        'Y' TO QT65-IORGC.
                 IF    PC14-CLCTRC (I2)                                 DOT
                       = (001 OR 007)
                 AND   CL01-CLTYP = 'P'
      *COUNT THE NUMBER OF OWNERS
           ADD         1 TO QT65-QITEM.
                 IF    WS00-CLCTRC = 004                                DOT
           MOVE        CL01-CLTYP TO WS00-TAXPAYER-CLTYP.
                 IF    CL01-CLTYP NOT = 'P'                             DOT
      *CLIENT IS NOT A PERSON
               GO TO     F35HE-900.
      *READ CL03                                                        DOT
           PERFORM     F94CC THRU F94CC-FN.
                 IF    IK = '1'                                         DOT
      *CL03 NOT FOUND
           MOVE        012161 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F35BB-FN.
                 IF    WS00-CLCTRC = 004                                DOT
      *CLIENT IS TAXPAYER
      *SAVE DEATH DATE & INDICATOR
           MOVE        CL03-CLDOD TO WS00-CLDOD
           MOVE        CL03-CLDTH TO WS00-CLDTH.
      *N35HK.    NOTE *GET THE PERSON'S AGE IN YEARS      *.
       F35HK.    IF    WS00-CLID = S-CLU01-CLID                         lv30
                 NEXT SENTENCE ELSE GO TO     F35HK-FN.
           MOVE        CL03-CLDOB TO DF34-DTGRGA
           MOVE        WS00-CURRDATE TO DF34-DTGRGB
           MOVE        8 TO DF30-CDTSF
           MOVE        3 TO DF34-CDTUC
           PERFORM     F92DF THRU F92DF-FN
      *SAVE THE AGE IN YEARS
           MOVE        DF34-NDTUN TO WS00-AGE.
       F35HK-FN. EXIT.
      *N35HN.    NOTE *SOME ONE DEAD IS RELATED TO ACCT   *.
       F35HN.                                                           lv30
                 IF    CL03-CLDOD > ZERO                                DOT
                 OR    CL03-CLDTH = 'Y'
           MOVE        'Y' TO ACCT-DEAD.
       F35HN-FN. EXIT.
       F35HH-FN. EXIT.
       F35HE-900. GO TO F35HE.
       F35HE-FN. EXIT.
      *N35HO.    NOTE *HAS ORGANIZATION CLIENT            *.
       F35HO.    IF    QT65-IORGC = 'Y'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F35HO-FN.
           MOVE        ZERO TO QT65-QITEM.
       F35HO-FN. EXIT.
      *N35HY.    NOTE *THIS IS BAD. NO OWNER FOR ACCT     *.
       F35HY.    IF    WS00-CLCTRC = ZERO                               lv20
                 NEXT SENTENCE ELSE GO TO     F35HY-FN.
                 IF    IK = '1'                                         DOT
           MOVE        012792 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F35BB-FN.
       F35HY-FN. EXIT.
       F35HB-FN. EXIT.
      *N35IB.    NOTE *CALL CI0019 ACCOUNT GROUPS         *.
       F35IB.                                                           lv15
      *TO FIND IF THE GROUP IS PENSION
           PERFORM     F97EB THRU F97EB-FN
      *CHECK RETURN FROM CALLED PROGRAM
           MOVE        CI0019 TO WS00-PROGR
           PERFORM     F97ZB THRU F97ZB-FN
           MOVE        PD15-GR01 (1) TO GR01
           MOVE        PD15-GR07 (1) TO GR07
           MOVE        GR01-GRIDC TO WS00-GRIDC.
       F35IB-FN. EXIT.
      *N35JB.    NOTE *CALL CI0135 TO GET CERT INFO       *.
       F35JB.    IF    CT01-CTIDA = 001                                 lv15
                 AND   TA5A-CPRDA1 = (107 OR 108)
                 NEXT SENTENCE ELSE GO TO     F35JB-FN.
           PERFORM     F97IB THRU F97IB-FN
      *CHECK RETURN FROM CALLED PROGRAM
           MOVE        CI0135 TO WS00-PROGR
           PERFORM     F97ZB THRU F97ZB-FN.
       F35JB-FN. EXIT.
      *N35KB.    NOTE *GET INSURANCE/ANNUITY INFO         *.
       F35KB.    IF    CT01-CTIDA = (004 OR 005)                        lv15
                 NEXT SENTENCE ELSE GO TO     F35KB-FN.
      *N35KE.    NOTE *CALL CI0141 FOR VANTAGE            *.
       F35KE.    IF    TB5B-IVANT = 'Y'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F35KE-FN.
           PERFORM     F97KB THRU F97KB-FN.
                 IF    WF41-CVSTC NOT = SPACES                          DOT
           MOVE        'Y' TO ACCT-VALERR.
      *CHECK RETURN FROM CALLED PROGRAM                                 DOT
           MOVE        CI0141 TO WS00-PROGR
           PERFORM     F97ZB THRU F97ZB-FN.
       F35KE-FN. EXIT.
      *N35KH.    NOTE *CALL CI0140 FOR NON-VANTAGE        *.
       F35KH.    IF    (TB5B-IVANT = 'N'                                lv20
                 AND   (TB5B-CVSYS NOT = '21'
                 AND   TB5B-CVSYS NOT = '22'))
                 NEXT SENTENCE ELSE GO TO     F35KH-FN.
      *AND NON-RPS ACCOUNT ONLY
           PERFORM     F97JB THRU F97JB-FN
      *CHECK RETURN FROM CALLED PROGRAM
           MOVE        CI0140 TO WS00-PROGR
           PERFORM     F97ZB THRU F97ZB-FN.
       F35KH-FN. EXIT.
       F35KB-FN. EXIT.
      *N35MB.    NOTE *CALL CI0124 FOR ANNT HOLD CODES    *.
       F35MB.    IF    ANNUITY                                          lv15
                 NEXT SENTENCE ELSE GO TO     F35MB-FN.
      *
           PERFORM     F97HB THRU F97HB-FN
      *CHECK RETURN FROM CALLED PROGRAM
           MOVE        CI0124 TO WS00-PROGR
           PERFORM     F97ZB THRU F97ZB-FN.
       F35MB-FN. EXIT.
       F35DB-FN. EXIT.
      *N35NB.    NOTE *SETUP CX2Y LOOP                    *.
       F35NB.                                                           lv10
      *SET UP TO PROCESS THE ARR DB VIA
      *SECONDARY INDEX
           MOVE        QT63-CTID TO S-CXU2Y-CTID
           S-CXU06-CTID
           CX06-CTID
           MOVE        ZEROES TO S-CXU2Y-CLID
           S-CXU2Y-CARTY
           S-CXU2Y-NARRS
           MOVE        '>=' TO S-CXU2Y-OPER
      *GU >= ON CX2Y
           PERFORM     F94AA THRU F94AA-FN.
      *N35ND.    NOTE *PROCESS ARRANGEMENTS MATCHING      *.
       F35ND.    IF    IK = '0'                                         lv15
                 AND   CX2Y-CTID = QT63-CTID
                 NEXT SENTENCE ELSE GO TO     F35ND-FN.
      *ON THE ACCOUNT NUMBER
      *N35NF.    NOTE *PROCESS ONLY BANK AUTH             *.
       F35NF.    IF    (CX2Y-CARTY = 01                                 lv20
                 OR    CX2Y-CARTY = 10)
                 NEXT SENTENCE ELSE GO TO     F35NF-FN.
      *SPO, AD, AP, DI, IP ETC.
      *GU CX03
           MOVE        CX2Y-CLID TO S-CXU01-CX01K
           MOVE        CX2Y-CARTY TO S-CXU03-CARTY
           MOVE        CX2Y-NARRS TO S-CXU03-NARRS
           PERFORM     F94DC THRU F94DC-FN.
                 IF    IK = '1'                                         DOT
      *CX03 NOT FOUND; RETURN ERROR
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012007 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N35OB.    NOTE *PROCESS ARRANGEMENTS BY CARTY      *.
       F35OB.    IF    CX2Y-CARTY = 01                                  lv25
                 NEXT SENTENCE ELSE GO TO     F35OB-FN.
      *N35OD.    NOTE *GET FIRST CX12 SEGMENT GN CX12     *.
       F35OD.                                                           lv30
           INITIALIZE  CX12
           PERFORM     F94DG THRU F94DG-FN.
                 IF    IK = '1'                                         DOT
      *NO DETAIL FOUND; RETURN ERROR
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012675 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35OD-FN. EXIT.
      *N35OF.    NOTE *LOOP THRU ALL CX12 SEGMENTS        *.
       F35OF.    IF    IK = '0'                                         lv30
                 NEXT SENTENCE ELSE GO TO     F35OF-FN.
      *N35OH.    NOTE *COLLECT STATS ON THE SEGMENT       *.
       F35OH.         EXIT.                                             lv35
      *N35OI.    NOTE *RECURRING COUNTS -  "ALL BANKS"    *.
       F35OI.    IF    CX12-NAPDS NOT = 99                              lv40
                 NEXT SENTENCE ELSE GO TO     F35OI-FN.
      *USED TO ENFORCE BUSINESS RULES
      *THAT SPAN BANKS.
      *
      *TOTAL - ENFORCES 97 MAX
           ADD         1 TO ACCT-BA-COUNT
      *ACTIVE LOANS
                 IF    CX12-CPMTC = 01                                  DOT
                 AND   CX12-CDEST = 01
           ADD         1 TO ACCT-BA-LCOUNT.
      *ACTIVE REGULARS                                                  DOT
                 IF    CX12-CDEST = 01                                  DOT
                 AND   CX12-CPMTC = 00
           ADD         1 TO ACCT-BA-ACOUNT.
      *UNUSED COUNTS                                                    DOT
                 IF    CX12-CPMTC = 00                                  DOT
           ADD         1 TO ACCT-BA-RCOUNT.
                 IF    CX12-CPMTC = 02                                  DOT
           ADD         1 TO ACCT-BA-CCOUNT.
                 IF    CX12-CPMTC = 03                                  DOT
           ADD         1 TO ACCT-BA-TCOUNT.
                 IF    CX12-CDEST = 02                                  DOT
           ADD         1 TO ACCT-BA-PCOUNT.
                 IF    CX12-CDEST = 03                                  DOT
           ADD         1 TO ACCT-BA-ICOUNT.
                 IF    CX12-CDEST = 04                                  DOT
           ADD         1 TO ACCT-BA-FCOUNT.
      *N35OJ.    NOTE *RECURRING DETAILS  "ALL BANKS"     *.
       F35OJ.                                                           lv45
      *USED TO ENFORCE RULES FOR
      *PRODUCTS THAT LIMIT NUMBER OF
      *ARRANGEMENTS.
      *********************************
      **** INCREMENT THE COUNTER ***
           ADD         1 TO IACCTR
           MOVE        IACCTR TO IACCTL
      **** FLAGS ARE SET FOR EACH
      *ARRANGEMENT. **************
                 IF    CX12-CPMTC = 00                                  DOT
           ADD         1 TO ACCT-RCOUNT (IACCTR).
                 IF    CX12-CPMTC = 01                                  DOT
           ADD         1 TO ACCT-LCOUNT (IACCTR).
                 IF    CX12-CDEST = 01                                  DOT
           ADD         1 TO ACCT-ACOUNT (IACCTR).
                 IF    CX12-CDEST = 04                                  DOT
           ADD         1 TO ACCT-FCOUNT (IACCTR).
      ***** UNUSED COUNTS ********                                      DOT
           ADD         1 TO ACCT-COUNT (IACCTR).
                 IF    CX12-CPMTC = 02                                  DOT
           ADD         1 TO ACCT-CCOUNT (IACCTR).
                 IF    CX12-CPMTC = 03                                  DOT
           ADD         1 TO ACCT-TCOUNT (IACCTR).
                 IF    CX12-CDEST = 02                                  DOT
           ADD         1 TO ACCT-PCOUNT (IACCTR).
                 IF    CX12-CDEST = 03                                  DOT
           ADD         1 TO ACCT-ICOUNT (IACCTR).
       F35OJ-FN. EXIT.
      *N35OK.    NOTE *RECURRING DETAILS "THIS BANK"      *.
       F35OK.    IF    QT63-NARRS = CX03-NARRS                          lv45
                 NEXT SENTENCE ELSE GO TO     F35OK-FN.
      *USED TO GET NEXT ARRANGEMENT SEQ
           ADD         1 TO IBAPDR
           MOVE        IBAPDR TO IBAPDL
           ADD         1 TO BAPD-BA-COUNT (IBAPDR).
      *UNUSED COUNTS                                                    DOT
                 IF    CX12-CPMTC = 00                                  DOT
           ADD         1 TO BAPD-BA-RCOUNT (IBAPDR).
                 IF    CX12-CPMTC = 01                                  DOT
           ADD         1 TO BAPD-BA-LCOUNT (IBAPDR).
                 IF    CX12-CPMTC = 02                                  DOT
           ADD         1 TO BAPD-BA-CCOUNT (IBAPDR).
                 IF    CX12-CPMTC = 03                                  DOT
           ADD         1 TO BAPD-BA-TCOUNT (IBAPDR).
                 IF    CX12-CDEST = 01                                  DOT
           ADD         1 TO BAPD-BA-ACOUNT (IBAPDR).
                 IF    CX12-CDEST = 02                                  DOT
           ADD         1 TO BAPD-BA-PCOUNT (IBAPDR).
                 IF    CX12-CDEST = 03                                  DOT
           ADD         1 TO BAPD-BA-ICOUNT (IBAPDR).
                 IF    CX12-CDEST = 04                                  DOT
           ADD         1 TO BAPD-BA-FCOUNT (IBAPDR).
       F35OK-FN. EXIT.
       F35OI-900. GO TO F35OL-FN.
       F35OI-FN. EXIT.
      *N35OL.    NOTE *ON-DEMAND COUNTS "ALL BANKS"       *.
       F35OL.                                                           lv40
      *ENFORCES MAX
           ADD         1 TO ACCT-99-COUNT
      *ONLY NON-SHELLS COUNT FOR EDITS
                 IF    CX12-CDEST = 01                                  DOT
                 AND   CX12-APMTL > 0
                 AND   CX12-DNPMT > 0
           ADD         1 TO ACCT-99-ACOUNT.
      *UNUSED COUNTS                                                    DOT
                 IF    CX12-CPMTC = 00                                  DOT
           ADD         1 TO ACCT-99-RCOUNT.
                 IF    CX12-CPMTC = 01                                  DOT
           ADD         1 TO ACCT-99-LCOUNT.
                 IF    CX12-CPMTC = 02                                  DOT
           ADD         1 TO ACCT-99-CCOUNT.
                 IF    CX12-CPMTC = 03                                  DOT
           ADD         1 TO ACCT-99-TCOUNT.
                 IF    CX12-CDEST = 02                                  DOT
           ADD         1 TO ACCT-99-PCOUNT.
                 IF    CX12-CDEST = 03                                  DOT
           ADD         1 TO ACCT-99-ICOUNT.
                 IF    CX12-CDEST = 04                                  DOT
           ADD         1 TO ACCT-99-FCOUNT.
      *N35OM.    NOTE *OD DETAILS FOR "THIS BANK"         *.
       F35OM.    IF    QT63-NARRS = CX03-NARRS                          lv45
                 NEXT SENTENCE ELSE GO TO     F35OM-FN.
           ADD         1 TO IBAODR
           MOVE        IBAODR TO IBAODL.
                 IF    CX12-CPMTC = 00                                  DOT
           ADD         1 TO BAOD-99-RCOUNT (IBAODR).
                 IF    CX12-CPMTC = 01                                  DOT
           ADD         1 TO BAOD-99-LCOUNT (IBAODR).
                 IF    CX12-CPMTC = 02                                  DOT
           ADD         1 TO BAOD-99-CCOUNT (IBAODR).
      *REAL ODS - NON-SHELLS                                            DOT
                 IF    CX12-CDEST = 01                                  DOT
                 AND   CX12-APMTL > 0
                 AND   CX12-DNPMT > 0
           ADD         1 TO BAOD-99-ACOUNT (IBAODR).
                 IF    CX12-CDEST = 03                                  DOT
           ADD         1 TO BAOD-99-ICOUNT (IBAODR).
      *UNUSED COUNTS                                                    DOT
           ADD         1 TO BAOD-99-COUNT (IBAODR).
                 IF    CX12-CPMTC = 03                                  DOT
           ADD         1 TO BAOD-99-TCOUNT (IBAODR).
                 IF    CX12-CDEST = 01                                  DOT
                 AND   CX12-APMTL = 0
                 AND   CX12-DNPMT = 0
           ADD         1 TO BAOD-99-SCOUNT (IBAODR).
                 IF    CX12-CDEST = 02                                  DOT
           ADD         1 TO BAOD-99-PCOUNT (IBAODR).
                 IF    CX12-CDEST = 04                                  DOT
           ADD         1 TO BAOD-99-FCOUNT (IBAODR).
       F35OM-FN. EXIT.
       F35OL-FN. EXIT.
       F35OH-FN. EXIT.
      *N35OY.    NOTE *GET THE NEXT CX12 RECORD           *.
       F35OY.                                                           lv35
           PERFORM     F94DG THRU F94DG-FN.
       F35OY-FN. EXIT.
       F35OF-900. GO TO F35OF.
       F35OF-FN. EXIT.
       F35OB-FN. EXIT.
      *N35PB.    NOTE *PROCESS SPO/DI/IP                  *.
       F35PB.    IF    CX2Y-CARTY = 10                                  lv25
                 NEXT SENTENCE ELSE GO TO     F35PB-FN.
      *N35PD.    NOTE *GET FIRST CX13                     *.
       F35PD.                                                           lv30
           INITIALIZE  CX13
           PERFORM     F94DI THRU F94DI-FN.
      *THE REASON THERE IS NOT AN ERROR                                 DOT
      *PROCESSING HERE ON NOT FOUND IS
      *BECAUSE THE DATABASE IN DEVO
      *HAS BAD DATA
       F35PD-FN. EXIT.
      *N35PF.    NOTE *LOOP THRU ALL CX13 SEGMENTS        *.
       F35PF.    IF    IK = '0'                                         lv30
                 NEXT SENTENCE ELSE GO TO     F35PF-FN.
      *N35PH.    NOTE *LOOK ONLY AT COMPLETED ONES        *.
       F35PH.    IF    CX13-IIARR = 'Y'                                 lv35
                 AND   (CX13-CARTZ = 01
                 OR    CX13-CARTZ = 02
                 OR    CX13-CARTZ = 04
                 OR    CX13-CARTZ = 05
                 OR    CX13-CARTZ = 06
                 OR    CX13-CARTZ = 07
                 OR    CX13-CARTZ = 09)
                 NEXT SENTENCE ELSE GO TO     F35PH-FN.
      *SP SYSTEMATIC PAYOUT
      *CP SYSTEMATIC PARTIAL PAYOUT
      *AP AAPS
      *DI, LP DIVIDEND DISTRIBUTION
      *IP INTEREST PAYOUT
      *AD, ID ANNUITY DISTRIBUTION
      *AI ANNUITY INTEREST
      *N35PJ.    NOTE *GET FIRST CX14                     *.
       F35PJ.                                                           lv40
           INITIALIZE  CX14
           MOVE        CX13-CX13K TO S-CXU13-CX13K
           PERFORM     F94DK THRU F94DK-FN.
                 IF    IK = '1'                                         DOT
      *CX14 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012759 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35PJ-FN. EXIT.
      *N35PL.    NOTE *LOOP THRU ALL CX14 SEGMENTS        *.
       F35PL.    IF    IK = '0'                                         lv40
                 NEXT SENTENCE ELSE GO TO     F35PL-FN.
      *N35PN.    NOTE *SET THE CARTZA VALUE               *.
       F35PN.                                                           lv45
                 IF    CX13-CARTZ = 01                                  DOT
           MOVE        'FP' TO WS00-CARTZA
               GO TO     F35PN-FN.
                 IF    CX13-CARTZ = 02                                  DOT
           MOVE        'CP' TO WS00-CARTZA
               GO TO     F35PN-FN.
                 IF    CX13-CARTZ = 04                                  DOT
           MOVE        'AP' TO WS00-CARTZA
               GO TO     F35PN-FN.
                 IF    CX13-CARTZ = 05                                  DOT
                 AND   CT01-CTIDA = 002
           MOVE        'DI' TO WS00-CARTZA
               GO TO     F35PN-FN.
                 IF    CX13-CARTZ = 05                                  DOT
                 AND   CT01-CTIDA NOT = 002
           MOVE        'LP' TO WS00-CARTZA
               GO TO     F35PN-FN.
                 IF    CX13-CARTZ = 06                                  DOT
           MOVE        'IP' TO WS00-CARTZA
               GO TO     F35PN-FN.
                 IF    CX13-CARTZ = 09                                  DOT
           MOVE        'AI' TO WS00-CARTZA.
      *N35PP.    NOTE *EXCEPTION LOGIC FOR ANNUITIES      *.
       F35PP.    IF    CX13-CARTZ = 07                                  lv50
                 NEXT SENTENCE ELSE GO TO     F35PP-FN.
                 IF    ANNUITY                                          DOT
                 AND   CX06-CTID = CX14-CTID
           MOVE        'AD' TO WS00-CARTZA
               GO TO     F35PP-FN.
                 IF    UL                                               DOT
                 AND   CX06-CTID = CX14-CTID
           MOVE        'ID' TO WS00-CARTZA
               GO TO     F35PP-FN.
      *IF ACCT #S NOT = THEN AAPS                                       DOT
           MOVE        'AP' TO WS00-CARTZA.
       F35PP-FN. EXIT.
       F35PN-FN. EXIT.
      *N35PR.    NOTE *COLLECT STATS ON SPO               *.
       F35PR.                                                           lv45
           ADD         1 TO ACCT-SPO
      *BYPASS INACTIVE ONES
                 IF    CX13-CDEST = 03                                  DOT
               GO TO     F35PR-FN.
           ADD         1 TO ACCT-SPO-ACTPEN.                            DOT
                 IF    WS00-CARTZA = 'FP'                               DOT
           ADD         1 TO ACCT-SPO-FP.
                 IF    WS00-CARTZA = 'CP'                               DOT
           ADD         1 TO ACCT-SPO-CP.
                 IF    WS00-CARTZA = 'AP'                               DOT
           ADD         1 TO ACCT-SPO-AP.
                 IF    WS00-CARTZA = 'DI'                               DOT
           ADD         1 TO ACCT-SPO-DI.
                 IF    WS00-CARTZA = 'LP'                               DOT
           ADD         1 TO ACCT-SPO-LP.
                 IF    WS00-CARTZA = 'IP'                               DOT
           ADD         1 TO ACCT-SPO-IP.
                 IF    WS00-CARTZA = 'AD'                               DOT
           ADD         1 TO ACCT-SPO-AD.
                 IF    WS00-CARTZA = 'ID'                               DOT
           ADD         1 TO ACCT-SPO-ID.
                 IF    WS00-CARTZA = 'AI'                               DOT
           ADD         1 TO ACCT-SPO-AI.
                 IF    WS00-CARTZA = 'FP'                               DOT
                 AND   CX14-CPITC = 2
                 AND   CX14-CTIDA = 002
           ADD         1 TO ACCT-SPO-FP-FF.
                 IF    WS00-CARTZA = 'DI'                               DOT
                 AND   CX14-CPITC = 2
                 AND   CX14-CTIDA = 002
           ADD         1 TO ACCT-SPO-DI-FF.
      *N35PT.    NOTE *COUNT ACTIVE ONES ONLY             *.
       F35PT.    IF    CX13-CDEST = 01                                  lv50
                 NEXT SENTENCE ELSE GO TO     F35PT-FN.
           ADD         1 TO ACCT-SPO-ACTIVE.
                 IF    WS00-CARTZA = 'FP'                               DOT
           ADD         1 TO ACCT-SPO-FP-A.
                 IF    WS00-CARTZA = 'CP'                               DOT
           ADD         1 TO ACCT-SPO-CP-A.
                 IF    WS00-CARTZA = 'AP'                               DOT
           ADD         1 TO ACCT-SPO-AP-A.
                 IF    WS00-CARTZA = 'DI'                               DOT
           ADD         1 TO ACCT-SPO-DI-A.
                 IF    WS00-CARTZA = 'LP'                               DOT
           ADD         1 TO ACCT-SPO-LP-A.
                 IF    WS00-CARTZA = 'IP'                               DOT
           ADD         1 TO ACCT-SPO-IP-A.
                 IF    WS00-CARTZA = 'AD'                               DOT
           ADD         1 TO ACCT-SPO-AD-A.
                 IF    WS00-CARTZA = 'ID'                               DOT
           ADD         1 TO ACCT-SPO-ID-A.
                 IF    WS00-CARTZA = 'AI'                               DOT
           ADD         1 TO ACCT-SPO-AI-A.
                 IF    WS00-CARTZA = 'FP'                               DOT
                 AND   CX14-CPITC = 2
                 AND   CX14-CTIDA = 002
           ADD         1 TO ACCT-SPO-FP-FF-A.
                 IF    WS00-CARTZA = 'DI'                               DOT
                 AND   CX14-CPITC = 2
                 AND   CX14-CTIDA = 002
           ADD         1 TO ACCT-SPO-DI-FF-A.
       F35PT-FN. EXIT.
      *N35PV.    NOTE *COUNT PENDING ONES ONLY            *.
       F35PV.    IF    CX13-CDEST = 02                                  lv50
                 NEXT SENTENCE ELSE GO TO     F35PV-FN.
           ADD         1 TO ACCT-SPO-PEND.
                 IF    WS00-CARTZA = 'FP'                               DOT
           ADD         1 TO ACCT-SPO-FP-P.
                 IF    WS00-CARTZA = 'CP'                               DOT
           ADD         1 TO ACCT-SPO-CP-P.
                 IF    WS00-CARTZA = 'AP'                               DOT
           ADD         1 TO ACCT-SPO-AP-P.
                 IF    WS00-CARTZA = 'DI'                               DOT
           ADD         1 TO ACCT-SPO-DI-P.
                 IF    WS00-CARTZA = 'LP'                               DOT
           ADD         1 TO ACCT-SPO-LP-P.
                 IF    WS00-CARTZA = 'IP'                               DOT
           ADD         1 TO ACCT-SPO-IP-P.
                 IF    WS00-CARTZA = 'AD'                               DOT
           ADD         1 TO ACCT-SPO-AD-P.
                 IF    WS00-CARTZA = 'ID'                               DOT
           ADD         1 TO ACCT-SPO-ID-P.
                 IF    WS00-CARTZA = 'AI'                               DOT
           ADD         1 TO ACCT-SPO-AI-P.
                 IF    WS00-CARTZA = 'FP'                               DOT
                 AND   CX14-CPITC = 2
                 AND   CX14-CTIDA = 002
           ADD         1 TO ACCT-SPO-FP-FF-P.
                 IF    WS00-CARTZA = 'DI'                               DOT
                 AND   CX14-CPITC = 2
                 AND   CX14-CTIDA = 002
           ADD         1 TO ACCT-SPO-DI-FF-P.
       F35PV-FN. EXIT.
       F35PR-FN. EXIT.
      *N35PX.    NOTE *GET THE NEXT CX14                  *.
       F35PX.                                                           lv45
           PERFORM     F94DK THRU F94DK-FN.
       F35PX-FN. EXIT.
       F35PL-900. GO TO F35PL.
       F35PL-FN. EXIT.
       F35PH-FN. EXIT.
      *N35PY.    NOTE *GET THE NEXT CX13                  *.
       F35PY.                                                           lv35
           PERFORM     F94DI THRU F94DI-FN.
       F35PY-FN. EXIT.
       F35PF-900. GO TO F35PF.
       F35PF-FN. EXIT.
       F35PB-FN. EXIT.
       F35NF-FN. EXIT.
      *N35RB.    NOTE *GET THE NEXT CX2Y RECORD           *.
       F35RB.                                                           lv20
           PERFORM     F94AB THRU F94AB-FN.
       F35RB-FN. EXIT.
       F35ND-900. GO TO F35ND.
       F35ND-FN. EXIT.
       F35NB-FN. EXIT.
      *N35RH.    NOTE *PROCESSING FOR GROUP BILL ARR      *.
       F35RH.    IF    7-LIFE-INSURANCE-GROUP                           lv10
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F35RH-FN.
      *TO CHECK WHETHER EXISTS ACTIVE
      *GROUP BILL ARRANGEMENT
      *INITIATE THE LINKAGE
           INITIALIZE  DA8G
           DA8H
           MS03
      *INITIATE THE SWITCHES
           MOVE        'N' TO EODS-SERVICE-DOWN
           EODS-QUEUE-FAILED
           MOVE        0 TO ACCT-GB-ACTIVE.
      *N35RJ.    NOTE *CALL EODS VIA CALLING CI0975       *.
       F35RJ.                                                           lv15
      *
           MOVE        14 TO DA8G-CARTY
           MOVE        QT63-CTID TO DA8H-CTID (1)
           MOVE        1 TO DA8H-QITEM
           MOVE        'RPC' TO DA8G-CSYS
           PERFORM     F97MD THRU F97MD-FN.
      *N35RL.    NOTE *GET THE RESPONSE QUEUE FROM EODS   *.
       F35RL.    IF    (DA8G-CERRE = SPACES                             lv20
                 OR    DA8G-CERRE = ZEROES)
                 AND   MS03-NMESS2 = ZEROES
                 AND   DA8G-GERTC = 'Y'
                 NEXT SENTENCE ELSE GO TO     F35RL-FN.
      *SUCCESSFULLY
      *N35RN.    NOTE *IF THE CALL IS SUCCESSFUL          *.
       F35RN.    IF    DA8G-CFAUL1 = SPACES                             lv25
                 AND   DA8G-TFACT1 = SPACES
                 NEXT SENTENCE ELSE GO TO     F35RN-FN.
      *
      *N35RP.    NOTE *CHECK WHETHER THE GROUP BILL ARR   *.
       F35RP.    IF    QT63-CTID = DA8G-CTID (1)                        lv30
                 AND   DA8G-QITEM >= 1
                 NEXT SENTENCE ELSE GO TO     F35RP-FN.
      *HAVE BEEN RETURNED OR NOT
      *
           MOVE        1 TO ACCT-GB-ACTIVE.
       F35RP-900. GO TO F35RQ-FN.
       F35RP-FN. EXIT.
      *N35RQ.    NOTE *NO GROUP BILL ARRANGEMENT RETURN   *.
       F35RQ.                                                           lv30
      *DUE TO FOLLOW REASON:
                 IF    DA8G-CFAUL2 = '0102'                             DOT
                 OR    DA8G-CFAUL3 = '1000'
      *1. REQUEST ACCOUNT NOT FOUND OR
      *2. NO RECORDS FOUND
           MOVE        0 TO ACCT-GB-ACTIVE
                 ELSE
      *IF EODS SERVICE ERROR, SET THE
      *EODS SERVICE SWITCH AS 'Y'.
           MOVE        'Y' TO EODS-SERVICE-DOWN.
       F35RQ-FN. EXIT.
       F35RN-900. GO TO F35RR-FN.
       F35RN-FN. EXIT.
      *N35RR.    NOTE *CALL SERVICE FAILED                *.
       F35RR.                                                           lv25
      *SET THE SWITCH FOR EODS SERVICE
      *AS 'Y'
           MOVE        'Y' TO EODS-SERVICE-DOWN.
       F35RR-FN. EXIT.
       F35RL-900. GO TO F35RS-FN.
       F35RL-FN. EXIT.
      *N35RS.    NOTE *IF GET QUEUE FAILED, SET THE       *.
       F35RS.                                                           lv20
      *EODS QUEUE SWITCH AS 'Y'
      *
           MOVE        'Y' TO EODS-QUEUE-FAILED.
       F35RS-FN. EXIT.
       F35RJ-FN. EXIT.
       F35RH-FN. EXIT.
      *N35SB.    NOTE *SETUP CX6Y LOOP                    *.
       F35SB.                                                           lv10
      *LOOK FOR ARRANGEMENTS WHERE THE
      *GIVEN ACCOUNT IS THE TARGET ACCT
           MOVE        CT01-CTID TO S-CXU6Y-CTID
           MOVE        ZEROES TO S-CXU6Y-CLID
           S-CXU6Y-CARTY
           S-CXU6Y-NARRS
           MOVE        SPACES TO S-CXU6Y-CTID1
           MOVE        ZEROES TO S-CXU6Y-CARTZ
           S-CXU6Y-NAPDS
           S-CXU6Y-NPISQ
           MOVE        '>=' TO S-CXU6Y-OPER
      *GU >= ON CX6Y
           PERFORM     F94EA THRU F94EA-FN.
      *N35SE.    NOTE *PROCESS ARR MATCHING ON ACCOUNT    *.
       F35SE.    IF    IK = '0'                                         lv15
                 AND   CX6Y-CTID = QT63-CTID
                 NEXT SENTENCE ELSE GO TO     F35SE-FN.
      *N35SH.    NOTE *PROCESS ONLY THE DCA SEGMENTS      *.
       F35SH.    IF    CX6Y-CARTY = 10                                  lv20
                 AND   CX6Y-CTID NOT = CX6Y-CTID1
                 NEXT SENTENCE ELSE GO TO     F35SH-FN.
      *CARTZ = 07 HAVE FROM, TO ACCT
      *NUMBERS SAME. SKIP THESE SINCE
      *THEY HAVE BEEN PROCESSED.
      *SKIP ALSO CERT OPTIONS AND
      *CLAIM DISTRIBUTIONS.
      *READ THE CX03 ARRANGEMENT INFO                                   DOT
           MOVE        CX6Y-CLID TO S-CXU01-CX01K
           CX01-CX01K
           MOVE        CX6Y-CARTY TO S-CXU03-CARTY
           MOVE        CX6Y-NARRS TO S-CXU03-NARRS
           PERFORM     F94DC THRU F94DC-FN.
                 IF    IK = '1'                                         DOT
      *CX03 NOT FOUND; RETURN ERROR
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012007 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N35SK.    NOTE *FULLY QUALIFIED GU ON CX13         *.
       F35SK.                                                           lv25
           INITIALIZE  CX13
           MOVE        CX6Y-CTID1 TO S-CXU06-CTID
           CX06-CTID
           MOVE        CX6Y-CARTZ TO S-CXU13-CARTZ
           MOVE        CX6Y-NAPDS TO S-CXU13-NAPDS
           PERFORM     F94DH THRU F94DH-FN.
                 IF    IK = '1'                                         DOT
      *CX13 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012009 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N35SN.    NOTE *LOOK ONLY AT COMPLETED ONES        *.
       F35SN.    IF    CX13-IIARR = 'Y'                                 lv30
                 AND   CX13-CARTZ NOT = 03
                 AND   CX13-CARTZ NOT = 08
                 NEXT SENTENCE ELSE GO TO     F35SN-FN.
      *SKIP ALSO CERT OPTIONS AND
      *CLAIM DISTRIBUTIONS
      *N35SQ.    NOTE *GET THE CX14 USING CX6Y KEY        *.
       F35SQ.                                                           lv35
           INITIALIZE  CX14
           MOVE        CX6Y-NPISQ TO S-CXU14-NPISQ
           PERFORM     F94DJ THRU F94DJ-FN.
                 IF    IK = '1'                                         DOT
      *CX14 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012759 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
           MOVE        'MC' TO WS00-CARTZA.                             DOT
       F35SQ-FN. EXIT.
      *N35ST.    NOTE *COLLECT STATS ON MC                *.
       F35ST.                                                           lv35
           ADD         1 TO ACCT-DCA.
                 IF    CX13-CDEST = 03                                  DOT
      *BYPASS INACTIVE ONES
               GO TO     F35ST-FN.
           ADD         1 TO ACCT-DCA-MC.                                DOT
       F35ST-FN. EXIT.
       F35SN-FN. EXIT.
       F35SK-FN. EXIT.
       F35SH-FN. EXIT.
      *N35TB.    NOTE *GET THE NEXT CX6Y RECORD           *.
       F35TB.                                                           lv20
           PERFORM     F94EB THRU F94EB-FN.
       F35TB-FN. EXIT.
       F35SE-900. GO TO F35SE.
       F35SE-FN. EXIT.
       F35SB-FN. EXIT.
       F35BB-FN. EXIT.
      *N35ZB.    NOTE *ERROR IN THIS FUNC ? TERMINATE     *.
       F35ZB.    IF    QT63-IERRC = 'Y'                                 lv07
                 NEXT SENTENCE ELSE GO TO     F35ZB-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        QT63-NMESA TO MS03-NMESS2                        ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35ZB-FN. EXIT.
       F35-FN.   EXIT.
      *N36.      NOTE *************************************.
      *               *                                   *
      *               *GET PAYMENT DETAIL SPECIFIC INFO   *
      *               *                                   *
      *               *************************************.
       F36.                                                             lv05
      *F35 IS DONE ONLY ONCE PER ACCT
      *N36ZB.    NOTE *ERROR IN THIS FUNC ? TERMINATE     *.
       F36ZB.    IF    QT63-IERRC = 'Y'                                 lv07
                 NEXT SENTENCE ELSE GO TO     F36ZB-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        QT63-NMESA TO MS03-NMESS2                        ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F36ZB-FN. EXIT.
       F36-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *IDENTIFY SETUP CONSTRAINTS         *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40BB.    NOTE *CLIENT INFO                        *.
       F40BB.                                                           lv10
                 IF    (WS00-AGE1 > ZERO                                DOT
                 AND   WS00-AGE1 < 21)
                 AND   (UTMA OR UGMA)
      *THE USER LOGINED MYFA IS MINOR
           MOVE        'Y' TO CLIENT-MINOR.
                 IF    WS00-AGE NOT < 18                                DOT
                 AND   CT01-CIRAT = 007
           MOVE        'Y' TO ACCT-NOT-MINOR.
                 IF    WS00-TAXPAYER-CLID NOT = ZERO                    DOT
           MOVE        'N' TO ACCT-NO-TAXPAYER
                 ELSE
           MOVE        'Y' TO ACCT-NO-TAXPAYER.
       F40BB-FN. EXIT.
      *N40DB.    NOTE *ACCOUNT INFO                       *.
       F40DB.                                                           lv10
                 IF    QT63-CTCUS = (001 OR 004                         DOT
                 OR    005 OR 007)
                 OR    WS00-GRIDC = 002
                 OR    WS00-GRIDC = 004
           MOVE        'Y' TO ACCT-PENSION.
                 IF    CT01-CQACT = 001                                 DOT
           MOVE        'Y' TO ACCT-IRA.
                 IF    CT01-CTSTA = 03                                  DOT
           MOVE        'Y' TO ACCT-INACTIVE.
                 IF    CT01-CTSTA = 01                                  DOT
           MOVE        'Y' TO ACCT-PENDING.
       F40DB-FN. EXIT.
      *N40EB.    NOTE *BANK ACCOUNT INFO                  *.
       F40EB.                                                           lv10
      *USE SAVED CX03-CARST
                 IF    WS00-CARST = 03                                  DOT
           MOVE        'Y' TO BANK-INACTIVE.
                 IF    WS00-CARST = 04                                  DOT
           MOVE        'Y' TO BANK-HOLD.
       F40EB-FN. EXIT.
      *N40GB.    NOTE *ANNUITY ACCOUNT                    *.
       F40GB.    IF    ANNUITY                                          lv10
                 NEXT SENTENCE ELSE GO TO     F40GB-FN.
                 IF    CT01-PRCOD = 780 OR 781                          DOT
           MOVE        'Y' TO ANNT-FIREFUND.
                 IF    CT01-PRCOD = 815 OR 816                          DOT
                 OR    817
           MOVE        'Y' TO ANNT-EMPBENEFIT.
                 IF    CT01-PRCOD = 870                                 DOT
           MOVE        'Y' TO ANNT-REVA.
                 IF    TB5B-CVSYS = '21' OR '22'                        DOT
           MOVE        'Y' TO ANNT-RPS
           MOVE        'Y' TO ANNT-PAYOUT.
                 IF    PG01-GEHCD (1) NOT = ZERO                        DOT
           MOVE        'Y' TO ANNT-HOLD.
                 IF    CT01-CTIDND (1:4) = '9200'                       DOT
           MOVE        'Y' TO ANNT-FIXEDSUBS.
      *N40GE.    NOTE *VANTAGE ANNUITY ACCOUNT            *.
       F40GE.    IF    TB5B-IVANT = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F40GE-FN.
                 IF    WF41-CVALB = '403' OR '501'                      DOT
           MOVE        'Y' TO ANNT-TSA.
                 IF    CT01-CTIDA = 004                                 DOT
                 AND   (CT01-PRCOD NOT < 1041
                 AND   CT01-PRCOD NOT > 1076)
                 OR    CT01-CTIDA = 005
                 AND   (CT01-PRCOD NOT < 1041
                 AND   CT01-PRCOD NOT > 1052)
                 OR    CT01-CTIDA = 005
                 AND   (CT01-PRCOD NOT < 1061
                 AND   CT01-PRCOD NOT > 1076)
           MOVE        'Y' TO ANNT-SINGLEPAY.
                 IF    WF41-CSSUP2 NOT = SPACES                         DOT
           MOVE        'Y' TO ANNT-MONEY-SUSP.
                 IF    WF41-CVOMC1 NOT = SPACES                         DOT
           MOVE        'Y' TO ANNT-RESTRICT.
                 IF    WF41-CASTA = 'D' OR 'E'                          DOT
           MOVE        'Y' TO ANNT-DEATH.
                 IF    WF41-CGMBR = 'A'                                 DOT
           MOVE        'Y' TO ANNT-RAVAPLUS-GMAB.
       F40GE-900. GO TO F40GH-FN.
       F40GE-FN. EXIT.
      *N40GH.    NOTE *NON-VANTAGE ANNUITY ACCOUNT        *.
       F40GH.                                                           lv15
                 IF    WE40-ALPLNI = 2 OR 3                             DOT
           MOVE        'Y' TO ANNT-TSA.
                 IF    WE40-ALFGH (1:1) > '2'                           DOT
           MOVE        'Y' TO ANNT-PAYOUT.
                 IF    WE40-ALPLNJ = 6                                  DOT
           MOVE        'Y' TO ANNT-SHEARSON.
                 IF    WE40-ALFGH (2:1) = '1'                           DOT
           MOVE        'Y' TO ANNT-SINGLEPAY.
                 IF    WE40-CSUSI NOT = ZERO                            DOT
           MOVE        'Y' TO ANNT-MONEY-SUSP.
                 IF    WE40-ALFGH = 200                                 DOT
                 AND   WE40-ALINNO = (0 OR 2)
                 AND   WE40-ALPLNI = 0
                 AND   WE40-COLPL < 900
           MOVE        'Y' TO ANNT-INNOVESTA.
                 IF    WE40-ALFGH = 200                                 DOT
                 AND   WE40-ALINNO = (0 OR 2)
                 AND   WE40-ALPLNI NOT = 0
                 AND   WE40-COLPL < 900
                 AND   WE40-COLPL NOT = 502
           MOVE        'Y' TO ANNT-INNOVESTB.
                 IF    WE40-ALFGH = 210                                 DOT
                 AND   CT01-PRCOD = 710
                 AND   WE40-COLPL = 809
           MOVE        'Y' TO ANNT-VARSUB.
       F40GH-FN. EXIT.
      *N40GK.    NOTE *SET ANNUITY REGULAR MINIMUMS       *.
       F40GK.                                                           lv15
           MOVE        50 TO ACCT-MONTHLY
           MOVE        150 TO ACCT-QUARTERLY
           MOVE        300 TO ACCT-SEMIANNUAL
           MOVE        600 TO ACCT-ANNUAL
           MOVE        100 TO ACCT-BIMONTHLY
           MOVE        50 TO ACCT-SEMIMONTHLY
           ACCT-BIWEEKLY
           ACCT-WEEKLY
           ACCT-OD.
       F40GK-FN. EXIT.
       F40GB-FN. EXIT.
      *N40JB.    NOTE *CERTIFICATE ACCOUNT                *.
       F40JB.    IF    7-CERTIFICATE-PRODUCT                            lv10
                 NEXT SENTENCE ELSE GO TO     F40JB-FN.
                 IF    CT01-PRCOD = 165 OR 166                          DOT
                 OR    971 OR 973
           MOVE        'Y' TO CERT-FLEXSAV.
                 IF    PJ02-CELBL NOT > ZERO                            DOT
           MOVE        'Y' TO CERT-NOLOANBAL.
                 IF    CT01-PRCOD = 181 OR 961                          DOT
           MOVE        'Y' TO CERT-MKTSTRATEGY.
      *N40JE.    NOTE *SET CERTIFICATE REGULAR MINIMUMS   *.
       F40JE.                                                           lv15
           MOVE        50 TO ACCT-MONTHLY
           ACCT-QUARTERLY
           ACCT-SEMIANNUAL
           ACCT-ANNUAL
           ACCT-BIMONTHLY
           ACCT-SEMIMONTHLY
           ACCT-BIWEEKLY
           ACCT-WEEKLY
           ACCT-OD.
       F40JE-FN. EXIT.
       F40JB-FN. EXIT.
      *N40LB.    NOTE *FUND ACCOUNT                       *.
       F40LB.    IF    7-MUTUAL-FUND-PRODUCT                            lv10
                 NEXT SENTENCE ELSE GO TO     F40LB-FN.
                 IF    TB5B-PRCAUT = 'T'                                DOT
           MOVE        'Y' TO FUND-TERMINATED.
                 IF    (CT01-PRCOD = 00013                              DOT
                 OR    CT01-PRCOD = 00167)
                 AND   CT01-PRSCD = '000000006'
           MOVE        'Y' TO FUND-CASHMGMTC.
      *THE FOLLOWING IS USED ONLY FOR                                   DOT
      *REACTIVATE LOGIC AND IS NOT TO
      *BE USED FOR MINIMUM AMOUNTS
                 IF    CT01-CQACT = 000                                 DOT
                 AND   (CT01-PRCOD = 00013
                 OR    00167 OR 00016)
                 AND   QT63-AACTV < 2000
           MOVE        'Y' TO FUND-NOMINBAL.
      *CASH MGMT A OR TAX FREE MONEY                                    DOT
                 IF    ((CT01-PRCOD = 00013                             DOT
                 OR    CT01-PRCOD = 00167)
                 AND   CT01-PRSCD = '000000001')
                 OR    CT01-PRCOD = 00016
           MOVE        'Y' TO FUND-FUNDSPO-OK.
                 IF    CT01-PRCOD = 00006                               DOT
           MOVE        'Y' TO FUND-NEWDIM.
                 IF    CT01-PRSCD = '000000002'                         DOT
      *MUTUAL FUND CLASS B
           MOVE        'Y' TO FUND-CLASSB.
      *N40LE.    NOTE *GET THE REGULAR MINIMUM AMOUNTS    *.
       F40LE.                                                           lv15
      *QUALIFIED ACCOUNT
                 IF    ACCT-IRA = 'Y'                                   DOT
           MOVE        50 TO ACCT-MONTHLY
           ACCT-QUARTERLY
           ACCT-SEMIANNUAL
           ACCT-ANNUAL
           ACCT-BIMONTHLY
           ACCT-SEMIMONTHLY
           ACCT-BIWEEKLY
           ACCT-WEEKLY
           ACCT-OD
               GO TO     F40LE-FN.
      *NON-QUALIFIED ACCOUNT.                                           DOT
      *NEW DIMENSIONS FUND SPECIAL                                      DOT
                 IF    (FUND-NEWDIM = 'Y'                               DOT
                 AND   (UTMA OR UGMA))
           MOVE        100 TO ACCT-MONTHLY
           MOVE        300 TO ACCT-QUARTERLY
           MOVE        600 TO ACCT-SEMIANNUAL
           MOVE        1200 TO ACCT-ANNUAL
           MOVE        200 TO ACCT-BIMONTHLY
           MOVE        100 TO ACCT-SEMIMONTHLY
           ACCT-BIWEEKLY
           ACCT-WEEKLY
           ACCT-OD
               GO TO     F40LE-FN.
      *IF BELOW 2000, 100 PER MONTH                                     DOT
      *OTHERWISE 100 ANY FREQUENCY
                 IF    QT63-AACTV < 2000                                DOT
           MOVE        100 TO ACCT-MONTHLY
           MOVE        300 TO ACCT-QUARTERLY
           MOVE        600 TO ACCT-SEMIANNUAL
           MOVE        1200 TO ACCT-ANNUAL
           MOVE        200 TO ACCT-BIMONTHLY
           MOVE        100 TO ACCT-SEMIMONTHLY
           ACCT-BIWEEKLY
           ACCT-WEEKLY
           ACCT-OD
                 ELSE
           MOVE        100 TO ACCT-MONTHLY
           ACCT-QUARTERLY
           ACCT-SEMIANNUAL
           ACCT-ANNUAL
           ACCT-BIMONTHLY
           ACCT-SEMIMONTHLY
           ACCT-BIWEEKLY
           ACCT-WEEKLY
           ACCT-OD.
       F40LE-FN. EXIT.
      *N40LG.    NOTE *OD EXCEPTION FOR FUNDS APPLIES     *.
       F40LG.    IF    (ACCT-INACTIVE = 'Y' OR                          lv15
                       ACCT-PENDING = 'Y')
                 AND   (CT01-PRCOD = 13 OR 16
                 OR    167)
                 AND   ACCT-IRA NOT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40LG-FN.
      *ONLY TO NQ CM ACCOUNTS
           MOVE        2000 TO ACCT-OD.
       F40LG-FN. EXIT.
      *N40LJ.    NOTE *OD EXCEPTION FOR FUNDS APPLIES     *.
       F40LJ.    IF    (ACCT-INACTIVE = 'Y' OR                          lv15
                       ACCT-PENDING = 'Y')
                 AND   (CT01-PRCOD = 102 OR 106
                       OR 108)
                 NEXT SENTENCE ELSE GO TO     F40LJ-FN.
      *TO INFLATION PROTECTED
      *SECURITIES FUND OR FRF FUND OR
      *ARC FUND OR DSV FUND
           MOVE        5000 TO ACCT-OD.
       F40LJ-FN. EXIT.
      *N40LK.    NOTE *OD EXCEPTION FOR FUNDS APPLIES     *.
       F40LK.    IF    (ACCT-INACTIVE = 'Y' OR                          lv15
                       ACCT-PENDING = 'Y')
                 AND   (CT01-PRCOD = 107 OR 124
                       OR 125 OR 126)
                 NEXT SENTENCE ELSE GO TO     F40LK-FN.
      *TO INFLATION PROTECTED
      *ARC OR CONTRARIAN EQUITY OR
      *US EQUITY FUND
      *OR THREADNEEDLE GLOBAL EXTENDED
      *ALPHA FUND
           MOVE        10000 TO ACCT-OD.
       F40LK-FN. EXIT.
       F40LB-FN. EXIT.
      *N40NB.    NOTE *INSURANCE ACCOUNT                  *.
       F40NB.    IF    7-LIFE-INSURANCE-GROUP                           lv10
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F40NB-FN.
      *CLASSIFY THE ACCOUNT
                 IF    CT01-PRCOD NOT > 199                             DOT
                 OR    (CT01-PRCOD > 299
                 AND   CT01-PRCOD < 599)
           MOVE        'Y' TO LIFE-TRAD-ACCT.
                 IF    CT01-PRCOD > 199                                 DOT
                 AND   CT01-PRCOD < 261
           MOVE        'Y' TO LIFE-UL-ACCT.
                 IF    CT01-PRCOD > 299                                 DOT
                 AND   CT01-PRCOD NOT > 399
           MOVE        'Y' TO LIFE-TERM.
                 IF    CT01-PRCOD = 500 OR 501                          DOT
           MOVE        'Y' TO LIFE-DI.
                 IF    CT01-PRCOD = 201                                 DOT
           MOVE        'Y' TO LIFE-LPPLUS.
                 IF    CT01-PRCOD = 222                                 DOT
           MOVE        'Y' TO LIFE-V2D.
                 IF    CT01-PRCOD = 225 OR 235                          DOT
                 OR    238 OR 239
           MOVE        'Y' TO LIFE-V2D2.
                 IF    CT01-PRCOD = 223                                 DOT
           MOVE        'Y' TO LIFE-VULIII.
                 IF    CT01-PRCOD = 204 OR 205                          DOT
                 OR    206 OR 207 OR 228
                 OR    229 OR 231 OR 232
                 OR    233 OR 234 OR 236
                 OR    237 OR 241 OR 242
                 OR    211 OR 212 OR 213
      *FOR IUL PRODUCT(211, 212), THE
      *RULE IS SAME WITH FUL(204, 205)
           MOVE        'Y' TO LIFE-VUL4.
                 IF    CT01-PRCOD = 210 OR 220                          DOT
                 OR    250
           MOVE        'Y' TO LIFE-SGLPAY.
                 IF    CT01-PRCOD = 400                                 DOT
           MOVE        'Y' TO LIFE-SGLPAYWHL.
                 IF    CT01-PRCOD = 230 OR 240                          DOT
           MOVE        'Y' TO LIFE-SHEARSON.
                 IF    CT01-PRCOD = 208 OR 209                          DOT
                 OR    243 OR 244
           MOVE        'Y' TO LIFE-F2D.
      *N40NE.    NOTE *VANTAGE ACCOUNT                    *.
       F40NE.    IF    TB5B-IVANT = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F40NE-FN.
                 IF    WF41-ALPAY NOT > ZERO                            DOT
           MOVE        'Y' TO LIFE-NOLOAN.
      *MOVE PREMIUM DUE DATE AND POLICY                                 DOT
      *DATE TO WORKING STORAGE
           MOVE        WF41-ALDDUE TO LIFE-ALDDUE
           MOVE        WF41-ALPLDT TO LIFE-ALPLDT.
       F40NE-900. GO TO F40NH-FN.
       F40NE-FN. EXIT.
      *N40NH.    NOTE *NON-VANTAGE ACCOUNT                *.
       F40NH.                                                           lv15
                 IF    WE40-ALLNB NOT > ZERO                            DOT
           MOVE        'Y' TO LIFE-NOLOAN.
           MOVE        WE40-ALDDUE TO LIFE-ALDDUE                       DOT
           MOVE        WE40-ALPLDT TO LIFE-ALPLDT.
       F40NH-FN. EXIT.
      *N40OB.    NOTE *CALCULATE GRACE DATE               *.
       F40OB.                                                           lv15
           MOVE        LIFE-ALDDUE TO 7-82-WORK-DATE
           PERFORM     F80HB THRU F80HB-FN
           MOVE        7-82-WORK-DATE TO LIFE-DGRAC.
       F40OB-FN. EXIT.
      *N40PB.    NOTE *INIT THE REGULAR MINIMUM AMOUNTS   *.
       F40PB.                                                           lv15
           MOVE        ZERO TO ACCT-MONTHLY
           MOVE        ZERO TO ACCT-QUARTERLY
           MOVE        ZERO TO ACCT-OD.
       F40PB-FN. EXIT.
      *N40QB.    NOTE *UNIVERSAL LIFE ACCOUNT             *.
       F40QB.    IF    LIFE-UL-ACCT = 'Y'                               lv15
                 NEXT SENTENCE ELSE GO TO     F40QB-FN.
      *N40QC.    NOTE *MIN IS BASED ON PRODUCT CODE       *.
       F40QC.         EXIT.                                             lv20
      *N40QD.    NOTE *UNIVERSAL LIFE                     *.
       F40QD.    IF    CT01-PRCOD =                                     lv25
                       200
                 NEXT SENTENCE ELSE GO TO     F40QD-FN.
           MOVE        WF41-APYMT TO
           ACCT-MONTHLY
           COMPUTE     ACCT-QUARTERLY ROUNDED =
           WF41-APYMT * 3.
                 IF    ACCT-MONTHLY < 20                                DOT
           MOVE        20 TO ACCT-MONTHLY.
                 IF    ACCT-QUARTERLY < 20                              DOT
           MOVE        20 TO ACCT-QUARTERLY.
           MOVE        ACCT-MONTHLY TO ACCT-OD.                         DOT
       F40QD-900. GO TO F40QC-FN.
       F40QD-FN. EXIT.
      *N40QE.    NOTE *LP-PLUS PRODUCT                    *.
       F40QE.    IF    CT01-PRCOD =                                     lv25
                       201 OR 202 OR 203
                 NEXT SENTENCE ELSE GO TO     F40QE-FN.
           MOVE        WF41-APYMT TO
           ACCT-MONTHLY
           COMPUTE     ACCT-QUARTERLY ROUNDED =
           WF41-APYMT * 3.
                 IF    ACCT-MONTHLY < 20                                DOT
           MOVE        25 TO ACCT-MONTHLY.
                 IF    ACCT-QUARTERLY < 20                              DOT
           MOVE        25 TO ACCT-QUARTERLY.
           MOVE        ACCT-MONTHLY TO ACCT-OD.                         DOT
       F40QE-900. GO TO F40QC-FN.
       F40QE-FN. EXIT.
      *N40QH.    NOTE *V2D, V2D2, F2D, F2DES PRODUCT      *.
       F40QH.    IF    CT01-PRCOD =                                     lv25
                       222 OR 225 OR 208 OR 209
                 OR    235 OR 238 OR 239 OR 243
                 OR    244
                 NEXT SENTENCE ELSE GO TO     F40QH-FN.
           MOVE        WF41-APYMT TO
           ACCT-MONTHLY
           COMPUTE     ACCT-QUARTERLY ROUNDED =
           WF41-APYMT * 3.
                 IF    ACCT-MONTHLY < 25                                DOT
           MOVE        25 TO ACCT-MONTHLY.
                 IF    ACCT-QUARTERLY < 25                              DOT
           MOVE        25 TO ACCT-QUARTERLY.
           MOVE        ACCT-MONTHLY TO ACCT-OD.                         DOT
       F40QH-900. GO TO F40QC-FN.
       F40QH-FN. EXIT.
      *N40QK.    NOTE *VUL PRODUCT                        *.
       F40QK.    IF    CT01-PRCOD =                                     lv25
                       204 OR 205 OR 206 OR 207
                 OR    221 OR 223 OR 228 OR 229
                 OR    231 OR 232 OR 233 OR 234
                 OR    236 OR 237 OR 241 OR 242
                 OR    211 OR 212 OR 213
                 NEXT SENTENCE ELSE GO TO     F40QK-FN.
      *IUL PRODUCT(211, 212)
      *TRIO UL PRODUCT(213)
           MOVE        WF41-APYMT TO
           ACCT-MONTHLY
           COMPUTE     ACCT-QUARTERLY ROUNDED =
           WF41-APYMT * 3.
                 IF    ACCT-MONTHLY < 25                                DOT
           MOVE        25 TO ACCT-MONTHLY.
                 IF    ACCT-QUARTERLY < 25                              DOT
           MOVE        25 TO ACCT-QUARTERLY.
           MOVE        ACCT-MONTHLY TO ACCT-OD.                         DOT
       F40QK-900. GO TO F40QC-FN.
       F40QK-FN. EXIT.
      *N40QN.    NOTE *SPVL PRODUCT / SP / SHEARSON       *.
       F40QN.    IF    CT01-PRCOD =                                     lv25
                       210 OR 220 OR 230 OR 240
                 OR    250 OR 260 OR 226 OR 227
                 NEXT SENTENCE ELSE GO TO     F40QN-FN.
      *IMPOSSIBLE SITUATION PER TA98
      *ONLY LOAN ALLOWED AND THAT VALUE
      *IS FIXED AMOUNT FOR ALL LOANS
       F40QN-900. GO TO F40QC-FN.
       F40QN-FN. EXIT.
      *N40QQ.    NOTE *OTHER (NON-VANTAGE) PRODUCT        *.
       F40QQ.                                                           lv25
      *CURRENT DATE - POLICY DATE
           MOVE        WE40-ALPLDT TO DF34-DTGRGA
           MOVE        WS00-CURRDATE TO DF34-DTGRGB
           MOVE        8 TO DF30-CDTSF
           MOVE        3 TO DF34-CDTUC
           PERFORM     F92DF THRU F92DF-FN
      *SAVE POLICY AGE IN YEARS
           MOVE        DF34-NDTUN TO WS00-POLAGE.
                 IF    WS00-POLAGE < 3                                  DOT
      *RELATIVELY NEW POLICY < 3 YEARS
           MOVE        WE40-AAMFY TO ACCT-MONTHLY
           MULTIPLY    3 BY WE40-AAMFY
           GIVING ACCT-QUARTERLY
                 ELSE
      *OK FINE IT IS AN OLD POLICY THEN
           MOVE        WE40-AMMP TO ACCT-MONTHLY
           MULTIPLY    3 BY WE40-AMMP
           GIVING ACCT-QUARTERLY.
                 IF    ACCT-MONTHLY < 20                                DOT
           MOVE        20 TO ACCT-MONTHLY.
                 IF    ACCT-QUARTERLY < 20                              DOT
           MOVE        20 TO ACCT-QUARTERLY.
           MOVE        ACCT-MONTHLY TO ACCT-OD.                         DOT
       F40QQ-FN. EXIT.
       F40QC-FN. EXIT.
       F40QB-FN. EXIT.
      *N40TB.    NOTE *TRADITIONAL LIFE ACCOUNT           *.
       F40TB.    IF    LIFE-TRAD-ACCT = 'Y'                             lv15
                 NEXT SENTENCE ELSE GO TO     F40TB-FN.
      *THESE HAVE EXACT PREMIUMS
      *BASED ON A CALCULATION
           PERFORM     F80BB THRU F80BB-FN
      *CHECK THE ANNIVERSARY DATE
      *AGAINST THE DUE DATE
      *TO DETERMINE IF MO, QT, SA, AN
      *TERMS ARE ALLOWED.
           PERFORM     F80DB THRU F80DB-FN
      *SET ON DEMAND MINIMUM
                 IF    WE40-MPMTF = 'MONTHLY'                           DOT
           MOVE        ACCT-MONTHLY TO ACCT-OD.
                 IF    WE40-MPMTF = 'QUARTERLY'                         DOT
           MOVE        ACCT-QUARTERLY TO ACCT-OD.
                 IF    WE40-MPMTF = 'SEMI-ANNUAL'                       DOT
           MOVE        ACCT-SEMIANNUAL TO ACCT-OD.
                 IF    WE40-MPMTF = 'ANNUAL'                            DOT
           MOVE        WE40-ALPAGR TO ACCT-OD.
       F40TB-FN. EXIT.
       F40NB-FN. EXIT.
       F40-FN.   EXIT.
      *N42.      NOTE *************************************.
      *               *                                   *
      *               *CHECK CLIENT'S SUITABLITY DATA     *
      *               *                                   *
      *               *************************************.
       F42.                                                             lv05
      *IF IT IS COMPLETE THEN ALLOW BA
      *N42BB.    NOTE *IF  TAXPAYER  IS  FOUND  FOR  AN   *.
       F42BB.    IF    ACCT-NO-TAXPAYER = 'N'                           lv10
                 NEXT SENTENCE ELSE GO TO     F42BB-FN.
      *ACCOUNT, CHECK FOR SUITABILITY
      *N42CB.    NOTE *>>>> CHECK FOR SUITABILITY  <<<<   *.
       F42CB.         EXIT.                                             lv15
      *N42DB.    NOTE *GET CONTEXT TYPE CODE, MARKETING   *.
       F42DB.                                                           lv20
      *PROGRAM CODE AND DISTRIBUTION
      *CHANNEL SELLING\ORIGINATING
      *NUMBER FROM CI0223.
           INITIALIZE  V223                                             DOT
           MOVE        CT01-CTID TO V223-CTID
           MOVE        WS00-TAXPAYER-CLID (1:3) TO
           V223-CLIDOA
           MOVE        WS00-TAXPAYER-CLID (4:) TO
           V223-CLID1A
      *>>>>>>>>>> CALL CI0223 <<<<<<<<<
           PERFORM     F97LB THRU F97LB-FN.
      *CHECK RETURN FROM CALLED PROGRAM                                 DOT
                 IF    V223-GERTC = 'A' OR 'C'                          DOT
           MOVE        014313 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F42CB-FN.
       F42DB-FN. EXIT.
      *N42EB.    NOTE *DETERMINE WHETHER A COMPLETE       *.
       F42EB.                                                           lv20
      *SUITABILITY PROFILE IS REQUIRED
      *FOR THE TRANSACTION. CALL CI0297
           INITIALIZE  K996
           MOVE        CT01-CTIDA TO K996-CTIDA
           MOVE        CT01-CQACT TO K996-CQACT
           MOVE        CT01-PRCOD TO K996-PRCOD
           MOVE        CT01-PRSCD TO K996-PRSCD
           MOVE        CT01-AYSID TO K996-AYSID
           MOVE        CT49-CTOWN TO K996-CTOWN.
                 IF    V223-CAACCT = 'Y'                                DOT
           MOVE        V223-CCTXT TO K996-CCTXT
           MOVE        V223-NDCIDA TO K996-NDCID.
                 IF    V223-CAACCT = 'N'                                DOT
           MOVE        SPACES TO K996-CCTXT
           MOVE        ZEROES TO K996-NDCID.
                 IF    V223-ICLID = 'Y'                                 DOT
           MOVE        V223-CMPRD TO K996-CMPRD
           MOVE        -1 TO K996-DRELEP.
                 IF    V223-ICLID = 'N'                                 DOT
           MOVE        ZEROES TO K996-CMPRD
           MOVE        SPACES TO K996-DRELEP.
           MOVE        WS00-GRIDC TO K996-GRIDC                         DOT
           MOVE        ACCT-MONTHLY TO K996-ACASH
           MOVE        SPACES TO K996-GERTC
           K996-ISURQ
           MOVE        'BA' TO K996-CINRT
      *>>>>>>>>>> CALL CI0297 <<<<<<<<<
           PERFORM     F97MB THRU F97MB-FN.
      *N42FB.    NOTE *CHECK THE RETURN CODE FROM         *.
       F42FB.    IF    K996-GERTC = 'E'                                 lv25
                 OR    K996-ISURQ = SPACES
                 NEXT SENTENCE ELSE GO TO     F42FB-FN.
      *CI0297. IF ERROR, THEN TERMINATE
           MOVE        014314 TO QT63-NMESA
           MOVE        'Y' TO QT63-IERRC
               GO TO     F42CB-FN.
       F42FB-FN. EXIT.
      *N42GB.    NOTE *CHECK WHETHER SUITABILITY DATA     *.
       F42GB.    IF    K996-GERTC = 'Y'                                 lv25
                 AND   K996-ISURQ = 'N'
                 NEXT SENTENCE ELSE GO TO     F42GB-FN.
      *IS REQUIRED. IF NOT REQUIRED,
      *DON'T STOP BA
           MOVE        'N' TO ACCT-SUITABILITY
           MOVE        'N' TO CLIENT-SUITABILITY
           MOVE        'N' TO ACCOUNT-SUITABILITY.
       F42GB-FN. EXIT.
      *N42HB.    NOTE *SUITABILITY IS REQUIRED, CALL      *.
       F42HB.    IF    (K996-GERTC = 'Y'                                lv25
                 AND   K996-ISURQ = 'Y')
                 AND   (WS00-CLDOD = ZEROES
                 OR    WS00-CLDTH = 'N')
                 NEXT SENTENCE ELSE GO TO     F42HB-FN.
      *CI0320
      *
      *
           INITIALIZE  W167
           MOVE        WS00-TAXPAYER-CLID TO W167-CLID
      *>>>>>>>>>> CALL CI0320 <<<<<<<<<
           PERFORM     F97NB THRU F97NB-FN.
      *N42HH.    NOTE *CHECK FOR BAD RETURN STATUS AND    *.
       F42HH.    IF    W167-CRETU NOT = SPACES                          lv30
                 AND   W167-CRETU NOT = ZEROES
                 NEXT SENTENCE ELSE GO TO     F42HH-FN.
      *TERMINATE IF FATAL ERROR
                 IF    W167-CRETU = '04'                                DOT
           MOVE        014315 TO QT63-NMESA
           MS03-NMESS2
                 ELSE
           MOVE        013171 TO QT63-NMESA
           MS03-NMESS2
           MOVE        'Y' TO QT63-IERRC
               GO TO     F42CB-FN.
       F42HH-FN. EXIT.
      *N42HI.    NOTE *PHONE/DEPENDENTS SUITABILITY       *.
       F42HI.                                                           lv30
      *PROCESSING FOR FA
                 IF    WS00-TAXPAYER-CLTYP = 'P'                        DOT
           PERFORM     F95BB THRU F95BB-FN.
                 IF    WS00-TAXPAYER-CLTYP = 'O'                        DOT
           PERFORM     F95CB THRU F95CB-FN.
       F42HI-FN. EXIT.
      *N42HN.    NOTE *COMPLETE SUITABILITY PROFILE IS    *.
       F42HN.    IF    W167-ICMPF (1) = 'N'                             lv30
                 NEXT SENTENCE ELSE GO TO     F42HN-FN.
      *REQUIRED BUT IT IS INCOMPLETE
      *STOP THE SETTING OF BA
           MOVE        'Y' TO ACCT-SUITABILITY
           MOVE        'Y' TO CLIENT-SUITABILITY.
       F42HN-900. GO TO F42JB-FN.
       F42HN-FN. EXIT.
      *N42JB.    NOTE *ACCOUNT SUITABILITY IS REQUIRED,   *.
       F42JB.                                                           lv30
      *CALL CI0323
      *
           INITIALIZE  W260
           MOVE        CT01-CTID TO W260-CTID
      *>>>>>>>>>> CALL CI0323 <<<<<<<<<
           PERFORM     F97PB THRU F97PB-FN.
      *N42JH.    NOTE *CHECK FOR BAD RETURN STATUS AND    *.
       F42JH.    IF    W260-CRETU NOT = SPACES                          lv35
                 AND   W260-CRETU NOT = ZEROES
                 NEXT SENTENCE ELSE GO TO     F42JH-FN.
      *TERMINATE IF FATAL ERROR
                 IF    W260-CRETU = '04'                                DOT
                 AND   W260-CERRE = '7266'
           MOVE        015315 TO MS03-NMESS2
           MOVE        'N' TO W260-ITSCI
           PERFORM     F98IC THRU F98IC-FN.
                 IF    W260-CRETU = '04'                                DOT
                 AND   W260-CERRE = '7270'
           MOVE        'X' TO W260-ITSCI
                 IF    W260-CRETU = '12'                                DOT
           MOVE        013171 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F42JH-FN. EXIT.
      *N42JN.    NOTE *COMPLETE SUITABILITY PROFILE IS    *.
       F42JN.    IF    W260-ITSCI = 'N'                                 lv35
                 NEXT SENTENCE ELSE GO TO     F42JN-FN.
      *REQUIRED BUT IT IS INCOMPLETE
      *STOP THE SETTING OF BA
           MOVE        'Y' TO ACCT-SUITABILITY
           MOVE        'Y' TO ACCOUNT-SUITABILITY.
       F42JN-FN. EXIT.
       F42JB-FN. EXIT.
       F42HB-FN. EXIT.
       F42EB-FN. EXIT.
       F42CB-FN. EXIT.
      *N42ZB.    NOTE *>>>ERROR IN CALLED MODULE,SEND<<   *.
       F42ZB.    IF    QT63-IERRC = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F42ZB-FN.
      *>>>THE MESSAGE AND TERMINATE  <<
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        QT63-NMESA TO MS03-NMESS2                        ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F42ZB-FN. EXIT.
       F42BB-FN. EXIT.
       F42-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *ACCESS BA DETAIL                   *
      *               *                                   *
      *               *************************************.
       F45.                                                             lv05
           MOVE        'N' TO WS00-DUPCX12.
                 IF    WS00-PARTKEY = 'N'                               DOT
               GO TO     F45-FN.
      *N45BB.    NOTE *THE ENTIRE KEY WAS PASSED          *.
       F45BB.    IF    WS00-NOCX12 = 'N'                                lv10
                 NEXT SENTENCE ELSE GO TO     F45BB-FN.
      *GET THE CX12 SEGMENT
           MOVE        QT63-CLID TO S-CXU01-CLID
           MOVE        01 TO S-CXU03-CARTY
           MOVE        QT63-NARRS TO S-CXU03-NARRS
           MOVE        QT63-CTID TO S-CXU06-CTID
           MOVE        QT63-CPMTC TO S-CXU12-CPMTC
           MOVE        QT63-NAPDS TO S-CXU12-NAPDS
           MOVE        QT63-GESTD TO S-CXU12-GESTD
           PERFORM     F94DF THRU F94DF-FN.
                 IF    IK = '1'                                         DOT
      *KNOWN CX12 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012675 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN                              ADU119
                 ELSE
           MOVE        CX12 TO WT12.
      *N45BE.    NOTE *CALCULATE THE SKIP DATES           *.
       F45BE.                                                           lv15
      *OUTPUT IS IN WS00-DSKIP
           MOVE        WT12-DNPMT TO WS00-DSKIP
           PERFORM     F96RA THRU F96RA-FN
           MOVE        WS00-DSKIP TO WS00-DSKIP1
           PERFORM     F96RA THRU F96RA-FN
           MOVE        WS00-DSKIP TO WS00-DSKIP2
           PERFORM     F96RA THRU F96RA-FN.
       F45BE-FN. EXIT.
       F45BB-FN. EXIT.
      *N45BG.    NOTE *SET OD FLAGS                       *.
       F45BG.    IF    WS00-NOCX12 = 'N'                                lv10
                 NEXT SENTENCE ELSE GO TO     F45BG-FN.
           MOVE        'N' TO ACCT-SHELL
      *SHELLS ARE ZERO $, 0 DATE,
      *ACTIVE STATUS ON DEMANDS
                 IF    WT12-APMTL = 0                                   DOT
                 AND   WT12-DNPMT = 0
                 AND   WT12-CDEST = 01
                 AND   WT12-NAPDS = 99
           MOVE        'Y' TO ACCT-SHELL.
       F45BG-FN. EXIT.
      *N45DB.    NOTE *PARTIAL KEY; MUST BE FOR AN ADD    *.
       F45DB.    IF    WS00-NOCX12 = 'Y'                                lv10
                 NEXT SENTENCE ELSE GO TO     F45DB-FN.
      *SEE IF A CX12 LIKE THIS EXISTS
      *NOT CHECKING FUTURES
           MOVE        QT63-CLID TO S-CXU01-CLID
           MOVE        01 TO S-CXU03-CARTY
           MOVE        QT63-NARRS TO S-CXU03-NARRS
           MOVE        QT63-CTID TO S-CXU06-CTID
           PERFORM     F94DT THRU F94DT-FN.
      *N45DE.    NOTE *LOOK FOR THE FIRST CX12            *.
       F45DE.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F45DE-FN.
           PERFORM     F94DG THRU F94DG-FN.
                 IF    IK = '1'                                         DOT
                 OR    (IK = '0'
                 AND   CX12-NAPDS = 99)
      *NONE FOUND OR ON DEMANDS ONLY
               GO TO     F45DE-FN.
      *N45DK.    NOTE *READ THRU ALL CX12 TO SEE DUPS     *.
       F45DK.    IF    IK = '0'                                         lv20
                 AND   WS00-DUPCX12 NOT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F45DK-FN.
           INITIALIZE  WT12.
                 IF    CX12-NAPDS = 99                                  DOT
      *ON DEMANDS FOUND
               GO TO     F45DE-FN.
      *CHECK FOR DUPLICATE
           MOVE        CX12 TO WT12
      *LOOK FOR DUPLICATE
           PERFORM     F91CB THRU F91CB-FN.
      *N45DN.    NOTE *READ NEXT CX12                     *.
       F45DN.                                                           lv25
           PERFORM     F94DG THRU F94DG-FN.
       F45DN-FN. EXIT.
       F45DK-900. GO TO F45DK.
       F45DK-FN. EXIT.
       F45DE-FN. EXIT.
       F45DB-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *LOAD OUTPUT AREA SWITCHES          *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *DEFAULT TO TA98 SETTINGS
      *MOVE 'N' TO OST OUT OF SCOPE 1'S
           MOVE        TD98-IARTYA TO QT63-IARTYA.
                 IF    WS00-NOCX12 = 'N'                                DOT
           MOVE        'Y' TO QT63-IACHI.
                 IF    QT63-IARTYA = 'Y'                                DOT
                 AND   WS00-NOCX12 = 'N'
           MOVE        'Y' TO QT63-IAIND2.
           MOVE        TD98-IARRGA TO QT63-IARRGA                       DOT
           QT63-IARRG1
           QT63-IARCDA
           QT63-IARCPA
           QT63-IARCD1
           MOVE        'N' TO QT63-IARCP1
           MOVE        TD98-IARLNA TO QT63-IARLNA
           QT63-IARLN1
           MOVE        TD98-IFQAN TO QT63-IFQAN
           MOVE        TD98-IFQSA TO QT63-IFQSA
           MOVE        TD98-IFQQT TO QT63-IFQQT
           MOVE        TD98-IFQBM TO QT63-IFQBM
           MOVE        TD98-IFQMO TO QT63-IFQMO
           MOVE        TD98-IFQSM TO QT63-IFQSM
           MOVE        TD98-IFQBW TO QT63-IFQBW
           MOVE        TD98-IFQWK TO QT63-IFQWK
           MOVE        TD98-IFQOD TO QT63-IFQOD
           INITIALIZE  QT63-NMESA
           WS00-NMESA
           WS00-NMESA1.
                 IF    QT63-CTIDA = (004 OR 005)                        DOT
                 AND   7-PRCOD = 213
      *PREVENT ANY ACTION ON TRIO TO
      *NOT POPULATE MESSAGE FOR GRACE
      *PERIOD AND DISALLOW LOAN PAYMENT
           MOVE        'N' TO QT63-IARTYA
           QT63-IACHI
           QT63-IAIND2.
      *N50AK.    NOTE *DISALLOW MODIFY TO CHANGE          *.
       F50AK.    IF    WS00-PARTKEY = 'Y'                               lv07
                 AND   WS00-NOCX12 = 'N'
                 NEXT SENTENCE ELSE GO TO     F50AK-FN.
      *FROM RECURRING TO OD OR FROM
      *OD TO RECURRING FREQUENCY.
      *
      *TURN OFF RECURRING FREQUENCIES
      *IF THIS IS AN OD MODIFY
                 IF    WT12-CPMTF = 99                                  DOT
           PERFORM     F91GB THRU F91GB-FN
           MOVE        014348 TO WS00-NMESA.
      *TURN OFF OD FREQUENCY IF THIS IS                                 DOT
      *A RECURRING MODIFY
                 IF    WT12-CPMTF NOT = 99                              DOT
           MOVE        'N' TO QT63-IFQOD.
       F50AK-FN. EXIT.
      *N50BA.    NOTE *SET ADD ALLOWED SWITCH IARTYA      *.
       F50BA.                                                           lv07
      *********************************
      *DETERMINE IF ADD FUNCTION VALID
      *****************  ENDS F50BZ  **
      *N50BB.    NOTE *SET ADD ALLOWED SWITCH IARTYA      *.
       F50BB.    IF    QT63-IARTYA = 'Y'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50BB-FN.
      *BOGUS ACCOUNT NUMBER
                 IF    ACCT-INVALID = 'Y'                               DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        012234 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *BANK HAS A HOLD
                 IF    BANK-HOLD = 'Y'                                  DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014175 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *BANK IS INACTIVE
                 IF    BANK-INACTIVE = 'Y'                              DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014216 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ACCOUNT IS INACTIVE
                 IF    ACCT-INACTIVE = 'Y'                              DOT
                 AND   CT01-CTIDA NOT = 002
      *********************************
      *BA'S ALLOWED ON INACTIVE FUNDS
      *********************************
           MOVE        'N' TO QT63-IARTYA
           MOVE        014178 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ALL BA S PRESENT
                 IF    ACCT-BA-COUNT >= IACCTM                          DOT
                 AND   ACCT-99-COUNT > 0
           MOVE        'N' TO QT63-IARTYA
           MOVE        014179 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *PENSION ACCOUNT
                 IF    ACCT-PENSION = 'Y'                               DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014181 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *OWNERSHIP MEANS NO BA TRANS
      *CURATOR, COMMITTEE, NEXT FRIEND
      *ARE NOT OK FOR ADD
                 IF    (GUARDIAN                                        DOT
                 OR    (TRUST
                 AND   TESTAMENTARY))
           MOVE        'N' TO QT63-IARTYA
           MOVE        014217 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *OWNERSHIP MEANS NO ADD BA                                        DOT
      *VALUES FOR CTCUS ARE AS FOLLOWS
      *IRA BENEFICIAL               009
      *ROTH CONTRIBUTORY BENEFICIAL 012
      *ROTH CONVERSION BENEFICIAL   014
      *SEP IRA                      008
                 IF    CONDITIONAL-MINOR                                DOT
                 OR    CUSTODIAL-TSCA
                 OR    QT63-CTCUS = 009
                 OR    QT63-CTCUS = 012
                 OR    QT63-CTCUS = 014
                 OR    QT63-CTCUS = 008
                 OR    LIFE-TENANT
                 OR    SSI-REPRESENTATIVE
                 OR    USUFRUCTORY
                 OR    EXECUTOR
           MOVE        'N' TO QT63-IARTYA
           MOVE        014217 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *SOMEONE DEAD IS RELATED TO ACCT                                  DOT
                 IF    ACCT-DEAD = 'Y'                                  DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014218 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *EDUCATIONAL IRA, OWNER NOT MINOR                                 DOT
                 IF    ACCT-NOT-MINOR = 'Y'                             DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014219 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *TAXPAYER NOT FOUND FOR THE A/C                                   DOT
                 IF    ACCT-NO-TAXPAYER = 'Y'                           DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014316 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *CLIENT AGE < 21 AND UTMA/UGMA                                    DOT
                 IF    CLIENT-MINOR = 'Y'                               DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        015494 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *N50BE.    NOTE *ANNUITY SPECIFIC EDITS             *.
       F50BE.    IF    ANNUITY                                          lv15
                 NEXT SENTENCE ELSE GO TO     F50BE-FN.
      *ANNUITY IS A TSA
                 IF    ANNT-TSA = 'Y'                                   DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014182 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ANNUITY IS IN PAYOUT
                 IF    ANNT-PAYOUT = 'Y'                                DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014183 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *SHEARSON ANNUITY
                 IF    ANNT-SHEARSON = 'Y'                              DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014184 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *FIREMAN'S FUND ANNUITY
                 IF    ANNT-FIREFUND = 'Y'                              DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014185 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ANNUITY IS A SINGLE PAY
                 IF    ANNT-SINGLEPAY = 'Y'                             DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014186 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ANNUITY IS EMPLOYEE BENEFIT
                 IF    ANNT-EMPBENEFIT = 'Y'                            DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014187 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ANNUITY IS REVA
                 IF    ANNT-REVA = 'Y'                                  DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014188 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ANNUITY IS RPS
                 IF    ANNT-RPS = 'Y'                                   DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014189 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ANNUITY MONEY IN SUSPENSE
                 IF    ANNT-MONEY-SUSP = 'Y'                            DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014190 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ANNUITY HOLD ON ACCOUNT
                 IF    ANNT-HOLD = 'Y'                                  DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014191 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ANNUITY RESTRICT CODE
                 IF    ANNT-RESTRICT = 'Y'                              DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014191 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ANNUITY DEATH OR DEATH PENDING
                 IF    ANNT-DEATH = 'Y'                                 DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014191 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ANNUITY INNOVEST A
                 IF    ANNT-INNOVESTA = 'Y'                             DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014192 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ANNUITY INNOVEST B
                 IF    ANNT-INNOVESTB = 'Y'                             DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014192 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ANNUITY HAS VAR SUB ACCOUNTS
                 IF    ANNT-VARSUB = 'Y'                                DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014192 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ANNUITY HAS ACTIVE APPS OR
      *INTEREST/EARNINGS ARRANGEMENTS.
      *OD NOT VALID, SET ADD INVALID
                 IF    (ACCT-SPO-AP-A > ZERO                            DOT
                 OR    ACCT-SPO-AI-A > ZERO)
                 AND   QT63-IFQOD = 'N'
           MOVE        'N' TO QT63-IARTYA
           MOVE        014198 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ON DEMAND EXCEPTION TO ABOVE
      *RULE.  SET RECURRING FREQUENCIES
      *OFF, BUT LEAVE ADD AVAILABLE.
                 IF    ACCT-SPO-AP-A > ZERO                             DOT
                 OR    ACCT-SPO-AI-A > ZERO
                 AND   QT63-IFQOD = 'Y'
           PERFORM     F91GB THRU F91GB-FN
           MOVE        014424 TO WS00-NMESA
           QT63-NMESAA.
      *ANNUITY HAS FIXED SUBACCTS ONLY                                  DOT
                 IF    ANNT-FIXEDSUBS = 'Y'                             DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014202 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *IF RAVA-PLUS ACCOUNT, ASSOCIATED
      *WITH GMAB RIDER OR STANDALONE
      *PORTFOLIO NAVIGATOR
                 IF    ANNT-RAVAPLUS-GMAB = 'Y'                         DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014587 TO QT63-NMESAA
               GO TO     F50BA-FN.
       F50BE-FN. EXIT.
      *N50BH.    NOTE *CERTIFICATE SPECIFIC EDITS         *.
       F50BH.    IF    7-CERTIFICATE-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50BH-FN.
      *CERT IS FLEX SAVINGS
                 IF    CERT-FLEXSAV = 'Y'                               DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014193 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *CERT IS MARKET STRATEGY
                 IF    CERT-MKTSTRATEGY = 'Y'                           DOT
                 AND   ACCT-PENDING = 'Y'
           MOVE        'N' TO QT63-IARTYA
           MOVE        014244 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *CERT HAS ACTIVE OR PENDING
      *SPO ARRANGEMENT (CP)
      *OD NOT VALID, SET ADD INVALID
                 IF    ACCT-SPO-CP > ZERO                               DOT
                 AND   QT63-IFQOD = 'N'
           MOVE        'N' TO QT63-IARTYA
           MOVE        014198 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ON DEMAND EXCEPTION TO ABOVE
      *RULE.  SET RECURRING FREQUENCIES
      *OFF, BUT LEAVE ADD AVAILABLE.
                 IF    ACCT-SPO-CP > ZERO                               DOT
                 AND   QT63-IFQOD = 'Y'
           PERFORM     F91GB THRU F91GB-FN
           MOVE        014424 TO WS00-NMESA
           QT63-NMESAA.
      *CERT HAS ACTIVE INTEREST PAYOUT                                  DOT
      *ARRANGEMENT (IP)
      *OD NOT VALID, SET ADD INVALID
                 IF    ACCT-SPO-IP-A > ZERO                             DOT
                 AND   QT63-IFQOD = 'N'
           MOVE        'N' TO QT63-IARTYA
           MOVE        014198 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ON DEMAND EXCEPTION TO ABOVE
      *RULE.  SET RECURRING FREQUENCIES
      *OFF, BUT LEAVE ADD AVAILABLE.
                 IF    ACCT-SPO-IP-A > ZERO                             DOT
                 AND   QT63-IFQOD = 'Y'
           PERFORM     F91GB THRU F91GB-FN
           MOVE        014424 TO WS00-NMESA
           QT63-NMESAA.
      *DISALLOW OD TO SAVINGS ACCOUNT                                   DOT
                 IF    CX18-CCBAT = 02                                  DOT
                 AND   WS00-NOBANK = 'N'
           MOVE        'N' TO QT63-IFQOD
           MOVE        014344 TO QT63-NMESA
           WS00-NMESA1.
       F50BH-FN. EXIT.
      *N50BK.    NOTE *FUNDS SPECIFIC EDITS               *.
       F50BK.    IF    7-MUTUAL-FUND-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50BK-FN.
      *FUND IS TERMINATED
                 IF    FUND-TERMINATED = 'Y'                            DOT
           MOVE        'N' TO QT63-IARTYA
               GO TO     F50BA-FN.
      *FUND IS CASH MANAGEMENT C
                 IF    FUND-CASHMGMTC = 'Y'                             DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014195 TO QT63-NMESAA
               GO TO     F50BA-FN.
                 IF    FUND-CLASSB = 'Y'                                DOT
      *FUND IS MUTUAL FUND CLASS B
           MOVE        'N' TO QT63-IARTYA
           MOVE        015572 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *MINIMUM BALANCE IS NOT MET
      *OD NOT VALID, SET ADD INVALID
                 IF    FUND-NOMINBAL = 'Y'                              DOT
                 AND   QT63-IFQOD = 'N'
           MOVE        'N' TO QT63-IARTYA
           MOVE        014196 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ON DEMAND EXCEPTION TO ABOVE
      *RULE.  SET RECURRING FREQUENCIES
      *OFF, BUT LEAVE ADD AVAILABLE.
                 IF    FUND-NOMINBAL = 'Y'                              DOT
                 AND   QT63-IFQOD = 'Y'
           PERFORM     F91GB THRU F91GB-FN
           MOVE        014196 TO WS00-NMESA
           QT63-NMESAA.
      *FUND TO NON-FUND SPO EXISTS                                      DOT
      *OD NOT VALID, SET ADD INVALID
                 IF    FUND-FUNDSPO-OK NOT = 'Y'                        DOT
                 AND   ACCT-SPO-FP NOT =
                       ACCT-SPO-FP-FF
                 AND   QT63-IFQOD = 'N'
           MOVE        'N' TO QT63-IARTYA
           MOVE        014198 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ON DEMAND EXCEPTION TO ABOVE
      *RULE.  SET RECURRING FREQUENCIES
      *OFF, BUT LEAVE ADD AVAILABLE.
                 IF    FUND-FUNDSPO-OK NOT = 'Y'                        DOT
                 AND   ACCT-SPO-FP NOT =
                       ACCT-SPO-FP-FF
                 AND   QT63-IFQOD = 'Y'
           PERFORM     F91GB THRU F91GB-FN
           MOVE        014424 TO WS00-NMESA
           QT63-NMESAA.
      *FUND TO NON-FUND DIV DIST EXISTS                                 DOT
      *OD NOT VALID, SET ADD INVALID
                 IF    FUND-FUNDSPO-OK NOT = 'Y'                        DOT
                 AND   ACCT-SPO-DI NOT =
                       ACCT-SPO-DI-FF
                 AND   QT63-IFQOD = 'N'
           MOVE        'N' TO QT63-IARTYA
           MOVE        014198 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ON DEMAND EXCEPTION TO ABOVE
      *RULE.  SET RECURRING FREQUENCIES
      *OFF, BUT LEAVE ADD AVAILABLE.
                 IF    FUND-FUNDSPO-OK NOT = 'Y'                        DOT
                 AND   ACCT-SPO-DI NOT =
                       ACCT-SPO-DI-FF
                 AND   QT63-IFQOD = 'Y'
           PERFORM     F91GB THRU F91GB-FN
           MOVE        014424 TO WS00-NMESA
           QT63-NMESAA.
                 IF    (ACCT-INACTIVE = 'Y'                             DOT
                 OR    ACCT-PENDING = 'Y')
                 AND   (CT01-PRCOD = 102 OR 106
                       OR 108
                       OR 107
                       OR 124
                       OR 125
                       OR 126)
      *SET RECURRING FREQUENCIES OFF,
      *BUT LEAVE ADD AVAILABLE IN CASE
      *OF INFLATION PROTECTED
      *SECURITIES FUND OR FRF FUND OR
      *ARC FUND OR DSV FUND
      *OR CONTRARIAN EQUITY FUND
      *OR U.S. EQUITY FUND
      *OR THREADNEEDLE GLOBALEXTENDED
      *ALPHA FUND
           PERFORM     F91GB THRU F91GB-FN
           MOVE        012651 TO QT63-NMESAA
           WS00-NMESA.
      *DISALLOW OD TO SAVINGS ACCOUNT                                   DOT
                 IF    CX18-CCBAT = 02                                  DOT
                 AND   WS00-NOBANK = 'N'
           MOVE        'N' TO QT63-IFQOD
           MOVE        014344 TO QT63-NMESA
           WS00-NMESA1.
                 IF    CLOSE-B-SHARE                                    DOT
                 AND   CT01-PRSCD = '000000002'
      *CLOSE FOLLOWING B SHARES
           MOVE        'N' TO QT63-IARTYA
           MOVE        15320 TO QT63-NMESAA
               GO TO     F50BA-FN.
       F50BK-FN. EXIT.
      *N50BN.    NOTE *INSURANCE SPECIFIC EDITS           *.
       F50BN.    IF    7-LIFE-INSURANCE-GROUP                           lv15
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F50BN-FN.
      *.
      *DISABLE ON DEMAND ACH-IN FOR                                     DOT
      *INSURANCE ACCOUNT
      *REMOVE TO RE-ENABLE
           MOVE        'N' TO QT63-IFQOD
           MOVE        12650 TO QT63-NMESA
           WS00-NMESA1
      *ACCOUNT IS SHEARSON INSURANCE
                 IF    LIFE-SHEARSON = 'Y'                              DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        014197 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ACCOUNT HAS ACTIVE GROUP BILL
      *OD NOT VALID, SET ADD INVALID
                 IF    ACCT-GB-ACTIVE > ZERO                            DOT
                 AND   QT63-IFQOD = 'N'
           MOVE        'N' TO QT63-IARTYA
           MOVE        014200 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ON DEMAND EXCEPTION TO ABOVE
      *RULE.  SET RECURRING FREQUENCIES
      *OFF, BUT LEAVE ADD AVAILABLE.
                 IF    ACCT-GB-ACTIVE > ZERO                            DOT
                 AND   QT63-IFQOD = 'Y'
           PERFORM     F91GB THRU F91GB-FN
           MOVE        014200 TO WS00-NMESA
           QT63-NMESAA.
      *ACCOUNT HAS MONEY COMING IN                                      DOT
      *OD NOT VALID, SET ADD INVALID
                 IF    ACCT-DCA-MC > ZERO                               DOT
                 AND   LIFE-TRAD-ACCT = 'Y'
                 AND   QT63-IFQOD = 'N'
           MOVE        'N' TO QT63-IARTYA
           MOVE        014201 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ON DEMAND EXCEPTION TO ABOVE
      *RULE.  SET RECURRING FREQUENCIES
      *OFF, BUT LEAVE ADD AVAILABLE.
                 IF    ACCT-DCA-MC > ZERO                               DOT
                 AND   LIFE-TRAD-ACCT = 'Y'
                 AND   QT63-IFQOD = 'Y'
           PERFORM     F91GB THRU F91GB-FN
           MOVE        014201 TO WS00-NMESA
           QT63-NMESAA.
      *RECURRING AND ON DEMAND FREQ                                     DOT
      *START DATES PAST GRACE PERIOD.
      *DISABLE ADD FUNCTION.
                 IF    LIFE-DGRAC < WS00-GESTE                          DOT
                 AND   LIFE-DGRAC < WS00-GESTD1
           MOVE        'N' TO QT63-IARTYA
           MOVE        014229 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *ONLY RECURRING FREQUENCY START
      *DATE PAST GRACE PERIOD.
      *DISABLE RECURRING FREQUENCIES.
                 IF    LIFE-DGRAC < WS00-GESTE                          DOT
                 AND   LIFE-DGRAC NOT < WS00-GESTD1
           PERFORM     F91GB THRU F91GB-FN
           MOVE        014229 TO WS00-NMESA
           QT63-NMESAA.
      *ONLY ON DEMAND FREQUENCY START                                   DOT
      *DATE PAST GRACE PERIOD
      *DISABLE ON DEMAND FREQUENCY
                 IF    LIFE-DGRAC NOT < WS00-GESTE                      DOT
                 AND   LIFE-DGRAC < WS00-GESTD1
           MOVE        'N' TO QT63-IFQOD
           MOVE        014229 TO QT63-NMESA
           WS00-NMESA1.
      *VALUATION ERROR - NO DICE                                        DOT
                 IF    ACCT-VALERR = 'Y'                                DOT
           MOVE        'N' TO QT63-IARTYA
           MOVE        012234 TO QT63-NMESAA
               GO TO     F50BA-FN.
      *UNSUPPORTED TRAD DIRECT BILL FRQ                                 DOT
                 IF    LIFE-TRAD-ACCT = 'Y'                             DOT
                 AND   WE40-MPMTF NOT = 'MONTHLY'
                 AND   WE40-MPMTF NOT = 'ANNUAL'
                 AND   WE40-MPMTF NOT = 'QUARTERLY'
                 AND   WE40-MPMTF NOT =
                       'SEMI-ANNUAL'
           MOVE        'N' TO QT63-IFQOD
           MOVE        12651 TO QT63-NMESA
           WS00-NMESA1.
                 IF    EODS-QUEUE-FAILED = 'Y'                          DOT
      *EODS QUEUE IS NOT AVAILABLE FOR
      *GROUP BILL, SET ADD INVALID
           MOVE        'N' TO QT63-IARTYA
           MOVE        015005 TO QT63-NMESAA
               GO TO     F50BA-FN.
                 IF    EODS-SERVICE-DOWN = 'Y'                          DOT
      *EODS SERVICE IS DOWN FOR GROUP
      *BILL, SET ADD INVALID
           MOVE        'N' TO QT63-IARTYA
           MOVE        015017 TO QT63-NMESAA
               GO TO     F50BA-FN.
       F50BN-FN. EXIT.
      *N50BT.    NOTE *SUITABILITY  PROFILE  IS           *.
       F50BT.                                                           lv15
                 IF    ACCT-SUITABILITY = 'Y'                           DOT
      *INCOMPLETE, CHECK THE INDICATOR
           MOVE        'N' TO QT63-IARTYA.
                 IF    CLIENT-SUITABILITY = 'Y'                         DOT
           MOVE        014315 TO QT63-NMESAA.
                 IF    ACCOUNT-SUITABILITY = 'Y'                        DOT
           MOVE        015315 TO QT63-NMESAA
               GO TO     F50BA-FN.
       F50BT-FN. EXIT.
       F50BB-900. GO TO F50BZ-FN.
       F50BB-FN. EXIT.
      *N50BZ.    NOTE *TA98 RULES DO NO ALLOW             *.
       F50BZ.                                                           lv10
      *SAY PRODUCT RULES DO NOT ALLOW
           MOVE        'N' TO QT63-IARTYA
           MOVE        014202 TO QT63-NMESAA
               GO TO     F50BA-FN.
       F50BZ-FN. EXIT.
       F50BA-FN. EXIT.
      *N50CD.    NOTE *THE ADD ERROR IS FOR BOTH RECUR    *.
       F50CD.                                                           lv07
                 IF    QT63-IARTYA = 'N'                                DOT
                 AND   QT63-NMESAA NOT = ZEROS
      *AN ON-DEMAND
           MOVE        QT63-NMESAA TO QT63-NMESA
               GO TO     F50CD-FN.
       F50CD-FN. EXIT.
      *N50DA.    NOTE *SET CHG ALLOWED IND  IACHI         *.
       F50DA.                                                           lv07
      *********************************
      *DETERMINE IF MODIFY FUNCTION
      *VALID.
      *IF THIS IS AN 'ADD' CALL (NO
      *CX12 KEY PASSED) QT63-IACHI WILL
      *BE SPACES AND ALL MODIFY LOGIC
      *IS SKIPPED.
      *****************  ENDS F50DZ  **
      *N50DB.    NOTE *SET CHG ALLOWED IND  IACHI         *.
       F50DB.    IF    QT63-IACHI = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F50DB-FN.
      *BOGUS ACCOUNT NUMBER
                 IF    ACCT-INVALID = 'Y'                               DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        012234 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *BANK HAS A HOLD
                 IF    BANK-HOLD = 'Y'                                  DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014176 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *BANK IS INACTIVE
                 IF    BANK-INACTIVE = 'Y'                              DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014177 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *ACCOUNT IS INACTIVE
                 IF    ACCT-INACTIVE = 'Y'                              DOT
                 AND   CT01-CTIDA NOT = 002
      *********************************
      *BA'S ALLOWED ON INACTIVE FUNDS
      *********************************
           MOVE        'N' TO QT63-IACHI
           MOVE        014178 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *PENSION ACCOUNT
                 IF    ACCT-PENSION = 'Y'                               DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014181 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *OWNERSHIP MEANS NO BA TRANS
      *CURATOR, COMMITTEE, NEXT FRIEND
      *ARE OK FOR MODIFY ONLY
                 IF    (GUARDIAN                                        DOT
                 OR    (TRUST
                 AND   TESTAMENTARY))
                 AND   NOT CURATOR
           MOVE        'N' TO QT63-IACHI
           MOVE        014217 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *BA IS A FUTURE      OUT OF SCOPE
                 IF    WT12-CDEST = 04                                  DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014210 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *RECURR BA IS INACTIVE CANT DO IT
                 IF    WT12-CDEST = 03                                  DOT
                 AND   WT12-NAPDS NOT = 99
           MOVE        'N' TO QT63-IACHI
           MOVE        014211 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *OD BA IS NOT ACTIVE OR PENDING
                 IF    (WT12-CDEST NOT = 01                             DOT
                 AND   WT12-CDEST NOT = 02)
                 AND   WT12-NAPDS = 99
           MOVE        'N' TO QT63-IACHI
           MOVE        014346 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *SOMEONE DEAD IN RELATED TO ACCT                                  DOT
                 IF    ACCT-DEAD = 'Y'                                  DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014218 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *EDUCATIONAL IRA, OWNER NOT MINOR                                 DOT
                 IF    ACCT-NOT-MINOR = 'Y'                             DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014220 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *FREQUENCY NOT SUPPORTED BY OST                                   DOT
      *CURRENTLY ONLY ANNUAL, SEMI-
      *ANNUAL, QUARTERLY, BI-MONTHLY,
      *MONTHLY, SEMI-MONTHLY, BI-WEEKLY
      *WEEKLY AND IRREGULAR MONTHLY ARE
      *SUPPORTED.
                 IF    WT12-CPMTF NOT = 01                              DOT
                 AND   WT12-CPMTF NOT = 02
                 AND   WT12-CPMTF NOT = 04
                 AND   WT12-CPMTF NOT = 06
                 AND   WT12-CPMTF NOT = 12
                 AND   WT12-CPMTF NOT = 24
                 AND   WT12-CPMTF NOT = 26
                 AND   WT12-CPMTF NOT = 52
                 AND   WT12-CPMTF NOT = 99
           MOVE        'N' TO QT63-IACHI
           MOVE        014231 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *TAXPAYER NOT FOUND FOR THE A/C                                   DOT
                 IF    ACCT-NO-TAXPAYER = 'Y'                           DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014317 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *OD SHELLS ARE NOT MODIFIABLE                                     DOT
                 IF    ACCT-SHELL = 'Y'                                 DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014346 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *NO MODIFY OF OD AFTER 3PM                                        DOT
                 IF    WT12-NAPDS = 99                                  DOT
                 AND   ((WT12-DNPMT = WS00-DCACG1
                 AND   WS00-HH > 14)
                 OR    WT12-DNPMT < WS00-DCACG1)
           MOVE        'N' TO QT63-IACHI
           MOVE        014343 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *CLIENT AGE < 21 AND UTMA/UGMA                                    DOT
                 IF    CLIENT-MINOR = 'Y'                               DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        015494 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *N50DC.    NOTE *DETAIL HAS A FUTURE ASSOCIATED     *.
       F50DC.                                                           lv15
           MOVE        1                        TO J50DCR
                                    GO TO     F50DC-B.
       F50DC-A.
           ADD         1                        TO J50DCR.
       F50DC-B.
           IF          J50DCR                   >  IACCTL
                                    GO TO     F50DC-FN.
                 IF    ACCT-FCOUNT (J50DCR) > 0                         DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014210 TO QT63-NMESAC
               GO TO     F50DA-FN.
       F50DC-900. GO TO F50DC-A.
       F50DC-FN. EXIT.
      *N50DE.    NOTE *ANNUITY SPECIFIC EDITS             *.
       F50DE.    IF    ANNUITY                                          lv15
                 NEXT SENTENCE ELSE GO TO     F50DE-FN.
      *ANNUITY IS A TSA
                 IF    ANNT-TSA = 'Y'                                   DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014182 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *ANNUITY MONEY IN SUSPENSE
                 IF    ANNT-MONEY-SUSP = 'Y'                            DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014190 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *ANNUITY HOLD ON ACCOUNT
                 IF    ANNT-HOLD = 'Y'                                  DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014191 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *ANNUITY RESTRICT CODE
                 IF    ANNT-RESTRICT = 'Y'                              DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014191 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *ANNUITY DEATH OR DEATH PENDING
                 IF    ANNT-DEATH = 'Y'                                 DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014191 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *IF RAVA-PLUS ACCOUNT, ASSOCIATED
      *WITH GMAB RIDER OR STANDALONE
      *PORTFOLIO NAVIGATOR
                 IF    ANNT-RAVAPLUS-GMAB = 'Y'                         DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014587 TO QT63-NMESAC
               GO TO     F50DA-FN.
       F50DE-FN. EXIT.
      *N50DH.    NOTE *CERTIFICATE SPECIFIC EDITS         *.
       F50DH.    IF    7-CERTIFICATE-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50DH-FN.
      *CERT IS FLEX SAVINGS
                 IF    CERT-FLEXSAV = 'Y'                               DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014193 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *CERT IS MARKET STRATEGY
                 IF    CERT-MKTSTRATEGY = 'Y'                           DOT
                 AND   ACCT-PENDING = 'Y'
           MOVE        'N' TO QT63-IACHI
           MOVE        014244 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *OD NOT ALLOWED FROM SAVINGS
                 IF    WT12-NAPDS = 99                                  DOT
                 AND   CX18-CCBAT = 02
                 AND   WS00-NOBANK = 'N'
           MOVE        'N' TO QT63-IACHI
           MOVE        014344 TO QT63-NMESAC
               GO TO     F50DA-FN.
       F50DH-FN. EXIT.
      *N50DK.    NOTE *FUNDS SPECIFIC EDITS               *.
       F50DK.    IF    7-MUTUAL-FUND-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50DK-FN.
      *OD NOT ALLOWED FROM SAVINGS
                 IF    WT12-NAPDS = 99                                  DOT
                 AND   CX18-CCBAT = 02
                 AND   WS00-NOBANK = 'N'
           MOVE        'N' TO QT63-IACHI
           MOVE        014344 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *MODIFY BA NOT ALLOWED FOR ACTIVE                                 DOT
      *FUND ACCTS WHOSE VALUE IS ZERO
                 IF    FUND-VALERR = 'Y'                                DOT
                 AND   CT01-CTSTA = 02
           MOVE        'N' TO QT63-IACHI
           MOVE        013758 TO QT63-NMESAC
               GO TO     F50DA-FN.
                 IF    CLOSE-B-SHARE                                    DOT
                 AND   CT01-PRSCD = '000000002'
      *CLOSE FOLLOWING B SHARES
           MOVE        'N' TO QT63-IACHI
           MOVE        15320 TO QT63-NMESAC
               GO TO     F50DA-FN.
                 IF    FUND-CLASSB = 'Y'                                DOT
      *MODIFY BA NOT ALLOW FOR MUTUAL
      *FUND CLASS B ACCOUNT
           MOVE        'N' TO QT63-IACHI
           MOVE        015572 TO QT63-NMESAC
               GO TO     F50DA-FN.
       F50DK-FN. EXIT.
      *N50DN.    NOTE *INSURANCE SPECIFIC EDITS           *.
       F50DN.    IF    7-LIFE-INSURANCE-GROUP                           lv15
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F50DN-FN.
      *ACCOUNT IS PENDING
                 IF    ACCT-PENDING = 'Y'                               DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014199 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *BA DETAIL STATUS IS PENDING
                 IF    WT12-CDEST = 02                                  DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        014214 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *BA FREQUENCY IS NOT MONTHLY,                                     DOT
      *QUARTERLY OR OD - MYFA FORBIDS
      *MODIFICATIONS.
      *DISABLE ON DEMAND ACH-IN FOR
      *INSURANCE ACCOUNT
      *ADD NOT = 99 TO RE-ENABLE.
                 IF    WT12-CPMTF NOT = 12                              DOT
                 AND   WT12-CPMTF NOT = 04
           MOVE        'N' TO QT63-IACHI
           MOVE        014215 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *RECURRING FREQUENCY START DATE
      *IS PAST GRACE PERIOD.
      *DISABLE ADD FUNCTION.
                 IF    LIFE-DGRAC < WS00-GESTE                          DOT
                 AND   WT12-NAPDS NOT = 99
           MOVE        'N' TO QT63-IACHI
           MOVE        014229 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *ON DEMAND FREQUENCY START DATE
      *IS PAST GRACE PERIOD.
      *DISABLE ADD FUNCTION.
                 IF    LIFE-DGRAC < WS00-GESTD1                         DOT
                 AND   WT12-NAPDS = 99
           MOVE        'N' TO QT63-IACHI
           MOVE        014229 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *VALUATION ERROR - NO DICE
                 IF    ACCT-VALERR = 'Y'                                DOT
           MOVE        'N' TO QT63-IACHI
           MOVE        012234 TO QT63-NMESAC
               GO TO     F50DA-FN.
      *UNSUPPORTED TRAD DIRECT BILL FRQ                                 DOT
                 IF    WT12-CPMTF = 99                                  DOT
                 AND   LIFE-TRAD-ACCT = 'Y'
                 AND   WE40-MPMTF NOT = 'MONTHLY'
                 AND   WE40-MPMTF NOT = 'ANNUAL'
                 AND   WE40-MPMTF NOT = 'QUARTERLY'
                 AND   WE40-MPMTF NOT =
                       'SEMI-ANNUAL'
           MOVE        'N' TO QT63-IACHI
           MOVE        014426 TO QT63-NMESAC.
       F50DN-FN. EXIT.
      *N50DQ.    NOTE *ALL PRODS; THIS SHOULD BE LAST     *.
       F50DQ.                                                           lv15
      *DATE OF NEXT PAYMENT TOO EARLY
                 IF    WT12-DNPMT <                                     DOT
                       WS00-GESTE
                 AND   WT12-CPMTF NOT = 99
           MOVE        'N' TO QT63-IACHI
           MOVE        014213 TO QT63-NMESAC
               GO TO     F50DA-FN.
       F50DQ-FN. EXIT.
      *N50DT.    NOTE *SUITABILITY PROFILE  IS            *.
       F50DT.                                                           lv15
                 IF    ACCT-SUITABILITY = 'Y'                           DOT
      *INCOMPLETE, CHECK THE INDICATOR
           MOVE        'N' TO QT63-IACHI
           MOVE        014321 TO QT63-NMESAC
               GO TO     F50DA-FN.
       F50DT-FN. EXIT.
       F50DB-900. GO TO F50DZ-FN.
       F50DB-FN. EXIT.
      *N50DZ.    NOTE *TA98 RULES DO NO ALLOW             *.
       F50DZ.                                                           lv10
      *SAY PRODUCT RULES DO NOT ALLOW
           MOVE        'N' TO QT63-IACHI
           MOVE        014202 TO QT63-NMESAC
               GO TO     F50DA-FN.
       F50DZ-FN. EXIT.
       F50DA-FN. EXIT.
      *N50FA.    NOTE *SET REACT ALLOWED SWITCH IAIND2    *.
       F50FA.                                                           lv07
      *********************************
      *DETERMINE IF REACTIVATE FUNCTION
      *VALID
      *IF THIS IS AN 'ADD' CALL (NO
      *CX12 KEY PASSED) QT63-IAIND2
      *WILL BE SPACES AND ALL
      *REACTIVATE LOGIC IS SKIPPED.
      *****************  ENDS F50FZ  **
      *N50FB.    NOTE *SET REACT ALLOWED SWITCH IAIND2    *.
       F50FB.    IF    QT63-IAIND2 = 'Y'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50FB-FN.
      *BOGUS ACCOUNT NUMBER
                 IF    ACCT-INVALID = 'Y'                               DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        012234 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *BANK HAS A HOLD
                 IF    BANK-HOLD = 'Y'                                  DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014176 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *BANK IS INACTIVE
                 IF    BANK-INACTIVE = 'Y'                              DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014177 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ACCOUNT IS INACTIVE
                 IF    ACCT-INACTIVE = 'Y'                              DOT
                 AND   ((CT01-CTIDA NOT = 002
                 AND   CT01-CTIDA NOT = 021)
                 OR    (CT01-CTIDA = 021
                 AND   (7-PRCOD NOT = 00001
                 AND   7-PRCOD NOT = 00006
                 AND   7-PRCOD NOT = 00008)))
      *********************************
      *BA'S ALLOWED ON INACTIVE FUNDS
      *AND BASIC,ONE,IMA BROKERAGE ACCT
      *********************************
           MOVE        'N' TO QT63-IAIND2
           MOVE        014178 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *BA DETAIL IS NOT INACTIVE
                 IF    WT12-CDEST NOT = 03                              DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014221 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *PENSION ACCOUNT
                 IF    ACCT-PENSION = 'Y'                               DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014181 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *OWNERSHIP MEANS NO BA TRANS
      *CURATOR, COMMITTEE, NEXT FRIEND
      *ARE NOT OK FOR REACTIVATE
                 IF    (GUARDIAN                                        DOT
                 OR    (TRUST
                 AND   TESTAMENTARY))
           MOVE        'N' TO QT63-IAIND2
           MOVE        014217 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *OWNERSHIP MEANS NO REACTIVATE BA                                 DOT
      *VALUES FOR CTCUS ARE AS FOLLOWS
      *IRA BENEFICIAL               009
      *ROTH CONTRIBUTORY BENEFICIAL 012
      *ROTH CONVERSION BENEFICIAL   014
      *SEP IRA                      008
                 IF    CONDITIONAL-MINOR                                DOT
                 OR    CUSTODIAL-TSCA
                 OR    QT63-CTCUS = 009
                 OR    QT63-CTCUS = 012
                 OR    QT63-CTCUS = 014
                 OR    QT63-CTCUS = 008
                 OR    LIFE-TENANT
                 OR    SSI-REPRESENTATIVE
                 OR    USUFRUCTORY
                 OR    EXECUTOR
           MOVE        'N' TO QT63-IAIND2
           MOVE        014217 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *SOMEONE DEAD IN RELATED TO ACCT                                  DOT
                 IF    ACCT-DEAD = 'Y'                                  DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014218 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *EDUCATIONAL IRA, OWNER NOT MINOR                                 DOT
                 IF    ACCT-NOT-MINOR = 'Y'                             DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014219 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *FREQUENCY NOT SUPPORTED BY OST                                   DOT
                 IF    WT12-CPMTF NOT = 01                              DOT
                 AND   WT12-CPMTF NOT = 02
                 AND   WT12-CPMTF NOT = 04
                 AND   WT12-CPMTF NOT = 06
                 AND   WT12-CPMTF NOT = 12
                 AND   WT12-CPMTF NOT = 24
                 AND   WT12-CPMTF NOT = 26
                 AND   WT12-CPMTF NOT = 52
                 AND   WT12-CPMTF NOT = 99
           MOVE        'N' TO QT63-IAIND2
           MOVE        014231 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *TAXPAYER NOT FOUND FOR THE A/C                                   DOT
                 IF    ACCT-NO-TAXPAYER = 'Y'                           DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014317 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *DISALLOW REACT OF ON DEMANDS
                 IF    WT12-CPMTF = 99                                  DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014342 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *CLIENT AGE < 21 AND UTMA/UGMA                                    DOT
                 IF    CLIENT-MINOR = 'Y'                               DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        015494 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *N50FC.    NOTE *PRODUCT DOES NOT ALLOW MULTIPLE    *.
       F50FC.                                                           lv15
           MOVE        1                        TO J50FCR
                                    GO TO     F50FC-B.
       F50FC-A.
           ADD         1                        TO J50FCR.
       F50FC-B.
           IF          J50FCR                   >  IACCTL
                                    GO TO     F50FC-FN.
      *REGULARS
                 IF    J50FCR = 1                                       DOT
           MOVE        ZERO TO WS00-COUNTER.
                 IF    ACCT-RCOUNT (J50FCR) > 0                         DOT
                 AND   ACCT-ACOUNT (J50FCR) > 0
           ADD         1 TO WS00-COUNTER.
                 IF    TD98-IMPRA = 'N'                                 DOT
                 AND   WS00-COUNTER > ZERO
                 AND   WT12-CPMTC = 00
           MOVE        'N' TO QT63-IAIND2
           MOVE        014235 TO QT63-NMESAR
           MOVE        ZEROES TO WS00-COUNTER
               GO TO     F50FA-FN.
       F50FC-900. GO TO F50FC-A.
       F50FC-FN. EXIT.
      *N50FE.    NOTE *ANNUITY SPECIFIC EDITS             *.
       F50FE.    IF    ANNUITY                                          lv15
                 NEXT SENTENCE ELSE GO TO     F50FE-FN.
      *ANNUITY IS A TSA
                 IF    ANNT-TSA = 'Y'                                   DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014182 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ANNUITY IS IN PAYOUT
                 IF    ANNT-PAYOUT = 'Y'                                DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014183 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *SHEARSON ANNUITY
                 IF    ANNT-SHEARSON = 'Y'                              DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014184 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *FIREMAN'S FUND ANNUITY
                 IF    ANNT-FIREFUND = 'Y'                              DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014185 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ANNUITY IS A SINGLE PAY
                 IF    ANNT-SINGLEPAY = 'Y'                             DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014186 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ANNUITY IS EMPLOYEE BENEFIT
                 IF    ANNT-EMPBENEFIT = 'Y'                            DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014187 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ANNUITY IS REVA
                 IF    ANNT-REVA = 'Y'                                  DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014188 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ANNUITY IS RPS
                 IF    ANNT-RPS = 'Y'                                   DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014189 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ANNUITY MONEY IN SUSPENSE
                 IF    ANNT-MONEY-SUSP = 'Y'                            DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014190 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ANNUITY HOLD ON ACCOUNT
                 IF    ANNT-HOLD = 'Y'                                  DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014191 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ANNUITY RESTRICT CODE
                 IF    ANNT-RESTRICT = 'Y'                              DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014191 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ANNUITY DEATH OR DEATH PENDING
                 IF    ANNT-DEATH = 'Y'                                 DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014191 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ANNUITY INNOVEST A
                 IF    ANNT-INNOVESTA = 'Y'                             DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014192 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ANNUITY INNOVEST B
                 IF    ANNT-INNOVESTB = 'Y'                             DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014192 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ANNUITY HAS VAR SUB ACCOUNTS
                 IF    ANNT-VARSUB = 'Y'                                DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014192 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ANNUITY HAS ACTIVE APPS OR
      *INTEREST/EARNINGS ARRANGEMENTS
                 IF    ACCT-SPO-AP-A > ZERO                             DOT
                 OR    ACCT-SPO-AI-A > ZERO
           MOVE        'N' TO QT63-IAIND2
           MOVE        014198 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *IF RAVA-PLUS ACCOUNT, ASSOCIATED
      *WITH GMAB RIDER OR STANDALONE
      *PORTFOLIO NAVIGATOR
                 IF    ANNT-RAVAPLUS-GMAB = 'Y'                         DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014587 TO QT63-NMESAR
               GO TO     F50FA-FN.
       F50FE-FN. EXIT.
      *N50FH.    NOTE *CERTIFICATE SPECIFIC EDITS         *.
       F50FH.    IF    7-CERTIFICATE-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50FH-FN.
      *CERT IS FLEX SAVINGS
                 IF    CERT-FLEXSAV = 'Y'                               DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014193 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *CERT IS MARKET STRATEGY
                 IF    CERT-MKTSTRATEGY = 'Y'                           DOT
                 AND   ACCT-PENDING = 'Y'
           MOVE        'N' TO QT63-IAIND2
           MOVE        014244 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *CERT HAS ACTIVE OR PENDING
      *SPO ARRANGEMENT (CP)
                 IF    ACCT-SPO-CP > ZERO                               DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014198 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *CERT HAS ACTIVE INTEREST PAYOUT
      *ARRANGEMENT (IP)
                 IF    ACCT-SPO-IP-A > ZERO                             DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014424 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *NO LOAN BALANCE ON CERTIFICATE
                 IF    CERT-NOLOANBAL = 'Y'                             DOT
                 AND   WT12-CPMTC = 01
           MOVE        'N' TO QT63-IAIND2
           MOVE        014208 TO QT63-NMESAR
               GO TO     F50FA-FN.
       F50FH-FN. EXIT.
      *N50FK.    NOTE *FUNDS SPECIFIC EDITS               *.
       F50FK.    IF    7-MUTUAL-FUND-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50FK-FN.
      *FUND IS TERMINATED
                 IF    FUND-TERMINATED = 'Y'                            DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014194 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *FUND IS CASH MANAGEMENT C
                 IF    FUND-CASHMGMTC = 'Y'                             DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014195 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *MINIMUM BALANCE IS NOT MET
                 IF    FUND-NOMINBAL = 'Y'                              DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014196 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *FUND TO NON-FUND SPO EXISTS
                 IF    FUND-FUNDSPO-OK NOT = 'Y'                        DOT
                 AND   ACCT-SPO-FP NOT =
                       ACCT-SPO-FP-FF
           MOVE        'N' TO QT63-IAIND2
           MOVE        014198 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *FUND TO NON-FUND DIV DIST EXISTS
                 IF    FUND-FUNDSPO-OK NOT = 'Y'                        DOT
                 AND   ACCT-SPO-DI NOT =
                       ACCT-SPO-DI-FF
           MOVE        'N' TO QT63-IAIND2
           MOVE        014198 TO QT63-NMESAR
               GO TO     F50FA-FN.
                 IF    CLOSE-B-SHARE                                    DOT
                 AND   CT01-PRSCD = '000000002'
      *CLOSE FOLLOWING B SHARES
           MOVE        'N' TO QT63-IAIND2
           MOVE        15320 TO QT63-NMESAR
               GO TO     F50FA-FN.
                 IF    FUND-CLASSB = 'Y'                                DOT
      *REACTIVE BA NOT ALLOW FOR MUTUAL
      *FUND CLASS B ACCOUNT
           MOVE        'N' TO QT63-IAIND2
           MOVE        015572 TO QT63-NMESAR
               GO TO     F50FA-FN.
       F50FK-FN. EXIT.
      *N50FN.    NOTE *INSURANCE SPECIFIC EDITS           *.
       F50FN.    IF    7-LIFE-INSURANCE-GROUP                           lv15
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F50FN-FN.
      *ACCOUNT IS PENDING
                 IF    ACCT-PENDING = 'Y'                               DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014199 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ACCOUNT IS SHEARSON INSURANCE
                 IF    LIFE-SHEARSON = 'Y'                              DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014197 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ACCOUNT HAS ACTIVE GROUP BILL
                 IF    ACCT-GB-ACTIVE > ZERO                            DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014200 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *ACCOUNT HAS MONEY COMING IN
                 IF    ACCT-DCA-MC > ZERO                               DOT
                 AND   LIFE-TRAD-ACCT = 'Y'
           MOVE        'N' TO QT63-IAIND2
           MOVE        014201 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *START DATE IS PAST GRACE PERIOD
                 IF    LIFE-DGRAC < WS00-GESTE                          DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        014229 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *NO LOAN BALANCE
                 IF    LIFE-NOLOAN = 'Y'                                DOT
                 AND   WT12-CPMTC = 01
           MOVE        'N' TO QT63-IAIND2
           MOVE        014208 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *NO ACTIVE REGULAR BA'S FOR TRAD
                 IF    LIFE-TRAD-ACCT = 'Y'                             DOT
                 AND   ACCT-BA-ACOUNT = ZERO
                 AND   WT12-CPMTC = 01
           MOVE        'N' TO QT63-IAIND2
           MOVE        014209 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *VALUATION ERROR - NO DICE
                 IF    ACCT-VALERR = 'Y'                                DOT
           MOVE        'N' TO QT63-IAIND2
           MOVE        012234 TO QT63-NMESAR
               GO TO     F50FA-FN.
                 IF    EODS-QUEUE-FAILED = 'Y'                          DOT
      *EODS QUEUE IS NOT AVAILABLE FOR
      *GROUP BILL, SET MOD INVALID
           MOVE        'N' TO QT63-IAIND2
           MOVE        015005 TO QT63-NMESAR
               GO TO     F50FA-FN.
                 IF    EODS-SERVICE-DOWN = 'Y'                          DOT
      *EODS SERVICE IS DOWN FOR GROUP
      *BILL, SET MOD INVALID
           MOVE        'N' TO QT63-IAIND2
           MOVE        015017 TO QT63-NMESAR
               GO TO     F50FA-FN.
      *N50FP.    NOTE *ACCOUNT HAS ONE ACTIVE REG BA      *.
       F50FP.                                                           lv20
           MOVE        1                        TO J50FPR
                                    GO TO     F50FP-B.
       F50FP-A.
           ADD         1                        TO J50FPR.
       F50FP-B.
           IF          J50FPR                   >  IACCTL
                                    GO TO     F50FP-FN.
      *OR ACCT HAS ONE ACTIVE LOAN BA
                 IF    J50FPR = 1                                       DOT
           MOVE        ZERO TO WS00-RCOUNT
           MOVE        ZERO TO WS00-LCOUNT.
                 IF    ACCT-RCOUNT (J50FPR) > 0                         DOT
                 AND   ACCT-ACOUNT (J50FPR) > 0
           ADD         1 TO WS00-RCOUNT.
                 IF    ACCT-LCOUNT (J50FPR) > 0                         DOT
                 AND   ACCT-ACOUNT (J50FPR) > 0
           ADD         1 TO WS00-LCOUNT
      *CHECK FOR ONE ACTIVE REG BA
                 IF    WS00-RCOUNT > ZERO                               DOT
                 AND   WT12-CPMTC = 00
           MOVE        'N' TO QT63-IAIND2
           MOVE        014205 TO QT63-NMESAR
           MOVE        ZERO TO WS00-RCOUNT
               GO TO     F50FA-FN.
      *CHECK FOR ONE ACTIVE LOAN BA
                 IF    WS00-LCOUNT > ZERO                               DOT
                 AND   WT12-CPMTC = 01
           MOVE        'N' TO QT63-IAIND2
           MOVE        014207 TO QT63-NMESAR
           MOVE        ZERO TO WS00-LCOUNT
               GO TO     F50FA-FN.
       F50FP-900. GO TO F50FP-A.
       F50FP-FN. EXIT.
       F50FN-FN. EXIT.
      *N50FT.    NOTE *SUITABILITY  PROFILE  IS           *.
       F50FT.                                                           lv15
                 IF    ACCT-SUITABILITY = 'Y'                           DOT
      *INCOMPLETE, CHECK THE INDICATOR
           MOVE        'N' TO QT63-IAIND2
           MOVE        014321 TO QT63-NMESAR
               GO TO     F50FA-FN.
       F50FT-FN. EXIT.
       F50FB-900. GO TO F50FZ-FN.
       F50FB-FN. EXIT.
      *N50FZ.    NOTE *TA98 RULES DO NO ALLOW             *.
       F50FZ.                                                           lv10
      *SAY PRODUCT RULES DO NOT ALLOW
           MOVE        'N' TO QT63-IAIND2
           MOVE        014202 TO QT63-NMESAR
               GO TO     F50FA-FN.
       F50FZ-FN. EXIT.
       F50FA-FN. EXIT.
      *N50JA.    NOTE *SET REG ALLOWED SWITCH IARRGA      *.
       F50JA.                                                           lv07
      *********************************
      *DETERMINE VALID ** RECURRING **
      *PAYMENT TYPES:
      *-  IARRGA "REGULAR"
      *-  IARLNA "LOAN"
      *-  IARCDA "CASH DEPOSIT"
      *-  IARCPA "CASH WITH PURCHASE"
      *****************  ENDS F50PN  **
      *N50JB.    NOTE *SET REG ALLOWED SWITCH IARRGA      *.
       F50JB.    IF    QT63-IARRGA = 'Y'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50JB-FN.
      *N50JC.    NOTE *MULTIPLE REGULARS NOT ALLOWED      *.
       F50JC.                                                           lv15
           MOVE        1                        TO J50JCR
                                    GO TO     F50JC-B.
       F50JC-A.
           ADD         1                        TO J50JCR.
       F50JC-B.
           IF          J50JCR                   >  IACCTL
                                    GO TO     F50JC-FN.
                 IF    J50JCR = 1                                       DOT
           MOVE        ZERO TO WS00-RCOUNT.
                 IF    ACCT-RCOUNT (J50JCR) > 0                         DOT
                 AND   ACCT-ACOUNT (J50JCR) > 0
           ADD         1 TO WS00-RCOUNT.
                 IF    TD98-IMPRA = 'N'                                 DOT
                 AND   WS00-RCOUNT > 0
                 AND   QT63-NMESAA = ZEROS
           MOVE        014235 TO QT63-NMESAA.
                 IF    TD98-IMPRA = 'N'                                 DOT
                 AND   WS00-RCOUNT > 0
           MOVE        'N' TO QT63-IARRGA
           MOVE        014235 TO QT63-NMESA0
           PERFORM     F91GB THRU F91GB-FN
           MOVE        ZERO TO WS00-RCOUNT
               GO TO     F50JA-FN.
       F50JC-900. GO TO F50JC-A.
       F50JC-FN. EXIT.
      *N50JE.    NOTE *ANNUITY SPECIFIC EDITS             *.
       F50JE.    IF    ANNUITY                                          lv15
                 NEXT SENTENCE ELSE GO TO     F50JE-FN.
      *NONE AT THIS TIME
       F50JE-FN. EXIT.
      *N50JH.    NOTE *CERTIFICATE SPECIFIC EDITS         *.
       F50JH.    IF    7-CERTIFICATE-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50JH-FN.
      *NONE AT THIS TIME
       F50JH-FN. EXIT.
      *N50JK.    NOTE *FUNDS SPECIFIC EDITS               *.
       F50JK.    IF    7-MUTUAL-FUND-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50JK-FN.
      *NONE AT THIS TIME
       F50JK-FN. EXIT.
      *N50JN.    NOTE *INSURANCE SPECIFIC EDITS           *.
       F50JN.    IF    7-LIFE-INSURANCE-GROUP                           lv15
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F50JN-FN.
      *SINGLE PAY AND SINGLE PAY WHOLE                                  DOT
      *CANNOT HAVE REGULAR BA'S
                 IF    LIFE-SGLPAY = 'Y'                                DOT
                 OR    LIFE-SGLPAYWHL = 'Y'
           MOVE        'N' TO QT63-IARRGA
           MOVE        014203 TO QT63-NMESA0
               GO TO     F50JA-FN.
      *N50JO.    NOTE *ACCOUNT HAS ONE ACTIVE REG BA      *.
       F50JO.                                                           lv20
           MOVE        1                        TO J50JOR
                                    GO TO     F50JO-B.
       F50JO-A.
           ADD         1                        TO J50JOR.
       F50JO-B.
           IF          J50JOR                   >  IACCTL
                                    GO TO     F50JO-FN.
                 IF    J50JOR = 1                                       DOT
           MOVE        ZERO TO WS00-RCOUNT.
                 IF    ACCT-RCOUNT (J50JOR) > 0                         DOT
                 AND   ACCT-ACOUNT (J50JOR) > 0
           ADD         1 TO WS00-RCOUNT.
                 IF    WS00-RCOUNT > ZERO                               DOT
                 AND   QT63-NMESAA = ZEROS
           MOVE        014205 TO QT63-NMESAA.
                 IF    WS00-RCOUNT > ZERO                               DOT
           MOVE        'N' TO QT63-IARRGA
           MOVE        014205 TO QT63-NMESA0
           PERFORM     F91GB THRU F91GB-FN
           MOVE        ZERO TO WS00-RCOUNT
               GO TO     F50JA-FN.
       F50JO-900. GO TO F50JO-A.
       F50JO-FN. EXIT.
       F50JN-FN. EXIT.
       F50JB-900. GO TO F50JZ-FN.
       F50JB-FN. EXIT.
      *N50JZ.    NOTE *TA98 RULES DO NOT ALLOW            *.
       F50JZ.                                                           lv10
      *SAY PRODUCT RULES DO NOT ALLOW
           MOVE        'N' TO QT63-IARRGA
           MOVE        014203 TO QT63-NMESA0
               GO TO     F50JA-FN.
       F50JZ-FN. EXIT.
       F50JA-FN. EXIT.
      *N50LA.    NOTE *SET LOAN ALLOWED SWITCH IARLNA     *.
       F50LA.         EXIT.                                             lv07
      *N50LB.    NOTE *SET LOAN ALLOWED SWITCH IARLNA     *.
       F50LB.    IF    QT63-IARLNA = 'Y'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50LB-FN.
      *IRA ACCOUNT
                 IF    ACCT-IRA = 'Y'                                   DOT
           MOVE        'N' TO QT63-IARLNA
           MOVE        014206 TO QT63-NMESA1
               GO TO     F50LA-FN.
      *N50LC.    NOTE *MULTIPLE LOANS NOT ALLOWED         *.
       F50LC.                                                           lv15
           MOVE        1                        TO J50LCR
                                    GO TO     F50LC-B.
       F50LC-A.
           ADD         1                        TO J50LCR.
       F50LC-B.
           IF          J50LCR                   >  IACCTL
                                    GO TO     F50LC-FN.
                 IF    J50LCR = 1                                       DOT
           MOVE        ZERO TO WS00-LCOUNT.
                 IF    ACCT-LCOUNT (J50LCR) > 0                         DOT
                 AND   ACCT-ACOUNT (J50LCR) > 0
           ADD         1 TO WS00-LCOUNT.
                 IF    TD98-IMLNA = 'N'                                 DOT
                 AND   WS00-LCOUNT > 0
           MOVE        'N' TO QT63-IARLNA
           MOVE        014207 TO QT63-NMESA1
           MOVE        ZERO TO WS00-LCOUNT
               GO TO     F50LA-FN.
       F50LC-900. GO TO F50LC-A.
       F50LC-FN. EXIT.
      *N50LE.    NOTE *ANNUITY SPECIFIC EDITS             *.
       F50LE.    IF    ANNUITY                                          lv15
                 NEXT SENTENCE ELSE GO TO     F50LE-FN.
      *LOANS FORBIDDED ON ANNUITIES
           MOVE        'N' TO QT63-IARLNA
           MOVE        014204 TO QT63-NMESA1
               GO TO     F50LA-FN.
       F50LE-FN. EXIT.
      *N50LH.    NOTE *CERTIFICATE SPECIFIC EDITS         *.
       F50LH.    IF    7-CERTIFICATE-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50LH-FN.
      *NO LOAN BALANCE ON CERTIFICATE
                 IF    CERT-NOLOANBAL = 'Y'                             DOT
           MOVE        'N' TO QT63-IARLNA
           MOVE        014208 TO QT63-NMESA1
               GO TO     F50LA-FN.
       F50LH-FN. EXIT.
      *N50LK.    NOTE *FUNDS SPECIFIC EDITS               *.
       F50LK.    IF    7-MUTUAL-FUND-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50LK-FN.
      *LOANS FORBIDDEN FOR FUNDS
           MOVE        'N' TO QT63-IARLNA
           MOVE        014204 TO QT63-NMESA1
               GO TO     F50LA-FN.
       F50LK-FN. EXIT.
      *N50LN.    NOTE *INSURANCE SPECIFIC EDITS           *.
       F50LN.    IF    7-LIFE-INSURANCE-GROUP                           lv15
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F50LN-FN.
                 IF    7-PRCOD = 213                                    DOT
      *LOAN PAYMENT NOT ALLOWED FOR
      *TRIO PRODUCT IN MYFA
           MOVE        'N' TO QT63-IARLNA
           MOVE        014204 TO QT63-NMESA1
               GO TO     F50LA-FN.
      *NO LOAN BALANCE
                 IF    LIFE-NOLOAN = 'Y'                                DOT
           MOVE        'N' TO QT63-IARLNA
           MOVE        014208 TO QT63-NMESA1
               GO TO     F50LA-FN.
      *LTC OR DI CAN HAVE NO LOANS
                 IF    7-LONG-TERM-CARE-GROUP                           DOT
                 OR    7-DISABILITY-GROUP
           MOVE        'N' TO QT63-IARLNA
           MOVE        014204 TO QT63-NMESA1
               GO TO     F50LA-FN.
      *NO ACTIVE REGULAR BA'S FOR TRAD
                 IF    LIFE-TRAD-ACCT = 'Y'                             DOT
                 AND   ACCT-BA-ACOUNT = ZERO
           MOVE        'N' TO QT63-IARLNA
           MOVE        014209 TO QT63-NMESA1
               GO TO     F50LA-FN.
      *1 ACTIVE LOAN BA EXISTS ALREADY
                 IF    ACCT-BA-LCOUNT > ZERO                            DOT
           MOVE        'N' TO QT63-IARLNA
           MOVE        014207 TO QT63-NMESA1
               GO TO     F50LA-FN.
       F50LN-FN. EXIT.
       F50LB-900. GO TO F50LZ-FN.
       F50LB-FN. EXIT.
      *N50LZ.    NOTE *TA98 RULES DO NOT ALLOW            *.
       F50LZ.                                                           lv10
      *SAY PRODUCT RULES DO NOT ALLOW
           MOVE        'N' TO QT63-IARLNA
           MOVE        014204 TO QT63-NMESA1
               GO TO     F50LA-FN.
       F50LZ-FN. EXIT.
       F50LA-FN. EXIT.
      *N50NA.    NOTE *SET CASH DEPOSIT SWITCH IARCDA     *.
       F50NA.         EXIT.                                             lv07
      *N50NE.    NOTE *ANNUITY SPECIFIC EDITS             *.
       F50NE.    IF    ANNUITY                                          lv15
                 NEXT SENTENCE ELSE GO TO     F50NE-FN.
      *CD FORBIDDED ON ANNUITIES
           MOVE        'N' TO QT63-IARCDA
           MOVE        014339 TO QT63-NMESA2
               GO TO     F50NA-FN.
       F50NE-FN. EXIT.
      *N50NH.    NOTE *CERTIFICATE SPECIFIC EDITS         *.
       F50NH.    IF    7-CERTIFICATE-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50NH-FN.
      *CD FORBIDDEN ON CERTIFICATE
           MOVE        'N' TO QT63-IARCDA
           MOVE        014339 TO QT63-NMESA2
               GO TO     F50NA-FN.
       F50NH-FN. EXIT.
      *N50NK.    NOTE *FUNDS SPECIFIC EDITS               *.
       F50NK.    IF    7-MUTUAL-FUND-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50NK-FN.
      *CD FORBIDDEN FOR FUNDS
           MOVE        'N' TO QT63-IARCDA
           MOVE        014339 TO QT63-NMESA2
               GO TO     F50NA-FN.
       F50NK-FN. EXIT.
      *N50NN.    NOTE *INSURANCE SPECIFIC EDITS           *.
       F50NN.    IF    7-LIFE-INSURANCE-GROUP                           lv15
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F50NN-FN.
      *CD FORBIDDEN ON INSURANCE
           MOVE        'N' TO QT63-IARCDA
           MOVE        014339 TO QT63-NMESA2
               GO TO     F50NA-FN.
       F50NN-FN. EXIT.
       F50NA-FN. EXIT.
      *N50PA.    NOTE *SET CASH WITH PURCH SW IARCPA      *.
       F50PA.         EXIT.                                             lv07
      *N50PE.    NOTE *ANNUITY SPECIFIC EDITS             *.
       F50PE.    IF    ANNUITY                                          lv15
                 NEXT SENTENCE ELSE GO TO     F50PE-FN.
      *CWP FORBIDDED ON ANNUITIES
           MOVE        'N' TO QT63-IARCPA
           MOVE        014340 TO QT63-NMESA3
               GO TO     F50PA-FN.
       F50PE-FN. EXIT.
      *N50PH.    NOTE *CERTIFICATE SPECIFIC EDITS         *.
       F50PH.    IF    7-CERTIFICATE-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50PH-FN.
      *CWP FORBIDDEN ON CERTIFICATE
           MOVE        'N' TO QT63-IARCPA
           MOVE        014340 TO QT63-NMESA3
               GO TO     F50PA-FN.
       F50PH-FN. EXIT.
      *N50PK.    NOTE *FUNDS SPECIFIC EDITS               *.
       F50PK.    IF    7-MUTUAL-FUND-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50PK-FN.
      *CWP FORBIDDEN FOR FUNDS
           MOVE        'N' TO QT63-IARCPA
           MOVE        014340 TO QT63-NMESA3
               GO TO     F50PA-FN.
       F50PK-FN. EXIT.
      *N50PN.    NOTE *INSURANCE SPECIFIC EDITS           *.
       F50PN.    IF    7-LIFE-INSURANCE-GROUP                           lv15
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F50PN-FN.
      *CWP FORBIDDEN ON INSURANCE
           MOVE        'N' TO QT63-IARCPA
           MOVE        014340 TO QT63-NMESA3
               GO TO     F50PA-FN.
       F50PN-FN. EXIT.
       F50PA-FN. EXIT.
      *N50RA.    NOTE *SET OD REG ALLOWED SWITCH IARRG1   *.
       F50RA.                                                           lv07
      *********************************
      *DETERMINE VALID ** ON DEMAND **
      *PAYMENT TYPES:
      *-  IARRG1 "REGULAR"
      *-  IARLN1 "LOAN"
      *-  IARCD1 "CASH DEPOSIT"
      *-  IARCP1 "CASH WITH PURCHASE"
      *****************  ENDS F50UN  **
      *N50RB.    NOTE *SET OD REG ALLOWED SWITCH IARRG1   *.
       F50RB.    IF    TD98-IARRGA = 'Y'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50RB-FN.
      *N50RE.    NOTE *ANNUITY SPECIFIC EDITS             *.
       F50RE.    IF    ANNUITY                                          lv15
                 NEXT SENTENCE ELSE GO TO     F50RE-FN.
      *
      *ONE REG OD ALLOWED
      *INACTIVES DISABLE PAYMENT TYPE
           PERFORM     F91HC THRU F91HC-FN.
                 IF    QT63-IARRG1 = 'N'                                DOT
               GO TO     F50RA-FN.
       F50RE-FN. EXIT.
      *N50RH.    NOTE *CERTIFICATE SPECIFIC EDITS         *.
       F50RH.    IF    7-CERTIFICATE-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50RH-FN.
      *
      *ONE REG OD ALLOWED
      *INACTIVES DISABLE PAYMENT TYPE
           PERFORM     F91HC THRU F91HC-FN.
                 IF    QT63-IARRG1 = 'N'                                DOT
               GO TO     F50RA-FN.
       F50RH-FN. EXIT.
      *N50RK.    NOTE *FUNDS SPECIFIC EDITS               *.
       F50RK.    IF    7-MUTUAL-FUND-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50RK-FN.
      *
      *ONE REG OD ALLOWED
      *INACTIVES DISABLE PAYMENT TYPE
           PERFORM     F91HC THRU F91HC-FN.
                 IF    QT63-IARRG1 = 'N'                                DOT
               GO TO     F50RA-FN.
       F50RK-FN. EXIT.
      *N50RN.    NOTE *INSURANCE SPECIFIC EDITS           *.
       F50RN.    IF    7-LIFE-INSURANCE-GROUP                           lv15
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F50RN-FN.
      *DISABLE ON DEMAND ACH-IN FOR                                     DOT
      *INSURANCE ACCOUNT
      *REMOVE TO RE-ENABLE
           MOVE        'N' TO QT63-IARRG1
           MOVE        14203 TO QT63-NMESA4
               GO TO     F50RA-FN.
      *SINGLE PAY AND SINGLE PAY WHOLE                                  DOT
      *CANNOT HAVE REGULAR BA'S
                 IF    LIFE-SGLPAY = 'Y'                                DOT
                 OR    LIFE-SGLPAYWHL = 'Y'
           MOVE        'N' TO QT63-IARRG1
           MOVE        014203 TO QT63-NMESA4
               GO TO     F50RA-FN.
      *ONE REG OD ALLOWED                                               DOT
      *INACTIVES DISABLE PAYMENT TYPE
           PERFORM     F91HC THRU F91HC-FN.
                 IF    QT63-IARRG1 = 'N'                                DOT
               GO TO     F50RA-FN.
       F50RN-FN. EXIT.
       F50RB-900. GO TO F50RZ-FN.
       F50RB-FN. EXIT.
      *N50RZ.    NOTE *TA98 RULES DO NOT ALLOW            *.
       F50RZ.                                                           lv10
      *SAY PRODUCT RULES DO NOT ALLOW
           MOVE        'N' TO QT63-IARRG1
           MOVE        014203 TO QT63-NMESA4
               GO TO     F50RA-FN.
       F50RZ-FN. EXIT.
       F50RA-FN. EXIT.
      *N50SA.    NOTE *SET OD LOAN SWITCH IARLN1          *.
       F50SA.         EXIT.                                             lv07
      *N50SB.    NOTE *SET OD LOAN SWITCH IARLN1          *.
       F50SB.    IF    TD98-IARLNA = 'Y'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50SB-FN.
      *IRA ACCOUNT
                 IF    ACCT-IRA = 'Y'                                   DOT
           MOVE        'N' TO QT63-IARLN1
           MOVE        014206 TO QT63-NMESA5
               GO TO     F50SA-FN.
      *N50SE.    NOTE *ANNUITY SPECIFIC EDITS             *.
       F50SE.    IF    ANNUITY                                          lv15
                 NEXT SENTENCE ELSE GO TO     F50SE-FN.
      *LOANS FORBIDDEN ON ANNUITIES
           MOVE        'N' TO QT63-IARLN1
           MOVE        014204 TO QT63-NMESA5
               GO TO     F50SA-FN.
       F50SE-FN. EXIT.
      *N50SH.    NOTE *CERTIFICATE SPECIFIC EDITS         *.
       F50SH.    IF    7-CERTIFICATE-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50SH-FN.
      *NO LOAN BALANCE ON CERTIFICATE
                 IF    CERT-NOLOANBAL = 'Y'                             DOT
           MOVE        'N' TO QT63-IARLN1
           MOVE        014208 TO QT63-NMESA5
               GO TO     F50SA-FN.
      *ONE LOAN OD ALLOWED
      *INACTIVES DISABLE PAYMENT TYPE
           PERFORM     F91HF THRU F91HF-FN.
                 IF    QT63-IARLN1 = 'N'                                DOT
               GO TO     F50SA-FN.
       F50SH-FN. EXIT.
      *N50SK.    NOTE *FUNDS SPECIFIC EDITS               *.
       F50SK.    IF    7-MUTUAL-FUND-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50SK-FN.
      *LOANS FORBIDDEN FOR FUNDS
           MOVE        'N' TO QT63-IARLN1
           MOVE        014204 TO QT63-NMESA5
               GO TO     F50SA-FN.
       F50SK-FN. EXIT.
      *N50SN.    NOTE *INSURANCE SPECIFIC EDITS           *.
       F50SN.    IF    7-LIFE-INSURANCE-GROUP                           lv15
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F50SN-FN.
      *DISABLE ON DEMAND ACH-IN FOR                                     DOT
      *INSURANCE ACCOUNT
      *REMOVE TO RE-ENABLE, BUT TRIO
      *PRODUCT WILL BE AN EXCEPTION.
           MOVE        'N' TO QT63-IARLN1
           MOVE        014204 TO QT63-NMESA5
               GO TO     F50SA-FN.
      *NO LOAN BALANCE                                                  DOT
                 IF    LIFE-NOLOAN = 'Y'                                DOT
           MOVE        'N' TO QT63-IARLN1
           MOVE        014208 TO QT63-NMESA5
               GO TO     F50SA-FN.
      *LTC OR DI CAN HAVE NO LOANS                                      DOT
                 IF    7-LONG-TERM-CARE-GROUP                           DOT
                 OR    7-DISABILITY-GROUP
           MOVE        'N' TO QT63-IARLN1
           MOVE        014204 TO QT63-NMESA5
               GO TO     F50SA-FN.
      *NO ACTIVE REGULAR BA'S FOR TRAD                                  DOT
                 IF    LIFE-TRAD-ACCT = 'Y'                             DOT
                 AND   ACCT-BA-ACOUNT = ZERO
                 AND   ACCT-99-ACOUNT = ZERO
           MOVE        'N' TO QT63-IARLN1
           MOVE        014209 TO QT63-NMESA5
               GO TO     F50SA-FN.
      *ONE LOAN OD ALLOWED                                              DOT
      *INACTIVES DISABLE PAYMENT TYPE
           PERFORM     F91HF THRU F91HF-FN.
                 IF    QT63-IARLN1 = 'N'                                DOT
               GO TO     F50SA-FN.
       F50SN-FN. EXIT.
       F50SB-900. GO TO F50SZ-FN.
       F50SB-FN. EXIT.
      *N50SZ.    NOTE *TA98 RULES DO NOT ALLOW            *.
       F50SZ.                                                           lv10
      *SAY PRODUCT RULES DO NOT ALLOW
           MOVE        'N' TO QT63-IARLN1
           MOVE        014204 TO QT63-NMESA5
               GO TO     F50SA-FN.
       F50SZ-FN. EXIT.
       F50SA-FN. EXIT.
      *N50TA.    NOTE *SET OD CD SWITCH IARCD1            *.
       F50TA.         EXIT.                                             lv07
      *N50TE.    NOTE *ANNUITY SPECIFIC EDITS             *.
       F50TE.    IF    ANNUITY                                          lv15
                 NEXT SENTENCE ELSE GO TO     F50TE-FN.
      *CD FORBIDDEN ON ANNUITIES
           MOVE        'N' TO QT63-IARCD1
           MOVE        014339 TO QT63-NMESA6
               GO TO     F50TA-FN.
       F50TE-FN. EXIT.
      *N50TH.    NOTE *CERTIFICATE SPECIFIC EDITS         *.
       F50TH.    IF    7-CERTIFICATE-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50TH-FN.
      *CD FORBIDDEN ON CERTIFICATE
           MOVE        'N' TO QT63-IARCD1
           MOVE        014339 TO QT63-NMESA6
               GO TO     F50TA-FN.
       F50TH-FN. EXIT.
      *N50TK.    NOTE *FUNDS SPECIFIC EDITS               *.
       F50TK.    IF    7-MUTUAL-FUND-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50TK-FN.
      *CD FORBIDDEN FOR FUNDS
           MOVE        'N' TO QT63-IARCD1
           MOVE        014339 TO QT63-NMESA6
               GO TO     F50TA-FN.
       F50TK-FN. EXIT.
      *N50TN.    NOTE *INSURANCE SPECIFIC EDITS           *.
       F50TN.    IF    7-LIFE-INSURANCE-GROUP                           lv15
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F50TN-FN.
      *CD FORBIDDEN ON INSURANCE
           MOVE        'N' TO QT63-IARCD1
           MOVE        014339 TO QT63-NMESA6
               GO TO     F50TA-FN.
       F50TN-FN. EXIT.
       F50TA-FN. EXIT.
      *N50UA.    NOTE *SET OD CWP SWITCH IARCP1           *.
       F50UA.                                                           lv07
      *ON DEMAND CASH WITH PURCHASE IS
      *NEVER ALLOWED
      *N50UE.    NOTE *ANNUITY SPECIFIC EDITS             *.
       F50UE.    IF    ANNUITY                                          lv15
                 NEXT SENTENCE ELSE GO TO     F50UE-FN.
      *CWP FORBIDDEN ON ANNUITIES
           MOVE        'N' TO QT63-IARCP1
           MOVE        014340 TO QT63-NMESA7
               GO TO     F50UA-FN.
       F50UE-FN. EXIT.
      *N50UH.    NOTE *CERTIFICATE SPECIFIC EDITS         *.
       F50UH.    IF    7-CERTIFICATE-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50UH-FN.
      *CWP FORBIDDEN ON CERTIFICATE
           MOVE        'N' TO QT63-IARCP1
           MOVE        014340 TO QT63-NMESA7
               GO TO     F50UA-FN.
       F50UH-FN. EXIT.
      *N50UK.    NOTE *FUNDS SPECIFIC EDITS               *.
       F50UK.    IF    7-MUTUAL-FUND-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F50UK-FN.
      *CWP FORBIDDEN FOR FUNDS
           MOVE        'N' TO QT63-IARCP1
           MOVE        014340 TO QT63-NMESA7
               GO TO     F50UA-FN.
       F50UK-FN. EXIT.
      *N50UN.    NOTE *INSURANCE SPECIFIC EDITS           *.
       F50UN.    IF    7-LIFE-INSURANCE-GROUP                           lv15
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F50UN-FN.
      *CWP FORBIDDEN ON INSURANCE
           MOVE        'N' TO QT63-IARCP1
           MOVE        014340 TO QT63-NMESA7
               GO TO     F50UA-FN.
       F50UN-FN. EXIT.
       F50UA-FN. EXIT.
      *N50WA.    NOTE *SET SPECIFIC ADD ALLOWED IAINDA    *.
       F50WA.         EXIT.                                             lv07
      *N50WB.    NOTE *SET SPECIFIC ADD ALLOWED IAINDA    *.
       F50WB.                                                           lv10
                 IF    WS00-NOCX12 = 'N'                                DOT
      *THE ENTIRE KEY WAS PASSED
           MOVE        'N' TO QT63-IAINDA
           MOVE        ZERO TO QT63-NAPDSK
               GO TO     F50WB-FN.
      *POSSIBLE DUPLICATE WAS DETECTED
                 IF    WS00-DUPCX12 = 'Y'                               DOT
           MOVE        'N' TO QT63-IAINDA
           MOVE        ZERO TO QT63-NAPDSK
               GO TO     F50WB-FN.
           MOVE        'Y' TO QT63-IAINDA.                              DOT
      *N50WC.    NOTE *SET NAPDSK FOR RECURRING           *.
       F50WC.    IF    QT63-CPMTF NOT = 99                              lv15
                 NEXT SENTENCE ELSE GO TO     F50WC-FN.
      *N50WF.    NOTE *LOOP TO FIND OPEN SEQUENCE         *.
       F50WF.                                                           lv20
           MOVE        IBAPDM                   TO J50WFR
                                    GO TO     F50WF-B.
       F50WF-A.
           SUBTRACT 1                         FROM J50WFR.
       F50WF-B.
           IF          J50WFR                   <  1
                                    GO TO     F50WF-FN.
                 IF    BAPD-BA-COUNT (J50WFR) = 0                       DOT
           MOVE        J50WFR TO QT63-NAPDSK.
       F50WF-900. GO TO F50WF-A.
       F50WF-FN. EXIT.
       F50WC-FN. EXIT.
      *N50WH.    NOTE *SET NAPDSK FOR ON DEMAND           *.
       F50WH.    IF    QT63-CPMTF = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F50WH-FN.
           MOVE        99 TO QT63-NAPDSK.
       F50WH-FN. EXIT.
       F50WB-FN. EXIT.
       F50WA-FN. EXIT.
       F50-FN.   EXIT.
      *N51.      NOTE *************************************.
      *               *                                   *
      *               *ADJUST SWITCHES BASED ON OTHERS    *
      *               *                                   *
      *               *************************************.
       F51.           EXIT.                                             lv05
      *N51BB.    NOTE *ADJUST ADD SWITCH IF NEEDED        *.
       F51BB.    IF    QT63-IARTYA = 'Y'                                lv10
                 NEXT SENTENCE ELSE GO TO     F51BB-FN.
      *N51BF.    NOTE *EVALUATE PAYMENT TYPE ALLOWED      *.
       F51BF.    IF    WS00-NOCX12 = 'Y'                                lv15
                 NEXT SENTENCE ELSE GO TO     F51BF-FN.
      *FLAGS TO DETERMIE WHAT
      *FREQUENCIES ARE AVAILABLE FOR
      *ADD.
      *
      *ALL RECURRING PAYMENT TYPES
      *ARE OFF, DISABLE RECURR FREQS
                 IF    QT63-IARRGA = 'N'                                DOT
                 AND   QT63-IARLNA = 'N'
                 AND   QT63-IARCDA = 'N'
                 AND   QT63-IARCPA = 'N'
           PERFORM     F91GB THRU F91GB-FN.
      *ALL OD PAYMENT TYPES                                             DOT
      *ARE OFF, DISABLE OD FREQUENCY
                 IF    QT63-IARRG1 = 'N'                                DOT
                 AND   QT63-IARLN1 = 'N'
                 AND   QT63-IARCD1 = 'N'
                 AND   QT63-IARCP1 = 'N'
           MOVE        'N' TO QT63-IFQOD.
       F51BF-FN. EXIT.
      *N51BH.    NOTE *NO PMNT TYPES REMAIN VALID         *.
       F51BH.    IF    (QT63-IARRGA = 'N'                               lv15
                 AND   QT63-IARLNA = 'N'
                 AND   QT63-IARCDA = 'N'
                 AND   QT63-IARCPA = 'N'
                 AND   QT63-IARRG1 = 'N'
                 AND   QT63-IARLN1 = 'N'
                 AND   QT63-IARCD1 = 'N'
                 AND   QT63-IARCP1 = 'N')
                 OR    (QT63-IFQAN = 'N'
                 AND   QT63-IFQSA = 'N'
                 AND   QT63-IFQQT = 'N'
                 AND   QT63-IFQBM = 'N'
                 AND   QT63-IFQMO = 'N'
                 AND   QT63-IFQSM = 'N'
                 AND   QT63-IFQBW = 'N'
                 AND   QT63-IFQWK = 'N'
                 AND   QT63-IFQOD = 'N')
                 NEXT SENTENCE ELSE GO TO     F51BH-FN.
      *OR NO FREQUENCIES REMAIN VALID
      *N51BN.    NOTE *TURN OFF ADD WITH CORRECT MSG.     *.
       F51BN.    IF    (QT63-IARRGA = 'N'                               lv20
                 AND   QT63-IARLNA = 'N'
                 AND   QT63-IARCDA = 'N'
                 AND   QT63-IARCPA = 'N')
                 OR    (QT63-IFQAN = 'N'
                 AND   QT63-IFQSA = 'N'
                 AND   QT63-IFQQT = 'N'
                 AND   QT63-IFQBM = 'N'
                 AND   QT63-IFQMO = 'N'
                 AND   QT63-IFQSM = 'N'
                 AND   QT63-IFQBW = 'N'
                 AND   QT63-IFQWK = 'N')
                 NEXT SENTENCE ELSE GO TO     F51BN-FN.
      *IF IN RECURRING FLOW
                 IF    QT63-NMESA0 = ZERO                               DOT
           MOVE        WS00-NMESA TO QT63-NMESA0.
                 IF    QT63-NMESA1 = ZERO                               DOT
           MOVE        WS00-NMESA TO QT63-NMESA1.
                 IF    QT63-NMESA2 = ZERO                               DOT
           MOVE        WS00-NMESA TO QT63-NMESA2.
                 IF    QT63-NMESA3 = ZERO                               DOT
           MOVE        WS00-NMESA TO QT63-NMESA3.
      *                                                                 DOT
      *IF RECURRING REGULAR APPLIES,
      *GIVE THE REASON IT WAS TURNED
      *OFF
                 IF    TD98-IARRGA = 'Y'                                DOT
                 AND   QT63-NMESA0 NOT = ZEROS
           MOVE        QT63-NMESA0 TO QT63-NMESAA.
      *                                                                 DOT
                 IF    QT63-NMESA0 = ZEROS                              DOT
                 AND   QT63-NMESA2 = ZEROS
      *ADD NOT POSSIBLE
      *SHOULD NOT GET HERE, BUT JUST
      *IN CASE
           MOVE        014202 TO QT63-NMESAA.
       F51BN-FN. EXIT.
      *N51BP.    NOTE *TURN OFF ADD WITH CORRECT MSG      *.
       F51BP.    IF    (QT63-IARRG1 = 'N'                               lv20
                 AND   QT63-IARLN1 = 'N'
                 AND   QT63-IARCD1 = 'N'
                 AND   QT63-IARCP1 = 'N')
                 OR    QT63-IFQOD = 'N'
                 NEXT SENTENCE ELSE GO TO     F51BP-FN.
      *IF IN ONE-TIME FLOW
                 IF    QT63-NMESA4 = ZERO                               DOT
           MOVE        WS00-NMESA1 TO QT63-NMESA4.
                 IF    QT63-NMESA6 = ZERO                               DOT
           MOVE        WS00-NMESA1 TO QT63-NMESA6.
      *                                                                 DOT
      *IF ONE-TIME REGULAR APPLIES,
      *GIVE THE REASON IT WAS TURNED
      *OFF
                 IF    TD98-IARRGA = 'Y'                                DOT
                 AND   QT63-NMESA4 NOT = ZEROS
           MOVE        QT63-NMESA4 TO QT63-NMESA.
      *                                                                 DOT
                 IF    QT63-NMESA4 = ZEROS                              DOT
                 AND   QT63-NMESA6 = ZEROS
      *ADD NOT POSSIBLE
      *SHOULD NOT GET HERE, BUT JUST
      *IN CASE
           MOVE        014202 TO QT63-NMESA.
       F51BP-FN. EXIT.
       F51BH-FN. EXIT.
       F51BB-FN. EXIT.
      *N51DB.    NOTE *ADD W/SPECIFIED KEY IMPOSSIBLE     *.
       F51DB.    IF    QT63-IARTYA = 'N'                                lv10
                 AND   QT63-IAINDA = 'Y'
                 NEXT SENTENCE ELSE GO TO     F51DB-FN.
           MOVE        'N' TO QT63-IAINDA
           MOVE        ZERO TO QT63-NAPDSK.
       F51DB-FN. EXIT.
       F51-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *LOAD OUTPUT AREA AMT, FREQ         *
      *               *                                   *
      *               *************************************.
       F55.      IF    QT63-IARTYA = 'Y'                                lv05
                 OR    QT63-IACHI = 'Y'
                 OR    QT63-IAIND2 = 'Y'
                 NEXT SENTENCE ELSE GO TO     F55-FN.
      *SOMETHING MUST BE ALLOWED FOR
      *ONE TO WANT THE VALID RANGES
      *ADD, CHG, REACTIVATE
      *N55BB.    NOTE *SET BA DETAIL STATUS CDEST1        *.
       F55BB.                                                           lv10
      *THIS IS WHAT THE BA DETAIL
      *SHOULD GET IF A BA IS ADDED
      *N55BE.    NOTE *LIFE PRODUCT                       *.
       F55BE.    IF    CT01-CTIDA = 004 OR 005                          lv15
                 NEXT SENTENCE ELSE GO TO     F55BE-FN.
      *N55BH.    NOTE *ACCT STATUS ACTIVE                 *.
       F55BH.    IF    CT01-CTSTA = 2                                   lv20
                 OR    (CT01-CTSTA = 1 AND
                       (TC8A-CLAST = 201
                       OR TC8A-CLAST = 202
                       OR TC8A-CLAST = 203
                       OR TC8A-CLAST = 205
                       OR TC8A-CLAST = 204
                       OR (TC8A-CLAST = 206
                       OR TC8A-CLAST = 207
                       OR TC8A-CLAST = 208)))
                 NEXT SENTENCE ELSE GO TO     F55BH-FN.
      *OR PENDING (IF RAVA)
      *RAVA
      *RAVA2
      *CRAVA
      *RAVA-SELECT
      *RAVA-REFRESH
      *NOTE CTSTA = 2 IS ACTIVE WHEREAS
      *CDEST = 2 IS PENDING.
      *SET BA DETAIL STATUS TO ACTIVE
           MOVE        1 TO QT63-CDEST1.
       F55BH-900. GO TO F55BK-FN.
       F55BH-FN. EXIT.
      *N55BK.    NOTE *ACCT STATUS NOT ACTIVE             *.
       F55BK.                                                           lv20
      *AND NOT PENDING (IF RAVA)
      *(INACTIVES WILL NOT ALLOW ADDS)
      *SET BA DETAIL STATUS TO PENDING
           MOVE        2 TO QT63-CDEST1.
       F55BK-FN. EXIT.
       F55BE-900. GO TO F55BN-FN.
       F55BE-FN. EXIT.
      *N55BN.    NOTE *ALL OTHER PRODUCTS ACTIVE          *.
       F55BN.                                                           lv15
      *OTHERS ARE CERTS, ACTIVE, PEND
      *FUNDS ACTIVE, PEND AND INACTIVE
      *BROKERAGE ACTIVE
           MOVE        1 TO QT63-CDEST1.
       F55BN-FN. EXIT.
       F55BB-FN. EXIT.
      *N55DB.    NOTE *RESET ANY FREQUENCIES BASED ON     *.
       F55DB.                                                           lv10
      *THE OST RULES
      *N55DE.    NOTE *ANNUITY SPECIFIC EDITS             *.
       F55DE.    IF    ANNUITY                                          lv15
                 NEXT SENTENCE ELSE GO TO     F55DE-FN.
      *NO SPECIFIC RULES
       F55DE-FN. EXIT.
      *N55DH.    NOTE *CERTIFICATE SPECIFIC EDITS         *.
       F55DH.    IF    7-CERTIFICATE-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F55DH-FN.
      *NO SPECIFIC RULES
       F55DH-FN. EXIT.
      *N55DK.    NOTE *FUNDS SPECIFIC EDITS               *.
       F55DK.    IF    7-MUTUAL-FUND-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F55DK-FN.
      *NO SPECIFIC RULES
       F55DK-FN. EXIT.
      *N55DN.    NOTE *INSURANCE SPECIFIC EDITS           *.
       F55DN.    IF    7-LIFE-INSURANCE-GROUP                           lv15
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F55DN-FN.
      *ONLY ALLOWED FREQUENCIES FOR OST
      *ARE MONTHLY AND QUARTERLY
      *TURN OFF THE REST
           MOVE        'N' TO QT63-IFQAN
           QT63-IFQSA
           QT63-IFQBM
           QT63-IFQSM
           QT63-IFQBW
           QT63-IFQWK.
      *FOR TRAD DUE DATE MUST BE                                        DOT
      *A QUARTERLY DATE
                 IF    LIFE-TRAD-ACCT = 'Y'                             DOT
           MOVE        WS00-QT-OK TO QT63-IFQQT.
       F55DN-FN. EXIT.
       F55DB-FN. EXIT.
      *N55HB.    NOTE *SET THE MIN LOAN AMT ALMIN         *.
       F55HB.                                                           lv10
      *DEFAULT IS 100; THEN ADJUST BY
      *PRODUCT
           MOVE        100 TO QT63-ALMIN
           QT63-ALMIN3.
      *N55HE.    NOTE *ANNUITY SPECIFIC EDITS             *.
       F55HE.    IF    ANNUITY                                          lv15
                 NEXT SENTENCE ELSE GO TO     F55HE-FN.
      *LOANS NOT ALLOWED
       F55HE-FN. EXIT.
      *N55HH.    NOTE *CERTIFICATE SPECIFIC EDITS         *.
       F55HH.    IF    7-CERTIFICATE-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F55HH-FN.
           MOVE        50 TO QT63-ALMIN
           QT63-ALMIN3.
       F55HH-FN. EXIT.
      *N55HK.    NOTE *FUNDS SPECIFIC EDITS               *.
       F55HK.    IF    7-MUTUAL-FUND-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F55HK-FN.
      *LOANS NOT ALLOWED
       F55HK-FN. EXIT.
      *N55HN.    NOTE *INSURANCE SPECIFIC EDITS           *.
       F55HN.    IF    7-LIFE-INSURANCE-GROUP                           lv15
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F55HN-FN.
           MOVE        20 TO QT63-ALMIN
           QT63-ALMIN3.
       F55HN-FN. EXIT.
       F55HB-FN. EXIT.
      *N55JB.    NOTE *SET TRADITIONAL INSURANCE AMTS     *.
       F55JB.    IF    LIFE-TRAD-ACCT = 'Y'                             lv10
                 NEXT SENTENCE ELSE GO TO     F55JB-FN.
           MOVE        ACCT-MONTHLY TO QT63-ALPAGM
           MOVE        ACCT-QUARTERLY TO QT63-ALPAGQ.
       F55JB-FN. EXIT.
      *N55LB.    NOTE *SET INSURANCE FIELDS               *.
       F55LB.    IF    7-LIFE-INSURANCE-GROUP                           lv10
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F55LB-FN.
           MOVE        LIFE-ALDDUE TO QT63-ALDDUE
           MOVE        LIFE-DGRAC TO QT63-DGRAC
           MOVE        LIFE-ALPLDT TO QT63-ALPLDT.
       F55LB-FN. EXIT.
      *N55NB.    NOTE *SET START AND END DATE RANGES      *.
       F55NB.                                                           lv10
           MOVE        WS00-GESTE TO QT63-GESTE
           QT63-GEENE
           MOVE        WS00-GEENL TO QT63-GESTL
           QT63-GEENL
           MOVE        WS00-GESTD1 TO QT63-GESTD1.
       F55NB-FN. EXIT.
      *N55PB.    NOTE *SET THE SKIP DATE DSKIP            *.
       F55PB.    IF    WS00-NOCX12 = 'N'                                lv10
                 NEXT SENTENCE ELSE GO TO     F55PB-FN.
      *SET THE CURRENT CX12 BASED FLDS
           MOVE        WS00-DSKIP1 TO QT63-DSKIP1
           MOVE        WS00-DSKIP2 TO QT63-DSKIP2
           MOVE        WS00-DSKIP TO QT63-DSKIP.
       F55PB-FN. EXIT.
      *N55SB.    NOTE *SET THE REGULAR MINIMUM AMOUNTS    *.
       F55SB.                                                           lv10
      *NOTE: THE ORDER OF THESE STMTS
      *DICTATES THE SORT ORDER ON THE
      *WEB PAGE. KNOW WHAT YOU ARE
      *MESSING WITH
           MOVE        QT63-AMAXAL TO WS00-ACOTU1.
                 IF    QT63-IFQMO = 'Y'                                 DOT
           MOVE        12 TO WS00-CPMTG1
           MOVE        'Monthly' TO WS00-MPMTF1
           MOVE        ACCT-MONTHLY TO WS00-ACOTL1
           PERFORM     F91DB THRU F91DB-FN.
                 IF    QT63-IFQQT = 'Y'                                 DOT
           MOVE        04 TO WS00-CPMTG1
           MOVE        'Quarterly' TO WS00-MPMTF1
           MOVE        ACCT-QUARTERLY TO WS00-ACOTL1
           PERFORM     F91DB THRU F91DB-FN.
                 IF    QT63-IFQBM = 'Y'                                 DOT
           MOVE        06 TO WS00-CPMTG1
           MOVE        'Bi-monthly' TO WS00-MPMTF1
           MOVE        ACCT-BIMONTHLY TO WS00-ACOTL1
           PERFORM     F91DB THRU F91DB-FN.
                 IF    QT63-IFQBW = 'Y'                                 DOT
           MOVE        26 TO WS00-CPMTG1
           MOVE        'Bi-weekly' TO WS00-MPMTF1
           MOVE        ACCT-BIWEEKLY TO WS00-ACOTL1
           PERFORM     F91DB THRU F91DB-FN.
                 IF    QT63-IFQSM = 'Y'                                 DOT
           MOVE        24 TO WS00-CPMTG1
           MOVE        'Semi-monthly' TO WS00-MPMTF1
           MOVE        ACCT-SEMIMONTHLY TO WS00-ACOTL1
           PERFORM     F91DB THRU F91DB-FN.
                 IF    QT63-IFQWK = 'Y'                                 DOT
           MOVE        52 TO WS00-CPMTG1
           MOVE        'Weekly' TO WS00-MPMTF1
           MOVE        ACCT-WEEKLY TO WS00-ACOTL1
           PERFORM     F91DB THRU F91DB-FN.
                 IF    QT63-IFQSA = 'Y'                                 DOT
           MOVE        02 TO WS00-CPMTG1
           MOVE        'Semi-annual' TO WS00-MPMTF1
           MOVE        ACCT-SEMIANNUAL TO WS00-ACOTL1
           PERFORM     F91DB THRU F91DB-FN.
                 IF    QT63-IFQAN = 'Y'                                 DOT
           MOVE        01 TO WS00-CPMTG1
           MOVE        'Annual' TO WS00-MPMTF1
           MOVE        ACCT-ANNUAL TO WS00-ACOTL1
           PERFORM     F91DB THRU F91DB-FN.
                 IF    QT63-IFQOD = 'Y'                                 DOT
           MOVE        99 TO WS00-CPMTG1
           PERFORM     F91SB THRU F91SB-FN
           MOVE        QT63-AMAXA2 TO WS00-ACOTU1
           MOVE        'On demand        ' TO
           WS00-MPMTF1
           MOVE        ACCT-OD TO WS00-ACOTL1
           PERFORM     F91DB THRU F91DB-FN.
      *N55SE.    NOTE *TRADITIONAL INSURANCE              *.
       F55SE.    IF    LIFE-TRAD-ACCT = 'Y'                             lv15
                 NEXT SENTENCE ELSE GO TO     F55SE-FN.
      *THESE HAVE EXACT AMTS;
      *1 AND 2 ARE RECURRING
      *3 IS ON DEMAND
           MOVE        QT63-ACOTL1 TO QT63-ACOTU1
           MOVE        QT63-ACOTL2 TO QT63-ACOTU2.
                 IF    QT63-MPMTF3 = 'On demand'                        DOT
           MOVE        QT63-ACOTL3 TO QT63-ACOTU3.
       F55SE-FN. EXIT.
       F55SB-FN. EXIT.
      *N55UB.    NOTE *SET THE RECURRING PAYMENT TYPES    *.
       F55UB.                                                           lv10
      *FOR WEB DROP DOWN
                 IF    QT63-IARRGA = 'Y'                                DOT
           MOVE        'REGULAR' TO WS00-MPMTT1
           PERFORM     F91FB THRU F91FB-FN.
                 IF    QT63-IARLNA = 'Y'                                DOT
           MOVE        'LOAN' TO WS00-MPMTT1
           PERFORM     F91FB THRU F91FB-FN.
                 IF    QT63-IARCDA = 'Y'                                DOT
           MOVE        'CASH DEPOSIT' TO WS00-MPMTT1
           PERFORM     F91FB THRU F91FB-FN.
                 IF    QT63-IARCPA = 'Y'                                DOT
           MOVE        'CASH WITH PURCHASE' TO
           WS00-MPMTT1
           PERFORM     F91FB THRU F91FB-FN.
       F55UB-FN. EXIT.
      *N55UD.    NOTE *SET THE ON DEMAND PAYMENT TYPES    *.
       F55UD.    IF    QT63-IFQOD = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F55UD-FN.
      *FOR WEB DROP DOWN
                 IF    QT63-IARRG1 = 'Y'                                DOT
           MOVE        'REGULAR' TO WS00-MPMTT1
           PERFORM     F91FD THRU F91FD-FN.
                 IF    QT63-IARLN1 = 'Y'                                DOT
           MOVE        'LOAN' TO WS00-MPMTT1
           PERFORM     F91FD THRU F91FD-FN.
                 IF    QT63-IARCD1 = 'Y'                                DOT
           MOVE        'CASH DEPOSIT' TO WS00-MPMTT1
           PERFORM     F91FD THRU F91FD-FN.
                 IF    QT63-IARCP1 = 'Y'                                DOT
           MOVE        'CASH WITH PURCHASE' TO
           WS00-MPMTT1
           PERFORM     F91FD THRU F91FD-FN.
       F55UD-FN. EXIT.
      *N55WB.    NOTE *ACTIVE ALLOWED; INACTIVE ALLOWED   *.
       F55WB.    IF    WS00-NOCX12 = 'N'                                lv10
                 NEXT SENTENCE ELSE GO TO     F55WB-FN.
      *SWITCHES
           MOVE        'N' TO QT63-IABAA
           QT63-IIBAA
      *IF EITHER A CHANGE IS ALLOWED
      *(MEANS IT CAN BE INACTIVATED)
      *OR REACTIVATE IS ALLOWED,
      *(MEANS IT IS NOW INACTIVE)
      *INACTIVATE BUTTON MUST BE
      *ENABLED
                 IF    QT63-IACHI = 'Y'                                 DOT
                 OR    QT63-IAIND2 = 'Y'
           MOVE        'Y' TO QT63-IIBAA.
      *IF CHANGE IS ALLOWED AND IT IS                                   DOT
      *NOT PENDING (MUST BE ACTIVE NOW)
      *OR IT CAN BE REACTIVATED (TO
      *BECOME ACTIVE), THEN ACTIVE
      *BUTTON MUST BE ENABLED
                 IF    (QT63-IACHI = 'Y'                                DOT
                 AND   WT12-CDEST NOT = 2)
                 OR    QT63-IAIND2 = 'Y'
           MOVE        'Y' TO QT63-IABAA.
       F55WB-FN. EXIT.
      *N55YB.    NOTE *SET CPCCD1 VALUE                   *.
       F55YB.         EXIT.                                             lv10
      *N55YE.    NOTE *ANNUITY                            *.
       F55YE.    IF    ANNUITY                                          lv15
                 NEXT SENTENCE ELSE GO TO     F55YE-FN.
           MOVE        30000 TO QT63-CPCCD1.
       F55YE-FN. EXIT.
      *N55YH.    NOTE *CERTIFICATE                        *.
       F55YH.    IF    7-CERTIFICATE-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F55YH-FN.
           MOVE        10000 TO QT63-CPCCD1.
       F55YH-FN. EXIT.
      *N55YK.    NOTE *FUNDS                              *.
       F55YK.    IF    7-MUTUAL-FUND-PRODUCT                            lv15
                 NEXT SENTENCE ELSE GO TO     F55YK-FN.
           MOVE        20000 TO QT63-CPCCD1.
       F55YK-FN. EXIT.
      *N55YN.    NOTE *INSURANCE                          *.
       F55YN.    IF    7-LIFE-INSURANCE-GROUP                           lv15
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F55YN-FN.
                 IF    LIFE-TRAD-ACCT = 'Y'                             DOT
           MOVE        41000 TO QT63-CPCCD1.
                 IF    LIFE-UL-ACCT = 'Y'                               DOT
           MOVE        42000 TO QT63-CPCCD1.
       F55YN-FN. EXIT.
       F55YB-FN. EXIT.
       F55-FN.   EXIT.
      *N56.      NOTE *************************************.
      *               *                                   *
      *               *SPILL OVER FROM F55.               *
      *               *                                   *
      *               *************************************.
       F56.      IF    QT63-IARTYA = 'Y'                                lv05
                 OR    QT63-IACHI = 'Y'
                 OR    QT63-IAIND2 = 'Y'
                 NEXT SENTENCE ELSE GO TO     F56-FN.
      *SOMETHING MUST BE ALLOWED FOR
      *ONE TO WANT THE VALID RANGES
      *ADD, CHG, REACTIVATE
      *N56BB.    NOTE *SET MORE STUFF                     *.
       F56BB.         EXIT.                                             lv10
      *N56BE.    NOTE *INSURANCE                          *.
       F56BE.    IF    7-LIFE-INSURANCE-GROUP                           lv15
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP
                 NEXT SENTENCE ELSE GO TO     F56BE-FN.
      *N56BH.    NOTE *TRAD DIRECT BILL INDICATORS        *.
       F56BH.    IF    LIFE-TRAD-ACCT = 'Y'                             lv20
                 NEXT SENTENCE ELSE GO TO     F56BH-FN.
           MOVE        WS00-AN-OK TO QT63-IDBAN
           MOVE        WS00-SA-OK TO QT63-IDBSA
           MOVE        WS00-QT-OK TO QT63-IDBQT.
                 IF    (LIFE-TERM = 'Y'                                 DOT
                 AND   WE40-COLPL = 486 OR 488)
                 OR    (LIFE-DI = 'Y'
                 AND   WE40-CRTBK > 18)
           MOVE        'N' TO QT63-IDBMO
                 ELSE
           MOVE        'Y' TO QT63-IDBMO.
                 IF    QT63-IDBQT = 'N'                                 DOT
           MOVE        'Y' TO QT63-IDBMO.
       F56BH-FN. EXIT.
      *N56BK.    NOTE *UL DIRECT BILL INDICATORS          *.
       F56BK.    IF    LIFE-UL-ACCT = 'Y'                               lv20
                 NEXT SENTENCE ELSE GO TO     F56BK-FN.
           MOVE        'Y' TO QT63-IDBAN
           QT63-IDBSA
           QT63-IDBQT
           QT63-IDBMO.
                 IF    LIFE-LPPLUS = 'Y'                                DOT
                 OR    LIFE-V2D = 'Y'
                 OR    LIFE-V2D2 = 'Y'
                 OR    LIFE-VULIII = 'Y'
                 OR    LIFE-VUL4 = 'Y'
                 OR    LIFE-F2D = 'Y'
           MOVE        'N' TO QT63-IDBMO.
       F56BK-FN. EXIT.
      *N56BN.    NOTE *LOAN TYPES DONT GET THIS SET       *.
       F56BN.    IF    WS00-NOCX12 = 'N'                                lv20
                 AND   WT12-CPMTC = 01
                 NEXT SENTENCE ELSE GO TO     F56BN-FN.
           MOVE        'N' TO QT63-IDBAN
           QT63-IDBSA
           QT63-IDBQT
           QT63-IDBMO.
       F56BN-FN. EXIT.
       F56BE-FN. EXIT.
       F56BB-FN. EXIT.
       F56-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *SET UNCONDITIONAL OUTPUT FIELDS    *
      *               *                                   *
      *               *************************************.
       F60.           EXIT.                                             lv05
      *N60BB.    NOTE *SET CT01 BASED FIELDS              *.
       F60BB.                                                           lv10
      *DIRAC1  IRA CUTOFF MONTH NUMBER
      *CIRAT   IRA TYPE CODE
      *CIRAS   IRA STATUS CODE
      *CQACT   QUALIFIED ACCT TYPE
      *PRCOD   PRODUCT CODE
                 IF    ACCT-IRA = 'Y'                                   DOT
                 AND   ((CT01-CIRAT = 001
                 OR    CT01-CIRAT = 005
                 OR    CT01-CIRAT = 006)
                 OR    (CT01-CIRAT = 007
                 AND   QT63-DCACG > 20020430))
           MOVE        CT01-DIRAC TO WS00-DIRAC
           MOVE        WS00-NIRACM TO QT63-DIRAC1
                 ELSE
           MOVE        99 TO QT63-DIRAC1.
           MOVE        CT01-CIRAT TO QT63-CIRAT                         DOT
           MOVE        CT01-CIRAS TO QT63-CIRAS
           MOVE        CT01-CQACT TO QT63-CQACT
           MOVE        CT01-PRCOD TO QT63-PRCOD.
       F60BB-FN. EXIT.
      *N60DB.    NOTE *PRODUCT CATEGORIZATION CODE        *.
       F60DB.                                                           lv10
           MOVE        WS00-CPCCDE TO QT63-CPCCDE.
       F60DB-FN. EXIT.
      *N60FB.    NOTE *OWNERSHIP FROM CI0083              *.
       F60FB.                                                           lv10
           MOVE        PF1F-OUTPUT TO QT63-OWNOUT.
       F60FB-FN. EXIT.
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
      *N80.      NOTE *************************************.
      *               *                                   *
      *               *MACRO AAOLBB CODE IS HERE          *
      *               *                                   *
      *               *************************************.
       F80.                                                             lv05
      *WITH APPROPRIATE MODIFICATIONS
      *N80BB.    NOTE *CHECK FOR FLORIDA DI POLICIES      *.
       F80BB.                                                           lv10
      *LOAD WS AND RESET SWITCHES
           MOVE        WE40-ALPAGR TO 7-82-PREMIUMS
           MOVE        7-82-ONE TO 7-82-ONES
           MOVE        'N' TO 7-82-FL-DI
      *SET SWITCH WHICH IS USED IN
      *LATER CALCULATIONS TO NOT CHARGE
      *MODAL PREMIUMS FOR FLORIDA DI
           MOVE        CT01-CTIDND TO 7-82-ALCIDN
           MOVE        CT01-PRSCD TO 7-82-PRSCD.
                 IF    WE40-ALAPST = 15                                 DOT
                 AND   7-82-FIRST-4 = 9100
                 AND   7-82-DE-CODE = 22
                 AND   ((7-82-FGH-CODE = 160)
                 OR    (7-82-FGH-CODE < 140
                 OR    7-82-FGH-CODE > 184))
      *SET FLORIDA DI SWITCH
           MOVE        'Y' TO 7-82-FL-DI.
                 IF    CT01-PRCOD = 300                                 DOT
                 AND   (7-82-FIRST-4 = 9000
                       OR 9700)
                 AND   7-82-DE-CODE = 19
                 AND   (7-82-FGH-CODE = 313
                       OR 316 OR 318)
      *SET 2012 NEW TERM SWITCH
           MOVE        'Y' TO 7-82-TL-2012
                 ELSE
      *IF NOT 2012 NEW TERM
           MOVE        'N' TO 7-82-TL-2012.
      *N80BE.    NOTE *CHECK POLICY DATE FOR FORMULA      *.
       F80BE.    IF    WE40-ALPLDT > 19661002                           lv20
                 NEXT SENTENCE ELSE GO TO     F80BE-FN.
           MOVE        ZERO TO AA85-CF
           MOVE        7-82-ALCIDN TO S-AAU10-ALCIDN
           PERFORM     F94FA THRU F94FA-FN.
                 IF    IK = ZERO                                        DOT
           MOVE        '1' TO AA85-CF.
      *N80BH.    NOTE *NEW CALCULATION  - FORMULA         *.
       F80BH.    IF    AA85-CF = '1'                                    lv25
                 AND   WE40-CPCAL = 1
                 NEXT SENTENCE ELSE GO TO     F80BH-FN.
      *MATCH LIC0025 LOGIC
      *DON'T CHARGE PREMIUM FOR FL DI
                 IF    7-82-FL-DI = 'Y'                                 DOT
           COMPUTE     ACCT-MONTHLY ROUNDED =
           (WE40-ALPAGR * 0.0868)
                 ELSE
           COMPUTE     ACCT-MONTHLY ROUNDED =
           ((WE40-ALPAGR * 0.0868) + 0.60).
                 IF    7-82-FL-DI = 'Y'                                 DOT
           COMPUTE     ACCT-QUARTERLY ROUNDED =
           (WE40-ALPAGR * 0.258)
                 ELSE
           COMPUTE     ACCT-QUARTERLY ROUNDED =
           ((WE40-ALPAGR * 0.258) + 0.50).
                 IF    7-82-FL-DI = 'Y'                                 DOT
           COMPUTE     ACCT-SEMIANNUAL ROUNDED =
           (WE40-ALPAGR * 0.502)
                 ELSE
           COMPUTE     ACCT-SEMIANNUAL ROUNDED =
           ((WE40-ALPAGR * 0.502) + 0.40).
                 IF    CT01-PRCOD = 310                                 DOT
                 OR    7-82-TL-2012 = 'Y'
      *FOR IPE INSURED PRODUCTS
      *AND NEW TERM 2012
      *DON'T CHARGE PREMIUM FOR FL DI
           PERFORM     F80LB THRU F80LB-FN.
       F80BH-FN. EXIT.
      *N80BK.    NOTE *OLD CALCULATION - TABLE            *.
       F80BK.    IF    AA85-CF = ZERO                                   lv25
                 OR    WE40-CPCAL NOT = 1
                 NEXT SENTENCE ELSE GO TO     F80BK-FN.
      *MATCH LIC0025 LOGIC
      *MONTHLY
           COMPUTE     7-82-TENS =
           (WE40-ALPAGR - 7-82-ONE)
           COMPUTE     7-82-PREMIUM-T ROUNDED =
           (7-82-TENS * 0.0868)
           COMPUTE     7-82-PREMIUM-O ROUNDED =
           (7-82-CONES * 0.0868)
           COMPUTE     ACCT-MONTHLY =
           ((7-82-PREMIUM-T
           + 7-82-PREMIUM-O) + 0.60)
      *QUARTERLY
           COMPUTE     7-82-PREMIUM-T ROUNDED =
           (7-82-TENS * 0.258)
           COMPUTE     7-82-PREMIUM-O ROUNDED =
           (7-82-CONES * 0.258)
           COMPUTE     ACCT-QUARTERLY =
           ((7-82-PREMIUM-T
           + 7-82-PREMIUM-O) + 0.50)
      *SEMI-ANNUAL
           COMPUTE     7-82-PREMIUM-T ROUNDED =
           (7-82-TENS * 0.502)
           COMPUTE     7-82-PREMIUM-O ROUNDED =
           (7-82-CONES * 0.502)
           COMPUTE     ACCT-SEMIANNUAL =
           ((7-82-PREMIUM-T
           + 7-82-PREMIUM-O) + 0.40).
                 IF    CT01-PRCOD = 310                                 DOT
                 OR    7-82-TL-2012 = 'Y'
      *FOR IPE INSURED PRODUCTS
      *AND NEW TERM 2012
           PERFORM     F80LB THRU F80LB-FN.
       F80BK-FN. EXIT.
       F80BE-900. GO TO F80BN-FN.
       F80BE-FN. EXIT.
      *N80BN.    NOTE *USE OLD PREMIUM CALC FORMULA       *.
       F80BN.                                                           lv20
      *MATCH LIC0025 LOGIC
           COMPUTE     ACCT-QUARTERLY ROUNDED =
           (WE40-ALPAGR * 0.260)
           COMPUTE     ACCT-MONTHLY ROUNDED =
           (WE40-ALPAGR * 0.0875)
           COMPUTE     ACCT-SEMIANNUAL ROUNDED =
           (WE40-ALPAGR * 0.510).
       F80BN-FN. EXIT.
       F80BB-FN. EXIT.
      *N80DB.    NOTE *CHECK IF NEXT PAYMENT DATE IS      *.
       F80DB.                                                           lv10
      *IN SYNC WITH THE DATE ALLOWED
      *FOR A GIVEN FREQUENCY
      *
      *CHECK IF QUARTERLY IS OK
           MOVE        WE40-ALDDUE TO 7-82-WORK-DATE
           MOVE        WE40-ALPLDT TO 7-82-ANN-DATE
           MOVE        'N' TO WS00-OK
           MOVE        3 TO WS00-DIV
           PERFORM     F80FB THRU F80FB-FN
           MOVE        WS00-OK TO WS00-QT-OK
      *CHECK IF SEMI-ANNUAL IS OK
           MOVE        WE40-ALDDUE TO 7-82-WORK-DATE
           MOVE        WE40-ALPLDT TO 7-82-ANN-DATE
           MOVE        'N' TO WS00-OK
           MOVE        6 TO WS00-DIV
           PERFORM     F80FB THRU F80FB-FN
           MOVE        WS00-OK TO WS00-SA-OK
      *CHECK IF ANNUAL IS OK
           MOVE        WE40-ALDDUE TO 7-82-WORK-DATE
           MOVE        WE40-ALPLDT TO 7-82-ANN-DATE
           MOVE        'N' TO WS00-OK.
                 IF    7-82-WORK-MM = 7-82-ANN-MM                       DOT
           MOVE        'Y' TO WS00-OK.
           MOVE        WS00-OK TO WS00-AN-OK.                           DOT
       F80DB-FN. EXIT.
      *N80FB.    NOTE *CHECK IF MONTHS DON'T MATCH        *.
       F80FB.                                                           lv10
      *INPUT FLDS  7-82-WORK-DATE
      *            7-82-ANN-DATE
      *&                WS00-DIV
                 IF    7-82-WORK-MM > 7-82-ANN-MM                       DOT
           SUBTRACT    7-82-ANN-MM FROM 7-82-WORK-MM
           DIVIDE      WS00-DIV INTO 7-82-WORK-MM GIVING
           7-82-RESULT REMAINDER 7-82-REM
                 ELSE
           SUBTRACT    7-82-WORK-MM FROM 7-82-ANN-MM
           DIVIDE      WS00-DIV INTO 7-82-ANN-MM GIVING
           7-82-RESULT REMAINDER 7-82-REM.
                 IF    7-82-REM = ZERO                                  DOT
           MOVE        'Y' TO WS00-OK.
       F80FB-FN. EXIT.
      *N80HB.    NOTE *ADD ONE MONTH TO THE WORK DATE     *.
       F80HB.                                                           lv10
      *INPUT & OUTPUT IN 7-82-WORK-DATE
           ADD         1 TO 7-82-WORK-MM.
      *N80HD.    NOTE *IF CHANGE YEAR, FIX DAY UP         *.
       F80HD.    IF    7-82-WORK-MM = 13                                lv25
                 NEXT SENTENCE ELSE GO TO     F80HD-FN.
      *
           MOVE        01 TO 7-82-WORK-MM
           ADD         1 TO 7-82-WORK-YY.
                 IF    7-82-WORK-YY = ZERO                              DOT
           ADD         1 TO 7-82-WORK-CC.
       F80HD-FN. EXIT.
       F80HB-FN. EXIT.
      *N80LB.    NOTE *CALCULATION  - FORMULA             *.
       F80LB.                                                           lv10
      *MATCH MACRO AAOLBB LOGIC CHANGE
      *ONLY FOR IPE INSURED PRODUCTS
           COMPUTE     ACCT-MONTHLY ROUNDED =
           (WE40-ALPAGR * 0.0875)
           COMPUTE     ACCT-QUARTERLY ROUNDED =
           (WE40-ALPAGR * 0.2625)
           COMPUTE     ACCT-SEMIANNUAL ROUNDED =
           (WE40-ALPAGR * 0.515).
       F80LB-FN. EXIT.
       F80-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N91.      NOTE *************************************.
      *               *                                   *
      *               *SOME PERFORMED FUNCTIONS           *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91BB.    NOTE *SET IK ON ERROR                    *.
       F91BB.                                                           lv10
           MOVE        '1' TO IK.
       F91BB-FN. EXIT.
      *N91CB.    NOTE *CHECK FOR DUPLICATE BA DETAIL      *.
       F91CB.    IF    QT63-APMTL NUMERIC                               lv10
                 AND   QT63-CPMTF NUMERIC
                 NEXT SENTENCE ELSE GO TO     F91CB-FN.
      *START DATE, TYPE, AMOUNT AND                                     DOT
      *FREQUENCY AND STATUS THEN
      *IT IS A DUP
                 IF    QT63-APMTL = WT12-APMTL                          DOT
                 AND   QT63-CPMTF = WT12-CPMTF
                 AND   QT63-CPMTC = WT12-CPMTC
                 AND   QT63-GESTD = WT12-DNPMT
                 AND   WT12-CDEST = (01 OR 02)
           MOVE        'Y' TO WS00-DUPCX12.
       F91CB-FN. EXIT.
      *N91DB.    NOTE *LOAD NEXT AVAILABLE FREQ SLOT      *.
       F91DB.                                                           lv10
                 IF    QT63-CPMTG1 = ZERO                               DOT
           MOVE        WS00-CPMTG1 TO QT63-CPMTG1
           MOVE        WS00-MPMTF1 TO QT63-MPMTF1
           MOVE        WS00-ACOTL1 TO QT63-ACOTL1
           MOVE        WS00-ACOTU1 TO QT63-ACOTU1
               GO TO     F91DB-FN.
                 IF    QT63-CPMTG2 = ZERO                               DOT
           MOVE        WS00-CPMTG1 TO QT63-CPMTG2
           MOVE        WS00-MPMTF1 TO QT63-MPMTF2
           MOVE        WS00-ACOTL1 TO QT63-ACOTL2
           MOVE        WS00-ACOTU1 TO QT63-ACOTU2
               GO TO     F91DB-FN.
                 IF    QT63-CPMTG3 = ZERO                               DOT
           MOVE        WS00-CPMTG1 TO QT63-CPMTG3
           MOVE        WS00-MPMTF1 TO QT63-MPMTF3
           MOVE        WS00-ACOTL1 TO QT63-ACOTL3
           MOVE        WS00-ACOTU1 TO QT63-ACOTU3
               GO TO     F91DB-FN.
                 IF    QT63-CPMTG4 = ZERO                               DOT
           MOVE        WS00-CPMTG1 TO QT63-CPMTG4
           MOVE        WS00-MPMTF1 TO QT63-MPMTF4
           MOVE        WS00-ACOTL1 TO QT63-ACOTL4
           MOVE        WS00-ACOTU1 TO QT63-ACOTU4
               GO TO     F91DB-FN.
                 IF    QT63-CPMTG5 = ZERO                               DOT
           MOVE        WS00-CPMTG1 TO QT63-CPMTG5
           MOVE        WS00-MPMTF1 TO QT63-MPMTF5
           MOVE        WS00-ACOTL1 TO QT63-ACOTL5
           MOVE        WS00-ACOTU1 TO QT63-ACOTU5
               GO TO     F91DB-FN.
                 IF    QT63-CPMTG6 = ZERO                               DOT
           MOVE        WS00-CPMTG1 TO QT63-CPMTG6
           MOVE        WS00-MPMTF1 TO QT63-MPMTF6
           MOVE        WS00-ACOTL1 TO QT63-ACOTL6
           MOVE        WS00-ACOTU1 TO QT63-ACOTU6
               GO TO     F91DB-FN.
                 IF    QT63-CPMTG7 = ZERO                               DOT
           MOVE        WS00-CPMTG1 TO QT63-CPMTG7
           MOVE        WS00-MPMTF1 TO QT63-MPMTF7
           MOVE        WS00-ACOTL1 TO QT63-ACOTL7
           MOVE        WS00-ACOTU1 TO QT63-ACOTU7
               GO TO     F91DB-FN.
                 IF    QT63-CPMTG8 = ZERO                               DOT
           MOVE        WS00-CPMTG1 TO QT63-CPMTG8
           MOVE        WS00-MPMTF1 TO QT63-MPMTF8
           MOVE        WS00-ACOTL1 TO QT63-ACOTL8
           MOVE        WS00-ACOTU1 TO QT63-ACOTU8
               GO TO     F91DB-FN.
                 IF    QT63-CPMTG9 = ZERO                               DOT
           MOVE        WS00-CPMTG1 TO QT63-CPMTG9
           MOVE        WS00-MPMTF1 TO QT63-MPMTF9
           MOVE        WS00-ACOTL1 TO QT63-ACOTL9
           MOVE        WS00-ACOTU1 TO QT63-ACOTU9.
       F91DB-FN. EXIT.
      *N91FB.    NOTE *LOAD NEXT AVAILABLE RECURRING      *.
       F91FB.                                                           lv10
      *PAYMENT TYPE SLOT.  MPMTT1, 2
      *CARRY ALLOWED RECURR FREQS.
      *MPMTT3, 4 WILL CARRY ALLOWED
      *OD FREQS.
      *
                 IF    QT63-MPMTT1 = SPACES                             DOT
           MOVE        WS00-MPMTT1 TO QT63-MPMTT1
               GO TO     F91FB-FN.
                 IF    QT63-MPMTT2 = SPACES                             DOT
           MOVE        WS00-MPMTT1 TO QT63-MPMTT2
               GO TO     F91FB-FN.
       F91FB-FN. EXIT.
      *N91FD.    NOTE *LOAD NEXT AVAILABLE ON DEMAND      *.
       F91FD.                                                           lv10
      *PAYMENT TYPE SLOT.  (SEE NOTE
      *ABOVE).
                 IF    QT63-MPMTT3 = SPACES                             DOT
           MOVE        WS00-MPMTT1 TO QT63-MPMTT3
               GO TO     F91FD-FN.
                 IF    QT63-MPMTT4 = SPACES                             DOT
           MOVE        WS00-MPMTT1 TO QT63-MPMTT4
               GO TO     F91FD-FN.
       F91FD-FN. EXIT.
      *N91GB.    NOTE *DISABLE ALL RECURRING              *.
       F91GB.                                                           lv10
      *FREQUENCIES
           MOVE        'N' TO QT63-IFQAN
           QT63-IFQSA
           QT63-IFQQT
           QT63-IFQBM
           QT63-IFQMO
           QT63-IFQSM
           QT63-IFQBW
           QT63-IFQWK
      *DISABLE TRAD LIFE FLAGS
           MOVE        'N' TO WS00-QT-OK
           WS00-SA-OK
           WS00-AN-OK.
       F91GB-FN. EXIT.
      *N91HC.    NOTE *ONE ON DEMAND REGULAR ALLOWED.     *.
       F91HC.                                                           lv10
           MOVE        1                        TO J91HCR
                                    GO TO     F91HC-B.
       F91HC-A.
           ADD         1                        TO J91HCR.
       F91HC-B.
           IF          J91HCR                   >  IBAODL
                                    GO TO     F91HC-FN.
      *INACTIVE DISABLES OD REG.
                 IF    J91HCR = 1                                       DOT
           MOVE        ZERO TO WS00-ACOUNT
           WS00-ICOUNT.
                 IF    BAOD-99-RCOUNT (J91HCR) > 0                      DOT
                 AND   BAOD-99-ACOUNT (J91HCR) > 0
           ADD         1 TO WS00-ACOUNT.
                 IF    BAOD-99-RCOUNT (J91HCR) > 0                      DOT
                 AND   BAOD-99-ICOUNT (J91HCR) > 0
           ADD         1 TO WS00-ICOUNT.
      *NO REG BECAUSE ALREADY EXISTS                                    DOT
                 IF    WS00-ACOUNT > 0                                  DOT
           MOVE        'N' TO QT63-IARRG1
           MOVE        014345 TO QT63-NMESA4
           QT63-NMESA.
      *NO REG BECAUSE INACTIVE EXISTS                                   DOT
                 IF    WS00-ICOUNT > 0                                  DOT
           MOVE        'N' TO QT63-IARRG1
           MOVE        014347 TO QT63-NMESA4
           QT63-NMESA.
       F91HC-900. GO TO F91HC-A.
       F91HC-FN. EXIT.
      *N91HF.    NOTE *ONE ON DEMAND LOAN ALLOWED.        *.
       F91HF.                                                           lv10
           MOVE        1                        TO J91HFR
                                    GO TO     F91HF-B.
       F91HF-A.
           ADD         1                        TO J91HFR.
       F91HF-B.
           IF          J91HFR                   >  IBAODL
                                    GO TO     F91HF-FN.
      *INACTIVE DISABLES OD LOAN.
                 IF    J91HFR = 1                                       DOT
           MOVE        ZERO TO WS00-ACOUNT
           WS00-ICOUNT.
                 IF    BAOD-99-LCOUNT (J91HFR) > 0                      DOT
                 AND   BAOD-99-ACOUNT (J91HFR) > 0
           ADD         1 TO WS00-ACOUNT.
                 IF    BAOD-99-LCOUNT (J91HFR) > 0                      DOT
                 AND   BAOD-99-ICOUNT (J91HFR) > 0
           ADD         1 TO WS00-ICOUNT.
      *NO LOAN BECAUSE ALREADY EXISTS                                   DOT
                 IF    WS00-ACOUNT > 0                                  DOT
           MOVE        'N' TO QT63-IARLN1
           MOVE        014345 TO QT63-NMESA5.
      *NO LOAN BECAUSE INACTIVE EXISTS                                  DOT
                 IF    WS00-ICOUNT > 0                                  DOT
           MOVE        'N' TO QT63-IARLN1
           MOVE        014347 TO QT63-NMESA5.
       F91HF-900. GO TO F91HF-A.
       F91HF-FN. EXIT.
      *N91HH.    NOTE *ONE ON DEMAND CASH DEPOSIT         *.
       F91HH.                                                           lv10
           MOVE        1                        TO J91HHR
                                    GO TO     F91HH-B.
       F91HH-A.
           ADD         1                        TO J91HHR.
       F91HH-B.
           IF          J91HHR                   >  IBAODL
                                    GO TO     F91HH-FN.
      *ALLOWED.
      *INACTIVE DISABLES OD CD.
                 IF    J91HHR = 1                                       DOT
           MOVE        ZERO TO WS00-ACOUNT
           WS00-ICOUNT.
                 IF    BAOD-99-CCOUNT (J91HHR) > 0                      DOT
                 AND   BAOD-99-ACOUNT (J91HHR) > 0
           ADD         1 TO WS00-ACOUNT.
                 IF    BAOD-99-CCOUNT (J91HHR) > 0                      DOT
                 AND   BAOD-99-ICOUNT (J91HHR) > 0
           ADD         1 TO WS00-ICOUNT.
      *NO CD BECAUSE ALREADY EXISTS                                     DOT
                 IF    WS00-ACOUNT > 0                                  DOT
           MOVE        'N' TO QT63-IARCD1
           MOVE        014345 TO QT63-NMESA6
           QT63-NMESA.
      *NO CD BECAUSE INACTIVE EXISTS                                    DOT
                 IF    WS00-ICOUNT > 0                                  DOT
           MOVE        'N' TO QT63-IARCD1
           MOVE        014347 TO QT63-NMESA6
           QT63-NMESA.
       F91HH-900. GO TO F91HH-A.
       F91HH-FN. EXIT.
      *N91SB.    NOTE *OVERWRITE THE ON-DEMAND MAXIMUM    *.
       F91SB.                                                           lv10
                 IF    ((7-LIFE-INSURANCE-GROUP                         DOT
                 OR    7-LONG-TERM-CARE-GROUP
                 OR    7-DISABILITY-GROUP)
                 AND   LIFE-TRAD-ACCT NOT = 'Y')
                 OR    (ANNUITY
                 AND   TB5B-IVANT = 'Y')
      *FOR NON-TRAD LIFE ACCOUNTS
      *THE MAXIMUM AMOUNT IS 99,999.99
           MOVE        99999.99 TO QT63-AMAXA2.
       F91SB-FN. EXIT.
       F91-FN.   EXIT.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *DATE AND PACTABLE ACCESS MODULE    *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
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
      *N92NA.    NOTE *CALCULATE NEXT ACCOUNTING DATE     *.
       F92NA.                                                           lv10
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
       F92NA-FN. EXIT.
      *N92TA.    NOTE *RANDOM TABLE READ FOR TA5A         *.            ADUTAB
       F92TA.                                                           lv10
           MOVE        'R1' TO G-TA5A-TABFO                             ADUTAB
           COMPUTE     G-TA5A-LTH = 60 + G-TA5A-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA5A-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA5A)                                ADUTAB
                       LENGTH (G-TA5A-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA5A-TABCR NOT = '00'                          DOT
           PERFORM     F91BB THRU F91BB-FN.                             ADUTAB
       F92TA-FN. EXIT.
      *N92TB.    NOTE *RANDOM TABLE READ FOR TB5B         *.            ADUTAB
       F92TB.                                                           lv10
           MOVE        'R1' TO G-TB5B-TABFO                             ADUTAB
           COMPUTE     G-TB5B-LTH = 60 + G-TB5B-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TB5B-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TB5B)                                ADUTAB
                       LENGTH (G-TB5B-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TB5B-TABCR NOT = '00'                          DOT
           PERFORM     F91BB THRU F91BB-FN.                             ADUTAB
       F92TB-FN. EXIT.
      *N92TC.    NOTE *RANDOM TABLE READ FOR TC8A         *.            ADUTAB
       F92TC.                                                           lv10
           MOVE        'R1' TO G-TC8A-TABFO                             ADUTAB
           COMPUTE     G-TC8A-LTH = 60 + G-TC8A-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TC8A-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TC8A)                                ADUTAB
                       LENGTH (G-TC8A-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TC8A-TABCR NOT = '00'                          DOT
      *NO ERROR OVERRIDE
           INITIALIZE  TC8A.
       F92TC-FN. EXIT.
      *N92TD.    NOTE *RANDOM TABLE READ FOR TD98         *.            ADUTAB
       F92TD.                                                           lv10
           MOVE        'R1' TO G-TD98-TABFO                             ADUTAB
           COMPUTE     G-TD98-LTH = 60 + G-TD98-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TD98-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TD98)                                ADUTAB
                       LENGTH (G-TD98-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TD98-TABCR NOT = '00'                          DOT
           PERFORM     F91BB THRU F91BB-FN.                             ADUTAB
       F92TD-FN. EXIT.
      *N92VA.    NOTE *DATE VALIDATION                    *.            AADA56
       F92VA.                                                           lv10
           MOVE        1 TO DEL-ER.                                     AADA56
                 IF    DD01-XDATG NOT NUMERIC                           DOT
           MOVE        4 TO DEL-ER                                      AADA56
               GO TO     F92VA-FN.                                      AADA56
                 IF    DD01-XDAT1 > '99'                                DOT
                 OR    DD01-XDAT1 < '18'                                AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F92VA-FN.                                      AADA56
                 IF    DD01-XDAT3 > '12'                                DOT
                 OR    DD01-XDAT3 = '00'                                AADA56
                 OR    DD01-XDAT4 > '31'                                AADA56
                 OR    DD01-XDAT4 = '00'                                AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F92VA-FN.                                      AADA56
                 IF    DD01-XDAT4 > '30'                                DOT
                 AND   (DD01-XDAT3 = '04'                               AADA56
                 OR    DD01-XDAT3 = '06'                                AADA56
                 OR    DD01-XDAT3 = '09'                                AADA56
                 OR    DD01-XDAT3 = '11')                               AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F92VA-FN.                                      AADA56
                 IF    DD01-XDAT3 NOT = '02'                            DOT
               GO TO     F92VA-FN.                                      AADA56
                 IF    DD01-XDAT4 > '29'                                DOT
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F92VA-FN.                                      AADA56
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
               GO TO     F92VA-FN.                                      AADA56
       F92VA-FN. EXIT.
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
      *               *DATABASE ACCESS MODULE             *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94AA.    NOTE *CALL GU ON CX2Y                    *.            ADU026
       F94AA.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XK06 CX2Y                                                    ADU026
           S-CXU2Y-SSA                                                  ADU026
           MOVE        XK06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94AA-FN. EXIT.
      *N94AB.    NOTE *CALL GN ON CX2Y                    *.            ADU026
       F94AB.                                                           lv10
           MOVE        'ARAY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX2Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XK06 CX2Y                                                    ADU026
           S-CX2Y-SSA                                                   ADU026
           MOVE        XK06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94AB-FN. EXIT.
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
      *N94DB.    NOTE *CALL GU ON CX01                    *.            ADU026
       F94DB.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XJ06 CX01                                                    ADU026
           S-CXU01-SSA                                                  ADU026
           MOVE        XJ06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DB-FN. EXIT.
      *N94DC.    NOTE *CALL GU ON CX03                    *.            ADU026
       F94DC.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XJ06 CX03                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           MOVE        XJ06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DC-FN. EXIT.
      *N94DF.    NOTE *CALL GU ON CX12                    *.            ADU026
       F94DF.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XJ06 CX12                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU12-SSA
           MOVE        XJ06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DF-FN. EXIT.
      *N94DG.    NOTE *CALL GN ON CX12                    *.            ADU026
       F94DG.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XJ06 CX12                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CX12-SSA
           MOVE        XJ06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DG-FN. EXIT.
      *N94DH.    NOTE *CALL GU ON CX13                    *.            ADU026
       F94DH.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XJ06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           MOVE        XJ06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DH-FN. EXIT.
      *N94DI.    NOTE *CALL GN ON CX13                    *.            ADU026
       F94DI.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XJ06 CX13                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CX13-SSA
           MOVE        XJ06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DI-FN. EXIT.
      *N94DJ.    NOTE *CALL GU ON CX14                    *.            ADU026
       F94DJ.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX14' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XJ06 CX14                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           S-CXU14-SSA
           MOVE        XJ06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DJ-FN. EXIT.
      *N94DK.    NOTE *CALL GN ON CX14                    *.            ADU026
       F94DK.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX14' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XJ06 CX14                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU13-SSA
           S-CX14-SSA
           MOVE        XJ06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DK-FN. EXIT.
      *N94DL.    NOTE *CALL GU ON CX18                    *.            ADU026
       F94DL.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XJ06 CX18                                                    ADU026
           S-CXU01-SSA S-CXU18-SSA                                      ADU026
           MOVE        XJ06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DL-FN. EXIT.
      *N94DT.    NOTE *CALL GU ON CX06                    *.            ADU026
       F94DT.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XJ06 CX06                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA
           MOVE        XJ06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94DT-FN. EXIT.
      *N94EA.    NOTE *CALL GU ON CX6Y                    *.            ADU026
       F94EA.                                                           lv10
           MOVE        'AREY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX6Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XN06 CX6Y                                                    ADU026
           S-CXU6Y-SSA                                                  ADU026
           MOVE        XN06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94EA-FN. EXIT.
      *N94EB.    NOTE *CALL GN ON CX6Y                    *.            ADU026
       F94EB.                                                           lv10
           MOVE        'AREY' TO DE10-XDBDNM                            ADU026
           MOVE        'CX6Y' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XN06 CX6Y                                                    ADU026
           S-CX6Y-SSA                                                   ADU026
           MOVE        XN06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94EB-FN. EXIT.
      *N94FA.    NOTE *CALL GU ON AA85                    *.            ADU026
       F94FA.                                                           lv10
           MOVE        'LM1P' TO DE10-XDBDNM                            ADU026
           MOVE        'AA85' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XG06 AA85                                                    ADU026
           S-AAU10-SSA S-AA85-SSA                                       ADU026
           MOVE        XG06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94FA-FN. EXIT.
      *N94FC.    NOTE *CALL GN ON CT49                    *.            ADU026
       F94FC.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT49' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XL06 CT49                                                    ADU026
           S-CTU01-SSA S-CT49-SSA                                       ADU026
           MOVE        XL06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94FC-FN. EXIT.
       F94-FN.   EXIT.
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *PHONE/DEPENDENTS SUITABILITY       *
      *               *                                   *
      *               *************************************.
       F95.      IF    W167-ICMPF (1) = 'N'                             lv05
                 NEXT SENTENCE ELSE GO TO     F95-FN.
      *PROCESSING FOR FA
      *N95BB.    NOTE *PROCESS PERSON CLIENTS             *.
       F95BB.         EXIT.                                             lv10
      *N95BC.    NOTE *FOR CLIENT WHICH ARE NOT OWNER/    *.
       F95BC.    IF    W167-ICFAE (1) = 'N'                             lv15
                 AND   W167-ICFAR (1) = 'N'
                 AND   W167-INTAX (1) = 'N'
                 AND   W167-CSEOC (1) NOT = SPACES
                 AND   W167-CIAIC (1) NOT = SPACES
                 AND   W167-CNWCD (1) NOT = SPACES
                 AND   W167-CLNWC (1) NOT = SPACES
                 AND   W167-INAIN (1) = 'N'
                 AND   W167-INNWA (1) = 'N'
                 AND   W167-INLWA (1) = 'N'
                 AND   W167-CINEX (1) NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F95BC-FN.
      *OFFICER OR FINANCIAL INSTITUION
      *EMPLOYEE
           MOVE        'Y' TO W167-ICMPF (1).
       F95BC-FN. EXIT.
      *N95BD.    NOTE *FOR CLIENT WHICH ARE EITHER OWNE   *.
       F95BD.    IF    ((W167-ICFAE (1) = 'Y'                           lv15
                 AND   W167-ICFAR (1) NOT = SPACE)
                 OR    (W167-ICFAR (1) = 'Y'
                 AND   W167-ICFAE (1) NOT = SPACE))
                 AND   (W167-INTAX (1) = 'N'
                 AND   W167-CSEOC (1) NOT = SPACES
                 AND   W167-CIAIC (1) NOT = SPACES
                 AND   W167-CNWCD (1) NOT = SPACES
                 AND   W167-CLNWC (1) NOT = SPACES
                 AND   W167-CINEX (1) NOT = SPACES
                 AND   W167-INAIN (1) = 'N'
                 AND   W167-INNWA (1) = 'N'
                 AND   W167-INLWA (1) = 'N'
                 AND   W167-MEMNM (1) NOT = SPACES
                 AND   W167-MEAD1A (1) NOT = SPACES
                 AND   W167-MECIT (1) NOT = SPACES
                 AND   W167-TOCCU (1) NOT = SPACES)
                 NEXT SENTENCE ELSE GO TO     F95BD-FN.
      *OFFICER OR FINANCIAL INSTITUION
      *EMPLOYEE
                 IF    W167-GECTRY (1) = 'USA'                          DOT
                 AND   W167-CEPOS (1) NOT = SPACES
                 AND   W167-CESCD (1) NOT = SPACES
      *POSTAL AND STATE CODE ARE
      *MANDATORY IF THE COUNTRY IS USA
           MOVE        'Y' TO W167-ICMPF (1).
                 IF    W167-GECTRY (1) NOT = 'USA'                      DOT
      *POSTAL AND STATE CODE ARE NOT
      *MANDATORY IF COUNTRY IS NOT USA
           MOVE        'Y' TO W167-ICMPF (1).
       F95BD-FN. EXIT.
       F95BB-FN. EXIT.
      *N95CB.    NOTE *PROCESS ORGANIZATION CLIENTS       *.
       F95CB.         EXIT.                                             lv10
      *N95CC.    NOTE *FOR ORGANIZATION CLIENTS           *.
       F95CC.    IF    W167-INTAX (1) = 'N'                             lv15
                 AND   W167-CIAIC (1) NOT = SPACES
                 AND   W167-CNWCD (1) NOT = SPACES
                 AND   W167-CLNWC (1) NOT = SPACES
                 AND   W167-INAIN (1) = 'N'
                 AND   W167-INNWA (1) = 'N'
                 AND   W167-INLWA (1) = 'N'
                 AND   W167-CINEX (1) NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F95CC-FN.
           MOVE        'Y' TO W167-ICMPF (1).
       F95CC-FN. EXIT.
       F95CB-FN. EXIT.
       F95-FN.   EXIT.
      *N96.      NOTE *************************************.
      *               *                                   *
      *               *CALCULATE THE SKIP DATES           *
      *               *                                   *
      *               *************************************.
       F96.           EXIT.                                             lv05
      *N96RA.    NOTE *CALCULATE NEXT PMT DATE            *.            $DNPMT
       F96RA.                                                           lv10
      *MOVE IN PMT DATE                                                 $DNPMT
           MOVE        WS00-DSKIP TO 7-RA-DATE-TEMP                     $DNPMT
      *MOVE IN IRREG MONTHS                                             $DNPMT
           MOVE        WT12-CIRMO TO 7-RA-WT12-CIRMO-IN                 $DNPMT
      *INIT ERROR FLAG                                                  $DNPMT
           MOVE        'Y' TO 7-RA-ERR-FLAG                             $DNPMT
      *SET UP DAYS IN MONTH                                             $DNPMT
           MOVE        '312831303130313130313031' TO                    $DNPMT
           7-RA-MONTH-LOAD                                              $DNPMT
      *CALC LEAP YEAR                                                   $DNPMT
           PERFORM     F96RV THRU F96RV-FN.                             $DNPMT
      *N96RB.    NOTE *CHECK FOR INPUT ERRORS             *.            $DNPMT
       F96RB.    IF    7-RA-DA > 31                                     lv15
                 OR    7-RA-WT12-CIRMO-IN = SPACES                      $DNPMT
                 OR    (WT12-CPMTF < 90                                 $DNPMT
                 AND   7-RA-WT12-CIRMO-IN NOT =                         $DNPMT
                       'XXXXXXXXXXXX')                                  $DNPMT
                 NEXT SENTENCE ELSE GO TO     F96RB-FN.                 $DNPMT
           MOVE        'Y' TO 7-RA-ERR-FLAG                             $DNPMT
               GO TO     F96RA-FN.                                      $DNPMT
       F96RB-FN. EXIT.
      *N96RC.    NOTE *FOR 01,02,03,04,06,91              *.            $DNPMT
       F96RC.    IF    WT12-CPMTF = 01 OR 02                            lv15
                 OR    03 OR 04 OR 06 OR 12 OR 91                       $DNPMT
                 NEXT SENTENCE ELSE GO TO     F96RC-FN.                 $DNPMT
           DIVIDE 12 BY WT12-CPMTF                                      $DNPMT
           GIVING 7-RA-NUMB-OF-MOS                                      $DNPMT
           MOVE        'Y' TO 7-RA-LOOP-END                             $DNPMT
           MOVE        'N' TO 7-RA-ERR-FLAG.                            $DNPMT
                 IF    WT12-CPMTF = 91                                  DOT
           MOVE        1 TO 7-RA-NUMB-OF-MOS                            $DNPMT
           MOVE        'N' TO 7-RA-LOOP-END.                            $DNPMT
      *N96RE.    NOTE *LOOP IF IRREG                      *.            $DNPMT
       F96RE.                       GO TO     F96RE-B.                  lv20
       F96RE-A.
                 IF    7-RA-LOOP-END = 'Y'                              $DNPMT
                                    GO TO     F96RE-FN.                 $DNPMT
       F96RE-B.
           ADD         7-RA-NUMB-OF-MOS TO 7-RA-MO.                     $DNPMT
                 IF    7-RA-MO > 12                                     DOT
           ADD         1 TO 7-RA-YR                                     $DNPMT
           SUBTRACT    12 FROM 7-RA-MO                                  $DNPMT
      *CHECK FOR LEAP YEAR                                              $DNPMT
           PERFORM     F96RV THRU F96RV-FN.                             $DNPMT
                 IF    7-RA-CIRMO (7-RA-MO) = 'X'                       DOT
           MOVE        'Y' TO 7-RA-LOOP-END.                            $DNPMT
       F96RE-900. GO TO F96RE-A.
       F96RE-FN. EXIT.
       F96RC-FN. EXIT.
      *N96RH.    NOTE *FOR 13, 26, 52, 92, 94, 95         *.            $DNPMT
       F96RH.    IF    WT12-CPMTF = 13 OR 92                            lv15
                 OR    26 OR 94 OR 52 OR 95                             $DNPMT
                 NEXT SENTENCE ELSE GO TO     F96RH-FN.                 $DNPMT
           MOVE        'N' TO 7-RA-ERR-FLAG.                            $DNPMT
       F96RI.                                                           lv20
                 IF    WT12-CPMTF = 13 OR 92                            DOT
           MOVE        28 TO 7-RA-NUMB-OF-DAYS.                         $DNPMT
                 IF    WT12-CPMTF = 26 OR 94                            DOT
           MOVE        14 TO 7-RA-NUMB-OF-DAYS.                         $DNPMT
                 IF    WT12-CPMTF = 52 OR 95                            DOT
           MOVE        7 TO 7-RA-NUMB-OF-DAYS.                          $DNPMT
                 IF    WT12-CPMTF = 92 OR 94 OR 95                      DOT
           MOVE        'N' TO 7-RA-LOOP-END                             $DNPMT
                 ELSE                                                   $DNPMT
           MOVE        'Y' TO 7-RA-LOOP-END.                            $DNPMT
       F96RI-FN. EXIT.
      *N96RJ.    NOTE *LOOP FOR IRREGS                    *.            $DNPMT
       F96RJ.                       GO TO     F96RJ-B.                  lv20
       F96RJ-A.
                 IF    7-RA-LOOP-END = 'Y'                              $DNPMT
                                    GO TO     F96RJ-FN.                 $DNPMT
       F96RJ-B.
           ADD         7-RA-NUMB-OF-DAYS TO 7-RA-DA.                    $DNPMT
       F96RL.    IF    7-RA-DA >                                        lv25
                       7-RA-MONTH (7-RA-MO)                             $DNPMT
                 NEXT SENTENCE ELSE GO TO     F96RL-FN.                 $DNPMT
           SUBTRACT    7-RA-MONTH (7-RA-MO) FROM 7-RA-DA                $DNPMT
           ADD         1 TO 7-RA-MO.                                    $DNPMT
                 IF    7-RA-MO > 12                                     DOT
           ADD         1 TO 7-RA-YR                                     $DNPMT
           SUBTRACT    12 FROM 7-RA-MO                                  $DNPMT
      *RE-CHECK FOR LEAP YEAR                                           $DNPMT
           PERFORM     F96RV THRU F96RV-FN.                             $DNPMT
       F96RL-FN. EXIT.
       F96RM.    IF    7-RA-CIRMO (7-RA-MO) = 'X'                       lv25
                 NEXT SENTENCE ELSE GO TO     F96RM-FN.                 $DNPMT
           MOVE        'Y' TO 7-RA-LOOP-END.                            $DNPMT
       F96RM-FN. EXIT.
       F96RJ-900. GO TO F96RJ-A.
       F96RJ-FN. EXIT.
       F96RH-FN. EXIT.
      *N96RP.    NOTE *FOR 24,93                          *.            $DNPMT
       F96RP.    IF    WT12-CPMTF = 24 OR 93                            lv15
                 NEXT SENTENCE ELSE GO TO     F96RP-FN.                 $DNPMT
           MOVE        'N' TO 7-RA-ERR-FLAG.                            $DNPMT
                 IF    WT12-CPMTF = 93                                  DOT
           MOVE        'N' TO 7-RA-LOOP-END                             $DNPMT
                 ELSE                                                   $DNPMT
           MOVE        'Y' TO 7-RA-LOOP-END.                            $DNPMT
      *N96RQ.    NOTE *LOOP FOR IRREG                     *.            $DNPMT
       F96RQ.                       GO TO     F96RQ-B.                  lv20
       F96RQ-A.
                 IF    7-RA-LOOP-END = 'Y'                              $DNPMT
                                    GO TO     F96RQ-FN.                 $DNPMT
       F96RQ-B.       EXIT.
       F96RR.    IF    7-RA-DA > 15                                     lv25
                 NEXT SENTENCE ELSE GO TO     F96RR-FN.                 $DNPMT
           SUBTRACT    15 FROM 7-RA-DA                                  $DNPMT
           ADD         1 TO 7-RA-MO.                                    $DNPMT
                 IF    7-RA-MO > 12                                     DOT
           MOVE        1 TO 7-RA-MO                                     $DNPMT
           ADD         1 TO 7-RA-YR.                                    $DNPMT
       F96RR-900. GO TO F96RS-FN.
       F96RR-FN. EXIT.
       F96RS.                                                           lv25
           ADD         15 TO 7-RA-DA.                                   $DNPMT
       F96RS-FN. EXIT.
       F96RT.    IF    7-RA-CIRMO (7-RA-MO) = 'X'                       lv25
                 NEXT SENTENCE ELSE GO TO     F96RT-FN.                 $DNPMT
           MOVE        'Y' TO 7-RA-LOOP-END.                            $DNPMT
       F96RT-FN. EXIT.
       F96RQ-900. GO TO F96RQ-A.
       F96RQ-FN. EXIT.
       F96RP-FN. EXIT.
       F96RU.                                                           lv15
           MOVE        7-RA-DATE-TEMP TO WS00-DSKIP                     $DNPMT
               GO TO     F96RA-FN.                                      $DNPMT
       F96RU-FN. EXIT.
      *N96RV.    NOTE *LEAP YEAR CALC                     *.            $DNPMT
       F96RV.                                                           lv15
           DIVIDE 7-RA-YR BY 4                                          $DNPMT
           GIVING 7-RA-T0.                                              $DNPMT
                 IF    7-RA-T1-DEC > 0                                  DOT
           MOVE        28 TO 7-RA-MONTH (2)                             $DNPMT
               GO TO     F96RV-FN.                                      $DNPMT
                 IF    7-RA-YR-X-YY > 0                                 DOT
           MOVE        29 TO 7-RA-MONTH (2)                             $DNPMT
               GO TO     F96RV-FN.                                      $DNPMT
           DIVIDE 7-RA-YR-X-CC BY 4                                     $DNPMT
           GIVING 7-RA-T0.                                              $DNPMT
                 IF    7-RA-T1-DEC > 0                                  DOT
           MOVE        28 TO 7-RA-MONTH (2)                             $DNPMT
                 ELSE                                                   $DNPMT
           MOVE        29 TO 7-RA-MONTH (2).                            $DNPMT
       F96RV-FN. EXIT.
       F96RA-FN. EXIT.
       F96-FN.   EXIT.
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
           MOVE        QT63-DCACG TO PB07-DCACG                         AM0004
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
           MOVE        QT63-DCACG TO PC14-DCACG                         AM0018
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
      *N97EB.    NOTE *CALL CI0019 - ACCOUNT GROUPS       *.            AM0019
       F97EB.                                                           lv10
      *                                                                 AM0019
      *********************************                                 AM0019
      ** THIS MODULE WILL READ THE    *                                 AM0019
      ** CONTRACT DATABASE TO GET ALL *                                 AM0019
      ** THE GROUPS FOR THE ACCOUNT   *                                 AM0019
      ** NUMBER.                      *                                 AM0019
      *********************************                                 AM0019
      *                                                                 AM0019
           INITIALIZE      PD15                                         AM0019
           MOVE        CT01-CTID TO PD15-CTID                           AM0019
           MOVE        QT63-DCACG TO PD15-DCACG                         AM0019
           MOVE        01 TO PD15-XIMAX                                 AM0019
           MOVE        'Y' TO PD15-IPOCH                                AM0019
           SET CI0019-PCB-CT1P-PTR1 TO                                  AM0019
                       PCB-CT1P-PTR1                                    AM0019
           SET CI0019-PCB-GR1P-PTR1 TO                                  AM0019
                       PCB-GR1P-PTR1                                    AM0019
           INITIALIZE      DE10-DU03                                    AM0019
           CALL        CI0019 USING                                     AM0019
           DFHEIBLK                                                     AM0019
           DFHCOMMAREA                                                  AM0019
           DLIUIBII                                                     AM0019
           CI0019-PCB-ADDRESS-LIST                                      AM0019
           PD15                                                         AM0019
           DE10                                                         AM0019
           MS03.                                                        AM0019
       F97EB-FN. EXIT.
      *N97GB.    NOTE *CALL CI0083 - DECIPHER OWNERSHIP   *.            AM0083
       F97GB.                                                           lv10
      *********************************.                                DOT
      ** THIS MODULE WILL READ THE    *.                                DOT
      ** OWNERSHIP LINES ACCESSED BY  *.                                DOT
      ** CI0003 AND SEARCH THROUGH    *.                                DOT
      ** THE TEXT LOOKING FOR ACCT    *.                                DOT
      ** CLASSIFICATION               *.                                DOT
      *********************************                                 DOT
           INITIALIZE   PF1F                                            AM0083
           MOVE        PROGR TO PF1F-PROGR.                             AM0083
      *INSERT ADDITIONAL MOVES BELOW.                                   DOT
      *THINGS LIKE MAPPN AND ADDR LINES.                                DOT
      *INSERT ADDITIONAL MOVES ABOVE                                    DOT
           CALL        CI0083 USING                                     AM0083
           DFHEIBLK                                                     AM0083
           DFHCOMMAREA                                                  AM0083
           CT01                                                         AM0083
           PA04                                                         AM0083
           PF1F                                                         AM0083
           MS03                                                         AM0083
           MX11.                                                        AM0083
      *N97GE.    NOTE *IF NO ERRORS - BREAK DOWN PF1F     *.            AM0083
       F97GE.    IF    (MS03-NMESS2 = ZEROS                             lv15
                 OR    (MS03-NMESS2 NOT = ZEROS                         AM0083
                 AND   MS03-CMESB < 11))                                AM0083
                 NEXT SENTENCE ELSE GO TO     F97GE-FN.                 AM0083
           MOVE        PF1F-OUTPUT TO W-PF00-DECIPHER.                  AM0083
      *TALLY NUMBER OF 'Y' (TRUE) FLAGS                                 DOT
           INITIALIZE  TALLI                                            AM0083
           INSPECT     PF1F-OUTPUT TALLYING TALLI                       AM0083
           FOR ALL 'Y'.                                                 AM0083
       F97GE-FN. EXIT.
       F97GB-FN. EXIT.
      *N97HB.    NOTE *---> Call CI0124                   *.            AM0124
       F97HB.                                                           lv10
      *     Get Account Hold Codes                                      AM0124
      *                                                                 AM0124
           INITIALIZE  PG01                                             AM0124
           DE10-DU03                                                    AM0124
           MOVE        CT01-CTID TO PG01-CTID                           AM0124
           MOVE        QT63-DCACG TO PG01-DASOF                         AM0124
           SET CI0124-PCB-CT1P-PTR1 TO                                  AM0124
                      PCB-CT1P-PTR1                                     AM0124
           CALL        CI0124 USING                                     AM0124
           DFHEIBLK                                                     AM0124
           DFHCOMMAREA                                                  AM0124
           DLIUIBII                                                     AM0124
           CI0124-PCB-ADDRESS-LIST                                      AM0124
           PG01                                                         AM0124
           DE10                                                         AM0124
           MS03                                                         AM0124
           MX11.                                                        AM0124
       F97HB-FN. EXIT.
      *N97IB.    NOTE *---> Call CI0135                   *.            AM0135
       F97IB.                                                           lv10
      *     Get Cert Account Info                                       AM0135
      *                                                                 AM0135
           INITIALIZE  PJ02                                             AM0135
           DE10-DU03                                                    AM0135
           MOVE        CT01-CTID TO PJ02-CTID                           AM0135
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
           PJ02                                                         AM0135
           DE10                                                         AM0135
           MS03                                                         AM0135
           MX11.                                                        AM0135
       F97IB-FN. EXIT.
      *N97JB.    NOTE *CALL LIFE DETAIL INFO    MODULE    *.            AM0140
       F97JB.                                                           lv10
      *SET PCB POINTERS                                                 DOT
           SET CI0140-PH-PCB-LM1P-PTR1 TO                               AM0140
                         PCB-LM1P-PTR1                                  AM0140
           SET CI0140-PH-PCB-LUVP-PTR1 TO                               AM0140
                         PCB-LUVP-PTR1                                  AM0140
           SET CI0140-PH-PCB-LH1P-PTR1 TO                               AM0140
                         PCB-LH1P-PTR1                                  AM0140
           SET CI0140-PH-PCB-LARP-PTR1 TO                               AM0140
                         PCB-LARP-PTR1                                  AM0140
           SET CI0140-PH-PCB-LPDP-PTR1 TO                               AM0140
                         PCB-LPDP-PTR1                                  AM0140
           SET CI0140-PH-PCB-ARAY-PTR1 TO                               AM0140
                         PCB-ARAY-PTR1                                  AM0140
           SET CI0140-PH-PCB-AR1P-PTR1 TO                               AM0140
                         PCB-AR1P-PTR1.                                 AM0140
      *INITIALIZE OUTPUT PASS AREA                                      DOT
           INITIALIZE  DE10-DU03                                        AM0140
           INITIALIZE  WE40.                                            AM0140
      *LOAD INPUT PARMS  PASS AREA                                      DOT
           MOVE        CT01-CTID TO WE40-CTID
           MOVE        CT01-PRCOD TO WE40-PRCOD.
           CALL        CI0140 USING                                     DOT
           DFHEIBLK                                                     AM0140
           DFHCOMMAREA                                                  AM0140
           DLIUIBII                                                     AM0140
           CI0140-PH-PCB-ADDR-LIST                                      AM0140
           WE40                                                         AM0140
           DE10                                                         AM0140
           MS03                                                         AM0140
           MX11.                                                        AM0140
       F97JB-FN. EXIT.
      *N97KB.    NOTE *---> CALL CI0141                   *.            AM0141
       F97KB.                                                           lv10
      *     GET VANTAGE LIFE ACCT INFO                                  AM0141
      *.                                                                AM0141
      *INITIALIZE        PASS AREA                                      DOT
           INITIALIZE  WF41-K94R.                                       AM0141
      *LOAD INPUT PARMS  PASS AREA                                      DOT
           MOVE        CT01-CTID TO WF41-CTID
           MOVE        CT01-PRCOD TO WF41-PRCOD.
           CALL        CI0141 USING                                     DOT
           DFHEIBLK                                                     AM0141
           DFHCOMMAREA                                                  AM0141
           WF41                                                         AM0141
           MS03                                                         AM0141
           MX11.                                                        AM0141
       F97KB-FN. EXIT.
      *N97LB.    NOTE *CALL CI0223 HOUSEHOLD VALUES       *.            AM0223
       F97LB.                                                           lv10
      *                                                                 AM0223
      *********************************                                 AM0223
      ** CALL THE PROGRAM TO          *                                 AM0223
      ** PERFORM CNS AND CLIENT       *                                 AM0223
      ** MARKETING READ               *                                 AM0223
      *********************************                                 AM0223
      *                                                                 AM0223
           CALL        CI0223 USING                                     AM0223
           DFHEIBLK                                                     AM0223
           DFHCOMMAREA                                                  AM0223
           V223.                                                        AM0223
       F97LB-FN. EXIT.
      *N97MB.    NOTE *CALL SUIT RULES INFO MODULE        *.            AM0297
       F97MB.                                                           lv10
      ***                      **                                       AM0297
      ***  PROGRAMS USING THIS **                                       AM0297
      *** MACRO NEED TO ADD    **                                       AM0297
      *** APPROPRIATE ERROR    **                                       AM0297
      *** HANDLING AFTER       **                                       AM0297
      *** RETURN FROM CI0297.  **                                       AM0297
      ***                      **.                                      AM0297
           CALL        CI0297 USING                                     DOT
           DFHEIBLK                                                     AM0297
           DFHCOMMAREA                                                  AM0297
           K996.                                                        AM0297
       F97MB-FN. EXIT.
      *N97MD.    NOTE *CALL CI0975 TO GET ARRANGEMENT     *.            AM0975
       F97MD.                                                           lv10
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
       F97MD-FN. EXIT.
      *N97NB.    NOTE *CALL CI0320 CLIENT INFORMATION     *.            AMC320
       F97NB.                                                           lv10
      *                                                                 AMC320
      *********************************                                 AMC320
      ** CALL THE DRIVER PROGRAM FOR  *                                 AMC320
      ** FIND CLIENT SUITABILITY.     *                                 AMC320
      *********************************                                 AMC320
           CALL        CI0320 USING                                     AMC320
           DFHEIBLK                                                     AMC320
           DFHCOMMAREA                                                  AMC320
           W167                                                         AMC320
           PCB-CL1P-PTR1.                                               AMC320
       F97NB-FN. EXIT.
      *N97PB.    NOTE *CALL CI0323                        *.            AMC323
       F97PB.                                                           lv10
      *                                                                 AMC323
      *********************************                                 AMC323
      ** CALL THE DRIVER PROGRAM FOR  *                                 AMC323
      ** GET ACCOUNT SUITABILITY.     *                                 AMC323
      *********************************                                 AMC323
           CALL        CI0323 USING                                     AMC323
           DFHEIBLK                                                     AMC323
           DFHCOMMAREA                                                  AMC323
           W260                                                         AMC323
           PCB-CT1P-PTR1.                                               AMC323
       F97PB-FN. EXIT.
      *N97ZB.    NOTE *CHECK ERRORS FROM CALL             *.
       F97ZB.                                                           lv10
      *EXCEPTIONS TO KNOWN PROD PROBS
                 IF    MS03-NMESS2 = 13239                              DOT
                 AND   WS00-PROGR = 'CI0140'
      *AA10'S MISSING WHEN CT01 IS NOT
           INITIALIZE  MS03.
                 IF    MS03-NMESS2 = 13328                              DOT
                 AND   WS00-PROGR = 'CI0140'
                 AND   CT01-CTSTA = 03
      *AA16'S CAN MISS ON INACTIVE ACCT
           INITIALIZE  MS03.
                 IF    ACCT-INVALID = 'Y'                               DOT
      *SKIP BAD ACCOUNTS IN PRODUCTION
           INITIALIZE  MS03.
                 IF    ACCT-VALERR = 'Y'                                DOT
      *FORGIVE VALUATION ERROR PROBLEMS
           INITIALIZE  MS03.
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
      *N99.      NOTE *************************************.
      *               *                                   *
      *               *CALL MKT CLOSE INQUIRY MODULE      *
      *               *                                   *
      *               *************************************.
       F99.           EXIT.                                             lv05
      *N99BB.    NOTE *********************************   *.            ACMCTI
       F99BB.                                                           lv10
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
      *N99CR.    NOTE *CHECK RETURN CLDES                 *.
       F99CR.    IF    I93B-CRTNC = 11111                               lv15
                 OR    I93B-CRTNC = 22222
                 OR    I93B-CRTNC = 100
                 NEXT SENTENCE ELSE GO TO     F99CR-FN.
      *IF INVALID CODE, SEND ERROR MSG
      *AND TERMINATE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012786 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F99CR-FN. EXIT.
      *N99CU.    NOTE *REFORMAT DATE                      *.
       F99CU.                                                           lv15
           MOVE        I93B-GETIMM TO WS-GETIMM
           MOVE        WS-HH TO WS-MKT-HH
           MOVE        WS-MM TO WS-MKT-MM
           MOVE        WS-SS TO WS-MKT-SS.
       F99CU-FN. EXIT.
       F99BB-FN. EXIT.
       F99-FN.   EXIT.
