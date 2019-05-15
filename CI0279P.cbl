       IDENTIFICATION DIVISION.                                         CI0279
       PROGRAM-ID.  CI0279P.                                            CI0279
      *AUTHOR.         INITIALISE TSQ BROWSER INPUTS.                   CI0279
      *DATE-COMPILED.   09/08/14.                                       CI0279
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
       ENVIRONMENT DIVISION.                                            CI0279
       CONFIGURATION SECTION.                                           CI0279
       SOURCE-COMPUTER. IBM-370.                                        CI0279
       OBJECT-COMPUTER. IBM-370.                                        CI0279
       DATA DIVISION.                                                   CI0279
       WORKING-STORAGE SECTION.                                         CI0279
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0218           PIC X(08)  VALUE 'CI0218P '.                AM0218
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0279
            10            XW05-XW06.                                    CI0279
            11            XW05-XDBPCB.                                  CI0279
            12            XW05-XDBDNM PICTURE  X(08)                    CI0279
                          VALUE                SPACE.                   CI0279
            12            XW05-XSEGLV PICTURE  X(02)                    CI0279
                          VALUE                SPACE.                   CI0279
            12            XW05-XRC    PICTURE  X(02)                    CI0279
                          VALUE                SPACE.                   CI0279
            12            XW05-XPROPT PICTURE  X(04)                    CI0279
                          VALUE                SPACE.                   CI0279
            12            XW05-FILLER PICTURE  S9(5)                    CI0279
                          VALUE                ZERO                     CI0279
                          BINARY.                                       CI0279
            12            XW05-XSEGNM PICTURE  X(08)                    CI0279
                          VALUE                SPACE.                   CI0279
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0279
                          VALUE                ZERO                     CI0279
                          BINARY.                                       CI0279
            12            XW05-XSEGNB PICTURE  9(05)                    CI0279
                          VALUE                ZERO                     CI0279
                          BINARY.                                       CI0279
            12            XW05-XCOKEY PICTURE  X(70)                    CI0279
                          VALUE                SPACE.                   CI0279
            10            XW05-XW07.                                    CI0279
            11            XW05-XIOPCB.                                  CI0279
            12            XW05-XTERMI PICTURE  X(08)                    CI0279
                          VALUE                SPACE.                   CI0279
            12            XW05-FILLER PICTURE  XX                       CI0279
                          VALUE                SPACE.                   CI0279
            12            XW05-XRC1   PICTURE  X(02)                    CI0279
                          VALUE                SPACE.                   CI0279
            12            XW05-FILLER PICTURE  X(12)                    CI0279
                          VALUE                SPACE.                   CI0279
            12            XW05-XMODNM PICTURE  X(8)                     CI0279
                          VALUE                SPACE.                   CI0279
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0279
                          VALUE                ZERO.                    CI0279
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0279
                          VALUE                ZERO.                    CI0279
            10            XW05-XGU    PICTURE  X(4)                     CI0279
                          VALUE                'GU  '.                  CI0279
            10            XW05-XGHU   PICTURE  X(4)                     CI0279
                          VALUE                'GHU '.                  CI0279
            10            XW05-XGN    PICTURE  X(4)                     CI0279
                          VALUE                'GN  '.                  CI0279
            10            XW05-XGHN   PICTURE  X(4)                     CI0279
                          VALUE                'GHN '.                  CI0279
            10            XW05-XGNP   PICTURE  X(4)                     CI0279
                          VALUE                'GNP '.                  CI0279
            10            XW05-XGHNP  PICTURE  X(4)                     CI0279
                          VALUE                'GHNP'.                  CI0279
            10            XW05-XREPL  PICTURE  XXXX                     CI0279
                          VALUE                'REPL'.                  CI0279
            10            XW05-XISRT  PICTURE  X(4)                     CI0279
                          VALUE                'ISRT'.                  CI0279
            10            XW05-XDLET  PICTURE  X(4)                     CI0279
                          VALUE                'DLET'.                  CI0279
            10            XW05-XOPEN  PICTURE  X(4)                     CI0279
                          VALUE                'OPEN'.                  CI0279
            10            XW05-XCLSE  PICTURE  X(4)                     CI0279
                          VALUE                'CLSE'.                  CI0279
            10            XW05-XCHKP  PICTURE  X(4)                     CI0279
                          VALUE                'CHKP'.                  CI0279
            10            XW05-XXRST  PICTURE  X(4)                     CI0279
                          VALUE                'XRST'.                  CI0279
            10            XW05-XTERM  PICTURE  X(4)                     CI0279
                          VALUE                'TERM'.                  CI0279
            10            XW05-XNFPAC PICTURE  X(13)                    CI0279
                          VALUE                SPACE.                   CI0279
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0279
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0279
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
      *WORK FIELDS
       01  WS01-SUB         PIC 9(05)  VALUE 1.
      ******************************************************************
      *  MISC WORK FIELDS
      ******************************************************************
      *
      ******************************************************************
      *THESE FIELDS USED TO INTERACT WITH TSQ UTILITY
      ******************************************************************
       01  WT-CANUMB.
           05  FILLER              PIC X(22)  VALUE SPACES.
           05  WT-NSEQ5            PIC 9(05)  VALUE ZERO.
      *
       01  WT-TSQ-MAX              PIC S9(4)  COMP VALUE 0.
      *
       01  WT-TSQ-TYPE-FIELDS.
         05  WT-TYPE-A             PIC X(10)   VALUE 'ACCTLIST  '.
         05  WT-TYPE-X             PIC X(10)   VALUE 'ACCTKEY   '.
         05  WT-TYPE-S             PIC X(10)   VALUE 'SUBACCT   '.
      ******************************************************************
      *THESE FIELDS USED IN GETMAIN FOR WORK TABLE
      ******************************************************************
       01  7-WT-LEN                PIC S9(04)  COMP.
       01  7-WT-SPACE              PIC X(01)   VALUE SPACE.
       01  7-WT-RESP               PIC S9(08)  COMP.
      *!WI
       01  7-WT-NPNTRA
                          POINTER.                                      CI0279
       01   DEBUT-WSS.                                                  CI0279
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0279
            05   IK     PICTURE X.                                      CI0279
       01  CONSTANTES-PAC.                                              CI0279
           05  FILLER  PICTURE X(87)   VALUE                            CI0279
                     '6015 CAT09/08/14CI0279ADMIN   14:35:12CI0279P AMERCI0279
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0279
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0279
           05  NUGNA   PICTURE X(5).                                    CI0279
           05  APPLI   PICTURE X(3).                                    CI0279
           05  DATGN   PICTURE X(8).                                    CI0279
           05  PROGR   PICTURE X(6).                                    CI0279
           05  CODUTI  PICTURE X(8).                                    CI0279
           05  TIMGN   PICTURE X(8).                                    CI0279
           05  PROGE   PICTURE X(8).                                    CI0279
           05  COBASE  PICTURE X(4).                                    CI0279
           05  DATGNC  PICTURE X(10).                                   CI0279
           05  RELEAS  PICTURE X(7).                                    CI0279
           05  DATGE   PICTURE X(10).                                   CI0279
           05  DATSQ   PICTURE X(10).                                   CI0279
       01  DATCE.                                                       CI0279
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0279
         05  DATOR.                                                     CI0279
           10  DATOA  PICTURE XX.                                       CI0279
           10  DATOM  PICTURE XX.                                       CI0279
           10  DATOJ  PICTURE XX.                                       CI0279
       01   VARIABLES-CONDITIONNELLES.                                  CI0279
            05                  FT      PICTURE X VALUE '0'.            CI0279
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0279
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0279
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   ZONES-UTILISATEUR PICTURE X.                                CI0279
      *SUB ACCOUNT RECORD LAYOUT
      *!WF DSP=K9 DSL=K9 SEL=79 FOR=I DES=1 LEV=1 PLT=K9
       01                 K979.                                         CI0279
            10            K979-K970.                                    CI0279
            11            K979-MFDNMS PICTURE  X(30).                   CI0279
            11            K979-ALPALC PICTURE  S9(04)V999               CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-PSUBA  PICTURE  S999V999                 CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-PMDAP  PICTURE  S9(4)V9(5)               CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-CFIDC  PICTURE  X(5).                    CI0279
            11            K979-NSEQ4  PICTURE  9(4).                    CI0279
            11            K979-CACCT  PICTURE  X.                       CI0279
            11            K979-AACTVF PICTURE  S9(7)V99                 CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-AACTVG PICTURE  S9(7)V99                 CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-PSUBA1 PICTURE  S999V999                 CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-ADBRQA PICTURE  S9(9)V99                 CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-ADBRQC PICTURE  S9(9)V99.                CI0279
            11            K979-ADBRQF PICTURE  S9(9)V99                 CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-ADBRQG PICTURE  S9(9)V99                 CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-PACT1  PICTURE  S999V999                 CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-PACT1A PICTURE  S999V999                 CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-IALLV  PICTURE  X.                       CI0279
            11            K979-ITRNB  PICTURE  X.                       CI0279
            11            K979-AEDRQ1 PICTURE  S9(09)V99                CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-AEDRQ2 PICTURE  S9(09)V99                CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-PVFPA  PICTURE  S9(3)V9(1)               CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-ISUBA1 PICTURE  X.                       CI0279
            11            K979-ISUBA2 PICTURE  X.                       CI0279
            11            K979-DRSNC1 PICTURE  9(8).                    CI0279
            11            K979-DRSNC2 PICTURE  9(8).                    CI0279
            11            K979-CRSNG1 PICTURE  X(02).                   CI0279
            11            K979-CRSNG2 PICTURE  X(02).                   CI0279
            11            K979-AMAXD  PICTURE  S9(7)V99                 CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-CAMOT4 PICTURE  X(1).                    CI0279
            11            K979-CAMOT5 PICTURE  X(1).                    CI0279
            11            K979-ALLNB  PICTURE  S9(07)V99                CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-ALSHR1 PICTURE  S9(7)V9(6)               CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-ALPRCV PICTURE  S9(3)V9(6)               CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            K979-QSACTF PICTURE  9(3).                    CI0279
            11            K979-QSACTT PICTURE  9(3).                    CI0279
            11            K979-CACCT1 PICTURE  X.                       CI0279
            11            K979-IFTDY1 PICTURE  X.                       CI0279
            11            K979-FILLER PICTURE  X(99).                   CI0279
            10            K979-FILLER PICTURE  X(1726).                 CI0279
      ******************************************************************AM0218
      ** WORKING STORAGE SEGMENT FOR CI0218                            *AM0218
      ******************************************************************AM0218
      *                                                                 AM0218
       01   7-X100-STAGING.                                             AM0218
      *!WI pl=X1210                                                     AM0218
            05  7-X100-XFUNC                                            AM0218
                        PICTURE X(04).                                  CI0279
      *!WI    X1220                                                     AM0218
            05  7-X100-NPNTRA                                           AM0218
                          POINTER.                                      CI0279
       LINKAGE SECTION.                                                 ADU102
      *>>>>>>   DLIUIBII Copybook                                       ADU129
            COPY  DLIUIBII.                                             ADU129
      *                                                                 ADU129
      *>>>>>> Address list of PCB's                                     ADU129
      *                                                                 ADU129
      *01   PCB-ADDRESS-LIST.
      *                                                                 ADU129
      *NOTE: All PCB pointers must be added here using macro ADU015     ADU129
      *      once for each database used                                ADU129
      *                                                                 ADU129
      *NOTE: Following PCB pointers, include PCB masks using ADU015     ADU129
      *      once for each database used                                ADU129
      *                                                                 ADU129
      *-----------------------------------------------------------------ADU129
      *
      *!WF DSP=VW DSL=VW SEL=02 FOR=I DES=1 LEV=1 PLT=05
       01                 VW02.                                         CI0279
            10            VW02-MTQUE  PICTURE  X(08).                   CI0279
            10            VW02-GEOPD2 PICTURE  X(8).                    CI0279
            10            VW02-GEAUN  PICTURE  9(5).                    CI0279
            10            VW02-XIMAX  PICTURE  S9(4)                    CI0279
                          OCCURS       004     TIMES                    CI0279
                          BINARY.                                       CI0279
            10            VW02-MPLNR2 PICTURE  X(40).                   CI0279
            10            VW02-ATROLL PICTURE  X(25).                   CI0279
            10            VW02-GESTNS PICTURE  X(2).                    CI0279
            10            VW02-NFLID  PICTURE  X(8).                    CI0279
            10            VW02-CAOSTA PICTURE  X.                       CI0279
            10            VW02-CAOSTB PICTURE  X.                       CI0279
            10            VW02-CAOSTC PICTURE  X.                       CI0279
            10            VW02-CAOSTD PICTURE  X.                       CI0279
            10            VW02-CAOSTE PICTURE  X.                       CI0279
            10            VW02-CAOSTF PICTURE  X.                       CI0279
            10            VW02-CAOSTG PICTURE  X.                       CI0279
            10            VW02-CAOSTH PICTURE  X.                       CI0279
            10            VW02-CAOSTI PICTURE  X.                       CI0279
            10            VW02-XDATE  PICTURE  X(8).                    CI0279
            10            VW02-GTMST  PICTURE  X(6).                    CI0279
            10            VW02-FILLER PICTURE  X(135).                  CI0279
      *BROWSER INPUT
      *!WF DSP=T9 DSL=K9 SEL=78 FOR=E DES=1 LEV=1 PLT=10
       01                 T978.                                         CI0279
            10            T978-MAPPN  PICTURE  X(10).                   CI0279
            10            T978-NSSSI  PICTURE  X(24).                   CI0279
            10            T978-CAPPL  PICTURE  X(8).                    CI0279
      **** AREA USED TO COMMUNICATE WITH TSQ ***************************
      *!WF DSP=X1 DSL=VW SEL=01 FOR=I DES=1 LEV=1 PLT=10
       01                 X101.                                         CI0279
            10            X101-FILLER.                                  CI0279
            11            X101-XFUNC  PICTURE  X(04).                   CI0279
            11            X101-CANUMB PICTURE  X(27).                   CI0279
            10            X101-CENTT  PICTURE  X.                       CI0279
            10            X101-FILLER PICTURE  X(68).                   CI0279
            10            X101-MTQUE  PICTURE  X(08).                   CI0279
            10            X101-FILLER PICTURE  X(04).                   CI0279
            10            X101-FILLER.                                  CI0279
            11            X101-NPNTRB                                   CI0279
                          POINTER.                                      CI0279
            11            X101-NPNTRC                                   CI0279
                          POINTER.                                      CI0279
            11            X101-NPNTRD                                   CI0279
                          POINTER.                                      CI0279
            11            X101-NPNTRE                                   CI0279
                          POINTER.                                      CI0279
            11            X101-NPNTRF                                   CI0279
                          POINTER.                                      CI0279
            11            X101-NPNTRG                                   CI0279
                          POINTER.                                      CI0279
            11            X101-NPNTRH                                   CI0279
                          POINTER.                                      CI0279
            10            X101-DCACG  PICTURE  9(8).                    CI0279
            10            X101-FILLER PICTURE  X(492).                  CI0279
      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0279
          05              DE00-SUITE.                                   CI0279
            15       FILLER         PICTURE  X(00653).                  CI0279
       01                 DE10  REDEFINES      DE00.                    CI0279
            10            DE10-DU11.                                    CI0279
            11            DE10-XFONC  PICTURE  X(4).                    CI0279
            11            DE10-MPSBN  PICTURE  X(8).                    CI0279
            11            DE10-XDBDNM PICTURE  X(08).                   CI0279
            11            DE10-XSEGNM PICTURE  X(08).                   CI0279
            11            DE10-XRC    PICTURE  X(02).                   CI0279
            11            DE10-MSEG   PICTURE  X(08).                   CI0279
            11            DE10-XCOKEY PICTURE  X(70).                   CI0279
            11            DE10-CUIBR  PICTURE  X(01).                   CI0279
            11            DE10-CUIBA  PICTURE  X(01).                   CI0279
            11            DE10-IPBIK  PICTURE  X(1).                    CI0279
            10            DE10-DU03.                                    CI0279
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            DE10-CMSSF  PICTURE  XX.                      CI0279
            11            DE10-DU09.                                    CI0279
            12            DE10-CMESA  PICTURE  S9(9)                    CI0279
                          BINARY.                                       CI0279
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0279
                          BINARY.                                       CI0279
            12            DE10-CMESB  PICTURE  S9(9)                    CI0279
                          BINARY.                                       CI0279
            12            DE10-CMSST  PICTURE  S9(9)                    CI0279
                          BINARY.                                       CI0279
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0279
                          BINARY.                                       CI0279
            12            DE10-QELLAA PICTURE  S9(9)                    CI0279
                          BINARY.                                       CI0279
            12            DE10-TMESS4 PICTURE  X(512).                  CI0279
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0279
          05              MS00-SUITE.                                   CI0279
            15       FILLER         PICTURE  X(00542).                  CI0279
       01                 MS03  REDEFINES      MS00.                    CI0279
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0279
                          COMPUTATIONAL-3.                              CI0279
            10            MS03-CMSSF  PICTURE  XX.                      CI0279
            10            MS03-DU09.                                    CI0279
            11            MS03-CMESA  PICTURE  S9(9)                    CI0279
                          BINARY.                                       CI0279
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0279
                          BINARY.                                       CI0279
            11            MS03-CMESB  PICTURE  S9(9)                    CI0279
                          BINARY.                                       CI0279
            11            MS03-CMSST  PICTURE  S9(9)                    CI0279
                          BINARY.                                       CI0279
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0279
                          BINARY.                                       CI0279
            11            MS03-QELLAA PICTURE  S9(9)                    CI0279
                          BINARY.                                       CI0279
            11            MS03-TMESS4 PICTURE  X(512).                  CI0279
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0279
            10            MX11-QMSGS  PICTURE  9(03).                   CI0279
            10            MX11-PJ09                                     CI0279
                          OCCURS       025     TIMES.                   CI0279
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0279
                          COMPUTATIONAL-3.                              CI0279
            11            MX11-CMESB  PICTURE  S9(9)                    CI0279
                          BINARY.                                       CI0279
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                T978
                                X101
                                VW02
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0279
      *               *                                   *             CI0279
      *               *INITIALISATIONS                    *             CI0279
      *               *                                   *             CI0279
      *               *************************************.            CI0279
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0279
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0279
      *               *                                   *             CI0279
      *               *FIN DE TRAITEMENT                  *             CI0279
      *               *                                   *             CI0279
      *               *************************************.            CI0279
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0279
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *LOOP THROUGH TSQ READ SUB          *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *ACCOUNT AND INITIALISE FIELDS
      *N50FF.    NOTE *LOOP UNTIL END OF SUB ACCOUNTS     *.
       F50FF.                       GO TO     F50FF-B.                  lv10
       F50FF-A.
                 IF    WS01-SUB > VW02-XIMAX (2)
                                    GO TO     F50FF-FN.
       F50FF-B.       EXIT.
      *N50KK.    NOTE *READ SUB ACCOUNT QUE               *.
       F50KK.                                                           lv15
      *READ TSQ RECORD FOR KEY TO USE
      *********************************
           MOVE        'READ' TO 7-X100-XFUNC
           MOVE        WS01-SUB TO X101-CANUMB (23:)
           MOVE        'S' TO X101-CENTT
           INITIALIZE  K979
           PERFORM     F91TA THRU F91TA-FN.
       F50KK-FN. EXIT.
      *N50NN.    NOTE *INITIALISE FIELDS                  *.
       F50NN.                                                           lv15
                 IF    T978-CAPPL = 'CIME13'                            DOT
                 OR    T978-CAPPL = 'CIME11'
      **
      **
           MOVE        K979-ADBRQF TO K979-ADBRQA
           MOVE        K979-ADBRQG TO K979-ADBRQC
           MOVE        0 TO K979-ADBRQF
           K979-ADBRQG
           K979-PACT1
           K979-PACT1A
           K979-AEDRQ1
           K979-AEDRQ2
           MOVE        K979-CACCT TO K979-CACCT1
           MOVE        SPACE TO K979-CACCT
           K979-IALLV
           K979-ITRNB
           K979-CAMOT4
           K979-CACCT.
                 IF    T978-CAPPL = 'CIME14'                            DOT
           MOVE        0 TO K979-PVFPA.
       F50NN-FN. EXIT.
      *N50RR.    NOTE *(RE)WRITE ACCT RECORD IN TSQ       *.
       F50RR.                                                           lv15
      *********************************
           MOVE        'WRIT' TO 7-X100-XFUNC
           MOVE        WS01-SUB TO X101-CANUMB (23:)
           MOVE        'S' TO X101-CENTT
           PERFORM     F91TA THRU F91TA-FN.
       F50RR-FN. EXIT.
      *N50UU.    NOTE *INCREASE SUBSCRIPT                 *.
       F50UU.                                                           lv15
           ADD         1 TO WS01-SUB.
       F50UU-FN. EXIT.
       F50FF-900. GO TO F50FF-A.
       F50FF-FN. EXIT.
       F50-FN.   EXIT.
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
      *N91TA.    NOTE *CALL CI0218 TEMP STORAGE UTIL      *.            AM0218
       F91TA.                                                           lv10
      *                                                                 AM0218
           MOVE        7-X100-XFUNC TO X101-XFUNC                       AM0218
           SET 7-X100-NPNTRA TO NULL                                    AM0218
           STRING      'FDCX'                                           AM0218
           T978-NSSSI                                                   AM0218
           DELIMITED BY SIZE                                            AM0218
           INTO X101-MTQUE                                              AM0218
           CALL        CI0218 USING                                     AM0218
           DFHEIBLK                                                     AM0218
           X101                                                         AM0218
           MS03                                                         AM0218
           MX11                                                         AM0218
      *AREAS TO PASS GO BEFORE LINE 940                                 AM0218
                  WT-TYPE-S
                  K979
                  7-X100-NPNTRA                                         AM0218
      *7-X100-NPNTRA MUST BE THE LAST                                   AM0218
      *PARM PASSED TO CI0218                                            AM0218
                 IF    MS03-NMESS2 NOT = ZERO                           DOT
      *CHECK FOR ERROR IN CI0218                                        AM0218
           PERFORM     F98ET THRU F98ET-FN.                             AM0218
       F91TA-FN. EXIT.
       F91-FN.   EXIT.
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
