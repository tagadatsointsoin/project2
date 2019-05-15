       IDENTIFICATION DIVISION.                                         CI0247
       PROGRAM-ID.  CI0247P.                                            CI0247
      *AUTHOR.         eFUNDING SECURITY.                               CI0247
      *DATE-COMPILED.   09/08/14.                                       CI0247
       ENVIRONMENT DIVISION.                                            CI0247
       CONFIGURATION SECTION.                                           CI0247
       SOURCE-COMPUTER. IBM-370.                                        CI0247
       OBJECT-COMPUTER. IBM-370.                                        CI0247
       DATA DIVISION.                                                   CI0247
       WORKING-STORAGE SECTION.                                         CI0247
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0018           PIC X(8) VALUE 'CI0018P '.                  AM0018
       01  CI0020           PIC X(8) VALUE 'CI0020P '.                  AM0020
       01  CI0052           PIC X(8) VALUE 'CI0052P'.                   AM0052
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0247
            10            XW05-XW06.                                    CI0247
            11            XW05-XDBPCB.                                  CI0247
            12            XW05-XDBDNM PICTURE  X(08)                    CI0247
                          VALUE                SPACE.                   CI0247
            12            XW05-XSEGLV PICTURE  X(02)                    CI0247
                          VALUE                SPACE.                   CI0247
            12            XW05-XRC    PICTURE  X(02)                    CI0247
                          VALUE                SPACE.                   CI0247
            12            XW05-XPROPT PICTURE  X(04)                    CI0247
                          VALUE                SPACE.                   CI0247
            12            XW05-FILLER PICTURE  S9(5)                    CI0247
                          VALUE                ZERO                     CI0247
                          BINARY.                                       CI0247
            12            XW05-XSEGNM PICTURE  X(08)                    CI0247
                          VALUE                SPACE.                   CI0247
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0247
                          VALUE                ZERO                     CI0247
                          BINARY.                                       CI0247
            12            XW05-XSEGNB PICTURE  9(05)                    CI0247
                          VALUE                ZERO                     CI0247
                          BINARY.                                       CI0247
            12            XW05-XCOKEY PICTURE  X(70)                    CI0247
                          VALUE                SPACE.                   CI0247
            10            XW05-XW07.                                    CI0247
            11            XW05-XIOPCB.                                  CI0247
            12            XW05-XTERMI PICTURE  X(08)                    CI0247
                          VALUE                SPACE.                   CI0247
            12            XW05-FILLER PICTURE  XX                       CI0247
                          VALUE                SPACE.                   CI0247
            12            XW05-XRC1   PICTURE  X(02)                    CI0247
                          VALUE                SPACE.                   CI0247
            12            XW05-FILLER PICTURE  X(12)                    CI0247
                          VALUE                SPACE.                   CI0247
            12            XW05-XMODNM PICTURE  X(8)                     CI0247
                          VALUE                SPACE.                   CI0247
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0247
                          VALUE                ZERO.                    CI0247
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0247
                          VALUE                ZERO.                    CI0247
            10            XW05-XGU    PICTURE  X(4)                     CI0247
                          VALUE                'GU  '.                  CI0247
            10            XW05-XGHU   PICTURE  X(4)                     CI0247
                          VALUE                'GHU '.                  CI0247
            10            XW05-XGN    PICTURE  X(4)                     CI0247
                          VALUE                'GN  '.                  CI0247
            10            XW05-XGHN   PICTURE  X(4)                     CI0247
                          VALUE                'GHN '.                  CI0247
            10            XW05-XGNP   PICTURE  X(4)                     CI0247
                          VALUE                'GNP '.                  CI0247
            10            XW05-XGHNP  PICTURE  X(4)                     CI0247
                          VALUE                'GHNP'.                  CI0247
            10            XW05-XREPL  PICTURE  XXXX                     CI0247
                          VALUE                'REPL'.                  CI0247
            10            XW05-XISRT  PICTURE  X(4)                     CI0247
                          VALUE                'ISRT'.                  CI0247
            10            XW05-XDLET  PICTURE  X(4)                     CI0247
                          VALUE                'DLET'.                  CI0247
            10            XW05-XOPEN  PICTURE  X(4)                     CI0247
                          VALUE                'OPEN'.                  CI0247
            10            XW05-XCLSE  PICTURE  X(4)                     CI0247
                          VALUE                'CLSE'.                  CI0247
            10            XW05-XCHKP  PICTURE  X(4)                     CI0247
                          VALUE                'CHKP'.                  CI0247
            10            XW05-XXRST  PICTURE  X(4)                     CI0247
                          VALUE                'XRST'.                  CI0247
            10            XW05-XTERM  PICTURE  X(4)                     CI0247
                          VALUE                'TERM'.                  CI0247
            10            XW05-XNFPAC PICTURE  X(13)                    CI0247
                          VALUE                SPACE.                   CI0247
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0247
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0247
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
       01                 NS00.                                         CI0247
          05              NS00-00.                                      CI0247
            10            NS00-NS00K.                                   CI0247
            11            NS00-PRCSTK PICTURE  XX.                      CI0247
          05              NS00-SUITE.                                   CI0247
            15       FILLER         PICTURE  X(00078).                  CI0247
       01                 NS20  REDEFINES      NS00.                    CI0247
            10       FILLER         PICTURE  X(00002).                  CI0247
            10            NS20-DCACG  PICTURE  9(8).                    CI0247
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0247
                          COMPUTATIONAL-3.                              CI0247
            10            NS20-CCDAT  PICTURE  X(8).                    CI0247
            10            NS20-DCALP  PICTURE  X(12).                   CI0247
            10            NS20-DNACG  PICTURE  9(8).                    CI0247
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0247
                          COMPUTATIONAL-3.                              CI0247
            10            NS20-CNDAT  PICTURE  X(8).                    CI0247
            10            NS20-DNALP  PICTURE  X(12).                   CI0247
            10            NS20-DCACD  PICTURE  X(10).                   CI0247
            10            NS20-FILLER PICTURE  X(4).                    CI0247
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
                                                                        AM0018
      ******************************************************************AM0018
      **     PCB ADDRESS LIST FOR CI0018.  MODULE CI0018 WILL NEED     *AM0018
      **     PCB'S FOR:                                                *AM0018
      **                CONTRACT DATABASE(CT1P)                        *AM0018
      ******************************************************************AM0018
                                                                        AM0018
       01  CI0018-PCB-ADDRESS-LIST.                                     AM0018
           05  CI0018-PCB-CT1P-PTR1      POINTER.                       AM0018
      *
      *-----------------------------------------------------------------
      * USED TO GET CLIENTS FOR ACCOUNT FROM CI0018
      *-----------------------------------------------------------------
      *
      *!WF DSP=PC DSL=DU SEL=14 FOR=I LEV=1 PLT=PC
       01                 PC00.                                         CI0247
          05              PC00-SUITE.                                   CI0247
            15       FILLER         PICTURE  X(00917).                  CI0247
       01                 PC14  REDEFINES      PC00.                    CI0247
            10            PC14-C299.                                    CI0247
            11            PC14-CTID.                                    CI0247
            12            PC14-CTIDA  PICTURE  9(3).                    CI0247
            12            PC14-CTIDN.                                   CI0247
            13            PC14-CTIDNP PICTURE  X(13).                   CI0247
            13            PC14-CTIDND PICTURE  9(11).                   CI0247
            10            PC14-DCACG  PICTURE  9(8).                    CI0247
            10            PC14-IPOCH  PICTURE  X.                       CI0247
            10            PC14-FILLER PICTURE  X(100).                  CI0247
            10            PC14-CLID01.                                  CI0247
            11            PC14-CLIDO1 PICTURE  X(3).                    CI0247
            11            PC14-NCLID1.                                  CI0247
            12            PC14-CLIDP1 PICTURE  X(12).                   CI0247
            12            PC14-CLIDNA PICTURE  9(8).                    CI0247
            10            PC14-CLCTR  PICTURE  9(3).                    CI0247
            10            PC14-DU21                                     CI0247
                          OCCURS       025     TIMES.                   CI0247
            11            PC14-C199.                                    CI0247
            12            PC14-CLID.                                    CI0247
            13            PC14-CLIDO  PICTURE  9(3).                    CI0247
            13            PC14-CLIDN.                                   CI0247
            14            PC14-CLIDNP PICTURE  X(12).                   CI0247
            14            PC14-CLIDND PICTURE  9(8).                    CI0247
            11            PC14-CLCTRC PICTURE  9(3).                    CI0247
            10            PC14-QITEM  PICTURE  9(3).                    CI0247
            10            PC14-XIMAX  PICTURE  S9(4)                    CI0247
                          BINARY.                                       CI0247
            10            PC14-CRROL  PICTURE  X.                       CI0247
            10            PC14-FILLER PICTURE  X(099).                  CI0247
      *
      *
      *-----------------------------------------------------------------
      * USED TO GET CLIENTS FOR GROUP FROM CI0052
      *-----------------------------------------------------------------
      *
      *!WF DSP=WZ DSL=CP SEL=04 FOR=I LEV=1 PLT=WZ
       01                 WZ00.                                         CI0247
          05              WZ00-SUITE.                                   CI0247
            15       FILLER         PICTURE  X(02413).                  CI0247
       01                 WZ04  REDEFINES      WZ00.                    CI0247
            10            WZ04-G198.                                    CI0247
            11            WZ04-GRID.                                    CI0247
            12            WZ04-GRIDC  PICTURE  9(3).                    CI0247
            12            WZ04-GRIDN.                                   CI0247
            13            WZ04-GRIDNP PICTURE  99.                      CI0247
            13            WZ04-GRIDND PICTURE  9(8).                    CI0247
            10            WZ04-C199                                     CI0247
                          OCCURS       100     TIMES.                   CI0247
            11            WZ04-CLID.                                    CI0247
            12            WZ04-CLIDO  PICTURE  9(3).                    CI0247
            12            WZ04-CLIDN.                                   CI0247
            13            WZ04-CLIDNP PICTURE  X(12).                   CI0247
            13            WZ04-CLIDND PICTURE  9(8).                    CI0247
            10            WZ04-GECKD  PICTURE  9                        CI0247
                          OCCURS       100     TIMES.                   CI0247
      *
      ******************************************************************AM0052
      *        CI0052 I/O FIELDS                                       *AM0052
      ******************************************************************AM0052
                                                                        AM0052
      *!WF DSP=WZ DSL=CP SEL=04 FOR=I DES=1 LEV=1                       AM0052
       01  CI0052-PCB-ADDRESS-LIST.                                     AM0052
           05  CI0052-PCB-GR1P-PTR1        POINTER.                     AM0052
      *
       01   DEBUT-WSS.                                                  CI0247
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0247
            05   IK     PICTURE X.                                      CI0247
       01  CONSTANTES-PAC.                                              CI0247
           05  FILLER  PICTURE X(87)   VALUE                            CI0247
                     '6015 CAT09/08/14CI0247ADMIN   14:35:06CI0247P AMERCI0247
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0247
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0247
           05  NUGNA   PICTURE X(5).                                    CI0247
           05  APPLI   PICTURE X(3).                                    CI0247
           05  DATGN   PICTURE X(8).                                    CI0247
           05  PROGR   PICTURE X(6).                                    CI0247
           05  CODUTI  PICTURE X(8).                                    CI0247
           05  TIMGN   PICTURE X(8).                                    CI0247
           05  PROGE   PICTURE X(8).                                    CI0247
           05  COBASE  PICTURE X(4).                                    CI0247
           05  DATGNC  PICTURE X(10).                                   CI0247
           05  RELEAS  PICTURE X(7).                                    CI0247
           05  DATGE   PICTURE X(10).                                   CI0247
           05  DATSQ   PICTURE X(10).                                   CI0247
       01  DATCE.                                                       CI0247
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0247
         05  DATOR.                                                     CI0247
           10  DATOA  PICTURE XX.                                       CI0247
           10  DATOM  PICTURE XX.                                       CI0247
           10  DATOJ  PICTURE XX.                                       CI0247
       01   VARIABLES-CONDITIONNELLES.                                  CI0247
            05                  FT      PICTURE X VALUE '0'.            CI0247
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0247
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0247
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J50EER PICTURE S9(4) VALUE  ZERO.
            05           J50GGR PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0247
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
      ** PCB POINTER FOR GR1P                                           ADU015
            05 PCB-GR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=XA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XA00.                                         CI0247
          05              XA00-SUITE.                                   CI0247
            15       FILLER         PICTURE  X(00106).                  CI0247
       01                 XA06  REDEFINES      XA00.                    CI0247
            10            XA06-XDBPCB.                                  CI0247
            11            XA06-XDBDNM PICTURE  X(08).                   CI0247
            11            XA06-XSEGLV PICTURE  X(02).                   CI0247
            11            XA06-XRC    PICTURE  X(02).                   CI0247
            11            XA06-XPROPT PICTURE  X(04).                   CI0247
            11            XA06-FILLER PICTURE  S9(5)                    CI0247
                          BINARY.                                       CI0247
            11            XA06-XSEGNM PICTURE  X(08).                   CI0247
            11            XA06-XKEYLN PICTURE  S9(05)                   CI0247
                          BINARY.                                       CI0247
            11            XA06-XSEGNB PICTURE  9(05)                    CI0247
                          BINARY.                                       CI0247
            11            XA06-XCOKEY PICTURE  X(70).                   CI0247
      *** PCB MASK FOR GR1P                                             ADU015
      *!WF DSP=XB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XB00.                                         CI0247
          05              XB00-SUITE.                                   CI0247
            15       FILLER         PICTURE  X(00106).                  CI0247
       01                 XB06  REDEFINES      XB00.                    CI0247
            10            XB06-XDBPCB.                                  CI0247
            11            XB06-XDBDNM PICTURE  X(08).                   CI0247
            11            XB06-XSEGLV PICTURE  X(02).                   CI0247
            11            XB06-XRC    PICTURE  X(02).                   CI0247
            11            XB06-XPROPT PICTURE  X(04).                   CI0247
            11            XB06-FILLER PICTURE  S9(5)                    CI0247
                          BINARY.                                       CI0247
            11            XB06-XSEGNM PICTURE  X(08).                   CI0247
            11            XB06-XKEYLN PICTURE  S9(05)                   CI0247
                          BINARY.                                       CI0247
            11            XB06-XSEGNB PICTURE  9(05)                    CI0247
                          BINARY.                                       CI0247
            11            XB06-XCOKEY PICTURE  X(70).                   CI0247
      ******************************************************************
      **    LINKAGE SECTION FROM CALLING MODULE
      ******************************************************************
      **
      *!WF DSP=QT DSL=QT SEL=47 FOR=I LEV=1 PLT=75
       01                 QT00.                                         CI0247
          05              QT00-SUITE.                                   CI0247
            15       FILLER         PICTURE  X(00091).                  CI0247
       01                 QT47  REDEFINES      QT00.                    CI0247
            10            QT47-CLID   PICTURE  X(23).                   CI0247
            10            QT47-GRID   PICTURE  X(13).                   CI0247
            10            QT47-CTID   PICTURE  X(27).                   CI0247
            10            QT47-CLID4  PICTURE  X(23).                   CI0247
            10            QT47-NARRS  PICTURE  S9(3)                    CI0247
                          COMPUTATIONAL-3.                              CI0247
            10            QT47-GBACH.                                   CI0247
            11            QT47-IAINDV PICTURE  X.                       CI0247
            11            QT47-IAINDA PICTURE  X.                       CI0247
            11            QT47-IAIND1 PICTURE  X.                       CI0247
      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0247
          05              DE00-SUITE.                                   CI0247
            15       FILLER         PICTURE  X(00653).                  CI0247
       01                 DE10  REDEFINES      DE00.                    CI0247
            10            DE10-DU11.                                    CI0247
            11            DE10-XFONC  PICTURE  X(4).                    CI0247
            11            DE10-MPSBN  PICTURE  X(8).                    CI0247
            11            DE10-XDBDNM PICTURE  X(08).                   CI0247
            11            DE10-XSEGNM PICTURE  X(08).                   CI0247
            11            DE10-XRC    PICTURE  X(02).                   CI0247
            11            DE10-MSEG   PICTURE  X(08).                   CI0247
            11            DE10-XCOKEY PICTURE  X(70).                   CI0247
            11            DE10-CUIBR  PICTURE  X(01).                   CI0247
            11            DE10-CUIBA  PICTURE  X(01).                   CI0247
            11            DE10-IPBIK  PICTURE  X(1).                    CI0247
            10            DE10-DU03.                                    CI0247
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0247
                          COMPUTATIONAL-3.                              CI0247
            11            DE10-CMSSF  PICTURE  XX.                      CI0247
            11            DE10-DU09.                                    CI0247
            12            DE10-CMESA  PICTURE  S9(9)                    CI0247
                          BINARY.                                       CI0247
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0247
                          BINARY.                                       CI0247
            12            DE10-CMESB  PICTURE  S9(9)                    CI0247
                          BINARY.                                       CI0247
            12            DE10-CMSST  PICTURE  S9(9)                    CI0247
                          BINARY.                                       CI0247
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0247
                          BINARY.                                       CI0247
            12            DE10-QELLAA PICTURE  S9(9)                    CI0247
                          BINARY.                                       CI0247
            12            DE10-TMESS4 PICTURE  X(512).                  CI0247
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0247
          05              MS00-SUITE.                                   CI0247
            15       FILLER         PICTURE  X(00542).                  CI0247
       01                 MS03  REDEFINES      MS00.                    CI0247
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0247
                          COMPUTATIONAL-3.                              CI0247
            10            MS03-CMSSF  PICTURE  XX.                      CI0247
            10            MS03-DU09.                                    CI0247
            11            MS03-CMESA  PICTURE  S9(9)                    CI0247
                          BINARY.                                       CI0247
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0247
                          BINARY.                                       CI0247
            11            MS03-CMESB  PICTURE  S9(9)                    CI0247
                          BINARY.                                       CI0247
            11            MS03-CMSST  PICTURE  S9(9)                    CI0247
                          BINARY.                                       CI0247
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0247
                          BINARY.                                       CI0247
            11            MS03-QELLAA PICTURE  S9(9)                    CI0247
                          BINARY.                                       CI0247
            11            MS03-TMESS4 PICTURE  X(512).                  CI0247
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0247
            10            MX11-QMSGS  PICTURE  9(03).                   CI0247
            10            MX11-PJ09                                     CI0247
                          OCCURS       025     TIMES.                   CI0247
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0247
                          COMPUTATIONAL-3.                              CI0247
            11            MX11-CMESB  PICTURE  S9(9)                    CI0247
                          BINARY.                                       CI0247
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                QT47
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N0TSC.    NOTE *SET ADDRESSES FOR PCB LINKAGE      *.
       F0TSC.                                                           lv10
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF XA06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR GR1P                                             DOT
           SET ADDRESS OF XB06 TO                                       ADU015
                PCB-GR1P-PTR1.                                          ADU015
       F0TSC-FN. EXIT.
      *N01.      NOTE *************************************.            CI0247
      *               *                                   *             CI0247
      *               *INITIALISATIONS                    *             CI0247
      *               *                                   *             CI0247
      *               *************************************.            CI0247
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0247
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0247
      *               *                                   *             CI0247
      *               *FIN DE TRAITEMENT                  *             CI0247
      *               *                                   *             CI0247
      *               *************************************.            CI0247
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0247
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATE PARAMETERS                *
      *               *                                   *
      *               *************************************.
       F35.           EXIT.                                             lv05
      *N35CC.    NOTE *CLIENT REQUIRED                    *.
       F35CC.    IF    QT47-CLID = 0                                    lv10
                 NEXT SENTENCE ELSE GO TO     F35CC-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012002 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35CC-FN. EXIT.
      *N35CE.    NOTE *ACCOUNT REQUIRED                   *.
       F35CE.    IF    QT47-CTID = 0                                    lv10
                 NEXT SENTENCE ELSE GO TO     F35CE-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012004 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35CE-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *INITIAL DATA ACCESS                *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40DA.    NOTE *CALL CI0020 - CAMS ACCTG DATE      *.            AM0020
       F40DA.                                                           lv10
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
       F40DA-FN. EXIT.
      *N40FA.    NOTE *INITIALIZE RETURN VALUES           *.
       F40FA.                                                           lv10
           MOVE ALL    'N' TO QT47-GBACH.
       F40FA-FN. EXIT.
       F40-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *VIEW / ADD                         *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50CA.    NOTE *CALL CI0018 TO GET ROLES ON ACCT   *.
       F50CA.                                                           lv10
           PERFORM     F91HB THRU F91HB-FN.
       F50CA-FN. EXIT.
      *N50EC.    NOTE *IS CLIENT AN OWNER ON THE ACCT ?   *.
       F50EC.         EXIT.                                             lv10
      *N50EE.    NOTE *LOOP ON CLIENT ROLES FOR ACCOUNT   *.
       F50EE.                                                           lv15
           MOVE        1                        TO J50EER
                                    GO TO     F50EE-B.
       F50EE-A.
           ADD         1                        TO J50EER.
       F50EE-B.
           IF          J50EER                   >  PC14-QITEM
                                    GO TO     F50EE-FN.
                 IF    PC14-CLID (J50EER) =                             DOT
                       QT47-CLID
           MOVE        'Y' TO QT47-IAINDV
           MOVE        'Y' TO QT47-IAINDA
               GO TO     F50-FN.
       F50EE-900. GO TO F50EE-A.
       F50EE-FN. EXIT.
       F50EC-FN. EXIT.
      *N50GC.    NOTE *IF NOT, IS CLIENT PART OF GRP ?    *.
       F50GC.    IF    QT47-GRID > 0                                    lv10
                 NEXT SENTENCE ELSE GO TO     F50GC-FN.
      *N50GE.    NOTE *CALL CI0052 FOR GROUP LIST         *.
       F50GE.                                                           lv15
           PERFORM     F91CL THRU F91CL-FN.
       F50GE-FN. EXIT.
      *N50GG.    NOTE *LOOP ON MEMBERS OF GROUP           *.
       F50GG.                                                           lv15
           MOVE        1                        TO J50GGR
                                    GO TO     F50GG-B.
       F50GG-A.
           ADD         1                        TO J50GGR.
       F50GG-B.
           IF          J50GGR                   >  100
                                    GO TO     F50GG-FN.
      *N50GH.    NOTE *AT END OF LIST                     *.
       F50GH.    IF    WZ04-CLIDO (J50GGR) =                            lv20
                       ZEROS
                 NEXT SENTENCE ELSE GO TO     F50GH-FN.
               GO TO     F50-FN.
       F50GH-FN. EXIT.
      *N50GI.    NOTE *CLIENT IS PART OF GROUP            *.
       F50GI.    IF    WZ04-CLID (J50GGR) =                             lv20
                       QT47-CLID
                 NEXT SENTENCE ELSE GO TO     F50GI-FN.
           MOVE        'Y' TO QT47-IAINDV
               GO TO     F50-FN.
       F50GI-FN. EXIT.
       F50GG-900. GO TO F50GG-A.
       F50GG-FN. EXIT.
       F50GC-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *MODIFY                             *
      *               *                                   *
      *               *************************************.
       F55.      IF    QT47-CLID4 > 0                                   lv05
                 NEXT SENTENCE ELSE GO TO     F55-FN.
      *N55CC.    NOTE *REQUEST CLIENT MUST = ARR CLIENT   *.
       F55CC.    IF    QT47-CLID = QT47-CLID4                           lv10
                 NEXT SENTENCE ELSE GO TO     F55CC-FN.
           MOVE        'Y' TO QT47-IAINDV
           MOVE        'Y' TO QT47-IAINDA
           MOVE        'Y' TO QT47-IAIND1.
       F55CC-FN. EXIT.
       F55-FN.   EXIT.
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
      *N91CL.    NOTE *CALL CI0052 - CLIENTS FOR GROUP    *.            AM0052
       F91CL.                                                           lv10
      *                                                                 AM0052
      *********************************                                 AM0052
      ** THIS MODULE WILL READ THE    *                                 AM0052
      ** CAMS GROUP GR01, GR16, &     *                                 AM0052
      ** GR17 SEGMENTS FOR THE GRID   *                                 AM0052
      ** PASSED.                      *                                 AM0052
      *********************************                                 AM0052
      *                                                                 AM0052
           INITIALIZE  DE10-DU03
           INITIALIZE  MS03
           INITIALIZE  WZ04                                             AM0052
           INITIALIZE  MX11                                             AM0052
           MOVE        QT47-GRID TO                                     AM0052
           WZ04-G198                                                    AM0052
           SET CI0052-PCB-GR1P-PTR1 TO                                  AM0052
                         PCB-GR1P-PTR1                                  AM0052
           INITIALIZE  DE10-DU03                                        AM0052
           CALL        CI0052 USING                                     AM0052
           DFHEIBLK                                                     AM0052
           DFHCOMMAREA                                                  AM0052
           DLIUIBII                                                     AM0052
           CI0052-PCB-ADDRESS-LIST                                      AM0052
           WZ04                                                         AM0052
           DE10                                                         AM0052
           MS03                                                         AM0052
           MX11.                                                        AM0052
       F91CL-FN. EXIT.
      *N91HB.    NOTE *CALL CI0018 - ACCT CLIENTS         *.            AM0018
       F91HB.                                                           lv10
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
           INITIALIZE  DE10-DU03
           INITIALIZE  MS03
           MOVE        QT47-CTID TO PC14-CTID                           AM0018
           MOVE        NS20-DCACG TO PC14-DCACG                         AM0018
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
       F91HB-FN. EXIT.
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
