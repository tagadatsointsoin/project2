       IDENTIFICATION DIVISION.                                         CI0091
       PROGRAM-ID.  CI0091P.                                            CI0091
      *AUTHOR.         CLIENT ARRANGEMENT INFO UPDATE.                  CI0091
      *DATE-COMPILED.   09/08/14.                                       CI0091
       ENVIRONMENT DIVISION.                                            CI0091
       CONFIGURATION SECTION.                                           CI0091
       SOURCE-COMPUTER. IBM-370.                                        CI0091
       OBJECT-COMPUTER. IBM-370.                                        CI0091
       DATA DIVISION.                                                   CI0091
       WORKING-STORAGE SECTION.                                         CI0091
      *                                                                 ADU031
      ******************************************************************ADU031
      **        ACF2 COMMON AREA FOR CALLING ACF2 MODULES              *ADU031
      ******************************************************************ADU031
      *                                                                 ADU031
         COPY ACFUAREA.                                                 ADU031
      *                                                                 ADU031
      *                                                                 ADU031
      *                                                                 ADU031
      *                                                                 ADU031
      *>>>>>>> Audit Log Work Area                                      ADU165
                                                                        ADU165
       01               AL00-ADDR.                                      ADU165
              05        AL00-NPNTR     USAGE IS POINTER.                ADU165
                                                                        ADU165
      *!WI pl=AL005                                                     ADU165
       01               AL00-NSEQ2P    VALUE ZERO                       ADU165
                        PICTURE S9(3)                                   CI0091
                          COMPUTATIONAL-3.                              CI0091
                                                                        ADU165
      *>>>>>>> Linkage Area for Logger Program DBI110                   ADU165
      *!WF DSP=DH DSL=DH SEL=10 FOR=I DES=2 LEV=1                       ADU165
       01                 DH10.                                         CI0091
            10            DH10-GERTC  PICTURE  X                        CI0091
                          VALUE                SPACE.                   CI0091
            10            DH10-XUIBP  PICTURE  S9(8)                    CI0091
                          VALUE                ZERO                     CI0091
                          BINARY.                                       CI0091
            10            DH10-NSEQ2P PICTURE  S9(3)                    CI0091
                          VALUE                ZERO                     CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            DH10-CAUL   PICTURE  X                        CI0091
                          VALUE                SPACE.                   CI0091
            10            DH10-MAUSB  PICTURE  X(8)                     CI0091
                          VALUE                SPACE.                   CI0091
            10            DH10-NAUSK  PICTURE  X(50)                    CI0091
                          VALUE                SPACE.                   CI0091
            10            DH10-CSYS   PICTURE  X(4)                     CI0091
                          VALUE                SPACE.                   CI0091
            10            DH10-CAPPL  PICTURE  X(8)                     CI0091
                          VALUE                SPACE.                   CI0091
            10            DH10-CAUSR  PICTURE  X                        CI0091
                          VALUE                SPACE.                   CI0091
            10            DH10-CAUFR  PICTURE  S9(5)                    CI0091
                          VALUE                ZERO                     CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            DH10-CAUAC  PICTURE  S9(5)                    CI0091
                          VALUE                ZERO                     CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            DH10-GEOPID PICTURE  X(6)                     CI0091
                          VALUE                SPACE.                   CI0091
            10            DH10-CAUNIT PICTURE  X(4)                     CI0091
                          VALUE                SPACE.                   CI0091
            10            DH10-GAUVR  PICTURE  X(400)                   CI0091
                          VALUE                SPACE.                   CI0091

      *>>>>>>> Before and after images for CX03 and CX06 records
      *!WF DSP=VA DSL=VA SEL=0506 FOR=I DES=2 LEV=1
      * PLT=AL
       01                 VA05.                                         CI0091
            10            VA05-K11A.                                    CI0091
            11            VA05-CX03K.                                   CI0091
            12            VA05-CARTY  PICTURE  99                       CI0091
                          VALUE                ZERO.                    CI0091
            12            VA05-NARRS  PICTURE  S9(3)                    CI0091
                          VALUE                ZERO                     CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            VA05-CX06K.                                   CI0091
            12            VA05-CTIDA1 PICTURE  9(3)                     CI0091
                          VALUE                ZERO.                    CI0091
            12            VA05-NACID1 PICTURE  X(24)                    CI0091
                          VALUE                SPACE.                   CI0091
            10            VA05-CARST  PICTURE  99                       CI0091
                          VALUE                ZERO.                    CI0091
            10            VA05-GECSQ  PICTURE  S9(3)                    CI0091
                          VALUE                ZERO                     CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            VA05-CPMTG  PICTURE  99                       CI0091
                          VALUE                ZERO.                    CI0091
            10            VA05-GRCRNG PICTURE  9(3)                     CI0091
                          VALUE                ZERO.                    CI0091
            10            VA05-DEXDT  PICTURE  9(8)                     CI0091
                          VALUE                ZERO.                    CI0091
            10            VA05-DASUP  PICTURE  9(8)                     CI0091
                          VALUE                ZERO.                    CI0091
            10            VA05-FILLER PICTURE  X(20)                    CI0091
                          VALUE                SPACE.                   CI0091
            10            VA05-CY50.                                    CI0091
            11            VA05-NARID  PICTURE  X(30)                    CI0091
                          VALUE                SPACE.                   CI0091
            10            VA05-CY51                                     CI0091
                          REDEFINES            VA05-CY50.               CI0091
            11            VA05-NDIDN  PICTURE  9(12).                   CI0091
            11            VA05-FILLER PICTURE  X(18).                   CI0091
       01                 VA06.                                         CI0091
            10            VA06-K11A.                                    CI0091
            11            VA06-CX03K.                                   CI0091
            12            VA06-CARTY  PICTURE  99                       CI0091
                          VALUE                ZERO.                    CI0091
            12            VA06-NARRS  PICTURE  S9(3)                    CI0091
                          VALUE                ZERO                     CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            VA06-CX06K.                                   CI0091
            12            VA06-CTIDA1 PICTURE  9(3)                     CI0091
                          VALUE                ZERO.                    CI0091
            12            VA06-NACID1 PICTURE  X(24)                    CI0091
                          VALUE                SPACE.                   CI0091
      *!WF DSP=VB DSL=VB SEL=05 FOR=I DES=2 LEV=1 PLT=AL
       01                 VB05.                                         CI0091
            10            VB05-VB01.                                    CI0091
            11            VB05-CX03K.                                   CI0091
            12            VB05-CARTY  PICTURE  99                       CI0091
                          VALUE                ZERO.                    CI0091
            12            VB05-NARRS  PICTURE  S9(3)                    CI0091
                          VALUE                ZERO                     CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            VB05-CX06K.                                   CI0091
            12            VB05-CTIDA1 PICTURE  9(3)                     CI0091
                          VALUE                ZERO.                    CI0091
            12            VB05-NACID1 PICTURE  X(24)                    CI0091
                          VALUE                SPACE.                   CI0091
            10            VB05-CARST  PICTURE  99                       CI0091
                          VALUE                ZERO.                    CI0091
            10            VB05-GECSQ  PICTURE  S9(3)                    CI0091
                          VALUE                ZERO                     CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            VB05-CPMTG  PICTURE  99                       CI0091
                          VALUE                ZERO.                    CI0091
            10            VB05-GRCRNG PICTURE  9(3)                     CI0091
                          VALUE                ZERO.                    CI0091
            10            VB05-DEXDT  PICTURE  9(8)                     CI0091
                          VALUE                ZERO.                    CI0091
            10            VB05-DASUP  PICTURE  9(8)                     CI0091
                          VALUE                ZERO.                    CI0091
            10            VB05-FILLER PICTURE  X(20)                    CI0091
                          VALUE                SPACE.                   CI0091
            10            VB05-CY50.                                    CI0091
            11            VB05-NARID  PICTURE  X(30)                    CI0091
                          VALUE                SPACE.                   CI0091
            10            VB05-CY51                                     CI0091
                          REDEFINES            VB05-CY50.               CI0091
            11            VB05-NDIDN  PICTURE  9(12).                   CI0091
            11            VB05-FILLER PICTURE  X(18).                   CI0091
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0020           PIC X(8) VALUE 'CI0020P '.                  AM0020
       01                 CX00.                                         CI0091
            02            CX01.                                         CI0091
            10            CX01-CX01K.                                   CI0091
            11            CX01-C199.                                    CI0091
            12            CX01-CLID.                                    CI0091
            13            CX01-CLIDO  PICTURE  9(3).                    CI0091
            13            CX01-CLIDN.                                   CI0091
            14            CX01-CLIDNP PICTURE  X(12).                   CI0091
            14            CX01-CLIDND PICTURE  9(8).                    CI0091
            10            CX01-GEMDA  PICTURE  9(8).                    CI0091
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0091
                          BINARY.                                       CI0091
            10            CX01-FILLER PICTURE  X(5).                    CI0091
            02            CX03.                                         CI0091
            10            CX03-GELL   PICTURE  9(4)                     CI0091
                          BINARY.                                       CI0091
            10            CX03-CY00.                                    CI0091
            11            CX03-CX03K.                                   CI0091
            12            CX03-CARTY  PICTURE  99.                      CI0091
            12            CX03-NARRS  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            CX03-CARST  PICTURE  99.                      CI0091
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            CX03-CPMTG  PICTURE  99.                      CI0091
            11            CX03-GRCRNG PICTURE  9(3).                    CI0091
            11            CX03-DEXDT  PICTURE  9(8).                    CI0091
            11            CX03-DASUP  PICTURE  9(8).                    CI0091
            11            CX03-CSTEC  PICTURE  X(3).                    CI0091
            11            CX03-FILLER PICTURE  X(17).                   CI0091
            11            CX03-CY50.                                    CI0091
            12            CX03-NARID  PICTURE  X(30).                   CI0091
            11            CX03-CY51                                     CI0091
                          REDEFINES            CX03-CY50.               CI0091
            12            CX03-NDIDN  PICTURE  9(12).                   CI0091
            12            CX03-FILLER PICTURE  X(18).                   CI0091
            11            CX03-CY52                                     CI0091
                          REDEFINES            CX03-CY50.               CI0091
            12            CX03-NAIDC  PICTURE  9(12).                   CI0091
            12            CX03-FILLER PICTURE  X(18).                   CI0091
            11            CX03-CY53                                     CI0091
                          REDEFINES            CX03-CY50.               CI0091
            12            CX03-NAMEXB PICTURE  9(15).                   CI0091
            12            CX03-FILLER PICTURE  X(15).                   CI0091
            10            CX03-CY99.                                    CI0091
            11            CX03-FILLER PICTURE  X(109).                  CI0091
            10            CX03-CY01                                     CI0091
                          REDEFINES            CX03-CY99.               CI0091
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            CX03-ICPCI  PICTURE  X.                       CI0091
            11            CX03-CLUPD  PICTURE  9(3).                    CI0091
            11            CX03-DLAUP  PICTURE  9(8).                    CI0091
            11            CX03-CWRC   PICTURE  99.                      CI0091
            11            CX03-CHCR   PICTURE  99.                      CI0091
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0091
            11            CX03-GEAUN  PICTURE  9(5).                    CI0091
            11            CX03-DPCHD  PICTURE  9(8).                    CI0091
            11            CX03-DLRCHK PICTURE  9(8).                    CI0091
            11            CX03-QTRCHK PICTURE  9(2).                    CI0091
            11            CX03-DNPMT  PICTURE  9(8).                    CI0091
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            CX03-CY02                                     CI0091
                          REDEFINES            CX03-CY99.               CI0091
            11            CX03-QSIRQ  PICTURE  99.                      CI0091
            11            CX03-QDRMN  PICTURE  9(2)                     CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            CX03-DDPRE  PICTURE  9(8).                    CI0091
            11            CX03-DDSHP  PICTURE  9(8).                    CI0091
            11            CX03-NDRFTB PICTURE  9(5).                    CI0091
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0091
            11            CX03-DDSHPA PICTURE  9(8).                    CI0091
            11            CX03-NDRFTF PICTURE  9(5).                    CI0091
            11            CX03-QDIPBK PICTURE  9(3).                    CI0091
            11            CX03-CREOR  PICTURE  X(1).                    CI0091
            11            CX03-CREOR1 PICTURE  X(1).                    CI0091
            11            CX03-DDASC  PICTURE  9(8).                    CI0091
            11            CX03-FILLER PICTURE  X(7).                    CI0091
            10            CX03-CY03                                     CI0091
                          REDEFINES            CX03-CY99.               CI0091
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0091
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0091
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0091
            11            CX03-DOPDA  PICTURE  99.                      CI0091
            11            CX03-CPMTF  PICTURE  99.                      CI0091
            11            CX03-CIRMO  PICTURE  X(12).                   CI0091
            11            CX03-CPALL  PICTURE  X(1).                    CI0091
            11            CX03-CCOLM  PICTURE  9(2).                    CI0091
            11            CX03-CBLTP  PICTURE  X(1).                    CI0091
            11            CX03-CASUB  PICTURE  9(2).                    CI0091
            11            CX03-CBLFM  PICTURE  9(2).                    CI0091
            11            CX03-IBILS  PICTURE  X.                       CI0091
            11            CX03-IPAOS  PICTURE  X.                       CI0091
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0091
            11            CX03-DLBPD  PICTURE  9(8).                    CI0091
            11            CX03-DNBPD  PICTURE  9(8).                    CI0091
            11            CX03-DODBD  PICTURE  9(8).                    CI0091
            11            CX03-CPSRE  PICTURE  99.                      CI0091
            11            CX03-ISPHN  PICTURE  X.                       CI0091
            11            CX03-TCARR  PICTURE  X(6).                    CI0091
            11            CX03-CBKPT  PICTURE  9(2).                    CI0091
            11            CX03-IECNT  PICTURE  X.                       CI0091
            11            CX03-ICONV  PICTURE  X(1).                    CI0091
            11            CX03-FILLER PICTURE  X(4).                    CI0091
            10            CX03-CY04                                     CI0091
                          REDEFINES            CX03-CY99.               CI0091
            11            CX03-CCARD  PICTURE  X(02).                   CI0091
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0091
            11            CX03-IREMT  PICTURE  X(01).                   CI0091
            11            CX03-ISBILA PICTURE  X.                       CI0091
            11            CX03-DLBPDA PICTURE  9(8).                    CI0091
            11            CX03-DNBPDA.                                  CI0091
            12            CX03-DNCYM  PICTURE  9(6).                    CI0091
            12            CX03-CEDTD  PICTURE  9(2).                    CI0091
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            CX03-DREMT  PICTURE  9(8).                    CI0091
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0091
            11            CX03-CWRC2  PICTURE  99.                      CI0091
            11            CX03-CHCR2  PICTURE  99.                      CI0091
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0091
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0091
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0091
            02            CX06.                                         CI0091
            10            CX06-CX06K.                                   CI0091
            11            CX06-C299.                                    CI0091
            12            CX06-CTID.                                    CI0091
            13            CX06-CTIDA  PICTURE  9(3).                    CI0091
            13            CX06-CTIDN.                                   CI0091
            14            CX06-CTIDNP PICTURE  X(13).                   CI0091
            14            CX06-CTIDND PICTURE  9(11).                   CI0091
            10            CX06-NPECK  PICTURE  9(02).                   CI0091
            10            CX06-FILLER PICTURE  X.                       CI0091
      *                                                                 ADU155
      ******************************************************************ADU155
      ** WORK AREA NEEDED FOR MACRO ADU155                             *ADU155
      **        DATE COMMON AREA FOR EXECUTING CICS ASKTIME/FORMATTIME *ADU155
      ******************************************************************ADU155
      *                                                                 ADU155
      *!WI pl=DD100                                                     ADU155
       01  DD01-XMSTS                                                   ADU155
                        PICTURE S9(15)                                  CI0091
                          COMPUTATIONAL-3.                              CI0091
       01  DD01-F2CCYY             PIC S9(08) COMP.                     ADU155
      *!WI pl=DD200                                                     ADU155
       01  DD01-XDAT69                                                  ADU155
                        PICTURE 9(6).                                   CI0091
       01  DD01-UDATE.                                                  ADU155
           05  DD01-YEAR           PIC  9(04).                          ADU155
           05  DD01-MMDD           PIC  9(04).                          ADU155
      *!WI pl=DD280                                                     ADU155
       01  DD01-XDATCU REDEFINES DD01-UDATE                             ADU155
                        PICTURE X(8).                                   CI0091
      *                                                                 ADU155
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0091
            10            XW05-XW06.                                    CI0091
            11            XW05-XDBPCB.                                  CI0091
            12            XW05-XDBDNM PICTURE  X(08)                    CI0091
                          VALUE                SPACE.                   CI0091
            12            XW05-XSEGLV PICTURE  X(02)                    CI0091
                          VALUE                SPACE.                   CI0091
            12            XW05-XRC    PICTURE  X(02)                    CI0091
                          VALUE                SPACE.                   CI0091
            12            XW05-XPROPT PICTURE  X(04)                    CI0091
                          VALUE                SPACE.                   CI0091
            12            XW05-FILLER PICTURE  S9(5)                    CI0091
                          VALUE                ZERO                     CI0091
                          BINARY.                                       CI0091
            12            XW05-XSEGNM PICTURE  X(08)                    CI0091
                          VALUE                SPACE.                   CI0091
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0091
                          VALUE                ZERO                     CI0091
                          BINARY.                                       CI0091
            12            XW05-XSEGNB PICTURE  9(05)                    CI0091
                          VALUE                ZERO                     CI0091
                          BINARY.                                       CI0091
            12            XW05-XCOKEY PICTURE  X(70)                    CI0091
                          VALUE                SPACE.                   CI0091
            10            XW05-XW07.                                    CI0091
            11            XW05-XIOPCB.                                  CI0091
            12            XW05-XTERMI PICTURE  X(08)                    CI0091
                          VALUE                SPACE.                   CI0091
            12            XW05-FILLER PICTURE  XX                       CI0091
                          VALUE                SPACE.                   CI0091
            12            XW05-XRC1   PICTURE  X(02)                    CI0091
                          VALUE                SPACE.                   CI0091
            12            XW05-FILLER PICTURE  X(12)                    CI0091
                          VALUE                SPACE.                   CI0091
            12            XW05-XMODNM PICTURE  X(8)                     CI0091
                          VALUE                SPACE.                   CI0091
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0091
                          VALUE                ZERO.                    CI0091
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0091
                          VALUE                ZERO.                    CI0091
            10            XW05-XGU    PICTURE  X(4)                     CI0091
                          VALUE                'GU  '.                  CI0091
            10            XW05-XGHU   PICTURE  X(4)                     CI0091
                          VALUE                'GHU '.                  CI0091
            10            XW05-XGN    PICTURE  X(4)                     CI0091
                          VALUE                'GN  '.                  CI0091
            10            XW05-XGHN   PICTURE  X(4)                     CI0091
                          VALUE                'GHN '.                  CI0091
            10            XW05-XGNP   PICTURE  X(4)                     CI0091
                          VALUE                'GNP '.                  CI0091
            10            XW05-XGHNP  PICTURE  X(4)                     CI0091
                          VALUE                'GHNP'.                  CI0091
            10            XW05-XREPL  PICTURE  XXXX                     CI0091
                          VALUE                'REPL'.                  CI0091
            10            XW05-XISRT  PICTURE  X(4)                     CI0091
                          VALUE                'ISRT'.                  CI0091
            10            XW05-XDLET  PICTURE  X(4)                     CI0091
                          VALUE                'DLET'.                  CI0091
            10            XW05-XOPEN  PICTURE  X(4)                     CI0091
                          VALUE                'OPEN'.                  CI0091
            10            XW05-XCLSE  PICTURE  X(4)                     CI0091
                          VALUE                'CLSE'.                  CI0091
            10            XW05-XCHKP  PICTURE  X(4)                     CI0091
                          VALUE                'CHKP'.                  CI0091
            10            XW05-XXRST  PICTURE  X(4)                     CI0091
                          VALUE                'XRST'.                  CI0091
            10            XW05-XTERM  PICTURE  X(4)                     CI0091
                          VALUE                'TERM'.                  CI0091
            10            XW05-XNFPAC PICTURE  X(13)                    CI0091
                          VALUE                SPACE.                   CI0091
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0091
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0091
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
       01                 NS00.                                         CI0091
          05              NS00-00.                                      CI0091
            10            NS00-NS00K.                                   CI0091
            11            NS00-PRCSTK PICTURE  XX.                      CI0091
          05              NS00-SUITE.                                   CI0091
            15       FILLER         PICTURE  X(00078).                  CI0091
       01                 NS20  REDEFINES      NS00.                    CI0091
            10       FILLER         PICTURE  X(00002).                  CI0091
            10            NS20-DCACG  PICTURE  9(8).                    CI0091
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            NS20-CCDAT  PICTURE  X(8).                    CI0091
            10            NS20-DCALP  PICTURE  X(12).                   CI0091
            10            NS20-DNACG  PICTURE  9(8).                    CI0091
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            NS20-CNDAT  PICTURE  X(8).                    CI0091
            10            NS20-DNALP  PICTURE  X(12).                   CI0091
            10            NS20-DCACD  PICTURE  X(10).                   CI0091
            10            NS20-FILLER PICTURE  X(4).                    CI0091
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *>>>>> Copy segments for use when determining what to
      *      write out to the audit logs...
      *!WF DSP=WX DSL=CX SEL=03 FOR=I LEV=1 PLT=WS
       01                 WX00.                                         CI0091
          05              WX00-SUITE.                                   CI0091
            15       FILLER         PICTURE  X(00190).                  CI0091
       01                 WX03  REDEFINES      WX00.                    CI0091
            10            WX03-GELL   PICTURE  9(4)                     CI0091
                          BINARY.                                       CI0091
            10            WX03-CY00.                                    CI0091
            11            WX03-CX03K.                                   CI0091
            12            WX03-CARTY  PICTURE  99.                      CI0091
            12            WX03-NARRS  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            WX03-CARST  PICTURE  99.                      CI0091
            11            WX03-GECSQ  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            WX03-CPMTG  PICTURE  99.                      CI0091
            11            WX03-GRCRNG PICTURE  9(3).                    CI0091
            11            WX03-DEXDT  PICTURE  9(8).                    CI0091
            11            WX03-DASUP  PICTURE  9(8).                    CI0091
            11            WX03-CSTEC  PICTURE  X(3).                    CI0091
            11            WX03-FILLER PICTURE  X(17).                   CI0091
            11            WX03-CY50.                                    CI0091
            12            WX03-NARID  PICTURE  X(30).                   CI0091
            11            WX03-CY51                                     CI0091
                          REDEFINES            WX03-CY50.               CI0091
            12            WX03-NDIDN  PICTURE  9(12).                   CI0091
            12            WX03-FILLER PICTURE  X(18).                   CI0091
            11            WX03-CY52                                     CI0091
                          REDEFINES            WX03-CY50.               CI0091
            12            WX03-NAIDC  PICTURE  9(12).                   CI0091
            12            WX03-FILLER PICTURE  X(18).                   CI0091
            11            WX03-CY53                                     CI0091
                          REDEFINES            WX03-CY50.               CI0091
            12            WX03-NAMEXB PICTURE  9(15).                   CI0091
            12            WX03-FILLER PICTURE  X(15).                   CI0091
            10            WX03-CY99.                                    CI0091
            11            WX03-FILLER PICTURE  X(109).                  CI0091
            10            WX03-CY01                                     CI0091
                          REDEFINES            WX03-CY99.               CI0091
            11            WX03-NBASQ  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            WX03-ICPCI  PICTURE  X.                       CI0091
            11            WX03-CLUPD  PICTURE  9(3).                    CI0091
            11            WX03-DLAUP  PICTURE  9(8).                    CI0091
            11            WX03-CWRC   PICTURE  99.                      CI0091
            11            WX03-CHCR   PICTURE  99.                      CI0091
            11            WX03-GEOPD2 PICTURE  X(8).                    CI0091
            11            WX03-GEAUN  PICTURE  9(5).                    CI0091
            11            WX03-DPCHD  PICTURE  9(8).                    CI0091
            11            WX03-DLRCHK PICTURE  9(8).                    CI0091
            11            WX03-QTRCHK PICTURE  9(2).                    CI0091
            11            WX03-DNPMT  PICTURE  9(8).                    CI0091
            11            WX03-APMTLA PICTURE  S9(9)V99                 CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            WX03-CY02                                     CI0091
                          REDEFINES            WX03-CY99.               CI0091
            11            WX03-QSIRQ  PICTURE  99.                      CI0091
            11            WX03-QDRMN  PICTURE  9(2)                     CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            WX03-DDPRE  PICTURE  9(8).                    CI0091
            11            WX03-DDSHP  PICTURE  9(8).                    CI0091
            11            WX03-NDRFTB PICTURE  9(5).                    CI0091
            11            WX03-QDIPBJ PICTURE  9(3).                    CI0091
            11            WX03-DDSHPA PICTURE  9(8).                    CI0091
            11            WX03-NDRFTF PICTURE  9(5).                    CI0091
            11            WX03-QDIPBK PICTURE  9(3).                    CI0091
            11            WX03-CREOR  PICTURE  X(1).                    CI0091
            11            WX03-CREOR1 PICTURE  X(1).                    CI0091
            11            WX03-DDASC  PICTURE  9(8).                    CI0091
            11            WX03-FILLER PICTURE  X(7).                    CI0091
            10            WX03-CY03                                     CI0091
                          REDEFINES            WX03-CY99.               CI0091
            11            WX03-DLAUP1 PICTURE  9(8).                    CI0091
            11            WX03-GEOPD3 PICTURE  X(8).                    CI0091
            11            WX03-DNPMT1 PICTURE  9(8).                    CI0091
            11            WX03-DOPDA  PICTURE  99.                      CI0091
            11            WX03-CPMTF  PICTURE  99.                      CI0091
            11            WX03-CIRMO  PICTURE  X(12).                   CI0091
            11            WX03-CPALL  PICTURE  X(1).                    CI0091
            11            WX03-CCOLM  PICTURE  9(2).                    CI0091
            11            WX03-CBLTP  PICTURE  X(1).                    CI0091
            11            WX03-CASUB  PICTURE  9(2).                    CI0091
            11            WX03-CBLFM  PICTURE  9(2).                    CI0091
            11            WX03-IBILS  PICTURE  X.                       CI0091
            11            WX03-IPAOS  PICTURE  X.                       CI0091
            11            WX03-CBLSQ  PICTURE  X(4).                    CI0091
            11            WX03-DLBPD  PICTURE  9(8).                    CI0091
            11            WX03-DNBPD  PICTURE  9(8).                    CI0091
            11            WX03-DODBD  PICTURE  9(8).                    CI0091
            11            WX03-CPSRE  PICTURE  99.                      CI0091
            11            WX03-ISPHN  PICTURE  X.                       CI0091
            11            WX03-TCARR  PICTURE  X(6).                    CI0091
            11            WX03-CBKPT  PICTURE  9(2).                    CI0091
            11            WX03-IECNT  PICTURE  X.                       CI0091
            11            WX03-ICONV  PICTURE  X(1).                    CI0091
            11            WX03-FILLER PICTURE  X(4).                    CI0091
            10            WX03-CY04                                     CI0091
                          REDEFINES            WX03-CY99.               CI0091
            11            WX03-CCARD  PICTURE  X(02).                   CI0091
            11            WX03-MCSIG4 PICTURE  X(20).                   CI0091
            11            WX03-IREMT  PICTURE  X(01).                   CI0091
            11            WX03-ISBILA PICTURE  X.                       CI0091
            11            WX03-DLBPDA PICTURE  9(8).                    CI0091
            11            WX03-DNBPDA.                                  CI0091
            12            WX03-DNCYM  PICTURE  9(6).                    CI0091
            12            WX03-CEDTD  PICTURE  9(2).                    CI0091
            11            WX03-AREMT  PICTURE  S9(7)V99                 CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            WX03-DREMT  PICTURE  9(8).                    CI0091
            11            WX03-ADBRQ  PICTURE  S9(11)V99                CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            WX03-CLUPD1 PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            WX03-DLAUP3 PICTURE  9(8).                    CI0091
            11            WX03-CWRC2  PICTURE  99.                      CI0091
            11            WX03-CHCR2  PICTURE  99.                      CI0091
            11            WX03-GEOPD9 PICTURE  X(8).                    CI0091
            11            WX03-GEAUN1 PICTURE  9(5).                    CI0091
            11            WX03-DPCHD1 PICTURE  9(8).                    CI0091
      *>>>>>> Miscellaneous work fields...
      *
      *!WF DSP=WX DSL=CX SEL=03 FOR=I DES=1 LEV=1 PLT=WX
       01   DEBUT-WSS.                                                  CI0091
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0091
            05   IK     PICTURE X.                                      CI0091
       01  CONSTANTES-PAC.                                              CI0091
           05  FILLER  PICTURE X(87)   VALUE                            CI0091
                     '6015 CAT09/08/14CI0091ADMIN   14:34:43CI0091P AMERCI0091
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0091
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0091
           05  NUGNA   PICTURE X(5).                                    CI0091
           05  APPLI   PICTURE X(3).                                    CI0091
           05  DATGN   PICTURE X(8).                                    CI0091
           05  PROGR   PICTURE X(6).                                    CI0091
           05  CODUTI  PICTURE X(8).                                    CI0091
           05  TIMGN   PICTURE X(8).                                    CI0091
           05  PROGE   PICTURE X(8).                                    CI0091
           05  COBASE  PICTURE X(4).                                    CI0091
           05  DATGNC  PICTURE X(10).                                   CI0091
           05  RELEAS  PICTURE X(7).                                    CI0091
           05  DATGE   PICTURE X(10).                                   CI0091
           05  DATSQ   PICTURE X(10).                                   CI0091
       01  DATCE.                                                       CI0091
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0091
         05  DATOR.                                                     CI0091
           10  DATOA  PICTURE XX.                                       CI0091
           10  DATOM  PICTURE XX.                                       CI0091
           10  DATOJ  PICTURE XX.                                       CI0091
       01   VARIABLES-CONDITIONNELLES.                                  CI0091
            05                  FT      PICTURE X VALUE '0'.            CI0091
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0091
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0091
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0091
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0091
       01               S-CX01-SSA.                                     CI0091
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0091
                                      VALUE 'CX01    '.                 CI0091
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0091
            10          S-CX01-CCOD   PICTURE X(5)                      CI0091
                                      VALUE '-----'.                    CI0091
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0091
       01            S-CXU01-SSA.                                       CI0091
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0091
                                      VALUE 'CX01    '.                 CI0091
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0091
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0091
                                      VALUE '-----'.                    CI0091
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0091
                                      VALUE '(CX01K'.                   CI0091
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0091
            10       S-CXU01-CX01K.                                     CI0091
            11       S-CXU01-C199.                                      CI0091
            12       S-CXU01-CLID.                                      CI0091
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0091
            13       S-CXU01-CLIDN.                                     CI0091
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0091
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0091
            10  FILLER   PICTURE X    VALUE ')'.                        CI0091
       01               S-CX03-SSA.                                     CI0091
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0091
                                      VALUE 'CX03    '.                 CI0091
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0091
            10          S-CX03-CCOD   PICTURE X(5)                      CI0091
                                      VALUE '-----'.                    CI0091
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0091
       01            S-CXA03-SSA.                                       CI0091
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0091
                                      VALUE 'CX03    '.                 CI0091
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0091
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0091
                                      VALUE '-----'.                    CI0091
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0091
                                      VALUE '(CARTY'.                   CI0091
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0091
            12       S-CXA03-CARTY    PICTURE  99.                      CI0091
            12  FILLER   PICTURE X    VALUE ')'.                        CI0091
       01            S-CXB03-SSA.                                       CI0091
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0091
                                      VALUE 'CX03    '.                 CI0091
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0091
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0091
                                      VALUE '-----'.                    CI0091
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0091
                                      VALUE '(NARRS'.                   CI0091
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0091
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            12  FILLER   PICTURE X    VALUE ')'.                        CI0091
       01            S-CXC03-SSA.                                       CI0091
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0091
                                      VALUE 'CX03    '.                 CI0091
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0091
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0091
                                      VALUE '-----'.                    CI0091
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0091
                                      VALUE '(CPMTG'.                   CI0091
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0091
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0091
            11  FILLER   PICTURE X    VALUE ')'.                        CI0091
       01            S-CXD03-SSA.                                       CI0091
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0091
                                      VALUE 'CX03    '.                 CI0091
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0091
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0091
                                      VALUE '-----'.                    CI0091
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0091
                                      VALUE '(GRCRNG'.                  CI0091
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0091
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0091
            11  FILLER   PICTURE X    VALUE ')'.                        CI0091
       01            S-CXE03-SSA.                                       CI0091
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0091
                                      VALUE 'CX03    '.                 CI0091
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0091
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0091
                                      VALUE '-----'.                    CI0091
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0091
                                      VALUE '(DEXDT'.                   CI0091
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0091
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0091
            11  FILLER   PICTURE X    VALUE ')'.                        CI0091
       01            S-CXF03-SSA.                                       CI0091
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0091
                                      VALUE 'CX03    '.                 CI0091
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0091
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0091
                                      VALUE '-----'.                    CI0091
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0091
                                      VALUE '(CY50'.                    CI0091
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0091
            11       S-CXF03-CY50.                                      CI0091
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0091
            11  FILLER   PICTURE X    VALUE ')'.                        CI0091
       01            S-CXG03-SSA.                                       CI0091
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0091
                                      VALUE 'CX03    '.                 CI0091
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0091
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0091
                                      VALUE '-----'.                    CI0091
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0091
                                      VALUE '(NBASQ'.                   CI0091
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0091
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11  FILLER   PICTURE X    VALUE ')'.                        CI0091
       01            S-CXH03-SSA.                                       CI0091
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0091
                                      VALUE 'CX03    '.                 CI0091
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0091
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0091
                                      VALUE '-----'.                    CI0091
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0091
                                      VALUE '(NARID'.                   CI0091
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0091
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0091
            12  FILLER   PICTURE X    VALUE ')'.                        CI0091
       01            S-CXU03-SSA.                                       CI0091
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0091
                                      VALUE 'CX03    '.                 CI0091
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0091
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0091
                                      VALUE '-----'.                    CI0091
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0091
                                      VALUE '(CX03K'.                   CI0091
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0091
            11       S-CXU03-CX03K.                                     CI0091
            12       S-CXU03-CARTY    PICTURE  99.                      CI0091
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11  FILLER   PICTURE X    VALUE ')'.                        CI0091
       01               S-CX06-SSA.                                     CI0091
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0091
                                      VALUE 'CX06    '.                 CI0091
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0091
            10          S-CX06-CCOD   PICTURE X(5)                      CI0091
                                      VALUE '-----'.                    CI0091
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0091
       01            S-CXU06-SSA.                                       CI0091
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0091
                                      VALUE 'CX06    '.                 CI0091
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0091
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0091
                                      VALUE '-----'.                    CI0091
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0091
                                      VALUE '(CX06K'.                   CI0091
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0091
            10       S-CXU06-CX06K.                                     CI0091
            11       S-CXU06-C299.                                      CI0091
            12       S-CXU06-CTID.                                      CI0091
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0091
            13       S-CXU06-CTIDN.                                     CI0091
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0091
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0091
            10  FILLER   PICTURE X    VALUE ')'.                        CI0091
       01   ZONES-UTILISATEUR PICTURE X.                                CI0091
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
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR DATP                                           ADU015
            05 PCB-DATP-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0091
          05              PA00-SUITE.                                   CI0091
            15       FILLER         PICTURE  X(00106).                  CI0091
       01                 PA06  REDEFINES      PA00.                    CI0091
            10            PA06-XDBPCB.                                  CI0091
            11            PA06-XDBDNM PICTURE  X(08).                   CI0091
            11            PA06-XSEGLV PICTURE  X(02).                   CI0091
            11            PA06-XRC    PICTURE  X(02).                   CI0091
            11            PA06-XPROPT PICTURE  X(04).                   CI0091
            11            PA06-FILLER PICTURE  S9(5)                    CI0091
                          BINARY.                                       CI0091
            11            PA06-XSEGNM PICTURE  X(08).                   CI0091
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0091
                          BINARY.                                       CI0091
            11            PA06-XSEGNB PICTURE  9(05)                    CI0091
                          BINARY.                                       CI0091
            11            PA06-XCOKEY PICTURE  X(70).                   CI0091
      *** PCB MASK FOR DATP                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0091
          05              PB00-SUITE.                                   CI0091
            15       FILLER         PICTURE  X(00106).                  CI0091
       01                 PB06  REDEFINES      PB00.                    CI0091
            10            PB06-XDBPCB.                                  CI0091
            11            PB06-XDBDNM PICTURE  X(08).                   CI0091
            11            PB06-XSEGLV PICTURE  X(02).                   CI0091
            11            PB06-XRC    PICTURE  X(02).                   CI0091
            11            PB06-XPROPT PICTURE  X(04).                   CI0091
            11            PB06-FILLER PICTURE  S9(5)                    CI0091
                          BINARY.                                       CI0091
            11            PB06-XSEGNM PICTURE  X(08).                   CI0091
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0091
                          BINARY.                                       CI0091
            11            PB06-XSEGNB PICTURE  9(05)                    CI0091
                          BINARY.                                       CI0091
            11            PB06-XCOKEY PICTURE  X(70).                   CI0091
      *-----> SEGMENT LINKAGE FOR INPUT AND OUTPUT
      *
      *!WF DSP=PJ DSL=PJ SEL=08 FOR=I DES=1 LEV=1 PLT=80
       01                 PJ08.                                         CI0091
            10            PJ08-MAPPN  PICTURE  X(10).                   CI0091
            10            PJ08-CLID   PICTURE  X(23).                   CI0091
            10            PJ08-CARTY  PICTURE  99.                      CI0091
            10            PJ08-NARRS  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            PJ08-CTID   PICTURE  X(27).                   CI0091
            10            PJ08-CARTZ  PICTURE  99.                      CI0091
            10            PJ08-NAPDS  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            PJ08-NPISQ  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            PJ08-NBASQ  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            PJ08-CDEL1  PICTURE  9(3).                    CI0091
            10            PJ08-NDELS  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            PJ08-IPERT  PICTURE  X.                       CI0091
            10            PJ08-NEIBT  PICTURE  X(7).                    CI0091
            10            PJ08-GESQ2C PICTURE  S99                      CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            PJ08-CACTM  PICTURE  X(1).                    CI0091
            10            PJ08-CPROT  PICTURE  X(02).                   CI0091
            10            PJ08-DCACG  PICTURE  9(8).                    CI0091
            10            PJ08-GECSQ  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            PJ08-ICX01  PICTURE  X.                       CI0091
            10            PJ08-ICX03  PICTURE  X.                       CI0091
            10            PJ08-ICX06  PICTURE  X.                       CI0091
            10            PJ08-ICX13  PICTURE  X.                       CI0091
            10            PJ08-ICX14  PICTURE  X.                       CI0091
            10            PJ08-ICX18  PICTURE  X.                       CI0091
            10            PJ08-ICX21  PICTURE  X.                       CI0091
            10            PJ08-CPMTF  PICTURE  99.                      CI0091
            10            PJ08-NPAIS  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            PJ08-ICX09  PICTURE  X.                       CI0091
            10            PJ08-FILLER PICTURE  X(22).                   CI0091
      *!WF DSP=LX DSL=CX SEL=010306 FOR=I DES=1 LEV=1
      * PLT=80
       01                 LX01.                                         CI0091
            10            LX01-CX01K.                                   CI0091
            11            LX01-C199.                                    CI0091
            12            LX01-CLID.                                    CI0091
            13            LX01-CLIDO  PICTURE  9(3).                    CI0091
            13            LX01-CLIDN.                                   CI0091
            14            LX01-CLIDNP PICTURE  X(12).                   CI0091
            14            LX01-CLIDND PICTURE  9(8).                    CI0091
            10            LX01-GEMDA  PICTURE  9(8).                    CI0091
            10            LX01-NSEQ4B PICTURE  9(8)                     CI0091
                          BINARY.                                       CI0091
            10            LX01-FILLER PICTURE  X(5).                    CI0091
       01                 LX03.                                         CI0091
            10            LX03-GELL   PICTURE  9(4)                     CI0091
                          BINARY.                                       CI0091
            10            LX03-CY00.                                    CI0091
            11            LX03-CX03K.                                   CI0091
            12            LX03-CARTY  PICTURE  99.                      CI0091
            12            LX03-NARRS  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            LX03-CARST  PICTURE  99.                      CI0091
            11            LX03-GECSQ  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            LX03-CPMTG  PICTURE  99.                      CI0091
            11            LX03-GRCRNG PICTURE  9(3).                    CI0091
            11            LX03-DEXDT  PICTURE  9(8).                    CI0091
            11            LX03-DASUP  PICTURE  9(8).                    CI0091
            11            LX03-CSTEC  PICTURE  X(3).                    CI0091
            11            LX03-FILLER PICTURE  X(17).                   CI0091
            11            LX03-CY50.                                    CI0091
            12            LX03-NARID  PICTURE  X(30).                   CI0091
            11            LX03-CY51                                     CI0091
                          REDEFINES            LX03-CY50.               CI0091
            12            LX03-NDIDN  PICTURE  9(12).                   CI0091
            12            LX03-FILLER PICTURE  X(18).                   CI0091
            11            LX03-CY52                                     CI0091
                          REDEFINES            LX03-CY50.               CI0091
            12            LX03-NAIDC  PICTURE  9(12).                   CI0091
            12            LX03-FILLER PICTURE  X(18).                   CI0091
            11            LX03-CY53                                     CI0091
                          REDEFINES            LX03-CY50.               CI0091
            12            LX03-NAMEXB PICTURE  9(15).                   CI0091
            12            LX03-FILLER PICTURE  X(15).                   CI0091
            10            LX03-CY99.                                    CI0091
            11            LX03-FILLER PICTURE  X(109).                  CI0091
            10            LX03-CY01                                     CI0091
                          REDEFINES            LX03-CY99.               CI0091
            11            LX03-NBASQ  PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            LX03-ICPCI  PICTURE  X.                       CI0091
            11            LX03-CLUPD  PICTURE  9(3).                    CI0091
            11            LX03-DLAUP  PICTURE  9(8).                    CI0091
            11            LX03-CWRC   PICTURE  99.                      CI0091
            11            LX03-CHCR   PICTURE  99.                      CI0091
            11            LX03-GEOPD2 PICTURE  X(8).                    CI0091
            11            LX03-GEAUN  PICTURE  9(5).                    CI0091
            11            LX03-DPCHD  PICTURE  9(8).                    CI0091
            11            LX03-DLRCHK PICTURE  9(8).                    CI0091
            11            LX03-QTRCHK PICTURE  9(2).                    CI0091
            11            LX03-DNPMT  PICTURE  9(8).                    CI0091
            11            LX03-APMTLA PICTURE  S9(9)V99                 CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            LX03-CY02                                     CI0091
                          REDEFINES            LX03-CY99.               CI0091
            11            LX03-QSIRQ  PICTURE  99.                      CI0091
            11            LX03-QDRMN  PICTURE  9(2)                     CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            LX03-DDPRE  PICTURE  9(8).                    CI0091
            11            LX03-DDSHP  PICTURE  9(8).                    CI0091
            11            LX03-NDRFTB PICTURE  9(5).                    CI0091
            11            LX03-QDIPBJ PICTURE  9(3).                    CI0091
            11            LX03-DDSHPA PICTURE  9(8).                    CI0091
            11            LX03-NDRFTF PICTURE  9(5).                    CI0091
            11            LX03-QDIPBK PICTURE  9(3).                    CI0091
            11            LX03-CREOR  PICTURE  X(1).                    CI0091
            11            LX03-CREOR1 PICTURE  X(1).                    CI0091
            11            LX03-DDASC  PICTURE  9(8).                    CI0091
            11            LX03-FILLER PICTURE  X(7).                    CI0091
            10            LX03-CY03                                     CI0091
                          REDEFINES            LX03-CY99.               CI0091
            11            LX03-DLAUP1 PICTURE  9(8).                    CI0091
            11            LX03-GEOPD3 PICTURE  X(8).                    CI0091
            11            LX03-DNPMT1 PICTURE  9(8).                    CI0091
            11            LX03-DOPDA  PICTURE  99.                      CI0091
            11            LX03-CPMTF  PICTURE  99.                      CI0091
            11            LX03-CIRMO  PICTURE  X(12).                   CI0091
            11            LX03-CPALL  PICTURE  X(1).                    CI0091
            11            LX03-CCOLM  PICTURE  9(2).                    CI0091
            11            LX03-CBLTP  PICTURE  X(1).                    CI0091
            11            LX03-CASUB  PICTURE  9(2).                    CI0091
            11            LX03-CBLFM  PICTURE  9(2).                    CI0091
            11            LX03-IBILS  PICTURE  X.                       CI0091
            11            LX03-IPAOS  PICTURE  X.                       CI0091
            11            LX03-CBLSQ  PICTURE  X(4).                    CI0091
            11            LX03-DLBPD  PICTURE  9(8).                    CI0091
            11            LX03-DNBPD  PICTURE  9(8).                    CI0091
            11            LX03-DODBD  PICTURE  9(8).                    CI0091
            11            LX03-CPSRE  PICTURE  99.                      CI0091
            11            LX03-ISPHN  PICTURE  X.                       CI0091
            11            LX03-TCARR  PICTURE  X(6).                    CI0091
            11            LX03-CBKPT  PICTURE  9(2).                    CI0091
            11            LX03-IECNT  PICTURE  X.                       CI0091
            11            LX03-ICONV  PICTURE  X(1).                    CI0091
            11            LX03-FILLER PICTURE  X(4).                    CI0091
            10            LX03-CY04                                     CI0091
                          REDEFINES            LX03-CY99.               CI0091
            11            LX03-CCARD  PICTURE  X(02).                   CI0091
            11            LX03-MCSIG4 PICTURE  X(20).                   CI0091
            11            LX03-IREMT  PICTURE  X(01).                   CI0091
            11            LX03-ISBILA PICTURE  X.                       CI0091
            11            LX03-DLBPDA PICTURE  9(8).                    CI0091
            11            LX03-DNBPDA.                                  CI0091
            12            LX03-DNCYM  PICTURE  9(6).                    CI0091
            12            LX03-CEDTD  PICTURE  9(2).                    CI0091
            11            LX03-AREMT  PICTURE  S9(7)V99                 CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            LX03-DREMT  PICTURE  9(8).                    CI0091
            11            LX03-ADBRQ  PICTURE  S9(11)V99                CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            LX03-CLUPD1 PICTURE  S9(3)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            LX03-DLAUP3 PICTURE  9(8).                    CI0091
            11            LX03-CWRC2  PICTURE  99.                      CI0091
            11            LX03-CHCR2  PICTURE  99.                      CI0091
            11            LX03-GEOPD9 PICTURE  X(8).                    CI0091
            11            LX03-GEAUN1 PICTURE  9(5).                    CI0091
            11            LX03-DPCHD1 PICTURE  9(8).                    CI0091
       01                 LX06.                                         CI0091
            10            LX06-CX06K.                                   CI0091
            11            LX06-C299.                                    CI0091
            12            LX06-CTID.                                    CI0091
            13            LX06-CTIDA  PICTURE  9(3).                    CI0091
            13            LX06-CTIDN.                                   CI0091
            14            LX06-CTIDNP PICTURE  X(13).                   CI0091
            14            LX06-CTIDND PICTURE  9(11).                   CI0091
            10            LX06-NPECK  PICTURE  9(02).                   CI0091
            10            LX06-FILLER PICTURE  X.                       CI0091
      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0091
          05              DE00-SUITE.                                   CI0091
            15       FILLER         PICTURE  X(00653).                  CI0091
       01                 DE10  REDEFINES      DE00.                    CI0091
            10            DE10-DU11.                                    CI0091
            11            DE10-XFONC  PICTURE  X(4).                    CI0091
            11            DE10-MPSBN  PICTURE  X(8).                    CI0091
            11            DE10-XDBDNM PICTURE  X(08).                   CI0091
            11            DE10-XSEGNM PICTURE  X(08).                   CI0091
            11            DE10-XRC    PICTURE  X(02).                   CI0091
            11            DE10-MSEG   PICTURE  X(08).                   CI0091
            11            DE10-XCOKEY PICTURE  X(70).                   CI0091
            11            DE10-CUIBR  PICTURE  X(01).                   CI0091
            11            DE10-CUIBA  PICTURE  X(01).                   CI0091
            11            DE10-IPBIK  PICTURE  X(1).                    CI0091
            10            DE10-DU03.                                    CI0091
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            DE10-CMSSF  PICTURE  XX.                      CI0091
            11            DE10-DU09.                                    CI0091
            12            DE10-CMESA  PICTURE  S9(9)                    CI0091
                          BINARY.                                       CI0091
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0091
                          BINARY.                                       CI0091
            12            DE10-CMESB  PICTURE  S9(9)                    CI0091
                          BINARY.                                       CI0091
            12            DE10-CMSST  PICTURE  S9(9)                    CI0091
                          BINARY.                                       CI0091
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0091
                          BINARY.                                       CI0091
            12            DE10-QELLAA PICTURE  S9(9)                    CI0091
                          BINARY.                                       CI0091
            12            DE10-TMESS4 PICTURE  X(512).                  CI0091
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0091
          05              MS00-SUITE.                                   CI0091
            15       FILLER         PICTURE  X(00542).                  CI0091
       01                 MS03  REDEFINES      MS00.                    CI0091
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            10            MS03-CMSSF  PICTURE  XX.                      CI0091
            10            MS03-DU09.                                    CI0091
            11            MS03-CMESA  PICTURE  S9(9)                    CI0091
                          BINARY.                                       CI0091
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0091
                          BINARY.                                       CI0091
            11            MS03-CMESB  PICTURE  S9(9)                    CI0091
                          BINARY.                                       CI0091
            11            MS03-CMSST  PICTURE  S9(9)                    CI0091
                          BINARY.                                       CI0091
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0091
                          BINARY.                                       CI0091
            11            MS03-QELLAA PICTURE  S9(9)                    CI0091
                          BINARY.                                       CI0091
            11            MS03-TMESS4 PICTURE  X(512).                  CI0091
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0091
            10            MX11-QMSGS  PICTURE  9(03).                   CI0091
            10            MX11-PJ09                                     CI0091
                          OCCURS       025     TIMES.                   CI0091
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0091
                          COMPUTATIONAL-3.                              CI0091
            11            MX11-CMESB  PICTURE  S9(9)                    CI0091
                          BINARY.                                       CI0091
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PJ08
                                LX01
                                LX03
                                LX06
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0091
      *               *                                   *             CI0091
      *               *INITIALISATIONS                    *             CI0091
      *               *                                   *             CI0091
      *               *************************************.            CI0091
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
      *N02CA.    NOTE *---> Initialize output             *.
       F02CA.                                                           lv10
           MOVE        'N' TO PJ08-ICX01
           PJ08-ICX03
           PJ08-ICX06
           INITIALIZE  LX01 CX01
           LX03 CX03
           LX06 CX06.
       F02CA-FN. EXIT.
      *N02XA.    NOTE *---> Set PCB's to pointers...      *.
       F02XA.                                                           lv10
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR DATP                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-DATP-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0091
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0091
      *               *                                   *             CI0091
      *               *FIN DE TRAITEMENT                  *             CI0091
      *               *                                   *             CI0091
      *               *************************************.            CI0091
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0091
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N40AD.    NOTE *CALL ACF EXIT MODULE               *.            ADU031
       F40AD.                                                           lv10
           EXEC CICS   LINK PROGRAM (ACF-PROG)                          ADU031
                       COMMAREA (ACF-USER-AREA)                         ADU031
                       LENGTH (ACF-AREA-LEN)                 END-EXEC.  ADU031
       F40AD-FN. EXIT.
      *N40BA.    NOTE *CALL CI0020 - CAMS ACCTG DATE      *.            AM0020
       F40BA.                                                           lv10
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
      *N40CA.    NOTE *---> Ooops... no acctg date...     *.
       F40CA.    IF    MS03-NMESS2 NOT = ZEROES                         lv15
                 NEXT SENTENCE ELSE GO TO     F40CA-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        MS03-NMESS2 TO MS03-NMESS2                       ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40CA-FN. EXIT.
       F40BA-FN. EXIT.
      *N50AA.    NOTE *---> Validate Action code          *.
       F50AA.    IF    PJ08-CACTM NOT = 'M'                             lv10
                 AND   PJ08-CACTM NOT = 'A'
                 NEXT SENTENCE ELSE GO TO     F50AA-FN.
      *     Only allow Add or Modify
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012774 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50AA-FN. EXIT.
      *N50BA.    NOTE *---> Check if add mode that all    *.
       F50BA.    IF    (PJ08-CTID = SPACES                              lv10
                 OR    PJ08-CLID = SPACES)
                 AND   PJ08-CACTM = 'A'
                 NEXT SENTENCE ELSE GO TO     F50BA-FN.
      *     mandatory keys are present
      *     to prevent update with
      *     zero values....
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012309 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50BA-FN. EXIT.
      *N55AA.    NOTE *---> Modify Processing             *.
       F55AA.    IF    PJ08-CACTM = 'M'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F55AA-FN.
      *N55BA.    NOTE *---> Locate the root CX01 segmnt   *.
       F55BA.                                                           lv15
      *     Get Hold Unique
           MOVE        PJ08-CLID TO S-CXU01-CLID
           PERFORM     F94G1 THRU F94G1-FN.
      *N55BC.    NOTE *---> Error on return               *.
       F55BC.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F55BC-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012006 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F55BC-900. GO TO F55BE-FN.
       F55BC-FN. EXIT.
      *N55BE.    NOTE *---> Otherwise update the stamp    *.
       F55BE.                                                           lv20
           ADD         +1 TO CX01-NSEQ4B
           SIZE ERROR
           MOVE        +1 TO CX01-NSEQ4B.
      *---> Obtain System Date...                                       DOT
           PERFORM     F92DA THRU F92DA-FN
      *
      *---> Replace the record...
           PERFORM     F94R1 THRU F94R1-FN
      *
      *---> Update output linkage
           MOVE        CX01 TO LX01
           MOVE        'Y' TO PJ08-ICX01.
       F55BE-FN. EXIT.
       F55BA-FN. EXIT.
      *N55CA.    NOTE *---> Updates to the CX03 too...    *.
       F55CA.    IF    PJ08-CPROT = '02'                                lv15
                 AND   IK = '0'
                 NEXT SENTENCE ELSE GO TO     F55CA-FN.
      *     Get Hold Next
           MOVE        PJ08-CARTY TO S-CXU03-CARTY
           MOVE        PJ08-NARRS TO S-CXU03-NARRS
           PERFORM     F94N1 THRU F94N1-FN.
      *N55CC.    NOTE *---> Something went wrong...       *.
       F55CC.    IF    IK NOT = '0'                                     lv20
                 NEXT SENTENCE ELSE GO TO     F55CC-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012007 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F55CC-900. GO TO F55CF-FN.
       F55CC-FN. EXIT.
      *N55CF.    NOTE *---> Reset the CX03 Event detail   *.
       F55CF.                                                           lv20
      *
           MOVE        CX03 TO WX03
           MOVE        ZEROES TO CX03-DEXDT
           MOVE        99 TO CX03-CPMTG
      *
      *---> Replace the record...
           PERFORM     F94R2 THRU F94R2-FN
      *
      *---> Update Output Linkage
           MOVE        CX03 TO LX03
           MOVE        'Y' TO PJ08-ICX03
      *
      *---> Write Audit records
           PERFORM     F92AM THRU F92AM-FN.
       F55CF-FN. EXIT.
       F55CA-FN. EXIT.
       F55AA-FN. EXIT.
      *N56AA.    NOTE *---> Add Processing                *.
       F56AA.    IF    PJ08-CACTM = 'A'                                 lv10
                 AND   (PJ08-MAPPN = 'SD'
                 OR    PJ08-MAPPN = 'FDC')
                 NEXT SENTENCE ELSE GO TO     F56AA-FN.
      *NOTE: THIS MODULE WILL UPDATE
      *the arrangement database
      *regardless of the data sent.
      *
      *Data sent to this module should
      *be validated using CI0082 to
      *ensure that it is correct.
      *
      *The check on Application Name
      *will prohibit erroneous
      *use of this routine by any
      *other application than SD
      *and FDC
      *N56BA.    NOTE *---> Locate the root CX01 segmnt   *.
       F56BA.                                                           lv15
      *     Get Hold Unique
           MOVE        PJ08-CLID TO S-CXU01-CLID
           PERFORM     F94G1 THRU F94G1-FN.
      *N56BE.    NOTE *---> Record exists...              *.
       F56BE.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F56BE-FN.
      *     Increment stamp...
           ADD         +1 TO CX01-NSEQ4B
           SIZE ERROR
           MOVE        +1 TO CX01-NSEQ4B.
      *---> Obtain System Date...                                       DOT
           PERFORM     F92DA THRU F92DA-FN
      *
      *---> Replace the record...
           PERFORM     F94R1 THRU F94R1-FN.
       F56BE-900. GO TO F56BH-FN.
       F56BE-FN. EXIT.
      *N56BH.    NOTE *---> New CX01 record...            *.
       F56BH.                                                           lv20
           MOVE        +1 TO CX01-NSEQ4B
           MOVE        PJ08-CLID TO CX01-CLID
      *---> Obtain System Date...
           PERFORM     F92DA THRU F92DA-FN
      *---> Insert the CX01
           PERFORM     F94I1 THRU F94I1-FN.
      *N56BJ.    NOTE *---> Attempt to insert again       *.
       F56BJ.    IF    IK = '1'                                         lv25
                 AND   DE10-NMESS2 = ZEROES
                 NEXT SENTENCE ELSE GO TO     F56BJ-FN.
      *     Increment stamp...
           ADD         +1 TO CX01-NSEQ4B
           SIZE ERROR
           MOVE        +1 TO CX01-NSEQ4B.
      *Perform the insert                                               DOT
           PERFORM     F94I1 THRU F94I1-FN.
       F56BJ-900. GO TO F56BJ.
       F56BJ-FN. EXIT.
       F56BH-FN. EXIT.
      *N56BX.    NOTE *---> Update the output linkage     *.
       F56BX.                                                           lv20
           MOVE        CX01 TO LX01
           MOVE        'Y' TO PJ08-ICX01.
       F56BX-FN. EXIT.
       F56BA-FN. EXIT.
      *N56CA.    NOTE *---> Updates to the CX03           *.
       F56CA.    IF    PJ08-NARRS NOT = ZERO                            lv15
                 NEXT SENTENCE ELSE GO TO     F56CA-FN.
      *     Locate a unique CX03
           MOVE        PJ08-CARTY TO S-CXU03-CARTY
           MOVE        PJ08-NARRS TO S-CXU03-NARRS
           PERFORM     F94N1 THRU F94N1-FN.
       F56CA-900. GO TO F56CC-FN.
       F56CA-FN. EXIT.
      *N56CC.    NOTE *---> Otherwise find first for      *.
       F56CC.                                                           lv15
      *     Arrangement type...
           MOVE        PJ08-CARTY TO S-CXA03-CARTY
           PERFORM     F94N2 THRU F94N2-FN.
       F56CC-FN. EXIT.
      *N56CF.    NOTE *---> Reset the CX03 Event detail   *.
       F56CF.    IF    IK = '0'                                         lv15
                 AND   DE10-NMESS2 = ZEROES
                 NEXT SENTENCE ELSE GO TO     F56CF-FN.
           MOVE        CX03 TO WX03
           MOVE        ZEROES TO CX03-DEXDT
           MOVE        99 TO CX03-CPMTG
      *
      *---> Replace the record...
           PERFORM     F94R2 THRU F94R2-FN
      *---> Write Audit Logs
           PERFORM     F92AM THRU F92AM-FN.
       F56CF-900. GO TO F56CK-FN.
       F56CF-FN. EXIT.
      *N56CK.    NOTE *---> Otherwise insert...           *.
       F56CK.                                                           lv15
           MOVE        +81 TO CX03-GELL
           MOVE        NS20-DCACG TO CX03-DASUP
           MOVE        ZERO TO CX03-GRCRNG
           MOVE        PJ08-GECSQ TO CX03-GECSQ
           MOVE        PJ08-CARTY TO CX03-CARTY
           MOVE        +1 TO CX03-NARRS
           MOVE        +1 TO CX03-CARST
           MOVE        ZEROES TO CX03-DEXDT
           MOVE        99 TO CX03-CPMTG
           PERFORM     F94I2 THRU F94I2-FN.
      *N56CM.    NOTE *---> REPEAT UNTIL OK               *.
       F56CM.    IF    IK = '1'                                         lv20
                 AND   DE10-NMESS2 = ZEROES
                 NEXT SENTENCE ELSE GO TO     F56CM-FN.
           ADD         +1 TO CX03-NARRS
           SIZE ERROR
           MOVE        +1 TO CX03-NARRS.
      *Perform the insert                                               DOT
           PERFORM     F94I2 THRU F94I2-FN.
       F56CM-900. GO TO F56CM.
       F56CM-FN. EXIT.
      *N56CP.    NOTE *---> Write Audit Logs              *.
       F56CP.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F56CP-FN.
           PERFORM     F92AN THRU F92AN-FN.
       F56CP-FN. EXIT.
       F56CK-FN. EXIT.
      *N56CX.    NOTE *---> Update output linkage         *.
       F56CX.                                                           lv15
           MOVE        CX03 TO LX03
           MOVE        'Y' TO PJ08-ICX03.
       F56CX-FN. EXIT.
      *N56DB.    NOTE *---> Attempt to locate a CX06      *.
       F56DB.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F56DB-FN.
      *
           MOVE        PJ08-CTID TO S-CXU06-CTID
           MOVE        CX03-CARTY TO S-CXU03-CARTY
           MOVE        CX03-NARRS TO S-CXU03-NARRS
           PERFORM     F94N3 THRU F94N3-FN.
       F56DB-FN. EXIT.
      *N56DK.    NOTE *---> Acces fails - create CX06     *.
       F56DK.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F56DK-FN.
      *
           INITIALIZE  CX06
           MOVE        PJ08-CTID TO CX06-CTID
           MOVE        ZERO TO CX06-NPECK
           PERFORM     F94I3 THRU F94I3-FN.
      *N56DN.    NOTE *---> Insert failed                 *.
       F56DN.    IF    (IK = '1')                                       lv20
                 OR    (DE10-NMESS2 NOT = ZEROES)
                 NEXT SENTENCE ELSE GO TO     F56DN-FN.
       F56DN-900. GO TO F56DQ-FN.
       F56DN-FN. EXIT.
      *N56DQ.    NOTE *---> Write Audit Log record        *.
       F56DQ.                                                           lv20
           PERFORM     F92AO THRU F92AO-FN.
      *N56DS.    NOTE *---> Format log record             *.
       F56DS.         EXIT.                                             lv25
       F56DS-FN. EXIT.
       F56DQ-FN. EXIT.
       F56DK-FN. EXIT.
      *N56DX.    NOTE *---> Update output linkage         *.
       F56DX.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F56DX-FN.
           MOVE        CX06 TO LX06
           MOVE        'Y' TO PJ08-ICX06.
       F56DX-FN. EXIT.
       F56AA-FN. EXIT.
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
      *N92AL.    NOTE *---> Audit Logs                    *.
       F92AL.                                                           lv10
      *     These may be written for
      *     (1) A modfied CX03
      *     (2) A new CX03
      *     (3) A new CX06
      *N92AM.    NOTE *---> (1) A modfied CX03            *.
       F92AM.                                                           lv15
           MOVE        SPACES TO VA05
           VB05
           MOVE        'CLIENT' TO DH10-MAUSB
           MOVE        CX01-CX01K TO DH10-NAUSK
           MOVE        CX03-CX03K TO VA05-CX03K
           VB05-CX03K
           MOVE        ZEROES TO VA05-CX06K
           VB05-CX06K
           MOVE        CX03-CARST TO VA05-CARST
           MOVE        CX03-GECSQ TO VA05-GECSQ
           MOVE        CX03-CPMTG TO VA05-CPMTG
           MOVE        CX03-GRCRNG TO VA05-GRCRNG
           MOVE        CX03-DEXDT TO VA05-DEXDT
           MOVE        CX03-DASUP TO VA05-DASUP
           MOVE        CX03-CY50 TO VA05-CY50
      *
           MOVE        WX03-CARST TO VB05-CARST
           MOVE        WX03-GECSQ TO VB05-GECSQ
           MOVE        WX03-CPMTG TO VB05-CPMTG
           MOVE        WX03-GRCRNG TO VB05-GRCRNG
           MOVE        WX03-DEXDT TO VB05-DEXDT
           MOVE        WX03-DASUP TO VB05-DASUP
           MOVE        WX03-CY50 TO VB05-CY50.
                 IF    VA05 NOT = VB05                                  DOT
      *Write Before and After Images
      *when a change has occured...
           MOVE        VB05 TO DH10-GAUVR
           MOVE        +70020 TO DH10-CAUFR
           MOVE        +00002 TO DH10-CAUAC
           PERFORM     F96AL THRU F96AL-FN
           MOVE        VA05 TO DH10-GAUVR
           MOVE        +70020 TO DH10-CAUFR
           MOVE        +00003 TO DH10-CAUAC
           PERFORM     F96AL THRU F96AL-FN.
       F92AM-FN. EXIT.
      *N92AN.    NOTE *---> (2) A new CX03                *.
       F92AN.                                                           lv15
           MOVE        CX01-CX01K TO DH10-NAUSK
           MOVE        'CLIENT' TO DH10-MAUSB
           MOVE        +70020 TO DH10-CAUFR
           MOVE        +00001 TO DH10-CAUAC
           MOVE        CX03-CARST TO VA05-CARST
           MOVE        CX03-GECSQ TO VA05-GECSQ
           MOVE        CX03-CPMTG TO VA05-CPMTG
           MOVE        CX03-GRCRNG TO VA05-GRCRNG
           MOVE        CX03-DEXDT TO VA05-DEXDT
           MOVE        CX03-DASUP TO VA05-DASUP
           MOVE        CX03-CY50 TO VA05-CY50
      *Write After Image
      *
           MOVE        VA05 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F92AN-FN. EXIT.
      *N92AO.    NOTE *---> (3) A new CX06                *.
       F92AO.                                                           lv15
           MOVE        CX01-CX01K TO DH10-NAUSK
           MOVE        CX03-CX03K TO VA06-CX03K
           MOVE        CX06-CX06K TO VA06-CX06K
           MOVE        VA06 TO DH10-GAUVR
           MOVE        +70021 TO DH10-CAUFR
           MOVE        +00001 TO DH10-CAUAC
           MOVE        'CLIENT' TO DH10-MAUSB
           PERFORM     F96AL THRU F96AL-FN.
       F92AO-FN. EXIT.
       F92AL-FN. EXIT.
      *N92DA.    NOTE *---> Obtain the system date for    *.
       F92DA.                                                           lv10
      *     CX01 Updates...
           EXEC CICS   ASKTIME ABSTIME (DD01-XMSTS)          END-EXEC.  ADU155
           EXEC CICS   FORMATTIME ABSTIME (DD01-XMSTS)                  ADU155
                       YYMMDD (DD01-XDAT69)                             ADU155
                       YEAR (DD01-F2CCYY)                    END-EXEC.  ADU155
           COMPUTE     DD01-YEAR = DD01-F2CCYY                          ADU155
      ** MOVE DD01-UDATE TO YOUR FIELD                                  ADU155
           MOVE        DD01-XDAT69 (3:4) TO DD01-MMDD                   ADU155
      *
           MOVE        DD01-UDATE TO CX01-GEMDA.
       F92DA-FN. EXIT.
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
      *N94G1.    NOTE *CALL GHU ON CX01                   *.            ADU026
       F94G1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PA06 CX01                                                    ADU026
           S-CXU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94G1-FN. EXIT.
      *N94I1.    NOTE *CALL ISRT ON CX01                  *.            ADU026
       F94I1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 CX01                                                    ADU026
           S-CX01-SSA                                                   ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94I1-FN. EXIT.
      *N94I2.    NOTE *CALL ISRT ON CX03                  *.            ADU026
       F94I2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 CX03                                                    ADU026
           S-CXU01-SSA S-CX03-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94I2-FN. EXIT.
      *N94I3.    NOTE *CALL ISRT ON CX06                  *.            ADU026
       F94I3.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 CX06                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CX06-SSA                           ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94I3-FN. EXIT.
      *N94N1.    NOTE *CALL GHN ON CX03                   *.            ADU026
       F94N1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHN                        ADU026
           PA06 CX03                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHN TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94N1-FN. EXIT.
      *N94N2.    NOTE *CALL GHN ON CX03                   *.            ADU026
       F94N2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHN                        ADU026
           PA06 CX03                                                    ADU026
           S-CXU01-SSA S-CXA03-SSA                                      ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHN TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94N2-FN. EXIT.
      *N94N3.    NOTE *CALL GN ON CX06                    *.            ADU026
       F94N3.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CX06                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CXU06-SSA                          ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94N3-FN. EXIT.
      *N94R1.    NOTE *CALL REPL ON CX01                  *.            ADU026
       F94R1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PA06 CX01                                                    ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94R1-FN. EXIT.
      *N94R2.    NOTE *CALL REPL ON CX03                  *.            ADU026
       F94R2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PA06 CX03                                                    ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94R2-FN. EXIT.
      *N96AL.    NOTE *---> Audit Log Process             *.            ADU165
       F96AL.         EXIT.                                             lv10
      *N96AN.    NOTE *---> Format Audit Log Data         *.            ADU165
       F96AN.                                                           lv15
           SET AL00-NPNTR                                               ADU165
           TO ADDRESS OF DLIUIBII                                       ADU165
           MOVE        AL00-ADDR TO DH10-XUIBP                          ADU165
           MOVE        AL00-NSEQ2P TO DH10-NSEQ2P                       ADU165
           MOVE        'E' TO DH10-CAUL                                 ADU165
           MOVE        'CLIENT' TO DH10-MAUSB                           ADU165
           MOVE        PJ08-CLID TO DH10-NAUSK                          ADU165
           MOVE        'CATS' TO DH10-CSYS                              ADU165
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
