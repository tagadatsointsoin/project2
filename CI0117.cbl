       IDENTIFICATION DIVISION.                                         CI0117
       PROGRAM-ID.  CI0117.                                             CI0117
      *AUTHOR.         CATS AAPS SERVICE MODULE.                        CI0117
      *DATE-COMPILED.   09/08/14.                                       CI0117
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2007                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE CATS  SYSTEM AND ALL INFORMATION RELATING THERETO,    * ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE CATS  SYSTEM AND ALL            * ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE CATS        * ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 2007                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0117
       CONFIGURATION SECTION.                                           CI0117
       SOURCE-COMPUTER. IBM-370.                                        CI0117
       OBJECT-COMPUTER. IBM-370.                                        CI0117
       DATA DIVISION.                                                   CI0117
       WORKING-STORAGE SECTION.                                         CI0117
      ******BOOLEAN SSA FOR CX2Y SEGMENT ACCESS.
       01  S-CYN2Y-SSA.
           10  FILLER            PICTURE X(8)  VALUE 'CX2Y    '.
           10  FILLER            PICTURE X     VALUE '*'.
           10  S-CYN2Y-CCOD      PICTURE X(5)  VALUE '-----'.
           10  FILLER            PICTURE X     VALUE '('.
           10  FILLER            PICTURE X(8)  VALUE 'CX2YK   '.
           10  S-CYN2Y-OPER1     PICTURE X(2)  VALUE '> '.
           10  S-CYN2Y-CX2YK1.
           11  S-CYN2Y-2991.
           12  S-CYN2Y-CTID1.
           13  S-CYN2Y-CTIDA1    PICTURE 9(3).
           13  S-CYN2Y-CTIDN1.
           14  S-CYN2Y-CTIDNP1   PICTURE X(13).
           14  S-CYN2Y-CTIDND1   PICTURE 9(11).
           11  S-CYN2Y-1991.
           12  S-CYN2Y-CLID1.
           13  S-CYN2Y-CLIDO1    PICTURE 9(3).
           13  S-CYN2Y-CLIDN1.
           14  S-CYN2Y-CLIDNP1   PICTURE X(12).
           14  S-CYN2Y-CLIDND1   PICTURE 9(8).
           11  S-CYN2Y-CARTY1    PICTURE 9(2).
           11  S-CYN2Y-NARRS1    PICTURE S9(3) COMP-3.
           10  FILLER            PICTURE X     VALUE ')'.
      ******************************************************************
      * WORKING STORAGE AREA TO DECLARE FLAGS INDICATING SUCCESSFUL/   *
      * UNSUCCESSFUL READS FROM DATABASE SEGMENTS / PACTABLES          *
      ******************************************************************
       01                 CF00.
      *FLAG FOR READ OF CX2Y SEGMENT
         05               CY2Y-CF    PIC X
                             VALUE SPACES.
      *FLAG FOR READ OF CX13 SEGMENT
         05               CX13-CF    PIC X
                             VALUE SPACES.
      *FLAG USED TO INDICATE THE PRESENCE OF CY2Y
         05               CY2Y-FOUND PIC X
                             VALUE SPACES.
      *FLAG FOR READ OF PACTABLE TA71
         05               TA71-CF    PIC X
                             VALUE SPACES.
      *FLAG FOR READ OF CX14 SEGMENT
         05               CX14-CF    PIC X
                             VALUE SPACES.
      *FLAG FOR READ OF CX06 SEGMENT
         05               CX06-CF    PIC X
                             VALUE SPACES.
       01                 CX00.                                         CI0117
            02            CX01.                                         CI0117
            10            CX01-CX01K.                                   CI0117
            11            CX01-C199.                                    CI0117
            12            CX01-CLID.                                    CI0117
            13            CX01-CLIDO  PICTURE  9(3).                    CI0117
            13            CX01-CLIDN.                                   CI0117
            14            CX01-CLIDNP PICTURE  X(12).                   CI0117
            14            CX01-CLIDND PICTURE  9(8).                    CI0117
            10            CX01-GEMDA  PICTURE  9(8).                    CI0117
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0117
                          BINARY.                                       CI0117
            10            CX01-FILLER PICTURE  X(5).                    CI0117
            02            CX03.                                         CI0117
            10            CX03-GELL   PICTURE  9(4)                     CI0117
                          BINARY.                                       CI0117
            10            CX03-CY00.                                    CI0117
            11            CX03-CX03K.                                   CI0117
            12            CX03-CARTY  PICTURE  99.                      CI0117
            12            CX03-NARRS  PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX03-CARST  PICTURE  99.                      CI0117
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX03-CPMTG  PICTURE  99.                      CI0117
            11            CX03-GRCRNG PICTURE  9(3).                    CI0117
            11            CX03-DEXDT  PICTURE  9(8).                    CI0117
            11            CX03-DASUP  PICTURE  9(8).                    CI0117
            11            CX03-CSTEC  PICTURE  X(3).                    CI0117
            11            CX03-FILLER PICTURE  X(17).                   CI0117
            11            CX03-CY50.                                    CI0117
            12            CX03-NARID  PICTURE  X(30).                   CI0117
            11            CX03-CY51                                     CI0117
                          REDEFINES            CX03-CY50.               CI0117
            12            CX03-NDIDN  PICTURE  9(12).                   CI0117
            12            CX03-FILLER PICTURE  X(18).                   CI0117
            11            CX03-CY52                                     CI0117
                          REDEFINES            CX03-CY50.               CI0117
            12            CX03-NAIDC  PICTURE  9(12).                   CI0117
            12            CX03-FILLER PICTURE  X(18).                   CI0117
            11            CX03-CY53                                     CI0117
                          REDEFINES            CX03-CY50.               CI0117
            12            CX03-NAMEXB PICTURE  9(15).                   CI0117
            12            CX03-FILLER PICTURE  X(15).                   CI0117
            10            CX03-CY99.                                    CI0117
            11            CX03-FILLER PICTURE  X(109).                  CI0117
            10            CX03-CY01                                     CI0117
                          REDEFINES            CX03-CY99.               CI0117
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX03-ICPCI  PICTURE  X.                       CI0117
            11            CX03-CLUPD  PICTURE  9(3).                    CI0117
            11            CX03-DLAUP  PICTURE  9(8).                    CI0117
            11            CX03-CWRC   PICTURE  99.                      CI0117
            11            CX03-CHCR   PICTURE  99.                      CI0117
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0117
            11            CX03-GEAUN  PICTURE  9(5).                    CI0117
            11            CX03-DPCHD  PICTURE  9(8).                    CI0117
            11            CX03-DLRCHK PICTURE  9(8).                    CI0117
            11            CX03-QTRCHK PICTURE  9(2).                    CI0117
            11            CX03-DNPMT  PICTURE  9(8).                    CI0117
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0117
                          COMPUTATIONAL-3.                              CI0117
            10            CX03-CY02                                     CI0117
                          REDEFINES            CX03-CY99.               CI0117
            11            CX03-QSIRQ  PICTURE  99.                      CI0117
            11            CX03-QDRMN  PICTURE  9(2)                     CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX03-DDPRE  PICTURE  9(8).                    CI0117
            11            CX03-DDSHP  PICTURE  9(8).                    CI0117
            11            CX03-NDRFTB PICTURE  9(5).                    CI0117
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0117
            11            CX03-DDSHPA PICTURE  9(8).                    CI0117
            11            CX03-NDRFTF PICTURE  9(5).                    CI0117
            11            CX03-QDIPBK PICTURE  9(3).                    CI0117
            11            CX03-CREOR  PICTURE  X(1).                    CI0117
            11            CX03-CREOR1 PICTURE  X(1).                    CI0117
            11            CX03-DDASC  PICTURE  9(8).                    CI0117
            11            CX03-FILLER PICTURE  X(7).                    CI0117
            10            CX03-CY03                                     CI0117
                          REDEFINES            CX03-CY99.               CI0117
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0117
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0117
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0117
            11            CX03-DOPDA  PICTURE  99.                      CI0117
            11            CX03-CPMTF  PICTURE  99.                      CI0117
            11            CX03-CIRMO  PICTURE  X(12).                   CI0117
            11            CX03-CPALL  PICTURE  X(1).                    CI0117
            11            CX03-CCOLM  PICTURE  9(2).                    CI0117
            11            CX03-CBLTP  PICTURE  X(1).                    CI0117
            11            CX03-CASUB  PICTURE  9(2).                    CI0117
            11            CX03-CBLFM  PICTURE  9(2).                    CI0117
            11            CX03-IBILS  PICTURE  X.                       CI0117
            11            CX03-IPAOS  PICTURE  X.                       CI0117
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0117
            11            CX03-DLBPD  PICTURE  9(8).                    CI0117
            11            CX03-DNBPD  PICTURE  9(8).                    CI0117
            11            CX03-DODBD  PICTURE  9(8).                    CI0117
            11            CX03-CPSRE  PICTURE  99.                      CI0117
            11            CX03-ISPHN  PICTURE  X.                       CI0117
            11            CX03-TCARR  PICTURE  X(6).                    CI0117
            11            CX03-CBKPT  PICTURE  9(2).                    CI0117
            11            CX03-IECNT  PICTURE  X.                       CI0117
            11            CX03-ICONV  PICTURE  X(1).                    CI0117
            11            CX03-FILLER PICTURE  X(4).                    CI0117
            10            CX03-CY04                                     CI0117
                          REDEFINES            CX03-CY99.               CI0117
            11            CX03-CCARD  PICTURE  X(02).                   CI0117
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0117
            11            CX03-IREMT  PICTURE  X(01).                   CI0117
            11            CX03-ISBILA PICTURE  X.                       CI0117
            11            CX03-DLBPDA PICTURE  9(8).                    CI0117
            11            CX03-DNBPDA.                                  CI0117
            12            CX03-DNCYM  PICTURE  9(6).                    CI0117
            12            CX03-CEDTD  PICTURE  9(2).                    CI0117
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX03-DREMT  PICTURE  9(8).                    CI0117
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0117
            11            CX03-CWRC2  PICTURE  99.                      CI0117
            11            CX03-CHCR2  PICTURE  99.                      CI0117
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0117
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0117
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0117
            02            CX06.                                         CI0117
            10            CX06-CX06K.                                   CI0117
            11            CX06-C299.                                    CI0117
            12            CX06-CTID.                                    CI0117
            13            CX06-CTIDA  PICTURE  9(3).                    CI0117
            13            CX06-CTIDN.                                   CI0117
            14            CX06-CTIDNP PICTURE  X(13).                   CI0117
            14            CX06-CTIDND PICTURE  9(11).                   CI0117
            10            CX06-NPECK  PICTURE  9(02).                   CI0117
            10            CX06-FILLER PICTURE  X.                       CI0117
            02            CX13.                                         CI0117
            10            CX13-GELL   PICTURE  9(4)                     CI0117
                          BINARY.                                       CI0117
            10            CX13-CY20.                                    CI0117
            11            CX13-CX13K.                                   CI0117
            12            CX13-CARTZ  PICTURE  99.                      CI0117
            12            CX13-NAPDS  PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX13-GESTD  PICTURE  9(8).                    CI0117
            11            CX13-GEEND  PICTURE  9(8).                    CI0117
            11            CX13-DASUQ  PICTURE  9(8).                    CI0117
            11            CX13-CDEST  PICTURE  99.                      CI0117
            11            CX13-IIARR  PICTURE  X.                       CI0117
            11            CX13-DLAUP  PICTURE  9(8).                    CI0117
            11            CX13-GEOPD2 PICTURE  X(8).                    CI0117
            11            CX13-GEAUN  PICTURE  9(5).                    CI0117
            11            CX13-DPCHD  PICTURE  9(8).                    CI0117
            11            CX13-PPOT1  PICTURE  S9(3)V99                 CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX13-ACOT1  PICTURE  S9(9)V99                 CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX13-QPST1  PICTURE  S9(7)V999                CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX13-FILLER PICTURE  X(03).                   CI0117
            10            CX13-CY96.                                    CI0117
            11            CX13-FILLER PICTURE  X(50).                   CI0117
            10            CX13-CY21                                     CI0117
                          REDEFINES            CX13-CY96.               CI0117
            11            CX13-DNPMT  PICTURE  9(8).                    CI0117
            11            CX13-CPMTF  PICTURE  99.                      CI0117
            11            CX13-ADBRQ  PICTURE  S9(11)V99                CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX13-QSHOWQ PICTURE  S9(9)V999                CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX13-PACT1  PICTURE  S999V999                 CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX13-DOPDA  PICTURE  99.                      CI0117
            11            CX13-DNEXE  PICTURE  9(8).                    CI0117
            11            CX13-CIRMO  PICTURE  X(12).                   CI0117
            10            CX13-CY98.                                    CI0117
            11            CX13-FILLER PICTURE  X(120).                  CI0117
            10            CX13-CY25                                     CI0117
                          REDEFINES            CX13-CY98.               CI0117
            11            CX13-COPTC  PICTURE  9(1).                    CI0117
            11            CX13-ILPOI  PICTURE  X(1).                    CI0117
            11            CX13-CATOC  PICTURE  X(1).                    CI0117
            11            CX13-CEOIA  PICTURE  S9(7)V99                 CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX13-ACOAR  PICTURE  S9(9)V99                 CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX13-CEOTR  PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX13-DSTMO  PICTURE  99.                      CI0117
            10            CX13-CY27                                     CI0117
                          REDEFINES            CX13-CY98.               CI0117
            11            CX13-QMTH1  PICTURE  9(3).                    CI0117
            11            CX13-IDRMD  PICTURE  X.                       CI0117
            10            CX13-CY28                                     CI0117
                          REDEFINES            CX13-CY98.               CI0117
            11            CX13-AALLBL PICTURE  S9(8)V99                 CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX13-PSURR  PICTURE  S9(3)V999                CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX13-DFPMT  PICTURE  9(8).                    CI0117
            11            CX13-QMTHLA PICTURE  9(3).                    CI0117
            11            CX13-PWHLDS PICTURE  S999V9(5)                CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX13-ISWHO  PICTURE  X(1).                    CI0117
            10            CX13-CY29                                     CI0117
                          REDEFINES            CX13-CY98.               CI0117
            11            CX13-IINDI1 PICTURE  X(1).                    CI0117
            11            CX13-IINDI2 PICTURE  X(1).                    CI0117
            11            CX13-IINDI3 PICTURE  X(1).                    CI0117
            11            CX13-PWHLD5 PICTURE  S999V99                  CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX13-CCSMQ  PICTURE  X.                       CI0117
            11            CX13-CPLEC  PICTURE  XX.                      CI0117
            11            CX13-IPTRDA PICTURE  X(01).                   CI0117
            11            CX13-GCUSPY PICTURE  X(12).                   CI0117
            11            CX13-ALOIDA PICTURE  S9(11)V99                CI0117
                          COMPUTATIONAL-3.                              CI0117
            11            CX13-DELOI  PICTURE  9(8).                    CI0117
            11            CX13-CLGND  PICTURE  X.                       CI0117
            11            CX13-CORTYA PICTURE  X(3).                    CI0117
            11            CX13-CPH3U  PICTURE  X.                       CI0117
            11            CX13-CNAVR  PICTURE  X(1).                    CI0117
            11            CX13-NEXEC  PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            02            CX14.                                         CI0117
            10            CX14-GELL   PICTURE  9(4)                     CI0117
                          BINARY.                                       CI0117
            10            CX14-CX14K.                                   CI0117
            11            CX14-NPISQ  PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            10            CX14-ACOTD  PICTURE  S9(9)V99                 CI0117
                          COMPUTATIONAL-3.                              CI0117
            10            CX14-PPOTD  PICTURE  S9(3)V99                 CI0117
                          COMPUTATIONAL-3.                              CI0117
            10            CX14-QPSTD  PICTURE  S9(7)V999                CI0117
                          COMPUTATIONAL-3.                              CI0117
            10            CX14-CPITC  PICTURE  99.                      CI0117
            10            CX14-FILLER PICTURE  X(04).                   CI0117
            10            CX14-CY97.                                    CI0117
            11            CX14-FILLER PICTURE  X(32).                   CI0117
            10            CX14-CY30                                     CI0117
                          REDEFINES            CX14-CY97.               CI0117
            11            CX14-IOWNC  PICTURE  X.                       CI0117
            11            CX14-CTYPE  PICTURE  X.                       CI0117
            11            CX14-C299.                                    CI0117
            12            CX14-CTID.                                    CI0117
            13            CX14-CTIDA  PICTURE  9(3).                    CI0117
            13            CX14-CTIDN.                                   CI0117
            14            CX14-CTIDNP PICTURE  X(13).                   CI0117
            14            CX14-CTIDND PICTURE  9(11).                   CI0117
            11            CX14-CPMTC  PICTURE  99.                      CI0117
            11            CX14-IACSD  PICTURE  X.                       CI0117
            10            CX14-CY31                                     CI0117
                          REDEFINES            CX14-CY97.               CI0117
            11            CX14-FILLER PICTURE  X(2).                    CI0117
            11            CX14-IDELI  PICTURE  X.                       CI0117
            11            CX14-CDEL1  PICTURE  9(3).                    CI0117
            11            CX14-NDELS  PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            10            CX14-CY32                                     CI0117
                          REDEFINES            CX14-CY97.               CI0117
            11            CX14-GCUSPZ PICTURE  X(12).                   CI0117
       01                 CY00.                                         CI0117
            02            CY2Y.                                         CI0117
            10            CY2Y-CX2YK.                                   CI0117
            11            CY2Y-C299.                                    CI0117
            12            CY2Y-CTID.                                    CI0117
            13            CY2Y-CTIDA  PICTURE  9(3).                    CI0117
            13            CY2Y-CTIDN.                                   CI0117
            14            CY2Y-CTIDNP PICTURE  X(13).                   CI0117
            14            CY2Y-CTIDND PICTURE  9(11).                   CI0117
            11            CY2Y-C199.                                    CI0117
            12            CY2Y-CLID.                                    CI0117
            13            CY2Y-CLIDO  PICTURE  9(3).                    CI0117
            13            CY2Y-CLIDN.                                   CI0117
            14            CY2Y-CLIDNP PICTURE  X(12).                   CI0117
            14            CY2Y-CLIDND PICTURE  9(8).                    CI0117
            11            CY2Y-CARTY  PICTURE  99.                      CI0117
            11            CY2Y-NARRS  PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
      ******  DYNAMIC CALL TO PACTABLE BATCH INTERFACE  *****           AAAT80
       01  PTA900             PIC X(6)  VALUE 'PTA903'.                 AAAT80
       01        G-TA71.                                                CI0117
           04    G-TA71-PARAM.                                          CI0117
             10  G-TA71-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0117
                        VALUE      +042.                                CI0117
             10  G-TA71-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0117
                        VALUE      +001.                                CI0117
             10  G-TA71-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0117
                        VALUE      +002.                                CI0117
             10  G-TA71-NUAPP  PICTURE 99                               CI0117
                        VALUE       0.                                  CI0117
             10  G-TA71-NUTAB  PICTURE X(6)                             CI0117
                        VALUE 'CPMTF '.                                 CI0117
             10  G-TA71-TABFO  PICTURE XX                 VALUE SPACE.  CI0117
             10  G-TA71-TABCR  PICTURE XX                 VALUE SPACE.  CI0117
             10  G-TA71-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0117
             10  G-TA71-NUSSC  PICTURE X  VALUE   ' '.                  CI0117
             10  G-TA71-NUSSY  PICTURE X                  VALUE SPACE.  CI0117
             10  G-TA71-TRANID PICTURE X(4)               VALUE SPACE.  CI0117
             10  G-TA71-FILSYS.                                         CI0117
             15  G-TA71-USERC  PICTURE X(6)               VALUE SPACE.  CI0117
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0117
           04             TA71.                                         CI0117
            10            TA71-CPMTF  PICTURE  99                       CI0117
                          VALUE                ZERO.                    CI0117
            10            TA71-MPMTF  PICTURE  X(14)                    CI0117
                          VALUE                SPACE.                   CI0117
            10            TA71-CPMTFA PICTURE  X(2)                     CI0117
                          VALUE                SPACE.                   CI0117
            10            TA71-MPMTFL PICTURE  X(24)                    CI0117
                          VALUE                SPACE.                   CI0117
      *THE VARIOUS WORKING STORAGE VARIABLES.
       01    WS00.
      *!WI id=1
         05               WS00-CARTZ    VALUE ZEROES
                        PICTURE 99.                                     CI0117
      *!WI id=2
         05               WS00-NAPDS    VALUE ZEROES
                        PICTURE S9(3)                                   CI0117
                            COMPUTATIONAL-3.                            CI0117
      *!WI id=3
         05               WS00-GESTD    VALUE ZEROES
                        PICTURE 9(8).                                   CI0117
      *!WI id=4
         05               WS00-GEEND    VALUE ZEROES
                        PICTURE 9(8).                                   CI0117
      *!WI id=5
         05               WS00-CPMTF    VALUE ZEROES
                        PICTURE 99.                                     CI0117
      *!WI id=6
         05               WS00-ADBRQ    VALUE ZEROES
                        PICTURE S9(11)V99                               CI0117
                            COMPUTATIONAL-3.                            CI0117
      *FLAG USED WHEN FUTURE DATED ARRANGEMT FOUND
       01                 WS00-FUT-FND   PIC X
                                        VALUE SPACES.
      *FLAG USED WHEN ACTIVE ARRANGEMENT FOUND
       01                 WS00-ACT-FND   PIC X
                                        VALUE SPACES.
       01                 WS00-TA71-RD   PIC X
                                        VALUE SPACES.
       01                 WS00-INACT-FND PIC X
                                        VALUE SPACES.
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       AAAD30
       01                 XW05.                                         CI0117
            10            XW05-XW06.                                    CI0117
            11            XW05-XDBPCB.                                  CI0117
            12            XW05-XDBDNM PICTURE  X(08)                    CI0117
                          VALUE                SPACE.                   CI0117
            12            XW05-XSEGLV PICTURE  X(02)                    CI0117
                          VALUE                SPACE.                   CI0117
            12            XW05-XRC    PICTURE  X(02)                    CI0117
                          VALUE                SPACE.                   CI0117
            12            XW05-XPROPT PICTURE  X(04)                    CI0117
                          VALUE                SPACE.                   CI0117
            12            XW05-FILLER PICTURE  S9(5)                    CI0117
                          VALUE                ZERO                     CI0117
                          BINARY.                                       CI0117
            12            XW05-XSEGNM PICTURE  X(08)                    CI0117
                          VALUE                SPACE.                   CI0117
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0117
                          VALUE                ZERO                     CI0117
                          BINARY.                                       CI0117
            12            XW05-XSEGNB PICTURE  9(05)                    CI0117
                          VALUE                ZERO                     CI0117
                          BINARY.                                       CI0117
            12            XW05-XCOKEY PICTURE  X(70)                    CI0117
                          VALUE                SPACE.                   CI0117
            10            XW05-XW07.                                    CI0117
            11            XW05-XIOPCB.                                  CI0117
            12            XW05-XTERMI PICTURE  X(08)                    CI0117
                          VALUE                SPACE.                   CI0117
            12            XW05-FILLER PICTURE  XX                       CI0117
                          VALUE                SPACE.                   CI0117
            12            XW05-XRC1   PICTURE  X(02)                    CI0117
                          VALUE                SPACE.                   CI0117
            12            XW05-FILLER PICTURE  X(12)                    CI0117
                          VALUE                SPACE.                   CI0117
            12            XW05-XMODNM PICTURE  X(8)                     CI0117
                          VALUE                SPACE.                   CI0117
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0117
                          VALUE                ZERO.                    CI0117
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0117
                          VALUE                ZERO.                    CI0117
            10            XW05-XGU    PICTURE  X(4)                     CI0117
                          VALUE                'GU  '.                  CI0117
            10            XW05-XGHU   PICTURE  X(4)                     CI0117
                          VALUE                'GHU '.                  CI0117
            10            XW05-XGN    PICTURE  X(4)                     CI0117
                          VALUE                'GN  '.                  CI0117
            10            XW05-XGHN   PICTURE  X(4)                     CI0117
                          VALUE                'GHN '.                  CI0117
            10            XW05-XGNP   PICTURE  X(4)                     CI0117
                          VALUE                'GNP '.                  CI0117
            10            XW05-XGHNP  PICTURE  X(4)                     CI0117
                          VALUE                'GHNP'.                  CI0117
            10            XW05-XREPL  PICTURE  XXXX                     CI0117
                          VALUE                'REPL'.                  CI0117
            10            XW05-XISRT  PICTURE  X(4)                     CI0117
                          VALUE                'ISRT'.                  CI0117
            10            XW05-XDLET  PICTURE  X(4)                     CI0117
                          VALUE                'DLET'.                  CI0117
            10            XW05-XOPEN  PICTURE  X(4)                     CI0117
                          VALUE                'OPEN'.                  CI0117
            10            XW05-XCLSE  PICTURE  X(4)                     CI0117
                          VALUE                'CLSE'.                  CI0117
            10            XW05-XCHKP  PICTURE  X(4)                     CI0117
                          VALUE                'CHKP'.                  CI0117
            10            XW05-XXRST  PICTURE  X(4)                     CI0117
                          VALUE                'XRST'.                  CI0117
            10            XW05-XTERM  PICTURE  X(4)                     CI0117
                          VALUE                'TERM'.                  CI0117
            10            XW05-XNFPAC PICTURE  X(13)                    CI0117
                          VALUE                SPACE.                   CI0117
       01   DLIERROR  PIC X(8) VALUE 'DLIERROR'.                        AAAD30
       01   DEBUT-WSS.                                                  CI0117
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0117
            05   IK     PICTURE X.                                      CI0117
       01  CONSTANTES-PAC.                                              CI0117
           05  FILLER  PICTURE X(87)   VALUE                            CI0117
                     '9999 CAT09/08/14CI0117ADMIN   19:32:50CI0117  BVAPCI0117
      -    '09/08/20143.5 V0419/02/201425/02/2014'.                     CI0117
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0117
           05  NUGNA   PICTURE X(5).                                    CI0117
           05  APPLI   PICTURE X(3).                                    CI0117
           05  DATGN   PICTURE X(8).                                    CI0117
           05  PROGR   PICTURE X(6).                                    CI0117
           05  CODUTI  PICTURE X(8).                                    CI0117
           05  TIMGN   PICTURE X(8).                                    CI0117
           05  PROGE   PICTURE X(8).                                    CI0117
           05  COBASE  PICTURE X(4).                                    CI0117
           05  DATGNC  PICTURE X(10).                                   CI0117
           05  RELEAS  PICTURE X(7).                                    CI0117
           05  DATGE   PICTURE X(10).                                   CI0117
           05  DATSQ   PICTURE X(10).                                   CI0117
       01  DATCE.                                                       CI0117
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0117
         05  DATOR.                                                     CI0117
           10  DATOA  PICTURE XX.                                       CI0117
           10  DATOM  PICTURE XX.                                       CI0117
           10  DATOJ  PICTURE XX.                                       CI0117
       01   VARIABLES-CONDITIONNELLES.                                  CI0117
            05               FT.                                        CI0117
              10          TA-FT      PICTURE X VALUE '0'.               CI0117
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0117
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0117
            05           ITA71L PICTURE S9(4) VALUE  ZERO.              CI0117
            05           ITA71R PICTURE S9(4) VALUE  ZERO.              CI0117
            05           ITA71M PICTURE S9(4) VALUE +0100.              CI0117
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0117
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0117
            05       5-CY00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0117
       01               S-CX01-SSA.                                     CI0117
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0117
                                      VALUE 'CX01    '.                 CI0117
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0117
            10          S-CX01-CCOD   PICTURE X(5)                      CI0117
                                      VALUE '-----'.                    CI0117
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0117
       01            S-CXU01-SSA.                                       CI0117
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX01    '.                 CI0117
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0117
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(CX01K'.                   CI0117
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0117
            10       S-CXU01-CX01K.                                     CI0117
            11       S-CXU01-C199.                                      CI0117
            12       S-CXU01-CLID.                                      CI0117
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0117
            13       S-CXU01-CLIDN.                                     CI0117
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0117
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0117
            10  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01               S-CX03-SSA.                                     CI0117
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0117
                                      VALUE 'CX03    '.                 CI0117
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0117
            10          S-CX03-CCOD   PICTURE X(5)                      CI0117
                                      VALUE '-----'.                    CI0117
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0117
       01            S-CXA03-SSA.                                       CI0117
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX03    '.                 CI0117
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0117
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(CARTY'.                   CI0117
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0117
            12       S-CXA03-CARTY    PICTURE  99.                      CI0117
            12  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01            S-CXB03-SSA.                                       CI0117
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX03    '.                 CI0117
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0117
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(NARRS'.                   CI0117
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0117
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            12  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01            S-CXC03-SSA.                                       CI0117
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX03    '.                 CI0117
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0117
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(CPMTG'.                   CI0117
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0117
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0117
            11  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01            S-CXD03-SSA.                                       CI0117
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX03    '.                 CI0117
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0117
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(GRCRNG'.                  CI0117
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0117
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0117
            11  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01            S-CXE03-SSA.                                       CI0117
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX03    '.                 CI0117
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0117
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(DEXDT'.                   CI0117
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0117
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0117
            11  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01            S-CXF03-SSA.                                       CI0117
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX03    '.                 CI0117
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0117
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(CY50'.                    CI0117
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0117
            11       S-CXF03-CY50.                                      CI0117
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0117
            11  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01            S-CXG03-SSA.                                       CI0117
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX03    '.                 CI0117
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0117
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(NBASQ'.                   CI0117
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0117
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            11  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01            S-CXH03-SSA.                                       CI0117
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX03    '.                 CI0117
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0117
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(NARID'.                   CI0117
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0117
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0117
            12  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01            S-CXU03-SSA.                                       CI0117
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX03    '.                 CI0117
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0117
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(CX03K'.                   CI0117
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0117
            11       S-CXU03-CX03K.                                     CI0117
            12       S-CXU03-CARTY    PICTURE  99.                      CI0117
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            11  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01               S-CX06-SSA.                                     CI0117
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0117
                                      VALUE 'CX06    '.                 CI0117
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0117
            10          S-CX06-CCOD   PICTURE X(5)                      CI0117
                                      VALUE '-----'.                    CI0117
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0117
       01            S-CXU06-SSA.                                       CI0117
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX06    '.                 CI0117
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0117
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(CX06K'.                   CI0117
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0117
            10       S-CXU06-CX06K.                                     CI0117
            11       S-CXU06-C299.                                      CI0117
            12       S-CXU06-CTID.                                      CI0117
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0117
            13       S-CXU06-CTIDN.                                     CI0117
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0117
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0117
            10  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01               S-CX13-SSA.                                     CI0117
            10         S1-CX13-SEGNAM PICTURE X(8)                      CI0117
                                      VALUE 'CX13    '.                 CI0117
            10         S1-CX13-CCOM   PICTURE X VALUE '*'.              CI0117
            10          S-CX13-CCOD   PICTURE X(5)                      CI0117
                                      VALUE '-----'.                    CI0117
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0117
       01            S-CXA13-SSA.                                       CI0117
            11      S1-CXA13-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX13    '.                 CI0117
            11      S1-CXA13-CCOM   PICTURE X VALUE '*'.                CI0117
            11       S-CXA13-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            11      S1-CXA13-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(CDEST'.                   CI0117
            11       S-CXA13-OPER  PICTURE XX VALUE ' ='.               CI0117
            11       S-CXA13-CDEST    PICTURE  99.                      CI0117
            11  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01            S-CXB13-SSA.                                       CI0117
            12      S1-CXB13-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX13    '.                 CI0117
            12      S1-CXB13-CCOM   PICTURE X VALUE '*'.                CI0117
            12       S-CXB13-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            12      S1-CXB13-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(CARTZ'.                   CI0117
            12       S-CXB13-OPER  PICTURE XX VALUE ' ='.               CI0117
            12       S-CXB13-CARTZ    PICTURE  99.                      CI0117
            12  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01            S-CXC13-SSA.                                       CI0117
            12      S1-CXC13-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX13    '.                 CI0117
            12      S1-CXC13-CCOM   PICTURE X VALUE '*'.                CI0117
            12       S-CXC13-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            12      S1-CXC13-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(NAPDS'.                   CI0117
            12       S-CXC13-OPER  PICTURE XX VALUE ' ='.               CI0117
            12       S-CXC13-NAPDS    PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            12  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01            S-CXU13-SSA.                                       CI0117
            11      S1-CXU13-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX13    '.                 CI0117
            11      S1-CXU13-CCOM   PICTURE X VALUE '*'.                CI0117
            11       S-CXU13-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            11      S1-CXU13-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(CX13K'.                   CI0117
            11       S-CXU13-OPER  PICTURE XX VALUE ' ='.               CI0117
            11       S-CXU13-CX13K.                                     CI0117
            12       S-CXU13-CARTZ    PICTURE  99.                      CI0117
            12       S-CXU13-NAPDS    PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            11  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01            S-CX113-SSA.                                       CI0117
            11      S1-CX113-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX13    '.                 CI0117
            11      S1-CX113-CCOM   PICTURE X VALUE '*'.                CI0117
            11       S-CX113-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            11      S1-CX113-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(XGCUSPY'.                 CI0117
            11       S-CX113-OPER  PICTURE XX VALUE ' ='.               CI0117
            11       S-CX113-GCUSPY   PICTURE  X(12).                   CI0117
            11  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01               S-CX14-SSA.                                     CI0117
            10         S1-CX14-SEGNAM PICTURE X(8)                      CI0117
                                      VALUE 'CX14    '.                 CI0117
            10         S1-CX14-CCOM   PICTURE X VALUE '*'.              CI0117
            10          S-CX14-CCOD   PICTURE X(5)                      CI0117
                                      VALUE '-----'.                    CI0117
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0117
       01            S-CXU14-SSA.                                       CI0117
            10      S1-CXU14-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX14    '.                 CI0117
            10      S1-CXU14-CCOM   PICTURE X VALUE '*'.                CI0117
            10       S-CXU14-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            10      S1-CXU14-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(CX14K'.                   CI0117
            10       S-CXU14-OPER  PICTURE XX VALUE ' ='.               CI0117
            10       S-CXU14-CX14K.                                     CI0117
            11       S-CXU14-NPISQ    PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            10  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01            S-CX114-SSA.                                       CI0117
            11      S1-CX114-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX14    '.                 CI0117
            11      S1-CX114-CCOM   PICTURE X VALUE '*'.                CI0117
            11       S-CX114-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            11      S1-CX114-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(XGCUSPZ'.                 CI0117
            11       S-CX114-OPER  PICTURE XX VALUE ' ='.               CI0117
            11       S-CX114-GCUSPZ   PICTURE  X(12).                   CI0117
            11  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01               S-CY2Y-SSA.                                     CI0117
            10         S1-CY2Y-SEGNAM PICTURE X(8)                      CI0117
                                      VALUE 'CX2Y    '.                 CI0117
            10         S1-CY2Y-CCOM   PICTURE X VALUE '*'.              CI0117
            10          S-CY2Y-CCOD   PICTURE X(5)                      CI0117
                                      VALUE '-----'.                    CI0117
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0117
       01            S-CYA2Y-SSA.                                       CI0117
            11      S1-CYA2Y-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX2Y    '.                 CI0117
            11      S1-CYA2Y-CCOM   PICTURE X VALUE '*'.                CI0117
            11       S-CYA2Y-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            11      S1-CYA2Y-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(CARTY'.                   CI0117
            11       S-CYA2Y-OPER  PICTURE XX VALUE ' ='.               CI0117
            11       S-CYA2Y-CARTY    PICTURE  99.                      CI0117
            11  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01            S-CYB2Y-SSA.                                       CI0117
            11      S1-CYB2Y-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX2Y    '.                 CI0117
            11      S1-CYB2Y-CCOM   PICTURE X VALUE '*'.                CI0117
            11       S-CYB2Y-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            11      S1-CYB2Y-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(C299'.                    CI0117
            11       S-CYB2Y-OPER  PICTURE XX VALUE ' ='.               CI0117
            11       S-CYB2Y-C299.                                      CI0117
            12       S-CYB2Y-CTID.                                      CI0117
            13       S-CYB2Y-CTIDA    PICTURE  9(3).                    CI0117
            13       S-CYB2Y-CTIDN.                                     CI0117
            14       S-CYB2Y-CTIDNP   PICTURE  X(13).                   CI0117
            14       S-CYB2Y-CTIDND   PICTURE  9(11).                   CI0117
            11  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01            S-CYU2Y-SSA.                                       CI0117
            10      S1-CYU2Y-SEGNAM PICTURE X(8)                        CI0117
                                      VALUE 'CX2Y    '.                 CI0117
            10      S1-CYU2Y-CCOM   PICTURE X VALUE '*'.                CI0117
            10       S-CYU2Y-CCOD   PICTURE X(5)                        CI0117
                                      VALUE '-----'.                    CI0117
            10      S1-CYU2Y-FLDNAM PICTURE X(9)                        CI0117
                                      VALUE '(CX2YK'.                   CI0117
            10       S-CYU2Y-OPER  PICTURE XX VALUE ' ='.               CI0117
            10       S-CYU2Y-CX2YK.                                     CI0117
            11       S-CYU2Y-C299.                                      CI0117
            12       S-CYU2Y-CTID.                                      CI0117
            13       S-CYU2Y-CTIDA    PICTURE  9(3).                    CI0117
            13       S-CYU2Y-CTIDN.                                     CI0117
            14       S-CYU2Y-CTIDNP   PICTURE  X(13).                   CI0117
            14       S-CYU2Y-CTIDND   PICTURE  9(11).                   CI0117
            11       S-CYU2Y-C199.                                      CI0117
            12       S-CYU2Y-CLID.                                      CI0117
            13       S-CYU2Y-CLIDO    PICTURE  9(3).                    CI0117
            13       S-CYU2Y-CLIDN.                                     CI0117
            14       S-CYU2Y-CLIDNP   PICTURE  X(12).                   CI0117
            14       S-CYU2Y-CLIDND   PICTURE  9(8).                    CI0117
            11       S-CYU2Y-CARTY    PICTURE  99.                      CI0117
            11       S-CYU2Y-NARRS    PICTURE  S9(3)                    CI0117
                          COMPUTATIONAL-3.                              CI0117
            10  FILLER   PICTURE X    VALUE ')'.                        CI0117
       01               1-TA-TABLE.                                     CI0117
           02           1-TA71T.                                        CI0117
            05          1-TA71 OCCURS                0100.              CI0117
            10          1-TA71-CPMTF  PICTURE  99.                      CI0117
            10          1-TA71-MPMTF  PICTURE  X(14).                   CI0117
            10          1-TA71-CPMTFA PICTURE  X(2).                    CI0117
            10          1-TA71-MPMTFL PICTURE  X(24).                   CI0117
       01   ZONES-UTILISATEUR PICTURE X.                                CI0117
       LINKAGE SECTION.                                                 AAAD30
      *PCB MASK FOR AR1P
      *!WF DSP=PA DSL=XW SEL=06 FOR=I DES=2 LEV=1 PLT=03 id=7
       01                 PA06.                                         CI0117
            10            PA06-XDBPCB.                                  CI0117
            11            PA06-XDBDNM PICTURE  X(08)                    CI0117
                          VALUE                SPACE.                   CI0117
            11            PA06-XSEGLV PICTURE  X(02)                    CI0117
                          VALUE                SPACE.                   CI0117
            11            PA06-XRC    PICTURE  X(02)                    CI0117
                          VALUE                SPACE.                   CI0117
            11            PA06-XPROPT PICTURE  X(04)                    CI0117
                          VALUE                SPACE.                   CI0117
            11            PA06-FILLER PICTURE  S9(5)                    CI0117
                          VALUE                ZERO                     CI0117
                          BINARY.                                       CI0117
            11            PA06-XSEGNM PICTURE  X(08)                    CI0117
                          VALUE                SPACE.                   CI0117
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0117
                          VALUE                ZERO                     CI0117
                          BINARY.                                       CI0117
            11            PA06-XSEGNB PICTURE  9(05)                    CI0117
                          VALUE                ZERO                     CI0117
                          BINARY.                                       CI0117
            11            PA06-XCOKEY PICTURE  X(70)                    CI0117
                          VALUE                SPACE.                   CI0117
      *PCB MASK FOR ARAY
      *!WF DSP=PB DSL=XW SEL=06 FOR=I DES=2 LEV=1 PLT=03 id=8
       01                 PB06.                                         CI0117
            10            PB06-XDBPCB.                                  CI0117
            11            PB06-XDBDNM PICTURE  X(08)                    CI0117
                          VALUE                SPACE.                   CI0117
            11            PB06-XSEGLV PICTURE  X(02)                    CI0117
                          VALUE                SPACE.                   CI0117
            11            PB06-XRC    PICTURE  X(02)                    CI0117
                          VALUE                SPACE.                   CI0117
            11            PB06-XPROPT PICTURE  X(04)                    CI0117
                          VALUE                SPACE.                   CI0117
            11            PB06-FILLER PICTURE  S9(5)                    CI0117
                          VALUE                ZERO                     CI0117
                          BINARY.                                       CI0117
            11            PB06-XSEGNM PICTURE  X(08)                    CI0117
                          VALUE                SPACE.                   CI0117
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0117
                          VALUE                ZERO                     CI0117
                          BINARY.                                       CI0117
            11            PB06-XSEGNB PICTURE  9(05)                    CI0117
                          VALUE                ZERO                     CI0117
                          BINARY.                                       CI0117
            11            PB06-XCOKEY PICTURE  X(70)                    CI0117
                          VALUE                SPACE.                   CI0117
      ******************************************************************
      **** INPUT AND OUTPUT SEGMENTS FOR CALLING THE CATS AAPS MODULE **
      ******************************************************************
       01                 RT00.
          05              RT00-IND      PIC X(1)     VALUE SPACES.
          05              RT00-MESG     PIC X(30)    VALUE SPACES.
          05              RT00-CPMTF    PIC X(2)     VALUE SPACES.
      *!WI id=9
          05              RT00-ADBRQ                 VALUE ZEROES
                        PICTURE S9(11)V99                               CI0117
                            COMPUTATIONAL-3.                            CI0117
      *!WI id=10
       01                 CT01-CTID                  VALUE SPACES
                        PICTURE X(27).                                  CI0117
      *
      *WORKING STORAGE TO HOLD THE POINTER VARIABLE
      ******************************************************************
      ** PCB ADDR LIST FOR CI0117.THE PCB IS REQUIRED FOR CONTRACT DB  *
      ******************************************************************
       01      CI0117-PCB-ARAY-PRT1      POINTER.
       01      CI0117-PCB-AR1P-PRT1      POINTER.
       PROCEDURE DIVISION USING
                          CT01-CTID
                          CI0117-PCB-ARAY-PRT1
                          CI0117-PCB-AR1P-PRT1
                          RT00.
                                                                        DOT
      *N01.      NOTE *************************************.            CI0117
      *               *                                   *             CI0117
      *               *INITIALISATIONS                    *             CI0117
      *               *                                   *             CI0117
      *               *************************************.            CI0117
       F01.      EXIT.
      *N01TA.    NOTE *OPENING TA71                       *.
       F01TA.    IF    WS00-TA71-RD NOT = 'Y'                           lv10
                 NEXT SENTENCE ELSE GO TO     F01TA-FN.
      *OPENING TA71                                                     DOT
           MOVE        'OP' TO G-TA71-TABFO                             AAAT84
           CALL        PTA900                                           AAAT84
           USING G-TA71.                                                AAAT84
                 IF    G-TA71-TABCR NOT = '00'                          DOT
           PERFORM     F93TA THRU F93TA-FN                              AAAT84
           MOVE                    ALL '1' TO FT GO TO F20.             AAAT84
       F01TA-FN. EXIT.
       F01-FN.   EXIT.
      *N02SZ.    NOTE *LOADING OF TA71 IF FIRST TIME      *.
       F02SZ.    IF    WS00-TA71-RD NOT = 'Y'                           lv10
                 NEXT SENTENCE ELSE GO TO     F02SZ-FN.
      *N02TA.    NOTE *LOADING OF TA71                    *.
       F02TA.                       GO TO     F02TA-B.                  lv15
       F02TA-A.
                 IF    ITA71L > ITA71M
                                    GO TO     F02TA-FN.
       F02TA-B.
           MOVE        'L1' TO G-TA71-TABFO                             AAAT83
           CALL        PTA900 USING G-TA71.                             AAAT83
                 IF    G-TA71-TABCR = '10'                              DOT
           MOVE        '1' TO TA-FT                                     AAAT83
           GO TO F02SZ-FN.                                              AAAT83
                 IF    G-TA71-TABCR NOT = '00'                          DOT
           MOVE        SPACES TO RT00-CPMTF
           MOVE        ZEROES TO RT00-ADBRQ
           MOVE        '2' TO RT00-IND
           MOVE        'INTERNAL ERROR' TO RT00-MESG
           MOVE                    ALL '1' TO FT GO TO F20.             AAAT83
           ADD         1 TO ITA71L.                                     DOT
                 IF    ITA71L NOT > ITA71M                              DOT
           MOVE        TA71 TO 1-TA71 (ITA71L)                          AAAT83
                 ELSE
           MOVE        SPACES TO RT00-CPMTF
           MOVE        ZEROES TO RT00-ADBRQ
           MOVE        '2' TO RT00-IND
           MOVE        'INTERNAL ERROR' TO RT00-MESG
           MOVE                     ALL '1' TO FT GO TO F20.
       F02TA-900. GO TO F02TA-A.
       F02TA-FN. EXIT.
       F02SZ-FN. EXIT.
      *N02ZA.    NOTE *RESET 'FT' FOR MULTIPLE CALLS      *.
       F02ZA.                                                           lv10
      *********************************
      *IN ORDER FOR THIS PROGRAM TO
      *BE CALLED MULTIPLE TIMES BY THE
      *SAME CALLING PROGRAM, 'FT' MUST
      *BE SET BACK TO ZEROES.
      *********************************
           MOVE ALL    ZEROES TO FT.
       F02ZA-FN. EXIT.
      *N02ZZ.    NOTE *SET ADDRESSES FOR DATABASE ACCES   *.
       F02ZZ.                                                           lv10
      *********************************
           SET ADDRESS OF PA06 TO
                     CI0117-PCB-ARAY-PRT1
           SET ADDRESS OF PB06 TO
                     CI0117-PCB-AR1P-PRT1.
       F02ZZ-FN. EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0117
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0117
      *               *                                   *             CI0117
      *               *FIN DE TRAITEMENT                  *             CI0117
      *               *                                   *             CI0117
      *               *************************************.            CI0117
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0117
      *N20TA.    NOTE *CLOSING TA71                       *.
       F20TA.    IF    WS00-TA71-RD NOT = 'Y'                           lv10
                 NEXT SENTENCE ELSE GO TO     F20TA-FN.
           MOVE        'Y' TO WS00-TA71-RD.                             DOT
      *CLOSING TA71                                                     DOT
           MOVE        'CL' TO G-TA71-TABFO                             AAAT84
           CALL        PTA900                                           AAAT84
           USING G-TA71.                                                AAAT84
                 IF    G-TA71-TABCR NOT = '00'                          DOT
           PERFORM     F93TA THRU F93TA-FN.                             AAAT84
       F20TA-FN. EXIT.
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
      *THIS CALLED PROGRAM WILL ACCEPT
      *THE ACCOUNT ID FROM THE I&A
      *SYSTEM AND RETURN THE ERROR MSG,
      *FREQUENCY,AMOUNT AND THE RETURN
      *CODE TO INDICATE THE SUCCESS OR
      *FAILURE OF THE CALL.
           INITIALIZE  WS00
           MOVE        'N' TO WS00-ACT-FND WS00-FUT-FND
           WS00-INACT-FND
           INITIALIZE  CF00.
      *N40BB.    NOTE *READ CY2Y SEGMENT                  *.
       F40BB.                                                           lv10
      *********************************
           MOVE        CT01-CTID TO S-CYU2Y-CTID
           MOVE        ZEROS TO S-CYU2Y-C199
           MOVE        ZEROS TO S-CYU2Y-CARTY
           S-CYU2Y-NARRS
           MOVE        'GE' TO S-CYU2Y-OPER
      *GU CALL ON CY2Y
           PERFORM     F96AD THRU F96AD-FN.
       F40BB-FN. EXIT.
      *N40BF.    NOTE *LOOP THROUGH ALL CX2Y, WHERE       *.
       F40BF.    IF    CY2Y-CF = '1'                                    lv10
                 AND   CY2Y-CTID = CT01-CTID
                 AND   WS00-FUT-FND NOT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40BF-FN.
      *ACCT ID IS EQUAL TO THE RECEIVED
      *********************************
      *N40BJ.    NOTE *CX2Y FOUND, CHECK IF SD ARRANG.    *.
       F40BJ.    IF    CY2Y-CARTY = 10                                  lv15
                 NEXT SENTENCE ELSE GO TO     F40BJ-FN.
      *IF NOT READ THE NEXT CX2Y
      *********************************
           MOVE        'Y' TO CY2Y-FOUND
           MOVE        CY2Y-CLID TO S-CXU01-CLID
           MOVE        CY2Y-CARTY TO S-CXU03-CARTY
           MOVE        CY2Y-NARRS TO S-CXU03-NARRS
           MOVE        CY2Y-CTID TO S-CXU06-CTID
      *GU CALL ON CX06
           PERFORM     F96AN THRU F96AN-FN.
                 IF    CX06-CF = '1'                                    DOT
      *GN CALL ON CX13
           PERFORM     F96AF THRU F96AF-FN.
      *N40BN.    NOTE *LOOP THROUGH CX13'S                *.
       F40BN.    IF    CX13-CF = '1'                                    lv20
                 AND   WS00-FUT-FND NOT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40BN-FN.
      *********************************
           MOVE        CX13-CARTZ TO WS00-CARTZ
           MOVE        CX13-NAPDS TO WS00-NAPDS.
      *N40BR.    NOTE *IF CX13 FOUND,CHECK ON THE CARTZ   *.
       F40BR.    IF    (CX13-CARTZ = 07                                 lv25
                 OR    CX13-CARTZ = 09)
                 NEXT SENTENCE ELSE GO TO     F40BR-FN.
      *VALUE FOR EITHER 'AD' OR 'IO'.
      *********************************
      *N40BT.    NOTE *IF CX13 FOUND,AND EITHER AN 'AD'   *.
       F40BT.                                                           lv30
      *OR 'SD', READ CX14 SEGMENT.
      *********************************
           MOVE        CX13-CARTZ TO S-CXU13-CARTZ
           MOVE        CX13-NAPDS TO S-CXU13-NAPDS
      *GN CALL ON CX14
           PERFORM     F96AK THRU F96AK-FN.
      *N40BU.    NOTE *IF CX14 READ SUCCESSFUL,CHECK      *.
       F40BU.    IF    (CX14-CF = '1'                                   lv35
                 AND   CX14-CPITC = 02
                 AND   CX14-CTID = CT01-CTID)
                 NEXT SENTENCE ELSE GO TO     F40BU-FN.
      *FOR THE TYPE OF ARRANGEMENT
      *(TRANSFER/NON TRANSFER). IF
      *INTRA-TRANSFER, THEN IGNORE IT.
      *********************************
               GO TO     F40BT-FN.
       F40BU-FN. EXIT.
       F40BT-FN. EXIT.
      *N40BV.    NOTE *CHECK THE STATUS.IF FUTURE,        *.
       F40BV.    IF    CX13-CDEST = 04                                  lv30
                 NEXT SENTENCE ELSE GO TO     F40BV-FN.
      *CAPTURE THE FREQ AND AMOUNT AND
      *RETURN TO THE CALLING PROGRAM
      *INDICATING THE STATUS OF THE CAL
      *AS SUCCESSFUL.
      *********************************
           MOVE        CX13-CPMTF TO TA71-CPMTF
           MOVE 1 TO     ITA71R.
       F40BV-120. IF     ITA71R NOT >    ITA71L
           AND         1-TA71-CPMTF     (ITA71R)
           NOT =           TA71-CPMTF
           ADD 1 TO      ITA71R    GO TO F40BV-120.
                 IF    ITA71L > ITA71R                                  DOT
           MOVE        1-TA71-CPMTFA (ITA71R) TO
           RT00-CPMTF
                 ELSE
           MOVE        SPACES TO RT00-CPMTF.
           MOVE        CX13-ADBRQ TO RT00-ADBRQ                         DOT
           MOVE        '1' TO RT00-IND
           MOVE        SPACES TO RT00-MESG
           MOVE        'Y' TO WS00-FUT-FND.
       F40BV-FN. EXIT.
      *N40CA.    NOTE *CHECK THE STATUS.IF ACTIVE,        *.
       F40CA.    IF    CX13-CDEST = 01                                  lv30
                 AND   CX13-GESTD > WS00-GEEND
                 NEXT SENTENCE ELSE GO TO     F40CA-FN.
      *CHECK FOR THE MOST RECENT ACTIVE
      *AAPS ARRANGEMENT CREATED.
      *********************************
           MOVE        CX13-GESTD TO WS00-GESTD
           MOVE        CX13-GEEND TO WS00-GEEND
           MOVE        CX13-CPMTF TO WS00-CPMTF
           MOVE        CX13-ADBRQ TO WS00-ADBRQ
           MOVE        'Y' TO WS00-ACT-FND.
       F40CA-FN. EXIT.
      *N40CC.    NOTE *CHECK THE STATUS.IF INACTIVE,      *.
       F40CC.    IF    CX13-CDEST = 03                                  lv30
                 NEXT SENTENCE ELSE GO TO     F40CC-FN.
      *CHECK FOR THE
      *INACTIVE AAPS ARRANGEMENT
      *CREATED
      *********************************
           MOVE        CX13-CPMTF TO WS00-CPMTF
           MOVE        CX13-ADBRQ TO WS00-ADBRQ
           MOVE        'Y' TO WS00-INACT-FND.
       F40CC-FN. EXIT.
       F40BR-FN. EXIT.
      *N40CE.    NOTE *READ THE NEXT CX13 SEGMENT.        *.
       F40CE.                                                           lv25
      *********************************
           PERFORM     F96AF THRU F96AF-FN.
       F40CE-FN. EXIT.
       F40BN-900. GO TO F40BN.
       F40BN-FN. EXIT.
       F40BJ-FN. EXIT.
      *N40CG.    NOTE *READ THE NEXT CX2Y SEGMENT.        *.
       F40CG.                                                           lv15
      *********************************
           PERFORM     F96AH THRU F96AH-FN.
       F40CG-FN. EXIT.
       F40BF-900. GO TO F40BF.
       F40BF-FN. EXIT.
      *N40CI.    NOTE *END OF CX13'S/END OF CX2Y AND      *.
       F40CI.    IF    WS00-ACT-FND = 'Y'                               lv10
                 OR    WS00-INACT-FND = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40CI-FN.
      *AAPS ARRANGEMENT FOUND
      *********************************
      *N40CJ.    NOTE *END OF CX13'S/END OF CX2Y AND      *.
       F40CJ.    IF    WS00-ACT-FND = 'Y'                               lv15
                 NEXT SENTENCE ELSE GO TO     F40CJ-FN.
      *ACTIVE AAPS ARRANGEMENT FOUND
      *********************************
           MOVE        WS00-CPMTF TO TA71-CPMTF
           MOVE 1 TO     ITA71R.
       F40CJ-070. IF     ITA71R NOT >    ITA71L
           AND         1-TA71-CPMTF     (ITA71R)
           NOT =           TA71-CPMTF
           ADD 1 TO      ITA71R    GO TO F40CJ-070.
                 IF    ITA71L > ITA71R                                  DOT
           MOVE        1-TA71-CPMTFA (ITA71R) TO
           RT00-CPMTF
                 ELSE
           MOVE        SPACES TO RT00-CPMTF.
           MOVE        WS00-ADBRQ TO RT00-ADBRQ                         DOT
           MOVE        '1' TO RT00-IND
           MOVE        SPACES TO RT00-MESG.
       F40CJ-FN. EXIT.
      *N40CK.    NOTE *END OF CX13'S/END OF CX2Y AND      *.
       F40CK.    IF    WS00-ACT-FND = 'N'                               lv15
                 AND   WS00-FUT-FND = 'N'
                 AND   WS00-INACT-FND = 'Y'
                 NEXT SENTENCE ELSE GO TO     F40CK-FN.
      *INACTIVE AAPS ARRANGEMENT FOUND
      *********************************
           MOVE        WS00-CPMTF TO TA71-CPMTF
           MOVE 1 TO     ITA71R.
       F40CK-070. IF     ITA71R NOT >    ITA71L
           AND         1-TA71-CPMTF     (ITA71R)
           NOT =           TA71-CPMTF
           ADD 1 TO      ITA71R    GO TO F40CK-070.
                 IF    ITA71L > ITA71R                                  DOT
           MOVE        1-TA71-CPMTFA (ITA71R) TO
           RT00-CPMTF
                 ELSE
           MOVE        SPACES TO RT00-CPMTF.
           MOVE        WS00-ADBRQ TO RT00-ADBRQ                         DOT
           MOVE        '3' TO RT00-IND
           MOVE        'ARRANGEMENT INACTIVE' TO RT00-MESG.
       F40CK-FN. EXIT.
       F40CI-FN. EXIT.
      *N40CM.    NOTE *NO ARRANGEMENTS FOUND              *.
       F40CM.    IF    WS00-ACT-FND = 'N'                               lv10
                 AND   WS00-FUT-FND = 'N'
                 AND   WS00-INACT-FND = 'N'
                 NEXT SENTENCE ELSE GO TO     F40CM-FN.
      *********************************
           MOVE        SPACES TO RT00-CPMTF
           MOVE        ZEROES TO RT00-ADBRQ
           MOVE        '2' TO RT00-IND
           MOVE        'NO ARRANGEMENTS FOUND' TO
           RT00-MESG.
       F40CM-FN. EXIT.
       F40-FN.   EXIT.
      *N79.      NOTE *************************************.
      *               *                                   *
      *               *END OF ITERATION                   *
      *               *                                   *
      *               *************************************.
       F79.                                                             lv05
      *********************************
      *GET OUT OF THE PACBASE GENERATED
      *LOOP HERE, BECAUSE WE DO NOT
      *WANT TO AUTOMATICALLY WRITE AN
      *OUTPUT RECORD. THIS SECTION
      *BYPASSES THE AUTOMATIC SEGEMENT
      *WRITE.
      *********************************
      *N7999.    NOTE *INITIATE RETURN TO CALLING PGM     *.
       F7999.                                                           lv10
      *********************************
      *INITIATE RETURN TO CALLING
      *PROGRAM BY:
      *1: SETTING 'FT' TO ALL ONES
      *2: BRANCHING TO F20
      *********************************
           MOVE                     ALL '1' TO FT GO TO F20.
       F7999-FN. EXIT.
       F79-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
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
           MOVE                    ALL '1' TO FT GO TO F20.             AAAD30
       F93EC-FN. EXIT.
      *N93TA.    NOTE *RETURN CODE PROCESSING FUNCTION    *.            AAAT84
       F93TA.                                                           lv10
           DISPLAY     'TABLE MANAGER ERROR '                           AAAT84
           DISPLAY     'TABLE :   ' G-TA71-NUTAB                        AAAT84
           DISPLAY     'FUNCTION :   ' G-TA71-TABFO                     AAAT84
           DISPLAY     'RETURN CODE :   ' G-TA71-TABCR.                 AAAT84
       F93TA-FN. EXIT.
      *N96AD.    NOTE *CALL GU ON CY2Y                    *.
       F96AD.                                                           lv10
      *CALL GU ON CY2Y                                                  DOT
           MOVE        '96AD-100 GU' TO XW05-XNFPAC                     AAAD36
           CALL        'CBLTDLI' USING XW05-XGU                         AAAD36
           PA06 CY2Y                                                    AAAD36
           S-CYU2Y-SSA                                                  AAAD36
           MOVE        PA06 TO XW05-XDBPCB                              AAAD36
           PERFORM     F93EA THRU F93EA-FN.                             AAAD36
                 IF    IK = '0'                                         DOT
      *CY2Y SEGMENT FOUND
           MOVE        '1' TO CY2Y-CF
                 ELSE
      *CY2Y SEGMENT NOT FOUND
           MOVE        '0' TO CY2Y-CF.
       F96AD-FN. EXIT.
      *N96AF.    NOTE *CALL GN ON CX13                    *.
       F96AF.                                                           lv10
      *CALL GN ON CX13                                                  DOT
           MOVE        '96AF-100 GN' TO XW05-XNFPAC                     AAAD36
           CALL        'CBLTDLI' USING XW05-XGN                         AAAD36
           PB06 CX13                                                    AAAD36
           S-CXU01-SSA S-CXU03-SSA                                      AAAD36
           S-CXU06-SSA S-CX13-SSA
           MOVE        PB06 TO XW05-XDBPCB                              AAAD36
           PERFORM     F93EA THRU F93EA-FN.                             AAAD36
                 IF    IK = '0'                                         DOT
      *CX13 SEGMENT FOUND
           MOVE        '1' TO CX13-CF
                 ELSE
      *CX13 SEGMENT NOT FOUND
           MOVE        '0' TO CX13-CF.
       F96AF-FN. EXIT.
      *N96AH.    NOTE *CALL GN ON CY2Y                    *.
       F96AH.                                                           lv10
      *CALL GN ON CY2Y                                                  DOT
           MOVE        '96AH-100 GN' TO XW05-XNFPAC                     AAAD36
           CALL        'CBLTDLI' USING XW05-XGN                         AAAD36
           PA06 CY2Y                                                    AAAD36
           S-CYU2Y-SSA                                                  AAAD36
           MOVE        PA06 TO XW05-XDBPCB                              AAAD36
           PERFORM     F93EA THRU F93EA-FN.                             AAAD36
                 IF    IK = '0'                                         DOT
      *CY2Y SEGMENT FOUND
           MOVE        '1' TO CY2Y-CF
                 ELSE
      *CY2Y SEGMENT NOT FOUND
           MOVE        '0' TO CY2Y-CF.
       F96AH-FN. EXIT.
      *N96AK.    NOTE *CALL GN ON CX14                    *.
       F96AK.                                                           lv10
      *CALL GN ON CX14                                                  DOT
           MOVE        '96AK-100 GN' TO XW05-XNFPAC                     AAAD36
           CALL        'CBLTDLI' USING XW05-XGN                         AAAD36
           PB06 CX14                                                    AAAD36
           S-CXU01-SSA S-CXU03-SSA                                      AAAD36
           S-CXU06-SSA S-CXU13-SSA
           S-CX14-SSA
           MOVE        PB06 TO XW05-XDBPCB                              AAAD36
           PERFORM     F93EA THRU F93EA-FN.                             AAAD36
                 IF    IK = '0'                                         DOT
      *CX14 SEGMENT FOUND
           MOVE        '1' TO CX14-CF
                 ELSE
      *CX14 SEGMENT NOT FOUND
           MOVE        '0' TO CX14-CF.
       F96AK-FN. EXIT.
      *N96AN.    NOTE *CALL GU ON CX06                    *.
       F96AN.                                                           lv10
      *CALL GU ON CX06                                                  DOT
           MOVE        '96AN-100 GU' TO XW05-XNFPAC                     AAAD36
           CALL        'CBLTDLI' USING XW05-XGU                         AAAD36
           PB06 CX06                                                    AAAD36
           S-CXU01-SSA S-CXU03-SSA                                      AAAD36
           S-CXU06-SSA
           MOVE        PB06 TO XW05-XDBPCB                              AAAD36
           PERFORM     F93EA THRU F93EA-FN.                             AAAD36
                 IF    IK = '0'                                         DOT
      *CX06 SEGMENT FOUND
           MOVE        '1' TO CX06-CF
                 ELSE
      *CX06 SEGMENT NOT FOUND
           MOVE        '0' TO CX06-CF.
       F96AN-FN. EXIT.
