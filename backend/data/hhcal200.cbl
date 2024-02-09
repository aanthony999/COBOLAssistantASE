000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID.    HHCAL200.                                         00020000
000210 DATE-COMPILED.                                                   00021000
000220******************************************************************00022000
000230* NATIONAL HHA PRICER EFFECTIVE JANUARY 1, 2020.                 *00023000
000231******************************************************************00023100
000232*    CY2020.0 CHANGES                                            *00023200
000233*----------------------------------------------------------------*00023300
000234* UPDATED RURAL FACTOR FROM 1.0043 TO 1.0024                     *00023400
000235* UPDATED RURAL ADD ON PERCENTAGES                               *00023500
000236* UPDATED STANDARDIZED PAYMENT AMOUNT                            *00023600
000237* ALL RAPS SHOULD BE PAID 20%                                    *00023700
000238* OUTLIER-THRESHOLD-AMT = FED-EPISODE-RATE-AMT X .56 (FROM .51)  *00023800
000239******************************************************************00023900
000240******************************************************************00024000
000250*    RETURN CODES                                                *00025000
000260******************************************************************00026000
000270*  00 = FINAL PAYMENT                                            *00027000
000280*       TOB = 327, 329, 32F, 32G, 32H, 32I, 32J,                 *00028000
000290*             32K, 32M, 32Q, 32P                                 *00029000
000300*       WITH HRG,REVENUE CODE WHERE NO OUTLIER APPLIES           *00030000
000400*  01 = FINAL PAYMENT                                            *00040000
000500*       TOB = 327, 329, 32F, 32G, 32H, 32I, 32J,                 *00050000
000600*             32K, 32M, 32Q, 32P                                 *00060000
000700*       WITH HRG,REVENUE CODE WHERE OUTLIER APPLIES              *00070000
000800*  03 = INITIAL HALF PAYMENT PAYMENT WILL BE ZERO                *00080000
000900*       TOB = 322                                                *00090000
001000*  04 = INITIAL HALF PAYMENT PAID AT 50%                         *00100000
001100*       TOB = 322                                                *00110000
001200*       WITH INITIAL (FIRST) HRG AND NO REVENUE CODES            *00120000
001300*  05 = INITIAL HALF PAYMENT PAID AT 60%                         *00130000
001400*       TOB = 322                                                *00140000
001500*       WITH INITIAL (FIRST) HRG AND NO REVENUE CODES            *00150000
001600*  06 = LUPA PAYMENT ONLY                                        *00160000
001700*       TOB = 327, 329, 32F, 32G, 32H, 32I, 32J,                 *00170000
001800*             32K, 32M, 32Q, 32P                                 *00180000
001900*       WITH REVENUE CODES AND REVENUE QTYS < 5                  *00190000
002000*  09 = FINAL PAYMENT, PEP = Y, NO OUTLIER                       *00200000
002100*  11 = FINAL PAYMENT, PEP = Y, WITH OUTLIER                     *00210000
002200******************************************************************00220000
002300*                                                                *00230000
002400*            HHA-RTC   NO PAYMENTS RETURNED                      *00240000
002500*                                                                *00250000
002600*  10 = INVALID TOB                                              *00260000
002700*  15 = INVALID HRG DAYS FOR SHORTENED EPISODE                   *00270000
002800*  16 = INVALID HRG DAYS, > 30 DAYS                              *00280000
002900*  20 = INVALID PEP INDICATOR                                    *00290000
003000*  25 = INVALID MED REVIEW INDICATOR                             *00300000
003100*  30 = INVALID CBSA CODE                                        *00310000
003200*  31 = COUNTY CODE MISSING OR INVALID                           *00320000
003300*  35 = INVALID INITIAL PAYMENT INDICATOR                        *00330000
003400*       0 = MAKE NORMAL PERCENTAGE PAYMENT                       *00340000
003500*       1 = PAY 0%                                               *00350000
003600*       2 = MAKE FINAL PAYMENT REDUCED BY 2%                     *00360000
003700*       3 = MAKE FINAL PAYMENT REDUCED BY 2%,                    *00370000
003800*           PAY RAPS AT 0%                                       *00380000
003900*  40 = ADMIT-DATE > SERV-FROM-DATE                              *00390000
004000*  70 = INVALID OR NO HRG CODE PRESENT                           *00400000
004100*  75 = REMOVED IN CY2020 RELEASE                                *00410000
004200*  80 = INVALID REVENUE CODE                                     *00420000
004300*  85 = NO REVENUE CODE PRESENT                                  *00430000
004400*       TOB = 327, 329, 32F, 32G, 32H, 32I, 32J,                 *00440000
004500*             32K, 32M, 32Q, 32P                                 *00450000
004600*                                                                *00460000
004700******************************************************************00470000
004800* QRP  = QUALITY REPORTING PROGRAM                               *00480000
004900* LUPA = LOW UTILIZATION PAYMENT ADJUSTMENT                      *00490000
005000******************************************************************00500000
005100                                                                  00510000
005200 ENVIRONMENT DIVISION.                                            00520000
005300 CONFIGURATION SECTION.                                           00530000
005400 SOURCE-COMPUTER.            IBM-370.                             00540000
005500 OBJECT-COMPUTER.            IBM-370.                             00550000
005600 INPUT-OUTPUT  SECTION.                                           00560000
005700 FILE-CONTROL.                                                    00570000
005800                                                                  00580000
005900 DATA DIVISION.                                                   00590000
006000 FILE SECTION.                                                    00600000
006100                                                                  00610000
006200 WORKING-STORAGE SECTION.                                         00620000
006300 01  FILLER                         PIC X(40)  VALUE              00630000
006400     'HHCAL    - W O R K I N G   S T O R A G E'.                  00640000
006500 01  CAL-VERSION                    PIC X(07)  VALUE 'C2020.0'.   00650000
006600 01  CO1                            PIC S9(04)    VALUE 0.        00660000
006700 01  SS-QCV                         PIC S9(04)    VALUE 0.        00670000
006800 01  SS-ADJ                         PIC S9(04)    VALUE 0.        00680000
006900 01  WK-RTC-ADJ-IND                 PIC 9         VALUE 0.        00690000
007000 01  WS-STD-VALUE-NLUPA-AMT         PIC 9(7)V9(2) VALUE 0.        00700000
007100 01  WS-STD-VALUE-NLUPA-OUTL        PIC 9(7)V9(2) VALUE 0.        00710000
007200 01  WS-STDV-LUPA-ADDON-FAC         PIC 9(1)V9(4) VALUE 0.        00720000
007300 01  WS-STDV-RURAL-FAC              PIC 9V9999 VALUE 1.0024.      00730000
007400 01  WS-STDV-EPISODE-AMT            PIC 9(05)V9(02) VALUE 0.      00740000
007500*-------------------------------------------------------------*   00750000
007600*    RURAL ADD ON PERCENTAGES FOR CY2020                      *   00760000
007700*-------------------------------------------------------------*   00770000
007800 01  WS-RURAL-ADDON-A               PIC 9V999 VALUE 1.005.        00780000
007900 01  WS-RURAL-ADDON-B               PIC 9V999 VALUE 1.030.        00790000
008000 01  WS-RURAL-ADDON-C               PIC 9V999 VALUE 1.020.        00800000
008100*-------------------------------------------------------------*  R00810000
008200*    RURAL ADD ON PERCENTAGES FOR CY2020                      *   00820000
008300*    CATEGORY:                                                *   00830000
008400*       (A) HIGH UTILIZATION                                  *   00840000
008500*       (B) LOW POPULATION DENSITY                            *   00850000
008600*       (C) ALL OTHER                                         *   00860000
008700*-------------------------------------------------------------*   00870000
008800*01  WS-RURAL-ADDON-A               PIC 9V999 VALUE 1.005.        00880000
008900*01  WS-RURAL-ADDON-B               PIC 9V999 VALUE 1.030.        00890000
009000*01  WS-RURAL-ADDON-C               PIC 9V999 VALUE 1.020.        00900000
009100 01  WS-RURAL-CATEGORY              PIC X     VALUE SPACE.        00910000
009200***************************************************************   00920000
009300*   YEARCHANGE - NO CHANGE FOR 2020                           *   00930000
009400*-------------------------------------------------------------*   00940000
009500* - FROM CLAIM EXAMPLE SPREAD SHEET HIPPSPAYCALC TAB          *   00950000
009600***************************************************************   00960000
009700 01  LABOR-NLABOR-PERCENT.                                        00970000
009800     05 LABOR-PERCENT        PIC 9V9(05)  VALUE 0.76100.          00980000
009900     05 NONLABOR-PERCENT     PIC 9V9(05)  VALUE 0.23900.          00990000
010000***************************************************************   01000000
010100*   YEARCHANGE - NO CHANGE FOR 2019                           *   01010000
010200***************************************************************   01020000
010300 01  LUPA-ADD-ON-SN4              PIC 9(01)V9(04) VALUE 00.8451.  01030000
010400 01  LUPA-ADD-ON-PT1              PIC 9(01)V9(04) VALUE 00.6700.  01040000
010500 01  LUPA-ADD-ON-SLT3             PIC 9(01)V9(04) VALUE 00.6266.  01050000
010600***************************************************************** 01060000
010700*   YEARCHANGE - NO CHANGE FOR 2020                           *   01070000
010800***    EXAMPLE    *********************************************** 01080000
010900*** FED-EPISODE-RATE-AMT TIMES 1.13 = OUTLIER-THRESHOLD-AMT ***** 01090000
011000******  2327.68 TIMES 0.65  = 1512.99  ROUNDED UP  ************** 01100000
011100***************************************************************** 01110000
011200 01  OUTL-LOSS-SHAR-RATIO-PERCENT PIC 9(01)V9(02) VALUE 0.80.     01120000
011300 01  LUPA-LABOR-ADJ               PIC 9(03)V9(02) VALUE 0.        01130000
011400 01  LUPA-NON-LABOR-ADJ           PIC 9(03)V9(02) VALUE 0.        01140000
011500 01  FED-EPISODE-RATE-AMT         PIC 9(05)V9(02) VALUE 0.        01150000
011600 01  OUTLIER-THRESHOLD-AMT        PIC 9(05)V9(02) VALUE 0.        01160000
011700 01  WK-HRG-NO-OF-DAYS            PIC 9(03)       VALUE 0.        01170000
011800 01  WK-HRG-NO-OF-DAYS-FAC        PIC 9V99        VALUE 0.        01180000
011900 01  WK-ALL-TOTALS.                                               01190000
012000     05  FED-ADJ                        PIC S9(07)V9(02).         01200000
012100     05  FED-ADJP                       PIC S9(07)V9(02).         01210000
012200     05  FED-ADJ1                       PIC S9(07)V9(02).         01220000
012300     05  FED-ADJ2                       PIC S9(07)V9(02).         01230000
012400     05  FED-ADJ3                       PIC S9(07)V9(02).         01240000
012500     05  FED-ADJ4                       PIC S9(07)V9(02).         01250000
012600     05  FED-ADJ5                       PIC S9(07)V9(02).         01260000
012700     05  FED-ADJ6                       PIC S9(07)V9(02).         01270000
012800     05  FED-LUPA-ADJ1                  PIC S9(07)V9(02).         01280000
012900     05  FED-LUPA-ADJ2                  PIC S9(07)V9(02).         01290000
013000     05  FED-LUPA-ADJ3                  PIC S9(07)V9(02).         01300000
013100     05  FED-LUPA-ADJ4                  PIC S9(07)V9(02).         01310000
013200     05  FED-LUPA-ADJ5                  PIC S9(07)V9(02).         01320000
013300     05  FED-LUPA-ADJ6                  PIC S9(07)V9(02).         01330000
013400     05  FED-LABOR-ADJ                  PIC S9(07)V9(02).         01340000
013500     05  FED-LABOR-ADJP                 PIC S9(07)V9(02).         01350000
013600     05  FED-LABOR-ADJ1                 PIC S9(07)V9(02).         01360000
013700     05  FED-LABOR-ADJ2                 PIC S9(07)V9(02).         01370000
013800     05  FED-LABOR-ADJ3                 PIC S9(07)V9(02).         01380000
013900     05  FED-LABOR-ADJ4                 PIC S9(07)V9(02).         01390000
014000     05  FED-LABOR-ADJ5                 PIC S9(07)V9(02).         01400000
014100     05  FED-LABOR-ADJ6                 PIC S9(07)V9(02).         01410000
014200     05  FED-LABOR-LUPA-ADJ1            PIC S9(07)V9(02).         01420000
014300     05  FED-LABOR-LUPA-ADJ2            PIC S9(07)V9(02).         01430000
014400     05  FED-LABOR-LUPA-ADJ3            PIC S9(07)V9(02).         01440000
014500     05  FED-LABOR-LUPA-ADJ4            PIC S9(07)V9(02).         01450000
014600     05  FED-LABOR-LUPA-ADJ5            PIC S9(07)V9(02).         01460000
014700     05  FED-LABOR-LUPA-ADJ6            PIC S9(07)V9(02).         01470000
014800     05  FED-NON-LABOR-ADJ              PIC S9(07)V9(02).         01480000
014900     05  FED-NON-LABOR-ADJP             PIC S9(07)V9(02).         01490000
015000     05  FED-NON-LABOR-ADJ1             PIC S9(07)V9(02).         01500000
015100     05  FED-NON-LABOR-ADJ2             PIC S9(07)V9(02).         01510000
015200     05  FED-NON-LABOR-ADJ3             PIC S9(07)V9(02).         01520000
015300     05  FED-NON-LABOR-ADJ4             PIC S9(07)V9(02).         01530000
015400     05  FED-NON-LABOR-ADJ5             PIC S9(07)V9(02).         01540000
015500     05  FED-NON-LABOR-ADJ6             PIC S9(07)V9(02).         01550000
015600     05  FED-NON-LABOR-LUPA-ADJ1        PIC S9(07)V9(02).         01560000
015700     05  FED-NON-LABOR-LUPA-ADJ2        PIC S9(07)V9(02).         01570000
015800     05  FED-NON-LABOR-LUPA-ADJ3        PIC S9(07)V9(02).         01580000
015900     05  FED-NON-LABOR-LUPA-ADJ4        PIC S9(07)V9(02).         01590000
016000     05  FED-NON-LABOR-LUPA-ADJ5        PIC S9(07)V9(02).         01600000
016100     05  FED-NON-LABOR-LUPA-ADJ6        PIC S9(07)V9(02).         01610000
016200     05  OUT-THRES-AMT-ADJ              PIC S9(07)V9(02).         01620000
016300     05  OUT-THRES-LABOR-ADJ            PIC S9(07)V9(02).         01630000
016400     05  OUT-THRES-NON-LABOR-ADJ        PIC S9(07)V9(02).         01640000
016500     05  WK-3000-PEP-N-PRETOT-PAY       PIC S9(07)V9(02).         01650000
016600     05  WK-3000-PEP-N-PAYMENT          PIC S9(07)V9(02).         01660000
016700     05  WK-4000-PEP-Y-PRETOT-PAY       PIC S9(07)V9(02).         01670000
016800     05  WK-4000-PEP-Y-PAYMENT          PIC S9(07)V9(02).         01680000
016900     05  WK-7000-OUTLIER-VALUE-A        PIC S9(07)V9(02).         01690000
017000     05  WK-7000-AB-DIFF                PIC S9(07)V9(02).         01700000
017100     05  WK-7000-CALC                   PIC S9(07)V9(02).         01710000
017200     05  WK-8000-OUTLIER-VALUE-B        PIC S9(07)V9(02).         01720000
017300     05  WK-8000-OUTLIER-LAB-NLAB       PIC S9(07)V9(02).         01730000
017400     05  WK-9100-TOTAL-PAYMENT          PIC 9(07)V9(02).          01740000
017500     05  WK-10000-OUTLIER-POOL-DIF      PIC S9(07)V9(02).         01750000
017600     05  WK-10000-OUTLIER-POOL-PERCENT  PIC S9(09)V9(02).         01760000
017700     05  WK-10000-OUTLIER-AVAIL-POOL    PIC S9(09)V9(02).         01770000
017800                                                                  01780000
017900*----------------------------------------------------------------*01790000
018000*    STATE & COUNTY CODE RURAL ADD ON TABLE                      *01800000
018100*----------------------------------------------------------------*01810000
018200     COPY ADDONTBL.                                               01820000
018300*----------------------------------------------------------------*01830000
018400*  INPUT/OUTPUT RECORD LAYOUT - PIC X(650)                       *01840000
018500*  THIS RECORD WAS MODIFIED FOR THE 01/01/2020 RELEASE           *01850000
018600*----------------------------------------------------------------*01860000
018700 01  HOLD-HHA-DATA.                                               01870000
018800     05  H-HHA-INPUT-DATA.                                        01880000
018900         10  H-HHA-NPI               PIC X(10).                   01890000
019000         10  H-HHA-HIC               PIC X(12).                   01900000
019100         10  H-HHA-PROV-NO           PIC X(06).                   01910000
019200         10  H-HHA-INIT-PAY-QRP-IND  PIC X(01).                   01920000
019300             88  H-HHA-WITH-DATA-CHECK VALUE '0', '1'.            01930000
019400             88  H-HHA-NO-DATA-CHECK VALUE '2', '3'.              01940000
019500         10  H-HHA-PROV-VBP-ADJ-FAC  PIC 9V9(5).                  01950000
019600         10  H-HHA-PROV-OUTLIER-PAY-TOTAL PIC 9(08)V9(02).        01960000
019700         10  H-HHA-PROV-PAYMENT-TOTAL PIC 9(09)V9(02).            01970000
019800         10  H-HHA-TOB               PIC X(03).                   01980000
019900             88 H-HHA-VALID-TOB-CLAIM VALUE                       01990000
020000             '327', '329',                                        02000000
020100             '32F', '32G', '32H', '32I', '32J',                   02010000
020200             '32K', '32M', '32Q', '32P'.                          02020000
020300             88 H-HHA-VALID-TOB-RAP  VALUE                        02030000
020400             '322'.                                               02040000
020500         10  H-HHA-CBSA              PIC X(05).                   02050000
020600             88  H-HHA-CBSA-RURAL-CHECK-ALL VALUE                 02060000
020700             '50001', '50002', '50005', '50007', '50025',         02070000
020800             '50028', '50031', '50035', '50036', '50037',         02080000
020900             '50041', '50045', '50047', '50048', '50050',         02090000
021000             '50056', '50057', '50066', '50068', '50071',         02100000
021100             '50073', '50080', '50084', '50087', '50089',         02110000
021200             '50090', '50091', '50103', '50104', '50111',         02120000
021300             '50115', '50117', '50118', '50120', '50121',         02130000
021400             '50139', '50146', '50147', '50149', '50151',         02140000
021500             '50164', '50165', '50168', '50169', '50173',         02150000
021600             '50174', '50177', '50180', '50182', '50183'.         02160000
021700         10  FILLER  REDEFINES  H-HHA-CBSA.                       02170000
021800             15  H-HHA-CBSA-RURAL    PIC X(03).                   02180000
021900             88  H-HHA-CBSA-RURAL-CHECK VALUE '999'.              02190000
022000             15  FILLER             PIC X(02).                    02200000
022100         10  H-HHA-COUNTY-CODE       PIC X(05).                   02210000
022200         10  H-HHA-SERV-FROM-DATE.                                02220000
022300             15  H-HHA-FROM-CC       PIC XX.                      02230000
022400             15  H-HHA-FROM-YYMMDD.                               02240000
022500                 25  H-HHA-FROM-YY   PIC XX.                      02250000
022600                 25  H-HHA-FROM-MM   PIC XX.                      02260000
022700                 25  H-HHA-FROM-DD   PIC XX.                      02270000
022800         10  H-HHA-SERV-THRU-DATE.                                02280000
022900             15  H-HHA-THRU-CC       PIC XX.                      02290000
023000             15  H-HHA-THRU-YYMMDD.                               02300000
023100                 25  H-HHA-THRU-YY   PIC XX.                      02310000
023200                 25  H-HHA-THRU-MM   PIC XX.                      02320000
023300                 25  H-HHA-THRU-DD   PIC XX.                      02330000
023400         10  H-HHA-ADMIT-DATE.                                    02340000
023500             15  H-HHA-ADMIT-CC      PIC XX.                      02350000
023600             15  H-HHA-ADMIT-YYMMDD.                              02360000
023700                 25  H-HHA-ADMIT-YY  PIC XX.                      02370000
023800                 25  H-HHA-ADMIT-MM  PIC XX.                      02380000
023900                 25  H-HHA-ADMIT-DD  PIC XX.                      02390000
024000         10  H-HHA-LUPA-SRC-ADM         PIC X.                    02400000
024100         10  H-HHA-ADJ-IND              PIC X.                    02410000
024200         10  H-HHA-PEP-IND           PIC X.                       02420000
024300         10  H-HHA-HRG-INPUT-CODE     PIC X(05).                  02430000
024400         10  H-HHA-HRG-NO-OF-DAYS     PIC 9(03).                  02440000
024500         10  H-HHA-HRG-WGTS           PIC 9(02)V9(04).            02450000
024600         10  H-HHA-HRG-PAY            PIC 9(07)V9(02).            02460000
024700         10  H-HHA-REVENUE-DATA   OCCURS 6.                       02470000
024800             15  H-HHA-REVENUE-CODE            PIC X(04).         02480000
024900             15  H-HHA-REVENUE-QTY-COV-VISITS  PIC 9(03).         02490000
025000             15  H-HHA-REVENUE-QTY-OUTL-UNITS  PIC 9(05).         02500000
025100             15  H-HHA-REVENUE-EARLIEST-DATE   PIC 9(08).         02510000
025200             15  H-HHA-REVENUE-DOLL-RATE       PIC 9(07)V9(02).   02520000
025300             15  H-HHA-REVENUE-COST            PIC 9(07)V9(02).   02530000
025400             15  H-HHA-REVENUE-ADD-ON-VISIT-AMT PIC 9(07)V9(02).  02540000
025500         10  H-HHA-PAY-RTC              PIC 99.                   02550000
025600         10  H-HHA-REVENUE-SUM1-6-QTY-ALL PIC 9(05).              02560000
025700         10  H-HHA-OUTLIER-PAYMENT      PIC 9(07)V9(02).          02570000
025800         10  H-HHA-TOTAL-PAYMENT        PIC 9(07)V9(02).          02580000
025900         10  H-HHA-VBP-ADJ-AMT          PIC S9(7)V99.             02590000
026000         10  H-HHA-PPS-STD-VALUE        PIC 9(7)V99.              02600000
026100         10  FILLER                     PIC X(206).               02610000
026200 LINKAGE SECTION.                                                 02620000
026300*----------------------------------------------------------------*02630000
026400*  INPUT/OUTPUT RECORD LAYOUT - PIC X(650)                       *02640000
026500*  THIS RECORD WAS MODIFIED FOR THE 01/01/2020 RELEASE           *02650000
026600*----------------------------------------------------------------*02660000
026700 01  HHA-INPUT-DATA.                                              02670000
026800     05  HHA-DATA.                                                02680000
026900         10  HHA-NPI                 PIC X(10).                   02690000
027000         10  HHA-HIC                 PIC X(12).                   02700000
027100         10  HHA-PROV-NO             PIC X(06).                   02710000
027200         10  HHA-INIT-PAY-QRP-IND        PIC X(01).               02720000
027300             88  HHA-WITH-DATA-CHECK VALUE '0', '1'.              02730000
027400             88  HHA-NO-DATA-CHECK   VALUE '2', '3'.              02740000
027500         10  HHA-PROV-VBP-ADJ-FAC    PIC 9V9(5).                  02750000
027600         10  HHA-PROV-OUTLIER-PAY-TOTAL PIC 9(08)V9(02).          02760000
027700         10  HHA-PROV-PAYMENT-TOTAL  PIC 9(09)V9(02).             02770000
027800         10  HHA-TOB                 PIC X(03).                   02780000
027900             88 HHA-VALID-TOB-CLAIM  VALUE                        02790000
028000             '327', '329',                                        02800000
028100             '32F', '32G', '32H', '32I', '32J',                   02810000
028200             '32K', '32M', '32Q', '32P'.                          02820000
028300             88 HHA-VALID-TOB-RAP    VALUE                        02830000
028400             '322'.                                               02840000
028500         10  HHA-CBSA                PIC X(05).                   02850000
028600             88  HHA-CBSA-RURAL-CHECK-ALL VALUE                   02860000
028700             '50001', '50002', '50005', '50007', '50025',         02870000
028800             '50028', '50031', '50035', '50036', '50037',         02880000
028900             '50041', '50045', '50047', '50048', '50050',         02890000
029000             '50056', '50057', '50066', '50068', '50071',         02900000
029100             '50073', '50080', '50084', '50087', '50089',         02910000
029200             '50090', '50091', '50103', '50104', '50111',         02920000
029300             '50115', '50117', '50118', '50120', '50121',         02930000
029400             '50139', '50146', '50147', '50149', '50151',         02940000
029500             '50164', '50165', '50168', '50169', '50173',         02950000
029600             '50174', '50177', '50180', '50182', '50183'.         02960000
029700         10  FILLER  REDEFINES  HHA-CBSA.                         02970000
029800             15  HHA-CBSA-RURAL      PIC X(03).                   02980000
029900             88  HHA-CBSA-RURAL-CHECK  VALUE '999'.               02990000
030000             15  FILLER             PIC X(02).                    03000000
030100         10  HHA-COUNTY-CODE         PIC X(05).                   03010000
030200         10  HHA-SERV-FROM-DATE.                                  03020000
030300             15  HHA-FROM-CC         PIC XX.                      03030000
030400             15  HHA-FROM-YYMMDD.                                 03040000
030500                 25  HHA-FROM-YY     PIC XX.                      03050000
030600                 25  HHA-FROM-MM     PIC XX.                      03060000
030700                 25  HHA-FROM-DD     PIC XX.                      03070000
030800         10  HHA-SERV-THRU-DATE.                                  03080000
030900             15  HHA-THRU-CC         PIC XX.                      03090000
031000             15  HHA-THRU-YYMMDD.                                 03100000
031100                 25  HHA-THRU-YY     PIC XX.                      03110000
031200                 25  HHA-THRU-MM     PIC XX.                      03120000
031300                 25  HHA-THRU-DD     PIC XX.                      03130000
031400         10  HHA-ADMIT-DATE.                                      03140000
031500             15  HHA-ADMIT-CC        PIC XX.                      03150000
031600             15  HHA-ADMIT-YYMMDD.                                03160000
031700                 25  HHA-ADMIT-YY    PIC XX.                      03170000
031800                 25  HHA-ADMIT-MM    PIC XX.                      03180000
031900                 25  HHA-ADMIT-DD    PIC XX.                      03190000
032000         10  HHA-LUPA-SRC-ADM           PIC X.                    03200000
032100         10  HHA-ADJ-IND                PIC X.                    03210000
032200         10  HHA-PEP-IND             PIC X.                       03220000
032300         10  HHA-HRG-INPUT-CODE       PIC X(05).                  03230000
032400         10  HHA-HRG-NO-OF-DAYS       PIC 9(03).                  03240000
032500         10  HHA-HRG-WGTS             PIC 9(02)V9(04).            03250000
032600         10  HHA-HRG-PAY              PIC 9(07)V9(02).            03260000
032700         10  HHA-REVENUE-DATA     OCCURS 6.                       03270000
032800             15  HHA-REVENUE-CODE              PIC X(04).         03280000
032900             15  HHA-REVENUE-QTY-COV-VISITS    PIC 9(03).         03290000
033000             15  HHA-REVENUE-QTY-OUTL-UNITS    PIC 9(05).         03300000
033100             15  HHA-REVENUE-EARLIEST-DATE     PIC 9(08).         03310000
033200             15  HHA-REVENUE-DOLL-RATE         PIC 9(07)V9(02).   03320000
033300             15  HHA-REVENUE-COST              PIC 9(07)V9(02).   03330000
033400             15  HHA-REVENUE-ADD-ON-VISIT-AMT  PIC 9(07)V9(02).   03340000
033500         10  HHA-PAY-RTC                PIC 99.                   03350000
033600         10  HHA-REVENUE-SUM1-6-QTY-ALL PIC 9(05).                03360000
033700         10  HHA-OUTLIER-PAYMENT        PIC 9(07)V9(02).          03370000
033800         10  HHA-TOTAL-PAYMENT          PIC 9(07)V9(02).          03380000
033900         10  HHA-VBP-ADJ-AMT            PIC S9(7)V99.             03390000
034000         10  HHA-PPS-STD-VALUE          PIC 9(7)V99.              03400000
034100         10  FILLER                     PIC X(206).               03410000
034200                                                                  03420000
034300 01  HOLD-VARIABLES-DATA.                                         03430000
034400     02  HOLD-VAR-DATA.                                           03440000
034500         05  PRICER-OPTION-SW                   PIC X(01).        03450000
034600         05  HHOPN-VERSION                      PIC X(07).        03460000
034700         05  HHDRV-VERSION                      PIC X(07).        03470000
034800         05  HHCAL-VERSION                      PIC X(07).        03480000
034900         05  FILLER                             PIC X(20).        03490000
035000                                                                  03500000
035100 01  CBSA-WAGE-INDEX-DATA.                                        03510000
035200     02  HOLD-WIR-DATA.                                           03520000
035300         05  WIR-CBSA                       PIC X(05).            03530000
035400         05  WIR-CBSA-EFFDATE               PIC X(08).            03540000
035500         05  WIR-CBSA-WAGEIND               PIC 9(02)V9(04).      03550000
035600                                                                  03560000
035700 01  TB-REV-DOLL-RATE-UNITS.                                      03570000
035800     05 WK-REV-DOLL-RATE-UNITS  PIC 9(07)V9(02)                   03580000
035900                                OCCURS 6.                         03590000
036000 01  TB-STDV-DATA.                                                03600000
036100     05 TB-STDV-TABLE           OCCURS 6.                         03610000
036200        10  TB-STDV-REV-CODE           PIC X(04).                 03620000
036300        10  TB-STDV-REV-DOLL-RATE      PIC 9(07)V9(02).           03630000
036400                                                                  03640000
036500 01  L-HRG-THRESHOLD       PIC 9(01).                             03650000
036600                                                                  03660000
036700 PROCEDURE DIVISION  USING HHA-INPUT-DATA                         03670000
036800                           HOLD-VARIABLES-DATA                    03680000
036900                           CBSA-WAGE-INDEX-DATA                   03690000
037000                           TB-REV-DOLL-RATE-UNITS                 03700000
037100                           TB-STDV-DATA                           03710000
037200                           L-HRG-THRESHOLD.                       03720000
037300                                                                  03730000
037400     MOVE CAL-VERSION    TO HHCAL-VERSION.                        03740000
037500                                                                  03750000
037600     MOVE HHA-INPUT-DATA TO HOLD-HHA-DATA.                        03760000
037700                                                                  03770000
037800     INITIALIZE      WK-ALL-TOTALS.                               03780000
037900     INITIALIZE      WK-RTC-ADJ-IND.                              03790000
038000                                                                  03800000
038100     PERFORM 400-CALC-THE-HHA     THRU 400-EXIT.                  03810000
038200                                                                  03820000
038300     MOVE HOLD-HHA-DATA  TO HHA-INPUT-DATA.                       03830000
038400                                                                  03840000
038500     GOBACK.                                                      03850000
038600                                                                  03860000
038700 400-CALC-THE-HHA.                                                03870000
038800                                                                  03880000
038900     IF H-HHA-VALID-TOB-CLAIM                                     03890000
039000     OR H-HHA-VALID-TOB-RAP                                       03900000
039100         CONTINUE                                                 03910000
039200     ELSE                                                         03920000
039300         MOVE '10' TO H-HHA-PAY-RTC                               03930000
039400         GO TO 400-EXIT.                                          03940000
039500                                                                  03950000
039600     IF H-HHA-ADMIT-DATE > H-HHA-SERV-FROM-DATE                   03960000
039700         MOVE '40' TO H-HHA-PAY-RTC                               03970000
039800         GO TO 400-EXIT.                                          03980000
039900                                                                  03990000
040000     IF H-HHA-CBSA-RURAL = '999'                                  04000000
040100        PERFORM 2000-TBL-SCC-SEARCH THRU 2000-EXIT                04010000
040200        IF H-HHA-PAY-RTC = '31'                                   04020000
040300           GO TO 400-EXIT.                                        04030000
040400                                                                  04040000
040500     IF H-HHA-HRG-INPUT-CODE = SPACE                              04050000
040600         MOVE '70' TO H-HHA-PAY-RTC                               04060000
040700         GO TO 400-EXIT.                                          04070000
040800                                                                  04080000
040900     IF H-HHA-VALID-TOB-CLAIM                                     04090000
041000        IF H-HHA-REVENUE-CODE (1) = SPACE                         04100000
041100           MOVE '85' TO H-HHA-PAY-RTC                             04110000
041200           GO TO 400-EXIT.                                        04120000
041300                                                                  04130000
041400     IF H-HHA-VALID-TOB-CLAIM                                     04140000
041500        IF H-HHA-PEP-IND = 'Y'                                    04150000
041600           IF H-HHA-HRG-NO-OF-DAYS = ZEROES                       04160000
041700              MOVE '15' TO H-HHA-PAY-RTC                          04170000
041800              GO TO 400-EXIT.                                     04180000
041900                                                                  04190000
042000***************************************************************   04200000
042100*                 YEARCHANGE                                  *   04210000
042200*-------------------------------------------------------------*   04220000
042300* RATES AND THRESHOLDS ARE APPLIED FOR RURAL AND NON-RURAL    *   04230000
042400*-------------------------------------------------------------*   04240000
042500* OUTLIER-THRESHOLD-AMT = FED-EPISODE-RATE-AMT X .56          *   04250000
042600***************************************************************   04260000
042700*-------------------------------------------------------------*   04270000
042800*   STANDARDIZED PAYMENT AMOUNT                               *   04280000
042900*   ( SAVE FOR STANDARD VALUE CALCULATION )                   *   04290000
043000*-------------------------------------------------------------*   04300000
043100     MOVE 01864.03           TO  WS-STDV-EPISODE-AMT.             04310000
043200*-------------------------------------------------------------*   04320000
043300*   STANDARDIZED PAYMENT AMOUNT - QUALITY DATA                *   04330000
043400*-------------------------------------------------------------*   04340000
043500     IF HHA-WITH-DATA-CHECK                                       04350000
043600        MOVE 01864.03 TO   FED-EPISODE-RATE-AMT                   04360000
043700        MOVE 01043.86 TO   OUTLIER-THRESHOLD-AMT                  04370000
043800     END-IF.                                                      04380000
043900*-------------------------------------------------------------*   04390000
044000*   STANDARDIZED PAYMENT AMOUNT - NO QUALITY DATA             *   04400000
044100*-------------------------------------------------------------*   04410000
044200     IF HHA-NO-DATA-CHECK                                         04420000
044300        MOVE 01827.30 TO   FED-EPISODE-RATE-AMT                   04430000
044400        MOVE 01023.29 TO   OUTLIER-THRESHOLD-AMT                  04440000
044500     END-IF.                                                      04450000
044600                                                                  04460000
044700     IF WS-RURAL-CATEGORY = SPACE                                 04470000
044800        CONTINUE                                                  04480000
044900     ELSE                                                         04490000
045000     IF WS-RURAL-CATEGORY = 'A'                                   04500000
045100       COMPUTE FED-EPISODE-RATE-AMT ROUNDED =                     04510000
045200               FED-EPISODE-RATE-AMT  * WS-RURAL-ADDON-A           04520000
045300       COMPUTE OUTLIER-THRESHOLD-AMT ROUNDED =                    04530000
045400               OUTLIER-THRESHOLD-AMT * WS-RURAL-ADDON-A           04540000
045500     ELSE                                                         04550000
045600     IF WS-RURAL-CATEGORY = 'B'                                   04560000
045700       COMPUTE FED-EPISODE-RATE-AMT ROUNDED =                     04570000
045800               FED-EPISODE-RATE-AMT  * WS-RURAL-ADDON-B           04580000
045900       COMPUTE OUTLIER-THRESHOLD-AMT ROUNDED =                    04590000
046000               OUTLIER-THRESHOLD-AMT * WS-RURAL-ADDON-B           04600000
046100     ELSE                                                         04610000
046200     IF WS-RURAL-CATEGORY = 'C'                                   04620000
046300       COMPUTE FED-EPISODE-RATE-AMT ROUNDED =                     04630000
046400               FED-EPISODE-RATE-AMT  * WS-RURAL-ADDON-C           04640000
046500       COMPUTE OUTLIER-THRESHOLD-AMT ROUNDED =                    04650000
046600               OUTLIER-THRESHOLD-AMT * WS-RURAL-ADDON-C           04660000
046700     END-IF.                                                      04670000
046800                                                                  04680000
046900 PROCESS-PAYMENT.                                                 04690000
047000                                                                  04700000
047100* ADJUST REVENUE-DOLL-RATE IF RURAL                               04710000
047200     PERFORM 425-ADJ-REV-DOLL-RATE THRU 425-EXIT                  04720000
047300             VARYING SS-ADJ FROM 1 BY 1                           04730000
047400             UNTIL SS-ADJ > 6.                                    04740000
047500                                                                  04750000
047600     IF H-HHA-VALID-TOB-RAP                                       04760000
047700        PERFORM 500-INITIAL-PAYMENT THRU 500-EXIT                 04770000
047800     END-IF.                                                      04780000
047900                                                                  04790000
048000     IF H-HHA-VALID-TOB-CLAIM                                     04800000
048100        PERFORM 1000-FINAL-PAYMENT THRU 1000-EXIT                 04810000
048200     END-IF.                                                      04820000
048300                                                                  04830000
048400*----------------------------------------------------------------*04840000
048500* CALCULATE THE HHA-PPS-STD-VALUE                                *04850000
048600*----------------------------------------------------------------*04860000
048700     PERFORM 9200-CALC-STD-VALUE  THRU 9200-EXIT.                 04870000
048800*----------------------------------------------------------------*04880000
048900* CALCULATE THE HHA-VBP-ADJ-AMT                                  *04890000
049000*----------------------------------------------------------------*04900000
049100     PERFORM 9100-VBP-CALC        THRU 9100-EXIT.                 04910000
049200                                                                  04920000
049300 400-EXIT.   EXIT.                                                04930000
049400                                                                  04940000
049500 425-ADJ-REV-DOLL-RATE.                                           04950000
049600                                                                  04960000
049700     IF WS-RURAL-CATEGORY = 'A'                                   04970000
049800         COMPUTE H-HHA-REVENUE-DOLL-RATE (SS-ADJ) ROUNDED =       04980000
049900                 H-HHA-REVENUE-DOLL-RATE (SS-ADJ) *               04990000
050000                 WS-RURAL-ADDON-A                                 05000000
050100     ELSE                                                         05010000
050200     IF WS-RURAL-CATEGORY = 'B'                                   05020000
050300         COMPUTE H-HHA-REVENUE-DOLL-RATE (SS-ADJ) ROUNDED =       05030000
050400                 H-HHA-REVENUE-DOLL-RATE (SS-ADJ) *               05040000
050500                 WS-RURAL-ADDON-B                                 05050000
050600     ELSE                                                         05060000
050700     IF WS-RURAL-CATEGORY = 'C'                                   05070000
050800         COMPUTE H-HHA-REVENUE-DOLL-RATE (SS-ADJ) ROUNDED =       05080000
050900                 H-HHA-REVENUE-DOLL-RATE (SS-ADJ) *               05090000
051000                 WS-RURAL-ADDON-C                                 05100000
051100     END-IF.                                                      05110000
051200                                                                  05120000
051300 425-EXIT.    EXIT.                                               05130000
051400                                                                  05140000
051500 500-INITIAL-PAYMENT.                                             05150000
051600                                                                  05160000
051700***************************************************************   05170000
051800*            TOB = 322  INITIAL PAYMENT                           05180000
051900***************************************************************   05190000
052000                                                                  05200000
052100     IF  H-HHA-INIT-PAY-QRP-IND  = '0' OR '1' OR '2' OR '3'       05210000
052200         NEXT SENTENCE                                            05220000
052300     ELSE                                                         05230000
052400         MOVE '35' TO H-HHA-PAY-RTC                               05240000
052500         GO TO 500-EXIT.                                          05250000
052600                                                                  05260000
052700     IF  H-HHA-INIT-PAY-QRP-IND  = '1' OR '3'                     05270000
052800         MOVE '03' TO H-HHA-PAY-RTC                               05280000
052900         GO TO 500-EXIT.                                          05290000
053000                                                                  05300000
053100     COMPUTE FED-ADJ ROUNDED =                                    05310000
053200             H-HHA-HRG-WGTS  * FED-EPISODE-RATE-AMT.              05320000
053300                                                                  05330000
053400     COMPUTE FED-LABOR-ADJ ROUNDED =                              05340000
053500             WIR-CBSA-WAGEIND *                                   05350000
053600             LABOR-PERCENT *                                      05360000
053700             FED-ADJ.                                             05370000
053800                                                                  05380000
053900     COMPUTE FED-NON-LABOR-ADJ ROUNDED =                          05390000
054000              (NONLABOR-PERCENT * FED-ADJ).                       05400000
054100                                                                  05410000
054200*    IF H-HHA-SERV-FROM-DATE = H-HHA-ADMIT-DATE                   05420000
054300*       COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                     05430000
054400*      (FED-LABOR-ADJ + FED-NON-LABOR-ADJ) * .6                   05440000
054500*       MOVE H-HHA-TOTAL-PAYMENT TO H-HHA-HRG-PAY                 05450000
054600*       MOVE '05' TO H-HHA-PAY-RTC                                05460000
054700*    ELSE                                                         05470000
054800*       COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                     05480000
054900*      (FED-LABOR-ADJ + FED-NON-LABOR-ADJ) * .5                   05490000
055000*       MOVE H-HHA-TOTAL-PAYMENT TO H-HHA-HRG-PAY                 05500000
055100*       MOVE '04' TO H-HHA-PAY-RTC.                               05510000
055200                                                                  05520000
055300* FOR PDGM, ALL RAPS SHOULD BE PAID 20%                          *05530000
055400     COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                        05540000
055500             (FED-LABOR-ADJ + FED-NON-LABOR-ADJ) * .2             05550000
055600             MOVE H-HHA-TOTAL-PAYMENT TO H-HHA-HRG-PAY            05560000
055700             MOVE '04' TO H-HHA-PAY-RTC.                          05570000
055800                                                                  05580000
055900 500-EXIT.   EXIT.                                                05590000
056000                                                                  05600000
056100 1000-FINAL-PAYMENT.                                              05610000
056200                                                                  05620000
056300     COMPUTE H-HHA-REVENUE-SUM1-6-QTY-ALL ROUNDED =               05630000
056400             H-HHA-REVENUE-QTY-COV-VISITS (1) +                   05640000
056500             H-HHA-REVENUE-QTY-COV-VISITS (2) +                   05650000
056600             H-HHA-REVENUE-QTY-COV-VISITS (3) +                   05660000
056700             H-HHA-REVENUE-QTY-COV-VISITS (4) +                   05670000
056800             H-HHA-REVENUE-QTY-COV-VISITS (5) +                   05680000
056900             H-HHA-REVENUE-QTY-COV-VISITS (6).                    05690000
057000                                                                  05700000
057100     IF H-HHA-REVENUE-SUM1-6-QTY-ALL < L-HRG-THRESHOLD            05710000
057200       NEXT SENTENCE                                              05720000
057300     ELSE                                                         05730000
057400       GO TO PEP-CHECK.                                           05740000
057500                                                                  05750000
057600**   CHANGE MISSING DATES TO DEFAULT FOR EARLIEST DATE COMPARE ** 05760000
057700                                                                  05770000
057800     IF H-HHA-REVENUE-EARLIEST-DATE (1) = 0                       05780000
057900        MOVE 29990101 TO H-HHA-REVENUE-EARLIEST-DATE (1).         05790000
058000                                                                  05800000
058100     IF H-HHA-REVENUE-EARLIEST-DATE (3) = 0                       05810000
058200        MOVE 29990101 TO H-HHA-REVENUE-EARLIEST-DATE (3).         05820000
058300                                                                  05830000
058400     IF H-HHA-REVENUE-EARLIEST-DATE (4) = 0                       05840000
058500        MOVE 29990101 TO H-HHA-REVENUE-EARLIEST-DATE (4).         05850000
058600                                                                  05860000
058700*    IF REVENUE EARLIEST DATES = ALL 9'S THEN                     05870000
058800*    LUPA ADD ON DOES NOT CALCULATE                               05880000
058900                                                                  05890000
059000     IF (H-HHA-REVENUE-EARLIEST-DATE (1) = 99999999 AND           05900000
059100         H-HHA-REVENUE-EARLIEST-DATE (3) = 99999999 AND           05910000
059200         H-HHA-REVENUE-EARLIEST-DATE (4) = 99999999)              05920000
059300                                                                  05930000
059400           MOVE 0 TO  H-HHA-REVENUE-ADD-ON-VISIT-AMT (1)          05940000
059500                      H-HHA-REVENUE-ADD-ON-VISIT-AMT (2)          05950000
059600                      H-HHA-REVENUE-ADD-ON-VISIT-AMT (3)          05960000
059700                      H-HHA-REVENUE-ADD-ON-VISIT-AMT (4)          05970000
059800                      H-HHA-REVENUE-ADD-ON-VISIT-AMT (5)          05980000
059900                                                                  05990000
060000           GO TO RTC-CHECK.                                       06000000
060100                                                                  06010000
060200*    IF  REVENUE EARLIEST DATES = DEFAULT THEN                    06020000
060300*    LUPA ADD ON DOES NOT CALCULATE                               06030000
060400                                                                  06040000
060500     IF (H-HHA-REVENUE-EARLIEST-DATE (1) = 29990101 AND           06050000
060600         H-HHA-REVENUE-EARLIEST-DATE (3) = 29990101 AND           06060000
060700         H-HHA-REVENUE-EARLIEST-DATE (4) = 29990101)              06070000
060800                                                                  06080000
060900         MOVE 0 TO  H-HHA-REVENUE-ADD-ON-VISIT-AMT (1)            06090000
061000                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (2)            06100000
061100                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (3)            06110000
061200                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (4)            06120000
061300                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (5)            06130000
061400                                                                  06140000
061500           GO TO RTC-CHECK.                                       06150000
061600                                                                  06160000
061700*    IF PT OCCURS ON EARLIEST DATE THEN LUPA ADD ON APPLIES TO    06170000
061800*       PT                                                        06180000
061900                                                                  06190000
062000     IF (H-HHA-REVENUE-EARLIEST-DATE (1) <                        06200000
062100         H-HHA-REVENUE-EARLIEST-DATE (3)) AND                     06210000
062200        (H-HHA-REVENUE-EARLIEST-DATE (1) <                        06220000
062300         H-HHA-REVENUE-EARLIEST-DATE (4))                         06230000
062400        COMPUTE  H-HHA-REVENUE-ADD-ON-VISIT-AMT (1) ROUNDED =     06240000
062500           H-HHA-REVENUE-DOLL-RATE (1) * LUPA-ADD-ON-PT1          06250000
062600        MOVE LUPA-ADD-ON-PT1     TO WS-STDV-LUPA-ADDON-FAC        06260000
062700        GO TO RTC-CHECK.                                          06270000
062800                                                                  06280000
062900*    IF SLT OCCURS ON EARLIEST DATE THEN LUPA ADD ON APPLIES TO   06290000
063000*       SLT                                                       06300000
063100                                                                  06310000
063200     IF (H-HHA-REVENUE-EARLIEST-DATE (3) <                        06320000
063300         H-HHA-REVENUE-EARLIEST-DATE (1)) AND                     06330000
063400        (H-HHA-REVENUE-EARLIEST-DATE (3) <                        06340000
063500         H-HHA-REVENUE-EARLIEST-DATE (4))                         06350000
063600        COMPUTE  H-HHA-REVENUE-ADD-ON-VISIT-AMT (3) ROUNDED =     06360000
063700           H-HHA-REVENUE-DOLL-RATE (3) * LUPA-ADD-ON-SLT3         06370000
063800        MOVE LUPA-ADD-ON-SLT3    TO WS-STDV-LUPA-ADDON-FAC        06380000
063900        GO TO RTC-CHECK.                                          06390000
064000                                                                  06400000
064100*    IF SN OCCURS ON EARLIEST DATE THEN LUPA ADD ON APPLIES TO    06410000
064200*       SN                                                        06420000
064300                                                                  06430000
064400     IF (H-HHA-REVENUE-EARLIEST-DATE (4) <                        06440000
064500         H-HHA-REVENUE-EARLIEST-DATE (1)) AND                     06450000
064600        (H-HHA-REVENUE-EARLIEST-DATE (4) <                        06460000
064700         H-HHA-REVENUE-EARLIEST-DATE (3))                         06470000
064800        COMPUTE  H-HHA-REVENUE-ADD-ON-VISIT-AMT (4) ROUNDED =     06480000
064900           H-HHA-REVENUE-DOLL-RATE (4) * LUPA-ADD-ON-SN4          06490000
065000        MOVE LUPA-ADD-ON-SN4     TO WS-STDV-LUPA-ADDON-FAC        06500000
065100           GO TO RTC-CHECK.                                       06510000
065200                                                                  06520000
065300*    IF PT  EARLIEST DATE = SLT EARLIEST AND = SN EARLIEST        06530000
065400*    THEN LUPA ADD ON APPLIES TO SN                               06540000
065500*                                                                 06550000
065600     IF (H-HHA-REVENUE-EARLIEST-DATE (1) =                        06560000
065700         H-HHA-REVENUE-EARLIEST-DATE (3)) AND                     06570000
065800        (H-HHA-REVENUE-EARLIEST-DATE (1) =                        06580000
065900         H-HHA-REVENUE-EARLIEST-DATE (4))                         06590000
066000        COMPUTE  H-HHA-REVENUE-ADD-ON-VISIT-AMT (4) ROUNDED =     06600000
066100           H-HHA-REVENUE-DOLL-RATE (4) * LUPA-ADD-ON-SN4          06610000
066200        MOVE LUPA-ADD-ON-SN4     TO WS-STDV-LUPA-ADDON-FAC        06620000
066300        GO TO RTC-CHECK.                                          06630000
066400                                                                  06640000
066500*    IF PT EARLIEST DATE = SN EARLIEST                            06650000
066600*    THEN LUPA ADD ON APPLIES TO SN                               06660000
066700                                                                  06670000
066800     IF (H-HHA-REVENUE-EARLIEST-DATE (1) =                        06680000
066900         H-HHA-REVENUE-EARLIEST-DATE (4))                         06690000
067000        COMPUTE  H-HHA-REVENUE-ADD-ON-VISIT-AMT (4) ROUNDED =     06700000
067100           H-HHA-REVENUE-DOLL-RATE (4) * LUPA-ADD-ON-SN4          06710000
067200        MOVE LUPA-ADD-ON-SN4     TO WS-STDV-LUPA-ADDON-FAC        06720000
067300        GO TO RTC-CHECK.                                          06730000
067400                                                                  06740000
067500*    IF SLT EARLIEST DATE = SN EARLIEST                           06750000
067600*    THEN LUPA ADD ON APPLIES TO SN                               06760000
067700                                                                  06770000
067800     IF (H-HHA-REVENUE-EARLIEST-DATE (3) =                        06780000
067900         H-HHA-REVENUE-EARLIEST-DATE (4))                         06790000
068000        COMPUTE  H-HHA-REVENUE-ADD-ON-VISIT-AMT (4) ROUNDED =     06800000
068100           H-HHA-REVENUE-DOLL-RATE (4) * LUPA-ADD-ON-SN4          06810000
068200        MOVE LUPA-ADD-ON-SN4     TO WS-STDV-LUPA-ADDON-FAC        06820000
068300        GO TO RTC-CHECK.                                          06830000
068400                                                                  06840000
068500*    IF PT  EARLIEST DATE = SLT EARLIEST                          06850000
068600*    THEN LUPA ADD ON APPLIES TO PT                               06860000
068700                                                                  06870000
068800     IF (H-HHA-REVENUE-EARLIEST-DATE (1) =                        06880000
068900         H-HHA-REVENUE-EARLIEST-DATE (3))                         06890000
069000        COMPUTE  H-HHA-REVENUE-ADD-ON-VISIT-AMT (1) ROUNDED =     06900000
069100           H-HHA-REVENUE-DOLL-RATE (1) * LUPA-ADD-ON-PT1          06910000
069200        MOVE LUPA-ADD-ON-PT1     TO WS-STDV-LUPA-ADDON-FAC        06920000
069300        GO TO RTC-CHECK.                                          06930000
069400                                                                  06940000
069500 RTC-CHECK.                                                       06950000
069600************************************************************      06960000
069700* ZERO OUT LUPA ADD-ON PAYMENT WHEN CERTAIN CONDITIONS MET *      06970000
069800************************************************************      06980000
069900                                                                  06990000
070000     IF H-HHA-ADMIT-DATE NOT = H-HHA-SERV-FROM-DATE               07000000
070100         MOVE 0 TO  H-HHA-REVENUE-ADD-ON-VISIT-AMT (1)            07010000
070200                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (2)            07020000
070300                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (3)            07030000
070400                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (4)            07040000
070500                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (5).           07050000
070600                                                                  07060000
070700     IF H-HHA-LUPA-SRC-ADM = 'B'                                  07070000
070800         MOVE 0 TO  H-HHA-REVENUE-ADD-ON-VISIT-AMT (1)            07080000
070900                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (2)            07090000
071000                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (3)            07100000
071100                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (4)            07110000
071200                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (5).           07120000
071300                                                                  07130000
071400     IF H-HHA-ADJ-IND = '2'                                       07140000
071500         MOVE 0 TO  H-HHA-REVENUE-ADD-ON-VISIT-AMT (1)            07150000
071600                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (2)            07160000
071700                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (3)            07170000
071800                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (4)            07180000
071900                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (5).           07190000
072000                                                                  07200000
072100     IF H-HHA-REVENUE-SUM1-6-QTY-ALL = 0                          07210000
072200         MOVE 0 TO  H-HHA-REVENUE-ADD-ON-VISIT-AMT (1)            07220000
072300                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (2)            07230000
072400                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (3)            07240000
072500                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (4)            07250000
072600                    H-HHA-REVENUE-ADD-ON-VISIT-AMT (5).           07260000
072700                                                                  07270000
072800     PERFORM 1050-LUPA THRU 1050-EXIT.                            07280000
072900                                                                  07290000
073000     IF  H-HHA-REVENUE-ADD-ON-VISIT-AMT (1) > 0                   07300000
073100     OR  H-HHA-REVENUE-ADD-ON-VISIT-AMT (2) > 0                   07310000
073200     OR  H-HHA-REVENUE-ADD-ON-VISIT-AMT (3) > 0                   07320000
073300     OR  H-HHA-REVENUE-ADD-ON-VISIT-AMT (4) > 0                   07330000
073400     OR  H-HHA-REVENUE-ADD-ON-VISIT-AMT (5) > 0                   07340000
073500         MOVE '14' TO H-HHA-PAY-RTC                               07350000
073600     ELSE                                                         07360000
073700         MOVE '06' TO H-HHA-PAY-RTC                               07370000
073800     END-IF.                                                      07380000
073900                                                                  07390000
074000**   CHANGE DATES WITH DEFAULT BACK TO ZERO FOR PASSBACK       ** 07400000
074100                                                                  07410000
074200     IF H-HHA-REVENUE-EARLIEST-DATE (1) = 29990101                07420000
074300        MOVE 0 TO H-HHA-REVENUE-EARLIEST-DATE (1).                07430000
074400                                                                  07440000
074500     IF H-HHA-REVENUE-EARLIEST-DATE (3) = 29990101                07450000
074600        MOVE 0 TO H-HHA-REVENUE-EARLIEST-DATE (3).                07460000
074700                                                                  07470000
074800     IF H-HHA-REVENUE-EARLIEST-DATE (4) = 29990101                07480000
074900        MOVE 0 TO H-HHA-REVENUE-EARLIEST-DATE (4).                07490000
075000                                                                  07500000
075100     COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                        07510000
075200             H-HHA-REVENUE-COST (1) +                             07520000
075300             H-HHA-REVENUE-ADD-ON-VISIT-AMT (1) +                 07530000
075400             H-HHA-REVENUE-COST (2) +                             07540000
075500             H-HHA-REVENUE-ADD-ON-VISIT-AMT (2) +                 07550000
075600             H-HHA-REVENUE-COST (3) +                             07560000
075700             H-HHA-REVENUE-ADD-ON-VISIT-AMT (3) +                 07570000
075800             H-HHA-REVENUE-COST (4) +                             07580000
075900             H-HHA-REVENUE-ADD-ON-VISIT-AMT (4) +                 07590000
076000             H-HHA-REVENUE-COST (5) +                             07600000
076100             H-HHA-REVENUE-ADD-ON-VISIT-AMT (5) +                 07610000
076200             H-HHA-REVENUE-COST (6) +                             07620000
076300             H-HHA-REVENUE-ADD-ON-VISIT-AMT (6).                  07630000
076400                                                                  07640000
076500     GO TO 1000-EXIT.                                             07650000
076600                                                                  07660000
076700******************************************************************07670000
076800* PEP - PARTIAL EPISODE PAYMENT - Y/N                            *07680000
076900******************************************************************07690000
077000 PEP-CHECK.                                                       07700000
077100                                                                  07710000
077200     IF H-HHA-PEP-IND = 'Y' OR 'N'                                07720000
077300        CONTINUE                                                  07730000
077400     ELSE                                                         07740000
077500        MOVE '20' TO H-HHA-PAY-RTC                                07750000
077600        GO TO 1000-EXIT.                                          07760000
077700                                                                  07770000
077800     IF H-HHA-HRG-NO-OF-DAYS > 30                                 07780000
077900        MOVE '16' TO H-HHA-PAY-RTC                                07790000
078000        GO TO 1000-EXIT.                                          07800000
078100                                                                  07810000
078200*----------------------------------------------------------------*07820000
078300*       HRG  PAYMENT                                             *07830000
078400*----------------------------------------------------------------*07840000
078500                                                                  07850000
078600     IF H-HHA-PEP-IND = 'N'                                       07860000
078700        PERFORM 3000-PEP-N-ADJUST THRU 3000-EXIT                  07870000
078800        PERFORM 7000-OUTLIER-PAYMENT THRU 7000-EXIT.              07880000
078900                                                                  07890000
079000     IF H-HHA-PEP-IND = 'Y'                                       07900000
079100        PERFORM 4000-PEP-Y-ADJUST THRU 4000-EXIT                  07910000
079200        PERFORM 7000-OUTLIER-PAYMENT THRU 7000-EXIT.              07920000
079300                                                                  07930000
079400 1000-EXIT.  EXIT.                                                07940000
079500                                                                  07950000
079600 1050-LUPA.                                                       07960000
079700***************************************************************   07970000
079800*                    LUPA PAYMENT                                 07980000
079900***************************************************************   07990000
080000                                                                  08000000
080100     COMPUTE FED-ADJ1 ROUNDED =                                   08010000
080200            (H-HHA-REVENUE-QTY-COV-VISITS (1) *                   08020000
080300             H-HHA-REVENUE-DOLL-RATE (1)).                        08030000
080400                                                                  08040000
080500     COMPUTE FED-LUPA-ADJ1 ROUNDED =                              08050000
080600             H-HHA-REVENUE-ADD-ON-VISIT-AMT (1).                  08060000
080700                                                                  08070000
080800     COMPUTE FED-LABOR-ADJ1 ROUNDED =                             08080000
080900             WIR-CBSA-WAGEIND *                                   08090000
081000             LABOR-PERCENT *                                      08100000
081100             FED-ADJ1.                                            08110000
081200                                                                  08120000
081300     COMPUTE FED-LABOR-LUPA-ADJ1 ROUNDED =                        08130000
081400             WIR-CBSA-WAGEIND *                                   08140000
081500             LABOR-PERCENT *                                      08150000
081600             FED-LUPA-ADJ1.                                       08160000
081700                                                                  08170000
081800     COMPUTE FED-NON-LABOR-ADJ1 ROUNDED =                         08180000
081900             NONLABOR-PERCENT *                                   08190000
082000             FED-ADJ1.                                            08200000
082100                                                                  08210000
082200     COMPUTE FED-NON-LABOR-LUPA-ADJ1 ROUNDED =                    08220000
082300             NONLABOR-PERCENT *                                   08230000
082400             FED-LUPA-ADJ1.                                       08240000
082500                                                                  08250000
082600     COMPUTE H-HHA-REVENUE-COST (1) ROUNDED =                     08260000
082700             (FED-LABOR-ADJ1 + FED-NON-LABOR-ADJ1).               08270000
082800     COMPUTE H-HHA-REVENUE-ADD-ON-VISIT-AMT (1) ROUNDED =         08280000
082900             (FED-LABOR-LUPA-ADJ1 + FED-NON-LABOR-LUPA-ADJ1).     08290000
083000                                                                  08300000
083100     COMPUTE FED-ADJ2 ROUNDED =                                   08310000
083200            (H-HHA-REVENUE-QTY-COV-VISITS (2) *                   08320000
083300             H-HHA-REVENUE-DOLL-RATE (2)).                        08330000
083400                                                                  08340000
083500     COMPUTE FED-LABOR-ADJ2 ROUNDED =                             08350000
083600             WIR-CBSA-WAGEIND *                                   08360000
083700             LABOR-PERCENT *                                      08370000
083800             FED-ADJ2.                                            08380000
083900                                                                  08390000
084000     COMPUTE FED-NON-LABOR-ADJ2 ROUNDED =                         08400000
084100             NONLABOR-PERCENT *                                   08410000
084200             FED-ADJ2.                                            08420000
084300                                                                  08430000
084400     COMPUTE H-HHA-REVENUE-COST (2) ROUNDED =                     08440000
084500             (FED-LABOR-ADJ2 + FED-NON-LABOR-ADJ2).               08450000
084600                                                                  08460000
084700     COMPUTE FED-ADJ3 ROUNDED =                                   08470000
084800            (H-HHA-REVENUE-QTY-COV-VISITS (3) *                   08480000
084900             H-HHA-REVENUE-DOLL-RATE (3)).                        08490000
085000                                                                  08500000
085100     COMPUTE FED-LUPA-ADJ3 ROUNDED =                              08510000
085200             H-HHA-REVENUE-ADD-ON-VISIT-AMT (3).                  08520000
085300                                                                  08530000
085400     COMPUTE FED-LABOR-ADJ3 ROUNDED =                             08540000
085500             WIR-CBSA-WAGEIND *                                   08550000
085600             LABOR-PERCENT *                                      08560000
085700             FED-ADJ3.                                            08570000
085800                                                                  08580000
085900     COMPUTE FED-LABOR-LUPA-ADJ3 ROUNDED =                        08590000
086000             WIR-CBSA-WAGEIND *                                   08600000
086100             LABOR-PERCENT *                                      08610000
086200             FED-LUPA-ADJ3.                                       08620000
086300                                                                  08630000
086400     COMPUTE FED-NON-LABOR-ADJ3 ROUNDED =                         08640000
086500             NONLABOR-PERCENT *                                   08650000
086600             FED-ADJ3.                                            08660000
086700                                                                  08670000
086800     COMPUTE FED-NON-LABOR-LUPA-ADJ3 ROUNDED =                    08680000
086900             NONLABOR-PERCENT *                                   08690000
087000             FED-LUPA-ADJ3.                                       08700000
087100                                                                  08710000
087200     COMPUTE H-HHA-REVENUE-COST (3) ROUNDED =                     08720000
087300             (FED-LABOR-ADJ3 + FED-NON-LABOR-ADJ3).               08730000
087400                                                                  08740000
087500     COMPUTE H-HHA-REVENUE-ADD-ON-VISIT-AMT (3) ROUNDED =         08750000
087600             (FED-LABOR-LUPA-ADJ3 + FED-NON-LABOR-LUPA-ADJ3).     08760000
087700                                                                  08770000
087800     COMPUTE FED-ADJ4 ROUNDED =                                   08780000
087900            (H-HHA-REVENUE-QTY-COV-VISITS (4) *                   08790000
088000             H-HHA-REVENUE-DOLL-RATE (4)).                        08800000
088100                                                                  08810000
088200     COMPUTE FED-LUPA-ADJ4 ROUNDED =                              08820000
088300             H-HHA-REVENUE-ADD-ON-VISIT-AMT (4).                  08830000
088400                                                                  08840000
088500     COMPUTE FED-LABOR-ADJ4 ROUNDED =                             08850000
088600             WIR-CBSA-WAGEIND *                                   08860000
088700             LABOR-PERCENT *                                      08870000
088800             FED-ADJ4.                                            08880000
088900                                                                  08890000
089000     COMPUTE FED-LABOR-LUPA-ADJ4 ROUNDED =                        08900000
089100             WIR-CBSA-WAGEIND *                                   08910000
089200             LABOR-PERCENT *                                      08920000
089300             FED-LUPA-ADJ4.                                       08930000
089400                                                                  08940000
089500     COMPUTE FED-NON-LABOR-ADJ4 ROUNDED =                         08950000
089600             NONLABOR-PERCENT *                                   08960000
089700             FED-ADJ4.                                            08970000
089800                                                                  08980000
089900     COMPUTE FED-NON-LABOR-LUPA-ADJ4 ROUNDED =                    08990000
090000             NONLABOR-PERCENT *                                   09000000
090100             FED-LUPA-ADJ4.                                       09010000
090200                                                                  09020000
090300     COMPUTE H-HHA-REVENUE-COST (4) ROUNDED =                     09030000
090400             (FED-LABOR-ADJ4 + FED-NON-LABOR-ADJ4).               09040000
090500                                                                  09050000
090600     COMPUTE H-HHA-REVENUE-ADD-ON-VISIT-AMT (4) ROUNDED =         09060000
090700             (FED-LABOR-LUPA-ADJ4 + FED-NON-LABOR-LUPA-ADJ4).     09070000
090800                                                                  09080000
090900     COMPUTE FED-ADJ5 ROUNDED =                                   09090000
091000            (H-HHA-REVENUE-QTY-COV-VISITS (5) *                   09100000
091100             H-HHA-REVENUE-DOLL-RATE (5)).                        09110000
091200                                                                  09120000
091300     COMPUTE FED-LABOR-ADJ5 ROUNDED =                             09130000
091400             WIR-CBSA-WAGEIND *                                   09140000
091500             LABOR-PERCENT *                                      09150000
091600             FED-ADJ5.                                            09160000
091700                                                                  09170000
091800                                                                  09180000
091900     COMPUTE FED-NON-LABOR-ADJ5 ROUNDED =                         09190000
092000             NONLABOR-PERCENT *                                   09200000
092100             FED-ADJ5.                                            09210000
092200                                                                  09220000
092300     COMPUTE H-HHA-REVENUE-COST (5) ROUNDED =                     09230000
092400             (FED-LABOR-ADJ5 + FED-NON-LABOR-ADJ5).               09240000
092500                                                                  09250000
092600     COMPUTE FED-ADJ6 ROUNDED =                                   09260000
092700            (H-HHA-REVENUE-QTY-COV-VISITS (6) *                   09270000
092800             H-HHA-REVENUE-DOLL-RATE (6)).                        09280000
092900                                                                  09290000
093000     COMPUTE FED-LABOR-ADJ6 ROUNDED =                             09300000
093100             WIR-CBSA-WAGEIND *                                   09310000
093200             LABOR-PERCENT *                                      09320000
093300             FED-ADJ6.                                            09330000
093400                                                                  09340000
093500                                                                  09350000
093600     COMPUTE FED-NON-LABOR-ADJ6 ROUNDED =                         09360000
093700             NONLABOR-PERCENT *                                   09370000
093800             FED-ADJ6.                                            09380000
093900                                                                  09390000
094000     COMPUTE H-HHA-REVENUE-COST (6) ROUNDED =                     09400000
094100             (FED-LABOR-ADJ6 + FED-NON-LABOR-ADJ6).               09410000
094200                                                                  09420000
094300 1050-EXIT.   EXIT.                                               09430000
094400                                                                  09440000
094500***************************************************************   09450000
094600*           STATE & COUNTY CODE TABLE SEARCH                      09460000
094700***************************************************************   09470000
094800 2000-TBL-SCC-SEARCH.                                             09480000
094900                                                                  09490000
095000     MOVE SPACE TO  WS-RURAL-CATEGORY.                            09500000
095100                                                                  09510000
095200     IF HHA-COUNTY-CODE = SPACES                                  09520000
095300        MOVE '31' TO H-HHA-PAY-RTC                                09530000
095400        GO TO 2000-EXIT                                           09540000
095500     END-IF.                                                      09550000
095600                                                                  09560000
095700     SEARCH ALL T-SCC-DATA                                        09570000
095800         AT END                                                   09580000
095900            MOVE '31' TO H-HHA-PAY-RTC                            09590000
096000            GO TO 2000-EXIT                                       09600000
096100         WHEN T-SCC-CODE (IX-SCC) = HHA-COUNTY-CODE               09610000
096200            MOVE T-SCC-CATEGORY (IX-SCC) TO WS-RURAL-CATEGORY     09620000
096300            MOVE '00' TO H-HHA-PAY-RTC                            09630000
096400     END-SEARCH.                                                  09640000
096500                                                                  09650000
096600 2000-EXIT.   EXIT.                                               09660000
096700                                                                  09670000
096800******************************************************************09680000
096900* PEP - PARTIAL EPISODE PAYMENT - NO                             *09690000
097000******************************************************************09700000
097100 3000-PEP-N-ADJUST.                                               09710000
097200                                                                  09720000
097300     MOVE H-HHA-HRG-NO-OF-DAYS  TO WK-HRG-NO-OF-DAYS.             09730000
097400                                                                  09740000
097500     COMPUTE FED-ADJ ROUNDED =                                    09750000
097600             H-HHA-HRG-WGTS  * FED-EPISODE-RATE-AMT.              09760000
097700                                                                  09770000
097800     COMPUTE FED-LABOR-ADJ ROUNDED =                              09780000
097900              (WIR-CBSA-WAGEIND *                                 09790000
098000               LABOR-PERCENT * FED-ADJ).                          09800000
098100                                                                  09810000
098200     COMPUTE FED-NON-LABOR-ADJ ROUNDED =                          09820000
098300              (NONLABOR-PERCENT * FED-ADJ).                       09830000
098400                                                                  09840000
098500     COMPUTE WK-3000-PEP-N-PAYMENT ROUNDED =                      09850000
098600          (FED-LABOR-ADJ + FED-NON-LABOR-ADJ).                    09860000
098700                                                                  09870000
098800     COMPUTE H-HHA-HRG-PAY ROUNDED =                              09880000
098900             WK-3000-PEP-N-PAYMENT.                               09890000
099000                                                                  09900000
099100     COMPUTE WK-3000-PEP-N-PRETOT-PAY ROUNDED =                   09910000
099200             WK-3000-PEP-N-PRETOT-PAY + WK-3000-PEP-N-PAYMENT.    09920000
099300                                                                  09930000
099400 3000-EXIT.   EXIT.                                               09940000
099500                                                                  09950000
099600******************************************************************09960000
099700* PEP - PARTIAL EPISODE PAYMENT - YES                            *09970000
099800******************************************************************09980000
099900 4000-PEP-Y-ADJUST.                                               09990000
100000                                                                  10000000
100100     MOVE 2 TO WK-RTC-ADJ-IND.                                    10010000
100200     MOVE H-HHA-HRG-NO-OF-DAYS TO WK-HRG-NO-OF-DAYS.              10020000
100300                                                                  10030000
100400     COMPUTE FED-ADJP ROUNDED =                                   10040000
100500             H-HHA-HRG-WGTS   * FED-EPISODE-RATE-AMT.             10050000
100600                                                                  10060000
100700     COMPUTE FED-LABOR-ADJP ROUNDED =                             10070000
100800               WIR-CBSA-WAGEIND *                                 10080000
100900               LABOR-PERCENT * FED-ADJP.                          10090000
101000                                                                  10100000
101100     COMPUTE FED-NON-LABOR-ADJP ROUNDED =                         10110000
101200               NONLABOR-PERCENT * FED-ADJP.                       10120000
101300                                                                  10130000
101400     COMPUTE WK-4000-PEP-Y-PAYMENT ROUNDED =                      10140000
101500         (FED-LABOR-ADJP + FED-NON-LABOR-ADJP).                   10150000
101600                                                                  10160000
101700     COMPUTE WK-HRG-NO-OF-DAYS-FAC ROUNDED =                      10170000
101800               (WK-HRG-NO-OF-DAYS / 30).                          10180000
101900                                                                  10190000
102000     COMPUTE WK-4000-PEP-Y-PAYMENT ROUNDED =                      10200000
102100             WK-4000-PEP-Y-PAYMENT *                              10210000
102200             WK-HRG-NO-OF-DAYS-FAC.                               10220000
102300                                                                  10230000
102400     COMPUTE H-HHA-HRG-PAY  ROUNDED =                             10240000
102500             WK-4000-PEP-Y-PAYMENT.                               10250000
102600                                                                  10260000
102700     COMPUTE WK-4000-PEP-Y-PRETOT-PAY ROUNDED =                   10270000
102800             WK-4000-PEP-Y-PRETOT-PAY + WK-4000-PEP-Y-PAYMENT.    10280000
102900                                                                  10290000
103000 4000-EXIT.   EXIT.                                               10300000
103100                                                                  10310000
103200***************************************************************   10320000
103300*                    OUTLIER PAYMENT                              10330000
103400***************************************************************   10340000
103500 7000-OUTLIER-PAYMENT.                                            10350000
103600                                                                  10360000
103700     COMPUTE OUT-THRES-LABOR-ADJ ROUNDED =                        10370000
103800               WIR-CBSA-WAGEIND *                                 10380000
103900               LABOR-PERCENT * OUTLIER-THRESHOLD-AMT.             10390000
104000                                                                  10400000
104100     COMPUTE OUT-THRES-NON-LABOR-ADJ ROUNDED =                    10410000
104200               NONLABOR-PERCENT * OUTLIER-THRESHOLD-AMT.          10420000
104300                                                                  10430000
104400     COMPUTE OUT-THRES-AMT-ADJ ROUNDED  =                         10440000
104500             (OUT-THRES-LABOR-ADJ +                               10450000
104600              OUT-THRES-NON-LABOR-ADJ).                           10460000
104700                                                                  10470000
104800      COMPUTE WK-7000-OUTLIER-VALUE-A ROUNDED =                   10480000
104900              OUT-THRES-AMT-ADJ +                                 10490000
105000             WK-3000-PEP-N-PRETOT-PAY +                           10500000
105100             WK-4000-PEP-Y-PRETOT-PAY.                            10510000
105200                                                                  10520000
105300      PERFORM 8000-ADD-REV-DOLL THRU 8000-EXIT                    10530000
105400                  VARYING CO1 FROM 1 BY 1 UNTIL                   10540000
105500                   CO1 > 6.                                       10550000
105600                                                                  10560000
105700      COMPUTE WK-7000-AB-DIFF ROUNDED =                           10570000
105800              WK-8000-OUTLIER-VALUE-B - WK-7000-OUTLIER-VALUE-A.  10580000
105900****===================                                           10590000
106000      IF WK-7000-AB-DIFF > ZERO                                   10600000
106100         COMPUTE WK-7000-CALC ROUNDED =                           10610000
106200               OUTL-LOSS-SHAR-RATIO-PERCENT * WK-7000-AB-DIFF     10620000
106300                                                                  10630000
106400*** ================== NEW OUTLIER CAP HERE ========              10640000
106500         PERFORM 10000-OUTLIER-CAP-CALC THRU 10000-EXIT           10650000
106600*** ================== NEW OUTLIER CAP HERE ========              10660000
106700                                                                  10670000
106800****===================                                           10680000
106900         COMPUTE H-HHA-OUTLIER-PAYMENT ROUNDED =                  10690000
107000               WK-7000-CALC                                       10700000
107100                                                                  10710000
107200****===================                                           10720000
107300         COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                    10730000
107400                (WK-7000-CALC +                                   10740000
107500                 WK-3000-PEP-N-PRETOT-PAY +                       10750000
107600                 WK-4000-PEP-Y-PRETOT-PAY )                       10760000
107700         PERFORM 9000-WHICH-RTC-OUTLIER THRU 9000-EXIT            10770000
107800      ELSE                                                        10780000
107900         COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                    10790000
108000                (WK-3000-PEP-N-PRETOT-PAY +                       10800000
108100                 WK-4000-PEP-Y-PRETOT-PAY)                        10810000
108200         PERFORM 9050-WHICH-RTC-NO-OUTLIER THRU 9050-EXIT.        10820000
108300                                                                  10830000
108400 7000-EXIT.   EXIT.                                               10840000
108500                                                                  10850000
108600 8000-ADD-REV-DOLL.                                               10860000
108700                                                                  10870000
108800***************************************************************   10880000
108900*        ADD ALL REVENUE DOLLARS                                  10890000
109000***************************************************************   10900000
109100     IF H-HHA-REVENUE-CODE (CO1) = SPACES                         10910000
109200        MOVE 6 TO CO1                                             10920000
109300        GO TO 8000-EXIT.                                          10930000
109400                                                                  10940000
109500     PERFORM 8100-ADJ-REV-DOLL    THRU 8100-EXIT.                 10950000
109600                                                                  10960000
109700     COMPUTE FED-ADJ ROUNDED =                                    10970000
109800             WK-REV-DOLL-RATE-UNITS (CO1) *                       10980000
109900             H-HHA-REVENUE-QTY-OUTL-UNITS (CO1).                  10990000
110000                                                                  11000000
110100     COMPUTE FED-LABOR-ADJ ROUNDED =                              11010000
110200               WIR-CBSA-WAGEIND *                                 11020000
110300               LABOR-PERCENT * FED-ADJ.                           11030000
110400                                                                  11040000
110500     COMPUTE FED-NON-LABOR-ADJ ROUNDED =                          11050000
110600               NONLABOR-PERCENT * FED-ADJ.                        11060000
110700                                                                  11070000
110800     COMPUTE WK-8000-OUTLIER-LAB-NLAB ROUNDED =                   11080000
110900           (FED-LABOR-ADJ + FED-NON-LABOR-ADJ).                   11090000
111000                                                                  11100000
111100     COMPUTE H-HHA-REVENUE-COST (CO1) ROUNDED =                   11110000
111200               WK-8000-OUTLIER-LAB-NLAB.                          11120000
111300                                                                  11130000
111400     COMPUTE WK-8000-OUTLIER-VALUE-B ROUNDED =                    11140000
111500             WK-8000-OUTLIER-VALUE-B + WK-8000-OUTLIER-LAB-NLAB.  11150000
111600                                                                  11160000
111700 8000-EXIT.   EXIT.                                               11170000
111800                                                                  11180000
111900*----------------------------------------------------------------*11190000
112000* ADJUST DOLLAR RATE USING THE RURAL ADD ON FACTOR               *11200000
112100*----------------------------------------------------------------*11210000
112200 8100-ADJ-REV-DOLL.                                               11220000
112300                                                                  11230000
112400     IF WS-RURAL-CATEGORY = 'A'                                   11240000
112500         COMPUTE WK-REV-DOLL-RATE-UNITS  (CO1) ROUNDED =          11250000
112600                 WK-REV-DOLL-RATE-UNITS  (CO1) *                  11260000
112700                 WS-RURAL-ADDON-A                                 11270000
112800     END-IF.                                                      11280000
112900     IF WS-RURAL-CATEGORY = 'B'                                   11290000
113000         COMPUTE WK-REV-DOLL-RATE-UNITS  (CO1) ROUNDED =          11300000
113100                 WK-REV-DOLL-RATE-UNITS  (CO1) *                  11310000
113200                 WS-RURAL-ADDON-B                                 11320000
113300     END-IF.                                                      11330000
113400     IF WS-RURAL-CATEGORY = 'C'                                   11340000
113500         COMPUTE WK-REV-DOLL-RATE-UNITS  (CO1) ROUNDED =          11350000
113600                 WK-REV-DOLL-RATE-UNITS  (CO1) *                  11360000
113700                 WS-RURAL-ADDON-C                                 11370000
113800     END-IF.                                                      11380000
113900                                                                  11390000
114000 8100-EXIT.   EXIT.                                               11400000
114100                                                                  11410000
114200 9000-WHICH-RTC-OUTLIER.                                          11420000
114300                                                                  11430000
114400     MOVE '01' TO H-HHA-PAY-RTC.                                  11440000
114500                                                                  11450000
114600     IF WK-RTC-ADJ-IND = 2  MOVE '11' TO H-HHA-PAY-RTC.           11460000
114700     IF WK-RTC-ADJ-IND = 4  MOVE '02' TO H-HHA-PAY-RTC.           11470000
114800                                                                  11480000
114900 9000-EXIT.   EXIT.                                               11490000
115000                                                                  11500000
115100 9050-WHICH-RTC-NO-OUTLIER.                                       11510000
115200                                                                  11520000
115300     MOVE '00' TO H-HHA-PAY-RTC.                                  11530000
115400                                                                  11540000
115500     IF WK-RTC-ADJ-IND = 2  MOVE '09' TO H-HHA-PAY-RTC.           11550000
115600                                                                  11560000
115700 9050-EXIT.   EXIT.                                               11570000
115800                                                                  11580000
115900******************************************************************11590000
116000* CALCULATES THE VALUE BASED PURCHASING ADJUSTMENT AMOUNT        *11600000
116100******************************************************************11610000
116200 9100-VBP-CALC.                                                   11620000
116300                                                                  11630000
116400     INITIALIZE H-HHA-VBP-ADJ-AMT.                                11640000
116500     MOVE H-HHA-TOTAL-PAYMENT TO WK-9100-TOTAL-PAYMENT.           11650000
116600     MOVE 0                   TO H-HHA-TOTAL-PAYMENT.             11660000
116700                                                                  11670000
116800     IF H-HHA-HRG-PAY > 0                                         11680000
116900       COMPUTE H-HHA-HRG-PAY ROUNDED =                            11690000
117000               H-HHA-HRG-PAY * H-HHA-PROV-VBP-ADJ-FAC             11700000
117100       END-COMPUTE                                                11710000
117200       COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                      11720000
117300               H-HHA-TOTAL-PAYMENT + H-HHA-HRG-PAY                11730000
117400       END-COMPUTE                                                11740000
117500     END-IF.                                                      11750000
117600                                                                  11760000
117700     IF H-HHA-HRG-PAY = 0                                         11770000
117800        PERFORM 9120-VBP-REV-COST  THRU 9120-EXIT                 11780000
117900     END-IF.                                                      11790000
118000                                                                  11800000
118100     COMPUTE H-HHA-OUTLIER-PAYMENT ROUNDED =                      11810000
118200             H-HHA-OUTLIER-PAYMENT * H-HHA-PROV-VBP-ADJ-FAC       11820000
118300     END-COMPUTE.                                                 11830000
118400                                                                  11840000
118500     COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                        11850000
118600             H-HHA-TOTAL-PAYMENT + H-HHA-OUTLIER-PAYMENT          11860000
118700     END-COMPUTE.                                                 11870000
118800                                                                  11880000
118900     COMPUTE H-HHA-VBP-ADJ-AMT ROUNDED =                          11890000
119000             H-HHA-TOTAL-PAYMENT - WK-9100-TOTAL-PAYMENT          11900000
119100     END-COMPUTE.                                                 11910000
119200                                                                  11920000
119300 9100-EXIT.   EXIT.                                               11930000
119400                                                                  11940000
119500 9120-VBP-REV-COST.                                               11950000
119600                                                                  11960000
119700     COMPUTE H-HHA-REVENUE-COST (1) ROUNDED =                     11970000
119800             H-HHA-REVENUE-COST (1) * H-HHA-PROV-VBP-ADJ-FAC      11980000
119900     END-COMPUTE.                                                 11990000
120000                                                                  12000000
120100     COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                        12010000
120200             H-HHA-TOTAL-PAYMENT + H-HHA-REVENUE-COST (1)         12020000
120300     END-COMPUTE.                                                 12030000
120400                                                                  12040000
120500     COMPUTE H-HHA-REVENUE-ADD-ON-VISIT-AMT (1) ROUNDED =         12050000
120600             H-HHA-REVENUE-ADD-ON-VISIT-AMT (1) *                 12060000
120700             H-HHA-PROV-VBP-ADJ-FAC                               12070000
120800     END-COMPUTE.                                                 12080000
120900                                                                  12090000
121000     COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                        12100000
121100             H-HHA-TOTAL-PAYMENT +                                12110000
121200             H-HHA-REVENUE-ADD-ON-VISIT-AMT (1)                   12120000
121300     END-COMPUTE.                                                 12130000
121400                                                                  12140000
121500     COMPUTE H-HHA-REVENUE-COST (2) ROUNDED =                     12150000
121600             H-HHA-REVENUE-COST (2) * H-HHA-PROV-VBP-ADJ-FAC      12160000
121700     END-COMPUTE.                                                 12170000
121800                                                                  12180000
121900     COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                        12190000
122000             H-HHA-TOTAL-PAYMENT + H-HHA-REVENUE-COST (2)         12200000
122100     END-COMPUTE.                                                 12210000
122200                                                                  12220000
122300     COMPUTE H-HHA-REVENUE-ADD-ON-VISIT-AMT (2) ROUNDED =         12230000
122400             H-HHA-REVENUE-ADD-ON-VISIT-AMT (2) *                 12240000
122500             H-HHA-PROV-VBP-ADJ-FAC                               12250000
122600     END-COMPUTE.                                                 12260000
122700                                                                  12270000
122800     COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                        12280000
122900             H-HHA-TOTAL-PAYMENT +                                12290000
123000             H-HHA-REVENUE-ADD-ON-VISIT-AMT (2)                   12300000
123100     END-COMPUTE.                                                 12310000
123200                                                                  12320000
123300     COMPUTE H-HHA-REVENUE-COST (3) ROUNDED =                     12330000
123400             H-HHA-REVENUE-COST (3) * H-HHA-PROV-VBP-ADJ-FAC      12340000
123500     END-COMPUTE.                                                 12350000
123600                                                                  12360000
123700     COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                        12370000
123800             H-HHA-TOTAL-PAYMENT + H-HHA-REVENUE-COST (3)         12380000
123900     END-COMPUTE.                                                 12390000
124000                                                                  12400000
124100     COMPUTE H-HHA-REVENUE-ADD-ON-VISIT-AMT (3) ROUNDED =         12410000
124200             H-HHA-REVENUE-ADD-ON-VISIT-AMT (3) *                 12420000
124300             H-HHA-PROV-VBP-ADJ-FAC                               12430000
124400     END-COMPUTE.                                                 12440000
124500                                                                  12450000
124600     COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                        12460000
124700             H-HHA-TOTAL-PAYMENT +                                12470000
124800             H-HHA-REVENUE-ADD-ON-VISIT-AMT (3)                   12480000
124900     END-COMPUTE.                                                 12490000
125000                                                                  12500000
125100     COMPUTE H-HHA-REVENUE-COST (4) ROUNDED =                     12510000
125200             H-HHA-REVENUE-COST (4) * H-HHA-PROV-VBP-ADJ-FAC      12520000
125300     END-COMPUTE.                                                 12530000
125400                                                                  12540000
125500     COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                        12550000
125600             H-HHA-TOTAL-PAYMENT + H-HHA-REVENUE-COST (4)         12560000
125700     END-COMPUTE.                                                 12570000
125800                                                                  12580000
125900     COMPUTE H-HHA-REVENUE-ADD-ON-VISIT-AMT (4) ROUNDED =         12590000
126000             H-HHA-REVENUE-ADD-ON-VISIT-AMT (4) *                 12600000
126100             H-HHA-PROV-VBP-ADJ-FAC                               12610000
126200     END-COMPUTE.                                                 12620000
126300                                                                  12630000
126400     COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                        12640000
126500             H-HHA-TOTAL-PAYMENT +                                12650000
126600             H-HHA-REVENUE-ADD-ON-VISIT-AMT (4)                   12660000
126700     END-COMPUTE.                                                 12670000
126800                                                                  12680000
126900     COMPUTE H-HHA-REVENUE-COST (5) ROUNDED =                     12690000
127000             H-HHA-REVENUE-COST (5) * H-HHA-PROV-VBP-ADJ-FAC      12700000
127100     END-COMPUTE.                                                 12710000
127200                                                                  12720000
127300     COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                        12730000
127400             H-HHA-TOTAL-PAYMENT + H-HHA-REVENUE-COST (5)         12740000
127500     END-COMPUTE.                                                 12750000
127600                                                                  12760000
127700     COMPUTE H-HHA-REVENUE-ADD-ON-VISIT-AMT (5) ROUNDED =         12770000
127800             H-HHA-REVENUE-ADD-ON-VISIT-AMT (5) *                 12780000
127900             H-HHA-PROV-VBP-ADJ-FAC                               12790000
128000     END-COMPUTE.                                                 12800000
128100                                                                  12810000
128200     COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                        12820000
128300             H-HHA-TOTAL-PAYMENT +                                12830000
128400             H-HHA-REVENUE-ADD-ON-VISIT-AMT (5)                   12840000
128500     END-COMPUTE.                                                 12850000
128600                                                                  12860000
128700     COMPUTE H-HHA-REVENUE-COST (6) ROUNDED =                     12870000
128800             H-HHA-REVENUE-COST (6) * H-HHA-PROV-VBP-ADJ-FAC      12880000
128900     END-COMPUTE.                                                 12890000
129000                                                                  12900000
129100     COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                        12910000
129200             H-HHA-TOTAL-PAYMENT + H-HHA-REVENUE-COST (6)         12920000
129300     END-COMPUTE.                                                 12930000
129400                                                                  12940000
129500     COMPUTE H-HHA-REVENUE-ADD-ON-VISIT-AMT (6) ROUNDED =         12950000
129600             H-HHA-REVENUE-ADD-ON-VISIT-AMT (6) *                 12960000
129700             H-HHA-PROV-VBP-ADJ-FAC                               12970000
129800     END-COMPUTE.                                                 12980000
129900                                                                  12990000
130000     COMPUTE H-HHA-TOTAL-PAYMENT ROUNDED =                        13000000
130100             H-HHA-TOTAL-PAYMENT +                                13010000
130200             H-HHA-REVENUE-ADD-ON-VISIT-AMT (6)                   13020000
130300     END-COMPUTE.                                                 13030000
130400                                                                  13040000
130500 9120-EXIT.   EXIT.                                               13050000
130600                                                                  13060000
130700******************************************************************13070000
130800* CALCULATES THE STANDARDIZED ALLOWED AMOUNT                     *13080000
130900******************************************************************13090000
131000 9200-CALC-STD-VALUE.                                             13100000
131100                                                                  13110000
131200     MOVE 0                   TO H-HHA-PPS-STD-VALUE.             13120000
131300                                                                  13130000
131400     IF H-HHA-REVENUE-SUM1-6-QTY-ALL < 5 AND                      13140000
131500        H-HHA-TOB IS NOT EQUAL TO 322                             13150000
131600        PERFORM 9210-CALC-STD-VALUE-LUPA                          13160000
131700           THRU 9210-EXIT                                         13170000
131800     END-IF.                                                      13180000
131900                                                                  13190000
132000     IF H-HHA-REVENUE-SUM1-6-QTY-ALL > 4 OR                       13200000
132100        H-HHA-TOB IS EQUAL TO 322                                 13210000
132200        PERFORM 9220-CALC-STD-VALUE-NLUPA                         13220000
132300           THRU 9220-EXIT                                         13230000
132400     END-IF.                                                      13240000
132500                                                                  13250000
132600 9200-EXIT.  EXIT.                                                13260000
132700                                                                  13270000
132800******************************************************************13280000
132900* ADDED FOR CY2018 RELEASE                                       *13290000
133000* CALCULATES THE STANDARDIZED ALLOWED AMOUNT FOR LUPA CLAIMS     *13300000
133100******************************************************************13310000
133200 9210-CALC-STD-VALUE-LUPA.                                        13320000
133300                                                                  13330000
133400     MOVE 0                   TO SS-QCV.                          13340000
133500     PERFORM 6 TIMES                                              13350000
133600       ADD 1                  TO SS-QCV                           13360000
133700                                                                  13370000
133800       IF H-HHA-REVENUE-QTY-COV-VISITS (SS-QCV) > 0               13380000
133900                                                                  13390000
134000         COMPUTE H-HHA-PPS-STD-VALUE ROUNDED =                    13400000
134100                 H-HHA-PPS-STD-VALUE +                            13410000
134200                (H-HHA-REVENUE-QTY-COV-VISITS (SS-QCV) *          13420000
134300                 TB-STDV-REV-DOLL-RATE (SS-QCV))                  13430000
134400         END-COMPUTE                                              13440000
134500                                                                  13450000
134600       END-IF                                                     13460000
134700                                                                  13470000
134800       IF H-HHA-REVENUE-ADD-ON-VISIT-AMT (SS-QCV) > 0             13480000
134900                                                                  13490000
135000         COMPUTE H-HHA-PPS-STD-VALUE ROUNDED =                    13500000
135100                  H-HHA-PPS-STD-VALUE +                           13510000
135200                 (TB-STDV-REV-DOLL-RATE (SS-QCV) *                13520000
135300                      WS-STDV-LUPA-ADDON-FAC)                     13530000
135400         END-COMPUTE                                              13540000
135500                                                                  13550000
135600       END-IF                                                     13560000
135700     END-PERFORM.                                                 13570000
135800                                                                  13580000
135900     COMPUTE H-HHA-PPS-STD-VALUE ROUNDED =                        13590000
136000             H-HHA-PPS-STD-VALUE *                                13600000
136100             WS-STDV-RURAL-FAC                                    13610000
136200     END-COMPUTE.                                                 13620000
136300                                                                  13630000
136400 9210-EXIT.  EXIT.                                                13640000
136500                                                                  13650000
136600******************************************************************13660000
136700* ADDED FOR CY2018 RELEASE                                       *13670000
136800* CALCULATES THE STANDARDIZED ALLOWED AMOUNT FOR NON-LUPA CLAIMS *13680000
136900******************************************************************13690000
137000 9220-CALC-STD-VALUE-NLUPA.                                       13700000
137100                                                                  13710000
137200* HIPPS CALCULATION                                               13720000
137300                                                                  13730000
137400     COMPUTE WS-STD-VALUE-NLUPA-AMT ROUNDED =                     13740000
137500       ( ( H-HHA-HRG-WGTS * WS-STDV-EPISODE-AMT ) +               13750000
137600           0 )*                                                   13760000
137700           H-HHA-HRG-NO-OF-DAYS / 30 *                            13770000
137800           WS-STDV-RURAL-FAC * 1                                  13780000
137900     END-COMPUTE.                                                 13790000
138000                                                                  13800000
138100* OUTLIER CALCULATION                                             13810000
138200                                                                  13820000
138300     COMPUTE WS-STD-VALUE-NLUPA-OUTL ROUNDED =                    13830000
138400       H-HHA-OUTLIER-PAYMENT /                                    13840000
138500       ( (LABOR-PERCENT * WIR-CBSA-WAGEIND) +                     13850000
138600         (1 - LABOR-PERCENT) )                                    13860000
138700     END-COMPUTE.                                                 13870000
138800                                                                  13880000
138900* STANDARD VALUE CALCULATION                                      13890000
139000                                                                  13900000
139100     COMPUTE H-HHA-PPS-STD-VALUE ROUNDED =                        13910000
139200             WS-STD-VALUE-NLUPA-AMT +                             13920000
139300             WS-STD-VALUE-NLUPA-OUTL                              13930000
139400     END-COMPUTE.                                                 13940000
139500                                                                  13950000
139600     IF H-HHA-TOB = 322                                           13960000
139700        IF H-HHA-INIT-PAY-QRP-IND = '1' OR '3'                    13970000
139800           COMPUTE H-HHA-PPS-STD-VALUE ROUNDED =                  13980000
139900                   H-HHA-PPS-STD-VALUE * 0                        13990000
140000           END-COMPUTE                                            14000000
140100           GO TO 9220-EXIT                                        14010000
140200        END-IF                                                    14020000
140300     END-IF.                                                      14030000
140400                                                                  14040000
140500     IF H-HHA-TOB = 322                                           14050000
140600        COMPUTE H-HHA-PPS-STD-VALUE ROUNDED =                     14060000
140700                H-HHA-PPS-STD-VALUE * .20                         14070000
140800        END-COMPUTE                                               14080000
140900     END-IF.                                                      14090000
141000                                                                  14100000
141100*    IF H-HHA-TOB = 322                                           14110000
141200*       IF H-HHA-ADMIT-DATE = H-HHA-SERV-FROM-DATE                14120000
141300*          COMPUTE H-HHA-PPS-STD-VALUE ROUNDED =                  14130000
141400*                  H-HHA-PPS-STD-VALUE * .60                      14140000
141500*          END-COMPUTE                                            14150000
141600*       ELSE                                                      14160000
141700*          COMPUTE H-HHA-PPS-STD-VALUE ROUNDED =                  14170000
141800*                  H-HHA-PPS-STD-VALUE * .50                      14180000
141900*          END-COMPUTE                                            14190000
142000*       END-IF                                                    14200000
142100*    END-IF.                                                      14210000
142200                                                                  14220000
142300 9220-EXIT.  EXIT.                                                14230000
142400                                                                  14240000
142500 10000-OUTLIER-CAP-CALC.                                          14250000
142600                                                                  14260000
142700     IF  HHA-PROV-PAYMENT-TOTAL = 0                               14270000
142800        GO TO 10000-EXIT.                                         14280000
142900                                                                  14290000
143000     IF  HHA-PROV-OUTLIER-PAY-TOTAL = 0                           14300000
143100        GO TO 10000-EXIT.                                         14310000
143200                                                                  14320000
143300     COMPUTE WK-10000-OUTLIER-POOL-PERCENT ROUNDED =              14330000
143400         HHA-PROV-PAYMENT-TOTAL * .1.                             14340000
143500                                                                  14350000
143600     COMPUTE WK-10000-OUTLIER-AVAIL-POOL ROUNDED =                14360000
143700      WK-10000-OUTLIER-POOL-PERCENT - HHA-PROV-OUTLIER-PAY-TOTAL. 14370000
143800                                                                  14380000
143900      COMPUTE WK-10000-OUTLIER-POOL-DIF ROUNDED =                 14390000
144000         WK-10000-OUTLIER-AVAIL-POOL - WK-7000-CALC.              14400000
144100                                                                  14410000
144200      IF WK-10000-OUTLIER-POOL-DIF > 0                            14420000
144300        GO TO 10000-EXIT.                                         14430000
144400                                                                  14440000
144500      IF WK-10000-OUTLIER-POOL-DIF < 0 OR                         14450000
144600         HHA-PROV-OUTLIER-PAY-TOTAL < 0                           14460000
144700        COMPUTE WK-7000-CALC ROUNDED = 0                          14470000
144800        MOVE 4 TO WK-RTC-ADJ-IND.                                 14480000
144900                                                                  14490000
145000 10000-EXIT.   EXIT.                                              14500000
