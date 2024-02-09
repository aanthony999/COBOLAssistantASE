************************************************************************00010000
*                                                                      *00011000
*                           README                                     *00012000
*                                                                      *00013000
* DATASET NAME:        MU00.@BFN2699.HHNDM213                          *00014016
* PRICER:              HOME HEALTH PPS PRICER (HHA)                    *00015000
* VERSION:             2021.3                                          *00016016
* CR#:                 11855 & 12107                                   *00016103
*                                                                      *00016200
* EFFECTIVE DATE:      01/01/2021                                      *00016306
* BETA/PRODUCTION:     PRODUCTION                                      *00016401
* RELEASE DATE:        04/23/2021                                      *00016518
*                                                                      *00016600
************************************************************************00016700
                                                                        00016800
DESCRIPTION:                                                            00016900
************************************************************************00017000
*                CY2021.3 CHANGES                                      *00018016
*-- HHDRV213 ----------------------------------------------------------*00018216
* UPDATED PROGRAM TO CALL HHCAL213                                     *00018417
* UPDATED TO NOT GET THE CBSA FOR TOB-RAP WITH A                       *00018517
*    HHA-SERV-FROM-DATE > '20201231'                                   *00018617
*-- HHCAL213 ----------------------------------------------------------*00018716
* UPDATED PARAGRAPH 9110-COMPUTE-LATE-SUB-PENALTY                      *00018817
* IF THE DAYS DIFFERENCE IS GREAT THAN 30 DAYS, USE 30 DAYS            *00018917
* IF THE DAYS DIFFERENCE IS LESS THAN 31 DAYS, USE ACTUAL DAYS         *00019117
*-- ADDONTBL ----------------------------------------------------------*00019316
* UPDATED WITH ADDITIONAL COUNTY CODES                                 *00020016
************************************************************************00040000
                                                                        00050000
RELEASE CONTENTS:                                                       00051000
                                                                        00052000
************************************************************************00053000
MU00.@BFN2699.HHNDM213:                                                 00054016
                                                                        00055000
HHDRV213 == 2021.3 HHA DRIVER PROGRAM CY2021                            00056016
HHCAL213 == 2021.3 HHA CALC   PROGRAM CY2021                            00057016
ADDONTBL == 2021.3 HHA COUNTY CODE ADD ON TABLE                         00057116
MANIFEST == 2021.3 LIST OF MEMBERS ASSOCIATE WITH THIS RELEASE          00058016
README   == 2021.3 DESCRIPTION OF RELEASE                               00059016
                                                                        00060000
************************************************************************00070000
