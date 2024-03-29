C Last modified on 2/21/2023
C national forest district crosswalk with EcoProvince
C The column listed as: Districtor number,EcoProv
       INTEGER DistCnt/505/
       REAL DistProv(505,2)
       INTEGER ForstCnt/108/
       REAL ForstProv(108,2)
       INTEGER RegnCnt/9/
       REAL RegnProv(9,2)
       INTEGER I1, J1, I2, J2, I3, J3
       DATA ((DistProv(I1,J1),J1=1,2),I1=1,50) /
     & 10201,1331,
     & 10203,1331,
     & 10204,1331,
     & 10206,1331,
     & 10208,1331,
     & 10301,1331,
     & 10302,1331,
     & 10303,1331,
     & 10304,1331,
     & 10403,1331,
     & 10404,1331,
     & 10406,1331,
     & 10407,1331,
     & 10408,1331,
     & 11001,1331,
     & 11004,1331,
     & 11006,1331,
     & 11007,1331,
     & 11008,1331,
     & 11103,1331,
     & 11104,1331,
     & 11106,1331,
     & 11107,1331,
     & 11182,1331,
     & 11183,331,
     & 11184,331,
     & 11401,1331,
     & 11403,1331,
     & 11404,1331,
     & 11405,1331,
     & 11407,1331,
     & 11501,1331,
     & 11506,1331,
     & 11507,1331,
     & 11511,1331,
     & 11512,1331,
     & 11514,1331,
     & 11603,1331,
     & 11604,1331,
     & 11605,1331,
     & 11606,1331,
     & 11607,1331,
     & 11701,331,
     & 11705,1331,
     & 11706,1331,
     & 11752,1331,
     & 11753,1331,
     & 11755,1331,
     & 11801,251,
     & 11806,331/

       DATA ((DistProv(I1,J1),J1=1,2),I1=51,100) /
     & 11807,331,
     & 11808,331,
     & 20201,1331,
     & 20203,1331,
     & 20206,1331,
     & 20301,1331,
     & 20303,1331,
     & 20306,1331,
     & 20308,1331,
     & 20402,1331,
     & 20405,1331,
     & 20406,1331,
     & 20407,1331,
     & 20408,1331,
     & 20601,1331,
     & 20602,1331,
     & 20603,1331,
     & 20604,1331,
     & 20605,1331,
     & 20609,331,
     & 20701,331,
     & 20702,331,
     & 20705,331,
     & 20706,331,
     & 20709,331,
     & 20903,1331,
     & 20904,1331,
     & 20907,1331,
     & 21001,1331,
     & 21005,1331,
     & 21006,331,
     & 21007,1331,
     & 21008,1331,
     & 21201,1331,
     & 21202,1331,
     & 21203,1331,
     & 21206,331,
     & 21207,331,
     & 21209,1331,
     & 21210,1331,
     & 21211,1331,
     & 21305,1331,
     & 21306,1331,
     & 21308,1331,
     & 21401,1331,
     & 21402,1331,
     & 21403,1331,
     & 21404,1331,
     & 21405,1331,
     & 21501,1331/

       DATA ((DistProv(I1,J1),J1=1,2),I1=101,150) /
     & 21502,1331,
     & 21503,1331,
     & 21504,1331,
     & 21507,1331,
     & 21508,1331,
     & 21510,1331,
     & 30101,1311,
     & 30102,1311,
     & 30103,311,
     & 30106,1311,
     & 30107,1311,
     & 30201,1331,
     & 30202,1331,
     & 30203,311,
     & 30204,331,
     & 30206,1331,
     & 30207,331,
     & 30302,1311,
     & 30303,1311,
     & 30304,1311,
     & 30305,311,
     & 30306,311,
     & 30307,311,
     & 30406,1311,
     & 30407,1311,
     & 30408,1311,
     & 30501,323,
     & 30502,323,
     & 30503,323,
     & 30504,323,
     & 30505,323,
     & 30602,1311,
     & 30603,1311,
     & 30604,1311,
     & 30605,1311,
     & 30606,1311,
     & 30607,323,
     & 30701,1311,
     & 30703,311,
     & 30704,311,
     & 30801,1311,
     & 30802,1311,
     & 30803,1311,
     & 30901,311,
     & 30903,311,
     & 30905,311,
     & 31001,1311,
     & 31002,1311,
     & 31003,1331,
     & 31005,1331/

       DATA ((DistProv(I1,J1),J1=1,2),I1=151,200) /
     & 31006,331,
     & 31201,311,
     & 31202,311,
     & 31203,311,
     & 31204,1311,
     & 31205,1311,
     & 31206,311,
     & 40101,1331,
     & 40102,1331,
     & 40103,1331,
     & 40104,1331,
     & 40201,1331,
     & 40203,1331,
     & 40204,1331,
     & 40205,1331,
     & 40206,1331,
     & 40301,1331,
     & 40302,1331,
     & 40303,1331,
     & 40304,1331,
     & 40306,1331,
     & 40307,1331,
     & 40701,341,
     & 40702,1341,
     & 40703,1341,
     & 40704,1341,
     & 40801,1341,
     & 40802,1341,
     & 40803,1341,
     & 40804,1341,
     & 41001,341,
     & 41002,1341,
     & 41003,1331,
     & 41004,341,
     & 41005,341,
     & 41201,1331,
     & 41202,341,
     & 41203,1331,
     & 41204,1331,
     & 41206,1331,
     & 41301,1331,
     & 41302,1331,
     & 41304,1331,
     & 41306,1331,
     & 41307,1331,
     & 41308,1331,
     & 41401,341,
     & 41403,1331,
     & 41404,1331,
     & 41405,1331/

       DATA ((DistProv(I1,J1),J1=1,2),I1=201,250) /
     & 41551,1331,
     & 41552,1331,
     & 41553,1331,
     & 41554,1331,
     & 41555,1331,
     & 41556,1331,
     & 41557,341,
     & 41701,1261,
     & 41702,341,
     & 41703,1341,
     & 41705,323,
     & 41706,341,
     & 41709,1341,
     & 41710,341,
     & 41901,341,
     & 41902,1331,
     & 41903,1331,
     & 41904,1331,
     & 41906,1331,
     & 41907,1331,
     & 41908,1331,
     & 50151,1261,
     & 50152,1261,
     & 50153,1261,
     & 50252,1261,
     & 50253,1261,
     & 50254,1261,
     & 50351,1261,
     & 50353,1261,
     & 50355,1261,
     & 50356,1261,
     & 50451,341,
     & 50452,1261,
     & 50453,341,
     & 50454,1261,
     & 50551,1261,
     & 50552,1261,
     & 50554,1261,
     & 50555,1261,
     & 50557,1261,
     & 50558,1261,
     & 50651,1261,
     & 50653,1261,
     & 50658,1261,
     & 50751,263,
     & 50753,1261,
     & 50754,263,
     & 50755,1261,
     & 50757,1261,
     & 50853,1261/

       DATA ((DistProv(I1,J1),J1=1,2),I1=251,300) /
     & 50854,1261,
     & 50856,1261,
     & 50953,1261,
     & 50954,1261,
     & 50955,1261,
     & 50956,1261,
     & 51051,1261,
     & 51052,1261,
     & 51053,1261,
     & 51054,1261,
     & 51101,1261,
     & 51102,1261,
     & 51103,1261,
     & 51252,1261,
     & 51253,1261,
     & 51255,1261,
     & 51351,1261,
     & 51352,1261,
     & 51354,1261,
     & 51451,1261,
     & 51452,1261,
     & 51454,1261,
     & 51456,1261,
     & 51458,1261,
     & 51459,1261,
     & 51461,1261,
     & 51553,1261,
     & 51555,1261,
     & 51651,1261,
     & 51652,1261,
     & 51653,1261,
     & 51654,1261,
     & 51753,1261,
     & 51754,1261,
     & 51756,1261,
     & 51757,1261,
     & 51953,1261,
     & 60101,1242,
     & 60102,1242,
     & 60105,1242,
     & 60201,1242,
     & 60202,1261,
     & 60203,1242,
     & 60204,1242,
     & 60211,1242,
     & 60212,1242,
     & 60213,1261,
     & 60301,1242,
     & 60303,1242,
     & 60305,1242/

       DATA ((DistProv(I1,J1),J1=1,2),I1=301,350) /
     & 60401,1331,
     & 60402,1331,
     & 60404,1331,
     & 60501,1242,
     & 60502,1242,
     & 60505,1242,
     & 60506,1242,
     & 60601,1242,
     & 60605,1242,
     & 60606,1242,
     & 60609,1242,
     & 60701,1331,
     & 60702,1331,
     & 60705,341,
     & 60901,1242,
     & 60902,1242,
     & 60903,1242,
     & 60905,1242,
     & 61020,1261,
     & 61022,1261,
     & 61024,1242,
     & 61026,1261,
     & 61028,1261,
     & 61201,1242,
     & 61208,1242,
     & 61402,341,
     & 61404,1331,
     & 61405,1331,
     & 61406,1331,
     & 61501,1242,
     & 61502,1261,
     & 61503,1242,
     & 61506,1242,
     & 61631,1331,
     & 61602,1331,
     & 61604,1331,
     & 61605,1331,
     & 61606,1331,
     & 61702,1242,
     & 61703,1242,
     & 61704,1242,
     & 61705,1242,
     & 61707,1242,
     & 61708,1242,
     & 61801,1242,
     & 61803,1242,
     & 61804,1242,
     & 61805,1242,
     & 62103,1331,
     & 62104,1331/

       DATA ((DistProv(I1,J1),J1=1,2),I1=351,400) /
     & 62105,1331,
     & 62109,1331,
     & 62112,1331,
     & 80101,231,
     & 80103,231,
     & 80104,231,
     & 80105,231,
     & 80106,231,
     & 80107,231,
     & 80211,221,
     & 80214,221,
     & 80216,221,
     & 80217,221,
     & 80301,231,
     & 80302,1221,
     & 80306,1221,
     & 80308,231,
     & 80402,1221,
     & 80403,221,
     & 80404,221,
     & 80406,1221,
     & 80501,231,
     & 80502,231,
     & 80504,231,
     & 80505,231,
     & 80506,231,
     & 80601,231,
     & 80602,231,
     & 80603,231,
     & 80604,231,
     & 80608,231,
     & 80701,231,
     & 80702,231,
     & 80704,231,
     & 80705,231,
     & 80706,231,
     & 80707,231,
     & 80717,231,
     & 80802,1221,
     & 80803,1221,
     & 80804,1221,
     & 80806,1221,
     & 80811,1221,
     & 80812,221,
     & 80813,1221,
     & 80814,1221,
     & 80901,231,
     & 80902,1231,
     & 80903,231,
     & 80904,231/

       DATA ((DistProv(I1,J1),J1=1,2),I1=401,450) /
     & 80905,1231,
     & 80906,1231,
     & 80907,1231,
     & 80908,1231,
     & 80909,231,
     & 80910,1231,
     & 80911,1231,
     & 80912,1231,
     & 81001,221,
     & 81002,1221,
     & 81003,1221,
     & 81004,1221,
     & 81005,1221,
     & 81006,231,
     & 81007,231,
     & 81102,1221,
     & 81103,231,
     & 81104,1221,
     & 81105,1221,
     & 81107,1221,
     & 81109,1221,
     & 81110,231,
     & 81111,1221,
     & 81202,1221,
     & 81203,231,
     & 81204,231,
     & 81205,231,
     & 81301,231,
     & 81303,231,
     & 81304,231,
     & 81307,231,
     & 81308,251,
     & 81601,411,
     & 90301,211,
     & 90302,211,
     & 90303,211,
     & 90401,211,
     & 90402,211,
     & 90403,211,
     & 90404,211,
     & 90405,211,
     & 90406,211,
     & 90407,211,
     & 90503,221,
     & 90504,221,
     & 90505,221,
     & 90507,221,
     & 90521,221,
     & 90523,221,
     & 90701,211/

       DATA ((DistProv(I1,J1),J1=1,2),I1=451,500) /
     & 90702,211,
     & 90703,211,
     & 90704,211,
     & 90705,211,
     & 90706,211,
     & 90802,221,
     & 90804,221,
     & 90901,211,
     & 90902,211,
     & 90905,211,
     & 90906,211,
     & 90907,211,
     & 91001,211,
     & 91002,211,
     & 91003,211,
     & 91004,211,
     & 91005,211,
     & 91202,221,
     & 91204,221,
     & 91301,211,
     & 91302,211,
     & 91303,211,
     & 91304,211,
     & 91305,211,
     & 91401,221,
     & 91403,221,
     & 91902,211,
     & 91903,211,
     & 92001,1211,
     & 92002,1211,
     & 92003,211,
     & 92005,1211,
     & 92101,1221,
     & 92102,1221,
     & 92103,1221,
     & 92104,1221,
     & 92105,1221,
     & 92106,1221,
     & 92202,1211,
     & 92204,1211,
     & 92205,1211,
     & 92206,221,
     & 100410,1242,
     & 100420,1242,
     & 100430,1242,
     & 100510,1242,
     & 100521,1242,
     & 100522,1242,
     & 100531,1242,
     & 100532,1242/

       DATA ((DistProv(I1,J1),J1=1,2),I1=501,505) /
     & 100533,1242,
     & 100534,1242,
     & 100535,1242,
     & 100552,1242,
     & 100554,1242/

       DATA ((ForstProv(I2,J2),J2=1,2),I2=1,50) /
     & 102,1331,
     & 103,1331,
     & 104,1331,
     & 110,1331,
     & 111,1331,
     & 114,1331,
     & 115,1331,
     & 116,1331,
     & 117,1331,
     & 118,331,
     & 202,1331,
     & 203,1331,
     & 204,1331,
     & 206,1331,
     & 207,331,
     & 209,1331,
     & 210,1331,
     & 212,1331,
     & 213,1331,
     & 214,1331,
     & 215,1331,
     & 301,1311,
     & 302,1331,
     & 303,311,
     & 304,1311,
     & 305,323,
     & 306,1311,
     & 307,311,
     & 308,1311,
     & 309,311,
     & 310,1311,
     & 312,311,
     & 401,1331,
     & 402,1331,
     & 403,1331,
     & 407,1341,
     & 408,1341,
     & 410,341,
     & 412,1331,
     & 413,1331,
     & 414,1331,
     & 415,1331,
     & 417,341,
     & 419,1331,
     & 501,1261,
     & 502,1261,
     & 503,1261,
     & 504,341,
     & 505,1261,
     & 506,1261/

       DATA ((ForstProv(I2,J2),J2=1,2),I2=51,100) /
     & 507,1261,
     & 508,1261,
     & 509,1261,
     & 510,1261,
     & 511,1261,
     & 512,1261,
     & 513,1261,
     & 514,1261,
     & 515,1261,
     & 516,1261,
     & 517,1261,
     & 519,1261,
     & 601,1242,
     & 602,1242,
     & 603,1242,
     & 604,1331,
     & 605,1242,
     & 606,1242,
     & 607,1331,
     & 609,1242,
     & 610,1261,
     & 612,1242,
     & 614,1331,
     & 615,1242,
     & 616,1331,
     & 617,1242,
     & 618,1242,
     & 621,1331,
     & 801,231,
     & 802,221,
     & 803,231,
     & 804,221,
     & 805,231,
     & 806,231,
     & 807,231,
     & 808,1221,
     & 809,1231,
     & 810,1221,
     & 811,1221,
     & 812,231,
     & 813,231,
     & 816,411,
     & 903,211,
     & 904,211,
     & 905,221,
     & 907,211,
     & 908,221,
     & 909,211,
     & 910,211,
     & 912,221/

       DATA ((ForstProv(I2,J2),J2=1,2),I2=101,108) /
     & 913,211,
     & 914,221,
     & 919,211,
     & 920,1211,
     & 921,1221,
     & 922,1211,
     & 1004,1242,
     & 1005,1242/

       DATA ((RegnProv(I3,J3),J3=1,2),I3=1,9) /
     & 1,1331,
     & 2,1331,
     & 3,1311,
     & 4,1331,
     & 5,1261,
     & 6,1242,
     & 8,231,
     & 9,211,
     & 10,1242/
