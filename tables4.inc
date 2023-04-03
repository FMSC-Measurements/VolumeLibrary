C Last modified on 2/21/2023
C NVBC equation coefficients from Table S4
C The column listed as: SPCD,DIVISION,STDORGCD,equation,a, a0, a1, b, b0, b1, b2, c, c1
       INTEGER Tbl4Cnt/361/
       REAL SPcoef(361,13)
       REAL JKcoef(9,5)
       INTEGER I1, J1
       DATA ((SPcoef(I1,J1),J1=1,13),I1=1,50) /
     & 12,130,0,6,2.307138234658,0,0,0.919617774205,0,0,0,0,0,
     & 12,210,0,6,2.256494854552,0,0,0.929101469947,0,0,0,0,0,
     & 12,220,0,6,2.385795624417,0,0,0.933831836626,0,0,0,0,0,
     & 12,1210,0,6,2.268692670371,0,0,0.912654151757,0,0,0,0,0,
     & 12,0,0,6,2.290293338415,0,0,0.923304144356,0,0,0,0,0,
     & 15,1260,0,6,2.281307537384,0,0,0.935346691815,0,0,0,0,0,
     & 15,1310,0,6,2.25895348339,0,0,0.880677001029,0,0,0,0,0,
     & 15,1330,0,6,2.314050308374,0,0,0.935771018907,0,0,0,0,0,
     & 15,0,0,6,2.286492555572,0,0,0.923976554173,0,0,0,0,0,
     & 16,0,0,6,2.297842170057,0,0,0.942559224185,0,0,0,0,0,
     & 17,0,0,6,2.344739100852,0,0,0.932565061683,0,0,0,0,0,
     & 18,0,0,6,2.431042145105,0,0,0.942559224185,0,0,0,0,0,
     & 19,1330,0,6,2.455324416301,0,0,0.992885248756,0,0,0,0,0,
     & 19,0,0,6,2.443217781319,0,0,0.974403517709,0,0,0,0,0,
     & 20,1260,0,6,2.258270253529,0,0,0.903804319435,0,0,0,0,0,
     & 20,0,0,6,2.260578864688,0,0,0.904996599285,0,0,0,0,0,
     & 42,1240,0,6,2.244140496087,0,0,0.913681160395,0,0,0,0,0,
     & 42,0,0,6,2.244140496087,0,0,0.913681160395,0,0,0,0,0,
     & 43,230,0,6,2.100704540159,0,0,0.84703394036,0,0,0,0,0,
     & 43,0,0,6,2.104512570742,0,0,0.848892330294,0,0,0,0,0,
     & 68,220,0,6,2.637687139734,0,0,0.925679299474,0,0,0,0,0,
     & 68,230,0,6,2.486450201597,0,0,0.868528907592,0,0,0,0,0,
     & 68,0,0,6,2.565420642796,0,0,0.896177794421,0,0,0,0,0,
     & 70,0,0,6,2.282279868046,0,0,0.883333468058,0,0,0,0,0,
     & 71,130,0,6,2.311227844993,0,0,0.870122737772,0,0,0,0,0,
     & 71,210,0,6,2.310717292769,0,0,0.905502950613,0,0,0,0,0,
     & 71,0,0,6,2.31414720497,0,0,0.87926962056,0,0,0,0,0,
     & 73,0,0,6,2.05083267798,0,0,0.883333468058,0,0,0,0,0,
     & 81,1260,0,6,2.504316269517,0,0,0.902838067968,0,0,0,0,0,
     & 81,0,0,6,2.504316269517,0,0,0.902838067968,0,0,0,0,0,
     & 90,0,0,6,2.319395665449,0,0,0.924037454475,0,0,0,0,0,
     & 91,210,0,6,2.425115333367,0,0,0.921208491974,0,0,0,0,0,
     & 91,0,0,6,2.426885311937,0,0,0.921888258605,0,0,0,0,0,
     & 93,1330,0,6,2.316054364146,0,0,0.918526810669,0,0,0,0,0,
     & 93,1340,0,6,2.238474748867,0,0,0.94573696295,0,0,0,0,0,
     & 93,0,0,6,2.285884410155,0,0,0.929353274897,0,0,0,0,0,
     & 94,130,0,6,2.233513167314,0,0,0.889823346604,0,0,0,0,0,
     & 94,210,0,6,2.380687159278,0,0,0.915792217878,0,0,0,0,0,
     & 94,1130,0,6,2.277135883636,0,0,0.931583150475,0,0,0,0,0,
     & 94,0,0,6,2.247547562826,0,0,0.892936617578,0,0,0,0,0,
     & 95,130,0,6,2.202048385499,0,0,0.901607246417,0,0,0,0,0,
     & 95,210,0,6,2.295419501732,0,0,0.907234341416,0,0,0,0,0,
     & 95,1130,0,6,2.287326781482,0,0,0.939555564623,0,0,0,0,0,
     & 95,0,0,6,2.220640876897,0,0,0.903244638799,0,0,0,0,0,
     & 97,130,0,6,2.200532244909,0,0,0.891695910925,0,0,0,0,0,
     & 97,210,0,6,2.11969343597,0,0,0.929883580972,0,0,0,0,0,
     & 97,1210,0,6,2.192381616969,0,0,0.928607272531,0,0,0,0,0,
     & 97,0,0,6,2.162471773316,0,0,0.921406673027,0,0,0,0,0,
     & 98,1240,0,6,2.238794894984,0,0,0.936633343235,0,0,0,0,0,
     & 98,0,0,6,2.238794894984,0,0,0.936633343235,0,0,0,0,0/

       DATA ((SPcoef(I1,J1),J1=1,13),I1=51,100) /
     & 100,130,0,6,2.110191719824,0,0,0.945742440595,0,0,0,0,0,
     & 100,0,0,6,2.110191719824,0,0,0.945742440595,0,0,0,0,0,
     & 105,130,0,6,2.191458559312,0,0,0.923503345596,0,0,0,0,0,
     & 105,210,0,6,2.280209057975,0,0,0.931641134905,0,0,0,0,0,
     & 105,220,0,6,2.127630401028,0,0,0.924409074799,0,0,0,0,0,
     & 105,0,0,6,2.204858649928,0,0,0.924734369895,0,0,0,0,0,
     & 107,230,0,6,2.164143489136,0,0,0.917954645888,0,0,0,0,0,
     & 107,0,0,6,2.164143489136,0,0,0.917954645888,0,0,0,0,0,
     & 108,130,0,6,2.027520610841,0,0,0.957326182931,0,0,0,0,0,
     & 108,330,0,6,2.114226943554,0,0,0.926405003971,0,0,0,0,0,
     & 108,1130,0,6,2.12410357755,0,0,0.992398762612,0,0,0,0,0,
     & 108,1330,0,6,2.204433240999,0,0,0.996077592276,0,0,0,0,0,
     & 108,0,0,6,2.069631914771,0,0,0.96037752551,0,0,0,0,0,
     & 110,220,0,6,2.010776058304,0,0,0.924693380566,0,0,0,0,0,
     & 110,230,0,6,2.063275681077,0,0,0.886018608685,0,0,0,0,0,
     & 110,1220,0,6,2.020889449807,0,0,0.946288783297,0,0,0,0,0,
     & 110,1230,0,6,2.052449175268,0,0,0.944128713816,0,0,0,0,0,
     & 110,0,0,6,2.048385017296,0,0,0.897850093982,0,0,0,0,0,
     & 111,230,0,6,2.031462229442,0,0,0.829936344786,0,0,0,0,0,
     & 111,0,0,6,2.031149553878,0,0,0.829797693464,0,0,0,0,0,
     & 111,230,1,6,2.1271005837,0,0,0.85574289238,0,0,0,0,0,
     & 111,0,1,6,2.1271005837,0,0,0.85574289238,0,0,0,0,0,
     & 113,0,0,6,2.277333794522,0,0,0.903512603508,0,0,0,0,0,
     & 115,230,0,6,2.213738290753,0,0,0.953380674624,0,0,0,0,0,
     & 115,0,0,6,2.213952901306,0,0,0.953133232797,0,0,0,0,0,
     & 116,0,0,6,2.215875645493,0,0,0.903512603508,0,0,0,0,0,
     & 117,1260,0,6,2.338885674283,0,0,0.912274910574,0,0,0,0,0,
     & 117,0,0,6,2.34255722257,0,0,0.913010954048,0,0,0,0,0,
     & 119,0,0,6,2.252383496413,0,0,0.903512603508,0,0,0,0,0,
     & 121,230,0,6,2.117686248461,0,0,0.907598818612,0,0,0,0,0,
     & 121,0,0,6,2.117680702834,0,0,0.907590354365,0,0,0,0,0,
     & 122,310,0,6,2.295808563259,0,0,0.957389664178,0,0,0,0,0,
     & 122,330,0,6,2.191231136088,0,0,0.938322327117,0,0,0,0,0,
     & 122,340,0,6,2.214213634041,0,0,0.920768443325,0,0,0,0,0,
     & 122,1240,0,6,2.218203838569,0,0,0.886290842724,0,0,0,0,0,
     & 122,1260,0,6,2.253795549126,0,0,0.90625503545,0,0,0,0,0,
     & 122,1310,0,6,2.259121711874,0,0,0.959872267444,0,0,0,0,0,
     & 122,1330,0,6,2.212922806239,0,0,0.938568732849,0,0,0,0,0,
     & 122,1340,0,6,2.314560911648,0,0,1.026064936147,0,0,0,0,0,
     & 122,0,0,6,2.225646063067,0,0,0.933592758312,0,0,0,0,0,
     & 123,1220,0,6,2.007590011021,0,0,0.907416892224,0,0,0,0,0,
     & 123,0,0,6,1.966083128976,0,0,0.901106430655,0,0,0,0,0,
     & 125,130,0,6,2.06917700336,0,0,0.916687773733,0,0,0,0,0,
     & 125,210,0,6,2.077114007109,0,0,0.932287210448,0,0,0,0,0,
     & 125,220,0,6,2.097208194965,0,0,0.923821225248,0,0,0,0,0,
     & 125,1210,0,6,2.210695303425,0,0,0.914712358146,0,0,0,0,0,
     & 125,0,0,6,2.078680997376,0,0,0.924148129724,0,0,0,0,0,
     & 126,1220,0,6,2.11653398942,0,0,0.924391197153,0,0,0,0,0,
     & 126,0,0,6,2.12488111252,0,0,0.921727135205,0,0,0,0,0,
     & 128,230,0,6,2.053605674203,0,0,0.882312922855,0,0,0,0,0/

       DATA ((SPcoef(I1,J1),J1=1,13),I1=101,150) /
     & 128,0,0,6,2.053605674203,0,0,0.882312922855,0,0,0,0,0,
     & 129,130,0,6,2.110134399318,0,0,0.899611189358,0,0,0,0,0,
     & 129,210,0,6,2.14382457528,0,0,0.926956801598,0,0,0,0,0,
     & 129,220,0,6,2.16824144336,0,0,0.92893727093,0,0,0,0,0,
     & 129,230,0,6,2.263702307496,0,0,0.873853394327,0,0,0,0,0,
     & 129,1210,0,6,2.325833967092,0,0,0.938352813319,0,0,0,0,0,
     & 129,1220,0,6,2.322484121321,0,0,0.919936243836,0,0,0,0,0,
     & 129,0,0,6,2.168405377457,0,0,0.908425248262,0,0,0,0,0,
     & 130,0,0,6,2.340985443012,0,0,0.903512603508,0,0,0,0,0,
     & 131,220,0,6,2.191093431968,0,0,0.897131272363,0,0,0,0,0,
     & 131,230,0,6,2.120196112478,0,0,0.870729288379,0,0,0,0,0,
     & 131,1230,0,6,2.229790826677,0,0,0.935838183583,0,0,0,0,0,
     & 131,0,0,6,2.122822027836,0,0,0.871992951105,0,0,0,0,0,
     & 131,230,1,6,2.250737023919,0,0,0.895496713776,0,0,0,0,0,
     & 131,0,1,6,2.252471647379,0,0,0.895208838092,0,0,0,0,0,
     & 132,220,0,6,2.063242651877,0,0,0.893255702249,0,0,0,0,0,
     & 132,230,0,6,2.07109313416,0,0,0.87625833688,0,0,0,0,0,
     & 132,1220,0,6,2.119827773598,0,0,0.891856867793,0,0,0,0,0,
     & 132,0,0,6,2.083913927755,0,0,0.881605303758,0,0,0,0,0,
     & 202,130,0,6,2.243520687131,0,0,0.905819729229,0,0,0,0,0,
     & 202,240,0,6,2.386864288974,0,0,0.907607415992,0,0,0,0,0,
     & 202,260,0,6,2.292462699062,0,0,0.903012327254,0,0,0,0,0,
     & 202,340,0,6,2.200638785761,0,0,0.899314507064,0,0,0,0,0,
     & 202,1240,0,6,2.320767541932,0,0,0.925394093799,0,0,0,0,0,
     & 202,1260,0,6,2.250783654313,0,0,0.935419648298,0,0,0,0,0,
     & 202,1310,0,6,2.365869800419,0,0,0.901434070276,0,0,0,0,0,
     & 202,1330,0,6,2.290748434508,0,0,0.912358253452,0,0,0,0,0,
     & 202,0,0,6,2.295591234774,0,0,0.913185703689,0,0,0,0,0,
     & 211,260,0,6,2.355955490568,0,0,0.904651279221,0,0,0,0,0,
     & 211,0,0,6,2.358359447798,0,0,0.905462311373,0,0,0,0,0,
     & 221,230,0,6,1.909978071084,0,0,0.645965350533,0,0,0,0,0,
     & 221,0,0,6,1.909978071084,0,0,0.645965350533,0,0,0,0,0,
     & 222,230,0,6,1.992238843363,0,0,0.605750323394,0,0,0,0,0,
     & 222,0,0,6,1.992238843363,0,0,0.605750323394,0,0,0,0,0,
     & 241,130,0,6,2.378956884374,0,0,0.815165791852,0,0,0,0,0,
     & 241,210,0,6,2.352321217132,0,0,0.840044116297,0,0,0,0,0,
     & 241,0,0,6,2.371876652167,0,0,0.819660561604,0,0,0,0,0,
     & 242,340,0,6,2.115533058086,0,0,0.76312243228,0,0,0,0,0,
     & 242,1240,0,6,2.468046048338,0,0,0.924229452735,0,0,0,0,0,
     & 242,0,0,6,2.386957496285,0,0,0.882454212712,0,0,0,0,0,
     & 260,0,0,6,1.999964491999,0,0,0.942559224185,0,0,0,0,0,
     & 261,130,0,6,2.371677774584,0,0,0.885184041394,0,0,0,0,0,
     & 261,210,0,6,2.40554824986,0,0,0.931091220493,0,0,0,0,0,
     & 261,220,0,6,2.531785323476,0,0,0.952694421005,0,0,0,0,0,
     & 261,1220,0,6,2.463884413375,0,0,0.918421064223,0,0,0,0,0,
     & 261,0,0,6,2.423444796372,0,0,0.912869245282,0,0,0,0,0,
     & 263,340,0,6,2.343390395083,0,0,0.962890453233,0,0,0,0,0,
     & 263,1240,0,6,2.313637041588,0,0,0.957111472708,0,0,0,0,0,
     & 263,0,0,6,2.320819492498,0,0,0.958415881477,0,0,0,0,0,
     & 264,0,0,6,2.30440804951,0,0,0.942559224185,0,0,0,0,0/

       DATA ((SPcoef(I1,J1),J1=1,13),I1=151,200) /
     & 311,0,0,6,2.633775314893,0,0,0.914221744065,0,0,0,0,0,
     & 313,0,0,6,2.518143654078,0,0,0.939678042487,0,0,0,0,0,
     & 314,0,0,6,2.473107871222,0,0,0.895693875803,0,0,0,0,0,
     & 315,0,0,6,2.491381868165,0,0,0.914221744065,0,0,0,0,0,
     & 316,210,0,6,2.436960587679,0,0,0.928868546808,0,0,0,0,0,
     & 316,220,0,6,2.581816163495,0,0,0.932380744473,0,0,0,0,0,
     & 316,230,0,6,2.498523812238,0,0,0.835176259658,0,0,0,0,0,
     & 316,1220,0,6,2.509392320362,0,0,0.909544424852,0,0,0,0,0,
     & 316,0,0,6,2.500241064397,0,0,0.88374141693,0,0,0,0,0,
     & 317,220,0,6,2.641813545371,0,0,0.906482972658,0,0,0,0,0,
     & 317,0,0,6,2.664858045113,0,0,0.895692903484,0,0,0,0,0,
     & 318,210,0,6,2.370344508184,0,0,0.944922217514,0,0,0,0,0,
     & 318,220,0,6,2.311214149994,0,0,0.93442617282,0,0,0,0,0,
     & 318,1210,0,6,2.244248991535,0,0,0.929589658499,0,0,0,0,0,
     & 318,1220,0,6,2.620892258336,0,0,0.918088136651,0,0,0,0,0,
     & 318,0,0,6,2.363842996376,0,0,0.938627556159,0,0,0,0,0,
     & 330,1220,0,6,2.511665161827,0,0,0.924935095069,0,0,0,0,0,
     & 330,0,0,6,2.51257041628,0,0,0.926378801455,0,0,0,0,0,
     & 351,1240,0,6,2.33974574632,0,0,0.921133403353,0,0,0,0,0,
     & 351,0,0,6,2.344539648007,0,0,0.923693746676,0,0,0,0,0,
     & 370,230,0,6,2.56691236662,0,0,0.851839015861,0,0,0,0,0,
     & 370,1220,0,6,2.501805384345,0,0,0.889701048259,0,0,0,0,0,
     & 370,0,0,6,2.52329363783,0,0,0.873544856067,0,0,0,0,0,
     & 371,130,0,6,2.814348682438,0,0,0.984179291992,0,0,0,0,0,
     & 371,210,0,6,2.194622984428,0,0,0.888301633378,0,0,0,0,0,
     & 371,220,0,6,2.18843158377,0,0,0.9460723967,0,0,0,0,0,
     & 371,1210,0,6,2.203704193552,0,0,0.913895221474,0,0,0,0,0,
     & 371,1220,0,6,2.650541601242,0,0,0.89346213399,0,0,0,0,0,
     & 371,0,0,6,2.326737852884,0,0,0.9192468438,0,0,0,0,0,
     & 372,0,0,6,2.472498631511,0,0,0.905071442811,0,0,0,0,0,
     & 373,0,0,6,2.714435367399,0,0,0.914221744065,0,0,0,0,0,
     & 375,130,0,6,2.441659786785,0,0,0.924048667662,0,0,0,0,0,
     & 375,210,0,6,2.32711446318,0,0,0.938617975854,0,0,0,0,0,
     & 375,220,0,6,2.198640560954,0,0,0.892754511863,0,0,0,0,0,
     & 375,1210,0,6,2.337232641831,0,0,0.94891981591,0,0,0,0,0,
     & 375,0,0,6,2.374241254527,0,0,0.933325360582,0,0,0,0,0,
     & 379,0,0,6,2.388692295458,0,0,0.914221744065,0,0,0,0,0,
     & 391,0,0,6,2.524928391411,0,0,0.902460357794,0,0,0,0,0,
     & 400,220,0,6,2.423858428801,0,0,0.857488620076,0,0,0,0,0,
     & 400,230,0,6,2.441770007731,0,0,0.859526582865,0,0,0,0,0,
     & 400,1220,0,6,2.403589931134,0,0,0.865554193395,0,0,0,0,0,
     & 400,1230,0,6,2.522831953991,0,0,0.961128668727,0,0,0,0,0,
     & 400,0,0,6,2.430905595241,0,0,0.864969743921,0,0,0,0,0,
     & 402,0,0,6,2.448398408064,0,0,0.895693875803,0,0,0,0,0,
     & 403,220,0,6,2.449134707202,0,0,0.903225622198,0,0,0,0,0,
     & 403,0,0,6,2.39074912123,0,0,0.896947270178,0,0,0,0,0,
     & 404,230,0,6,2.413489657723,0,0,0.885403439596,0,0,0,0,0,
     & 404,0,0,6,2.413489657723,0,0,0.885403439596,0,0,0,0,0,
     & 405,0,0,6,2.478900472355,0,0,0.895693875803,0,0,0,0,0,
     & 407,0,0,6,2.411218817185,0,0,0.899160985677,0,0,0,0,0/

       DATA ((SPcoef(I1,J1),J1=1,13),I1=201,250) /
     & 409,0,0,6,2.50044411555,0,0,0.895693875803,0,0,0,0,0,
     & 421,220,0,6,2.622035050324,0,0,0.942845625741,0,0,0,0,0,
     & 421,0,0,6,2.622035050324,0,0,0.942845625741,0,0,0,0,0,
     & 460,230,0,6,2.629425824482,0,0,0.851642209543,0,0,0,0,0,
     & 460,0,0,6,2.643840861199,0,0,0.85208304775,0,0,0,0,0,
     & 461,230,0,6,2.503691452246,0,0,0.90601368405,0,0,0,0,0,
     & 461,0,0,6,2.503691452246,0,0,0.90601368405,0,0,0,0,0,
     & 462,0,0,6,2.638271878844,0,0,0.902460357794,0,0,0,0,0,
     & 471,0,0,6,2.327129564411,0,0,0.902460357794,0,0,0,0,0,
     & 491,230,0,6,2.628770481131,0,0,0.878890905605,0,0,0,0,0,
     & 491,0,0,6,2.622765312486,0,0,0.871626558008,0,0,0,0,0,
     & 521,230,0,6,2.244302927477,0,0,0.861049105942,0,0,0,0,0,
     & 521,0,0,6,2.256779260295,0,0,0.854768010675,0,0,0,0,0,
     & 531,210,0,6,2.320736982268,0,0,0.944512627114,0,0,0,0,0,
     & 531,220,0,6,2.286181828843,0,0,0.936513016234,0,0,0,0,0,
     & 531,230,0,6,2.794098687761,0,0,0.873295878445,0,0,0,0,0,
     & 531,1210,0,6,2.305537580767,0,0,0.935247966666,0,0,0,0,0,
     & 531,1220,0,6,2.676337647131,0,0,0.899059041871,0,0,0,0,0,
     & 531,0,0,6,2.503712085291,0,0,0.922426667341,0,0,0,0,0,
     & 540,230,0,6,2.165550462772,0,0,0.774395070684,0,0,0,0,0,
     & 540,1220,0,6,2.473147847492,0,0,0.917732495583,0,0,0,0,0,
     & 540,0,0,6,2.255097409942,0,0,0.81000959684,0,0,0,0,0,
     & 541,210,0,6,2.515992448491,0,0,0.924060926482,0,0,0,0,0,
     & 541,220,0,6,2.385411557347,0,0,0.939880821682,0,0,0,0,0,
     & 541,1210,0,6,2.33197580106,0,0,0.937726228994,0,0,0,0,0,
     & 541,1220,0,6,2.643286712686,0,0,0.969994279331,0,0,0,0,0,
     & 541,0,0,6,2.484769335569,0,0,0.930237199202,0,0,0,0,0,
     & 543,210,0,6,2.307776320373,0,0,0.897421403585,0,0,0,0,0,
     & 543,0,0,6,2.313812216647,0,0,0.903597451941,0,0,0,0,0,
     & 544,230,0,6,2.552265134189,0,0,0.830166133656,0,0,0,0,0,
     & 544,0,0,6,2.488153285789,0,0,0.857884534069,0,0,0,0,0,
     & 552,0,0,6,2.545450581919,0,0,0.902460357794,0,0,0,0,0,
     & 555,230,0,6,2.296837460438,0,0,0.790698942572,0,0,0,0,0,
     & 555,0,0,6,2.296837460438,0,0,0.790698942572,0,0,0,0,0,
     & 591,230,0,6,2.481503563967,0,0,0.928105014469,0,0,0,0,0,
     & 591,0,0,6,2.481503563967,0,0,0.928105014469,0,0,0,0,0,
     & 601,0,0,6,2.516943148816,0,0,0.902460357794,0,0,0,0,0,
     & 602,220,0,6,2.424143029234,0,0,0.950547646934,0,0,0,0,0,
     & 602,1220,0,6,2.502202836361,0,0,0.930080404067,0,0,0,0,0,
     & 602,0,0,6,2.499796233311,0,0,0.945887372378,0,0,0,0,0,
     & 611,230,0,6,2.282656638532,0,0,0.838748052439,0,0,0,0,0,
     & 611,0,0,6,2.283517802093,0,0,0.83944281436,0,0,0,0,0,
     & 621,220,0,6,2.466360746654,0,0,0.941714455244,0,0,0,0,0,
     & 621,230,0,6,2.33451333607,0,0,0.876582536633,0,0,0,0,0,
     & 621,1220,0,6,2.421612862753,0,0,0.943375272959,0,0,0,0,0,
     & 621,0,0,6,2.372655682043,0,0,0.904388001423,0,0,0,0,0,
     & 650,0,0,6,2.319233912849,0,0,0.902460357794,0,0,0,0,0,
     & 651,1220,0,6,2.389740036558,0,0,0.940990495289,0,0,0,0,0,
     & 651,0,0,6,2.324881278656,0,0,0.932036846624,0,0,0,0,0,
     & 652,0,0,6,2.455293370594,0,0,0.902460357794,0,0,0,0,0/

       DATA ((SPcoef(I1,J1),J1=1,13),I1=251,300) /
     & 653,230,0,6,2.149596166338,0,0,0.756088756994,0,0,0,0,0,
     & 653,0,0,6,2.149596166338,0,0,0.756088756994,0,0,0,0,0,
     & 680,0,0,6,2.858353056226,0,0,0.902460357794,0,0,0,0,0,
     & 691,230,0,6,2.286926620045,0,0,0.623051573242,0,0,0,0,0,
     & 691,0,0,6,2.286926620045,0,0,0.623051573242,0,0,0,0,0,
     & 693,220,0,6,2.443944110042,0,0,0.895127734318,0,0,0,0,0,
     & 693,230,0,6,2.320240242183,0,0,0.819372379174,0,0,0,0,0,
     & 693,1220,0,6,2.341122507788,0,0,0.900278322424,0,0,0,0,0,
     & 693,0,0,6,2.336705332356,0,0,0.864944712492,0,0,0,0,0,
     & 694,230,0,6,2.071230649657,0,0,0.684052253799,0,0,0,0,0,
     & 694,0,0,6,2.071230649657,0,0,0.684052253799,0,0,0,0,0,
     & 701,0,0,6,2.530702213076,0,0,0.902460357794,0,0,0,0,0,
     & 711,0,0,6,2.472458958529,0,0,0.902460357794,0,0,0,0,0,
     & 731,230,0,6,2.30041018555,0,0,0.835124538954,0,0,0,0,0,
     & 731,0,0,6,2.326676793248,0,0,0.834775082154,0,0,0,0,0,
     & 740,130,0,6,2.243790144342,0,0,0.871999961609,0,0,0,0,0,
     & 740,1330,0,6,2.152156100347,0,0,0.969447212847,0,0,0,0,0,
     & 740,0,0,6,2.254777398503,0,0,0.932766525831,0,0,0,0,0,
     & 741,130,0,6,2.320044511733,0,0,0.933560601532,0,0,0,0,0,
     & 741,210,0,6,2.406351929837,0,0,0.920820002987,0,0,0,0,0,
     & 741,0,0,6,2.340880628124,0,0,0.924655081737,0,0,0,0,0,
     & 742,0,0,6,2.5772182151,0,0,0.925115047932,0,0,0,0,0,
     & 743,130,0,6,2.292377306701,0,0,0.964387460762,0,0,0,0,0,
     & 743,210,0,6,2.311375758208,0,0,0.970306740239,0,0,0,0,0,
     & 743,0,0,6,2.308099836459,0,0,0.96736921026,0,0,0,0,0,
     & 745,0,0,6,2.750870342649,0,0,0.965584036242,0,0,0,0,0,
     & 746,130,0,6,2.196485783849,0,0,0.957638664492,0,0,0,0,0,
     & 746,210,0,6,2.248339144687,0,0,0.950086359767,0,0,0,0,0,
     & 746,220,0,6,2.217906978453,0,0,0.936890779193,0,0,0,0,0,
     & 746,1130,0,6,2.451986171086,0,0,0.994446906801,0,0,0,0,0,
     & 746,1330,0,6,2.277876716892,0,0,0.963595474779,0,0,0,0,0,
     & 746,0,0,6,2.217737024787,0,0,0.954474602572,0,0,0,0,0,
     & 762,210,0,6,2.39136093684,0,0,0.950609983749,0,0,0,0,0,
     & 762,220,0,6,2.530477876285,0,0,0.961967017186,0,0,0,0,0,
     & 762,230,0,6,2.515112697511,0,0,0.912642176533,0,0,0,0,0,
     & 762,1220,0,6,2.417498737083,0,0,0.962712431877,0,0,0,0,0,
     & 762,0,0,6,2.435498049897,0,0,0.947256312595,0,0,0,0,0,
     & 800,230,0,6,2.478170078514,0,0,0.873668316104,0,0,0,0,0,
     & 800,0,0,6,2.478170078514,0,0,0.873668316104,0,0,0,0,0,
     & 802,210,0,6,2.605951905197,0,0,0.889926474993,0,0,0,0,0,
     & 802,220,0,6,2.431021635505,0,0,0.8661631105,0,0,0,0,0,
     & 802,230,0,6,2.368234502956,0,0,0.815443198117,0,0,0,0,0,
     & 802,1220,0,6,2.413673220682,0,0,0.851093936311,0,0,0,0,0,
     & 802,1230,0,6,2.480182683688,0,0,0.913941060474,0,0,0,0,0,
     & 802,0,0,6,2.397656373196,0,0,0.835018633686,0,0,0,0,0,
     & 804,0,0,6,2.549867815844,0,0,0.895693875803,0,0,0,0,0,
     & 806,220,0,6,2.385529309344,0,0,0.882220892107,0,0,0,0,0,
     & 806,230,0,6,2.38685050735,0,0,0.831045132843,0,0,0,0,0,
     & 806,1220,0,6,2.394328554906,0,0,0.866263532709,0,0,0,0,0,
     & 806,0,0,6,2.392050741136,0,0,0.856518917441,0,0,0,0,0/

       DATA ((SPcoef(I1,J1),J1=1,13),I1=301,350) /
     & 809,0,0,6,2.57975394106,0,0,0.895693875803,0,0,0,0,0,
     & 812,230,0,6,2.434957686163,0,0,0.824926387604,0,0,0,0,0,
     & 812,0,0,6,2.430894246483,0,0,0.827652003645,0,0,0,0,0,
     & 813,230,0,6,2.327109557086,0,0,0.810843012299,0,0,0,0,0,
     & 813,0,0,6,2.330417273801,0,0,0.811809788057,0,0,0,0,0,
     & 817,0,0,6,2.66173442577,0,0,0.895693875803,0,0,0,0,0,
     & 820,230,0,6,2.374065815653,0,0,0.800110500485,0,0,0,0,0,
     & 820,0,0,6,2.373998029115,0,0,0.800346242802,0,0,0,0,0,
     & 822,230,0,6,2.525892680434,0,0,0.880947283979,0,0,0,0,0,
     & 822,0,0,6,2.525892680434,0,0,0.880947283979,0,0,0,0,0,
     & 823,0,0,6,2.523171845374,0,0,0.895693875803,0,0,0,0,0,
     & 825,230,0,6,2.277576596895,0,0,0.754766397224,0,0,0,0,0,
     & 825,0,0,6,2.283945369611,0,0,0.756971713155,0,0,0,0,0,
     & 826,0,0,6,2.595277688385,0,0,0.895693875803,0,0,0,0,0,
     & 827,230,0,6,2.41538228072,0,0,0.839266917262,0,0,0,0,0,
     & 827,0,0,6,2.41538228072,0,0,0.839266917262,0,0,0,0,0,
     & 828,230,0,6,2.544569441955,0,0,0.87217364367,0,0,0,0,0,
     & 828,0,0,6,2.544569441955,0,0,0.87217364367,0,0,0,0,0,
     & 830,0,0,6,2.575554919996,0,0,0.895693875803,0,0,0,0,0,
     & 831,230,0,6,2.480055545414,0,0,0.835312318618,0,0,0,0,0,
     & 831,0,0,6,2.476835259222,0,0,0.834920863235,0,0,0,0,0,
     & 832,220,0,6,2.439776307756,0,0,0.922999813735,0,0,0,0,0,
     & 832,230,0,6,2.46533765954,0,0,0.896245651362,0,0,0,0,0,
     & 832,1220,0,6,2.445861834893,0,0,0.909950840055,0,0,0,0,0,
     & 832,0,0,6,2.450485705415,0,0,0.908883498543,0,0,0,0,0,
     & 833,210,0,6,2.433354119172,0,0,0.892772812638,0,0,0,0,0,
     & 833,220,0,6,2.476535009321,0,0,0.892300820683,0,0,0,0,0,
     & 833,230,0,6,2.303017093688,0,0,0.806732870276,0,0,0,0,0,
     & 833,1220,0,6,2.482979108945,0,0,0.887190619919,0,0,0,0,0,
     & 833,0,0,6,2.437105961565,0,0,0.874737566567,0,0,0,0,0,
     & 834,0,0,6,2.599079762895,0,0,0.895693875803,0,0,0,0,0,
     & 835,220,0,6,2.546574110539,0,0,0.904814857222,0,0,0,0,0,
     & 835,230,0,6,2.46796614944,0,0,0.841875682298,0,0,0,0,0,
     & 835,0,0,6,2.484033947115,0,0,0.855892084131,0,0,0,0,0,
     & 837,220,0,6,2.431655450681,0,0,0.888874861644,0,0,0,0,0,
     & 837,230,0,6,2.404156898672,0,0,0.832740045229,0,0,0,0,0,
     & 837,1210,0,6,2.26246668695,0,0,0.908913582407,0,0,0,0,0,
     & 837,1220,0,6,2.454223205094,0,0,0.879063389189,0,0,0,0,0,
     & 837,0,0,6,2.426763441358,0,0,0.8716335072,0,0,0,0,0,
     & 838,230,0,6,2.61485067581,0,0,0.852650936999,0,0,0,0,0,
     & 838,0,0,6,2.61485067581,0,0,0.852650936999,0,0,0,0,0,
     & 901,1220,0,6,2.484302556441,0,0,0.922023406929,0,0,0,0,0,
     & 901,0,0,6,2.486563201541,0,0,0.918070279877,0,0,0,0,0,
     & 920,0,0,6,2.355911112697,0,0,0.965584036242,0,0,0,0,0,
     & 922,0,0,6,2.521901805306,0,0,0.965584036242,0,0,0,0,0,
     & 931,0,0,6,2.196547242483,0,0,0.902460357794,0,0,0,0,0,
     & 950,210,0,6,1.884744112,0,0,0.827486236847,0,0,0,0,0,
     & 950,1220,0,6,2.354532009675,0,0,0.950834226533,0,0,0,0,0,
     & 950,0,0,6,2.091127405823,0,0,0.873285578539,0,0,0,0,0,
     & 951,210,0,6,2.302846226769,0,0,0.952512915091,0,0,0,0,0/

       DATA ((SPcoef(I1,J1),J1=1,13),I1=351,361) /
     & 951,220,0,6,2.373852809743,0,0,0.93250435061,0,0,0,0,0,
     & 951,0,0,6,2.321775129113,0,0,0.950405419302,0,0,0,0,0,
     & 970,230,0,6,2.470065214477,0,0,0.852413996698,0,0,0,0,0,
     & 970,0,0,6,2.476714228773,0,0,0.855815791256,0,0,0,0,0,
     & 971,0,0,6,2.565858523493,0,0,0.902460357794,0,0,0,0,0,
     & 972,210,0,6,2.776398290661,0,0,0.889107149582,0,0,0,0,0,
     & 972,0,0,6,2.715680442955,0,0,0.890875629787,0,0,0,0,0,
     & 975,0,0,6,2.239892081381,0,0,0.902460357794,0,0,0,0,0,
     & 999,230,0,6,2.476866562011,0,0,0.842332131774,0,0,0,0,0,
     & 999,1220,0,6,2.306852741587,0,0,0.869370347056,0,0,0,0,0,
     & 999,0,0,6,2.385998142067,0,0,0.852145003033,0,0,0,0,0/


       DATA ((JKcoef(I1,J1),J1=1,5),I1=1, 9) /
     & 1,6,2.27287205553,0.81583127324,0,
     & 2,6,2.295591234774,0.913185703689,0,
     & 3,6,2.315849710366,0.926650501746,0,
     & 4,6,2.123005404685,0.889868807398,0,
     & 5,6,2.223964139072,0.906473983625,0,
     & 6,6,2.247027060542,0.944938402154,0,
     & 7,6,2.432627082007,0.902678817152,0,
     & 8,6,2.317280548447,0.846218848701,0,
     & 9,6,2.430338525578,0.866173151871,0/