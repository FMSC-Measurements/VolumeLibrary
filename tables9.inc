C Last modified on 2/21/2023
C NVBC equation coefficients from Table S9
C The column listed as: SPCD,DIVISION,STDORGCD,equation,a, a0, a1, b, b0, b1, b2, c, c1
       INTEGER Tbl9Cnt/201/
       REAL SPcoef(201,13)
       REAL JKcoef(9,5)
       INTEGER I1, J1
       DATA ((SPcoef(I1,J1),J1=1,13),I1=1,50) /
     & 12,210,0,1,11.539010367964,0,0,2.483196825997,0,0,
     &    0,-1.094254110011,0,
     & 12,0,0,1,15.605084742471,0,0,2.635317276572,0,0,
     &    0,-1.24698502093,0,
     & 15,1260,0,1,0.190013872533,0,0,1.723532757879,0,0,
     &    0,0.358146254457,0,
     & 15,0,0,1,0.45257970782,0,0,1.839181317263,0,0,0,0.090906829291,0,
     & 16,0,0,1,2.263841230775,0,0,2.149913486634,0,0,
     &    0,-0.503239821284,0,
     & 17,1330,0,1,0.301608040893,0,0,2.062927801381,0,0,
     &    0,0.170076208344,0,
     & 17,0,0,1,2.742349630078,0,0,1.9494839195,0,0,0,-0.31488549412,0,
     & 19,1330,0,1,9.599908709032,0,0,2.35496141518,0,0,
     &    0,-0.89912555409,0,
     & 19,0,0,1,3.958526266318,0,0,2.354548256443,0,0,
     &    0,-0.693801733604,0,
     & 20,0,0,1,2.263841230775,0,0,2.270287197573,0,0,
     &    0,-0.503239821284,0,
     & 68,0,0,1,1.229992968048,0,0,1.91003854589,0,0,
     &    0,-0.282127411799,0,
     & 71,210,0,1,1.867233716729,0,0,2.181310435128,0,0,
     &    0,-0.715231695716,0,
     & 71,0,0,1,8.571727724531,0,0,2.48230008161,0,0,
     &    0,-1.222946645319,0,
     & 73,1330,0,1,2.332767688452,0,0,2.192831044072,0,0,
     &    0,-0.845807879337,0,
     & 73,0,0,1,1.108074285001,0,0,2.11027190681,0,0,
     &    0,-0.597411426768,0,
     & 81,0,0,1,1.229992968048,0,0,1.950696127272,0,0,
     &    0,-0.282127411799,0,
     & 91,0,0,1,6.911634726964,0,0,2.769861537205,0,0,
     &    0,-0.986830404355,0,
     & 93,1330,0,1,3.859977303158,0,0,2.263740717787,0,0,
     &    0,-0.5922118042,0,
     & 93,0,0,1,2.988093047549,0,0,2.245664172177,0,0,
     &    0,-0.541530255269,0,
     & 94,210,0,1,20.730559777339,0,0,2.733935183586,0,0,
     &    0,-1.386865266707,0,
     & 94,0,0,1,6.581820798637,0,0,2.691511677904,0,0,
     &    0,-1.010198107218,0,
     & 95,130,0,1,69.834428469656,0,0,2.928142967451,0,0,
     &    0,-1.788182000874,0,
     & 95,210,0,1,13.485005172552,0,0,2.697469918701,0,0,
     &    0,-1.257385803227,0,
     & 95,0,0,1,42.719115960252,0,0,2.7249775935,0,0,
     &    0,-1.585388570998,0,
     & 97,1220,0,1,2.045056850874,0,0,1.850051242907,0,0,
     &    0,-0.290769146728,0,
     & 97,0,0,1,2.685953180251,0,0,1.943987143806,0,0,
     &    0,-0.420476272142,0,
     & 98,0,0,1,6.911634726964,0,0,2.52221171001,0,0,
     &    0,-0.986830404355,0,
     & 101,0,0,1,6.857362436773,0,0,2.173509838505,0,0,
     &    0,-1.104092286666,0,
     & 105,130,0,1,4.886995544865,0,0,2.570258650412,0,0,
     &    0,-1.015986463361,0,
     & 105,210,0,1,38585.5866150749,0,0,3.91136590129,0,0,
     &    0,-4.097306422612,0,
     & 105,0,0,1,20.786326457262,0,0,2.666534899752,0,0,
     &    0,-1.487746884702,0,
     & 108,1330,0,2,0,7.897891369725,0,0,2.922159816047,2.804650740743,
     &    0,-1.306961710828,0,
     & 108,0,0,2,0,5.449179614537,0,0,2.824503256615,2.516175387511,
     &    0,-1.163243204707,0,
     & 110,230,0,1,2.01290513454,0,0,2.579140325749,0,0,
     &    0,-0.854025041692,0,
     & 110,1230,0,1,8.10781980873,0,0,2.746458252462,0,0,
     &    0,-1.420072219885,0,
     & 110,0,0,1,2.195307738473,0,0,2.814921128662,0,0,
     &    0,-1.063846911125,0,
     & 111,230,0,1,6.320403109915,0,0,2.828071285132,0,0,
     &    0,-1.251061938318,0,
     & 111,0,0,1,6.320403109915,0,0,2.828071285132,0,0,
     &    0,-1.251061938318,0,
     & 111,230,1,4,25.811510230051,0,0,2.654632369001,0,0,
     &    -0.037573714528,-1.500902066464,0,
     & 111,0,1,4,25.811510230051,0,0,2.654632369001,0,0,
     &    -0.037573714528,-1.500902066464,0,
     & 116,0,0,1,6.857362436773,0,0,2.781426712456,0,0,
     &    0,-1.104092286666,0,
     & 119,0,0,1,6.857362436773,0,0,2.563692337229,0,0,
     &    0,-1.104092286666,0,
     & 121,230,0,1,2.021188492527,0,0,2.36859036142,0,0,
     &    0,-0.647543390273,0,
     & 121,0,0,1,2.021188492527,0,0,2.36859036142,0,0,
     &    0,-0.647543390273,0,
     & 122,1240,0,1,1.221980506256,0,0,2.584056909775,0,0,
     &    0,-0.723256352975,0,
     & 122,1260,0,1,1.937929560794,0,0,2.39552908587,0,0,
     &    0,-0.557391922529,0,
     & 122,1310,0,1,0.945500162881,0,0,2.775912005349,0,0,
     &    0,-0.822849471485,0,
     & 122,1330,0,1,0.79856113285,0,0,2.385559427099,0,0,
     &    0,-0.471429027557,0,
     & 122,0,0,1,0.965263779612,0,0,2.482840715083,0,0,
     &    0,-0.56001370318,0,
     & 123,0,0,1,6.857362436773,0,0,2.344041733148,0,0,
     &    0,-1.104092286666,0/

       DATA ((SPcoef(I1,J1),J1=1,13),I1=51,100) /
     & 125,210,0,1,4.356820969604,0,0,2.144698775149,0,0,
     &    0,-0.746957998247,0,
     & 125,0,0,1,5.098080304392,0,0,2.168002353191,0,0,
     &    0,-0.800604544968,0,
     & 126,0,0,1,6.857362436773,0,0,2.437176888835,0,0,
     &    0,-1.104092286666,0,
     & 128,0,0,1,6.857362436773,0,0,2.068587015628,0,0,
     &    0,-1.104092286666,0,
     & 129,210,0,1,2.2593835254,0,0,2.058702622483,0,0,
     &    0,-0.646815596975,0,
     & 129,1220,0,1,23.394396078009,0,0,2.707164838998,0,0,
     &    0,-1.553003018826,0,
     & 129,0,0,1,4.565769387087,0,0,2.319340078153,0,0,
     &    0,-0.965629246886,0,
     & 131,230,0,2,0,8.337502274954,0,0,2.388420507856,2.620206080045,
     &    0,-1.11938861596,0,
     & 131,0,0,2,0,11.317191225674,0,0,2.453122303,2.685927753485,
     &    0,-1.229936328861,0,
     & 131,230,1,3,16.294487734613,0,1.985339013907,0,0,0.148348452554,
     &    0,-0.859129789175,0.618339172768,
     & 131,0,1,3,16.294487734613,0,1.985339013907,0,0,0.148348452554,
     &    0,-0.859129789175,0.618339172768,
     & 132,1220,0,1,22.978974493061,0,0,2.417410557068,0,0,
     &    0,-1.425483752014,0,
     & 132,0,0,1,19.652679629152,0,0,2.312104169344,0,0,
     &    0,-1.293378466284,0,
     & 133,340,0,1,11.545262277026,0,0,1.501359265197,0,0,
     &    0,-0.474110468044,0,
     & 133,0,0,1,10.810716870835,0,0,1.493695175391,0,0,
     &    0,-0.448522571343,0,
     & 202,240,0,2,0,0.477184595914,0,0,2.592670351881,1.249237428914,
     &    0,-0.325050455055,0,
     & 202,1210,0,2,0,0.976718930862,0,0,1.888740043925,1.549486061144,
     &    0,-0.132262670294,0,
     & 202,1240,0,2,0,19.494956525468,0,0,2.719154988425,2.296286409523,
     &    0,-1.341525208266,0,
     & 202,1260,0,2,0,1.263596954534,0,0,2.405107268532,2.510805499206,
     &    0,-0.622599504709,0,
     & 202,1330,0,2,0,4.885819494226,0,0,2.045280169322,2.066284358809,
     &    0,-0.625377411844,0,
     & 202,0,0,2,0,3.292996404588,0,0,2.300932814855,1.868929882965,
     &    0,-0.668257644479,0,
     & 211,260,0,1,1.191820107431,0,0,1.232984483949,0,0,
     &    0,0.149164980509,0,
     & 211,0,0,1,1.205578219769,0,0,1.235502171359,0,0,
     &    0,0.142166609769,0,
     & 212,0,0,1,1.229992968048,0,0,1.729529585345,0,0,
     &    0,-0.282127411799,0,
     & 221,0,0,1,1.229992968048,0,0,1.11168762574,0,0,
     &    0,-0.282127411799,0,
     & 222,0,0,1,1.229992968048,0,0,1.549206327831,0,0,
     &    0,-0.282127411799,0,
     & 241,210,0,1,0.413877859177,0,0,2.255042125168,0,0,
     &    0,-0.294566570961,0,
     & 241,0,0,1,0.413877859177,0,0,2.255042125168,0,0,
     &    0,-0.294566570961,0,
     & 242,0,0,1,1.229992968048,0,0,2.048487969601,0,0,
     &    0,-0.282127411799,0,
     & 261,210,0,1,0.447507796809,0,0,2.00987665729,0,0,
     &    0,-0.163759423904,0,
     & 261,1220,0,1,0.105067949517,0,0,1.01733757354,0,0,
     &    0,0.822402649359,0,
     & 261,0,0,1,0.394212172805,0,0,1.67192050985,0,0,
     &    0,0.091153181074,0,
     & 263,0,0,1,5.117016134914,0,0,2.679607477481,0,0,
     &    0,-1.067402223925,0,
     & 312,0,0,1,2.768287112606,0,0,1.890950302834,0,0,
     &    0,-0.812510842684,0,
     & 313,230,0,1,86.639008448887,0,0,3.729452689127,0,0,
     &    0,-2.395382766815,0,
     & 313,0,0,1,86.639008448887,0,0,3.729452689127,0,0,
     &    0,-2.395382766815,0,
     & 316,210,0,1,5.202047909856,0,0,2.114300246481,0,0,
     &    0,-0.975485822654,0,
     & 316,230,0,1,0.133774283462,0,0,1.945302525561,0,0,
     &    0,0.09800163185,0,
     & 316,1220,0,1,4.505398952978,0,0,2.14216740368,0,0,
     &    0,-0.83965506717,0,
     & 316,0,0,1,0.850316556558,0,0,1.998961809584,0,0,
     &    0,-0.418446486365,0,
     & 317,0,0,1,24.894029598454,0,0,1.717168898392,0,0,
     &    0,-1.080721416383,0,
     & 318,210,0,1,0.392154397043,0,0,1.749876619316,0,0,
     &    0,-0.11978145606,0,
     & 318,0,0,1,0.162369407246,0,0,1.733240678727,0,0,
     &    0,0.107905407362,0,
     & 351,240,0,1,11.652373169069,0,0,2.791100545348,0,0,
     &    0,-1.520751946471,0,
     & 351,0,0,1,2.411251329331,0,0,2.442199749744,0,0,
     &    0,-0.92815983796,0,
     & 361,0,0,1,1.369098120116,0,0,2.128492123541,0,0,
     &    0,-0.706453868101,0,
     & 370,0,0,1,2.768287112606,0,0,2.330264644684,0,0,
     &    0,-0.812510842684,0,
     & 371,210,0,1,7.592341638798,0,0,1.529823396175,0,0,
     &    0,-0.790470945421,0,
     & 371,0,0,1,1.801590291071,0,0,1.519332591318,0,0,
     &    0,-0.442314932155,0,
     & 372,0,0,1,2.768287112606,0,0,2.275710124292,0,0,
     &    0,-0.812510842684,0/

       DATA ((SPcoef(I1,J1),J1=1,13),I1=101,150) /
     & 375,210,0,1,68.158765671863,0,0,3.172276410541,0,0,
     &    0,-2.118709411404,0,
     & 375,1210,0,1,2.879275885414,0,0,2.766984910086,0,0,
     &    0,-0.999972535727,0,
     & 375,0,0,1,3.342942244464,0,0,2.623874309926,0,0,
     &    0,-1.036863522144,0,
     & 391,0,0,1,0.101935668035,0,0,2.150969015552,0,0,
     &    0,0.125610203884,0,
     & 400,210,0,1,0.098851773999,0,0,2.340936762892,0,0,
     &    0,-0.064264694586,0,
     & 400,230,0,1,0.001610249662,0,0,1.781365466824,0,0,
     &    0,1.308047667966,0,
     & 400,0,0,1,0.10843696133,0,0,2.163204823758,0,0,0,0.06056329306,0,
     & 402,0,0,1,0.383493367847,0,0,1.9869543512,0,0,
     &    0,-0.131159328276,0,
     & 403,0,0,1,0.383493367847,0,0,2.037133546743,0,0,
     &    0,-0.131159328276,0,
     & 431,0,0,1,1.369098120116,0,0,2.298313125758,0,0,
     &    0,-0.706453868101,0,
     & 460,0,0,1,1.369098120116,0,0,2.378234855422,0,0,
     &    0,-0.706453868101,0,
     & 461,230,0,1,0.003623694825,0,0,1.57694450612,0,0,
     &    0,1.109521989583,0,
     & 461,0,0,1,0.003623694825,0,0,1.57694450612,0,0,
     &    0,1.109521989583,0,
     & 462,0,0,1,1.369098120116,0,0,2.287490816968,0,0,
     &    0,-0.706453868101,0,
     & 471,0,0,1,1.369098120116,0,0,2.129799744354,0,0,
     &    0,-0.706453868101,0,
     & 491,230,0,1,0.713461736101,0,0,2.263126763835,0,0,
     &    0,-0.434861857761,0,
     & 491,0,0,1,0.236496975913,0,0,2.370433359627,0,0,
     &    0,-0.12373909345,0,
     & 521,0,0,1,1.369098120116,0,0,2.35663320051,0,0,
     &    0,-0.706453868101,0,
     & 531,210,0,1,0.324920714585,0,0,1.609509076065,0,0,
     &    0,-0.150995117932,0,
     & 531,0,0,1,0.005694717636,0,0,1.435768475135,0,0,
     &    0,1.034561288601,0,
     & 540,230,0,1,0.857704027144,0,0,2.409028649925,0,0,
     &    0,-0.713260835304,0,
     & 540,0,0,1,0.005592866653,0,0,1.652831403738,0,0,
     &    0,0.908279097974,0,
     & 541,210,0,1,0.118000151775,0,0,1.328628443014,0,0,
     &    0,0.351061805712,0,
     & 541,0,0,1,0.1355879307,0,0,1.330617476394,0,0,0,0.31649131556,0,
     & 543,0,0,1,1.369098120116,0,0,1.990651632952,0,0,
     &    0,-0.706453868101,0,
     & 544,230,0,1,0.041451851049,0,0,1.270652587738,0,0,
     &    0,0.669962614594,0,
     & 544,0,0,1,0.106761063559,0,0,1.480384048899,0,0,
     &    0,0.299913309144,0,
     & 591,0,0,1,1.369098120116,0,0,2.286721882621,0,0,
     &    0,-0.706453868101,0,
     & 611,230,0,1,1.126667596249,0,0,2.296820298918,0,0,
     &    0,-0.706002084985,0,
     & 611,0,0,1,1.211029864853,0,0,2.292090163725,0,0,
     &    0,-0.718887613048,0,
     & 621,230,0,1,6.840226755732,0,0,2.56528584501,0,0,
     &    0,-1.285330794947,0,
     & 621,1220,0,1,16.478119020567,0,0,2.63421704133,0,0,
     &    0,-1.457944182499,0,
     & 621,0,0,1,4.177171877632,0,0,2.113762653994,0,0,
     &    0,-0.908415934662,0,
     & 631,0,0,1,1.369098120116,0,0,2.440421384149,0,0,
     &    0,-0.706453868101,0,
     & 653,0,0,1,1.369098120116,0,0,2.249364027279,0,0,
     &    0,-0.706453868101,0,
     & 691,230,0,1,11.354024232125,0,0,2.445719489878,0,0,
     &    0,-1.479591494678,0,
     & 691,0,0,1,11.354024232125,0,0,2.445719489878,0,0,
     &    0,-1.479591494678,0,
     & 693,0,0,1,1.369098120116,0,0,2.245028064194,0,0,
     &    0,-0.706453868101,0,
     & 694,230,0,1,15.145084825995,0,0,3.154480317282,0,0,
     &    0,-1.852174058905,0,
     & 694,0,0,1,15.145084825995,0,0,3.154480317282,0,0,
     &    0,-1.852174058905,0,
     & 711,0,0,1,1.369098120116,0,0,1.996351891746,0,0,
     &    0,-0.706453868101,0,
     & 731,0,0,1,1.369098120116,0,0,2.397951373069,0,0,
     &    0,-0.706453868101,0,
     & 740,230,0,1,6.058287747638,0,0,2.77055384274,0,0,
     &    0,-1.515738803968,0,
     & 740,0,0,1,375.925078101129,0,0,3.474680126123,0,0,
     &    0,-2.829832114379,0,
     & 741,210,0,1,0.339557969757,0,0,1.61290222975,0,0,
     &    0,-0.118743697941,0,
     & 741,0,0,1,0.339557969757,0,0,1.61290222975,0,0,
     &    0,-0.118743697941,0,
     & 742,230,0,1,21.738935960807,0,0,2.471001179146,0,0,
     &    0,-1.345104883,0,
     & 742,0,0,1,21.738935960807,0,0,2.471001179146,0,0,
     &    0,-1.345104883,0,
     & 743,210,0,1,8.968358501893,0,0,2.366034779507,0,0,
     &    0,-1.3219555427,0,
     & 743,0,0,1,8.968358501893,0,0,2.366034779507,0,0,
     &    0,-1.3219555427,0/

       DATA ((SPcoef(I1,J1),J1=1,13),I1=151,200) /
     & 746,130,0,1,2.813617869467,0,0,2.578245150077,0,0,
     &    0,-1.092783190046,0,
     & 746,210,0,1,2.720163593645,0,0,2.543098464583,0,0,
     &    0,-1.098977777865,0,
     & 746,1330,0,1,0.881281432101,0,0,1.722206011569,0,0,
     &    0,-0.526259153401,0,
     & 746,0,0,1,0.222624778186,0,0,1.942697994951,0,0,
     &    0,-0.202392206335,0,
     & 761,0,0,1,1.369098120116,0,0,2.147922301203,0,0,
     &    0,-0.706453868101,0,
     & 762,210,0,1,1.61568822268,0,0,1.534361966125,0,0,
     &    0,-0.507104558952,0,
     & 762,0,0,1,0.508613911697,0,0,1.400511704001,0,0,
     &    0,-0.086220596993,0,
     & 800,0,0,1,0.383493367847,0,0,1.891016280581,0,0,
     &    0,-0.131159328276,0,
     & 802,210,0,1,0.003211888508,0,0,1.242312652242,0,0,
     &    0,1.340444127166,0,
     & 802,220,0,1,2.744926621884,0,0,2.142558783502,0,0,
     &    0,-0.71658058793,0,
     & 802,230,0,1,0.241927499698,0,0,2.43074136534,0,0,
     &    0,-0.194894241093,0,
     & 802,1220,0,1,0.03832401169,0,0,1.740655717258,0,0,
     &    0,0.500290321354,0,
     & 802,0,0,1,0.011370280063,0,0,1.365067161545,0,0,
     &    0,1.090732935119,0,
     & 806,0,0,1,349.277131807459,0,0,2.888645202729,0,0,
     &    0,-2.224941584838,0,
     & 807,0,0,1,0.383493367847,0,0,1.852450376292,0,0,
     &    0,-0.131159328276,0,
     & 812,0,0,1,7.753553301043,0,0,2.822250274386,0,0,
     &    0,-1.28787472921,0,
     & 813,0,0,1,0.383493367847,0,0,2.041564262296,0,0,
     &    0,-0.131159328276,0,
     & 819,0,0,1,0.383493367847,0,0,2.569148668971,0,0,
     &    0,-0.131159328276,0,
     & 820,0,0,1,0.383493367847,0,0,2.030312775536,0,0,
     &    0,-0.131159328276,0,
     & 822,230,0,1,0.465148293317,0,0,1.970075532575,0,0,
     &    0,-0.249313450173,0,
     & 822,0,0,1,0.465148293317,0,0,1.970075532575,0,0,
     &    0,-0.249313450173,0,
     & 823,0,0,1,0.383493367847,0,0,1.807204004733,0,0,
     &    0,-0.131159328276,0,
     & 824,0,0,1,0.383493367847,0,0,2.57931628515,0,0,
     &    0,-0.131159328276,0,
     & 827,230,0,1,9.412284538358,0,0,2.338414867873,0,0,
     &    0,-1.011294080903,0,
     & 827,0,0,1,9.412284538358,0,0,2.338414867873,0,0,
     &    0,-1.011294080903,0,
     & 828,230,0,1,7.249260296234,0,0,1.858688751424,0,0,
     &    0,-0.75625809564,0,
     & 828,0,0,1,7.249260296234,0,0,1.858688751424,0,0,
     &    0,-0.75625809564,0,
     & 831,230,0,1,0.121217896613,0,0,1.645923709266,0,0,
     &    0,0.244195488692,0,
     & 831,0,0,1,0.121217896613,0,0,1.645923709266,0,0,
     &    0,0.244195488692,0,
     & 832,1220,0,1,0.420062157564,0,0,2.11695175154,0,0,
     &    0,-0.26276432368,0,
     & 832,0,0,1,2.864293585676,0,0,2.297214959554,0,0,
     &    0,-0.809503031914,0,
     & 833,210,0,1,0.044051316102,0,0,1.891799289661,0,0,
     &    0,0.326716180826,0,
     & 833,0,0,1,0.109338181326,0,0,1.66948604496,0,0,
     &    0,0.246229103614,0,
     & 835,0,0,1,0.383493367847,0,0,1.932598943214,0,0,
     &    0,-0.131159328276,0,
     & 837,0,0,1,74.100118953367,0,0,2.764381909621,0,0,
     &    0,-1.882379699337,0,
     & 840,0,0,1,0.383493367847,0,0,2.369671350752,0,0,
     &    0,-0.131159328276,0,
     & 842,0,0,1,0.383493367847,0,0,2.368733660992,0,0,
     &    0,-0.131159328276,0,
     & 901,0,0,1,1.369098120116,0,0,2.305449323924,0,0,
     &    0,-0.706453868101,0,
     & 920,0,0,1,0.682168963277,0,0,2.08187586701,0,0,
     &    0,-0.485308401391,0,
     & 922,0,0,1,0.682168963277,0,0,2.200140357137,0,0,
     &    0,-0.485308401391,0,
     & 931,0,0,1,1.369098120116,0,0,2.196778494487,0,0,
     &    0,-0.706453868101,0,
     & 935,0,0,1,1.369098120116,0,0,2.270668606911,0,0,
     &    0,-0.706453868101,0,
     & 950,210,0,1,6.084887866781,0,0,0.871949820801,0,0,
     &    0,-0.41310410528,0,
     & 950,0,0,1,1.90839636015,0,0,0.800356147485,0,0,
     &    0,-0.040441697762,0,
     & 951,0,0,1,1.369098120116,0,0,2.134589487739,0,0,
     &    0,-0.706453868101,0,
     & 952,0,0,1,1.369098120116,0,0,2.006987858329,0,0,
     &    0,-0.706453868101,0,
     & 970,230,0,1,0.044396757312,0,0,1.984376755703,0,0,
     &    0,0.349578528761,0,
     & 970,0,0,1,0.02067089529,0,0,1.895260515595,0,0,0,0.57370371029,0,
     & 972,210,0,1,0.253789985237,0,0,1.622158362726,0,0,
     &    0,-0.050499883357,0,
     & 972,0,0,1,0.066987852328,0,0,1.499037434737,0,0,
     &    0,0.36817745849,0/

       DATA ((SPcoef(I1,J1),J1=1,13),I1=201,201) /
     & 999,0,0,1,0.108882364946,0,0,1.689140367688,0,0,
     &    0,0.357235092852,0/


       DATA ((JKcoef(I1,J1),J1=1,5),I1=1, 9) /
     & 1,1,0.533879162094,1.406444117599,0.140813780321,
     & 2,1,5.283087706773,1.9638662772,-0.627486802388,
     & 3,1,1.166751266951,1.870489765999,-0.184122364347,
     & 4,1,2.907354607175,2.427910288447,-0.827824824437,
     & 5,1,4.462024983392,2.337033381186,-0.72547304977,
     & 6,1,0.19413556879,1.755572237767,-0.022224992876,
     & 7,1,0.308712403616,1.614565842869,0.011692232809,
     & 8,1,0.224842334323,1.842407924212,-0.071910485099,
     & 9,1,0.294576339481,1.681051225941,0.084742743371/