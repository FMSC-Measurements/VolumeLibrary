C WARNING! The codes were created by NBEL Tester program. Please do NOT modify
C Last modified on 7/17/2015
C This sunroutine is generated from NBEL Tester for selected biomass equations
       SUBROUTINE CalcBiomass(BEQ, DBH, THT, P3, BMS)
       CHARACTER*12 BEQ
       REAL DBH, THT, P3, BMS, a,b,c,d,e
       INTEGER EqForm
       CHARACTER*5 diaUnit, BmsUnit,HtUnit,diaType
       
       BMS = 0
       a = 0
       b = 0
       c = 0
       d = 0
       e = 0
       IF(LEN(BEQ).EQ.0) THEN 
         BMS = 0
       ELSEIF(BEQ.EQ.'WY2015TIP01G') THEN
         EqForm = 4
         a = 39.61376
         c = 0.350321
         d = 0.8841
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'WY2015CRW01G') THEN
         EqForm = 16
         a = 4.833
         b = 1.7883
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'WY2096TIP01G') THEN
         EqForm = 4
         a = 84.60268
         c = -3.75349
         d = 1.0798
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'WY2096CRW01G') THEN
         EqForm = 16
         a = 1.5978
         b = 2.1371
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'WY2122TIP01G') THEN
         EqForm = 4
         a = 25.45383
         c = 4.810467
         d = 0.728
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'WY2122CRW01G') THEN
         EqForm = 17
         a = 468.7
         b = -109.56
         c = 7.0873
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'WY2202TIP01G') THEN
         EqForm = 4
         a = 55.33516
         c = -3.09924
         d = 0.8087
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'WY2202CRW01G') THEN
         EqForm = 16
         a = 0.577
         b = 2.5765
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'TNK122AGT01D') THEN
         EqForm = 4
         DBH = DBH*2.54
         THT = THT*0.3048
         DBH = DBH * DBH * THT
         a = 20.283
         b = 0.018849
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
         BMS = BMS * 2.204623
       ELSEIF(BEQ.EQ.'STD122AST01D') THEN
         EqForm = 11
         DBH = DBH*0.0254
         THT = THT*0.3048
         a = 1.6
         b = 187
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
         BMS = BMS * 2.204623
       ELSEIF(BEQ.EQ.'BRN017LCR01D') THEN
         EqForm = 2
         a = 1.0144
         c = 1.6156
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN017LCR02D') THEN
         EqForm = 2
         a = 1.3094
         c = 1.6076
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN017LCR03D') THEN
         EqForm = 15
         b = 0.4284
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN017LCR04D') THEN
         EqForm = 15
         c = 0.0538
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN019LCR01D') THEN
         EqForm = 4
         a = 7.345
         c = 1.255
         d = 2
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN019LCR02D') THEN
         EqForm = 12
         a = -3.335
         c = 2.303
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN073LCR01D') THEN
         EqForm = 2
         a = 0.4373
         c = 1.6786
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN073LCR02D') THEN
         EqForm = 15
         b = 0.1128
         c = 0.00813
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN093LCR01D') THEN
         EqForm = 2
         a = 1.0404
         c = 1.7096
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN093LCR02D') THEN
         EqForm = 12
         a = -3.932
         c = 2.571
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN101LCR01D') THEN
         EqForm = 4
         a = -1
         c = 0.8371
         d = 2
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN101LCR02D') THEN
         EqForm = 15
         a = 0.07
         c = 0.02446
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN108LCR01D') THEN
         EqForm = 2
         a = 0.1224
         c = 1.882
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN108LCR02D') THEN
         EqForm = 15
         c = 0.03111
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN119LCR01D') THEN
         EqForm = 2
         a = 0.7276
         c = 1.5497
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN119LCR02D') THEN
         EqForm = 15
         b = 0.3292
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN119LCR03D') THEN
         EqForm = 11
         a = 3.65
         b = 0.01233
         c = -0.04534
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN122LCR01D') THEN
         EqForm = 2
         a = 0.268
         c = 2.074
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN122LCR02D') THEN
         EqForm = 2
         a = -0.7572
         c = 2.216
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN122LCR03D') THEN
         EqForm = 15
         b = 0.3451
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN122LCR04D') THEN
         EqForm = 12
         a = -2.7297
         c = 1.1707
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN202LCR01D') THEN
         EqForm = 2
         a = 1.1368
         c = 1.5819
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN202LCR02D') THEN
         EqForm = 2
         a = 0.1508
         c = 1.862
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN202LCR03D') THEN
         EqForm = 4
         a = -20.74
         c = 1.0237
         d = 2
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN202LCR04D') THEN
         EqForm = 12
         a = -4.212
         c = 2.7168
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN202LCR05D') THEN
         EqForm = 12
         a = -2.8065
         c = 1.4802
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN202LCR06D') THEN
         EqForm = 4
         a = 10.9
         b = -11.34
         c = 4.059
         d = 2
         e = -0.03283
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN242LCR01D') THEN
         EqForm = 2
         a = 0.8815
         c = 1.6389
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN242LCR02D') THEN
         EqForm = 2
         a = 0.5743
         c = 1.796
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN242LCR03D') THEN
         EqForm = 15
         c = 0.04833
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN242LCR04D') THEN
         EqForm = 15
         c = 0.0307
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN263LCR01D') THEN
         EqForm = 2
         a = 0.7218
         c = 1.7502
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN263LCR02D') THEN
         EqForm = 12
         a = -5.126
         c = 2.563
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN263LCR03D') THEN
         EqForm = 5
         a = 3.6
         c = -1.545
         d = 0.01734
         e = 0.388
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN263LCR04D') THEN
         EqForm = 2
         a = -0.4691
         c = 2.165
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN017DCR01D') THEN
         EqForm = 2
         a = -2.6214
         c = 2.5492
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN017DCR02D') THEN
         EqForm = 2
         a = -5.3154
         c = 3.5638
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN017DCR03D') THEN
         EqForm = 11
         a = 1.36
         b = -0.003564
         c = 0.04371
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN019DCR01D') THEN
         EqForm = 2
         a = -6.5431
         c = 4.0365
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN093DCR01D') THEN
         EqForm = 2
         a = -6.686
         c = 3.6172
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN101DCR01D') THEN
         EqForm = 4
         c = 0.06117
         d = 2
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN101DCR02D') THEN
         EqForm = 11
         a = 0.28
         b = 0.001397
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN119DCR01D') THEN
         EqForm = 2
         a = -4.397
         c = 2.6076
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN122DCR01D') THEN
         EqForm = 2
         a = -3.7398
         c = 2.8376
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN122DCR02D') THEN
         EqForm = 2
         a = -2.5176
         c = 2.51
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN202DCR01D') THEN
         EqForm = 2
         a = -1.928
         c = 2.353
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN202DCR02D') THEN
         EqForm = 4
         c = 0.01094
         d = 3
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN242DCR01D') THEN
         EqForm = 4
         c = 0.01063
         d = 3
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN242DCR02D') THEN
         EqForm = 2
         a = -2.799
         c = 2.4942
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN263DCR01D') THEN
         EqForm = 2
         a = -6.6768
         c = 3.3664
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN263DCR02D') THEN
         EqForm = 12
         a = -19.34
         b = -2.0496
         c = 6.0111
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN017STT01D') THEN
         EqForm = 5
         a = 0.62
         c = 0.8024
         d = 0.1724
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN017STT02D') THEN
         EqForm = 4
         a = -1.63
         c = 2.172
         d = 2
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN017STT03D') THEN
         EqForm = 11
         a = 0.34
         b = 0.09182
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN019STT01D') THEN
         EqForm = 5
         a = 1.55
         d = 0.414
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN019STT02D') THEN
         EqForm = 11
         a = 0.28
         b = 0.02692
         d = 0.1912
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN073STT01D') THEN
         EqForm = 5
         a = 0.96
         d = 0.6532
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN073STT02D') THEN
         EqForm = 4
         a = 0.65
         d = 0
         e = 0.1004
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN093STT01D') THEN
         EqForm = 2
         a = 0.8381
         c = 1.3803
         d = 1
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN093STT02D') THEN
         EqForm = 3
         a = 0.7839
         b = 1.29
         d = 0
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN101STT01D') THEN
         EqForm = 4
         a = 0.52
         c = 1.441
         d = 2
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN101STT02D') THEN
         EqForm = 4
         a = 1.33
         d = 0
         e = 0.08614
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN108STT01D') THEN
         EqForm = 4
         a = 1.49
         b = -2.388
         c = 2.297
         d = 2
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN119STT01D') THEN
         EqForm = 5
         a = 1.15
         d = 0.5297
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN122STT01D') THEN
         EqForm = 4
         a = 1.08
         c = 0.9361
         d = 2
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN122STT02D') THEN
         EqForm = 5
         a = 0.74
         d = 0.4006
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN122STT03D') THEN
         EqForm = 4
         a = 0.2
         d = 0
         e = 0.07058
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN122STT04D') THEN
         EqForm = 4
         a = 0.56
         c = 1.1029
         d = 2
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN202STT01D') THEN
         EqForm = 4
         a = 0.74
         c = 1.591
         d = 2
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN202STT02D') THEN
         EqForm = 4
         a = -0.88
         c = 2.234
         d = 2
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN242STT01D') THEN
         EqForm = 5
         b = 1.436
         d = 0.3326
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN242STT02D') THEN
         EqForm = 4
         a = 0.52
         c = 1.35
         d = 2
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN263STT01D') THEN
         EqForm = 4
         a = 0.11
         c = 1.665
         d = 2
         e = 0
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ELSEIF(BEQ.EQ.'BRN263STT02D') THEN
         EqForm = 4
         a = 0.31
         b = 0.8334
         d = 0
         e = 0.06819
         CALL BiomassFormula(EqForm,DBH,THT,P3,a,b,c,d,e,BMS)
       ENDIF
       END
