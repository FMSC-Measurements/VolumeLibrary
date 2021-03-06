! This is from FIA package code NIMS_VOL_FIA_NC_PKB
! HAHN_CU - CUBIC VOLUME TO 4 INCH TOP --CU000055
! HAHN_MCU - CUBIC SAWLOG VOLUME       --CU000056
! HAHN_BD - BOARD FOOT VOLUME          --BD000043
! The coefficients and equation model in this FIA package code is different
! from Hahn's paper NC-250. 
! Not sure how FIA get this equations and nobody knows. I just added as is. (YW 7/26/2018)
!
! NVEL equation number as:
! N01HAH0068  N01HAH0129  N01HAH0132  N01HAH0221  N01HAH0261
! N01HAH0313  N01HAH0316  N01HAH0318  N01HAH0375  N01HAH0402
! N01HAH0407  N01HAH0531  N01HAH0544  N01HAH0602  N01HAH0621
! N01HAH0701  N01HAH0731  N01HAH0742  N01HAH0746  N01HAH0802
! N01HAH0830  N01HAH0833  N01HAH0835  N01HAH0922  N01HAH0972
      SUBROUTINE HAHN_VOL(VOLEQ,DBH,MTOPP,BFMIND,SI,VOL,ERRFLG)
      REAL DBH, MTOPP, BFMIND, VOL(15)
      INTEGER SPN, VOLSP(25), SI
      CHARACTER*10 VOLEQ
      REAL CUCOEF(25,5),TOPCOEF(25,2),BDCOEF(25,5)
      REAL A1, A2, A3, A4, B1, C1, C2, C3, C4
      REAL CV4, BD, TOP, TOPLEN, TOPVOL, SAWCU
      INTEGER CNT, IDX, ERRFLG, I,J
      DATA VOLSP/ 
     &  68, 129, 132, 221, 261, 313, 316, 318, 375, 402, 
     & 407, 531, 544, 602, 621, 701, 731, 742, 746, 802, 
     & 830, 833, 835, 922, 972/    
!     Hahn cubic foot coefficients     
      DATA ((CUCOEF(J,I), I=1,5), J=1,25) /
     &  68,112.59,0.12504,-0.00010633,2.626,
     & 129,122.77,0.41477,-0.00002397,2.7239,
     & 132,122.58,0.20684,-0.000057669,2.7719,
     & 221,337.22,0.11585,-8.18E-06,3.2004,
     & 261,454.13,0.13672,-0.000025473,2.6247,
     & 313,94.985,0.24089,-0.000049457,2.8454,
     & 316,431.19,0.019385,-0.000046628,2.6776,
     & 318,118.8,0.21064,-0.000071839,2.7238,
     & 375,94.985,0.24089,-0.000049457,2.8454,
     & 402,60.548,0.29655,-0.000065367,2.8739,
     & 407,84.154,0.2452,-0.000064831,2.8221,
     & 531,194.75,0.17455,-0.000045078,2.732,
     & 544,73.722,0.22835,-0.000080785,2.7999,
     & 602,137.32,0.13993,-0.000074308,2.6999,
     & 621,232.88,0.20935,-0.000033792,2.7596,
     & 701,94.985,0.24089,-0.000049457,2.8454,
     & 731,326.3,0.19774,-0.000032404,2.6696,
     & 742,479.95,0.11037,-0.000030495,2.6521,
     & 746,61.217,0.28402,-0.00011838,2.631,
     & 802,138.51,0.17688,-0.00007041,2.6666,
     & 830,151.65,0.25967,-0.000044082,2.672,
     & 833,167.98,0.28275,-0.000046166,2.5895,
     & 835,146.07,0.24217,-0.000052693,2.6322,
     & 922,39.018,0.38544,-0.000087759,2.7685,
     & 972,191.45,0.16087,-0.000042091,2.7469/
!    --Taper coefficients for top volume
      DATA ((TOPCOEF(J,I), I=1,2), J=1,25) /
     &  68,	0.35933,
     & 129,	0.29204,
     & 132,	0.30131,
     & 221,	0.30797,
     & 261,	0.28516,
     & 313,	0.28367,
     & 316,	0.30461,
     & 318,	0.30171,
     & 375,	0.28367,
     & 402,	0.29118,
     & 407,	0.28686,
     & 531,	0.35571,
     & 544,	0.31065,
     & 602,	0.30397,
     & 621,	0.31615,
     & 701,	0.28367,
     & 731,	0.30304,
     & 742,	0.3494,
     & 746,	0.31359,
     & 802,	0.33464,
     & 830,	0.33531,
     & 833,	0.35216,
     & 835,	0.32485,
     & 922,	0.25003,
     & 972,	0.31607/
!     Hahn board foot volume coefficeints
      DATA ((BDCOEF(J,I), I=1,5), J=1,25) /
     &  68,629.29,0.1133,-0.0002294,2.3101,
     & 129,925.49,0.26403,-0.000018181,2.9261,
     & 132,815.53,0.13267,-0.000041692,2.9311,
     & 221,859.71,0.28961,-3.44E-06,3.4958,
     & 261,915.49,0.27188,-0.000034362,2.6441,
     & 313,648.08,0.20082,-0.000027844,3.0139,
     & 316,2373.9,0.029109,-0.000023232,2.8795,
     & 318,585.04,0.25898,-0.000036843,2.9252,
     & 375,648.08,0.20082,-0.000027844,3.0139,
     & 402,394.51,0.32714,-0.000033027,2.9987,
     & 407,731.89,0.22652,-0.000031279,2.9264,
     & 531,1325.2,0.17482,-0.000028034,2.8268,
     & 544,418.9,0.23001,-0.000033227,3.0904,
     & 602,527.23,0.20734,-0.000043098,2.9185,
     & 621,569.15,0.34972,-0.000017671,3.0633,
     & 701,648.08,0.20082,-0.000027844,3.0139,
     & 731,819.4,0.34016,-0.000016264,2.9545,
     & 742,916.03,0.22828,-0.000020292,2.9949,
     & 746,116.99,0.37141,-0.00003645,3.3609,
     & 802,749.07,0.22139,-0.000048526,2.7286,
     & 830,904.22,0.22289,-0.000028079,2.8615,
     & 833,744.15,0.26268,-0.000034871,2.8042,
     & 835,416.12,0.40012,-0.000038622,2.7496,
     & 922,103.92,0.57448,-0.000039898,2.9957,
     & 972,751.36,0.28452,-0.00002692,2.8188/
        
      IF(SI.LT.10) SI = 65
      READ(VOLEQ(8:10),'(I3)')SPN
      CNT = 25
      IDX = 0
      CALL SEARCH_SP(CNT,VOLSP,SPN,IDX,ERRFLG)
      IF(IDX.LE.0) THEN
        ERRFLG = 6
        RETURN
      ENDIF
      
      IF(IDX.GT.0.AND.SPN.EQ.CUCOEF(IDX,1)) THEN
        A1 = CUCOEF(IDX,2)
        A2 = CUCOEF(IDX,3)
        A3 = CUCOEF(IDX,4)
        A4 = CUCOEF(IDX,5)
!        IF(DBH.LT.5.0) DBH = 5.0
        CV4 = A1*SI**A2*(1-EXP(A3*DBH**A4))
        VOL(4) = CV4
        IF(SPN.LT.300)THEN
          IF(BFMIND.LT.0.1) BFMIND = 9.0
          IF(MTOPP.LT.0.1) MTOPP = 7.0
        ELSE
          IF(BFMIND.LT.0.1) BFMIND = 11.0
          IF(MTOPP.LT.0.1) MTOPP = 9.0
        ENDIF
        IF(DBH.GE.BFMIND)THEN
          B1 = TOPCOEF(IDX,2)
          C1 = BDCOEF(IDX,2)
          C2 = BDCOEF(IDX,3)
          C3 = BDCOEF(IDX,4)
          C4 = BDCOEF(IDX,5)  
          TOP = MTOPP
          TOPLEN = (TOP-4.0)/B1
          TOPVOL = TOPLEN/2*.005454*(TOP**2+4**2)
          SAWCU = CV4 - TOPVOL
          VOL(7) = TOPVOL
          VOL(4) = SAWCU
          BD = C1*SI**C2*(1-EXP(C3*DBH**C4))
          VOL(10) = BD
        ENDIF
      ENDIF
      RETURN
      END SUBROUTINE HAHN_VOL
C ------------------------------------------------------------------
C Stone's eqn Res. Pap. NC-250, Appendix
C STONE_CU  --CU000054
C STONE_BD  --BD000042
!
! NVEL equation number as:
! N01STN0012  N01STN0068  N01STN0071  N01STN0094  N01STN0095
! N01STN0105  N01STN0125  N01STN0129  N01STN0132  N01STN0221
! N01STN0241  N01STN0261  N01STN0313  N01STN0316  N01STN0318
! N01STN0371  N01STN0373  N01STN0375  N01STN0402  N01STN0407
! N01STN0462  N01STN0531  N01STN0543  N01STN0544  N01STN0602
! N01STN0611  N01STN0621  N01STN0691  N01STN0701  N01STN0731
! N01STN0742  N01STN0743  N01STN0746  N01STN0762  N01STN0802
! N01STN0830  N01STN0833  N01STN0834  N01STN0835  N01STN0901
! N01STN0951  N01STN0972  N01STN0999  
      SUBROUTINE STONE_VOL(VOLEQ,DBHOB,MTOPP,SI,BA,VOL,ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,DBH,HTTOT,MHT,MTOPP, VOL(15)
      REAL D1,D2,D3,D4,D5,D6,E1,E2,HTCOEF(43,7),BKCOEF(43,3)
      INTEGER SPN,ERRFLG,IDX,CNT,VOLSP(43),I,J,SI,BA
      REAL TCU,MCU,BD,BFMIND,THT
      DATA VOLSP/
     &  12,68,71,94,95,105,125,129,132,221,
     & 241,261,313,316,318,371,373,375,402,407,
     & 462,531,543,544,602,611,621,691,701,731,
     & 742,743,746,762,802,830,833,834,835,901,
     & 951,972,999/
!     --Height to variable top coefficients used in Stone equations
       DATA ((HTCOEF(J,I), I=1,7), J=1,43) /
     & 12,14.304,0.19894,1.4195,0.23349,0.76878,0.12399,
     & 68,8.2079,0.19672,1.3112,0.33978,0.76173,0.11666,
     & 71,13.62,0.24255,1.2885,0.25831,0.68128,0.10771,
     & 94,31.957,0.18511,1.702,0,0.68967,0.162,
     & 95,20.038,0.18981,1.2909,0.17836,0.57343,0.10159,
     & 105,16.934,0.12972,1,0.20854,0.77792,0.12902,
     & 125,36.851,0.08298,1,0.00001,0.63884,0.18231,
     & 129,16.281,0.08621,1,0.1622,0.86833,0.23316,
     & 132,36.851,0.08298,1,0.00001,0.63884,0.18231,
     & 221,16.281,0.08621,1,0.1622,0.86833,0.23316,
     & 241,8.2079,0.19672,1.3112,0.33978,0.76173,0.11666,
     & 261,6.077,0.2495,3.8663,0.5018,0.8244,0.0482,
     & 313,6.9572,0.26564,1,0.4866,0.76954,0.01617,
     & 316,6.86,0.27725,1.4287,0.40115,0.85299,0.12403,
     & 318,5.3416,0.23044,1.1529,0.54194,0.8344,0.06372,
     & 371,7.1852,0.28384,1.4417,0.38884,0.82157,0.11411,
     & 373,7.2773,0.22721,1,0.41179,0.76498,0.11046,
     & 375,7.2773,0.22721,1,0.41179,0.76498,0.11046,
     & 402,6.1034,0.17368,1,0.44725,1.0237,0.1461,
     & 407,6.1034,0.17368,1,0.44725,1.0237,0.1461,
     & 462,6.86,0.27725,1.4287,0.40115,0.85299,0.12403,
     & 531,7.1852,0.28384,1.4417,0.38884,0.82157,0.11411,
     & 543,11.291,0.2525,1.5466,0.35711,0.7506,0.06859,
     & 544,8.1782,0.27316,1.725,0.38694,0.75822,0.10847,
     & 602,6.3628,0.27859,1.8677,0.49589,0.76169,0.05841,
     & 611,5.3416,0.23044,1.1529,0.54194,0.8344,0.06372,
     & 621,6.3628,0.27859,1.8677,0.49589,0.76169,0.05841,
     & 691,5.3416,0.23044,1.1529,0.54194,0.8344,0.06372,
     & 701,6.9572,0.26564,1,0.4866,0.76954,0.01617,
     & 731,6.3628,0.27859,1.8677,0.49589,0.76169,0.05841,
     & 742,13.625,0.28668,1.6124,0.30651,1.0292,0.07461,
     & 743,5.5346,0.22637,1,0.46918,0.72456,0.11782,
     & 746,6.4301,0.23545,1.338,0.4737,0.73385,0.08228,
     & 762,5.3416,0.23044,1.1529,0.54194,0.8344,0.06372,
     & 802,9.2078,0.22208,1,0.31723,0.8256,0.13465,
     & 830,3.8011,0.39213,2.9053,0.55634,0.84317,0.09593,
     & 833,6.6844,0.19049,1,0.43972,0.82962,0.10806,
     & 834,6.6844,0.19049,1,0.43972,0.82962,0.10806,
     & 835,9.2078,0.22208,1,0.31723,0.8256,0.13465,
     & 901,6.9572,0.26564,1,0.4866,0.76954,0.01617,
     & 951,6.3628,0.27859,1.8677,0.49589,0.76169,0.05841,
     & 972,8.458,0.27527,1.9602,0.34894,0.89213,0.12594,
     & 999,6.9572,0.26564,1,0.4866,0.76954,0.01617/
!!--Bark coefficients used in Stone     
      DATA ((BKCOEF(J,I), I=1,3), J=1,43) /
     & 12,99.92,0,
     & 68,110.32,0.6,
     & 71,105.52,0,
     & 94,105.52,0,
     & 95,95.77,0.5,
     & 105,86.77,0.85,
     & 125,101.79,-0.55,
     & 129,95.88,0,
     & 132,79.1,0.6,
     & 221,79.1,0.6,
     & 241,103.43,0,
     & 261,84.68,0.62,
     & 313,82.6,0.6,
     & 316,102.19,0,
     & 318,104.42,-0.16,
     & 371,102.41,0,
     & 373,83.62,0.6,
     & 375,98.21,0.2,
     & 402,85.09,0.6,
     & 407,85.18,0.6,
     & 462,98.95,0,
     & 531,111.73,-0.23,
     & 543,98.44,0,
     & 544,83,0.6,
     & 602,114.84,-0.88,
     & 611,82.52,0.6,
     & 621,90.23,0.6,
     & 691,81.01,0.6,
     & 701,84.71,0.6,
     & 731,93.83,0.6,
     & 742,82.5,0.6,
     & 743,91.51,0.37,
     & 746,91.51,0.37,
     & 762,114.84,-0.88,
     & 802,83.95,0.6,
     & 830,84.59,0.6,
     & 833,84.21,0.6,
     & 834,84.21,0.6,
     & 835,84.24,0.6,
     & 901,82.6,0.6,
     & 951,97.07,0,
     & 972,98.95,0,
     & 999,84.71,0.6/
      HT(DBH,TOP,D1,D2,D3,D4,D5,D6,SI,BA) = 4.5 +
     & D1*(1.0-(EXP(-D2*DBH)))**D3*SI**D4*(1.00001-TOP/DBH)**D5*BA**D6
      V(DBH,MHT,TOP) =
     &    (.0030086+.0020355*DBH-.0030018*TOP+.000062381*DBH**2+
     &     .000025705*DBH**2*MHT-.0000070090*MHT**2+
     &     .000036708*MHT*TOP**2+.00000000081400*DBH**2*MHT**3
     &     -.0000000019*DBH**2*MHT**2*TOP)*79;
     
      IF(SI.LT.10) SI = 65
      IF(BA.LT.10) BA = 80
      READ(VOLEQ(8:10),'(I3)')SPN
      CNT = 43
      IDX = 0
      CALL SEARCH_SP(CNT,VOLSP,SPN,IDX,ERRFLG)
      IF(IDX.LE.0) THEN
        ERRFLG = 6
        RETURN
      ENDIF
      
      DBH = DBHOB
      IF(SI.LT.20)THEN
        SI = 20
      ELSEIF(SI.GT.120)THEN
        SI = 120
      ENDIF
      IF(BA.LT.50)THEN
        BA = 50
      ELSEIF(BA.GT.350)THEN
        BA = 350
      ENDIF
!      IF(DBH<5) DBH = 5
      IF(IDX.GT.0.AND.SPN.EQ.HTCOEF(IDX,1)) THEN
        D1 = HTCOEF(IDX,2)
        D2 = HTCOEF(IDX,3)
        D3 = HTCOEF(IDX,4)
        D4 = HTCOEF(IDX,5)
        D5 = HTCOEF(IDX,6)
        D6 = HTCOEF(IDX,7)
        E1 = BKCOEF(IDX,2)
        E2 = BKCOEF(IDX,3)
!       Total cubic volume from stump to tip        
        TOP = 0.0
        THT = HT(DBH,TOP,D1,D2,D3,D4,D5,D6,SI,BA)
        TCU = V(DBH,THT,TOP)
!       --adjust for bark difference   
        TCU = TCU*(E1+E2*DBH)/100
        VOL(1) = TCU
!       Total cubic volume from stump to 4 INCH TOP  
        TOP = 4.0
        MHT = HT(DBH,TOP,D1,D2,D3,D4,D5,D6,SI,BA)
        MCU = V(DBH,MHT,TOP)
!       --adjust for bark difference   
        MCU = MCU*(E1+E2*DBH)/100
        VOL(4) = MCU
              
        IF(SPN.LT.300)THEN
          IF(MTOPP.LT.4.0) MTOPP = 7.0
          BFMIND = 9.0
        ELSE
          IF(MTOPP.LT.4.0) MTOPP = 9.0
          BFMIND = 11.0
        ENDIF
        TOP = MTOPP
        IF(DBH.GE.BFMIND)THEN
            MHT = HT(DBH,TOP,D1,D2,D3,D4,D5,D6,SI,BA)
            MCU = V(DBH,MHT,TOP)
!       --adjust for bark difference   
            MCU = MCU*(E1+E2*DBH)/100
            BD = 17.7488+7.3846*MCU-2.3523*DBH-.89945*MHT+2.0726*TOP
            IF(BD.LT.0) BD = 0
            VOL(7) = VOL(4) - MCU
            VOL(4) = MCU
            VOL(10) = BD
        ENDIF
      ENDIF
      RETURN
      END SUBROUTINE STONE_VOL
!-------------------------------------------------------------------
! ------------------------------------------------------------------      
C  This Subroutine searches an array to find the maching element
      SUBROUTINE SEARCH_SP(M,ARY,SPC,DONE,ERRFLAG)
      INTEGER M,I
      INTEGER ARY(M)
      INTEGER SPC,LOC,FIRST,LAST,HALF,DONE,ERRFLAG
      FIRST=1
      LAST=M
      DONE=0
      DO 5 I = FIRST, LAST
        IF(ARY(I).EQ.SPC)THEN
          DONE = I
          EXIT
        ENDIF
  5   CONTINUE 
  
      RETURN
      END       