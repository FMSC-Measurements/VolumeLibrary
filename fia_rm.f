      SUBROUTINE FIA_RM(BEQ, DBHOB, HTTOT, STEMS, VOL, BMS)
      CHARACTER(12) BEQ
      REAL DBHOB, HTTOT, BMS, DBH, HT
      REAL VOL(15), WD_DEN(15), BK_RAT(15), WT2(15), WT4(15), SG(13)
      INTEGER SPN,SPLIST1(15),SPLIST2(13),DONE,LAST,ERRFLG,STEMS,I
      REAL BOLE_WD,BOLE_BK,WDDEN,BKRAT,BOLE,TOP_LIMB,K,ABOVE_STUMP 
      REAL SPSG, Bio3_M, WT_FOL, WT_BRA, WT_FOL_M
! VALID EQUATION NUMBER:
! FROM Van Hooser and Chojnacky 1983. INT-29
! FRM***WB101D, FRM***MST01D, FRM***MSW01D, FRM***MSB01D, FRM***BTP01D
! WHERE *** IS 15, 17, 19, 73, 93,108, 119, 122, 202, 242, 263, 299, 746, 740, 999
! FRM202WB102D, FRM202MST02D, FRM202MSW02D, FRM202MSB02D, FRM202BTP02D
! FRM999WB102D, FRM999MST02D, FRM999MSW02D, FRM999MSB02D, FRM999BTP02D
! FROM Chojnacky CV TO 3" TOP
! FRM***WB101D, FRM***MST01D, FRM***AST01D, FRM***BRT01D, FRM***FOT01D
! FROM Chojnacky CV TO 1.5" TOP
! FRM***WB102D, FRM***MST02D, FRM***AST02D, FRM***BRT02D, FRM***FOT02D
! WHERE *** IS 57,63,65,69,106,310,475,755,756,757,758,810,814
      DATA (SPLIST1(I),I=1,15)/15, 17, 19, 73, 93, 
     + 108, 119, 122, 202, 242, 263, 299, 746, 740, 999/
      DATA (WD_DEN(I),I=1,15)/23.09,21.84,19.34,29.95,20.59,
     + 23.71,21.84,23.71,28.08,19.34,26.21,23.71,21.84,19.34,24.96/
      DATA (BK_RAT(I),I=1,15)/0.18,0.18,0.18,0.10,0.13,
     + 0.08,0.16,0.19,0.15,0.10,0.13,0.10,0.27,0.27,0.27/
      DATA (WT2(I),I=1,15)/10,10,11,10,12,10,9,10,11,9,10,10,4,4,4/
      DATA (WT4(I),I=1,15)/43,43,43,50,43,40,42,40,49,38,38,43,19,19,19/
      DATA (SPLIST2(I),I=1,13)/57,63,65,69,106,
     + 310,475,755,756,757,758,810,814/
      DATA (SG(I),I=1,13)/0.533,0.517,0.523,0.558,0.496,
     + 0.65,0.7,0.69,0.69,0.69,0.69,0.567,0.634/

      READ(BEQ(4:6),'(I3)') SPN
      LAST = 15
      DONE = 0
      DBH = DBHOB
      HT = HTTOT
      WT_BRA = 0.0
      WT_FOL = 0.0

C     First check the species for in the SPLIST1
      CALL SEARCH(LAST,SPLIST1,SPN,DONE,ERRFLG)
      IF(DONE.GT.0) THEN
        IF(VOL(4).GT.0.AND. DBHOB.GE.5) THEN
C         For species in most locations
          IF(BEQ(10:11).EQ.'01') THEN
            BOLE_WD = VOL(4)*WD_DEN(DONE)
            BOLE_BK = BOLE_WD*BK_RAT(DONE)
C         For Douglas-fir and other hardwood species in CO, UT, 
          ELSE
            IF(SPN.EQ.202)THEN
              WDDEN = 26.83
              BKRAT = 0.15
            ELSE
              WDDEN = 34.32
              BKRAT = 0.27
            ENDIF
            BOLE_WD = VOL(4)*WDDEN
            BOLE_BK = BOLE_WD*BKRAT
          ENDIF
          BOLE = BOLE_WD + BOLE_BK
          IF(SPN.EQ.122.OR.SPN.EQ.119.OR.(SPN.EQ.108.AND.DBH.LT.14))THEN
            TOP_LIMB = 193.5-43.5412*DBH+3.1659*DBH**2
          ELSEIF(SPN.LT.300) THEN
            TOP_LIMB = 0.191+2.0304*DBH+0.7031*DBH**2
          ELSE
            K = EXP((-1.*(ABS((((DBH/27.56)-1.)/.56))**3.5)))
            TOP_LIMB = (129.69*K-.05)*(2.2046)
          ENDIF
          ABOVE_STUMP = BOLE + TOP_LIMB
        ELSEIF(DBH.GE.3) THEN
          ABOVE_STUMP = WT4(DONE);
        ELSEIF(DBH.GE.1) THEN
          ABOVE_STUMP = WT2(DONE)
        ENDIF
C      
C       Return the biomass for the BEQ       
C       Stem and branches wood and bark 
        IF(BEQ(7:9).EQ.'WB1')THEN
          BMS = ABOVE_STUMP
C       Merch stem total (wood and bark)
        ELSEIF(BEQ(7:9).EQ.'MST')THEN
          BMS = BOLE
C       Merch stem wood only
        ELSEIF(BEQ(7:9).EQ.'MSW')THEN
          BMS = BOLE_WD
C       Merch stem bark only
        ELSEIF(BEQ(7:9).EQ.'MSB')THEN
          BMS = BOLE_BK
C       Top limb (branched plus stem tip)
        ELSEIF(BEQ(7:9).EQ.'BTP')THEN
          BMS = TOP_LIMB 
        ENDIF
      ELSE
C     Check species in the SPLIST2 (woodland species)
        LAST = 13
        DONE = 0
        CALL SEARCH(LAST,SPLIST2,SPN,DONE,ERRFLG)
        IF(DONE.GT.0)THEN
          SPSG = SG(DONE)
C       First check if the input vol(4) is the volume to 1.5 inch branch diameter. 
C       This is the volume calculated from Chojnacky (1985) equation. In this case,
C       the VOL(4) is set to be same as VOL(1)
C          IF(VOL(4).EQ.VOL(1))THEN
C       CHANGED TO USE EQUATION NUMBER 02 FOR USING CV FOR 1.5" TOP.(YW 2019/05/20)
          IF(BEQ(10:11).EQ.'01') THEN
            BIO3 = VOL(4)*SPSG*62.4
          ELSEIF(BEQ(10:11).EQ.'02') THEN
            IF(VOL(1).EQ.0) VOL(1) = VOL(4)
            CALL CHO_WDBK_1530(SPN,DBH,HTTOT,VOL(1),STEMS,SPSG,BIO3)
          ENDIF
          BIO3_M = BIO3/2.2046
          IF(SPN.GE.300)THEN
            WT_FOL = 10**(-.5655+(.8382*(LOG10(BIO3)))+(-.0094*HT))
            WT_BRA = 10**(.3036+(.7752*(LOG10(BIO3)))+(-.0049*HT))
          ELSE
            IF(SPN.EQ.106)THEN
              IF(BIO3_M.LE.468.028)THEN
               WT_FOL_M = EXP(1.0254+.559*LOG(BIO3_M))
              ELSE
               WT_FOL_M=EXP(1.0254+.559*(1.0+LOG(468.028)
     &         -468.028/BIO3_M))
              ENDIF
            ELSE
              IF(BIO3_M.LE.150.0)THEN
               WT_FOL_M = EXP(1.2867+.649*LOG(BIO3_M))
              ELSE
               WT_FOL_M = EXP(1.2867+.649*(1.0+LOG(150.0)-150.0/BIO3_M))
              ENDIF
            ENDIF
            WT_FOL = WT_FOL_M*2.2046
            IF(BEQ(10:11).EQ.'02') THEN
              WT_BRA = WT_FOL*0.75
              WT_FOL = WT_FOL*0.25
            ENDIF
          ENDIF

C         Return component biomass
C         Above ground wood and bark (stem plus branches)
          IF(BEQ(7:9).EQ.'WB1')THEN
            BMS = BIO3 + WT_BRA
C         Above ground total (stem, branches and foliages)
          ELSEIF(BEQ(7:9).EQ.'AST')THEN
            BMS = BIO3 + WT_BRA + WT_FOL
C         merch stem total TO 1.5"/3.0* TOP DIB
          ELSEIF(BEQ(7:9).EQ.'WB2')THEN
            BMS = BIO3
C         Total branches
          ELSEIF(BEQ(7:9).EQ.'BRT')THEN
            BMS = WT_BRA
C         Foliage total
          ELSEIF(BEQ(7:9).EQ.'FOT')THEN
            BMS = WT_FOL
          ENDIF
        ENDIF
      ENDIF
      END
C ------------------------------------------------------------------------------
C     This routine calculate wood and bark biomass to 3 inch branch diameter with
C     the given volume (CV15) to a 1.5 inch branch diameter
      SUBROUTINE CHO_WDBK_1530(SPN, DRC, HT, CV15, STEMS, SG, BIO3)
      INTEGER SPN, STEMS,LAST, DONE, ERRFLG
      REAL DRC, HT, BIO3, SG, VR, DSH, CV3, CV15 
      
      IF(SPN.EQ.106) THEN
        IF(STEMS.LE.1) THEN
          DSH = -0.03 + (0.9826*DRC) - 0.20
          VR = 1 - (0.27612*(1.5**0.67360))/(DSH**0.21114)
        ELSE
          DSH = -0.30 + (0.9826*DRC)
          VR = 1 -(0.66949*(1.5**0.62895))/(DSH**0.44205)
        ENDIF
      ELSE
        IF(SPN.EQ.57.OR.SPN.EQ.63.OR.SPN.EQ.65.OR.SPN.EQ.69)THEN
          IF(STEMS.LE.1)THEN
            DSH = -0.77 + (0.9603*DRC) + 0.22
            VR = 1 - (0.44761*(1.5**0.65698))/(DSH**0.38835)
          ELSE
            DSH = -0.77 + (0.9603*DRC)
            VR = 1 -(0.82565*(1.5**0.59404))/(DSH**0.45831)
          ENDIF
        ENDIF
      ENDIF
      IF(SPN.EQ.57.OR.SPN.EQ.63.OR.SPN.EQ.65.OR.
     +   SPN.EQ.69.OR.SPN.EQ.106)THEN
        CV3 = CV15*VR
      ELSEIF(SPN.EQ.475)THEN
        CV3 = .00356*(DRC**2.920)
      ELSE
        CV3 = Cv15
      ENDIF
C     Compute small tree volume if none provided
      IF(DRC.GE.1.AND.DRC.LT.5.AND.SPN.NE.475)THEN
        CV3 = .25*.005454*DRC**2*HT
      ENDIF
      BIO3 = SG*CV3*62.4

      END
! ---------------------------------------------------------------------
      !This subroutine calculate biomass for woodland species
      SUBROUTINE WOODLAND_BIO(SPCD, DRC,THT,STEMS,CV15,DRYBIO,ERRFLG)
      IMPLICIT NONE
      INTEGER SPCD, SPN,ERRFLG,DONE,LAST,STEMS,I
      INTEGER SPLIST(13)
      REAL DRC,THT,SPSG,CV15,BIO3,BIO3_M,WT_FOL,WT_BRA,WT_FOL_M,WT_BRA_M
      REAL DRYBIO(15),SG(13)
      DATA (SPLIST(I),I=1,13)/57,63,65,69,106,
     + 310,475,755,756,757,758,810,814/
      DATA (SG(I),I=1,13)/0.533,0.517,0.523,0.558,0.496,
     + 0.65,0.7,0.69,0.69,0.69,0.69,0.567,0.634/

      SPN = SPCD
      DONE = 0
      LAST = 13
      !CV15 = VOL(1)
      !First check if the SPCD is in the woodland species list
      !This is to match FIA BIOMASS_SP configuration
      !IF(SPN.EQ.62.OR.SPN.EQ.66)THEN
      !Added check for species 63 and 65 (2025/05/08)
      IF((SPN.GE.57.AND.SPN.LE.66).AND.(SPN.NE.63.AND.SPN.NE.65))THEN
          SPN = 57
      ELSEIF(SPN.GE.133.AND.SPN.LE.143)THEN
          SPN = 106
      ELSEIF(SPN.EQ.303.OR.SPN.EQ.304.OR.SPN.EQ.363)THEN
          SPN = 757
      ELSEIF(SPN.EQ.321.OR.SPN.EQ.322)THEN
          SPN = 310
      ELSEIF(SPN.EQ.522.OR.SPN.EQ.7532)THEN
          SPN = 475
      ELSEIF(SPN.GE.523.AND.SPN.LE.758)THEN
          SPN = 757
      ELSEIF(SPN.EQ.803)THEN
          SPN = 810
      ELSEIF(SPN.GE.829.AND.SPN.LE.867)THEN
          SPN = 810
      ENDIF
      CALL SEARCH(LAST,SPLIST,SPN,DONE,ERRFLG)
      IF(DONE.EQ.0)THEN
          ERRFLG = 6
          RETURN
      !    IF(SPN.LT.70)THEN
      !        SPN = 57
      !    ELSEIF(SPN.LT.200)THEN
      !        SPN = 106
      !    ELSEIF(SPN.LT.330)THEN
      !        SPN = 310
      !    ELSE
      !        SPN = 814
      !    ENDIF
      !    ERRFLG = 0
      !    CALL SEARCH(LAST,SPLIST,SPN,DONE,ERRFLG)
      ENDIF
      SPSG = SG(DONE)
      ! For woodland species, the VOL(1) from woodland volume equation is the outside bark volume from ground to 1.5" top
      ! For the DRYBIO, the wood and bark are not seperated and saved in the wood element 
      DRYBIO(2) = CV15*SPSG*62.4
      ! Stump wood and bark
      !DRYBIO(4) = 3.14*(DRC/2/12)**2*SPSG*62.4
      ! Get the BIO to 3" top
      CALL CHO_WDBK_1530(SPN,DRC,THT,CV15,STEMS,SPSG,BIO3)
      !Changed SPN to SPCD (2025/05/08)
      !CALL CHO_WDBK_1530(SPCD,DRC,THT,CV15,STEMS,SPSG,BIO3)
      DRYBIO(6) = BIO3
      ! Get the tip from 3" to 1.5"
      !DRYBIO(10) = DRYBIO(2) - BIO3
      ! Branches and foliage
      BIO3_M = BIO3/2.2046
      IF(SPN.GE.300)THEN
          WT_FOL = 10**(-.5655+(.8382*(LOG10(BIO3)))+(-.0094*THT))
          WT_BRA = 10**(.3036+(.7752*(LOG10(BIO3)))+(-.0049*THT))
      ELSE
          IF(SPN.EQ.106)THEN
              IF(BIO3_M.LE.468.028)THEN
               WT_FOL_M = EXP(1.0254+.559*LOG(BIO3_M))
              ELSE
               WT_FOL_M=EXP(1.0254+.559*(1.0+LOG(468.028)
     &         -468.028/BIO3_M))
              ENDIF
          ELSE
              IF(BIO3_M.LE.150.0)THEN
               WT_FOL_M = EXP(1.2867+.649*LOG(BIO3_M))
              ELSE
               WT_FOL_M = EXP(1.2867+.649*(1.0+LOG(150.0)-150.0/BIO3_M))
              ENDIF
          ENDIF
          WT_FOL = WT_FOL_M*2.2046
          WT_BRA = WT_FOL*0.75
          WT_FOL = WT_FOL*0.25
      ENDIF
      DRYBIO(12) = WT_BRA
      DRYBIO(13) = WT_FOL
      DRYBIO(1) = DRYBIO(6)+DRYBIO(12)
      
      RETURN
      END