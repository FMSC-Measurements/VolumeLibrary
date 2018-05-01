C The percentage (in decimal) of bark volume to wood volume is calculated based this paper:
C Bark Volume Determinatiopn in Trees, H. A. Meyer (1946), 
C Journal of Forestry, Vol. 44 No. 12
      SUBROUTINE BARKVOLPCT(REGN, FORST, DIST, SPN, DBH, HT, BTR, DBTBH,
     &  BVP)
      INCLUDE FVS_FIAJSP.INC
      INCLUDE FIASP_SGMCBP.INC
      INTEGER REGN, SPN, FORNUM, IS, CNT, I
      CHARACTER*2 FORST, DIST, VAR
      CHARACTER*3 SPC
      REAL DBH, HT, DBTBH, BTR, BVP, k
      
      BVP = 0.0
      IF(BTR.GT.1.0)THEN
        k = BTR/100
      ELSE IF(BTR.GT.0)THEN
        k = BTR
      ELSE IF(DBTBH.GT.0)THEN
        k = (DBH-DBTBH)/DBH
      ELSE
        WRITE (SPC, '(I0)') SPN
        IF(SPC(3:3) .LT. '0')THEN
          SPC(2:3) = SPC(1:2)
          SPC(1:1) = '0'
        ENDIF
        GETVARIANT(REGN,FORST,DIST,VAR)
C get bark ratio (DIB/DOB) from variant equation       
        IF(VAR.EQ.'AK')THEN
           CNT = COUNT(AK_FIAJSP)
           IS = VAR_SPIDX(CNT, AK_FIAJSP, SPC)
           AK_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'BM')THEN
           CNT = COUNT(BM_FIAJSP)
           IS = VAR_SPIDX(CNT, BM_FIAJSP, SPC)
           BM_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'CA')THEN
           CNT = COUNT(CA_FIAJSP)
           IS = VAR_SPIDX(CNT, CA_FIAJSP, SPC)
           CA_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'CI')THEN
           CNT = COUNT(CI_FIAJSP)
           IS = VAR_SPIDX(CNT, CI_FIAJSP, SPC)
           CI_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'CR')THEN
           CNT = COUNT(CR_FIAJSP)
           IS = VAR_SPIDX(CNT, CR_FIAJSP, SPC)
           CR_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'CS')THEN
           CNT = COUNT(CS_FIAJSP)
           IS = VAR_SPIDX(CNT, CS_FIAJSP, SPC)
           CS_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'EC')THEN
           CNT = COUNT(EC_FIAJSP)
           IS = VAR_SPIDX(CNT, EC_FIAJSP, SPC)
           EC_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'EM')THEN
           CNT = COUNT(EM_FIAJSP)
           IS = VAR_SPIDX(CNT, EM_FIAJSP, SPC)
           EM_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'IE')THEN
           CNT = COUNT(IE_FIAJSP)
           IS = VAR_SPIDX(CNT, IE_FIAJSP, SPC)
           IE_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'KT')THEN
           CNT = COUNT(KT_FIAJSP)
           IS = VAR_SPIDX(CNT, KT_FIAJSP, SPC)
           KT_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'LS')THEN
           CNT = COUNT(LS_FIAJSP)
           IS = VAR_SPIDX(CNT, LS_FIAJSP, SPC)
           LS_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'NC')THEN
           CNT = COUNT(NC_FIAJSP)
           IS = VAR_SPIDX(CNT, NC_FIAJSP, SPC)
           NC_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'NE')THEN
           CNT = COUNT(NE_FIAJSP)
           IS = VAR_SPIDX(CNT, NE_FIAJSP, SPC)
           NE_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'NI')THEN
           CNT = COUNT(NI_FIAJSP)
           IS = VAR_SPIDX(CNT, NI_FIAJSP, SPC)
           NI_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'PN')THEN
           CNT = COUNT(PN_FIAJSP)
           IS = VAR_SPIDX(CNT, PN_FIAJSP, SPC)
           PN_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'SN')THEN
           CNT = COUNT(SN_FIAJSP)
           IS = VAR_SPIDX(CNT, SN_FIAJSP, SPC)
           SN_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'SO')THEN
           CNT = COUNT(SO_FIAJSP)
           IS = VAR_SPIDX(CNT, SO_FIAJSP, SPC)
           SO_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'TT')THEN
           CNT = COUNT(TT_FIAJSP)
           IS = VAR_SPIDX(CNT, TT_FIAJSP, SPC)
           TT_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'UT')THEN
           CNT = COUNT(UT_FIAJSP)
           IS = VAR_SPIDX(CNT, UT_FIAJSP, SPC)
           UT_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'WC')THEN
           CNT = COUNT(WC_FIAJSP)
           IS = VAR_SPIDX(CNT, WC_FIAJSP, SPC)
           WC_BRATIO(IS,DBH,HT,k)
        ELSE IF(VAR.EQ.'WS')THEN
           CNT = COUNT(WS_FIAJSP)
           IS = VAR_SPIDX(CNT, WS_FIAJSP, SPC)
           WS_BRATIO(IS,DBH,HT,k)
           
        ENDIF
      ENDIF
C The percentage of bark volume to wood volume calculate using bark ratio (DIB/DOB) as below 
C BVP is in decimal, i.e. 18% will be 0.18
      BVP = 1.0/(k*k) - 1.0
C If the bark volume percentage cannot be calculated, us the default value from FIADB for the species
      IF(BVP.GT.1.0 .OR. BVP.LT.0.1)THEN
        BVP = 0
        DO I = 1, 455
          IF(FIASP_SG(I, 1).EQ.SPN)THEN
            BVP=FIASP_SG(I, 6)/100.0
            RETURN
          ENDIF
        ENDDO
        IF(BVP.EQ.0)THEN
          IF(SPN.LT.300)THEN
C Use the other softwood 299 for softwood species
            BVP=FIASP_SG(119,6)/100.0
          ELSE
            BVP=FIASP_SG(455,6)/100.0
          ENDIF
        ENDIF
      ENDIF
      END

C Search the list of the species for the variant to find the position of the spceies
      FUNCTION VAR_SPIDX(CNT, FIAJSP, SPC)
      INTEFER CNT, I, SPIDX
      CHARACTER*3 SPC, FIAJSP(CNT)
      
      SPIDX = 0
      DO I = 1, CNT
        IF(FIAJSP(I).EQ.SPC)THEN 
          SPIDX = I
          RETURN
        ENDIF;
      ENDDO
C If the species is not in the variant list species, use the last species as default
      IF(SPIDX.EQ.0) SPIDX = CNT
      END
