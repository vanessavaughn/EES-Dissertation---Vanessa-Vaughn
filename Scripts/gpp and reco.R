#GPP and RECO correlation matrices

########################
#DBB


c.dbb_gsreco<- data.frame(TA_F_MDS = dbb_gsreco$TA_F_MDS,
                  SW_IN_F = dbb_gsreco$SW_IN_F,
                  LW_IN_F = dbb_gsreco$LW_IN_F,
                  VPD_F = dbb_gsreco$VPD_F,
                  P_F = dbb_gsreco$P_F,
                  WS_F = dbb_gsreco$WS_F,
                  RH = dbb_gsreco$RH,
                  NETRAD = dbb_gsreco$NETRAD,
                  TS_F_MDS_1 = dbb_gsreco$TS_F_MDS_1,
                  TS_F_MDS_2 = dbb_gsreco$TS_F_MDS_2,
                  LE_F_MDS = dbb_gsreco$LE_F_MDS,
                  H_F_MDS = dbb_gsreco$H_F_MDS,
                  Lai = dbb_gsreco$Lai,
                  WTD = dbb_gsreco$WTD,
                  RECO = dbb_gsreco$RECO_DT_CUT_MEAN)
c.dbb_gsgpp<- data.frame(TA_F_MDS = dbb_gsgpp$TA_F_MDS,
                          SW_IN_F = dbb_gsgpp$SW_IN_F,
                          LW_IN_F = dbb_gsgpp$LW_IN_F,
                          VPD_F = dbb_gsgpp$VPD_F,
                          P_F = dbb_gsgpp$P_F,
                          WS_F = dbb_gsgpp$WS_F,
                          RH = dbb_gsgpp$RH,
                          NETRAD = dbb_gsgpp$NETRAD,
                          TS_F_MDS_1 = dbb_gsgpp$TS_F_MDS_1,
                          TS_F_MDS_2 = dbb_gsgpp$TS_F_MDS_2,
                          LE_F_MDS = dbb_gsgpp$LE_F_MDS,
                          H_F_MDS = dbb_gsgpp$H_F_MDS,
                          Lai = dbb_gsgpp$Lai,
                          WTD = dbb_gsgpp$WTD,
                          GPP = dbb_gsgpp$GPP_DT_CUT_MEAN)

write.csv(bzf_ddreco, "C:/Users/vanes/Downloads/diss/data/bzf_ddall.csv")
write.csv(dbb_ddgpp, "C:/Users/vanes/Downloads/diss/data/dbb_ddgpp.csv")

plot(c.dbb_gsgpp$GPP~ c.dbb_gsgpp$SW_IN_F)
plot(c.dbb_gsreco$RECO ~ c.dbb_gsreco$SW_IN_F)

c.dbb_gsgpp<- na.omit(c.dbb_gsgpp)
res.c.dbb_gsgpp<- cor(c.dbb_gsgpp)
corrplot(res.c.dbb_gsgpp, type = "lower", order = "original", tl.col = "black", tl.srt = 45,  method = "color")

c.dbb_gsreco<- na.omit(c.dbb_gsreco)
res.c.dbb_gsreco<- cor(c.dbb_gsreco)
corrplot(res.c.dbb_gsreco, type = "lower", order = "original", tl.col = "black", tl.srt = 45,  method = "color")

res_dbb.reco<- cor.test(c.dbb_gsreco$RECO, c.dbb_gsreco$LE_F_MDS, method = "pearson")
res_dbb.reco


#######################
#AMO



c.amo_gsreco<- data.frame(TA_F_MDS = amo_gsreco$TA_F_MDS,
                          SW_IN_F = amo_gsreco$SW_IN_F,
                          LW_IN_F = amo_gsreco$LW_IN_F,
                          VPD_F = amo_gsreco$VPD_F,
                          P_F = amo_gsreco$P_F,
                          WS_F = amo_gsreco$WS,
                          RH = amo_gsreco$RH,
                          NETRAD = amo_gsreco$NETRAD,
                          TS_F_MDS_1 = amo_gsreco$TS_F_MDS_1,
                          SWC_F_MDS_1 = amo_gsreco$SWC_F_MDS_1,
                          LE_F_MDS = amo_gsreco$LE_F_MDS,
                          H_F_MDS = amo_gsreco$H_F_MDS,
                          Lai = amo_gsreco$Lai,
                          WTD = amo_gsreco$WTD,
                          RECO = amo_gsreco$RECO_DT_CUT_MEAN)
c.amo_gsgpp<- data.frame(TA_F_MDS = amo_gsgpp$TA_F_MDS,
                         SW_IN_F = amo_gsgpp$SW_IN_F,
                         LW_IN_F = amo_gsgpp$LW_IN_F,
                         VPD_F = amo_gsgpp$VPD_F,
                         P_F = amo_gsgpp$P_F,
                         WS_F = amo_gsgpp$WS,
                         RH = amo_gsgpp$RH,
                         NETRAD = amo_gsgpp$NETRAD,
                         TS_F_MDS_1 = amo_gsgpp$TS_F_MDS_1,
                         SWC_F_MDS_1 = amo_gsgpp$SWC_F_MDS_1,
                         LE_F_MDS = amo_gsgpp$LE_F_MDS,
                         H_F_MDS = amo_gsgpp$H_F_MDS,
                         Lai = amo_gsgpp$Lai,
                         WTD = amo_gsgpp$WTD,
                         GPP = amo_gsgpp$GPP_DT_CUT_MEAN)

write.csv(amo_ddreco, "C:/Users/vanes/Downloads/diss/data/amo_ddreco.csv")
write.csv(amo_ddgpp, "C:/Users/vanes/Downloads/diss/data/amo_ddgpp.csv")


c.amo_gsgpp<- na.omit(c.amo_gsgpp)
res.c.amo_gsgpp<- cor(c.amo_gsgpp)
corrplot(res.c.amo_gsgpp, type = "lower", order = "original", tl.col = "black", tl.srt = 45,  method = "color")

c.amo_gsreco<- na.omit(c.amo_gsreco)
res.c.amo_gsreco<- cor(c.amo_gsreco)
corrplot(res.c.amo_gsreco, type = "lower", order = "original", tl.col = "black", tl.srt = 45,  method = "color")

res_amo.reco<- cor.test(c.amo_gsreco$RECO, c.amo_gsreco$TA_F_MDS, method = "pearson")
res_amo.reco


#######################
#BZF



c.bzf_gsreco<- data.frame(TA_F_MDS = bzf_gsreco$TA_F_MDS,
                          SW_IN_F = bzf_gsreco$SW_IN_F,
                          LW_IN_F = bzf_gsreco$LW_IN_F,
                          VPD_F = bzf_gsreco$VPD_F,
                          P_F = bzf_gsreco$P_F,
                          WS_F = bzf_gsreco$WS_F,
                          RH = bzf_gsreco$RH,
                          NETRAD = bzf_gsreco$NETRAD,
                          TS_F_MDS_1 = bzf_gsreco$TS_F_MDS_1,
                          SWC_F_MDS_1 = bzf_gsreco$SWC_F_MDS_1,
                          LE_F_MDS = bzf_gsreco$LE_F_MDS,
                          H_F_MDS = bzf_gsreco$H_F_MDS,
                          Lai = bzf_gsreco$Lai,
                          Lai2 = bzf_gsreco$Lai,
                          RECO = bzf_gsreco$RECO_DT_CUT_MEAN)

c.bzf_gsgpp<- data.frame(TA_F_MDS = bzf_gsgpp$TA_F_MDS,
                          SW_IN_F = bzf_gsgpp$SW_IN_F,
                          LW_IN_F = bzf_gsgpp$LW_IN_F,
                          VPD_F = bzf_gsgpp$VPD_F,
                          P_F = bzf_gsgpp$P_F,
                          WS_F = bzf_gsgpp$WS_F,
                          RH = bzf_gsgpp$RH,
                          NETRAD = bzf_gsgpp$NETRAD,
                          TS_F_MDS_1 = bzf_gsgpp$TS_F_MDS_1,
                          SWC_F_MDS_1 = bzf_gsgpp$SWC_F_MDS_1,
                          LE_F_MDS = bzf_gsgpp$LE_F_MDS,
                          H_F_MDS = bzf_gsgpp$H_F_MDS,
                          Lai = bzf_gsgpp$Lai,
                          Lai2 = bzf_gsgpp$Lai,
                          GPP = bzf_gsgpp$GPP_DT_CUT_MEAN)


write.csv(bzf_ddreco, "C:/Users/vanes/Downloads/diss/data/bzf_ddreco.csv")
write.csv(bzf_.ddgpp, "C:/Users/vanes/Downloads/diss/data/bzf_.ddgpp.csv")


c.bzf_gsgpp<- na.omit(c.bzf_gsgpp)
res.c.bzf_gsgpp<- cor(c.bzf_gsgpp)
corrplot(res.c.bzf_gsgpp, type = "lower", order = "original", tl.col = "black", tl.srt = 45,  method = "color")

c.bzf_gsreco<- na.omit(c.bzf_gsreco)
res.c.bzf_gsreco<- cor(c.bzf_gsreco)
corrplot(res.c.bzf_gsreco, type = "lower", order = "original", tl.col = "black", tl.srt = 45,  method = "color")

res_bzf.gpp<- cor.test(c.bzf_gsgpp$GPP, c.bzf_gsgpp$TA_F_MDS, method = "pearson")
res_bzf.gpp



#######################
#SII



c.sii_gsreco<- data.frame(TA_F_MDS = sii_gsreco$TA_F_MDS,
                          SW_IN_F = sii_gsreco$SW_IN_F,
                          LW_IN_F = sii_gsreco$LW_IN_F,
                          VPD_F = sii_gsreco$VPD_F,
                          P_F = sii_gsreco$P_F,
                          WS_F = sii_gsreco$WS_F,
                          RH = sii_gsreco$RH,
                          NETRAD = sii_gsreco$NETRAD,
                          TS_F_MDS_1 = sii_gsreco$TS_F_MDS_1,
                          SWC_F_MDS_1 = sii_gsreco$SWC_F_MDS_1,
                          LE_F_MDS = sii_gsreco$LE_F_MDS,
                          H_F_MDS = sii_gsreco$H_F_MDS,
                          Lai = sii_gsreco$Lai,
                          WTD = sii_gsreco$WTD,
                          RECO = sii_gsreco$RECO_DT_CUT_MEAN,
                          GPP = sii_gsgpp$GPP_DT_CUT_MEAN,
                          )

c.sii_gsgpp<- data.frame(TA_F_MDS = sii_gsgpp$TA_F_MDS,
                         SW_IN_F = sii_gsgpp$SW_IN_F,
                         LW_IN_F = sii_gsgpp$LW_IN_F,
                         VPD_F = sii_gsgpp$VPD_F,
                         P_F = sii_gsgpp$P_F,
                         WS_F = sii_gsgpp$WS_F,
                         RH = sii_gsgpp$RH,
                         NETRAD = sii_gsgpp$NETRAD,
                         TS_F_MDS_1 = sii_gsgpp$TS_F_MDS_1,
                         SWC_F_MDS_1 = sii_gsgpp$SWC_F_MDS_1,
                         LE_F_MDS = sii_gsgpp$LE_F_MDS,
                         H_F_MDS = sii_gsgpp$H_F_MDS,
                         Lai = sii_gsgpp$Lai,
                         WTD = sii_gsgpp$WTD,
                         GPP = sii_gsgpp$GPP_DT_CUT_MEAN)



write.csv(cumulative, "C:/Users/vanes/Downloads/diss/data/cumulative.csv")


c.sii_gsgpp<- na.omit(c.sii_gsgpp)
res.c.sii_gsgpp<- cor(c.sii_gsgpp)
corrplot(res.c.sii_gsgpp, type = "lower", order = "original", tl.col = "black", tl.srt = 45,  method = "color")

c.sii_gsreco<- na.omit(c.sii_gsreco)
res.c.sii_gsreco<- cor(c.sii_gsreco)
corrplot(res.c.sii_gsreco, type = "lower", order = "original", tl.col = "black", tl.srt = 45,  method = "color")

res_sii.reco<- cor.test(c.sii_gsreco$RECO, c.sii_gsreco$TA_F_MDS, method = "pearson")


res_sii.reco


