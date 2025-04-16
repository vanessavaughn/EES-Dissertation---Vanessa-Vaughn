#GPP and RECO correlation matrices

########################
#DBB
dbb_hh$date <- rep(1:(nrow(dbb_hh)/192), each = 192, length.out = nrow(dbb_hh))
dbb_ddqc<- dbb_hhqc%>% group_by(date) %>%
  summarise (across( everything(), mean),
             .groups = 'drop') %>%
  as.data.frame()


dbb_hhqc <- subset(dbb_hh, NEE_CUT_REF_QC == 0)
dbb_hhgpp <- subset(dbb_hhqc, GPP_DT_CUT_MEAN > 0)
dbb_ddgpp<- dbb_hhgpp%>% group_by(date) %>%
  summarise (across( everything(), mean),
             .groups = 'drop') %>%
  as.data.frame()
plot(dbb_ddqc$date, dbb_ddqc$NEE_CUT_REF)


dbb_ddgpp <- merge(dbb_ddgpp,LAI_DBB_14_20,by="date") 
dbb_ddgpp$Lai<- dbb_ddgpp$Lai/10

dbb_ddreco <- merge(dbb_ddqc,LAI_DBB_14_20,by="date") 
dbb_ddreco$Lai<- dbb_ddreco$Lai/10

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

amo_hh$date <- rep(1:(nrow(amo_hh)/192), each = 192, length.out = nrow(amo_hh))
amo_ddqc<- amo_hhqc%>% group_by(date) %>%
  summarise (across( everything(), mean),
             .groups = 'drop') %>%
  as.data.frame()


amo_hhqc <- subset(amo_hh, NEE_CUT_REF_QC == 0)
amo_hhgpp <- subset(amo_hhqc, GPP_DT_CUT_MEAN > 0)
amo_ddgpp<- amo_hhgpp%>% group_by(date) %>%
  summarise (across( everything(), mean),
             .groups = 'drop') %>%
  as.data.frame()


amo_ddgpp <- merge(amo_ddgpp,LAI_AMO_22_24,by="date") 
amo_ddgpp$Lai<- amo_ddgpp$Lai/10

amo_ddreco <- merge(amo_ddqc,LAI_AMO_22_24,by="date") 
amo_ddreco$Lai<- amo_ddreco$Lai/10

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

bzf_hhqc <- subset(bzf_hh, NEE_CUT_REF_QC == 0)
bzf_ddqc<- bzf_hhqc%>% group_by(date) %>%
  summarise (across( everything(), mean),
             .groups = 'drop') %>%
  as.data.frame()


bzf_hhgpp <- subset(bzf_hhqc, GPP_DT_CUT_MEAN > 0)
bzf_ddgpp<- bzf_hhgpp%>% group_by(date) %>%
  summarise (across( everything(), mean),
             .groups = 'drop') %>%
  as.data.frame()


bzf_.ddgpp <- merge(bzf_ddgpp,LAI_BZF_11_22,by="date") 
bzf_.ddgpp$Lai<- bzf_.ddgpp$Lai/10

bzf_ddreco <- merge(bzf_ddqc,LAI_BZF_11_22,by="date") 
bzf_ddreco$Lai<- bzf_ddreco$Lai/10

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

sii_hhqc <- subset(sii_hh, NEE_CUT_REF_QC == 0)
sii_ddqc<- sii_hhqc%>% group_by(date) %>%
  summarise (across( everything(), mean),
             .groups = 'drop') %>%
  as.data.frame()


sii_hhgpp <- subset(sii_hhqc, GPP_DT_CUT_MEAN > 0)
sii_ddgpp<- sii_hhgpp%>% group_by(date) %>%
  summarise (across( everything(), mean),
             .groups = 'drop') %>%
  as.data.frame()


sii_ddgpp <- merge(sii_ddgpp,LAI_SII_19_24,by="date") 
sii_ddgpp$Lai<- sii_ddgpp$Lai/10

sii_ddreco <- merge(sii_ddqc,LAI_SII_19_24,by="date") 
sii_ddreco$Lai<- sii_ddreco$Lai/10

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


##############
#models



sii.gs<- data.frame(TA_F_MDS = sii_gsreco$TA_F_MDS,
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
                          NEE = sii_gsreco$NEE_CUT_REF)
sii.gs<- na.omit(sii.gs)      
write.csv(sii.gs, "C:/Users/vanes/Downloads/diss/data/GS/sii.gs.csv")

bzf.gs<- data.frame(TA_F_MDS = bzf_gsreco$TA_F_MDS,
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
                          RECO = bzf_gsreco$RECO_DT_CUT_MEAN,
                          GPP = bzf_gsgpp$GPP_DT_CUT_MEAN,
                          NEE = bzf_gsreco$NEE_CUT_REF)
bzf.gs<- na.omit(bzf.gs)      
write.csv(bzf.gs, "C:/Users/vanes/Downloads/diss/data/GS/bzf.gs.csv")

amo.gs<- data.frame(TA_F_MDS = amo_gsreco$TA_F_MDS,
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
                          RECO = amo_gsreco$RECO_DT_CUT_MEAN,
                    GPP = amo_gsgpp$GPP_DT_CUT_MEAN,
                    NEE = amo_gsreco$NEE_CUT_REF)
amo.gs<- na.omit(amo.gs)      
write.csv(amo.gs, "C:/Users/vanes/Downloads/diss/data/GS/amo.gs.csv")

dbb.gs<- data.frame(TA_F_MDS = dbb_gsreco$TA_F_MDS,
              SW_IN_F = dbb_gsreco$SW_IN_F,
              LW_IN_F = dbb_gsreco$LW_IN_F,
              VPD_F = dbb_gsreco$VPD_F,
              P_F = dbb_gsreco$P_F,
              WS_F = dbb_gsreco$WS_F,
              RH = dbb_gsreco$RH,
              NETRAD = dbb_gsreco$NETRAD,
              TS_F_MDS_1 = dbb_gsreco$TS_F_MDS_1,
              LE_F_MDS = dbb_gsreco$LE_F_MDS,
              H_F_MDS = dbb_gsreco$H_F_MDS,
              Lai = dbb_gsreco$Lai,
              WTD = dbb_gsreco$WTD,
              RECO = dbb_gsreco$RECO_DT_CUT_MEAN,
              GPP = dbb_gsgpp$GPP_DT_CUT_MEAN,
              NEE = dbb_gsreco$NEE_CUT_REF)
dbb.gs<- na.omit(dbb.gs)      
write.csv(dbb.gs, "C:/Users/vanes/Downloads/diss/data/GS/dbb.gs.csv")

# Initialize an empty model

forward_model <- lm(GPP ~ ., data = c.sii_gsgpp)

# Forward stepwise regression

forward_model <- step(forward_model, direction = "forward", scope = formula(~ .))

backward_model <- lm(GPP ~ ., data = c.sii_gsgpp)
# Backward stepwise regression
backward_model <- step(backward_model, direction = "backward")

# Initialize a model with all predictors
both_model <- lm(GPP ~ ., data = c.sii_gsgpp)
# Both-direction stepwise regression
both_model <- step(both_model, direction = "both")
summary(both_model)

gsmodel<- lm(GPP ~ SITE*SWC_F_MDS_1, data = GS_ALL)
summary(gsmodel)
anova(gsmodel)
interact_plot(gsmodel, pred="SWC_F_MDS_1", modx = "SITE", plot.points = TRUE, vary.lty = FALSE, interval = TRUE,
              colors = c("green4", "orange","cornflowerblue","magenta"
                              ), x.label = "RECO (µmol CO2 m-2 s-1)",
              y.label = "GPP (µmol CO2 m-2 s-1)")
install.packages("interactions")
library(interactions)

##transformations
GS_ALL$sqrt.ta<- sqrt(GS_ALL$TA_F_MDS)
GS_ALL$sqrt.gpp<- sqrt(GS_ALL$GPP)











plot(amo_hhqc$LE_F_MDS, amo_hhqc$NEE_CUT_REF)
abline(2.481318, 0.027837)
abline(1.211065,.006306)

hist(amo_hhqc$LE_F_MDS)





amo.all<- data.frame(TA_F_MDS = amo_ddreco$TA_F_MDS,
                    SW_IN_F = amo_ddreco$SW_IN_F,
                    LW_IN_F = amo_ddreco$LW_IN_F,
                    VPD_F = amo_ddreco$VPD_F,
                    P_F = amo_ddreco$P_F,
                    WS_F = amo_ddreco$WS,
                    RH = amo_ddreco$RH,
                    NETRAD = amo_ddreco$NETRAD,
                    TS_F_MDS_1 = amo_ddreco$TS_F_MDS_1,
                    SWC_F_MDS_1 = amo_ddreco$SWC_F_MDS_1,
                    LE_F_MDS = amo_ddreco$LE_F_MDS,
                    H_F_MDS = amo_ddreco$H_F_MDS,
                    Lai = amo_ddreco$Lai,
                    WTD = amo_ddreco$WTD,
                    RECO = amo_ddreco$RECO_DT_CUT_MEAN,
                    GPP = amo_ddreco$GPP_DT_CUT_MEAN,
                    NEE = amo_ddreco$NEE_CUT_REF,
                    dofy = amo_ddreco$date,
                    date = amo_ddreco$DATE)
sii.all<- data.frame(TA_F_MDS = sii_ddreco$TA_F_MDS,
                     SW_IN_F = sii_ddreco$SW_IN_F,
                     LW_IN_F = sii_ddreco$LW_IN_F,
                     VPD_F = sii_ddreco$VPD_F,
                     P_F = sii_ddreco$P_F,
                     WS_F = sii_ddreco$WS_F,
                     RH = sii_ddreco$RH,
                     NETRAD = sii_ddreco$NETRAD,
                     TS_F_MDS_1 = sii_ddreco$TS_F_MDS_1,
                     SWC_F_MDS_1 = sii_ddreco$SWC_F_MDS_1,
                     LE_F_MDS = sii_ddreco$LE_F_MDS,
                     H_F_MDS = sii_ddreco$H_F_MDS,
                     Lai = sii_ddreco$Lai,
                     WTD = sii_ddreco$WTD,
                     RECO = sii_ddreco$RECO_DT_CUT_MEAN,
                     GPP = sii_ddreco$GPP_DT_CUT_MEAN,
                     NEE = sii_ddreco$NEE_CUT_REF,
                     dofy = sii_ddreco$date,
                     date = sii_ddreco$TIMESTAMP_START)
bzf.all<- data.frame(TA_F_MDS = bzf_ddreco$TA_F_MDS,
                     SW_IN_F = bzf_ddreco$SW_IN_F,
                     LW_IN_F = bzf_ddreco$LW_IN_F,
                     VPD_F = bzf_ddreco$VPD_F,
                     P_F = bzf_ddreco$P_F,
                     WS_F = bzf_ddreco$WS_F,
                     RH = bzf_ddreco$RH,
                     NETRAD = bzf_ddreco$NETRAD,
                     TS_F_MDS_1 = bzf_ddreco$TS_F_MDS_1,
                     SWC_F_MDS_1 = bzf_ddreco$SWC_F_MDS_1,
                     LE_F_MDS = bzf_ddreco$LE_F_MDS,
                     H_F_MDS = bzf_ddreco$H_F_MDS,
                     Lai = bzf_ddreco$Lai,
                     RECO = bzf_ddreco$RECO_DT_CUT_MEAN,
                     GPP = bzf_ddreco$GPP_DT_CUT_MEAN,
                     NEE = bzf_ddreco$NEE_CUT_REF,
                     dofy = bzf_ddreco$date,
                     date = bzf_ddreco$TIMESTAMP_START)
dbb.all<- data.frame(TA_F_MDS = dbb_ddreco$TA_F_MDS,
                     SW_IN_F = dbb_ddreco$SW_IN_F,
                     LW_IN_F = dbb_ddreco$LW_IN_F,
                     VPD_F = dbb_ddreco$VPD_F,
                     P_F = dbb_ddreco$P_F,
                     WS_F = dbb_ddreco$WS_F,
                     RH = dbb_ddreco$RH,
                     NETRAD = dbb_ddreco$NETRAD,
                     TS_F_MDS_1 = dbb_ddreco$TS_F_MDS_1,
                     LE_F_MDS = dbb_ddreco$LE_F_MDS,
                     H_F_MDS = dbb_ddreco$H_F_MDS,
                     Lai = dbb_ddreco$Lai,
                     WTD = dbb_ddreco$WTD,
                     RECO = dbb_ddreco$RECO_DT_CUT_MEAN,
                     GPP = dbb_ddreco$GPP_DT_CUT_MEAN,
                     NEE = dbb_ddreco$NEE_CUT_REF,
                     dofy = dbb_ddreco$date,
                     date = dbb_ddreco$TIMESTAMP_START)
siigpp<- data.frame(GPP = sii_ddgpp$GPP_DT_CUT_MEAN,
                    dofy = sii_ddgpp$date)
sii.all <- merge(siigpp,sii.all,by="dofy") 
amogpp<- data.frame(GPP = amo_ddgpp$GPP_DT_CUT_MEAN,
                    dofy = amo_ddgpp$date)
amo.all <- merge(amogpp,amo.all,by="dofy") 
dbbgpp<- data.frame(GPP = dbb_ddgpp$GPP_DT_CUT_MEAN,
                    dofy = dbb_ddgpp$date)
dbb.all <- merge(dbbgpp,dbb.all,by="dofy") 
bzfgpp<- data.frame(GPP = bzf_ddgpp$GPP_DT_CUT_MEAN,
                    dofy = bzf_ddgpp$date)
bzf.all <- merge(bzfgpp,bzf.all,by="dofy") 
write.csv(dbb.all, "C:/Users/vanes/Downloads/diss/data/dbb.all.csv")
write.csv(sii.all, "C:/Users/vanes/Downloads/diss/data/sii.all.csv")
write.csv(amo.all, "C:/Users/vanes/Downloads/diss/data/amo.all.csv")
write.csv(bzf.all, "C:/Users/vanes/Downloads/diss/data/bzf.all.csv")

allmodel<- lm(GPP~ SITE*SWC_F_MDS_1, data = alldays_allsites)
summary(allmodel)
anova(allmodel)

interact_plot(allmodel, pred="SWC_F_MDS_1", modx = "SITE", plot.points = TRUE, vary.lty = FALSE, interval = TRUE,
              color.class = c("green4", "orange","magenta"))
write.csv(GS_ALL, "C:/Users/vanes/Downloads/diss/data/GS_allsites.csv")
write.csv(alldays_allsites, "C:/Users/vanes/Downloads/diss/data/alldays_allsites.csv")
