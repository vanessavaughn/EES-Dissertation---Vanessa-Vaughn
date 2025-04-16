##correlation matrix
install.packages("corrplot")
library(corrplot)



bzf_corr<- na.omit(bzf_4d)
res.bzf<- cor(bzf_corr)
round(res,2)
res_bzf<- cor.test(bzf_4d$NEE_CUT_REF, bzf_4d$WS_F, method = "pearson")
res_bzf

corrplot(res.bzf, type = "lower", order = "original", tl.col = "black", tl.srt = 45,  method = "color")


sii_corr<- na.omit(sii_4d)
res.sii<- cor(sii_corr)
round(res,2)
res_sii<- cor.test(sii_4d$NEE_CUT_REF, sii_4d$WS_F, method = "pearson")
res_sii

corrplot(res.sii, type = "lower", order = "original", tl.col = "black", tl.srt = 45,  method = "color")

dbb_corr<- na.omit(dbb_4d)
res.dbb<- cor(dbb_corr)
round(res,2)
res_dbb<- cor.test(dbb_4d$NEE_CUT_REF, dbb_4d$Lai, method = "pearson")
res_dbb

-corrplot(res.dbb, type = "lower", order = "original", tl.col = "black", tl.srt = 45,  method = "color")

amo_corr<- na.omit(amo_4d)
res.amo<- cor(amo_corr)
round(res,2)
res_amo<- cor.test(amo_4d$NEE_CUT_REF, amo_4d$WTD, method = "pearson")
res_amo

corrplot(res.amo, type = "lower", order = "original", tl.col = "black", tl.srt = 45, method = "color")

cor(bzf_4d$NEE_CUT_REF, bzf_4d$VPD_F)

#with LAI and WTD



amo_wtd_hh$day <- rep(1:(nrow(amo_wtd_hh)/192), each = 192, length.out = nrow(amo_wtd_hh))

amo_wtd_hh2<- amo_wtd_hh %>% group_by(day) %>%
  summarise (across( everything(), mean),
             .groups = 'drop') %>%
  as.data.frame()

amo_wtd_lai <- merge(amo_wtd_hh2,LAI_AMO_22_24,by="day") 
amo_wtd_lai$Lai<- amo_wtd_lai$Lai/10


amo_wtd_lai<- na.omit(amo_wtd_lai)
res.amo.wtd<- cor(amo_wtd_lai)
corrplot(res.amo.wtd, type = "lower", order = "original", tl.col = "black", tl.srt = 45,  method = "color")







#with wtd


dbb_hh<- na.omit(dbb_hh)
res.dbb<- cor(dbb_hh)
round(res,2)

corrplot(res.dbb, type = "lower", order = "original", tl.col = "black", tl.srt = 45,  method = "color")



amo_hh<- na.omit(Amo_HH)
res.amo.wtd<- cor(amo_hh)
corrplot(res.amo.wtd, type = "lower", order = "original", tl.col = "black", tl.srt = 45,  method = "color")


sii_hh<- na.omit(sii_hh)
res.sii.wtd<- cor(sii_hh)
corrplot(res.sii.wtd, type = "lower", order = "original", tl.col = "black", tl.srt = 45,  method = "color")
