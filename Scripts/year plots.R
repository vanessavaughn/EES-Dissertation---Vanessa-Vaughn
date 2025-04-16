sii.plot<- subset(alldays_allsites, SITE == "SII")

sii.plot$week <- rep(1:(nrow(sii.plot)/3), each = 3, length.out = nrow(sii.plot))

sii.plot<- sii.plot%>% group_by(week) %>%
  summarise (across( everything(), mean),
             .groups = 'drop') %>%
  as.data.frame()

ggplot(bzf.plot, aes(x= week))+
  geom_line(aes(y= NEE, color="NEE (µmol CO2 m-2 s-1)") )+
  geom_line(aes(y= VPD_F, color="VPD (hPa)") )+
  geom_line(aes(y= TS_F_MDS_1, color="Soil Temperature (deg C)") )+
  geom_line(aes(y=Lai, color = "LAI")) +
  geom_line(aes(y= RECO, color="RECO (µmol CO2 m-2 s-1)") )+
  geom_line(aes(y= GPP, color="GPP (µmol CO2 m-2 s-1)") )+
  geom_line(aes(y= SWC_F_MDS_1, color="SWC") )+
  ggtitle("Bonanza Creek Fen")+
  labs(x= "Day",
       y= "")+
  theme_minimal()+
  scale_x_continuous(breaks=lubridate::yday(dateticks),
                     labels=lubridate::month(dateticks, label=TRUE, abbr=TRUE))
                     

net_sinks %>% split(.$SITE) %>% map(summary)

net_sinks %>% group_by(SITE) %>% summarise(sd=sd(NETSINK))

summary(bzf.plot$VPD_F)
summary(sii.plot$VPD_F)

dateticks <- seq.Date(as.Date("2014-01-01"), as.Date("2022-07-01"),by="month")

 

#STATS
alldays_allsites%>% split(.$SITE) %>% map(summary)

alldays_allsites %>% group_by(SITE) %>% summarise(sd=sd(SWC_F_MDS_1))

ts_aov<- oneway.test(TS_F_MDS_1 ~ SITE, data = GS_ALL,var.equal = FALSE )
ts_aov

lai_aov<- oneway.test(Lai ~ SITE, data = GS_ALL,var.equal = FALSE )
lai_aov

vpd_aov<- oneway.test(VPD_F ~ SITE, data = GS_ALL,var.equal = FALSE )
vpd_aov

wtd_aov<- oneway.test(WTD ~ SITE, data = GS_ALL,var.equal = FALSE )
wtd_aov

                                        