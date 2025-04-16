##cumulative




carbon_sink <- cumulative %>% 
  group_by (YEAR, SITE) %>%
  mutate(running_total = cumsum(NEE_gC))

ggplot(carbon_sink, aes(DOFY, running_total, group = interaction(YEAR, SITE))) +
  geom_line(aes(col = SITE, lty = YEAR)) + 
  scale_color_manual(values=c("green4","orange", "cornflowerblue", "magenta"))+
  xlab("Day of Year")+
  ylab("Cumulative NEE (g C m-2)")+
  theme_minimal()



### conversions

install.packages("bigleaf")
library(bigleaf)

cumulative$NEE_gC <-umolCO2.to.gC(cumulative$NEE_CUT_REF)
cumulative$GPP_gC <-umolCO2.to.gC(cumulative$GPP_DT_CUT_MEAN)
cumulative$RECO_gC <-umolCO2.to.gC(cumulative$RECO_DT_CUT_MEAN)

##cumulative GPP and RECO

cumulative_gpp <- cumulative %>% 
  group_by (YEAR, SITE) %>%
  mutate(running_total = cumsum(GPP_gC))

ggplot(cumulative_gpp, aes(DOFY, running_total, group = interaction(YEAR, SITE))) +
  geom_line(aes(col = SITE, lty = YEAR)) + 
  scale_color_manual(values=c("green4","orange", "cornflowerblue", "magenta"))+
  xlab("Day of Year")+
  ylab("Cumulative GPP (g C m-2)")+
  theme_minimal()

cumulative_reco <- cumulative %>% 
  group_by (YEAR, SITE) %>%
  mutate(running_total = cumsum(RECO_gC))

ggplot(cumulative_reco, aes(DOFY, running_total, group = interaction(YEAR, SITE))) +
  geom_line(aes(col = SITE, lty = YEAR)) + 
  scale_color_manual(values=c("green4","orange", "cornflowerblue", "magenta"))+
  xlab("Day of Year")+
  ylab("Cumulative RECO (g C m-2)")+
  theme_minimal()

###stats


net_sinks<- data.frame(SITE = c("DBB", "DBB","DBB","DBB",
                                "BZF","BZF","BZF","BZF","BZF","BZF","BZF",
                                "AMO","AMO",
                                "SII","SII","SII","SII"),
                       YEAR = c( 2017, 2018, 2019, 2020, 
                                2014, 2016, 2017, 2018, 2019, 2020, 2021,
                                2022, 2023,
                                2019, 2020, 2022, 2023),
                       NETSINK = c(
                                   -211.06331463,
                                   -178.03478343,
                                   -180.17732537,
                                   -185.596649,
                                   -26.5264045,
                                   -263.1558517,
                                   -279.895595,
                                   -113.316892,
                                   -202.316892,
                                   -404.4337539,
                                   -179.659911,
                                   -378.712918,
                                   -458.6766987,
                                   -200.6832888,
                                   -186.6060120,
                                   -268.1194479,
                                   -196.34063),
                       NETGPP = c(482.264,
                                  544.034,
                                  531.988,
                                  535.326,
                                  344.612,
                                  796.293,
                                  621.338,
                                  465.057,
                                  592.462,
                                  768.563,
                                  565.258,
                                  1490.956,
                                  1564.838,
                                  525.345,
                                  527.333,
                                  571.148,
                                  525.103),
                       NETRECO = c(333.269,
                                   392.822,
                                   423.286,
                                   405.378,
                                   242.183,
                                   457.067,
                                   304.660,
                                   286.424,
                                   378.429,
                                   337.773,
                                   350.606,
                                   1164.402,
                                   1141.222,
                                   319.557,
                                   336.722,
                                   309.419,
                                   324.788))
net_sinks$YEAR <- as.character(net_sinks$YEAR)
boxplot(NETSINK ~SITE, data = net_sinks)
hist(net_sinks$NETSINK)
sinkmod<- aov(NETSINK ~ SITE, data = net_sinks)
summary(sinkmod)
res_aov<- oneway.test(NETSINK ~ SITE, data = net_sinks,var.equal = FALSE )
res_aov

TukeyHSD(sinkmod, which = "SITE")

library(purrr)

net_sinks %>% split(.$SITE) %>% map(summary)

net_sinks %>% group_by(SITE) %>% summarise(sd=sd(NETSINK))

net_sinks %>% group_by(SITE) %>% summarise(sd=sd(NETGPP))

net_sinks %>% group_by(SITE) %>% summarise(sd=sd(NETRECO))


#######GS length
GS_length$YEAR <- as.character(GS_length$YEAR)

GS_Length<- merge(GS_length,net_sinks,by=c("YEAR", "SITE") )

dateticks <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-01"),by="month")

ggplot(GS_length)+
  aes(ymin = as.numeric(YEAR) -.45, ymax = as.numeric(YEAR) +.45, xmin = GS_START, xmax = GS_END, fill = YEAR)+
      geom_rect(color = "black")+
        facet_wrap(~GS_length$SITE, nrow = 4)+
        theme_bw(base_size = 15)+
  scale_y_continuous(breaks=seq_along(GS_length$YEAR), labels = GS_length$YEAR)+
  scale_x_continuous(breaks=lubridate::yday(dateticks),
                     labels=lubridate::month(dateticks, label=TRUE, abbr=TRUE),
                     limits = c(0,365))


GS_length %>% split(.$SITE) %>% map(summary)

GS_length %>% group_by(SITE) %>% summarise(sd=sd(GS_LENGTH))

gs_aov<- oneway.test(GS_END ~ SITE, data = GS_length,var.equal = FALSE )
gs_aov
                  