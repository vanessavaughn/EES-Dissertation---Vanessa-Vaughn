#### Year long model, all sites
install.packages("interactions")
library(interactions)
install.packages("rstatix")
library(rstatix)


# difference is significant if log-likelihood ratio test is more than 2 points

### models of co2 flux ~ site * enviro variable 
        #   r^2 represents how much of the variation between NEE at different sites is explained by that enviro variable (hopefully)
        # looking at the difference between model fit with interaction term vs with just flux ~ SITE

#year long model, all sites
#some model fit is better/worse 
#model fit - infer causality from what in the model explains more variance, doesn't mean that the variable is a stronger driver of NEE.  
#kruskal wallis test for non linear - see if there are same significant effects - if consistent w glm there is no problem. 



allmodel_site<- glm(NEE~ SITE, data = alldays_allsites, family = Gaussian ("identity"))
summary(allmodel_site)
anova(allmodel_site)
ancova(allmodel_site)
allmodel<- glm(RECO ~ SITE*LE_F_MDS, data = alldays_allsites)
summary(allmodel)
anova(allmodel)
interact_plot(allmodel, pred="LE_F_MDS", modx = "SITE", plot.points = TRUE, vary.lty = FALSE, interval = TRUE,
              colors = c("green4","orange","cornflowerblue","magenta"
              ), x.label = "Latent Heat Flux (W m-2)",
              y.label = "RECO (µmol CO2 m-2 s-1)")

alldays_allsites %>% anova_test(NEE~ SITE*VPD_F)

# different point symbols for different variables. 




#### Growing season only model, all sites

gsmodel<- lm(NEE ~ SITE*TA_F_MDS, data = GS_ALL) 
summary(gsmodel)
anova(gsmodel)
interact_plot(gsmodel, pred="TA_F_MDS", modx = "SITE", plot.points = TRUE, vary.lty = FALSE, interval = TRUE,
              colors = c("green4", "orange","cornflowerblue","magenta"),
              x.label = "Air Temp (deg C)",
              y.label = "NEE (µmol CO2 m-2 s-1)")
#repeat for all variables 


