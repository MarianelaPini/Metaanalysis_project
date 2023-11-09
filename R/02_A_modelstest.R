#Metaanalysis
#models separated
#Starting with effect size
data<- read.csv(here::here("Data/processed", "data_te.csv"), sep = ";")
data<- read.csv("~/Brasil/mestrado/metaanalysis/Data/processed/data_te.csv",
                sep = ";",
                stringsAsFactors = FALSE,
                fileEncoding = "latin1")
#Loading packages
library(metafor)
library(readxl)
library(dplyr)
library(ggpubr)
#Loading data
#######################################################
### Calculating control x treatment ,effect size#######
#######################################################
#Transforming numeric
data$MEANc<-as.numeric(data$MEANc)
data$MEANn<-as.numeric(data$MEANn)
data$SDc<-as.numeric(data$SDc)
data$SDn<-as.numeric(data$SDn)
#Transforming factors
data$deg_status<-as.factor(data$deg_status)
data$control_type2<-as.factor(data$control_type2)
data$control_type3<-as.factor(data$control_type3)
data$t_lifestage1<-as.factor(data$t_lifestage1)
data$study_type<-as.factor(data$study_type)
data$t_lifestage1
#Getting effect sizes
data <- escalc(measure="SMD", m1i = as.numeric(MEANn), sd1i = as.numeric(SDn), m2i = as.numeric(MEANc), sd2i = as.numeric(SDc),
               n1i = as.numeric(Nn), n2i = as.numeric(Nc), data = data)
str(data)

######################
##                  ##
##  Fitting models  ## 
##                  ##
######################
##################################################
#                                                #
#Control (with tree)  x treatment (with Leucaena)#
#                                                #
##################################################
datact<-data[data$control_type2 == "tree",]
datact
#Geral model#
model_geralct <- rma.mv(yi, vi, random = ~1 | study_id/outcome_id, data = datact)
model_geralct
datact
#Overall forest plot
forest(model_geralct,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = "obs", 
       slab = datact$reference, 
       header = "Reference",
       ilab=  cbind((datact$Nc), (datact$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
X11(width = 14, height = 7)
savePlot(filename = "fpctree_geral.png", type = "png")
#Heterogeneity analisis
datact$wi <- 1/datact$vi
datact
sv.mdatact <- sum(datact$wi * (length(datact$wi) - 1))/(sum(datact$wi)^2 - sum(datact$wi^2))
sv.mdatact
#Calculating I^2 using variance components of the model associated with random factors
#(those summarized in the sigma2 structure components)
I2.geralct <- ((model_geralct$sigma2[1] + model_geralct$sigma2[2])/(model_geralct$sigma2[1] + model_geralct$sigma2[2] + sv.mdatact)
               * 100)
I2.geralct
#Egger regression, publication bias
egger.geralct<- lm(residuals.rma(model_geralct)~datact$vi)
summary(egger.geralct)
#Funnel plot
#Funnel plot (https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2020-December/002491.html)
cols <- palette.colors(length(unique(datact$study_id)), palette="polychrome")
cols <- cols[as.numeric(factor(datact$study_id))]
funnel(model_geralct, col=cols)
funnel(datact$yi, datact$vi, yaxis = "sei",  ylab = "Standard Error", xlab = "Effect size (SMD)", col = cols)
funnel(datact$yi, datact$vi, yaxis = "seinv", ylab = "Precision (1/SE)", xlab = "Effect size (SMD)", col = cols)
#Sensitivity analysis
#If residual standard >3 AND hatvalue >2 times the average of hatvalues, 
#run analysis with those cases deleted to test for sensitivity (from Habeck & Schultz 2015).
rs.model_geralct <- rstandard(model_geralct)
hat.model_geralct <- hatvalues(model_geralct) / mean(hatvalues(model_geralct))
plot(hat.model_geralct, rs.model_geralct$resid, xlab="Hat/average hat value", ylab= "Standard residuals", 
     xlim=c(0,4), ylim=c(-4,4), cex.lab=1.2)
abline (h=-3)
abline (h=3)
abline (v=(2))
###########################################
#Model with conservation degree estimator##
###########################################
modeldeg <- rma.mv (yi,vi,mods = ~0+deg_status ,random = ~1 | study_id/outcome_id, 
                    data = datact)
modeldeg
#model with intercept
modeldeg2 <- rma.mv (yi,vi,mods = ~1+deg_status ,random = ~1 | study_id/outcome_id, 
                     data = datact)
datact$deg_status
modeldeg2
#graphic conservation degree estimator#
forest(modeldeg,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = "obs", 
       slab = datact$reference, 
       header = "Reference",
       ilab=  cbind((datact$Nc), (datact$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
help("forest")
### add pooled estimate to the forest plot 
addpoly(model_geralct,mlab= "",cex=1)
help("savePlot")
savePlot(filename = "fpctree_conservation.png", type = "png")
##
#Heterogeneity analisis
datact$wi <- 1/datact$vi
datact
sv.mdatact <- sum(datact$wi * (length(datact$wi) - 1))/(sum(datact$wi)^2 - sum(datact$wi^2))
sv.mdatact
#Calculating I^2 using variance components of the model associated with random factors
#(those summarized in the sigma2 structure components)
I2.geralctdeg <- ((modeldeg$sigma2[1] + modeldeg$sigma2[2])/(modeldeg$sigma2[1] + modeldeg$sigma2[2] + sv.mdatact)
                  * 100)
I2.geralctdeg
#Egger regression, publication bias
egger.degct<- lm(residuals.rma(modeldeg)~datact$vi)
summary(egger.degct)
#Funnel plot
#Funnel plot (https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2020-December/002491.html)
cols <- palette.colors(length(unique(datact$study_id)), palette="polychrome")
cols <- cols[as.numeric(factor(datact$study_id))]
funnel(modeldeg, col=cols)
funnel(datact$yi, datact$vi, yaxis = "sei",  ylab = "Standard Error", xlab = "Effect size (SMD)", col = cols)
funnel(datact$yi, datact$vi, yaxis = "seinv", ylab = "Precision (1/SE)", xlab = "Effect size (SMD)", col = cols)
#Sensitivity analysis
#If residual standard >3 AND hatvalue >2 times the average of hatvalues, 
#run analysis with those cases deleted to test for sensitivity (from Habeck & Schultz 2015).
rs.modeldeg <- rstandard(modeldeg)
hat.modeldeg <- hatvalues(modeldeg) / mean(hatvalues(modeldeg))
plot(hat.modeldeg, rs.modeldeg$resid, xlab="Hat/average hat value", ylab= "Standard residuals", 
     xlim=c(0,4), ylim=c(-4,4), cex.lab=1.2)
abline (h=-3)
abline (h=3)
abline (v=(2))
#####################################################
#                                                   #
#Control (without tree)  x treatment (with Leucaena)#
#                                                   #
#####################################################
datacnt<-data[data$control_type2 == "notree",]
datacnt
#geral model
model_geralcnt <- rma.mv(yi, vi, random = ~1 | study_id/outcome_id, data = datacnt)
model_geralcnt
datacnt
#Forest plot geral model
forest(model_geralcnt,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = "obs", 
       slab = datacnt$reference, 
       header = "Reference",
       ilab=  cbind((datacnt$Nc), (datacnt$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
X11(width = 14, height = 7)
savePlot(filename = "fpntree.png", type = "png")
#Heterogeneity analisis
datacnt$wi <- 1/datacnt$vi
datacnt
sv.mdatacnt <- sum(datacnt$wi * (length(datacnt$wi) - 1))/(sum(datacnt$wi)^2 - sum(datacnt$wi^2))
sv.mdatacnt
#Calculating I^2 using variance components of the model associated with random factors
#(those summarized in the sigma2 structure components)
I2.geralcnt <- ((model_geralcnt$sigma2[1] + model_geralcnt$sigma2[2])/(model_geralcnt$sigma2[1] + model_geralcnt$sigma2[2] + sv.mdatacnt)
                * 100)
I2.geralcnt
#Egger regression, publication bias
egger.geralcnt<- lm(residuals.rma(model_geralcnt)~datacnt$vi)
summary(egger.geralcnt)
#Funnel plot
#Funnel plot (https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2020-December/002491.html)
cols <- palette.colors(length(unique(datacnt$study_id)), palette="polychrome")
cols <- cols[as.numeric(factor(datacnt$study_id))]
funnel(model_geralcnt, col=cols)
funnel(datacnt$yi, datacnt$vi, yaxis = "sei",  ylab = "Standard Error", xlab = "Effect size (SMD)", col = cols)
funnel(datacnt$yi, datacnt$vi, yaxis = "seinv", ylab = "Precision (1/SE)", xlab = "Effect size (SMD)", col = cols)
#Sensitivity analysis
#If residual standard >3 AND hatvalue >2 times the average of hatvalues, 
#run analysis with those cases deleted to test for sensitivity (from Habeck & Schultz 2015).
rs.model_geralcnt <- rstandard(model_geralcnt)
hat.model_geralcnt <- hatvalues(model_geralcnt) / mean(hatvalues(model_geralcnt))
plot(hat.model_geralcnt, rs.model_geralcnt$resid, xlab="Hat/average hat value", ylab= "Standard residuals", 
     xlim=c(0,4), ylim=c(-4,4), cex.lab=1.2)
abline (h=-3)
abline (h=3)
abline (v=(2))
##########################################
#Model with conservation degree estimator#
##########################################
model_degnt <- rma.mv (yi,vi,mods = ~0+deg_status ,random = ~1 | study_id/outcome_id, 
                       data = datacnt)
model_degnt
#model with intercept
model_degnt2 <- rma.mv (yi,vi,mods = ~1+deg_status ,random = ~1 | study_id/outcome_id, 
                        data = datacnt)
model_degnt2
#deg_status forest plot control tree
forest(model_degnt,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = "obs", 
       slab = datacnt$reference, 
       header = "Reference",
       ilab=  cbind((datacnt$Nc), (datacnt$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
savePlot(filename = "fpcnotree_conservation.png", type = "png")
#Heterogeneity analisis
datacnt$wi <- 1/datacnt$vi
datacnt
sv.mdatacnt <- sum(datacnt$wi * (length(datacnt$wi) - 1))/(sum(datacnt$wi)^2 - sum(datacnt$wi^2))
sv.mdatacnt
#Calculating I^2 using variance components of the model associated with random factors
#(those summarized in the sigma2 structure components)
I2.degnt <- ((model_degnt$sigma2[1] + model_degnt$sigma2[2])/
               (model_degnt$sigma2[1] + model_degnt$sigma2[2] + sv.mdatacnt)
             * 100)
I2.degnt
#Egger regression, publication bias
egger.degnt<- lm(residuals.rma(model_degnt)~datacnt$vi)
summary(egger.degnt)
#Funnel plot
#Funnel plot (https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2020-December/002491.html)
cols <- palette.colors(length(unique(datacnt$study_id)), palette="polychrome")
cols <- cols[as.numeric(factor(datacnt$study_id))]
funnel(model_degnt, col=cols)
funnel(datacnt$yi, datacnt$vi, yaxis = "sei",  ylab = "Standard Error", xlab = "Effect size (SMD)", col = cols)
funnel(datacnt$yi, datacnt$vi, yaxis = "seinv", ylab = "Precision (1/SE)", xlab = "Effect size (SMD)", col = cols)
#Sensitivity analysis
#If residual standard >3 AND hatvalue >2 times the average of hatvalues, 
#run analysis with those cases deleted to test for sensitivity (from Habeck & Schultz 2015).
rs.model_degnt <- rstandard(model_degnt)
hat.model_degnt <- hatvalues(model_degnt) / mean(hatvalues(model_degnt))
plot(hat.model_degnt, rs.model_degnt$resid, xlab="Hat/average hat value", ylab= "Standard residuals", 
     xlim=c(0,4), ylim=c(-4,4), cex.lab=1.2)
abline (h=-3)
abline (h=3)
abline (v=(2))