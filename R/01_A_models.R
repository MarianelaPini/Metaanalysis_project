#Metaanalysis
data<- read.csv(here::here("Data/processed", "data_te_master.csv"), sep = ";")
data<- read.csv("~/Brasil/mestrado/metaanalysis/Data/processed/data_te.csv",
                sep = ";",
                stringsAsFactors = FALSE,
                fileEncoding = "latin1")
data
#Loading packages
library(metafor)
library(readxl)
library(dplyr)
library(ggpubr)
library(emmeans)
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
data$scale<-as.factor(data$scale)
data$performance3<-as.factor(data$performance3)
#Getting effect sizes
data <- escalc(measure="SMD", m1i = as.numeric(MEANn), sd1i = as.numeric(SDn), m2i = as.numeric(MEANc), sd2i = as.numeric(SDc),
               n1i = as.numeric(Nn), n2i = as.numeric(Nc), data = data)
data
write.csv(data,"data_te.csv")
######################
##                  ##
##  Fitting models  ## 
##                  ##
######################
#Geral model with random variable only#
model_geral <- rma.mv(yi, vi, random = ~1 | study_id/outcome_id, data = data)
model_geral
#Overall forest plot
X11(width = 11, height = 12)
par(mar=c(4,4,1,2))
forest(model_geral,
       xlab = "Hedge's g",xlim=c(-12,17.5),alim = c(-12,17.5),
       cex = 0.8, 
       order = "obs",
       header = "Reference",slab = data$reference,
       )
savePlot(filename = "forestplot2.png", type = "png")
####################################################
#model with interaction control type and deg status#
####################################################
##
forest(model_gint0,
       xlab = "Hedge's g",xlim=c(-13,17.5),alim = c(-13,17.5),
       cex = 0.7, 
       order = "obs",
       header = "Reference",slab = data$reference,
)
#without intercept, that model is what was used#
model_gint0 <- rma.mv (yi,vi,mods = ~0+deg_status*control_type2 ,
                       random = ~1 | study_id/outcome_id, 
                      data = data)
model_gint0
#saving predicts with function emmeans
res.variables.model_gint0 <- qdrg(object = model_gint0, 
                                       data = data, 
                                       at = list(sqrt_inv_n_tilda = 0, year.c = 0))
#means for model
overall.model_gint0 <- emmeans(res.variables.model_gint0, 
                                    specs = ~1, 
                                    df = model_gint0$ddf, 
                                    weights = "prop")
overall.model_gint0
# marginalized means for different levels of deg status and control type
mm.model_gint0 <- emmeans(res.variables.model_gint0, 
                               specs = c("deg_status", "control_type2"), 
                               df = model_gint0$ddf, 
                               )
#mean and range
summary(mm.model_gint0)
pvalues<-test(mm.model_gint0)
pvalues
mm.modelpvalues <- as.data.frame(pvalues)
mm.modelpvalues
#forest plot by deg status
X11(width = 14, height = 7)
colour_group<-c("orange","blue")
forest(model_gint0,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = data$deg_status, 
       slab = data$reference, 
       header = "Reference",
       ilab=  cbind((data$Nc), (data$Nn)), 
       ilab.xpos = c(-9.5, -8),colout = colour_group[data$deg_status])
abline(h = 0)
savePlot(filename = "fplot_intd.png", type = "png")
####################################################
#Model with origin control estimator#
#In this model I test control type with trees,if it is native,exotic or native
#and exotic
#extracting data
datact<-data[data$control_type2 == "tree",]
model_oct2<- rma.mv (yi,vi,mods = ~0+control_type3 ,random = ~1 | study_id/outcome_id, 
                   data = datact)
model_oct2
##saving predicts with function emmeans, for model control type
res.variables.model_oct2 <- qdrg(object = model_oct2, 
                                  data = datact, 
                                  at = list(sqrt_inv_n_tilda = 0, year.c = 0))
#means for model
overall.model_oct2 <- emmeans(res.variables.model_oct2, 
                               specs = ~1, 
                               df = model_oct2$ddf, 
                               weights = "prop")
overall.model_oct2
# marginalized means for different levels of deg status and control type
mm.model_oct2 <- emmeans(res.variables.model_oct2, 
                          specs = "control_type3", 
                          df = model_oct2$ddf, 
)
#mean and range
summary(mm.model_oct2)
pvalues<-test(mm.model_oct2)
pvalues
mm.modelpvalues <- as.data.frame(pvalues)
mm.modelpvalues
###########################################
##                                      ##
##########################################
###Heterogeneity and Egger tests for each model##
##
#########################################
##test heterogeneity for geral model#
## #Heterogeneity I^2 for hierarchical models is not provided by metafor
#We calculate total heterogeneity using the formulas provided by Nakagawa & Santos 2012
#Heterogeneity analisis
data$wi <- 1/data$vi
sv.mdata <- sum(data$wi * (length(data$wi) - 1))/(sum(data$wi)^2 - sum(data$wi^2))
sv.mdata
#Calculating I^2 using variance components of the model associated with random factors
#(those summarized in the sigma2 structure components)
I2.geral <- ((model_geral$sigma2[1] + model_geral$sigma2[2])/(model_geral$sigma2[1] + model_geral$sigma2[2] + sv.mdata)
             * 100)
I2.geral
#Egger regression, publication bias
egger.geral<- lm(residuals.rma(model_geral)~data$vi)
summary(egger.geral)
#Funnel plot
#Funnel plot (https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2020-December/002491.html)
cols <- palette.colors(length(unique(data$study_id)), palette="polychrome")
cols <- cols[as.numeric(factor(data$study_id))]
funnel(model_geral, col=cols)
funnel(data$yi, data$vi, yaxis = "sei",  ylab = "Standard Error", xlab = "Effect size (SMD)", col = cols)
funnel(data$yi, data$vi, yaxis = "seinv", ylab = "Precision (1/SE)", xlab = "Effect size (SMD)", col = cols)
#Sensitivity analysis
#If residual standard >3 AND hatvalue >2 times the average of hatvalues, 
#run analysis with those cases deleted to test for sensitivity (from Habeck & Schultz 2015).
rs.model_geral <- rstandard(model_geral)
hat.model_geral <- hatvalues(model_geral) / mean(hatvalues(model_geral))
plot(hat.model_geral, rs.model_geral$resid, xlab="Hat/average hat value", ylab= "Standard residuals", 
     xlim=c(0,4), ylim=c(-4,4), cex.lab=1.2)
abline (h=-3)
abline (h=3)
abline (v=(2))
###
# heterogeneity and egger for control and deg status
#Calculating I^2 using variance components of the model associated with random factors
#(those summarized in the sigma2 structure components)
I2.geralct <- ((model_gint0$sigma2[1] + model_gint0$sigma2[2])/
                 (model_gint0$sigma2[1]+ model_gint0$sigma2[2] + sv.mdata)
               * 100)
I2.geralct
#Egger regression, publication bias
egger.ct<- lm(residuals.rma(model_gint0)~data$vi)
summary(egger.ct)
#Funnel plot
#Funnel plot (https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2020-December/002491.html)
cols <- palette.colors(length(unique(data$study_id)), palette="polychrome")
cols <- cols[as.numeric(factor(data$study_id))]
funnel(model_gint0, col=cols)
funnel(data$yi, data$vi, yaxis = "sei",  ylab = "Standard Error", xlab = "Effect size (SMD)", col = cols)
funnel(data$yi, data$vi, yaxis = "seinv", ylab = "Precision (1/SE)", xlab = "Effect size (SMD)", col = cols)
#Sensitivity analysis
#If residual standard >3 AND hatvalue >2 times the average of hatvalues, 
#run analysis with those cases deleted to test for sensitivity (from Habeck & Schultz 2015).
rs.model_gint0 <- rstandard(model_gint0)
hat.model_gint0 <- hatvalues(model_gint0) / mean(hatvalues(model_gint0))
plot(hat.model_gint0, rs.model_gint0$resid, xlab="Hat/average hat value", ylab= "Standard residuals", 
     xlim=c(0,4), ylim=c(-4,5), cex.lab=1.2)
abline (h=-3)
abline (h=3)
abline (v=(2))
##
##
#Heterogeneity analisis for control type with trees#
datact$wi <- 1/datact$vi
datact
sv.mdatact <- sum(datact$wi * (length(datact$wi) - 1))/(sum(datact$wi)^2 - sum(datact$wi^2))
sv.mdatact
#Calculating I^2 using variance components of the model associated with random factors
#(those summarized in the sigma2 structure components)
I2.geraloct <- ((model_oct2$sigma2[1] + model_oct2$sigma2[2])/
                  (model_oct2$sigma2[1]+ model_oct2$sigma2[2] + sv.mdatact)
                * 100)
I2.geraloct
#Egger regression, publication bias
egger.oct2<- lm(residuals.rma(model_oct2)~datact$vi)
summary(egger.oct2)
#Funnel plot
#Funnel plot (https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2020-December/002491.html)
cols <- palette.colors(length(unique(datact$study_id)), palette="polychrome")
cols <- cols[as.numeric(factor(datact$study_id))]
funnel(model_oct2, col=cols)
funnel(datact$yi, datact$vi, yaxis = "sei",  ylab = "Standard Error", xlab = "Effect size (SMD)", col = cols)
funnel(datact$yi, datact$vi, yaxis = "seinv", ylab = "Precision (1/SE)", xlab = "Effect size (SMD)", col = cols)
#Sensitivity analysis
#If residual standard >3 AND hatvalue >2 times the average of hatvalues, 
#run analysis with those cases deleted to test for sensitivity (from Habeck & Schultz 2015).
rs.modeloct2 <- rstandard(model_oct2)
hat.modeloct2 <- hatvalues(model_oct2) / mean(hatvalues(model_oct2))
plot(hat.modeloct2, rs.modeloct2$resid, xlab="Hat/average hat value", ylab= "Standard residuals", 
     xlim=c(0,4), ylim=c(-4,5), cex.lab=1.2)
abline (h=-3)
abline (h=3)
abline (v=(2))
