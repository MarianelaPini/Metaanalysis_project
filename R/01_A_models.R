#Metaanalysis
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
model_geral <- rma.mv(yi, vi, random = ~1 | study_id/outcome_id, data = data)
model_geral
#Overall forest plot
forest(model_geral,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = "obs", 
       slab = data$reference, 
       header = "Reference",
       ilab=  cbind((data$Nc), (data$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
X11(width = 14, height = 7)
savePlot(filename = "forestplot.png", type = "png")
## #Heterogeneity I^2 for hierarchical models is not provided by metafor
#We calculate total heterogeneity using the formulas provided by Nakagawa & Santos 2012
#Heterogeneity analisis
data$wi <- 1/data$vi
data
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
#This analysis is for each model, and each dataset#

###############################################
##                                          ##
##Model with controltype estimator          ##
##                                          ##
##############################################
#In this model I test control type without trees and with trees
modelctype <- rma.mv (yi,vi,mods = ~1+control_type2 ,random = ~1 | study_id/outcome_id, 
                    data = data)
modelctype
modelctype2 <- rma.mv(yi = yi, V = vi, random = 
                       list( ~ 1 | study_id, ~ 1 | outcome_id), data = data)
###graphic caterpillar
model_results <- orchaRd::mod_results(modelctype2, mod = "1", at = NULL,  group = "study_id")
model_results
#test
orchaRd::caterpillars(model_results, mod="1", xlab = "Standardised mean difference") 
X11(width = 14, height = 7)
savePlot(filename = "forestplotctype.png", type = "png")
#sem intercepto
modelct0 <- rma.mv (yi,vi,mods = ~0+control_type2 ,random = ~1 | study_id/outcome_id, 
                   data = data)
modelct0
#control_type forest plot
forest(modelctype,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = "obs", 
       slab = data$reference, 
       header = "Reference",
       ilab=  cbind((data$Nc), (data$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
X11(width = 14, height = 7)
savePlot(filename = "fplot_controlsd.png", type = "png")
#Heterogeneity analisis
data$wi <- 1/data$vi
data
sv.mdata <- sum(data$wi * (length(data$wi) - 1))/(sum(data$wi)^2 - sum(data$wi^2))
sv.mdata
#Calculating I^2 using variance components of the model associated with random factors
#(those summarized in the sigma2 structure components)
I2.geralctype <- ((modelctype$sigma2[1] + modelctype$sigma2[2])/
                    (modelctype$sigma2[1] + modelctype$sigma2[2] + sv.mdata)
             * 100)
I2.geralctype
#Egger regression, publication bias
egger.geralctype<- lm(residuals.rma(modelctype)~data$vi)
summary(egger.geralctype)
#Funnel plot
#Funnel plot (https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2020-December/002491.html)
cols <- palette.colors(length(unique(data$study_id)), palette="polychrome")
cols <- cols[as.numeric(factor(data$study_id))]
funnel(modelctype, col=cols)
funnel(data$yi, data$vi, yaxis = "sei",  ylab = "Standard Error", xlab = "Effect size (SMD)", col = cols)
funnel(data$yi, data$vi, yaxis = "seinv", ylab = "Precision (1/SE)", xlab = "Effect size (SMD)", col = cols)
#Sensitivity analysis
#If residual standard >3 AND hatvalue >2 times the average of hatvalues, 
#run analysis with those cases deleted to test for sensitivity (from Habeck & Schultz 2015).
rs.modelctype <- rstandard(modelctype)
hat.modelctype <- hatvalues(modelctype) / mean(hatvalues(modelctype))
hatvalues(modelctype)
plot(hat.modelctype, rs.modelctype$resid, xlab="Hat/average hat value", ylab= "Standard residuals", 
     xlim=c(0,4), ylim=c(-4,4), cex.lab=1.2)
abline (h=-3)
abline (h=3)
abline (v=(2))
####################################################
#model with interaction control type and deg status#
####################################################
model_gint <- rma.mv (yi,vi,mods = ~1+deg_status*control_type2 ,random = ~1 | study_id/outcome_id, 
                        data = data)
model_gint
#forest plot
forest(model_gint,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = "obs", 
       slab = data$reference, 
       header = "Reference",
       ilab=  cbind((data$Nc), (data$Nn)), 
       ilab.xpos = c(-9.5, -8),colout = colour_group[data$deg_status])
abline(h = 0)
X11(width = 14, height = 7)
savePlot(filename = "fplot_int.png", type = "png")
#forest plot by deg status
help(forest)
colour_group<-c("orange","blue")
forest(model_gint,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = data$deg_status, 
       slab = data$reference, 
       header = "Reference",
       ilab=  cbind((data$Nc), (data$Nn)), 
       ilab.xpos = c(-9.5, -8),colout = colour_group[data$deg_status])
abline(h = 0)
X11(width = 14, height = 7)
savePlot(filename = "fplot_intd.png", type = "png")
#Heterogeneity analisis
data$wi <- 1/data$vi
data
sv.mdata <- sum(data$wi * (length(data$wi) - 1))/(sum(data$wi)^2 - sum(data$wi^2))
sv.mdata
#Calculating I^2 using variance components of the model associated with random factors
#(those summarized in the sigma2 structure components)
I2.geralct <- ((model_gint$sigma2[1] + model_gint$sigma2[2])/
                  (model_gint$sigma2[1]+ model_gint$sigma2[2] + sv.mdata)
                * 100)
I2.geralct
#Egger regression, publication bias
egger.ct<- lm(residuals.rma(model_gint)~data$vi)
summary(egger.ct)
#Funnel plot
#Funnel plot (https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2020-December/002491.html)
cols <- palette.colors(length(unique(data$study_id)), palette="polychrome")
cols <- cols[as.numeric(factor(data$study_id))]
funnel(model_gint, col=cols)
funnel(data$yi, data$vi, yaxis = "sei",  ylab = "Standard Error", xlab = "Effect size (SMD)", col = cols)
funnel(data$yi, data$vi, yaxis = "seinv", ylab = "Precision (1/SE)", xlab = "Effect size (SMD)", col = cols)
#Sensitivity analysis
#If residual standard >3 AND hatvalue >2 times the average of hatvalues, 
#run analysis with those cases deleted to test for sensitivity (from Habeck & Schultz 2015).
rs.model_gint <- rstandard(model_gint)
hat.model_gint <- hatvalues(model_gint) / mean(hatvalues(model_gint))
plot(hat.model_gint, rs.model_gint$resid, xlab="Hat/average hat value", ylab= "Standard residuals", 
     xlim=c(0,4), ylim=c(-4,4), cex.lab=1.2)
abline (h=-3)
abline (h=3)
abline (v=(2))
####################################################
#Model with origin control estimator#
#In this model I test control type with trees,if it is native,exotic or native
#and exotic
datact<-data[data$control_type2 == "tree",]
datact
model_oct2<- rma.mv (yi,vi,mods = ~0+control_type3 ,random = ~1 | study_id/outcome_id, 
                   data = datact)
model_oct2
#with intercept
model_oct <- rma.mv (yi,vi,mods = ~1+control_type3 ,random = ~1 | study_id/outcome_id, 
                    data = datact)
model_oct
#control type origin,forest plot
forest(model_oct,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = "obs", 
       slab = datact$reference, 
       header = "Reference",
       ilab=  cbind((datact$Nc), (datact$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
savePlot(filename = "fplot_contorigin.png", type = "png")
#Heterogeneity analisis
datact$wi <- 1/datact$vi
datact
sv.mdatact <- sum(datact$wi * (length(datact$wi) - 1))/(sum(datact$wi)^2 - sum(datact$wi^2))
sv.mdatact
#Calculating I^2 using variance components of the model associated with random factors
#(those summarized in the sigma2 structure components)
I2.geraloct <- ((model_oct$sigma2[1] + model_oct$sigma2[2])/
                    (model_oct$sigma2[1]+ model_oct$sigma2[2] + sv.mdatact)
                  * 100)
I2.geraloct
#Egger regression, publication bias
egger.oct<- lm(residuals.rma(model_oct)~datact$vi)
summary(egger.oct)
#Funnel plot
#Funnel plot (https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2020-December/002491.html)
cols <- palette.colors(length(unique(datact$study_id)), palette="polychrome")
cols <- cols[as.numeric(factor(datact$study_id))]
funnel(model_oct, col=cols)
funnel(datact$yi, datact$vi, yaxis = "sei",  ylab = "Standard Error", xlab = "Effect size (SMD)", col = cols)
funnel(datact$yi, datact$vi, yaxis = "seinv", ylab = "Precision (1/SE)", xlab = "Effect size (SMD)", col = cols)
#Sensitivity analysis
#If residual standard >3 AND hatvalue >2 times the average of hatvalues, 
#run analysis with those cases deleted to test for sensitivity (from Habeck & Schultz 2015).
rs.modeloct <- rstandard(model_oct)
hat.modeloct <- hatvalues(model_oct) / mean(hatvalues(model_oct))
plot(hat.modeloct, rs.modeloct$resid, xlab="Hat/average hat value", ylab= "Standard residuals", 
     xlim=c(0,4), ylim=c(-4,4), cex.lab=1.2)
abline (h=-3)
abline (h=3)
abline (v=(2))
##########################################################
#model with type of study (observational or experimental)#
model_tstudy <- rma.mv (yi,vi,mods = ~1+study_type ,random = ~1 | study_id/outcome_id, 
                      data = data)
model_tstudy
#Heterogeneity analisis
data$wi <- 1/data$vi
data
sv.mdata <- sum(data$wi * (length(data$wi) - 1))/(sum(data$wi)^2 - sum(data$wi^2))
sv.mdata
#Calculating I^2 using variance components of the model associated with random factors
#(those summarized in the sigma2 structure components)
I2.geralct <- ((model_tstudy$sigma2[1] + model_tstudy$sigma2[2])/
                 (model_tstudy$sigma2[1]+ model_tstudy$sigma2[2] + sv.mdata)
               * 100)
I2.geralct
#Egger regression, publication bias
egger.ct<- lm(residuals.rma(model_tstudy)~data$vi)
summary(egger.ct)
#Funnel plot
#Funnel plot (https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2020-December/002491.html)
cols <- palette.colors(length(unique(data$study_id)), palette="polychrome")
cols <- cols[as.numeric(factor(data$study_id))]
funnel(model_tstudy, col=cols)
funnel(data$yi, data$vi, yaxis = "sei",  ylab = "Standard Error", xlab = "Effect size (SMD)", col = cols)
funnel(data$yi, data$vi, yaxis = "seinv", ylab = "Precision (1/SE)", xlab = "Effect size (SMD)", col = cols)
#Sensitivity analysis
#If residual standard >3 AND hatvalue >2 times the average of hatvalues, 
#run analysis with those cases deleted to test for sensitivity (from Habeck & Schultz 2015).
rs.model_tstudy <- rstandard(model_tstudy)
hat.model_tstudy <- hatvalues(model_tstudy) / mean(hatvalues(model_tstudy))
plot(hat.model_tstudy, rs.model_tstudy$resid, xlab="Hat/average hat value", ylab= "Standard residuals", 
     xlim=c(0,4), ylim=c(-4,4), cex.lab=1.2)
abline (h=-3)
abline (h=3)
abline (v=(2))
#model study type with interaction control type
model_tstudy2 <- rma.mv (yi,vi,mods = ~1+study_type*control_type2 ,random = ~1 | study_id/outcome_id, 
                        data = data)
model_tstudy2
#não aparece a interação, é porque sem arvore tem só experimentais?
#model study type with interaction deg status
model_tstudy3 <- rma.mv (yi,vi,mods = ~1+study_type*deg_status ,random = ~1 | study_id/outcome_id, 
data = data)
model_tstudy3

##########################################################
##########################################################
#model with type of performance (fitness or community)#
data$performance3
model_performance <- rma.mv (yi,vi,mods = ~1+performance3 ,random = ~1 | study_id/outcome_id, 
                        data = data)
model_performance
#Heterogeneity analisis
data$wi <- 1/data$vi
data
sv.mdata <- sum(data$wi * (length(data$wi) - 1))/(sum(data$wi)^2 - sum(data$wi^2))
sv.mdata
#Calculating I^2 using variance components of the model associated with random factors
#(those summarized in the sigma2 structure components)
I2.geralct <- ((model_performance$sigma2[1] + model_performance$sigma2[2])/
                 (model_performance$sigma2[1]+ model_performance$sigma2[2] + sv.mdata)
               * 100)
I2.geralct
#Egger regression, publication bias
egger.ct<- lm(residuals.rma(model_performance)~data$vi)
summary(egger.ct)
#Funnel plot
#Funnel plot (https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2020-December/002491.html)
cols <- palette.colors(length(unique(data$study_id)), palette="polychrome")
cols <- cols[as.numeric(factor(data$study_id))]
funnel(model_performance, col=cols)
funnel(data$yi, data$vi, yaxis = "sei",  ylab = "Standard Error", xlab = "Effect size (SMD)", col = cols)
funnel(data$yi, data$vi, yaxis = "seinv", ylab = "Precision (1/SE)", xlab = "Effect size (SMD)", col = cols)
#Sensitivity analysis
#If residual standard >3 AND hatvalue >2 times the average of hatvalues, 
#run analysis with those cases deleted to test for sensitivity (from Habeck & Schultz 2015).
rs.model_performance <- rstandard(model_performance)
hat.model_performance <- hatvalues(model_performance) / mean(hatvalues(model_performance))
plot(hat.model_performance, rs.model_performance$resid, xlab="Hat/average hat value", ylab= "Standard residuals", 
     xlim=c(0,4), ylim=c(-4,4), cex.lab=1.2)
abline (h=-3)
abline (h=3)
abline (v=(2))
#model performance with interaction control type
model_performance2 <- rma.mv (yi,vi,mods = ~1+performance3*control_type2 ,random = ~1 | study_id/outcome_id, 
                      data = data)
model_performance2
#model performance with interaction deg status
model_performance3 <- rma.mv (yi,vi,mods = ~1+performance3*control_type2 ,random = ~1 | study_id/outcome_id, 
                              data = data)
model_performance3
#model study type with interaction
