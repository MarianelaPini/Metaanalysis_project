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
data$control_type3
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
       order = order(data$study_id), 
       slab = data$reference, 
       header = "Reference",
       ilab=  cbind((data$Nc), (data$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
X11(width = 14, height = 7)
savePlot(filename = "forestplot.png", type = "png")
#Model with conservation degree estimator
#
modeldeg <- rma.mv (yi,vi,mods = ~1+deg_status ,random = ~1 | study_id/outcome_id, 
                 data = data)
modeldeg
#deg_status forest plot
forest(modeldeg,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = order(data$study_id), 
       slab = data$reference, 
       header = "Reference",
       ilab=  cbind((data$Nc), (data$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
#Model with controltype estimator
#In this model I test control type without trees and with trees
modelct <- rma.mv (yi,vi,mods = ~1+control_type2 ,random = ~1 | study_id/outcome_id, 
                    data = data)
modelct
#control_type forest plot
forest(modelct,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = order(data$study_id), 
       slab = data$reference, 
       header = "Reference",
       ilab=  cbind((data$Nc), (data$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
#Model with origincontrol estimator
#In this model I test control type with trees,if it is native,exotic or native
#and exotic
modeloct <- rma.mv (yi,vi,mods = ~1+control_type3 ,random = ~1 | study_id/outcome_id, 
                   data = data)
modeloct
#control type origin,forest plot
forest(modeloct,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = order(data$study_id), 
       slab = data$reference, 
       header = "Reference",
       ilab=  cbind((data$Nc), (data$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
#testing model with two moderators together
modelcont <- rma.mv (yi,vi,mods = ~1+ control_type3 + control_type2 ,random = ~1 | study_id/outcome_id, 
                    data = data)
modelcont
#forestplot model together
forest(modelcont,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = order(data$study_id), 
       slab = data$reference, 
       header = "Reference",
       ilab=  cbind((data$Nc), (data$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
