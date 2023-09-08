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
       order = order(data$study_id), 
       slab = data$reference, 
       header = "Reference",
       ilab=  cbind((data$Nc), (data$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
X11(width = 14, height = 7)
savePlot(filename = "forestplot.png", type = "png")
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
#Overall forest plot
forest(model_geralct,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = order(datact$study_id), 
       slab = datact$reference, 
       header = "Reference",
       ilab=  cbind((datact$Nc), (datact$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
X11(width = 14, height = 7)
savePlot(filename = "fpctree_geral.png", type = "png")

#Model with conservation degree estimator
modeldeg <- rma.mv (yi,vi,mods = ~0+deg_status ,random = ~1 | study_id/outcome_id, 
                 data = datact)
modeldeg
#model with intercept
modeldeg2 <- rma.mv (yi,vi,mods = ~1+deg_status ,random = ~1 | study_id/outcome_id, 
                    data = datact)
datact$deg_status
modeldeg2
#graphic conservation degree estimator#
forest(modeldeg2,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = order(datact$study_id), 
       slab = datact$reference, 
       header = "Reference",
       ilab=  cbind((datact$Nc), (datact$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
help("forest")
### add pooled estimate to the forest plot 
addpoly(model_geralct)

savePlot(filename = "fpctree_conservation.png", type = "png")
#####################################################
#                                                   #
#Control (without tree)  x treatment (with Leucaena)#
#                                                   #
#####################################################
datacnt<-data[data$control_type2 == "notree",]
datacnt
#Model with conservation degree estimator
modeldegnt <- rma.mv (yi,vi,mods = ~0+deg_status ,random = ~1 | study_id/outcome_id, 
                    data = datacnt)
modeldegnt
#model with intercept
modeldegnt2 <- rma.mv (yi,vi,mods = ~1+deg_status ,random = ~1 | study_id/outcome_id, 
                     data = datacnt)
modeldegnt2
#deg_status forest plot control tree
forest(modeldegnt,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = order(datacnt$study_id), 
       slab = datacnt$reference, 
       header = "Reference",
       ilab=  cbind((datacnt$Nc), (datacnt$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
savePlot(filename = "fpcnotree_conservation.png", type = "png")
#Model with controltype estimator
#In this model I test control type without trees and with trees
modelct <- rma.mv (yi,vi,mods = ~1+control_type2 ,random = ~1 | study_id/outcome_id, 
                    data = data)
modelct
#sem intercepto
modelct0 <- rma.mv (yi,vi,mods = ~0+control_type2 ,random = ~1 | study_id/outcome_id, 
                   data = data)
modelct0
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
savePlot(filename = "fplot_controls.png", type = "png")
#Model with origin control estimator
#In this model I test control type with trees,if it is native,exotic or native
#and exotic
modeloct <- rma.mv (yi,vi,mods = ~0+control_type3 ,random = ~1 | study_id/outcome_id, 
                   data = datact)
modeloct
#with intercept
modeloct2 <- rma.mv (yi,vi,mods = ~1+control_type3 ,random = ~1 | study_id/outcome_id, 
                    data = datact)
modeloct2
#control type origin,forest plot
forest(modeloct,
       xlab = "Hedge's g",
       cex = 0.5, 
       order = order(datact$study_id), 
       slab = datact$reference, 
       header = "Reference",
       ilab=  cbind((datact$Nc), (datact$Nn)), 
       ilab.xpos = c(-9.5, -8))
abline(h = 0)
savePlot(filename = "fplot_contorigin.png", type = "png")
############   test ###############
#                                 #
###################################
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
#testing lifestage of target plant, control with tree
#sem intercepto
modellt0 <- rma.mv (yi,vi,mods = ~0+ t_lifestage1,
                   random = ~1 | study_id/outcome_id, 
                     data = datact)
modellt0
#com intercepto
modellt <- rma.mv (yi,vi,mods = ~1+ t_lifestage1,
                    random = ~1 | study_id/outcome_id, 
                    data = datact)
modellt
#testing lifestage of target plant, control without tree
modellt0 <- rma.mv (yi,vi,mods = ~0+ t_lifestage1,
                    random = ~1 | study_id/outcome_id, 
                    data = datacnt)
modellt0
#com intercepto
modellt <- rma.mv (yi,vi,mods = ~1+ t_lifestage1,
                   random = ~1 | study_id/outcome_id, 
                   data = datacnt)
modellt
#possible correlation between observational and experimental
datact$study_type
modelst <- rma.mv (yi,vi,mods = ~0+ study_type,
                    random = ~1 | study_id/outcome_id, 
                    data = datacnt)
modelst
#con intercepto
modelst1 <- rma.mv (yi,vi,mods = ~1+ study_type,
                   random = ~1 | study_id/outcome_id, 
                   data = datacnt)
modelst1
