#Metaanalysis
#Starting with effect size
setwd("C:/Users/maria/OneDrive/Documentos/Brasil/mestrado/Diretorio R/Metaanalysis/Analysis")
#Loading packages
library(metafor)
library(readxl)
library(dplyr)
library(ggpubr)
#Loading data
#######################################################
### Calculating control x treatment ,effect size#######
#######################################################
data<- read_excel("C:/Users/maria/OneDrive/Documentos/Brasil/mestrado/Diretorio R/Metaanalysis/Analysis/data_te.xlsx")
str(data)
#Transforming numeric
data$MEANc<-as.numeric(data$MEANc)
data$MEANn<-as.numeric(data$MEANn)
data$SDc<-as.numeric(data$SDc)
data$SDn<-as.numeric(data$SDn)
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
