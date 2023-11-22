#Grafico grau de degradação e tipo de controle
#########################################
#juntando os dois gráficos no mesmo painel
X11(width = 12, height = 6)
layout(matrix(c(1, 2),ncol=2, nrow=1), width=c(6, 6))
layout.show(2) 
par (mar = c(5, 12, 4, 3.5), bty = "n" )
plot(x = NULL, y = NULL, xlim = c(-7, 6), ylim = c(0.5, 5), 
     type = "n", yaxt = "n", xlab = "Effect Size (Hedges´d)", 
     ylab = "",  main = " A) STUDY AREA")
#linhas guias e eixo#
#funcao abline, lty para definir o tipo de linha, 2 e tracejada#
abline (v = 0, lty = 2) 
#legendas
axis(side = 2, at = c(1,2,3,4),
     labels=c("without tree degraded (3)",
              "without tree intermediate (4)","with tree degraded (8)",
              "with tree intermediate (12)"),las =2,tick = FALSE,lty="blank")
# without tree degraded
points(x = 2.8107, y = 1, pch = 19) # pch: tipo de simbolo
points(x = 0.5989, y = 1, pch = "|", cex = 1.2)
points(x = 5.0225, y = 1, pch = "|", cex = 1.2)
segments(x0 = 0.5989, y0 = 1, x1 = 5.0225, y1 = 1) # um segmento
# without tree intermediate
points(x = -3.8738, y = 2, pch = 19) # pch: tipo de simbolo
points(x = -6.9631, y = 2, pch = "|", cex = 1.2)
points(x = -0.7845, y = 2, pch = "|", cex = 1.2)
segments(x0 = -6.9631, y0 = 2, x1 = -0.7845, y1 = 2) # um segmento
#with tree degraded
points(x = -1.6131, y = 3, pch = 19) # pch: tipo de simbolo
points(x = -4.2164, y = 3, pch = "|", cex = 1.2)
points(x = 0.9903, y = 3, pch = "|", cex = 1.2)
segments(x0 = -4.2164, y0 = 3, x1 = 0.9903, y1 = 3) # um segmento
#with tree intermediate
points(x = 2.0111, y = 4, pch = 19) # pch: tipo de simbolo
points(x = -1.5155, y = 4, pch = "|", cex = 1.2)
points(x = 5.5377, y = 4, pch = "|", cex = 1.2)
segments(x0 = -1.5155, y0 = 4, x1 = 5.5377, y1 = 4) # um segmento
#segundo grafico
par (mar = c(5, 12, 4, 3.5), bty = "n" )
plot(x = NULL, y = NULL, xlim = c(-7, 6), ylim = c(0.5, 5), 
     type = "n", yaxt = "n", xlab = "Effect Size (Hedges´d)", 
     ylab = "",  main = "B) ORIGIN OF CONTROL TREES")
#linhas guias e eixo#
#funcao abline, lty para definir o tipo de linha, 2 e tracejada#
abline (v = 0, lty = 2) 
#legendas
axis(side = 2, at = c(1.5,2.5,3.5), 
     labels=c("native trees (4)","exotic trees (8)", 
              "native and exotic trees (8)"), 
     las =2,tick = FALSE,lty="blank")
# native trees
points(x = -2.5017, y = 1.5, pch = 19) # pch: tipo de simbolo
points(x = -0.0672, y = 1.5, pch = "|", cex = 1.2)
points(x = -4.9362, y = 1.5, pch = "|", cex = 1.2)
segments(x0 = -4.9362, y0 = 1.5, x1 = -0.0672, y1 = 1.5) # um segmento
# exotic trees
points(x = 1.2119, y = 2.5, pch = 19) # pch: tipo de simbolo
points(x = -0.2794, y = 2.5, pch = "|", cex = 1.2)
points(x = 2.7031, y = 2.5, pch = "|", cex = 1.2)
segments(x0 = -0.2794, y0 = 2.5, x1 = 2.7031, y1 = 2.5) # um segmento
# native and exotic trees
points(x = -1.5872, y = 3.5, pch = 19) # pch: tipo de simbolo
points(x = -3.5955, y = 3.5, pch = "|", cex = 1.2)
points(x = 0.4212, y = 3.5, pch = "|", cex = 1.2)
segments(x0 = -3.5955, y0 = 3.5, x1 = 0.4212, y1 = 3.5) # um segmento
savePlot("metaGraf2.png", type = "png")
help(axis)
#Installing ORchard
#install.packages("pacman")
rm(list = ls())
devtools::install_github("daniel1noble/orchaRd", ref = "main", force = TRUE)
pacman::p_load(devtools, tidyverse, metafor, patchwork, R.rsp, orchaRd, emmeans, ape, phytools, flextable)
data(english)
english
# We need to calculate the effect sizes, in this case d
english <- escalc(measure = "SMD", n1i = NStartControl, sd1i = SD_C, m1i = MeanC, n2i = NStartExpt, sd2i = SD_E, m2i = MeanE, 
                  var.names=c("SMD","vSMD"),
                  data = english)

english_MA <- rma.mv(yi = SMD, V = vSMD, random = list( ~ 1 | StudyNo, ~ 1 | EffectID), data = english)
summary(english_MA)
model_results <- orchaRd::mod_results(english_MA, mod = "1", at = NULL,  group = "StudyNo")
model_results
orchaRd::orchard_plot(english_MA, mod="1", group = "StudyNo", xlab = "Standardised mean difference", 
                      transfm = "none") 
# a caterpillar plot (not a caterpillars plot)
orchaRd::caterpillars(model_results, mod="1", xlab = "Standardised mean difference") 
#funnel plots
X11(width = 12, height = 6)
layout(matrix(c(1,2,1,3),ncol=2, nrow=2), width=c(6, 6))
layout.show(3)

#modelo geral
cols <- palette.colors(length(unique(data$study_id)), palette="polychrome")
cols <- cols[as.numeric(factor(data$study_id))]
funnel(data$yi, data$vi, yaxis = "seinv", ylab = "Precision (1/SE)", xlab = "Effect size (SMD)", col = cols, main ="A")
#model conservation and control type
funnel(data$yi, data$vi, yaxis = "seinv", ylab = "Precision (1/SE)", xlab = "Effect size (SMD)", col = cols, main ="B")
#model with control trees
datact<-data[data$control_type2 == "tree",]
datact
cols <- palette.colors(length(unique(datact$study_id)), palette="polychrome")
cols <- cols[as.numeric(factor(datact$study_id))]
funnel(datact$yi, datact$vi, yaxis = "seinv", ylab = "Precision (1/SE)", xlab = "Effect size (SMD)", col = cols, main ="C")
savePlot("funnelplots.png", type = "png")
#Sensitivity plots
X11(width = 12, height = 12)
layout(matrix(c(1,2,3),ncol=1, nrow=3), width=c(4, 4))
layout.show(3)
###first model geral###
#If residual standard >3 AND hatvalue >2 times the average of hatvalues, 
#run analysis with those cases deleted to test for sensitivity (from Habeck & Schultz 2015).
rs.modelctype <- rstandard(modelctype)
rs.modelctype
hat.modelctype <- hatvalues(modelctype) / mean(hatvalues(modelctype))
hatvalues(modelctype)
plot(hat.modelctype, rs.modelctype$resid, xlab="Hat/average hat value", ylab= "Standard residuals", 
     xlim=c(0,4), ylim=c(-6,4), cex.lab=1.2, main= "A")
abline (h=-3)
abline (h=3)
abline (v=(2))
#model with degradation and control type
rs.model_gint <- rstandard(model_gint)
hat.model_gint <- hatvalues(model_gint) / mean(hatvalues(model_gint))
plot(hat.model_gint, rs.model_gint$resid, xlab="Hat/average hat value", ylab= "Standard residuals", 
     xlim=c(0,4), ylim=c(-4,5), cex.lab=1.2, main = "B")
abline (h=-3)
abline (h=3)
abline (v=(2))
#model with control type
rs.modeloct <- rstandard(model_oct)
hat.modeloct <- hatvalues(model_oct) / mean(hatvalues(model_oct))
plot(hat.modeloct, rs.modeloct$resid, xlab="Hat/average hat value", ylab= "Standard residuals", 
     xlim=c(0,4), ylim=c(-4,5), cex.lab=1.2, main = "C")
abline (h=-3)
abline (h=3)
abline (v=(2))
savePlot("sensitivityplots.png", type = "png")