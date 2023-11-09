#Grafico grau de degradação e tipo de controle
#Abrindo o dispositivo#
graphics.off()
X11(width = 12, height = 8)
#Criando o layout#
#funcao layout mais flexivel que mfrow e permite criar paineis de diferentes tamanhos#
#criando duas colunas, esquerda 80% da largura total direita 20%#
layout(matrix(1, ncol = 1, nrow = 1)
layout.show(1) 
# mostra o layout dos dois paineis
#Primeiro definimos os parametros graficos globais do primeiro panel, logo espaco de coordenadas
#sem nenhum elemento#
par (mar = c(5, 12, 4, 3.5), bty = "n" )
plot(x = NULL, y = NULL, xlim = c(-7, 6), ylim = c(0.5, 7.5), 
     type = "n", yaxt = "n", xlab = "Effect Size (Hedges´d)", 
     ylab = "",  main = "STUDY AREA")
#linhas guias e eixo#
#funcao abline, lty para definir o tipo de linha, 2 e tracejada#
abline (v = 0, lty = 2) 
#legendas
axis(side = 2, at = c(1,3,5,7), labels=c("without tree degraded (3)", 
      "without tree intermediate (4)", "with tree degraded (8)", 
      "with tree intermediate (12)"),las =2)
# without tree degraded
points(x = 2.8107, y = 1, pch = 19) # pch: tipo de simbolo
points(x = 0.5989, y = 1, pch = "|", cex = 1.2)
points(x = 5.0225, y = 1, pch = "|", cex = 1.2)
segments(x0 = 0.5989, y0 = 1, x1 = 5.0225, y1 = 1) # um segmento
# without tree intermediate
points(x = -3.8738, y = 3, pch = 19) # pch: tipo de simbolo
points(x = -6.9631, y = 3, pch = "|", cex = 1.2)
points(x = -0.7845, y = 3, pch = "|", cex = 1.2)
segments(x0 = -6.9631, y0 = 3, x1 = -0.7845, y1 = 3) # um segmento
#with tree degraded
points(x = -1.6131, y = 5, pch = 19) # pch: tipo de simbolo
points(x = -4.2164, y = 5, pch = "|", cex = 1.2)
points(x = 0.9903, y = 5, pch = "|", cex = 1.2)
segments(x0 = -4.2164, y0 = 5, x1 = 0.9903, y1 = 5) # um segmento
#with tree intermediate
points(x = 2.0111, y = 7, pch = 19) # pch: tipo de simbolo
points(x = -1.5155, y = 7, pch = "|", cex = 1.2)
points(x = 5.5377, y = 7, pch = "|", cex = 1.2)
segments(x0 = -1.5155, y0 = 7, x1 = 5.5377, y1 = 7) # um segmento
help(points)
savePlot("metaGraf1.png", type = "png")
#########################################
#juntando os dois gráficos no mesmo painel
X11(width = 12, height = 6)
layout(matrix(c(1, 2),ncol=2, nrow=1), width=c(6, 6))
layout.show(2) 
par (mar = c(5, 12, 4, 3.5), bty = "n" )
plot(x = NULL, y = NULL, xlim = c(-7, 6), ylim = c(0.5, 5), 
     type = "n", yaxt = "n", xlab = "Effect Size (Hedges´d)", 
     ylab = "",  main = "STUDY AREA")
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
     ylab = "",  main = "ORIGIN OF CONTROL TREES")
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
