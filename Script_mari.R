#Script mari
####conferencia pontos

local2 <- "C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Mestrado/mestrado/metaanalysis/"
install.packages("readxl")
library(readxl)
data<- read_excel("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Mestrado/mestrado/metaanalysis/holaa.xlsx") 
#A planilha tem os artigos novos, e os artigos já triados, todos juntos
data <- read.table("hola.csv",  sep=";", header = T)
data
#criando a lista das especies q quero comparar
#nessa lista vão ser os trabalhos que já foram triados
lista1 <- data[data$grupo=="triado",]
#nessa lista vão ser os trabalhos novos
lista2 <- data[data$grupo=="novo",]
#Crio objetos para ter só os titulos de cada grupo
a <- lista1$title
b <- lista2$title

#comparando pontos entre grupos, triados e novos
pontos_dif <- setdiff(b, a)
View(pontos_dif)
pontos_dif
#intersect(a,b)

write.table(pontos_dif,"C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Mestrado/mestrado/metaanalysis/complist.csv", sep=",",dec=".")
