setwd("/Brasil/mestrado/Diretorio R/Primeiras buscas meta analise/Testando com sucesao")
#abrimos os pacotes
library(devtools)
library(revtools)
library(stringi)
library(stringr)
library(remotes)
library(litsearchr)
library(synthesisr)
library(data.table)
library(openxlsx)
remotes::install_github("ropensci/litsearchr")
install.packages("remotes")
library(remotes)
install_github("elizagrames/litsearchr", ref="main")
search_directory<-("/Brasil/mestrado/Diretorio R/Primeiras buscas meta analise/Testando com sucesao")
#importamos os dados
naiveimport<-litsearchr::import_results(directory = "/Brasil/mestrado/Diretorio R/Primeiras buscas meta analise/Testando com sucesao", verbose = TRUE)
colnames(naiveimport)
naiveresults <- 
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")
nrow(naiveresults)
#salvamos o arquivo
write.xlsx (naiveresults, "naiveresults.xlsx")
AAA <- read.xlsx("naiveresults.xlsx", colNames = TRUE, cols=c(18,2,8,10,9,38,39))
setcolorder (AAA, c(5,1,2,4,3,6,7))
write.xlsx (AAA, "literature.xlsx")