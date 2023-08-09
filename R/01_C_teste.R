library("here")
data_csv <- read.csv(here::here("Data/processed", "data_te.csv"), sep = ";")
data_csv <- read.csv("~/Brasil/mestrado/metaanalysis/Data/processed/data_te.csv",
                     sep = ";",
                     stringsAsFactors = FALSE,
                     fileEncoding = "latin1")
