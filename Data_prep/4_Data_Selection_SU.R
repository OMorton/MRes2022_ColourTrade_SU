# setwd("G:/My Drive/TUoS/Teaching/Masters/2022/Shriya Uday/Project")
.libPaths("C:/Packages")

library(tidyverse)

CITESFull_ER <- data.table::fread("G:/My Drive/TUoS/Teaching/Masters/2022/Dom_Meeks/Project/Data/3_CITES_Vert_dat_ER.csv") %>% select(-V1)
CITESFull_IR <- data.table::fread("G:/My Drive/TUoS/Teaching/Masters/2022/Dom_Meeks/Project/Data/3_CITES_Vert_dat_IR.csv") %>% select(-V1)

## Focus on just the wild bird trade (but include live and not live)
## ER
Aves_CITES_Data <- CITESFull_ER %>% filter(Class == "Aves", Source_clean == "Wild") %>% group_by(Taxon, Exporter, Importer) %>%
  filter(sum(n) > 0)
length(unique(Aves_CITES_Data$Exporter))


write.csv(Aves_CITES_Data, "Data/4_SU_Aves_ER.csv", na = "")
