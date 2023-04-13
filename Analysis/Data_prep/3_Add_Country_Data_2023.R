# setwd("G:/My Drive/TUoS/Teaching/Masters/2022/Dom_Meeks/Project")
.libPaths("C:/Packages")

library(tidyverse)

#### Read in data ####
CITES_IUCN_data <- data.table::fread("G:/My Drive/TUoS/Teaching/Masters/2022/Dom_Meeks/Project/Data/2_CITES_Vert_Exp_Reported.csv", na.strings = "")
CITES_IUCN_data_IR <- data.table::fread("G:/My Drive/TUoS/Teaching/Masters/2022/Dom_Meeks/Project/Data/2_CITES_Vert_Imp_Reported.csv", na.strings = "")
NLP_dat <- data.table::fread("G:/My Drive/TUoS/Teaching/Masters/2022/Dom_Meeks/Project/Data/Party_NLP_Full.csv", na.strings = "") %>%
  select(ISO, Year) %>% mutate(ISO = ifelse(ISO == "NA", "NAM", ISO), Year = Year - 2000) %>%
  unite("Check", 1:2) %>% rbind(data.frame(Check = c("GG_0", "GG_1", "GG_2", "GG_3", "GG_4", "GG_5", "GG_6", "GG_7", "GG_8", "GG_9", "GG_10",
                                                     "GG_11", "GG_12", "GG_13", "GG_14", "GG_15" ,"GG_16", "GG_17", "GG_18", "GG_19", "GG_20")))

length(unique(CITES_IUCN_data$Taxon)) ## 1668
length(unique(CITES_IUCN_data_IR$Taxon)) ## 1679

## https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv
Alpha_codes <- data.table::fread("Data/Alpha_codes.csv", na.strings = "") %>% mutate(alpha_2 = ifelse(alpha_2 == "NA", "NAM", alpha_2))
Alpha_Exp <- Alpha_codes %>% select(1,2, 6,7) %>% rename(Exporter_Country = 1, Exporter = 2, Exporter_region = 3, Exporter_subregion = 4)
Alpha_Imp <- Alpha_codes %>% select(1,2, 6,7) %>% rename(Importer_Country = 1, Importer = 2, Importer_region = 3, Importer_subregion = 4)



#### Exporter reported ####

CITES_IUCN_Trade_Data_Match_ER <- CITES_IUCN_data %>% left_join(Alpha_Exp, by = "Exporter") %>% left_join(Alpha_Imp, by = "Importer")

## This removes exports from former yugoslavia and former serbia and Montenegro
filter(CITES_IUCN_Trade_Data_Match_ER, is.na(Exporter_Country)) %>% distinct(Exporter)
## This excludes various (XV), unknown (XX), former serbia and Montenegro, former yugoslavia, USSR and  KV
filter(CITES_IUCN_Trade_Data_Match_ER, is.na(Importer_Country)) %>% distinct(Importer)
Check <- filter(CITES_IUCN_Trade_Data_Match_ER, is.na(Importer_Country))

## Remove the above countries
CITES_Countries_ER <- CITES_IUCN_Trade_Data_Match_ER %>% filter(!is.na(Importer_Country), !is.na(Exporter_Country))
length(unique(CITES_Countries_ER$Name_for_CITESdb)) # 1666

## Check each party is only present for the series it is a party to cites

Check2 <- CITES_Countries_ER %>% unite("Check", c(6,8), remove = FALSE) %>% filter(!Check %in% NLP_dat$Check) %>% tally(n)
CITES_Countries_ER_Final <- CITES_Countries_ER %>% unite("Check", c(6,8), remove = FALSE) %>% 
  filter(Check %in% NLP_dat$Check) %>% select(-Check, - filt)

write.csv(CITES_Countries_ER_Final, "Data/3_CITES_Vert_dat_ER.csv", na = "")
write_rds(CITES_Countries_ER_Final, "Data/3_CITES_Vert_dat_ER.rds")




#### Importer reported ####

CITES_IUCN_Trade_Data_Match_IR <- CITES_IUCN_data_IR %>% left_join(Alpha_Exp, by = "Exporter") %>% left_join(Alpha_Imp, by = "Importer")

## This excludes various (XV), unknown (XX), former serbia and Montenegro, former yugoslavia, USSR and  the Holy see
filter(CITES_IUCN_Trade_Data_Match_IR, is.na(Exporter_Country)) %>% distinct(Exporter)
## This excludes  former serbia and Montenegro, former yugoslavia
filter(CITES_IUCN_Trade_Data_Match_IR, is.na(Importer_Country)) %>% distinct(Importer)
Check <- filter(CITES_IUCN_Trade_Data_Match_IR, is.na(Importer_Country))

## Remove the above countries
CITES_Countries_IR <- CITES_IUCN_Trade_Data_Match_IR %>% filter(!is.na(Importer_Country), !is.na(Exporter_Country))
length(unique(CITES_Countries_IR$Name_for_CITESdb)) # 1675

## Check each party is only present for the series it is a party to cites

Check2 <- CITES_Countries_IR %>% unite("Check", c(7,8), remove = FALSE) %>% filter(!Check %in% NLP_dat$Check) %>% tally(n)
CITES_Countries_IR_Final <- CITES_Countries_IR %>% unite("Check", c(7,8), remove = FALSE) %>% filter(Check %in% NLP_dat$Check) %>% select(-Check, - filt)

write.csv(CITES_Countries_IR_Final, "Data/3_CITES_Vert_dat_IR.csv", na = "")
write_rds(CITES_Countries_IR_Final, "Data/3_CITES_Vert_dat_IR.rds")
