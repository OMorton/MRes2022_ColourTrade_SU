
Historic_CITES <- Historic_CITES %>% filter(Order == "Passeriformes") 

## Get the unique listings from the listing data/
## This tidies and gets the first year a species is CITES listed (the start of its possible time series)
Addition <- Historic_CITES %>% group_by(Class, Order, Family, Genus, FullName, Year, ChangeType, Appendix, RankName) %>% 
  tally() %>%
  mutate(FullName = ifelse(FullName == Order, NA, FullName),
         FullName = ifelse(FullName == Family, NA, FullName),
         FullName = ifelse(FullName == Genus, NA, FullName)) %>%
  rename(Taxon = FullName) %>%
  ungroup() %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.)))) %>%
  filter(Class == "Aves", ChangeType == "ADDITION") %>% ungroup()

Deletion <- Historic_CITES %>% group_by(Class, Order, Family, Genus, FullName, Year, ChangeType, Appendix, RankName) %>% 
  tally() %>%
  mutate(FullName = ifelse(FullName == Order, NA, FullName),
         FullName = ifelse(FullName == Family, NA, FullName),
         FullName = ifelse(FullName == Genus, NA, FullName)) %>%
  rename(Taxon = FullName) %>%
  ungroup() %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.)))) %>%
  filter(Class == "Aves", ChangeType == "DELETION") %>% ungroup()

Taxonomy <- Addition %>% group_by(Class, Order, Family, Genus, Taxon,RankName) %>% tally() %>% select(-n)

## First match all species level CITES listings
A_SP <- Addition %>% filter(RankName == "SPECIES") %>% select(Taxon, Year, Appendix) %>% mutate(Year = as.integer(Year))

D_SP <- Deletion %>% filter(RankName == "SPECIES") %>% select(Taxon, Year, Appendix) %>% mutate(Year = as.integer(Year))

sp_done_A <- A_SP %>% filter(!is.na(Year)) %>% mutate(ADD = "ADD")
sp_done_D <- D_SP %>% filter(!is.na(Year)) %>% mutate(DEL = "DEL")

Sp_series <- expand.grid(Taxon = unique(sp_done_A$Taxon), Year = 1975:2020) %>% 
  left_join(select(sp_done_A, Taxon, Year, Appendix, ADD)) %>%
  left_join(select(sp_done_D, Taxon, Year, DEL), by = c("Taxon", "Year")) %>%
  mutate(Change = case_when(ADD == "ADD" ~ "ADD",
                            ADD == "ADD" & DEL == "DEL" ~ "ADD",
                            DEL == "DEL" ~ "DEL",
                            TRUE ~ ADD)) %>%
  arrange(Taxon, Year) %>% group_by(Taxon) %>% 
  fill(Change, .direction = "down") %>%
  fill(Appendix, .direction = "down") %>%
  filter(!is.na(Appendix)) %>%
  mutate(Appendix = ifelse(Change == "DEL", "Not listed", Appendix)) %>%
  left_join(filter(Taxonomy, RankName == "SPECIES"))

check <- Sp_series %>% group_by(Taxon) %>% filter(n_distinct(Appendix) > 1)
n_distinct(Sp_series$Taxon) ## 108

## Second match at genus level
genus_to_match <- Addition %>% filter(is.na(Taxon)) %>% select(-Year) %>% select(Taxon, Genus, Family, Order)
## get all the genus level appendix listings
A_GEN <- Addition %>% filter(is.na(Taxon), !is.na(Genus)) %>% select(Genus, Year, Appendix)%>% mutate(Year = as.integer(Year))

D_GEN <- Deletion %>% filter(is.na(Taxon), !is.na(Genus)) %>% select(Genus, Year, Appendix) %>% mutate(Year = as.integer(Year))

GEN_done_A <- A_GEN %>% filter(!is.na(Year)) %>% mutate(ADD = "ADD")
GEN_done_D <- D_GEN %>% filter(!is.na(Year)) %>% mutate(DEL = "DEL")

GEN_series <- expand.grid(Genus = unique(GEN_done_A$Genus), Year = 1975:2020) %>% 
  left_join(select(GEN_done_A, Genus, Year, Appendix, ADD)) %>%
  left_join(select(GEN_done_D, Genus, Year, DEL), by = c("Genus", "Year")) %>%
  mutate(Change = case_when(ADD == "ADD" ~ "ADD",
                            ADD == "ADD" & DEL == "DEL" ~ "ADD",
                            DEL == "DEL" ~ "DEL",
                            TRUE ~ ADD)) %>%
  arrange(Genus, Year) %>% group_by(Genus) %>% 
  fill(Change, .direction = "down") %>%
  fill(Appendix, .direction = "down") %>%
  filter(!is.na(Appendix)) %>%
  mutate(Appendix = ifelse(Change == "DEL", "Not listed", Appendix)) %>%
  left_join(filter(Taxonomy, RankName == "GENUS"))

n_distinct(GEN_series$Genus) ## 1


## third match at family level
## get all the family level listings 53 listings
## Second match at genus level
fam_to_match <- Addition %>% filter(is.na(Genus)) %>% select(-Year) %>% select(Taxon, Family, Order)
## get all the genus level appendix listings
A_FAM <- Addition %>% filter(is.na(Taxon), is.na(Genus), !is.na(Family)) %>% select(Family, Year, Appendix)%>% mutate(Year = as.integer(Year))

D_FAM <- Deletion %>% filter(is.na(Taxon), is.na(Genus), !is.na(Family)) %>% select(Family, Year, Appendix) %>% mutate(Year = as.integer(Year))

FAM_done_A <- A_FAM %>% filter(!is.na(Year)) %>% mutate(ADD = "ADD")
FAM_done_D <- D_FAM %>% filter(!is.na(Year)) %>% mutate(DEL = "DEL")

FAM_series <- expand.grid(Family = unique(FAM_done_A$Family), Year = 1975:2020) %>% 
  left_join(select(FAM_done_A, Family, Year, Appendix, ADD)) %>%
  left_join(select(FAM_done_D, Family, Year, DEL), by = c("Family", "Year")) %>%
  mutate(Change = case_when(ADD == "ADD" ~ "ADD",
                            ADD == "ADD" & DEL == "DEL" ~ "ADD",
                            DEL == "DEL" ~ "DEL",
                            TRUE ~ ADD)) %>%
  arrange(Family, Year) %>% group_by(Family) %>% 
  fill(Change, .direction = "down") %>%
  fill(Appendix, .direction = "down") %>%
  filter(!is.na(Appendix)) %>%
  mutate(Appendix = ifelse(Change == "DEL", "Not listed", Appendix)) %>%
  left_join(filter(Taxonomy, RankName == "FAMILY"))

n_distinct(FAM_series$Family) ## 3

## Fourth match at ssp level
ssp_to_match <- Addition %>% filter(RankName == "SUBSPECIES") %>% select(-Year) %>% select(Taxon, Family, Order)
## get all the ssp level appendix listings
A_ssp <- Addition %>% filter(RankName == "SUBSPECIES") %>% 
  select(Taxon, Year, Appendix) %>% mutate(Year = as.integer(Year))

D_ssp <- Deletion %>% filter(RankName == "SUBSPECIES") %>%
  select(Taxon, Year, Appendix)%>% mutate(Year = as.integer(Year))

Ssp_done_A <- A_ssp %>% filter(!is.na(Year)) %>% mutate(ADD = "ADD")
Ssp_done_D <- D_ssp %>% filter(!is.na(Year)) %>% mutate(DEL = "DEL")

Ssp_series <- expand.grid(Taxon = unique(Ssp_done_A$Taxon), Year = 1975:2020) %>% 
  left_join(select(Ssp_done_A, Taxon, Year, Appendix, ADD)) %>%
  left_join(select(Ssp_done_D, Taxon, Year, DEL), by = c("Taxon", "Year")) %>%
  mutate(Change = case_when(ADD == "ADD" ~ "ADD",
                            ADD == "ADD" & DEL == "DEL" ~ "ADD",
                            DEL == "DEL" ~ "DEL",
                            TRUE ~ ADD)) %>%
  arrange(Taxon, Year) %>% group_by(Taxon) %>% 
  fill(Change, .direction = "down") %>%
  fill(Appendix, .direction = "down") %>%
  filter(!is.na(Appendix)) %>%
  mutate(Appendix = ifelse(Change == "DEL", "Not listed", Appendix)) %>%
  left_join(filter(Taxonomy, RankName == "SUBSPECIES"))

n_distinct(Ssp_series$Taxon) ## 3

All_sp_fl <- rbind(Sp_series, GEN_series, FAM_series, Ssp_series)

All_sp_fl_2000 <- All_sp_fl %>% filter(Year > 1999) %>% select(-ADD, -DEL, - Change) %>%
  group_by(Taxon, Genus, Family) %>%
  filter(!all(Appendix == "Not listed"))

write.csv(All_sp_fl_2000, "Outputs/CITES/Listed_Passeriformes_2000_2020.csv")
