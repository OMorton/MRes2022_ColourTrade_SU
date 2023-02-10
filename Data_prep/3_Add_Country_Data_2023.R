
# setwd("G:/My Drive/TUoS/Teaching/Masters/2022/Dom_Meeks/Project")
.libPaths("C:/Packages")

library(rredlist)
library(tidyverse)

CITES_Vert <- data.table::fread("Data/1_Output_CITES_Raw.csv", na.strings = "")

## 1861 species in both Importer and Exported Data (3 less than 1_CITES... as hatinesis merged with edwardsii and 2 unlisted species removed)
Sp_List <- CITES_Vert %>% group_by(Taxon) %>% tally() %>% select(Taxon) %>% as.data.frame()
CITES_Vert %>% filter(Year > 1999) %>% group_by(Source_clean, Live) %>% summarise(n_distinct(Taxon))

## 24 total 
##  1 domestic species.
## 1 wild goat to keep
## 2 hybirds to remove.
## 5 sub species specific statuses to manually add.
## 15 never evaluated species.
NA_update <- Sp_List %>% mutate(IUCNName = case_when(Taxon == "Aceros cassidix" ~ "Rhyticeros cassidix",
                                                     Taxon == "Aceros corrugatus" ~"Rhabdotorrhinus corrugatus",
                                                     Taxon == "Aceros leucocephalus" ~ "Rhabdotorrhinus leucocephalus",
                                                     Taxon == "Aceros waldeni" ~ "Rhabdotorrhinus waldeni",
                                                     Taxon == "Aglaiocercus kingi" ~ "Aglaiocercus kingii",
                                                     Taxon == "Alisterus chloropterus mozskowskii" ~ "Alisterus chloropterus",
                                                     Taxon == "Amazilia amazilia" ~ "Amazilis amazilia",
                                                     Taxon == "Amazilia cyanocephala" ~ "Saucerottia cyanocephala",
                                                     Taxon == "Amazilia chionogaster" ~ "Elliotomyia chionogaster",
                                                     Taxon == "Amazilia julie" ~ "Chlorestes julie",
                                                     Taxon == "Amazona festiva festiva" ~ "Amazona festiva",
                                                     Taxon == "Amazona mercenaria" ~ "Amazona mercenarius",
                                                     Taxon == "Amazona xanthops" ~ "Alipiopsitta xanthops",
                                                     Taxon == "Ameerega altamazonica" ~ "Ameerega altamazonica", ## NE
                                                     Taxon == "Ameerega maculata" ~ "Epipedobates maculatus",
                                                     Taxon == "Anas clypeata" ~ "Spatula clypeata",
                                                     Taxon == "Anas formosa" ~ "Sibirionetta formosa",
                                                     Taxon == "Anas penelope" ~ "Mareca penelope",
                                                     Taxon == "Anas querquedula" ~ "Spatula querquedula",
                                                     Taxon == "Aonyx capensis microdon" ~ "Aonyx capensis",
                                                     Taxon == "Aonyx cinerea" ~ "Aonyx cinereus",
                                                     Taxon == "Aquila clanga" ~ "Clanga clanga",
                                                     Taxon == "Aquila pomarina" ~ "Clanga pomarina",
                                                     Taxon == "Aratinga acuticaudata" ~ "Psittacara acuticaudatus",
                                                     Taxon == "Aratinga aurea" ~ "Eupsittula aurea",
                                                     Taxon == "Aratinga cactorum" ~ "Eupsittula cactorum",
                                                     Taxon == "Aratinga canicularis" ~ "Eupsittula canicularis",
                                                     Taxon == "Aratinga chloroptera" ~ "Psittacara chloropterus",
                                                     Taxon == "Aratinga erythrogenys" ~ "Psittacara erythrogenys",
                                                     Taxon == "Aratinga euops" ~ "Psittacara euops",
                                                     Taxon == "Aratinga finschi" ~ "Psittacara finschi",
                                                     Taxon == "Aratinga holochlora" ~ "Psittacara holochlorus",
                                                     Taxon == "Aratinga leucophthalma" ~ "Psittacara leucophthalmus",
                                                     Taxon == "Aratinga mitrata" ~ "Psittacara mitratus",
                                                     Taxon == "Aratinga nana" ~ "Eupsittula nana",
                                                     Taxon == "Aratinga pertinax" ~ "Eupsittula pertinax",
                                                     Taxon == "Aratinga wagleri" ~ "Psittacara wagleri",
                                                     Taxon == "Baillonius bailloni" ~ "Pteroglossus bailloni",
                                                     Taxon == "Barnardius barnardi" ~ "Barnardius zonarius",
                                                     Taxon == "Branta canadensis leucopareia" ~ "Branta canadensis",
                                                     Taxon == "Boa constrictor occidentalis" ~ "Boa constrictor",
                                                     Taxon == "Bubo bubo bengalensis" ~ "Bubo bubo",
                                                     Taxon == "Buceros hydrocorax hydrocorax" ~ "Buceros hydrocorax",
                                                     Taxon == "Buteo magnirostris" ~ "Rupornis magnirostris",
                                                     Taxon == "Buteo poecilochrous" ~ "Geranoaetus polyosoma",
                                                     Taxon == "Buteo polyosoma" ~ "Geranoaetus polyosoma",
                                                     Taxon == "Calyptorhynchus baudinii" ~ "Zanda baudinii",
                                                     Taxon == "Calyptorhynchus funereus" ~ "Zanda funerea",
                                                     Taxon == "Calyptorhynchus latirostris" ~ "Zanda latirostris",
                                                     Taxon == "Callithrix melanura" ~ "Mico melanurus",
                                                     Taxon == "Capra falconeri heptneri" ~ "Capra falconeri",
                                                     Taxon == "Capra hircus aegagrus" ~ "Capra hircus", # wild goat (not domestic - keep)
                                                     Taxon == "Capricornis thar" ~ "Capricornis sumatraensis",
                                                     Taxon == "Carduelis cucullata" ~ "Spinus cucullatus",
                                                     Taxon == "Carduelis yarrellii" ~ "Spinus yarrellii", 
                                                     Taxon == "Chalcopsitta sintillata" ~ "Chalcopsitta scintillata",
                                                     Taxon == "Cebus xanthosternos" ~ "Sapajus xanthosternos",
                                                     Taxon == "Cervus elaphus barbarus" ~ "Cervus elaphus",
                                                     Taxon == "Chlorostilbon swainsonii" ~ "Riccordia swainsonii",
                                                     Taxon == "Colinus virginianus ridgwayi" ~ "Colinus virginianus",
                                                     Taxon == "Crotalus durissus unicolor" ~ "Crotalus durissus",
                                                     Taxon == "Dama dama mesopotamica" ~ "Dama mesopotamica",
                                                     Taxon == "Damophila julie" ~ "Chlorestes julie",
                                                     Taxon == "Eos rubra" ~ "Eos bornea",
                                                     Taxon == "Eos squamata riciniata" ~ "Eos squamata",
                                                     Taxon == "Estrilda caerulescens" ~ "Estrilda coerulescens",
                                                     Taxon == "Eupodotis vigorsii" ~ "Heterotetrax vigorsii",
                                                     Taxon == "Falco pelegrinoides" ~ "Falco peregrinus",
                                                     Taxon == "Falco pelegrinoides babylonicus" ~ "Falco peregrinus",
                                                     Taxon == "Falco peregrinus anatum" ~ "Falco peregrinus",
                                                     Taxon == "Falco peregrinus peregrinus" ~ "Falco peregrinus",
                                                     Taxon == "Grus canadensis pratensis" ~ "Grus canadensis",
                                                     Taxon == "Grus leucogeranus" ~ "Leucogeranus leucogeranus",
                                                     Taxon == "Guarouba guarouba" ~ "Guaruba guarouba",
                                                     Taxon == "Gyps rueppellii" ~ "Gyps rueppelli",
                                                     Taxon == "Hieraaetus fasciatus" ~ "Aquila fasciata",
                                                     Taxon == "Hieraaetus spilogaster" ~ "Aquila spilogaster",
                                                     Taxon == "Leptoptilos crumeniferus" ~ "Leptoptilos crumenifer",
                                                     Taxon == "Leucopternis albicollis" ~ "Pseudastur albicollis",
                                                     Taxon == "Lonchura bicolor" ~ "Spermestes bicolor",
                                                     Taxon == "Lonchura cantans" ~ "Euodice cantans",
                                                     Taxon == "Lonchura cucullata" ~ "Spermestes cucullata",
                                                     Taxon == "Lonchura fringilloides" ~ "Spermestes fringilloides",
                                                     Taxon == "Lonchura oryzivora" ~ "Padda oryzivora",
                                                     Taxon == "Lophura imperialis" ~ "Lophura imperialis", ## hybrid
                                                     Taxon == "Morelia boeleni" ~ "Simalia boeleni",
                                                     Taxon == "Nandayus nenday" ~ "Aratinga nenday",
                                                     Taxon == "Nigrita canicapilla" ~ "Nigrita canicapillus",
                                                     Taxon == "Nyctea scandiaca" ~ "Bubo scandiacus",
                                                     Taxon == "Orthopsittaca manilata" ~ "Orthopsittaca manilatus",
                                                     Taxon == "Otus asio" ~ "Megascops asio",
                                                     Taxon == "Otus choliba" ~ "Megascops choliba",
                                                     Taxon == "Otus kennicottii" ~ "Megascops kennicottii",
                                                     Taxon == "Otus roboratus" ~ "Megascops roboratus",
                                                     Taxon == "Otus watsonii" ~ "Megascops watsonii",
                                                     Taxon == "Paradisaea rudolphi" ~ "Paradisornis rudolphi",
                                                     Taxon == "Phoenicopterus ruber ruber" ~ "Phoenicopterus ruber",
                                                     Taxon == "Pionopsitta barrabandi" ~ "Pyrilia barrabandi",
                                                     Taxon == "Pionopsitta caica" ~ "Pyrilia caica",
                                                     Taxon == "Pitta guajana" ~ "Hydrornis guajanus",
                                                     Taxon == "Platycercus barnardi" ~ "Barnardius zonarius",
                                                     Taxon == "Platycercus zonarius" ~ "Barnardius zonarius",
                                                     Taxon == "Poephila cincta cincta" ~ "Poephila cincta",
                                                     Taxon == "Psephotus chrysopterygius" ~ "Psephotellus chrysopterygius",
                                                     Taxon == "Psephotus dissimilis" ~ "Psephotellus dissimilis",
                                                     Taxon == "Psephotus pulcherrimus" ~ "Psephotellus pulcherrimus",
                                                     Taxon == "Psephotus varius" ~ "Psephotellus varius",
                                                     Taxon == "Pseudoscops clamator" ~ "Asio clamator",
                                                     Taxon == "Psittacula calthorpae" ~ "Nicopsitta calthrapae",
                                                     Taxon == "Psittacula columboides" ~ "Nicopsitta columboides",
                                                     Taxon == "Psittacula cyanocephala" ~ "Himalayapsitta cyanocephala",
                                                     Taxon == "Psittacula echo" ~ "Alexandrinus eques",
                                                     Taxon == "Psittacula eupatria" ~ "Palaeornis eupatria",
                                                     Taxon == "Psittacula finschii" ~ "Himalayapsitta finschii",
                                                     Taxon == "Psittacula himalayana" ~ "Himalayapsitta himalayana",
                                                     Taxon == "Psittacula krameri" ~ "Alexandrinus krameri",
                                                     Taxon == "Psittacula longicauda" ~ "Belocercus longicaudus",
                                                     Taxon == "Psittacula roseata" ~ "Himalayapsitta roseata",
                                                     Taxon == "Psittacus erithacus timneh" ~ "Psittacus timneh",
                                                     Taxon == "Psitteuteles iris" ~ "Trichoglossus iris",
                                                     Taxon == "Pterocnemia pennata" ~ "Rhea pennata",
                                                     Taxon == "Pterocnemia pennata pennata" ~ "Rhea pennata",
                                                     Taxon == "Ptiloris magnificus" ~ "Lophorina magnifica",
                                                     Taxon == "Rhea americana albescens" ~ "Rhea americana",
                                                     Taxon == "Serinus leucopygius" ~ "Crithagra leucopygia",
                                                     Taxon == "Serinus mozambicus" ~ "Crithagra mozambica",
                                                     Taxon == "Spizaetus africanus" ~ "Aquila africana",
                                                     Taxon == "Spizaetus nipalensis" ~ "Nisaetus nipalensis",
                                                     Taxon == "Spizaetus cirrhatus" ~ "Nisaetus cirrhatus",
                                                     Taxon == "Spizastur melanoleucus" ~ "Spizaetus melanoleucus",
                                                     Taxon == "Streptopelia senegalensis" ~ "Spilopelia senegalensis",
                                                     Taxon == "Strix virgata" ~ "Ciccaba virgata",
                                                     Taxon == "Tauraco porphyreolophus" ~ "Gallirex porphyreolophus",
                                                     Taxon == "Torgos tracheliotus" ~ "Torgos tracheliotos",
                                                     Taxon == "Aldabrachelys gigantea" ~ "Geochelone gigantea",
                                                     Taxon == "Anaplectes melanotis" ~ "Anaplectes rubriceps",
                                                     Taxon == "Antilocapra americana mexicana" ~ "Antilocapra americana",
                                                     Taxon == "Bison bison athabascae" ~ "Bison bison",
                                                     Taxon == "Buteo albicaudatus" ~ "Geranoaetus albicaudatus",
                                                     Taxon == "Caiman crocodilus crocodilus" ~ "Caiman crocodilus",
                                                     Taxon == "Caiman crocodilus fuscus" ~ "Caiman crocodilus",
                                                     Taxon == "Caiman crocodilus yacare" ~ "Caiman yacare",
                                                     Taxon == "Callithrix argentata" ~ "Mico argentatus",
                                                     Taxon == "Callithrix pygmaea" ~ "Cebuella pygmaea",
                                                     Taxon == "Canis lupus crassodon" ~ "Canis lupus",
                                                     Taxon == "Canis lupus monstrabilis" ~ "Canis lupus",
                                                     Taxon == "Cebus apella" ~ "Sapajus apella",
                                                     Taxon == "Ceratotherium simum simum" ~ "Ceratotherium simum ssp. simum", ## Add manually
                                                     Taxon == "Cercocebus albigena" ~ "Lophocebus albigena",
                                                     Taxon == "Cercopithecus dryas" ~ "Chlorocebus dryas",
                                                     Taxon == "Cercopithecus kandti" ~ "Cercopithecus mitis ssp. kandti", ## Add manually
                                                     Taxon == "Cercopithecus lhoesti" ~ "Allochrocebus lhoesti",
                                                     Taxon == "Chelonoidis carbonarius" ~ "Chelonoidis carbonarius", ## NE
                                                     Taxon == "Chelonoidis denticulatus" ~ "Chelonoidis denticulata",
                                                     Taxon == "Crocodylus cataphractus" ~ "Mecistops cataphractus",
                                                     Taxon == "Cyclagras gigas" ~ "Hydrodynastes gigas",
                                                     Taxon == "Damaliscus pygargus pygargus" ~ "Damaliscus pygargus ssp. pygargus", ## Add manually
                                                     Taxon == "Diphyllodes magnificus" ~ "Cicinnurus magnificus",
                                                     Taxon == "Epicrates angulifer" ~ "Chilabothrus angulifer",
                                                     Taxon == "Epicrates chrysogaster" ~ "Chilabothrus chrysogaster",
                                                     Taxon == "Epicrates cenchria cenchria" ~ "Epicrates cenchria",
                                                     Taxon == "Epicrates fordii" ~ "Chilabothrus fordii",
                                                     Taxon == "Epicrates gracilis" ~ "Chilabothrus gracilis",
                                                     Taxon == "Epicrates inornatus" ~ "Chilabothrus inornatus",
                                                     Taxon == "Epicrates striatus" ~ "Chilabothrus striatus",
                                                     Taxon == "Epicrates subflavus" ~ "Chilabothrus subflavus",
                                                     Taxon == "Epipedobates bilinguis" ~ "Ameerega bilinguis",
                                                     Taxon == "Epipedobates darwinwallacei" ~ "Epipedobates darwinwallacei", ## NE
                                                     Taxon == "Epimachus fastuosus" ~ "Epimachus fastosus",
                                                     Taxon == "Equus przewalskii" ~ "Equus ferus",
                                                     Taxon == "Equus zebra hartmannae" ~ "Equus zebra ssp. hartmannae", ## Add manually
                                                     Taxon == "Equus zebra zebra" ~ "Equus zebra ssp. zebra", ## Add manually
                                                     Taxon == "Galago alleni" ~ "Sciurocheirus alleni",
                                                     Taxon == "Galago demidoff" ~ "Galagoides demidoff",
                                                     Taxon == "Gongylophis colubrinus" ~ "Eryx colubrinus",
                                                     Taxon == "Gongylophis conicus" ~ "Eryx conicus",
                                                     Taxon == "Gongylophis muelleri" ~ "Eryx muelleri",
                                                     Taxon == "Hexaprotodon liberiensis" ~ "Choeropsis liberiensis",
                                                     Taxon == "Homopus signatus" ~ "Chersobius signatus",
                                                     Taxon == "Hyaena brunnea" ~ "Parahyaena brunnea",
                                                     Taxon == "Karusaurus polyzonus" ~ "Karusasaurus polyzonus",
                                                     Taxon == "Kinixys belliana" ~ "Kinixys belliana", ## NE
                                                     Taxon == "Kinixys nogueyi" ~ "Kinixys nogueyi", ## NE
                                                     Taxon == "Kinixys spekii" ~ "Kinixys spekii", ## NE
                                                     Taxon == "Kinixys zombensis" ~ "Kinixys zombensis", ## NE
                                                     Taxon == "Lagothrix lagotricha" ~ "Lagothrix lagothricha",
                                                     Taxon == "Lagothrix cana" ~ "Lagothrix lagothricha",
                                                     Taxon == "Leiopython hoserae" ~ "Leiopython meridionalis",
                                                     Taxon == "Manis gigantea" ~ "Smutsia gigantea",
                                                     Taxon == "Manis temminckii" ~ "Smutsia temminckii",
                                                     Taxon == "Manis tetradactyla" ~ "Phataginus tetradactyla",
                                                     Taxon == "Manis tricuspis" ~ "Phataginus tricuspis",
                                                     Taxon == "Mauremys iversoni" ~ "Mauremys iversoni", ## hybrid
                                                     Taxon == "Mauremys pritchardi" ~ "Mauremys reevesii",
                                                     Taxon == "Mazama temama cerasina" ~ "Mazama temama",
                                                     Taxon == "Morelia clastolepis" ~ "Simalia clastolepis",
                                                     Taxon == "Morelia nauta" ~ "Simalia nauta",
                                                     Taxon == "Morelia tracyae" ~ "Simalia tracyae",
                                                     Taxon == "Mustela erminea ferghanae" ~ "Mustela erminea",
                                                     Taxon == "Nasua nasua solitaria" ~ "Nasua nasua",
                                                     Taxon == "Nigrita fusconota" ~ "Nigrita fusconotus",
                                                     Taxon == "Odocoileus virginianus mayensis" ~ "Odocoileus virginianus",
                                                     Taxon == "Ovis aries" ~ "Ovis aries", ## Domestic Sheep REMOVE
                                                     Taxon == "Ovis bochariensis" ~ "Ovis vignei",
                                                     Taxon == "Ovis cycloceros arkal" ~ "Ovis vignei",
                                                     Taxon == "Ovis darwini" ~ "Ovis ammon",
                                                     Taxon == "Ovis polii" ~ "Ovis ammon",
                                                     Taxon == "Pelodiscus axenaria" ~ "Pelodiscus axenaria", ## NE
                                                     Taxon == "Pelomedusa subrufa" ~ "Pelomedusa galeata",
                                                     Taxon == "Pelusios adansonii" ~ "Pelusios adansonii", ## NE
                                                     Taxon == "Pelusios castaneus" ~ "Pelusios castaneus", ## NE
                                                     Taxon == "Pelusios gabonensis" ~ "Pelusios gabonensis", ## NE
                                                     Taxon == "Penelopides exarhatus" ~ "Rhabdotorrhinus exarhatus",
                                                     Taxon == "Phelsuma andamanense" ~ "Phelsuma andamanensis",
                                                     Taxon == "Phrynosoma cerroense" ~ "Phrynosoma cerroense", ## NE
                                                     Taxon == "Podocnemis vogli" ~ "Podocnemis vogli", ## NE
                                                     Taxon == "Prionailurus bengalensis bengalensis" ~ "Prionailurus bengalensis",
                                                     Taxon == "Psammobates oculifer" ~ "Psammobates oculifer", ## NE
                                                     Taxon == "Psammobates tentorius trimeni" ~ "Psammobates tentorius",
                                                     Taxon == "Ptyas mucosus" ~ "Ptyas mucosa",
                                                     Taxon == "Puma concolor couguar" ~ "Puma concolor",
                                                     Taxon == "Python reticulatus" ~ "Malayopython reticulatus",
                                                     Taxon == "Python molurus molurus" ~ "Python molurus",
                                                     Taxon == "Python timoriensis" ~ "Malayopython timoriensis",
                                                     Taxon == "Rupicapra pyrenaica ornata" ~ "Rupicapra pyrenaica",
                                                     Taxon == "Saguinus fuscicollis" ~ "Leontocebus fuscicollis",
                                                     Taxon == "Saguinus nigricollis" ~ "Leontocebus nigricollis",
                                                     Taxon == "Serinus gularis" ~ "Crithagra gularis",
                                                     Taxon == "Trachypithecus villosus" ~ "Trachypithecus cristatus",
                                                     Taxon == "Tarsius syrichta" ~ "Carlito syrichta",
                                                     Taxon == "Trimeresurus mangshanensis" ~ "Protobothrops mangshanensis",
                                                     Taxon == "Ursus americanus emmonsii" ~ "Ursus americanus",
                                                     Taxon == "Uromastyx nigriventris" ~ "Uromastyx nigriventris", ## NE
                                                     Taxon == "Varanus ornatus" ~ "Varanus niloticus",
                                                     Taxon == "Varanus similis" ~ "Varanus scalaris",
                                                     Taxon == "Vipera wagneri" ~ "Montivipera wagneri",
                                                     Taxon == "Vulpes vulpes griffithi" ~ "Vulpes vulpes",
                                                     Taxon == "Vulpes vulpes montana" ~ "Vulpes vulpes",
                                                     Taxon == "Woodworthia maculatus" ~ "Woodworthia maculata",
                                                     Taxon == "Xenochrophis piscator" ~ "Fowlea piscator",
                                                     Taxon == "Zaglossus bruijni" ~ "Zaglossus bruijnii",
                                                     TRUE ~ Taxon))


#### Add the IUCN Statuses ####

## get key
## token can be generated here
## https://apiv3.iucnredlist.org/api/v3/token
apikey <- "a3fc116c122aefc621329055aeae8f67483e575c518acc14fcb77709bd94f6a2"

length(unique(NA_update$Taxon))
length(unique(NA_update$IUCNName))

## Dummy data frame for the loops
df <- data.frame(IUCNName = character(),
                 Year = character(),
                 IUCN_code = character(),
                 IUCN_cat = character())

## Get species historical statuses.
## Needs an API key and because of the delay needed between calls takes ~1 hour to run.
## Do not remove the delay see rredlist guide for details.
## https://cran.r-project.org/web/packages/rredlist/rredlist.pdf

for(i in 1:nrow(NA_update)){ # would have used for(sp in speciesList) but need i for progress bar?
  ## incorporate 2s delay between each query
  Sys.sleep(2)
  ## Progress update
  cat('Species=',i, '\n')
  ## get historical data from website
  sp <- NA_update$IUCNName[i]
  iucnHistory <- rl_history(name=sp, key=apikey)
  # IF species cannot be found
  if (length(iucnHistory$result) == 0){ 
    spDf <- data.frame(IUCNName = sp,
                       Year = NA,
                       IUCN_code = NA,
                       IUCN_cat = NA)
    df <- rbind(df, spDf)
    # cat('Check ', sp, '\n')
  } else { 
    spdf <- data.frame(IUCNName = sp,
                       Year = iucnHistory$result$year,
                       IUCN_code = iucnHistory$result$code,
                       IUCN_cat = iucnHistory$result$category)
    df <- rbind(df, spdf)
  }
}

## write out the the data for speed.
#write.csv(df, "Data/IUCN_API/IUCN_API_NA_ASSESSMENTS.csv")  
df <- read.csv("Data/IUCN_API/IUCN_API_NA_ASSESSMENTS.csv") %>% select(-X)

## 24 NA species (this is as expected as per the totals above)
df %>% filter(is.na(Year))

Ssp_code <- rbind(data.frame(Taxon = "Equus zebra zebra", IUCNName = "Equus zebra ssp. zebra", Year = c(1996, 2008, 2019), 
                             IUCN_code = c("EN", "VU", "LC")),
                  data.frame(Taxon = "Equus zebra hartmannae", IUCNName = "Equus zebra ssp. hartmannae", Year = c(1996, 2008, 2019), 
                             IUCN_code = c("EN", "VU", "VU")),
                  data.frame(Taxon = "Damaliscus pygargus pygargus", IUCNName = "Damaliscus pygargus ssp. pygargus", Year = c(1996, 2003, 2008, 2017), 
                             IUCN_code = c("VU", "VU", "NT", "VU")),
                  data.frame(Taxon = "Cercopithecus kandti", IUCNName = "Cercopithecus mitis ssp. kandti", Year = c(2000, 2008, 2020), 
                             IUCN_code = c("EN", "EN", "EN")),
                  data.frame(Taxon = "Ceratotherium simum simum", IUCNName = "Ceratotherium simum ssp. simum", Year = c(2000, 2020), 
                             IUCN_code = c("NT", "NT"))) %>% mutate(IUCN_cat = NA) %>% select(- Taxon)

df <- rbind(filter(df, !IUCNName %in% c("Equus zebra ssp. zebra", "Equus zebra ssp. hartmannae", "Damaliscus pygargus ssp. pygargus", 
                                        "Cercopithecus mitis ssp. kandti", "Ceratotherium simum ssp. simum")),
            Ssp_code)

## IUCN names will always be less than Taxon as some CITES sp/ssp resolve only to a single IUCN recognized taxon. 
length(unique(df$IUCNName)) ## 1819
df_all <- left_join(NA_update, df)
length(unique(df_all$Taxon)) ## 1861


#### Cleaning pre 2000 IUCN codes ####
## Check the early version of the IUCN are updated prior to 2000
## all instance where the following codes are given species are updated again before 2000
check <- df_all %>% group_by(Taxon) %>% filter(any(IUCN_code %in% c("CT", "NR", "K", "R", "T", "V", "E", "I", "nt", NA)))
length(unique(df_all$Taxon)) ## 1861


## Therefore remove these
Historic_IUCN <- df_all %>% 
  ## Set Not assessed species as not assessed in 2000 otherwise they are also NA for Year and get removed later when they shouldnt
  mutate(Year = if_else(is.na(Year), 2000, as.numeric(Year))) %>%
  filter(!IUCN_code %in% c("CT", "NR", "K", "R", "T", "V", "E", "I", "nt"))

length(unique(Historic_IUCN$Taxon)) ## 1861

## Only EX and EW species are a single Chelonoidis niger (in 2000) and 2 Oryx from wild sources in 2017 and 19.
## Cyanopsitta spixii, Equus przewalskii, Mustela nigripes, Phelsuma gigas, Psephotus pulcherrimus
## Keep these but flag them
Historic_IUCN %>% filter(IUCN_code %in% c("EX", "EW"))

## Convert the 1994 system to post-2000
Historic_IUCN_up <- Historic_IUCN %>%
  mutate(IUCN_code = replace(IUCN_code, IUCN_code == "LR/lc", "LC"),
         IUCN_code = replace(IUCN_code, IUCN_code == "LR/nt", "NT"),
         IUCN_code = replace(IUCN_code, IUCN_code == "LR/cd", "NT"))

## Check removal and conversion left only the post 2001 framework
unique(Historic_IUCN_up$IUCN_code)
unique(Historic_IUCN_up$Taxon) ## 1861

## Backbone of values 2000 - 2020
backbone <- expand.grid(Year = as.integer(1975:2020), Taxon = unique(Historic_IUCN_up$Taxon))

## left join this and create your unrolled status
## Some species in trade before being IUCN assessed these are the NA values
Historic_IUCN_up$Year <- as.integer(Historic_IUCN_up$Year)
backbone$Year <- as.integer(backbone$Year)

## Here we add the backbone of species and dates to the IUCN data
df_new <- left_join(backbone, Historic_IUCN_up) %>%
  arrange(Taxon, Year) %>% group_by(Taxon) %>% 
  fill(IUCN_code , .direction = "down") %>% 
  fill(IUCNName, .direction = "down") %>% 
  fill(IUCNName, .direction = "up") %>% 
  fill(Taxon, .direction = "down") %>% 
  fill(Taxon, .direction = "up") %>% 
  filter(Year %in% c(1999:2020)) %>% ungroup() %>%
  mutate(Year = as.numeric(Year) - 2000) %>% 
  select(Year, Taxon, IUCN_code, IUCNName, Taxon) %>% 
  mutate(IUCN_code = replace_na(IUCN_code, "Not assessed")) %>% distinct()

## Now we have a clean time series of all statuses for all species 1999 - 2020 (-1 - 20)
length(unique(df_new$Taxon)) ## 1861 species
## Tidy up and remove the two hybrids and the domestic sheep.
df_new <- df_new %>% filter(!Taxon %in% c("Lophura imperialis", "Mauremys iversoni", "Ovis aries"))
length(unique(df_new$Taxon)) ## 1858 species

## Check no species were assessed twice in one year (2 records for 1 year interfere with the joins)
df_new %>% group_by(Year, Taxon) %>% tally() %>% filter(n != 1)
df_new <- df_new %>% mutate(filt = paste(Taxon, Year, IUCN_code)) %>% filter(!filt %in% c("Macaca nemestrina 20 VU", "Homopus signatus 18 VU"))

#### Custom Species TimeFrames #####

## Tidy up and remove the two hybrids and the domestic sheep.
CITES_Vert_Clean <- CITES_Vert %>% filter(!Taxon %in% c("Lophura imperialis", "Mauremys iversoni", "Ovis aries"))
length(unique(CITES_Vert_Clean$Taxon)) ## 1858 species


#### Exporter Series ####
## Get all trades to volumes per species per year
CITES_Vert_pro <- CITES_Vert_Clean %>% filter(Reporter.type == "E") %>%
  ## Group and tally per species/year
  group_by(Year, Taxon, Class, Exporter, Importer, FL_year, Year_DEL, Source_clean, Live) %>% 
  tally(WOE) %>% group_by(Taxon, Class, Exporter) %>% filter(n() > 0) %>%
  ## here we remove record for species that are not cites listed and as such lack a year they were listed
  filter(!is.na(FL_year)) %>% mutate(FL_year = as.numeric(FL_year), Year_DEL = as.numeric(Year_DEL))

length(unique(CITES_Vert_pro$Taxon)) ## 1669 ER

## We automate the process but at this point we checked all species that our code produced as being listed then
## removed. We manually read the the historic cites listings for each species.
Manual_check <- CITES_Vert_pro %>% group_by(Taxon) %>% filter(Year == max(Year)) %>% filter(Year != 2020)

## Get the complex cases
Historic_CITES <- data.table::fread("Data/History_of_CITES_Listings_2021.csv") %>% 
  mutate(Year = format(as.Date(EffectiveAt, format="%d/%m/%Y"),"%Y"))
Complex_series <- rbind((filter(Historic_CITES, ChangeType == "DELETION" & IsCurrent == "TRUE") %>%
                           select(Year, FullName, ChangeType)), (filter(Historic_CITES, IsCurrent == "TRUE" & ChangeType == "ADDITION") %>%
                                                                   select(Year, FullName, ChangeType))) %>%
  group_by(FullName) %>% filter(n() > 1) %>% filter(n_distinct(ChangeType) > 1)

Complex_series_SP <- unique(Complex_series$FullName)
CXSP <- CITES_Vert_pro %>% filter(Taxon %in% Complex_series_SP) %>% summarise(Taxon = unique(Taxon))
unique(CXSP$Taxon)

## Replace these species with the correct time series dates
## Note Trionyx triunguis is listed remvoed then readded so we will add this manually
CITES_Vert_pro <- CITES_Vert_pro %>% mutate(FL_year = case_when(Taxon == "Anas formosa" ~ 1975,
                                                                Taxon == "Batagur borneoensis" ~ 1975,
                                                                Taxon == "Dendrocygna autumnalis" ~ 1975,
                                                                Taxon == "Dendrocygna bicolor" ~ 1975,
                                                                Taxon == "Hyaena hyaena" ~ 2014,
                                                                Taxon == "Mellivora capensis" ~ 1976,
                                                                TRUE ~ FL_year),
                                            Year_DEL = case_when(Taxon == "Anas formosa" ~ 2020,
                                                                 Taxon == "Batagur borneoensis" ~ 2020,
                                                                 Taxon == "Dendrocygna autumnalis" ~ 2020,
                                                                 Taxon == "Dendrocygna bicolor" ~ 2020,
                                                                 Taxon == "Hyaena hyaena" ~ 2020,
                                                                 Taxon == "Mellivora capensis" ~ 2020,
                                                                 TRUE ~ Year_DEL))

## Make time frame
Species_timeframe_base <- CITES_Vert_pro %>% group_by(Taxon, Class, Exporter, Importer) %>% 
  summarise(Year = seq(from = min(FL_year), to = max(Year_DEL), length.out = max(Year_DEL) - min(FL_year) + 1))

Species_timeframe <- rbind(mutate(Species_timeframe_base, Source_clean = "Wild", Live = "Live"),
                           mutate(Species_timeframe_base, Source_clean = "Captive", Live = "Live"),
                           mutate(Species_timeframe_base, Source_clean = "Wild", Live = "Not live"),
                           mutate(Species_timeframe_base, Source_clean = "Captive", Live = "Not live"))
## Make correct TT one
TT_sp <- Species_timeframe %>% filter(Taxon == "Trionyx triunguis") %>% group_by(Taxon, Class, Exporter, Importer, Source_clean, Live) %>% tally() %>% select(-n)
TS_sp <- data.frame(Taxon = "Trionyx triunguis", Year = c(1976:2007, 2017:2020))         
TT_correct <- full_join(TT_sp, TS_sp)

Species_timeframe_full <- rbind(filter(Species_timeframe, Taxon != "Trionyx triunguis"), TT_correct) %>% 
  filter(Year > 1999)


## 1668
length(unique(Species_timeframe_full$Taxon))
## 1669 
length(unique(CITES_Vert_pro$Taxon))
## Only the one species lost Hippotragus equinus that was never traded when listed
CITES_Vert_pro %>% ungroup() %>% distinct(Taxon) %>% filter(!Taxon %in% unique(Species_timeframe_full$Taxon))


## Expand the data set to fill missing years between 2000 - 2020
CITES_Wild_Com_Exp <- left_join(Species_timeframe_full, CITES_Vert_pro, by = c("Taxon", "Class", "Year", "Exporter", "Importer", "Source_clean", "Live"))

## 1668
length(unique(CITES_Wild_Com_Exp$Taxon))

## For 2000 - 2018 when the species are listed and present in the appendices, by our species specific time series,
## if the species is absent its a zero volume. We go back to 1999 for the lagged volume.
CITES_Com <- CITES_Wild_Com_Exp %>% 
  group_by(Taxon) %>% 
  mutate(n = replace_na(n, 0)) %>%
  select(-FL_year, -Year_DEL) %>% 
  mutate(Year = Year -2000)


CITES_Vert_Series <- left_join(CITES_Com, df_new, by = c("Year", "Taxon")) %>%
  mutate(Name_for_CITESdb = Taxon,
         Name_for_rl_history = if_else(is.na(IUCNName), Taxon, IUCNName))

length(unique(CITES_Vert_Series$Name_for_CITESdb)) ## now 1668
length(unique(CITES_Vert_Series$Name_for_rl_history)) ## now 1641
length(unique(CITES_Vert_Series$Taxon)) ## now 1668


## This code prepares the two versions of IUCN assessments.
## IUCN_code is the "true" state of assessments. Species are marked as Not assessed prior to having an assessment.
## So a species would go from Not assessed to LC to NT.
CITES_IUCN_data <- CITES_Vert_Series %>% 
  mutate(IUCN_code = case_when(is.na(IUCN_code) ~ "Not assessed", 
                               TRUE ~ IUCN_code)) %>%
  mutate(Threat_code = case_when(IUCN_code == "LC" ~ "Non-threatened",
                                 IUCN_code == "NT" ~ "Non-threatened",
                                 IUCN_code == "VU" ~ "Threatened",
                                 IUCN_code == "EN" ~ "Threatened",
                                 IUCN_code == "CR" ~ "Threatened",
                                 IUCN_code == "EW" ~ "Threatened",
                                 IUCN_code == "EX" ~ "Extinct",
                                 IUCN_code == "Not assessed" ~ "Not assessed",
                                 IUCN_code == "DD" ~ "Not assessed",
                                 TRUE ~ "ERROR"))

CITES_IUCN_data %>% group_by(IUCN_code) %>% tally()
CITES_IUCN_data %>% group_by(Threat_code) %>% tally()

length(unique(CITES_IUCN_data$Name_for_rl_history)) ## 1641
length(unique(CITES_IUCN_data$Name_for_CITESdb)) ## 1668

## Write this data into a csv to be read into subsequent analysis to save time.
write.csv(CITES_IUCN_data, "Data/2_CITES_Vert_Exp_Reported.csv", na = "")
write_rds(CITES_IUCN_data, "Data/2_CITES_Vert_Exp_Reported.rds")


#### Importer Series ####
## Get all trades to volumes per species per year
CITES_Vert_pro_I <- CITES_Vert %>% filter(Reporter.type == "I") %>%
  ## Group and tally per species/year
  group_by(Year, Taxon, Class, Exporter, Importer, FL_year, Year_DEL, Source_clean, Live) %>% 
  tally(WOE) %>% group_by(Taxon, Class, Exporter) %>% filter(n() > 0) %>%
  ## here we remove record for species that are not cites listed and as such lack a year they were listed
  filter(!is.na(FL_year)) %>% mutate(FL_year = as.numeric(FL_year), Year_DEL = as.numeric(Year_DEL))

length(unique(CITES_Vert_pro_I$Taxon)) ## 1679 IR

## We automate the process but at this point we checked all species that our code produced as being listed then
## removed. We manually read the the historic cites listings for each species.
Manual_check <- CITES_Vert_pro_I %>% group_by(Taxon) %>% filter(Year == max(Year)) %>% filter(Year != 2020)

## Get the complex cases
Historic_CITES <- data.table::fread("Data/History_of_CITES_Listings_2021.csv") %>% 
  mutate(Year = format(as.Date(EffectiveAt, format="%d/%m/%Y"),"%Y"))
Complex_series <- rbind((filter(Historic_CITES, ChangeType == "DELETION" & IsCurrent == "TRUE") %>%
                           select(Year, FullName, ChangeType)), (filter(Historic_CITES, IsCurrent == "TRUE" & ChangeType == "ADDITION") %>%
                                                                   select(Year, FullName, ChangeType))) %>%
  group_by(FullName) %>% filter(n() > 1) %>% filter(n_distinct(ChangeType) > 1)

Complex_series_SP <- unique(Complex_series$FullName)
CXSP <- CITES_Vert_pro_I %>% filter(Taxon %in% Complex_series_SP) %>% summarise(Taxon = unique(Taxon))
unique(CXSP$Taxon)

## Replace these species with the correct time series dates
## Note Trionyx triunguis is listed remvoed then readded so we will add this manually
CITES_Vert_pro_I <- CITES_Vert_pro_I %>% mutate(FL_year = case_when(Taxon == "Anas formosa" ~ 1975,
                                                                    Taxon == "Batagur borneoensis" ~ 1975,
                                                                    Taxon == "Batagur dhongoka" ~ 2003,
                                                                    Taxon == "Dendrocygna autumnalis" ~ 1975,
                                                                    Taxon == "Dendrocygna bicolor" ~ 1975,
                                                                    Taxon == "Hyaena hyaena" ~ 2014,
                                                                    Taxon == "Mellivora capensis" ~ 1976,
                                                                    TRUE ~ FL_year),
                                                Year_DEL = case_when(Taxon == "Anas formosa" ~ 2020,
                                                                     Taxon == "Batagur borneoensis" ~ 2020,
                                                                     Taxon == "Batagur dhongoka" ~ 2020,
                                                                     Taxon == "Dendrocygna autumnalis" ~ 2020,
                                                                     Taxon == "Dendrocygna bicolor" ~ 2020,
                                                                     Taxon == "Hyaena hyaena" ~ 2020,
                                                                     Taxon == "Mellivora capensis" ~ 2020,
                                                                     TRUE ~ Year_DEL))

## Make time frame
Species_timeframe_base_I <- CITES_Vert_pro_I %>% group_by(Taxon, Class, Exporter, Importer) %>% 
  summarise(Year = seq(from = min(FL_year), to = max(Year_DEL), length.out = max(Year_DEL) - min(FL_year) + 1))

Species_timeframe_I <- rbind(mutate(Species_timeframe_base_I, Source_clean = "Wild", Live = "Live"),
                             mutate(Species_timeframe_base_I, Source_clean = "Captive", Live = "Live"),
                             mutate(Species_timeframe_base_I, Source_clean = "Wild", Live = "Not live"),
                             mutate(Species_timeframe_base_I, Source_clean = "Captive", Live = "Not live"))
## Make correct TT one
TT_sp <- Species_timeframe_I %>% filter(Taxon == "Trionyx triunguis") %>% group_by(Taxon, Class, Exporter, Importer, Source_clean, Live) %>% tally() %>% select(-n)
TS_sp <- data.frame(Taxon = "Trionyx triunguis", Year = c(1976:2007, 2017:2020))         
TT_correct <- full_join(TT_sp, TS_sp)

Species_timeframe_full_I <- rbind(filter(Species_timeframe_I, Taxon != "Trionyx triunguis"), TT_correct) %>% 
  filter(Year > 1999)


## 1679
length(unique(Species_timeframe_full_I$Taxon))
## 1679 
length(unique(CITES_Vert_pro_I$Taxon))

## Expand the data set to fill missing years between 2000 - 2018
CITES_Wild_Com_Imp <- left_join(Species_timeframe_full_I, CITES_Vert_pro_I, by = c("Taxon", "Class", "Year", "Exporter", "Importer", "Source_clean", "Live"))

## 1679
length(unique(CITES_Wild_Com_Imp$Taxon))

## For 2000 - 2018 when the species are listed and present in the appendices, by our species specific time series,
## if the species is absent its a zero volume. We go back to 1999 for the lagged volume.
CITES_Com_I <- CITES_Wild_Com_Imp %>% 
  group_by(Taxon) %>% 
  mutate(n = replace_na(n, 0)) %>%
  select(-FL_year, -Year_DEL) %>% 
  mutate(Year = Year -2000)


CITES_Vert_Series_I <- left_join(CITES_Com_I, df_new, by = c("Year", "Taxon")) %>%
  mutate(Name_for_CITESdb = Taxon,
         Name_for_rl_history = if_else(is.na(IUCNName), Taxon, IUCNName))

length(unique(CITES_Vert_Series_I$Name_for_CITESdb)) ## now 1679
length(unique(CITES_Vert_Series_I$Name_for_rl_history)) ## now 1653
length(unique(CITES_Vert_Series_I$Taxon)) ## now 1679


## This code prepares the two versions of IUCN assessments.
## IUCN_code is the "true" state of assessments. Species are marked as Not assessed prior to having an assessment.
## So a species would go from Not assessed to LC to NT.
CITES_IUCN_data_I <- CITES_Vert_Series_I %>% 
  mutate(IUCN_code = case_when(is.na(IUCN_code) ~ "Not assessed", 
                               TRUE ~ IUCN_code)) %>%
  mutate(Threat_code = case_when(IUCN_code == "LC" ~ "Non-threatened",
                                 IUCN_code == "NT" ~ "Non-threatened",
                                 IUCN_code == "VU" ~ "Threatened",
                                 IUCN_code == "EN" ~ "Threatened",
                                 IUCN_code == "CR" ~ "Threatened",
                                 IUCN_code == "EW" ~ "Threatened",
                                 IUCN_code == "EX" ~ "Extinct",
                                 IUCN_code == "Not assessed" ~ "Not assessed",
                                 IUCN_code == "DD" ~ "Not assessed",
                                 TRUE ~ "ERROR"))

CITES_IUCN_data_I %>% group_by(IUCN_code) %>% tally()
CITES_IUCN_data_I %>% group_by(Threat_code) %>% tally()

length(unique(CITES_IUCN_data_I$Name_for_rl_history)) ## 1653
length(unique(CITES_IUCN_data_I$Name_for_CITESdb)) ## 1679

## Write this data into a csv to be read into subsequent analysis to save time.
write.csv(CITES_IUCN_data_I, "Data/2_CITES_Vert_Imp_Reported.csv", na = "")
write_rds(CITES_IUCN_data_I, "Data/2_CITES_Vert_Imp_Reported.rds")
