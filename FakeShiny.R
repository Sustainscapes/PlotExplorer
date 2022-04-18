library(leaflet)
library(tidyverse)
library(htmltools)
library(bsplus)
library(vegan)
library(crosstalk)
library(sf)
library(DT)
library(plotly)
library(Artscore)

data("Habitat_List")
data("Species_List")

Test <- read_rds("/home/derek/Documents/Site_Selection_DK/AllData4.rds") %>%
  dplyr::filter(Dataset == "Novana") %>%
  separate(col = "ID", into = c("ID", "plot"))

Artlist <- data.table::fread("/home/derek/Documents/Danish-Flora-and-Vegetation.github.io/MapAndnmds/artsliste.csv") %>%
  as.data.frame()

library(readr)

frekvens2 <- read_csv("/home/derek/Documents/Danish-Flora-and-Vegetation.github.io/MapAndnmds/alledata-frekvens2.txt") %>%
  janitor::clean_names() %>%
  as_tibble()

frekvens2$species <- str_split(frekvens2$specieslist, ",")

frekvens2 <- frekvens2 %>%
  dplyr::select(-specieslist) %>%
  unnest(species) %>%
  mutate(species = str_trim(species, side = "both"),
         species = str_remove_all(species, "\\}"),
         site = str_remove_all(site, "\\{"),
         species = as.numeric(species)) %>%
  rename(ArtID = species) %>%
  left_join(Artlist) %>%
  mutate(plot = as.character(plot)) %>% dplyr::select(plot, year, LatArt) %>%
  dplyr::filter(year < (lubridate::year(lubridate::ymd(Sys.Date())) + 1)) %>%
  group_by(plot) %>%
  dplyr::filter(year == max(year))  %>%
  dplyr::filter(LatArt != "")

PlotRichness <- frekvens2 %>% ungroup() %>%
  group_by(plot) %>%
  summarise(Richness = n())

PlotCommunity <- frekvens2 %>%
  ungroup() %>%
  mutate(Pres = 1) %>%
  dplyr::distinct() %>%
  dplyr::filter(LatArt != "") %>%
  pivot_wider(names_from = LatArt, values_from = Pres, values_fill = 0) %>%
  janitor::clean_names()


PlotSppList <- frekvens2 %>%
  ungroup() %>%
  dplyr::filter(LatArt %in% Species_List$Scientific_name) %>%
  nest(Species = LatArt)

PlotSppNames <- frekvens2 %>%
  ungroup() %>%
  dplyr::filter(LatArt %in% Species_List$Scientific_name) %>%
  group_by(plot) %>%
  #    summarise(Species = paste(LatArt, collapse = "<br/> "), n = paste("Number of species", n()))
  summarise(Species = paste(LatArt, collapse = ", "), n = paste("Number of species", n()), Richness = n()) %>%
  filter(complete.cases(.))

Final <- list()

for(i in 1:nrow(PlotSppNames)){
  Button <- bs_button(PlotSppNames$n[i], button_type = "primary") %>% bs_attach_collapse(paste0("id_spp", i))
  SppList = bs_collapse(id = paste0("id_spp", i), PlotSppNames$Species[i])

  Final[[i]] = paste(sep = "<br/>", Button, SppList)
}

Final <- Final %>% purrr::reduce(c)

PlotSppNames$SpeciesButton <- Final
PlotSppNames <- PlotSppNames %>% dplyr::select(-Species)

#NMDS <- readRDS("/home/derek/Documents/Danish-Flora-and-Vegetation.github.io/MapAndnmds/NMDSplots.rds")

result <- Test %>%
  right_join(PlotSppList) %>%
  right_join(PlotSppNames) %>%
  dplyr::select(plot, Species, MajorHab, Richness, habtype, SpeciesButton) %>%
  mutate(habtype = as.character(habtype)) %>%
  dplyr::filter(habtype %in% Habitat_List$Code)


ARTS <- list()

for(i in 1:nrow(result)){
  try({ARTS[[i]] <- suppressMessages(Artscore(ScientificName = result$Species[i][[1]]$LatArt, Habitat_code = result$habtype[i])$Artsindex)}, silent = T)
  if(i %% 100 == 0){
    message(paste(i, "of", nrow(result), "ready", Sys.time()))
  }
}

cond <- ARTS %>% purrr::map(class) %>% reduce(c)
cond <- cond == "numeric"
ARTS <- ARTS %>% reduce(c)

result <- result[cond,]

result$Artsindex <- ARTS

### Ellemberg values

library(readxl)
data("Species_List")

Ellemberg <- read_excel("TestPlot/RawData/jpe12867-sup-0002-tables1.xlsx",
                        sheet = "TableS1") %>%
  janitor::clean_names()

TestSp <-sort(Ellemberg$scientific_name[!(Ellemberg$scientific_name %in% Species_List$Scientific_name)])

TestSp <- data.frame(Ellemberg  = TestSp, Spp2 = NA, Score = NA)

library(stringdist)

for(i in 1:nrow(TestSp)){
  TestSp$Score[i] <- min(stringdist(a = TestSp$Ellemberg[i], b = Species_List$Scientific_name, method = "cosine"))
  TestSp$Spp2[i] <- Species_List$Scientific_name[stringdist(a = TestSp$Ellemberg[i], b = Species_List$Scientific_name, method = "cosine") == min(stringdist(a = TestSp$Ellemberg[i], b = Species_List$Scientific_name, method = "cosine"))]
}


TestSp <- TestSp %>% dplyr::filter(Score <= 0.1)

for(i in 1:nrow(TestSp)){
  Ellemberg[Ellemberg$scientific_name == TestSp$Ellemberg[i],]$scientific_name <- TestSp$Spp2[i]
}

Temp <- list()

for(i in 1:nrow(result)){
  suppressMessages({
    Temp[[i]] <- result[i,] %>% unnest(cols = "Species") %>%
      rename(scientific_name = LatArt) %>%
      left_join(Ellemberg) %>%
      select("plot", "MajorHab", "Richness", "habtype",
             "SpeciesButton", "geometry", "Artsindex",
             "eiv_l", "eiv_f", "eiv_r", "eiv_n", "grime_strategy", "grime_c",
             "grime_s", "grime_r","eiv_n_r_ratio")
    Temp[[i]] <- Temp[[i]] %>% group_by(plot, MajorHab) %>%
      summarise(L = median(eiv_l, na.rm = T), f =median(eiv_f, na.rm = T), R = median(eiv_r, na.rm = T), N = median(eiv_n, na.rm = T), N_R = median(eiv_n_r_ratio, na.rm = T), grime_C = median(grime_c, na.rm = T),
                grime_S = median(grime_s, na.rm = T), grime_R= median(grime_r, na.rm = T)) %>% ungroup()
  })
  if(i %% 500 == 0){
    message(paste(i, "of", nrow(result), "ready", Sys.time()))
  }
}

Temp <- Temp %>%
  purrr::reduce(bind_rows)

Temp <- Temp %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

#####
result <- result %>%
  right_join(Temp) %>%
  st_transform("+proj=longlat +datum=WGS84") %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(MajorHab))



Habitat2 <- Habitat_List %>% rename(habtype = Code)

result <- result %>% left_join(Habitat2)

result <- result %>% rename(F = f)
set.seed(2022)

BaseIndex <- sample(1:nrow(result), size = 100)


Base <- result[-BaseIndex,]

saveRDS(Base, "TestPlot/Base.rds")

Groups <- result[BaseIndex,]

set.seed(2022)

Groups$Group <- sample(LETTERS[1:5], size = nrow(Groups), replace = T)

saveRDS(Groups, "TestPlot/Groups.rds")
