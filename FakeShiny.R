library(leaflet)
library(tidyverse)
library(htmltools)
library(bsplus)
library(vegan)
library(crosstalk)
library(sf)
library(DT)
library(plotly)

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
  nest(Species = LatArt)

PlotSppNames <- frekvens2 %>%
  ungroup() %>%
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

PlotSppNames$Species <- Final

NMDS <- readRDS("/home/derek/Documents/Danish-Flora-and-Vegetation.github.io/MapAndnmds/NMDSplots.rds")

result <- Test %>%
  right_join(PlotSppNames) %>% dplyr::select(plot, Species, MajorHab, Richness) %>%
  right_join(NMDS) %>%
  st_transform("+proj=longlat +datum=WGS84") %>%
  dplyr::distinct() %>%
  dplyr::select(-year, -CheckPoint) %>%
  dplyr::filter(!is.na(MajorHab))


set.seed(2022)

BaseIndex <- sample(1:nrow(result), size = 100)


Base <- result[-BaseIndex,]

saveRDS(Base, "TestPlot/Base.rds")

Groups <- result[BaseIndex,]

set.seed(2022)

Groups$Group <- sample(LETTERS[1:5], size = nrow(Groups), replace = T)

saveRDS(Groups, "TestPlot/Groups.rds")
