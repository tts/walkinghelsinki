library(sf)
library(tidyverse)

#-----
# Area
#-----

url <-"https://www.hel.fi/hel2/tietokeskus/data/kartta_aineistot/PKS_Kartta_Rajat_KML2011.zip"
temp <- tempfile()
download.file(url, temp)
unzip(zipfile = temp)
unlink(temp)

areas_big <- st_read("PKS_suuralue.kml")

# Helsinki
hki_big_area <- areas_big %>%
  filter(Name %in% c("Läntinen", "Keskinen", "Pohjoinen", "Itäinen", 
                     "Koillinen", "Kaakkoinen", "Eteläinen"))

# All districts
areas_small <- st_read("PKS_pienalue.kml")

# Districts inside Helsinki
# Note: to get rid of the error message with the help of st_buffer, see
# https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec
distr_in_area <- st_join(st_buffer(areas_small, dist = 0),
                    st_buffer(hki_big_area, dist = 0),
                    left = FALSE, largest = TRUE)

distr_in_area <- distr_in_area %>% 
  sf::st_transform(crs = 4326)

write_rds(distr_in_area, "distr_in_area.RDS")

#-----------
# Park roads
#-----------

url <-"https://kartta.hel.fi/avoindata/Helsinki_liikennevaylat_avoin_data.zip"
temp <- tempfile()
download.file(url, temp)
unzip(zipfile = temp)
unlink(temp)

roads <- st_read("Helsinki_liikennevaylat_avoin_data/KML/Hki_liikennevaylat.kml")

park_roads <- roads %>% 
  filter(str_detect(Description, "Puisto.*")) %>% 
  select(-Description)

park_roads <- park_roads %>%
  mutate(length_n = as.numeric(ceiling(st_length(.))),
         length = paste0(length_n, " m")) %>% 
  sf::st_transform(crs = 4326)

write_rds(park_roads, "park_roads.RDS")

#--------------
# Planted trees
#--------------

baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?"
wfs_request <- "request=GetFeature&service=WFS&version=2.0.0&typeName=Puurekisteri_piste&outputFormat=json"
hki_trees_wfs <- paste0(baseurl,wfs_request)
trees <- st_read(hki_trees_wfs)

trees <- trees %>% 
  mutate(nimi = paste0(stringr::str_to_sentence(suomenknimi), " (", suku, " ", laji, ")")) %>% 
  select(nimi, kokoluokka) %>% 
  sf::st_transform(crs = 4326)

saveRDS(trees, "trees.RDS")

#-----------------------
# Protected buildings
#-----------------------

baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?request=GetFeature&service=WFS&version=2.0.0"
type <- "avoindata:Asemakaavoissa_suojellut_rakennukset_alue"
request <- paste0(baseurl, "&typeName=", type, "&outputFormat=json")
build <- st_read(request, stringsAsFactors = FALSE)
build <- st_as_sf(build)

build <- build %>% 
  select(osoite, voimaantulo_pvm, laji, starts_with("kaavaselitys")) %>% 
  sf::st_transform(crs = 4326)

#--------------------
# City bike stations
#--------------------

# Note that the url can change
url <- "https://opendata.arcgis.com/datasets/726277c507ef4914b0aec3cbcfcbfafc_0.geojson"
download.file(url, destfile = "bikestations.geojson", method="curl", extra="-k")
stations <- read_sf("bikestations.geojson")

stations <- stations %>% 
  filter(Kaupunki != "Espoo") %>% 
  rename(Paikkoja = Kapasiteet) %>% 
  select(Nimi, ID, Osoite, Paikkoja) %>%
  st_transform(crs = 4326) 

#----------------------
# Join info by district
#----------------------

trees_in_distr_in_area <- st_join(trees, distr_in_area)
trees_in_distr_in_area <- trees_in_distr_in_area %>% 
  select(nimi, kokoluokka, Name.x)

roads_in_distr_in_area <- st_join(park_roads, distr_in_area)
roads_in_distr_in_area <- roads_in_distr_in_area %>% 
  select(length_n, length, Name.x)

prot_build_in_distr_in_area <- st_join(build, distr_in_area)
prot_build_in_distr_in_area <- prot_build_in_distr_in_area %>% 
  select(-Description.x, -Description.y, -Name.y) 

bikestations_in_distr_in_area <- st_join(stations, distr_in_area)
bikestations_in_distr_in_area <- bikestations_in_distr_in_area %>% 
  select(-Description.x, -Description.y, -Name.y)

write_rds(trees_in_distr_in_area, "trees_in_distr_in_area.RDS")
write_rds(roads_in_distr_in_area, "roads_in_distr_in_area.RDS")
write_rds(prot_build_in_distr_in_area, "prot_build_in_distr_in_area.RDS")
write_rds(bikestations_in_distr_in_area, "bikestations_in_distr_in_area.RDS")

