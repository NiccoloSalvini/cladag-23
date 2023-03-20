##  prep enrich data
##  "2023-01-09"
library(dplyr)
library(purrr)
library(stringr)
library(readr)


enrich_comuni = read_rds(here("data", "enrich_comuni.rds"))
cigs_2020_joined_filtr_cpv33 = read_rds(here("data", "cigs_2020_joined_filtr_cpv33.rds"))
elenco_tabella_conversioni_codici_comuni = read_csv(here("data", "elenco_tabella_conversioni_codici_comuni.csv"))

## 0.1 rehydrata `luogo_istat`, per molti non c'è ----
### da `cf_amministrazione_appaltante` -> nome_comune -> codice_istat
### prior rehydra  count NA  =  24
### afert rehydra  count NA
cigs_2020_joined_filtr_cpv33 %>% 
  head(100) %>% 
  mutate(
    denominazione_comune = map_chr(.x = cf_amministrazione_appaltante, .f = from_cfamm_to_munname)
  ) %>% 
  left_join()

cigs_2020_joined_filtr_cpv33 %>% 
  head(100) %>% 
  select(cf_amministrazione_appaltante)
## 1 arricchisci da https://www.indicepa.gov.it/ipa-portale/consultazione/indirizzo-sede/ricerca-ente ---- 

### step 1 prendi indirizzo da codice fiscale
### passaggio mancante


### step 2 da indirizzo a latlong
from_address_to_latlong <- function(address) {
  url = glue("https://nominatim.openstreetmap.org/search?q={URLencode(address)}&format=geocodejson")
  fromJSON(url) %>% 
    .$features$geometry$coordinates[[1]] %>% 
    return()
}

## guarda un p0'

library(tidyverse)
library(here)
enrich_comuni = read_rds(here("data", "enrich_comuni.rds"))
cigs_2020_joined_filtr_cpv33  = read_rds(here("data", "cigs_2020_joined_filtr_cpv33.rds"))

joined  = cigs_2020_joined_filtr_cpv33 %>% 
  left_join(enrich_comuni, by =  c("luogo_istat" = "codice_istat"), multiple = "any")


### count quanti ne mancano di luoghi istat

cigs_2020_joined_filtr_cpv33 %>% 
  count(luogo_istat)


cigs_2020_joined_filtr_cpv33 %>% 
  filter(luogo_istat == "001048") %>% view


