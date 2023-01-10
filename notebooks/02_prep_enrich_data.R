##  prep enrich data
##  "2023-01-09"
library(dplyr)
library(purrr)
library(furrr)
library(future)
library(stringr)
library(glue)
library(sf)
library(readr)
library(janitor)
library(jsonlite)





## 0 select and prep ----
prepped = mock_data_core %>%
  select(-contains("cod"),
         -contains("id"),
         -contains("data"))

## 0.1 rehydrata `luogo_istat`, per molti non c'è ----
### passaggio mancante 


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



## 1.1 arricchisci da codici istat ----

codici_istat = read_delim("data/codici_stat_istat.csv", delim =";", locale=locale(encoding="latin1"), name_repair = janitor::make_clean_names) %>% 
  transmute(
    codice_regione,
    codice_dell_unita_territoriale_sovracomunale_valida_a_fini_statistici,
    codice_comune_formato_alfanumerico,
    denominazione_in_italiano,
    denominazione_regione,
    codice_comune_formato_numerico
  )

## assign 0 and 1 when are present or missing or wrong (which?)
total = prepped %>% 
  left_join(codici_istat, by =  c("luogo_istat" = "codice_comune_formato_numerico")) 
  







