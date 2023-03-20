library(readxl)
library(readr)
library(dplyr)
library(rsdmx)
library(janitor)

# ANAC ----
anac_indicatori_comunali <- read_excel("data/enrichment/Indicatori Comunali - Dataset.xlsx", 
                                          sheet = "Layout 2", .name_repair = make_clean_names) %>% 
  filter(anno_sa == 2019) %>% 
  select(-x)

anac_indicatori_contesto <- read_excel("data/enrichment/Indicatori Contesto - Dataset.xlsx", 
                                         sheet = "Provincia",.name_repair = make_clean_names ) %>% 
  filter(anno_sa == 2017) %>% 
  select(-x)


# Mef ----
mef_irpef <- read_delim("data/enrichment/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2020.csv", 
                                                                             delim = ";", escape_double = FALSE, trim_ws = TRUE,
                        show_col_types = FALSE, name_repair = make_clean_names) %>% 
  select(codice_istat_comune, 
         denominazione_comune, 
         reddito_imponibile_frequenza,
         reddito_imponibile_ammontare_in_euro) %>% 
  mutate(reddito_imponibile_medio = reddito_imponibile_ammontare_in_euro/reddito_imponibile_frequenza)

# ISTAT ----
sdmx.popolazione <- readSDMX(providerId = "ISTAT", 
                      resource = "data", 
                      flowRef  = "22_289",
                      key = list("A", "TOTAL", NULL, 9, 99, "JAN"),
                      start = 2020, 
                      end = 2020,
                      dsd = T)
istat_popolazione <- as.data.frame(sdmx.popolazione, labels = T) %>% 
  select(codice_istat = ITTER107, popolazione = obsValue)

istat_istruzione <- read_excel("data/enrichment/2-Istruzione.xlsx", 
                         sheet = "Tav. 3.1 Comuni", skip = 3) %>% 
  select(codice_istat = CodiceComuneIstat, istruzione_bambini_servizi_comunali_infanzia=`2020`)

istat_cultura1 <- read_excel("data/enrichment/5-Cultura.xlsx", 
                             sheet = "Tav. 1.1 Comuni", skip = 3) %>% 
  select(codice_istat = Codice.Comune.Istat, cultura_musei_ab=`2018`)

istat_cultura2 <- read_excel("data/enrichment/5-Cultura.xlsx", 
                              sheet = "Tav. 2.1 Comuni", skip = 3) %>% 
  select(codice_istat = CodiceComuneIstat, cultura_visitatori=`2018`)

istat_economia1 <- read_excel("data/enrichment/7a-Economia-insediata-rapporti-caratteristici.xlsx", 
                               sheet = "Tav. 1.1 Comuni", skip = 3) %>% 
  select(codice_istat = `Codice Comune Istat`, economia_tasso_imprenditorialita=`2020`)

istat_economia2 <- read_excel("data/enrichment/7a-Economia-insediata-rapporti-caratteristici.xlsx", 
                                                             sheet = "Tav. 2.1 Comuni", skip = 3) %>% 
  select(codice_istat = `Codice Comune Istat`, economia_densita_unita_locali=`2020`)

istat_ricerca <- read_excel("data/enrichment/8-Ricerca-innovazione.xlsx", 
                      sheet = "Tav. 1.1 Comuni", skip = 3) %>% 
  select(codice_istat = `Codice Comune Istat`, economia_specializzazione_produttiva=`2020`)

istat_mobilita <- read_excel("data/enrichment/9-Infrastrutture-e-mobilità.xlsx", 
                       sheet = "Tav. 1.1 Comuni", skip = 3) %>% 
  select(codice_istat = `Codice Comune Istat`, mobilita_tasso_incidentalita=`2020`)



# Merge all ---

istat <- istat_istruzione %>% 
  left_join(istat_cultura1) %>% 
  left_join(istat_cultura2) %>% 
  left_join(istat_economia1) %>% 
  left_join(istat_economia2) %>% 
  left_join(istat_ricerca) %>% 
  left_join(istat_mobilita) %>% 
  left_join(istat_popolazione) %>% 
  mutate_all(function(x) ifelse(x==".", 0, x)) %>% 
  mutate_at(vars(istruzione_bambini_servizi_comunali_infanzia:popolazione), as.numeric)

enrich_comuni <- istat %>% 
  left_join(mef_irpef %>% 
              select(codice_istat, reddito_imponibile_medio))

saveRDS(enrich_comuni, file = "data/enrich_comuni.rds")


