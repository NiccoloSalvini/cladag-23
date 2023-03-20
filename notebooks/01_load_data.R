library(dplyr) 
library(janitor)
library(purrr)
library(vroom)
library(stringr)


## https://dati.anticorruzione.it/opendata/download/dataset/aggiudicazioni/filesystem/aggiudicazioni_csv.zip
aggiudicazioni = vroom("data/aggiudicazioni_csv.csv", .name_repair = janitor::make_clean_names) # 1,622,864 × 18

## https://dati.anticorruzione.it/opendata/download/dataset/quadro-economico/filesystem/quadro-economico_csv.zip
quadro_economico = vroom("data/quadro-economico_csv.csv", .name_repair = janitor::make_clean_names) # A tibble: 2,109,476 × 12

## loop in the folder for each monthly file
cigs_2020 = map_df(list.files("data/2020", full.names = T), ~vroom(.x, .name_repair = janitor::make_clean_names))


## A tibble: 417,716 × 98
cigs_2020_final = cigs_2020 %>% 
  left_join(aggiudicazioni, by  ="cig") %>%
  left_join(quadro_economico, by  = "id_aggiudicazione")


saveRDS(cigs_2020_final,file = "data/cigs_2020_final.rds")


# A tibble: 143,642 × 1 for cpv starting with 33
cigs_2020_final_cpv33 = cigs_2020_final %>% 
  filter(substr(cod_cpv, 1, 2) == "33") %>% 
  select(cod_cpv) 

saveRDS(cigs_2020_final_cpv33, file = "data/cigs_2020_final_cpv33.rds")
HyperWrite Logo