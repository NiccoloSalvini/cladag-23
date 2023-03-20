##################
#  Data Quality  #
#  v1.2.1        #
##################

### Packages ----
library(dplyr)
library(tidyr)
library(readr)


### Input ----
data_path <- "data/CIGs_2020_joined.rds"
data_path_output <- "data/quality_out.rds"
par_a_abs <- TRUE
par_a_tolleranza <- 0.0015
par_a_soglia_importo_disposizione <- 5000 
par_a_soglia_differenza <- 40000 
par_a_soglia_importo_disposizione_rel <- 0.05
par_a_soglia_differenza_rel <- 0.05
par_b_min_ribasso <- 1
par_b_max_ribasso <- 50
par_c1_max_ribasso <- 50
par_c2_max_ribasso <- 40
par_c3_max_potenze <- 10
par_c3_tolleranza <- 0.05
par_d_rap_tolleranza <- 0.05
par_std_scelta_contraente <- c(1, 2, 3, 4, 28, 30, 15, 7, 8, 34, 38, 33, 35, 6, 14, 22, 23)
par_std_modalita_realizzazione <- c(1, 7, 11)


data <- read_rds(data_path) %>% 
  rename_all(., .funs = toupper)


### Data preparation ----
data_original <- data

data <- data %>% 
  distinct(CIG, .keep_all=T) %>% 
  select(CIG, COD_TIPO_SCELTA_CONTRAENTE, COD_MODALITA_REALIZZAZIONE, IMPORTO_FORNITURE, 
         IMPORTO_LAVORI, IMPORTO_SERVIZI, SOMME_A_DISPOSIZIONE, 
         IMPORTO_SICUREZZA, IMPORTO_PROGETTAZIONE, ULTERIORI_ONERI_NON_SOGGETTI_RIBASSO, 
         IMPORTO_LOTTO, IMPORTO_AGGIUDICAZIONE,RIBASSO_AGGIUDICAZIONE) %>% 
  mutate_if(is.numeric, replace_na, replace = 0) 

## A - Importo a base d'asta ----
if(par_a_abs){
  step_a <- data %>% 
    filter(SOMME_A_DISPOSIZIONE > par_a_soglia_importo_disposizione, 
           IMPORTO_LOTTO - SOMME_A_DISPOSIZIONE >= par_a_soglia_differenza)
}else{
  step_a <- data %>% 
    filter(SOMME_A_DISPOSIZIONE > par_a_soglia_importo_disposizione_rel*IMPORTO_LOTTO, 
           IMPORTO_LOTTO - SOMME_A_DISPOSIZIONE >= par_a_soglia_differenza_rel*IMPORTO_LOTTO)
}

step_a <- step_a %>% 
  mutate(importo_calcolato = IMPORTO_LAVORI + IMPORTO_SERVIZI + IMPORTO_FORNITURE +
           IMPORTO_SICUREZZA + ULTERIORI_ONERI_NON_SOGGETTI_RIBASSO + SOMME_A_DISPOSIZIONE,
         check_a = abs(importo_calcolato - IMPORTO_LOTTO)/IMPORTO_LOTTO < par_a_tolleranza, 
         IMPORTO_LOTTO = ifelse(check_a, IMPORTO_LOTTO - SOMME_A_DISPOSIZIONE, IMPORTO_LOTTO)) %>% 
  select(CIG, IMPORTO_LOTTO, check_a)

data <- data %>% 
  rename(old_IMPORTO_LOTTO = IMPORTO_LOTTO) %>% 
  left_join(step_a, by="CIG")

## B - Ribasso di aggiudicazione ----

step_b1 <- data %>% 
  mutate(check_b11 = IMPORTO_LOTTO > IMPORTO_AGGIUDICAZIONE & RIBASSO_AGGIUDICAZIONE < 0, 
         check_b12 = IMPORTO_LOTTO < IMPORTO_AGGIUDICAZIONE & RIBASSO_AGGIUDICAZIONE > 0, 
         RIBASSO_AGGIUDICAZIONE = 
           ifelse(check_b11 | check_b12, RIBASSO_AGGIUDICAZIONE*-1, RIBASSO_AGGIUDICAZIONE)) %>% 
  select(CIG, RIBASSO_AGGIUDICAZIONE, check_b11, check_b12)

data <- data %>% 
  rename(old_RIBASSO_AGGIUDICAZIONE = RIBASSO_AGGIUDICAZIONE) %>% 
  left_join(step_b1, by="CIG")


step_b2 <- data %>%
  mutate(ribasso_calcolato = (1-((IMPORTO_AGGIUDICAZIONE-IMPORTO_SICUREZZA-ULTERIORI_ONERI_NON_SOGGETTI_RIBASSO)/
                                   (IMPORTO_LOTTO-IMPORTO_SICUREZZA-ULTERIORI_ONERI_NON_SOGGETTI_RIBASSO)))*100,
         check_b21 = IMPORTO_LOTTO > IMPORTO_AGGIUDICAZIONE & RIBASSO_AGGIUDICAZIONE == 0 & abs(ribasso_calcolato) < par_b_max_ribasso & abs(ribasso_calcolato > par_b_min_ribasso),
         check_b22 = IMPORTO_LOTTO < IMPORTO_AGGIUDICAZIONE & RIBASSO_AGGIUDICAZIONE == 0 & abs(ribasso_calcolato) < par_b_max_ribasso & abs(ribasso_calcolato > par_b_min_ribasso),
         RIBASSO_AGGIUDICAZIONE = ifelse(check_b21 | check_b22, ribasso_calcolato, RIBASSO_AGGIUDICAZIONE))  %>% 
  mutate(RIBASSO_AGGIUDICAZIONE = ifelse(is.na(RIBASSO_AGGIUDICAZIONE), 0, RIBASSO_AGGIUDICAZIONE)) %>% 
  select(CIG, RIBASSO_AGGIUDICAZIONE, check_b21, check_b22)


data <- data %>% 
  rename(old_RIBASSO_AGGIUDICAZIONE_stepb1 = RIBASSO_AGGIUDICAZIONE) %>% 
  left_join(step_b2, by="CIG")


## C - Importo di aggiudicazione ----


step_c <- data %>% 
  mutate(importo_aggiudicazione_calcolato = (IMPORTO_LOTTO - IMPORTO_SICUREZZA - ULTERIORI_ONERI_NON_SOGGETTI_RIBASSO)*(1-RIBASSO_AGGIUDICAZIONE/100)+
           ULTERIORI_ONERI_NON_SOGGETTI_RIBASSO + IMPORTO_SICUREZZA, 
         rap = importo_aggiudicazione_calcolato/IMPORTO_AGGIUDICAZIONE) %>% 
  select(CIG, importo_aggiudicazione_calcolato, IMPORTO_AGGIUDICAZIONE, rap, RIBASSO_AGGIUDICAZIONE, IMPORTO_LOTTO) 

step_c1 <- step_c %>% 
  mutate(check_c1 = !is.na(importo_aggiudicazione_calcolato) & IMPORTO_AGGIUDICAZIONE == 0 & 
           abs(RIBASSO_AGGIUDICAZIONE) < par_c1_max_ribasso,  
         IMPORTO_AGGIUDICAZIONE = ifelse(check_c1, importo_aggiudicazione_calcolato, IMPORTO_AGGIUDICAZIONE)) %>% 
  select(CIG, check_c1, IMPORTO_AGGIUDICAZIONE)

data <- data %>% 
  rename(old_IMPORTO_AGGIUDICAZIONE_stepc1 = IMPORTO_AGGIUDICAZIONE) %>% 
  left_join(step_c1, by="CIG")


step_c2 <- step_c %>% 
  mutate(check_c2 = !is.na(importo_aggiudicazione_calcolato) & IMPORTO_AGGIUDICAZIONE == IMPORTO_LOTTO & 
           abs(RIBASSO_AGGIUDICAZIONE) < par_c2_max_ribasso &  RIBASSO_AGGIUDICAZIONE > 0,  
         IMPORTO_AGGIUDICAZIONE = ifelse(check_c2, importo_aggiudicazione_calcolato, IMPORTO_AGGIUDICAZIONE)) %>% 
  select(CIG, check_c2, IMPORTO_AGGIUDICAZIONE)

data <- data %>% 
  rename(old_IMPORTO_AGGIUDICAZIONE_stepc2 = IMPORTO_AGGIUDICAZIONE) %>% 
  left_join(step_c2, by="CIG")


rap_potenze <- sapply(-par_c3_max_potenze:par_c3_max_potenze, function(x) 10^x)
rap_potenze <- rap_potenze[rap_potenze != 1]
check_rap <- function(rap) ifelse(!is.na(rap), min(abs((rap_potenze - rap)/rap_potenze)) < par_c3_tolleranza,  NA)

step_c3 <- step_c %>% 
  rowwise() %>% 
  mutate(check_c3 = check_rap(rap), 
         IMPORTO_AGGIUDICAZIONE = ifelse(check_c3, importo_aggiudicazione_calcolato, IMPORTO_AGGIUDICAZIONE)) %>% 
  select(CIG, check_c3, IMPORTO_AGGIUDICAZIONE)

data <- data %>% 
  rename(old_IMPORTO_AGGIUDICAZIONE_stepc3 = IMPORTO_AGGIUDICAZIONE) %>% 
  left_join(step_c3, by="CIG")


## D - correzione in base al rap ----
step_d <-data %>% 
  mutate(importo_aggiudicazione_calcolato = (IMPORTO_LOTTO - IMPORTO_SICUREZZA - ULTERIORI_ONERI_NON_SOGGETTI_RIBASSO)*(1-RIBASSO_AGGIUDICAZIONE/100)+
           ULTERIORI_ONERI_NON_SOGGETTI_RIBASSO + IMPORTO_SICUREZZA, 
         rap = importo_aggiudicazione_calcolato/IMPORTO_AGGIUDICAZIONE, 
         check_d = abs(rap - 1)>par_d_rap_tolleranza) %>% 
  select(CIG, rap, check_d)

data <- data %>% 
  left_join(step_d, by="CIG")

#### STD - casi standard ----
data <- data %>% 
  mutate(std = COD_TIPO_SCELTA_CONTRAENTE %in% par_std_scelta_contraente & COD_MODALITA_REALIZZAZIONE %in% par_std_modalita_realizzazione) 



#### Unisco con dati originali ----
data <- data_original %>% 
  rename(old_IMPORTO_LOTTO=IMPORTO_LOTTO, 
         old_IMPORTO_AGGIUDICAZIONE = IMPORTO_AGGIUDICAZIONE, 
         old_RIBASSO_AGGIUDICAZIONE = RIBASSO_AGGIUDICAZIONE) %>% 
  left_join(data %>% 
              select(CIG, IMPORTO_LOTTO, IMPORTO_AGGIUDICAZIONE, 
                     RIBASSO_AGGIUDICAZIONE, 
                     starts_with("check"), std), by="CIG")


write_rds(data, file = data_path_output)


