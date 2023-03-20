library(dplyr)
library(tidyr)
data = load('data/quality_out.rds')



data %>% 
  summarise_at(vars(starts_with("check"), "std"), sum, na.rm = T) %>% 
  pivot_longer(everything(), names_to = "check", values_to="true") %>% 
  left_join( data %>% 
               summarise_at(vars(starts_with("check"), "std"), function(x) sum(is.na(x))) %>% 
               pivot_longer(everything(), names_to = "check", values_to="non_applicabile"), 
             by="check") %>% 
  mutate(totale = nrow(data), 
         false = totale - non_applicabile - true, 
         perc_totale=true/totale, 
         perc_applicabile= true/(totale-non_applicabile)) %>% 
  select(check, true, false, non_applicabile, totale, everything()) %>% 
  mutate_if(is.numeric, round, digits=4)


true <- data %>% 
  group_by(std) %>% 
  summarise_at(vars(starts_with("check")), sum, na.rm = T) %>% 
  pivot_longer(starts_with("check"), names_to = "check", values_to="true")

non_applicabile <- data %>% 
  group_by(std) %>% 
  summarise_at(vars(starts_with("check")), function(x) sum(is.na(x))) %>% 
  pivot_longer(starts_with("check"), names_to = "check", values_to="non_applicabile")

count <- data %>% 
  group_by(std) %>%
  summarise(totale = n())

analysis_std <- true %>% 
  left_join(non_applicabile, by=c("check", "std")) %>% 
  left_join(count, by=c("std")) %>% 
  mutate(false = totale - non_applicabile - true, 
         perc_totale=true/totale, 
         perc_applicabile= true/(totale-non_applicabile)) %>% 
  select(check, true, false, non_applicabile, totale, everything()) %>% 
  mutate_if(is.numeric, round, digits=4) %>% 
  arrange(check, std) %>% 
  select(check, std, everything())


analysis_std %>% 
  filter(!is.na(std)) %>% 
  select(check, std, perc_totale) %>% 
  mutate(std = ifelse(std, "standard", "non_standard")) %>% 
  pivot_wider(names_from=std, values_from=perc_totale) %>% 
  select(check, standard, non_standard)

