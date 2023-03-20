library(randomForest)
library(fastDummies)
library(dplyr)
library(caret)
data= read_rds('data/quality_out.rds')

data_subset <- data %>% 
  #dplyr::filter(!is.na(check_name)) %>% 
  select(CIG, check = check_a) %>% 
  inner_join(data) %>% 
  mutate(check = as.numeric(check)) %>% 
  select(check, 
         #criterio_agg2, oepv,  
         NUM_IMPRESE_OFFERENTI, NUMERO_OFFERTE_AMMESSE, NUMERO_OFFERTE_ESCLUSE, FLAG_SUBAPPALTO,
         NUM_IMPRESE_RICHIEDENTI, NUM_IMPRESE_INVITATE, ASTA_ELETTRONICA,
         #nvarianti, sosp, 
         #OGGETTO_LOTTO, OGGETTO_GARA, CPV.DESCRIZIONE_CPV, nome_provincia, nome_regione (troppi missing), rip_geo
         N_LOTTI_COMPONENTI, 
         MODALITA_REALIZZAZIONE, TIPO_SCELTA_CONTRAENTE, OGGETTO_PRINCIPALE_CONTRATTO) %>% 
  mutate(check = as.factor(check)) %>% 
  mutate_if(is.character, factor) %>% 
  mutate_if(is.logical, as.factor) %>% 
  drop_na %>% 
  data.frame 

ids <- sample(1:nrow(data_subset), nrow(data_subset)*0.8)
train <- data_subset[ids,]
test <- data_subset[-ids,]

rf_classifier = randomForest(x=train[,-1], y=train[,1], ntree=1000, mtry=10, importance=TRUE, na.action=na.omit, 
                             localImp = TRUE)
rf_classifier

varImpPlot(rf_classifier, n.var=4)


predictions<- predict(rf_classifier,test[,-1])
confusionMatrix(data=predictions, reference = test$check, positive="1")
as.data.frame(sort(round(importance(rf_classifier)[,2], 2),decreasing = TRUE),optional = T)

data_subset %>% 
  group_by(NUM_IMPRESE_INVITATE) %>% 
  summarise(n = n(), 
            check = round(sum(check==1)/n(), 2),) %>% 
  filter(n > 5) %>% 
  arrange(desc(check)) %>% 
  ggplot() +
  geom_col(aes(NUM_IMPRESE_INVITATE, check)) +
  xlim(c(0, 50))


data_subset %>% 
  group_by(NUMERO_OFFERTE_AMMESSE) %>% 
  summarise(n = n(), 
            check = round(sum(check==1)/n(), 2)) %>% 
  arrange(desc(check)) %>% 
  ggplot() +
  geom_col(aes(NUMERO_OFFERTE_AMMESSE, check)) +
  xlim(c(0, 50))


data_subset %>% 
  group_by(NUMERO_OFFERTE_AMMESSE) %>% 
  summarise(n = n(), 
            check = round(sum(check==1)/n(), 2)) %>% 
  arrange(desc(check)) %>% 
  View

data_subset %>% 
  group_by(MODALITA_REALIZZAZIONE) %>% 
  summarise(n = n(), 
            check = round(sum(check==1)/n(), 2),) %>% 
  filter(n > 5) %>% 
  arrange(desc(check)) %>% 
  ggplot() +
  geom_col(aes(MODALITA_REALIZZAZIONE, check)) +
  theme(axis.text.x = element_text(angle = 90, vjust = -1, hjust=1))



data_subset %>% 
  group_by(N_LOTTI_COMPONENTI) %>% 
  summarise(n = n(), 
            check = round(sum(check==1)/n(), 2),) %>% 
  filter(n > 5) %>% 
  arrange(desc(check)) %>% 
  ggplot() +
  geom_col(aes(N_LOTTI_COMPONENTI, check)) +
  theme(axis.text.x = element_text(angle = 90, vjust = -1, hjust=1))+
  xlim(c(0, 30))

data_subset %>% 
  group_by(nome_regione) %>% 
  summarise(n = n(), 
            check = round(sum(check==1)/n(), 2),) %>% 
  filter(n > 2) %>% 
  arrange(desc(check)) %>% 
  View

data_subset %>% 
  group_by(CPV.DESCRIZIONE_CPV) %>% 
  summarise(n = n(), 
            check = round(sum(check==1)/n(), 2),) %>% 
  filter(n > 2) %>% 
  arrange(desc(check)) %>% 
  View

data_subset %>% 
  group_by(CPV.DESCRIZIONE_CPV) %>% 
  summarise(n = n(), 
            check = round(sum(check==1)/n(), 2),) %>% 
  filter(n > 2) %>% 
  arrange(desc(check)) %>% 
  View


data_subset %>% 
  group_by(MODALITA_REALIZZAZIONE) %>% 
  summarise(n = n(), 
            check = round(sum(check==1)/n(), 2),) %>% 
  filter(n > 5) %>% 
  arrange(desc(check)) %>% 
  View

data_subset %>% 
  group_by(importo_cat8) %>% 
  summarise(n = n(), 
            check = round(sum(check==1)/n(), 2),) %>% 
  filter(n > 5) %>% 
  arrange(desc(check)) %>% 
  View


data_subset %>% 
  group_by(NUM_IMPRESE_OFFERENTI) %>% 
  summarise(n = n(), 
            check = round(sum(check==1)/n(), 2),) %>% 
  filter(n > 5) %>% 
  arrange(desc(check)) %>% 
  View



data_subset %>% 
  group_by(OGGETTO_PRINCIPALE_CONTRATTO) %>% 
  summarise(n = n(), 
            check = round(sum(check==1)/n(), 2),) %>% 
  filter(n > 5) %>% 
  arrange(desc(check)) %>% 
  View


data_subset %>% 
  group_by(NUMERO_OFFERTE_AMMESSE) %>% 
  summarise(n = n(), 
            check = round(sum(check==1)/n(), 2),) %>% 
  filter(n > 5) %>% 
  arrange(desc(check)) %>% 
  View


