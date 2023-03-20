# missing <- data_original %>% 
#   select(missing=flag_agg_assente, 
#          anno_pubbl, importo_cat, scelta_contr2, 
#          rip_geo, nome_provincia,BANDO.OGGETTO_PRINCIPALE_CONTRATTO, 
#          BANDO.SETTORE, 
#          BANDO.MODALITA_REALIZZAZIONE, 
#          BANDO.IMPORTO_COMPLESSIVO_GARA, 
#          BANDO.IMPORTO_LOTTO, 
#          BANDO.N_LOTTI_COMPONENTI
#          #BANDO.CPV.COD_CPV
#          ) %>% 
#   mutate(missing=factor(as.numeric(missing))) %>% 
#   na.omit()
# 
# write.csv(missing, file = "missing.csv")

# missing 1 se assente

library(dplyr)
library(forcats)
library(tidyr)
library(sf)
library(ggplot2)
library(tmap)
map <- read_sf("Limiti01012021_g/ProvCM01012021_g/ProvCM01012021_g_WGS84.shp") %>% 
  mutate(DEN_UTS = case_when(DEN_UTS == "Bolzano" ~ "Bolzano/Bozen", 
                             DEN_UTS == "Forli'-Cesena" ~ "Forl<ec>-Cesena", 
                             DEN_UTS == "Massa Carrara"  ~ "Massa-Carrara", 
                             DEN_UTS == "Reggio di Calabria"  ~ "Reggio Calabria", 
                             DEN_UTS == "Aosta" ~ "Valle d'Aosta/Vall<e9>e d'Aoste", 
                             TRUE ~ DEN_UTS)) %>% 
  select(COD_PROV, DEN_UTS)

missing <- read.csv("missing.csv")[, -1]

# mappa ----

mappa <- missing %>% 
  select(missing, anno_pubbl, nome_provincia, BANDO.OGGETTO_PRINCIPALE_CONTRATTO, BANDO.SETTORE) %>% 
  group_by(anno_pubbl, nome_provincia) %>% 
  summarise(miss = mean(missing)) 


map1 <- map %>% 
  right_join(mappa, by=c("DEN_UTS"="nome_provincia")) 

m1 <- tm_shape(map1) + 
  tm_fill("miss",
          style = "quantile",
          n=8, 
          palette = "Blues", 
          title = "Missing") +
  tm_borders(col = "black", lwd = 0.2) +
  tm_layout(frame = FALSE)
m1
tmap_save(m1, "choropleth.png", width=1080, height=1920)


mappa <- missing %>% 
  select(missing, anno_pubbl, nome_provincia, BANDO.OGGETTO_PRINCIPALE_CONTRATTO, BANDO.SETTORE) %>% 
  group_by(nome_provincia, BANDO.OGGETTO_PRINCIPALE_CONTRATTO, BANDO.SETTORE) %>% 
  summarise(miss = mean(missing))

map2 <- map %>% 
  right_join(mappa, by=c("DEN_UTS"="nome_provincia")) 


m2 <- tm_shape(map2) + 
  tm_fill("miss",
          style = "quantile",
          n=8, 
          palette = "Blues", 
          title = "Missing") +
  tm_borders(col = "black", lwd = 0.2) +
  tm_layout(frame = FALSE) +
  tm_facets(by=c("BANDO.SETTORE", "BANDO.OGGETTO_PRINCIPALE_CONTRATTO"), ncol  =3, showNA = FALSE)

m2
tmap_save(m2, "choropleth_facet.png", width=1920*2, height=1080*2)

# modello ----


missing <- missing %>% 
  select(-importo_cat) %>% 
  mutate(missing = as.factor(missing), 
         BANDO.MODALITA_REALIZZAZIONE=fct_lump_prop(BANDO.MODALITA_REALIZZAZIONE, prop = 0.3)) 
summary(missing)

model <- glm( missing ~ .-1, data = missing, family = binomial)
summary(model)

missing <- missing %>% 
  mutate(predict = predict.glm(model, newdata = ., type="response"))



# ids <- sample(1:nrow(missing), nrow(missing)*0.5)
# train <- missing[ids, ]
# test <- missing[-ids, ]
# rf <- randomForest(missing ~., data=train)
# rf
# 
# rf$importance
# 
# t.test(BANDO.IMPORTO_COMPLESSIVO_GARA ~ missing, data=train)
# t.test(BANDO.IMPORTO_LOTTO ~ missing, data=train)
# anova()
# res.aov <- aov(missing ~ BANDO.MODALITA_REALIZZAZIONE, data = missing)
# res.aov <- aov(BANDO.MODALITA_REALIZZAZIONE ~ missing, data = missing)
# 
# summary(res.aov)
# 
# summary(missing$BANDO.MODALITA_REALIZZAZIONE)
# 
# chisq <- chisq.test(housetasks)
# 
# train$missing
# train$BANDO.MODALITA_REALIZZAZIONE

realizzazione <- missing %>% 
  select(missing, BANDO.MODALITA_REALIZZAZIONE) %>% 
  group_by(missing, BANDO.MODALITA_REALIZZAZIONE) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from=missing, values_from=n)


# 
# chisq <- chisq.test(as.matrix(realizzazione[,2:3]))
# round(chisq$expected,2)
# round(chisq$observed,2)


anno <- missing %>% 
  select(missing, anno_pubbl) %>% 
  group_by(missing, anno_pubbl) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from=missing, values_from=n)

chisq <- chisq.test(as.matrix(anno[,2:3]))
chisq
round(chisq$expected,2)
round(chisq$observed,2)


# provincia

model <- glm( missing ~ nome_provincia-1, data = missing, family = binomial)
summary(model)


exp(coef(model))
prob <- exp(coef(model))/(1+exp(coef(model)))

prob[order(prob)]

missing %>% 
  select(missing, nome_provincia) %>% 
  group_by(missing, nome_provincia) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from=missing, values_from=n) %>% 
  mutate(ratio = `1`/(`1`+`0`)) %>% 
  arrange(ratio)


# oggetto contratto
model <- glm( missing ~ BANDO.OGGETTO_PRINCIPALE_CONTRATTO-1, data = missing, family = binomial)
summary(model)


exp(coef(model))
prob <- exp(coef(model))/(1+exp(coef(model)))

prob[order(prob)]

confi <- confint(model)
prob <- apply(confi, 2, function(x) exp(x)/(1+exp(x)))
prob


missing %>% 
  select(missing, BANDO.OGGETTO_PRINCIPALE_CONTRATTO) %>% 
  group_by(missing, BANDO.OGGETTO_PRINCIPALE_CONTRATTO) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from=missing, values_from=n) %>% 
  mutate(ratio = `1`/(`1`+`0`)) %>% 
  arrange(desc(ratio))


# ripartizione geografica
model <- glm( missing ~ rip_geo+BANDO.SETTORE+BANDO.N_LOTTI_COMPONENTI-1, data = missing, family = binomial)
summary(model)


exp(coef(model))
prob <- exp(coef(model))/(1+exp(coef(model)))

prob[order(prob)]

confi <- confint(model)
prob <- apply(confi, 2, function(x) exp(x)/(1+exp(x)))
prob

predict.glm(model, newdata = missing[1,], type="response")

missing[1,]


missing %>% 
  select(missing, BANDO.OGGETTO_PRINCIPALE_CONTRATTO) %>% 
  group_by(missing, BANDO.OGGETTO_PRINCIPALE_CONTRATTO) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from=missing, values_from=n) %>% 
  mutate(ratio = `1`/(`1`+`0`)) %>% 
  arrange(desc(ratio))






# Modello totale

model <- glm( missing ~ rip_geo-1, data = missing, family = binomial)
summary(model)

missing %>%
  select(BANDO.IMPORTO_LOTTO, BANDO.IMPORTO_COMPLESSIVO_GARA, BANDO.N_LOTTI_COMPONENTI) %>%
  cor()

exp(coef(model))
prob <- exp(coef(model))/(1+exp(coef(model)))

prob[order(prob)]

missing$prob <- (1-predict(model, missing, type="response"))
missing$weights <- 1/missing$prob

summary(missing$weights)
plot(density(as.numeric(missing$weights)))


