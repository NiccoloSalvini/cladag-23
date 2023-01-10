## study missing at random, not at random etc
## https://github.com/topepo/FES/blob/master/08_Handling_Missing_Data/08_01_Understanding.R
library(caret)
library(tidyverse)
library(naniar)
library(visdat)
library(ComplexHeatmap)


theme_set(theme_bw())

## load and prep
source("notebooks/01_load_data.R")
source("notebooks/02_prep_enrich_data.R")

convert_missing <- function(x) ifelse(is.na(x), 0, 1)
total_missing <- apply(total, 2, convert_missing)
# https://bookdown.org/max/FES/understanding-the-nature-and-severity-of-missing-information.html#fig:missing-vis

Heatmap(
  total_missing, 
  name = "Missing", #title of legend
  column_title = "Predictors", row_title = "Samples",
  col = c("black","lightgrey"),
  show_heatmap_legend = FALSE,
  row_names_gp = gpar(fontsize = 0) # Text size for row names
)

gg_miss_upset(total_missing, nsets = 7) 



total_flat <- 
  total %>%
  mutate(flat = ifelse(flat == 1, "yes", "no"))


## decide dimensions
ggplot(total_flat, aes(col = flat)) + 
  geom_point(aes(x = Diameter, y = importo_lotto), alpha = .5) + 
  geom_rug(data = total_flat[is.na(total_flat$Mass),], 
           aes(x = Diameter), 
           sides = "b", 
           lwd = 1)+ 
  geom_rug(data = total_flat[is.na(total_flat$Diameter),], 
           aes(y = Mass), 
           sides = "l", 
           lwd = 1) + 
  theme(legend.position = "top")



## pca
total_missing_pca <-
  apply(total, MARGIN = 1, function(x)
    sum(is.na(x))) / ncol(total)

pca_total <- prcomp(total_missing_pca)

pca_d <- 
  data.frame(pca_total$x) %>%
  dplyr::select(PC1) %>% 
  mutate(Percent = total_missing_pca * 100) %>% 
  dplyr::distinct(PC1, Percent)

