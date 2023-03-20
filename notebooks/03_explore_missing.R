## study missing at random, not at random etc
## https://github.com/topepo/FES/blob/master/08_Handling_Missing_Data/08_01_Understanding.R
library(caret)
library(tidyverse)
library(naniar)
library(visdat)
library(ComplexHeatmap)


theme_set(theme_bw())

## load and prep
# source("notebooks/01_load_data.R")
# source("notebooks/02_prep_enrich_data.R")


cigs_2020_joined <- readRDS("~/Desktop/r_projects/cladag-23/data/cigs_2020_joined.rds")

convert_missing <- function(x) ifelse(is.na(x), 0, 1)
total_missing <- apply(cigs_2020_joined, 2, convert_missing)
# https://bookdown.org/max/FES/understanding-the-nature-and-severity-of-missing-information.html#fig:missing-vis



## heatmap ----
Heatmap(
  total_missing[1:10,], 
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


## missing pairs ----
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



## pca ----


## convertig data into missing and non missing 0s and 1s
## then calculating percentage of missing for each cols 
## then retaining first two components which capture missingness

 
total_missing_pcn <-
  apply(cigs_2020_joined, MARGIN = 1, function(x)
    sum(is.na(x))) / ncol(cigs_2020_joined)

pca_total <- prcomp(total_missing)

pca_d <- 
  data.frame(pca_total$x) %>%
  dplyr::select(PC1, PC2) %>% 
  mutate(Percent = total_missing_pcn * 100) %>% 
  dplyr::distinct(PC1,PC2, Percent)

pca_d_rng <- extendrange(c(pca_d$PC1, pca_d$PC2))

# https://bookdown.org/max/FES/understanding-the-nature-and-severity-of-missing-information.html#fig:missing-PCA-dates
ggplot(pca_d, aes(x = PC1, y = PC2, size = Percent)) +
  geom_point(alpha = .5, col = "#1B9E77") +
  xlim(pca_d_rng) + 
  ylim(pca_d_rng) 
  # scale_size(limits = c(0, 10), range = c(.5, 10))




## redo it withouy grouping by contract,
## instead grouping by row.




## other viz





