library(here)
library(tidyverse)
library(summarytools)
library(sf)

# Dissolve boundary
region <- st_read(here("dl_geo", "a_cty.shp")) %>%
  st_union(.)
  
# Reliability by geography
d_cty <- read_csv(here("dl_data", "A102101_5_cty.csv")) %>%
  mutate_at(vars(GEOID), as.character) %>%
  pull(cv)
d_puma <- read_csv(here("dl_data", "A102101_5_puma.csv")) %>%
  mutate_at(vars(GEOID), as.character) %>%
  pull(cv)
d_tad <- read_csv(here("dl_data", "A102101_5_tad.csv")) %>%
  mutate_at(vars(GEOID), as.character) %>%
  pull(cv)
d_trct <- read_csv(here("dl_data", "A102101_5_tract.csv")) %>%
  mutate_at(vars(GEOID), as.character) %>%
  pull(cv)
d_taz <- read_csv(here("dl_data", "A102101_5_taz.csv")) %>%
  mutate_at(vars(GEOID), as.character) %>%
  pull(cv)
full <- list(d_cty, d_puma, d_tad, d_trct, d_taz)
full_name <- c("County", "PUMA", "TAD", "Tract", "TAZ")
# Not perfect but works
sum_tab <- matrix(nrow = 4)
for (i in 1:length(full)){
  res <- descr(full[[i]], stats = c("min", "med", "mean", "max")) %>%
    as.data.frame(.) %>%
    mutate_all(round, 2)
  names(res) <- full_name[i]
  sum_tab <- cbind(sum_tab, res)
}
sum_tab <- sum_tab[,2:ncol(sum_tab)] %>%
  as.data.frame(.) %>%
  write_csv(., here("output_data", "by_geo.csv"))

# Reliability by variable detail / crosstabs
d_cty <- read_csv(here("dl_data", "A111102_7_cty.csv")) %>%
  mutate_at(vars(GEOID), as.character) %>%
  pull(cv)
d_puma <- read_csv(here("dl_data", "A111102_7_puma.csv")) %>%
  mutate_at(vars(GEOID), as.character) %>%
  pull(cv)
d_tad <- read_csv(here("dl_data", "A111102_7_tad.csv")) %>%
  mutate_at(vars(GEOID), as.character) %>%
  pull(cv)
d_trct <- read_csv(here("dl_data", "A111102_7_tract.csv")) %>%
  mutate_at(vars(GEOID), as.character) %>%
  pull(cv)
d_taz <- read_csv(here("dl_data", "A111102_7_taz.csv")) %>%
  mutate_at(vars(GEOID), as.character) %>%
  pull(cv)
full <- list(d_cty, d_puma, d_tad, d_trct, d_taz)
full_name <- c("County", "PUMA", "TAD", "Tract", "TAZ")
sum_tab <- matrix(nrow = 4)
for (i in 1:length(full)){
  res <- descr(full[[i]], stats = c("min", "med", "mean", "max")) %>%
    as.data.frame(.) %>%
    mutate_all(round, 2)
  names(res) <- full_name[i]
  sum_tab <- cbind(sum_tab, res)
}
sum_tab <- sum_tab[,2:ncol(sum_tab)] %>%
  as.data.frame(.) %>%
  write_csv(., here("output_data", "by_var_1.csv"))

# f32 est f33 moe
d_cty <- st_read(here("raw", "cty", "A112310.shp")) %>%
  rename(sum_est = F32, sum_moe = F33) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100)) %>%
  pull(cv)
d_puma <- st_read(here("raw", "puma", "A112310.shp")) %>%
  rename(sum_est = F32, sum_moe = F33) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100)) %>%
  pull(cv)
d_tad <- st_read(here("raw", "tad", "A112310.shp")) %>%
  rename(sum_est = F32, sum_moe = F33) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100)) %>%
  pull(cv)
d_trct <- st_read(here("raw", "tract", "A112310.shp")) %>%
  rename(sum_est = F32, sum_moe = F33) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100)) %>%
  pull(cv)
d_taz <- st_read(here("raw", "taz", "A112310.shp")) %>%
  rename(sum_est = F32, sum_moe = F33) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100)) %>%
  pull(cv)
full <- list(d_cty, d_puma, d_tad, d_trct, d_taz)
full_name <- c("County", "PUMA", "TAD", "Tract", "TAZ")
sum_tab <- matrix(nrow = 4)
for (i in 1:length(full)){
  res <- descr(full[[i]], stats = c("min", "med", "mean", "max")) %>%
    as.data.frame(.) %>%
    mutate_all(round, 2)
  names(res) <- full_name[i]
  sum_tab <- cbind(sum_tab, res)
}
sum_tab <- sum_tab[,2:ncol(sum_tab)] %>%
  as.data.frame(.) %>%
  write_csv(., here("output_data", "by_var_2.csv"))

