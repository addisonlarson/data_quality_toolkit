library(here)
library(tidyverse)
library(summarytools)
library(sf)
library(data.table)

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
# f2 f3 are estimate and MOE for zero car hhs
d_cty <- st_read(here("raw", "residence", "cty", "A112310.shp")) %>%
  rename(sum_est = F2, sum_moe = F3) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100)) %>%
  pull(cv)
d_puma <- st_read(here("raw", "residence", "puma", "A112310.shp")) %>%
  rename(sum_est = F2, sum_moe = F3) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100)) %>%
  pull(cv)
d_tad <- st_read(here("raw", "residence", "tad", "A112310.shp")) %>%
  rename(sum_est = F2, sum_moe = F3) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100)) %>%
  pull(cv)
d_trct <- st_read(here("raw", "residence", "tract", "A112310.shp")) %>%
  rename(sum_est = F2, sum_moe = F3) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100)) %>%
  pull(cv)
d_taz <- st_read(here("raw", "residence", "taz", "A112310.shp")) %>%
  rename(sum_est = F2, sum_moe = F3) %>%
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
  write_csv(., here("output_data", "by_var_1.csv"))

# f22 est f23 moe
d_cty <- st_read(here("raw", "residence", "cty", "A112310.shp")) %>%
  rename(sum_est = F22, sum_moe = F23) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100)) %>%
  pull(cv)
d_puma <- st_read(here("raw", "residence", "puma", "A112310.shp")) %>%
  rename(sum_est = F22, sum_moe = F23) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100)) %>%
  pull(cv)
d_tad <- st_read(here("raw", "residence", "tad", "A112310.shp")) %>%
  rename(sum_est = F22, sum_moe = F23) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100)) %>%
  pull(cv)
d_trct <- st_read(here("raw", "residence", "tract", "A112310.shp")) %>%
  rename(sum_est = F22, sum_moe = F23) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100)) %>%
  pull(cv)
d_taz <- st_read(here("raw", "residence", "taz", "A112310.shp")) %>%
  rename(sum_est = F22, sum_moe = F23) %>%
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

# Addendum to zero-car: classification x uncertainty
# f2 f3 are estimate and MOE for zero car hhs
# We'll use a 5-class standard deviation
zc <- st_read(here("raw", "residence", "tract", "A112310.shp")) %>%
  filter(str_sub(geoid, 1, 5) == "42101") %>%
  rename(sum_est = F2, sum_moe = F3) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100))
zc_breaks <- c(-1, 15.317, 318.147, 620.978, 923.808, max(zc$sum_est))
zc_labels <- c("(-Inf, -1.5 SD]",
               "(-1.5 SD, -0.5 SD]",
               "(-0.5 SD, 0.5 SD]",
               "(0.5 SD, 1.5 SD]",
               "(1.5 SD, Inf]")
zc <- zc %>%
  mutate(classification = cut(sum_est, breaks = zc_breaks, labels = zc_labels),
         cv_cat = case_when(cv <= 15 ~ "0-15",
                            cv > 15 & cv <= 30 ~ "15.1-30",
                            cv > 30 & cv <= 60 ~ "30.1-60",
                            cv > 60 ~ "60.1+")) %>%
  st_set_geometry(NULL) %>%
  select(classification, cv_cat)
write.table(table(zc$cv_cat, zc$classification),
            here("output_data", "zc_tab.csv"), sep = ",")
write.table(round(prop.table(table(zc$cv_cat, zc$classification)) * 100, 2),
            here("output_data", "zc_tab_pct.csv"), sep = ",")

# f22 est f23 moe
zc_1 <- st_read(here("raw", "residence", "tract", "A112310.shp")) %>%
  filter(str_sub(geoid, 1, 5) == "42101") %>%
  rename(sum_est = F22, sum_moe = F23) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100))
zc_1_breaks <- c(-1, 1, 97.891, 231.411, 364.932, max(zc_1$sum_est))
zc_1 <- zc_1 %>%
  mutate(classification = cut(sum_est, breaks = zc_1_breaks, labels = zc_labels),
         cv_cat = case_when(cv <= 15 ~ "0-15",
                            cv > 15 & cv <= 30 ~ "15.1-30",
                            cv > 30 & cv <= 60 ~ "30.1-60",
                            cv > 60 ~ "60.1+")) %>%
  st_set_geometry(NULL) %>%
  select(classification, cv_cat)
write.table(table(zc_1$cv_cat, zc_1$classification),
            here("output_data", "zc_xtab.csv"), sep = ",")
write.table(round(prop.table(table(zc_1$cv_cat, zc_1$classification)) * 100, 2),
            here("output_data", "zc_xtab_pct.csv"), sep = ",")

# Summary tables of data reliability by geography and table type
inputfile_xwalk <- read_csv(here("inputfile_rac_xwalk.csv")) %>%
  mutate(tableno = as.numeric(str_sub(file, 9, -1)))
keep_vars <- inputfile_xwalk %>%
  group_by(tableid, tableno) %>%
  summarize(available = n()) %>%
  filter(available == 5) %>%
  pull(tableid) %>%
  unique(.)
inputfile_xwalk <- inputfile_xwalk %>%
  filter(tableid %in% keep_vars) %>%
  write_csv(., here("output_data", "final_rac_fields.csv"))
full <- data.frame()
for(n in 1:nrow(inputfile_xwalk)){
  file_id <- paste0(inputfile_xwalk$file[n], "_", inputfile_xwalk$geo[n], ".csv")
  a <- inputfile_xwalk$geo[n]
  b <- read_csv(here("dl_data", file_id)) %>%
    mutate_at(vars(GEOID), as.character) %>%
    mutate(geo = a)
  full <- rbind(b, full)
}
full <- full %>%
  mutate(cat = case_when(cv <= 15 ~ "0-15%",
                         cv > 15 & cv <= 30 ~ "15.1-30%",
                         cv > 30 & cv <= 60 ~ "30.1-60%",
                         cv > 60 ~ "60.1+%")) %>%
  group_by(geo, cat) %>%
  summarize(n = n()) %>%
  mutate(pct = round(n / sum(n) * 100, 3)) %>%
  write_csv(., here("output_data", "final_rac_tab.csv"))

inputfile_xwalk <- read_csv(here("inputfile_wac_xwalk.csv")) %>%
  mutate(tableno = as.numeric(str_sub(file, 9, -1)))
keep_vars <- inputfile_xwalk %>%
  group_by(tableid, tableno) %>%
  summarize(available = n()) %>%
  filter(available == 5) %>%
  pull(tableid) %>%
  unique(.)
inputfile_xwalk <- inputfile_xwalk %>%
  filter(tableid %in% keep_vars) %>%
  write_csv(., here("output_data", "final_wac_fields.csv"))
full <- data.frame()
for(n in 1:nrow(inputfile_xwalk)){
  file_id <- paste0(inputfile_xwalk$file[n], "_", inputfile_xwalk$geo[n], ".csv")
  a <- inputfile_xwalk$geo[n]
  b <- read_csv(here("dl_data", file_id)) %>%
    mutate_at(vars(GEOID), as.character) %>%
    mutate(geo = a)
  full <- rbind(b, full)
}
full <- full %>%
  mutate(cat = case_when(cv <= 15 ~ "0-15%",
                         cv > 15 & cv <= 30 ~ "15.1-30%",
                         cv > 30 & cv <= 60 ~ "30.1-60%",
                         cv > 60 ~ "60.1+%")) %>%
  group_by(geo, cat) %>%
  summarize(n = n()) %>%
  mutate(pct = round(n / sum(n) * 100, 3)) %>%
  write_csv(., here("output_data", "final_wac_tab.csv"))

inputfile_xwalk <- read_csv(here("inputfile_od_xwalk.csv")) %>%
  distinct(.) %>%
  mutate(tableno = as.numeric(str_sub(file, 9, -1)))
# Modification: only keep tables available at all geos
keep_vars <- inputfile_xwalk %>%
  group_by(tableid, tableno) %>%
  summarize(available = n()) %>%
  filter(available == 3) %>%
  pull(tableid) %>%
  unique(.)
inputfile_xwalk <- inputfile_xwalk %>%
  filter(tableid %in% keep_vars)
# Make a note of no. tables and no. fields
length(keep_vars)
write_csv(inputfile_xwalk, here("output_data", "final_od_fields.csv"))
# Read all datasets for your table type (residence etc)
full <- data.frame()
for(n in 1:nrow(inputfile_xwalk)){
  file_id <- paste0(inputfile_xwalk$file[n], "_", inputfile_xwalk$geo[n], ".csv")
  a <- read_csv(here("dl_data", file_id)) %>%
    mutate_at(vars(GEOID), as.character)
  full <- rbind(a, full)
}
full <- full %>%
  mutate(geo_chars = nchar(GEOID),
         geo = case_when(geo_chars == 14 ~ "PUMA",
                         geo_chars == 12 ~ "County",
                         geo_chars == 23 ~ "TAD")) %>%
  mutate(cat = case_when(cv <= 15 ~ "0-15%",
                         cv > 15 & cv <= 30 ~ "15.1-30%",
                         cv > 30 & cv <= 60 ~ "30.1-60%",
                         cv > 60 ~ "60.1+%")) %>%
  group_by(geo, cat) %>%
  summarize(n = n()) %>%
  mutate(pct = round(n / sum(n) * 100, 3))
write_csv(full, here("output_data", "final_od_tab.csv"))

# Stack up 5 fields with identical universes
rac <- c("A102101", "A102102", "A102105", "A102106", "A102110")
wac <- c("A202100", "A202101", "A202104", "A202105", "A202113")
od <- c("A302100", "B302101", "B302102", "A302103", "B302106")
variabletype <- c("tot_workers", "age", "industry", "transpo", "trav_time")

rac_fields <- read_csv(here("inputfile_rac_xwalk.csv")) %>%
  filter(tableid %in% rac & geo == "cty") %>%
  filter(!(file %in% c("A102102_5", "A102105_5", "A102110_5", "A102106_5"))) # Drop duplicate universes
rac_full <- data.frame()
for (t in 1:nrow(rac_fields)){
  temp <- read_csv(here("dl_data", paste(rac_fields$file[t], "cty.csv", sep = "_"))) %>%
    mutate(tableid = rac_fields$tableid[t], tabletype = "rac") %>%
    mutate_at(vars(GEOID), as.character)
  rac_full <- rbind(rac_full, temp)
}

wac_fields <- read_csv(here("inputfile_wac_xwalk.csv")) %>%
  filter(tableid %in% wac & geo == "cty") %>%
  filter(!(file %in% c("A202101_5", "A202104_5", "A202105_5", "A202113_5"))) # Drop duplicate universes
wac_full <- data.frame()
for (t in 1:nrow(wac_fields)){
  temp <- read_csv(here("dl_data", paste(wac_fields$file[t], "cty.csv", sep = "_"))) %>%
    mutate(tableid = wac_fields$tableid[t], tabletype = "wac") %>%
    mutate_at(vars(GEOID), as.character)
  wac_full <- rbind(wac_full, temp)
}

od_fields <- read_csv(here("inputfile_od_xwalk.csv")) %>%
  filter(tableid %in% od & geo == "cty") %>%
  filter(!(file %in% c("A302103_3", "B302101_3", "B302102_3", "B302106_3"))) # Drop duplicate universes
od_full <- data.frame()
for (t in 1:nrow(od_fields)){
  temp <- read_csv(here("dl_data", paste(od_fields$file[t], "cty.csv", sep = "_"))) %>%
    mutate(tableid = od_fields$tableid[t], tabletype = "od") %>%
    mutate_at(vars(GEOID), as.character)
  od_full <- rbind(od_full, temp)
}

# Compare total workers across table types
for (i in 1:length(variabletype)){
  temp <- bind_rows(rac_full %>% filter(tableid == rac[i]),
                    wac_full %>% filter(tableid == wac[i])) %>%
    bind_rows(., od_full %>% filter(tableid == od[i])) %>%
    group_by(tabletype) %>%
    summarize(min_cv = min(cv),
              median_cv = median(cv),
              mean_cv = mean(cv),
              iqr_cv = IQR(cv),
              max_cv = max(cv)) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  temp <- dcast(melt(temp, id.vars = "tabletype"), variable ~ tabletype) %>%
    select(variable, rac, wac, od)
  write_csv(temp, here("output_data", paste(variabletype[i], "bytype.csv", sep = "_")))
}

# And overall
temp <- bind_rows(rac_full, wac_full) %>%
  bind_rows(., od_full) %>%
  group_by(tabletype) %>%
  summarize(min_cv = min(cv),
            median_cv = median(cv),
            mean_cv = mean(cv),
            iqr_cv = IQR(cv),
            max_cv = max(cv)) %>%
  mutate_if(is.numeric, funs(round(., 2)))
temp <- dcast(melt(temp, id.vars = "tabletype"), variable ~ tabletype) %>%
  select(variable, rac, wac, od)
write_csv(temp, here("output_data", "overall_bytype.csv"))
