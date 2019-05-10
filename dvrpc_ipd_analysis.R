# Questions I'll need to answer
# What is a CV? What's an "acceptable CV?"
# What's the distribution of CVs across variables? (Summary of each variable)
# What's the distribution of CVs in the region? (Map of mean CV)
# How are CVs in tracts where IPD score is high vs. the rest of the region?
# (Table of mean CVs by IPD score bracket)

# Packages, functions, input data
library(here); library(tidyverse); library(tigris)
library(sf); library(viridis); library(extrafont)
options(tigris_class = "sf")
loadfonts(device = "win")
summarizer <- function(i){
  cv_min <- min(i, na.rm = TRUE)
  cv_med <- median(i, na.rm = TRUE)
  cv_mean <- mean(i, na.rm = TRUE)
  cv_max <- max(i, na.rm = TRUE)
  res <- c(cv_min, cv_med, cv_mean, cv_max)
  return(res)
}
a <- st_read(here("dl_geo", "a_cty.shp"))
a_mask <- st_union(a)
h2o <- st_read(here("dl_geo", "h2o.shp")) %>%
  st_transform(., st_crs(a_mask)) %>%
  st_intersection(., a_mask)
ipd <- read_csv(here("dl_data", "ipd.csv")) %>%
  select(GEOID10, ends_with("PctEst"), ends_with("PctMOE")) %>%
  mutate_all(funs(ifelse(. == -99999, NA, .))) %>%
  select(GEOID10, order(current_vars()))
cv_names <- unique(str_replace_all(names(ipd)[2:length(ipd)], "PctEst|PctMOE", "CV"))
cv_cat_names <- unique(str_replace_all(names(ipd)[2:length(ipd)], "PctEst|PctMOE", "Cat"))
cv_df <- NULL
cv_cat_df <- NULL
for (i in seq(2, length(ipd) - 1, by = 2)){
  est <- ipd[i]
  moe <- ipd[i + 1]
  cv <- as.numeric(unlist((moe / 1.645) / est * 100))
  cv[is.infinite(cv)] <- 100
  cv_cat <- case_when(cv <= 15 ~ "0-15%",
                      cv > 15 & cv <= 30 ~ "15.1-30%",
                      cv > 30 & cv <= 60 ~ "30.1-60%",
                      cv > 60 ~ "60.1+%")
  cv_df <- cbind(cv_df, cv)
  cv_cat_df <- cbind(cv_cat_df, cv_cat)
}
cv_df <- as_tibble(cv_df)
names(cv_df) <- cv_names
cv_cat_df <- as_tibble(cv_cat_df)
names(cv_cat_df) <- cv_cat_names

# What's the distribution of CVs across variables? (Summary of each variable)
cv_sumstat <- as_tibble(sapply(cv_df, summarizer))
cv_cat_cnt <- as_tibble(apply(cv_cat_df, 2, table))
cv_cat_pct <- as_tibble(apply(cv_cat_cnt, 2,
                              function(i) round(i / sum(i) * 100, 2)))

# What's the distribution of CVs in the region? (Map of mean CV)
stcty <- c("42101", "42091", "42045", "42029", "42017",
           "34021", "34015", "34007", "34005")
st <- str_sub(stcty, 1, 2)
cty <- str_sub(stcty, 3, 5)
reg <- map2(
  st, cty,
  ~ tracts(state = .x,
           county = .y)
  )
reg <- do.call("rbind", reg)

cv_bygeo <- bind_cols(ipd, cv_df) %>%
  bind_cols(., cv_cat_df) %>%
  mutate_at(vars(GEOID10), as.character) %>%
  mutate(mean_cv = (D_CV + EM_CV + F_CV +
                      FB_CV + LEP_CV + LI_CV +
                      OA_CV + RM_CV + Y_CV) / 9,
         mean_cv_cat = case_when(cv <= 15 ~ "0-15%",
                                 cv > 15 & cv <= 30 ~ "15.1-30%",
                                 cv > 30 & cv <= 60 ~ "30.1-60%",
                                 cv > 60 ~ "60.1+%")) %>%
  inner_join(reg, ., by = c("GEOID" = "GEOID10"))

ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(data = cv_bygeo, aes(fill = mean_cv_cat), color = NA) +
  scale_fill_viridis_d("Mean CV", na.value = "gainsboro") +
  labs(title = "Mean IPD CV by census tract") +
  geom_sf(data = h2o, fill = "#669999", color = NA) +
  geom_sf(data = a, fill = NA, color = "gray") +
  coord_sf(datum = NA)
ggsave(here("figs", "ipd_tract_cv.png"), width = 10, height = 7.5, units = "in", dpi = 400)

for (v in cv_cat_names) {
  var_sym <- str_split_fixed(v, "_", n = 2)[1,1]
  ggplot() +
    theme(text = element_text(family = "CMU Serif")) +
    theme(panel.background = element_blank()) +
    geom_sf(data = cv_bygeo, aes(fill = !!ensym(v)), color = NA) +
    scale_fill_viridis_d("CV", na.value = "gainsboro") +
    labs(title = paste(var_sym, "CV by census tract", sep = " ")) +
    geom_sf(data = h2o, fill = "#669999", color = NA) +
    geom_sf(data = a, fill = NA, color = "gray") +
    coord_sf(datum = NA)
  ggsave(here("figs", paste("ipd", v, "CV.png", sep = "_")), width = 10, height = 7.5, units = "in", dpi = 400)
}

# How are CVs in tracts where IPD score is high vs. the rest of the region?
# (Table of mean CVs by IPD score bracket)
scr <- read_csv(here("dl_data", "ipd.csv")) %>%
  mutate_at(vars(GEOID10), as.character) %>%
  mutate_all(funs(ifelse(. == -99999, NA, .))) %>%
  mutate(IPD_Bin = case_when(IPD_Score <= 12 ~ "0-12",
                             IPD_Score > 12 & IPD_Score <= 18 ~ "13-18",
                             IPD_Score > 18 & IPD_Score <= 24 ~ "19-24",
                             IPD_Score > 24 ~ "25-36")) %>%
  select(GEOID10, IPD_Bin)

cv_byscr <- left_join(cv_bygeo, scr, by = c("GEOID" = "GEOID10")) %>%
  mutate(`High CV` = case_when(mean_cv_cat != "0-15%" & IPD_Bin == "25-36" ~ "IPD Score 25-36, High CV",
                               mean_cv_cat == "0-15%" & IPD_Bin == "25-36" ~ "IPD Score 25-36, Low CV",
                               mean_cv_cat != "0-15%" & IPD_Bin != "25-36" ~ "IPD Score 0-24, High CV",
                               mean_cv_cat == "0-15%" & IPD_Bin != "25-36" ~ "IPD Score 0-24, Low CV"))
cv_byscr_flat <- cv_byscr %>% st_set_geometry(NULL)
cv_cat_scr <- as_tibble(table(cv_byscr_flat$mean_cv_cat, cv_byscr_flat$IPD_Bin), .name_repair = make.names)

ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(legend.title = element_blank()) +
  theme(panel.background = element_blank()) +
  geom_sf(data = cv_byscr, aes(fill = `High CV`), color = NA) +
  scale_fill_viridis_d(na.value = "gainsboro") +
  labs(title = "Census tracts by IPD score and CV") +
  geom_sf(data = h2o, fill = "#669999", color = NA) +
  geom_sf(data = a, fill = NA, color = "gray") +
  coord_sf(datum = NA)
ggsave(here("figs", "ipd_tract_cv_scr.png"), width = 10, height = 7.5, units = "in", dpi = 400)

write_csv(cv_sumstat, here("output_data", "ipd_cv_sumstat.csv"))
write_csv(cv_cat_cnt, here("output_data", "ipd_cv_cat_cnt.csv"))
write_csv(cv_cat_pct, here("output_data", "ipd_cv_cat_pct.csv"))
write_csv(cv_cat_scr, here("output_data", "ipd_cv_cat_scr.csv"))
