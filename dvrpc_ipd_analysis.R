# Questions I'll need to answer
# What is a CV? What's an "acceptable CV?"
# What's the distribution of CVs across variables? (Summary of each variable)
# What's the distribution of CVs in the region? (Map of mean CV)
# Given the estimates, their sample error, and the assigned score,
# what's the likelihood of improper classification? (Summary tables, maps)
# How are CVs in tracts where IPD score is high vs. the rest of the region?
# (Table of mean CVs by IPD score bracket)

# Packages, functions, input data
library(here); library(tidyverse); library(tigris)
library(sf); library(viridis); library(extrafont)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
loadfonts(device = "win")
summarizer <- function(i){
  cv_min <- min(i, na.rm = TRUE)
  cv_med <- median(i, na.rm = TRUE)
  cv_mean <- mean(i, na.rm = TRUE)
  cv_max <- max(i, na.rm = TRUE)
  res <- c(cv_min, cv_med, cv_mean, cv_max)
  return(res)
}
# Obtain percentage lower-bound errors
lower <- function(x, i){
  pnorm(as.numeric(as.character(x[i]$lowerBound)),
        mean = x[i]$estimate, sd = x[i]$sd,
        lower.tail = TRUE) * 100
}
# Obtain percentage upper-bound errors
upper <- function(x, i){
  pnorm(as.numeric(as.character(x[i]$upperBound)),
        mean = x[i]$estimate, sd = x[i]$sd,
        lower.tail = FALSE) * 100
}
# Mean map error by class
average <- function(x, i){
  mean(x[i])
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
ggsave(here("figs", "ipd_tract_cv.png"), width = 7.5, height = 5.625, units = "in", dpi = 400)

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
  ggsave(here("figs", paste("ipd", v, "CV.png", sep = "_")), width = 7.5, height = 5.625, units = "in", dpi = 400)
}

# Given the estimates, their sample error, and the assigned score,
# what's the likelihood of improper classification? (Summary tables, maps)
cb <- read_csv(here("dl_data", "ipd_class_breaks.csv"))
est_names <- names(ipd)[str_detect(names(ipd), "PctEst")]
moe_names <- names(ipd)[str_detect(names(ipd), "PctMOE")]
err_by_indicator <- matrix(nrow = nrow(ipd), ncol = length(est_names))
for (idx in 1:length(est_names)){
  var_sym <- str_split_fixed(est_names[idx], "_", n = 2)[1,1]
  dat <- ipd %>% select(!!est_names[idx], !!moe_names[idx]) %>%
    rename(estimate = c(1), moe = c(2)) %>%
    mutate(sd = moe / 1.645)
  userBreaks <- cb %>% select(!!est_names[idx]) %>% pull(.)
  dat$classCode <- cut(dat$estimate, labels = FALSE, breaks = userBreaks,
                       include.lowest = TRUE, right = TRUE)
  dat$lowerBound <- cut(dat$estimate,
                        breaks = userBreaks,
                        labels = c(userBreaks[1:length(userBreaks) - 1]),
                        include.lowest = TRUE, right = TRUE)
  dat$upperBound <- cut(dat$estimate,
                        breaks = userBreaks,
                        labels = c(userBreaks[2:length(userBreaks)]),
                        include.lowest = TRUE, right = TRUE)
  dat$lowerBound <- as.numeric(as.character(dat$lowerBound))
  dat$upperBound <- as.numeric(as.character(dat$upperBound))
  dat$lowerBound[dat$lowerBound == min(dat$lowerBound)] <- -Inf
  dat$upperBound[dat$upperBound == max(dat$upperBound)] <- Inf
  dat$lbe <- pnorm(dat$lowerBound, mean = dat$estimate, sd = dat$sd, lower.tail = TRUE) * 100
  dat$ube <- pnorm(dat$upperBound, mean = dat$estimate, sd = dat$sd, lower.tail = FALSE) * 100
  dat$cert <- 100 - (dat$lbe + dat$ube)
  err_by_indicator[,idx] <- dat$lbe + dat$ube
  dat <- dat %>% mutate(cert_class = case_when(cert <= 25 ~ "0-25%",
                                              cert > 25 & cert <= 50 ~ "25.1-50%",
                                              cert > 50 & cert <= 75 ~ "50.1-75%",
                                              cert > 75 ~ "75.1-100%"))
  dat$cert_class <- as.factor(dat$cert_class)
  dat$GEOID <- as.character(ipd$GEOID10)
  dat <- left_join(reg, dat)
  p <- ggplot() +
    theme(text = element_text(family = "CMU Serif")) +
    theme(panel.background = element_blank()) +
    geom_sf(data = dat, aes(fill = cert_class), color = NA) +
    scale_fill_viridis_d("Certainty", na.value = "gainsboro", direction = -1) +
    labs(title = paste(var_sym, "certainty of classification", sep = " "),
         subtitle = "Possible values range from 0-100%") +
    geom_sf(data = h2o, fill = "#669999", color = NA) +
    geom_sf(data = a, fill = NA, color = "gray") +
    coord_sf(datum = NA)
  png(here("figs", paste("ipd", var_sym, "ClassErr.png", sep = "_")), width = 7.5, height = 5.625, units = "in", res = 400)
  plot(p)
  dev.off()
}
# ipd_tract_ClassErr
err_by_trct <- data.frame(err = rowSums(err_by_indicator) / 9)
err_by_trct$GEOID <- as.character(ipd$GEOID10)
err_by_trct <- left_join(reg, err_by_trct) %>%
  mutate(err_class = case_when(err <= 10 ~ "0-10%",
                               err > 10 & err <= 20 ~ "10.1-20%",
                               err > 20 & err <= 30 ~ "20.1-30%",
                               err > 30 ~ "30.1+%"))
ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(data = err_by_trct, aes(fill = err_class), color = NA) +
  scale_fill_viridis_d("Mean error", na.value = "gainsboro", direction = -1) +
  labs(title = "Mean classification error of IPD input variables") +
  geom_sf(data = h2o, fill = "#669999", color = NA) +
  geom_sf(data = a, fill = NA, color = "gray") +
  coord_sf(datum = NA)
ggsave(here("figs", "ipd_tract_ClassErr.png"), width = 7.5, height = 5.625, units = "in", dpi = 400)

# Mean classification error by indicator
mean_err_by_indicator <- data.frame(mean_err = round(colMeans(err_by_indicator, na.rm = TRUE),3))
mean_err_by_indicator$var <- est_names
write_csv(mean_err_by_indicator,
          here("output_data", "ipd_indicator_class_errors.csv"))

full_errors <- list()
for (idx in 1:9){
  dat <- ipd %>% select(!!est_names[idx], !!moe_names[idx]) %>%
    rename(estimate = c(1), moe = c(2)) %>%
    mutate(sd = moe / 1.645) %>%
    drop_na()
  userBreaks <- cb %>% select(!!est_names[idx]) %>% pull(.)
  dat$classCode <- cut(dat$estimate, labels = FALSE, breaks = userBreaks,
                       include.lowest = TRUE, right = TRUE)
  dat$lowerBound <- cut(dat$estimate,
                        breaks = userBreaks,
                        labels = c(userBreaks[1:length(userBreaks) - 1]),
                        include.lowest = TRUE, right = TRUE)
  dat$upperBound <- cut(dat$estimate,
                        breaks = userBreaks,
                        labels = c(userBreaks[2:length(userBreaks)]),
                        include.lowest = TRUE, right = TRUE)
  dat$classCode <- paste0("Class", dat$classCode)
  dat2 <- split(dat, dat$classCode)
  dat2[[1]]$lowerBound <- -Inf
  dat2[[length(dat2)]]$upperBound <- Inf
  
  lowerBoundErrors <- sapply(dat2, lower)
  upperBoundErrors <- sapply(dat2, upper)
  totalObs <- sapply(dat2, function(i) nrow(i))
  totalErrorsU <- as.data.frame(sapply(lowerBoundErrors, average))
  colnames(totalErrorsU)[1] <- "LowerBound"
  totalErrorsU$UpperBound <- sapply(upperBoundErrors, average)
  totalErrorsU$totalObs <- totalObs
  totalErrorsU[4] <- totalErrorsU[1] + totalErrorsU[2]
  colnames(totalErrorsU)[4] <- "OvrAvr"
  totalErrorsU$var <- str_split_fixed(est_names[idx], "_", n = 2)[1,1]
  totalErrorsU <- as.data.frame(totalErrorsU)
  full_errors[[idx]] <- totalErrorsU
}
write_csv(do.call(rbind, full_errors), here("output_data", "ipd_class_errors.csv"))

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
ggsave(here("figs", "ipd_tract_cv_scr.png"), width = 7.5, height = 5.625, units = "in", dpi = 400)

write_csv(cv_sumstat, here("output_data", "ipd_cv_sumstat.csv"))
write_csv(cv_cat_cnt, here("output_data", "ipd_cv_cat_cnt.csv"))
write_csv(cv_cat_pct, here("output_data", "ipd_cv_cat_pct.csv"))
write_csv(cv_cat_scr, here("output_data", "ipd_cv_cat_scr.csv"))

# Is there any relationship between IPD mean classification error and CVs?
cv_class_err <- inner_join(cv_bygeo %>% select(GEOID, mean_cv) %>% st_set_geometry(NULL),
                           err_by_trct %>% select(GEOID, err) %>% st_set_geometry(NULL)) %>%
  drop_na(.) %>%
  filter(mean_cv < quantile(mean_cv, 0.95))
cor(cv_class_err$mean_cv, cv_class_err$err) # 0.1161285
ggplot(cv_class_err, aes(x = mean_cv, y = err)) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_point(color = "#45055B") +
  geom_smooth(method = "lm", color = "gray", fill = "gray") +
  labs(title = "Relationship between data reliability\nand classification error",
       x = "Mean CV of census tracts (%)", y = "Likelihood of classification error")
ggsave(here("figs", "ipd_cv_class_errors.png"), width = 4.5, height = 4.5, units = "in", dpi = 400)
# What I'm getting from this is that perhaps the classification method we're using to create the IPD scores
# isn't ideal.

# Would aggregating geographies improve the classification error?
# Or is the only real way to improve classification error to scrap the IPD binning procedure?


