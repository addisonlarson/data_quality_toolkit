library(here)
library(tidyverse)
library(tidycensus)
library(sf)
library(spdep)
# 2017 data
a <- get_acs(geography = "tract",
             state = "DC",
             variable = "B08101_025") %>% # Count of transit commuters
  mutate_at(vars(estimate), funs(na_if(., 0))) %>%
  mutate(se = moe / 1.645,
         cv = se / estimate * 100,
         year = 2017) %>%
  drop_na(.)
# 2015 data
b <- get_acs(geography = "tract",
             year = 2015,
             state = "DC",
             variable = "B08101_025") %>% # Count of transit commuters
  mutate_at(vars(estimate), funs(na_if(., 0))) %>%
  mutate(se = moe / 1.645,
         cv = se / estimate * 100,
         year = 2015) %>%
  drop_na(.)
# 2013 data
c <- get_acs(geography = "tract",
             year = 2013,
             state = "DC",
             variable = "B08101_025") %>% # Count of transit commuters
  mutate_at(vars(estimate), funs(na_if(., 0))) %>%
  mutate(se = moe / 1.645,
         cv = se / estimate * 100,
         year = 2013) %>%
  drop_na(.)
# 2011 data
d <- get_acs(geography = "tract",
             year = 2011,
             state = "DC",
             variable = "B08101_025") %>% # Count of transit commuters
  mutate_at(vars(estimate), funs(na_if(., 0))) %>%
  mutate(se = moe / 1.645,
         cv = se / estimate * 100,
         year = 2011) %>%
  drop_na(.)

## DEFINING AN OUTLIER
# Temporally
a_cutoff <- quantile(a$cv, 0.9)
b_cutoff <- quantile(b$cv, 0.9)
c_cutoff <- quantile(c$cv, 0.9)
d_cutoff <- quantile(d$cv, 0.9)
a_top <- filter(a, cv >= a_cutoff) %>% pull(GEOID)
b_top <- filter(b, cv >= b_cutoff) %>% pull(GEOID)
c_top <- filter(c, cv >= c_cutoff) %>% pull(GEOID)
d_top <- filter(d, cv >= d_cutoff) %>% pull(GEOID)
targets <- unique(c(a_top, b_top, c_top, d_top))

a_cutoff <- quantile(a$cv, 0.05)
b_cutoff <- quantile(b$cv, 0.05)
c_cutoff <- quantile(c$cv, 0.05)
d_cutoff <- quantile(d$cv, 0.05)
a_bottom <- filter(a, cv <= a_cutoff) %>% pull(GEOID)
b_bottom <- filter(b, cv <= b_cutoff) %>% pull(GEOID)
c_bottom <- filter(c, cv <= c_cutoff) %>% pull(GEOID)
d_bottom <- filter(d, cv <= d_cutoff) %>% pull(GEOID)
targets <- unique(c(targets, a_bottom, b_bottom, c_bottom, d_bottom)) %>%
  sample(., 20, replace = FALSE)

temporal <- bind_rows(a %>% filter(GEOID %in% targets),
                      b %>% filter(GEOID %in% targets)) %>%
  bind_rows(., c %>% filter(GEOID %in% targets)) %>%
  bind_rows(., d %>% filter(GEOID %in% targets))

ggplot(data = temporal, aes(x = year, y = cv)) +
  geom_line(aes(color = GEOID)) +
  ggtitle("CV over time for 20 census tracts") +
  theme(legend.position = "none")
# ggsave(here("figs", "temporal.png"), width = 7, height = 5, units = "in")

# Spatially
a <- get_acs(geography = "tract",
             state = "DC",
             variable = "B08101_025",
             geometry = TRUE) %>% # Count of transit commuters
  mutate_at(vars(estimate), funs(na_if(., 0))) %>%
  mutate(se = moe / 1.645,
         cv = se / estimate * 100,
         year = 2017) %>%
  drop_na(.)
a_spatial <- as(a, "Spatial")
a_nb <- poly2nb(a_spatial, queen = FALSE)

# First, for a single tract
flag <- vector()
test <- a_nb[[1]]
nb_properties <- a %>% slice(test) %>% pull(cv) %>% mean(.)
self_properties <- a %>% slice(1) %>% pull(cv) %>% mean(.)
diff <- (self_properties - nb_properties) /
  ((self_properties + nb_properties) / 2) * 100
if(diff >= 40){
  flag <- c(flag, 1)
} else {
  flag <- c(flag, 0)
}
