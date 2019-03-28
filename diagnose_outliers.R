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
a_nb <- poly2nb(a_spatial, queen = TRUE)
a_lag <- nblag(a_nb, 2)
a_lag_nb <- nblag_cumul(a_lag)

# For happy maps
disp <- spTransform(a_spatial, CRS("+proj=longlat +datum=WGS84"))
coords <- coordinates(disp)
disp_nb <- poly2nb(disp, queen = TRUE)
disp_lag_nb <- nblag(disp_nb, 2)
# png(here("figs", "first_order.png"), width = 5, height = 7, units = "in", res = 400)
plot(disp, col = "gainsboro", main = "First-order links")
plot(disp_lag_nb[[1]], test_coords, col = "darkolivegreen", add = TRUE)
# dev.off()
# png(here("figs", "second_order.png"), width = 5, height = 7, units = "in", res = 400)
plot(disp, col = "gainsboro", main = "Second-order links")
plot(disp_lag_nb[[2]], test_coords, col = "darkolivegreen", add = TRUE)
# dev.off()
# High percentage difference from neighbors
flag <- vector()
for(j in 1:length(a_nb)){
  sub <- a_nb[[j]]
  nb_properties <- a %>% slice(sub) %>% pull(cv) %>% mean(.)
  self_properties <- a %>% slice(j) %>% pull(cv) %>% mean(.)
  diff <- (self_properties - nb_properties) /
    ((self_properties + nb_properties) / 2) * 100
  if(diff >= 40){
    flag <- c(flag, 1)
  } else {
    flag <- c(flag, 0)
  }
}
a$sp_outlier <- as.factor(flag)
png(here("figs", "sp_outlier.png"), width = 5, height = 7, units = "in", res = 400)
plot(a["sp_outlier"], main = "Outliers by tract")
dev.off()

# Local spatial autocorrelation
link <- localmoran(a_spatial$cv, nb2listw(a_nb), alternative = "two.tailed")
lag_link <- localmoran(a_spatial$cv, nb2listw(a_lag_nb), alternative = "two.tailed")
a$moran1 <- link[,5]
a$moran2 <- lag_link[,5]
png(here("figs", "cv.png"), width = 5, height = 7, units = "in", res = 400)
plot(a["cv"], main = "CV by tract")
dev.off()
png(here("figs", "first_moran.png"), width = 5, height = 7, units = "in", res = 400)
plot(a["moran1"], main = "First-order clusters by tract")
dev.off()
png(here("figs", "second_moran.png"), width = 5, height = 7, units = "in", res = 400)
plot(a["moran2"], main = "First- and second-order clusters by tract")
dev.off()
