library(here)
library(tidyverse)
library(sf)
library(grid)
library(gridExtra)
library(extrafont)
library(tigris)
library(viridis)
options(tigris_class = "sf")
loadfonts(device = "win")

a <- st_read(here("dl_geo", "a_cty.shp")) %>%
  filter(str_sub(GEOID, 1, 5) == 42101)
a_mask <- st_union(a)
b <- st_read(here("dl_geo", "a_puma.shp")) %>%
  st_intersection(., a_mask)
bb <- st_read(here("dl_geo", "a_powpuma.shp")) %>%
  st_intersection(., a_mask)
c <- st_read(here("dl_geo", "a_tad.shp")) %>%
  st_intersection(., a_mask)
d <- st_read(here("dl_geo", "a_trct.shp")) %>%
  filter(str_sub(GEOID, 1, 5) == 42101) %>%
  mutate(destination = ifelse(GEOID == "42101000402", "Yes", "No"))
e <- st_read(here("dl_geo", "a_taz.shp")) %>%
  st_intersection(., a_mask)
h2o <- st_read(here("dl_geo", "h2o.shp")) %>%
  st_transform(., st_crs(a_mask)) %>%
  st_intersection(., a_mask)

# Plot Census Tract 4.02
ggplot(d) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gainsboro", "#45055B")) +
  geom_sf(aes(fill = destination), color = NA) +
  geom_sf(data = h2o, fill = "#669999", color = NA) +
  geom_sf(data = a, fill = NA, color = "gray") +
  coord_sf(datum = NA) +
  ggtitle("Location of Census Tract 4.02")
ggsave(here("figs", "destination.png"), width = 4.5, height = 4.5, units = "in", dpi = 400)

# Plot boundaries
a_geo <- ggplot(a) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(color = "gray", fill = "gainsboro") +
  coord_sf(datum = NA) +
  ggtitle("County")
b_geo <- ggplot(b) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(color = "gray", fill = "gainsboro") +
  coord_sf(datum = NA) +
  ggtitle("PUMA")
bb_geo <- ggplot(bb) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(color = "gray", fill = "gainsboro") +
  coord_sf(datum = NA) +
  ggtitle("POWPUMA")
c_geo <- ggplot(c) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(color = "gray", fill = "gainsboro") +
  coord_sf(datum = NA) +
  ggtitle("TAD")
d_geo <- ggplot(d) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(color = "gray", fill = "gainsboro") +
  coord_sf(datum = NA) +
  ggtitle("Tract")
e_geo <- ggplot(e) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(color = "gray", fill = "gainsboro") +
  coord_sf(datum = NA) +
  ggtitle("TAZ")

png(here("figs", "geo_examples.png"), width = 7, height = 4, units = "in", res = 400)
gridExtra::grid.arrange(a_geo, bb_geo, b_geo, c_geo, d_geo, e_geo, nrow = 2, ncol = 3)
dev.off()

# TAZ vs block group just for Philadelphia
f_phila <- block_groups(state = 42, county = 101)

f_geo <- ggplot(f_phila) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(color = "gray", fill = "gainsboro") +
  coord_sf(datum = NA) +
  ggtitle("Block Group")

png(here("figs", "geo_taz_bg.png"), width = 7, height = 3, units = "in", res = 400)
gridExtra::grid.arrange(e_geo, f_geo, ncol = 2)
dev.off()

# Example: bicycle commuters to Census Tract 4.02
bike <- st_read(here("dl_geo", "A302103.shp")) %>%
  rename(est = F24, moe = F25) %>%
  mutate(GEOID = str_sub(res_id, 8, -1)) %>%
  filter(str_sub(GEOID, 1, 5) == "42101") %>%
  select(GEOID, est, moe) %>%
  st_set_geometry(NULL)
bike <- inner_join(d, bike) %>%
  mutate(sub_est = ifelse(est >= 30, est, NA))

destination <- bike %>% filter(destination == "Yes")

ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(data = bike, aes(fill = sub_est), color = NA) +
  scale_fill_viridis("Estimate", na.value = "gainsboro", limits = c(0,50)) +
  labs(title = "Estimated bicycle commuters by census tract",
       subtitle = "Black tract is destination") +
  geom_sf(data = destination, fill = "black", color = NA) +
  geom_sf(data = h2o, fill = "#669999", color = NA) +
  geom_sf(data = a, fill = NA, color = "gray") +
  coord_sf(datum = NA)
ggsave(here("figs", "bikers.png"), width = 4.5, height = 4.5, units = "in", dpi = 400)

# Density plots
bike_density <- bike %>%
  st_set_geometry(NULL) %>%
  filter(est >= 30) %>%
  arrange(est, moe)

png(here("figs", "bikers_density.png"), width = 4.5, height = 4.5, units = "in", res = 400)
ggplot(data = data.frame(x = c(-100, 200)), aes(x)) +
  stat_function(fun = dnorm, n = 101, color = "gray", fill = "gainsboro",
                alpha = 0.5, geom = "area",
                args = list(mean = bike_density[1,]$est, sd = bike_density[1,]$moe / 1.645)) +
  stat_function(fun = dnorm, n = 101, color = "gray", fill = "gainsboro",
                alpha = 0.5, geom = "area",
                args = list(mean = bike_density[2,]$est, sd = bike_density[2,]$moe / 1.645)) +
  stat_function(fun = dnorm, n = 101, color = "gray", fill = "gainsboro",
                alpha = 0.5, geom = "area",
                args = list(mean = bike_density[3,]$est, sd = bike_density[3,]$moe / 1.645)) +
  stat_function(fun = dnorm, n = 101, color = "gray", fill = "gainsboro",
                alpha = 0.5, geom = "area",
                args = list(mean = bike_density[4,]$est, sd = bike_density[4,]$moe / 1.645)) +
  stat_function(fun = dnorm, n = 101, color = "gray", fill = "gainsboro",
                alpha = 0.5, geom = "area",
                args = list(mean = bike_density[5,]$est, sd = bike_density[5,]$moe / 1.645)) +
  stat_function(fun = dnorm, n = 101, color = "gray", fill = "gainsboro",
                alpha = 0.5, geom = "area",
                args = list(mean = bike_density[6,]$est, sd = bike_density[6,]$moe / 1.645)) +
  stat_function(fun = dnorm, n = 101, color = "#45055B", fill = "gainsboro",
                alpha = 0.5, geom = "area",
                args = list(mean = bike_density[7,]$est, sd = bike_density[7,]$moe / 1.645)) +
  stat_function(fun = dnorm, n = 101, color = "#45055B", fill = "gainsboro",
                alpha = 0.5, geom = "area",
                args = list(mean = bike_density[8,]$est, sd = bike_density[8,]$moe / 1.645)) +
  geom_hline(yintercept = 0, color = "gray") +
  labs(x = "Potential value of estimate",
       y = "Probability",
       title = "Potential number of cyclists to Census Tract 4.02") +
  scale_y_continuous(breaks = NULL) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank())
dev.off()

# Plot the zero car hhs and zero car / 1 person hhs
# We'll use a 5-class standard deviation MODDED s.t. 0 estimates are bottom class
# f22 est f23 moe
zc_1 <- st_read(here("raw", "residence", "tract", "A112310.shp")) %>%
  filter(str_sub(geoid, 1, 5) == "42101") %>%
  rename(sum_est = F22, sum_moe = F23) %>%
  mutate(se = sum_moe / 1.645,
         cv = case_when(sum_est == 0 ~ 100,
                        sum_est != 0 ~ se / sum_est * 100)) %>%
  st_transform(., 26918)
zc_1_breaks <- c(-1, 1, 97.891, 231.411, 364.932, max(zc_1$sum_est))
zc_labels <- c("(-Inf, -1.5 SD]   ",
               "(-1.5 SD, -0.5 SD]   ",
               "(-0.5 SD, 0.5 SD]   ",
               "(0.5 SD, 1.5 SD]   ",
               "(1.5 SD, Inf)   ")
zc_1 <- zc_1 %>%
  mutate(classification = cut(sum_est, breaks = zc_1_breaks, labels = zc_labels),
         cv_cat = case_when(cv <= 15 ~ "0-15%     ",
                            cv > 15 & cv <= 30 ~ "15.1-30%     ",
                            cv > 30 & cv <= 60 ~ "30.1-60%     ",
                            cv > 60 ~ "60.1+%     ")) %>%
  st_transform(., 26918)

zc_1_est <- ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(data = zc_1, aes(fill = classification), color = NA) +
  scale_fill_viridis_d("Estimate") +
  geom_sf(data = h2o, fill = "#669999", color = NA) +
  geom_sf(data = a, fill = NA, color = "gray") +
  coord_sf(datum = NA)

zc_1_cv <- ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(data = zc_1, aes(fill = cv_cat), color = NA) +
  scale_fill_viridis_d("CV") +
  geom_sf(data = h2o, fill = "#669999", color = NA) +
  geom_sf(data = a, fill = NA, color = "gray") +
  coord_sf(datum = NA)

png(here("figs", "zc_1_est_cv.png"), width = 7, height = 3, units = "in", res = 400)
gridExtra::grid.arrange(zc_1_est, zc_1_cv, ncol = 2, widths = c(1.12, 1))
dev.off()

# Local context example 1: IPD scoring
field_fronts <- c("D", "EM", "F", "FB", "LEP", "LI", "OA", "RM", "Y")
field_finals <- c(paste(field_fronts, "PctEst", sep = "_"), paste(field_fronts, "PctMOE", sep = "_"))
ipd <- read_csv(here("dl_data", "ipd.csv")) %>%
  filter(D_PctEst != -99999) %>% # Drop NA observations
  select(!!!field_finals, IPD_Score) %>%
  mutate(D_CV = (D_PctMOE / 1.645) / D_PctEst * 100,
         EM_CV = (EM_PctMOE / 1.645) / EM_PctEst * 100,
         F_CV = (F_PctMOE / 1.645) / F_PctEst * 100,
         FB_CV = (FB_PctMOE / 1.645) / FB_PctEst * 100,
         LEP_CV = (LEP_PctMOE / 1.645) / LEP_PctEst * 100,
         LI_CV = (LI_PctMOE / 1.645) / LI_PctEst * 100,
         OA_CV = (OA_PctMOE / 1.645) / OA_PctEst * 100,
         RM_CV = (RM_PctMOE / 1.645) / RM_PctEst * 100,
         Y_CV = (Y_PctMOE / 1.645) / Y_PctEst * 100)
ipd[mapply(is.infinite, ipd)] <- 100 # Replace 0 estimate CVs with CVs of 100%

# Compute mean CV of observation
ipd <- ipd %>%
  mutate(Mean_CV = (D_CV + EM_CV + F_CV + FB_CV + LEP_CV + LI_CV + OA_CV + RM_CV + Y_CV) / 9)

cor(ipd$Mean_CV, ipd$IPD_Score) # -0.455
mod <- glm(IPD_Score ~ Mean_CV, family = "poisson", data = ipd)
summary(mod)
ggplot(ipd, aes(x = Mean_CV, y = IPD_Score)) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_vline(xintercept = 30, color = "gray", lwd = 1) +
  geom_point(color = "#45055B") +
  labs(title = "Relationship between IPD score\nand data reliability",
       x = "Mean CV of IPD population groups (%)", y = "IPD score")
ggsave(here("figs", "ipd.png"), width = 4.5, height = 4.5, units = "in", dpi = 400)
