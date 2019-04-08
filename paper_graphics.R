library(here)
library(tidyverse)
library(sf)
library(gridExtra)
library(extrafont)
library(viridis)
loadfonts(device = "win")

a <- st_read(here("dl_geo", "a_cty.shp"))
b <- st_read(here("dl_geo", "a_puma.shp"))
c <- st_read(here("dl_geo", "a_tad.shp"))
d <- st_read(here("dl_geo", "a_trct.shp"))
e <- st_read(here("dl_geo", "a_taz.shp"))

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
gridExtra::grid.arrange(a_geo, b_geo, c_geo, d_geo, e_geo, nrow = 2, ncol = 3)
dev.off()

# TAZ vs block group just for Philadelphia
library(tigris)
options(tigris_class = "sf")

e_phila <- e %>% filter(str_sub(GEOID, 1, 5) == "42101")

f_phila <- block_groups(state = 42, county = 101)

e_geo_v2 <- ggplot(e_phila) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(color = "gray", fill = "gainsboro") +
  coord_sf(datum = NA) +
  ggtitle("TAZ")
f_geo <- ggplot(f_phila) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(color = "gray", fill = "gainsboro") +
  coord_sf(datum = NA) +
  ggtitle("Block Group")

png(here("figs", "geo_taz_bg.png"), width = 7, height = 3, units = "in", res = 400)
gridExtra::grid.arrange(e_geo_v2, f_geo, ncol = 2)
dev.off()

# Example: bicycle commuters to Census Tract 4.02
bike <- st_read(here("dl_geo", "A302103.shp")) %>%
  rename(est = F24, moe = F25) %>%
  mutate(GEOID = str_sub(res_id, 8, -1)) %>%
  filter(str_sub(GEOID, 1, 5) == "42101") %>%
  select(GEOID, est, moe) %>%
  st_set_geometry(NULL)
bike <- inner_join(d, bike)

png(here("figs", "bikers.png"), width = 4.5, height = 4.5, units = "in", res = 400)
ggplot(bike) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(aes(fill = est), color = NA) +
  scale_fill_viridis("Estimate") +
  coord_sf(datum = NA) +
  ggtitle("Estimated bicycle commuters by census tract")
dev.off()

# Density plots
bike_density <- bike %>%
  st_set_geometry(NULL) %>%
  filter(est > 30)

png(here("figs", "bikers_density.png"), width = 4.5, height = 4.5, units = "in", res = 400)
ggplot(data = data.frame(x = c(-100, 200)), aes(x)) +
  stat_function(fun = dnorm, n = 101, color = "#45055B", fill = "gainsboro",
                alpha = 0.5, geom = "area",
                args = list(mean = bike_density[1,]$est, sd = bike_density[1,]$moe / 1.645)) +
  stat_function(fun = dnorm, n = 101, color = "darkgray", fill = "gainsboro",
                alpha = 0.5, geom = "area",
                args = list(mean = bike_density[2,]$est, sd = bike_density[2,]$moe / 1.645)) +
  stat_function(fun = dnorm, n = 101, color = "#45055B", fill = "gainsboro",
                alpha = 0.5, geom = "area",
                args = list(mean = bike_density[3,]$est, sd = bike_density[3,]$moe / 1.645)) +
  labs(x = "Potential value of estimate",
       y = "Probability",
       title = "Potential number of bikers to Census Tract 4.02") +
  scale_y_continuous(breaks = NULL) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank())
dev.off()
