library(here)
library(tidyverse)
library(sf)
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
c <- st_read(here("dl_geo", "a_tad.shp")) %>%
  st_intersection(., a_mask)
d <- st_read(here("dl_geo", "a_trct.shp")) %>%
  filter(str_sub(GEOID, 1, 5) == 42101) %>%
  mutate(destination = ifelse(GEOID == "42101000402", "Yes", "No"))
e <- st_read(here("dl_geo", "a_taz.shp")) %>%
  st_intersection(., a_mask)

# Plot Census Tract 4.02
ggplot(d) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gainsboro", "#45055B")) +
  geom_sf(aes(fill = destination), color = NA) +
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
  geom_sf(data = a, fill = NA, color = "gray") +
  coord_sf(datum = NA) +
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
  labs(x = "Potential value of estimate",
       y = "Probability",
       title = "Potential number of cyclists to Census Tract 4.02") +
  scale_y_continuous(breaks = NULL) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank())
dev.off()
