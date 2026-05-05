library(bbsBayes2)
library(tidyverse)
library(sf)

bcrs <- load_map("bbs")

rt_start <- data.frame(name = "route_start",
                       x = -78.081259,
                       y = 44.748734) %>%
  sf::st_as_sf(crs = 4326,
               coords = c("x","y")) %>%  # WGS 84
  sf::st_transform(crs = bbsBayes2::equal_area_crs)

bb <- st_bbox(rt_start)
zoom <- 300000 # number of meters on either side of the point to plot

map <- ggplot()+
  geom_sf(data = bcrs,
          aes(fill = strata_name),
          alpha = 0.1)+
  geom_sf_text(data = bcrs,
               aes(label = strata_name))+
  geom_sf(data = rt_start)+
  coord_sf(xlim = c(bb["xmin"]-zoom,bb["xmax"]+zoom),
           ylim = c(bb["ymin"]-zoom,bb["ymax"]+zoom))+
  theme(legend.position = "none")
plot(map)

