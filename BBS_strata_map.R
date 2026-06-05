### exploring the spatial patterns in annual BBS surveys



library(bbsBayes2)
library(tidyverse)
library(sf)

re_download <- FALSE # change to TRUE if you need to download the BBS database
if(re_download){
  fetch_bbs_data(include_unacceptable = TRUE, # this includes the run_type == 0, unacceptable surveys
                 force = TRUE) # this will overwrite any previous download, including any that excluded the unacceptable surveys
  bbs_unacceptable <- load_bbs_data()

  saveRDS(bbs_unacceptable,"bbs_unacceptable_full.rds")
}else{
  bbs_unacceptable <- readRDS("bbs_unacceptable_full.rds")

}



years <- 1970:2024


base_map <- load_map("bbs")

resort <- sample(unique(base_map$bcr),
                 size = length(unique(base_map$bcr)),replace = FALSE)


  base_map <- base_map |>
  mutate(bcrf = factor(bcr,levels = resort,ordered = TRUE))


base_mapc <- base_map |>
  summarise()

base_map2 <- load_map("prov_state") |>
  st_intersection(base_mapc) |>
  group_by(strata_name) |>
  summarise()


all_routes_ever_run <- bbs_unacceptable$routes %>%
  filter(year %in% years) %>%
  select(route_name,longitude,latitude) %>%
  distinct() %>%
  mutate(lat = latitude,
         long = longitude) %>%
  st_as_sf(coords = c("long","lat"), crs = 4326) %>%
  st_transform(crs = bbsBayes2::equal_area_crs)

bb <- st_bbox(all_routes_ever_run)

tplot <- ggplot()+
  geom_sf(data = base_map, aes(fill = bcr),
          alpha = 0.4,
          colour = grey(0.4))+
  geom_sf(data = all_routes_ever_run, colour = grey(0.5),
          alpha = 0.7,
          size = 0.15)+
  geom_sf(data = base_map2, fill = NA,
          colour = grey(0.1))+
  # geom_sf_text(data = base_map2,
  #              aes(label = strata_name),
  #              colour = grey(0.3),
  #              size = 1.8)+

  coord_sf(xlim = bb[c("xmin","xmax")],
           ylim = bb[c("ymin","ymax")])+
  scale_fill_viridis_d(begin = 0.2, end = 0.9,
                         direction = -1,
                       option = "turbo")+
  xlab("")+
  ylab("")+
  theme_bw()+
  theme(legend.position = "none")

png("strata_map.png",
    width = 9,
    height = 9,
    units = "in",
    res = 300)
print(tplot)
dev.off()

