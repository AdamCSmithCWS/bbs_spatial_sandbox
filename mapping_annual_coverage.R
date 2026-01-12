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


years <- 2014:2024

base_map <- load_map("bbs") %>%
  filter(country == "Canada")

surveys_by_year <- bbs_unacceptable$routes %>%
  filter(country == "CA",
         year %in% years) %>%
  mutate(lat = latitude,
         long = longitude,
         acceptable = ifelse(run_type == 1,TRUE,FALSE)) %>%
  st_as_sf(coords = c("long","lat"), crs = 4326) %>%
  st_transform(crs = bbsBayes2::equal_area_crs)


pdf("annual_BBS_surveys_Canada_map.pdf",
    width = 11,height = 8.5)

for(y in years){
  tmp <- surveys_by_year %>%
    filter(year == y)

  tplot <- ggplot()+
    geom_sf(data = base_map, colour = grey(0.8))+
    geom_sf(data = tmp,
            aes(colour = acceptable),
            alpha = 0.7)+
    scale_colour_viridis_d(begin = 0.2, end = 0.9,
                           direction = -1)+
    labs(title = y)+
    theme_bw()

print(tplot)

  }

dev.off()





pdf("2016-2019_vs_2021-2024_BBS_surveys_Canada_map.pdf",
    width = 11,height = 8.5)


  tmp <- surveys_by_year %>%
    filter(year %in% c(2016:2019))

  tplot <- ggplot()+
    geom_sf(data = base_map, colour = grey(0.8))+
    geom_sf(data = tmp,
            aes(colour = acceptable),
            alpha = 0.7)+
    scale_colour_viridis_d(begin = 0.2, end = 0.9,
                           direction = -1)+
    labs(title = "2016-2019")+
    theme_bw()

  print(tplot)


  tmp <- surveys_by_year %>%
    filter(year %in% c(2021:2024))

  tplot <- ggplot()+
    geom_sf(data = base_map, colour = grey(0.8))+
    geom_sf(data = tmp,
            aes(colour = acceptable),
            alpha = 0.7)+
    scale_colour_viridis_d(begin = 0.2, end = 0.9,
                           direction = -1)+
    labs(title = "2021-2024")+
    theme_bw()

  print(tplot)



dev.off()




