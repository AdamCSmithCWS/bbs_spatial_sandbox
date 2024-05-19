library(sf)
library(tidyverse)
library(ebirdst)
library(terra)





# BBS Routes --------------------------------------------------------------


comb <- readRDS("data/United_States_selected_BBS_route_paths.rds")

miss_data <- comb %>%
  filter(is.na(first_year_surveyed))

miss_spatial <- us_rts_wdata %>%
  filter(!route_name %in% usgs_bbs2$RTENAME)

vie <- ggplot()+
  geom_sf(data = comb,
          aes(colour = first_year_surveyed))

vie

tmp <- comb %>%
  filter(prov_state %in% c("OR"),
         #RTENAME %in% c("DAYTON"),
         length_km < 43, length_km > 42)


vie <- ggplot()+
  geom_sf(data = tmp,
          aes(colour = length_km))+
  geom_sf_text(data = tmp,
               aes(label = RTENAME),
               size = 2)

vie




tmpb <- tmp %>%
  st_buffer(., dist = 1000)

tmpbb <- tmp %>%
  st_buffer(., dist = 10000) %>%
  st_bbox()

vie <- ggplot()+
  geom_sf(data = tmpb,fill = NA)+
  geom_sf(data = tmp)+
  coord_sf(xlim = tmpbb[c("xmin","xmax")],
           ylim = tmpbb[c("ymin","ymax")])+
  theme_void()


pdf("route_buf1.pdf",
    width = 24,
    height = 24)
print(vie)
dev.off()




# eBird relative abundance ------------------------------------------------

species <- "Rufous Hummingbird"
species_ebird <- ebirdst::get_species(species)


down <- try(ebirdst::ebirdst_download_status(species_ebird,
                                             download_ranges = FALSE,
                                             download_abundance = FALSE,
                                             download_occurrence = TRUE),
            silent = TRUE)


abd_seasonal_occurence <- ebirdst::load_raster(species = species,
                                           resolution = "3km",
                                           period = "seasonal",
                                           product = "occurrence")  #27km low resolution


breed_occ <- abd_seasonal_occurence[["breeding"]]


region_boundary <- rnaturalearth::ne_states(country = c("United States of America","Canada"),
                                            returnclass = "sf") |>
  filter(name %in% c( "Oregon","Washington",
                      "British Columbia"))

# project boundary to match raster data
region_boundary_proj <- st_transform(region_boundary, st_crs(breed_occ))

# crop and mask to boundary of wyoming
breed_occ <- crop(breed_occ, region_boundary_proj) |>
  mask(region_boundary_proj)


# define projection
crs_laea <- paste0("+proj=laea +lat_0=", 42.1,
                   " +lon_0=", -120)

tmpb <- tmpb %>%
  st_transform(.,crs = st_crs(crs_laea))

tmp <- tmp %>%
  st_transform(.,crs = st_crs(crs_laea))

tmpbb <- tmp %>%
  st_buffer(., dist = 10000) %>%
  st_bbox()

# transform to the custom projection using nearest neighbor resampling
breed_occ_pr <- project(breed_occ,
                        crs_laea,
                             method = "near")


breed_occ_df <- as.data.frame(breed_occ_pr, xy = TRUE )

vie <- ggplot()+
  geom_tile(data = breed_occ_df,
              aes(x = x, y = y,
                  fill = breeding))+
  scale_fill_viridis_c()+
  theme_void()
  # geom_sf(data = tmpb)+
  # geom_sf(data = tmp)+
  # coord_sf(xlim = tmpbb[c("xmin","xmax")],
  #          ylim = tmpbb[c("ymin","ymax")])

vie


pdf("rufu_hab_cartoon.pdf",
    height = 22,width = 17)
print(vie)
dev.off()




