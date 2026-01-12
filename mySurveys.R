# my surveys
#
#



my_surveys <- bbs_unacceptable$routes %>%
  filter(route_name %in% c("EGANVILLE","ASHDAD"),
         obs_n == 1151102)



