
# Code for making reproducible, small example isoscape data.

my_extent <- ext(-110, -95, 35, 45)

prepRast <- function(
  isoscape_file_location = "~/isocat/setup_data/66100_caitjcampbell_Annual_NoAm_Map_H_1980_2010/66100_caitjcampbell_Annual_NoAm_Map_H_1980_2010",
  extension,
  my_extent = ext(-110, -95, 35, 45)
  ){

  r1 <- rast( file.path( isoscape_file_location, extension ) )
  # r2 <- aggregate(r1, fact = 2)
  r2 <- r1

  as.data.frame(
    crop(
      r2,
      my_extent ),
    xy = TRUE)
}

# growing season (main example)
isoscape    <- prepRast(isoscape_file_location = "~/isocat/setup_data/GlobalPrecipGS", extension = "d2h_GS.tif", my_extent = my_extent)
isoscape_sd <- prepRast(isoscape_file_location = "~/isocat/setup_data/GlobalPrecipGS", extension = "d2h_se_GS.tif", my_extent = my_extent)

# monthly
iso_monthly <- lapply(7:9, function(x) {
  rast(file.path(
    "~/isocat/setup_data/GlobalPrecip",
    paste0("d2h_0", x, ".tif")
  ))
}) %>% rast %>% crop(., my_extent) %>%
  as.data.frame(xy = T)

iso_monthly_se <- lapply(7:9, function(x) {
  rast(file.path(
    "~/isocat/setup_data/GlobalPrecip",
    paste0("d2h_se_0", x, ".tif")
  ))
}) %>% rast %>% crop(., my_extent) %>%
  as.data.frame(xy = T)


# save( isoscape, isoscape_sd, file = "~/isocat/data/isoscape.RData" )
usethis::use_data(isoscape, isoscape_sd, iso_monthly, iso_monthly_se, overwrite = TRUE)
