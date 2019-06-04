
# Code for making reproducible, small example isoscape data.

prepRast <- function(
  isoscape_file_location = "~/isocat/setup_data/66100_caitjcampbell_Annual_NoAm_Map_H_1980_2010/66100_caitjcampbell_Annual_NoAm_Map_H_1980_2010",
  extension,
  my_extent = extent(-120, -90, 35, 50)
  ){
  as.data.frame(
    crop(
      raster( file.path( isoscape_file_location, extension ) ),
      my_extent ),
    xy = TRUE)
}

isoscape    <- prepRast(extension = "predkrig.tiff")
isoscape_sd <- prepRast(extension = "stdkrig.tiff")

# save( isoscape, isoscape_sd, file = "~/isocat/data/isoscape.RData" )
usethis::use_data(isoscape, isoscape_sd, overwrite= TRUE)
