library(rasterVis)

path <- "C:/Users/angus/OneDrive/Desktop/lynker/CPRA/data/"

crs <- CRS('+init=EPSG:26915')

ext <- extent(c(405220, 909700, 3199570, 3416530))

# salinity rasters
waterways  <- raster(paste0(path, "waterways/navig_waterways_buffer_raster.tif"))
sal3       <- raster(paste0(path, "salinity/salinity_03_03_480m_res_mask.tif"))
sal10      <- raster(paste0(path, "salinity/salinity_10_10_480m_res_mask.tif"))
library(mapview)

mapview(fetch2) + fetch_rc
crs(fetch3) <- crs
crs(fetch2)
ext <- extent(fetch2)
fetch_rc3 <- raster::resample(fetch_rc2, fetch2)
mapview(fetch_rc) + fetch_rc2
sf::write_sf(road_buffer, "road_buffers.shp")

raster::cellStats(sal3, "min")

fetch_rc
fetch2
