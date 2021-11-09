library(tidyr)
library(dplyr)
library(leaflet)
library(sf)
library(raster)
library(mapview)
path <- "C:/Users/angus/OneDrive/Desktop/lynker/CPRA/data/"

roads_5 <- sf::read_sf(paste0(path, "roads/road_buffers_5.shp")) %>%
  st_cast("MULTILINESTRING") %>%
  arrange(buffer) %>%
  st_transform(26915) %>%
  st_transform(4326) %>%
  mutate(
    buffer_dist = case_when(
      buffer == 2   ~ "2km road buffer",
      buffer == 5   ~ "5km road buffer",
      buffer == 10  ~ "10km road buffer",
      buffer == 20  ~ "20km road buffer")
  )
saveRDS(roads_5, "road_buffer2.rds")


plot(road_buffer$geometry[1])
roads2km_sf <- sf::read_sf(path, "roads/roads_buff2km.shp") %>%
  dplyr::select(fid)

roads5km_sf <- sf::read_sf(path, "roads/roads_buff5km.shp") %>%
  dplyr::select(fid)

roads10km_sf <- sf::read_sf(path, "roads/roads_buff10km.shp") %>%
  dplyr::select(fid)

roads20km_sf <- sf::read_sf(path, "roads/roads_buff20km.shp") %>%
  dplyr::select(fid)

# buffer_pt <- data.frame(x = -92.59, y = 30.73) %>%
#   st_as_sf(coords = c("x", "y"), crs = 4326) %>%
#   st_buffer(30000) %>%
#   st_bbox() %>%
#   st_as_sfc() %>%
#   st_sf()

xmin  <- -94.810511
ymin  <-  30.45855
xmax = -89.4
ymax <- 32.15
xmin2  <- -94.810511
ymin2  <-  30.15
xmax2  <-  -91.88
ymax2  <- 31.57

# bounding boxes to crop out portion of road buffer polygons for memory purposes
crop_bb <- st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin), crs = st_crs(4326)) %>%
  st_as_sfc() %>%
  st_sf() %>%
  st_transform(crs = crs(roads2km_sf))

crop_bb2 <- st_bbox(c(xmin = xmin2, xmax = xmax2, ymax = ymax2, ymin = ymin2), crs = st_crs(4326)) %>%
  st_as_sfc() %>%
  st_sf() %>%
  st_transform(crs = crs(roads2km_sf))

# crop 2km buffer
road_crop2km <- st_difference(roads2km_sf, crop_bb)
road_crop2km <- st_difference(road_crop2km, crop_bb2)
road_crop2km <- road_crop2km %>%
  mutate(buffer = 2)

# crop 5km buffer
road_crop5km <- st_difference(roads5km_sf, crop_bb)
road_crop5km <- st_difference(road_crop5km, crop_bb2)
road_crop5km <- road_crop5km %>%
  mutate(buffer = 5)


# crop 10km buffer
road_crop10km <- st_difference(roads10km_sf, crop_bb)
road_crop10km <- st_difference(road_crop10km, crop_bb2)
road_crop10km <- road_crop10km %>%
  mutate(buffer = 10)
# crop 20km buffer
road_crop20km <- st_difference(roads20km_sf, crop_bb)
road_crop20km <- st_difference(road_crop20km, crop_bb2)
road_crop20km <- road_crop20km %>%
  mutate(buffer = 20)

st_sfc(road_crop2km$geometry, road_crop5km$geometry, road_crop10km$geometry)
road_df <- bind_rows(road_crop2km, road_crop5km, road_crop10km, road_crop20km)
saveRDS(road_df, "road_buffer.rds")
class(road_df)
road_df <- data.frame(road_buffer2k =  road_crop2km, road_buffer5k = road_crop5km)
mapview(road_crop10km) + road_crop20km


# -------------------------
# ------ Raster data ------
# -------------------------

road_stk <- road_stk %>% setNames(c(
  "road_buffer_2km",
  "road_buffer_5km",
  "road_buffer_10km",
  "road_buffer_20km")
)





















