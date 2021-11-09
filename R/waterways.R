library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(mapview)
library(progress)

path <- "C:/Users/angus/OneDrive/Desktop/lynker/CPRA/data/"

crs <- CRS('+init=EPSG:26915')

ext <- extent(c(405220, 909700, 3199570, 3416530))
extent(tmp_ww)
tmp_ww <- resample(waterways, sal_agg)
# --- extent ---
# xmin       : 404500
# xmax       : 1025140
# ymin       : 3186850
# ymax       : 3435010

# --- dimensions ---
# nrows:  517
# ncol:   1293,
# ncells: 668481

waterways  <- raster(paste0(path, "waterways/navig_waterways_buffer_raster.tif"))

waterways_shp  <- readRDS(paste0(path, "waterways/navig_waterways_buffer.rds"))
split(df, sample(1:N, nrow(df), replace=T))
mapview(waterways)
num_groups = 10

iris %>%
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)
plot(sal_agg)
plot(tmp_ww, col = "red", add = T)
plot(waterways, col = "yellow", add = T)
wter <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPRA/data/fetch/points/water_points.rds")
water_mask <- raster(paste0(path, "land_type/raster/water_masked_480m_res.tif"))

# convert water mask to points, add lat/lon columns
water_pts <- water_mask %>%
  raster::rasterToPoints() %>%
  data.frame() %>%
  st_as_sf(
    coords =  c("x", "y"),
    crs    =  CRS('+init=EPSG:26915')
  ) %>%
  st_transform(26915) %>%
  mutate(
    lng    =  st_coordinates(.)[,1],
    lat    =  st_coordinates(.)[,2]
  )

# find points NOT intersecting w/ land polygon
pt_intersect <- sapply(st_intersects(water_pts, land_sf),function(x){length(x)==0})


# filter water points for those not intersecting polygon
pts_int <- water_pts[pt_intersect,]

pts_split <- pts_int %>%
  mutate(id = 1:84580) %>%
  group_by(id) %>%
  group_split()

beep()
tmp <- pts_split[1:10]
pts_lst <- lapply(pts_split, FUN = function(x)
  output <- x %>%
    as("Spatial") %>%
    as("SpatialPoints") %>%
    spTransform(CRS('+init=EPSG:26915'))
)

pb <- progress_bar$new(
  format = "(:spin) [:bar] :percent [Elapsed: :elapsedfull || Est time remaining: :eta || current: :current]",
  total  = nrow(pts_int), complete = "=",  incomplete = "-",  current = ">",  clear = FALSE,  width = 200)

library(pbapply)
fetch_lst <- list()
rm(r, pt, fetch_pts, tmp, fetch_calc, fetch_df)
group_split()
pts_split <- pts_int %>%
  # as("Spatial") %>%
  # as("SpatialPoints") %>%
  # spTransform(CRS('+init=EPSG:26915')) %>%

# group_by(lng, lat) %>%
mutate(id = 1:84580) %>%
  group_by(id) %>%
  group_split()
beep()
tmp <- pts_split[1:15]
tmp[[1]]
poly_sp <- land_sf %>%
  as("Spatial") %>%
  as("SpatialPolygonsDataFrame") %>%
  spTransform(CRS('+init=EPSG:26915'))
g <- tmp[[1]]$
g$id
pts_split <- pts_int %>%
  mutate(id = 1:84580) %>%
  group_by(id) %>%
  group_split()
pts_lst <- pbapply::pblapply(tmp, FUN = function(x)

  output <- x %>%
    as("Spatial") %>%
    as("SpatialPoints") %>%
    spTransform(CRS('+init=EPSG:26915')) %>%
    fetch_len(
      # p           = x,
      bearings    = c(0, 45, 90, 135, 180, 225, 270, 315),
      shoreline   = poly_sp,
      dmax        = 20000,
      spread      = c(1,0,1),
      projected   = TRUE
    ) %>%
    data.frame() %>%
    tibble::rownames_to_column() %>%
    # pivot_wider(names_from = "rowname", values_from = ".") %>%
    setNames(c("bearing", "fetch")) %>%
    mutate(
      id  = x$id,
      lng = x$lng,
      lat = x$lat
    )
)
hy <- bind_rows(pts_lst)
plot(cpra_projects)
plot(restoration_proj, add = T)
library(waver)
fetch_output <- pblapply(pts_lst, FUN = function(x)
  fetch_calc <- fetch_len(
    p           = x,
    bearings    = c(0, 45, 90, 135, 180, 225, 270, 315),
    shoreline   = poly_sp,
    dmax        = 20000,
    spread      = c(1,0,1),
    projected   = TRUE
  )
)

library(progress)
# progress bar used in For loop
pb <- progress_bar$new(
  format = "(:spin) [:bar] :percent [Elapsed: :elapsedfull || Est time remaining: :eta || current: :current]",
  total  = nrow(tmp), complete = "=",  incomplete = "-",  current = ">",  clear = FALSE,  width = 200)

fetch_lst <- list()
# loop over each water point and calculate the fetch
system.time(
  for (i in 1:nrow(tmp)) {
    pb$tick()

    pt <- tmp[i,]

    fetch_pts <- pt %>%
      as("Spatial") %>%
      as("SpatialPoints") %>%
      spTransform(CRS('+init=EPSG:26915'))

    fetch_calc <- fetch_len(
      p           = fetch_pts,
      bearings    = c(0, 45, 90, 135, 180, 225, 270, 315),
      shoreline   = poly_sp,
      dmax        = 20000,
      spread      = c(1,0,1),
      projected   = TRUE
    )

    fetch_df <- fetch_calc %>%
      data.frame() %>%
      tibble::rownames_to_column() %>%
      # pivot_wider(names_from = "rowname", values_from = ".") %>%
      setNames(c("bearing", "fetch")) %>%
      mutate(
        id  = i,
        lng = pt$lng,
        lat = pt$lat
      )

    # add fetch df to list
    fetch_lst[[i]] <- fetch_df

  }
)
install.packages("pbapply")

