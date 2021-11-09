library(tidyr)
library(dplyr)
library(leaflet)
library(sf)
library(raster)
library(mapview)


# salinity rasters
sal3  <-  raster::raster("salinity_03_03_480m_res_mask_v2.tif")
sal10  <-  raster::raster("salinity_10_10_480m_res_mask_v2.tif")

hsi_sal3  <- raster::raster("hsi_salinity_03_03_resample.tif")
hsi_sal10  <- raster::raster("hsi_salinity_10_10_resample.tif")

# convert salinity 3 raster to polygon
sal3_mask <-sal3 %>%
  terra::rast() %>%
  terra::as.polygons() %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  sf::st_union() %>%
  st_sf()

# convert salinity 10 raster to polygon
sal10_mask <-sal10 %>%
  terra::rast() %>%
  terra::as.polygons() %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  sf::st_union() %>%
  st_sf()

# mask HSI rasters to salinity area
hsi_sal3_mask <- mask(hsi_sal3, sal3_mask)
hsi_sal10_mask <- mask(hsi_sal10, sal10_mask)

raster::writeRaster(hsi_sal3_mask, "hsi_salinity_03_03_resample_mask.tif")
raster::writeRaster(hsi_sal10_mask, "hsi_salinity_10_10_resample_mask.tif")

plot(hsi_sal3_mask)
plot(hsi_sal10_mask)
# ====================================
# ---- Mask HSI to full mask area ----
# ====================================

hsi_sal3_mask <- raster::raster("hsi_salinity_03_03_resample_mask.tif")
hsi_sal10_mask<- raster::raster( "hsi_salinity_10_10_resample_mask.tif")

# full mask area
mask_shp <- readRDS("grid_mask_ldh_open_intermed.rds")

# mask HSI
hsi_sal3_full_mask <- raster::mask(hsi_sal3_mask, mask_shp)
hsi_sal10_full_mask <- raster::mask(hsi_sal10_mask, mask_shp)

raster::writeRaster(hsi_sal3_full_mask, "hsi_salinity_03_03_full_mask.tif")
raster::writeRaster(hsi_sal10_full_mask, "hsi_salinity_10_10_full_mask.tif")

# ====================================

mapview::mapview(hsi_sal3_full_mask) + hsi_sal10_full_mask
pal1 <- colorFactor("viridis", domain = cities1$Type)
pal2 <- colorFactor("Set1", domain = cities2$Type)
cities1
leaflet(cities1) %>%
  addTiles() %>%
  addCircles(data = cities1, lng = ~Long, lat = ~Lat, weight = 1, group="one",
             radius = ~sqrt(Pop) * 30, popup = ~City, color = ~pal1(Type), opacity = .9
  ) %>%
  addLegend(pal = pal1, values = ~Type, group  = "one", layerId = "one") %>%
  addCircles(data = cities2, lng = ~Long, lat = ~Lat, weight = 1, group = "two",
             radius = ~sqrt(Pop) * 30, popup = ~City, color = ~pal2(Type), opacity = .9

  ) %>%
  addLegend(pal = pal2, values = ~Type, data = cities2, group = "two", layerId = "two") %>%
  addLayersControl(
    baseGroups = c("one", "two"),
    options = layersControlOptions(collapsed = FALSE),
    position = "topleft"
  ) %>%
  htmlwidgets::onRender("
    function() {
      var map = this;
      var legends = map.controls._controlsById;
      function addActualLegend() {
         var sel = $('.leaflet-control-layers-base').find('input[type=\"radio\"]:checked').siblings('span').text().trim();
         $.each(map.controls._controlsById, (nm) => map.removeControl(map.controls.get(nm)));
         map.addControl(legends[sel]);
      }
      $('.leaflet-control-layers-base').on('click', addActualLegend);
      addActualLegend();
   }")

df = local({
  n = 300; x = rnorm(n); y = rnorm(n)
  z = sqrt(x^2 + y^2); z[sample(n, 10)] = NA
  z2 <- z ^ 2
  data.frame(x, y, z, z2)
})
df
pal1 = colorNumeric('OrRd', df$z)
pal2 = colorNumeric('OrRd', df$z2)

leaflet(df) %>%
  addTiles() %>%
  addCircleMarkers(~x, ~y, color = ~pal1(z), group='circles1') %>%
  addCircleMarkers(~x, ~y, color = ~pal2(z2), group='circles2') %>%
  addLegend(
    pal = pal1,
    values = ~z,
    group='circles1',
    layerId = "circles1",
    position='bottomleft'
    ) %>%
  addLegend(
    pal = pal2,
    values = ~z2,
    group='circles2',
    layerId = "circles2",
    position='bottomleft'
    ) %>%
  addLayersControl(overlayGroups  = c('circles1', 'circles2')) %>%
  hideGroup(c("circles2"))
  # htmlwidgets::onRender("
  #   function() {
  #     var map = this;
  #     var legends = map.controls._controlsById;
  #     function addActualLegend() {
  #        var sel = $('.leaflet-control-layers-base').find('input[type=\"radio\"]:checked').siblings('span').text().trim();
  #        $.each(map.controls._controlsById, (nm) => map.removeControl(map.controls.get(nm)));
  #        map.addControl(legends[sel]);
  #     }
  #     $('.leaflet-control-layers-base').on('click', addActualLegend);
  #     addActualLegend();
  #  }")




