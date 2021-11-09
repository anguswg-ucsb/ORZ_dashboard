library(tidyverse)
library(raster)

# Water level variability
wlvl_2021       <- raster::raster("wlvl_variability_2021_threshold.tif")
wlvl_clamp      <- raster::raster("wlvl_variability_2021_05_threshold.tif")

h1 <- hist(wlvl_clamp, breaks = 50, main = "Water level variability\n(0.01 m bins)", xlab = "Water level variability (meters)")
h2 <- hist(wlvl_clamp, breaks = 25, main = "Water level variability\n(0.02 m bins)", xlab = "Water level variability (meters)")
library(patchwork)
30/0.5

0.5/30

wlvl2 <- raster::clamp(wlvl_clamp, upper = 0.25)

wlvl2 <- setValues(
  wlvl_2021, ifelse(getValues(wlvl_2021) > 0.25, NA, getValues(wlvl_2021))
)

plot(wlvl2)

wlvl_cols         <- data.frame(numeric_cols = 0:1)
wlvl_pal          <- colorNumeric(turbo(n = 20), domain = values(wlvl2), na.color = NA, reverse = F)
# wlvl_pal          <- colorNumeric("magma", domain = values(wlvl2), na.color = NA, reverse = F)
# ==================================
# ----------- LEAFLET MAP ----------
# ==================================
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
  addRasterImage(
    wlvl2,
    colors    = wlvl_pal,
    opacity   = 0.8,
    group     = "Water level variability") %>%
  addLegend(
    pal       = wlvl_pal,
    title     = "Water level variability", position  = "bottomleft",
    group     = "Water level variability", layerId   = "Water level variability", values = values(wlvl2))
