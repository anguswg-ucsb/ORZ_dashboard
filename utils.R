# --- Shiny utils ---
basemap <- function(
                land_sf, road_buffer, cpra_proj_poly, cup,
                ldh, aoc, mask_open, mask_interm, sal3, sal10, depth1,
                wlvl_clamp, hsi_sal3, hsi_sal10, hsi_sal3_mask, hsi_sal10_mask,
                fetch_cat7, waterways, sowb, pts = NULL
                ) {

  # ==================================
  # ---- COLOR PALETTES & LABELS -----
  # ==================================

  # Fetch color palette + labels
  fetch_categories <- length(unique(raster::values(fetch_cat7))) -1
  fetch_cols       <- data.frame(fetch_cols = c(1, 2, 3, 4, 5, 10, 20))  # fetch_cols <- data.frame(fetch_cols = 1:fetch_categories)
  turbo_pal        <- viridisLite::turbo(n = fetch_categories, direction = -1)
  fetch_fact_df    <- fetch_cols %>% mutate(fetch_cols = factor(fetch_cols))
  fetch_fact       <- colorFactor(turbo_pal,   domain = fetch_fact_df$fetch_cols)
  fetch_labels     <- c("1km", "2m", "3km", "4km", "5km", "10km", "20km")
  # fetch_pal      <- colorNumeric(palette = turbo_pal,   domain = fetch_cols$fetch_cols, reverse = TRUE, na.color = NA)
  # pal = colorNumeric("Blues", reverse= F, na.color = "#00000000",      domain = unique(values(fetch_rc)))

  # Salinity color palette + labels
  sal_cols         <- data.frame(numeric_cols = 1:36)
  sal_pal          <- colorNumeric('viridis', domain = sal_cols$numeric_cols, na.color = NA, reverse = TRUE)

  # Water level variability palatte + labels
  wlvl_pal          <- colorNumeric(turbo(n = 20), domain = values(wlvl_clamp), na.color = NA, reverse = F)
  # wlvl_pal          <- colorNumeric(turbo(n = 20), domain = wlvl_cols$numeric_cols, na.color = NA, reverse = F)
  # wlvl_cols         <- data.frame(numeric_cols = c(0.05, 0.10, 0.15, 0.20, 0.25))

  # Depth factors color palette + labels
  depth_lvl          <- data.frame(numeric_cols = 1:3)
  depth_labels       <- c("Too shallow", "Shallow water", "Deep water")
  depth_fact_pal     <- colorFactor(brewer.pal(9, "Spectral"), domain = depth_labels, reverse = T)
  # depth_fact_pal   <- colorNumeric(mako(n = 3), domain = values(depth1), reverse = T)
  # depth_fact_pal   <- colorFactor(brewer.pal(9, "Spectral"), domain = depth_labels, reverse = T)
  # depth_cols         <- data.frame(numeric_cols = 0:6)
  # depth_pal          <- colorNumeric(viridisLite::turbo(n = 6), domain = depth_cols$numeric_cols, reverse = F)


  hsi_cols         <- data.frame(numeric_cols = 0:1)
  hsi_pal          <- colorNumeric('magma', domain =values(hsi_sal3), na.color = NA, reverse = F)
  # hsi_pal          <- colorNumeric('YlGnBu', domain = values(hsi_sal3), na.color = NA, reverse = T)

  # LDH factor color palette
  factpal          <- colorFactor(c("red", "yellow", "green"),   domain = ldh$Status)

  # sowb legend color palette + label
  sowb_label       <- "State owned water bottoms"
  sowb_pal         <- colorFactor(c("#00bfb2"),   domain = sowb_label)

  # Coastal use permits legend color palette + label
  cup_label        <- "Coastal use permits"
  cup_pal          <- colorFactor(c("#EAC435"),   domain = cup_label)

  # CPRA restoration proj legend color palette + label
  cpra_proj_label  <- "CPRA projects buffer"
  cpra_proj_pal    <- colorFactor(c("hotpink"),   domain = cpra_proj_label)

  # Waterways legend color palette + label
  waterways_label  <- "Waterways"
  waterways_pal    <- colorFactor(c("dodgerblue"), domain = waterways_label)

  # Mask open legend color palette + label
  mask_open_label          <- "Full mask (LDH open)"
  mask_open_pal            <- colorFactor(c("grey"),   domain = mask_open_label)

  # Mask open + interm. legend color palette + label
  mask_interm_label        <- "Full mask (LDH open & intermed.)"
  mask_interm_pal          <- colorFactor(c("grey"),   domain = mask_interm_label)

  # AOC active/not active factor color palette
  aoc_pal          <- colorFactor(c("darkorange"),   domain = aoc$label)

  # ==================================
  # ----------- LEAFLET MAP ----------
  # ==================================
  leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
    addProviderTiles(providers$Esri.OceanBasemap, group = "Topographic") %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat. Geo. Topographic") %>%
    addScaleBar("bottomleft") %>%
    addMeasure(position = "bottomright", primaryLengthUnit = "feet",
               primaryAreaUnit = "sqmiles", activeColor = "red", completedColor = "green") %>%
    leafem::addMouseCoordinates() %>%
    setView(lng = -91.47, lat = 30.295, zoom = 8) %>%
    addPolygons(
        data             = ldh,
        fillColor        = ~factpal(Status), fillOpacity = 0.2, color = ~factpal(Status),
        highlightOptions = highlightOptions(opacity = 1, weight = 6, bringToFront = TRUE), weight = 3, opacity = 1,
        label            = ~Status, group = "LDH") %>%
    addLegend(
        pal       = factpal,
        position  = "topleft",
        title     ='LDH Status',
        # title = htmltools::tags$div('LDH Status', style = 'font-size: 16px; color: black;'),
        values    = ldh$Status,
        group     = "LDH",
        layerId = "LDH") %>%
    addPolygons(
      data             = aoc,
      fillColor        = ~aoc_pal(label), fillOpacity = 0.2, color = ~aoc_pal(label),
      highlightOptions = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE), weight = 3, opacity = 1,
      label            = ~label, group = "AOC") %>%
    addLegend(
      pal       = aoc_pal,
      position  = "topleft",
      title     = "AOC",
      # title = htmltools::tags$div('LDH Status', style = 'font-size: 16px; color: black;'),
      values    = aoc$label,
      group     = "AOC",
      layerId   = "AOC") %>%
    addPolygons(
        data             = mask_interm,
        fillColor        = "white", fillOpacity = 0.5, color = "white", weight = 1.5, opacity = 1,
        highlightOptions = highlightOptions(color = "white", opacity = 1, weight = 2.5, bringToFront = TRUE),
        label            = ~label, group = "Full mask (LDH open & intermed.)" )  %>%
    addPolygons(
        data             = mask_open,
        fillColor        = "white", fillOpacity = 0.5, color = "white", weight = 1.5, opacity = 1,
        highlightOptions = highlightOptions(color = "white", opacity = 1, weight = 2.5, bringToFront = TRUE),
        label            = ~label, group = "Full mask (LDH open)")  %>%
    addRasterImage(
        sowb,
        colors    = "#00bfb2", opacity   = 0.7,
        group     = "State owned water bottoms") %>% # colors  = if (raster::is.factor(fetch_cat7)) "Set1" else "YlGnBu",
    addLegend(
        pal       = sowb_pal,
        position  = "topleft", values    = sowb_label,
        group     = "State owned water bottoms", layerId = "State owned water bottoms") %>%
    addPolygons(
        data             = cpra_proj_poly,
        col              = "black",
        fillColor        = "hotpink", # "#CC2936",  #  "#F45B69",
        fillOpacity      = 0.7, weight = 1.5,
        highlightOptions = highlightOptions(color = "hotpink", opacity = 1, weight = 2, bringToFront = TRUE),
        label            = ~label, group = "CPRA projects buffer") %>%
    addLegend(
        pal       = cpra_proj_pal,
        position  = "topleft", values = cpra_proj_label,
        group     = "CPRA projects buffer", layerId  = "CPRA projects buffer") %>%
    addPolygons(
        data              = cup,
        col               = "black", fillColor = "#EAC435", fillOpacity = 0.4, weight = 1.5, #"#DD7373",
        highlightOptions  = highlightOptions(color = "#EAC435", opacity = 1, weight = 2, bringToFront = TRUE),
        label             = ~label, group = "Coastal Use Permits") %>%
    addLegend(
        pal       = cup_pal,
        position  = "topleft", values = cup_label,
        group     = "Coastal Use Permits", layerId  = "Coastal Use Permits") %>%
   # leaflegend::addLegendFactor(pal = factpal,  title = htmltools::tags$div('LDH Status', style = 'font-size: 16px; color: black;'),  values = ldh$Status, labelStyle = 'font-size: 12px; font-weight: bold;',   group = "LDH") %>%
    addRasterImage(
        fetch_cat7,
        opacity   = 0.8,
        colors    = turbo_pal,
        group     = "Fetch") %>%
    addLegend(
        pal       = fetch_fact,
        position  = "bottomright", title  = "Fetch", labFormat = labelFormat(suffix = " km"),
        group     = "Fetch", layerId  = "Fetch", values    = fetch_cols$fetch_cols ) %>%
    addRasterImage(
        sal3,
        opacity   = 0.8,
        colors    = sal_pal,
        group     = "Salinity year 1") %>%
    addRasterImage(
        sal10,
        opacity   = 0.8,
        colors    = sal_pal,
        group     = "Salinity year 8") %>%
    addLegend(
        pal       = sal_pal,
        title     = "Salinity year 1", position  = "bottomleft",
        labFormat = labelFormat(suffix = " g/L"),
        group     = "Salinity year 1",  layerId = "Salinity year 1", values = sal_cols$numeric_cols) %>%
    addLegend(
        pal       = sal_pal,
        title     = "Salinity year 8", position  = "bottomleft",
        labFormat = labelFormat(suffix = " g/L"),
        group     = "Salinity year 8", layerId = "Salinity year 8", values = sal_cols$numeric_cols) %>%
    addRasterImage(
        depth1,
        opacity   = 0.8,
        colors    =  brewer.pal(9, "Spectral"),  # colors    =  c(brewer.pal(11, "Spectral")[c(2, 6)],"#5E4FA2"),
        group     = "Depth year 1") %>%
    addLegend(
        pal       = depth_fact_pal,
        title     = "Depth year 1",  position  = "topleft",
        group     = "Depth year 1",  layerId = "Depth year 1", values = depth_labels) %>%
    addRasterImage(
      wlvl_clamp,
      colors    = wlvl_pal,
      opacity   = 0.8,
      group     = "Water level variability") %>%
    addLegend(
      pal       = wlvl_pal,
      labFormat = labelFormat(suffix = " m"),
      title     = "Water level variability", position  = "bottomleft",
      group     = "Water level variability", layerId   = "Water level variability", values = values(wlvl_clamp)) %>%
    # addRasterImage(
    #     wlvl_clamp,
    #     colors    = wlvl_pal,
    #     opacity   = 0.8,
    #     group     = "Water level variability") %>%
    # addLegend(
    #     pal       = wlvl_pal,
    #     labFormat = labelFormat(suffix = " m"),
    #     title     = "Water level variability", position  = "bottomleft",
    #     group     = "Water level variability", layerId   = "Water level variability", values = values(wlvl_clamp)) %>%
    # addRasterImage(
    #   depth10,
    #   colors    = depth_pal,
    #   group     = "Depth year 8") %>%
    # addLegend(
    #   pal       = depth_pal,
    #   title     = "Depth year 8", position  = "bottomleft",
    #   labFormat = labelFormat(suffix = " m"),
    #   group     = "Depth year 8",  layerId = "Depth year 8", values = depth_cols$numeric_cols) %>%
    addRasterImage(
        hsi_sal3,
        colors    = viridisLite::magma(n = 20),
        # colors    = RColorBrewer::brewer.pal(9, "YlOrRd"),
        opacity   = 0.7,
        group     = "HSI Salinity year 1") %>%
    addLegend(
        pal       = hsi_pal,
        title     = "HSI", position  = "bottomright",
        group     = "HSI Salinity year 1", layerId = "HSI Salinity year 1", values = values(hsi_sal3)) %>%
    addRasterImage(
        hsi_sal10,
        colors    = viridisLite::magma(n = 20),
        # colors    = rev(RColorBrewer::brewer.pal(9, "YlOrRd")),
        opacity   = 0.7,
        group     = "HSI Salinity year 8") %>%
    addLegend(
        pal       = hsi_pal,
        title     = "HSI", position  = "bottomright",
        group     = "HSI Salinity year 8", layerId = "HSI Salinity year 8", values = values(hsi_sal10)) %>%
    addRasterImage(
        hsi_sal3_mask,
        colors    = viridisLite::magma(n = 20),
        # colors    = rev(RColorBrewer::brewer.pal(9, "YlOrRd")),
        opacity   = 0.7,
        group     = "HSI Salinity full mask year 1") %>%
    addLegend(
        pal       = hsi_pal,
        title     = "HSI", position  = "bottomright",
        group     = "HSI Salinity full mask year 1", layerId = "HSI Salinity full mask year 1", values = values(hsi_sal3)) %>%
    addRasterImage(
        hsi_sal10_mask,
        colors    = viridisLite::magma(n = 20),
        # colors    = rev(RColorBrewer::brewer.pal(9, "YlOrRd")),
        opacity   = 0.7,
        group     = "HSI Salinity full mask year 8") %>%
    addLegend(
        pal       = hsi_pal,
        title     = "HSI", position  = "bottomright",
        group     = "HSI Salinity full mask year 8", layerId = "HSI Salinity full mask year 8", values = values(hsi_sal10)) %>%
    # addRasterImage(fetch_rc, colors  = if (raster::is.factor(fetch_rc)) "Set1" else  "YlGnBu",group = "Fetch") %>% # addLegendNumeric( pal    = fetch_pal, values = fetch_cols$fetch_cols)
    addPolylines(
      data              = road_buffer[4,],
      fillColor         = 'transparent',  col   = "red", opacity  = 1, weight  = 3,
      label             = ~road_buffer$buffer_dist[1], group = "Road buffer 20km",
      highlightOptions  = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
    addPolylines(
      data               = road_buffer[3,],
      fillColor          = 'transparent', col   = "red", opacity = 1, weight  = 3,
      label              = ~road_buffer$buffer_dist[2],  group = "Road buffer 10km",
      highlightOptions   = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
    addPolylines(
      data               = road_buffer[2,],
      fillColor          = 'transparent',  col   = "red", opacity     = 1,  weight  = 3,
      label              = ~road_buffer$buffer_dist[3], group = "Road buffer 5km",
      highlightOptions   = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
    addPolylines(
      data               = road_buffer[1,],
      fillColor          = 'transparent', col  = "red",opacity  = 1, weight  = 3,
      label              = ~road_buffer$buffer_dist[4],  group = "Road buffer 2km",
      highlightOptions   = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
    addPolygons(
      data                = waterways,
      fillColor           = "dodgerblue", weight = 2, fillOpacity = 0.7, color = "white",
      highlightOptions    = highlightOptions(color = "white", opacity = 1, weight = 4, bringToFront = TRUE),
      label               = ~label,  group = "Waterways") %>%
    addLegend(
      pal       = waterways_pal,
      position  = "topleft", values = waterways_label,
      group     = "Waterways", layerId  = "Waterways"
    ) %>%
    addLegend(
      pal       = mask_interm_pal,
      position  = "topleft", values = mask_interm_label,
      group     = "Full mask (LDH open & intermed.)",  layerId = "Full mask (LDH open & intermed.)"
    ) %>%
    addLegend(
      pal       = mask_open_pal,
      position  = "topleft", values = mask_open_label,
      group     = "Full mask (LDH open)", layerId = "Full mask (LDH open)"
    ) %>%
    addPolygons(
      data       = land_sf,
      fillColor  = 'white', fillOpacity =  0.3, col  = "black",
      opacity    = 1, weight  = 1.5, label = ~label,  group  = "Land") %>%
    # addRasterImage(  waterways, colors  = "dodgerblue",  group = "Waterways") %>%
    # addRasterImage( cpra_projects, colors  = turbo_pal, group = "CPRA projects buffer") %>%
      addLayersControl(
        options = layersControlOptions(collapsed = TRUE),
        baseGroups = c("Imagery", "Topographic", "Nat. Geo. Topographic"), overlayGroups = c(
          "LDH", "AOC", "Salinity year 1", "Salinity year 8","Depth year 1", "Water level variability", "Fetch","HSI Salinity year 1", "HSI Salinity year 8", "HSI Salinity full mask year 1", "HSI Salinity full mask year 8",
          "Road buffer 2km", "Road buffer 5km","Road buffer 10km", "Road buffer 20km",
          "Waterways", "CPRA projects buffer", "Coastal Use Permits", "State owned water bottoms",
          "Full mask (LDH open)", "Full mask (LDH open & intermed.)", "Land")) %>%
    hideGroup(
      c("AOC", "Salinity year 1", "Salinity year 8", "Depth year 1", "Water level variability", "Fetch", "HSI Salinity year 1", "HSI Salinity year 8", "HSI Salinity full mask year 1", "HSI Salinity full mask year 8",
        "Road buffer 2km", "Road buffer 5km", "Road buffer 10km", "Road buffer 20km",  "Waterways",
        "CPRA projects buffer", "Coastal Use Permits",  "State owned water bottoms",
        "Full mask (LDH open)", "Full mask (LDH open & intermed.)", "Land")
      )
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
    # htmlwidgets::onRender("
    #   function(el, x) {
    #      var updateLegend = function () {
    #         var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
    #
    #         document.querySelectorAll('.legend').forEach(a => a.hidden=true);
    #         document.querySelectorAll('.legend').forEach(l => {
    #            if (l.classList.contains(selectedGroup)) l.hidden=false;
    #         });
    #      };
    #      updateLegend();
    #      this.on('baselayerchange', el => updateLegend());
    #   }")
}
# addRasterImage(  road_stk$road_buffer_20km,  col   = "red",   opacity   = 0.2,  group = "Road buffer 20km) %>%
# addRasterImage(   road_stk$road_buffer_10km, opacity   =  0.3,  col   = "red",   group = "Road buffer 10km") %>%
# addRasterImage(  road_stk$road_buffer_5km,  col  = "red",#   opacity =  0.5,  group = "Road buffer 5km" ) %>%
# addRasterImage( road_stk$road_buffer_2km, opacity  =  0.8,  col  = "red", group = "Road buffer 2km" ) %>%
# leaflet() %>%
#   addProviderTiles(providers$Esri.OceanBasemap, group = "Topographic") %>%
#   # addProviderTiles(providers$Esri.DeLorme, group = "ESRI DeLorme") %>%
#   addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
#   addPolygons(
#     data = land_sf,
#     fillColor     = 'grey',
#     fillOpacity   =  0.4,
#     col     = "black",
#     opacity = 1,
#     weight    = 1.5,
#     group         = "Land",
#     labelOptions  = labelOptions(
#       noHide = F,
#       style = list("color" = "black", "font-weight" = "1000"))) %>%
#   addRasterImage(
#     fetch_cat7,
#     colors      = if (raster::is.factor(fetch_cat7)) "Set1" else
#       "YlGnBu",
#     group       = "Fetch") %>%
#   # addRasterImage(
#   #   fetch_rc,
#   #   colors      = if (raster::is.factor(fetch_rc)) "Set1" else
#   #                       "YlGnBu",
#   #   group       = "Fetch") %>%
#   addPolylines(
#     data = road_buffer[1,],
#     fillColor   = 'transparent',
#     col         = "red",
#     opacity     = 1,
#     weight      = 3,
#     label       = ~road_buffer$buffer_dist[1],
#     group       = "Road buffer 2km") %>%
#   addPolylines(
#     data = road_buffer[2,],
#     fillColor   = 'transparent',
#     col         = "red",
#     opacity     = 1,
#     weight      = 3,
#     label       = ~road_buffer$buffer_dist[2],
#     group       = "Road buffer 5km") %>%
#   addPolylines(
#     data = road_buffer[3,],
#     fillColor   = 'transparent',
#     col         = "red",
#     opacity     = 1,
#     weight      = 3,
#     label       = ~road_buffer$buffer_dist[3],
#     group       = "Road buffer 10km") %>%
#   addPolylines(
#     data        = road_buffer[4,],
#     fillColor   = 'transparent',
#     col         = "red",
#     opacity     = 1,
#     weight      = 3,
#     label       = ~road_buffer$buffer_dist[4],
#     group       = "Road buffer 20km") %>%
#   addRasterImage(
#     waterways,
#     colors      = "green",
#     group       = "Waterways"
#   ) %>%
#   # addRasterImage(
#   #   road_stk$road_buffer_20km,  col         = "red",   opacity     = 0.2,  group       = "Road buffer 20km" ) %>%
#   # addRasterImage(
#   #   road_stk$road_buffer_10km, opacity     =  0.3,  col         = "red",   group       = "Road buffer 10km") %>%
#   # addRasterImage(
#   #   road_stk$road_buffer_5km,  col         = "red",#   opacity     =  0.5,  group       = "Road buffer 5km" ) %>%
#   # addRasterImage(
#   #   road_stk$road_buffer_2km, opacity     =  0.8,  col         = "red", group       = "Road buffer 2km" ) %>%
#   addLayersControl(
#     options = layersControlOptions(collapsed = FALSE),
#     baseGroups = c("Topographic", "Imagery"),
#     overlayGroups = c("Land",
#                       "Road buffer 2km", "Road buffer 5km", "Road buffer 10km", "Road buffer 20km", "Waterways",
#                       "Fetch")) %>%
#   addScaleBar("bottomleft") %>%
#   addMeasure(
#     position = "bottomleft",
#     primaryLengthUnit = "feet",
#     primaryAreaUnit = "sqmiles",
#     activeColor = "red",
#     completedColor = "green") %>%
#   leafem::addMouseCoordinates()
